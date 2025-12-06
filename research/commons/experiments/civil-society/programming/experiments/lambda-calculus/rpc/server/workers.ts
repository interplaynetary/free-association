/**
 * Cloudflare Workers Server Example
 * 
 * Deploy this to Cloudflare Workers to create a relay server
 * that connects recognition-based peers.
 * 
 * Deploy with:
 * ```bash
 * npx wrangler deploy
 * ```
 */

import { RelayServer } from '../relay-server';
import { RpcTarget } from '../rpc-target';

// WebSocket adapter interface
interface WebSocketAdapter {
  send(data: string): void;
  close(): void;
}

// Global relay instance (persists across requests in same Worker)
let relay: RelayServer | null = null;

function getRelay(): RelayServer {
  if (!relay) {
    relay = new RelayServer();
  }
  return relay;
}

/**
 * Cloudflare Workers fetch handler
 * 
 * Handles both WebSocket and HTTP requests.
 */
export default {
  async fetch(request: Request): Promise<Response> {
    const url = new URL(request.url);
    const relay = getRelay();

    // WebSocket upgrade
    if (request.headers.get('Upgrade') === 'websocket') {
      return handleWebSocket(request, relay);
    }

    // HTTP RPC endpoint
    if (url.pathname === '/rpc' || url.pathname === '/') {
      return handleRpc(request, relay);
    }

    // Stats endpoint
    if (url.pathname === '/stats') {
      return new Response(JSON.stringify(relay.getStats(), null, 2), {
        headers: { 'Content-Type': 'application/json' }
      });
    }

    // Authentication endpoints
    if (url.pathname === '/auth/challenge') {
      const challenge = relay.createChallenge();
      return new Response(JSON.stringify(challenge), {
        headers: { 'Content-Type': 'application/json' }
      });
    }

    if (url.pathname === '/auth/authenticate' && request.method === 'POST') {
      try {
        const body = await request.json();
        const { challenge, signature, publicKey } = body;
        const session = await relay.authenticate(challenge, signature, publicKey);
        return new Response(JSON.stringify({ 
          sessionId: (session as any).entityId,
          success: true
        }), {
          headers: { 'Content-Type': 'application/json' }
        });
      } catch (error) {
        return new Response(JSON.stringify({
          error: error instanceof Error ? error.message : String(error),
          success: false
        }), {
          status: 401,
          headers: { 'Content-Type': 'application/json' }
        });
      }
    }

    return new Response('Not Found', { status: 404 });
  }
};

/**
 * Handle WebSocket connections (DRY: uses shared message handler)
 */
async function handleWebSocket(request: Request, relay: RelayServer): Promise<Response> {
  const upgradeHeader = request.headers.get('Upgrade');
  if (upgradeHeader !== 'websocket') {
    return new Response('Expected Upgrade: websocket', { status: 426 });
  }

  // Create WebSocket pair
  const [client, server] = Object.values(new WebSocketPair());

  // Accept the connection
  server.accept();

  // Create adapter for Workers WebSocket API
  const wsAdapter: WebSocketAdapter = {
    send: (data: string) => server.send(data),
    close: () => server.close()
  };

  // Handle messages using shared handler
  server.addEventListener('message', async (event: MessageEvent) => {
    await handleRelayMessage(wsAdapter, event.data as string, relay);
  });

  server.addEventListener('close', () => {
    console.log('WebSocket closed');
  });

  // Return WebSocket response (Cloudflare Workers specific)
  return new Response(null, {
    status: 101,
    webSocket: client
  } as any);
}

/**
 * Handle HTTP RPC requests
 */
async function handleRpc(request: Request, relay: RelayServer): Promise<Response> {
  if (request.method !== 'POST') {
    return new Response('Method Not Allowed', { status: 405 });
  }

  try {
    const body = await request.json();

    // Handle batch requests
    if (body.batch && Array.isArray(body.batch)) {
      const results = [];
      for (const call of body.batch) {
        results.push(await handleCall(call, relay));
      }
      return new Response(JSON.stringify(results), {
        headers: { 'Content-Type': 'application/json' }
      });
    }

    // Handle single request
    const result = await handleCall(body, relay);
    return new Response(JSON.stringify(result), {
      headers: { 'Content-Type': 'application/json' }
    });
  } catch (error) {
    return new Response(JSON.stringify({
      error: error instanceof Error ? error.message : String(error)
    }), {
      status: 500,
      headers: { 'Content-Type': 'application/json' }
    });
  }
}

/**
 * Handle individual RPC call
 */
async function handleCall(call: any, relay: RelayServer): Promise<any> {
  const { method, params = [] } = call;

  // Call the method on relay
  if (typeof (relay as any)[method] === 'function') {
    const result = await (relay as any)[method](...params);
    return { result };
  }

  throw new Error(`Unknown method: ${method}`);
}

/**
 * Handle WebSocket message
 */
async function handleRelayMessage(
  ws: WebSocketAdapter,
  message: string,
  relay: RelayServer
): Promise<void> {
  try {
    const call = JSON.parse(message);
    const result = await handleCall(call, relay);
    ws.send(JSON.stringify(result));
  } catch (error) {
    ws.send(JSON.stringify({
      error: error instanceof Error ? error.message : String(error)
    }));
  }
}

// Type for Workers environment
declare global {
  class WebSocketPair {
    0: WebSocket;
    1: WebSocket;
  }
}

