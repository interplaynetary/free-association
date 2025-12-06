/**
 * Node.js / Bun Server Example
 * 
 * Run this with Node.js or Bun to create a relay server.
 * 
 * ```bash
 * # With Node.js
 * node server/node.ts
 * 
 * # With Bun (faster!)
 * bun run server/node.ts
 * ```
 */

import { RelayServer } from '../relay-server';

const PORT = process.env.PORT || 8080;

/**
 * Create HTTP server with WebSocket support
 */
async function createServer() {
  const relay = new RelayServer();

  // Use native Node.js http if available, otherwise Bun
  if (typeof Bun !== 'undefined') {
    return createBunServer(relay);
  } else {
    return createNodeServer(relay);
  }
}

/**
 * Create Bun server (simpler!)
 */
function createBunServer(relay: RelayServer) {
  return Bun.serve({
    port: PORT,
    
    async fetch(req, server) {
      const url = new URL(req.url);

      // WebSocket upgrade
      if (req.headers.get('upgrade') === 'websocket') {
        const success = server.upgrade(req);
        return success 
          ? undefined 
          : new Response('WebSocket upgrade failed', { status: 400 });
      }

      // HTTP RPC endpoint
      if (url.pathname === '/rpc' || url.pathname === '/') {
        return handleHttpRpc(req, relay);
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

      if (url.pathname === '/auth/authenticate' && req.method === 'POST') {
        try {
          const body = await req.json();
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
    },

    websocket: {
      message(ws, message) {
        handleWebSocketMessage(ws, message, relay);
      },
      open(ws) {
        console.log('WebSocket opened');
      },
      close(ws) {
        console.log('WebSocket closed');
      }
    }
  });
}

/**
 * Create Node.js server
 */
function createNodeServer(relay: RelayServer) {
  const http = require('http');
  const { WebSocketServer } = require('ws');

  const server = http.createServer(async (req: any, res: any) => {
    const url = new URL(req.url, `http://${req.headers.host}`);

    // HTTP RPC endpoint
    if (url.pathname === '/rpc' || url.pathname === '/') {
      return handleHttpRpcNode(req, res, relay);
    }

    // Stats endpoint
    if (url.pathname === '/stats') {
      res.writeHead(200, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify(relay.getStats(), null, 2));
      return;
    }

    // Authentication endpoints
    if (url.pathname === '/auth/challenge') {
      const challenge = relay.createChallenge();
      res.writeHead(200, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify(challenge));
      return;
    }

    if (url.pathname === '/auth/authenticate' && req.method === 'POST') {
      let body = '';
      req.on('data', (chunk: any) => body += chunk);
      req.on('end', async () => {
        try {
          const data = JSON.parse(body);
          const { challenge, signature, publicKey } = data;
          const session = await relay.authenticate(challenge, signature, publicKey);
          res.writeHead(200, { 'Content-Type': 'application/json' });
          res.end(JSON.stringify({ 
            sessionId: (session as any).entityId,
            success: true
          }));
        } catch (error) {
          res.writeHead(401, { 'Content-Type': 'application/json' });
          res.end(JSON.stringify({
            error: error instanceof Error ? error.message : String(error),
            success: false
          }));
        }
      });
      return;
    }

    res.writeHead(404);
    res.end('Not Found');
  });

  // WebSocket server
  const wss = new WebSocketServer({ server });
  
  wss.on('connection', (ws: any) => {
    console.log('WebSocket connected');
    
    ws.on('message', (message: any) => {
      handleWebSocketMessage(ws, message, relay);
    });
    
    ws.on('close', () => {
      console.log('WebSocket disconnected');
    });
  });

  server.listen(PORT);
  return server;
}

/**
 * Handle WebSocket messages
 */
async function handleWebSocketMessage(ws: any, message: any, relay: RelayServer) {
  try {
    const data = JSON.parse(message.toString());
    
    if (data.type === 'register') {
      await relay.register(data.entityId);
      ws.send(JSON.stringify({
        type: 'registered',
        entityId: data.entityId
      }));
    } else if (data.type === 'connect') {
      await relay.connect(data.fromId, data.toId);
      ws.send(JSON.stringify({
        type: 'connected',
        fromId: data.fromId,
        toId: data.toId
      }));
    } else {
      ws.send(JSON.stringify({
        type: 'response',
        result: 'ok'
      }));
    }
  } catch (error) {
    ws.send(JSON.stringify({
      type: 'error',
      error: error instanceof Error ? error.message : String(error)
    }));
  }
}

/**
 * Handle HTTP RPC (Bun/Web standard)
 */
async function handleHttpRpc(req: Request, relay: RelayServer): Promise<Response> {
  if (req.method !== 'POST') {
    return new Response('Method Not Allowed', { status: 405 });
  }

  try {
    const body = await req.json();

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
 * Handle HTTP RPC (Node.js)
 */
async function handleHttpRpcNode(req: any, res: any, relay: RelayServer) {
  if (req.method !== 'POST') {
    res.writeHead(405);
    res.end('Method Not Allowed');
    return;
  }

  let body = '';
  req.on('data', (chunk: any) => body += chunk);
  req.on('end', async () => {
    try {
      const data = JSON.parse(body);

      // Handle batch or single
      const result = data.batch
        ? await Promise.all(data.batch.map((call: any) => handleCall(call, relay)))
        : await handleCall(data, relay);

      res.writeHead(200, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify(result));
    } catch (error) {
      res.writeHead(500, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify({
        error: error instanceof Error ? error.message : String(error)
      }));
    }
  });
}

/**
 * Handle individual RPC call
 */
async function handleCall(call: any, relay: RelayServer): Promise<any> {
  const { method, params = [] } = call;

  if (typeof (relay as any)[method] === 'function') {
    const result = await (relay as any)[method](...params);
    return { result };
  }

  throw new Error(`Unknown method: ${method}`);
}

// Start server
createServer().then((server) => {
  console.log(`✨ Free Association Relay Server running on port ${PORT}`);
  console.log(`   WebSocket: ws://localhost:${PORT}`);
  console.log(`   HTTP RPC:  http://localhost:${PORT}/rpc`);
  console.log(`   Stats:     http://localhost:${PORT}/stats`);
});

// Graceful shutdown
process.on('SIGINT', async () => {
  console.log('\n🛑 Shutting down...');
  process.exit(0);
});

