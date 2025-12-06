/**
 * Server Middleware - DRY Server Configuration
 * 
 * One unified middleware for all server implementations.
 * Works with Workers, Node, Bun, Deno, or any HTTP/WebSocket server.
 */

import { RelayServer } from '../relay-server';
import { handleRelayMessage } from './message-handler';
import { handleHttpRpcBatch, handleStatsRequest } from './http-handler';
import type { WebSocketAdapter } from './message-handler';
import type { HttpRequest, HttpResponse } from './http-handler';

/**
 * Server middleware provides unified handlers for all server operations
 */
export interface ServerMiddleware {
  /**
   * Handle WebSocket messages
   * @param ws WebSocket adapter for your server implementation
   * @param message Incoming message data
   */
  websocket: (ws: WebSocketAdapter, message: string | Buffer) => Promise<void>;

  /**
   * Handle HTTP RPC batch requests
   * @param request HTTP request adapter
   */
  http: (request: HttpRequest) => Promise<HttpResponse>;

  /**
   * Handle stats requests
   */
  stats: () => HttpResponse;

  /**
   * The underlying RelayServer instance
   */
  relay: RelayServer;
}

/**
 * Create unified middleware for a RelayServer
 * 
 * This is the recommended way to set up any server implementation.
 * 
 * @example
 * // Cloudflare Workers
 * const middleware = createServerMiddleware(relay);
 * 
 * if (isWebSocket) {
 *   const wsAdapter = { send: (data) => ws.send(data), close: () => ws.close() };
 *   await middleware.websocket(wsAdapter, event.data);
 * }
 * 
 * if (isHttp) {
 *   const httpReq = { method: req.method, body: await req.text(), headers: {...} };
 *   const httpRes = await middleware.http(httpReq);
 *   return new Response(httpRes.body, { status: httpRes.status, headers: httpRes.headers });
 * }
 * 
 * @example
 * // Node.js
 * const middleware = createServerMiddleware(relay);
 * 
 * ws.on('message', (msg) => {
 *   const wsAdapter = { send: (data) => ws.send(data), close: () => ws.close() };
 *   middleware.websocket(wsAdapter, msg);
 * });
 * 
 * app.post('/rpc', async (req, res) => {
 *   const httpReq = { method: 'POST', body: req.body, headers: req.headers };
 *   const httpRes = await middleware.http(httpReq);
 *   res.status(httpRes.status).set(httpRes.headers).send(httpRes.body);
 * });
 */
export function createServerMiddleware(relay: RelayServer): ServerMiddleware {
  return {
    websocket: async (ws: WebSocketAdapter, message: string | Buffer) => {
      await handleRelayMessage(ws, message, relay);
    },

    http: async (request: HttpRequest) => {
      return await handleHttpRpcBatch(request, relay);
    },

    stats: () => {
      return handleStatsRequest(relay);
    },

    relay
  };
}

/**
 * Create a new RelayServer with middleware
 * 
 * Convenience function that creates both the server and middleware.
 * 
 * @example
 * const { relay, middleware } = createRelayServerWithMiddleware();
 */
export function createRelayServerWithMiddleware(): {
  relay: RelayServer;
  middleware: ServerMiddleware;
} {
  const relay = new RelayServer();
  const middleware = createServerMiddleware(relay);
  return { relay, middleware };
}

/**
 * Adapter for Cloudflare Workers WebSocket
 */
export function createWorkersWebSocketAdapter(server: any): WebSocketAdapter {
  return {
    send: (data: string) => server.send(data),
    close: () => server.close()
  };
}

/**
 * Adapter for Node.js ws WebSocket
 */
export function createNodeWebSocketAdapter(ws: any): WebSocketAdapter {
  return {
    send: (data: string) => ws.send(data),
    close: () => ws.close()
  };
}

/**
 * Adapter for Bun WebSocket
 */
export function createBunWebSocketAdapter(ws: any): WebSocketAdapter {
  return {
    send: (data: string) => ws.send(data),
    close: () => ws.close()
  };
}

/**
 * Adapter for Workers HttpRequest
 */
export async function createWorkersHttpRequest(request: Request): Promise<HttpRequest> {
  const headers: Record<string, string> = {};
  request.headers.forEach((value, key) => {
    headers[key] = value;
  });
  
  return {
    method: request.method,
    body: await request.text(),
    headers
  };
}

/**
 * Adapter for Node.js http.IncomingMessage
 */
export async function createNodeHttpRequest(req: any): Promise<HttpRequest> {
  return {
    method: req.method,
    body: await new Promise<string>((resolve) => {
      let body = '';
      req.on('data', (chunk: Buffer) => { body += chunk.toString(); });
      req.on('end', () => resolve(body));
    }),
    headers: req.headers
  };
}

/**
 * Convert HttpResponse to Workers Response
 */
export function toWorkersResponse(httpResponse: HttpResponse): Response {
  return new Response(httpResponse.body, {
    status: httpResponse.status,
    headers: httpResponse.headers
  });
}

/**
 * Convert HttpResponse to Node.js response
 */
export function toNodeResponse(httpResponse: HttpResponse, res: any): void {
  res.writeHead(httpResponse.status, httpResponse.headers);
  res.end(httpResponse.body);
}

