/**
 * Free Association Protocol - Cloudflare Workers Server Example
 * 
 * This is a complete, production-ready RPC server using Cap'n Web
 */

import { newWorkersRpcResponse } from 'capnweb';
import { ParticipantServer } from '../protocol.js';

/**
 * Cloudflare Workers entry point
 * 
 * Deploy with: wrangler deploy
 */
export default {
  async fetch(request: Request, env: any, ctx: any): Promise<Response> {
    const url = new URL(request.url);
    
    // Serve RPC API at /api endpoint
    if (url.pathname === "/api") {
      // Create server instance and handle RPC
      // Cap'n Web automatically handles:
      // - WebSocket upgrades
      // - HTTP batch requests
      // - Promise pipelining
      // - Bidirectional calling
      return newWorkersRpcResponse(request, new ParticipantServer());
    }
    
    // Serve landing page
    if (url.pathname === "/") {
      return new Response(
        `
<!DOCTYPE html>
<html>
<head>
  <title>Free Association Protocol</title>
  <style>
    body { font-family: system-ui; max-width: 800px; margin: 50px auto; padding: 20px; }
    pre { background: #f5f5f5; padding: 15px; border-radius: 5px; overflow-x: auto; }
    code { color: #d73a49; }
  </style>
</head>
<body>
  <h1>Free Association Protocol</h1>
  <p>RPC API server using Cap'n Web</p>
  
  <h2>WebSocket Connection</h2>
  <pre><code>import { newWebSocketRpcSession } from 'capnweb';

const api = newWebSocketRpcSession('wss://${url.host}/api');
const session = await api.authenticate('alice@example.com', credentials);
const budget = await session.getRecognitionBudget();
await budget.allocateRecognition('bob@example.com', 0.6);</code></pre>

  <h2>HTTP Batch Request</h2>
  <pre><code>import { newHttpBatchRpcSession } from 'capnweb';

const batch = newHttpBatchRpcSession('https://${url.host}/api');
const session = batch.authenticate('alice@example.com', credentials);
const network = session.getNetworkState();
const mr = await network.computeMutualRecognition('alice@example.com', 'bob@example.com');</code></pre>

  <h2>Endpoints</h2>
  <ul>
    <li><code>/api</code> - RPC endpoint (WebSocket or HTTP POST)</li>
    <li><code>/health</code> - Health check</li>
  </ul>

  <h2>Documentation</h2>
  <p>See <a href="https://github.com/cloudflare/capnweb">Cap'n Web Documentation</a></p>
</body>
</html>
        `,
        {
          headers: {
            "Content-Type": "text/html; charset=utf-8",
          },
        }
      );
    }
    
    // Health check endpoint
    if (url.pathname === "/health") {
      return Response.json({
        status: "healthy",
        protocol: "Free Association v1.0",
        rpc: "Cap'n Web",
        timestamp: new Date().toISOString()
      });
    }
    
    // Not found
    return new Response("Not found", { status: 404 });
  }
};

/**
 * For local development with wrangler:
 * 
 * 1. Install dependencies:
 *    npm install capnweb zod
 * 
 * 2. Create wrangler.toml:
 *    name = "free-association-protocol"
 *    main = "research/matrix/example-server.ts"
 *    compatibility_date = "2024-01-01"
 * 
 * 3. Run locally:
 *    npx wrangler dev
 * 
 * 4. Deploy:
 *    npx wrangler deploy
 */

