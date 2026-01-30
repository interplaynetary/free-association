/**
 * Bun Proxy Server for App Platform
 * 
 * This is a simple Bun server that proxies:
 * - /api/* → Droplet backend (port 3000)
 * - /mesh → Droplet WebSocket (port 8766)
 * 
 * Only needed if using the 'services' approach in app.yaml
 * (requires paid App Platform plan)
 * 
 * Note: For Option A (recommended), configure routes via App Platform UI instead
 */

const PORT = process.env.PORT || 8080;
const DROPLET_IP = process.env.DROPLET_IP;

if (!DROPLET_IP) {
    console.error('ERROR: DROPLET_IP environment variable not set!');
    process.exit(1);
}

console.log(`Starting Bun proxy server...`);
console.log(`Proxying to droplet: ${DROPLET_IP}`);

const server = Bun.serve({
    port: PORT,

    async fetch(req) {
        const url = new URL(req.url);

        // Health check
        if (url.pathname === '/health') {
            return new Response(JSON.stringify({ status: 'ok', proxy: 'running' }), {
                headers: { 'Content-Type': 'application/json' }
            });
        }

        // Proxy /api/* to backend
        if (url.pathname.startsWith('/api/')) {
            const targetUrl = `http://${DROPLET_IP}:3000${url.pathname}${url.search}`;
            console.log(`[API] ${req.method} ${url.pathname} → ${targetUrl}`);

            try {
                const response = await fetch(targetUrl, {
                    method: req.method,
                    headers: req.headers,
                    body: req.body
                });

                return response;
            } catch (error) {
                console.error(`[API] Proxy error:`, error.message);
                return new Response(JSON.stringify({ error: 'Backend unavailable' }), {
                    status: 502,
                    headers: { 'Content-Type': 'application/json' }
                });
            }
        }

        // Proxy /mesh to WebSocket server
        if (url.pathname === '/mesh') {
            const targetUrl = `http://${DROPLET_IP}:8766${url.pathname}${url.search}`;
            console.log(`[WS] ${req.method} ${url.pathname} → ${targetUrl}`);

            try {
                const response = await fetch(targetUrl, {
                    method: req.method,
                    headers: req.headers,
                    body: req.body
                });

                return response;
            } catch (error) {
                console.error(`[WS] Proxy error:`, error.message);
                return new Response(JSON.stringify({ error: 'WebSocket server unavailable' }), {
                    status: 502,
                    headers: { 'Content-Type': 'application/json' }
                });
            }
        }

        // Not found
        return new Response('Not found', { status: 404 });
    },

    // WebSocket upgrade handler
    websocket: {
        open(ws) {
            console.log('[WS] Client connected');
        },
        message(ws, message) {
            // Forward WebSocket messages to droplet
            // Note: This is a simplified implementation
            // For production, you'd need proper WebSocket proxying
            console.log('[WS] Message received:', message);
        },
        close(ws) {
            console.log('[WS] Client disconnected');
        }
    }
});

console.log(`✅ Bun proxy server running on port ${PORT}`);
console.log(`   /api/* → http://${DROPLET_IP}:3000`);
console.log(`   /mesh → http://${DROPLET_IP}:8766`);

// Graceful shutdown
process.on('SIGTERM', () => {
    console.log('SIGTERM received, shutting down gracefully...');
    server.stop();
    process.exit(0);
});
