import { initMesh } from '$lib/network/mesh';
import { WebSocketTransportServer } from '@playnet/mesh';

// WebSocket server instance (singleton)
let wsServer: ReturnType<typeof WebSocketTransportServer> | null = null;

/** @type {import('@sveltejs/kit').Handle} */
export async function handle({ event, resolve }) {
    // Initialize Mesh on the server side using Bun runtime
    // This provides a full Mesh instance that can act as a relay/peer
    try {
        await initMesh();
        console.log('[HOOKS] Server-side Mesh initialized successfully');

        // Start WebSocket server (only once)
        if (!wsServer) {
            const WS_PORT = process.env.MESH_WS_PORT ? parseInt(process.env.MESH_WS_PORT) : 8080;
            wsServer = WebSocketTransportServer({
                port: WS_PORT,
            });
            console.log(`[HOOKS] ✅ WebSocket server listening on port ${WS_PORT}`);
            console.log(`[HOOKS] Clients can connect to: ws://localhost:${WS_PORT}`);
        }
    } catch (err) {
        console.error('[HOOKS] Failed to initialize Mesh on server:', err);
    }

    const response = await resolve(event);
    return response;
}


// Looking at the code, you need to manually wire them together. The WebSocketTransportServer needs to be connected to the Mesh instance's Wire layer. Let me check the Wire implementation to see how transports are configured: