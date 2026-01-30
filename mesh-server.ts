/**
 * WebSocket Server for Mesh P2P Network
 * 
 * This creates a WebSocket server that Mesh clients can connect to.
 * The server acts as a relay/bootstrap peer for the P2P network.
 */

import { WebSocketTransportServer } from '@playnet/mesh';

// Initialize WebSocket server on port 8080 (or from env)
const WS_PORT = process.env.MESH_WS_PORT ? parseInt(process.env.MESH_WS_PORT) : 8080;

console.log(`[MESH WS] Starting WebSocket server on port ${WS_PORT}...`);

const wsServer = WebSocketTransportServer({
    port: WS_PORT,
    path: '/'
});

console.log(`[MESH WS] ✅ WebSocket server listening on ws://localhost:${WS_PORT}`);
console.log(`[MESH WS] Clients should connect to: wss://free.playnet.lol`);

// Keep the process alive
process.on('SIGINT', () => {
    console.log('\n[MESH WS] Shutting down...');
    process.exit(0);
});
