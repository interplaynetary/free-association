/**
 * Transport Adapters for Cap'n Web RPC
 * 
 * Provides different transport layers for peer-to-peer communication:
 * - WebSocket: For client-server or relay-based connections
 * - postMessage: For iframe/Worker communication
 * - WebRTC: For direct peer-to-peer connections
 * - HTTP Batch: For request/response style with batching
 */

export { WebSocketTransport } from './websocket';
export { PostMessageTransport } from './postmessage';
export { WebRTCTransport } from './webrtc';

export type { Transport, TransportMessage, TransportOptions } from './types';

