import Holster from '@mblaney/holster';
import express from 'express';
import { WebSocketServer } from 'ws';
import { createServer } from 'http';

// Configuration from environment variables with defaults
const config = {
  host: process.env.HOLSTER_RELAY_HOST || '0.0.0.0',
  port: parseInt(process.env.HOLSTER_RELAY_PORT) || 8766,
  storageEnabled: process.env.HOLSTER_RELAY_STORAGE === 'true' || true,
  storagePath: process.env.HOLSTER_RELAY_STORAGE_PATH || './holster-data',
  maxConnections: parseInt(process.env.HOLSTER_MAX_CONNECTIONS) || 100
};

console.log('Starting Holster Relay Server with configuration:');
console.log(JSON.stringify(config, null, 2));

// Create Express app
const app = express();

// Basic health check endpoint
app.get('/health', (req, res) => {
  res.json({ status: 'ok', service: 'holster-relay', timestamp: Date.now() });
});

// Create HTTP server
const server = createServer(app);

// Create WebSocket server
const wss = new WebSocketServer({
  server,
  path: '/holster'
});

// Initialize Holster with configuration
const holster = Holster({
  wss,
  storage: config.storageEnabled ? config.storagePath : undefined,
  secure: true
});

// Track connections
let connectionCount = 0;
let activeConnections = 0;

wss.on('connection', (ws, req) => {
  // Check connection limit
  if (activeConnections >= config.maxConnections) {
    console.warn(`Connection rejected: limit reached (${config.maxConnections})`);
    ws.close(1008, 'Maximum connections reached');
    return;
  }

  connectionCount++;
  activeConnections++;
  const clientId = connectionCount;
  console.log(`Client ${clientId} connected from ${req.socket.remoteAddress} (active: ${activeConnections}/${config.maxConnections})`);

  ws.on('close', () => {
    activeConnections--;
    console.log(`Client ${clientId} disconnected (active: ${activeConnections}/${config.maxConnections})`);
  });

  ws.on('error', (error) => {
    console.error(`Client ${clientId} error:`, error.message);
  });
});

// Start the server
server.listen(config.port, config.host, () => {
  console.log(`Holster Relay Server listening on ${config.host}:${config.port}`);
  console.log(`WebSocket endpoint: ws://${config.host}:${config.port}/holster`);
  console.log(`Health check: http://${config.host}:${config.port}/health`);
  console.log(`Storage: ${config.storageEnabled ? 'enabled at ' + config.storagePath : 'disabled'}`);
});

// Graceful shutdown
process.on('SIGTERM', () => {
  console.log('SIGTERM received, shutting down gracefully');
  server.close(() => {
    console.log('Server closed');
    process.exit(0);
  });
});

process.on('SIGINT', () => {
  console.log('SIGINT received, shutting down gracefully');
  server.close(() => {
    console.log('Server closed');
    process.exit(0);
  });
});
