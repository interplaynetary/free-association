import Holster from '@mblaney/holster';
import express from 'express';

// Configuration from environment variables with sensible defaults
const config = {
  host: process.env.HOLSTER_RELAY_HOST || '0.0.0.0',
  port: parseInt(process.env.HOLSTER_RELAY_PORT) || 8766,
  storageEnabled: process.env.HOLSTER_RELAY_STORAGE === 'true' || true,
  storagePath: process.env.HOLSTER_RELAY_STORAGE_PATH || './holster-data',
  maxConnections: parseInt(process.env.HOLSTER_MAX_CONNECTIONS) || 100
};

console.log('Starting Holster Relay Server with configuration:');
console.log(JSON.stringify(config, null, 2));

// Minimal Express app for health check
const app = express();
app.get('/health', (req, res) => {
  res.json({ status: 'ok', service: 'holster-relay', timestamp: Date.now() });
});
const server = app.listen(config.port, config.host, () => {
  console.log(`Express health endpoint: http://${config.host}:${config.port}/health`);
});

// Initialize Holster Relay with built-in WebSocket server and connection management
const holster = Holster({
  port: config.port,
  secure: true,
  peers: [], // No peers by default
  maxConnections: config.maxConnections,
  file: config.storageEnabled ? config.storagePath : undefined,
  // Other Holster options can be placed here
});

// Graceful shutdown
const shutdown = () => {
  console.log('Received shutdown signal, closing server...');
  server.close(() => {
    console.log('Express server closed');
    process.exit(0);
  });
  // Holster will close its WS server automatically on process exit
};
process.on('SIGTERM', shutdown);
process.on('SIGINT', shutdown);
