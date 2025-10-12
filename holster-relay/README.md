# Holster Relay Server

Standalone Holster relay server for the free-association project.

## Configuration

Configure via environment variables:

- `HOLSTER_RELAY_HOST` - Host to bind to (default: `0.0.0.0`)
- `HOLSTER_RELAY_PORT` - Port to listen on (default: `8766`)
- `HOLSTER_RELAY_STORAGE` - Enable persistent storage (default: `true`)
- `HOLSTER_RELAY_STORAGE_PATH` - Path for data storage (default: `./holster-data`)
- `HOLSTER_MAX_CONNECTIONS` - Maximum concurrent WS connections (default: `100`)

## How it Works

This relay uses Holster's built-in WebSocket server and connection management. All configuration (port, storage, connection limits) is passed directly to Holster in `server.js`, which handles all WebSocket protocol, connection, persistence and relay logic.

Manual WebSocket server setup, connection limiting, and event tracking are no longer necessary—Holster manages all relay responsibilities internally.

Express is used only for a `/health` endpoint (for Docker/monitoring). All real client connections and relay logic are handled by Holster on `ws://<host>:<port>/holster`.

## Running Locally

```bash
bun install
bun run server.js
```

## Running with Docker

```bash
docker build -t holster-relay .
docker run -p 8766:8766 -v $(pwd)/holster-data:/app/holster-data holster-relay
```

> The Dockerfile now uses the Bun runtime (oven/bun:latest).

## Holster Endpoint

The Holster WebSocket server is accessible at: `ws://localhost:8766/holster`

Health check endpoint: `http://localhost:8766/health`
