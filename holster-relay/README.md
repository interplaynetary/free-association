# Holster Relay Server

Standalone Holster relay server for the free-association project.

## Configuration

Configure via environment variables:

- `HOLSTER_RELAY_HOST` - Host to bind to (default: `0.0.0.0`)
- `HOLSTER_RELAY_PORT` - Port to listen on (default: `8766`)
- `HOLSTER_RELAY_STORAGE` - Enable persistent storage (default: `true`)
- `HOLSTER_RELAY_STORAGE_PATH` - Path for data storage (default: `./holster-data`)

## Running Locally

```bash
npm install
npm start
```

## Running with Docker

```bash
docker build -t holster-relay .
docker run -p 8766:8766 -v $(pwd)/holster-data:/app/holster-data holster-relay
```

## Holster Endpoint

The Holster WebSocket server is accessible at: `ws://localhost:8766/holster`

Health check endpoint: `http://localhost:8766/health`
