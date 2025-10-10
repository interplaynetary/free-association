# Gun Relay Server

Standalone Gun relay server for the free-association project.

## Configuration

Configure via environment variables:

- `GUN_RELAY_HOST` - Host to bind to (default: `0.0.0.0`)
- `GUN_RELAY_PORT` - Port to listen on (default: `8765`)
- `GUN_RELAY_STORE` - Enable persistent storage (default: `true`)
- `GUN_RELAY_PATH` - Path to serve static files (default: `public`)
- `GUN_RELAY_SHOW_QR` - Show QR code on startup (default: `false`)

## Running Locally

```bash
npm install
npm start
```

## Running with Docker

```bash
docker build -t gun-relay .
docker run -p 8765:8765 -v $(pwd)/store:/app/store gun-relay
```

## Gun Endpoint

The Gun database is accessible at: `http://localhost:8765/gun`
