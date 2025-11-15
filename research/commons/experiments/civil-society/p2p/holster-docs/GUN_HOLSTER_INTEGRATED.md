# Gun & Holster Relays - Now Integrated! 🎉

The Gun and Holster relays have been successfully integrated into the SvelteKit application!

## What Changed

### ✅ Integrated Services

Both Gun and Holster relays now run automatically when you start the SvelteKit dev server. No separate Docker containers needed!

**Gun Relay:**
- Runs on port `8765` (configurable)
- WebSocket endpoint: `ws://localhost:8765/gun`
- Persists data to `gun-data/` directory

**Holster Relay:**
- Runs on port `8766` (configurable)
- WebSocket endpoint: `ws://localhost:8766/holster`
- Persists data to `holster-data/` directory
- Health endpoint: `http://localhost:8766/health`

### 📁 New Files Created

```
src/
├── hooks.server.ts                      # Initializes relays on startup
├── lib/server/relays/
│   ├── gun-relay.ts                     # Gun relay wrapper
│   └── holster-relay.ts                 # Holster relay wrapper
└── routes/api/relays/
    ├── gun/+server.ts                   # Gun relay API info
    └── holster/+server.ts               # Holster relay API info
```

### 📦 Dependencies Added

- `express` - For Holster health endpoint
- `ws` - WebSocket support for Holster
- Already had: `@gun-vue/relay` and `@mblaney/holster`

## Configuration

Add these to your `.env` file:

```env
# Gun Relay Configuration
GUN_RELAY_HOST=0.0.0.0
GUN_RELAY_PORT=8765
GUN_RELAY_STORE=true
GUN_RELAY_PATH=gun-data
GUN_RELAY_SHOW_QR=false

# Holster Relay Configuration
HOLSTER_RELAY_HOST=0.0.0.0
HOLSTER_RELAY_PORT=8766
HOLSTER_RELAY_STORAGE=true
HOLSTER_RELAY_STORAGE_PATH=./holster-data
HOLSTER_MAX_CONNECTIONS=100
```

## Usage

### Starting Everything

Just run the dev server as normal:

```bash
bun run dev
```

You'll see console output showing both relays starting:

```
=== GUN RELAY SERVER ===
Configuration: {
  "host": "0.0.0.0",
  "port": 8765,
  "store": true,
  "path": "gun-data",
  "showQr": false
}
✅ Gun Relay Server started successfully
   Listening on: 0.0.0.0:8765
   Storage: enabled at gun-data
   WebSocket: ws://0.0.0.0:8765/gun
========================

=== HOLSTER RELAY SERVER ===
Configuration: {
  "host": "0.0.0.0",
  "port": 8766,
  "storageEnabled": true,
  "storagePath": "./holster-data",
  "maxConnections": 100
}
✅ Express health endpoint: http://0.0.0.0:8766/health
✅ Holster Relay Server started successfully
   Listening on: 0.0.0.0:8766
   Storage: enabled at ./holster-data
   WebSocket: ws://0.0.0.0:8766/holster
   Max connections: 100
============================
```

### Testing the Relays

**Gun Relay Info:**
```bash
curl http://localhost:5173/api/relays/gun
```

Response:
```json
{
  "status": "ok",
  "service": "gun-relay",
  "timestamp": 1234567890,
  "config": {
    "host": "0.0.0.0",
    "port": 8765,
    "url": "ws://0.0.0.0:8765/gun"
  }
}
```

**Holster Relay Info:**
```bash
curl http://localhost:5173/api/relays/holster
```

Response:
```json
{
  "status": "ok",
  "service": "holster-relay",
  "timestamp": 1234567890,
  "initialized": true,
  "running": true,
  "config": {
    "host": "0.0.0.0",
    "port": 8766,
    "url": "ws://0.0.0.0:8766/holster"
  }
}
```

**Holster Health Check:**
```bash
curl http://localhost:8766/health
```

Response:
```json
{
  "status": "ok",
  "service": "holster-relay",
  "timestamp": 1234567890,
  "connections": 100
}
```

## Client Configuration

Your existing client code should work without changes! The relays run on the same ports as before:

```typescript
// In src/lib/config.ts
export const config = {
  gun: {
    peers: [
      'http://localhost:8765/gun',  // Local relay
      // Fallback peers...
    ]
  },
  holster: {
    peers: [
      'ws://localhost:8766/holster',  // Local relay
      // Fallback peers...
    ]
  }
};
```

## How It Works

### Initialization Flow

1. **Server Starts** → `hooks.server.ts` is loaded
2. **Relays Initialize** → Both Gun and Holster relays start
3. **Separate Servers** → Each relay runs on its own port
4. **SvelteKit Runs** → Your app runs on port 5173
5. **All Ready** → Make requests to any service

### Architecture

```
┌─────────────────────────────────────────┐
│         SvelteKit Server (5173)         │
│                                         │
│  ┌────────────────────────────────┐    │
│  │   API Routes (/api/*)          │    │
│  │   - /api/ai/completion         │    │
│  │   - /api/llm/route             │    │
│  │   - /api/keys/status           │    │
│  │   - /api/relays/gun            │    │
│  │   - /api/relays/holster        │    │
│  └────────────────────────────────┘    │
└─────────────────────────────────────────┘

┌─────────────────────┐  ┌──────────────────────┐
│  Gun Relay (8765)   │  │ Holster Relay (8766) │
│                     │  │                      │
│  WebSocket Server   │  │  WebSocket Server    │
│  Data: gun-data/    │  │  Data: holster-data/ │
└─────────────────────┘  └──────────────────────┘
```

## Benefits

### Before (Docker):
- ❌ Needed Docker installed
- ❌ Separate `docker-compose up` command
- ❌ Multiple containers to manage
- ❌ Complex networking setup
- ❌ Harder to debug

### After (Integrated):
- ✅ Single `bun run dev` command
- ✅ No Docker required
- ✅ Automatic startup
- ✅ Simpler configuration
- ✅ Easier debugging
- ✅ All logs in one place

## Data Persistence

Both relays persist data to local directories:

```
free-association/
├── gun-data/           # Gun database files
├── holster-data/       # Holster database files
└── src/
```

These directories are created automatically on first run.

### Backup Your Data

```bash
# Backup Gun data
tar czf gun-backup.tar.gz gun-data/

# Backup Holster data
tar czf holster-backup.tar.gz holster-data/
```

### Reset Data

```bash
# Clear Gun data
rm -rf gun-data/

# Clear Holster data
rm -rf holster-data/
```

## Production Deployment

The relays will work in production too! Just make sure to:

1. Set proper environment variables
2. Use appropriate storage paths
3. Configure firewall rules for ports 8765 and 8766
4. Consider using a process manager (PM2, systemd)

### Example Production .env

```env
GUN_RELAY_HOST=0.0.0.0
GUN_RELAY_PORT=8765
GUN_RELAY_STORE=true
GUN_RELAY_PATH=/var/lib/gun-data
GUN_RELAY_SHOW_QR=false

HOLSTER_RELAY_HOST=0.0.0.0
HOLSTER_RELAY_PORT=8766
HOLSTER_RELAY_STORAGE=true
HOLSTER_RELAY_STORAGE_PATH=/var/lib/holster-data
HOLSTER_MAX_CONNECTIONS=500
```

## Troubleshooting

### Ports Already in Use

If ports 8765 or 8766 are already in use:

```env
# Change to different ports
GUN_RELAY_PORT=18765
HOLSTER_RELAY_PORT=18766
```

Then update your client config to match.

### Relays Not Starting

Check console output for errors. Common issues:

1. **Missing dependencies**: Run `bun install`
2. **Port conflicts**: Change ports in `.env`
3. **Permission issues**: Check write access to data directories

### Connection Refused

Make sure:
1. Relays started successfully (check console)
2. Firewall isn't blocking ports
3. Using correct URLs in client config

## API Endpoints

### New Relay Endpoints

```
GET /api/relays/gun       # Gun relay configuration
GET /api/relays/holster   # Holster relay configuration and status
```

### Existing Endpoints

All existing API endpoints still work:
- `/api/ai/*` - AI proxy
- `/api/llm/*` - LLM router  
- `/api/keys/*` - Key pool

## Summary

✅ **Gun Relay** - Integrated and running on 8765  
✅ **Holster Relay** - Integrated and running on 8766  
✅ **No Docker needed** - Everything in one process  
✅ **Automatic startup** - Runs with dev server  
✅ **Data persistence** - Local file storage  
✅ **Health checks** - API endpoints available  

**Total Services Now Integrated:** 5
1. AI Proxy
2. LLM Router
3. Key Pool
4. Gun Relay
5. Holster Relay

All running with a single `bun run dev` command! 🚀

