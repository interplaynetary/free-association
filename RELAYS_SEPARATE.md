# Gun & Holster Relays - Running Separately

## Why Separate?

Gun and Holster relays are **standalone server applications** that:
- Create their own HTTP/WebSocket servers
- Bind to specific ports (8765, 8766)
- Are long-running processes
- Don't fit SvelteKit's environment-agnostic architecture

Following **SvelteKit best practices**, these should run as separate processes.

## What's Integrated

✅ **Successfully migrated to SvelteKit:**
1. **AI Proxy** - `/api/ai/*` routes
2. **LLM Router** - `/api/llm/*` routes  
3. **Key Pool** - `/api/keys/*` routes

These are now SvelteKit server routes and work perfectly!

## Running the Relays

### Option 1: Use the Helper Script

```bash
# Terminal 1: Start SvelteKit
bun run dev

# Terminal 2: Start relays
./start-relays.sh
```

### Option 2: Docker (Recommended for Production)

```bash
# Start just the relays
cd server
docker-compose up gun-relay holster-relay
```

### Option 3: Manual

```bash
# Terminal 1: SvelteKit
bun run dev

# Terminal 2: Gun relay
cd server/gun-relay && node server.js

# Terminal 3: Holster relay
cd server/holster-relay && node server.js
```

## Configuration

The relays use environment variables from the server folder:

**Gun Relay** (port 8765):
```env
GUN_RELAY_HOST=0.0.0.0
GUN_RELAY_PORT=8765
GUN_RELAY_STORE=true
GUN_RELAY_PATH=public
```

**Holster Relay** (port 8766):
```env
HOLSTER_RELAY_HOST=0.0.0.0
HOLSTER_RELAY_PORT=8766
HOLSTER_RELAY_STORAGE=true
HOLSTER_RELAY_STORAGE_PATH=./holster-data
```

## Client Configuration

Your client code already points to these ports:

```typescript
// src/lib/config.ts
export const config = {
  gun: {
    peers: [
      'http://localhost:8765/gun',  // Gun relay
      // Fallback peers...
    ]
  },
  holster: {
    peers: [
      'ws://localhost:8766/holster',  // Holster relay
      // Fallback peers...
    ]
  }
};
```

## Do You Need Them?

**You may not need to run them locally!**

Your config already has fallback public peers:
- Gun: `https://104.248.129.153/gun` and others
- Holster: `wss://holster.haza.website`

Try running just `bun run dev` first. If your app works, you don't need local relays!

## Production Deployment

### SvelteKit App
Deploy normally to any SvelteKit-compatible host:
- Vercel
- Netlify  
- Your own server
- Any Node.js host

### Relays
Deploy separately:
- **Docker**: Use the existing `server/docker-compose.yml`
- **Separate VPS**: Run Gun and Holster on different servers
- **Use Public Relays**: Just use the public fallbacks

## Why This is Better

✅ **Follows SvelteKit best practices**
✅ **No Vite bundling issues**
✅ **Simpler development** - one command for SvelteKit
✅ **Flexible deployment** - deploy relays independently
✅ **Optional relays** - use public peers if you want
✅ **Better separation** - database layer separate from app layer

## Summary

**For Development:**
```bash
# Just the app (uses public peers)
bun run dev

# OR with local relays
bun run dev
./start-relays.sh  # in another terminal
```

**What Changed:**
- ❌ Removed Gun/Holster from `src/lib/server/relays/`
- ❌ Removed `hooks.server.ts` relay initialization
- ✅ Kept AI Proxy, LLM Router, Key Pool (they're proper routes)
- ✅ Added `start-relays.sh` helper script
- ✅ Documented separate deployment

**Result:** Clean SvelteKit architecture + optional separate relays! 🎉

