# Example Applications

Complete working examples showing the elegance of the recognition-based RPC system.

## Simple Chat

Recognition-based peer-to-peer chat.

```typescript
import { ChatClient } from './simple-chat';

// Create client (no initialize!)
const alice = new ChatClient('alice', 'ws://localhost:8080');
await alice.start();

// Recognize another user
await alice.recognizeUser('bob', 0.7);

// Send message
await alice.sendMessage('bob', 'Hello!');

// Check mutual recognition
const mr = await alice.getMutualRecognition('bob');
```

**Features:**
- No explicit `initialize()` call
- Natural callback subscriptions
- Recognition-based routing
- Automatic serialization

**Run it:**
```bash
# Start relay server
bun run server/node.ts

# Run chat example
bun run examples/apps/simple-chat.ts
```

## More Examples (Coming Soon)

- **Collaborative Docs** - Google Docs-style with recognition-based access
- **P2P Network** - Direct peer-to-peer without relay
- **Recognition DAO** - Governance weighted by mutual recognition
- **Federated AI** - AI agents coordinating via recognition

## Key Improvements Demonstrated

### 1. No Initialize Call

**Before:**
```typescript
const api = newWebSocketSession('alice', 'wss://...');
await api.initialize(); // ← Extra step
const mr = await api.getMutualRecognition('bob');
```

**After:**
```typescript
const api = newWebSocketSession('alice', 'wss://...');
// Just works - auto-initializes!
const mr = await api.getMutualRecognition('bob');
```

### 2. Natural Callbacks

**Before:**
```typescript
session.subscribeSyncUpdates((update) => {
  // Handle update
});
```

**After:**
```typescript
await api.subscribe(update => {
  console.log('Recognition changed:', update);
});
```

### 3. Automatic Serialization

Everything serializes automatically - Maps, ITC Stamps, Sparse Graphs, etc.

```typescript
// Just pass anything - it works!
await api.allocateRecognition('bob', 0.7);
const graph = await api.getSparseGraph(); // Auto-deserializes!
```

### 4. Complete Server Example

Both client AND server code provided:

**Client:**
```typescript
const alice = newWebSocketSession('alice', 'wss://relay.example.com');
const mr = await alice.getMutualRecognition('bob');
```

**Server (Cloudflare Workers):**
```typescript
export default {
  fetch(request) {
    return newWorkersRpcResponse(request, new RelayServer());
  }
}
```

**Server (Node.js/Bun):**
```bash
bun run server/node.ts
```

## Architecture

```
┌──────────────┐         ┌──────────────┐
│   Alice      │◄───────►│   Bob        │
│  (Client)    │         │  (Client)    │
└──────┬───────┘         └──────┬───────┘
       │                        │
       │    ┌──────────────┐    │
       └───►│ Relay Server │◄───┘
            │  (Workers    │
            │   /Node.js)  │
            └──────────────┘
```

## Recognition Flow

1. **Register** - Entities register with relay
2. **Recognize** - Allocate recognition to peers
3. **Connect** - Form connections based on MR
4. **Coordinate** - Messages, updates, collaboration
5. **Evolve** - Recognition changes over time

## Next Steps

1. Run the examples
2. Modify for your use case
3. Deploy relay to Cloudflare Workers
4. Build your recognition-based app!

## Learn More

- **API Reference:** `../OVERVIEW.md`
- **Server Setup:** `../server/README.md`
- **Tests:** `../__tests__/`

