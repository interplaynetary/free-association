# Lambda Calculus RPC System

Symmetric peer-to-peer RPC implementation for the Free Association Lambda Calculus framework.

## Overview

This RPC system enables **symmetric peer-to-peer communication** where any entity can connect to any other entity without a client/server distinction. Built on Cap'n Web RPC principles with offline-first architecture and sparse matrix optimizations.

### Key Features

- **🔄 Symmetric Protocol**: No client/server distinction - both sides are equal peers
- **🔐 Mutual Authentication**: Both parties verify each other's identity
- **💾 Offline-First**: Operations queue locally and sync when reconnected
- **⚡ Sparse Operations**: Efficient for large networks (133× memory reduction)
- **🔗 Multiple Transports**: WebSocket, postMessage, WebRTC
- **📊 CRDT-Style Sync**: Vector clock conflict resolution
- **🎯 Budget Constraints**: Automatic enforcement of recognition limits

## Architecture

```
Entity A (Browser)           Entity B (Browser/Worker)
┌──────────────────┐        ┌──────────────────┐
│  IndexedDB       │        │  IndexedDB       │
│  (sparse graph)  │        │  (sparse graph)  │
└────────┬─────────┘        └────────┬─────────┘
         │                           │
┌────────▼─────────┐        ┌────────▼─────────┐
│  EntitySession   │◄──RPC──►│  EntitySession   │
│  (RpcTarget)     │        │  (RpcTarget)     │
└──────────────────┘        └──────────────────┘
```

Both entities export their `EntitySession` as capability and can call each other symmetrically.

## Quick Start

### Basic P2P Connection

```typescript
import { createP2PConnection } from '@free-association/lambda-calculus/rpc';

// Alice connects
const alice = await createP2PConnection('alice', 'websocket', 'ws://relay-server');

// Bob connects
const bob = await createP2PConnection('bob', 'websocket', 'ws://relay-server');

// Mutual authentication
await alice.mutualAuthenticate(aliceProof);
await bob.mutualAuthenticate(bobProof);

// Alice allocates recognition to Bob
await alice.getLocalSession().allocateRecognition('bob', 0.6);

// Bob can query Alice's data
const mr = await bob.getRemoteSession().getMutualRecognition('alice');
console.log('Mutual recognition:', mr); // 0 (Bob hasn't allocated back yet)
```

### Offline-First Operation

```typescript
// Make changes while offline - they get queued
await session.allocateRecognition('bob', 0.5);
await session.allocateRecognition('charlie', 0.3);

// When back online, process sync queue
const synced = await session.processSyncQueue(async (update) => {
  // Send to remote peer
  await remotePeer.receiveSyncUpdate(update);
});

console.log(`Synced ${synced} operations`);
```

### Collective Coordination

```typescript
// Form collective based on MRD threshold
const collectiveMembers = ['alice', 'bob', 'charlie'];

for (const entityId of candidates) {
  const mrd = await session.getMRD(collectiveMembers);
  
  if (mrd >= MRD_THRESHOLD) {
    collective.push(entityId);
  }
}
```

## Core Components

### EntitySession

The symmetric RPC target representing an authenticated entity. Both parties export this.

```typescript
class EntitySession {
  // Identity
  async verifyIdentity(proof: Credential): Promise<boolean>
  
  // Recognition operations (budget-constrained)
  async allocateRecognition(targetId: string, amount: number): Promise<void>
  async revokeRecognition(targetId: string): Promise<void>
  async getMyAllocations(): Promise<SerializedSparseGraph>
  
  // Query operations (sparse & cached)
  async getMutualRecognition(otherId: string): Promise<number>
  async getMRS(universeIds: string[]): Promise<Distribution>
  async getTMR(universeIds: string[]): Promise<number>
  async getMRD(collectiveMembers: string[]): Promise<number>
  
  // Sync operations
  async receiveSyncUpdate(update: SyncUpdate): Promise<void>
  async subscribeSyncUpdates(callback: SyncCallback): Promise<void>
}
```

### PeerConnection

Manages symmetric connection between two EntitySessions.

```typescript
const connection = await PeerConnection.connect({
  localEntityId: 'alice',
  transport: {
    type: 'websocket',
    url: 'ws://relay-server',
    options: {
      reconnect: true,
      heartbeatInterval: 30000
    }
  },
  autoSync: true
});

// Access sessions
const local = connection.getLocalSession();  // Alice's session
const remote = connection.getRemoteSession(); // Bob's session (stub)
```

### BrowserStorage

IndexedDB-based persistent storage with sparse graph optimization.

```typescript
const storage = new BrowserStorage('alice');
await storage.initialize();

// Sparse operations
await storage.setRecognitionEdge('alice', 'bob', 0.6);
const amount = await storage.getRecognitionEdge('alice', 'bob');

// Outgoing/incoming edges (O(k) where k = connections)
const outgoing = await storage.getOutgoingEdges('alice');
const incoming = await storage.getIncomingEdges('alice');

// Full graph
const graph = await storage.loadSparseGraph();
```

### RecognitionCache

TTL-based cache with LRU eviction.

```typescript
const cache = new RecognitionCache({
  maxSize: 10000,
  defaultTTL: 5 * 60 * 1000,  // 5 minutes
  cleanupInterval: 60 * 1000   // 1 minute
});

// Cache MR result
cache.set('mr:alice:bob', 0.5);
const mr = cache.get('mr:alice:bob');

// Invalidate when recognition changes
cache.invalidateEntity('alice');
```

## Capability Management

The RPC system uses Cap'n Web RPC capability references:

### Export/Import Tables

```typescript
// Alice's perspective after connection:
exports: {
  0: aliceSession,        // Her main session
  -1: updateCallback,     // Function she passed to Bob
  -2: aliceCollective     // Collective she shared
}

imports: {
  0: bobSession,          // Bob's main session (RpcStub)
  1: bobMRSResult        // Result of a push operation
}
```

### What Gets Passed Over Network?

**Primitives** (serialized directly):
- Numbers, strings, booleans
- Arrays: `[["item1", "item2"]]`

**Capabilities** (passed by reference):
- Entity sessions: `["ref", exportId]`
- Callback functions: `["ref", exportId]`

**Recognition data** (sparse!):
```typescript
{
  type: 'sparse-graph',
  edges: [
    ["alice", "bob", 0.6],
    ["alice", "charlie", 0.4]
  ]
}
```

## Network Efficiency

### Sparse vs Dense

| Data | Dense | Sparse | Reduction |
|------|-------|--------|-----------|
| 10k entities, 500k edges | 800 MB | 6 MB | 133× |
| Typical update (1 edge) | Full matrix | 12 bytes | 66,666× |
| MRS query result | 10k numbers | ~50 numbers | 200× |

### Example

```typescript
// 10k entities with avg 50 connections per entity:
// - Dense: 10k × 10k = 100M entries = ~800MB
// - Sparse: 10k × 50 = 500k entries = ~6MB
// - Operations: O(k) instead of O(n) where k << n
```

## Authentication

### Credential Types

```typescript
// Public key authentication
const pubkeyProof: Credential = {
  type: 'pubkey',
  publicKey: 'ed25519-public-key',
  signature: 'signature-of-challenge',
  challenge: 'random-challenge'
};

// DID authentication
const didProof: Credential = {
  type: 'did',
  did: 'did:key:z6Mkf...',
  proof: 'verification-proof'
};

// OAuth authentication
const oauthProof: Credential = {
  type: 'oauth',
  provider: 'github',
  token: 'github-oauth-token',
  tokenType: 'Bearer'
};
```

### Mutual Authentication

```typescript
// Both parties verify each other
await alice.mutualAuthenticate(aliceProof);
await bob.mutualAuthenticate(bobProof);

// Now both are authenticated
if (alice.isAuthenticated() && bob.isAuthenticated()) {
  // Can make RPC calls
}
```

## Transports

### WebSocket

```typescript
import { createWebSocketTransport } from '@free-association/lambda-calculus/rpc/transports';

const transport = createWebSocketTransport('ws://relay-server', {
  reconnect: true,
  reconnectDelay: 1000,
  heartbeatInterval: 30000
});

await transport.connect();
```

### postMessage (iframe/Worker)

```typescript
import { createIframeTransport, createWorkerTransport } from '@free-association/lambda-calculus/rpc/transports';

// Iframe
const iframeTransport = createIframeTransport(iframe, 'https://origin.com');

// Worker
const workerTransport = createWorkerTransport(worker);
```

### WebRTC (P2P)

```typescript
import { createWebRTCTransport } from '@free-association/lambda-calculus/rpc/transports';

const transport = createWebRTCTransport([
  { urls: 'stun:stun.l.google.com:19302' }
]);

// Exchange offers/answers via signaling server
const offer = await transport.createOffer();
// Send offer to remote peer via signaling
// ...
const answer = await transport.handleOffer(remoteOffer);
```

## Examples

See `rpc/examples/` for comprehensive examples:

- **peer-to-peer.ts**: Basic P2P connections, multi-entity networks
- **offline-sync.ts**: Offline operations, conflict resolution, batch sync
- **collective-coordination.ts**: Collective formation, resource allocation, MRD calculations

Run all examples:
```typescript
import { runAllExamples } from '@free-association/lambda-calculus/rpc/examples';
await runAllExamples();
```

## Performance

### Sparse Operations

```typescript
// Instead of iterating all 10k entities:
for (const entity of allEntities) {  // O(n)
  // ...
}

// Only iterate actual connections:
for (const [targetId, amount] of outgoing) {  // O(k) where k << n
  // ...
}
```

### Caching

```typescript
// First call: Compute from storage
const mrs = await session.getMRS(universe);  // ~10ms

// Subsequent calls: From cache
const mrs2 = await session.getMRS(universe); // ~0.01ms

// Invalidate on change
await session.allocateRecognition('bob', 0.7);
// Cache auto-invalidated for affected entities
```

## Testing

Run tests:
```bash
npm test
```

Test files:
- `__tests__/cache.test.ts` - RecognitionCache tests
- `__tests__/serialization.test.ts` - Serialization tests
- `../src/sparse/__tests__/sparse-ops.test.ts` - Sparse operations tests

## API Reference

### Types

```typescript
import type {
  // Core types
  EntityId,
  SparseRecognitionGraph,
  Distribution,
  
  // RPC types
  Credential,
  SyncUpdate,
  SyncOperation,
  SyncCallback,
  
  // Transport types
  TransportType,
  TransportConfig,
  ConnectionInfo,
  ConnectionState,
  
  // Cache types
  CacheEntry,
  CacheKey
} from '@free-association/lambda-calculus/rpc';
```

### Functions

```typescript
import {
  // Main API
  createP2PConnection,
  PeerConnection,
  EntitySession,
  BrowserStorage,
  RecognitionCache,
  
  // Transports
  createWebSocketTransport,
  createIframeTransport,
  createWorkerTransport,
  createWebRTCTransport,
  
  // Serialization
  serializeSparseGraph,
  deserializeSparseGraph,
  
  // Examples
  runAllExamples
} from '@free-association/lambda-calculus/rpc';
```

## Integration with Existing Package

The RPC layer is **optional** - the existing lambda-calculus elegant/core APIs work standalone. RPC adds:

- Network synchronization between entities
- Browser-based persistence
- Offline-first operation
- Multi-entity coordination

```typescript
// Existing API still works
import { elegant } from '@free-association/lambda-calculus';
const mr = elegant.mutual(matrix)(alice)(bob);

// RPC adds network capabilities
import { createP2PConnection } from '@free-association/lambda-calculus/rpc';
const peer = await createP2PConnection('alice', 'websocket');
const remoteMR = await peer.getRemoteSession().getMutualRecognition('bob');
```

## License

SEE LICENSE IN ../../LICENSE.md

