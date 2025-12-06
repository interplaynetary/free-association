# Free Association - Symmetric Protocol Architecture

## The Core Insight: No "Client" or "Server"

From Cap'n Web documentation:
> "Since Cap'n Web is a symmetric protocol, there is no well-defined 'client' or 'server' at the protocol level. There are just two parties exchanging messages across a connection."

## Three Architecture Patterns

### 1. Centralized (Traditional Client-Server)

```
┌─────────────────────────────────────────────────────────────┐
│                    Cloudflare Workers                        │
│  ┌────────────────────────────────────────────────────────┐ │
│  │ ParticipantServer (Export ID: 0)                       │ │
│  │   ├─ authenticate() → AuthenticatedParticipant         │ │
│  │   └─ getPublicNetworkView() → NetworkState            │ │
│  └────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
                         ▲
                         │ RPC (WebSocket/HTTP)
                         │
         ┌───────────────┼───────────────┐
         │               │               │
    ┌────▼────┐    ┌────▼────┐    ┌────▼────┐
    │ Alice   │    │   Bob   │    │  Carol  │
    │ Client  │    │ Client  │    │ Client  │
    └─────────┘    └─────────┘    └─────────┘
```

**Characteristics:**
- Server is central authority
- All state stored centrally
- Clients query server for everything
- Simple to reason about
- Single point of failure

**When to use:** MVP, small scale (< 1,000 users), simple governance

### 2. Peer-to-Peer (Pure Distributed)

```
┌──────────────────┐         ┌──────────────────┐
│   Alice's Node   │◄───────►│   Bob's Node     │
│ PeerParticipant  │         │ PeerParticipant  │
│  (Export ID: 0)  │         │  (Export ID: 0)  │
└────────┬─────────┘         └─────────┬────────┘
         │                             │
         │                             │
         │         ┌──────────────────┐│
         └────────►│  Carol's Node    │◄
                   │ PeerParticipant  │
                   │  (Export ID: 0)  │
                   └──────────────────┘
```

**Characteristics:**
- No central server
- Each node runs same code
- Direct peer-to-peer transfers
- Fully decentralized
- Resilient to node failures

**When to use:** Maximum decentralization, censorship resistance, offline-first

### 3. Hybrid (Coordinator + Peer-to-Peer)

```
                  ┌──────────────────────┐
                  │  NetworkCoordinator  │
                  │   (Discovery only)   │
                  │    (Export ID: 0)    │
                  └───────────┬──────────┘
                              │
           ┌──────────────────┼──────────────────┐
           │                  │                  │
    1. Register          1. Register       1. Register
           │                  │                  │
           ▼                  ▼                  ▼
    ┌──────────┐       ┌──────────┐      ┌──────────┐
    │  Alice   │       │   Bob    │      │  Carol   │
    │   Peer   │       │   Peer   │      │   Peer   │
    └────┬─────┘       └────┬─────┘      └────┬─────┘
         │                  │                  │
         │  2. Get stub     │                  │
         └──────────────────┤                  │
                            │                  │
         ┌──────────────────┤                  │
         │  3. Direct P2P   ▼                  │
         └───────────────►  │                  │
                            │  4. Direct P2P   │
                            └──────────────────►
```

**Characteristics:**
- Coordinator for discovery (lightweight)
- Capacity transfers peer-to-peer (efficient)
- Hybrid resilience
- Scales well (coordinator is simple)

**When to use:** Production (10,000+ users), balance of simplicity and decentralization

## How Symmetric Protocol Works

### Export Tables

Each side maintains an export table with signed integer IDs:

**Alice's Export Table:**
```
[0]  = Alice's main object (PeerParticipant)
[-1] = Bob's stub (object Alice passed to Bob)
[-2] = Callback object (passed to Bob)
[1]  = Result of Bob's method call
[2]  = Result of another call
```

**Bob's Export Table:**
```
[0]  = Bob's main object (PeerParticipant)
[-1] = Alice's stub (object Bob passed to Alice)
[-2] = Callback object (passed to Alice)
[1]  = Result of Alice's method call
[2]  = Result of another call
```

### Message Flow (Symmetric!)

```
ALICE                          BOB
  │                             │
  ├─ ["push", ["pipeline", 0, "sendCapacity", [100]]]
  │  (Call Bob's sendCapacity)  →
  │                             ├─ Execute method
  │                             ├─ Callback to Alice!
  │  ← ["push", ["pipeline", -2, "onCapacityReceived", [100]]]
  │  (Bob calls Alice's callback)
  ├─ Execute callback           │
  │                             │
  ├─ ["resolve", 3, 100]        →
  │  (Alice responds to Bob)    │
  │                             │
```

**Notice:**
- Messages go BOTH directions
- Either side can initiate calls
- Callbacks are just RPC stubs in reverse direction
- Completely symmetric!

## Capability Security in Symmetric Protocol

### Traditional (ACL-Based):

```typescript
// Every method checks permissions
function sendCapacity(userId, recipientId, amount) {
  if (!authenticate(userId)) throw "Unauthorized";
  if (!hasPermission(userId, "transfer")) throw "Forbidden";
  if (getBalance(userId) < amount) throw "Insufficient";
  // ... actual logic ...
}
```

### Symmetric Capability-Based:

```typescript
// No permission checks - capability IS the permission!
class PeerParticipant extends RpcTarget {
  async sendCapacity(recipient, amount) {
    // If this method is called, caller HAS the capability
    // `this.capacity` is bound to THIS instance
    // `recipient` is an unforgeable stub
    if (this.capacity < amount) throw "Insufficient";
    this.capacity -= amount;
    await recipient.receiveCapacity(this.id, amount);
  }
}
```

**Why this works:**
1. Can't call method without the stub (unforgeable reference)
2. Each instance has its own capacity (bound at construction)
3. Recipient stub is verified by Cap'n Web (export table)
4. No global permission checks needed!

## Scaling Properties

### Single Node (Centralized)
```
Capacity: 1,000-10,000 participants
Bottleneck: Single server WebSocket connections
Memory: O(n) for participant state
Latency: 10-50ms (single hop)
Cost: $5-100/month (Cloudflare Workers)
```

### Peer-to-Peer (Distributed)
```
Capacity: Unlimited (adds linearly per node)
Bottleneck: Peer discovery
Memory: O(connections) per node
Latency: 20-100ms (direct peer connection)
Cost: ~$5/month per node
```

### Hybrid (Best of Both)
```
Capacity: 100,000+ participants
Bottleneck: Coordinator queries (but read-only)
Memory: O(connections) on coordinator, O(active_peers) on participants
Latency: 30-80ms (discovery + P2P)
Cost: $20-200/month (small coordinator + many peers)
```

## Implementation Examples

### Centralized Server

```typescript
// server.ts
import { newWorkersRpcResponse } from 'capnweb';
import { ParticipantServer } from './protocol.js';

export default {
  fetch(request: Request) {
    return newWorkersRpcResponse(request, new ParticipantServer());
  }
};
```

### Peer Node (Both Client AND Server)

```typescript
// peer.ts
import { RpcTarget, newWebSocketRpcSession } from 'capnweb';
import { PeerParticipant } from './example-symmetric.js';

// This node is BOTH:
// 1. Server (exports PeerParticipant at ID 0)
const myPeer = new PeerParticipant("alice@example.com");
const server = createWebSocketServer(myPeer);
server.listen(8787);

// 2. Client (connects to other peers)
const bobStub = newWebSocketRpcSession("ws://bob-node:8787");
await myPeer.connectToPeer(bobStub);

// Now both can call each other!
await myPeer.sendCapacity(bobStub, 100);  // Alice → Bob
await bobStub.sendCapacity(myPeer, 50);   // Bob → Alice
```

### Hybrid Coordinator + Peers

```typescript
// coordinator.ts (lightweight!)
import { newWorkersRpcResponse } from 'capnweb';
import { NetworkCoordinator } from './example-symmetric.js';

export default {
  fetch(request: Request) {
    return newWorkersRpcResponse(request, new NetworkCoordinator());
  }
};

// peer.ts (same as above, but registers with coordinator)
const coordinatorStub = newWebSocketRpcSession("wss://coordinator.example.com");
await coordinatorStub.registerPeer("alice@example.com", myPeerStub);

// Discovery
const bobStub = await coordinatorStub.findPeer("bob@example.com");

// Direct connection (coordinator not involved anymore!)
await myPeer.connectToPeer(bobStub);
await myPeer.sendCapacity(bobStub, 100); // Peer-to-peer!
```

## Why This Matters for Free Association

### Traditional Systems
```
All capacity flows through central server
  ↓
Single point of control
  ↓
Requires trust in central authority
  ↓
Can censor, manipulate, surveil
```

### Symmetric Capability System
```
Capacity flows peer-to-peer
  ↓
No central control point
  ↓
Trust in mathematics + cryptography
  ↓
Cannot censor, manipulate, or surveil
```

## Mathematical Properties + Symmetric Protocol

| Axiom | Mathematical Property | Symmetric Protocol Enforcement |
|-------|----------------------|-------------------------------|
| Axiom 1: Budget | Σ R(a,x) = 1 | RecognitionBudget is bound to each peer instance |
| Axiom 2: Mutual | MR(a,b) = min(R(a,b), R(b,a)) | Requires stubs to BOTH peers |
| Axiom 3: Flow | C(a,b) = κ_b × g(MR) | Capacity bound to peer instance |
| Axiom 4: Goal | P(G) = f(Σ C(a,b)) | Goals tracked in peer instance |
| Axiom 5: Beneficial | Only b∈B contributes | Beneficial set is private Set<Stub> |

**Every axiom is enforced locally at each peer, no global state needed!**

## Conclusion

The symmetric protocol enables three deployment models:
1. **Centralized**: Simple, traditional (works today)
2. **Peer-to-Peer**: Fully decentralized (maximum resilience)
3. **Hybrid**: Best of both worlds (practical for scale)

All three use the **SAME protocol.ts code** - just different deployment!

This is the power of Cap'n Web's symmetric design combined with Free Association's mathematical foundations. ✨

