# Capabilities vs Signatures: When Do We Need Each? 🤔

## Your Intuition

> "Somehow we could avoid the signing layer if we just did capabilities"

**You're partially right!** Let's analyze when each is needed.

## Two Different Security Problems

### Problem 1: Authorization (Who Can Call What?)

**Solution: Capabilities** ✅

```typescript
// Pure capability-based security (no signatures needed!)
const relay = await connectToRelay();

// Authenticate once
const session = await relay.authenticate(password);

// Session IS the authorization!
await session.allocateRecognition('bob', 0.8);
await session.getMutualRecognition('alice', 'bob');

// Security: Can't call these without the session object
// No signatures needed here! The RPC system prevents forgery.
```

**Why capabilities work here:**
- ✅ Real-time, live connection
- ✅ RPC system controls object references
- ✅ Can't forge a session object
- ✅ Having the reference = having the permission

### Problem 2: Data Integrity (Can We Trust Stored Data?)

**Solution: Signatures** ✅

```typescript
// Later, when restoring from replicas...
const fragments = await fetchFromReplicas(publicKey);

// ⚠️ How do we know replicas didn't tamper with this data?
// ⚠️ How do we know these updates really came from Alice?
// ⚠️ Capabilities don't help here - this is stored data!

// Need cryptographic proof!
for (const update of fragments) {
  const isReallyFromAlice = await verifySignature(update, alicePublicKey);
  // ✅ Signature proves authenticity
}
```

**Why capabilities DON'T work here:**
- ❌ No live connection to verify with
- ❌ Data stored by untrusted third parties (replicas)
- ❌ No RPC system to prevent forgery
- ❌ Historical data, not real-time operations

## The Key Distinction

```
┌─────────────────────────────────────────────────────────────┐
│         LIVE SESSION (Capabilities Sufficient)               │
│                                                              │
│  Client ←──────── WebSocket ──────→ Server                  │
│                                                              │
│  const session = await relay.authenticate()                 │
│  await session.allocateRecognition('bob', 0.8)              │
│                                                              │
│  Security: RPC system prevents forging session              │
│  ✅ No signatures needed for authorization!                 │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│      PERSISTENT STATE (Signatures Required)                  │
│                                                              │
│  Client                Replica 1 (stores data)              │
│    ↓                   Replica 2 (stores data)              │
│  Logout                Replica 3 (stores data)              │
│    ...                                                       │
│  Login                                                       │
│    ↓                                                         │
│  Fetch state from replicas                                  │
│                                                              │
│  ⚠️ How do we know replicas didn't tamper?                  │
│  ✅ Cryptographic signatures prove authenticity!            │
└─────────────────────────────────────────────────────────────┘
```

## Where Each Security Mechanism Applies

### Capabilities Handle (No Signatures Needed)

```typescript
// ✅ Real-time RPC calls
const session = await relay.authenticate(challenge, signature);
await session.allocateRecognition('bob', 0.8);

// ✅ Passing callbacks
session.subscribe((update) => {
  console.log('Update:', update);
});

// ✅ Returning objects
const userProfile = await session.getUserProfile();
await userProfile.updateName('Alice');

// ✅ Authorization
// Having the session object = authorized!
```

### Signatures Handle (Capabilities Not Enough)

```typescript
// ✅ State restoration from replicas
const fragments = await replica.getStateFor(publicKey);
// Need to verify each update was really signed by the entity

// ✅ Offline storage
const localData = await localStorage.getItem('my-state');
// Need to verify it wasn't tampered with

// ✅ Multi-party consensus
const fragments = await Promise.all(
  replicas.map(r => r.getState())
);
// Need to verify all replicas agree and data is authentic

// ✅ Historical audit
const allUpdates = await getHistoricalUpdates();
// Need to prove each update was really made by the entity

// ✅ Byzantine fault tolerance
// Some replicas might be malicious - need cryptographic proof
```

## Could We Use Only Capabilities?

Let's try...

### Attempt 1: Trust the Replicas ❌

```typescript
// ❌ Pure capability approach
const session = await relay.authenticate(password);
const state = await session.restoreState();

// Problem: How does the replica know this state is authentic?
// What if a malicious user creates fake state?
// No cryptographic proof!
```

### Attempt 2: Capabilities All The Way Down ❌

```typescript
// ❌ Try to use capability pattern for storage
class Replica extends RpcTarget {
  async storeState(session: EntitySession, state: State) {
    // Store the state
    this.states.set(session.entityId, state);
  }
}

// Problem: What if client logs out, then logs back in?
// We need to fetch state, but we don't have the original session object!
// The session object doesn't persist across logins.
```

### Attempt 3: Long-Lived Capabilities ❌

```typescript
// ❌ Keep the session object forever?
class Replica {
  private sessions = new Map<EntityId, EntitySession>();
  
  async storeState(session: EntitySession, state: State) {
    // Keep the session reference
    this.sessions.set(session.entityId, session);
  }
}

// Problems:
// 1. Sessions can't outlive the connection (WebSocket closes)
// 2. No persistence across server restarts
// 3. No way to verify old data is authentic
// 4. Memory leak (sessions never cleaned up)
// 5. Can't distribute across replicas
```

## The Fundamental Problem

**Capabilities are about WHO has access NOW.**

**Signatures are about WHAT was done THEN.**

```
Capabilities = Authorization (present)
  "I have this session object, so I can call these methods"
  
Signatures = Authenticity (past)
  "This data was created by Alice yesterday, here's the proof"
```

## The Hybrid Model (What We Need)

### During Live Session: Capabilities

```typescript
// 1. Authenticate with challenge-response (ONE signature here)
const challenge = await relay.createChallenge();
const signature = await signChallenge(challenge, privateKey);
const session = await relay.authenticate(challenge, signature);

// 2. Use capabilities for everything else (no signatures!)
await session.allocateRecognition('bob', 0.8);
await session.getMutualRecognition('alice', 'bob');
await session.getMRS('alice');

// ✅ Fast! No signing on every call
// ✅ Clean! Capability-based authorization
// ✅ Type-safe! Can't call without session
```

### For Persistent State: Signatures

```typescript
// When storing to replicas, sign the data
const update = {
  from: 'alice',
  to: 'bob',
  value: 0.8,
  timestamp: Date.now(),
  nonce: crypto.randomUUID()
};

const signature = await sign(update, privateKey);

await replica.storeSignedUpdate({
  ...update,
  signature,
  publicKey
});

// ✅ Cryptographic proof of authenticity
// ✅ Replicas can't forge updates
// ✅ Works across sessions/connections
```

### On Restoration: Verify Signatures

```typescript
// When restoring, verify all signatures
const fragments = await Promise.all(
  replicas.map(r => r.getSignedUpdates(publicKey))
);

for (const update of allUpdates) {
  const isAuthentic = await verifySignature(update);
  
  if (!isAuthentic) {
    // Forged! Reject it.
    console.error('Tampered data detected!');
    continue;
  }
  
  // Only apply verified updates
  applyUpdate(update);
}

// ✅ Byzantine fault tolerance
// ✅ Tamper detection
// ✅ Cryptographic proof
```

## Optimized Architecture

### Layer 1: Capabilities (Fast, Clean, Type-Safe)

```typescript
// All real-time operations use pure capabilities
class SecureEntitySession extends RpcTarget {
  async allocateRecognition(to: string, amount: number) {
    // Just do it! No signing needed for authorization.
    // The fact that you HAVE this session object = authorized!
    
    // ... but we DO sign for persistence ...
  }
}
```

### Layer 2: Signatures (Only When Persisting)

```typescript
class SecureEntitySession extends RpcTarget {
  async allocateRecognition(to: string, amount: number) {
    // Update in-memory state (fast, no signature)
    this.inMemoryState.set(to, amount);
    
    // When persisting, THEN sign
    if (this.needsPersistence()) {
      const signedUpdate = await this.secureContext.signUpdate(to, amount);
      await this.persistToReplicas(signedUpdate);
    }
  }
}
```

## Decision Matrix

| Scenario | Capabilities? | Signatures? | Why |
|----------|--------------|-------------|-----|
| **RPC authorization** | ✅ Yes | ❌ No | Real-time, RPC system prevents forgery |
| **Passing callbacks** | ✅ Yes | ❌ No | RPC system handles references |
| **Live updates** | ✅ Yes | ❌ No | Trusted connection |
| **Storing to replica** | ⚠️ Helps | ✅ Yes | Need proof for later |
| **Loading from replica** | ❌ No | ✅ Yes | No live session, need proof |
| **Cross-session data** | ❌ No | ✅ Yes | Capabilities don't persist |
| **Byzantine tolerance** | ❌ No | ✅ Yes | Need cryptographic proof |
| **Audit trail** | ❌ No | ✅ Yes | Need historical proof |

## The Answer

**You're right that we can avoid signatures for AUTHORIZATION!**

```typescript
// ✅ Pure capabilities for authorization
const session = await relay.authenticate(challenge, signature);
await session.allocateRecognition('bob', 0.8);
// No signature on this call! Session object IS the auth.
```

**But we still need signatures for DATA INTEGRITY!**

```typescript
// ✅ Signatures for persistent data
const signedUpdate = await signStateUpdate(update, privateKey, publicKey);
await replica.store(signedUpdate);

// Later...
const updates = await replica.getUpdates();
for (const update of updates) {
  await verifySignature(update);  // Prove it's authentic!
}
```

## Elegant Compromise

### What We Can Optimize Away

```typescript
// ❌ DON'T sign every RPC call
await session.allocateRecognition('bob', 0.8);
// This call itself doesn't need a signature!
// The session capability provides authorization.

// ❌ DON'T verify every RPC response
const result = await session.getMutualRecognition('alice', 'bob');
// Result doesn't need signature verification!
// The connection is trusted.
```

### What We Must Keep

```typescript
// ✅ DO sign when persisting
async allocateRecognition(to: string, amount: number) {
  // Update in-memory (no signature)
  this.state.set(to, amount);
  
  // Persist with signature
  const signed = await this.sign({ from: this.id, to, amount });
  await this.storage.store(signed);
  await this.replicas.broadcast(signed);
}

// ✅ DO verify when loading
async restoreState() {
  const updates = await this.replicas.fetchAll();
  
  for (const update of updates) {
    if (!await this.verify(update)) {
      console.error('Forged update detected!');
      continue;
    }
    this.applyUpdate(update);
  }
}
```

## Summary

### Your Intuition Is Correct For:

✅ **Live RPC calls** - Pure capabilities work!  
✅ **Authorization** - Session object is enough!  
✅ **Real-time operations** - No signatures needed!

### But We Still Need Signatures For:

✅ **Persistent storage** - Data outlives the session  
✅ **State restoration** - Verifying old data  
✅ **Byzantine replicas** - Cryptographic proof  
✅ **Cross-session integrity** - Capabilities don't persist

## The Elegant Design

```typescript
// Authenticate once (capability + one signature)
const session = await relay.authenticate(challenge, signature);

// Use capabilities for everything live (no signatures!)
await session.allocateRecognition('bob', 0.8);
await session.getMutualRecognition('alice', 'bob');

// Behind the scenes: sign only when persisting
// (Transparent to the developer!)

// On restore: verify all persistent data
// (Also transparent to the developer!)
```

**Result:**
- 🚀 **Fast:** No signatures on every call
- 🔒 **Secure:** Signatures protect persistent data  
- ✨ **Elegant:** Capabilities for authorization, signatures for integrity
- 🎯 **Optimal:** Each mechanism used where it's strongest

---

**Capabilities for WHO. Signatures for WHAT. Both together = Complete security!** 🔑🔏

