# Cap'n Web Promise Pipelining Guide

## Overview

**Promise pipelining** is Cap'n Web's killer feature that allows chaining multiple RPC calls in a **single network round trip**. This guide shows you how to leverage it effectively in the Free Association Protocol.

---

## What is Promise Pipelining?

Traditional RPC requires awaiting each call:

```typescript
// ❌ 3 round trips (slow)
const session = await api.authenticate(id, creds);
const network = await session.getNetworkState();
const mr = await network.computeMutualRecognition("alice", "bob");
```

With promise pipelining, you **don't await** intermediate results:

```typescript
// ✅ 1 round trip (fast!)
const session = api.authenticate(id, creds);
const network = session.getNetworkState();
const mr = await network.computeMutualRecognition("alice", "bob");
```

**Key insight:** Cap'n Web returns **proxy objects** that look like promises but support immediate method calls.

---

## Performance Impact

| Pattern | Round Trips | Example Latency |
|---------|-------------|-----------------|
| Sequential (await each) | N calls = N trips | 3 calls = 300ms (100ms each) |
| Pipelined (await last) | 1 trip | 3 calls = 100ms total |
| **Speedup** | **N× faster** | **3× faster** ⚡ |

For a typical 3-call chain:
- **Without pipelining**: 300ms (3 × 100ms)
- **With pipelining**: 100ms (1 round trip)
- **Improvement**: 3× faster!

---

## How It Works

### 1. Proxy Objects

Cap'n Web returns **JavaScript Proxy** objects that intercept method calls:

```typescript
const session = api.authenticate(id, creds);
// session is a Proxy, not a regular Promise!

const network = session.getNetworkState();
// Calling method on Proxy = pipelined call

const mr = await network.computeMutualRecognition("alice", "bob");
// Only await the final result
```

### 2. Message Batching

Under the hood, Cap'n Web sends **one message** with multiple operations:

```json
[
  ["push", ["pipeline", 0, "authenticate", [id, creds]]],
  ["push", ["pipeline", 1, "getNetworkState", []]],
  ["push", ["pipeline", 2, "computeMutualRecognition", ["alice", "bob"]]],
  ["pull", 3]
]
```

All three calls execute server-side in sequence, returning only the final result.

---

## Usage Patterns

### Pattern 1: Linear Chain

Most common pattern - straight chain of dependent calls:

```typescript
const result = await api
  .authenticate(id, creds)
  .getNetworkState()
  .computeMutualRecognition("alice", "bob");

// 3 calls = 1 round trip
```

### Pattern 2: Branching Chains

Fork from a common point:

```typescript
const session = api.authenticate(id, creds);

// Branch 1: Network operations
const network = session.getNetworkState();
const mr = network.computeMutualRecognition("alice", "bob");
const total = network.computeTotalMR("alice");

// Branch 2: Budget operations
const budget = session.getRecognitionBudget();
const recognition = budget.getRecognitionTo("bob");

// Await all branches (still 1 round trip!)
const [mrResult, totalResult, recResult] = await Promise.all([
  mr, total, recognition
]);

// 5 calls = 1 round trip
```

### Pattern 3: Parallel Chains

Multiple independent chains:

```typescript
const network1 = api.authenticate(id1, creds1).getNetworkState();
const network2 = api.authenticate(id2, creds2).getNetworkState();

const [mr1, mr2] = await Promise.all([
  network1.computeMutualRecognition("alice", "bob"),
  network2.computeMutualRecognition("bob", "alice")
]);

// 6 calls = 2 round trips (2 separate chains)
```

### Pattern 4: Pipeline Builder

For complex chains, use the builder pattern:

```typescript
import { pipeline } from './client';

const result = await pipeline(api)
  .call('authenticate', id, creds)
  .call('getNetworkState')
  .call('computeMutualRecognition', "alice", "bob")
  .execute();
```

---

## With Memoization

Our implementation combines pipelining with memoization for **best of both worlds**:

```typescript
import { createPipelinedClient } from './client';

const pipelinedApi = createPipelinedClient(api);

// First call: 1 round trip (pipelined)
const mr1 = await pipelinedApi
  .authenticate(id, creds)
  .getNetworkState()
  .computeMutualRecognition("alice", "bob");

// Second call: 0 round trips (cached!)
const mr2 = await pipelinedApi
  .authenticate(id, creds)
  .getNetworkState()
  .computeMutualRecognition("alice", "bob");
```

**Cache key** based on full pipeline:
```
pipeline:authenticate(alice)-
>getNetworkState()->computeMutualRecognition(alice,bob)
```

---

## Best Practices

### ✅ DO

**1. Don't await intermediate results:**
```typescript
✅ const result = await api.method1().method2().method3();
❌ const result = await (await (await api.method1()).method2()).method3();
```

**2. Use Promise.all for parallel branches:**
```typescript
✅ const [r1, r2] = await Promise.all([chain1, chain2]);
❌ const r1 = await chain1; const r2 = await chain2;
```

**3. Await only final results:**
```typescript
✅ const data = await api.auth().getState().query();
❌ const session = await api.auth();
   const state = await session.getState();
   const data = await state.query();
```

**4. Share common prefixes:**
```typescript
✅ const session = api.authenticate(id, creds);
   const mr1 = await session.getNetworkState().computeMR(a, b);
   const mr2 = await session.getNetworkState().computeMR(a, c);
❌ const mr1 = await api.authenticate(id, creds).getNetworkState().computeMR(a, b);
   const mr2 = await api.authenticate(id, creds).getNetworkState().computeMR(a, c);
```

### ❌ DON'T

**1. Don't await in the middle of a chain:**
```typescript
❌ const session = await api.authenticate(id, creds);
   const network = session.getNetworkState();
   // Breaks pipelining!
```

**2. Don't use await in loops:**
```typescript
❌ for (const id of ids) {
     results.push(await api.query(id)); // N round trips
   }
✅ const promises = ids.map(id => api.query(id));
   const results = await Promise.all(promises); // 1 round trip
```

**3. Don't pipeline write operations without care:**
```typescript
❌ // These might fail without proper error handling
   api.allocate(target1, amount1);
   api.allocate(target2, amount2);
   await api.allocate(target3, amount3);
```

---

## HTTP Batch Mode

For **one-time queries**, use HTTP batch mode instead of WebSocket:

```typescript
import { LocalFirstBatchClient } from './client';

const batch = new LocalFirstBatchClient(serverUrl);
await batch.authenticate(id, creds);

// Multiple queries in single HTTP request
const [mr1, mr2, total] = await Promise.all([
  batch.getMutualRecognition("alice", "bob"),
  batch.getMutualRecognition("alice", "carol"),
  batch.computeTotalMR("alice")
]);
```

**When to use:**
- ✅ One-time queries (dashboard load, analytics)
- ✅ Simple read operations
- ✅ No real-time updates needed
- ❌ Ongoing connections
- ❌ Frequent writes
- ❌ Bidirectional communication

---

## Real-World Examples

### Dashboard Loading

```typescript
async function loadDashboard(userId: string) {
  const session = api.authenticate(userId, credentials);
  const network = session.getNetworkState();
  const budget = session.getRecognitionBudget();
  
  // Load all metrics in parallel (1 round trip!)
  const [
    mrWithBob,
    mrWithCarol,
    totalMR,
    totalBudget,
    recognitionToBob
  ] = await Promise.all([
    network.computeMutualRecognition(userId, "bob"),
    network.computeMutualRecognition(userId, "carol"),
    network.computeTotalMR(userId),
    budget.getTotalAllocated(),
    budget.getRecognitionTo("bob")
  ]);
  
  return { mrWithBob, mrWithCarol, totalMR, totalBudget, recognitionToBob };
}

// 7 operations = 1 round trip! ⚡
```

### Multi-User Query

```typescript
async function compareUsers(userA: string, userB: string) {
  // Setup both sessions (can be parallel)
  const sessionA = api.authenticate(userA, credsA);
  const sessionB = api.authenticate(userB, credsB);
  
  const networkA = sessionA.getNetworkState();
  const networkB = sessionB.getNetworkState();
  
  // Query from both perspectives
  const [mrFromA, mrFromB] = await Promise.all([
    networkA.computeMutualRecognition(userA, userB),
    networkB.computeMutualRecognition(userB, userA)
  ]);
  
  return { mrFromA, mrFromB, symmetric: mrFromA === mrFromB };
}

// 6 operations = 2 round trips (one per user)
```

### Batch Analytics

```typescript
async function computeNetworkMetrics(userIds: string[]) {
  const session = api.authenticate(adminId, adminCreds);
  const network = session.getNetworkState();
  
  // Compute metrics for all users in parallel
  const metrics = await Promise.all(
    userIds.map(id => network.computeTotalMR(id))
  );
  
  return userIds.map((id, i) => ({ id, totalMR: metrics[i] }));
}

// N users = 1 round trip! ⚡
```

---

## Debugging Pipelined Calls

### Enable Logging

```typescript
import { createPipelinedClient } from './client';

const pipelinedApi = createPipelinedClient(api, {
  enableLogging: true
});

// Logs:
// [PIPELINE-CACHE-MISS] pipeline:authenticate(...)->getNetworkState()->...
// [PIPELINE-CACHE-HIT] pipeline:authenticate(...)->getNetworkState()->...
```

### Visualize Round Trips

```typescript
console.time('Operation');

const result = await api
  .authenticate(id, creds)
  .getNetworkState()
  .computeMutualRecognition("alice", "bob");

console.timeEnd('Operation');
// Operation: 100ms (expected ~1 round trip)
```

### Check Network Tab

In browser dev tools:
- **Without pipelining**: 3 WebSocket messages (or 3 HTTP requests)
- **With pipelining**: 1 WebSocket message (or 1 HTTP request)

---

## Performance Benchmarks

### Benchmark: Dashboard Load

| Metric | Without Pipelining | With Pipelining | Improvement |
|--------|-------------------|-----------------|-------------|
| Round trips | 7 | 1 | 7× fewer |
| Latency (100ms RTT) | 700ms | 100ms | 7× faster |
| Latency (50ms RTT) | 350ms | 50ms | 7× faster |
| Throughput | 1.4 req/s | 10 req/s | 7× higher |

### Benchmark: Multi-User Query

| Users | Without Pipelining | With Pipelining | Improvement |
|-------|-------------------|-----------------|-------------|
| 10 users | 60 round trips | 10 round trips | 6× fewer |
| 100 users | 600 round trips | 100 round trips | 6× fewer |
| 1000 users | 6000 round trips | 1000 round trips | 6× fewer |

---

## Compatibility

| Transport | Pipelining | Batch Mode |
|-----------|------------|------------|
| **WebSocket** | ✅ Full support | N/A |
| **HTTP Batch** | ✅ Full support | ✅ Yes |
| **HTTP Streaming** | ✅ Full support | N/A |
| **postMessage** | ✅ Full support | N/A |

---

## Summary

✅ **Promise pipelining** reduces round trips by chaining calls  
✅ **Don't await** intermediate results  
✅ **Use Promise.all** for parallel branches  
✅ **Combine with memoization** for instant cached responses  
✅ **HTTP batch mode** for one-time queries  
✅ **Real-world speedup**: 3-10× faster!  

**Key takeaway:** With pipelining, think in terms of **call chains** not individual calls. Design your APIs for chaining!

---

## See Also

- [example-pipelining.ts](./example-pipelining.ts) - Comprehensive examples
- [example-batch-mode.ts](./example-batch-mode.ts) - HTTP batch mode
- [example-fluent-api.ts](./example-fluent-api.ts) - Fluent chaining patterns
- [LOCAL-FIRST.md](./LOCAL-FIRST.md) - Local-first architecture
- [Cap'n Web Blog Post](https://blog.cloudflare.com/capnweb-javascript-rpc-library/) - Original article

