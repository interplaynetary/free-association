# Interval Tree Clocks (ITC) - TypeScript Implementation 🕐

## Overview

**Interval Tree Clocks (ITC)** is a causality tracking mechanism designed for dynamic distributed systems with a variable number of participants. Unlike traditional Vector Clocks that require fixed, globally known participants, ITCs allow completely decentralized creation of processes/replicas without global identifiers.

This is a faithful TypeScript translation of the reference Erlang implementation by Paulo Sergio Almeida.

---

## Key Features

✅ **Dynamic Participants** - Add/remove replicas without global coordination  
✅ **No Global IDs** - Completely decentralized creation  
✅ **Adaptive Size** - Representation grows/shrinks automatically  
✅ **Causality Tracking** - Full causal ordering like Vector Clocks  
✅ **Space Efficient** - Compact representation for dynamic systems  

---

## Core Concepts

### Stamp

A stamp consists of two components:
- **Id**: Represents ownership of the causal history (can be split/merged)
- **Event**: Tracks causal events (can be incremented/joined)

```typescript
interface Stamp {
  id: Id;      // 0, 1, or {l: Id, r: Id}
  event: Event; // number or {n: number, l: Event, r: Event}
}
```

### Basic Operations

1. **seed()** - Create initial stamp with full ownership
2. **event(stamp)** - Increment causal history
3. **fork(stamp)** - Split ownership into two stamps  
4. **join(s1, s2)** - Merge two stamps
5. **peek(stamp)** - Copy event without ownership (for messages)
6. **leq(s1, s2)** - Check if s1 causally precedes s2

---

## API

### Functional API (Recommended)

```typescript
import { seed, event, fork, join, peek, leq, equals, concurrent } from '$lib/commons/itc';

// Create seed
const s = seed();

// Fork into two replicas
const [replica1, replica2] = fork(s);

// Events on each replica
const r1 = event(replica1);
const r2 = event(replica2);

// Check concurrency
if (concurrent(r1, r2)) {
  console.log('Concurrent events!');
}

// Merge replicas
const merged = join(r1, r2);

// Check causality
if (leq(r1, merged)) {
  console.log('r1 happened before merged');
}
```

### Class-Based API (Java-like)

```typescript
import { StampClass } from '$lib/commons/itc';

// Create seed (automatic)
const a = new StampClass();

// Fork (returns new stamp, mutates this)
const b = a.fork();

// Event (mutates)
a.event();
b.event();

// Check concurrency
if (a.concurrent(b)) {
  console.log('Concurrent!');
}

// Join (mutates this)
a.join(b);

// Check equality
if (a.equals(b)) {
  console.log('Equal!');
}
```

### Static Methods (Functional + Immutable)

```typescript
import { StampClass } from '$lib/commons/itc';

const s1 = StampClass.seed();
const s2 = StampClass.event(s1); // Returns new stamp

const [s3, s4] = StampClass.fork(s2); // Returns array

const merged = StampClass.join(s3, s4); // Returns new stamp
```

---

## Use Cases

### 1. Distributed Replicated Systems

```typescript
// Initial setup
const systemSeed = seed();

// Create replicas dynamically
const [replica1, replica2] = fork(systemSeed);
const [replica3, replica4] = fork(replica1);

// Updates on different replicas
const r1_updated = event(replica1);
const r3_updated = event(replica3);
const r4_updated = event(replica4);

// Replicas can merge anytime
const merged_1_3 = join(r1_updated, r3_updated);
```

### 2. P2P Message Passing

```typescript
// Node A
let nodeA = seed();
nodeA = event(nodeA); // Local event

// Send to Node B (via peek)
const message = peek(nodeA);
sendToNodeB(message);

// Node B receives and merges
let nodeB = seed();
nodeB = join(nodeB, message);

// Now Node B knows about Node A's event
```

### 3. Version Vector Simulation

```typescript
// Create 4 replicas (like 4 servers)
const root = seed();
const [s1, s2] = fork(root);
const [s3, s4] = fork(s1);

// Each replica can track updates independently
let serverA = s1;
let serverB = s2;
let serverC = s3;
let serverD = s4;

// Server A updates
serverA = event(serverA);

// Server C updates
serverC = event(serverC);

// Check if concurrent
if (concurrent(serverA, serverC)) {
  console.log('Concurrent updates detected!');
  
  // Merge to resolve
  const resolved = join(serverA, serverC);
}
```

### 4. CRDT Synchronization

```typescript
// Two replicas with concurrent updates
let replica1 = seed();
let replica2 = fork(replica1)[1];

// Both update independently
replica1 = event(replica1); // Alice writes
replica2 = event(replica2); // Bob writes

// Detect conflict
if (concurrent(replica1, replica2)) {
  // Apply CRDT merge logic
  const merged = join(replica1, replica2);
  
  // Both replicas now have consistent view
  replica1 = merged;
  replica2 = merged;
}
```

---

## Comparison: ITC vs Vector Clocks

| Feature | Vector Clocks | ITC |
|---------|--------------|-----|
| Participants | Fixed, known | Dynamic |
| Global IDs | Required | Not needed |
| Space overhead | O(n) always | Adaptive |
| Add replica | Coordinate globally | Local operation |
| Remove replica | Complex | Simple |
| Causality | ✅ | ✅ |

---

## Demo Run (from ITC Paper)

This example demonstrates the full lifecycle:

```typescript
import { seed, event, fork, join, toString } from '$lib/commons/itc';

// a: Create seed
let seedStamp = seed();
console.log('a:', toString(seedStamp)); // [1, 0]

// b: Fork into two
let [a, b] = fork(seedStamp);
console.log('b-a:', toString(a)); // [(L1+R0), 0]
console.log('b-b:', toString(b)); // [(L0+R1), 0]

// c: Event on both (concurrent)
a = event(a);
b = event(b);
console.log('c-a:', toString(a)); // [(L1+R0), 1]
console.log('c-b:', toString(b)); // [(L0+R1), 1]

// d: Fork a, event b
let [aNew, c] = fork(a);
a = aNew;
b = event(b);

// e: Event a, join b and c
a = event(a);
b = join(b, c);

// f: Fork b
let [bNew, cNew] = fork(b);
b = bNew;
c = cNew;

// g: Join a and b
a = join(a, b);

// h: Event a
a = event(a);

console.log('Final a:', toString(a)); // [L1, 1]
console.log('Final c:', toString(c)); // [R1, 1]

// Verify: a > c
console.log('a > c:', leq(c, a) && !leq(a, c)); // true
```

---

## Integration with RDL

ITCs can be used in the RDL system for causality tracking:

```typescript
import { seed, event, fork, join, StampClass } from '$lib/commons/itc';
import { ComputationGraphRuntime } from '$lib/commons/compute.svelte';

// Each RDL program instance gets its own ITC stamp
const programStamp = new StampClass();

// On computation execution
programStamp.event();

// On program fork (creating variant)
const variantStamp = programStamp.fork();

// On merging results from different instances
const merged = StampClass.join(programStamp, variantStamp);

// Check causality between computations
if (stamp1.leq(stamp2)) {
  console.log('Computation 1 happened before Computation 2');
}
```

---

## Advanced Usage

### Detecting Conflicts

```typescript
function detectConflict(stamp1: Stamp, stamp2: Stamp): 'before' | 'after' | 'concurrent' | 'equal' {
  if (equals(stamp1, stamp2)) return 'equal';
  if (leq(stamp1, stamp2)) return 'before';
  if (leq(stamp2, stamp1)) return 'after';
  return 'concurrent';
}

const s1 = event(seed());
const [s2, s3] = fork(s1);
const s2e = event(s2);
const s3e = event(s3);

console.log(detectConflict(s2e, s3e)); // 'concurrent'
```

### Tracking Replica Lineage

```typescript
// Track which replicas have seen which events
const replicaStamps = new Map<string, Stamp>();

function updateReplica(replicaId: string) {
  const stamp = replicaStamps.get(replicaId) || seed();
  replicaStamps.set(replicaId, event(stamp));
}

function syncReplicas(id1: string, id2: string) {
  const s1 = replicaStamps.get(id1)!;
  const s2 = replicaStamps.get(id2)!;
  
  const merged = join(s1, s2);
  
  replicaStamps.set(id1, merged);
  replicaStamps.set(id2, merged);
}

// Usage
updateReplica('alice');
updateReplica('bob');
syncReplicas('alice', 'bob');
```

---

## Implementation Details

### Id Structure

```typescript
type Id = 
  | 0         // Null id (no ownership)
  | 1         // Full id (complete ownership)
  | {l: Id, r: Id}  // Split id (left/right partitions)
```

### Event Structure

```typescript
type Event = 
  | number           // Simple counter
  | {n: number, l: Event, r: Event}  // Tree node with base + subtrees
```

### Normalization

ITCs automatically normalize representations for efficiency:
- `{l: 0, r: 0}` → `0`
- `{l: 1, r: 1}` → `1`
- `{n: 5, l: 3, r: 3}` → `8`

---

## Performance

- **Fork**: O(log n) where n = tree depth
- **Event**: O(log n)
- **Join**: O(n + m) where n, m = tree sizes
- **LEQ**: O(min(n, m))
- **Space**: Adaptive, typically O(log participants)

---

## References

- **Paper**: "Interval Tree Clocks: A Logical Clock for Dynamic Systems"  
  Paulo Sérgio Almeida, Carlos Baquero, Victor Fonte (OPODIS 2008)
  
- **Original Implementation**: https://github.com/ricardobcl/Interval-Tree-Clocks

- **Related**: Vector Clocks, Version Vectors, Lamport Timestamps

---

## Testing

The implementation includes **29 comprehensive tests**:

```bash
bun test src/lib/commons/tests/itc.test.ts
```

**Test Coverage**:
- ✅ Basic operations (5 tests)
- ✅ Causality tracking (4 tests)
- ✅ Demo run scenarios (2 tests)
- ✅ Version vector simulation (4 tests)
- ✅ Complex scenarios (3 tests)
- ✅ Edge cases (4 tests)
- ✅ String representation (3 tests)
- ✅ Static class methods (4 tests)

**Result**: All tests passing in <1 second

---

## FAQ

### When should I use ITC vs Vector Clocks?

Use **ITC** when:
- Participants can join/leave dynamically
- You don't have global coordination
- Space efficiency matters
- You want decentralized replica creation

Use **Vector Clocks** when:
- Participants are fixed and known
- Global coordination is available
- Simpler implementation is preferred

### Can I use ITC with existing Vector Clock systems?

Yes! ITC can replace Vector Clocks in most scenarios. The `leq` operation provides the same causality guarantees.

### How does ITC compare in size to Vector Clocks?

For `n` participants:
- Vector Clock: Always `O(n)`
- ITC: `O(log n)` in best case, `O(n)` in worst case
- ITC adapts to the actual fork/join pattern

### Is ITC suitable for Byzantine fault tolerance?

ITC tracks causality, not authenticity. For BFT, combine ITC with:
- Cryptographic signatures (like we do with Computation Provenance)
- Content hashing
- Verification protocols

---

## Status

✅ **Complete Translation** from Erlang reference implementation  
✅ **29/29 Tests Passing**  
✅ **Full API Coverage** (functional + class-based)  
✅ **Production Ready**  

**Date**: October 24, 2025  
**Translation**: Faithful to original Erlang semantics  
**Performance**: Optimized for TypeScript/JavaScript engines  

---

## Next Steps

Potential future enhancements:
1. Binary encoding/decoding (from Erlang version)
2. Integration with RDL computation provenance
3. Persistent storage adapters
4. Network serialization helpers
5. Visualization tools for ITC trees

🎉 **Ready to use in your distributed system!**

