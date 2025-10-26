# MerkleMap DAG - The Best Option! ⭐

## TL;DR

**Use `MerkleMap` for your DAG needs** - it's available NOW in o1js v2.10.0 and provides:
- ✅ Key-value interface (eventHash → parentHash)
- ✅ Essentially unlimited capacity (height 256)
- ✅ Natural mapping for DAG relationships
- ✅ Simpler than MerkleTree (no index tracking)
- ✅ Similar to future IndexedMerkleMap

## Why MerkleMap is Best

### vs MerkleTree
| Feature | MerkleTree | **MerkleMap** ⭐ |
|---------|------------|------------------|
| Interface | Index-based | **Key-value** |
| Capacity | 33 million | **Unlimited** |
| Index Tracking | Manual | **Automatic** |
| Natural DAG Mapping | No (index → hash) | **Yes (hash → hash)** |
| Complexity | Higher | **Lower** |

### vs IndexedMerkleMap (Future)
| Feature | IndexedMerkleMap | **MerkleMap** ⭐ |
|---------|------------------|------------------|
| Status | Future (v2.12+) | **NOW (v2.10.0)** |
| Interface | Key-value | **Key-value** |
| Capacity | 4 billion | **Unlimited** |
| API Complexity | More features | **Simpler** |

## Architecture

```
Event ID (hash) ──→ MerkleMap ──→ Parent ID (hash)
                       │
                       ├─→ get(eventHash) → parentHash
                       ├─→ set(eventHash, parentHash)
                       ├─→ getRoot() → commitment
                       └─→ getWitness(eventHash) → proof
```

Perfect for DAG because:
- Event hash IS the key
- Parent hash IS the value
- No artificial index mapping needed!

## Usage

```typescript
import {
  initializeDAGMapCircuits,
  buildDAGMapFromEvents,
  proveMembershipInDAGMap,
  proveLineageInDAGMap,
  queryParentInDAGMap,
  getDAGMapStats
} from '$lib/commons/compute/zk';

// 1. Initialize (once)
await initializeDAGMapCircuits();

// 2. Build DAG from events
const events: ProvenanceEvent[] = [/* your events */];
const dagMap = buildDAGMapFromEvents(events);

// 3. Query (off-chain)
const parent = queryParentInDAGMap(dagMap, childEventId);
console.log('Parent:', parent);

const stats = getDAGMapStats(dagMap);
console.log('Root:', stats.root);
console.log('Capacity:', stats.maxCapacity); // ~1.8e76 events!

// 4. Prove membership (on-chain/ZK)
const { proof, dagRoot } = await proveMembershipInDAGMap(
  dagMap,
  eventId
);

// 5. Prove lineage (on-chain/ZK)
const { proof: lineageProof } = await proveLineageInDAGMap(
  dagMap,
  childId,
  parentId
);

// Verifier only sees:
// - Root hash
// - Proof (~2KB)
// - Event hashes being proved
// All other events stay PRIVATE!
```

## Example: Build DAG

```typescript
import { createDAGMap, hashToField } from '$lib/commons/compute/zk';
import { Field } from 'o1js';

// Create empty DAG
const dagMap = createDAGMap();

// Add genesis event (no parent)
const genesisHash = hashToField('0xabc...');
dagMap.addEvent(genesisHash, Field(0)); // parent = 0 for root

// Add child events
const event1Hash = hashToField('0xdef...');
dagMap.addEvent(event1Hash, genesisHash); // parent = genesis

const event2Hash = hashToField('0x123...');
dagMap.addEvent(event2Hash, event1Hash); // parent = event1

// Query relationships
const parent = dagMap.getParent(event2Hash);
console.log('Parent of event2:', parent.toString());

// Get root commitment
const root = dagMap.getRoot();
console.log('DAG root:', root.toString());
```

## Comparison Table

| DAG Implementation | Status | Capacity | Interface | Best For |
|-------------------|--------|----------|-----------|----------|
| **MerkleMap** ⭐ | ✅ NOW | Unlimited | Key-value | **Most use cases** |
| MerkleTree | ✅ NOW | 33M | Index | Fixed-size needs |
| MerkleList | ✅ NOW | Unlimited | Sequential | Chronological logs |
| IndexedMerkleMap | ⏳ Future | 4B | Key-value | When v2.12+ available |

## Recommendation

**Use MerkleMap for your DAG!**

It's:
1. **Available NOW** (o1js v2.10.0)
2. **Natural fit** (hash → hash mapping)
3. **Unlimited capacity** (height 256)
4. **Simpler** than MerkleTree (no indices)
5. **Future-proof** (similar to IndexedMerkleMap)

## Migration from MerkleTree

If you started with MerkleTree:

```typescript
// OLD: MerkleTree approach
import { createDAG, addEvent } from './zk';
const dag = createDAG();
const index = dag.addEvent(eventHash); // returns index
const witness = dag.getWitness(index); // need index

// NEW: MerkleMap approach (SIMPLER!)
import { createDAGMap } from './zk';
const dagMap = createDAGMap();
dagMap.addEvent(eventHash, parentHash); // natural mapping
const witness = dagMap.getWitness(eventHash); // use hash directly
```

## Performance

- **Build**: O(N) where N = events
- **Query**: O(1) hash lookup
- **Proof generation**: O(256) hashes (constant)
- **Verification**: O(256) hash checks (constant)
- **Space**: O(N) stored mappings

Height 256 means proof contains 256 hashes, but this is still constant size (~8KB) regardless of DAG size.

## Summary

MerkleMap is the **sweet spot**:
- ✅ Available today
- ✅ Perfect for DAG (hash → hash)
- ✅ Unlimited capacity
- ✅ Simple API
- ✅ Provable in circuits
- ✅ No waiting for IndexedMerkleMap

**Use MerkleMap for your provable DAG needs!** 🎉

