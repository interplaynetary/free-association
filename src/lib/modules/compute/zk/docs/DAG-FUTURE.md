# IndexedMerkleMap DAG (Future Enhancement)

## Status

⚠️ **Not Available in v2.10.0**  
Current version: v2.10.0  
Available in: v2.12+ (estimated)

`IndexedMerkleMap` was introduced in o1js v2.12+ as part of the `Experimental` namespace.

**Verified**: Not present in v2.10.0 Experimental exports.  
**Expected**: Available in future o1js versions (check latest releases).

## Upgrade Path

```bash
npm install o1js@latest
```

Then use the DAG implementation in `zk-dag.ts`.

## Why IndexedMerkleMap?

### Current Implementation (`provenance-dag.svelte.ts`)
- ❌ Uses JavaScript Map (not provable)
- ❌ Manual Merkle tree construction
- ❌ Manual witness generation
- ✅ Works with current o1js version

### Future Implementation (`zk-dag.ts`)
- ✅ Native o1js IndexedMerkleMap (provable!)
- ✅ Automatic Merkle proofs
- ✅ Use directly in ZkProgram private inputs
- ✅ Optimized for ZK operations
- ❌ Requires o1js v2.12+

## Key Benefits

### 1. Provable in Circuits

```typescript
ZkProgram({
  methods: {
    test: {
      privateInputs: [ProvenanceDAG, Field],
      
      method(dag: ProvenanceDAG, eventHash: Field) {
        // Prove event exists WITHOUT revealing entire DAG
        let parent = dag.get(eventHash);
        parent.assertEquals(expectedParent);
      }
    }
  }
})
```

### 2. Automatic Witness Generation

No more manual Merkle proof construction - o1js handles it!

### 3. Massive Capacity

```typescript
// Height 32 = 4 billion events
class ProvenanceDAG extends IndexedMerkleMap(32) {}

// Height 40 = 1 trillion events
class LargeProvenanceDAG extends IndexedMerkleMap(40) {}

// Max height 52 = 4.5 quadrillion events
```

### 4. Clean API

```typescript
const dag = new ProvenanceDAG();

// Insert
dag.insert(eventHash, parentHash);

// Query
let parent = dag.get(eventHash);         // Throws if not found
let parent = dag.getOption(eventHash);   // Returns Option type

// Root for commitment
let root = dag.root;
```

## Use Cases Unlocked

### 1. Private DAG Membership Proofs

```typescript
// Prove "Event X is in the DAG" without revealing:
// - Other events in the DAG
// - DAG structure
// - DAG size

const proof = await proveMembership(dag, eventId);
// Verifier only sees: root hash + proof + claimed event
```

### 2. Provable Lineage

```typescript
// Prove "Y is ancestor of X" without revealing:
// - Intermediate events
// - Full path
// - Other branches

const proof = await proveLineage(dag, childId, ancestorId);
```

### 3. Incremental DAG Updates

```typescript
// Prove DAG update is valid
// Old root → New root
// Proves new event links to existing parent

const { proof, newRoot } = await proveDAGUpdate(
  dag,
  newEventId,
  parentId
);
```

### 4. Cross-System Proofs

```typescript
// Share DAG root as commitment
// Generate proofs to other systems
// Verify without syncing entire DAG

const root = dag.root;
// Publish root to blockchain/IPFS/etc
// Generate proofs on-demand
```

## Architecture

```
┌─────────────────────────────────────────────────┐
│ Provenance Events                                │
└───────────────┬─────────────────────────────────┘
                │
                │ buildDAGFromEvents()
                │
┌───────────────▼─────────────────────────────────┐
│ IndexedMerkleMap DAG                            │
│                                                   │
│  ┌──────────────┐                               │
│  │ Root Hash    │ ← Public Commitment           │
│  └──────┬───────┘                               │
│         │                                        │
│  ┌──────▼───────────────────┐                   │
│  │  Sparse Merkle Tree      │                   │
│  │  eventHash → parentHash  │                   │
│  │  (4 billion capacity)    │                   │
│  └──────────────────────────┘                   │
└─────────────────────────────────────────────────┘
                │
                │ Private Input
                │
┌───────────────▼─────────────────────────────────┐
│ ZkPrograms                                       │
│ - DAGMembershipProgram                          │
│ - DAGLineageProgram                             │
│ - DAGUpdateProgram                              │
└─────────────────────────────────────────────────┘
```

## Implementation Preview

See `zk-dag.ts` and `zk-dag-integration.svelte.ts` for the full implementation (ready for o1js v2.12+).

## Migration Path

### Phase 1: Current (o1js v2.10)
- ✅ Use existing `provenance-dag.svelte.ts`
- ✅ Use existing `provenance-proof.svelte.ts`
- ✅ Manual Merkle proofs

### Phase 2: Upgrade (o1js v2.12+)
- Upgrade o1js
- Enable `zk-dag.ts` and `zk-dag-integration.svelte.ts`
- Start building IndexedMerkleMap DAG in parallel
- Test provable operations

### Phase 3: Transition
- Migrate existing DAG data to IndexedMerkleMap
- Switch to provable DAG operations
- Deprecate manual Merkle proof construction

### Phase 4: Future
- Cross-system proofs
- DAG commitments on blockchain
- Recursive proof composition
- Efficient dispute resolution

## Performance Comparison

### Manual Merkle Tree (Current)
- Insert: O(1) Map operation
- Proof generation: O(h) where h = tree height
- Proof size: O(h) hashes
- Verification: O(h) hash operations

### IndexedMerkleMap (Future)
- Insert: O(h) with circuit overhead
- Proof generation: Automatic witness
- Proof size: O(1) - constant ZK proof
- Verification: O(1) - constant time
- **Bonus**: Provable in circuits!

## Timeline

- **Now**: Document & prepare implementation
- **When o1js v2.12+ available**: Drop-in replacement
- **Estimated effort**: 1-2 days testing + migration

## References

- [o1js IndexedMerkleMap docs](https://docs.o1labs.org/o1js/api-reference/namespaces/Experimental/variables/IndexedMerkleMap)
- [o1js changelog](https://github.com/o1-labs/o1js/releases)
- Implementation ready in `zk-dag.ts`

