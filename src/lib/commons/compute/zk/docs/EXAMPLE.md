# ZK Module - Usage Examples

## Quick Start

```typescript
import {
  initializeZkSystem,
  proveAllocation,
  verifyAllocation,
  proveEventChain,
  proveAllocationRollup
} from '$lib/commons/compute/zk';

// Initialize once (takes 1-5 minutes)
await initializeZkSystem();
```

## Example 1: Prove Allocation Correctness (⭐ KILLER APP)

Prove that an allocation is computed correctly **WITHOUT revealing MR values**!

```typescript
// Generate proof
const { proof, allocatedAmount, verificationKey } = await proveAllocation({
  recipientId: 'alice',
  mrValue: 0.35,        // PRIVATE! Never revealed
  mrSum: 1.0,           // PRIVATE! Never revealed
  totalCapacity: 100    // PRIVATE! Never revealed
});

console.log('Allocated:', allocatedAmount); // 35
// MR values stay completely private!

// Verify proof
const isValid = await verifyAllocation(proof, verificationKey);
console.log('Proof valid:', isValid); // true
```

### Why This Matters

- **Privacy**: MR values never leave the prover
- **Verifiability**: Anyone can verify the allocation is correct
- **Trust**: No need to trust the allocator
- **Dispute Resolution**: Cryptographic proof of fairness

## Example 2: Prove Event Integrity

Prove that an event hash matches its contents:

```typescript
import { proveEventIntegrity, verifyEventIntegrity } from '$lib/commons/compute/zk';

const event: ProvenanceEvent = {
  id: '0x123...',
  author: 'alice',
  parents: ['0xabc...'],
  payload: { type: 'computation', data: {...} },
  timestamp: new Date().toISOString(),
  signature: '...',
  itcStamp: {...}
};

// Generate proof
const { proof, verificationKey } = await proveEventIntegrity(event);

// Verify
const isValid = await verifyEventIntegrity(proof, verificationKey);
```

## Example 3: Recursive Chain Proof

Compress an entire event chain into a constant-size proof:

```typescript
import { proveEventChain, verifyChain } from '$lib/commons/compute/zk';

const events: ProvenanceEvent[] = [
  /* 1000 events */
];

// Generate recursive proof (constant size!)
const { proof, verificationKey } = await proveEventChain(events);

// Proof size is O(1) regardless of chain length
// Verification time is O(1)
```

## Example 4: Tree-Based Rollup

Compress N allocations into O(log N) proof:

```typescript
import { proveAllocationRollup, verifyRollup } from '$lib/commons/compute/zk';

const allocations = [10, 20, 15, 25, 30]; // 5 allocations
const totalCapacity = 100;

// Generate rollup proof
const { proof, totalAllocated, verificationKey } = await proveAllocationRollup(
  allocations,
  totalCapacity
);

console.log('Total allocated:', totalAllocated); // 100
console.log('Proof compresses', allocations.length, 'allocations');

// Verify
const isValid = await verifyRollup(proof, verificationKey);
```

## Standalone Usage (No Blockchain Required)

o1js proofs work **without** Mina blockchain:

```typescript
import { verify } from 'o1js';

// Export proof for storage/sharing
const proofJSON = proof.toJSON();
const proofString = JSON.stringify({ proof: proofJSON, verificationKey });

// Later: Import and verify
const imported = JSON.parse(proofString);
const isValid = await verify(imported.proof, imported.verificationKey);
```

## Recursion Patterns

### Linear Recursion (Blockchain Compression)

```typescript
// Like Mina's blockchain compression
// Proof_N verifies Proof_{N-1} + Event_N
// Final proof is constant size regardless of chain length
const chainProof = await proveEventChain(events);
```

### Tree Recursion (Rollup)

```typescript
// Like Mina's rollup mechanism
// Merge proofs in binary tree structure
// O(log N) proof generation parallelism
const rollupProof = await proveAllocationRollup(allocations, capacity);
```

## Performance

- **Compilation**: 1-5 minutes (once per application lifetime)
- **Proof Generation**: 
  - Event integrity: ~500ms
  - Allocation: ~800ms
  - Recursive step: ~1s
  - Rollup merge: ~1.5s
- **Verification**: ~100ms (constant time)
- **Proof Size**: ~2KB (constant size)

## Architecture

```
┌─────────────────────────────────────────────────┐
│ Provenance System (existing)                    │
│ - Events, DAG, Signatures, ITC                  │
└───────────────┬─────────────────────────────────┘
                │
                │ Integration Layer (minimal overhead)
                │
┌───────────────▼─────────────────────────────────┐
│ ZK Module (o1js)                                 │
│                                                   │
│  ┌─────────────┐  ┌─────────────┐               │
│  │   Circuit   │  │   Circuit   │               │
│  │ Definitions │→│  Compiler   │               │
│  └─────────────┘  └──────┬──────┘               │
│                           │                      │
│                   ┌───────▼────────┐             │
│                   │ Prover/Verifier│             │
│                   └────────────────┘             │
└─────────────────────────────────────────────────┘
```

## Security Properties

✅ **Zero-Knowledge**: Private inputs never revealed  
✅ **Soundness**: Can't prove false statements  
✅ **Completeness**: True statements always provable  
✅ **Succinctness**: Constant-size proofs  
✅ **Non-Interactive**: No back-and-forth required  

## Use Cases

1. **Private Fair Allocation**: Prove fairness without revealing MR values
2. **Audit Trails**: Constant-size proofs of computation history
3. **Dispute Resolution**: Cryptographic evidence of correctness
4. **Cross-Network Proofs**: Prove events to other systems
5. **Scalability**: Compress N operations into O(1) verification

## Limitations

- **Compilation Time**: 1-5 minutes initial setup
- **Field Arithmetic Only**: No floating point (use fixed precision)
- **Circuit Complexity**: More complex logic = longer proof time
- **Browser Support**: Requires WebAssembly

## Next Steps

- See `README.md` for architectural overview
- See `SETUP.md` for installation instructions
- See `ARCHITECTURE.md` for technical details
- See source code for implementation

