# Provenance System

A complete event-based provenance system with cryptographic signatures, Merkle-DAG structure, and ITC causality tracking.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    PROVENANCE SYSTEM                         │
│                                                              │
│  ┌─────────────┐   ┌──────────────┐   ┌─────────────────┐ │
│  │   Events    │──▶│   Signing    │──▶│   DAG Store     │ │
│  │  (Schema)   │   │    (SEA)     │   │   (Holster)     │ │
│  └─────────────┘   └──────────────┘   └─────────────────┘ │
│         │                  │                     │          │
│         ▼                  ▼                     ▼          │
│  ┌─────────────┐   ┌──────────────┐   ┌─────────────────┐ │
│  │ Verification│◀──│ ITC Causality│◀──│ Proof Builder   │ │
│  │   Engine    │   │   Tracking   │   │                 │ │
│  └─────────────┘   └──────────────┘   └─────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

## Key Features

✅ **Content-Addressed Events** - Event ID = SHA-256(event body)
✅ **Cryptographic Signatures** - SEA/ECDSA signatures for authenticity
✅ **Merkle-DAG Structure** - Hash-linked parent events
✅ **ITC Causality** - Interval Tree Clocks for distributed causality
✅ **Full Verification** - Recursive verification of event chains
✅ **Proof Construction** - Build compact provenance proofs

## Quick Start

```typescript
import {
  createComputationEvent,
  verifyEvent,
  constructProof
} from '$lib/commons/compute/provenance';

// 1. Create a computation event
const event = await createComputationEvent({
  payload: {
    programHash: '...',
    computationId: 'my_computation',
    computationHash: '...',
    inputs: { x: { contentHash: '...', source: 'value' } },
    outputs: { result: { contentHash: '...', path: '/output' } },
    deterministicHash: '...'
  },
  parents: [], // Parent event IDs
  tags: ['allocation', 'v2']
});

console.log('Event created:', event.id);

// 2. Verify the event
const result = await verifyEvent(event);
if (result.valid) {
  console.log('✅ Event is valid');
} else {
  console.log('❌ Event invalid:', result.errors);
}

// 3. Build a provenance proof
const proof = await constructProof(event.id, {
  proofType: 'full',
  maxDepth: 100
});

console.log('Proof:', proof);
```

## Modules

### 1. Event Schema (`provenance-event-schema.ts`)

Defines the event structure using Zod v4:
- `ProvenanceEvent` - Core event type
- `ComputationEvent` - RDL-specific event
- Validation helpers

### 2. Signing (`provenance-signing.svelte.ts`)

Cryptographic operations:
- `signEvent()` - Sign with SEA
- `verifyEventSignature()` - Verify signature
- `computeEventId()` - Content-addressed ID
- `hashData()` - Deterministic hashing

### 3. DAG Management (`provenance-dag.svelte.ts`)

Storage and traversal:
- `storeEvent()` - Store in Holster
- `getEvent()` - Retrieve event
- `traverseToRoots()` - Backward traversal
- `findPath()` - Find path between events

### 4. Verification Engine (`provenance-verification.svelte.ts`)

Event validation:
- `verifyEvent()` - Single event verification
- `verifyEventRecursive()` - Recursive chain verification
- `verifyITCCausality()` - Causality validation
- `getCausalRelationship()` - Compare events

### 5. Proof Construction (`provenance-proof.svelte.ts`)

Build and verify proofs:
- `constructProof()` - Build compact proof
- `verifyProof()` - Verify proof validity
- `buildProvenancePath()` - Extract event path
- Proof types: `full`, `shallow`, `witness`

### 6. High-Level API (`provenance.svelte.ts`)

Unified interface:
- `createEvent()` - Create any event
- `createComputationEvent()` - Create computation event
- `initializeProvenanceSystem()` - System setup
- Query helpers and utilities

## Event Structure

```typescript
interface ProvenanceEvent {
  // Content-addressed ID (SHA-256 of body)
  id: Hash;
  
  // Author's public key
  author: string;
  
  // Application payload
  payload: any;
  
  // ITC stamp for causality
  itc: ITCStamp;
  
  // Parent event hashes (Merkle links)
  parents: Hash[];
  
  // ISO timestamp
  timestamp: string;
  
  // Event metadata
  meta: {
    type: 'computation' | 'allocation' | 'generic';
    version: string;
    tags?: string[];
  };
  
  // SEA signature
  sig: {
    m: string; // Message (event ID)
    s: string; // Signature
  };
}
```

## Storage Structure

```
~{myPubKey}/provenance/
  events/
    {eventId}          → EventStoreEntry
  heads/
    {authorId}         → HeadIndexEntry (latest events)
  parents/
    {parentId}         → ParentIndexEntry (reverse edges)
```

## Verification Process

1. **Hash Integrity** - Recompute event ID, verify match
2. **Signature** - Verify SEA signature with author's pubkey
3. **ITC Causality** - Check ITC stamp consistency
4. **Parent Chain** - Recursively verify parents
5. **Structure** - Validate all required fields

## Proof Types

### Full Proof
- Complete ancestor chain
- All events from target to roots
- Full verification steps

### Shallow Proof
- Only direct parents (depth=1)
- Quick verification
- Minimal size

### Witness Proof
- Target event only
- Signature + hash verification
- Fastest verification

## Integration with Compute Runtime

```typescript
// In compute.svelte.ts
import { createComputationEventFromLegacy } from './provenance.svelte';

// After executing computation
const event = await createComputationEventFromLegacy({
  programHash: this.programHash,
  computationId: computation.id,
  computationHash: hashComputation(computation),
  inputs: inputProvenance,
  outputs: outputProvenance,
  deterministicHash: deterministicHash
}, parentEventIds);

// Event is automatically signed and stored
console.log('Provenance event:', event.id);
```

## Security Properties

✅ **Immutability** - Events are content-addressed, cannot change
✅ **Authenticity** - SEA signatures prove authorship
✅ **Causality** - ITC + parents track causal relationships
✅ **Integrity** - Hash chains prevent tampering
✅ **Non-repudiation** - Signatures cannot be denied

## Best Practices

1. **Always verify events** before trusting computation results
2. **Use full proofs** for critical operations
3. **Check ITC causality** to detect concurrent updates
4. **Store events immediately** after creation
5. **Build proofs incrementally** for efficiency
6. **Anchor Merkle roots** periodically (future: blockchain)

## Future Enhancements

- [ ] Anchoring to external ledger (blockchain/timestamp authority)
- [ ] Key rotation support
- [ ] Selective disclosure (privacy)
- [ ] Compressed proofs (zero-knowledge)
- [ ] Cross-chain verification
- [ ] Efficient root/leaf indexes
- [ ] Batch anchoring with Merkle trees

## References

- [Interval Tree Clocks Paper](http://gsd.di.uminho.pt/members/cbm/ps/itc2012.pdf)
- [Merkle-DAG (IPFS)](https://docs.ipfs.tech/concepts/merkle-dag/)
- [Gun SEA Documentation](https://gun.eco/docs/SEA)
- Original design: See parent directory docs/

## License

Part of the Free Association project.

