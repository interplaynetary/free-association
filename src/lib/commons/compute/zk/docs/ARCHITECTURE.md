# ZK System Architecture

## Complete System Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    PROVENANCE + ZK SYSTEM                                │
│                                                                          │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │                    PROVENANCE LAYER                               │  │
│  │  ┌────────────┐  ┌─────────────┐  ┌───────────┐  ┌────────────┐ │  │
│  │  │   Events   │─▶│   Signing   │─▶│    DAG    │─▶│  Proofs    │ │  │
│  │  │  (Schema)  │  │    (SEA)    │  │ (Holster) │  │ (Merkle)   │ │  │
│  │  └────────────┘  └─────────────┘  └───────────┘  └────────────┘ │  │
│  └──────────────────────────────────────────────────────────────────┘  │
│                              │                                           │
│                              ▼                                           │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │                        ZK LAYER                                   │  │
│  │  ┌────────────┐  ┌─────────────┐  ┌───────────┐  ┌────────────┐ │  │
│  │  │  ZK Types  │─▶│ ZK Programs │─▶│ZK Proofs  │─▶│  Verify    │ │  │
│  │  │  (o1js)    │  │ (Circuits)  │  │ (SNARKs)  │  │ (Instant)  │ │  │
│  │  └────────────┘  └─────────────┘  └───────────┘  └────────────┘ │  │
│  └──────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────┘
```

## Layer 1: Provenance System

### Purpose
Create **signed, content-addressed events** that form a Merkle-DAG.

### Components

1. **Events** (`provenance-event-schema.ts`)
   - Content-addressed (ID = SHA-256 hash)
   - Cryptographically signed (SEA/ECDSA)
   - Hash-linked parents (Merkle DAG)
   - ITC causality tracking

2. **Signing** (`provenance-signing.svelte.ts`)
   - Canonical serialization
   - SEA signature generation/verification
   - Content hash computation

3. **DAG** (`provenance-dag.svelte.ts`)
   - Event storage (Holster)
   - Graph traversal
   - Lineage tracking

4. **Verification** (`provenance-verification.svelte.ts`)
   - Hash integrity
   - Signature validation
   - ITC causality
   - Recursive verification

5. **Proofs** (`provenance-proof.svelte.ts`)
   - Path construction
   - Compact proof generation
   - Proof verification

### Data Flow

```
Create Event → Sign (SEA) → Store (Holster) → Verify → Build Proof
```

## Layer 2: ZK System

### Purpose
Add **privacy** and **efficiency** through zero-knowledge proofs.

### Components

1. **ZK Types** (`zk-types.ts`)
   - Convert provenance types to o1js format
   - Provable structs (Field, Bool, etc.)
   - Fixed-size arrays (o1js requirement)

2. **ZK Programs** (`zk-programs.ts`)
   - Circuit definitions (o1js ZkProgram)
   - Constraint logic
   - Proof generation

3. **ZK Integration** (`zk-provenance.svelte.ts`)
   - Convert events → ZK format
   - High-level API
   - Batch operations

### Circuits

#### Circuit 1: Event Integrity
**Proves:** Event ID matches content hash
**Privacy:** Event contents stay private
**Use Case:** Selective disclosure

#### Circuit 2: Event Chain
**Proves:** Chain of events is valid
**Privacy:** Intermediate events stay private
**Use Case:** Efficient lineage verification

#### Circuit 3: Allocation ⭐
**Proves:** Allocation is correct
**Privacy:** MR values stay private
**Use Case:** Transparent allocation without revealing MR

#### Circuit 4: Recursive Proof
**Proves:** Multiple proofs are valid
**Privacy:** N/A (composition)
**Use Case:** O(1) verification of N proofs

### Data Flow

```
Provenance Event → Convert to ZK → Generate Proof → Verify (fast!)
```

## Integration Points

### 1. Event Creation → ZK Proof Embedding

```typescript
// Create event
const event = await createComputationEvent({...});

// Generate ZK proof
const zkProof = await generateEventIntegrityProof(event);

// Embed in next event
const nextEvent = await createEvent({
  payload: {
    computation: {...},
    zkProof: exportZkProof(zkProof)  // Embed!
  },
  parents: [event.id]
});
```

### 2. Allocation → Privacy-Preserving Proof

```typescript
// Compute allocation (private MR values)
const allocation = computeAllocation(mrValues);

// Generate ZK proof (MR values stay private!)
const proof = await generateAllocationProof({
  recipientId: 'alice',
  allocatedAmount: allocation.alice,
  mrValue: mrValues.alice,  // PRIVATE
  mrSum: sum(mrValues)       // PRIVATE
});

// Publish proof (public, verifiable, private!)
await storeEvent({
  payload: {
    allocation: allocation.alice,
    proof: exportZkProof(proof)
  }
});
```

### 3. Verification → Fast Batch Verification

```typescript
// Traditional: Verify N events (slow)
for (const event of events) {
  await verifyEvent(event);  // O(n)
}

// ZK: Verify recursive proof (fast)
const recursiveProof = await composeProofs(zkProofs);
await verifyZkProof(recursiveProof);  // O(1)
```

## Performance Characteristics

### Provenance Layer

| Operation | Time | Space |
|-----------|------|-------|
| Create Event | ~10ms | 2KB |
| Sign Event | ~5ms | 256 bytes |
| Verify Event | ~15ms | - |
| Store Event | ~50ms | 2KB (Holster) |
| Traverse DAG | ~100ms | O(depth) |

### ZK Layer

| Operation | Time | Space |
|-----------|------|-------|
| Compile Circuits | 3-5 min (once) | 4MB (keys) |
| Generate Proof | 500ms-2s | 2-3KB |
| Verify Proof | ~50ms | - |
| Compose Proofs | ~2s | 3KB |

**Key Insight:** ZK verification is constant-time regardless of computation complexity!

## Security Model

### Provenance Layer Security

✅ **Immutability** - Content-addressed events can't change
✅ **Authenticity** - SEA signatures prove authorship
✅ **Causality** - ITC + parents track relationships
✅ **Integrity** - Hash chains prevent tampering
✅ **Non-repudiation** - Signatures can't be denied

### ZK Layer Security

✅ **Completeness** - True claims always provable
✅ **Soundness** - False claims never provable
✅ **Zero-Knowledge** - Verifier learns nothing except validity
✅ **Succinctness** - Proofs are small (2-3KB)
✅ **Non-Interactive** - No communication needed

### Combined Security

✅ **Privacy** - Hide sensitive data (MR values)
✅ **Efficiency** - O(1) verification
✅ **Transparency** - Prove correctness publicly
✅ **Sovereignty** - No trusted third parties
✅ **Auditability** - Complete history verifiable

## Scalability

### Provenance Scaling

- **Events:** Unlimited (DAG grows unbounded)
- **Storage:** O(total events) in Holster
- **Verification:** O(depth) for lineage
- **Throughput:** ~100 events/sec

### ZK Scaling

- **Proofs:** Unlimited generation
- **Storage:** O(1) per proof (2-3KB)
- **Verification:** O(1) always (~50ms)
- **Throughput:** ~20 proofs/sec

### Bottlenecks

1. **Proof Generation** - 500ms-2s (parallelizable)
2. **Compilation** - 3-5 min (cache keys!)
3. **Circuit Size** - Fixed bounds (plan ahead)

### Optimization Strategies

1. **Key Caching** - Compile once, reuse forever
2. **Worker Threads** - Generate proofs in parallel
3. **Batch Proofs** - Compose many → one
4. **Selective Proving** - Only prove what needs privacy

## Deployment Architecture

```
┌─────────────────────────────────────────────────────────┐
│                      APPLICATION                         │
│  ┌────────────┐  ┌────────────┐  ┌──────────────────┐  │
│  │  Frontend  │─▶│  Backend   │─▶│  ZK Worker Pool  │  │
│  │  (Verify)  │  │  (Prove)   │  │  (Generate)      │  │
│  └────────────┘  └────────────┘  └──────────────────┘  │
│         │              │                    │            │
│         ▼              ▼                    ▼            │
│  ┌────────────────────────────────────────────────────┐ │
│  │              HOLSTER (Storage)                     │ │
│  │  Events + DAG + Proofs + Verification Keys        │ │
│  └────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────┘
```

### Recommendations

1. **Frontend:** Verify proofs (fast!)
2. **Backend:** Generate proofs (slow but parallelizable)
3. **Workers:** Dedicated proof generation pool
4. **Storage:** Holster for events + proofs
5. **CDN:** Serve verification keys

## Future Extensions

### 1. Recursive SNARKs
Unbounded proof composition

### 2. Zero-Knowledge Smart Contracts
Private computations on public data

### 3. Cross-Chain Verification
Verify proofs on other blockchains

### 4. Distributed Proving
Generate proofs across multiple nodes

### 5. Quantum-Resistant
Post-quantum signature schemes

## Comparison to Alternatives

### vs Traditional Signatures (SEA)

| Feature | SEA | ZK |
|---------|-----|-----|
| Privacy | ❌ Public | ✅ Private |
| Verification | ⚡ Fast | ⚡ Fast |
| Proof Size | 256 bytes | 2-3KB |
| Generation | ⚡ Fast | 🐌 Slow |

**Verdict:** Use SEA by default, ZK when privacy needed.

### vs Blockchain

| Feature | Blockchain | ZK + DAG |
|---------|------------|----------|
| Consensus | ⚡ Built-in | ⚠️ Manual (ITC) |
| Finality | 🐌 Slow | ⚡ Instant |
| Cost | 💰 Expensive | 💰 Free |
| Privacy | ❌ Public | ✅ Private |

**Verdict:** ZK + DAG for private, fast, free consensus.

### vs Traditional Audit

| Feature | Manual Audit | ZK Proof |
|---------|--------------|----------|
| Trust | 👤 Auditor | 🔐 Math |
| Speed | 🐌 Days | ⚡ Seconds |
| Cost | 💰💰💰 | 💰 (one-time) |
| Privacy | ⚠️ Limited | ✅ Full |

**Verdict:** ZK for automated, instant, trustless audits.

## Summary

The combined Provenance + ZK system provides:

1. **Trust without Transparency** - Prove correctness without revealing data
2. **Efficiency at Scale** - O(1) verification regardless of complexity
3. **Privacy by Default** - Hide sensitive data (MR values, inputs)
4. **Sovereignty** - No trusted third parties needed
5. **Auditability** - Complete history verifiable on demand

This enables **truly decentralized coordination** with the transparency of blockchains and the privacy of private systems.

