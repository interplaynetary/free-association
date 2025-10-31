**# Zero-Knowledge Provenance System (o1js)

A complete zero-knowledge proof system for privacy-preserving provenance verification built with [o1js](https://docs.o1labs.org/o1js).

## 🎯 Purpose

This module adds **privacy** and **scalability** to the provenance system through zero-knowledge proofs. It enables:

1. **Privacy-Preserving Verification** - Prove computations are correct WITHOUT revealing sensitive data
2. **Transparent Allocations** - Prove allocations are fair WITHOUT revealing everyone's MR values
3. **Efficient Verification** - Verify complex provenance chains with constant-size proofs
4. **Selective Disclosure** - Reveal only what's necessary for verification

## 🏗️ Architecture

```
┌────────────────────────────────────────────────────────┐
│                ZK PROVENANCE SYSTEM                     │
│                                                        │
│  ┌──────────────┐   ┌──────────────┐   ┌───────────┐ │
│  │  ZK Types    │──▶│ ZK Programs  │──▶│  Proofs   │ │
│  │ (Circuits)   │   │  (o1js)      │   │ (Public)  │ │
│  └──────────────┘   └──────────────┘   └───────────┘ │
│         ▲                   ▲                   │     │
│         │                   │                   ▼     │
│  ┌──────────────────────────────────────────────────┐ │
│  │         Provenance System Integration            │ │
│  │  (Convert events → ZK format → Generate proofs)  │ │
│  └──────────────────────────────────────────────────┘ │
└────────────────────────────────────────────────────────┘
```

## 🚀 Quick Start

### Installation

```bash
npm install o1js
```

### Initialize System

```typescript
import { initializeZkSystem } from '$lib/commons/compute/zk';

// Call once at app startup (takes 1-5 minutes)
await initializeZkSystem();
```

### Generate Privacy-Preserving Proof

```typescript
import { createPrivacyProof } from '$lib/commons/compute/zk';
import { getEvent } from '$lib/commons/compute/provenance';

// Get an event
const event = await getEvent(eventId);

// Create ZK proof (event content stays PRIVATE!)
const proof = await createPrivacyProof(event);

// Share proof with verifier
const proofJSON = exportZkProof(proof);

// Verifier can confirm event is legitimate WITHOUT seeing contents!
const isValid = await verifyAnyZkProof(proof);
```

### Generate Transparent Allocation Proof

```typescript
import { createTransparentAllocationProof } from '$lib/commons/compute/zk';

// Alice's allocation claim
const allocationProof = await createTransparentAllocationProof({
  recipientId: 'alice_pubkey',
  slotId: 'room_A',
  allocatedAmount: 50,
  totalCapacity: 100,
  mrValue: 0.3,      // PRIVATE - not revealed!
  mrSum: 2.5         // PRIVATE - not revealed!
});

// Proof shows Alice gets 50/100 WITHOUT revealing:
// - Alice's MR value (0.3)
// - Total MR sum (2.5)
// - Anyone else's MR values!

// Anyone can verify the allocation is correct
const isValid = await verifyAllocationProof(
  allocationProof.proof,
  allocationProof.verificationKey
);
```

## 📚 ZK Circuits

### 1. Event Integrity Circuit

Proves an event's ID matches its content hash without revealing the event.

**Public Inputs:**
- Event ID

**Private Inputs:**
- Full event data

**Proof:**
- Event ID = Poseidon(event fields)

**Use Case:** Privacy-preserving event verification

### 2. Event Chain Circuit

Proves a chain of events is valid (causally ordered, hash-linked) without revealing all events.

**Public Inputs:**
- Target event ID
- Root event ID

**Private Inputs:**
- Event chain data

**Proof:**
- Events are hash-linked
- ITC stamps are causally ordered

**Use Case:** Efficient lineage verification

### 3. Allocation Circuit ⭐ **KILLER APP**

Proves an allocation was computed correctly **WITHOUT revealing MR values**.

**Public Inputs:**
- Recipient ID
- Allocated amount

**Private Inputs:**
- Recipient's MR value (PRIVATE!)
- Sum of all MR values (PRIVATE!)
- Total capacity

**Proof:**
- `allocatedAmount = (mrValue / mrSum) * totalCapacity`
- `0 <= allocatedAmount <= totalCapacity`
- `0 <= mrValue <= mrSum`

**Use Case:** Transparent allocation with private MR values

### 4. Recursive Proof Circuit

Composes multiple proofs into one for efficient verification.

**Use Case:** Prove 100 allocations with a single verification

## 🎯 Use Cases

### Use Case 1: Private Resource Allocation

**Scenario:** Housing co-op needs to allocate rooms fairly based on mutual recognition, but people don't want their MR values public.

```typescript
// Generate proofs for all recipients
const proofs = await generateAllocationProofsBatch([
  { recipientId: 'alice', ..., mrValue: 0.3, mrSum: 2.5 },
  { recipientId: 'bob', ..., mrValue: 0.5, mrSum: 2.5 },
  { recipientId: 'carol', ..., mrValue: 0.7, mrSum: 2.5 }
]);

// Each person can verify their allocation is correct
// WITHOUT seeing anyone else's MR values!
```

**Impact:** Fair allocations with privacy.

### Use Case 2: Selective Disclosure

**Scenario:** Alice wants to prove she executed a computation correctly but doesn't want to reveal all inputs.

```typescript
// Alice generates proof
const proof = await generateEventIntegrityProof(computationEvent);

// Bob verifies WITHOUT seeing Alice's inputs
const isValid = await verifyEventIntegrityProof(
  proof.proof,
  proof.verificationKey
);
// ✅ Bob knows computation is valid
// ❌ Bob doesn't see Alice's private inputs
```

**Impact:** Trust without transparency.

### Use Case 3: Efficient Verification at Scale

**Scenario:** Network with 1000 participants needs to verify allocations efficiently.

```typescript
// Traditional: Verify 1000 events (slow)
for (const event of events) {
  await verifyEvent(event);  // 1000 verifications
}

// With ZK: Verify recursive proof (fast)
const recursiveProof = await composeProofs(allProofs);
await verifyZkProof(recursiveProof);  // 1 verification!
```

**Impact:** O(1) verification instead of O(n).

## 🔐 Security Properties

### What ZK Proofs Guarantee

✅ **Completeness** - If claim is true, prover can convince verifier
✅ **Soundness** - If claim is false, prover cannot convince verifier
✅ **Zero-Knowledge** - Verifier learns nothing except validity
✅ **Succinctness** - Proof size is small (constant or logarithmic)
✅ **Non-Interactive** - No back-and-forth communication needed

### What ZK Does NOT Guarantee

❌ **Proof of Work** - Doesn't prevent spam (use rate limiting)
❌ **Liveness** - Doesn't ensure timely responses (use timeouts)
❌ **Byzantine Agreement** - Doesn't solve consensus (use ITC + DAG)

## 📊 Performance

### Compilation (Once)

| Circuit | Time | Keys Size |
|---------|------|-----------|
| Event Integrity | ~30s | ~500KB |
| Event Chain | ~45s | ~800KB |
| Allocation | ~60s | ~1MB |
| Recursive | ~90s | ~1.5MB |

**Total:** 3-5 minutes to compile all circuits

### Proof Generation

| Circuit | Time | Proof Size |
|---------|------|------------|
| Event Integrity | ~500ms | ~2KB |
| Allocation | ~800ms | ~2.5KB |
| Recursive (10 proofs) | ~2s | ~3KB |

### Proof Verification

| Circuit | Time |
|---------|------|
| Any proof | ~50ms |

**Key Insight:** Verification is fast and constant-time!

## 🎨 Integration with Provenance System

### Convert Events to ZK Format

```typescript
import { convertEventToZK } from '$lib/commons/compute/zk';

// Full provenance event
const event = await getEvent(eventId);

// Convert to ZK format
const eventZK = convertEventToZK(event);

// Use in circuits
const proof = await proveEventIntegrity(eventZK);
```

### Embed ZK Proofs in Provenance Events

```typescript
// Create event with embedded ZK proof
const event = await createComputationEvent({
  payload: {
    ...computationData,
    zkProof: exportZkProof(allocationProof)  // Embed proof
  },
  parents: [parentId]
});

// Later: Extract and verify
const embedded = event.payload.zkProof;
const isValid = await verifyAnyZkProof(importZkProof(embedded));
```

## 🛠️ Advanced Usage

### Custom Circuits

Create your own o1js circuits for domain-specific verification:

```typescript
import { ZkProgram, Field } from 'o1js';

const MyCustomProgram = ZkProgram({
  name: 'MyCustom',
  publicInput: Field,
  
  methods: {
    verify: {
      privateInputs: [Field],
      async method(publicInput: Field, privateData: Field) {
        // Your constraint logic here
        const result = privateData.mul(Field(2));
        result.assertEquals(publicInput);
      }
    }
  }
});

// Compile and use
await MyCustomProgram.compile();
const proof = await MyCustomProgram.verify(Field(10), Field(5));
```

### Proof Composition

Compose multiple proofs for complex verification:

```typescript
// Generate individual proofs
const proof1 = await generateAllocationProof(claim1);
const proof2 = await generateAllocationProof(claim2);

// Compose them
const composedProof = await RecursiveProofProgram.composeProofs(
  publicInput,
  proof1.proof,
  proof2.proof
);

// Verify composed proof (faster than verifying both separately)
const isValid = await verifyZkProof(composedProof);
```

## 🚧 Limitations & Tradeoffs

### What You Give Up

1. **Compilation Time** - First run takes 3-5 minutes
2. **Proof Generation Time** - 500ms-2s per proof
3. **Fixed Circuit Size** - Must know max array sizes at compile time
4. **No Dynamic Logic** - Execution path must be deterministic

### Mitigation Strategies

1. **Cache Compiled Keys** - Compile once, reuse forever
2. **Generate Proofs Async** - Don't block UI
3. **Use Appropriate Array Bounds** - Balance size vs flexibility
4. **Keep Circuits Focused** - "Prove the smallest thing"

## 🎓 Learning Resources

- [o1js Documentation](https://docs.o1labs.org/o1js)
- [ZK Constraint Systems](https://docs.o1labs.org/o1js/getting-started/what-is-a-zk-constraint-system)
- [ZkProgram Guide](https://docs.o1labs.org/o1js/writing-constraint-systems/full-proof-flow-with-zkprogram)
- [Mina Protocol](https://minaprotocol.com/)

## 📝 API Reference

See `index.ts` for complete API documentation.

## 🤝 Contributing

When adding new circuits:

1. **Follow o1js Conventions** - Use provable types (Field, Bool, Struct)
2. **Document Constraints** - Explain what the circuit proves
3. **Test Thoroughly** - ZK bugs are hard to debug
4. **Measure Performance** - Profile proof generation time
5. **Keep Circuits Focused** - "Prove the smallest thing"

## 🔮 Future Enhancements

- [ ] Proof caching and reuse
- [ ] Recursive SNARK for unbounded chains
- [ ] Integration with Mina blockchain
- [ ] Browser-based proof generation
- [ ] Distributed proof generation (workers)
- [ ] Zero-knowledge membership proofs
- [ ] Private smart contracts (ZkApps)

## 📄 License

Part of the Free Association project.

