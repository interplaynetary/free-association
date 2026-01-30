# Technical Stack Overview: Complete Architecture

## TL;DR - The Full Stack

```
┌─────────────────────────────────────────────────────────────────┐
│                  APPLICATION LAYER                               │
│  User interfaces, forms, visualizations                         │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│                  COMPUTE RUNTIME LAYER                           │
│  RDL interpreter, reactive execution, program lifecycle         │
│  • Runtime Manager  • Compute Engine  • Program Registry        │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│                  PROVENANCE LAYER                                │
│  Cryptographic event tracking, verification, lineage            │
│  • Events (content-addressed)  • Signatures (SEA/ECDSA)         │
│  • DAG (hash-linked)  • ITC (causality)  • Verification         │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│                  ZK PROOF LAYER (Optional)                       │
│  Zero-knowledge proofs for external verification                │
│  • MerkleMap DAG  • o1js circuits  • Proof generation           │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│                  KERNEL LAYER                                    │
│  User space management, subscriptions, replication              │
│  • kernel-core (language-agnostic)                              │
│  • kernel-rdl (RDL-specific)                                    │
│  • kernel-domain (economic coordination)                        │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│                  STORAGE LAYER                                   │
│  Mesh/Gun (P2P encrypted database)                          │
│  • IndexedDB (browser)  • Disk (node)  • P2P sync              │
└─────────────────────────────────────────────────────────────────┘
```

---

## Layer 1: Storage (Mesh/Gun)

### Purpose
Peer-to-peer encrypted database for distributed data storage.

### What It Provides
- **User spaces**: Each user has `~{pubKey}/` namespace
- **P2P sync**: Automatic synchronization across devices
- **Encryption**: SEA (Security, Encryption, Authorization)
- **Local-first**: Data lives on your device (IndexedDB/disk)
- **Subscriptions**: Real-time updates when data changes

### Key APIs
```typescript
// Write data
mesh.get('~alice/data/item').put({ value: 42 });

// Read data
mesh.get('~alice/data/item').once((data) => {
  console.log(data.value);
});

// Subscribe to changes
mesh.get('~alice/data/item').on((data) => {
  console.log('Updated:', data.value);
});
```

### Storage Paths
```
~{pubKey}/
  ├─ programs/          # Program registry
  ├─ compute/           # RDL computation state
  ├─ provenance/        # Provenance events
  ├─ subscriptions/     # Subscription tracking
  ├─ nodes/             # Tree structures
  ├─ causality/         # ITC stamps
  ├─ allocation/        # Economic coordination
  ├─ trees/             # Priority trees
  └─ replication/       # Encrypted peer data
```

---

## Layer 2: Kernel (User Space Management)

### Purpose
Manages the structure and organization of user space across all namespaces.

### Three Kernel Modules

#### kernel-core.ts (Language-Agnostic)
Infrastructure that works with ANY programming language:

```typescript
// Program Registry (works with RDL, SQL, WASM, etc.)
type GenericProgramDefinition = {
  program_type: 'RDL' | 'SQL' | 'WASM' | ...;
  language_version: string;
  program_data: any;  // Language-specific
  program_hash: string;
};

// Subscriptions (cross-user, bidirectional)
~{pubKey}/subscriptions/
  ├─ outbound/local/    # What I'm watching locally
  ├─ outbound/peers/    # What I'm watching from peers
  └─ inbound/           # Who's watching me

// Causality (ITC stamps)
~{pubKey}/causality/
  ├─ itc_stamp          # My logical clock
  └─ peer_stamps/       # Peer logical clocks

// Nodes (tree structures)
~{pubKey}/nodes/{nodeId}/
  ├─ node               # Tree node data
  └─ storage/           # Optional node storage

// Replication (encrypted peer data)
~{pubKey}/replication/{peerPubKey}/
  └─ encrypted_data     # Peer-encrypted backup
```

#### kernel-rdl.ts (RDL-Specific)
Extensions for RDL (Reactive Declarative Language):

```typescript
// RDL Program Storage
~{pubKey}/programs/registry/{programHash}/
  ├─ definition         # ReactiveComputationGraph
  ├─ metadata           # Version, description
  └─ status             # Active, enabled

// RDL Computation State
~{pubKey}/compute/{programHash}/
  ├─ state/
  │   ├─ variables/     # Variable values
  │   ├─ computations/  # Computation results
  │   └─ metadata/      # Execution stats
  ├─ outputs/           # Published outputs
  └─ provenance/        # Execution records
```

#### kernel-domain.ts (Economic Coordination)
Domain-specific extensions for economic coordination:

```typescript
// Allocation Namespace
~{pubKey}/allocation/
  ├─ commitment         # My capacity & needs
  ├─ allocation_state   # Computed allocations
  └─ network/           # Peer commitments & states

// Trees Namespace
~{pubKey}/trees/
  ├─ my_tree            # My priority tree
  └─ network_trees/     # Peer trees
```

### Design Philosophy
- **Separation of concerns**: Language-agnostic vs language-specific vs domain-specific
- **Extensible**: Add new languages (kernel-sql.ts) or domains (kernel-chat.ts)
- **Unified API**: kernel.ts re-exports everything for backwards compatibility

---

## Layer 3: Provenance System

### Purpose
Cryptographic tracking of computation lineage with verification and privacy.

### Architecture

```
┌──────────────────────────────────────────────────────────────┐
│ PROVENANCE EVENTS (Immutable Records)                        │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  Event = {                                                    │
│    id: SHA256(event body),        // Content-addressed       │
│    author: pubkey,                // Who created it          │
│    payload: {...},                // What happened           │
│    itc: ITCStamp,                 // Logical time            │
│    parents: [parentHashes],       // Merkle-DAG links        │
│    timestamp: ISO8601,            // Wall-clock (non-auth)   │
│    meta: { type, version },       // Event metadata          │
│    sig: SEASignature              // ECDSA signature         │
│  }                                                            │
│                                                               │
│  Properties:                                                  │
│  ✅ Tamper-proof (content-addressed)                        │
│  ✅ Verifiable (signed)                                      │
│  ✅ Causal (ITC + parents)                                   │
│  ✅ Immutable (hash-linked)                                  │
│                                                               │
└──────────────────────────────────────────────────────────────┘
```

### Module Structure

```
provenance/
├─ provenance-event-schema.ts      # Zod schemas for events
├─ provenance-signing.svelte.ts    # SEA signatures + hashing
├─ provenance-dag.svelte.ts        # DAG storage in Mesh
├─ provenance-verification.svelte.ts # Verification engine
├─ provenance-proof.svelte.ts      # Proof construction
├─ provenance.svelte.ts            # High-level API
└─ index.ts                        # Barrel exports
```

### Storage in Mesh

```
~{pubKey}/provenance/
├─ events/{eventId}                # Event storage
│   ├─ event                       # Full event data
│   ├─ stored_at                   # When stored
│   └─ verified                    # Verification status
├─ heads/{authorId}                # Latest events per author
│   └─ event_ids[]                 # Head event IDs
└─ parents/{parentId}              # Reverse edges (children)
    └─ child_ids[]                 # Child event IDs
```

### Key Operations

```typescript
// 1. Create signed event
const event = await createSignedEvent({
  author: myPubKey,
  payload: { programHash, inputs, outputs },
  itc: myItcStamp,
  parents: [parentEventId]
});
// Result: Content-addressed, signed, causal event

// 2. Store in DAG
await storeEvent(event);
// Writes to: events, heads, parents indexes

// 3. Verify event
const isValid = await verifyEvent(event);
// Checks: hash, signature, ITC, parents

// 4. Traverse lineage
const ancestors = await traverseToRoots(eventId);
// BFS/DFS traversal of parent links

// 5. Build proof
const proof = await constructProof({
  target: eventId,
  roots: trustedRootIds
});
// Minimal set of events to verify lineage
```

### Four Core Components

#### 1. Content-Addressing (SHA-256)
```typescript
// Event ID = hash of canonical body
id = SHA256(deterministicStringify({
  author,
  payload,
  itc,
  parents,
  merkle_root,
  timestamp,
  meta
}));

// Change ANYTHING → ID changes → chain breaks
```

#### 2. Digital Signatures (SEA/ECDSA)
```typescript
// Sign the event body
const sig = await SEA.sign(eventBody, userPair);

// Verify authorship
const isValid = await SEA.verify(sig.m, sig.s, authorPubKey);

// Properties:
// ✅ Non-repudiation (only author can sign)
// ✅ Integrity (signature breaks if tampered)
// ✅ Authentication (proves who created it)
```

#### 3. Merkle-DAG (Hash-Linking)
```typescript
// Each event links to parents by hash
event2.parents = [event1.id];
event3.parents = [event2.id];
event4.parents = [event2.id, event3.id];  // Merge!

// Properties:
// ✅ Tamper-evident (change parent → child hash changes)
// ✅ Verifiable lineage (follow links back to genesis)
// ✅ Merkle proofs (prove membership with O(log N) data)
```

#### 4. ITC (Interval Tree Clocks)
```typescript
// Logical time without global clock
stamp = { id: 1, event: 5 };

// Operations:
stamp.event();              // Increment: { id: 1, event: 6 }
[s1, s2] = stamp.fork();   // Split: concurrent replicas
stamp.join(peerStamp);     // Merge: sync causality

// Ordering:
if (stamp1.leq(stamp2))     → event1 happened before event2
if (stamp1.concurrent(stamp2)) → events happened independently

// Properties:
// ✅ No coordination needed
// ✅ Detects causality vs concurrency
// ✅ Works in fully distributed systems
```

---

## Layer 4: ZK Proof Layer (Optional)

### Purpose
Generate zero-knowledge proofs about provenance WITHOUT revealing private data.

### When To Use
- **External verification**: Share with auditors, smart contracts, other systems
- **Privacy needs**: Prove correctness without revealing values
- **Succinctness**: Compress 1000 events into 2KB proof
- **Cross-system interop**: Verify in blockchain, database, etc.

### Architecture

```
┌──────────────────────────────────────────────────────────────┐
│ 1. FETCH EVENTS (from provenance system)                     │
│    const events = await getEventsBatch(eventIds);            │
└──────────────────────────────────────────────────────────────┘
                           ↓
┌──────────────────────────────────────────────────────────────┐
│ 2. BUILD MERKLEMAP (ephemeral, in-memory)                    │
│    const dagMap = buildDAGMapFromEvents(events);             │
│                                                               │
│    MerkleMap: eventHash → parentHash                         │
│    • Height 256 = unlimited capacity                         │
│    • Natural key-value mapping                               │
│    • O(1) lookups                                            │
└──────────────────────────────────────────────────────────────┘
                           ↓
┌──────────────────────────────────────────────────────────────┐
│ 3. GENERATE ZK PROOF                                         │
│    const { proof } = await proveMembershipInDAGMap(          │
│      dagMap,                                                  │
│      eventId                                                  │
│    );                                                         │
│                                                               │
│    Proof = ~2-8KB (constant size!)                           │
└──────────────────────────────────────────────────────────────┘
                           ↓
┌──────────────────────────────────────────────────────────────┐
│ 4. VERIFY ANYWHERE                                           │
│    const isValid = await verify(proof, vkey);                │
│                                                               │
│    Verifier CANNOT see:                                      │
│    ❌ Other events                                           │
│    ❌ DAG structure                                          │
│    ❌ Private values                                         │
│                                                               │
│    Verifier CAN verify:                                      │
│    ✅ Event exists in DAG                                    │
│    ✅ Lineage is valid                                       │
│    ✅ Allocation is correct                                  │
└──────────────────────────────────────────────────────────────┘
```

### Module Structure

```
zk/
├─ zk-programs.ts                  # 4 core o1js circuits
│   ├─ EventIntegrityProgram       # Prove event hash correct
│   ├─ EventChainProgram           # Recursive event chain
│   ├─ AllocationProgram           # Private fair allocation
│   └─ AllocationRollupProgram     # Tree-based aggregation
│
├─ zk-provenance.svelte.ts         # Integration layer
│   ├─ initializeZkSystem()
│   ├─ proveEventIntegrity()
│   ├─ proveAllocation()           # ⭐ KILLER FEATURE
│   └─ proveEventChain()
│
├─ zk-dag-map.ts                   # MerkleMap DAG ⭐ BEST
│   ├─ DAGMap class
│   ├─ DAGMapMembershipProgram
│   ├─ DAGMapLineageProgram
│   └─ DAGMapUpdateProgram
│
├─ zk-dag-map-integration.svelte.ts
│   ├─ buildDAGMapFromEvents()
│   ├─ proveMembershipInDAGMap()
│   └─ proveLineageInDAGMap()
│
└─ index.ts                        # Unified exports
```

### Available Circuits

#### 1. EventIntegrityProgram
```typescript
// Prove: claimedId = hash(author, parent0, parent1, payload, timestamp)
const { proof } = await proveEventIntegrity({
  claimedId: event.id,
  author: event.author,          // PRIVATE
  parent0: event.parents[0],     // PRIVATE
  parent1: event.parents[1],     // PRIVATE
  payloadHash: hash(payload),    // PRIVATE
  timestamp: event.timestamp     // PRIVATE
});

// Verifier sees ONLY: event ID
// Verifier CANNOT see: who created it, payload, parents
```

#### 2. AllocationProgram ⭐ KILLER FEATURE
```typescript
// Prove: allocation = (mrValue * totalCapacity) / mrSum
const { proof, allocatedAmount } = await proveAllocation({
  recipientId: 'alice',
  mrValue: 0.35,        // PRIVATE! Never revealed
  mrSum: 1.0,           // PRIVATE! Never revealed
  totalCapacity: 100    // PRIVATE! Never revealed
});

// Result: allocatedAmount = 35
// Verifier sees: 35 is correctly allocated
// Verifier CANNOT see: MR values, sums, or capacity

// USE CASE: Prove fair allocation without revealing recognition weights!
```

#### 3. DAGMapMembershipProgram
```typescript
// Prove: event exists in DAG
const { proof } = await proveMembershipInDAGMap(dagMap, eventId);

// Verifier sees: DAG root + event ID
// Verifier CANNOT see: other events, DAG size, structure

// Proof size: ~2-8KB (constant, regardless of DAG size!)
```

#### 4. DAGMapLineageProgram
```typescript
// Prove: childEvent is descendant of parentEvent
const { proof } = await proveLineageInDAGMap(
  dagMap,
  childId,
  parentId
);

// Verifier sees: parent/child relationship is valid
// Verifier CANNOT see: intermediate events, full lineage
```

#### 5. EventChainProgram (Recursive)
```typescript
// Prove: chain of N events is valid
const { proof } = await proveEventChain(events);

// Base case: single event
// Recursive step: verify previous proof + link current event

// Result: Constant-size proof for arbitrarily long chains!
```

#### 6. AllocationRollupProgram (Tree Recursion)
```typescript
// Prove: sum of multiple allocations
const { proof, totalAllocated } = await proveAllocationRollup([
  proof1, proof2, proof3, ...
]);

// Tree-based recursion: parallelize proof generation
// Result: Aggregate many allocations into single proof
```

### Key Insight: Hybrid Architecture

```
INTERNAL (your system):
  ├─ Provenance DAG in Mesh
  ├─ Events are VISIBLE to you
  ├─ Traditional queries (fast)
  └─ NO zero-knowledge needed

EXTERNAL (sharing with others):
  ├─ Build ephemeral MerkleMap
  ├─ Generate ZK proof
  ├─ Share proof + root
  └─ Events stay PRIVATE
```

**You don't do "ZK everything"!**  
You use ZK proofs ONLY when sharing externally or proving privately.

---

## Layer 5: Compute Runtime (RDL)

### Purpose
Execute reactive declarative programs with automatic provenance tracking.

### What It Does

```typescript
// Define a program (like Excel formulas)
const program = {
  computations: [
    {
      id: 'sum',
      inputs: ['alice_offers', 'bob_offers'],
      function: 'add',
      output: 'total_offers'
    }
  ]
};

// Runtime automatically:
// 1. Subscribes to inputs
// 2. Executes when inputs change
// 3. Creates provenance events
// 4. Publishes outputs
// 5. Stores state
```

### Module Structure

```
compute/
├─ schema.ts                       # RDL language definition (Zod)
├─ compute.svelte.ts               # Computation engine
├─ runtime-manager.svelte.ts       # Program lifecycle
├─ program-hash.svelte.ts          # Content addressing
└─ kernel-*.ts                     # Storage integration
```

### Execution Flow

```
1. REGISTER PROGRAM
   ├─ Hash program definition
   ├─ Store in program registry
   └─ ~{pubKey}/programs/registry/{programHash}/

2. ACTIVATE PROGRAM
   ├─ Create reference in active programs
   ├─ Initialize compute state
   └─ ~{pubKey}/compute/{programHash}/state/

3. SUBSCRIBE TO INPUTS
   ├─ Register subscriptions
   ├─ Watch for input changes
   └─ ~{pubKey}/subscriptions/outbound/

4. EXECUTE COMPUTATION
   ├─ When input changes:
   │   ├─ Load computation definition
   │   ├─ Fetch current input values
   │   ├─ Execute function
   │   ├─ Create provenance event
   │   ├─ Sign event with SEA
   │   ├─ Store event in DAG
   │   ├─ Update computation state
   │   └─ Publish output
   └─ ~{pubKey}/provenance/events/{eventId}

5. PUBLISH OUTPUT
   ├─ Write to output path
   ├─ Trigger subscriptions
   └─ ~{pubKey}/compute/{programHash}/outputs/{key}
```

### Provenance Integration

```typescript
// When computation executes:
const event = await createSignedEvent({
  author: myPubKey,
  payload: {
    type: 'computation',
    programHash: hash(program),
    computationId: computation.id,
    inputs: {
      alice_offers: {
        source: 'subscription',
        path: '~alice/offers',
        contentHash: hash(aliceValue),
        provenance: aliceEventId  // Links to Alice's event!
      },
      bob_offers: {
        source: 'subscription',
        path: '~bob/offers',
        contentHash: hash(bobValue),
        provenance: bobEventId    // Links to Bob's event!
      }
    },
    outputs: {
      total: {
        path: '~me/total',
        contentHash: hash(result)
      }
    },
    deterministicHash: hash(program + inputHashes)
  },
  itc: myItcStamp,
  parents: [aliceEventId, bobEventId],  // Merkle-DAG links!
  timestamp: new Date().toISOString()
});

await storeEvent(event);

// Now we can:
// ✅ Prove this computation happened
// ✅ Prove it used Alice's and Bob's data
// ✅ Trace the full lineage back to sources
// ✅ Verify the computation is reproducible
// ✅ Generate ZK proofs for external verification
```

---

## Complete Integration: How It All Works Together

### Scenario: Fair Allocation with Provenance

```typescript
// ═══════════════════════════════════════════════════════════════
// STEP 1: Users Publish Data
// ═══════════════════════════════════════════════════════════════

// Alice publishes capacity
const aliceEvent = await createSignedEvent({
  author: 'alice',
  payload: { capacity: 100 },
  itc: aliceStamp,
  parents: []  // Genesis event
});
await storeEvent(aliceEvent);

// Bob publishes recognition
const bobEvent = await createSignedEvent({
  author: 'bob',
  payload: { mrValues: { alice: 0.35, carol: 0.65 } },
  itc: bobStamp,
  parents: []
});
await storeEvent(bobEvent);

// ═══════════════════════════════════════════════════════════════
// STEP 2: RDL Program Runs Allocation
// ═══════════════════════════════════════════════════════════════

const program = {
  computations: [{
    id: 'allocate',
    inputs: ['alice_capacity', 'bob_mr_values'],
    function: 'calculateAllocation',
    output: 'allocations'
  }]
};

// Runtime automatically executes when inputs change
// Creates provenance event:
const computationEvent = await createSignedEvent({
  author: 'system',
  payload: {
    type: 'allocation',
    programHash: hash(program),
    inputs: {
      capacity: {
        provenance: aliceEvent.id,  // Links to Alice!
        contentHash: hash(100)
      },
      mrValues: {
        provenance: bobEvent.id,    // Links to Bob!
        contentHash: hash({ alice: 0.35, carol: 0.65 })
      }
    },
    outputs: {
      allocations: {
        contentHash: hash({ alice: 35, carol: 65 })
      }
    }
  },
  itc: systemStamp,
  parents: [aliceEvent.id, bobEvent.id],  // Merkle-DAG!
  timestamp: new Date().toISOString()
});
await storeEvent(computationEvent);

// ═══════════════════════════════════════════════════════════════
// STEP 3: Query Lineage (Internal)
// ═══════════════════════════════════════════════════════════════

// "Where did this allocation come from?"
const lineage = await traverseToRoots(computationEvent.id);
// Result: [computationEvent, bobEvent, aliceEvent]

// "Verify it's legitimate"
for (const event of lineage) {
  const isValid = await verifyEvent(event);
  console.log(`${event.id}: ${isValid ? '✅' : '❌'}`);
}

// ═══════════════════════════════════════════════════════════════
// STEP 4: Generate ZK Proofs (External Verification)
// ═══════════════════════════════════════════════════════════════

// Build MerkleMap from events
const dagMap = buildDAGMapFromEvents(lineage);

// Prove allocation is part of legitimate lineage
const { proof: membershipProof } = await proveMembershipInDAGMap(
  dagMap,
  computationEvent.id
);

// Prove allocation math is correct (WITHOUT revealing MR values!)
const { proof: allocationProof } = await proveAllocation({
  recipientId: 'alice',
  mrValue: 0.35,        // PRIVATE!
  mrSum: 1.0,           // PRIVATE!
  totalCapacity: 100    // PRIVATE!
});

// ═══════════════════════════════════════════════════════════════
// STEP 5: Share With External Party
// ═══════════════════════════════════════════════════════════════

sendToAuditor({
  membershipProof,      // Proves event exists in DAG (~2KB)
  allocationProof,      // Proves math is correct (~2KB)
  dagRoot: dagMap.getRoot(),
  publicData: {
    allocatedAmount: 35,
    recipientId: 'alice'
  }
});

// Auditor verifies:
const membershipValid = await verify(membershipProof, vkey1);
const allocationValid = await verify(allocationProof, vkey2);

console.log('Membership:', membershipValid);  // ✅
console.log('Allocation:', allocationValid);  // ✅

// Auditor CANNOT see:
// ❌ MR values (private!)
// ❌ Other events in DAG
// ❌ Full lineage
// ❌ Input data

// But auditor CAN verify:
// ✅ Allocation is part of legitimate DAG
// ✅ Math is correct
// ✅ No fraud occurred
```

---

## Key Design Principles

### 1. Separation of Concerns

```
Storage (Mesh)
  ↓ Provides: P2P encrypted database
Kernel (User Space)
  ↓ Provides: Structured namespaces
Provenance (Events)
  ↓ Provides: Cryptographic lineage
ZK (Proofs)
  ↓ Provides: Privacy-preserving verification
Compute (RDL)
  ↓ Provides: Reactive execution

Each layer is independent but composable
```

### 2. Language-Agnostic Core

```
kernel-core.ts        → Works with ANY language
kernel-rdl.ts         → RDL-specific extension
kernel-sql.ts         → (Future) SQL extension
kernel-wasm.ts        → (Future) WASM extension

Provenance system     → Language-agnostic
ZK proof layer        → Works with any events
```

### 3. Hybrid Privacy Model

```
INTERNAL:
  • Full visibility
  • Fast queries
  • Traditional operations
  • No ZK overhead

EXTERNAL:
  • Generate ZK proofs
  • Share only proofs
  • Privacy-preserving
  • Succinct verification
```

### 4. Composable Architecture

```
Each layer provides primitives that compose:

Mesh.subscribe()
  ↓ triggers
RDL.execute()
  ↓ creates
Provenance.createEvent()
  ↓ stores
Provenance.storeEvent()
  ↓ can generate
ZK.proveLineage()

No tight coupling - each layer independent
```

---

## Performance Characteristics

### Storage Layer (Mesh)
- **Write**: O(1) - Local IndexedDB write
- **Read**: O(1) - Local IndexedDB read
- **Sync**: O(N) - N = changes since last sync
- **Space**: ~1-5KB per event

### Kernel Layer
- **Path lookup**: O(1) - Direct path access
- **Subscription**: O(1) - Register callback
- **Validation**: O(N) - N = fields in schema

### Provenance Layer
- **Create event**: O(1) - Hash + sign
- **Store event**: O(1) - Write to Mesh
- **Verify event**: O(1) - Check hash + signature
- **Traverse DAG**: O(D) - D = depth of traversal
- **Build proof**: O(N) - N = events in proof

### ZK Layer
- **Build MerkleMap**: O(N) - N = events
- **Generate proof**: O(256) - MerkleMap height (constant!)
- **Verify proof**: O(256) - Constant time!
- **Proof size**: ~2-8KB - Constant size!

### Compute Runtime
- **Subscribe**: O(1) - Register callback
- **Execute**: O(C) - C = computation complexity
- **Update state**: O(1) - Write result

---

## Summary: The Complete Stack

```
┌─────────────────────────────────────────────────────────────┐
│ APPLICATION: User interfaces, forms, visualizations         │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ COMPUTE: RDL reactive execution with automatic provenance   │
│  • Subscribes to inputs                                     │
│  • Executes computations                                    │
│  • Creates provenance events                                │
│  • Publishes outputs                                        │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ PROVENANCE: Cryptographic event tracking                    │
│  • Content-addressing (SHA-256)                             │
│  • Digital signatures (SEA/ECDSA)                           │
│  • Merkle-DAG (hash-linking)                                │
│  • ITC (causality tracking)                                 │
│  • Storage in Mesh                                       │
│  • Verification engine                                      │
└─────────────────────────────────────────────────────────────┘
                            ↓ (optional)
┌─────────────────────────────────────────────────────────────┐
│ ZK PROOFS: Privacy-preserving verification                  │
│  • Build ephemeral MerkleMap from events                    │
│  • Generate constant-size proofs (~2-8KB)                   │
│  • Prove without revealing                                  │
│  • Verify in O(1) time                                      │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ KERNEL: User space structure & management                   │
│  • kernel-core (language-agnostic)                          │
│  • kernel-rdl (RDL-specific)                                │
│  • kernel-domain (economic coordination)                    │
│  • Subscriptions, causality, replication                    │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ STORAGE: Mesh/Gun P2P encrypted database                │
│  • Local-first (IndexedDB/disk)                             │
│  • P2P sync                                                 │
│  • User spaces (~{pubKey}/)                                 │
│  • Real-time subscriptions                                  │
└─────────────────────────────────────────────────────────────┘
```

**Result**: A complete planetary-scale computational commons with cryptographic provenance, privacy-preserving verification, and distributed consensus - all running for free on participants' devices.

---

## File Locations

```
src/lib/commons/
├─ compute/
│   ├─ schema.ts                    # RDL language
│   ├─ compute.svelte.ts            # Runtime
│   ├─ runtime-manager.svelte.ts    # Lifecycle
│   ├─ kernel-core.ts               # Language-agnostic
│   ├─ kernel-rdl.ts                # RDL-specific
│   ├─ kernel-domain.ts             # Economic domain
│   ├─ kernel.ts                    # Unified exports
│   ├─ kernel.svelte.ts             # Stores
│   ├─ provenance/                  # Provenance system
│   │   ├─ provenance-event-schema.ts
│   │   ├─ provenance-signing.svelte.ts
│   │   ├─ provenance-dag.svelte.ts
│   │   ├─ provenance-verification.svelte.ts
│   │   ├─ provenance-proof.svelte.ts
│   │   ├─ provenance.svelte.ts
│   │   └─ index.ts
│   └─ zk/                          # ZK proof layer
│       ├─ zk-programs.ts
│       ├─ zk-provenance.svelte.ts
│       ├─ zk-dag-map.ts
│       ├─ zk-dag-map-integration.svelte.ts
│       └─ index.ts
├─ utils/
│   └─ itc.ts                       # ITC implementation
└─ state/
    └─ mesh.svelte.ts            # Mesh integration
```

**This is the infrastructure for post-app-italism computing.** 🌍✨

