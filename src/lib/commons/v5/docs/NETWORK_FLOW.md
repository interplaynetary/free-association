# V5 Network Flow - Complete Data Flow Diagram

## 🌊 The Complete Flow

```
┌─────────────────────────────────────────────────────────────────────────┐
│ STEP 1: BUILD MY RECOGNITION TREE                                        │
│ (User Action: Add contributors to tree)                                  │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  User adds "Alice" as contributor (60 pts) to Healthcare node           │
│  User adds "Bob" as contributor (40 pts) to Healthcare node             │
│                                                                           │
│  myRecognitionTreeStore.set(updatedTree)                                │
│  ├─ Persisted to Holster: trees/recognition_tree                        │
│  └─ Triggers: myRecognitionWeights (derived store)                      │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
                                ↓
┌─────────────────────────────────────────────────────────────────────────┐
│ STEP 2: COMPUTE MY RECOGNITION WEIGHTS                                   │
│ (Automatic: Derived Store)                                               │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  myRecognitionWeights (derived from myRecognitionTreeStore)             │
│  ├─ Computation: sharesOfGeneralFulfillmentMap(tree, {})                │
│  ├─ Result: { alice: 0.6, bob: 0.4 }                                    │
│  └─ Triggers: Auto-subscription sync (if enabled)                        │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
                                ↓
┌─────────────────────────────────────────────────────────────────────────┐
│ STEP 3: AUTO-SUBSCRIBE TO CONTRIBUTORS                                   │
│ (Automatic: if enableAutoSubscriptionSync() called)                      │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  Tree changed → syncSubscriptionsWithTree() called                       │
│  ├─ Extract contributors: [alice, bob]                                   │
│  ├─ Compare with current subscriptions: []                               │
│  ├─ New: [alice, bob]                                                    │
│  └─ Actions:                                                             │
│     ├─ subscribeToCommitment('alice')                                    │
│     │  └─ Holster subscribes to: alice/allocation/commitment            │
│     └─ subscribeToCommitment('bob')                                      │
│        └─ Holster subscribes to: bob/allocation/commitment              │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
                                ↓
┌─────────────────────────────────────────────────────────────────────────┐
│ STEP 4: RECEIVE NETWORK COMMITMENTS                                      │
│ (Network: Holster delivers their commitments)                            │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  Alice's commitment arrives from Holster                                 │
│  ├─ Contains: { global_recognition_weights: { me: 0.5, ... } }          │
│  └─ networkCommitments.update(map => map.set('alice', commitment))      │
│                                                                           │
│  Bob's commitment arrives from Holster                                   │
│  ├─ Contains: { global_recognition_weights: { me: 0.3, ... } }          │
│  └─ networkCommitments.update(map => map.set('bob', commitment))        │
│                                                                           │
│  NOTE: Their recognition_weights are AUTOMATICALLY NORMALIZED!           │
│  (Done in subscribeToCommitment() to ensure proper fractions)           │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
                                ↓
┌─────────────────────────────────────────────────────────────────────────┐
│ STEP 5: COMPUTE MUTUAL RECOGNITION                                       │
│ (Automatic: Derived Store)                                               │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  myMutualRecognition (derived from myRecognitionWeights + networkCommit)│
│  ├─ My recognition: { alice: 0.6, bob: 0.4 }                            │
│  ├─ Their recognition of me:                                             │
│  │  ├─ Alice recognizes me: 0.5                                          │
│  │  └─ Bob recognizes me: 0.3                                            │
│  ├─ Mutual recognition formula: MR(me, X) = min(myRec[X], XRec[me])     │
│  └─ Result: { alice: min(0.6, 0.5) = 0.5, bob: min(0.4, 0.3) = 0.3 }   │
│                                                                           │
│  Console: "[MUTUAL-REC] Computed mutual recognition: 2 mutual relations"│
│                                                                           │
│  Triggers: enableAutoCommitmentComposition() (if enabled)                │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
                                ↓
┌─────────────────────────────────────────────────────────────────────────┐
│ STEP 6: COMPOSE MY COMMITMENT                                            │
│ (Manual or Automatic)                                                    │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  composeCommitmentFromSources() called (auto or manual)                  │
│  ├─ Gathers from source stores:                                          │
│  │  ├─ need_slots ← myNeedSlotsStore                                     │
│  │  ├─ capacity_slots ← myCapacitySlotsStore                             │
│  │  ├─ global_recognition_weights ← myRecognitionWeights (derived)      │
│  │  └─ global_mr_values ← myMutualRecognition (derived)                 │
│  │                                                                        │
│  ├─ Preserves stateful data:                                             │
│  │  └─ multi_dimensional_damping (from existing commitment)             │
│  │                                                                        │
│  └─ Result: Complete Commitment object                                   │
│     {                                                                     │
│       need_slots: [...],                                                 │
│       capacity_slots: [...],                                             │
│       global_recognition_weights: { alice: 0.6, bob: 0.4 },             │
│       global_mr_values: { alice: 0.5, bob: 0.3 },                       │
│       multi_dimensional_damping: {...},                                  │
│       itcStamp: {...},                                                   │
│       timestamp: 1234567890                                              │
│     }                                                                     │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
                                ↓
┌─────────────────────────────────────────────────────────────────────────┐
│ STEP 7: PUBLISH MY COMMITMENT                                            │
│ (Automatic: Holster persistence)                                         │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  myCommitmentStore.set(commitment)                                       │
│  ├─ Persisted to Holster: allocation/commitment                          │
│  └─ Published to network (other participants can now subscribe)         │
│                                                                           │
│  Network can now see:                                                    │
│  ├─ My needs (what I need from commons)                                 │
│  ├─ My capacity (what I provide to commons)                              │
│  ├─ Who I recognize (my global_recognition_weights)                     │
│  └─ My mutual recognition values (for allocation)                        │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
                                ↓
┌─────────────────────────────────────────────────────────────────────────┐
│ STEP 8: ALLOCATION (Two-Tier Algorithm)                                  │
│ (Automatic: Reactive allocation computation)                             │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  Free-algorithm.svelte.ts                                                │
│  ├─ Reads: myCommitmentStore (my commitment)                             │
│  ├─ Reads: networkCommitments (their commitments)                        │
│  ├─ Computes:                                                            │
│  │  ├─ TIER 1: Mutual tier (MR > 0)                                      │
│  │  │  └─ Allocate based on: MR × active need                           │
│  │  └─ TIER 2: Non-mutual tier (I recognize, but not mutual)            │
│  │     └─ Allocate remaining capacity                                    │
│  │                                                                        │
│  └─ Result: Allocations for each recipient                               │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
```

## 🔑 Key Reactive Relationships

### 1. **Tree → Weights** (Automatic)
```typescript
myRecognitionTreeStore → myRecognitionWeights (derived)
```
When tree changes, weights auto-recompute via `sharesOfGeneralFulfillmentMap()`

### 2. **Tree → Subscriptions** (Automatic with Auto-Sync)
```typescript
myRecognitionTreeStore → syncSubscriptionsWithTree()
```
When tree changes, subscriptions auto-sync (add/remove participants)

### 3. **Network + Weights → Mutual Recognition** (Automatic)
```typescript
(networkCommitments + myRecognitionWeights) → myMutualRecognition (derived)
```
When either changes, mutual recognition auto-recomputes

### 4. **Sources → Commitment** (Manual or Automatic)
```typescript
(tree + needs + capacity + mutualRec) → myCommitmentStore
```
With `enableAutoCommitmentComposition()`, commitment auto-updates

## 📊 Data Dependencies

```
myRecognitionTreeStore (SOURCE)
    ↓
myRecognitionWeights (DERIVED from tree)
    ↓
syncSubscriptionsWithTree() (DERIVED from weights)
    ↓
networkCommitments (NETWORK data)
    ↓
myMutualRecognition (DERIVED from weights + network)
    ↓
myCommitmentStore (COMPOSED from all above)
```

## 🎯 What Goes Into a Commitment?

```typescript
Commitment = {
  // From SOURCE stores
  need_slots,              // ← myNeedSlotsStore
  capacity_slots,          // ← myCapacitySlotsStore
  
  // From DERIVED stores
  global_recognition_weights,  // ← myRecognitionWeights (from tree)
  global_mr_values,            // ← myMutualRecognition (from weights + network)
  
  // STATEFUL (preserved)
  multi_dimensional_damping,   // ← Preserved from previous commitment
  
  // METADATA
  itcStamp,                    // ← Incremental (ITC causality)
  timestamp                    // ← Current time
}
```

## 🔄 Reactive Update Chains

### Chain 1: User Edits Tree
```
User edits tree
  → myRecognitionTreeStore.set()
    → myRecognitionWeights updates (derived)
      → syncSubscriptionsWithTree() runs (if auto-sync enabled)
        → New subscriptions created
          → Network commitments arrive
            → myMutualRecognition updates (derived)
              → Commitment recomposes (if auto-compose enabled)
                → Published to network
```

### Chain 2: Network Commitment Arrives
```
Network commitment arrives (Alice recognizes me!)
  → networkCommitments.update()
    → myMutualRecognition updates (derived)
      → Commitment recomposes (if auto-compose enabled)
        → Published to network with updated MR values
```

### Chain 3: User Adds Need
```
User adds need slot
  → myNeedSlotsStore.set()
    → Commitment recomposes (if auto-compose enabled)
      → Published to network with new needs
```

## 🚀 Initialization Sequence

```typescript
// 1. Initialize stores (load from Holster)
initializeAllocationStores();

// 2. Enable auto-subscription (watch tree for contributors)
const unsubSync = enableAutoSubscriptionSync();

// 3. Enable auto-composition (watch sources for changes)
const unsubCompose = enableAutoCommitmentComposition();

// Now fully reactive!
// - Edit tree → weights update → subscriptions sync → commitments arrive → MR updates → commitment publishes
// - Edit needs → commitment publishes
// - Edit capacity → commitment publishes
```

## 💡 Key Insights

### 1. **Tree is Source of Truth for Recognition**
- Don't manually edit recognition weights
- Edit the tree, weights auto-compute

### 2. **Network Commitments Complete the Picture**
- My weights = who I recognize
- Network commitments = who they recognize (including me!)
- Mutual recognition = intersection

### 3. **Subscriptions are Dynamic**
- Add contributor to tree → auto-subscribe
- Remove contributor from tree → auto-unsubscribe
- No manual subscription management needed

### 4. **Commitment is Compositional**
- Not a single source store
- Composed from multiple sources + derived values
- Can be auto-composed or manually controlled

### 5. **Everything is Reactive**
- Change tree → everything updates automatically
- Network update → mutual recognition updates automatically
- Fully reactive data flow

## 🛠️ Manual vs Automatic Control

### Automatic Mode (Recommended for Most Users):
```typescript
initializeAllocationStores();
enableAutoSubscriptionSync();      // Auto-manage subscriptions
enableAutoCommitmentComposition(); // Auto-publish commitment

// Just edit sources, everything else is automatic!
myRecognitionTreeStore.set(newTree);
myNeedSlotsStore.set(newNeeds);
```

### Manual Mode (Advanced Users):
```typescript
initializeAllocationStores();

// Manually manage subscriptions
syncSubscriptionsWithTree(); // When you're ready

// Manually compose commitment
const commitment = composeCommitmentFromSources();
myCommitmentStore.set(commitment); // When you're ready
```

## 🎓 Example Walkthrough

```
SCENARIO: I want to recognize Alice for her medical help

STEP 1: I edit my recognition tree
  myRecognitionTreeStore.set(treeWithAlice)
  
STEP 2: Weights auto-compute
  myRecognitionWeights → { alice: 0.4, ... }
  
STEP 3: Auto-subscribe to Alice (if enabled)
  subscribeToCommitment('alice')
  
STEP 4: Wait for Alice's commitment
  (Holster delivers it when Alice publishes)
  networkCommitments.set('alice', aliceCommitment)
  
STEP 5: Mutual recognition auto-computes
  Alice recognizes me: 0.3
  I recognize Alice: 0.4
  Mutual: min(0.4, 0.3) = 0.3
  
STEP 6: Commitment auto-composes (if enabled)
  global_recognition_weights: { alice: 0.4 }
  global_mr_values: { alice: 0.3 }
  
STEP 7: Published to network
  Others can now see I recognize Alice
  Allocation can use MR(me, alice) = 0.3
  
DONE! Fully reactive, no manual steps needed.
```

## 📈 Performance Characteristics

- **Tree → Weights**: O(N×M) where N=nodes, M=contributors per node
- **Subscriptions Sync**: O(C) where C=contributors in tree
- **Mutual Recognition**: O(P) where P=participants (fast!)
- **Composition**: O(1) (just copying references)

All reactive, all efficient! 🚀

