# Commons Architecture v2 - ITC & Event-Driven Design

This guide outlines the architecture of the **Mutual-Priority Allocation Commons** - a peer-to-peer system for decentralized resource allocation using mutual recognition and slot-based capacity matching.

Built with **Holster** (P2P sync), **Svelte 5** (reactive state), **ITC** (causality tracking), **Zod v4** (schema validation), and **TypeScript**.

---

## Architecture Overview

The commons implements a **slot-native two-tier allocation algorithm** where:

1. **Participants declare** capacity slots (what they can provide) and need slots (what they need)
2. **Recognition weights** determine allocation priority (mutual recognition gets priority)
3. **Slot matching** ensures time/location compatibility before allocation
4. **Adaptive damping** prevents oscillations during convergence
5. **ITC stamps** track causality in a fully decentralized manner

---

## Key Design Principles (v2)

### 1. **Event-Driven, Not Round-Based** ✅

**Old (v1)**: Discrete rounds with synchronized phases (0-90 seconds per round)
**New (v2)**: Continuous reactive flow (recompute on every change, ~100ms latency)

```typescript
// v1: Wait for round advancement
while (!shouldAdvanceRound()) await sleep(100);
await advanceToNextRound();

// v2: Automatic reactive recomputation
const myAllocations = derived(
  [myCommitmentStore, networkCommitments],
  ($deps) => computeAllocation(...$deps)
);
```

### 2. **ITC for Causality, Not Vector Clocks** ✅

**Old (v1)**: Vector clocks (unbounded growth, O(all participants ever))
**New (v2)**: ITC stamps (adaptive size, O(log active participants))

```typescript
// v1: Vector clock (grows forever)
VectorClock = { alice: 5, bob: 3, charlie: 0, dave: 0, ... }

// v2: ITC stamp (compact tree)
Stamp = { id: {l: 1, r: 0}, event: 5 }
```

### 3. **Continuous Convergence Monitoring** ✅

**Old (v1)**: Check convergence once per round (90-second intervals)
**New (v2)**: Check after every computation (sub-second detection)

### 4. **Time-Based Damping History** ✅

**Old (v1)**: Last 3 rounds (≈270 seconds, depends on round duration)
**New (v2)**: Last 30 seconds or last 3 computations (precise, configurable)

---

## Store Architecture (v2)

### Raw Data Stores (Holster-Backed)

| Store | Type | Source | Updates | Holster Path |
|-------|------|--------|---------|--------------|
| `myCommitmentStore` | HolsterStore | My local state | When I change capacity_slots/need_slots/recognition | `allocation/commitment` |
| `myAllocationStateStore` | HolsterStore | My local computation | **Reactive** - auto-updates when commitments change | `allocation/allocationState` |
| `myRecognitionWeightsStore` | HolsterStore | My local state | When I update recognition weights | `allocation/recognitionWeights` |
| `networkCommitments` | Map<pubKey, Commitment> | Holster subscriptions | Real-time from subscribed participants | `~pubKey/allocation/commitment` |
| `networkAllocationStates` | Map<pubKey, AllocationState> | Holster subscriptions | Real-time from subscribed providers | `~pubKey/allocation/allocationState` |
| `networkRecognitionWeights` | Map<pubKey, Record> | Holster subscriptions | Real-time from subscribed participants | `~pubKey/allocation/recognitionWeights` |

**Removed**: ~~`myRoundStateStore`~~, ~~`networkRoundStates`~~ (no rounds!)

### Derived Subgroups (Algorithm-Driven)

| Subgroup | Derived From | Purpose | Data Needed | Subscription Type |
|----------|--------------|---------|-------------|-------------------|
| `myMutualRecognition` | `myRecognitionWeightsStore` + `networkRecognitionWeights` | Bilateral MR values → compute priority allocations | My weights + their weights | Full (for MR computation) |
| `myMutualBeneficiaries` | `myMutualRecognition` | People with MR > 0 → Tier 1 allocation | Their commitments (need_slots) | Commitment + Allocation + Recognition |
| `myNonMutualBeneficiaries` | `myRecognitionWeightsStore` - `myMutualRecognition` | One-way recognition → Tier 2 allocation | Their commitments (need_slots) | Commitment only |
| `mutualProvidersForMe` | `myMutualRecognition` + `networkCommitments` | Providers with MR > 0 and capacity → priority allocation to me | Their commitments, allocations | Commitment + Allocation + Recognition |
| `nonMutualProvidersForMe` | `networkCommitments` - `myMutualRecognition` | Providers who recognize me (one-way) → leftover allocation to me | Their commitments, allocations | Commitment + Allocation |
| `mutualBeneficiariesWithNeeds` | `myMutualBeneficiaries` + `networkCommitments` | Mutual partners with need_slots > 0 → contribute to Tier 1 denominators | Their need_slots, damping_factor | Commitment (filtered) |
| `nonMutualBeneficiariesWithNeeds` | `myNonMutualBeneficiaries` + `networkCommitments` | Non-mutual recipients with need_slots > 0 → contribute to Tier 2 denominators | Their need_slots, damping_factor | Commitment (filtered) |
| `allMutualPartners` | `myMutualBeneficiaries` ∪ `mutualProvidersForMe` | Bidirectional recognition → full data exchange | Everything | Full participant subscription |
| `activeParticipants` | `networkCommitments` + timestamp check | Fresh commitments (< 60s) → currently active | All commitment data | Commitment (freshness filtered) |
| `oscillatingParticipants` | `networkCommitments` + damping_factor check | Participants with damping < 1.0 → convergence issues | damping_factor, damping_history | Commitment (damping filtered) |
| `hasSystemConverged` | `myAllocationStateStore` + previous denominators | Denominators stable (Δ < ε) → equilibrium reached | slot_denominators (current + previous) | N/A (local computation) |

---

## Continuous Async Operations (Event-Driven)

| Operation | Trigger | Purpose | Debounce | Coordination |
|-----------|---------|---------|----------|--------------|
| **Commitment Publishing** | User changes capacity/need slots or recognition | Broadcast my state to network | 100ms | ITC stamp increment |
| **Recognition Weights Publishing** | User updates recognition weights | Update MR values with others | 200ms | ITC stamp increment |
| **Algorithm-Driven Subscriptions** | `allMutualPartners`, `myNonMutualBeneficiaries` change | Maintain data connections based on recognition | 100ms | Reactive (automatic) |
| **MR Computation** | When either my weights or their weights change | Compute min(myRec, theirRec) for all participants | Immediate (derived) | Reactive (automatic) |
| **Slot Matching** | When slots change | Find compatible recipients for each availability slot | Per computation | Pure function |
| **Allocation Computation** | **When commitments or MR values change (reactive)** | Compute two-tier slot-native allocations | 100ms (debounced) | **Reactive (automatic)** |
| **Allocation Publishing** | After allocation computation completes | Broadcast allocations to recipients | 100ms | ITC stamp increment |
| **Convergence Detection** | After allocation update | Check if slot denominators stable (Δ < ε) | Immediate (derived) | Continuous monitoring |
| **Damping Update** | After receiving total allocations | Update damping_history, recompute damping_factor | On computation | Time-based history |

**Removed**: ~~Vector Clock Gossip~~, ~~Round Advancement Check~~ (no rounds!)

---

## Event-Driven Flow (No Rounds)

### Continuous Reactive Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ 1. PARTICIPANT PUBLISHES COMMITMENT                         │
│    Action: User updates capacity/need slots                 │
│    Causality: myITCStamp = event(myITCStamp)                │
│    Persist: myCommitmentStore.set(commitment)               │
│    Latency: ~100ms (debounced)                              │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 2. PEERS RECEIVE COMMITMENT (via Holster sync)             │
│    Causality Check: leq(peerStamp, myStamp)?               │
│    If new: Merge stamps: myStamp = join(myStamp, peer)     │
│    Store: networkCommitments.set(pubKey, commitment)        │
│    Triggers: Reactive recomputation (automatic)             │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 3. REACTIVE ALLOCATION COMPUTATION (Automatic)              │
│    Triggered by: networkCommitments change (Svelte derived) │
│    Compute: myAllocations = computeAllocation(...)          │
│    Check convergence: Compare with previous denominators    │
│    Latency: ~50ms (computation) + 100ms (debounce)          │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 4. PUBLISH ALLOCATION STATE                                 │
│    Causality: myITCStamp = event(myITCStamp)                │
│    Persist: myAllocationStateStore.set(allocations)         │
│    Broadcast: Via Holster to all recipients                 │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 5. RECIPIENTS RECEIVE ALLOCATIONS                           │
│    Aggregate: Total received from all providers             │
│    Update damping: Add to time-based history                │
│    Check: Over-allocation oscillation detection             │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 6. CONTINUOUS CONVERGENCE MONITORING                        │
│    Monitor: Each participant's convergence flag             │
│    Network convergence: ≥80% of participants converged      │
│    No waiting: Detected in real-time                        │
└─────────────────────────────────────────────────────────────┘
```

**Key differences from v1**:
- ❌ No discrete phases (0-90s timing)
- ❌ No "wait for ≥50% to advance"
- ❌ No round synchronization barriers
- ✅ Fully reactive (trigger on change)
- ✅ Continuous flow (no artificial delays)
- ✅ Sub-second latency (vs 0-90s)

---

## ITC Causality Rules (Replaces Vector Clock Rules)

| Rule | Scenario | Action | Data Required |
|------|----------|--------|---------------|
| **Happened-Before** | leq(peerStamp, myStamp) && !equals() | Accept (causally later) | Peer's ITC stamp |
| **Concurrent** | concurrent(myStamp, peerStamp) | Accept both (merge stamps) | Both ITC stamps |
| **Stale Update** | leq(peerStamp, myStamp) && equals() | Ignore (already processed) | Peer's ITC stamp |
| **Stamp Merge** | Receive peer's stamp | myStamp = join(myStamp, peerStamp) | Both ITC stamps |
| **Self Increment** | Before publishing state | myStamp = event(myStamp) | Own stamp |
| **Participant Join** | New peer joins | [myStamp, newStamp] = fork(myStamp) | Own stamp |
| **Participant Leave** | Peer becomes inactive | No action (stamp stops advancing) | N/A (natural) |

**Key improvements over vector clocks**:
- ✅ O(log n) space instead of O(all participants ever)
- ✅ Natural join/leave semantics (fork/stop)
- ✅ No unbounded growth
- ✅ Automatic adaptation to participant churn

---

## Data Requirements Per Operation

### Provider's Perspective (Computing Allocations)

| Operation | Data Subset | Schema | Freshness | Source |
|-----------|-------------|--------|-----------|--------|
| **Slot Matching** | All participants' need_slots + capacity_slots | `AvailabilitySlotSchema`, `NeedSlotSchema` | < 60s | `networkCommitments` (filtered by freshness) |
| **MR Computation** | Mutual partners' recognition_weights | `Record<string, number>` | < 60s | `myRecognitionWeightsStore` + `networkRecognitionWeights` |
| **Tier 1 Numerators** | Mutual recipients' need_slots, damping_factor | `CommitmentSchema` | < 60s | `mutualBeneficiariesWithNeeds` |
| **Tier 2 Numerators** | Non-mutual recipients' need_slots, damping_factor | `CommitmentSchema` | < 60s | `nonMutualBeneficiariesWithNeeds` |
| **Capping Check** | Each recipient's total need_slots.quantity | `NeedSlotSchema[]` | < 60s | `networkCommitments` |
| **Causality Check** | Peer ITC stamps | `Stamp` | Current | Commitment.itcStamp |

### Recipient's Perspective (Receiving Allocations)

| Operation | Data Subset | Schema | Freshness | Source |
|-----------|-------------|--------|-----------|--------|
| **Expected Allocation** | Providers' slot_denominators + my position in them | `TwoTierAllocationStateSchema` | < 60s | `networkAllocationStates` |
| **Total Received** | All providers' slot_allocations where recipient = me | `SlotAllocationRecordSchema[]` | < 60s | `networkAllocationStates` (aggregated) |
| **Damping Update** | My stated_need (sum of need_slots) + total_received | `CommitmentSchema` | Current | `myCommitmentStore` + aggregated allocations |
| **Convergence Check** | My slot_denominators (current + previous) | `TwoTierAllocationStateSchema` | Current + Previous | `myAllocationStateStore` + tracked history |
| **Causality Check** | Provider ITC stamps | `Stamp` | Current | AllocationState.itcStamp |

**Removed**: ~~Vector Clock Merge~~, ~~Round Consensus~~, ~~Advancement Decision~~ (no rounds!)

---

## Layer 1: Schemas (Data Validation)

**File**: `schemas.ts`

All data structures are defined using **Zod schemas** for runtime validation and TypeScript type inference.

### Core Schemas (v2 Updates):

- **`AvailabilitySlotSchema`** - A specific time/location/quantity of available capacity
- **`NeedSlotSchema`** - A specific time/location/quantity of needed capacity
- **`CommitmentSchema`** - A participant's declaration (capacity_slots, need_slots, recognition_weights, damping state, **itcStamp**)
- **`TwoTierAllocationStateSchema`** - Provider's computed allocations (slot denominators, slot allocations, recipient totals, **converged flag**)
- **`ITCStampSchema`** - ITC causality stamp (replaces VectorClockSchema)

**Removed**: ~~`RoundStateSchema`~~, ~~`VectorClockSchema`~~ (no rounds, no vector clocks!)

### Updated Commitment Schema

```typescript
export const CommitmentSchema = z.object({
  pubKey: z.string(),
  
  // Capacity & needs
  capacity_slots: z.array(AvailabilitySlotSchema),
  need_slots: z.array(NeedSlotSchema),
  
  // Recognition
  recognition_weights: z.record(z.string(), z.number()),
  mr_values: z.record(z.string(), z.number()).optional(),
  
  // Causality tracking (ITC)
  itcStamp: ITCStampSchema,
  timestamp: z.number().int().positive(),
  
  // Adaptive damping (time-based history)
  damping_factor: z.number().min(0).max(1).default(1.0),
  damping_history: z.array(z.object({
    overAllocation: z.number(),
    timestamp: z.number().int().positive()
  })).optional()
});
```

**Changes from v1**:
- ✅ `itcStamp` instead of `vectorClock`
- ❌ Removed `round` field
- ✅ `damping_history` is now time-stamped (not round-indexed)

### Updated Allocation State Schema

```typescript
export const TwoTierAllocationStateSchema = z.object({
  // Slot-level allocations
  slot_denominators: z.record(z.string(), z.object({
    mutual: z.number(),
    nonMutual: z.number()
  })),
  slot_allocations: z.array(SlotAllocationRecordSchema),
  recipient_totals: z.record(z.string(), z.number()),
  
  // Convergence tracking
  converged: z.boolean(),
  convergenceHistory: z.array(z.object({
    denominatorDelta: z.number(),
    timestamp: z.number().int().positive()
  })).optional(),
  
  // Causality tracking (ITC)
  itcStamp: ITCStampSchema,
  timestamp: z.number().int().positive()
});
```

**Changes from v1**:
- ✅ `converged` flag (local convergence state)
- ✅ `convergenceHistory` with timestamps
- ✅ `itcStamp` instead of `vectorClock`
- ❌ Removed `round` field

---

## Layer 2: Store Factory (Generic Persistence)

**File**: `store.svelte.ts`

Provides a **generic factory** for creating Holster-backed stores with any Zod schema.

### Features:

- **Zod validation** - All network data validated before updating store
- **Timestamp conflict resolution** - Network wins if newer
- **localStorage caching** - Instant UI on page load
- **Debounced persistence** - Configurable debounce to batch updates
- **Queue management** - Handles simultaneous network/local updates
- **Cross-user subscriptions** - Subscribe to any participant's data
- **ITC causality checks** - Filter stale updates automatically

---

## Layer 3: Allocation Stores (Typed Persistence)

**File**: `stores.svelte.ts`

Uses the generic store factory to create **typed stores** for allocation data.

### My Data Stores (Published to Network):

- **`myCommitmentStore`** - My capacity_slots, need_slots, recognition_weights, damping state, **itcStamp**
- **`myAllocationStateStore`** - My computed allocations (if I'm a provider), **converged flag**, **itcStamp**
- **`myRecognitionWeightsStore`** - My recognition of others (% weights)

**Removed**: ~~`myRoundStateStore`~~ (no rounds!)

### Network Data Maps (Subscribed from Network):

- **`networkCommitments`** - Map<pubKey, Commitment> from all participants
- **`networkAllocationStates`** - Map<pubKey, AllocationState> from providers
- **`networkRecognitionWeights`** - Map<pubKey, weights> for MR computation

**Removed**: ~~`networkRoundStates`~~ (no rounds!)

### Subscription Management:

Provides functions to subscribe to specific participants:

- `subscribeToCommitment(pubKey)` - Get their capacity/needs
- `subscribeToAllocationState(pubKey)` - Get their allocations
- `subscribeToRecognitionWeights(pubKey)` - Get their recognition weights
- `subscribeToFullParticipant(pubKey)` - Subscribe to all data from a mutual partner

---

## Layer 4: Algorithm (Allocation Logic)

**File**: `algorithm.svelte.ts`

Implements the **mutual-priority allocation algorithm** with slot-native processing and **reactive recomputation**.

### Derived Stores (Reactive Subgroups):

All subgroups are **derived from base stores** and auto-update:

- **`myMutualRecognition`** - My MR values with all participants (min of bilateral recognition)
- **`myMutualBeneficiaries`** - People with MR > 0 (priority allocation)
- **`myNonMutualBeneficiaries`** - People I recognize one-way (leftover capacity)
- **`mutualProvidersForMe`** - Providers with MR > 0 who have capacity
- **`nonMutualProvidersForMe`** - Providers who recognize me one-way
- **`mutualBeneficiariesWithNeeds`** - Mutual partners who still have needs
- **`activeParticipants`** - Participants with fresh commitments (< 60s old)
- **`oscillatingParticipants`** - Participants with damping_factor < 1.0
- **`hasSystemConverged`** - True when denominators stabilized (< 0.001 change)

### NEW: Reactive Allocation Computation

```typescript
/**
 * Reactive allocation computation
 * Automatically recomputes when commitments or recognition changes
 */
export const myAllocationsReactive = derived(
  [myCommitmentStore, networkCommitments, myMutualRecognition, myRecognitionWeightsStore],
  ([$myCommit, $network, $mr, $weights], set) => {
    if (!$myCommit || !$weights) return;
    
    // Compute allocations (pure function)
    const allocations = computeAllocation(
      myPubKey,
      $myCommit,
      $mr,
      $weights,
      $network
    );
    
    // Check convergence (continuous)
    const converged = checkConvergence(
      allocations.slot_denominators,
      previousDenominators
    );
    
    // Update history for next check
    previousDenominators = allocations.slot_denominators;
    
    // Increment ITC stamp (new allocation published)
    myITCStamp = event(myITCStamp);
    
    // Return with convergence flag
    set({
      ...allocations,
      converged,
      itcStamp: myITCStamp,
      timestamp: Date.now()
    });
    
    // Publish to network (debounced)
    debouncedPublish();
  },
  undefined,
  { debounce: 100 } // Debounce to prevent too frequent recomputations
);
```

**Key change**: Allocation is now **reactive** (automatic), not manual!

### Algorithm-Driven Subscriptions:

The algorithm **automatically manages subscriptions** based on subgroups:

- Mutual partners → Subscribe to full data (commitment, allocation, recognition)
- Non-mutual beneficiaries → Subscribe to commitments only (to see their needs)
- Non-mutual providers → Subscribe to commitments + allocations (to compute expected allocation)

**Initialization**:

```typescript
import { initializeAlgorithmSubscriptions } from './algorithm.svelte';

// After Holster authentication + store initialization
initializeAlgorithmSubscriptions();
```

### Core Algorithm Functions:

**`computeAllocation()`** - Slot-native two-tier allocation (UNCHANGED):

1. For each availability slot:
   - Find compatible recipients (time/location matching)
   - **Tier 1**: Allocate to mutual recipients (MR-weighted, priority)
   - **Tier 2**: Allocate remaining to non-mutual recipients (one-way weighted)
   - Cap all allocations by recipient's actual need (contractiveness)
2. Return slot-level denominators, allocations, and recipient totals

**Performance Optimizations** (UNCHANGED):
- Bucketing (reduce compatibility checks from O(N×A) to O(N×A_bucket))
- Pre-computed compatibility matrix (avoid redundant checks)
- Active set tracking (filter out recipients without recognition early)
- Early exit conditions (skip slots with no compatible/recognized recipients)

**`computeMutualRecognition()`** - Bilateral MR calculation (UNCHANGED):

```
MR(Me, Them) = min(MyRecognitionOf[Them], TheirRecognitionOf[Me])
```

**`updateCommitmentDamping()`** - Adaptive damping (UPDATED):

- Tracks over-allocation history (last 30 seconds or last 3 computations)
- Detects oscillation pattern (up-down-up or down-up-down)
- Adjusts damping factor: 0.5 (oscillating), 0.8 (moderate), 1.0 (smooth)
- `ActiveNeed = ResidualNeed × DampingFactor`

**Changes from v1**:
```typescript
// v1: Last 3 rounds
history = [round87, round88, round89];

// v2: Last 30 seconds (time-based)
history = [
  { overAlloc: 5, timestamp: t1 },
  { overAlloc: 8, timestamp: t2 },
  { overAlloc: 4, timestamp: t3 }
].filter(m => m.timestamp > Date.now() - 30000);
```

### ITC Functions (NEW):

- **`incrementMyITCStamp()`** - Call before publishing state changes
- **`mergeITCStampFromPeer()`** - Merge peer's stamp with mine
- **`isPeerUpdateStale()`** - Check if peer's update is already seen
- **`getCausallyConsistentCommitments()`** - Get snapshot of causally-consistent commitments

**Removed**: ~~`incrementMyVectorClock()`~~, ~~`updateVectorClockFromPeer()`~~, ~~`advanceToNextRound()`~~, ~~`shouldAdvanceRound()`~~, ~~`publishMyRoundState()`~~

---

## Layer 5: Slot Matching (Compatibility Logic)

**File**: `match.svelte.ts`

Pure functions for slot compatibility checking, bilateral filters, and space-time utilities.

**(UNCHANGED from v1)**

---

## Layer 6: Visualization (UI Component)

**File**: `Visualization.svelte`

Svelte component for visualizing allocation state, subgroups, and network health.

**(UNCHANGED from v1)**

---

## Reactive Data Flow (v2)

```
┌─────────────────────────────────────────────────────────────┐
│ 1. USER ACTIONS                                             │
│    - Declare capacity_slots / need_slots                    │
│    - Set recognition_weights                                │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 2. LOCAL STORES (myCommitmentStore, myRecognitionStore)    │
│    - Validate with Zod                                      │
│    - Update svelte store                                    │
│    - Increment ITC stamp: myStamp = event(myStamp)         │
│    - Persist to Holster (debounced)                         │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 3. NETWORK SYNC (Holster P2P)                              │
│    - Broadcast to peers                                     │
│    - Receive updates from peers                             │
│    - Check causality: leq(peerStamp, myStamp)?             │
│    - Merge stamps: myStamp = join(myStamp, peerStamp)      │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 4. NETWORK STORES (networkCommitments, networkWeights)     │
│    - Validate with Zod                                      │
│    - Update Map<pubKey, data>                               │
│    - Trigger derived store recomputation (AUTOMATIC!)       │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 5. REACTIVE ALLOCATION (myAllocationsReactive)              │
│    - AUTOMATICALLY triggered by commitment changes          │
│    - Compute slot-native two-tier allocations               │
│    - Apply adaptive damping (time-based history)            │
│    - Cap by recipient needs (contractiveness)               │
│    - Check convergence (continuous)                         │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 6. PUBLISH ALLOCATION STATE (Automatic)                     │
│    - Increment ITC stamp: myStamp = event(myStamp)         │
│    - Update myAllocationStateStore                          │
│    - Persist to Holster (debounced)                         │
│    - Broadcast to network                                   │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 7. RECIPIENTS RECEIVE ALLOCATIONS (Continuous)              │
│    - Subscribe to provider's allocationState                │
│    - See expected allocation (slot-level, real-time)        │
│    - Aggregate from all providers                           │
│    - Update damping history (time-based)                    │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 8. CONTINUOUS CONVERGENCE MONITORING                        │
│    - Each participant tracks local convergence              │
│    - Network convergence = ≥80% converged                   │
│    - Real-time detection (no waiting for rounds)            │
└─────────────────────────────────────────────────────────────┘
```

**Key differences from v1**:
- ✅ Steps 5-7 are **fully automatic** (reactive triggers)
- ✅ **No manual coordination** (no round advancement)
- ✅ **No waiting periods** (no 90-second rounds)
- ✅ **Continuous monitoring** (not once-per-round)
- ✅ **Sub-second latency** (vs 0-90 seconds)

---

## Implementation Checklist

### Step 1: Define Schemas (v2 Updates)
✓ Update Commitment schema (itcStamp, damping_history with timestamps)
✓ Update AllocationState schema (converged flag, itcStamp)
✓ Add ITCStampSchema
✓ Remove RoundStateSchema
✓ Remove VectorClockSchema

### Step 2: Set Up Stores (v2 Updates)
✓ Update my data stores (commitment, allocation, recognition with ITC)
✓ Update network data maps (commitments, allocations, weights with ITC)
✓ Remove myRoundStateStore
✓ Remove networkRoundStates
✓ Call `initializeAllocationStores()` after Holster authentication

### Step 3: Set Up Reactive Allocations (NEW)
✓ Create `myAllocationsReactive` derived store
✓ Automatic recomputation on commitment/recognition changes
✓ Debounced publishing (100ms)
✓ Continuous convergence checking

### Step 4: Replace Vector Clocks with ITC (NEW)
✓ Implement `incrementMyITCStamp()`
✓ Implement `mergeITCStampFromPeer()`
✓ Implement `isPeerUpdateStale()`
✓ Remove all vector clock functions

### Step 5: Remove Round Coordination (NEW)
✓ Delete `shouldAdvanceRound()`
✓ Delete `advanceToNextRound()`
✓ Delete `publishMyRoundState()`
✓ Delete `handlePeerRoundState()`
✓ Delete `startRoundStateGossip()`
✓ Delete `stopRoundStateGossip()`

### Step 6: Update Damping to Time-Based (NEW)
✓ Track damping_history with timestamps
✓ Filter by time window (30 seconds) or count (last 3)
✓ Remove round-based history

### Step 7: Implement Continuous Convergence (NEW)
✓ Check convergence after every computation
✓ Track convergenceHistory with timestamps
✓ Monitor network convergence (≥80% converged)

### Step 8: Implement UI (v2 Updates)
✓ Subscribe to derived stores in components
✓ Display real-time convergence status
✓ Show ITC stamp info (for debugging)
✓ Remove round number displays

### Step 9: Handle Lifecycle (v2 Updates)
✓ Initialize on authentication: `initializeAllocationStores()` → `initializeAlgorithmSubscriptions()`
✓ Cleanup on logout: `cleanupAllocationStores()` → `cleanupAlgorithmSubscriptions()`
✓ No round state cleanup needed

---

## Key Design Principles (v2)

1. **Schema-First**: All types inferred from Zod schemas (single source of truth)
2. **Validation at Boundaries**: Validate all network data before updating stores
3. **Reactive Architecture**: Use Svelte derived stores for automatic recomputation
4. **Event-Driven**: No rounds, no synchronization barriers - pure event flow
5. **ITC Causality**: Adaptive causality tracking with O(log n) space
6. **Slot-Native**: Allocate each availability slot independently (not aggregate capacity)
7. **Two-Tier Priority**: Mutual recognition gets priority, non-mutual gets leftovers
8. **Adaptive Damping**: Prevent oscillations with time-based history and damping factors
9. **Contractiveness**: Cap all allocations by recipient's actual need (ensures convergence)
10. **Continuous Monitoring**: Real-time convergence detection, not once-per-round
11. **Optimistic Matching**: Assume compatibility when slot info is missing (better UX)
12. **Algorithm-Driven Subscriptions**: Automatically subscribe based on subgroup membership

---

## Performance Comparison (v1 vs v2)

| Metric | v1 (Rounds + Vector Clocks) | v2 (Reactive + ITC) | Improvement |
|--------|----------------------------|---------------------|-------------|
| **Latency** | 0-90 seconds | ~100ms | **900x faster** |
| **Responsiveness** | Once per round | Every change | **Immediate** |
| **Convergence Detection** | 90-second intervals | Sub-second | **90x faster** |
| **Space Complexity** | O(all participants ever) | O(log active) | **10-1000x smaller** |
| **Participant Churn** | Unbounded growth | Natural adaptation | **Infinite improvement** |
| **Code Complexity** | 9 phases, timers, coordination | Event-driven, reactive | **~200 lines removed** |
| **Network Traffic** | Synchronized bursts | Natural spread | **Lower peak load** |
| **Scalability** | Limited by coordination | No coordination overhead | **Linear scaling** |

---

## Debugging Tools

Window-exposed functions (browser console):

```javascript
// Log subgroup state
window.debugAllocation();

// Manually compute allocation (for testing)
window.computeAllocation(providerPubKey, commitment, mrValues, weights, commitments);

// Test damping (time-based)
window.computeDampingFactor([
  { overAlloc: 10, timestamp: t1 },
  { overAlloc: 5, timestamp: t2 },
  { overAlloc: 8, timestamp: t3 }
]); // Returns 0.5 (oscillating)

// ITC operations (new)
window.itcEvent(stamp);        // Increment stamp
window.itcFork(stamp);         // Fork stamp
window.itcJoin(stamp1, stamp2); // Merge stamps
window.itcLeq(stamp1, stamp2);  // Check causality
```

---

## Migration Guide (v1 → v2)

### Phase 1: Add ITC Support (Dual-Track)

Keep vector clocks temporarily, add ITC:

```typescript
interface Commitment {
  vectorClock: VectorClock;  // Keep temporarily
  itcStamp: Stamp;           // Add new
  round?: number;            // Make optional
}
```

### Phase 2: Use ITC for Causality Checks

Switch to ITC for staleness detection:

```typescript
// Old: Vector clock comparison
if (compareVectorClocks(peerVC, myVC) <= 0) {
  return; // Stale
}

// New: ITC comparison
if (leq(peerStamp, myStamp)) {
  return; // Stale
}
```

### Phase 3: Make Allocations Reactive

Add reactive allocation store:

```typescript
// New reactive store (alongside manual computation)
export const myAllocationsReactive = derived(
  [myCommitmentStore, networkCommitments],
  ($deps) => computeAllocation(...$deps)
);
```

### Phase 4: Remove Vector Clocks & Rounds

Delete old fields and functions:

```typescript
interface Commitment {
  // vectorClock: VectorClock;  ← DELETE
  itcStamp: Stamp;
  // round?: number;            ← DELETE
}

// Delete functions:
// - All vector clock functions
// - All round coordination functions
```

### Phase 5: Update UI & Tests

- Update components to use ITC stamps
- Remove round number displays
- Update tests for reactive behavior
- Add ITC causality tests

---

**Design Philosophy**: Elegant recognition-based economics, validated data integrity, reactive state management, event-driven coordination, adaptive causality tracking, and slot-native allocation for real-world time/location constraints.

**v2 Improvements**: Dramatic simplification (~200 lines removed), 100-900x faster response, natural participant churn handling, and continuous real-time convergence monitoring.

