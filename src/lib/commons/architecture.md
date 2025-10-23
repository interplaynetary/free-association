# Commons Architecture - Design Guide

This guide outlines the architecture of the **Mutual-Priority Allocation Commons** - a peer-to-peer system for decentralized resource allocation using mutual recognition and slot-based capacity matching.

Built with **Holster** (P2P sync), **Svelte 5** (reactive state), **Zod v4** (schema validation), and **TypeScript**.

---

## Architecture Overview

The commons implements a **slot-native two-tier allocation algorithm** where:

1. **Participants declare** capacity slots (what they can provide) and need slots (what they need)
2. **Recognition weights** determine allocation priority (mutual recognition gets priority)
3. **Slot matching** ensures time/location compatibility before allocation
4. **Adaptive damping** prevents oscillations during iterative convergence
5. **Vector clocks** coordinate decentralized rounds across the network

---

## Store Architecture (Commons)
When designing such systems its helpful to create a table for me of columns sync/async with the rows being the round, and stating whose data is exactly needed (which subset) at that point.

### Raw Data Stores (Holster-Backed)

| Store | Type | Source | Updates | Holster Path |
|-------|------|--------|---------|--------------|
| `myCommitmentStore` | HolsterStore | My local state | When I change capacity_slots/need_slots/recognition | `allocation/commitment` |
| `myAllocationStateStore` | HolsterStore | My local computation | After computing slot allocations | `allocation/allocationState` |
| `myRecognitionWeightsStore` | HolsterStore | My local state | When I update recognition weights | `allocation/recognitionWeights` |
| `myRoundStateStore` | HolsterStore | My local state | Vector clock updates, round advancement | `allocation/roundState` |
| `networkCommitments` | Map<pubKey, Commitment> | Holster subscriptions | Real-time from subscribed participants | `~pubKey/allocation/commitment` |
| `networkAllocationStates` | Map<pubKey, AllocationState> | Holster subscriptions | Real-time from subscribed providers | `~pubKey/allocation/allocationState` |
| `networkRecognitionWeights` | Map<pubKey, Record> | Holster subscriptions | Real-time from subscribed participants | `~pubKey/allocation/recognitionWeights` |
| `networkRoundStates` | Map<pubKey, RoundState> | Holster subscriptions | Real-time from subscribed participants | `~pubKey/allocation/roundState` |

### Derived Subgroups (Algorithm-Driven)

| Subgroup | Derived From | Purpose | Data Needed | Subscription Type |
|----------|--------------|---------|-------------|-------------------|
| `myMutualRecognition` | `myRecognitionWeightsStore` + `networkRecognitionWeights` | Bilateral MR values → compute priority allocations | My weights + their weights | Full (for MR computation) |
| `myMutualBeneficiaries` | `myMutualRecognition` | People with MR > 0 → Tier 1 allocation | Their commitments (need_slots) | Commitment + Allocation + Recognition + Rounds |
| `myNonMutualBeneficiaries` | `myRecognitionWeightsStore` - `myMutualRecognition` | One-way recognition → Tier 2 allocation | Their commitments (need_slots) | Commitment only |
| `mutualProvidersForMe` | `myMutualRecognition` + `networkCommitments` | Providers with MR > 0 and capacity → priority allocation to me | Their commitments, allocations | Commitment + Allocation + Recognition + Rounds |
| `nonMutualProvidersForMe` | `networkCommitments` - `myMutualRecognition` | Providers who recognize me (one-way) → leftover allocation to me | Their commitments, allocations | Commitment + Allocation |
| `mutualBeneficiariesWithNeeds` | `myMutualBeneficiaries` + `networkCommitments` | Mutual partners with need_slots > 0 → contribute to Tier 1 denominators | Their need_slots, damping_factor | Commitment (filtered) |
| `nonMutualBeneficiariesWithNeeds` | `myNonMutualBeneficiaries` + `networkCommitments` | Non-mutual recipients with need_slots > 0 → contribute to Tier 2 denominators | Their need_slots, damping_factor | Commitment (filtered) |
| `allMutualPartners` | `myMutualBeneficiaries` ∪ `mutualProvidersForMe` | Bidirectional recognition → full data exchange | Everything | Full participant subscription |
| `activeParticipants` | `networkCommitments` + timestamp check | Fresh commitments (< 60s) → currently active | All commitment data | Commitment (freshness filtered) |
| `oscillatingParticipants` | `networkCommitments` + damping_factor check | Participants with damping < 1.0 → convergence issues | damping_factor, over_allocation_history | Commitment (damping filtered) |
| `hasSystemConverged` | `myAllocationStateStore` + previous denominators | Denominators stable (Δ < ε) → equilibrium reached | slot_denominators (current + previous) | N/A (local computation) |

---

## Continuous Async Operations (Decentralized Coordination)

| Operation | Sync/Async | Data Source | Trigger | Purpose | Debounce |
|-----------|------------|-------------|---------|---------|----------|
| **Commitment Publishing** | Async | `myCommitmentStore` | User changes capacity/need slots or recognition | Broadcast my state to network | 100ms |
| **Recognition Weights Publishing** | Async | `myRecognitionWeightsStore` | User updates recognition weights | Update MR values with others | 200ms |
| **Algorithm-Driven Subscriptions** | Async | Derived subgroups | `allMutualPartners`, `myNonMutualBeneficiaries` change | Maintain data connections based on recognition | 100ms |
| **MR Computation** | Async (derived) | `myRecognitionWeightsStore` + `networkRecognitionWeights` | When either my weights or their weights change | Compute min(myRec, theirRec) for all participants | Immediate (derived) |
| **Slot Matching** | Async (derived) | `networkCommitments` (need_slots + capacity_slots) | When slots change | Find compatible recipients for each availability slot | Per computation |
| **Allocation Computation** | Async | `myCommitmentStore` + `networkCommitments` + `myMutualRecognition` | When commitments or MR values change | Compute two-tier slot-native allocations | On-demand |
| **Allocation Publishing** | Async | `myAllocationStateStore` | After allocation computation completes | Broadcast allocations to recipients | 100ms |
| **Convergence Detection** | Async (derived) | `myAllocationStateStore` + previous denominators | After allocation update | Check if slot denominators stable (Δ < ε) | Immediate (derived) |
| **Vector Clock Gossip** | Async (periodic) | `myRoundStateStore` | Every 5 seconds | Coordinate rounds via gossip (no coordinator) | 5000ms (periodic) |
| **Round Advancement Check** | Async | `networkRoundStates` + `activeParticipants` | When peer round state received | Advance if ≥50% of peers ahead | Per peer update |

---

## Round-Synchronized Operations (Vector Clock Coordination)

**Note**: Commons uses **decentralized vector clock coordination** instead of centralized round announcements. Each participant advances independently when ≥50% of active peers are ahead.

### Phase Timing Breakdown

| Phase | Duration | Sync Type | Critical Data | Purpose | Coordination |
|-------|----------|-----------|---------------|---------|--------------|
| **0. Vector Clock Increment** | Instant | Local | My current vector clock | Increment my counter before state change | Local |
| **1. Commitment Update** | 0-5s | Async | capacity_slots, need_slots, recognition_weights, vectorClock | Publish my updated state with causal metadata | Holster P2P |
| **2. Peer Clock Merge** | 5-10s | Async | Peers' vectorClock values | Merge received clocks, update my clock | Automatic (on receive) |
| **3. Slot Compatibility Matrix** | 10-20s | **SYNC (Local)** | All active participants' capacity_slots + need_slots | Pre-compute which slots can fulfill which needs | Bucketing optimization |
| **4. Two-Tier Allocation Computation** | 20-40s | **SYNC (Local)** | MR values, recognition weights, compatible slots, damping factors | For each slot: Tier 1 (mutual), Tier 2 (non-mutual) | Per-slot computation |
| **5. Allocation Publishing** | 40-45s | Async | slot_denominators, slot_allocations, recipient_totals, vectorClock | Broadcast my allocations to recipients | Holster P2P |
| **6. Aggregation Window** | 45-75s | Async with timeout | All providers' slot_allocations where recipient = me | Collect total received across all providers | Subscribe to provider allocations |
| **7. Damping Update** | 75-80s | **SYNC (Local)** | stated_need + total_received → over_allocation | Update over_allocation_history, recompute damping_factor | Local computation |
| **8. Round Advancement Check** | 80-85s | Async | Peers' round numbers from roundState | If ≥50% ahead, advance my round and increment vector clock | Gossip-based consensus |
| **9. Convergence Check** | 85-90s | Async (derived) | My slot_denominators (current + previous) | Check if max(Δ) < ε across all slots | Local + Network sampling |

### Vector Clock Causality Rules

| Rule | Scenario | Action | Data Required |
|------|----------|--------|---------------|
| **Happened-Before** | peer.VC[peer] > my.VC[peer] | Accept update (causally later) | Peer's vector clock |
| **Concurrent** | Neither VC dominates | Accept both (merge clocks) | Both vector clocks |
| **Stale Update** | peer.VC[peer] ≤ my.VC[peer] | Ignore (already processed) | Peer's vector clock |
| **Clock Merge** | Receive peer's VC | my.VC = max(my.VC, peer.VC) for all entries | Full vector clock from peer |
| **Self Increment** | Before publishing state | my.VC[me]++ | Own counter |

---

## Data Subset Requirements Per Round Phase

### Provider's Perspective (Computing Allocations)

| Phase | Data Subset | Schema | Freshness | Source |
|-------|-------------|--------|-----------|--------|
| **Slot Matching** | All participants' need_slots + capacity_slots | `AvailabilitySlotSchema`, `NeedSlotSchema` | < 60s | `networkCommitments` (filtered by freshness) |
| **MR Computation** | Mutual partners' recognition_weights | `Record<string, number>` | < 60s | `myRecognitionWeightsStore` + `networkRecognitionWeights` |
| **Tier 1 Numerators** | Mutual recipients' need_slots, damping_factor | `CommitmentSchema` | < 60s | `mutualBeneficiariesWithNeeds` |
| **Tier 2 Numerators** | Non-mutual recipients' need_slots, damping_factor | `CommitmentSchema` | < 60s | `nonMutualBeneficiariesWithNeeds` |
| **Capping Check** | Each recipient's total need_slots.quantity | `NeedSlotSchema[]` | < 60s | `networkCommitments` |

### Recipient's Perspective (Receiving Allocations)

| Phase | Data Subset | Schema | Freshness | Source |
|-------|-------------|--------|-----------|--------|
| **Expected Allocation** | Providers' slot_denominators + my position in them | `TwoTierAllocationStateSchema` | < 60s | `networkAllocationStates` |
| **Total Received** | All providers' slot_allocations where recipient = me | `SlotAllocationRecordSchema[]` | < 60s | `networkAllocationStates` (aggregated) |
| **Damping Update** | My stated_need (sum of need_slots) + total_received | `CommitmentSchema` | Current | `myCommitmentStore` + aggregated allocations |
| **Convergence Check** | My slot_denominators (current round + previous round) | `TwoTierAllocationStateSchema` | Current + Previous | `myAllocationStateStore` + tracked history |

### Coordination Perspective (Round Advancement)

| Phase | Data Subset | Schema | Freshness | Source |
|-------|-------------|--------|-----------|--------|
| **Vector Clock Merge** | All active participants' vectorClock | `VectorClockSchema` | < 60s | `networkRoundStates` |
| **Round Consensus** | All active participants' round number | `RoundStateSchema` | < 60s | `networkRoundStates` |
| **Advancement Decision** | Count of peers with round > my_round | `number` | < 60s | Derived from `networkRoundStates` |

---

## Layer 1: Schemas (Data Validation)

**File**: `schemas.ts`

All data structures are defined using **Zod schemas** for runtime validation and TypeScript type inference. This ensures data integrity when receiving data from the network.

### Core Schemas:

- **`AvailabilitySlotSchema`** - A specific time/location/quantity of available capacity
- **`NeedSlotSchema`** - A specific time/location/quantity of needed capacity
- **`CommitmentSchema`** - A participant's declaration (capacity_slots, need_slots, recognition_weights, damping state)
- **`TwoTierAllocationStateSchema`** - Provider's computed allocations (slot denominators, slot allocations, recipient totals)
- **`RoundStateSchema`** - Decentralized round coordination (round number, vector clock)
- **`VectorClockSchema`** - Causal consistency tracking (maps pubKey → logical timestamp)

### Design Principle:

Schemas are the **single source of truth** for types. All TypeScript types are inferred from Zod schemas using `z.infer<>`.

```typescript
export type Commitment = z.infer<typeof CommitmentSchema>;
export type TwoTierAllocationState = z.infer<typeof TwoTierAllocationStateSchema>;
```

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

### API:

```typescript
const myStore = createStore({
  holsterPath: 'allocation/commitment',
  schema: CommitmentSchema,
  cacheable: true,
  persistDebounce: 100
});

// Initialize (subscribes to network)
myStore.initialize();

// Update local data (triggers persistence)
myStore.set(newCommitment);

// Subscribe to changes
myStore.subscribe(data => console.log(data));

// Subscribe to another user
myStore.subscribeToUser(theirPubKey, data => console.log(data));
```

---

## Layer 3: Allocation Stores (Typed Persistence)

**File**: `stores.svelte.ts`

Uses the generic store factory to create **typed stores** for allocation data.

### My Data Stores (Published to Network):

- **`myCommitmentStore`** - My capacity_slots, need_slots, recognition_weights, damping state
- **`myAllocationStateStore`** - My computed allocations (if I'm a provider)
- **`myRecognitionWeightsStore`** - My recognition of others (% weights)
- **`myRoundStateStore`** - My current round state (for coordination)

### Network Data Maps (Subscribed from Network):

- **`networkCommitments`** - Map<pubKey, Commitment> from all participants
- **`networkAllocationStates`** - Map<pubKey, AllocationState> from providers
- **`networkRecognitionWeights`** - Map<pubKey, weights> for MR computation
- **`networkRoundStates`** - Map<pubKey, RoundState> for round coordination

### Subscription Management:

Provides functions to subscribe to specific participants:

- `subscribeToCommitment(pubKey)` - Get their capacity/needs
- `subscribeToAllocationState(pubKey)` - Get their allocations
- `subscribeToRecognitionWeights(pubKey)` - Get their recognition weights
- `subscribeToRoundState(pubKey)` - Get their round state
- `subscribeToFullParticipant(pubKey)` - Subscribe to all data from a mutual partner

---

## Layer 4: Algorithm (Allocation Logic)

**File**: `algorithm.svelte.ts`

Implements the **mutual-priority allocation algorithm** with slot-native processing.

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

### Algorithm-Driven Subscriptions:

The algorithm **automatically manages subscriptions** based on subgroups:

- Mutual partners → Subscribe to full data (commitment, allocation, recognition, rounds)
- Non-mutual beneficiaries → Subscribe to commitments only (to see their needs)
- Non-mutual providers → Subscribe to commitments + allocations (to compute expected allocation)
- Active participants → Subscribe to round states (for coordination)

**Initialization**:

```typescript
import { initializeAlgorithmSubscriptions } from './algorithm.svelte';

// After Holster authentication + store initialization
initializeAlgorithmSubscriptions();
```

### Core Algorithm Functions:

**`computeAllocation()`** - Slot-native two-tier allocation:

1. For each availability slot:
   - Find compatible recipients (time/location matching)
   - **Tier 1**: Allocate to mutual recipients (MR-weighted, priority)
   - **Tier 2**: Allocate remaining to non-mutual recipients (one-way weighted)
   - Cap all allocations by recipient's actual need (contractiveness)
2. Return slot-level denominators, allocations, and recipient totals

**Performance Optimizations**:
- Bucketing (reduce compatibility checks from O(N×A) to O(N×A_bucket))
- Pre-computed compatibility matrix (avoid redundant checks)
- Active set tracking (filter out recipients without recognition early)
- Early exit conditions (skip slots with no compatible/recognized recipients)

**`computeMutualRecognition()`** - Bilateral MR calculation:

```
MR(Me, Them) = min(MyRecognitionOf[Them], TheirRecognitionOf[Me])
```

**`updateCommitmentDamping()`** - Adaptive damping:

- Tracks over-allocation history (last 3 rounds)
- Detects oscillation pattern (up-down-up or down-up-down)
- Adjusts damping factor: 0.5 (oscillating), 0.8 (moderate), 1.0 (smooth)
- `ActiveNeed = ResidualNeed × DampingFactor`

**`publishMyCommitment()`** - Enhanced publishing:

- Increments vector clock (causal ordering)
- Adds MR values, recognition weights
- Persists to Holster (auto-validated by store)

### Vector Clock Coordination:

- **`incrementMyVectorClock()`** - Call before publishing state changes
- **`updateVectorClockFromPeer()`** - Merge peer's clock with mine
- **`advanceToNextRound()`** - Move to next round when ≥50% of participants are ahead
- **`publishMyRoundState()`** - Periodic gossip for coordination (every 5s)

---

## Layer 5: Slot Matching (Compatibility Logic)

**File**: `match.svelte.ts`

Pure functions for checking slot compatibility.

### Compatibility Checks:

**`timeRangesOverlap(slot1, slot2)`** - Returns true if time ranges overlap
- Handles date-only and datetime formats
- Optimistic: returns true if time info missing

**`locationsCompatible(slot1, slot2)`** - Returns true if locations match
- Checks: city, country, coordinates (within 50km), online/remote
- Optimistic: returns true if location info missing

**`slotsCompatible(needSlot, availSlot)`** - Combined check
- Must pass BOTH time AND location compatibility

### Slot Matching:

**`matchNeedToCapacitySlots(need, capacity, maxAmount)`** - Match algorithm

Returns:
- `compatible_pairs` - Array of (needSlot, availSlot, matchableQty)
- `total_matchable` - Total quantity that can be allocated
- `unmatched_need_slots` - Need slots with no compatible availability
- `unmatched_availability_slots` - Availability slots with no compatible needs

---

## Layer 6: Visualization (UI Component)

**File**: `Visualization.svelte`

Svelte component for visualizing allocation state, subgroups, and network health.

---

## Reactive Data Flow

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
│    - Persist to Holster (debounced)                         │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 3. NETWORK SYNC (Holster P2P)                              │
│    - Broadcast to peers                                     │
│    - Receive updates from peers                             │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 4. NETWORK STORES (networkCommitments, networkWeights)     │
│    - Validate with Zod                                      │
│    - Update Map<pubKey, data>                               │
│    - Trigger derived store recomputation                    │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 5. DERIVED STORES (myMutualRecognition, subgroups)         │
│    - Reactively compute from base stores                    │
│    - Auto-update UI when dependencies change                │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 6. ALGORITHM (computeAllocation)                            │
│    - Triggered by commitment changes                        │
│    - Compute slot-native two-tier allocations               │
│    - Apply adaptive damping                                 │
│    - Cap by recipient needs (contractiveness)               │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 7. PUBLISH ALLOCATION STATE                                 │
│    - Update myAllocationStateStore                          │
│    - Persist to Holster                                     │
│    - Broadcast to network                                   │
└─────────────────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 8. RECIPIENTS RECEIVE ALLOCATIONS                           │
│    - Subscribe to provider's allocationState                │
│    - See expected allocation (slot-level)                   │
│    - Update own needs based on received amounts             │
└─────────────────────────────────────────────────────────────┘
```

---

## Implementation Checklist

### Step 1: Define Schemas
✓ Create Zod schemas for all data types
✓ Export inferred TypeScript types
✓ Add validation helpers (`parseCommitment`, etc.)

### Step 2: Set Up Stores
✓ Initialize my data stores (commitment, allocation, recognition, round)
✓ Initialize network data maps (commitments, allocations, weights, rounds)
✓ Call `initializeAllocationStores()` after Holster authentication

### Step 3: Set Up Subscriptions
✓ Call `initializeAlgorithmSubscriptions()` to enable reactive subscription management
✓ Algorithm automatically subscribes based on subgroups (mutual partners, providers, etc.)

### Step 4: Implement UI
✓ Subscribe to derived stores in components (e.g., `$myMutualBeneficiaries`)
✓ Update local stores on user actions (e.g., `myCommitmentStore.set(...)`)
✓ Display allocation results from `myAllocationStateStore` and `networkAllocationStates`

### Step 5: Handle Lifecycle
✓ Initialize on authentication: `initializeAllocationStores()` → `initializeAlgorithmSubscriptions()`
✓ Cleanup on logout: `cleanupAllocationStores()` → `cleanupAlgorithmSubscriptions()`

---

## Key Design Principles

1. **Schema-First**: All types inferred from Zod schemas (single source of truth)
2. **Validation at Boundaries**: Validate all network data before updating stores
3. **Reactive Architecture**: Use Svelte derived stores for automatic recomputation
4. **Slot-Native**: Allocate each availability slot independently (not aggregate capacity)
5. **Two-Tier Priority**: Mutual recognition gets priority, non-mutual gets leftovers
6. **Adaptive Damping**: Prevent oscillations with history-based damping factors
7. **Contractiveness**: Cap all allocations by recipient's actual need (ensures convergence)
8. **Causal Consistency**: Use vector clocks for decentralized round coordination
9. **Optimistic Matching**: Assume compatibility when slot info is missing (better UX)
10. **Algorithm-Driven Subscriptions**: Automatically subscribe based on subgroup membership

---

## Debugging Tools

Window-exposed functions (browser console):

```javascript
// Log subgroup state
window.debugAllocation();

// Manually compute allocation
window.computeAllocation(providerPubKey, commitment, mrValues, weights, commitments);

// Test damping
window.computeDampingFactor([10, 5, 8]); // Returns 0.5 (oscillating)
window.updateCommitmentDamping(commitment, totalReceived);
```

---

**Design Philosophy**: Elegant recognition-based economics, validated data integrity, reactive state management, and slot-native allocation for real-world time/location constraints.
