# Circles: Slot-Native Mutual-Priority Allocation System

A decentralized peer-to-peer resource allocation algorithm for the free-association project. This system enables fair distribution of resources based on mutual recognition and bilateral relationships, with precise time/location matching at the slot level.

## Overview

The Circles module implements a **Slot-Native Two-Tier Allocation Algorithm** that allocates resources between participants based on their recognition of each other. The system works at the slot level - each availability slot is allocated independently using the same recognition-based logic, enabling real-world constraints like time and location matching.

### Key Features

- **Slot-Native Allocation**: Works at slot level (time/location-specific)
- **Two-Tier Recognition**: Prioritizes mutual relationships over one-way recognition
- **Time/Location Matching**: Only allocates when schedules and locations are compatible
- **Adaptive Damping**: Prevents oscillations during convergence
- **Decentralized Coordination**: Vector clocks and gossip-based round advancement
- **Schema-Driven**: Zod validation for type-safe data exchange
- **P2P Synchronized**: Real-time data sharing via Holster
- **Convergence Guarantees**: Contractiveness through allocation capping
- **Full Transparency**: Slot-to-slot allocation records

## Architecture

### File Structure

```
circles/
├── schemas.ts                      # Zod schemas (slot-based commitments)
├── match.svelte.ts                 # Slot compatibility matching logic
├── store.svelte.ts                 # Generic Holster store utility
├── stores.svelte.ts                # Allocation-specific store instances
├── algorithm.svelte.ts             # Core slot-native allocation algorithm
├── CircleVisualization.svelte      # D3.js visualization component
├── index.ts                        # Module exports
├── circle.tsx                      # [Legacy] React prototype (superseded)
├── convergence.test.ts             # Algorithm convergence tests
├── ARCHITECTURE_REVIEW.md          # Architecture review documentation
├── TRANSITION_COMPLETE.md          # Transition notes
├── MIGRATION_FROM_REACT.md         # Migration guide from circle.tsx
└── README.md                       # This file
```

### Data Flow

```
User Recognition + Slot Declarations → Commitment Publishing 
  → Per-Slot Matching → Per-Slot Allocation → Network Sync
```

### Key Insight

**Each availability slot is a mini "capacity"** that gets allocated using the same two-tier recognition logic. Instead of allocating aggregate capacity, we allocate each slot's quantity independently, considering time/location compatibility.

## Core Concepts

### 1. Mutual Recognition (MR)

**Mutual Recognition** is the bilateral minimum of how two participants recognize each other:

```
MR(A, B) = min(A's recognition of B, B's recognition of A)
```

- **Symmetric**: MR is always equal from both perspectives
- **Zero-based**: If either party doesn't recognize the other, MR = 0
- **Self-recognition**: Participants can recognize themselves (self-care)

### 2. Two-Tier Allocation

**TIER 1: Mutual Recognition (Priority)**
- Allocates capacity to participants with MR > 0 first
- Uses **MRD (Mutual Recognition Distribution)**: `MR / TotalMutualRecognition`
- Gets first access to capacity

**TIER 2: Non-Mutual (Leftover)**
- Allocates remaining capacity to one-way recognition
- Uses renormalized shares: `Weight / TotalNonMutualRecognition`
- Only receives what Tier 1 doesn't consume

### 3. Adaptive Damping

Prevents oscillations during iterative convergence:

- **Oscillating** (up-down-up pattern): damping = 0.5
- **Smooth** (monotonic decrease): damping = 1.0  
- **Moderate** (otherwise): damping = 0.8

Formula: `ActiveNeed = ResidualNeed × DampingFactor`

### 4. Allocation Capping

All allocations are capped by recipient's actual residual need:

```typescript
cappedAllocation = min(rawAllocation, residual_need)
```

This ensures **contractiveness** (Banach Fixed-Point Theorem), guaranteeing convergence.

### 5. Vector Clocks

Provides causal consistency in decentralized coordination:

- Each participant maintains their own clock
- Clocks merge when receiving peer data
- Enables causally consistent snapshots

## Data Schemas

### Commitment (Slot-Native)

Published by each participant to declare their capacity and needs:

```typescript
{
  capacity_slots?: AvailabilitySlot[],  // What I can provide (if provider)
  need_slots?: NeedSlot[],              // What I need (if recipient)
  mr_values?: Record<string, number>,  // MR with all participants
  recognition_weights?: Record<string, number>,  // One-way recognition
  damping_factor?: number,      // Current damping (0.5-1.0)
  over_allocation_history?: number[],  // Last 3 over-allocations
  timestamp: number,
  vectorClock?: VectorClock,
  round?: number
}
```

### AvailabilitySlot / NeedSlot

Each slot has quantity + time/location constraints:

```typescript
{
  id: string,
  quantity: number,  // How much capacity/need
  
  // Time constraints
  start_date?: string,
  end_date?: string,
  start_time?: string,
  end_time?: string,
  time_zone?: string,
  recurrence?: string,
  
  // Location constraints
  city?: string,
  country?: string,
  latitude?: number,
  longitude?: number,
  location_type?: string,  // e.g., "remote", "in-person"
  online_link?: string,
  
  // ... more fields
}
```

### TwoTierAllocationState (Slot-Native)

Published by providers after computing slot-level allocations:

```typescript
{
  slot_denominators: Record<slotId, { mutual: number, nonMutual: number }>,
  slot_allocations: SlotAllocationRecord[],  // Detailed slot-to-slot records
  recipient_totals: Record<pubKey, number>,  // Aggregate view
  timestamp: number
}
```

### SlotAllocationRecord

Tracks which slot fulfills which need:

```typescript
{
  availability_slot_id: string,
  recipient_pubkey: string,
  recipient_need_slot_id?: string,
  quantity: number,
  time_compatible: boolean,
  location_compatible: boolean,
  tier: 'mutual' | 'non-mutual'
}
```

### RoundState

Published periodically for coordination:

```typescript
{
  pubKey: string,
  round: number,
  vectorClock: VectorClock,
  timestamp: number
}
```

## Usage

### Initialization

```typescript
import { 
  initializeAllocationStores,
  initializeAlgorithmSubscriptions 
} from '$lib/commons/circles';

// After Holster authentication
initializeAllocationStores();
initializeAlgorithmSubscriptions();
```

### Publishing Recognition

```typescript
import { publishMyRecognitionWeights } from '$lib/commons/circles/algorithm.svelte';

const weights = {
  'pubkey1': 0.4,  // 40% recognition
  'pubkey2': 0.3,  // 30% recognition
  'pubkey3': 0.3   // 30% recognition
};

await publishMyRecognitionWeights(weights);
```

### Publishing Commitment (Slot-Native)

```typescript
import { publishMyCommitment } from '$lib/commons/circles/algorithm.svelte';

const commitment = {
  capacity_slots: [
    {
      id: "mon-evening",
      quantity: 3,  // 3 hours
      start_date: "2024-06-10",
      start_time: "18:00",
      end_time: "21:00",
      city: "Berlin",
      country: "Germany"
    }
  ],
  need_slots: [
    {
      id: "childcare-morning",
      quantity: 4,  // 4 hours
      start_date: "2024-06-10",
      start_time: "08:00",
      end_time: "12:00",
      city: "Berlin",
      country: "Germany"
    }
  ],
  timestamp: Date.now()
};

await publishMyCommitment(commitment);
```

### Computing Slot-Native Allocations

```typescript
import { computeAndPublishAllocations } from '$lib/commons/circles/algorithm.svelte';

// Algorithm automatically uses computeSlotNativeAllocation()
// - Matches slots by time/location
// - Applies two-tier recognition logic per slot
// - Tracks which slots fulfill which needs
await computeAndPublishAllocations();
```

### Subscribing to Participants

The system automatically subscribes based on algorithm needs:

- **Mutual Partners**: Full data exchange (all stores)
- **Non-Mutual Beneficiaries**: Commitments only
- **Non-Mutual Providers**: Commitments + allocation states
- **All Active Participants**: Round states for coordination

Manual subscription is also available:

```typescript
import { subscribeToFullParticipant } from '$lib/commons/circles/stores.svelte';

subscribeToFullParticipant('pubkey-to-subscribe');
```

## Reactive Stores

### Derived Subgroups

```typescript
import {
  myMutualBeneficiaries,           // People I allocate to (mutual)
  myNonMutualBeneficiaries,        // People I allocate to (one-way)
  mutualProvidersForMe,            // People who allocate to me (mutual)
  nonMutualProvidersForMe,         // People who allocate to me (one-way)
  allMutualPartners,               // Union of mutual beneficiaries + providers
  activeParticipants,              // Fresh commitments (< 60s)
  oscillatingParticipants,         // Participants with damping < 1.0
  hasSystemConverged               // Convergence indicator
} from '$lib/commons/circles/algorithm.svelte';

```

### My Data Stores

```typescript
import {
  myCommitmentStore,
  myAllocationStateStore,
  myRecognitionWeightsStore,
  myRoundStateStore
} from '$lib/commons/circles/stores.svelte';

// Subscribe to changes
myCommitmentStore.subscribe(commitment => {
  console.log('My commitment updated:', commitment);
});
```

## Constants

```typescript
STALE_THRESHOLD_MS = 60000           // 60 seconds (freshness check)
CONVERGENCE_EPSILON = 0.001          // Convergence threshold
ROUND_GOSSIP_INTERVAL_MS = 5000      // 5 seconds (round state publishing)
ROUND_ADVANCEMENT_THRESHOLD = 0.5    // 50% (advance when >= 50% ahead)
DENOMINATOR_FLOOR = 0.0001           // Minimum denominator (prevent div/0)
```

## Algorithm Details

### Two-Tier Allocation Formula

**Tier 1 (Mutual):**
```
MRD(recipient) = MR(me, recipient) / TotalMutualRecognition
Numerator(recipient) = MRD(recipient) × ActiveNeed(recipient)
Allocation(recipient) = Capacity × Numerator(recipient) / ΣNumerators
```

**Tier 2 (Non-Mutual):**
```
Share(recipient) = Weight(recipient) / TotalNonMutualRecognition  
Numerator(recipient) = Share(recipient) × ActiveNeed(recipient)
Allocation(recipient) = RemainingCapacity × Numerator(recipient) / ΣNumerators
```

**Final Capping:**
```
FinalAllocation = min(Allocation, ResidualNeed)
```

### Convergence Properties

1. **Contractiveness**: Allocations are always capped by residual need
2. **Monotonicity**: Needs decrease (or stay constant) each round
3. **Lipschitz Continuity**: Denominator floor ensures bounded changes
4. **Fixed-Point**: System converges to equilibrium by Banach Fixed-Point Theorem

### Damping Algorithm

```typescript
function computeDampingFactor(history: number[]): number {
  if (history.length < 3) return 1.0;
  
  if (detectOscillation(history)) return 0.5;  // Slow down
  if (detectSmoothConvergence(history)) return 1.0;  // Full speed
  return 0.8;  // Moderate
}
```

Oscillation patterns:
- **Up-Down-Up**: `history[0] < history[1] && history[1] > history[2]`
- **Down-Up-Down**: `history[0] > history[1] && history[1] < history[2]`

## Round Coordination

### Vector Clock Operations

```typescript
// Increment my clock before publishing
await incrementMyVectorClock();

// Update from peer data
await updateVectorClockFromPeer(peerPubKey, peerVectorClock);

// Check causal consistency
const snapshot = getCausallyConsistentCommitments();
```

### Round Advancement

Automatically advances when **≥ 50%** of active participants are ahead:

```typescript
// Check periodically
if (shouldAdvanceRound()) {
  await advanceToNextRound();
}
```

Round state is gossiped every 5 seconds for coordination.

## Store Utilities

### Generic Store Creation

The `store.svelte.ts` file provides a generic factory for Holster-backed stores:

```typescript
import { createStore } from './store.svelte';

const myStore = createStore({
  holsterPath: 'my/data/path',
  schema: MyZodSchema,
  cacheable: true,           // Enable localStorage caching
  persistDebounce: 100       // Debounce writes (ms)
});

myStore.initialize();
await myStore.set(newData);
await myStore.cleanup();
```

### Features

- **Schema Validation**: Zod validation on all data
- **Timestamp Management**: Automatic conflict resolution
- **Caching**: localStorage for instant UI load
- **Queue Management**: Handles updates during persistence
- **Cross-User Subscriptions**: Subscribe to other participants

## Cleanup

```typescript
import { 
  cleanupAllocationStores,
  cleanupAlgorithmSubscriptions 
} from '$lib/commons/circles';

// Before logout or unmount
await cleanupAllocationStores();
cleanupAlgorithmSubscriptions();
```

## Debugging

Debug utilities are attached to `window` for browser console access:

```javascript
// Log current state
window.debugAllocation();

// Test allocation computation
window.computeTwoTierAllocation(providerPub, capacity, capacityId, mrValues, weights, commitments);

// Test damping
window.computeDampingFactor([50, 30, 40]);  // Returns 0.5 (oscillating)
window.updateCommitmentDamping(commitment, totalReceived);
```

## Mathematical Foundations

### Banach Fixed-Point Theorem

The algorithm guarantees convergence by satisfying:

1. **Complete Metric Space**: Residual needs bounded by [0, stated_need]
2. **Contraction Mapping**: `||T(x) - T(y)|| ≤ k||x - y||` where k < 1
   - Achieved through allocation capping and denominator floor
3. **Unique Fixed Point**: System converges to unique equilibrium

### Lipschitz Continuity

The allocation function is Lipschitz continuous:

```
|f(x₁) - f(x₂)| ≤ L|x₁ - x₂|
```

Guaranteed by:
- Denominator floor (prevents unbounded changes)
- Capping by residual need (bounds output)

## Security Considerations

- **Schema Validation**: All network data validated before processing
- **Timestamp Integrity**: Vector clocks prevent causality violations
- **Filter Safety**: Capacity filters use safe attribute matching (no eval)
- **Stale Data**: Automatic freshness checks (60s threshold)

## Performance

### Storage & Persistence
- **Debounced Persistence**: Reduces write frequency (100-200ms)
- **localStorage Caching**: Instant UI load
- **Incremental Updates**: Only changed data persisted
- **Selective Subscriptions**: Only subscribe to relevant participants

### Slot-Native Allocation Optimizations
- **Bucketing**: Time/location bucketing reduces compatibility checks by 10-100x
- **Pre-computed Compatibility Matrix**: Avoids redundant slot matching across tiers
- **Active Set Tracking**: Pre-filters recipients without recognition or compatible slots
- **Early Exit Conditions**: Skips unnecessary computation for exhausted slots
- **Capacity Utilization Tracking**: Monitors and logs allocation efficiency

See `SLOT_NATIVE_ARCHITECTURE.md` for detailed performance analysis.

## Visualization

### CircleVisualization Component

The `CircleVisualization.svelte` component provides a real-time D3.js-based visualization of the allocation system:

**Features:**
- **Live Denominators**: Circular visualizations showing capacity distribution
- **Two-Tier Color Coding**: Blue (mutual) vs amber (generous) for providers
- **Need Fulfillment**: Inner circles showing sources (green for mutual, lime for generous)
- **Mutual Recognition Links**: Dashed lines connecting mutually recognized participants
- **Breathing Animation**: Living system that pulses to show dynamic state
- **Interactive Tooltips**: Hover to see allocation details

**Usage:**

```svelte
<script>
	import { CircleVisualization } from '$lib/commons/circles';
	import { initializeAllocationStores } from '$lib/commons/circles';
	
	// Initialize stores after holster authentication
	initializeAllocationStores();
</script>

<CircleVisualization width={900} height={700} />
```

**Visual Elements:**
- **Outer pie slices** = Provider's capacity allocation (who they give to)
- **Inner pie slices** = Receiver's sources (who they receive from)
- **Circle size** = Total capacity or need magnitude
- **Line thickness** = Mutual recognition strength

**Integration:**
The component automatically:
- Subscribes to Holster stores for real-time updates
- Computes allocations using the slot-native algorithm
- Visualizes both aggregate capacity/need and slot-level allocations
- Shows fulfillment percentages for receivers

**Migration from circle.tsx:**
This Svelte component replaces the standalone React visualization (`circle.tsx`) with full integration into the circles infrastructure:
- Uses real schemas from `schemas.ts` (not mock data)
- Integrates with Holster stores for P2P sync
- Leverages the actual allocation algorithm from `algorithm.svelte.ts`
- Supports slot-based capacity/need (not just simple quantities)
- Shows real two-tier denominator computations

## Future Enhancements

- [ ] Attribute-based capacity filtering (location, skills)
- [ ] Multi-capacity allocation (different resource types)
- [ ] Historical allocation tracking
- [ ] Reputation system integration
- [ ] Capacity commitment scheduling

## License

Part of the free-association project.

