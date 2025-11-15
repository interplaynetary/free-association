# Allocation Architecture Comparison

**Date:** 2025-11-09  
**Files Analyzed:**
- `src/lib/protocol/allocation.ts` (Multi-Provider Free Allocation)
- `src/lib/protocol/collective/collective-recognition.ts` (Collective Recognition Allocation)

## Executive Summary

The two allocation modules implement **fundamentally different allocation paradigms** that serve distinct use cases:

1. **`collective-recognition.ts`**: Single-provider collective capacity allocation
2. **`allocation.ts`**: Multi-provider distributed individual capacity allocation

They are **NOT conformant** to each other's architecture, but this is by design. They solve different problems.

---

## Architectural Comparison

### 1. Provider Model

#### `collective-recognition.ts` (Single-Provider)
```typescript
export function computeAllocations(
	capacity: BaseCapacity,  // ONE capacity declaration
	needs: Map<string, BaseNeed>,
	memberTrees: Map<string, Node>,
	recognitionData?: RecognitionData[]
): AllocationComputationResult
```

**Characteristics:**
- One capacity declaration per computation
- Capacity can be from a single provider OR pooled from multiple contributors (but treated as one collective pot)
- Allocation decision is **collective** - based on collective recognition shares
- Member set defined by the provider (or updated via MRD)
- Philosophy: "What should the collective decide to give to each member?"

#### `allocation.ts` (Multi-Provider)
```typescript
export function computeAllocations(
	myPubKey: string,
	myCapacitySlots: AvailabilitySlot[],  // MY capacity
	myRecognition: GlobalRecognitionWeights,  // MY recognition
	mutualRecognition: Record<string, number>,
	allCommitments: Record<string, Commitment>,  // Network state
	currentState: SystemStateSnapshot,
	previousState: SystemStateSnapshot | null,
	needsIndex?: SpaceTimeIndex
): AllocationResult
```

**Characteristics:**
- Each provider independently runs their own allocation
- Each provider has their own capacity slots
- Each provider has their own recognition graph
- Allocation decision is **individual** - based on provider's own perspective
- Philosophy: "What do I want to give to others based on my recognition?"

**Divergence:** ❌ **FUNDAMENTAL INCOMPATIBILITY**  
The collective recognition module allocates ONE capacity to many recipients. The allocation module is designed for MANY providers each independently allocating their own capacity. These are different allocation models.

---

### 2. Recognition Model

#### `collective-recognition.ts` (Collective Recognition Shares)
```typescript
export function calculateCollectiveRecognitionShares(
	memberSet: string[],
	memberTrees: Map<string, Node>
): {
	shares: Map<string, number>;
	memberRecognitionSums: Map<string, number>;
	totalPool: number;
	mutualRecognitionMatrix: Map<string, Map<string, number>>;
}
```

**Formula:**
```
Pool = Σ MutualRecognition(i, j) for all pairs in member set
Member's Share = (Σ MutualRecognition(Member, Others)) / Pool
```

**Characteristics:**
- Symmetric recognition within a member set
- Recognition is calculated collectively (everyone's mutual recognition with each other)
- Results in a normalized share (0-1) for each member
- Member set is fixed for the computation
- Recognition is **relative** - your share depends on others' mutual recognition

#### `allocation.ts` (Individual Recognition Graph)
```typescript
export function computeMutualRecognition(
	myRecognition: GlobalRecognitionWeights,  // MY view of others
	othersRecognition: Record<string, GlobalRecognitionWeights>,  // Others' views of me
	myPubKey: string
): Record<string, number>
```

**Formula:**
```
MR(me, other) = min(myRec[other], otherRec[me])
```

**Characteristics:**
- Asymmetric recognition from provider's perspective
- Each provider has their own recognition weights (my view of the network)
- Mutual recognition calculated pairwise
- Recognition is **absolute** - not normalized across a fixed set
- No requirement that recognition sums to 100%

**Divergence:** ❌ **DIFFERENT RECOGNITION SEMANTICS**  
Collective recognition uses symmetric, normalized shares within a closed set. Individual allocation uses asymmetric, non-normalized weights across an open network. These serve different purposes: collective decision-making vs individual preference expression.

---

### 3. Allocation Strategy

#### `collective-recognition.ts` (Proportional to Recognition Shares)
```typescript
// Single-tier: Everyone in member set gets proportional allocation
const memberTargets = new Map<string, number>();
for (const [recipientId, share] of recognitionShares.entries()) {
	const target = share * totalCapacity;  // Proportional to share
	memberTargets.set(recipientId, target);
}
```

**Strategy:**
- Single-tier allocation
- Everyone in the member set gets allocated based on their collective recognition share
- No distinction between mutual/non-mutual
- Target allocation = share × total capacity
- Filters can override (block or cap) but baseline is proportional

#### `allocation.ts` (Two-Tier: Mutual + Non-Mutual)
```typescript
// TIER 1: Mutual Recognition (priority allocation)
for (const [recipientPub, needSlots] of compatibleRecipients.entries()) {
	const mutualRec = mutualRecognition[recipientPub] || 0;
	if (mutualRec > 0) {
		// Allocate based on mutual recognition
	}
}

// TIER 2: Non-Mutual Recognition (use remaining capacity)
for (const [recipientPub, needSlots] of compatibleRecipients.entries()) {
	const mutualRec = mutualRecognition[recipientPub] || 0;
	if (mutualRec <= 0) {
		const myRecOfThem = myRecognition[recipientPub] || 0;
		// Allocate based on my one-way recognition
	}
}
```

**Strategy:**
- Two-tier allocation
- Tier 1 (mutual): Allocate to people who recognize me back
- Tier 2 (non-mutual): Allocate remaining capacity to people I recognize (even if one-way)
- Recognizes the difference between mutual and one-way relationships
- Self-care is supported (I can allocate to myself)

**Divergence:** ❌ **DIFFERENT ALLOCATION PRIORITIES**  
Collective recognition treats all members equally (proportional to shares). Individual allocation prioritizes mutual relationships over one-way relationships. This reflects different social models: collective equity vs reciprocity-based allocation.

---

### 4. Slot Matching & Compatibility

#### `collective-recognition.ts` (Slot-Based Matching)
```typescript
export function matchNeedToCapacitySlots(
	need: BaseNeed,
	capacity: BaseCapacity,
	maxAmount: number
): {
	compatible_pairs: Array<{
		need_slot: NeedSlot;
		availability_slot: AvailabilitySlot;
		matchable_quantity: number;
	}>;
	total_matchable: number;
}
```

**Features:**
- Slot-to-slot matching (need slot → availability slot)
- Time/location compatibility checked via `slotsCompatible()`
- Tracks which specific slots are matched
- Multi-pass allocation with active recipient/slot tracking
- Spatial/temporal bucketing for O(k) lookups (month-level time, city-level location)

#### `allocation.ts` (Slot-Based Matching)
```typescript
function findCompatibleRecipients(
	capacitySlot: AvailabilitySlot,
	allCommitments: Record<string, Commitment>,
	myPubKey: string,
	needsIndex?: SpaceTimeIndex
): Map<string, NeedSlot[]>
```

**Features:**
- Slot-to-slot matching (capacity slot → need slots)
- Time/location compatibility checked via `slotsCompatible()`
- Spatial/temporal indexing for O(k) lookups (via SpaceTimeIndex)
- Multi-pass proportional allocation with redistribution
- Divisibility constraints (natural units, minimum percentage)
- Remainder redistribution using Largest Remainder Method

**Conformance:** ✅ **ALIGNED**  
Both use slot-based matching with time/location compatibility. Both use spatial/temporal optimization strategies. Both support multi-pass allocation. This is a **strong point of alignment**.

---

### 5. Multi-Pass Allocation Logic

#### `collective-recognition.ts` (Multi-Pass Until Targets Met)
```typescript
let activeRecipients = new Set<string>();
let passCount = 0;
const maxPasses = 10;

while (hasUnallocatedCapacity && passCount < maxPasses && 
       activeRecipients.size > 0 && totalRemainingCapacity > 0) {
	passCount++;
	
	for (const recipientId of activeRecipients) {
		const target = memberTargets.get(recipientId)!;
		const currentTotal = memberTotals.get(recipientId) || 0;
		
		if (currentTotal >= target) {
			activeRecipients.delete(recipientId);  // Satisfied
			continue;
		}
		
		// Allocate up to remaining target
		const remainingTarget = target - currentTotal;
		// ... allocate slots ...
	}
}
```

**Strategy:**
- Each recipient has a target allocation (based on recognition share)
- Continue allocating until:
  - All recipients reach their target, OR
  - No more compatible slots, OR
  - Max passes reached
- Remove satisfied recipients from active set
- Unused capacity can result if no compatible slots or needs are met

#### `allocation.ts` (Multi-Pass Proportional with Recognition-Based Denominator)
```typescript
let unsatisfiedRecipients = [...mutualEligibleRecipients];
let remainingCapacity = providersAvailableCapacity;
let passCount = 0;

while (remainingCapacity > EPSILON && unsatisfiedRecipients.length > 0 && passCount < maxPasses) {
	passCount++;
	
	// Calculate denominator with only unsatisfied recipients
	let denominator = unsatisfiedRecipients.reduce(
		(sum, r) => sum + r.mutualRecShare, 0
	);
	
	// Calculate ALL proportional allocations BEFORE capping
	const proportionalAllocations = unsatisfiedRecipients.map(recipient => {
		const rawAllocation = remainingCapacity * recipient.mutualRecShare / denominator;
		return {
			recipient,
			rawAllocation,
			cappedAllocation: Math.min(rawAllocation, recipient.remainingNeed)
		};
	});
	
	// Apply allocations, track satisfaction, redistribute
	// ...
	
	// Remove satisfied recipients for next pass
	unsatisfiedRecipients = unsatisfiedRecipients.filter(r => !nowSatisfied.includes(r));
}
```

**Strategy:**
- Each recipient gets proportional allocation based on recognition share
- Key difference: **denominator is recalculated each pass** with only unsatisfied recipients
- This ensures automatic redistribution of capacity from satisfied recipients
- Continue until:
  - All recipients satisfied (need met), OR
  - Capacity exhausted, OR
  - No progress made (stuck), OR
  - Max passes reached
- Remainder redistribution via Largest Remainder Method

**Conformance:** ✅ **SIMILAR BUT DIFFERENT**  
Both use multi-pass allocation. Both track satisfied recipients. Both have early exit conditions. However, `allocation.ts` uses dynamic denominators for true proportional redistribution, while `collective-recognition.ts` uses fixed targets. Both are valid approaches, but serve different needs.

---

### 6. Divisibility Constraints

#### `collective-recognition.ts`
```typescript
// No explicit divisibility constraint handling
// Allocations are continuous (can allocate fractional amounts)
// Assumes all capacity is perfectly divisible
```

**Approach:**
- No `max_natural_div` field
- No minimum allocation percentage
- Allocations can be fractional
- No remainder redistribution
- Assumes capacity is infinitely divisible (or handled externally)

#### `allocation.ts`
```typescript
export function applyDivisibilityConstraints(
	rawQuantity: number,
	sharePercentage: number,
	capacitySlot: AvailabilitySlot
): number {
	const maxNatural = capacitySlot.max_natural_div || 1;
	const minPercent = capacitySlot.min_allocation_percentage || 0.0;
	
	// 1. Check minimum percentage threshold
	if (minPercent > EPSILON && sharePercentage < minPercent - EPSILON) {
		return 0;  // Reject: too fragmented
	}
	
	// 2. Round to natural units (e.g., whole rooms)
	const naturalConstrained = Math.floor(rawQuantity / maxNatural) * maxNatural;
	
	return naturalConstrained;
}
```

**Approach:**
- Explicit `max_natural_div` field (e.g., can't divide a room into quarters)
- Minimum allocation percentage to prevent over-fragmentation
- Remainder redistribution using Largest Remainder Method
- Ensures practical allocations (whole units, meaningful percentages)

**Divergence:** ❌ **NOT IMPLEMENTED IN COLLECTIVE RECOGNITION**  
The collective recognition module does not implement divisibility constraints. This is a **missing feature** that would be needed for real-world capacity like housing, equipment, or vehicles. However, it could be added without breaking the core architecture.

---

### 7. Convergence & System Dynamics

#### `collective-recognition.ts`
```typescript
// No convergence tracking
// Single-shot allocation (no iterations)
// No system state updates
// Each capacity allocation is independent
```

**Model:**
- Static allocation
- No concept of "previous state" or "current iteration"
- Each `computeAllocations()` call is a one-time computation
- No tracking of need reduction over time
- No damping factors for over-allocation

#### `allocation.ts`
```typescript
export function computeConvergenceSummary(
	currentState: SystemStateSnapshot,
	previousState: SystemStateSnapshot | null,
	iterationStartTime: number
): ConvergenceSummary {
	const currentMagnitude = computeTotalNeedMagnitude(currentState);
	const contractionRate = computeContractionRate(currentMagnitude, previousMagnitude);
	const percentNeedReduction = computePercentNeedReduction(currentMagnitude, previousMagnitude);
	// ... more metrics ...
}
```

**Model:**
- Dynamic allocation with iterative convergence
- Tracks system state across iterations
- Monitors convergence metrics:
  - Total need magnitude (Frobenius norm)
  - Contraction rate (how fast needs shrink)
  - Percent need reduction (progress per iteration)
  - Iterations to convergence (estimated)
- Need update law: `N_next = N_current - Received`
- Damping factors for over-allocation correction

**Divergence:** ❌ **FUNDAMENTALLY DIFFERENT MODELS**  
Collective recognition is a **static allocation model** (one-time distribution of a capacity pot). Individual allocation is a **dynamic convergence model** (iterative reduction of needs across multiple allocations from multiple providers). These serve different purposes and cannot be directly compared.

---

### 8. Compliance Filters

#### `collective-recognition.ts`
```typescript
export type ComplianceFilter = 
	| { type: 'blocked'; value: 0 }           // $0 - No allocation
	| { type: 'capped'; value: number }       // $X - Maximum allocation
	| { type: 'unlimited' }                   // Unlimited allocation

export function unionOfFilters(
	filter1: ComplianceFilter, 
	filter2: ComplianceFilter
): ComplianceFilter {
	const val1 = getFilterValue(filter1);
	const val2 = getFilterValue(filter2);
	return createFilter(Math.min(val1, val2));  // Most restrictive wins
}
```

**Features:**
- Three filter types (blocked, capped, unlimited)
- Applied per member in the capacity declaration
- Filter union for proxy scenarios (e.g., external provider using entity as proxy)
- Filters override recognition shares

#### `allocation.ts`
```typescript
// No explicit filter system
// Allocation is purely recognition-based
// Hard constraints come from:
//   - Slot compatibility (time/location)
//   - Divisibility constraints
//   - Need caps (can't receive more than you need)
```

**Features:**
- No compliance filter system
- Recognition is the only soft constraint
- Need is a hard cap (can't over-allocate)
- Divisibility provides practical constraints

**Divergence:** ❌ **FILTERS NOT IMPLEMENTED IN ALLOCATION.TS**  
The collective recognition module has an explicit compliance filter system for regulatory/legal/policy constraints. The individual allocation module does not. This is a **feature gap** that could be beneficial to add to `allocation.ts` for real-world deployment (e.g., jurisdictional limits, provider risk tolerance).

---

### 9. Multi-Provider Scenarios

#### `collective-recognition.ts`
```typescript
// Section: MULTI-PROVIDER SCENARIOS

export function applyFilterUnion<T>(
	providerCapacity: T,
	entityFilters: Map<string, ComplianceFilter>
): T {
	// Union of filters for proxy scenarios
	// Example: External provider ($50K max) using entity as proxy ($30K max)
	// Effective filter: min($50K, $30K) = $30K
}
```

**Multi-Provider Support:**
- Has a section labeled "MULTI-PROVIDER SCENARIOS"
- But it only handles **filter unions** for proxy cases
- Does NOT handle multiple providers simultaneously allocating
- Each capacity is still allocated independently (one at a time)
- No coordination or aggregation across providers

#### `allocation.ts`
```typescript
// Designed for true multi-provider allocation

// Each provider runs computeAllocations() independently:
const aliceAllocations = computeAllocations(
	'alice', aliceCapacity, aliceRecognition, ...
);
const bobAllocations = computeAllocations(
	'bob', bobCapacity, bobRecognition, ...
);
const carolAllocations = computeAllocations(
	'carol', carolCapacity, carolRecognition, ...
);

// Recipients receive from multiple providers:
// Alice receives: 50 from Bob + 30 from Carol = 80 total
// System converges as multiple allocations fulfill needs
```

**Multi-Provider Support:**
- Inherently designed for N providers × M recipients
- Each provider independently allocates their capacity
- Recipients can receive from multiple providers simultaneously
- System-level convergence emerges from individual allocations
- True decentralized allocation (no coordinator needed)

**Divergence:** ❌ **COLLECTIVE RECOGNITION IS NOT TRULY MULTI-PROVIDER**  
Despite having a "MULTI-PROVIDER SCENARIOS" section, the collective recognition module does not support true multi-provider allocation where multiple providers simultaneously allocate to the same recipient. It only handles proxy scenarios where one entity manages capacity on behalf of another. This is a **fundamental architectural difference**.

---

## Use Case Analysis

### When to Use `collective-recognition.ts`

✅ **Collective Capacity Pools**
- Co-op housing allocating apartments to members
- Community tool library allocating equipment
- Worker cooperative allocating work shifts
- Collective budget allocating funds to projects

✅ **Single-Provider with Collective Governance**
- Municipality allocating social housing based on need + community recognition
- Foundation distributing grants based on collective priorities
- Community organization allocating volunteer hours

✅ **Explicit Compliance Requirements**
- Need to block certain members (regulatory/legal)
- Need to cap allocations (risk management)
- Need to apply filter unions (proxy scenarios)

✅ **Transparent Collective Decisions**
- Calculation of collective recognition shares visible to all
- Allocation formula deterministic and auditable
- Member set explicitly defined

**Key Characteristic:** ONE capacity pot, MANY recipients, COLLECTIVE decision

---

### When to Use `allocation.ts`

✅ **Decentralized Individual Capacity**
- Multiple people each allocating their own time/resources
- Network of providers each making independent decisions
- No central coordinator or collective pot

✅ **Personal Recognition-Based Giving**
- I allocate MY capacity based on MY recognition of others
- Mutual relationships prioritized (reciprocity)
- Self-care supported (I can allocate to myself)

✅ **Iterative Convergence Systems**
- Needs reduce over time as allocations are received
- Multiple rounds of allocation
- System converges to need fulfillment

✅ **Complex Divisibility Constraints**
- Resources that can't be arbitrarily divided (rooms, vehicles, equipment)
- Minimum allocation thresholds to prevent over-fragmentation
- Remainder redistribution for efficient capacity utilization

✅ **Dynamic Network Systems**
- Participants can join/leave
- Recognition weights can change
- No fixed member set

**Key Characteristic:** MANY providers, MANY recipients, INDIVIDUAL decisions, EMERGENT convergence

---

## Integration Possibilities

### Can These Two Models Coexist?

**Yes!** They serve different purposes and can be used together in a hybrid system:

#### Example: Community Resource Network

```
Layer 1: INDIVIDUAL ALLOCATION (allocation.ts)
├─ Alice allocates her tools based on her recognition
├─ Bob allocates his truck based on his recognition
└─ Carol allocates her time based on her recognition

Layer 2: COLLECTIVE ALLOCATION (collective-recognition.ts)
├─ Community Tool Co-op allocates shared workshop space
├─ Community Land Trust allocates housing
└─ Community Fund allocates grant money
```

**Integration Pattern:**
1. Individual capacities use `allocation.ts` (personal resources, individual decisions)
2. Collective capacities use `collective-recognition.ts` (pooled resources, collective decisions)
3. Recipients can receive from both individual providers AND collective pools
4. Recognition data flows through both systems (shared recognition graph)

---

## Recommendations

### For `collective-recognition.ts`

1. **Add Divisibility Constraints**
   - Implement `max_natural_div` and `min_allocation_percentage`
   - Add remainder redistribution
   - Ensure practical allocations for indivisible resources

2. **Consider Two-Tier Option**
   - Add optional tier system (mutual vs non-mutual within member set)
   - Allow prioritization of members with higher mutual recognition
   - Maintain current single-tier as default

3. **Clarify "Multi-Provider" Section**
   - Rename to "Proxy Scenarios" or "Filter Unions"
   - Document that this is NOT multi-provider allocation
   - Clarify use cases (external provider using entity as proxy)

### For `allocation.ts`

1. **Add Compliance Filter System**
   - Borrow filter types from `collective-recognition.ts`
   - Allow providers to set per-recipient filters (blocked, capped)
   - Useful for real-world constraints (legal, risk, policy)

2. **Document Single-Provider Mode**
   - Clarify that `computeAllocations()` is run PER provider
   - Provide examples of multi-provider coordination
   - Document how system-level convergence emerges

3. **Consider Collective Capacity Support**
   - Add a flag: `is_collective_capacity: boolean`
   - If true, use collective recognition shares instead of individual recognition
   - Allow hybrid systems (some providers individual, some collective)

---

## Conclusion

The collective recognition allocation and multi-provider free allocation modules implement **fundamentally different allocation paradigms**:

| Aspect | `collective-recognition.ts` | `allocation.ts` |
|--------|----------------------------|-----------------|
| **Provider Model** | Single provider (collective pot) | Multi-provider (distributed) |
| **Recognition Model** | Collective shares (symmetric) | Individual graphs (asymmetric) |
| **Allocation Strategy** | Proportional to shares | Two-tier (mutual + non-mutual) |
| **Slot Matching** | ✅ Slot-based, time/location compatible | ✅ Slot-based, time/location compatible |
| **Multi-Pass Logic** | ✅ Yes (target-based) | ✅ Yes (proportional with dynamic denominator) |
| **Divisibility** | ❌ No constraints | ✅ Natural units + min percentage |
| **Convergence** | ❌ Static (one-shot) | ✅ Dynamic (iterative) |
| **Filters** | ✅ Blocked, capped, unlimited | ❌ No filter system |
| **Multi-Provider** | ❌ Proxy only | ✅ True multi-provider |

**Conformance Assessment:**
- **Slot matching & compatibility:** ✅ **Aligned**
- **Multi-pass allocation:** ✅ **Similar** (different strategies)
- **Recognition model:** ❌ **Incompatible** (different semantics)
- **Provider model:** ❌ **Incompatible** (different architectures)
- **System dynamics:** ❌ **Incompatible** (static vs dynamic)

**Overall Verdict:** These modules are **NOT conformant** to each other's architecture, but this is **by design**. They solve different problems and should be viewed as complementary rather than competing approaches. A mature Free Association system would support BOTH models, allowing communities to choose the allocation paradigm that fits their context.

---

**Next Steps:**
1. Document the intended use cases for each module
2. Create examples showing how they can coexist in a hybrid system
3. Consider cross-pollination of features (filters for allocation.ts, divisibility for collective-recognition.ts)
4. Build integration tests showing hybrid allocation scenarios

