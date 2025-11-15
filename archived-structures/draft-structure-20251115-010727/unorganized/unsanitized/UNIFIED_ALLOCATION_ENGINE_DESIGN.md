# Unified Allocation Engine Design

**Date:** 2025-11-09  
**Proposal:** Separate distribution calculation from allocation execution

## Problem Statement

Currently, our allocation logic is tightly coupled with distribution calculation:

```
┌─────────────────────────────────────────────────────────────┐
│  allocation.ts (Mutual Recognition)                         │
│                                                              │
│  ┌─────────────────────┐     ┌──────────────────────┐      │
│  │ Calculate MR        │────▶│ Allocate Slots       │      │
│  │ Distribution        │     │ (Multi-pass logic)   │      │
│  └─────────────────────┘     └──────────────────────┘      │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│  collective-recognition.ts (Collective Recognition)         │
│                                                              │
│  ┌─────────────────────┐     ┌──────────────────────┐      │
│  │ Calculate CR        │────▶│ Allocate Slots       │      │
│  │ Shares              │     │ (Simpler logic)      │      │
│  └─────────────────────┘     └──────────────────────┘      │
└─────────────────────────────────────────────────────────────┘
```

**Issues:**
1. **Code Duplication**: Slot allocation logic is duplicated
2. **Inconsistency**: Different allocation behaviors for same problem
3. **Inflexibility**: Can't easily add new distribution methods
4. **Complexity**: Hard to reason about which allocation logic is "better"

## Proposed Architecture

Separate distribution calculation from allocation execution:

```
┌──────────────────────────────────────────────────────────────────┐
│  Distribution Calculation Layer                                   │
│                                                                    │
│  ┌─────────────────┐  ┌─────────────────┐  ┌───────────────┐   │
│  │ Mutual          │  │ Collective      │  │ Custom        │   │
│  │ Recognition     │  │ Recognition     │  │ Distribution  │   │
│  └────────┬────────┘  └────────┬────────┘  └───────┬───────┘   │
│           │                     │                    │            │
│           └─────────────────────┼────────────────────┘            │
│                                 ▼                                 │
│                    ┌─────────────────────────┐                   │
│                    │  Distribution Result    │                   │
│                    │  Record<recipientId,    │                   │
│                    │         share: number>  │                   │
│                    └────────────┬────────────┘                   │
└─────────────────────────────────┼────────────────────────────────┘
                                  │
                                  ▼
┌──────────────────────────────────────────────────────────────────┐
│  Unified Allocation Engine                                        │
│                                                                    │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  allocateCapacityWithDistribution()                       │   │
│  │                                                            │   │
│  │  Input:                                                    │   │
│  │  - Capacity slots                                          │   │
│  │  - Need slots (per recipient)                             │   │
│  │  - Distribution (shares per recipient)                    │   │
│  │  - Constraints (divisibility, filters, etc.)              │   │
│  │                                                            │   │
│  │  Output:                                                   │   │
│  │  - Slot allocations (which slot → which recipient)        │   │
│  │  - Allocation records                                      │   │
│  │  - Utilization metrics                                     │   │
│  │                                                            │   │
│  │  Features:                                                 │   │
│  │  ✅ Multi-pass proportional allocation                    │   │
│  │  ✅ Slot-level time/location compatibility                │   │
│  │  ✅ Divisibility constraints (natural units, min %)       │   │
│  │  ✅ Remainder redistribution (Largest Remainder Method)   │   │
│  │  ✅ Compliance filters (blocked, capped, unlimited)       │   │
│  │  ✅ Spatial/temporal indexing for O(k) lookups            │   │
│  │  ✅ Active recipient tracking for efficiency              │   │
│  └──────────────────────────────────────────────────────────┘   │
└──────────────────────────────────────────────────────────────────┘
```

## Key Insight

The slot allocation logic is **distribution-agnostic**! It doesn't care HOW you calculated shares, it just needs:

1. **Who** should receive (recipient IDs)
2. **What proportion** each should get (shares: 0-1)
3. **What constraints** apply (filters, divisibility)

Everything else (slot matching, multi-pass, remainder redistribution) is the same regardless of distribution method!

---

## Detailed Design

### 1. Distribution Interface

```typescript
/**
 * Distribution Result
 * 
 * Represents the outcome of ANY distribution calculation method.
 * The allocation engine doesn't care HOW this was calculated.
 */
export interface DistributionResult {
	/** 
	 * Recipient shares (0-1, should sum to ≤ 1.0)
	 * Share represents: "What proportion of total capacity should this recipient receive?"
	 */
	shares: Map<string, number>;
	
	/**
	 * Method used to calculate distribution (for transparency)
	 */
	method: 'mutual-recognition' | 'collective-recognition' | 'equal-shares' | 'custom';
	
	/**
	 * Metadata about distribution calculation (for transparency/verification)
	 */
	metadata?: {
		/** For mutual recognition: pairwise MR matrix */
		mutualRecognitionMatrix?: Record<string, Record<string, number>>;
		
		/** For collective recognition: member recognition sums, pool */
		memberRecognitionSums?: Record<string, number>;
		totalPool?: number;
		
		/** Timestamp of calculation */
		timestamp: number;
		
		/** Any other method-specific data */
		[key: string]: any;
	};
}
```

### 2. Distribution Calculation Functions

```typescript
// === MUTUAL RECOGNITION DISTRIBUTION ===

/**
 * Calculate distribution based on mutual recognition
 * 
 * This is the EXISTING logic from allocation.ts, extracted as pure function
 */
export function calculateMutualRecognitionDistribution(
	myRecognition: GlobalRecognitionWeights,
	othersRecognition: Record<string, GlobalRecognitionWeights>,
	myPubKey: string,
	options?: {
		includeSelf?: boolean;  // Include self-recognition?
		tierStrategy?: 'mutual-only' | 'two-tier';  // Mutual only or include non-mutual?
	}
): DistributionResult {
	const shares = new Map<string, number>();
	const mutualRecognitionMatrix: Record<string, Record<string, number>> = {};
	
	// Calculate mutual recognition for each potential recipient
	const mutualRecognition = computeMutualRecognition(
		myRecognition, 
		othersRecognition, 
		myPubKey
	);
	
	// Calculate shares
	// For mutual recognition, shares are just the normalized MR values
	let totalMR = 0;
	for (const [recipientId, mr] of Object.entries(mutualRecognition)) {
		if (mr > 0) {
			totalMR += mr;
		}
	}
	
	if (totalMR > 0) {
		for (const [recipientId, mr] of Object.entries(mutualRecognition)) {
			if (mr > 0) {
				shares.set(recipientId, mr / totalMR);
			}
		}
	}
	
	// Build MR matrix for transparency
	mutualRecognitionMatrix[myPubKey] = {};
	for (const [recipientId, mr] of Object.entries(mutualRecognition)) {
		mutualRecognitionMatrix[myPubKey][recipientId] = mr;
	}
	
	return {
		shares,
		method: 'mutual-recognition',
		metadata: {
			mutualRecognitionMatrix,
			timestamp: Date.now(),
			tierStrategy: options?.tierStrategy || 'two-tier'
		}
	};
}

// === COLLECTIVE RECOGNITION DISTRIBUTION ===

/**
 * Calculate distribution based on collective recognition shares
 * 
 * This is the EXISTING logic from collective-recognition.ts, extracted
 */
export function calculateCollectiveRecognitionDistribution(
	memberSet: string[],
	memberTrees: Map<string, Node>
): DistributionResult {
	// Use existing calculateCollectiveRecognitionShares function
	const result = calculateCollectiveRecognitionShares(memberSet, memberTrees);
	
	return {
		shares: result.shares,
		method: 'collective-recognition',
		metadata: {
			mutualRecognitionMatrix: Object.fromEntries(
				Array.from(result.mutualRecognitionMatrix.entries()).map(
					([id, map]) => [id, Object.fromEntries(map)]
				)
			),
			memberRecognitionSums: Object.fromEntries(result.memberRecognitionSums),
			totalPool: result.totalPool,
			timestamp: Date.now()
		}
	};
}

// === OTHER DISTRIBUTION METHODS ===

/**
 * Equal shares distribution (fallback)
 */
export function calculateEqualSharesDistribution(
	recipientIds: string[]
): DistributionResult {
	const shares = new Map<string, number>();
	const equalShare = 1.0 / recipientIds.length;
	
	for (const id of recipientIds) {
		shares.set(id, equalShare);
	}
	
	return {
		shares,
		method: 'equal-shares',
		metadata: {
			timestamp: Date.now(),
			recipientCount: recipientIds.length
		}
	};
}

/**
 * Custom distribution (user-provided shares)
 */
export function createCustomDistribution(
	shares: Map<string, number>
): DistributionResult {
	return {
		shares,
		method: 'custom',
		metadata: {
			timestamp: Date.now()
		}
	};
}
```

### 3. Unified Allocation Engine

```typescript
/**
 * Allocation Configuration
 */
export interface AllocationConfig {
	/** Compliance filters per recipient */
	filters?: Map<string, ComplianceFilter>;
	
	/** Damping factors per recipient (for over-allocation correction) */
	dampingFactors?: Map<string, number>;
	
	/** Spatial/temporal index for O(k) lookups */
	needsIndex?: SpaceTimeIndex;
	
	/** Maximum passes (prevent infinite loops) */
	maxPasses?: number;
	
	/** Capacity epsilon (floating point tolerance) */
	epsilon?: number;
}

/**
 * Unified Allocation Engine
 * 
 * Allocates capacity to recipients based on a pre-computed distribution.
 * This function is AGNOSTIC to how the distribution was calculated!
 * 
 * Features:
 * - Multi-pass proportional allocation
 * - Slot-level time/location compatibility
 * - Divisibility constraints (natural units, min %)
 * - Remainder redistribution (Largest Remainder Method)
 * - Compliance filters (blocked, capped, unlimited)
 * - Spatial/temporal indexing for O(k) lookups
 * 
 * @param capacitySlots - Provider's available capacity slots
 * @param needsByRecipient - Each recipient's need slots
 * @param distribution - Pre-computed distribution (shares per recipient)
 * @param config - Allocation configuration
 * @returns Detailed allocation result with slot-level breakdown
 */
export function allocateCapacityWithDistribution(
	capacitySlots: AvailabilitySlot[],
	needsByRecipient: Map<string, NeedSlot[]>,
	distribution: DistributionResult,
	config?: AllocationConfig
): {
	// Slot-level allocations
	slot_allocations: SlotAllocationRecord[];
	
	// Per-recipient totals
	recipient_totals: Map<string, number>;
	
	// Per-recipient targets (share × capacity, limited by filters)
	recipient_targets: Map<string, number>;
	
	// Capacity metrics
	total_capacity: number;
	total_allocated: number;
	total_unused: number;
	utilization_rate: number;
	
	// Detailed slot states (for debugging/transparency)
	capacity_slot_states: Map<string, CapacitySlotState>;
	need_slot_states: Map<string, NeedSlotState>;
	
	// Distribution metadata (for verification)
	distribution_method: string;
	distribution_metadata?: any;
} {
	const EPSILON = config?.epsilon || 0.0001;
	const maxPasses = config?.maxPasses || 10;
	const filters = config?.filters || new Map();
	const dampingFactors = config?.dampingFactors || new Map();
	
	// === STEP 1: Calculate total capacity ===
	const totalCapacity = capacitySlots.reduce((sum, slot) => sum + slot.quantity, 0);
	
	// === STEP 2: Calculate target allocations for each recipient ===
	// Target = share × total capacity, limited by filter
	const recipientTargets = new Map<string, number>();
	
	for (const [recipientId, share] of distribution.shares.entries()) {
		const filter = filters.get(recipientId) || { type: 'unlimited' };
		const filterValue = getFilterValue(filter);
		
		// Apply damping if provided
		const damping = dampingFactors.get(recipientId) || 1.0;
		
		// Target = share × capacity × damping, limited by filter
		const rawTarget = share * totalCapacity * damping;
		const target = Math.min(rawTarget, filterValue);
		
		recipientTargets.set(recipientId, target);
	}
	
	// === STEP 3: Initialize slot states ===
	const capacitySlotStates = new Map<string, CapacitySlotState>();
	for (const slot of capacitySlots) {
		capacitySlotStates.set(slot.id, {
			slot_id: slot.id,
			original_quantity: slot.quantity,
			remaining_quantity: slot.quantity,
			allocations: []
		});
	}
	
	const needSlotStates = new Map<string, NeedSlotState>();
	for (const [recipientId, needSlots] of needsByRecipient.entries()) {
		for (const slot of needSlots) {
			needSlotStates.set(slot.id, {
				slot_id: slot.id,
				recipient_id: recipientId,
				original_quantity: slot.quantity,
				fulfilled_quantity: 0,
				allocations: []
			});
		}
	}
	
	// === STEP 4: Build compatibility matrix ===
	// Pre-compute which capacity slots are compatible with which need slots
	// This uses spatial/temporal indexing if provided (O(k) instead of O(N))
	const compatibilityMatrix = buildCompatibilityMatrix(
		capacitySlots,
		needsByRecipient,
		config?.needsIndex
	);
	
	// === STEP 5: Multi-pass proportional allocation ===
	const slotAllocations: SlotAllocationRecord[] = [];
	const recipientTotals = new Map<string, number>();
	
	let activeRecipients = new Set<string>(distribution.shares.keys());
	let activeCapacitySlots = new Set<string>(capacitySlots.map(s => s.id));
	let totalAllocated = 0;
	let passCount = 0;
	
	while (
		activeRecipients.size > 0 && 
		activeCapacitySlots.size > 0 && 
		totalAllocated < totalCapacity - EPSILON &&
		passCount < maxPasses
	) {
		passCount++;
		const passStartAllocated = totalAllocated;
		
		// Iterate over active recipients
		for (const recipientId of activeRecipients) {
			const target = recipientTargets.get(recipientId) || 0;
			const currentTotal = recipientTotals.get(recipientId) || 0;
			
			// Skip if at target
			if (currentTotal >= target - EPSILON) {
				activeRecipients.delete(recipientId);
				continue;
			}
			
			const remainingTarget = target - currentTotal;
			const needSlots = needsByRecipient.get(recipientId) || [];
			let recipientPassTotal = 0;
			
			// Try to allocate from compatible capacity slots
			for (const needSlot of needSlots) {
				const needSlotState = needSlotStates.get(needSlot.id)!;
				const remainingNeed = needSlot.quantity - needSlotState.fulfilled_quantity;
				
				if (remainingNeed <= EPSILON) continue;
				
				// Get compatible capacity slots from pre-computed matrix
				const compatibleCapSlotIds = compatibilityMatrix.get(needSlot.id) || [];
				
				for (const capSlotId of compatibleCapSlotIds) {
					if (!activeCapacitySlots.has(capSlotId)) continue; // Exhausted
					
					const capSlotState = capacitySlotStates.get(capSlotId)!;
					
					if (capSlotState.remaining_quantity <= EPSILON) {
						activeCapacitySlots.delete(capSlotId);
						continue;
					}
					
					// Calculate allocation amount
					const canAllocate = Math.min(
						capSlotState.remaining_quantity,
						remainingNeed,
						remainingTarget - recipientPassTotal
					);
					
					if (canAllocate <= EPSILON) continue;
					
					// Apply divisibility constraints
					const capSlot = capacitySlots.find(s => s.id === capSlotId)!;
					const sharePercentage = canAllocate / capSlot.quantity;
					const constrained = applyDivisibilityConstraints(
						canAllocate,
						sharePercentage,
						capSlot
					);
					
					// Check minimum allocation threshold
					if (!meetsMinimumAllocation(constrained, capSlot)) {
						continue;
					}
					
					// Create allocation record
					const allocation: SlotAllocationRecord = {
						availability_slot_id: capSlotId,
						recipient_pubkey: recipientId,
						recipient_need_slot_id: needSlot.id,
						quantity: constrained,
						need_type_id: needSlot.need_type_id,
						time_compatible: true,
						location_compatible: true,
						tier: 'distribution-based' // No tier distinction with unified engine
					};
					
					// Update states
					capSlotState.remaining_quantity -= constrained;
					capSlotState.allocations.push(allocation);
					needSlotState.fulfilled_quantity += constrained;
					needSlotState.allocations.push(allocation);
					recipientPassTotal += constrained;
					totalAllocated += constrained;
					
					slotAllocations.push(allocation);
					
					// If reached target, move to next recipient
					if (recipientPassTotal >= remainingTarget - EPSILON) break;
				}
				
				if (recipientPassTotal >= remainingTarget - EPSILON) break;
			}
			
			// Update recipient total
			recipientTotals.set(recipientId, currentTotal + recipientPassTotal);
			
			// Remove from active set if no unfulfilled needs
			const allNeedsFulfilled = needSlots.every(slot => {
				const state = needSlotStates.get(slot.id)!;
				return state.fulfilled_quantity >= slot.quantity - EPSILON;
			});
			if (allNeedsFulfilled) {
				activeRecipients.delete(recipientId);
			}
		}
		
		// Early exit if no progress
		if (totalAllocated <= passStartAllocated + EPSILON) {
			break; // Stuck
		}
	}
	
	// === STEP 6: Remainder redistribution ===
	// Apply Largest Remainder Method to redistribute leftover capacity
	// This is the SAME logic from allocation.ts
	if (totalAllocated < totalCapacity - EPSILON) {
		// Calculate remainders for each recipient
		const remainders = new Map<string, number>();
		for (const [recipientId, target] of recipientTargets.entries()) {
			const allocated = recipientTotals.get(recipientId) || 0;
			const remainder = target - allocated;
			if (remainder > EPSILON) {
				remainders.set(recipientId, remainder);
			}
		}
		
		// Redistribute using Largest Remainder Method
		// (This would use the existing redistributeRemainders function)
		// ... implementation ...
	}
	
	// === STEP 7: Return result ===
	const totalUnused = totalCapacity - totalAllocated;
	const utilizationRate = totalCapacity > 0 ? totalAllocated / totalCapacity : 0;
	
	return {
		slot_allocations: slotAllocations,
		recipient_totals: recipientTotals,
		recipient_targets: recipientTargets,
		total_capacity: totalCapacity,
		total_allocated: totalAllocated,
		total_unused: totalUnused,
		utilization_rate: utilizationRate,
		capacity_slot_states: capacitySlotStates,
		need_slot_states: needSlotStates,
		distribution_method: distribution.method,
		distribution_metadata: distribution.metadata
	};
}

// === HELPER TYPES ===

interface CapacitySlotState {
	slot_id: string;
	original_quantity: number;
	remaining_quantity: number;
	allocations: SlotAllocationRecord[];
}

interface NeedSlotState {
	slot_id: string;
	recipient_id: string;
	original_quantity: number;
	fulfilled_quantity: number;
	allocations: SlotAllocationRecord[];
}

/**
 * Build compatibility matrix: need_slot_id → compatible capacity_slot_ids
 * Uses spatial/temporal indexing if provided
 */
function buildCompatibilityMatrix(
	capacitySlots: AvailabilitySlot[],
	needsByRecipient: Map<string, NeedSlot[]>,
	needsIndex?: SpaceTimeIndex
): Map<string, string[]> {
	const matrix = new Map<string, string[]>();
	
	// For each need slot, find compatible capacity slots
	for (const [recipientId, needSlots] of needsByRecipient.entries()) {
		for (const needSlot of needSlots) {
			const compatible: string[] = [];
			
			// Use index if provided (O(k)) or full scan (O(N))
			const candidateSlots = needsIndex 
				? getCandidateSlotsFromIndex(needSlot, capacitySlots, needsIndex)
				: capacitySlots;
			
			for (const capSlot of candidateSlots) {
				// Check detailed compatibility
				if (slotsCompatible(needSlot, capSlot)) {
					compatible.push(capSlot.id);
				}
			}
			
			matrix.set(needSlot.id, compatible);
		}
	}
	
	return matrix;
}
```

---

## Migration Path

### Phase 1: Extract Distribution Functions (No Breaking Changes)

1. Create new file: `src/lib/protocol/distribution.ts`
2. Extract distribution calculation from `allocation.ts`:
   - `calculateMutualRecognitionDistribution()`
3. Extract distribution calculation from `collective-recognition.ts`:
   - `calculateCollectiveRecognitionDistribution()`
4. Keep existing functions as wrappers (backward compatible)

### Phase 2: Create Unified Allocation Engine

1. Create new file: `src/lib/protocol/allocation-engine.ts`
2. Extract core allocation logic from `allocation.ts`:
   - Multi-pass algorithm
   - Divisibility constraints
   - Remainder redistribution
   - Compatibility matrix building
3. Make it accept `DistributionResult` as input
4. Keep existing `computeAllocations()` as wrapper

### Phase 3: Migrate Collective Recognition

1. Update `collective-recognition.ts` to use unified engine
2. Replace its allocation logic with call to `allocateCapacityWithDistribution()`
3. Test extensively to ensure same results

### Phase 4: Update Consumers

1. Update `allocation.svelte.ts` to use new architecture
2. Update tests to cover both distribution methods
3. Update documentation

---

## Benefits

### 1. **Code Reuse**
- Single implementation of sophisticated allocation logic
- No duplication between modules
- Easier to maintain and improve

### 2. **Consistency**
- Same allocation behavior regardless of distribution method
- Predictable results
- Easier to test

### 3. **Flexibility**
- Easy to add new distribution methods:
  - Needs-based (allocate based on need urgency)
  - Time-weighted (allocate more to long-term members)
  - Hybrid (combination of recognition + needs)
  - DAO voting-based
- Distribution calculation is a plugin

### 4. **Testability**
- Distribution calculation can be tested independently
- Allocation engine can be tested with known distributions
- Easier to verify correctness

### 5. **Transparency**
- Clear separation of "who gets what" (distribution) from "how to allocate" (engine)
- Easier to explain and audit
- Users can understand what's happening

---

## Example Usage

```typescript
// === Example 1: Mutual Recognition Allocation ===

// Step 1: Calculate distribution
const mrDistribution = calculateMutualRecognitionDistribution(
	myRecognition,
	othersRecognition,
	myPubKey
);

// Step 2: Allocate using unified engine
const result = allocateCapacityWithDistribution(
	myCapacitySlots,
	needsByRecipient,
	mrDistribution,
	{ filters, dampingFactors, needsIndex }
);

console.log(`Allocated ${result.total_allocated} / ${result.total_capacity}`);
console.log(`Method: ${result.distribution_method}`);

// === Example 2: Collective Recognition Allocation ===

// Step 1: Calculate distribution
const crDistribution = calculateCollectiveRecognitionDistribution(
	memberSet,
	memberTrees
);

// Step 2: Allocate using SAME unified engine
const result = allocateCapacityWithDistribution(
	capacitySlots,
	needsByRecipient,
	crDistribution,
	{ filters }
);

console.log(`Allocated ${result.total_allocated} / ${result.total_capacity}`);
console.log(`Method: ${result.distribution_method}`);

// === Example 3: Custom Distribution ===

// Step 1: Create custom distribution (e.g., from DAO vote)
const customShares = new Map([
	['alice', 0.4],
	['bob', 0.3],
	['carol', 0.3]
]);

const customDistribution = createCustomDistribution(customShares);

// Step 2: Allocate using SAME unified engine
const result = allocateCapacityWithDistribution(
	capacitySlots,
	needsByRecipient,
	customDistribution
);

console.log(`Allocated ${result.total_allocated} / ${result.total_capacity}`);
console.log(`Method: ${result.distribution_method}`);
```

---

## Open Questions

1. **Should we maintain tier system in unified engine?**
   - Current `allocation.ts` has two-tier (mutual + non-mutual)
   - Unified engine could accept TWO distributions (tier 1 + tier 2)
   - Or remove tier system entirely and handle via distribution method

2. **How to handle convergence metrics?**
   - Current `allocation.ts` tracks convergence over iterations
   - Should unified engine track this?
   - Or is convergence only relevant for multi-provider iterative systems?

3. **Should filters be part of distribution or allocation?**
   - Current design: filters in allocation engine
   - Alternative: filters constrain distribution calculation
   - Affects how shares are normalized

4. **How to handle damping factors?**
   - Current: damping applied during allocation
   - Alternative: damping applied during distribution calculation
   - Affects whether distribution.shares already account for damping

---

## Next Steps

1. **Review this design** with team/community
2. **Prototype** the distribution interface
3. **Extract** distribution functions (Phase 1)
4. **Implement** unified engine (Phase 2)
5. **Migrate** collective recognition (Phase 3)
6. **Test** extensively with both distribution methods
7. **Document** the new architecture
8. **Deploy** with backward compatibility

---

## Conclusion

By separating distribution calculation from allocation execution, we create a more modular, flexible, and maintainable system. The unified allocation engine becomes a shared primitive that can work with ANY distribution method, while distribution methods become plugins that can be easily added, tested, and swapped.

This design aligns with the Free Association principles of modularity, transparency, and flexibility.

