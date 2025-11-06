# Memoization Opportunities in Allocation Algorithm

## Summary

This document identifies functions that would benefit from memoization to improve performance when called with the same inputs repeatedly.

## High Priority Memoization Targets

### 1. `findCompatibleRecipients` (allocation.ts:792-821)

**Current Complexity**: O(R × S) per capacity slot  
**Call Frequency**: Once per capacity slot in `computeAllocations`  
**Memoization Strategy**: Cache by `capacitySlot.id` + commitments hash

**Implementation**:
```typescript
// In allocation.ts
import { createMemoCacheWithKey, hashObject } from '$lib/protocol/utils/memoize';

const memoizedFindCompatibleRecipients = createMemoCacheWithKey(
	findCompatibleRecipients,
	(capacitySlot, allCommitments, myPubKey) => 
		`${capacitySlot.id}:${hashObject(allCommitments)}:${myPubKey}`,
	50 // Cache up to 50 capacity slot lookups
);

// Replace calls to findCompatibleRecipients with memoizedFindCompatibleRecipients
```

**Expected Benefit**: 
- Avoids recomputing compatibility when commitments haven't changed
- Especially beneficial when processing multiple capacity slots of the same type

### 2. `computeMutualRecognition` (allocation.ts:478-513)

**Current Complexity**: O(n × m) where n = my recognition entries, m = others' recognition entries  
**Call Frequency**: Called in reactive contexts, potentially multiple times per render  
**Memoization Strategy**: Cache by recognition weights hash

**Implementation**:
```typescript
// In allocation.ts
import { createMemoCacheWithKey, hashObject } from '$lib/protocol/utils/memoize';

const memoizedComputeMutualRecognition = createMemoCacheWithKey(
	computeMutualRecognition,
	(myRecognition, othersRecognition, myPubKey) => 
		`${hashObject(myRecognition)}:${hashObject(othersRecognition)}:${myPubKey}`,
	100 // Cache up to 100 recognition computations
);

// Export memoized version
export { memoizedComputeMutualRecognition as computeMutualRecognition };
```

**Expected Benefit**: 
- High impact - mutual recognition is computed frequently
- Recognition weights change infrequently, so cache hit rate should be high

### 3. `buildSystemState` (allocation.ts:124-160)

**Current Complexity**: O(C × S) where C = commitments, S = slots  
**Call Frequency**: Called in `updateSystemStateFromNetwork()` and inside `myAllocationsAsProvider`  
**Memoization Strategy**: Cache by commitments hash + previous state hash

**Implementation**:
```typescript
// In allocation.ts
import { createMemoCacheWithKey, hashObject } from '$lib/protocol/utils/memoize';

const memoizedBuildSystemState = createMemoCacheWithKey(
	buildSystemState,
	(commitments, previousState) => 
		`${hashObject(commitments)}:${previousState ? hashObject(previousState) : 'null'}`,
	20 // Cache up to 20 system state builds
);

// Export memoized version
export { memoizedBuildSystemState as buildSystemState };
```

**Expected Benefit**: 
- Medium impact - system state is rebuilt frequently
- Commitments change less frequently than allocations are computed

## Medium Priority Memoization Targets

### 4. `computeDampingFactors` (allocation.ts:400-425)

**Current Complexity**: O(n) where n = number of types  
**Call Frequency**: Called in reactive derived store  
**Memoization Strategy**: Cache by history hash

**Implementation**:
```typescript
// In allocation.ts
import { createMemoCache, hashObject } from '$lib/protocol/utils/memoize';

const memoizedComputeDampingFactors = createMemoCache(
	computeDampingFactors,
	50 // Cache up to 50 damping factor computations
);

// Export memoized version
export { memoizedComputeDampingFactors as computeDampingFactors };
```

**Expected Benefit**: 
- Low-Medium impact - function is already relatively cheap
- Easy to implement, low risk

## Very High Priority: Reactive Store Optimization

### 5. `myAllocationsAsProvider` Derived Store (allocation.svelte.ts:436-981)

**Current Complexity**: O(C × R × S) - most expensive operation  
**Issue**: Recalculates whenever ANY dependency changes, even if content is identical  
**Strategy**: Deep equality check before recomputing

**Implementation**:
```typescript
// In allocation.svelte.ts
import { deepEqual } from '$lib/protocol/utils/memoize';

let lastInputs: {
	myPub: string | null;
	myMR: Record<string, number>;
	myRec: GlobalRecognitionWeights;
	myCommitment: Commitment | null;
} | null = null;

let lastResult: {
	allocations: SlotAllocationRecord[];
	totalsByTypeAndRecipient: Record<string, Record<string, number>>;
	convergence: ConvergenceSummary | null;
	slotDenominators: Record<string, { mutual: number; nonMutual: number; need_type_id: string }>;
} | null = null;

export const myAllocationsAsProvider: Readable<...> = derived(
	[myPublicKey, myMutualRecognition, myRecognitionOfOthers, myCommitmentStore],
	([$myPub, $myMR, $myRec, $myCommitment]) => {
		// Check if inputs actually changed
		const currentInputs = { myPub: $myPub, myMR: $myMR, myRec: $myRec, myCommitment: $myCommitment };
		
		if (lastInputs && deepEqual(currentInputs, lastInputs) && lastResult) {
			console.log('[MEMOIZATION] Reusing allocation result (inputs unchanged)');
			return lastResult;
		}
		
		// ... existing computation ...
		
		// Store for next time
		lastInputs = currentInputs;
		lastResult = { allocations, totalsByTypeAndRecipient, convergence, slotDenominators };
		
		return lastResult;
	}
);
```

**Expected Benefit**: 
- **Very High** - This is the main performance bottleneck
- Prevents unnecessary O(C × R × S) computations when dependencies haven't meaningfully changed
- Svelte stores use reference equality, so object references can change even when content is identical

## Low Priority (Already Optimized)

### 6. `getCandidateRecipients` (allocation.svelte.ts:378-418)

**Status**: Already uses spatial/temporal indexing (O(k) instead of O(N))  
**Memoization**: Not needed - already optimized

### 7. Convergence Metrics Functions

**Status**: Already relatively cheap (O(n) or O(1))  
**Memoization**: Low benefit, but could be added if profiling shows they're bottlenecks

## Implementation Notes

1. **Cache Size Limits**: All caches have size limits to prevent memory leaks
2. **Cache Key Strategy**: Use JSON.stringify or custom hash functions for complex objects
3. **Testing**: Memoization should be transparent - same inputs produce same outputs
4. **Profiling**: Measure before/after to verify improvements

## When NOT to Memoize

- Functions with side effects (memoization assumes purity)
- Functions that are already very cheap (< 1ms)
- Functions called infrequently
- Functions where inputs change on every call (no cache hits)

## Performance Testing

After implementing memoization:

1. Profile allocation computation time
2. Measure cache hit rates
3. Monitor memory usage
4. Verify correctness (same outputs for same inputs)

## Future Optimizations

- **Web Workers**: Move allocation computation to background thread
- **Incremental Updates**: Only recompute changed parts of allocations
- **Parallel Processing**: Process multiple capacity slots in parallel

