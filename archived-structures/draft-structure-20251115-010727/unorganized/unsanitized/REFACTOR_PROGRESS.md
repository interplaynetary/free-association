# Allocation Refactor Progress

**Date:** 2025-11-09  
**Goal:** Make allocation.ts distribution-agnostic by accepting pre-computed distributions

## Status: Phase 1 Complete ✅

---

## ✅ Completed

### 1. Created Distribution Module (`src/lib/protocol/distribution.ts`)

**What it contains:**
- `DistributionResult` interface - unified result type for all distribution methods
- `computeMutualRecognition()` - calculates mutual recognition between participants
- `calculateTwoTierMutualRecognitionDistribution()` - two-tier (mutual + non-mutual) distribution
- `calculateMutualRecognitionDistribution()` - simple mutual recognition distribution
- `calculateEqualSharesDistribution()` - equal shares fallback
- `createCustomDistribution()` - custom/manual distribution

**Benefits:**
- Single source of truth for distribution calculation
- Pluggable distribution strategies
- Clean separation of concerns (distribution ≠ allocation)
- Easy to test and verify

### 2. Updated `allocation.ts` Imports

**Changes:**
- Added import of all distribution functions from `$lib/protocol/distribution`
- Re-exported distribution functions for backward compatibility
- Removed duplicate `_computeMutualRecognition` implementation
- Removed duplicate `computeMutualRecognition` export

**Result:**
- No breaking changes for existing consumers
- `allocation.ts` now uses distribution functions from distribution module
- Cleaner code structure

---

## 🔨 Next Steps

### Phase 2: Create Distribution-Agnostic Allocation Function

Currently, `computeAllocations()` in `allocation.ts` still has hard-coded two-tier mutual recognition logic.

**What needs to be done:**

1. **Create `allocateWithDistribution()` function** (already exists at line 1216-1435, but needs verification)
   ```typescript
   export function allocateWithDistribution(
       myPubKey: string,
       myCapacitySlots: AvailabilitySlot[],
       distribution: DistributionResult,
       allCommitments: Record<string, Commitment>,
       options?: {
           dampingFactors?: Record<string, Record<string, number>>;
           needsIndex?: SpaceTimeIndex;
       }
   ): SlotAllocationRecord[]
   ```

2. **Update `computeAllocations()` to use distributions**
   ```typescript
   export function computeAllocations(
       myPubKey: string,
       myCapacitySlots: AvailabilitySlot[],
       myRecognition: GlobalRecognitionWeights,
       mutualRecognition: Record<string, number>,
       allCommitments: Record<string, Commitment>,
       currentState: SystemStateSnapshot,
       previousState: SystemStateSnapshot | null,
       needsIndex?: SpaceTimeIndex
   ): AllocationResult {
       // STEP 1: Calculate distribution
       const distribution = calculateTwoTierMutualRecognitionDistribution(
           myRecognition,
           othersRecognitionFromCommitments(allCommitments),
           myPubKey
       );
       
       // STEP 2: Allocate using distribution
       const allocations = allocateWithDistribution(
           myPubKey,
           myCapacitySlots,
           distribution,
           allCommitments,
           { dampingFactors, needsIndex }
       );
       
       // STEP 3: Compute convergence
       const convergence = computeConvergenceSummary(...);
       
       return { allocations, convergence, ... };
   }
   ```

3. **Extract helper: `othersRecognitionFromCommitments()`**
   - Currently scattered throughout `computeAllocations()`
   - Should be extracted as pure function
   - Converts `allCommitments` into `Record<string, GlobalRecognitionWeights>`

### Phase 3: Update Collective Recognition Module

Once allocation.ts is distribution-agnostic, update `collective-recognition.ts`:

1. **Use `calculateCollectiveRecognitionDistribution()`**
   ```typescript
   import { calculateCollectiveRecognitionDistribution } from '$lib/protocol/distribution';
   
   export function computeAllocations(
       capacity: BaseCapacity,
       needs: Map<string, BaseNeed>,
       memberTrees: Map<string, Node>,
       recognitionData?: RecognitionData[]
   ): AllocationComputationResult {
       // STEP 1: Calculate collective recognition distribution
       const distribution = calculateCollectiveRecognitionDistribution(
           capacity.members,
           memberTrees
       );
       
       // STEP 2: Use unified allocation engine
       const result = allocateWithDistribution(
           capacity,
           needsByRecipient,
           distribution,
           { filters }
       );
       
       return result;
   }
   ```

2. **Add collective recognition distribution to `distribution.ts`**
   ```typescript
   import { calculateCollectiveRecognitionShares } from '$lib/protocol/collective/collective-recognition';
   
   export function calculateCollectiveRecognitionDistribution(
       memberSet: string[],
       memberTrees: Map<string, Node>
   ): DistributionResult {
       const result = calculateCollectiveRecognitionShares(memberSet, memberTrees);
       
       return {
           shares: Object.fromEntries(result.shares),
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
   ```

### Phase 4: Testing & Verification

1. **Unit Tests**
   - Test each distribution function independently
   - Test `allocateWithDistribution()` with different distributions
   - Verify backward compatibility

2. **Integration Tests**
   - Test full allocation pipeline
   - Verify results match previous implementation
   - Test with real-world scenarios

3. **Performance Tests**
   - Measure impact on allocation speed
   - Verify memoization is working
   - Check memory usage

### Phase 5: Documentation & Examples

1. **Update README**
   - Document new architecture
   - Provide examples of custom distributions
   - Explain distribution interface

2. **Create Examples**
   - Example: Custom distribution from DAO voting
   - Example: Needs-based distribution
   - Example: Hybrid distribution (recognition + needs)

3. **Migration Guide**
   - For users of current API
   - Breaking changes (if any)
   - Benefits of new architecture

---

## Benefits of This Refactor

### 1. **Modularity**
- Distribution calculation separated from allocation execution
- Easy to add new distribution methods
- Clear interfaces between components

### 2. **Flexibility**
- Any distribution method can use the same sophisticated allocation engine
- Users can create custom distributions
- Easy to experiment with new distribution strategies

### 3. **Testability**
- Distribution and allocation can be tested independently
- Mock distributions for testing allocation logic
- Easier to verify correctness

### 4. **Performance**
- Memoization works across both modules
- Avoid duplicate calculations
- Clear optimization boundaries

### 5. **Maintainability**
- Single source of truth for each concern
- Easier to understand and modify
- Less code duplication

---

## Example: Using Custom Distribution

```typescript
// 1. Create your own distribution (e.g., from DAO vote)
const myDistribution: DistributionResult = {
    shares: {
        'alice': 0.4,  // 40% of capacity
        'bob': 0.3,    // 30% of capacity
        'carol': 0.3   // 30% of capacity
    },
    method: 'custom',
    metadata: {
        timestamp: Date.now(),
        voteResults: { /* DAO voting data */ }
    }
};

// 2. Use unified allocation engine
const allocations = allocateWithDistribution(
    myPubKey,
    myCapacitySlots,
    myDistribution,
    allCommitments,
    { needsIndex }
);

// 3. Done! All the sophisticated slot matching, divisibility,
//    and remainder redistribution happens automatically
```

---

## Timeline

- ✅ **Phase 1**: Distribution module created (DONE)
- 🔄 **Phase 2**: Refactor allocation.ts (IN PROGRESS)
- ⏳ **Phase 3**: Update collective recognition
- ⏳ **Phase 4**: Testing & verification
- ⏳ **Phase 5**: Documentation & examples

**Estimated completion:** 1-2 days of focused work

---

## Notes

- All changes maintain backward compatibility
- Existing code continues to work unchanged
- New features are additive, not breaking
- Memoization preserved throughout

