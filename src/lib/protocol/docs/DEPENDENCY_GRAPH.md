# Dependency Graph & Parallelization Opportunities

## Current Reactive Chain

### Commitment Structure
```typescript
interface Commitment {
  capacity_slots?: AvailabilitySlot[];      // What they provide
  need_slots?: NeedSlot[];                  // What they need
  global_recognition_weights?: Record<...>; // Who they recognize
  global_mr_values?: Record<...>;           // Mutual recognition
  multi_dimensional_damping?: {...};        // Damping history
  itcStamp: ITCStamp;                       // Causality
  timestamp: number;                        // Wall clock
}
```

### What Depends on What?

```
networkCommitments (Map<pubKey, Commitment>)
  │
  ├─→ myMutualRecognition (derived)
  │   └─ Depends on: commitment.global_recognition_weights
  │   └─ Used by: myAllocationsAsProvider, commitment composition
  │
  ├─→ networkNeedsIndex (readable)
  │   └─ Depends on: commitment.need_slots
  │   └─ Used by: myAllocationsAsProvider (finding recipients)
  │
  ├─→ networkCapacityIndex (readable)
  │   └─ Depends on: commitment.capacity_slots
  │   └─ Used by: (future) finding providers for my needs
  │
  └─→ myAllocationsAsProvider (derived)
      └─ Depends on: myMutualRecognition + networkNeedsIndex + myCommitmentStore
      └─ Used by: UI, publishing allocation results
```

## Problem: Over-Triggering

### Scenario 1: Only Recognition Changed
```typescript
// Alice updates her recognition weights
newCommitment = {
  ...oldCommitment,
  global_recognition_weights: { /* changed */ },
  // need_slots: unchanged
  // capacity_slots: unchanged
}

// Current behavior:
networkCommitments.update(...);
  ↓
  ├─ myMutualRecognition ✅ SHOULD recalculate (recognition changed)
  ├─ networkNeedsIndex ❌ SHOULDN'T rebuild (needs unchanged)
  └─ networkCapacityIndex ❌ SHOULDN'T rebuild (capacity unchanged)
```

### Scenario 2: Only Needs Changed
```typescript
// Bob updates his needs
newCommitment = {
  ...oldCommitment,
  need_slots: [ /* changed */ ],
  // global_recognition_weights: unchanged
  // capacity_slots: unchanged
}

// Current behavior:
networkCommitments.update(...);
  ↓
  ├─ myMutualRecognition ❌ SHOULDN'T recalculate (recognition unchanged)
  ├─ networkNeedsIndex ✅ SHOULD rebuild (needs changed)
  └─ networkCapacityIndex ❌ SHOULDN'T rebuild (capacity unchanged)
```

### Scenario 3: Only Capacity Changed
```typescript
// Carol updates her capacity
newCommitment = {
  ...oldCommitment,
  capacity_slots: [ /* changed */ ],
  // global_recognition_weights: unchanged
  // need_slots: unchanged
}

// Current behavior:
networkCommitments.update(...);
  ↓
  ├─ myMutualRecognition ❌ SHOULDN'T recalculate (recognition unchanged)
  ├─ networkNeedsIndex ❌ SHOULDN'T rebuild (needs unchanged)
  └─ networkCapacityIndex ✅ SHOULD rebuild (capacity changed)
```

## Solution 1: Fine-Grained Staleness (Partial Updates)

### Add Field-Level Change Detection

```typescript
interface FieldChanges {
  recognitionChanged: boolean;
  needsChanged: boolean;
  capacityChanged: boolean;
  dampingChanged: boolean;
}

function detectChangedFields(
  existing: Commitment,
  incoming: Commitment
): FieldChanges {
  return {
    recognitionChanged: !deepEquals(
      existing.global_recognition_weights,
      incoming.global_recognition_weights
    ),
    needsChanged: !deepEquals(
      existing.need_slots,
      incoming.need_slots
    ),
    capacityChanged: !deepEquals(
      existing.capacity_slots,
      incoming.capacity_slots
    ),
    dampingChanged: !deepEquals(
      existing.multi_dimensional_damping,
      incoming.multi_dimensional_damping
    )
  };
}
```

### Selective Updates

```typescript
const changes = detectChangedFields(existingCommitment, commitment);

// Only update if something actually changed
if (changes.recognitionChanged || changes.needsChanged || 
    changes.capacityChanged || changes.dampingChanged) {
  
  // Update main store
  networkCommitments.update(...);
  
  // Trigger selective recalculations
  if (changes.recognitionChanged) {
    // myMutualRecognition will auto-recalculate (it's derived)
  }
  
  if (changes.needsChanged) {
    // Trigger networkNeedsIndex update
    scheduleNeedsIndexUpdate(pubKey, commitment);
  }
  
  if (changes.capacityChanged) {
    // Trigger networkCapacityIndex update
    scheduleCapacityIndexUpdate(pubKey, commitment);
  }
}
```

## Solution 2: Split Stores (Fine-Grained Architecture)

### Problem with Current Approach
```typescript
// ONE store for entire commitment
networkCommitments: Map<pubKey, Commitment>

// Any change → entire Map updates → ALL derived stores recalculate
```

### Alternative: Separate Stores

```typescript
// Split into independent stores
networkRecognitionWeights: Map<pubKey, GlobalRecognitionWeights>
networkNeedSlots: Map<pubKey, NeedSlot[]>
networkCapacitySlots: Map<pubKey, AvailabilitySlot[]>
networkDamping: Map<pubKey, MultiDimensionalDamping>

// Now updates are isolated:
// - Recognition change → only networkRecognitionWeights updates
//                     → only myMutualRecognition recalculates
// - Needs change → only networkNeedSlots updates
//               → only networkNeedsIndex rebuilds
// - Capacity change → only networkCapacitySlots updates
//                  → only networkCapacityIndex rebuilds
```

**Trade-off**:
- ✅ Fine-grained reactivity (only recalculate what changed)
- ✅ Parallel updates (recognition + needs + capacity independent)
- ❌ More stores to manage (4 instead of 1)
- ❌ Holster subscriptions (4 paths instead of 1?)
- ❌ Complexity (need to compose commitment from 4 stores)

## Solution 3: Parallel Index Updates

### Current Sequential Flow

```typescript
// Alice + Bob + Carol all update simultaneously
// Current: Sequential processing

await updateIndexForAlice();  // 10ms
await updateIndexForBob();    // 10ms
await updateIndexForCarol();  // 10ms
// Total: 30ms
```

### Parallel Processing

```typescript
// Process independent updates in parallel
await Promise.all([
  updateIndexForAlice(),   // 10ms ┐
  updateIndexForBob(),     // 10ms ├─ parallel
  updateIndexForCarol()    // 10ms ┘
]);
// Total: 10ms (3× faster!)
```

### Implementation

```typescript
const processPendingUpdates = async () => {
  if (pendingUpdates.size === 0) return;
  
  // Process all pending updates IN PARALLEL
  const updatePromises = Array.from(pendingUpdates.entries()).map(
    ([pubKey, commitment]) => 
      updateIndexForParticipant(pubKey, commitment, index, true)
  );
  
  await Promise.all(updatePromises);
  
  console.log(`[NEEDS-INDEX] Parallel updated ${pendingUpdates.size} participants`);
  pendingUpdates.clear();
  
  set({ ...index });
};
```

**Trade-off**:
- ✅ 3-10× faster for bursts (N participants updated)
- ✅ No architectural change needed
- ⚠️ Order not guaranteed (but doesn't matter - updates are independent)
- ⚠️ More memory (all updates in flight simultaneously)

## Solution 4: Memoization / Caching

### Cache Individual Calculations

```typescript
// Cache recognition weights per pubKey
const recognitionCache = new Map<pubKey, {
  weights: GlobalRecognitionWeights,
  computedAt: number
}>();

// In myMutualRecognition derived store:
for (const [theirPub, commitment] of $networkCommits.entries()) {
  const cached = recognitionCache.get(theirPub);
  
  // Skip if recognition unchanged since last computation
  if (cached && 
      deepEquals(cached.weights, commitment.global_recognition_weights)) {
    mutualRec[theirPub] = cached.mutualRecognition;
    continue; // ✅ Skip expensive computation
  }
  
  // Compute and cache
  const mr = computeMutualRecognition(...);
  recognitionCache.set(theirPub, {
    weights: commitment.global_recognition_weights,
    mutualRecognition: mr,
    computedAt: Date.now()
  });
  mutualRec[theirPub] = mr;
}
```

## Recommended Hybrid Approach

### Phase 1: Field-Level Change Detection (Low Complexity)

Add to existing `checkStaleness()`:

```typescript
interface StalenessCheckResult {
  shouldSkip: boolean;
  reason?: string;
  checkType?: 'itc' | 'timestamp' | 'semantic';
  changedFields?: string[]; // NEW: Which fields changed
}

// In isCommitmentEquivalent, track what changed:
function isCommitmentEquivalent(a: Commitment, b: Commitment): {
  equivalent: boolean;
  changedFields: string[];
} {
  const changedFields: string[] = [];
  
  if (!deepEquals(a.global_recognition_weights, b.global_recognition_weights)) {
    changedFields.push('recognition');
  }
  if (!deepEquals(a.need_slots, b.need_slots)) {
    changedFields.push('needs');
  }
  if (!deepEquals(a.capacity_slots, b.capacity_slots)) {
    changedFields.push('capacity');
  }
  
  return {
    equivalent: changedFields.length === 0,
    changedFields
  };
}
```

Then use `changedFields` to selectively trigger updates.

### Phase 2: Parallel Index Updates (Medium Complexity)

Change index updates from sequential to parallel:
- `Promise.all()` for batch updates
- Web Workers for heavy computations (future)

### Phase 3: Split Stores (High Complexity, High Gain)

If profiling shows coarse-grained reactivity is still a bottleneck:
- Split `networkCommitments` into separate stores
- Requires architectural refactor
- But enables perfect fine-grained reactivity

## Measurement Strategy

### Before Optimization
```typescript
console.time('commitment-update-alice');
// Alice's commitment arrives
console.timeEnd('commitment-update-alice');
// → 100ms (includes unnecessary recalculations)
```

### Measure Field Changes
```typescript
const changes = detectChangedFields(existing, incoming);
console.log('[FIELD-CHANGES]', {
  recognition: changes.recognitionChanged,
  needs: changes.needsChanged,
  capacity: changes.capacityChanged,
  // How many unnecessary recalculations?
  unnecessaryRecalcs: 
    (!changes.recognitionChanged && mutualRecReran ? 1 : 0) +
    (!changes.needsChanged && needsIndexReran ? 1 : 0) +
    (!changes.capacityChanged && capacityIndexReran ? 1 : 0)
});
```

### Expected Gains

| Scenario | Current | With Field Detection | Speedup |
|----------|---------|---------------------|---------|
| Recognition only changed | 100ms (all recalc) | 30ms (only MR) | 3.3× |
| Needs only changed | 100ms (all recalc) | 40ms (only needs index) | 2.5× |
| Capacity only changed | 100ms (all recalc) | 40ms (only capacity index) | 2.5× |
| All changed | 100ms | 100ms | 1× (no optimization) |

**Expected average**: 2-3× speedup (assuming 70% of updates are partial)

## Parallelization Opportunities

### Independent Computations (Can Run in Parallel)

```typescript
// These can ALL run in parallel:
await Promise.all([
  // Update recognition weights
  computeMutualRecognition($myWeights, $networkCommits),
  
  // Update needs index
  rebuildNeedsIndex($networkCommits),
  
  // Update capacity index
  rebuildCapacityIndex($networkCommits),
  
  // Update other derived state
  updateConvergenceMetrics($networkCommits)
]);
```

**Why they're independent**:
- Don't depend on each other's results
- Read from same source (`networkCommitments`)
- Write to different stores

### Dependent Computation (Must Be Sequential)

```typescript
// Must run AFTER mutual recognition computed:
const allocations = computeAllocations(
  $myMutualRecognition,  // Depends on previous step
  $networkNeedsIndex     // Depends on previous step
);
```

## Next Steps

1. **Profile current behavior**: Measure how often partial updates happen
2. **Implement field-level detection**: Low-hanging fruit, big gains
3. **Add parallel index updates**: Easy win for burst scenarios
4. **Monitor improvement**: Track skip rate per field
5. **Consider split stores**: Only if profiling shows it's still a bottleneck

Want me to implement the field-level change detection first?

