# Migration Guide: v2 → v4

## Overview

Version 4 extends v2 with explicit need state tracking, full contraction mapping implementation, and comprehensive convergence metrics. This guide helps migrate from v2 to v4.

## Breaking Changes

### 1. Recognition Weights are Auto-Normalized

**v2**: Used raw recognition weights
```typescript
// v2: Weights could sum to anything
const weights = { 'pub1': 0.5, 'pub2': 0.3 }; // Sum = 0.8 ❌
```

**v4**: Enforces `Σ_i R_A(i) = 1.0`
```typescript
// v4: Auto-normalized to sum to 1.0
const weights = { 'pub1': 0.5, 'pub2': 0.3 }; // Normalized to { 'pub1': 0.625, 'pub2': 0.375 } ✅
```

**Migration Action**: 
- No code changes needed
- `publishMyRecognitionWeights()` automatically normalizes
- Existing weights will be normalized on first v4 publish

### 2. Allocation Result Structure Changed

**v2**: Returned `TwoTierAllocationState`
```typescript
const allocations: TwoTierAllocationState = {
  slot_denominators: {...},
  slot_allocations: [...],
  recipient_totals: {...},
  timestamp: number,
  converged?: boolean,
  itcStamp?: ITCStamp
};
```

**v4**: Returns `AllocationOperatorResult` with metrics
```typescript
const result: AllocationOperatorResult = {
  allocations: TwoTierAllocationState,        // Same as v2
  updatedNeedVector: Map<string, NeedState>,  // NEW: N⃗(t+1)
  convergenceMetrics: ConvergenceMetrics      // NEW: Full metrics
};
```

**Migration Action**:
```typescript
// v2 code
myAllocationsReactive.subscribe((allocations) => {
  if (!allocations) return;
  console.log(allocations.recipient_totals);
});

// v4 code
myAllocationsReactive.subscribe((result) => {
  if (!result) return;
  console.log(result.allocations.recipient_totals);        // Access through .allocations
  console.log(result.convergenceMetrics.heavenAchieved);   // NEW: Heaven condition
  console.log(result.updatedNeedVector);                   // NEW: Need states
});
```

## New Features (Non-Breaking)

### 1. System State Access

```typescript
import { getCurrentSystemState } from '@/lib/commons/v4';

const state = getCurrentSystemState();
console.log('Iteration:', state.iteration);
console.log('Need vector size:', state.needVector.size);
console.log('Capacity vector size:', state.capacityVector.size);

// Inspect individual needs
for (const [pubKey, needState] of state.needVector.entries()) {
  console.log(`${pubKey}: ${needState.residualNeed} / ${needState.maxNeed}`);
}
```

### 2. Convergence Metrics

```typescript
import { myAllocationsReactive, logConvergenceMetrics } from '@/lib/commons/v4';

myAllocationsReactive.subscribe((result) => {
  if (!result) return;
  
  const metrics = result.convergenceMetrics;
  
  // Check convergence
  if (metrics.isConverged) {
    console.log('✅ System converged!');
  }
  
  // Monitor contraction
  console.log('Contraction constant k:', metrics.contractionConstant);
  console.log('Need vector norm:', metrics.needVectorNorm);
  
  // Heaven condition
  if (metrics.heavenAchieved) {
    console.log('🌟 Heaven achieved! All needs met.');
  }
  
  // Freedom metric (E45)
  console.log('Freedom (||N⃗||):', metrics.freedomMetric);
  
  // Log all metrics
  logConvergenceMetrics(metrics);
});
```

### 3. Heaven & Freedom Tracking

```typescript
import { 
  checkHeavenCondition, 
  computePercentNeedsMet,
  getCurrentSystemState 
} from '@/lib/commons/v4';

const state = getCurrentSystemState();

// Check if all needs are met (E41)
const heaven = checkHeavenCondition(state.needVector);
console.log('Heaven achieved:', heaven);

// Get percentage of needs met
const pct = computePercentNeedsMet(state.needVector);
console.log('Needs met:', pct.toFixed(1) + '%');
```

### 4. Manual Need State Initialization

```typescript
import { initializeNeedState, initializeCapacityState } from '@/lib/commons/v4';

// Initialize from commitment
const needState = initializeNeedState('pubkey123', commitment);
console.log('Max need:', needState.maxNeed);
console.log('Residual need:', needState.residualNeed);

const capacityState = initializeCapacityState('pubkey456', commitment);
console.log('Max capacity:', capacityState.maxCapacity);
console.log('Available capacity:', capacityState.availableCapacity);
```

### 5. Weight Normalization Utilities

```typescript
import { 
  normalizeRecognitionWeights, 
  validateRecognitionWeights 
} from '@/lib/commons/v4';

// Normalize weights
const raw = { 'pub1': 0.5, 'pub2': 0.3, 'pub3': 0.4 }; // Sum = 1.2
const normalized = normalizeRecognitionWeights(raw);
// Returns: { 'pub1': 0.417, 'pub2': 0.25, 'pub3': 0.333 } (sum = 1.0)

// Validate weights
const isValid = validateRecognitionWeights(normalized);
console.log('Valid:', isValid); // true
```

## API Compatibility Matrix

| Feature | v2 | v4 | Compatible? |
|---------|----|----|-------------|
| ITC stamps | ✅ | ✅ | ✅ Yes |
| Damping (E12-E15) | ✅ | ✅ | ✅ Yes |
| Two-tier allocation | ✅ | ✅ | ✅ Yes |
| Slot-native matching | ✅ | ✅ | ✅ Yes |
| Reactive allocations | ✅ | ✅ | ⚠️ Changed structure |
| Recognition weights | ✅ | ✅ | ⚠️ Auto-normalized in v4 |
| Need state tracking | ❌ | ✅ | ➕ New in v4 |
| Convergence metrics | Partial | ✅ | ➕ Enhanced in v4 |
| Heaven condition | ❌ | ✅ | ➕ New in v4 |
| Freedom metric | ❌ | ✅ | ➕ New in v4 |

## Step-by-Step Migration

### Step 1: Update Imports

```typescript
// Before (v2)
import { 
  publishMyCommitment,
  myAllocationsReactive 
} from '@/lib/commons/v2/algorithm.svelte';

// After (v4)
import { 
  publishMyCommitment,
  myAllocationsReactive 
} from '@/lib/commons/v4'; // Uses index.ts
```

### Step 2: Update Allocation Subscription

```typescript
// Before (v2)
myAllocationsReactive.subscribe((allocations) => {
  if (!allocations) return;
  
  // Use allocations directly
  const totals = allocations.recipient_totals;
  const converged = allocations.converged;
});

// After (v4)
myAllocationsReactive.subscribe((result) => {
  if (!result) return;
  
  // Access through .allocations
  const totals = result.allocations.recipient_totals;
  const converged = result.convergenceMetrics.isConverged;
  
  // NEW: Access metrics and need vector
  const heaven = result.convergenceMetrics.heavenAchieved;
  const needVector = result.updatedNeedVector;
});
```

### Step 3: Handle Recognition Weights

```typescript
// Before (v2) - weights could be unnormalized
const weights = {
  'pub1': 0.5,
  'pub2': 0.3
  // Sum = 0.8, not normalized
};
await publishMyRecognitionWeights(weights);

// After (v4) - auto-normalized
const weights = {
  'pub1': 0.5,
  'pub2': 0.3
  // Will be normalized to sum to 1.0
};
await publishMyRecognitionWeights(weights); // Handles normalization internally
```

### Step 4: Add Convergence Monitoring (Optional)

```typescript
// NEW in v4: Monitor convergence
import { logConvergenceMetrics } from '@/lib/commons/v4';

myAllocationsReactive.subscribe((result) => {
  if (!result) return;
  
  // Log convergence metrics to console
  logConvergenceMetrics(result.convergenceMetrics);
  
  // Or access specific metrics
  const { contractionConstant, heavenAchieved, freedomMetric } = result.convergenceMetrics;
  
  if (heavenAchieved) {
    console.log('🌟 All needs met!');
  }
});
```

### Step 5: Update UI Components (if applicable)

```svelte
<script lang="ts">
  import { myAllocationsReactive } from '@/lib/commons/v4';
  
  // v2 pattern
  // $: allocations = $myAllocationsReactive;
  
  // v4 pattern
  $: result = $myAllocationsReactive;
  $: allocations = result?.allocations;
  $: metrics = result?.convergenceMetrics;
  $: needVector = result?.updatedNeedVector;
</script>

{#if result}
  <div>
    <h2>Allocations</h2>
    <p>Recipients: {Object.keys(allocations.recipient_totals).length}</p>
    
    <!-- NEW: Convergence info -->
    <h2>Convergence</h2>
    <p>Contraction k: {metrics.contractionConstant.toFixed(3)}</p>
    <p>Heaven: {metrics.heavenAchieved ? '✅' : '⏳'}</p>
    <p>Needs met: {metrics.percentNeedsMet.toFixed(1)}%</p>
    <p>Freedom: {metrics.freedomMetric.toFixed(3)}</p>
  </div>
{/if}
```

## Backward Compatibility

### Can I use v2 and v4 side-by-side?

**Yes**, but with caveats:

1. **ITC stamps are compatible** - Both use the same ITC implementation
2. **Commitments are compatible** - Both use same schema
3. **Recognition weights** - v4 will normalize, v2 won't
4. **Allocation results** - Different structures (see above)

### Example: Gradual Migration

```typescript
// Keep v2 for production
import * as v2 from '@/lib/commons/v2/algorithm.svelte';

// Add v4 for testing/comparison
import * as v4 from '@/lib/commons/v4';

// Use v2 for actual allocations
v2.myAllocationsReactive.subscribe((allocations) => {
  // Production logic
});

// Use v4 for monitoring/metrics
v4.myAllocationsReactive.subscribe((result) => {
  // Log metrics for comparison
  if (result) {
    console.log('[V4 METRICS]', result.convergenceMetrics);
  }
});
```

## Testing

### Test Recognition Weight Normalization

```typescript
import { normalizeRecognitionWeights } from '@/lib/commons/v4';

// Test 1: Normal weights
const weights1 = { a: 0.5, b: 0.3, c: 0.2 }; // Sum = 1.0
const norm1 = normalizeRecognitionWeights(weights1);
console.assert(Math.abs(Object.values(norm1).reduce((a,b) => a+b) - 1.0) < 0.0001);

// Test 2: Unnormalized weights
const weights2 = { a: 0.5, b: 0.3 }; // Sum = 0.8
const norm2 = normalizeRecognitionWeights(weights2);
console.assert(Math.abs(Object.values(norm2).reduce((a,b) => a+b) - 1.0) < 0.0001);

// Test 3: Zero sum
const weights3 = { a: 0, b: 0 }; // Sum = 0
const norm3 = normalizeRecognitionWeights(weights3);
console.assert(Object.values(norm3).every(w => Math.abs(w - 0.5) < 0.0001)); // Equal weights
```

### Test Contraction Property

```typescript
import { 
  getCurrentSystemState, 
  computeNeedVectorNorm,
  myAllocationsReactive 
} from '@/lib/commons/v4';

let previousNorm = Infinity;

myAllocationsReactive.subscribe((result) => {
  if (!result) return;
  
  const currentNorm = result.convergenceMetrics.needVectorNorm;
  
  // Theorem 1: ||N⃗(t+1)|| ≤ ||N⃗(t)||
  console.assert(currentNorm <= previousNorm, 'Contraction property violated!');
  
  // Theorem 1: k < 1.0 (strict contraction when allocating)
  if (Object.keys(result.allocations.recipient_totals).length > 0) {
    console.assert(result.convergenceMetrics.contractionConstant < 1.0);
  }
  
  previousNorm = currentNorm;
});
```

### Test Heaven Condition

```typescript
import { 
  checkHeavenCondition,
  getCurrentSystemState,
  CONVERGENCE_EPSILON
} from '@/lib/commons/v4';

myAllocationsReactive.subscribe((result) => {
  if (!result) return;
  
  const state = getCurrentSystemState();
  const heaven = checkHeavenCondition(state.needVector);
  
  // Heaven ⟺ all needs near zero
  if (heaven) {
    for (const needState of state.needVector.values()) {
      console.assert(needState.residualNeed < CONVERGENCE_EPSILON);
    }
    console.log('🌟 Heaven condition verified!');
  }
});
```

## Performance Considerations

### v4 Overhead

v4 adds minimal overhead to v2:

1. **Need vector operations**: O(n) per iteration
2. **Norm computation**: O(n) using cached values
3. **Metrics computation**: O(1) arithmetic
4. **Memory**: ~100 bytes per participant for need state

### Optimization Tips

```typescript
// Don't compute metrics every frame if not needed
let shouldLogMetrics = false;

myAllocationsReactive.subscribe((result) => {
  if (!result) return;
  
  // Only log metrics occasionally
  if (shouldLogMetrics) {
    logConvergenceMetrics(result.convergenceMetrics);
    shouldLogMetrics = false;
  }
});

// Enable periodic logging
setInterval(() => {
  shouldLogMetrics = true;
}, 5000); // Every 5 seconds
```

## Troubleshooting

### Issue: Recognition weights don't sum to 1.0

**Solution**: v4 auto-normalizes. Check the normalized values:

```typescript
const weights = { a: 0.5, b: 0.3 };
const normalized = normalizeRecognitionWeights(weights);
console.log('Sum:', Object.values(normalized).reduce((a,b) => a+b)); // Should be 1.0
```

### Issue: `result.allocations` is undefined

**Solution**: Check if you're accessing the right property:

```typescript
// ❌ Wrong
myAllocationsReactive.subscribe((allocations) => {
  console.log(allocations.slot_allocations); // undefined in v4
});

// ✅ Correct
myAllocationsReactive.subscribe((result) => {
  console.log(result.allocations.slot_allocations); // Works
});
```

### Issue: Convergence metrics show NaN

**Solution**: Need vector might be empty. Check system state:

```typescript
const state = getCurrentSystemState();
if (state.needVector.size === 0) {
  console.warn('No participants with needs');
}
```

## Summary

| Aspect | Action Required |
|--------|-----------------|
| Import paths | Update to `v4/` or `v4/index` |
| Allocation subscriptions | Access through `.allocations` property |
| Recognition weights | None (auto-normalized) |
| New features | Optional - add metrics monitoring |
| Performance | No changes needed |
| Testing | Add contraction property tests |

**Migration time estimate**: 30-60 minutes for typical codebase

**Risk level**: Low (mostly additive changes)

**Rollback strategy**: Keep v2 imports, switch back if needed

