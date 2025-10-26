# v4 Quick Reference Cheatsheet

## Import

```typescript
import { 
  // Publishing
  publishMyCommitment,
  publishMyRecognitionWeights,
  
  // Reactive stores
  myAllocationsReactive,
  myPubKey,
  
  // State access
  getCurrentSystemState,
  
  // Debug
  logSystemState,
  logConvergenceMetrics,
  
  // Types
  type AllocationOperatorResult,
  type ConvergenceMetrics,
  type NeedState,
  type SystemState
} from '@/lib/commons/v4';
```

## Core Operations

### Publish Commitment
```typescript
await publishMyCommitment({
  need_slots: [...],
  capacity_slots: [...]
});
```

### Publish Recognition (Auto-Normalized)
```typescript
// Weights will be normalized to sum to 1.0
await publishMyRecognitionWeights({
  'pub1': 0.4,
  'pub2': 0.6
});
```

### Subscribe to Allocations
```typescript
myAllocationsReactive.subscribe((result) => {
  if (!result) return;
  
  // Access allocations
  const { allocations, convergenceMetrics, updatedNeedVector } = result;
  
  // Check metrics
  console.log('Heaven:', convergenceMetrics.heavenAchieved);
  console.log('Contraction k:', convergenceMetrics.contractionConstant);
  console.log('Freedom:', convergenceMetrics.freedomMetric);
});
```

## State Access

### Get System State
```typescript
const state = getCurrentSystemState();

console.log('Iteration:', state.iteration);
console.log('Need vector size:', state.needVector.size);

// Inspect needs
for (const [pubKey, needState] of state.needVector.entries()) {
  console.log(`${pubKey}: ${needState.residualNeed} / ${needState.maxNeed}`);
}
```

## Debug Functions

### In Browser Console
```javascript
// Log full system state
window.debugAllocationV4();

// Log convergence metrics
window.debugConvergenceV4(result);

// Get current state
window.getCurrentSystemStateV4();

// Normalize weights
window.normalizeWeightsV4({ a: 0.5, b: 0.3 });
```

### In Code
```typescript
import { logSystemState, logConvergenceMetrics } from '@/lib/commons/v4';

logSystemState();
logConvergenceMetrics(result.convergenceMetrics);
```

## Key Metrics

### Convergence Metrics Object
```typescript
interface ConvergenceMetrics {
  // System-level
  needVectorNorm: number;              // ||N⃗(t)||
  needVectorNormPrevious: number;      // ||N⃗(t-1)||
  contractionConstant: number;         // k where ||N⃗(t+1)|| ≤ k||N⃗(t)||
  
  // Convergence
  isConverged: boolean;                // ||N⃗(t)|| < ε
  iterationsToConvergence: number;     // Estimated remaining
  convergenceRate: number;             // Exponential rate
  
  // Timing
  responseLatency: number;             // ms
  iterationFrequency: number;          // Hz
  
  // Heaven
  heavenAchieved: boolean;             // ∀i: N_i = 0
  percentNeedsMet: number;             // %
  
  // Freedom
  freedomMetric: number;               // ||N⃗(t)||
}
```

### Quick Checks
```typescript
// Is system converged?
if (result.convergenceMetrics.isConverged) { ... }

// Is heaven achieved?
if (result.convergenceMetrics.heavenAchieved) { ... }

// Is contracting?
if (result.convergenceMetrics.contractionConstant < 1.0) { ... }

// What % of needs met?
const pct = result.convergenceMetrics.percentNeedsMet;
```

## Common Patterns

### Monitor Convergence
```typescript
myAllocationsReactive.subscribe((result) => {
  if (!result) return;
  
  const { needVectorNorm, contractionConstant, heavenAchieved } = result.convergenceMetrics;
  
  if (heavenAchieved) {
    console.log('🌟 Heaven achieved!');
  } else {
    console.log(`||N⃗|| = ${needVectorNorm.toFixed(3)}, k = ${contractionConstant.toFixed(3)}`);
  }
});
```

### Track Need Reduction
```typescript
let needHistory: number[] = [];

myAllocationsReactive.subscribe((result) => {
  if (!result) return;
  
  const norm = result.convergenceMetrics.needVectorNorm;
  needHistory.push(norm);
  
  if (needHistory.length > 10) {
    needHistory.shift();
  }
  
  console.log('Need trajectory:', needHistory.map(n => n.toFixed(2)));
});
```

### Verify Contraction
```typescript
let prevNorm = Infinity;

myAllocationsReactive.subscribe((result) => {
  if (!result) return;
  
  const norm = result.convergenceMetrics.needVectorNorm;
  
  if (norm > prevNorm) {
    console.warn('⚠️ Contraction property violated!');
  } else {
    console.log('✅ Contracting:', prevNorm.toFixed(3), '→', norm.toFixed(3));
  }
  
  prevNorm = norm;
});
```

## UI Integration

### Svelte Component
```svelte
<script lang="ts">
  import { myAllocationsReactive } from '@/lib/commons/v4';
  
  $: result = $myAllocationsReactive;
  $: metrics = result?.convergenceMetrics;
  $: allocations = result?.allocations;
</script>

{#if result && metrics}
  <div class="convergence-panel">
    <h3>System Status</h3>
    
    <div class="metric">
      <span>Heaven:</span>
      <span>{metrics.heavenAchieved ? '✅' : '⏳'}</span>
    </div>
    
    <div class="metric">
      <span>Needs Met:</span>
      <span>{metrics.percentNeedsMet.toFixed(1)}%</span>
    </div>
    
    <div class="metric">
      <span>Contraction k:</span>
      <span>{metrics.contractionConstant.toFixed(3)}</span>
    </div>
    
    <div class="metric">
      <span>Freedom ||N⃗||:</span>
      <span>{metrics.freedomMetric.toFixed(3)}</span>
    </div>
    
    {#if metrics.iterationsToConvergence !== null}
      <div class="metric">
        <span>Iterations to convergence:</span>
        <span>{metrics.iterationsToConvergence}</span>
      </div>
    {/if}
  </div>
{/if}
```

## Equation Reference

| Equation | Code Function | Line |
|----------|--------------|------|
| `N_i(t+1) = max(0, N_i(t) - A)` | `applyAllocationOperator()` | 441 |
| `MR(A,B) = min(R_A(B), R_B(A))` | `computeMutualRecognition()` | 286 |
| `Σ_i R_A(i) = 1.0` | `normalizeRecognitionWeights()` | 159 |
| `A(j→i) ≤ N_i` | Allocation capping | 774, 871 |
| `α ∈ {0.5, 1.0, 0.8}` | `computeDampingFactor()` | 355 |
| `Heaven ⟺ ∀i: N_i = 0` | `checkHeavenCondition()` | 217 |
| `Freedom = ||N⃗||` | `computeNeedVectorNorm()` | 193 |

## Constants

```typescript
CONVERGENCE_EPSILON = 0.001         // ε for convergence
DENOMINATOR_FLOOR = 0.0001          // ε for Lipschitz continuity
DAMPING_OSCILLATING = 0.5           // α when oscillating
DAMPING_SMOOTH = 1.0                // α when smooth
DAMPING_MODERATE = 0.8              // α otherwise
STALE_THRESHOLD_MS = 60000          // 60 seconds
DAMPING_HISTORY_WINDOW_MS = 30000   // 30 seconds
DAMPING_HISTORY_MAX_COUNT = 3       // Last 3 entries
```

## Testing Snippets

### Test Normalization
```typescript
import { normalizeRecognitionWeights } from '@/lib/commons/v4';

const raw = { a: 0.5, b: 0.3, c: 0.4 }; // Sum = 1.2
const norm = normalizeRecognitionWeights(raw);
const sum = Object.values(norm).reduce((a, b) => a + b, 0);
console.assert(Math.abs(sum - 1.0) < 0.0001, 'Normalization failed!');
```

### Test Contraction
```typescript
let prevNorm = Infinity;
myAllocationsReactive.subscribe((result) => {
  if (!result) return;
  const norm = result.convergenceMetrics.needVectorNorm;
  console.assert(norm <= prevNorm, 'Not contracting!');
  prevNorm = norm;
});
```

### Test Heaven
```typescript
import { checkHeavenCondition, getCurrentSystemState, CONVERGENCE_EPSILON } from '@/lib/commons/v4';

myAllocationsReactive.subscribe((result) => {
  if (!result) return;
  
  if (result.convergenceMetrics.heavenAchieved) {
    const state = getCurrentSystemState();
    for (const need of state.needVector.values()) {
      console.assert(need.residualNeed < CONVERGENCE_EPSILON, 'Heaven but needs remain!');
    }
    console.log('🌟 Heaven verified!');
  }
});
```

## Migration from v2

```typescript
// v2 code
myAllocationsReactive.subscribe((allocations) => {
  if (!allocations) return;
  console.log(allocations.recipient_totals);
});

// v4 code
myAllocationsReactive.subscribe((result) => {
  if (!result) return;
  console.log(result.allocations.recipient_totals);  // Note: .allocations
  console.log(result.convergenceMetrics.heavenAchieved);  // NEW
});
```

## Troubleshooting

### Issue: `result.allocations` is undefined
**Fix**: Access through `.allocations` property
```typescript
// ❌ Wrong
console.log(result.slot_allocations);

// ✅ Correct
console.log(result.allocations.slot_allocations);
```

### Issue: Weights don't sum to 1.0
**Fix**: They're auto-normalized, check normalized values
```typescript
const normalized = normalizeRecognitionWeights(weights);
```

### Issue: Metrics show NaN
**Fix**: Need vector might be empty
```typescript
const state = getCurrentSystemState();
if (state.needVector.size === 0) {
  console.warn('No needs in system');
}
```

## Performance Tips

```typescript
// Don't log every iteration
let shouldLog = false;
myAllocationsReactive.subscribe((result) => {
  if (!result || !shouldLog) return;
  logConvergenceMetrics(result.convergenceMetrics);
  shouldLog = false;
});

// Log periodically
setInterval(() => { shouldLog = true; }, 5000);
```

## Summary

### The Three Key Theorems
1. **Contractiveness**: `||N⃗(t+1)|| ≤ k||N⃗(t)||` where `k < 1`
2. **Banach Fixed-Point**: System converges to unique `N⃗* = 0⃗`
3. **Exponential Convergence**: Reaches heaven in ~5-20 iterations

### The Central Proof
```
Given: MR > 0, Σ_j C_j ≥ Σ_i N_i
Then: lim(t→∞) N⃗(t) = 0⃗
```

**Love is not utopian. Love is contractive. ∴ Love converges.**

**QED.** ∎

