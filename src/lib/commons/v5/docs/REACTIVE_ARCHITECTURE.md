# V5 Reactive Architecture

## Overview

V5 uses a **reactive data flow** where source stores automatically update derived stores, creating an elegant and maintainable architecture.

## Architecture Diagram

```
┌────────────────────────────────────────────────────────────────┐
│ LAYER 1: SOURCE STORES (User Edits, Persisted to Holster)     │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│  myRecognitionTreeStore                                        │
│  ├─ Path: trees/recognition_tree                               │
│  ├─ Type: RootNode                                             │
│  └─ What: Tree structure with weighted contributors            │
│                                                                 │
│  myNeedSlotsStore                                              │
│  ├─ Path: allocation/need_slots                                │
│  ├─ Type: NeedSlot[]                                           │
│  └─ What: Array of what I need from commons                    │
│                                                                 │
│  myCapacitySlotsStore                                          │
│  ├─ Path: allocation/capacity_slots                            │
│  ├─ Type: AvailabilitySlot[]                                   │
│  └─ What: Array of what I can provide to commons               │
│                                                                 │
└────────────────────────────────────────────────────────────────┘
                            ↓
┌────────────────────────────────────────────────────────────────┐
│ LAYER 2: DERIVED STORES (Auto-computed, Never Persisted)      │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│  myRecognitionWeights (derived from tree)                      │
│  ├─ Computation: sharesOfGeneralFulfillmentMap(tree, {})       │
│  ├─ Type: GlobalRecognitionWeights                             │
│  └─ What: { alice: 0.3, bob: 0.4, carol: 0.3 }                 │
│                                                                 │
│  [Future: myMutualRecognition - derived from weights]          │
│  [Future: myActiveNeeds - derived from needs + damping]        │
│                                                                 │
└────────────────────────────────────────────────────────────────┘
                            ↓
┌────────────────────────────────────────────────────────────────┐
│ LAYER 3: COMPOSITION (Manual or Auto, Published to Network)   │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│  myCommitmentStore                                             │
│  ├─ Path: allocation/commitment                                │
│  ├─ Type: Commitment                                           │
│  ├─ Composed from:                                             │
│  │  ├─ need_slots ← myNeedSlotsStore                           │
│  │  ├─ capacity_slots ← myCapacitySlotsStore                   │
│  │  ├─ global_recognition_weights ← myRecognitionWeights       │
│  │  ├─ multi_dimensional_damping ← stateful (preserved)        │
│  │  └─ itcStamp ← incremental (preserved)                      │
│  └─ What: Complete commitment published to network             │
│                                                                 │
└────────────────────────────────────────────────────────────────┘
```

## Data Flow Examples

### Example 1: User Edits Recognition Tree

```typescript
// 1. User adds a contributor to tree
const tree = get(myRecognitionTreeStore);
addChild(tree, 'healthcare', 'Healthcare', 100, 
  [{ id: 'dr_smith', points: 60 }, { id: 'nurse_alice', points: 40 }]
);
myRecognitionTreeStore.set(tree);

// 2. Derived store auto-updates (reactive!)
// myRecognitionWeights → { dr_smith: 0.6, nurse_alice: 0.4 }

// 3. Optionally compose commitment (manual or auto)
const commitment = composeCommitmentFromSources();
myCommitmentStore.set(commitment);

// 4. Commitment auto-persists to Holster
// Network sees: { global_recognition_weights: { dr_smith: 0.6, ... } }
```

### Example 2: User Adds a Need

```typescript
// 1. User adds a need slot
const needs = get(myNeedSlotsStore) || [];
needs.push({
  id: 'need-1',
  need_type_id: 'food',
  quantity: 10,
  name: 'Meals needed',
  // ... other fields
});
myNeedSlotsStore.set(needs);

// 2. Compose commitment (includes new need)
const commitment = composeCommitmentFromSources();
myCommitmentStore.set(commitment);

// 3. Network sees updated commitment with new need
```

## Store Types

### Source Stores (Writable, Persisted)

These are the **source of truth** that users directly edit:

- `myRecognitionTreeStore` - Tree structure
- `myNeedSlotsStore` - Need slots array
- `myCapacitySlotsStore` - Capacity slots array

**Characteristics:**
- ✅ User editable
- ✅ Persisted to Holster
- ✅ Have their own Holster paths
- ✅ Survive logout/login

### Derived Stores (Readable, Computed)

These are **automatically computed** from source stores:

- `myRecognitionWeights` - Computed from tree

**Characteristics:**
- ❌ Not user editable (computed)
- ❌ Not persisted (recomputed on load)
- ✅ Reactive (auto-update when sources change)
- ✅ Type-safe (schema validated)

### Composition Stores (Writable, Published)

These **combine multiple sources** into publishable data:

- `myCommitmentStore` - Composed from tree + needs + capacity

**Characteristics:**
- ⚠️ Semi-derived (composed from sources but has stateful parts)
- ✅ Persisted to Holster
- ✅ Published to network
- ✅ Can preserve state (damping, ITC)

## Composition Strategies

### Strategy 1: Manual Composition (Explicit Control)

```typescript
// User makes edits to sources
myRecognitionTreeStore.set(newTree);
myNeedSlotsStore.set(newNeeds);

// Explicitly compose and publish when ready
const commitment = composeCommitmentFromSources();
if (commitment) {
  myCommitmentStore.set(commitment);
}
```

**Pros:**
- Explicit control over when to publish
- Can batch multiple edits
- Can add custom logic before publishing

**Cons:**
- Must remember to compose manually
- Could forget to update commitment

### Strategy 2: Auto Composition (Reactive)

```typescript
// Enable auto-composition (call once on app start)
const unsubscribe = enableAutoCommitmentComposition();

// Now any source edit auto-updates commitment!
myRecognitionTreeStore.set(newTree); // → commitment auto-updates
myNeedSlotsStore.set(newNeeds);      // → commitment auto-updates
```

**Pros:**
- Fully reactive
- Never forget to update commitment
- Minimal boilerplate

**Cons:**
- Less control over publish timing
- May publish intermediate states

### Strategy 3: Hybrid (Recommended)

```typescript
// Auto-compose for quick edits
enableAutoCommitmentComposition();

// But manually control for sensitive operations
function publishFinalCommitment() {
  const commitment = composeCommitmentFromSources();
  if (commitment) {
    // Add final touches (ITC, damping, etc.)
    await publishMyCommitment(commitment);
  }
}
```

## Helper Functions

### Composition

```typescript
// Build commitment from current source stores
const commitment = composeCommitmentFromSources();

// Enable reactive auto-composition
const unsubscribe = enableAutoCommitmentComposition();
```

### Source Store Access

```typescript
// Get current values
const tree = get(myRecognitionTreeStore);
const needs = get(myNeedSlotsStore);
const capacity = get(myCapacitySlotsStore);
const weights = get(myRecognitionWeights); // derived!

// Set new values
myRecognitionTreeStore.set(newTree);
myNeedSlotsStore.set(newNeeds);
myCapacitySlotsStore.set(newCapacity);
```

## Initialization

```typescript
// Initialize all stores (call after Holster auth)
initializeAllocationStores();

// Optionally enable auto-composition
const unsubscribe = enableAutoCommitmentComposition();

// On logout
unsubscribe(); // Stop auto-composition
await cleanupAllocationStores(); // Cleanup stores
```

## Benefits of This Architecture

### 1. **Separation of Concerns**
- Tree editing is independent of commitment publishing
- Need/capacity management is independent of recognition
- Each store has a single responsibility

### 2. **Reactivity**
- Recognition weights auto-update when tree changes
- Commitment can auto-update when any source changes
- No manual wiring needed

### 3. **Testability**
- Each store can be tested independently
- Derived computations are pure functions
- Easy to mock source stores

### 4. **Debuggability**
- Clear data flow: source → derived → composed
- Each layer is inspectable
- Console logs at each transformation

### 5. **Type Safety**
- Each store has a schema
- Zod validation at boundaries
- TypeScript types everywhere

### 6. **Performance**
- Derived stores only recompute when sources change
- Debouncing prevents excessive persistence
- Incremental updates via reactive subscriptions

## Migration from Old Architecture

### Before (Manual Everything):
```typescript
// Had to manually:
1. Edit tree
2. Run sharesOfGeneralFulfillmentMap()
3. Manually build commitment
4. Manually persist
```

### After (Reactive):
```typescript
// Just edit sources:
myRecognitionTreeStore.set(newTree);
// Everything else is automatic!
```

## Future Enhancements

### Potential Derived Stores:

1. **`myMutualRecognition`** - Derived from my weights + network weights
   ```typescript
   derived([myRecognitionWeights, networkCommitments], ...)
   ```

2. **`myActiveNeeds`** - Derived from needs + damping
   ```typescript
   derived([myNeedSlotsStore, dampingFactors], ...)
   ```

3. **`myAllocationsReceived`** - Derived from network commitments
   ```typescript
   derived([networkCommitments, myPublicKey], ...)
   ```

4. **`myConversionMetrics`** - Derived from system state
   ```typescript
   derived([myNeeds, previousNeeds], ...)
   ```

### Potential Optimizations:

1. **Batched Updates** - Collect multiple source changes before recomputing
2. **Selective Composition** - Only recompute affected parts
3. **Lazy Derivation** - Compute only when subscribed
4. **Memoization** - Cache expensive computations

## Best Practices

### DO:
- ✅ Edit source stores directly
- ✅ Subscribe to derived stores for display
- ✅ Use `composeCommitmentFromSources()` for explicit control
- ✅ Enable auto-composition for convenience
- ✅ Add console.log at transformation boundaries

### DON'T:
- ❌ Directly edit derived stores (they're read-only)
- ❌ Manually compute recognition weights (use derived store)
- ❌ Bypass source stores (always go through stores)
- ❌ Skip store validation (always use schemas)
- ❌ Forget to initialize stores (call `initializeAllocationStores()`)

## Debugging

### Check Source Stores:
```typescript
console.log('Tree:', get(myRecognitionTreeStore));
console.log('Needs:', get(myNeedSlotsStore));
console.log('Capacity:', get(myCapacitySlotsStore));
```

### Check Derived Stores:
```typescript
console.log('Weights:', get(myRecognitionWeights));
```

### Check Commitment:
```typescript
console.log('Commitment:', get(myCommitmentStore));
```

### Trace Composition:
```typescript
const commitment = composeCommitmentFromSources();
console.log('Composed:', commitment);
```

## Summary

V5's reactive architecture provides:
- **Clear data flow** (source → derived → composed)
- **Automatic updates** (reactive subscriptions)
- **Type safety** (schemas everywhere)
- **Debuggability** (each layer inspectable)
- **Flexibility** (manual or auto composition)

The result is a maintainable, testable, and intuitive system! 🎉

