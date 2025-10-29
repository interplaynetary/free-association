# 🎯 V5 Slots Architecture Simplification - DERIVED FROM COMMITMENT

## **The Insight**

You discovered a critical architectural redundancy: if `myCommitmentStore` already contains slots, why persist them separately?

**The answer: We shouldn't!**

## **Before (Redundant)**

```
myNeedSlotsStore → persist to 'allocation/need_slots'
                ↓
          myCommitmentStore → persist to 'allocation/commitment' (includes same slots!)
```

**Problems:**
1. ❌ **Data duplication** - Same data stored twice
2. ❌ **Sync issues** - Can get out of sync
3. ❌ **Persistence bugs** - Commitment had correct data, slot stores didn't
4. ❌ **Infinite loops** - Auto-composition subscribed to slots, which triggered re-composition

## **After (Simplified)**

```
myCommitmentStore → persist ONCE to 'allocation/commitment'
         ↓
myNeedSlotsStore (DERIVED, read-only)
myCapacitySlotsStore (DERIVED, read-only)
```

**Benefits:**
1. ✅ **Single source of truth** - Commitment is authoritative
2. ✅ **Always consistent** - Derived stores always match commitment
3. ✅ **Fewer bugs** - One place to update, one place to go wrong
4. ✅ **Simpler code** - Less initialization, less cleanup, less subscription management
5. ✅ **No infinite loops** - Auto-composition only reacts to recognition changes

## **Implementation Changes**

### **1. Slot Stores → Derived**

```typescript
// Before: Separate persistent stores
export const myNeedSlotsStore = createStore({
	holsterPath: 'allocation/need_slots',
	schema: z.array(NeedSlotSchema)
});

// After: Derived from commitment
export const myNeedSlotsStore: Readable<NeedSlot[] | null> = derived(
	[myCommitmentStore],
	([$commitment]) => $commitment?.need_slots || null
);
```

### **2. Helper Functions for Updates**

Since slot stores are now read-only, we provide helper functions:

```typescript
/**
 * Update need slots
 * Updates the commitment directly
 */
export function setMyNeedSlots(needSlots: NeedSlot[]) {
	const current = get(myCommitmentStore);
	const recognitionWeights = get(myRecognitionWeights);
	const mutualRecognition = get(myMutualRecognition);
	
	const updated: Commitment = {
		need_slots: needSlots,  // ← Update slots
		capacity_slots: current?.capacity_slots || [],
		global_recognition_weights: recognitionWeights,
		global_mr_values: mutualRecognition,
		multi_dimensional_damping: current?.multi_dimensional_damping,
		itcStamp: getMergedITCStamp(current?.itcStamp),
		timestamp: Date.now()
	};
	
	myCommitmentStore.set(updated);
}
```

### **3. Simplified Initialization**

```typescript
// Before: Initialize 4 stores
export function initializeAllocationStores() {
	myRecognitionTreeStore.initialize();
	myNeedSlotsStore.initialize();  // ← No longer needed
	myCapacitySlotsStore.initialize();  // ← No longer needed
	myCommitmentStore.initialize();
}

// After: Initialize 2 stores
export function initializeAllocationStores() {
	myRecognitionTreeStore.initialize();
	myCommitmentStore.initialize(); // THE source of truth!
	
	// Derived stores (need/capacity) don't need initialization!
}
```

### **4. Auto-Composition Simplified**

```typescript
// Before: Subscribe to everything (including slots)
export function enableAutoCommitmentComposition() {
	const unsubTree = myRecognitionTreeStore.subscribe(...);
	const unsubNeeds = myNeedSlotsStore.subscribe(...);  // ← Removed
	const unsubCapacity = myCapacitySlotsStore.subscribe(...);  // ← Removed
	const unsubNetworkRec = networkRecognitionWeights.subscribe(...);
	
	return () => {
		unsubTree();
		unsubNeeds();
		unsubCapacity();
		unsubNetworkRec();
	};
}

// After: Only subscribe to recognition (slots updated via helpers)
export function enableAutoCommitmentComposition() {
	const unsubTree = myRecognitionTreeStore.subscribe(...);
	const unsubNetworkRec = networkRecognitionWeights.subscribe(...);
	
	return () => {
		unsubTree();
		unsubNetworkRec();
	};
}
```

**This fixed the infinite loop!** Before, `myMutualRecognition` (derived from `myCommitmentStore`) triggered auto-composition, which updated `myCommitmentStore`, which triggered `myMutualRecognition` again!

### **5. Preservation in Composition**

```typescript
// composeCommitmentFromSources() now PRESERVES existing slots
export function composeCommitmentFromSources(): Commitment | null {
	const existingCommitment = get(myCommitmentStore);
	
	const commitment: Commitment = {
		// PRESERVE existing slots (updated via setMyNeedSlots/setMyCapacitySlots)
		need_slots: existingCommitment?.need_slots || [],
		capacity_slots: existingCommitment?.capacity_slots || [],
		
		// UPDATE recognition data (from tree + network)
		global_recognition_weights: recognitionWeights,
		global_mr_values: mutualRecognition,
		
		// ... rest
	};
	
	return commitment;
}
```

## **Migration Strategy**

### **For UI Components**

```typescript
// Before: Set slot stores directly
myNeedSlotsStore.set([...newNeeds]);
myCapacitySlotsStore.set([...newCapacities]);

// After: Use helper functions
setMyNeedSlots([...newNeeds]);
setMyCapacitySlots([...newCapacities]);
```

### **For Reading Slots**

```typescript
// Before & After: Same! (still subscribe to slot stores)
const needs = get(myNeedSlotsStore);
const capacities = get(myCapacitySlotsStore);
```

## **Data Storage**

### **Persistent Stores (2)**

1. `trees/recognition_tree` - Recognition tree
2. `allocation/commitment` - **THE source of truth** (contains slots!)

### **Derived Stores (3)**

1. `myNeedSlotsStore` - Derived from `commitment.need_slots`
2. `myCapacitySlotsStore` - Derived from `commitment.capacity_slots`
3. `myRecognitionWeights` - Derived from recognition tree

## **Benefits Summary**

| Aspect | Before | After |
|--------|--------|-------|
| **Persistent stores** | 4 | 2 |
| **Data duplication** | Yes (slots in 2 places) | No (slots in 1 place) |
| **Sync issues** | Possible | Impossible |
| **Initialization complexity** | Higher | Lower |
| **Auto-composition subscriptions** | 4 | 2 |
| **Infinite loop risk** | High (circular deps) | Low (one-way deps) |
| **Code lines** | ~50 more | ~50 fewer |
| **Conceptual complexity** | Higher | Lower |

## **The Philosophical Win**

**Commitment IS the source of truth.**

It's not "composed from" slots - it **contains** slots. This is more than a technical detail; it's a conceptual clarity:

- **Recognition tree** → Who I recognize (persistent)
- **Commitment** → What I offer & need + who I recognize (persistent)
- **Everything else** → Derived computations (not persistent)

This matches the protocol perfectly: participants publish commitments, not separate slot lists!

---

## **Next Steps**

✅ **Done:**
- Slot stores are now derived
- Helper functions added
- Initialization simplified
- Auto-composition fixed
- Infinite loop eliminated

⚠️ **TODO:**
- Update UI components to use `setMyNeedSlots()` / `setMyCapacitySlots()`
- Test slot creation and editing flows
- Verify no regressions

🎯 **Result:** A cleaner, simpler, more robust architecture with a single source of truth!

