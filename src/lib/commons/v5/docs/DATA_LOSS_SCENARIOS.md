# Data Loss Scenarios Due to Network Latency

## ✅ **CRITICAL ISSUE - RESOLVED**

**Status:** 🟢 **FIXED** (2025-01-29)

**Previous status:** ⚠️ CRITICAL - Data loss due to unsynchronized ITC stamps

**Fix:** ITC stamps are now merged with all network commitments before publishing

**See:** `ITC_MERGE_FIX_SUMMARY.md` for complete fix details

---

## Original Analysis (Historical Record)

**Note:** This document is preserved for historical reference. The issues described below have been fixed.

Yes, there are **real scenarios** where network latency **could have caused** data loss (now prevented):

---

## Scenario 1: Local Edit During Network Update (**MOST CRITICAL**)

### The Problem

**ITC stamps are NOT synchronized with network updates!**

### Code Analysis

```typescript
// stores.svelte.ts line 1260
export function composeCommitmentFromSources(): Commitment | null {
  const existingCommitment = get(myCommitmentStore);
  
  const commitment: Commitment = {
    need_slots: needSlots || [],
    capacity_slots: capacitySlots || [],
    global_recognition_weights: recognitionWeights,
    global_mr_values: mutualRecognition,
    multi_dimensional_damping: existingCommitment?.multi_dimensional_damping,
    
    // ❌ CRITICAL BUG: Uses local ITC stamp, NOT merged with network!
    itcStamp: existingCommitment?.itcStamp || null as any,
    timestamp: Date.now()
  };
  
  return commitment;
}
```

**The Issue:**
- Network commitments go to `networkCommitments` (VersionedStore)
- Own commitment is in `myCommitmentStore` (separate store!)
- **Composition uses `myCommitmentStore.itcStamp`** 
- **Does NOT merge with network ITC stamps!**

### The Attack Scenario

```
Time →

t=0     User A: Edit need slots locally
        myNeedSlotsStore.set([...newNeeds])
        ↓
        Triggers auto-composition (100ms debounce)

t=20    Network: User B's commitment arrives
        {
          needs: [B's needs],
          itcStamp: stamp_B  ← User B's latest ITC
        }
        ↓
        Goes to networkCommitments (NOT myCommitmentStore!)
        ↓
        Triggers myMutualRecognition recalculation
        ↓
        Triggers auto-composition again (100ms debounce)

t=100   Composition executes:
        const existingCommitment = get(myCommitmentStore);
        // This has stamp_A (User A's old stamp)
        // Does NOT include stamp_B from network!
        
        const commitment = {
          ...
          itcStamp: stamp_A  ← ❌ OLD STAMP!
        };
        
        myCommitmentStore.set(commitment);
        ↓
        Persists to network

t=120   Network: User A's commitment arrives at User B
        {
          needs: [A's NEW needs],
          itcStamp: stamp_A  ← But this is OLD relative to stamp_B!
        }
        
        User B's VersionedStore:
        - ITC check: leq(stamp_A, stamp_B)? 
        - If stamp_A < stamp_B → REJECTED as stale!
        - ❌ User A's edits are LOST!

Result: User A's local edits never propagated to the network!
```

### Why This Happens

**Two separate ITC lineages:**

```
User A's local chain:
  stamp_A_1 → stamp_A_2 → stamp_A_3
  (in myCommitmentStore)

Network chain:
  stamp_B_1 → stamp_B_2 → stamp_C_1
  (in networkCommitments)

❌ These are NEVER MERGED!

When User A publishes with stamp_A_3:
- Other users have seen stamp_B_2, stamp_C_1
- stamp_A_3 might be causally unrelated or old
- Gets rejected as stale!
```

---

## Scenario 2: Debounce Window Race

### The Problem

**100ms debounce can lose rapid edits**

### Flow

```
t=0     User: Edit needs
        myNeedSlotsStore.set(needs_v1)
        ↓
        Debounce timer starts (100ms)

t=50    Network: Update arrives
        networkCommitments.update(...)
        ↓
        Triggers mutual recognition update
        ↓
        Debounce timer restarts (100ms)

t=60    User: Edit needs again
        myNeedSlotsStore.set(needs_v2)
        ↓
        Debounce timer restarts (100ms)

t=100   Network: Another update arrives
        ↓
        Debounce timer restarts (100ms)

t=200   Composition finally executes
        Reads: needs_v2 (correct)
        But ITC stamp: might not reflect all intermediate network updates!
```

**Problem:**
- Multiple debounce restarts can delay composition
- Network updates during debounce might not be reflected in ITC
- Final published ITC might be stale

---

## Scenario 3: Persistence Race

### The Problem

**Network update arrives while persisting**

### Flow

```
t=0     User: Edit locally
        myCommitmentStore.set(commitment_A)
        ↓
        Persistence starts (isPersisting=true)
        ↓
        Network updates are QUEUED

t=50    Network: commitment_B arrives
        subscribeToCommitment callback fires
        ↓
        Sees isPersisting=true
        ↓
        Queues update: queuedNetworkUpdate = commitment_B

t=100   Persistence completes
        ↓
        Processes queue: networkCommitments.update(commitment_B)
        ↓
        Updates mutual recognition
        ↓
        Triggers auto-composition

t=200   Composition reads old ITC from myCommitmentStore
        ❌ Doesn't reflect commitment_B's ITC!
```

**Note:** This scenario exists in `store.svelte.ts` (generic store), but `myCommitmentStore` uses this pattern!

---

## Scenario 4: Concurrent Device Edits (Lost Update)

### Classic distributed systems problem!

```
Device 1 (Laptop):
  t=0   Edit recognition tree
        ITC: stamp_L1
        Publish to network

Device 2 (Phone):
  t=0   Edit needs (concurrent!)
        ITC: stamp_P1
        Publish to network

Network propagation:
  t=10  Laptop update arrives at Phone
        Phone's networkCommitments gets stamp_L1
        
  t=20  Phone reads its own myCommitmentStore (has stamp_P1)
        Composes new commitment
        ❌ Uses stamp_P1 (doesn't merge with stamp_L1!)
        Publishes to network

  t=30  Phone's new update arrives at Laptop
        Laptop's VersionedStore:
        - Has stamp_L1 in networkCommitments (from Phone's first update)
        - Receives stamp_P1 (from Phone's second update)
        - ITC check: Are these concurrent? Maybe!
        - But stamp_P1 doesn't include laptop's edit!
        - Last-write-wins at field level
        - ❌ Potential data loss depending on which fields changed
```

---

## Root Cause Analysis

### 1. **Dual ITC Lineage**

```
┌────────────────────────┐     ┌────────────────────────┐
│   myCommitmentStore    │     │  networkCommitments    │
│   (own commitment)     │     │  (others' commitments) │
│                        │     │                        │
│   itcStamp: stamp_A    │ ❌  │  Map<pubKey, stamp_X>  │
│                        │     │                        │
│   NEVER MERGED!        │     │  Separate stores!      │
└────────────────────────┘     └────────────────────────┘
```

### 2. **Composition Doesn't Merge Network ITCs**

```typescript
// ❌ WRONG: Only looks at own store
itcStamp: existingCommitment?.itcStamp || null

// ✅ CORRECT: Should merge with network
itcStamp: mergeWithNetworkITCs(existingCommitment?.itcStamp)
```

### 3. **No ITC Increment on Local Edit**

When user edits:
- Edit happens immediately
- Composition happens after debounce (100ms)
- ITC increment happens at composition time
- If network update arrives during debounce, not reflected!

---

## Impact Assessment

### 🔴 **HIGH SEVERITY**

**Data Loss Probability:**
- Single-user, single-device: **LOW** (timestamps work fine)
- Multi-device (same user): **MEDIUM** (concurrent edits)
- Multi-user: **HIGH** (frequent concurrent updates)

**Affected Operations:**
- ✅ **Reading** from network: Safe (VersionedStore works correctly)
- ❌ **Writing** to network: Unsafe (ITC not synchronized)
- ❌ **Concurrent edits**: Unsafe (no causal merge)

---

## Proof of Concept Test

```typescript
// TEST: Concurrent edit should preserve both changes
// EXPECTED: Pass
// ACTUAL: Might FAIL!

describe('Data Loss Scenario', () => {
  it('should preserve concurrent edits from different devices', async () => {
    // Device 1: Edit recognition
    await device1.myRecognitionTreeStore.set(tree_with_alice);
    await device1.publishCommitment(); // ITC: stamp_1
    
    // Device 2: Edit needs (concurrent!)
    await device2.myNeedSlotsStore.set([foodNeed]);
    await device2.publishCommitment(); // ITC: stamp_2
    
    // Wait for network propagation
    await delay(200);
    
    // Device 1 receives Device 2's update
    await device1.receiveFromNetwork(device2.commitment);
    
    // Device 2 receives Device 1's update  
    await device2.receiveFromNetwork(device1.commitment);
    
    // Both devices should have BOTH edits
    expect(device1.commitment.need_slots).toContain(foodNeed); // ❌ MIGHT FAIL
    expect(device2.commitment.global_recognition_weights).toContain('alice'); // ❌ MIGHT FAIL
  });
});
```

---

## Solutions

### Solution 1: Merge Network ITCs on Composition (**CRITICAL FIX**)

```typescript
export function composeCommitmentFromSources(): Commitment | null {
  const existingCommitment = get(myCommitmentStore);
  const networkCommitMap = networkCommitments.get();
  
  // ✅ Merge ITC with all network ITCs
  let mergedITC = existingCommitment?.itcStamp || itcSeed();
  
  for (const [_, versionedEntity] of networkCommitMap.entries()) {
    if (versionedEntity.metadata.itcStamp) {
      mergedITC = itcJoin(mergedITC, versionedEntity.metadata.itcStamp);
    }
  }
  
  // ✅ Increment after merge (our local event)
  mergedITC = itcEvent(mergedITC);
  
  const commitment: Commitment = {
    need_slots: needSlots || [],
    capacity_slots: capacitySlots || [],
    global_recognition_weights: recognitionWeights,
    global_mr_values: mutualRecognition,
    multi_dimensional_damping: existingCommitment?.multi_dimensional_damping,
    
    itcStamp: mergedITC, // ✅ Merged and incremented
    timestamp: Date.now()
  };
  
  return commitment;
}
```

### Solution 2: Immediate ITC Increment on Edit

```typescript
// When user edits
export function setMyNeedSlots(slots: NeedSlot[]) {
  // 1. Increment ITC immediately
  const existingCommitment = get(myCommitmentStore);
  let newITC = existingCommitment?.itcStamp || itcSeed();
  newITC = itcEvent(newITC);
  
  // 2. Store with new ITC
  myCommitmentStore.update(current => ({
    ...current,
    need_slots: slots,
    itcStamp: newITC,
    timestamp: Date.now()
  }));
  
  // 3. Trigger persistence (debounced)
  // Now the ITC is already incremented!
}
```

### Solution 3: Pessimistic Locking (Advanced)

```typescript
// Before publishing, fetch latest network state
export async function publishCommitmentWithLock(): Promise<void> {
  // 1. Fetch latest network commitments
  await syncNetworkCommitments();
  
  // 2. Merge ITCs
  const mergedITC = mergeAllNetworkITCs();
  
  // 3. Compose with merged ITC
  const commitment = composeCommitmentFromSources();
  commitment.itcStamp = mergedITC;
  
  // 4. Publish
  await myCommitmentStore.set(commitment);
}
```

### Solution 4: Event Sourcing (Architectural)

```typescript
// Don't store state, store events!
interface CommitmentEvent {
  type: 'ADD_NEED' | 'EDIT_RECOGNITION' | ...;
  data: any;
  itcStamp: ITCStamp;
  timestamp: number;
}

// Commitment is derived from event log
const commitment = reduceEvents(eventLog);
```

---

## Recommendations

### 🔴 **IMMEDIATE (CRITICAL)**

**Implement Solution 1** - Merge network ITCs on composition

```typescript
// Add this to stores.svelte.ts
function getMergedNetworkITC(): ITCStamp | null {
  const networkCommitMap = networkCommitments.get();
  let mergedITC: ITCStamp | null = null;
  
  for (const [_, versionedEntity] of networkCommitMap.entries()) {
    if (versionedEntity.metadata.itcStamp) {
      if (!mergedITC) {
        mergedITC = versionedEntity.metadata.itcStamp;
      } else {
        mergedITC = itcJoin(mergedITC, versionedEntity.metadata.itcStamp);
      }
    }
  }
  
  return mergedITC;
}

// Update composeCommitmentFromSources
export function composeCommitmentFromSources(): Commitment | null {
  // ... existing code ...
  
  // ✅ Merge with network ITCs
  let myITC = existingCommitment?.itcStamp;
  const networkITC = getMergedNetworkITC();
  
  if (myITC && networkITC) {
    myITC = itcJoin(myITC, networkITC);
  } else if (networkITC) {
    myITC = networkITC;
  } else if (!myITC) {
    myITC = itcSeed();
  }
  
  // ✅ Increment for this local event
  myITC = itcEvent(myITC);
  
  const commitment: Commitment = {
    // ... existing fields ...
    itcStamp: myITC,
    timestamp: Date.now()
  };
  
  return commitment;
}
```

### 🟡 **SHORT-TERM**

1. **Add integration tests** for concurrent edit scenarios
2. **Add logging** to track ITC merge operations
3. **Add metrics** to detect data loss in production

### 🟢 **LONG-TERM**

1. Consider **event sourcing** architecture
2. Consider **CRDT** data structures (e.g., Automerge, Yjs)
3. Consider **operational transformation** for real-time collab

---

## Testing Strategy

```typescript
// Add to tests/stores.test.ts
describe('Data Loss Prevention', () => {
  it('should merge network ITCs on composition', () => {
    // Setup: Network has stamp_A
    networkCommitments.update('alice', {
      itcStamp: stamp_A,
      // ...
    });
    
    // User edits locally (has old stamp_B)
    myCommitmentStore.set({
      itcStamp: stamp_B,
      need_slots: [newNeed]
    });
    
    // Compose
    const composed = composeCommitmentFromSources();
    
    // Should merge both stamps
    expect(itcLeq(stamp_A, composed.itcStamp)).toBe(true);
    expect(itcLeq(stamp_B, composed.itcStamp)).toBe(true);
  });
  
  it('should not lose concurrent edits', async () => {
    // Simulate concurrent edits
    // ... (see POC test above)
  });
});
```

---

## Conclusion

**YES, data loss is possible due to:**
1. ❌ ITC stamps not merged with network
2. ❌ Debounce delays can accumulate stale state
3. ❌ No pessimistic locking
4. ❌ Last-write-wins at field level

**The fix is straightforward:**
- Merge network ITCs before publishing
- Increment ITC after merge
- ~20 lines of code

**This should be fixed ASAP before production use with multiple users!** 🚨

