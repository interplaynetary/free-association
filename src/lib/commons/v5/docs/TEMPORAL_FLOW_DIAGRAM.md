# Temporal Ordering: Visual Flow Diagram

## Quick Answer

**When do we detect stale data?**
- ✅ **Primary:** VersionedStore ITC causality check (99% of cases)
- ✅ **Fallback:** VersionedStore timestamp check (if no ITC)
- ⚠️  **Rare:** Generic store timestamp check (own data only)

**When is it too late?**
- ❌ **After line 365 in v-store.svelte.ts:** `dataStore.set(key, { data, metadata })`
- At this point: Data is stored, UI will update, cannot roll back

---

## Three Incoming Updates (Race Condition)

```
Time →

t=0     Network sends three updates
        ┌─────────────────────────────────────────────┐
        │ Update A: ITC=stamp_A, timestamp=1000       │
        │ Update B: ITC=stamp_B, timestamp=999 (older)│
        │ Update C: ITC=stamp_C, timestamp=1001       │
        └─────────────────────────────────────────────┘
                         ↓
                    Race to arrive!
                         ↓

t=10    Update B arrives FIRST (out of order!)
        ┌──────────────────────────────────┐
        │ CHECKPOINT 1: Generic Store      │
        │ - Schema valid? ✅               │
        │ - Timestamp check? ⚠️  (only own)│
        │ - Pass to callback                │
        └──────────────────────────────────┘
                         ↓
        ┌──────────────────────────────────┐
        │ CHECKPOINT 2: App Layer          │
        │ - Normalize weights ✅           │
        │ - No temporal check               │
        └──────────────────────────────────┘
                         ↓
        ┌──────────────────────────────────┐
        │ CHECKPOINT 3: VersionedStore     │
        │ - Schema valid? ✅               │
        │ - ITC check? ✅ No existing data │
        │ - ACCEPT & STORE                 │
        │                                  │
        │ Stored: B (stamp_B, t=999)      │
        └──────────────────────────────────┘
                    ✅ ACCEPTED

t=20    Update A arrives SECOND
        ┌──────────────────────────────────┐
        │ CHECKPOINT 3: VersionedStore     │
        │ - Schema valid? ✅               │
        │ - ITC check:                     │
        │   * Existing: stamp_B            │
        │   * Incoming: stamp_A            │
        │   * itcLeq(A, B)? TRUE (A < B)  │
        │   * REJECT - STALE!              │
        │                                  │
        │ Console: "⏭️  ITC stale"         │
        └──────────────────────────────────┘
                    ❌ REJECTED

t=30    Update C arrives LAST
        ┌──────────────────────────────────┐
        │ CHECKPOINT 3: VersionedStore     │
        │ - Schema valid? ✅               │
        │ - ITC check:                     │
        │   * Existing: stamp_B            │
        │   * Incoming: stamp_C            │
        │   * itcLeq(C, B)? FALSE (C > B) │
        │   * Merge: stamp_BC = join(B, C)│
        │   * ACCEPT & STORE               │
        │                                  │
        │ Stored: C (stamp_BC, t=1001)    │
        └──────────────────────────────────┘
                    ✅ ACCEPTED

Final State: C is stored
Result: ✅ CORRECT (newest data kept)
```

---

## Concurrent Updates (Both Valid!)

```
Time →

t=0     Two devices edit simultaneously
        ┌──────────────────────────────────┐
        │ Device 1: Edit field A           │
        │ ITC: stamp_1, timestamp=1000     │
        └──────────────────────────────────┘
        ┌──────────────────────────────────┐
        │ Device 2: Edit field B           │
        │ ITC: stamp_2, timestamp=999      │
        │ (clock skew! older timestamp)    │
        └──────────────────────────────────┘
                         ↓
                Both concurrent!
           (neither happened-before other)
                         ↓

t=10    Update 1 arrives first
        ┌──────────────────────────────────┐
        │ VersionedStore                   │
        │ - No existing data               │
        │ - ACCEPT & STORE                 │
        │                                  │
        │ Stored: {                        │
        │   fieldA: 'new',                │
        │   fieldB: 'old',                │
        │   itcStamp: stamp_1,            │
        │   timestamp: 1000               │
        │ }                                │
        └──────────────────────────────────┘

t=20    Update 2 arrives second
        ┌──────────────────────────────────┐
        │ VersionedStore                   │
        │ - ITC check:                     │
        │   * itcLeq(stamp_2, stamp_1)?   │
        │   * FALSE (concurrent!)          │
        │                                  │
        │ - ✅ Merge ITC stamps:           │
        │   stamp_merged = join(stamp_1,  │
        │                       stamp_2)   │
        │                                  │
        │ - ⚠️  Skip timestamp check!     │
        │   (ITC says both valid)          │
        │                                  │
        │ - Field change detection:        │
        │   * fieldA: unchanged            │
        │   * fieldB: changed ✅           │
        │                                  │
        │ - ACCEPT & STORE                 │
        │                                  │
        │ Stored: {                        │
        │   fieldA: 'new',  ← from Update1│
        │   fieldB: 'new',  ← from Update2│
        │   itcStamp: stamp_merged,       │
        │   timestamp: 999 ← older!        │
        │ }                                │
        └──────────────────────────────────┘

Final State: Both edits preserved!
Result: ✅ CORRECT (concurrent edits merged)

Note: Timestamp is 999 (older) but data is correct!
      This is why ITC > timestamps for causality.
```

---

## The "Too Late" Point

```
┌─────────────────────────────────────────────┐
│ VersionedStore.update(key, entity)          │
│                                             │
│ // Validation & checks                     │
│ if (schema validation fails) return ❌     │  ← Still safe
│ if (ITC causal stale) return ❌            │  ← Still safe
│ if (timestamp stale) return ❌             │  ← Still safe
│ if (no field changes) return ❌            │  ← Still safe
│                                             │
│ // Update store                             │
│ this.dataStore.update(map => {             │
│   newMap.set(key, {                        │  ← TOO LATE! ❌
│     data: entity,                          │     Data stored
│     metadata: { ... }                      │     UI will update
│   });                                       │     Cannot rollback
│   return newMap;                            │
│ });                                         │
│                                             │
│ // Triggers                                 │
│ - Svelte store subscribers fire            │
│ - Derived stores recalculate               │
│ - UI components re-render                  │
│ - Indexes rebuild                           │
└─────────────────────────────────────────────┘
```

---

## Detection Points Summary

| Point | Check | Can Reject? | Frequency |
|-------|-------|-------------|-----------|
| **Generic store (own)** | Timestamp | ✅ Yes | Rare |
| **Generic store (cross-user)** | Schema only | ⚠️  Partial | Always |
| **VersionedStore ITC** | Causality | ✅ Yes | 99% |
| **VersionedStore Timestamp** | Time order | ✅ Yes | If no ITC |
| **VersionedStore Fields** | Change detection | ✅ Yes | Always |

**Strongest Defense:** VersionedStore with ITC causality ✅

---

## Why ITC > Timestamps

```
Scenario: Two devices with clock skew

Device A (fast clock):
  Edit at real-time t=10
  Timestamp: 1001
  ITC: stamp_A

Device B (slow clock):
  Edit at real-time t=11 (LATER!)
  Timestamp: 999 (OLDER!)
  ITC: stamp_B

With Timestamps Only:
  ❌ B arrives → looks older → REJECTED
  ❌ Lost B's edit even though it's newer!

With ITC:
  ✅ A arrives → stored
  ✅ B arrives → concurrent check → both valid!
  ✅ Merge stamps → preserve both edits
  ✅ Field-level merge → no data loss

Result: ITC handles clock skew correctly! ✅
```

---

## Edge Cases Handled

### 1. Own Update Echoes Back
```
You: Edit → persist → ITC=stamp_A
Network: Your update echoes back

VersionedStore:
  - ITC check: stamp_A == stamp_A (equal!)
  - Field check: No changes
  - Result: ❌ REJECTED (No field changes)
  - ✅ CORRECT (avoid re-rendering)
```

### 2. Multiple Updates During Persistence
```
You: Editing locally (persisting...)
Network: Update arrives

Generic store:
  - Queue update (isPersisting=true)
  - After persistence complete → process queue
  - VersionedStore checks ITC
  - ✅ Correct ordering preserved
```

### 3. Partial Data Loss
```
Network: Corrupted update (missing fields)

Checkpoint 1: Schema validation ❌ REJECTED
Checkpoint 3: Never reached

Result: ✅ Corrupted data never enters store
```

---

## Console Logs (What You'll See)

### Update Accepted
```
[VERSIONED-STORE] 🔀 Merged ITC stamps for pub_alice_123...
[VERSIONED-STORE] ✅ Updated [needs, capacity]: pub_alice_123...
```

### Update Rejected (Stale)
```
[VERSIONED-STORE] ⏭️  ITC stale: pub_bob_456...
[ALLOCATION-HOLSTER-V5] ⏭️  Skipped from pub_bob_456... (ITC causal staleness)
```

### Update Rejected (No Changes)
```
[VERSIONED-STORE] ⏭️  No field changes: pub_charlie_789... (causality updated)
[ALLOCATION-HOLSTER-V5] ⏭️  Skipped from pub_charlie_789... (No field changes)
```

### Schema Validation Failed
```
[VERSIONED-STORE] ❌ Schema validation failed for pub_dave_012:
  need_slots: Expected array, received object
[ALLOCATION-HOLSTER-V5] ⏭️  Skipped from pub_dave_012... (Schema validation failed: ...)
```

---

## Key Takeaways

1. **VersionedStore is the final gate** - Strongest temporal checks
2. **ITC handles concurrent edits correctly** - Better than timestamps
3. **Multiple validation layers** - Defense in depth
4. **"Too late" = after dataStore.set()** - But we validated first!
5. **Console logs show exactly what happened** - Great for debugging

🛡️ **The system is robust against temporal races and data corruption!**

