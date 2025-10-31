# Temporal Ordering & Staleness Detection Analysis

## Question

**"What happens when we receive data from different times? At what point do we realize data is not the newest? At what point is it too late?"**

## Complete Data Flow with Temporal Checks

```
┌─────────────────────────────────────────────────────────────────┐
│               HOLSTER P2P NETWORK                                │
│          Multiple updates arrive at different times              │
│                                                                  │
│  Update A (t=1000, ITC=stamp_A)  ────┐                          │
│  Update B (t=999,  ITC=stamp_B)  ────┼─→ Race condition!        │
│  Update C (t=1001, ITC=stamp_C)  ────┘   Who arrives first?     │
└──────────────────────────┬──────────────────────────────────────┘
                           │ holsterUser.get(path).on(callback)
                           ▼
┌─────────────────────────────────────────────────────────────────┐
│           CHECKPOINT 1: Generic Store (Layer 1)                  │
│              processNetworkUpdate() - FIRST DEFENSE              │
│                                                                  │
│  1. Extract timestamp: getTimestamp(data)                       │
│     → networkTimestamp = data._updatedAt                        │
│                                                                  │
│  2. Remove metadata: { _updatedAt, ...actualData }              │
│                                                                  │
│  3. Convert format: fromHolsterFormat(actualData)               │
│     → Records become Arrays, enums normalized                   │
│                                                                  │
│  4. ✅ SCHEMA VALIDATION:                                       │
│     const validation = schema.safeParse(convertedData)          │
│     if (!validation.success) return; // ← REJECTED              │
│                                                                  │
│  5. ⚠️  TIMESTAMP CHECK (for own data path only):               │
│     if (!lastNetworkTimestamp ||                                │
│         networkTimestamp > lastNetworkTimestamp) {              │
│       store.set(validation.data); // ← ACCEPTED                 │
│       lastNetworkTimestamp = networkTimestamp;                  │
│     }                                                            │
│     // SILENTLY SKIPPED if stale (no error logged!)             │
│                                                                  │
│  STATUS: Own data path has timestamp check                      │
│          Cross-user path (subscribeToUser) does NOT check!      │
└──────────────────────────┬──────────────────────────────────────┘
                           │ callback(validatedData)
                           ▼
┌─────────────────────────────────────────────────────────────────┐
│         CHECKPOINT 2: Application Layer (Layer 2)                │
│             subscribeToCommitment() - NORMALIZATION              │
│                                                                  │
│  1. Handle deletion: if (!commitment) delete from store         │
│                                                                  │
│  2. ✅ NORMALIZE recognition weights:                           │
│     normalizeGlobalRecognitionWeights() → Σ = 1.0              │
│                                                                  │
│  3. Pass to VersionedStore: networkCommitments.update()         │
│                                                                  │
│  STATUS: No temporal checks here, just transforms data          │
└──────────────────────────┬──────────────────────────────────────┘
                           │ networkCommitments.update(pubKey, data)
                           ▼
┌─────────────────────────────────────────────────────────────────┐
│        CHECKPOINT 3: VersionedStore (Layer 3) - FINAL GATE       │
│                update() method - STRONGEST DEFENSE               │
│                                                                  │
│  STEP 0: ✅ SCHEMA VALIDATION (NEW!)                            │
│  ─────────────────────────────────────────────────────────      │
│  if (config.schema) {                                           │
│    const validation = schema.safeParse(entity);                 │
│    if (!validation.success) {                                   │
│      return { applied: false, reason: 'Schema failed' };        │
│    }                                                             │
│  }                                                               │
│                                                                  │
│  STEP 1: ✅ ITC CAUSALITY CHECK (Primary)                       │
│  ─────────────────────────────────────────────────────────      │
│  Extract: entityITC = itcExtractor(entity)                      │
│           entityTimestamp = timestampExtractor(entity)          │
│                                                                  │
│  if (existing && entityITC && existing.metadata.itcStamp) {    │
│    // Check: Is incoming causally stale?                        │
│    if (itcLeq(entityITC, existing.itcStamp) &&                 │
│        !itcEquals(entityITC, existing.itcStamp)) {             │
│      console.log('⏭️  ITC stale');                              │
│      return { applied: false, reason: 'ITC causal staleness' };│
│    }                                                             │
│                                                                  │
│    // ✅ MERGE ITC stamps (preserve causal history)            │
│    entityITC = itcJoin(existing.itcStamp, entityITC);          │
│    console.log('🔀 Merged ITC stamps');                         │
│                                                                  │
│    // ✅ CRITICAL: Skip timestamp check when ITC available!    │
│    // ITC is source of truth - timestamps can have clock skew  │
│  } else {                                                        │
│    // ✅ TIMESTAMP CHECK (Fallback - only when NO ITC)         │
│    if (entityTimestamp && existing.metadata.timestamp) {       │
│      if (entityTimestamp <= existing.metadata.timestamp) {     │
│        console.log('⏭️  Timestamp stale');                      │
│        return { applied: false, reason: 'Timestamp staleness' };│
│      }                                                           │
│    }                                                             │
│  }                                                               │
│                                                                  │
│  STEP 2: ✅ FIELD CHANGE DETECTION                              │
│  ─────────────────────────────────────────────────────────      │
│  Compare fields with existing data                              │
│  if (no fields changed && existing) {                           │
│    // Update causality metadata only                            │
│    return { applied: false, reason: 'No field changes' };      │
│  }                                                               │
│                                                                  │
│  STEP 3: ✅ STORE UPDATE                                        │
│  ─────────────────────────────────────────────────────────      │
│  dataStore.set(key, { data: entity, metadata: {...} });        │
│  return { applied: true, changedFields: [...] };               │
│                                                                  │
│  STATUS: This is the STRONGEST gate - triple validation!        │
└──────────────────────────┬──────────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────────┐
│                  STORED IN VERSIONED STORE                       │
│              🎯 TOO LATE! Data is now in the store              │
└─────────────────────────────────────────────────────────────────┘
```

## Detailed Analysis by Checkpoint

### CHECKPOINT 1: Generic Store (`processNetworkUpdate`)

**Location:** `src/lib/commons/utils/store.svelte.ts` lines 125-152

**Checks:**
1. ✅ Schema validation (Zod)
2. ⚠️  Timestamp check (only for own data, not cross-user!)

**Code:**
```typescript
function processNetworkUpdate(data: any) {
  if (!data) return;
  
  // Extract timestamp
  const networkTimestamp = getTimestamp(data);
  
  // Convert & validate
  const convertedData = fromHolsterFormat(actualData);
  const validation = schema.safeParse(convertedData);
  if (!validation.success) {
    console.warn('Invalid network data');
    return; // ← REJECTED
  }
  
  // ⚠️  Timestamp check (ONLY for own data path!)
  if (!lastNetworkTimestamp || 
      (networkTimestamp && networkTimestamp > lastNetworkTimestamp)) {
    store.set(validation.data); // ← ACCEPTED
    lastNetworkTimestamp = networkTimestamp;
  }
  // If stale: SILENTLY SKIPPED (no log!)
}
```

**Cross-User Path:**
```typescript
function subscribeToUser(pubKey: string, callback: ...) {
  holsterUser.get([pubKey, path]).on((data: any) => {
    // Convert & validate
    const convertedData = fromHolsterFormat(actualData);
    const validation = schema.safeParse(convertedData);
    
    if (!validation.success) {
      callback(null);
      return;
    }
    
    // ⚠️  NO TIMESTAMP CHECK HERE!
    callback(validation.data); // ← Always passes through
  });
}
```

**Key Issue:**
- Own data path: Has timestamp check ✅
- Cross-user path: No timestamp check ❌
- **Relies entirely on VersionedStore for temporal ordering!**

### CHECKPOINT 2: Application Layer (`subscribeToCommitment`)

**Location:** `src/lib/commons/v5/stores.svelte.ts` lines 461-501

**Checks:**
1. ✅ Null handling (deletion detection)
2. ✅ Recognition weight normalization
3. ❌ **NO temporal checks**

**Code:**
```typescript
myCommitmentStore.subscribeToUser(pubKey, (commitment) => {
  if (!commitment) {
    networkCommitments.delete(pubKey);
    return;
  }
  
  // Normalize (semantic transform, not temporal check)
  let normalizedCommitment = commitment;
  if (commitment.global_recognition_weights) {
    normalizedCommitment = {
      ...commitment,
      global_recognition_weights: normalizeGlobalRecognitionWeights(...)
    };
  }
  
  // Pass to VersionedStore (NO temporal check here!)
  const result = networkCommitments.update(pubKey, normalizedCommitment);
});
```

**Key Point:**
- This layer **trusts** that VersionedStore will handle temporal ordering
- No timestamp checks, no ITC checks
- Just semantic transformations

### CHECKPOINT 3: VersionedStore (`update()`)

**Location:** `src/lib/commons/v5/v-store.svelte.ts` lines 267-393

**Checks:**
1. ✅ Schema validation (defensive, NEW!)
2. ✅ ITC causality check (primary)
3. ✅ Timestamp check (fallback, only when no ITC)
4. ✅ Field change detection

**This is the STRONGEST gate!**

**Code:**
```typescript
update(key: K, entity: T): UpdateResult {
  const existing = this.dataStore.get(key);
  
  // STEP 0: Schema validation (NEW!)
  if (this.config.schema) {
    const validation = this.config.schema.safeParse(entity);
    if (!validation.success) {
      return { applied: false, reason: 'Schema validation failed' };
    }
  }
  
  // STEP 1: ITC causality check
  let entityITC = this.config.itcExtractor?.(entity);
  const entityTimestamp = this.config.timestampExtractor?.(entity);
  
  if (existing) {
    // PRIMARY: ITC check
    if (entityITC && existing.metadata.itcStamp) {
      // Check staleness
      if (itcLeq(entityITC, existing.metadata.itcStamp) &&
          !itcEquals(entityITC, existing.metadata.itcStamp)) {
        console.log('⏭️  ITC stale');
        return { applied: false, reason: 'ITC causal staleness' };
      }
      
      // Merge ITC stamps (preserves full causal history)
      entityITC = itcJoin(existing.metadata.itcStamp, entityITC);
      
      // ✅ CRITICAL: Skip timestamp check!
      // When ITC available, it's the source of truth
      // Concurrent updates can have different timestamps (clock skew)
      
    } else {
      // FALLBACK: Timestamp check (only when NO ITC)
      if (entityTimestamp && existing.metadata.timestamp) {
        if (entityTimestamp <= existing.metadata.timestamp) {
          console.log('⏭️  Timestamp stale');
          return { applied: false, reason: 'Timestamp staleness' };
        }
      }
    }
  }
  
  // STEP 2: Field change detection
  const changes = this.detectFieldChanges(existing, entity);
  if (changes.changedFields.size === 0 && existing) {
    // No changes - update causality metadata only
    return { applied: false, reason: 'No field changes' };
  }
  
  // STEP 3: Store update
  this.dataStore.set(key, {
    data: entity,
    metadata: {
      itcStamp: entityITC,        // Merged stamp!
      timestamp: entityTimestamp,
      fieldVersions: changes.newVersions,
      lastUpdate: Date.now()
    }
  });
  
  return { applied: true, changedFields: changes.changedFields };
}
```

## Race Condition Scenarios

### Scenario 1: Sequential Updates (Correct Ordering)

```
Network:
  t1: Update A (timestamp=1000, ITC=stamp_A) arrives first
  t2: Update B (timestamp=1001, ITC=stamp_B) arrives second

Flow:
  Update A → VersionedStore:
    - No existing data
    - Store: { data: A, metadata: { itcStamp: stamp_A, timestamp: 1000 } }
    - Result: ✅ ACCEPTED
  
  Update B → VersionedStore:
    - Existing: stamp_A
    - ITC check: itcLeq(stamp_B, stamp_A) → FALSE (B > A)
    - ITC merge: stamp_B' = join(stamp_A, stamp_B) → stamp_B
    - Store: { data: B, metadata: { itcStamp: stamp_B, timestamp: 1001 } }
    - Result: ✅ ACCEPTED

Final: B is stored (correct! ✅)
```

### Scenario 2: Out-of-Order Arrival (ITC Protects)

```
Network:
  t1: Update B (timestamp=1001, ITC=stamp_B) arrives first ← Later update first!
  t2: Update A (timestamp=1000, ITC=stamp_A) arrives second ← Earlier update second!

Flow:
  Update B → VersionedStore:
    - No existing data
    - Store: { data: B, metadata: { itcStamp: stamp_B, timestamp: 1001 } }
    - Result: ✅ ACCEPTED (first arrival)
  
  Update A → VersionedStore:
    - Existing: stamp_B
    - ITC check: itcLeq(stamp_A, stamp_B) → TRUE (A < B)
                 !itcEquals(stamp_A, stamp_B) → TRUE
    - Result: ❌ REJECTED (ITC causal staleness)

Final: B is stored (correct! ✅)
Console: "⏭️  ITC stale: pub_key_123..."
```

### Scenario 3: Concurrent Updates (Both Accepted!)

```
Device 1: Edit field A → stamp_1 (timestamp=1000)
Device 2: Edit field B → stamp_2 (timestamp=999) ← Clock skew!

Network:
  t1: Update from Device 1 arrives first
  t2: Update from Device 2 arrives second

Flow:
  Update 1 → VersionedStore:
    - No existing data
    - Store: { data: data1, metadata: { itcStamp: stamp_1, timestamp: 1000 } }
    - Result: ✅ ACCEPTED
  
  Update 2 → VersionedStore:
    - Existing: stamp_1
    - ITC check: itcLeq(stamp_2, stamp_1) → FALSE (concurrent!)
    - ITC merge: stamp_merged = join(stamp_1, stamp_2)
    - ⚠️  NO timestamp check (ITC is source of truth)
    - Store: { data: data2, metadata: { itcStamp: stamp_merged, timestamp: 999 } }
    - Result: ✅ ACCEPTED (LWW - last write wins on field level)

Final: data2 is stored with merged ITC stamp ✅
Note: Both edits are causally valid! Last one wins at field level.
```

### Scenario 4: Network Echoes Own Update

```
You: Edit locally → persist to Holster → timestamp=1000, ITC=stamp_A
Network: Your own update echoes back

Flow:
  Your update → VersionedStore:
    - Existing: stamp_A (your previous version)
    - ITC check: itcLeq(stamp_A, stamp_A) → TRUE
                 !itcEquals(stamp_A, stamp_A) → FALSE
    - ⚠️  ITC stamps are EQUAL (not stale!)
    - Field change detection: No changes
    - Result: ❌ REJECTED (No field changes)

Final: No-op (correct! ✅)
Console: "⏭️  No field changes: pub_key_123... (causality updated)"
```

## When Is It "Too Late"?

### ✅ NOT Too Late (Rejected Before Storage)

1. **Invalid schema** → Rejected at Checkpoint 1 or 3
2. **ITC causal staleness** → Rejected at Checkpoint 3 (STEP 1)
3. **Timestamp staleness** → Rejected at Checkpoint 3 (STEP 1, fallback)
4. **No field changes** → Rejected at Checkpoint 3 (STEP 2)

**Result:** Data never enters the store, no derived stores trigger, no UI updates

### ❌ TOO LATE (Already Stored)

**After `dataStore.set()` is called** (line 365 in v-store.svelte.ts):

```typescript
this.dataStore.update(map => {
  const newMap = new Map(map);
  newMap.set(key, { data: entity, metadata: {...} }); // ← TOO LATE!
  return newMap;
});
```

**At this point:**
- Data is in the versioned store ✅
- Derived stores will trigger ✅
- Indexes will rebuild ✅
- UI components will re-render ✅
- **Cannot roll back!**

## Critical Insights

### 1. ITC is Primary, Timestamp is Fallback

**From code:**
```typescript
if (entityITC && existing.metadata.itcStamp) {
  // ITC check - PRIMARY
  // Skip timestamp check even if timestamp is older!
} else {
  // Timestamp check - FALLBACK (only when NO ITC)
}
```

**Why?**
- ITC handles concurrent updates correctly
- Timestamps can have clock skew (Device A: 1001, Device B: 999)
- If both devices concurrently edit, both are valid!
- ITC merge preserves full causal history

### 2. Generic Store Has Limited Temporal Checking

**Own data path:**
```typescript
// ✅ Has timestamp check
if (networkTimestamp > lastNetworkTimestamp) {
  store.set(data);
}
```

**Cross-user path:**
```typescript
// ❌ NO timestamp check
callback(validatedData); // Always passes through
```

**Implication:**
- Cross-user data relies **entirely** on VersionedStore for temporal ordering
- If VersionedStore didn't have ITC/timestamp checks, stale data would be accepted!
- **This is actually OK** because VersionedStore is designed for this

### 3. Field-Level LWW (Last Write Wins)

When concurrent updates happen:
- Both are causally valid (neither happened-before the other)
- ITC merge preserves both stamps
- But at the **data level**: Last write wins
- **Field versioning** tracks which fields changed

**Example:**
```
Device A: Edit recognition → stamp_A
Device B: Edit needs → stamp_B

Both concurrent!

Result: Merged stamp, last update's data wins
But: Field versions track that BOTH fields changed
```

## Recommendations

### ✅ Current System is Robust

**Strengths:**
1. **Triple validation** (Checkpoint 1 + 3 + 3)
2. **ITC causality** (handles concurrent edits correctly)
3. **Timestamp fallback** (works without ITC)
4. **Field versioning** (fine-grained tracking)
5. **Defensive schema validation** (NEW - catches corruption)

**Weaknesses:**
1. Generic store cross-user path has no temporal check
   - **But this is OK** - VersionedStore handles it
2. Concurrent updates use LWW at data level
   - **But this is expected** - field versions track changes

### 🔧 Potential Improvements

#### 1. Add Logging to Generic Store

```typescript
function subscribeToUser(pubKey: string, callback: ...) {
  holsterUser.get([pubKey, path]).on((data: any) => {
    const validation = schema.safeParse(convertedData);
    
    if (!validation.success) {
      console.warn(`[STORE:${path}] Invalid data from ${pubKey.slice(0,20)}`);
      callback(null);
      return;
    }
    
    // ✅ ADD: Log that we're passing data without temporal check
    console.log(`[STORE:${path}] Received data from ${pubKey.slice(0,20)}, delegating temporal check to VersionedStore`);
    
    callback(validation.data);
  });
}
```

#### 2. Add Timestamp Metadata to Callback

```typescript
function subscribeToUser(pubKey: string, callback: (data, metadata) => void) {
  holsterUser.get([pubKey, path]).on((data: any) => {
    const networkTimestamp = getTimestamp(data);
    const validation = schema.safeParse(convertedData);
    
    if (!validation.success) {
      callback(null, null);
      return;
    }
    
    // Pass timestamp metadata so caller can make informed decisions
    callback(validation.data, { timestamp: networkTimestamp });
  });
}
```

#### 3. Add Statistics Tracking

```typescript
// Track temporal rejections for monitoring
const temporalStats = {
  itcRejectionsCount: 0,
  timestampRejectionsCount: 0,
  schemaRejectionsCount: 0,
  acceptedCount: 0
};

export function getTemporalStats() {
  return { ...temporalStats };
}
```

## Summary Table

| Checkpoint | Location | Temporal Check | Schema Check | Can Reject? |
|------------|----------|----------------|--------------|-------------|
| **CP1a** | Generic store (own data) | ✅ Timestamp | ✅ Zod | ✅ Yes |
| **CP1b** | Generic store (cross-user) | ❌ None | ✅ Zod | ⚠️  Schema only |
| **CP2** | subscribeToCommitment | ❌ None | ❌ None | ❌ No |
| **CP3** | VersionedStore.update() | ✅ ITC + Timestamp | ✅ Zod (NEW) | ✅ Yes |

**Final Gate:** CP3 (VersionedStore) is the strongest defense!

## Conclusion

**Q: When do we realize data is not the newest?**
- **Primary:** At VersionedStore.update() via ITC causality check (STEP 1)
- **Fallback:** At VersionedStore.update() via timestamp check (STEP 1, if no ITC)
- **Rare:** At Generic store processNetworkUpdate() (own data only)

**Q: When is it too late?**
- **After:** `dataStore.set(key, { data, metadata })` executes
- **Result:** Data is stored, derived stores trigger, UI updates
- **Cannot roll back** - but that's OK because we already validated!

**Q: What if we accept stale data?**
- **Not possible** (with ITC) - causal staleness is detected before storage
- **Edge case** (without ITC) - timestamp check catches it
- **Fail-safe** - schema validation catches corruption

**The system has defense-in-depth with VersionedStore as the final, strongest gate!** 🛡️

