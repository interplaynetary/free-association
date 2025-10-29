# Rigorous Audit of Versioned Store Implementation

## ✅ Issue 1: ITC Join (FIXED)

**Problem**: Was overwriting ITC stamps instead of joining them.
**Status**: ✅ FIXED (lines 231)
**Verification**: Now correctly calls `itcJoin(existing.metadata.itcStamp, entityITC)`

---

## ⚠️ Issue 2: Timestamp Check Interaction with ITC

### Current Code (Lines 239-246)

```typescript
// Timestamp check (fallback)
if (entityTimestamp && existing.metadata.timestamp) {
  if (entityTimestamp <= existing.metadata.timestamp) {
    return { applied: false, reason: 'Timestamp staleness' };
  }
}
```

### Problem

This happens **AFTER** the ITC join! Consider this scenario:

```
Device A: concurrent update, stamp_A, timestamp 1000
Device B: concurrent update, stamp_B, timestamp 999

Receiver:
  1. Gets A: stores stamp_A, timestamp 1000
  2. Gets B:
     - ITC check: concurrent with A → PASS
     - ITC join: stamp_merged = join(stamp_A, stamp_B)
     - Timestamp check: 999 <= 1000 → REJECT! ❌

Result: Lost B's update due to clock skew!
```

### Analysis

**Concurrent updates can have different timestamps** (clock skew, different devices).

Timestamp check should ONLY apply if:
1. No ITC stamps available (fallback mode), OR
2. ITC stamps are equal (same causal history, use timestamp as tiebreaker)

### Recommended Fix

```typescript
if (existing) {
  // ITC check (primary)
  if (entityITC && existing.metadata.itcStamp) {
    // Check if incoming is causally stale
    if (itcLeq(entityITC, existing.metadata.itcStamp) &&
        !itcEquals(entityITC, existing.metadata.itcStamp)) {
      return { applied: false, reason: 'ITC causal staleness' };
    }
    
    // Merge ITC stamps
    entityITC = itcJoin(existing.metadata.itcStamp, entityITC);
    
    // ✅ FIX: Skip timestamp check when ITC available
    // ITC is the source of truth for causality
    // Timestamp only used when ITC not available
  } else {
    // ✅ ONLY use timestamp when NO ITC stamps
    if (entityTimestamp && existing.metadata.timestamp) {
      if (entityTimestamp <= existing.metadata.timestamp) {
        return { applied: false, reason: 'Timestamp staleness' };
      }
    }
  }
}
```

**Severity**: 🔴 HIGH - Can lose concurrent updates with clock skew

---

## ✅ Issue 3: Field Change Detection Logic

### Code (Lines 454-485)

```typescript
private detectFieldChanges(
  existing: VersionedEntity<T> | undefined,
  incoming: T
): FieldChanges {
  // ...
  for (const fieldName of this.fieldNames) {
    const extractor = this.config.fields[fieldName];
    const equalityChecker = this.config.fieldEqualityCheckers[fieldName] || this.defaultEquals;
    
    const oldVersion = existing?.metadata.fieldVersions[fieldName] || 0;
    oldVersions[fieldName] = oldVersion;
    
    const oldValue = existing ? extractor(existing.data) : undefined;
    const newValue = extractor(incoming);
    
    const changed = !equalityChecker(oldValue, newValue);
    
    if (changed) {
      changedFields.add(fieldName);
      newVersions[fieldName] = oldVersion + 1;
    } else {
      newVersions[fieldName] = oldVersion;
    }
  }
}
```

**Status**: ✅ CORRECT

**Verification**:
- ✅ Handles first time (existing = undefined)
- ✅ Uses custom equality checkers if provided
- ✅ Falls back to defaultEquals
- ✅ Monotonic version increment (oldVersion + 1)
- ✅ Preserves version when unchanged

---

## ⚠️ Issue 4: Default Equality Checker - Arrays with Functions

### Code (Lines 490-521)

```typescript
private defaultEquals(a: any, b: any): boolean {
  // Arrays
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) {
      if (!this.defaultEquals(a[i], b[i])) return false;
    }
    return true;
  }
  // ...
}
```

### Problem

Arrays containing **functions** or **class instances** may not compare correctly:

```typescript
const a = [{ fn: () => {} }];
const b = [{ fn: () => {} }];
defaultEquals(a, b); // false (different function references)
```

### Is This A Problem?

**For your use case**: Probably not!
- Commitments have: primitives, objects, arrays of objects
- Unlikely to have functions in field data
- If needed, provide custom equality checker

**Recommendation**: Document this limitation, provide custom checkers for special cases

**Severity**: 🟡 LOW - Edge case, easy to work around with custom checkers

---

## ⚠️ Issue 5: Derived Store Triggering

### Code (Lines 366-383)

```typescript
deriveField<F>(fieldName: string): Readable<Map<K, F>> {
  const extractor = this.config.fields[fieldName];
  
  return derived(
    this.dataStore,
    ($dataMap) => {
      const fieldMap = new Map<K, F>();
      for (const [key, versionedEntity] of $dataMap.entries()) {
        const fieldValue = extractor(versionedEntity.data);
        fieldMap.set(key, fieldValue);
      }
      return fieldMap;
    }
  );
}
```

### Problem

This derived store triggers on **ANY** change to `dataStore`, not just when the field changes!

**Example**:
```
Alice updates needs (recognition unchanged)
  → dataStore updates
  → recognitionStore derived runs (unnecessary!)
  → Extracts recognition (same value)
  → Returns new Map (different object reference)
  → Triggers downstream subscribers ❌
```

### Analysis

The derived store **ALWAYS runs** when base store updates, but returns a **new Map** each time, even if field values didn't change!

This defeats the purpose of field versioning!

### Impact

**Scenario**:
```
Alice updates needs
  → recognitionStore.subscribe() fires
  → myMutualRecognition recalculates (wasted!)
```

### Why This Happens

Svelte's `derived()` triggers when dependencies change. The derived store doesn't know about field versions - it just sees the base store changed.

### Potential Solutions

#### Option A: Accept It (Current)
- Derived store runs, but extracts same value
- Downstream computations see same value
- Svelte may optimize (shallow comparison)
- Not as bad as full commitment recalculation

#### Option B: Version-Aware Derived Store
```typescript
deriveField<F>(fieldName: string): Readable<Map<K, F>> {
  const extractor = this.config.fields[fieldName];
  let lastVersions = new Map<K, number>();
  let lastFieldMap = new Map<K, F>();
  
  return derived(
    this.dataStore,
    ($dataMap, set) => {
      let changed = false;
      
      for (const [key, versionedEntity] of $dataMap.entries()) {
        const currentVersion = versionedEntity.metadata.fieldVersions[fieldName];
        const lastVersion = lastVersions.get(key);
        
        if (currentVersion !== lastVersion) {
          changed = true;
          lastVersions.set(key, currentVersion);
          const fieldValue = extractor(versionedEntity.data);
          lastFieldMap.set(key, fieldValue);
        }
      }
      
      // Only return new Map if field actually changed
      if (changed) {
        return new Map(lastFieldMap);
      } else {
        return lastFieldMap; // Return same reference
      }
    }
  );
}
```

#### Option C: Custom Store (Best)
```typescript
deriveField<F>(fieldName: string): Readable<Map<K, F>> {
  let fieldMap = new Map<K, F>();
  let lastVersions = new Map<K, number>();
  
  return readable(fieldMap, (set) => {
    return this.dataStore.subscribe(($dataMap) => {
      let changed = false;
      
      for (const [key, versionedEntity] of $dataMap.entries()) {
        const currentVersion = versionedEntity.metadata.fieldVersions[fieldName] || 0;
        const lastVersion = lastVersions.get(key) || -1;
        
        if (currentVersion !== lastVersion) {
          changed = true;
          lastVersions.set(key, currentVersion);
          fieldMap.set(key, extractor(versionedEntity.data));
        }
      }
      
      // Handle deletions
      for (const key of fieldMap.keys()) {
        if (!$dataMap.has(key)) {
          changed = true;
          fieldMap.delete(key);
          lastVersions.delete(key);
        }
      }
      
      if (changed) {
        fieldMap = new Map(fieldMap); // Clone for reactivity
        set(fieldMap);
      }
    });
  });
}
```

**Severity**: 🟡 MEDIUM - Reduces effectiveness of field versioning

**Recommendation**: Implement Option C for maximum efficiency

---

## ✅ Issue 6: subscribeToFieldForKey Version Tracking

### Code (Lines 417-445)

```typescript
subscribeToFieldForKey<F>(
  key: K,
  fieldName: string,
  callback: (fieldValue: F | undefined, version: number) => void
): () => void {
  let lastVersion = -1;
  
  return this.dataStore.subscribe(($dataMap) => {
    const versionedEntity = $dataMap.get(key);
    if (!versionedEntity) {
      callback(undefined, -1);
      return;
    }
    
    const currentVersion = versionedEntity.metadata.fieldVersions[fieldName] || 0;
    
    // Only fire callback if version changed
    if (currentVersion !== lastVersion) {
      lastVersion = currentVersion;
      const fieldValue = extractor(versionedEntity.data);
      callback(fieldValue, currentVersion);
    }
  });
}
```

**Status**: ✅ CORRECT

**Verification**:
- ✅ Tracks version per field
- ✅ Only fires callback when version changes
- ✅ Handles entity deletion (callback with undefined)
- ✅ Correctly uses version comparison

This is actually the RIGHT pattern! The derived store should use this approach.

---

## ⚠️ Issue 7: Edge Case - Empty Field Extractors

### Scenario

```typescript
const store = createVersionedStore({
  fields: {},  // Empty!
  itcExtractor: (e) => e.stamp
});
```

### Current Behavior

- `fieldNames` = []
- `detectFieldChanges` returns empty `changedFields`
- Every update returns `{ applied: false, reason: 'No field changes' }`
- Metadata updates, but data never updates

### Is This Valid?

**Yes!** Could be used for:
- Pure causality tracking (no field data)
- Metadata-only stores
- ITC stamp aggregation

**Recommendation**: Document this as supported use case

**Severity**: 🟢 LOW - Edge case, but valid

---

## ⚠️ Issue 8: Entity Deletion with Undefined

### Code (Lines 310-327)

```typescript
delete(key: K): boolean {
  const currentMap = get(this.dataStore);
  if (!currentMap.has(key)) {
    return false; // Already absent
  }
  
  this.dataStore.update(map => {
    const newMap = new Map(map);
    newMap.delete(key);
    return newMap;
  });
  
  return true;
}
```

### Missing: Deletion Detection in Derived Stores

When entity is deleted:
- `deriveField()` doesn't remove the key from fieldMap
- Field map keeps stale data!

### Example

```
1. Alice publishes commitment → recognitionStore has alice entry
2. Alice deletes commitment → recognitionStore STILL has alice entry ❌
```

### Fix Needed in deriveField (Option C implementation)

```typescript
// Handle deletions
for (const key of fieldMap.keys()) {
  if (!$dataMap.has(key)) {
    changed = true;
    fieldMap.delete(key);
    lastVersions.delete(key);
  }
}
```

**Severity**: 🔴 HIGH - Causes stale data in derived stores

---

## Summary of Issues

| Issue | Severity | Status | Impact |
|-------|----------|--------|--------|
| 1. ITC Join Missing | 🔴 HIGH | ✅ FIXED | Loses causal history |
| 2. Timestamp + ITC Interaction | 🔴 HIGH | ✅ FIXED | Rejects valid concurrent updates |
| 3. Field Change Detection | ✅ OK | ✅ CORRECT | None |
| 4. Default Equality (Date/Map/Set) | 🟡 LOW | ✅ FIXED | Edge case - now handles special types |
| 5. Derived Store Efficiency | 🟡 MEDIUM | ✅ FIXED | Version-aware derived stores |
| 6. subscribeToFieldForKey | ✅ OK | ✅ CORRECT | None |
| 7. Empty Fields | 🟢 LOW | ✅ FIXED | Edge case - now warns with Zod validation |
| 8. Deletion in Derived Stores | 🔴 HIGH | ✅ FIXED | Stale data |

## Recommended Fixes (Priority Order)

### 1. 🔴 HIGH PRIORITY

#### Fix A: Timestamp Check (Lines 238-246)
Move timestamp check inside else block (when no ITC):

```typescript
if (entityITC && existing.metadata.itcStamp) {
  // ITC logic...
} else {
  // Only use timestamp when NO ITC
  if (entityTimestamp && existing.metadata.timestamp) {
    if (entityTimestamp <= existing.metadata.timestamp) {
      return { applied: false, reason: 'Timestamp staleness' };
    }
  }
}
```

#### Fix B: Derived Store Deletions (Lines 366-383)
Implement Option C with deletion handling.

### 2. 🟡 MEDIUM PRIORITY

#### Optimization: Version-Aware Derived Store
Implement Option C for `deriveField()` to only trigger when field version changes.

### 3. 🟢 LOW PRIORITY

#### Documentation: Edge Cases
- Document function/class instance comparison limitation
- Document empty fields as valid use case
- Add examples of custom equality checkers

## Test Cases Needed

```typescript
// Test 1: Concurrent updates with clock skew
test('accepts concurrent updates despite clock skew', () => {
  const store = createVersionedStore({ ... });
  
  const stampA = itcEvent(itcSeed());
  const stampB = itcEvent(itcSeed()); // Concurrent
  
  store.update('key', { data: 'A', stamp: stampA, timestamp: 1000 });
  const result = store.update('key', { data: 'B', stamp: stampB, timestamp: 999 });
  
  assert(result.applied); // Should accept despite older timestamp!
});

// Test 2: Entity deletion clears derived stores
test('deletion removes from derived stores', () => {
  const store = createVersionedStore({ ... });
  const fieldStore = store.deriveField('value');
  
  store.update('key', { value: 'A' });
  let fieldMap = get(fieldStore);
  assert(fieldMap.has('key'));
  
  store.delete('key');
  fieldMap = get(fieldStore);
  assert(!fieldMap.has('key')); // Should be removed!
});

// Test 3: Derived store only triggers on field change
test('derived store not triggered by other field changes', () => {
  const store = createVersionedStore({
    fields: { a: e => e.a, b: e => e.b }
  });
  const aStore = store.deriveField('a');
  
  let aTriggerCount = 0;
  aStore.subscribe(() => aTriggerCount++);
  
  store.update('key', { a: 1, b: 1 });
  const count1 = aTriggerCount;
  
  store.update('key', { a: 1, b: 2 }); // Only b changed
  const count2 = aTriggerCount;
  
  assert(count1 === count2); // Should NOT trigger!
});
```

