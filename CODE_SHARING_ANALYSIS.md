# Code Sharing Analysis: Collective Schedulers

## ✅ Excellent Sharing - Computational Logic

Both schedulers share the **same mathematical algorithms** from `.svelte.ts` modules:

### Shared Algorithm Modules

```typescript
// ✅ Membership & MRD Computation (both schedulers use this)
import { MRDMembershipModule } from '$lib/protocol/collective/collective-membership.svelte';

// ✅ Allocation Computation (both schedulers use this)
import { 
  computeAllocations,
  calculateCollectiveRecognitionShares
} from '$lib/protocol/collective/collective-recognition.svelte';

// ✅ Tree Operations (tree scheduler uses this)
import {
  mergeContributorTrees,
  calculateCollectiveRecognition,
  calculateCollectiveCapacityAllocation
} from '$lib/protocol/collective/collective-tree.svelte';

// ✅ Tree Utilities (both use this)
import { sharesOfGeneralFulfillmentMap } from '$lib/protocol/tree';
```

**Why this is good:**
- ✅ Same algorithms run on frontend and backend
- ✅ Mathematical consistency guaranteed
- ✅ Single source of truth for computations
- ✅ No duplication of complex logic

## ✅ Newly Improved - Data Access Patterns

Created `shared-utils.ts` to eliminate duplication:

### Before Refactoring (❌ Duplication)

Both callbacks independently implemented:
- Recognition data extraction from trees
- Tree fetching from Holster
- Capacity extraction from nodes
- Logging patterns

**~200 lines of duplicated code**

### After Refactoring (✅ Shared)

```typescript
// src/lib/server/collective/shared-utils.ts

// ✅ Shared recognition extraction
export async function fetchAllRecognitionData(): Promise<RecognitionData[]>
export function extractRecognitionFromTree(userId: string, tree: Node): RecognitionData[]

// ✅ Shared tree fetching
export async function fetchTree(userId: string): Promise<Node | null>
export async function fetchTrees(userIds: string[]): Promise<Map<string, Node>>
export async function fetchTreesAsRecord(userIds: string[]): Promise<Record<string, Node>>

// ✅ Shared capacity extraction
export function extractCapacitiesFromTree(tree: Node): Record<string, number>
export async function fetchAllIndividualCapacities(): Promise<Record<string, Record<string, number>>>

// ✅ Shared logging
export async function logComputationEvent(collection, event, data): Promise<void>

// ✅ Shared validation
export async function validateBasicDataAccess(): Promise<ValidationResult>

// ✅ Shared storage patterns
export async function saveComputationResult(collection, id, result, latestCollection): Promise<void>
```

**Result: ~60 lines of shared utilities instead of ~200 lines duplicated**

## Code Sharing Breakdown

### Collective Recognition & Membership Scheduler

**Now uses shared utilities:**
- ✅ `fetchRecognitionData()` → `SharedUtils.fetchAllRecognitionData()`
- ✅ `fetchMemberTrees()` → `SharedUtils.fetchTrees()`
- ✅ `logComputation()` → `SharedUtils.logComputationEvent()`

**Still custom (specific to membership/allocation):**
- `fetchAutoUpdateCapacities()` - Filters by `auto_update_members_by_mrd`
- `saveCapacityMembers()` - Membership-specific storage
- `fetchCapacitiesForAllocation()` - Filters by capacity slots
- `fetchNeeds()` - Need-specific queries
- `saveAllocations()` - Allocation-specific storage

### Collective Tree Scheduler

**Now uses shared utilities:**
- ✅ `fetchContributorTrees()` → `SharedUtils.fetchTreesAsRecord()`
- ✅ `fetchIndividualCapacities()` → `SharedUtils.fetchAllIndividualCapacities()`
- ✅ `logComputation()` → `SharedUtils.logComputationEvent()`

**Still custom (specific to tree operations):**
- `fetchAutoMergeCollectives()` - Filters by `auto_merge: true`
- `saveCollectiveTree()` - Tree-specific storage with merge stats
- `fetchCollectiveTrees()` - Collective tree queries
- `saveCollectiveRecognition()` - Recognition-specific storage
- `saveCapacityAllocation()` - Tree allocation storage

## Duplication Eliminated

### Recognition Data Extraction: -45 lines

**Before:**
```typescript
// In collective/callbacks.ts (duplicated)
const treesData = await holsterGet(['trees']);
for (const [userId, userTrees] of Object.entries(treesData)) {
  const tree = await holsterGet(['trees', userId, 'recognition_tree']);
  const shares = sharesOfGeneralFulfillmentMap(tree);
  // ... conversion to RecognitionData
}

// In collective-tree/callbacks.ts (duplicated)
// Similar logic but for different purpose
```

**After:**
```typescript
// Both schedulers use:
return SharedUtils.fetchAllRecognitionData();
```

### Tree Fetching: -30 lines

**Before:**
```typescript
// In collective/callbacks.ts (duplicated)
for (const memberId of memberIds) {
  const tree = await holsterGet(['trees', memberId, 'recognition_tree']);
  if (tree) trees.set(memberId, tree);
}

// In collective-tree/callbacks.ts (duplicated)
for (const contributorId of contributorIds) {
  const tree = await holsterGet(['trees', contributorId, 'recognition_tree']);
  if (tree) trees[contributorId] = tree;
}
```

**After:**
```typescript
// Collective scheduler:
return SharedUtils.fetchTrees(memberIds);

// Tree scheduler:
return SharedUtils.fetchTreesAsRecord(contributorIds);
```

### Capacity Extraction: -50 lines

**Before:**
```typescript
// In collective-tree/callbacks.ts (45 lines)
const usersData = await holsterGet(['trees']);
for (const [userId, _] of Object.entries(usersData)) {
  const tree = await holsterGet(['trees', userId, 'recognition_tree']);
  if (tree && 'capacities' in tree) {
    // Extract capacity totals from slots
    for (const [capacityType, capacity] of Object.entries(tree.capacities)) {
      // ... extraction logic
    }
  }
}
```

**After:**
```typescript
// Tree scheduler:
return SharedUtils.fetchAllIndividualCapacities();
```

### Logging: -20 lines

**Before:**
```typescript
// In collective/callbacks.ts (duplicated)
const logKey = `${event}_${timestamp.getTime()}`;
await holsterNextPut('computation_logs', logKey, { event, data, timestamp });
await holsterNextPut('computation_logs_latest', event, { ...data, timestamp });

// In collective-tree/callbacks.ts (duplicated)
const logKey = `${event}_${timestamp.getTime()}`;
await holsterNextPut('collective_tree_computation_logs', logKey, { event, data, timestamp });
await holsterNextPut('collective_tree_computation_logs_latest', event, { ...data, timestamp });
```

**After:**
```typescript
// Both schedulers:
return SharedUtils.logComputationEvent(collection, event, data);
```

## Holster Database Utilities (Already Shared) ✅

Both schedulers already share low-level Holster operations:

```typescript
// From src/lib/server/holster/db.ts (already shared before this refactor)
import { 
  holsterGet,           // ✅ Used by both
  holsterNextPut,       // ✅ Used by both
  holsterGetArray,      // ✅ Used by both
  ensureAuthenticated   // ✅ Used by both
} from '$lib/server/holster/db';
```

**This was already good!** We built on top of these shared primitives.

## Summary: Code Sharing Quality

### Level 1: Algorithm Logic (Perfect ✨)
- ✅ **100% shared** computational algorithms
- ✅ Same `.svelte.ts` modules used by frontend and backend
- ✅ Mathematical consistency guaranteed

### Level 2: Data Access Patterns (Now Excellent ✨)
- ✅ **Shared utilities** for common patterns (NEW!)
- ✅ ~145 lines of duplication eliminated
- ✅ DRY principle properly applied
- ✅ Maintainability greatly improved

### Level 3: Database Primitives (Already Perfect ✅)
- ✅ Shared Holster utilities from the start
- ✅ Consistent low-level operations

### Level 4: Scheduler-Specific Logic (Appropriately Custom ✅)
- ✅ Each scheduler has domain-specific callbacks
- ✅ No forced sharing where it doesn't make sense
- ✅ Clear separation of concerns

## Benefits of This Architecture

### 1. Maintainability
- ✅ Fix a bug once, it's fixed everywhere
- ✅ Shared utilities have single source of truth
- ✅ Clear dependency hierarchy

### 2. Testability
- ✅ Shared utilities can be tested independently
- ✅ Mock shared utilities for scheduler tests
- ✅ Consistent behavior across schedulers

### 3. Performance
- ✅ No redundant code shipped
- ✅ Reusable patterns reduce bundle size
- ✅ Efficient data fetching patterns

### 4. Developer Experience
- ✅ Clear where to find common logic
- ✅ Easy to add new schedulers
- ✅ Consistent patterns to learn

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────┐
│         Computational Logic (Shared .svelte.ts)         │
│  • collective-membership.svelte.ts (MRD)                │
│  • collective-recognition.svelte.ts (Allocation)        │
│  • collective-tree.svelte.ts (Tree Merging)             │
│  • tree.ts (Recognition utilities)                      │
└────────────────┬────────────────────────────────────────┘
                 │
                 │ imported by both
                 │
    ┌────────────┴────────────┐
    │                         │
┌───▼──────────────┐  ┌───────▼────────────┐
│  Membership &    │  │  Collective Tree   │
│  Allocation      │  │  Operations        │
│  Scheduler       │  │  Scheduler         │
└───┬──────────────┘  └───────┬────────────┘
    │                         │
    │ uses                    │ uses
    │                         │
    └────────────┬────────────┘
                 │
         ┌───────▼────────┐
         │ Shared Utils   │
         │ (NEW!)         │
         │ • Recognition  │
         │ • Trees        │
         │ • Capacities   │
         │ • Logging      │
         └───┬────────────┘
             │
             │ uses
             │
      ┌──────▼─────────┐
      │ Holster DB     │
      │ Utilities      │
      │ • holsterGet   │
      │ • holsterPut   │
      │ • etc.         │
      └────────────────┘
```

## Conclusion

**Yes, the various computations are now properly sharing logic where possible! ✅**

We have:
1. ✅ **Perfect sharing** at the algorithm level (was already good)
2. ✅ **Excellent sharing** at the data access level (newly improved)
3. ✅ **Appropriate separation** where schedulers need custom logic
4. ✅ **Clear architecture** with proper layering

The refactoring eliminated ~145 lines of duplication while maintaining clear separation of concerns. Each scheduler can still customize what it needs, but common patterns are now in one place.

