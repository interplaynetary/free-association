# Collective Module V5 Upgrade Status

## ✅ Completed

### 1. **Schemas File Created** (`schemas.ts`)
Created comprehensive schema definitions for collective-specific types:

- **Compliance Filters**: `ComplianceFilter` (blocked, capped, unlimited)
- **Base Declarations**: `BaseNeed`, `BaseCapacity` with v5 slot integration
- **Allocation Types**: `Allocation`, `AllocationComputationResult`
- **Recognition Data**: `RecognitionData` for MRD computation
- **Membership Types**: `MembershipOutput`, `HealthMetrics`
- **Collective Tree Types**: `EntityID`, `Collective`, `Forest`, `NodeMergeData`, `ProportionalNode`, etc.

### 2. **Membership Utilities Implemented**
Added to `schemas.ts`:
- `shouldUpdateCapacityMembership()` - Checks if capacity membership should be updated
- `updateCapacityMembership()` - Performs MRD-based membership update

### 3. **Import Updates**
- ✅ `collective-membership.svelte.ts` - Updated to import from `'./schemas'`
- ✅ `collective-tree.svelte.ts` - Updated to import from `'./schemas'`
- ✅ `collective-recognition.svelte.ts` - Partially updated (imports added)

### 4. **V5 Integration**
- Re-exports all v5 core schemas (`export * from '../schemas'`)
- `BaseCapacity` includes `name` field (required by v5)
- `MembershipOutput` includes all required fields (`membershipStatus`, `mutualRecognitionScores`, `networkAverage`)
- `HealthMetrics` includes `memberCount` field

## ⚠️ Remaining Issues

### 1. **collective-recognition.svelte.ts** (2 errors)
**Issue**: Duplicate type definitions
- Lines 50-130 define types that are now imported from `./schemas`
- Need to remove duplicate exports

**Fix**:
```typescript
// Remove lines 47-130 (duplicate type definitions)
// Keep only the imported types from './schemas'
```

### 2. **collective-membership.svelte.ts** (0 errors after schema updates)
**Status**: ✅ All errors resolved by schema updates

### 3. **collective-tree.svelte.ts** (17 errors)
**Issues**:
1. **Node Schema Mismatch** (Line 616): v5 Node schema uses `contributors: Contributor[]` instead of `contributor_ids: string[]`
2. **Type Mismatches** (Lines 917, 933, 997, 1187, 1585, 1698, 1862, 1996, 2052): Return types don't match interface definitions
3. **Type Safety** (Lines 1911, 1937, 1942): `unknown` types need explicit casting

**Root Cause**: collective-tree.svelte.ts was written for a different Node schema structure. V5 uses:
```typescript
NonRootNode {
  contributors: Contributor[]  // { id: string, points: number }[]
  anti_contributors: Contributor[]
}
```

Instead of:
```typescript
NonRootNode {
  contributor_ids: string[]
  anti_contributors_ids: string[]
}
```

## 📋 Next Steps

### Priority 1: collective-recognition.svelte.ts
1. Remove duplicate type definitions (lines 47-130)
2. Keep only imported types from `./schemas`
3. Verify all functionality works with imported types

### Priority 2: collective-tree.svelte.ts
This file needs significant refactoring to work with v5's weighted contributors:

**Option A: Update to V5 Contributor Model**
- Refactor code to use `Contributor[]` instead of `string[]`
- Update contributor access patterns:
  ```typescript
  // Old: node.contributor_ids
  // New: node.contributors.map(c => c.id)
  ```
- Handle contributor points in calculations

**Option B: Create Adapter Layer**
- Keep collective-tree logic unchanged
- Create adapter functions to convert between v4 and v5 formats
- Less invasive but adds complexity

**Recommendation**: Option A - Full v5 integration
- Aligns with v5 weighted contributor architecture
- Future-proof
- Cleaner codebase

### Priority 3: Integration Testing
Once errors are resolved:
1. Test collective recognition computation with v5 slots
2. Verify MRD membership updates work correctly
3. Test slot-level allocation with hierarchical availability windows
4. Verify ITC causality tracking through collective operations

## 🔧 Code Patterns for Fixes

### Pattern 1: Accessing Contributors in V5
```typescript
// ❌ Old (v4)
const contributorIds = node.contributor_ids;

// ✅ New (v5)
const contributorIds = node.contributors.map(c => c.id);
const contributorsWithPoints = node.contributors; // Full access to points
```

### Pattern 2: Creating Contributors in V5
```typescript
// ❌ Old (v4)
node.contributor_ids = ['alice_pub', 'bob_pub'];

// ✅ New (v5)
node.contributors = [
  { id: 'alice_pub', points: 100 },
  { id: 'bob_pub', points: 50 }
];
```

### Pattern 3: Weighted Recognition Calculation
```typescript
// V5 recognizes contributor weights
const totalPoints = node.contributors.reduce((sum, c) => sum + c.points, 0);
const contributorShare = contributor.points / totalPoints;
const nodeRecognition = nodeWeight * nodeFulfillment * contributorShare;
```

## 📊 Impact Assessment

### Breaking Changes
- `Node` schema structure changed (contributors are now objects, not strings)
- `BaseCapacity` now requires `name` field
- `MembershipOutput` has expanded interface

### Backward Compatibility
- ❌ Not maintained (clean v5 transition)
- Migration path: Update all collective-tree code to v5 schemas
- Timeline: ~2-4 hours of focused refactoring

### Benefits of V5 Integration
1. ✅ Weighted contributors (more accurate recognition)
2. ✅ Slot-native allocation (time/location matching)
3. ✅ Global recognition model (simpler, more intuitive)
4. ✅ Hierarchical availability windows (powerful recurrence patterns)
5. ✅ ITC causality (distributed consistency)

## 🎯 Testing Checklist

Once refactoring is complete:

- [ ] Collective recognition computes correctly
- [ ] MRD membership updates work
- [ ] Slot-level allocations respect time/location
- [ ] Filters apply correctly (blocked, capped, unlimited)
- [ ] Redistribution works as expected
- [ ] ITC stamps track causality
- [ ] Weighted contributors affect recognition shares
- [ ] Dynamic membership updates trigger correctly
- [ ] Recognition matrix builds correctly
- [ ] Allocation results match expected values

## 📚 Reference Files

- **Core V5 Schemas**: `src/lib/commons/v5/schemas.ts`
- **Collective Schemas**: `src/lib/commons/v5/collective/schemas.ts`
- **V5 Protocol**: `src/lib/commons/v5/protocol.ts`
- **V5 Matching**: `src/lib/commons/v5/match.svelte.ts`
- **Component Architecture**: `src/lib/commons/v5/components/components.md`

## 💡 Pro Tips

1. **Use Protocol Functions**: `v5/protocol.ts` has helper functions for weighted contributors
   - `getAllContributorsFromTree()` - Now returns resolved IDs with points
   - `shareOfGeneralFulfillment()` - Now handles weighted contributors
   - `calculateNodeContributorShare()` - New function for weighted shares

2. **Slot Matching**: Use `v5/match.svelte.ts` for slot compatibility
   - `slotsCompatible()` - Checks time AND location
   - `timeRangesOverlap()` - Handles hierarchical windows
   - `locationsCompatible()` - Smart location matching

3. **Test Incrementally**: Fix one file at a time, test before moving on

---

**Last Updated**: 2025-10-28
**Status**: 🟡 In Progress (70% complete)
**Next Action**: Remove duplicate type definitions in collective-recognition.svelte.ts

