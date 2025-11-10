# JsonLogic Filter System Migration

## Overview

The Free Association Protocol's filter system has been completely refactored to use **Zod schemas** for runtime validation and **JsonLogic** for dynamic, expressive filter rules.

## What Changed

### Before: TypeScript Discriminated Unions

```typescript
// Old compliance filter format
type ComplianceFilter =
  | { type: 'blocked'; value: 0 }
  | { type: 'capped'; value: number }
  | { type: 'unlimited' };

// Old eligibility filter format
type FilterRule =
  | { type: 'trust'; min_mutual_recognition: number }
  | { type: 'location'; allowed_cities: string[] }
  | { type: 'certification'; required: string[] };

// Hard-coded filter evaluation
function evaluateFilter(filter: FilterRule, context: FilterContext): boolean {
  switch (filter.type) {
    case 'trust': /* ... */
    case 'location': /* ... */
    // etc.
  }
}
```

**Problems with old approach:**
- ❌ Hard-coded filter types (need code changes to add new filters)
- ❌ Not serializable (can't store complex logic in database)
- ❌ Not runtime-validated (TypeScript types only)
- ❌ Not extensible (users can't define custom filters)
- ❌ Language-specific (doesn't work across different implementations)

### After: Zod + JsonLogic

```typescript
// New compliance filter format (JsonLogic rule returning number)
const complianceFilter: ComplianceFilter = {
  "if": [
    {"==": [{"var": "attributes.tier"}, "premium"]},
    100000,  // Premium: $100K
    50000    // Regular: $50K
  ]
};

// New eligibility filter format (JsonLogic rule returning boolean)
const eligibilityFilter: EligibilityFilter = {
  "and": [
    {">=": [{"var": "mutualRecognition"}, 0.1]},
    {"or": [
      {"in": [{"var": "commitment.city"}, ["SF", "NYC"]]},
      {"in": ["licensed", {"var": "attributes.certifications"}]}
    ]}
  ]
};

// Generic JsonLogic evaluation
import jsonLogic from 'json-logic-js';
const result = jsonLogic.apply(filter, context);
```

**Benefits of new approach:**
- ✅ Infinitely extensible (no code changes needed)
- ✅ Fully serializable (pure JSON)
- ✅ Runtime validated (Zod schemas)
- ✅ User-configurable (define filters in UI)
- ✅ Language-agnostic (same rules work everywhere)
- ✅ Auditable (transparent logic)
- ✅ Testable (filters are data)

## Migration Guide

### Compliance Filters (Numeric Limits)

#### Old Format → New Format

```typescript
// BEFORE (discriminated union)
const blocked = { type: 'blocked', value: 0 };
const capped = { type: 'capped', value: 50000 };
const unlimited = { type: 'unlimited' };

// AFTER (JsonLogic / literals)
const blocked = 0;
const capped = 50000;
const unlimited = null;
```

#### Old Usage → New Usage

```typescript
// BEFORE
import { getFilterValue } from './old-compliance-filters';
const limit = getFilterValue(filter); // No context needed

// AFTER
import { evaluateComplianceFilter } from '$lib/protocol/utils/filters';
const limit = evaluateComplianceFilter(filter, context);

// Or use the backward-compatible getFilterValue (handles old format too)
import { getFilterValue } from '$lib/protocol/utils/filters';
const limit = getFilterValue(filter, context); // Works with old and new formats
```

### Eligibility Filters (Boolean Matching)

#### Old Format → New Format

```typescript
// BEFORE
const trustFilter = {
  type: 'trust',
  min_mutual_recognition: 0.1
};

const locationFilter = {
  type: 'location',
  allowed_cities: ['SF', 'NYC']
};

// AFTER (JsonLogic)
const trustFilter = {
  ">=": [{"var": "mutualRecognition"}, 0.1]
};

const locationFilter = {
  "in": [{"var": "commitment.city"}, ["SF", "NYC"]]
};
```

#### Old Usage → New Usage

```typescript
// BEFORE
import { evaluateFilter } from './old-eligibility-filters';
const passed = evaluateFilter(filter, context); // Switch statement

// AFTER
import { evaluateEligibilityFilter } from '$lib/protocol/utils/filters';
const passed = evaluateEligibilityFilter(filter, context); // JsonLogic
```

## Backward Compatibility

The new system is **backward compatible** with the old discriminated union format. The following functions automatically detect and handle legacy filters:

- `getFilterValue(filter, context)` - Works with both old and new compliance filters
- `unionOfFilters(filter1, filter2, context)` - Works with both formats
- `evaluateEligibilityFilter(filter, context)` - Gracefully handles legacy formats

### Migration Helper

```typescript
import { migrateLegacyComplianceFilter } from '$lib/protocol/collective/schemas';

// Convert old format to new format
const oldFilter = { type: 'capped', value: 50000 };
const newFilter = migrateLegacyComplianceFilter(oldFilter);
// → 50000 (literal number)
```

## New Capabilities

### 1. Conditional Logic

```typescript
// Tiered caps based on membership level
const filter = {
  "if": [
    {"==": [{"var": "attributes.tier"}, "premium"]},
    100000,
    {"if": [
      {"==": [{"var": "attributes.tier"}, "regular"]},
      50000,
      25000  // basic tier
    ]}
  ]
};
```

### 2. Dynamic Calculations

```typescript
// Cap scales with mutual recognition
const filter = {
  "*": [{"var": "mutualRecognition"}, 100000]
};
// MR=0.5 → $50K, MR=0.8 → $80K
```

### 3. Complex Boolean Logic

```typescript
// Require (trust AND location) OR certification
const filter = {
  "or": [
    {
      "and": [
        {">=": [{"var": "mutualRecognition"}, 0.1]},
        {"in": [{"var": "commitment.city"}, ["SF", "NYC"]]}
      ]
    },
    {"in": ["licensed", {"var": "attributes.certifications"}]}
  ]
};
```

### 4. Array Operations

```typescript
// Require ALL certifications
const filter = {
  "all": [
    ["licensed", "insured", "bonded"],
    {"in": [{"var": ""}, {"var": "attributes.certifications"}]}
  ]
};
```

### 5. Time-Based Rules

```typescript
// Cap increases over time
const filter = {
  "+": [
    10000,  // Base
    {"*": [
      {"/": [{"-": [Date.now(), 1704067200000]}, 86400000]}, // Days
      1000  // Per day increase
    ]}
  ]
};
```

## API Reference

### Compliance Filters

```typescript
import {
  ComplianceFilterSchema,           // Zod schema
  type ComplianceFilter,             // TypeScript type
  evaluateComplianceFilter,          // Evaluate to number
  getFilterValue,                    // Legacy-compatible evaluation
  createFilter,                      // Create from number
  unionOfFilters,                    // Most restrictive wins
  passesComplianceFilter,            // Check allocation
  getRemainingRoom,                  // Calculate room
  applyComplianceFilter,             // Cap allocation
  ComplianceFilters                  // Helper patterns
} from '$lib/protocol/utils/filters';
```

### Eligibility Filters

```typescript
import {
  EligibilityFilterSchema,           // Zod schema
  type EligibilityFilter,             // TypeScript type
  evaluateEligibilityFilter,          // Evaluate to boolean
  evaluateFilter,                     // Detailed result
  passesSlotFilters,                  // Bilateral filtering
  passesAllEligibilityFilters,        // Union (AND logic)
  createCompositeFilter,              // Combine filters
  EligibilityFilters                  // Helper patterns
} from '$lib/protocol/utils/filters';
```

### Helper Patterns

```typescript
// Compliance helpers
ComplianceFilters.cap(50000);
ComplianceFilters.blocked();
ComplianceFilters.unlimited();
ComplianceFilters.tieredCap({ premium: 100000, regular: 50000 });
ComplianceFilters.trustBasedCap(100000);

// Eligibility helpers
EligibilityFilters.trust(0.1);
EligibilityFilters.onlyMutual();
EligibilityFilters.cityIn(['SF', 'NYC']);
EligibilityFilters.hasCertification('licensed');
EligibilityFilters.and(...filters);
EligibilityFilters.or(...filters);
```

## Testing

All existing tests pass with the new filter system. The backward compatibility layer ensures no breaking changes.

```bash
bun test src/lib/protocol/tests/allocation.test.ts
# ✅ 110 pass, 3 todo, 0 fail
```

## Examples

See comprehensive examples in:
- **Documentation**: `docs/UNIFIED_FILTER_SYSTEM.md`
- **Code Examples**: `docs/examples/jsonlogic-filters.ts`
- **Tests**: `src/lib/protocol/tests/allocation.test.ts`

## Next Steps

1. ✅ Create unified filter system with Zod + JsonLogic
2. ✅ Add backward compatibility layer
3. ✅ Update collective/schemas.ts
4. ✅ Write documentation and examples
5. ✅ Verify all tests pass
6. ⏳ Update `match.ts` to use unified filters
7. ⏳ Update `allocation.ts` to use ComplianceFilters
8. ⏳ Refactor `allocation.ts` to accept `DistributionResult`
9. ⏳ Update `collective-recognition.ts` to use unified allocation engine

## Resources

- **JsonLogic Documentation**: https://jsonlogic.com/operations.html
- **JsonLogic Playground**: https://jsonlogic.com/play.html
- **Zod Documentation**: https://zod.dev/
- **Filter System Docs**: `docs/UNIFIED_FILTER_SYSTEM.md`

## Questions?

For questions or issues, see:
- `docs/UNIFIED_FILTER_SYSTEM.md` - Full architecture documentation
- `docs/examples/jsonlogic-filters.ts` - 17 working examples
- `src/lib/protocol/utils/filters/` - Source code

---

**Status**: ✅ Complete (Phase 1)  
**Date**: 2025-11-10  
**Breaking Changes**: None (backward compatible)  
**Tests**: All passing

