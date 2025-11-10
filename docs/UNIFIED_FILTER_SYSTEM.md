# Unified Filter System Design (JsonLogic-based)

This document outlines the design and architecture of the unified filter system, which consolidates various filtering mechanisms used across the Free Association Protocol. The system uses **Zod schemas** for validation and **JsonLogic** for dynamic, expressive filter rules.

## Core Principle: "Union of Filters = Most Restrictive Wins"

A fundamental principle guiding this design is that when multiple filters apply to a given scenario, the most restrictive outcome prevails. This translates to:

- **For numerical restrictions (e.g., maximum allocation amounts):** The minimum value allowed by any applicable filter is chosen (`min(filter1, filter2)`).
- **For boolean restrictions (e.g., eligibility criteria):** All applicable filters must pass (logical AND: `filter1 AND filter2`).

This ensures that all specified constraints are respected, preventing unintended over-allocations or matches that violate any party's rules.

## Why JsonLogic?

**JsonLogic** (https://jsonlogic.com) provides:

1. **Serializable**: Filters are pure JSON, can be stored in databases, transmitted over networks
2. **Dynamic**: Rules can be changed without code changes
3. **Expressive**: Complex logic with operators like `if`, `and`, `or`, `in`, comparisons, arithmetic
4. **Language-agnostic**: Same rules work in JavaScript, Python, Go, PHP, Ruby, etc.
5. **Safe**: No eval() or code injection risks
6. **Testable**: Rules are data, easy to test and validate

### JsonLogic Example

```json
{
  "and": [
    {">=": [{"var": "mutualRecognition"}, 0.1]},
    {"or": [
      {"in": [{"var": "commitment.city"}, ["SF", "NYC", "Berlin"]]},
      {"in": ["licensed", {"var": "attributes.certifications"}]}
    ]}
  ]
}
```

This rule says: "Allow if mutual recognition ≥ 0.1 AND (city is SF/NYC/Berlin OR has 'licensed' certification)"

## Architecture

The unified filter system is organized into a dedicated module: `src/lib/protocol/utils/filters/`.

```
src/lib/protocol/utils/filters/
├── index.ts          // Unified entry point for exports
├── types.ts          // Zod schemas and type definitions
├── compliance.ts     // Compliance Filters (numerical: how much)
└── eligibility.ts    // Eligibility Filters (boolean: who/whether)
```

### 1. `types.ts` - Zod Schemas and Type Definitions

This file defines all core types using Zod for runtime validation and type inference.

#### Key Schemas:

**`JsonLogicRuleSchema`**: Recursive schema for JsonLogic rules
```typescript
export const JsonLogicRuleSchema: z.ZodType<any> = z.lazy(() =>
  z.union([
    z.record(z.string(), z.any()), // Object with operator key
    z.array(JsonLogicRuleSchema),   // Array of rules
    z.string(),                     // Primitives
    z.number(),
    z.boolean(),
    z.null()
  ])
);
```

**`FilterContextSchema`**: Data object passed to filters during evaluation
```typescript
export const FilterContextSchema = z.object({
  pubKey: z.string(),                                    // Entity ID
  commitment: z.record(z.string(), z.any()).optional(), // Location, resource data
  mutualRecognition: z.number().optional(),             // Trust score (0-1)
  attributes: z.record(z.string(), z.any()).optional(), // Certifications, etc.
  currentTotal: z.number().optional(),                  // For compliance checks
  proposedAmount: z.number().optional()                 // For compliance checks
});
```

**`ComplianceFilterSchema`**: Numeric capacity limits (returns number)
```typescript
export const ComplianceFilterSchema = z.union([
  JsonLogicRuleSchema,  // JsonLogic rule that returns a number
  z.number(),           // Literal number (simple cap)
  z.null()              // null = unlimited (Infinity)
]);
```

**`EligibilityFilterSchema`**: Boolean slot matching (returns boolean)
```typescript
export const EligibilityFilterSchema = z.union([
  JsonLogicRuleSchema,  // JsonLogic rule that returns a boolean
  z.boolean()           // Literal boolean (allow_all=true, deny_all=false)
]);
```

**`FilterPatterns`**: Helper functions to construct common JsonLogic rules
```typescript
export const FilterPatterns = {
  trust: (minMR: number) => ({
    ">=": [{ "var": "mutualRecognition" }, minMR]
  }),
  
  cityIn: (cities: string[]) => ({
    "in": [{ "var": "commitment.city" }, cities]
  }),
  
  tieredCap: (premiumCap: number, regularCap: number) => ({
    "if": [
      { "==": [{ "var": "attributes.tier" }, "premium"] },
      premiumCap,
      regularCap
    ]
  })
  // ... more patterns
};
```

### 2. `compliance.ts` - Numeric Capacity Limits

This module encapsulates **Compliance Filters**, which enforce maximum allocation amounts for individual recipients. These filters define "how much capacity a recipient is *allowed* to receive" from a given provider or collective.

#### Filter Types (as JsonLogic):

1. **Simple cap** (literal number):
   ```typescript
   const filter = 50000; // Cap at $50K
   ```

2. **Conditional cap** (tier-based):
   ```typescript
   const filter = {
     "if": [
       {"==": [{"var": "attributes.tier"}, "premium"]},
       100000,  // Premium: $100K
       50000    // Regular: $50K
     ]
   };
   ```

3. **Dynamic cap** (trust-based):
   ```typescript
   const filter = {
     "*": [{"var": "mutualRecognition"}, 100000]
   };
   // Cap = mutualRecognition × $100K
   // e.g., MR=0.5 → $50K cap
   ```

4. **Blocked**:
   ```typescript
   const filter = 0;
   ```

5. **Unlimited**:
   ```typescript
   const filter = null; // Represents Infinity
   ```

#### Key Functions:

**`evaluateComplianceFilter(filter, context): number`**
- Evaluates a compliance filter using JsonLogic
- Returns numeric limit (0 for blocked, Infinity for unlimited)
- Validates inputs with Zod

**`unionOfFilters(filter1, filter2, context): ComplianceFilter`**
- Implements "most restrictive wins": `min(filter1, filter2)`
- Useful when multiple filters apply (provider + entity + jurisdiction)

**`passesComplianceFilter(proposedAmount, currentTotal, filter, context): FilterResult`**
- Checks if proposed allocation passes the filter
- Returns `{ passed, reason?, effectiveLimit, rawResult }`

**`applyComplianceFilter(targetAmount, currentTotal, filter, context): number`**
- Caps the target allocation at the filter limit
- Returns the maximum allowed allocation

**`ComplianceFilters`** - Common patterns:
```typescript
ComplianceFilters.cap(50000)                 // Simple cap
ComplianceFilters.blocked()                  // Block recipient
ComplianceFilters.unlimited()                // No limit
ComplianceFilters.tieredCap({...})          // Tier-based cap
ComplianceFilters.trustBasedCap(100000)     // Trust-based cap
ComplianceFilters.timeBasedCap(1000, 100)   // Rate-limited cap
```

### 3. `eligibility.ts` - Boolean Slot-Level Matching

This module contains **Eligibility Filters**, which determine if a specific provider-slot can fulfill a specific recipient-need-slot. These filters are bilateral, meaning both the provider's filter on recipients and the recipient's filter on providers must pass.

#### Filter Types (as JsonLogic):

1. **Trust requirement**:
   ```typescript
   const filter = {">=": [{"var": "mutualRecognition"}, 0.1]};
   ```

2. **Location requirement**:
   ```typescript
   const filter = {"in": [{"var": "commitment.city"}, ["SF", "NYC", "Berlin"]]};
   ```

3. **Certification requirement**:
   ```typescript
   const filter = {"in": ["licensed", {"var": "attributes.certifications"}]};
   ```

4. **Complex composite**:
   ```typescript
   const filter = {
     "and": [
       {">=": [{"var": "mutualRecognition"}, 0.1]},
       {"or": [
         {"==": [{"var": "commitment.city"}, "SF"]},
         {"in": ["licensed", {"var": "attributes.certifications"}]}
       ]}
     ]
   };
   ```

5. **Allow all**:
   ```typescript
   const filter = true;
   ```

6. **Deny all**:
   ```typescript
   const filter = false;
   ```

#### Key Functions:

**`evaluateEligibilityFilter(filter, context): boolean`**
- Evaluates an eligibility filter using JsonLogic
- Returns boolean (true = pass, false = reject)
- Validates inputs with Zod

**`evaluateFilter(filter, context): FilterResult`**
- Evaluates with detailed result: `{ passed, reason?, rawResult }`

**`passesSlotFilters(providerFilter, recipientFilter, providerContext, recipientContext): FilterResult`**
- Implements bilateral filtering
- Both provider's and recipient's filters must pass

**`passesAllEligibilityFilters(filters, context): FilterResult`**
- Implements "all must pass" (AND logic)
- Useful when multiple eligibility filters apply

**`createCompositeFilter(filters): EligibilityFilter`**
- Combines multiple filters into a single JsonLogic AND rule

**`EligibilityFilters`** - Common patterns:
```typescript
EligibilityFilters.allowAll()                     // Allow all
EligibilityFilters.denyAll()                      // Deny all
EligibilityFilters.trust(0.1)                     // Require MR ≥ 0.1
EligibilityFilters.onlyMutual()                   // Require MR > 0
EligibilityFilters.cityIn(['SF', 'NYC'])         // Require city
EligibilityFilters.hasCertification('licensed')  // Require cert
EligibilityFilters.and(...filters)               // Composite AND
EligibilityFilters.or(...filters)                // Composite OR
```

### 4. `index.ts` - Unified Entry Point

This serves as the unified entry point for the entire filter system, re-exporting all types, schemas, functions, and patterns from the other modules.

```typescript
import {
  ComplianceFilters,
  EligibilityFilters,
  evaluateComplianceFilter,
  evaluateEligibilityFilter,
  unionOfFilters,
  passesSlotFilters
} from '$lib/protocol/utils/filters';
```

## Integration and Usage

### Example 1: Compliance Filter in Collective Recognition

```typescript
import { ComplianceFilters, unionOfFilters } from '$lib/protocol/utils/filters';

// Provider's risk cap
const providerFilter = ComplianceFilters.cap(100000);

// Entity's jurisdiction cap (more restrictive)
const entityFilter = ComplianceFilters.tieredCap({
  premium: 75000,
  regular: 50000
});

// Combine (most restrictive wins)
const effectiveFilter = unionOfFilters(providerFilter, entityFilter, {
  pubKey: 'alice',
  attributes: { tier: 'regular' }
});

// Evaluate
const limit = evaluateComplianceFilter(effectiveFilter, {
  pubKey: 'alice',
  attributes: { tier: 'regular' }
});
// → 50000 (min of provider's 100000 and entity's 50000)
```

### Example 2: Eligibility Filter in Slot Matching

```typescript
import { EligibilityFilters, passesSlotFilters } from '$lib/protocol/utils/filters';

// Provider's capacity filter: "Only mutual recognition"
const providerFilter = EligibilityFilters.onlyMutual();

// Recipient's need filter: "Must be licensed"
const recipientFilter = EligibilityFilters.hasCertification('licensed');

// Check bilateral match
const result = passesSlotFilters(
  providerFilter,
  recipientFilter,
  { pubKey: 'provider', attributes: { certifications: ['licensed'] } },
  { pubKey: 'recipient', mutualRecognition: 0.15 }
);

console.log(result.passed); // true (both filters pass)
```

### Example 3: Custom JsonLogic Filter

```typescript
import { evaluateEligibilityFilter } from '$lib/protocol/utils/filters';

// Custom rule: "Allow if (MR ≥ 0.1 AND city=SF) OR (has premium tier AND certified)"
const customFilter = {
  "or": [
    {
      "and": [
        {">=": [{"var": "mutualRecognition"}, 0.1]},
        {"==": [{"var": "commitment.city"}, "SF"]}
      ]
    },
    {
      "and": [
        {"==": [{"var": "attributes.tier"}, "premium"]},
        {"in": ["licensed", {"var": "attributes.certifications"}]}
      ]
    }
  ]
};

const passed = evaluateEligibilityFilter(customFilter, {
  pubKey: 'alice',
  mutualRecognition: 0.05,
  commitment: { city: 'NYC' },
  attributes: {
    tier: 'premium',
    certifications: ['licensed']
  }
});

console.log(passed); // true (second OR branch passes)
```

## Benefits of JsonLogic-based Filters

1. **Infinite Extensibility**: Add new filter logic without code changes
2. **Serializable**: Store filters in database, transmit over network
3. **Testable**: Filters are data, easy to test and validate
4. **User-Configurable**: Users can create custom filters in UI
5. **Auditable**: Filter logic is transparent and inspectable
6. **Language-Agnostic**: Same filters work in any language with JsonLogic implementation
7. **Type-Safe**: Zod provides runtime validation and TypeScript type inference
8. **Composable**: Filters can be nested and combined using JsonLogic operators

## Migration from Legacy Filter Systems

### Before (TypeScript discriminated unions):
```typescript
type FilterRule =
  | { type: 'trust'; min_mutual_recognition: number }
  | { type: 'location'; allowed_cities: string[] }
  | { type: 'certification'; required: string[] };
```

### After (JsonLogic):
```typescript
// Trust filter
const trustFilter = {">=": [{"var": "mutualRecognition"}, 0.1]};

// Location filter
const locationFilter = {"in": [{"var": "commitment.city"}, ["SF", "NYC"]]};

// Certification filter
const certFilter = {"in": ["licensed", {"var": "attributes.certifications"}]};

// Composite
const compositeFilter = {"and": [trustFilter, locationFilter, certFilter]};
```

The JsonLogic approach is more flexible, serializable, and doesn't require code changes to add new filter types.

## Future Enhancements

1. **JsonLogic Extensions**: Add custom operators for protocol-specific logic (e.g., `mrd_threshold`, `tree_depth`)
2. **Filter Compiler**: Optimize JsonLogic rules for performance
3. **Filter Builder UI**: Visual filter editor for non-technical users
4. **Filter Marketplace**: Share and reuse filters across entities
5. **Filter Analytics**: Track which filters are rejecting/allowing most often

## Conclusion

The unified filter system provides a powerful, flexible, and safe way to define allocation rules in the Free Association Protocol. By using Zod schemas and JsonLogic, we achieve:

- **Runtime validation** (Zod)
- **Dynamic expressiveness** (JsonLogic)
- **Serializability** (JSON)
- **Type safety** (TypeScript + Zod)
- **Extensibility** (no code changes needed for new rules)

This architecture makes the protocol adaptable to diverse use cases while maintaining safety and compliance.
