# JsonLogic Efficiency Analysis

## The Efficiency Question

**When does JsonLogic make sense, and when doesn't it?**

This is a critical architectural question. JsonLogic provides flexibility at the cost of runtime overhead. Let's analyze where we use it and whether it's appropriate.

---

## JsonLogic Trade-offs

### Pros ✅
- **Dynamic**: Rules can change without code deployment
- **Serializable**: Can be stored in database, transmitted over network
- **User-configurable**: End users can define their own logic
- **Infinitely extensible**: Any conditional logic expressible
- **Cross-system**: Rules work across languages (JS, Python, etc.)
- **Version-controllable**: Rules can be versioned separately from code

### Cons ❌
- **Runtime overhead**: Parsing and evaluation cost
- **Less type-safe**: Errors caught at runtime, not compile time
- **Harder to debug**: Stack traces less clear
- **Performance**: ~10-100x slower than native code for complex rules
- **Bundle size**: Adds `json-logic-js` dependency (~5KB minified)

---

## Current Usage in Our Codebase

### 1. ComplianceFilter (Recipient Caps)

**Location**: `src/lib/protocol/utils/filters/compliance.ts`

**Usage Pattern**:
```typescript
// Evaluated inside allocation loop (hot path!)
filterLimit = evaluateComplianceFilter(filter, {
  pubKey: recipient.pubKey,
  currentTotal,
  proposedAmount: rawAllocation,
  commitment: recipientCommitment,
  mutualRecognition: mutualRecognition[recipient.pubKey],
  attributes: recipientCommitment.attributes || {}
});
```

**Frequency**: O(R × P × S) where:
- R = recipients (~10-100)
- P = passes (~2-5)
- S = slots per recipient (~1-10)
- **Total**: ~100-5000 evaluations per allocation cycle

**Is JsonLogic appropriate here?**
- ✅ Yes for **user-defined filters** (entity caps, jurisdiction limits)
- ❌ No for **static caps** (hardcoded limits)

### 2. EligibilityFilter (Slot Matching)

**Location**: `src/lib/protocol/utils/filters/eligibility.ts`

**Usage Pattern**:
```typescript
// Evaluated during slot compatibility check (hot path!)
const filterResult = evaluateFilter(filter, context);
if (!filterResult.passed) {
  return false;
}
```

**Frequency**: O(C × R × S) where:
- C = capacity slots (~5-20)
- R = potential recipients (~10-100)
- S = need slots per recipient (~1-10)
- **Total**: ~500-20,000 evaluations per allocation cycle

**Is JsonLogic appropriate here?**
- ✅ Yes for **provider/recipient preferences** (trust, location filters)
- ❌ No for **structural matching** (type compatibility, time overlap)

---

## Performance Benchmarks

Based on typical JsonLogic performance:

### Simple Literal Values (✅ FAST)
```typescript
// Literal number: ~0.001ms
const filter = 50000;

// Literal boolean: ~0.001ms
const filter = true;
```

### Simple JsonLogic Rules (⚠️ MODERATE)
```typescript
// Simple comparison: ~0.01-0.05ms
const filter = {">=": [{"var": "mutualRecognition"}, 0.1]};

// Simple AND/OR: ~0.05-0.1ms
const filter = {
  "and": [
    {">=": [{"var": "mutualRecognition"}, 0.1]},
    {"in": [{"var": "commitment.city"}, ["SF", "NYC"]]}
  ]
};
```

### Complex JsonLogic Rules (❌ SLOW)
```typescript
// Nested conditionals: ~0.5-2ms
const filter = {
  "if": [
    {"==": [{"var": "attributes.tier"}, "premium"]},
    {"*": [{"var": "mutualRecognition"}, 100000]},
    {"if": [
      {">=": [{"var": "attributes.reputation"}, 0.8]},
      50000,
      25000
    ]}
  ]
};

// Array operations: ~1-5ms
const filter = {
  "all": [
    {"var": "attributes.certifications"},
    {"in": [{"var": ""}, ["licensed", "certified", "verified"]]}
  ]
};
```

### Impact on Allocation
- **100 recipients × 3 passes × simple filter**: ~3-15ms overhead ⚠️
- **100 recipients × 3 passes × complex filter**: ~150-600ms overhead ❌

---

## Recommendations

### ✅ Use JsonLogic When:

1. **User-Defined Rules**
   ```typescript
   // Provider setting their own trust filter
   const providerFilter = {">=": [{"var": "mutualRecognition"}, 0.2]};
   
   // Entity setting jurisdiction caps
   const entityCap = {
     "if": [
       {"in": [{"var": "commitment.country"}, ["US", "CA"]]},
       100000,
       50000
     ]
   };
   ```

2. **Database-Stored Filters**
   ```typescript
   // Filter rules stored in DB, loaded at runtime
   const userPreferences = await db.getUserFilterRules(userId);
   ```

3. **Cross-System Rules**
   ```typescript
   // Rules defined once, used in frontend, backend, mobile
   const sharedFilter = {
     "and": [
       {">=": [{"var": "mutualRecognition"}, 0.1]},
       {"<": [{"var": "currentTotal"}, 50000]}
     ]
   };
   ```

4. **Complex Conditional Logic**
   ```typescript
   // Tiered caps based on multiple factors
   const tierFilter = {
     "if": [
       {"==": [{"var": "attributes.tier"}, "premium"]},
       {"*": [{"var": "mutualRecognition"}, 200000]},
       {"if": [
         {"==": [{"var": "attributes.tier"}, "standard"]},
         100000,
         50000
       ]}
     ]
   };
   ```

### ❌ Don't Use JsonLogic When:

1. **Static/Hardcoded Logic**
   ```typescript
   // BAD: JsonLogic for static cap
   const filter = 50000; // Just use the number!
   
   // BAD: JsonLogic for boolean flag
   const filter = true; // Just use true!
   ```

2. **Performance-Critical Paths** (without caching)
   ```typescript
   // BAD: Complex JsonLogic in tight loop
   for (let i = 0; i < 10000; i++) {
     evaluateJsonLogic(complexRule, data); // 10-50ms overhead!
   }
   ```

3. **Type-Safe Validation**
   ```typescript
   // BAD: JsonLogic for structural validation
   // Use Zod schema validation instead
   const isValid = evaluateJsonLogic(
     {"and": [
       {"!!": {"var": "pubKey"}},
       {"!!": {"var": "timestamp"}}
     ]},
     data
   );
   
   // GOOD: Zod schema
   const schema = z.object({
     pubKey: z.string(),
     timestamp: z.number()
   });
   ```

4. **Simple Comparisons**
   ```typescript
   // BAD: JsonLogic for simple check
   const passed = evaluateJsonLogic(
     {">": [{"var": "amount"}, 0]},
     {amount: 100}
   );
   
   // GOOD: Native comparison
   const passed = amount > 0;
   ```

---

## Optimization Strategies

### 1. Fast Path for Literals ✅ (Already Implemented)

```typescript
export function evaluateComplianceFilter(filter: ComplianceFilter, context: FilterContext): number {
  if (filter === null) return Infinity; // ✅ Instant
  if (typeof filter === 'number') return filter; // ✅ Instant
  
  // Only use JsonLogic if it's actually a rule
  return evaluateJsonLogic(filter, context); // Slower path
}
```

### 2. Memoization/Caching

```typescript
// Cache JsonLogic evaluation results
const filterCache = new Map<string, number>();

export function evaluateComplianceFilterCached(
  filter: ComplianceFilter,
  context: FilterContext
): number {
  // Fast path for literals
  if (filter === null) return Infinity;
  if (typeof filter === 'number') return filter;
  
  // Cache key based on filter + context
  const cacheKey = `${hashObject(filter)}:${context.pubKey}:${context.currentTotal}`;
  
  if (filterCache.has(cacheKey)) {
    return filterCache.get(cacheKey)!;
  }
  
  const result = evaluateJsonLogic(filter, context);
  filterCache.set(cacheKey, result);
  
  return result;
}
```

### 3. Pre-Compilation (Advanced)

```typescript
// Compile JsonLogic rule to native function
export function compileFilter(filter: EligibilityFilter): (context: FilterContext) => boolean {
  // For simple rules, generate optimized function
  if (filter && typeof filter === 'object' && '>=&#39; in filter) {
    const [varPath, threshold] = filter['>='];
    if (varPath && 'var' in varPath) {
      const path = varPath.var;
      return (context) => {
        const value = getNestedValue(context, path);
        return value >= threshold;
      };
    }
  }
  
  // Fallback to JsonLogic for complex rules
  return (context) => evaluateJsonLogic(filter, context);
}
```

### 4. Batch Evaluation

```typescript
// Evaluate filter once, apply to multiple contexts
export function batchEvaluateFilter(
  filter: EligibilityFilter,
  contexts: FilterContext[]
): boolean[] {
  // For simple literal filters, skip JsonLogic entirely
  if (typeof filter === 'boolean') {
    return contexts.map(() => filter);
  }
  
  // Evaluate each context
  return contexts.map(ctx => evaluateJsonLogic(filter, ctx));
}
```

---

## Recommended Architecture Changes

### Current: Everything is JsonLogic-capable

```typescript
export const ComplianceFilterSchema = z.union([
  z.number().nonnegative(), // Literal
  z.null(),                 // Unlimited
  JsonLogicRuleSchema       // Dynamic
]);
```

**Problem**: Every filter goes through JsonLogic evaluation path, even literals.

### Proposed: Two-tier filter system

```typescript
// Tier 1: Static filters (compile-time known)
export type StaticComplianceFilter = number | null;

// Tier 2: Dynamic filters (runtime evaluation)
export type DynamicComplianceFilter = JsonLogicRule;

// Combined type
export type ComplianceFilter = StaticComplianceFilter | DynamicComplianceFilter;

// Smart evaluation
export function evaluateComplianceFilter(
  filter: ComplianceFilter,
  context: FilterContext
): number {
  // FAST PATH: Static filters (99% of cases in practice)
  if (typeof filter === 'number' || filter === null) {
    return filter === null ? Infinity : filter;
  }
  
  // SLOW PATH: Dynamic filters (1% of cases - user-defined rules)
  return evaluateJsonLogicCached(filter, context);
}
```

---

## Specific Recommendations for Our Codebase

### 1. Compliance Filters (allocation.ts)

**Current usage**:
```typescript
// Evaluated 100-5000 times per allocation cycle
filterLimit = evaluateComplianceFilter(filter, {...});
```

**Recommendation**: ✅ Keep JsonLogic
- Most compliance filters will be static (numbers)
- Fast path handles these efficiently
- Dynamic filters needed for user-defined caps
- Add caching for dynamic filters

**Action**: Add memoization (see strategy #2 above)

### 2. Eligibility Filters (match.ts)

**Current usage**:
```typescript
// Evaluated 500-20,000 times per allocation cycle
if (!evaluateFilter(filter, context)) {
  return false;
}
```

**Recommendation**: ⚠️ Optimize hot path
- Most filters will be user-defined (trust, location preferences)
- Heavy usage in compatibility checking
- High impact on performance

**Action**: 
1. Add fast path for common patterns (trust threshold, city whitelist)
2. Pre-compile filters at commitment load time
3. Cache evaluation results per recipient

### 3. Example Implementation

```typescript
// src/lib/protocol/utils/filters/optimized.ts

// Pre-compiled filter cache
const compiledFilters = new WeakMap<EligibilityFilter, (ctx: FilterContext) => boolean>();

export function evaluateFilterOptimized(
  filter: EligibilityFilter | null | undefined,
  context: FilterContext
): boolean {
  if (!filter) return true;
  
  // FAST PATH: Boolean literals
  if (typeof filter === 'boolean') return filter;
  
  // MEDIUM PATH: Pre-compiled filters
  if (compiledFilters.has(filter)) {
    return compiledFilters.get(filter)!(context);
  }
  
  // SLOW PATH: JsonLogic evaluation with caching
  const compiled = compileSimpleFilter(filter);
  if (compiled) {
    compiledFilters.set(filter, compiled);
    return compiled(context);
  }
  
  // FALLBACK: Full JsonLogic
  return evaluateJsonLogic(filter, context);
}

function compileSimpleFilter(filter: any): ((ctx: FilterContext) => boolean) | null {
  // Compile common patterns to native functions
  
  // Pattern: {">=": [{"var": "mutualRecognition"}, 0.1]}
  if (filter['>='] && filter['>='][0]?.var === 'mutualRecognition') {
    const threshold = filter['>='][1];
    return (ctx) => (ctx.mutualRecognition || 0) >= threshold;
  }
  
  // Pattern: {"in": [{"var": "commitment.city"}, ["SF", "NYC"]]}
  if (filter['in'] && filter['in'][0]?.var === 'commitment.city') {
    const allowedCities = new Set(filter['in'][1]);
    return (ctx) => allowedCities.has(ctx.commitment?.city);
  }
  
  // Pattern: {"and": [rule1, rule2]}
  if (filter['and'] && Array.isArray(filter['and'])) {
    const compiledRules = filter['and'].map(compileSimpleFilter).filter(Boolean);
    if (compiledRules.length === filter['and'].length) {
      return (ctx) => compiledRules.every(rule => rule(ctx));
    }
  }
  
  return null; // Can't compile, use JsonLogic
}
```

---

## Performance Impact Estimate

### Current (Unoptimized)
- **Simple filters**: ~0.01ms × 1000 calls = 10ms overhead ⚠️
- **Complex filters**: ~0.5ms × 1000 calls = 500ms overhead ❌

### With Fast Paths
- **Static filters (90%)**: ~0.001ms × 900 calls = 0.9ms overhead ✅
- **Simple dynamic (8%)**: ~0.01ms × 80 calls = 0.8ms overhead ✅
- **Complex dynamic (2%)**: ~0.5ms × 20 calls = 10ms overhead ⚠️

**Total improvement**: 10-500ms → 11.7ms (2-40x faster)

### With Caching
- **First evaluation**: 11.7ms
- **Subsequent evaluations**: ~1ms (10x faster again)

**Final overhead**: ~1-2ms per allocation cycle ✅

---

## Conclusion

### JsonLogic is Appropriate When:
1. ✅ Filters are **user-defined** (stored in DB)
2. ✅ Logic needs to be **serializable** (transmitted over network)
3. ✅ Rules change **frequently** (without code deployment)
4. ✅ **Complex conditionals** that would be verbose in code

### JsonLogic is NOT Appropriate When:
1. ❌ Filters are **static/hardcoded**
2. ❌ Performance is **critical** (without optimization)
3. ❌ Logic is **simple** (basic comparisons)
4. ❌ Type safety is **required** (use Zod instead)

### For Our Codebase:
- ✅ Keep JsonLogic for ComplianceFilter (mostly static, fast path works well)
- ⚠️ Optimize JsonLogic for EligibilityFilter (add caching, pre-compilation)
- ✅ Document best practices for filter creation
- ✅ Provide helper functions for common patterns

**Overall**: JsonLogic is a good architectural choice for our use case, but we should add optimizations for the hot paths (eligibility filtering during slot matching).

---

**Next Steps**:
1. Implement filter result caching (easy win)
2. Add pre-compilation for common filter patterns (medium effort)
3. Benchmark real-world performance (measure before optimizing further)
4. Document performance best practices for filter authors

