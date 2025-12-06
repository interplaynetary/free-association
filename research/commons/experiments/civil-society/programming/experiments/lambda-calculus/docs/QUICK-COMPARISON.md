# Quick Comparison: Core vs Elegant

## 🎯 **One-Page Reference**

### Same Problem, Different Thinking

```typescript
// Calculate mutual recognition for Alice with everyone

// ═══════════════════════════════════════
// CORE: "Do this for each person"
// ═══════════════════════════════════════
import { mutual } from '@free-association/lambda-calculus';

const results = people.map(person => 
  mutual(matrix, 'alice', person)
);

// Thinking: "Call mutual for each person"
// ✓ Simple
// ✓ Clear
// ✓ Familiar


// ═══════════════════════════════════════
// ELEGANT: "Apply this function to everyone"
// ═══════════════════════════════════════
import { elegant } from '@free-association/lambda-calculus';

const aliceMutual = elegant.mutual(matrix)('alice');
const results = people.map(aliceMutual);

// Thinking: "Create a function, then map it"
// ✓ Reusable (aliceMutual is a value you can pass around)
// ✓ No repetition (matrix and 'alice' stated once)
// ✓ Composable
```

---

## 📊 **Mental Models**

### Core: Think in PROCEDURES
```
START
  ↓
Load data
  ↓
Process item 1
  ↓
Process item 2
  ↓
Process item 3
  ↓
END
```

### Elegant: Think in PIPELINES
```
Data → Transform1 → Transform2 → Transform3 → Result
```

---

## 🔥 **The Key Insight**

### Core
```typescript
// Functions are ACTIONS you perform
mutual(matrix, 'alice', 'bob');
```
"I'm performing the action of calculating mutual recognition"

### Elegant  
```typescript
// Functions are VALUES you create
const aliceMutual = elegant.mutual(matrix)('alice');
```
"I've created a mutual recognition calculator for Alice"

---

## 🎨 **Complexity: How They Scale**

### Simple Task (1-2 operations)
**Core wins** - Less ceremony

```typescript
// Core: Direct and clear
const mr = mutual(matrix, 'alice', 'bob');

// Elegant: Overkill
const mr = elegant.mutual(matrix)('alice')('bob');
```

### Medium Task (3-5 operations)
**Equal** - Personal preference

```typescript
// Core: Sequential steps
const dist = mrs(matrix, 'alice', universe);
const filtered = filterByThreshold(dist, 0.5);
const top = topK(filtered, 10);

// Elegant: Pipeline
const result = elegant.pipe(
  elegant.mrs(matrix)('alice')(universe),
  elegant.filters.threshold(0.5)(extractor),
  elegant.limits.topK(10)
);
```

### Complex Task (6+ operations)
**Elegant wins** - Better composition

```typescript
// Core: Variable soup
const dist1 = mrs(matrix, 'alice', universe);
const dist2 = filterByMRD(dist1, 0.5);
const dist3 = filterByTime(dist2, yesterday);
const dist4 = topK(dist3, 10);
const dist5 = applyLimit(dist4, cap);
const dist6 = normalize(dist5);

// Elegant: Clean pipeline
const analyze = elegant.pipe(
  getMRS,
  filterMRD(0.5),
  filterTime(yesterday),
  topK(10),
  applyCap,
  normalize
);
```

---

## 💡 **Quick Decision Tree**

```
Are you building something reusable?
  ├─ No  → Use CORE
  └─ Yes → Consider ELEGANT

Do you need complex composition?
  ├─ No  → Use CORE
  └─ Yes → Consider ELEGANT

Are you comfortable with FP?
  ├─ No  → Use CORE
  └─ Yes → Use ELEGANT

Is this a simple one-off?
  ├─ Yes → Use CORE
  └─ No  → Either works

Working in a team?
  ├─ Yes → Use CORE (easier to onboard)
  └─ No  → Either works
```

---

## 🎯 **Common Patterns**

### Pattern: "Do something with many items"

**Core:**
```typescript
for (const item of items) {
  const result = doSomething(context, item);
  results.push(result);
}
```

**Elegant:**
```typescript
const doer = doSomething(context);
const results = items.map(doer);
```

### Pattern: "Chain operations"

**Core:**
```typescript
let data = initial;
data = transform1(data, config1);
data = transform2(data, config2);
data = transform3(data, config3);
```

**Elegant:**
```typescript
const data = pipe(
  initial,
  transform1(config1),
  transform2(config2),
  transform3(config3)
);
```

### Pattern: "Conditional logic"

**Core:**
```typescript
if (condition) {
  result = doA(data);
} else {
  result = doB(data);
}
```

**Elegant:**
```typescript
const transform = condition ? doA : doB;
const result = transform(data);
```

---

## 🎪 **The "Aha!" Examples**

### Example 1: The Reusable Calculator

```typescript
// Core: Repeat context every time
const mr1 = mutual(matrix, 'alice', 'bob');
const mr2 = mutual(matrix, 'alice', 'charlie');
const mr3 = mutual(matrix, 'alice', 'dana');
//          ^^^^^^        ^^^^^^^ - Repeating!

// Elegant: State context once
const aliceMutual = elegant.mutual(matrix)('alice');
const mr1 = aliceMutual('bob');      // Clean!
const mr2 = aliceMutual('charlie');  // Clean!
const mr3 = aliceMutual('dana');     // Clean!
```

### Example 2: The Building Blocks

```typescript
// Core: Can't really build abstractions
// Each call is standalone

// Elegant: Build your own tools
const toolkit = {
  alice: {
    mutual: elegant.mutual(matrix)('alice'),
    mrs: elegant.mrs(matrix)('alice'),
    tmr: elegant.tmr(matrix)('alice'),
  },
  bob: {
    mutual: elegant.mutual(matrix)('bob'),
    mrs: elegant.mrs(matrix)('bob'),
    tmr: elegant.tmr(matrix)('bob'),
  }
};

// Now use your tools:
const mr = toolkit.alice.mutual('bob');
const dist = toolkit.alice.mrs(universe);
```

### Example 3: The Pipeline

```typescript
// Core: Hard to see the flow
function analyze(entity) {
  const dist = mrs(matrix, entity.id, universe);
  const filtered = filterByMRD(dist, 0.5);
  const limited = applyLimit(filtered, cap);
  const normalized = normalize(limited);
  return extractTop(normalized, 10);
}

// Elegant: Flow is obvious
const analyze = elegant.pipe(
  (entity) => elegant.mrs(matrix)(entity.id)(universe),
  filterByMRD(0.5),
  applyLimit(cap),
  normalize,
  extractTop(10)
);
// Read it like: "Data enters → filter → limit → normalize → extract"
```

---

## 🎓 **Learning Path**

### Week 1: Start with Core
```typescript
// Get comfortable with the concepts
const mr = mutual(matrix, 'alice', 'bob');
const dist = mrs(matrix, 'alice', universe);
```

### Week 2: Try Simple Elegant
```typescript
// Start with partial application
const aliceMutual = elegant.mutual(matrix)('alice');
const mr = aliceMutual('bob');
```

### Week 3: Try Pipelines
```typescript
// Chain a few operations
const result = elegant.pipe(
  getMRS,
  filter,
  normalize
);
```

### Week 4: Mix Both
```typescript
// Use each where it shines
const system = initSystem(entities);  // Core
const analyze = elegant.pipe(...);     // Elegant
```

---

## ⚡ **Performance Note**

**Both are equally fast for single calls.**

Elegant might be *slightly* slower for:
- Single one-off calculations (extra function creation)

Elegant might be *slightly* faster for:
- Repeated operations (function created once, reused)

**In practice: The difference is negligible. Choose based on readability.**

---

## 🎯 **Bottom Line**

| Aspect | Winner |
|--------|--------|
| **Simplicity** | Core |
| **Reusability** | Elegant |
| **Onboarding** | Core |
| **Composition** | Elegant |
| **One-offs** | Core |
| **Pipelines** | Elegant |
| **Team work** | Core |
| **Advanced FP** | Elegant |

**Best practice**: Start with Core, graduate to Elegant as needed.

---

## 🚀 **Next Steps**

**To learn Elegant style:**
1. Read `docs/MENTAL-MODELS.md` (detailed explanation)
2. Try the examples in `examples/`
3. Start with simple partial application
4. Graduate to pipelines
5. One day it "clicks" 💡

**To stick with Core:**
- That's fine! It's complete and production-ready
- You can always mix in Elegant later
- 90% of developers prefer imperative style
- Nothing wrong with that

**Remember**: The goal is readable, maintainable code. Choose what works for you and your team! 🎉

