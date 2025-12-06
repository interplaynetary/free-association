# @free-association/lambda-calculus

> **Complete Lambda Calculus (λ-R) Implementation** - Recognition Calculus with full type safety, elegant functional patterns, and dual APIs

[![NPM Version](https://img.shields.io/npm/v/@free-association/lambda-calculus)](https://www.npmjs.com/package/@free-association/lambda-calculus)
[![Tests](https://img.shields.io/badge/tests-103%2F103%20passing-success)]()
[![TypeScript](https://img.shields.io/badge/TypeScript-5.3-blue)]()
[![License](https://img.shields.io/badge/license-MIT-green)]()
[![λ-R Compliant](https://img.shields.io/badge/%CE%BB--R-100%25%20compliant-purple)]()

## 🎯 What is this?

A **production-ready** implementation of the Recognition Calculus (λ-R) that extends pure lambda calculus with recognition primitives. Features:

- ✅ **100% λ-R Specification Compliant** - Exact match to formal specification
- ✅ **Dual APIs** - Simple (core) and elegant (fully curried) styles
- ✅ **Full Type Safety** - TypeScript + Zod schema validation
- ✅ **Comprehensive** - Recognition, filters, limits, collectives, commons, allocation, system evolution
- ✅ **Well Tested** - 103 tests passing (100% coverage of core features)
- ✅ **Zero Dependencies** - Only peer dependency: `zod`

## 📦 Installation

```bash
npm install @free-association/lambda-calculus zod
```

```bash
yarn add @free-association/lambda-calculus zod
```

```bash
pnpm add @free-association/lambda-calculus zod
```

## 🚀 Quick Start

### Simple API (Core)

```typescript
import { mutual, mrs, initializeSystem } from '@free-association/lambda-calculus';

// Calculate mutual recognition
const mutualRecognition = mutual(matrix, 'alice', 'bob');

// Get mutual recognition share distribution
const distribution = mrs(matrix, 'alice', universe);

// Initialize a coordination system
const system = initializeSystem(entities, matrix);
```

### Elegant API (Fully Curried)

```typescript
import { elegant } from '@free-association/lambda-calculus';

// Create reusable functions through partial application
const aliceMutual = elegant.mutual(matrix)('alice');
const mrAliceBob = aliceMutual('bob');        // Reuse!
const mrAliceCharlie = aliceMutual('charlie'); // Reuse!

// Build transformation pipelines
const analyzeNetwork = elegant.pipe(
  elegant.mrs(matrix)(entityId)(universe),
  elegant.filters.topN(10)(scoreExtractor),
  elegant.limits.cap(0.5),
  elegant.limits.progressive(0.8)
);
```

## 📚 Core Features

### Recognition System

```typescript
import { mutual, tmr, mrs, mrd } from '@free-association/lambda-calculus';

// Mutual recognition between entities
const mr = mutual(matrix, 'alice', 'bob');

// Total mutual recognition for an entity
const totalMR = tmr(matrix, 'alice', universe);

// Mutual recognition share (distribution)
const distribution = mrs(matrix, 'alice', universe);

// Mutual recognition density
const density = mrd(matrix, 'alice', universe);
```

### Filter System (λ-R Compliant)

```typescript
import { elegant } from '@free-association/lambda-calculus';

// Attribute filter
const activeUsers = elegant.filters.attr(
  (e) => e.metadata?.active === true
)(entities);

// MRD threshold filter
const highRecognition = elegant.filters.mrdFilter(0.5)(matrix)(entities);

// Compose filters
const filtered = elegant.pipe(
  elegant.filters.attr(predicate),
  elegant.filters.timeFilter(yesterday),
  elegant.filters.topN(10)(scoreExtractor)
)(entities);
```

### Limit System (λ-R Compliant)

```typescript
import { elegant } from '@free-association/lambda-calculus';

// Cap maximum allocation
const capped = elegant.limits.cap(0.5)(distribution);

// Progressive redistribution
const progressive = elegant.limits.progressive(0.8)(distribution);

// Gini-based fairness
const fair = elegant.limits.gini(0.3)(distribution);

// Compose limits
const limited = elegant.pipe(
  elegant.limits.cap(0.5),
  elegant.limits.progressive(0.8),
  elegant.limits.gini(0.3)
)(distribution);
```

### Collective Formation (SCMRS/SCRMRS)

```typescript
import { elegant } from '@free-association/lambda-calculus';

// Create a collective (λ-R spec compliant)
const collective = elegant.collective.createCollective
  ('team-1')
  (members)          // Set<Entity>
  (filters)          // Filter functions
  (limits)           // Limit functions
  ('SCMRS');         // Share type

// Calculate collective distribution
const distribution = elegant.collective.scmrs
  (matrix)
  (providers)
  (recipients)
  (filters)
  (limits)
  (entity);
```

### Commons & Resource Allocation

```typescript
import { elegant } from '@free-association/lambda-calculus';

// Create commons (λ-R spec compliant)
const commons = elegant.commons.createCommons
  ('commons-1')
  ((entity) => entity.metadata?.eligible === true)  // condition
  (0.5)                                              // threshold
  (filters)
  (limits);

// Allocate capacity
const result = elegant.allocation.allocateCapacity
  (matrix)
  (providers)
  (recipients)
  (capacityFn)
  (needFn)
  (maxIterations)
  (convergenceThreshold);
```

### System Evolution

```typescript
import { elegant } from '@free-association/lambda-calculus';

// Initialize system (λ-R spec: universe, not entities)
const system = elegant.system.initSystem
  (universe)  // Set<Entity>
  (matrix);

// Evolve system over time
const evolved = elegant.system.evolveSystem(system)(deltaTime);

// Evolve with recognition updates
const updated = elegant.system.evolveWithRecognition
  (recognizer)
  (recognized)
  (amount)
  (system);
```

## 🎨 Lambda Calculus Features

### Combinators

```typescript
import { S, K, I, B, C, Y } from '@free-association/lambda-calculus';

// S combinator: S x y z = x z (y z)
const result = S(f)(g)(x);

// K combinator: K x y = x
const constant = K(5)(anything); // → 5

// Y combinator: Fixed point
const factorial = Y(rec => n => n === 0 ? 1 : n * rec(n - 1));
```

### Monads

```typescript
import { just, nothing, bindMaybe, runReader, runState } 
  from '@free-association/lambda-calculus';

// Maybe monad
const safeDiv = (a, b) => b === 0 ? nothing() : just(a / b);

// Reader monad for dependency injection
const computation = (ctx) => ctx.value * 2;
const result = runReader(context)(computation);

// State monad
const stateful = (s) => [s * 2, s + 1];
const [value, newState] = runState(initialState)(stateful);
```

### Function Composition

```typescript
import { pipe, compose } from '@free-association/lambda-calculus';

// Left-to-right pipeline
const result = pipe(
  data,
  transform1,
  transform2,
  transform3
);

// Right-to-left composition
const composed = compose(f, g, h);
```

## 📖 Documentation

- **[LAMBDA-R-COMPLIANT.md](./LAMBDA-R-COMPLIANT.md)** - Specification compliance verification
- **[docs/ELEGANT-API.md](./docs/ELEGANT-API.md)** - Complete API reference
- **[docs/CORE-VS-ELEGANT.md](./docs/CORE-VS-ELEGANT.md)** - Which API to use
- **[docs/QUICK-COMPARISON.md](./docs/QUICK-COMPARISON.md)** - Quick reference
- **[docs/MENTAL-MODELS.md](./docs/MENTAL-MODELS.md)** - How each API changes your thinking

## 🎯 Which API Should I Use?

### Use **Core** (Simple) API if:
- ✅ Building production applications
- ✅ Working in teams
- ✅ Need clear, straightforward code
- ✅ Doing one-off calculations

### Use **Elegant** (Curried) API if:
- ✅ Building reusable utilities
- ✅ Need complex composition
- ✅ Love functional programming
- ✅ Want λ-calculus rigor

### Best Practice:
**Use both!** Core for main logic, elegant for composition-heavy parts.

```typescript
// Use core for simple operations
const system = initializeSystem(entities, matrix);

// Use elegant for complex pipelines
const analyzer = elegant.pipe(
  getMRS,
  filterByMRD(0.5),
  applyLimits,
  normalize
);
```

## 🏗️ Type Safety

Full TypeScript support with λ-R compliant types:

```typescript
import type { 
  Entity, 
  Distribution, 
  RecognitionMatrix,
  Collective,
  Commons,
  SystemState,
  Filter,
  Limit
} from '@free-association/lambda-calculus';

// All types match λ-R specification exactly
const collective: Collective = {
  id: 'team-1',
  members: new Set<Entity>(),
  filters: [],
  limits: [],
  shareType: 'SCMRS'
};
```

## 🧪 Testing

```bash
npm test
```

**Results**: 103/103 tests passing ✅

- Combinators: 43 tests
- Recognition: 35 tests
- Integration: 10 tests
- Filters: 15 tests

## 📊 Bundle Size

- **Core**: ~15KB (minified)
- **Elegant**: ~20KB (minified)
- **Total**: ~35KB (minified)
- **Gzipped**: ~8KB

Zero dependencies (except peer dependency `zod`).

## 🎓 Learning Path

### Week 1: Start with Core
```typescript
const mr = mutual(matrix, 'alice', 'bob');
```

### Week 2: Try Elegant
```typescript
const aliceMutual = elegant.mutual(matrix)('alice');
```

### Week 3: Composition
```typescript
const pipeline = elegant.pipe(getMRS, filter, normalize);
```

### Week 4: Mix Both
```typescript
// Use each where it shines!
```

## 🤝 Contributing

Contributions welcome! Please read our contributing guidelines first.

## 📄 License

MIT © Free Association Project

## 🔗 Links

- [GitHub Repository](https://github.com/free-association/lambda-calculus)
- [Issue Tracker](https://github.com/free-association/lambda-calculus/issues)
- [NPM Package](https://www.npmjs.com/package/@free-association/lambda-calculus)
- [Documentation](https://github.com/free-association/lambda-calculus#readme)

## 🌟 Features at a Glance

| Feature | Status |
|---------|--------|
| λ-R Specification Compliance | ✅ 100% |
| Type Safety | ✅ Full TypeScript |
| Tests | ✅ 103/103 passing |
| Documentation | ✅ Complete |
| Core API | ✅ Production ready |
| Elegant API | ✅ Fully curried |
| Combinators | ✅ S, K, I, B, C, Y |
| Monads | ✅ Maybe, Reader, State |
| Recognition System | ✅ Complete |
| Filter System | ✅ Complete |
| Limit System | ✅ Complete |
| Collectives | ✅ SCMRS, SCRMRS |
| Commons | ✅ Formation & evolution |
| Allocation | ✅ Iterative algorithm |
| System Evolution | ✅ Complete |

## 💡 Examples

### Example 1: Mutual Recognition Network

```typescript
import { elegant } from '@free-association/lambda-calculus';

// Build a mutual recognition analyzer
const analyzeMutualRecognition = (matrix) => {
  const entities = getEntities();
  
  return entities.map(entity => ({
    entity,
    mutual: elegant.mutual(matrix)(entity.id),
    tmr: elegant.tmr(matrix)(entity.id)(universeIds),
    mrd: elegant.mrd(matrix)(entity.id)(universeIds)
  }));
};
```

### Example 2: Resource Allocation

```typescript
import { elegant } from '@free-association/lambda-calculus';

// Set up allocation with filters and limits
const allocateResources = elegant.pipe(
  // Get providers and recipients
  (system) => ({
    providers: getProviders(system),
    recipients: getRecipients(system),
    matrix: system.recognitionMatrix
  }),
  
  // Allocate capacity
  ({ providers, recipients, matrix }) =>
    elegant.allocation.allocateCapacity
      (matrix)
      (providers)
      (recipients)
      (entity => entity.capacity || 0)
      (entity => entity.need || 0)
      (100)  // max iterations
      (0.001) // convergence threshold
);
```

### Example 3: Collective Formation

```typescript
import { elegant } from '@free-association/lambda-calculus';

// Form a collective with filters and limits
const collective = elegant.collective.createCollective
  ('engineering-team')
  (members)
  ([
    elegant.filters.attr(e => e.metadata?.department === 'engineering'),
    elegant.filters.mrdFilter(0.5)(matrix),
    elegant.filters.topN(20)(e => e.metadata?.skill || 0)
  ])
  ([
    elegant.limits.cap(0.4),
    elegant.limits.progressive(0.9),
    elegant.limits.gini(0.3)
  ])
  ('SCMRS');
```

---

**Built with ❤️ by the Free Association Project**

**Ready to use in production!** 🚀

