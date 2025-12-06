## Elegant Lambda Calculus Implementation

This directory contains a more elegant, mathematically rigorous implementation of the Recognition Calculus (λ-R) that follows pure lambda calculus principles.

### Key Features

- **Fully Curried Functions**: All functions are curried for partial application
- **Point-Free Style**: Minimal use of intermediate variables
- **Function Composition**: Natural composition using combinators
- **Monadic Patterns**: Reader and State monads for context threading
- **SKI Combinators**: Direct use of lambda calculus combinators
- **Church Encodings**: Boolean and numeric encodings
- **Y Combinator**: Recursion via fixed-point combinator

### Structure

```
elegant/
├── combinators.ts          # Lambda calculus combinators (S, K, I, B, C, Y)
├── recognition.ts          # Elegant recognition system (fully curried)
├── index.ts               # Main exports
├── ELEGANCE.md            # Guide to elegant patterns
├── COMPARISON.md          # Comparison with original implementation
└── __tests__/             # Comprehensive test suite
    ├── combinators.test.ts    # Tests for all combinators
    ├── recognition.test.ts    # Tests for recognition system
    └── integration.test.ts    # Integration scenarios
```

### Quick Start

```typescript
import { elegant } from '../lambda-calculus';

// Fully curried functions
const matrix = elegant.uniformRecognitionMatrix(entities);
const mutualInMatrix = elegant.mutual(matrix);
const aliceMutual = mutualInMatrix('alice');
const mrBob = aliceMutual('bob');

// Point-free composition
const calculateMRD = elegant.pipe(
  elegant.mrd(matrix),
  (mrdFn) => mrdFn(entityId),
  (mrdInEntity) => mrdInEntity(entities)
);

// Reader monad for context
const computation = elegant.mrsR('alice');
const result = elegant.runReader(context)(computation);
```

### Differences from Original

| Feature | Original | Elegant |
|---------|----------|---------|
| Currying | Partial | Full |
| Composition | Manual | Natural |
| Context | Explicit | Reader Monad |
| Style | Imperative | Point-Free |
| Combinators | None | SKI, B, C, Y |

### Testing

Run the comprehensive test suite:

```bash
npm test -- elegant/__tests__
```

Tests cover:
- All combinators (S, K, I, B, C, Y)
- Maybe, Reader, State monads
- Church encodings
- Recognition operations
- Integration scenarios
- Edge cases

### Documentation

- **ELEGANCE.md**: Detailed guide on elegant patterns
- **COMPARISON.md**: Side-by-side comparison with spec
- Test files: Executable examples

### Migration

Both APIs are available:

```typescript
// Original API (simple)
import { mutual } from './lambda-calculus';
const mr = mutual(matrix, 'alice', 'bob');

// Elegant API (composable)
import { elegant } from './lambda-calculus';
const mr = elegant.mutual(matrix)('alice')('bob');
```

### Philosophy

This implementation prioritizes:
1. **Mathematical Correctness**: Matches λ-R spec exactly
2. **Composability**: Functions compose naturally
3. **Type Safety**: Strong type inference
4. **Testability**: Pure functions, easy to test
5. **Performance**: Enables memoization and lazy evaluation

See **ELEGANCE.md** for detailed patterns and examples.

