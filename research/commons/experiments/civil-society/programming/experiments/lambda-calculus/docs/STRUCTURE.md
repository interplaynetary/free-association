# Lambda Calculus Package Structure

Organized structure for the Recognition Calculus (λ-R) implementation.

## 📁 Directory Structure

```
lambda-calculus/
├── index.ts                      # Main entry point
├── package.json                  # Package configuration
├── tsup.config.ts                # Build configuration
│
├── src/                          # Source code
│   ├── core/                     # Core implementation
│   │   ├── types.ts                  # Types and Zod schemas
│   │   ├── primitives.ts             # Basic operations
│   │   ├── recognition.ts            # Recognition system
│   │   ├── filters.ts                # Filter system
│   │   ├── limits.ts                 # Limit system
│   │   ├── collective.ts             # Collectives
│   │   ├── commons.ts                # Commons
│   │   ├── allocation.ts             # Allocation algorithm
│   │   └── system.ts                 # System evolution
│   │
│   └── elegant/                  # Elegant implementation
│       ├── index.ts                  # Elegant exports
│       ├── combinators.ts            # Lambda calculus combinators
│       ├── recognition.ts            # Curried recognition API
│       └── __tests__/                # Test suites
│           ├── combinators.test.ts
│           ├── recognition.test.ts
│           └── integration.test.ts
│
├── docs/                         # Documentation
│   ├── README.md                     # Main guide
│   ├── PACKAGING.md                  # Packaging guide
│   ├── DEPLOYMENT-GUIDE.md           # Deployment steps
│   ├── STRUCTURE.md                  # This file
│   ├── ELEGANCE.md                   # Elegance patterns (elegant API)
│   ├── COMPARISON.md                 # Spec comparison
│   └── SUMMARY.md                    # Overview
│
└── examples/                     # Usage examples
    └── example.ts                    # Basic examples
```

## 📦 Exports

### Main Package (`@free-association/lambda-calculus`)

```typescript
import {
  // Types
  Entity, Distribution, RecognitionMatrix,
  
  // Recognition
  mutual, tmr, mrs, mrd,
  
  // Filters & Limits
  attrFilter, mrdFilter, timeFilter,
  capLimit, progressiveLimit,
  
  // Collectives & Commons
  formCollective, scmrs, scrmrs,
  formCommons, evolveCommons,
  
  // System
  initializeSystem, evolveSystem,
  
  // Elegant API
  elegant,
} from '@free-association/lambda-calculus';
```

### Elegant Subpackage (`@free-association/lambda-calculus/elegant`)

```typescript
import {
  // Combinators
  S, K, I, B, C, Y,
  pipe, compose,
  
  // Monads
  Maybe, Reader, State,
  
  // Curried Recognition
  mutual, mrs, mrd, tmr,
} from '@free-association/lambda-calculus/elegant';
```

## 🎯 Module Responsibilities

### Core (`src/core/`)

**Purpose**: Standard implementation with straightforward APIs

- `types.ts`: All TypeScript types and Zod validation schemas
- `primitives.ts`: Basic math, set, and distribution operations
- `recognition.ts`: Recognition calculations (mutual, TMR, MRS, MRD)
- `filters.ts`: Entity filtering logic
- `limits.ts`: Allocation limit transformations
- `collective.ts`: Collective formation and SCMRS/SCRMRS
- `commons.ts`: Commons creation and evolution
- `allocation.ts`: Capacity allocation algorithm
- `system.ts`: System state and evolution orchestration

### Elegant (`src/elegant/`)

**Purpose**: Functional programming style with full currying

- `combinators.ts`: Pure lambda calculus combinators and utilities
- `recognition.ts`: Fully curried recognition operations
- `__tests__/`: Comprehensive test suites

### Documentation (`docs/`)

**Purpose**: Guides and reference materials

- User guides for different audiences
- Packaging and deployment instructions
- Theoretical background and comparisons

### Examples (`examples/`)

**Purpose**: Practical usage demonstrations

- Real-world scenarios
- Best practices
- Integration patterns

## 🔄 Import Paths

### Internal Imports (within package)

```typescript
// From elegant to core
import type { Distribution } from '../core/types';
import { normalize } from '../core/types';

// From core to primitives
import { sumOver } from './primitives';

// From tests to source
import { mutual } from '../recognition';
```

### External Imports (after publishing)

```typescript
// Main package
import { mutual, mrs } from '@free-association/lambda-calculus';

// Elegant subpackage
import { elegant } from '@free-association/lambda-calculus';
import { Y, pipe } from '@free-association/lambda-calculus/elegant';
```

## 📊 Build Output

After running `npm run build`:

```
dist/
├── index.js              # CommonJS main entry
├── index.mjs             # ESM main entry
├── index.d.ts            # TypeScript types
├── core/                 # Compiled core modules
│   ├── types.js
│   ├── recognition.js
│   └── ...
└── elegant/              # Compiled elegant modules
    ├── index.js
    ├── index.mjs
    ├── index.d.ts
    ├── combinators.js
    └── recognition.js
```

## 🧪 Testing Structure

```
src/elegant/__tests__/
├── combinators.test.ts    # 43 tests - Lambda calculus combinators
├── recognition.test.ts    # 35 tests - Curried recognition operations
└── integration.test.ts    # 10 tests - Real-world scenarios
```

Run tests:
```bash
npm test                          # All tests
npm test -- combinators.test.ts   # Specific suite
npm run test:watch                # Watch mode
```

## 📝 File Naming Conventions

- **Types**: `*.ts` for implementation, `*.d.ts` for declarations
- **Tests**: `*.test.ts` for test files
- **Docs**: `*.md` for markdown documentation
- **Config**: `*.config.ts` for configuration files

## 🎨 Code Organization Principles

1. **Separation of Concerns**: Core vs Elegant implementations
2. **Single Responsibility**: Each module has one clear purpose
3. **Dependency Direction**: Core ← Elegant (never Core → Elegant)
4. **Test Proximity**: Tests close to implementation
5. **Documentation Clarity**: Docs separate from implementation

## 🚀 Development Workflow

1. **Edit Source**: Modify files in `src/`
2. **Run Tests**: `npm test`
3. **Type Check**: `npm run type-check`
4. **Build**: `npm run build`
5. **Verify**: Check `dist/` output

## 📚 Documentation Organization

- **User-facing**: `docs/README.md`, `docs/DEPLOYMENT-GUIDE.md`
- **Developer-facing**: `docs/STRUCTURE.md` (this file)
- **Theory**: `docs/ELEGANCE.md`, `docs/COMPARISON.md`
- **Process**: `docs/PACKAGING.md`

This structure ensures clean separation, easy navigation, and professional package organization.

