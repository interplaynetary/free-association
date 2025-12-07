# Protocol Directory Reorganization

## Overview

The protocol directory has been reorganized to cleanly separate **pure protocol logic** from **Svelte-specific reactive wrappers**.

## New Structure

```
src/lib/protocol/
├── core/                          # Pure protocol (framework-agnostic, npm-ready)
│   ├── index.ts                   # Main export
│   ├── schemas.ts                 # Zod schemas & types (53KB)
│   ├── allocation.ts              # Pure allocation algorithm (79KB)
│   ├── distribution.ts            # Pure distribution calculation (15KB)
│   ├── tree.ts                    # Pure tree operations (33KB)
│   ├── config.ts                  # Configuration (1.8KB)
│   │
│   ├── utils/                     # Pure utilities
│   │   ├── index.ts
│   │   ├── contributors.ts        # Contributor helpers
│   │   ├── commitments.ts         # Commitment helpers
│   │   ├── slots.ts               # Slot helpers
│   │   ├── match.ts               # Slot matching logic (72KB)
│   │   ├── memoize.ts             # Memoization utilities
│   │   ├── needTypes.ts           # Need type utilities
│   │   └── capacity-filters.ts    # Capacity filtering
│   │
│   ├── filters/                   # Pure filter logic
│   │   ├── index.ts
│   │   ├── filters.ts             # Core filter system
│   │   ├── compliance.ts          # Compliance filters
│   │   ├── eligibility.ts         # Eligibility filters
│   │   ├── types.ts               # Filter type definitions
│   │   └── treeSearch.ts          # Tree search filters
│   │
│   ├── attributes/                # Pure attribute logic
│   │   ├── index.ts
│   │   ├── attribute-recognition.ts
│   │   └── attribute-types.ts
│   │
│   └── collective/                # Pure collective logic
│       ├── index.ts
│       ├── collective-membership.ts
│       ├── collective-recognition.ts
│       └── schemas.ts
│
├── stores/                        # Svelte reactive wrappers
│   ├── index.ts
│   ├── stores.svelte.ts          # Main stores (89KB)
│   ├── allocation.svelte.ts      # Allocation stores (44KB)
│   ├── attributes.svelte.ts      # Attribute stores
│   ├── collective-membership.svelte.ts
│   ├── collective-recognition.svelte.ts
│   ├── collective-tree.svelte.ts
│   │
│   └── filters/                   # Reactive filter stores
│       ├── objectFiltering.svelte.ts
│       ├── space.svelte.ts
│       ├── time.svelte.ts
│       └── capacitySpecific.svelte.ts
│
├── index.ts                       # Main protocol export
├── tests/                         # Tests (unchanged location)
├── cli/                           # CLI tools (unchanged location)
└── docs/                          # Documentation (unchanged location)
```

## Import Paths

### From Within Protocol

**Core files import from other core files:**
```typescript
// ✅ Correct
import { Commitment } from '../schemas';
import { computeAllocations } from '../allocation';
import { FilterContext } from '../filters/types';
```

**Store files import from core:**
```typescript
// ✅ Correct
import { Commitment } from '../core/schemas';
import { computeAllocations } from '../core/allocation';
```

### From Outside Protocol

**Import pure protocol:**
```typescript
// ✅ Via main barrel export
import { Commitment, computeAllocations } from '$lib/protocol';

// ✅ Direct from core (when you need pure only)
import { Commitment } from '$lib/protocol/core/schemas';
```

**Import Svelte stores:**
```typescript
// ✅ Via main barrel export (includes both core + stores)
import { myCommitmentStore, myRecognitionTreeStore } from '$lib/protocol';

// ✅ Direct from stores
import { myCommitmentStore } from '$lib/protocol/stores/stores.svelte';
```

## Dependencies

### Core (`src/lib/protocol/core/`)
- **Only depends on:**
  - `zod` - Schema validation
  - `json-logic-js` - Filter rule evaluation (for compliance/eligibility filters)
  - Standard JavaScript/TypeScript

- **Zero dependencies on:**
  - Svelte
  - Browser APIs
  - Any UI framework

### Stores (`src/lib/protocol/stores/`)
- **Depends on:**
  - `svelte` - Reactive stores
  - `$lib/protocol/core` - Pure protocol logic
  - `$lib/network/holster.svelte` - P2P synchronization
  - `$lib/utils/primitives/itc` - Interval Tree Clocks

## Benefits

1. **Clean Separation**: Clear boundary between pure logic and reactive wrappers
2. **npm-Ready**: `core/` can be extracted as standalone package
3. **Framework-Agnostic**: Core protocol works in Node.js, browser, CLI, anywhere
4. **Testable**: Pure functions are trivial to test in isolation
5. **Maintainable**: No confusion about dependencies
6. **Type-Safe**: Full TypeScript support with proper module resolution

## Migration Notes

All import paths have been updated throughout the codebase. The main changes:

- `$lib/protocol/schemas` → `$lib/protocol/core/schemas`
- `$lib/protocol/allocation` → `$lib/protocol/core/allocation`
- `$lib/protocol/tree` → `$lib/protocol/core/tree`
- `$lib/protocol/stores.svelte` → `$lib/protocol/stores/stores.svelte`
- `$lib/protocol/allocation.svelte` → `$lib/protocol/stores/allocation.svelte`
- `$lib/protocol/utils/*` → `$lib/protocol/core/utils/*`
- `$lib/protocol/attributes/attribute-recognition.svelte` → `$lib/protocol/stores/attributes.svelte`

## Future: npm Package

To create an npm package from the core:

1. Copy `src/lib/protocol/core/` to a new repo/directory
2. Create `package.json`:
```json
{
  "name": "@free-association/protocol",
  "version": "0.1.0",
  "type": "module",
  "main": "./index.ts",
  "types": "./index.ts",
  "dependencies": {
    "zod": "^3.x",
    "json-logic-js": "^2.x"
  }
}
```
3. Publish to npm

The core protocol will work in any JavaScript environment!

