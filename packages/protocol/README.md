# @playnet/free-association

> Pure TypeScript implementation of the Free Association Protocol - A decentralized mutual aid and resource allocation system.

[![npm version](https://img.shields.io/npm/v/@playnet/free-association.svg)](https://www.npmjs.com/package/@playnet/free-association)
[![License: AGPL-3.0](https://img.shields.io/badge/License-AGPL%203.0-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)

## Overview

The Free Association Protocol provides a mathematical framework for:

- **Recognition-based allocation**: Resources flow based on mutual recognition, not markets or centralized control
- **Slot-based matching**: Time/location-aware matching of needs to available capacity
- **Compliance filters**: Fine-grained control over who can receive from whom
- **Collective recognition**: Groups can coordinate recognition and resource allocation
- **Transparent computation**: Every allocation decision is traceable and auditable

## Installation

```bash
npm install @playnet/free-association
# or
bun add @playnet/free-association
# or
yarn add @playnet/free-association
```

## Quick Start

```typescript
import { 
  computeAllocations,
  type Commitment,
  type Node 
} from '@playnet/free-association';

// Define your recognition tree (who recognizes whom)
const recognitionTree: Node = {
  id: 'root',
  value: 1.0,
  children: [
    { id: 'alice', value: 0.3, children: [] },
    { id: 'bob', value: 0.7, children: [] }
  ]
};

// Define commitments (needs and capacity)
const commitments: Commitment[] = [
  {
    timestamp: Date.now(),
    pubKey: 'alice',
    capacity_slots: [{
      id: 'alice-food-1',
      name: 'Food donations',
      quantity: 100,
      need_type_id: 'food',
      time_start: Date.now(),
      time_end: Date.now() + 86400000, // 24 hours
      // Optional: Add compliance filter
      compliance_filter: {
        "<=": [{"var": "currentTotal"}, 50] // Max $50 per recipient
      }
    }]
  },
  {
    timestamp: Date.now(),
    pubKey: 'bob',
    need_slots: [{
      id: 'bob-food-1',
      name: 'Need food',
      quantity: 30,
      need_type_id: 'food',
      time_start: Date.now(),
      time_end: Date.now() + 86400000
    }]
  }
];

// Compute allocations
const result = computeAllocations(
  commitments,
  recognitionTree,
  'root' // Your perspective
);

console.log('Allocations:', result.allocations);
console.log('Recognition shares:', result.recognitionShares);
```

## Core Concepts

### 1. Recognition Tree

A tree structure representing mutual recognition relationships:

```typescript
import { type Node, mutualFulfillment } from '@playnet/free-association';

const tree: Node = {
  id: 'root',
  value: 1.0, // Recognition weight
  children: [
    { 
      id: 'alice', 
      value: 0.4,
      children: [
        { id: 'charlie', value: 0.5, children: [] }
      ]
    },
    { id: 'bob', value: 0.6, children: [] }
  ]
};

// Calculate mutual recognition between two nodes
const mutualRec = mutualFulfillment(tree, 'alice', 'bob');
console.log('Mutual recognition:', mutualRec); // 0.0 to 1.0
```

### 2. Commitments (Needs & Capacity)

Declarations of what you need and what you can provide:

```typescript
import { type Commitment, CommitmentSchema } from '@playnet/free-association';

const commitment: Commitment = {
  timestamp: Date.now(),
  pubKey: 'alice',
  
  // What you can provide
  capacity_slots: [{
    id: 'slot-1',
    name: 'Tutoring',
    quantity: 10, // 10 hours
    need_type_id: 'tutoring',
    time_start: Date.now(),
    time_end: Date.now() + 604800000, // 1 week
    location: { lat: 37.7749, lng: -122.4194 },
    
    // Optional: Filter who can receive
    eligibility_filter: {
      ">=": [{"var": "mutualRecognition"}, 0.1] // Min 0.1 mutual recognition
    },
    
    // Optional: Limit how much each recipient gets
    compliance_filter: {
      "<=": [{"var": "currentTotal"}, 3] // Max 3 hours per person
    }
  }],
  
  // What you need
  need_slots: [{
    id: 'need-1',
    name: 'Childcare',
    quantity: 5,
    need_type_id: 'childcare',
    time_start: Date.now(),
    time_end: Date.now() + 604800000
  }]
};

// Validate with Zod schema
const validated = CommitmentSchema.parse(commitment);
```

### 3. Filters (JsonLogic)

Control allocation rules using [JsonLogic](https://jsonlogic.com/):

```typescript
import { type EligibilityFilter, type ComplianceFilter } from '@playnet/free-association';

// Eligibility: WHO can receive? (boolean)
const eligibilityFilter: EligibilityFilter = {
  "and": [
    {">=": [{"var": "mutualRecognition"}, 0.2]}, // Min recognition
    {"in": [{"var": "commitment.city"}, ["SF", "Oakland"]]} // Location check
  ]
};

// Compliance: HOW MUCH can they receive? (number)
const complianceFilter: ComplianceFilter = {
  "if": [
    {">=": [{"var": "mutualRecognition"}, 0.5]}, // High trust
    100, // Can receive up to $100
    50 // Otherwise max $50
  ]
};
```

Available variables in filter context:
- `pubKey` - Recipient's public key
- `mutualRecognition` - Mutual recognition score (0-1)
- `commitment.*` - Recipient's commitment data
- `attributes.*` - Recipient's attributes
- `currentTotal` - Already allocated amount (for compliance)
- `proposedAmount` - New amount being allocated (for compliance)

### 4. Allocation Algorithm

The allocation algorithm follows these steps:

1. **Calculate recognition shares** - Based on the recognition tree
2. **Match slots** - Find compatible need/capacity slots (time, location, type)
3. **Apply eligibility filters** - Check if recipient passes provider's filter
4. **Compute base allocation** - Distribute based on recognition shares
5. **Apply compliance filters** - Enforce per-recipient limits
6. **Redistribute** - Reallocate unused capacity

```typescript
import { computeAllocations } from '@playnet/free-association';

const result = computeAllocations(commitments, recognitionTree, 'root');

// result contains:
// - allocations: Allocation[] - Final allocations
// - recognitionShares: Record<string, number> - Recognition scores
// - totalAllocated: Record<string, number> - Totals per recipient
// - debug: AllocationDebugInfo - Detailed computation steps
```

## Advanced Usage

### Collective Recognition

Groups can coordinate recognition:

```typescript
import { 
  type CollectiveTree,
  shouldUpdateCapacityMembership 
} from '@playnet/free-association/collective';

const collectiveTree: CollectiveTree = {
  id: 'our-collective',
  members: ['alice', 'bob', 'charlie'],
  recognition_tree: {
    id: 'our-collective',
    value: 1.0,
    children: [...]
  }
};

// Check if someone should be added based on capacity
if (shouldUpdateCapacityMembership(newProvider, collectiveTree, threshold)) {
  // Add to collective
}
```

### Attribute Recognition

Tag and recognize specific attributes:

```typescript
import { type AttributeRecognition } from '@playnet/free-association/attributes';

const attributes: AttributeRecognition = {
  attribute: 'organic-farming',
  recognizers: {
    'alice': 0.8,
    'bob': 0.6
  }
};
```

### Tree Operations

Manipulate recognition trees:

```typescript
import { 
  findNodeInTree,
  updateNodeInTree,
  normalizeTree 
} from '@playnet/free-association/tree';

// Find a node
const node = findNodeInTree(tree, 'alice');

// Update recognition
const newTree = updateNodeInTree(tree, 'alice', { value: 0.5 });

// Normalize values to sum to 1.0
const normalized = normalizeTree(tree);
```

## Package Structure

```
@playnet/free-association/
├── index               # Main exports
├── schemas             # Zod schemas & types
├── allocation          # Allocation algorithm
├── distribution        # Distribution calculations
├── tree                # Tree operations
├── config              # Configuration
├── utils/*             # Utility functions
├── filters/*           # Filter logic
├── attributes/*        # Attribute system
└── collective/*        # Collective coordination
```

## TypeScript Support

Full TypeScript support with exported types:

```typescript
import type {
  // Core types
  Node,
  Commitment,
  AvailabilitySlot,
  NeedSlot,
  Allocation,
  
  // Filter types
  EligibilityFilter,
  ComplianceFilter,
  FilterContext,
  
  // Result types
  AllocationComputationResult,
  RecognitionData,
  
  // Collective types
  CollectiveTree,
  
  // Attribute types
  AttributeRecognition
} from '@playnet/free-association';
```

## Dependencies

- **zod** - Schema validation
- **json-logic-js** - Filter rule evaluation

Zero browser dependencies - works in Node.js, Bun, Deno, and browsers.

## Framework Integration

### Svelte

```typescript
import { writable, derived } from 'svelte/store';
import { computeAllocations } from '@playnet/free-association';

const commitments = writable<Commitment[]>([]);
const recognitionTree = writable<Node>({ id: 'root', value: 1, children: [] });

const allocations = derived(
  [commitments, recognitionTree],
  ([$commitments, $tree]) => computeAllocations($commitments, $tree, 'root')
);
```

### React

```typescript
import { useMemo } from 'react';
import { computeAllocations } from '@playnet/free-association';

function useAllocations(commitments: Commitment[], tree: Node) {
  return useMemo(
    () => computeAllocations(commitments, tree, 'root'),
    [commitments, tree]
  );
}
```

### Vue

```typescript
import { computed, ref } from 'vue';
import { computeAllocations } from '@playnet/free-association';

const commitments = ref<Commitment[]>([]);
const recognitionTree = ref<Node>({ id: 'root', value: 1, children: [] });

const allocations = computed(() => 
  computeAllocations(commitments.value, recognitionTree.value, 'root')
);
```

## Contributing

Contributions welcome! This package is part of the [Free Association project](https://github.com/interplaynetary/free-association).

## License

AGPL-3.0-or-later

## Links

- [GitHub Repository](https://github.com/interplaynetary/free-association)
- [Full Documentation](https://docs.openassociation.org)
- [Protocol Specification](https://github.com/interplaynetary/free-association/blob/main/PROTOCOL.md)
- [npm Package](https://www.npmjs.com/package/@playnet/free-association)

