# Free Association Protocol v5

## Overview

A pure, environment-agnostic implementation of the Free Association Protocol with beautiful CLI tools.

## 🚀 Quick Start

### Interactive CLI (Recommended)

```bash
bun run src/lib/commons/v5/cli.ts interactive
```

### Simple Example

```bash
bun run src/lib/commons/v5/cli.ts example
```

### Terminal API

```typescript
import { computeAllocations, buildSystemState } from './free';

const result = computeAllocations(
  myPubKey,
  myCapacity,
  myRecognition,
  mutualRecognition,
  allCommitments,
  systemState,
  previousState
);
```

## 📁 Architecture

```
v5/
├── Core Algorithm
│   ├── free.ts                 Pure allocation algorithm
│   ├── tree.ts                 Recognition tree operations
│   ├── match.ts                Slot matching logic
│   └── schemas.ts              Type definitions
│
├── Utilities
│   ├── utils/contributors.ts   Contributor operations
│   ├── utils/slots.ts          Slot formatting & queries
│   └── utils/commitments.ts    Commitment helpers
│
├── Svelte Integration
│   ├── stores.svelte.ts        Reactive data stores
│   ├── free-algorithm.svelte.ts  Reactive computations
│   └── holster.svelte.ts       P2P synchronization
│
├── CLI Tools
│   ├── cli.ts                  Legacy text interface
│   └── cli/
│       ├── index.tsx           Ink-based UI entry
│       ├── App.tsx             Main application
│       ├── Logo.tsx            ASCII logo
│       ├── Menu.tsx            Interactive menu
│       └── AllocationDisplay.tsx  Results visualization
│
├── Examples
│   └── examples/terminal-example.ts  Working scenarios
│
└── Documentation
    ├── README.md               This file
    ├── CLI_GUIDE.md            CLI documentation
    ├── FREE_PURE_API.md        API reference
    ├── TERMINAL_READY.md       Terminal usage guide
    ├── REFACTORING_SUMMARY.md  Phase 1 changes
    └── REFACTORING_V2_SUMMARY.md  Phase 2 changes
```

## 📚 Documentation

- **[CLI Guide](./CLI_GUIDE.md)** - Complete CLI documentation
- **[Pure API Reference](./FREE_PURE_API.md)** - Core API documentation
- **[Terminal Guide](./TERMINAL_READY.md)** - Using the protocol in terminal
- **[Refactoring Summary](./REFACTORING_SUMMARY.md)** - Phase 1 changes
- **[Refactoring V2](./REFACTORING_V2_SUMMARY.md)** - Phase 2 changes
- **[Zod Schema Improvements](./ZOD_SCHEMA_IMPROVEMENTS.md)** - Leveraging Zod v4 schemas

## ✨ Features

### Pure Algorithm Core
- ✅ Environment-agnostic (works anywhere JS runs)
- ✅ Zero dependencies on UI frameworks
- ✅ Fully typed with TypeScript + Zod schemas
- ✅ Runtime validation with Zod v4
- ✅ Immutable data structures
- ✅ Easy to test

### Beautiful CLI
- ✅ Interactive Ink-based UI
- ✅ ASCII art logo
- ✅ Color-coded output
- ✅ Real-time metrics
- ✅ Navigation with arrow keys

### Modular Architecture
- ✅ Separated concerns (tree, slots, contributors, commitments)
- ✅ Reusable utility functions
- ✅ Clear import paths
- ✅ Better discoverability

## 🎯 Use Cases

### 1. Terminal/CLI Applications
```bash
bun run src/lib/commons/v5/cli.ts example
```

### 2. Server-Side Processing
```typescript
import { computeAllocations } from './free';

app.post('/allocate', (req, res) => {
  const result = computeAllocations(...);
  res.json(result);
});
```

### 3. Svelte Frontend (Reactive)
```typescript
import { myActiveNeeds, myAllocationsAsProvider } from './free-algorithm.svelte';

$effect(() => {
  console.log('Needs:', $myActiveNeeds);
  console.log('Allocations:', $myAllocationsAsProvider);
});
```

### 4. Batch Processing
```typescript
const scenarios = loadScenarios();
for (const scenario of scenarios) {
  const result = computeAllocations(...);
  saveResults(scenario.id, result);
}
```

## 🔧 Development

### Install Dependencies

```bash
bun install
```

### Run Tests

```bash
bun test src/lib/commons/v5/tests/
```

### Type Check

```bash
tsc --noEmit
```

### Lint

```bash
bun run lint
```

## 📖 API Overview

### Core Functions

```typescript
// State management
buildSystemState(commitments, previousState)
createInitialState()

// Allocations
computeAllocations(myPubKey, capacity, recognition, ...)

// Convergence
computeConvergenceSummary(currentState, previousState, startTime)
computeTotalNeedMagnitude(state)
checkUniversalSatisfaction(state)

// Mutual recognition
computeMutualRecognition(myRec, othersRec, myPubKey)

// Damping
computeDampingFactors(history)
updateOverAllocationHistory(history, received, needs)

// Need updates
applyNeedUpdateLaw(currentNeeds, received)
```

### Tree Operations

```typescript
// Navigation
findNodeById(tree, nodeId)
getPathToNode(tree, nodeId)
getParentNode(tree, nodeId)
getDescendants(node)

// Calculations
weight(node, tree)
fulfilled(node, tree)
desire(node, tree)
shareOfParent(node, tree)

// Recognition
sharesOfGeneralFulfillmentMap(rootNode, nodesMap)
mutualFulfillment(nodeA, nodeB, nodesMap)
getAllContributorsFromTree(tree)

// Mutations
addChild(parentNode, id, name, points, contributors)
addContributors(node, contributorIds, antiContributorIds)
updateNodeById(tree, nodeId, updater)
```

### Utility Functions

```typescript
// Contributors
totalContributorPoints(contributors)
findContributor(id, contributors)
getContributorPoints(contributors, id)

// Slots
isSlotRecurring(slot)
formatSlotTimeDisplay(slot)
formatSlotLocationDisplay(slot)
extractResourceMetadata(slot)

// Commitments
createCommitment(recognition, capacity, needs, itcStamp)
getAllocationRecordsForRecipient(state, pubkey)
getMutualTierTotal(state, slotId)
```

## 🎨 CLI Features

### Interactive Mode
- Beautiful ASCII logo
- Arrow key navigation
- Color-coded results
- Real-time metrics
- Scenario selection

### Text Mode
- Quick examples
- Custom simulations
- Batch processing
- CI/CD friendly

## 🔍 Testing

All components are tested:

```bash
# Run all tests
bun test src/lib/commons/v5/

# Test specific module
bun test src/lib/commons/v5/tests/free-algorithm.test.ts

# Test CLI
bun run src/lib/commons/v5/cli.ts example
```

## 📊 Performance

- **Pure functions**: Zero overhead
- **Immutable data**: Predictable performance
- **Type-safe**: Compile-time validation
- **Modular**: Tree-shakeable imports

## 🤝 Contributing

1. Read the documentation
2. Check existing tests
3. Follow TypeScript best practices
4. Add tests for new features
5. Update documentation

## 📝 License

MIT - See project root LICENSE file

## 🙏 Credits

- **Ink** - React for CLIs by Vadim Demedes
- **Free Association Protocol** - Recognition-based allocation
- **TypeScript** - Type safety
- **Bun** - Fast JavaScript runtime

## 🎯 Roadmap

### Near-term
- [ ] Custom scenario builder in CLI
- [ ] Network visualization
- [ ] Export to JSON/CSV
- [ ] Performance profiling tools

### Medium-term
- [ ] WASM compilation
- [ ] Multi-threaded processing
- [ ] Real-time dashboard
- [ ] Web-based visualizer

### Long-term
- [ ] Multi-language bindings (Python, Rust, Go)
- [ ] Machine learning integration
- [ ] Distributed computing support

## 🐛 Troubleshooting

### Common Issues

**"Module not found"**
```bash
bun install
```

**"Permission denied"**
```bash
chmod +x src/lib/commons/v5/cli/index.tsx
```

**"Logo not displaying"**
- Check terminal supports Unicode
- Try a different terminal emulator

## 💡 Tips

1. Use **interactive mode** for exploration
2. Use **text mode** for automation
3. Import from specific modules for better tree-shaking
4. Check convergence metrics for optimization
5. Use damping factors to prevent oscillation

## 📞 Support

- Check documentation first
- Review examples in `examples/`
- Read test files in `tests/`
- Check issue tracker (if applicable)

---

**Version:** v5.1.0 "Playnet"  
**Status:** Production Ready  
**Last Updated:** October 30, 2025

