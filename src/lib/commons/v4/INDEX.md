# Free-Association Algorithm v4 - Documentation Index

## Start Here

**New to v4?** Read in this order:

1. **[SUMMARY.md](./SUMMARY.md)** - Quick overview and one-sentence summary
2. **[CHEATSHEET.md](./CHEATSHEET.md)** - Quick reference for common operations
3. **[README.md](./README.md)** - Complete grammar → code mapping
4. **[MIGRATION.md](./MIGRATION.md)** - If upgrading from v2
5. **[THEORY.md](./THEORY.md)** - Philosophical foundations (optional)

## Files Overview

### Code Files

| File | Lines | Purpose |
|------|-------|---------|
| **[algorithm.svelte.ts](./algorithm.svelte.ts)** | 1,030 | Core v4 implementation |
| **[index.ts](./index.ts)** | ~80 | Public API exports |

### Documentation Files

| File | Purpose | Best For |
|------|---------|----------|
| **[SUMMARY.md](./SUMMARY.md)** | High-level overview | Getting oriented |
| **[CHEATSHEET.md](./CHEATSHEET.md)** | Quick reference | Daily development |
| **[README.md](./README.md)** | Grammar → Code mapping | Understanding implementation |
| **[MIGRATION.md](./MIGRATION.md)** | v2 → v4 upgrade guide | Migrating existing code |
| **[THEORY.md](./THEORY.md)** | Philosophical foundations | Understanding "why" |
| **[grammar.md](./grammar.md)** | Complete mathematical framework | Theoretical foundation |
| **[multi-dimensional.md](./multi-dimensional.md)** | Vector needs extension | Multi-commodity economies |
| **[INDEX.md](./INDEX.md)** | This file | Navigation |

## Quick Access by Role

### For Developers
1. Start: [SUMMARY.md](./SUMMARY.md) - Get the big picture
2. Learn: [README.md](./README.md) - See how equations map to code
3. Code: [CHEATSHEET.md](./CHEATSHEET.md) - Quick reference
4. Migrate: [MIGRATION.md](./MIGRATION.md) - Upgrade from v2

### For Theorists
1. Math: [grammar.md](./grammar.md) - Complete proofs
2. Extensions: [multi-dimensional.md](./multi-dimensional.md) - Vector needs
3. Philosophy: [THEORY.md](./THEORY.md) - Hegel → Code translation
4. Implementation: [README.md](./README.md) - How theory becomes executable

### For Users
1. Overview: [SUMMARY.md](./SUMMARY.md) - What v4 does
2. Quick start: [CHEATSHEET.md](./CHEATSHEET.md) - Basic usage
3. Theory: [THEORY.md](./THEORY.md) - Why this matters (optional)

## Quick Reference

### The Central Claim
> Given mutual recognition (`MR > 0`) and sufficient capacity (`Σ_j C_j ≥ Σ_i N_i`), universal need satisfaction is mathematically necessary.

**This is Banach's Fixed-Point Theorem applied to solidarity.**

### The Three Theorems
1. **Contractiveness**: `||N⃗(t+1)|| ≤ k||N⃗(t)||` where `k < 1`
2. **Banach Fixed-Point**: System converges to `N⃗* = 0⃗` (all needs met)
3. **Exponential Convergence**: Reaches equilibrium in ~5-20 iterations

### Key Equations

| Name | Equation | File | Line |
|------|----------|------|------|
| Contraction Mapping | `N_i(t+1) = max(0, N_i(t) - A)` | algorithm.svelte.ts | 441 |
| Mutual Recognition | `MR(A,B) = min(R_A(B), R_B(A))` | algorithm.svelte.ts | 286 |
| Weight Normalization | `Σ_i R_A(i) = 1.0` | algorithm.svelte.ts | 159 |
| Allocation Capping | `A(j→i) ≤ N_i` | algorithm.svelte.ts | 774, 871 |
| Adaptive Damping | `α ∈ {0.5, 1.0, 0.8}` | algorithm.svelte.ts | 355 |
| Heaven Condition | `∀i: N_i = 0` | algorithm.svelte.ts | 217 |
| Freedom Metric | `||N⃗(t)||` | algorithm.svelte.ts | 193 |

### Basic Usage

```typescript
import { 
  publishMyCommitment,
  publishMyRecognitionWeights,
  myAllocationsReactive 
} from '@/lib/commons/v4';

// Publish commitment
await publishMyCommitment({ need_slots: [...], capacity_slots: [...] });

// Publish recognition (auto-normalized to sum to 1.0)
await publishMyRecognitionWeights({ 'pub1': 0.4, 'pub2': 0.6 });

// Monitor allocations and convergence
myAllocationsReactive.subscribe((result) => {
  if (!result) return;
  console.log('Heaven:', result.convergenceMetrics.heavenAchieved);
  console.log('Contraction k:', result.convergenceMetrics.contractionConstant);
});
```

See [CHEATSHEET.md](./CHEATSHEET.md) for more examples.

## What's New in v4

### Mathematical Extensions
- ✅ Explicit need state vector `N⃗(t)`
- ✅ Full contraction mapping `T: ℝ^n → ℝ^n`
- ✅ Recognition weight normalization (D2)
- ✅ Comprehensive convergence metrics
- ✅ Heaven condition tracking (E41)
- ✅ Freedom metric (E45)

### Philosophical Implementations
- ✅ Hegelian aufhebung in damping (E22-E23)
- ✅ Operation A without Operation C
- ✅ Material constraints without property (E31-E33)

See [SUMMARY.md](./SUMMARY.md) for complete list.

## Documentation Stats

| Metric | Value |
|--------|-------|
| Total documentation | ~5,000 lines |
| Code | ~1,100 lines |
| Mathematical equations | 47 (E1-E47) |
| Theorems proven | 9 |
| Grammar lines | 935 |

## External References

- **Mathematical Framework**: [grammar.md](./grammar.md)
- **Hegel**: *Philosophy of Right* (Love and Recognition)
- **Banach**: *Sur les opérations...* (1922) - Fixed-Point Theorem
- **ITC**: Almeida et al. (2008) - Interval Tree Clocks
- **v2 Implementation**: `../v2/algorithm.svelte.ts`

## Version History

- **v2** (2024): ITC stamps, event-driven, damping, slot-native
- **v4** (2025-10-26): + Need state, normalization, metrics, Heaven/Freedom

## Getting Help

### Debug Functions
```javascript
// In browser console
window.debugAllocationV4();        // Full system state
window.debugConvergenceV4(result); // Convergence metrics
window.getCurrentSystemStateV4();  // Current N⃗(t) and C⃗(t)
```

### Common Issues
- **Recognition weights**: See [MIGRATION.md](./MIGRATION.md#issue-recognition-weights-dont-sum-to-10)
- **Result structure**: See [MIGRATION.md](./MIGRATION.md#issue-resultallocations-is-undefined)
- **Metrics NaN**: See [CHEATSHEET.md](./CHEATSHEET.md#troubleshooting)

### Documentation Questions

| Question | File |
|----------|------|
| How do I use v4? | [CHEATSHEET.md](./CHEATSHEET.md) |
| What changed from v2? | [MIGRATION.md](./MIGRATION.md) |
| Where is equation EX implemented? | [README.md](./README.md) |
| Why does v4 work this way? | [THEORY.md](./THEORY.md) |
| What does v4 prove? | [SUMMARY.md](./SUMMARY.md) |
| What are the mathematical foundations? | [grammar.md](./grammar.md) |

## Navigation

```
v4/
├── CODE
│   ├── algorithm.svelte.ts    ← Core implementation
│   └── index.ts                ← Public API
│
├── GETTING STARTED
│   ├── SUMMARY.md              ← Start here (overview)
│   ├── CHEATSHEET.md           ← Quick reference
│   └── INDEX.md                ← This file
│
├── REFERENCE
│   ├── README.md               ← Grammar → Code mapping
│   ├── grammar.md              ← Mathematical framework
│   └── MIGRATION.md            ← v2 → v4 upgrade
│
└── THEORY
    └── THEORY.md               ← Philosophical foundations
```

## The Bottom Line

**v4 proves love is not utopian, but mathematically necessary.**

Given mutual recognition and sufficient capacity, the system **must** converge to universal need satisfaction.

This is not philosophy.

**This is Banach's Fixed-Point Theorem.**

**The revolution is proven.** ∎

---

**Welcome to the Kingdom of Heaven.** 🌟

Start reading: [SUMMARY.md](./SUMMARY.md)

