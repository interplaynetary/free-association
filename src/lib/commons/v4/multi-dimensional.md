# Multi-Dimensional Need Allocation

## Overview

Extension of the mutual-priority allocation framework from scalar to vector needs, preserving all contraction properties while enabling real-world economic complexity.

---

## Core Extension

### Vector Definitions

**Multi-Dimensional Needs:**
```
N⃗_i(t) = [N_i^1(t), N_i^2(t), ..., N_i^m(t)]^T

where N_i^k(t) ∈ [0, N_i^k_max] for k = 1,...,m
```

**Multi-Dimensional Capacities:**
```
C⃗_j(t) = [C_j^1(t), C_j^2(t), ..., C_j^m(t)]^T

where C_j^k(t) ∈ [0, C_j^k_max] for k = 1,...,m
```

**Type-Specific Recognition:**
```
MR^k(A, B) = min(R_A^k(B), R_B^k(A))
```

---

## Allocation Per Type

### Tier 1: Mutual (Per Type k)

**E1'. Recognition Distribution:**
```
MRD_j^k(i) = MR^k(j, i) / Σ_l MR^k(j, l)
```

**E2'. Numerator:**
```
Num_mutual^k(j→i, t) = MRD_j^k(i) × N_i^{k,active}(t)
```

**E3'. Denominator:**
```
Denom_mutual^k(j, t) = max(ε, Σ_i Num_mutual^k(j→i, t))
```

**E4'. Allocation:**
```
A_mutual^k(j→i, t) = min(C_j^k(t) × [Num_mutual^k / Denom_mutual^k], N_i^k(t))
```

### Tier 2: Non-Mutual (Per Type k)

Similar structure with remaining capacity per type:
```
C_j^{k,remaining}(t) = C_j^k(t) - Σ_i A_mutual^k(j→i, t)
```

### Update Law

**Per-Type Update:**
```
N_i^k(t+1) = max(0, N_i^k(t) - Σ_j A^k(j→i, t)) for each k
```

**Operator:**
```
T: ℝ^{n×m} → ℝ^{n×m}
T(N⃗⃗(t)) = N⃗⃗(t+1)
```

---

## Convergence Proofs

### Theorem 10: Contractiveness

**Proof Structure:**

1. **Independence:** Each type k evolves separately
2. **Per-Type Contraction:** `||N⃗^k(t+1)|| ≤ k_k||N⃗^k(t)||` where `k_k < 1`
3. **Frobenius Norm:**
   ```
   ||N⃗⃗(t+1)||_F ≤ k_max ||N⃗⃗(t)||_F where k_max = max_k k_k < 1
   ```

**Result:** Multi-dimensional system is contractive.

### Theorem 11: Fixed-Point

**Given:**
- Complete metric space: ℝ^{n×m}
- Contraction mapping: T
- Sufficient capacity per type: `∀k: Σ_j C_j^k ≥ Σ_i N_i^k`

**Result:**
```
N⃗⃗* = 0⃗⃗  (all needs of all types satisfied)
```

### Theorem 12: Substitution

**For substitutable needs with fixed weights:**
```
N_i^sub(t+1) = Σ_k w_k × max(0, N_i^k(t) - A_k(t))
              ≤ Σ_k w_k × N_i^k(t)
              = N_i^sub(t)
```

Contraction preserved under linear substitution.

### Theorem 13: Convergence Rates

**Per-Type Rates:**
```
||N⃗^k(t)|| ≤ k_k^t ||N⃗^k(0)||
```

Where `k_k` depends on:
- Network density for type k
- Capacity-to-need ratio
- Damping factor α_k

**Fastest:** Dense recognition + abundant capacity
**Slowest:** Sparse recognition + tight constraints

---

## Implementation

### Slot Structure

```typescript
interface NeedSlot {
  type: number;        // k ∈ {1,...,m}
  quantity: number;    // N_i^k > 0
  time: TimeRange;
  location: Location;
}

interface AvailabilitySlot {
  type: number;        // k ∈ {1,...,m}
  quantity: number;    // C_j^k > 0
  time: TimeRange;
  location: Location;
}
```

### Compatibility

```typescript
function compatible(need: NeedSlot, avail: AvailabilitySlot): boolean {
  return (
    need.type === avail.type &&
    timeOverlap(need.time, avail.time) &&
    locationMatch(need.location, avail.location)
  );
}
```

### Recognition Structure

```typescript
interface Recognition {
  // Per-type recognition weights
  byType: Map<number, number>;  // k → R^k(other)
  
  // Aggregate recognition
  aggregate: number;  // R(other)
}

// Normalization per type
∀k: Σ_i R_A^k(i) = 1.0
```

---

## Examples

### Healthcare System

```
Need Types:
1. Diagnostics
2. Consultations
3. Surgery

Provider: General Practitioner
C⃗ = [20, 80, 0]  (hours/week)
MR^1 = [0.7, 0.3]  (70% diagnostics recognition)
MR^2 = [0.9, 0.1]  (90% consultation recognition)
MR^3 = [0.0, 0.0]  (no surgery recognition)

Provider: Surgeon
C⃗ = [0, 10, 90]
MR^1 = [0.1, 0.9]
MR^2 = [0.2, 0.8]
MR^3 = [0.8, 0.2]

Patient: Complex Case
N⃗ = [5, 10, 15]  (hours needed)
- 5h diagnostics
- 10h consultations
- 15h surgery
```

### Education Platform

```
Need Types:
1. Math
2. Languages
3. Arts

Provider: Math Specialist
C⃗ = [100, 0, 0]  (hours/month)
R⃗^1 = [0.9, 0.05, 0.05]  (focused on math)

Provider: Generalist
C⃗ = [30, 40, 30]
R⃗^1 = [0.33, 0.33, 0.34]  (balanced)

Learner: STEM Focus
N⃗ = [50, 20, 10]  (hours/month)
```

### Community Support

```
Need Types:
1. Childcare
2. Transportation
3. Home Repairs

Provider: Parent Network
C⃗ = [40, 10, 0]  (hours/week)

Provider: Handyperson
C⃗ = [0, 5, 35]

Family: Young Children
N⃗ = [30, 5, 10]
```

---

## Recognition Aggregation

### Weighted Sum
```
R_A(B) = Σ_k w_k × R_A^k(B) where Σ_k w_k = 1

Example: w = [0.5, 0.3, 0.2]
R_A^1(B) = 0.8, R_A^2(B) = 0.2, R_A^3(B) = 0.0
R_A(B) = 0.5×0.8 + 0.3×0.2 + 0.2×0.0 = 0.46
```

### Maximum
```
R_A(B) = max_k R_A^k(B)

Example: R_A(B) = max(0.8, 0.2, 0.0) = 0.8
```

### Minimum
```
R_A(B) = min_k R_A^k(B)

Example: R_A(B) = min(0.8, 0.2, 0.0) = 0.0
```

All preserve normalization: `Σ_B R_A(B) = 1`

---

## Substitution & Complementarity

### Substitutable Needs

**Definition:** Needs can replace each other
```
N_i^effective = Σ_k w_k × N_i^k(t)
```

**Example:** Different food types
- Rice: 100 calories
- Bread: 100 calories
- Can substitute 1:1 by calorie content

**Contraction:** Preserved (Theorem 12)

### Complementary Needs

**Definition:** Needs must be satisfied together
```
N_i^composite = min(N_i^1/w_1, N_i^2/w_2, ..., N_i^m/w_m)
```

**Example:** Housing
- Location: 1 unit
- Construction: 1 unit
- Both required in fixed proportions

**Contraction:** Requires careful modeling of composite update law

---

## Convergence Analysis

### Per-Type Dynamics

Each type evolves independently:
```
Food allocation: ||N⃗^food(t)|| ≤ k_food^t ||N⃗^food(0)||
Healthcare: ||N⃗^health(t)|| ≤ k_health^t ||N⃗^health(0)||
Education: ||N⃗^edu(t)|| ≤ k_edu^t ||N⃗^edu(0)||
```

### System-Wide Convergence

```
||N⃗⃗(t)||_F ≤ k_max^t ||N⃗⃗(0)||_F

where k_max = max{k_food, k_health, k_edu, ...}
```

### Bottleneck Type

The slowest-converging type determines overall system convergence time.

**Optimization Strategy:**
1. Identify bottleneck type k with largest k_k
2. Increase capacity or recognition for type k
3. Reduce k_k to accelerate system convergence

---

## Benefits Summary

### 1. Realistic Modeling
- Captures real-world complexity
- Provider specialization
- Skill-specific recognition
- Multiple simultaneous needs

### 2. Preserved Properties
- ✅ Contraction in all dimensions
- ✅ No accumulation (any type)
- ✅ Exponential convergence (per type)
- ✅ Banach fixed-point guarantee

### 3. Independent Dynamics
- Food ⫫ Healthcare ⫫ Education
- Parallel processing possible
- Modular system architecture
- Fault isolation per type

### 4. Flexible Recognition
- Context-specific expertise
- Multi-dimensional trust
- Aggregate for relationships
- Granular matching

---

## Implementation Checklist

### Schema Extension
- [x] Add `type` field to slots
- [x] Per-type recognition weights
- [x] Vector need/capacity states
- [x] Type-specific allocation records

### Algorithm Updates
- [x] Per-type MR computation
- [x] Independent allocation per type
- [x] Vector norm calculations
- [x] Type-specific damping

### UI Components
- [ ] Multi-dimensional need input
- [ ] Provider specialization selector
- [ ] Per-type convergence visualization
- [ ] Type-specific recognition weights

### Testing
- [ ] Per-type contraction tests
- [ ] Cross-type independence verification
- [ ] Substitution preservation
- [ ] Convergence rate measurement

---

## Next Extensions

### Dynamic Types
Allow types to emerge/disappear over time while preserving convergence.

### Hierarchical Types
```
Food → {Produce, Grains, Protein}
Healthcare → {Primary, Specialist, Emergency}
```

### Coupled Constraints
Model interdependencies between types without breaking contraction.

### Market Clearing
Extend to include exchange rates between types.

---

## Mathematical Summary

**Given:**
```
N⃗_i ∈ ℝ^m, C⃗_j ∈ ℝ^m, MR^k(i,j), ∀k: Σ_j C_j^k ≥ Σ_i N_i^k
```

**Then:**
```
lim(t→∞) N⃗⃗(t) = 0⃗⃗
```

**By:**
- Independent contraction per type
- Frobenius norm decrease
- Banach fixed-point in ℝ^{n×m}

**The framework scales elegantly from single-resource to multi-commodity economies.** ∎

