# Multi-Level Denominators: Nested Renormalization System

## Core Concept

A hierarchical allocation system where each level maintains local proportionality through its own denominator, creating a fractal structure of conservation laws.

---

## The Three-Level Structure

```
LEVEL 1: Global Resource Pool
    ↓ [Global Denominator]
LEVEL 2: Provider Capacities  
    ↓ [Provider Denominators]
LEVEL 3: Recipient Needs
    ↓ [Need-Type Denominators]
ACTUAL ALLOCATIONS
```

Each level implements **local proportionality** with its own denominator.

---

## Level 1: Global Resource Allocation

### Setup
```
Total-Global-Resources = R (fixed amount available globally)

Providers = {P₁, P₂, ..., Pₘ}

Each provider has "weight" in the global system:
    Global-Weight(Pᵢ) = 
        Function of their total recognition received,
        capacity to help, or participation level
```

### Formula
```
Global-Denominator = Σ Global-Weight(Pⱼ) for all providers j

Capacity(Pᵢ) = Total-Global-Resources × 
               Global-Weight(Pᵢ)
               ─────────────────────
               Global-Denominator
```

### Example: Recognition-Based Global Weight
```
Global-Weight(Provider) = Σ MR(R, Provider) for all recipients R

Interpretation: Providers who are more recognized globally
                get more capacity to allocate
```

### Conservation Law
```
Σ Capacity(Pᵢ) = Total-Global-Resources
```

---

## Level 2: Provider → Recipient Allocation

### Setup
```
Provider P has Capacity(P) from Level 1

Recipients = {R₁, R₂, ..., Rₙ}

Each recipient has "weight" with this provider:
    Provider-Weight(P, Rᵢ) = MR(P, Rᵢ) × Effective-Desire(Rᵢ)
```

### Formula
```
Provider-Denominator(P) = Σ Provider-Weight(P, Rⱼ) 
                          for all recipients j with mutual-desire

Allocation(P → Rᵢ) = Capacity(P) × 
                     Provider-Weight(P, Rᵢ)
                     ─────────────────────────
                     Provider-Denominator(P)
```

### Conservation Law
```
Σ Allocation(P → Rᵢ) = Capacity(P)
```

---

## Level 3: Recipient Need-Type Allocation

### Setup
```
Recipient R receives Total-Received(R) from Level 2:
    Total-Received(R) = Σ Allocation(Pⱼ → R) for all providers j

R has multiple need-types: {food, housing, healthcare, ...}

Each need-type has "weight":
    Need-Weight(R, type) = 
        Priority(type) × Urgency(type) × Unsatisfied-Amount(type)
```

### Formula
```
Need-Denominator(R) = Σ Need-Weight(R, type) for all need-types

Fulfillment(R, type) = Total-Received(R) × 
                       Need-Weight(R, type)
                       ──────────────────────
                       Need-Denominator(R)
```

### Conservation Law
```
Σ Fulfillment(R, type) = Total-Received(R)
```

---

## Complete Three-Level Flow

```
Global Resources (R)
    |
    | Global-Denominator = Σ Global-Weight(Pⱼ)
    ↓
Provider P₁ gets: R × [Global-Weight(P₁) / Global-Denominator]
Provider P₂ gets: R × [Global-Weight(P₂) / Global-Denominator]
...
    |
    | Provider-Denominator(P₁) = Σ MR(P₁,Rⱼ) × Desire(Rⱼ)
    ↓
Recipient R₁ gets: Capacity(P₁) × [Weight(R₁) / Provider-Denom(P₁)]
                 + Capacity(P₂) × [Weight(R₁) / Provider-Denom(P₂)]
                 + ...
    |
    | Need-Denominator(R₁) = Σ Need-Weight(R₁, type)
    ↓
Food:      Total-Received(R₁) × [Food-Weight / Need-Denom(R₁)]
Housing:   Total-Received(R₁) × [Housing-Weight / Need-Denom(R₁)]
Healthcare: Total-Received(R₁) × [Healthcare-Weight / Need-Denom(R₁)]
```

---

## Concrete Example

### Global Level
```
Total-Global-Resources = 1000

Provider A:
    Recognized by 5 recipients with total MR = 2.0
    Global-Weight(A) = 2.0

Provider B:
    Recognized by 3 recipients with total MR = 1.5
    Global-Weight(B) = 1.5

Global-Denominator = 2.0 + 1.5 = 3.5

Capacity(A) = 1000 × (2.0 / 3.5) = 571.43
Capacity(B) = 1000 × (1.5 / 3.5) = 428.57

Check: 571.43 + 428.57 = 1000 ✓
```

### Provider Level (Provider A)
```
Capacity(A) = 571.43

A's recipients:
    R1: MR(A,R1) = 0.6, Desire = 500 → Weight = 300
    R2: MR(A,R2) = 0.4, Desire = 300 → Weight = 120

Provider-Denominator(A) = 300 + 120 = 420

Allocation(A → R1) = 571.43 × (300/420) = 408.16
Allocation(A → R2) = 571.43 × (120/420) = 163.27

Check: 408.16 + 163.27 = 571.43 ✓
```

### Recipient Level (Recipient R1)
```
Total-Received(R1) = 408.16 (from A) + 149.49 (from B) = 557.65

R1's needs:
    Food:      Priority = 1.0, Unsatisfied = 200 → Weight = 200
    Housing:   Priority = 0.8, Unsatisfied = 300 → Weight = 240
    Healthcare: Priority = 0.5, Unsatisfied = 100 → Weight = 50

Need-Denominator(R1) = 200 + 240 + 50 = 490

Fulfillment(R1, Food) = 557.65 × (200/490) = 227.61
Fulfillment(R1, Housing) = 557.65 × (240/490) = 273.13
Fulfillment(R1, Healthcare) = 557.65 × (50/490) = 56.91

Check: 227.61 + 273.13 + 56.91 = 557.65 ✓
```

---

## The Cascade of Conservation Laws

```
Level 1: Σ Capacity(Pᵢ) = Global-Resources
         ↓
Level 2: Σ Allocation(P → Rⱼ) = Capacity(P)    [for each P]
         ↓
Level 3: Σ Fulfillment(R, type) = Total-Received(R)    [for each R]
```

Each level **perfectly conserves** what it receives from the level above.

This creates a **fractal structure** of nested proportionality.

---

## Dynamic Renormalization Across Levels

### Scenario: Provider B drops out

**Level 1 responds:**
```
Global-Weight(A) = 2.0  (unchanged)
Global-Weight(B) = 0    (dropped out!)

Global-Denominator = 2.0 + 0 = 2.0  (was 3.5)

Capacity(A) = 1000 × (2.0 / 2.0) = 1000  (was 571.43)

Provider A now gets ALL global resources!
```

**Level 2 responds (Provider A):**
```
Capacity(A) increased from 571.43 → 1000

But Provider-Denominator(A) unchanged = 420

Allocation(A → R1) = 1000 × (300/420) = 714.29  (was 408.16)
Allocation(A → R2) = 1000 × (120/420) = 285.71  (was 163.27)

Both recipients get more!
```

**Level 3 responds (Recipient R1):**
```
Total-Received(R1) increased:
    Was: 408.16 (from A) + 149.49 (from B) = 557.65
    Now: 714.29 (from A) + 0 (B dropped)   = 714.29

Need-Denominator(R1) unchanged = 490

Fulfillment(R1, Food) = 714.29 × (200/490) = 291.55  (was 227.61)
Fulfillment(R1, Housing) = 714.29 × (240/490) = 349.86  (was 273.13)
Fulfillment(R1, Healthcare) = 714.29 × (50/490) = 72.88  (was 56.91)

All need-types get more!
```

**The cascade propagates through!** Each level automatically redistributes based on its local denominator.

---

## Composing Allocations Through Denominators

The **end-to-end allocation** from global resources to a specific need-type:

```
Allocation(Global → R, Need-Type) = 
    
    Global-Resources ×
    
    [Global-Weight(P) / Global-Denom] ×
    
    [Provider-Weight(P, R) / Provider-Denom(P)] ×
    
    [Need-Weight(R, Type) / Need-Denom(R)]
```

This is a **product of normalized fractions** at each level.

Each denominator **factors out** dependence on absolute scales.

---

## Information Flow

### Top-Down (Resource Flow)
```
Global pool
    ↓ [Denominator 1 normalizes]
Providers
    ↓ [Denominator 2 normalizes]
Recipients  
    ↓ [Denominator 3 normalizes]
Need-types
```

Resources **cascade down**, renormalized at each level.

### Bottom-Up (Signal Flow)
```
Need-types (unsatisfied amounts)
    ↑ [aggregate into total recipient need]
Recipients (desires, MR values)
    ↑ [aggregate into provider workload]
Providers (global recognition)
    ↑ [aggregate into global demand]
```

Information **bubbles up**, determining weights at each level.

---

## Extended Model: Arbitrary Depth

You could have **N levels**:

```
Level 0: Ultimate resource pool
    ↓ [Denom₀]
Level 1: Continental allocation
    ↓ [Denom₁]
Level 2: National allocation
    ↓ [Denom₂]
Level 3: Regional allocation
    ↓ [Denom₃]
...
Level N: Individual need-types
```

### General Formula

```
Allocation(Level-k → Level-(k+1)) = 

    Capacity(Level-k) × 
    Weight(Level-(k+1)) 
    ─────────────────────────────────
    Denominator(Level-k)

where:
    Denominator(Level-k) = Σ Weight(all children at Level-(k+1))
```

This is a **tree structure** where each node renormalizes among its children.

---

## Equilibrium: Multi-Level Fixed Point

The system converges when **all denominators stabilize**:

```
Converged state:
    ∀ Provider P:
        Provider-Denom(P) stops changing
        → All Allocation(P → R) stable
        → All R's effective-desires stable
        
    ∀ Recipient R:
        Need-Denom(R) stops changing
        → All Fulfillment(R, type) stable
        → All priorities/urgencies stable
    
    Global-Denom stops changing
        → All Capacity(P) stable
```

This is a **hierarchical fixed point**.

---

## Cross-Level Coupling

### Bottom-Up Feedback
```
If Recipient R's needs aren't met:
    → R's satisfaction low
    → R gives less recognition to providers
    → Providers' global weights decrease
    → Global denominator shifts
    → Resource reallocation at top level
```

### Top-Down Constraints
```
If Global resources shrink:
    → All provider capacities decrease
    → Provider denominators unchanged
    → But absolute allocations scale down
    → Recipient satisfactions decrease
    → Need-level weights rebalance
```

---

## Properties

### Local Incentive Compatibility

At each level, no entity benefits from misreporting:

```
Provider P wants to maximize Capacity(P)
    → Wants to maximize Global-Weight(P)
    → Should maximize recognition received
    → Cannot fake (bilateral MR values)

Recipient R wants to maximize Total-Received(R)
    → Wants to maximize Provider-Weight(P,R) for all P
    → Should maximize MR values given (mutual recognition)
    → Cannot unilaterally inflate
```

### Strategyproof Renormalization

The denominator makes the system **strategyproof** at each level:

```
If Provider A tries to "inflate" their global weight:
    Global-Denominator increases
    A's share = Weight(A) / (Weight(A) + Weight(B) + ...)
    
    If only numerator increases, share increases
    But if denominator increases proportionally, share unchanged!
```

The system resists manipulation because **everyone is in the denominator**.

---

## Summary Table

| Level | Input | Denominator | Output | Conservation |
|-------|-------|-------------|--------|--------------|
| **Global** | Total resources R | Σ Global-Weight(P) | Capacity(P) for each provider | Σ Capacity = R |
| **Provider** | Capacity(P) | Σ Provider-Weight(P,R) | Allocation(P→R) for each recipient | Σ Allocation = Capacity(P) |
| **Recipient** | Total-Received(R) | Σ Need-Weight(R,type) | Fulfillment(R,type) | Σ Fulfillment = Received(R) |

Each row is **independent** yet **composed** with rows above/below.

---

## Comparison to Other Algorithms

### vs. Single-Level Allocation (Base/Need-Fulfillment)
- **Single-level:** One provider allocates to recipients
- **Multi-level:** Hierarchical cascade with multiple coordination points
- **Advantage:** Handles larger-scale coordination, nested scarcity constraints

### vs. Centralized Hierarchy
- **Centralized:** Top-down control at each level
- **Multi-level denominators:** Local proportionality, automatic propagation
- **Advantage:** No central decision-maker, self-organizing

### Key Insight

Each level is **locally fair** (proportional sharing via denominator), and when composed, creates **globally coherent** resource distribution.

---

## Use Cases

1. **Supply chains:** Multi-tier distribution from manufacturers → distributors → retailers → consumers
2. **Organizations:** Budget allocation from company → departments → teams → individuals
3. **Ecosystems:** Energy flow through trophic levels
4. **Recognition economy:** Global recognition → provider capacities → recipient allocations → need fulfillment

---

## Mathematical Beauty

The multi-level denominator structure is **functorial**:

```
Category of Resource Distributions:
    Objects: Resource pools at each level
    Morphisms: Allocation functions (with denominators)

Properties:
    1. Composition: 
       Level-1 → Level-2 → Level-3 
       equals
       Level-1 → Level-3 (by multiplying fractions)
       
    2. Identity:
       Each level perfectly conserves (Σ = 1 when normalized)
       
    3. Associativity:
       Order of renormalization doesn't matter
```

---

## Summary

**Multi-level denominators create a cascade of local proportionality that preserves global structure.**

Each level:
- Has its own scarcity constraint (denominator)
- Implements local fairness (proportional sharing)
- Responds to changes independently
- Automatically propagates effects up/down

Together they form a **coherent hierarchical system** where:
- Resources flow top-down
- Signals flow bottom-up  
- Proportionality maintained at every scale
- Conservation laws nested recursively

**In one sentence:** Multi-level denominators extend proportional allocation to hierarchical systems by nesting renormalization operators at each scale, creating a fractal structure where local conservation laws compose into global coherence.

