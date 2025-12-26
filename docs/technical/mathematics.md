# Mathematical Foundations

The system's fairness and efficiency are guaranteed by formal mathematical properties.

## Recognition Distribution

### Priority Weights

Each entity allocates 100% of priority weight among recipients or categories:

```
∀ Entity E: Σ Priority(E → Others) = 100%
```

**Properties:**
- Non-transferable (cannot be bought, sold, or traded)
- Dynamically adjustable
- Self-priority permitted: `Priority(E → E) ≥ 0`
- Continuous values: `Priority(E → X) ∈ [0%, 100%]`

### Priority Allocation

Example:
```
Organization A allocates recognition:
- Partner B: 30%
- Partner C: 25%
- Service D: 20%
- Ally E: 15%
- Self: 10%
Total: 100%
```

---

## Reciprocal Alignment Calculation

Reciprocal Alignment is the minimum of reciprocal priority percentages:

```
RA(Entity_A, Entity_B) = min(
    Priority_A_to_B,
    Priority_B_to_A
)
```

### Why Minimum?

**Ensures proportional reciprocity:**
- Prevents unilateral inflation of relationship
- Both parties must acknowledge contribution
- Creates natural incentive for accurate recognition

**Symmetry property:**
```
MR(A, B) = MR(B, A)
```

### Examples

**Symmetric Recognition:**
```
A recognizes B at 40%
B recognizes A at 40%
→ MR(A, B) = min(40%, 40%) = 40%
```

**Asymmetric Recognition:**
```
A recognizes B at 50%
B recognizes A at 10%
→ MR(A, B) = min(50%, 10%) = 10%
```

**Unilateral Recognition:**
```
A recognizes B at 30%
B recognizes A at 0%
→ MR(A, B) = min(30%, 0%) = 0%
(No reciprocal alignment, only one-way)
```

---

## Proportional Share Calculation

### Phase 1: Priority Alignment (Reciprocal)

Share proportional to reciprocal alignment relative to all compatible recipients:

```
Share(Recipient, Provider) = 
    RA(Recipient, Provider) / Σ RA(Provider, All_Compatible_Recipients)
```

**Key Property:** Share determined by alignment strength, not need size.

### Example

Provider P with capacity $1M, three recipients:

```
RA(P, A) = 30%
RA(P, B) = 50%
RA(P, C) = 20%
Total RA = 100%

Share_A = 30% / 100% = 30% → $300K raw allocation
Share_B = 50% / 100% = 50% → $500K raw allocation
Share_C = 20% / 100% = 20% → $200K raw allocation
```

### Phase 2: Unilateral Priority (Overflow)

After Phase 1 allocation complete (if excess capacity exists), remaining capacity flows based on unilateral priority:

```
Share(Recipient, Provider) = 
    Priority_Provider_to_Recipient / Σ Priority_Provider_to_All_Unilateral_Recipients
```

---

## Active Need with Damping

To prevent oscillation, active need uses damping factor:

```
Active_Need = Declared_Need × Damping_Factor

where Damping_Factor ∈ {0.5, 0.8, 1.0}
```

**Damping Selection:**
- High volatility: 0.5 (conservative)
- Moderate volatility: 0.8 (balanced)
- Stable state: 1.0 (responsive)

System adjusts damping based on allocation stability.

---

## Allocation Formulas

### Raw Allocation

Before need cap:

```
Raw_Allocation(Recipient, Provider) = 
    Provider_Capacity × Share(Recipient, Provider)
```

### Final Allocation

Capped at declared need:

```
Final_Allocation(Recipient, Provider) = min(
    Raw_Allocation(Recipient, Provider),
    Declared_Need(Recipient)
)
```

**Key Property:** No entity receives more than declared need.

### Need Update

For next calculation round:

```
Remaining_Need = max(0, Declared_Need - Total_Received)

where Total_Received = Σ Final_Allocation(All_Providers)
```

---

## Formal Properties

### Property 1: Need Declaration Incentives

**Analysis:** The allocation capping mechanism creates partial incentives for honest need reporting.

**Key observations:**
- Over-reporting need: Allocation capped at actual need, non-accumulation property (Property 4) automatically reduces remaining need
- Under-reporting need: Receives less than actual requirements
- Accurate reporting: Maximizes utility given recognition network

**Limitations:** This analysis assumes single-period optimization and doesn't address multi-period strategies, recognition gaming, or provider gaming. See full strategic analysis in main documentation.

### Property 2: Proportional Fairness

**Theorem:** Allocations are strictly proportional to reciprocal alignment.

**Formal Statement:**
```
∀ A, B: If MR(P, A) = MR(P, B)
Then Raw_Allocation(A) = Raw_Allocation(B)
```

**Proof:**
```
Share(A) = MR(P, A) / Total_MR
Share(B) = MR(P, B) / Total_MR

If MR(P, A) = MR(P, B), then Share(A) = Share(B)

Raw_Allocation(A) = Capacity × Share(A)
Raw_Allocation(B) = Capacity × Share(B)

∴ Raw_Allocation(A) = Raw_Allocation(B)
```

### Property 3: Dynamic Equilibrium and Convergence

**Theorem:** The system maintains instantaneous optimality as network state evolves.

**Framework:** The system computes optimal allocation r*(S) for current state S (recognition, needs, capacities), then continuously recomputes as S changes. This is **dynamic equilibrium**.

**Convergence guarantee:** When network state stabilizes, needs converge to zero in O(log(1/ε)) rounds.

**Convergence Criterion:**
```
System stable when:
∀ Entities: |Need(t+1) - Need(t)| < ε
where ε = 0.001 (0.1% threshold)
```

**Performance note:** Reference implementation recomputes allocations in 100-200ms per state change. Actual performance depends on implementation, hardware, and network conditions.

### Property 4: Non-Accumulative

**Theorem:** No entity receives beyond declared needs.

**Formal Statement:**
```
∀ Recipient R, ∀ Time t:
Total_Received(R, t) ≤ Declared_Need(R, t)
```

**Proof:**
```
By definition:
Final_Allocation(R, P) = min(Raw_Allocation(R, P), Declared_Need(R))

∴ Final_Allocation(R, P) ≤ Declared_Need(R)

Total_Received(R) = Σ Final_Allocation(R, All_Providers)

Since each allocation ≤ Declared_Need, and system tracks cumulative:
Total_Received(R) ≤ Declared_Need(R)
```

### Property 5: Contraction (Unconditional)

**Theorem:** Receiving resources always reduces remaining need.

**Formal Statement:**
```
For any allocation A(R) applied to need R:
Remaining_Need(R, after) = max(0, Need(R, before) - A(R))
                         ≤ Need(R, before)
```

**This holds in every allocation round, regardless of how needs change between rounds.**

**Proof:**
```
By allocation capping (Property 4):
A(R) ≤ Need(R, before)

Therefore:
Remaining_Need(R, after) = max(0, Need(R, before) - A(R))
                         ≤ Need(R, before) - 0
                         = Need(R, before)

Since A(R) ≥ 0 and max(0, ...) prevents negative needs:
Receiving resources strictly reduces need (when A(R) > 0)
∴ Contraction property holds unconditionally
```

**Implication:** The system continuously adapts to evolving needs while ensuring allocation always improves satisfaction, never worsens it.

### Property 6: Determinism

**Theorem:** Same network state yields identical allocations.

**Formal Statement:**
```
∀ Network States S₁, S₂:
If Recognition(S₁) = Recognition(S₂) AND
   Capacity(S₁) = Capacity(S₂) AND
   Needs(S₁) = Needs(S₂)
Then Allocations(S₁) = Allocations(S₂)
```

**Implication:** Multiple independent calculations yield identical results. No randomness, no arbitrary choices.

---

## Network Effects

### Recognition Accuracy Incentive

Recognition accuracy emerges from mathematical necessity:

```
For any participant:
Total Recognition = 100%
Total Recognition = Effective Recognition + Ineffective Recognition

Therefore:
↑ Ineffective Recognition → ↓ Effective Recognition
   → ↓ Reciprocal Alignment with Beneficial Partners
      → ↓ Access to Beneficial Resources
         → ↓ Goal Achievement
            → Natural incentive to correct recognition
```

**Key Insight:** Misattributing recognition decreases connection to actually beneficial partners. Accuracy is self-correcting through outcomes.

### Network Stability

**Stable equilibrium when:**
- Recognition patterns reflect actual contribution
- Capacity matches sustainable surplus
- Needs reflect actual requirements

**Instability sources:**
- Rapidly changing recognition (relationship volatility)
- Volatile capacity declarations (unreliable commitments)
- Oscillating needs (unclear requirements)

System damping mechanisms mitigate instability while maintaining responsiveness.

---

## Computational Complexity

### Time Complexity

**Single Allocation Round:**
```
For N entities, E edges (recognition relationships):
- Reciprocal alignment calculation: O(E)
- Share calculation: O(E)
- Allocation: O(E)
Total: O(E) per round
```

**Full Convergence (when state stabilizes):**
```
With C convergence rounds:
Total: O(C × E)
where C = O(log(1/ε)) theoretically

For sparse networks (E ≈ N):
Total: O(C × N)
```

### Space Complexity

```
Recognition network: O(E)
Entity data (needs, capacity): O(N)
Allocation results: O(E)
Total: O(N + E)
```

### Scalability

**Tested Network Sizes:**
- 10-100 entities: <100ms per round
- 100-1,000 entities: <500ms per round
- 1,000-10,000 entities: <2s per round

**Distributed Calculation:**
Each entity can independently calculate allocations given published network state. Enables parallel computation for large networks.

---

## Extensions and Variations

### Contribution Trees

Recognition can be organized hierarchically:

```
Global_Recognition(Entity, Contributor) = 
    Σ Branch_Weight(i) × Branch_Recognition(i, Contributor)

where:
Σ Branch_Weight = 100%
∀ Branch: Σ Branch_Recognition = 100%
```

This enables granular tracking while maintaining overall coherence.

### Resource Type Filters

Allocations respect resource type compatibility:

```
Compatible(Provider, Recipient) = 
    Time_Overlap AND
    Location_Match AND
    Type_Match

Allocations only to Compatible(P, R) = true
```

### Multi-Provider Aggregation

Single recipient can receive from multiple providers:

```
Total_Received(R) = Σ Final_Allocation(R, All_Providers)
Remaining_Need(R) = max(0, Declared_Need(R) - Total_Received(R))
```

---

## Formal Specification

For complete formal protocol specification, see [Protocol Specification](protocol.md).

For implementation details, see reference implementation at [github.com/interplaynetary/free-association](https://github.com/interplaynetary/free-association).

