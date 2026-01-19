# Allocation

Allocation is the **derived distribution** of capacities to needs based on recognition weights and constraints.

## Overview

The allocation algorithm solves a **two-sided constrained optimization** problem:

**Challenge**: Find allocation matrix **X** such that:
- Providers allocate to recipients they recognize most
- Recipients receive from providers they prefer most  
- All capacity and need constraints are satisfied

$$
\text{Find } X \text{ s.t. } \forall i, \sum_j X_{ij} \leq C_i \land \forall j, \sum_i X_{ij} \leq N_j
$$

Where:
- $C_i$ = Capacity of provider i
- $N_j$ = Need of recipient j
- $X_{ij}$ = Allocation from provider i to recipient j

---

## Two-Sided Optimization

The system must simultaneously satisfy:

1. **Provider priorities**: Allocate proportionally to recognition of recipients
2. **Recipient preferences**: Receive from preferred providers
3. **Capacity constraints**: $\sum_j X_{ij} \leq C_i$ for all providers
4. **Need constraints**: $\sum_i X_{ij} \leq N_j$ for all recipients

This is a **constrained weighted allocation problem** that finds the allocation matrix minimizing deviation from both providers' priorities and recipients' preferences.

---

## Mathematical Properties

### Proportional Preservation

If you express that Recipient A should receive twice as much as Recipient B (through recognition), the system allocates approximately twice as much capacity to A when feasible given constraints.

$$
\frac{X_{ij}}{X_{ik}} \approx \frac{P_{ij}}{P_{ik}}
$$

The proportional relationships you express are preserved in the final allocation.

### Least Biased Solution

Among all possible allocations satisfying the constraints, the system selects the one that introduces the least additional bias beyond what entities express.

This is the **entropy-maximizing** (information-theoretically optimal) solution.

### Constraint Propagation

When constraints bind (e.g., a recipient reaches capacity), the effects propagate through the network.

Capacity that cannot flow to a full recipient automatically redistributes to other compatible needs according to expressed preferences.

### Equilibrium Convergence

The system converges to a stable equilibrium where no entity can improve their allocation quality (measured by preference satisfaction) without degrading someone else's.

This is a **Pareto-efficient** outcome.

---

## How Allocation Happens

Once all entities have published their data:

1. **Filter** for compatible resource specifications
2. **Calculate** recognition weights for all potential allocations
3. **Optimize** allocation matrix to satisfy both provider priorities and recipient preferences
4. **Apply** constraints (capacity limits and need bounds)
5. **Update** remaining needs automatically
6. **Recompute** optimal allocation as network state changes (~100-200ms per update)

The entire process happens automatically through two-sided optimization. The system finds the allocation that best satisfies both:
- **Provider priorities**: Who providers want to support (proportional to recognition)
- **Recipient preferences**: Who recipients want to receive from

Recognition determines the proportions. Constraints set the bounds. **No meetings. No applications. No bureaucracy.**

---

## Dynamic Updates

The system continuously adapts to changing conditions.

### Need Updates

After each allocation round:

```
Remaining_Need = max(0, Declared_Need - Total_Received)
```

As allocations are received, remaining needs decrease. System recalculates optimal allocation for updated need state.

### Independent Resource Tracking

Each resource type tracks independently:
- Funding needs separate from expertise needs
- Time commitments independent of facility access

---

## Convergence Properties

### Speed

System converges to stable equilibrium rapidly (~100-200ms per update).

### Stability

Once converged, allocations remain stable unless:
- Network state changes (new needs, capacity, or recognition)
- Resource specifications updated
- Participants join or leave

Upon a change, system immediately re-calculates allocations. Dynamic convergence takes place.

### Optimality

At equilibrium:
- Resources distributed proportional to recognition weights
- All capacity and need constraints satisfied
- **Pareto Efficiency**: No allocation can be improved without violating a constraint or degrading another entity's preference satisfaction
- **Proportional Preservation**: Allocation ratios match recognition ratios where constraints allow

---

## Implementation: IPF Algorithm

The reference implementation uses **Iterative Proportional Fitting (IPF)**:

1. Initialize allocation matrix with recognition weights
2. Iteratively adjust to satisfy row constraints (capacity)
3. Iteratively adjust to satisfy column constraints (needs)
4. Repeat until convergence (Δ < ε)

**Properties:**
- Guaranteed convergence
- Preserves proportional relationships
- Entropy-maximizing solution

**Performance**: O(N × M × K) where N = providers, M = recipients, K = iterations to convergence

---

## Further Reading

- [Recognition](recognition.md) - How recognition weights are derived
- [Resources](../1-publishing/resources.md) - How capacities and needs are declared
- [Mathematics Reference](../reference/mathematics.md) - Formal proofs and properties
