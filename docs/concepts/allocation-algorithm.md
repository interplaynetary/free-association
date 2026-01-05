# The Allocation Algorithm

## Two-Sided Constrained Optimization

The allocation algorithm solves a multi-provider, multi-recipient resource distribution problem:

**Challenge:** Find allocation matrix **X** such that:
- Providers allocate to recipients they recognize most
- Recipients receive from providers they prefer most  
- All capacity and need constraints are satisfied

$$
\text{Find } X \text{ s.t. } \forall i, \sum_j X_{ij} \leq C_i \land \forall j, \sum_i X_{ij} \leq N_j
$$

Where $C_i$ = Capacity of provider i, $N_j$ = Need of recipient j.

### Provider Constraints
Each provider has finite capacity to distribute among compatible recipients. They prefer to allocate to recipients whose contributions they value most highly (proportional to recognition).

### Recipient Constraints
Each recipient has specific needs with finite capacity requirements. They prefer to receive from providers they trust/value most highly.

### Two-Sided Optimization
The system must simultaneously satisfy:
1. **Provider priorities**: Allocate proportionally to recognition of recipients
2. **Recipient preferences**: Receive from preferred providers
3. **Capacity constraints**: $\sum_j X_{ij} \leq C_i$ for all providers
4. **Need constraints**: $\sum_i X_{ij} \leq N_j$ for all recipients

This is a **constrained weighted allocation problem** that finds the allocation matrix minimizing deviation from both providers' priorities and recipients' preferences.

---

## Mathematical Properties

The allocation mechanism has several important properties:

### Proportional Preservation
If you express that Recipient A should receive twice as much as Recipient B (through recognition), the system allocates approximately twice as much capacity to A when feasible given constraints.

$$
\frac{X_{ij}}{X_{ik}} \approx \frac{P_{ij}}{P_{ik}}
$$

The proportional relationships you express are preserved in the final allocation.

### Least Biased Solution
Among all possible allocations satisfying the constraints, the system selects the one that introduces the least additional bias beyond what entities express. This is the entropy-maximizing (information-theoretically optimal) solution.

### Constraint Propagation
When constraints bind (e.g., a recipient reaches capacity), the effects propagate through the network. Capacity that cannot flow to a full recipient automatically redistributes to other compatible needs according to expressed preferences.

### Equilibrium Convergence
The system converges to a stable equilibrium where no entity can improve their allocation quality (measured by preference satisfaction) without degrading someone else's. This is a Pareto-efficient outcome.

---

## Dynamic Updates

The system continuously adapts to changing conditions:

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
System converges to stable equilibrium rapidly. 

### Stability
Once converged, allocations remain stable unless:
- Network state changes (new needs, capacity, or recognition)
- Resource specifications updated
- Participants join or leave
Upon a change, system immediately re-calculates allocations, dynamic convergence takes place.

### Optimality
At equilibrium:
- Resources distributed proportional to recognition weights
- All capacity and need constraints satisfied
- **Pareto Efficiency**: No allocation can be improved without violating a constraint or degrading another entity's preference satisfaction
- **Proportional Preservation**: Allocation ratios match recognition ratios where constraints allow

