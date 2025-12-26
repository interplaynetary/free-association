# The Allocation Algorithm

## Constrained Weighted Allocation

Resources are distributed through a rigorous mathematical process that balances:
1.  **Provider Priorities**: "I want to give X% to you."
2.  **Recipient Preferences**: "I want Y% of my help to come from you."
3.  **Hard Constraints**: Capacity limits and Declared Needs.

### Phase 1: Symmetric Intent (The Seed)
Instead of distinct "Push" and "Pull" phases, we construct a single **Hypothesis Matrix** representing the "pure intent" of the network before physical constraints are applied.

$Seed = (Priority_{provider} + \epsilon) \times (Preference_{recipient} + \epsilon)^\gamma$

*   **Constructive Intersection**: A connection is strong only if **both** the provider wants to give and the recipient wants to receive.
*   **Hidden Demand**: The $\epsilon$ term allows the system to discover latent connections that aren't currently active but could be valuable.

### Phase 2: Entropic Equilibrium
The system actively molds this Seed Matrix to fit the physical constraints of reality (Capacity and Need). This is not done by ad-hoc adjustment, but by finding the **Entropic Equilibrium**—the single unique state that minimizes the information divergence from the Seed Matrix.

**Goal**: Preserve the *ratios* of the Seed Matrix as much as physically possible.

---

## Allocation Process

### Step 1: Filter Compatible Resources
Match resource specifications:
- Time windows overlap
- Geographic constraints satisfied
- Resource types compatible (Type IDs match)

### Step 2: Construct Seed
Build the Symmetric Intent matrix for all compatible connections. This captures the "ideal world" distribution of resources if there were no limits.

### Step 3: Iterative Proportional Fitting (IPF)
The algorithm iteratively scales the matrix to satisfy constraints:
- **Row Scaling (Provider Force)**: Scale allocations to exactly match valid provider capacity.
- **Column Scaling (Recipient Clamp)**: Scale allocations down if they exceed recipient need.
- **Hydraulic Displacement**: High-priority providers naturally displace low-priority ones through this pressure-balancing process.

### Step 4: Convergence
The cycle repeats until the system reaches a stable state (typically 10-20 iterations).

### Step 5: Final Settlement
The result is a unique, stable equilibrium where:
1.  **No Need Exceeded**: Allocations $\le$ Declared Need.
2.  **No Capacity Exceeded**: Total Allocations $\le$ Capacity.
3.  **Optimal Alignment**: The distribution represents the best possible compromise between all competing priorities.

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
System converges to stable equilibrium rapidly. Each round takes 100-200ms.

### Stability
Once converged, allocations remain stable unless:
- Network state changes (new needs, capacity, or priorities)
- Resource specifications updated
- Participants join or leave

### Optimality
At equilibrium:
- All needs met if sufficient capacity exists
- Resources distributed proportional to priority weights
- No entity receives beyond declared needs
- **Pareto Efficiency**: No allocation can be improved without violating a priority constraint.
