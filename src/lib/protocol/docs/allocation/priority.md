# Priority-Based Allocation: Two-Sided Constrained Optimization

## Core Principle
**The protocol implements a Two-Sided Matching mechanism that simultaneously optimizes:**

1.  **Provider Priorities**: Providers allocate capacity to the needs they value most.
2.  **Recipient Preferences**: Recipients compose their total supply from the providers they trust/value most.

This equilibrium is achieved through a **Constrained Weighted Allocation** algorithm that minimizes the "Source Deviation" (the difference between actual and ideal allocation mixes).

## Key Difference from Abstract Model
- **NOT**: Static "filling buckets".
- **YES**: Dynamic vector optimization.
- **Key Feature**: **Displacement**. A high-priority provider can displace a low-priority provider from a full recipient, ensuring resources always flow to the highest-alignment matches.

## The Algorithm: Iterative Deviational Optimization

The system resolves tensions (e.g., Provider A wants Recipient X, but X prefers Provider B) via an iterative process where both sides exert "pressure" on the allocation matrix.

### Phase 1: Provider Constraints (The "Push")
Initially, providers "push" capacity primarily based on **their own priorities**.
- Allocations are initialized proportional to Provider Priority.
- *Goal*: Ensure providers allocate to their highest-weighted compatible needs.

### Phase 2: Recipient Refinement (The "Pull" & "Filter")
The system calculates a **Total Deviation** metric that combines both perspectives.

#### The Adjustment Mechanism
1.  **Retraction**: If a recipient is receiving too much from a low-preference provider, the system detects a positive deviation and reduces that allocation.
2.  **Expansion (Hidden Demand)**: If a recipient is receiving too little from a high-preference provider (even if current allocation is 0), the system detects a negative deviation and increases that allocation.
3.  **Overshoot**: High-preference providers are allowed to temporarily "overshoot" a recipient's need limit during the adjustment phase.
4.  **Global Clamping**: At the end of each iteration, `enforceNeedLimits` proportionally scales down all providers if a recipient is over-allocated.

**This combination allows "Squeeze-In":**
*   Provider A (High Priority) pushes allocation to Recipient R (who is full).
*   R becomes temporarily over-allocated.
*   "Clamping" scales everyone down.
*   Provider B (Low Priority) loses share.
*   *Result*: Provider A displaces Provider B.

## Priority Distribution Model

### Provider Side (Availability Slot)
For each `AvailabilitySlot`:
1.  Find all compatible `NeedSlot`s.
2.  Distribute 100% weight among **compatible recipient slots**.
3.  **Hidden Demand Discovery**: Even if a Need is currently unserved (0 allocation), it is considered in the ideal distribution.

### Recipient Side (Need Slot)
For each `NeedSlot`:
1.  Find all compatible `AvailabilitySlot`s.
2.  Distribute 100% weight among **compatible provider slots**.
3.  Weight guides the "ideal source mix".

## Mathematical Formulation

For a system with Providers $P$ and Recipients $R$:

We seek the allocation matrix $A$ that minimizes the global energy function:
$$ E(A) = \alpha \sum_{p} D_{provider}(p, A) + (1-\alpha) \sum_{r} D_{recipient}(r, A) $$

Subject to:
1.  **Capacity Constraints**: $\sum_r A_{pr} \le C_p$
2.  **Need Constraints**: $\sum_p A_{pr} \le N_r$ (Enforced via Clamping)

Where $D_{provider}$ measures deviation from provider's priority weights, and $D_{recipient}$ measures deviation from recipient's source preferences.

## Key Properties

### 1. Proportional Fairness
Allocations approximate provider weights when feasible given constraints.

### 2. Dynamic Displacement
High-priority providers can displace low-priority providers if space is scarce. The system does not lock in "first movers".

### 3. Hidden Demand Resolution
Providers correctly identify and reallocate to high-priority needs that were initially unserved, fixing the "blindness" of pure greedy algorithms.

### 4. Pareto Efficiency
No one can improve their allocation quality without degrading someone else's priority satisfaction.

## Integration with match.ts

The priority allocation algorithm **depends on** `match.ts` for:
1.  **Compatibility Checking**: `slotsCompatible(capacitySlot, needSlot)`
    - Type matching, Time overlap, Location compatibility
2.  **Spatial/Temporal Indexing**: `getTimeBucketKey(slot)`, `getLocationBucketKey(slot)`