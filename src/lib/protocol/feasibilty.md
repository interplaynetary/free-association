# Problem Space: Spatio-Temporal Optimization in Resource Allocation

## The Core Challenge
The current [DistributedIPF](file:///home/ruzgar/Programs/playnet/free-association/src/lib/protocol/solver.ts#42-70) solver operates on **Scalar Flow**. It asks: "How much *quantity* does Provider A give to Need B?"
*   **Input**: `Capacity = 10 hours`, `Need = 10 hours`.
*   **Output**: `Flow = 10 hours`.
*   **Result**: "Need Satisfied."

**The Reality Gap**:
1.  **Temporal Mismatch**: Provider A is free Monday. Need B is for Tuesday. Scalar solver says "Matched". Reality says "Failure".
2.  **Spatial Inefficiency**: Provider A is 100km away. Provider B is 1km away. Scalar solver sees them as equal (unless preferences are manually set). Reality prefers B.
3.  **Fragmentation**: Need 10 hours. Solver gives 10 providers × 1 hour each. Transaction costs explode.

## Dimension 1: Coverage (The "When")
We need to move from **Scalar Satisfaction** to **Spatio-Temporal Coverage**.
*   **Scalar**: `Quantity_Supplied >= Quantity_Needed`
*   **Temporal**: `Time_Supplied ∩ Time_Needed >= Duration_Needed`

**Solution: Precise Intersection**
We introduce [calculateAvailabilityIntersection(SlotA, SlotB)](file:///home/ruzgar/Programs/playnet/free-association/src/lib/protocol/match.ts#410-454).
*   Instead of `Flow = Min(Cap, Need)`, the flow limit becomes `Duration(Intersect(TimeA, TimeB))`.
*   If [Intersect](file:///home/ruzgar/Programs/playnet/free-association/src/lib/protocol/match.ts#410-454) is empty (disjoint times), Flow is strictly 0.
*   *This effectively "prunes" the bipartite graph where edges exist but are temporally invalid.*

## Dimension 2: Optimization (The "Where")
We want to minimize the **Space-Time Distance** to satisfaction.
*   **Space**: Physical travel distance.
*   **Time**: Delay until fulfillment (or deviation from preferred time).

**Solution: Distance Decay in Preference (`k_pr`)**
In the IPF model, $k_{pr}$ represents the "affinity" or "seed power" of a connection.
We insert a decay function:
$$ k_{pr}' = k_{pr} \times \frac{1}{1 + (\frac{\text{distance}}{d_{half}})^2} $$
*   **Closer providers** have higher $k_{pr}$.
*   **Result**: In the competitive equilibrium, flow naturally routes to the closest providers first, only spilling over to distant ones if local capacity is exhausted.

## Synthesis
By combining these two mechanisms, we transform the solver:
1.  **Intersection** enforces Constraints (Validity).
2.  **Decay** drives Optimization (Efficiency).

This moves the protocol from a "Commodity Exchange" (trading generic hours) to a "Context-Aware Service Discovery" (finding the *right* person at the *right* time/place).