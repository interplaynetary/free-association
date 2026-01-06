# Protocol Behavior Specification: IPF-Based Allocation (Symmetric Scaling)

This document outlines the expected behavioral properties of the **Iterative Proportional Fitting (IPF)** algorithm under various constrained scenarios, derived from `protocol/docs/priority-ipf.md`. While the scenarios match the heuristic model, the underlying mechanisms (Matrix Scaling, Entropy Minimization) differ.

## 1. Baseline Convergence

**Scenario**: Single Provider, Single compatible Need.
- **Given**: One Provider (Capacity: 100), One Need (Limit: 100).
- **Expectation**: Algorithm converges to 100% satisfaction (Allocation: 100).
- **IPF Mechanism**: **Row & Column Scaling**. The single matrix entry $A_{1,1}$ is scaled up by Row Scaling to meet Capacity (100), and verified by Column Scaling to not exceed Need (100).

## 2. Proportional Fairness (Provider-Side Priority)

**Scenario**: One Provider allocates to two competing Needs based on *their* own priority.
- **Given**:
    - Provider (Capacity: 100).
    - Need A (Limit: 100). Provider Priority to A: **High (0.8)**.
    - Need B (Limit: 100). Provider Priority to B: **Low (0.2)**.
- **Expectation**: Allocations split proportional to seeded priority weights (approx 80 to A, 20 to B).
- **IPF Mechanism**: **Seed Matrix Preservation**. The Seed Matrix is initialized with values proportional to priority (e.g., $S_{1,A} \approx 0.8, S_{1,B} \approx 0.2$). Row scaling multiplies both by the same factor to reach Capacity 100, preserving the $4:1$ ratio.

## 3. Proportional Fairness (Recipient-Side Preference)

**Scenario**: Two Providers compete for one Need, filtered by the *Recipient's* preference.
- **Given**:
    - Need (Limit: 100).
    - Provider A (Capacity: 100). Recipient Preference for A: **High (0.8)**.
    - Provider B (Capacity: 100). Recipient Preference for B: **Low (0.2)**.
- **Expectation**: Recipient fills their need by drawing proportionally from A and B based on preference.
- **IPF Mechanism**: **Symmetric Seed Construction**. The Seed Matrix incorporates recipient preference (controlled by $\gamma$). Users A and B have seed values proportional to preference. Column Scaling limits the total to 100, scaling A and B down equally, thus preserving the preference ratio.

## 4. Displacement (Hydraulic Equilibrium)

**Scenario**: A High-Priority Provider displaces a Low-Priority Provider from a full recipient.
- **Given**:
    - Need (Limit: 10). Currently fully served by Provider B.
    - Provider B (Low Priority).
    - Provider A (High Priority) enters.
- **Expectation**: Provider A displaces Provider B (e.g., A gets 8, B gets 2).
- **IPF Mechanism**: **Entropy Minimization**. The solver seeks a matrix closest to the Seed Matrix (where A has a large value and B has a small value) that satisfies the Need limit.
    - **Step 1**: Both A and B "push" (Row Scaling).
    - **Step 2**: The Need is over-supplied (e.g., 20 offered for 10 needed).
    - **Step 3**: **Column Scaling** multiplies both offers by $0.5$.
    - **Result**: Since A started with a larger "Push" (Seed), A retains a larger share of the constrained resource. B is forced to "flow" their capacity elsewhere in subsequent iterations.

## 5. Hidden Demand Discovery (Epsilon Activation)

**Scenario**: Discovery of unserved needs.
- **Given**:
    - Provider has Capacity.
    - Need exists but currently has 0 allocation (unserved).
- **Expectation**: Flow initiates to the unserved need despite zero initial history.
- **IPF Mechanism**: **Epsilon ($\epsilon$) Potentials**. The Seed Matrix adds a small $\epsilon$ to all compatible slots. Even if Priority/Preference is technically 0, the seed is non-zero ($0 + \epsilon$). If constraints block other paths, Row Scaling will amplify this tiny $\epsilon$ until it becomes significant enough to absorb the provider's capacity.

## 6. Global Clamping (Distributed Constraint Enforcement)

**Scenario**: Aggregate supply exceeds demand.
- **Given**:
    - Need (Limit: 100).
    - Provider A pushes 80.
    - Provider B pushes 80.
- **Expectation**: Both scaled down to sum to 100.
- **IPF Mechanism**: **Distributed Clamping**.
    - In **Centralized/Simulated IPF**: Iterative Column Scaling reduces the column sum to 100.
    - In **Distributed IPF**: We apply explicit output clamping: $Allocation = \min(Proposal, Need \times \frac{Proposal}{\sum Proposals})$. This ensures strict adherence to limits between async updates.

## 7. Multi-Dimensional Constraints (Matrix Convergence)

**Scenario**: Both Provider Capacity and Recipient Needs are acting as constraints simultaneously.
- **Given**: Complex topology.
- **Expectation**: The system reaches a stable equilibrium.
- **IPF Mechanism**: **Unique Matrix Convergence**. Mathematical guarantee that (given sufficient connectivity) the iterative process converges to a unique matrix $A$ that satisfies $\sum_{row} = Capacity$ and $\sum_{col} = Need$ while minimizing information loss from the preferences/priorities expressed in the Seed.
