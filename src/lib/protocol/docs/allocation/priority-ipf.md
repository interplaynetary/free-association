# IPF-Based Allocation: Symmetric Scaling

> [!NOTE]
> This document describes the **Iterative Proportional Fitting (IPF)** implementation of the allocation protocol (`allocation-ipf.ts`), which offers a rigorous mathematical alternative to the heuristic-based process described in `priority.md`.

## Core Principle
**The protocol implements a Hybrid Symmetric/Asymmetric mechanism that simultaneously optimizes:**

1.  **Provider Priorities**: Providers "push" capacity to preferred needs.
2.  **Recipient Preferences**: Recipients "pull" resources from preferred providers.

This equilibrium is achieved through **Biproportional Matrix Scaling** (Sinkhorn-Knopp algorithm) on a constructed **Intent Matrix**.

## The Algorithm: Weighted Iterative Proportional Fitting (W-IPF)

Instead of heuristic "adjustments" and "clamping", IPF solves the problem by iteratively projecting a "Seed Matrix" onto the row (capacity) and column (need) constraints until convergence.

### 1. The Seed Matrix (Symmetric Intent)
We first construct a matrix representing the ideal distribution if no constraints existed.

$$ Seed_{pr} = (Priority_{pr} + \epsilon) \times (Preference_{pr} + \epsilon)^\gamma $$

*   **Provider Priority ($Priority_{pr}$)**: The Provider's intent (Push).
*   **Recipient Preference ($Preference_{pr}$)**: The Recipient's intent (Pull).
*   **Gamma ($\gamma$)**: A tuning factor ($0 \le \gamma \le 1$) controlling Recipient influence.
    *   $\gamma=0$: Pure Provider Push (Asymmetric).
    *   $\gamma=1$: Fully Symmetric.
*   **Epsilon ($\epsilon$)**: A tiny "potential" value added to compatible connections. This solves the "Hidden Demand" problem by ensuring even 0-priority slots have a non-zero probability of activation if constraints force flow there.

### 2. The Loop (Constraints)
The algorithm alternates between two simple operations:

#### A. Row Scaling (Provider Force)
$$ Matrix_{pr} \leftarrow Matrix_{pr} \times \frac{Capacity_p}{\sum_r Matrix_{pr}} $$
Providers scale their row to match their full Capacity.
*   *Interpretation*: "I have 100 units. My current distribution only sums to 80. Everyone gets scaled up by 1.25x."

#### B. Column Scaling (Recipient Clamp)
$$ Matrix_{pr} \leftarrow Matrix_{pr} \times \min\left(1, \frac{Need_r}{\sum_p Matrix_{pr}}\right) $$
Recipients scale their column to match their Need **only if** they are over-supplied.
*   *Interpretation*: "I need 10. The providers are pushing 15. Everyone gets scaled down by 0.66x."

### 3. Convergence
These steps repeat. Mathematical theory guarantees this process converges to a unique matrix that:
1.  Respects all constraints.
2.  Is "closest" (in information-theoretic terms) to the Seed Matrix.

> [!IMPORTANT]
> **Distributed Implementation Note**: In the distributed IPF implementation (`allocation-ipf-distributed.ts`), proposals are computed directly from scaling factors as `K_pr × x_p × y_r` without iterating through the full Sinkhorn loop. Since providers and recipients update their scaling factors independently and asynchronously, the product may temporarily exceed the recipient's need before convergence. To ensure the need constraint is always respected, final allocations are clamped:
> ```
> allocation = min(K_pr × x_p × y_r, Need_r)
> ```
> The centralized algorithm (`allocation-ipf.ts`) enforces this constraint through iterative column scaling, which scales down all allocations when a recipient's column sum exceeds their need. Both approaches ensure allocations never exceed needs, but the distributed version requires explicit clamping because it computes proposals from potentially stale scaling factors rather than iterating to convergence.

## Key Properties

### 1. Auto-Displacement (Hydraulics)
IPF naturally handles displacement.
*   If High-Priority Provider $A$ pushes to Full Recipient $R$:
*   $R$'s column scaling forces $A$ (and everyone else) down.
*   $A$ now has "loose capacity" (row sum < capacity).
*   Next Row Scaling step, $A$ pushes harder to other needs.
*   This pressure propagates through the network like hydraulic fluid finding the path of least resistance.

### 2. Proportional Fairness
Because scaling is multiplicative, ratios are preserved. If $A$ was preferred 2x more than $B$ in the Seed, $A$ retains roughly 2x the allocation of $B$ in the final result (subject to constraints).

### 3. Hidden Demand Resolution
The $\epsilon$ term ensures that if "Plan A" (high priority) is blocked, the algorithm can "wake up" "Plan B" (zero priority compatible matches) because $0+\epsilon$ can be scaled up to a significant number if necessary.

### 4. Seed Values and Expected Distribution

The seed matrix determines the **proportional distribution** of allocations. Understanding this relationship is critical for predicting outcomes.

#### How Seed Values Translate to Allocations

Given the seed formula:
$$ Seed_{pr} = (Priority_{pr} + \epsilon) \times (Preference_{pr} + \epsilon)^\gamma $$

**Key Insight**: When multiple providers compete for the same recipient, their **relative seed values** determine their **relative allocations** (subject to capacity constraints).

#### Concrete Example

**Scenario**: Two providers competing for one recipient's $100 need:

| Provider | Capacity | Priority (recognition of recipient) | Seed Value (γ=0.5, ε≈0) |
|----------|----------|-------------------------------------|-------------------------|
| Provider A (Self) | $100 | 0.37 (37% recognition) | 0.37 × 1^0.5 = 0.37 |
| Provider B (Other) | $100 | 0.15 (15% recognition) | 0.15 × 1^0.5 = 0.15 |

**Expected Allocation Ratio**: 0.37 : 0.15 = **2.47 : 1**

If both have sufficient capacity:
- Provider A should provide: $100 × (0.37 / 0.52) = **$71.15**
- Provider B should provide: $100 × (0.15 / 0.52) = **$28.85**

**Important**: The priority values (0.37, 0.15) are typically derived from:
- **Global recognition weights** (`global_recognition_weights[recipientPubkey]`)
- **Slot-specific priorities** (`priority_distribution[recipientPubkey]`)

These values should be **normalized** (sum to 1.0) within the provider's commitment to represent their relative valuation of different recipients.

## Mathematical Formulation

We seek the matrix $A$ that minimizes the Kullback-Leibler divergence from the Seed Matrix $S$:

$$ \min \sum_{pr} A_{pr} \ln \left( \frac{A_{pr}}{S_{pr}} \right) $$

Subject to:
1.  $\sum_r A_{pr} \le C_p$
2.  $\sum_p A_{pr} \le N_r$

This is the entropy-minimizing solution—the "most unbiased" way to satisfy constraints given the expressed preferences.

## Comparison to `allocation-local.ts`

| Feature | Heuristic (`allocation-local.ts`) | Strict IPF (`allocation-ipf.ts`) |
| :--- | :--- | :--- |
| **Logic** | Iterative Gradient Adjustment | Alternating Matrix Scaling |
| **Stability** | Approximate (depends on step size) | Exact (convergent) |
| **Displacement** | Explicit "Squeeze-In" Logic | Emergent Hydraulic Property |
| **Symmetry** | Operational Asymmetry (Push then Pull) | Configurable Symmetry ($\gamma$) |
| **Performance** | Adjustable speed | $O(N^2)$ per iter, very fast |
