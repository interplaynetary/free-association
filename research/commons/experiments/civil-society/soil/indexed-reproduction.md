# Indexed Reproduction & Expansion

This document defines the core laws of **Indexed Reproduction** and **System Expansion**. It unifies the concepts of maintenance (restoration) and growth (expansion) into a single structural framework.

## I. The Fundamental Law of Productive Systems

In any productive system, long-term stability requires that what an agent extracts from the system (or the cost they bear) is returned to that same agent in proportion to what they contributed. This is a structural necessity for survival, not merely an ethical preference.

### 1. Agent State Evolution
For any agent $i$, their internal capacity or state $S_i(t)$ evolves as:

$$
S_i(t+1) = S_i(t) + R_i - X_i
$$

Where:
- $X_i$: Reproduction cost / Extraction / Loss borne by agent $i$
- $R_i$: Return / Restitution received by agent $i$

### 2. The Two Regimes

#### Regime A: Restoration (Maintenance)
To maintain the system in a steady state without depletion:

$$
R_i = X_i \implies S_i(t+1) = S_i(t)
$$

This is **Indexed Reproduction**. It ensures that the specific structure of the system is preserved. Averaging returns (where $\sum R_i = \sum X_i$ but $R_i \neq X_i$) leads to local depletion and structural collapse.

#### Regime B: Expansion (Growth)
Growth occurs when the return exceeds the cost of reproduction:

$$
R_i > X_i \implies S_i(t+1) > S_i(t)
$$

## II. Sources of Surplus

In a closed system, $R_i$ cannot exceed $X_i$ universally without an additional source. The **Gross Surplus** ($\Sigma$) is defined as:

$$
\Sigma = I + G - L
$$

Where:
- $I$: External Inflows (e.g., solar energy, new participants)
- $G$: Endogenous Gain (efficiency, learning, structure reduction of entropy)
- $L$: Irreversible Loss (dissipation)

If $\Sigma \le 0$, no expansion is possible.

## III. The Expansion Protocol

When $\Sigma > 0$, the surplus must be strictly partitioned before allocation to prevent consumption of the seed corn.

### 1. Separation of Surplus
$$
\Sigma = E + Y
$$

Defined by an expansion rate $\alpha$ ($0 < \alpha < 1$):
- **$E = \alpha \Sigma$ (Expansion Capacity/Investment):**
  - Not distributable.
  - Not owned by individual agents.
  - Reinvested to increase future $G$.
  - Formal Effect: $G(t+k) = G(t) + \phi(E)$ (where $\phi' > 0$).

- **$Y = (1-\alpha) \Sigma$ (Expansion Yield/Dividend):**
  - Distributable growth.
  - Allocated to agents.

### 2. Indexed Allocation of Yield
The yield $Y$ is distributed strictly in proportion to contribution $C$.

$$
w_i = \frac{C_i}{\sum_j C_j}
$$

$$
R_i^{\text{exp}} = w_i \cdot Y
$$

### 3. Total Return
The total return to an agent is the sum of their reproduction cost (guaranteed first) and their share of the yield:

$$
R_i = X_i + \frac{C_i}{\sum_j C_j} (1-\alpha)(I + G - L)
$$

## IV. The General Relation (System-Independent)

This framework generalizes to any productive system (biological, economic, computational).

**The Canonical Minimal Form:**

$$
\boxed{ \forall i:\; \frac{R_i - \text{shared\_gain}_i}{X_i} = 1 \;\;\land\;\; \sum_i R_i = \sum_i X_i + Y }
$$

Or more simply, the **One-Sentence Law**:

> A system endures only when every agent’s loss is locally compensated in proportion to the value it supplies, and global compensation equals global loss plus sustainable yield.

## V. Failure Modes

| Violation | Mechanism | Outcome |
| :--- | :--- | :--- |
| **$R_i^{\text{repro}} < X_i$** | Hidden Depletion | Capability erosion, burnout, soil death. |
| **Distribute $E$** | Short-termism | Eating the seed corn; future growth collapses. |
| **Fund $E$ from Repro** | Taxation of Survival | Agent collapse (starvation). |
| **Unindexed $Y$** | Free-riding | Parasitism; productive agents defect or die. |
| **$\alpha = 0$** | Stagnation | No investment in future capacity. |
| **$\alpha = 1$** | Starvation | No immediate incentive to participate. |

## VI. Why "Indexed"?

"Indexed" means that restitution is mapped (indexed) to the specific origin of the loss/contribution.

- **In Agriculture:** If a plot loses Nitrogen, it must get Nitrogen back, not just "biomass".
- **In Networks:** If a node burns CPU for the network, it must receive capacity/tokens, not just "thanks".

Matching the *type* and *location* of the return to the *type* and *location* of the cost is what makes the reproduction "indexed." Without this, the system suffers from **metabolic rifts**.
