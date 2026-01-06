# Protocol Behavior Specification: Priority-Based Allocation

This document outlines the expected behavioral properties of the algorithm under various constrained scenarios, derived from `protocol/docs/priority.md`.

## 1. Baseline Convergence

**Scenario**: Single Provider, Single compatible Need.
- **Given**: One Provider (Capacity: 100), One Need (Limit: 100).
- **Expectation**: Algorithm converges to 100% satisfaction (Allocation: 100).

## 2. Proportional Fairness (Provider-Side Priority)

**Scenario**: One Provider allocates to two competing Needs based on *their* own priority.
- **Given**:
    - Provider (Capacity: 100).
    - Need A (Limit: 100). Provider Priority to A: **High (0.8)**.
    - Need B (Limit: 100). Provider Priority to B: **Low (0.2)**.
- **Expectation**: Allocations split proportional to provider priority weights (approx 80 to A, 20 to B), as capacity is the constraining factor.

## 3. Proportional Fairness (Recipient-Side Preference)

**Scenario**: Two Providers compete for one Need, filtered by the *Recipient's* preference.
- **Given**:
    - Need (Limit: 100).
    - Provider A (Capacity: 100). Recipient Preference for A: **High (0.8)**.
    - Provider B (Capacity: 100). Recipient Preference for B: **Low (0.2)**.
- **Expectation**: Recipient fills their need by drawing proportionally from A and B based on preference (approx 80 from A, 20 from B).

## 4. Displacement ("Squeeze-In")

**Scenario**: A High-Priority Provider displaces a Low-Priority Provider from a full recipient.
- **Given**:
    - Need (Limit: 10). Currently fully served by Provider B.
    - Provider B (Low Priority/Preference). allocation = 10.
    - Provider A (High Priority/Preference) enters.
- **Expectation**:
    - Provider A "pushes" allocation into the fully saturated Need.
    - System detects overshoot and clamps.
    - Due to higher weight, Provider A retains more share during clamping.
    - **Result**: Provider A displaces Provider B (e.g., A gets 8, B gets 2).

## 5. Hidden Demand Discovery

**Scenario**: Discovery of unserved needs.
- **Given**:
    - Provider has Capacity.
    - Need exists but currently has 0 allocation (unserved).
- **Expectation**: The "Pull" mechanism identifies the negative deviation (shortage) and initiates flow to the unserved need, ensuring it isn't ignored just because current flow is zero.

## 6. Global Clamping (Overshoot Protection)

**Scenario**: Aggregate supply exceeds demand.
- **Given**:
    - Need (Limit: 100).
    - Provider A pushes 80.
    - Provider B pushes 80.
    - Total draft allocation: 160.
- **Expectation**: `enforceNeedLimits` scales both down proportionally to fit the limit of 100 (e.g., 50 each, assuming equal priority).

## 7. Multi-Dimensional Constraints

**Scenario**: Both Provider Capacity and Recipient Needs are acting as constraints simultaneously.
- **Given**: Complex topology.
- **Expectation**: The system reaches a Nash Equilibrium-like state where no entity can unilaterally improve their own priority satisfaction without violating a constraint or degrading a higher-weighted relationship.
