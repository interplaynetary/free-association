# Free Association Protocol Specification

**Version:** 1.0 (Draft)  
**Status:** Working Draft  
**Last Updated:** January 2026

---

## 1. Abstract

Free Association is a coordination protocol enabling **Priority-Aligned Capacity Distribution**. It operates by strictly separating:
1.  **Publishing**: Participants sovereignty declaring their state (what they have, need, and prioritize).
2.  **Derivation**: The network collectively inferring operational state (Recognition and Allocation) from these published claims.

---

## 2. Publishing ("What Is")

*The Foundation: Sovereign Data*

Participants publish signed statements about their local reality. These are the inputs to the system.

### 2.1 Identity & Resources
- **Identities**: Cryptographic keys representing agents.
- **Resources**:
    - **Needs**: Declared requirements (What I need).
    - **Capacities**: Available capacity (What I have).
    - **Slots**: Discrete units defining Time, Location, and Type for needs/capacities.

### 2.2 Priorities (Recognition Claims)
- **Priority Weights**: A participant publishes how they value others ($\sum w_i = 100\%$).
- **Claims**: "I recognize Agent B for contributing to Scope X."
- These are raw signals of value, published directly by the sovereign agent.

---

## 3. Derivation ("What We Infer")

*The Process: Collective Intelligence*

The network applies deterministic logic to Validated Published Data to produce Derived State.

### 3.1 Derived Recognition
*Acknowledgement of who/what contributes*

From the raw graph of published Priority Claims, the network derives the **Recognition State**:
- **Traversals**: Calculating indirect recognition through the web of trust.
- **Scopes**: Resolving which agents are trusted for specific domains (e.g., "Food Safety").
- **Reciprocal Alignment**: Deriving the mutual intersection of priorities between two agents ($\min(A \to B, B \to A)$).

### 3.2 Derived Allocation
*How we divide our capacities*

From **Resources** (Publishing) and **Derived Recognition** (Derivation Step 1), the network derives the **Allocation State**:
- **The Solver**: A deterministic function (Reference: Iterative Proportional Fitting) that maps Capacity Slots to Need Slots.
- **Input**: Capacity Matrix + Need Vector + Recognition State.
- **Output**: A set of `SlotAllocationRecords`.

---

*Protocol maintained by the Free Association Coalition.*