# Free Association Protocol Specification

**Version:** 1.0  
**Status:** Reference Standard  
**Last Updated:** November 2025

---

## Abstract
 
 Free Association is a coordination protocol enabling resource allocation through priority aligned capacity distribution. This specification defines the mathematical requirements for protocol-conformant implementations without prescribing implementation details.
 
 ---
 
 ## Core Principles (Non-Negotiable)
 
 Any implementation claiming Free Association conformance MUST preserve these properties:
 
 ### 1. Priority Alignment Foundation
 - Priority weights sum to 100% per entity
 - Reciprocal Alignment = min(A→B priority, B→A priority)
 - Priorities are non-transferable
 - Priorities are dynamically adjustable
 
 ### 2. Needs-Based Allocation
 - Allocations capped at declared need (no accumulation possible)
 - Remaining need = max(0, declared need - received)
 - Each resource type tracked independently
 
 ### 3. Mathematical Fairness Guarantees
 - **Contraction Property**: Total remaining needs decrease or stay constant
 - **Proportional Allocation**: Shares determined by priority weights, not need size
 - **Constrained Priority Phases**: Provider constraints (Phase 1) respected before source refinement (Phase 2)
 - **No Wealth Accumulation**: No formula enables Need(t+1) > Need(t) through receiving

---

## Mathematical Requirements

### Priority Weights
 
 For any entity E:
 
 ```
 ∀E: Σ(E→Others) = 100%
 Priority(E→E) ≥ 0  (self-prioritization permitted)
 Priorities are non-transferable
 ```
 
 ### Reciprocal Alignment
 
 ```
 Alignment(A, B) = min(Priority(A→B), Priority(B→A))
 ```
 
 This MUST be symmetric:
 ```
 Alignment(A, B) = Alignment(B, A)
 ```

### Allocation Formula (Core Requirement)
 
 The core allocation function `φ(Capacity, Needs, Weights)` MUST satisfy the **Constrained Weighted Allocation** model:
 
 **Phase 1: Weighted Provider Allocation (Mandatory)**
 
 For each provider P with capacity C and compatible needs N_1...N_k:
 
 1.  **Ideal Targets**: Calculate weighted targets based on provider priorities (w_i).
     ```
     Target_i = w_i × C  (where Σ w_i = 100%)
     ```
 
 2.  **Constraint Satisfaction**: Find allocations A_i minimizing distance to Targets subject to:
     -   `0 ≤ A_i ≤ Need_i` (Need constraint)
     -   `Σ A_i ≤ C` (Capacity constraint)
     -   If `max_natural_div` exists, A_i must be integer multiple of unit size.
 
     *Algorithm Effect:* If specific needs are less than their weighted share, the unused capacity MUST be redistributed to other needs that are unmet, up to their weighted share, before being considered "surplus".
 
 **Phase 2: Recipient Source Refinement (Mandatory)**
 
 3.  **Source Adjustment**: Recipients may shift received quantities between providers to match *their* preference weights (`w_recipient`), provided:
     -   Total received per recipient remains constant (`Σ A_ij` unchanged for recipient j).
     -   Total provided per provider remains constant (`Σ A_ij` unchanged for provider i).
     -   No provider's specific willingness limit is violated (if such hard limits exist).
 
 This formulation guarantees Pareto efficiency with respect to the constraints and weights.

### Update Law

```
RemainingNeed(r, t+1) = max(0, DeclaredNeed(r, t) - TotalReceived(r, t))
```

Where:
- DeclaredNeed can be updated by r at any time
- TotalReceived tracks cumulative allocation
- Update applies independently per resource type

---

## Required Properties (Must Be Provable)

Any conformant implementation MUST demonstrate:

### Property 1: Contraction
```
For any allocation A(r) applied to need r:
RemainingNeed(after) = max(0, Need(before) - A(r))
                     ≤ Need(before)
```
This holds unconditionally in every allocation round. Receiving resources always reduces remaining need.

### Property 2: No Accumulation
```
∀r, ∀t: Received(r, t) ≤ DeclaredNeed(r, t)
```
No entity can receive more than declared need per allocation round

### Property 3: Fairness
 ```
 ∀A, B: If Priority(P, A) = Priority(P, B)
 Then Share(A) = Share(B)
 ```
 Equal priority alignment yields equal proportional shares (before need cap)

### Property 4: Determinism
```
Same (Recognition, Needs, Capacities) → Same Allocations
```
Multiple independent computations yield identical results

---

## Protocol Violations (Non-Conformant)

The following are **explicit violations** of Free Association protocol:

### Accumulation Mechanisms
- Allowing Received > DeclaredNeed
- Enabling wealth/resource stockpiling
- Creating investment or profit mechanisms

### Priority Manipulation
 - Making priorities transferable or tradeable
 - Allowing priority inheritance
 - Enabling priority markets
 
 ### Allocation Distortions
 - Weighting by need size instead of priority for share calculation
 - Prioritizing non-reciprocal over reciprocal alignment
- Adding pricing or payment mechanisms
- Introducing central authority control over allocations

### Algorithm Modifications
- Changing mutual recognition formula from min(A→B, B→A)
- Removing needs cap on allocations
- Altering two-tier priority structure
- Breaking determinism property

---

## Implementation Requirements

### Minimum Viable Conformance

A conformant implementation MUST:

1. **Implement core allocation formula** as specified above
2. **Prove contraction property** mathematically or empirically
3. **Demonstrate no accumulation** through formula inspection
4. **Support deterministic computation** (same state → same result)
5. **Enable peer-to-peer operation** (no mandatory central server)

### Recommended Features

Implementations SHOULD:

- Support time/location/type slot matching
- Provide damping mechanisms for oscillation prevention
- Enable contribution tree structures for recognition calculation
- Implement causal consistency for distributed state
- Provide transparency/auditability of allocations

### Acceptable Variations

Implementations MAY:

- Use different data structures (trees, graphs, flat lists)
- Optimize computation algorithms (as long as results match)
- Add features beyond minimum spec (as long as core properties preserved)
- Support additional resource types or metadata
- Implement different network protocols

---

## Conformance Testing

### Test Suite Requirements

Conformant implementations must pass:

**Basic Allocation Tests:**
 - Equal priority alignment → equal shares (before need cap)
 - Higher reciprocal alignment → higher shares
 - Allocation never exceeds declared need
 - Provider constraints (Phase 1) respect all limits before refinement
 
 **Property Tests:**
 - Contraction: Needs decrease or stay constant
 - No accumulation: Received ≤ Need always
 - Determinism: Repeated computation yields same result
 - Symmetry: Alignment(A,B) = Alignment(B,A)

**Edge Cases:**
- Zero capacity handling
- Zero need handling
- Self-recognition handling
- Insufficient capacity scenarios

---

## Versioning and Evolution

### Version Semantics

**Major version (X.0)**: Breaking changes to allocation formula or core properties  
**Minor version (1.X)**: Backwards-compatible additions or clarifications  
**Patch version (1.0.X)**: Documentation fixes, no formula changes

### Protocol Change Process

1. **RFC Submission**: Proposed changes documented with rationale
2. **Mathematical Proof**: Demonstrate preserved properties
3. **Community Review**: Minimum 30 days for major changes
4. **Reference Implementation**: Update and test
5. **Version Increment**: Update specification version

Changes that violate core properties (Section 2) are **not** Free Association and must use different protocol name.

---

## Enforcement and Certification

### Self-Certification

Implementations may self-certify conformance by:
1. Publishing test suite results
2. Documenting which formula variations (if any) were used
3. Providing mathematical proofs of required properties
4. Making source code available for inspection (per AGPL-3.0)

### Community Verification

The Free Association community may:
- Review conformance claims
- Run independent tests
- Document non-conformant implementations
- Provide guidance on achieving conformance

### Non-Conformant Use

Implementations that:
- Fail conformance tests
- Violate core properties
- Cannot prove required guarantees

Should clearly state: "Based on Free Association but non-conformant" and document deviations.

---

## Interoperability

### Data Format Requirements (Future)

Conformant implementations should work toward:
- Standard recognition weight exchange format
- Standard need/capacity declaration format  
- Standard allocation result format

*Formal specification to be published in v1.1*

### Network Protocol (Implementation-Specific)

This specification does NOT mandate:
- Specific network protocol
- Specific data serialization
- Specific cryptographic approach
- Specific consensus mechanism

Implementations choose these based on their context.

---

## References

**Full Mathematical Documentation:**  
[github.com/interplaynetary/free-association/src/lib/protocol/README.md]

**Reference Implementation:**  
[github.com/interplaynetary/free-association]

**RFC Process:**  
[github.com/interplaynetary/free-association/GOVERNANCE.md]

---

## Summary: What Makes It Free Association?

A system is Free Association conformant if and only if:

✅ Reciprocal Alignment = min(A→B, B→A)  
 ✅ Allocations capped at declared need  
 ✅ Shares proportional to priority weights (not need size)  
 ✅ Constrained Priority Phases (Provider constraints → Source refinement)  
 ✅ Contraction property holds  
 ✅ No accumulation possible  
 ✅ Deterministic and peer-to-peer capable

Everything else is implementation detail.

---

**This specification is normative.**  
Implementations claiming Free Association conformance must satisfy these requirements.  
The reference implementation demonstrates one way to achieve conformance, not the only way.

---

_Protocol maintained by Free Association Development Team_  
_Contact: info@openassociation.org_  
_License: This specification is CC0 (public domain)_  
_Reference implementation: AGPL-3.0 with additional terms_