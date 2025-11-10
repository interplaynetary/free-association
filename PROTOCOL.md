# Free Association Protocol Specification

**Version:** 1.0  
**Status:** Reference Standard  
**Last Updated:** November 2025

---

## Abstract

Free Association is a coordination protocol enabling resource allocation through mutual recognition. This specification defines the mathematical requirements for protocol-conformant implementations without prescribing implementation details.

---

## Core Principles (Non-Negotiable)

Any implementation claiming Free Association conformance MUST preserve these properties:

### 1. Mutual Recognition Foundation
- Recognition weights sum to 100% per entity
- Mutual recognition = min(A→B recognition, B→A recognition)
- Recognition is non-transferable
- Recognition is dynamically adjustable

### 2. Needs-Based Allocation
- Allocations capped at declared need (no accumulation possible)
- Remaining need = max(0, declared need - received)
- Each resource type tracked independently

### 3. Mathematical Fairness Guarantees
- **Contraction Property**: Total remaining needs decrease or stay constant
- **Proportional Allocation**: Shares determined by mutual recognition ratios, not need size
- **Two-Tier Priority**: Mutual recognition (Tier 1) before one-way recognition (Tier 2)
- **No Wealth Accumulation**: No formula enables Need(t+1) > Need(t) through receiving

---

## Mathematical Requirements

### Recognition Weights

For any entity E:

```
∀E: Σ(E→Others) = 100%
Recognition(E→E) ≥ 0  (self-recognition permitted)
Recognition is non-transferable
```

### Mutual Recognition

```
MutualRecognition(A, B) = min(Recognition(A→B), Recognition(B→A))
```

This MUST be symmetric:
```
MutualRecognition(A, B) = MutualRecognition(B, A)
```

### Allocation Formula (Core Requirement)

For provider P allocating capacity C to recipients R:

**Tier 1 (Mutual Recognition):**

```
Filter: R_mutual = {r ∈ R | MutualRecognition(P, r) > 0 AND r needs compatible resource}

For each r ∈ R_mutual:
  Share(r) = MutualRecognition(P, r) / Σ(MutualRecognition(P, R_mutual))
  RawAllocation(r) = C × Share(r)
  FinalAllocation(r) = min(RawAllocation(r), Need(r))
```

**Tier 2 (One-Way Recognition):**

```
RemainingCapacity = C - Σ(FinalAllocation(R_mutual))

Filter: R_oneway = {r ∈ R | Recognition(P→r) > 0 AND r ∉ R_mutual AND r needs compatible resource}

For each r ∈ R_oneway:
  Share(r) = Recognition(P→r) / Σ(Recognition(P→R_oneway))
  RawAllocation(r) = RemainingCapacity × Share(r)
  FinalAllocation(r) = min(RawAllocation(r), Need(r))
```

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
Σ(RemainingNeeds(t+1)) ≤ Σ(RemainingNeeds(t))
```
Under assumption: No arbitrary need declaration increases

### Property 2: No Accumulation
```
∀r, ∀t: Received(r, t) ≤ DeclaredNeed(r, t)
```
No entity can receive more than declared need per allocation round

### Property 3: Fairness
```
∀A, B: If MutualRecognition(P, A) = MutualRecognition(P, B)
Then Share(A) = Share(B)
```
Equal mutual recognition yields equal proportional shares (before need cap)

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

### Recognition Manipulation
- Making recognition transferable or tradeable
- Allowing recognition inheritance
- Enabling recognition markets

### Allocation Distortions
- Weighting by need size instead of mutual recognition for share calculation
- Prioritizing non-mutual over mutual recognition
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
- Equal mutual recognition → equal shares (before need cap)
- Higher mutual recognition → higher shares
- Allocation never exceeds declared need
- Tier 1 allocates before Tier 2

**Property Tests:**
- Contraction: Needs decrease or stay constant
- No accumulation: Received ≤ Need always
- Determinism: Repeated computation yields same result
- Symmetry: MutualRecognition(A,B) = MutualRecognition(B,A)

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

✅ Mutual recognition = min(A→B, B→A)  
✅ Allocations capped at declared need  
✅ Shares proportional to mutual recognition (not need size)  
✅ Two-tier priority (mutual before one-way)  
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