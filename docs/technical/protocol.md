# Protocol Specification

This is a reference to the formal Free Association Protocol Specification.

## Full Specification

The complete protocol specification is available in the main repository:

**[PROTOCOL.md](../../PROTOCOL.md)**

## Quick Reference

### Core Requirements

Any implementation claiming Free Association conformance preserves:

**1. Recognition Foundation**
- Recognition weights sum to 100% per entity
- Recognition is non-transferable
- Recognition is dynamically adjustable
- Allocation proportional to recognition

**2. Two-Sided Optimization**
- Provider priorities: Allocate proportionally to recognition
- Recipient preferences: Prefer valued providers
- Capacity and need constraints respected
- Proportional preservation where feasible

**3. Mathematical Fairness Guarantees**
- Proportional allocation
- Constraint satisfaction
- Pareto efficiency
- Determinism

### Allocation Formula

**Proportional Allocation:**
```
Allocation_i = Capacity × (Recognition_i / Σ Recognition)

Subject to:
  Σ_j Allocation_j ≤ Capacity_Provider
  Σ_i Allocation_i ≤ Need_Recipient
```

**Two-Sided Optimization:**
The system finds the allocation matrix that minimizes deviation from both:
- Provider priorities (recognition weights)
- Recipient preferences (source preferences)

While satisfying all capacity and need constraints.

### Required Properties

Conformant implementations demonstrate:

✅ **Proportional Allocation:** Allocations proportional to recognition  
✅ **Constraint Satisfaction:** Respects capacity and need bounds  
✅ **Pareto Efficiency:** No improvement without violating constraints  
✅ **Determinism:** Same state → same allocations

### Protocol Violations

The following are NON-CONFORMANT:

❌ Making recognition transferable or tradeable  
❌ Breaking proportional allocation property  
❌ Violating capacity or need constraints  
❌ Breaking determinism property

## Version Information

**Current Version:** 1.0  
**Status:** Reference Standard  
**Last Updated:** November 2025

## Conformance Testing

Implementations pass test suite covering:
- Basic allocation scenarios
- Property verification
- Edge case handling

## Interoperability

Future versions will specify:
- Standard data format for recognition exchange
- Need/capacity declaration format
- Allocation result format

Current version: Implementation-specific approaches permitted.

## Changes and Evolution

**RFC Process:**
1. Submit proposal with `protocol-rfc` tag
2. Community discussion (minimum 2-4 weeks)
3. Mathematical proof of preserved properties
4. Reference implementation update
5. Version increment

**Version Semantics:**
- Major (X.0): Breaking changes to core formula
- Minor (1.X): Backwards-compatible additions
- Patch (1.0.X): Documentation only

## Resources

**Full Protocol Specification:** [PROTOCOL.md](../../PROTOCOL.md)

**Reference Implementation:** [github.com/interplaynetary/free-association](https://github.com/interplaynetary/free-association)

**Mathematical Foundations:** [mathematics.md](mathematics.md)

**Governance Process:** [../../GOVERNANCE.md](../../GOVERNANCE.md)

## Contact

**Protocol Questions:** info@openassociation.org

**Technical Issues:** [GitHub Issues](https://github.com/interplaynetary/free-association/issues)

**RFC Submissions:** [GitHub Issues with protocol-rfc tag](https://github.com/interplaynetary/free-association/issues/new)

---

**Note:** This page provides overview and quick reference. See the [full specification](../../PROTOCOL.md) for complete normative requirements.

