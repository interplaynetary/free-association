# Protocol Specification

This is a reference to the formal Free Association Protocol Specification.

## Full Specification

The complete protocol specification is available in the main repository:

**[PROTOCOL.md](../../PROTOCOL.md)**

## Quick Reference

### Core Requirements

Any implementation claiming Free Association conformance MUST preserve:

**1. Mutual Recognition Foundation**
- Recognition weights sum to 100% per entity
- Mutual recognition = min(A→B, B→A)
- Recognition is non-transferable
- Recognition is dynamically adjustable

**2. Needs-Based Allocation**
- Allocations capped at declared need
- Remaining need = max(0, declared - received)
- Each resource type tracked independently

**3. Mathematical Fairness Guarantees**
- Contraction property
- Proportional allocation
- Two-tier priority
- No accumulation possible

### Allocation Formula

**Tier 1 (Mutual Recognition):**
```
Share(r) = MR(P, r) / Σ MR(P, All_Mutual_Recipients)
RawAllocation(r) = Capacity × Share(r)
FinalAllocation(r) = min(RawAllocation(r), Need(r))
```

**Tier 2 (Unilateral Recognition):**
```
RemainingCapacity = Capacity - Σ FinalAllocation(Tier1)
Share(r) = Recognition(P→r) / Σ Recognition(P→All_Unilateral)
RawAllocation(r) = RemainingCapacity × Share(r)
FinalAllocation(r) = min(RawAllocation(r), Need(r))
```

### Required Properties

Conformant implementations must demonstrate:

✅ **Contraction:** Total needs decrease or stay constant  
✅ **No Accumulation:** Received ≤ Declared Need always  
✅ **Fairness:** Equal mutual recognition → equal shares  
✅ **Determinism:** Same state → same allocations

### Protocol Violations

The following are NON-CONFORMANT:

❌ Allowing accumulation beyond declared need  
❌ Making recognition transferable or tradeable  
❌ Weighting by need size instead of recognition  
❌ Prioritizing unilateral over mutual recognition  
❌ Breaking determinism property

## Version Information

**Current Version:** 1.0  
**Status:** Reference Standard  
**Last Updated:** November 2025

## Conformance Testing

Implementations should pass test suite covering:
- Basic allocation scenarios
- Property verification
- Edge case handling
- Convergence behavior

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

