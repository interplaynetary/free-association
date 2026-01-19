# Core Concepts

This section covers the fundamental concepts of Free Association. Each concept can be **published** (declared by participants) or **derived** (computed from network data), depending on your coordination needs.

## The Concepts

**[Identity](identity.md)** - Who you are
- Verifiable Credentials
- Portable, offline-verifiable proofs
- Self-sovereign identity

**[Resources](resources.md)** - What you have and need
- Capacities (what you can offer)
- Needs (what you require)
- Constraints (time, location, type)

**[Recognition](recognition.md)** - Acknowledging contribution
- Publishing recognition weights (100% budget)
- Deriving alignment metrics (True vs False)
- Contribution trees and priorities

**[Allocation](allocation.md)** - Distributing resources
- Publishing allocation decisions
- Deriving optimal allocation (IPF algorithm)
- Proportional fairness

## Publishing vs Deriving

Many concepts can be approached in two ways:

**Publishing** - You declare it directly:
- "I recognize Partner A at 30%"
- "I allocate $50K to Project B"
- "I am a member of Coalition C"

**Deriving** - The network computes it:
- "Based on network behavior, alignment with Partner A is 85%"
- "Optimal allocation to Project B is $47K given all constraints"
- "Based on recognition patterns, you're effectively part of Coalition C"

The choice depends on your trust model, coordination needs, and desired level of automation.

## Further Reading

- [Architecture](../architecture/distributed-architecture.md) - How the system works
- [Implementation](../implementation/organizations.md) - How to use it
- [Reference](../reference/mathematics.md) - Formal specifications
