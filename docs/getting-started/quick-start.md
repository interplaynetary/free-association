# Quick Start Guide

## For Decision-Makers (5 minutes)

### The Problem
Traditional resource coordination operates slower than modern challenges require. Crisis response takes months when days would serve better.

### The Solution
Free Association enables automatic resource allocation proportional to recognition—no bureaucracy, no gatekeepers.

### Key Benefits
- **<48 hours** from need to commitment (vs. 90+ days typical)
- **>95%** resources to mission (vs. ~70% after overhead)
- **Real-time** adaptation as circumstances evolve

[Explore organizational pilots →](../implementation/organizations.md)

---

## For Technical Teams (5 minutes)

### System Architecture
- Peer-to-peer network
- Published data: recognition weights, capacity, needs
- Distributed calculation
- Proportional allocation algorithm with constraint satisfaction

### Core Algorithm
```
Recognition → Proportional Allocation → Constraint Satisfaction
Dynamic equilibrium: recomputes ~100-200ms per state change
```

### Implementation
- Open source (AGPL-3.0)
- Browser-based reference implementation
- Protocol specification available

[Development setup →](../implementation/developers.md)

---

## For Researchers (5 minutes)

### Mathematical Properties
- **Proportional allocation**: Allocations proportional to recognition
- **Two-sided optimization**: Satisfies both provider priorities and recipient preferences
- **Dynamic equilibrium**: Maintains instantaneous optimality as network evolves
- **Constraint satisfaction**: Respects capacity and need bounds

### Key Formula
```
Allocation ∝ Recognition
A_i ∝ R_i / Σ R (proportional to recognition share)
Subject to: Σ A_i ≤ Capacity and A_i ≤ Need_i
```

[Mathematical foundations →](../reference/mathematics.md)

---

## Three-Minute Concept

### 1. Recognition (Who contributes to your goals?)
Each entity allocates 100% recognition among contributors based on their contribution to goal realization.

### 2. Capacity & Needs (What can you offer? What do you need?)
Entities declare available resources and requirements.

### 3. Automatic Allocation
System calculates optimal distribution through two-sided optimization:
- **Provider side**: Allocate proportional to recognition of recipients
- **Recipient side**: Prefer sources they value most
- **Constraints**: Respect capacity limits and need bounds
- **Updates**: Real-time recalculation as network evolves

### Result
Resources flow automatically to recognized partners based on actual need and contribution—no meetings, no applications, no bureaucracy.

---

## Next Steps by Role

**Organizational Leaders:**
- [See use cases](../use-cases/README.md)
- [Join pilot program](../implementation/organizations.md)
- [Contact coalition team](../project/contact.md)

**Technical Contributors:**
- [Development setup](../implementation/developers.md)
- [Review protocol](../reference/protocol-spec.md)
- [Explore codebase](https://github.com/interplaynetary/free-association)

**Researchers:**
- [Mathematical foundations](../reference/mathematics.md)
- [Recognition](../concepts/recognition.md)
- [Allocation](../concepts/allocation.md)

