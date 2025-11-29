# Quick Start Guide

## For Decision-Makers (5 minutes)

### The Problem
Traditional resource coordination operates slower than modern challenges require. Crisis response takes months when days would serve better.

### The Solution
Free Association enables automatic resource allocation based on mutual recognition—no bureaucracy, no gatekeepers.

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
- Need-capped allocation algorithm (prevents accumulation)

### Core Algorithm
```
Recognition → Mutual Recognition → Proportional Share → Allocation (capped at need)
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
- **Need-capped allocation**: Allocations never exceed declared needs (prevents accumulation)
- **Proportional fairness**: Strict proportionality to mutual recognition
- **Dynamic equilibrium**: Maintains instantaneous optimality as network evolves
- **Contraction guarantee**: Receiving resources always reduces remaining need (unconditional)

### Key Formula
```
MR(A, B) = min(Recognition_A_gives_B, Recognition_B_gives_A)
Share(R, P) = MR(R, P) / Σ MR(P, All_Recipients)
Allocation(R, P) = min(Capacity_P × Share(R, P), Need_R)
```

[Mathematical foundations →](../technical/mathematics.md)

---

## Three-Minute Concept

### 1. Recognition (Who contributes to your goals?)
Each entity allocates 100% recognition among contributors.

### 2. Mutual Recognition (Bidirectional acknowledgment)
Takes minimum of reciprocal recognition percentages.

### 3. Capacity & Needs (What can you offer? What do you need?)
Entities declare available resources and requirements.

### 4. Automatic Allocation
System calculates optimal distribution:
- Priority to mutual recognition pairs
- Proportional to recognition strength
- Capped at declared needs
- Updates in real-time

### Result
Resources flow automatically to mission-aligned partners based on actual need and mutual contribution—no meetings, no applications, no bureaucracy.

---

## Next Steps by Role

**Organizational Leaders:**
- [See use cases](../use-cases/README.md)
- [Join pilot program](../implementation/organizations.md)
- [Contact coalition team](../project/contact.md)

**Technical Contributors:**
- [Development setup](../implementation/developers.md)
- [Review protocol](../technical/protocol.md)
- [Explore codebase](https://github.com/interplaynetary/free-association)

**Researchers:**
- [Mathematical foundations](../technical/mathematics.md)
- [Network dynamics](../technical/network-dynamics.md)
- [Theoretical distinctions](../theory/vs-charity.md)

