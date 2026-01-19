# Glossary

## Core Concepts

### Free Association
A coordination protocol enabling resource allocation through recognition of contributions. Entities acknowledge each other's value and allocate resources accordingly without centralized control.

### Recognition
The percentage (summing to 100%) that an entity allocates among contributors to their organizational goals. Non-transferable and dynamically adjustable.



### Entity
Any participant in the network—organization, community, institution, individual—that can publish priority weights, capacity, and needs.

### Network
The collection of entities and their recognition relationships. Forms the basis for resource allocation calculation.

---

## Recognition System

### Priority Weights
The distribution of an entity's 100% priority among contributors. Represents assessment of who contributes to organizational goals.

### Contribution Tree
Hierarchical structure organizing recognition by contribution category. Each branch has weight; contributors within each branch receive recognition points.

### Unilateral Recognition
Recognition in one direction only. Entity A recognizes Entity B, but B doesn't recognize A (or recognizes at lower percentage).

### Self-Recognition
Entity recognizing itself. Permitted in protocol, useful for time-shifting resources within own organization.

### Recognition Accuracy
Degree to which recognition pattern connects entity with actually beneficial resources and partnerships. Validated through outcomes.

### Effective Recognition
Recognition that connects entity with resources and partnerships genuinely advancing organizational goals.

### Ineffective Recognition
Recognition that fails to connect entity with beneficial resources or creates harmful dependencies.

---

## Resource Coordination

### Capacity
Available surplus resources an entity can offer. Can include funding, expertise, facilities, time, equipment.

### Declared Need
Specific resource requirement stated by an entity. Allocation capped at declared need to prevent accumulation.

### Remaining Need
Unsatisfied portion of declared need: `Remaining_Need = max(0, Declared_Need - Total_Received)`.

### Resource Type
Category of resource being coordinated. Can be tangible (funding, facilities) or intangible (expertise, mission-aligned values).

### Filter
Specification limiting resource compatibility—time windows, geographic locations, resource types, or other constraints.

---

## Allocation Mechanics

### Proportional Share
Percentage of provider's capacity allocated to specific recipient, based on recognition.

### Raw Allocation
Allocation before need cap is applied: `Raw_Allocation = Provider_Capacity × Share`.

### Final Allocation
Allocation after need cap: `Final_Allocation = min(Raw_Allocation, Declared_Need)`.

---

## System Properties

### Determinism
Property ensuring same network state always yields identical allocation results.

### Proportional Preservation
Property ensuring allocation ratios match recognition ratios where constraints allow.

### Pareto Efficiency
Property ensuring no entity can improve their allocation quality without degrading another's.

---

## Implementation Terms

### Reference Implementation
Canonical implementation of Free Association protocol, maintained by core development team. Used to measure protocol conformance.

### Conformant Implementation
Implementation satisfying all requirements of Free Association protocol specification.

### Protocol Violation
Implementation behavior that breaks core protocol properties (accumulation, transferable recognition, etc.).

### RFC (Request for Comments)
Proposal for protocol change, submitted through governance process for community discussion.

---

## Use Case Terms

### Pilot Program
Limited-scope implementation of Free Association to test and learn. Typically 10-20% of capacity, 3-12 months.

### Free Association Coalition
Group of organizations implementing Free Association for coordination.

### Collective Resource Coordination
Using Free Association to allocate shared resource pools among member entities.

### Crisis Response
Use of Free Association for rapid resource deployment in emergency situations.

---

## Mathematical Terms

### Recognition Distribution
Allocation of entity's 100% recognition among contributors: `Σ Recognition(E→Others) = 100%`.

### True Recognition
Recognition of contribution to the realization of priorities that *enables the continued realization of priorities* (self-sustaining).

### False Recognition
Recognition of contribution to the realization of priorities that *impairs the continued realization of priorities* (self-terminating).

### Alignment (α)
Measures how closely your capacity allocation matches true recognition. Formula: `Alignment (α) = Σ_i min(Allocation_i / Capacity, TrueRecognition_i)`. Ranges from 0 (completely misaligned) to 1 (perfectly aligned).

### Alignment Velocity (v)
Measures how fast alignment improves or degrades. Formula: `Velocity (v) = ΔAlignment / ΔTime`. Positive velocity indicates learning and correcting, negative indicates degrading, zero indicates stability.

### Proportional Preservation
Property ensuring that if you express Recipient A should receive twice as much as Recipient B (through recognition), the system allocates approximately twice as much capacity to A when feasible given constraints.

### Least Biased Solution
Among all possible allocations satisfying constraints, the system selects the one that introduces the least additional bias beyond what entities express. This is the entropy-maximizing (information-theoretically optimal) solution.

### Constraint Propagation
When constraints bind (e.g., a recipient reaches capacity), effects propagate through the network. Capacity that cannot flow to a full recipient automatically redistributes to other compatible needs according to expressed preferences.

### Pareto Efficiency
The system converges to a stable equilibrium where no entity can improve their allocation quality (measured by preference satisfaction) without degrading someone else's.

---

## Related Terms

### Digital Public Infrastructure (DPI)
Infrastructure serving public interest, openly accessible, not privately controlled. Free Association designed as DPI for resource coordination.

### Peer-to-Peer (P2P)
Decentralized architecture where participants interact directly without central intermediary.

### Coordination Mechanism
Method for aligning actions of multiple entities toward shared or compatible goals.

### Resource Allocation
Process of distributing available resources among entities with requirements.

### Mutual Aid
Support exchanged among peers based on reciprocity and solidarity. Free Association provides scalable mutual aid infrastructure.

---

## Acronyms

**DPI** - Digital Public Infrastructure  

**P2P** - Peer-to-Peer

**RFC** - Request for Comments

**AGPL** - Affero General Public License (project license)

---

## Further Reading

**Concepts:** [Core Concepts](../concepts/README.md)

**Mathematics:** [Mathematical Foundations](mathematics.md)

**Protocol:** [Protocol Specification](protocol.md)

**Implementation:** [For Organizations](../implementation/organizations.md)

