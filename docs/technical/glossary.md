# Glossary

## Core Concepts

### Free Association
A coordination protocol enabling resource allocation through reciprocal alignment of contributions. Entities acknowledge each other's value and allocate resources accordingly without centralized control.

### Recognition
The percentage (summing to 100%) that an entity allocates among contributors to their organizational goals. Non-transferable and dynamically adjustable.

### Reciprocal Alignment (RA)
**RA** - Reciprocal Alignment
The calculated overlap in priorities between two entities. Determined by the minimum of the reciprocal priority percentages. Ensures that resource flows reflect shared commitment. Equal to `min(Priority_A_to_B, Priority_B_to_A)`.

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

### Active Need
Declared need adjusted by damping factor to prevent oscillation: `Active_Need = Declared_Need × Damping_Factor`.

### Remaining Need
Unsatisfied portion of declared need: `Remaining_Need = max(0, Declared_Need - Total_Received)`.

### Resource Type
Category of resource being coordinated. Can be tangible (funding, facilities) or intangible (expertise, mission-aligned values).

### Filter
Specification limiting resource compatibility—time windows, geographic locations, resource types, or other constraints.

---

## Allocation Mechanics

### Proportional Share
Percentage of provider's capacity allocated to specific recipient, based on reciprocal alignment: `Share = RA(P, R) / Σ RA(P, All_Recipients)`.

### Raw Allocation
Allocation before need cap is applied: `Raw_Allocation = Provider_Capacity × Share`.

### Final Allocation
Allocation after need cap: `Final_Allocation = min(Raw_Allocation, Declared_Need)`.

### Phase 1 Allocation
Allocation step where reciprocal alignment relationships receive priority. Capacity flows efficiently between entities that mutually prioritize each other.

### Phase 2 Allocation
Remaining capacity (after Phase 1) allocated among entities with unilateral priority from provider. Allows for exploration of new relationships and support for up-and-coming entities.

---

## System Properties

### Convergence
Process of system reaching stable equilibrium state. Typically occurs in 5-10 calculation rounds (1-2 seconds).

### Damping Factor
Multiplier applied to declared need to prevent allocation oscillation. Values: 0.5 (conservative), 0.8 (balanced), 1.0 (responsive).

### Need Declaration Incentives
Allocation capping mechanism creates incentives for honest need reporting. Over-reporting doesn't accumulate resources (non-accumulation property applies), under-reporting reduces allocation. The 100% recognition budget combined with outcome feedback creates self-correcting dynamics for ongoing participants. Protocol v6 (draft) adds satisfaction-based learning that automatically resolves provider non-delivery.

### Proportional Fairness
Property ensuring allocations strictly proportional to reciprocal alignment strength.

### Non-Accumulative
Property preventing entities from receiving resources beyond declared needs.

### Contraction Property
Guarantee that receiving resources always reduces remaining need. This holds unconditionally in every allocation round, regardless of how needs change between rounds.

### Determinism
Property ensuring same network state always yields identical allocation results.

---

## Network Patterns

### Hub-and-Spoke Network
Network structure with central organization recognizing many partners, who primarily recognize the hub.

### Mesh Network
Dense network where multiple entities have reciprocal alignment relationships. No single central hub.

### Hierarchical Network
Network organized in tiers, with recognition flowing primarily within tiers and some cross-tier relationships.

### Hybrid Network
Network combining multiple structural patterns (hub-and-spoke, mesh, hierarchical) based on actual relationships.

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

### Coalition
Group of organizations jointly implementing Free Association for coordination.

### Collective Resource Coordination
Using Free Association to allocate shared resource pools among member entities.

### Crisis Response
Use of Free Association for rapid resource deployment in emergency situations.

---

## Organizational Terms

### Mission-Aligned Partners
Organizations working on compatible goals, even without direct collaboration.

### Contribution Assessment
Process of determining recognition percentages based on evaluation of contributions to organizational goals.

### Capacity Declaration
Statement of available surplus resources with specifications (filters, timing, type).

### Need Declaration
Statement of specific resource requirements with specifications.

### Recognition Pattern
Overall distribution of entity's recognition across contributors. Encodes strategic priorities and relationships.

---

## Comparative Terms

### Traditional Coordination
Resource coordination through markets (purchasing power), charity (donations), or bureaucracy (committees).

### Bureaucratic Overhead
Administrative time and resources consumed by coordination process. Target reduction: 70-90% with Free Association.

### Fundraising Overhead
Time organizations spend acquiring resources rather than mission work. Target reduction: 75-85% with Free Association.

### Due Diligence
Process of verifying potential partners. In Free Association, recognition encodes ongoing assessment.

---

## Mathematical Terms

### Recognition Distribution
Allocation of entity's 100% recognition among contributors: `Σ Recognition(E→Others) = 100%`.

### Symmetry Property
Reciprocal alignment is symmetric: `RA(A, B) = RA(B, A)`.

### Minimum Function
Taking the lower of two values. Used in reciprocal alignment: `RA(A,B) = min(Priority_A_to_B, Priority_B_to_A)`.

### Convergence Criterion
Threshold for determining system has reached stable state: `|Need(t+1) - Need(t)| < ε`.

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

**RA** - Reciprocal Alignment

**DPI** - Digital Public Infrastructure  

**P2P** - Peer-to-Peer

**RFC** - Request for Comments

**AGPL** - Affero General Public License (project license)

---

## Further Reading

**Concepts:** [How It Works](../concepts/how-it-works.md)

**Mathematics:** [Mathematical Foundations](mathematics.md)

**Protocol:** [Protocol Specification](protocol.md)

**Implementation:** [For Organizations](../implementation/organizations.md)

