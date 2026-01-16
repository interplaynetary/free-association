## Free Association: A Digital Public Infrastructure for Resource Coordination

**Free Association** is a mathematically-proven coordination infrastructure for priority alligned capacity distribution that enables organizations, communities, and individuals to allocate resources without centralized control, bureaucratic overhead, or market exclusion.

**The Core Problem:** Traditional resource coordination relies on markets (which exclude those without purchasing power), charity (which creates dependency), or bureaucracy (which is slow and inflexible). None of these mechanisms are adequate for the speed and scale of challenges we face — from climate adaptation to humanitarian response.

**The Solution:** A fourth type of economic relationship based on **priority aligned capacity distribution**. This creates a self-organizing coordination system that is:
 - **Fast**: Allocations converge in seconds, not months
 - **Fair**: Mathematically guaranteed proportional distribution based on priorities
 - **Efficient**: Resources flow directly based on declared needs and priority contribution
- **Decentralized**: No central authority controls outcomes or data

**Try It:** [free.playnet.lol](https://free.playnet.lol) (browser-based, peer-to-peer, no installation required)

### How the System Works

The infrastructure operates on three simple data points published by each participant:

**1. Priority Weights** - How do you prioritize the distribution of your capacity?
    - Each participant allocates 100% of weight among recipients or categories
    - Non-transferable and dynamically adjustable as priorities evolve
    - Weights reflect contribution, or operational necessity
    - Organized as a Prioritization tree tracking different types of support
 
 **2. Available Capacity** - What can you offer?
    - Examples: funds, expertise, facilities, time
    - Specify filters (time windows, locations, resource types)
    - Update dynamically as circumstances change
 
 **3. Declared Needs** - What do you require?
    - State specific requirements: Type, Quantity, Time, Location
    - Update in real-time as needs evolve
    - System caps allocations at actual declared needs (preventing accumulation)

 **4. Prioritization trees** - Structured tracking of contribution
    - Each branch represents a category (program areas, operational support, etc.)
    - Weights distributed among recipients within each branch
    - Global priority calculated from weighted contributions to priority-realization across all branches
    - Enables granular tracking while maintaining overall coherence

**System handles both intangible/tangible contribution:**
   - **Priorities**: Contributions toward one's goals/priorities/values do not require us to call them the same thing for allignment to occur as %'s of total-prioritization.
   - **Resources**: Concrete resources require common terminology (funding, expertise, time) to facilitate capacity-need matching

### The Allocation Algorithm
 
 **Constrained Weighted Allocation:**
 
 The system solves a resource allocation problem where providers and recipients express preferences through priorities (weights), subject to strict constraints.
 
 **1. Proportionality & Fairness**:
    - **Provider Weights**: "I dedicate X% of my capacity to this need" -> Guides initial distribution.
    - **Recipient Weights**: "I prefer X% of my support to come from you" -> Guides refined source adjustment.
 
 **2. Hard Constraints**:
    - **Capacity Limits**: Allocations never exceed available capacity.
    - **Physical Units**: Respects minimum divisible units (e.g. "people" must be integers).
 
 **3. Allocation Phase 1 (Provider Constraints)**:
    - Calculates ideal weighted targets based on provider priorities.
    - Distributes capacity proportionally to weights, respecting all limits.

 **4. Allocation Phase 2 (Recipient Refinement & Overshoot)**:
    - Recipients adjust *who* they receive from, shifting allocations to match their source preferences.
    - **Overshoot**: High-priority providers can temporarily displace low-priority incumbents even if the recipient is full.
    - **Hidden Demand**: Unserved but compatible needs are discovered and funded.
    - **Global Clamping**: Strict limits are enforced at the end of each round.

 **Dynamic & Self-Correcting**:
 - Remaining need = max(0, Declared Need - Total Received)
 - System converges to stable equilibrium as needs are met or network state changes.
 - Each resource type tracks independently.

### Mathematical Foundations

The system's fairness and efficiency are guaranteed by these formal properties:

```
Provider Optimization Problem (Phase 1):

Given:
- C: Total Provider Capacity
- N_i: Declared Need for recipient i
- w_i: Provider's weight for recipient i (Σ w_i = 100%)

Objective:
Minimize Σ (A_i - w_i * C)^2  (Minimize deviation from weighted proportional share)

Subject to Constraints:
1. 0 <= A_i <= N_i        (Need Constraint: Cannot exceed declared need)
2. Σ A_i <= C             (Capacity Constraint: Cannot exceed avail capacity)

Recipient Source Refinement (Phase 2):
Recipients adjust source mix to match their preferences (w_recip):
Minimize Σ (S_j - w_recip_j * Total_Received)^2

Subject to:
1. Total_Received constant (No change in total help)
2. Provider allocations constant (No change in provider load)

Update Law (Next Cycle):
Remaining_Need(t+1) = max(0, Declared_Need(t) - Total_Received(t))
```

**Key Properties:**
- **Proportional fairness**: Allocations strictly proportional to priority
- **Dynamic equilibrium**: System maintains instantaneous optimality as network state evolves

**Note on performance:** Reference implementation recomputes allocations in 100-200ms per state change. When network state stabilizes, needs converge to zero in O(log(1/ε)) rounds. In dynamic environments, the system continuously adapts rather than converging to a fixed point.

<details>
  <summary><b>Formal Proofs of System Properties</b></summary>

### Strategic Properties

**Recognition Gaming:**
The 100% recognition budget constraint creates self-correcting dynamics. False recognition automatically reduces recognition of beneficial partners, decreasing access to beneficial resources. For ongoing participants, the mathematics prevents gaming. See Recognition Gaming Analysis below for details.

### Proportional Fairness

**Theorem:** Allocations are strictly proportional to provider priorities.
 
 **Formal Statement:**
 ```
 ∀ Recipients A, B with respect to Provider P:
 If Priority(P, A) = Priority(P, B)
 Then Raw_Allocation(A) = Raw_Allocation(B)
 ```

**Proof:**
 
 The allocation solves: `Minimize Σ (A_i - w_i * C)^2`
 
 The optimality condition (ignoring boundary constraints) sets the derivative to zero:
 `2(A_i - w_i * C) = λ` (Lagrange multiplier for capacity constraint)
 
 This implies:
 `A_i = w_i * C + λ/2`
 
 If we assume the capacity constraint is binding (Σ A_i = C) and weights sum to 1 (Σ w_i = 1):
 `Σ A_i = Σ (w_i * C + λ/2) = C + n*λ/2 = C`
 `=> λ = 0`
 
 Therefore, when needs allow:
 `A_i = w_i * C`
 
 For any two recipients A and B:
 `A_A / A_B = (w_A * C) / (w_B * C) = w_A / w_B`
 
 Thus, allocations are strictly proportional to provider weights (which reflect priority contribution). Constraints (needs) simply cap this allocation, but do not distort the proportionality of the *target* distribution. □

### Allocation: The Core Mechanism

*Constrained Weighted Capacity Distribution*

Entities have **needs** (goals whose realization depends on capacity) and **availabilities** (capacities they can provide). The challenge is multi-provider, multi-recipient need satisfaction under constraints:

$$
\text{Find } X \text{ s.t. } \forall i, \sum_j X_{ij} \leq C_i \land \forall j, \sum_i X_{ij} \leq N_j
$$

Where $C_i$ = Capacity of provider $i$, $N_j$ = Need of recipient $j$.

#### Provider Constraints

Each provider has finite capacities (each summing to 100%) to distribute among compatible recipients. They prefer to allocate to needs whose contributions they value most highly.

#### Recipient Constraints

Each recipient has specific needs with finite capacity requirements. They prefer to receive from providers they trust/value most highly.

#### Two-Sided Optimization

The system must simultaneously satisfy provider preferences (allocate to valued needs) and recipient preferences (receive from valued providers) while respecting capacity/need limits.

This is a **constrained weighted allocation problem**: finding the allocation matrix that minimizes deviation from both providers' priorities and recipients' source preferences, subject to capacity and need constraints.

$$
\min_X \sum_{i,j} \left(\Phi(X_{ij}, P_{ij}) + \Psi(X_{ij}, R_{ji})\right)
$$

Where $P_{ij}$ = Provider $i$'s priority for recipient $j$, $R_{ji}$ = Recipient $j$'s priority for provider $i$, $\Phi, \Psi$ = Cost functions.

**Key Mechanism:** The protocol finds the allocation matrix that satisfies all capacity and need constraints while remaining as close as possible to the expressed preferences of both providers and recipients. This is the *least biased* solution - it doesn't impose any preference beyond what entities themselves express. The system converges to this solution through iterative constraint satisfaction, where capacity and need limits are enforced while preserving the proportional relationships in the expressed preferences.

$$
\frac{X_{ij}}{X_{ik}} \approx \frac{P_{ij}}{P_{ik}} \text{ (Proportional Preservation)}
$$

The allocation mechanism has several important mathematical properties that emerge from constraint satisfaction:

#### Proportional Preservation

If you express that Need A is twice as aligned as Need B, the system allocates approximately twice as much capacity to A (when feasible given constraints). The proportional relationships you express are preserved in the final allocation.

#### Least Biased Solution

Among all possible allocations that satisfy the constraints, the system selects the one that introduces the least additional bias beyond what entities express. This is the entropy-maximizing (information-theoretically optimal) solution.

#### Constraint Propagation

When constraints bind (e.g., a recipient reaches capacity), the effects propagate through the network. Capacity that cannot flow to a full recipient automatically redistributes to other compatible needs according to expressed preferences.

#### Equilibrium Convergence

The system converges to a stable equilibrium where no entity can improve their allocation quality (measured by preference satisfaction) without degrading someone else's. This is a Pareto-efficient outcome.

---

</details>

<details>
  <summary><b><i>Being Explored: What if Organizations/States Freely-Associated?</i></b></summary>

Thus far we have principally spoken of free-association between individuals, but what about between organizations, communities, states etc.?

If States/Organizations mutually recognized eachother's contributions towards the realization of eachother's priorities, and surplus flowed bi-directionally, there would be no more need for imports/exports or international-trade because resources and coordinations flows as surplus from mutual-recognized contributors.

Internally each state/organization would have a mechanism for collective setting of the proportions of the branches of self-actualization of their community.
For example: **Each member has an equal share of proportion-setting-power (at which levels?)**

Surplus would distribute according to mutual-fulfillment exactly the same as occurs between individuals.

We can also imagine a mechanism by which citizens could delegate a portion of their proportion-setting-power to another agent, within a particular category. So for example delegating 10% of your proportion-setting-power in the category of "environmental protection" to an ecologist.

The design space is vast, especially for all those decisions that do not concern proportions, for example:
- how is membership determined
- do all members get equal shares of proportion-setting-power (at which levels?) How is this determined?
- How are contributors added to nodes? How is this determined?
- Can nodes represent groups of contributors, and the tree represent a federation of groups? Could these groups have their own decision making logic for membership within them?
- how do new nodes get created in an organization's prioritization-tree? (are there limits to this?) - can one add point to one's own created-node? Or to a node one is a part of?
- can one add points towards a node one is a part of?
- How are capacities collectively declared, and how are their absolute values determined? How are their filters on share-distribution determined

If all people in the world are seperated through at most 6 degrees of seperation, then we can imagine all organization must at most be seperated by 3 or 4. This would be a significant computational gain for calculating transitive surplus shares.

This computational advantage could make organizational-level Free-Association more immediately practical than individual-level implementations for certain types of surplus. Resources that naturally flow at organizational scales (like electricity generation, manufacturing capacity, or agricultural output) might be more efficiently distributed through these shorter organizational networks.

</details>


## Use Cases and Outcomes

### Crisis Response: From Months to Seconds

Traditional coordination requires lengthy political negotiations before resources reach those in need. Free Association transforms this:

**Traditional System:**
- Day 1: Crisis hits
- Day 30: Coordination bodies convene
- Day 90: Political negotiations begin
- Day 180: Pledges finalized
- Day 270+: Resources begin flowing

**Free Association:**
- Day 1: Entity declares need in system
- Immediately: All participants see need; system recalculates optimal allocation
- Day 1-2: Resource commitments transparent and automatic based on pre-established prioritization
- Day 2-3: First resources arrive from mutual partners
- Ongoing: System continuously adapts as needs evolve

### Organizational Resource Coordination

**Foundation Grant Allocation**: Instead of lengthy application processes, foundations recognize mission-aligned organizations. When capacity is available, it flows automatically to recognized partners based on their declared needs and **contribution (based on recognition)**.

**Humanitarian Response**: Aid organizations mutually recognize each other's contributions to shared goals. When a crisis emerges, resources flow automatically to organizations with strongest **contribution (based on recognition)** and greatest need—no coordination meetings required.

**Impact Investment Networks**: Investors recognize organizations working on aligned goals. Capital flows based on **contribution (based on recognition)** and declared capital needs, creating efficient deployment without traditional fundraising overhead.

**Community Resource Sharing**: Communities allocate shared resources (facilities, equipment, expertise) based on members' **contribution (based on recognition)** and declared needs. The system handles allocation automatically, reducing administrative burden.

### Key Outcomes

**Speed**: Resource allocation occurs in seconds rather than months
- Target: <48 hours from need identification to commitment
- vs. typical 90+ days in traditional systems

**Efficiency**: Direct resource flow with minimal overhead
- Target: >95% of resources deployed to mission
- vs. typical ~70% after administrative costs

**Contribution**: Resources automatically flow to mission-aligned partners
 - No need for lengthy due diligence on abstract shared values, contribution is concrete

**Adaptability**: System responds in real-time as circumstances evolve
- Priorities change → allocations recalculate automatically
- New needs emerge → system converges to new equilibrium
- Partners join/leave → network adapts seamlessly

## System Properties and Network Dynamics

### How Resource Flows Are Determined

**Constrained Priority Allocation:**
 1. **Phase 1 - Provider Constraints**: Capacity is distributed according to provider priorities, bounded by declared needs and total capacity.
 2. **Phase 2 - Source Refinement**: Recipients refine their allocations to prefer sources they recognize, ensuring reciprocal stability.

**Real-Time Adaptation:**
- System recalculates allocations automatically when network state changes
- Response time: 100-200ms for allocation convergence
- Mathematical guarantee: If sufficient capacity exists, all needs are met through optimal allocation

**Mission-Aligned Resource Flow:**
- Prioritiest can extend to entities working on aligned causes
- Resources flow based on contribution to declared organizational goals
- Enables support for broader ecosystem of mission-aligned work

### Anti-Gaming & Alignment

*Why Free-Riding and Gaming Self-Destruct*

**True Recognition:** Recognition of contribution to the realization of priorities that *enables the continued realization of priorities* (self-sustaining).

**False Recognition:** Recognition of contribution to the realization of priorities that *impairs the continued realization of priorities* (self-terminating).

Participants define their goals subjectively, but achieving them depends on objective access to resources and contributions. Proportional recognition accuracy is validated through outcomes.

**The Causality Chain:**

$$
\text{Given: } \text{Total Recognition} = 100\%, \text{True} \cap \text{False} = \emptyset, \text{Capacity Directed} \propto \text{Recognition-Share}
$$

$$
\text{Total Recognition} = \text{True Recognition} + \text{False Recognition} \\
\uparrow \text{False Recognition} \implies \downarrow \text{True Recognition} \implies \downarrow \text{Alignment } (\alpha) \\
\implies \uparrow \text{Capacity Directed to non-beneficial} \\
\implies \downarrow \text{Capacity Directed to beneficial} \\
\implies \downarrow \text{Goal Achievement} \\
\implies \text{Immediate incentive to revoke recognition} \\
\implies \text{Free-rider loses allocation}
$$

**The causality chain:** False recognition → Displaced capacity → Worse outcomes → Immediate incentive to correct → Free-rider loses allocation.

**Key Implication:** The system creates natural incentives for true recognition. Inflation or misattribution of contribution to priority realization only decreases connection to actually beneficial partners. Participants that maintain **True Recognition** better-align their capacity allocation and achieve better outcomes.

## Collective Resource Coordination

Free Association can coordinate shared resource pools (organizational budgets, collective funds, shared facilities) among member entities.

### Allocation Process

**1. Define Collective Membership**
   - Collective defines member entities
   - Example: Coalition of humanitarian organizations

**2. Members Declare Needs**
   - Each member states resource requirements
   - Organization A: $1M/month operational funding
   - Organization B: $500K/month program funding
   - Organization C: $200K/month emergency reserve

**3. Members Establish Priority Contribution**
    - Each member prioritizes contribution sources aligned with the shared mission
    - Priority contribution percentages determine allocation weights
 
 **4. Calculate Collective Priority Distribution**
    - Sum all contribution values across collective members
    - Each member's share = their  contribution / total collective contribution

**5. Allocate Resources**
   - Distribute collective resources according to calculated shares
   - Cap each allocation at member's declared need

### Key Properties

- **Needs-based**: No entity receives beyond declared needs
 - **Priority-weighted**: Stronger contribution yields proportionally larger shares
 - **Continuously optimal**: System recalculates as network state evolves
 - **Non-accumulative**: Cannot accumulate resources beyond stated requirements

### Decentralized Coordination Advantages

- **No centralized value definition**: Each entity determines what constitutes meaningful contribution
- **Distributed assessment**: Value determination emerges from network rather than central authority
- **Flexible membership**: No centralized registry required for participation
- **Autonomous data**: Each entity maintains its own view of collective membership and resource availability

## Implementation and Participation

### For Organizations and Institutions

**Pilot Programs**: Organizations interested in piloting Free Association for resource coordination can:
- Start with a discretionary budget allocation
- Implement within specific program areas or partnerships
- Join coordination coalitions with other pilot organizations
- Access technical support and implementation guidance

**Coalition Participation**: Multiple engagement levels available:
- **Active Members**: Implementing pilots with resource commitments
- **Supporting Members**: Observing and learning from active pilots
- **Aligned Allies**: Staying connected to developments and findings

### For Developers and Contributors

**Development Priorities:**
- User interface refinement
- Protocol implementation and testing
- Documentation and educational materials
- Technical infrastructure and deployment

**Ways to Contribute:**
- **Technical Skills**: Contact team to discuss development needs
- **Resources**: Support infrastructure and operational requirements
- **Network Building**: Share with potential organizational partners

<a href="https://opencollective.com/free-association">
    <img width="300" src="https://opencollective.com/free-association/donate/button@2x.png?color=blue" />
</a>

**Contact:**
- General inquiries: info@openassociation.org
- Coalition and partnerships: coalition@openassociation.org
- Technical questions: See [PROTOCOL.md](PROTOCOL.md) or open an issue

## Appendix: Theoretical Foundations

### Distinction from Charity Models

Free Association differs fundamentally from charitable resource distribution:

**Traditional Charity:**
- Unidirectional resource flow (donor → recipient)
- Creates dependency relationships
- Donor receives no material reciprocity
- Hierarchical structure

**Free Association:**
- Bidirectional prioritization and resource flows
- Creates interdependency and mutual support
- Resources flow to entities contributing to your goals
- Peer-to-peer network structure

Priority-alligned allocation transcends charity by creating organic resource flows based on actual contributions to shared goals. Resources become part of a coordination network that strengthens mission contribution while enabling mutual achievement.

### Distinction from Investment/Equity Models

Free Association also differs from traditional investment structures:

**Traditional Equity Investment:**
- Fixed ownership shares acquired through capital provision
- Permanent claims on organizational value
- Transferable ownership enables accumulation
- Past contributions dominate present relationships
- Creates ownership and control dynamics

**Mutual Stakeholding (Equity-Based):**
- Organizations exchange shares for bidirectional value flows
- Creates non-zero-sum relationships
- **Critical limitation**: Ownership can be acquired through duplicitous means and persists regardless of actual contribution value
- Share transferability enables persistent false recognition

**Free Association:**
- No ownership of other entities—each maintains 100% autonomy
- Priority continuously adjustable based on current contributions
- Non-transferable priority prevents accumulation of false claims
- Present contributions determine resource flows, not past investments
- Mathematical properties ensure false priority naturally decays:

```
For any participant:
Total Recognition = 100%
Total Recognition = True Recognition + False Recognition
   ∴ ↑ False Recognition → ↓ True Recognition
      → ↓ Allocation to actually Beneficial Partners
         → ↓ Access to Beneficial Resources
            → ↓ Goal Achievement
               → Natural correction toward accurate recognition
```

**Key Distinction**: Priority (and the resulting access to resources) cannot be owned or accumulated. It reflects ongoing contribution relationships and adjusts continuously to reflect current coordination reality. This prevents power accumulation through ownership while maintaining incentives for genuine contribution.

This represents a resolution of traditional ownership/control tensions: mutual contribution without domination, coordination without centralized authority, reciprocity without permanent obligation.

 ### Additional Resources

- [openassociation.org](https://openassociation.org) - Project website and additional context
- [Glossary](GLOSSARY.md) - Technical terminology and definitions
- [Protocol Specification](PROTOCOL.md) - Formal protocol documentation
- [Governance](GOVERNANCE.md) - Project governance structure

**Interface Demonstration:**

[![Interface Overview](https://img.youtube.com/vi/gapy9mBpP5w/hqdefault.jpg)](https://youtu.be/gapy9mBpP5w?si=B2sbZpoaXStg4eKL)

## Technical Documentation

### Development Setup

**Prerequisites:**
- [Install bun](https://fenilsonani.com/articles/installation-step-of-bun)

**Development:**
```bash
bun install
bun run dev
```

**Testing:**
```bash
npm test  # Uses vitest and playwright
```

**Production Build:**
```bash
bun install
bun run build
```

## License & Governance

**License:** [GNU Affero General Public License v3.0](LICENSE) with 
[Additional Terms](LICENSE-ADDITIONAL-TERMS.md)

### Quick Summary

✅ **Anyone can use Free Association** - individuals, cooperatives, non-profits, 
governments, corporations

✅ **You can modify and deploy it** - fork it, adapt it, run your own instance

✅ **If you run it as a network service** - you must share your source code 
(that's the AGPL requirement)

✅ **You must give attribution** - credit where credit is due, prevents invisible use

✅ **Modified versions must be clearly marked** - prevents protocol fragmentation

### Why AGPL-3.0?

We want **universal access without universal capture**. AGPL ensures:
- Anyone can use it (no restrictions on who)
- Network services must share improvements (prevents proprietary capture)
- Modifications remain open (keeps the commons healthy)

### Why Additional Terms?

The additional terms (permitted by AGPL Section 7) add:
- **Attribution requirement** - prevents invisible cooptation
- **Protocol fidelity marking** - prevents trust-breaking fragmentation
- **Interoperability commitment** - prevents vendor lock-in

Together: Open for all, captured by none.

### Reference Implementation

This repository is the **canonical reference implementation** of the Free 
Association protocol, maintained by the core development team.

Other implementations are encouraged, but protocol conformance is measured 
against this implementation. See [PROTOCOL.md](PROTOCOL.md) for the formal 
specification.

### Questions?

- Licensing: info@openassociation.org
- Protocol: See [PROTOCOL.md](PROTOCOL.md) or open an issue
- Governance: See [GOVERNANCE.md](GOVERNANCE.md) (coming soon)