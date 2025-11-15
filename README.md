## Free Association: A Digital Public Infrastructure for Resource Coordination [![](https://img.shields.io/opencollective/backers/playnet)](https://opencollective.com/playnet#section-contributors) 

**Free Association** is a mathematically-proven coordination infrastructure that enables organizations, communities, and individuals to allocate resources efficiently based on mutual recognition of contributions—without centralized control, bureaucratic overhead, or market exclusion.

**The Core Problem:** Traditional resource coordination relies on markets (which exclude those without purchasing power), charity (which creates dependency), or bureaucracy (which is slow and inflexible). None of these mechanisms are adequate for the speed and scale of challenges we face — from climate adaptation to humanitarian response.

**The Solution:** A fourth type of economic relationship based on **mutual recognition**—where entities acknowledge each other's contributions toward shared goals and allocate resources accordingly. This creates a self-organizing coordination system that is:

- **Fast**: Allocations converge in seconds, not months
- **Fair**: Mathematically guaranteed proportional distribution
- **Efficient**: Resources flow directly based on declared needs and mutual recognition
- **Decentralized**: No central authority controls outcomes or data

**Try It:** [interplaynetary.github.io/free-association](https://interplaynetary.github.io/free-association/) (browser-based, peer-to-peer, no installation required)

### How the System Works

The infrastructure operates on three simple data points published by each participant:

**1. Recognition Weights** - Who contributes to your organizational goals?
   - Each entity allocates 100% of recognition among contributors
   - Non-transferable and dynamically adjustable as relationships evolve
   - Recognition can reflect contributions to direct operations or broader mission-aligned values
   - Organized as a contribution tree tracking different types of support

**2. Available Capacity** - What resources can you offer?
   - Declare surplus resources (funds, expertise, facilities, time)
   - Specify filters (time windows, locations, resource types)
   - Update dynamically as circumstances change

**3. Declared Needs** - What resources do you require?
   - State specific resource requirements
   - Update in real-time as needs evolve
   - System caps allocations at actual declared needs (preventing accumulation)

**4. Mutual Recognition** - Bidirectional acknowledgment of contributions
   - Calculated as the minimum of reciprocal recognition percentages
   - Example: If Organization A recognizes B at 50% and B recognizes A at 10%, their mutual recognition is 10%
   - Taking the minimum ensures proportional reciprocity
   - Self-recognition is valid for time-shifting resources within your own organization

**5. Contribution Trees** - Structured tracking of contribution types
   - Each branch represents a category (program areas, operational support, etc.)
   - Points distributed among contributors within each branch
   - Global recognition calculated from weighted contributions across all branches
   - Enables granular tracking while maintaining overall coherence

**Resource Types:**
   - **Mission-aligned values**: Contributions toward organizational mission and values (no shared definitions required)
   - **Specific resource types**: Concrete resources requiring common terminology (funding, expertise, facilities)
   - System handles both intangible mission contributions and tangible resource coordination

### The Allocation Algorithm

**Two-Tier Priority System:**

**Tier 1 - Mutual Recognition Priority**: Entities with mutual recognition receive first priority based on:
   - Strength of mutual recognition
   - Declared resource needs
   - Compatible resource specifications (time, location, type)

**Tier 2 - Unilateral Recognition**: Remaining capacity flows to entities you recognize (even without mutual recognition)
   - Enables support for new partners building recognition networks
   - Maintains incentives for genuine contribution

**Allocation Process:**
1. Filter for compatible resource specifications
2. Calculate proportional shares based on mutual recognition
3. Recognition determines the split; need size sets the cap
4. Allocations capped at declared needs (no accumulation beyond stated requirements)

**Dynamic Updates:**
- Remaining need = max(0, Declared Need - Total Received)
- Adaptive damping prevents allocation oscillation
- Each resource type tracks independently
- System converges to stable equilibrium in 5-10 calculation rounds

### Mathematical Foundations

The system's fairness and efficiency are guaranteed by these formal properties:

```
Recognition Distribution:
- Total Recognition per Entity = 100%
- Recognition allocated among contributors as percentages

Mutual Recognition Calculation:
MR(Entity_A, Entity_B) = min(
    Recognition_A_gives_B,
    Recognition_B_gives_A
)

Proportional Share Calculation:
Share(Recipient, Provider) = 
    MR(Recipient, Provider) / Σ MR(Provider, All_Compatible_Recipients)

Active Need (with oscillation prevention):
Active_Need = Declared_Need × Damping_Factor
    where Damping_Factor ∈ {0.5, 0.8, 1.0}

Raw Allocation:
Raw_Allocation(Recipient, Provider) = 
    Provider_Capacity × Share(Recipient, Provider)

Final Allocation (capped at need):
Final_Allocation(Recipient, Provider) = min(
    Raw_Allocation(Recipient, Provider),
    Declared_Need(Recipient)
)

Need Update (next cycle):
Remaining_Need = max(0, Declared_Need - Total_Received)
```

**Key Properties:**
- **Need-capped allocation**: Allocations never exceed declared needs (prevents accumulation)
- **Proportional fairness**: Allocations strictly proportional to mutual recognition
- **Dynamic equilibrium**: System maintains instantaneous optimality as network state evolves
- **Contraction guarantee**: Receiving resources always reduces remaining need (holds unconditionally)
- **Deterministic algorithm**: Same inputs always produce same allocations

**Note on performance:** Reference implementation recomputes allocations in 100-200ms per state change. When network state stabilizes, needs converge to zero in O(log(1/ε)) rounds. In dynamic environments, the system continuously adapts rather than converging to a fixed point.

<details>
  <summary><b>Formal Proofs of System Properties</b></summary>

### Strategic Properties

**Need Declaration Incentives:**

The allocation capping mechanism creates incentives for honest need reporting:

```
Allocation(R, P) = min(Raw_Allocation(R, P), Declared_Need)
```

**Key observations:**
- **Over-reporting** beyond actual need: Receives allocation but cannot utilize excess (Property 4: Non-Accumulative automatically reduces remaining need)
- **Under-reporting** below actual need: Guaranteed to receive less than actual requirements
- **Honest reporting**: Maximizes utility given current recognition network

**Recognition Gaming:**
The 100% recognition budget constraint creates self-correcting dynamics. False recognition automatically reduces recognition of beneficial partners, decreasing access to beneficial resources. For ongoing participants, the mathematics prevents gaming. See Recognition Gaming Analysis below for details.

**Provider Gaming:**
Provider non-delivery is resolved in Protocol v6 (draft) through satisfaction-based learning. See Provider Gaming Analysis below.

### Property 2: Proportional Fairness

**Theorem:** Allocations are strictly proportional to mutual recognition.

**Formal Statement:**
```
∀ Recipients A, B with respect to Provider P:
If MR(P, A) = MR(P, B)
Then Raw_Allocation(A) = Raw_Allocation(B)
```

**Proof:**

Given the allocation formula:
```
Share(Recipient, Provider) = MR(Recipient, Provider) / Total_MR

Where Total_MR = Σ MR(Provider, All_Compatible_Recipients)
```

For recipients A and B:
```
Share(A) = MR(P, A) / Total_MR
Share(B) = MR(P, B) / Total_MR

If MR(P, A) = MR(P, B), then:
Share(A) = Share(B)

Raw allocation calculation:
Raw_Allocation(A) = Provider_Capacity × Share(A)
Raw_Allocation(B) = Provider_Capacity × Share(B)

Since Share(A) = Share(B) and Provider_Capacity is constant:
∴ Raw_Allocation(A) = Raw_Allocation(B)
```

**Generalization:** For any ratio r = MR(P, A) / MR(P, B):
```
Raw_Allocation(A) / Raw_Allocation(B) = r
```

Allocations are strictly proportional to mutual recognition ratios. □

### Property 3: Dynamic Equilibrium and Contraction

**Theorem:** The system maintains instantaneous optimality as network state evolves.

**Framework:** The system computes optimal allocation r*(S) for current state S (recognition, needs, capacities), then continuously recomputes as S changes. This is **dynamic equilibrium**, not convergence to a fixed point.

**Formal Proof of Contraction:**

**Step 1: Allocation capping ensures needs always decrease from receiving**

For any recipient i in any round t:
```
φ_i(r) ≤ r_i  (allocation never exceeds need)
```

Therefore, receiving resources always reduces remaining need:
```
Remaining_Need(t+1) = Need(t) - φ(r(t))
                    ≤ Need(t) - 0
                    = Need(t)
```

**This holds regardless of whether needs change between rounds.**

**Step 2: Per-round contraction with positive allocation**

Define fill fraction: f = Σ φ_i(r) / Σ r_i (fraction of needs satisfied per round)

When f > 0 (some allocation occurs):
```
||r(t+1)|| = ||r(t) - φ(r(t))||
           ≤ (1 - f)·||r(t)||
```

Contraction constant: k = (1 - f) < 1

**Step 3: Convergence to zero (static case only)**

If network state becomes static (no new needs, stable recognition/capacity):
```
||r(t)|| ≤ k^t · ||r(0)|| → 0 as t → ∞
```

Rate: T_ε = O(log(||r(0)||/ε) / log(1/k))

**Dynamic case:** When needs evolve, the system doesn't converge to zero but instead maintains **instantaneous optimality** - always computing the best allocation for current state.

**Key insight:**
- **Contraction property holds always**: Receiving reduces need
- **Convergence to zero**: Only relevant when state stabilizes
- **Real-world operation**: System continuously adapts to changing needs/capacities

**Damping:** Oscillation detection (α = 0.5) prevents over-allocation cycles, improving stability without compromising responsiveness.

**Implementation note:** Reference implementation recomputes allocations in 100-200ms per state change, achieving near-instantaneous adaptation to network evolution. □

### Property 4: Non-Accumulative

**Theorem:** No entity receives beyond declared needs.

**Formal Statement:**
```
∀ Recipient R, ∀ Time t:
Total_Received(R, t) ≤ Declared_Need(R, t)
```

**Proof:**

By the allocation formula definition:
```
Final_Allocation(R, P) = min(Raw_Allocation(R, P), Declared_Need(R))
```

For any single provider P:
```
Final_Allocation(R, P) ≤ Declared_Need(R)
```

Total received from all providers:
```
Total_Received(R) = Σ Final_Allocation(R, All_Providers)
```

The system tracks cumulative allocations and updates remaining need:
```
Remaining_Need(R) = max(0, Declared_Need(R) - Total_Received(R))
```

Once Total_Received(R) ≥ Declared_Need(R):
```
Remaining_Need(R) = max(0, Declared_Need(R) - Total_Received(R))
                  = 0
```

When Remaining_Need = 0:
```
No further allocations occur to R
Total_Received cannot exceed Declared_Need
```

**System Guarantee:**
```
The min() capping function combined with remaining need tracking ensures:
∀ R, ∀ t: Total_Received(R, t) ≤ Declared_Need(R, t)

No accumulation beyond stated requirements is mathematically possible.
```

**Conclusion:** The allocation formula structurally prevents accumulation. □

### Property 5: Contraction (Always Holds)

**Theorem:** Receiving resources always reduces remaining need.

**Formal Statement:**
```
For any recipient R receiving allocation A(R) in round t:
Remaining_Need(R, after) = max(0, Need(R, before) - A(R))
                         ≤ Need(R, before)
```

**Proof:**

By allocation capping (Property 1):
```
A(R) ≤ Need(R, before)
```

Therefore:
```
Remaining_Need(R, after) = max(0, Need(R, before) - A(R))
                         ≤ Need(R, before) - 0
                         = Need(R, before)
```

Since A(R) ≥ 0 (allocations are non-negative) and max(0, ...) prevents negative needs:
```
Receiving resources strictly reduces need (when A(R) > 0)
Receiving zero resources leaves need unchanged
∴ Remaining_Need ≤ Need (contraction property)
```

**This holds in every allocation round, regardless of how needs change between rounds.**

**Implication for dynamic systems:**

Even when needs evolve over time:
- Each allocation reduces the specific need it addresses
- System continuously recomputes as new needs emerge
- No accumulation beyond stated requirements possible
- The system never "makes things worse" through allocation

**Static equilibrium case:**

When network state stabilizes (no new needs, constant recognition/capacity):
```
If Remaining_Need(R) > 0 and
   Available_Capacity > 0 and
   MR(R, Providers) > 0
Then Received(R) > 0
   ∴ Remaining_Need decreases

The only stable equilibrium is Remaining_Need = 0 for all R
(assuming sufficient network capacity)
```

**Conclusion:** The contraction property is unconditional - allocation always moves each specific need toward satisfaction, enabling the system to track evolving requirements while preventing accumulation. □

### Property 6: Determinism (Algorithm-Level)

**Theorem:** The allocation algorithm produces identical results for identical inputs.

**Formal Statement:**
```
∀ Inputs I₁, I₂:
If Recognition(I₁) = Recognition(I₂) AND
   Capacity(I₁) = Capacity(I₂) AND
   Needs(I₁) = Needs(I₂)
Then allocate(I₁) = allocate(I₂)
```

**Proof:**

The allocation function is defined as a pure mathematical operation:
```
φ(Recognition, Capacity, Needs) → Allocations
```

Each step is deterministic:
```
1. Mutual Recognition calculation:
   MR(A, B) = min(Recognition_A→B, Recognition_B→A)
   Pure function of Recognition weights → Deterministic ✓

2. Share calculation:
   Share(R, P) = MR(R, P) / Σ MR(P, All_R)
   Pure arithmetic → Deterministic ✓

3. Raw Allocation:
   Raw_Alloc = Capacity × Share
   Pure multiplication → Deterministic ✓

4. Final Allocation:
   Final_Alloc = min(Raw_Alloc, Need)
   Pure function → Deterministic ✓
```

**Scope:** This property guarantees that the allocation algorithm itself is deterministic. It does NOT address:
- Network-level consistency (different nodes may observe different states at different times)
- Byzantine fault tolerance (malicious nodes may compute incorrectly)
- Causal consistency guarantees (handled separately by ITC/vector clocks)

**Implication:** Given complete and identical input data, any correct implementation of the algorithm will produce identical allocation results.

**Conclusion:** The allocation algorithm is deterministic by construction. □

---

### Provider Gaming Analysis

**Capacity Under-Declaration:**

A provider might under-declare capacity to reserve resources or maintain optionality. However:
- Under-declared capacity simply reduces provider's contribution
- Provider loses influence proportional to withheld capacity
- No benefit to provider from under-declaring

**Non-Delivery (Current Protocol - v5):**

In the current protocol, a provider could declare capacity but fail to deliver. Recognition updates are social/manual, requiring active community response.

**Resolution (Protocol v6 - Draft):**

Protocol v6 introduces **satisfaction-based learning** that automatically resolves non-delivery:
- Recipients rate actual delivery quality/usefulness
- Non-delivery receives 0.0 satisfaction rating
- Future allocations automatically weighted: `share × satisfaction`
- Non-delivering providers automatically removed from future allocations

See `protocolv6.mmd` for complete specification. This transforms provider non-delivery from an algorithmic vulnerability to a self-correcting property.

**Conditional Capacity:**

The protocol supports `SlotFilter` specifications for time/location/type constraints. This is intentional design allowing legitimate targeting, though it cannot algorithmically distinguish between legitimate filtering and strategic preference.

---

### Recognition Gaming Analysis

**Question:** Can entities manipulate outcomes through recognition weight gaming?

**The Self-Correcting Mechanism:**

The system has a fundamental mathematical property that prevents most recognition gaming:

```
Total Recognition = 100% (zero-sum constraint)
Total Recognition = Effective Recognition + Ineffective Recognition

Therefore:
↑ Ineffective Recognition → ↓ Effective Recognition
   → ↓ Mutual Recognition with Beneficial Partners
      → ↓ Access to Beneficial Resources
         → ↓ Goal Achievement
```

**Why this resolves most "gaming" attempts:**

1. **Individual false recognition:** Giving recognition to non-contributors automatically reduces recognition of actual contributors. The entity starves itself of beneficial resources through its own choices.

2. **Recognition "cartels":** 
   - If cartel members genuinely meet each other's needs → this is a legitimate cooperative, not gaming
   - If cartel members don't actually help each other → needs remain unmet, forcing recognition reallocation to external helpers or actual cooperation
   - 100% budget forces tradeoff: high internal recognition = low external recognition = reduced access to external resources

3. **Time-shifting recognition:** Reducing recognition after receiving decreases future mutual recognition, reducing future allocations. The bidirectional constraint makes this self-defeating over multiple periods.

**The One Remaining Algorithmic Vulnerability: Single-Period Exit**

An entity could:
1. Build genuine recognition through contribution
2. In final period before exit: declare maximal needs
3. Receive allocation based on accumulated recognition
4. Exit before any correction mechanisms operate

**Why this works once:**
- Non-accumulation property limits extraction to declared need per period
- But entity can take one period's allocation and leave
- No future period exists for correction mechanism

**Mitigation:**
- Non-accumulation caps extraction (can't stockpile)
- Requires building genuine recognition first
- Network can observe patterns
- Social reputation effects

**Conclusion:**

The protocol is **self-correcting for ongoing participants** through:
- 100% recognition budget constraint + outcome feedback (need gaming)
- Satisfaction-based learning in v6 (provider gaming)

The only remaining algorithmic vulnerability is **one-time exit after building trust**, which is fundamentally a trust/reputation problem that exists in any system requiring multi-period interaction.

---

</details>

<details>
  <summary><b><i>Being Explored: What if Organizations/States Freely-Associated?</i></b></summary>

Thus far we have principally spoken of free-association between individuals, but what about between organizations, communities, states etc.?

If States/Organizations mutually recognized eachother's contributions towards their own self actualization, and surplus flowed bi-directionally, there would be no more need for imports/exports or international-trade because resources and coordinations flows as surplus from mutual-recognized contributors.

Internally each state/organization would have a mechanism for collective setting of the proportions of the branches of self-actualization of their community.
For example: **Each member has an equal share of proportion-setting-power (at which levels?)**

Surplus would distribute according to mutual-fulfillment exactly the same as occurs between individuals.

We can also imagine a mechanism by which citizens could delegate a portion of their proportion-setting-power to another agent, within a particular category. So for example delegating 10% of your proportion-setting-power in the category of "environmental protection" to an ecologist.

The design space is vast, especially for all those decisions that do not concern proportions, for example:
- how is membership determined
- do all members get equal shares of proportion-setting-power (at which levels?) How is this determined?
- How are contributors added to nodes? How is this determined?
- Can nodes represent groups of contributors, and the tree represent a federation of groups? Could these groups have their own decision making logic for membership within them?
- how do new nodes get created in an organization's recognition-tree? (are there limits to this?) - can one add point to one's own created-node? Or to a node one is a part of?
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
- Day 1-2: Resource commitments transparent and automatic based on pre-established mutual recognition
- Day 2-3: First resources arrive from mutual partners
- Ongoing: System continuously adapts as needs evolve

### Organizational Resource Coordination

**Foundation Grant Allocation**: Instead of lengthy application processes, foundations recognize mission-aligned organizations. When capacity is available, it flows automatically to recognized partners based on their declared needs and mutual recognition strength.

**Humanitarian Response**: Aid organizations mutually recognize each other's contributions to shared goals. When a crisis emerges, resources flow automatically to organizations with strongest mutual recognition and greatest need—no coordination meetings required.

**Impact Investment Networks**: Investors recognize organizations working on aligned goals. Capital flows based on mutual recognition and declared capital needs, creating efficient deployment without traditional fundraising overhead.

**Community Resource Sharing**: Communities allocate shared resources (facilities, equipment, expertise) based on members' mutual recognition of contributions and declared needs. The system handles allocation automatically, reducing administrative burden.

### Key Outcomes

**Speed**: Resource allocation occurs in seconds rather than months
- Target: <48 hours from need identification to commitment
- vs. typical 90+ days in traditional systems

**Efficiency**: Direct resource flow with minimal overhead
- Target: >95% of resources deployed to mission
- vs. typical ~70% after administrative costs

**Alignment**: Resources automatically flow to mission-aligned partners
- Mutual recognition ensures values alignment
- No need for lengthy due diligence on shared values

**Adaptability**: System responds in real-time as circumstances evolve
- Priorities change → allocations recalculate automatically
- New needs emerge → system converges to new equilibrium
- Partners join/leave → network adapts seamlessly

## System Properties and Network Dynamics

### How Resource Flows Are Determined

**Priority-Based Allocation:**
1. **Tier 1 - Mutual Recognition**: Entities with mutual recognition receive priority proportional to recognition strength and declared needs
2. **Tier 2 - Unilateral Recognition**: Remaining capacity flows to recognized entities (even without mutual recognition), enabling support for emerging partnerships

**Real-Time Adaptation:**
- System recalculates allocations automatically when network state changes
- Response time: 100-200ms for allocation convergence
- Mathematical guarantee: If sufficient capacity exists, all needs are met through optimal allocation

**Mission-Aligned Resource Flow:**
- Recognition can extend to entities working on aligned causes
- Resources flow based on contribution to declared organizational goals
- Enables support for broader ecosystem of mission-aligned work

### Self-Correcting Network Properties

The system naturally promotes accurate recognition through mathematical necessity:

**Recognition Accuracy and Network Integrity:**

Organizations define their goals and priorities subjectively, but achieving them depends on objective access to resources and partnerships. Recognition accuracy is validated through outcomes:

- **Effective Recognition**: Recognition that, when acted upon, connects you with resources and partnerships that genuinely advance your organizational goals (validated by positive outcomes)

- **Ineffective Recognition**: Recognition that fails to connect you with beneficial resources or creates harmful dependencies (invalidated by negative outcomes)

**Mathematical Consequence:**

```
For any participant:
Total Recognition = 100%
Total Recognition = Effective Recognition + Ineffective Recognition

Therefore:
↑ Ineffective Recognition → ↓ Effective Recognition
   → ↓ Mutual Recognition with Actually Beneficial Partners
      → ↓ Access to Actually Beneficial Resources
         → ↓ Organizational Goal Achievement
            → Natural incentive to correct recognition accuracy
```

**Key Implication**: The system creates natural incentives for accurate recognition. Inflating or misattributing recognition only decreases connection to beneficial partners and resources. Organizations that maintain accurate recognition patterns receive better-aligned resources and achieve better outcomes.

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

**3. Members Establish Mutual Recognition**
   - Each member recognizes others' contributions to shared mission
   - Mutual recognition percentages determine allocation weights

**4. Calculate Collective Priority Distribution**
   - Sum all mutual recognition values across collective members
   - Each member's share = their mutual recognition / total collective mutual recognition

**5. Allocate Resources**
   - Distribute collective resources according to calculated shares
   - Cap each allocation at member's declared need

### Key Properties

- **Needs-based**: No entity receives beyond declared needs
- **Recognition-weighted**: Stronger mutual recognition yields proportionally larger shares
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

**Coalition Membership**: Multiple engagement levels available:
- **Active Members**: Implementing pilots with resource commitments
- **Supporting Members**: Observing and learning from active pilots
- **Aligned Allies**: Staying connected to developments and findings

See [DPI.md](DPI.md) for detailed information about organizational pilots and coalition membership.

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

<a href="https://opencollective.com/playnet">
    <img width="300" src="https://opencollective.com/playnet/donate/button@2x.png?color=blue" />
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
- Bidirectional recognition and resource flows
- Creates interdependency and mutual support
- Resources flow to entities contributing to your goals
- Peer-to-peer network structure

Recognition-based allocation transcends charity by creating organic resource flows based on actual contributions to shared goals. Resources become part of a coordination network that strengthens mission alignment while enabling mutual achievement.

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
- Recognition continuously adjustable based on current contributions
- Non-transferable recognition prevents accumulation of false claims
- Present contributions determine resource flows, not past investments
- Mathematical properties ensure false recognition naturally decays:

```
For any participant:
Total Recognition = 100%
Total Recognition = Effective Recognition + Ineffective Recognition
   ∴ ↑ Ineffective Recognition → ↓ Effective Recognition
      → ↓ Mutual Recognition with Beneficial Partners
         → ↓ Access to Beneficial Resources
            → ↓ Goal Achievement
               → Natural correction toward accurate recognition
```

**Key Distinction**: Recognition cannot be owned or accumulated. It reflects ongoing contribution relationships and adjusts continuously to reflect current coordination reality. This prevents power accumulation through ownership while maintaining incentives for genuine contribution.

This represents a resolution of traditional ownership/control tensions: mutual recognition without domination, coordination without centralized authority, reciprocity without permanent obligation.

### Additional Resources

- [Playnet.lol](https://playnet.lol) - Project website and additional context
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