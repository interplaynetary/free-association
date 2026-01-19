# Free Association Coalition

**Participation Framework for Cooperative Production and Coordination**

**Version:** 0.2.0 (Draft for Review)  
**Date:** November 29, 2025  
**Based on:** Free-Association Organizational Protocol (playnet.gitbook.io/docs)

***

## I. Foundation

### 1.1 Philosophical Basis

**"A community of free individuals, carrying on their work with the means of production in common, in which the labour-power of all the different individuals is consciously applied as the combined labour-power of the community."**

This coalition exists to realize freedom of association in its fullest sense:
* Individuals are free to form, join, or leave collective bodies
* Collectives themselves act freely and cooperatively
* Producers stand as equals
* Ownership and decision-making are shared rather than imposed
* Voluntary association becomes identical with consciously organized collective activity

### 1.2 Core Principle

**All capacity  -  whether external (donations, grants) or internal (proceeds of labor)  -  flows through computational recognition protocols.**

Recognition is your acknowledgment of contributions:
* Each participant has 100% recognition to distribute
* Represents what enables you to self-actualize (directly/transitively)
* Non-transferable (only you can judge what enables you)
* Dynamically adjustable as enabling relationships evolve
* Includes both direct contributions and contributions to values/conditions you need

Mutual Recognition = min(recognition in both directions)

* Ensures reciprocity in proportion
* Measures genuine mutual interdependence
* One-sided recognition doesn't create mutual relationship

### 1.3 Protocol Architecture

The coalition operates primarily through computational mechanisms, not governance:

**1. Membership Module** → Determines who participates (MRD ≥ threshold)  
**2. Resource Allocation** → Allocates both external capacity and internal proceeds  
**3. Decider** → Tunes parameters (rarely, weighted by MRD)

No committees. No appointments. No grant applications. Pure computation from recognition patterns.

**Protocol Specification:** playnet.gitbook.io/docs/protocol  
**Protocol Parameters:** See protocol-parameters.md for detailed parameter definitions

***

## II. Participation Model

### 2.1 Publishing and Derivation

**The key insight is separating:**

1. **Publishing** (what is, what I have/need)  
2. **Derivation** (what we can infer collectively)

**Sovereignty and Interoperability:**  
Participants retain full control over their own data, recognitions, and priorities. They choose whose data to subscribe to. The system enables collaboration without requiring surrender of autonomy.

**Automation of Cooperation:**  
A significant portion of capacity/resource allocation (funding, technical support, proceeds) is automatically derived based on the state of network data, drastically reducing transaction costs and delays.

### 2.2 What Participants Publish

* **Recognition of CONTRIBUTIONS**: Percentage allocations showing who/what enables your self-actualization (sum = 100%)
* **Declaration of NEEDS**: What you require (means of production, common needs, individual consumption) (with time/location/quantity slots)
* **Capacities**: Resources you can provide to support others (if you're a provider)
* **Labor Output**: Work products, services, value created through collective labor
* **Filters**: Compliance, legal, or mission-based constraints on allocations
* **Organizational Membership**: Which collectives you participate in
* **Environmental Data**: Context, conditions, qualities relevant to coordination
* **Sources for Derivation**: Which published data you subscribe to and trust

**Critical:** Recognition is about CONTRIBUTIONS (who enables you). Needs are about REQUIREMENTS (what you need). These are separate data types in your tree.

### 2.3 What Participants Derive

From local and network-published data, participants/protocols automatically derive:

* **Mutual Recognition**: min(A→B recognition, B→A recognition)
* **Mutual Recognition Score (MRS)**: Sum of all mutual recognitions for a participant
* **Mutual Recognition Density (MRD)**: MRS relative to network average
* **Membership Status**: Whether MRD ≥ threshold (computational, automatic)
* **Collective Recognition Shares**: Each member's share of mutual recognition within a provider's capacity set
* **Allocations**: Resources distributed based on recognition + needs + capacity + filters
* **Proceeds Distribution**: Internal revenue/output allocated to members and common funds
* **Organizational Recognition**: Collective's aggregate mutual recognition patterns
* **Goals and Estimates**: Projections based on current recognition and capacity patterns

***

## III. Common Ownership of Proceeds

### 3.1 The Total Social Product

**When members carry on work with means of production in common, the proceeds of labor belong to the collective.**

The cooperative proceeds of labor are the **total social product**: all revenue, output, value, and resources created through collective activity.

This includes:
* Revenue from services rendered
* Products created and sold
* Intellectual property and knowledge goods
* Capacity built through collective effort
* Network effects and reputation value
* Any other form of collectively-generated value

### 3.2 Computational Allocation of Proceeds

**The same recognition-based protocols that allocate external capacity also allocate internal proceeds.**

**The collective tree IS the allocation structure.**

Members DECLARE needs. That's it.
Members RECOGNIZE each other's CONTRIBUTIONS (labor, enabling work).

Declared needs form a **collective tree**. The MS formula allocates proceeds TO declared needs BASED ON contribution recognition.

**No predetermined categories. No abstract stages. Categories emerge from practice.**

#### How It Works: Needs and Recognition

```
Members DECLARE needs as they emerge from practice:

- "Replace workshop lathe: $12,000" (Alice declares)
- "Healthcare fund: ongoing" (collective entity declares)
- "Alice's groceries, rent: $2,500/month" (Alice declares)
- "New water filtration: $15,000" (Bob declares)
- "Children's education: ongoing" (Charlie declares)
- "Bob's food, housing: $2,800/month" (Bob declares)
- "Emergency reserve: 5% of proceeds" (collective entity declares)
- "Protocol maintenance: ongoing" (collective entity declares)
- "Charlie's subsistence: $2,200/month" (Charlie declares)
- "Elder care: ongoing" (Dave declares)

ALL declared needs become nodes in collective tree.
NO predetermined categories.
NO tagging required.
```

**Two separate flows:**

```
RECOGNITION OF CONTRIBUTION (what you give):
- Alice recognizes Bob's work: 30% (Bob enables Alice)
- Bob recognizes Alice's work: 25% (Alice enables Bob)
- Charlie recognizes Alice: 20%, Bob: 25%, etc.
- This is your 100% recognition allocation to contributors
- Creates Mutual Recognition (MR) scores
- Used in MS formula as provider-side weight

DECLARATION OF NEED (what you receive):
- Alice declares: "I need new lathe ($12K) + groceries/rent ($2.5K/month)"
- Bob declares: "I need water filtration ($15K) + food/housing ($2.8K/month)"
- These are nodes in collective tree (recipient-side)
- MS formula allocates TO needs BASED ON declarer's contribution recognition

MS(collective, need) = MR(collective, declarer) × need_share
Allocation(need) = proceeds × (MS / Σ_all_MS)
```

**Categories emerge from practice:**

After observing patterns, you might notice:
- Some needs cluster around tools/infrastructure
- Some around healthcare/education/welfare
- Some around individual subsistence

But you don't need to decide this upfront. Members declare needs organically. The collective tree reveals patterns naturally. You can query/filter by observed patterns if useful, but categories aren't imposed.

**Constraints:** Minimum individual allocation ensures everyone receives at least subsistence, regardless of contribution recognition. Paid first, remainder distributed by MS formula.

**CRITICAL DISTINCTION: Two Separate Data Types**

```
DATA TYPE 1: RECOGNITION (of contributions)
  Purpose: Measure who/what enables your self-actualization
  Format: Percentage allocation (sum = 100%)
  Example: "I recognize Alice's work: 30%, Bob's tools: 25%, Carol's mentoring: 20%"
  Used for: Calculating MR (Mutual Recognition) and membership (MRD)
  
  In protocolv6.mmd:
    recognition: {Carol: 30%, Kitchen: 20%}  ← recognizing CONTRIBUTIONS
    
DATA TYPE 2: NEEDS (declarations)
  Purpose: State what you require to function/live
  Format: Resource quantity with time/location slots
  Example: "I need $2,500/month for groceries/rent" or "I need new lathe"
  Used for: Creating nodes in collective tree that receive allocation
  
  In protocolv6.mmd:
    needs: {food: 100}  ← declaring NEEDS

THE MS FORMULA CONNECTS THEM:
  MS(collective, Alice) = MR(collective, Alice) × Alice_share
  
  Where:
  - MR based on Alice's CONTRIBUTION recognition by collective
  - Allocation flows TO Alice's declared NEEDS
  - BASED ON her contribution recognition
  
YOU DO NOT "recognize Alice's need for toothpaste" ❌
YOU RECOGNIZE "Alice's contributions to collective" ✓
ALICE DECLARES "I need $2,500 for groceries/toothpaste/rent" ✓
ALLOCATION flows to Alice's declared needs, amount determined by contribution recognition
```

**Key insight:**  
"What the producer is deprived of in his capacity as a private individual benefits him directly or indirectly in his capacity as a member of society."

**Key insight:**  
"What the producer is deprived of in his capacity as a private individual benefits him directly or indirectly in his capacity as a member of society."

**Emergent vs. Predetermined Allocation:**

```
❌ TRADITIONAL BUDGETING (Top-Down):
   1. Committee votes: "40% to capital, 20% to common, 40% to wages"
   2. Categorize every expense into predetermined buckets
   3. Within each bucket, negotiate specific allocations
   4. Requires constant governance and adjustment

✓ EMERGENT ALLOCATION (Bottom-Up):
   1. Members DECLARE needs organically (no categories)
   2. Members RECOGNIZE contributions (who/what enables them)
   3. Declared needs form collective tree
   4. MS formula allocates TO needs BASED ON declarer's contribution recognition
   5. Patterns emerge from what people actually declare
   6. NO VOTES on percentages
   7. NO predetermined categories
   
If collective needs more healthcare:
   - Traditional: Call meeting, debate, vote on budget increase
   - Emergent: Declare healthcare need → enters tree → MS formula allocates

The collective tree IS the budget. Declared needs ARE the allocation structure.
Categories emerge from observation, not imposition.
```

See **protocol-parameters.md** for detailed parameters (filtering, minimums, convergence).

### 3.3 Two Phases of Distribution

**Phase 1: Bourgeois Right (Current Implementation)**

```
All needs allocated via MS formula:

MS(collective, need) = MR(declarer) × need_share

Where:
- MR = Mutual Recognition based on declarer's CONTRIBUTION
  (Members recognize each other's contributions to collective)
  
- need_share = Proportion based on contribution recognition
  (Initially by recognition, later weighted by satisfaction)

- Allocation goes TO declared NEEDS (whatever they are)

Key mechanism:
1. Alice CONTRIBUTES labor to collective
2. Other members RECOGNIZE Alice's contributions (30%, 25%, etc.)
3. Alice DECLARES needs (lathe $12K, groceries/rent $2.5K/month)
4. MS formula allocates TO Alice's declared needs BASED ON contribution recognition

NOT: "We recognize Alice's needs" ❌
BUT: "We recognize Alice's contributions" → allocation flows to her declared needs ✓

The "bourgeois right" aspect:
- Allocation proportional to CONTRIBUTION (measured via MR)
- Higher contribution recognition = higher allocation to your declared needs
- "Equal standard (contribution) applied to unequal individuals"
- Labor is still the measure (not pure need-based allocation yet)
- Applies to ALL needs (tools, healthcare, consumption - no categories)
```

**Phase 2: From Each According to Ability, To Each According to Needs (Future Evolution)**

```
When productive forces have sufficiently developed:
- Recognition still measures CONTRIBUTION (for membership and coordination)
- But allocation NO LONGER proportional to contribution
- Declared NEEDS determine allocation directly (any needs, no categories)
- "Narrow horizon of bourgeois right" transcended
- Requires abundance: "springs of co-operative wealth flow more abundantly"

Protocol transition:
- Phase 1: MS formula allocates based on contribution recognition
  Allocation ∝ MR (contribution-based)
  
- Phase 2: Allocation based purely on declared need
  Allocation ∝ Declared_Need (need-based)
  Recognition still used for membership (MRD ≥ threshold)
  
- Transition: Gradually shift weight in allocation formula
  Allocation = α × MR_share + (1-α) × Need_share
  As abundance grows: α → 0 (pure need-based)
  
- Applies to ALL needs uniformly (no special treatment by category)
```

The protocol enables this transition computationally  -  no constitutional change needed, just parameter adjustment via Decider.

**Important:** Phase 1→2 affects the weighting formula (contribution vs pure need). The collective tree structure (what needs people declare) continues to determine what gets allocated in BOTH phases. No categories required in either phase.

### 3.4 Proceeds Allocation Example

```
Collective monthly revenue: $100,000 (total social product)

Members DECLARE needs organically (no categories):

COLLECTIVE TREE (all declared needs):
1. Alice declares: "Replace workshop lathe: $12,000 one-time"
2. Bob declares: "New water filtration system: $15,000 one-time"
3. Collective declares: "Emergency risk reserve: $5,000"
4. Charlie declares: "Healthcare fund (ongoing)"
5. Dave declares: "Children's education program (ongoing)"
6. Collective declares: "Welfare fund for unable-to-work"
7. Collective declares: "Protocol maintenance / admin"
8. Alice declares: "My groceries/rent: $2,500/month"
9. Bob declares: "My food/housing: $2,800/month"
10. Charlie declares: "My subsistence: $2,200/month"
11. Dave declares: "My rent/food: $2,400/month"
12. Eve declares: "My groceries/utilities: $2,600/month"

NO predetermined categories. Just needs declared by members.

CONTRIBUTION RECOGNITION (separate flow):
Members recognize each other's contributions:
- Alice: MRS = 0.28 (others recognize Alice's work)
- Bob: MRS = 0.22 (others recognize Bob's work)
- Charlie: MRS = 0.19
- Dave: MRS = 0.18
- Eve: MRS = 0.13
Total MRS = 1.0

MS FORMULA ALLOCATION (Total = $100,000):

Allocations TO declared needs BASED ON declarer's contribution recognition:

Alice's declared needs:
- Lathe (Alice declarer, MRS=0.28): $100K × 12% = $12,000
- Groceries/rent (Alice, MRS=0.28): $100K × 7% = $7,000
Alice total: $19,000

Bob's declared needs:
- Water filtration (Bob, MRS=0.22): $100K × 15% = $15,000  
- Food/housing (Bob, MRS=0.22): $100K × 6% = $6,000
Bob total: $21,000

Charlie's declared needs:
- Healthcare fund (Charlie, MRS=0.19): $100K × 12% = $12,000
- Subsistence (Charlie, MRS=0.19): $100K × 4% = $4,000
Charlie total: $16,000

Dave's declared needs:
- Education (Dave, MRS=0.18): $100K × 8% = $8,000
- Rent/food (Dave, MRS=0.18): $100K × 4% = $4,000
Dave total: $12,000

Eve's declared needs:
- Groceries/utilities (Eve, MRS=0.13): $100K × 3% = $3,000

Collective entity's declared needs:
- Risk reserve: $100K × 5% = $5,000
- Welfare fund: $100K × 5% = $5,000  
- Admin: $100K × 3% = $3,000

TOTAL: ~$100,000 allocated across all declared needs
```

**Key properties:**
* NO predetermined categories  -  members declare needs organically
* NO predetermined percentages  -  proportions emerge from what's declared
* Observed patterns: ~32% tools/infrastructure, ~28% common funds, ~40% individual consumption
  (But these are OBSERVED, not imposed)
* Members DECLARE needs (whatever they need)
* Members RECOGNIZE contributions (separate flow)
* MS formula: MS(collective, need) = MR(declarer) × need_share
* Allocation BASED ON contribution recognition, FLOWS TO declared needs
* If next month more tool needs declared → tool allocation increases automatically
* If fewer individual needs declared → individual allocation decreases automatically
* Collective tree structure determines proportions dynamically
* MS formula allocates across ALL nodes as one unified tree
* Categories emerge from practice, not from predetermined frameworks
* Fully transparent and computational

**Critical distinction preserved:**
- Recognition = CONTRIBUTION recognition (who enables you, 100% allocation)
- Needs = DECLARED requirements (what you need, nodes in tree)
- MS formula = allocates TO needs BASED ON contribution recognition
- You don't "recognize needs," you DECLARE your needs and RECOGNIZE contributions

***

## IV. Implementation Patterns

### 4.1 Administrative Structures

The protocols are **implementation-agnostic**. Any administrative structure can wrap them:

**A. Incorporated Entity** (e.g., Swiss Verein, US 501(c)(3), UK CIC)

* Own legal personality and bank account
* Admins selected by MRD ranking (opt-in model)
* Constitutional documents reference protocols
* See: playnet.gitbook.io/docs/organizations/swiss-association

**B. Fiscally Hosted Collective** (e.g., via Open Collective Europe, Social.coop)

* No own legal status (uses host's)
* No own bank account (host holds funds)
* Admins approve expenses via platform
* Operating guidelines reference protocols
* See: playnet.gitbook.io/docs/organizations/open-collective

**C. Cooperative Entity** (e.g., Platform Cooperative, Worker Cooperative)

* Member-owned legal structure
* Shares/membership certificates based on MRD
* Dividends/patronage refunds computed by protocol
* Voting replaced by recognition-based Decider

**D. DAO/Smart Contract** (future)

* On-chain execution
* Automated compliance via oracles
* No traditional admins (code executes)

**E. Network of Entities**

* Multiple administrative wrappers coordinating
* Each entity implements protocols independently
* Inter-entity recognition and capacity flows
* Federated coalition model

### 4.2 Core Requirements

Regardless of implementation, all structures need:

1. **Membership Registry**: MRD computation → current member list
2. **Recognition Tracking**: Member recognitions → mutual recognition patterns
3. **Proceeds Accounting**: Track total social product and allocation stages
4. **Resource Tracking**: External capacity + internal proceeds → allocation instructions
5. **Execution Mechanism**: Admins or automated system executes transfers
6. **Compliance Layer**: Filters applied before allocation computation
7. **Transparency**: All recognition patterns, allocations, and proceeds visible

### 4.3 Generic Operational Flow

```
1. Members submit recognition data (via protocol interface)
   → Protocol computes MRD scores weekly
   → Membership list updated automatically

2. Collective produces value through labor
   → Track revenue/output (total social product)
   → Members declare needs (tools, healthcare, consumption, etc.)
   → Protocol allocates to all declared needs via MS formula
   → No sequential stages - unified allocation across all needs

3. Members declare needs (via protocol interface)
   → All needs visible to network
   → Needs inform allocation adjustments

4. External providers declare capacities (optional)
   → Capacity includes: set of members, total amount, type
   → Protocol computes collective-recognition-shares within set
   → Provider allocates to needs

5. Protocol generates allocation instructions
   → Based on: recognition shares + needs + capacity + filters
   → Output: Transfer instructions to admins

6. Admins execute (mechanical, no discretion)
   → Verify protocol authenticity
   → Execute transfers via available mechanism
   → Record all transactions

7. Transparency (continuous)
   → All allocations visible with rationale
   → Public or member-visible proceeds allocation
   → Recognition patterns observable
   → Full audit trail
```

### 4.4 Admins Are Executors, Not Governors

**Critical principle:** Admins never decide allocations.

**What admins DO:**

* Sign bank transfers as instructed by protocol
* Execute transfers from collective accounts
* File required legal/tax reports  
* Maintain bank account access
* Verify protocol computation authenticity

**What admins NEVER do:**

* Decide membership (protocol computes MRD)
* Decide resource allocation (protocol computes)
* Decide proceeds distribution (protocol computes)
* Override compliance filters (compliance service determines)
* Make discretionary decisions
* Set strategy (emerges from member activity)

Admins are selected by MRD ranking (opt-in model): highest-ranked members offered admin positions. If they decline, offer goes to next highest. No elections, no appointments.

***

## V. Compliance and Filtering

### 5.1 Compliance Responsibility

**Each provider (including collectives distributing their own proceeds) is wholly responsible for implementing compliance filters.**

Filters ensure legal/regulatory compliance:

* AML/KYC verification (Know Your Customer)
* Sanctions screening (OFAC, UN, EU lists)
* Jurisdiction transfer limits
* Tax compliance requirements
* Mission/purpose alignment

### 5.2 Computational Filtering

```
Filter(Member, Capacity) = Maximum amount allocable to Member from this Capacity

Values:
- $0 = Cannot allocate (sanctions, KYC failed)
- $X = Cap (jurisdiction limits, risk levels)  
- Unlimited = No restrictions

Actual Allocation = min(
  Recognition-Share × Total-Capacity,
  Filter(Member, Capacity),
  Member-Declared-Need
)

Members hitting filter limit receive up to that limit.
Unallocated capacity redistributes to other members by recognition shares.
```

### 5.3 Compliance Service Integration

```
Independent compliance service provider:

1. Performs KYC verification for all members
2. Screens against sanctions lists daily
3. Determines jurisdiction transfer limits
4. Maintains Filter(Member) for each member
5. Updates protocol when status changes
6. Provides audit trail

Service is independent of admins (no admin override possible).
Protocol applies filters computationally before generating transfer instructions.
```

### 5.4 Union of Filters

When external providers use collective as executor, BOTH filters apply:

```
Effective-Filter(Member) = min(
  Provider-Filter(Member),
  Collective-Filter(Member)
)

Most restrictive filter wins. Both must allow allocation.
```

***

## VI. Decision Making: The Decider

### 6.1 When to Use Decider

**Rarely. Only for:**

* Setting membership threshold (default 0.5)
* Setting minimum recognition level (default 0%)
* Setting computation frequency (default weekly)
* Adjusting safety constraints (e.g., minimum subsistence floor, minimum means-of-production warning threshold)
* Tuning filtering parameters (minimum quorum, recognition threshold for allocation)
* Constitutional/structural changes
* Transitioning from Phase 1 to Phase 2 distribution model (need-weighting)

**Not for:**

* Resource allocation (use Collective Recognition via MS formula)
* Membership decisions (use MRD computation)
* Daily operations (automatic)
* Project priorities (emerge from recognition patterns)
* Proceeds distribution (automatic via protocol)
* Need categories or proportions (emerge from declared needs, NOT set by vote)

### 6.2 Decider Process

**Participants:** Only current members (MRD ≥ threshold)

**Weighted by Recognition:** Support points × MRD score

**Phases:**

1. Proposal phase (1 week)
2. Challenge phase (1 week)
3. Discussion phase (1 week)
4. Improvement phase (1 week)
5. Support distribution phase (1 week)
6. Result computation (weighted support)
7. Implementation (next computation cycle)

**Specification:** playnet.gitbook.io/docs/decider/decider

### 6.3 Example: Adjusting Minimum Subsistence Floor

```
Current parameter:
- MINIMUM_INDIVIDUAL_ALLOCATION_USD = $2,000/month

Member proposes:
"Increase minimum to $2,500/month"
Rationale: "Cost of living has increased 20% in our region, current floor 
no longer covers basic subsistence for members"

Decider process:
- Proposal phase: 3 alternatives proposed ($2,500, $2,300, or keep $2,000)
- Challenge phase: Members discuss collective financial capacity
- Discussion phase: Data shared on actual costs and current proceeds trends
- Improvement phase: Proposal refined to $2,400 with cost-of-living indexing
- Support phase: Each member distributes support points × their MRD
- Result: Winning proposal determined by weighted support (66% threshold)
- Implementation: Protocol applies new minimum starting next distribution cycle

NOTE: This adjusts a safety constraint, NOT the Stage proportions.
Stage proportions emerge from collective tree recognition patterns.
If members want more common healthcare, they recognize healthcare needs more heavily.
No vote needed  -  just update recognition in personal trees.
```

***

## VII. Coalition Network Effects

### 7.1 Multiple Collectives Coordinating

**Coalition = Network of collectives, all using same protocols**

Each collective:

* Has its own members (determined by MRD within that collective)
* Allocates its own proceeds (based on collective recognition within)
* Declares external capacities to support other collectives
* Can recognize contributions from members of other collectives

**Inter-collective recognition:**

```
Alice is member of Collective A (water infrastructure)
Bob is member of Collective B (agriculture)

Alice recognizes Bob: 8% (Bob's agricultural work enables Alice's goals)
Bob recognizes Alice: 10% (Alice's water systems enable Bob's goals)

Mutual recognition: min(8%, 10%) = 8%

This cross-collective recognition:
- Doesn't make Alice a member of Collective B (MRD computed within collectives)
- But DOES enable Collective A to declare capacity for Bob's needs
- And DOES enable Collective B to declare capacity for Alice's needs
- Creates coordination pathways across collectives
```

**Coalition intelligence emerges from:**

* Multiple collective perspectives (different organizational lenses)
* Multiple capacity declarations (collectives supporting each other)
* Shared recognition patterns (visibility across collectives)
* Resource flows guided by inter-collective recognition
* No central coordination needed

### 7.2 Prismatic Coordination

**Same individuals, different collective contexts:**

```
Alice participates in:
- Collective A (water infrastructure)
- Collective B (education programs)
- Collective C (community organizing)

Alice's MRD computed independently in each:
- Collective A: MRD = 1.2 (high recognition, member)
- Collective B: MRD = 0.8 (medium recognition, member)
- Collective C: MRD = 0.4 (low recognition, not yet member)

Alice receives individual consumption allocations from:
- Collective A proceeds: $2,400/month (28% collective recognition share)
- Collective B proceeds: $1,800/month (22% collective recognition share)
Total: $4,200/month

Alice's labor distributed across multiple collectives.
No single collective "employs" Alice. Her self-actualization spans multiple contexts.
```

### 7.3 Coalition-Wide Capacity Pools

**Large providers can declare capacity for coalition-wide sets:**

```
Foundation X declares:
  Set: {All members across Collectives A, B, C, D, E}
  Capacity: $500K for "Bioregional Water Infrastructure"

Foundation X:
- Sees coalition-wide mutual recognition patterns
- Calculates collective-recognition-shares across entire coalition
- Allocates to highest-recognition members regardless of which collective they're in
- Creates coalition-level coordination through its capacity lens

No central coalition governance needed.
Provider capacity declarations create the coordination layer.
```

### 7.4 Emergence Without Central Control

**Roles emerge naturally:**

* Protocol tracks recognition by type/tag
* Dashboard shows who's most recognized for what across coalition
* No appointments needed
* Roles visible from recognition patterns

**Priorities emerge naturally:**

* Needs with high collective-recognition-shares = high priority
* Needs receiving multiple provider allocations = high priority
* No planning committee needed

**Quality emerges naturally:**

* High quality work → high recognition → higher allocations
* Low quality work → low recognition → lower allocations
* No quality review board needed

**The coalition is self-organizing intelligence based on mutual recognition of enabling contributions.**

***

## VIII. Properties and Guarantees

### 8.1 What the Coalition Eliminates

* ❌ Membership approval processes
* ❌ Resource allocation committees  
* ❌ Grant application reviews
* ❌ Hiring/firing decisions
* ❌ Salary negotiations
* ❌ Performance reviews
* ❌ Governance meetings (except rare Decider)
* ❌ Budget planning sessions
* ❌ Voting on percentage allocations
* ❌ Arbitrary funding targets
* ❌ Centralized planning
* ❌ Giver/receiver power dynamics
* ❌ Wage labor relations
* ❌ Manager/worker hierarchies

### 8.2 What the Coalition Guarantees

* ✓ Membership emerges from contribution and recognition
* ✓ Proceeds distributed by collective recognition + needs
* ✓ Success amplifies through recognition increase
* ✓ Failure corrects through recognition decrease
* ✓ Transparent and auditable
* ✓ Scale-invariant (works from 5 to 5000 participants)
* ✓ Sybil-resistant
* ✓ Self-organizing around real contribution
* ✓ Common ownership of means of production
* ✓ Common ownership of proceeds of labor
* ✓ Voluntary association (free to join, leave, form new collectives)
* ✓ Computational rather than political coordination

### 8.3 Self-Correcting Properties

* Phantom capacity eliminated (only providers with actual resources declare)
* False recognition hurts your own self-actualization (wrong allocation)
* Success increases recognition (enabling others → they recognize you more)
* Failure decreases recognition (not enabling → recognition drops)  
* Stop contributing → lose membership (MRD drops below threshold)
* Truth-telling is optimal strategy (accurate recognition optimizes outcomes)

### 8.4 Scaling Properties

* **Fixed individual complexity**: Always manage 100% recognition (doesn't increase with network size)
* **Distributed computation**: No central processor bottleneck
* **Natural clustering**: Sub-networks form around enabling relationships
* **Multiple coordination layers**: Provider capacities create high-level coordination
* **Network effects increase with scale**: More collectives → more coordination opportunities

***

## IX. Joining the Coalition

### 9.1 For Individuals

```
1. Find an existing collective using Free Association protocols
2. Begin contributing and building relationships
3. Recognize others' contributions (allocate your 100%)
4. Others recognize your contributions
5. Protocol computes your MRD weekly
6. When MRD ≥ threshold → automatic membership
7. Begin receiving allocations from collective proceeds

No applications, no approvals, no interviews.
Membership emerges from mutual recognition.
```

### 9.2 For Existing Organizations

```
1. Study protocol specifications (playnet.gitbook.io/docs)
2. Choose administrative wrapper (incorporated, hosted, cooperative, etc.)
3. Adopt constitutional documents referencing protocols
4. Deploy protocol implementation (software + compliance service)
5. Seed initial members (bootstrap recognition network)
6. Begin weekly MRD computations
7. Allocate proceeds via protocol
8. Publish your capacity declarations to wider coalition network

Your organization becomes a node in the coalition.
No coalition approval needed.
Implement protocols, begin coordinating.
```

### 9.3 For Providers/Funders

```
1. Identify collectives in coalition using protocols
2. Declare capacity: specify set of members you want to support
3. Protocol shows collective-recognition-shares within your set
4. Review member needs
5. Allocate your capacity based on recognition + needs + your judgment
6. Choose execution path:
   - Direct transfers (you send funds directly)
   - Via collective admin (collective executes your allocations)
7. Track outcomes and adjust future capacity declarations

You become a provider node in coalition.
Your capacity declarations create coordination.
No coalition governance to navigate.
```

### 9.4 Coalition Membership

**There is no "coalition membership" separate from collective membership.**

Coalition = set of all collectives implementing Free Association protocols

You participate in coalition by:

* Being a member of at least one collective (MRD ≥ threshold in that collective)
* Recognizing contributions across collectives
* Declaring capacities/needs visible to coalition network
* Allocating via protocol

Coalition is not an organization. It's a coordination protocol.

***

## X. Technical Integration

### 10.1 Data Publishing Standards

All participants publish data in compatible formats:

```typescript
// Recognition
{
  fromId: string,
  toId: string, 
  percentage: number, // 0-100, sum = 100
  tags: string[], // ["water stewardship", "education"]
  timestamp: Date
}

// Need
{
  id: string,
  name: string,
  unit: string, // "$", "hours", "acres"
  declarer_id: string,
  need_slots: NeedSlot[], // time/location/quantity
  status: "open" | "partially-fulfilled" | "fulfilled"
}

// Capacity (Provider)
{
  providerId: string,
  setOfPeople: string[],
  capacityType: string,
  totalAmount: number,
  availability_slots: AvailabilitySlot[],
  filters: Record<string, ComplianceFilter>
}

// Proceeds (Collective)
{
  collectiveId: string,
  period: {start: Date, end: Date},
  totalSocialProduct: number,
  collective_tree: CollectiveTree, // merged tree from all members
  allocations_by_node: Record<nodeId, {
    category: "means-of-production" | "common-needs" | "individual-consumption",
    collective_recognition: number,
    ms_allocation: number,
    final_allocation: number
  }>,
  stage_totals: {
    means_of_production: number,
    common_needs: number, 
    individual_consumption: number
  } // emergent totals, not predetermined
}
```

### 10.2 Protocol Implementation

Reference implementation: playnet.gitbook.io/docs/implementation

**Core modules:**

1. **Membership Module**: Computes MRD from recognition data
2. **Collective Tree Module**: Merges personal trees into collective tree (see collective-tree.svelte.ts)
3. **Resource Allocation Module**: Applies MS formula to allocate capacity + proceeds
4. **Decider Module**: Structured parameter adjustment
5. **Compliance Module**: Applies filters to allocations
6. **Transparency Module**: Publishes all data for auditability

**Key functions (from collective-tree.svelte.ts):**

* `mergeContributorTrees()`: Merges individual recognition trees into collective tree
* `nodeRecognitionToDistribution()`: Converts collective recognition to allocation shares
* `allocateFromCollectiveTree()`: Executes allocation via MS formula
* `governAndAllocate()`: Complete pipeline from trees to allocations

**Computation schedule:**

* Weekly: MRD computation, membership updates
* Monthly: Collective tree merge, proceeds distribution via MS formula
* As-needed: Decider sessions

### 10.3 Interoperability

**Participants choose whose data to subscribe to:**

```
Alice subscribes to:
- Collective A's member list (trusts their MRD computation)
- Foundation X's capacity declarations (trusts their allocations)
- Bob's individual recognitions (trusts Bob's judgment)
- Coalition-wide need declarations (wants visibility)

Alice does NOT subscribe to:
- Collective Y's data (doesn't trust their process)
- Foundation Z's data (misaligned values)

Alice's protocol instance:
- Computes only from subscribed data sources
- Derives recognition patterns from trusted nodes
- Makes allocation decisions based on filtered view
- Publishes own data for others to subscribe to
```

**No central registry required. Participants curate their own data sources.**

***

## XI. Summary

### 11.1 The Core Model

**Free Association Coalition enables:**

1. **Common ownership of means of production** → Collective holds tools, infrastructure, resources
2. **Common ownership of proceeds of labor** → Revenue/output belongs to collective
3. **Computational allocation** → Recognition protocols distribute both external capacity and internal proceeds
4. **Voluntary association** → Free to form, join, leave collectives
5. **No wage labor** → No employer/employee relation, members receive based on recognition + needs
6. **No governance overhead** → Computation replaces committees, voting, bureaucracy
7. **Distributed coordination** → Multiple collectives coordinate via recognition and capacity flows
8. **Self-actualization economy** → Resources flow to what enables mutual self-actualization

### 11.2 How It Works

```
Individual Level:
- I recognize CONTRIBUTIONS that enable my self-actualization (allocate 100%)
  * Who/what enables me to actualize my potential
  * This determines Mutual Recognition (MR) and membership
  
- I declare NEEDS organically (whatever I actually need):
  * Tools, infrastructure, equipment
  * Healthcare, education, welfare
  * Groceries, toothpaste, rent
  * NO predetermined categories - just needs
  
- I contribute labor to collective(s)

- I receive proceeds via MS formula:
  * MS(collective, my_needs) = MR(me) × need_share
  * MR based on others recognizing MY contributions
  * Allocation flows TO my declared needs
  * BASED ON my contribution recognition, not need recognition

Collective Level:  
- Protocol computes membership (MRD ≥ threshold)
- Collective produces value through members' combined labor (total social product)
- Members declare needs, forming collective tree:
  * Nodes = all declared needs (no predetermined categories)
  * Each need has declarer (who declared it)
  * No predetermined percentages
- Protocol allocates proceeds via MS formula:
  * MS(collective, need) = MR(declarer) × need_share
  * Allocation(need) = proceeds × (MS / Σ_all_MS)
  * Patterns emerge from what people actually declare
  * Categories emerge from practice, NOT imposed
- Collective declares external capacities to support other collectives

Coalition Level:
- Inter-collective recognition creates coordination pathways
- Provider capacity declarations guide resource flows
- No central governance required
- Intelligence emerges from distributed recognition patterns
- Same MS formula operates across all scales
- Pure emergence - no frameworks imposed
```

### 11.3 The Transformation

**From:**

* Wage labor (workers sell labor-power to employers)
* Private ownership (individuals/corporations own means of production)
* Market coordination (prices + profit motive guide resources)
* Hierarchical governance (managers decide, workers execute)
* Charity/philanthropy (donors give to beneficiaries)
* Budgeting processes (committees vote on percentage allocations)

**To:**

* Associated labor (freely cooperating individuals)
* Common ownership (collectives own means of production + proceeds)
* Recognition-based coordination (mutual enabling guides resources)
* Computational governance (protocols compute from recognition patterns)
* Mutual self-actualization (everyone invests in what enables them)
* Emergent allocation (collective tree + MS formula, no predetermined budgets)

### 11.4 "The narrow horizon of bourgeois right"

**Phase 1 (current):**  
All needs allocated based on declarer's CONTRIBUTION recognition. Members recognize each other's CONTRIBUTIONS to the collective (labor, enabling work). MS formula allocates TO declared needs (any needs - tools, healthcare, consumption) BASED ON contribution recognition of who declared them. Those who contribute more (as measured by collective recognition) receive more allocation to their declared needs. "Equal standard (contribution) applied to unequal individuals."

**Phase 2 (future):**  
From each according to ability, to each according to needs. Allocation purely by declared NEED, not contribution. If you declare a need (any need), you receive allocation (if available). Recognition of contribution still determines membership (MRD) and coordination, but NOT allocation to needs. Requires abundance: "springs of co-operative wealth flow more abundantly."

The protocol enables this transition computationally. As productive forces develop:
- Phase 1: Allocation ∝ Contribution_Recognition (bourgeois right)
- Phase 2: Allocation ∝ Declared_Need (communist principle)
- Transition: Allocation = α × Contribution + (1-α) × Need, α → 0
- Recognition of contribution continues for membership in BOTH phases

**Crucial distinction:** Phase 1→2 changes the BASIS of allocation (contribution vs need), NOT the distinction between recognizing contributions vs declaring needs. You ALWAYS recognize contributions and declare needs. What changes is whether contribution recognition affects consumption allocation.

### 11.5 The Key Insight

**Traditional structures require governance because they assume:**

* Someone must decide who belongs
* Someone must decide who gets resources  
* Someone must decide budget allocations (% to capital, % to operations, % to wages)
* Someone must decide compensation levels
* Someone must monitor performance
* Someone must coordinate activity

**Free Association eliminates need for governance by recognizing:**

* Recognition patterns determine who belongs (MRD computation)
* Recognition patterns determine resource flows (collective tree + MS formula)
* Recognition patterns determine budget allocations (emergent from collective tree, not votes)
* Recognition patterns measure performance (recognition increases/decreases)
* Recognition patterns create coordination (capacity declarations + need matching)
* Proceeds flow computationally (no salary negotiations, no budget meetings, no discretion)

**The critical architectural innovation:** The collective tree structure means there are NO predetermined categories or percentages. Members declare needs organically. Members recognize contributions. The MS formula allocates TO needs BASED ON declarer's contribution. Patterns and categories emerge from practice, not from imposed frameworks. Budgets emerge from declared needs and contribution recognition, not from voting or committee decisions.

**Result:** An organization that operates through recognition of mutual enabling, with computational mechanisms handling all coordination (including budget allocation), and minimal administrative wrapper for legal compliance only.

***

## XII. Getting Started

### 12.1 Experiment and Learn

This framework is experimental. We expect it to evolve through use.

**Start small:**

* Form a collective with 5-10 initial members
* Begin recognizing each other's contributions
* Track one cycle of proceeds allocation (even if symbolic amounts)
* Observe recognition patterns and adjust
* Iterate on administrative wrapper as needed

### 12.2 Technical Support

**Protocol specifications:** playnet.gitbook.io/docs  
**Reference implementation:** github.com/freeassociation/protocol  
**Coalition registry:** coalition.freeassociation.coop (opt-in visibility)

### 12.3 Legal/Compliance Support

**Administrative wrapper guides:**

* Swiss Association: playnet.gitbook.io/docs/organizations/swiss-association
* Open Collective: playnet.gitbook.io/docs/organizations/open-collective
* Worker Cooperative: playnet.gitbook.io/docs/organizations/worker-coop
* Platform Cooperative: playnet.gitbook.io/docs/organizations/platform-coop

**Compliance services:**

* ComplyAdvantage (sanctions/AML)
* Onfido (KYC verification)
* Sumsub (identity verification)

### 12.4 Questions and Discussion

**Coalition coordination channels:**

* Matrix: #freeassociation:matrix.org
* Forum: discuss.freeassociation.coop
* Email: coalition@freeassociation.coop

**This framework is a living document. Contribute improvements via Decider process.**

***

**"Only when individuals are free to associate, and that association is based on common ownership and mutual recognition of enabling contributions, can the narrow horizon of bourgeois right be crossed in its entirety."**

***

**Version History:**

* v0.1.0 (Nov 18, 2025): Initial draft, basic publishing/derivation model
* v0.2.0 (Nov 29, 2025): Added common ownership of proceeds, Gotha Programme integration, complete coalition framework