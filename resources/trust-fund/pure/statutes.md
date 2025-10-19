# Statutes of [Name] Association

## Article 1 — Name and Seat

The Association "[Name]" (the "Association") is established under Articles 60 et seq. of the Swiss Civil Code. Its seat is in [City], Switzerland.

## Article 2 — Purpose

The Association exists to support and experiment with games/organizations that enable societal flourishing.

## Article 3 — Powers

In furtherance of its purpose, the Association may:
- Manage assets and reserves in any form, including digital assets
- Engage in commercial activities consistent with its purpose
- Utilize computational, algorithmic, or automated processes for coordination and governance
- Establish partnerships, collaborations, subsidiaries, or participate in other legal entities
- Issue internal coordination mechanisms or units
- Delegate operational authority to designated persons, services, or systems
- Employ any other lawful means consistent with its purpose

## Article 4 — Membership

**4.1 Determination**  
Membership is determined computationally. A person is a member when their Mutual Recognition Density (MRD) equals or exceeds the threshold value (currently 0.5).

**4.2 MRD Computation**  
MRD is computed weekly according to the Membership Module specification, available at [URL/reference].

**4.3 Member List**  
The current member list consists of all participants with MRD ≥ threshold as of the most recent computation.

**4.4 Exit**  
Membership ends automatically when MRD falls below threshold. Members may also exit by written declaration.

## Article 5 — Organs

The Association has two organs:

**5.1 General Assembly**  
Comprises all current members (MRD ≥ threshold).

**5.2 Board**  
Comprises three members with collective signature authority. Board positions are offered to members in order of highest MRD scores. If a member declines, the position is offered to the next highest MRD.

All Board members have equal authority. Roles exist only for legal requirements:
- One member designated as "President" (legal representative)
- All members are authorized signatories

Board composition updates when:
- MRD computation changes ranking, AND
- Current Board member declines to continue, OR
- Higher-ranked member accepts position

## Article 6 — General Assembly

**6.1 Powers**  
The General Assembly:
- Acknowledges computational results (membership, resource flows)
- Approves annual financial statements
- Adopts constitutional amendments via Decider process
- Supervises the Board

**6.2 Convening**  
Meets at least annually. All members may attend. Notice period: 14 days.

**6.3 Decisions**  
Decisions on constitutional matters follow the Decider process specification at [URL/reference]. Routine matters are determined by computational protocols.

## Article 7 — Board

**7.1 Minimal Responsibilities**  
Board members have only these duties:
- Sign bank transfers as instructed by protocol
- Sign legal documents as required by Swiss law
- File required reports with authorities
- Maintain bank account access

**7.2 What Board Does NOT Do**  
The Board does not:
- Decide membership (protocol computes MRD)
- Decide resource allocation (protocol computes allocations)
- Decide compliance filters (compliance service determines)
- Set strategy (emerges from member activity)
- Make discretionary decisions

**7.3 Signatory Authority**  
Any two Board members sign collectively. All Board members have equal signing authority.

**7.4 Compensation**  
Board service is voluntary. Reasonable expenses may be reimbursed.

**7.5 Liability Protection**  
Board members execute protocol instructions mechanically. They are not personally liable for allocation decisions made by the protocol.

## Article 8 — Compliance Services

**8.1 Compliance Requirement**  
The Association must comply with Swiss AML (Anti-Money Laundering) and KYC (Know Your Customer) laws.

**8.2 Compliance Service Provider**  
The Association engages a compliance service provider to:
- Verify member identities (KYC)
- Screen members against sanctions lists (OFAC, UN, EU)
- Determine jurisdiction transfer limits
- Maintain compliance filter data
- Update filters when compliance status changes

**8.3 Compliance Filters**  
The compliance service determines Filter(Member) for each member:
- $0 = Cannot receive funds (sanctions, KYC failure)
- $X = Maximum allocation (jurisdiction limits, risk caps)
- Unlimited = No restrictions

**8.4 Protocol Integration**  
The protocol applies compliance filters computationally. The Board does not override or modify compliance filters.

**8.5 Compliance Service Selection**  
The General Assembly may designate a compliance service provider via Decider process. The service must be independent of the Board.

## Article 9 — Resources

**9.1 Resource Allocation**  
Resources are allocated according to the Resource Allocation Protocol specification at [URL/reference].

**9.2 Needs and Allocations**  
Members declare needs. Providers declare capacities and allocate to needs. When the Association itself holds unrestricted funds, the protocol automatically allocates to member needs based on collective-recognition-shares and compliance filters. The Board executes approved transfers.

**9.3 Financial Records**  
All flows are recorded in a transparent ledger maintained per the protocol specification.

## Article 10 — Computational Protocols

**10.1 Protocol Specifications**  
The Association operates through three computational protocols:
- Membership Module (MRD computation)
- Resource Allocation Protocol (provider capacities and need matching)
- Decider Process (parameter adjustments)

**10.2 Protocol References**  
Current protocol specifications are maintained at [URL/repository] and may be updated via Decider process.

**10.3 Interface**  
The Board implements protocol outputs. Protocol computations are authoritative for membership and resource allocation.

## Article 11 — Dissolution

**11.1 Decision**  
Dissolution requires Decider process with supermajority (75% weighted support).

**11.2 Liquidation**  
Upon dissolution, assets are distributed to members pro-rata by collective-recognition-share (calculated across all members as of dissolution decision), or to organizations with similar purpose.

## Article 12 — Entry into Force

These Statutes enter into force upon adoption by the founding members.

---

**Adopted:** [Date]  
**Place:** [City]

**Founding Members:**  
[Signatures of at least 2 founding members]

---

## Implementation Notes

### Interface Points Between Statutes and Protocols

| Statute Article | Protocol/Service | Data Flow |
|-----------------|------------------|-----------|
| Art. 4.2 | Membership Module | Weekly MRD computation → Member list |
| Art. 5.2 | Membership Module | Weekly MRD ranking → Board position offers |
| Art. 6.3 | Decider Process | Constitutional proposals → Weighted decision |
| Art. 8.2 | Compliance Service | KYC/sanctions checks → Compliance filters |
| Art. 9.1 | Resource Allocation | Needs + Provider capacities + Filters → Transfer instructions |
| Art. 9.3 | Resource Allocation | All transactions → Ledger entries |

### Required Protocol Outputs (to Board)

**Weekly:**
- Current member list (names, MRD scores)
- Current MRD ranking (for Board position offers)

**As-needed:**
- Transfer instructions (from provider allocations with filters applied)
- Decider results (for constitutional changes)
- Compliance filter updates (from compliance service)

**Annually:**
- Financial statements (auto-generated from ledger)
- Membership changes summary

### Board Selection Process (Opt-In Model)

**Position offers go by MRD ranking:**
```
Week 1: MRD computation ranks all members
Week 2: Positions offered in order:
1. Highest MRD → Offered Board position
   - If accepts: Becomes Board member
   - If declines: Offer goes to next highest

2. 2nd highest MRD → Offered Board position
   - If accepts: Becomes Board member
   - If declines: Offer goes to next highest

3. Continue until 3 positions filled

Current Board members:
- Automatically offered to continue if still in top ranks
- Can decline to continue (position offered to next highest)
- Must explicitly accept to continue each term
```

**Term length:** Board positions reviewed quarterly after MRD computation.

**Example:**
```
MRD Ranking:
1. Alice (MRD: 1.8)
2. Bob (MRD: 1.5)
3. Charlie (MRD: 1.3)
4. Dave (MRD: 1.2)
5. Eve (MRD: 1.0)

Offers:
1. Alice offered → Accepts → Board Member 1
2. Bob offered → Declines (too busy)
3. Charlie offered → Accepts → Board Member 2
4. Dave offered → Accepts → Board Member 3

Final Board: {Alice, Charlie, Dave}
President designation: Alice (highest MRD of actual Board)
```

**Why Board members might decline:**
- Time constraints
- Travel commitments
- Conflict of interest
- Don't want legal signing responsibility
- Any personal reason

**No penalty for declining:** MRD score unaffected, full participation rights maintained.

### Compliance Service

**Independent service provider performs:**
```
1. Member onboarding:
   - KYC verification (identity documents)
   - Sanctions screening (OFAC, UN, EU lists)
   - Jurisdiction assessment
   - Risk evaluation

2. Ongoing monitoring:
   - Daily sanctions list updates
   - Quarterly re-verification
   - Status change notifications

3. Filter determination:
   For each member, compliance service sets:
   - Filter = $0 (cannot receive: sanctions, KYC failed)
   - Filter = $X (cap: jurisdiction limits, risk levels)
   - Filter = Unlimited (no restrictions)

4. Protocol integration:
   - Provides filter data to protocol
   - Protocol applies filters computationally
   - Board receives filtered allocations
   - Board never modifies filters
```

**Service characteristics:**
- Independent of Board (no Board override)
- Automated sanctions screening
- API integration with protocol
- Transparent filter logic
- Auditable compliance trail

**Example compliance services:**
- ComplyAdvantage (sanctions/AML screening)
- Onfido (KYC verification)
- Sumsub (identity verification)
- Custom service built on these APIs

**Key separation:**
- Compliance service determines who can receive (filters)
- Protocol determines who should receive (recognition)
- Board executes transfers (mechanical only)
- No single entity has full discretion

### Board's Minimal Role

The Board performs ONLY these mechanical tasks:
1. **Sign bank transfers:** As instructed by protocol (with compliance filters already applied)
2. **Sign legal documents:** As required by Swiss law
3. **File reports:** With Swiss authorities
4. **Maintain access:** To bank account

The Board does NOT:
- Decide who is a member (protocol does)
- Decide resource allocation (protocol does)
- Decide compliance filters (compliance service does)
- Set strategic direction (emerges from members)
- Make discretionary decisions
- Override protocol outputs
- Have fiduciary discretion

### Reconciling Distributed Decisions with Centralized Execution

**Key insight:** The Board never sees capacity declarations or needs. The Board only receives specific transfer instructions.

**The flow:**
```
Layer 1 - Distributed (No centralization):
Members declare needs (visible to all)
- Alice: "I need $50K for water infrastructure"
- Bob: "I need $30K for agriculture"

Providers declare capacities (only providers, with actual resources)
- Foundation X: "{Alice,Bob,Charlie,Dave,Eve} - I can provide $200K"
- Foundation Y: "{Bob,Charlie,Dave} - I can provide $100K"

Layer 2 - Provider Allocation (Distributed):
Each provider:
- Calculates collective-recognition-shares within their set
- Sees needs from members in their set
- Allocates based on recognition + needs + available capacity
- Foundation X: "I'll send $30K to Alice, $25K to Bob"
- Foundation Y: "I'll send $20K to Bob"

Layer 3 - Board Execution (Mechanical):
Board receives ONLY: "Transfer $30K to Alice, $45K to Bob"

Board does NOT see:
❌ Capacity declarations ($200K, $100K)
❌ Need amounts ($50K, $30K)
❌ Collective-recognition-shares
❌ Provider reasoning

Board ONLY sees:
✓ Specific transfer instructions
✓ Amounts and recipients
✓ That's it
```

**No centralization of capacities or needs.** Both remain distributed.

**What gets centralized:** Only the execution of specific transfers that already emerged from the distributed process.

**Board = Signing robot.** Receives final instructions, executes mechanically, sees nothing upstream.

### Resource Flow: Protocol to Board Interface

**The distributed protocol determines allocations. The Board executes approved transfers.**

**Flow:**
```
1. Members declare needs (visible to all)
2. Providers declare capacities (only providers, with actual resources)
3. For each provider capacity:
   - Protocol computes collective-recognition-shares within provider's set
   - Provider sees needs from members in their set
   - Provider allocates based on recognition + needs + capacity
4. Providers allocate to needs:

   Path A: Direct transfer (provider to recipient)
   - Provider has funds outside Verein
   - Provider transfers directly to recipient
   - No Board involvement
   
   Path B: Verein transfer (provider instructs Board)
   - Provider contributed funds TO Verein
   - Provider allocation = instruction to Board
   - Board executes transfer from Verein account

5. Board receives transfer instructions from protocol
6. Board executes authorized transfers
```

**Example:**
```
Alice declares need: $50K for water infrastructure

Foundation X declares capacity:
  Set: {Alice, Bob, Charlie}
  Total: $200K via Verein
Foundation X allocates: $30K to Alice

Individual Y declares capacity:
  Set: {Alice}
  Total: $5K direct
Individual Y allocates: $5K to Alice (direct transfer)

Foundation Z declares capacity:
  Set: {Alice, Bob}
  Total: $10K direct
Foundation Z allocates: $10K to Alice (from their account)

Board sees:
- Protocol output: "Transfer $30K to Alice (from Foundation X allocation)"
- Board verifies: Foundation X contributed $100K to Verein
- Board executes: Bank transfer $30K to Alice
- Board records: Transaction in ledger

Board does NOT see or approve:
- Individual Y's direct $5K transfer
- Foundation Z's direct $10K transfer
These bypass Verein entirely (peer-to-peer)
```

**Key separation:**
- **Protocol authority:** Determines who should receive what (via collective recognition)
- **Board execution:** Implements transfers from Verein funds only
- **Direct transfers:** Providers can respond outside Verein (peer-to-peer)

**Board's limited scope:**
```
Board CAN:
- Execute transfers authorized by protocol
- Verify provider has deposited funds
- Maintain accounting records
- Ensure legal compliance

Board CANNOT:
- Decide who gets resources (providers decide)
- Override protocol allocations
- Prioritize one need over another
- Withhold authorized transfers
```

**Provider's choice:**
```
When Foundation X contributes $100K to Verein:
1. Foundation declares capacity for members
2. Protocol shows collective-recognition-shares within Foundation's set
3. Foundation sees needs from members in their set
4. Foundation allocates: $30K to Alice
5. Foundation instructs Verein: "Transfer $30K to Alice"
6. Board executes: $30K transfer from Verein account

Foundation could also:
- Keep funds in own account
- Transfer directly to Alice (bypass Verein)
- No Board involvement in that case

Contributing to Verein gives Foundation trust in execution,
but Board still doesn't decide allocation.
```

### Two Provider Scenarios

The Verein operates under two provider models:

**Primary: Verein as Computational Provider**

This is the core model.

```
Verein has $100K available (general donations, accumulated surplus):
1. Protocol detects available balance
2. Protocol automatically declares capacity: all current members
3. Protocol computes collective-recognition-shares for all members
4. Protocol applies compliance filters (AML/KYC verification)
5. Protocol reviews all open needs
6. Protocol allocates automatically by:
   - Filtered collective-recognition-shares
   - Need amounts and urgency
   - Available capacity
7. Board receives: "Transfer $50K to Alice, $30K to Bob, $20K to Charlie"
8. Board executes mechanically

Provider: Verein (but allocation is computational)
Allocator: Protocol (not Board, not General Assembly)
Board: Execution only (doesn't decide)
Compliance: Protocol applies filters before allocation
```

**Secondary: External Provider Using Verein as Executor**

External entities can use the Verein for execution.

```
Foundation X (external provider):
1. Deposits $100K to Verein bank account
2. Declares capacity: {Alice, Bob, Charlie} - $100K
3. Reviews collective-recognition-shares + needs
4. Applies their own compliance filters
5. Makes allocation decision: $30K to Alice, $25K to Bob
6. Instructs Board: "Execute these transfers"
7. Board executes mechanically

Provider: Foundation X (makes allocation decisions)
Board: Execution agent only (doesn't decide)
Foundation X: Responsible for their own compliance
```

**Key distinction:**
- In Primary model, the protocol makes allocation decisions (with compliance filtering)
- In Secondary model, external entity makes allocation decisions (with their own compliance)
- In both models, the Board only executes (never decides)

### Compliance Filtering

**Providers are wholly responsible for implementing compliance filters.**

Each provider (including Verein itself) must filter their capacity declarations based on:
- AML/KYC compliance (Know Your Customer verification)
- Sanctions screening (OFAC, UN, EU sanctions lists)
- Legal jurisdiction restrictions
- Tax compliance requirements
- Mission/purpose alignment

**Allocation Filter Model:**
```
Filter(Member, Capacity) = Maximum amount that can be allocated to Member from this Capacity

Values:
- $0 = Cannot allocate (sanctions, KYC failed, etc.)
- $X = Can allocate up to $X (jurisdiction limits, risk caps, etc.)
- Unlimited = No restriction

Actual-Allocation = min(
  Recognition-Share × Total-Capacity,
  Filter(Member, Capacity),
  Member-Need
)

Members who hit their filter limit receive up to that limit.
Unallocated capacity redistributes to other members by recognition shares.
```

**Union of Filters (External Provider via Verein):**
```
When external provider uses Verein as executor, BOTH filters apply:

Effective-Filter(Member) = min(
  Provider-Filter(Member),
  Verein-Filter(Member)
)

Most restrictive filter wins. Both must allow allocation.

Example:
- Foundation X filter for Alice: $50K max
- Verein filter for Alice: $30K max (jurisdiction limit)
- Effective filter: min($50K, $30K) = $30K

- Foundation X filter for Charlie: $40K max
- Verein filter for Charlie: $0 (sanctions list)
- Effective filter: min($40K, $0) = $0 → Cannot allocate

External provider's approval is necessary but not sufficient.
Verein's compliance filters always apply (Swiss law).
```

**Example:**
```
Verein has $100K to allocate to 5 members

Verein's compliance filters:
- Alice: Unlimited (fully compliant)
- Bob: Unlimited (fully compliant)
- Charlie: $0 (KYC failed - cannot allocate)
- Dave: Unlimited (fully compliant)
- Eve: $5K max (jurisdiction limit)

Initial allocation by recognition share:
- Alice: $28K, Bob: $22K, Charlie: $19K, Dave: $18K, Eve: $13K

After applying filters:
- Alice: $28K ✓
- Bob: $22K ✓
- Charlie: $0 (excluded)
- Dave: $18K ✓
- Eve: $5K (limited)

Unallocated $27K redistributes to Alice, Bob, Dave by recognition shares.

Final: Alice: $44K, Bob: $30K, Charlie: $0, Dave: $20K, Eve: $5K
```

**Key principle:** Compliance filtering is computational, not Board discretion. Board executes only compliant, filtered allocations.

### Decider Process Integration

When constitutional change needed:
1. Member initiates Decider session via protocol
2. Decider process runs (weeks 1-5)
3. Result: Weighted decision output
4. General Assembly formally adopts result
5. Board implements (if legal changes needed)

**Example 1: Protocol parameter (no legal filing)**
```
Changing MRD threshold from 0.5 to 0.6

Decider runs → Result: Adopt 0.6 threshold (60% support)
General Assembly acknowledges
Protocol updated → New threshold takes effect
Board: No action needed
```

**Example 2: Legal structure (requires filing)**
```
Changing seat from Zurich to Basel

Decider runs → Result: Move to Basel (64% support)
General Assembly adopts amendment
Board: File amended Article 1 with commercial register
Board: Update bank, tax records
New seat legally effective
```

**Example 3: Self-amendment (higher threshold)**
```
Changing amendment threshold from 60% to 66%

Decider runs → Result: Adopt 66% threshold (78% support)
General Assembly adopts amendment to Article 9
Article 9.3 updated
Future amendments require 66% (except Article 9 itself: 75%)
```

All decisions follow same process. Board executes mechanically, cannot override.

### Minimum Viable Implementation

**To launch:**
1. 2+ founding members sign statutes
2. Register with Swiss authorities
3. Open bank account (Board signatories)
4. Deploy computational protocols at [URL]
5. Begin weekly MRD computations

**Ongoing operation:**
- Week 1: MRD computation → Member list updated
- Week 2-52: Repeat
- Annually: General Assembly acknowledges results
- As-needed: Board executes transfers per protocol

**No meetings, no votes, no governance** except annual formality and rare constitutional changes via Decider.