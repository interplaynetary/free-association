# Statutes of [Name] Association

## Article 1 — Name and Seat

The Association "[Name]" (the "Association") is established under Articles 60 et seq. of the Swiss Civil Code. Its seat is in [City], Switzerland.

## Article 2 — Purpose

The Association exists to support and experiment with games/organizations that enable societal flourishing.

## Article 3 — Membership

**3.1 Determination**  
Membership is determined computationally. A person is a member when their Mutual Recognition Density (MRD) equals or exceeds the threshold value (currently 0.5).

**3.2 MRD Computation**  
MRD is computed weekly according to the Membership Module specification, available at [URL/reference].

**3.3 Member List**  
The current member list consists of all participants with MRD ≥ threshold as of the most recent computation.

**3.4 Exit**  
Membership ends automatically when MRD falls below threshold. Members may also exit by written declaration.

## Article 4 — Organs

The Association has two organs:

**4.1 General Assembly**  
Comprises all current members (MRD ≥ threshold).

**4.2 Board**  
Comprises the three members with highest MRD scores:
- Highest MRD = President
- 2nd highest MRD = Treasurer  
- 3rd highest MRD = Secretary

Board composition updates automatically with each MRD computation.

## Article 5 — General Assembly

**5.1 Powers**  
The General Assembly:
- Acknowledges computational results (membership, resource flows)
- Approves annual financial statements
- Adopts constitutional amendments via Decider process
- Supervises the Board

**5.2 Convening**  
Meets at least annually. All members may attend. Notice period: 14 days.

**5.3 Decisions**  
Decisions on constitutional matters follow the Decider process specification at [URL/reference]. Routine matters are determined by computational protocols.

## Article 6 — Board

**6.1 Powers**  
The Board:
- Represents the Association externally
- Signs legal documents
- Maintains statutory compliance
- Executes transfers per computational protocol outputs
- Manages bank accounts

**6.2 Signatory Authority**  
The President and one other Board member sign collectively.

**6.3 Limitation**  
The Board does not decide membership, resource allocation, or strategy. These are determined by computational protocols.

## Article 7 — Resources

**7.1 Resource Allocation**  
Resources are allocated according to the Resource Allocation Protocol specification at [URL/reference].

**7.2 Needs and Allocations**  
Members declare needs. Providers declare capacities and allocate to needs. When the Association itself holds unrestricted funds, the protocol automatically allocates to member needs based on collective-recognition-shares. The Board executes approved transfers.

**7.3 Financial Records**  
All flows are recorded in a transparent ledger maintained per the protocol specification.

## Article 8 — Computational Protocols

**8.1 Protocol Specifications**  
The Association operates through three computational protocols:
- Membership Module (MRD computation)
- Resource Allocation Protocol (provider capacities and need matching)
- Decider Process (parameter adjustments)

**8.2 Protocol References**  
Current protocol specifications are maintained at [URL/repository] and may be updated via Decider process.

**8.3 Interface**  
The Board implements protocol outputs. Protocol computations are authoritative for membership and resource allocation.

## Article 9 — Dissolution

**9.1 Decision**  
Dissolution requires Decider process with supermajority (75% weighted support).

**9.2 Liquidation**  
Upon dissolution, assets are distributed to members pro-rata by collective-recognition-share (calculated across all members as of dissolution decision), or to organizations with similar purpose.

## Article 10 — Entry into Force

These Statutes enter into force upon adoption by the founding members.

---

**Adopted:** [Date]  
**Place:** [City]

**Founding Members:**  
[Signatures of at least 2 founding members]

---

## Implementation Notes

### Interface Points Between Statutes and Protocols

| Statute Article | Protocol | Data Flow |
|-----------------|----------|-----------|
| Art. 3.2 | Membership Module | Weekly MRD computation → Member list |
| Art. 4.2 | Membership Module | Weekly MRD ranking → Board composition |
| Art. 5.3 | Decider Process | Constitutional proposals → Weighted decision |
| Art. 7.1 | Resource Allocation | Needs + Provider capacities → Transfer instructions |
| Art. 7.3 | Resource Allocation | All transactions → Ledger entries |

### Required Protocol Outputs (to Board)

**Weekly:**
- Current member list (names, MRD scores)
- Current Board composition (top 3 MRD)

**As-needed:**
- Transfer instructions (from provider allocations)
- Decider results (for constitutional changes)

**Annually:**
- Financial statements (auto-generated from ledger)
- Membership changes summary

### Board's Mechanical Role

The Board receives computational outputs and executes:
1. **Membership changes:** Update Swiss registry if required
2. **Resource transfers:** Execute bank transfers per protocol
3. **Legal representation:** Sign documents as authorized
4. **Compliance:** File required reports with authorities

The Board does NOT:
- Decide who is a member (protocol does)
- Decide resource allocation (protocol does)
- Set strategic direction (members + protocol do)
- Override protocol outputs

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

**Why this works:**
- Need declarations are distributed (members declare)
- Capacity declarations are distributed (only providers declare)
- Provider allocations are voluntary (providers decide)
- Board execution is mechanical (Board doesn't decide)
- Funds can flow through Verein (with Board execution) or peer-to-peer (no Board)
- Verein provides legal structure for those who want it
- Protocol operates independently of Verein's legal status

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

**Why Primary model is core:**
- Verein receives unrestricted donations (most funding)
- Verein accumulates surplus from operations
- Maintains computational allocation even for Verein's own funds
- Board never gains discretionary power over allocations
- Compliance filtering happens computationally (not Board discretion)

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