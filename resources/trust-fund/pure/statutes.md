# Statutes of [Name] Association

## Article 1 — Name and Seat

The Association "[Name]" (the "Association") is established under Articles 60 et seq. of the Swiss Civil Code. Its seat is in [City], Switzerland.

## Article 2 — Purpose

The Association facilitates bioregional regenerative projects through computational coordination mechanisms.

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
Resources are allocated according to the Collective Recognition Module specification at [URL/reference].

**7.2 Claims and Responses**  
Members declare needs and collective capacities. Providers respond to claims. The Board executes approved transfers.

**7.3 Financial Records**  
All flows are recorded in a transparent ledger maintained per the protocol specification.

## Article 8 — Computational Protocols

**8.1 Protocol Specifications**  
The Association operates through three computational protocols:
- Membership Module (MRD computation)
- Collective Recognition Module (resource allocation)
- Decider Process (parameter adjustments)

**8.2 Protocol References**  
Current protocol specifications are maintained at [URL/repository] and may be updated via Decider process.

**8.3 Interface**  
The Board implements protocol outputs. Protocol computations are authoritative for membership and resource allocation.

## Article 9 — Dissolution

**9.1 Decision**  
Dissolution requires Decider process with supermajority (75% weighted support).

**9.2 Liquidation**  
Upon dissolution, assets are distributed to members pro-rata by MRD score at time of dissolution decision, or to organizations with similar purpose.

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
| Art. 7.1 | Collective Recognition | Claims + Responses → Transfer instructions |
| Art. 7.3 | Collective Recognition | All transactions → Ledger entries |

### Required Protocol Outputs (to Board)

**Weekly:**
- Current member list (names, MRD scores)
- Current Board composition (top 3 MRD)

**As-needed:**
- Transfer instructions (from Collective Recognition)
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

**Key insight:** The Board never sees collective capacity declarations. The Board only receives specific transfer instructions.

**The flow:**
```
Layer 1 - Distributed (No centralization):
Members declare collective capacities (subjective, overlapping)
- Alice: "{Alice,Bob,Charlie} has $500K water capacity"
- Bob: "{Bob,Charlie,Dave} has $300K agriculture capacity"

Layer 2 - Protocol Computation (Deterministic):
Protocol computes claims from each declaration
- Alice's claim: $288K (from her declaration)
- Bob's claim: $144K (from his declaration)

Layer 3 - Provider Response (Distributed):
Providers see claims, respond voluntarily
- Foundation X: "I'll send $30K to Alice"
- Foundation Y: "I'll send $20K to Bob"

Layer 4 - Board Execution (Mechanical):
Board receives ONLY: "Transfer $30K to Alice, $20K to Bob"

Board does NOT see:
❌ Collective capacity declarations ($500K, $300K)
❌ Claim amounts ($288K, $144K)
❌ Why these transfers
❌ Provider reasoning

Board ONLY sees:
✓ Specific transfer instructions
✓ Amounts and recipients
✓ That's it
```

**No centralization of capacities.** Capacities remain distributed and subjective.

**What gets centralized:** Only the execution of specific transfers that already emerged from the distributed process.

**Board = Signing robot.** Receives final instructions, executes mechanically, sees nothing upstream.

### Resource Flow: Protocol to Board Interface

**The distributed protocol determines allocations. The Board executes approved transfers.**

**Flow:**
```
1. Members declare collective capacities (distributed, subjective)
2. Protocol computes collective-recognition-shares → Claims
3. Providers see claims in network
4. Providers respond to claims:

   Path A: Direct transfer (provider to claimant)
   - Provider has funds outside Verein
   - Provider transfers directly to claimant
   - No Board involvement
   
   Path B: Verein transfer (provider instructs Board)
   - Provider contributed funds TO Verein
   - Provider response = instruction to Board
   - Board executes transfer from Verein account

5. Board receives transfer instructions from protocol
6. Board executes authorized transfers
```

**Example:**
```
Alice has $50K claim (from collective capacity declaration)

Provider responses:
- Foundation X: "Transfer $30K from Verein account to Alice"
  → Goes through Board (Foundation donated to Verein)
  
- Individual Y: "I'm sending $5K directly to Alice" 
  → Direct transfer (no Board involvement)
  
- Foundation Z: "Transfer $10K from our account to Alice"
  → Direct transfer (Foundation pays from own account)

Board sees:
- Protocol output: "Transfer $30K to Alice (from Foundation X response)"
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
- Decide who gets resources (protocol decides)
- Override protocol allocations
- Prioritize one claim over another
- Withhold authorized transfers
```

**Provider's choice:**
```
When Foundation X contributes $100K to Verein:
1. Foundation sees network claims
2. Foundation responds to Alice's claim: $30K
3. Foundation instructs Verein: "Transfer $30K to Alice"
4. Protocol records: Foundation X → Alice response
5. Board executes: $30K transfer from Verein account

Foundation could also:
- Keep funds in own account
- Transfer directly to Alice (bypass Verein)
- No Board involvement in that case

Contributing to Verein gives Foundation trust in execution,
but Board still doesn't decide allocation.
```

**Why this works:**
- Collective Recognition is distributed (members decide)
- Provider responses are voluntary (providers decide)
- Board execution is mechanical (Board doesn't decide)
- Funds can flow through Verein (with Board execution) or peer-to-peer (no Board)
- Verein provides legal structure for those who want it
- Protocol operates independently of Verein's legal status

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