### Swiss Bank Account Compatibility

**Can we open a Swiss bank account with this structure? Yes.**

**What Swiss banks require:**
```
1. Legal entity (Swiss Verein) ✓
   - Registered with commercial register
   - Valid statutes
   - Registered seat in Switzerland

2. Authorized signatories ✓
   - Board members (3 members with equal authority)
   - Signatory cards filed
   - Collective signature (2 signatures required)

3. Clear governance ✓
   - Statutes define Board powers (Article 6)
   - Board authorized to manage bank accounts
   - General Assembly supervises Board

4. Compliance documentation ✓
   - Purpose of association (Article 2)
   - Source of funds (documented)
   - Anti-money laundering (AML) compliance
```

**What banks DON'T require:**
```
❌ Banks don't care HOW the organization decides internally
❌ Banks don't care about computational protocols
❌ Banks don't care about distributed decision-making
❌ Banks don't need to understand MRD or collective recognition

Banks only care:
✓ Who can sign (Board)
✓ What entity it is (Swiss Verein)
✓ Proper documentation (Statutes, registry)
✓ Legal compliance (AML, etc.)
```

**From bank's perspective:**
```
Bank sees:
- Swiss Verein "Playnet" registered in [Canton]
- Board: 3 members with equal authority (Person A, Person B, Person C)
- President: Person A (legal representative designation)
- Signatory authority: Any 2 Board members collectively
- Transfer request: Signed by two Board members
- Bank verifies: Valid signatures, sufficient funds
- Bank executes: Transfer

Bank doesn't see or care:
- How Board decided to make this transfer
- That protocol computed the allocation
- That members made distributed declarations
- That collective recognition determined amount
```

**The statutes satisfy banks because:**
```
Article 6.1: "The Board manages bank accounts"
→ Bank knows: Board is authorized

Article 6.2: "President and one other Board member sign collectively"
→ Bank knows: Who can sign, how many signatures needed

Article 7.2: "Board executes approved transfers"
→ Bank knows: Board has authority to instruct transfers

Banks don't need to know WHAT "approved" means.
The statutes say Board executes transfers.
That's sufficient for banks.
```

**Operational flow with Swiss bank:**
```
1. Open account:
   - Board members go to bank
   - Present: Statutes, commercial register extract, IDs
   - Fill signatory cards
   - Account opened
   - Bank notes: Board authorized signatories

2. Receive funds (Primary model):
   - Individual donors contribute $100K (unrestricted)
   - Bank receives wire transfers
   - Verein balance: $100K available
   - Board records: General funds received

3. Protocol allocation (Primary model):
   - Protocol detects $100K available balance
   - Protocol computes collective-recognition-shares for all members
   - Protocol applies compliance filters (AML/KYC/sanctions)
   - Protocol outputs: "Transfer $30K to Alice, $25K to Bob, $20K to Charlie"
   - Board receives transfer instructions
   
4. Execute transfers:
   - Board verifies: Funds available, protocol authentic, compliance verified
   - Board signs: Transfer instructions to bank
   - Bank receives: Signed transfer forms
   - Bank verifies: Valid signatures
   - Bank executes: $30K to Alice, $25K to Bob, $20K to Charlie
   - Board records: Transactions in ledger

5. Bank sees:
   - Normal organizational transfers
   - Properly authorized by Board
   - Legal and compliant
   - Recipients verified (KYC/AML)
   
6. Bank doesn't see:
   - That protocol determined allocations
   - That collective recognition computed amounts
   - That compliance filtering was computational
   - That this was distributed decision-making
```

**Why this is fully compatible:**
```
Legal Layer (Bank cares):
- Swiss Verein structure
- Board signing authority
- Proper documentation
→ Standard Swiss association

Computational Layer (Bank doesn't care):
- Distributed collective capacities
- Protocol computations
- Provider responses
→ Internal organizational process

The statutes bridge these layers:
- Provide legal structure for banks
- Reference computational protocols for members
- Board executes protocol outputs mechanically
```

**Specific banking examples:**

**Example 1: PostFinance (Swiss Post)**
```
Requirements:
- Swiss Verein registration ✓
- Statutes ✓
- Board signatory authority ✓
- Purpose statement ✓

Account opening:
- Board presents documents
- PostFinance reviews statutes
- Sees: Article 6 (Board manages accounts)
- Sees: Article 7 (Board executes transfers)
- Opens account with Board as signatories
- No questions about computational protocols
```

**Example 2: Cantonal bank (e.g., ZKB, BCGE)**
```
Same process:
- Board is authorized signatory
- Statutes are clear on authority
- Bank doesn't question internal decision process
- Board signs all transfers
- Bank executes based on valid signatures
```

**The key insight:**
```
Separation of concerns:

Legal/Banking Layer:
- Swiss Verein provides legal personality
- Board provides signing authority
- Statutes provide governance framework
→ Banks are satisfied

Computational Layer:
- Protocol determines allocations
- Members make distributed decisions
- Compliance filters applied computationally
→ This happens "above" the legal layer

Board is the interface:
- Receives protocol outputs (already filtered for compliance)
- Signs for legal/banking layer
- Pure execution, no discretion
→ Bridge between layers
```

**AML/KYC Compliance with Computational Model:**

```
How compliance works:

1. Member onboarding:
   - Member applies to join network
   - Verein collects KYC information (identity verification)
   - Verein screens against sanctions lists (OFAC, UN, EU)
   - Verein assigns compliance Filter(Member):
     - Unlimited = Fully compliant, no restrictions
     - $X = Can allocate up to $X (jurisdiction/risk limits)
     - $0 = Cannot allocate (sanctions, KYC failed)

2. Protocol allocation (Primary model):
   - Protocol computes collective-recognition-shares
   - Protocol applies compliance filters as limits:
     Allocation = min(Recognition-Share × Capacity, Filter, Need)
   - Members who hit filter limit get capped
   - Excess redistributes to others by recognition shares

3. Board execution:
   - Board receives filtered allocation instructions
   - Board verifies: All allocations respect filters
   - Board signs transfers (only to compliant recipients)
   - Bank executes transfers

4. Bank perspective:
   - Sees normal organizational transfers
   - Recipients are pre-verified by Verein
   - Board signing = proper authorization
   - No questions about how recipients were chosen

Key: Compliance filtering happens computationally (in protocol).
Board doesn't decide who passes compliance or their limits.
Protocol applies filters as hard caps on allocations.
Bank never sees the filtering mechanism.
```

**Example 1: Sanctions screening:**

```
Scenario: Member added to sanctions list

Week 1: Charlie is compliant member
- Filter: Unlimited
- Receives allocations normally
- Collective-recognition-share: 19%
- Allocated: $19K from $100K pool

Week 2: Charlie added to UN sanctions list
- Verein's compliance system detects change
- Updates Charlie's Filter: Unlimited → $0
- Protocol automatically excludes Charlie from allocations
- Charlie's $19K redistributes to other members

Week 3: Protocol runs allocation
- Charlie has 19% general recognition
- Charlie has $0 compliance filter
- Charlie receives $0 (excluded)
- Other members receive proportionally more

Board sees:
- Transfer instructions without Charlie
- No explanation needed
- Simply executes as instructed

Bank sees:
- Normal transfers to verified recipients
- No unusual activity
- Board authorized all transfers

Key: Compliance enforcement is computational, not Board discretion.
Protocol applies sanctions screening automatically.
Board never decides to exclude someone - protocol does.
```

**Example 2: Union of filters (External provider via Verein):**

```
Scenario: Foundation X uses Verein to allocate $100K

Foundation X's filters (their compliance):
- Alice: $50K max (their risk limit)
- Bob: $40K max (their risk limit)
- Charlie: $0 (outside their target region)
- Dave: $30K max (their risk limit)

Verein's filters (Swiss AML/KYC):
- Alice: $30K max (jurisdiction limit for her country)
- Bob: Unlimited (fully compliant)
- Charlie: $0 (on EU sanctions list)
- Dave: Unlimited (fully compliant)

Effective filters (min of both):
- Alice: min($50K, $30K) = $30K
- Bob: min($40K, Unlimited) = $40K
- Charlie: min($0, $0) = $0
- Dave: min($30K, Unlimited) = $30K

Foundation X wants to allocate:
- $35K to Alice → Effective filter caps at $30K
- $40K to Bob → Passes both filters, allocated $40K
- $25K to Charlie → Effective filter = $0, cannot allocate
- $30K to Dave → Passes both filters, allocated $30K

Board receives instruction: "Transfer $30K to Alice, $40K to Bob, $30K to Dave"
(Charlie excluded by union of filters, Alice capped by Verein's jurisdiction limit)

Board executes mechanically.

Bank sees:
- Three normal transfers
- All properly authorized by Board
- All recipients pre-verified

Bank doesn't see:
- That Foundation X wanted to send more to Alice
- That Foundation X wanted to send to Charlie
- That two independent filter systems applied
- How the allocation decisions were made

Key: Both filters must pass. Most restrictive wins.
External provider cannot override Verein's compliance filters.
Verein ensures Swiss law compliance regardless of provider's wishes.
```
