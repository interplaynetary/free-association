### Swiss Bank Account Compatibility

**Can we open a Swiss bank account with this structure? Yes.**

**What Swiss banks require:**
```
1. Legal entity (Swiss Verein) ✓
   - Registered with commercial register
   - Valid statutes
   - Registered seat in Switzerland

2. Authorized signatories ✓
   - Board members (President + Treasurer typically)
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
- Board: President (Person A), Treasurer (Person B), Secretary (Person C)
- Signatory authority: President + 1 other Board member
- Transfer request: Signed by President + Treasurer
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

2. Receive funds:
   - Foundation X wants to contribute $100K
   - Foundation transfers to Verein account
   - Bank receives wire transfer
   - Board records: Foundation X deposited $100K

3. Execute transfer (protocol-determined):
   - Protocol outputs: "Transfer $30K to Alice"
   - Board verifies: Funds available, protocol authentic
   - Board signs: Transfer instruction to bank
   - Bank receives: Signed transfer form
   - Bank verifies: Valid signatures
   - Bank executes: $30K to Alice
   - Board records: Transaction in ledger

4. Bank sees:
   - Normal organizational transfer
   - Properly authorized by Board
   - Legal and compliant
   
5. Bank doesn't see:
   - That protocol determined allocation
   - That collective recognition computed amount
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
- Provider responses are voluntary
→ This happens "above" the legal layer

Board is the interface:
- Receives protocol outputs
- Signs for legal/banking layer
- Pure execution, no discretion
→ Bridge between layers
```
