# Open Collective

## Playnet - Governance and Operating Guidelines

**Status:** Hosted Collective on Open Collective\
**Fiscal Host:** Open Collective Europe (Belgian non-profit)\
**Platform:** opencollective.com

***

### Overview

This collective operates as a **Hosted Collective** on Open Collective, with **Open Collective Europe** as our fiscal host.

**What this means:**

* We do NOT have our own legal entity
* We do NOT have our own bank account
* We do NOT handle our own taxes, accounting, or legal compliance
* Open Collective Europe provides legal status (Belgian non-profit)
* Open Collective Europe holds our funds in their bank account
* Open Collective Europe handles all administrative obligations

**What we control:**

* Our collective's purpose and activities
* Who our members are
* How we allocate our resources
* What expenses we approve
* Our transparent budget on Open Collective

***

### 1. Purpose

This collective exists to support and experiment with games/organizations that enable societal flourishing.

We coordinate through computational protocols that eliminate traditional governance overhead while maintaining transparency and fairness.

***

### 2. Legal Structure

#### 2.1 Fiscal Host Relationship

**Legal Entity:** Open Collective Europe ASBL (Belgian non-profit)\
**Registered Address:** \[Belgium]\
**Tax Status:** Belgian non-profit\
**Bank Account:** Held by Open Collective Europe

**Services Provided by Open Collective Europe:**

* Legal status and liability coverage
* Bank account for holding collective funds
* Payment processing (Stripe, Wise, PayPal, bank transfers)
* Tax compliance and reporting
* Accounting and bookkeeping
* Regulatory compliance (Belgian and EU law)
* Anti-money laundering (AML) compliance
* Platform access and support

**Host Fees:**

* Open Collective Europe charges a hosting fee (typically 5-10%)
* Covers administrative overhead and legal services
* Transparent and visible on all transactions

#### 2.2 Our Responsibilities

As a hosted collective, we are responsible for:

* Managing our transparent budget on Open Collective
* Approving or rejecting expense submissions
* Maintaining our collective's purpose and activities
* Communicating with contributors and members
* Following Open Collective Europe's policies
* Ensuring expenses align with our stated purpose

#### 2.3 What We Cannot Do

Because we don't have independent legal status:

* Cannot enter contracts in our own name (must go through OC Europe)
* Cannot open our own bank account
* Cannot have our own tax status
* Cannot sue or be sued independently
* Cannot own property directly

***

### 3. Membership

#### 3.1 Computational Membership

Membership is determined by the **Membership Module** - a computational protocol based on mutual recognition.

**Mutual Recognition Density (MRD):**

* Each participant recognizes others' contributions to their own self-actualization
* Each participant distributes 100% recognition total
* Mutual Recognition = min(your recognition of them, their recognition of you)
* MRD = Your total mutual recognition / Network average mutual recognition
* Membership threshold: MRD ≥ 0.5 (adjustable via Decider)

**Automatic Membership:**

* Computed weekly
* No approval process
* No membership fees
* Join by building genuine mutual recognition
* Leave when MRD drops below threshold or by written declaration

**Full protocol specification:** [playnet.gitbook.io/docs/organizations/protocol](https://playnet.gitbook.io/docs/organizations/protocol)

#### 3.2 Member Rights

All members (MRD ≥ threshold) can:

* Submit recognition data
* Declare needs
* Declare capacity perspectives (mapping what resources exist)
* Allocate resources if they are providers
* Participate in Decider processes
* View transparent budget on Open Collective
* Propose expenses to the collective

#### 3.3 Open Collective Account

All members are encouraged to create an Open Collective account to:

* Submit expenses for approved allocations
* View transparent budget
* See allocation rationale
* Track collective activities

***

### 4. Governance Structure

#### 4.1 Collective Admins

**Selection:** Top 3 members by MRD score (opt-in)

* Positions offered by MRD ranking
* If a member declines, offer goes to next highest
* Members must explicitly accept to become admin
* Positions reviewed quarterly

**Responsibilities:**

* Approve expenses on Open Collective (based on protocol outputs)
* Manage collective settings on platform
* Communicate with Open Collective Europe when needed
* Execute protocol decisions mechanically
* Maintain collective description and transparency

**What Admins Do NOT Do:**

* Decide membership (protocol computes MRD)
* Decide resource allocation (protocol computes allocations)
* Make discretionary decisions
* Override protocol outputs
* Control individual member activities

**Admin Access:**

* All 3 admins have equal access to Open Collective admin panel
* Any admin can approve expenses
* Changes to collective settings require 2 admins

#### 4.2 No Traditional Governance

This collective has:

* ❌ No board of directors
* ❌ No executive committee
* ❌ No voting on resource allocation
* ❌ No approval processes for members
* ❌ No appointed positions

Everything is determined by:

* ✓ Computational protocols (membership, allocation)
* ✓ Mutual recognition patterns
* ✓ Provider capacity declarations
* ✓ Transparent public budget

***

### 5. Resource Allocation Protocol

#### 5.1 Core Principles

Resources flow based on computational protocols, not governance decisions.

**Recognition = Your acknowledgment of contributions to YOUR self-actualization**

* Each member has 100% recognition to distribute
* Non-transferable (can't be sold or traded)
* Dynamically adjustable
* Represents what enables you to self-actualize

**Mutual Recognition = min(your recognition of them, their recognition of you)**

* Ensures genuine reciprocity
* One-sided recognition doesn't create mutual relationship
* Measures real interdependence

#### 5.2 Allocation Process

**Step 1: Members declare needs**

* Post needs via protocol interface
* Example: "I need $1000 for water infrastructure"
* All needs visible to collective

**Step 2: Members recognize contributions**

* Each member distributes their 100% recognition
* Recognition patterns emerge from genuine enabling relationships
* Protocol tracks recognition data

**Step 3: Members declare capacity perspectives**

* Any member can map: "From my view, Collective X can allocate $5000 to {Alice, Bob, Charlie}"
* These are perspectives of what resources exist
* All members see collective-recognition-shares for any declared capacity
* Enables distributed coordination

**Step 4: Protocol computes collective-recognition-shares**

* For each capacity declaration, calculates each member's share
* Alice's share = (Alice's mutual recognition with others in set) / (Total mutual recognition in set)
* Visible to all members for any declared capacity lens

**Step 5: Providers allocate**

* When someone with actual resources (provider) declares capacity, they allocate to needs
* Based on collective-recognition-shares + actual needs + available capacity
* Providers can be: External donors, grant-makers, or the collective itself

**Step 6: Admins approve expenses on Open Collective**

* Protocol outputs allocation decision
* Admins create/approve expense on Open Collective
* Include allocation rationale: "Collective-recognition-share: 28%, Need: $1000, Protocol timestamp: \[date]"
* Member receives payment via Open Collective (Stripe/Wise/PayPal/bank transfer)

#### 5.3 Automatic Allocation of Collective Funds

When the collective has available balance on Open Collective:

**Automatic Protocol:**

1. Protocol detects available balance
2. Protocol generates capacity declaration: all current members
3. Protocol computes collective-recognition-shares for all members
4. Protocol reviews all open needs from members
5. Protocol allocates automatically by recognition shares + needs
6. Admins receive allocation instructions
7. Admins approve expenses on Open Collective
8. Members receive payments automatically

**Example:**

```
Collective balance: $5000

Members: {Alice, Bob, Charlie, Dave, Eve}
Collective-recognition-shares:
- Alice: 28%
- Bob: 22%
- Charlie: 19%
- Dave: 18%
- Eve: 13%

Open needs:
- Alice: $2000 (water infrastructure)
- Bob: $1500 (agriculture)
- Charlie: $1000 (education)
- Dave: $800 (research)
- Eve: $500 (organizing)

Protocol allocation (by recognition + feasibility):
- Alice: $2000 (highest share, full need)
- Bob: $1500 (second priority, full need)
- Charlie: $1000 (third priority, full need)
- Dave: $500 (partial, capacity exhausted)
- Total: $5000 allocated

Admins approve expenses on OC for Alice, Bob, Charlie, Dave
Members receive payments via Open Collective
```

**Key: Admins never decide allocations. Protocol computes based on recognition.**

#### 5.4 External Provider Allocations

External funders can contribute with their own allocation decisions:

**Option A: General contribution**

* Donor contributes to collective on Open Collective
* Funds go into general balance
* Protocol allocates automatically based on collective-recognition-shares

**Option B: Directed allocation**

* Donor contributes to collective
* Provides allocation instructions: "I want this allocated to {Alice, Bob, Charlie} based on protocol"
* Protocol computes collective-recognition-shares within donor's specified set
* Donor sees shares and makes allocation decision
* Admins approve expenses per donor's instructions

**Option C: Direct expense**

* External provider creates expense for specific member
* Admins approve if aligned with purpose and protocol
* Member receives payment

#### 5.5 Compliance and Restrictions

**Open Collective Europe handles:**

* KYC verification (identity checks)
* Sanctions screening (OFAC, UN, EU lists)
* Payment processor compliance (Stripe, Wise)
* Belgian/EU regulatory requirements
* Tax reporting

**We apply computational filters:**

* Protocol can apply additional filters based on compliance data
* Filter(Member) = Maximum allocation
  * $0 = Cannot allocate (sanctions, compliance failure)
  * $X = Capped amount (jurisdiction limits, risk management)
  * Unlimited = No restrictions
* Filters applied before admins see allocation instructions
* Admins never override filters

**Expense Approval Criteria:**

* ✓ Aligned with collective purpose
* ✓ Recipient is member or approved by protocol
* ✓ Amount matches protocol allocation
* ✓ Compliance filters respected
* ✓ Documentation provided
* ✓ Open Collective Europe policies followed

***

### 6. Decider Process

For constitutional/parameter changes only (rarely used).

**When to use:**

* Changing MRD threshold
* Changing protocol parameters
* Changing collective purpose
* Major structural changes

**When NOT to use:**

* Resource allocation (use protocol)
* Membership decisions (use MRD computation)
* Daily operations (use protocol)
* Project priorities (use protocol)

**Process:**

1. Member initiates Decider session
2. Proposal phase (1 week)
3. Challenge phase (1 week)
4. Discussion phase (1 week)
5. Improvement phase (1 week)
6. Support distribution phase (1 week)
7. Result: Weighted by MRD scores
8. Implementation: Admins update settings

**Full specification:** [**pl**aynet.gitbook.io/docs/decider/decider](https://playnet.gitbook.io/docs/decider/decider)

***

### 7. Transparency and Public Budget

#### 7.1 Open Collective Transparency

Everything is public on our Open Collective page:

**Visible to Everyone:**

* All incoming contributions (who gave, how much, when)
* All outgoing expenses (who received, how much, for what, when)
* Current balance and reserves
* Allocation rationale (collective-recognition-shares)
* Admin activity
* Member list (optional)

**Budget Dashboard:**

* Real-time balance
* Monthly income and expenses
* Historical transactions
* Contribution trends
* Impact metrics

#### 7.2 Protocol Transparency

In addition to Open Collective budget:

**Protocol Data (on our interface):**

* Current member list (MRD ≥ threshold)
* MRD scores and rankings
* Recognition patterns (anonymized or public, as we choose)
* Collective-recognition-shares for any capacity declaration
* Open needs from members
* Allocation history and rationale

**Full Transparency:**

* Anyone can verify protocol computations
* Anyone can see why allocations were made
* Anyone can observe recognition patterns
* Anyone can understand collective-recognition-shares

#### 7.3 Communication

**Update Collective regularly:**

* Post updates on Open Collective
* Share impact stories
* Show how funds were used
* Thank contributors publicly
* Explain protocol decisions

**Build Trust:**

* Transparency creates confidence
* Clear allocation rationale
* Public recognition patterns
* Auditable protocol computations

***

### 8. Practical Operations

#### 8.1 Receiving Contributions

**Anyone can contribute via Open Collective:**

* One-time donations
* Recurring contributions (monthly, yearly)
* Custom amounts
* Payment methods: Credit card, PayPal, bank transfer, cryptocurrency (via partner integrations)

**Contributors receive:**

* Instant receipt
* Public acknowledgment (unless anonymous)
* Real-time tracking of how funds are used
* Visibility of allocation rationale

#### 8.2 Making Expenses

**Members submit expenses:**

1. Create expense on Open Collective
2. Provide documentation (invoice, receipt, description)
3. Link to protocol allocation (if applicable)
4. Submit for admin approval

**Admins review:**

1. Check protocol output (was this allocated?)
2. Verify alignment with purpose
3. Confirm compliance filters respected
4. Ensure documentation adequate
5. Approve or request changes

**Payment:**

* Open Collective processes payment
* Via Stripe, Wise, PayPal, or bank transfer
* Typically within 1-5 business days
* Member receives notification

#### 8.3 Different Expense Types

**Protocol Allocations:**

* Based on collective-recognition-shares
* Pre-computed by protocol
* Admins verify and approve mechanically

**Project Expenses:**

* Member doing work declares need
* Protocol computes allocation
* Member submits expense with deliverables
* Admins approve based on protocol

**Reimbursements:**

* Member spends own money for collective purpose
* Submits receipt and justification
* If aligned with protocol allocation, approved
* If not, can be approved if admins consensus (2 of 3)

**Grants to External Projects:**

* If aligned with purpose
* Can be approved by admins
* Visible on transparent budget

#### 8.4 Host Relationship

**Open Collective Europe:**

* Holds our funds legally
* Processes all payments
* Handles tax reporting
* Ensures compliance
* Charges hosting fee
* Provides platform access

**We must:**

* Follow OC Europe policies
* Ensure expenses align with purpose
* Provide adequate documentation
* Respond to OC Europe inquiries
* Maintain active collective (transactions, updates)

**Host can:**

* Request additional documentation
* Freeze collective for policy violations
* Remove collective if terms violated
* Require compliance verification

***

### 9. Member Experience

#### 9.1 For Regular Members

**Participating:**

1. Submit recognition via protocol interface
2. Declare your needs
3. Declare capacity perspectives (what resources you see)
4. View collective-recognition-shares for different lenses
5. See allocations and rationale on Open Collective

**Receiving Allocations:**

1. Protocol computes allocation based on your collective-recognition-share
2. Admin creates expense for you on Open Collective
3. You receive email notification
4. Submit documentation (invoice, work product, receipts)
5. Admin approves expense
6. Receive payment via your preferred method
7. Transaction visible publicly on Open Collective

#### 9.2 For Contributors/Donors

**Contributing:**

1. Visit collective on opencollective.com/\[collective-name]
2. See transparent budget and impact
3. View allocation methodology (collective-recognition-shares)
4. Decide contribution amount and frequency
5. Contribute via credit card, PayPal, or bank transfer
6. Receive instant receipt

**Tracking Impact:**

1. See how your contribution was allocated
2. View expenses funded by your contribution
3. Understand allocation rationale (recognition shares)
4. Observe collective activities and outcomes
5. Continue contributing if aligned with impact

#### 9.3 For Admins

**Daily:**

* Check for expense submissions
* Verify protocol allocations
* Approve qualifying expenses
* Respond to member questions

**Weekly:**

* Review MRD computation output
* Update admin composition if needed
* Post collective updates

**Quarterly:**

* Review protocol parameters
* Consider Decider proposals
* Evaluate collective health
* Report to contributors

**As Needed:**

* Communicate with Open Collective Europe
* Handle edge cases
* Update collective description
* Manage platform settings

***

### 10. Advantages of This Model

#### 10.1 Legal and Administrative

✓ **No incorporation needed** - OC Europe provides legal status\
✓ **No bank account needed** - OC Europe holds funds\
✓ **No tax compliance burden** - OC Europe handles\
✓ **No accounting overhead** - OC Europe maintains books\
✓ **No insurance/liability** - Covered by OC Europe\
✓ **No lawyer fees** - OC Europe manages legal

#### 10.2 Operational

✓ **Fast setup** - Start immediately, no registration delay\
✓ **Global payments** - Stripe, Wise, PayPal, bank transfers\
✓ **Multiple currencies** - Accept contributions in any currency\
✓ **Professional platform** - Clean interface, automated receipts\
✓ **Event ticketing** - Built-in if needed\
✓ **Recurring contributions** - Automated monthly/yearly support

#### 10.3 Governance

✓ **No traditional governance** - Computational protocols decide\
✓ **No voting needed** - Recognition patterns determine allocation\
✓ **No committees** - Admins execute mechanically\
✓ **No approval processes** - Membership emerges from MRD\
✓ **No politics** - Math doesn't have opinions\
✓ **No gatekeeping** - Recognition is earned, not granted

#### 10.4 Transparency

✓ **Public budget** - Everything visible on Open Collective\
✓ **Clear rationale** - Allocation logic explained\
✓ **Real-time tracking** - Contributors see impact live\
✓ **Auditable protocol** - Anyone can verify computations\
✓ **Trust through transparency** - No hidden decisions

***

### 11. Limitations and Considerations

#### 11.1 What We Can't Do

❌ **Independent legal identity** - We operate under OC Europe\
❌ **Own bank account** - Funds held by OC Europe\
❌ **Own contracts** - Must go through OC Europe\
❌ **Own property** - Cannot hold assets directly\
❌ **Own IP** - Intellectual property held by OC Europe or members individually\
❌ **Legal standing** - Cannot sue/be sued independently

#### 11.2 Host Dependency

* Dependent on Open Collective Europe's policies
* Must follow their rules and procedures
* Subject to their fee structure
* Risk if OC Europe changes terms or closes
* Limited recourse if disagreement with host

**Mitigation:**

* OC Europe is established and stable
* Can switch hosts if needed (requires fund transfer)
* Can become Independent Collective later (requires legal entity)
* Protocol is portable (can move to different infrastructure)

#### 11.3 Payment Processing

* Dependent on Stripe, Wise, PayPal availability
* Subject to their terms and fees
* Some countries not supported
* Payment delays possible
* Currency conversion fees

#### 11.4 Protocol Trust

* Members must trust protocol computations
* Requires understanding of how MRD works
* Recognition patterns can be gamed theoretically
* Requires active participation for meaningful recognition

**Mitigation:**

* Protocol is transparent and auditable
* Recognition gaming hurts gamer's own allocations
* Natural correction through recognition decrease
* Community norms enforce genuine participation

***

### 12. Evolution and Flexibility

#### 12.1 Changing Parameters

**Via Decider Process:**

* MRD threshold
* Protocol computation frequency
* Allocation algorithms
* Compliance filter logic
* Collective purpose (within OC Europe limits)

**Via Admin Consensus:**

* Open Collective settings
* Transparency preferences
* Communication approaches
* Documentation requirements

#### 12.2 Scaling

**As Collective Grows:**

* More members → richer recognition network
* More contributions → larger allocations possible
* More needs → protocol prioritizes by recognition shares
* More providers → multiple capacity lenses emerge

**Network Effects:**

* Larger network = more enabling relationships
* More recognition data = better allocation accuracy
* More transparency = higher contributor trust
* More impact = attracts more contributors

#### 12.3 Future Options

**If Needs Change:**

**Option A: Stay Hosted**

* Continue with Open Collective Europe
* Simplest and cheapest
* Good for most collectives

**Option B: Switch Hosts**

* Move to different fiscal host
* Might offer specialized services
* Transfer funds on Open Collective

**Option C: Become Independent Collective**

* Establish own legal entity (e.g., Swiss Verein, US 501(c)(3))
* Open own bank account
* Keep using Open Collective for transparency
* More control, more responsibility

**Option D: Become Fiscal Host**

* Register as fiscal host on Open Collective
* Host other collectives under your legal entity
* Requires significant administrative capacity
* Generates hosting fee revenue

***

### 13. Contact and Support

**Collective Admins:**

* Ruzgar Imski: <ruzgar@playnet.lol>
* Kern Mangan Walker: <kernmanganwalker@gmail.com>
* David Rug: <david.rug98@icloud.com>

**Open Collective Europe:**

* Website: [opencollective.com/europe](https://opencollective.com/europe)
* Email: <hello@opencollective.com>
* Documentation: [docs.opencollective.com](https://docs.opencollective.com)

**Our Resources:**

* Open Collective: [opencollective.com/playnet](https://opencollective.com/playnet)
* Protocol Interface: [interplaynetary.github.io/free-association](https://interplaynetary.github.io/free-association)
* Documentation: [playnet.gitbook.io](https://playnet.gitbook.io)
* GitHub: [github.com/interplaynetary](https://github.com/interplaynetary)
* Chat: <https://t.me/interplaynetary>

***

### Appendix A: Protocol Specifications

**Membership Module:**

* [playnet.gitbook.io/docs/membership-module](https://playnet.gitbook.io/docs/membership-module)
* \[Link to recognition submission interface]
* \[Link to membership computation code]

**Resource Allocation Protocol:**

* [playnet.gitbook.io/docs/collective-recognition](https://playnet.gitbook.io/docs/collective-recognition)
* \[Link to capacity declaration spec]
* \[Link to need declaration spec]
* \[Link to collective-recognition-share computation]

**Decider Process:**

* [playnet.gitbook.io/docs/decider/decider](https://playnet.gitbook.io/docs/decider/decider)
* \[Link to proposal template]
* \[Link to voting interface]

***

### Appendix B: Open Collective Europe Policies

**Must Comply With:**

* \[Link to OC Europe terms of fiscal sponsorship]
* \[Link to OC Europe acceptable use policy]
* \[Link to Open Collective platform terms]
* \[Belgian non-profit law]
* \[EU data protection (GDPR)]

**Hosting Fees:**

* Standard: 5% of incoming contributions
* May vary based on agreement
* Visible on every transaction

**Payment Processing Fees:**

* Stripe: \~2.9% + $0.30 per transaction
* Wise: Variable by currency/country
* PayPal: \~3.5% per transaction
* Bank transfer: Variable by bank

***

### Appendix C: Glossary

**Collective:** Group coordinating on Open Collective\
**Fiscal Host:** Legal entity holding funds and providing admin (OC Europe for us)\
**MRD:** Mutual Recognition Density - computational membership score\
**Recognition:** Your acknowledgment of contributions to your self-actualization\
**Mutual Recognition:** Min of bidirectional recognition between two members\
**Collective-Recognition-Share:** Your proportion of mutual recognition within a set\
**Capacity Declaration:** Statement of resources available for allocation\
**Need Declaration:** Statement of what you need for your work\
**Provider:** Someone with actual resources who can allocate\
**Allocation:** Distribution of resources from provider to need\
**Compliance Filter:** Maximum amount that can be allocated to a member\
**Decider:** Structured decision process for parameter changes\
**Admin:** Collective member with Open Collective admin access\
**Expense:** Submitted payment request on Open Collective\
**Contribution:** Incoming donation/payment to collective

***

**Version:** 1.0\
**Last Updated:** \[Date]\
**Next Review:** \[Date]