# **Free Association Coalition Founding Process**

**Status:** Draft v0.1 (November 2025)  
**Part of:** Participation Framework b1 v0.43

## **Overview**

This document outlines the step-by-step process for establishing the Free Association Coalition Secretariat at COP30 2025. The process is designed to be transparent, auditable, and aligned with the principles of distributed governance and mutual recognition.

## **Founding Principles**

The founding process adheres to:

* **Minimal Viable Structure**: Start with essential components, evolve through experience
* **Transparency**: All actions recorded in append-only public record
* **Distributed Authority**: No single point of control or failure
* **Iterative Refinement**: Framework evolves through feedback and amendment
* **Self-Organization**: Founding members bootstrap without external authority

## **Prerequisites**

### **Before Beginning**

Founding members must have:

* ✅ Read and understood the [Participation Framework](/docs/coalition/participation-framework.md)
* ✅ Generated PGP key pairs for secure communication
* ✅ Identified organizational representative authorized to act
* ✅ Prepared initial capacity and need declarations
* ✅ Access to infrastructure for record publication

### **Technical Requirements**

* PGP key generation tool (GPG, Keybase, ProtonMail)
* Email address for contact registration
* Ability to sign and verify messages
* Access to record publication system (provided by coalition infrastructure)

## **Founding Process Steps**

### **Phase 1: Infrastructure Setup**

**Timeline:** Days 1-2

#### **Step 1.1: Set Up Contact Registration Infrastructure**

**Responsible:** Coalition technical coordinators (or self-organized by founding members)

**Actions:**
* Deploy record publication system
* Configure identity verification endpoints
* Initialize append-only record store
* Set up secure communication channels

**Outputs:**
* ✅ Record publication endpoints available
* ✅ Contact registration interface ready
* ✅ Public record viewer accessible at record.openassociation.org

**Verification:**
* Test record submission from multiple clients
* Confirm append-only semantics
* Verify public readability

---

### **Phase 2: Founding Member Registration**

**Timeline:** Days 2-4

Each founding member completes registration independently.

#### **Step 2.1: Founding Member Contact Registration**

**Responsible:** Each founding member individually

**Actions:**

1. **Generate UUID** (if not already existing)
   ```bash
   uuidgen
   # Example output: f47ac10b-58cc-4372-a567-0e02b2c3d479
   ```

2. **Prepare contact information**
   * Organization name
   * Member UUID
   * Designated email address
   * PGP public key
   * Organizational representative name (optional)

3. **Create registration record**
   ```json
   {
     "record_type": "contact_registration",
     "entity_uuid": "f47ac10b-58cc-4372-a567-0e02b2c3d479",
     "organization_name": "Climate Action Network International",
     "email": "coalition@climateactionnetwork.org",
     "pgp_public_key": "-----BEGIN PGP PUBLIC KEY BLOCK----- ...",
     "representative_name": "Dr. Maria Santos",
     "role": "Coalition Representative",
     "timestamp": "2025-11-17T10:00:00Z",
     "signature": "..."
   }
   ```

4. **Sign record with PGP key**
   ```bash
   gpg --sign --armor contact_registration.json
   ```

5. **Submit to record system**
   * POST to record publication endpoint
   * Receive confirmation and record hash
   * Verify record appears in public viewer

**Outputs:**
* ✅ Member 1 registered
* ✅ Member 2 registered
* ✅ Member 3 registered

**Minimum Founding Members:** 3 (enables meaningful mutual recognition)

---

### **Phase 3: Secretariat Formation**

**Timeline:** Day 5

#### **Step 3.1: Initial Secretariat Membership Declaration**

**Responsible:** All founding members (coordinated action)

**Actions:**

1. **Founding members reach informal agreement** to form Secretariat
   * Discussion via secure channels (encrypted email, secure meetings)
   * Consensus on initial membership (minimum 3 members)

2. **Each member publishes membership declaration**
   ```json
   {
     "record_type": "statement",
     "statement_type": "membership_declaration",
     "issuer": "f47ac10b-58cc-4372-a567-0e02b2c3d479",
     "organization": "FAC-Secretariat",
     "declares": "I declare membership in the Free Association Coalition Secretariat",
     "recognizes_members": [
       "uuid-member-1",
       "uuid-member-2",
       "uuid-member-3"
     ],
     "effective_date": "2025-11-17T10:00:00Z",
     "timestamp": "2025-11-17T10:00:00Z",
     "signature": "..."
   }
   ```

3. **Verify mutual recognition**
   * Each member appears in others' `recognizes_members` list
   * Forms closed set of founding secretariat

**Outputs:**
* ✅ Secretariat membership formally declared
* ✅ Founding members mutually recognized
* ✅ Organizational entity established

**Verification:**
* All founding members have published membership declarations
* Each member recognizes all other founding members
* Records are signed and publicly visible

---

#### **Step 3.2: Founding Declaration Statement**

**Responsible:** Any founding member (typically coordinated together)

**Actions:**

1. **Draft founding declaration**
   * Establishes the Free Association Coalition Secretariat
   * States purpose: "offer open-source solutions to support coalition participants"
   * References COP30 2025 as founding context
   * Commits to participation framework

2. **Publish founding statement**
   ```json
   {
     "record_type": "statement",
     "statement_type": "founding_declaration",
     "issuer": "FAC-Secretariat",
     "title": "Establishment of Free Association Coalition Secretariat at COP30 2025",
     "content": "We, the founding members of the Free Association Coalition Secretariat, hereby declare the establishment of this body during COP30 2025 in Belem, Brazil. Our purpose is to offer open-source solutions to support coalition participants in coordinating resource allocation through mutual recognition...",
     "founding_members": [
       "uuid-member-1",
       "uuid-member-2",
       "uuid-member-3"
     ],
     "framework_version": "b1v0.43",
     "location": "COP30, Belem, Brazil",
     "timestamp": "2025-11-17T12:00:00Z",
     "signature": "..."
   }
   ```

**Outputs:**
* ✅ Official founding declaration published
* ✅ Secretariat establishment on public record
* ✅ Framework version formally adopted

---

### **Phase 4: Decision-Making Protocol Adoption**

**Timeline:** Days 6-13 (varies by protocol chosen)

#### **Step 4.1: Proposal to Adopt Decision-Making Protocol**

**Responsible:** Any founding member (typically Member 2 in example)

**Actions:**

1. **Choose initial protocol** from available options:
   * [Iterative Consensus Protocol](/docs/coalition/secretariat/decision-making-protocol.md)
   * [Node Protocol Delegation](/docs/coalition/secretariat/node-protocol-delegation.md)

2. **Submit protocol proposal**
   ```json
   {
     "record_type": "proposal",
     "proposal_type": "secretariat_decision",
     "issuer": "uuid-member-2",
     "title": "Adopt Iterative Consensus Protocol as Secretariat Decision-Making Mechanism",
     "description": "Proposal to adopt the Iterative Consensus Protocol v1.0.0 as the official decision-making mechanism for the Free Association Coalition Secretariat",
     "proposal_content": {
       "protocol_name": "Iterative Consensus Protocol",
       "protocol_version": "v1.0.0",
       "protocol_document_hash": "sha256:...",
       "effective_date": "2025-11-20T00:00:00Z"
     },
     "timestamp": "2025-11-17T14:00:00Z",
     "signature": "..."
   }
   ```

**Outputs:**
* ✅ Protocol adoption proposal submitted
* ✅ Proposal visible to all members

---

#### **Step 4.2-4.4: Support Expression**

**Responsible:** All founding members

**Process varies by protocol being adopted:**

##### **If adopting Iterative Consensus Protocol:**

Since this is the first decision, members can use simplified unanimous consensus:

1. **Each member expresses support**
   ```json
   {
     "record_type": "position",
     "position": "support",
     "proposal_id": "uuid-of-protocol-proposal",
     "issuer": "uuid-member-1",
     "weight": 1.0,
     "comment": "I support adopting the Iterative Consensus Protocol",
     "timestamp": "2025-11-18T10:00:00Z",
     "signature": "..."
   }
   ```

2. **All three members express support**
   * Member 1: support (weight 1.0)
   * Member 2: support (weight 1.0)
   * Member 3: support (weight 1.0)

**Outputs:**
* ✅ Member 1 support recorded
* ✅ Member 2 support recorded
* ✅ Member 3 support recorded
* ✅ Unanimous support achieved (3.0 aggregate weight)

##### **If adopting Node Protocol Delegation:**

Each member submits mandate proposals (see [Node Protocol docs](/docs/coalition/secretariat/node-protocol-delegation.md))

---

#### **Step 4.5: Decision Outcome — Protocol Adoption**

**Responsible:** Any member or automated system

**Actions:**

1. **Calculate aggregate support**
   * Sum all support weights: 3.0 (unanimous)
   * Quorum met (3 of 3 members = 100%)
   * Early adoption path (no challenges or modifications)

2. **Publish decision outcome**
   ```json
   {
     "record_type": "decision_outcome",
     "proposal_id": "uuid-of-protocol-proposal",
     "outcome": "adopted",
     "adoption_path": "early_unanimous",
     "aggregate_support": 3.0,
     "total_members": 3,
     "participation_rate": 1.0,
     "adopted_version": "original",
     "effective_date": "2025-11-20T00:00:00Z",
     "timestamp": "2025-11-18T12:00:00Z",
     "signature": "..."
   }
   ```

**Outputs:**
* ✅ Protocol officially adopted
* ✅ Decision outcome recorded
* ✅ Secretariat has formal decision-making process

---

#### **Step 4.6: Protocol Adoption Record (Formal)**

**Responsible:** Secretariat (collective action)

**Actions:**

1. **Create formal protocol adoption record**
   ```json
   {
     "record_type": "protocol_adoption",
     "protocol_name": "Iterative Consensus Protocol",
     "protocol_version": "v1.0.0",
     "protocol_document_hash": "sha256:a1b2c3d4...",
     "adopted_by": "FAC-Secretariat",
     "decision_record_id": "uuid-of-decision-outcome",
     "effective_date": "2025-11-20T00:00:00Z",
     "document_url": "docs.openassociation.org/coalition/secretariat/decision-making-protocol.md",
     "timestamp": "2025-11-18T12:00:00Z",
     "signature": "..."
   }
   ```

**Outputs:**
* ✅ Protocol adoption formally recorded
* ✅ Content hash provides version verification
* ✅ Future amendments traceable

---

#### **Step 4.7: Framework Version Record**

**Responsible:** Secretariat

**Actions:**

1. **Record initial framework version**
   ```json
   {
     "record_type": "framework_version",
     "version_identifier": "b1v0.43",
     "version_type": "bootstrap",
     "framework_document_hash": "sha256:e5f6g7h8...",
     "adopted_by": "FAC-Secretariat",
     "effective_date": "2025-11-17T00:00:00Z",
     "document_url": "docs.openassociation.org/coalition/participation-framework.md",
     "notes": "Initial bootstrap version adopted during COP30 2025 founding process",
     "timestamp": "2025-11-18T12:00:00Z",
     "signature": "..."
   }
   ```

**Outputs:**
* ✅ Framework version recorded
* ✅ Baseline established for future amendments
* ✅ Document hash enables verification

---

### **Phase 5: First Assembly**

**Timeline:** Days 14-21

#### **Step 5.1: Invitation to Founding Assembly**

**Responsible:** Any founding member (typically Member 3 in example)

**Actions:**

1. **Prepare assembly invitation**
   ```json
   {
     "record_type": "invitation",
     "invitation_type": "assemble",
     "assembly_type": "founding_assembly",
     "issuer": "uuid-member-3",
     "invitees": [
       "uuid-member-1",
       "uuid-member-2",
       "uuid-member-3"
     ],
     "proposed_date": "2025-11-21T14:00:00Z",
     "duration_hours": 3,
     "location": "Virtual (Google Meet: https://meet.google.com/...)",
     "agenda": [
       "Review founding process completion",
       "Initial recognition declarations",
       "Capacity and need statements",
       "Next assembly scheduling",
       "Open discussion"
     ],
     "rsvp_deadline": "2025-11-20T12:00:00Z",
     "timestamp": "2025-11-18T15:00:00Z",
     "signature": "..."
   }
   ```

**Outputs:**
* ✅ Assembly invitation published
* ✅ All members notified
* ✅ Agenda visible

---

#### **Step 5.2-5.4: Assembly Responses**

**Responsible:** Each member individually

**Actions:**

Each member submits response:

```json
{
  "record_type": "invitation_response",
  "invitation_id": "uuid-of-invitation",
  "responder": "uuid-member-1",
  "response": "accept",
  "attendance_mode": "virtual",
  "notes": "Looking forward to the founding assembly",
  "timestamp": "2025-11-19T10:00:00Z",
  "signature": "..."
}
```

**Outputs:**
* ✅ Member 1 acceptance recorded
* ✅ Member 2 acceptance recorded
* ✅ Member 3 acceptance recorded
* ✅ Assembly confirmed (all members attending)

---

#### **Step 5.5: Founding Assembly Convenes**

**Responsible:** All founding members

**Actions:**

**Assembly Activities:**

1. **Opening** (15 min)
   * Roll call and attendance confirmation
   * Agenda review and approval

2. **Founding Process Review** (30 min)
   * Verify all founding steps completed
   * Review public record entries
   * Confirm framework and protocol adoption

3. **Initial Recognition Declarations** (45 min)
   * Each member presents their initial recognition distributions
   * Discussion of recognition rationale
   * Submission of recognition records

4. **Capacity and Need Statements** (45 min)
   * Each member presents capacities they can share
   * Each member presents needs they have
   * Discussion of potential matches

5. **Next Steps and Scheduling** (30 min)
   * Schedule next regular assembly
   * Identify working group needs
   * Assign action items

6. **Open Discussion** (15 min)
   * Questions and concerns
   * Proposals for future consideration

**During Assembly:**

Members may submit records in real-time:

* Recognition declarations
* Capacity offers
* Need statements
* Proposals for next assembly
* Statements of intent

---

#### **Step 5.6: Founding Assembly Minutes**

**Responsible:** Designated recorder (rotates among members)

**Actions:**

1. **Compile assembly minutes**
   ```json
   {
     "record_type": "assembly_minutes",
     "assembly_type": "founding_assembly",
     "assembly_id": "uuid-of-invitation",
     "date": "2025-11-21T14:00:00Z",
     "attendees": [
       "uuid-member-1",
       "uuid-member-2",
       "uuid-member-3"
     ],
     "attendance_mode": {
       "uuid-member-1": "virtual",
       "uuid-member-2": "virtual",
       "uuid-member-3": "virtual"
     },
     "decisions_made": [
       {
         "decision": "Next regular assembly scheduled for 2026-01-15",
         "decision_record_id": "uuid-of-decision"
       }
     ],
     "action_items": [
       {
         "action": "Member 1 to draft pilot implementation proposal",
         "assignee": "uuid-member-1",
         "deadline": "2025-12-01"
       },
       {
         "action": "Member 2 to prepare technical infrastructure documentation",
         "assignee": "uuid-member-2",
         "deadline": "2025-12-15"
       }
     ],
     "next_assembly": {
       "date": "2026-01-15T14:00:00Z",
       "type": "regular_assembly"
     },
     "minutes_url": "record.openassociation.org/minutes/founding-assembly-2025-11-21",
     "timestamp": "2025-11-21T17:30:00Z",
     "signature": "..."
   }
   ```

**Outputs:**
* ✅ Minutes published to public record
* ✅ Decisions formally recorded
* ✅ Action items tracked
* ✅ Next assembly scheduled

---

## **Founding Process Complete**

### **Verification Checklist**

At completion, the public record contains:

* ✅ **Contact registrations** for all founding members (min. 3)
* ✅ **Membership declarations** establishing the Secretariat
* ✅ **Founding declaration** stating purpose and context
* ✅ **Protocol adoption** decision with supporting records
* ✅ **Framework version** record establishing baseline
* ✅ **Assembly invitation** and responses
* ✅ **Assembly minutes** documenting first gathering

### **Secretariat Status**

The Secretariat is now officially established with:

* ✅ Identified founding members
* ✅ Adopted decision-making protocol
* ✅ Public record of all founding actions
* ✅ Schedule for ongoing assemblies
* ✅ Initial recognition network forming

### **Operational Capabilities**

The Secretariat can now:

* ✅ Accept new member applications
* ✅ Process proposals via adopted protocol
* ✅ Coordinate coalition participants
* ✅ Publish official statements
* ✅ Allocate resources (if received)
* ✅ Convene regular assemblies

---

## **Post-Founding Activities**

### **Immediate (Weeks 1-4)**

* **Onboard additional members** responding to coordination sessions
* **Publish implementation guidelines** for coalition participants
* **Establish working groups** for specific focus areas
* **Launch pilot implementations** with early adopters

### **Near-Term (Months 2-3)**

* **First regular assembly** (as scheduled in founding minutes)
* **Framework amendments** based on early experience
* **Technical infrastructure improvements** based on usage
* **Outreach to potential participants**

### **Long-Term (Months 4-12)**

* **Annual general assembly** (per participation framework requirement)
* **Protocol evaluation and potential evolution**
* **Scale pilot implementations**
* **Publish impact assessments and learnings**

---

## **Appendix: Record Types Reference**

All record types used in founding process:

| Record Type | Purpose | Example |
|-------------|---------|---------|
| `contact_registration` | Register member contact info and PGP key | Step 2.1 |
| `statement` (membership_declaration) | Declare secretariat membership | Step 3.1 |
| `statement` (founding_declaration) | Formal establishment statement | Step 3.2 |
| `proposal` | Propose protocol adoption | Step 4.1 |
| `position` | Express support/challenge/oppose | Step 4.2-4.4 |
| `decision_outcome` | Record decision result | Step 4.5 |
| `protocol_adoption` | Formally record adopted protocol | Step 4.6 |
| `framework_version` | Record framework version | Step 4.7 |
| `invitation` | Invite to assembly | Step 5.1 |
| `invitation_response` | Accept/decline assembly | Step 5.2-5.4 |
| `assembly_minutes` | Document assembly outcomes | Step 5.6 |

Full record format specifications: [record format documentation](/docs/coalition/secretariat/record/format.md)

---

## **Support and Questions**

For questions about the founding process:

* **Technical**: coalition@openassociation.org
* **Process**: [coordination sessions](/docs/coalition/coordination-sessions.md)
* **Documentation**: docs.openassociation.org

