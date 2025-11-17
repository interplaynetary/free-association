# **Free Association Coalition**

**Draft: Participation Framework b1 v0.43**  
**Result of Informal Association COP30 2025**  
**Drafted by:** Initial working group convened at COP30 2025 coordination sessions  
**Contributors:** Coalition secretariat members and early adopter organizations

This coalition consists of entities experimenting with piloting **Free Association.**

The **Free Association Coalition** (FAC) proposes a radical re-engineering of how collective action (Mutirão) and resource allocation can be coordinated.

The key insight is separating:

1. **Recognition** (who/what contributes to my goals)  
2. **State declaration** (what is, what I have/need)  
3. **Derivation** (what we can infer collectively)  
4. **Allocation** (how we divide our capacities)

**Participants can:**

* recognize  
  * who/what is a member of which organization (ids: universal-unique-identifier)  
    * subscribe to the membership recognitions of others to inform their perspective of organization membership  
      * examples:  
        * \<Org\> :  \<member-ids\>  
        * WHO : \<member-ids\>  
        * UNDP : \<memberids\>  
        * …

* recognize  
  * who contribute to the realization of your priorities and satisfaction of your needs  
    * \[total-recognition to distribute across entities: 0 to 100%\]  
      * examples:  
        * \<recognizer\> : \<%-of-total-recognition\> \-\> \<attributed-to\>  
        * WHO : 12% \-\> Doctors without Borders  
        * UNDP : 5% \-\> UNICEF  
        * …  
  * capacities  
    * examples:  
      * \<Provider\> | \<Type\> | \<Quantity\> | \<Unit\> | \<Capacity-Source\>  
      * WHO | Money | 50M | Dollars | Revenue   
      * UNDP | Money | 10B | Dollars | Donations   
      * UNICEF | Technical Support | 500 | Hours | Tech-Staff  
      * …  
  * needs  
    * examples:  
      * \<Recipient\> | \<Type\> | \<Quantity\> | \<Unit\> | \<Need-Source\>  
      * Zimbabwe | Money | 50M | Dollars | Disaster-Relief   
      * Tanzania | Money | 10B | Dollars | Climate-Transition  
      * UNDP | Technical Support | 1000 | Hours | Tech-Staff  
      * …  
  * environmental data  
    * examples:  
      * \<Scope\> | \<Variable\> | \<Value\> | \<Unit\> | \<Source\>  
      * Space-Time-Coord-A | Temperature | 30 | Celsius | Weather-Station-1   
      * Space-Time-Coord-B | Sea-Level | 1.2 | Meters-Above-Mean | Tide-Gauge-3   
      * …  
  * qualities of entities/resources  
    * examples:  
      * \<Entity\> | \<Quality\> | \<Value\> | \<Assessment-Source\>  
      * Solar-Panel-Project | Implementation-Readiness | High | Technical-Review  
      * Community-Org-X | Local-Trust-Level | Verified | Community-Survey  
      * Infrastructure-Asset | Climate-Resilience | Medium | Engineering-Assessment  
* derive  
  * data from local and network-data  
    * examples:  
      * \<derivations and their sources\>  
      * filters and their applications  
* derive  
  * data from local and network-data  
    * examples:  
      * distributions  
        * examples:  
          1. recognition  
             1. Total Recognition per Entity \= 100%  
             2. Recognition allocated as percentages/portions, is non-transferable, and dynamically adjustable  
          2. mutual-recognition  
             1. Calculated as the lower of the recognition percentages that two entities assign to each other  
             2. MR(entity-a, entity-b) \= min(  
                recognition-a-attributes-to-b,  
                recognition-b-attributes-to-a  
                )  
          3. organizational-recognition  
             1. Each member's share \= their total-mutual-recognition across all mutual-relations with organization members / total-mutual-recognition-in-organization  
      * capacities  
      * needs  
      * environmental estimates  
      * goals  
      * offers  
      * any other data  
* propose/offer/allocate  
  * using protocols of your choice

**Secretariat Purpose & Governance:**

* The Secretariat is a council governed by the coalition's adopted protocols. Its purpose is to offer open-source solutions to support coalition participants.

**Secretariat must:**

* invite  
  * its members to its assembly  
* assemble  
  * at least once per year  
* decide  
  * via adopted decision-making protocol  
* maintain (append only immutable public)  
  * record of its activity and decisions  
  * registry of its members  
  * registry of coalition participants it recognizes  
    * with one email / public-key per member as designated contact point

**Secretariat can:**

* express  
  * proposals  
  * statements  
* invite  
  * others to join the secretariat  
  * consultants to advise the secretariat  
* allocate  
  * assets allocated to the secretariat's custody

**Secretariat Member can:**

* express  
  * proposals  
  * positions towards proposals according to the secretariat's decision-making protocol:  
    * support  
    * challenge (raise concerns)  
    * oppose  
    * abstain

**General Information:** openassociation.org  
**Documentation:** docs.openassociation.org  
**Coalition Inquiries:** coalition@openassociation.org  
**Secretariat Record:** [record.openassociation.org](http://record.openassociation.org)

**Drafting Process:**  
This framework is emerging through iterative refinement during informal coordination sessions at COP30 2025. The structure prioritizes sovereignty, minimal coordination overhead, and interoperability. Feedback cycles are incorporating insights from potential member organizations spanning UN agencies, national governments, and civil society networks.

**Next Steps:**

* **Set up Contact Registration Infrastructure**  
* **Founding Member Contact Registration** \- Member 1 registers their contact information and PGP public key  
* **Founding Member Contact Registration** \- Member 2 registers their contact information and PGP public key  
* **Founding Member Contact Registration** \- Member 3 registers their contact information and PGP public key  
* **Initial Secretariat Membership Declaration \-** Formal declaration that these three members form the Secretariat  
* **Founding Declaration Statement** \- Official founding statement declaring the establishment of the Free Association Coalition Secretariat at COP30 2025  
* **Proposal to Adopt Decision-Making Protocol** \- Member 2 proposes adopting the Iterative Consensus Protocol as the Secretariat's decision-making mechanism  
    
  ***\<The following is contingent on the specific Decision-Making Protocol adopted\>***  
    
* **Support Expression from Member 1** \- Member 1 expresses full support (weight: 1.0) for the protocol proposal  
* **Support Expression from Member 3** \- Member 3 expresses full support (weight: 1.0) for the protocol proposal  
* **Support Expression from Member 2 (Proposer)** \- Member 2 (the proposer) expresses full support (weight: 1.0) for their own proposal  
* **Decision Outcome — Protocol Adoption** \- The protocol is adopted via unanimous support (3.0 aggregate weight, early adoption path)  
* **Protocol Adoption Record (Formal)** \- Formal record of the Iterative Consensus Protocol v1.0.0 adoption with content hash  
* **Framework Version Record \-** Records Participation Framework version b0v0.43 as the initial bootstrap version  
* **Invitation to Founding Assembly \-** Member 3 invites all members to the founding assembly  
* **Assembly Response — Member 1** \- Member 1 accepts the assembly invitation  
* **Assembly Response — Member 2** \- Member 2 accepts the assembly invitation  
* **Assembly Response — Member 3** \- Member 3 accepts the assembly invitation  
* **Founding Assembly Minutes \-** Official minutes from the founding assembly including decisions made, action items, and next assembly date 

**APPENDIX**

**Benefit:**

* Partner-Discovery: If you don't have any partners with which you could form mutual-recognition at the moment, the coalition serves as a platform for partner discovery

* **benefits:**  
    * critical or timely resource sharing:  
    * Sharing resource capacities is separate from promising or committing resources.  
    * enhanced information sharing:  
    * Covers all the following areas: existing information; data collection capabilities; future potential to gather information (personnel, facilities, equipment).  

**Principles:**

* **Solidarity:** Coalition members are invited to offer support to each other in protocol implementation, and organize their cooperation in alignment with the principles of freedom of association and freedom of organizational expression.

**Technical Clarifications:**

**Recognition Types:**

* **Contribution Recognition** (-100% to 100%): Evaluates whether an entity positively or negatively impacts your goals/needs. Negative recognition acknowledges harmful or obstructive relationships. Used for evaluation and assessment.

* **Allocation Weight** (0-100%, non-transferable, dynamically adjustable): Determines how to divide shared capacities among recognized contributors. Only positive values—you allocate to those you support. Used in resource distribution formulas.

**Derivation Algorithms:**

* **Mutual Recognition:** Calculated as minimum of bidirectional recognition: `min(A→B recognition, B→A recognition)`. Identifies symmetric cooperation relationships.

* **Organizational Recognition:** Aggregates individual recognitions weighted by organizational membership. Formula: `Σ(member_recognition × member_weight) / total_members` where member_weight reflects their standing within the organization.

* **Allocation Formula:** Resources distributed proportionally to allocation weights among recognized entities. For capacity C allocated among entities E with weights W: `entity_share = (W_entity / ΣW_all) × C`

**Identity & Verification:**

* **UUIDs:** Universal Unique Identifiers generated using UUID v4 standard (RFC 4122). Participants self-generate and register their identifiers.

* **Contact Verification:** Email addresses verified via confirmation link; public keys verified through challenge-response signing or cross-referenced with existing key servers (PGP, X.509 CAs, DIDs).

**Registry vs. Records:**

* **Records:** Append-only, immutable, timestamped entries in participant-owned namespaces. Each participant writes records only to their own space; all members read from all participant spaces.

* **Registries:** Derived views computed from records. Current state representation (e.g., "who is currently a member" derived from all membership_update records aggregated across all participants).

**Bootstrap Process:**

1. **Initial Participants:** Self-organized group declares itself as the founding secretariat
2. **First Protocol:** Founding secretariat adopts initial decision-making protocol via consensus
3. **Registry Initialization:** Members register their UUIDs and contact information  
4. **First Assembly:** Convened to formalize structure and invite additional participants
5. **Operational Phase:** Regular operation begins with annual assembly cycle

**Temporal Aspects & State Management:**

* **Record Validity:** State declarations include `valid_until` or `expiry` timestamps. Expired records don't automatically delete—they remain in history but are excluded from current state derivations.

* **Recognition Currency:** Recognition distributions remain valid until superseded by newer distributions from the same issuer. No automatic expiry; participants update as relationships evolve.

* **Framework Versioning:** Each framework version tracked via `framework_version` records. Participants may operate on different versions simultaneously; interoperability maintained through schema transformations.

* **Retroactive Amendments:** Amendments don't alter original records (immutability). Instead, `record_amendment` records reference originals and provide corrected interpretations. Derived views apply latest amendments.

**Error Handling & Validation:**

* **Format Validation:** All records validated against schemas before acceptance. Invalid records receive `validation_report` with status="invalid" and detailed error descriptions.

* **Logic Validation:** Checks for consistency (e.g., recognition percentages within bounds, referenced UUIDs exist). Warnings issued for anomalies but don't block acceptance.

* **Authority Validation:** Verifies issuer has authority for the action (participant can only write to their own namespace). Unauthorized writes automatically rejected by topology.

* **Technical Conflict Resolution:** When concurrent record edits occur, CRDT semantics provide automatic field-level conflict resolution via last-write-wins with timestamp tie-breaking.

* **Dispute Resolution:** When semantic conflicts arise (e.g., two entities claim same identifier, factual disagreements), `dispute` mechanism invoked. Secretariat applies dispute resolution protocol to determine authoritative interpretation.

**Assembly Mechanics:**

* **Quorum:** Decision-making protocol specifies quorum requirements (typically majority of registered members).

* **Participation Modes:** Physical presence, video conference, and asynchronous participation all supported. Assembly minutes record participation method per attendee.

* **Assembly Types:**
  - **Annual:** Mandatory yearly gathering, reviews operations and updates protocols
  - **Emergency:** Convened for urgent matters, expedited decision timeline
  - **Working Group:** Focused sub-assembly addressing specific topics, reports to main assembly

* **Attendance Confirmation:** Invitations require responses. Attendees confirmed when they post `invitation_response` with response="accept". Minutes use attendance list from responses.

**Subscription Mechanism:**

* **Data Stream Types:** Participants can subscribe to membership changes, recognition updates, state declarations, or computed derivations from any entity.

* **Filters:** Subscriptions include optional filter criteria (e.g., "only recognition >10%", "only capacity offers of type 'funding'"). Filters reduce notification volume.

* **Notification Methods:** 
  - **Webhook:** Real-time HTTP callbacks when matching records posted
  - **Poll:** Subscriber queries for updates at their convenience
  
* **Subscription Privacy:** Source entities see who subscribes to their data streams. Enables relationship awareness and reciprocal subscriptions.

* **Implementation:** Subscribers post `subscription` records to their own namespace. Implementations monitor source entity record spaces and deliver matching records according to specified notification method. See `format.md` for subscription lifecycle details.

**Data Topology:**

* **Participant-Centric:** Each participant maintains their own record space; no shared write location exists
* **CRDT Conflict Resolution:** Concurrent record submissions resolve automatically via field-level timestamps
* **Local Aggregation:** Each member aggregates records from all participants into local derived views
* **Network Partition Tolerance:** Local-first architecture allows continued operation during connectivity issues
