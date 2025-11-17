# **Participation Framework Appendix**

**Status:** Draft v0.1 (November 2025)  
**Part of:** Participation Framework b1 v0.43

---

## **A. Coalition Benefits**

### **Benefit:**

* Partner-Discovery: If you don't have any partners with which you could form mutual-recognition at the moment, the coalition serves as a platform for partner discovery

* **benefits:**  
    * critical or timely resource sharing:  
    * Sharing resource capacities is separate from promising or committing resources.  
    * enhanced information sharing:  
    * Covers all the following areas: existing information; data collection capabilities; future potential to gather information (personnel, facilities, equipment).  

**Principles:**

* **Solidarity:** Coalition members are invited to offer support to each other in protocol implementation, and organize their cooperation in alignment with the principles of freedom of association and freedom of organizational expression.

---

## **B. Technical Clarifications**

### **Recognition Types:**

* **Contribution Recognition** (-100% to 100%): Evaluates whether an entity positively or negatively impacts your goals/needs. Negative recognition acknowledges harmful or obstructive relationships. Used for evaluation and assessment.

* **Allocation Weight** (0-100%, non-transferable, dynamically adjustable): Determines how to divide shared capacities among recognized contributors. Only positive values—you allocate to those you support. Used in resource distribution formulas.

### **Derivation Algorithms:**

* **Mutual Recognition:** Calculated as minimum of bidirectional recognition: `min(A→B recognition, B→A recognition)`. Identifies symmetric cooperation relationships.

* **Organizational Recognition:** Aggregates individual recognitions weighted by organizational membership. Formula: `Σ(member_recognition × member_weight) / total_members` where member_weight reflects their standing within the organization.

* **Allocation Formula:** Resources distributed proportionally to allocation weights among recognized entities. For capacity C allocated among entities E with weights W: `entity_share = (W_entity / ΣW_all) × C`

### **Identity & Verification:**

* **UUIDs:** Universal Unique Identifiers generated using UUID v4 standard (RFC 4122). Participants self-generate and register their identifiers.

* **Contact Verification:** Email addresses verified via confirmation link; public keys verified through challenge-response signing or cross-referenced with existing key servers (PGP, X.509 CAs, DIDs).

### **Registry vs. Records:**

* **Records:** Append-only, immutable, timestamped entries in participant-owned namespaces. Each participant writes records only to their own space; all members read from all participant spaces.

* **Registries:** Derived views computed from records. Current state representation (e.g., "who is currently a member" derived from all membership_update records aggregated across all participants).

### **Bootstrap Process:**

1. **Initial Participants:** Self-organized group declares itself as the founding secretariat
2. **First Protocol:** Founding secretariat adopts initial decision-making protocol via consensus
3. **Registry Initialization:** Members register their UUIDs and contact information  
4. **First Assembly:** Convened to formalize structure and invite additional participants
5. **Operational Phase:** Regular operation begins with annual assembly cycle

### **Temporal Aspects & State Management:**

* **Record Validity:** State declarations include `valid_until` or `expiry` timestamps. Expired records don't automatically delete—they remain in history but are excluded from current state derivations.

* **Recognition Currency:** Recognition distributions remain valid until superseded by newer distributions from the same issuer. No automatic expiry; participants update as relationships evolve.

* **Framework Versioning:** Each framework version tracked via `framework_version` records. Participants may operate on different versions simultaneously; interoperability maintained through schema transformations.

* **Retroactive Amendments:** Amendments don't alter original records (immutability). Instead, `record_amendment` records reference originals and provide corrected interpretations. Derived views apply latest amendments.

### **Error Handling & Validation:**

* **Format Validation:** All records validated against schemas before acceptance. Invalid records receive `validation_report` with status="invalid" and detailed error descriptions.

* **Logic Validation:** Checks for consistency (e.g., recognition percentages within bounds, referenced UUIDs exist). Warnings issued for anomalies but don't block acceptance.

* **Authority Validation:** Verifies issuer has authority for the action (participant can only write to their own namespace). Unauthorized writes automatically rejected by topology.

* **Technical Conflict Resolution:** When concurrent record edits occur, CRDT semantics provide automatic field-level conflict resolution via last-write-wins with timestamp tie-breaking.

* **Dispute Resolution:** When semantic conflicts arise (e.g., two entities claim same identifier, factual disagreements), `dispute` mechanism invoked. Secretariat applies dispute resolution protocol to determine authoritative interpretation.

### **Assembly Mechanics:**

* **Quorum:** Decision-making protocol specifies quorum requirements (typically majority of registered members).

* **Participation Modes:** Physical presence, video conference, and asynchronous participation all supported. Assembly minutes record participation method per attendee.

* **Assembly Types:**
  - **Annual:** Mandatory yearly gathering, reviews operations and updates protocols
  - **Emergency:** Convened for urgent matters, expedited decision timeline
  - **Working Group:** Focused sub-assembly addressing specific topics, reports to main assembly

* **Attendance Confirmation:** Invitations require responses. Attendees confirmed when they post `invitation_response` with response="accept". Minutes use attendance list from responses.

### **Subscription Mechanism:**

* **Data Stream Types:** Participants can subscribe to membership changes, recognition updates, state declarations, or computed derivations from any entity.

* **Filters:** Subscriptions include optional filter criteria (e.g., "only recognition >10%", "only capacity offers of type 'funding'"). Filters reduce notification volume.

* **Notification Methods:** 
  - **Webhook:** Real-time HTTP callbacks when matching records posted
  - **Poll:** Subscriber queries for updates at their convenience
  
* **Subscription Privacy:** Source entities see who subscribes to their data streams. Enables relationship awareness and reciprocal subscriptions.

* **Implementation:** Subscribers post `subscription` records to their own namespace. Implementations monitor source entity record spaces and deliver matching records according to specified notification method. See `format.md` for subscription lifecycle details.

### **Data Topology:**

* **Participant-Centric:** Each participant maintains their own record space; no shared write location exists
* **CRDT Conflict Resolution:** Concurrent record submissions resolve automatically via field-level timestamps
* **Local Aggregation:** Each member aggregates records from all participants into local derived views
* **Network Partition Tolerance:** Local-first architecture allows continued operation during connectivity issues

