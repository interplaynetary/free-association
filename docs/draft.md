## **![][image1]**

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
      * sources for deriving 
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
             1. Each member’s share \= their total-mutual-recognition across all mutual-relations with organization members / total-mutual-recognition-in-organization  
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
  * assets allocated to the secretariat’s custody

**Secretariat Member can:**

* express  
  * proposals  
  * positions towards proposals according to the secretariat’s decision-making protocol:  
    * support  
    * challenge (raise concerns)  
    * oppose  
    * abstain	

**General Information:** openassociation.org  
**Documentation:** docs.openassociation.org  
**Coalition Inquiries:** [coalition@openassociation.org](mailto:coalition@openassociation.org)  
**Secretariat Record:** [record.openassociation.org](http://record.openassociation.org)

---

**Decision-Making Protocol: Node Protocol Delegation**

**Participants can:**

* bootstrap nodes with initial functions during setup phase  
  * examples:  
    * `bootstrap.addMethod('allocateFunds', function(amount) { ... })`  
    * `bootstrap.addMember('Climate-Action-Network')`  
* create proposals with cross-node method execution  
  * examples:  
    * `proposal.addAction(nodeA, 'increaseFunding', 'allocateFunds', [1000])`  
    * `proposal.addAction(nodeB, 'acceptFunding', 'receiveFunds', [1000])`

**Nodes:**

* Its **members** are **delegates** holding **mandates**.  
* Each mandate is a **live proposal** containing:  
  * `methodName`: The executable function to call  
  * `methodArgs`: Parameters for execution  
  * `supporters`: Real-time list of supporting members  
  * `target-node`: The node where methods execute

**Nodes must:**

* maintain a registry of recognized delegates and their current voting power  
  * `<delegate> | <voting-power> | <source-node>`  
  * `Alice Chen | 3 | Climate-Action-Network`  
  * `Dr. Benoit | 2 | Research-Coalition-X`  
* process proposals  
  * A proposal passes when it gathers support from delegates representing a **quorum of \> 50% of the total voting power** in the node.

**Delegates can:**

* express support for a proposal on behalf of their mandate  
  * The weight of their support equals their current `voting-power`.  
* be revoked  
  * If the support for their mandate in their source-node is withdrawn, their `voting-power` and authority in the secretariat are automatically and immediately updated.

**Delegates hold:**

* **Executable authority** \- not just voting power, but the ability to trigger specific method calls  
* **Dynamically calculated influence** based on current mandate support  
* **Programmatically revocable access** that can be automatically triggered

**A Mandate is a Live Proposal.**

* A mandate is a proposal, passed by a source-node, that specifies:  
  * `delegate`: The entity representing the source-node.  
  * `target-node`: The node the delegate is being sent to (e.g., the Secretariat).  
  * `supporters`: The list of members in the source-node who support this delegation.  
  * `scope` (Optional): Any instructions or constraints on the delegate's authority.

**Accountability & Enforcement:**

* A source-node may send an **observer** with a delegate to monitor adherence to the mandate.  
* A source-node may **signal a violation** of a mandate.  
  * If a violation is confirmed by the source-node, it triggers an automatic revocation of the delegate.

**Inter-Node Relations:**

* Decisions of the target-node are binding on its member-nodes.  
* To not accept and proceed to implementing a decision is to revoke one's membership in the target-node.  
* The target-node may vote to revoke a member-node's membership.

**The Nodes Protocol technically enforces:**

* **One delegate per source-target node pair** via code constraints  
* **Method execution only during approved proposal processing** via proxy security  
* **Real-time voting power updates** as mandate support changes  
* **Revocable access** to delegates through JavaScript Proxy revocation  
* **Streaming proposal status** via async generator pattern

---

**Drafting Process:**  
This framework is emerging through iterative refinement during informal coordination sessions at COP30 2025\. The structure prioritizes sovereignty, minimal coordination overhead, and interoperability. Feedback cycles are incorporating insights from potential member organizations spanning UN agencies, national governments, and civil society networks.

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

### Implications and Significance

* **Sovereignty and Interoperability:** Participants retain full control over their own data, recognitions, and priorities. They choose whose data to subscribe to. The system enables collaboration without requiring surrender of autonomy.  
* **Radical Transparency:** The append-only public record and the explicit nature of recognitions and mandates make the "politics" of the coalition transparent and auditable.  
* **Automation of Cooperation:** The vision is to have a significant portion of capacity/resource allocation (funding, technical support) be automatically triggered by the state of the network's derived data, drastically reducing transaction costs and delays.

### Informal Working Group Coordination Session Offers

* **Virtual: November 17-21**  
  * https://meet.google.com/smf-eopi-ipi  
* **New York, United Nations: November 17-21**  
  * **Contact-point:**   
    * Lubna Dajani, Co-Chair USA Catalyst Nowi | whatsapp: \+1 (201) 982-0934  
* **COP30 Belem Blue Zone: November 10-21, 2025 @ 10h-16h**  
  * **Contact-point:**   
    * Tom Guimberteau | whatsapp: \+33 7  77 86 72 13  
* **COP30 Belem Green & Free Zone: November 10-21, 2025 @ 16h-24h**  
  * **Contact-point:**   
    * Jacob Lucas | whatsapp: \+49 1516 8126224  
* **Climate Finance: November**  
  * Heidi Cuppari | whatsapp: \+1 (917) 699-8351

# **![][image2]**

# **Free Association**

**Total Recognition (100%):** Each participant has a fixed "budget" of recognition to distribute. This forces prioritization and trade-offs. Recognition is non-transferable and dynamically-adjustable.


**Mutual Recognition (MR):** The min() function creates a natural incentive for reciprocity and relationship-building. A one-sided relationship (where A recognizes B highly, but B does not recognize A) is valued at the lower amount, discouraging free-riding and encouraging mutual engagement.

**The system naturally promotes accurate recognition through mathematical necessity:**  
Entities define their goals/priorities subjectively, but achieving them depends on objective access to resources and partnerships. Recognition accuracy is validated through outcomes:

* **Effective Recognition:** Recognition that, when acted upon, connects you with resources and partnerships that genuinely advance your goals (validated by positive outcomes)  
* **Ineffective Recognition:** Recognition that fails to connect you with beneficial resources or creates harmful dependencies (invalidated by negative outcomes)

**Mathematical Consequence:**  
**For any participant:**  
**Total Recognition \= 100%**  
**Total Recognition \= Effective Recognition \+ Ineffective Recognition**

**Therefore:**  
**↑ Ineffective Recognition → ↓ Effective Recognition**  
   **→ ↓ Mutual Recognition with Actually Beneficial Partners**  
      **→ ↓ Access to Actually Beneficial Resources**  
         **→ ↓ Goal Achievement**  
            **→ Natural incentive to correct recognition accuracy**

**Key Implication:** The system creates natural incentives for accurate recognition. Inflating or misattributing recognition only decreases connection to beneficial partners and resources. Entities that maintain accurate recognition patterns receive better-aligned resources and achieve better outcomes.