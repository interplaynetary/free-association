<div align="center">
  <img src="../../static/logo.png" alt="Free Association Coalition Logo" width="400"/>
</div>

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

---

**See [Appendix](appendix.md) for detailed technical clarifications and coalition benefits.**
