# **Free Association Coalition**

**Draft Participation Framework for Review** \[Dec 1, 15:41\]  
**Drafted by:** Initial working group convened at COP30 and Global coordination sessions

This coalition consists of entities experimenting with protocols for voluntary coordination. The coalition proposes a re-engineering of how collective action and resource allocation can be coordinated. 

**A key insight is separating:**
1. **Publishing** (what is, what I have/need)  
2. **Derivation** (what we can infer collectively)  
   a. **Recognition** (who/what contributes)  
   b. **Allocation** (how we divide our capacities)

**Implications & Significance:**

* **Sovereignty and Interoperability:** Participants retain full control over their own data, recognitions, and capacity allocations. The system enables collaboration without requiring surrender of autonomy.  
* **Automation of Cooperation:** The vision is to have a significant portion of capacity/resource allocation (funding, technical support) be automatically derived based on the state of network data, drastically reducing transaction costs and delays.

**Participants may publish/derive data from local/network-data::** For example capacities, needs, recognitions, proportions, collective membership, environmental data, qualities, goals, estimates, sources for deriving, filters and their applications, or any other data.

## **Key derivations include:**

**Recognition (R):** Acknowledgement of contributions to the realization of one’s priorities/values.

**Relative-Recognition (RR):** Recognition normalized over **Total Recognition** to obtain proportions of 100%. Each participant has a fixed "budget" of total-recognition to divide and attribute. This normalization forces prioritization and trade-offs. **Recognition is non-transferable and dynamically adjustable.** Each participant can allocate recognition to entities who contribute to achieving their goals/priorities.

**Mutual-Recognition (MR):** Calculated as the lower of relative-recognition percentages that two entities assign to each other: `MR(X,Y) = min(X_rec_of_Y, Y_rec_of_X)`. This creates *perfect reciprocity in proportion*. A one-sided relationship where A recognizes B highly (ex: 50%), but B recognizes A little (ex: 1%) is valued at the lower amount (ex: 1%), discouraging free-riding and encouraging mutual engagement and support. 

**Relative Mutual-Recognition (RMR):** Mutual-Recognition normalized over **Total Mutual-Recognition** to obtain proportions of 100%. When we **recognize** each other, we have **mutual-recognition of mutual-value** and **can choose to allocate our capacities to each-other in precise proportion to how relatively mutually-fulfilling we are to each other.**

**Collective-Mutual-Recognition (CMR):** For a recognized member set, each member's mutual-recognitions with other members summed and normalized over the **Total Pool** of all mutual-recognitions between all members: `Share(Member) = Σ MR(Member, Others) / Σ MR(all pairs)`. Members with stronger network integration have proportionally more influence. Used when contribution should be weighted by relationship strength.

**Collective-Relative-Mutual-Recognition (CRMR):** For a recognized member set, each member's relative-mutual-recognitions (normalized to 100%) treated as equal votes, then aggregated: `Share(Member) = Σ RMR_votes_for_Member / Total_Members`. Each member has equal voting power regardless of network position. Used when equal voice is desired (governance, democratic contexts).

**Mutual-Recognition-Density (MRD):** Measures network integration depth 
by normalizing participant's total mutual-recognition against network 
average: `MRD(i) = Σ MR(i, members) / Average_MRS`. Can be used for 
membership determination (when `MRD ≥ threshold`, typically 0.5) enabling 
membership to emerge from relationship depth. Average can be calculated 
from current members (collective model: coherent, rising bar) or all 
participants (commons model: open, stable bar) — naturally resistant to 
Sybil attacks and collusion while providing transparent onboarding paths.

Note, distribution choice reflects: *whose contribution-recognitions should be taken into account when formulating proportions to prioritize allocations?* — individual control suggests relative-recognition/relative-mutual-recognition, cooperatively-realized capacities suggest collective-mutual-recognition (weighted by contribution) or collective-relative-mutual-recognition (equal voice).

**Participants can publish/propose/offer/allocate with the help of protocols of their choosing.** 

For example: **Multi-provider-need-satisfaction** where providers allocate capacity proportionally (`Provider_Capacity × Share`) capped at declared needs (`min(Raw_Allocation, Declared_Need)`), with remaining needs updating across rounds (`max(0, Declared_Need - Total_Received)`) until equilibrium.

**The system naturally promotes accurate recognition through mathematical necessity:**  
Entities define their goals/priorities subjectively, but achieving them depends on objective access to capacities and partnerships.

| ∝ | is proportional to |
| :---- | :---- |
| **↑** | increase in |
| **↓** | decreases in |
| **∴** | therefore |

**FOR ANY PARTICIPANT:**  
**GIVEN:**  
  **• Total Recognition \= 100%**  
  **• Capacities distributed ∝ Relative-(Mutual-)Recognition**  
  **• Goals require access to specific capacities/partnerships**  
**THEN:**  
  **↑ Recognition allocated to non-beneficial partners**  
    **∴ ∝ ↓ Recognition available for beneficial partners   \[total-relative-recognition budget constraint\]**  
    **∴ ↓ Mutual-Recognition with beneficial partners**  
   	 **∴ ↓ Access to needed capacities \[proportional allocation\]**  
    	 	**∴ ↓ Goal Achievement**  
    	 		**∴  RESULT: Natural incentive to correct recognition allocation**

**Key Implication:** The system creates natural incentives for accurate recognition. Inflating or misattributing recognition only decreases connection to beneficial partners and capacities. Entities that maintain accurate recognition patterns receive better-aligned capacities and achieve better outcomes.

---

## **Agnostic to Institutionalized Intermediation**

Traditional coordination operates via enforcement infrastructure (property, governance, currency, jurisdiction, markets) to force coordination flow through standardized interfaces.

This framework bypasses institutionalized intermediaries by asking: **"Whose perspective should be taken into account in formulating the allocation of this capacity?"**

**Intermediaries become optional:**
- **Property** (who owns) → allocate via own/synthetic recognition
- **Governance** (who decides) → each autonomously decides whose recognition to include
- **Currency** (medium of exchange) → value flows directly as capacity based on recognition
- **Jurisdiction** (which rules) → emerges from mutual recognition of protocols
- **Markets** (price signals) → value directly expressed through recognition allocations

**Why this works:** Recognition is more fundamental than reified coordination layers. Direct protocol using mutual recognition as primitive enables coordination without building enforcement infrastructure first.

**Key properties:**
- **Interoperability**: Worker cooperative ↔ sole proprietor ↔ state enterprise coordinate via same protocol
- **Minimal infrastructure**: Recognition declaration + mathematical algorithms + communication protocol; everything else optional
- **Power as exit/voice**: Cannot force others to include your perspective; can only choose whose recognition you include
- **Scale invariance**: Individual → collective → organization → global; same mechanism, only computation scales

**What remains:**

*Required agreement:* Mathematics for distributions, protocol for communication

*Local choice:* How to determine recognition, generate capacity, organize internally, whether to use traditional intermediaries as convenience layers

*Contested domain:* Whose contribution recognized and how much — social/epistemic (learning) not structural/legal (enforcement)

**The framework enables direct relational coordination at any scale without requiring agreement on intermediate structures.**
