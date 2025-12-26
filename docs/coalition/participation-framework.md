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

**Priority (P):** Acknowledgement of contributions to the realization of one’s priorities/values.

**Relative-Priority (RP):** Priority normalized over **Total Priority** to obtain proportions of 100%. Each participant has a fixed "budget" of total-priority to divide and attribute. This normalization forces trade-offs. **Priority is non-transferable and dynamically adjustable.**

**Reciprocal Alignment (RA):** Calculated as the lower of relative-priority percentages that two entities assign to each other: `RA(X,Y) = min(X_priority_to_Y, Y_priority_to_X)`. This creates *perfect reciprocity in proportion*. A one-sided relationship where A prioritizes B highly (ex: 50%), but B prioritizes A little (ex: 1%) is valued at the lower amount (ex: 1%), encouraging mutual engagement.

**Relative Reciprocal-Alignment (RRA):** Reciprocal-Alignment normalized over **Total Reciprocal-Alignment** to obtain proportions of 100%. When we **align** with each other, we can **choose to allocate our capacities to each-other in precise proportion to our alignment.**

**Collective-Reciprocal-Alignment (CRA):** For a member set, each member's reciprocal-alignments with other members summed and normalized. `Share(Member) = Σ RA(Member, Others) / Σ RA(all pairs)`. Members with stronger network integration have proportionally more influence.

**Reciprocal-Alignment-Density (RAD):** Measures network integration depth by normalizing participant's total reciprocal-alignment against network average. `RAD(i) = Σ RA(i, members) / Average_RA`. Used for membership determination (when `RAD ≥ threshold`) enabling membership to emerge from relationship depth.

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
  **• Total Priority = 100%**  
  **• Capacities distributed ∝ Reciprocal-Alignment**  
  **• Goals require access to specific capacities/partnerships**  
**THEN:**  
  **↑ Priority allocated to non-beneficial partners**  
    **∴ ∝ ↓ Priority available for beneficial partners   [total-priority budget constraint]**  
    **∴ ↓ Reciprocal-Alignment with beneficial partners**  
   	 **∴ ↓ Access to needed capacities [proportional allocation]**  
    	 	**∴ ↓ Goal Achievement**  
    	 		**∴  RESULT: Natural incentive to correct priority allocation**

**Key Implication:** The system creates natural incentives for accurate recognition. Inflating or misattributing recognition only decreases connection to beneficial partners and capacities. Entities that maintain accurate recognition patterns receive better-aligned capacities and achieve better outcomes.

---

## **Agnostic to Institutionalized Intermediation**

Traditional coordination operates via enforcement infrastructure (property, governance, currency, jurisdiction, markets) to force coordination flow through standardized interfaces.

This framework bypasses institutionalized intermediaries by asking: **"Whose perspective should be taken into account in formulating the allocation of this capacity?"**

**Intermediaries become optional:**
- **Property** (who owns) → allocate via own/synthetic recognition
- **Governance** (who decides) → each autonomously decides whose recognition to include
- **Currency** (medium of exchange) → value flows directly as capacity based on recognition
- **Jurisdiction** (which rules) → emerges from reciprocal alignment of protocols
- **Markets** (price signals) → value directly expressed through recognition allocations

**Why this works:** Recognition is more fundamental than reified coordination layers. Direct protocol using reciprocal alignment as primitive enables coordination without building enforcement infrastructure first.

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
