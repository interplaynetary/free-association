# **Decision-Making Protocol: Node Protocol Delegation**

**Status:** Draft v1.0 (November 2025)  
**Part of:** Free Association Coalition Governance Framework

> **Implementation Note:** This protocol is implemented in the codebase as the **Councils** module (`src/lib/modules/councils/councils.ts`). The Councils implementation provides delegate management, mandate tracking, revocable proxies, and inter-council coordination. The coalition documentation formalizes the protocol that is already working in code.

## **Overview**

The Node Protocol Delegation is an alternative decision-making protocol for the Free Association Coalition Secretariat. Unlike traditional voting or consensus mechanisms, this protocol treats **mandates as executable code** with dynamically calculated authority based on real-time support from source organizations.

## **Core Concept**

Members are **delegates** holding **live mandates** \- proposals that remain active and executable as long as they maintain sufficient support from their source organization. This creates a system where:

* Decisions are **executable code**, not just statements
* Authority is **dynamically calculated** based on current support
* Accountability is **programmatically enforced** through automatic revocation
* Voting power **updates in real-time** as support changes

## **Protocol Components**

### **Participants can:**

* bootstrap nodes with initial functions during setup phase  
  * examples:  
    * `bootstrap.addMethod('allocateFunds', function(amount) { ... })`  
    * `bootstrap.addMember('Climate-Action-Network')`
* create proposals with cross-node method execution  
  * examples:  
    * `proposal.addAction(nodeA, 'increaseFunding', 'allocateFunds', [1000])`  
    * `proposal.addAction(nodeB, 'acceptFunding', 'receiveFunds', [1000])`

### **Nodes:**

* Its **members** are **delegates** holding **mandates**.  
* Each mandate is a **live proposal** containing:  
  * `methodName`: The executable function to call  
  * `methodArgs`: Parameters for execution  
  * `supporters`: Real-time list of supporting members  
  * `target-node`: The node where methods execute

### **Nodes must:**

* maintain a registry of recognized delegates and their current voting power  
  * `<delegate> | <voting-power> | <source-node>`  
  * `Alice Chen | 3 | Climate-Action-Network`  
  * `Dr. Benoit | 2 | Research-Coalition-X`
* process proposals  
  * A proposal passes when it gathers support from delegates representing a **quorum of \> 50% of the total voting power** in the node.

### **Delegates can:**

* express support for a proposal on behalf of their mandate  
  * The weight of their support equals their current `voting-power`.  
* be revoked  
  * If the support for their mandate in their source-node is withdrawn, their `voting-power` and authority in the secretariat are automatically and immediately updated.

### **Delegates hold:**

* **Executable authority** \- not just voting power, but the ability to trigger specific method calls  
* **Dynamically calculated influence** based on current mandate support  
* **Programmatically revocable access** that can be automatically triggered

## **Mandate Structure**

### **A Mandate is a Live Proposal.**

A mandate is a proposal, passed by a source-node, that specifies:  

* `delegate`: The entity representing the source-node.  
* `target-node`: The node the delegate is being sent to (e.g., the Secretariat).  
* `supporters`: The list of members in the source-node who support this delegation.  
* `scope` (Optional): Any instructions or constraints on the delegate's authority.

### **Example Mandate Record:**

```json
{
  "record_type": "proposal",
  "proposal_type": "delegation_mandate",
  "delegate": "uuid-alice-chen",
  "source_node": "Climate-Action-Network",
  "target_node": "FAC-Secretariat",
  "supporters": [
    "uuid-member-1",
    "uuid-member-2",
    "uuid-member-3"
  ],
  "voting_power": 3,
  "scope": {
    "authorized_methods": ["allocateFunds", "proposeMember"],
    "funding_limit": 50000,
    "valid_until": "2026-12-31"
  },
  "timestamp": "2025-11-17T10:00:00Z"
}
```

## **Accountability & Enforcement**

### **Observer Mechanism:**

* A source-node may send an **observer** with a delegate to monitor adherence to the mandate.  
* Observers can submit `statement` records documenting delegate actions

### **Violation Signaling:**

* A source-node may **signal a violation** of a mandate.  
  * If a violation is confirmed by the source-node, it triggers an automatic revocation of the delegate.

### **Violation Process:**

1. Source-node member submits `position` record with position="violation_claim"
2. Source-node processes claim through their internal decision protocol
3. If confirmed, source-node submits `mandate_revocation` record
4. Target-node automatically updates delegate's voting power to 0
5. All pending proposals supported by revoked delegate are recalculated

## **Inter-Node Relations**

### **Binding Decisions:**

* Decisions of the target-node are binding on its member-nodes.  
* To not accept and proceed to implementing a decision is to revoke one's membership in the target-node.  
* The target-node may vote to revoke a member-node's membership.

### **Hierarchical Structure:**

```
Target Node (Secretariat)
├─ Member Node A
│  └─ Delegate: Alice (voting power: 3)
├─ Member Node B
│  └─ Delegate: Benoit (voting power: 2)
└─ Member Node C
   └─ Delegate: Chen (voting power: 5)
```

### **Enforcement Mechanism:**

* **Voluntary compliance**: Member-nodes implement target-node decisions
* **Automatic consequences**: Failure to implement = automatic membership revocation
* **Explicit revocation**: Target-node can vote to remove non-compliant members

## **Technical Implementation**

### **The Nodes Protocol technically enforces:**

* **One delegate per source-target node pair** via code constraints  
* **Method execution only during approved proposal processing** via proxy security  
* **Real-time voting power updates** as mandate support changes  
* **Revocable access** to delegates through JavaScript Proxy revocation  
* **Streaming proposal status** via async generator pattern

### **Security Model:**

```javascript
// Delegate receives revocable proxy to target-node methods
const delegateProxy = new Proxy(targetNode, {
  get(target, method) {
    // Check delegate still has valid mandate
    if (!hasValidMandate(delegate, method)) {
      throw new Error('Mandate revoked or method unauthorized');
    }
    // Check method is in authorized scope
    if (!isAuthorizedMethod(delegate, method)) {
      throw new Error('Method not in delegation scope');
    }
    return target[method];
  }
});

// Mandate support changes trigger automatic updates
mandateStream.on('support_change', (mandate) => {
  updateVotingPower(mandate.delegate, calculatePower(mandate.supporters));
  if (mandate.voting_power === 0) {
    revokeProxy(mandate.delegate);
  }
});
```

### **Real-Time Updates:**

```javascript
// Delegates can subscribe to mandate status
async function* watchMandateStatus(mandateId) {
  while (true) {
    const status = await calculateMandateStatus(mandateId);
    yield {
      voting_power: status.current_power,
      supporters: status.current_supporters,
      active_proposals: status.proposals_supported,
      last_updated: Date.now()
    };
    await sleep(1000); // Update every second
  }
}
```

## **Comparison: Node Protocol vs Iterative Consensus**

| Aspect | Node Protocol Delegation | Iterative Consensus Protocol |
|--------|-------------------------|------------------------------|
| **Authority Type** | Executable code permissions | Advisory voting |
| **Voting Power** | Dynamically calculated from mandate | Fixed per member |
| **Revocation** | Automatic and immediate | Requires separate process |
| **Decision Speed** | Real-time as support changes | Multi-phase deliberation |
| **Accountability** | Programmatically enforced | Socially enforced |
| **Complexity** | High (technical infrastructure) | Medium (process management) |
| **Transparency** | Code-level auditability | Record-level auditability |
| **Best For** | Technical orgs, automated systems | Policy deliberation, consensus-building |

## **Use Cases**

### **Ideal For:**

* **Technical organizations** comfortable with code-based governance
* **Automated resource allocation** requiring real-time updates
* **Multi-level governance** with nested organizational structures
* **High-frequency decisions** where deliberation overhead is costly
* **Programmatic enforcement** where accountability must be automatic

### **Less Suitable For:**

* **Policy deliberation** requiring extensive discussion and refinement
* **Organizations without technical infrastructure** for proxy management
* **Situations requiring nuanced negotiation** over binary execution
* **Groups preferring human judgment** over algorithmic enforcement

## **Migration Path**

Organizations can adopt both protocols simultaneously:

1. **Use Iterative Consensus for**: Strategic decisions, framework amendments, policy setting
2. **Use Node Protocol for**: Resource allocations, operational decisions, automated processes

### **Hybrid Example:**

```javascript
// Strategic decision using Iterative Consensus
secretariat.proposeFrameworkAmendment({
  protocol: 'iterative_consensus',
  phases: ['submission', 'challenge', 'deliberation', 'support']
});

// Operational decision using Node Protocol
secretariat.proposeFundingAllocation({
  protocol: 'node_delegation',
  method: 'allocateFunds',
  args: [recipientNode, amount],
  requires: ['quorum_50_percent']
});
```

## **Record Types Used**

This protocol utilizes:

* **`proposal`** (proposal_type: "delegation_mandate")
* **`position`** (position: "support" | "violation_claim")
* **`mandate_revocation`** (custom record type)
* **`method_execution`** (records method calls and results)
* **`voting_power_update`** (tracks power changes over time)

## **Bootstrap Process**

1. **Node Setup**: Initialize target-node with authorized methods
2. **Member Registration**: Source-nodes register as member-nodes
3. **Mandate Proposals**: Source-nodes propose delegates with mandates
4. **Mandate Approval**: Target-node accepts or rejects mandates
5. **Proxy Creation**: Approved delegates receive revocable method proxies
6. **Operational Phase**: Delegates can execute authorized methods with quorum

## **Philosophical Foundation**

Traditional governance separates:
* **Decision** (what should happen)
* **Implementation** (making it happen)

Node Protocol Delegation unifies them:
* **Mandate** = Executable decision with built-in accountability
* **Support** = Real-time authorization, not historical vote
* **Revocation** = Automatic enforcement, not political pressure

This creates governance that is:
* **Responsive**: Authority updates immediately as support changes
* **Accountable**: Violations trigger automatic consequences
* **Transparent**: All authority is code-auditable
* **Efficient**: No gap between decision and execution

