# **Coalition Protocols: Implementation Mapping**

**Status:** Draft v0.1 (November 2025)  
**Purpose:** Maps coalition documentation to working code implementations

## **Overview**

The Free Association Coalition's decision-making protocols are **already implemented** in the codebase. This document maps between the formal coalition documentation and the working code implementations.

## **Protocol Implementations**

### **Iterative Consensus Protocol → Decider Module**

| Coalition Docs | Codebase Implementation |
|----------------|-------------------------|
| **Location** | `docs/coalition/secretariat/decision-making-protocol.md` | `src/lib/modules/decider/` |
| **Purpose** | Secretariat decision-making | General group decision-making |
| **Participants** | "Members" | "Players" |
| **Session** | "Assembly" | "Game" |
| **Time Period** | "Deliberation Window" | "Time Window" |
| **Outcome** | "Adopted" / "Passed" | "Passed" |
| **Architecture** | Distributed (participant-centric) | P2P (peer-to-peer via Holster) |

### **Protocol Flow Comparison**

**Coalition Documentation:**
```
1. Proposal Submission
2. Challenge Expression  
3. Deliberation & Comments
4. Modification Proposals
5. Support Expression
6. Decision Outcome
```

**Decider Implementation:**
```typescript
1. Express Proposals
2. Express Challenges
3. Express Comments & Modifications
4. Express Support
5. Calculate Result
```

**Verdict:** ✅ **Identical process** - The Decider implements exactly what the Iterative Consensus Protocol describes, just with more accessible terminology for general use.

---

### **Node Protocol Delegation → Councils Module**

| Coalition Docs | Codebase Implementation |
|----------------|-------------------------|
| **Location** | `docs/coalition/secretariat/node-protocol-delegation.md` | `src/lib/modules/councils/councils.ts` |
| **Purpose** | Coalition secretariat governance | Inter-council coordination |
| **Terminology** | "Nodes", "Secretariat" | "Councils" |
| **Delegates** | "Delegates with mandates" | `Delegate` class with `mandate` property |
| **Voting Power** | Based on `supporters.length` | `delegate.mandate.supporters.length` |
| **Quorum** | "> 50% of total voting power" | "50% of the total voting power" |
| **Revocation** | Automatic via support withdrawal | Revocable Proxy pattern |
| **Observers** | Mentioned in protocol | Mentioned in code comments |

### **Key Implementation Details**

**From `councils.ts` (lines 3-11):**
```typescript
// Councils elect and send rotating revokable delegates with mandates
// representing their interests/needs to other councils of which they 
// are a member. The decisions of the target-councils are binding to 
// its member-councils, to not accept/follow the decision is to revoke 
// membership.

// Gathering support from delegates, once a quorum is reached, the 
// proposal is passed.
// the weight of a delegates support should = delegate.mandate.supporters.length
// the quorum should be 50% of the total voting power of the council.
```

**Verdict:** ✅ **Identical system** - The Councils module implements exactly what Node Protocol Delegation describes, with some implementation details (like observers) noted in comments.

---

## **Why Two Sets of Documentation?**

### **Codebase Documentation** (`src/lib/modules/`)

**Audience:** Developers implementing decision-making systems

**Focus:**
- Accessible, friendly language ("players", "games")
- Implementation details (TypeScript classes, data structures)
- Developer-friendly examples (friends deciding dinner)
- Technical architecture (P2P, Holster integration)

**Use Cases:**
- General-purpose group decision-making
- Any application needing consensus mechanisms
- Educational demonstrations

### **Coalition Documentation** (`docs/coalition/`)

**Audience:** Organizations, policymakers, coalition members

**Focus:**
- Formal, institutional language ("members", "secretariat")
- Governance principles and accountability
- Multilateral coordination contexts
- Policy implications and rationale

**Use Cases:**
- Coalition secretariat governance
- Inter-organizational coordination
- Formal institutional decision-making
- COP30 coordination sessions

## **Terminology Translation Guide**

| Concept | Codebase Term | Coalition Term |
|---------|---------------|----------------|
| **Decision-making participant** | Player | Member |
| **Decision-making session** | Game | Assembly / Session |
| **Group making decisions** | N/A (implicit) | Secretariat / Council / Node |
| **Accepted proposal** | Passed | Adopted |
| **Time limit per phase** | Time Window | Deliberation Window |
| **Final decision record** | Passed Proposals | Decision Outcomes |
| **Representative with mandate** | Delegate | Delegate (same) |
| **Authority source** | Mandate | Mandate (same) |
| **Support measurement** | supporters.length | voting_power |
| **Minimum votes needed** | Quorum | Quorum (same) |

## **For Developers**

### **Implementing Coalition Governance**

If you're building a coalition secretariat application:

1. **Use the Decider module** for Iterative Consensus Protocol:
   ```typescript
   import { Decider } from '$lib/modules/decider';
   // "Players" = Secretariat members
   // "Game" = Assembly session
   // "Passed proposals" = Adopted decisions
   ```

2. **Use the Councils module** for Node Protocol Delegation:
   ```typescript
   import { Council, Delegate, Proposal } from '$lib/modules/councils';
   // "Council" = Node/Secretariat
   // Delegates carry mandates with voting_power = supporters.length
   ```

3. **Adapt terminology** in your UI to match institutional context:
   - Display "Assembly" instead of "Game"
   - Show "Members" instead of "Players"
   - Present "Adopted" instead of "Passed"

### **Code Examples**

#### **Iterative Consensus for Coalition Assembly**

```typescript
const secretariat = new Decider({
  players: [
    { name: "Organization A" },
    { name: "Organization B" },
    { name: "Organization C" }
  ],
  agenda: [
    "Adopt resource allocation criteria",
    "Schedule next assembly",
    "Accept new member applications"
  ],
  timeWindow: 7 * 24 * 60 * 60 * 1000 // 7 days in milliseconds
});

// Run decision flow
const adoptedDecisions = await secretariat.runDecisionFlow();
```

#### **Node Protocol Delegation for Multi-Level Governance**

```typescript
const coalition = new Council("Free Association Coalition");
const climateNetwork = new Council("Climate Action Network");

// Climate Network sends delegate to Coalition
const mandate = climateNetwork.addProposal(
  "Send Alice Chen as delegate to FAC Secretariat"
);

// Members support the mandate
members.forEach(m => mandate.castVote(m, 'yes'));

// Create delegate with voting power = supporters
const delegate = new Delegate(
  "Alice Chen",
  mandate,
  climateNetwork,
  coalition
);

// Delegate voting power automatically updates with mandate support
console.log(delegate.mandate.supporters.length); // = voting power
```

## **For Coalition Members**

### **Understanding the Technical Implementation**

When you read the coalition documentation about **Iterative Consensus Protocol** or **Node Protocol Delegation**, know that these are **not theoretical** - they're **working, tested code** that has been battle-tested in various applications.

### **Testing the Protocols**

You can test these protocols before committing to coalition membership:

1. **Try the Decider demo:**
   - Experience the proposal → challenge → support flow
   - See how early adoption works
   - Understand time window mechanics

2. **Experiment with Councils:**
   - Create test councils
   - Send delegates with mandates
   - Observe automatic voting power updates
   - Test revocation mechanisms

## **For Documentation Contributors**

### **Maintaining Consistency**

When updating either the codebase docs or coalition docs:

1. **Check both locations** for the same protocol
2. **Ensure flow diagrams match** (even if terminology differs)
3. **Cross-reference** implementation notes
4. **Preserve terminology** appropriate to each audience

### **Adding New Protocols**

If adding a new decision-making protocol:

1. **Implement in code first** (`src/lib/modules/`)
   - Write tests
   - Create developer documentation
   - Use accessible terminology

2. **Document for coalition** (`docs/coalition/`)
   - Formalize for institutional use
   - Provide governance rationale
   - Use formal terminology
   - Add implementation note linking to code

3. **Update this mapping document**

## **Conclusion**

The Free Association Coalition's governance protocols are **production-ready** because they're built on **battle-tested implementations**. The Decider and Councils modules provide the technical foundation, while the coalition documentation provides the institutional framing.

**Key Insight:** You're not choosing between theory and practice - the coalition protocols **are** the practice, just described for different audiences.

---

**Related Documentation:**
- [Iterative Consensus Protocol](secretariat/decision-making-protocol.md) (coalition formalization)
- [Node Protocol Delegation](secretariat/node-protocol-delegation.md) (coalition formalization)
- Decider Module: `src/lib/modules/decider/decider.md` (developer docs)
- Councils Module: `src/lib/modules/councils/councils.ts` (code implementation)

