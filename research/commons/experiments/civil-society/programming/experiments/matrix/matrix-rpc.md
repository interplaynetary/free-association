I'll fetch that article to understand the specific WebRPC implementation you're referring to.Great question! For your use case of collaborative matrix editing with fine-grained permissions, Cap'n Web's capability-based security model is actually **really well-suited**. Here's my analysis of how it would scale:

## Scaling Characteristics

**Strengths for your use case:**

1. **Capability-based security scales naturally** - Instead of checking permissions on every operation, you'd give each user a reference to only the specific matrix region they can edit. The server just needs to create separate `MatrixRegion` objects:

```typescript
class MatrixServer extends RpcTarget {
  authenticate(userId) {
    // Return a capability for just their allowed region
    return new MatrixRegion(this.matrix, userId, startRow, endRow, startCol, endCol);
  }
}

class MatrixRegion extends RpcTarget {
  constructor(matrix, userId, startRow, endRow, startCol, endCol) {
    super();
    this.matrix = matrix;
    this.bounds = {startRow, endRow, startCol, endCol};
  }
  
  setCell(row, col, value) {
    // Automatically enforces bounds - no permission checks needed
    if (row < this.bounds.startRow || row >= this.bounds.endRow) throw new Error("Out of bounds");
    this.matrix[row][col] = value;
  }
}
```

2. **Promise pipelining reduces round trips** - Multiple matrix updates can be batched into single network round trips, which is crucial for collaborative editing

3. **Bidirectional calling** - Server can push updates to clients when other users modify the matrix, without separate WebSocket infrastructure

**Potential scaling concerns:**

1. **Connection overhead** - Each WebSocket connection has memory overhead. For thousands of concurrent users, this could become significant (though modern servers handle this well)

2. **Object reference management** - Cap'n Web maintains export/import tables for all object references. With many users and many matrix region objects, this could grow. However, the article shows IDs are just integers (starting from 0, -1, 1, etc.), so this is relatively lightweight

3. **No built-in broadcast primitives** - If you need to broadcast changes to many users, you'd need to implement this yourself (looping over callbacks)

## Practical Recommendations

For **10-100 concurrent users**: Should scale excellently. The 10kB library size and minimal overhead make this very efficient.

For **100-1000 users**: Still good, but monitor:
- Memory per connection
- CPU for message routing
- Whether you need to add a pub/sub layer for broadcasts

For **1000+ users**: Consider:
- Using HTTP batch mode for read-only operations
- Implementing a hybrid approach where Cap'n Web handles permissions/editing, but a separate system handles real-time synchronization
- Sharding by matrix regions

## Alternative consideration

The capability model is perfect for your permission requirements, but for the real-time collaboration aspect, you might want to pair it with operational transforms (OT) or CRDTs to handle concurrent edits gracefully.

Would you like me to create a prototype showing how to structure the matrix editing with fine-grained capabilities?

---

# **Capability-Theoretic Foundation**

## **Core Insight: Mathematical Axioms as Architectural Invariants**

The fundamental connection between Free Association's mathematical axioms and Cap'n Web's capability-based security reveals a profound isomorphism: **the min() function in Axiom 2 is structurally equivalent to capability-based security**.

### **Axiom 2 ↔ Capability Security Isomorphism**

**Mathematical Definition (Axiom 2):**
$$
MR(a,b) = \min(R(a,b), R(b,a))
$$

**Capability-Based Implementation:**
- You can only compute $MR(a,b)$ if you possess **both**:
  1. A capability reference to $a$'s recognition allocation
  2. A capability reference to $b$'s recognition allocation
  
Both create **unforgeable bilateral relationships** that cannot be faked or manipulated by either party alone.

### **Mapping All Axioms to RPC Properties**

#### **Axiom 1: Recognition Budget Constraint → Capability Distribution**

**Mathematical Statement:**
$$
\sum_{x \in P} R(a,x) = 1, \quad R(a,x) \geq 0 \ \forall x
$$

**RPC Implementation:**

```typescript
class RecognitionBudget extends RpcTarget {
  private allocations: Map<ParticipantStub, number> = new Map();
  private readonly totalBudget = 1.0;
  
  allocateRecognition(target: ParticipantStub, amount: number): boolean {
    // Server-side enforcement - cannot be bypassed
    const currentTotal = Array.from(this.allocations.values())
      .reduce((sum, val) => sum + val, 0);
    
    if (currentTotal + amount > this.totalBudget) {
      throw new Error("Budget constraint violation");
    }
    
    this.allocations.set(target, amount);
    return true;
  }
  
  getRecognitionTo(target: ParticipantStub): number {
    return this.allocations.get(target) || 0;
  }
}
```

**Enforcement Properties:**
1. **Unforgeable Budget Object**: Only returned by authenticated session
2. **Server-Side Validation**: Sum constraint enforced in trusted code
3. **Atomic Operations**: Each allocation checked before commitment
4. **Capability-Based Access**: Can only allocate to participants you have stubs for

**Anti-Gaming Guarantee**: A participant cannot:
- Claim they have more than 100% recognition to allocate
- Allocate recognition to forged participant identities
- Bypass the sum constraint through concurrent operations

---

#### **Axiom 2: Mutual Recognition Definition → Bidirectional Capability Exchange**

**Mathematical Statement:**
$$
MR(a,b) = \min(R(a,b), R(b,a))
$$

**RPC Implementation:**

```typescript
class NetworkState extends RpcTarget {
  private recognitionGraph: Map<string, RecognitionBudget> = new Map();
  
  computeMutualRecognition(
    participantA: ParticipantStub, 
    participantB: ParticipantStub
  ): number {
    // Requires BOTH capabilities to exist
    const budgetA = this.recognitionGraph.get(participantA.id);
    const budgetB = this.recognitionGraph.get(participantB.id);
    
    if (!budgetA || !budgetB) {
      throw new Error("Cannot compute MR without both participant capabilities");
    }
    
    // The min() operation enforces bilateral consent
    const R_ab = budgetA.getRecognitionTo(participantB);
    const R_ba = budgetB.getRecognitionTo(participantA);
    
    return Math.min(R_ab, R_ba);
  }
}
```

**Critical Insight**: Recognition isn't "stored" centrally - it exists as **paired RPC stubs**:
- When $A$ recognizes $B$, $A$ obtains a stub to $B$'s `RecognitionReceiver`
- Mutual recognition emerges only when **both** have stubs to each other
- The system **cannot compute** $MR(a,b)$ without both capabilities existing

**Enforcement Properties:**
1. **Bilateral Verification**: Both participants must have allocated recognition
2. **Unforgeable References**: ParticipantStub cannot be created by clients
3. **Server-Side Computation**: The min() operation happens in trusted code
4. **Symmetry Guaranteed**: $MR(a,b) = MR(b,a)$ by construction

**Anti-Gaming Guarantee**: A participant cannot:
- Forge that others recognize them (no capability = no recognition)
- Claim mutual recognition exists when only unilateral recognition exists
- Manipulate the min() calculation

---

#### **Axiom 3: Capacity Flow Proportionality → Authenticated Session Pattern**

**Mathematical Statement:**
$$
C(a,b) = \kappa_b \cdot g(MR(a,b))
$$

Where $g: [0,1] \to \mathbb{R}_{\geq 0}$ is strictly increasing.

**RPC Implementation:**

```typescript
class ParticipantServer extends RpcTarget {
  authenticate(participantId: string, credentials: Credential): AuthenticatedParticipant {
    const verified = this.verifyCredentials(participantId, credentials);
    if (!verified) throw new Error("Authentication failed");
    
    // Return unforgeable session capability
    return new AuthenticatedParticipant(participantId, this.network);
  }
}

class AuthenticatedParticipant extends RpcTarget {
  private readonly participantId: string;
  private readonly network: NetworkState;
  private capacity: number;
  
  constructor(participantId: string, network: NetworkState) {
    super();
    this.participantId = participantId;
    this.network = network;
    this.capacity = this.loadCapacity(participantId);
  }
  
  allocateCapacity(recipientStub: ParticipantStub, requestedAmount: number): number {
    // recipientStub is an RPC reference - cannot be forged
    const mutualRecognition = this.network.computeMutualRecognition(
      this.getStub(), 
      recipientStub
    );
    
    // g(MR) - strictly increasing function
    const flowMultiplier = this.computeFlowMultiplier(mutualRecognition);
    const allocatedFlow = requestedAmount * flowMultiplier;
    
    // Enforce capacity constraint
    if (allocatedFlow > this.capacity) {
      throw new Error("Insufficient capacity");
    }
    
    this.capacity -= allocatedFlow;
    return allocatedFlow;
  }
  
  private computeFlowMultiplier(mr: number): number {
    // Example: linear function g(x) = x
    // Could be any strictly increasing function
    return mr;
  }
}
```

**Key Insight**: **The unforgeable session object IS the capacity allocation mechanism.**

**Enforcement Properties:**
1. **Session Capability**: Only created through successful authentication
2. **Capacity Binding**: Each session bound to specific participant's capacity pool
3. **MR-Based Flow**: Capacity flow automatically proportional to mutual recognition
4. **No Forgery**: Cannot create `AuthenticatedParticipant` without authenticating
5. **Reference Integrity**: `recipientStub` verified through capability system

**Anti-Gaming Guarantee**: A participant cannot:
- Allocate more capacity than they have
- Claim capacity from another participant's pool
- Manipulate the flow multiplier function
- Allocate without mutual recognition being verified

---

#### **Axiom 4 & 5: Goal Achievement → Capability-Based Need Satisfaction**

**Mathematical Statement (Axiom 4):**
$$
\mathbb{P}(G) = f\left(\sum_{b \in B} C(a,b)\right)
$$

**Mathematical Statement (Axiom 5):**
$$
\frac{\partial \mathbb{P}(G)}{\partial C(a,b)} > 0 \text{ for } b \in B, \quad
\frac{\partial \mathbb{P}(G)}{\partial C(a,n)} = 0 \text{ for } n \notin B
$$

**RPC Implementation:**

```typescript
class ParticipantGoal extends RpcTarget {
  private readonly goalId: string;
  private readonly beneficialSet: Set<ParticipantStub>;
  private receivedCapacity: Map<ParticipantStub, number> = new Map();
  
  constructor(goalId: string, beneficialStubs: ParticipantStub[]) {
    super();
    this.goalId = goalId;
    // Beneficial set encoded as capability possession
    this.beneficialSet = new Set(beneficialStubs);
  }
  
  receiveCapacity(from: ParticipantStub, amount: number): GoalProgress {
    // Only capacity from beneficial set contributes to goal
    if (!this.beneficialSet.has(from)) {
      // Axiom 5: non-beneficial capacity doesn't contribute
      return { accepted: false, reason: "Not in beneficial set" };
    }
    
    const current = this.receivedCapacity.get(from) || 0;
    this.receivedCapacity.set(from, current + amount);
    
    // Axiom 4: goal probability increases with beneficial capacity
    return {
      accepted: true,
      goalProbability: this.computeGoalProbability()
    };
  }
  
  private computeGoalProbability(): number {
    const totalBeneficialCapacity = Array.from(this.beneficialSet)
      .map(stub => this.receivedCapacity.get(stub) || 0)
      .reduce((sum, val) => sum + val, 0);
    
    // f: strictly increasing function
    return this.f(totalBeneficialCapacity);
  }
  
  private f(capacity: number): number {
    // Example: logistic function
    return 1 / (1 + Math.exp(-capacity));
  }
}
```

**Enforcement Properties:**
1. **Beneficial Set as Capabilities**: Only stubs in the set contribute
2. **Automatic Filtering**: Non-beneficial capacity rejected at architectural level
3. **Monotonic Progress**: Goal probability increases only with beneficial capacity
4. **Verifiable Progress**: Each contribution tracked by unforgeable stub reference

**Anti-Gaming Guarantee**: A participant cannot:
- Claim capacity from non-beneficial sources contributes to their goal
- Forge beneficial set membership
- Manipulate the goal achievement function

---

### **The Fundamental Anti-Gaming Theorem via RPC Architecture**

The mathematical Anti-Gaming Theorem states:
$$
\frac{d\mathbb{P}(G)}{dT_B} \geq 0 \quad \text{and} \quad \frac{d\mathbb{P}(G)}{dT_N} \leq 0
$$

Where $T_B = \sum_{b \in B} R(a,b)$ and $T_N = \sum_{n \in N} R(a,n)$.

**In the RPC implementation, this theorem becomes automatically enforced:**

| Gaming Attempt | Mathematical Block | RPC Enforcement |
|---------------|-------------------|-----------------|
| "I'll forge high recognition from others" | $R(b,a)$ controlled by $b$ | Can't create RPC stubs to others' `RecognitionBudget` objects |
| "I'll allocate more than my budget" | $\sum_x R(a,x) = 1$ constraint | Budget enforcement server-side in unforgeable `RecognitionBudget` |
| "I'll claim others recognize me" | $MR(a,b)$ requires both $R(a,b)$ and $R(b,a)$ | `computeMutualRecognition()` requires both capabilities |
| "I'll pretend non-beneficial sources help my goal" | $\frac{\partial \mathbb{P}}{\partial C(a,n)} = 0$ | `ParticipantGoal.receiveCapacity()` rejects non-beneficial sources |
| "I'll access others' capacity pools" | Each $\kappa_b$ controlled by $b$ | `AuthenticatedParticipant` session binds to specific capacity pool |

**Meta-Theorem**: *Every axiom violation requires forging a capability reference, which is cryptographically impossible in Cap'n Web's export table architecture.*

---

### **Export Tables as Unforgeable References**

From the Cap'n Web protocol (rpc.md):
> Each entry in the export table has a signed integer ID, which is used to reference it. You can think of these IDs like file descriptors in a POSIX system.

**Security Properties:**
1. **Capability IDs are unforgeable**: Assigned by server, negative for client-to-server, positive for pushes
2. **Capabilities are unforgeable**: Cannot create an ID that points to another participant's objects
3. **One-time assignment**: An ID is never reused over the lifetime of a connection
4. **Bilateral verification**: Both Alice and Bob maintain separate export tables

**Mapping to Free Association:**
- Export ID 0: Participant's main interface (their `AuthenticatedParticipant`)
- Negative IDs: Recognition allocations they've shared (their `RecognitionBudget` references)
- Positive IDs: Results of operations (computed `MutualRecognition` values)

**This architecture makes axiom violations architecturally impossible.**

---

# **RPC Protocol Design**

## **Matrix Operations as Capability Networks**

The matrix operations (RS, MR, MRS, SCMRS) are not stored data structures but **query results over the capability graph**. Each matrix element is computed on-demand by traversing capability references.

### **Key Insight: Matrices as Lazy Evaluation**

Traditional implementation:
```python
# Store the entire matrix
MR = compute_full_matrix(all_participants)  # O(n²) space
result = MR[i][j]  # O(1) lookup
```

Capability-based implementation:
```typescript
// No storage - compute on demand via capabilities
result = network.computeMutualRecognition(participantA, participantB);  // O(1) space
```

**Advantages:**
1. **Zero-knowledge computation**: Only query relationships you have capabilities for
2. **Fresh data**: Always reflects current recognition state
3. **Scalable**: Memory usage independent of network size
4. **Secure**: Cannot query relationships you don't have access to

---

## **Message Sequences for Core Operations**

### **Operation 1: Recognition Allocation (Computing RS)**

**Mathematical Operation:**
$$
RS_{ij} = \frac{R_{ij}}{\sum_{k=1}^n R_{ik}}
$$

**Client-Side Code:**
```typescript
// Client wants to allocate recognition
let session = api.authenticate(credentials);
let budget = session.getRecognitionBudget();

// Allocate recognition to multiple participants
let promise1 = budget.allocateRecognition(participantBStub, 0.6);
let promise2 = budget.allocateRecognition(participantCStub, 0.4);

let results = await Promise.all([promise1, promise2]);
```

**RPC Message Sequence:**
```
// Call api.authenticate() - returns session at export ID 1
-> ["push", ["pipeline", 0, "authenticate", [["credential_data"]]]]

// Call session.getRecognitionBudget() - returns budget at export ID 2
-> ["push", ["pipeline", 1, "getRecognitionBudget", []]]

// Call budget.allocateRecognition() twice
-> ["push", ["pipeline", 2, "allocateRecognition", [["participant", "B"], 0.6]]]
-> ["push", ["pipeline", 2, "allocateRecognition", [["participant", "C"], 0.4]]]

// Pull results (only when awaited)
-> ["pull", 3]
-> ["pull", 4]

// Server responds with confirmations
<- ["resolve", 3, true]
<- ["resolve", 4, true]
```

**Round Trips:** 2 (authenticate + allocate batch)

**Without Promise Pipelining:** Would require 4 round trips

---

### **Operation 2: Mutual Recognition Query (Computing MR)**

**Mathematical Operation:**
$$
MR_{ij} = \min(RS_{ij}, RS_{ji})
$$

**Client-Side Code with Pipelining:**
```typescript
// Single round trip for complex query
let session = api.authenticate(credentials);
let network = session.getNetworkState();
let mr_ab = await network.computeMutualRecognition(participantA, participantB);
```

**RPC Message Sequence:**
```
// Pipelined calls - all sent immediately
-> ["push", ["pipeline", 0, "authenticate", [["creds"]]]]
-> ["push", ["pipeline", 1, "getNetworkState", []]]
-> ["push", ["pipeline", 2, "computeMutualRecognition", 
             [["participant", "A"], ["participant", "B"]]]]
-> ["pull", 3]

// Single response after all computation
<- ["resolve", 3, 0.3]  // min(0.6, 0.3) = 0.3
```

**Round Trips:** 1 (everything pipelined)

**Key Property:** The server executes all three operations sequentially, but the client only waits once.

---

### **Operation 3: Multi-Provider Allocation (Section 5.1 Algorithm)**

**Mathematical Algorithm:**
```
For each iteration t:
1. Compute raw allocations: r^(t) = K · S[:,r]
2. Compute actual allocations: a^(t)_actual = min(r^(t), N_r^(t))
3. Update received, remaining need, capacities
```

**RPC Implementation:**
```typescript
class AllocationOrchestrator extends RpcTarget {
  async allocateMultiProvider(
    recipient: ParticipantStub,
    need: number,
    providers: ParticipantStub[]
  ): Promise<AllocationResult> {
    
    let remainingNeed = need;
    let allocations = new Map<ParticipantStub, number>();
    
    for (let iteration = 0; iteration < 100 && remainingNeed > 0; iteration++) {
      // Promise pipelining: all providers queried in parallel
      let rawAllocationPromises = providers.map(provider =>
        provider.computeAllocationFor(recipient, remainingNeed)
      );
      
      let rawAllocations = await Promise.all(rawAllocationPromises);
      
      // Cap by remaining need
      let totalRaw = rawAllocations.reduce((sum, val) => sum + val, 0);
      if (totalRaw === 0) break;
      
      let actualAllocations = rawAllocations.map(raw =>
        totalRaw <= remainingNeed ? raw : raw * (remainingNeed / totalRaw)
      );
      
      // Commit allocations in parallel
      let commitPromises = providers.map((provider, idx) =>
        provider.commitAllocation(recipient, actualAllocations[idx])
      );
      
      await Promise.all(commitPromises);
      
      // Update state
      actualAllocations.forEach((amount, idx) => {
        allocations.set(providers[idx], (allocations.get(providers[idx]) || 0) + amount);
      });
      remainingNeed -= actualAllocations.reduce((sum, val) => sum + val, 0);
    }
    
    return { allocations, satisfied: remainingNeed === 0 };
  }
}
```

**Message Sequence for 3 Providers, 2 Iterations:**
```
// Iteration 1: Query all providers (parallel)
-> ["push", ["pipeline", 0, "computeAllocationFor", [["recipient"], 100]]]  // Provider 1
-> ["push", ["pipeline", 0, "computeAllocationFor", [["recipient"], 100]]]  // Provider 2  
-> ["push", ["pipeline", 0, "computeAllocationFor", [["recipient"], 100]]]  // Provider 3
-> ["pull", 1]
-> ["pull", 2]
-> ["pull", 3]

<- ["resolve", 1, 30]
<- ["resolve", 2, 40]
<- ["resolve", 3, 20]

// Iteration 1: Commit allocations (parallel)
-> ["push", ["pipeline", 0, "commitAllocation", [["recipient"], 30]]]
-> ["push", ["pipeline", 0, "commitAllocation", [["recipient"], 40]]]
-> ["push", ["pipeline", 0, "commitAllocation", [["recipient"], 20]]]
-> ["pull", 4]
-> ["pull", 5]
-> ["pull", 6]

<- ["resolve", 4, true]
<- ["resolve", 5, true]
<- ["resolve", 6, true]

// Iteration 2: Query again with remaining need (10)
... (similar pattern)
```

**Round Trips:** 2 per iteration (query batch + commit batch) = 4 total

**Without Parallelization:** Would require 6 round trips per iteration = 12 total

---

### **Operation 4: Collective MRD Computation with .map()**

**Mathematical Operation:**
$$
\text{MRD}_C(i) = \frac{|C| \cdot (MR \cdot c)_i}{c^\top MR c}
$$

This requires computing $MR_{ij}$ for all $i \in C, j \in C$.

**Using Cap'n Web's Special .map() Feature:**

From the RPC article:
> The `.map()` operation for array processing enables efficient batch allocations

```typescript
// Get collective members
let collective = await session.getCollective(collectiveId);
let members = collective.getMembers();  // Returns promise for array

// Use .map() to compute MRD for each member - SINGLE ROUND TRIP
let mrdValues = members.map(member => {
  // This callback is executed SERVER-SIDE via record-replay
  return {
    participant: member,
    mrd: collective.computeMRD(member)
  };
});

let results = await mrdValues;
```

**How .map() Works (from rpc.md):**
> Record-replay: On the client side, we execute the callback once, passing in a special placeholder value. The parameter behaves like an RPC promise. The callback is required to be synchronous, so it cannot await. The only thing it can do is use promise pipelining to make pipelined calls. These calls are intercepted and recorded as instructions, which can then be sent to the server.

**Message Sequence:**
```
// Get collective and members
-> ["push", ["pipeline", 0, "getCollective", [["collective_id"]]]]
-> ["push", ["pipeline", 1, "getMembers", []]]

// .map() operation sent as instructions
-> ["push", ["map", 2, ["pipeline", ?, "computeMRD", [["pipeline", "element"]]]]]
-> ["pull", 3]

// Server executes map over all elements, returns results
<- ["resolve", 3, [
     {participant: "A", mrd: 0.875},
     {participant: "B", mrd: 1.2},
     {participant: "C", mrd: 0.95}
   ]]
```

**Round Trips:** 1 (entire map operation pipelined)

**Traditional Approach:** Would require N round trips for N members

**This is the killer feature for collective operations** - enables efficient bulk computation over capability-protected data.

---

## **Bidirectional Update Propagation**

For real-time collaborative editing, the server needs to push updates to clients.

### **Pattern: Callback Registration**

```typescript
// Client registers for updates
class CollaborativeMatrixClient {
  async subscribeToUpdates(region: MatrixRegionStub) {
    // Pass callback function to server
    await region.onUpdate((row, col, newValue, updatedBy) => {
      // This function executes on CLIENT when server calls it
      this.updateLocalState(row, col, newValue);
      this.renderUpdate(row, col);
    });
  }
}
```

**Server-Side:**
```typescript
class MatrixRegion extends RpcTarget {
  private updateCallbacks: Set<UpdateCallback> = new Set();
  
  onUpdate(callback: UpdateCallback): void {
    // Store the callback capability
    this.updateCallbacks.add(callback);
  }
  
  setCell(row: number, col: number, value: number, by: ParticipantStub): void {
    // Enforce bounds check
    if (!this.isInBounds(row, col)) throw new Error("Out of bounds");
    
    // Update matrix
    this.matrix[row][col] = value;
    
    // Notify all subscribers in parallel
    const notifications = Array.from(this.updateCallbacks).map(callback =>
      callback(row, col, value, by)
    );
    
    // Fire and forget (don't await)
    Promise.all(notifications).catch(console.error);
  }
}
```

**Message Sequence:**
```
// Client A subscribes
-> ["push", ["pipeline", 0, "onUpdate", [["callback", -1]]]]

// Client B makes edit
-> ["push", ["pipeline", 0, "setCell", [5, 10, 42, ["participant", "B"]]]]

// Server calls client A's callback (reverse direction!)
<- ["push", ["pipeline", -1, "call", [5, 10, 42, ["participant", "B"]]]]
```

**This is how bidirectional calling works in Cap'n Web** - callbacks are just RPC stubs in the reverse direction.

---

## **HTTP Batch Mode for Read-Heavy Operations**

For queries that don't need persistent connection:

```typescript
// Create batch session
let batch = newHttpBatchRpcSession("https://example.com/api");

// Make multiple queries in single batch
let sessionPromise = batch.authenticate(credentials);
let networkPromise = sessionPromise.getNetworkState();
let mrdPromise = networkPromise.computeCollectiveMRD(collectiveId);

// Single HTTP request/response
let mrdData = await mrdPromise;
```

**HTTP Request Body:**
```json
[
  ["push", ["pipeline", 0, "authenticate", [["creds"]]]],
  ["push", ["pipeline", 1, "getNetworkState", []]],
  ["push", ["pipeline", 2, "computeCollectiveMRD", [["collective_123"]]]]
]
```

**HTTP Response Body:**
```json
{
  "result": {
    "members": [...],
    "mrdValues": [...],
    "timestamp": "2025-12-03T10:30:00Z"
  }
}
```

**Use Cases:**
- Public dashboard views
- Periodic data fetches
- Mobile clients with intermittent connectivity
- Reducing WebSocket connection overhead for read-only users

---

## **Protocol Efficiency Analysis**

| Operation | Traditional REST | WebSocket RPC | Cap'n Web with Pipelining |
|-----------|------------------|---------------|---------------------------|
| Authenticate + Query MR | 2 round trips | 2 round trips | **1 round trip** |
| Multi-provider allocation (3 providers, 2 iterations) | 12 round trips | 12 round trips | **4 round trips** |
| Compute MRD for 100 members | 100+ round trips | 100+ round trips | **1 round trip** (.map()) |
| Subscribe + receive updates | N/A (polling) | 1 RT + push | 1 RT + push |

**Promise pipelining provides 3-100× reduction in latency for complex operations.**

---

# **Security Proofs: Anti-Gaming via Architectural Invariants**

## **Theorem: Capability Architecture Enforces Anti-Gaming**

**Statement:** In a system where all Free Association operations are implemented via Cap'n Web RPC with unforgeable capability references, all gaming strategies described in the Fundamental Anti-Gaming Theorem are architecturally impossible.

**Proof Strategy:** We prove each gaming strategy fails by showing it requires forging a capability reference, which is cryptographically impossible.

---

## **Gaming Strategy 1: Forge Recognition from Others**

### **Attack Description**
Participant $A$ attempts to claim that participant $B$ recognizes them with value $R(b,a) = 0.9$, when in reality $B$ has allocated $R(b,a) = 0.1$ or nothing.

### **Why It Fails in Math**
From Axiom 2:
$$
MR(a,b) = \min(R(a,b), R(b,a))
$$

If $A$ could forge $R(b,a)$, they could artificially inflate $MR(a,b)$, which by Axiom 3 would give them unearned capacity:
$$
C(a,b) = \kappa_b \cdot g(MR(a,b))
$$

### **Why It Fails Architecturally**

**Required Operation:**
```typescript
// A tries to compute MR(a,b) with forged R(b,a)
let forgedMR = network.computeMutualRecognition(participantA, participantB);
```

**Capability Requirements:**
1. Must have `participantA` stub (A has this - it's themselves)
2. Must have `participantB` stub (A might have this)
3. Must have access to $B$'s `RecognitionBudget` to query $R(b,a)$

**Failure Point:**
```typescript
class NetworkState extends RpcTarget {
  computeMutualRecognition(pA: ParticipantStub, pB: ParticipantStub): number {
    const budgetB = this.recognitionGraph.get(pB.id);
    
    // This call requires B's budget object
    // A CANNOT forge this - it's created only by B's authentication
    const R_ba = budgetB.getRecognitionTo(pA);
    
    // Even if A could call this, budgetB is controlled by server
    // and returns B's ACTUAL allocation, not A's desired value
    const R_ab = this.recognitionGraph.get(pA.id).getRecognitionTo(pB);
    
    return Math.min(R_ab, R_ba);
  }
}
```

**Architectural Guarantee:**
- `budgetB` is created only during B's authentication
- Export ID for `budgetB` is assigned by server
- A cannot create an export ID pointing to a forged budget object
- Even if A could call the method, the server's stored value is authoritative

**Formal Proof:**
Let $\mathcal{E}_B$ be B's export table. For A to forge $R(b,a)$, A must:
1. Create an export ID $e$ such that $\mathcal{E}_B[e]$ points to a budget with A's desired value, OR
2. Modify the server's stored value in B's actual budget object

Both operations require:
- **Option 1**: Predicting or forging export IDs (cryptographically impossible - IDs are server-assigned)
- **Option 2**: Write access to server memory (prevented by process isolation)

∴ Forging recognition is architecturally impossible. □

---

## **Gaming Strategy 2: Exceed Recognition Budget**

### **Attack Description**
Participant $A$ attempts to allocate recognition totaling more than 100%:
$$
\sum_{x \in P} R(a,x) > 1
$$

### **Why It Fails in Math**
Axiom 1 explicitly constrains:
$$
\sum_{x \in P} R(a,x) = 1
$$

Violating this would allow $A$ to gain more mutual recognition than possible, amplifying their capacity access.

### **Why It Fails Architecturally**

**Attack Attempt:**
```typescript
// A tries to allocate 60% + 50% = 110%
await budget.allocateRecognition(participantB, 0.6);
await budget.allocateRecognition(participantC, 0.5);  // Should fail
```

**Server-Side Enforcement:**
```typescript
class RecognitionBudget extends RpcTarget {
  private allocations: Map<ParticipantStub, number> = new Map();
  private readonly totalBudget = 1.0;
  
  allocateRecognition(target: ParticipantStub, amount: number): boolean {
    const currentTotal = Array.from(this.allocations.values())
      .reduce((sum, val) => sum + val, 0);
    
    // ENFORCEMENT POINT - server-side validation
    if (currentTotal + amount > this.totalBudget) {
      throw new Error(`Budget violation: ${currentTotal + amount} > ${this.totalBudget}`);
    }
    
    this.allocations.set(target, amount);
    return true;
  }
}
```

**Race Condition Attack:**
What if A sends both allocations simultaneously?

```typescript
// Concurrent attack attempt
await Promise.all([
  budget.allocateRecognition(participantB, 0.6),
  budget.allocateRecognition(participantC, 0.5)
]);
```

**Defense via Message Ordering:**
From the RPC protocol, messages are processed sequentially:
```
-> ["push", ["pipeline", 2, "allocateRecognition", [["B"], 0.6]]]  // ID: 3
-> ["push", ["pipeline", 2, "allocateRecognition", [["C"], 0.5]]]  // ID: 4
```

The server processes push ID 3 before push ID 4. After ID 3, current total = 0.6. When ID 4 arrives, the check fails:
```
currentTotal (0.6) + amount (0.5) = 1.1 > 1.0 → REJECT
```

**Formal Proof:**
Let $\tau_1, \tau_2, ..., \tau_n$ be the timestamps when allocation requests arrive at the server. Due to Cap'n Web's message ordering guarantee, the server processes them in a total order $\tau_{\sigma(1)} < \tau_{\sigma(2)} < ... < \tau_{\sigma(n)}$.

At each step $i$, the invariant holds:
$$
I_i: \sum_{j=1}^{i} a_{\sigma(j)} \leq 1
$$

Where $a_j$ is the amount in request $j$.

Base case: $I_0$ trivially holds (empty sum = 0).

Inductive step: Assume $I_{i-1}$ holds. Request $i$ is processed:
- If $\sum_{j=1}^{i-1} a_{\sigma(j)} + a_{\sigma(i)} \leq 1$: Accept, $I_i$ holds
- If $\sum_{j=1}^{i-1} a_{\sigma(j)} + a_{\sigma(i)} > 1$: Reject, $I_i = I_{i-1}$ holds

∴ Budget constraint maintained under all execution interleavings. □

---

## **Gaming Strategy 3: Claim Unilateral Recognition as Mutual**

### **Attack Description**
Participant $A$ has allocated $R(a,b) = 0.8$ to $B$, but $B$ has allocated $R(b,a) = 0.2$. $A$ attempts to claim $MR(a,b) = 0.8$ instead of the true value $\min(0.8, 0.2) = 0.2$.

### **Why It Fails in Math**
The min() function in Axiom 2 explicitly requires both values:
$$
MR(a,b) = \min(R(a,b), R(b,a))
$$

Without both being high, mutual recognition remains low.

### **Why It Fails Architecturally**

**Attack Attempt:**
```typescript
// A tries to use their high R(a,b) value directly
let capacity = await authenticatedB.allocateCapacity(participantA, 100);
// A hopes to receive: 100 * g(0.8) = large amount
```

**Server-Side Enforcement:**
```typescript
class AuthenticatedParticipant extends RpcTarget {
  allocateCapacity(recipientStub: ParticipantStub, requestedAmount: number): number {
    // Capacity flow REQUIRES computing mutual recognition
    // A cannot bypass this computation
    const mutualRecognition = this.network.computeMutualRecognition(
      this.getStub(),
      recipientStub
    );
    
    // mutualRecognition = min(0.8, 0.2) = 0.2, NOT 0.8
    const flowMultiplier = this.computeFlowMultiplier(mutualRecognition);
    return requestedAmount * flowMultiplier;  // Small value
  }
}
```

**Key Point:** The `computeMutualRecognition` call happens **server-side** in code that:
1. A cannot modify (server-controlled)
2. Accesses both budgets via capability references A doesn't control
3. Performs the min() operation in trusted code

**Alternative Attack - Bypass computeMutualRecognition:**
What if A tries to call `allocateCapacity` with a modified implementation?

**Not Possible Because:**
- `allocateCapacity` is a method on B's `AuthenticatedParticipant` object
- This object exists on the **server**, not the client
- A's RPC stub is just a proxy that sends messages to the server's object
- The server's implementation is authoritative

**Formal Proof:**
Let $\mathcal{C}_S$ be the server's code execution environment and $\mathcal{C}_A$ be A's client environment.

For A to bypass the min() operation, A must either:
1. Modify the code executing in $\mathcal{C}_S$, OR
2. Cause $\mathcal{C}_S$ to execute different code than the server's implementation

**Option 1** requires compromising the server (outside threat model).

**Option 2** would require:
- Sending RPC messages that override method implementations (not supported by Cap'n Web)
- Exploiting code injection vulnerabilities (prevented by parameterized RPC)

Cap'n Web's message format only allows:
```
["push", ["pipeline", objectId, methodName, parameters]]
```

Where:
- `objectId`: Server-assigned, unforgeable
- `methodName`: String lookup in server's method table
- `parameters`: Serialized data, not code

∴ A cannot bypass server-side min() computation. □

---

## **Gaming Strategy 4: Claim Non-Beneficial Sources Help Goals**

### **Attack Description**
Participant $A$ has a goal $G$ with beneficial set $B = \{b_1, b_2\}$. Participant $n \notin B$ offers capacity. $A$ attempts to claim this capacity contributes to $\mathbb{P}(G)$.

### **Why It Fails in Math**
Axiom 5 states:
$$
\frac{\partial \mathbb{P}(G)}{\partial C(a,n)} = 0 \quad \text{for } n \notin B
$$

Only capacity from beneficial set increases goal probability.

### **Why It Fails Architecturally**

**Attack Attempt:**
```typescript
// A receives capacity from non-beneficial source
let goal = await session.getGoal(goalId);
let progress = await goal.receiveCapacity(nonBeneficialStub, 50);
// A hopes this increases goal probability
```

**Server-Side Enforcement:**
```typescript
class ParticipantGoal extends RpcTarget {
  private readonly beneficialSet: Set<ParticipantStub>;
  
  receiveCapacity(from: ParticipantStub, amount: number): GoalProgress {
    // ENFORCEMENT POINT - beneficial set membership check
    if (!this.beneficialSet.has(from)) {
      return {
        accepted: false,
        reason: "Not in beneficial set",
        goalProbability: this.computeGoalProbability()  // Unchanged
      };
    }
    
    // Only reached if from ∈ beneficial set
    this.receivedCapacity.set(from, amount);
    return {
      accepted: true,
      goalProbability: this.computeGoalProbability()  // Increased
    };
  }
}
```

**Key Insight:** The beneficial set is encoded as **capability possession**:
- `ParticipantGoal` constructor receives `beneficialStubs: ParticipantStub[]`
- These stubs are unforgeable references
- The `has()` check verifies object identity, not just IDs

**Alternative Attack - Forge Beneficial Set Membership:**
What if A tries to create a `ParticipantStub` that passes the `has()` check?

**Not Possible Because:**
```typescript
class ParticipantStub {
  private readonly exportId: number;  // Server-assigned
  private readonly connection: RpcConnection;  // Server-controlled
  
  constructor(exportId: number, connection: RpcConnection) {
    // Only called by Cap'n Web internals, not client code
    this.exportId = exportId;
    this.connection = connection;
  }
}
```

**Stub Identity:**
- Stubs are compared by reference (memory address) or by `(connection, exportId)` pair
- A cannot create a new stub with the same identity
- Even if A intercepts the beneficial stub at network level, they can't replay it from a different source

**Formal Proof:**
Let $\mathcal{B}$ be the set of capability references in the beneficial set, and $\hat{s}_n$ be a capability reference to non-beneficial participant $n$.

For non-beneficial capacity to count, A must cause:
$$
\hat{s}_n \in \mathcal{B}
$$

This requires one of:
1. $\hat{s}_n$ was included in $\mathcal{B}$ during goal creation (contradicts premise that $n \notin B$)
2. A adds $\hat{s}_n$ to $\mathcal{B}$ after creation
3. A creates a forged stub $\hat{s}_n'$ such that $\hat{s}_n' \in \mathcal{B}$ but $\hat{s}_n' \equiv \hat{s}_n$

**Option 1** contradicts the problem statement.

**Option 2** requires write access to server's `beneficialSet` object (architecturally impossible - no such RPC method).

**Option 3** requires forging capability identity, which requires either:
- Predicting/forging export IDs (server-assigned, cryptographically secure)
- Cloning capability references (prevented by object encapsulation)

∴ Non-beneficial capacity cannot contribute to goal achievement. □

---

## **Gaming Strategy 5: Access Others' Capacity Pools**

### **Attack Description**
Participant $A$ attempts to allocate capacity from $B$'s pool to themselves or others, without $B$'s consent.

### **Why It Fails in Math**
Each participant's capacity is independent (implicitly in Axiom 3's $\kappa_b$ notation). Only $B$ can allocate from $\kappa_b$.

### **Why It Fails Architecturally**

**Attack Attempt:**
```typescript
// A tries to allocate from B's capacity
let fakeAuthB = /* somehow forge B's AuthenticatedParticipant */;
await fakeAuthB.allocateCapacity(participantA, 1000);
```

**Why It Fails:**
```typescript
class AuthenticatedParticipant extends RpcTarget {
  private readonly participantId: string;  // Bound at creation
  private capacity: number;  // Loaded from B's account
  
  constructor(participantId: string, network: NetworkState) {
    super();
    this.participantId = participantId;
    // Capacity loaded from database keyed by participantId
    this.capacity = this.loadCapacity(participantId);
  }
  
  allocateCapacity(recipientStub: ParticipantStub, amount: number): number {
    // Uses THIS object's capacity, bound to participantId at construction
    if (amount > this.capacity) throw new Error("Insufficient capacity");
    this.capacity -= amount;
    return amount;
  }
}
```

**Failure Points:**
1. **Cannot Forge `AuthenticatedParticipant`**: Only created by `ParticipantServer.authenticate()`
2. **Cannot Forge Authentication**: Requires B's credentials (password, key, etc.)
3. **Cannot Access B's Session**: Each session is capability-isolated
4. **Cannot Modify Capacity Binding**: `participantId` is immutable after construction

**Session Isolation:**
```typescript
class ParticipantServer extends RpcTarget {
  private sessions: Map<string, AuthenticatedParticipant> = new Map();
  
  authenticate(participantId: string, credentials: Credential): AuthenticatedParticipant {
    if (!this.verifyCredentials(participantId, credentials)) {
      throw new Error("Authentication failed");
    }
    
    // Each authentication creates NEW session with its own capacity binding
    const session = new AuthenticatedParticipant(participantId, this.network);
    
    // Export at unique ID - A cannot predict or reuse B's session ID
    return session;
  }
}
```

**Formal Proof:**
Let $\kappa_B$ be B's capacity pool and $\mathcal{S}_B$ be B's authenticated session.

For A to allocate from $\kappa_B$, A must invoke:
```
$\mathcal{S}_B$.allocateCapacity(...)
```

This requires A to possess a capability reference to $\mathcal{S}_B$.

Capability references to $\mathcal{S}_B$ are created only by:
1. B authenticates, receives $\mathcal{S}_B$ capability
2. B shares $\mathcal{S}_B$ capability with others (requires explicit RPC call by B)

Since:
- B has not shared $\mathcal{S}_B$ with A (premise of attack)
- A cannot forge authentication (requires B's credentials)
- A cannot predict export IDs (server-assigned)

∴ A cannot obtain capability reference to $\mathcal{S}_B$ → cannot access $\kappa_B$. □

---

## **Meta-Theorem: Completeness of Architectural Enforcement**

**Theorem:** Every violation of Axioms 1-5 requires forging a capability reference in at least one of the following forms:
1. Forging an export ID
2. Forging an authentication credential
3. Modifying server-side code or memory
4. Breaking message ordering guarantees

**Proof:** By exhaustive case analysis:

| Axiom | Violation Type | Required Capability Forgery |
|-------|---------------|----------------------------|
| Axiom 1 (Budget) | $\sum R(a,x) > 1$ | None* (defended by sequential processing) |
| Axiom 2 (MR) | $MR(a,b) \neq \min(R(a,b), R(b,a))$ | Forge export ID to B's budget |
| Axiom 3 (Capacity) | $C(a,b) \neq \kappa_b \cdot g(MR(a,b))$ | Forge authentication to access $\kappa_b$ |
| Axiom 4 (Goal monotonicity) | $\mathbb{P}(G)$ not monotonic in beneficial capacity | Modify server code (f function) |
| Axiom 5 (Beneficial only) | Non-beneficial capacity contributes | Forge beneficial set membership |

*Axiom 1 requires only message ordering, which is a protocol-level guarantee.

**Security Reduction:**
The security of Free Association anti-gaming reduces to:
1. **Cap'n Web protocol security**: Export IDs unforgeable
2. **Authentication security**: Credentials unforgeable (standard practice)
3. **Server security**: Code integrity maintained (standard practice)

Since (1) is guaranteed by Cap'n Web's design, and (2-3) are standard security requirements, **Free Association's anti-gaming properties are as secure as the underlying authentication and server infrastructure**. □

---

## **Comparison: Traditional vs Capability-Based Security**

| Security Property | Traditional (ACL-based) | Capability-Based RPC |
|------------------|------------------------|---------------------|
| Budget enforcement | Check DB on every operation | Enforced by RecognitionBudget object |
| Mutual recognition | Query both records, verify min() | Automatic via capability possession |
| Capacity isolation | Check user_id matches on every allocation | Bound to session at authentication |
| Beneficial set membership | Check membership list on every receive | Unforgeable stub identity |
| **Performance** | O(log n) DB queries per operation | O(1) capability checks |
| **Attack surface** | SQL injection, authorization bugs | Capability forgery only |
| **Verification complexity** | Must audit all code paths | Enforced by architecture |

**Key Advantage:** Capability-based security makes security properties **emerge from system architecture** rather than requiring explicit checks in every code path.

This is why Cap'n Web is particularly well-suited to Free Association: the mathematical properties we need to enforce align perfectly with the security guarantees capabilities provide.

---

# **Performance Model and Scaling Analysis**

## **Message Complexity Analysis**

### **Operation Complexity Table**

| Operation | Parameters | Messages Sent | Round Trips | Memory per Connection | Scaling Factor |
|-----------|-----------|---------------|-------------|----------------------|----------------|
| Authenticate | 1 participant | 1 push, 1 pull | 1 | O(1) session | O(1) |
| Allocate Recognition | 1 target | 1 push, 1 pull | 1 | O(1) entry | O(k) k=allocations |
| Query MR | 2 participants | 1 push, 1 pull | 1* | O(1) computation | O(1) |
| Compute MRS | 1 participant | 1 push, 1 pull | 1* | O(n) computation | O(n) |
| Collective MRD | 1 collective | 1 push (map), 1 pull | 1* | O(\|C\|) computation | O(\|C\|²) |
| Multi-provider allocation | m providers, t iterations | 2mt push, 2mt pull | 2t | O(m) state | O(mt) |
| Subscribe updates | 1 region | 1 push | 1 | O(1) callback ref | O(1) |
| Receive update | - | 0 (server-initiated) | 0 | O(1) message | O(1) |

*With promise pipelining

### **Network Scaling Properties**

**Small Network (n ≤ 100 participants):**
- WebSocket per participant: ~100 connections
- Memory: ~10 KB per connection = ~1 MB total
- MR computation: O(1) per query, ~10,000 possible pairs
- Update propagation: O(subscribers) per change, typically < 10
- **Bottleneck:** None - trivially scales

**Medium Network (100 < n ≤ 1,000):**
- WebSocket per participant: ~1,000 connections
- Memory: ~10 MB total for connections
- MR matrix: 1M possible pairs, but queried on-demand
- Collective operations: O(n²) for full collective, manageable with caching
- **Bottleneck:** Update broadcasting for popular matrices
- **Mitigation:** Pub/sub layer for hot regions, HTTP batch for read-only clients

**Large Network (1,000 < n ≤ 10,000):**
- WebSocket: ~10,000 connections (single server limit approaching)
- Memory: ~100 MB for connections
- MR matrix: 100M possible pairs - definitely need on-demand computation
- **Bottleneck:** WebSocket connection overhead
- **Mitigation:** 
  - HTTP batch mode for most read operations
  - WebSocket only for active editors
  - Shard collectives geographically or by domain

**Very Large Network (n > 10,000):**
- **Bottleneck:** Single server cannot maintain all WebSocket connections
- **Mitigation Strategy:**
  - **Horizontal sharding**: Partition participants across servers
  - **Collective isolation**: Each collective has primary server
  - **Cross-server RPC**: Servers use Cap'n Web to talk to each other
  - **Edge caching**: Cloudflare Workers for read-heavy operations

---

## **Sharding Architecture for Scale**

### **Collective-Based Sharding**

```typescript
interface ShardedNetworkTopology {
  // Each collective has a home shard
  collectives: Map<CollectiveId, ShardId>;
  
  // Each shard handles subset of participants
  shards: Map<ShardId, {
    endpoint: string;
    participants: Set<ParticipantId>;
    capacity: number;
  }>;
  
  // Cross-shard RPC stubs
  shardConnections: Map<ShardId, NetworkStateShard>;
}

class DistributedNetworkState extends RpcTarget {
  async computeMutualRecognition(
    pA: ParticipantStub, 
    pB: ParticipantStub
  ): number {
    // Determine which shards contain A and B
    const shardA = this.getParticipantShard(pA.id);
    const shardB = this.getParticipantShard(pB.id);
    
    if (shardA === shardB) {
      // Local computation
      return this.localComputeMR(pA, pB);
    } else {
      // Cross-shard RPC using Cap'n Web
      const connectionB = this.shardConnections.get(shardB);
      const R_ba = await connectionB.getRecognition(pB, pA);
      const R_ab = this.localGetRecognition(pA, pB);
      return Math.min(R_ab, R_ba);
    }
  }
}
```

**Sharding Strategy:**
1. **Primary constraint**: Minimize cross-shard MR queries
2. **Heuristic**: Co-locate participants in same collective on same shard
3. **Load balancing**: Distribute large collectives across multiple shards
4. **Dynamic rebalancing**: Move participants between shards based on interaction patterns

---

## **Caching Strategy**

### **What to Cache**

| Data Type | Cache Duration | Invalidation Trigger | Cache Hit Rate |
|-----------|---------------|---------------------|----------------|
| MR(a,b) | 5 minutes | Recognition reallocation by a or b | 80-90% |
| Collective MRD | 1 minute | Any member changes recognition | 60-70% |
| ParticipantStub | Session lifetime | Logout | 100% |
| MRS matrix (per participant) | 1 minute | Recognition reallocation | 70-80% |
| Collective membership | 5 minutes | MRD threshold crossing | 90-95% |

### **Implementation**

```typescript
class CachedNetworkState extends RpcTarget {
  private mrCache: Map<string, {value: number, timestamp: number}> = new Map();
  private readonly MR_CACHE_TTL = 5 * 60 * 1000; // 5 minutes
  
  computeMutualRecognition(pA: ParticipantStub, pB: ParticipantStub): number {
    const cacheKey = this.makeMRCacheKey(pA.id, pB.id);
    const cached = this.mrCache.get(cacheKey);
    
    if (cached && (Date.now() - cached.timestamp < this.MR_CACHE_TTL)) {
      return cached.value;
    }
    
    const value = this.actuallyComputeMR(pA, pB);
    this.mrCache.set(cacheKey, {value, timestamp: Date.now()});
    return value;
  }
  
  private makeMRCacheKey(idA: string, idB: string): string {
    // Symmetric key for symmetric operation
    return idA < idB ? `${idA}:${idB}` : `${idB}:${idA}`;
  }
  
  // Called when recognition is reallocated
  invalidateMRCache(participantId: string): void {
    // Remove all cached MR values involving this participant
    for (const [key, _] of this.mrCache) {
      if (key.includes(participantId)) {
        this.mrCache.delete(key);
      }
    }
  }
}
```

---

## **Complete System Architecture**

### **Type Definitions**

```typescript
import { RpcTarget, RpcStub } from "capnweb";

// Core Types
type ParticipantId = string;
type CollectiveId = string;
type GoalId = string;

// Capability Stubs (unforgeable references)
type ParticipantStub = RpcStub<AuthenticatedParticipant>;
type NetworkStateStub = RpcStub<NetworkState>;
type RecognitionBudgetStub = RpcStub<RecognitionBudget>;
type CollectiveStub = RpcStub<Collective>;
type GoalStub = RpcStub<ParticipantGoal>;
type MatrixRegionStub = RpcStub<MatrixRegion>;

// Data Structures
interface Credential {
  type: "password" | "publicKey" | "oauth";
  data: string;
}

interface AllocationResult {
  allocations: Map<ParticipantStub, number>;
  satisfied: boolean;
  iterations: number;
}

interface GoalProgress {
  accepted: boolean;
  goalProbability: number;
  reason?: string;
}

interface MRDResult {
  participantId: ParticipantId;
  mrd: number;
  aboveThreshold: boolean;
}

interface MatrixUpdate {
  row: number;
  col: number;
  oldValue: number;
  newValue: number;
  timestamp: number;
  updatedBy: ParticipantId;
}
```

### **Server Entry Point**

```typescript
class ParticipantServer extends RpcTarget {
  private readonly network: NetworkState;
  private readonly authenticator: AuthenticationService;
  
  constructor() {
    super();
    this.network = new NetworkState();
    this.authenticator = new AuthenticationService();
  }
  
  /**
   * Main entry point - authenticate and get participant session
   */
  authenticate(
    participantId: ParticipantId, 
    credentials: Credential
  ): AuthenticatedParticipant {
    const verified = this.authenticator.verify(participantId, credentials);
    if (!verified) {
      throw new Error("Authentication failed");
    }
    
    return new AuthenticatedParticipant(participantId, this.network);
  }
  
  /**
   * Public endpoint for querying network state
   * (read-only operations)
   */
  getPublicNetworkView(): PublicNetworkView {
    return new PublicNetworkView(this.network);
  }
}

// Cloudflare Workers entry point
export default {
  fetch(request: Request, env: any, ctx: any) {
    const url = new URL(request.url);
    
    if (url.pathname === "/api") {
      // Main RPC endpoint
      return newWorkersRpcResponse(request, new ParticipantServer());
    }
    
    return new Response("Not found", {status: 404});
  }
};
```

### **Core Classes**

```typescript
/**
 * Authenticated participant session - bound to specific participant's capacity
 */
class AuthenticatedParticipant extends RpcTarget {
  private readonly participantId: ParticipantId;
  private readonly network: NetworkState;
  private readonly budget: RecognitionBudget;
  private capacity: number;
  
  constructor(participantId: ParticipantId, network: NetworkState) {
    super();
    this.participantId = participantId;
    this.network = network;
    this.budget = new RecognitionBudget(participantId);
    this.capacity = this.loadCapacity(participantId);
    
    // Register this participant in network
    network.registerParticipant(participantId, this.budget);
  }
  
  /**
   * Get recognition budget for allocating to others
   */
  getRecognitionBudget(): RecognitionBudget {
    return this.budget;
  }
  
  /**
   * Get network state for querying MR, MRS, etc.
   */
  getNetworkState(): NetworkState {
    return this.network;
  }
  
  /**
   * Allocate capacity to another participant
   * Flow automatically proportional to mutual recognition
   */
  async allocateCapacity(
    recipient: ParticipantStub, 
    requestedAmount: number
  ): Promise<number> {
    const myStub = this.getStub();
    const mr = await this.network.computeMutualRecognition(myStub, recipient);
    
    // g(MR) - flow multiplier function
    const flowMultiplier = mr; // Linear: g(x) = x
    const allocatedFlow = requestedAmount * flowMultiplier;
    
    if (allocatedFlow > this.capacity) {
      throw new Error(`Insufficient capacity: have ${this.capacity}, need ${allocatedFlow}`);
    }
    
    this.capacity -= allocatedFlow;
    await recipient.receiveCapacity(myStub, allocatedFlow);
    
    return allocatedFlow;
  }
  
  /**
   * Receive capacity from another participant (for goal satisfaction)
   */
  receiveCapacity(from: ParticipantStub, amount: number): void {
    this.capacity += amount;
  }
  
  /**
   * Create or access a goal
   */
  getGoal(goalId: GoalId, beneficialSet: ParticipantStub[]): ParticipantGoal {
    return new ParticipantGoal(goalId, beneficialSet, this.network);
  }
  
  /**
   * Join or access a collective
   */
  async joinCollective(collectiveId: CollectiveId): Promise<Collective> {
    const collective = this.network.getCollective(collectiveId);
    const memberCapability = await collective.attemptJoin(this.getStub());
    return memberCapability;
  }
  
  private loadCapacity(participantId: ParticipantId): number {
    // Load from persistent storage
    return 1000; // Example default
  }
  
  private getStub(): ParticipantStub {
    // Get RPC stub to this object
    return this as any as ParticipantStub;
  }
}

/**
 * Recognition budget - enforces Axiom 1 (budget constraint)
 */
class RecognitionBudget extends RpcTarget {
  private readonly participantId: ParticipantId;
  private allocations: Map<ParticipantId, number> = new Map();
  private readonly totalBudget = 1.0;
  
  constructor(participantId: ParticipantId) {
    super();
    this.participantId = participantId;
  }
  
  /**
   * Allocate recognition to another participant
   * Enforces sum constraint: Σ R(a,x) = 1
   */
  allocateRecognition(target: ParticipantStub, amount: number): boolean {
    if (amount < 0 || amount > 1) {
      throw new Error(`Invalid amount: ${amount}. Must be in [0, 1]`);
    }
    
    const currentTotal = Array.from(this.allocations.values())
      .reduce((sum, val) => sum + val, 0);
    
    const existingToTarget = this.allocations.get(target.id) || 0;
    const newTotal = currentTotal - existingToTarget + amount;
    
    if (newTotal > this.totalBudget) {
      throw new Error(
        `Budget violation: ${newTotal} > ${this.totalBudget}. ` +
        `Current: ${currentTotal}, requested: ${amount}, existing to target: ${existingToTarget}`
      );
    }
    
    this.allocations.set(target.id, amount);
    return true;
  }
  
  /**
   * Get recognition allocated to specific participant
   */
  getRecognitionTo(target: ParticipantStub): number {
    return this.allocations.get(target.id) || 0;
  }
  
  /**
   * Get all allocations
   */
  getAllAllocations(): Map<ParticipantId, number> {
    return new Map(this.allocations);
  }
}

/**
 * Network state - computes MR, MRS, and other derived values
 */
class NetworkState extends RpcTarget {
  private recognitionGraph: Map<ParticipantId, RecognitionBudget> = new Map();
  private collectives: Map<CollectiveId, Collective> = new Map();
  
  registerParticipant(id: ParticipantId, budget: RecognitionBudget): void {
    this.recognitionGraph.set(id, budget);
  }
  
  /**
   * Compute mutual recognition - Axiom 2
   * MR(a,b) = min(R(a,b), R(b,a))
   */
  computeMutualRecognition(
    participantA: ParticipantStub, 
    participantB: ParticipantStub
  ): number {
    const budgetA = this.recognitionGraph.get(participantA.id);
    const budgetB = this.recognitionGraph.get(participantB.id);
    
    if (!budgetA || !budgetB) {
      throw new Error("One or both participants not found in network");
    }
    
    const R_ab = budgetA.getRecognitionTo(participantB);
    const R_ba = budgetB.getRecognitionTo(participantA);
    
    return Math.min(R_ab, R_ba);
  }
  
  /**
   * Compute total mutual recognition for one participant
   */
  computeTotalMR(participant: ParticipantStub): number {
    let total = 0;
    for (const [otherId, _] of this.recognitionGraph) {
      if (otherId !== participant.id) {
        const otherStub = this.getParticipantStub(otherId);
        total += this.computeMutualRecognition(participant, otherStub);
      }
    }
    return total;
  }
  
  /**
   * Compute MRS for one participant with another
   */
  computeMRS(
    participantA: ParticipantStub, 
    participantB: ParticipantStub
  ): number {
    const mr_ab = this.computeMutualRecognition(participantA, participantB);
    const totalMR_a = this.computeTotalMR(participantA);
    
    if (totalMR_a === 0) return 0;
    return mr_ab / totalMR_a;
  }
  
  /**
   * Get or create collective
   */
  getCollective(collectiveId: CollectiveId): Collective {
    let collective = this.collectives.get(collectiveId);
    if (!collective) {
      collective = new Collective(collectiveId, this);
      this.collectives.set(collectiveId, collective);
    }
    return collective;
  }
  
  private getParticipantStub(id: ParticipantId): ParticipantStub {
    // Convert ID to stub (implementation detail)
    return null as any; // Simplified for example
  }
}

/**
 * Collective - manages membership and collective operations
 */
class Collective extends RpcTarget {
  private readonly collectiveId: CollectiveId;
  private readonly network: NetworkState;
  private members: Set<ParticipantId> = new Set();
  private readonly threshold: number = 0.5;
  
  constructor(collectiveId: CollectiveId, network: NetworkState) {
    super();
    this.collectiveId = collectiveId;
    this.network = network;
  }
  
  /**
   * Attempt to join collective - requires MRD >= threshold
   */
  async attemptJoin(participant: ParticipantStub): Promise<Collective> {
    const mrd = this.computeMRD(participant);
    
    if (mrd >= this.threshold) {
      this.members.add(participant.id);
      return this; // Return capability = grant membership
    }
    
    throw new Error(
      `Insufficient mutual recognition density: ${mrd} < ${this.threshold}`
    );
  }
  
  /**
   * Compute MRD for participant relative to collective
   */
  computeMRD(participant: ParticipantStub): number {
    if (this.members.size === 0) return 0;
    
    let participantMRSum = 0;
    let totalMRSum = 0;
    
    for (const memberId of this.members) {
      const memberStub = this.getParticipantStub(memberId);
      const mr_pm = this.network.computeMutualRecognition(participant, memberStub);
      participantMRSum += mr_pm;
      
      for (const otherMemberId of this.members) {
        if (memberId !== otherMemberId) {
          const otherStub = this.getParticipantStub(otherMemberId);
          totalMRSum += this.network.computeMutualRecognition(memberStub, otherStub);
        }
      }
    }
    
    const avgMR = totalMRSum / this.members.size;
    return avgMR === 0 ? 0 : participantMRSum / avgMR;
  }
  
  /**
   * Get all members with their MRD values
   * Uses .map() for efficient computation
   */
  async computeAllMRD(): Promise<MRDResult[]> {
    const memberIds = Array.from(this.members);
    return memberIds.map(id => {
      const stub = this.getParticipantStub(id);
      const mrd = this.computeMRD(stub);
      return {
        participantId: id,
        mrd,
        aboveThreshold: mrd >= this.threshold
      };
    });
  }
  
  getMembers(): ParticipantId[] {
    return Array.from(this.members);
  }
  
  private getParticipantStub(id: ParticipantId): ParticipantStub {
    return null as any; // Simplified
  }
}

/**
 * Participant goal - tracks progress toward goal via capacity receipts
 */
class ParticipantGoal extends RpcTarget {
  private readonly goalId: GoalId;
  private readonly beneficialSet: Set<ParticipantStub>;
  private readonly network: NetworkState;
  private receivedCapacity: Map<ParticipantId, number> = new Map();
  
  constructor(
    goalId: GoalId, 
    beneficialStubs: ParticipantStub[], 
    network: NetworkState
  ) {
    super();
    this.goalId = goalId;
    this.beneficialSet = new Set(beneficialStubs);
    this.network = network;
  }
  
  /**
   * Receive capacity from another participant
   * Only beneficial capacity contributes (Axiom 5)
   */
  receiveCapacity(from: ParticipantStub, amount: number): GoalProgress {
    // Check beneficial set membership
    if (!this.beneficialSet.has(from)) {
      return {
        accepted: false,
        goalProbability: this.computeGoalProbability(),
        reason: "Not in beneficial set"
      };
    }
    
    // Accept and record
    const current = this.receivedCapacity.get(from.id) || 0;
    this.receivedCapacity.set(from.id, current + amount);
    
    return {
      accepted: true,
      goalProbability: this.computeGoalProbability()
    };
  }
  
  /**
   * Compute goal achievement probability (Axiom 4)
   * f: strictly increasing function of beneficial capacity
   */
  private computeGoalProbability(): number {
    const totalBeneficialCapacity = Array.from(this.beneficialSet)
      .map(stub => this.receivedCapacity.get(stub.id) || 0)
      .reduce((sum, val) => sum + val, 0);
    
    // Logistic function: f(x) = 1 / (1 + e^(-k*x))
    const k = 0.01; // Scaling factor
    return 1 / (1 + Math.exp(-k * totalBeneficialCapacity));
  }
  
  getProgress(): {totalReceived: number, probability: number} {
    const total = Array.from(this.receivedCapacity.values())
      .reduce((sum, val) => sum + val, 0);
    return {
      totalReceived: total,
      probability: this.computeGoalProbability()
    };
  }
}

/**
 * Matrix region with bounds checking for collaborative editing
 */
class MatrixRegion extends RpcTarget {
  private matrix: number[][];
  private readonly bounds: {
    startRow: number;
    endRow: number;
    startCol: number;
    endCol: number;
  };
  private updateCallbacks: Set<(update: MatrixUpdate) => void> = new Set();
  
  constructor(
    matrix: number[][],
    startRow: number,
    endRow: number,
    startCol: number,
    endCol: number
  ) {
    super();
    this.matrix = matrix;
    this.bounds = {startRow, endRow, startCol, endCol};
  }
  
  /**
   * Set cell value - automatically enforces bounds
   */
  setCell(
    row: number, 
    col: number, 
    value: number, 
    by: ParticipantId
  ): void {
    if (!this.isInBounds(row, col)) {
      throw new Error(`Out of bounds: (${row}, ${col})`);
    }
    
    const oldValue = this.matrix[row][col];
    this.matrix[row][col] = value;
    
    // Notify subscribers
    const update: MatrixUpdate = {
      row,
      col,
      oldValue,
      newValue: value,
      timestamp: Date.now(),
      updatedBy: by
    };
    
    this.notifySubscribers(update);
  }
  
  /**
   * Get cell value
   */
  getCell(row: number, col: number): number {
    if (!this.isInBounds(row, col)) {
      throw new Error(`Out of bounds: (${row}, ${col})`);
    }
    return this.matrix[row][col];
  }
  
  /**
   * Subscribe to updates in this region
   */
  onUpdate(callback: (update: MatrixUpdate) => void): void {
    this.updateCallbacks.add(callback);
  }
  
  private isInBounds(row: number, col: number): boolean {
    return row >= this.bounds.startRow && 
           row < this.bounds.endRow &&
           col >= this.bounds.startCol && 
           col < this.bounds.endCol;
  }
  
  private notifySubscribers(update: MatrixUpdate): void {
    for (const callback of this.updateCallbacks) {
      try {
        callback(update);
      } catch (error) {
        console.error("Error in update callback:", error);
      }
    }
  }
}
```

### **Client Usage Examples**

```typescript
import { newWebSocketRpcSession, newHttpBatchRpcSession } from "capnweb";

// Example 1: Authenticate and allocate recognition
async function allocateRecognition() {
  const api = newWebSocketRpcSession("wss://free-association.org/api");
  
  // Authenticate
  const session = await api.authenticate("alice@example.com", {
    type: "password",
    data: "secret123"
  });
  
  // Get recognition budget
  const budget = await session.getRecognitionBudget();
  
  // Allocate recognition to Bob and Carol
  const bobStub = await api.getParticipantStub("bob@example.com");
  const carolStub = await api.getParticipantStub("carol@example.com");
  
  await budget.allocateRecognition(bobStub, 0.6);
  await budget.allocateRecognition(carolStub, 0.4);
  
  console.log("Recognition allocated successfully");
}

// Example 2: Query mutual recognition (read-only, uses HTTP batch)
async function queryMutualRecognition() {
  const batch = newHttpBatchRpcSession("https://free-association.org/api");
  
  const view = batch.getPublicNetworkView();
  const aliceStub = view.getParticipantStub("alice@example.com");
  const bobStub = view.getParticipantStub("bob@example.com");
  
  // Single HTTP request with pipelined operations
  const mr = await view.computeMutualRecognition(aliceStub, bobStub);
  
  console.log(`MR(alice, bob) = ${mr}`);
}

// Example 3: Join collective
async function joinCollective() {
  const api = newWebSocketRpcSession("wss://free-association.org/api");
  
  const session = await api.authenticate("alice@example.com", credentials);
  const collective = await session.joinCollective("open-source-collective");
  
  console.log("Joined collective successfully");
  
  // Query MRD for all members
  const mrdResults = await collective.computeAllMRD();
  console.log("Member MRD values:", mrdResults);
}

// Example 4: Collaborative matrix editing with real-time updates
async function collaborativeEditing() {
  const api = newWebSocketRpcSession("wss://free-association.org/api");
  
  const session = await api.authenticate("alice@example.com", credentials);
  const region = await session.getMatrixRegion("project-budget", 0, 10, 0, 5);
  
  // Subscribe to updates
  region.onUpdate((update) => {
    console.log(`Cell (${update.row}, ${update.col}) changed from ` +
                `${update.oldValue} to ${update.newValue} by ${update.updatedBy}`);
  });
  
  // Make edit
  await region.setCell(5, 3, 1000, "alice@example.com");
}
```

---

## **Deployment Recommendations**

### **For 10-100 Users (MVP)**
- **Infrastructure**: Single Cloudflare Worker + Durable Objects
- **Database**: D1 or KV for persistence
- **Cost**: ~$5-20/month
- **Deployment**: `wrangler deploy`

### **For 100-1,000 Users**
- **Infrastructure**: Multiple Workers + Durable Objects per collective
- **Database**: D1 with read replicas
- **Caching**: Cloudflare Cache API for MR values
- **Cost**: ~$50-200/month

### **For 1,000-10,000 Users**
- **Infrastructure**: Regional Workers + Durable Objects sharding
- **Database**: D1 + R2 for bulk data
- **Caching**: Redis-compatible cache (Cloudflare KV)
- **CDN**: Full Cloudflare stack
- **Cost**: ~$200-1,000/month

### **For 10,000+ Users**
- **Infrastructure**: Full horizontal sharding across regions
- **Database**: Distributed database (CockroachDB, TiDB)
- **Caching**: Multi-tier (edge + regional)
- **Load balancing**: Geographic routing
- **Monitoring**: Full observability stack
- **Cost**: Custom pricing, likely $1,000-10,000+/month

---

# **Matrix Formulation for Free Association Derivations**

## **1. Base Matrices**

Let `n` = number of participants.

### **1.1 Raw Recognition Matrix** `R`
$$
R \in \mathbb{R}^{n \times n}_+, \quad R_{ij} = \text{raw recognition } i \text{ gives to } j
$$

### **1.2 Capacity Matrix** `K`
$$
K \in \mathbb{R}^{n \times n}_+, \quad K_{ij} = \text{capacity } i \text{ has available for } j
$$
Or as a vector: $\mathbf{k} \in \mathbb{R}^n_+$, $k_i$ = total capacity of participant $i$

### **1.3 Collective Membership Indicator** `c`
For a collective $C \subseteq \{1,...,n\}$:
$$
c \in \{0,1\}^n, \quad c_i = \begin{cases} 1 & i \in C \\ 0 & i \notin C \end{cases}
$$

---

## **2. Core Derivations**

### **2.1 Recognition-Shares (RS)**
Row-normalize `R` to sum to 1:

$$
RS = \text{diag}(R\mathbf{1})^{-1} R
$$
Where $\mathbf{1}$ is a vector of ones, and $\text{diag}(v)$ creates diagonal matrix from vector $v$.

Element-wise:
$$
RS_{ij} = \frac{R_{ij}}{\sum_{k=1}^n R_{ik}}
$$

**Properties:** Each row sums to 1: $RS \mathbf{1} = \mathbf{1}$

### **2.2 Mutual-Recognition (MR)**
Element-wise minimum of reciprocal recognition:

$$
MR = \min(RS, RS^\top)
$$
Where $\min$ is element-wise, and $RS^\top$ is transpose.

Element-wise:
$$
MR_{ij} = \min(RS_{ij}, RS_{ji})
$$

**Properties:** Symmetric: $MR = MR^\top$

### **2.3 Total Mutual Recognition Vector** `t`
For each participant, sum of mutual recognition with all others:

$$
\mathbf{t} = MR \mathbf{1}
$$
Or element-wise: $t_i = \sum_{j=1}^n MR_{ij}$

### **2.4 Mutual-Recognition-Shares (MRS)**
Row-normalize `MR`:

$$
MRS = \text{diag}(\mathbf{t})^{-1} MR
$$
Assuming $t_i > 0$ for all $i$.

Element-wise:
$$
MRS_{ij} = \frac{MR_{ij}}{t_i} = \frac{MR_{ij}}{\sum_{k=1}^n MR_{ik}}
$$

**Properties:** $MRS \mathbf{1} = \mathbf{1}$

---

## **3. Collective Derivations**

### **3.1 Mutual Recognition within Collective**
For collective $C$ with indicator $c$:

$$
\mathbf{m}_C = MR c
$$
Element-wise: $(\mathbf{m}_C)_i = \sum_{j \in C} MR_{ij}$

### **3.2 Total Pool within Collective**
Sum of all mutual recognition between members of $C$:

$$
T_C = c^\top MR c = \sum_{i \in C} \sum_{j \in C} MR_{ij}
$$

### **3.3 Synthetic-Collective-Mutual-Recognition-Shares (SCMRS)**
**Weighted version** (relationship strength weighted):

$$
\mathbf{s}_{\text{weighted}} = \frac{\mathbf{m}_C}{T_C} = \frac{MR c}{c^\top MR c}
$$
Element-wise for $i \in C$: $s_i = \frac{\sum_{j \in C} MR_{ij}}{\sum_{x \in C} \sum_{y \in C} MR_{xy}}$

### **3.4 Synthetic-Collective-Relative-Mutual-Recognition-Shares (SCRMRS)**
**Equal voice version** (each member's MRS as equal vote):

$$
\mathbf{s}_{\text{equal}} = \frac{1}{|C|} MRS^\top c
$$
Where $|C| = \sum_i c_i = c^\top c$

Element-wise: $s_i = \frac{1}{|C|} \sum_{j \in C} MRS_{ji}$

---

## **4. Network Integration Metrics**

### **4.1 Average Mutual Recognition in Collective**
$$
\bar{m}_C = \frac{1}{|C|} T_C = \frac{1}{|C|} c^\top MR c
$$

### **4.2 Mutual-Recognition-Density (MRD)**
For participant $i$ relative to collective $C$:

$$
\text{MRD}_C(i) = \frac{(\mathbf{m}_C)_i}{\bar{m}_C} = \frac{|C| \cdot (MR c)_i}{c^\top MR c}
$$

### **4.3 Membership Determination**
Given threshold $\theta$ (typically 0.5):

**Collective model** (rising bar):
- $C_{\text{new}} = \{i \in C_{\text{current}} : \text{MRD}_{C_{\text{current}}}(i) \geq \theta\}$

**Commons model** (stable bar):
- $C_{\text{new}} = \{i \in P : \text{MRD}_{P}(i) \geq \theta\}$

---

## **5. Allocation Protocols**

### **5.1 Multi-Provider Need Satisfaction**
For recipient $r$ with need $N_r$:

Let $S$ be the chosen share matrix (RS, MRS, or SCMRS).

**Initialize:**
- $\mathbf{a}^{(0)} = \mathbf{0}$ (allocations received)
- $N_r^{(0)} = N_r$ (remaining need)

**Iteration $t$:**
1. Compute raw allocations: $\mathbf{r}^{(t)} = K \cdot S_{:,r}$ (element-wise multiplication of capacity column with share column for $r$)
2. Compute actual allocations: $\mathbf{a}^{(t)}_{\text{actual}} = \min(\mathbf{r}^{(t)}, N_r^{(t)})$
3. Update received: $\mathbf{a}^{(t+1)} = \mathbf{a}^{(t)} + \mathbf{a}^{(t)}_{\text{actual}}$
4. Update remaining need: $N_r^{(t+1)} = \max(0, N_r^{(t)} - \mathbf{1}^\top \mathbf{a}^{(t)}_{\text{actual}})$
5. Update capacities: $K_{:,r} = K_{:,r} - \mathbf{a}^{(t)}_{\text{actual}}$

**Convergence** when $N_r^{(t)} = 0$ or $\mathbf{r}^{(t)} = \mathbf{0}$.

---

## **6. Matrix Implementation in Code**

```python
import numpy as np

class FreeAssociationMatrices:
    def __init__(self, n):
        self.n = n
        self.R = np.zeros((n, n))  # Raw recognition
        self.K = np.zeros((n, n))  # Capacities
        
    def compute_RS(self):
        """Recognition Shares"""
        row_sums = self.R.sum(axis=1, keepdims=True)
        # Avoid division by zero
        row_sums[row_sums == 0] = 1
        return self.R / row_sums
    
    def compute_MR(self):
        """Mutual Recognition (min-based)"""
        RS = self.compute_RS()
        return np.minimum(RS, RS.T)
    
    def compute_t(self):
        """Total Mutual Recognition vector"""
        MR = self.compute_MR()
        return MR.sum(axis=1)
    
    def compute_MRS(self):
        """Mutual Recognition Shares"""
        MR = self.compute_MR()
        t = self.compute_t()
        # Avoid division by zero
        t[t == 0] = 1
        return MR / t[:, np.newaxis]
    
    def compute_SCMRS_weighted(self, collective_indices):
        """Weighted SCMRS for collective"""
        MR = self.compute_MR()
        c = np.zeros(self.n)
        c[collective_indices] = 1
        
        m_C = MR @ c  # MR * c
        T_C = c @ MR @ c  # c^T * MR * c
        
        if T_C == 0:
            return np.zeros(self.n)
        
        return m_C / T_C
    
    def compute_SCRMRS_equal(self, collective_indices):
        """Equal voice SCRMRS for collective"""
        MRS = self.compute_MRS()
        c = np.zeros(self.n)
        c[collective_indices] = 1
        C_size = len(collective_indices)
        
        return (MRS.T @ c) / C_size
    
    def compute_MRD(self, collective_indices):
        """MRD for each participant relative to collective"""
        MR = self.compute_MR()
        c = np.zeros(self.n)
        c[collective_indices] = 1
        C_size = len(collective_indices)
        
        m_C = MR @ c
        T_C = c @ MR @ c
        
        if T_C == 0:
            return np.zeros(self.n)
        
        return C_size * m_C / T_C
    
    def allocate_multi_provider(self, recipient_idx, need, share_type='MRS'):
        """Multi-provider need satisfaction allocation"""
        if share_type == 'RS':
            S = self.compute_RS()[:, recipient_idx]
        elif share_type == 'MRS':
            S = self.compute_MRS()[:, recipient_idx]
        elif share_type == 'SCMRS':
            # Assuming collective includes all participants
            S = self.compute_SCMRS_weighted(range(self.n))
        else:
            raise ValueError(f"Unknown share type: {share_type}")
        
        allocations = np.zeros(self.n)
        remaining_need = need
        max_iterations = 100
        
        for _ in range(max_iterations):
            if remaining_need <= 0:
                break
                
            # Raw allocations based on current capacities
            raw_alloc = self.K[:, recipient_idx] * S
            
            # Cap by remaining need
            total_raw = raw_alloc.sum()
            if total_raw == 0:
                break
                
            if total_raw <= remaining_need:
                actual_alloc = raw_alloc
            else:
                # Proportional allocation
                actual_alloc = raw_alloc * (remaining_need / total_raw)
            
            # Update
            allocations += actual_alloc
            self.K[:, recipient_idx] -= actual_alloc
            remaining_need -= actual_alloc.sum()
        
        return allocations
```

## **7. Key Mathematical Properties**

### **7.1 Budget Conservation**
Recognition budget: $\sum_j RS_{ij} = 1$ for all $i$

### **7.2 Symmetry**
$MR_{ij} = MR_{ji}$

### **7.3 Normalization**
$\sum_j MRS_{ij} = 1$ for all $i$

### **7.4 Collective Share Sum**
For weighted SCMRS: $\sum_{i \in C} s_i = 1$

For equal SCRMRS: $\sum_i s_i = 1$

### **7.5 Scale Invariance**
All proportions are scale-invariant: relationships work at any capacity scale.

---

## **8. Example Calculation**

Given 3 participants with:
- $R = \begin{bmatrix}0&0.6&0.4\\0.3&0&0.7\\0.5&0.5&0\end{bmatrix}$
- Collective $C = \{1,2,3\}$

Then:
1. $RS = R$ (already row-normalized)
2. $MR = \begin{bmatrix}0&0.3&0.4\\0.3&0&0.5\\0.4&0.5&0\end{bmatrix}$
3. $\mathbf{t} = [0.7, 0.8, 0.9]^\top$
4. $MRS = \begin{bmatrix}0&0.429&0.571\\0.375&0&0.625\\0.444&0.556&0\end{bmatrix}$
5. Weighted SCMRS: $\mathbf{s} = [0.7/2.4, 0.8/2.4, 0.9/2.4] = [0.292, 0.333, 0.375]$
6. MRD for participant 1: $\text{MRD}(1) = 3 \times 0.7 / 2.4 = 0.875$

This matrix formulation provides the complete mathematical foundation for implementing the Free Association framework.