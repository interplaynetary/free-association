# Generic Distributed Reactive Architecture
*A Pattern Language for Decentralized Coordination*

This guide outlines the architectural principles for building systems where autonomous agents coordinate to reach shared states without central authority.

---

## 1. Core Philosophy

### 1.1 Sovereign State
There is no "Global Database". There are only:
1.  **Local Declarations**: What an *Agent* signs.
    - **Reflexive**: Claims about self (Needs, Capacities).
    - **Relational**: Claims about others (Verifiable Credentials, Endorsements, Recognition).
2.  **Network Shadows**: What an *Agent* sees of others (Cached copies of peer states).

The Agent is the definitive source of truth for their own signed data. Agents never write to another peer's state; they only react to it.

### 1.2 Subjective Relativity
Every agent sees a slightly different version of the network.
- **Local View**: Defined by who the agent trusts and is connected to.
- **Peer View**: Defined by the peer's relationships.
- **Consensus**: Not "everyone seeing the exact same bits", but "all overlapping views being compatible."

### 1.3 Reactive Derivation
The system logic is a **Pure Function**:
$$ \text{Local State} = f(\text{Local Inputs}, \text{Network Shadows}) $$

When the network changes, local state automatically re-derives. This guarantees that if the network stabilizes, all agents will converge to consistent states.

---

## 2. The Coordination Cycle

The system operates in a continuous loop of **Publishing**, **Listening**, and **Refining**.

### Phase 1: Publishing (Async)
*Action*: Agents broadcast their sovereign state.
*Principle*: **"Shoot and Forget"**. State is pushed to the network layer without waiting for global ACKs.
*Data*: Identity, capabilities, needs, priorities.

### Phase 2: Listening (Sync/Async)
*Action*: Agents collect updates from relevant peers/paths.

### Phase 3: Refining (Sync)
*Action*: Local algorithms process the inputs.
*Principle*: **Deterministic Logic**. Given the same set of cached peers, the logic must always produce the same result. This ensures debuggability and stability.
*Output*: A new "Derived State" (e.g., specific allocations, decisions).

---

## 3. Network Topology: libp2p Pubsub

### 3.1 Topic Structure
Data is published to **topic-based channels** following a strict naming convention:

```
topic = {pubkey}/{path}
```

Where:
- `{pubkey}`: The DID public key of the signing agent (from Verifiable Credential)
- `{path}`: Hierarchical path to the data (e.g., `resources/needs`, `recognition/tree`)

**Example Topics:**
```
did:key:z6Mk.../resources/capacities
did:key:z6Mk.../recognition/weights
did:key:z6Mk.../allocation/state
```

### 3.2 Cache Replication Rules
Agents selectively cache network data based on **topic prefix validation**:

**Rule**: Only cache data where `topic_prefix == signing_pubkey`

This ensures:
- **Authenticity**: Topic namespace is owned by the signer
- **Spam Prevention**: Cannot publish to another agent's namespace
- **Selective Sync**: Only subscribe to relevant agent paths

### 3.3 Conflict Resolution
When multiple updates arrive for the same `(author, path)`, the system uses **strictly increasing counters**:

```typescript
type Update = {
  author: PublicKey,
  path: string,
  counter: number,  // Strictly increasing per (author, path)
  data: SignedVC,
  timestamp: number
}
```

**Resolution Logic:**
1. If `incoming.counter > cached.counter` → Accept update
2. If `incoming.counter ≤ cached.counter` → Reject (stale)
3. Counter must increment for each update to the same path

This provides:
- **Deterministic ordering** without clock synchronization
- **Idempotent delivery** (duplicate messages ignored)
- **Causal consistency** within an agent's own updates

---

## 4. Implementation Pattern: The Store Matrix

A robust implementation organizes state into a clear matrix:

| Layer | Source | Persistence | Example |
| :--- | :--- | :--- | :--- |
| **1. Source** | User / Local Device | Durable (Disk) | `local_profile`, `local_needs` |
| **2. Network** | P2P Gossip | Cache (Temp) | `peer_profiles` |
| **3. Derived** | $f(\text{Source}, \text{Network})$ | Ephemeral (RAM) | `allocations`, `search_results` |

**The Golden Rule**: Never persist Derived data as the source of truth. Always re-derive it.

---

## 5. Implementation Pattern: The Sync/Async Matrix

To prevent race conditions and stale reads, map every protocol phase to a **Sync/Async Matrix**. This defines exactly what data must be present before a computation runs.

| Phase | Operation Type | Data Subset Required | Source | Freshness Rule |
| :--- | :--- | :--- | :--- | :--- |
| **1. Publish** | **Local Sync** | My Sovereign State | Self | Current |
| **2. Broadcast** | **Async Push** | My State $\to$ Network | Self | Current |
| **3. Gather** | **Async Pull** | Peer States | Network | $< 60s$ (or Threshold) |
| **4. Compute** | **Local Sync** | Aggregated Inputs | Derived | Current |

**Why this matters:**
- **Race Prevention**: Never run a Local Sync computation (Phase 4) until the Async Pull (Phase 3) is complete.
- **Optimization**: Don't request the whole dataset (Phase 3) if you only need a subset for the current Logic Unit.
- **Stale Protection**: Explicitly define "Freshness" (e.g., $< 60s$) to reject outdated network shadows.

---