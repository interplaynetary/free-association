# Generic Distributed Reactive Networked Protocol Architecture - Design Guide

This guide outlines a **general-purpose architecture pattern** for building decentralized, peer-to-peer systems with reactive state management, eventual consistency, and algorithm-driven data coordination.

Applicable to: resource allocation, collaborative editing, distributed computation, decentralized marketplaces, federated social networks, and any system requiring coordinated state across autonomous peers.

**Reference Stack**: P2P sync layer (Holster, IPFS, libp2p), Reactive framework (Svelte 5, Solid, Vue), Schema validation (Zod, Yup, io-ts), Type-safe language (TypeScript, ReScript).

---

## Architecture Overview

A distributed reactive protocol implements:

1. **Local state declarations** that participants broadcast to the network
2. **Relationship-based priority** determining how peers interact
3. **Algorithm-driven computation** that processes network state into derived results
4. **Causal consistency** coordinating operations across decentralized nodes
5. **Convergence mechanisms** ensuring the system reaches equilibrium

---

## Sync/Async Matrix Pattern

When designing distributed systems, create a **matrix** showing:
- **Columns**: Sync vs Async operations
- **Rows**: Protocol phases/rounds
- **Cells**: Which subset of network data is needed at each point

This prevents common bugs:
- Race conditions (async operation expects sync result)
- Stale reads (using outdated cached data)
- Over-fetching (requesting full dataset when subset suffices)
- Under-coordination (missing causal dependencies)

### Example Matrix Structure

| Phase | Sync/Async | Data Subset Required | Source Nodes | Freshness |
|-------|------------|---------------------|--------------|-----------|
| Phase 1 | Local Sync | My local state only | Self | Current |
| Phase 2 | Async Broadcast | My state → all peers | Self | Current |
| Phase 3 | Async Gather | Specific peers' state | Relationship-filtered | <60s |
| Phase 4 | Local Sync | Aggregated peer data | Derived | Current |
| Phase 5 | Async Coordination | Consensus signals | Active peers | <30s |

---

## Store Architecture (Generic Pattern)

### Raw Data Stores (Network-Backed)

| Store | Type | Source | Updates | Network Path |
|-------|------|--------|---------|--------------|
| `myLocalState` | NetworkStore | My local declarations | When I change state | `/{protocol}/state` |
| `myComputedResults` | NetworkStore | My local computation | After processing | `/{protocol}/results` |
| `myRelationships` | NetworkStore | My peer preferences | When relationships change | `/{protocol}/relationships` |
| `myCoordinationState` | NetworkStore | My protocol position | Phase transitions | `/{protocol}/coordination` |
| `networkStates` | Map<peerId, State> | Network subscriptions | Real-time from peers | `~{peerId}/{protocol}/state` |
| `networkResults` | Map<peerId, Results> | Network subscriptions | Real-time from peers | `~{peerId}/{protocol}/results` |
| `networkRelationships` | Map<peerId, Relationships> | Network subscriptions | Real-time from peers | `~{peerId}/{protocol}/relationships` |
| `networkCoordination` | Map<peerId, Coordination> | Network subscriptions | Real-time from peers | `~{peerId}/{protocol}/coordination` |

### Derived Subgroups (Algorithm-Driven)

| Subgroup | Derived From | Purpose | Data Needed | Subscription Type |
|----------|--------------|---------|-------------|-------------------|
| `myPrimaryPeers` | `myRelationships` + `networkRelationships` | Bidirectional relationships → priority processing | Full relationship data | Full (bidirectional check) |
| `myPrimaryBeneficiaries` | `myPrimaryPeers` | Peers with mutual relationship → primary computation tier | Their state declarations | State + Results + Relationships + Coordination |
| `mySecondaryBeneficiaries` | `myRelationships` - `myPrimaryPeers` | One-way relationships → secondary tier | Their state declarations | State only |
| `primarySourcesForMe` | `myPrimaryPeers` + `networkStates` | Primary peers with relevant state → priority inputs | Their state, results | State + Results + Relationships + Coordination |
| `secondarySourcesForMe` | `networkStates` - `myPrimaryPeers` | Secondary peers with relevant state → fallback inputs | Their state, results | State + Results |
| `primaryPeersWithNeeds` | `myPrimaryBeneficiaries` + `networkStates` | Primary peers requiring processing → tier 1 computation | Their state (filtered) | State (filtered) |
| `secondaryPeersWithNeeds` | `mySecondaryBeneficiaries` + `networkStates` | Secondary peers requiring processing → tier 2 computation | Their state (filtered) | State (filtered) |
| `allPrimaryPartners` | `myPrimaryBeneficiaries` ∪ `primarySourcesForMe` | Bidirectional relationships → full data exchange | Everything | Full peer subscription |
| `activeParticipants` | `networkStates` + timestamp check | Fresh state (< threshold) → currently active | All state data | State (freshness filtered) |
| `unstableParticipants` | `networkStates` + stability metric | Participants with convergence issues | Stability metrics | State (stability filtered) |
| `hasSystemConverged` | `myComputedResults` + previous results | Results stable (Δ < ε) → equilibrium reached | Current + previous results | N/A (local computation) |

---

## Continuous Async Operations (Decentralized Coordination)

| Operation | Sync/Async | Data Source | Trigger | Purpose | Debounce |
|-----------|------------|-------------|---------|---------|----------|
| **State Publishing** | Async | `myLocalState` | User changes local state | Broadcast my state to network | 100ms |
| **Relationship Publishing** | Async | `myRelationships` | User updates peer preferences | Update relationship values with others | 200ms |
| **Algorithm-Driven Subscriptions** | Async | Derived subgroups | `allPrimaryPartners`, `mySecondaryBeneficiaries` change | Maintain data connections based on relationships | 100ms |
| **Relationship Computation** | Async (derived) | `myRelationships` + `networkRelationships` | When either my preferences or their preferences change | Compute bidirectional relationship metrics | Immediate (derived) |
| **Compatibility Matching** | Async (derived) | `networkStates` | When state changes | Find compatible peers for processing | Per computation |
| **Result Computation** | Async | `myLocalState` + `networkStates` + relationship metrics | When state or relationships change | Compute algorithm-specific results | On-demand |
| **Result Publishing** | Async | `myComputedResults` | After computation completes | Broadcast results to relevant peers | 100ms |
| **Convergence Detection** | Async (derived) | `myComputedResults` + previous results | After result update | Check if results stable (Δ < ε) | Immediate (derived) |
| **Coordination Gossip** | Async (periodic) | `myCoordinationState` | Every N seconds | Coordinate protocol phases via gossip | Configurable (periodic) |
| **Phase Advancement Check** | Async | `networkCoordination` + `activeParticipants` | When peer coordination received | Advance if threshold of peers ahead | Per peer update |

---

## Phase-Synchronized Operations (Causal Coordination)

**Note**: This pattern uses **decentralized causal clock coordination** instead of centralized phase announcements. Each participant advances independently when threshold of active peers are ahead.

### Phase Timing Breakdown (Generic)

| Phase | Duration | Sync Type | Critical Data | Purpose | Coordination |
|-------|----------|-----------|---------------|---------|--------------|
| **0. Causal Clock Increment** | Instant | Local | My current causal clock | Increment my counter before state change | Local |
| **1. State Update** | 0-T₁ | Async | Local state + causal metadata | Publish my updated state with causal ordering | P2P Network |
| **2. Peer Clock Merge** | T₁-T₂ | Async | Peers' clock values | Merge received clocks, update my clock | Automatic (on receive) |
| **3. Compatibility Matrix** | T₂-T₃ | **SYNC (Local)** | All active peers' state | Pre-compute which peers can interact | Optimization layer |
| **4. Multi-Tier Computation** | T₃-T₄ | **SYNC (Local)** | Relationship metrics, compatible peers, stability factors | Primary tier (bidirectional), Secondary tier (one-way) | Per-peer computation |
| **5. Result Publishing** | T₄-T₅ | Async | Computed results + causal metadata | Broadcast my results to relevant peers | P2P Network |
| **6. Aggregation Window** | T₅-T₆ | Async with timeout | All sources' results where target = me | Collect total inputs across all sources | Subscribe to source results |
| **7. Stability Update** | T₆-T₇ | **SYNC (Local)** | My state + received results → delta | Update stability history, recompute factors | Local computation |
| **8. Phase Advancement Check** | T₇-T₈ | Async | Peers' phase numbers from coordination | If threshold ahead, advance my phase and increment clock | Gossip-based consensus |
| **9. Convergence Check** | T₈-T₉ | Async (derived) | My results (current + previous) | Check if max(Δ) < ε across all metrics | Local + Network sampling |

### Causal Clock Rules (Vector Clock Pattern)

| Rule | Scenario | Action | Data Required |
|------|----------|--------|---------------|
| **Happened-Before** | peer.Clock[peer] > my.Clock[peer] | Accept update (causally later) | Peer's causal clock |
| **Concurrent** | Neither clock dominates | Accept both (merge clocks) | Both causal clocks |
| **Stale Update** | peer.Clock[peer] ≤ my.Clock[peer] | Ignore (already processed) | Peer's causal clock |
| **Clock Merge** | Receive peer's clock | my.Clock = max(my.Clock, peer.Clock) for all entries | Full clock from peer |
| **Self Increment** | Before publishing state | my.Clock[me]++ | Own counter |

---

## Data Subset Requirements Per Protocol Phase

### Source's Perspective (Computing Results)

| Phase | Data Subset | Schema Type | Freshness | Source |
|-------|-------------|------------|-----------|--------|
| **Compatibility Matching** | All peers' state declarations | `StateSchema` | < threshold | `networkStates` (filtered by freshness) |
| **Relationship Computation** | Primary peers' relationship data | `RelationshipSchema` | < threshold | `myRelationships` + `networkRelationships` |
| **Tier 1 Computation** | Primary peers' state + stability | `StateSchema` | < threshold | `primaryPeersWithNeeds` |
| **Tier 2 Computation** | Secondary peers' state + stability | `StateSchema` | < threshold | `secondaryPeersWithNeeds` |
| **Boundary Check** | Each peer's state constraints | `StateSchema` | < threshold | `networkStates` |

### Target's Perspective (Receiving Results)

| Phase | Data Subset | Schema Type | Freshness | Source |
|-------|-------------|-------------|-----------|--------|
| **Expected Results** | Sources' results + my position in them | `ResultSchema` | < threshold | `networkResults` |
| **Total Received** | All sources' results where target = me | `ResultSchema` | < threshold | `networkResults` (aggregated) |
| **Stability Update** | My declared state + total received | `StateSchema` | Current | `myLocalState` + aggregated results |
| **Convergence Check** | My results (current phase + previous phase) | `ResultSchema` | Current + Previous | `myComputedResults` + tracked history |

### Coordination Perspective (Phase Advancement)

| Phase | Data Subset | Schema Type | Freshness | Source |
|-------|-------------|-------------|-----------|--------|
| **Causal Clock Merge** | All active peers' clocks | `CausalClockSchema` | < threshold | `networkCoordination` |
| **Phase Consensus** | All active peers' phase number | `CoordinationSchema` | < threshold | `networkCoordination` |
| **Advancement Decision** | Count of peers with phase > my_phase | `number` | < threshold | Derived from `networkCoordination` |

---

## Layer 1: Schemas (Data Validation)

**File**: `schemas.ts`

All data structures defined using **schema validation** for runtime validation and static type inference. This ensures data integrity at network boundaries.

### Generic Schema Categories:

- **`StateSchema`** - A participant's local state declaration
- **`ResultSchema`** - A participant's computed results from processing network state
- **`RelationshipSchema`** - A participant's preference weights for other peers
- **`CoordinationSchema`** - Decentralized phase coordination metadata
- **`CausalClockSchema`** - Causal consistency tracking (maps peerId → logical timestamp)

### Design Principle:

Schemas are the **single source of truth** for types. All language types are inferred from schemas.

```typescript
// TypeScript example
export type State = z.infer<typeof StateSchema>;
export type Results = z.infer<typeof ResultSchema>;

// ReScript example
type state = Schema.t<stateSchema>
type results = Schema.t<resultSchema>
```

---

## Layer 2: Store Factory (Generic Persistence)

**File**: `store.ts`

Provides a **generic factory** for creating network-backed stores with any schema.

### Features:

- **Schema validation** - All network data validated before updating store
- **Timestamp conflict resolution** - Network wins if newer
- **Local caching** - Instant UI on initialization
- **Debounced persistence** - Configurable debounce to batch updates
- **Queue management** - Handles simultaneous network/local updates
- **Cross-peer subscriptions** - Subscribe to any participant's data

### API:

```typescript
const myStore = createStore({
  networkPath: '/{protocol}/state',
  schema: StateSchema,
  cacheable: true,
  persistDebounce: 100
});

// Initialize (subscribes to network)
myStore.initialize();

// Update local data (triggers persistence)
myStore.set(newState);

// Subscribe to changes
myStore.subscribe(data => console.log(data));

// Subscribe to another peer
myStore.subscribeToUser(theirPeerId, data => console.log(data));
```

---

## Layer 3: Protocol Stores (Typed Persistence)

**File**: `stores.ts`

Uses the generic store factory to create **typed stores** for protocol data.

### My Data Stores (Published to Network):

- **`myLocalState`** - My state declarations (what I contribute to protocol)
- **`myComputedResults`** - My computed results (if I'm a processor)
- **`myRelationships`** - My preference weights for other peers
- **`myCoordinationState`** - My current phase state (for coordination)

### Network Data Maps (Subscribed from Network):

- **`networkStates`** - Map<peerId, State> from all participants
- **`networkResults`** - Map<peerId, Results> from processors
- **`networkRelationships`** - Map<peerId, Relationships> for metric computation
- **`networkCoordination`** - Map<peerId, Coordination> for phase coordination

### Subscription Management:

Provides functions to subscribe to specific participants:

- `subscribeToState(peerId)` - Get their state declarations
- `subscribeToResults(peerId)` - Get their computed results
- `subscribeToRelationships(peerId)` - Get their preference weights
- `subscribeToCoordination(peerId)` - Get their phase state
- `subscribeToFullPeer(peerId)` - Subscribe to all data from a primary partner

---

## Layer 4: Algorithm (Protocol Logic)

**File**: `algorithm.ts`

Implements the **core protocol algorithm** with relationship-based processing.

### Derived Stores (Reactive Subgroups):

All subgroups are **derived from base stores** and auto-update:

- **`myPrimaryRelationships`** - My bidirectional relationship metrics with all peers
- **`myPrimaryBeneficiaries`** - Peers with bidirectional relationship (priority processing)
- **`mySecondaryBeneficiaries`** - Peers with one-way relationship (secondary processing)
- **`primarySourcesForMe`** - Sources with bidirectional relationship and relevant state
- **`secondarySourcesForMe`** - Sources with one-way relationship
- **`primaryPeersWithNeeds`** - Primary peers requiring processing
- **`activeParticipants`** - Participants with fresh state (< threshold)
- **`unstableParticipants`** - Participants with stability issues
- **`hasSystemConverged`** - True when results stabilized (< ε change)

### Algorithm-Driven Subscriptions:

The algorithm **automatically manages subscriptions** based on subgroups:

- Primary partners → Subscribe to full data (state, results, relationships, coordination)
- Secondary beneficiaries → Subscribe to state only
- Secondary sources → Subscribe to state + results
- Active participants → Subscribe to coordination (for phase advancement)

**Initialization**:

```typescript
import { initializeAlgorithmSubscriptions } from './algorithm';

// After network authentication + store initialization
initializeAlgorithmSubscriptions();
```

### Core Algorithm Functions:

**`computeResults()`** - Multi-tier protocol processing:

1. For each processing unit:
   - Find compatible targets (compatibility checking)
   - **Tier 1**: Process for primary relationships (bidirectional, priority)
   - **Tier 2**: Process for secondary relationships (one-way, fallback)
   - Apply boundary constraints (contractiveness)
2. Return unit-level metrics, results, and target totals

**Performance Optimizations**:
- Bucketing (reduce compatibility checks from O(N×U) to O(N×U_bucket))
- Pre-computed compatibility matrix (avoid redundant checks)
- Active set tracking (filter out low-priority targets early)
- Early exit conditions (skip units with no compatible/related targets)

**`computeRelationshipMetrics()`** - Bidirectional relationship calculation:

```
Metric(Me, Them) = f(MyPreferenceFor[Them], TheirPreferenceFor[Me])
```

**`updateStateStability()`** - Adaptive stability tracking:

- Tracks change history (last N phases)
- Detects oscillation patterns
- Adjusts stability factors based on convergence behavior
- `ActiveState = DeclaredState × StabilityFactor`

**`publishMyState()`** - Enhanced publishing:

- Increments causal clock (causal ordering)
- Adds relationship metrics, preference weights
- Persists to network (auto-validated by store)

### Causal Clock Coordination:

- **`incrementMyCausalClock()`** - Call before publishing state changes
- **`updateCausalClockFromPeer()`** - Merge peer's clock with mine
- **`advanceToNextPhase()`** - Move to next phase when threshold of peers ahead
- **`publishMyCoordinationState()`** - Periodic gossip for coordination

---

## Layer 5: Compatibility Logic (Matching Functions)

**File**: `match.ts`

Pure functions for checking peer/state compatibility.

### Generic Compatibility Checks:

**`constraintsCompatible(state1, state2)`** - Returns true if constraints match
- Protocol-specific constraint checking
- Optimistic: returns true if constraint info missing

**`metadataCompatible(state1, state2)`** - Returns true if metadata matches
- Checks protocol-specific metadata requirements
- Optimistic: returns true if metadata missing

**`statesCompatible(targetState, sourceState)`** - Combined check
- Must pass ALL compatibility requirements

### State Matching:

**`matchTargetToSources(target, sources, maxAmount)`** - Match algorithm

Returns:
- `compatible_pairs` - Array of (targetState, sourceState, matchableAmount)
- `total_matchable` - Total amount that can be processed
- `unmatched_targets` - Target states with no compatible sources
- `unmatched_sources` - Source states with no compatible targets

---

## Layer 6: Visualization (UI Component)

**File**: `Visualization.{svelte|tsx}`

Framework component for visualizing protocol state, subgroups, and network health.

### Key Visualization Elements:

- **State Overview** - My current state vs network state
- **Relationship Graph** - Primary/secondary peer relationships
- **Result Flow** - Computed results and their distribution
- **Convergence Metrics** - System stability indicators
- **Phase Coordination** - Current phase across active participants

---

## Reactive Data Flow (Generic Pattern)

```
┌─────────────────────────────────────────────────────────────┐
│ 1. USER ACTIONS                                             │
│    - Declare local state                                    │
│    - Set peer preferences                                   │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 2. LOCAL STORES (myLocalState, myRelationships)            │
│    - Validate with schema                                   │
│    - Update reactive store                                  │
│    - Persist to network (debounced)                         │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 3. NETWORK SYNC (P2P Layer)                                │
│    - Broadcast to peers                                     │
│    - Receive updates from peers                             │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 4. NETWORK STORES (networkStates, networkRelationships)    │
│    - Validate with schema                                   │
│    - Update Map<peerId, data>                               │
│    - Trigger derived store recomputation                    │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 5. DERIVED STORES (relationships, subgroups)               │
│    - Reactively compute from base stores                    │
│    - Auto-update UI when dependencies change                │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 6. ALGORITHM (computeResults)                               │
│    - Triggered by state changes                             │
│    - Compute multi-tier protocol results                    │
│    - Apply adaptive stability factors                       │
│    - Apply boundary constraints                             │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 7. PUBLISH RESULTS                                          │
│    - Update myComputedResults                               │
│    - Persist to network                                     │
│    - Broadcast to relevant peers                            │
└─────────────────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 8. TARGETS RECEIVE RESULTS                                  │
│    - Subscribe to source's results                          │
│    - See expected processing (unit-level)                   │
│    - Update own state based on received amounts             │
└─────────────────────────────────────────────────────────────┘
```

---

## Implementation Checklist (Generic)

### Step 1: Define Schemas
- [ ] Create validation schemas for all data types
- [ ] Export inferred language types
- [ ] Add validation helpers (`parseState`, `parseResults`, etc.)

### Step 2: Set Up Stores
- [ ] Initialize my data stores (state, results, relationships, coordination)
- [ ] Initialize network data maps (states, results, relationships, coordination)
- [ ] Call `initializeProtocolStores()` after network authentication

### Step 3: Set Up Subscriptions
- [ ] Call `initializeAlgorithmSubscriptions()` to enable reactive subscription management
- [ ] Algorithm automatically subscribes based on subgroups

### Step 4: Implement UI
- [ ] Subscribe to derived stores in components
- [ ] Update local stores on user actions
- [ ] Display results from `myComputedResults` and `networkResults`

### Step 5: Handle Lifecycle
- [ ] Initialize on authentication: `initializeProtocolStores()` → `initializeAlgorithmSubscriptions()`
- [ ] Cleanup on logout: `cleanupProtocolStores()` → `cleanupAlgorithmSubscriptions()`

---

## Key Design Principles (Universal)

1. **Schema-First**: All types inferred from validation schemas (single source of truth)
2. **Validation at Boundaries**: Validate all network data before updating stores
3. **Reactive Architecture**: Use framework-native derived stores for automatic recomputation
4. **Unit-Native Processing**: Process each unit independently (not aggregate)
5. **Multi-Tier Priority**: Bidirectional relationships get priority, one-way gets fallback
6. **Adaptive Stability**: Prevent oscillations with history-based stability factors
7. **Contractiveness**: Apply boundary constraints (ensures convergence)
8. **Causal Consistency**: Use causal clocks for decentralized phase coordination
9. **Optimistic Matching**: Assume compatibility when info missing (better UX)
10. **Algorithm-Driven Subscriptions**: Automatically subscribe based on subgroup membership

---

## Convergence Mechanisms (Generic Pattern)

### Contractiveness
Every operation must respect declared boundaries:
```
ProcessedAmount[target] ≤ DeclaredCapacity[target]
```

This ensures the system cannot grow unbounded and must converge.

### Adaptive Damping
Track change history to detect and dampen oscillations:

```typescript
function computeStabilityFactor(history: number[]): number {
  if (detectsOscillation(history)) return LOW_STABILITY;
  if (detectsModerateChange(history)) return MEDIUM_STABILITY;
  return HIGH_STABILITY;
}
```

### Denominator Stability
Convergence detected when key metrics change by less than epsilon:

```typescript
function hasConverged(current: Metrics, previous: Metrics, epsilon: number): boolean {
  return Object.keys(current).every(key => 
    Math.abs(current[key] - previous[key]) < epsilon
  );
}
```

---

## Network Topology Patterns

### Star Topology
- Central coordinator publishes authoritative state
- Peers subscribe to coordinator only
- Use when: Strong consistency required, coordinator highly available

### Mesh Topology
- All peers subscribe to all peers
- Full network visibility
- Use when: Small network (< 50 peers), complete information needed

### Relationship-Based Topology (Recommended)
- Peers subscribe based on relationship strength
- Primary relationships → full data sync
- Secondary relationships → partial sync
- Use when: Scalable to large networks, natural relationship structure

### Gossip Topology
- Peers subscribe to random subset
- Periodic re-sampling for gossip
- Use when: Very large networks, eventual consistency acceptable

---

## Debugging Tools (Generic)

Window-exposed functions (browser console):

```javascript
// Log protocol state
window.debugProtocol();

// Manually compute results
window.computeResults(sourcePeerId, state, relationships, weights, peerStates);

// Test stability
window.computeStabilityFactor([10, 5, 8]); // Returns stability metric
window.updateStateStability(state, receivedResults);

// Inspect causal clocks
window.inspectCausalClocks();

// Force phase advancement
window.advanceToNextPhase();
```

---

## Example Protocols Implementable with This Pattern

1. **Resource Allocation** - Distribute capacity based on mutual need recognition
2. **Collaborative Editing** - CRDT-like document merging with author relationships
3. **Federated Social Networks** - Content distribution based on follow graphs
4. **Distributed Computation** - Work distribution based on peer capabilities
5. **Decentralized Marketplaces** - Order matching based on reputation
6. **P2P File Sharing** - Bandwidth allocation based on contribution history
7. **Federated Learning** - Model weight aggregation from trusted peers
8. **Decentralized Governance** - Voting weight based on participation history

---

## Performance Characteristics

### Time Complexity
- **State Publishing**: O(1) per peer
- **Relationship Computation**: O(N) where N = number of peers
- **Compatibility Matching**: O(N × U) where U = units, optimized to O(N × U/B) with bucketing
- **Result Computation**: O(N × U × R) where R = relationships per peer
- **Convergence Detection**: O(M) where M = number of metrics tracked

### Space Complexity
- **Local Stores**: O(1) per store
- **Network Maps**: O(N) where N = subscribed peers
- **Compatibility Matrix**: O(N × U) pre-computed
- **Causal Clocks**: O(N) entries per clock

### Network Complexity
- **Full Mesh**: O(N²) connections
- **Relationship-Based**: O(N × R_avg) connections where R_avg = average relationships per peer
- **Gossip**: O(N × S) where S = sample size

---

## Security Considerations

1. **Schema Validation**: Reject malformed data at network boundary
2. **Rate Limiting**: Prevent spam from malicious peers
3. **Signature Verification**: Verify peer identity before accepting updates
4. **Bounded Resources**: Cap memory/storage per peer
5. **Byzantine Tolerance**: Design for up to F malicious peers (if applicable)
6. **Privacy**: Consider zero-knowledge proofs for sensitive relationship data

---

**Design Philosophy**: Elegant relationship-based coordination, validated data integrity, reactive state management, and unit-native processing for real-world constraints. Applicable to any decentralized protocol requiring peer coordination and eventual consistency.