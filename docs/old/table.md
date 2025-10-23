# Algorithm Synchronization & Data Flow (elegant.svelte.ts)

## Store Architecture from elegant.svelte.ts

### Raw Data Stores (Populated by Holster Subscriptions)

| Store | Type | Source | Updates |
|-------|------|--------|---------|
| `myCommitment` | Writable | My local state | When I change needs/capacity |
| `myDenominators` | Writable | My local computation | After computing from tree members' needs |
| `myTree` | Writable | My local state | When I update recognition |
| `networkCommitments` | Writable | Holster subscriptions | Real-time from network |
| `networkDenominators` | Writable | Holster subscriptions | Real-time from network |
| `networkTrees` | Writable | Holster subscriptions | Real-time from network |
| `currentRound` | Writable | Coordinators | When new round announced |

### Derived Subgroups (Emerge from Algorithm)

| Subgroup | Derived From | Purpose | Data Needed |
|----------|--------------|---------|-------------|
| `myTreeMembers` | `myTree` | People I recognize → compute my denominator | Their commitments (residual_need) |
| `activeContributors` | `networkCommitments`, `networkDenominators` | Providers who recognize me → compute what I receive | Their capacity, denominators |
| `activeRecipients` | `myTreeMembers`, `networkCommitments` | People in my tree with needs → appear in my numerators | Their residual_need |
| `mutualContributors` | `myTreeMembers` ∩ `activeContributors` | Bidirectional recognition → full coordination | Everything |
| `satisfiedRecipients` | `myTreeMembers`, `networkCommitments` | Zero residual need → drop from denominators | Their residual_need = 0 |
| `saturatedProviders` | `networkDenominators` | Zero denominators → at rest | All denominators = 0 |
| `isConverged` | `myDenominators`, `previousDenominators` | System equilibrium | ΔDenominator < ε |

---

## Continuous Async Operations (No Round Coordination Needed)

| Operation | Sync/Async | Data Source | Trigger | Purpose |
|-----------|------------|-------------|---------|---------|
| **Tree Management** | Async | `myTree` | User edit | Update recognition → triggers subscription changes |
| **Subscription Sync** | Async | Derived stores | `myTreeMembers`, `activeContributors` change | Maintain data connections |
| **Commitment Updates** | Async | `myCommitment` | Local state change | Publish needs/capacity to network |
| **Denominator Computation** | Async | `myTreeMembers` + `networkCommitments` | When tree members' needs change | Compute `Σ[MR × Residual]` |
| **Convergence Detection** | Async | `myDenominators` + `previousDenominators` | After denominator update | Check if `ΔDenom < ε` |

---

## Round-Synchronized Operations

### Phase Timing Breakdown

| Phase | Duration | Sync Type | Critical Data | Purpose |
|-------|----------|-----------|---------------|---------|
| **0. Round Announcement** | Instant | Pub-Sub | Round number from coordinator | All participants know round N started |
| **1. Commitment Snapshot** | 0-5s | **SYNC** | All participants' current commitments | Create consistent view for allocations |
| **2. Allocation Computation** | 5-15s | Async/Local | My tree members' residual needs (from snapshot) | Compute allocations using consistent denominator |
| **3. Allocation Publishing** | 15-20s | Async/Pub-Sub | My computed allocations | Send to recipients |
| **4. Aggregation Window** | 20-50s | Async with timeout | All providers' allocations to me | Collect total received |
| **5. Residual Update** | 50-55s | **SYNC** | My stated_need + total_received | Update residual for next round |
| **6. Convergence Check** | 55-60s | Async | Sample denominators across network | Detect if system converged |

---

## Data Dependencies by Role

### As Provider (I allocate to my tree members):

```typescript
// CONTINUOUS: Subscribe to tree members
myTreeMembers.subscribe((members) => {
  syncBeneficiarySubscriptions(members); // Get their commitments
});

// ROUND N: Allocation computation
const myDenominator = computed from {
  myTree: Record<pubKey, MR_value>,
  networkCommitments: { [pubKey]: { residual_need, stated_need } }
};

const allocations = myCapacity * (
  (MR(me, recipient) * residual_need(recipient)) / myDenominator
);

// ROUND N: Publish allocations
publishAllocations(round, allocations);
```

### As Recipient (I receive from contributors):

```typescript
// CONTINUOUS: Subscribe to contributors
activeContributors.subscribe((contributors) => {
  syncContributorSubscriptions(contributors); // Get their capacity & denominators
});

// ROUND N: Estimate expected allocations
const expectedFromProvider = 
  provider.capacity * provider.MR(me) * myResidualNeed / provider.denominator;

// ROUND N: Aggregate received
const totalReceived = aggregateAllocations(round, activeContributors);

// ROUND N: Update residual (SYNC POINT)
myResidualNeed = max(0, myStatedNeed - totalReceived);
```

---

## Critical Synchronization Points

### 1. **Commitment Snapshot** (Round Start)
```
ALL participants must use commitments from the SAME moment
→ Providers use consistent residual_need values in denominators
→ Prevents race conditions in allocation calculations

Implementation in elegant.svelte.ts:
- Round coordinator announces round N
- All participants freeze current networkCommitments
- Use frozen snapshot for allocation computation
```

### 2. **Residual Need Update** (Round End)
```
ALL recipients must update residual_need AFTER receiving ALL allocations
→ Prevents feedback loop: Provider sees updated need before sending allocation
→ Ensures denominator convergence

Implementation in elegant.svelte.ts:
- Wait for aggregation window to close
- Update myCommitment.residual_need = stated_need - total_received
- Publish updated commitment for next round
```

---

## Participant Dropout Analysis

### Scenario 1: Provider Drops Out Mid-Round

**What Happens:**
```typescript
// Round N starts
providers = [Alice, Bob, Carol]
Alice.denominator = 500
Bob.denominator = 300
Carol.denominator = 200

// Carol drops out at t=25s (after allocation phase, before publishing)
// Recipients were expecting allocations from Carol

// Aggregation phase (t=20-50s)
recipient.expectedAllocations = [Alice's, Bob's, Carol's]
recipient.receivedAllocations = [Alice's, Bob's] // Carol missing ❌

// At aggregation timeout (t=50s)
recipient.totalReceived < recipient.expectedTotal
recipient.residualNeed = stated_need - totalReceived
// residualNeed is HIGHER than expected → next round will allocate more
```

**Recovery:**
```
Round N+1:
- activeContributors derived store recomputes (Carol removed if no commitment)
- Carol drops from expectedAllocations
- Remaining providers' denominators INCREASE (Carol's capacity removed)
- Remaining providers allocate MORE to compensate
→ System converges with fewer providers
```

**Elegant.svelte.ts handles this via:**
- `activeContributors` only includes providers with recent commitments & denominators
- Timeout in aggregation phase (don't wait forever)
- Residual need naturally increases if under-allocated

### Scenario 2: Recipient Drops Out Mid-Round

**What Happens:**
```typescript
// Round N starts
myTreeMembers = [Dave, Eve, Frank]
Dave.residual_need = 100
Eve.residual_need = 50
Frank.residual_need = 75

// Eve drops out at t=30s (after allocation phase)
// I already computed and sent allocations to Eve

myDenominator(round N) = MR(me,Dave)*100 + MR(me,Eve)*50 + MR(me,Frank)*75
myAllocations = {
  Dave: allocation_dave,
  Eve: allocation_eve, // ❌ Eve won't receive this
  Frank: allocation_frank
}

// My capacity is used but Eve's allocation is wasted
```

**Recovery:**
```
Round N+1:
- Eve doesn't publish updated commitment
- networkCommitments[Eve] becomes stale
- activeRecipients derived store removes Eve (no recent commitment)
- My denominator recomputes WITHOUT Eve
- My capacity redistributes to Dave and Frank
→ No wasted capacity in next round
```

**Elegant.svelte.ts handles this via:**
- `activeRecipients` filters by recent commitment timestamp
- Stale commitments (>timeout) excluded from denominator
- `satisfiedRecipients` / dropped recipients treated identically
- Automatic rebalancing next round

### Scenario 3: Coordinator Drops Out

**What Happens:**
```typescript
// Coordinator Alice announces rounds
roundCoordinators = ['alice_pub', 'bob_pub']

// Alice drops out, no round announcement from her
// Bob is backup coordinator

// In elegant.svelte.ts:
roundCoordinators.subscribe((coordinators) => {
  coordinators.forEach(pub => {
    holsterUser.get([pub, 'round']).on(roundData => {
      if (roundData.number > currentRound) {
        currentRound.set(roundData.number); // First one wins
      }
    });
  });
});
```

**Recovery:**
```
- Multiple coordinators can announce rounds
- First announcement triggers round for all participants
- If all coordinators drop: any participant can self-coordinate
- No single point of failure
```

### Scenario 4: Participant Rejoins After Dropout

**What Happens:**
```typescript
// Carol dropped at round N
// Carol rejoins at round N+5

// Carol publishes commitment with fresh timestamp
commitment = {
  residual_need: 200,
  stated_need: 200,
  capacity: 500,
  timestamp: now()
}

// Derived stores immediately recompute:
// - If Carol is in someone's tree → activeRecipients includes her
// - If Carol has capacity & recognizes others → activeContributors includes her
// - Subscriptions sync within 100ms (debounce)
```

**Recovery:**
```
Round N+6:
- Carol appears in denominators again
- Allocations flow to/from Carol
- No special "rejoin" logic needed
→ Graceful rejoin via reactive stores
```

---

## Dropout Recovery Properties

### 1. **Self-Healing Denominators**
```
Provider drops → Removed from activeContributors → Capacity redistributes
Recipient drops → Removed from activeRecipients → Denominator shrinks

No manual cleanup needed - derived stores handle it
```

### 2. **Timeout-Based Resilience**
```
Aggregation phase has timeout (30s typical)
- Don't wait forever for missing allocations
- Update residual with whatever was received
- Under-allocation auto-corrects next round
```

### 3. **Timestamp-Based Freshness**
```typescript
// In activeContributors derived store:
const STALE_THRESHOLD = 60000; // 60 seconds

const isStale = (commitment: Commitment) => 
  Date.now() - commitment.timestamp > STALE_THRESHOLD;

// Stale commitments excluded automatically
```

### 4. **No Consensus Required**
```
Dropout is detected locally by each participant
No need for global agreement that someone dropped
Eventually consistent via reactive stores
```

---

## Data Freshness & Staleness

### Holster Subscription Guarantees

```typescript
// Holster automatically propagates data
user.get([pub, 'commitments']).on(data => {
  updateNetworkCommitment(pub, data);
}, true); // true = get current value immediately

// If pub drops:
// - No new data arrives
// - Last timestamp becomes increasingly stale
// - Derived stores filter out stale participants
```

### Staleness Detection

```typescript
// Add to elegant.svelte.ts:
export const activeParticipants = derived(
  [networkCommitments],
  ([$networkCommitments]) => {
    const now = Date.now();
    const STALE_THRESHOLD = 60000; // 60s
    
    return Object.entries($networkCommitments)
      .filter(([_, commitment]) => 
        now - commitment.timestamp < STALE_THRESHOLD
      )
      .map(([pubKey, _]) => pubKey);
  }
);
```

---

## The Beautiful Emergent Behavior

### Dropout is Indistinguishable from Satisfaction

```
Satisfied recipient: residual_need → 0 → drops from denominators
Dropped recipient: commitment.timestamp stale → excluded from denominators

Same mathematical result!
```

### System Self-Stabilizes

```
Round N: Provider drops → recipients under-allocated
Round N+1: Higher residual needs → remaining providers allocate more
Round N+2: Converges to new equilibrium

No external intervention needed
```

### Zero-Configuration Recovery

```
Participant drops → Eventually excluded by staleness timeout
Participant rejoins → Immediately included by fresh commitment
No "leave" or "join" protocol required
```

---

## Summary: What Requires Sync vs Async

### SYNC (Round-Coordinated):
1. **Commitment snapshot** - All use same starting state
2. **Residual update** - All update after aggregation completes

### ASYNC (Continuous):
1. Tree changes → subscription updates
2. Commitment publishing → network propagation
3. Denominator computation → local calculation
4. Dropout detection → staleness filtering
5. Convergence detection → local threshold check

### The Key Insight:
**Only the allocation feedback loop requires synchronization - everything else, including dropout handling, is naturally async via reactive stores!**

```
Dropout → Stale commitment → Filtered by derived store → Denominator recomputes → System rebalances
                              (All automatic, no sync needed)
```
