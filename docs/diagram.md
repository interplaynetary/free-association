# Free Association Allocation: Reactive Flow Diagram with ITC Operations

```mermaid
sequenceDiagram
    participant User
    participant UI (+page.svelte)
    participant myCommitmentStore
    participant networkCommitments
    participant myAllocationsAsProvider
    participant enableAutoPublishing
    participant allocation.ts
    participant ITC (itc.ts)
    participant Holster
    participant Network
    participant Peer_Commitment
    participant Peer_Allocations

    Note over User,Peer_Allocations: ═══════════ INITIALIZATION ═══════════

    User->>UI (+page.svelte): Page loads
    UI (+page.svelte)->>myCommitmentStore: Initialize (line 266-277 stores.svelte.ts)
    myCommitmentStore->>Holster: Subscribe to 'allocation/commitment'
    Holster-->>myCommitmentStore: Load stored state {need_slots, capacity_slots, slot_allocations}
    
    rect rgb(230, 240, 255)
        Note over myCommitmentStore,ITC (itc.ts): 🕐 ITC INITIALIZATION<br/>If no existing stamp, create seed
        myCommitmentStore->>ITC (itc.ts): itcSeed() (stores.svelte.ts:1959)
        ITC (itc.ts)-->>myCommitmentStore: New stamp {id: 1, event: 0}
    end
    
    UI (+page.svelte)->>enableAutoPublishing: enableAutoAllocationPublishing() (line 926 allocation.svelte.ts)
    Note over enableAutoPublishing: Subscribes to myAllocationsAsProvider<br/>Auto-publishes slot_allocations to commitment
    
    UI (+page.svelte)->>Network: syncSubscriptionsWithTree() (line 1539-1555 stores.svelte.ts)
    Note over Network: Subscribe to contributors' commitments<br/>Based on recognition tree

    Note over User,Peer_Allocations: ═══════════ INITIAL ALLOCATION (SELF ONLY) ═══════════

    Note over myCommitmentStore,myAllocationsAsProvider: networkCommitments is empty → compute with self only
    
    myCommitmentStore->>myAllocationsAsProvider: Reactive trigger (depends on myCommitmentStore)
    Note over myAllocationsAsProvider: Line 476-498: Derived store dependencies:<br/>✅ myPublicKey<br/>✅ myMutualRecognition<br/>✅ myRecognitionOfOthers<br/>✅ myCommitmentStore<br/>✅ networkCommitments (NEW!)
    
    myAllocationsAsProvider->>allocation.ts: getAllCommitmentsRecord()
    allocation.ts-->>myAllocationsAsProvider: {myPub: myCommitment} (only me!)
    
    myAllocationsAsProvider->>allocation.ts: computeAllocations(myCapacity, allCommitments)
    Note over allocation.ts: Line 1317-1400: Pure allocation function<br/>Reads DECLARED needs from commitments<br/>NO adjustment for received allocations<br/>Provider-side only computation
    
    allocation.ts-->>myAllocationsAsProvider: {allocations: [self-allocation], ...}
    myAllocationsAsProvider-->>UI (+page.svelte): Display initial allocations
    
    myAllocationsAsProvider-->>enableAutoPublishing: Subscription callback fires
    enableAutoPublishing->>enableAutoPublishing: Check isPublishing flag (line 940)
    enableAutoPublishing->>enableAutoPublishing: Check lastPublishedHash (line 956)
    
    rect rgb(230, 240, 255)
        Note over enableAutoPublishing,ITC (itc.ts): 🕐 ITC INCREMENT<br/>Increment for local allocation event
        enableAutoPublishing->>ITC (itc.ts): incrementMyITCStamp() → itcEvent() (allocation.svelte.ts:122-128)
        ITC (itc.ts)-->>enableAutoPublishing: Updated stamp with incremented event
    end
    
    enableAutoPublishing->>myCommitmentStore: set({...commitment, slot_allocations: [...], itcStamp: updated})
    
    myCommitmentStore->>Holster: persistDebounced() (100ms)
    Holster->>Network: .put() - Publish to network

    Note over User,Peer_Allocations: ═══════════ PEER COMMITMENT ARRIVES ═══════════

    Network->>networkCommitments: Peer's commitment received!
    Note over networkCommitments: Line 778-814 stores.svelte.ts<br/>Updates versioned store with peer data
    
    rect rgb(230, 240, 255)
        Note over networkCommitments,ITC (itc.ts): 🕐 ITC CAUSALITY CHECK<br/>Validate peer stamp is not from future
        networkCommitments->>ITC (itc.ts): itcLeq(peerStamp, myStamp) (allocation.svelte.ts:142-148)
        ITC (itc.ts)-->>networkCommitments: true = accept, false = reject (from future)
    end
    
    rect rgb(230, 240, 255)
        Note over networkCommitments,ITC (itc.ts): 🕐 ITC MERGE<br/>Merge peer's causal history into mine
        networkCommitments->>ITC (itc.ts): mergeITCStampFromPeer() → itcJoin() (allocation.svelte.ts:130-140)
        ITC (itc.ts)-->>networkCommitments: Merged stamp containing both histories
    end
    
    rect rgb(200, 255, 200)
        Note over networkCommitments,myAllocationsAsProvider: 🎯 KEY FIX: myAllocationsAsProvider now depends on networkCommitments!<br/>When peer arrives, allocation AUTOMATICALLY recomputes
    end
    
    networkCommitments->>myAllocationsAsProvider: Reactive trigger (NEW DEPENDENCY!)
    myAllocationsAsProvider->>allocation.ts: getAllCommitmentsRecord()
    allocation.ts-->>myAllocationsAsProvider: {myPub: myCommit, peerPub: peerCommit}
    
    myAllocationsAsProvider->>allocation.ts: computeAllocations(myCapacity, allCommitments)
    Note over allocation.ts: Line 1317-1342: Classify into tiers<br/>Line 1344-1380: Compute distribution shares<br/>Line 1387-1395: Call allocateWithDistribution()<br/>Allocates to BOTH self AND peer based on mutual recognition
    
    allocation.ts-->>myAllocationsAsProvider: {allocations: [to_self, to_peer], ...}
    myAllocationsAsProvider-->>UI (+page.svelte): Display updated allocations ✅
    
    myAllocationsAsProvider-->>enableAutoPublishing: Trigger
    
    rect rgb(230, 240, 255)
        Note over enableAutoPublishing,ITC (itc.ts): 🕐 ITC INCREMENT<br/>Increment for new allocation event
        enableAutoPublishing->>ITC (itc.ts): incrementMyITCStamp() → itcEvent() (allocation.svelte.ts:122-128)
        ITC (itc.ts)-->>enableAutoPublishing: Updated stamp
    end
    
    enableAutoPublishing->>myCommitmentStore: Update slot_allocations + itcStamp
    myCommitmentStore->>Holster: Persist
    Holster->>Network: Publish updated allocations

    Note over User,Peer_Allocations: ═══════════ INFINITE LOOP PREVENTION ═══════════

    rect rgb(255, 255, 200)
        Note over myCommitmentStore,enableAutoPublishing: Potential circular dependency:<br/>myCommitmentStore update → myAllocationsAsProvider → enableAutoPublishing → myCommitmentStore
    end
    
    myCommitmentStore->>myAllocationsAsProvider: Would trigger again...
    myAllocationsAsProvider->>myAllocationsAsProvider: Line 510-548: MEMOIZATION CHECK
    Note over myAllocationsAsProvider: Deep equality on inputs (excluding metadata)<br/>If unchanged, return cached result<br/>Prevents redundant computation
    
    myAllocationsAsProvider-->>enableAutoPublishing: Same allocations returned
    enableAutoPublishing->>enableAutoPublishing: Line 956: lastPublishedHash check
    Note over enableAutoPublishing: Hash matches → SKIP publish<br/>✅ Loop prevented!

    Note over User,Peer_Allocations: ═══════════ USER CHANGES THEIR NEEDS ═══════════

    User->>UI (+page.svelte): Change needs (e.g., add $20 rent need)
    UI (+page.svelte)->>myCommitmentStore: Update need_slots
    
    rect rgb(230, 240, 255)
        Note over myCommitmentStore,ITC (itc.ts): 🕐 ITC MERGE + INCREMENT<br/>Merge all network stamps, then increment
        myCommitmentStore->>ITC (itc.ts): getMergedITCStamp() (stores.svelte.ts:1959-1982)
        Note over ITC (itc.ts): For each peer stamp: itcJoin(myStamp, peerStamp)<br/>Then: itcEvent(mergedStamp)
        ITC (itc.ts)-->>myCommitmentStore: Merged + incremented stamp
    end
    
    myCommitmentStore->>myAllocationsAsProvider: Reactive trigger
    myAllocationsAsProvider->>allocation.ts: computeAllocations()
    
    rect rgb(255, 220, 220)
        Note over allocation.ts: ⚠️ DISTRIBUTED SYSTEM TIMING:<br/>Peer hasn't seen my change yet<br/>Their commitment still has OLD data<br/>I compute based on current network state
    end
    
    allocation.ts-->>myAllocationsAsProvider: {allocations: [...]} (based on OLD peer state)
    myAllocationsAsProvider-->>UI (+page.svelte): Display allocations (INTERMEDIATE STATE)
    
    myAllocationsAsProvider-->>enableAutoPublishing: Trigger
    
    rect rgb(230, 240, 255)
        Note over enableAutoPublishing,ITC (itc.ts): 🕐 ITC INCREMENT<br/>Increment for allocation change
        enableAutoPublishing->>ITC (itc.ts): incrementMyITCStamp() → itcEvent()
        ITC (itc.ts)-->>enableAutoPublishing: Updated stamp
    end
    
    enableAutoPublishing->>myCommitmentStore: Update slot_allocations + itcStamp
    myCommitmentStore->>Holster: Persist
    Holster->>Network: Publish my NEW needs + NEW allocations + MERGED ITC

    Note over User,Peer_Allocations: ═══════════ NETWORK PROPAGATION (ASYNC) ═══════════

    Network->>Peer_Commitment: My commitment arrives (100-500ms latency)
    Note over Peer_Commitment: Peer's myAllocationsAsProvider depends on<br/>their networkCommitments (which includes me!)
    
    rect rgb(230, 240, 255)
        Note over Peer_Commitment,ITC (itc.ts): 🕐 PEER ITC OPERATIONS<br/>Peer validates and merges my stamp
        Peer_Commitment->>ITC (itc.ts): itcLeq(myStamp, theirStamp)?
        Note over ITC (itc.ts): Check if my update is causally consistent
        Peer_Commitment->>ITC (itc.ts): itcJoin(theirStamp, myStamp)
        Note over ITC (itc.ts): Merge my causal history
    end
    
    Peer_Commitment->>Peer_Allocations: Reactive trigger
    Peer_Allocations->>Peer_Allocations: Recompute allocations with MY new needs
    
    rect rgb(230, 240, 255)
        Note over Peer_Allocations,ITC (itc.ts): 🕐 PEER ITC INCREMENT
        Peer_Allocations->>ITC (itc.ts): itcEvent(theirStamp)
    end
    
    Peer_Allocations->>Network: Publish THEIR updated allocations + ITC

    Note over Network: ⏱️ Time passes (another 100-500ms)...

    Network->>networkCommitments: Peer's UPDATED commitment arrives!
    
    rect rgb(230, 240, 255)
        Note over networkCommitments,ITC (itc.ts): 🕐 ITC CAUSALITY CHECK + MERGE
        networkCommitments->>ITC (itc.ts): itcLeq(peerUpdatedStamp, myStamp)?
        networkCommitments->>ITC (itc.ts): itcJoin(myStamp, peerUpdatedStamp)
        ITC (itc.ts)-->>networkCommitments: Merged stamp
    end
    
    networkCommitments->>myAllocationsAsProvider: Reactive trigger (depends on networkCommitments!)
    myAllocationsAsProvider->>allocation.ts: computeAllocations()
    
    rect rgb(200, 255, 200)
        Note over allocation.ts: ✅ NOW HAS COMPLETE STATE:<br/>My NEW needs<br/>Peer's UPDATED allocations (reacted to my change)<br/>System has converged!
    end
    
    allocation.ts-->>myAllocationsAsProvider: {allocations: [...]} (FINAL CONVERGED)
    myAllocationsAsProvider-->>UI (+page.svelte): Display final allocations ✅

    Note over User,Peer_Allocations: ═══════════ PROVIDER-SIDE DAMPENING (README.md line 283-298) ═══════════

    rect rgb(255, 240, 200)
        Note over myAllocationsAsProvider,allocation.ts: 🎚️ DAMPENING: Prevent oscillation in allocations<br/><br/>When computing allocations, providers apply dampening<br/>to perceived need based on oscillation history
        
        myAllocationsAsProvider->>myAllocationsAsProvider: Check oscillation history (allocation.svelte.ts:~700)<br/>overAllocationHistory[recipientPub][typeId]
        
        Note over myAllocationsAsProvider: Detect pattern: 100→0→100 (oscillation!)<br/>vs: 100→80→60 (smooth convergence)
        
        myAllocationsAsProvider->>allocation.ts: computeDampingFactors(history)
        Note over allocation.ts: Per README.md line 291:<br/>oscillation detected → damping = 0.6-0.8<br/>smooth convergence → damping = 1.0
        allocation.ts-->>myAllocationsAsProvider: dampingFactors[typeId] = 0.7
        
        Note over myAllocationsAsProvider: Apply dampening to perceived need:<br/>activeNeed = declaredNeed × dampingFactor<br/>= 100 × 0.7 = 70<br/><br/>Provider allocates based on activeNeed (70),<br/>NOT declaredNeed (100)
        
        myAllocationsAsProvider->>allocation.ts: computeAllocations(activeNeed=70, ...)
        allocation.ts-->>myAllocationsAsProvider: Allocations based on damped need
    end

    Note over User,Peer_Allocations: ═══════════ RECIPIENT-SIDE AUTO-UPDATE (README.md line 312) ═══════════

    rect rgb(200, 255, 200)
        Note over networkCommitments,myCommitmentStore: 📥 AUTOMATIC REMAINING NEED TRACKING<br/><br/>System automatically tracks allocations received,<br/>computes remaining need, and publishes updates
        
        Note over networkCommitments: networkAllocations field store (stores.svelte.ts:473)<br/>Fine-grained reactivity: only triggers when allocations change
        
        networkCommitments->>networkCommitments: Peer publishes slot_allocations<br/>networkAllocations field updates
        
        networkCommitments->>myCommitmentStore: enableAutoRemainingNeedTracking() ⚠️ TO IMPLEMENT<br/>(allocation.svelte.ts - MISSING!)
        
        Note over myCommitmentStore: Subscribe to networkAllocations<br/>Filter: allocation.recipient_pubkey === myPub
        
        myCommitmentStore->>myCommitmentStore: recordAllocationReceived(typeId, amount, providerPub)<br/>(allocation.svelte.ts:694)
        
        Note over myCommitmentStore: Updates totalReceivedByType store<br/>totalReceivedByType[typeId] += amount
        
        myCommitmentStore->>myCommitmentStore: myNeedsAtNextStep (derived store, line 659)<br/>Computes: remainingNeed = max(0, declaredNeed - totalReceived)
        
        myCommitmentStore->>myCommitmentStore: Debounce (500ms) - batch multiple allocations
        
        myCommitmentStore->>myCommitmentStore: applyNeedUpdateLawToCommitment()<br/>(allocation.svelte.ts:670)
        
        Note over myCommitmentStore: Update need_slots with remaining needs<br/>Auto-publish updated commitment to network
        
        myCommitmentStore->>Holster: Persist updated commitment
        Holster->>Network: Publish UPDATED needs (remaining, not declared!)
    end

    rect rgb(200, 255, 200)
        Note over User,Peer_Allocations: ✅ IMPLEMENTATION COMPLETE (2025-11-11):<br/><br/>enableAutoRemainingNeedTracking() is NOW implemented!<br/><br/>✅ totalReceivedByType store (line 651)<br/>✅ myNeedsAtNextStep derived store (line 659)<br/>✅ applyNeedUpdateLawToCommitment() function (line 670)<br/>✅ recordAllocationReceived() function (line 694)<br/>✅ networkAllocations field store (stores.svelte.ts:473)<br/>✅ enableAutoRemainingNeedTracking() function (line 745-840)<br/>✅ Enabled in +page.svelte:62<br/><br/>Recipients automatically reduce their needs,<br/>providers see updated remaining needs, system converges!
    end

    Note over User,Peer_Allocations: ═══════════ ITC OPERATIONS SUMMARY ═══════════

    Note over User,Peer_Allocations: 🕐 ITC OPERATIONS USED:<br/><br/>1. **itcSeed()** - itc.ts:54<br/>   • Create initial stamp {id: 1, event: 0}<br/>   • Used in: stores.svelte.ts:1959, allocation.ts:169<br/><br/>2. **itcEvent(stamp)** - itc.ts:61<br/>   • Increment event counter for local changes<br/>   • Used in: allocation.svelte.ts:122-128 (incrementMyITCStamp)<br/>   • Used in: stores.svelte.ts:1975 (getMergedITCStamp)<br/><br/>3. **itcJoin(stamp1, stamp2)** - itc.ts:84<br/>   • Merge two stamps (causal history union)<br/>   • Used in: allocation.svelte.ts:130-140 (mergeITCStampFromPeer)<br/>   • Used in: stores.svelte.ts:1969 (getMergedITCStamp loop)<br/><br/>4. **itcLeq(stamp1, stamp2)** - itc.ts:100<br/>   • Check if stamp1 ≤ stamp2 (causally precedes)<br/>   • Used in: allocation.svelte.ts:142-148 (isPeerUpdateStale)<br/>   • Used in: allocation.svelte.ts:150-189 (getCausallyConsistentCommitments)

    Note over User,Peer_Allocations: 🕐 ITC CAUSALITY GUARANTEES:<br/><br/>✅ **Happens-Before Relationship**<br/>   If A → B (A happened before B), then stamp_A ≤ stamp_B<br/><br/>✅ **Concurrent Events**<br/>   If A || B (concurrent), neither stamp_A ≤ stamp_B nor stamp_B ≤ stamp_A<br/><br/>✅ **Eventual Consistency**<br/>   All nodes eventually see all events in causal order<br/><br/>✅ **No Stale Writes**<br/>   Reject updates with stamp from "future" (not causally consistent)<br/><br/>✅ **Idempotent Merges**<br/>   join(A, B) = join(B, A), join(A, A) = A

    Note over User,Peer_Allocations: ═══════════ ARCHITECTURE SUMMARY ═══════════

    Note over User,Peer_Allocations: ✅ KEY ARCHITECTURAL PROPERTIES:<br/><br/>1. PROVIDER-SIDE ALLOCATION + DAMPENING:<br/>   - Each provider computes allocations independently<br/>   - Reads recipients' PUBLISHED REMAINING needs from commitments<br/>   - ✅ Applies dampening to perceived need (oscillation prevention)<br/>   - activeNeed = declaredNeed × dampingFactor<br/>   - Allocates based on activeNeed, NOT declaredNeed<br/>   - Dampening: 0.6-0.8 (oscillation) or 1.0 (smooth)<br/>   - ✅ FULLY IMPLEMENTED (allocation.ts:1328-1383)<br/><br/>2. RECIPIENT-SIDE AUTO-UPDATE (COORDINATION):<br/>   - ✅ FULLY IMPLEMENTED (allocation.svelte.ts:745-840)<br/>   - System automatically tracks received allocations<br/>   - Computes: remainingNeed = max(0, declaredNeed - totalReceived)<br/>   - Auto-publishes updated commitment with remaining need<br/>   - ✅ enableAutoRemainingNeedTracking() function exists and enabled<br/>   - Result: providers see updated needs, system converges!<br/><br/>3. REACTIVE NETWORK SYNC:<br/>   - myAllocationsAsProvider depends on networkCommitments<br/>   - networkAllocations field store (fine-grained reactivity)<br/>   - When ANY peer commitment/allocation changes, recompute<br/>   - Automatic convergence through reactive updates<br/><br/>4. ITC CAUSALITY TRACKING:<br/>   - Every commitment has ITC stamp<br/>   - Validates causal consistency before accepting updates<br/>   - Merges peer histories to maintain causal ordering<br/>   - Prevents out-of-order processing and data loss<br/><br/>5. CIRCULAR DEPENDENCY GUARDS:<br/>   - Memoization: Skip recompute if inputs unchanged<br/>   - Hash guards: Skip publish if allocations unchanged<br/>   - isPublishing flag: Prevent re-entry<br/><br/>6. CONTINUOUS REACTIVE CONVERGENCE:<br/>   - No discrete rounds - continuous event-driven updates<br/>   - Provider dampening prevents oscillation<br/>   - Recipient auto-update ensures coordination<br/>   - Over-allocation is temporary and self-correcting<br/>   - System converges through parallel, independent updates<br/>   - ITC ensures causal consistency across all nodes

    Note over User,Peer_Allocations: 💡 WHY REFRESH "FIXES" IT:<br/><br/>When you refresh after making changes:<br/>1. Network has already propagated updates (>500ms passed)<br/>2. Both commitments are in final converged state<br/>3. ITC stamps are merged and consistent<br/>4. Initial load computes with complete information<br/>5. No waiting for async propagation<br/><br/>This is NORMAL distributed system behavior!<br/>Not a bug - it's eventual consistency by design.

    Note over User,Peer_Allocations: 🔧 CRITICAL FIX APPLIED:<br/><br/>Added networkCommitments as dependency to myAllocationsAsProvider<br/>(Line 482-490 allocation.svelte.ts)<br/><br/>BEFORE: Only recomputed when MY data changed<br/>AFTER: Recomputes when ANY peer commitment arrives/updates<br/><br/>Result: Automatic cross-user allocation without refresh! ✅
```

## ITC Operations Reference

### File Locations

#### itc.ts (Interval Tree Clocks Library)
- **Line 54**: `seed()` - Create initial stamp `{id: 1, event: 0}`
- **Line 61**: `event(stamp)` - Increment event counter
- **Line 84**: `join(stamp1, stamp2)` - Merge two stamps
- **Line 100**: `leq(stamp1, stamp2)` - Check causal precedence (≤)
- **Line 93**: `peek(stamp)` - Create stamp with null id (for messages)

#### stores.svelte.ts (Network Store Management)
- **Line 1959**: `itcSeed()` - Initialize stamp if none exists
- **Line 1969**: `itcJoin()` - Merge network stamps (loop over all peers)
- **Line 1975**: `itcEvent()` - Increment after merging

#### allocation.svelte.ts (Reactive Allocation Wrapper)
- **Line 122-128**: `incrementMyITCStamp()` - Calls `itcEvent()` for local changes
- **Line 130-140**: `mergeITCStampFromPeer()` - Calls `itcJoin()` with peer stamp
- **Line 142-148**: `isPeerUpdateStale()` - Calls `itcLeq()` to check causality
- **Line 150-189**: `getCausallyConsistentCommitments()` - Uses `itcLeq()` to filter valid updates

#### allocation.ts (Pure Algorithm)
- **Line 169**: `itcSeed()` - Create initial system state stamp
- **Uses ITCStamp type throughout but doesn't call ITC operations directly**

### ITC Usage Pattern

```
INITIALIZATION:
  stores.svelte.ts:1959 → itcSeed() → {id: 1, event: 0}

LOCAL CHANGE (needs/capacity/allocation):
  allocation.svelte.ts:122 → incrementMyITCStamp()
    → stores.svelte.ts:1959 → getMergedITCStamp()
      → itcJoin() with all peers (stores.svelte.ts:1969)
      → itcEvent() to increment (stores.svelte.ts:1975)

PEER UPDATE ARRIVES:
  allocation.svelte.ts:142 → isPeerUpdateStale()
    → itcLeq(peerStamp, myStamp) - validate not from future
  
  allocation.svelte.ts:130 → mergeITCStampFromPeer()
    → itcJoin(myStamp, peerStamp) - merge histories
```

### Causality Guarantees

1. **Happens-Before (→)**: If event A happened before B, then `leq(stampA, stampB) = true`
2. **Concurrent (||)**: If events are concurrent, neither `leq(A,B)` nor `leq(B,A)` is true
3. **Total Order**: All nodes eventually see events in same causal order
4. **No Stale Writes**: Updates from "future" (not causally consistent) are rejected
5. **Idempotent Merges**: `join(A,B) = join(B,A)`, `join(A,A) = A`

### Key Insight: ITC vs Vector Clocks

**Why ITC instead of Vector Clocks?**
- Dynamic participants (no pre-assigned IDs needed)
- Efficient for peer-to-peer systems
- Compact representation (no ID list)
- Fork/join semantics for decentralized creation

**Trade-off:**
- Slightly more complex operations (tree manipulation)
- But better for systems with unknown/changing participant count

## Flow Summary

### Reactive Dependencies Chain

```
networkCommitments (Map<pubKey, Commitment>)
    ↓ (reactive)
myAllocationsAsProvider (derived store)
    ↓ (subscription)
enableAutoAllocationPublishing
    ↓ (writes to)
myCommitmentStore.slot_allocations + itcStamp
    ↓ (persists to)
Holster → Network → Peers
```

### Key Reactive Triggers

1. **myCommitmentStore changes** → triggers `myAllocationsAsProvider`
2. **networkCommitments changes** → triggers `myAllocationsAsProvider` ✨ NEW!
3. **myAllocationsAsProvider changes** → triggers `enableAutoAllocationPublishing`
4. **enableAutoAllocationPublishing** → updates `myCommitmentStore.slot_allocations` + increments ITC

### Loop Prevention Mechanisms

1. **Memoization** (allocation.svelte.ts:510-548)
   - Deep equality check on inputs
   - Excludes metadata (itcStamp, timestamp, _updatedAt)
   - Returns cached result if inputs unchanged

2. **Hash Guard** (allocation.svelte.ts:956-976)
   - `lastPublishedHash` tracks last published allocations
   - Skip publish if hash matches
   - Prevents redundant network writes

3. **isPublishing Flag** (allocation.svelte.ts:940)
   - Simple re-entry guard
   - Prevents cascading updates during publish

4. **ITC Causality** (allocation.svelte.ts:142-189)
   - Reject updates from "future" (not causally consistent)
   - Prevents processing out-of-order updates
   - Maintains causal ordering across network

### Pure Allocation Logic

The allocation computation in `allocation.ts` is **provider-side only**:
- Reads `need_slots` from commitments (recipients' **published remaining needs**)
- Applies **dampening** to perceived need: `activeNeed = declaredNeed × dampingFactor`
- Allocates based on `activeNeed` (not declared need directly)
- Does NOT read `slot_allocations` from other providers
- Each provider computes independently
- Convergence happens through:
  1. **Provider dampening** → prevents oscillation
  2. **Recipient auto-update** → ensures coordination (⚠️ TO IMPLEMENT)
  3. **Reactive network propagation** → continuous convergence
- ITC stamps ensure causal consistency

This is a **stateless, pure function** approach where:
- Inputs: My capacity + All commitments (published remaining needs) + Dampening factors
- Output: My allocations to recipients
- No global coordination needed
- ITC stamps provide causal ordering without synchronization

**Theoretical Model (README.md):**
- Providers read recipients' **remaining need** (auto-computed by recipient system)
- Recipients automatically track received allocations
- Recipients automatically compute: `remainingNeed = max(0, declaredNeed - totalReceived)`
- Recipients automatically publish updated commitment with remaining need
- Over-allocation is temporary and self-correcting
- System converges through parallel, independent updates
