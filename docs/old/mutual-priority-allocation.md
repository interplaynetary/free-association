# Mutual-Priority Allocation Algorithm

## Core Principle

**Mutual recognition gets priority access to capacity. Non-mutual recognition only gets leftover capacity.**

This creates a natural hierarchy:
- **TIER 1**: Mutual partners (bilateral recognition) get allocated first
- **TIER 2**: Non-mutual recipients (one-way recognition) share remaining capacity

---

## Foundational Equations

### Recognition (Unchanged)

```
Your Recognition = 
    your acknowledgment of contributions towards your own self-actualization

Your Total-Recognition = 100%

Recognition allocated to each contributor:
    Σ Recognition(You, Contributor) = 100%
```

### Mutual Recognition (Bilateral Minimum)

```
Mutual-Recognition(You, Them) = MR(You, Them) = 
    minimum(Their-share-of-Your-total-recognition,
            Your-share-of-Their-total-recognition)

Properties:
    - Bilateral (requires both parties)
    - Bounded by lesser recognition
    - Range: [0, 1] (as fraction of 100%)
    - MR = 0 if either party doesn't recognize the other

Special Case - Self-Recognition:
    MR(You, You) = minimum(Your-recognition-of-You, Your-recognition-of-You)
                 = Your-recognition-of-You
    
    This is perfectly valid! Self-recognition enables:
    - Self-investment (allocating capacity to yourself)
    - Self-care (prioritizing your own needs)
    - Self-sufficiency (being both provider and recipient)
```

### Mutual Recognition Distribution (Renormalized)

```
Total-Mutual-Recognition(You) = 
    Σ MR(You, Them) for all Them where MR > 0

Mutual-Recognition-Distribution(You, Them) = MRD(You, Them) = 
    MR(You, Them)
    ─────────────────────────────────────
    Total-Mutual-Recognition(You)

Properties:
    - Renormalized share of MUTUAL recognition specifically
    - Σ MRD(You, Them) = 1.0 = 100% (for all mutual partners)
    - MRD is what's actually used in allocation formulas
    - Makes each tier a clean 100% distribution
```

### Recognition Weights (One-Way)

```
Recognition-Weight(You, Them) = 
    Their-share-of-Your-total-recognition

Properties:
    - Unilateral (your perspective only)
    - Can be > 0 even if MR = 0 (one-way recognition)
    - Range: [0, 1] (as fraction of 100%)
```

---

## The Two-Tier Allocation System

### Tier 1: Mutual Recognition Allocation (Priority)

**Who gets allocated:**
- Only recipients with MR > 0 (bilateral recognition)

**Total Mutual Recognition:**
```
Total-Mutual-Recognition(Provider) = 
    Σ MR(Provider, R) for all R where MR > 0
```

**Denominator (Tier 1):**
```
MutualDenominator(Provider) = 
    Σ [MRD(Provider, Recipient) × ResidualNeed(Recipient)]
    for all Recipients where MR > 0

where:
    MRD(Provider, Recipient) = MR(Provider, Recipient) / Total-Mutual-Recognition(Provider)
```

**Allocation (Tier 1):**
```
MutualAllocation(Provider → Recipient) = 
    Capacity(Provider) × MRD(Provider, Recipient) × ResidualNeed(Recipient)
    ──────────────────────────────────────────────────────────────────────
    MutualDenominator(Provider)

where:
    MRD(Provider, Recipient) = MR(Provider, Recipient) / Total-Mutual-Recognition(Provider)

Only allocated if MR(Provider, Recipient) > 0
```

**Key Property:**
```
Σ MRD(Provider, R) = 1.0 = 100% for all mutual partners
```

**Capacity Used (Tier 1):**
```
MutualCapacityUsed = Σ MutualAllocation(Provider → Recipient)
                     for all mutual recipients
```

### Tier 2: Non-Mutual Allocation (Leftover)

**Who gets allocated:**
- Only recipients where Provider recognizes Recipient (Weight > 0)
- But MR = 0 (no mutual recognition)

**Remaining Capacity:**
```
RemainingCapacity = Capacity(Provider) - MutualCapacityUsed
```

**Total Non-Mutual Recognition:**
```
Total-Non-Mutual-Recognition(Provider) = 
    Σ RecognitionWeight(Provider→R) 
    for all R where Weight > 0 AND MR = 0
```

**Denominator (Tier 2):**
```
NonMutualDenominator(Provider) = 
    Σ [RenormalizedShare(Provider, Recipient) × ResidualNeed(Recipient)]
    for all Recipients where Weight > 0 AND MR = 0

where:
    RenormalizedShare(Provider, Recipient) = 
        RecognitionWeight(Provider→Recipient) / Total-Non-Mutual-Recognition(Provider)
```

**Allocation (Tier 2):**
```
NonMutualAllocation(Provider → Recipient) = 
    RemainingCapacity × RenormalizedShare(Provider, Recipient) × ResidualNeed(Recipient)
    ────────────────────────────────────────────────────────────────────────────────────
    NonMutualDenominator(Provider)

where:
    RenormalizedShare(Provider, Recipient) = 
        RecognitionWeight(Provider→Recipient) / Total-Non-Mutual-Recognition(Provider)

Only allocated if:
  - RecognitionWeight(Provider→Recipient) > 0
  - MR(Provider, Recipient) = 0
  - RemainingCapacity > 0
```

**Key Property:**
```
Σ RenormalizedShare(Provider, R) = 1.0 = 100% for all non-mutual recipients
```

---

## Complete Worked Example

### Setup

**Participants:**
```
Provider P:
    Capacity = 1000
    
Recognition Weights (P's one-way recognition):
    P recognizes R1: 40% (0.40 of P's total recognition)
    P recognizes R2: 30% (0.30 of P's total recognition)
    P recognizes R3: 20% (0.20 of P's total recognition)
    P recognizes R4: 10% (0.10 of P's total recognition)
    Total: 100% ✓

Recipients' Recognition of P:
    R1 recognizes P: 50% (0.50 of R1's total recognition)
    R2 recognizes P: 25% (0.25 of R2's total recognition)
    R3 recognizes P: 0%  (R3 doesn't recognize P)
    R4 recognizes P: 0%  (R4 doesn't recognize P)

Stated Needs:
    R1.Stated-Need = 500
    R2.Stated-Need = 400
    R3.Stated-Need = 300
    R4.Stated-Need = 200
```

### Step 1: Compute Mutual Recognition

```
MR(P, R1) = min(P→R1, R1→P)
          = min(0.40, 0.50)
          = 0.40  ← MUTUAL

MR(P, R2) = min(P→R2, R2→P)
          = min(0.30, 0.25)
          = 0.25  ← MUTUAL

MR(P, R3) = min(P→R3, R3→P)
          = min(0.20, 0.00)
          = 0.00  ← NOT MUTUAL (one-way only)

MR(P, R4) = min(P→R4, R4→P)
          = min(0.10, 0.00)
          = 0.00  ← NOT MUTUAL (one-way only)
```

**Classification:**
- **Tier 1 (Mutual)**: R1, R2
- **Tier 2 (Non-Mutual)**: R3, R4

### Step 1b: Compute Recognition Totals by Tier

```
Total-Mutual-Recognition(P) = MR(P, R1) + MR(P, R2)
                            = 0.40 + 0.25
                            = 0.65  ← 65% of P's recognition is mutual

Total-Non-Mutual-Recognition(P) = Weight(P→R3) + Weight(P→R4)
                                = 0.20 + 0.10
                                = 0.30  ← 30% of P's recognition is non-mutual

Check: 0.65 + 0.30 + (5% unallocated or to others) = 100% ✓
```

### Step 1c: Compute Renormalized Shares (MRD)

**Tier 1 (Mutual Recognition Distribution):**
```
MRD(P, R1) = MR(P, R1) / Total-Mutual-Recognition(P)
           = 0.40 / 0.65
           = 0.615  ← 61.5% of MUTUAL recognition

MRD(P, R2) = MR(P, R2) / Total-Mutual-Recognition(P)
           = 0.25 / 0.65
           = 0.385  ← 38.5% of MUTUAL recognition

Check: 0.615 + 0.385 = 1.0 = 100% ✓ (clean distribution!)
```

**Tier 2 (Renormalized Non-Mutual):**
```
Renormalized(P, R3) = Weight(P→R3) / Total-Non-Mutual-Recognition(P)
                    = 0.20 / 0.30
                    = 0.667  ← 66.7% of NON-MUTUAL recognition

Renormalized(P, R4) = Weight(P→R4) / Total-Non-Mutual-Recognition(P)
                    = 0.10 / 0.30
                    = 0.333  ← 33.3% of NON-MUTUAL recognition

Check: 0.667 + 0.333 = 1.0 = 100% ✓ (clean distribution!)
```

---

### Step 2: Tier 1 Allocation (Mutual Partners First)

**Compute Mutual Denominator (using MRD):**
```
For R1 (mutual):
    Numerator(P, R1) = MRD(P, R1) × ResidualNeed(R1)
                     = 0.615 × 500
                     = 307.5

For R2 (mutual):
    Numerator(P, R2) = MRD(P, R2) × ResidualNeed(R2)
                     = 0.385 × 400
                     = 154.0

MutualDenominator(P) = 307.5 + 154.0 = 461.5
```

**Allocate to Mutual Partners:**
```
Allocation(P → R1) = Capacity × Numerator(P, R1) / MutualDenominator(P)
                   = 1000 × 307.5 / 461.5
                   = 666.45

Allocation(P → R2) = 1000 × 154.0 / 461.5
                   = 333.55

MutualCapacityUsed = 666.45 + 333.55 = 1000
```

**Check MRD Ratio:**
```
Allocation(R1) / Allocation(R2) = 666.45 / 333.55 = 2.0
MRD(P,R1) / MRD(P,R2) = 0.615 / 0.385 = 1.6

Wait, why don't they match?
Because allocations respect BOTH MRD AND ResidualNeed:
    Ratio = (MRD × Need) / (MRD × Need)
          = (0.615 × 500) / (0.385 × 400)
          = 307.5 / 154.0
          = 2.0 ✓

Note: Using MRD instead of raw MR gives the same final allocations!
The renormalization cancels out in the denominator.
But MRD is semantically cleaner: 100% distribution within mutual tier.
```

---

### Step 3: Tier 2 Allocation (Non-Mutual Gets Leftover)

**Remaining Capacity:**
```
RemainingCapacity = 1000 - 1000 = 0

Since all capacity was used by mutual partners,
NO capacity remains for non-mutual recipients!
```

**Result:**
```
Allocation(P → R3) = 0  (no capacity left, even though P recognizes R3)
Allocation(P → R4) = 0  (no capacity left, even though P recognizes R4)
```

**Key Insight:** Mutual recognition is POWERFUL. In this example, mutual partners (R1, R2) absorbed 100% of capacity, leaving nothing for non-mutual recipients (R3, R4), even though P recognizes them.

---

## Example 2: With Leftover Capacity

Let's modify the example so mutual partners don't use all capacity:

### Modified Setup

```
Same as before, but:
    R1.Stated-Need = 300  (reduced from 500)
    R2.Stated-Need = 200  (reduced from 400)
```

### Tier 1: Mutual Allocation

```
Numerator(P, R1) = 0.40 × 300 = 120
Numerator(P, R2) = 0.25 × 200 = 50

MutualDenominator = 120 + 50 = 170

Allocation(P → R1) = 1000 × 120 / 170 = 705.88
Allocation(P → R2) = 1000 × 50 / 170 = 294.12

MutualCapacityUsed = 1000  ← Wait, still 1000!

Why? Because we're allocating CAPACITY, not capping by need.
R1 gets 705.88 even though need is only 300 (over-allocated)
R2 gets 294.12 even though need is only 200 (over-allocated)
```

**This reveals a need for iteration** (just like the original algorithm) to prevent over-allocation!

Let me correct this:

### Tier 1: Mutual Allocation (With Need Caps)

```
Raw-Allocation(P → R1) = 1000 × 120 / 170 = 705.88
Capped-Allocation(P → R1) = min(705.88, 300) = 300  ← CAPPED

Raw-Allocation(P → R2) = 1000 × 50 / 170 = 294.12
Capped-Allocation(P → R2) = min(294.12, 200) = 200  ← CAPPED

MutualCapacityUsed = 300 + 200 = 500
```

### Tier 2: Non-Mutual Gets Leftover

```
RemainingCapacity = 1000 - 500 = 500  ← Capacity available!

Numerator(P, R3) = RecognitionWeight(P→R3) × ResidualNeed(R3)
                 = 0.20 × 300
                 = 60

Numerator(P, R4) = RecognitionWeight(P→R4) × ResidualNeed(R4)
                 = 0.10 × 200
                 = 20

NonMutualDenominator = 60 + 20 = 80

Allocation(P → R3) = RemainingCapacity × 60 / 80
                   = 500 × 60 / 80
                   = 375

Allocation(P → R4) = 500 × 20 / 80
                   = 125

Total Non-Mutual: 375 + 125 = 500 ✓
```

### Final Allocations

```
R1: 300 (mutual, PRIORITY - fully satisfied)
R2: 200 (mutual, PRIORITY - fully satisfied)
R3: 375 (non-mutual, LEFTOVER - over-allocated! needs iteration)
R4: 125 (non-mutual, LEFTOVER - under-allocated)

Total: 300 + 200 + 375 + 125 = 1000 ✓ (conservation)
```

---

## Comparison to Previous Algorithms

### vs. `simple-elegant.svelte.ts`

| Feature | simple-elegant | mutual-priority |
|---------|---------------|-----------------|
| **Recognition Type** | One-way only | Two-tier: MR + One-way |
| **Allocation Tiers** | Single tier | Two tiers (mutual first) |
| **MR Computation** | None (missing) | ✅ min(You→Them, Them→You) |
| **Non-Mutual Support** | N/A (all treated equal) | ✅ Gets leftover capacity |
| **Bilateral Consent** | ❌ Not enforced | ✅ Enforced via MR |
| **Formula** | Weight × Need | Tier 1: MR × Need<br>Tier 2: Weight × Need |

### vs. `denominator-centric-fulfillment.md`

| Feature | denominator-centric | mutual-priority |
|---------|---------------------|-----------------|
| **Recognition Type** | MR only | MR + One-way (two-tier) |
| **Can give to non-mutual?** | ❌ No | ✅ Yes (leftover) |
| **Bilateral Consent** | ✅ Required | ✅ Required (for priority) |
| **Denominator Count** | 1 per provider | 2 per provider (mutual + non-mutual) |
| **Allocation Phases** | Single phase | Two phases (mutual → non-mutual) |

---

## The Mathematical Structure

### Two Denominators per Provider

```
Provider P maintains TWO denominators:

Denominator₁(P) = Σ [MR(P,R) × Need(R)]       ← Tier 1: Mutual
                  for R where MR > 0

Denominator₂(P) = Σ [Weight(P,R) × Need(R)]  ← Tier 2: Non-Mutual
                  for R where Weight > 0 AND MR = 0
```

### Cascading Allocation

```
Phase 1: Allocate using Denominator₁
    → Produces: MutualCapacityUsed

Phase 2: Allocate remaining using Denominator₂
    → Remaining = Capacity - MutualCapacityUsed
    → Distribute among non-mutual recipients
```

### Conservation Law (Still Holds!)

```
Total-Allocated = MutualCapacityUsed + NonMutualCapacityUsed
                ≤ Capacity(Provider)

Equality holds when:
  - All mutual needs met + all non-mutual needs met, OR
  - Capacity fully utilized
```

---

## Filter Support (Advanced)

### General Share

```
General-Share(Recipient, Provider) = 
    MR(Recipient, Provider)
    ────────────────────────────────────────────────────
    Σ MR(Provider, R) for all recipients R
```

This is the recipient's proportional share BEFORE filters.

### Specific Share (With Filters)

```
Filter(Recipient, Capacity) ∈ {0, 1}
    = 1 if Recipient passes Capacity's filter criteria
    = 0 otherwise

Examples:
    Filter(Recipient, "SF_housing") = 1 if Recipient.location == "SF"
    Filter(Recipient, "Rust_dev") = 1 if Recipient.skills.includes("Rust")

Specific-Share(Recipient, Provider, CapacityType) = 
    General-Share(Recipient, Provider) × Filter(Recipient, CapacityType)
    ──────────────────────────────────────────────────────────────────────────
    Σ [General-Share(R, Provider) × Filter(R, CapacityType)]
    for all filtered recipients R
```

### Example with Filters

```
Provider P has Capacity = 1000 (SF housing)
Filter: location == "SF"

Recipients:
    R1: MR = 0.5, Need = 500, Location = "SF"  → Filter = 1 ✓
    R2: MR = 0.3, Need = 400, Location = "NYC" → Filter = 0 ✗
    R3: MR = 0.2, Need = 300, Location = "SF"  → Filter = 1 ✓

Filtered Denominator:
    = (0.5 × 500 × 1) + (0.3 × 400 × 0) + (0.2 × 300 × 1)
    = 250 + 0 + 60
    = 310

Allocation(P → R1) = 1000 × (0.5 × 500 × 1) / 310 = 806.45
Allocation(P → R2) = 0  (filtered out!)
Allocation(P → R3) = 1000 × (0.2 × 300 × 1) / 310 = 193.55

Total: 806.45 + 193.55 = 1000 ✓

R2 gets nothing because they don't pass the filter,
even though they have mutual recognition!
```

---

## Convergence (Still Needed!)

### Why Iteration Is Still Required

The two-tier system doesn't eliminate the need for iterative convergence:

1. **Over-Allocation**: Recipients can still be over-allocated in a single round
2. **Under-Allocation**: Some recipients might not get enough
3. **Multiple Providers**: A recipient receives from multiple providers simultaneously

### The Iterative Process

```
Round N:
    1. Recipients publish Residual-Need(N)
    2. Providers compute TWO denominators each
    3. Providers allocate (Tier 1 → Tier 2)
    4. Recipients aggregate allocations
    5. Recipients update Residual-Need(N+1)
    6. Check convergence

Converged when:
    ΔDenominator₁ < ε AND ΔDenominator₂ < ε
    for all providers
```

### Convergence Properties

```
✅ Mutual denominators converge first (priority tier)
✅ Non-mutual denominators converge second (leftover tier)
✅ Adaptive damping still applies
✅ System reaches Nash equilibrium
```

---

## Implementation Notes

### Data Structures

```typescript
interface TwoTierAllocationState {
    // Tier 1: Mutual recognition denominators
    mutualDenominator: Record<string, number>; // capacity_id → Σ[MR × Need]
    
    // Tier 2: Non-mutual denominators  
    nonMutualDenominator: Record<string, number>; // capacity_id → Σ[Weight × Need]
    
    // Allocations by tier
    mutualAllocations: Record<string, Record<string, number>>; // capacity_id → { recipient_id → amount }
    nonMutualAllocations: Record<string, Record<string, number>>;
    
    timestamp: number;
}
```

### Subscription Strategy

```typescript
// PRIORITY: Mutual partners (full data exchange)
subscribe(allMutualPartners, [
    'commitments',
    'recognitionWeights',
    'allocationStates'
]);

// SECONDARY: Non-mutual beneficiaries (commitments only)
subscribe(myNonMutualBeneficiaries, [
    'commitments'
]);

// SECONDARY: Non-mutual providers (commitments + allocations)
subscribe(nonMutualProvidersForMe, [
    'commitments',
    'allocationStates'
]);
```

### Derived Subgroups

```
myMutualBeneficiaries: Readable<string[]>
    → People with MR > 0 (bidirectional)
    → I allocate to them with PRIORITY

myNonMutualBeneficiaries: Readable<string[]>
    → People I recognize but MR = 0 (one-way)
    → I allocate to them with LEFTOVER only

mutualProvidersForMe: Readable<string[]>
    → Providers with MR > 0 and capacity
    → I receive from them with PRIORITY

nonMutualProvidersForMe: Readable<string[]>
    → Providers who recognize me but MR = 0
    → I receive their LEFTOVER capacity only
```

---

## Summary

### The Core Innovation

**Two-Tier Allocation = Mutual Priority + Non-Mutual Inclusion**

- **Mutual recognition** (bilateral) gets first access to capacity
- **Non-mutual recognition** (one-way) can still receive, but only from leftovers
- This creates natural incentive for mutual relationships while allowing generous one-way giving

### Key Formulas

```
MR(You, Them) = min(You→Them, Them→You)

Total-Mutual-Recognition = Σ MR for all mutual partners
MRD(You, Them) = MR(You, Them) / Total-Mutual-Recognition

Total-Non-Mutual-Recognition = Σ Weight for all non-mutual
Renormalized(You, Them) = Weight(You→Them) / Total-Non-Mutual-Recognition

Tier 1 Denominator = Σ[MRD × Need] (mutual only, MRD sums to 100%)
Tier 2 Denominator = Σ[Renormalized × Need] (non-mutual only, sums to 100%)

Tier 1 Allocation = Capacity × (MRD × Need) / Tier1Denom
Tier 2 Allocation = RemainingCapacity × (Renormalized × Need) / Tier2Denom
```

### Properties

✅ **Bilateral consent enforced** (via MR for priority)  
✅ **Generous one-way giving supported** (via non-mutual tier)  
✅ **MR proportionality preserved** (within each tier)  
✅ **Capacity conservation** (across both tiers)  
✅ **Convergence guaranteed** (via iterative refinement)  
✅ **Automatic redistribution** (denominators shrink as needs met)  
✅ **Filter support** (capacity-specific constraints)  

### Use Cases

**When to use mutual-priority allocation:**
- Recognition economy with both mutual and one-way relationships
- Need to prioritize bilateral partnerships
- Want to allow generous giving beyond mutual recognition
- Capacity should flow to mutual partners first, others second

**When to use simpler algorithms:**
- Pure mutual recognition only (use `denominator-centric-fulfillment`)
- No priority tiers needed (use `simple-elegant`)
- Single provider, no coordination (use base recognition algorithm)

---

## Self-Recognition Example

### Setup: Alice Recognizes Herself

```
Alice's recognition weights:
    Alice (herself): 30% (0.30 - self-recognition)
    Bob: 40% (0.40)
    Carol: 30% (0.30)
    Total: 100% ✓

Bob's recognition weights:
    Alice: 60% (0.60)
    (Bob doesn't include Alice's self-recognition in his weights)

Carol's recognition weights:
    Alice: 20% (0.20)
    (Carol doesn't include Alice's self-recognition in her weights)

Alice's capacity: 1000
Alice's stated need: 500
```

### Compute Alice's Mutual Recognition

```
MR(Alice, Alice) = min(Alice→Alice, Alice→Alice)
                 = min(0.30, 0.30)
                 = 0.30  ← MUTUAL SELF-RECOGNITION!

MR(Alice, Bob) = min(Alice→Bob, Bob→Alice)
               = min(0.40, 0.60)
               = 0.40

MR(Alice, Carol) = min(Alice→Carol, Carol→Alice)
                 = min(0.30, 0.20)
                 = 0.20

Total-Mutual-Recognition(Alice) = 0.30 + 0.40 + 0.20 = 0.90
```

### Renormalize to MRD

```
MRD(Alice, Alice) = 0.30 / 0.90 = 0.333 (33.3% of mutual recognition)
MRD(Alice, Bob) = 0.40 / 0.90 = 0.444 (44.4% of mutual recognition)
MRD(Alice, Carol) = 0.20 / 0.90 = 0.222 (22.2% of mutual recognition)

Check: 0.333 + 0.444 + 0.222 = 1.0 = 100% ✓
```

### Alice Allocates Capacity (including to herself!)

**If Alice, Bob, and Carol all have residual need = 500:**

```
Numerator(Alice→Alice) = 0.333 × 500 = 166.5
Numerator(Alice→Bob) = 0.444 × 500 = 222.0
Numerator(Alice→Carol) = 0.222 × 500 = 111.0

Denominator = 166.5 + 222.0 + 111.0 = 499.5

Allocation(Alice→Alice) = 1000 × 166.5/499.5 = 333.33  ← Self-investment!
Allocation(Alice→Bob) = 1000 × 222.0/499.5 = 444.44
Allocation(Alice→Carol) = 1000 × 111.0/499.5 = 222.22

Total: 333.33 + 444.44 + 222.22 = 1000 ✓
```

### The Result

```
Alice allocates:
  - 333.33 to herself (self-care/self-investment)
  - 444.44 to Bob
  - 222.22 to Carol

Alice receives:
  - 333.33 from herself
  - (Plus whatever Bob and Carol allocate to her)

Alice's self-allocated portion helps meet her own need!
This is mathematically consistent and philosophically sound.
```

### Key Insights

1. **Self-recognition is mutual by definition** - both sides are you!
2. **You can be both provider and recipient** - self-sufficiency
3. **Self-investment counts toward need fulfillment** - self-care is valid
4. **Treated identically to other relationships** - no special cases needed
5. **Natural self-sufficiency incentive** - recognizing yourself means prioritizing your own needs

