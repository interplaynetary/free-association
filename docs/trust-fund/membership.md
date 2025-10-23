# Membership Module

## Membership Module: Mutual Recognition Density (MRD)

### Purpose

This module computes network membership based on the strength of mutual recognition relationships, providing a foundation for any system that requires determination of a participant set without centralized authority.

## Advantages of this Approach

* **No centralized authority deciding membership** - No person or committee determines who belongs; membership emerges purely from the computational analysis of mutual recognition patterns
* **No arbitrary membership criteria or gatekeeping** - No resumes, applications, interviews, or subjective approval processes; integration depth is the only criterion
* **Distributed determination of belonging** - Each participant's membership status arises from the aggregate of their bilateral recognition relationships, not from any single authority
* **No privileged historical position** - Past membership or founder status provides no inherent advantage; membership depends entirely on current relationship patterns
* **Scale-invariant fairness** - The threshold automatically adjusts as the network grows or shrinks, maintaining consistent standards regardless of network size
* **Protection against Sybil attacks** - Fake accounts cannot achieve genuine mutual recognition with real members, making identity manipulation ineffective
* **Resistance to collusion and gaming** - Small groups cannot easily manipulate membership without building genuine network-wide integration
* **Reciprocity requirement** - One-sided recognition contributes nothing; both parties must acknowledge the relationship's value
* **Transparent and auditable computation** - All calculations are visible, reproducible, and verifiable by any participant
* **Natural onboarding path** - New participants can see their progress toward membership and understand what integration requires
* **Self-correcting for disengagement** - Members who stop contributing naturally lose recognition and eventually membership without requiring removal decisions
* **No governance overhead** - Zero meetings, votes, or deliberations needed to determine membership status
* **Present-oriented assessment** - Based on current recognition patterns, not locked-in status or historical contributions
* **Tolerance for network clustering** - Sub-communities can form and maintain distinct patterns while remaining part of the larger network
* **Grief-resistant** - No negative votes or punishment mechanisms; based entirely on positive mutual recognition
* **Modular and composable** - Works independently but integrates cleanly with other recognition-based systems like resource allocation
* **Parameter experimentation** - Communities can tune the threshold based on their needs without changing the core mechanism
* **Early feedback for integration** - Participants can see their MRD score trajectory before reaching membership, understanding their path to inclusion
* **Prevention of celebrity dynamics** - Being widely recognized is insufficient; must also extend recognition to others, preventing one-way parasocial relationships

### Core Concept

**Membership is not granted—it emerges from the depth of reciprocal relationships.**

A person is a member when they have sufficient **mutual recognition density**: the total strength of their mutual recognition relationships, proportional to the network's natural connectivity.

### Mathematical Definition

#### Mutual Recognition

For any pair of participants (i, j):

```
MutualRecognition(i, j) = min(Recognition(i→j), Recognition(j→i))
```

Where:

* `Recognition(i→j)` = how much i recognizes j's contributions (percentage, 0-100%)
* `Recognition(j→i)` = how much j recognizes i's contributions (percentage, 0-100%)

**Key property:** Mutual recognition only exists when both people recognize each other. It is limited by the weaker recognition of the pair.

**Examples:**

* Alice→Bob: 20%, Bob→Alice: 15% → Mutual: 15%
* Alice→Charlie: 10%, Charlie→Alice: 0% → Mutual: 0% (one-sided, doesn't count)
* Bob→Charlie: 30%, Charlie→Bob: 30% → Mutual: 30%

#### Mutual Recognition Score

For participant i (relative to current members):

```
MutualRecognitionScore(i) = Σⱼ∈Members MutualRecognition(i, j)
    where j ≠ i (sum only across current members)
```

This is the **total strength** of all mutual recognition relationships participant i has.

**Example:**

```
Alice's relationships:
- Alice ↔ Bob: 15% mutual
- Alice ↔ Charlie: 10% mutual
- Alice ↔ Dave: 8% mutual

MutualRecognitionScore(Alice) = 15% + 10% + 8% = 33%
```

#### Network Average

```
AverageMRS = Σ MutualRecognitionScore(member) / TotalMembers
```

Calculated across all **current members** only.

**Why current members only?** The average represents "what normal integration looks like in this network." We calculate it from members because they define the network's connectivity baseline.

#### Mutual Recognition Density

For participant i:

```
MRD(i) = MutualRecognitionScore(i) / AverageMRS
```

**Translation:** How does your total mutual recognition compare to the average member's?

* MRD = 1.0: Exactly average integration
* MRD = 2.0: Twice as integrated as average
* MRD = 0.5: Half as integrated as average

#### Membership Status

```
IsMember(i) = MRD(i) ≥ threshold

Default threshold = 0.5
Epsilon comparison: treat MRD ≥ (threshold - ε), with ε = 1e-9
```

**Translation:** You're a member when your total mutual recognition is at least half of what the average member has.

### Algorithm

#### Step 1: Bootstrap (Initial Network)

```
Input: Seed participants (those who showed up to first dialogue session)

Process:
1. Seed participants submit initial recognition of each other
2. Calculate mutual recognition for all seed pairs
3. Calculate MutualRecognitionScore for each seed participant
4. Calculate AverageMRS across all seed participants
5. All seed participants become initial members

Note: With honest initial recognition, all seeds will have MRD ≈ 1.0
```

#### Step 2: Continuous Computation

```
For each computation cycle (e.g., weekly):
    
    1. Collect recognition data:
       Recognition(i→j) for all participant pairs
    
    2. Calculate mutual recognition for all pairs:
       For each pair (i, j):
           MutualRecognition(i, j) = min(Recognition(i→j), Recognition(j→i))
    
    3. Calculate mutual recognition scores for all participants (relative to current members):
       For each participant i:
           MutualRecognitionScore(i) = Σⱼ∈Members MutualRecognition(i, j)
    
    4. Calculate network average using current members:
       AverageMRS = sum(MRS(member)) / count(members)
    
    5. Calculate MRD for all participants:
       For each participant i:
           MRD(i) = MutualRecognitionScore(i) / AverageMRS
    
    6. Update membership status:
       For each participant i:
           IsMember(i) = (MRD(i) ≥ threshold)
    
    7. If membership changed, iterate once more for precision:
       - Recalculate AverageMRS with new member set
       - Recalculate MRD values
       - Update membership status
       - Usually stabilizes in 1-2 iterations
```

#### Step 3: Fixed-Point Iteration (For Precision)

```
Since membership depends on AverageMRS which depends on who is a member,
we iterate until stable:

membership_changed = True
iteration_count = 0
max_iterations = 5

while membership_changed and iteration_count < max_iterations:
    previous_members = current_members
    
    # Recalculate using current member set
    AverageMRS = calculate_average(current_members)
    
    # Update all MRD values
    for participant in all_participants:
        MRD(participant) = MRS(participant) / AverageMRS
        IsMember(participant) = MRD(participant) ≥ threshold
    
    # Check if membership changed
    membership_changed = (current_members != previous_members)
    iteration_count += 1

Typically converges in 1-2 iterations
```

### Complete Example

#### Initial Network (Week 0)

```
Seed participants: {Alice, Bob, Charlie}

Initial recognition:
Alice → Bob: 25%      Bob → Alice: 20%      → Mutual: 20%
Alice → Charlie: 15%  Charlie → Alice: 18%  → Mutual: 15%
Bob → Charlie: 22%    Charlie → Bob: 20%    → Mutual: 20%

Mutual Recognition Scores:
- Alice: 20% + 15% = 35%
- Bob: 20% + 20% = 40%
- Charlie: 15% + 20% = 35%

AverageMRS = (35% + 40% + 35%) / 3 = 36.67%

MRD values:
- Alice: 35% / 36.67% = 0.95
- Bob: 40% / 36.67% = 1.09
- Charlie: 35% / 36.67% = 0.95

All have MRD > 0.5 ✓
Members: {Alice, Bob, Charlie}
```

#### Week 4: Dave Joins and Contributes

```
Dave has been participating and building relationships:

Recognition patterns (new + existing):
Alice → Bob: 25%      Bob → Alice: 20%      → Mutual: 20%
Alice → Charlie: 15%  Charlie → Alice: 18%  → Mutual: 15%
Bob → Charlie: 22%    Charlie → Bob: 20%    → Mutual: 20%
Alice → Dave: 12%     Dave → Alice: 10%     → Mutual: 10%
Bob → Dave: 8%        Dave → Bob: 6%        → Mutual: 6%
Charlie → Dave: 0%    Dave → Charlie: 5%    → Mutual: 0% (one-sided)

Mutual Recognition Scores:
- Alice: 20% + 15% + 10% = 45%
- Bob: 20% + 20% + 6% = 46%
- Charlie: 15% + 20% + 0% = 35%
- Dave: 10% + 6% + 0% = 16%

Current members: {Alice, Bob, Charlie}
AverageMRS = (45% + 46% + 35%) / 3 = 42%

MRD values:
- Alice: 45% / 42% = 1.07 ✓
- Bob: 46% / 42% = 1.10 ✓
- Charlie: 35% / 42% = 0.83 ✓
- Dave: 16% / 42% = 0.38 ✗

Dave's MRD (0.38) < threshold (0.5)
Dave is NOT yet a member
```

#### Week 8: Dave Deepens Integration

```
Dave continues contributing, relationships deepen:

Recognition patterns:
Alice → Bob: 25%      Bob → Alice: 20%      → Mutual: 20%
Alice → Charlie: 15%  Charlie → Alice: 18%  → Mutual: 15%
Bob → Charlie: 22%    Charlie → Bob: 20%    → Mutual: 20%
Alice → Dave: 15%     Dave → Alice: 14%     → Mutual: 14%
Bob → Dave: 10%       Dave → Bob: 9%        → Mutual: 9%
Charlie → Dave: 8%    Dave → Charlie: 7%    → Mutual: 7%

Mutual Recognition Scores:
- Alice: 20% + 15% + 14% = 49%
- Bob: 20% + 20% + 9% = 49%
- Charlie: 15% + 20% + 7% = 42%
- Dave: 14% + 9% + 7% = 30%

Current members: {Alice, Bob, Charlie}
AverageMRS = (49% + 49% + 42%) / 3 = 46.67%

MRD values:
- Alice: 49% / 46.67% = 1.05 ✓
- Bob: 49% / 46.67% = 1.05 ✓
- Charlie: 42% / 46.67% = 0.90 ✓
- Dave: 30% / 46.67% = 0.64 ✓

Dave's MRD (0.64) > threshold (0.5) ✓
Dave becomes a member!

Membership changes, so we iterate once more:

New members: {Alice, Bob, Charlie, Dave}
AverageMRS = (49% + 49% + 42% + 30%) / 4 = 42.5%

Recalculated MRD:
- Alice: 49% / 42.5% = 1.15 ✓
- Bob: 49% / 42.5% = 1.15 ✓
- Charlie: 42% / 42.5% = 0.99 ✓
- Dave: 30% / 42.5% = 0.71 ✓

All still members, system has stabilized
Final members: {Alice, Bob, Charlie, Dave}
```

#### Week 12: Eve Attempts Many Weak Connections

```
Eve tries to join by getting minimal recognition from many people:

Eve's relationships:
- Eve ↔ Alice: 2% mutual
- Eve ↔ Bob: 3% mutual
- Eve ↔ Charlie: 2% mutual
- Eve ↔ Dave: 2% mutual
- Eve ↔ Frank (non-member): 1% mutual
- Eve ↔ Grace (non-member): 1% mutual

MutualRecognitionScore(Eve) = 2% + 3% + 2% + 2% + 1% + 1% = 11%

Current AverageMRS = 42.5%
MRD(Eve) = 11% / 42.5% = 0.26

0.26 < 0.5 threshold ✗
Eve is NOT a member

Despite having 6 connections, they're too weak
The system correctly identifies Eve as peripheral
```

#### Week 12: Frank Attempts Few Strong Connections

```
Frank focuses on deep collaboration with 2 people:

Frank's relationships:
- Frank ↔ Alice: 18% mutual
- Frank ↔ Bob: 16% mutual

MutualRecognitionScore(Frank) = 18% + 16% = 34%

Current AverageMRS = 42.5%
MRD(Frank) = 34% / 42.5% = 0.80

0.80 > 0.5 threshold ✓
Frank becomes a member!

With only 2 strong relationships, Frank is more integrated than Eve with 6 weak ones
The system correctly values depth over breadth
```

### Parameters

#### Threshold (Community-Tunable)

```
Default: 0.5
Recommended range: 0.3 to 0.8

Lower threshold (e.g., 0.3):
- More inclusive
- Easier to join
- Appropriate for: open networks, growth phases, loose collaboration
- Risk: May include peripherally-connected people

Higher threshold (e.g., 0.7):
- More exclusive  
- Requires deeper integration
- Appropriate for: tight-knit communities, resource-constrained, high-trust needs
- Risk: May exclude valuable contributors who are still building relationships

The community experiments with this parameter based on network health
```

#### Outgoing Recognition Budget (Enforced)

```
Per participant per cycle, the sum of outgoing recognitions is capped at 100%.
This normalization prevents inflation of totals and ensures comparability.
Formal constraint for each participant i: Σ_j Recognition(i→j) ≤ 100%.
```

#### Minimum Recognition Level (Optional)

```
Default: 0% (any positive mutual recognition counts)
Alternative: 1% or 2% (requires meaningful mutual recognition)

If set above 0%, mutual recognition below threshold is ignored:
  
  MutualRecognition(i,j) = 0  if min(i→j, j→i) < minimum
  MutualRecognition(i,j) = min(i→j, j→i)  otherwise

Use case: Filter out trivial/pro-forma recognition
Effect: Requires more substantial relationships
```

#### Computation Frequency

```
Default: Weekly
Alternatives: Daily, Bi-weekly, Monthly

Weekly recommended because:
- Long enough for contributions to demonstrate impact
- Short enough to adapt to changing patterns
- Matches typical collaboration rhythms (sprint cycles, weekly meetings)
- Not overwhelming for participants to update recognition

More frequent (daily): Better responsiveness, higher cognitive load
Less frequent (monthly): Lower burden, slower adaptation
```

### Interface

#### Inputs

```ts
type RecognitionData = {
    fromId: string;
    toId: string;
    percentage: number; // 0.0 to 100.0
    timestamp: Date;
};

type MembershipInput = {
    recognitionData: RecognitionData[];
    currentMembers: Set<string> | string[];
    threshold?: number; // default 0.5
    minimumRecognition?: number; // default 0.0
};
```

#### Outputs

```ts
interface MembershipOutput {
    membershipStatus: Record<string, boolean>; // participantId -> isMember
    mrdScores: Record<string, number>; // participantId -> MRD
    mutualRecognitionScores: Record<string, number>; // participantId -> total mutual recognition
    networkAverage: number; // current network average MRS
    threshold: number; // threshold used
    timestamp: Date;
    iterations: number; // iterations to converge
}
```

#### Query Methods

```ts
class MRDMembershipModule {
    computeMembership(recognitionData: RecognitionData[], currentMembers: Set<string> | string[]): MembershipOutput;
    isMember(participantId: string): boolean;
    getMrd(participantId: string): number;
    getMutualRecognitionScore(participantId: string): number;
    getNetworkAverage(): number;
    getMembershipHistory(participantId: string): Array<{ timestamp: Date; isMember: boolean }>;
    getMrdHistory(participantId: string): Array<{ timestamp: Date; mrd: number }>;
    getIntegrationBreakdown(participantId: string, recognitionData: RecognitionData[]): Record<string, number>;
    setThreshold(newThreshold: number): void;
    setMinimumRecognition(newMinimum: number): void;
}
```

### Properties Guaranteed

#### Mathematical

* ✓ **Scale-Invariant:** Threshold automatically adjusts as network grows/shrinks
* ✓ **Computable:** O(n²) for mutual recognition matrix, O(n) per participant
* ✓ **Deterministic:** Same recognition data → same membership, always
* ✓ **Continuous:** Small recognition changes → small MRD changes
* ✓ **Monotonic:** More mutual recognition → higher MRD (never hurts)

#### Social

* ✓ **Reciprocity-Requiring:** One-sided recognition contributes zero
* ✓ **Strength-Sensitive:** Deep relationships valued over many shallow ones
* ✓ **Integration-Measuring:** Reflects actual depth of network weaving
* ✓ **Topology-Aware:** Works with clustered and distributed networks
* ✓ **Fair Across Time:** Same integration level = same likelihood regardless of network size

#### Security

* ✓ **Sybil-Resistant:** Fake accounts can't achieve mutual recognition with real members
* ✓ **Collusion-Resistant:** Small groups can't easily game without genuine network integration
* ✓ **Griefing-Resistant:** No negative votes; based on positive recognition only
* ✓ **Inflation-Resistant:** If everyone recognizes everyone highly and genuinely, that's fine

#### Governance

* ✓ **No External Arbiter:** Purely computational, no human decision-maker
* ✓ **Transparent:** All calculations visible and reproducible
* ✓ **Auditable:** Can reconstruct historical membership from logs
* ✓ **Parameter-Tunable:** Community can experiment with threshold

#### Philosophical

* ✓ **Free-Association Aligned:** Membership emerges from relationships, not authority
* ✓ **Present-Oriented:** Based on current recognition patterns, not historical status
* ✓ **Relational:** Fundamentally about strength of connections between people
* ✓ **Non-Alienating:** No \[S2] governs \[S1's] membership; network computes it

### Integration with Collective Recognition

The MRD Membership Module is **independent but complementary** to Collective Recognition resource allocation:

```
┌─────────────────────────────────────────────┐
│          MRD Membership Module              │
│                                             │
│  Input: Recognition data                   │
│  Output: Set of current members            │
│  Computation: Weekly                       │
│                                             │
│  Determines: WHO is in the network         │
└─────────────────┬───────────────────────────┘
                  │
                  │ Member Set
                  ▼
┌─────────────────────────────────────────────┐
│      Collective Recognition Module          │
│                                             │
│  Input: Member set, Needs, Recognition     │
│  Output: Resource allocation priorities    │
│  Computation: Can be daily                 │
│                                             │
│  Determines: HOW resources flow to members │
└─────────────────────────────────────────────┘
```

#### Data Flow

```
Week N:
1. MRD Module receives recognition data
2. MRD Module computes: Members = {Alice, Bob, Charlie}
3. MRD Module outputs member set

Day N, N+1, N+2... (within week):
4. Collective Recognition receives:
   - Members: {Alice, Bob, Charlie}
   - Needs: {Alice: $1000, Bob: $500, Charlie: $200}
   - Recognition: {Alice↔Bob: 10%, Bob↔Charlie: 5%, etc.}
5. Collective Recognition computes daily:
   - Each member's share of collective recognition
   - Priority distribution
   - Resource allocation
6. Resources flow only to current members

Week N+1:
7. MRD Module recomputes membership (may change)
8. Collective Recognition adapts to new member set
```

#### Key Separation of Concerns

| Aspect         | MRD Module                         | Collective Recognition Module      |
| -------------- | ---------------------------------- | ---------------------------------- |
| **Question**   | Who is in the network?             | How much does each member receive? |
| **Input**      | Recognition patterns               | Member set + Needs + Recognition   |
| **Output**     | Boolean membership status          | Resource shares (percentages)      |
| **Frequency**  | Weekly (or less frequent)          | Daily (or more frequent)           |
| **Purpose**    | Network boundary definition        | Value distribution within boundary |
| **Governance** | None (computed from relationships) | None (computed from recognition)   |
| **Threshold**  | MRD ≥ 0.5 (epsilon-adjusted)       | Share > 0 (any recognition)        |

#### Why Separate Modules?

1. **Different timescales:** Membership changes slowly (weekly), resource allocation changes quickly (daily)
2. **Different purposes:** Who vs. How Much
3. **Modularity:** Can use MRD with other allocation systems, or other membership systems with Collective Recognition
4. **Clarity:** Each module has single responsibility
5. **Flexibility:** Can tune parameters independently

### Edge Cases

#### Case 1: Network of 2 People

```
Members: {Alice, Bob}
Alice ↔ Bob: 30% mutual

MutualRecognitionScore(Alice) = 30%
MutualRecognitionScore(Bob) = 30%
AverageMRS = 30%

MRD(Alice) = 30% / 30% = 1.0 ✓
MRD(Bob) = 30% / 30% = 1.0 ✓

Both are members
Works correctly
```

#### Case 2: Completely Disconnected Clusters

```
Cluster A: {Alice, Bob, Charlie} - internally connected
Cluster B: {Dave, Eve, Frank} - internally connected
Zero recognition between clusters

Within Cluster A:
- Each member: ~50% total mutual recognition
Within Cluster B:
- Each member: ~50% total mutual recognition

If calculated as one network:
AverageMRS = 50%
All have MRD = 1.0
All are members ✓

This is correct: Two separate communities coexisting
If they never interact, they might fork into separate networks
If they start collaborating, bridge recognition forms naturally
```

#### Case 3: Recognition Drops Below Threshold

```
Week N:
Alice: 45% mutual recognition, AverageMRS = 40%
MRD(Alice) = 1.13 ✓ Member

Week N+1:
Alice stops contributing, people reduce recognition:
Alice: 15% mutual recognition, AverageMRS = 40%  
MRD(Alice) = 0.375 ✗ Not a member

Alice's share in Collective Recognition → 0
Alice receives no resources this cycle
Alice can rebuild recognition and rejoin
```

#### Case 4: Everyone Recognizes Everyone Maximally

```
10 people, everyone genuinely values everyone's contribution highly
Each person → each other: 10% recognition
All mutual recognition: 10%

Each person's MutualRecognitionScore = 9 × 10% = 90%
AverageMRS = 90%
Everyone's MRD = 1.0

Everyone is a member ✓

This is correct: If recognition is genuine, high mutual recognition 
means everyone should be a member

If recognition is false (not based on real value), the Collective 
Recognition module handles correction:
- False recognition → fewer shares of useful capacities
- → less ability to self-actualize
- → natural correction pressure
```

#### Case 5: Network Growth Spurt

```
Week N: 10 members
Average member has 60% total mutual recognition
Threshold: Need 30% to join (0.5 × 60%)

Week N+10: 100 members join
Average member now has 80% total mutual recognition
Threshold: Need 40% to join (0.5 × 80%)

The threshold rose, but proportionally:
- Week N: Need mutual recognition with ~5 people at 6% each
- Week N+10: Need mutual recognition with ~7 people at 6% each

Slightly harder but reasonable scaling
```

#### Case 6: Specialist vs. Generalist

```
Alice (deep specialist):
- Works intensely with 2 people
- Alice ↔ Bob: 40% mutual
- Alice ↔ Charlie: 35% mutual
- Total: 75%

Dave (broad generalist):
- Works with 10 people
- Dave ↔ each: 5% mutual
- Total: 50%

Network AverageMRS = 62%

MRD(Alice) = 75% / 62% = 1.21 ✓ Member
MRD(Dave) = 50% / 62% = 0.81 ✓ Member

Both are members, but Alice has higher MRD
System correctly values both integration styles
```

#### Case 7: New Person Building Recognition

```
Week 1: Grace joins
Grace ↔ Alice: 2% mutual
MRS(Grace) = 2%, AverageMRS = 40%
MRD(Grace) = 0.05 → Not a member

Week 2: Grace contributes more
Grace ↔ Alice: 5% mutual
Grace ↔ Bob: 3% mutual
MRS(Grace) = 8%, AverageMRS = 40%
MRD(Grace) = 0.20 → Not a member (but growing!)

Week 4: Grace is well-integrated
Grace ↔ Alice: 10% mutual
Grace ↔ Bob: 8% mutual
Grace ↔ Charlie: 6% mutual
MRS(Grace) = 24%, AverageMRS = 40%
MRD(Grace) = 0.60 → Member! ✓

Grace can see her progress: 0.05 → 0.20 → 0.60
System provides perceivable path to membership
```

### Implementation Notes

#### Performance

```
Time Complexity:
- Mutual recognition matrix: O(n²) to compute all pairs
- MRD per participant: O(n) to sum mutual recognitions
- Full computation: O(n²) per cycle

Space Complexity:
- Recognition data: O(n²) for all pairs
- Can use sparse matrix (most pairs have 0 recognition)

For large networks (>10,000 participants):
- Use sparse matrix storage
- Cache mutual recognition calculations
- Incremental updates (only recalculate changed pairs)
- Consider sampling for approximate MRD in very large networks
```

#### Computation Consistency

```
Best practices:
1. Computation cycles are atomic transactions
2. All recognition data from same time window
3. Membership changes take effect at cycle boundaries
4. Historical data preserved for auditability
5. Fixed-point iteration logged for transparency

Handling concurrent updates:
- Lock during computation cycle
- Queue recognition updates for next cycle
- Clear start/end boundaries for each computation
```

#### Transparency Requirements

```
For every computation, publish:
1. Recognition data used (anonymized if needed)
2. Mutual recognition matrix
3. All MRD scores
4. Threshold used
5. Who became/ceased being members
6. Iteration count and convergence info

Members can verify:
- Their own MRD calculation
- Network average calculation
- Threshold application
- Complete audit trail
```

### Open Questions for Community Tuning

#### 1. Threshold Value

```
Start with: 0.5 (need half the average mutual recognition)

Adjust based on:
- Does the network feel too exclusive? → Lower threshold (e.g., 0.4)
- Does the network feel too diffuse? → Raise threshold (e.g., 0.6)
- Are valuable contributors excluded? → Review individual cases
- Are peripheral people included? → May be appropriate if recognized

This is an experimental parameter, not a governance decision
```

#### 2. Minimum Recognition Filter

```
Start with: 0% (any positive mutual recognition counts)

Consider raising if:
- Lots of trivial/pro-forma recognition
- Want to emphasize substantial relationships
- Filter noise in large networks

Experiment: Try 1% or 2% minimum and observe effects
```

#### 3. Computation Frequency

```
Start with: Weekly

Adjust based on:
- How quickly relationships form/fade in your community
- Cognitive load on participants
- Resource allocation frequency needs
- Growth rate of network

Faster-growing networks may want more frequent computation
Stable networks may want less frequent
```

#### 5. Visualization and Feedback

```
What should participants see?
- Their own MRD score and trajectory
- How close they are to threshold
- Breakdown of mutual recognition by person
- Network health metrics
- Comparison to network average

Balance transparency with information overload
```

### Community Health Metrics

The MRD system also provides diagnostics for network health:

```ts
interface HealthMetrics {
    memberCount: number;
    participantCount: number;
    membershipRate: number;
    mrsStdDev: number;
    mrdStdDev: number;
    mrsMedian: number;
    mrdMedian: number;
    concentration: number;
    nearThresholdCount: number;
    stronglyIntegratedCount: number;
    peripheralCount: number;
}
```

### Summary

The MRD Membership Module provides:

#### What It Does

* **Computes membership** based on strength of mutual recognition relationships
* **No arbitrary decisions** - purely mathematical from recognition patterns
* **Scale-invariant** - works from 3 to 3,000 participants
* **Transparent and auditable** - all calculations visible

#### What It Values

* **Reciprocity** - one-sided recognition doesn't count
* **Present integration** - based on current patterns, not historical status
* **Relationship quality** - total mutual recognition strength

#### What It Prevents

* **Governance capture** - no one decides membership
* **Sybil attacks** - fake accounts can't achieve mutual recognition
* **Celebrity dynamics** - being recognized isn't enough; must recognize back
* **Temporal domination** - past membership doesn't guarantee future membership

#### What It Enables

* **Natural growth** - new members join by building genuine relationships
* **Self-correction** - members who stop contributing naturally lose membership
* **Network clustering** - sub-communities can form while maintaining coherence
* **Modular composition** - works with any recognition-based resource allocation

#### Integration Point

```
MRD Module Output (weekly):
  → Member Set: {Alice, Bob, Charlie, Dave}

Collective Recognition Input (daily):
  ← Uses member set to determine who receives resources
```

The modules work together but remain independent, maintaining clean separation of concerns:

* **MRD**: Who is in the network?
* **Collective Recognition**: How much does each member receive?

Both are governance-free, computed from recognition patterns, and aligned with free-association principles.
