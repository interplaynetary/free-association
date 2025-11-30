# Protocol Parameters: Emergent Allocation Framework

**Document:** Coalition Participation Framework v0.2.0  
**Purpose:** Define parameters for emergent, recognition-based proceeds allocation  
**Architecture:** Bottom-up (no predetermined percentages)  
**Date:** November 29, 2025

***

## Core Principle

**Proceeds allocation emerges from collective recognition patterns, not predetermined budgets.**

The collective tree IS the allocation structure. Members recognize needs across all categories (means of production, common needs, individual consumption). The protocol aggregates these recognitions computationally. Resources flow proportionally to collective recognition.

**No voting on percentages. No budget committees. Pure computation from recognition.**

***

## I. Mathematical Foundation: The MS Formula

### 1.1 Single Consistent Formula

From protocolv6.mmd and collective-tree.svelte.ts:

```typescript
MS(provider, recipient) = MR × recipient_share

Where:
  MR = Mutual Recognition = min(recognition_A→B, recognition_B→A)
  recipient_share = proportion of provider's capacity allocated to recipient

Bootstrap (no satisfaction data):
  recipient_share = recognition points / total recognition

Learned (with satisfaction data):
  recipient_share = (recognition × satisfaction) / Σ(recognition × satisfaction)
```

**This formula allocates BOTH external capacity AND internal proceeds.**

### 1.2 Collective as Provider

When allocating collective proceeds:

```typescript
provider = collective (total social product = capacity)
recipients = all nodes in collective tree (needs + members)

For each node N in collective tree:
  MS(collective, N) = MR(collective, N) × N_share
  
  Allocation(N) = Total_Proceeds × (MS(collective, N) / Σ_All_MS)
```

**Proportions emerge from recognition, not predetermined splits.**

***

## II. Collective Tree Structure

### 2.1 Need Declaration (No Predetermined Categories)

Members declare needs organically. Categories emerge from practice, not predetermined taxonomies.

```typescript
ALLOW_CUSTOM_TAGS: boolean = true
  Description: Members can tag needs however makes sense to them
  Examples: ["tools", "healthcare", "groceries", "infrastructure", "education", "urgent"]
  
  No required taxonomy.
  No predetermined Stage 1/2/3 labels.
  Just free-form need declarations with optional tags.

ENABLE_TAG_SUGGESTIONS: boolean = true
  Description: System can suggest common tags based on observed patterns
  Example: After seeing "lathe", "drill press", "tools" frequently co-occur,
           suggest "tools" tag for similar needs
  
  Suggestions emerge from collective practice, not imposed framework.
```

**Purpose:** Members declare needs naturally. Patterns and categories emerge from what people actually declare. System can observe and suggest patterns, but doesn't enforce categories.

### 2.2 Tree Aggregation Mode

How collective recognition is aggregated across the tree (from collective-tree.svelte.ts):

```typescript
ALLOCATION_AGGREGATION_MODE: enum = "tree-wide"
  Options: ["tree-wide", "single-node", "weighted-path"]
  Adjustable via Decider
  
  tree-wide (default):
    - All nodes weighted equally by their recognition
    - Democratic: every recognized need matters equally
    - Formula: Σ(node_weight × contributor_weight) for all nodes
  
  single-node:
    - Allocate based on one specific decision node
    - Use case: Special-purpose allocation for one decision
    - Formula: Use only target node's contributor_weights
  
  weighted-path:
    - Weight by depth (root nodes prioritized)
    - Depth_weight = 1 / (depth + 1)
    - Use case: Prioritize high-level collective decisions
    - Formula: Σ(depth_weight × contributor_weight)

// Most collectives should use "tree-wide" (democratic)
```

### 2.3 Recognition Precision

```typescript
RECOGNITION_MINIMUM_PERCENTAGE: number = 0.0
  Range: [0.0, 5.0]
  Units: Percentage
  Description: Minimum recognition that can be assigned to any node
  Default: 0.0 (no minimum, can ignore entirely)

RECOGNITION_MAXIMUM_PERCENTAGE: number = 100.0
  Range: [30.0, 100.0]
  Units: Percentage  
  Description: Maximum recognition for any single node
  Default: 100.0 (can give all to one thing if desired)

RECOGNITION_PRECISION: number = 0.1
  Options: [1.0, 0.5, 0.1, 0.01]
  Units: Percentage
  Description: Smallest increment (e.g., 0.1 = can do 5.3%, not 5.37%)
  Default: 0.1

RECOGNITION_UPDATE_FREQUENCY: enum = "continuous"
  Options: ["continuous", "weekly", "monthly"]
  Description: How often members can update recognition
  Default: "continuous" (update anytime)
```

***

## III. Safety Constraints (Minimums Only)

### 3.1 Observational Monitoring (Optional)

Optionally monitor patterns in declared needs for informational purposes:

```typescript
ENABLE_PATTERN_MONITORING: boolean = false
  Description: Whether to monitor and report on observed patterns
  Default: false (no monitoring, pure emergence)

PATTERN_MONITORING_SUGGESTIONS: Record<string, MonitorConfig> = {
  "tools_and_infrastructure": {
    tags: ["tools", "infrastructure", "equipment", "replacement"],
    min_suggested_percentage: 0.10,
    message: "Observed: <10% of needs related to tools/infrastructure. Consider if maintenance needs are adequately declared."
  },
  "collective_reserves": {
    tags: ["reserve", "risk", "emergency", "insurance"],
    min_suggested_percentage: 0.05,
    message: "Observed: <5% of needs for emergency reserves. Consider collective risk hedging."
  }
}
  Description: Optional monitoring configs that can suggest (not enforce) patterns
  Enforcement: Information only - displays suggestions, never overrides tree
  Adjustable via Decider

This is OPTIONAL. Most collectives should disable monitoring and trust pure emergence.
```

### 3.2 Individual Subsistence Floor

Ensure everyone receives minimum regardless of recognition:

```typescript
MINIMUM_INDIVIDUAL_ALLOCATION_USD: number = 2000.0
  Range: [500.0, 10000.0]
  Units: USD per month (adjust for other currencies/periods)
  Adjustable via Decider
  Description: Minimum monthly proceeds per member

MINIMUM_ALLOCATION_LOCATION_MULTIPLIERS: Record<string, number> = {
  "US": 1.0,
  "CH": 1.5,   // Switzerland higher cost of living
  "KE": 0.4,   // Kenya lower cost of living
  "UG": 0.35,
  // ... per country ISO code
}
  Description: Location-based cost-of-living adjustments

MINIMUM_ALLOCATION_ENFORCEMENT: enum = "guaranteed_first"
  Options: ["guaranteed_first", "proportional_reduction", "skip_if_insufficient"]
  Adjustable via Decider
  
  guaranteed_first (default):
    - Pay all minimums first
    - Distribute remainder by MS formula
    - If insufficient: reduce all proportionally
  
  proportional_reduction:
    - If total < Σ(minimums): everyone gets minimum × (Available / Σ minimums)
  
  skip_if_insufficient:
    - If insufficient: ignore minimums, use pure MS formula

Example:
  5 members, $2K minimum each = $10K needed
  Total proceeds = $100K
  → Pay $10K minimums first
  → Distribute $90K by MS formula
  → Everyone gets ≥$2K, recognized contributors get more
```

***

## IV. Filtering and Consensus

### 4.1 Recognition Threshold

Filter out nodes with insufficient collective support:

```typescript
MINIMUM_COLLECTIVE_RECOGNITION_THRESHOLD: number = 0.05
  Range: [0.01, 0.20]
  Units: Fraction (0.05 = 5% of collective tree aggregate weight)
  Adjustable via Decider
  Description: Nodes below this threshold filtered from allocation
  
  Implementation:
    filteredTree = filterTreeByMinimumPercentage(
      collectiveTree, 
      MINIMUM_COLLECTIVE_RECOGNITION_THRESHOLD
    )
    
  Effect:
    - Node with 3% aggregate recognition → filtered out
    - Node with 8% aggregate recognition → receives allocation
    - Prevents tiny, poorly-recognized needs from fragmenting proceeds

Example:
  Healthcare fund: 22% aggregate recognition → KEEP
  Alice's hobby: 2% aggregate recognition → FILTER
  Bob's rent: 18% aggregate recognition → KEEP
```

### 4.2 Contributor Quorum

Require minimum number of members recognizing a need:

```typescript
MINIMUM_QUORUM_FOR_ALLOCATION: number = 2
  Range: [1, 10]
  Units: Number of members
  Adjustable via Decider
  Description: Need must be recognized by ≥N members to receive proceeds
  
  Implementation:
    filteredTree = filterTreeByMinimumQuorum(
      collectiveTree,
      MINIMUM_QUORUM_FOR_ALLOCATION
    )
    
  Effect:
    - Prevents single member from unilaterally capturing proceeds
    - Ensures collective legitimacy of needs
    - Node recognized by 1 member only → filtered
    - Node recognized by 3 members → eligible for allocation

Example:
  Healthcare: recognized by 5 members → KEEP
  Alice's personal need: recognized by Alice only → FILTER (quorum = 2)
  New tool: recognized by 3 members → KEEP
```

### 4.3 Multi-Criteria Filtering

Combine multiple filters with logical AND:

```typescript
FILTER_CONFIG_COMBINED: UnifiedFilterConfig = {
  min_percentage: 0.05,              // ≥5% collective recognition
  min_contributor_count: 2,          // ≥2 members recognize it
  preserve_paths: true,              // Keep structural parent nodes
  
  // Optional: Additional filters
  min_total_recognition?: number,    // Raw recognition sum
  min_average_mrd?: number,          // Average mutual recognition density
  logic_rule?: JsonLogicRule,        // Custom JSON Logic
  custom_filter?: Function           // Custom function
}

// Apply all filters:
filteredTree = applyUnifiedFilter(collectiveTree, nodeRecognition, FILTER_CONFIG_COMBINED)

// Only nodes passing ALL criteria receive allocation
```

***

## V. Satisfaction Learning

### 5.1 Satisfaction Weighting

From protocolv6.mmd - satisfaction adjusts shares over time:

```typescript
SATISFACTION_LEARNING_ENABLED: boolean = true
  Description: Whether satisfaction ratings adjust future allocations
  Default: true (learning enabled)

SATISFACTION_WEIGHT_FORMULA: string = "(recognition × satisfaction) / Σ"
  Description: How satisfaction combines with recognition
  
  Bootstrap (Cycle 1, no satisfaction data):
    recipient_share = recognition / Σ recognition
  
  Learned (Cycle 2+, with satisfaction data):
    effective_recognition = recognition × satisfaction
    recipient_share = effective_recognition / Σ effective_recognition
  
  Effect:
    - High satisfaction → increased share → more allocation
    - Low satisfaction → decreased share → less allocation
    - System learns what actually works, not just beliefs

SATISFACTION_RATING_SCALE: [number, number] = [0.0, 1.0]
  Description: Range for satisfaction ratings
  Default: 0.0 (completely unsatisfied) to 1.0 (perfectly satisfied)

SATISFACTION_DECAY_ENABLED: boolean = false
  Description: Whether old satisfaction ratings decay over time
  Default: false (all historical ratings weighted equally)

SATISFACTION_DECAY_HALFLIFE_DAYS: number = 180
  Range: [30, 730]
  Description: Days for satisfaction weight to decay to 50%
  Only applies if SATISFACTION_DECAY_ENABLED = true
```

### 5.2 Provider Quality Signals

Providers observe their aggregate satisfaction:

```typescript
PROVIDER_CAPACITY_SATISFACTION_FORMULA: string = "Σ(accepted × satisfaction) / Σ accepted"
  Description: Weighted average of satisfaction for provider's allocations
  
  Example:
    Carol allocated:
      - 50 units to Alice, satisfaction = 0.85
      - 45 units to Bob, satisfaction = 0.40
    
    Carol's capacity_satisfaction = (50×0.85 + 45×0.40) / (50+45)
                                  = (42.5 + 18) / 95
                                  = 0.637
    
    Interpretation: Carol's offerings are 63.7% satisfactory
    Signal: Improve quality to increase future demand

// Providers can observe and adjust based on this feedback
// No enforcement - just information for learning
```

***

## VI. Convergence and Stability

### 6.1 Oscillation Dampening

From protocolv6.mmd lines 236-290:

```typescript
OSCILLATION_DETECTION_ENABLED: boolean = true
  Description: Whether to detect and dampen need oscillations
  Default: true

OSCILLATION_PATTERN: string = "N(t-2) > 0 AND N(t-1) = 0 AND N(t) ≈ N(t-2)"
  Description: Pattern that triggers dampening
  Detection: Need returns to previous level after being satisfied

OSCILLATION_THRESHOLD_PERCENTAGE: number = 0.2
  Range: [0.1, 0.5]
  Description: Similarity threshold for oscillation detection
  Formula: |N(t) - N(t-2)| / N(t-2) < threshold
  Example: 0.2 = within 20% of previous level

DAMPING_FACTOR: number = 0.7
  Range: [0.5, 1.0]
  Adjustable via Decider
  Description: Reduction applied to oscillating needs
  Formula: activeNeed = declaredNeed × DAMPING_FACTOR
  
  Effect:
    - Prevents rapid swings in allocation
    - Smoother convergence
    - Still allows over-allocation (expected!)
    - Reduces volatility

Example:
  Alice: 100 → 0 → 100 (oscillation detected!)
  → activeNeed = 100 × 0.7 = 70
  → Providers allocate based on 70, not 100
  → Reduces over-allocation from 115 to 140 (improvement)
```

### 6.2 Convergence Monitoring

```typescript
CONVERGENCE_METRIC: string = "Σ all needs over time"
  Description: Total network needs should decrease monotonically
  
  Guarantee:
    Total_Needs(t+1) = Σ max(0, Need_i(t) - Accepted_i(t))
                     ≤ Σ Need_i(t)
                     = Total_Needs(t)
  
  Properties:
    - Monotonic decrease
    - Eventually converges to equilibrium
    - Typical convergence: 5-10 cycles

CONVERGENCE_ALERT_ENABLED: boolean = true
  Description: Alert if oscillation persists beyond threshold

CONVERGENCE_ALERT_CYCLES: number = 15
  Range: [5, 50]
  Description: Alert if not converged after N cycles
  Suggests: Tune DAMPING_FACTOR or review need declarations
```

***

## VII. Allocation Execution

### 7.1 Distribution Computation Schedule

```typescript
PROCEEDS_DISTRIBUTION_FREQUENCY: enum = "monthly"
  Options: ["weekly", "biweekly", "monthly", "quarterly"]
  Adjustable via Decider
  Description: How often proceeds are calculated and distributed

PROCEEDS_ACCOUNTING_PERIOD: enum = "calendar_month"
  Options: ["calendar_week", "calendar_month", "rolling_30days", "custom"]
  Description: Period over which total social product is measured

PROCEEDS_DISTRIBUTION_DAY: number = 1
  Range: [1, 28]
  Description: Day of month for monthly distribution (1 = first day)
  
  Example:
    Monthly distribution on the 1st:
    - Dec 1: Collect November total social product
    - Dec 1: Compute collective tree allocations
    - Dec 1: Apply filters, minimums, adjustments
    - Dec 1: Execute transfers to members/funds
```

### 7.2 Advance Payments

```typescript
PROCEEDS_ADVANCE_ALLOWED: boolean = true
  Description: Whether members can request advances against future allocation

PROCEEDS_ADVANCE_MAX_PERCENTAGE: number = 50.0
  Range: [0.0, 90.0]
  Description: Maximum percentage of expected allocation that can be advanced
  
  Example:
    Alice's trailing average allocation: $3,000/month
    Advance max: 50%
    Alice can request: up to $1,500 advance against next distribution
    Next distribution: Alice receives $3,000 - $1,500 = $1,500 (net)
```

***

## VIII. Asset and Ownership Parameters

### 8.1 Means of Production Ownership

```typescript
ASSET_LEGAL_OWNER: enum = "administrative_entity"
  Options: ["administrative_entity", "members_jointly", "trust"]
  Description: Legal owner of collectively-held means of production
  Jurisdiction-specific

ASSET_TRACKING_METHOD: enum = "depreciation_schedule"
  Options: ["depreciation_schedule", "current_value", "replacement_cost"]
  Description: How assets are valued and tracked
  
  depreciation_schedule:
    - Track purchase price and depreciation over time
    - Standard accounting method
    
  current_value:
    - Track market value (requires periodic appraisal)
    
  replacement_cost:
    - Track cost to replace with equivalent capacity

ASSET_CONTRIBUTION_RECOGNITION_ENABLED: boolean = true
  Description: Whether contributing capital assets increases recognition

ASSET_CONTRIBUTION_RECOGNITION_METHOD: enum = "amortized"
  Options: ["immediate", "amortized", "proportional_to_use"]
  Description: How capital contributions are recognized
  
  amortized:
    - Spread recognition over asset lifespan
    - Example: $50K equipment, 5-year life = $833/month credit
    
  immediate:
    - Full recognition credit at contribution time
    - Risk: One-time boost, then fades
    
  proportional_to_use:
    - Recognition based on how much asset is actually used
    - Fair but harder to track

ASSET_CONTRIBUTION_RECOGNITION_PERIOD_MONTHS: number = 36
  Range: [12, 120]
  Description: Period over which amortized recognition is distributed
```

### 8.2 Member Exit and Asset Claims

```typescript
MEMBER_EXIT_ASSET_CLAIM: enum = "proportional_value"
  Options: ["none", "original_contribution", "proportional_value", "negotiated"]
  Adjustable via Decider
  Description: What departing member can claim regarding assets
  
  none:
    - No claim on assets when leaving
    - All contributions become collective property
    
  original_contribution:
    - Return original contributed value (depreciated)
    
  proportional_value:
    - Share of current collective asset value
    - Proportional to contributed value and time
    
  negotiated:
    - Case-by-case negotiation with collective

MEMBER_EXIT_PAYOUT_ENABLED: boolean = true
  Description: Whether departing members receive payout

MEMBER_EXIT_PAYOUT_CALCULATION: enum = "trailing_average_allocation"
  Options: ["trailing_average_allocation", "risk_reserve_share", "none"]
  Description: Method for calculating exit payout
  
  trailing_average_allocation:
    - Average of last N months' individual allocations
    - Multiplied by exit payout period
    
  risk_reserve_share:
    - Proportional share of risk reserve fund
    - Based on cumulative contributions

MEMBER_EXIT_PAYOUT_MONTHS: number = 3.0
  Range: [0.0, 12.0]
  Description: Months of trailing average paid on exit
  
  Example:
    Bob exits, trailing 6-month average = $2,500/month
    Payout months = 3
    Exit payout = $2,500 × 3 = $7,500
```

### 8.3 Intellectual Property

```typescript
IP_DEFAULT_OWNER: enum = "collective"
  Options: ["collective", "creator", "public_domain"]
  Description: Default legal owner of IP created during collective activity

IP_DEFAULT_LICENSE: enum = "collective_proprietary"
  Options: [
    "collective_proprietary",
    "creative_commons_by_sa",
    "gpl_v3",
    "apache_2.0",
    "public_domain"
  ]
  Description: Default license for collectively-created IP

IP_MEMBER_USAGE_AFTER_EXIT: enum = "non_commercial"
  Options: ["full", "non_commercial", "none"]
  Description: Rights departing members retain to use collective IP
  
  full: Can use for any purpose (including commercial)
  non_commercial: Can use but not sell/profit
  none: No usage rights after exit

IP_COMMERCIAL_REVENUE_ALLOCATION: enum = "proceeds"
  Options: ["proceeds", "creator_direct", "split"]
  Description: How revenue from IP licensing is handled
  
  proceeds:
    - All revenue goes into total social product
    - Allocated via MS formula like other proceeds
    
  creator_direct:
    - Goes directly to creator(s)
    - Bypasses collective allocation
    
  split:
    - Percentage to creator, percentage to collective
```

***

## IX. Membership and Recognition Parameters

### 9.1 MRD Computation

```typescript
MEMBERSHIP_THRESHOLD: number = 0.5
  Range: [0.0, 2.0]
  Adjustable via Decider
  Description: Minimum MRD score required for membership
  
  Formula:
    MRS(Alice) = Σ MutualRecognition(Alice, Member) for all current members
    AverageMRS = Σ MRS(Member) / Count(Members)
    MRD(Alice) = MRS(Alice) / AverageMRS
    
    IsMember(Alice) = MRD(Alice) ≥ MEMBERSHIP_THRESHOLD

MRD_COMPUTATION_FREQUENCY: enum = "weekly"
  Options: ["daily", "weekly", "biweekly", "monthly"]
  Description: How often MRD scores are recomputed

MRD_COMPUTATION_DAY: enum = "monday"
  Options: ["monday", "tuesday", ..., "sunday"]
  Description: Day of week for MRD computation (if weekly/biweekly)

MRD_COMPUTATION_TIME_UTC: string = "00:00"
  Format: "HH:MM" in UTC
  Description: Time of day for MRD computation
```

### 9.2 Bootstrap Parameters

```typescript
BOOTSTRAP_SEED_MEMBER_COUNT: number = 3
  Range: [2, 10]
  Description: Minimum founding members to bootstrap collective

BOOTSTRAP_INITIAL_MRD: number = 1.0
  Range: [0.5, 2.0]
  Description: Initial MRD assigned to seed members during bootstrap

BOOTSTRAP_MINIMUM_MUTUAL_RECOGNITION: number = 5.0
  Range: [1.0, 20.0]
  Units: Percentage
  Description: Minimum mutual recognition between seed members

BOOTSTRAP_DURATION_WEEKS: number = 4
  Range: [1, 12]
  Description: Weeks of fixed MRD before normal computation begins
```

***

## X. Compliance and Legal Parameters

### 10.1 Compliance Filters

```typescript
COMPLIANCE_FILTER_ENABLED: boolean = true
  Description: Whether to apply compliance filters to allocations

COMPLIANCE_FILTER_SANCTIONS_ACTION: enum = "block_all"
  Options: ["block_all", "manual_review", "reduced_cap"]
  Description: Action when member appears on sanctions list

COMPLIANCE_FILTER_KYC_FAILED_ACTION: enum = "block_all"
  Options: ["block_all", "reduced_cap", "manual_review"]
  Description: Action when member fails KYC verification

COMPLIANCE_FILTER_JURISDICTION_DEFAULT_CAP_USD: number = 10000.0
  Range: [1000.0, 1000000.0]
  Description: Default cap for jurisdictions without specific limits

COMPLIANCE_FILTER_RISK_LEVEL_CAPS_USD: Record<string, number> = {
  "low": 100000.0,
  "medium": 25000.0,
  "high": 5000.0,
  "very_high": 1000.0
}
  Description: Allocation caps by risk level

COMPLIANCE_FILTER_REFRESH_FREQUENCY: enum = "daily"
  Options: ["realtime", "hourly", "daily", "weekly"]
  Description: How often compliance filters are updated

COMPLIANCE_FILTER_MANUAL_OVERRIDE_ALLOWED: boolean = false
  Description: Whether admins can override compliance filters
  Default: false (compliance is computational, not discretionary)
```

### 10.2 Tax Classification

```typescript
TAX_STAGE3_CLASSIFICATION: enum = "self_employment"
  Options: [
    "wages_w2",           // US: W-2 wages
    "self_employment_1099", // US: 1099 self-employment
    "dividends",          // US: Dividend income
    "distributions",      // Partnership/LLC distributions
    "jurisdiction_specific"
  ]
  Description: Tax classification of individual consumption allocations
  Jurisdiction-specific, consult tax advisor

TAX_REPORTING_ENTITY: enum = "collective"
  Options: ["collective", "individual", "both"]
  Description: Who is responsible for tax reporting

TAX_WITHHOLDING_ENABLED: boolean = false
  Description: Whether collective withholds taxes from allocations

TAX_WITHHOLDING_PERCENTAGE: number = 25.0
  Range: [0.0, 50.0]
  Description: Percentage withheld if withholding enabled

TAX_YEAR_END_MONTH: number = 12
  Range: [1, 12]
  Description: End month of tax year (12 = December, calendar year)
```

***

## XI. Governance and Decider Parameters

### 11.1 Decider Process

```typescript
DECIDER_PROPOSAL_PHASE_DAYS: number = 7
  Range: [3, 30]
  Adjustable via Decider (meta!)
  Description: Days for proposal submission phase

DECIDER_CHALLENGE_PHASE_DAYS: number = 7
  Range: [3, 30]
  Description: Days for challenge phase

DECIDER_DISCUSSION_PHASE_DAYS: number = 7
  Range: [3, 30]
  Description: Days for discussion phase

DECIDER_IMPROVEMENT_PHASE_DAYS: number = 7
  Range: [3, 30]
  Description: Days for proposal improvement phase

DECIDER_SUPPORT_PHASE_DAYS: number = 7
  Range: [3, 30]
  Description: Days for support distribution phase

DECIDER_PARTICIPATION_QUORUM_PERCENTAGE: number = 50.0
  Range: [20.0, 100.0]
  Description: Percentage of members who must participate for valid decision

DECIDER_WINNING_THRESHOLD_PERCENTAGE: number = 50.0
  Range: [40.0, 75.0]
  Description: Percentage of weighted support needed to win

DECIDER_WINNING_THRESHOLD_CONSTITUTIONAL: number = 66.0
  Range: [60.0, 90.0]
  Description: Percentage needed for constitutional changes

DECIDER_NO_WINNER_ACTION: enum = "status_quo"
  Options: ["status_quo", "repeat_process", "admin_decides"]
  Description: What happens if no proposal reaches threshold
  Default: "status_quo" (no change)
```

### 11.2 Admin Selection

```typescript
ADMIN_COUNT: number = 3
  Range: [1, 7]
  Adjustable via Decider
  Description: Number of admin positions

ADMIN_TERM_LENGTH_MONTHS: number = 12
  Range: [3, 36]
  Description: Term length for admin positions

ADMIN_TERM_RENEWABLE: boolean = true
  Description: Whether admins can serve consecutive terms

ADMIN_SELECTION_FREQUENCY: enum = "quarterly"
  Options: ["monthly", "quarterly", "annually", "at_term_end"]
  Description: How often admin selection is reviewed

ADMIN_REMOVAL_MRD_THRESHOLD: number = 0.5
  Description: If admin's MRD falls below this, position is re-offered

ADMIN_DECLINE_PENALTY: boolean = false
  Description: Whether declining admin position affects recognition
  Default: false (no penalty)
```

***

## XII. Network and Coalition Parameters

### 12.1 Inter-Collective Recognition

```typescript
INTER_COLLECTIVE_RECOGNITION_ENABLED: boolean = true
  Description: Whether members can recognize members of other collectives

INTER_COLLECTIVE_RECOGNITION_MRD_WEIGHT: number = 0.5
  Range: [0.0, 1.0]
  Description: Weight of inter-collective recognition toward MRD
  Default: 0.5 (half weight compared to intra-collective)

INTER_COLLECTIVE_RECOGNITION_MAX_PERCENTAGE: number = 30.0
  Range: [10.0, 100.0]
  Description: Maximum percentage of recognition that can go outside own collective
```

### 12.2 Coalition Visibility

```typescript
COALITION_DATA_VISIBILITY_DEFAULT: enum = "coalition_members"
  Options: ["public", "coalition_members", "collective_only", "private"]
  Description: Default visibility of recognition patterns and allocations

COALITION_DISCOVERY_ENABLED: boolean = true
  Description: Whether collective publishes to coalition-wide discovery registry

COALITION_NEED_VISIBILITY: enum = "coalition_wide"
  Options: ["coalition_wide", "provider_only", "collective_only"]
  Description: Who can see declared needs

COALITION_CAPACITY_VISIBILITY: enum = "coalition_wide"
  Options: ["coalition_wide", "recipient_set_only", "private"]
  Description: Who can see capacity declarations
```

***

## XIII. Data and Privacy Parameters

### 13.1 Recognition Privacy

```typescript
RECOGNITION_PRIVACY_LEVEL: enum = "pseudonymous"
  Options: ["public", "pseudonymous", "aggregate_only", "private"]
  Description: Privacy level of recognition data
  
  public: Names + percentages visible
  pseudonymous: IDs + percentages visible (no names)
  aggregate_only: Only MRS/MRD visible (not individual recognitions)
  private: Only MRD visible, no recognition details

RECOGNITION_HISTORY_RETENTION_YEARS: number = 7
  Range: [1, 10]
  Description: Years to retain historical recognition data

MEMBER_DATA_EXPORT_ENABLED: boolean = true
  Description: Whether members can export their own data (GDPR-like right)

MEMBER_DATA_DELETION_ON_EXIT: enum = "anonymize"
  Options: ["full_delete", "anonymize", "retain"]
  Description: What happens to member data when they exit
```

***

## XIV. Emergency and Edge Cases

### 14.1 Insufficient Proceeds

```typescript
INSUFFICIENT_PROCEEDS_ACTION: enum = "proportional_reduction"
  Options: [
    "proportional_reduction",  // Scale all allocations down proportionally
    "prioritize_minimums",     // Pay minimums first, then MS formula
    "defer_payment",           // Accumulate deficit, pay next cycle
    "emergency_loan"           // Draw from reserves or external loan
  ]
  Description: Action when proceeds insufficient to cover allocations

DEFICIT_OPERATION_ALLOWED: boolean = true
  Description: Whether collective can operate with negative proceeds

DEFICIT_MAXIMUM_MONTHS: number = 3
  Range: [0, 12]
  Description: Maximum consecutive months of deficit before intervention

DEFICIT_RESERVE_DRAW_LIMIT_PERCENTAGE: number = 50.0
  Range: [0.0, 100.0]
  Description: Maximum percentage of risk reserve drawable for deficit
```

### 14.2 Collective Dissolution

```typescript
DISSOLUTION_TRIGGER_MEMBER_MINIMUM: number = 2
  Range: [1, 5]
  Description: Minimum members required to maintain collective

DISSOLUTION_INACTIVITY_MONTHS: number = 6
  Range: [3, 24]
  Description: Months of zero activity before dissolution consideration

DISSOLUTION_ASSET_DISTRIBUTION: enum = "recognition_share"
  Options: [
    "recognition_share",      // Proportional to final MRS
    "equal_split",           // Equal to all members
    "original_contributors", // Only to those who contributed assets
    "related_collective"     // Transfer to another collective
  ]
  Description: How assets are distributed on dissolution

DISSOLUTION_DEBT_HANDLING: enum = "proportional_liability"
  Options: [
    "proportional_liability", // Shared by recognition shares
    "administrative_entity_liable", // Entity liable, not members
    "assets_only"            // Only collective assets liable
  ]
  Description: How debts are handled on dissolution
```

***

## XV. Meta-Parameters (Adjusting the Adjustment Process)

### 15.1 Parameter Update Rules

```typescript
PARAMETER_UPDATE_FREQUENCY_MINIMUM_DAYS: number = 90
  Range: [30, 365]
  Description: Minimum days between parameter updates (stability)

PARAMETER_UPDATE_CONSTITUTIONAL_LIST: string[] = [
  "MEMBERSHIP_THRESHOLD",
  "MINIMUM_MEANS_OF_PRODUCTION_AGGREGATE",
  "MINIMUM_INDIVIDUAL_ALLOCATION_USD",
  "ADMIN_COUNT",
  "DISSOLUTION_TRIGGER_MEMBER_MINIMUM"
]
  Description: Parameters requiring constitutional threshold (66%+) to change

PARAMETER_UPDATE_NOTICE_PERIOD_DAYS: number = 14
  Range: [7, 90]
  Description: Days notice before parameter changes take effect

PARAMETER_UPDATE_TRANSITION_ENABLED: boolean = true
  Description: Whether parameter changes transition gradually or immediately
```

***

## XVI. Implementation Checklist

### Required Decisions (For New Collective)

**Before Launch:**

1. [ ] Need taxonomy (categories for organizing tree)
2. [ ] Minimum individual allocation (subsistence floor)
3. [ ] Membership threshold (MRD for participation)
4. [ ] Recognition filtering (minimum % and quorum)
5. [ ] Proceeds distribution frequency
6. [ ] Admin count and selection method
7. [ ] Asset ownership legal structure
8. [ ] Tax classification (jurisdiction-specific)

**Recommended Defaults (Adjust via Decider):**

1. [ ] Allocation aggregation mode = "tree-wide"
2. [ ] Satisfaction learning = enabled
3. [ ] Oscillation dampening = enabled (0.7)
4. [ ] Minimum means of production = 10%
5. [ ] Minimum recognition threshold = 5%
6. [ ] Minimum quorum = 2 members
7. [ ] Bootstrap duration = 4 weeks

**Optional (Add Later):**

1. [ ] Inter-collective recognition
2. [ ] Coalition visibility settings
3. [ ] Advanced filtering rules
4. [ ] Custom acceptance strategies
5. [ ] Satisfaction decay

***

## XVII. Key Differences from Top-Down Model

### ❌ REMOVED (Technocratic Planning):

- STAGE1_PERCENTAGE
- STAGE2_PERCENTAGE  
- STAGE3_PERCENTAGE
- STAGE1_REPLACEMENT_PERCENTAGE
- STAGE1_EXPANSION_PERCENTAGE
- STAGE2_ADMIN_CAP_PERCENTAGE
- All predetermined budget allocations

### ✓ ADDED (Emergent Constraints):

- Need taxonomy (categories, not budgets)
- Safety floors (minimums only)
- Filtering rules (consensus requirements)
- MS formula parameters
- Satisfaction learning
- Collective tree aggregation modes

### Core Philosophical Shift:

**Before:** "Decider votes on 40% for Stage 1, 20% for Stage 2, 40% for Stage 3"  
**After:** "Members recognize needs, collective tree emerges, proceeds flow via MS formula"

**Before:** Top-down budgeting with predetermined splits  
**After:** Bottom-up recognition patterns determining proportions computationally

**Before:** Governance meetings to allocate proceeds  
**After:** Pure computation from collective recognition (no meetings)

***

## XVIII. Summary

### The Complete Flow

```
1. Members recognize needs in their personal trees:
   - Means of production needs
   - Common needs
   - Individual consumption needs
   - Others' contributions

2. Collective tree merges all personal trees:
   - Weighted by contributor recognition
   - Aggregated via collective-tree.svelte.ts
   - NO PREDETERMINED PERCENTAGES

3. Proceeds allocated via MS formula:
   - For each node: MS(collective, node) = MR × node_share
   - Allocation = proceeds × (MS / Σ_all_MS)
   - PROPORTIONS EMERGE FROM RECOGNITION

4. Filters and minimums applied:
   - Remove nodes below threshold
   - Ensure subsistence floors
   - Apply compliance filters

5. Satisfaction learns over time:
   - Recipients rate quality
   - Shares adjust: recognition × satisfaction
   - System optimizes toward utility

6. Convergence:
   - Oscillation dampening
   - Monotonic decrease of needs
   - Typical: 5-10 cycles to equilibrium
```

### The One Formula

```typescript
MS(provider, recipient) = MR × recipient_share

// Allocates everything:
// - External capacity (donations/grants)
// - Internal proceeds (revenue from labor)
// - Means of production needs
// - Common needs
// - Individual consumption

// Same formula, different contexts
// No special cases, no predetermined splits
// Pure computational allocation from recognition
```

**This is the protocol: recognition → collective tree → MS formula → allocation → satisfaction → learning → convergence.**

All parameters exist to tune THIS process, not to override it with predetermined budgets.

***

**Total Parameters: ~85 (down from 120+)**

**Categories:**
- Mathematical Foundation: 2 core formulas
- Tree Structure: 10 parameters
- Safety Constraints: 8 parameters
- Filtering: 6 parameters
- Satisfaction Learning: 8 parameters
- Convergence: 6 parameters
- Allocation Execution: 5 parameters
- Assets/Ownership: 10 parameters
- Membership: 8 parameters
- Compliance: 10 parameters
- Governance: 12 parameters
- Network/Coalition: 5 parameters
- Privacy: 4 parameters
- Emergency Cases: 8 parameters
- Meta-parameters: 4 parameters

**Implementation:** See collective-tree.svelte.ts and protocolv6.mmd for reference implementations.
