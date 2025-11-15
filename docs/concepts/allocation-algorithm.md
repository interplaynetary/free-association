# The Allocation Algorithm

## Two-Tier Priority System

Resources allocate through a priority structure that balances mutual relationships with support for emerging partnerships.

### Tier 1: Mutual Recognition Priority

Entities with mutual recognition receive first priority.

**Allocation Factors:**
- Strength of mutual recognition
- Declared resource needs
- Compatible resource specifications (time, location, type)

**Why First Priority?**
Mutual recognition indicates bidirectional contribution and established coordination relationships. These partnerships receive priority for stable resource flows.

---

### Tier 2: Unilateral Recognition

Remaining capacity flows to entities you recognize, even without mutual recognition.

**Purpose:**
- Support for new partners building recognition networks
- Enable mission-aligned resource flow
- Maintain incentives for genuine contribution

**Example:**
Foundation recognizes emerging organization at 15%, but emerging organization doesn't yet recognize foundation (no established relationship). Remaining foundation capacity after Tier 1 allocations flows to emerging organization proportional to recognition.

---

## Allocation Process

### Step 1: Filter Compatible Resources

Match resource specifications:
- Time windows overlap
- Geographic constraints satisfied
- Resource types compatible

Only compatible resources participate in allocation calculation.

### Step 2: Calculate Mutual Recognition

For each provider-recipient pair:
```
MR(Recipient, Provider) = min(
    Recognition_Recipient_gives_Provider,
    Recognition_Provider_gives_Recipient
)
```

### Step 3: Determine Proportional Shares

**Tier 1 Calculation:**
```
Share(Recipient, Provider) = 
    MR(Recipient, Provider) / Σ MR(Provider, All_Compatible_Recipients)
```

Each recipient's share is proportional to their mutual recognition relative to all recipients.

**Tier 2 Calculation:**
```
Share(Recipient, Provider) = 
    Recognition_Provider_gives_Recipient / Σ Recognition_Provider_gives_All_Recipients
```

Unilateral recognition determines share among recipients without mutual recognition.

### Step 4: Calculate Raw Allocation

```
Raw_Allocation(Recipient, Provider) = 
    Provider_Capacity × Share(Recipient, Provider)
```

### Step 5: Cap at Declared Need

```
Final_Allocation(Recipient, Provider) = min(
    Raw_Allocation(Recipient, Provider),
    Declared_Need(Recipient)
)
```

**Key Property:** No entity receives more than they declare they need. This prevents accumulation and ensures resources flow to actual requirements.

---

## Dynamic Updates

The system continuously adapts to changing conditions:

### Need Updates

After each allocation round:
```
Remaining_Need = max(0, Declared_Need - Total_Received)
```

As allocations are received, remaining needs decrease. System recalculates optimal allocation for updated need state.

### Oscillation Prevention

Adaptive damping prevents allocation oscillation:
```
Active_Need = Declared_Need × Damping_Factor

where Damping_Factor ∈ {0.5, 0.8, 1.0}
```

Damping factor adjusts based on allocation stability, preventing oscillation while maintaining responsiveness.

### Independent Resource Tracking

Each resource type tracks independently:
- Funding needs separate from expertise needs
- Time commitments independent of facility access
- System optimizes across all resource types simultaneously

---

## Convergence Properties

### Speed
System converges to stable equilibrium in **5-10 calculation rounds**.

Each round takes 100-200ms, meaning full convergence in **1-2 seconds** after network state change.

### Stability
Once converged, allocations remain stable unless:
- Network state changes (new needs, capacity, or recognition)
- Resource specifications updated
- Participants join or leave

### Optimality
At equilibrium:
- All needs met if sufficient capacity exists
- Resources distributed proportional to mutual recognition
- No entity receives beyond declared needs
- Tier 1 allocations satisfied before Tier 2

---

## Example Calculation

### Network State
**Provider Foundation (P):**
- Available capacity: $1,000,000

**Recipients:**
- Organization A: Declared need $400K
- Organization B: Declared need $600K  
- Organization C: Declared need $800K

**Mutual Recognition (P's perspective):**
- P recognizes A at 40%, A recognizes P at 30% → MR = 30%
- P recognizes B at 35%, B recognizes P at 50% → MR = 35%
- P recognizes C at 25%, C recognizes P at 20% → MR = 20%

**Total Mutual Recognition:** 30% + 35% + 20% = 85%

### Tier 1 Allocation

**Organization A Share:**
```
Share_A = 30% / 85% = 35.3%
Raw_Allocation_A = $1,000,000 × 0.353 = $353,000
Final_Allocation_A = min($353,000, $400,000) = $353,000
```

**Organization B Share:**
```
Share_B = 35% / 85% = 41.2%
Raw_Allocation_B = $1,000,000 × 0.412 = $412,000
Final_Allocation_B = min($412,000, $600,000) = $412,000
```

**Organization C Share:**
```
Share_C = 20% / 85% = 23.5%
Raw_Allocation_C = $1,000,000 × 0.235 = $235,000
Final_Allocation_C = min($235,000, $800,000) = $235,000
```

**Result:**
- A receives: $353,000 (remaining need: $47,000)
- B receives: $412,000 (remaining need: $188,000)
- C receives: $235,000 (remaining need: $565,000)
- Total allocated: $1,000,000

All capacity deployed. Allocations strictly proportional to mutual recognition. Each allocation capped at declared need.

---

## Key Properties

**Strategy-Proof:**
Honest reporting of needs is the optimal strategy. Over-reporting doesn't increase allocation (capped at need), under-reporting reduces allocation.

**Proportional Fairness:**
Allocations strictly proportional to mutual recognition strength. No arbitrary prioritization.

**Non-Accumulative:**
Cannot receive resources beyond declared needs. Prevents hoarding and ensures efficient flow to actual requirements.

**Real-Time Adaptive:**
System recalculates automatically as any network parameter changes. Maintains continuous optimization.

[Next: Use cases →](../use-cases/README.md)

