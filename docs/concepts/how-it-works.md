# How It Works

Free Association operates on simple data points published by each participant. The system uses these to calculate optimal resource allocation automatically.

## Core Data Points

### 1. Priority Weights
**How do you prioritize your capacity?**

Each entity allocates 100% of weight among recipients or categories.

**Properties:**
- **Often derived from recognition of contribution**
- Non-transferable (cannot be bought or sold)
- Dynamically adjustable as priorities evolve
- Organized as a prioritization tree

**Example:**
```
Humanitarian Organization A prioritizes:
- Partner NGO B: 30%
- Local Community Group C: 25%
- Technical Infrastructure Provider D: 20%
- Aligned Advocacy Network E: 15%
- Emergency Response Partner F: 10%
Total: 100%
```

---

### 2. Available Capacity
**What resources can you offer?**

Declare surplus resources available for allocation.

**Resource Types:**
- Funds
- Expertise
- Facilities
- Time
- Equipment

**Filters:**
- Time windows
- Geographic locations
- Resource type specifications

**Example:**
```
Foundation X declares:
- $500K/month operational funding
- Available: Next 6 months
- Filter: Healthcare and education sectors
- Location: Any
```

---

### 3. Declared Needs
**What resources do you require?**

State specific resource requirements.

**Properties:**
- Real-time updates as needs evolve
- System caps allocations at declared needs
- Prevents resource accumulation
- Enables precise matching

**Example:**
```
Organization B declares:
- $200K/month operational funding
- $100K emergency medical supplies
- 40 hours/week technical expertise
```

---

### 4. Recognition
**Assessment of contribution to goal realization**

Entities allocate recognition based on contribution toward their goals.

**Key Properties:**
- Subjective assessment by each entity
- 100% budget forces prioritization
- Non-transferable (cannot be bought or sold)
- Dynamically adjustable as understanding evolves

**Example:**
```
Organization A recognizes:
- Partner B: 50% (major program contributor)
- Partner C: 30% (operational support)
- Partner D: 20% (mission alignment)
```

**Self-Recognition:**
Valid for time-shifting resources within your own organization.

---

### 5. Contribution Trees
**Structured tracking of contribution types**

Recognition organized as branches representing different contribution categories.

**Structure:**
- Each branch = contribution category (program areas, operational support, etc.)
- Points distributed among contributors within each branch
- Global recognition calculated from weighted contributions across all branches

**Benefits:**
- Granular tracking of different contribution types
- Maintains overall coherence
- Enables precise recognition patterns

**Example:**
```
Organization Structure:
├── Program Delivery (50% weight)
│   ├── Partner A: 60%
│   └── Partner B: 40%
├── Operational Support (30% weight)
│   ├── Provider C: 70%
│   └── Provider D: 30%
└── Mission Alignment (20% weight)
    ├── Ally E: 50%
    └── Ally F: 50%

Global Recognition Calculated:
Partner A = 0.5 × 0.6 = 30%
Provider C = 0.3 × 0.7 = 21%
Ally E = 0.2 × 0.5 = 10%
...etc
```

---

## Core Derivations

**Total Recognition (100%):** Each participant has a fixed "budget" of recognition to distribute. This forces prioritization and trade-offs. Recognition is non-transferable and dynamically adjustable.

**Allocation Follows Recognition:** Resources are distributed proportionally to recognition, subject to capacity and need constraints. The allocation algorithm performs two-sided optimization, satisfying both provider priorities (who they want to support) and recipient preferences (who they want to receive from).

## Anti-Gaming: True vs. False Recognition

**The system naturally promotes accurate recognition through mathematical necessity:**

Participants define their goals subjectively, but achieving them depends on objective access to resources and contributions. Recognition accuracy is validated through outcomes.

**True Recognition:** Recognition of contribution to the realization of priorities that *enables the continued realization of priorities* (self-sustaining).

**False Recognition:** Recognition of contribution to the realization of priorities that *impairs the continued realization of priorities* (self-terminating).

### The Causality Chain

**GIVEN:**
- Total Recognition = 100%
- True ∩ False = ∅ (mutually exclusive)
- Capacity Directed ∝ Recognition Share

**IMPLICATIONS:**

↑ False Recognition  
⟹ ↓ True Recognition (budget constraint)  
⟹ ↓ Alignment (α) between allocation and true contribution  
⟹ ↑ Capacity Directed to non-beneficial partners  
⟹ ↓ Capacity Directed to beneficial partners  
⟹ ↓ Goal Achievement  
⟹ Immediate incentive to revoke false recognition  
⟹ Free-rider loses allocation

**Key Insight:** False recognition is self-punishing. When you allocate recognition to someone who doesn't actually help you achieve your goals, you have less capacity for people who do. Your outcomes get worse, you notice, and you correct the misallocation.

### Alignment (α)

**Alignment** measures how closely your capacity allocation matches true recognition:

```
Alignment (α) = Σ min(Allocation_i / Capacity, TrueRecognition_i)
```

Where:
- **Capacity** = Your total available capacity
- **Allocation_i** = Capacity you give to partner i
- **TrueRecognition_i** = Actual proportion of contribution to your goal realization

Alignment ranges from 0 (completely misaligned) to 1 (perfectly aligned).

### Alignment Velocity (v)

**Alignment Velocity** measures how fast alignment improves:

```
Velocity (v) = ΔAlignment / ΔTime
```

- **Positive velocity** → Getting more aligned (learning, correcting)
- **Negative velocity** → Getting less aligned (degrading)
- **Zero velocity** → Stable (either perfect or stuck)

Entities are incentivized to maximize alignment velocity through:
1. **Transparency** - Real-time visibility into allocations and outcomes
2. **Sovereignty** - Unilateral power to reallocate instantly
3. **Revocability** - Instant withdrawal of allocation
4. **Discovery** - Low-friction mechanisms to find better partners

**Key Implication:** The system creates natural incentives for accurate recognition. Misattributing recognition decreases connection to beneficial partners. Entities that maintain accurate recognition patterns achieve better outcomes.

---

## Resource Types

### Mission-Aligned Values
Contributions toward organizational mission and values.

**Key Property:** No shared definitions required. Each entity determines what constitutes meaningful contribution to their goals.

### Specific Resource Types
Concrete resources requiring common terminology:
- Funding
- Expertise
- Facilities
- Equipment
- Time

**Key Property:** Requires compatible specifications for matching.

---

## How Allocation Happens

Once all entities have published their data:

1. **Filter** for compatible resource specifications
2. **Calculate** recognition weights for all potential allocations
3. **Optimize** allocation matrix to satisfy both provider priorities and recipient preferences
4. **Apply** constraints (capacity limits and need bounds)
5. **Update** remaining needs automatically
6. **Recompute** optimal allocation as network state changes (~100-200ms per update)

The entire process happens automatically through two-sided optimization. The system finds the allocation that best satisfies both:
- **Provider priorities**: Who providers want to support (proportional to recognition)
- **Recipient preferences**: Who recipients want to receive from

Recognition determines the proportions. Constraints set the bounds. No meetings. No applications. No bureaucracy.

[Next: The allocation algorithm →](allocation-algorithm.md)

