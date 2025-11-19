# How It Works

Free Association operates on simple data points published by each participant. The system uses these to calculate optimal resource allocation automatically.

## Core Data Points

### 1. Recognition Weights
**Who contributes to your organizational goals?**

Each entity allocates 100% of recognition among contributors.

**Properties:**
- Non-transferable (cannot be bought or sold)
- Dynamically adjustable as relationships evolve
- Can reflect direct operations or broader mission-aligned values
- Organized as a contribution tree

**Example:**
```
Humanitarian Organization A recognizes:
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

### 4. Mutual Recognition
**Bidirectional acknowledgment of contributions**

The system calculates mutual recognition between any two entities:

```
MR(Entity_A, Entity_B) = min(
    Recognition_A_gives_B,
    Recognition_B_gives_A
)
```

**Why minimum?**
- Ensures proportional reciprocity
- Prevents unilateral inflation
- Creates natural incentive for accurate recognition

**Example:**
```
Organization A recognizes B at 50%
Organization B recognizes A at 10%
→ Mutual Recognition = 10%
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

**Mutual Recognition (MR):** Calculated as the lower of the recognition percentages that two entities assign to each other. This creates *perfect reciprocity in proportion*. A one-sided relationship (where A recognizes B highly, but B recognizes A little) is valued at the lower amount, discouraging free-riding and encouraging mutual engagement and support.

When we **recognize** each other, we have **mutual-recognition of mutual-value** and **can choose to allocate our capacities to each-other in precise proportion to how mutually-fulfilling we are to each other.**

**The system naturally promotes accurate recognition through mathematical necessity:**  

Entities define their goals/priorities subjectively, but achieving them depends on objective access to capacities and partnerships.

**FOR ANY PARTICIPANT:**  

**GIVEN:**  

  **• Total Recognition = 100%**  

  **• Capacities distributed ∝ (Mutual)-Recognition**  

  **• Goals require access to specific capacities/partnerships**  

**THEN:**  

  **↑ Recognition allocated to non-beneficial partners**  

    **∴ ∝ ↓ Recognition available for beneficial partners [budget constraint]**  

    **∴ ↓ Mutual-Recognition with beneficial partners**  

    **∴ ↓ Access to needed capacities [proportional allocation]**  

    **∴ ↓ Goal Achievement**  

    **∴ Natural incentive to correct recognition allocation**

**Key Implication:** The system creates natural incentives for accurate recognition. Inflating or misattributing recognition only decreases connection to beneficial partners and capacities. Entities that maintain accurate recognition patterns receive better-aligned capacities and achieve better outcomes.

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
2. **Calculate** mutual recognition between all pairs
3. **Determine** proportional shares based on recognition strength
4. **Allocate** resources (capped at declared needs)
5. **Update** remaining needs automatically
6. **Recompute** optimal allocation as network state changes (~100-200ms per update)

The entire process happens automatically. When state stabilizes, needs converge in O(log(1/ε)) rounds.

Recognition determines the split. Need size sets the cap. No meetings. No applications. No bureaucracy.

[Next: The allocation algorithm →](allocation-algorithm.md)

