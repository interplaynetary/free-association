# Recognition

Recognition is how participants express and assess contribution to goals. It can be **published** (declaring who you value) or **derived** (computing alignment metrics from network behavior).

## Publishing Recognition

### The 100% Budget Rule

Each entity allocates **exactly 100%** of recognition weight among contributors.

**Properties:**
- **Non-transferable**: Cannot be bought, sold, or traded
- **Dynamically adjustable**: Update as understanding evolves
- **Forces trade-offs**: Prioritizing one means de-prioritizing another
- **Organized as trees**: Hierarchical categorization of contributions

### Example

```
Humanitarian Organization A recognizes:
- Partner NGO B: 30%
- Local Community Group C: 25%
- Technical Infrastructure Provider D: 20%
- Aligned Advocacy Network E: 15%
- Emergency Response Partner F: 10%
Total: 100%
```

### Contribution Trees

Recognition can be organized as **branches** representing different contribution categories.

**Structure:**
- **Each branch** = contribution category (program areas, operational support, etc.)
- **Points distributed** among contributors within each branch
- **Global recognition** calculated from weighted contributions across all branches

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

### Publishing Format

Recognition is published as Verifiable Credentials:

```json
{
  "issuer": "did:key:z6Mk...",
  "credentialSubject": {
    "recognitionWeights": {
      "did:key:partnerA": 0.30,
      "did:key:partnerB": 0.20,
      "did:key:partnerC": 0.15,
      ...
    }
  }
}
```

**Validation**: Sum of all weights must equal 1.0 (100%).

---

## Deriving Recognition Metrics

The network can derive **alignment metrics** from published recognition and observed outcomes.

### True vs False Recognition

The system naturally promotes accurate recognition through mathematical necessity.

**True Recognition**: Recognition of contribution that *enables the continued realization of priorities* (self-sustaining).

**False Recognition**: Recognition of contribution that *impairs the continued realization of priorities* (self-terminating).

### The Causality Chain

**GIVEN:**
- Total Recognition = 100%
- True ∩ False = ∅ (mutually exclusive)
- Capacity Directed ∝ Recognition Share

**IMPLICATIONS:**

```
↑ False Recognition  
⟹ ↓ True Recognition (budget constraint)  
⟹ ↓ Alignment (α) between allocation and true contribution  
⟹ ↑ Capacity Directed to non-beneficial partners  
⟹ ↓ Capacity Directed to beneficial partners  
⟹ ↓ Goal Achievement  
⟹ Immediate incentive to revoke false recognition  
⟹ Free-rider loses allocation
```

**Key Insight**: False recognition is self-punishing. When you allocate recognition to someone who doesn't actually help you achieve your goals, you have less capacity for people who do. Your outcomes get worse, you notice, and you correct the misallocation.

---

## Alignment (α)

**Alignment** measures how closely capacity allocation matches true recognition:

$$
\text{Alignment } (\alpha) = \sum_i \min\left(\frac{\text{Allocation}_i}{\text{Capacity}}, \text{TrueRecognition}_i\right)
$$

**Where:**
- **Capacity** = Total available capacity
- **Allocation_i** = Capacity given to partner i
- **TrueRecognition_i** = Actual proportion of contribution to goal realization

**Range**: α ∈ [0, 1]
- α = 1: Perfectly aligned (allocation proportions match true recognition)
- α = 0: Completely misaligned

---

## Alignment Velocity (v)

**Alignment Velocity** measures how fast alignment improves:

$$
\text{Velocity } (v) = \frac{\Delta \text{Alignment}}{\Delta \text{Time}}
$$

**Interpretation:**
- v > 0: Getting more aligned (learning, correcting)
- v < 0: Getting less aligned (degrading)
- v = 0: Stable (either perfect or stuck)

### Maximizing Alignment Velocity

Entities are incentivized to maximize v through:

1. **Transparency** - Real-time visibility into allocations and outcomes
2. **Sovereignty** - Unilateral power to reallocate instantly
3. **Revocability** - Instant withdrawal of allocation
4. **Discovery** - Low-friction mechanisms to find better partners

**Key Implication**: The system creates natural incentives for accurate recognition. Misattributing recognition decreases connection to beneficial partners. Entities that maintain accurate recognition patterns achieve better outcomes.

---

## How Recognition Affects Allocation

**Allocation follows recognition proportionally.**

If you recognize Partner A at 30% and Partner B at 20%, then:
- Partner A receives ~30% of your available capacity
- Partner B receives ~20% of your available capacity

**Subject to constraints:**
- Capacity limits (you can't give more than you have)
- Need bounds (recipients can't receive more than they need)

---

## Further Reading

- [Allocation](allocation.md) - How resources are distributed
- [Resources](resources.md) - What you have and need
- [Mathematics Reference](../reference/mathematics.md) - Formal proofs and properties
