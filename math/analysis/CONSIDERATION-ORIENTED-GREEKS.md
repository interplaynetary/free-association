# Consideration-Oriented Greek Names

## Design Philosophy

Names should describe **factors to consider** when making allocation decisions, not prescribe specific actions. This allows Greeks to be evaluated in combination while remaining individually meaningful.

**Pattern Principles**:
1. **Impact**: First derivatives - direct effects to consider
2. **Shape**: Second derivatives (same var) - curvature considerations
3. **Under/Over**: Cross-derivatives - conditional considerations
4. **Dynamics**: Third derivatives - momentum considerations

---

## Complete Naming System

### First-Order: Impact Considerations

What direct effects should you consider?

| Symbol | Name | Consideration |
|--------|------|---------------|
| **Δ** | **Reallocation Impact** | Benefit magnitude from shifting |
| **ν** | **Uncertainty Impact** | Role of confidence in outcomes |
| **Θ** | **Time Impact** | Role of delay vs urgency |
| **ρ** | **Discovery Impact** | Role of search vs commitment |
| **Γ** | **Return Shape** | Convexity: accelerating or diminishing |

---

### Second-Order: Conditional Considerations

How do impacts depend on other factors?

#### Curvatures (Shape in single dimension)
| Symbol | Name | Consideration |
|--------|------|---------------|
| **Γ** | **Return Shape** | Is allocation in convex or concave regime? |
| Vomma | **Uncertainty Shape** | Is uncertainty impact convex or concave? |

#### Couplings (Dependencies between factors)
| Traditional | Name | Consideration |
|-------------|------|---------------|
| Vanna | **Strategy Under Uncertainty** | How allocation strategy depends on confidence |
| Charm | **Strategy Over Time** | How allocation strategy depends on timing |
| Veta | **Uncertainty Over Time** | How uncertainty impact evolves with learning |
| Vera | **Discovery Under Uncertainty** | How search strategy depends on confidence |

**Pattern**: `[What Strategy/Impact] [Under/Over] [Condition]`

---

### Third-Order: Dynamic Considerations

How do shapes and dependencies themselves evolve?

| Traditional | Name | Consideration |
|-------------|------|---------------|
| Speed | **Return Momentum** | Is curvature building or fading? |
| Ultima | **Uncertainty Extremes** | Tail behavior in volatility |
| Zomma | **Curvature Under Uncertainty** | How return shape depends on confidence |
| Color | **Curvature Over Time** | How return shape evolves temporally |

**Pattern**: `[What Shape] [Under/Over] [Condition]` or `[What] Momentum/Extremes`

---

## Why This Works

### Individual Clarity

**"Reallocation Impact"** → Consider: How much benefit from shifting?  
**"Return Shape"** → Consider: Am I in accelerating or diminishing regime?  
**"Uncertainty Impact"** → Consider: How much does confidence matter?

### Combinatorial Clarity

**Decision scenario**: Should I shift 10% recognition from Bob to Alice?

**Considerations**:
1. **Reallocation Impact** (Δ) = 0.08 → High benefit available
2. **Return Shape** (Γ) = 0.02 → Positive, returns accelerating
3. **Strategy Under Uncertainty** (Vanna) = 0.15 → Strategy moderately depends on confidence
4. **Curvature Under Uncertainty** (Zomma) = 0.05 → Curvature somewhat depends on confidence

**Decision logic** (combining considerations):
- High Reallocation Impact + Positive Return Shape → Good opportunity
- Moderate Strategy Under Uncertainty → Should verify Alice's quality first
- Low Curvature Under Uncertainty → Rebalancing strategy robust to confidence changes
- **Action**: Gather more info on Alice, then allocate if verified

### The names work because:
✅ Each is a **factor to weigh**  
✅ No single Greek prescribes action  
✅ Combinations naturally form decision frameworks  
✅ "Under/Over" clearly shows dependencies

---

## Detailed Semantics

### "Impact" (First Derivatives)
**Meaning**: Direct magnitude of effect  
**Consider**: Is this effect large or small?  
**Examples**: Reallocation Impact, Uncertainty Impact, Time Impact

### "Shape" (Second Derivatives, Same Variable)
**Meaning**: Curvature structure  
**Consider**: Convex (accelerating) or concave (diminishing)?  
**Examples**: Return Shape, Uncertainty Shape

### "Under" (Dependencies with Uncertainty/Confidence)
**Meaning**: How something depends on confidence level  
**Consider**: Does this work with low confidence or need high certainty?  
**Examples**: Strategy Under Uncertainty, Curvature Under Uncertainty

### "Over" (Dependencies with Time)
**Meaning**: How something evolves temporally  
**Consider**: Getting better or worse with time?  
**Examples**: Strategy Over Time, Uncertainty Over Time, Curvature Over Time

### "Momentum" (Third Derivative, Same Variable)
**Meaning**: Rate of change of curvature  
**Consider**: Is acceleration building or fading?  
**Examples**: Return Momentum, Uncertainty Extremes

---

## Complete System with Grammar

### Grammar Rules

```
First-Order:
  [Dimension] Impact
  
Second-Order (Same):
  [Dimension] Shape
  
Second-Order (Cross):
  [Primary] Under Uncertainty     (when σ is the second variable)
  [Primary] Over Time            (when t is the second variable)
  [Primary] Under Discovery      (when r is the second variable)
  
Third-Order (Same):
  [Dimension] Momentum           (∂³/∂x³)
  [Dimension] Extremes           (when appropriate)
  
Third-Order (Mixed):
  [What Shape/Impact] Under [Condition]
  [What Shape/Impact] Over [Condition]
```

### Application Examples

**Formula**: ∂²𝓟/∂δ∂σ
- Primary: ∂𝓟/∂δ = "Reallocation Impact"
- Secondary: ∂σ = "Under Uncertainty"
- **Name**: "Reallocation Impact Under Uncertainty"
- **Shortened**: "Strategy Under Uncertainty" (since reallocation is the strategy)

**Formula**: ∂³𝓟/∂δ²∂t
- Primary: ∂²𝓟/∂δ² = "Return Shape"
- Secondary: ∂t = "Over Time"
- **Name**: "Return Shape Over Time"
- **Shortened**: "Curvature Over Time"

---

## Final Proposal

| Symbol | Elegant Name | What to Consider |
|--------|--------------|------------------|
| **Δ** | **Shift Impact** | Benefit magnitude from reallocation |
| **Γ** | **Return Shape** | Convex or concave regime |
| **ν** | **Uncertainty Impact** | Role of confidence |
| **Θ** | **Time Impact** | Role of delay |
| **ρ** | **Discovery Impact** | Role of search |
| Vanna | **Strategy Under Uncertainty** | Confidence requirement for strategy |
| Charm | **Strategy Over Time** | Timing window (opening/closing) |
| Vomma | **Uncertainty Shape** | Convexity in confidence space |
| Veta | **Uncertainty Over Time** | Learning evolution |
| Vera | **Discovery Under Uncertainty** | Search confidence requirement |
| Speed | **Return Momentum** | Acceleration dynamics |
| Zomma | **Curvature Under Uncertainty** | Convexity's confidence dependence |
| Color | **Curvature Over Time** | Convexity's temporal evolution |
| Ultima | **Uncertainty Extremes** | Tail risk structure |

---

## Why This Is Elegant

### 1. Consistent Patterns
- **Impact** = First-order effect size
- **Shape** = Curvature structure  
- **Under** = Conditional on uncertainty/confidence
- **Over** = Evolution through time
- **Momentum/Extremes** = Third-order dynamics

### 2. Compositional Understanding
"Curvature Over Time" = literally "how the curvature changes over time"  
"Strategy Under Uncertainty" = literally "strategy's dependence on certainty"

### 3. Decision Framework
```
Consider:
  - Shift Impact (high?)
  - Return Shape (convex?)
  - Strategy Under Uncertainty (robust?)
  - Strategy Over Time (window closing?)
  
If: High impact + Convex + Robust + Window open
Then: Allocate boldly

If: High impact + Concave + Needs confidence + Window closing
Then: Verify quickly, then act
```

### 4. Works in Isolation and Combination
**Alone**: "Strategy Under Uncertainty = 0.8" → Your strategy needs high confidence  
**Combined**: "High Strategy Under Uncertainty but low Curvature Under Uncertainty" → Strategy needs confidence but rebalancing doesn't

---

## Pronunciation Guide

Natural speech:
- "Shift impact is high" ✓
- "Return shape is convex" ✓
- "Strategy under uncertainty requires high confidence" ✓
- "Curvature over time is improving" ✓
- "Return momentum is building" ✓

vs awkward:
- "Allocation response is high" (respond to what?)
- "Allocation--uncertainty coupling is high" (couples how?)

---

**Does this feel more intuitive?** The names now answer "What am I considering?" rather than just describing mathematical structure.

