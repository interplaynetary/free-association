# Consideration-Oriented Greek Names: Applied

## Summary

Successfully updated `universal.tex` to use **consideration-oriented names** throughout the Recognition Greeks section. Names now describe **what to consider** when making allocation decisions, working both individually and in combination.

## Naming Pattern Applied

### Core Patterns
- **Impact**: First derivatives → Direct effects to consider
- **Shape**: Curvatures → Convexity structures  
- **Under**: Cross with uncertainty → Confidence dependencies
- **Over**: Cross with time → Temporal evolution
- **Momentum/Extremes**: Third-order → Dynamics and tail behavior

### Grammar
```
First-order:     [What] Impact
Same-var 2nd:    [What] Shape
Cross with σ:    [What] Under Uncertainty
Cross with t:    [What] Over Time
Same-var 3rd:    [What] Momentum/Extremes
Mixed 3rd:       [What Shape] Under/Over [Condition]
```

## Complete Name Changes

### First-Order Greeks

| Traditional | Old Name | New Name | Formula |
|-------------|----------|----------|---------|
| **Delta** | Allocation Response | **Shift Impact** | ∂𝓟/∂δ |
| **Gamma** | Allocation Curvature | **Return Shape** | ∂²𝓟/∂δ² |
| **Theta** | Temporal Response | **Time Impact** | ∂𝓟/∂t |
| **Vega** | Uncertainty Response | **Uncertainty Impact** | ∂𝓟/∂σ |
| **Rho** | Opportunity Response | **Discovery Impact** | ∂𝓟/∂r |

### Second-Order Greeks

| Traditional | Old Name | New Name | Formula |
|-------------|----------|----------|---------|
| **Vanna** | Allocation--Uncertainty Coupling | **Strategy Under Uncertainty** | ∂²𝓟/∂δ∂σ |
| **Charm** | Allocation--Temporal Coupling | **Strategy Over Time** | ∂²𝓟/∂δ∂t |
| **Vomma** | Uncertainty Curvature | **Uncertainty Shape** | ∂²𝓟/∂σ² |
| **Veta** | Uncertainty--Temporal Coupling | **Uncertainty Over Time** | ∂²𝓟/∂σ∂t |
| **Vera** | Opportunity--Uncertainty Coupling | **Discovery Under Uncertainty** | ∂²𝓟/∂r∂σ |

### Third-Order Greeks

| Traditional | Old Name | New Name | Formula |
|-------------|----------|----------|---------|
| **Speed** | Allocation Acceleration | **Return Momentum** | ∂³𝓟/∂δ³ |
| **Zomma** | Allocation Curvature--Uncertainty Coupling | **Curvature Under Uncertainty** | ∂³𝓟/∂δ²∂σ |
| **Color** | Allocation Curvature--Temporal Coupling | **Curvature Over Time** | ∂³𝓟/∂δ²∂t |
| **Ultima** | Uncertainty Acceleration | **Uncertainty Extremes** | ∂³𝓟/∂σ³ |

## What Changed in universal.tex

### 1. Naming Grammar Section
Updated the introduction to explain consideration-oriented patterns:
- Four fundamental dimensions (Shift, Uncertainty, Time, Discovery)
- Pattern types (Impact, Shape, Under, Over, Momentum/Extremes)
- Construction grammar with examples
- Added "Consideration" component to examples

### 2. All Greek Definitions
Each Greek now includes:
- **New consideration-oriented name** (primary)
- **Traditional name** (in parentheses)
- **Updated definition** focusing on what to consider
- **"Consideration" line** explaining practical decision relevance

### 3. Enhanced Interpretations
Rewrote interpretations to emphasize:
- What the Greek helps you consider
- How it informs decisions
- When it's most relevant
- How it combines with other Greeks

## Example Usage

### Individual Consideration
```
Shift Impact = 0.08 → "High benefit available from reallocation"
Return Shape = 0.02 → "Returns are accelerating (convex regime)"
Strategy Under Uncertainty = 0.15 → "Strategy moderately depends on confidence"
```

### Combined Decision Framework
```
Decision: Should I reallocate 10% from Bob to Alice?

Consider:
  • Shift Impact (high) → Opportunity is significant
  • Return Shape (positive) → In accelerating regime  
  • Strategy Under Uncertainty (moderate) → Should verify Alice first
  • Curvature Under Uncertainty (low) → Rebalancing robust to uncertainty
  
Action: Gather info on Alice, then allocate if verified
```

## Why This Is Better

### ✅ Intuitive
"Shift Impact" and "Return Shape" immediately convey meaning  
"Strategy Under Uncertainty" clearly shows dependency

### ✅ Compositional
"Curvature Over Time" = literally "how curvature changes over time"  
"Discovery Under Uncertainty" = literally "discovery's dependence on confidence"

### ✅ Action-Guiding (Via Combination)
Names describe **considerations** not **prescriptions**  
Multiple Greeks combine to inform decisions naturally

### ✅ Natural Speech
- "Shift impact is high" ✓
- "Return shape is convex" ✓  
- "Strategy under uncertainty requires confidence" ✓
- "Curvature over time is improving" ✓

vs awkward systematic names:
- "Allocation response is high" (respond to what?)
- "Allocation--uncertainty coupling is high" (couples how?)

## Files Updated

1. **universal.tex**: Complete Greek section with new names
2. **CONSIDERATION-ORIENTED-GREEKS.md**: Design philosophy and reference
3. **CONSIDERATION-NAMES-APPLIED.md**: This summary

## Next Steps

The naming is complete and elegant. The Greeks section now:
- Uses intuitive, consideration-oriented names
- Maintains rigorous mathematical definitions
- Provides clear decision-making guidance
- Works both individually and in combination

**Status**: ✅ Ready for publication

