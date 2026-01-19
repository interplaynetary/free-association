# Systematic Recognition Greek Nomenclature

## Design Principles

A truly elegant naming system should be:
1. **Self-explanatory**: Name reveals what it measures
2. **Hierarchical**: Shows derivative order clearly
3. **Relational**: Shows connections between Greeks
4. **Unambiguous**: No confusion about what's being differentiated
5. **Compositional**: Can construct name from components

---

## Naming Architecture

### Base Components (Dimensions)

Four fundamental dimensions:

| Symbol | Dimension | Meaning |
|--------|-----------|---------|
| **δ** | Allocation | Recognition shift between partners |
| **σ** | Uncertainty | Confidence in benefit estimates |
| **t** | Temporal | Time evolution |
| **r** | Opportunity | Discovery rate / search cost |

### Derivative Types

| Order | Type | Pattern |
|-------|------|---------|
| 1st | **Response** | Sensitivity to single variable |
| 2nd (same) | **Curvature** | Convexity in single dimension |
| 2nd (cross) | **Coupling** | Interaction between dimensions |
| 3rd | **Acceleration** | Rate of curvature change |

---

## Complete Systematic Naming

### First-Order: Base Responses

**Pattern**: `[Dimension] Response`

| Traditional | Systematic Name | Formula | Meaning |
|------------|-----------------|---------|---------|
| Delta (Δ) | **Allocation Response** | ∂𝓟/∂δ | How goal responds to allocation shift |
| Vega (ν) | **Uncertainty Response** | ∂𝓟/∂σ | How goal responds to confidence change |
| Theta (Θ) | **Temporal Response** | ∂𝓟/∂t | How goal responds to time passage |
| Rho (ρ) | **Opportunity Response** | ∂𝓟/∂r | How goal responds to discovery rate |
| Gamma (Γ) | **Allocation Curvature** | ∂²𝓟/∂δ² | Convexity of allocation response |

**Why better**:
- "Response" clearly indicates first derivative
- "Curvature" clearly indicates second derivative (same variable)
- Dimension name tells you what's varying

---

### Second-Order: Curvatures and Couplings

**Pattern for Curvature**: `[Dimension] Curvature`  
**Pattern for Coupling**: `[Dim₁]–[Dim₂] Coupling`

| Traditional | Systematic Name | Formula | Meaning |
|------------|-----------------|---------|---------|
| Gamma (Γ) | **Allocation Curvature** | ∂²𝓟/∂δ² | Convexity in allocation space |
| Vomma | **Uncertainty Curvature** | ∂²𝓟/∂σ² | Convexity in uncertainty space |
| Vanna | **Allocation–Uncertainty Coupling** | ∂²𝓟/∂δ∂σ | How allocation response depends on uncertainty |
| Charm | **Allocation–Temporal Coupling** | ∂²𝓟/∂δ∂t | How allocation response depends on time |
| Veta | **Uncertainty–Temporal Coupling** | ∂²𝓟/∂σ∂t | How uncertainty response depends on time |
| Vera | **Opportunity–Uncertainty Coupling** | ∂²𝓟/∂r∂σ | How opportunity response depends on uncertainty |

**Why better**:
- "Curvature" = ∂²/∂x² (same variable twice)
- "Coupling" = ∂²/∂x∂y (two different variables)
- Hyphenated dimensions show exact interaction
- Order doesn't matter (coupling is symmetric)

**Reading the names**:
- "Allocation–Uncertainty Coupling" = "How allocation sensitivity couples with uncertainty"
- "Uncertainty Curvature" = "Second-order structure in uncertainty space"

---

### Third-Order: Accelerations and Curvature Dynamics

**Pattern for Acceleration**: `[Dimension] Acceleration`  
**Pattern for Curvature Coupling**: `[Dimension] Curvature–[Other] Coupling`

| Traditional | Systematic Name | Formula | Meaning |
|------------|-----------------|---------|---------|
| Speed | **Allocation Acceleration** | ∂³𝓟/∂δ³ | Rate of curvature change in allocation |
| Ultima | **Uncertainty Acceleration** | ∂³𝓟/∂σ³ | Rate of curvature change in uncertainty |
| Zomma | **Allocation Curvature–Uncertainty Coupling** | ∂³𝓟/∂δ²∂σ | How allocation curvature depends on uncertainty |
| Color | **Allocation Curvature–Temporal Coupling** | ∂³𝓟/∂δ²∂t | How allocation curvature depends on time |

**Why better**:
- "Acceleration" = ∂³/∂x³ (third derivative, same variable)
- "Curvature–X Coupling" = Shows which curvature is interacting with what
- Order matters: "Allocation Curvature–Uncertainty" means ∂(∂²/∂δ²)/∂σ
- Can deduce formula from name

**Reading the names**:
- "Allocation Acceleration" = "Third derivative: how fast is curvature changing?"
- "Allocation Curvature–Uncertainty Coupling" = "How does allocation curvature vary with uncertainty?"

---

## Hierarchical Structure Visualized

```
𝓟(G)  -  Goal Achievement
│
├─ FIRST ORDER (Responses)
│  ├─ Allocation Response (Δ)
│  ├─ Uncertainty Response (ν)
│  ├─ Temporal Response (Θ)
│  └─ Opportunity Response (ρ)
│
├─ SECOND ORDER
│  ├─ Curvatures (∂²/∂x²)
│  │  ├─ Allocation Curvature (Γ)
│  │  └─ Uncertainty Curvature (Vomma)
│  │
│  └─ Couplings (∂²/∂x∂y)
│     ├─ Allocation–Uncertainty Coupling (Vanna)
│     ├─ Allocation–Temporal Coupling (Charm)
│     ├─ Uncertainty–Temporal Coupling (Veta)
│     └─ Opportunity–Uncertainty Coupling (Vera)
│
└─ THIRD ORDER
   ├─ Accelerations (∂³/∂x³)
   │  ├─ Allocation Acceleration (Speed)
   │  └─ Uncertainty Acceleration (Ultima)
   │
   └─ Curvature Couplings (∂³/∂x²∂y)
      ├─ Allocation Curvature–Uncertainty Coupling (Zomma)
      └─ Allocation Curvature–Temporal Coupling (Color)
```

---

## Naming Rules (Generative Grammar)

### Rule 1: First-Order
**Formula**: `[Dimension] Response`

**Examples**:
- ∂𝓟/∂δ → Allocation Response
- ∂𝓟/∂σ → Uncertainty Response

### Rule 2: Second-Order Same Variable (Curvature)
**Formula**: `[Dimension] Curvature`

**Examples**:
- ∂²𝓟/∂δ² → Allocation Curvature
- ∂²𝓟/∂σ² → Uncertainty Curvature

### Rule 3: Second-Order Cross Variables (Coupling)
**Formula**: `[Dim₁]–[Dim₂] Coupling`

**Examples**:
- ∂²𝓟/∂δ∂σ → Allocation–Uncertainty Coupling
- ∂²𝓟/∂σ∂t → Uncertainty–Temporal Coupling

### Rule 4: Third-Order Same Variable (Acceleration)
**Formula**: `[Dimension] Acceleration`

**Examples**:
- ∂³𝓟/∂δ³ → Allocation Acceleration
- ∂³𝓟/∂σ³ → Uncertainty Acceleration

### Rule 5: Third-Order Mixed (Curvature Coupling)
**Formula**: `[Primary Dimension (²)] Curvature–[Other Dimension] Coupling`

**Where the primary dimension appears twice**

**Examples**:
- ∂³𝓟/∂δ²∂σ → Allocation Curvature–Uncertainty Coupling
- ∂³𝓟/∂σ²∂t → Uncertainty Curvature–Temporal Coupling

---

## Advantages Over Previous System

### 1. **Completely Unambiguous**

**Old**: "Confidence Sensitivity"
- Which direction? Δ to σ or σ to Δ?
- What order? Could be interpreted multiple ways

**New**: "Allocation–Uncertainty Coupling"
- Clear: Second-order cross-derivative
- Symmetric: Order doesn't matter
- Precise: Both dimensions named

### 2. **Mathematically Constructive**

Given a formula, you can construct the name:

```
∂³𝓟/∂δ²∂t
│ │  │ │ │
│ │  │ │ └─ "Temporal"
│ │  └─┴─── "Allocation²" → "Allocation Curvature"
│ └──────── Order 3: "Coupling" (mixed)
└────────── Result: "Allocation Curvature–Temporal Coupling"
```

### 3. **Predictable Extensions**

Want to name ∂²𝓟/∂r∂t (doesn't have traditional name)?
- Two different variables → Coupling
- Opportunity and Temporal dimensions
- **Name**: "Opportunity–Temporal Coupling"

Want to name ∂³𝓟/∂r²∂σ?
- Order 3, mixed
- r appears twice → "Opportunity Curvature"
- Also has σ → "Coupling"
- **Name**: "Opportunity Curvature–Uncertainty Coupling"

### 4. **Teaches the Mathematics**

**Old**: "Charm measures delta decay"
- Must memorize

**New**: "Allocation–Temporal Coupling"
- Name tells you: ∂(Allocation Response)/∂t
- Immediately understand what's being measured

### 5. **Hierarchical Understanding**

You can see the progression:
1. **Allocation Response** (Δ) → First-order
2. **Allocation Curvature** (Γ) → Second-order (convexity)
3. **Allocation Acceleration** → Third-order (rate of curvature change)

Or the interaction chain:
1. **Allocation Response** → Varies with allocation
2. **Allocation–Uncertainty Coupling** → How that varies with uncertainty
3. **Allocation Curvature–Uncertainty Coupling** → How the curvature of that varies with uncertainty

---

## Complete Mapping Table

| Traditional | Systematic Name | Type | Formula |
|------------|-----------------|------|---------|
| Delta (Δ) | Allocation Response | 1st Order | ∂𝓟/∂δ |
| Vega (ν) | Uncertainty Response | 1st Order | ∂𝓟/∂σ |
| Theta (Θ) | Temporal Response | 1st Order | ∂𝓟/∂t |
| Rho (ρ) | Opportunity Response | 1st Order | ∂𝓟/∂r |
| Gamma (Γ) | Allocation Curvature | 2nd Order (same) | ∂²𝓟/∂δ² |
| Vomma | Uncertainty Curvature | 2nd Order (same) | ∂²𝓟/∂σ² |
| Vanna | Allocation–Uncertainty Coupling | 2nd Order (cross) | ∂²𝓟/∂δ∂σ |
| Charm | Allocation–Temporal Coupling | 2nd Order (cross) | ∂²𝓟/∂δ∂t |
| Veta | Uncertainty–Temporal Coupling | 2nd Order (cross) | ∂²𝓟/∂σ∂t |
| Vera | Opportunity–Uncertainty Coupling | 2nd Order (cross) | ∂²𝓟/∂r∂σ |
| Speed | Allocation Acceleration | 3rd Order (same) | ∂³𝓟/∂δ³ |
| Ultima | Uncertainty Acceleration | 3rd Order (same) | ∂³𝓟/∂σ³ |
| Zomma | Allocation Curvature–Uncertainty Coupling | 3rd Order (mixed) | ∂³𝓟/∂δ²∂σ |
| Color | Allocation Curvature–Temporal Coupling | 3rd Order (mixed) | ∂³𝓟/∂δ²∂t |

---

## Pronunciation and Abbreviations

### Full Names (Formal)
- "Allocation–Uncertainty Coupling"
- "Uncertainty Response"

### Short Forms (Conversational)
- "Alloc–Uncertainty" or "AU Coupling"
- "Uncertainty Response" or "U-Response"

### Code Variables (Implementation)
```python
# Traditional (ambiguous)
vanna = compute_vanna()

# Systematic (clear)
allocation_uncertainty_coupling = compute_cross_derivative(delta, sigma)
# or abbreviated:
au_coupling = compute_cross_derivative(delta, sigma)
```

---

## Usage in Sentences

### Old System
"The high Vanna indicates we should check Zomma before applying Color adjustments."
→ Requires looking up each Greek

### Systematic System
"The high Allocation–Uncertainty Coupling indicates we should check Allocation Curvature–Uncertainty Coupling before applying Allocation Curvature–Temporal Coupling adjustments."
→ Every term is self-explanatory:
- Allocation–Uncertainty Coupling: allocation sensitivity depends on confidence
- Allocation Curvature–Uncertainty: convexity depends on confidence
- Allocation Curvature–Temporal: convexity depends on time

### With Abbreviations (Best)
"The high AU-Coupling indicates we should check ACU-Coupling before applying ACT-Coupling adjustments."

**Abbreviation key**:
- A = Allocation
- U = Uncertainty
- T = Temporal
- C = Curvature (when followed by another letter)

---

## Pattern Recognition

Once you know the rules, you can instantly decode any Greek:

**"Allocation–Temporal Coupling"**
→ Cross-derivative (coupling)
→ Between allocation (δ) and time (t)
→ Formula: ∂²𝓟/∂δ∂t

**"Uncertainty Acceleration"**
→ Third derivative (acceleration)
→ Of uncertainty (σ)
→ Formula: ∂³𝓟/∂σ³

**"Allocation Curvature–Uncertainty Coupling"**
→ Third order mixed (curvature coupling)
→ Allocation curvature (∂²/∂δ²) interacting with uncertainty (σ)
→ Formula: ∂³𝓟/∂δ²∂σ

---

## Future Greeks (Predictive Naming)

The system extends naturally to any Greek we might need:

| Formula | Systematic Name |
|---------|----------------|
| ∂²𝓟/∂r∂t | Opportunity–Temporal Coupling |
| ∂²𝓟/∂r² | Opportunity Curvature |
| ∂³𝓟/∂r³ | Opportunity Acceleration |
| ∂³𝓟/∂σ²∂t | Uncertainty Curvature–Temporal Coupling |
| ∂³𝓟/∂σ²∂r | Uncertainty Curvature–Opportunity Coupling |
| ∂⁴𝓟/∂δ⁴ | Allocation Hyper-Acceleration |
| ∂⁴𝓟/∂δ³∂σ | Allocation Acceleration–Uncertainty Coupling |

**No memorization needed** - just apply the rules!

---

## Comparison: Three Naming Systems

| Greek | Traditional | Previous Elegant | Systematic |
|-------|------------|-----------------|------------|
| Vanna | Vanna | Confidence Sensitivity | Allocation–Uncertainty Coupling |
| Charm | Charm | Urgency Gradient | Allocation–Temporal Coupling |
| Vomma | Vomma | Uncertainty Curvature | Uncertainty Curvature ✓ |
| Veta | Veta | Learning Gradient | Uncertainty–Temporal Coupling |
| Vera | Vera | Discovery Volatility | Opportunity–Uncertainty Coupling |
| Speed | Speed | Acceleration | Allocation Acceleration |
| Zomma | Zomma | Convexity Uncertainty | Allocation Curvature–Uncertainty Coupling |
| Color | Color | Convexity Maturation | Allocation Curvature–Temporal Coupling |
| Ultima | Ultima | Extremal Sensitivity | Uncertainty Acceleration |

**Systematic wins**:
- ✅ Unambiguous formula reconstruction
- ✅ Shows variable relationships
- ✅ Consistent pattern language
- ✅ Predictive for new Greeks
- ✅ Mathematical precision

---

## Implementation Strategy

### Three-Tier Notation

**Tier 1 (Academic)**: Traditional names with systematic in parentheses
> "Vanna (Allocation–Uncertainty Coupling) measures..."

**Tier 2 (Educational)**: Systematic names primary
> "The Allocation–Uncertainty Coupling (traditionally called Vanna) shows..."

**Tier 3 (Pure)**: Only systematic names
> "Allocation–Uncertainty Coupling measures how allocation sensitivity depends on confidence..."

### Documentation Format

```latex
\textbf{Allocation–Uncertainty Coupling (Vanna)}

\textbf{Mathematical Definition}: 
\[ \frac{\partial^2 \mathbb{P}(G)}{\partial \delta \partial \sigma_\beta} 
   = \frac{\partial (\text{Allocation Response})}{\partial \sigma_\beta}
   = \frac{\partial (\text{Uncertainty Response})}{\partial \delta} \]

\textbf{Type}: Second-order cross-derivative (coupling)

\textbf{Traditional name}: Vanna
```

---

## Summary

The **Systematic Greek Nomenclature** provides:

✅ **Zero ambiguity**: Name uniquely determines formula  
✅ **Compositional**: Build names from components  
✅ **Predictive**: Name any future Greek instantly  
✅ **Educational**: Names teach the mathematics  
✅ **Hierarchical**: Clear derivative structure  
✅ **Relational**: Shows connections between Greeks  
✅ **Professional**: Sophisticated without jargon  

**Core vocabulary**:
- **Response** = First derivative
- **Curvature** = Second derivative (same variable)
- **Coupling** = Cross-derivative (different variables)
- **Acceleration** = Third derivative (same variable)
- **Curvature Coupling** = Third derivative (mixed)

**Dimensions**: Allocation, Uncertainty, Temporal, Opportunity

**Formula → Name**: Mechanical transformation  
**Name → Formula**: Reverse engineering possible

This is a **generative naming system** - a grammar for Greeks! 🎯

