# The Design Space of Proportional Mutual Coordination
## A Parametric Framework

---

## Abstract

We present a parametric framework characterizing the design space of proportional mutual coordination mechanisms. Rather than prescribing a single mechanism, we identify the fundamental parameters and their constraints, mapping the space of coherent coordination structures. We show that different parameter choices yield qualitatively different mechanisms, each with distinct properties. The Free Association protocol emerges as one point in this space - characterized by symmetric minimum aggregation, satisfaction-weighted feedback, and non-transferable authority. We prove which parameters have constrained ranges (logical necessity) versus which represent genuine design choices (parametric freedom). This meta-framework enables principled mechanism design by making explicit the degrees of freedom and their consequences.

**Keywords**: mechanism design, design space, parametric frameworks, coordination theory, proportional reasoning

---

## 1. The Parametric Structure

### 1.1 Core Insight

Any proportional mutual coordination mechanism can be characterized by a parameter vector Θ = (N, A, F, T, S, D) where:

- **N** (Normalization): How recognition sums to totality
- **A** (Aggregation): How bilateral recognitions combine  
- **F** (Feedback): How empirical signals modulate allocation
- **T** (Transferability): Whether recognition authority can be delegated
- **S** (Symmetry): Whether parties have equal structural positions
- **D** (Dimensionality): Number of distinct recognition dimensions

Different Θ instantiate different mechanisms with distinct properties.

### 1.2 Notation

```
r_{i→j}^k    recognition from i to j on dimension k
ω(·)         aggregation operator
φ(·)         feedback function
τ            transferability ∈ {0,1}
σ            symmetry parameter ∈ [0,1]
d            dimensionality ∈ ℕ
```

---

## 2. Parameter Space Analysis

### 2.1 Parameter N: Normalization

**Definition**. Recognition budget normalization constant.

**Constraint**: For commensurability, must satisfy:
```
Σ_j r_{i→j} = k  where k is constant across all i
```

**Theorem 2.1** (Normalization Constraint). For scale-invariant commensurable coordination, k must be fixed and finite.

*Proof*. If k varies across agents or is infinite, recognition values are incommensurable. ∎

**Design Freedom**: The value of k is conventional. Common choices:
- k = 1 (normalized to unity)
- k = 100 (percentage)
- k = n (number of entities)

**Canonical Form**: We use k = 1 without loss of generality (rescaling).

**Property**: All choices with finite k are isomorphic. ∴ N is constrained to finite normalization but value is free parameter.

---

### 2.2 Parameter A: Aggregation Operator

**Definition**. Function ω: [0,1]² → [0,1] combining bilateral recognitions.

```
MR(i,j) = ω(r_{i→j}, r_{j→i})
```

**Design Space**: The space of aggregation operators Ω is large. Key examples:

| Operator | Formula | Properties |
|----------|---------|------------|
| Minimum | ω(x,y) = min(x,y) | Complete veto, symmetric |
| Harmonic | ω(x,y) = 2xy/(x+y) | Attenuated veto, symmetric |
| Geometric | ω(x,y) = (x^α · y^(1-α)) | Weighted, asymmetric if α≠0.5 |
| Average | ω(x,y) = (x+y)/2 | No veto, symmetric |
| Product | ω(x,y) = xy | Probabilistic, symmetric |

**Theorem 2.2** (Aggregation Constraints). For mutual coordination, ω must satisfy:
1. **Boundary**: ω(0,y) = 0 ∀y (either party can nullify)
2. **Monotonicity**: ω increasing in both arguments
3. **Commutativity**: ω(x,y) = ω(y,x) for symmetric relations

*Proof*. (1) Required for genuine mutuality - either party can decline participation. (2) More recognition from either party shouldn't decrease mutuality. (3) Symmetry requires identical treatment. ∎

**Design Freedom**: Within these constraints, infinite valid operators exist.

**Characterization Theorem 2.3**. The aggregation operator ω uniquely determines:
- **Veto power**: How much one party can constrain the other
- **Aggregation style**: Pessimistic (min) vs optimistic (max) vs balanced

**Example Comparison**:
```
r_{A→B} = 0.8, r_{B→A} = 0.2

min:      MR = 0.2  (complete veto - pessimistic)
harmonic: MR = 0.32 (attenuated veto)  
geometric: MR = 0.40 (balanced)
average:  MR = 0.50 (no veto - optimistic)
```

**Selection Principle**: Choose ω based on desired veto strength:
- **Complete veto** (min): Either party can fully control relationship
- **Attenuated veto** (harmonic): Lower value dominates but doesn't dictate
- **Balanced** (geometric): Compromise between values
- **No veto** (average): Both equally weighted regardless

---

### 2.3 Parameter F: Feedback Function

**Definition**. Function φ: ℝⁿ → [0,1] mapping empirical signals to allocation weights.

```
share_j^i = φ(r_{i→j}, s_j^i, history, ...)
```

**Design Space**: Three primary classes:

**Class F₀: No Feedback**
```
share_j^i = r_{i→j} / Σ_k r_{i→k}
```
Properties:
- Pure subjective recognition
- No learning or adaptation
- Logically coherent but non-correcting

**Class F₁: Simple Multiplicative**
```
share_j^i = (r_{i→j} · s_j^i) / Σ_k (r_{i→k} · s_k^i)
```
Properties:
- Satisfaction modulates recognition
- Preserves autonomy (0 recognition → 0 share regardless of satisfaction)
- Simple convergence dynamics

**Class F₂: Complex Feedback**
```
share_j^i = φ(r_{i→j}, s_j^i, s_history, network_effects, ...)
```
Properties:
- Incorporates history, network structure, other signals
- Richer dynamics but more complex
- May include EMA, reputation systems, etc.

**Theorem 2.4** (Feedback Trade-offs). 
- F₀: Preserves pure autonomy, no convergence guarantee
- F₁: Balances autonomy with learning, convergence under conditions
- F₂: More adaptive but harder to analyze

**Design Freedom**: F is entirely free parameter. Choice depends on goals:
- Goal: preserve pure preference → choose F₀
- Goal: quality convergence → choose F₁ or F₂
- Goal: rich adaptation → choose F₂

---

### 2.4 Parameter T: Transferability

**Definition**. Binary parameter τ ∈ {0,1} indicating whether recognition authority can be transferred.

```
τ = 0: Recognition non-transferable (inalienable)
τ = 1: Recognition transferable (delegable, sellable)
```

**Properties**:

**τ = 0 (Non-transferable)**:
- Strong autonomy (inalienable control)
- No markets in recognition
- Protection against coercion via economic pressure
- Each agent permanently sovereign over their recognition

**τ = 1 (Transferable)**:
- Weak autonomy (current control but alienable)
- Markets in recognition possible
- Enables delegation, specialization
- Vulnerable to economic coercion

**Theorem 2.5** (Transferability Implications). 
- If τ = 1 and economic inequality exists, recognition can concentrate
- If τ = 0, recognition distribution reflects current agents' choices only

**Design Freedom**: T is completely free parameter. Choice depends on normative commitment:
- Value: inalienable agency → choose τ = 0
- Value: market efficiency → choose τ = 1

---

### 2.5 Parameter S: Symmetry

**Definition**. Parameter σ ∈ [0,1] indicating degree of symmetry in aggregation.

For asymmetric aggregation:
```
MR(i,j) = ω_σ(r_{i→j}, r_{j→i}, σ)
```

**Examples**:

**σ = 0.5 (Fully Symmetric)**:
```
ω(r₁, r₂) = min(r₁, r₂)  or  (r₁ + r₂)/2
```
Both parties treated identically.

**σ ≠ 0.5 (Asymmetric)**:
```
ω_σ(r₁, r₂) = r₁^σ · r₂^(1-σ)
```
Party 1 weighted σ, party 2 weighted (1-σ).

**Use Cases**:
- σ = 0.5: Equal partners
- σ = 0.7: Expert-novice (expert's recognition weighted higher)
- σ = 0.9: Mentor-mentee (mentor's recognition dominant)

**Theorem 2.6** (Symmetry Constraint). For symmetric mutual coordination, must have σ = 0.5.

**Design Freedom**: S is free parameter. Symmetric relations require σ = 0.5, but asymmetric relations can use any σ ∈ (0,1).

---

### 2.6 Parameter D: Dimensionality

**Definition**. Number d ∈ ℕ of distinct recognition dimensions.

**d = 1 (Unidimensional)**:
```
r_{i→j} ∈ [0,1]  (single value)
Σ_j r_{i→j} = 1
```

**d > 1 (Multi-dimensional)**:
```
r_{i→j} = (r_{i→j}^1, r_{i→j}^2, ..., r_{i→j}^d)
Σ_j r_{i→j}^k = 1  ∀k
```

**Example (d=3)**:
- Dimension 1: Competence/Quality
- Dimension 2: Trustworthiness  
- Dimension 3: Availability/Reliability

Each dimension sums to 1 independently.

**Aggregation with Multi-dimensional Recognition**:
```
MR(i,j) = (ω₁(r_{i→j}^1, r_{j→i}^1), 
           ω₂(r_{i→j}^2, r_{j→i}^2),
           ...
           ω_d(r_{i→j}^d, r_{j→i}^d))
```

Different dimensions can use different aggregation operators.

**Design Freedom**: D is free parameter. Choice depends on richness needed:
- Simple contexts: d = 1
- Complex relationships: d > 1

---

## 3. Mechanism Instantiation

### 3.1 The Parametric Framework

Any coordination mechanism is an instantiation:
```
M(Θ) where Θ = (N, A, F, T, S, D)
```

**Constrained Parameters** (forced by logic):
- N: Must be finite (constrained to k ∈ ℝ₊, value free)

**Free Parameters** (design choices):
- A: Choice of ω ∈ Ω (subject to constraints in Theorem 2.2)
- F: Choice of φ (no constraints)
- T: Choice of τ ∈ {0,1}
- S: Choice of σ ∈ [0,1]
- D: Choice of d ∈ ℕ

### 3.2 Example Mechanisms

**Free Association (Standard)**:
```
Θ_FA = (1, min, F₁, 0, 0.5, 1)

N = 1      (normalized to unity)
A = min    (complete veto)
F = F₁     (satisfaction-weighted)
T = 0      (non-transferable)
S = 0.5    (symmetric)
D = 1      (single dimension)
```

**Free Association (Attenuated)**:
```
Θ_FAₐ = (1, harmonic, F₁, 0, 0.5, 1)

A = harmonic  (attenuated veto instead of complete)
(all else same)
```

**Expert Network**:
```
Θ_EN = (1, geometric(0.7), F₂, 0, 0.7, 3)

A = geometric with α=0.7  (asymmetric, experts weighted higher)
F = F₂                     (complex feedback with history)
S = 0.7                    (asymmetric)
D = 3                      (competence, trust, availability)
```

**Market-Augmented**:
```
Θ_MA = (1, min, F₀, 1, 0.5, 1)

F = F₀  (no feedback - pure subjective)
T = 1   (transferable - markets allowed)
```

### 3.3 Properties by Parameter

**Theorem 3.1** (Convergence). Quality convergence requires F ∈ {F₁, F₂}.

**Theorem 3.2** (Veto Strength). Complete veto iff A = min.

**Theorem 3.3** (Autonomy). Strong autonomy iff T = 0.

**Theorem 3.4** (Symmetry). Symmetric relations iff S = 0.5.

---

## 4. Design Principles

### 4.1 Parameter Selection Guide

**For Symmetric Peer Coordination**:
```
Θ_peer = (1, min, F₁, 0, 0.5, 1)
- Complete veto (either can decline)
- Quality feedback (convergence)
- Non-transferable (inalienable)
- Symmetric (peers)
```

**For Expert-Novice Coordination**:
```
Θ_expert = (1, geometric(α), F₁, 0, α, 1)
- Weighted toward expert (α > 0.5)
- Quality feedback
- Asymmetric
```

**For Trust-Sensitive Coordination**:
```
Θ_trust = (1, min, F₁, 0, 0.5, 2)
- Two dimensions: quality, trust
- Both must be mutually recognized
- Minimum on each dimension
```

**For Market-Based Coordination**:
```
Θ_market = (1, average, F₀, 1, 0.5, 1)
- No veto (market clearing)
- No feedback (pure preference)
- Transferable (markets)
```

### 4.2 The Constraint Space

**Hard Constraints** (logical necessity):
```
• N finite
• A satisfies Theorem 2.2 (boundary, monotonicity)
• If symmetric relations: S = 0.5
```

**Soft Constraints** (functional requirements):
```
• Quality convergence requires: F ≠ F₀
• Complete veto requires: A = min
• Strong autonomy requires: T = 0
```

**No Constraints**:
```
• Specific value of N (convention)
• Choice within valid A operators
• Choice of F complexity
• Choice of D dimensionality
```

---

## 5. The Design Space Topology

### 5.1 Distance Metric

Define distance between mechanisms:
```
dist(M₁, M₂) = w_N |N₁ - N₂| + w_A d_A(A₁, A₂) + ... 
```

where d_A measures functional difference between operators.

### 5.2 Equivalence Classes

**Definition 5.1**. Mechanisms M₁, M₂ are *functionally equivalent* if they produce identical allocations for all inputs.

**Theorem 5.1** (Normalization Equivalence). All mechanisms differing only in N (with finite N) are functionally equivalent.

*Proof*. Rescaling preserves proportions. ∎

**Corollary 5.2**. The design space modulo equivalence is:
```
Space = Ω × Φ × {0,1} × [0,1] × ℕ
       (A    F      T      S     D)
```

### 5.3 Continuity

**Observation 5.3**. Small changes in A create continuous changes in behavior:
```
min → harmonic → geometric → arithmetic

Veto strength:  complete → strong → moderate → weak
```

**Observation 5.4**. Discrete parameters create discontinuous changes:
- T: 0→1 creates qualitative shift (inalienable → alienable)
- D: 1→2 adds entire new dimension of evaluation

---

## 6. Optimality and Trade-offs

### 6.1 No Universal Optimum

**Theorem 6.1** (No Dominant Mechanism). No mechanism M(Θ) dominates all others across all contexts and values.

*Proof by construction*. Consider two contexts:
- Context 1: Symmetric peers requiring mutual consent
  - Optimal: Θ = (1, min, F₁, 0, 0.5, 1)
- Context 2: Expert network with knowledge asymmetry
  - Optimal: Θ = (1, geometric(α), F₁, 0, α, d)

No single Θ optimizes both. ∎

### 6.2 Trade-off Space

**Fundamental Trade-offs**:

**Veto Power ↔ Flexibility**:
- Strong veto (min) → protects autonomy, may block beneficial coordination
- Weak veto (avg) → enables coordination, vulnerable to exploitation

**Feedback ↔ Autonomy**:
- Strong feedback (F₂) → quality convergence, reduces pure preference
- No feedback (F₀) → pure preference, no quality correction

**Transferability ↔ Stability**:
- Transferable (T=1) → enables markets, allows concentration
- Non-transferable (T=0) → stable distribution, no markets

**Dimensionality ↔ Complexity**:
- High D → rich evaluation, complex to manage
- Low D → simple, may miss important distinctions

### 6.3 Context-Dependent Selection

**Principle**: Select Θ by:
1. Identify context requirements (symmetric? expert structure? trust-sensitive?)
2. Identify values (strong autonomy? quality convergence? market efficiency?)
3. Select parameters satisfying requirements and values
4. Analyze trade-offs explicitly

---

## 7. The Meta-Theorem

**Theorem 7.1** (Parametric Completeness). Any proportional mutual coordination mechanism satisfying commutativity and boundary conditions can be represented as M(Θ) for some Θ in the design space.

*Sketch*. 
- N captures normalization (must be finite for commensurability)
- A captures aggregation (constrained by Theorem 2.2)
- F captures feedback (unconstrained)
- T captures transferability (binary choice)
- S captures asymmetry (continuous parameter)
- D captures dimensionality (natural number)

Together these span the space of coherent mechanisms. ∎

**Corollary 7.2**. The Free Association protocol is not unique but sits at specific coordinates:
```
Θ_FA = (1, min, F₁, 0, 0.5, 1)
```
Other coherent mechanisms exist at other coordinates.

---

## 8. Navigation Principles: Selecting Coordinates

### 8.1 The Selection Problem

**Question**: Given a coordination context, which Θ should we choose?

**Answer**: No universal answer, but principled methodology exists.

### 8.2 The Four-Step Navigation Framework

**Step 1: Context Analysis** → Identify structural requirements

**Step 2: Value Specification** → Identify normative commitments  

**Step 3: Functional Analysis** → Identify desired outcomes

**Step 4: Trade-off Resolution** → Balance competing considerations

---

### 8.3 Step 1: Context Analysis

**Principle 8.1** (Context Determines Constraints). Analyze coordination context to identify forced parameters.

**Decision Tree for Aggregation (A)**:

```
Context: What kind of relationships?
│
├─ Symmetric peers (equal standing)?
│  │
│  ├─ YES: Set S = 0.5
│  │  │
│  │  └─ Need complete veto power?
│  │     ├─ YES: A = min
│  │     ├─ PARTIAL: A = harmonic
│  │     └─ NO: A = geometric(0.5) or average
│  │
│  └─ NO: Asymmetric relations
│     │
│     └─ Expert-novice? Authority-subordinate?
│        ├─ Expert dominant: A = geometric(α), α > 0.5, S = α
│        └─ Balanced asymmetry: A = geometric(α), 0.5 < α < 0.7
│
└─ Multiple relationship types?
   └─ Consider D > 1 (multi-dimensional)
```

**Decision Tree for Dimensionality (D)**:

```
Context: Evaluation complexity?
│
├─ Single criterion sufficient? (e.g., pure quality)
│  └─ D = 1
│
├─ Multiple independent criteria? (e.g., quality AND trust)
│  └─ D = number of distinct criteria
│  
└─ Hierarchical criteria? (quality has sub-dimensions)
   └─ D = flattened dimension count or nested structure
```

**Examples**:

**Context: Academic Peer Review**
- Symmetric: YES (peers reviewing peers) → S = 0.5
- Veto needed: YES (either can reject) → A = min
- Dimensions: Multiple (originality, rigor, clarity) → D = 3

**Context: Apprenticeship**
- Symmetric: NO (master-apprentice) → S ≠ 0.5
- Master judgment weighted: α = 0.8 → A = geometric(0.8)
- Single dimension: Quality of work → D = 1

---

### 8.4 Step 2: Value Specification

**Principle 8.2** (Values Determine Normative Parameters). Identify core values to fix T and aspects of F.

**Decision Tree for Transferability (T)**:

```
Value: What conception of autonomy?
│
├─ Strong autonomy (inalienable self-determination)?
│  └─ T = 0 (non-transferable)
│
├─ Weak autonomy (current control, alienable)?
│  └─ T = 1 (transferable)
│
└─ Collective autonomy (delegation acceptable)?
   └─ T = 1 with governance constraints
```

**Decision Tree for Feedback (F)**:

```
Value: Primacy of subjective preference vs empirical grounding?
│
├─ Pure subjective preference (no objective quality)?
│  └─ F = F₀ (no feedback)
│
├─ Balance subjective and objective?
│  └─ F = F₁ (simple multiplicative feedback)
│
├─ Empirical grounding primary?
│  └─ F = F₂ (strong feedback, rich signals)
│
└─ Democratic: subjective, but informed by collective wisdom?
   └─ F = F₁ or F₂ with reputation systems
```

**Value-Parameter Mapping Table**:

| Value Commitment | Parameter Implications |
|-----------------|------------------------|
| Inalienable dignity | T = 0, S = 0.5 |
| Market efficiency | T = 1, F = F₀ |
| Quality convergence | F ∈ {F₁, F₂} |
| Protective autonomy | A = min, T = 0 |
| Collaborative flexibility | A = harmonic or geometric |
| Expertise respect | S > 0.5 for experts |
| Democratic equality | S = 0.5, F = F₁ |

---

### 8.5 Step 3: Functional Analysis

**Principle 8.3** (Goals Determine Functional Parameters). Desired outcomes constrain parameter choices.

**Goal-Parameter Requirements**:

```
Goal: Quality convergence over time
├─ REQUIRES: F ≠ F₀
├─ REQUIRES: Some form of empirical feedback
└─ SUGGESTS: F = F₁ (simple) or F₂ (complex)

Goal: Stable long-run allocation
├─ REQUIRES: F with convergence guarantees
├─ SUGGESTS: F₁ with EMA smoothing
└─ AVOID: F₀ (no convergence)

Goal: Rapid adaptation to changes
├─ REQUIRES: F₂ with responsive feedback
├─ SUGGESTS: Lower α in EMA (faster response)
└─ TRADE-OFF: Less stability

Goal: Protection against single-party failure
├─ REQUIRES: A with veto property
├─ SUGGESTS: A = min (complete protection)
└─ ALTERNATIVE: A = harmonic (attenuated protection)

Goal: Encourage exploration/discovery
├─ SUGGESTS: F₁ with diversity bonus
├─ SUGGESTS: A = harmonic (less harsh than min)
└─ AVOID: A = min with F₀ (lock-in without learning)

Goal: Market clearing (everyone transacts)
├─ REQUIRES: A without strong veto
├─ SUGGESTS: A = average or geometric(0.5)
└─ COMPATIBLE: T = 1 (transferable)
```

**Functional Incompatibilities**:

```
⊗  F₀ + Goal(quality convergence)
   Pure preference without feedback cannot converge to quality

⊗  A = average + Goal(veto protection)  
   No-veto aggregation cannot provide protection

⊗  T = 1 + Goal(prevent concentration)
   Transferability enables concentration under inequality

⊗  D = 1 + Complex multi-criteria evaluation
   Single dimension cannot capture multiple independent criteria
```

---

### 8.6 Step 4: Trade-off Resolution

**Principle 8.4** (Optimize Along Pareto Frontier). When goals conflict, choose parameters on Pareto frontier.

**Trade-off Space Map**:

```
                    Protection (Veto Strength)
                           ↑
                           │
        A = min ●          │          
                │          │          
    A = harmonic ●         │          
                │          │          
  A = geometric  ●         │          
                │          │          
    A = average  ●─────────┼─────────→ Flexibility
                           │
                           │
```

**Resolution Strategies**:

**Strategy 1: Lexicographic Ordering**
1. Rank goals by priority
2. Satisfy highest priority first
3. Among solutions satisfying top priority, optimize for second priority
4. Continue down priority list

*Example*:
```
Priority 1: Protection from coercion (T = 0)
Priority 2: Quality convergence (F = F₁)
Priority 3: Flexibility in coordination (A = harmonic, not min)

Result: Θ = (1, harmonic, F₁, 0, 0.5, 1)
```

**Strategy 2: Weighted Optimization**
1. Assign weights to each goal: w₁, w₂, ..., wₙ
2. Define utility function: U(Θ) = Σ wᵢ · gᵢ(Θ)
3. Choose Θ maximizing U(Θ)

*Example*:
```
w₁ = 0.5 (protection)
w₂ = 0.3 (flexibility)  
w₃ = 0.2 (quality convergence)

Evaluate:
- Θ₁ = (1, min, F₁, 0, 0.5, 1):     U = 0.5(1.0) + 0.3(0.3) + 0.2(1.0) = 0.79
- Θ₂ = (1, harmonic, F₁, 0, 0.5, 1): U = 0.5(0.8) + 0.3(0.7) + 0.2(1.0) = 0.81
- Θ₃ = (1, average, F₁, 0, 0.5, 1):  U = 0.5(0.0) + 0.3(1.0) + 0.2(1.0) = 0.50

Choose Θ₂ (harmonic)
```

**Strategy 3: Satisficing**
1. Define minimum acceptable threshold for each goal
2. Choose any Θ satisfying all thresholds
3. Among satisficing solutions, apply other criteria (simplicity, familiarity, etc.)

*Example*:
```
Threshold₁: Veto strength ≥ 0.7 (A ∈ {min, harmonic})
Threshold₂: Quality convergence (F ≠ F₀)
Threshold₃: Inalienable (T = 0)

Satisficing set: {(1, min, F₁, 0, 0.5, 1), (1, harmonic, F₁, 0, 0.5, 1)}
Among these: Choose min for simplicity
```

**Strategy 4: Robustness**
1. Identify uncertainty about context or values
2. Choose Θ robust across uncertainty range
3. Prefer parameters less sensitive to perturbations

*Example*:
```
Uncertainty: Exact symmetry level unclear (0.45 ≤ S ≤ 0.55)

Options:
- Symmetric (S = 0.5): Works well across range
- Asymmetric (S = 0.6): May be wrong if symmetry appropriate

Robustness favors: S = 0.5
```

---

### 8.7 Complete Navigation Example

**Scenario**: Designing coordination for open-source software development

**Step 1: Context Analysis**
- Relationships: Peer contributors (mostly symmetric)
- Some experienced maintainers with elevated judgment
- Multiple criteria: code quality, documentation, community interaction
- **Implications**: S ≈ 0.5 (slightly asymmetric for maintainers), D = 3

**Step 2: Value Specification**
- Value: Meritocracy (quality should matter)
- Value: Inalienable contribution rights (can't buy commit access)
- Value: Democratic among peers
- **Implications**: T = 0, F = F₁ or F₂, S = 0.5 for peers (0.6 for maintainers)

**Step 3: Functional Analysis**
- Goal: Quality code converges to top contributors
- Goal: Allow flexibility (mistakes shouldn't permanently exclude)
- Goal: Multiple criteria independently evaluated
- **Implications**: F = F₁, A = harmonic (not min), D = 3

**Step 4: Trade-off Resolution**
- Protection vs Flexibility: Slightly favor flexibility (harmonic over min)
- Quality vs Pure preference: Favor quality (F₁ over F₀)
- Simplicity vs Richness: Accept complexity for richer evaluation (D = 3)

**Result**:
```
Θ_OSS = (1, harmonic, F₁, 0, 0.5, 3)

N = 1         (normalized to unity)
A = harmonic  (attenuated veto - forgiving of mistakes)
F = F₁        (quality feedback - meritocratic)
T = 0         (non-transferable - can't buy reputation)
S = 0.5       (symmetric peers, slight modifier for maintainers)
D = 3         (code quality, documentation, community)
```

---

### 8.8 Navigation Heuristics

**Heuristic 8.1** (Start Simple). Begin with D = 1, F = F₁, S = 0.5. Add complexity only as needed.

**Heuristic 8.2** (Safety First). If unsure about veto strength, favor A = min or harmonic over average.

**Heuristic 8.3** (Match Aggregation to Trust). 
- High trust context: Can use weaker veto (average)
- Low trust context: Need stronger veto (min)

**Heuristic 8.4** (Feedback Gradient). Start with F₁. Upgrade to F₂ only if F₁ insufficient.

**Heuristic 8.5** (Transferability Caution). Default to T = 0 unless market efficiency critically important.

**Heuristic 8.6** (Dimension Parsimony). Use smallest D capturing essential distinctions. Don't add dimensions for marginal improvements.

**Heuristic 8.7** (Symmetry Default). Use S = 0.5 unless clear asymmetry justification exists.

---

### 8.9 Navigation Anti-Patterns

**Anti-Pattern 1**: "Maximize Everything"
- Trying to have complete veto AND maximum flexibility
- Impossible: These are trade-offs
- Resolution: Prioritize or weight

**Anti-Pattern 2**: "Default to Complexity"
- Starting with D = 10, F = F₂
- Creates cognitive overload, implementation difficulty
- Resolution: Start simple, add complexity incrementally

**Anti-Pattern 3**: "Ignore Context"
- Using symmetric (S = 0.5) for expert-novice relations
- Using non-transferable (T = 0) in market contexts
- Resolution: Do thorough context analysis first

**Anti-Pattern 4**: "Values Mismatch"
- Claiming strong autonomy while using T = 1
- Claiming quality focus while using F = F₀
- Resolution: Align parameters with stated values

**Anti-Pattern 5**: "Goal Conflicts Unresolved"
- Wanting quality convergence (needs F₁) but pure preference (needs F₀)
- Wanting protection (needs min) but flexibility (needs average)
- Resolution: Explicit trade-off analysis required

---

### 8.10 The Navigation Meta-Principle

**Meta-Principle** (Reflective Equilibrium). The right Θ is the one achieving reflective equilibrium among:
1. Context facts (what IS the situation?)
2. Value commitments (what do we CARE about?)
3. Functional requirements (what OUTCOMES do we want?)
4. Coherence constraints (what's LOGICALLY consistent?)

Navigate iteratively:
```
Initial Θ → Check context fit → Adjust
         → Check value alignment → Adjust
         → Check goal satisfaction → Adjust
         → Check coherence → Adjust
         → Repeat until equilibrium
```

**Equilibrium Test**: Θ is in reflective equilibrium when:
- All stakeholders can endorse it given their understanding
- No parameter change improves fit without worse trade-off elsewhere
- Justifications form coherent narrative
- Implementation seems feasible

---

## 9. Conclusion: The Design Space Perspective

Rather than claiming a single mechanism is "necessary," we have mapped the design space and provided navigation principles.

### What We've Established

**Constrained Dimensions** (logical necessity):
- Normalization must be finite (Theorem 2.1)
- Aggregation must satisfy boundary and monotonicity (Theorem 2.2)
- Symmetry fixed at 0.5 for symmetric relations (Theorem 2.6)

**Free Dimensions** (design choices):
- Choice of aggregation operator (min, harmonic, geometric, average, ...)
- Choice of feedback function (none, simple, complex)
- Choice of transferability (0 or 1)
- Choice of asymmetry parameter (for asymmetric relations)
- Choice of dimensionality (1 to many)

**Navigation Principles** (Section 8):
1. **Context Analysis**: Identify structural requirements from situation
2. **Value Specification**: Map normative commitments to parameters
3. **Functional Analysis**: Derive parameter requirements from goals
4. **Trade-off Resolution**: Balance competing considerations using lexicographic, weighted, satisficing, or robustness strategies

### Key Insight

The minimum operator is not absolutely necessary - it is necessary **given complete veto as requirement**. Different contexts and values lead to different optimal Θ:

```
Symmetric peers + complete veto       → Θ = (1, min, F₁, 0, 0.5, 1)
Symmetric peers + attenuated veto     → Θ = (1, harmonic, F₁, 0, 0.5, 1)
Expert-novice + quality focus         → Θ = (1, geometric(α), F₁, 0, α, 1)
Market + efficiency                   → Θ = (1, average, F₀, 1, 0.5, 1)
```

### What This Enables

**1. Principled Design**
- Not arbitrary invention but systematic parameter selection
- Four-step navigation framework (Section 8.2)
- Explicit trade-off analysis
- Reflective equilibrium methodology

**2. Mechanism Comparison**
- Understand mechanisms as points in parametric space
- Compare via distance metrics
- Identify equivalence classes
- Map relationships between mechanisms

**3. Context-Appropriate Selection**
- Match mechanism to context (Section 8.3)
- Align parameters with values (Section 8.4)
- Satisfy functional requirements (Section 8.5)
- Resolve trade-offs explicitly (Section 8.6)

**4. Systematic Innovation**
- Identify unexplored regions of design space
- Combine parameters in novel ways
- Test new mechanisms against requirements
- Iterate toward better fit

### The Meta-Lesson

The design space is large, structured, and navigable. Navigation requires:
- Understanding constraints (what logic forces)
- Analyzing context (what situation demands)
- Clarifying values (what principles require)
- Specifying goals (what outcomes desired)
- Resolving trade-offs (what to prioritize)

**Not chaos, not necessity, but parametric structure with principled navigation.**

Different regions serve different purposes. The framework transforms mechanism design from art into engineering: systematic exploration of a well-defined space guided by explicit principles.

The Free Association protocol (Θ_FA = (1, min, F₁, 0, 0.5, 1)) is one excellent point for symmetric peer coordination with strong autonomy and quality convergence. Other points serve other contexts equally well. The space is rich with possibilities, each justified by its fit to context, values, and goals.

---

## Appendix A: Summary Table

### Parameter Overview

| Parameter | Symbol | Type | Range | Constraint | Selection Principle |
|-----------|--------|------|-------|------------|---------------------|
| Normalization | N | Real | ℝ₊ | Finite | Convention (use 1) |
| Aggregation | A | Function | Ω | Boundary, monotone | Context + veto needs |
| Feedback | F | Function | Φ | None | Value + goals |
| Transferability | T | Binary | {0,1} | None | Autonomy conception |
| Symmetry | S | Real | [0,1] | S=0.5 if symmetric | Relationship structure |
| Dimensionality | D | Natural | ℕ | None | Evaluation complexity |

### Common Instantiations

| Mechanism | Θ | Use Case |
|-----------|---|----------|
| Free Association (Standard) | (1, min, F₁, 0, 0.5, 1) | Symmetric peers, strong veto |
| Free Association (Flexible) | (1, harmonic, F₁, 0, 0.5, 1) | Symmetric peers, attenuated veto |
| Expert Network | (1, geometric(0.7), F₁, 0, 0.7, 3) | Expert-weighted, multi-criteria |
| Market-Augmented | (1, average, F₀, 1, 0.5, 1) | Market efficiency, pure preference |
| Trust-Sensitive | (1, min, F₁, 0, 0.5, 2) | Quality + trust dimensions |
| Pure Preference | (1, min, F₀, 0, 0.5, 1) | Subjective autonomy, no feedback |

### Navigation Quick Reference

**Step 1: Context Analysis**
- Symmetric relations? → S = 0.5
- Asymmetric (expert-novice)? → S > 0.5
- Multiple criteria? → D > 1

**Step 2: Value Specification**
- Strong autonomy? → T = 0
- Quality matters? → F ≠ F₀
- Democratic equality? → S = 0.5

**Step 3: Functional Analysis**
- Need quality convergence? → F = F₁ or F₂
- Need protection? → A ∈ {min, harmonic}
- Need flexibility? → A ∈ {harmonic, geometric, average}

**Step 4: Trade-off Resolution**
- Protection vs Flexibility: Choose along {min, harmonic, geometric, average}
- Autonomy vs Feedback: Balance F strength
- Simplicity vs Richness: Choose D carefully

### Design Space Size

```
Space = ℝ₊ × Ω × Φ × {0,1} × [0,1] × ℕ
        (N)  (A)  (F)    (T)    (S)    (D)

Isomorphic reduction (N equivalence):
Space ≅ Ω × Φ × {0,1} × [0,1] × ℕ

Dimensions:
- A: Infinite (function space)
- F: Infinite (function space)  
- T: 2 (binary)
- S: Uncountably infinite (continuous)
- D: Countably infinite (natural numbers)

Total: Infinite but structured
```

---

## Appendix B: Navigation Flowchart

```
START
  ↓
[1. CONTEXT ANALYSIS]
  ↓
Symmetric relations? ──YES──→ Set S = 0.5
  │                           ↓
  NO                    Need complete veto? ──YES──→ A = min
  ↓                           │
Set S ≠ 0.5                   NO
(based on asymmetry)          ↓
  ↓                     A ∈ {harmonic, geometric, average}
  └──────────→ [2. VALUE SPECIFICATION]
                    ↓
              Strong autonomy? ──YES──→ T = 0
                    │
                    NO
                    ↓
                  T = 1
                    ↓
              Quality important? ──YES──→ F ≠ F₀
                    │
                    NO
                    ↓
                  F = F₀
                    ↓
              [3. FUNCTIONAL ANALYSIS]
                    ↓
              Multiple criteria? ──YES──→ D > 1
                    │
                    NO
                    ↓
                  D = 1
                    ↓
              [4. TRADE-OFF RESOLUTION]
                    ↓
              Apply strategy:
              - Lexicographic
              - Weighted optimization
              - Satisficing
              - Robustness
                    ↓
              Final Θ = (N, A, F, T, S, D)
                    ↓
              Reflective equilibrium check
                    ↓
              Satisfied? ──NO──→ [ITERATE]
                    │
                   YES
                    ↓
                  END
```

---

*The design space is not chaos but parametric structure. Navigation is not art but principled engineering.*

