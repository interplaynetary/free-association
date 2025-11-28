# The Logical Structure of Proportional Mutual Recognition

---

## Abstract

We prove that the minimum operator is **uniquely necessary** for symmetric bilateral coordination with veto power. Given explicit requirements (bilateral consent, complete veto, symmetry, no free-riding), no alternative operator is coherent. We show proportional structure is logically necessary for commensurable coordination, while satisfaction feedback is functionally necessary for convergence, and non-transferability is definitionally necessary given autonomy (defined as self-determination with revocable delegation rights). The analysis distinguishes what follows from logic, from goals, and from values.

---

## Three Types of Necessity

| Type | Definition | Example |
|------|------------|---------|
| **Logical** | No coherent alternative given requirements | Commutativity: 2+3 = 3+2 |
| **Functional** | Required for specific outcomes | Feedback for learning |
| **Definitional** | Follows from concept definitions | Autonomy requires non-transferability |

---

## I. Logical Necessity: Proportional Structure

### The Commensurability Problem

**Problem**: Agent A (capacity 150) and agent B (capacity 10,000) must coordinate. Absolute quantities are incommensurable.

**Theorem 1** (Proportional Necessity). For commensurable coordination across heterogeneous agents, recognition must be proportional: r_{i→j} ∈ [0,1].

**Proof**: Scale-invariance and comparability uniquely determine proportional form. Absolute form fails both requirements. ∎

**Theorem 2** (Normalization). The sum must equal unity: Σ_j r_{i→j} = 1.

**Proof**: For "complete allocation" to have unique meaning, total must represent the whole. Self-identity: X = 100% of X. For recognition budget R: R = 1 × R. ∎

**Note**: Whether we write "1" or "100%" is conventional. Both represent identical proportional structure.

---

## II. Logical Necessity: Minimum Operator

### The Mutual Recognition Problem

**Setup**: Two recognition values r_{A→B} and r_{B→A} must synthesize into one relationship value MR(A,B).

**Requirements**:
1. Bilateral consent (both must participate)
2. Complete veto power (either can set relationship to zero)
3. Symmetry (neither privileged)
4. No free-riding (no benefit without participation)

**Theorem 3** (Uniqueness of Minimum). Given requirements 1-4, the minimum operator f(r₁, r₂) = min(r₁, r₂) is uniquely necessary.

**Proof by Elimination**:

| Operator | Formula | Test: (r₁=1, r₂=0) | Verdict |
|----------|---------|-------------------|---------|
| Average | (r₁+r₂)/2 | = 0.5 | ✗ Free-riding: r₂=0 yet relationship=0.5 |
| Maximum | max(r₁,r₂) | = 1.0 | ✗ Unilateral: r₁ forces high value |
| Product | r₁ × r₂ | = 0 | ✓ Veto, but wrong type (independence) |
| Harmonic | 2/(1/r₁+1/r₂) | undefined | ~ Attenuated veto (not complete) |
| **Minimum** | **min(r₁,r₂)** | **= 0** | **✓ Satisfies all requirements** |

**Analysis**:
- Average: Violates requirements 1, 4 (party 2 contributes nothing, benefits anyway)
- Maximum: Violates requirements 1, 3 (unilateral inflation)
- Product: Treats recognitions as independent (but they're mutually constitutive)
- Harmonic: Closest alternative, provides *partial* veto but not *complete* veto
- Minimum: Only operator satisfying all four requirements

∎

**Corollary**: If requirement 2 is weakened to "attenuated veto," harmonic mean is viable. If asymmetry is allowed, weighted geometric mean is viable.

**Corollary** (Self-Recognition): MR(A,A) = min(r_{A→A}, r_{A→A}) = r_{A→A} (pure autonomy).

### The Master-Slave Test

Unilateral structure: Master gives r_{M→S} = 0, receives r_{S→M} = 1.

| Operator | Result | Correct? |
|----------|--------|----------|
| Average | 0.5 | ✗ (falsely suggests relationship) |
| Maximum | 1.0 | ✗ (master gets full value) |
| **Minimum** | **0** | **✓ (correctly: no mutual relationship)** |

Only minimum correctly identifies that coerced recognition is not genuine recognition. ∎

---

## III. Functional Necessity: Satisfaction Feedback

**Observation**: Pure proportional allocation a_i ∝ r_i is **logically coherent** but empirically arbitrary.

**Proposition 4** (Functional Necessity). For outcomes:
- Quality convergence
- Learning from experience  
- Reality correction

satisfaction feedback is necessary.

**Mechanism**:
```
Without feedback:  share_i ∝ r_i              (arbitrary, persistent)
With feedback:     share_i ∝ r_i × s_i        (corrected by reality)
```

where s_i ∈ [0,1] is satisfaction from provider i.

**Effect**:
- High r_i, high s_i → high share (reinforced)
- High r_i, low s_i → reduced share (corrected)
- Low r_i, high s_i → low share (autonomy preserved)

**Conclusion**: Not logically necessary (system coherent without it), but functionally necessary for quality-seeking behavior. ∎

---

## IV. Definitional Necessity: Autonomy Structure

**Definition** (Autonomy). An agent has *autonomy* over recognition if:
1. Recognition authority is **non-transferable** (cannot be sold or permanently given away)
2. If delegated (temporary management), delegation is **always-revokable** (agent retains ultimate control)

This encompasses both direct control and delegated management while preserving self-determination.

**Theorem 5** (Autonomy Requires Non-Transferability). Given the autonomy definition, recognition authority must be non-transferable.

**Proof**: If transferable (permanently alienable) → coercion via economic pressure ("sell or starve") → loss of self-determination → violates autonomy. ∎

**Corollary** (Revocable Delegation). Temporary delegation with revocation rights preserves autonomy (agent retains ultimate control).

**Example**: Alice can authorize Bob to "manage my recognition allocations for 1 month" as long as Alice can revoke this authorization at any time. This delegation doesn't violate autonomy because control is never permanently alienated.

**Conclusion**: Non-transferability is definitionally necessary given autonomy as self-determination with revocable delegation rights. ∎

---

## V. The Complete System

**Three-Tier Structure**:

```
TIER 1: LOGICAL NECESSITIES
  r_{i→j} ∈ [0,1],  Σ_j r_{i→j} = 1          [proportional, normalized]
  MR(i,j) = min(r_{i→j}, r_{j→i})            [minimum operator]

TIER 2: FUNCTIONAL ADDITIONS (for quality convergence)
  s_j^i ∈ [0,1]                               [satisfaction]
  share_j^i = (r_{i→j} · s_j^i) / Σ_k(r_{i→k} · s_k^i)
  MS(i,j) = MR(i,j) × share_j^i
  a_j^i = c_i × MS(i,j) / Σ_k MS(i,k)

TIER 3: DEFINITIONAL CONSTRAINTS (given autonomy)
  r_{i→j} ∈ non-transferable control of i     [permanent ownership]
  Delegation allowed if always-revokable       [temporary management OK]
```

---

## VI. Results Summary

| Component | Necessity Type | Theorem | Conclusion |
|-----------|---------------|---------|------------|
| Proportional (Σr=1) | Logical | 1, 2 | Required for commensurability |
| Minimum operator | **Logical** | **3** | **Uniquely determined** |
| Satisfaction feedback | Functional | 4 | Required for convergence, not coherence |
| Non-transferability | Definitional | 5 | Follows from autonomy (non-transferable + revocable delegation) |

**Core Result**: Theorem 3 proves minimum operator is **uniquely logically necessary** given explicit requirements.

---

## VII. Practical Implications

**Incentive Compatibility**:
```
↑ r_non-beneficial → ↓ r_beneficial [Σr=1] → ↓ MR → ↓ allocation → ↓ goals
```
∴ Misallocation is self-penalizing.

**Quality Convergence** (with Tier 2):
```
High s_i → ↑ share_i → ↑ allocation → agents observe → ↑ r_i toward high-s_i
```
∴ System gravitates toward quality.

---

## VIII. Philosophical Note

**Infinity and Finitude**: The self-identity principle (X = 100% of X) applies to infinity: ∞ = 100% of ∞. Since 100% ≡ 1 = Σ parts, infinity exists as normalized distributable totality.

This is true for *any* normalized measure (probabilities, proportions). Not unique to this mechanism, but reveals interesting structure: infinity as finite distributivity.

---

## Conclusion

We distinguish three necessity types:

**Logical** (proven): Proportional structure for commensurability. Minimum operator for symmetric bilateral consent with complete veto power.

**Functional** (pragmatic): Satisfaction feedback for quality convergence and learning.

**Definitional** (value-based): Non-transferability from autonomy (self-determination with revocable delegation).

The minimum operator uniqueness (Theorem 3) is the core contribution: given explicit structural requirements, it is **uniquely determined** with **logical necessity**.

---

## References

[To be added for journal submission: coordination mechanisms, mechanism design, mutual recognition theory, distributed systems]

