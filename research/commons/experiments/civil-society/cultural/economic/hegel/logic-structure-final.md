# The Logical Structure of Proportional Mutual Recognition

---

## Abstract

We prove the minimum operator is **uniquely necessary** for mutual recognition by identifying the core requirement: **perfect reciprocal proportionality** - each party's proportional claim must equal their commitment level (bounded by the other party). We show proportional structure is logically necessary for commensurable coordination, satisfaction feedback is functionally necessary for quality convergence, and non-transferability follows definitionally from autonomy. The analysis distinguishes logical necessity (no alternative), functional necessity (goal-dependent), and definitional necessity (concept-dependent).

---

## Three Types of Necessity

| Type | Definition | Example |
|------|------------|---------|
| **Logical** | No coherent alternative given requirements | min is only operator with perfect reciprocal proportionality |
| **Functional** | Required for specific outcomes | Feedback for learning |
| **Definitional** | Follows from concept definitions | Autonomy requires non-transferability |

---

## I. Logical Necessity: Proportional Structure

**Problem**: Agent A (capacity 150) and agent B (capacity 10,000) must coordinate. Absolute quantities are incommensurable.

**Theorem 1** (Proportional Necessity). For commensurable coordination across heterogeneous agents, recognition must be proportional: r_{i→j} ∈ [0,1], Σ_j r_{i→j} = 1.

**Proof**: Scale-invariance and comparability uniquely determine normalized proportional form. ∎

**Note**: Whether written as "1" or "100%" is conventional notation for the same structure.

---

## II. Logical Necessity: Minimum Operator

### The Core Requirement

**Insight**: "Mutual recognition" means each party establishes a proportional claim through their commitment. The claim must **equal the commitment** (if reciprocated).

**Definition** (Perfect Reciprocal Proportionality). An operator f has *perfect reciprocal proportionality* if:
```
MR(A,B) ∈ {r_{A→B}, r_{B→A}}
```
The mutual recognition value equals (at least) one party's actual commitment level.

### The Uniqueness Theorem

**Theorem 2** (Uniqueness of Minimum). The minimum operator is the unique symmetric, continuous operator satisfying perfect reciprocal proportionality.

**Proof by Example**:

Consider r_{A→B} = 0.8, r_{B→A} = 0.2

| Operator | Formula | Result | Equals a commitment? |
|----------|---------|--------|---------------------|
| Minimum | min(0.8, 0.2) | 0.2 | ✓ (equals r_{B→A}) |
| Average | (0.8+0.2)/2 | 0.5 | ✗ (neither) |
| Product | 0.8 × 0.2 | 0.16 | ✗ (neither) |
| Harmonic | 2/(1/0.8+1/0.2) | 0.32 | ✗ (neither) |
| Maximum | max(0.8, 0.2) | 0.8 | ✓ but asymmetric |

**Analysis**:
- **Minimum**: MR = 0.2 = Bob's commitment. Alice committed 0.8 but relationship is **bounded by** Bob's 0.2. Bob's lower commitment limits the relationship. Both parties have equal 0.2 proportional claims. ✓

- **Product**: MR = 0.16. Neither party's claim (0.16) equals their commitment (0.8 or 0.2). The commitment-claim relationship is **attenuated** by multiplication. ✗

- **Average**: MR = 0.5. Claims (0.5) equal neither commitment. Creates value not present in either commitment. ✗

- **Harmonic**: MR = 0.32. Same issue - commitment-claim disconnection. ✗

- **Maximum**: MR = 0.8 = Alice's commitment. But this violates symmetry - Alice's high value overrides Bob's low commitment. ✗

**Why Minimum Works**: 
```
If I commit r to you:
  - If you commit ≥ r: MR = r (my commitment becomes my claim)
  - If you commit < r: MR = your commitment (you limit it)
  
The relationship value IS one party's actual commitment level.
This preserves direct proportional correspondence between commitment and claim.
```

**Why Others Fail**:
- Product/Average/Harmonic: Create new values not equal to any commitment → break proportional correspondence
- Maximum: Asymmetric → higher commitment overrides lower → no mutual constraint

∎

**Corollary** (Self-Recognition): MR(A,A) = min(r_{A→A}, r_{A→A}) = r_{A→A}. Perfect self-proportionality.

### Derived Properties

From perfect reciprocal proportionality, minimum automatically satisfies:

✓ **Bilateral consent**: Both parties must participate (either = 0 → MR = 0)
✓ **Complete veto power**: Either party can set relationship to their level
✓ **Symmetry**: min(r₁, r₂) = min(r₂, r₁)
✓ **No free-riding**: Zero commitment → zero claim

These follow as consequences, not separate requirements.

---

## III. Functional Necessity: Satisfaction Feedback

**Observation**: Pure proportional allocation (a_i ∝ r_i) is logically coherent but empirically arbitrary - no correction mechanism.

**Proposition 3** (Functional Necessity). For quality convergence and learning from experience, satisfaction feedback is necessary:

```
share_i ∝ r_i × s_i    where s_i ∈ [0,1] is satisfaction
```

**Effect**:
| Recognition r_i | Satisfaction s_i | Share | Interpretation |
|----------------|------------------|--------|----------------|
| High (0.8) | High (0.9) | High (0.72) | Reinforced |
| High (0.8) | Low (0.3) | Reduced (0.24) | Corrected |
| Low (0.2) | High (0.9) | Low (0.18) | Autonomy preserved |

**Conclusion**: Not logically necessary (system coherent without it) but functionally necessary for quality-seeking behavior. ∎

**Note**: The multiplicative form (r_i × s_i) is one reasonable choice; alternatives like r_i × s_i² or r_i + λ·s_i are also viable for different feedback strengths.

---

## IV. Definitional Necessity: Autonomy Structure

**Definition** (Autonomy). An agent has *autonomy* over recognition if:
1. Recognition authority is **non-transferable** (cannot be permanently alienated)
2. If delegated, delegation is **always-revokable** (temporary management preserves ultimate control)

**Theorem 4** (Non-Transferability from Autonomy). Given autonomy, recognition must be non-transferable.

**Proof**: Permanent transferability → coercion via economic pressure → loss of self-determination → contradicts autonomy. ∎

**Example**: Alice can authorize "Bob manages my allocations for 1 month" with revocation rights. This preserves autonomy while allowing practical delegation.

---

## V. The Complete System

```
TIER 1: LOGICAL NECESSITIES
  r_{i→j} ∈ [0,1],  Σ_j r_{i→j} = 1          [proportional structure]
  MR(i,j) = min(r_{i→j}, r_{j→i})            [perfect reciprocal proportionality]

TIER 2: FUNCTIONAL ADDITIONS (for quality convergence)
  s_j^i ∈ [0,1]                               [satisfaction]
  share_j^i = (r_{i→j} · s_j^i) / Σ_k(r_{i→k} · s_k^i)
  MS(i,j) = MR(i,j) × share_j^i               [mutual satisfaction]
  a_j^i = c_i × MS(i,j) / Σ_k MS(i,k)        [allocation]

TIER 3: DEFINITIONAL CONSTRAINTS (given autonomy)
  r_{i→j} non-transferable, delegation revokable
```

---

## VI. Results Summary

| Component | Type | Core Property | Status |
|-----------|------|---------------|--------|
| Proportional (Σr=1) | Logical | Commensurability | Necessary |
| **Minimum operator** | **Logical** | **Perfect reciprocal proportionality** | **Uniquely necessary** |
| Satisfaction feedback | Functional | Quality convergence | Goal-dependent |
| Non-transferability | Definitional | Autonomy | Concept-dependent |

---

## VII. Practical Implications

**Incentive Compatibility**:
```
↑ r_non-beneficial → ↓ r_beneficial [Σr=1] → ↓ MR → ↓ allocation
```
Misallocation is self-penalizing.

**Quality Convergence** (with Tier 2):
```
High s_i → ↑ share_i → ↑ allocation → ↑ r_i toward high-s_i
```
System gravitates toward quality.

**Perfect Proportionality** (Tier 1):
```
My commitment r = my proportional claim (if reciprocated)
Direct correspondence: commitment ≡ claim
```
No attenuation, no inflation - just bounded reciprocity.

---

## VIII. Why This Is Elegant

**Single Core Property**: Perfect reciprocal proportionality uniquely determines the minimum operator.

**Everything Else Follows**:
- Bilateral consent: consequence of perfect reciprocity
- Veto power: consequence of perfect reciprocity  
- Symmetry: consequence of perfect reciprocity
- No free-riding: consequence of perfect reciprocity

**The Logic**: "Mutual recognition" means "my commitment IS my proportional claim (bounded by yours)." This single requirement → minimum is the only answer.

---

## Conclusion

We identify three necessity types:

**Logical**: Minimum operator via perfect reciprocal proportionality - commitment equals claim (bounded by other party). Proportional structure for commensurability.

**Functional**: Satisfaction feedback for quality convergence and learning.

**Definitional**: Non-transferability from autonomy (self-determination with revocable delegation).

**Core Result**: Perfect reciprocal proportionality uniquely determines the minimum operator. This is the logical necessity from which all other properties flow.

---

## Appendix: The Perfect Reciprocity Principle

**Formal Statement**: For mutual recognition to preserve direct proportional correspondence between commitment and claim:

```
∀A,B: MR(A,B) = r_{A→B}  OR  MR(A,B) = r_{B→A}
```

At least one party's commitment must equal the mutual recognition value.

**Proof that only minimum satisfies**:

For any continuous, symmetric operator f:
- If f(0.8, 0.2) ∉ {0.8, 0.2}, perfect reciprocity fails
- min(0.8, 0.2) = 0.2 ∈ {0.8, 0.2} ✓
- Any weighted combination creates values outside the commitment set ✗

∴ Minimum is uniquely necessary. ∎

---

## References

[To be added: coordination mechanisms, mechanism design, mutual recognition theory]

