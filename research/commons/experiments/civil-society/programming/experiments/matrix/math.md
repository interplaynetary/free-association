# **Fundamental Axioms of Free Association**

## **Axiom 1: Recognition Budget Constraint**
For any participant $a \in P$:
$$
\sum_{x \in P} R(a,x) = 1, \quad R(a,x) \geq 0 \ \forall x
$$

**Meaning:** Each participant has exactly 100% recognition to allocate across others.

---

## **Axiom 2: Mutual Recognition Definition**
For any two participants $a, b \in P$:
$$
MR(a,b) = \min(R(a,b), R(b,a))
$$

**Properties:**
- Symmetry: $MR(a,b) = MR(b,a)$
- Boundedness: $0 \leq MR(a,b) \leq \min(R(a,b), R(b,a))$

---

## **Axiom 3: Capacity Flow Proportionality**
For any two participants $a, b \in P$:
$$
C(a,b) = \kappa_b \cdot g(MR(a,b))
$$
Where:
- $C(a,b)$ = capacity flowing from $b$ to $a$
- $\kappa_b > 0$ = $b$'s total available capacity
- $g: [0,1] \to \mathbb{R}_{\geq 0}$ is strictly increasing: $g'(x) > 0 \ \forall x$

**Meaning:** More mutual recognition → more capacity shared.

---

## **Axiom 4: Goal Achievement Monotonicity**
For participant $a$ with goal $G$, there exists:
- A set $B \subseteq P$ of beneficial partners
- A function $f: \mathbb{R}_{\geq 0} \to [0,1]$ with $f'(x) > 0 \ \forall x$

Such that:
$$
\mathbb{P}(G) = f\left(\sum_{b \in B} C(a,b)\right)
$$

**Meaning:** Goal achievement increases with total beneficial capacity received.

---

## **Axiom 5: Capacity Independence**
For $b \in B$ and $n \notin B$:
- $C(a,b)$ contributes positively to $\mathbb{P}(G)$
- $C(a,n)$ does **not** contribute to $\mathbb{P}(G)$

**Formally:**
$$
\frac{\partial \mathbb{P}(G)}{\partial C(a,b)} > 0, \quad \frac{\partial \mathbb{P}(G)}{\partial C(a,n)} = 0
$$

---

## **Derived Mathematical Rules**

### **Rule 1: Capacity-MR Monotonicity**
From Axiom 3:
$$
\frac{dC(a,b)}{dMR(a,b)} = \kappa_b \cdot g'(MR(a,b)) > 0
$$

**Thus:** $MR(a,b) \uparrow \Rightarrow C(a,b) \uparrow$

---

### **Rule 2: MR-R Recognition Response**
From Axiom 2:

**Case A:** When $R(a,b) < R(b,a)$:
$$
\frac{\partial MR(a,b)}{\partial R(a,b)} = 1, \quad \frac{\partial MR(a,b)}{\partial R(b,a)} = 0
$$

**Case B:** When $R(a,b) > R(b,a)$:
$$
\frac{\partial MR(a,b)}{\partial R(a,b)} = 0, \quad \frac{\partial MR(a,b)}{\partial R(b,a)} = 1
$$

**Case C:** When $R(a,b) = R(b,a)$:
- Derivative undefined, but $MR(a,b)$ is continuous
- Small increases in either $R$ can increase $MR$ if the other $R$ doesn't decrease

---

### **Rule 3: Chain Rule for Goal Achievement**
Combining Axioms 3-4:
$$
\frac{\partial \mathbb{P}(G)}{\partial R(a,b)} = 
\frac{\partial \mathbb{P}}{\partial C(a,b)} \cdot \frac{dC}{dMR} \cdot \frac{\partial MR}{\partial R(a,b)}
$$

For $b \in B$:
- $\frac{\partial \mathbb{P}}{\partial C(a,b)} > 0$ (Axiom 4)
- $\frac{dC}{dMR} > 0$ (Rule 1)
- $\frac{\partial MR}{\partial R(a,b)} \geq 0$ (Rule 2)

**Thus:** $\frac{\partial \mathbb{P}(G)}{\partial R(a,b)} \geq 0$, with equality only if $\frac{\partial MR}{\partial R(a,b)} = 0$

---

### **Rule 4: Budget Transfer Opportunity Cost**
Let $b \in B$, $n \notin B$. Transfer $\delta$ recognition from $n$ to $b$:
- $R'(a,b) = R(a,b) + \delta$
- $R'(a,n) = R(a,n) - \delta$
- Budget preserved: $\sum_x R'(a,x) = 1$

The change in goal achievement:
$$
\Delta\mathbb{P} \approx 
\left[\frac{\partial \mathbb{P}}{\partial R(a,b)} - \frac{\partial \mathbb{P}}{\partial R(a,n)}\right] \delta
$$

From Axiom 5 and Rule 3: $\frac{\partial \mathbb{P}}{\partial R(a,n)} = 0$

Thus:
$$
\Delta\mathbb{P} \approx \frac{\partial \mathbb{P}}{\partial R(a,b)} \cdot \delta \geq 0
$$

**Equality only if** $\frac{\partial MR}{\partial R(a,b)} = 0$ (i.e., $R(a,b) \geq R(b,a)$)

---

## **The Fundamental Anti-Gaming Theorem**

### **Theorem Statement:**
Given Axioms 1-5, for any participant $a$ with goal $G$, beneficial set $B$, and non-beneficial set $N = P \setminus B$:

Let $T_B = \sum_{b \in B} R(a,b)$, $T_N = \sum_{n \in N} R(a,n)$.

Then:
$$
\frac{d\mathbb{P}(G)}{dT_B} \geq 0 \quad \text{and} \quad \frac{d\mathbb{P}(G)}{dT_N} \leq 0
$$

With strict inequality whenever:
1. $\exists b \in B$ with $R(a,b) < R(b,a)$
2. Capacity constraints not binding ($\kappa_b > 0$)
3. $f$ and $g$ are strictly increasing

### **Proof Sketch:**
1. **Decompose total derivative:**
   $$
   \frac{d\mathbb{P}}{dT_B} = \sum_{b \in B} \frac{\partial\mathbb{P}}{\partial R(a,b)} \cdot \frac{\partial R(a,b)}{\partial T_B}
   $$

2. **From Rule 3:** $\frac{\partial\mathbb{P}}{\partial R(a,b)} \geq 0 \ \forall b \in B$

3. **From Axiom 1:** $\frac{\partial T_B}{\partial R(a,b)} = 1$ for any $b \in B$

4. **Thus:** $\frac{d\mathbb{P}}{dT_B} = \sum_{b \in B} \frac{\partial\mathbb{P}}{\partial R(a,b)} \geq 0$

5. **For strictness:** If $\exists b$ with $R(a,b) < R(b,a)$, then $\frac{\partial\mathbb{P}}{\partial R(a,b)} > 0$

---

## **Corollary: Optimal Allocation Characterization**

### **Optimality Conditions:**
At maximum $\mathbb{P}(G)$ subject to $\sum_x R(a,x) = 1$:

1. **Zero to non-beneficial:** $R(a,n) = 0 \ \forall n \notin B$

2. **Beneficial allocation:**
   - If $R(a,b) < R(b,a)$: $\frac{\partial\mathbb{P}}{\partial R(a,b)} = \lambda$ (equal marginal benefit)
   - If $R(a,b) \geq R(b,a)$: $\frac{\partial\mathbb{P}}{\partial R(a,b)} \leq \lambda$

3. **Shadow price:** $\lambda = \text{marginal value of recognition budget}$

---

## **Elegant Reformulation**

Define the **Recognition Efficiency Ratio**:
$$
\text{RER}(a) = \frac{\sum_{b \in B} R(a,b)}{\sum_{n \notin B} R(a,n)} = \frac{T_B}{T_N}
$$

**Theorem:** Under Axioms 1-5:
$$
\mathbb{P}(G) = h(\text{RER}(a)), \quad h'(x) > 0
$$

Where $h$ is a monotonically increasing function.

**Proof:** Since $\mathbb{P}(G)$ increases with $T_B$ and decreases with $T_N$, and $T_B + T_N = 1$, then:
- Fixing $T_B$, $\mathbb{P}(G)$ decreases as $T_N$ increases
- $\text{RER} = \frac{T_B}{1 - T_B}$ increases with $T_B$
- Thus $\mathbb{P}(G)$ increases with $\text{RER}$

---

## **Minimal Sufficient Conditions**

For anti-gaming to hold, we need:

1. **Fixed recognition budget** (Axiom 1)
2. **Mutual recognition ≤ individual recognition** (Axiom 2, though other forms work)
3. **Capacity increases with mutual recognition** (Axiom 3, monotonic)
4. **Goal depends only on beneficial capacities** (Axioms 4-5)

**Key Insight:** The **min()** function in mutual recognition is crucial! It creates reciprocity requirement, but other symmetric functions could work too (e.g., geometric mean, product).

---

## **Generalization to Alternative Frameworks**

Let $MR(a,b) = \Phi(R(a,b), R(b,a))$ where $\Phi$ satisfies:
1. Symmetry: $\Phi(x,y) = \Phi(y,x)$
2. Monotonicity: $\frac{\partial\Phi}{\partial x} \geq 0$, $\frac{\partial\Phi}{\partial y} \geq 0$
3. Boundedness: $\Phi(x,y) \leq \min(x,y)$ or similar

Then anti-gaming holds if:
- $\frac{\partial C}{\partial \Phi} > 0$
- Goal depends on beneficial capacities

**The core mechanism:** Your recognition to others affects your capacity receipts only through mutual relationships, creating incentive to recognize those who recognize you back and contribute to your goals.