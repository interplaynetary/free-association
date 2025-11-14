## Unified Entity Model with Perspectival Membership

### I. Foundational Definitions

**Entity Space**
```
E = {e₁, e₂, ..., eₙ}

Each entity e ∈ E has:
  - entity_id: string (pubkey or org_id)
  - Trivial membership: individuals have M(e, *) = [e]
  - Non-trivial membership: organizations have M(e, *) = [e₁, ..., eₖ]
```

**Recognition Tree Structure**
```
Tree(e) = (C, P)
  where:
    C = [c₁, c₂, ..., cₖ]  contributors, cᵢ ∈ E
    P = [p₁, p₂, ..., pₖ]  points, pᵢ ∈ [0,1]
    
  constraint: Σpᵢ = 1 (100%)
```

### II. Perspectival Membership

**Membership Function (Perspectival)**
```
M : E × E → P(E)
M(org, observer) = [e₁, e₂, ..., eₘ]

Properties:
  1. Non-canonical: M(org, o₁) ≠ M(org, o₂) in general
  2. Self-membership: M(pubkey, *) = [pubkey] ∀ observers
  3. Recursive: eᵢ ∈ M(org, observer) may be org_id or pubkey
```

**Subscription Function**
```
S : E → (E → E)
S(user)(org) = observer

Meaning: user chooses which observer's perspective to use for org
```

**Publication**
```
Publish(observer, org, members) : void
  
  Effect: Defines M(org, observer) = members
  
  Storage: Holster.get(observer).next('org-views').next(org)
```

### III. Membership Resolution

**Recursive Resolution**
```
R : E × E × P(E) → P(Pubkey)
R(entity, user, visited) = resolved_pubkeys

Base cases:
  R(pubkey, user, visited) = {pubkey}
  R(org, user, visited) where org ∈ visited = ∅

Recursive case:
  Let observer = S(user)(org)
  Let members = M(org, observer)
  Let visited' = visited ∪ {org}
  
  R(org, user, visited) = ⋃ R(m, user, visited')
                          m∈members
```

**Resolution Properties**
```
P1. Cycle-safe: visited prevents infinite loops
P2. Perspective-bound: depends on S(user)
P3. Transitive: flattens nested organizations
P4. Deterministic: same inputs → same outputs
```

### IV. Recognition Distribution

**Base Recognition (Pre-Expansion)**
```
Given Tree(user) = (C, P)
  where C = [c₁, ..., cₖ]
        P = [p₁, ..., pₖ]

Base recognition to contributor cᵢ:
  Recognition₀(user → cᵢ) = pᵢ

Conservation:
  Σ Recognition₀(user → cᵢ) = 1
  i
```

**Expanded Recognition (Post-Expansion)**
```
For each contributor cᵢ:
  Resolved(cᵢ) = R(cᵢ, user, ∅)
  |Resolved(cᵢ)| = count of resolved pubkeys

For each pubkey r ∈ ⋃ Resolved(cᵢ):
                    i
  Recognition(user → r) = Σ [ pᵢ / |Resolved(cᵢ)| ]
                          cᵢ: r∈Resolved(cᵢ)

Conservation (maintained):
  Σ Recognition(user → r) = 1
  r
```

**Example**
```
Tree(alice) = ([bob, org_brazil, carol], [0.2, 0.6, 0.2])

S(alice)(org_brazil) = indigenous_view
R(org_brazil, alice, ∅) = {carlos, diana, elena, fernando}

Recognition(alice → bob) = 0.2
Recognition(alice → carlos) = 0.6/4 = 0.15
Recognition(alice → diana) = 0.6/4 = 0.15
Recognition(alice → elena) = 0.6/4 = 0.15
Recognition(alice → fernando) = 0.6/4 = 0.15
Recognition(alice → carol) = 0.2

Σ = 0.2 + 4×0.15 + 0.2 = 1.0 ✓
```

### V. Mutual Recognition

**Direct Mutual Recognition**
```
For pubkeys A, B ∈ E:
  
  MR(A, B) = min(Recognition(A → B), Recognition(B → A))
```

**Organization-Inclusive Mutual Recognition**
```
For entities A, B ∈ E (possibly organizations):

Step 1: Check if A recognizes B
  B_resolved = R(B, A, ∅)
  A ∈ B_resolved ⟹ r_AB = Recognition(B → A)

Step 2: Check if B recognizes A
  A_resolved = R(A, B, ∅)
  B ∈ A_resolved ⟹ r_BA = Recognition(A → B)

Step 3: Mutual recognition
  MR(A, B) = min(r_AB, r_BA) if both exist
           = 0 otherwise
```

### VI. Resource Allocation

**Provider Capacity**
```
Provider P with:
  - Capacity: C ∈ ℝ⁺
  - Recognition: Tree(P) = (Contributors, Points)
  - Filters: F (time, location, type constraints)
```

**Step 1: Resolve Recipients**
```
Recipients = ⋃ R(cᵢ, P, ∅)
             cᵢ∈Contributors

Filter-compatible recipients:
  Recipients' = {r ∈ Recipients : matches(r, F)}
```

**Step 2: Calculate Shares**
```
For each r ∈ Recipients':

  BaseShare(r) = Recognition(P → r)
  
  NormalizedShare(r) = BaseShare(r) / Σ BaseShare(r')
                                       r'∈Recipients'
```

**Step 3: Apply Need Caps**
```
For each r ∈ Recipients':
  
  RawAllocation(P → r) = C × NormalizedShare(r)
  
  FinalAllocation(P → r) = min(RawAllocation(P → r), Need(r))
```

**Step 4: Redistribute Unused**
```
Used = Σ FinalAllocation(P → r)
Unused = C - Used

If Unused > 0:
  UnderfullRecipients = {r : FinalAllocation(r) < Need(r)}
  
  Redistribute Unused proportionally among UnderfullRecipients
```

### VII. Core Properties

**Property 1: Recognition Conservation**
```
∀ user ∈ E, ∀ Tree(user):
  
  Σ Recognition(user → r) = 1
  r∈R_all

where R_all = ⋃ R(cᵢ, user, ∅)
              cᵢ

Recognition always sums to 100%, regardless of expansion.
```

**Property 2: Perspective Independence**
```
∀ org ∈ E, ∀ u₁, u₂ ∈ E:

  S(u₁)(org) ≠ S(u₂)(org) ⟹ R(org, u₁, ∅) ≠ R(org, u₂, ∅)

Same org_id yields different recipients based on subscription choice.
```

**Property 3: No Canonical Authority**
```
∄ canonical_M(org)

All membership definitions are observer-relative.
Authority emerges from voluntary subscription.
```

**Property 4: User-Organization Duality**
```
∀ pubkey ∈ E:
  M(pubkey, *) = [pubkey]
  Tree(pubkey) behaves identically to Tree(org)

Users are organizations with singleton membership.
Organizations are users with multi-element membership.
```

**Property 5: Allocation Determinism**
```
Given fixed:
  - Tree(provider)
  - S(provider) subscription choices
  - Need functions
  - Filters

Then: Allocation is deterministic and reproducible.
```

### VIII. Governance Extensions

**Internal Recognition (Optional)**
```
Organizations may define internal recognition:
  
  Internal : E × E → [0,1]
  Internal(org, member) = proportion-setting power in org

Distribution options:

  A. Egalitarian:
     Internal(org, m) = 1/|M(org, org)| for all m

  B. Weighted:
     Internal(org, m) = f(contribution, delegation, tenure, ...)

  C. External:
     Use observer's published Internal(org, *)
```

**Capacity Declaration**
```
Organizations may declare collective capacity:
  
  Capacity(org) = resources available for allocation
  
  Distribution among members:
    - Equal split
    - Internal recognition weighted
    - Need-based
    - Hybrid (filtered by role, then weighted)
```

### IX. Network Dynamics

**Multi-Context Participation**
```
For any entity e:
  
  Contexts(e) = {org : e ∈ R(org, *, ∅) for some observer}
  
  e can simultaneously belong to unlimited contexts.
  e receives allocations from all contexts that recognize e.
```

**Transitive Recognition**
```
If org₁ recognizes org₂ at weight w₁
And org₂ recognizes entity e at weight w₂
Then (via some observer's resolution):
  Recognition(org₁ → e) = w₁ × w₂

Recognition cascades through organizational hierarchies.
```

**Perspective Competition**
```
Multiple observers publish M(org, *):
  
  Quality signals:
    - Number of subscribers
    - Outcomes (allocation effectiveness)
    - Trust in observer
    - Alignment with values
  
  Natural selection: perspectives that yield better
  outcomes attract more subscribers.
```

### X. Mathematical Guarantees

**Guarantee 1: Total Capacity Conservation**
```
For provider P with capacity C:
  
  Σ Allocation(P → r) ≤ C
  r

No over-allocation possible.
```

**Guarantee 2: Need Satisfaction**
```
For recipient r with Need(r):
  
  Allocation(* → r) ≤ Need(r)
  
No recipient receives beyond declared need.
```

**Guarantee 3: Proportional Fairness**
```
For recipients r₁, r₂ with equal filters and unbounded needs:
  
  Allocation(P → r₁)   Recognition(P → r₁)
  ─────────────────── = ─────────────────────
  Allocation(P → r₂)   Recognition(P → r₂)

Allocations strictly proportional to recognition.
```

**Guarantee 4: Strategy-Proofness**
```
Honest declaration of:
  - Recognition (who contributes)
  - Needs (what you require)
  - Membership views (who belongs)

Is the dominant strategy.
Misrepresentation reduces access to beneficial resources.
```