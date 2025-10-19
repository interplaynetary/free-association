# Collective Recognition

We use free-association to allocate resources through provider capacity declarations!

## How do providers allocate to needs?

{% content-ref url="donate" %}
[donate](https://playnet.gitbook.io/docs/donate)
{% endcontent-ref %}

**Core Concepts:**

Your Recognition = your acknowledgment of contributions towards your own self-actualization  
Your Total-Recognition = 100%

Mutual-Recognition(You, Them) = min(Their-share-of-Your-total-recognition, Your-share-of-Their-total-recognition)

**Allocation Process:**

1\) **Members submit their needs** (via free-association interface)

* Alice: needs $1000/month
* Bob: needs $500/month
* Charlie: needs $200/month

2\) **Members recognize contributions** (via free-association interface)

* Each member recognizes others' contributions towards their own self-actualization
* When members mutually recognize each other, they establish mutual-recognition percentages

3\) **Members declare capacities** (their perspective of what exists)

* Any member can declare: "From my perspective, Collective X can allocate $5000/month to {Alice, Bob, Charlie, Dave}"
* These are capacity mappings - each member's view of what resources exist and who they could support
* **Key: Only the actual provider's capacity matters for real allocation**
* Non-provider declarations are just perspectives; provider's declaration is what allocates

4\) **Protocol computes collective-recognition-shares** (within each declared capacity set)

* For each capacity declaration, calculates each member's share of total mutual-recognition within that set
* Alice's-Share = (Alice's mutual-recognition with others in set) / (Total mutual-recognition in set)
* All members can see these shares for any declared capacity
* This allows distributed coordination - everyone can see allocation priorities from different lenses

5\) **Providers allocate** (only providers with actual resources)

* When a provider (with actual resources) declares a capacity, they allocate to needs
* Protocol applies compliance filters (KYC, sanctions, jurisdiction limits)
* Provider allocates based on recognition shares, actual needs, and available capacity
* Example: Collective X has $5000, allocates $1000 to Alice, $500 to Bob, $200 to Charlie

**Filter Equation:**
```
Filter(Member, Capacity) = Maximum amount that can be allocated to Member from this Capacity

Values:
- $0 = Cannot allocate (sanctions, KYC failed, etc.)
- $X = Can allocate up to $X (jurisdiction limits, risk caps, etc.)
- Unlimited = No restriction

Actual-Allocation = min(
  Recognition-Share × Total-Capacity,
  Filter(Member, Capacity),
  Member-Need
)

Members who hit their filter limit receive up to that limit.
Unallocated capacity redistributes to other members by recognition shares.
```

**Union of Filters:**
```
When an external provider uses a proxy as executor, 
BOTH filters apply:

Effective-Filter(Member) = min(
  Provider-Filter(Member),
  Proxy-Filter(Member)
)

Most restrictive filter wins. Both must allow allocation.

Example:
- Foundation X filter for Alice: $50K max (their risk limit)
- Org Y filter for Alice: $30K max (jurisdiction limit)
- Effective filter: min($50K, $30K) = $30K

- Foundation X filter for Charlie: $40K max (they approve Charlie)
- Org Y filter for Charlie: $0 (on sanctions list)
- Effective filter: min($40K, $0) = $0 → Cannot allocate

Key: External provider's approval is necessary but not sufficient.
Proxy's compliance filters always apply.
```

**Key: Distributed capacity mapping, but only providers allocate**
- All members can declare capacities (their perspective of what exists)
- Each declaration shows collective-recognition-shares within that lens
- But only providers with actual resources can allocate
- Non-provider declarations enable coordination; provider declarations enable allocation

## Advantages

* **No centralized definition of what is a meaningful contribution** - Each member determines recognition based on their own self-actualization
* **Distributed determination of value** - Recognition emerges from genuine mutual enabling relationships
* **Distributed capacity mapping** - Members can declare their perspective of what capacities exist
* **Multiple lenses** - Each capacity declaration shows collective-recognition-shares from a different lens
* **Provider reality check** - Only providers with actual resources can allocate (non-provider declarations are just coordination)
* **No phantom allocation** - Members can map anything, but only real providers allocate
* **Coordination without consensus** - Everyone sees allocation priorities from different capacity lenses
* **Direct allocation** - When providers allocate, it goes directly to needs based on recognition + capacity + filters