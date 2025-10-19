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

* Any member can declare: "From my perspective, Foundation X can allocate $5000/month to {Alice, Bob, Charlie, Dave}"
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
* Example: Foundation X has $5000, allocates $1000 to Alice, $500 to Bob, $200 to Charlie

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

## Implementation Notes

* **For Playnet Verein:** When the Verein holds unrestricted funds, the protocol automatically declares capacity for all members and allocates computationally based on collective-recognition-shares + compliance filters
* **For external providers:** Providers can deposit funds to Verein and instruct allocations, or transfer directly to recipients
