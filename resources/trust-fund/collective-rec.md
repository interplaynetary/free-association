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

Needs are slot-based, mirroring capacity structure:

* Alice: "I need 20 hours of web development" (Need slots: 10 hours Mon-Fri 9am-5pm Berlin, 10 hours flexible)
* Bob: "I need $500 for materials" (Need slot: $500 by March 15th, Berlin)
* Charlie: "I need 5 acres of farmland" (Need slot: 5 acres, year-round, Oregon)

Each need has:
- Multiple need slots (time/location/quantity specific)
- When the need exists (time patterns, recurrence)
- Where the need is relevant (location constraints)
- Fulfillment tracking (open/partially-fulfilled/fulfilled)

2\) **Members recognize contributions** (via free-association interface)

* Each member recognizes others' contributions towards their own self-actualization
* When members mutually recognize each other, they establish mutual-recognition percentages

3\) **Members declare capacities** (their perspective of what exists)

Capacities are slot-based, mirroring need structure:

* "Collective X has $5000 available" (Capacity slots: $2000 by March 1st, $3000 by April 1st, for {Alice, Bob, Charlie, Dave})
* "I have 30 hours/week available" (Availability slots: 15 hours Mon-Wed 9am-5pm Berlin, 15 hours Thu-Fri flexible)
* "Community farm has 10 acres available" (Availability slot: 10 acres, year-round access, Oregon)

Each capacity has:
- Multiple availability slots (time/location/quantity specific)
- When capacity is available (time patterns, recurrence)
- Where capacity can be used (location constraints)
- Set of people who can receive from this capacity

**Key: Only the actual provider's capacity matters for real allocation**
- Non-provider declarations are just perspectives; provider's declaration is what allocates

4\) **Protocol computes collective-recognition-shares** (within each declared capacity set)

* For each capacity declaration, calculates each member's share of total mutual-recognition within that set
* Alice's-Share = (Alice's mutual-recognition with others in set) / (Total mutual-recognition in set)
* All members can see these shares for any declared capacity
* This allows distributed coordination - everyone can see allocation priorities from different lenses

5\) **Providers allocate** (only providers with actual resources)

* When a provider (with actual resources) declares a capacity, they allocate to needs
* **Allocation is PROPORTIONAL to recognition shares** (not greedy/first-come-first-served)
* Protocol applies compliance filters (KYC, sanctions, jurisdiction limits)
* Each member gets allocated up to: `Recognition-Share × Total-Capacity` (limited by filters and needs)

**How proportional allocation works:**
```
Provider has 100 hours available
Recognition shares: Alice 40%, Bob 30%, Charlie 20%, Others 10%

Target allocations:
- Alice target: 40% × 100h = 40h
- Bob target: 30% × 100h = 30h
- Charlie target: 20% × 100h = 20h
- Others target: 10% × 100h = 10h

Each recipient allocated up to their target (respecting slot compatibility, needs, and filters)
Multi-pass redistribution ensures unused capacity goes proportionally to those who can use more

Result: Fair distribution respecting collective priorities, not winner-takes-all
```

* Example: Collective X has $5000, Alice gets $2000 (40% share), Bob gets $1500 (30%), Charlie gets $1000 (20%), Others get $500 (10%)

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

## Need-Capacity Mirror Structure

**Needs and Capacities are mirror images:**

```
Need = "What I require"          ↔  Capacity = "What I can provide"
--------------------------------------------------------------------
NeedSlot                         ↔  AvailabilitySlot
- quantity                       ↔  - quantity
- time patterns (when needed)    ↔  - time patterns (when available)
- location (where needed)        ↔  - location (where provided)
- priority/urgency               ↔  - priority
- mutual_agreement_required      ↔  - mutual_agreement_required

BaseNeed                         ↔  BaseCapacity
- name, emoji, unit              ↔  - name, emoji, unit
- description                    ↔  - description
- need_slots[]                   ↔  - availability_slots[]
- declarer_id                    ↔  - owner_id
- fulfilled_amount, status       ↔  - (tracked via allocations)
```

**Why this matters:**
- Enables sophisticated need-capacity matching by time/location/quantity
- Both express temporal and spatial constraints
- Can automatically match "I need X on Monday in Berlin" with "I have Y available Tuesday in Berlin"
- Supports complex scheduling and coordination patterns
- Makes the symmetry of the system explicit

## Advantages

* **No centralized definition of what is a meaningful contribution** - Each member determines recognition based on their own self-actualization
* **Distributed determination of value** - Recognition emerges from genuine mutual enabling relationships
* **Distributed capacity mapping** - Members can declare their perspective of what capacities exist
* **Multiple lenses** - Each capacity declaration shows collective-recognition-shares from a different lens
* **Provider reality check** - Only providers with actual resources can allocate (non-provider declarations are just coordination)
* **No phantom allocation** - Members can map anything, but only real providers allocate
* **Coordination without consensus** - Everyone sees allocation priorities from different capacity lenses
* **Direct allocation** - When providers allocate, it goes directly to needs based on recognition + capacity + filters
* **Proportional & fair** - Allocation is proportional to recognition shares, not greedy/winner-takes-all (everyone gets their share)
* **Slot-based matching** - Needs mirror capacities with time/location/quantity constraints for sophisticated coordination
* **Temporal & spatial awareness** - System understands when and where resources are needed vs. available
* **Automatic scheduling** - Can match needs to capacities by time patterns and location constraints
* **Flexible fulfillment** - Multiple need slots can be fulfilled by different providers at different times/locations
* **Multi-pass optimization** - Unused capacity redistributes proportionally to those who can use more (maximizes utilization while maintaining fairness)