Oh! This is a fundamental ontological shift - **there is no depletion, only apportionment at every level.**

## Apportionment All The Way Down

### Current Model (Depletion Metaphor):

```typescript
CoreCapacity: 40 hours time
  → Teaching actuates: DEPLETES 10 hours → 30 remaining
  → Writing actuates: DEPLETES 8 hours → 22 remaining
  → Rest actuates: DEPLETES 15 hours → 7 remaining
```

This treats core capacity as a **consumable stock** that shrinks.

### Reframed Model (Apportionment Metaphor):

```typescript
CoreCapacity: 40 hours time (fixed quantity for the period)

Apportionment of these 40 hours:
  → 10 hours apportioned to Teaching capacity
  → 8 hours apportioned to Writing capacity  
  → 15 hours apportioned to Rest/Recovery capacity
  → 7 hours apportioned to Unallocated/Buffer
  
Total: 40 hours (fully apportioned, nothing depleted)
```

The 40 hours don't shrink - they get **divided among different uses**. Each derived capacity is a **proportion of the base capacity**, not a depletion of it.

## Multi-Level Apportionment

```typescript
// Level 0: Core Capacity (the base quantity)
CoreCapacity {
  time: 40 hours  // Fixed for this period (e.g., one week)
}

// Level 1: Apportionment to Derived Capacities
DerivedCapacities = apportion(CoreCapacity.time, [
  { target: "teaching", weight: 0.25 },     // 10 hours
  { target: "writing", weight: 0.20 },      // 8 hours
  { target: "rest", weight: 0.375 },        // 15 hours
  { target: "buffer", weight: 0.175 }       // 7 hours
])

// Level 2: Apportionment to Recipients (network coordination)
TeachingAllocation = apportion(DerivedCapacities.teaching, [
  { recipient: Alice, MR: 0.6 },   // 6 hours
  { recipient: Bob, MR: 0.4 }      // 4 hours
])

WritingAllocation = apportion(DerivedCapacities.writing, [
  { recipient: Journal, MR: 0.5 }, // 4 hours
  { recipient: Blog, MR: 0.5 }     // 4 hours
])
```

**Key insight:** Each level is a **division problem**, not a depletion event.

## The Apportionment Formula (Unified)

At every level, we have the same structure:

```
Apportionment Formula:
  Total Available = Q
  Claimants = {C₁, C₂, ..., Cₙ}
  Weights = {w₁, w₂, ..., wₙ}
  
  Share(Cᵢ) = Q × (wᵢ / Σwⱼ)
  
Where weights represent:
  - Level 1 (core → derived): Your priority/allocation of time to different activities
  - Level 2 (derived → recipients): Mutual recognition strengths
```

## Data Structure

```typescript
interface BaseCapacity {
  id: string
  type: string  // "time", "energy", "knowledge"
  totalQuantity: number  // Fixed for this period
  
  // How is this base capacity apportioned?
  apportionments: Apportionment[]
}

interface Apportionment {
  target: string  // DerivedCapacity ID or "self" or "buffer"
  weight: number  // Your priority weight for this use
  
  // Computed
  allocatedQuantity: number  // = baseCapacity × (weight / totalWeights)
}

interface DerivedCapacity {
  id: string
  type: string  // "teaching", "consulting", "caregiving"
  
  // Inputs: What base capacities does this draw from?
  inputs: {
    baseCapacityId: string
    coefficient: number  // hours of base per unit of derived
  }[]
  
  // Available quantity (computed from inputs)
  availableQuantity: number
  
  // How is this derived capacity apportioned to recipients?
  recipientApportionments: RecipientApportionment[]
}

interface RecipientApportionment {
  recipientId: string
  mutualRecognition: number  // The weight
  
  // Computed
  allocatedQuantity: number  // = derivedCapacity × (MR / totalMR)
}
```

## The Computation Flow

```typescript
// Step 1: Apportion base capacity to derived capacities
function apportionBaseToDeriveds(base: BaseCapacity): void {
  const totalWeight = sum(base.apportionments.map(a => a.weight))
  
  base.apportionments.forEach(apportionment => {
    apportionment.allocatedQuantity = 
      base.totalQuantity × (apportionment.weight / totalWeight)
  })
}

// Step 2: Compute derived capacity availability
function computeDerivedAvailability(derived: DerivedCapacity): void {
  // Derived capacity = minimum of all input proportions
  derived.availableQuantity = min(
    derived.inputs.map(input => {
      const base = getBaseCapacity(input.baseCapacityId)
      const apportionedToMe = base.apportionments
        .find(a => a.target === derived.id)
        .allocatedQuantity
      
      return apportionedToMe / input.coefficient
    })
  )
}

// Step 3: Apportion derived capacity to recipients
function apportionDerivedToRecipients(derived: DerivedCapacity): void {
  const totalMR = sum(derived.recipientApportionments.map(a => a.mutualRecognition))
  
  derived.recipientApportionments.forEach(apportionment => {
    apportionment.allocatedQuantity = 
      derived.availableQuantity × (apportionment.mutualRecognition / totalMR)
  })
}
```

## Key Difference from Depletion

**Depletion Model:**
- Sequential: First teaching happens (depletes), then writing (depletes more), etc.
- Order-dependent: What happens first affects what's left
- Negative feedback: Each use reduces remaining capacity
- Temporal: Happens over time as actuations occur

**Apportionment Model:**
- Simultaneous: All apportionments decided together for the period
- Order-independent: Division is computed comprehensively
- Proportional: Each use gets its share of the whole
- Structural: The division structure defines the period

## Example: 40 Hours Over One Week

### Apportionment Model:

```typescript
MyCapacityThisWeek {
  baseCapacities: {
    time: {
      totalQuantity: 40 hours,
      apportionments: [
        { target: "teaching", weight: 10 },
        { target: "writing", weight: 8 },
        { target: "rest", weight: 15 },
        { target: "buffer", weight: 7 }
      ]
    }
  },
  
  derivedCapacities: {
    teaching: {
      inputs: [{ baseCapacityId: "time", coefficient: 1 }],
      availableQuantity: 10 hours,
      recipientApportionments: [
        { recipientId: "Alice", MR: 0.6 },  // Gets 6 hours
        { recipientId: "Bob", MR: 0.4 }     // Gets 4 hours
      ]
    },
    
    writing: {
      inputs: [{ baseCapacityId: "time", coefficient: 1 }],
      availableQuantity: 8 hours,
      recipientApportionments: [
        { recipientId: "Journal", MR: 0.5 },  // Gets 4 hours
        { recipientId: "Blog", MR: 0.5 }      // Gets 4 hours
      ]
    }
  }
}

// The 40 hours are comprehensively apportioned
// Nothing is "consumed" or "depleted"
// Just divided among uses and recipients
```

## What About Need Emergence?

If there's no depletion, how do needs emerge?

**Reframe:** Needs emerge from **under-apportionment**, not depletion:

```typescript
MyNeeds {
  rest: {
    desired: 20 hours/week,     // What I want
    apportioned: 15 hours/week, // What I allocated
    deficit: 5 hours/week       // Under-apportioned!
  },
  
  time_for_self: {
    desired: 10 hours/week,
    apportioned: 7 hours/week,
    deficit: 3 hours/week
  }
}

// My needs = gap between desired and apportioned
// Not caused by "depletion" but by insufficient apportionment
```

Or even more radically: **needs ARE apportionment claims**:

```typescript
// When I declare a need, I'm saying:
// "I want to be apportioned X from the network's capacity"

MyNeed {
  type: "rest/recovery support",
  desiredApportionment: 5 hours/week,
  currentlyApportioned: 0,  // Not yet receiving from network
  deficit: 5 hours
}

// The network then apportions its capacity to satisfy my need-claim
```

## Complete Picture

```
Period: One Week

Base Capacities (40 hrs time) 
  ↓ [Apportionment Level 1: Self-allocation]
  ├─→ Teaching Capacity (10 hrs)
  │     ↓ [Apportionment Level 2: Network coordination]
  │     ├─→ Alice (6 hrs)
  │     └─→ Bob (4 hrs)
  │
  ├─→ Writing Capacity (8 hrs)
  │     ↓ [Apportionment Level 2]
  │     ├─→ Journal (4 hrs)
  │     └─→ Blog (4 hrs)
  │
  ├─→ Rest Capacity (15 hrs)
  │     ↓ [Self-apportioned, but deficit exists]
  │     └─→ Self (15 hrs, but need 20 hrs)
  │
  └─→ Buffer (7 hrs)
```

Everything is apportionment - the only question is **who/what sets the weights**:
- **Level 1:** You set the weights (how to divide your base capacity)
- **Level 2:** Mutual recognition sets the weights (how network divides your derived capacity)

Does this capture it? **Depletion is just apportionment viewed sequentially instead of simultaneously**?