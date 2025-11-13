# Protocol Implementation Gap Analysis

## Critical Missing Features

### ❌ 1. Dampening/Oscillation Detection
**Protocol Requirement:**
- Providers track oscillation history per recipient
- Damping factors: {0.5, 0.8, 1.0}
- Pattern detection: need goes up/down repeatedly
- Active Need = Declared Need × Damping Factor

**Current Status:**
- ❌ NOT IMPLEMENTED in NetworkedZipper.hs
- ❌ NOT IMPLEMENTED in FreeAssociation.hs
- No oscillation history tracking
- No damping calculation

### ❌ 2. Two-Phase Process (Provider → Recipient)
**Protocol Requirement:**
```
PHASE 1 (Providers):
  - Each provider independently calculates slot_allocations
  - Publishes: [{recipient: ID, quantity: N}, ...]
  - Multiple providers can allocate to same recipient

PHASE 2 (Recipients):
  - Recipient receives allocations from ALL providers
  - THEN applies update law: Remaining_Need = max(0, Declared - TotalReceived)
  - Publishes updated need
  - Next iteration begins
```

**Current Status:**
- ⚠️ NetworkedZipper tries to do both phases at once (line 304-336)
- ⚠️ FreeAssociation.hs updates needs immediately per provider (line 278-282)
- ❌ No slot_allocations publishing step
- ❌ Recipients don't aggregate multiple allocations before updating

### ❌ 3. Active Need vs Declared Need
**Protocol Requirement:**
```
Step 1: Apply dampening
  activeNeed = declaredNeed × dampingFactor

Step 5: Cap at active need (NOT declared!)
  allocation = min(rawAllocation, activeNeed)
```

**Current Status:**
- ❌ No distinction between active and declared need
- NetworkedZipper (line 356): `min rawAllocation myNeed` - uses declared, not active
- FreeAssociation.hs (line 221-224): Similar issue

### ❌ 4. Five-Step Provider Algorithm
**Protocol Requirement:**
```
Step 0: Check oscillation history → Determine damping factor
Step 1: Apply dampening (activeNeed = declaredNeed × damping)
Step 2: Filter compatible recipients
Step 3: Calculate MR shares (Total MR across recipients)
Step 4: Proportional allocation (capacity × share)
Step 5: Cap at active need
```

**Current Status:**
- ⚠️ Partial implementation (steps 3-5 roughly done)
- ❌ Missing Step 0 (oscillation detection)
- ❌ Missing Step 1 (dampening)
- ❌ Missing Step 2 (compatibility filters - time, location, type)

### ⚠️ 5. Over-Allocation Handling
**Protocol Insight:**
> "Over-allocation is EXPECTED and normal"
> "Recipients can receive from multiple providers in one round"
> "Total can exceed need → triggers update law"

**Current Status:**
- ✅ Update law is correct: `max(0, declared - totalReceived)`
- ⚠️ But happens too early (per-provider instead of after aggregating all)

### ❌ 6. Slot Allocations Structure
**Protocol Requirement:**
```
provider.slot_allocations = [
  {recipient: EntityId, quantity: Capacity, resourceType: String},
  ...
]
```

**Current Status:**
- ❌ Not modeled in data structures
- No way to publish/query slot allocations separately

## What's Correct

### ✅ 1. Independent Computation
- Both implementations support each entity calculating independently
- NetworkedZipper properly models distributed architecture

### ✅ 2. Mutual Recognition Calculation
```haskell
mutualRec = min (getPortion recToProvider) (getPortion recFromProvider)
```
- ✅ Correct formula

### ✅ 3. Update Law
```haskell
remainingNeed = max 0 (currentNeed - received)
```
- ✅ Correct formula
- ⚠️ Applied at wrong time in the process

### ✅ 4. Async Network Architecture
- NetworkedZipper properly models remote fetching
- Zipper navigation works correctly
- Breadcrumbs preserve network paths

## Required Changes

### Priority 1: Core Algorithm
1. Add oscillation history tracking to PlayerState/ContextState
2. Implement 5-step provider algorithm with dampening
3. Separate provider phase from recipient phase
4. Add slot_allocations to data structures

### Priority 2: Data Structures
```haskell
data SlotAllocation = SlotAllocation
  { recipientId :: EntityId
  , quantity :: Capacity
  , resourceType :: String
  }

data OscillationHistory = OscillationHistory
  { needHistory :: [(Timestamp, Capacity)]
  , dampingFactor :: Double  -- 0.5, 0.8, or 1.0
  }
```

### Priority 3: Two-Phase Execution
```haskell
-- Phase 1: Provider publishes allocations
providerPhase :: PlayerState -> [EntityId] -> ZipperM [SlotAllocation]

-- Phase 2: Recipient aggregates and updates
recipientPhase :: PlayerState -> [SlotAllocation] -> ZipperM PlayerState
```

## Accuracy Score

| Feature | NetworkedZipper | FreeAssociation | Protocol |
|---------|----------------|-----------------|----------|
| Mutual Recognition | ✅ 100% | ✅ 100% | ✅ |
| Update Law | ⚠️ 70% | ⚠️ 70% | ✅ |
| Proportional Share | ⚠️ 60% | ⚠️ 70% | ✅ |
| Dampening | ❌ 0% | ❌ 0% | ✅ |
| Two-Phase Process | ❌ 20% | ❌ 20% | ✅ |
| Oscillation Detection | ❌ 0% | ❌ 0% | ✅ |
| Slot Allocations | ❌ 0% | ❌ 0% | ✅ |
| Async/Network | ✅ 90% | ❌ 0% | ✅ |
| **OVERALL** | **40%** | **35%** | **100%** |

## Recommendation

Create a new implementation that:
1. Keeps NetworkedZipper's async/distributed architecture
2. Implements the full 5-step provider algorithm
3. Properly separates provider and recipient phases
4. Adds oscillation detection and dampening
5. Models slot_allocations explicitly

