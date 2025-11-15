# Free-Association User Journey & Component Trace

## Overview

This document traces the complete user journey through the Free-Association interface, mapping each step to the technical components required for implementation.

---

## 🎯 PHASE 1: AUTHENTICATION & IDENTITY

### User Story
> "I want to join the Free-Association network and establish my identity"

### Journey Steps

#### 1.1 Landing & Authentication
**User Action:** User visits the application and needs to authenticate

**UI Components:**
- `Header.svelte` - Top navigation with login/signup controls
- Authentication modal (triggered from Header)

**State Management:**
- `src/lib/state/auth.svelte.ts` - Unified authentication interface
- `src/lib/state/holster.svelte.ts` - Holster-based P2P authentication
- `src/lib/state/gun.svelte.ts` - GUN-based P2P authentication (alternative)

**Backend Functions:**
```typescript
// Authentication
login(alias: string, password: string): Promise<void>
signup(alias: string, password: string): Promise<void>

// State
holsterUser.recall() - Check existing auth
holsterUser.auth() - Authenticate user
holsterUser.create() - Create new account
```

**Data Flow:**
1. User enters alias + password
2. System authenticates via Holster (P2P)
3. Generates public/private keypair (stored locally)
4. Sets `holsterUserAlias` and `holsterUserPub` stores
5. Initializes data streams: `initializeHolsterDataStreams()`

**What's Stored:**
- Public Key (`holsterUserPub`) - User's network identity
- Private Key (encrypted locally) - Never leaves device
- Alias (`holsterUserAlias`) - Human-readable name

---

## 🌲 PHASE 2: RECOGNITION TREE MANAGEMENT

### User Story
> "I want to declare who contributes to my well-being and how much"

### Journey Steps

#### 2.1 Create/View Recognition Tree
**User Action:** Navigate to main view, see their contribution tree

**UI Components:**
- `+page.svelte` - Main layout with view switcher
- `Parent.svelte` - Recognition tree treemap visualization (D3-based)
- `Child.svelte` - Individual tree node component
- `ToolBar.svelte` - Bottom toolbar with actions

**State Management:**
- `src/lib/state/core.svelte.ts`:
  - `userTree` - User's recognition tree structure
  - `isLoadingTree` - Loading state
  - `userSogf` - Calculated shares of general fulfillment

**Backend Functions:**
```typescript
// From protocol.ts
createRootNode(id, name, manual?) - Initialize tree
findNodeById(tree, nodeId) - Navigate tree
getPathToNode(tree, nodeId) - Get navigation path
getDescendants(node) - Get all children recursively

// From calculations.svelte.ts
recalculateFromTree() - Compute recognition shares
```

**Data Flow:**
1. Load tree from P2P network: `gun.get('user').get('tree')`
2. Render treemap using D3.js with `d3.treemap()`, `d3.hierarchy()`
3. Calculate layout: `treemap.size([width, height])(root)`
4. Render nodes with colors based on fulfillment

**What's Displayed:**
- Hierarchical tree structure (treemap visualization)
- Node names and sizes (proportional to points)
- Node colors (based on fulfillment: 0% = red → 100% = green)
- Contributors on each node

#### 2.2 Add Contributors to Nodes
**User Action:** Click node → Add contributor → Select person

**UI Components:**
- `Child.svelte` - Node with click handler
- `DropDown.svelte` - Contributor selection dropdown
- `ui-providers.svelte.ts` - `createChildContributorsDataProvider()` for contact list

**State Management:**
- `src/lib/state/users.svelte.ts`:
  - `contacts` - User's contact list
  - `contactsPublicKeys` - Resolved public keys

**Backend Functions:**
```typescript
// From protocol.ts
addContributors(node, contributorIds, antiContributorIds)
updateNodeById(tree, nodeId, updater)

// From users.svelte.ts
createContact(name, pubkey?)
resolveToPublicKey(contactId) - Convert contact_id → pubkey
```

**Data Flow:**
1. User clicks "Add Contributor" on node
2. Opens dropdown with contacts list
3. User selects contact or creates new one
4. System adds `contributor_ids` to node
5. Recalculates recognition: `sharesOfGeneralFulfillmentMap()`
6. Updates tree in P2P network

**What's Stored:**
```typescript
{
  id: "node_123",
  name: "Healthcare",
  contributor_ids: ["contact_abc", "pubkey_xyz"],
  points: 70
}
```

#### 2.3 Add Anti-Contributors
**User Action:** Mark someone who hampered work quality

**UI Components:**
- Same as contributors, but with `contributorMode = 'anti-contributor'`

**Backend Functions:**
```typescript
addContributors(node, contributorIds, antiContributorIds)
```

**Recognition Calculation:**
From free-association.md:
- Anti-contributors only effective when `manual_fulfillment < 100%`
- Creates "desire" pool for negative recognition
- Anti-recognition = bounded by desire pool

**Data Flow:**
1. Add anti-contributor to node
2. System calculates desire: `desire(node) = 1.0 - fulfilled(node)`
3. Splits recognition into positive/negative pools
4. Anti-contributors share negative pool

#### 2.4 Adjust Node Structure
**User Actions:**
- Add child node
- Reorder nodes (drag & drop)
- Delete nodes
- Adjust points

**UI Components:**
- `ToolBar.svelte` - "Add Node" button
- `DraggedNode.svelte` - Drag preview
- `globalState.svelte.ts` - Drag state management

**Backend Functions:**
```typescript
// Adding nodes
addChild(parentNode, id, name, points, contributorIds, antiContributorIds)
calculateNodePoints(parentNode) - Calculate appropriate points

// Reordering
reorderNode(tree, nodeId, newParentId)
extractSubtree(tree, nodeId)
insertSubtree(tree, targetNodeId, subtree)

// Deletion
deleteSubtree(node)

// Updates
updatePoints(node, points)
updateName(node, name)
updateManualFulfillment(node, value)
```

**Data Flow:**
1. User performs action (add/move/delete)
2. System mutates tree structure
3. Recalculates all dependent values:
   - Weights: `weight(node, tree)`
   - Fulfillment: `fulfilled(node, tree)`
   - Recognition shares: `sharesOfGeneralFulfillmentMap()`
4. Updates P2P network
5. Triggers reactive UI updates

---

## 📊 PHASE 3: VIEWING RECOGNITION

### User Story
> "I want to see who I recognize and how much"

### Journey Steps

#### 3.1 View Recognition Bars
**User Action:** View bars at bottom of tree view

**UI Components:**
- `Bar.svelte` - Horizontal bar chart with segments
- `+page.svelte` - Embeds Bar components

**State Management:**
- `userSogf` store - Shares Of General Fulfillment map

**Display:**
```
Your Recognition (YR):
┌────────────────────────────────────────────┐
│ Alice 30%  │ Bob 30%   │ Carol 40%         │
└────────────────────────────────────────────┘
```

**Backend Functions:**
```typescript
// From protocol.ts
sharesOfGeneralFulfillmentMap(
  rootNode, 
  nodesMap, 
  specificContributors?, 
  resolveToPublicKey?
): ShareMap

// Returns: { "pubkey_alice": 0.30, "pubkey_bob": 0.30, "pubkey_carol": 0.40 }
```

**Calculation:**
From free-association.md, the tree:
```
Kitchen's Contributors (100%)
├─ Food Prep (60%)
│  ├─ Alice (50%) → 60% × 50% = 30%
│  └─ Bob (50%)   → 60% × 50% = 30%
└─ Equipment (40%)
   └─ Carol (100%) → 40% × 100% = 40%
```

Results in global recognition: `{ Alice: 0.30, Bob: 0.30, Carol: 0.40 }`

---

## 🎁 PHASE 4: CAPACITY DECLARATION

### User Story
> "I want to declare what I can provide to others"

### Journey Steps

#### 4.1 Navigate to Inventory
**User Action:** Click "Inventory" view in toolbar

**UI Components:**
- `ToolBar.svelte` - View switcher
- `+page.svelte` - Routes to inventory view
- `Capacities.svelte` - List of capacity declarations

**State Management:**
- `src/lib/state/core.svelte.ts`:
  - `userCapacities` - Array of ProviderCapacity objects
  - `networkCapacities` - Other people's capacities

#### 4.2 Create Capacity
**User Action:** Click "New Capacity" button

**UI Components:**
- `Capacity.svelte` - Individual capacity card
- Form inputs: emoji, name, unit, description

**Backend Functions:**
```typescript
// Creates a new capacity
const newCapacity: ProviderCapacity = {
  id: generateId(),
  name: "Tutoring",
  emoji: "📚",
  unit: "hours",
  description: "Math and physics tutoring",
  availability_slots: []
}
```

**Data Flow:**
1. User clicks "New Capacity"
2. Creates empty capacity object
3. Adds to `userCapacities` store
4. Syncs to P2P network: `gun.get('user').get('capacities')`

#### 4.3 Add Availability Slots
**User Action:** Click "Add Slot" on capacity

**UI Components:**
- `Slot.svelte` - Individual slot editor
- Time/Date pickers
- Location inputs
- `CountrySelector.svelte`, `TimezoneSelector.svelte`

**Slot Configuration:**
```typescript
interface AvailabilitySlot {
  id: string;
  quantity: number;
  need_type_id: string; // "food", "healthcare", etc.
  
  // Time
  start_date: string; // "2024-03-04"
  end_date?: string;
  recurrence?: "daily" | "weekly" | "monthly" | "yearly";
  availability_window?: AvailabilityWindow; // Hierarchical time structure
  time_zone?: string; // "America/New_York"
  
  // Location
  location_type?: string; // "Specific", "Online"
  city?: string;
  country?: string;
  latitude?: number;
  longitude?: number;
  online_link?: string;
  
  // Constraints
  advance_notice_hours?: number;
  booking_window_hours?: number;
  max_natural_div?: number; // Can't divide resource more than this
  max_percentage_div?: number; // Can't allocate less than this %
  
  // Filters
  filter_rule?: FilterRule; // Who can receive from this
}
```

**Example Slot Creation:**
```typescript
// Weekly tutoring
{
  id: "slot_123",
  quantity: 10,
  need_type_id: "education",
  recurrence: "weekly",
  availability_window: {
    day_schedules: [{
      days: ["monday", "friday"],
      time_ranges: [{ start_time: "14:00", end_time: "18:00" }]
    }]
  },
  time_zone: "America/New_York",
  location_type: "Online",
  online_link: "https://meet.jit.si/tutoring",
  advance_notice_hours: 24
}
```

**What This Means:**
"I can tutor for 10 hours every Monday and Friday, 2-6pm EST, online, with 24h notice"

#### 4.4 Configure Filters
**User Action:** Set who can receive from this capacity

**UI Components:**
- Filter dropdown in `Capacity.svelte`
- `Rules` from `$lib/filters`

**Filter Types:**
```typescript
type FilterRule = 
  | { type: "trust"; min_mutual_recognition?: number; only_mutual?: boolean }
  | { type: "location"; allowed_cities?: string[]; max_distance_km?: number }
  | { type: "attribute"; required?: string[]; forbidden?: string[] }
  | { type: "certification"; required?: string[]; min_level?: number }
  | { type: "allow_all" }
  | { type: "deny_all" }
```

**Example:**
```typescript
// Only allocate to people I mutually recognize
{
  type: "trust",
  only_mutual: true,
  min_mutual_recognition: 0.05
}
```

**Data Flow:**
1. User configures filter
2. Stored in `slot.filter_rule`
3. During allocation, system checks: `passesSlotFilters(needSlot, availSlot, providerCtx, recipientCtx)`

#### 4.5 Link to Recognition Tree
**User Action:** Associate capacity with subtree filters

**UI Components:**
- Subtree dropdown in `Capacity.svelte`
- `createSubtreesDataProvider()` - Lists all subtrees

**Purpose:**
Filter recipients based on recognition tree structure:
- "Only allocate to contributors in my 'Healthcare' subtree"
- "Prioritize people in my 'Food Network' branch"

**Data Flow:**
1. User selects subtree
2. System adds to `selectedSubtrees` array
3. During allocation, checks: `isContributorInSubtree(recipientId, subtreeId)`

---

## 🙏 PHASE 5: NEED DECLARATION

### User Story
> "I want to declare what I need from others"

### Journey Steps

#### 5.1 View Network Capacities
**User Action:** Browse available capacities in map view

**UI Components:**
- `Map.svelte` - Geographic visualization
- `MapSidePanel.svelte` - Capacity details panel
- Leaflet.js for map rendering

**State Management:**
- `networkCapacities` - All capacities from network
- `networkCapacityIndex` - Spatial/temporal index for fast lookup

**Index Structure:**
```typescript
interface SpaceTimeIndex {
  byType: Map<string, Set<string>>; // need_type_id → Set<pubKey>
  byLocation: Map<string, Set<string>>; // location_key → Set<pubKey>
  byTime: Map<string, Set<string>>; // time_bucket → Set<pubKey>
  byTypeAndLocation: Map<string, Set<string>>;
  byTypeAndTime: Map<string, Set<string>>;
  byAll: Map<string, Set<string>>;
}
```

**Performance:**
- O(1) lookup instead of O(N) scan
- Incremental updates: O(M) per participant change
- Built reactively via `networkCapacityIndex` store

#### 5.2 Express Desire (Declare Need)
**User Action:** Click capacity → "Express Desire"

**UI Components:**
- `Share.svelte` - Network capacities with desire inputs
- `SlotCompositionItem.svelte` - Desire input for specific slot

**Desire Input:**
```typescript
{
  capacityId: "kitchen_capacity",
  slotId: "monday_meals",
  desiredQuantity: 40 // I need 40 meals
}
```

**Data Flow:**
1. User enters desired quantity
2. System creates composition record:
   ```typescript
   userDesiredSlotComposeFrom.set({
     [slotId]: {
       targetCapacityId: "kitchen_capacity",
       targetSlotId: "monday_meals",
       desiredQuantity: 40,
       compositionUnit: "meals"
     }
   })
   ```
3. Syncs to P2P network
4. Triggers allocation computation

#### 5.3 Create Own Need Slots (Alternative)
**User Action:** Declare needs without browsing network first

**Note:** Current UI primarily uses "desire" composition model, but schemas support standalone need slots

**Need Slot Structure:**
```typescript
interface NeedSlot {
  id: string;
  quantity: number;
  need_type_id: string;
  
  // Same time/location/filter structure as AvailabilitySlot
  start_date?: string;
  recurrence?: "daily" | "weekly" | "monthly" | "yearly";
  availability_window?: AvailabilityWindow;
  location_type?: string;
  city?: string;
  // ... etc
  
  filter_rule?: FilterRule; // Who can PROVIDE to this need
}
```

**Backend Functions:**
```typescript
// From schemas.ts
parseCommitment(data): Commitment

// Commitment contains:
{
  need_slots: NeedSlot[],
  capacity_slots: AvailabilitySlot[],
  global_recognition_weights: { [pubKey]: number }
}
```

---

## ⚡ PHASE 6: ALLOCATION COMPUTATION

### User Story
> "The system automatically allocates resources based on needs and mutual recognition"

### Journey Steps

#### 6.1 Trigger Allocation
**Triggers:**
- User updates desires
- User updates capacity
- User updates recognition tree
- Network state changes

**Backend Functions:**
```typescript
// From free-algorithm.svelte.ts

// 1. Get system state
getCurrentSystemState(): SystemStateSnapshot

// 2. For each availability slot, find compatible recipients
getCandidateRecipients(
  capacitySlot: AvailabilitySlot,
  needsIndex: SpaceTimeIndex
): Set<string>

// 3. Check compatibility
slotsCompatible(needSlot, availabilitySlot): boolean
// Checks: type match, time overlap, location compatibility

// 4. Check filters
passesSlotFilters(needSlot, availSlot, providerCtx, recipientCtx): boolean

// 5. Run two-tier allocation
runEfficientAllocation(
  myPub: string,
  myCommitment: Commitment,
  networkCommitments: Record<string, Commitment>,
  ...
): TwoTierAllocationState
```

**Algorithm Flow:**
```typescript
// For each of MY availability slots:
for (const mySlot of myCommitment.capacity_slots) {
  // Find compatible recipients
  const candidates = getCandidateRecipients(mySlot, needsIndex);
  
  // Split into tiers
  const tier1: RecipientData[] = []; // Mutual recognition
  const tier2: RecipientData[] = []; // Non-mutual recognition
  
  for (const recipientPub of candidates) {
    const recipientCommitment = networkCommitments[recipientPub];
    
    // Find compatible need slots
    for (const needSlot of recipientCommitment.need_slots) {
      if (!slotsCompatible(needSlot, mySlot)) continue;
      if (!passesSlotFilters(needSlot, mySlot, providerCtx, recipientCtx)) continue;
      
      // Calculate mutual recognition
      const mr = computeMutualRecognition(myPub, recipientPub, commitments);
      
      if (mr > 0) {
        tier1.push({ recipientPub, needSlot, mr, needQuantity: needSlot.quantity });
      } else if (myRecognitionOfThem > 0) {
        tier2.push({ recipientPub, needSlot, recognition: myRecognitionOfThem });
      }
    }
  }
  
  // Allocate Tier 1 (Mutual)
  const tier1Allocations = allocateTier(tier1, mySlot.quantity, "mutual");
  
  // Allocate Tier 2 (Non-Mutual) with remaining
  const remaining = mySlot.quantity - tier1Total;
  const tier2Allocations = allocateTier(tier2, remaining, "non-mutual");
  
  // Record allocations
  recordAllocation(mySlot, tier1Allocations, tier2Allocations);
}
```

**Two-Tier Allocation Math:**

From free-association.md:

**Tier 1 (Mutual):**
```
Your-Mutual-Recognition-Share = 
  Your_MR_with_Provider / Sum_of_All_Mutual_Recognition

Your-Raw-Allocation = 
  Provider's_Capacity 
  × Your_MR_Share 
  × Your_Active_Need 
  / Sum(Everyone's_Active_Need × Their_MR_Share)

Your-Final-Allocation = min(Your-Raw-Allocation, Your-Actual-Need)
```

**Example:**
```
Kitchen has 100 meals
Alice needs 40 meals, MR = 30%
Bob needs 30 meals, MR = 30%
Carol needs 50 meals, MR = 40%

Total MR = 100%

Alice's share = 30% × 100 = 30 meals
Bob's share = 30% × 100 = 30 meals
Carol's share = 40% × 100 = 40 meals
```

#### 6.2 Apply Damping
**Purpose:** Prevent oscillation, ensure convergence

**Backend Functions:**
```typescript
// From free-algorithm.svelte.ts

// Calculate over-allocation history
const overAllocationHistory = [
  { timestamp: t-2, overAllocation: 10 },
  { timestamp: t-1, overAllocation: -5 },
  { timestamp: t, overAllocation: 8 }
]; // Oscillating!

// Detect oscillation
detectOscillation(history): boolean

// Compute damping factor
computeDampingFactor(history): number // 0.5, 0.8, or 1.0

// Apply damping
Your-Active-Need = Your-Stated-Need × Damping-Factor
```

**From free-association.md:**
- Full-Speed (1.0): Smooth convergence
- Medium (0.8): Default
- Slow (0.5): Oscillation detected

**Data Storage:**
```typescript
{
  multi_dimensional_damping: {
    damping_factors: {
      "food": 1.0,      // Smooth
      "healthcare": 0.5  // Oscillating
    },
    damping_history: {
      "food": [...],
      "healthcare": [...]
    }
  }
}
```

#### 6.3 Update Needs
**Purpose:** Reduce needs based on allocations received

**Backend Functions:**
```typescript
// From free-algorithm.svelte.ts

// Apply update law
applyNeedUpdateLaw()

// Updates commitment:
Need(tomorrow) = max(0, Need(today) - Allocation-Received(today))

// Per-type:
for (const typeId of needTypes) {
  const received = totalReceivedByType[typeId] || 0;
  const currentNeed = getNeedForType(typeId);
  const newNeed = Math.max(0, currentNeed - received);
  updateNeedSlotQuantity(typeId, newNeed);
}
```

**From free-association.md:**
```
Alice needs 50 meals
Alice receives 30 meals today
Tomorrow: Alice needs max(0, 50 - 30) = 20 meals
Next day: Alice receives 25 meals
Day after: Alice needs max(0, 20 - 25) = 0 meals (satisfied!)
```

#### 6.4 Publish Updated Commitment
**Purpose:** Share updated state with network

**Backend Functions:**
```typescript
// From free-algorithm.svelte.ts

publishMyCommitment(commitment: Commitment): Promise<void>

// Commitment contains:
{
  capacity_slots: [...], // Updated allocated_quantity
  need_slots: [...],     // Updated quantity (reduced)
  global_recognition_weights: {...}, // From tree
  timestamp: Date.now(),
  itcStamp: myITCStamp   // Causality tracking
}
```

**P2P Sync:**
```typescript
// Via stores.svelte.ts
myCommitmentStore.update(() => commitment);

// Automatically syncs to:
holster.get('allocation/commitment').put(commitment)

// Other participants subscribe:
subscribeToCommitment(pubKey)
```

---

## 📈 PHASE 7: VIEWING ALLOCATIONS

### User Story
> "I want to see what I'm giving and receiving"

### Journey Steps

#### 7.1 View Shares Received
**User Action:** Navigate to "Shares" view in Inventory

**UI Components:**
- `Shares.svelte` - List of all capacities you have access to
- `Share.svelte` - Individual capacity card

**State Management:**
- `networkCapacities` - All available capacities
- Computed allocations from algorithm

**Display per Capacity:**
```
Kitchen - Food Capacity 🍽️
  Slot: Monday 12-2pm
    ├─ Allocated: 30 meals
    ├─ Available: 100 meals total
    ├─ Your share: 30%
    ├─ Your desire: 40 meals
    └─ Status: ✅ In mutual tier
```

**Backend Functions:**
```typescript
// From protocol.ts
getSlotAllocatedQuantity(capacity, slotId): number
getSlotAvailableQuantity(capacity, slotId): number
getAllocatedSlotCount(capacity): number
getTotalAllocated(capacity): number
```

#### 7.2 View Allocations Given
**User Action:** See who's receiving from your capacities

**UI Components:**
- `Capacity.svelte` - Shows recipient shares
- Per-slot breakdown

**Display:**
```
My Tutoring Capacity 📚
  Slot: Monday 2-4pm, 10 hours
    ├─ Alice: 3 hours (MR: 30%)
    ├─ Bob: 3 hours (MR: 30%)
    ├─ Carol: 4 hours (MR: 40%)
    └─ Remaining: 0 hours
```

**Backend Functions:**
```typescript
// From protocol.ts (allocation utilities)
getMutualTierRecipients(allocationState, slotId): string[]
getMutualTierTotal(allocationState, slotId): number
getNonMutualTierTotal(allocationState, slotId): number
isRecipientMutual(allocationState, slotId, recipientPub): boolean
```

#### 7.3 View Convergence Metrics
**User Action:** See system health and convergence status

**UI Components:**
- Debug panels (development mode)
- Console logs

**Backend Functions:**
```typescript
// From free-algorithm.svelte.ts

computeConvergenceSummary(
  currentState: SystemStateSnapshot,
  previousState: SystemStateSnapshot | null,
  iterationStartTime: number
): ConvergenceSummary

// Returns:
{
  totalNeedMagnitude: number;        // ||N⃗⃗(t)||_F (Frobenius norm)
  contractionRate: number;           // k < 1
  isConverged: boolean;              // Below threshold?
  percentNeedsMet: number;           // % of people satisfied
  universalSatisfaction: boolean;    // All needs zero?
  iterationsToConvergence: number | null;
  responseLatency: number;           // ms per iteration
  maxPersonNeed?: number;            // Worst-case participant
  needVariance?: number;             // Distribution inequality
  peopleStuck?: number;              // Unchanging needs
}
```

**From free-association.md:**
```
Total-Needs(tomorrow) = k × Total-Needs(today), where k < 1

Example: k = 0.8
After 10 iterations: 0.8^10 = 10.7% of original needs remain
After 20 iterations: 0.8^20 = 1.2% of original needs remain

Typical convergence: ~5-20 iterations, 0.5-2 seconds
```

---

## 🌐 PHASE 8: NETWORK COORDINATION

### User Story
> "The system coordinates with others without a central server"

### Journey Steps

#### 8.1 Discover Participants
**Mechanism:** P2P gossip protocol

**Backend Functions:**
```typescript
// From stores.svelte.ts

// Subscribe to participant's data
subscribeToCommitment(pubKey: string)
subscribeToFullParticipant(pubKey: string)

// Unsubscribe
unsubscribeFromParticipant(pubKey: string)

// Get subscribed list
getSubscribedParticipants(): string[]
```

**Discovery Process:**
1. User's recognition tree contains contributor IDs
2. System resolves contact IDs → public keys: `resolveToPublicKey(contactId)`
3. Auto-subscribes to contributors: `subscribeToFullParticipant(pubKey)`
4. Receives their commitments via: `networkCommitments.update(...)`

**Data Flow:**
```typescript
// My tree references Alice
{ contributor_ids: ["contact_alice_123"] }

// System resolves
resolveToPublicKey("contact_alice_123") → "pubkey_abc..."

// System subscribes
subscribeToCommitment("pubkey_abc...")

// Receives Alice's commitment
networkCommitments.update((map) => {
  map.set("pubkey_abc...", aliceCommitment);
  return map;
});

// Triggers reactive allocation recomputation
```

#### 8.2 Maintain Causal Consistency
**Purpose:** Ensure everyone sees events in consistent order

**Mechanism:** ITC (Interval Tree Clocks)

**Backend Functions:**
```typescript
// From free-algorithm.svelte.ts

// My ITC stamp
getMyITCStamp(): ITCStamp

// Increment when I make changes
incrementMyITCStamp(): void

// Merge when receiving peer updates
mergeITCStampFromPeer(peerStamp: ITCStamp): void

// Check if peer update is stale
isPeerUpdateStale(peerStamp: ITCStamp): boolean

// Get causally consistent view
getCausallyConsistentCommitments(): Record<string, Commitment>
```

**ITC Structure:**
```typescript
interface ITCStamp {
  id: 0 | 1 | { l: ITCId; r: ITCId };      // Fork ID component
  event: number | { n: number; l: ITCEvent; r: ITCEvent }; // Event counter
}
```

**Usage:**
```
My stamp: { id: 1, event: 5 }
Alice's update: { id: ..., event: 7 }

isPeerUpdateStale(aliceStamp)?
  → No, Alice's event 7 > my event 5
  → Accept and merge: incrementMyITCStamp()
```

**From free-association.md:**
> "Causal Consistency: The system tracks which events each participant has seen, ensuring everyone has a consistent view of history even if messages arrive in different orders."

#### 8.3 Build Spatial/Temporal Indexes
**Purpose:** Fast lookup instead of O(N) scans

**Backend Functions:**
```typescript
// From stores.svelte.ts

// Reactive indexes (auto-maintained)
export const networkNeedsIndex: Readable<SpaceTimeIndex>
export const networkCapacityIndex: Readable<SpaceTimeIndex>

// Incremental updates O(M) instead of O(N×M)
function updateIndexForParticipant(
  pubKey: string,
  commitment: Commitment | undefined,
  index: SpaceTimeIndex,
  isNeedIndex: boolean
): void
```

**Index Building:**
```typescript
// When Alice's commitment changes:
updateIndexForParticipant("alice_pub", aliceCommitment, needsIndex, true);

// Updates:
needsIndex.byType.get("food").add("alice_pub");
needsIndex.byLocation.get("san-francisco").add("alice_pub");
needsIndex.byTime.get("2024-06").add("alice_pub");
needsIndex.byAll.get("food|san-francisco|2024-06").add("alice_pub");

// O(M) complexity where M = Alice's slots
// NOT O(N×M) where N = all participants!
```

**Usage in Allocation:**
```typescript
// Fast candidate lookup
const candidates = getCandidateRecipients(mySlot, needsIndex);

// O(1) lookup:
const foodRecipients = needsIndex.byType.get(mySlot.need_type_id);
const localRecipients = needsIndex.byLocation.get(mySlot.city);
const nowRecipients = needsIndex.byTime.get(currentMonth);

// Intersection = compatible candidates
```

---

## 🔄 PHASE 9: CONTINUOUS ITERATION

### User Story
> "The system continuously adapts as needs and capacities change"

### Journey Steps

#### 9.1 Detect Changes
**Triggers:**
- User updates recognition tree
- User updates capacity/needs
- Network state changes
- Time passes (recurring slots activate)

**Reactive System:**
```typescript
// Svelte 5 reactivity
$effect(() => {
  if (myTreeChanged || networkStateChanged) {
    recomputeAllocations();
  }
});
```

#### 9.2 Recompute Allocations
**Frequency:** Event-driven (no fixed rounds)

**Backend Functions:**
```typescript
// From free-algorithm.svelte.ts

// Update system state
updateSystemStateFromNetwork(): void

// Run allocation
const newAllocations = runEfficientAllocation(...);

// Apply need updates
applyNeedUpdateLaw();

// Update damping
updateCommitmentWithDampingHistory(totalReceivedByType);

// Publish
publishMyCommitment(updatedCommitment);
```

**Performance:**
- Response time: ~100ms per iteration
- Convergence: ~0.5-2 seconds
- Iterations: ~5-20 until convergence

#### 9.3 Monitor Convergence
**Check:**
```typescript
const summary = computeConvergenceSummary(currentState, previousState, startTime);

if (summary.isConverged) {
  console.log("✅ System converged!");
  console.log(`${summary.percentNeedsMet}% of needs met`);
  console.log(`Contraction rate: ${summary.contractionRate}`);
}
```

**From free-association.md - Mathematical Guarantees:**
```
Proof 1: Needs Always Decrease
  Need(t+1) ≤ Need(t)
  → Total-Needs always contracting

Proof 2: Universal Satisfaction
  If capacity sufficient → All needs converge to zero
  Fixed point only possible at Need = 0

Proof 3: Exponential Convergence
  Need(t) ≤ k^t × Need(0), where k < 1
  → Predictable convergence time

Proof 4: No Accumulation
  No equation: Wealth(t+1) = Wealth(t) + Received(t)
  Only: Need(t+1) = Need(t) - Received(t)
  → Mathematical abolition of accumulation
```

---

## 🗂️ COMPLETE COMPONENT INVENTORY

### State Management (`src/lib/state/`)

#### Core State
- `core.svelte.ts` - Central state (tree, capacities, desires, recognition)
- `auth.svelte.ts` - Unified authentication interface
- `holster.svelte.ts` - Holster P2P authentication
- `gun.svelte.ts` - GUN P2P authentication
- `users.svelte.ts` - Contacts and public key resolution
- `chat.svelte.ts` - P2P messaging
- `calculations.svelte.ts` - Recognition calculations
- `network.svelte.ts` - Network synchronization

#### V5 Allocation System
- `v5/stores.svelte.ts` - P2P synchronized allocation stores
- `v5/protocol.ts` - Core recognition/allocation protocol
- `v5/schemas.ts` - Type definitions and validation
- `v5/match.svelte.ts` - Slot compatibility matching
- `v5/free-algorithm.svelte.ts` - Allocation algorithm implementation

### UI Components (`src/lib/components/`)

#### Recognition Tree
- `Parent.svelte` - Main tree visualization (D3 treemap)
- `Child.svelte` - Individual tree node
- `Bar.svelte` - Recognition bar charts
- `DropDown.svelte` - Contributor selection

#### Capacity Management
- `Capacities.svelte` - List of user's capacities
- `Capacity.svelte` - Individual capacity card
- `Slot.svelte` - Availability slot editor
- `SlotCompositionItem.svelte` - Desire input for specific slot

#### Allocation Viewing
- `Shares.svelte` - Network capacities available to user
- `Share.svelte` - Individual network capacity card
- `MapSidePanel.svelte` - Geographic capacity details

#### Navigation
- `Header.svelte` - Top navigation and auth
- `ToolBar.svelte` - Bottom toolbar with actions
- `Map.svelte` - Geographic visualization (Leaflet)

#### Utility
- `DraggedNode.svelte` - Drag preview
- `TagPill.svelte` - Tags and labels
- `CountrySelector.svelte` - Country picker
- `TimezoneSelector.svelte` - Timezone picker
- `Chat.svelte` - P2P messaging UI

### Routing (`src/routes/`)
- `+layout.svelte` - Root layout
- `+page.svelte` - Main view (tree/map/inventory)
- `collective/+page.svelte` - Collective allocation view
- `decider/+page.svelte` - P2P decision making
- `unconference/+page.svelte` - Unconference scheduling

### Utilities (`src/lib/`)
- `filters.ts` - Filter rule evaluation
- `templates.ts` - Tree templates (SDG, etc.)
- `translations.ts` - i18n support
- `ui-providers.svelte.ts` - Data providers for dropdowns
- `global.svelte.ts` - Global UI state

---

## 📊 DATA FLOW SUMMARY

```
┌─────────────────────────────────────────────────────────────┐
│                    USER INTERACTIONS                        │
└─────────────────────────────────────────────────────────────┘
  │
  ├─ Add contributor to tree → protocol.addContributors()
  ├─ Create capacity → userCapacities.update()
  ├─ Express desire → userDesiredSlotComposeFrom.update()
  └─ Adjust slot → Slot.svelte onChange
  
  ↓ (all trigger)
  
┌─────────────────────────────────────────────────────────────┐
│                RECOGNITION CALCULATION                      │
└─────────────────────────────────────────────────────────────┘
  │
  ├─ sharesOfGeneralFulfillmentMap(tree) 
  │   → { alice: 0.30, bob: 0.30, carol: 0.40 }
  │
  └─ Store in: userSogf, generalShares
  
  ↓
  
┌─────────────────────────────────────────────────────────────┐
│                  COMMITMENT CREATION                        │
└─────────────────────────────────────────────────────────────┘
  │
  ├─ Gather: capacity_slots, need_slots, recognition_weights
  ├─ Create: createCommitment()
  └─ Publish: publishMyCommitment()
  
  ↓ (P2P sync)
  
┌─────────────────────────────────────────────────────────────┐
│                   NETWORK STATE                             │
└─────────────────────────────────────────────────────────────┘
  │
  ├─ Subscribe: subscribeToCommitment(pubKey)
  ├─ Receive: networkCommitments.update()
  ├─ Index: networkCapacityIndex, networkNeedsIndex
  └─ Merge: mergeITCStampFromPeer()
  
  ↓ (triggers)
  
┌─────────────────────────────────────────────────────────────┐
│                ALLOCATION ALGORITHM                         │
└─────────────────────────────────────────────────────────────┘
  │
  ├─ For each availability slot:
  │   ├─ Find candidates: getCandidateRecipients()
  │   ├─ Check compatibility: slotsCompatible()
  │   ├─ Check filters: passesSlotFilters()
  │   ├─ Calculate MR: computeMutualRecognition()
  │   ├─ Split tiers: tier1 (mutual), tier2 (non-mutual)
  │   ├─ Allocate Tier 1: (MR-weighted, need-based)
  │   ├─ Allocate Tier 2: (recognition-weighted, need-based)
  │   └─ Record: SlotAllocationRecord[]
  │
  ├─ Apply damping: computeDampingFactor()
  ├─ Update needs: applyNeedUpdateLaw()
  └─ Check convergence: computeConvergenceSummary()
  
  ↓
  
┌─────────────────────────────────────────────────────────────┐
│                  STATE UPDATE                               │
└─────────────────────────────────────────────────────────────┘
  │
  ├─ Update local commitment
  ├─ Publish to network: publishMyCommitment()
  └─ Trigger UI reactivity
  
  ↓
  
┌─────────────────────────────────────────────────────────────┐
│                    UI DISPLAY                               │
└─────────────────────────────────────────────────────────────┘
  │
  ├─ Recognition bars: Bar.svelte ($userSogf)
  ├─ Capacity allocations: Capacity.svelte (recipient shares)
  ├─ Shares received: Share.svelte (allocated quantities)
  └─ Convergence metrics: Console logs
```

---

## 🎓 KEY CONCEPTS FROM FREE-ASSOCIATION.MD

### 1. Global Recognition (Not Type-Specific)
```
Tree encodes contributions:
  Healthcare (70pts) → Dr. Smith (80pts) = 56% global recognition
  Food (30pts) → Alice (50pts) = 15% global recognition

Same 56% and 15% used for ALL allocations (food, healthcare, housing)
Type preferences naturally encoded in tree structure
```

### 2. Two-Tier System
```
Tier 1: Mutual Recognition (priority)
  - Both people recognize each other
  - MR = min(A→B recognition, B→A recognition)

Tier 2: Generous Giving (remaining capacity)
  - One-way recognition (A recognizes B, but not mutual)
  - Solidarity with newcomers
```

### 3. Slot-Native Allocation
```
NOT: "Alice needs 100 food, Kitchen has 200 food"
YES: "Alice needs 40 meals Tuesday 12-2pm Downtown
      Kitchen offers 100 meals Tuesday 12-2pm Downtown"

Allocation respects space-time constraints
No abstract resource pools
```

### 4. Damping for Convergence
```
Problem: Oscillation (too much → too little → too much)
Solution: Adaptive damping

Active-Need = Stated-Need × Damping-Factor
  - 1.0 (full speed): Smooth convergence
  - 0.8 (medium): Default
  - 0.5 (slow): Oscillation detected

Per-type damping: Food might converge smoothly (1.0)
                  while healthcare oscillates (0.5)
```

### 5. Need Update Law
```
Need(tomorrow) = max(0, Need(today) - Received(today))

Properties:
  - Needs always decrease or stay same
  - Can never go below zero
  - No accumulation possible
  - Converges exponentially: Need(t) ≤ k^t × Need(0)
```

### 6. Mathematical Guarantees
```
IF:
  1. Mutual recognition exists
  2. Sufficient capacity in network
  3. Adaptive damping enabled

THEN (mathematically proven):
  - All needs converge to zero
  - Convergence time: seconds to minutes
  - No accumulation possible
  - System is antifragile (self-correcting)
```

---

## 🚀 FUTURE ENHANCEMENTS

### Currently Not Implemented But Spec'd
1. **Standalone Need Slots** - Currently UI uses "desire" composition model
2. **Advanced Filter Types** - Only basic filters implemented
3. **Convergence Visualization** - Metrics logged but not visualized
4. **Multi-Device Sync** - Works but could be more robust
5. **Offline Mode** - Partial support, needs enhancement

### Potential Additions
1. **Mobile App** - PWA exists, native app possible
2. **Notification System** - Basic notifications, could be enhanced
3. **Analytics Dashboard** - View network health, convergence rates
4. **Template Marketplace** - Share recognition tree templates
5. **Collective Governance** - Decision-making tools (partially implemented)

---

## 📚 REFERENCES

### Code Files
- **Protocol**: `src/lib/commons/v5/protocol.ts`
- **Algorithm**: `src/lib/commons/v5/free-algorithm.svelte.ts`
- **Schemas**: `src/lib/commons/v5/schemas.ts`
- **Matching**: `src/lib/commons/v5/match.svelte.ts`
- **Stores**: `src/lib/commons/v5/stores.svelte.ts`

### Documentation
- **Plain English Guide**: `src/lib/commons/v5/free-association.md`
- **Architecture**: `docs/allocation_differences_analysis.md`
- **API Guide**: `docs/APIs/guide.md`

### Key Types
```typescript
// Recognition
type ShareMap = Record<string, number>; // pubKey → share (0-1)

// Nodes
type Node = RootNode | NonRootNode;
interface NonRootNode {
  id: string;
  name: string;
  points: number;
  contributor_ids: string[];
  anti_contributors_ids: string[];
  children: Node[];
}

// Slots
interface AvailabilitySlot {
  id: string;
  quantity: number;
  need_type_id: string;
  availability_window?: AvailabilityWindow;
  location_type?: string;
  filter_rule?: FilterRule;
}

interface NeedSlot {
  // Same structure as AvailabilitySlot
}

// Commitment
interface Commitment {
  capacity_slots?: AvailabilitySlot[];
  need_slots?: NeedSlot[];
  global_recognition_weights: ShareMap;
  itcStamp: ITCStamp;
  timestamp: number;
}

// Allocation
interface TwoTierAllocationState {
  slot_denominators: Record<string, { mutual: number; nonMutual: number }>;
  slot_allocations: SlotAllocationRecord[];
  recipient_totals_by_type: Record<string, Record<string, number>>;
}
```

---

## ✅ IMPLEMENTATION CHECKLIST

### Phase 1: Authentication ✅
- [x] Holster integration
- [x] Public/private keypair generation
- [x] Recall authentication
- [x] Login/signup flows

### Phase 2: Recognition Tree ✅
- [x] Tree visualization (D3 treemap)
- [x] Add/edit/delete nodes
- [x] Add/remove contributors
- [x] Calculate recognition shares
- [x] Anti-contributors support

### Phase 3: Capacity Declaration ✅
- [x] Create capacities
- [x] Add availability slots
- [x] Time/location configuration
- [x] Filter rules
- [x] Recurrence patterns

### Phase 4: Need Declaration ✅
- [x] Browse network capacities
- [x] Express desire (composition model)
- [x] Need slot structure (spec'd)

### Phase 5: Allocation ✅
- [x] Slot matching algorithm
- [x] Two-tier allocation
- [x] Damping system
- [x] Need update law
- [x] Convergence metrics

### Phase 6: Network Coordination ✅
- [x] P2P sync (Holster/GUN)
- [x] ITC causality tracking
- [x] Spatial/temporal indexes
- [x] Incremental updates

### Phase 7: UI/UX ✅
- [x] Tree view
- [x] Map view
- [x] Inventory view
- [x] Shares view
- [x] Responsive design
- [x] i18n support

### Phase 8: Polish 🚧
- [x] Error handling
- [x] Loading states
- [ ] Advanced analytics UI
- [ ] Onboarding flow
- [ ] Help documentation

---

**END OF USER JOURNEY TRACE**

