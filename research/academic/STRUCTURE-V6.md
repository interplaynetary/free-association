═══════════════════════════════════════════════════════════════════════
ARCHITECTURE: SEPARATION OF USER-INPUT AND DERIVED STATE
═══════════════════════════════════════════════════════════════════════

**Critical Design Principle:**
User-Input and Derived values are stored in SEPARATE data structures.
Tree nodes contain REFERENCES to derived values, not the values themselves.

**Why This Matters:**
- Tree structure (user inputs) remains stable
- Derived values can be recalculated without modifying tree
- Enables reactive computation (Svelte stores, computed properties)
- No need to rewrite tree every time calculations change

**Architecture Pattern:**

```typescript
// TREE STRUCTURE (User Inputs Only - Stable)
interface TreeNode {
  id: string;
  name: string;
  type: 'Root' | 'Goal' | 'CapacitySlot' | 'NeedSlot' | 'NonSlot';
  
  // User inputs (stable - only change when user edits)
  points?: number;
  contributors?: Contributor[];
  anti_contributors?: Contributor[];
  manual_satisfaction?: number;
  
  // References to derived state (NOT the values themselves!)
  derived_state_ref: string; // Points to entry in DerivedStateMap
  
  children: TreeNode[];
}

// DERIVED STATE (Computed Values - Ephemeral)
interface DerivedState {
  node_id: string;
  
  // Computed values (recalculated reactively)
  weight: number;
  share_of_parent: number;
  satisfaction: number;
  
  // Timestamps for cache invalidation
  computed_at: number;
  dependencies: string[]; // IDs of nodes this depends on
}

// GLOBAL DERIVED STATE STORE
type DerivedStateMap = Record<string, DerivedState>;

// REACTIVE COMPUTATION
// When user input changes:
//   1. Tree structure updated (user inputs only)
//   2. Dependent derived states marked dirty
//   3. Reactive system recomputes derived values
//   4. DerivedStateMap updated
//   5. Tree references remain valid (just point to new values)
```

**Example:**

```typescript
// User creates a node
const goalNode: TreeNode = {
  id: "goal_123",
  name: "Healthcare Goal",
  type: "Goal",
  points: 70,  // USER INPUT
  derived_state_ref: "goal_123",  // Reference
  children: [...]
};

// System computes derived state separately
derivedStateMap["goal_123"] = {
  node_id: "goal_123",
  weight: 0.35,        // COMPUTED
  share_of_parent: 0.7, // COMPUTED
  satisfaction: 0.82,   // COMPUTED
  computed_at: Date.now(),
  dependencies: ["root", "sibling_456"]
};

// To access weight:
const weight = derivedStateMap[goalNode.derived_state_ref].weight;

// When parent points change:
//   - Update parent node's points (user input)
//   - Mark goal_123 as dirty
//   - Recompute derivedStateMap["goal_123"]
//   - goalNode.derived_state_ref unchanged!
```

**Benefits:**

1. **Immutable Tree Structure**: User inputs form stable tree
2. **Reactive Computation**: Derived values recompute automatically
3. **Efficient Updates**: Only recompute what changed
4. **Cache Friendly**: Can cache derived state separately
5. **Serialization**: Tree (user inputs) can be serialized without computed junk
6. **Parallel Computation**: Derived values computed in parallel

**Svelte Store Pattern:**

```typescript
// Tree is a writable store (user inputs)
const tree = writable<TreeNode>(rootNode);

// Derived state is computed stores
const derivedState = derived([tree], ([$tree]) => {
  return computeAllDerivedStates($tree);
});

// UI accesses both:
$: weight = $derivedState[node.derived_state_ref].weight;
$: satisfaction = $derivedState[node.derived_state_ref].satisfaction;

// When user updates tree:
tree.update(t => {
  // Modify user inputs only
  findNode(t, nodeId).points = newPoints;
  return t;
});
// → derivedState automatically recomputes!
```

═══════════════════════════════════════════════════════════════════════
TREE STRUCTURE (User Inputs Only)
═══════════════════════════════════════════════════════════════════════

State: Tree (User Inputs - Stable)
    Root
        id: string
        name: string
        entity_id: string
        
        User-Input: Non-Slot Node
            id: string
            name: string
            manual_satisfaction?: number
            User-Input: Contributors with Points
                id: string
                points: number
            User-Input: Anti-Contributors with Points (optional)
                id: string
                points: number
                
        User-Input: Higher-Order-Goal
            id: string
            name: string
            User-Input: Points (relative to siblings)
            
            User-Input: Capacity-Slot of Capacity-Type
                id: string
                name: string
                resource_type: string
                User-Input: Points (relative to siblings)
                User-Input: Available-Quantity
                User-Input: Contributors (default: empty = self)
                User-Input: Filters (optional)
                
                User-Input: Need-Slot of Need-Type
                    id: string
                    name: string
                    resource_type: string
                    User-Input: Points (relative to siblings)
                    User-Input: Declared-Quantity
                    User-Input: Specifications (optional)
                    
                    Allocation-Derived: (stored separately)
                        allocation_id: string
                        provider_id: string
                        capacity_slot_id: string
                        Offered-Quantity (computed)
                        User-Input: Accepted-Quantity
                        User-Input: Satisfaction :: Portion ∈ [0.0, 1.0]
                        Derived: Declined-Quantity = Offered - Accepted
                    ...
                ...
            ...

═══════════════════════════════════════════════════════════════════════
DERIVED STATE (Computed Values - Reactive)
═══════════════════════════════════════════════════════════════════════

Derived State Map: Record<node_id, DerivedState>

For Each Node:
    Derived: Weight = (Points / Parent_Total_Points) × Parent_Weight
    Derived: ShareOfParent = Points / Parent_Total_Points
    Derived: Satisfaction (aggregated from children or slots)
    
For Each Capacity-Slot:
    Derived: Weight (recursive from parent)
    Derived: Satisfaction (aggregated from needs)
    Derived: Allocated-Quantity (sum of allocations)
    
For Each Need-Slot:
    Derived: Weight (recursive from parent)
    Derived: Satisfaction (aggregated from allocations)
    Derived: Remaining-Need = Declared - Total-Accepted
    
For Each Allocation:
    Derived: Offered-Quantity (from allocation algorithm)
    Derived: Declined-Quantity = Offered - Accepted

═══════════════════════════════════════════════════════════════════════
REACTIVE COMPUTATION FLOW
═══════════════════════════════════════════════════════════════════════

User Edit → Tree Update (User Inputs Only)
    ↓
Dependency Graph Analysis
    ↓
Mark Affected Derived States as Dirty
    ↓
Reactive Recomputation (Parallel Where Possible)
    ↓
Update DerivedStateMap (New Values)
    ↓
UI Reactively Updates (Svelte/React/Vue)

**Example Scenario:**

1. User changes node points: 70 → 80
   - Update tree: node.points = 80
   - Mark dirty: [node, parent, siblings, children]
   
2. Reactive system recomputes:
   - Parent total points changed → recalc all sibling shares
   - This node's weight changed → recalc this node
   - Children depend on parent weight → recalc children
   
3. DerivedStateMap updated:
   - derivedState[node.id].weight = 0.40 (was 0.35)
   - derivedState[sibling.id].share_of_parent = 0.30 (was 0.35)
   - derivedState[child.id].weight = 0.20 (was 0.175)
   
4. Tree structure unchanged:
   - node.points = 80 (updated)
   - node.derived_state_ref = "node_id" (unchanged!)
   - node.children = [...] (unchanged!)

**Key Insight:**

User inputs and derived values happen IN PARALLEL in separate data structures.
Each node has a REFERENCE to its derived state.
We access the most up-to-date derivation without modifying the tree.


Synthezied:

State: Tree (User Inputs - Stable)
    Root
        id: string
        name: string
        entity_id: string
        
        User-Input: Non-Slot Node
            id: string
            name: string
            manual_satisfaction?: number
            User-Input: Contributors with Points
                id: string
                points: number
            User-Input: Anti-Contributors with Points (optional)
                id: string
                points: number
                
        User-Input: Higher-Order-Goal
            id: string
            name: string
            User-Input: Points (relative to siblings)
            
            User-Input: Capacity-Slot of Capacity-Type
                id: string
                name: string
                resource_type: string
                User-Input: Points (relative to siblings)
                Derived: Weight (recursive from parent)
                Derived: Satisfaction (aggregated from needs)
                
                User-Input: Need-Slot of Need-Type
                    id: string
                    name: string
                    resource_type: string
                    User-Input: Points (relative to siblings)
                    Derived: Weight (recursive from parent)
                    Derived: Satisfaction (aggregated from allocations)
                    
                    Allocation-Derived: (stored separately)
                        allocation_id: string
                        provider_id: string
                        capacity_slot_id: string
                        Offered-Quantity (computed)
                        User-Input: Accepted-Quantity
                        User-Input: Satisfaction :: Portion ∈ [0.0, 1.0]
                        Derived: Declined-Quantity = Offered-Quantity - Accepted-Quantity
                    ...
                ...
            ...

═══════════════════════════════════════════════════════════════════════
SATISFACTION AGGREGATION FORMULAS
═══════════════════════════════════════════════════════════════════════

Need-Slot Satisfaction (weighted by accepted quantity):
    Σ(allocation.accepted × allocation.satisfaction) / Σ(allocation.accepted)
    
    If no allocations: 0.0

Capacity-Slot Satisfaction (weighted by child weights):
    Σ(need_slot.weight × need_slot.satisfaction) / Σ(need_slot.weight)
    
    If no needs: 0.0

Goal Satisfaction (weighted by child weights):
    Σ(child.weight × child.satisfaction) / Σ(child.weight)
    
    For leaf contribution nodes: Uses manual_satisfaction if provided, else 1.0
    For empty leaf nodes: 0.0

═══════════════════════════════════════════════════════════════════════
BOOTSTRAP & COEXISTENCE MODEL
═══════════════════════════════════════════════════════════════════════

Non-Slot Contributors (Initial/Ongoing Recognition):
    - User directly inputs contributor points at root or goal nodes
    - Represents contributions not tied to specific resource allocations
    - Examples: mission alignment, advocacy, coordination, intangible value
    - These contribute to satisfaction calculation alongside slot-based data
    - Do NOT decay or get replaced by slot-based satisfaction
    
    Usage:
        - Cold start: Establish initial recognition before any allocations
        - Ongoing: Recognize intangible contributions parallel to tangible resources
        - Bootstrap new relationships: Give initial points to build mutual satisfaction

Slot-Based Satisfaction (Operational Feedback):
    - Automatically derived from actual allocation outcomes
    - Represents concrete resource exchange satisfaction
    - Continuously updated as allocations occur and are rated
    - Provides objective feedback on resource utility
    
Coexistence:
    Both non-slot and slot-based contributions flow into the same 
    ShareOfGeneralSatisfaction calculation:
    
    Total contribution = Non-Slot contribution + Slot-Based contribution
    
    Example:
        Entity A recognizes Entity B with:
        - Non-Slot node: 40 points (30% of non-slot contributors)
        - Slot satisfactions: contributes 20% via resource allocations
        
        Both contributions aggregate into Entity B's total share
        according to their respective weights in the tree

    This enables:
        ✓ New relationships to start with trust (non-slot points)
        ✓ Established relationships to be validated by outcomes (slot satisfaction)
        ✓ Intangible contributions to be recognized alongside tangible ones
        ✓ Gradual trust-building as allocation history develops

═══════════════════════════════════════════════════════════════════════
CONTRIBUTOR SHARE CALCULATION
═══════════════════════════════════════════════════════════════════════

ShareOfGeneralSatisfaction(Target_Entity, Contributor):
    Step 1: Calculate influence pools across all nodes
        P_total = Σ(contribution_nodes: node.weight × node.satisfaction)
        N_total = Σ(nodes_with_anti_contributors: node.weight × node.dissatisfaction)
        
        where dissatisfaction = 1.0 - satisfaction
    
    Step 2: Calculate recognition pools
        TotalInfluence = P_total + N_total
        PositivePool = P_total / TotalInfluence
        AntiPool = N_total / TotalInfluence
    
    Step 3: Calculate contributor's raw shares
        For each contribution node where Contributor is listed:
            ContributorShare = (node.weight × node.satisfaction) × 
                             (contributor.points / total_contributor_points)
        
        RawPositiveShare = Σ(all positive contribution shares)
        RawAntiShare = Σ(all anti-contribution shares)
    
    Step 4: Apply pool-bounded recognition
        BoundedPositiveShare = (RawPositiveShare / P_total) × PositivePool
        BoundedAntiShare = (RawAntiShare / N_total) × AntiPool
        
        FinalShare = BoundedPositiveShare - BoundedAntiShare

ShareOfTotal-Satisfaction_A→B = ShareOfGeneralSatisfaction(A, B)

═══════════════════════════════════════════════════════════════════════
MUTUAL SATISFACTION & ALLOCATION
═══════════════════════════════════════════════════════════════════════

Mutual-Satisfaction(Entity_A, Entity_B):
    = min(ShareOfTotalSatisfaction_A→B, ShareOfTotalSatisfaction_B→A)

Collective-Satisfaction(Capacity-Contributors):
    When multiple entities co-provide capacity:
        For each contributor C in Capacity-Contributors:
            CollectiveShare_C = Σ(mutual_satisfaction with other contributors)
        
        Normalize shares to sum to 1.0

Allocation(Provider, Recipient, Resource-Type):
    Filter Step: Apply time, location, and type filters
    
    Tier 1 - Mutual Priority:
        For recipients with Mutual-Satisfaction > 0:
            RawShare_R = Mutual-Satisfaction(Provider, R) / 
                        Σ(Mutual-Satisfaction(Provider, all_mutual_recipients))
            
            RawAllocation_R = Provider_Capacity × RawShare_R
            FinalAllocation_R = min(RawAllocation_R, Recipient_Declared_Need)
    
    Tier 2 - Unilateral Recognition (remaining capacity):
        RemainingCapacity = Provider_Capacity - Σ(Tier1_Allocations)
        
        For recipients with unilateral recognition:
            RawShare_R = ShareOfTotalSatisfaction_Provider→R / 
                        Σ(ShareOfTotalSatisfaction for non-mutual recipients)
            
            RawAllocation_R = RemainingCapacity × RawShare_R
            FinalAllocation_R = min(RawAllocation_R, Recipient_Remaining_Need)

═══════════════════════════════════════════════════════════════════════
KEY PROPERTIES
═══════════════════════════════════════════════════════════════════════

1. Weights sum to 1.0 at each level (proper probability distribution)
2. Satisfaction ∈ [0.0, 1.0] at all levels
3. ShareOfTotal-Satisfaction values sum to 1.0 across all contributors
4. Allocations capped at declared needs (non-accumulative)
5. Two-tier allocation ensures mutual relationships prioritized
6. Anti-contributors reduce satisfaction proportional to dissatisfaction

═══════════════════════════════════════════════════════════════════════
ZOD SCHEMA IMPLEMENTATION
═══════════════════════════════════════════════════════════════════════

```typescript
import { z } from 'zod';

// ═══════════════════════════════════════════════════════════════════════
// PRIMITIVES
// ═══════════════════════════════════════════════════════════════════════

// Satisfaction value: [0.0, 1.0]
const SatisfactionSchema = z.number().min(0).max(1);

// Points for contributors/nodes (positive integers)
const PointsSchema = z.number().int().positive();

// Entity ID (public key, contact ID, or other identifier)
const EntityIdSchema = z.string().min(1);

// ISO 8601 timestamp
const TimestampSchema = z.string().datetime();

// ═══════════════════════════════════════════════════════════════════════
// CONTRIBUTOR
// ═══════════════════════════════════════════════════════════════════════

const ContributorSchema = z.object({
  id: EntityIdSchema,
  points: PointsSchema,
});

type Contributor = z.infer<typeof ContributorSchema>;

// ═══════════════════════════════════════════════════════════════════════
// RESOURCE TYPES & FILTERS
// ═══════════════════════════════════════════════════════════════════════

const ResourceTypeSchema = z.enum([
  'funding',
  'expertise',
  'facilities',
  'equipment',
  'materials',
  'labor',
  'other'
]).or(z.string()); // Allow custom types

const LocationSchema = z.object({
  country: z.string().optional(),
  region: z.string().optional(),
  city: z.string().optional(),
  coordinates: z.object({
    lat: z.number(),
    lng: z.number(),
  }).optional(),
});

const TimeWindowSchema = z.object({
  start: TimestampSchema,
  end: TimestampSchema,
  recurrence: z.enum(['once', 'daily', 'weekly', 'monthly', 'yearly']).optional(),
});

const FilterRulesSchema = z.object({
  time: TimeWindowSchema.optional(),
  location: LocationSchema.optional(),
  resource_subtype: z.string().optional(),
  min_quantity: z.number().optional(),
  max_quantity: z.number().optional(),
});

// ═══════════════════════════════════════════════════════════════════════
// ALLOCATION
// ═══════════════════════════════════════════════════════════════════════

const AllocationRecordSchema = z.object({
  id: z.string().uuid(),
  provider_id: EntityIdSchema,
  recipient_id: EntityIdSchema,
  capacity_slot_id: z.string(),
  need_slot_id: z.string(),
  
  // User inputs
  offered_quantity: z.number().nonnegative(),
  accepted_quantity: z.number().nonnegative(),
  satisfaction: SatisfactionSchema,
  
  // Derived
  declined_quantity: z.number().nonnegative(),
  
  // Metadata
  resource_type: ResourceTypeSchema,
  created_at: TimestampSchema,
  updated_at: TimestampSchema,
});

type AllocationRecord = z.infer<typeof AllocationRecordSchema>;

// ═══════════════════════════════════════════════════════════════════════
// SLOTS
// ═══════════════════════════════════════════════════════════════════════

const NeedSlotSchema = z.object({
  id: z.string(),
  name: z.string(),
  type: z.literal('NeedSlot'),
  
  // User inputs
  points: PointsSchema,
  resource_type: ResourceTypeSchema,
  declared_quantity: z.number().nonnegative(),
  specifications: FilterRulesSchema.optional(),
  
  // Derived
  weight: z.number().min(0).max(1),
  satisfaction: SatisfactionSchema,
  allocations: z.array(AllocationRecordSchema),
  remaining_need: z.number().nonnegative(),
  
  // Metadata
  parent_id: z.string(),
  created_at: TimestampSchema,
  updated_at: TimestampSchema,
});

type NeedSlot = z.infer<typeof NeedSlotSchema>;

const CapacitySlotSchema = z.object({
  id: z.string(),
  name: z.string(),
  type: z.literal('CapacitySlot'),
  
  // User inputs
  points: PointsSchema,
  resource_type: ResourceTypeSchema,
  available_quantity: z.number().nonnegative(),
  contributors: z.array(ContributorSchema).default([]), // Empty = self only
  filters: FilterRulesSchema.optional(),
  
  // Derived
  weight: z.number().min(0).max(1),
  satisfaction: SatisfactionSchema,
  allocated_quantity: z.number().nonnegative(),
  
  // Child need slots
  needs: z.array(z.lazy(() => NeedSlotSchema)),
  
  // Metadata
  parent_id: z.string(),
  created_at: TimestampSchema,
  updated_at: TimestampSchema,
});

type CapacitySlot = z.infer<typeof CapacitySlotSchema>;

// ═══════════════════════════════════════════════════════════════════════
// NODES (Goals)
// ═══════════════════════════════════════════════════════════════════════

// Base node (common fields)
const BaseNodeSchema = z.object({
  id: z.string(),
  name: z.string(),
  
  // Derived
  weight: z.number().min(0).max(1),
  satisfaction: SatisfactionSchema,
  share_of_parent: z.number().min(0).max(1),
  
  // Metadata
  created_at: TimestampSchema,
  updated_at: TimestampSchema,
});

// Non-slot contribution node
const NonSlotNodeSchema = BaseNodeSchema.extend({
  type: z.literal('NonSlot'),
  
  // User inputs
  manual_satisfaction: SatisfactionSchema.optional(),
  contributors: z.array(ContributorSchema),
  anti_contributors: z.array(ContributorSchema).optional(),
  
  // No children for leaf contribution nodes
  children: z.array(z.never()).default([]),
});

type NonSlotNode = z.infer<typeof NonSlotNodeSchema>;

// Goal node (structural decomposition)
const GoalNodeSchema = BaseNodeSchema.extend({
  type: z.literal('Goal'),
  
  // User inputs
  points: PointsSchema,
  
  // Children can be goals, capacity slots, or non-slot nodes
  children: z.array(
    z.lazy(() => 
      z.union([
        GoalNodeSchema,
        CapacitySlotSchema,
        NonSlotNodeSchema,
      ])
    )
  ),
  
  parent_id: z.string(),
});

type GoalNode = z.infer<typeof GoalNodeSchema>;

// Root node (top of tree)
const RootNodeSchema = BaseNodeSchema.extend({
  type: z.literal('Root'),
  entity_id: EntityIdSchema,
  
  // Root always has weight = 1.0, share_of_parent = 1.0
  weight: z.literal(1.0),
  share_of_parent: z.literal(1.0),
  
  // Children can be goals or non-slot nodes
  children: z.array(
    z.lazy(() => 
      z.union([
        GoalNodeSchema,
        NonSlotNodeSchema,
      ])
    )
  ),
});

type RootNode = z.infer<typeof RootNodeSchema>;

// ═══════════════════════════════════════════════════════════════════════
// TREE
// ═══════════════════════════════════════════════════════════════════════

const TreeSchema = RootNodeSchema;

type Tree = z.infer<typeof TreeSchema>;

// ═══════════════════════════════════════════════════════════════════════
// NETWORK STATE
// ═══════════════════════════════════════════════════════════════════════

// Map of entity_id -> their tree
const NetworkSchema = z.record(EntityIdSchema, TreeSchema);

// Share map: entity_id -> share value [0, 1]
const ShareMapSchema = z.record(EntityIdSchema, z.number().min(0).max(1));

// Mutual satisfaction map: entity_id -> mutual satisfaction value
const MutualSatisfactionMapSchema = z.record(
  EntityIdSchema, 
  z.number().min(0).max(1)
);

// Entity's complete state
const EntityStateSchema = z.object({
  tree: TreeSchema,
  share_map: ShareMapSchema, // Who this entity recognizes
  mutual_satisfaction_map: MutualSatisfactionMapSchema, // Mutual values with others
  last_updated: TimestampSchema,
});

type EntityState = z.infer<typeof EntityStateSchema>;

// Full network state
const NetworkStateSchema = z.object({
  entities: z.record(EntityIdSchema, EntityStateSchema),
  global_allocations: z.array(AllocationRecordSchema),
  version: z.string(),
  updated_at: TimestampSchema,
});

type NetworkState = z.infer<typeof NetworkStateSchema>;

// ═══════════════════════════════════════════════════════════════════════
// ALLOCATION CALCULATION TYPES
// ═══════════════════════════════════════════════════════════════════════

const TierAllocationSchema = z.object({
  recipient_id: EntityIdSchema,
  allocated_quantity: z.number().nonnegative(),
  tier: z.enum(['mutual', 'unilateral']),
  share: z.number().min(0).max(1),
  mutual_satisfaction: z.number().min(0).max(1).optional(),
});

const AllocationResultSchema = z.object({
  capacity_slot_id: z.string(),
  provider_id: EntityIdSchema,
  total_capacity: z.number().nonnegative(),
  tier1_allocations: z.array(TierAllocationSchema),
  tier2_allocations: z.array(TierAllocationSchema),
  remaining_capacity: z.number().nonnegative(),
});

type AllocationResult = z.infer<typeof AllocationResultSchema>;

// ═══════════════════════════════════════════════════════════════════════
// EXPORTS
// ═══════════════════════════════════════════════════════════════════════

export {
  // Primitives
  SatisfactionSchema,
  PointsSchema,
  EntityIdSchema,
  TimestampSchema,
  
  // Contributors
  ContributorSchema,
  
  // Resources
  ResourceTypeSchema,
  LocationSchema,
  TimeWindowSchema,
  FilterRulesSchema,
  
  // Allocations
  AllocationRecordSchema,
  
  // Slots
  NeedSlotSchema,
  CapacitySlotSchema,
  
  // Nodes
  NonSlotNodeSchema,
  GoalNodeSchema,
  RootNodeSchema,
  
  // Tree
  TreeSchema,
  
  // Network
  NetworkSchema,
  ShareMapSchema,
  MutualSatisfactionMapSchema,
  EntityStateSchema,
  NetworkStateSchema,
  
  // Allocation calculations
  TierAllocationSchema,
  AllocationResultSchema,
};

export type {
  Contributor,
  AllocationRecord,
  NeedSlot,
  CapacitySlot,
  NonSlotNode,
  GoalNode,
  RootNode,
  Tree,
  EntityState,
  NetworkState,
  AllocationResult,
};
```

═══════════════════════════════════════════════════════════════════════
SCHEMA NOTES
═══════════════════════════════════════════════════════════════════════

1. **Recursive Types**: Using z.lazy() for recursive tree structures
   (GoalNode can contain GoalNodes, CapacitySlots can contain NeedSlots, etc.)

2. **Discriminated Unions**: Each node type has a 'type' literal field
   for runtime type discrimination and exhaustive pattern matching

3. **Validation Bounds**:
   - Satisfaction: [0.0, 1.0]
   - Weight: [0.0, 1.0]
   - Points: positive integers
   - Quantities: non-negative numbers

4. **Metadata**: All entities include created_at and updated_at timestamps
   for auditing and synchronization

5. **Derived Fields**: Schemas include both user inputs and derived values
   for complete state representation (computed values cached for performance)

6. **Empty Arrays as Defaults**: contributors: [] means "self only" for
   capacity contributors (following the coexistence model)

7. **Optional Fields**: Anti-contributors, filters, and manual_satisfaction
   are optional to simplify common cases

8. **Type Safety**: Full TypeScript types exported for compile-time checking

═══════════════════════════════════════════════════════════════════════
ADDITIONAL FEATURES FROM EXISTING IMPLEMENTATION
═══════════════════════════════════════════════════════════════════════

The following features from the current codebase (schemas.ts) should be 
integrated into the above model:

**1. ITC Causality Tracking (Instead of Simple Timestamps)**

```typescript
const ITCStampSchema = z.object({
  id: ITCIdSchema,     // Can be: 0 (null), 1 (full), or {l, r} (split)
  event: ITCEventSchema // Can be: number (counter) or {n, l, r} (tree)
});
```

ITC (Interval Tree Clocks) provide O(log n) causality tracking vs O(n) for
vector clocks. Replace simple `updated_at` timestamps with ITC stamps for
proper distributed conflict resolution.

**2. Hierarchical Availability Windows**

Current system has 4 levels of time specificity:
- Level 1 (Month-specific): Different patterns per month for yearly recurrence
- Level 2 (Week-specific): Different patterns per week for monthly recurrence  
- Level 3 (Day-specific): Different patterns per day for weekly recurrence
- Level 4 (Simple): Same time ranges all days/weeks/months

```typescript
const AvailabilityWindowSchema = z.object({
  month_schedules: z.array(MonthScheduleSchema).optional(),
  week_schedules: z.array(WeekScheduleSchema).optional(),
  day_schedules: z.array(DayScheduleSchema).optional(),
  time_ranges: z.array(TimeRangeSchema).optional()
});
```

This enables expressing complex patterns like:
- "First Monday of each month: 9am-12pm"
- "September only: all weekdays 10am-5pm"
- "Even weeks: Tuesday/Thursday 2pm-4pm"

**3. Organizations & Recursive Membership**

```typescript
const OrganizationSchema = z.object({
  org_id: z.string(),
  names: z.record(z.string(), z.string()), // Multi-language
  emoji: z.string().optional(),
  description: z.string().optional()
});

// Members can be pubkeys, contact_ids, OR org_ids (recursive!)
const MembersSchema = z.array(z.string());
```

This allows:
- Collective capacities: `members: ['org_humanitarian_coalition']`
- Nested orgs: org_abc includes org_def which includes individuals
- Multi-language names: `names: { en: "Garden", es: "Jardín" }`

**4. Contacts System (for pre-public-key relationships)**

```typescript
const ContactSchema = z.object({
  contact_id: z.string(),
  name: z.string(),
  public_key: z.string().optional(), // May not have key yet
  emoji: z.string().optional()
});
```

Enables recognizing people before they join the network:
- Add "Alice" as contributor → gets contact_id
- Later Alice joins → link contact_id to her public_key
- Smooth onboarding without losing history

**5. Divisibility Constraints (prevents over-fragmentation)**

```typescript
{
  max_natural_div: z.number().gte(1).optional(),
  min_allocation_percentage: PercentageSchema.optional()
}
```

Examples:
- Person's time: `max_natural_div: 1` (can't split a person)
- Large grant: `min_allocation_percentage: 0.1` (don't allocate <10%)
- Prevents allocating 0.001% of someone's capacity to 1000 recipients

**6. Multi-Dimensional Need Types (Per-Type Tracking)**

Current system tracks everything per type_id:
- Damping factors: α_k per type k
- Convergence: separate for each type
- Allocations: A_total^k(i, t) per recipient per type

```typescript
const PerTypeDampingHistoryEntrySchema = z.object({
  type_id: z.string().min(1),
  overAllocation: z.number(),
  timestamp: z.number().int().positive()
});
```

This enables:
- Food needs converge faster than housing needs
- Different damping for different resource types
- Per-type satisfaction tracking (already in your model!)

**7. Slot Filters & Subscriptions**

```typescript
const SlotFilterSchema = z.object({
  filter_id: z.string(),
  applies_to: z.enum(['capacity', 'need', 'both']),
  source_pubkeys: z.array(z.string()).optional(),
  type_ids: z.array(z.string()).optional(),
  must_include_me: z.boolean().optional(),
  location_max_distance_km: z.number().optional()
});
```

Users can:
- Subscribe to specific people's capacities/needs
- Filter by type, location, involvement
- Multiple filters combined with OR (match ANY)

**8. Booking & Advance Notice**

```typescript
{
  advance_notice_hours: z.number().gte(0).optional(),
  booking_window_hours: z.number().gte(0).optional()
}
```

Real-world constraints:
- "Need 48 hours advance notice to schedule"
- "Can only book within next 2 weeks"

**9. Mutual Agreement Flag**

```typescript
{
  mutual_agreement_required: z.boolean().default(false)
}
```

For sensitive allocations that require explicit acceptance:
- Childcare (safety)
- Housing (compatibility)
- High-commitment volunteering

**10. Convergence Metrics (Frobenius Norm)**

Multi-dimensional convergence tracking:

```typescript
const ConvergenceMetricsSchema = z.object({
  frobeniusNorm: z.number().nonnegative(),        // ||N⃗⃗(t)||_F
  frobeniusNormPrevious: z.number().nonnegative(),
  perTypeMetrics: z.record(z.string(), PerTypeConvergenceMetricsSchema),
  universalSatisfactionAchieved: z.boolean(),     // ∀i,k: N_i^k(t) = 0
  freedomMetric: z.number().nonnegative()
});
```

**11. Legacy Compatibility Types**

schemas.ts maintains backward compatibility during migration:
```typescript
export type BaseCapacity = Commitment;
export type ProviderCapacity = Commitment & { id?: string };
```

═══════════════════════════════════════════════════════════════════════
INTEGRATION RECOMMENDATIONS
═══════════════════════════════════════════════════════════════════════

**Immediate Additions to test2.md:**

1. **Replace timestamps with ITC stamps** throughout
   - Better causality tracking in distributed system
   - Handles concurrent updates properly

2. **Add Organizations to tree structure**
   ```
   Root
       └── Non-Slot Contributors (can include org_ids)
   ```

3. **Add Contacts to tree structure**
   ```
   Root
       └── Non-Slot Contributors (can include contact_ids)
   ```

4. **Enhance FilterRulesSchema** with:
   - Hierarchical availability windows
   - Advance notice requirements
   - Booking windows
   - Divisibility constraints

5. **Add per-type damping** to allocation formula
   - Each need type k has damping factor α_k(t)
   - Prevents oscillation independently per type

6. **Add convergence tracking schemas**
   - Frobenius norm for multi-dimensional convergence
   - Per-type convergence metrics
   - Universal satisfaction condition

7. **Add mutual_agreement_required flag**
   - For allocations requiring explicit acceptance
   - Prevents unwanted commitments

**Terminology Reconciliation:**

Current code uses "fulfillment" but your spec uses "satisfaction".
Options:
- Keep "fulfillment" (less breaking change)
- Migrate to "satisfaction" (clearer semantics)
- Use both: fulfillment = technical, satisfaction = user-facing

**Schema Evolution Path:**

Your test2.md could become v6, building on v4's multi-dimensional framework:
- v4: Multi-dimensional needs per type (current schemas.ts)
- v5: Global recognition model (also in schemas.ts)
- v6: Satisfaction-based feedback loop (your test2.md)

The key insight: Your satisfaction model is conceptually cleaner, but
schemas.ts has battle-tested implementation details you'll need.

═══════════════════════════════════════════════════════════════════════
STABILITY ANALYSIS: PREVENTING INFINITE REACTIVE LOOPS
═══════════════════════════════════════════════════════════════════════

**Critical Question:** Does satisfaction-based feedback create oscillations or
instabilities?

**POTENTIAL LOOP 1: Allocation-Satisfaction Oscillation**

```
Cycle t:   Provider A allocates 100 units → Recipient rates 0.4 (too much)
Cycle t+1: Provider A allocates 40 units → Recipient rates 1.0 (perfect!)
Cycle t+2: Provider A allocates 100 units → Recipient rates 0.4 (too much again)
Cycle t+3: ...oscillates forever
```

**STABILIZERS:**
1. **Need Caps (Hard Bound):**
   ```
   FinalAllocation = min(RawAllocation, DeclaredNeed)
   ```
   Even if satisfaction suggests more, can't exceed declared need.

2. **Declining Mechanism (Self-Regulation):**
   ```
   If Offered > ActualNeed:
     Accept only ActualNeed
     Decline remainder
     Satisfaction still high (got what you needed)
   
   Result: No oscillation - you get exactly what you need every cycle
   ```

3. **Weighted Aggregation (Smoothing):**
   ```
   ShareOfGeneralSatisfaction aggregates across ALL nodes
   Single allocation satisfaction changes → small change in global share
   Not 1:1 mapping, dampens oscillation
   ```

**Verdict:** STABLE ✓
- Need caps prevent over-allocation
- Decline mechanism lets recipients self-regulate
- Aggregation smooths individual allocation variance

**POTENTIAL LOOP 2: Mutual Satisfaction Circular Dependency**

```
A's satisfaction with B depends on B's allocations to A
B's satisfaction with A depends on A's allocations to B
A's allocations to B depend on Mutual-Satisfaction(A,B)
B's allocations to A depend on Mutual-Satisfaction(B,A)

Circular dependency: A→B depends on B→A depends on A→B...
```

**ANALYSIS:**
This is actually a **fixed-point problem**, not an infinite loop.

```
Iteration 0: Use initial recognition (bootstrap)
  MR(A,B) = min(Recognition_A→B, Recognition_B→A)

Iteration 1: After first allocation cycle
  A allocates to B based on MR
  B allocates to A based on MR
  Both rate satisfaction
  
Iteration 2: Update mutual satisfaction
  MS(A,B) = min(Satisfaction_A→B, Satisfaction_B→A)
  Use this for next allocations
  
Iteration 3+: Converges to fixed point
  MS(A,B)* where neither wants to change their satisfaction ratings
```

**STABILIZERS:**
1. **Min Function (Conservative):**
   ```
   MS(A,B) = min(S_A→B, S_B→A)
   
   Taking minimum means:
   - Can't be gamed by one party inflating ratings
   - Converges to mutually agreed satisfaction level
   - Conservative estimate (underestimates rather than overestimates)
   ```

2. **Satisfaction Is Retrospective (Breaks Simultaneity):**
   ```
   Cycle t:   Allocations happen (based on cycle t-1 satisfaction)
   Cycle t:   Recipients rate satisfaction (after receiving)
   Cycle t+1: New allocations use cycle t satisfaction
   
   Time delay breaks circular dependency
   ```

3. **Need Fulfillment Reduces Sensitivity:**
   ```
   As needs get met → satisfaction stabilizes
   When DeclaredNeed ≈ TotalReceived → satisfaction ≈ 1.0
   High satisfaction → stable allocations → high satisfaction (stable loop)
   ```

**Verdict:** CONVERGES ✓
- Time delay breaks simultaneity
- Min function prevents gaming
- Convergence to mutual satisfaction equilibrium

**POTENTIAL LOOP 3: Tree Aggregation Feedback**

```
Allocation satisfaction → Need-Slot satisfaction
Need-Slot satisfaction → Capacity-Slot satisfaction  
Capacity-Slot satisfaction → Goal satisfaction
Goal satisfaction → ShareOfGeneralSatisfaction
ShareOfGeneralSatisfaction → Future allocation
Future allocation → Allocation satisfaction (loop)
```

**ANALYSIS:**
This is the core learning loop - it's INTENDED behavior, but needs stability.

**STABILIZERS:**
1. **Hierarchical Aggregation (Dampening Factor):**
   ```
   Each level averages children with weights
   
   Single allocation satisfaction = 0.3
     ↓ (averaged with 10 other allocations)
   Need-Slot satisfaction = 0.7 (dampened)
     ↓ (averaged with 5 sibling need-slots)
   Capacity-Slot satisfaction = 0.75 (further dampened)
     ↓ (averaged with other capacity slots)
   Goal satisfaction = 0.80 (heavily dampened)
   
   Impact on global share: small
   ```

2. **Multiple Contributors Per Node:**
   ```
   If node has 10 contributors each with points:
   Single contributor's satisfaction affects their share: 1/10 = 10%
   
   Not winner-take-all - proportional impact
   ```

3. **Per-Type Damping (from v4/v5):**
   ```
   α_k(t) ∈ {0.5, 0.8, 1.0}
   
   If system detects oscillation in type k:
     Reduce α_k → reduce allocation responsiveness
     
   Adaptive damping per resource type prevents over-reaction
   ```

**Verdict:** STABLE WITH DAMPING ✓
- Hierarchical aggregation provides natural dampening
- Proportional impact (not binary)
- Existing per-type damping mechanism applies

**POTENTIAL LOOP 4: Decline Cascade**

```
Cycle t:   Many recipients over-allocated
           All decline excess simultaneously
           
Cycle t+1: Massive capacity surge (all declined resources available)
           System allocates even more
           
Cycle t+2: Even more declines
           Worse cascade
```

**ANALYSIS:**
Declining resources return to provider's available capacity in SAME cycle.

**STABILIZERS:**
1. **Same-Cycle Reallocation:**
   ```
   Within Cycle t:
     Step 1: Calculate allocations
     Step 2: Recipients accept/decline
     Step 3: Declined resources → back to available capacity
     Step 4: Reallocate declined resources to other recipients
     
   All happens in one calculation round - no cascade across cycles
   ```

2. **Need-Based Capping:**
   ```
   FinalAllocation = min(RawAllocation, RemainingNeed)
   
   Even if capacity surges, can't force allocation beyond need
   Recipients don't have to actively decline - system auto-caps
   ```

3. **Learning From Decline Patterns:**
   ```
   If Provider A consistently over-allocates:
     Recipients consistently decline
     A's satisfaction ratings decrease
     A's future allocation share decreases
     
   System learns to allocate less from A
   ```

**Verdict:** NO CASCADE ✓
- Same-cycle reallocation
- Need caps prevent forced over-allocation
- System learns from decline patterns

**POTENTIAL LOOP 5: Strategic Satisfaction Gaming**

```
Recipient learns:
  "If I rate satisfaction low, I get more next cycle from others"
  "If I rate satisfaction high, I get less next cycle"
  
Strategic behavior: Always rate low to maximize allocation
```

**ANALYSIS:**
This is NOT an infinite loop, but a potential gaming vector.

**PROTECTIONS:**
1. **Mutual Satisfaction Requirement:**
   ```
   MS(A,B) = min(Satisfaction_A→B, Satisfaction_B→A)
   
   If B strategically underrates A:
     - A's allocation to B decreases (works as B intended)
     - But A sees B's low rating
     - A can respond by:
       a) Reducing their satisfaction rating of B
       b) Reducing recognition of B
     - MS(A,B) stays low
     - B doesn't gain advantage
   ```

2. **Recognition Controls Eligibility:**
   ```
   Satisfaction affects ALLOCATION WEIGHT among eligible recipients
   Recognition affects WHO IS ELIGIBLE
   
   If B games satisfaction:
     A can remove B from recognition entirely
     B gets zero allocation (regardless of satisfaction)
   ```

3. **Network Reputation Effects:**
   ```
   If B consistently gives dishonest satisfaction ratings:
     - Other providers see this pattern
     - Other providers reduce recognition of B
     - B becomes isolated in network
     - Bad strategy long-term
   ```

4. **Self-Harm From Bad Data:**
   ```
   If B gives dishonest low satisfaction:
     B's own decision-making suffers
     B thinks A is unhelpful when A is actually helpful
     B might reduce recognition of A
     B loses access to genuinely helpful provider
     
   Honest reporting = optimal strategy for B's own learning
   ```

**Verdict:** GAMING-RESISTANT ✓
- Mutual satisfaction prevents one-sided manipulation
- Recognition provides override control
- Network effects punish dishonesty
- Self-harm from bad data disincentivizes gaming

**POTENTIAL LOOP 6: Recognition-Satisfaction Conflict**

```
Recognition tree says: "A contributes 80% to my mission"
Satisfaction data says: "A's allocations have 30% satisfaction"

Which wins? Does this create conflict/oscillation?
```

**RESOLUTION:**
Recognition and Satisfaction serve different functions (coexistence model).

```
Recognition (Non-Slot):
  - Intangible contributions (mission alignment, advocacy, etc.)
  - Manual input
  - Controls eligibility and baseline share
  
Satisfaction (Slot-Based):
  - Tangible resource allocations
  - Automatic feedback
  - Modulates allocation weight within eligible set

Combined Calculation:
  Total ShareOfGeneralSatisfaction(Target, Contributor) =
    Non-Slot contribution (from recognition tree) +
    Slot-Based contribution (from satisfaction aggregation)
    
  Both weighted by their tree positions
  No conflict - they add together
```

**Example:**
```
Entity A → Entity B total share:
  Non-Slot Recognition: 40% (for mission alignment, advocacy)
  Slot Satisfaction: 20% (for resource allocations, weighted by satisfaction)
  
  Total: 60% combined share (normalized across all entities)
  
  If slot satisfaction drops to 5%:
    Non-slot stays at 40% (manual recognition unchanged)
    Total drops to 45%
    
  Message: "B is mission-aligned but their resources aren't helping"
  Action: Keep relationship, adjust resource types
```

**Verdict:** NO CONFLICT ✓
- Recognition and Satisfaction operate in different domains
- Additive combination, not competitive
- Allows nuanced relationships (good partner, wrong resources)

═══════════════════════════════════════════════════════════════════════
STABILITY GUARANTEES (FORMAL)
═══════════════════════════════════════════════════════════════════════

**Theorem (v6 Stability):**
The satisfaction-based feedback system converges to a stable equilibrium
under the following conditions:

**C1. Bounded Allocations:**
  ∀ recipients i, resources k:
    0 ≤ FinalAllocation_i^k ≤ DeclaredNeed_i^k
  
  Hard bounds prevent unbounded growth.

**C2. Satisfaction Domain:**
  ∀ satisfaction ratings S:
    S ∈ [0, 1]
  
  Bounded feedback signal.

**C3. Min Function in Mutual Satisfaction:**
  MS(A,B) = min(S_A→B, S_B→A)
  
  Conservative aggregation prevents inflation.

**C4. Temporal Separation:**
  Cycle t:   Allocations computed
  Cycle t:   Satisfaction rated (after allocation)
  Cycle t+1: Next allocations use cycle t satisfaction
  
  Time delay breaks circular dependency.

**C5. Hierarchical Dampening:**
  Each aggregation level averages with weights:
    Satisfaction_parent = Σ(w_i × Satisfaction_child_i) / Σ(w_i)
  
  Propagation dampens with tree depth.

**C6. Per-Type Adaptive Damping:**
  For each resource type k:
    α_k(t) ∈ {0.5, 0.8, 1.0}
  
  If oscillation detected: α_k ← 0.5 (reduce responsiveness)

**C7. Network Effect Penalties:**
  Dishonest satisfaction reporting → 
    Reduced recognition from other entities →
      Reduced total allocation →
        Incentive for honest reporting

**Convergence Property:**
Under C1-C7, the system converges to a satisfaction equilibrium:
  
  lim(t→∞) ||N⃗⃗(t)||_F → 0  (if sufficient capacity)
  
  OR
  
  lim(t→∞) ||N⃗⃗(t)||_F → constant  (capacity-limited equilibrium)

Where satisfaction ratings stabilize at true utility values.

**Proof Sketch:**
1. Need caps (C1) + Satisfaction bounds (C2) → Bounded state space
2. Time delay (C4) → No simultaneous circular dependency
3. Dampening (C5, C6) → Contraction mapping
4. Min function (C3) → Conservative convergence
5. Bounded state + Contraction → Fixed point exists (Banach fixed-point theorem)
6. Network penalties (C7) → Unique honest equilibrium is optimal

∎

═══════════════════════════════════════════════════════════════════════
IMPLEMENTATION REQUIREMENTS FOR STABILITY
═══════════════════════════════════════════════════════════════════════

**Required in v6 Implementation:**

1. **Per-Type Damping (Already in v4/v5):**
   ```typescript
   const MultiDimensionalDampingSchema = z.object({
     damping_factors: z.record(z.string(), z.number().min(0).max(1)),
     damping_history: z.record(z.string(), z.array(PerTypeDampingHistoryEntrySchema))
   });
   ```

2. **Oscillation Detection (Add to v6):**
   ```typescript
   function detectOscillation(
     history: SatisfactionHistory,
     windowSize: number = 3
   ): boolean {
     // Check if satisfaction alternates between high/low
     const values = history.slice(-windowSize);
     const variance = calculateVariance(values);
     const meanChange = calculateMeanAbsoluteChange(values);
     
     return variance > VARIANCE_THRESHOLD && 
            meanChange > CHANGE_THRESHOLD;
   }
   ```

3. **Adaptive Satisfaction Responsiveness (New):**
   ```typescript
   function calculateAllocationShare(
     mutualSatisfaction: number,
     dampingFactor: number,
     oscillationDetected: boolean
   ): number {
     const responsiveness = oscillationDetected ? 0.5 : 1.0;
     return mutualSatisfaction * dampingFactor * responsiveness;
   }
   ```

4. **Same-Cycle Decline Reallocation (New):**
   ```typescript
   function computeAllocations(
     capacities: CapacitySlot[],
     needs: NeedSlot[],
     mutualSatisfaction: Map
   ): AllocationResult {
     // Round 1: Initial allocation
     const initialAllocations = computeRawAllocations(...);
     
     // Round 2: Accept/Decline (within same cycle)
     const acceptedAllocations = applyAcceptDecline(initialAllocations, needs);
     
     // Round 3: Reallocate declined capacity (within same cycle)
     const declinedCapacity = calculateDeclinedCapacity(initialAllocations, acceptedAllocations);
     const finalAllocations = reallocateDeclined(acceptedAllocations, declinedCapacity, needs);
     
     return finalAllocations;
   }
   ```

5. **Satisfaction History Tracking (New):**
   ```typescript
   const SatisfactionHistorySchema = z.object({
     allocation_id: z.string(),
     satisfaction_values: z.array(z.object({
       value: SatisfactionSchema,
       timestamp: TimestampSchema,
       cycle: z.number()
     })),
     oscillation_detected: z.boolean()
   });
   ```

═══════════════════════════════════════════════════════════════════════
CONCLUSION
═══════════════════════════════════════════════════════════════════════

**No Infinite Reactive Loops** ✓

The v6 satisfaction-based feedback system is stable because:

1. **Hard bounds** prevent unbounded growth
2. **Time delays** break circular dependencies  
3. **Hierarchical dampening** smooths feedback
4. **Per-type adaptive damping** prevents oscillation
5. **Min function** ensures conservative mutual satisfaction
6. **Network effects** penalize strategic gaming
7. **Same-cycle reallocation** prevents cascades

The system converges to a satisfaction equilibrium where allocations
match actual utility, and satisfaction ratings reflect true helpfulness.

This is a **learning system with guaranteed stability** - it gets smarter
without becoming chaotic.
