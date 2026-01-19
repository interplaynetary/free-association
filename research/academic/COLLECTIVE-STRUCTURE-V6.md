═══════════════════════════════════════════════════════════════════════
COLLECTIVE TREE ARCHITECTURE: GOVERNANCE LAYER FOR V6 PROTOCOL
═══════════════════════════════════════════════════════════════════════

**Purpose:**
This document describes the collective tree implementation that sits atop the
individual tree architecture from structure.md. While individual trees represent
personal resource management, collective trees enable group governance and
collective decision-making.

**Key Distinction:**
- Individual Trees (structure.md): Source of truth, persistent, personal
- Collective Trees (this doc): Derived, ephemeral, collective

═══════════════════════════════════════════════════════════════════════
ARCHITECTURE OVERVIEW
═══════════════════════════════════════════════════════════════════════

```
┌─────────────────────────────────────────────────────────────────────┐
│                     COLLECTIVE TREE LIFECYCLE                        │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  1. INPUT LAYER (Individual Trees)                                   │
│     ┌──────────┐  ┌──────────┐  ┌──────────┐                       │
│     │  Alice   │  │   Bob    │  │  Carol   │                       │
│     │   Tree   │  │   Tree   │  │   Tree   │                       │
│     └──────────┘  └──────────┘  └──────────┘                       │
│          │              │              │                             │
│          └──────────────┼──────────────┘                             │
│                         │                                            │
│  2. MERGING LAYER (Hash-Based Node Matching)                        │
│                         ↓                                            │
│               ┌────────────────────┐                                 │
│               │  Collective Tree   │                                 │
│               │  (Merged Nodes)    │                                 │
│               └────────────────────┘                                 │
│                         │                                            │
│  3. METRICS LAYER (Recognition Calculation)                         │
│                         ↓                                            │
│               ┌────────────────────┐                                 │
│               │ Node Recognition   │                                 │
│               │     Metrics        │                                 │
│               └────────────────────┘                                 │
│                         │                                            │
│  4. GOVERNANCE LAYER (Filtering)                                    │
│                         ↓                                            │
│               ┌────────────────────┐                                 │
│               │   Filtered Tree    │                                 │
│               │ (Governance Rules) │                                 │
│               └────────────────────┘                                 │
│                         │                                            │
│  5. DISTRIBUTION LAYER (Share Calculation)                          │
│                         ↓                                            │
│               ┌────────────────────┐                                 │
│               │   Distribution     │                                 │
│               │ {Alice: 40%, ...}  │                                 │
│               └────────────────────┘                                 │
│                         │                                            │
│  6. ALLOCATION LAYER (Resource Distribution)                        │
│                         ↓                                            │
│               ┌────────────────────┐                                 │
│               │   Allocations      │                                 │
│               │ [{to: Alice, ...}] │                                 │
│               └────────────────────┘                                 │
│                                                                       │
└─────────────────────────────────────────────────────────────────────┘
```

**Architecture Principles:**

1. **Ephemeral Computation**: Collective trees are computed on-demand, not stored
2. **Source of Truth**: Individual trees remain the only persistent state
3. **Composable Pipeline**: Each layer builds on previous, pure functions
4. **Governance Flexibility**: Multiple filtering/distribution strategies
5. **Backward Compatible**: Works with existing v6 individual tree protocol

═══════════════════════════════════════════════════════════════════════
CORE DATA STRUCTURES
═══════════════════════════════════════════════════════════════════════

**CollectiveNode (Merged Node with Contributor Tracking)**

```typescript
interface CollectiveNode {
  // Node identity (hash-based)
  id: string;  // hash(name + type + path)
  name: string;
  type: 'Root' | 'Goal' | 'CapacitySlot' | 'NeedSlot' | 'NonSlot';
  
  // Contributor tracking (NEW for collective)
  contributor_data: Record<string, {
    weight: number;           // This contributor's influence (0-1)
    node_ref: string;         // Reference to original node in contributor's tree
    points?: number;          // Contributor's points for this node
    manual_satisfaction?: number;
  }>;
  
  // Merged properties (weighted by contributor shares)
  merged_weight: number;      // Consensus weight for this node
  merged_points?: number;     // Weighted average points
  
  // Tree structure
  children: CollectiveNode[];
  parent_id: string | null;
  
  // Metadata
  depth: number;
  path_from_root: string[];
}
```

**NodeRecognitionMetrics (Quantifies Node Importance)**

```typescript
interface NodeRecognitionMetrics {
  node_id: string;
  node_name: string;
  
  // Recognition measures
  total_recognition: number;            // Σ(contributor_weight × points)
  average_mrd: number;                  // Multi-Recognition Depth (consensus)
  
  // Contributor breakdown
  contributor_weights: Record<string, number>;  // Normalized shares per contributor
  contributor_ids: string[];            // Who recognizes this node
  
  // Tree position
  path_from_root: string[];
  depth: number;
}
```

**Key Insight: Multi-Recognition Depth (MRD)**

MRD measures consensus, not just recognition strength:
- MRD = (# contributors who recognize node) / (total contributors)
- MRD = 1.0: Universal agreement (everyone recognizes)
- MRD = 0.5: Half recognize, half don't
- MRD = 0.1: Only one contributor recognizes (low consensus)

This prevents single contributors from dominating collective decisions.

═══════════════════════════════════════════════════════════════════════
PHASE 1: TREE MERGING
═══════════════════════════════════════════════════════════════════════

**Input**: Multiple individual trees + recognition shares

**Algorithm: mergeContributorTrees()**

```typescript
function mergeContributorTrees(config: {
  contributorTrees: Record<string, Node>;      // Individual trees
  recognitionShares?: Record<string, number>;  // Optional weights
}): CollectiveTree {
  
  // Step 1: Calculate contributor weights
  const weights = calculateContributorWeights(
    contributorIds,
    contributorTrees,
    recognitionShares  // If provided, use; else equal weights
  );
  
  // Step 2: Create merge data structure (accumulates nodes by hash)
  const mergeMap: Map<string, NodeMergeData> = new Map();
  
  // Step 3: Traverse all contributor trees
  for (const [contributorId, tree] of contributorTrees) {
    traverseTree(tree, (node, path) => {
      // Generate hash for this node
      const hash = hashNode(node.name, node.type, path);
      
      // Accumulate in merge map
      if (!mergeMap.has(hash)) {
        mergeMap.set(hash, {
          id: hash,
          name: node.name,
          type: node.type,
          contributors: new Map(),
          children: new Map()
        });
      }
      
      const mergeData = mergeMap.get(hash)!;
      mergeData.contributors.set(contributorId, {
        weight: weights[contributorId],
        node_ref: node.id,
        points: node.points,
        manual_satisfaction: node.manual_satisfaction
      });
    });
  }
  
  // Step 4: Convert merge map to collective tree
  const collectiveTree = buildCollectiveTreeFromMergeData(mergeMap, weights);
  
  return collectiveTree;
}
```

**Hash-Based Node Matching**

Nodes from different contributor trees are considered "the same" if:
- Same name (case-sensitive)
- Same type (Goal, CapacitySlot, etc.)
- Same position in tree (path from root)

```typescript
function hashNode(name: string, type: string, path: string[]): string {
  const pathStr = path.join('/');
  const combined = `${name}|${type}|${pathStr}`;
  return hash(combined);  // Cryptographic hash (e.g., SHA-256)
}
```

**Example:**

```
Alice's Tree:
  Root
    └─ "Food Security" (Goal)
        └─ "Emergency Food" (CapacitySlot)

Bob's Tree:
  Root
    └─ "Food Security" (Goal)
        └─ "Food Distribution" (CapacitySlot)

Carol's Tree:
  Root
    └─ "Food Security" (Goal)
        └─ "Emergency Food" (CapacitySlot)

Merged Collective Tree:
  Root (3 contributors)
    └─ "Food Security" (Goal, 3 contributors: Alice, Bob, Carol)
        ├─ "Emergency Food" (CapacitySlot, 2 contributors: Alice, Carol)
        └─ "Food Distribution" (CapacitySlot, 1 contributor: Bob)
```

**Weighted Property Merging**

When multiple contributors have the same node, properties are merged:

```typescript
function mergeNodeProperties(
  contributors: Map<string, ContributorData>
): MergedProperties {
  let totalWeight = 0;
  let weightedSum = 0;
  
  for (const [contributorId, data] of contributors) {
    totalWeight += data.weight;
    if (data.points) {
      weightedSum += data.weight * data.points;
    }
  }
  
  return {
    merged_points: weightedSum / totalWeight,
    merged_weight: totalWeight,
    contributor_count: contributors.size
  };
}
```

**Example:**
```
Contributors to "Food Security" node:
  Alice (weight: 0.5, points: 80)
  Bob (weight: 0.3, points: 60)
  Carol (weight: 0.2, points: 70)

Merged properties:
  merged_points = (0.5×80 + 0.3×60 + 0.2×70) / 1.0
                = (40 + 18 + 14) / 1.0
                = 72 points
  
  merged_weight = 0.5 + 0.3 + 0.2 = 1.0 (full collective agreement)
```

═══════════════════════════════════════════════════════════════════════
PHASE 2: NODE RECOGNITION METRICS
═══════════════════════════════════════════════════════════════════════

**Purpose**: Quantify importance and consensus for each node

**Calculation: For Each Node in Collective Tree**

```typescript
function calculateNodeRecognition(
  node: CollectiveNode,
  totalContributors: number
): NodeRecognitionMetrics {
  
  // 1. Total Recognition: Weighted sum
  let totalRecognition = 0;
  for (const [contributorId, data] of node.contributor_data) {
    totalRecognition += data.weight * (data.points || 0);
  }
  
  // 2. Average MRD: Consensus measure
  const contributorCount = node.contributor_data.size;
  const averageMRD = contributorCount / totalContributors;
  
  // 3. Contributor Weights: Normalized per-contributor influence
  const contributorWeights: Record<string, number> = {};
  for (const [contributorId, data] of node.contributor_data) {
    const contributorRecognition = data.weight * (data.points || 0);
    contributorWeights[contributorId] = contributorRecognition / totalRecognition;
  }
  
  return {
    node_id: node.id,
    node_name: node.name,
    total_recognition,
    average_mrd: averageMRD,
    contributor_weights,
    contributor_ids: Array.from(node.contributor_data.keys()),
    path_from_root: node.path_from_root,
    depth: node.depth
  };
}
```

**Interpretation:**

**High Recognition, High MRD**: Strong collective priority
- total_recognition: 150
- average_mrd: 0.9 (90% of contributors recognize)
- **Action**: Definitely include in governance decisions

**High Recognition, Low MRD**: Single contributor's priority
- total_recognition: 150
- average_mrd: 0.1 (only 10% recognize, but weighted heavily)
- **Action**: May want to filter out (not collective priority)

**Low Recognition, High MRD**: Weak collective agreement
- total_recognition: 20
- average_mrd: 0.8 (80% recognize, but low points)
- **Action**: Include if MRD important, exclude if threshold-based

**Low Recognition, Low MRD**: Not a priority
- total_recognition: 10
- average_mrd: 0.2
- **Action**: Likely filter out

═══════════════════════════════════════════════════════════════════════
PHASE 3: GOVERNANCE FILTERING
═══════════════════════════════════════════════════════════════════════

**Purpose**: Remove nodes that don't meet governance criteria

**Filter Types:**

**1. Recognition-Based Filters**
```typescript
{
  min_total_recognition: number;    // Minimum weighted recognition
  min_average_mrd: number;          // Minimum consensus (0-1)
  min_contributor_count: number;    // Minimum # of recognizers
  min_percentage: number;           // Minimum % of parent's weight
}
```

**2. JSON Logic Filters (Complex Conditions)**
```typescript
{
  logic_rule: {
    "or": [
      {">=": [{"var": "total_recognition"}, 100]},
      {"and": [
        {">=": [{"var": "average_mrd"}, 0.8]},
        {">=": [{"var": "contributor_count"}, 4]}
      ]}
    ]
  }
}
```

**3. Custom Function Filters**
```typescript
{
  custom_filter: (node: CollectiveNode, recognition: NodeRecognitionMetrics) => {
    // Arbitrary logic
    return recognition.total_recognition > 50 && 
           node.name.includes("Emergency");
  }
}
```

**Filtering Algorithm: applyUnifiedFilter()**

```typescript
function applyUnifiedFilter(
  collectiveTree: CollectiveTree,
  nodeRecognition: Map<string, NodeRecognitionMetrics>,
  config: UnifiedFilterConfig
): FilteredTreeResult {
  
  // Step 1: Evaluate each node against criteria
  function shouldKeepNode(node: CollectiveNode): boolean {
    const recognition = nodeRecognition.get(node.id);
    if (!recognition) return false;
    
    // Apply all configured filters
    const checks = [];
    
    if (config.min_total_recognition !== undefined) {
      checks.push(recognition.total_recognition >= config.min_total_recognition);
    }
    
    if (config.min_average_mrd !== undefined) {
      checks.push(recognition.average_mrd >= config.min_average_mrd);
    }
    
    if (config.min_contributor_count !== undefined) {
      checks.push(recognition.contributor_ids.length >= config.min_contributor_count);
    }
    
    if (config.logic_rule) {
      const logicResult = jsonLogic.apply(config.logic_rule, {
        total_recognition: recognition.total_recognition,
        average_mrd: recognition.average_mrd,
        contributor_count: recognition.contributor_ids.length,
        depth: recognition.depth
      });
      checks.push(logicResult);
    }
    
    if (config.custom_filter) {
      checks.push(config.custom_filter(node, recognition));
    }
    
    // ALL checks must pass
    return checks.every(c => c);
  }
  
  // Step 2: Filter tree recursively
  function filterNodeRecursive(
    node: CollectiveNode
  ): CollectiveNode | null {
    
    // Filter children first (bottom-up)
    const filteredChildren = node.children
      .map(filterNodeRecursive)
      .filter(c => c !== null);
    
    // Check if this node should be kept
    const keepNode = shouldKeepNode(node);
    
    // Path preservation logic
    if (config.preserve_paths) {
      // Keep if: node passes OR has any kept children
      if (keepNode || filteredChildren.length > 0) {
        return {
          ...node,
          children: filteredChildren
        };
      }
    } else {
      // Keep only if node itself passes
      if (keepNode) {
        return {
          ...node,
          children: filteredChildren
        };
      }
    }
    
    return null;  // Node filtered out
  }
  
  const filteredTree = filterNodeRecursive(collectiveTree.root);
  
  // Step 3: Renormalize weights (siblings must sum to 1.0)
  const renormalizedTree = renormalizeCollectiveTree(filteredTree);
  
  return {
    filtered_tree: renormalizedTree,
    original_node_count: countNodes(collectiveTree),
    filtered_node_count: countNodes(renormalizedTree),
    removed_nodes: /* track what was removed */
  };
}
```

**Path Preservation Example:**

Without path preservation (preserve_paths: false):
```
Original:
  Root
    ├─ A (recognition: 100) ✓
    │   └─ A1 (recognition: 20) ❌
    │       └─ A1a (recognition: 80) ✓ but orphaned!
    └─ B (recognition: 30) ❌

Filter: min_total_recognition = 50

Result:
  Root
    └─ A (only this kept, A1a orphaned and removed)
```

With path preservation (preserve_paths: true):
```
Result:
  Root
    └─ A
        └─ A1 (kept as path to A1a!)
            └─ A1a
```

═══════════════════════════════════════════════════════════════════════
PHASE 4: DISTRIBUTION CALCULATION
═══════════════════════════════════════════════════════════════════════

**Purpose**: Convert node recognition into contributor shares

**Distribution Modes:**

**1. Single-Node Distribution**
Focus on one specific node's contributor weights:

```typescript
function singleNodeDistribution(
  nodeRecognition: Map<string, NodeRecognitionMetrics>,
  targetNodeId: string
): Record<string, number> {
  const recognition = nodeRecognition.get(targetNodeId);
  if (!recognition) return {};
  
  // Return normalized contributor weights for this node
  return recognition.contributor_weights;
}
```

Example:
```
Node: "Emergency Food Fund"
Recognition:
  Alice: 45% (recognition: 90, weight: 0.5)
  Bob: 30% (recognition: 60, weight: 0.5)
  Carol: 25% (recognition: 50, weight: 0.5)

Distribution = { Alice: 0.45, Bob: 0.30, Carol: 0.25 }
```

**2. Tree-Wide Distribution**
Aggregate across entire tree:

```typescript
function treeWideDistribution(
  nodeRecognition: Map<string, NodeRecognitionMetrics>
): Record<string, number> {
  const totals: Record<string, number> = {};
  let grandTotal = 0;
  
  // Sum recognition across all nodes
  for (const recognition of nodeRecognition.values()) {
    for (const [contributorId, weight] of Object.entries(recognition.contributor_weights)) {
      const contribution = weight * recognition.total_recognition;
      totals[contributorId] = (totals[contributorId] || 0) + contribution;
      grandTotal += contribution;
    }
  }
  
  // Normalize
  for (const contributorId in totals) {
    totals[contributorId] /= grandTotal;
  }
  
  return totals;
}
```

Example:
```
Node A (recognition: 100): Alice: 60%, Bob: 40%
Node B (recognition: 50):  Alice: 80%, Carol: 20%
Node C (recognition: 75):  Bob: 100%

Contributions:
  Alice: 0.6×100 + 0.8×50 = 60 + 40 = 100
  Bob: 0.4×100 + 1.0×75 = 40 + 75 = 115
  Carol: 0.2×50 = 10
  Total: 225

Distribution = {
  Alice: 100/225 = 44.4%,
  Bob: 115/225 = 51.1%,
  Carol: 10/225 = 4.4%
}
```

**3. Weighted-Path Distribution**
Weight nodes by their position in tree:

```typescript
function weightedPathDistribution(
  nodeRecognition: Map<string, NodeRecognitionMetrics>,
  pathWeightFn: (depth: number) => number = (d) => 1.0 / (d + 1)
): Record<string, number> {
  const totals: Record<string, number> = {};
  let grandTotal = 0;
  
  for (const recognition of nodeRecognition.values()) {
    // Weight this node by its depth
    const pathWeight = pathWeightFn(recognition.depth);
    
    for (const [contributorId, weight] of Object.entries(recognition.contributor_weights)) {
      const contribution = weight * recognition.total_recognition * pathWeight;
      totals[contributorId] = (totals[contributorId] || 0) + contribution;
      grandTotal += contribution;
    }
  }
  
  // Normalize
  for (const contributorId in totals) {
    totals[contributorId] /= grandTotal;
  }
  
  return totals;
}
```

Path weighting examples:
- Depth 0 (root): weight = 1.0 / (0+1) = 1.0
- Depth 1: weight = 1.0 / (1+1) = 0.5
- Depth 2: weight = 1.0 / (2+1) = 0.33

Deeper nodes have less influence on distribution.

═══════════════════════════════════════════════════════════════════════
PHASE 5: CAPACITY ALLOCATION
═══════════════════════════════════════════════════════════════════════

**Purpose**: Distribute actual resources according to distribution shares

**Input:**
- Distribution shares (from phase 4)
- Capacity slots (available resources)
- Member commitments (who provides what)
- Compliance filters (matching rules)

**Algorithm: allocateFromCollectiveTree()**

```typescript
function allocateFromCollectiveTree(
  collectiveTree: CollectiveTree,
  nodeRecognition: Map<string, NodeRecognitionMetrics>,
  capacitySlots: AvailabilitySlot[],
  memberCommitments: Record<string, Commitment>,
  options: {
    providerPubKey?: string;
    targetNodeId?: string;
    aggregationMode?: 'single-node' | 'tree-wide' | 'weighted-path';
  }
): AllocationResult {
  
  // Step 1: Calculate distribution shares
  const distribution = nodeRecognitionToDistribution(
    nodeRecognition,
    {
      targetNodeId: options.targetNodeId,
      aggregationMode: options.aggregationMode
    }
  );
  
  // Step 2: Filter capacity slots by compliance
  const eligibleSlots = capacitySlots.filter(slot => {
    // Match need type, availability, location, etc.
    return meetsComplianceRules(slot, options);
  });
  
  // Step 3: Allocate by distribution shares
  const allocations: Allocation[] = [];
  
  for (const slot of eligibleSlots) {
    const totalCapacity = slot.quantity;
    
    for (const [recipientId, share] of Object.entries(distribution.shares)) {
      const rawAllocation = totalCapacity * share;
      
      // Check against recipient's declared need
      const recipientNeed = findRecipientNeed(recipientId, slot.type_id);
      const cappedAllocation = Math.min(rawAllocation, recipientNeed || Infinity);
      
      allocations.push({
        provider_id: options.providerPubKey!,
        recipient_id: recipientId,
        capacity_slot_id: slot.id,
        quantity: cappedAllocation,
        distribution_share: share,
        governance_compliant: true
      });
    }
  }
  
  // Step 4: Handle declined capacity (reallocate)
  const declinedCapacity = calculateDeclinedCapacity(allocations, memberCommitments);
  const finalAllocations = reallocateDeclined(allocations, declinedCapacity);
  
  return {
    allocations: finalAllocations,
    total_allocated: sumAllocations(finalAllocations),
    total_capacity: sumCapacity(eligibleSlots),
    utilization_rate: totalAllocated / totalCapacity,
    distribution_used: distribution
  };
}
```

**Example:**

```
Distribution (from governance):
  Alice: 50%
  Bob: 30%
  Carol: 20%

Capacity slot: 1000 units of food

Step 1: Initial allocation
  Alice: 1000 × 0.5 = 500
  Bob: 1000 × 0.3 = 300
  Carol: 1000 × 0.2 = 200

Step 2: Check declared needs
  Alice declared: 400 (cap at 400)
  Bob declared: 300 (keep 300)
  Carol declared: 0 (provider, not recipient)

Step 3: Capped allocation
  Alice: min(500, 400) = 400
  Bob: min(300, 300) = 300
  Carol: min(200, 0) = 0

Step 4: Reallocate declined
  Declined: 500-400 + 200-0 = 300 units
  Remaining recipients: Alice (need: 0), Bob (need: 0)
  → No one needs more, 300 units remain unallocated

Final:
  Alice: 400 (80% of her need met)
  Bob: 300 (100% of his need met)
  Utilization: 700/1000 = 70%
```

═══════════════════════════════════════════════════════════════════════
PHASE 6: INTEGRATED GOVERNANCE PIPELINE
═══════════════════════════════════════════════════════════════════════

**Purpose**: Single function that executes entire governance flow

**Function: governAndAllocate()**

```typescript
function governAndAllocate(config: {
  // Inputs
  collectiveTree: CollectiveTree;
  nodeRecognition: Map<string, NodeRecognitionMetrics>;
  
  // Filtering
  filterConfig: UnifiedFilterConfig;
  
  // Distribution
  distributionOptions?: {
    targetNodeId?: string;
    aggregationMode?: 'single-node' | 'tree-wide' | 'weighted-path';
  };
  
  // Allocation (optional)
  capacitySlots?: AvailabilitySlot[];
  memberCommitments?: Record<string, Commitment>;
  providerPubKey?: string;
}): {
  filtered_tree: CollectiveTree;
  node_recognition: Map<string, NodeRecognitionMetrics>;
  distribution: DistributionResult;
  allocation?: AllocationResult;
  filter_stats: FilterStats;
} {
  
  // Phase 1: Apply governance filters
  const filterResult = applyUnifiedFilter(
    config.collectiveTree,
    config.nodeRecognition,
    config.filterConfig
  );
  
  // Phase 2: Recalculate node recognition on filtered tree
  const filteredRecognition = calculateNodeRecognitionForTree(
    filterResult.filtered_tree
  );
  
  // Phase 3: Calculate distribution
  const distribution = nodeRecognitionToDistribution(
    filteredRecognition,
    config.distributionOptions
  );
  
  // Phase 4: Allocate (if capacity provided)
  let allocation: AllocationResult | undefined;
  if (config.capacitySlots && config.memberCommitments) {
    allocation = allocateFromCollectiveTree(
      filterResult.filtered_tree,
      filteredRecognition,
      config.capacitySlots,
      config.memberCommitments,
      {
        providerPubKey: config.providerPubKey,
        ...config.distributionOptions
      }
    );
  }
  
  return {
    filtered_tree: filterResult.filtered_tree,
    node_recognition: filteredRecognition,
    distribution,
    allocation,
    filter_stats: filterResult.stats
  };
}
```

**Complete Example:**

```typescript
// Step 1: Merge individual trees
const collectiveTree = mergeContributorTrees({
  contributorTrees: {
    Alice: aliceTree,
    Bob: bobTree,
    Carol: carolTree
  },
  recognitionShares: {
    Alice: 0.4,
    Bob: 0.3,
    Carol: 0.3
  }
});

// Step 2: Calculate recognition metrics
const nodeRecognition = calculateNodeRecognitionForTree(collectiveTree);

// Step 3: Run governance pipeline
const result = governAndAllocate({
  collectiveTree,
  nodeRecognition,
  
  // Governance rules
  filterConfig: {
    min_total_recognition: 50,
    min_average_mrd: 0.6,
    min_contributor_count: 2,
    preserve_paths: true
  },
  
  // Distribution strategy
  distributionOptions: {
    targetNodeId: 'emergency_food_fund',
    aggregationMode: 'single-node'
  },
  
  // Resources to allocate
  capacitySlots: [
    {
      id: 'food_slot_1',
      type_id: 'food',
      quantity: 1000,
      availability_windows: [/* ... */]
    }
  ],
  memberCommitments: {
    Carol: {
      id: 'carol_food',
      type_id: 'food',
      quantity: 1000,
      is_offer: true
    }
  },
  providerPubKey: 'Carol'
});

// Result contains:
// - result.filtered_tree: Governance-compliant tree
// - result.distribution: { Alice: 0.5, Bob: 0.3, Carol: 0.2 }
// - result.allocation: [{ recipient: Alice, quantity: 400 }, ...]
// - result.filter_stats: { nodes_removed: 3, contributors_affected: [...] }
```

═══════════════════════════════════════════════════════════════════════
INTEGRATION WITH V6 INDIVIDUAL TREE PROTOCOL
═══════════════════════════════════════════════════════════════════════

**Relationship: Collective as Governance Layer**

```
┌─────────────────────────────────────────────────────────────────────┐
│                    V6 PROTOCOL ARCHITECTURE                          │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  BASE LAYER: Individual Trees (structure.md)                        │
│  ┌──────────────────────────────────────────────────────┐           │
│  │ • Each entity publishes their own tree               │           │
│  │ • User inputs: needs, capacities, contributors       │           │
│  │ • Derived state: weights, satisfaction, shares       │           │
│  │ • Bilateral allocation: A ↔ B direct                 │           │
│  │ • Satisfaction feedback loop                         │           │
│  └──────────────────────────────────────────────────────┘           │
│                            ↕                                         │
│                  (trees published to network)                        │
│                            ↕                                         │
│  GOVERNANCE LAYER: Collective Trees (this document)                 │
│  ┌──────────────────────────────────────────────────────┐           │
│  │ • Merge individual trees → collective view           │           │
│  │ • Apply governance filters                           │           │
│  │ • Calculate collective distribution                  │           │
│  │ • Allocate pooled resources                          │           │
│  │ • No persistence (recompute on demand)               │           │
│  └──────────────────────────────────────────────────────┘           │
│                                                                       │
└─────────────────────────────────────────────────────────────────────┘
```

**When to Use Individual Trees:**
- Personal resource management (my needs, my capacity)
- Direct peer-to-peer relationships (I know Alice, I trust Bob)
- Fast bilateral allocation (no consensus needed)
- Private preference expression (my tree, my priorities)

**When to Use Collective Trees:**
- Coalition resource decisions (pooled fund distribution)
- Grant-making (who should receive limited collective resource?)
- Governance compliance (enforce rules: quorum, consensus)
- Democratic resource allocation (group decides together)

**Data Flow:**

Individual Trees (Persistent) → Network
Network → Collective Merging (Ephemeral)
Collective Tree → Governance Filter (Ephemeral)
Filtered Tree → Distribution (Ephemeral)
Distribution → Allocation (Recorded)
Allocation → Individual Satisfaction Ratings
Satisfaction → Individual Tree Updates
Updated Trees → Network (cycle repeats)

**Key Insight:**

Collective trees are **computed views** over individual trees, not separate
state. This maintains individual autonomy while enabling collective decisions.

═══════════════════════════════════════════════════════════════════════
IMPLEMENTATION NOTES
═══════════════════════════════════════════════════════════════════════

**Performance Considerations:**

1. **Merging Complexity**: O(N × M × D) where:
   - N = number of contributors
   - M = average nodes per tree
   - D = average depth
   - Optimization: Cache hashes, parallel tree traversal

2. **Recognition Calculation**: O(M) per node
   - Linear in number of nodes
   - Can parallelize (each node independent)

3. **Filtering**: O(M) per node
   - Bottom-up traversal (visit each node once)
   - Path preservation adds minor overhead

4. **Distribution**: O(C × N) where:
   - C = number of contributors
   - N = number of nodes (for tree-wide)
   - Single-node: O(C) only

5. **Allocation**: O(S × R) where:
   - S = capacity slots
   - R = recipients
   - Capping and reallocation: O(R) extra

**Caching Strategy:**

```typescript
// Cache merged tree (invalidate when any input tree changes)
const mergedTreeCache = new Map<string, CollectiveTree>();

function getCachedMergedTree(
  contributorTrees: Record<string, Node>
): CollectiveTree {
  const cacheKey = hashTrees(contributorTrees);
  
  if (!mergedTreeCache.has(cacheKey)) {
    const merged = mergeContributorTrees({ contributorTrees });
    mergedTreeCache.set(cacheKey, merged);
  }
  
  return mergedTreeCache.get(cacheKey)!;
}
```

**Reactive Computation (Svelte):**

```typescript
// Individual trees as stores
const aliceTree = writable<Node>(/* ... */);
const bobTree = writable<Node>(/* ... */);
const carolTree = writable<Node>(/* ... */);

// Collective tree derived from individual trees
const collectiveTree = derived(
  [aliceTree, bobTree, carolTree],
  ([$alice, $bob, $carol]) => {
    return mergeContributorTrees({
      contributorTrees: {
        Alice: $alice,
        Bob: $bob,
        Carol: $carol
      }
    });
  }
);

// Node recognition derived from collective tree
const nodeRecognition = derived(
  [collectiveTree],
  ([$tree]) => calculateNodeRecognitionForTree($tree)
);

// Distribution derived from recognition + filter config
const distribution = derived(
  [nodeRecognition, filterConfig],
  ([$recognition, $config]) => {
    const filtered = applyUnifiedFilter(/* ... */, $config);
    return nodeRecognitionToDistribution(/* ... */);
  }
);
```

**Testing Strategy:**

1. **Unit Tests:**
   - Hash collision handling
   - Weight normalization
   - MRD calculation accuracy
   - Filter logic correctness

2. **Integration Tests:**
   - End-to-end pipeline (merge → filter → distribute → allocate)
   - Multi-contributor scenarios (3+)
   - Edge cases (empty trees, single contributor, all filtered out)

3. **Property Tests:**
   - Distribution always sums to 1.0
   - Weights always sum to 1.0 at each level
   - No negative values
   - Filtered tree is subset of original

4. **Performance Tests:**
   - Large trees (1000+ nodes)
   - Many contributors (50+)
   - Deep trees (depth > 10)

═══════════════════════════════════════════════════════════════════════
COMPARISON: INDIVIDUAL vs COLLECTIVE ARCHITECTURE
═══════════════════════════════════════════════════════════════════════

```
┌────────────────────────────────────────────────────────────────────┐
│ FEATURE                │ Individual Trees │ Collective Trees       │
├────────────────────────────────────────────────────────────────────┤
│ Perspective            │ Personal         │ Merged/Consensus       │
│ Storage                │ Persistent       │ Ephemeral (computed)   │
│ Node Identity          │ Internal UUID    │ Hash-based             │
│ Recognition Metric     │ ShareOfTotal     │ MRD + Total            │
│ Satisfaction           │ Per allocation   │ Aggregated from indiv. │
│ Governance             │ Self-sovereign   │ Filtered/voted         │
│ Allocation Strategy    │ Mutual Sat. (MS) │ Distribution-based     │
│ Decision Speed         │ Fast (bilateral) │ Slower (consensus)     │
│ Use Case               │ Personal needs   │ Collective funds       │
│ Privacy                │ Full control     │ Shared visibility      │
│ Scalability            │ O(n²) pairs      │ O(n) collective        │
└────────────────────────────────────────────────────────────────────┘
```

**Architectural Complementarity:**

Individual trees provide the **foundation**:
- Autonomy: Each entity controls their own tree
- Flexibility: Express any structure, any priority
- Learning: Satisfaction feeds back to individual shares
- Bilateral: Direct relationships without group coordination

Collective trees provide **coordination**:
- Consensus: Democratic resource decisions
- Governance: Enforce collective rules
- Efficiency: Single pooled allocation vs. many bilateral
- Transparency: Shared view of collective priorities

**Neither replaces the other - they coexist:**

Example: Community fund distribution
1. Individual trees express personal priorities (individual layer)
2. Fund governance uses collective merging + filtering (collective layer)
3. Resources allocated by collective distribution (collective layer)
4. Recipients rate satisfaction in their individual trees (individual layer)
5. Updated individual trees feed into next collective decision (cycle repeats)

═══════════════════════════════════════════════════════════════════════
ADVANCED FEATURES
═══════════════════════════════════════════════════════════════════════

**1. Recursive Organizations**

Contributors can be individuals OR organizations:

```typescript
const contributorTrees = {
  Alice: aliceTree,              // Individual
  Bob: bobTree,                  // Individual
  HumanitarianCoalition: {       // Organization (itself a collective!)
    contributorTrees: {
      Carol: carolTree,
      Dave: daveTree,
      EmergencyNGO: emergencyTree  // Nested organization!
    }
  }
};

// Recursive merging handles nested collectives
```

**2. Time-Weighted Recognition**

Weight recent contributions more:

```typescript
function timeWeightedRecognition(
  node: CollectiveNode,
  decayFactor: number = 0.9
): number {
  let weightedRecognition = 0;
  
  for (const [contributorId, data] of node.contributor_data) {
    const age = now() - data.timestamp;
    const decay = Math.pow(decayFactor, age / ONE_WEEK);
    weightedRecognition += data.weight * data.points * decay;
  }
  
  return weightedRecognition;
}
```

**3. Quadratic Distribution**

Reduce influence of large contributors:

```typescript
function quadraticDistribution(
  linearDistribution: Record<string, number>
): Record<string, number> {
  const quadratic: Record<string, number> = {};
  let total = 0;
  
  // Take square root of each share
  for (const [contributorId, share] of Object.entries(linearDistribution)) {
    quadratic[contributorId] = Math.sqrt(share);
    total += quadratic[contributorId];
  }
  
  // Renormalize
  for (const contributorId in quadratic) {
    quadratic[contributorId] /= total;
  }
  
  return quadratic;
}
```

**4. Veto Rights**

Critical contributors can block decisions:

```typescript
{
  filterConfig: {
    required_contributors: ['Alice'],  // Alice must recognize for node to pass
    min_average_mrd: 0.5
  }
}
```

**5. Threshold Governance**

Tiered filtering based on amount:

```typescript
{
  filterConfig: {
    logic_rule: {
      "if": [
        {">=": [{"var": "allocation_amount"}, 10000]},  // Large allocations
        {
          "and": [
            {">=": [{"var": "average_mrd"}, 0.8]},       // Need 80% consensus
            {">=": [{"var": "contributor_count"}, 5]}    // At least 5 supporters
          ]
        },
        {">=": [{"var": "average_mrd"}, 0.5]}           // Small: just 50%
      ]
    }
  }
}
```

═══════════════════════════════════════════════════════════════════════
FUTURE WORK
═══════════════════════════════════════════════════════════════════════

**1. Incremental Merging**

Instead of recomputing entire collective tree when one contributor updates:
- Track which nodes affected by update
- Recompute only affected subtrees
- Merge incrementally into cached collective tree

**2. Distributed Merging**

Parallel computation across multiple workers:
- Partition contributors by hash ranges
- Merge partitions in parallel
- Combine results in final pass

**3. Differential Filtering**

Track filter results, recompute only when:
- Recognition metrics change significantly
- Filter rules change
- New contributors added/removed

**4. Allocation History Tracking**

Feed allocation outcomes back into individual trees:
- Track which distributions led to successful allocations
- Weight successful strategies higher in future merges
- Learn optimal governance rules from outcomes

**5. Interactive Governance**

Real-time updates as contributors adjust trees:
- Live preview of how changes affect distribution
- Negotiation UI: "If I increase X, what happens?"
- Convergence visualization: Track toward consensus

═══════════════════════════════════════════════════════════════════════
CONCLUSION
═══════════════════════════════════════════════════════════════════════

**Collective Tree Architecture Summary:**

The collective tree implementation provides a governance layer atop v6's
individual tree protocol. Key design decisions:

1. **Ephemeral, Not Persistent**: Collective trees computed on-demand
2. **Hash-Based Merging**: Semantic node matching across contributors
3. **Dual Metrics**: Recognition (weight) + MRD (consensus)
4. **Flexible Filtering**: Recognition, JSON Logic, custom functions
5. **Multiple Distribution Modes**: Single-node, tree-wide, weighted-path
6. **Backward Compatible**: Works with existing v6 individual trees

**Primary Innovation:**

Multi-Recognition Depth (MRD) as consensus measure prevents single
contributors from dominating collective decisions, even with high recognition
weight.

**Use Cases:**

- Grant distributions (limited funds, collective decision)
- Coalition resource pooling (shared capacity, fair allocation)
- Democratic governance (enforce quorum, consensus thresholds)
- Transparent decision-making (audit trail of governance rules)

**Integration with V6:**

Collective trees don't replace individual trees - they complement them.
Individual trees remain the source of truth and handle personal resource
management. Collective trees provide governance for group decisions.

═══════════════════════════════════════════════════════════════════════
END OF DOCUMENT
═══════════════════════════════════════════════════════════════════════

