# Server-Side Collective Tree Scheduler

## Overview

This module provides scheduled server-side computations for collective tree operations, complementing the collective recognition and membership scheduler.

## What It Does

### 1. Tree Merging (Every 1 hour by default)
Combines individual contributor trees into collective trees based on mutual recognition:
- Fetches contributor trees from Holster
- Merges using weighted recognition shares
- Resolves name collisions automatically
- Stores resulting collective trees

### 2. Collective Recognition Computation (Every 30 minutes by default)
Calculates how recognition flows through collective tree structures:
- Computes path-weighted recognition
- Calculates contributor influences
- Tracks recognition density per node

### 3. Capacity Allocation (Every 1 hour by default)
Distributes individual capacities across collective tree nodes:
- Aggregates individual capacities
- Allocates proportionally to tree weights
- Calculates efficiency and fairness metrics

## Architecture

```
Individual Trees (Holster)
    ↓
Tree Merging ─→ Collective Trees (Holster)
    ↓
Collective Recognition ─→ Recognition Results (Holster)
    ↓
Capacity Allocation ─→ Allocation Results (Holster)
```

## Configuration

See `COLLECTIVE_TREE_ENV_VARS.md` in project root for full configuration options.

### Quick Start

Add to `.env.local`:

```bash
# Computation intervals
COLLECTIVE_TREE_MERGE_INTERVAL_MS=3600000        # 1 hour
COLLECTIVE_RECOGNITION_INTERVAL_MS=1800000       # 30 minutes
COLLECTIVE_CAPACITY_ALLOCATION_INTERVAL_MS=3600000  # 1 hour

# Feature flags
AUTO_MERGE_COLLECTIVE_TREES=true
AUTO_COMPUTE_COLLECTIVE_RECOGNITION=true
AUTO_ALLOCATE_COLLECTIVE_CAPACITY=true

# Configuration
MINIMUM_COLLECTIVE_CONTRIBUTORS=2
COLLECTIVE_TREE_MERGE_STRATEGY=weighted_average
COLLECTIVE_TREE_VERBOSE_LOGGING=true  # For development
```

## API Endpoints

### Status & Validation

```bash
GET /api/collective-tree/status      # Check scheduler status
GET /api/collective-tree/validate    # Validate callbacks
```

### Manual Triggers

```bash
POST /api/collective-tree/trigger-merge         # Force tree merge now
POST /api/collective-tree/trigger-recognition   # Force recognition computation now
POST /api/collective-tree/trigger-allocation    # Force capacity allocation now
```

## Data Flow

### Tree Merge Flow

1. **Fetch Collective Definitions**: Query Holster for collectives with `auto_merge: true`
2. **Fetch Contributor Trees**: Get recognition trees for each contributor
3. **Merge Trees**: Use `mergeContributorTrees()` from collective-tree.svelte.ts
4. **Save Results**: Store merged tree and merge statistics in Holster

### Collective Recognition Flow

1. **Fetch Collective Trees**: Load all previously merged trees
2. **Compute Recognition**: For each node, calculate collective recognition values
3. **Save Results**: Store recognition matrices and path analyses

### Capacity Allocation Flow

1. **Fetch Collective Trees**: Load merged trees
2. **Fetch Individual Capacities**: Get capacity declarations from all users
3. **Compute Allocation**: Use `calculateCollectiveCapacityAllocation()`
4. **Save Results**: Store allocation maps and efficiency metrics

## Holster Data Structure

### Input Data

```javascript
// Collective definitions
holster['collective_definitions'][collectiveId] = {
  id: string,
  name: string,
  contributor_ids: string[],
  recognition_shares?: Record<string, number>,
  auto_merge: boolean,
  merge_strategy?: string,
  last_merge?: string
}

// Individual recognition trees
holster['trees'][userId]['recognition_tree'] = Node
```

### Output Data

```javascript
// Merged collective trees
holster['collective_trees'][collectiveId] = CollectiveTree

// Tree merge history
holster['collective_tree_merge_history'][`${collectiveId}_${timestamp}`] = {
  collective_id: string,
  merge_stats: {...},
  timestamp: string
}

// Collective recognition results
holster['collective_recognition_results'][`${treeId}_${timestamp}`] = {
  tree_id: string,
  recognition: {...},
  timestamp: string
}

// Latest pointers for quick access
holster['collective_recognition_latest'][treeId] = {
  result_key: string,
  timestamp: string,
  node_count: number
}

// Capacity allocation results
holster['collective_capacity_allocations'][`${treeId}_${timestamp}`] = {
  tree_id: string,
  allocation: {...},
  timestamp: string
}

// Computation logs
holster['collective_tree_computation_logs'][`${event}_${timestamp}`] = {
  event: string,
  data: {...},
  timestamp: string
}
```

## Integration with Main Scheduler

Both schedulers run independently:

**Collective Recognition & Membership** (`$lib/server/collective/`)
- Focus: Resource allocation **to members**
- Who gets what resources?
- Daily/weekly intervals

**Collective Tree Operations** (`$lib/server/collective-tree/`)
- Focus: Collective **structure and intelligence**
- How do trees combine? How does recognition flow?
- Hourly intervals (more dynamic)

They complement each other:
- Membership determines **who participates**
- Trees determine **collective structure**
- Recognition flows through **both systems**

## Mathematical Foundations

### Tree Merging

```
For each node in collective tree:
  node_weight = Σ(contributor_node_weight_i × contributor_recognition_i)

Where:
  contributor_node_weight_i = node's points / sibling_sum in contributor's tree
  contributor_recognition_i = contributor's share of collective recognition
```

### Collective Recognition

```
collective_recognition(node) = Σ(path_weight_i × contributor_weight_i)

Where:
  path_weight_i = ∏(percentage at each level from root)
  contributor_weight_i = recognition share in collective
```

### Capacity Allocation

```
node_capacity(node) = total_collective_capacity × node_weight

Where:
  total_collective_capacity = Σ(individual_capacity_i × contributor_weight_i)
  node_weight = cumulative weight from root to node
```

## Monitoring

Watch server logs for computation events:

```
[COLLECTIVE-TREE-SCHEDULER] 🚀 Starting scheduler...
  → Tree merge: every 1.0 hours
  → Collective recognition: every 30.0 minutes
  → Capacity allocation: every 1.0 hours

[COLLECTIVE-TREE-SCHEDULER] ✅ Tree merge completed in 234ms
  → Processed 5 collectives
  → Successfully merged 5 trees
  → Total nodes merged: 127

[COLLECTIVE-TREE-SCHEDULER] ✅ Recognition computation completed in 156ms
  → Processed 5 trees
  → Computed 5 recognition sets

[COLLECTIVE-TREE-SCHEDULER] ✅ Capacity allocation completed in 89ms
  → Processed 5 trees
  → Computed 5 allocations
```

## Example: Creating a Collective for Auto-Merge

```typescript
// In your application code, create a collective definition:

const collectiveDefinition = {
  id: 'housing-collective-berlin',
  name: 'Berlin Housing Collective',
  contributor_ids: ['alice', 'bob', 'carol', 'dave'],
  recognition_shares: {
    'alice': 0.30,  // Alice has 30% recognition share
    'bob': 0.25,
    'carol': 0.25,
    'dave': 0.20
  },
  auto_merge: true,  // Enable automatic merging
  merge_strategy: 'weighted_average',
  last_merge: null
};

// Save to Holster
await user.get('collective_definitions').next(collectiveDefinition.id).put(collectiveDefinition);

// The scheduler will automatically:
// 1. Detect this collective (has auto_merge: true)
// 2. Fetch trees for Alice, Bob, Carol, and Dave
// 3. Merge them into a collective tree every hour
// 4. Compute collective recognition every 30 minutes
// 5. Allocate capacities across the tree nodes
```

## Troubleshooting

### No collectives being merged

**Check**:
- Do you have collective definitions with `auto_merge: true`?
- Do contributors have recognition trees in Holster?
- Are there at least `MINIMUM_COLLECTIVE_CONTRIBUTORS`?

**Fix**:
```bash
curl http://localhost:3000/api/collective-tree/validate
```

### Trees not appearing

**Check**:
- Is scheduler running? Check `/api/collective-tree/status`
- Check server logs for errors
- Verify Holster data exists

**Fix**:
```bash
# Check status
curl http://localhost:3000/api/collective-tree/status

# Force a merge manually
curl -X POST http://localhost:3000/api/collective-tree/trigger-merge
```

### Slow computations

**Causes**:
- Large number of contributors (>20 per collective)
- Deep tree structures (>10 levels)
- Many collectives being processed

**Solutions**:
- Increase computation intervals
- Split large collectives
- Enable verbose logging to identify bottlenecks

## Performance

Expected performance:
- **Tree Merge**: ~50-100ms per collective (2-10 contributors)
- **Recognition**: ~20-50ms per tree
- **Allocation**: ~10-30ms per tree

For large networks:
- 10 collectives, 5 contributors each: < 2 seconds total
- 50 collectives, 10 contributors each: < 10 seconds total

## Security Considerations

1. **Tree Privacy**: Only public subtrees should be merged into collectives
2. **Collective Membership**: Contributors must consent to inclusion
3. **API Authentication**: Add authentication to manual trigger endpoints
4. **Data Access**: Ensure callbacks only access authorized data

## Future Enhancements

- **Incremental Merging**: Only recompute changed subtrees
- **Parallel Processing**: Merge multiple collectives in parallel
- **Conflict Detection**: Alert on significant recognition discrepancies
- **Historical Analysis**: Track collective evolution over time
- **Smart Scheduling**: Adjust intervals based on tree change rate

## Related Documentation

- **Main Setup**: `COLLECTIVE_SCHEDULER_SETUP.md` (project root)
- **Environment Variables**: `COLLECTIVE_TREE_ENV_VARS.md` (project root)
- **Collective Tree Math**: `src/lib/protocol/collective/collective-tree.svelte.ts`
- **Callback Implementation**: `./callbacks.ts`
- **Scheduler Logic**: `./scheduler.ts`

