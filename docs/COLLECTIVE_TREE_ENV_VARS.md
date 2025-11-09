# Collective Tree Scheduler - Environment Variables

Add these to your `.env.local` file to configure the collective tree scheduler.

## Computation Intervals

```bash
# Tree merge interval (milliseconds)
# Default: 3600000 (1 hour)
# How often to merge contributor trees into collective trees
COLLECTIVE_TREE_MERGE_INTERVAL_MS=3600000

# Collective recognition interval (milliseconds)
# Default: 1800000 (30 minutes)
# How often to compute collective recognition values
COLLECTIVE_RECOGNITION_INTERVAL_MS=1800000

# Capacity allocation interval (milliseconds)
# Default: 3600000 (1 hour)
# How often to allocate capacities across collective tree nodes
COLLECTIVE_CAPACITY_ALLOCATION_INTERVAL_MS=3600000
```

## Feature Flags

```bash
# Enable automatic tree merging
# Default: true
AUTO_MERGE_COLLECTIVE_TREES=true

# Enable automatic collective recognition computation
# Default: true
AUTO_COMPUTE_COLLECTIVE_RECOGNITION=true

# Enable automatic capacity allocation
# Default: true
AUTO_ALLOCATE_COLLECTIVE_CAPACITY=true

# Startup delay before first computation (milliseconds)
# Default: 30000 (30 seconds)
COLLECTIVE_TREE_STARTUP_DELAY_MS=30000

# Enable detailed logging
# Default: false
COLLECTIVE_TREE_VERBOSE_LOGGING=false
```

## Tree Merge Configuration

```bash
# Default merge strategy
# Options: 'weighted_average' (recommended), 'union', 'intersection'
# Default: 'weighted_average'
COLLECTIVE_TREE_MERGE_STRATEGY=weighted_average

# Name collision strategy
# Options: 'weighted_priority', 'append_contributor', 'manual_resolve'
# Default: 'weighted_priority'
COLLECTIVE_TREE_NAME_COLLISION=weighted_priority

# Minimum contributors required for collective tree creation
# Default: 2
MINIMUM_COLLECTIVE_CONTRIBUTORS=2
```

## Example Configurations

### Development (Fast Testing)
```bash
# Run computations frequently for testing
COLLECTIVE_TREE_MERGE_INTERVAL_MS=300000         # 5 minutes
COLLECTIVE_RECOGNITION_INTERVAL_MS=180000        # 3 minutes
COLLECTIVE_CAPACITY_ALLOCATION_INTERVAL_MS=300000  # 5 minutes
COLLECTIVE_TREE_STARTUP_DELAY_MS=5000            # 5 seconds
COLLECTIVE_TREE_VERBOSE_LOGGING=true
```

### Production (Standard)
```bash
# Hourly tree operations, moderate recognition updates
COLLECTIVE_TREE_MERGE_INTERVAL_MS=3600000        # 1 hour
COLLECTIVE_RECOGNITION_INTERVAL_MS=1800000       # 30 minutes
COLLECTIVE_CAPACITY_ALLOCATION_INTERVAL_MS=3600000  # 1 hour
MINIMUM_COLLECTIVE_CONTRIBUTORS=3                # Require 3+ contributors
COLLECTIVE_TREE_VERBOSE_LOGGING=false
```

### High Activity
```bash
# More frequent updates for active collectives
COLLECTIVE_TREE_MERGE_INTERVAL_MS=1800000        # 30 minutes
COLLECTIVE_RECOGNITION_INTERVAL_MS=600000        # 10 minutes
COLLECTIVE_CAPACITY_ALLOCATION_INTERVAL_MS=1800000  # 30 minutes
COLLECTIVE_TREE_VERBOSE_LOGGING=false
```

## Quick Reference

| Interval | Milliseconds |
|----------|--------------|
| 1 minute | 60000 |
| 5 minutes | 300000 |
| 10 minutes | 600000 |
| 30 minutes | 1800000 |
| 1 hour | 3600000 |
| 6 hours | 21600000 |
| 12 hours | 43200000 |
| 1 day | 86400000 |

