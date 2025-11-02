# Server-Side Collective Recognition & Membership Scheduler

This module provides scheduled server-side computations for the collective recognition and membership system.

## Overview

The scheduler runs two types of computations at configurable intervals:

1. **Membership Computation** (MRD-based): Updates capacity membership based on Mutual Recognition Density
2. **Allocation Computation**: Calculates resource allocations based on collective recognition shares

## Quick Start

### 1. Set Environment Variables

```bash
# .env.local or .env

# Membership computation interval (default: 7 days)
MEMBERSHIP_COMPUTATION_INTERVAL_MS=604800000  # 7 days in ms

# Allocation computation interval (default: 1 day)
ALLOCATION_COMPUTATION_INTERVAL_MS=86400000   # 1 day in ms

# MRD threshold for membership (default: 0.5)
MRD_THRESHOLD=0.5

# Minimum mutual recognition filter (default: 0.0)
MINIMUM_MUTUAL_RECOGNITION=0.0

# Enable/disable features (default: true)
AUTO_UPDATE_MEMBERSHIP=true
AUTO_COMPUTE_ALLOCATIONS=true

# Startup delay before first computation (default: 30 seconds)
COMPUTATION_STARTUP_DELAY_MS=30000

# Enable verbose logging (default: false)
COLLECTIVE_VERBOSE_LOGGING=true
```

### 2. Implement Callbacks

Create a file `src/lib/server/collective/callbacks.ts` based on the example:

```typescript
import type { ComputationCallbacks } from './scheduler';
import { getHolsterInstance } from '$lib/server/holster/core';
import { getDatabase } from '$lib/server/db';

export function createCallbacks(): ComputationCallbacks {
  return {
    async fetchRecognitionData() {
      // Query your database/Holster for recognition relationships
      const holster = getHolsterInstance();
      // ... implementation
      return recognitionData;
    },
    
    async fetchAutoUpdateCapacities() {
      // Query database for capacities with auto_update_members_by_mrd = true
      const db = getDatabase();
      // ... implementation
      return capacities;
    },
    
    async saveCapacityMembers(capacityId, members, added, removed, timestamp) {
      // Save updated members to database
      const db = getDatabase();
      // ... implementation
    },
    
    async fetchCapacitiesForAllocation() {
      // Query database for capacities with slots
      // ... implementation
    },
    
    async fetchNeeds() {
      // Query database for open/partially-fulfilled needs
      // ... implementation
    },
    
    async fetchMemberTrees(memberIds) {
      // Query Holster for member recognition trees
      // ... implementation
    },
    
    async saveAllocations(capacityId, allocations) {
      // Save allocation results to database
      // ... implementation
    },
    
    async logComputation(event, data) {
      // Optional: Log computation events
      // ... implementation
    }
  };
}
```

### 3. Initialize in hooks.server.ts

```typescript
import { startScheduler } from '$lib/server/collective';
import { createCallbacks } from '$lib/server/collective/callbacks';

let schedulerInitialized = false;

if (!schedulerInitialized) {
  // ... other initialization code
  
  // Initialize collective recognition scheduler
  const callbacks = createCallbacks();
  startScheduler(callbacks);
  
  schedulerInitialized = true;
}
```

## Architecture

### Shared Modules (Client & Server)

The computation modules are in `.svelte.ts` files and can be used by both frontend and backend:

- `$lib/protocol/collective/collective-membership.svelte.ts` - MRD membership computation
- `$lib/protocol/collective/collective-recognition.svelte.ts` - Allocation computation

### Server-Only Modules

The scheduler infrastructure is server-only:

- `$lib/server/collective/scheduler.ts` - Scheduling logic
- `$lib/server/collective/config.ts` - Configuration
- `$lib/server/collective/callbacks.ts` - Data access callbacks (you implement this)

## Configuration Options

### Intervals

| Variable | Default | Description |
|----------|---------|-------------|
| `MEMBERSHIP_COMPUTATION_INTERVAL_MS` | 604800000 (7 days) | How often to recompute membership |
| `ALLOCATION_COMPUTATION_INTERVAL_MS` | 86400000 (1 day) | How often to recompute allocations |
| `COMPUTATION_STARTUP_DELAY_MS` | 30000 (30 sec) | Delay before first computation |

### Thresholds

| Variable | Default | Description |
|----------|---------|-------------|
| `MRD_THRESHOLD` | 0.5 | Minimum MRD score for membership (0-1) |
| `MINIMUM_MUTUAL_RECOGNITION` | 0.0 | Minimum mutual recognition to count (0-100) |

### Feature Flags

| Variable | Default | Description |
|----------|---------|-------------|
| `AUTO_UPDATE_MEMBERSHIP` | true | Enable automatic membership updates |
| `AUTO_COMPUTE_ALLOCATIONS` | true | Enable automatic allocation computations |
| `COLLECTIVE_VERBOSE_LOGGING` | false | Enable detailed logging |

## Manual Triggers

You can manually trigger computations via API endpoints:

```typescript
// src/routes/api/admin/trigger-membership/+server.ts
import { triggerMembershipComputation } from '$lib/server/collective';

export async function POST() {
  await triggerMembershipComputation();
  return new Response('Membership computation triggered', { status: 200 });
}
```

```typescript
// src/routes/api/admin/trigger-allocation/+server.ts
import { triggerAllocationComputation } from '$lib/server/collective';

export async function POST() {
  await triggerAllocationComputation();
  return new Response('Allocation computation triggered', { status: 200 });
}
```

## Monitoring

Check scheduler status:

```typescript
import { getSchedulerStatus } from '$lib/server/collective';

const status = getSchedulerStatus();
console.log(status);
// {
//   isRunning: true,
//   lastMembershipRun: Date,
//   lastAllocationRun: Date,
//   membershipRunCount: 42,
//   allocationRunCount: 142,
//   config: { ... }
// }
```

## How It Works

### Membership Computation Flow

1. **Fetch Recognition Data**: Loads all recognition relationships from database/Holster
2. **Find Auto-Update Capacities**: Queries capacities where `auto_update_members_by_mrd = true`
3. **Compute MRD**: For each capacity, calculates Mutual Recognition Density for all participants
4. **Update Members**: Adds participants above threshold, removes those below
5. **Save Changes**: Persists updated member lists to database

### Allocation Computation Flow

1. **Fetch Capacities**: Loads all capacities with availability slots
2. **Fetch Needs**: Loads all open/partially-fulfilled needs with need slots
3. **Fetch Recognition Trees**: Loads member recognition trees for collective recognition calculation
4. **Compute Allocations**: For each capacity:
   - Calculate collective recognition shares
   - Match availability slots to need slots
   - Apply compliance filters
   - Generate final allocations
5. **Save Results**: Persists allocation computations to database

## Best Practices

### Production Deployment

1. **Start with longer intervals**: Test with daily membership updates before going to weekly
2. **Monitor performance**: Log computation times and adjust intervals if needed
3. **Handle failures gracefully**: Implement retry logic in callbacks
4. **Use database transactions**: Ensure atomicity of updates
5. **Log everything**: Use the `logComputation` callback for audit trails

### Development

1. **Use shorter intervals for testing**: Set to 1 minute for rapid testing
2. **Enable verbose logging**: Set `COLLECTIVE_VERBOSE_LOGGING=true`
3. **Test callbacks independently**: Use `validateCallbacks()` helper
4. **Mock data in dev**: Use the example callbacks for initial development

### Scaling

For large networks (>1000 participants):

1. **Batch processing**: Process capacities in batches instead of all at once
2. **Incremental updates**: Only recompute changed capacities
3. **Caching**: Cache recognition data between computations
4. **Async jobs**: Move computations to a job queue (Bull, BullMQ, etc.)
5. **Database optimization**: Add indexes on frequently queried fields

## Troubleshooting

### "No callbacks registered"

Make sure you call `startScheduler(callbacks)` in `hooks.server.ts`.

### Computations not running

1. Check that feature flags are enabled (`AUTO_UPDATE_MEMBERSHIP`, `AUTO_COMPUTE_ALLOCATIONS`)
2. Verify startup delay has passed
3. Check server logs for errors
4. Verify callbacks are implemented correctly

### Performance issues

1. Enable verbose logging to see computation times
2. Reduce interval frequency
3. Implement batching for large datasets
4. Add database indexes
5. Consider moving to job queue

## Example: Complete Integration

See the complete example in `src/lib/server/collective/example-integration.md`.

## API Reference

See individual module documentation:
- [Scheduler API](./scheduler.ts)
- [Configuration](./config.ts)
- [Callbacks](./callbacks.example.ts)

