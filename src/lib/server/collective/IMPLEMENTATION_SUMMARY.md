# Collective Recognition Scheduler - Implementation Summary

## What Was Built

A complete server-side scheduled computation system for collective recognition and membership calculations.

## Implementation Details

### Core Modules

1. **`callbacks.ts`** - Production implementation
   - Uses `holsterGet`, `holsterNextPut`, `holsterGetArray` from `db.ts`
   - Fetches recognition data from user trees
   - Queries capacities and needs from Holster
   - Saves computation results back to Holster
   - Includes validation function for testing

2. **`scheduler.ts`** - Scheduling engine
   - Uses Node.js `setInterval` for recurring tasks
   - Separate intervals for membership and allocation computations
   - Configurable startup delay
   - Comprehensive error handling and logging
   - Manual trigger support

3. **`config.ts`** - Environment configuration
   - Loads settings from environment variables
   - Provides defaults for all settings
   - Helper functions for formatting

4. **`index.ts`** - Public API
   - Clean exports for external use
   - Type definitions

## Data Flow

```
Server Startup
    ↓
hooks.server.ts initializes
    ↓
createCallbacks() → Returns ComputationCallbacks
    ↓
startScheduler(callbacks)
    ↓
[After startup delay]
    ↓
┌─────────────────────────────────────┐
│  MEMBERSHIP COMPUTATION (Weekly)    │
├─────────────────────────────────────┤
│ 1. Fetch recognition data           │
│    - Query all user trees            │
│    - Extract recognition shares      │
│                                      │
│ 2. Find auto-update capacities       │
│    - Filter by auto_update flag      │
│                                      │
│ 3. Compute MRD for each capacity     │
│    - Use MRDMembershipModule         │
│    - Apply threshold                 │
│                                      │
│ 4. Save updated members              │
│    - Update capacity in Holster      │
│    - Store history                   │
└─────────────────────────────────────┘
    ↓
[Repeat every week]

┌─────────────────────────────────────┐
│  ALLOCATION COMPUTATION (Daily)     │
├─────────────────────────────────────┤
│ 1. Fetch capacities with slots      │
│                                      │
│ 2. Fetch needs with slots           │
│                                      │
│ 3. Fetch member recognition trees   │
│                                      │
│ 4. Compute allocations               │
│    - Calculate recognition shares    │
│    - Match slots                     │
│    - Apply filters                   │
│                                      │
│ 5. Save allocation results           │
│    - Store computation result        │
│    - Update latest pointer           │
│    - Store individual allocations    │
└─────────────────────────────────────┘
    ↓
[Repeat every day]
```

## Holster Data Structure

### Input Data (Read)

```
holster/
├── trees/
│   └── {userId}/
│       └── recognition_tree          # Node (recognition tree)
│
├── capacities/
│   └── {capacityId}                  # BaseCapacity
│       ├── id
│       ├── members: string[]
│       ├── capacity_slots: AvailabilitySlot[]
│       ├── auto_update_members_by_mrd: boolean
│       ├── mrd_threshold?: number
│       └── filters?: Record<string, ComplianceFilter>
│
└── needs/
    └── {needId}                      # BaseNeed
        ├── id
        ├── declarer_id
        ├── need_slots: NeedSlot[]
        ├── status: 'open' | 'partially-fulfilled' | 'fulfilled'
        └── fulfilled_amount: number
```

### Output Data (Write)

```
holster/
├── capacities/
│   └── {capacityId}
│       ├── members: string[]         # UPDATED by membership computation
│       └── last_membership_update    # UPDATED timestamp
│
├── capacity_membership_history/
│   └── {capacityId}_{timestamp}      # Historical record
│       ├── capacity_id
│       ├── members: string[]
│       ├── added: string[]
│       ├── removed: string[]
│       └── timestamp
│
├── allocation_computations/
│   └── {capacityId}_{timestamp}      # Full computation result
│       ├── capacity_id
│       ├── total_capacity
│       ├── total_allocated
│       ├── member_set: string[]
│       ├── final_allocations: Record<string, number>
│       └── ... (full AllocationComputationResult)
│
├── allocation_computations_latest/
│   └── {capacityId}                  # Quick access pointer
│       ├── timestamp
│       ├── result_key
│       ├── total_allocated
│       └── member_count
│
├── allocations/
│   └── {capacityId}_{memberId}_{timestamp}
│       ├── capacity_id
│       ├── member_id
│       ├── amount
│       ├── timestamp
│       └── computation_result_key
│
└── computation_logs/
    ├── {event}_{timestamp}           # Detailed log
    └── latest/{event}                # Latest log per event type
```

## API Endpoints

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/api/collective/validate` | GET | Test callbacks and data access |
| `/api/collective/status` | GET | Get scheduler status |
| `/api/collective/trigger-membership` | POST | Manual membership computation |
| `/api/collective/trigger-allocation` | POST | Manual allocation computation |

## Environment Variables

| Variable | Default | Used By |
|----------|---------|---------|
| `MEMBERSHIP_COMPUTATION_INTERVAL_MS` | 604800000 (7d) | scheduler.ts |
| `ALLOCATION_COMPUTATION_INTERVAL_MS` | 86400000 (1d) | scheduler.ts |
| `MRD_THRESHOLD` | 0.5 | config.ts |
| `MINIMUM_MUTUAL_RECOGNITION` | 0.0 | config.ts |
| `AUTO_UPDATE_MEMBERSHIP` | true | config.ts |
| `AUTO_COMPUTE_ALLOCATIONS` | true | config.ts |
| `COMPUTATION_STARTUP_DELAY_MS` | 30000 (30s) | config.ts |
| `COLLECTIVE_VERBOSE_LOGGING` | false | config.ts |

## Key Design Decisions

### 1. Shared Computation Modules

The actual computation logic lives in `.svelte.ts` files that can be imported by both frontend and backend:
- `collective-membership.svelte.ts` - MRD computation
- `collective-recognition.svelte.ts` - Allocation computation

This ensures the same algorithms run everywhere.

### 2. Callback Pattern

The scheduler is agnostic to the data layer. You provide callbacks to fetch/save data. This makes it:
- Testable (can mock callbacks)
- Flexible (works with any database)
- Maintainable (separation of concerns)

### 3. Holster Integration

Uses the helper utilities from `db.ts`:
- `holsterGet()` - Promise-based reads
- `holsterNextPut()` - Promise-based writes
- `holsterGetArray()` - Collection queries with filtering
- `ensureAuthenticated()` - Auth checks

### 4. Error Handling

- Try/catch at every async boundary
- Graceful degradation (log errors, don't crash)
- Failed computations don't prevent future runs
- Detailed error logging for debugging

### 5. History Tracking

All membership changes and allocation computations are logged:
- Audit trail for governance
- Time-series analysis
- Debugging and verification

## Usage Pattern

### For Frontend

```typescript
// Real-time UI updates
import { computeAllocations } from '$lib/protocol/collective/collective-recognition.svelte';

// Use in components
const result = computeAllocations(capacity, needs, memberTrees);
```

### For Backend

```typescript
// Scheduled batch computations
import { createCallbacks } from '$lib/server/collective/callbacks';
import { startScheduler } from '$lib/server/collective';

// Initialize once in hooks.server.ts
const callbacks = createCallbacks();
startScheduler(callbacks);
```

## Testing Workflow

1. **Validate Setup**
   ```bash
   curl http://localhost:3000/api/collective/validate
   ```

2. **Check Status**
   ```bash
   curl http://localhost:3000/api/collective/status
   ```

3. **Manual Test Run**
   ```bash
   curl -X POST http://localhost:3000/api/collective/trigger-membership
   ```

4. **Monitor Logs**
   Watch server console for computation events

5. **Verify Data**
   Check Holster for updated members and allocations

## Performance Considerations

### Optimization 1: Filtered Queries
Uses `holsterGetArray` with filters to only load relevant data:
- Auto-update capacities only
- Open/partially-fulfilled needs only
- Capacities with slots only

### Optimization 2: Batch Processing
Processes multiple capacities in single computation cycle.

### Optimization 3: Efficient Slot Matching
The allocation engine includes multiple optimizations:
- Pre-computed compatibility matrix
- Time/location bucketing
- Active slot tracking
- Early exit conditions

### Scalability Notes

For networks with 100+ participants:
- Recognition data fetch: O(N) where N = number of users
- Membership computation: O(C × P²) where C = capacities, P = participants
- Allocation computation: O(C × M × S²) where M = members, S = slots

Expected performance:
- 100 participants: < 1 second per computation
- 1000 participants: < 10 seconds per computation

## Future Enhancements

### Potential Improvements

1. **Incremental Updates**
   - Track which recognition trees changed
   - Only recompute affected capacities

2. **Job Queue Integration**
   - Move to BullMQ or similar for large networks
   - Parallel processing of independent capacities

3. **Caching Layer**
   - Cache recognition data between computations
   - Invalidate on tree updates

4. **Notifications**
   - Notify members when membership changes
   - Alert capacity owners of allocation results

5. **Analytics**
   - Track membership stability over time
   - Analyze allocation utilization rates
   - Identify network health metrics

## Maintenance

### Monitoring Checklist

- [ ] Check computation logs weekly
- [ ] Verify allocations match expectations
- [ ] Monitor computation duration (should be stable)
- [ ] Review failed computation logs
- [ ] Validate data integrity periodically

### Troubleshooting

**Problem**: Computations not running
- Check: Environment variables set?
- Check: Scheduler initialized in hooks.server.ts?
- Check: Server logs for errors?

**Problem**: No data in computations
- Check: Holster authenticated?
- Check: Data exists in Holster?
- Check: Run validation endpoint

**Problem**: Slow computations
- Check: Network size?
- Check: Optimize intervals if needed
- Check: Consider job queue for large networks

## Documentation

- **Quick Start**: `COLLECTIVE_SCHEDULER_SETUP.md`
- **Environment Variables**: `COLLECTIVE_ENV_VARS.md`
- **Full Guide**: `README.md`
- **Integration Example**: `example-integration.md`
- **Callback Example**: `callbacks.example.ts`
- **This Summary**: `IMPLEMENTATION_SUMMARY.md`

