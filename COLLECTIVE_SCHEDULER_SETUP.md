# Collective Recognition & Membership Scheduler Setup

This document provides a quick reference for the server-side scheduled computation system.

## 🎯 What This Does

Runs automated computations on the SvelteKit server at defined intervals:

1. **Membership Computation** (Weekly by default)
   - Updates capacity membership based on Mutual Recognition Density (MRD)
   - Adds participants who meet threshold
   - Removes those who fall below threshold

2. **Allocation Computation** (Daily by default)
   - Calculates resource allocations based on collective recognition
   - Matches availability slots to need slots
   - Applies compliance filters
   - Generates final allocations

## 📁 File Structure

```
src/
├── lib/
│   ├── protocol/collective/
│   │   ├── collective-membership.svelte.ts      # ✅ Shared (client + server)
│   │   ├── collective-recognition.svelte.ts     # ✅ Shared (client + server)
│   │   └── schemas.ts                           # ✅ Shared (client + server)
│   │
│   └── server/collective/                       # 🔒 Server-only
│       ├── index.ts                             # Public API exports
│       ├── config.ts                            # Environment configuration
│       ├── scheduler.ts                         # Scheduling logic
│       ├── callbacks.example.ts                 # Example implementation
│       ├── example-integration.md               # Complete integration guide
│       └── README.md                            # Full documentation
│
├── routes/api/collective/
│   ├── status/+server.ts                        # GET /api/collective/status
│   ├── trigger-membership/+server.ts            # POST /api/collective/trigger-membership
│   └── trigger-allocation/+server.ts            # POST /api/collective/trigger-allocation
│
└── hooks.server.ts                              # Scheduler initialization (commented out by default)
```

## ✅ Status: READY TO USE

**TWO** schedulers are **fully implemented and enabled** in your codebase:

### 1. Collective Recognition & Membership Scheduler
- ✅ Callbacks implemented using Holster database utilities
- ✅ Scheduler initialized in `hooks.server.ts`
- ✅ API endpoints available for status and manual triggers
- ✅ Validation endpoint for testing

### 2. Collective Tree Scheduler (NEW!)
- ✅ Tree merging (combine contributor trees → collective trees)
- ✅ Collective recognition computation
- ✅ Capacity allocation across tree nodes
- ✅ Fully integrated with Holster database
- ✅ Separate API endpoints and configuration

## 🚀 Quick Setup (2 Steps)

### Step 1: Configure Environment

Add to `.env.local` (see `COLLECTIVE_ENV_VARS.md` for full reference):

```bash
# Run membership computation weekly
MEMBERSHIP_COMPUTATION_INTERVAL_MS=604800000

# Run allocation computation daily
ALLOCATION_COMPUTATION_INTERVAL_MS=86400000

# MRD threshold (0.5 = need 50% of average recognition)
MRD_THRESHOLD=0.5

# Enable features
AUTO_UPDATE_MEMBERSHIP=true
AUTO_COMPUTE_ALLOCATIONS=true

# Enable logging for development
COLLECTIVE_VERBOSE_LOGGING=true
```

### Step 2: Test the Setup

Validate callbacks are working:

```bash
curl http://localhost:3000/api/collective/validate
```

Expected response:
```json
{
  "success": true,
  "results": {
    "recognitionData": { "count": 42, "sample": [...] },
    "autoUpdateCapacities": { "count": 3, "ids": [...] },
    "needs": { "count": 12, "declarers": [...] }
  },
  "message": "All callbacks validated successfully"
}
```

If validation fails, check:
- Holster is initialized and authenticated
- Data exists in your Holster database
- Console logs for specific errors

## 📊 API Endpoints

### Collective Recognition & Membership

```bash
GET  /api/collective/validate                  # Validate setup
GET  /api/collective/status                    # Check scheduler status
POST /api/collective/trigger-membership        # Manual membership computation
POST /api/collective/trigger-allocation        # Manual allocation computation
```

### Collective Tree Operations (NEW!)

```bash
GET  /api/collective-tree/validate             # Validate tree callbacks
GET  /api/collective-tree/status               # Check tree scheduler status
POST /api/collective-tree/trigger-merge        # Manual tree merge
POST /api/collective-tree/trigger-recognition  # Manual collective recognition
POST /api/collective-tree/trigger-allocation   # Manual capacity allocation
```

### Example Usage

```bash
# Check both schedulers
curl http://localhost:3000/api/collective/status
curl http://localhost:3000/api/collective-tree/status

# Validate both systems
curl http://localhost:3000/api/collective/validate
curl http://localhost:3000/api/collective-tree/validate

# Trigger tree merge manually
curl -X POST http://localhost:3000/api/collective-tree/trigger-merge
```

### Monitor Logs

Watch server console for computation events:

```
[COLLECTIVE-SCHEDULER] 🚀 Starting scheduler...
  → Membership computation: every 7.0 days
  → Allocation computation: every 1.0 days

[COLLECTIVE-SCHEDULER] ✅ Membership computation completed in 234ms
  → Processed 3 capacities
  → Updated 1 capacities
  → Added 2 members, removed 0 members
```

## 🔧 Configuration Options

| Environment Variable | Default | Description |
|---------------------|---------|-------------|
| `MEMBERSHIP_COMPUTATION_INTERVAL_MS` | 604800000 (7 days) | Membership update frequency |
| `ALLOCATION_COMPUTATION_INTERVAL_MS` | 86400000 (1 day) | Allocation update frequency |
| `MRD_THRESHOLD` | 0.5 | Minimum MRD for membership (0-1) |
| `MINIMUM_MUTUAL_RECOGNITION` | 0.0 | Minimum mutual recognition filter (0-100) |
| `AUTO_UPDATE_MEMBERSHIP` | true | Enable automatic membership updates |
| `AUTO_COMPUTE_ALLOCATIONS` | true | Enable automatic allocation computations |
| `COMPUTATION_STARTUP_DELAY_MS` | 30000 (30s) | Delay before first computation |
| `COLLECTIVE_VERBOSE_LOGGING` | false | Enable detailed logging |

## 🎓 Key Concepts

### Shared Modules Architecture

The `.svelte.ts` files containing the computation logic can be imported by **both** client and server:

- **Frontend**: Import and use for real-time UI updates
- **Backend**: Import and use for scheduled batch computations

This ensures consistency - the same algorithms run everywhere.

### Two Complementary Schedulers

**1. Collective Recognition & Membership** (`$lib/server/collective/`)
   - **Membership**: Who is in the network? (MRD-based)
   - **Allocation**: How much does each member receive? (recognition shares)
   - Focus: **Resource allocation to members**

**2. Collective Tree Operations** (`$lib/server/collective-tree/`)
   - **Tree Merging**: Combine contributor trees → collective trees
   - **Collective Recognition**: Recognition flows through tree structure
   - **Capacity Allocation**: Distribute capacities across tree nodes
   - Focus: **Tree structure and collective intelligence**

Both run independently but use related mathematical principles.

### Server-Only Scheduler

The scheduler infrastructure lives in `$lib/server/` which is:
- Only available on the server
- Never bundled in client code
- Handles scheduling, data fetching, and persistence

### Callback Pattern

You provide callbacks to fetch/save data from your specific database/Holster setup. This makes the scheduler agnostic to your data layer.

## 📚 Documentation

- **Full Guide**: `src/lib/server/collective/README.md`
- **Integration Example**: `src/lib/server/collective/example-integration.md`
- **Callback Example**: `src/lib/server/collective/callbacks.example.ts`

## 🐛 Troubleshooting

### Scheduler not running

1. Check environment variables are set
2. Verify callbacks are implemented
3. Look for errors in server console
4. Ensure Holster is initialized before scheduler

### Linter errors about imports

The linter might show cached errors. Try:
```bash
source ~/.bashrc && bun run check
```

If errors persist, ensure you're importing from the correct modules:
- `computeAllocations` → from `collective-recognition.svelte.ts`
- `MRDMembershipModule` → from `collective-membership.svelte.ts`
- Types → from `schemas.ts`

### No data in computations

1. Check callbacks return actual data
2. Verify Holster paths are correct
3. Enable verbose logging to see what's fetched
4. Test callbacks independently

## 🎯 Next Steps

1. ✅ Review `example-integration.md` for complete implementation
2. ✅ Implement callbacks based on your data layer
3. ✅ Test with manual triggers first
4. ✅ Enable scheduler in hooks.server.ts
5. ✅ Monitor logs for first few computations
6. ✅ Adjust intervals based on your needs
7. ✅ Add authentication to manual trigger endpoints
8. ✅ Set up monitoring/alerts for production

## 💡 Production Tips

- Start with **longer intervals** (weekly membership, daily allocations)
- Enable **verbose logging** initially, disable in production
- Add **authentication** to manual trigger endpoints
- Monitor **computation times** and optimize if needed
- Store **computation history** for audit trails
- Set up **alerts** for failed computations
- Test thoroughly in **staging** with production-like data

---

**Questions?** Check the full documentation in `src/lib/server/collective/README.md`

