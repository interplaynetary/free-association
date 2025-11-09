# Collective Recognition Scheduler - Environment Variables

Add these to your `.env.local` file to configure the scheduler.

## Required Variables

```bash
# Membership computation interval (milliseconds)
# Default: 604800000 (7 days)
# How often to recompute capacity membership based on MRD
MEMBERSHIP_COMPUTATION_INTERVAL_MS=604800000

# Allocation computation interval (milliseconds)
# Default: 86400000 (1 day)
# How often to recompute resource allocations
ALLOCATION_COMPUTATION_INTERVAL_MS=86400000
```

## Optional Variables

```bash
# MRD threshold for membership (0-1)
# Default: 0.5
# Participants need this fraction of average mutual recognition to be members
# Lower = more inclusive, Higher = more exclusive
MRD_THRESHOLD=0.5

# Minimum mutual recognition to count (0-100)
# Default: 0.0
# Mutual recognition below this value is ignored
# Set to 1-2 to filter out trivial recognitions
MINIMUM_MUTUAL_RECOGNITION=0.0

# Enable automatic membership updates
# Default: true
AUTO_UPDATE_MEMBERSHIP=true

# Enable automatic allocation computations
# Default: true
AUTO_COMPUTE_ALLOCATIONS=true

# Startup delay before first computation (milliseconds)
# Default: 30000 (30 seconds)
# Gives Holster time to fully initialize
COMPUTATION_STARTUP_DELAY_MS=30000

# Enable detailed logging
# Default: false
# Set to true during development to see detailed computation logs
COLLECTIVE_VERBOSE_LOGGING=false
```

## Example Configurations

### Development (Fast Testing)
```bash
# Run computations every minute for rapid testing
MEMBERSHIP_COMPUTATION_INTERVAL_MS=60000
ALLOCATION_COMPUTATION_INTERVAL_MS=60000
COMPUTATION_STARTUP_DELAY_MS=5000
COLLECTIVE_VERBOSE_LOGGING=true
```

### Production (Conservative)
```bash
# Weekly membership, daily allocations
MEMBERSHIP_COMPUTATION_INTERVAL_MS=604800000
ALLOCATION_COMPUTATION_INTERVAL_MS=86400000
MRD_THRESHOLD=0.5
MINIMUM_MUTUAL_RECOGNITION=1.0
COMPUTATION_STARTUP_DELAY_MS=30000
COLLECTIVE_VERBOSE_LOGGING=false
```

### High-Activity Network
```bash
# Daily membership, hourly allocations
MEMBERSHIP_COMPUTATION_INTERVAL_MS=86400000
ALLOCATION_COMPUTATION_INTERVAL_MS=3600000
MRD_THRESHOLD=0.4
COLLECTIVE_VERBOSE_LOGGING=false
```

## Quick Reference

| Interval | Milliseconds |
|----------|--------------|
| 1 minute | 60000 |
| 5 minutes | 300000 |
| 1 hour | 3600000 |
| 6 hours | 21600000 |
| 1 day | 86400000 |
| 1 week | 604800000 |
| 1 month (30 days) | 2592000000 |

