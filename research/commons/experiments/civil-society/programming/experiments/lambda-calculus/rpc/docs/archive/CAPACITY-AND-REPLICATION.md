
# Recognition-Based Capacity & Replication

## Overview

This RPC system implements **recognition-based resource allocation**: entities can only consume computational, storage, and bandwidth resources proportional to the mutual recognition between them. This creates a fair, decentralized resource allocation system where higher recognition = more capacity.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    EntitySession (Alice)                     │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌────────────────────┐      ┌────────────────────┐        │
│  │ ComputeRateLimiter │      │  StorageQuotaManager│        │
│  │                    │      │                     │        │
│  │ MR(Bob)   = 0.6    │      │ MR(Bob)   = 0.6     │        │
│  │ Quota     = 600 ops│      │ Quota     = 600 MB  │        │
│  │ Used      = 42 ops │      │ Used      = 12 MB   │        │
│  └────────────────────┘      └────────────────────┘        │
│                                                              │
│  ┌────────────────────┐      ┌────────────────────┐        │
│  │ BandwidthThrottle  │      │ ReplicationManager │        │
│  │                    │      │                     │        │
│  │ MR(Bob)   = 0.6    │      │ Replicating:        │        │
│  │ Quota     = 6 MB/s │      │  - Bob's graph      │        │
│  │ Available = 5.2MB/s│      │  - Charlie's graph  │        │
│  └────────────────────┘      └────────────────────┘        │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

## Capacity Allocation

### Allocation Strategies

```typescript
type AllocationStrategy = 
  | 'proportional'   // Linear: capacity ∝ MR
  | 'quadratic'      // Quadratic: capacity ∝ MR²
  | 'threshold'      // Step: 0 below threshold, full above
  | 'progressive';   // f(x) = x(2-x) - diminishing returns
```

### Example: Proportional Strategy

```
MR = 0.0   →  Quota = 0 ops/sec
MR = 0.25  →  Quota = 250 ops/sec
MR = 0.5   →  Quota = 500 ops/sec  
MR = 0.75  →  Quota = 750 ops/sec
MR = 1.0   →  Quota = 1000 ops/sec
```

### Example: Quadratic Strategy

```
MR = 0.0   →  Quota = 0 ops/sec
MR = 0.5   →  Quota = 250 ops/sec  (0.5² = 0.25)
MR = 0.7   →  Quota = 490 ops/sec  (0.7² = 0.49)
MR = 1.0   →  Quota = 1000 ops/sec
```

Quadratic strategy rewards high recognition exponentially.

## Compute Rate Limiting

### Usage

```typescript
import { ComputeRateLimiter } from '@free-association/lambda-calculus/rpc/capacity';

// Create rate limiter with base quota
const rateLimiter = new ComputeRateLimiter(
  {
    computeOpsPerSecond: 1000,
    storageBytes: 1_000_000_000,        // 1 GB
    bandwidthBytesPerSecond: 10_000_000, // 10 MB/s
    recognitionBasis: 1.0
  },
  'proportional'
);

// Check if Bob can make RPC call
const bobMR = await session.getMutualRecognition('bob');
const { allowed, quota, violation } = await rateLimiter.checkComputeLimit(
  'bob',
  bobMR,
  100  // estimated 100ms operation
);

if (!allowed) {
  console.log('Rate limited:', violation);
  throw new Error('Rate limit exceeded');
}

// Proceed with operation
await expensiveComputation();
```

### Integration with EntitySession

```typescript
class EntitySession {
  private rateLimiter: ComputeRateLimiter;
  
  async handleRemoteCall(fromEntityId: string, method: string, args: any[]) {
    // Get MR with caller
    const mr = await this.getMutualRecognition(fromEntityId);
    
    // Check rate limit
    const { allowed } = await this.rateLimiter.checkComputeLimit(
      fromEntityId,
      mr
    );
    
    if (!allowed) {
      throw new Error('Rate limit exceeded');
    }
    
    // Execute method
    return await this[method](...args);
  }
}
```

## Storage Quota

### Usage

```typescript
import { StorageQuotaManager } from '@free-association/lambda-calculus/rpc/capacity';

const storageQuota = new StorageQuotaManager(baseQuota, 'proportional');

// Check if Bob can replicate data
const { allowed, quota } = await storageQuota.checkStorageLimit(
  'bob',
  bobMR,
  5_000_000  // 5 MB
);

if (allowed) {
  // Store the data
  await storage.saveSparseGraph(bobGraph);
  storageQuota.recordStorage('bob', 5_000_000, 1);
}
```

### Replication with Storage Limits

```typescript
// Select what to replicate based on MRS and available storage
const mrs = await session.getMRS(universe);
const availableStorage = storageQuota.getUsage('bob')?.quota || 0;

const replicationTargets = replicationManager.selectReplicationTargets(
  mrs,
  availableStorage
);

// Replicate only from high-MRS entities
for (const targetId of replicationTargets) {
  const graph = await remoteSession.getFullGraph();
  await replicationManager.replicateFrom(
    targetId,
    graph,
    vectorClock,
    availableStorage
  );
}
```

## Bandwidth Throttling

Uses **token bucket algorithm** for smooth rate limiting:

```typescript
import { BandwidthThrottle } from '@free-association/lambda-calculus/rpc/capacity';

const bandwidth = new BandwidthThrottle(baseQuota, 'proportional');

// Before sending data
const messageSize = JSON.stringify(message).length;
const { allowed } = await bandwidth.checkOutgoingLimit(
  'bob',
  bobMR,
  messageSize
);

if (allowed) {
  await transport.send(message);
} else {
  // Queue or delay
  await delay(100);
  // Retry
}
```

### Token Bucket Characteristics

- **Capacity**: Max burst size (bytes)
- **Refill Rate**: Bytes per second
- **Smooth**: Allows bursts but maintains average rate

Example: `capacity = 10MB, refillRate = 1MB/s`
- Can burst up to 10MB immediately
- Then limited to 1MB/s
- Tokens refill at 1MB/s up to 10MB max

## Replication System

### Selective Replication

```typescript
import { ReplicationManager } from '@free-association/lambda-calculus/rpc/replication';

// Create replication manager
const replicationManager = new ReplicationManager(
  'alice',
  {
    replicateFrom: new Set(),
    minMRS: 0.3,           // Only replicate if MRS >= 0.3
    maxReplicas: 10,        // Max 10 replicas
    strategy: 'highest-mrs' // Prioritize high MRS
  },
  'selective'              // Filter by threshold
);

// Get MRS to decide what to replicate
const mrs = await session.getMRS(universe);

// Select replication targets
const targets = replicationManager.selectReplicationTargets(
  mrs,
  storageQuota
);

console.log('Replicating from:', targets);
// Output: ['bob', 'charlie', 'diana'] - highest MRS entities
```

### Replication Strategies

```typescript
type ReplicationStrategy = 
  | 'full'       // Replicate entire graph
  | 'partial'    // Only edges above threshold (e.g., > 0.3)
  | 'selective'; // Based on policy
```

### Sync Strategies

```typescript
import { SyncCoordinator } from '@free-association/lambda-calculus/rpc/replication';

const syncCoordinator = new SyncCoordinator({
  mode: 'hybrid',          // Pull + push
  interval: 5000,          // Sync every 5 seconds
  batchSize: 10,           // Max 10 entities per sync
  priority: 'recognition'  // Sync high-MRS first
});

// Get entities that need sync
const syncQueue = syncCoordinator.getSyncQueue(mrsMap);

// Sync in priority order
for (const entityId of syncQueue) {
  await syncEntity(entityId);
  syncCoordinator.markSynced(entityId);
}
```

### Conflict Resolution

```typescript
import { ConflictResolver } from '@free-association/lambda-calculus/rpc/replication';

const resolver = new ConflictResolver();

// Check if update conflicts
const result = resolver.resolveVectorClock(localClock, remoteClock);

switch (result) {
  case 'local':
    // Local is newer - keep local
    break;
    
  case 'remote':
    // Remote is newer - accept remote
    await applyRemoteUpdate(update);
    break;
    
  case 'concurrent':
    // Concurrent updates - merge
    const merged = resolver.mergeConcurrent(
      localValue,
      remoteValue,
      'max'  // Take maximum (favor higher recognition)
    );
    await applyMergedValue(merged);
    break;
}
```

## Complete Example

```typescript
import { 
  createP2PConnection,
  ComputeRateLimiter,
  StorageQuotaManager,
  BandwidthThrottle,
  ReplicationManager,
  SyncCoordinator
} from '@free-association/lambda-calculus/rpc';

// Alice sets up with capacity management
const alice = await createP2PConnection('alice');

const rateLimiter = new ComputeRateLimiter(baseQuota, 'progressive');
const storageQuota = new StorageQuotaManager(baseQuota, 'progressive');
const bandwidth = new BandwidthThrottle(baseQuota, 'progressive');

const replicationManager = new ReplicationManager('alice', {
  replicateFrom: new Set(),
  minMRS: 0.3,
  maxReplicas: 10,
  strategy: 'highest-mrs'
}, 'selective');

// Bob connects
const bob = await createP2PConnection('bob');

// Authenticate
await alice.mutualAuthenticate(aliceProof);
await bob.mutualAuthenticate(bobProof);

// Bob tries to make expensive RPC to Alice
const bobMR = 0.6; // 60% mutual recognition

// Alice checks rate limit
const { allowed } = await rateLimiter.checkComputeLimit('bob', bobMR, 500);

if (allowed) {
  // Execute expensive operation
  const result = await alice.getLocalSession().getMRS(universe);
  
  // Check bandwidth before sending
  const size = estimateSize(result);
  const { allowed: canSend } = await bandwidth.checkOutgoingLimit(
    'bob',
    bobMR,
    size
  );
  
  if (canSend) {
    await sendResult(result);
  }
} else {
  throw new Error('Rate limited - increase mutual recognition for more capacity');
}

// Bob replicates Alice's graph
const { allowed: canReplicate } = await storageQuota.checkStorageLimit(
  'alice',
  bobMR,
  5_000_000
);

if (canReplicate) {
  const aliceGraph = await alice.getRemoteSession().getFullGraph();
  await replicationManager.replicateFrom(
    'alice',
    aliceGraph,
    vectorClock,
    5_000_000
  );
}
```

## Performance Impact

### Without Capacity Management

```
Bob (MR=0.1) makes 1000 RPC calls/sec → Alice overwhelmed
Charlie (MR=0.9) makes 10 RPC calls/sec → Underutilized
```

### With Capacity Management

```
Bob (MR=0.1) quota: 100 ops/sec   → Rate limited after 100
Charlie (MR=0.9) quota: 900 ops/sec → Can use more capacity
```

Fair resource allocation based on recognition!

## Benefits

1. **Fair Resource Allocation**: Higher recognition = more capacity
2. **DoS Protection**: Limits abuse from low-recognition entities
3. **Incentivizes Recognition**: Entities increase recognition to get more resources
4. **Decentralized**: No central authority needed
5. **Adaptive**: Quotas adjust as recognition changes
6. **Efficient Replication**: Only replicate high-value data

## See Also

- [RPC README](./README.md) - Main RPC documentation
- [Examples](./examples/) - Working examples
- [Tests](./capacity/__tests__/) - Capacity tests

