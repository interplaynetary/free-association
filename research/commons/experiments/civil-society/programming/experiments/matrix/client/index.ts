/**
 * Local-First Client Module
 * 
 * Exports:
 * - LocalFirstClient (main WebSocket client)
 * - LocalFirstBatchClient (HTTP batch mode client)
 * - PipelinedRpcWrapper (promise pipelining support)
 * - Memoization utilities
 * - PersistentCache
 * - BackgroundSyncManager
 * 
 * Quick Start:
 * ```typescript
 * import { LocalFirstClient } from './client';
 * 
 * const client = new LocalFirstClient(rpcServerStub);
 * await client.initialize();
 * await client.authenticate("alice@example.com", credentials);
 * 
 * // Instant (memoized)
 * const mr = await client.getMutualRecognition("alice", "bob");
 * ```
 * 
 * Promise Pipelining:
 * ```typescript
 * import { createPipelinedClient } from './client';
 * 
 * const pipelinedApi = createPipelinedClient(rpcStub);
 * 
 * // Single round trip!
 * const mr = await pipelinedApi
 *   .authenticate(id, creds)
 *   .getNetworkState()
 *   .computeMutualRecognition("alice", "bob");
 * ```
 * 
 * HTTP Batch Mode:
 * ```typescript
 * import { LocalFirstBatchClient } from './client';
 * 
 * const batch = new LocalFirstBatchClient(serverUrl);
 * const [mr1, mr2] = await Promise.all([
 *   batch.getMutualRecognition("alice", "bob"),
 *   batch.getMutualRecognition("alice", "carol")
 * ]);
 * ```
 */

export {
  LocalFirstClient,
  type LocalFirstClientOptions
} from './local-first-client';

export {
  LocalFirstBatchClient,
  createBatchClient,
  type BatchClientOptions
} from './batch-client';

export {
  PipelinedRpcWrapper,
  createPipelinedClient,
  pipeline,
  PipelineBuilder
} from './pipelined-client';

export {
  memoize,
  createMemoizedMethod,
  LRUCache,
  hashObject,
  createCacheKey,
  type CacheEntry,
  type MemoOptions
} from './memoization';

export {
  PersistentCache,
  type NetworkStateSnapshot,
  type CachedAllocation,
  type CachedComputation
} from './persistent-cache';

export {
  BackgroundSyncManager,
  createSyncOperation,
  type SyncOperation,
  type SyncStatus,
  type SyncEventType,
  type SyncEventHandler
} from './background-sync';

