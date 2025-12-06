/**
 * HTTP Batch Mode Client
 * 
 * Lightweight client for one-time batched queries without WebSocket overhead.
 * 
 * Features:
 * - Single HTTP request for multiple calls
 * - Automatic promise pipelining within batch
 * - Integrated memoization
 * - Simpler than WebSocket for quick queries
 * 
 * Example:
 * ```typescript
 * const batch = new LocalFirstBatchClient(serverUrl);
 * 
 * const mr1 = batch.getMutualRecognition("alice", "bob");
 * const mr2 = batch.getMutualRecognition("alice", "carol");
 * const total = batch.computeTotalMR("alice");
 * 
 * // All execute in single HTTP request
 * const [mr1Result, mr2Result, totalResult] = await Promise.all([mr1, mr2, total]);
 * ```
 */

import { newHttpBatchRpcSession } from 'capnweb';
import type { RpcStub } from 'capnweb';
import type { 
  IParticipantServer, 
  IAuthenticatedParticipant,
  ParticipantId,
  Credential
} from '../protocol';
import { LRUCache, createCacheKey } from './memoization';
import { PersistentCache } from './persistent-cache';

export interface BatchClientOptions {
  /** Enable memoization (default: true) */
  enableMemoization?: boolean;
  
  /** Enable persistent cache (default: true) */
  enablePersistentCache?: boolean;
  
  /** Memoization cache size (default: 500) */
  memoizationSize?: number;
  
  /** Memoization TTL in ms (default: 30000 = 30 seconds) */
  memoizationTTL?: number;
}

/**
 * Local-First Batch Client
 * 
 * Optimized for one-time queries with memoization.
 * Uses HTTP batch mode for lightweight requests.
 */
export class LocalFirstBatchClient {
  private readonly batchStub: RpcStub<IParticipantServer>;
  private readonly cache: LRUCache<string, any>;
  private readonly persistentCache?: PersistentCache;
  private readonly options: Required<BatchClientOptions>;
  
  private session: RpcStub<IAuthenticatedParticipant> | null = null;
  
  constructor(
    private readonly serverUrl: string,
    options: BatchClientOptions = {}
  ) {
    this.options = {
      enableMemoization: options.enableMemoization ?? true,
      enablePersistentCache: options.enablePersistentCache ?? true,
      memoizationSize: options.memoizationSize ?? 500,
      memoizationTTL: options.memoizationTTL ?? 30000
    };
    
    // Create batch RPC session
    this.batchStub = newHttpBatchRpcSession(serverUrl);
    
    // Setup memoization
    this.cache = new LRUCache<string, any>(
      this.options.memoizationSize,
      this.options.memoizationTTL
    );
    
    // Setup persistent cache
    if (this.options.enablePersistentCache) {
      this.persistentCache = new PersistentCache();
    }
  }
  
  /**
   * Initialize (optional - loads persistent cache)
   */
  async initialize(): Promise<void> {
    if (this.persistentCache) {
      await this.persistentCache.initialize();
    }
  }
  
  /**
   * Authenticate
   * 
   * Creates a new batch session for this authentication.
   */
  async authenticate(
    participantId: ParticipantId,
    credentials: Credential
  ): Promise<RpcStub<IAuthenticatedParticipant>> {
    const cacheKey = createCacheKey('auth', participantId);
    
    // Check cache
    if (this.options.enableMemoization && this.cache.has(cacheKey)) {
      console.log(`[BATCH-CACHE-HIT] ${cacheKey}`);
      return this.cache.get(cacheKey)!;
    }
    
    console.log(`[BATCH-CACHE-MISS] ${cacheKey}`);
    
    // Authenticate via batch RPC
    this.session = await this.batchStub.authenticate(participantId, credentials);
    
    // Cache session
    if (this.options.enableMemoization) {
      this.cache.set(cacheKey, this.session);
    }
    
    return this.session;
  }
  
  /**
   * Get Mutual Recognition (Memoized + Batched)
   * 
   * Multiple calls are automatically batched in single HTTP request.
   */
  async getMutualRecognition(idA: string, idB: string): Promise<number> {
    const cacheKey = createCacheKey('mr', idA, idB);
    
    // Check memory cache
    if (this.options.enableMemoization && this.cache.has(cacheKey)) {
      console.log(`[BATCH-CACHE-HIT] ${cacheKey}`);
      return this.cache.get(cacheKey)!;
    }
    
    // Check persistent cache
    if (this.persistentCache) {
      const cached = await this.persistentCache.loadComputation(cacheKey);
      if (cached !== null) {
        console.log(`[BATCH-PERSISTENT-HIT] ${cacheKey}`);
        // Promote to memory cache
        if (this.options.enableMemoization) {
          this.cache.set(cacheKey, cached);
        }
        return cached;
      }
    }
    
    console.log(`[BATCH-CACHE-MISS] ${cacheKey}`);
    
    // Make batched RPC call
    if (!this.session) {
      throw new Error('Not authenticated. Call authenticate() first.');
    }
    
    const network = (this.session as any).getNetworkState();
    const mr = await network.computeMutualRecognition(idA, idB);
    
    // Cache result
    if (this.options.enableMemoization) {
      this.cache.set(cacheKey, mr);
    }
    
    if (this.persistentCache) {
      await this.persistentCache.saveComputation(cacheKey, mr, this.options.memoizationTTL);
    }
    
    return mr;
  }
  
  /**
   * Compute Total MR (Memoized + Batched)
   */
  async computeTotalMR(id: string): Promise<number> {
    const cacheKey = createCacheKey('total-mr', id);
    
    // Check cache
    if (this.options.enableMemoization && this.cache.has(cacheKey)) {
      console.log(`[BATCH-CACHE-HIT] ${cacheKey}`);
      return this.cache.get(cacheKey)!;
    }
    
    console.log(`[BATCH-CACHE-MISS] ${cacheKey}`);
    
    if (!this.session) {
      throw new Error('Not authenticated. Call authenticate() first.');
    }
    
    const network = (this.session as any).getNetworkState();
    const total = await network.computeTotalMR(id);
    
    // Cache result
    if (this.options.enableMemoization) {
      this.cache.set(cacheKey, total);
    }
    
    return total;
  }
  
  /**
   * Compute MRS (Memoized + Batched)
   */
  async computeMRS(idA: string, idB: string): Promise<number> {
    const cacheKey = createCacheKey('mrs', idA, idB);
    
    // Check cache
    if (this.options.enableMemoization && this.cache.has(cacheKey)) {
      console.log(`[BATCH-CACHE-HIT] ${cacheKey}`);
      return this.cache.get(cacheKey)!;
    }
    
    console.log(`[BATCH-CACHE-MISS] ${cacheKey}`);
    
    if (!this.session) {
      throw new Error('Not authenticated. Call authenticate() first.');
    }
    
    const network = (this.session as any).getNetworkState();
    const mrs = await network.computeMRS(idA, idB);
    
    // Cache result
    if (this.options.enableMemoization) {
      this.cache.set(cacheKey, mrs);
    }
    
    return mrs;
  }
  
  /**
   * Get cache statistics
   */
  getCacheStats() {
    return {
      memoization: this.cache.getStats()
    };
  }
  
  /**
   * Clear caches
   */
  clearCache(): void {
    this.cache.clear();
  }
  
  /**
   * Close (cleanup persistent cache)
   */
  close(): void {
    if (this.persistentCache) {
      this.persistentCache.close();
    }
  }
}

/**
 * Create a batch client with automatic authentication
 * 
 * Convenience function for quick setup.
 */
export async function createBatchClient(
  serverUrl: string,
  participantId: ParticipantId,
  credentials: Credential,
  options?: BatchClientOptions
): Promise<LocalFirstBatchClient> {
  const client = new LocalFirstBatchClient(serverUrl, options);
  await client.initialize();
  await client.authenticate(participantId, credentials);
  return client;
}

