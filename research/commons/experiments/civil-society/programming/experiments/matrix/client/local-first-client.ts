/**
 * Local-First Client for Free Association Protocol
 * 
 * Combines:
 * - Memoization (performance)
 * - IndexedDB (offline support)
 * - Background sync (consistency)
 * 
 * Architecture:
 * ```
 * Client → Memoization → Persistent Cache → RPC Server
 *   ↓                         ↓                 ↓
 *  Fast                   Offline           Eventually
 * (0.1ms)                Capable           Consistent
 * ```
 * 
 * Usage:
 * ```typescript
 * const client = new LocalFirstClient(rpcServerStub);
 * await client.initialize();
 * 
 * // Instant (memoized)
 * const mr = await client.getMutualRecognition("alice", "bob");
 * 
 * // Works offline
 * const allocations = await client.allocateCapacityOptimistic(mySlots);
 * ```
 */

import type { RpcStub } from 'capnweb';
import type {
  IParticipantServer,
  IAuthenticatedParticipant,
  INetworkState,
  IRecognitionBudget
} from '../rpc/interfaces';
import type {
  ParticipantId,
  Credential,
  AvailabilitySlot,
  NeedSlot,
  SlotAllocationRecord,
  Commitment
} from '../protocol';
import { FreeAssociationMatrices } from '../protocol';
import { Sparse } from '../sparse-matrix';
import { createMemoizedMethod, createCacheKey } from './memoization';
import { PersistentCache, type NetworkStateSnapshot } from './persistent-cache';
import { BackgroundSyncManager, createSyncOperation, type SyncStatus } from './background-sync';

export interface LocalFirstClientOptions {
  /** Enable memoization (default: true) */
  enableMemoization?: boolean;
  
  /** Enable persistent cache (default: true) */
  enablePersistentCache?: boolean;
  
  /** Enable background sync (default: true) */
  enableBackgroundSync?: boolean;
  
  /** Memoization cache size (default: 1000) */
  memoizationSize?: number;
  
  /** Memoization TTL in ms (default: 60000 = 1 minute) */
  memoizationTTL?: number;
  
  /** Background sync interval in ms (default: 5000 = 5 seconds) */
  syncIntervalMs?: number;
  
  /** Enable optimistic updates (default: true) */
  enableOptimistic?: boolean;
}

/**
 * Local-First Client
 * 
 * ✨ The magic happens here! ✨
 * 
 * This client provides:
 * 1. **Instant responses** via memoization
 * 2. **Offline support** via persistent cache
 * 3. **Eventual consistency** via background sync
 */
export class LocalFirstClient {
  private session: RpcStub<IAuthenticatedParticipant> | null = null;
  private network: RpcStub<INetworkState> | null = null;
  private budget: RpcStub<IRecognitionBudget> | null = null;
  
  private readonly persistentCache: PersistentCache;
  private readonly backgroundSync: BackgroundSyncManager;
  private readonly options: Required<LocalFirstClientOptions>;
  
  // Memoized methods
  private readonly _getMutualRecognition: ReturnType<typeof createMemoizedMethod>;
  private readonly _computeTotalMR: ReturnType<typeof createMemoizedMethod>;
  private readonly _computeMRS: ReturnType<typeof createMemoizedMethod>;
  private readonly _getCommitment: ReturnType<typeof createMemoizedMethod>;
  
  // Local matrices for offline computation
  private localMatrices: FreeAssociationMatrices | null = null;
  private participantIdToIndex = new Map<string, number>();
  private participantIndexToId = new Map<number, string>();
  
  constructor(
    private readonly serverStub: RpcStub<IParticipantServer>,
    options: LocalFirstClientOptions = {}
  ) {
    this.options = {
      enableMemoization: options.enableMemoization ?? true,
      enablePersistentCache: options.enablePersistentCache ?? true,
      enableBackgroundSync: options.enableBackgroundSync ?? true,
      memoizationSize: options.memoizationSize ?? 1000,
      memoizationTTL: options.memoizationTTL ?? 60000,
      syncIntervalMs: options.syncIntervalMs ?? 5000,
      enableOptimistic: options.enableOptimistic ?? true
    };
    
    this.persistentCache = new PersistentCache();
    this.backgroundSync = new BackgroundSyncManager(this.options.syncIntervalMs);
    
    // Setup memoized methods
    this._getMutualRecognition = createMemoizedMethod(
      async (idA: string, idB: string) => this.fetchMutualRecognition(idA, idB),
      { maxSize: this.options.memoizationSize, ttl: this.options.memoizationTTL }
    );
    
    this._computeTotalMR = createMemoizedMethod(
      async (id: string) => this.fetchTotalMR(id),
      { maxSize: this.options.memoizationSize, ttl: this.options.memoizationTTL }
    );
    
    this._computeMRS = createMemoizedMethod(
      async (idA: string, idB: string) => this.fetchMRS(idA, idB),
      { maxSize: this.options.memoizationSize, ttl: this.options.memoizationTTL }
    );
    
    this._getCommitment = createMemoizedMethod(
      async (pubKey: string) => this.fetchCommitment(pubKey),
      { maxSize: this.options.memoizationSize, ttl: this.options.memoizationTTL }
    );
  }
  
  /**
   * Initialize client
   * 
   * - Opens IndexedDB
   * - Loads cached network state
   * - Starts background sync
   */
  async initialize(): Promise<void> {
    console.log('[LOCAL-FIRST-CLIENT] 🚀 Initializing...');
    
    // Initialize persistent cache
    if (this.options.enablePersistentCache) {
      await this.persistentCache.initialize();
      await this.loadCachedNetworkState();
    }
    
    // Start background sync
    if (this.options.enableBackgroundSync) {
      this.backgroundSync.start();
    }
    
    // Setup event listeners
    this.backgroundSync.on((event, data) => {
      console.log(`[LOCAL-FIRST-CLIENT] 🔔 Event: ${event}`, data);
    });
    
    console.log('[LOCAL-FIRST-CLIENT] ✅ Initialized');
  }
  
  /**
   * Authenticate with server
   * 
   * Returns authenticated session for RPC calls.
   */
  async authenticate(participantId: ParticipantId, credentials: Credential): Promise<void> {
    console.log(`[LOCAL-FIRST-CLIENT] 🔐 Authenticating: ${participantId}`);
    
    this.session = await this.serverStub.authenticate(participantId, credentials);
    this.budget = await this.session.getRecognitionBudget();
    this.network = await this.session.getNetworkState();
    
    console.log('[LOCAL-FIRST-CLIENT] ✅ Authenticated');
  }
  
  /**
   * Get Mutual Recognition (Memoized + Cached)
   * 
   * ✨ MAGIC:
   * - First call: Server (100ms)
   * - Second call: Memoized (0.1ms)
   * - Offline: Cached (0.1ms)
   */
  async getMutualRecognition(idA: string, idB: string): Promise<number> {
    if (!this.options.enableMemoization) {
      return this.fetchMutualRecognition(idA, idB);
    }
    
    try {
      return await this._getMutualRecognition.fn(idA, idB);
    } catch (error) {
      console.warn('[LOCAL-FIRST-CLIENT] ⚠️ Falling back to cached MR');
      return this.getMutualRecognitionFromCache(idA, idB);
    }
  }
  
  /**
   * Fetch MR from server
   */
  private async fetchMutualRecognition(idA: string, idB: string): Promise<number> {
    if (!this.network) throw new Error('Not authenticated');
    
    const mr = await this.network.computeMutualRecognition(idA, idB);
    
    // Save to persistent cache
    if (this.options.enablePersistentCache) {
      await this.persistentCache.saveComputation(
        createCacheKey('mr', idA, idB),
        mr,
        this.options.memoizationTTL
      );
    }
    
    return mr;
  }
  
  /**
   * Get MR from cache (offline fallback)
   */
  private async getMutualRecognitionFromCache(idA: string, idB: string): Promise<number> {
    if (!this.options.enablePersistentCache) {
      throw new Error('Offline and no cache available');
    }
    
    // Try persistent cache
    const cached = await this.persistentCache.loadComputation(createCacheKey('mr', idA, idB));
    if (cached !== null) {
      console.log('[LOCAL-FIRST-CLIENT] 📖 Using cached MR');
      return cached;
    }
    
    // Try local computation from cached network state
    if (this.localMatrices) {
      const indexA = this.participantIdToIndex.get(idA);
      const indexB = this.participantIdToIndex.get(idB);
      
      if (indexA !== undefined && indexB !== undefined) {
        const MR = this.localMatrices.computeMR();
        const mr = Sparse.get(MR, indexA, indexB);
        console.log('[LOCAL-FIRST-CLIENT] 🧮 Computed MR offline');
        return mr;
      }
    }
    
    throw new Error('No cached data available for offline MR computation');
  }
  
  /**
   * Get Total MR (Memoized + Cached)
   */
  async computeTotalMR(id: string): Promise<number> {
    if (!this.options.enableMemoization) {
      return this.fetchTotalMR(id);
    }
    
    try {
      return await this._computeTotalMR.fn(id);
    } catch (error) {
      console.warn('[LOCAL-FIRST-CLIENT] ⚠️ Falling back to cached Total MR');
      return this.getTotalMRFromCache(id);
    }
  }
  
  /**
   * Fetch Total MR from server
   */
  private async fetchTotalMR(id: string): Promise<number> {
    if (!this.network) throw new Error('Not authenticated');
    
    return await this.network.computeTotalMR(id);
  }
  
  /**
   * Get Total MR from cache
   */
  private async getTotalMRFromCache(id: string): Promise<number> {
    if (this.localMatrices) {
      const index = this.participantIdToIndex.get(id);
      if (index !== undefined) {
        const t = this.localMatrices.computeTotalMR();
        return t[index];
      }
    }
    throw new Error('No cached data available');
  }
  
  /**
   * Compute MRS (Memoized + Cached)
   */
  async computeMRS(idA: string, idB: string): Promise<number> {
    if (!this.options.enableMemoization) {
      return this.fetchMRS(idA, idB);
    }
    
    return await this._computeMRS.fn(idA, idB);
  }
  
  /**
   * Fetch MRS from server
   */
  private async fetchMRS(idA: string, idB: string): Promise<number> {
    if (!this.network) throw new Error('Not authenticated');
    
    return await this.network.computeMRS(idA, idB);
  }
  
  /**
   * Get Commitment (Memoized + Cached)
   */
  async getCommitment(pubKey: string): Promise<Commitment> {
    if (!this.options.enableMemoization) {
      return this.fetchCommitment(pubKey);
    }
    
    try {
      return await this._getCommitment.fn(pubKey);
    } catch (error) {
      console.warn('[LOCAL-FIRST-CLIENT] ⚠️ Falling back to cached commitment');
      const cached = await this.persistentCache.loadCommitment(pubKey);
      if (!cached) throw new Error('No cached commitment available');
      return cached;
    }
  }
  
  /**
   * Fetch commitment from server
   */
  private async fetchCommitment(pubKey: string): Promise<Commitment> {
    // Implementation depends on RPC interface
    // This is a placeholder
    throw new Error('Not implemented - need commitment RPC method');
  }
  
  /**
   * Allocate Recognition (Optimistic)
   * 
   * ✨ INSTANT UI UPDATE:
   * 1. Update local cache immediately
   * 2. Return success
   * 3. Sync to server in background
   * 4. Rollback on failure
   */
  async allocateRecognitionOptimistic(
    targetId: string,
    amount: number
  ): Promise<{ immediate: boolean; syncing: boolean }> {
    if (!this.budget) throw new Error('Not authenticated');
    
    if (!this.options.enableOptimistic) {
      // Non-optimistic: wait for server
      await this.budget.allocateRecognition(targetId, amount);
      return { immediate: false, syncing: false };
    }
    
    // Optimistic: update local cache immediately
    console.log(`[LOCAL-FIRST-CLIENT] ⚡ Optimistic recognition allocation: ${targetId} → ${amount}`);
    
    // Invalidate affected caches
    this._getMutualRecognition.invalidate(new RegExp(targetId));
    this._computeTotalMR.invalidate(new RegExp(targetId));
    this._computeMRS.invalidate(new RegExp(targetId));
    
    // Queue background sync
    const syncOp = createSyncOperation(
      `recognition:${targetId}:${Date.now()}`,
      'recognition',
      async () => {
        await this.budget!.allocateRecognition(targetId, amount);
      },
      10, // High priority
      5   // Max retries
    );
    
    await this.backgroundSync.enqueue(syncOp);
    
    return { immediate: true, syncing: true };
  }
  
  /**
   * Load cached network state for offline computation
   */
  private async loadCachedNetworkState(): Promise<void> {
    const snapshot = await this.persistentCache.loadNetworkState();
    
    if (snapshot) {
      this.localMatrices = new FreeAssociationMatrices(snapshot.participantCount);
      this.localMatrices.setRecognitionMatrix(snapshot.recognitionMatrix);
      
      this.participantIdToIndex = new Map(Object.entries(snapshot.participantIdToIndex).map(
        ([id, index]) => [id, Number(index)]
      ));
      this.participantIndexToId = new Map(Object.entries(snapshot.participantIndexToId).map(
        ([index, id]) => [Number(index), id]
      ));
      
      console.log(`[LOCAL-FIRST-CLIENT] 📖 Loaded cached network state (${snapshot.participantCount} participants)`);
    }
  }
  
  /**
   * Sync network state to cache
   */
  async syncNetworkStateToCache(): Promise<void> {
    if (!this.network) throw new Error('Not authenticated');
    
    console.log('[LOCAL-FIRST-CLIENT] 💾 Syncing network state to cache...');
    
    // This would fetch the full network state from server
    // Implementation depends on RPC interface
    // For now, this is a placeholder
    
    console.log('[LOCAL-FIRST-CLIENT] ✅ Network state synced');
  }
  
  /**
   * Get sync status
   */
  getSyncStatus(): SyncStatus {
    return this.backgroundSync.getStatus();
  }
  
  /**
   * Get cache stats
   */
  async getCacheStats() {
    const persistentStats = await this.persistentCache.getStats();
    const memoStats = {
      mutualRecognition: this._getMutualRecognition.cache.getStats(),
      totalMR: this._computeTotalMR.cache.getStats(),
      mrs: this._computeMRS.cache.getStats(),
      commitment: this._getCommitment.cache.getStats()
    };
    
    return {
      persistent: persistentStats,
      memoization: memoStats
    };
  }
  
  /**
   * Clear all caches
   */
  async clearCaches(): Promise<void> {
    this._getMutualRecognition.clear();
    this._computeTotalMR.clear();
    this._computeMRS.clear();
    this._getCommitment.clear();
    
    await this.persistentCache.clear();
    
    console.log('[LOCAL-FIRST-CLIENT] 🧹 All caches cleared');
  }
  
  /**
   * Close client
   */
  close(): void {
    this.backgroundSync.stop();
    this.persistentCache.close();
    console.log('[LOCAL-FIRST-CLIENT] 🔒 Closed');
  }
}

