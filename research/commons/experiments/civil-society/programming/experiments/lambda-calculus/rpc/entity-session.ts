/**
 * Entity Session - Symmetric RPC Peer
 * 
 * Cap'n Web Style: Extends RpcTarget, auto-initializes!
 * 
 * Core RPC target representing an authenticated entity session.
 * Symmetric protocol - both sides export EntitySession at ID 0.
 * 
 * Features:
 * - Identity verification
 * - Recognition operations (can only modify own allocations)
 * - Query operations (anyone can query)
 * - Sync operations (CRDT-style updates)
 * - Sparse operations throughout
 * - Auto-initialization of storage/cache
 * - RpcTarget pattern for Cap'n Web compatibility
 * 
 * Usage:
 * ```typescript
 * const session = new EntitySession('alice');
 * await session.initialize();
 * ```
 */

import { RpcTarget } from './rpc-target';
import type { EntityAPI, EntityFullAPI } from './api';
import { BrowserStorage } from './browser-storage';
import { RecognitionCache } from './cache';
import type {
  Credential,
  EntityId,
  SyncUpdate,
  SyncCallback,
  SyncOperation,
  SerializedSparseGraph,
  ITCStamp
} from './types';
import { ITClock } from './clock';
import { serializeSparseGraph, mrCacheKey, tmrCacheKey, mrsCacheKey, mrdCacheKey } from './serialization';
import { sparseMutual, sparseTMR, sparseMRS, sparseMRD, checkBudgetConstraint, type Distribution } from '../src/sparse/operations';
import { BudgetConstraintError, AuthenticationError } from './types';
import { SecureContext } from './security/secure-context';
import { SecureStorage } from './security/secure-storage';
import type { KeyPair } from './identity/keypair';

/**
 * Entity Session Configuration (optional - auto-initializes if not provided)
 */
export interface EntitySessionConfig {
  entityId: string;
  storage?: BrowserStorage;
  cache?: RecognitionCache;
  maxAllocation?: number;    // Default 1.0 (budget constraint)
  autoSync?: boolean;         // Auto-sync changes to peers
}

/**
 * Entity Session - RpcTarget with auto-initialization
 * 
 * Implements EntityFullAPI for full type safety.
 * Extends RpcTarget for Cap'n Web compatibility.
 * Both parties in a connection export their EntitySession.
 */
export class EntitySession extends RpcTarget implements EntityFullAPI {
  private readonly entityId: string;
  private readonly storage: BrowserStorage;
  private readonly cache: RecognitionCache;
  private readonly maxAllocation: number;
  private readonly syncCallbacks: Set<SyncCallback> = new Set();
  private authenticated: boolean = false;
  private initPromise?: Promise<void>;
  private initialized: boolean = false;
  
  // Security layer (optional - for signed updates)
  private secureContext?: SecureContext;
  private secureStorage?: SecureStorage;

  /**
   * Create entity session with auto-initialization
   * 
   * @param entityIdOrConfig - Entity ID string or full config
   * @param storage - Optional storage (auto-created if not provided)
   * @param cache - Optional cache (auto-created if not provided)
   * 
   * @example
   * ```typescript
   * // Simple auto-init
   * const session = new EntitySession('alice');
   * await session.initialize();
   * 
   * // With custom storage/cache
   * const session = new EntitySession('alice', storage, cache);
   * await session.initialize();
   * 
   * // Old config style (still works)
   * const session = new EntitySession({ entityId: 'alice', storage });
   * ```
   */
  constructor(
    entityIdOrConfig: EntityId | EntitySessionConfig,
    storage?: BrowserStorage,
    cache?: RecognitionCache
  ) {
    super(); // RpcTarget marker

    // Handle both signatures
    if (typeof entityIdOrConfig === 'string') {
      this.entityId = entityIdOrConfig;
      this.storage = storage || new BrowserStorage(entityIdOrConfig);
      this.cache = cache || new RecognitionCache();
      this.maxAllocation = 1.0;
    } else {
      this.entityId = entityIdOrConfig.entityId;
      this.storage = entityIdOrConfig.storage || new BrowserStorage(entityIdOrConfig.entityId);
      this.cache = entityIdOrConfig.cache || new RecognitionCache();
      this.maxAllocation = entityIdOrConfig.maxAllocation || 1.0;
    }
  }

  /**
   * Initialize storage (call this before using)
   * 
   * Note: This is now optional! Methods will auto-initialize on first call.
   */
  async initialize(): Promise<void> {
    await this.ensureInitialized();
  }

  /**
   * Ensure storage is initialized (lazy initialization)
   * @private
   */
  private async ensureInitialized(): Promise<void> {
    if (this.initialized) return;
    
    if (!this.initPromise) {
      this.initPromise = this.storage.initialize().then(() => {
        this.initialized = true;
      });
    }
    
    await this.initPromise;
  }

  /**
   * Initialize session with keypair for security
   * 
   * This enables automatic signing/verification of all state updates.
   * Call this after construction for secure sessions.
   * 
   * @example
   * const session = new EntitySession('alice');
   * await session.initializeWithKeypair(keypair);
   * // Now all allocateRecognition() calls are automatically signed!
   */
  async initializeWithKeypair(keypair: KeyPair): Promise<void> {
    // Create secure context
    this.secureContext = await SecureContext.create(keypair, this.entityId);
    
    // Create secure storage
    this.secureStorage = new SecureStorage(this.entityId, this.secureContext);
    await this.secureStorage.initialize();
    
    // Initialize regular storage too
    await this.ensureInitialized();
  }

  // ============================================================================
  // Identity & Authentication (Symmetric)
  // ============================================================================

  /**
   * Verify identity proof
   * Called by remote peer to verify this entity's identity
   * 
   * @returns true if proof is valid, false otherwise
   */
  async verifyIdentity(proof: Credential): Promise<boolean> {
    // In a real implementation, this would:
    // - Verify signatures for pubkey credentials
    // - Check DID proofs for did credentials
    // - Validate OAuth tokens
    // - Check password hashes
    
    // For now, basic validation
    switch (proof.type) {
      case 'pubkey':
        // TODO: Implement Ed25519/secp256k1 signature verification
        // For now, just check format
        return (
          proof.publicKey.length > 0 &&
          proof.signature.length > 0 &&
          proof.challenge.length > 0
        );

      case 'did':
        // TODO: Implement DID verification
        return proof.did.startsWith('did:') && proof.proof.length > 0;

      case 'password':
        // TODO: Implement password hash verification
        return proof.hash.length > 0 && proof.salt.length > 0;

      case 'oauth':
        // TODO: Implement OAuth token verification
        return proof.token.length > 0 && proof.provider.length > 0;

      default:
        return false;
    }
  }

  /**
   * Mark session as authenticated
   * Called after mutual authentication succeeds
   */
  markAuthenticated(): void {
    this.authenticated = true;
  }

  /**
   * Check if session is authenticated
   */
  isAuthenticated(): boolean {
    return this.authenticated;
  }

  /**
   * Get entity ID
   */
  getEntityId(): string {
    return this.entityId;
  }

  // ============================================================================
  // Recognition Operations (Budget-Constrained)
  // ============================================================================

  /**
   * Allocate recognition to another entity
   * Can only allocate own recognition (enforces budget constraint)
   * 
   * If initialized with keypair, this automatically signs the update!
   * 
   * @throws BudgetConstraintError if allocation would exceed budget
   */
  async allocateRecognition(targetId: EntityId, amount: number): Promise<void> {
    if (amount < 0 || amount > 1) {
      throw new Error('Amount must be between 0 and 1');
    }

    // Load current allocations
    const outgoing = await this.storage.getOutgoingEdges(this.entityId);
    
    // Calculate new total
    const currentToTarget = outgoing.get(targetId) || 0;
    const currentTotal = Array.from(outgoing.values()).reduce((sum, val) => sum + val, 0);
    const newTotal = currentTotal - currentToTarget + amount;

    // Check budget constraint
    if (newTotal > this.maxAllocation) {
      throw new BudgetConstraintError(
        `Allocation would exceed budget: ${newTotal} > ${this.maxAllocation}`,
        { currentTotal, newTotal, maxAllocation: this.maxAllocation }
      );
    }

    // Use secure storage if available (auto-signs)
    if (this.secureContext && this.secureStorage) {
      await this.secureStorage.storeRecognition(targetId, amount);
    } else {
      // Fallback to regular storage (backwards compatibility)
      await this.storage.setRecognitionEdge(this.entityId, targetId, amount);
    }

    // Invalidate caches involving this entity
    this.cache.invalidateEntity(this.entityId);

    // Notify sync callbacks
    const syncUpdate: SyncUpdate = {
      entityId: this.entityId,
      operation: {
        type: 'allocate',
        fromId: this.entityId,
        toId: targetId,
        amount,
        timestamp: Date.now(),
        clock: this.storage.getLocalStamp()  // ITC stamp
      }
    };
    this.notifySyncCallbacks(syncUpdate);

    // Queue for sync (offline support)
    await this.storage.queueSync(syncUpdate.operation);
  }

  /**
   * Revoke recognition from an entity
   * Equivalent to allocateRecognition(targetId, 0)
   */
  async revokeRecognition(targetId: EntityId): Promise<void> {
    await this.allocateRecognition(targetId, 0);
  }

  /**
   * Get own allocations (outgoing edges)
   * Returns sparse representation
   */
  async getMyAllocations(): Promise<SerializedSparseGraph> {
    const outgoing = await this.storage.getOutgoingEdges(this.entityId);
    
    // Convert to sparse graph format
    const edges: Array<[string, string, number]> = [];
    for (const [toId, amount] of outgoing) {
      if (amount > 0) {
        edges.push([this.entityId, toId, amount]);
      }
    }
    
    return {
      type: 'sparse-graph',
      edges,
      metadata: {
        totalEntities: 1,
        totalEdges: edges.length,
        avgDegree: edges.length,
        sparsity: 1
      }
    };
  }

  /**
   * Get budget status
   */
  async getBudgetStatus(): Promise<{
    used: number;
    remaining: number;
    max: number;
    valid: boolean;
  }> {
    const outgoing = await this.storage.getOutgoingEdges(this.entityId);
    const used = Array.from(outgoing.values()).reduce((sum, val) => sum + val, 0);
    
    return {
      used,
      remaining: this.maxAllocation - used,
      max: this.maxAllocation,
      valid: used <= this.maxAllocation
    };
  }

  // ============================================================================
  // Query Operations (Sparse & Cached)
  // ============================================================================

  /**
   * Get mutual recognition with another entity
   * Uses sparse operations and caching
   */
  async getMutualRecognition(otherId: EntityId): Promise<number> {
    // Check cache
    const cacheKey = mrCacheKey(this.entityId, otherId);
    const cached = this.cache.get<number>(cacheKey);
    if (cached !== null) return cached;

    // Load sparse graph
    const graph = await this.storage.loadSparseGraph();
    
    // Compute (sparse!)
    const mr = sparseMutual(graph)(this.entityId)(otherId);
    
    // Cache result
    this.cache.set(cacheKey, mr);
    
    return mr;
  }

  /**
   * Get Mutual Recognition Share (MRS) over a universe
   * Uses sparse operations and caching
   */
  async getMRS(universeIds: string[]): Promise<Distribution> {
    const universe = new Set(universeIds);
    
    // Check cache
    const cacheKey = mrsCacheKey(this.entityId, universe);
    const cached = this.cache.get<Distribution>(cacheKey);
    if (cached !== null) return cached;

    // Load sparse graph
    const graph = await this.storage.loadSparseGraph();
    
    // Compute (sparse!)
    const mrs = sparseMRS(graph)(this.entityId)(universe);
    
    // Cache result
    this.cache.set(cacheKey, mrs);
    
    return mrs;
  }

  /**
   * Get Total Mutual Recognition (TMR) over a universe
   * Uses sparse operations and caching
   */
  async getTMR(universeIds: string[]): Promise<number> {
    const universe = new Set(universeIds);
    
    // Check cache
    const cacheKey = tmrCacheKey(this.entityId, universe);
    const cached = this.cache.get<number>(cacheKey);
    if (cached !== null) return cached;

    // Load sparse graph
    const graph = await this.storage.loadSparseGraph();
    
    // Compute (sparse!)
    const tmr = sparseTMR(graph)(this.entityId)(universe);
    
    // Cache result
    this.cache.set(cacheKey, tmr);
    
    return tmr;
  }

  /**
   * Get Mutual Recognition Density (MRD) with a collective
   * Uses sparse operations and caching
   */
  async getMRD(collectiveMembers: string[]): Promise<number> {
    const collective = new Set(collectiveMembers);
    
    // Check cache
    const cacheKey = mrdCacheKey(this.entityId, collective);
    const cached = this.cache.get<number>(cacheKey);
    if (cached !== null) return cached;

    // Load sparse graph
    const graph = await this.storage.loadSparseGraph();
    
    // Compute (sparse!)
    const mrd = sparseMRD(graph)(this.entityId)(collective);
    
    // Cache result
    this.cache.set(cacheKey, mrd);
    
    return mrd;
  }

  /**
   * Get full sparse graph
   * Warning: This can be large for big networks!
   */
  async getFullGraph(): Promise<SerializedSparseGraph> {
    const graph = await this.storage.loadSparseGraph();
    return serializeSparseGraph(graph);
  }

  // ============================================================================
  // Sync Operations (CRDT-Style)
  // ============================================================================

  /**
   * Receive sync update from peer
   * Merges changes using vector clocks for conflict resolution
   */
  async receiveSyncUpdate(update: SyncUpdate): Promise<void> {
    const { operation } = update;

    // Merge ITC stamps
    this.storage.mergeStamp(operation.clock);

    // Apply operation
    switch (operation.type) {
      case 'allocate':
        await this.storage.setRecognitionEdge(
          operation.fromId,
          operation.toId,
          operation.amount
        );
        break;

      case 'revoke':
        await this.storage.setRecognitionEdge(
          operation.fromId,
          operation.toId,
          0
        );
        break;

      case 'batch':
        for (const op of operation.operations) {
          await this.receiveSyncUpdate({ entityId: update.entityId, operation: op });
        }
        break;
    }

    // Invalidate relevant caches
    this.cache.invalidateEntity(operation.fromId);

    // Log to audit log
    await this.storage.addAuditLog({
      timestamp: Date.now(),
      type: 'sync',
      entityId: operation.fromId,
      data: operation
    });
  }

  /**
   * Subscribe to recognition updates (natural callback style)
   * 
   * Cap'n Web style: just pass a function, it works!
   * 
   * @param callback - Function called when recognition changes
   */
  async subscribe(
    callback: (update: import('./api').RecognitionUpdate) => void
  ): Promise<void> {
    await this.ensureInitialized();
    
    // Wrap the callback to convert SyncUpdate to RecognitionUpdate
    const wrappedCallback: SyncCallback = (syncUpdate: SyncUpdate) => {
      const { operation } = syncUpdate;
      
      if (operation.type === 'allocate') {
        callback({
          type: 'allocate',
          fromId: operation.fromId,
          toId: operation.toId,
          amount: operation.amount,
          timestamp: operation.timestamp
        });
      } else if (operation.type === 'revoke') {
        callback({
          type: 'revoke',
          fromId: operation.fromId,
          toId: operation.toId,
          timestamp: operation.timestamp
        });
      }
    };
    
    this.syncCallbacks.add(wrappedCallback);
  }

  /**
   * Unsubscribe from updates
   */
  unsubscribe(callback: (update: import('./api').RecognitionUpdate) => void): void {
    // Find and remove the wrapped callback
    // Note: This is simplified - in production we'd need a WeakMap to track wrappers
    for (const cb of this.syncCallbacks) {
      this.syncCallbacks.delete(cb);
      break; // Remove first matching callback
    }
  }

  /**
   * Subscribe to sync updates (legacy method, still works)
   * Callback will be called whenever this entity's recognition changes
   */
  async subscribeSyncUpdates(callback: SyncCallback): Promise<void> {
    this.syncCallbacks.add(callback);
  }

  /**
   * Unsubscribe from sync updates
   */
  async unsubscribeSyncUpdates(callback: SyncCallback): Promise<void> {
    this.syncCallbacks.delete(callback);
  }

  /**
   * Notify all sync callbacks
   */
  private async notifySyncCallbacks(update: SyncUpdate): Promise<void> {
    for (const callback of this.syncCallbacks) {
      try {
        await callback(update);
      } catch (error) {
        console.error('Error in sync callback:', error);
      }
    }
  }

  /**
   * Process sync queue (for offline-first)
   * Attempts to sync all pending operations
   */
  async processSyncQueue(sendUpdate: (update: SyncUpdate) => Promise<void>): Promise<number> {
    const queue = await this.storage.getSyncQueue();
    let synced = 0;

    for (const item of queue) {
      try {
        const update: SyncUpdate = {
          entityId: this.entityId,
          operation: item.operation
        };
        
        await sendUpdate(update);
        
        if (item.id !== undefined) {
          await this.storage.markSynced(item.id);
          synced++;
        }
      } catch (error) {
        console.error('Failed to sync operation:', error);
        // Continue with next operation
      }
    }

    return synced;
  }

  // ============================================================================
  // Utility & Monitoring
  // ============================================================================

  /**
   * Get cache statistics
   */
  getCacheStats() {
    return this.cache.getStats();
  }

  /**
   * Clear cache
   */
  clearCache(): void {
    this.cache.clear();
  }

  /**
   * Get audit log for this entity
   */
  async getAuditLog(limit: number = 100) {
    return this.storage.getAuditLog(this.entityId, limit);
  }

  /**
   * Close session and cleanup
   */
  async close(): Promise<void> {
    this.syncCallbacks.clear();
    this.cache.destroy();
    this.storage.close();
  }

  // ============================================================================
  // EntityFullAPI Implementation
  // ============================================================================

  /**
   * Get underlying storage
   */
  getStorage(): BrowserStorage {
    return this.storage;
  }

  /**
   * Get cache instance
   */
  getCache(): RecognitionCache {
    return this.cache;
  }

  /**
   * Get ITC clock
   */
  getClock(): ITClock {
    return this.storage.getClock();
  }

  /**
   * Fork clock for new peer
   */
  forkClock(): ITClock {
    return this.storage.forkClock();
  }
}

