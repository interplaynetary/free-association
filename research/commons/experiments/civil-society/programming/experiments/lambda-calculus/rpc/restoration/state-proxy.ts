/**
 * StateProxy - Transparent Lazy State Loading
 * 
 * Tries sources in order: cache → localStorage → replicas
 * Caches promises for pipelining.
 */

import type { EntityId } from '../types';
import type { ReplicaNode, StateFragment } from './discovery';
import { RecognitionCache } from '../cache';

/**
 * Configuration for StateProxy
 */
export interface StateProxyConfig {
  entityId: EntityId;
  replicas: ReplicaNode[];
  cache?: RecognitionCache;
  useLocalStorage?: boolean;
}

/**
 * StateProxy - Lazy loading state access
 * 
 * Provides transparent access to recognition state:
 * - Tries memory cache first (instant)
 * - Falls back to localStorage (fast)
 * - Reconstructs from replicas (network)
 * 
 * Caches promises to enable pipelining.
 */
export class StateProxy {
  private entityId: EntityId;
  private replicas: ReplicaNode[];
  private cache: RecognitionCache;
  private useLocalStorage: boolean;
  
  // Promise cache for pipelining
  private promiseCache: Map<string, Promise<number>>;
  
  // Reconstructed state cache
  private stateCache: Map<string, Map<string, number>>;

  constructor(config: StateProxyConfig) {
    this.entityId = config.entityId;
    this.replicas = config.replicas;
    this.cache = config.cache || new RecognitionCache();
    this.useLocalStorage = config.useLocalStorage ?? true;
    this.promiseCache = new Map();
    this.stateCache = new Map();
  }

  /**
   * Get recognition value between two entities
   * 
   * This is the main entry point - tries all sources automatically.
   */
  async getRecognition(from: EntityId, to: EntityId): Promise<number> {
    const key = this.edgeKey(from, to);
    
    // Return cached promise for pipelining
    if (this.promiseCache.has(key)) {
      return this.promiseCache.get(key)!;
    }

    // Start fetch and cache promise immediately
    const promise = this.fetchRecognition(from, to);
    this.promiseCache.set(key, promise);
    
    return promise;
  }

  /**
   * Fetch recognition from available sources
   * @private
   */
  private async fetchRecognition(from: EntityId, to: EntityId): Promise<number> {
    // Try sources in order of speed
    
    // 1. Try memory cache (instant)
    const cached = this.tryMemory(from, to);
    if (cached !== null) {
      return cached;
    }

    // 2. Try RecognitionCache (in-memory, TTL-based)
    const cacheKey = `${from}→${to}`;
    const cacheValue = this.cache.get(from, to);
    if (cacheValue !== null) {
      return cacheValue;
    }

    // 3. Try localStorage (fast)
    if (this.useLocalStorage) {
      const localValue = await this.tryLocalStorage(from, to);
      if (localValue !== null) {
        // Cache it
        this.cache.set(from, to, localValue);
        return localValue;
      }
    }

    // 4. Reconstruct from replicas (network)
    const networkValue = await this.reconstructFromReplicas(from, to);
    
    // Cache the result
    this.cache.set(from, to, networkValue);
    if (this.useLocalStorage) {
      this.saveToLocalStorage(from, to, networkValue);
    }
    
    return networkValue;
  }

  /**
   * Try memory cache
   * @private
   */
  private tryMemory(from: EntityId, to: EntityId): number | null {
    const fromMap = this.stateCache.get(from);
    if (!fromMap) {
      return null;
    }
    
    const value = fromMap.get(to);
    return value !== undefined ? value : null;
  }

  /**
   * Try localStorage
   * @private
   */
  private async tryLocalStorage(from: EntityId, to: EntityId): Promise<number | null> {
    if (typeof window === 'undefined' || !window.localStorage) {
      return null;
    }

    try {
      const key = `fa-state-${this.entityId}-${from}-${to}`;
      const value = localStorage.getItem(key);
      
      if (value !== null) {
        return parseFloat(value);
      }
    } catch (error) {
      console.warn('localStorage read failed:', error);
    }

    return null;
  }

  /**
   * Save to localStorage
   * @private
   */
  private saveToLocalStorage(from: EntityId, to: EntityId, value: number): void {
    if (typeof window === 'undefined' || !window.localStorage) {
      return;
    }

    try {
      const key = `fa-state-${this.entityId}-${from}-${to}`;
      localStorage.setItem(key, value.toString());
    } catch (error) {
      console.warn('localStorage write failed:', error);
    }
  }

  /**
   * Reconstruct state from replicas
   * @private
   */
  private async reconstructFromReplicas(from: EntityId, to: EntityId): Promise<number> {
    // Fetch fragments from all replicas
    const fragmentPromises = this.replicas.map(replica =>
      replica.getStateFor(this.entityId).catch(err => {
        console.warn(`Failed to get state from replica:`, err);
        return null;
      })
    );

    const fragments = await Promise.all(fragmentPromises);
    const validFragments = fragments.filter((f): f is StateFragment => f !== null);

    if (validFragments.length === 0) {
      console.warn(`No replicas available for ${this.entityId}`);
      return 0; // Default value
    }

    // Get value from each fragment
    const values: number[] = [];
    for (const fragment of validFragments) {
      const fromMap = fragment.edges.get(from);
      if (fromMap) {
        const value = fromMap.get(to);
        if (value !== undefined) {
          values.push(value);
        }
      }
    }

    if (values.length === 0) {
      return 0; // No data found
    }

    // Use median value for consensus (simple Byzantine resistance)
    values.sort((a, b) => a - b);
    const median = values[Math.floor(values.length / 2)];
    
    // Cache in memory
    this.cacheInMemory(from, to, median);
    
    return median;
  }

  /**
   * Cache value in memory
   * @private
   */
  private cacheInMemory(from: EntityId, to: EntityId, value: number): void {
    let fromMap = this.stateCache.get(from);
    if (!fromMap) {
      fromMap = new Map();
      this.stateCache.set(from, fromMap);
    }
    fromMap.set(to, value);
  }

  /**
   * Generate cache key for an edge
   * @private
   */
  private edgeKey(from: EntityId, to: EntityId): string {
    return `${from}→${to}`;
  }

  /**
   * Prefetch state for common queries
   * 
   * Useful for warming up the cache before operations.
   */
  async prefetch(entities: EntityId[]): Promise<void> {
    const promises: Promise<number>[] = [];

    // Prefetch all edges between entities
    for (const from of entities) {
      for (const to of entities) {
        if (from !== to) {
          promises.push(this.getRecognition(from, to));
        }
      }
    }

    await Promise.all(promises);
  }

  /**
   * Clear all caches
   */
  clearCache(): void {
    this.promiseCache.clear();
    this.stateCache.clear();
    this.cache.clear();
  }

  /**
   * Get cache statistics
   */
  getCacheStats(): {
    promiseCacheSize: number;
    stateCacheSize: number;
    recognitionCacheStats: any;
  } {
    return {
      promiseCacheSize: this.promiseCache.size,
      stateCacheSize: this.stateCache.size,
      recognitionCacheStats: this.cache.getStats()
    };
  }

  /**
   * Update replicas (for replica changes during session)
   */
  updateReplicas(replicas: ReplicaNode[]): void {
    this.replicas = replicas;
    // Clear caches to force refresh
    this.clearCache();
  }

  /**
   * Preload state from fragments (during login)
   * 
   * This is used by the login flow to initialize the proxy
   * with already-fetched fragments.
   */
  preloadFromFragments(fragments: StateFragment[]): void {
    for (const fragment of fragments) {
      // Load all edges from fragment into memory cache
      for (const [from, toMap] of fragment.edges.entries()) {
        for (const [to, value] of toMap.entries()) {
          this.cacheInMemory(from, to, value);
          this.cache.set(from, to, value);
          
          if (this.useLocalStorage) {
            this.saveToLocalStorage(from, to, value);
          }
        }
      }
    }
  }
}

/**
 * Create a StateProxy with default configuration
 */
export function createStateProxy(
  entityId: EntityId,
  replicas: ReplicaNode[],
  options?: Partial<StateProxyConfig>
): StateProxy {
  return new StateProxy({
    entityId,
    replicas,
    ...options
  });
}

