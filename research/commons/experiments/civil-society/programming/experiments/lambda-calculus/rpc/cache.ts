/**
 * Recognition Cache with TTL and LRU Eviction
 * 
 * Caches computed values (MR, MRS, MRD) to avoid redundant calculations.
 * Features:
 * - TTL-based expiration (default 5 minutes)
 * - LRU eviction when cache is full
 * - Automatic invalidation when recognition changes
 * - Memory-efficient (limits cache size)
 */

import type { CacheEntry, CacheKey } from './types';

/**
 * Cache configuration
 */
export interface CacheConfig {
  maxSize: number;          // Maximum number of entries
  defaultTTL: number;       // Default TTL in milliseconds
  cleanupInterval: number;  // How often to clean expired entries (ms)
}

/**
 * Default cache configuration
 */
export const DEFAULT_CACHE_CONFIG: CacheConfig = {
  maxSize: 10000,           // 10k entries
  defaultTTL: 5 * 60 * 1000, // 5 minutes
  cleanupInterval: 60 * 1000 // 1 minute
};

/**
 * Recognition cache with TTL and LRU eviction
 */
export class RecognitionCache {
  private cache = new Map<CacheKey, CacheEntry<unknown>>();
  private config: CacheConfig;
  private cleanupTimer?: ReturnType<typeof setInterval>;

  constructor(config: Partial<CacheConfig> = {}) {
    this.config = { ...DEFAULT_CACHE_CONFIG, ...config };
    
    // Start periodic cleanup
    if (typeof setInterval !== 'undefined') {
      this.cleanupTimer = setInterval(
        () => this.cleanup(),
        this.config.cleanupInterval
      );
    }
  }

  /**
   * Get value from cache
   * Returns null if not found or expired
   */
  get<T>(key: CacheKey): T | null {
    const entry = this.cache.get(key) as CacheEntry<T> | undefined;
    
    if (!entry) return null;
    
    // Check if expired
    const age = Date.now() - entry.timestamp;
    if (age > entry.ttl) {
      this.cache.delete(key);
      return null;
    }
    
    // Update hit counter (for LRU)
    entry.hits++;
    
    return entry.value;
  }

  /**
   * Set value in cache with optional custom TTL
   */
  set<T>(key: CacheKey, value: T, ttl?: number): void {
    // Evict if cache is full
    if (this.cache.size >= this.config.maxSize) {
      this.evictLRU();
    }
    
    this.cache.set(key, {
      value,
      timestamp: Date.now(),
      ttl: ttl ?? this.config.defaultTTL,
      hits: 0
    });
  }

  /**
   * Check if key exists and is not expired
   */
  has(key: CacheKey): boolean {
    return this.get(key) !== null;
  }

  /**
   * Delete specific key
   */
  delete(key: CacheKey): boolean {
    return this.cache.delete(key);
  }

  /**
   * Clear entire cache
   */
  clear(): void {
    this.cache.clear();
  }

  /**
   * Invalidate all cache entries involving a specific entity
   * Called when that entity's recognition allocations change
   */
  invalidateEntity(entityId: string): number {
    let invalidated = 0;
    
    for (const [key, _] of this.cache) {
      if (key.includes(entityId)) {
        this.cache.delete(key);
        invalidated++;
      }
    }
    
    return invalidated;
  }

  /**
   * Invalidate all MR cache entries
   * Called when any recognition changes
   */
  invalidateMR(): number {
    let invalidated = 0;
    
    for (const [key, _] of this.cache) {
      if (key.startsWith('mr:')) {
        this.cache.delete(key);
        invalidated++;
      }
    }
    
    return invalidated;
  }

  /**
   * Invalidate cache entries matching a pattern
   */
  invalidatePattern(pattern: RegExp): number {
    let invalidated = 0;
    
    for (const [key, _] of this.cache) {
      if (pattern.test(key)) {
        this.cache.delete(key);
        invalidated++;
      }
    }
    
    return invalidated;
  }

  /**
   * Remove expired entries
   * Returns number of entries removed
   */
  cleanup(): number {
    let removed = 0;
    const now = Date.now();
    
    for (const [key, entry] of this.cache) {
      const age = now - entry.timestamp;
      if (age > entry.ttl) {
        this.cache.delete(key);
        removed++;
      }
    }
    
    return removed;
  }

  /**
   * Evict least recently used entry
   * Based on hit counter
   */
  private evictLRU(): void {
    let lruKey: CacheKey | null = null;
    let minHits = Infinity;
    
    for (const [key, entry] of this.cache) {
      if (entry.hits < minHits) {
        minHits = entry.hits;
        lruKey = key;
      }
    }
    
    if (lruKey) {
      this.cache.delete(lruKey);
    }
  }

  /**
   * Get cache statistics
   */
  getStats(): {
    size: number;
    maxSize: number;
    hitRate: number;
    avgAge: number;
    memoryEstimate: number;
  } {
    const now = Date.now();
    let totalHits = 0;
    let totalAge = 0;
    
    for (const entry of this.cache.values()) {
      totalHits += entry.hits;
      totalAge += now - entry.timestamp;
    }
    
    const size = this.cache.size;
    const avgAge = size > 0 ? totalAge / size : 0;
    
    // Rough memory estimate: 200 bytes per entry
    const memoryEstimate = size * 200;
    
    return {
      size,
      maxSize: this.config.maxSize,
      hitRate: totalHits / Math.max(size, 1),
      avgAge,
      memoryEstimate
    };
  }

  /**
   * Cleanup and stop timers
   */
  destroy(): void {
    if (this.cleanupTimer) {
      clearInterval(this.cleanupTimer);
    }
    this.cache.clear();
  }
}

