/**
 * Elegant Memoization Utilities
 * 
 * Provides high-performance caching for:
 * - RPC calls
 * - Matrix operations
 * - Distribution calculations
 * 
 * Features:
 * - LRU eviction
 * - TTL support
 * - Cache invalidation
 * - TypeScript-first
 */

export interface CacheEntry<T> {
  value: T;
  timestamp: number;
  hits: number;
  ttl?: number;
}

export interface MemoOptions {
  maxSize?: number;
  ttl?: number; // Time to live in ms
  keyGenerator?: (...args: any[]) => string;
}

/**
 * LRU Cache with TTL support
 */
export class LRUCache<K, V> {
  private cache = new Map<K, CacheEntry<V>>();
  private readonly maxSize: number;
  private readonly defaultTTL?: number;
  
  constructor(maxSize: number = 1000, defaultTTL?: number) {
    this.maxSize = maxSize;
    this.defaultTTL = defaultTTL;
  }
  
  get(key: K): V | undefined {
    const entry = this.cache.get(key);
    if (!entry) return undefined;
    
    // Check TTL
    if (entry.ttl && Date.now() - entry.timestamp > entry.ttl) {
      this.cache.delete(key);
      return undefined;
    }
    
    // Update stats
    entry.hits++;
    entry.timestamp = Date.now();
    
    // Move to end (most recently used)
    this.cache.delete(key);
    this.cache.set(key, entry);
    
    return entry.value;
  }
  
  set(key: K, value: V, ttl?: number): void {
    // Evict oldest if at capacity
    if (this.cache.size >= this.maxSize) {
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
    }
    
    this.cache.set(key, {
      value,
      timestamp: Date.now(),
      hits: 0,
      ttl: ttl ?? this.defaultTTL
    });
  }
  
  has(key: K): boolean {
    const entry = this.cache.get(key);
    if (!entry) return false;
    
    // Check TTL
    if (entry.ttl && Date.now() - entry.timestamp > entry.ttl) {
      this.cache.delete(key);
      return false;
    }
    
    return true;
  }
  
  delete(key: K): boolean {
    return this.cache.delete(key);
  }
  
  clear(): void {
    this.cache.clear();
  }
  
  invalidate(pattern: RegExp): number {
    let count = 0;
    for (const key of this.cache.keys()) {
      if (pattern.test(String(key))) {
        this.cache.delete(key);
        count++;
      }
    }
    return count;
  }
  
  getStats() {
    let totalHits = 0;
    let validEntries = 0;
    
    for (const entry of this.cache.values()) {
      if (!entry.ttl || Date.now() - entry.timestamp <= entry.ttl) {
        validEntries++;
        totalHits += entry.hits;
      }
    }
    
    return {
      size: validEntries,
      totalHits,
      avgHits: validEntries > 0 ? totalHits / validEntries : 0
    };
  }
}

/**
 * Memoize a function with automatic key generation
 * 
 * @example
 * const memoizedFn = memoize(
 *   async (a: number, b: number) => a + b,
 *   { maxSize: 100, ttl: 5000 }
 * );
 */
export function memoize<TArgs extends any[], TReturn>(
  fn: (...args: TArgs) => TReturn | Promise<TReturn>,
  options: MemoOptions = {}
): (...args: TArgs) => Promise<TReturn> {
  const {
    maxSize = 1000,
    ttl,
    keyGenerator = (...args) => JSON.stringify(args)
  } = options;
  
  const cache = new LRUCache<string, TReturn>(maxSize, ttl);
  
  return async (...args: TArgs): Promise<TReturn> => {
    const key = keyGenerator(...args);
    
    // Cache hit
    if (cache.has(key)) {
      const cached = cache.get(key);
      if (cached !== undefined) {
        console.log(`[MEMO-HIT] ${key}`);
        return cached;
      }
    }
    
    // Cache miss
    console.log(`[MEMO-MISS] ${key}`);
    const result = await fn(...args);
    cache.set(key, result, ttl);
    
    return result;
  };
}

/**
 * Create a memoized method with custom cache control
 * 
 * @example
 * class Client {
 *   getMR = createMemoizedMethod(
 *     async (idA, idB) => this.rpc.computeMR(idA, idB),
 *     { maxSize: 500 }
 *   );
 * }
 */
export function createMemoizedMethod<TArgs extends any[], TReturn>(
  fn: (...args: TArgs) => TReturn | Promise<TReturn>,
  options: MemoOptions = {}
): {
  fn: (...args: TArgs) => Promise<TReturn>;
  cache: LRUCache<string, TReturn>;
  invalidate: (pattern: RegExp) => number;
  clear: () => void;
} {
  const {
    maxSize = 1000,
    ttl,
    keyGenerator = (...args) => JSON.stringify(args)
  } = options;
  
  const cache = new LRUCache<string, TReturn>(maxSize, ttl);
  
  const memoizedFn = async (...args: TArgs): Promise<TReturn> => {
    const key = keyGenerator(...args);
    
    if (cache.has(key)) {
      const cached = cache.get(key);
      if (cached !== undefined) {
        return cached;
      }
    }
    
    const result = await fn(...args);
    cache.set(key, result, ttl);
    
    return result;
  };
  
  return {
    fn: memoizedFn,
    cache,
    invalidate: (pattern: RegExp) => cache.invalidate(pattern),
    clear: () => cache.clear()
  };
}

/**
 * Hash object for consistent cache keys
 */
export function hashObject(obj: any): string {
  return JSON.stringify(obj, Object.keys(obj).sort());
}

/**
 * Create cache key from arguments
 */
export function createCacheKey(prefix: string, ...args: any[]): string {
  return `${prefix}:${args.map(arg => 
    typeof arg === 'object' ? hashObject(arg) : String(arg)
  ).join(':')}`;
}

