/**
 * Memoization utilities for allocation algorithm
 * 
 * Provides memoization for expensive pure functions to avoid redundant computations
 * when inputs haven't changed.
 */

/**
 * Simple memoization cache with size limit
 * Uses JSON.stringify for key generation (deep equality)
 */
export function createMemoCache<TArgs extends any[], TResult>(
	fn: (...args: TArgs) => TResult,
	maxSize: number = 100
): (...args: TArgs) => TResult {
	const cache = new Map<string, TResult>();
	
	return (...args: TArgs): TResult => {
		// Generate cache key from arguments
		const key = JSON.stringify(args);
		
		if (cache.has(key)) {
			return cache.get(key)!;
		}
		
		const result = fn(...args);
		
		// Evict oldest entries if cache is full
		if (cache.size >= maxSize) {
			const firstKey = cache.keys().next().value;
			if (firstKey !== undefined) {
				cache.delete(firstKey);
			}
		}
		
		cache.set(key, result);
		return result;
	};
}

/**
 * Memoization with custom key generator
 * Useful when you want to use a subset of arguments or custom hashing
 */
export function createMemoCacheWithKey<TArgs extends any[], TResult>(
	fn: (...args: TArgs) => TResult,
	keyGenerator: (...args: TArgs) => string,
	maxSize: number = 100
): (...args: TArgs) => TResult {
	const cache = new Map<string, TResult>();
	
	return (...args: TArgs): TResult => {
		const key = keyGenerator(...args);
		
		if (cache.has(key)) {
			return cache.get(key)!;
		}
		
		const result = fn(...args);
		
		if (cache.size >= maxSize) {
			const firstKey = cache.keys().next().value;
			if (firstKey !== undefined) {
				cache.delete(firstKey);
			}
		}
		
		cache.set(key, result);
		return result;
	};
}

/**
 * Clear memoization cache (useful for testing or memory management)
 * 
 * Note: This requires the memoized function to expose a clearCache method.
 * For full cache clearing, recreate the memoized function.
 */
export function clearMemoCache<TArgs extends any[], TResult>(
	memoizedFn: (...args: TArgs) => TResult & { clearCache?: () => void }
): void {
	const fnWithCache = memoizedFn as unknown as { clearCache?: () => void };
	if (fnWithCache.clearCache) {
		fnWithCache.clearCache();
	}
}

/**
 * Hash function for objects (simple but effective)
 * Used for creating cache keys from complex objects
 */
export function hashObject(obj: any): string {
	return JSON.stringify(obj, Object.keys(obj).sort());
}

/**
 * Deep equality check for objects
 * More efficient than JSON.stringify for large objects
 */
export function deepEqual(a: any, b: any): boolean {
	if (a === b) return true;
	
	if (a == null || b == null) return false;
	if (typeof a !== 'object' || typeof b !== 'object') return false;
	
	const keysA = Object.keys(a);
	const keysB = Object.keys(b);
	
	if (keysA.length !== keysB.length) return false;
	
	for (const key of keysA) {
		if (!keysB.includes(key)) return false;
		if (!deepEqual(a[key], b[key])) return false;
	}
	
	return true;
}

