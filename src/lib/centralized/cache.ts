import type { Cache, CacheValue, ShareMap } from './types';

// Initialize an empty cache
export function emptyCache<K>(): Cache<K> {
	return {
		cacheMap: new Map<K, CacheValue>(),
		cacheMisses: 0,
		cacheHits: 0
	};
}

// Lookup a value in the cache
export function cacheLookup<K>(key: K, cache: Cache<K>): [CacheValue, Cache<K>] | null {
	const value = cache.cacheMap.get(key);
	if (value) {
		// Create a new cache with updated hits count (immutable pattern)
		const updatedCache: Cache<K> = {
			...cache,
			cacheHits: cache.cacheHits + 1
		};
		return [value, updatedCache];
	}
	return null;
}

// Insert a value into the cache
export function cacheInsert<K>(key: K, value: CacheValue, cache: Cache<K>): Cache<K> {
	// Create a new map with the new key-value pair (immutable pattern)
	const newMap = new Map(cache.cacheMap);
	newMap.set(key, value);

	// Create a new cache with updated map and misses count
	return {
		cacheMap: newMap,
		cacheMisses: cache.cacheMisses + 1,
		cacheHits: cache.cacheHits
	};
}

// Helper functions to work with CacheValue
export function fromFloat(f: number): CacheValue {
	return { type: 'float', value: f };
}

export function fromInt(i: number): CacheValue {
	return { type: 'int', value: i };
}

export function fromStringList(l: string[]): CacheValue {
	return { type: 'stringList', value: l };
}

export function fromShareMap(m: ShareMap): CacheValue {
	return { type: 'shareMap', value: m };
}

export function toFloat(value: CacheValue): number | null {
	if (value.type === 'float') {
		return value.value;
	}
	return null;
}

export function toInt(value: CacheValue): number | null {
	if (value.type === 'int') {
		return value.value;
	}
	return null;
}

export function toStringList(value: CacheValue): string[] | null {
	if (value.type === 'stringList') {
		return value.value;
	}
	return null;
}

export function toShareMap(value: CacheValue): ShareMap | null {
	if (value.type === 'shareMap') {
		return value.value;
	}
	return null;
}

// Generic caching function
export function withCache<K, A, B>(
	key: K,
	constructor: (b: B) => CacheValue,
	extractor: (value: CacheValue) => B | null,
	compute: (a: A) => B,
	cache: Cache<K>,
	arg: A
): [B, Cache<K>] {
	const lookupResult = cacheLookup(key, cache);

	if (lookupResult) {
		const [value, updatedCache] = lookupResult;
		const extracted = extractor(value);

		if (extracted !== null) {
			return [extracted, updatedCache];
		}

		return computeAndCache(updatedCache);
	}

	return computeAndCache(cache);

	function computeAndCache(currentCache: Cache<K>): [B, Cache<K>] {
		const computed = compute(arg);
		const newValue = constructor(computed);
		const newCache = cacheInsert(key, newValue, currentCache);
		return [computed, newCache];
	}
}
