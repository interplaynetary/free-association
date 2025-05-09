import type { Cache, CacheValue, ShareMap } from './types';

// Initialize an empty cache
export function emptyCache<K>(): Cache<K> {
	return {
		cacheMap: new Map<K, CacheValue>(),
		cacheMisses: 0,
		cacheHits: 0
	};
}

// Monadic cache lookup - mirrors the Haskell Maybe monad pattern
export function cacheLookup<K>(key: K, cache: Cache<K>): [CacheValue, Cache<K>] | null {
	const value = cache.cacheMap.get(key);
	if (value) {
		return [value, { ...cache, cacheHits: cache.cacheHits + 1 }];
	}
	return null;
}

// Cache insert maintaining immutability
export function cacheInsert<K>(key: K, value: CacheValue, cache: Cache<K>): Cache<K> {
	const newMap = new Map(cache.cacheMap);
	newMap.set(key, value);
	return {
		cacheMap: newMap,
		cacheMisses: cache.cacheMisses + 1,
		cacheHits: cache.cacheHits
	};
}

// Cacheable type class implementation
// This mirrors Haskell's typeclass pattern
export const Cacheable = {
	fromInt: (i: number): CacheValue => ({ type: 'int', value: i }),
	fromFloat: (f: number): CacheValue => ({ type: 'float', value: f }),
	fromStringList: (l: string[]): CacheValue => ({ type: 'stringList', value: l }),
	fromShareMap: (m: ShareMap): CacheValue => ({ type: 'shareMap', value: m }),

	toInt: (cv: CacheValue): number | null => (cv.type === 'int' ? cv.value : null),
	toFloat: (cv: CacheValue): number | null => (cv.type === 'float' ? cv.value : null),
	toStringList: (cv: CacheValue): string[] | null => (cv.type === 'stringList' ? cv.value : null),
	toShareMap: (cv: CacheValue): ShareMap | null => (cv.type === 'shareMap' ? cv.value : null)
};

// Implement withCacheM - monadic style caching function that mirrors the Haskell version
export function withCacheM<K, V, A>(
	key: K,
	toCache: (v: V) => CacheValue,
	fromCache: (cv: CacheValue) => V | null,
	compute: (a: A) => V,
	cache: Cache<K>,
	arg: A
): [V, Cache<K>] {
	const lookupResult = cacheLookup(key, cache);

	if (lookupResult) {
		const [value, updatedCache] = lookupResult;
		const extracted = fromCache(value);

		if (extracted !== null) {
			return [extracted, updatedCache];
		}

		return computeAndStore(updatedCache);
	}

	return computeAndStore(cache);

	function computeAndStore(currentCache: Cache<K>): [V, Cache<K>] {
		const result = compute(arg);
		const newCache = cacheInsert(key, toCache(result), currentCache);
		return [result, newCache];
	}
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
