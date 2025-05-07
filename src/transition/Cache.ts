import type { Cache, CacheValue } from './core';

export function createCache<K, V>(): Cache<K, V> {
	return {
		map: new Map<K, V>(),
		misses: 0,
		hits: 0
	};
}

export function cacheLookup<K, V>(
	key: K,
	cache: Cache<K, V>
): { value: V; cache: Cache<K, V> } | undefined {
	const value = cache.map.get(key);
	if (value === undefined) {
		return undefined;
	}

	return {
		value,
		cache: {
			...cache,
			hits: cache.hits + 1
		}
	};
}

export function cacheInsert<K, V>(key: K, value: V, cache: Cache<K, V>): Cache<K, V> {
	return {
		map: new Map(cache.map).set(key, value),
		hits: cache.hits,
		misses: cache.misses + 1
	};
}

// Helper functions for CacheValue
export const cacheValue = {
	fromFloat: (value: number): CacheValue => ({ type: 'float', value }),
	fromInt: (value: number): CacheValue => ({ type: 'int', value }),
	fromStringList: (value: string[]): CacheValue => ({ type: 'stringList', value }),
	fromShareMap: (value: Map<string, number>): CacheValue => ({ type: 'shareMap', value }),

	toFloat: (value: CacheValue): number | undefined =>
		value.type === 'float' ? value.value : undefined,

	toInt: (value: CacheValue): number | undefined =>
		value.type === 'int' ? value.value : undefined,

	toStringList: (value: CacheValue): string[] | undefined =>
		value.type === 'stringList' ? value.value : undefined,

	toShareMap: (value: CacheValue): Map<string, number> | undefined =>
		value.type === 'shareMap' ? value.value : undefined
};

// Generic caching function
export function withCache<K, A, B>(
	key: K,
	constructor: (value: B) => CacheValue,
	extractor: (value: CacheValue) => B | undefined,
	compute: (arg: A) => B,
	cache: Cache<K, CacheValue>,
	arg: A
): [B, Cache<K, CacheValue>] {
	const lookup = cacheLookup(key, cache);
	if (lookup) {
		const extracted = extractor(lookup.value);
		if (extracted !== undefined) {
			return [extracted, lookup.cache];
		}
	}

	const computed = compute(arg);
	const newValue = constructor(computed);
	const newCache = cacheInsert(key, newValue, cache);

	return [computed, newCache];
}
