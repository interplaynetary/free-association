// Utility functions for the free association system

// TextEncoder and TextDecoder for binary data serialization
export const encoder = new TextEncoder();
export const decoder = new TextDecoder();

// Normalize a map so its values sum to 1
export function normalizeMap<K>(map: Map<K, number>): Map<K, number> {
	const total = Array.from(map.values()).reduce((sum, val) => sum + val, 0);

	if (total === 0) return map;

	const normalized = new Map<K, number>();
	map.forEach((value, key) => {
		normalized.set(key, value / total);
	});

	return normalized;
}

// Normalize a ShareMap specifically (type alias for normalizeMap)
export function normalizeShareMap<K>(map: Map<K, number>): Map<K, number> {
	return normalizeMap(map);
}

// Helper function to safely get a value with a default
export function getOrDefault<K, V>(map: Map<K, V>, key: K, defaultValue: V): V {
	return map.has(key) ? map.get(key)! : defaultValue;
}

// Monadic helper functions for functional programming style
export function pipe<A, B>(a: A, f: (a: A) => B): B {
	return f(a);
}

export function compose<A, B, C>(f: (b: B) => C, g: (a: A) => B): (a: A) => C {
	return (a) => f(g(a));
}

// Safe navigation for optional values (similar to Maybe monad)
export function option<T, R>(value: T | null | undefined, f: (value: T) => R, defaultValue: R): R {
	return value != null ? f(value) : defaultValue;
}

// Function to clamp a value between min and max
export function clamp(value: number, min: number, max: number): number {
	return Math.max(min, Math.min(max, value));
}
