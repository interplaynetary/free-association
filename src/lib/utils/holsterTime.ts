/**
 * Holster Timestamp Utility
 *
 * Provides application-level timestamp management for Holster data.
 * Unlike Gun which uses internal metadata (_), Holster strips metadata
 * before returning data to the application. Therefore, we use explicit
 * _updatedAt fields in our data structures for conflict resolution.
 */

/**
 * Add or update timestamp on data
 *
 * @param data - The data object to timestamp
 * @returns New object with _updatedAt field set to current time
 *
 * @example
 * ```typescript
 * const timestampedData = addTimestamp({ name: 'John' });
 * // Returns: { name: 'John', _updatedAt: 1736294719234 }
 * ```
 */
export function addTimestamp<T extends Record<string, any>>(data: T): T & { _updatedAt: number } {
	return {
		...data,
		_updatedAt: Date.now()
	};
}

/**
 * Get timestamp from data
 *
 * @param data - The data object
 * @returns Unix timestamp in milliseconds, or null if unavailable
 */
export function getTimestamp(data: any): number | null {
	if (!data || typeof data !== 'object') {
		return null;
	}

	const timestamp = data._updatedAt;

	if (typeof timestamp === 'number' && timestamp > 0) {
		return timestamp;
	}

	return null;
}

/**
 * Compare two timestamps
 *
 * @param ts1 - First timestamp
 * @param ts2 - Second timestamp
 * @returns -1 if ts1 < ts2, 0 if equal, 1 if ts1 > ts2, null if either is invalid
 */
export function compareTimestamps(
	ts1: number | null,
	ts2: number | null
): -1 | 0 | 1 | null {
	if (ts1 === null || ts2 === null) {
		return null;
	}

	if (ts1 < ts2) return -1;
	if (ts1 > ts2) return 1;
	return 0;
}

/**
 * Check if data is newer than a given timestamp
 *
 * @param data - The data object with _updatedAt field
 * @param referenceTimestamp - The timestamp to compare against
 * @returns true if data is newer, false otherwise
 */
export function isNewer(data: any, referenceTimestamp: number | null): boolean {
	const dataTimestamp = getTimestamp(data);
	const comparison = compareTimestamps(dataTimestamp, referenceTimestamp);
	return comparison === 1;
}

/**
 * Check if timestamp is reliable (valid and not too old)
 *
 * @param timestamp - The timestamp to validate
 * @param maxAgeMs - Maximum age in milliseconds (default: 30 days)
 * @returns true if timestamp is valid and recent
 */
export function isReliableTimestamp(
	timestamp: number | null,
	maxAgeMs: number = 30 * 24 * 60 * 60 * 1000
): boolean {
	if (timestamp === null || timestamp <= 0) {
		return false;
	}

	const now = Date.now();
	const age = now - timestamp;

	// Timestamp shouldn't be in the future (allow 5 min clock skew)
	if (age < -5 * 60 * 1000) {
		return false;
	}

	// Timestamp shouldn't be too old
	if (age > maxAgeMs) {
		return false;
	}

	return true;
}

/**
 * Merge two data objects, keeping the one with the newer timestamp
 *
 * @param local - Local data
 * @param remote - Remote data
 * @returns The data with the newer timestamp, or remote if equal
 */
export function mergeByTimestamp<T>(local: T | null, remote: T | null): T | null {
	if (!local) return remote;
	if (!remote) return local;

	const localTs = getTimestamp(local);
	const remoteTs = getTimestamp(remote);
	const comparison = compareTimestamps(localTs, remoteTs);

	// If remote is newer or equal, prefer remote (tie-breaking for consistency)
	if (comparison === -1 || comparison === 0) {
		return remote;
	}

	return local;
}

/**
 * Check if we should persist data based on timestamps
 * Prevents overwriting newer network data with stale local data
 *
 * @param localTimestamp - Timestamp of data we want to write
 * @param networkTimestamp - Timestamp of current data on network
 * @returns true if safe to write, false if network has newer data
 */
export function shouldPersist(
	localTimestamp: number | null,
	networkTimestamp: number | null
): boolean {
	// If no network timestamp, safe to write
	if (networkTimestamp === null) {
		return true;
	}

	// If no local timestamp, don't write (invalid data)
	if (localTimestamp === null) {
		return false;
	}

	// Only write if local is newer or equal
	const comparison = compareTimestamps(localTimestamp, networkTimestamp);
	return comparison !== -1; // >= 0 (newer or equal)
}