/**
 * Gun Timestamp Utility
 *
 * Provides generic timestamp extraction from Gun data using Gun's internal
 * state tracking system (GUN.state.is). This eliminates the need for
 * application-level timestamp wrapping.
 *
 * NOTE: This is Gun-specific and will be replaced by holsterTimestamp.ts
 * during the Holster migration. Holster uses explicit application-level
 * timestamps (_updatedAt fields) instead of internal metadata.
 */

// Import Gun for type access
import type { IGunInstance } from 'gun';

// Extend global with Gun
declare global {
	interface Window {
		GUN?: any;
	}
	var GUN: any;
}

/**
 * Extract Gun's internal timestamp for a specific field on a node
 *
 * @param gunNode - The Gun node/data object
 * @param fieldName - The field name to get the timestamp for (defaults to entire node)
 * @returns Unix timestamp in milliseconds, or null if unavailable
 *
 * @example
 * ```typescript
 * const messageData = await gun.get(chatId).map().once();
 * const timestamp = getGunTimestamp(messageData, 'what');
 * ```
 */
export function getGunTimestamp(gunNode: any, fieldName: string = '_'): number | null {
	// Access global GUN object
	const GUN = typeof window !== 'undefined' ? window.GUN : global.GUN;

	if (!GUN || !GUN.state || typeof GUN.state.is !== 'function') {
		console.warn('[GUN-TIMESTAMP] GUN.state.is not available');
		return null;
	}

	try {
		// Use Gun's internal state tracker to get the timestamp
		const timestamp = GUN.state.is(gunNode, fieldName);

		if (typeof timestamp === 'number' && timestamp > 0) {
			return timestamp;
		}

		return null;
	} catch (error) {
		console.error('[GUN-TIMESTAMP] Error extracting timestamp:', error);
		return null;
	}
}

/**
 * Compare two Gun timestamps to determine which is newer
 *
 * @param timestamp1 - First timestamp (milliseconds)
 * @param timestamp2 - Second timestamp (milliseconds)
 * @returns Positive if timestamp1 is newer, negative if timestamp2 is newer, 0 if equal
 */
export function compareGunTimestamps(timestamp1: number | null, timestamp2: number | null): number {
	// Handle null cases
	if (timestamp1 === null && timestamp2 === null) return 0;
	if (timestamp1 === null) return -1; // timestamp2 is newer
	if (timestamp2 === null) return 1;  // timestamp1 is newer

	return timestamp1 - timestamp2;
}

/**
 * Check if Gun data is newer than a reference timestamp
 *
 * @param gunNode - The Gun node/data object
 * @param fieldName - The field name to check
 * @param referenceTimestamp - The timestamp to compare against (milliseconds)
 * @returns true if Gun data is newer, false otherwise
 */
export function isGunDataNewer(
	gunNode: any,
	fieldName: string,
	referenceTimestamp: number | null
): boolean {
	const gunTimestamp = getGunTimestamp(gunNode, fieldName);
	return compareGunTimestamps(gunTimestamp, referenceTimestamp) > 0;
}

/**
 * Extract the most recent timestamp from a Gun node by checking all fields
 * Useful for objects where any field update should be considered
 *
 * @param gunNode - The Gun node/data object
 * @returns The most recent timestamp found, or null if none available
 */
export function getMostRecentGunTimestamp(gunNode: any): number | null {
	if (!gunNode || typeof gunNode !== 'object') {
		return null;
	}

	const GUN = typeof window !== 'undefined' ? window.GUN : global.GUN;

	if (!GUN || !GUN.state || typeof GUN.state.is !== 'function') {
		return null;
	}

	let mostRecent: number | null = null;

	try {
		// Check all fields in the node
		for (const field of Object.keys(gunNode)) {
			// Skip Gun metadata fields
			if (field.startsWith('_')) continue;

			const timestamp = getGunTimestamp(gunNode, field);
			if (timestamp !== null) {
				if (mostRecent === null || timestamp > mostRecent) {
					mostRecent = timestamp;
				}
			}
		}
	} catch (error) {
		console.error('[GUN-TIMESTAMP] Error finding most recent timestamp:', error);
	}

	return mostRecent;
}

/**
 * Check if a Gun timestamp is considered "reliable"
 * Gun may use epoch time (0 or very low values) as placeholder
 *
 * @param timestamp - Timestamp to check (milliseconds)
 * @returns true if timestamp appears to be a real value, false if it's likely a placeholder
 */
export function isReliableGunTimestamp(timestamp: number | null): boolean {
	if (timestamp === null) return false;

	// Consider timestamps after Jan 2, 1970 as reliable
	// This accounts for Gun's epoch placeholders
	const MIN_RELIABLE_TIMESTAMP = new Date('1970-01-02T00:00:00.000Z').getTime();

	return timestamp > MIN_RELIABLE_TIMESTAMP;
}

/**
 * Format a Gun timestamp for display
 *
 * @param timestamp - Timestamp in milliseconds
 * @returns ISO string format, or 'Unknown' if null/invalid
 */
export function formatGunTimestamp(timestamp: number | null): string {
	if (timestamp === null) return 'Unknown';

	try {
		return new Date(timestamp).toISOString();
	} catch (error) {
		return 'Invalid';
	}
}
