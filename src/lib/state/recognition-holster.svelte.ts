/**
 * Holster Recognition/SOGF Module
 *
 * Manages Share of General Fulfillment (SOGF) data using Holster.
 * SOGF represents the share each contributor gets from the general fulfillment pool.
 */

import { writable, get } from 'svelte/store';
import { holsterUser } from './holster.svelte';
import type { ShareMap } from '$lib/schema';
import { parseShareMap } from '$lib/validation';
import { addTimestamp, getTimestamp, shouldPersist } from '$lib/utils/holsterTimestamp';

// Holster SOGF store
export const holsterSogf = writable<ShareMap | null>(null);
export const isLoadingHolsterSogf = writable(false);

// Track last network timestamp for conflict resolution
let lastNetworkTimestamp = $state<number | null>(null);
let hasReceivedRealData = false;

/**
 * Subscribe to real-time SOGF updates
 */
export function subscribeToHolsterSogf() {
	if (!holsterUser.is) return;

	holsterUser.get('sogf').on((sogfData) => {
		if (!sogfData) {
			if (!hasReceivedRealData) {
				console.log('[SOGF-HOLSTER] Subscription returned null, waiting for network data...');
			}
			return;
		}

		if (!hasReceivedRealData) {
			console.log('[SOGF-HOLSTER] First real data received from network');
			hasReceivedRealData = true;
		}

		try {
			// Extract timestamp
			const networkTimestamp = getTimestamp(sogfData);

			// Check if this is newer than what we have
			if (lastNetworkTimestamp !== null && networkTimestamp !== null) {
				if (networkTimestamp <= lastNetworkTimestamp) {
					console.log('[SOGF-HOLSTER] Ignoring older/same timestamp update');
					return;
				}
			}

			// Filter out metadata before validation
			const { _updatedAt, ...dataOnly } = sogfData;

			// Parse and validate
			const parsed = parseShareMap(dataOnly);
			if (parsed) {
				// Check if data actually changed
				const current = get(holsterSogf);
				if (JSON.stringify(current) === JSON.stringify(parsed)) {
					console.log('[SOGF-HOLSTER] Data unchanged, skipping update');
					return;
				}

				holsterSogf.set(parsed);
				lastNetworkTimestamp = networkTimestamp;
				isLoadingHolsterSogf.set(false);
			}
		} catch (error) {
			console.error('[SOGF-HOLSTER] Real-time parse error:', error);
		}
	}, true);
}

/**
 * Persist SOGF to Holster
 */
export async function persistHolsterSogf(sogf?: ShareMap): Promise<void> {
	if (!holsterUser.is) {
		console.log('[SOGF-HOLSTER] Not authenticated, skipping persistence');
		return;
	}

	const sogfToSave = sogf || get(holsterSogf);

	if (!sogfToSave || Object.keys(sogfToSave).length === 0) {
		console.log('[SOGF-HOLSTER] No SOGF to persist');
		return;
	}

	// Generate local timestamp for the data we want to write
	const localTimestamp = Date.now();

	// Check if we should persist (based on timestamps)
	if (!shouldPersist(localTimestamp, lastNetworkTimestamp)) {
		console.log('[SOGF-HOLSTER] Skipping persistence - network has newer data');
		return;
	}

	console.log('[SOGF-HOLSTER] Persisting SOGF...');

	try {
		// Add timestamp to data (use the same local timestamp for consistency)
		const timestampedData = { ...sogfToSave, _updatedAt: localTimestamp };

		return new Promise((resolve, reject) => {
			holsterUser.get('sogf').put(timestampedData, (err) => {
				if (err) {
					console.error('[SOGF-HOLSTER] Persist error:', err);
					reject(err);
				} else {
					// Update our timestamp tracker with the timestamp we just wrote
					lastNetworkTimestamp = localTimestamp;
					resolve();
				}
			});
		});
	} catch (error) {
		console.error('[SOGF-HOLSTER] Serialization error:', error);
		throw error;
	}
}

/**
 * Initialize Holster SOGF
 * Call this after authentication
 */
export function initializeHolsterSogf() {
	if (!holsterUser.is) {
		console.warn('[SOGF-HOLSTER] Cannot initialize - not authenticated');
		return;
	}

	console.log('[SOGF-HOLSTER] Initializing...');
	isLoadingHolsterSogf.set(true);

	subscribeToHolsterSogf();
}

/**
 * Get last network timestamp (for debugging)
 */
export function getHolsterSogfTimestamp(): number | null {
	return lastNetworkTimestamp;
}

/**
 * Subscribe to a contributor's SOGF from Holster
 * Used to receive their recognition of us
 */
export function subscribeToContributorHolsterSogf(
	contributorPubKey: string,
	onUpdate: (theirShare: number, timestamp: number | null) => void
) {
	if (!holsterUser.is) {
		console.log(`[SOGF-HOLSTER] Not authenticated, cannot subscribe to ${contributorPubKey}`);
		return;
	}


	// Subscribe to this contributor's SOGF
	holsterUser.get([contributorPubKey, 'sogf']).on((sogfData) => {
		if (!sogfData) {
			return;
		}

		try {
			// Extract timestamp
			const networkTimestamp = getTimestamp(sogfData);

			// Filter out metadata before validation
			const { _updatedAt, ...dataOnly } = sogfData;

			// Parse and validate
			const parsed = parseShareMap(dataOnly);
			if (!parsed) {
				console.warn(`[SOGF-HOLSTER] Invalid SOGF from ${contributorPubKey.slice(0, 20)}...`);
				return;
			}

			// Get our pub key to find our share in their SOGF
			const ourPubKey = holsterUser.is?.pub;
			if (!ourPubKey) {
				console.warn('[SOGF-HOLSTER] Cannot get our pub key');
				return;
			}

			// Extract their share for us
			const theirShare = parsed[ourPubKey] || 0;

			console.log(
				`[SOGF-HOLSTER] Received share from ${contributorPubKey.slice(0, 20)}...: ${theirShare.toFixed(4)}`
			);

			// Call the update callback
			onUpdate(theirShare, networkTimestamp);
		} catch (error) {
			console.error(`[SOGF-HOLSTER] Error processing SOGF from ${contributorPubKey.slice(0, 20)}...:`, error);
		}
	}, true);
}

/**
 * Cleanup Holster SOGF subscriptions
 * Call this on logout to prevent memory leaks
 */
export function cleanupHolsterSogf() {
	console.log('[SOGF-HOLSTER] Cleaning up subscriptions...');

	// Turn off own SOGF subscription (only if still authenticated)
	if (holsterUser.is) {
		holsterUser.get('sogf').off();
	}

	// Reset state
	holsterSogf.set(null);
	lastNetworkTimestamp = null;
	hasReceivedRealData = false;

	console.log('[SOGF-HOLSTER] Cleanup complete');
}

/**
 * Reset initialization state (for logout/re-login in same session)
 */
export function resetInitialization() {
	console.log('[SOGF-HOLSTER] Resetting initialization state');
	cleanupHolsterSogf();
}
