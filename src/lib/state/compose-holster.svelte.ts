/**
 * Compose Module - Holster Implementation
 *
 * Manages slot composition data (desiredSlotComposeFrom and desiredSlotComposeInto)
 * using Holster with timestamp-based conflict resolution.
 *
 * Pattern: Same as contacts-holster - key-value collection with timestamps
 */

import { writable, get } from 'svelte/store';
import { holsterUser } from './holster.svelte';
import type { UserSlotComposition } from '$lib/schema';
import { UserSlotCompositionSchema } from '$lib/schema';
import { addTimestamp, getTimestamp, shouldPersist } from '$lib/utils/holsterTimestamp';

// ============================================================================
// State
// ============================================================================

// Local state for compose-from (recipient desires: "I want X units FROM provider-slot")
export const holsterComposeFrom = writable<UserSlotComposition>({});

// Local state for compose-into (provider desires: "I want Y units FROM my-slot INTO recipient-target")
export const holsterComposeInto = writable<UserSlotComposition>({});

// Loading flags
export const isLoadingHolsterComposeFrom = writable(false);
export const isLoadingHolsterComposeInto = writable(false);

// Track last known network timestamps
let lastNetworkTimestampFrom: number | null = null;
let lastNetworkTimestampInto: number | null = null;

// Prevent duplicate initialization
let isInitializedFrom: boolean = false;
let isInitializedInto: boolean = false;

// ============================================================================
// Compose From (Recipient Desires)
// ============================================================================

let composeFromCallback: ((data: any) => void) | null = null;

/**
 * Subscribe to user's compose-from data
 */
function subscribeToComposeFrom() {
	if (!holsterUser.is) {
		console.log('[COMPOSE-FROM-HOLSTER] Cannot subscribe: no authenticated user');
		return;
	}

	console.log('[COMPOSE-FROM-HOLSTER] Subscribing for user:', holsterUser.is.username);

	composeFromCallback = (data: any) => {
		console.log('[COMPOSE-FROM-HOLSTER] Received update:', data);

		// Skip if loading (initial data fetch)
		if (get(isLoadingHolsterComposeFrom)) {
			return;
		}

		if (!data) {
			return;
		}

		// Extract timestamp and filter out metadata fields
		const networkTimestamp = getTimestamp(data);
		const { _updatedAt, ...dataOnly } = data;

		// Parse and validate
		const parseResult = UserSlotCompositionSchema.safeParse(dataOnly);
		if (!parseResult.success) {
			console.error('[COMPOSE-FROM-HOLSTER] Invalid compose-from data:', parseResult.error);
			return;
		}

		const networkComposeFrom = parseResult.data;

		// Only update if newer or first time
		if (!lastNetworkTimestampFrom || (networkTimestamp && networkTimestamp > lastNetworkTimestampFrom)) {
			holsterComposeFrom.set(networkComposeFrom);
			if (networkTimestamp) {
				lastNetworkTimestampFrom = networkTimestamp;
			}
			console.log('[COMPOSE-FROM-HOLSTER] Updated compose-from:', Object.keys(networkComposeFrom).length, 'entries');
		} else {
			console.log('[COMPOSE-FROM-HOLSTER] Skipping stale update');
		}
	};

	holsterUser.get('desiredSlotComposeFrom').on(composeFromCallback);
}

/**
 * Initialize compose-from subscription
 */
export function initializeHolsterComposeFrom() {
	if (!holsterUser.is) {
		console.log('[COMPOSE-FROM-HOLSTER] Cannot initialize: no authenticated user');
		return;
	}

	if (isInitializedFrom) {
		console.log('[COMPOSE-FROM-HOLSTER] Already initialized, skipping duplicate call');
		return;
	}

	console.log('[COMPOSE-FROM-HOLSTER] Initializing...');
	isInitializedFrom = true;
	isLoadingHolsterComposeFrom.set(true);

	// Load initial data
	holsterUser.get('desiredSlotComposeFrom', (data: any) => {
		console.log('[COMPOSE-FROM-HOLSTER] Initial load:', data);

		if (data) {
			// Extract timestamp and filter out metadata
			const timestamp = getTimestamp(data);
			const { _updatedAt, ...dataOnly } = data;

			// Parse and validate
			const parseResult = UserSlotCompositionSchema.safeParse(dataOnly);
			if (parseResult.success) {
				holsterComposeFrom.set(parseResult.data);
				if (timestamp) {
					lastNetworkTimestampFrom = timestamp;
				}
			} else {
				console.error('[COMPOSE-FROM-HOLSTER] Invalid initial data:', parseResult.error);
				holsterComposeFrom.set({});
			}
		} else {
			holsterComposeFrom.set({});
		}

		isLoadingHolsterComposeFrom.set(false);

		// Subscribe to updates
		subscribeToComposeFrom();
	});
}

/**
 * Cleanup compose-from subscription
 */
export function cleanupHolsterComposeFrom() {
	if (composeFromCallback) {
		holsterUser.get('desiredSlotComposeFrom').off(composeFromCallback);
		composeFromCallback = null;
	}
	holsterComposeFrom.set({});
	lastNetworkTimestampFrom = null;
	isInitializedFrom = false;
	console.log('[COMPOSE-FROM-HOLSTER] Cleaned up');
}

/**
 * Persist compose-from to Holster
 */
export async function persistHolsterComposeFrom(composeFrom?: UserSlotComposition): Promise<void> {
	if (!holsterUser.is) {
		console.log('[COMPOSE-FROM-HOLSTER] Not authenticated, skipping persistence');
		return;
	}

	const composeFromToSave = composeFrom || get(holsterComposeFrom);

	if (!composeFromToSave || Object.keys(composeFromToSave).length === 0) {
		console.log('[COMPOSE-FROM-HOLSTER] No compose-from to persist');
		return;
	}

	// Add timestamp
	const timestampedData = addTimestamp(composeFromToSave);
	const localTimestamp = getTimestamp(timestampedData);

	// Check if safe to persist
	if (!shouldPersist(localTimestamp, lastNetworkTimestampFrom)) {
		console.warn('[COMPOSE-FROM-HOLSTER] Skipping persist - network has newer data');
		return;
	}

	console.log('[COMPOSE-FROM-HOLSTER] Persisting compose-from...');

	return new Promise((resolve, reject) => {
		holsterUser.get('desiredSlotComposeFrom').put(timestampedData, (err: any) => {
			if (err) {
				console.error('[COMPOSE-FROM-HOLSTER] Persist error:', err);
				reject(err);
			} else {
				console.log('[COMPOSE-FROM-HOLSTER] Persisted successfully');
				if (localTimestamp) {
					lastNetworkTimestampFrom = localTimestamp;
				}
				resolve();
			}
		});
	});
}

// ============================================================================
// Compose Into (Provider Desires)
// ============================================================================

let composeIntoCallback: ((data: any) => void) | null = null;

/**
 * Subscribe to user's compose-into data
 */
function subscribeToComposeInto() {
	if (!holsterUser.is) {
		console.log('[COMPOSE-INTO-HOLSTER] Cannot subscribe: no authenticated user');
		return;
	}

	console.log('[COMPOSE-INTO-HOLSTER] Subscribing for user:', holsterUser.is.username);

	composeIntoCallback = (data: any) => {
		console.log('[COMPOSE-INTO-HOLSTER] Received update:', data);

		// Skip if loading (initial data fetch)
		if (get(isLoadingHolsterComposeInto)) {
			return;
		}

		if (!data) {
			return;
		}

		// Extract timestamp and filter out metadata fields
		const networkTimestamp = getTimestamp(data);
		const { _updatedAt, ...dataOnly } = data;

		// Parse and validate
		const parseResult = UserSlotCompositionSchema.safeParse(dataOnly);
		if (!parseResult.success) {
			console.error('[COMPOSE-INTO-HOLSTER] Invalid compose-into data:', parseResult.error);
			return;
		}

		const networkComposeInto = parseResult.data;

		// Only update if newer or first time
		if (!lastNetworkTimestampInto || (networkTimestamp && networkTimestamp > lastNetworkTimestampInto)) {
			holsterComposeInto.set(networkComposeInto);
			if (networkTimestamp) {
				lastNetworkTimestampInto = networkTimestamp;
			}
			console.log('[COMPOSE-INTO-HOLSTER] Updated compose-into:', Object.keys(networkComposeInto).length, 'entries');
		} else {
			console.log('[COMPOSE-INTO-HOLSTER] Skipping stale update');
		}
	};

	holsterUser.get('desiredSlotComposeInto').on(composeIntoCallback);
}

/**
 * Initialize compose-into subscription
 */
export function initializeHolsterComposeInto() {
	if (!holsterUser.is) {
		console.log('[COMPOSE-INTO-HOLSTER] Cannot initialize: no authenticated user');
		return;
	}

	if (isInitializedInto) {
		console.log('[COMPOSE-INTO-HOLSTER] Already initialized, skipping duplicate call');
		return;
	}

	console.log('[COMPOSE-INTO-HOLSTER] Initializing...');
	isInitializedInto = true;
	isLoadingHolsterComposeInto.set(true);

	// Load initial data
	holsterUser.get('desiredSlotComposeInto', (data: any) => {
		console.log('[COMPOSE-INTO-HOLSTER] Initial load:', data);

		if (data) {
			// Extract timestamp and filter out metadata
			const timestamp = getTimestamp(data);
			const { _updatedAt, ...dataOnly } = data;

			// Parse and validate
			const parseResult = UserSlotCompositionSchema.safeParse(dataOnly);
			if (parseResult.success) {
				holsterComposeInto.set(parseResult.data);
				if (timestamp) {
					lastNetworkTimestampInto = timestamp;
				}
			} else {
				console.error('[COMPOSE-INTO-HOLSTER] Invalid initial data:', parseResult.error);
				holsterComposeInto.set({});
			}
		} else {
			holsterComposeInto.set({});
		}

		isLoadingHolsterComposeInto.set(false);

		// Subscribe to updates
		subscribeToComposeInto();
	});
}

/**
 * Cleanup compose-into subscription
 */
export function cleanupHolsterComposeInto() {
	if (composeIntoCallback) {
		holsterUser.get('desiredSlotComposeInto').off(composeIntoCallback);
		composeIntoCallback = null;
	}
	holsterComposeInto.set({});
	lastNetworkTimestampInto = null;
	isInitializedInto = false;
	console.log('[COMPOSE-INTO-HOLSTER] Cleaned up');
}

/**
 * Persist compose-into to Holster
 */
export async function persistHolsterComposeInto(composeInto?: UserSlotComposition): Promise<void> {
	if (!holsterUser.is) {
		console.log('[COMPOSE-INTO-HOLSTER] Not authenticated, skipping persistence');
		return;
	}

	const composeIntoToSave = composeInto || get(holsterComposeInto);

	if (!composeIntoToSave || Object.keys(composeIntoToSave).length === 0) {
		console.log('[COMPOSE-INTO-HOLSTER] No compose-into to persist');
		return;
	}

	// Add timestamp
	const timestampedData = addTimestamp(composeIntoToSave);
	const localTimestamp = getTimestamp(timestampedData);

	// Check if safe to persist
	if (!shouldPersist(localTimestamp, lastNetworkTimestampInto)) {
		console.warn('[COMPOSE-INTO-HOLSTER] Skipping persist - network has newer data');
		return;
	}

	console.log('[COMPOSE-INTO-HOLSTER] Persisting compose-into...');

	return new Promise((resolve, reject) => {
		holsterUser.get('desiredSlotComposeInto').put(timestampedData, (err: any) => {
			if (err) {
				console.error('[COMPOSE-INTO-HOLSTER] Persist error:', err);
				reject(err);
			} else {
				console.log('[COMPOSE-INTO-HOLSTER] Persisted successfully');
				if (localTimestamp) {
					lastNetworkTimestampInto = localTimestamp;
				}
				resolve();
			}
		});
	});
}

// ============================================================================
// Combined Initialization & Cleanup
// ============================================================================

/**
 * Initialize both compose-from and compose-into
 */
export function initializeHolsterCompose() {
	if (!holsterUser.is) {
		console.log('[COMPOSE-HOLSTER] Cannot initialize: no authenticated user');
		return;
	}

	console.log('[COMPOSE-HOLSTER] Initializing both compose-from and compose-into...');
	initializeHolsterComposeFrom();
	initializeHolsterComposeInto();
}

/**
 * Cleanup both compose-from and compose-into
 */
export function cleanupHolsterCompose() {
	console.log('[COMPOSE-HOLSTER] Cleaning up both compose-from and compose-into...');
	cleanupHolsterComposeFrom();
	cleanupHolsterComposeInto();
}

/**
 * Reset initialization state (for logout/re-login in same session)
 */
export function resetInitialization() {
	console.log('[COMPOSE-HOLSTER] Resetting initialization state');
	cleanupHolsterCompose();
}

// ============================================================================
// Cross-User Data Fetching (for Mutual Contributors)
// ============================================================================

/**
 * Subscribe to a mutual contributor's compose-from data from Holster
 * Used to see what they want to receive from providers
 */
export function subscribeToContributorHolsterComposeFrom(
	contributorPubKey: string,
	onUpdate: (composeFrom: UserSlotComposition) => void
) {
	if (!holsterUser.is) {
		console.log(`[COMPOSE-FROM-HOLSTER] Not authenticated, cannot subscribe to ${contributorPubKey.slice(0, 20)}...`);
		return;
	}

	console.log(`[COMPOSE-FROM-HOLSTER] Subscribing to contributor compose-from: ${contributorPubKey.slice(0, 20)}...`);

	// Subscribe to this contributor's compose-from data
	holsterUser.get([contributorPubKey, 'desiredSlotComposeFrom']).on((composeFromData) => {
		if (!composeFromData) {
			console.log(`[COMPOSE-FROM-HOLSTER] No compose-from data from ${contributorPubKey.slice(0, 20)}...`);
			// Call with empty object to clear
			onUpdate({});
			return;
		}

		try {
			// Filter out metadata before validation
			const { _updatedAt, ...dataOnly } = composeFromData;

			// Parse and validate
			const parsed = UserSlotCompositionSchema.safeParse(dataOnly);
			if (!parsed.success) {
				console.warn(`[COMPOSE-FROM-HOLSTER] Invalid compose-from from ${contributorPubKey.slice(0, 20)}...`, parsed.error);
				return;
			}

			console.log(
				`[COMPOSE-FROM-HOLSTER] Received compose-from from ${contributorPubKey.slice(0, 20)}...:`,
				Object.keys(parsed.data).length,
				'entries'
			);

			// Call the update callback
			onUpdate(parsed.data);
		} catch (error) {
			console.error(`[COMPOSE-FROM-HOLSTER] Error processing compose-from from ${contributorPubKey.slice(0, 20)}...:`, error);
		}
	});
}

/**
 * Subscribe to a mutual contributor's compose-into data from Holster
 * Used to see what they want to provide to recipients
 */
export function subscribeToContributorHolsterComposeInto(
	contributorPubKey: string,
	onUpdate: (composeInto: UserSlotComposition) => void
) {
	if (!holsterUser.is) {
		console.log(`[COMPOSE-INTO-HOLSTER] Not authenticated, cannot subscribe to ${contributorPubKey.slice(0, 20)}...`);
		return;
	}

	console.log(`[COMPOSE-INTO-HOLSTER] Subscribing to contributor compose-into: ${contributorPubKey.slice(0, 20)}...`);

	// Subscribe to this contributor's compose-into data
	holsterUser.get([contributorPubKey, 'desiredSlotComposeInto']).on((composeIntoData) => {
		if (!composeIntoData) {
			console.log(`[COMPOSE-INTO-HOLSTER] No compose-into data from ${contributorPubKey.slice(0, 20)}...`);
			// Call with empty object to clear
			onUpdate({});
			return;
		}

		try {
			// Filter out metadata before validation
			const { _updatedAt, ...dataOnly } = composeIntoData;

			// Parse and validate
			const parsed = UserSlotCompositionSchema.safeParse(dataOnly);
			if (!parsed.success) {
				console.warn(`[COMPOSE-INTO-HOLSTER] Invalid compose-into from ${contributorPubKey.slice(0, 20)}...`, parsed.error);
				return;
			}

			console.log(
				`[COMPOSE-INTO-HOLSTER] Received compose-into from ${contributorPubKey.slice(0, 20)}...:`,
				Object.keys(parsed.data).length,
				'entries'
			);

			// Call the update callback
			onUpdate(parsed.data);
		} catch (error) {
			console.error(`[COMPOSE-INTO-HOLSTER] Error processing compose-into from ${contributorPubKey.slice(0, 20)}...:`, error);
		}
	});
}
