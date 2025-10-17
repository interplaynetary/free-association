/**
 * Holster-based allocation states management
 *
 * Provider allocation states represent the computed allocation decisions
 * that providers make for their capacity slots. These are computed fresh
 * based on current mutual desires and recognition, then published for recipients.
 */

import { writable, get } from 'svelte/store';
import { holsterUser } from './holster.svelte';
import type { ProviderAllocationStateData } from '$lib/schema';
import { parseProviderAllocationStateData } from '$lib/validation';
import {
	addTimestamp,
	getTimestamp,
	shouldPersist,
	type TimestampedData
} from '$lib/utils/holsterTimestamp';

// Store for provider allocation states
export const holsterProviderAllocationStates = writable<Record<string, ProviderAllocationStateData>>({});

// Loading flag
export const isLoadingHolsterAllocationStates = writable(false);

// Track last network timestamp for conflict detection
let lastNetworkTimestamp: number | null = null;

// Track first real data received
let hasReceivedRealData = false;

// Prevent duplicate initialization
let isInitialized: boolean = false;

/**
 * Initialize Holster allocation states (load + subscribe)
 */
export function initializeHolsterAllocationStates() {
	if (!holsterUser.is) {
		console.log('[ALLOCATION-STATES-HOLSTER] Cannot initialize - not authenticated');
		return;
	}

	if (isInitialized) {
		console.log('[ALLOCATION-STATES-HOLSTER] Already initialized, skipping duplicate call');
		return;
	}

	console.log('[ALLOCATION-STATES-HOLSTER] Initializing...');
	isInitialized = true;
	isLoadingHolsterAllocationStates.set(true);
	subscribeToHolsterAllocationStates();
}

/**
 * Subscribe to real-time allocation states updates
 */
function subscribeToHolsterAllocationStates() {
	if (!holsterUser.is) {
		console.log('[ALLOCATION-STATES-HOLSTER] Not authenticated, skipping subscription');
		return;
	}

	holsterUser.get('allocationStates').on((data: any) => {
		try {
			if (!data) {
				if (!hasReceivedRealData) {
					console.log('[ALLOCATION-STATES-HOLSTER] Subscription returned null, waiting for network data...');
				}
				return;
			}

			if (!hasReceivedRealData) {
				console.log('[ALLOCATION-STATES-HOLSTER] First real data received from network');
				hasReceivedRealData = true;
			}

			// Extract timestamp
			const { _updatedAt, ...dataOnly } = data as TimestampedData<any>;
			const networkTimestamp = _updatedAt || null;

			// Check if this update is newer than what we have
			if (networkTimestamp && lastNetworkTimestamp && networkTimestamp <= lastNetworkTimestamp) {
				console.log('[ALLOCATION-STATES-HOLSTER] Ignoring older/duplicate update');
				return;
			}

			// Parse the JSON string
			const parsed = typeof dataOnly === 'string' ? JSON.parse(dataOnly) : dataOnly;

			// Validate each capacity's allocation states
			const validatedAllocationStates: Record<string, ProviderAllocationStateData> = {};
			let hasValidData = false;

			Object.entries(parsed).forEach(([capacityId, capacityAllocations]) => {
				try {
					const validated = parseProviderAllocationStateData(capacityAllocations);
					if (validated) {
						validatedAllocationStates[capacityId] = validated;
						hasValidData = true;
					}
				} catch (error) {
					console.warn(`[ALLOCATION-STATES-HOLSTER] Failed to validate allocation states for capacity ${capacityId}:`, error);
				}
			});

			if (hasValidData) {
				holsterProviderAllocationStates.set(validatedAllocationStates);
				lastNetworkTimestamp = networkTimestamp;
				isLoadingHolsterAllocationStates.set(false);
			}
		} catch (error) {
			console.error('[ALLOCATION-STATES-HOLSTER] Error processing real-time update:', error);
		}
	}, true);
}

/**
 * Persist allocation states to Holster
 */
export async function persistHolsterAllocationStates(): Promise<void> {
	if (!holsterUser.is) {
		console.log('[ALLOCATION-STATES-HOLSTER] Not authenticated, skipping persistence');
		return;
	}

	const allocationStatesValue = get(holsterProviderAllocationStates);

	if (!allocationStatesValue) {
		console.log('[ALLOCATION-STATES-HOLSTER] No allocation states data to persist');
		return;
	}

	// Don't persist empty allocation states during initialization
	if (Object.keys(allocationStatesValue).length === 0) {
		console.log('[ALLOCATION-STATES-HOLSTER] Skipping persistence of empty allocation states');
		return;
	}

	console.log('[ALLOCATION-STATES-HOLSTER] Persisting allocation states for', Object.keys(allocationStatesValue).length, 'capacities...');

	try {
		// Clean data (remove undefined values)
		const cleanedData: Record<string, ProviderAllocationStateData> = {};
		Object.entries(allocationStatesValue).forEach(([capacityId, capacityAllocations]) => {
			cleanedData[capacityId] = capacityAllocations;
		});

		// Add timestamp
		const timestampedData = addTimestamp(cleanedData);

		// Serialize to JSON
		const dataJson = JSON.stringify(timestampedData);

		// Check if we should persist (compare timestamps)
		const currentTimestamp = getTimestamp(timestampedData);
		if (!shouldPersist(currentTimestamp, lastNetworkTimestamp)) {
			console.log('[ALLOCATION-STATES-HOLSTER] Skipping persistence - network has newer data');
			return;
		}

		return new Promise((resolve, reject) => {
			holsterUser.get('allocationStates').put(dataJson, (ack: any) => {
				if (ack?.err) {
					console.error('[ALLOCATION-STATES-HOLSTER] Error persisting allocation states:', ack.err);
					reject(ack.err);
				} else {
					// Update last network timestamp after successful persist
					lastNetworkTimestamp = currentTimestamp;
					resolve();
				}
			});
		});
	} catch (error) {
		console.error('[ALLOCATION-STATES-HOLSTER] Error serializing allocation states:', error);
		throw error;
	}
}

/**
 * Clear allocation states (useful for logout)
 */
export function clearHolsterAllocationStates(): void {
	console.log('[ALLOCATION-STATES-HOLSTER] Clearing allocation states');
	holsterProviderAllocationStates.set({});
	lastNetworkTimestamp = null;
	isInitialized = false;
	hasReceivedRealData = false;
}

// ============================================================================
// Cross-User Data Fetching (for Mutual Contributors)
// ============================================================================

/**
 * Subscribe to a mutual contributor's allocation states from Holster
 * Used to see their published provider allocations for mutual fulfillment algorithm
 */
export function subscribeToContributorHolsterAllocationStates(
	contributorPubKey: string,
	onUpdate: (allocationStates: Record<string, ProviderAllocationStateData>) => void
) {
	if (!holsterUser.is) {
		console.log(`[ALLOCATION-STATES-HOLSTER] Not authenticated, cannot subscribe to ${contributorPubKey.slice(0, 20)}...`);
		return;
	}


	// Subscribe to this contributor's allocation states
	holsterUser.get([contributorPubKey, 'allocationStates']).on((allocationStatesData) => {
		if (!allocationStatesData) {
			onUpdate({});
			return;
		}

		try {
			// Extract timestamp and filter out metadata
			const { _updatedAt, ...dataOnly } = allocationStatesData as TimestampedData<any>;

			// Parse the JSON string (or use object directly if already parsed)
			const parsed = typeof dataOnly === 'string' ? JSON.parse(dataOnly) : dataOnly;

			// Validate each capacity's allocation states
			const validatedAllocationStates: Record<string, ProviderAllocationStateData> = {};
			let hasValidData = false;

			Object.entries(parsed).forEach(([capacityId, capacityAllocations]) => {
				try {
					// Each capacity's allocation states is a ProviderAllocationStateData
					const validated = parseProviderAllocationStateData(capacityAllocations);
					if (validated) {
						validatedAllocationStates[capacityId] = validated;
						hasValidData = true;
					}
				} catch (error) {
					console.warn(`[ALLOCATION-STATES-HOLSTER] Failed to validate allocation states for capacity ${capacityId} from ${contributorPubKey.slice(0, 20)}...:`, error);
				}
			});

			if (!hasValidData) {
				console.warn(`[ALLOCATION-STATES-HOLSTER] No valid allocation states from ${contributorPubKey.slice(0, 20)}...`);
				onUpdate({});
				return;
			}

			console.log(
				`[ALLOCATION-STATES-HOLSTER] Received allocation states from ${contributorPubKey.slice(0, 20)}...:`,
				Object.keys(validatedAllocationStates).length,
				'capacities'
			);

			// Call the update callback
			onUpdate(validatedAllocationStates);
		} catch (error) {
			console.error(`[ALLOCATION-STATES-HOLSTER] Error processing allocation states from ${contributorPubKey.slice(0, 20)}...:`, error);
			onUpdate({});
		}
	}, true);
}
