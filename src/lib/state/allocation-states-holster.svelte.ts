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

/**
 * Initialize Holster allocation states (load + subscribe)
 */
export function initializeHolsterAllocationStates() {
	if (!holsterUser.is) {
		console.log('[ALLOCATION-STATES-HOLSTER] Cannot initialize - not authenticated');
		return;
	}

	console.log('[ALLOCATION-STATES-HOLSTER] Initializing...');
	loadHolsterAllocationStates();
	subscribeToHolsterAllocationStates();
}

/**
 * Load allocation states from Holster
 */
function loadHolsterAllocationStates() {
	if (!holsterUser.is) {
		console.log('[ALLOCATION-STATES-HOLSTER] Not authenticated, skipping load');
		return;
	}

	isLoadingHolsterAllocationStates.set(true);
	console.log('[ALLOCATION-STATES-HOLSTER] Loading allocation states...');

	holsterUser.get('allocationStates', (data: any) => {
		try {
			if (!data) {
				console.log('[ALLOCATION-STATES-HOLSTER] No allocation states found');
				holsterProviderAllocationStates.set({});
				return;
			}

			// Extract timestamp if present
			const { _updatedAt, ...dataOnly } = data as TimestampedData<any>;
			const networkTimestamp = _updatedAt || null;

			// Parse the JSON string
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
					console.warn(`[ALLOCATION-STATES-HOLSTER] Failed to validate allocation states for capacity ${capacityId}:`, error);
				}
			});

			if (hasValidData) {
				// Update store
				holsterProviderAllocationStates.set(validatedAllocationStates);
				lastNetworkTimestamp = networkTimestamp;
				console.log('[ALLOCATION-STATES-HOLSTER] Loaded allocation states for', Object.keys(validatedAllocationStates).length, 'capacities');
			} else {
				console.warn('[ALLOCATION-STATES-HOLSTER] No valid allocation states data found');
				holsterProviderAllocationStates.set({});
			}
		} catch (error) {
			console.error('[ALLOCATION-STATES-HOLSTER] Error loading allocation states:', error);
			holsterProviderAllocationStates.set({});
		} finally {
			isLoadingHolsterAllocationStates.set(false);
		}
	});
}

/**
 * Subscribe to real-time allocation states updates
 */
function subscribeToHolsterAllocationStates() {
	if (!holsterUser.is) {
		console.log('[ALLOCATION-STATES-HOLSTER] Not authenticated, skipping subscription');
		return;
	}

	console.log('[ALLOCATION-STATES-HOLSTER] Subscribing to real-time updates...');

	holsterUser.get('allocationStates').on((data: any) => {
		// Skip if currently loading (initial load will handle it)
		if (get(isLoadingHolsterAllocationStates)) {
			return;
		}

		try {
			if (!data) {
				console.log('[ALLOCATION-STATES-HOLSTER] Received null data (deleted?)');
				holsterProviderAllocationStates.set({});
				lastNetworkTimestamp = null;
				return;
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
				console.log('[ALLOCATION-STATES-HOLSTER] Real-time update:', Object.keys(validatedAllocationStates).length, 'capacities');
			}
		} catch (error) {
			console.error('[ALLOCATION-STATES-HOLSTER] Error processing real-time update:', error);
		}
	});
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
					console.log('[ALLOCATION-STATES-HOLSTER] Allocation states persisted successfully');
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
}
