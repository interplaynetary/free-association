/**
 * Capacities Module - Holster Implementation
 *
 * Migrated from Gun to Holster using the validated patterns from contacts-holster.svelte.ts:
 * - Application-level timestamps via _updatedAt fields
 * - Conflict resolution using shouldPersist()
 * - Real-time sync with subscription management
 * - Compatible with existing CapacitiesCollection schema
 */

import { writable, get } from 'svelte/store';
import { holsterUser } from './holster.svelte';
import type { CapacitiesCollection, Capacity } from '$lib/schema';
import { CapacitiesCollectionSchema } from '$lib/schema';
import { addTimestamp, getTimestamp, shouldPersist } from '$lib/utils/holsterTimestamp';
import { processCapacitiesLocations } from '$lib/utils/geocodingCache';

// ============================================================================
// State
// ============================================================================

// Local capacities state
export const holsterCapacities = writable<CapacitiesCollection | null>(null);

// Loading flag
export const isLoadingHolsterCapacities = writable(false);

// Track last known network timestamp for capacities collection
let lastNetworkTimestamp: number | null = null;

// Track capacity IDs that exist in Holster (for detecting deletions)
let lastKnownCapacityIds: Set<string> = new Set();

// Prevent duplicate initialization
let isInitialized: boolean = false;

// ============================================================================
// localStorage Cache
// ============================================================================

const CAPACITIES_CACHE_KEY = 'holster_capacities_cache';
const CAPACITIES_CACHE_TIMESTAMP_KEY = 'holster_capacities_cache_timestamp';

function getCachedCapacities(): CapacitiesCollection | null {
	if (typeof localStorage === 'undefined') return null;

	try {
		const cached = localStorage.getItem(CAPACITIES_CACHE_KEY);
		if (!cached) return null;

		const parsed = JSON.parse(cached);
		const validation = CapacitiesCollectionSchema.safeParse(parsed);

		if (validation.success) {
			const timestamp = localStorage.getItem(CAPACITIES_CACHE_TIMESTAMP_KEY);
			console.log('[CAPACITIES-HOLSTER] Loaded capacities from cache, timestamp:', timestamp);
			return validation.data;
		} else {
			console.warn('[CAPACITIES-HOLSTER] Invalid cached capacities, ignoring');
			return null;
		}
	} catch (error) {
		console.error('[CAPACITIES-HOLSTER] Error reading cached capacities:', error);
		return null;
	}
}

function setCachedCapacities(capacities: CapacitiesCollection, timestamp: number): void {
	if (typeof localStorage === 'undefined') return;

	try {
		localStorage.setItem(CAPACITIES_CACHE_KEY, JSON.stringify(capacities));
		localStorage.setItem(CAPACITIES_CACHE_TIMESTAMP_KEY, timestamp.toString());
		console.log('[CAPACITIES-HOLSTER] Cached capacities with timestamp:', timestamp);
	} catch (error) {
		console.error('[CAPACITIES-HOLSTER] Error caching capacities:', error);
	}
}

// ============================================================================
// Subscription Management
// ============================================================================

let capacitiesCallback: ((data: any) => void) | null = null;

/**
 * Subscribe to user's capacities from Holster
 * Uses object graph structure - each capacity is a separate node
 */
function subscribeToCapacities() {
	if (!holsterUser.is) {
		console.log('[CAPACITIES-HOLSTER] Cannot subscribe: no authenticated user');
		return;
	}

	console.log('[CAPACITIES-HOLSTER] Subscribing to capacities for user:', holsterUser.is.username);

	// Create callback for updates - this receives the entire capacities object graph
	capacitiesCallback = (data: any) => {
		console.log('[CAPACITIES-HOLSTER] Received update:', data);

		// Skip if loading (initial data fetch)
		if (get(isLoadingHolsterCapacities)) {
			return;
		}

		if (!data) {
			return;
		}

		// Helper to check if a value is "deleted" (null, undefined, or object with missing required fields)
		const isDeleted = (value: any, key?: string): boolean => {
			if (value === null || value === undefined) {
				console.log(`[CAPACITIES-HOLSTER] Skipping deleted entry${key ? ` (${key})` : ''}: null/undefined`);
				return true;
			}
			if (typeof value === 'object' && value !== null) {
				// Check if required capacity fields are missing (indicates deletion)
				if (!value.id || !value.name) {
					console.warn(`[CAPACITIES-HOLSTER] Skipping malformed entry${key ? ` (${key})` : ''}: missing required fields (id or name)`, value);
					return true;
				}
				// Check if all fields are null or undefined (Holster deletion pattern)
				const allFieldsNull = Object.values(value).every((v) => v === null || v === undefined);
				if (allFieldsNull) {
					console.log(`[CAPACITIES-HOLSTER] Skipping deleted entry${key ? ` (${key})` : ''}: all fields null/undefined`);
					return true;
				}
			}
			return false;
		};

		// Extract timestamp and filter out metadata fields AND null/deleted values
		const networkTimestamp = getTimestamp(data);
		const capacitiesOnly: any = {};
		for (const [key, value] of Object.entries(data)) {
			if (!isDeleted(value, key) && !key.startsWith('_')) {
				capacitiesOnly[key] = value;
			}
		}

		// Convert availability_slots objects back to arrays
		for (const [capacityId, capacity] of Object.entries(capacitiesOnly)) {
			if ((capacity as any).availability_slots && typeof (capacity as any).availability_slots === 'object') {
				const slotsObj = (capacity as any).availability_slots;
				const slotsArray: any[] = [];
				for (const slotId of Object.keys(slotsObj)) {
					if (slotsObj[slotId]) { // Only include non-null/undefined slots
						slotsArray.push(slotsObj[slotId]);
					}
				}
				(capacity as any).availability_slots = slotsArray;
			} else {
				(capacity as any).availability_slots = [];
			}
		}

		// Parse and validate (without timestamp)
		const parseResult = CapacitiesCollectionSchema.safeParse(capacitiesOnly);
		if (!parseResult.success) {
			console.error('[CAPACITIES-HOLSTER] Invalid capacities data:', parseResult.error);
			return;
		}

		const networkCapacities = parseResult.data;

		// Only update if newer or first time seeing capacities
		if (!lastNetworkTimestamp || (networkTimestamp && networkTimestamp > lastNetworkTimestamp)) {
			holsterCapacities.set(networkCapacities);
			if (networkTimestamp) {
				lastNetworkTimestamp = networkTimestamp;
				// Cache for faster future loads
				setCachedCapacities(networkCapacities, networkTimestamp);
			}
			// Track capacity IDs for deletion detection
			lastKnownCapacityIds = new Set(Object.keys(networkCapacities));
			console.log(
				'[CAPACITIES-HOLSTER] Updated capacities:',
				Object.keys(networkCapacities).length
			);
		} else {
			console.log('[CAPACITIES-HOLSTER] Skipping stale update');
		}
	};

	// Subscribe with on() - this watches the entire capacities object graph
	holsterUser.get('capacities').on(capacitiesCallback);
}

/**
 * Initialize capacities subscription when user logs in
 * Loads the object graph structure
 */
export function initializeHolsterCapacities() {
	if (!holsterUser.is) {
		console.log('[CAPACITIES-HOLSTER] Cannot initialize: no authenticated user');
		return;
	}

	if (isInitialized) {
		console.log('[CAPACITIES-HOLSTER] Already initialized, skipping duplicate call');
		return;
	}

	console.log('[CAPACITIES-HOLSTER] Initializing capacities...');
	isInitialized = true;
	isLoadingHolsterCapacities.set(true);

	// Try to load from cache first for instant UI
	const cachedCapacities = getCachedCapacities();
	const cachedTimestamp = cachedCapacities
		? parseInt(localStorage.getItem(CAPACITIES_CACHE_TIMESTAMP_KEY) || '0', 10)
		: 0;

	if (cachedCapacities) {
		console.log('[CAPACITIES-HOLSTER] Using cached capacities for instant UI');
		holsterCapacities.set(cachedCapacities);
		lastNetworkTimestamp = cachedTimestamp;
		lastKnownCapacityIds = new Set(Object.keys(cachedCapacities));
		isLoadingHolsterCapacities.set(false);

		// Continue loading from network in background to check for updates
		console.log('[CAPACITIES-HOLSTER] Fetching from network to check for updates...');
	}

	// Load initial data with get() - receives object graph
	holsterUser.get('capacities', (data: any) => {
		console.log('[CAPACITIES-HOLSTER] Initial load:', data);

		if (data) {
			// Helper to check if a value is "deleted" (null, undefined, or object with missing required fields)
			const isDeleted = (value: any, key?: string): boolean => {
				if (value === null || value === undefined) {
					console.log(`[CAPACITIES-HOLSTER] Skipping deleted entry${key ? ` (${key})` : ''}: null/undefined`);
					return true;
				}
				if (typeof value === 'object' && value !== null) {
					// Check if required capacity fields are missing (indicates deletion)
					if (!value.id || !value.name) {
						console.warn(`[CAPACITIES-HOLSTER] Skipping malformed entry${key ? ` (${key})` : ''}: missing required fields (id or name)`, value);
						return true;
					}
					// Check if all fields are null or undefined (Holster deletion pattern)
					const allFieldsNull = Object.values(value).every((v) => v === null || v === undefined);
					if (allFieldsNull) {
						console.log(`[CAPACITIES-HOLSTER] Skipping deleted entry${key ? ` (${key})` : ''}: all fields null/undefined`);
						return true;
					}
				}
				return false;
			};

			// Extract timestamp and filter out metadata fields AND null/deleted values
			const timestamp = getTimestamp(data);
			const capacitiesOnly: any = {};
			for (const [key, value] of Object.entries(data)) {
				if (!isDeleted(value, key) && !key.startsWith('_')) {
					capacitiesOnly[key] = value;
				}
			}

			// Convert availability_slots objects back to arrays
			for (const [capacityId, capacity] of Object.entries(capacitiesOnly)) {
				if ((capacity as any).availability_slots && typeof (capacity as any).availability_slots === 'object') {
					const slotsObj = (capacity as any).availability_slots;
					const slotsArray: any[] = [];
					for (const slotId of Object.keys(slotsObj)) {
						if (slotsObj[slotId]) { // Only include non-null/undefined slots
							slotsArray.push(slotsObj[slotId]);
						}
					}
					(capacity as any).availability_slots = slotsArray;
				} else {
					(capacity as any).availability_slots = [];
				}
			}

			// Parse and validate (without timestamp)
			const parseResult = CapacitiesCollectionSchema.safeParse(capacitiesOnly);
			if (parseResult.success) {
				// Only update if newer than cache or no cache
				if (!lastNetworkTimestamp || (timestamp && timestamp > lastNetworkTimestamp)) {
					holsterCapacities.set(parseResult.data);

					// Track initial timestamp
					if (timestamp) {
						lastNetworkTimestamp = timestamp;
						// Cache for faster future loads
						setCachedCapacities(parseResult.data, timestamp);
					}

					// Track capacity IDs for deletion detection
					lastKnownCapacityIds = new Set(Object.keys(parseResult.data));
				} else {
					console.log('[CAPACITIES-HOLSTER] Cached data is newer, skipping network data');
				}
			} else {
				console.error('[CAPACITIES-HOLSTER] Invalid initial data:', parseResult.error);
				holsterCapacities.set(null);
			}
		} else {
			holsterCapacities.set(null);
		}

		isLoadingHolsterCapacities.set(false);

		// Subscribe to updates
		subscribeToCapacities();
	});
}

/**
 * Cleanup subscription
 */
export function cleanupHolsterCapacities() {
	if (capacitiesCallback) {
		holsterUser.get('capacities').off(capacitiesCallback);
		capacitiesCallback = null;
	}
	holsterCapacities.set(null);
	lastNetworkTimestamp = null;
	lastKnownCapacityIds.clear();
	isInitialized = false; // Reset initialization flag
	console.log('[CAPACITIES-HOLSTER] Cleaned up');
}

// ============================================================================
// Persistence
// ============================================================================

/**
 * Convert array to object using pattern: for (let i = 0; i < a.length; i++) o[a[i].id] = a[i]
 */
function arrayToObject<T extends { id: string }>(arr: T[]): Record<string, T> {
	const obj: Record<string, T> = {};
	for (let i = 0; i < arr.length; i++) {
		obj[arr[i].id] = arr[i];
	}
	return obj;
}

/**
 * Convert object back to array using pattern: for (const key of Object.keys(o)) a.push(o[key])
 */
function objectToArray<T>(obj: Record<string, T>): T[] {
	const arr: T[] = [];
	for (const key of Object.keys(obj)) {
		arr.push(obj[key]);
	}
	return arr;
}

/**
 * Clean capacities data by removing undefined values and converting arrays to objects
 * Holster/Gun cannot handle undefined or arrays - must use null or omit field, and convert arrays to objects
 */
function cleanCapacitiesData(capacities: CapacitiesCollection): any {
	const cleaned: any = {};

	for (const [capacityId, capacity] of Object.entries(capacities)) {
		// Create base capacity with required fields
		const cleanedCapacity: any = {
			id: capacity.id,
			name: capacity.name
		};

		// Convert availability_slots array to object using slot IDs as keys
		if (capacity.availability_slots && capacity.availability_slots.length > 0) {
			const slotsObject: any = {};
			for (let i = 0; i < capacity.availability_slots.length; i++) {
				const slot = capacity.availability_slots[i];
				const cleanedSlot: any = {
					id: slot.id,
					quantity: slot.quantity
				};

				// Only include optional slot fields if they exist
				if (slot.advance_notice_hours !== undefined)
					cleanedSlot.advance_notice_hours = slot.advance_notice_hours;
				if (slot.booking_window_hours !== undefined)
					cleanedSlot.booking_window_hours = slot.booking_window_hours;
				if (slot.all_day !== undefined) cleanedSlot.all_day = slot.all_day;
				if (slot.recurrence !== undefined) cleanedSlot.recurrence = slot.recurrence;
				if (slot.custom_recurrence_repeat_every !== undefined)
					cleanedSlot.custom_recurrence_repeat_every = slot.custom_recurrence_repeat_every;
				if (slot.custom_recurrence_repeat_unit !== undefined)
					cleanedSlot.custom_recurrence_repeat_unit = slot.custom_recurrence_repeat_unit;
				if (slot.custom_recurrence_end_type !== undefined)
					cleanedSlot.custom_recurrence_end_type = slot.custom_recurrence_end_type;
				if (slot.custom_recurrence_end_value !== undefined)
					cleanedSlot.custom_recurrence_end_value = slot.custom_recurrence_end_value;
				if (slot.start_date !== undefined) cleanedSlot.start_date = slot.start_date;
				if (slot.start_time !== undefined) cleanedSlot.start_time = slot.start_time;
				if (slot.end_date !== undefined) cleanedSlot.end_date = slot.end_date;
				if (slot.end_time !== undefined) cleanedSlot.end_time = slot.end_time;
				if (slot.time_zone) cleanedSlot.time_zone = slot.time_zone;
				if (slot.location_type) cleanedSlot.location_type = slot.location_type;
				if (slot.longitude !== undefined) cleanedSlot.longitude = slot.longitude;
				if (slot.latitude !== undefined) cleanedSlot.latitude = slot.latitude;
				if (slot.street_address) cleanedSlot.street_address = slot.street_address;
				if (slot.city) cleanedSlot.city = slot.city;
				if (slot.state_province) cleanedSlot.state_province = slot.state_province;
				if (slot.postal_code) cleanedSlot.postal_code = slot.postal_code;
				if (slot.country) cleanedSlot.country = slot.country;
				if (slot.online_link) cleanedSlot.online_link = slot.online_link;
				if (slot.parent_slot_id) cleanedSlot.parent_slot_id = slot.parent_slot_id;
				if (slot.mutual_agreement_required !== undefined)
					cleanedSlot.mutual_agreement_required = slot.mutual_agreement_required;
				if (slot.priority !== undefined) cleanedSlot.priority = slot.priority;

				slotsObject[slot.id] = cleanedSlot;
			}
			cleanedCapacity.availability_slots = slotsObject;
		} else {
			cleanedCapacity.availability_slots = {};
		}

		// Only include optional fields if they have actual values
		if (capacity.emoji) cleanedCapacity.emoji = capacity.emoji;
		if (capacity.unit) cleanedCapacity.unit = capacity.unit;
		if (capacity.description) cleanedCapacity.description = capacity.description;
		if (capacity.max_natural_div !== undefined)
			cleanedCapacity.max_natural_div = capacity.max_natural_div;
		if (capacity.max_percentage_div !== undefined)
			cleanedCapacity.max_percentage_div = capacity.max_percentage_div;
		if (capacity.hidden_until_request_accepted !== undefined)
			cleanedCapacity.hidden_until_request_accepted = capacity.hidden_until_request_accepted;
		if (capacity.owner_id) cleanedCapacity.owner_id = capacity.owner_id;
		if (capacity.filter_rule !== undefined) cleanedCapacity.filter_rule = capacity.filter_rule;

		// Check if this is a recipient capacity (has provider_id)
		if ('provider_id' in capacity) {
			cleanedCapacity.provider_id = (capacity as any).provider_id;
		}

		cleaned[capacityId] = cleanedCapacity;
	}

	return cleaned;
}

/**
 * Persist capacities collection to Holster with conflict detection
 * Uses object graph structure - each capacity is stored as a separate node
 */
export async function persistHolsterCapacities(
	capacities?: CapacitiesCollection
): Promise<void> {
	console.log('[CAPACITIES-HOLSTER] persistHolsterCapacities called with', capacities ? Object.keys(capacities).length : 'store', 'capacities');

	if (!holsterUser.is) {
		console.log('[CAPACITIES-HOLSTER] Not authenticated, skipping persistence');
		return;
	}

	if (get(isLoadingHolsterCapacities)) {
		console.log('[CAPACITIES-HOLSTER] Still loading, deferring persistence');
		setTimeout(() => {
			if (!get(isLoadingHolsterCapacities)) {
				persistHolsterCapacities(capacities);
			}
		}, 500);
		return;
	}

	const capacitiesToSave = capacities || get(holsterCapacities);

	if (!capacitiesToSave || Object.keys(capacitiesToSave).length === 0) {
		console.log('[CAPACITIES-HOLSTER] No capacities to persist (empty collection)');
		return;
	}

	console.log('[CAPACITIES-HOLSTER] Starting persistence for', Object.keys(capacitiesToSave).length, 'capacities');

	try {
		// Process addresses for geocoding (same as Gun version)
		const capacitiesWithCoordinates = await processCapacitiesLocations(capacitiesToSave);

		// Clean the data (remove undefined values)
		const cleanedCapacities = cleanCapacitiesData(capacitiesWithCoordinates);

		// Add timestamp to the entire collection
		const timestampedCapacities = addTimestamp(cleanedCapacities);
		const localTimestamp = getTimestamp(timestampedCapacities);

		console.log('[CAPACITIES-HOLSTER] Timestamp check: local=', localTimestamp, 'network=', lastNetworkTimestamp);

		// Check if safe to persist
		if (!shouldPersist(localTimestamp, lastNetworkTimestamp)) {
			console.warn('[CAPACITIES-HOLSTER] Skipping persist - network has newer data. Local:', localTimestamp, 'Network:', lastNetworkTimestamp);
			return;
		}

		console.log('[CAPACITIES-HOLSTER] Timestamp check passed, proceeding with persistence');

		// Detect deleted capacities (IDs that were in Holster but aren't in new collection)
		const currentCapacityIds = new Set(Object.keys(cleanedCapacities));
		const deletedCapacityIds = Array.from(lastKnownCapacityIds).filter(
			id => !currentCapacityIds.has(id)
		);

		if (deletedCapacityIds.length > 0) {
			console.log('[CAPACITIES-HOLSTER] Detected', deletedCapacityIds.length, 'deleted capacities:', deletedCapacityIds);
		}

		// Store as object graph - put the entire collection with timestamp
		console.log('[CAPACITIES-HOLSTER] Persisting', Object.keys(cleanedCapacities).length, 'capacities');

		return new Promise(async (resolve, reject) => {
			// First, explicitly null out deleted capacities
			for (const deletedId of deletedCapacityIds) {
				console.log('[CAPACITIES-HOLSTER] Explicitly nulling deleted capacity:', deletedId);
				await new Promise<void>((resolveDelete) => {
					holsterUser.get('capacities').next(deletedId).put(null, (err: any) => {
						if (err) {
							console.warn('[CAPACITIES-HOLSTER] Error nulling deleted capacity:', deletedId, err);
						}
						resolveDelete();
					});
				});
			}

			// Then persist the current collection
			holsterUser.get('capacities').put(timestampedCapacities, (err: any) => {
				if (err) {
					console.error('[CAPACITIES-HOLSTER] Persist error:', err);
					reject(err);
				} else {
					console.log('[CAPACITIES-HOLSTER] Persisted successfully');
					if (localTimestamp) {
						lastNetworkTimestamp = localTimestamp;
						// Cache for faster future loads (cache the original data, not cleaned)
						setCachedCapacities(capacitiesWithCoordinates, localTimestamp);
					}
					// Update tracking set to current IDs
					lastKnownCapacityIds = currentCapacityIds;
					resolve();
				}
			});
		});
	} catch (error) {
		console.error('[CAPACITIES-HOLSTER] Error processing capacities:', error);
		throw error;
	}
}

// ============================================================================
// CRUD Operations (for compatibility with existing API)
// ============================================================================

/**
 * Delete a capacity from Holster
 * After deletion, persists the entire collection to update the timestamp
 * This ensures the deletion is permanent and won't be restored on reload
 */
export async function deleteHolsterCapacity(capacityId: string): Promise<void> {
	if (!holsterUser.is) {
		console.log('[CAPACITIES-HOLSTER] Not authenticated, skipping delete');
		return;
	}

	console.log('[CAPACITIES-HOLSTER] Deleting capacity:', capacityId);

	// Optimistically update local state
	const updatedCapacities = await new Promise<CapacitiesCollection>((resolve) => {
		holsterCapacities.update((capacities) => {
			if (!capacities) {
				resolve({});
				return null;
			}
			const { [capacityId]: deleted, ...remaining } = capacities;
			resolve(remaining);
			return remaining;
		});
	});

	// Remove from Holster by setting to null using .next() for object graph traversal
	await new Promise((resolve, reject) => {
		holsterUser.get('capacities').next(capacityId).put(null, (err: any) => {
			if (err) {
				console.error('[CAPACITIES-HOLSTER] Delete error:', err);
				reject(err);
			} else {
				console.log('[CAPACITIES-HOLSTER] Deleted from Holster:', capacityId);
				resolve(null);
			}
		});
	});

	// CRITICAL: Persist the entire collection to update the timestamp
	// This ensures the deletion is permanent and won't be restored on reload
	console.log('[CAPACITIES-HOLSTER] Persisting collection after deletion to update timestamp');
	await persistHolsterCapacities(updatedCapacities);
}

/**
 * Update the capacities store and persist
 * This is a helper that maintains the same API as the Gun version
 */
export async function updateHolsterCapacitiesStore(
	updatedCapacities: CapacitiesCollection
): Promise<void> {
	holsterCapacities.set(updatedCapacities);
	await persistHolsterCapacities(updatedCapacities);
}
