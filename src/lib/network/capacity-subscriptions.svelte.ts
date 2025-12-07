/**
 * Slot Subscription Module (Unified for Capacity & Need)
 * 
 * Manages subscription to others' slot declarations with filter-based auto-population.
 * Uses createStore() from store.svelte.ts for Holster persistence and sync.
 * 
 * V2 Unified Architecture:
 * - Single subscription store: Who to subscribe to + what types (capacity/need/both)
 * - Single filter store: Applies to capacity, need, or both
 * - Unified caching: All slots from network
 * - Auto-populate: Filtered slots merged based on applies_to
 * 
 * Pattern:
 * 1. Subscribe to someone's slots: { pubkey: { capacity: true, needs: false } }
 * 2. Their slots arrive via network → cached
 * 3. Filters applied (union: match ANY enabled filter, respecting applies_to)
 * 4. Matching slots auto-populate your own capacities/needs
 */

import { writable, get } from 'svelte/store';
import type { Writable } from 'svelte/store';
import { createStore } from '$lib/utils/primitives/store.svelte';
import type {
	SlotSubscriptions,
	SlotFiltersCollection,
	SlotFilter,
	AvailabilitySlot,
	NeedSlot
} from '$lib/protocol/core/schemas';
import {
	SlotSubscriptionsSchema,
	SlotFiltersCollectionSchema,
	SlotFilterSchema
} from '$lib/protocol/core/schemas';

// ═══════════════════════════════════════════════════════════════════
// UNIFIED STORES (Holster-backed via createStore)
// ═══════════════════════════════════════════════════════════════════

/**
 * Slot Subscriptions - Unified!
 * 
 * Maps pubkey → { capacity: bool, needs: bool }
 * Single store for both capacity and need subscriptions
 * 
 * Examples:
 * - Subscribe to Alice's capacities: { 'alice_pub': { capacity: true, needs: false } }
 * - Subscribe to Bob's needs: { 'bob_pub': { capacity: false, needs: true } }
 * - Subscribe to Carol's both: { 'carol_pub': { capacity: true, needs: true } }
 */
export const slotSubscriptions = createStore({
	holsterPath: 'slot-subscriptions',
	schema: SlotSubscriptionsSchema,
	persistDebounce: 200
});

/**
 * Slot Filters - Unified!
 * 
 * Single filter collection applies to capacity, need, or both
 * Each filter specifies applies_to: 'capacity' | 'need' | 'both'
 * Multiple filters combined with OR (union) - match ANY enabled filter
 */
export const slotFilters = createStore({
	holsterPath: 'slot-filters',
	schema: SlotFiltersCollectionSchema,
	persistDebounce: 200
});


// ═══════════════════════════════════════════════════════════════════
// CACHE STORES (In-memory, populated by network)
// ═══════════════════════════════════════════════════════════════════

/**
 * Capacity Cache
 * 
 * Maps source_pubkey → their capacity slots
 * Populated when we subscribe to their commitments
 */
export const capacityCache: Writable<Record<string, AvailabilitySlot[]>> = writable({});

/**
 * Need Cache
 * 
 * Maps source_pubkey → their need slots
 * Populated when we subscribe to their commitments
 */
export const needCache: Writable<Record<string, NeedSlot[]>> = writable({});

// ═══════════════════════════════════════════════════════════════════
// LIFECYCLE FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Initialize slot subscription stores (unified!)
 * Call this when user logs in
 */
export function initializeCapacitySubscriptions() {
	slotSubscriptions.initialize();
	slotFilters.initialize();
	console.log('[SLOT-SUBS] Initialized unified stores');
}

/**
 * Cleanup slot subscription stores (unified!)
 * Call this when user logs out
 */
export async function cleanupCapacitySubscriptions() {
	await slotSubscriptions.cleanup();
	await slotFilters.cleanup();
	capacityCache.set({});
	needCache.set({});
	console.log('[SLOT-SUBS] Cleaned up');
}

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION OPERATIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Subscribe to a user's slots (capacity, needs, or both) - UNIFIED!
 * 
 * @param pubkey - Public key of user to subscribe to
 * @param types - What to subscribe to: { capacity?: bool, needs?: bool }
 */
export function subscribeToSlots(
	pubkey: string,
	types: { capacity?: boolean; needs?: boolean } = { capacity: true, needs: true }
): void {
	const currentSubs = get(slotSubscriptions) || {};
	slotSubscriptions.set({
		...currentSubs,
		[pubkey]: {
			capacity: types.capacity ?? false,
			needs: types.needs ?? false
		}
	});
	console.log(`[SLOT-SUBS] ✅ Subscribed to ${pubkey.slice(0, 20)}...:`, types);
}

/**
 * Unsubscribe from a user's slots completely - UNIFIED!
 */
export function unsubscribeFromSlots(pubkey: string): void {
	const currentSubs = get(slotSubscriptions);
	if (!currentSubs) return;

	const { [pubkey]: removed, ...remaining } = currentSubs;
	slotSubscriptions.set(remaining);

	// Also clear from cache
	capacityCache.update((cache) => {
		const { [pubkey]: removedCache, ...remainingCache } = cache;
		return remainingCache;
	});
	needCache.update((cache) => {
		const { [pubkey]: removedCache, ...remainingCache } = cache;
		return remainingCache;
	});

	console.log(`[SLOT-SUBS] ❌ Unsubscribed from ${pubkey.slice(0, 20)}...`);
}

/**
 * Update subscription types for a user - UNIFIED!
 * 
 * @param pubkey - Public key of user
 * @param types - What to subscribe to: { capacity?: bool, needs?: bool }
 */
export function updateSlotSubscription(
	pubkey: string,
	types: { capacity?: boolean; needs?: boolean }
): void {
	const currentSubs = get(slotSubscriptions) || {};
	const existing = currentSubs[pubkey] || { capacity: false, needs: false };
	slotSubscriptions.set({
		...currentSubs,
		[pubkey]: {
			capacity: types.capacity ?? existing.capacity,
			needs: types.needs ?? existing.needs
		}
	});
	console.log(`[SLOT-SUBS] 🔄 Updated subscription for ${pubkey.slice(0, 20)}...:`, types);
}


// ═══════════════════════════════════════════════════════════════════
// FILTER OPERATIONS (UNIFIED!)
// ═══════════════════════════════════════════════════════════════════

/**
 * Create a new slot filter - UNIFIED!
 * 
 * Can apply to capacity, need, or both!
 */
export function createSlotFilter(
	filterData: Omit<SlotFilter, 'filter_id' | 'created_at' | 'updated_at'>
): SlotFilter {
	const now = Date.now();
	const filter_id = `filter_${now}_${Math.random().toString(36).substr(2, 9)}`;

	const newFilter: SlotFilter = {
		filter_id,
		name: filterData.name,
		enabled: filterData.enabled !== undefined ? filterData.enabled : true,
		applies_to: filterData.applies_to || 'both', // NEW: defaults to both!
		source_pubkeys: filterData.source_pubkeys,
		need_type_ids: filterData.need_type_ids,
		must_include_me: filterData.must_include_me,
		must_include_ids: filterData.must_include_ids, // UNIFIED!
		location_max_distance_km: filterData.location_max_distance_km,
		min_quantity: filterData.min_quantity,
		created_at: now,
		updated_at: now
	};

	// Validate the filter data
	const validatedFilter = SlotFilterSchema.parse(newFilter);

	// Add to filters collection
	const currentFilters = get(slotFilters) || {};
	const updatedFilters = {
		...currentFilters,
		[filter_id]: validatedFilter
	};

	slotFilters.set(updatedFilters);

	console.log(`[SLOT-SUBS] Created slot filter (${validatedFilter.applies_to}): ${filter_id}`);
	return validatedFilter;
}

/**
 * Update an existing slot filter - UNIFIED!
 */
export function updateSlotFilter(filter_id: string, updates: Partial<SlotFilter>): void {
	const currentFilters = get(slotFilters);
	if (!currentFilters) {
		console.warn(`[SLOT-SUBS] No filters loaded`);
		return;
	}

	const existingFilter = currentFilters[filter_id];
	if (!existingFilter) {
		console.warn(`[SLOT-SUBS] Filter with ID ${filter_id} not found`);
		return;
	}

	const updatedFilter = {
		...existingFilter,
		...updates,
		filter_id, // Never allow changing filter_id
		updated_at: Date.now()
	};

	// Validate the updated filter
	const validatedFilter = SlotFilterSchema.parse(updatedFilter);

	const updatedFilters = {
		...currentFilters,
		[filter_id]: validatedFilter
	};

	slotFilters.set(updatedFilters);

	console.log(`[SLOT-SUBS] Updated slot filter: ${filter_id}`);
}

/**
 * Delete a slot filter - UNIFIED!
 */
export function deleteSlotFilter(filter_id: string): void {
	const currentFilters = get(slotFilters);
	if (!currentFilters) return;

	const { [filter_id]: deleted, ...remaining } = currentFilters;

	slotFilters.set(remaining);

	console.log(`[SLOT-SUBS] Deleted slot filter: ${filter_id}`);
}


// ═══════════════════════════════════════════════════════════════════
// CACHE ACCESSORS
// ═══════════════════════════════════════════════════════════════════

/**
 * Get all cached capacity slots from all subscribed users
 */
export function getAllCachedCapacities(): AvailabilitySlot[] {
	const cache = get(capacityCache);
	const allSlots: AvailabilitySlot[] = [];

	for (const slots of Object.values(cache)) {
		allSlots.push(...slots);
	}

	return allSlots;
}

/**
 * Get all cached need slots from all subscribed users
 */
export function getAllCachedNeeds(): NeedSlot[] {
	const cache = get(needCache);
	const allSlots: NeedSlot[] = [];

	for (const slots of Object.values(cache)) {
		allSlots.push(...slots);
	}

	return allSlots;
}

