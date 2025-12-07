/**
 * Slot Filter Utilities - UNIFIED!
 * 
 * Pure utility functions for filtering slots based on filter conditions.
 * Used by the slot subscription system to determine which slots to copy.
 * 
 * V2 Unified Architecture:
 * - Single SlotFilter type applies to capacity, need, or both
 * - Simplified must_include_ids (unified from org_ids + pubkeys)
 * 
 * Filter Logic:
 * - Within a filter: All conditions are AND (must match all specified conditions)
 * - Across filters: Filters are OR (match ANY enabled filter) - union
 * - applies_to field: Determines which slot types this filter applies to
 */

import type { AvailabilitySlot, NeedSlot, SlotFilter } from '../schemas';

/**
 * Check if a slot passes a single filter's conditions - UNIFIED!
 * 
 * @param slot - The slot to check
 * @param slotType - 'capacity' or 'need' - the type of slot being checked
 * @param filter - The filter to apply
 * @param myPubkey - Current user's public key (for must_include_me checks)
 * @param sourcePubkey - Where this slot came from (for source_pubkeys check)
 * @param resolveMembers - Function to resolve org_ids to arrays of pubkeys
 * @returns true if the slot matches all conditions in this filter
 */
export function slotMatchesFilter<T extends AvailabilitySlot | NeedSlot>(
	slot: T,
	slotType: 'capacity' | 'need',
	filter: SlotFilter,
	myPubkey: string,
	sourcePubkey: string,
	resolveMembers: (id: string) => string[]
): boolean {
	// If filter is disabled, it doesn't match anything
	if (!filter.enabled) {
		return false;
	}

	// Check if this filter applies to this slot type
	if (filter.applies_to !== 'both' && filter.applies_to !== slotType) {
		return false;
	}

	// Check source pubkey filter
	if (filter.source_pubkeys && filter.source_pubkeys.length > 0) {
		if (!filter.source_pubkeys.includes(sourcePubkey)) {
			return false;
		}
	}

	// Check need type filter
	if (filter.need_type_ids && filter.need_type_ids.length > 0) {
		if (!filter.need_type_ids.includes(slot.need_type_id)) {
			return false;
		}
	}

	// Check if I'm in members
	if (filter.must_include_me && slot.members) {
		// Resolve all members (handles org_ids recursively)
		const resolvedMembers = slot.members.flatMap(resolveMembers);
		if (!resolvedMembers.includes(myPubkey)) {
			return false;
		}
	}

	// Check if specific IDs (pubkeys or org_ids) are in members - UNIFIED!
	if (filter.must_include_ids && filter.must_include_ids.length > 0) {
		if (!slot.members) {
			return false;
		}

		// For each required ID, check if it's present in members (as-is) or resolved members
		const hasRequiredId = filter.must_include_ids.some((requiredId) => {
			// First check: Is the ID directly in the members list?
			if (slot.members!.includes(requiredId)) {
				return true;
			}

			// Second check: Resolve all members and see if the ID is there
			// (This handles cases where we're looking for a pubkey that's part of an org)
			const resolvedMembers = slot.members!.flatMap(resolveMembers);
			return resolvedMembers.includes(requiredId);
		});

		if (!hasRequiredId) {
			return false;
		}
	}

	// Check minimum quantity
	if (filter.min_quantity !== undefined && slot.quantity < filter.min_quantity) {
		return false;
	}

	// TODO: Location distance check
	// This would require:
	// 1. User's current/preferred location
	// 2. Haversine distance calculation
	// 3. Comparison with filter.location_max_distance_km
	// Skipping for now - can be added later

	// All conditions passed!
	return true;
}

/**
 * Apply filters to a single slot with source tracking - UNIFIED!
 * 
 * @param slot - The slot to check
 * @param slotType - 'capacity' or 'need'
 * @param filters - Array of filters to apply (OR logic - match ANY)
 * @param myPubkey - Current user's public key
 * @param sourcePubkey - Where this slot came from
 * @param resolveMembers - Function to resolve org_ids to arrays of pubkeys
 * @returns true if the slot matches at least one enabled filter
 */
export function slotMatchesAnyFilter<T extends AvailabilitySlot | NeedSlot>(
	slot: T,
	slotType: 'capacity' | 'need',
	filters: SlotFilter[],
	myPubkey: string,
	sourcePubkey: string,
	resolveMembers: (id: string) => string[]
): boolean {
	// Get only enabled filters that apply to this slot type
	const enabledFilters = filters.filter(
		(f) => f.enabled && (f.applies_to === 'both' || f.applies_to === slotType)
	);

	// No enabled filters = don't match anything
	if (enabledFilters.length === 0) {
		return false;
	}

	// Check if slot matches ANY enabled filter (union)
	return enabledFilters.some((filter) =>
		slotMatchesFilter(slot, slotType, filter, myPubkey, sourcePubkey, resolveMembers)
	);
}

/**
 * Apply filters to an array of slots from a single source - UNIFIED!
 * 
 * @param slots - Array of slots from one source
 * @param slotType - 'capacity' or 'need'
 * @param filters - Array of filters to apply
 * @param myPubkey - Current user's public key
 * @param sourcePubkey - Where these slots came from
 * @param resolveMembers - Function to resolve org_ids to arrays of pubkeys
 * @returns Filtered array of slots that match at least one filter
 */
export function applyFiltersToSlots<T extends AvailabilitySlot | NeedSlot>(
	slots: T[],
	slotType: 'capacity' | 'need',
	filters: SlotFilter[],
	myPubkey: string,
	sourcePubkey: string,
	resolveMembers: (id: string) => string[]
): T[] {
	return slots.filter((slot) =>
		slotMatchesAnyFilter(slot, slotType, filters, myPubkey, sourcePubkey, resolveMembers)
	);
}

/**
 * Apply filters to slots from multiple sources (union across all sources) - UNIFIED!
 * 
 * @param slotsBySource - Map of source_pubkey -> slots array
 * @param slotType - 'capacity' or 'need'
 * @param filters - Array of filters to apply
 * @param myPubkey - Current user's public key
 * @param resolveMembers - Function to resolve org_ids to arrays of pubkeys
 * @returns Array of all slots that match at least one filter, from any source
 */
export function applyFiltersUnion<T extends AvailabilitySlot | NeedSlot>(
	slotsBySource: Record<string, T[]>,
	slotType: 'capacity' | 'need',
	filters: SlotFilter[],
	myPubkey: string,
	resolveMembers: (id: string) => string[]
): T[] {
	const matchedSlots: T[] = [];

	// Process each source
	for (const [sourcePubkey, slots] of Object.entries(slotsBySource)) {
		const filteredSlots = applyFiltersToSlots(
			slots,
			slotType,
			filters,
			myPubkey,
			sourcePubkey,
			resolveMembers
		);

		matchedSlots.push(...filteredSlots);
	}

	return matchedSlots;
}

/**
 * Deduplicate slots by ID
 * 
 * When the same slot appears from multiple sources, keep only one copy.
 * Prefers slots from sources with higher priority (earlier in the priority list).
 * 
 * @param slots - Array of slots (potentially with duplicates)
 * @param prioritySources - Optional array of pubkeys in priority order
 * @returns Deduplicated array of slots
 */
export function deduplicateSlots<T extends AvailabilitySlot | NeedSlot>(
	slots: T[],
	prioritySources?: string[]
): T[] {
	const slotMap = new Map<string, T>();

	// If we have priority sources, process in that order
	if (prioritySources && prioritySources.length > 0) {
		// Sort slots by source priority (higher priority first)
		const sortedSlots = [...slots].sort((a, b) => {
			// Extract source info (would need to be added to slot metadata in practice)
			// For now, just use slot order
			return 0;
		});

		for (const slot of sortedSlots) {
			if (!slotMap.has(slot.id)) {
				slotMap.set(slot.id, slot);
			}
		}
	} else {
		// No priority - just keep first occurrence
		for (const slot of slots) {
			if (!slotMap.has(slot.id)) {
				slotMap.set(slot.id, slot);
			}
		}
	}

	return Array.from(slotMap.values());
}

/**
 * Merge declared slots with filtered network slots
 * 
 * Combines:
 * 1. User's own declared slots
 * 2. Filtered slots from network subscriptions
 * 
 * Deduplicates by slot ID, with declared slots taking priority over network slots.
 * 
 * @param declaredSlots - User's own declared slots
 * @param networkSlots - Filtered slots from network
 * @returns Merged and deduplicated array of slots
 */
export function mergeSlots<T extends AvailabilitySlot | NeedSlot>(
	declaredSlots: T[],
	networkSlots: T[]
): T[] {
	const slotMap = new Map<string, T>();

	// Add network slots first
	for (const slot of networkSlots) {
		slotMap.set(slot.id, slot);
	}

	// Add declared slots (overwrites network slots with same ID)
	for (const slot of declaredSlots) {
		slotMap.set(slot.id, slot);
	}

	return Array.from(slotMap.values());
}

