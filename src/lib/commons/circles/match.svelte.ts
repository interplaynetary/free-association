/**
 * Slot-to-Slot Matching Logic
 * 
 * Pure functions for matching need slots to availability slots based on:
 * - Time compatibility (date/time range overlap)
 * - Location compatibility (city/country/coordinates/online)
 * - Quantity constraints
 * 
 * Extracted from collective-rec.svelte.ts for reusability in circles allocation
 */

import type { AvailabilitySlot, NeedSlot, BaseCapacity, BaseNeed } from './schemas';

// ═══════════════════════════════════════════════════════════════════
// SLOT COMPATIBILITY CHECKING
// ═══════════════════════════════════════════════════════════════════

/**
 * Calculate distance between two points using Haversine formula (in km)
 */
export function haversineDistance(lat1: number, lon1: number, lat2: number, lon2: number): number {
	const R = 6371; // Earth's radius in km
	const dLat = ((lat2 - lat1) * Math.PI) / 180;
	const dLon = ((lon2 - lon1) * Math.PI) / 180;
	const a =
		Math.sin(dLat / 2) * Math.sin(dLat / 2) +
		Math.cos((lat1 * Math.PI) / 180) *
			Math.cos((lat2 * Math.PI) / 180) *
			Math.sin(dLon / 2) *
			Math.sin(dLon / 2);
	const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
	return R * c;
}

/**
 * Check if two time ranges overlap
 * Returns true if there's any overlap, or if time info is missing (optimistic)
 */
export function timeRangesOverlap(
	slot1: { start_date?: string | null; end_date?: string | null; start_time?: string | null; end_time?: string | null },
	slot2: { start_date?: string | null; end_date?: string | null; start_time?: string | null; end_time?: string | null }
): boolean {
	// If either slot has no time info, be optimistic - assume they match
	if (!slot1.start_date && !slot1.end_date && !slot2.start_date && !slot2.end_date) {
		return true;
	}

	// If only one has time info, be optimistic
	if ((!slot1.start_date && !slot1.end_date) || (!slot2.start_date && !slot2.end_date)) {
		return true;
	}

	try {
		// Parse dates (handle both date-only and datetime strings)
		const start1 = slot1.start_date ? new Date(slot1.start_date) : new Date('1900-01-01');
		const end1 = slot1.end_date ? new Date(slot1.end_date) : new Date('2100-12-31');
		const start2 = slot2.start_date ? new Date(slot2.start_date) : new Date('1900-01-01');
		const end2 = slot2.end_date ? new Date(slot2.end_date) : new Date('2100-12-31');

		// Check if date ranges overlap
		// Range1: [start1, end1], Range2: [start2, end2]
		// Overlap if: start1 <= end2 AND start2 <= end1
		return start1 <= end2 && start2 <= end1;
	} catch (e) {
		// If date parsing fails, be optimistic
		return true;
	}
}

/**
 * Check if two locations are compatible
 * Returns true if locations match or are compatible, or if location info is missing (optimistic)
 */
export function locationsCompatible(
	slot1: {
		location_type?: string;
		city?: string;
		country?: string;
		online_link?: string;
		latitude?: number;
		longitude?: number;
	},
	slot2: {
		location_type?: string;
		city?: string;
		country?: string;
		online_link?: string;
		latitude?: number;
		longitude?: number;
	}
): boolean {
	// If neither has location info, be optimistic - assume they match
	const slot1HasLocation = slot1.city || slot1.country || slot1.latitude !== undefined;
	const slot2HasLocation = slot2.city || slot2.country || slot2.latitude !== undefined;

	if (!slot1HasLocation && !slot2HasLocation) {
		return true;
	}

	// If only one has location info, be optimistic
	if (!slot1HasLocation || !slot2HasLocation) {
		return true;
	}

	// If either is online/remote, consider compatible
	if (
		slot1.location_type?.toLowerCase().includes('online') ||
		slot1.location_type?.toLowerCase().includes('remote') ||
		slot2.location_type?.toLowerCase().includes('online') ||
		slot2.location_type?.toLowerCase().includes('remote') ||
		slot1.online_link ||
		slot2.online_link
	) {
		return true;
	}

	// Check country match (case insensitive)
	if (slot1.country && slot2.country) {
		if (slot1.country.toLowerCase() === slot2.country.toLowerCase()) {
			return true;
		}
	}

	// Check city match (case insensitive)
	if (slot1.city && slot2.city) {
		if (slot1.city.toLowerCase() === slot2.city.toLowerCase()) {
			return true;
		}
	}

	// If we have precise coordinates for both, check proximity (within ~50km)
	if (
		slot1.latitude !== undefined &&
		slot1.longitude !== undefined &&
		slot2.latitude !== undefined &&
		slot2.longitude !== undefined
	) {
		const distance = haversineDistance(
			slot1.latitude,
			slot1.longitude,
			slot2.latitude,
			slot2.longitude
		);
		return distance <= 50; // Within 50km
	}

	// If neither country nor city matched, be pessimistic
	// (we have location info but they don't match)
	if ((slot1.country || slot1.city) && (slot2.country || slot2.city)) {
		return false;
	}

	// Default: be optimistic if unclear
	return true;
}

/**
 * Check if a need slot can be fulfilled by an availability slot
 * 
 * @param needSlot - The need slot to check
 * @param availabilitySlot - The availability slot to check
 * @returns true if slots are compatible (time AND location match)
 */
export function slotsCompatible(needSlot: NeedSlot, availabilitySlot: AvailabilitySlot): boolean {
	// Check time compatibility
	if (!timeRangesOverlap(needSlot, availabilitySlot)) {
		return false;
	}

	// Check location compatibility
	if (!locationsCompatible(needSlot, availabilitySlot)) {
		return false;
	}

	// If both passed (or were optimistically matched), they're compatible!
	return true;
}

// ═══════════════════════════════════════════════════════════════════
// SLOT MATCHING AND QUANTITY CALCULATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Result of matching need slots to availability slots
 */
export interface SlotMatchResult {
	need_slot: NeedSlot;
	availability_slot: AvailabilitySlot;
	matchable_quantity: number;
}

/**
 * Complete result of matching a need to a capacity
 */
export interface NeedToCapacityMatchResult {
	compatible_pairs: SlotMatchResult[];
	total_matchable: number;
	unmatched_need_slots: NeedSlot[];
	unmatched_availability_slots: AvailabilitySlot[];
}

/**
 * Match need slots to availability slots and calculate possible allocations
 * 
 * @param need - Need with slots to fulfill
 * @param capacity - Capacity with availability slots
 * @param maxAmount - Maximum amount that can be allocated (from recognition share + filters)
 * @returns Object with matchable slots and total matchable amount
 */
export function matchNeedToCapacitySlots(
	need: BaseNeed,
	capacity: BaseCapacity,
	maxAmount: number
): NeedToCapacityMatchResult {
	const compatible_pairs: SlotMatchResult[] = [];
	const matched_need_slot_ids = new Set<string>();
	const matched_availability_slot_ids = new Set<string>();

	let totalMatchable = 0;

	// Try to match each need slot to availability slots
	for (const needSlot of need.need_slots) {
		for (const availSlot of capacity.availability_slots) {
			// Check if slots are compatible (time/location)
			if (slotsCompatible(needSlot, availSlot)) {
				// Calculate how much we can allocate from this availability slot to this need slot
				// Limited by: need slot quantity, availability slot quantity, and maxAmount
				const matchableQty = Math.min(
					needSlot.quantity,
					availSlot.quantity,
					maxAmount - totalMatchable
				);

				if (matchableQty > 0) {
					compatible_pairs.push({
						need_slot: needSlot,
						availability_slot: availSlot,
						matchable_quantity: matchableQty
					});
					matched_need_slot_ids.add(needSlot.id);
					matched_availability_slot_ids.add(availSlot.id);
					totalMatchable += matchableQty;

					// If we've hit the maxAmount, stop
					if (totalMatchable >= maxAmount) {
						break;
					}
				}
			}
		}

		// If we've hit the maxAmount, stop
		if (totalMatchable >= maxAmount) {
			break;
		}
	}

	// Collect unmatched slots
	const unmatched_need_slots = need.need_slots.filter((s) => !matched_need_slot_ids.has(s.id));
	const unmatched_availability_slots = capacity.availability_slots.filter(
		(s) => !matched_availability_slot_ids.has(s.id)
	);

	return {
		compatible_pairs,
		total_matchable: totalMatchable,
		unmatched_need_slots,
		unmatched_availability_slots
	};
}

// ═══════════════════════════════════════════════════════════════════
// NEED UTILITIES
// ═══════════════════════════════════════════════════════════════════

/**
 * Calculate total need amount from need slots
 */
export function calculateTotalNeedAmount(need: BaseNeed): number {
	return need.need_slots.reduce((sum, slot) => sum + slot.quantity, 0);
}

/**
 * Calculate remaining (unfulfilled) need amount
 */
export function getRemainingNeed(need: BaseNeed): number {
	const totalNeed = calculateTotalNeedAmount(need);
	return Math.max(0, totalNeed - need.fulfilled_amount);
}

/**
 * Calculate how much of a member's need can be fulfilled by a capacity
 * considering slot-level time/location compatibility
 * 
 * @param need - Member's need (with slots)
 * @param capacity - Provider's capacity (with availability slots)
 * @param maxAmount - Maximum allocation (from recognition + filters)
 * @returns Amount that can actually be fulfilled given slot constraints
 */
export function calculateSlotCompatibleAmount(
	need: BaseNeed | undefined,
	capacity: BaseCapacity,
	maxAmount: number
): number {
	if (!need) return 0;

	// Match slots and return total matchable amount
	const matching = matchNeedToCapacitySlots(need, capacity, maxAmount);
	return Math.min(matching.total_matchable, getRemainingNeed(need), maxAmount);
}

