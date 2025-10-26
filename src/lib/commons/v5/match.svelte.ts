/**
 * Slot-to-Slot Matching Logic v5 - Multi-Dimensional Framework
 * 
 * Pure functions for matching need slots to availability slots based on:
 * - **Type compatibility (need_type_id matching) - NEW in v5**
 * - Time compatibility (date/time range overlap)
 * - Location compatibility (city/country/coordinates/online)
 * - Quantity constraints
 * 
 * v5 changes:
 * - Uses v5 schemas with required need_type_id
 * - Type matching is built into slotsCompatible()
 * - Pure multi-dimensional design
 */

import type { AvailabilitySlot, NeedSlot } from './schemas';

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
 * Check if a need slot can be fulfilled by an availability slot (v5 - Multi-Dimensional)
 * 
 * COMPATIBILITY REQUIREMENTS:
 * - **Type match: need_type_id must be identical (E28' - CRITICAL for multi-dimensional)**
 * - Time compatibility: date/time ranges must overlap
 * - Location compatibility: city/country/coordinates must match
 * 
 * FILTER LOGIC:
 * - Availability slot filter: Who can RECEIVE from this slot
 * - Need slot filter: Who can PROVIDE to fulfill this need
 * - Both filters must pass for compatibility
 * 
 * Note: Filter checking requires provider/recipient context and should be done
 * in the allocation algorithm (see algorithm.svelte.ts via passesSlotFilters)
 * 
 * @param needSlot - The need slot to check
 * @param availabilitySlot - The availability slot to check
 * @returns true if slots are compatible (type AND time AND location match)
 */
export function slotsCompatible(needSlot: NeedSlot, availabilitySlot: AvailabilitySlot): boolean {
	// **V5 CRITICAL: Type matching (E28')**
	// Different need types CANNOT be matched (no cross-type allocation)
	if (needSlot.need_type_id !== availabilitySlot.need_type_id) {
		return false;
	}

	// Check time compatibility
	if (!timeRangesOverlap(needSlot, availabilitySlot)) {
		return false;
	}

	// Check location compatibility
	if (!locationsCompatible(needSlot, availabilitySlot)) {
		return false;
	}

	// All checks passed: type + time + location compatible!
	return true;
}

// ═══════════════════════════════════════════════════════════════════
// FILTER TYPES AND EVALUATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Filter context for evaluation
 */
export interface FilterContext {
	pubKey: string;
	commitment?: any;
	mutualRecognition?: number;
	attributes?: Record<string, any>;
}

/**
 * Standard filter rule types
 */
export type FilterRule =
	| { type: 'trust'; min_mutual_recognition?: number; only_mutual?: boolean }
	| { type: 'location'; allowed_cities?: string[]; allowed_countries?: string[]; max_distance_km?: number }
	| { type: 'attribute'; required?: string[]; forbidden?: string[] }
	| { type: 'certification'; required?: string[]; min_level?: number }
	| { type: 'resource_type'; allowed_types?: string[]; forbidden_types?: string[] }
	| { type: 'custom'; fn: string }
	| { type: 'allow_all' }
	| { type: 'deny_all' }
	| any; // Legacy/unknown filters

/**
 * Evaluate a single filter rule against a context
 * 
 * @param filter - The filter rule to evaluate
 * @param context - The context (person/entity) being evaluated
 * @returns true if filter passes, false if rejected
 */
export function evaluateFilter(filter: FilterRule | null | undefined, context: FilterContext): boolean {
	// No filter = pass by default (optimistic)
	if (!filter) return true;
	
	try {
		switch (filter.type) {
			case 'allow_all':
				return true;
			
			case 'deny_all':
				return false;
			
			case 'trust': {
				// Trust-based filter: require minimum mutual recognition
				if (filter.only_mutual) {
					// Require mutual recognition > 0
					const mr = context.mutualRecognition || 0;
					if (mr <= 0) {
						console.log(`[FILTER-REJECT:TRUST] ${context.pubKey.slice(0,8)} - only_mutual required, MR=${mr}`);
						return false;
					}
				}
				
				if (filter.min_mutual_recognition !== undefined) {
					const mr = context.mutualRecognition || 0;
					if (mr < filter.min_mutual_recognition) {
						console.log(`[FILTER-REJECT:TRUST] ${context.pubKey.slice(0,8)} - requires MR>=${filter.min_mutual_recognition}, has ${mr}`);
						return false;
					}
				}
				
				return true;
			}
			
			case 'location': {
				// Location-based filter
				if (!context.commitment) return true; // Optimistic if no commitment data
				
				const commitment = context.commitment;
				
				// Check allowed cities
				if (filter.allowed_cities && filter.allowed_cities.length > 0) {
					const city = commitment.city?.toLowerCase();
					if (!city || !filter.allowed_cities.some((c: string) => c.toLowerCase() === city)) {
						console.log(`[FILTER-REJECT:LOCATION] ${context.pubKey.slice(0,8)} - city not in allowed list`);
						return false;
					}
				}
				
				// Check allowed countries
				if (filter.allowed_countries && filter.allowed_countries.length > 0) {
					const country = commitment.country?.toLowerCase();
					if (!country || !filter.allowed_countries.some((c: string) => c.toLowerCase() === country)) {
						console.log(`[FILTER-REJECT:LOCATION] ${context.pubKey.slice(0,8)} - country not in allowed list`);
						return false;
					}
				}
				
				// Check distance (if coordinates available)
				if (filter.max_distance_km && context.commitment.latitude && context.commitment.longitude) {
					// Distance checking would need reference coordinates from the other party
					// For now, we'll pass this check
					console.log(`[FILTER-INFO:LOCATION] Distance check not yet implemented`);
				}
				
				return true;
			}
			
			case 'attribute': {
				// Attribute-based filter: require/forbid specific attributes
				const attrs = context.attributes || {};
				
				// Check required attributes
				if (filter.required && filter.required.length > 0) {
					for (const requiredAttr of filter.required) {
						if (!attrs[requiredAttr]) {
							console.log(`[FILTER-REJECT:ATTRIBUTE] ${context.pubKey.slice(0,8)} - missing required attribute: ${requiredAttr}`);
							return false;
						}
					}
				}
				
				// Check forbidden attributes
				if (filter.forbidden && filter.forbidden.length > 0) {
					for (const forbiddenAttr of filter.forbidden) {
						if (attrs[forbiddenAttr]) {
							console.log(`[FILTER-REJECT:ATTRIBUTE] ${context.pubKey.slice(0,8)} - has forbidden attribute: ${forbiddenAttr}`);
							return false;
						}
					}
				}
				
				return true;
			}
			
			case 'certification': {
				// Certification-based filter
				const attrs = context.attributes || {};
				const certifications = attrs.certifications || [];
				
				if (filter.required && filter.required.length > 0) {
					for (const requiredCert of filter.required) {
						if (!certifications.includes(requiredCert)) {
							console.log(`[FILTER-REJECT:CERTIFICATION] ${context.pubKey.slice(0,8)} - missing certification: ${requiredCert}`);
							return false;
						}
					}
				}
				
				if (filter.min_level !== undefined) {
					const level = attrs.certification_level || 0;
					if (level < filter.min_level) {
						console.log(`[FILTER-REJECT:CERTIFICATION] ${context.pubKey.slice(0,8)} - level ${level} < ${filter.min_level}`);
						return false;
					}
				}
				
				return true;
			}
			
			case 'resource_type': {
				// Resource type filter (for multi-type resources)
				const resourceType = context.commitment?.resource_type;
				
				if (filter.allowed_types && filter.allowed_types.length > 0) {
					if (!resourceType || !filter.allowed_types.includes(resourceType)) {
						console.log(`[FILTER-REJECT:RESOURCE_TYPE] ${context.pubKey.slice(0,8)} - type "${resourceType}" not in allowed list`);
						return false;
					}
				}
				
				if (filter.forbidden_types && filter.forbidden_types.length > 0) {
					if (resourceType && filter.forbidden_types.includes(resourceType)) {
						console.log(`[FILTER-REJECT:RESOURCE_TYPE] ${context.pubKey.slice(0,8)} - type "${resourceType}" is forbidden`);
						return false;
					}
				}
				
				return true;
			}
			
			case 'custom': {
				// Custom filter function (serialized)
				console.log(`[FILTER-WARN] Custom filter functions not yet implemented for security`);
				return true; // Be optimistic until implemented
			}
			
			default: {
				// Unknown filter type - log and be optimistic
				console.log(`[FILTER-WARN] Unknown filter type:`, filter);
				return true;
			}
		}
	} catch (error) {
		console.error(`[FILTER-ERROR] Error evaluating filter:`, error, filter);
		return false; // Be pessimistic on errors for safety
	}
}

/**
 * Check if a provider-recipient pair passes bilateral filters
 * 
 * BILATERAL FILTER CHECKING:
 * - Capacity filter (availSlot.filter_rule): Does recipient pass provider's filter?
 * - Need filter (needSlot.filter_rule): Does provider pass recipient's filter?
 * - Both must pass for allocation to occur
 * 
 * @param needSlot - Recipient's need slot (with optional filter on providers)
 * @param availabilitySlot - Provider's availability slot (with optional filter on recipients)
 * @param providerContext - Provider's context for evaluation
 * @param recipientContext - Recipient's context for evaluation
 * @returns true if both filters pass (or no filters present)
 */
export function passesSlotFilters(
	needSlot: NeedSlot,
	availabilitySlot: AvailabilitySlot,
	providerContext: FilterContext,
	recipientContext: FilterContext
): boolean {
	// Check availability slot filter (who can receive from provider)
	// Provider is checking if recipient passes their filter
	if (availabilitySlot.filter_rule) {
		if (!evaluateFilter(availabilitySlot.filter_rule, recipientContext)) {
			console.log(`[FILTER-BILATERAL-REJECT] Recipient ${recipientContext.pubKey.slice(0,8)} failed provider's capacity filter`);
			return false;
		}
	}
	
	// Check need slot filter (who can provide to recipient)
	// Recipient is checking if provider passes their filter
	if (needSlot.filter_rule) {
		if (!evaluateFilter(needSlot.filter_rule, providerContext)) {
			console.log(`[FILTER-BILATERAL-REJECT] Provider ${providerContext.pubKey.slice(0,8)} failed recipient's need filter`);
			return false;
		}
	}
	
	// Both filters passed (or no filters present)
	return true;
}

// ═══════════════════════════════════════════════════════════════════
// SLOT MATCHING AND QUANTITY CALCULATION (v5: Simplified)
// ═══════════════════════════════════════════════════════════════════

/**
 * NOTE: v5 uses slot-native allocation directly in algorithm.svelte.ts
 * 
 * The core matching logic is in slotsCompatible() above, which checks:
 * - Type compatibility (need_type_id match)
 * - Time compatibility (date/time overlap)
 * - Location compatibility (city/country/coordinates)
 * 
 * The allocation algorithm processes each availability slot independently
 * and finds compatible need slots using slotsCompatible().
 * 
 * Legacy v2 functions (matchNeedToCapacitySlots, etc.) are not needed in v5
 * since the multi-dimensional framework works directly with slot arrays.
 */

// ═══════════════════════════════════════════════════════════════════
// SPACE-TIME GROUPING & BUCKETING
// ═══════════════════════════════════════════════════════════════════

/**
 * Get time bucket key for coarse-grained filtering
 * Uses month-level granularity for fast bucketing
 * 
 * @param slot - Availability or need slot
 * @returns Time bucket key (e.g., "2024-06" or "any-time")
 */
export function getTimeBucketKey(slot: AvailabilitySlot | NeedSlot): string {
	if (slot.start_date) {
		// Month-level bucketing: "YYYY-MM"
		return slot.start_date.substring(0, 7);
	}
	return 'any-time';
}

/**
 * Get location bucket key for coarse-grained filtering
 * Buckets by: remote > city > country > unknown
 * 
 * @param slot - Availability or need slot
 * @returns Location bucket key (e.g., "remote", "san-francisco", "usa", "unknown")
 */
export function getLocationBucketKey(slot: AvailabilitySlot | NeedSlot): string {
	// Remote slots are universally compatible
	if (slot.location_type?.toLowerCase().includes('remote') || 
	    slot.location_type?.toLowerCase().includes('online') ||
	    slot.online_link) {
		return 'remote';
	}
	
	// Use city if available (most specific)
	if (slot.city) {
		return slot.city.toLowerCase();
	}
	
	// Fall back to country
	if (slot.country) {
		return slot.country.toLowerCase();
	}
	
	// No location specified
	return 'unknown';
}

/**
 * Generate a space-time signature for a slot
 * Slots with identical signatures can be aggregated
 * 
 * This is more precise than bucketing and used for exact grouping.
 * 
 * @param slot - Availability or need slot
 * @returns Space-time signature string
 */
export function getSpaceTimeSignature(
	slot: AvailabilitySlot | NeedSlot
): string {
	// Time component (precise)
	const timeKey = [
		slot.start_date || 'any-date',
		slot.start_time || 'any-time',
		slot.end_date || 'any-date',
		slot.end_time || 'any-time',
		slot.recurrence || 'no-recurrence',
		slot.all_day ? 'all-day' : 'specific-time'
	].join('|');
	
	// Location component (precise)
	const locKey = slot.location_type?.toLowerCase().includes('remote') || slot.online_link
		? 'remote'
		: [
			slot.city?.toLowerCase() || 'any-city',
			slot.country?.toLowerCase() || 'any-country',
			slot.latitude?.toFixed(2) || 'any-lat',
			slot.longitude?.toFixed(2) || 'any-lon'
		].join('|');
	
	return `${timeKey}::${locKey}`;
}

/**
 * Group slots by their space-time signature and aggregate quantities
 * Slots at the same time/location are merged
 * 
 * @param slots - Array of slots to group
 * @returns Map of signature -> aggregated quantity
 */
export function groupSlotsBySpaceTime<T extends AvailabilitySlot | NeedSlot>(
	slots: T[]
): Map<string, { quantity: number; slots: T[] }> {
	const groups = new Map<string, { quantity: number; slots: T[] }>();
	
	for (const slot of slots) {
		const signature = getSpaceTimeSignature(slot);
		const existing = groups.get(signature);
		
		if (existing) {
			existing.quantity += slot.quantity;
			existing.slots.push(slot);
		} else {
			groups.set(signature, {
				quantity: slot.quantity,
				slots: [slot]
			});
		}
	}
	
	return groups;
}

// ═══════════════════════════════════════════════════════════════════
// UTILITY FUNCTIONS (v5: Simplified for slot arrays)
// ═══════════════════════════════════════════════════════════════════

/**
 * Calculate total quantity from slot array
 * 
 * NOTE: This sums ALL slots regardless of time/space compatibility.
 * For allocation purposes, the algorithm handles compatibility per-slot.
 * This is useful for reporting/display purposes only.
 */
export function calculateTotalQuantity(slots: (NeedSlot | AvailabilitySlot)[]): number {
	return slots.reduce((sum, slot) => sum + slot.quantity, 0);
}

/**
 * Calculate space-time aware profile from slots
 * Groups slots by space-time signature and returns unique space-time combinations
 * 
 * This is the "true" quantity considering that slots at the same time/location
 * should be aggregated, but slots at different times/locations are separate.
 * 
 * @param slots - Array of slots to profile
 * @returns Array of space-time combinations with aggregated quantities
 */
export function getSpaceTimeProfile<T extends NeedSlot | AvailabilitySlot>(
	slots: T[]
): Array<{
	signature: string;
	quantity: number;
	slots: T[];
}> {
	const groups = groupSlotsBySpaceTime(slots);
	return Array.from(groups.entries()).map(([signature, data]) => ({
		signature,
		quantity: data.quantity,
		slots: data.slots
	}));
}

// ═══════════════════════════════════════════════════════════════════
// SPACE-TIME MATCHING EXAMPLES
// ═══════════════════════════════════════════════════════════════════

/**
 * EXAMPLE 1: Same Space-Time (Should Aggregate)
 * 
 * Provider has:
 *   - Slot A: Monday 9-10am @ SF, quantity 5
 *   - Slot B: Monday 9-10am @ SF, quantity 3
 * 
 * These have the SAME space-time signature, so they aggregate to 8 units
 * available on Monday 9-10am in SF.
 * 
 * In allocation, both slots allocate independently (slot-native), but they're
 * providing to the same space-time, so a recipient with a need at that exact
 * time/place can receive from both.
 */

/**
 * EXAMPLE 2: Same Time, Different Space (Cannot Aggregate)
 * 
 * Provider has:
 *   - Slot A: Monday 9-10am @ SF, quantity 5
 *   - Slot B: Monday 9-10am @ NYC, quantity 3
 * 
 * These have DIFFERENT space-time signatures (different cities).
 * A recipient in SF can only receive from Slot A.
 * A recipient in NYC can only receive from Slot B.
 * Total capacity is NOT 8 - it's 5 in SF and 3 in NYC (separate pools).
 */

/**
 * EXAMPLE 3: Same Space, Different Time (Cannot Aggregate)
 * 
 * Provider has:
 *   - Slot A: Monday 9-10am @ SF, quantity 5
 *   - Slot B: Tuesday 9-10am @ SF, quantity 3
 * 
 * These have DIFFERENT space-time signatures (different days).
 * A recipient needing Monday service can only receive from Slot A (5 units).
 * A recipient needing Tuesday service can only receive from Slot B (3 units).
 * Total capacity is NOT 8 - it's 5 on Monday and 3 on Tuesday (separate pools).
 */

/**
 * EXAMPLE 4: Overlapping Time Ranges
 * 
 * Provider has:
 *   - Slot A: Monday 9-11am @ SF, quantity 5
 *   - Slot B: Monday 10am-12pm @ SF, quantity 3
 * 
 * These overlap but have DIFFERENT signatures (different time ranges).
 * 
 * Recipient with need Monday 10-11am @ SF:
 *   - Compatible with Slot A (within 9-11am range) ✓
 *   - Compatible with Slot B (within 10am-12pm range) ✓
 *   - Can receive from BOTH slots (up to 8 units total)
 * 
 * This is correct! The provider is available during both time windows,
 * and the overlap period has both capacities available.
 */

/**
 * EXAMPLE 5: Remote vs In-Person
 * 
 * Provider has:
 *   - Slot A: Monday 9-10am @ Remote, quantity 5
 *   - Slot B: Monday 9-10am @ SF, quantity 3
 * 
 * These have DIFFERENT signatures (remote vs in-person).
 * 
 * Remote slots are compatible with ANY location (optimistic matching).
 * So a recipient in SF can potentially receive from both Slot A (remote)
 * and Slot B (in-person SF), getting up to 8 units.
 * 
 * This is correct! Remote capacity can serve anyone, anywhere.
 */

/**
 * ALLOCATION ALGORITHM BEHAVIOR:
 * 
 * The slot-native algorithm processes each availability slot independently:
 * 
 * 1. For each availability slot, find all compatible recipients
 * 2. Run two-tier allocation on that slot's quantity
 * 3. Record allocations with slot-to-slot pairing
 * 
 * This means:
 * - Slots at the same space-time allocate independently
 * - A recipient can receive from multiple slots (if compatible)
 * - Space-time compatibility is checked via slotsCompatible()
 * - Filters are checked bilaterally for each pairing
 * 
 * The space-time grouping functions (getSpaceTimeSignature, etc.) are
 * useful for ANALYSIS and VISUALIZATION, but the actual allocation
 * respects slot-level compatibility automatically through the
 * slot-native design.
 */

