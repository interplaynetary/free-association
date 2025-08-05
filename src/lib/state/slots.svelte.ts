import { writable, derived } from 'svelte/store';
import type { UserSlotClaims, NetworkSlotClaims } from '$lib/schema';
import { userNetworkCapacitiesWithShares, capacityShares } from './core.svelte';

/**
 * Unified Slot Allocation System (Simplified - No Credits)
 *
 * Direct flow: Recognition → Shares → Feasible Claims → Allocated Slots
 * No intermediate "credits" - just direct share-based calculations with clear UI metadata
 */

// Helper function to get slot by ID from a capacity
function getSlotById(capacity: any, slotId: string) {
	return capacity.availability_slots?.find((slot: any) => slot.id === slotId);
}

// Helper function to get total computed quantity from slot-based structure
function getTotalComputedQuantity(capacity: any): number {
	if (capacity.computed_quantities && Array.isArray(capacity.computed_quantities)) {
		return capacity.computed_quantities.reduce(
			(sum: number, slot: any) => sum + (slot.quantity || 0),
			0
		);
	}
	return 0;
}

// Apply divisibility constraints to a specific slot's available quantity
function applySlotConstraints(
	capacity: any,
	slot: any,
	rawQuantity: number
): { constrainedQuantity: number; constraintApplied: boolean; constraintReason?: string } {
	const maxPercent = capacity.max_percentage_div || 1;
	const maxNatural = capacity.max_natural_div || 1;

	// Apply percentage constraint (max % of this specific slot)
	const percentConstrained = Math.min(rawQuantity, slot.quantity * maxPercent);

	// Apply natural divisibility constraint (round to valid increments)
	const naturalConstrained = Math.round(percentConstrained / maxNatural) * maxNatural;

	const finalQuantity = Math.max(0, naturalConstrained);
	const constraintApplied = finalQuantity !== rawQuantity;

	let constraintReason: string | undefined;
	if (constraintApplied) {
		if (percentConstrained < rawQuantity) {
			constraintReason = `Limited to ${(maxPercent * 100).toFixed(1)}% of slot (${(slot.quantity * maxPercent).toFixed(1)} max units)`;
		} else if (naturalConstrained !== percentConstrained) {
			constraintReason = `Rounded to ${maxNatural} unit increments (${finalQuantity} units)`;
		}
	}

	return {
		constrainedQuantity: finalQuantity,
		constraintApplied,
		constraintReason
	};
}

// SLOT DESIRES MODEL:
// Structure: capacityId → slotId → desiredQuantity
// Example: "john-consulting" → "morning-slots" → 4 means:
//   "I want to claim 4 units from John's morning consultation slots"
export const userDesiredSlotClaims = writable<UserSlotClaims>({});

// NETWORK SLOT CLAIMS MODEL:
// What others want to claim from our slots
// Structure: userId → capacityId → slotId → desiredQuantity
export const networkDesiredSlotClaims = writable<NetworkSlotClaims>({});

// FEASIBLE SLOT CLAIMS (Direct Share-Based Calculation)
// No credits - direct calculation: shares → feasible amounts
export const feasibleSlotClaims = derived(
	[userDesiredSlotClaims, userNetworkCapacitiesWithShares],
	([$userDesiredSlotClaims, $userNetworkCapacitiesWithShares]) => {
		console.log('[FEASIBLE-SLOT-CLAIMS] Calculating share-based feasible slot claims...');

		const finalFeasible: UserSlotClaims = {};

		Object.entries($userDesiredSlotClaims).forEach(([capacityId, slotClaims]) => {
			Object.entries(slotClaims).forEach(([slotId, desiredQuantity]) => {
				const capacity = $userNetworkCapacitiesWithShares[capacityId];
				if (!capacity) return;

				const slot = getSlotById(capacity, slotId);
				if (!slot) return;

				// Direct calculation: slot.quantity × our_share_percentage
				const ourSharePercentage =
					'share_percentage' in capacity ? (capacity as any).share_percentage : 0;
				const rawAvailableUnits = slot.quantity * ourSharePercentage;

				// Apply slot-specific divisibility constraints
				const {
					constrainedQuantity: maxAvailableUnits,
					constraintApplied,
					constraintReason
				} = applySlotConstraints(capacity, slot, rawAvailableUnits);

				// Apply share limit constraint
				const finalFeasibleQuantity = Math.min(desiredQuantity, maxAvailableUnits);

				if (finalFeasibleQuantity > 0) {
					if (!finalFeasible[capacityId]) {
						finalFeasible[capacityId] = {};
					}
					finalFeasible[capacityId][slotId] = finalFeasibleQuantity;

					// Log constraint reasons with slot-level constraint details
					if (finalFeasibleQuantity < desiredQuantity) {
						let limitingFactor = `${(ourSharePercentage * 100).toFixed(1)}% share = ${rawAvailableUnits.toFixed(1)} raw units`;
						if (constraintApplied && constraintReason) {
							limitingFactor += ` → ${maxAvailableUnits.toFixed(1)} units (${constraintReason})`;
						}
						console.log(
							`[FEASIBLE-SLOT-CLAIMS] Constrained ${capacityId}:${slotId}: desired ${desiredQuantity} → ${finalFeasibleQuantity} (limited by ${limitingFactor})`
						);
					}
				}
			});
		});

		console.log(
			`[FEASIBLE-SLOT-CLAIMS] Generated share-based feasible claims for ${Object.keys(finalFeasible).length} capacities`
		);
		return finalFeasible;
	}
);

// ALLOCATED SLOTS (Pure Reactive - Share-Based Direct Allocation)
// No more credits - shares determine allocation directly
export const allocatedSlots = derived([feasibleSlotClaims], ([$feasibleSlotClaims]) => {
	console.log('[ALLOCATED-SLOTS] Pure reactive allocation: Shares → Direct Allocation');

	// Simple: Feasible claims become allocated slots directly
	// Timing constraints (advance_notice_hours, booking_window_hours) handled at usage time
	// Mutual agreement constraints handled in compose layer
	return $feasibleSlotClaims;
});

// CONVENIENCE: Single source of truth for allocated amounts
export const allocatedSlotAmounts = derived([allocatedSlots], ([$allocatedSlots]) => {
	console.log('[ALLOCATED-AMOUNTS] Converting to flat amount structure for compose system...');

	// Convert nested structure to flat amounts for easy consumption by compose system
	const amounts: Record<string, Record<string, number>> = {};

	Object.entries($allocatedSlots).forEach(([capacityId, slots]) => {
		amounts[capacityId] = {};
		Object.entries(slots).forEach(([slotId, quantity]) => {
			amounts[capacityId][slotId] = quantity;
		});
	});

	return amounts;
});

// UI-FRIENDLY CONSTRAINT METADATA (No Credits - Direct Share Information)
export const slotClaimMetadata = derived(
	[userDesiredSlotClaims, userNetworkCapacitiesWithShares],
	([$userDesiredSlotClaims, $userNetworkCapacitiesWithShares]) => {
		console.log('[SLOT-METADATA] Calculating UI-friendly constraint metadata for slot claims...');

		const metadata: Record<
			string,
			Record<
				string,
				{
					feasibleQuantity: number;
					maxAvailableUnits: number; // Clear UI indicator of upper limit
					constraintType: 'share_limit' | 'no_constraint';
					ourSharePercentage: number; // For UI display
					slotTotalQuantity: number; // For UI display
					reasonLimited?: string; // Human-readable constraint explanation
				}
			>
		> = {};

		Object.entries($userDesiredSlotClaims).forEach(([capacityId, slotClaims]) => {
			if (!metadata[capacityId]) metadata[capacityId] = {};

			Object.entries(slotClaims).forEach(([slotId, desiredQuantity]) => {
				const capacity = $userNetworkCapacitiesWithShares[capacityId];
				if (!capacity) return;

				const slot = getSlotById(capacity, slotId);
				if (!slot) return;

				// Direct calculations for UI with slot-level constraints
				const ourSharePercentage =
					'share_percentage' in capacity ? (capacity as any).share_percentage : 0;
				const slotTotalQuantity = slot.quantity;
				const rawAvailableUnits = slotTotalQuantity * ourSharePercentage;

				// Apply slot-specific divisibility constraints
				const {
					constrainedQuantity: maxAvailableUnits,
					constraintApplied,
					constraintReason
				} = applySlotConstraints(capacity, slot, rawAvailableUnits);

				// Apply share constraint
				const finalFeasibleQuantity = Math.min(desiredQuantity, maxAvailableUnits);

				// Determine constraint type and clear reason
				let constraintType: 'share_limit' | 'no_constraint';
				let reasonLimited: string | undefined;

				if (finalFeasibleQuantity < desiredQuantity) {
					constraintType = 'share_limit';
					let baseReason = `Limited by your ${(ourSharePercentage * 100).toFixed(1)}% share`;
					if (constraintApplied && constraintReason) {
						reasonLimited = `${baseReason}. ${constraintReason} (${maxAvailableUnits.toFixed(1)} units available)`;
					} else {
						reasonLimited = `${baseReason} (${maxAvailableUnits.toFixed(1)} units available)`;
					}
				} else {
					constraintType = 'no_constraint';
				}

				metadata[capacityId][slotId] = {
					feasibleQuantity: finalFeasibleQuantity,
					maxAvailableUnits, // 🎯 Key UI metadata - shows upper limit clearly
					constraintType,
					ourSharePercentage,
					slotTotalQuantity,
					reasonLimited
				};
			});
		});

		console.log(
			`[SLOT-METADATA] Generated UI-friendly metadata for ${Object.keys(metadata).length} capacity slots`
		);
		return metadata;
	}
);
