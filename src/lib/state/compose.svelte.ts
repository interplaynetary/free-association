import { writable, derived } from 'svelte/store';
import type { UserSlotComposition, NetworkSlotComposition } from '$lib/schema';
import {
	userCapacities,
	userNetworkCapacitiesWithShares,
	contributorCapacityShares,
	networkCapacities
} from '$lib/state/core.svelte';
import { allocatedSlots, allocatedSlotAmounts } from '$lib/state/slots.svelte';

/**
 * Slot-Aware Composition System
 *
 * This module implements slot-to-slot composition with mutual desire patterns.
 * Unlike the old capacity-level system, this works with specific claimed slots
 * to enable time-aware coordination and context preservation.
 *
 * Architecture:
 * 1. Slot Composition Desires (user input)
 * 2. Mutual Slot Desires (where both parties want the same slot-to-slot composition)
 * 3. Feasible Slot Composition (constrained by claimed slot availability)
 * 4. Mutual Feasible Slot Composition (achievable mutual compositions)
 */

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

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

// =============================================================================
// SLOT-AWARE COMPOSITION DESIRES
// =============================================================================

// SLOT COMPOSE-FROM MODEL:
// Maps source slots to target slots for composition
// Structure: sourceCapacityId → sourceSlotId → targetCapacityId → targetSlotId → desiredAbsoluteUnits
// Example: "my-consulting" → "morning-slot" → "their-project" → "planning-slot" → 2 means:
//   "I want to compose 2 units FROM my morning consulting slot INTO their planning slot"
//   Note: I must have claimed the morning-slot first, and they must have the planning-slot available
export const userDesiredSlotComposeFrom = writable<UserSlotComposition>({});

// SLOT COMPOSE-INTO MODEL (opposite direction):
// Maps source slots to target slots when composing into others' capacities
// Structure: myCapacityId → mySlotId → theirCapacityId → theirSlotId → desiredAbsoluteUnits
// Example: "my-cooking" → "sunday-prep" → "their-restaurant" → "dinner-service" → 3 means:
//   "I want to compose 3 units FROM my Sunday prep slot INTO their dinner service slot"
export const userDesiredSlotComposeInto = writable<UserSlotComposition>({});

// NETWORK SLOT COMPOSE-FROM MODEL:
// What others want to compose from our slots into their slots
// Structure: userId → sourceCapacityId → sourceSlotId → targetCapacityId → targetSlotId → desiredAbsoluteUnits
export const networkDesiredSlotComposeFrom = writable<NetworkSlotComposition>({});

// NETWORK SLOT COMPOSE-INTO MODEL (opposite direction):
// What others want to compose from their slots into our slots
// Structure: userId → sourceCapacityId → sourceSlotId → targetCapacityId → targetSlotId → desiredAbsoluteUnits
export const networkDesiredSlotComposeInto = writable<NetworkSlotComposition>({});

// =============================================================================
// FEASIBLE SLOT COMPOSITIONS (Simplified - Credit-Based)
// =============================================================================

// FEASIBLE SLOT COMPOSE-FROM: Constrains slot composition by claimed slot availability (Simplified)
// Direct allocation constraint - no additional scaling needed
export const feasibleSlotComposeFrom = derived(
	[userDesiredSlotComposeFrom, allocatedSlotAmounts],
	([$userDesiredSlotComposeFrom, $allocatedSlotAmounts]) => {
		console.log(
			'[FEASIBLE-SLOT-COMPOSE-FROM] Calculating allocation-based feasible slot compositions...'
		);

		const finalFeasible: UserSlotComposition = {};

		Object.entries($userDesiredSlotComposeFrom).forEach(([sourceCapacityId, sourceSlots]) => {
			Object.entries(sourceSlots).forEach(([sourceSlotId, targetCompositions]) => {
				Object.entries(targetCompositions).forEach(([targetCapacityId, targetSlots]) => {
					Object.entries(targetSlots).forEach(([targetSlotId, desiredUnits]) => {
						// Simple constraint: available allocated units from this source slot
						const availableSourceUnits =
							$allocatedSlotAmounts[sourceCapacityId]?.[sourceSlotId] || 0;
						const feasibleUnits = Math.min(desiredUnits, availableSourceUnits);

						if (feasibleUnits > 0) {
							// Initialize nested structure
							if (!finalFeasible[sourceCapacityId]) finalFeasible[sourceCapacityId] = {};
							if (!finalFeasible[sourceCapacityId][sourceSlotId])
								finalFeasible[sourceCapacityId][sourceSlotId] = {};
							if (!finalFeasible[sourceCapacityId][sourceSlotId][targetCapacityId])
								finalFeasible[sourceCapacityId][sourceSlotId][targetCapacityId] = {};

							finalFeasible[sourceCapacityId][sourceSlotId][targetCapacityId][targetSlotId] =
								feasibleUnits;

							if (feasibleUnits < desiredUnits) {
								console.log(
									`[FEASIBLE-SLOT-COMPOSE-FROM] Allocation-limited ${sourceCapacityId}:${sourceSlotId} → ${targetCapacityId}:${targetSlotId}: desired ${desiredUnits} → ${feasibleUnits} (limited by ${availableSourceUnits} allocated units)`
								);
							}
						}
					});
				});
			});
		});

		console.log(
			`[FEASIBLE-SLOT-COMPOSE-FROM] Generated allocation-based feasible compositions for ${Object.keys(finalFeasible).length} capacities`
		);
		return finalFeasible;
	}
);

// FEASIBLE SLOT COMPOSE-INTO: Constrains composing our slots into others' slots (Simplified)
// Direct allocation and share constraints - no additional scaling needed
export const feasibleSlotComposeInto = derived(
	[
		userDesiredSlotComposeInto,
		allocatedSlotAmounts,
		userCapacities,
		contributorCapacityShares,
		networkCapacities
	],
	([
		$userDesiredSlotComposeInto,
		$allocatedSlotAmounts,
		$userCapacities,
		$contributorCapacityShares,
		$networkCapacities
	]) => {
		console.log(
			'[FEASIBLE-SLOT-COMPOSE-INTO] Calculating allocation-based feasible slot compositions...'
		);

		const finalFeasible: UserSlotComposition = {};

		Object.entries($userDesiredSlotComposeInto).forEach(([sourceCapacityId, sourceSlots]) => {
			Object.entries(sourceSlots).forEach(([sourceSlotId, targetCompositions]) => {
				Object.entries(targetCompositions).forEach(([targetCapacityId, targetSlots]) => {
					Object.entries(targetSlots).forEach(([targetSlotId, desiredUnits]) => {
						// Constraint 1: Check source slot availability
						const availableSourceUnits =
							$allocatedSlotAmounts[sourceCapacityId]?.[sourceSlotId] || 0;

						if (availableSourceUnits === 0) return;

						// Constraint 2: Check recipient share constraints
						const targetProviderId = Object.keys($networkCapacities).find(
							(id) => $networkCapacities[id] && $networkCapacities[id][targetCapacityId]
						);

						if (!targetProviderId) return;

						const ourSourceCapacity = $userCapacities?.[sourceCapacityId];
						if (!ourSourceCapacity) return;

						const theirShareInOurCapacity =
							$contributorCapacityShares[targetProviderId]?.[sourceCapacityId] || 0;
						const ourSourceCapacityTotalQuantity =
							ourSourceCapacity.availability_slots?.reduce(
								(total, slot) => total + slot.quantity,
								0
							) || 0;
						const maxUnitsTheyCanReceive = ourSourceCapacityTotalQuantity * theirShareInOurCapacity;

						// Apply both constraints (no scaling - direct limits)
						const sourceConstrainedUnits = Math.min(desiredUnits, availableSourceUnits);
						const feasibleUnits = Math.min(sourceConstrainedUnits, maxUnitsTheyCanReceive);

						if (feasibleUnits > 0) {
							// Initialize structure
							if (!finalFeasible[sourceCapacityId]) finalFeasible[sourceCapacityId] = {};
							if (!finalFeasible[sourceCapacityId][sourceSlotId])
								finalFeasible[sourceCapacityId][sourceSlotId] = {};
							if (!finalFeasible[sourceCapacityId][sourceSlotId][targetCapacityId])
								finalFeasible[sourceCapacityId][sourceSlotId][targetCapacityId] = {};

							finalFeasible[sourceCapacityId][sourceSlotId][targetCapacityId][targetSlotId] =
								feasibleUnits;

							if (feasibleUnits < desiredUnits) {
								if (sourceConstrainedUnits < desiredUnits) {
									console.log(
										`[FEASIBLE-SLOT-COMPOSE-INTO] Allocation-limited ${sourceCapacityId}:${sourceSlotId} → ${targetCapacityId}:${targetSlotId}: desired ${desiredUnits} → ${feasibleUnits} (limited by ${availableSourceUnits} allocated units)`
									);
								} else {
									console.log(
										`[FEASIBLE-SLOT-COMPOSE-INTO] Share-limited ${sourceCapacityId}:${sourceSlotId} → ${targetCapacityId}:${targetSlotId}: desired ${desiredUnits} → ${feasibleUnits} (limited by ${(theirShareInOurCapacity * 100).toFixed(1)}% recipient share)`
									);
								}
							}
						}
					});
				});
			});
		});

		console.log(
			`[FEASIBLE-SLOT-COMPOSE-INTO] Generated allocation-based feasible compositions for ${Object.keys(finalFeasible).length} capacities`
		);
		return finalFeasible;
	}
);

// Helper function to calculate how well two slot desires align (same as capacity-level)
function calculateSlotDesireAlignment(ourDesire: number, theirDesire: number): number {
	if (ourDesire === 0 || theirDesire === 0) return 0;
	const ratio = Math.min(ourDesire, theirDesire) / Math.max(ourDesire, theirDesire);
	return ratio;
}

// =============================================================================
// MUTUAL SLOT DESIRES - Unified Pattern (matches original compose.svelte.ts)
// =============================================================================

// MUTUAL SLOT DESIRE (unified - no artificial "our vs their" distinction)
// Finds where both parties want the same slot-to-slot composition to happen
export const mutualSlotDesires = derived(
	[
		userDesiredSlotComposeFrom,
		networkDesiredSlotComposeFrom,
		userDesiredSlotComposeInto,
		networkDesiredSlotComposeInto,
		userNetworkCapacitiesWithShares,
		networkCapacities
	],
	([
		$userDesiredSlotComposeFrom,
		$networkDesiredSlotComposeFrom,
		$userDesiredSlotComposeInto,
		$networkDesiredSlotComposeInto,
		$userNetworkCapacitiesWithShares,
		$networkCapacities
	]) => {
		console.log('[MUTUAL-SLOT-DESIRES] Calculating mutual slot desires for all compositions...');

		const mutualDesires: Record<
			string,
			{
				sourceCapacityId: string;
				sourceSlotId: string;
				targetCapacityId: string;
				targetSlotId: string;
				ourDesiredAmount: number;
				theirDesiredAmount: number;
				providerId: string;
				compositionType: 'from' | 'into';
				desireViability: number;
			}
		> = {};

		// TYPE 1: FROM-compositions where both parties agree
		// We want: FROM sourceSlot INTO targetSlot
		// They want: FROM sourceSlot INTO targetSlot (same direction)
		Object.entries($userDesiredSlotComposeFrom).forEach(([sourceCapacityId, sourceSlots]) => {
			Object.entries(sourceSlots).forEach(([sourceSlotId, targetCompositions]) => {
				Object.entries(targetCompositions).forEach(([targetCapacityId, targetSlots]) => {
					Object.entries(targetSlots).forEach(([targetSlotId, ourDesiredAmount]) => {
						// Find who provides the source capacity
						const sourceProviderId =
							Object.keys($networkCapacities).find(
								(id) => $networkCapacities[id] && $networkCapacities[id][sourceCapacityId]
							) ||
							(Object.keys($userNetworkCapacitiesWithShares).includes(sourceCapacityId)
								? ($userNetworkCapacitiesWithShares[sourceCapacityId] as any).provider_id
								: null);

						if (!sourceProviderId) return;

						// Check if the source provider also wants this same composition
						const theirDesiredAmount =
							$networkDesiredSlotComposeFrom[sourceProviderId]?.[sourceCapacityId]?.[
								sourceSlotId
							]?.[targetCapacityId]?.[targetSlotId];

						if (!theirDesiredAmount) return;

						const desireViability = calculateSlotDesireAlignment(
							ourDesiredAmount,
							theirDesiredAmount
						);
						const compositionKey = `from:${sourceCapacityId}:${sourceSlotId}:${targetCapacityId}:${targetSlotId}:${sourceProviderId}`;

						mutualDesires[compositionKey] = {
							sourceCapacityId,
							sourceSlotId,
							targetCapacityId,
							targetSlotId,
							ourDesiredAmount,
							theirDesiredAmount,
							providerId: sourceProviderId,
							compositionType: 'from',
							desireViability
						};

						console.log(
							`[MUTUAL-SLOT-DESIRES] Found mutual FROM desire: ${compositionKey} (viability: ${desireViability.toFixed(2)})`
						);
					});
				});
			});
		});

		// TYPE 2: INTO-compositions where both parties agree
		// We want: FROM sourceSlot INTO targetSlot
		// They want: FROM sourceSlot INTO targetSlot (same direction)
		Object.entries($userDesiredSlotComposeInto).forEach(([sourceCapacityId, sourceSlots]) => {
			Object.entries(sourceSlots).forEach(([sourceSlotId, targetCompositions]) => {
				Object.entries(targetCompositions).forEach(([targetCapacityId, targetSlots]) => {
					Object.entries(targetSlots).forEach(([targetSlotId, ourDesiredAmount]) => {
						// Find who provides the target capacity
						const targetProviderId = Object.keys($networkCapacities).find(
							(id) => $networkCapacities[id] && $networkCapacities[id][targetCapacityId]
						);

						if (!targetProviderId) return;

						// Check if the target provider also wants this same composition
						const theirDesiredAmount =
							$networkDesiredSlotComposeInto[targetProviderId]?.[sourceCapacityId]?.[
								sourceSlotId
							]?.[targetCapacityId]?.[targetSlotId];

						if (!theirDesiredAmount) return;

						const desireViability = calculateSlotDesireAlignment(
							ourDesiredAmount,
							theirDesiredAmount
						);
						const compositionKey = `into:${sourceCapacityId}:${sourceSlotId}:${targetCapacityId}:${targetSlotId}:${targetProviderId}`;

						mutualDesires[compositionKey] = {
							sourceCapacityId,
							sourceSlotId,
							targetCapacityId,
							targetSlotId,
							ourDesiredAmount,
							theirDesiredAmount,
							providerId: targetProviderId,
							compositionType: 'into',
							desireViability
						};

						console.log(
							`[MUTUAL-SLOT-DESIRES] Found mutual INTO desire: ${compositionKey} (viability: ${desireViability.toFixed(2)})`
						);
					});
				});
			});
		});

		console.log(
			`[MUTUAL-SLOT-DESIRES] Found ${Object.keys(mutualDesires).length} total mutual slot desires`
		);
		return mutualDesires;
	}
);

// MUTUAL FEASIBLE SLOT COMPOSITIONS (unified)
// Takes mutual slot desires and constrains them by actual slot availability
export const mutualFeasibleSlotCompositions = derived(
	[mutualSlotDesires, feasibleSlotComposeFrom, feasibleSlotComposeInto],
	([$mutualSlotDesires, $feasibleSlotComposeFrom, $feasibleSlotComposeInto]) => {
		console.log('[MUTUAL-FEASIBLE-SLOTS] Calculating feasible mutual slot compositions...');

		const mutualFeasible: Record<
			string,
			{
				sourceCapacityId: string;
				sourceSlotId: string;
				targetCapacityId: string;
				targetSlotId: string;
				ourDesiredAmount: number;
				theirDesiredAmount: number;
				ourFeasibleAmount: number;
				providerId: string;
				compositionType: 'from' | 'into';
				desireViability: number;
				feasibleViability: number;
				constraintRatio: number;
			}
		> = {};

		Object.entries($mutualSlotDesires).forEach(([compositionKey, mutualDesire]) => {
			const { sourceCapacityId, sourceSlotId, targetCapacityId, targetSlotId, compositionType } =
				mutualDesire;

			// Get our feasible amount based on composition type
			let ourFeasibleAmount = 0;

			if (compositionType === 'from') {
				ourFeasibleAmount =
					$feasibleSlotComposeFrom[sourceCapacityId]?.[sourceSlotId]?.[targetCapacityId]?.[
						targetSlotId
					] || 0;
			} else {
				ourFeasibleAmount =
					$feasibleSlotComposeInto[sourceCapacityId]?.[sourceSlotId]?.[targetCapacityId]?.[
						targetSlotId
					] || 0;
			}

			if (ourFeasibleAmount > 0) {
				// Mutual feasible is the minimum of our feasible amount and their desired amount
				const mutualFeasibleAmount = Math.min(ourFeasibleAmount, mutualDesire.theirDesiredAmount);

				const feasibleViability = calculateSlotDesireAlignment(
					mutualFeasibleAmount,
					mutualDesire.theirDesiredAmount
				);

				const constraintRatio = mutualFeasibleAmount / mutualDesire.ourDesiredAmount;

				mutualFeasible[compositionKey] = {
					...mutualDesire,
					ourFeasibleAmount: mutualFeasibleAmount,
					feasibleViability,
					constraintRatio
				};

				console.log(
					`[MUTUAL-FEASIBLE-SLOTS] ${compositionKey}: desired ${mutualDesire.ourDesiredAmount.toFixed(2)} → feasible ${mutualFeasibleAmount.toFixed(2)} (${(constraintRatio * 100).toFixed(1)}% achievable)`
				);
			}
		});

		console.log(
			`[MUTUAL-FEASIBLE-SLOTS] Found ${Object.keys(mutualFeasible).length} feasible mutual slot compositions`
		);
		return mutualFeasible;
	}
);

// =============================================================================
// CONSTRAINT METADATA STORES
// =============================================================================

// Slot constraint metadata interface (simplified for direct allocation system)
interface SlotConstraintMetadata {
	feasibleAmount: number;
	constraintType: 'slot_limit' | 'share_limit' | 'no_constraint';
	availableAmount: number;
	reasonLimited?: string; // Human-readable constraint explanation
}

// SLOT COMPOSE-FROM CONSTRAINT METADATA (Simplified)
// Tracks detailed constraint information for each slot composition
export const feasibleSlotComposeFromMetadata = derived(
	[userDesiredSlotComposeFrom, allocatedSlotAmounts],
	([$userDesiredSlotComposeFrom, $allocatedSlotAmounts]) => {
		console.log('[FEASIBLE-SLOT-METADATA-FROM] Calculating allocation constraint metadata...');

		const metadata: Record<
			string,
			Record<string, Record<string, Record<string, SlotConstraintMetadata>>>
		> = {};

		Object.entries($userDesiredSlotComposeFrom).forEach(([sourceCapacityId, sourceSlots]) => {
			Object.entries(sourceSlots).forEach(([sourceSlotId, targetCompositions]) => {
				Object.entries(targetCompositions).forEach(([targetCapacityId, targetSlots]) => {
					Object.entries(targetSlots).forEach(([targetSlotId, desiredUnits]) => {
						// Simple constraint: available allocated units
						const availableSourceUnits =
							$allocatedSlotAmounts[sourceCapacityId]?.[sourceSlotId] || 0;
						const feasibleUnits = Math.min(desiredUnits, availableSourceUnits);

						// Determine constraint type and reason
						let constraintType: 'slot_limit' | 'share_limit' | 'no_constraint';
						let reasonLimited: string | undefined;

						if (feasibleUnits < desiredUnits) {
							constraintType = 'slot_limit';
							reasonLimited = `Limited by ${availableSourceUnits} allocated units in source slot`;
						} else {
							constraintType = 'no_constraint';
						}

						// Initialize metadata structure
						if (!metadata[sourceCapacityId]) metadata[sourceCapacityId] = {};
						if (!metadata[sourceCapacityId][sourceSlotId])
							metadata[sourceCapacityId][sourceSlotId] = {};
						if (!metadata[sourceCapacityId][sourceSlotId][targetCapacityId])
							metadata[sourceCapacityId][sourceSlotId][targetCapacityId] = {};

						metadata[sourceCapacityId][sourceSlotId][targetCapacityId][targetSlotId] = {
							feasibleAmount: feasibleUnits,
							constraintType,
							availableAmount: availableSourceUnits,
							reasonLimited
						};
					});
				});
			});
		});

		console.log(
			`[FEASIBLE-SLOT-METADATA-FROM] Generated allocation constraint metadata for ${Object.keys(metadata).length} capacity compositions`
		);
		return metadata;
	}
);

// SLOT COMPOSE-INTO CONSTRAINT METADATA (Simplified)
// Tracks detailed constraint information for each slot compose-into relationship
export const feasibleSlotComposeIntoMetadata = derived(
	[
		userDesiredSlotComposeInto,
		allocatedSlotAmounts,
		userCapacities,
		contributorCapacityShares,
		networkCapacities
	],
	([
		$userDesiredSlotComposeInto,
		$allocatedSlotAmounts,
		$userCapacities,
		$contributorCapacityShares,
		$networkCapacities
	]) => {
		console.log('[FEASIBLE-SLOT-METADATA-INTO] Calculating allocation constraint metadata...');

		const metadata: Record<
			string,
			Record<string, Record<string, Record<string, SlotConstraintMetadata>>>
		> = {};

		Object.entries($userDesiredSlotComposeInto).forEach(([sourceCapacityId, sourceSlots]) => {
			Object.entries(sourceSlots).forEach(([sourceSlotId, targetCompositions]) => {
				Object.entries(targetCompositions).forEach(([targetCapacityId, targetSlots]) => {
					Object.entries(targetSlots).forEach(([targetSlotId, desiredUnits]) => {
						// Same constraints as feasibleSlotComposeInto
						const availableSourceUnits =
							$allocatedSlotAmounts[sourceCapacityId]?.[sourceSlotId] || 0;

						if (availableSourceUnits === 0) return;

						const targetProviderId = Object.keys($networkCapacities).find(
							(id) => $networkCapacities[id] && $networkCapacities[id][targetCapacityId]
						);
						if (!targetProviderId) return;

						const ourSourceCapacity = $userCapacities?.[sourceCapacityId];
						if (!ourSourceCapacity) return;

						const theirShareInOurCapacity =
							$contributorCapacityShares[targetProviderId]?.[sourceCapacityId] || 0;
						const ourSourceCapacityTotalQuantity =
							ourSourceCapacity.availability_slots?.reduce(
								(total, slot) => total + slot.quantity,
								0
							) || 0;
						const maxUnitsTheyCanReceive = ourSourceCapacityTotalQuantity * theirShareInOurCapacity;

						const sourceConstrainedUnits = Math.min(desiredUnits, availableSourceUnits);
						const feasibleUnits = Math.min(sourceConstrainedUnits, maxUnitsTheyCanReceive);

						// Determine constraint type and reason
						let constraintType: 'slot_limit' | 'share_limit' | 'no_constraint';
						let reasonLimited: string | undefined;

						if (feasibleUnits < desiredUnits) {
							if (sourceConstrainedUnits < desiredUnits) {
								constraintType = 'slot_limit';
								reasonLimited = `Limited by ${availableSourceUnits} allocated units in source slot`;
							} else {
								constraintType = 'share_limit';
								reasonLimited = `Limited by ${(theirShareInOurCapacity * 100).toFixed(1)}% recipient share (max ${maxUnitsTheyCanReceive.toFixed(1)} units)`;
							}
						} else {
							constraintType = 'no_constraint';
						}

						// Initialize metadata structure
						if (!metadata[sourceCapacityId]) metadata[sourceCapacityId] = {};
						if (!metadata[sourceCapacityId][sourceSlotId])
							metadata[sourceCapacityId][sourceSlotId] = {};
						if (!metadata[sourceCapacityId][sourceSlotId][targetCapacityId])
							metadata[sourceCapacityId][sourceSlotId][targetCapacityId] = {};

						metadata[sourceCapacityId][sourceSlotId][targetCapacityId][targetSlotId] = {
							feasibleAmount: feasibleUnits,
							constraintType,
							availableAmount: availableSourceUnits,
							reasonLimited
						};
					});
				});
			});
		});

		console.log(
			`[FEASIBLE-SLOT-METADATA-INTO] Generated allocation constraint metadata for ${Object.keys(metadata).length} capacity compositions`
		);
		return metadata;
	}
);
