import { writable, derived, get } from 'svelte/store';
import type { Writable } from 'svelte/store';
import {
	normalizeShareMap,
	getSubtreeContributorMap,
	findNodeById,
	calculateRecipientSlotQuantities,
	computeQuantityShares
} from '$lib/protocol';
import { applyCapacityFilter, type FilterContext } from '$lib/filters';
import type {
	RootNode,
	CapacitiesCollection,
	Node,
	ShareMap,
	RecognitionCache,
	UserSlotQuantities,
	ProviderCapacity,
	UserSlotClaims,
	NetworkSlotClaims,
	RecipientCapacity,
	SlotAllocationAnalysis,
	SlotAllocationMetadata
} from '$lib/schema';

// Core reactive state - these form the main reactive chain
export const userTree: Writable<RootNode | null> = writable(null);
export const userSogf: Writable<ShareMap | null> = writable(null);
export const userCapacities: Writable<CapacitiesCollection | null> = writable(null);

export const networkCapacities: Writable<Record<string, CapacitiesCollection>> = writable({});
export const networkCapacityShares: Writable<Record<string, Record<string, number>>> = writable({});
export const networkCapacitySlotQuantities: Writable<Record<string, UserSlotQuantities>> = writable(
	{}
);

// SLOT CLAIMS STORES (moved from slots.svelte.ts to avoid circular dependency)
export const userDesiredSlotClaims: Writable<UserSlotClaims> = writable({});
export const networkDesiredSlotClaims: Writable<NetworkSlotClaims> = writable({});

// SLOT COMPOSITION DESIRE STORES (moved from compose.svelte.ts to avoid circular dependency)
import type { UserSlotComposition, NetworkSlotComposition } from '$lib/schema';
export const userDesiredSlotComposeFrom: Writable<UserSlotComposition> = writable({});
export const userDesiredSlotComposeInto: Writable<UserSlotComposition> = writable({});
export const networkDesiredSlotComposeFrom: Writable<NetworkSlotComposition> = writable({});
export const networkDesiredSlotComposeInto: Writable<NetworkSlotComposition> = writable({});

// New derived store that uses actual slot quantities instead of percentage calculations
export const userNetworkCapacitiesWithSlotQuantities = derived(
	[networkCapacitySlotQuantities, networkCapacities],
	([$networkCapacitySlotQuantities, $networkCapacities]) => {
		// Don't return early if one store is empty - wait for both to have data
		if (!$networkCapacitySlotQuantities) {
			console.log(
				'[NETWORK-CAPACITIES-SLOTS] No network slot quantity data available, returning empty'
			);
			return {};
		}

		if (!$networkCapacities) {
			console.log(
				'[NETWORK-CAPACITIES-SLOTS] No network capacities data available, returning empty'
			);
			return {};
		}

		console.log('networkCapacitySlotQuantities', $networkCapacitySlotQuantities);
		console.log('networkCapacities', $networkCapacities);

		// Filter capacities where we have slot quantities
		const filteredCapacities: CapacitiesCollection = {};

		// For each contributor's slot quantities
		Object.entries($networkCapacitySlotQuantities).forEach(([contributorId, slotQuantities]) => {
			// Get this contributor's capacities
			const contributorCapacities = $networkCapacities[contributorId];
			if (!contributorCapacities) {
				console.log(
					`[NETWORK-CAPACITIES-SLOTS] No capacities found for contributor ${contributorId} - stream timing issue, will process when available`
				);
				return;
			}

			// For each capacity we have slot quantities for
			Object.entries(slotQuantities).forEach(([capacityId, slotQuantityMap]) => {
				const capacity = contributorCapacities[capacityId];
				if (capacity) {
					// Convert slot quantities to computed_quantities format
					const computedQuantities = Object.entries(slotQuantityMap).map(([slotId, quantity]) => ({
						slot_id: slotId,
						quantity: quantity
					}));

					// Calculate total share percentage for display (optional)
					const totalSlotUnits =
						capacity.availability_slots?.reduce(
							(sum: number, slot: any) => sum + slot.quantity,
							0
						) || 1;
					const totalUserUnits = computedQuantities.reduce((sum, cq) => sum + cq.quantity, 0);
					const share_percentage = totalSlotUnits > 0 ? totalUserUnits / totalSlotUnits : 0;

					// Add the capacity with actual slot quantities
					filteredCapacities[capacityId] = {
						...capacity,
						share_percentage: share_percentage,
						computed_quantities: computedQuantities,
						provider_id: contributorId
					};
				} else {
					console.log(
						`[NETWORK-CAPACITIES-SLOTS] No capacity definition found for ${capacityId} from contributor ${contributorId} - stream timing issue, will process when available`
					);
				}
			});
		});

		console.log(
			`[NETWORK-CAPACITIES-SLOTS] Found ${Object.keys(filteredCapacities).length} capacities where we have slot quantities`
		);

		return filteredCapacities;
	}
);

// LEGACY STORE - DEPRECATED
// This store uses the old percentage-based approach and should be replaced with
// userNetworkCapacitiesWithSlotQuantities in all new code - excpe th chat system?
export const userNetworkCapacitiesWithShares = derived(
	[networkCapacityShares, networkCapacities],
	([$networkCapacityShares, $networkCapacities]) => {
		if (!$networkCapacityShares || !$networkCapacities) {
			console.log('[NETWORK-CAPACITIES] No network data available, returning empty');
			return {};
		}

		console.log('networkCapacityShares', $networkCapacityShares);
		console.log('networkCapacities', $networkCapacities);

		// Filter capacities where we have shares
		const filteredCapacities: CapacitiesCollection = {};

		// For each contributor's shares
		Object.entries($networkCapacityShares).forEach(([contributorId, shares]) => {
			// Get this contributor's capacities
			const contributorCapacities = $networkCapacities[contributorId];
			if (!contributorCapacities) return;

			// For each capacity we have a share in
			Object.entries(shares).forEach(([capacityId, share]) => {
				const capacity = contributorCapacities[capacityId];
				if (capacity) {
					const computedQuantities = computeQuantityShares(capacity, share);

					// Add the capacity with just our share and properly computed quantities
					filteredCapacities[capacityId] = {
						...capacity,
						share_percentage: share,
						computed_quantities: computedQuantities,
						provider_id: contributorId
					};
				}
			});
		});

		console.log(
			`[NETWORK-CAPACITIES] Found ${Object.keys(filteredCapacities).length} capacities where we have shares`
		);

		return filteredCapacities;
	}
);

// Node Map
export const nodesMap: Writable<Record<string, Node>> = writable({});

// Contributors state
export const contributors = writable<string[]>([]);

// All contributors we've ever had - used for SOGF calculation to ensure removed contributors get 0%
export const allKnownContributors = writable<string[]>([]);

// Recognition cache - maps contributor ID to {ourShare, theirShare}
export const recognitionCache = writable<RecognitionCache>({});

// Core derived stores that must stay here due to Svelte 5 export restrictions
// These form the main reactive chain: recognitionCache -> mutualRecognition -> providerShares

// Derived store for mutual recognition values (min of ourShare and theirShare)
export const mutualRecognition = derived(recognitionCache, ($recognitionCache) => {
	/*console.log(
		`[MUTUAL-RECOGNITION] ${new Date().toISOString()} Recalculating from cache:`,
		$recognitionCache
	);*/

	const mutualValues: Record<string, number> = {};

	for (const [contributorId, recognition] of Object.entries($recognitionCache)) {
		// Mutual recognition is the minimum of our share and their share
		const mutualValue = Math.min(recognition.ourShare, recognition.theirShare);
		mutualValues[contributorId] = mutualValue;
		/*
		console.log(
			`[MUTUAL-RECOGNITION] ${contributorId}: our=${recognition.ourShare.toFixed(4)}, their=${recognition.theirShare.toFixed(4)}, mutual=${mutualValue.toFixed(4)}`
		);*/
	}

	console.log('[MUTUAL-RECOGNITION] Final mutual values:', mutualValues);
	return mutualValues;
});

// Derived store for mutual contributors list
export const mutualContributors = derived(mutualRecognition, ($mutualRecognition) => {
	// Filter for contributors with mutual recognition > 0
	const mutualList = Object.entries($mutualRecognition)
		.filter(([_, value]) => value > 0)
		.map(([contributorId, _]) => contributorId);

	return mutualList;
});

// Derived store for normalized mutual recognition values (sum to 1.0)
export const providerShares = derived(mutualRecognition, ($mutualRecognition) => {
	/*console.log(
		`[PROVIDER-SHARES] ${new Date().toISOString()} Recalculating from mutual recognition:`,
		$mutualRecognition
	);*/

	// If empty, return empty object
	if (Object.keys($mutualRecognition).length === 0) {
		console.log('[PROVIDER-SHARES] No mutual recognition data, returning empty');
		return {};
	}

	// Use the normalizeShareMap function from protocol.ts
	const normalized = normalizeShareMap($mutualRecognition);
	//console.log('[PROVIDER-SHARES] Normalized shares:', normalized);
	return normalized;
});

// Derived store for subtree contributor mapping
export const subtreeContributorMap = derived([userTree], ([$userTree]) => {
	if (!$userTree) {
		console.log('[SUBTREE-MAP] No tree available, returning empty map');
		return {};
	}

	console.log('[SUBTREE-MAP] Calculating subtree contributor map...');

	// Get the subtree map - resolution will happen at the calculation level
	const filterMap = getSubtreeContributorMap($userTree);

	console.log('[SUBTREE-MAP] Generated filter map for', Object.keys(filterMap).length, 'subtrees');
	return filterMap;
});

// Derived store for capacity shares - maps capacity IDs to their filtered share maps
export const capacityShares = derived(
	[userCapacities, providerShares, subtreeContributorMap],
	([$userCapacities, $providerShares, $subtreeContributorMap]) => {
		console.log(
			`[CAPACITY-SHARES] ${new Date().toISOString()} Recalculating from capacities and provider shares`
		);

		// If no capacities or provider shares, return empty
		if (!$userCapacities || Object.keys($providerShares).length === 0) {
			console.log('[CAPACITY-SHARES] No capacities or provider shares, returning empty');
			return {};
		}

		// Calculate filtered shares for each capacity
		const shares: Record<string, ShareMap> = {};
		Object.entries($userCapacities).forEach(([capacityId, capacity]) => {
			try {
				// Create the context object for filtering
				const context: FilterContext = {
					subtreeContributors: $subtreeContributorMap
				};

				// Apply capacity filter to provider shares
				const filteredShares = applyCapacityFilter(capacity, $providerShares, context);

				// Store the filtered shares
				shares[capacityId] = filteredShares;
			} catch (error) {
				console.error(
					'[CAPACITY-SHARES] Error calculating shares for capacity:',
					capacityId,
					error
				);
				// On error, use empty share map for this capacity
				shares[capacityId] = {};
			}
		});

		console.log('[CAPACITY-SHARES] Generated shares for', Object.keys(shares).length, 'capacities');
		return shares;
	}
);

// Derived store that calculates slot quantities using mutual recognition shares
export const capacitySlotQuantities = derived(
	[userCapacities, capacityShares],
	([$userCapacities, $capacityShares]) => {
		console.log(
			'[CAPACITY-SLOT-QUANTITIES] Calculating slot quantities from mutual recognition shares'
		);

		if (!$userCapacities || !$capacityShares) {
			console.log(
				'[CAPACITY-SLOT-QUANTITIES] Missing capacities or capacity shares, returning empty'
			);
			return {};
		}

		const allSlotQuantities: Record<string, UserSlotQuantities> = {};

		// Process each capacity
		Object.entries($userCapacities).forEach(([capacityId, capacity]) => {
			try {
				// Only process provider capacities (our own capacities)
				if (!('recipient_shares' in capacity)) {
					return;
				}

				const providerCapacity = capacity as ProviderCapacity;

				// Get the already-calculated mutual recognition shares for this capacity
				const filteredShares = $capacityShares[capacityId] || {};

				// Calculate actual slot quantities using the mutual recognition shares
				const recipientSlotQuantities = calculateRecipientSlotQuantities(
					providerCapacity,
					filteredShares
				);

				// Merge the results
				Object.entries(recipientSlotQuantities).forEach(([recipientId, slotQuantities]) => {
					if (!allSlotQuantities[recipientId]) {
						allSlotQuantities[recipientId] = {};
					}
					Object.assign(allSlotQuantities[recipientId], slotQuantities);
				});

				console.log(
					`[CAPACITY-SLOT-QUANTITIES] Calculated slot quantities for capacity ${capacityId} with ${Object.keys(recipientSlotQuantities).length} recipients using mutual recognition shares`
				);
			} catch (error) {
				console.error(
					'[CAPACITY-SLOT-QUANTITIES] Error calculating slot quantities for capacity:',
					capacityId,
					error
				);
			}
		});

		// Note: Persistence is now handled in subscriptions.svelte.ts

		console.log(
			'[CAPACITY-SLOT-QUANTITIES] Generated slot quantities for',
			Object.keys(allSlotQuantities).length,
			'recipients using mutual recognition'
		);
		return allSlotQuantities;
	}
);

// Derived store for contributor capacity shares - maps contributor IDs to their capacity shares
export const contributorCapacityShares = derived(capacityShares, ($capacityShares) => {
	console.log(`[CAPACITY-SHARES] ${new Date().toISOString()} Recalculating from capacity shares`);

	const contributorShares: Record<string, Record<string, number>> = {};

	// For each capacity's shares
	Object.entries($capacityShares).forEach(([capacityId, shares]) => {
		// For each contributor's share in this capacity
		Object.entries(shares).forEach(([contributorId, share]) => {
			// Initialize the contributor's share map if it doesn't exist
			if (!contributorShares[contributorId]) {
				contributorShares[contributorId] = {};
			}

			// Add this capacity's share to the contributor's map
			contributorShares[contributorId][capacityId] = share;
		});
	});

	console.log('[CAPACITY-SHARES] Generated contributor shares map:', contributorShares);
	return contributorShares;
});

// Derived store that combines userCapacities with capacityShares to match current schema
export const userCapacitiesWithShares = derived(
	[userCapacities, capacityShares],
	([$userCapacities, $capacityShares]) => {
		console.log(
			`[CAPACITIES-WITH-SHARES] ${new Date().toISOString()} Combining capacities with shares`
		);

		if (!$userCapacities) {
			console.log('[CAPACITIES-WITH-SHARES] No capacities available, returning null');
			return null;
		}

		// Create a new capacities collection with shares included
		const capacitiesWithShares: CapacitiesCollection = {};

		Object.entries($userCapacities).forEach(([capacityId, capacity]) => {
			// Get the shares for this capacity
			const shares = $capacityShares[capacityId] || {};

			// Create a new capacity object with the shares included
			capacitiesWithShares[capacityId] = {
				...capacity,
				recipient_shares: shares
			};
		});

		console.log(
			'[CAPACITIES-WITH-SHARES] Generated',
			Object.keys(capacitiesWithShares).length,
			'capacities with shares'
		);
		return capacitiesWithShares;
	}
);

// Derived store that provides subtree options for UI components
export const subtreeOptions = derived([userTree], ([$userTree]) => {
	if (!$userTree) return [];

	const subtreeMap = getSubtreeContributorMap($userTree);

	return Object.entries(subtreeMap)
		.map(([subtreeId, contributorRecord]) => {
			// Convert contributorRecord to array of contributor IDs
			const contributors = Object.keys(contributorRecord);

			// Find the node to get its name
			const node = findNodeById($userTree, subtreeId);
			const name = node?.name || subtreeId;

			return {
				id: subtreeId,
				name,
				contributorCount: contributors.length,
				contributors
			};
		})
		.filter((option) => option.contributorCount > 0); // Only include subtrees with contributors
});

// Loading state flags
export const isLoadingCapacities = writable(false);
export const isLoadingTree = writable(false);
export const isLoadingSogf = writable(false);

export const isRecalculatingTree = writable(false);
export const isRecalculatingCapacities = writable(false);

// Additional state
export const publicTemplates: Writable<Record<string, any>> = writable({});

// SLOT ALLOCATION HELPER FUNCTIONS (moved from slots.svelte.ts)
function getSlotById(capacity: any, slotId: string) {
	return capacity.availability_slots?.find((slot: any) => slot.id === slotId);
}

// UNIFIED SLOT ALLOCATION ANALYSIS (moved from slots.svelte.ts to avoid circular dependency)
// Combines feasible claims calculation with rich metadata for UI
export const slotAllocationAnalysis = derived(
	[userDesiredSlotClaims, userNetworkCapacitiesWithSlotQuantities],
	([$userDesiredSlotClaims, $userNetworkCapacitiesWithSlotQuantities]): SlotAllocationAnalysis => {
		console.log('[SLOT-ALLOCATION] Analyzing slot allocations with unified approach...');

		const feasibleClaims: UserSlotClaims = {};
		const metadata: Record<string, Record<string, SlotAllocationMetadata>> = {};

		Object.entries($userDesiredSlotClaims).forEach(([capacityId, slotClaims]) => {
			if (!metadata[capacityId]) metadata[capacityId] = {};

			Object.entries(slotClaims).forEach(([slotId, desiredQuantity]) => {
				const capacity = $userNetworkCapacitiesWithSlotQuantities[capacityId] as RecipientCapacity;
				if (!capacity) return;

				const slot = getSlotById(capacity, slotId);
				if (!slot) return;

				// Get actual allocated slot quantities from discrete allocation
				const slotAllocation = capacity.computed_quantities?.find((cq) => cq.slot_id === slotId);
				if (!slotAllocation) return;

				// Calculate feasible quantity and metadata in one pass
				const maxAvailableUnits = slotAllocation.quantity;
				const finalFeasibleQuantity = Math.min(desiredQuantity, maxAvailableUnits);
				const ourSharePercentage = capacity.share_percentage || 0;

				// Store feasible claims
				if (finalFeasibleQuantity > 0) {
					if (!feasibleClaims[capacityId]) {
						feasibleClaims[capacityId] = {};
					}
					feasibleClaims[capacityId][slotId] = finalFeasibleQuantity;
				}

				// Store metadata
				const constraintType: 'share_limit' | 'no_constraint' =
					finalFeasibleQuantity < desiredQuantity ? 'share_limit' : 'no_constraint';

				const reasonLimited =
					constraintType === 'share_limit'
						? `Limited by ${maxAvailableUnits} discretely allocated units (from ${(ourSharePercentage * 100).toFixed(1)}% share)`
						: undefined;

				metadata[capacityId][slotId] = {
					feasibleQuantity: finalFeasibleQuantity,
					maxAvailableUnits,
					constraintType,
					ourSharePercentage,
					slotTotalQuantity: slot.quantity,
					reasonLimited
				};

				// Log constraints
				if (constraintType === 'share_limit') {
					console.log(
						`[SLOT-ALLOCATION] Constrained ${capacityId}:${slotId}: desired ${desiredQuantity} → ${finalFeasibleQuantity} (${reasonLimited})`
					);
				}
			});
		});

		console.log(
			`[SLOT-ALLOCATION] Analyzed allocations for ${Object.keys(feasibleClaims).length} capacities`
		);

		return { feasibleClaims, metadata };
	}
);

// FEASIBLE SLOT CLAIMS (Derived from unified analysis)
export const feasibleSlotClaims = derived(
	[slotAllocationAnalysis],
	([analysis]) => analysis.feasibleClaims
);

// ALLOCATED SLOTS (Pure Reactive - Share-Based Direct Allocation)
export const allocatedSlots = derived([feasibleSlotClaims], ([$feasibleSlotClaims]) => {
	console.log('[ALLOCATED-SLOTS] Pure reactive allocation: Shares → Direct Allocation');
	// Simple: Feasible claims become allocated slots directly
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

// UI-FRIENDLY CONSTRAINT METADATA (Derived from unified analysis)
export const slotClaimMetadata = derived(
	[slotAllocationAnalysis],
	([analysis]) => analysis.metadata
);

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
// SLOT-AWARE COMPOSITION DESIRES
// =============================================================================

// SLOT COMPOSITION DESIRE STORES - MOVED TO CORE.SVELTE.TS TO AVOID CIRCULAR DEPENDENCY
// These stores are now imported from core.svelte.ts
//
// userDesiredSlotComposeFrom: Maps source slots to target slots for composition
// userDesiredSlotComposeInto: Maps source slots when composing into others' capacities
// networkDesiredSlotComposeFrom: What others want to compose from our slots
// networkDesiredSlotComposeInto: What others want to compose into our slots

// =============================================================================
// FEASIBLE SLOT COMPOSITIONS
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
		userNetworkCapacitiesWithSlotQuantities,
		networkCapacities
	],
	([
		$userDesiredSlotComposeFrom,
		$networkDesiredSlotComposeFrom,
		$userDesiredSlotComposeInto,
		$networkDesiredSlotComposeInto,
		$userNetworkCapacitiesWithSlotQuantities,
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
		// We want: FROM their sourceSlot INTO our targetSlot
		// They want: FROM their sourceSlot INTO our targetSlot (their perspective: INTO)
		Object.entries($userDesiredSlotComposeFrom).forEach(([sourceCapacityId, sourceSlots]) => {
			Object.entries(sourceSlots).forEach(([sourceSlotId, targetCompositions]) => {
				Object.entries(targetCompositions).forEach(([targetCapacityId, targetSlots]) => {
					Object.entries(targetSlots).forEach(([targetSlotId, ourDesiredAmount]) => {
						// Find who provides the source capacity
						const sourceProviderId =
							Object.keys($networkCapacities).find(
								(id) => $networkCapacities[id] && $networkCapacities[id][sourceCapacityId]
							) ||
							(Object.keys($userNetworkCapacitiesWithSlotQuantities).includes(sourceCapacityId)
								? ($userNetworkCapacitiesWithSlotQuantities[sourceCapacityId] as any).provider_id
								: null);

						if (!sourceProviderId) return;

						// Check if the source provider wants to compose INTO our slot (opposite direction)
						const theirDesiredAmount =
							$networkDesiredSlotComposeInto[sourceProviderId]?.[sourceCapacityId]?.[
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
		// We want: FROM our sourceSlot INTO their targetSlot
		// They want: FROM our sourceSlot INTO their targetSlot (their perspective: FROM)
		Object.entries($userDesiredSlotComposeInto).forEach(([sourceCapacityId, sourceSlots]) => {
			Object.entries(sourceSlots).forEach(([sourceSlotId, targetCompositions]) => {
				Object.entries(targetCompositions).forEach(([targetCapacityId, targetSlots]) => {
					Object.entries(targetSlots).forEach(([targetSlotId, ourDesiredAmount]) => {
						// Find who provides the target capacity
						const targetProviderId = Object.keys($networkCapacities).find(
							(id) => $networkCapacities[id] && $networkCapacities[id][targetCapacityId]
						);

						if (!targetProviderId) return;

						// Check if the target provider wants to compose FROM our slot (opposite direction)
						const theirDesiredAmount =
							$networkDesiredSlotComposeFrom[targetProviderId]?.[sourceCapacityId]?.[
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

// =============================================================================
// REDISTRIBUTION LOGIC - Secondary Level (Elegant Implementation)
// =============================================================================

/**
 * SLOT REDISTRIBUTION SYSTEM
 *
 * When we have allocated slots but desire less for our own compositions,
 * redistribute the excess to others based on:
 * 1. What they want (networkDesiredSlotComposeFrom)
 * 2. Their share proportions (contributorCapacityShares)
 *
 * This maintains efficiency by only computing our own redistribution view.
 *
 * EXAMPLE SCENARIO:
 *
 * 1. Initial State:
 *    - We allocated 10 units from Alice's "morning-consulting" slot
 *    - We only want to use 6 units (userDesiredSlotComposeInto shows 6 units total)
 *    - Excess = 10 - 6 = 4 units available for redistribution
 *
 * 2. Network Desires vs Existing Allocations:
 *    - Bob wants 3 units from Alice's "morning-consulting" → his "project-work"
 *    - Bob already has 1 unit allocated → Gap = 3 - 1 = 2 units
 *    - Carol wants 2 units from Alice's "morning-consulting" → her "design-sprint"
 *    - Carol already has 0 units allocated → Gap = 2 - 0 = 2 units
 *    - Total gap = 4 units (only redistribute to fill unmet gaps)
 *
 * 3. Share Constraints on Gaps:
 *    - Bob has 40% share in Alice's capacity, already allocated 1 unit
 *    - Bob's remaining share capacity = (40% × 5 total) - 1 = 1 unit
 *    - Bob's effective gap = min(2 gap in desires, 1 remaining share) = 1 unit
 *    - Carol has 60% share, already allocated 0 units
 *    - Carol's remaining share capacity = (60% × 5 total) - 0 = 3 units
 *    - Carol's effective gap = min(2 gap in desires, 3 remaining share) = 2 units
 *
 * 4. Gap-Based Redistribution:
 *    - Available excess = 4 units
 *    - Total effective gaps = 1 + 2 = 3 units
 *    - Redistribution ratio = 4/3 = 1.33 (excess can fully cover gaps)
 *    - Bob gets: 1 × 1.0 = 1 unit (fills his gap completely)
 *    - Carol gets: 2 × 1.0 = 2 units (fills her gap completely)
 *    - Total redistributed: 3 units, 1 unit remains as true excess
 *
 * 5. Enhanced Feasible Compositions:
 *    - Bob's feasible composition enhanced from 1 → 2 units (gap filled)
 *    - Carol's feasible composition enhanced from 0 → 2 units (gap filled)
 *    - Efficient gap-filling ensures those who need it most get redistributed capacity
 */

// Helper: Initialize nested object structure safely
const ensureNestedPath = (obj: any, path: string[]) => {
	let current = obj;
	for (const key of path) {
		if (!current[key]) current[key] = {};
		current = current[key];
	}
	return current;
};

// Helper: Calculate total desires from a slot
const calculateSlotDesires = (desires: any, capacityId: string, slotId: string): number => {
	const slotDesires = desires[capacityId]?.[slotId] as
		| Record<string, Record<string, number>>
		| undefined;
	if (!slotDesires) return 0;

	return Object.values(slotDesires).reduce(
		(total: number, targetSlots: Record<string, number>) =>
			total + Object.values(targetSlots).reduce((sum: number, amount: number) => sum + amount, 0),
		0
	);
};

// Helper: Calculate constrained gap for a participant
const calculateConstrainedGap = (
	desiredAmount: number,
	alreadyAllocated: number,
	contributorShare: number,
	capacityTotalQuantity: number
): number => {
	const unmetGap = Math.max(0, desiredAmount - alreadyAllocated);
	if (unmetGap === 0) return 0;

	const maxTheyCanReceive = capacityTotalQuantity * contributorShare;
	const maxAdditionalTheyCanReceive = Math.max(0, maxTheyCanReceive - alreadyAllocated);

	return Math.min(unmetGap, maxAdditionalTheyCanReceive);
};

// Calculate our excess slot capacity available for redistribution
export const redistributableSlotCapacity = derived(
	[allocatedSlotAmounts, userDesiredSlotComposeInto, userDesiredSlotComposeFrom, userCapacities],
	([
		$allocatedSlotAmounts,
		$userDesiredSlotComposeInto,
		$userDesiredSlotComposeFrom,
		$userCapacities
	]) => {
		console.log(
			'[REDISTRIBUTION] Calculating excess slot capacity available for redistribution...'
		);

		if (!$userCapacities) return {};

		const redistributable: Record<string, Record<string, number>> = {};

		Object.entries($allocatedSlotAmounts).forEach(([capacityId, slots]) => {
			// Only consider our own capacities for redistribution
			if (!$userCapacities[capacityId]) return;

			Object.entries(slots).forEach(([slotId, allocatedAmount]) => {
				// Calculate total desires from this slot (both directions)
				const intoDesires = calculateSlotDesires($userDesiredSlotComposeInto, capacityId, slotId);
				const fromDesires = calculateSlotDesires($userDesiredSlotComposeFrom, capacityId, slotId);
				const totalDesires = intoDesires + fromDesires;

				// Calculate excess capacity
				const excessCapacity = allocatedAmount - totalDesires;

				if (excessCapacity > 0) {
					ensureNestedPath(redistributable, [capacityId])[slotId] = excessCapacity;
					console.log(
						`[REDISTRIBUTION] Found ${excessCapacity.toFixed(2)} excess units in ${capacityId}:${slotId} (allocated: ${allocatedAmount}, our desires: ${totalDesires})`
					);
				}
			});
		});

		console.log(
			`[REDISTRIBUTION] Found redistributable capacity in ${Object.keys(redistributable).length} capacities`
		);
		return redistributable;
	}
);

// Enhanced feasible compositions with redistribution
export const feasibleSlotComposeIntoWithRedistribution = derived(
	[
		feasibleSlotComposeInto,
		redistributableSlotCapacity,
		networkDesiredSlotComposeFrom,
		contributorCapacityShares,
		userCapacities
	],
	([
		$feasibleSlotComposeInto,
		$redistributableSlotCapacity,
		$networkDesiredSlotComposeFrom,
		$contributorCapacityShares,
		$userCapacities
	]) => {
		console.log('[REDISTRIBUTION] Enhancing feasible compositions with redistributed capacity...');

		if (!$userCapacities) return $feasibleSlotComposeInto;

		// Start with existing feasible compositions
		const enhanced: UserSlotComposition = JSON.parse(JSON.stringify($feasibleSlotComposeInto));

		// Process each slot with redistributable capacity
		Object.entries($redistributableSlotCapacity).forEach(([ourCapacityId, ourSlots]) => {
			const ourCapacity = $userCapacities[ourCapacityId];
			if (!ourCapacity) return;

			const ourCapacityTotalQuantity =
				ourCapacity.availability_slots?.reduce((total, slot) => total + slot.quantity, 0) || 0;

			Object.entries(ourSlots).forEach(([ourSlotId, excessCapacity]) => {
				// Collect participant gaps for this slot
				const participantGaps: Array<{
					contributorId: string;
					theirCapacityId: string;
					theirSlotId: string;
					effectiveGap: number;
				}> = [];

				Object.entries($networkDesiredSlotComposeFrom).forEach(
					([contributorId, contributorDesires]) => {
						const desiresFromOurSlot = contributorDesires[ourCapacityId]?.[ourSlotId];
						if (!desiresFromOurSlot) return;

						const contributorShare =
							$contributorCapacityShares[contributorId]?.[ourCapacityId] || 0;
						if (contributorShare === 0) return;

						Object.entries(desiresFromOurSlot).forEach(([theirCapacityId, theirSlots]) => {
							Object.entries(theirSlots).forEach(([theirSlotId, desiredAmount]) => {
								const alreadyAllocated =
									$feasibleSlotComposeInto[ourCapacityId]?.[ourSlotId]?.[theirCapacityId]?.[
										theirSlotId
									] || 0;

								const effectiveGap = calculateConstrainedGap(
									desiredAmount,
									alreadyAllocated,
									contributorShare,
									ourCapacityTotalQuantity
								);

								if (effectiveGap > 0) {
									participantGaps.push({
										contributorId,
										theirCapacityId,
										theirSlotId,
										effectiveGap
									});

									console.log(
										`[REDISTRIBUTION] Gap found for ${contributorId}: desired=${desiredAmount}, allocated=${alreadyAllocated}, gap=${effectiveGap}`
									);
								}
							});
						});
					}
				);

				// Apply proportional redistribution if there are gaps
				const totalGaps = participantGaps.reduce((total, p) => total + p.effectiveGap, 0);

				if (totalGaps > 0 && excessCapacity > 0) {
					const redistributionRatio = Math.min(1, excessCapacity / totalGaps);

					participantGaps.forEach(({ theirCapacityId, theirSlotId, effectiveGap }) => {
						const redistributedAmount = effectiveGap * redistributionRatio;

						// Add to enhanced feasible compositions
						const targetPath = ensureNestedPath(enhanced, [
							ourCapacityId,
							ourSlotId,
							theirCapacityId
						]);
						const existingAmount = targetPath[theirSlotId] || 0;
						targetPath[theirSlotId] = existingAmount + redistributedAmount;

						console.log(
							`[REDISTRIBUTION] Enhanced ${ourCapacityId}:${ourSlotId} → ${theirCapacityId}:${theirSlotId}: +${redistributedAmount.toFixed(2)} (from excess redistribution)`
						);
					});
				}
			});
		});

		console.log(
			'[REDISTRIBUTION] Enhanced feasible compositions with gap-based redistributed capacity'
		);
		return enhanced;
	}
);

// =============================================================================
// MUTUAL FEASIBLE WITH REDISTRIBUTION (Enhanced)
// =============================================================================

// Enhanced mutual feasible compositions that include redistribution
export const mutualFeasibleSlotCompositionsWithRedistribution = derived(
	[
		mutualSlotDesires,
		feasibleSlotComposeFrom,
		feasibleSlotComposeInto,
		feasibleSlotComposeIntoWithRedistribution
	],
	([
		$mutualSlotDesires,
		$feasibleSlotComposeFrom,
		$feasibleSlotComposeInto,
		$feasibleSlotComposeIntoWithRedistribution
	]) => {
		console.log(
			'[MUTUAL-FEASIBLE-SLOTS-ENHANCED] Calculating feasible mutual slot compositions with redistribution...'
		);

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
				redistributionEnhanced?: boolean;
			}
		> = {};

		Object.entries($mutualSlotDesires).forEach(([compositionKey, mutualDesire]) => {
			const { sourceCapacityId, sourceSlotId, targetCapacityId, targetSlotId, compositionType } =
				mutualDesire;

			// Get feasible amount (enhanced for 'into', original for 'from')
			const feasibleStore =
				compositionType === 'from'
					? $feasibleSlotComposeFrom
					: $feasibleSlotComposeIntoWithRedistribution;
			const ourFeasibleAmount =
				feasibleStore[sourceCapacityId]?.[sourceSlotId]?.[targetCapacityId]?.[targetSlotId] || 0;

			if (ourFeasibleAmount > 0) {
				const mutualFeasibleAmount = Math.min(ourFeasibleAmount, mutualDesire.theirDesiredAmount);
				const feasibleViability = calculateSlotDesireAlignment(
					mutualFeasibleAmount,
					mutualDesire.theirDesiredAmount
				);
				const constraintRatio = mutualFeasibleAmount / mutualDesire.ourDesiredAmount;

				// Check if redistribution enhanced this composition
				const originalFeasible =
					compositionType === 'into'
						? $feasibleSlotComposeInto[sourceCapacityId]?.[sourceSlotId]?.[targetCapacityId]?.[
								targetSlotId
							] || 0
						: 0;
				const redistributionEnhanced =
					compositionType === 'into' && ourFeasibleAmount > originalFeasible;

				mutualFeasible[compositionKey] = {
					...mutualDesire,
					ourFeasibleAmount: mutualFeasibleAmount,
					feasibleViability,
					constraintRatio,
					redistributionEnhanced
				};

				console.log(
					`[MUTUAL-FEASIBLE-SLOTS-ENHANCED] ${compositionKey}: desired ${mutualDesire.ourDesiredAmount.toFixed(2)} → feasible ${mutualFeasibleAmount.toFixed(2)} (${(constraintRatio * 100).toFixed(1)}% achievable)${redistributionEnhanced ? ' [REDISTRIBUTED]' : ''}`
				);
			}
		});

		console.log(
			`[MUTUAL-FEASIBLE-SLOTS-ENHANCED] Found ${Object.keys(mutualFeasible).length} feasible mutual slot compositions with redistribution`
		);
		return mutualFeasible;
	}
);
