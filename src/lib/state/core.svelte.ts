import { writable, derived, get } from 'svelte/store';
import type { Writable } from 'svelte/store';
import { normalizeShareMap, getSubtreeContributorMap, findNodeById } from '$lib/protocol';
import { applyCapacityFilter, type FilterContext } from '$lib/filters';
import { parseCompositionTarget } from '$lib/validation';
import { resolveToPublicKey } from './users.svelte';
import { USE_HOLSTER_CAPACITIES, USE_HOLSTER_RECOGNITION } from '$lib/config';
import {
	holsterCapacities,
	isLoadingHolsterCapacities
} from './capacities-holster.svelte';
import {
	holsterSogf,
	isLoadingHolsterSogf
} from './recognition-holster.svelte';
import type {
	RootNode,
	CapacitiesCollection,
	Node,
	ShareMap,
	RecognitionCache,
	ProviderCapacity,
	UserSlotComposition,
	ProviderAllocationStateData,
	NetworkAllocationStates
} from '$lib/schema';

// Core reactive state - these form the main reactive chain
// All timestamps are tracked by Gun internally via GUN.state.is()
export const userTree: Writable<RootNode | null> = writable(null);

// Gun-based SOGF (internal)
const gunUserSogf: Writable<ShareMap | null> = writable(null);
const gunIsLoadingSogf = writable(false);

// Conditional export: use Holster or Gun based on feature flag
export const userSogf: Writable<ShareMap | null> = USE_HOLSTER_RECOGNITION
	? holsterSogf
	: gunUserSogf;
export const isLoadingSogf = USE_HOLSTER_RECOGNITION
	? isLoadingHolsterSogf
	: gunIsLoadingSogf;

// Gun-based capacities (internal)
const gunUserCapacities: Writable<CapacitiesCollection | null> = writable(null);
const gunIsLoadingCapacities = writable(false);

// Conditional export: use Holster or Gun based on feature flag
export const userCapacities: Writable<CapacitiesCollection | null> = USE_HOLSTER_CAPACITIES
	? holsterCapacities
	: gunUserCapacities;
export const isLoadingCapacities = USE_HOLSTER_CAPACITIES
	? isLoadingHolsterCapacities
	: gunIsLoadingCapacities;

// DELETED: Timestamp tracking stores - Replaced by Gun's native timestamp tracking
// Use getGunTimestamp() from $lib/utils/gunTimestamp.ts when timestamp comparison is needed

// Loading state flags
export const isLoadingTree = writable(false);

export const isRecalculatingTree = writable(false);
export const isRecalculatingCapacities = writable(false);

// DELETED: Timestamp coordination helper functions - No longer needed
// Gun handles all timestamp tracking internally via GUN.state.is()
// Use getGunTimestamp() from $lib/utils/gunTimestamp.ts when needed

export const networkCapacities: Writable<Record<string, CapacitiesCollection>> = writable({});
// DELETED: networkCapacityShares - Replaced by efficient provider-centric algorithm
// Old percentage-based network sharing no longer needed
// DELETED: networkCapacitySlotQuantities - Replaced by efficient algorithm

// DELETED: userDesiredSlotClaims and networkDesiredSlotClaims - Replaced by unified compose-from model
// Slot claims are now handled as compose-from-self in the composition system

export const userDesiredSlotComposeFrom: Writable<UserSlotComposition> = writable({});
export const userDesiredSlotComposeInto: Writable<UserSlotComposition> = writable({});
// Network stores hold unwrapped data
export const networkDesiredSlotComposeFrom: Writable<Record<string, UserSlotComposition>> =
	writable({});
export const networkDesiredSlotComposeInto: Writable<Record<string, UserSlotComposition>> =
	writable({});

// ===== EFFICIENT DISTRIBUTION ALGORITHM STORES =====

// Provider-side: Our allocation states for our capacities (what we compute and publish)
export const providerAllocationStates: Writable<Record<string, ProviderAllocationStateData>> =
	writable({});

// Recipient-side: Network allocation states from all providers (what we receive)
export const networkAllocationStates: Writable<NetworkAllocationStates> = writable({});

// MOVED: userNetworkCapacitiesWithSlotQuantities stub moved after efficientSlotAllocations definition

// DELETED: Legacy percentage-based store - replaced with efficient algorithm
// All functionality moved to userNetworkCapacitiesWithSlotQuantities and efficientSlotAllocations

// Node Map
export const nodesMap: Writable<Record<string, Node>> = writable({});

// Contributors state
export const contributors = writable<string[]>([]);

// All contributors we've ever had - used for SOGF calculation to ensure removed contributors get 0%
export const allKnownContributors = writable<string[]>([]);

// Recognition cache - maps contributor ID to {ourShare, theirShare}
export const recognitionCache = writable<RecognitionCache>({});

// Core derived stores that must stay here due to Svelte 5 export restrictions
// These form the main reactive chain: recognitionCache -> mutualRecognition -> generalShares -> specificShares

// Derived store for mutual recognition values (min of ourShare and theirShare)
export const mutualRecognition = derived(recognitionCache, ($recognitionCache) => {
	console.log(
		`[MUTUAL-RECOGNITION] Recalculating from cache with ${Object.keys($recognitionCache).length} entries`
	);

	const mutualValues: Record<string, number> = {};

	for (const [contributorId, recognition] of Object.entries($recognitionCache)) {
		// Mutual recognition is the minimum of our share and their share
		const mutualValue = Math.min(recognition.ourShare, recognition.theirShare);
		mutualValues[contributorId] = mutualValue;

		if (mutualValue > 0) {
			console.log(
				`[MUTUAL-RECOGNITION] ✅ ${contributorId}: our=${recognition.ourShare.toFixed(4)}, their=${recognition.theirShare.toFixed(4)}, mutual=${mutualValue.toFixed(4)}`
			);
		}
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
export const generalShares = derived(mutualRecognition, ($mutualRecognition) => {
	/*console.log(
		`[GENERAL-SHARES] ${new Date().toISOString()} Recalculating from mutual recognition:`,
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

// MOVED: specificShares definition moved after subtreeContributorMap to resolve dependency order

// ===== EFFICIENT DISTRIBUTION ALGORITHM IMPLEMENTATION =====

// MOVED: computedProviderAllocations moved after specificShares to use filtered shares

// EFFICIENT ALGORITHM RECIPIENT VIEW
// Extracts our allocations from network allocation states (what we receive from providers)
export const efficientSlotAllocations = derived(
	[networkAllocationStates],
	([$networkAllocationStates]) => {
		console.log('[EFFICIENT-RECIPIENT] Extracting our slot allocations from network states...');

		const ourAllocations: Record<string, Record<string, number>> = {}; // capacityId -> slotId -> allocatedQuantity

		// Process each provider's allocation states
		Object.entries($networkAllocationStates).forEach(([providerId, providerCapacities]) => {
			Object.entries(providerCapacities).forEach(([capacityId, slotAllocations]) => {
				Object.entries(slotAllocations).forEach(([slotId, allocationResult]) => {
					// Get our allocation from this slot
					const ourAllocation = allocationResult.final_allocations[providerId] || 0; // TODO: Use actual user ID

					if (ourAllocation > 0) {
						if (!ourAllocations[capacityId]) {
							ourAllocations[capacityId] = {};
						}
						ourAllocations[capacityId][slotId] = ourAllocation;
					}
				});
			});
		});

		console.log(
			`[EFFICIENT-RECIPIENT] Extracted allocations for ${Object.keys(ourAllocations).length} capacities`
		);
		return ourAllocations;
	}
);

// INVENTORY DATA SOURCE: Combines network capacities with our actual slot allocations
// This replaces the old userNetworkCapacitiesWithSlotQuantities with efficient algorithm data
export const userNetworkCapacitiesWithSlotQuantities = derived(
	[networkCapacities, efficientSlotAllocations],
	([$networkCapacities, $efficientSlotAllocations]) => {
		console.log('[INVENTORY-DATA] Building capacity inventory with efficient allocations...');

		const inventoryData: Record<string, any> = {};

		// Process each provider's capacities
		Object.entries($networkCapacities).forEach(([providerId, providerCapacities]) => {
			Object.entries(providerCapacities).forEach(([capacityId, capacity]) => {
				// Start with the base capacity data
				const inventoryCapacity = {
					...capacity,
					provider_id: providerId,
					// Add our actual allocated quantities for each slot
					availability_slots:
						capacity.availability_slots?.map((slot) => ({
							...slot,
							// Our allocated quantity from efficient algorithm
							allocated_quantity: $efficientSlotAllocations[capacityId]?.[slot.id] || 0,
							// Availability is the original slot quantity
							available_quantity: slot.quantity
						})) || []
				};

				inventoryData[capacityId] = inventoryCapacity;
			});
		});

		console.log(
			`[INVENTORY-DATA] Built inventory data for ${Object.keys(inventoryData).length} capacities with efficient allocations`
		);
		return inventoryData;
	}
);

// TRANSPARENCY & STALENESS DETECTION
// Allows recipients to verify provider calculations and detect stale data
export const allocationTransparencyAnalysis = derived(
	[networkAllocationStates, networkDesiredSlotComposeFrom, mutualRecognition],
	([$networkAllocationStates, $networkDesiredSlotComposeFrom, $mutualRecognition]) => {
		console.log('[TRANSPARENCY] Analyzing provider allocation transparency and staleness...');

		const analysis: Record<
			string,
			Record<
				string,
				{
					providerId: string;
					capacityId: string;
					slotId: string;
					isStale: boolean;
					canRecompute: boolean;
					staleness_reasons: string[];
					our_expected_allocation?: number;
					provider_claimed_allocation: number;
					computation_age_minutes: number;
				}
			>
		> = {};

		// Process each provider's allocation states
		Object.entries($networkAllocationStates).forEach(([providerId, providerCapacities]) => {
			Object.entries(providerCapacities).forEach(([capacityId, slotAllocations]) => {
				Object.entries(slotAllocations).forEach(([slotId, allocationResult]) => {
					const staleness_reasons: string[] = [];
					let canRecompute = true;

					// Check computation age
					const computationTime = new Date(allocationResult.computation_timestamp);
					const now = new Date();
					const ageMinutes = (now.getTime() - computationTime.getTime()) / (1000 * 60);

					// Check if compose-from desires have changed since computation
					const currentDesires: Record<string, number> = {};
					Object.entries($networkDesiredSlotComposeFrom).forEach(
						([recipientId, composeFromData]) => {
							// Look for desires to compose FROM this slot INTO any target
							const fromThisSlot = composeFromData[capacityId]?.[slotId] || {};
							let totalDesiredFromThisSlot = 0;
							Object.values(fromThisSlot).forEach((targetSlots: any) => {
								Object.values(targetSlots).forEach((amount: any) => {
									totalDesiredFromThisSlot += Number(amount) || 0;
								});
							});
							if (totalDesiredFromThisSlot > 0) {
								currentDesires[recipientId] = totalDesiredFromThisSlot;
							}
						}
					);

					// Compare with provider's recorded desires
					const providerRecordedDesires = allocationResult.all_desires;
					const desiresChanged =
						JSON.stringify(currentDesires) !== JSON.stringify(providerRecordedDesires);

					if (desiresChanged) {
						staleness_reasons.push('Desires have changed since provider computation');
					}

					// Check if MR values have changed
					let mrChanged = false;
					Object.keys(allocationResult.mr_values).forEach((recipientId) => {
						const currentMR = $mutualRecognition[recipientId] || 0;
						const providerMR = allocationResult.mr_values[recipientId] || 0;
						if (Math.abs(currentMR - providerMR) > 0.0001) {
							mrChanged = true;
						}
					});

					if (mrChanged) {
						staleness_reasons.push('Mutual recognition values have changed');
					}

					// Check computation age (consider stale after 5 minutes)
					if (ageMinutes > 5) {
						staleness_reasons.push(`Computation is ${ageMinutes.toFixed(1)} minutes old`);
					}

					// Determine if we can recompute locally
					const hasAllMRValues = Object.keys(currentDesires).every(
						(recipientId) => $mutualRecognition[recipientId] !== undefined
					);

					if (!hasAllMRValues) {
						canRecompute = false;
					}

					// Calculate what we expect our allocation to be
					let ourExpectedAllocation: number | undefined;
					const ourUserId = providerId; // TODO: Get actual user ID

					if (canRecompute && hasAllMRValues) {
						// Recompute locally to verify
						const mutuallyDesiring = Object.keys(currentDesires);
						if (mutuallyDesiring.includes(ourUserId)) {
							let filteredMRSum = 0;
							mutuallyDesiring.forEach((recipientId) => {
								filteredMRSum += $mutualRecognition[recipientId] || 0;
							});

							if (filteredMRSum > 0) {
								const ourMR = $mutualRecognition[ourUserId] || 0;
								const normalizedShare = ourMR / filteredMRSum;
								const rawAllocation = allocationResult.total_quantity * normalizedShare;
								const ourDesire = currentDesires[ourUserId] || 0;
								ourExpectedAllocation = Math.min(rawAllocation, ourDesire);
								// Note: This is simplified - doesn't include redistribution
							}
						}
					}

					const isStale = staleness_reasons.length > 0;
					const providerClaimedAllocation = allocationResult.final_allocations[ourUserId] || 0;

					// Store analysis
					if (!analysis[capacityId]) analysis[capacityId] = {};
					analysis[capacityId][slotId] = {
						providerId,
						capacityId,
						slotId,
						isStale,
						canRecompute,
						staleness_reasons,
						our_expected_allocation: ourExpectedAllocation,
						provider_claimed_allocation: providerClaimedAllocation,
						computation_age_minutes: ageMinutes
					};

					if (
						isStale ||
						(ourExpectedAllocation &&
							Math.abs(ourExpectedAllocation - providerClaimedAllocation) > 0.01)
					) {
						console.log(
							`[TRANSPARENCY] Potential issue with ${providerId}:${capacityId}:${slotId} - Stale: ${isStale}, Expected: ${ourExpectedAllocation?.toFixed(2)}, Provider: ${providerClaimedAllocation.toFixed(2)}`
						);
					}
				});
			});
		});

		console.log(
			`[TRANSPARENCY] Analyzed ${Object.keys(analysis).length} capacities for staleness and transparency`
		);
		return analysis;
	}
);

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

// SPECIFIC SHARES: Apply capacity filters to general shares
// Implements: Specific-Share(You, Provider, Capacity) = General-Share(You, Provider) × Filter(You, Capacity) / Σ (General-Share(Each-Filtered-Participant, Provider) × Filter(Each-Filtered-Participant, Capacity))
export const specificShares = derived(
	[generalShares, userCapacities, subtreeContributorMap],
	([$generalShares, $userCapacities, $subtreeContributorMap]) => {
		console.log('[SPECIFIC-SHARES] Calculating filtered shares per capacity...');

		const capacitySpecificShares: Record<string, Record<string, number>> = {};

		if (!$userCapacities || Object.keys($generalShares).length === 0) {
			return capacitySpecificShares;
		}

		// Process each of our provider capacities
		Object.entries($userCapacities).forEach(([capacityId, capacity]) => {
			// Only process provider capacities (our own capacities)
			if (typeof capacity !== 'object' || capacity === null || !('recipient_shares' in capacity)) return;

			// Type assertion: we've verified it has recipient_shares, so it's a ProviderCapacity
			const providerCapacity = capacity as unknown as ProviderCapacity;

			// Create filter context for this capacity
			const context: FilterContext = {
				subtreeContributors: $subtreeContributorMap
			};

			// Apply capacity filter to the general shares
			// This will return the filtered and normalized shares for this capacity
			const capacityShares = applyCapacityFilter(providerCapacity, $generalShares, context);

			capacitySpecificShares[capacityId] = capacityShares;

			console.log(
				`[SPECIFIC-SHARES] Capacity ${capacityId}: ${Object.keys(capacityShares).length} filtered participants from ${Object.keys($generalShares).length} total`
			);
		});

		console.log(
			`[SPECIFIC-SHARES] Generated specific shares for ${Object.keys(capacitySpecificShares).length} capacities`
		);
		return capacitySpecificShares;
	}
);

// ===== COMPOSITION TARGET RESOLUTION HELPERS =====

/**
 * Resolve a composition target to recipient IDs for the allocation algorithm
 * Handles capacity IDs, individual pubkeys, and collective targets
 */
function resolveCompositionTargetToRecipients(
	targetId: string,
	networkCapacities: Record<string, any>
): string[] {
	const parsed = parseCompositionTarget(targetId);

	switch (parsed.type) {
		case 'individual':
			// Direct pubkey - return as recipient
			return parsed.recipients;

		case 'collective':
			// Multiple pubkeys - return all as recipients
			return parsed.recipients;

		case 'capacity':
			// Traditional capacity ID - find who owns this capacity
			const providerId = Object.keys(networkCapacities).find(
				(id) => networkCapacities[id] && networkCapacities[id][targetId]
			);
			return providerId ? [providerId] : [];

		default:
			console.warn(`[COMPOSITION-TARGET] Unknown target type for: ${targetId}`);
			return [];
	}
}

/**
 * Check if a composition target represents self-consumption for a given user
 */
function isTargetSelfConsumption(targetId: string, userPubkey: string): boolean {
	const parsed = parseCompositionTarget(targetId);
	return parsed.type === 'individual' && parsed.recipients.includes(userPubkey);
}

// EFFICIENT ALGORITHM IMPLEMENTATION (using specific shares)
// Computes provider allocation states using the efficient algorithm from distribution.md
export const computedProviderAllocations = derived(
	[
		userCapacities,
		networkDesiredSlotComposeFrom,
		userDesiredSlotComposeInto,
		specificShares,
		networkCapacities
	],
	([
		$userCapacities,
		$networkDesiredSlotComposeFrom,
		$userDesiredSlotComposeInto,
		$specificShares,
		$networkCapacities
	]) => {
		console.log(
			'[EFFICIENT-ALGORITHM] Computing provider allocations using mutual desires from compose-from/into...'
		);

		const allocations: Record<string, ProviderAllocationStateData> = {};

		if (!$userCapacities) return allocations;

		// Process each of our capacities
		Object.entries($userCapacities).forEach(([capacityId, capacity]) => {
			// Only process provider capacities (our own capacities)
			if (!('recipient_shares' in capacity)) return;

			const providerCapacity = capacity as ProviderCapacity;
			const capacityAllocations: ProviderAllocationStateData = {};

			// Process each slot in this capacity
			providerCapacity.availability_slots?.forEach((slot) => {
				// Phase 1: Calculate mutual desires from compose-from and compose-into
				const recipientComposeFromDesires: Record<string, number> = {};
				const providerComposeIntoDesires: Record<string, number> = {};
				const mutualDesires: Record<string, number> = {};

				// Collect recipients' compose-from desires (they want FROM our slot)
				Object.entries($networkDesiredSlotComposeFrom).forEach(([recipientId, composeFromData]) => {
					// Look for desires to compose FROM our slot INTO any target
					const fromOurSlot = composeFromData[capacityId]?.[slot.id] || {};
					let totalDesiredFromOurSlot = 0;
					Object.values(fromOurSlot).forEach((targetSlots: any) => {
						Object.values(targetSlots).forEach((amount: any) => {
							totalDesiredFromOurSlot += Number(amount) || 0;
						});
					});
					if (totalDesiredFromOurSlot > 0) {
						recipientComposeFromDesires[recipientId] = totalDesiredFromOurSlot;
					}
				});

				// Collect our compose-into desires (we want to give FROM our slot TO recipients)
				const ourComposeIntoForThisSlot = $userDesiredSlotComposeInto[capacityId]?.[slot.id] || {};
				Object.entries(ourComposeIntoForThisSlot).forEach(([targetId, targetSlots]) => {
					// 🎯 ELEGANT: Resolve composition target to actual recipient IDs
					const recipientIds = resolveCompositionTargetToRecipients(targetId, $networkCapacities);

					Object.entries(targetSlots).forEach(([targetSlotId, amount]) => {
						const amountPerRecipient = Number(amount) || 0;

						// For collectives, split the amount equally among recipients
						// TODO (in future): replace with collective recognition distributions! collective.svelte.ts
						const splitAmount =
							recipientIds.length > 0 ? amountPerRecipient / recipientIds.length : 0;

						recipientIds.forEach((recipientId) => {
							providerComposeIntoDesires[recipientId] =
								(providerComposeIntoDesires[recipientId] || 0) + splitAmount;
						});

						if (recipientIds.length > 1) {
							console.log(
								`[ELEGANT-COMPOSITION] Split ${amountPerRecipient} units among ${recipientIds.length} collective recipients (${splitAmount} each)`
							);
						}
					});
				});

				// Phase 2: Calculate mutual desires (minimum of both expressions)
				const allRecipients = new Set([
					...Object.keys(recipientComposeFromDesires),
					...Object.keys(providerComposeIntoDesires)
				]);

				allRecipients.forEach((recipientId) => {
					const recipientDesire = recipientComposeFromDesires[recipientId] || 0;
					const providerDesire = providerComposeIntoDesires[recipientId] || 0;
					const mutualDesire = Math.min(recipientDesire, providerDesire);

					if (mutualDesire > 0) {
						mutualDesires[recipientId] = mutualDesire;
					}
				});

				// Phase 3: Identify mutually-desiring recipients
				const mutuallyDesiringRecipients = Object.keys(mutualDesires);

				if (mutuallyDesiringRecipients.length === 0) {
					// No mutual desires for this slot - create empty allocation with full transparency
					capacityAllocations[slot.id] = {
						slot_id: slot.id,
						total_quantity: slot.quantity,
						all_desires: {}, // No mutual desires
						mutually_desiring_recipients: [],
						mr_values: {},
						filtered_mr_sum: 0,
						normalized_mr_shares: {},
						raw_mr_allocations: {},
						desire_constrained_allocations: {},
						unsatisfied_recipients: [],
						redistribution_amounts: {},
						final_allocations: {},
						unused_capacity: slot.quantity,

						// MR-BASED ALLOCATION RESULTS (empty when no mutual desires)
						mr_based_final_allocations: {},
						mr_based_unused_capacity: slot.quantity,

						// DESIRE-SCALED ALLOCATION (empty when no mutual desires)
						desire_scaled_provider_desires: {},
						desire_scaled_provider_sum: 0,
						desire_scaled_normalized_shares: {},
						desire_scaled_raw_allocations: {},
						desire_scaled_constrained_allocations: {},
						desire_scaled_final_allocations: {},
						desire_scaled_unused_capacity: slot.quantity,

						// ALLOCATION DEVIATION ANALYSIS (no deviations when no allocations)
						mr_vs_desire_deviation: {},
						total_absolute_deviation: 0,
						max_recipient_deviation: 0,
						deviation_recipients: [],

						computation_timestamp: new Date().toISOString(),
						algorithm_version: 'dual_allocation_v1'
					};
					return;
				}

				// Phase 3: Use capacity-specific filtered shares (already normalized)
				const capacityShares = $specificShares[capacityId] || {};
				let filteredMRSum = 0;
				const mrValues: Record<string, number> = {};

				mutuallyDesiringRecipients.forEach((recipientId) => {
					const mrValue = capacityShares[recipientId] || 0;
					mrValues[recipientId] = mrValue;
					filteredMRSum += mrValue;
				});

				const normalizedMRShares: Record<string, number> = {};
				if (filteredMRSum > 0) {
					mutuallyDesiringRecipients.forEach((recipientId) => {
						normalizedMRShares[recipientId] = mrValues[recipientId] / filteredMRSum;
					});
				}

				// Phase 4: Calculate raw MR allocation and apply desire constraints
				const rawMRAllocations: Record<string, number> = {};
				const desireConstrainedAllocations: Record<string, number> = {};
				let usedCapacity = 0;

				mutuallyDesiringRecipients.forEach((recipientId) => {
					const normalizedShare = normalizedMRShares[recipientId] || 0;
					const rawMRAllocation = slot.quantity * normalizedShare;
					const mutualDesiredAmount = mutualDesires[recipientId];
					const constrainedAllocation = Math.min(rawMRAllocation, mutualDesiredAmount);

					// Store transparent intermediate calculations
					rawMRAllocations[recipientId] = rawMRAllocation;

					if (constrainedAllocation > 0) {
						desireConstrainedAllocations[recipientId] = constrainedAllocation;
						usedCapacity += constrainedAllocation;
					}
				});

				// Phase 5: Redistribute unused capacity to unsatisfied recipients
				const unusedCapacity = slot.quantity - usedCapacity;
				const redistributionAmounts: Record<string, number> = {};
				const finalAllocations: Record<string, number> = { ...desireConstrainedAllocations };

				// Find recipients who are still unsatisfied (based on mutual desires)
				const unsatisfiedRecipients = mutuallyDesiringRecipients.filter((recipientId) => {
					const allocated = desireConstrainedAllocations[recipientId] || 0;
					const mutuallyDesired = mutualDesires[recipientId];
					return allocated < mutuallyDesired;
				});

				if (unusedCapacity > 0 && unsatisfiedRecipients.length > 0) {
					// Calculate redistribution proportions based on normalized MR shares
					let unsatisfiedMRSum = 0;
					unsatisfiedRecipients.forEach((recipientId) => {
						unsatisfiedMRSum += normalizedMRShares[recipientId] || 0;
					});

					if (unsatisfiedMRSum > 0) {
						unsatisfiedRecipients.forEach((recipientId) => {
							const redistributionShare = (normalizedMRShares[recipientId] || 0) / unsatisfiedMRSum;
							const redistributionAmount = unusedCapacity * redistributionShare;
							const currentAllocation = desireConstrainedAllocations[recipientId] || 0;
							const mutuallyDesiredAmount = mutualDesires[recipientId];
							const maxAdditional = mutuallyDesiredAmount - currentAllocation;
							const actualRedistribution = Math.min(redistributionAmount, maxAdditional);

							if (actualRedistribution > 0) {
								redistributionAmounts[recipientId] = actualRedistribution;
								finalAllocations[recipientId] = currentAllocation + actualRedistribution;
								usedCapacity += actualRedistribution;
							}
						});
					}
				}

				// DESIRE-SCALED ALLOCATION COMPUTATION (Sense-Drive approach - Desire Sovereignty)
				const desireScaledProviderDesires: Record<string, number> = {};
				let desireScaledProviderSum = 0;

				// Extract provider desires for mutually desiring recipients
				mutuallyDesiringRecipients.forEach((recipientId) => {
					const providerDesire = providerComposeIntoDesires[recipientId] || 0;
					desireScaledProviderDesires[recipientId] = providerDesire;
					desireScaledProviderSum += providerDesire;
				});

				// Normalize provider desires among mutually desiring recipients
				const desireScaledNormalizedShares: Record<string, number> = {};
				if (desireScaledProviderSum > 0) {
					mutuallyDesiringRecipients.forEach((recipientId) => {
						desireScaledNormalizedShares[recipientId] =
							desireScaledProviderDesires[recipientId] / desireScaledProviderSum;
					});
				}

				// Calculate raw desire-scaled allocations
				const desireScaledRawAllocations: Record<string, number> = {};
				const desireScaledConstrainedAllocations: Record<string, number> = {};
				let desireScaledUsedCapacity = 0;

				mutuallyDesiringRecipients.forEach((recipientId) => {
					const normalizedShare = desireScaledNormalizedShares[recipientId] || 0;
					const rawDesireAllocation = slot.quantity * normalizedShare;
					const mutualDesiredAmount = mutualDesires[recipientId];
					const constrainedAllocation = Math.min(rawDesireAllocation, mutualDesiredAmount);

					desireScaledRawAllocations[recipientId] = rawDesireAllocation;
					if (constrainedAllocation > 0) {
						desireScaledConstrainedAllocations[recipientId] = constrainedAllocation;
						desireScaledUsedCapacity += constrainedAllocation;
					}
				});

				// Redistribute unused capacity in desire-scaled approach
				const desireScaledUnusedCapacity = slot.quantity - desireScaledUsedCapacity;
				const desireScaledFinalAllocations: Record<string, number> = {
					...desireScaledConstrainedAllocations
				};

				// Find unsatisfied recipients in desire-scaled approach
				const desireScaledUnsatisfiedRecipients = mutuallyDesiringRecipients.filter(
					(recipientId) => {
						const allocated = desireScaledConstrainedAllocations[recipientId] || 0;
						const mutuallyDesired = mutualDesires[recipientId];
						return allocated < mutuallyDesired;
					}
				);

				if (desireScaledUnusedCapacity > 0 && desireScaledUnsatisfiedRecipients.length > 0) {
					// Redistribute by desire preference proportions among unsatisfied
					let unsatisfiedDesireSum = 0;
					desireScaledUnsatisfiedRecipients.forEach((recipientId) => {
						unsatisfiedDesireSum += desireScaledNormalizedShares[recipientId] || 0;
					});

					if (unsatisfiedDesireSum > 0) {
						desireScaledUnsatisfiedRecipients.forEach((recipientId) => {
							const redistributionShare =
								(desireScaledNormalizedShares[recipientId] || 0) / unsatisfiedDesireSum;
							const redistributionAmount = desireScaledUnusedCapacity * redistributionShare;
							const currentAllocation = desireScaledConstrainedAllocations[recipientId] || 0;
							const mutuallyDesiredAmount = mutualDesires[recipientId];
							const maxAdditional = mutuallyDesiredAmount - currentAllocation;
							const actualRedistribution = Math.min(redistributionAmount, maxAdditional);

							if (actualRedistribution > 0) {
								desireScaledFinalAllocations[recipientId] =
									currentAllocation + actualRedistribution;
								desireScaledUsedCapacity += actualRedistribution;
							}
						});
					}
				}

				// Calculate allocation deviations (MR-Based vs Desire-Scaled approaches)
				const mrVsDesireDeviation: Record<string, number> = {};
				let totalAbsoluteDeviation = 0;
				let maxRecipientDeviation = 0;
				const deviationRecipients: string[] = [];

				mutuallyDesiringRecipients.forEach((recipientId) => {
					const mrBasedAllocation = finalAllocations[recipientId] || 0;
					const desireScaledAllocation = desireScaledFinalAllocations[recipientId] || 0;
					const deviation = mrBasedAllocation - desireScaledAllocation;
					const absDeviation = Math.abs(deviation);

					mrVsDesireDeviation[recipientId] = deviation;
					totalAbsoluteDeviation += absDeviation;

					if (absDeviation > maxRecipientDeviation) {
						maxRecipientDeviation = absDeviation;
					}

					// Flag recipients with significant deviation (>0.1 units)
					if (absDeviation > 0.1) {
						deviationRecipients.push(recipientId);
					}
				});

				// Store the allocation result for this slot with DUAL ALLOCATION TRANSPARENCY
				capacityAllocations[slot.id] = {
					slot_id: slot.id,
					total_quantity: slot.quantity,

					// Phase 1: All mutual desires (transparent input from compose-from/into)
					all_desires: mutualDesires,

					// Phase 2: Mutually-desiring recipients (transparent)
					mutually_desiring_recipients: mutuallyDesiringRecipients,

					// Phase 3: MR calculations using specific shares (transparent)
					mr_values: mrValues,
					filtered_mr_sum: filteredMRSum,
					normalized_mr_shares: normalizedMRShares,

					// Phase 4: Allocation steps (transparent)
					raw_mr_allocations: rawMRAllocations,
					desire_constrained_allocations: desireConstrainedAllocations,

					// Phase 5: Redistribution details (transparent)
					unsatisfied_recipients: unsatisfiedRecipients,
					redistribution_amounts: redistributionAmounts,

					// MR-BASED ALLOCATION RESULTS (Form-Drive approach - Priority Sovereignty)
					final_allocations: finalAllocations, // LEGACY: backward compatibility
					unused_capacity: slot.quantity - usedCapacity, // LEGACY: backward compatibility
					mr_based_final_allocations: finalAllocations,
					mr_based_unused_capacity: slot.quantity - usedCapacity,

					// DESIRE-SCALED ALLOCATION (Sense-Drive approach - Desire Sovereignty)
					desire_scaled_provider_desires: desireScaledProviderDesires,
					desire_scaled_provider_sum: desireScaledProviderSum,
					desire_scaled_normalized_shares: desireScaledNormalizedShares,
					desire_scaled_raw_allocations: desireScaledRawAllocations,
					desire_scaled_constrained_allocations: desireScaledConstrainedAllocations,
					desire_scaled_final_allocations: desireScaledFinalAllocations,
					desire_scaled_unused_capacity: slot.quantity - desireScaledUsedCapacity,

					// ALLOCATION DEVIATION ANALYSIS (Play-Drive insights)
					mr_vs_desire_deviation: mrVsDesireDeviation,
					total_absolute_deviation: totalAbsoluteDeviation,
					max_recipient_deviation: maxRecipientDeviation,
					deviation_recipients: deviationRecipients,

					// Metadata
					computation_timestamp: new Date().toISOString(),
					algorithm_version: 'dual_allocation_v1'
				};

				console.log(
					`[EFFICIENT-ALGORITHM] Slot ${slot.id}: ${mutuallyDesiringRecipients.length} desiring recipients, ${usedCapacity.toFixed(2)}/${slot.quantity} allocated, ${(slot.quantity - usedCapacity).toFixed(2)} unused`
				);
			});

			allocations[capacityId] = capacityAllocations;
		});

		console.log(
			`[EFFICIENT-ALGORITHM] Computed allocations for ${Object.keys(allocations).length} capacities using specific shares`
		);
		return allocations;
	}
);

// ALLOCATION DEVIATION ANALYSIS: Compare MR-based vs Desire-scaled allocations (Play-Drive insights)
export const allocationDeviationAnalysis = derived(
	[computedProviderAllocations],
	([$computedProviderAllocations]) => {
		console.log('[DEVIATION-ANALYSIS] Analyzing MR vs Provider allocation deviations...');

		const analysis: Record<
			string, // capacityId
			{
				capacity_total_deviation: number;
				capacity_max_deviation: number;
				capacity_deviation_count: number;
				slots: Record<
					string, // slotId
					{
						total_absolute_deviation: number;
						max_recipient_deviation: number;
						deviation_recipients: string[];
						significant_deviations: Array<{
							recipient_id: string;
							mr_allocation: number;
							provider_allocation: number;
							deviation: number;
							deviation_percentage: number;
						}>;
						approach_preference: 'mr_based_favored' | 'desire_scaled_favored' | 'balanced';
					}
				>;
			}
		> = {};

		Object.entries($computedProviderAllocations).forEach(([capacityId, slotAllocations]) => {
			let capacityTotalDeviation = 0;
			let capacityMaxDeviation = 0;
			let capacityDeviationCount = 0;
			const slotAnalysis: Record<string, any> = {};

			Object.entries(slotAllocations).forEach(([slotId, allocation]) => {
				const slotDeviation = allocation.total_absolute_deviation;
				const slotMaxDeviation = allocation.max_recipient_deviation;

				capacityTotalDeviation += slotDeviation;
				if (slotMaxDeviation > capacityMaxDeviation) {
					capacityMaxDeviation = slotMaxDeviation;
				}
				if (allocation.deviation_recipients.length > 0) {
					capacityDeviationCount++;
				}

				// Analyze significant deviations
				const significantDeviations: Array<{
					recipient_id: string;
					mr_allocation: number;
					provider_allocation: number;
					deviation: number;
					deviation_percentage: number;
				}> = [];

				allocation.deviation_recipients.forEach((recipientId) => {
					const mrBasedAllocation = allocation.mr_based_final_allocations[recipientId] || 0;
					const desireScaledAllocation =
						allocation.desire_scaled_final_allocations[recipientId] || 0;
					const deviation = allocation.mr_vs_desire_deviation[recipientId] || 0;
					const totalAllocation = Math.max(mrBasedAllocation, desireScaledAllocation);
					const deviationPercentage =
						totalAllocation > 0 ? (Math.abs(deviation) / totalAllocation) * 100 : 0;

					significantDeviations.push({
						recipient_id: recipientId,
						mr_allocation: mrBasedAllocation,
						provider_allocation: desireScaledAllocation,
						deviation: deviation,
						deviation_percentage: deviationPercentage
					});
				});

				// Determine which approach is favored for this slot
				let approachPreference: 'mr_based_favored' | 'desire_scaled_favored' | 'balanced' =
					'balanced';
				if (significantDeviations.length > 0) {
					const avgDeviation =
						significantDeviations.reduce((sum, dev) => sum + dev.deviation, 0) /
						significantDeviations.length;
					if (avgDeviation > 0.5) {
						approachPreference = 'mr_based_favored'; // MR-based gives more on average
					} else if (avgDeviation < -0.5) {
						approachPreference = 'desire_scaled_favored'; // Desire-scaled gives more on average
					}
				}

				slotAnalysis[slotId] = {
					total_absolute_deviation: slotDeviation,
					max_recipient_deviation: slotMaxDeviation,
					deviation_recipients: allocation.deviation_recipients,
					significant_deviations: significantDeviations,
					approach_preference: approachPreference
				};
			});

			analysis[capacityId] = {
				capacity_total_deviation: capacityTotalDeviation,
				capacity_max_deviation: capacityMaxDeviation,
				capacity_deviation_count: capacityDeviationCount,
				slots: slotAnalysis
			};
		});

		return analysis;
	}
);

// DELETED: capacityShares - Replaced by efficient provider-centric algorithm
// Old recipient-centric percentage allocation approach removed in favor of
// computedProviderAllocations which implements the efficient algorithm

// DELETED: capacitySlotQuantities - Replaced by efficient provider-centric algorithm
// Old approach calculated slot quantities from recipient perspective using averaged percentages
// New approach: computedProviderAllocations provides discrete per-slot allocations

// DELETED: contributorCapacityShares - Replaced by efficient provider-centric algorithm
// Old approach mapped contributor shares using percentage-based allocation
// New approach: Provider computes allocations directly, recipients get transparent results

// DELETED: userCapacitiesWithShares - Replaced by efficient provider-centric algorithm
// Old approach combined capacities with percentage-based recipient shares
// New approach: computedProviderAllocations contains all allocation data with full transparency

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

// SLOT ALLOCATION HELPER FUNCTIONS (moved from slots.svelte.ts)
function getSlotById(capacity: any, slotId: string) {
	return capacity.availability_slots?.find((slot: any) => slot.id === slotId);
}

// DELETED: slotAllocationAnalysis - Replaced by efficient provider-centric algorithm
// Old approach analyzed constraints from recipient perspective using percentage shares
// New approach: allocationTransparencyAnalysis provides staleness detection and verification

// DELETED: Old slot allocation stores - Replaced by efficient provider-centric algorithm
// - feasibleSlotClaims: Old recipient-centric feasibility analysis
// - allocatedSlots: Old percentage-based allocation
// - slotClaimMetadata: Old constraint metadata
// New approach: efficientSlotAllocations provides direct allocations from provider computations

// TEMPORARY COMPATIBILITY BRIDGE: Convert efficient allocations to old format for composition system
// TODO: Update composition system to use efficientSlotAllocations directly
export const allocatedSlotAmounts = derived(
	[efficientSlotAllocations],
	([$efficientSlotAllocations]) => {
		console.log('[COMPATIBILITY-BRIDGE] Converting efficient allocations to old flat format...');
		// Convert efficient slot allocations to the old allocatedSlotAmounts format
		// This maintains compatibility with the composition system until it's updated
		return $efficientSlotAllocations;
	}
);

// TEMPORARY COMPATIBILITY BRIDGE: Stub for contributorCapacityShares
// TODO: Update composition system to not need this store
export const contributorCapacityShares = derived(
	[mutualRecognition],
	(): Record<string, Record<string, number>> => {
		console.log('[COMPATIBILITY-BRIDGE] Providing empty contributorCapacityShares stub...');
		// Return empty object - composition system should be updated to not rely on this
		// The new efficient algorithm handles allocation constraints internally
		return {};
	}
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
