import { writable, derived, get } from 'svelte/store';
import type { Writable, Readable } from 'svelte/store';
import {
	sharesOfGeneralFulfillmentMap,
	normalizeShareMap,
	getAllContributorsFromTree,
	getSubtreeContributorMap,
	findNodeById,
	computeQuantityShare
} from '$lib/protocol';
import { applyCapacityFilter, type FilterContext } from '$lib/filters';
import type { RootNode, CapacitiesCollection, Node, ShareMap, RecognitionCache } from '$lib/schema';

// Core reactive state - these form the main reactive chain
export const userTree: Writable<RootNode | null> = writable(null);
export const userSogf: Writable<ShareMap | null> = writable(null);
export const userCapacities: Writable<CapacitiesCollection | null> = writable(null);

export const networkCapacities: Writable<Record<string, CapacitiesCollection>> = writable({});
export const networkCapacityShares: Writable<Record<string, Record<string, number>>> = writable({});

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
					// Add the capacity with just our share and properly computed quantity
					filteredCapacities[capacityId] = {
						...capacity,
						share_percentage: share,
						computed_quantity: computeQuantityShare(capacity, share),
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

// COMPOSE-FROM MODEL:
// mapping our capacityIds to the capacityIds of our userNetworkCapacitiesWithShares that we want to compose from
// userDesiredComposeFrom represents: "I want to composeInto MY capacity FROM THEIR capacity (via the share I have in their capacity)"
// Structure: myCapacityId → theirCapacityId → desiredAbsoluteUnits
// Example: "cooking-skill" → "ingredients-supply" → 25 means:
//   "I want to compose 25 units FROM their ingredients supply INTO my cooking skill"
//   Note: This only works because I already have some share percentage in their ingredients capacity
export const userDesiredComposeFrom = writable<Record<string, Record<string, number>>>({});

// COMPOSE-INTO MODEL (opposite direction):
// userDesiredComposeInto represents: "I want to composeInto THEIR capacity using MY capacity (via the share they have in my capacity)"
// Structure: myCapacityId → theirCapacityId → desiredAbsoluteUnits
// Example: "cooking-skill" → "meal-service" → 30 means:
//   "I want to compose 30 units of my cooking skill INTO their meal service"
//   Note: This only works because they already have some share percentage in my cooking capacity
export const userDesiredComposeInto = writable<Record<string, Record<string, number>>>({});

// NETWORK COMPOSE-FROM MODEL:
// networkDesiredComposeFrom represents what others want to composeInto via their shares
// Structure: userId → theirCapacityId → ourCapacityId → desiredAbsoluteUnits
// Example: "alice" → "ingredients-supply" → "cooking-skill" → 20 means:
//   "Alice wants to compose 20 units FROM our cooking skill INTO her ingredients supply"
//   Note: This only works because Alice already has some share percentage in our cooking capacity
export const networkDesiredComposeFrom = writable<
	Record<string, Record<string, Record<string, number>>>
>({});

// NETWORK COMPOSE-INTO MODEL (opposite direction):
// networkDesiredComposeInto represents what others want to compose into our capacities via our shares
// Structure: userId → theirCapacityId → ourCapacityId → desiredAbsoluteUnits
// Example: "alice" → "ingredients-supply" → "cooking-skill" → 15 means:
//   "Alice wants to compose 15 units of her ingredients INTO our cooking skill"
//   Note: This only works because we already have some share percentage in Alice's ingredients capacity
export const networkDesiredComposeInto = writable<
	Record<string, Record<string, Record<string, number>>>
>({});

// FEASIBLE COMPOSE-FROM: Constrains raw desires by share access and resource allocation
// Step 1: Convert absolute unit desires to share-constrained feasible units
// Step 2: Apply resource allocation constraints when total desires exceed available resources
export const feasibleComposeFrom = derived(
	[userDesiredComposeFrom, userNetworkCapacitiesWithShares],
	([$userDesiredComposeFrom, $userNetworkCapacitiesWithShares]) => {
		console.log(
			'[FEASIBLE-COMPOSE-FROM] Calculating feasible compose-from with absolute units and resource allocation...'
		);

		// Step 1: Individual share-based feasibility check
		const shareConstrainedDesires: Record<string, Record<string, number>> = {};

		Object.entries($userDesiredComposeFrom).forEach(([ourCapacityId, ourDesires]) => {
			const constrainedDesires: Record<string, number> = {};

			Object.entries(ourDesires).forEach(([theirCapacityId, desiredAbsoluteUnits]) => {
				const networkCapacity = $userNetworkCapacitiesWithShares[theirCapacityId];

				if (!networkCapacity) {
					console.warn(`[FEASIBLE-COMPOSE-FROM] No network capacity found for: ${theirCapacityId}`);
					return;
				}

				// Get our available units from this capacity (computed_quantity is already share-adjusted)
				const ourAvailableUnits = (networkCapacity as any).computed_quantity || 0;
				const shareConstrainedUnits = Math.min(desiredAbsoluteUnits, ourAvailableUnits);

				if (shareConstrainedUnits > 0) {
					constrainedDesires[theirCapacityId] = shareConstrainedUnits;

					if (shareConstrainedUnits < desiredAbsoluteUnits) {
						console.log(
							`[FEASIBLE-COMPOSE-FROM] Share-constrained ${ourCapacityId} → ${theirCapacityId}: desired ${desiredAbsoluteUnits} → ${shareConstrainedUnits} units (limited by available ${ourAvailableUnits} units)`
						);
					}
				}
			});

			if (Object.keys(constrainedDesires).length > 0) {
				shareConstrainedDesires[ourCapacityId] = constrainedDesires;
			}
		});

		// Step 2: Resource allocation constraints - group by source capacity
		const sourceCapacityDemands: Record<
			string,
			{
				totalDemand: number;
				available: number;
				consumers: Array<{ ourCapacityId: string; desiredUnits: number }>;
			}
		> = {};

		// Group all desires by source capacity
		Object.entries(shareConstrainedDesires).forEach(([ourCapacityId, ourDesires]) => {
			Object.entries(ourDesires).forEach(([theirCapacityId, desiredUnits]) => {
				if (!sourceCapacityDemands[theirCapacityId]) {
					const networkCapacity = $userNetworkCapacitiesWithShares[theirCapacityId];
					const availableUnits = (networkCapacity as any)?.computed_quantity || 0;

					sourceCapacityDemands[theirCapacityId] = {
						totalDemand: 0,
						available: availableUnits,
						consumers: []
					};
				}

				sourceCapacityDemands[theirCapacityId].totalDemand += desiredUnits;
				sourceCapacityDemands[theirCapacityId].consumers.push({
					ourCapacityId,
					desiredUnits
				});
			});
		});

		// Step 3: Apply proportional scaling when total demand exceeds available resources
		const finalFeasible: Record<string, Record<string, number>> = {};

		Object.entries(sourceCapacityDemands).forEach(([theirCapacityId, demandInfo]) => {
			const { totalDemand, available, consumers } = demandInfo;

			let scalingFactor = 1.0;
			if (totalDemand > available) {
				scalingFactor = available / totalDemand;
				console.log(
					`[FEASIBLE-COMPOSE-FROM] Resource allocation scaling for ${theirCapacityId}: ${totalDemand} demanded > ${available} available, scaling by ${scalingFactor.toFixed(3)}`
				);
			}

			// Apply scaling to each consumer
			consumers.forEach(({ ourCapacityId, desiredUnits }) => {
				const finalFeasibleUnits = desiredUnits * scalingFactor;

				if (!finalFeasible[ourCapacityId]) {
					finalFeasible[ourCapacityId] = {};
				}
				finalFeasible[ourCapacityId][theirCapacityId] = finalFeasibleUnits;

				if (scalingFactor < 1.0) {
					console.log(
						`[FEASIBLE-COMPOSE-FROM] Resource-allocated ${ourCapacityId} → ${theirCapacityId}: ${desiredUnits} → ${finalFeasibleUnits.toFixed(1)} units`
					);
				}
			});
		});

		console.log(
			`[FEASIBLE-COMPOSE-FROM] Generated resource-allocated feasible compositions for ${Object.keys(finalFeasible).length} capacities`
		);
		return finalFeasible;
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
	console.log(
		`[MUTUAL-RECOGNITION] ${new Date().toISOString()} Recalculating from cache:`,
		$recognitionCache
	);

	const mutualValues: Record<string, number> = {};

	for (const [contributorId, recognition] of Object.entries($recognitionCache)) {
		// Mutual recognition is the minimum of our share and their share
		const mutualValue = Math.min(recognition.ourShare, recognition.theirShare);
		mutualValues[contributorId] = mutualValue;

		console.log(
			`[MUTUAL-RECOGNITION] ${contributorId}: our=${recognition.ourShare.toFixed(4)}, their=${recognition.theirShare.toFixed(4)}, mutual=${mutualValue.toFixed(4)}`
		);
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
	console.log(
		`[PROVIDER-SHARES] ${new Date().toISOString()} Recalculating from mutual recognition:`,
		$mutualRecognition
	);

	// If empty, return empty object
	if (Object.keys($mutualRecognition).length === 0) {
		console.log('[PROVIDER-SHARES] No mutual recognition data, returning empty');
		return {};
	}

	// Use the normalizeShareMap function from protocol.ts
	const normalized = normalizeShareMap($mutualRecognition);
	console.log('[PROVIDER-SHARES] Normalized shares:', normalized);
	return normalized;
});

// Derived store for subtree contributor mapping
export const subtreeContributorMap = derived([userTree, nodesMap], ([$userTree, $nodesMap]) => {
	if (!$userTree) {
		console.log('[SUBTREE-MAP] No tree available, returning empty map');
		return {};
	}

	console.log('[SUBTREE-MAP] Calculating subtree contributor map...');

	// Get the subtree map directly - it already returns the format we need
	const filterMap = getSubtreeContributorMap($userTree, $nodesMap);

	console.log('[SUBTREE-MAP] Generated filter map for', Object.keys(filterMap).length, 'subtrees');
	return filterMap;
});

// Derived store for capacity shares - maps capacity IDs to their filtered share maps
export const capacityShares = derived(
	[userCapacities, providerShares, nodesMap, subtreeContributorMap],
	([$userCapacities, $providerShares, $nodesMap, $subtreeContributorMap]) => {
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
					node: $nodesMap,
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

// FEASIBLE COMPOSE-INTO: Constrains raw desires by recipient share and our capacity limits
// Step 1: Convert absolute unit desires to recipient-share-constrained feasible units
// Step 2: Apply resource allocation constraints when total desires exceed our capacity
export const feasibleComposeInto = derived(
	[userDesiredComposeInto, contributorCapacityShares, networkCapacities, userCapacities],
	([$userDesiredComposeInto, $contributorCapacityShares, $networkCapacities, $userCapacities]) => {
		console.log(
			'[FEASIBLE-COMPOSE-INTO] Calculating feasible compose-into with absolute units and resource allocation...'
		);

		// Step 1: Individual recipient-share-based feasibility check
		const shareConstrainedDesires: Record<string, Record<string, number>> = {};

		Object.entries($userDesiredComposeInto).forEach(([ourCapacityId, ourDesires]) => {
			const constrainedDesires: Record<string, number> = {};

			Object.entries(ourDesires).forEach(([theirCapacityId, desiredAbsoluteUnits]) => {
				// Find who owns the target capacity
				const providerId = Object.keys($networkCapacities).find(
					(id) => $networkCapacities[id] && $networkCapacities[id][theirCapacityId]
				);

				if (!providerId) {
					console.warn(
						`[FEASIBLE-COMPOSE-INTO] No provider found for capacity: ${theirCapacityId}`
					);
					return;
				}

				// Get our capacity info to calculate their share-based limit
				const ourCapacity = $userCapacities?.[ourCapacityId];
				if (!ourCapacity) {
					console.warn(`[FEASIBLE-COMPOSE-INTO] Our capacity not found: ${ourCapacityId}`);
					return;
				}

				// Check how much share they have in our capacity (this constrains how much they can receive)
				const theirShareInOurCapacity =
					$contributorCapacityShares[providerId]?.[ourCapacityId] || 0;
				const ourCapacityQuantity = ourCapacity.quantity || 0;
				const maxUnitsTheyCanReceive = ourCapacityQuantity * theirShareInOurCapacity;

				const shareConstrainedUnits = Math.min(desiredAbsoluteUnits, maxUnitsTheyCanReceive);

				if (shareConstrainedUnits > 0) {
					constrainedDesires[theirCapacityId] = shareConstrainedUnits;

					if (shareConstrainedUnits < desiredAbsoluteUnits) {
						console.log(
							`[FEASIBLE-COMPOSE-INTO] Share-constrained ${ourCapacityId} → ${theirCapacityId}: desired ${desiredAbsoluteUnits} → ${shareConstrainedUnits} units (limited by ${(theirShareInOurCapacity * 100).toFixed(1)}% recipient share = ${maxUnitsTheyCanReceive} max units)`
						);
					}
				}
			});

			if (Object.keys(constrainedDesires).length > 0) {
				shareConstrainedDesires[ourCapacityId] = constrainedDesires;
			}
		});

		// Step 2: Resource allocation constraints - group by our source capacity
		const ourCapacityDemands: Record<
			string,
			{
				totalDemand: number;
				available: number;
				recipients: Array<{ theirCapacityId: string; desiredUnits: number }>;
			}
		> = {};

		// Group all desires by our source capacity
		Object.entries(shareConstrainedDesires).forEach(([ourCapacityId, ourDesires]) => {
			Object.entries(ourDesires).forEach(([theirCapacityId, desiredUnits]) => {
				if (!ourCapacityDemands[ourCapacityId]) {
					const ourCapacity = $userCapacities?.[ourCapacityId];
					const availableUnits = ourCapacity?.quantity || 0;

					ourCapacityDemands[ourCapacityId] = {
						totalDemand: 0,
						available: availableUnits,
						recipients: []
					};
				}

				ourCapacityDemands[ourCapacityId].totalDemand += desiredUnits;
				ourCapacityDemands[ourCapacityId].recipients.push({
					theirCapacityId,
					desiredUnits
				});
			});
		});

		// Step 3: Apply proportional scaling when total demand exceeds our capacity
		const finalFeasible: Record<string, Record<string, number>> = {};

		Object.entries(ourCapacityDemands).forEach(([ourCapacityId, demandInfo]) => {
			const { totalDemand, available, recipients } = demandInfo;

			let scalingFactor = 1.0;
			if (totalDemand > available) {
				scalingFactor = available / totalDemand;
				console.log(
					`[FEASIBLE-COMPOSE-INTO] Resource allocation scaling for our ${ourCapacityId}: ${totalDemand} demanded > ${available} available, scaling by ${scalingFactor.toFixed(3)}`
				);
			}

			// Apply scaling to each recipient
			recipients.forEach(({ theirCapacityId, desiredUnits }) => {
				const finalFeasibleUnits = desiredUnits * scalingFactor;

				if (!finalFeasible[ourCapacityId]) {
					finalFeasible[ourCapacityId] = {};
				}
				finalFeasible[ourCapacityId][theirCapacityId] = finalFeasibleUnits;

				if (scalingFactor < 1.0) {
					console.log(
						`[FEASIBLE-COMPOSE-INTO] Resource-allocated ${ourCapacityId} → ${theirCapacityId}: ${desiredUnits} → ${finalFeasibleUnits.toFixed(1)} units`
					);
				}
			});
		});

		console.log(
			`[FEASIBLE-COMPOSE-INTO] Generated resource-allocated feasible compositions for ${Object.keys(finalFeasible).length} capacities`
		);
		return finalFeasible;
	}
);

// Helper function to calculate how well two desired amounts align (absolute units)
function calculateDesireAlignment(ourDesire: number, theirDesire: number): number {
	if (ourDesire === 0 || theirDesire === 0) return 0;

	// Calculate the ratio between desires - works the same for absolute units
	const ratio = Math.min(ourDesire, theirDesire) / Math.max(ourDesire, theirDesire);

	// Return a score between 0 and 1 where 1 = perfect alignment
	return ratio;
}

// STEP 1: MUTUAL DESIRE (Aspirational - based on raw desires)
// TYPE 1: Mutual Desire for Our Capacities
// Both we and they want to enhance our capacity using their capacity (before constraints)
export const mutualDesireOurCapacities = derived(
	[userDesiredComposeFrom, networkDesiredComposeInto, userNetworkCapacitiesWithShares],
	([$userDesiredComposeFrom, $networkDesiredComposeInto, $userNetworkCapacitiesWithShares]) => {
		console.log('[MUTUAL-DESIRE-OURS] Calculating mutual desires for our capacities...');

		if (!$userDesiredComposeFrom || !$networkDesiredComposeInto) {
			console.log('[MUTUAL-DESIRE-OURS] No composition data available');
			return {};
		}

		const mutualDesires: Record<
			string,
			{
				ourCapacityId: string;
				theirCapacityId: string;
				ourDesiredAmount: number;
				theirDesiredAmount: number;
				providerId: string;
				desireViability: number;
			}
		> = {};

		Object.entries($userDesiredComposeFrom).forEach(([ourCapacityId, ourDesires]) => {
			Object.entries(ourDesires).forEach(([theirCapacityId, ourDesiredAmount]) => {
				const networkCapacity = $userNetworkCapacitiesWithShares[theirCapacityId];
				const providerId = (networkCapacity as any)?.provider_id;

				if (!providerId) {
					console.warn(`[MUTUAL-DESIRE-OURS] No provider found for capacity: ${theirCapacityId}`);
					return;
				}

				// Check if they also want to compose their capacity into our capacity
				const theirDesireForOurCapacity =
					$networkDesiredComposeInto[providerId]?.[theirCapacityId]?.[ourCapacityId];
				if (!theirDesireForOurCapacity) return;

				const desireViability = calculateDesireAlignment(
					ourDesiredAmount,
					theirDesireForOurCapacity
				);

				const compositionKey = `${ourCapacityId}:${theirCapacityId}:${providerId}`;
				mutualDesires[compositionKey] = {
					ourCapacityId,
					theirCapacityId,
					ourDesiredAmount,
					theirDesiredAmount: theirDesireForOurCapacity,
					providerId,
					desireViability
				};

				console.log(
					`[MUTUAL-DESIRE-OURS] Found mutual desire: ${compositionKey} (desire viability: ${desireViability.toFixed(2)})`
				);
			});
		});

		console.log(
			`[MUTUAL-DESIRE-OURS] Found ${Object.keys(mutualDesires).length} mutual desires for our capacities`
		);
		return mutualDesires;
	}
);

// TYPE 2: Mutual Desire for Their Capacities
// Both we and they want to enhance their capacity using our capacity (before constraints)
export const mutualDesireTheirCapacities = derived(
	[userDesiredComposeInto, networkDesiredComposeFrom, networkCapacities],
	([$userDesiredComposeInto, $networkDesiredComposeFrom, $networkCapacities]) => {
		console.log('[MUTUAL-DESIRE-THEIRS] Calculating mutual desires for their capacities...');

		if (!$userDesiredComposeInto || !$networkDesiredComposeFrom) {
			console.log('[MUTUAL-DESIRE-THEIRS] No composition data available');
			return {};
		}

		const mutualDesires: Record<
			string,
			{
				ourCapacityId: string;
				theirCapacityId: string;
				ourDesiredAmount: number;
				theirDesiredAmount: number;
				providerId: string;
				desireViability: number;
			}
		> = {};

		Object.entries($userDesiredComposeInto).forEach(([ourCapacityId, ourDesires]) => {
			Object.entries(ourDesires).forEach(([theirCapacityId, ourDesiredAmount]) => {
				// Find the provider who owns this capacity
				const providerId = Object.keys($networkCapacities).find(
					(id) => $networkCapacities[id] && $networkCapacities[id][theirCapacityId]
				);

				if (!providerId) {
					console.warn(`[MUTUAL-DESIRE-THEIRS] No provider found for capacity: ${theirCapacityId}`);
					return;
				}

				// Check if they also want to compose our capacity into their capacity
				const theirDesireForOurCapacity =
					$networkDesiredComposeFrom[providerId]?.[theirCapacityId]?.[ourCapacityId];
				if (!theirDesireForOurCapacity) return;

				const desireViability = calculateDesireAlignment(
					ourDesiredAmount,
					theirDesireForOurCapacity
				);

				const compositionKey = `${ourCapacityId}:${theirCapacityId}:${providerId}`;
				mutualDesires[compositionKey] = {
					ourCapacityId,
					theirCapacityId,
					ourDesiredAmount,
					theirDesiredAmount: theirDesireForOurCapacity,
					providerId,
					desireViability
				};

				console.log(
					`[MUTUAL-DESIRE-THEIRS] Found mutual desire: ${compositionKey} (desire viability: ${desireViability.toFixed(2)})`
				);
			});
		});

		console.log(
			`[MUTUAL-DESIRE-THEIRS] Found ${Object.keys(mutualDesires).length} mutual desires for their capacities`
		);
		return mutualDesires;
	}
);

// STEP 2: MUTUAL FEASIBLE (Realistic - constrained by shares)
// TYPE 1: Mutual Feasible Enhancement of Our Capacities
// Takes mutual desires and constrains them by actual share access
export const mutualFeasibleOurCapacities = derived(
	[mutualDesireOurCapacities, feasibleComposeFrom],
	([$mutualDesireOurCapacities, $feasibleComposeFrom]) => {
		console.log(
			'[MUTUAL-FEASIBLE-OURS] Calculating feasible mutual enhancements of our capacities...'
		);

		const mutualFeasible: Record<
			string,
			{
				ourCapacityId: string;
				theirCapacityId: string;
				ourDesiredAmount: number;
				theirDesiredAmount: number;
				ourFeasibleAmount: number;
				providerId: string;
				desireViability: number;
				feasibleViability: number;
				constraintRatio: number;
			}
		> = {};

		Object.entries($mutualDesireOurCapacities).forEach(([compositionKey, mutualDesire]) => {
			const { ourCapacityId, theirCapacityId } = mutualDesire;

			// Get our feasible amount (constrained by our shares)
			const ourFeasibleAmount = $feasibleComposeFrom[ourCapacityId]?.[theirCapacityId] || 0;

			if (ourFeasibleAmount > 0) {
				const feasibleViability = calculateDesireAlignment(
					ourFeasibleAmount,
					mutualDesire.theirDesiredAmount
				);

				const constraintRatio = ourFeasibleAmount / mutualDesire.ourDesiredAmount;

				mutualFeasible[compositionKey] = {
					...mutualDesire,
					ourFeasibleAmount,
					feasibleViability,
					constraintRatio
				};

				console.log(
					`[MUTUAL-FEASIBLE-OURS] ${compositionKey}: desired ${mutualDesire.ourDesiredAmount.toFixed(3)} → feasible ${ourFeasibleAmount.toFixed(3)} (${(constraintRatio * 100).toFixed(1)}% achievable)`
				);
			}
		});
		
		console.log(
			`[MUTUAL-FEASIBLE-OURS] Found ${Object.keys(mutualFeasible).length} feasible mutual enhancements of our capacities`
		);
		return mutualFeasible;
	}
);

// TYPE 2: Mutual Feasible Enhancement of Their Capacities
// Takes mutual desires and constrains them by recipient share access
export const mutualFeasibleTheirCapacities = derived(
	[mutualDesireTheirCapacities, feasibleComposeInto],
	([$mutualDesireTheirCapacities, $feasibleComposeInto]) => {
		console.log(
			'[MUTUAL-FEASIBLE-THEIRS] Calculating feasible mutual enhancements of their capacities...'
		);

		const mutualFeasible: Record<
			string,
			{
				ourCapacityId: string;
				theirCapacityId: string;
				ourDesiredAmount: number;
				theirDesiredAmount: number;
				ourFeasibleAmount: number;
				providerId: string;
				desireViability: number;
				feasibleViability: number;
				constraintRatio: number;
			}
		> = {};

		Object.entries($mutualDesireTheirCapacities).forEach(([compositionKey, mutualDesire]) => {
			const { ourCapacityId, theirCapacityId } = mutualDesire;

			// Get our feasible amount (constrained by their shares in our capacity)
			const ourFeasibleAmount = $feasibleComposeInto[ourCapacityId]?.[theirCapacityId] || 0;

			if (ourFeasibleAmount > 0) {
				const feasibleViability = calculateDesireAlignment(
					ourFeasibleAmount,
					mutualDesire.theirDesiredAmount
				);

				const constraintRatio = ourFeasibleAmount / mutualDesire.ourDesiredAmount;

				mutualFeasible[compositionKey] = {
					...mutualDesire,
					ourFeasibleAmount,
					feasibleViability,
					constraintRatio
				};

				console.log(
					`[MUTUAL-FEASIBLE-THEIRS] ${compositionKey}: desired ${mutualDesire.ourDesiredAmount.toFixed(3)} → feasible ${ourFeasibleAmount.toFixed(3)} (${(constraintRatio * 100).toFixed(1)}% achievable)`
				);
			}
		});

		console.log(
			`[MUTUAL-FEASIBLE-THEIRS] Found ${Object.keys(mutualFeasible).length} feasible mutual enhancements of their capacities`
		);
		return mutualFeasible;
	}
);

// LEGACY EXPORTS (for backward compatibility)
export const mutualEnhanceOurCapacities = mutualFeasibleOurCapacities;
export const mutualEnhanceTheirCapacities = mutualFeasibleTheirCapacities;

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
export const subtreeOptions = derived([userTree, nodesMap], ([$userTree, $nodesMap]) => {
	if (!$userTree) return [];

	const subtreeMap = getSubtreeContributorMap($userTree, $nodesMap);

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
export const isLoadingRecognitionCache = writable(false);

export const isRecalculatingCapacities = writable(false);
export const isRecalculatingTree = writable(false);

// Additional state
export const publicTemplates: Writable<Record<string, any>> = writable({});
