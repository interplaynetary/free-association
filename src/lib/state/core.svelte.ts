import { writable, derived, get } from 'svelte/store';
import type { Writable } from 'svelte/store';
import {
	normalizeShareMap,
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
