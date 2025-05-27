import { writable, derived, get } from 'svelte/store';
import type { Writable, Readable } from 'svelte/store';
import {
	sharesOfGeneralFulfillmentMap,
	normalizeShareMap,
	getAllContributorsFromTree,
	getSubtreeContributorMap
} from '$lib/protocol';
import type { RootNode, CapacitiesCollection, Node, ShareMap, RecognitionCache } from '$lib/schema';

// Core reactive state - these form the main reactive chain
export const userTree: Writable<RootNode | null> = writable(null);
export const userSogf: Writable<ShareMap | null> = writable(null);
export const userCapacities: Writable<CapacitiesCollection | null> = writable(null);
export const nodesMap: Writable<Record<string, Node>> = writable({});

// Contributors state
export const contributors = writable<string[]>([]);
export const mutualContributors = writable<string[]>([]);

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

	// Get the full subtree map with names and contributors
	const fullMap = getSubtreeContributorMap($userTree, $nodesMap);

	// Convert to the format expected by JSON Logic filters
	// This creates a nested object: subtreeId -> contributorId -> true
	const filterMap: Record<string, Record<string, boolean>> = {};

	Object.entries(fullMap).forEach(([subtreeId, { contributors }]) => {
		filterMap[subtreeId] = {};
		contributors.forEach((contributorId) => {
			filterMap[subtreeId][contributorId] = true;
		});
	});

	console.log('[SUBTREE-MAP] Generated filter map for', Object.keys(filterMap).length, 'subtrees');
	return filterMap;
});

// Derived store that provides subtree options for UI components
export const subtreeOptions = derived([userTree, nodesMap], ([$userTree, $nodesMap]) => {
	if (!$userTree) return [];

	const fullMap = getSubtreeContributorMap($userTree, $nodesMap);

	return Object.entries(fullMap).map(([subtreeId, { name, contributors }]) => ({
		id: subtreeId,
		name,
		contributorCount: contributors.length,
		contributors
	}));
});

// Loading state flags
export const isLoadingCapacities = writable(false);
export const isLoadingTree = writable(false);
export const isRecalculatingCapacities = writable(false);
export const isRecalculatingTree = writable(false);

// Additional state
export const recipientSharesMap: Writable<Record<string, any>> = writable({});
export const publicTemplates: Writable<Record<string, any>> = writable({});
