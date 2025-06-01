import { get } from 'svelte/store';
import {
	sharesOfGeneralFulfillmentMap,
	getAllContributorsFromTree,
	getReceiverShares
} from '$lib/protocol';
import type { RootNode, Node, ShareMap, RecognitionCache } from '$lib/schema';
import {
	userTree,
	userSogf,
	nodesMap,
	contributors,
	allKnownContributors,
	recognitionCache,
	isRecalculatingTree,
	capacityShares
} from './core.svelte';
import { persistSogf } from './gun.svelte';

/**
 * Core tree-based calculations that update our recognition of others
 */
export function recalculateFromTree() {
	console.log('[RECALC] Starting tree recalculation...');

	// Get current state values
	const tree = get(userTree);

	// Validate tree existence
	if (!tree) {
		console.error('[RECALC] Tree is missing, cannot proceed with recalculation');
		return;
	}

	console.log('[RECALC] Processing tree with', tree.children.length, 'children');

	try {
		// Set recalculating flag
		isRecalculatingTree.set(true);

		// Step 0: Collect all contributors in the tree
		try {
			console.log('[RECALC] Collecting contributors from tree...');
			const allContributorIds = getAllContributorsFromTree(tree);

			// Update contributors store
			contributors.set(allContributorIds);
			console.log('[RECALC] Found', allContributorIds.length, 'contributors');

			// Update all known contributors (union of current and previous)
			const currentKnownContributors = get(allKnownContributors);
			const updatedKnownContributors = [
				...new Set([...currentKnownContributors, ...allContributorIds])
			];
			allKnownContributors.set(updatedKnownContributors);
			console.log('[RECALC] Total known contributors:', updatedKnownContributors.length);
		} catch (error) {
			console.error('[RECALC] Error collecting contributors:', error);
		}

		// Step 1: Calculate SOGF and update recognition cache
		try {
			console.log('[RECALC] Calculating SOGF...');
			const allKnownContributorsList = get(allKnownContributors);
			const nodeMap = get(nodesMap);

			// Calculate SOGF using all known contributors
			const sogf = sharesOfGeneralFulfillmentMap(tree, nodeMap, allKnownContributorsList);
			userSogf.set(sogf);
			persistSogf();
			console.log(
				'[RECALC] SOGF calculation complete for',
				allKnownContributorsList.length,
				'known contributors'
			);

			// Update recognition cache with our share values
			allKnownContributorsList.forEach((contributorId) => {
				const ourShare = sogf[contributorId] || 0;
				const existing = get(recognitionCache)[contributorId];
				const theirShare = existing?.theirShare || 0;

				console.log(
					`[RECALC] Updating recognition for ${contributorId}: ourShare=${ourShare.toFixed(4)}, theirShare=${theirShare.toFixed(4)}`
				);
				updateRecognitionCache(contributorId, ourShare, theirShare);
			});
		} catch (error) {
			console.error('[RECALC] Error calculating SOGF:', error);
		}

		console.log('[RECALC] Tree recalculation process complete');
	} finally {
		// Always reset recalculating flag
		isRecalculatingTree.set(false);
	}
}

/**
 * Update the recognition cache with a value from another user
 */
export function updateRecognitionCache(
	contributorId: string,
	ourShare: number,
	theirShare: number
) {
	console.log(
		`[MUTUAL] Updating cache for ${contributorId}: our=${ourShare.toFixed(4)}, their=${theirShare.toFixed(4)}`
	);

	recognitionCache.update((cache) => {
		const oldEntry = cache[contributorId];
		cache[contributorId] = {
			ourShare,
			theirShare,
			timestamp: Date.now()
		};

		console.log(`[MUTUAL] Cache entry for ${contributorId}:`, {
			old: oldEntry,
			new: cache[contributorId]
		});

		return cache;
	});

	// Log the entire cache after update
	const fullCache = get(recognitionCache);
	console.log('[MUTUAL] Full recognition cache after update:', fullCache);
}
