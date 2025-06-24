import { get } from 'svelte/store';
import {
	sharesOfGeneralFulfillmentMap,
	getAllContributorsFromTree,
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

/**
 * Compare two ShareMap objects for equality
 */
function sogfEqual(a: ShareMap, b: ShareMap): boolean {
	const keysA = Object.keys(a);
	const keysB = Object.keys(b);

	// Different number of keys
	if (keysA.length !== keysB.length) {
		return false;
	}

	// Check each key-value pair
	for (const key of keysA) {
		if (!(key in b) || Math.abs(a[key] - b[key]) > 1e-10) {
			return false;
		}
	}

	return true;
}

/**
 * Compare two arrays of strings for equality
 */
function arrayEqual(a: string[], b: string[]): boolean {
	if (a.length !== b.length) {
		return false;
	}

	// Sort both arrays to compare regardless of order
	const sortedA = [...a].sort();
	const sortedB = [...b].sort();

	for (let i = 0; i < sortedA.length; i++) {
		if (sortedA[i] !== sortedB[i]) {
			return false;
		}
	}

	return true;
}

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

			// Check if contributors have changed
			const currentContributors = get(contributors);
			const contributorsChanged = !arrayEqual(currentContributors, allContributorIds);

			if (contributorsChanged) {
				console.log('[RECALC] Contributors have changed, updating store');
				contributors.set(allContributorIds);
				console.log('[RECALC] Found', allContributorIds.length, 'contributors');
			} else {
				console.log('[RECALC] Contributors unchanged, skipping update');
			}

			// Check if all known contributors have changed
			const currentKnownContributors = get(allKnownContributors);
			const updatedKnownContributors = [
				...new Set([...currentKnownContributors, ...allContributorIds])
			];
			const knownContributorsChanged = !arrayEqual(
				currentKnownContributors,
				updatedKnownContributors
			);

			if (knownContributorsChanged) {
				console.log('[RECALC] All known contributors have changed, updating store');
				allKnownContributors.set(updatedKnownContributors);
				console.log('[RECALC] Total known contributors:', updatedKnownContributors.length);
			} else {
				console.log('[RECALC] All known contributors unchanged, skipping update');
			}
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

			// Compare with current SOGF to see if it has changed
			const currentSogf = get(userSogf);
			const hasChanged = !currentSogf || !sogfEqual(currentSogf, sogf);

			if (hasChanged) {
				console.log('[RECALC] SOGF has changed, updating stores and recognition cache');
				userSogf.set(sogf);

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

				console.log(
					'[RECALC] SOGF calculation complete for',
					allKnownContributorsList.length,
					'known contributors'
				);
			} else {
				console.log('[RECALC] SOGF unchanged, skipping update');
			}
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
