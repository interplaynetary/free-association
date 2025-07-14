import { get } from 'svelte/store';
import {
	sharesOfGeneralFulfillmentMap,
	getAllContributorsFromTree,
	getDescendants
} from '$lib/protocol';
import type { RootNode, Node, NonRootNode, ShareMap, RecognitionCache } from '$lib/schema';
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
import { resolveToPublicKey } from './users.svelte';

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

		// Step 0: Collect all contributors in the tree (with resolution to public keys)
		try {
			console.log('[RECALC] Collecting contributors from tree...');

			// Debug: Check what's in the raw tree
			const allNodes = [tree, ...getDescendants(tree)];
			const rawContributorIds = new Set<string>();
			allNodes.forEach((node) => {
				if (node.type === 'NonRootNode') {
					(node as NonRootNode).contributor_ids.forEach((id: string) => rawContributorIds.add(id));
				}
			});
			console.log('[RECALC-DEBUG] Raw contributor IDs from tree:', [...rawContributorIds]);

			const allContributorIds = getAllContributorsFromTree(tree, resolveToPublicKey);
			console.log('[RECALC-DEBUG] Resolved contributor IDs:', allContributorIds);

			// Check if contributors have changed
			const currentContributors = get(contributors);
			const contributorsChanged = !arrayEqual(currentContributors, allContributorIds);

			console.log('[RECALC-DEBUG] Current contributors store:', currentContributors);
			console.log('[RECALC-DEBUG] Contributors changed:', contributorsChanged);

			if (contributorsChanged) {
				console.log('[RECALC] Contributors have changed, updating store');
				contributors.set(allContributorIds);
				console.log('[RECALC] Found', allContributorIds.length, 'resolved contributors');
			} else {
				console.log('[RECALC] Contributors unchanged, skipping update');
			}

			// Check if all known contributors have changed
			const currentKnownContributors = get(allKnownContributors);

			// Resolve all current known contributors to public keys and combine with new ones
			const resolvedCurrentKnownContributors = currentKnownContributors.map(
				(id) => resolveToPublicKey(id) || id
			);
			const updatedKnownContributors = [
				...new Set([...resolvedCurrentKnownContributors, ...allContributorIds])
			];
			const knownContributorsChanged = !arrayEqual(
				currentKnownContributors,
				updatedKnownContributors
			);

			console.log('[RECALC-DEBUG] Current known contributors:', currentKnownContributors);
			console.log('[RECALC-DEBUG] Updated known contributors:', updatedKnownContributors);
			console.log('[RECALC-DEBUG] Known contributors changed:', knownContributorsChanged);

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

			// Calculate SOGF using all known contributors with unified identifiers
			const sogf = sharesOfGeneralFulfillmentMap(
				tree,
				nodeMap,
				allKnownContributorsList,
				resolveToPublicKey
			);

			// Compare with current SOGF to see if it has changed
			const currentSogf = get(userSogf);
			const hasChanged = !currentSogf || !sogfEqual(currentSogf, sogf);

			if (hasChanged) {
				console.log('[RECALC] SOGF has changed, updating stores and recognition cache');
				userSogf.set(sogf);

				console.log(
					'[RECALC-DEBUG] Current recognition cache before update:',
					get(recognitionCache)
				);

				// Update recognition cache with our share values (using unified public keys)
				allKnownContributorsList.forEach((contributorId) => {
					const ourShare = sogf[contributorId] || 0;
					const existing = get(recognitionCache)[contributorId];
					const theirShare = existing?.theirShare || 0;

					console.log(
						`[RECALC] Updating recognition for ${contributorId}: ourShare=${ourShare.toFixed(4)}, theirShare=${theirShare.toFixed(4)}`
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
				});

				console.log('[RECALC-DEBUG] Recognition cache after update:', get(recognitionCache));

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
