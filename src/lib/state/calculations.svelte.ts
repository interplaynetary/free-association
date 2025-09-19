import { get } from 'svelte/store';
import {
	sharesOfGeneralFulfillmentMap,
	getAllContributorsFromTree,
	getDescendants
} from '$lib/protocol';
import type { NonRootNode, ShareMapData } from '$lib/schema';
import {
	userTree,
	userSogf,
	nodesMap,
	contributors,
	allKnownContributors,
	recognitionCache,
	isRecalculatingTree
} from './core.svelte';
import { resolveToPublicKey } from './users.svelte';

/**
 * Compare two ShareMapData objects for equality
 */
function sogfEqual(a: ShareMapData, b: ShareMapData): boolean {
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
			// This already returns public keys due to the enhanced protocol filtering
			const sogf = sharesOfGeneralFulfillmentMap(
				tree,
				nodeMap,
				allKnownContributorsList,
				resolveToPublicKey
			);

			// Compare with current SOGF to see if it has changed
			const currentSogf = get(userSogf);
			const hasChanged = !currentSogf || !sogfEqual(currentSogf, sogf);

			console.log('[RECALC-DEBUG] SOGF change check:', {
				currentSogfExists: !!currentSogf,
				currentSogf: currentSogf,
				newSogf: sogf,
				sogfEqual: currentSogf ? sogfEqual(currentSogf, sogf) : 'N/A',
				hasChanged: hasChanged
			});

			if (hasChanged) {
				console.log('[RECALC] SOGF has changed, updating stores and recognition cache');
				userSogf.set(sogf);

				console.log(
					'[RECALC-DEBUG] Current recognition cache before update:',
					get(recognitionCache)
				);

				// Add debugging to check if we're getting network SOGF data
				const currentCache = get(recognitionCache);
				console.log('[RECALC-DEBUG] Recognition cache entries:', Object.keys(currentCache).length);
				Object.entries(currentCache).forEach(([id, entry]) => {
					console.log(
						`[RECALC-DEBUG] Cache entry ${id}: our=${entry.ourShare.toFixed(4)}, their=${entry.theirShare.toFixed(4)}, timestamp=${entry.timestamp}`
					);
				});

				// Update recognition cache with our share values
				// Handle both public keys and contact IDs in the cache
				console.log('[RECALC-DEBUG] SOGF result keys:', Object.keys(sogf));
				console.log('[RECALC-DEBUG] All known contributors:', allKnownContributorsList);

				allKnownContributorsList.forEach((contributorId) => {
					// Try to resolve to public key, but use original ID if resolution fails
					const resolvedContributorId = resolveToPublicKey(contributorId) || contributorId;

					// The SOGF map now contains the actual keys used in calculation
					// (either resolved public keys or preserved contact IDs)
					const ourShare = sogf[resolvedContributorId] || 0;

					// Get existing cache entry - check both resolved and original IDs
					const existing =
						get(recognitionCache)[resolvedContributorId] || get(recognitionCache)[contributorId];
					const theirShare = existing?.theirShare || 0;

					console.log(`[RECALC-DEBUG] Processing contributor:`, {
						originalId: contributorId,
						resolvedId: resolvedContributorId,
						sogfKeys: Object.keys(sogf),
						ourShareFromSOGF: ourShare,
						theirShareFromCache: theirShare,
						existingCacheEntry: existing
					});

					console.log(
						`[RECALC] Updating recognition for ${contributorId} (resolved: ${resolvedContributorId}): ourShare=${ourShare.toFixed(4)}, theirShare=${theirShare.toFixed(4)}`
					);

					recognitionCache.update((cache) => {
						const oldEntry = cache[resolvedContributorId];

						// Store using the resolved ID (public key if available, contact ID if not)
						cache[resolvedContributorId] = {
							ourShare,
							theirShare,
							timestamp: Date.now()
						};

						// Clean up any duplicate entry under the original ID if different
						if (contributorId !== resolvedContributorId && cache[contributorId]) {
							console.log(`[RECALC] Cleaning up duplicate cache entry for ${contributorId}`);
							delete cache[contributorId];
						}

						console.log(`[MUTUAL] Cache entry for ${resolvedContributorId}:`, {
							old: oldEntry,
							new: cache[resolvedContributorId]
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
				console.log('[RECALC] SOGF unchanged, but still updating recognition cache entries');
			}

			// 🚨 CRITICAL FIX: Always update recognition cache entries, even if SOGF unchanged
			// This ensures mutual recognition works offline by initializing cache entries immediately
			console.log('[RECALC] Updating recognition cache with current SOGF values...');
			const currentCache = get(recognitionCache);

			allKnownContributorsList.forEach((contributorId) => {
				// Try to resolve to public key, but use original ID if resolution fails
				const resolvedContributorId = resolveToPublicKey(contributorId) || contributorId;

				// The SOGF map contains the actual keys used in calculation
				const ourShare = sogf[resolvedContributorId] || 0;

				// Get existing cache entry - check both resolved and original IDs
				const existing = currentCache[resolvedContributorId] || currentCache[contributorId];
				const theirShare = existing?.theirShare || 0;

				console.log(
					`[RECALC] Ensuring cache entry for ${contributorId} (resolved: ${resolvedContributorId}): ourShare=${ourShare.toFixed(4)}, theirShare=${theirShare.toFixed(4)}`
				);

				recognitionCache.update((cache) => {
					// Store using the resolved ID (public key if available, contact ID if not)
					cache[resolvedContributorId] = {
						ourShare,
						theirShare, // Preserve existing theirShare from network updates
						timestamp: Date.now()
					};

					// Clean up any duplicate entry under the original ID if different
					if (contributorId !== resolvedContributorId && cache[contributorId]) {
						console.log(`[RECALC] Cleaning up duplicate cache entry for ${contributorId}`);
						delete cache[contributorId];
					}

					return cache;
				});
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
