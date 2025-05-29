import { get } from 'svelte/store';
import {
	sharesOfGeneralFulfillmentMap,
	getAllContributorsFromTree,
	applyCapacityFilter,
	updateCapacityShareQuantities,
	getReceiverShares
} from '$lib/protocol';
import {
	userTree,
	userSogf,
	userCapacities,
	nodesMap,
	contributors,
	mutualContributors,
	allKnownContributors,
	recognitionCache,
	mutualRecognition,
	providerShares,
	subtreeContributorMap,
	recipientSharesMap,
	isLoadingCapacities,
	isLoadingTree,
	isRecalculatingCapacities,
	isRecalculatingTree
} from './core.svelte';
import { persistTree, persistCapacities, persistRecipientShares, persist } from './gun.svelte';

/**
 * Sequential recalculation of all dependent data when tree changes
 */
export function recalculateFromTree() {
	console.log('[RECALC] Starting tree recalculation...');

	// Get current state values
	const tree = get(userTree);
	const capacities = get(userCapacities);
	const nodeMap = get(nodesMap);

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

		// Step 1: Calculate SOGF
		try {
			console.log('[RECALC] Calculating SOGF...');
			// Use ALL known contributors for SOGF calculation to ensure removed contributors get 0%
			const allKnownContributorsList = get(allKnownContributors);

			// Use all known contributors for SOGF calculation
			const sogf = sharesOfGeneralFulfillmentMap(tree, nodeMap, allKnownContributorsList);
			userSogf.set(sogf);
			console.log(
				'[RECALC] SOGF calculation complete for',
				allKnownContributorsList.length,
				'known contributors'
			);

			// NOTE: We no longer clean up recognition cache entries for removed contributors
			// This preserves theirShare data so that if a contributor is re-added,
			// we still have their historical recognition data and they can appear in providerShares

			// Update recognition cache with our share values for ALL known contributors
			allKnownContributorsList.forEach((contributorId) => {
				// Note: contributor might not exist in nodeMap since contributors are external user IDs

				// Get our share from SOGF
				const ourShare = sogf[contributorId] || 0;
				console.log(
					`[RECALC] Processing known contributor ${contributorId}: ourShare=${ourShare.toFixed(4)}`
				);

				// Always update the cache with current ourShare (even if 0)
				// Get existing entry to preserve theirShare
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

		// Step 3: Calculate recipient shares if capacities exist and not loading
		if (capacities && !get(isLoadingCapacities)) {
			try {
				console.log(
					'[RECALC] Calculating recipient shares for',
					Object.keys(capacities).length,
					'capacities...'
				);

				// Set recalculating capacities flag to prevent loops
				isRecalculatingCapacities.set(true);

				// Get current provider shares and subtree map
				const currentProviderShares = get(providerShares);
				const currentSubtreeMap = get(subtreeContributorMap);

				// Track if any changes were made
				let hasCapacityChanges = false;

				Object.values(capacities).forEach((capacity) => {
					try {
						if (capacity.owner_id === tree.id) {
							// Save state before calculation
							const sharesBeforeCalc = JSON.stringify(capacity.recipient_shares || {});

							// Apply capacity filter to the providerShares store value with subtree map
							const filteredShares = applyCapacityFilter(
								capacity,
								currentProviderShares,
								nodeMap,
								currentSubtreeMap
							);

							// Store the filtered shares in the capacity
							capacity.recipient_shares = filteredShares;

							// Update computed quantities
							updateCapacityShareQuantities(capacity);

							// Check if anything changed
							const sharesAfterCalc = JSON.stringify(capacity.recipient_shares || {});
							if (sharesBeforeCalc !== sharesAfterCalc) {
								hasCapacityChanges = true;
							}
						}
					} catch (capacityError) {
						console.error(
							'[RECALC] Error calculating shares for capacity:',
							capacity.id,
							capacityError
						);
					}
				});

				// Only update capacities if changes were made
				if (hasCapacityChanges) {
					console.log('[RECALC] Capacity changes detected, updating capacities...');
					userCapacities.set(capacities);
				} else {
					console.log('[RECALC] No capacity changes detected, skipping update');
				}

				// Reset capacities recalculation flag
				isRecalculatingCapacities.set(false);

				console.log('[RECALC] Recipient shares calculation complete');
			} catch (error) {
				console.error('[RECALC] Error calculating recipient shares:', error);
				isRecalculatingCapacities.set(false);
			}
		} else if (get(isLoadingCapacities)) {
			console.log(
				'[RECALC] Skipping recipient shares calculation because capacities are being loaded'
			);
		} else {
			console.log('[RECALC] No capacities available, skipping recipient shares calculation');
		}

		// Step 4: Persist all data
		try {
			console.log('[RECALC] Persisting all updated data...');
			persist();
			console.log('[RECALC] Persistence complete');
		} catch (error) {
			console.error('[RECALC] Error during persistence:', error);

			// Final fallback to ensure tree is saved
			try {
				console.log('[RECALC] Attempting direct tree persistence as fallback...');
				persistTree();
			} catch (treeError) {
				console.error('[RECALC] Critical error: Failed to persist tree:', treeError);
			}
		}

		console.log('[RECALC] Tree recalculation process complete');
	} finally {
		// Always reset recalculating flag
		isRecalculatingTree.set(false);
	}
}

/**
 * Recalculation when only capacities change
 */
export function recalculateFromCapacities() {
	const tree = get(userTree);
	const capacities = get(userCapacities);
	const nodeMap = get(nodesMap);
	const currentProviderShares = get(providerShares); // Get the store's providerShares
	const currentSubtreeMap = get(subtreeContributorMap); // Get the subtree contributor map

	if (!tree || !capacities) return;

	console.log('[CAPACITIES-RECALC] Starting capacities recalculation...');

	try {
		// Set the recalculation flag to prevent triggering another recalculation cycle
		isRecalculatingCapacities.set(true);

		// Track if any changes were made
		let hasChanges = false;

		// Only recalculate recipient shares
		Object.values(capacities).forEach((capacity) => {
			try {
				if (capacity.owner_id === tree.id) {
					// Save the state before calculation
					const sharesBeforeCalc = JSON.stringify(capacity.recipient_shares || {});

					// Apply capacity filter to the providerShares store value with subtree map
					const filteredShares = applyCapacityFilter(
						capacity,
						currentProviderShares,
						nodeMap,
						currentSubtreeMap
					);

					// Store the filtered shares in the capacity
					capacity.recipient_shares = filteredShares;

					// Update computed quantities
					updateCapacityShareQuantities(capacity);

					// Check if anything changed
					const sharesAfterCalc = JSON.stringify(capacity.recipient_shares || {});
					if (sharesBeforeCalc !== sharesAfterCalc) {
						hasChanges = true;
					}
				}
			} catch (error) {
				console.error(
					'[CAPACITIES-RECALC] Error calculating shares for capacity:',
					capacity.id,
					error
				);
			}
		});

		// Only update the store if changes were made
		if (hasChanges) {
			console.log('[CAPACITIES-RECALC] Changes detected, updating capacities...');
			userCapacities.set(capacities);
			console.log('[CAPACITIES-RECALC] Persisting changes...');
			persistCapacities();

			// Also update recipient shares
			persistRecipientShares();
		} else {
			console.log('[CAPACITIES-RECALC] No changes detected, skipping update');
		}

		console.log('[CAPACITIES-RECALC] Recalculation complete');
	} finally {
		// Always reset the flag when done
		isRecalculatingCapacities.set(false);
	}
}

/**
 * Update the recognition cache with a value from another user
 * @param contributorId The ID of the contributor
 * @param ourShare Share we assign to them in our SOGF
 * @param theirShare Share they assign to us in their SOGF
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

/**
 * Get a recognition value from the cache if available
 * @param contributorId The ID of the contributor
 * @returns The cached values or null if not in cache
 */
export function getRecognitionFromCache(contributorId: string) {
	const cache = get(recognitionCache);
	return cache[contributorId] || null;
}

/**
 * Persist recipient-specific shares for efficient lookup
 */
export function persistRecipientSharesCalculation() {
	const tree = get(userTree);
	const capacities = get(userCapacities);
	const nodeMap = get(nodesMap);
	const recipients = get(recipientSharesMap);

	if (!tree || !capacities || !nodeMap) return;

	// Get all unique recipient IDs from all capacities
	const allRecipientIds = new Set<string>();
	Object.values(capacities).forEach((capacity) => {
		if (capacity.recipient_shares) {
			Object.keys(capacity.recipient_shares).forEach((id) => allRecipientIds.add(id));
		}
	});

	// Store shares for each recipient
	allRecipientIds.forEach((recipientId) => {
		const recipientNode = nodeMap[recipientId];
		if (!recipientNode) return;

		const sharesForRecipient = getReceiverShares(recipientNode, tree, capacities, nodeMap);

		// Update local state
		recipients[recipientId] = sharesForRecipient;
	});

	// Update the store
	recipientSharesMap.set(recipients);
}
