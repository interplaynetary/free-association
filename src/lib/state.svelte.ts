import Gun from 'gun';
import 'gun/sea';
import 'gun/axe';
import { get, writable, derived, type Writable, type Readable } from 'svelte/store';
import type { RootNode, CapacitiesCollection, Node, ShareMap, NonRootNode } from '$lib/protocol';
import {
	createRootNode,
	sharesOfGeneralFulfillmentMap,
	providerShares,
	calculateRecipientShares,
	getReceiverShares,
	getDescendants,
	mutualFulfillment,
	getAllContributorsFromTree
} from '$lib/protocol';

// User Space
export const userTree: Writable<RootNode | null> = writable(null);
export const userSogf: Writable<ShareMap | null> = writable(null);
export const userProviderShares: Writable<{
	1: ShareMap | null;
	2: ShareMap | null;
	3: ShareMap | null;
}> = writable({
	1: null,
	2: null,
	3: null
});
export const userCapacities: Writable<CapacitiesCollection | null> = writable(null);
export const nodesMap: Writable<Record<string, Node>> = writable({});
export const recipientSharesMap: Writable<Record<string, any>> = writable({});

export const contributors = writable<string[]>([]); // those we recognize in our tree, will be used to calculate mutual-recognition and populate our providerShares
export const mutualContributors = writable<string[]>([]); // those with whom we have mutual-recognition with

// Cache for recognition values - maps contributor ID to {ourShare, theirShare}
export const recognitionCache = writable<
	Record<
		string,
		{
			ourShare: number; // Share we assign to them in our SOGF
			theirShare: number; // Share they assign to us in their SOGF
			timestamp: number; // When this value was last updated
		}
	>
>({});
// whenever we update ourShare we will recalculate all mutual-recognition values
// we will update theirShare using a gun subscription

// Derived store for mutual recognition values (min of ourShare and theirShare)
export const mutualRecognition = derived(recognitionCache, ($recognitionCache) => {
	const mutualValues: Record<string, number> = {};

	for (const [contributorId, recognition] of Object.entries($recognitionCache)) {
		// Mutual recognition is the minimum of our share and their share
		mutualValues[contributorId] = Math.min(recognition.ourShare, recognition.theirShare);
	}

	return mutualValues;
});

// Derived store for normalized mutual recognition values (sum to 1.0)
export const normalizedMutualRecognition = derived(mutualRecognition, ($mutualRecognition) => {
	const normalizedValues: Record<string, number> = {};

	// Calculate sum of all mutual recognition values
	const sum = Object.values($mutualRecognition).reduce((total, value) => total + value, 0);

	// If sum is zero, return empty object
	if (sum === 0) return normalizedValues;

	// Normalize each value
	for (const [contributorId, value] of Object.entries($mutualRecognition)) {
		normalizedValues[contributorId] = value / sum;
	}

	return normalizedValues;
});

// Public SpaceThe leader of the Johnson Forest tendency had been in the Workers' Party.
// For Public Templates
export const publicTemplates: Writable<Record<string, any>> = writable({}); // Tree Templates (An Array of stringified JSONs)

// Loading state flags to prevent recalculation loops
export const isLoadingCapacities = writable(false);
export const isLoadingTree = writable(false);
export const isRecalculatingCapacities = writable(false);
export const isRecalculatingTree = writable(false);

// Database
export const gun = Gun();

export const usersList = gun.get('users');

export let user = gun.user().recall({ sessionStorage: true });

// Current User's username
export const username = writable('');
export const userpub = writable('');

gun.on('auth', async () => {
	const alias = await user.get('alias'); // username string
	username.set(alias);
	userpub.set(user.is?.pub);
	usersList.get(user.is?.pub).put({
		name: alias,
		lastSeen: Date.now()
	});

	console.log(`signed in as ${alias}`);

	// Load existing user data
	manifest();

	// Check if user has a tree, and if not, initialize one
	user.get('tree').once((treeData: any) => {
		// Use the healTree function from state.ts
		const healedTree = healTree(treeData);

		if (!healedTree && user.is?.pub) {
			// No tree exists yet and healing couldn't help, create a new one
			console.log('No tree found, creating initial tree for user');
			const newTree = createRootNode(user.is.pub, alias, user.is.pub);
			const jsonTree = JSON.stringify(newTree);
			console.log('Storing new tree as JSON string:', jsonTree);
			user.get('tree').put(jsonTree);
			userTree.set(newTree);
		}
	});
});

export function login(username: string, password: string) {
	user.auth(username, password, ({ err }: { err: any }) => err && alert(err));
}

export function signup(username: string, password: string) {
	user.create(username, password, ({ err }: { err: any }) => {
		if (err) {
			alert(err);
		} else {
			login(username, password);
		}
	});
}

export function signout() {
	user.leave();
	username.set('');
	userpub.set('');
}

// Force persist on window unload
if (typeof window !== 'undefined' && user?.is?.pub) {
	window.addEventListener('beforeunload', () => {
		console.log('Window closing, forcing persistence...');
		persist();
	});
}

// Force persist on any disconnect
gun.on('bye', () => {
	if (user.is?.pub) {
		console.log('Gun disconnecting, forcing persistence...');
		persist();
	}
});

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
		} catch (error) {
			console.error('[RECALC] Error collecting contributors:', error);
		}

		// Step 1: Calculate SOGF
		try {
			console.log('[RECALC] Calculating SOGF...');
			const contributorsList = get(contributors);

			// Use specific contributors if available
			const sogf = sharesOfGeneralFulfillmentMap(tree, nodeMap, contributorsList);
			userSogf.set(sogf);
			console.log('[RECALC] SOGF calculation complete');
		} catch (error) {
			console.error('[RECALC] Error calculating SOGF:', error);
		}

		// Step 2: Calculate provider shares
		try {
			console.log('[RECALC] Calculating provider shares...');
			const contributorsList = get(contributors);
			const mutualValues = get(normalizedMutualRecognition);

			// Calculate using all identified contributors
			const depth1Shares = providerShares(tree, 1, nodeMap, contributorsList);
			const depth2Shares = providerShares(tree, 2, nodeMap, contributorsList);
			const depth3Shares = providerShares(tree, 3, nodeMap, contributorsList);

			// Apply mutual recognition values to depth-1 shares if available
			if (Object.keys(mutualValues).length > 0) {
				console.log('[RECALC] Applying normalized mutual recognition values to depth-1 shares');

				// Set the provider shares with the normalized mutual values
				userProviderShares.set({
					1: mutualValues,
					2: depth2Shares,
					3: depth3Shares
				});
			} else {
				// Set provider shares with the calculated values
				userProviderShares.set({
					1: depth1Shares,
					2: depth2Shares,
					3: depth3Shares
				});
			}

			// Update mutual contributors based on depth-1 shares
			// These are contributors who have mutual recognition with us
			const mutualList = Object.keys(depth1Shares);
			mutualContributors.set(mutualList);
			console.log('[RECALC] Found', mutualList.length, 'mutual contributors');

			// Update recognition cache with calculated values
			const sogf = get(userSogf);
			if (sogf) {
				mutualList.forEach((contributorId) => {
					const contributor = nodeMap[contributorId];
					if (!contributor) return;

					// Get our share from SOGF
					const ourShare = sogf[contributorId] || 0;

					// Get their share from depth-1 shares
					const theirShare = depth1Shares[contributorId] || 0;

					// Only update if we have actual values
					if (ourShare > 0 && theirShare > 0) {
						updateRecognitionCache(contributorId, ourShare, theirShare);
					}
				});
			}

			console.log('[RECALC] Provider shares calculation complete');
		} catch (error) {
			console.error('[RECALC] Error calculating provider shares:', error);
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

				// Track if any changes were made
				let hasCapacityChanges = false;

				Object.values(capacities).forEach((capacity) => {
					try {
						if (capacity.owner_id === tree.id) {
							// Save state before calculation
							const sharesBeforeCalc = JSON.stringify(capacity.recipient_shares || {});

							calculateRecipientShares(capacity, tree, nodeMap);

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

					// Calculate recipient shares
					calculateRecipientShares(capacity, tree, nodeMap);

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
 * Update nodes map whenever the tree changes
 */
userTree.subscribe((tree) => {
	if (tree) {
		console.log('[TREE-SUB] Tree updated in store, rebuilding nodes map');
		console.log('[TREE-SUB] Tree has', tree.children.length, 'children');

		// Create a map of all nodes by ID for faster lookup
		const newNodesMap: Record<string, Node> = {};

		// Helper to traverse tree and add nodes to map
		function addNodeToMap(node: Node) {
			newNodesMap[node.id] = node;
			node.children.forEach(addNodeToMap);
		}

		addNodeToMap(tree);
		nodesMap.set(newNodesMap);

		console.log('[TREE-SUB] Rebuilt nodes map with', Object.keys(newNodesMap).length, 'nodes');

		// Force immediate tree persistence on every tree change
		// This ensures tree changes are always saved, even if recalculation fails
		console.log('[TREE-SUB] Forcing immediate tree persistence');
		persistTree();
	}
});

/**
 * Trigger recalculations when tree changes
 * Uses a simple debounce to prevent excessive calculations during rapid changes
 */
let treeRecalcTimer: ReturnType<typeof setTimeout> | null = null;
userTree.subscribe((tree) => {
	if (!tree) return;

	console.log('[TREE-RECALC-SUB] Tree updated, scheduling recalculation');
	console.log('[TREE-RECALC-SUB] Tree has', tree.children.length, 'children');

	// Skip recalculation if we're just loading from storage
	if (get(isLoadingTree)) {
		console.log('[TREE-RECALC-SUB] Skipping recalculation because tree is being loaded');
		return;
	}

	// Skip recalculation if this update is from a recalculation
	if (get(isRecalculatingTree)) {
		console.log('[TREE-RECALC-SUB] Skipping recalculation because update is from recalculation');
		return;
	}

	// Clear any pending recalculation
	if (treeRecalcTimer) {
		console.log('[TREE-RECALC-SUB] Clearing previous recalc timer');
		clearTimeout(treeRecalcTimer);
	}

	// Schedule a recalculation after a short delay
	console.log('[TREE-RECALC-SUB] Setting new recalc timer for 300ms');
	treeRecalcTimer = setTimeout(() => {
		console.log('[TREE-RECALC-SUB] Timer fired, running recalculation');

		// Try to run the full recalculation, but catch any errors to prevent
		// them from blocking the tree persistence
		try {
			recalculateFromTree();
		} catch (error) {
			console.error('[TREE-RECALC-SUB] Error during recalculation:', error);

			// Even if recalculation fails, ensure the tree is persisted
			console.log('[TREE-RECALC-SUB] Forcing tree persistence after error');
			persistTree();
		}

		treeRecalcTimer = null;
	}, 300); // 300ms debounce
});

/**
 * Trigger recalculations when capacities change
 */
let capacitiesRecalcTimer: ReturnType<typeof setTimeout> | null = null;
userCapacities.subscribe((capacities) => {
	if (!capacities) return;

	console.log('[CAPACITIES-SUB] Capacities updated, scheduling recalculation');
	console.log('[CAPACITIES-SUB] Capacities count:', Object.keys(capacities).length);

	// Force immediate capacity persistence on every change
	// This ensures capacity changes are always saved, even if recalculation fails
	console.log('[CAPACITIES-SUB] Forcing immediate capacities persistence');
	try {
		persistCapacities();
	} catch (error) {
		console.error('[CAPACITIES-SUB] Error during immediate persistence:', error);
	}

	// Skip recalculation if we're just loading from storage
	if (get(isLoadingCapacities)) {
		console.log('[CAPACITIES-SUB] Skipping recalculation because capacities are being loaded');
		return;
	}

	// Skip recalculation if this update is from a recalculation
	if (get(isRecalculatingCapacities)) {
		console.log('[CAPACITIES-SUB] Skipping recalculation because update is from recalculation');
		return;
	}

	// Clear any pending recalculation
	if (capacitiesRecalcTimer) {
		console.log('[CAPACITIES-SUB] Clearing previous capacities recalc timer');
		clearTimeout(capacitiesRecalcTimer);
	}

	// Schedule a recalculation after a short delay
	console.log('[CAPACITIES-SUB] Setting new capacities recalc timer for 300ms');
	capacitiesRecalcTimer = setTimeout(() => {
		console.log('[CAPACITIES-SUB] Timer fired, running capacities recalculation');
		try {
			recalculateFromCapacities();
		} catch (error) {
			console.error('[CAPACITIES-SUB] Error during recalculation:', error);
			// Even if recalculation fails, ensure capacities are persisted
			console.log('[CAPACITIES-SUB] Forcing capacities persistence after error');
			persistCapacities();
		}
		capacitiesRecalcTimer = null;
	}, 300); // 300ms debounce
});

/**
 * Persist the current application state to Gun
 */
export function persist() {
	try {
		console.log('[PERSIST] Starting full persistence...');

		// Always persist tree first, as it's the most important
		persistTree();

		// Try to persist other data if available
		try {
			persistSogf();
		} catch (e) {
			console.error('[PERSIST] Error persisting SOGF:', e);
		}
		try {
			persistProviderShares();
		} catch (e) {
			console.error('[PERSIST] Error persisting provider shares:', e);
		}
		try {
			persistCapacities();
		} catch (e) {
			console.error('[PERSIST] Error persisting capacities:', e);
		}
		try {
			persistRecipientShares();
		} catch (e) {
			console.error('[PERSIST] Error persisting recipient shares:', e);
		}
		try {
			persistRecognitionCache();
		} catch (e) {
			console.error('[PERSIST] Error persisting recognition cache:', e);
		}

		console.log('[PERSIST] Full persistence complete');
	} catch (error) {
		console.error('[PERSIST] Critical error persisting data to Gun:', error);
	}
}

export function persistTree() {
	const treeValue = get(userTree);
	if (treeValue) {
		console.log('[PERSIST] Starting tree persistence...');
		console.log('[PERSIST] Tree structure before serialization:', {
			id: treeValue.id,
			childCount: treeValue.children.length
		});

		// Serialize tree for storage
		const treeJson = JSON.stringify(structuredClone(treeValue));
		console.log('[PERSIST] Serialized tree length:', treeJson.length);
		console.log('[PERSIST] Tree JSON preview:', treeJson.substring(0, 100) + '...');

		// Store in Gun
		user.get('tree').put(treeJson, (ack: { err?: any }) => {
			if (ack.err) {
				console.error('[PERSIST] Error saving tree to Gun:', ack.err);
			} else {
				console.log('[PERSIST] Tree successfully saved to Gun');
			}
		});
	}
}

export function persistSogf() {
	const sogfValue = get(userSogf);
	if (sogfValue) {
		user.get('sogf').put(structuredClone(sogfValue));
	}
}

export function persistProviderShares() {
	// Store provider shares by depth
	const sharesValue = get(userProviderShares);
	if (sharesValue) {
		if (sharesValue[1])
			user.get('providerShares').get('first-degree').put(structuredClone(sharesValue[1]));
		if (sharesValue[2])
			user.get('providerShares').get('second-degree').put(structuredClone(sharesValue[2]));
		if (sharesValue[3])
			user.get('providerShares').get('third-degree').put(structuredClone(sharesValue[3]));
	}
}

export function persistCapacities() {
	const userCapacitiesValue = get(userCapacities);
	if (userCapacitiesValue) {
		console.log('[PERSIST] Starting capacities persistence...');
		console.log('[PERSIST] Capacities count:', Object.keys(userCapacitiesValue).length);

		try {
			// First create a deep clone to avoid any reactivity issues
			const capacitiesClone = structuredClone(userCapacitiesValue);

			// Then serialize to JSON
			const capacitiesJson = JSON.stringify(capacitiesClone);
			console.log('[PERSIST] Serialized capacities length:', capacitiesJson.length);
			console.log(
				'[PERSIST] Capacities JSON preview:',
				capacitiesJson.length > 100 ? capacitiesJson.substring(0, 100) + '...' : capacitiesJson
			);

			// Store in Gun with ACK callback
			user.get('capacities').put(capacitiesJson, (ack: { err?: any }) => {
				if (ack.err) {
					console.error('[PERSIST] Error saving capacities to Gun:', ack.err);
				} else {
					console.log('[PERSIST] Capacities successfully saved to Gun');
				}
			});
		} catch (error) {
			console.error('[PERSIST] Error serializing capacities:', error);

			// Fallback approach
			try {
				console.log('[PERSIST] Attempting fallback capacities persistence...');
				const snapshot = $state.snapshot(userCapacitiesValue);
				user.get('capacities').put(JSON.stringify(snapshot));
				console.log('[PERSIST] Fallback capacities persistence completed');
			} catch (fallbackError) {
				console.error('[PERSIST] Critical error: Failed to persist capacities:', fallbackError);
			}
		}
	}
}

/**
 * Persist recipient-specific shares for efficient lookup
 */
export function persistRecipientShares() {
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

		// Persist to Gun
		user
			.get('recipients')
			.get(recipientId)
			.put(JSON.stringify(structuredClone(sharesForRecipient)));
	});

	// Update the store
	recipientSharesMap.set(recipients);
}

/**
 * Load the application state from Gun
 */
export function manifest() {
	console.log('[MANIFEST] Loading data from Gun');

	// Set loading flag before loading tree
	isLoadingTree.set(true);

	// Load tree with healing
	user.get('tree').once((treeData: any) => {
		console.log(
			'[MANIFEST] Raw tree data from Gun:',
			typeof treeData === 'string' ? `String of length ${treeData.length}` : 'Object:',
			treeData
		);

		const healedTree = healTree(treeData);
		if (healedTree) {
			console.log('[MANIFEST] Loaded tree with children count:', healedTree.children?.length || 0);

			if (healedTree.children?.length > 0) {
				console.log('[MANIFEST] First child:', healedTree.children[0]);
			}

			userTree.set(healedTree);
		} else if (user.is?.pub) {
			// If healing returned null but we have user data, create a new tree
			console.log('[MANIFEST] No valid tree data found, creating initial tree');
			const newTree = createRootNode(user.is.pub, user.is?.alias || 'My Root', user.is.pub);
			user.get('tree').put(JSON.stringify(newTree));
			userTree.set(newTree);
		} else {
			console.log('[MANIFEST] No tree data found and no user to create one');
		}

		// Reset loading flag after tree is loaded
		isLoadingTree.set(false);
	});

	// Load SOGF
	user.get('sogf').once((sogfData: any) => {
		if (sogfData) {
			userSogf.set(sogfData);
		}
	});

	// Load provider shares
	user
		.get('providerShares')
		.get('first-degree')
		.once((shareData: any) => {
			if (shareData) {
				userProviderShares.update((shares) => ({
					...shares,
					1: shareData
				}));
			}
		});

	user
		.get('providerShares')
		.get('second-degree')
		.once((shareData: any) => {
			if (shareData) {
				userProviderShares.update((shares) => ({
					...shares,
					2: shareData
				}));
			}
		});

	user
		.get('providerShares')
		.get('third-degree')
		.once((shareData: any) => {
			if (shareData) {
				userProviderShares.update((shares) => ({
					...shares,
					3: shareData
				}));
			}
		});

	// Set loading flag before loading capacities
	isLoadingCapacities.set(true);

	user.get('capacities').once((capacitiesData: any) => {
		console.log('[MANIFEST] Loading capacities data...');
		if (capacitiesData) {
			try {
				console.log(
					'[MANIFEST] Raw capacities data from Gun:',
					typeof capacitiesData === 'string'
						? `String of length ${capacitiesData.length}`
						: 'Object'
				);

				// Handle both stringified and object formats
				let parsedCapacities;
				if (typeof capacitiesData === 'object') {
					console.log('[MANIFEST] Capacities data is already an object');
					parsedCapacities = capacitiesData;
				} else {
					console.log('[MANIFEST] Capacities data is a string, parsing...');
					try {
						parsedCapacities = JSON.parse(capacitiesData);
						console.log('[MANIFEST] Successfully parsed capacities data');
					} catch (parseError) {
						console.error('[MANIFEST] Failed to parse capacities data:', parseError);
						// Try to recover if this is a Svelte state object somehow
						parsedCapacities = {};
					}
				}

				if (parsedCapacities) {
					console.log('[MANIFEST] Loaded capacities count:', Object.keys(parsedCapacities).length);

					// Log first capacity for debugging
					const firstCapacityId = Object.keys(parsedCapacities)[0];
					if (firstCapacityId) {
						console.log('[MANIFEST] First capacity preview:', {
							id: firstCapacityId,
							name: parsedCapacities[firstCapacityId]?.name,
							shareCount: parsedCapacities[firstCapacityId]?.shares?.length || 0
						});
					}

					userCapacities.set(parsedCapacities);
					console.log('[MANIFEST] Capacities loaded successfully');
				}
			} catch (err) {
				console.error('[MANIFEST] Error processing capacities data:', err);
			}
		} else {
			console.log('[MANIFEST] No capacities data found');
			// Initialize with empty object if no data exists
			userCapacities.set({});
		}

		// Reset loading flag after capacities are loaded
		isLoadingCapacities.set(false);
	});

	// Load recipient shares
	loadRecipientShares();

	// Load recognition cache
	loadRecognitionCache();
}

/**
 * Validates and heals tree data to ensure it has a valid structure
 * @param treeData The raw tree data from Gun
 * @returns A valid RootNode, either from the provided data or newly created
 */
export function healTree(treeData: any): RootNode | null {
	// Handle no data case
	if (!treeData) {
		console.log('[HEAL] No tree data provided to heal');
		return null;
	}

	try {
		// Parse if it's a string
		let parsedTree;
		if (typeof treeData === 'object') {
			console.log('[HEAL] Tree data is already an object');
		} else {
			console.log('[HEAL] Tree data is a string, parsing...');
			try {
				parsedTree = JSON.parse(treeData);
				console.log('[HEAL] Successfully parsed tree data');
			} catch (parseError) {
				console.error('[HEAL] Failed to parse tree data:', parseError);
				return null;
			}
		}

		// Use the parsed object or the original if it was already an object
		parsedTree = parsedTree || treeData;

		console.log('[HEAL] Tree structure:', {
			id: parsedTree?.id,
			type: parsedTree?.type,
			hasChildren: Array.isArray(parsedTree?.children),
			childCount: Array.isArray(parsedTree?.children) ? parsedTree.children.length : 0
		});

		if (parsedTree?.children?.length > 0) {
			console.log('[HEAL] First child:', parsedTree.children[0]);
		}

		// Validate the tree structure
		if (!parsedTree || !parsedTree.id || !parsedTree.type || !Array.isArray(parsedTree.children)) {
			console.error('[HEAL] Invalid tree structure detected, needs healing:', parsedTree);

			// Create a new tree if user is available
			if (user.is?.pub) {
				console.log('[HEAL] Creating new tree with user data');
				parsedTree = createRootNode(user.is.pub, user.is?.alias || 'My Root', user.is.pub);

				// Save the healed tree back to Gun
				user.get('tree').put(JSON.stringify(parsedTree));
			} else {
				console.log('[HEAL] Cannot heal tree - no user data available');
				return null;
			}
		}

		console.log('[HEAL] Returning healed tree with', parsedTree.children.length, 'children');

		return parsedTree as RootNode;
	} catch (err) {
		console.error('[HEAL] Error parsing or healing tree data:', err);

		// Create a recovery tree if possible
		if (user.is?.pub) {
			console.log('[HEAL] Creating recovery tree after error');
			const newTree = createRootNode(user.is.pub, user.is?.alias || 'My Root', user.is.pub);

			// Save the recovery tree
			user.get('tree').put(JSON.stringify(newTree));
			return newTree;
		}

		return null;
	}
}

/**
 * Load recipient shares data
 */
export function loadRecipientShares() {
	user.get('recipients').once((recipientsData: any) => {
		if (recipientsData) {
			// Gun handles this as a collection of keys
			const loadedRecipients: Record<string, any> = {};

			// We'll need to wait for all the recipient data to load
			let recipientsToLoad = Object.keys(recipientsData).length;
			if (recipientsToLoad === 0) return;

			Object.keys(recipientsData).forEach((recipientId) => {
				user
					.get('recipients')
					.get(recipientId)
					.once((data: any) => {
						try {
							if (typeof data === 'string') {
								loadedRecipients[recipientId] = JSON.parse(data);
							} else {
								loadedRecipients[recipientId] = data;
							}
						} catch (e) {
							console.error(`Error parsing recipient data for ${recipientId}:`, e);
						}

						recipientsToLoad--;
						if (recipientsToLoad <= 0) {
							recipientSharesMap.set(loadedRecipients);
						}
					});
			});
		}
	});
}

/**
 * Get all shares for a specific user from all providers
 * @param userId The user ID to lookup shares for
 * @returns Map of provider ID to their provided capacities and shares
 */
export function getUserSharesFromAllProviders(userId: string) {
	// This function would be used to query the network for shares
	// from other users who have the current user as a recipient

	// For demo/local usage - just return the local user's recipient map
	const recipients = get(recipientSharesMap);
	return recipients[userId] || {};
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
		cache[contributorId] = {
			ourShare,
			theirShare,
			timestamp: Date.now()
		};
		return cache;
	});

	// Persist the updated cache
	persistRecognitionCache();
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
 * Persist recognition cache to Gun
 */
export function persistRecognitionCache() {
	const cache = get(recognitionCache);
	if (cache) {
		console.log(`[PERSIST] Saving recognition cache with ${Object.keys(cache).length} entries`);
		user.get('recognitionCache').put(JSON.stringify(structuredClone(cache)));
	}
}

/**
 * Load recognition cache from Gun
 */
export function loadRecognitionCache() {
	user.get('recognitionCache').once((data: any) => {
		if (data) {
			try {
				const parsedCache = typeof data === 'string' ? JSON.parse(data) : data;
				console.log(
					`[MANIFEST] Loaded recognition cache with ${Object.keys(parsedCache).length} entries`
				);
				recognitionCache.set(parsedCache);
			} catch (error) {
				console.error('[MANIFEST] Error parsing recognition cache:', error);
				recognitionCache.set({});
			}
		}
	});
}

/**
 * Update the recognition cache with a contributor's share for us from network
 * @param contributorId The ID of the contributor
 * @param theirShare Share they assign to us in their SOGF
 */
export function updateTheirShareFromNetwork(contributorId: string, theirShare: number) {
	console.log(`[NETWORK] Received share from ${contributorId}: ${theirShare.toFixed(4)}`);

	// Get current cache entry
	const cache = get(recognitionCache);
	const existing = cache[contributorId];

	if (existing) {
		// Update only theirShare in existing entry
		recognitionCache.update((cache) => {
			cache[contributorId] = {
				...cache[contributorId],
				theirShare,
				timestamp: Date.now()
			};
			return cache;
		});
	} else {
		// Create new entry with default ourShare of 0
		recognitionCache.update((cache) => {
			cache[contributorId] = {
				ourShare: 0, // We don't know our share yet
				theirShare,
				timestamp: Date.now()
			};
			return cache;
		});
	}

	// Persist the updated cache
	persistRecognitionCache();
}

/**
 * Set up network subscriptions for contributor's SOGF
 * @param contributorId The ID of the contributor to subscribe to
 */
export function subscribeToContributorSOGF(contributorId: string) {
	console.log(`[NETWORK] Setting up subscription to ${contributorId}'s SOGF`);

	// Create a Gun reference to the contributor's SOGF
	const contributorSogf = gun.user(contributorId).get('sogf');

	// Subscribe to changes
	contributorSogf.on((sogfData: any) => {
		if (!sogfData) return;

		console.log(`[NETWORK] Received SOGF update from ${contributorId}`);

		// Get our user ID
		const ourId = user.is?.pub;
		if (!ourId) return;

		// Extract our share from their SOGF
		const theirShare = sogfData[ourId] || 0;

		// Update our cache with their share for us
		updateTheirShareFromNetwork(contributorId, theirShare);
	});
}

/**
 * Set up subscriptions for all mutual contributors
 */
export function subscribeToAllMutualContributors() {
	console.log('[NETWORK] Setting up subscriptions for all mutual contributors');

	const currentMutualContributors = get(mutualContributors);

	currentMutualContributors.forEach((contributorId) => {
		subscribeToContributorSOGF(contributorId);
	});

	console.log(`[NETWORK] Set up ${currentMutualContributors.length} subscriptions`);
}

// Watch for changes to mutual contributors and subscribe to new ones
mutualContributors.subscribe((contributors) => {
	if (!contributors.length) return;

	// Only run this if we're authenticated
	if (!user.is?.pub) return;

	console.log(`[NETWORK] Mutual contributors changed, now have ${contributors.length}`);

	// Get current recognition cache to check which ones we're already subscribed to
	const cache = get(recognitionCache);

	// Subscribe to any contributors we don't have in our cache
	contributors.forEach((contributorId) => {
		if (
			!cache[contributorId] ||
			cache[contributorId].timestamp < Date.now() - 24 * 60 * 60 * 1000
		) {
			// Subscribe if we don't have data or it's older than 24 hours
			subscribeToContributorSOGF(contributorId);
		}
	});
});
