import { get, writable, derived, type Writable, type Readable } from 'svelte/store';
import { user } from '$lib/gunSetup';
import type { RootNode, CapacitiesCollection, Node, ShareMap } from '$lib/protocol/protocol';
import {
	createRootNode,
	sharesOfGeneralFulfillmentMap,
	providerShares,
	calculateRecipientShares,
	getReceiverShares
} from '$lib/protocol/protocol';

// User Space
const userTree: Writable<RootNode | null> = writable(null);
const userSogf: Writable<ShareMap | null> = writable(null);
const userProviderShares: Writable<{
	1: ShareMap | null;
	2: ShareMap | null;
	3: ShareMap | null;
}> = writable({
	1: null,
	2: null,
	3: null
});
const userCapacities: Writable<CapacitiesCollection | null> = writable(null);
const nodesMap: Writable<Record<string, Node>> = writable({});
const contributors = writable([]); // list of contributor IDS (those recognized in our tree) this will be useful for mutual-recognition equations
const recipientSharesMap: Writable<Record<string, any>> = writable({});

// Public Space
// For Public Templates
export const publicTemplates: Writable<Record<string, any>> = writable({}); // Tree Templates (An Array of stringified JSONs)

/*
if we are properly updating the userCapacities writeable we should be automatically running the persistance logic
*/

/**
 * Sequential recalculation of all dependent data when tree changes
 */
function recalculateFromTree() {
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

	// Step 1: Calculate SOGF
	try {
		console.log('[RECALC] Calculating SOGF...');
		const sogf = sharesOfGeneralFulfillmentMap(tree, nodeMap);
		userSogf.set(sogf);
		console.log('[RECALC] SOGF calculation complete');
	} catch (error) {
		console.error('[RECALC] Error calculating SOGF:', error);
	}

	// Step 2: Calculate provider shares
	try {
		console.log('[RECALC] Calculating provider shares...');
		const depth1Shares = providerShares(tree, 1, nodeMap);
		const depth2Shares = providerShares(tree, 2, nodeMap);
		const depth3Shares = providerShares(tree, 3, nodeMap);

		userProviderShares.set({
			1: depth1Shares,
			2: depth2Shares,
			3: depth3Shares
		});
		console.log('[RECALC] Provider shares calculation complete');
	} catch (error) {
		console.error('[RECALC] Error calculating provider shares:', error);
	}

	// Step 3: Calculate recipient shares if capacities exist
	if (capacities) {
		try {
			console.log(
				'[RECALC] Calculating recipient shares for',
				Object.keys(capacities).length,
				'capacities...'
			);

			Object.values(capacities).forEach((capacity) => {
				try {
					if (capacity.owner_id === tree.id) {
						calculateRecipientShares(capacity, tree, nodeMap);
					}
				} catch (capacityError) {
					console.error(
						'[RECALC] Error calculating shares for capacity:',
						capacity.id,
						capacityError
					);
				}
			});

			userCapacities.set(capacities);
			console.log('[RECALC] Recipient shares calculation complete');
		} catch (error) {
			console.error('[RECALC] Error calculating recipient shares:', error);
		}
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
}

/**
 * Recalculation when only capacities change
 */
function recalculateFromCapacities() {
	const tree = get(userTree);
	const capacities = get(userCapacities);
	const nodeMap = get(nodesMap);

	if (!tree || !capacities) return;

	console.log('[RECALC-CAP] Starting capacity recalculation...');

	// Only recalculate recipient shares, without triggering store updates
	try {
		Object.values(capacities).forEach((capacity) => {
			if (capacity.owner_id === tree.id) {
				calculateRecipientShares(capacity, tree, nodeMap);
			}
		});
		
		// Persist directly without updating store
		persistCapacities(capacities);
		
		console.log('[RECALC-CAP] Capacity recalculation complete');
	} catch (error) {
		console.error('[RECALC-CAP] Error recalculating capacities:', error);
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
	
	console.log('[CAP-SUB] Capacities updated in store, count:', Object.keys(capacities).length);
	
	// Clear any pending recalculation
	if (capacitiesRecalcTimer) clearTimeout(capacitiesRecalcTimer);
	
	// Schedule a recalculation after a short delay
	capacitiesRecalcTimer = setTimeout(() => {
		// Only recalculate recipient shares - don't trigger store update
		try {
			console.log('[CAP-SUB] Recalculating recipient shares without triggering store update');
			const tree = get(userTree);
			const nodeMap = get(nodesMap);
			
			if (tree && capacities) {
				// Update recipient shares in place, without setting the store again
				Object.values(capacities).forEach((capacity) => {
					if (capacity.owner_id === tree.id) {
						calculateRecipientShares(capacity, tree, nodeMap);
					}
				});
			}
			
			// Persist capacities directly - don't update the store
			console.log('[CAP-SUB] Persisting capacities directly');
			persistCapacities(capacities);
		} catch (error) {
			console.error('[CAP-SUB] Error in capacity recalculation:', error);
		}
		
		capacitiesRecalcTimer = null;
	}, 300); // 300ms debounce
});

/**
 * Persist the current application state to Gun
 */
function persist() {
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
			const capacities = get(userCapacities);
			if(capacities) {
				persistCapacities(capacities);
			}
		} catch (e) {
			console.error('[PERSIST] Error persisting capacities:', e);
		}
		try {
			persistRecipientShares();
		} catch (e) {
			console.error('[PERSIST] Error persisting recipient shares:', e);
		}

		console.log('[PERSIST] Full persistence complete');
	} catch (error) {
		console.error('[PERSIST] Critical error persisting data to Gun:', error);
	}
}

function persistTree() {
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

function persistSogf() {
	const sogfValue = get(userSogf);
	if (sogfValue) {
		user.get('sogf').put(structuredClone(sogfValue));
	}
}

function persistProviderShares() {
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

// Persist capacities directly without updating the store
function persistCapacities(capacitiesValue: CapacitiesCollection) {
	console.log('[PERSIST-CAP-DIRECT] Persisting capacities directly...');
	console.log('[PERSIST-CAP-DIRECT] Capacity count:', Object.keys(capacitiesValue).length);
	
	try {
		// Create a deep clone
		const capacitiesClone = structuredClone(capacitiesValue);
		
		// Serialize to JSON
		const capacitiesJson = JSON.stringify(capacitiesClone);
		console.log('[PERSIST-CAP-DIRECT] Serialized capacities length:', capacitiesJson.length);
		console.log(
			'[PERSIST-CAP-DIRECT] Capacities JSON preview:',
			capacitiesJson.substring(0, 100) + '...'
		);
		
		// Store in Gun with callback
		user.get('capacities').put(capacitiesJson, (ack: { err?: any }) => {
			if (ack.err) {
				console.error('[PERSIST-CAP-DIRECT] Error saving capacities to Gun:', ack.err);
			} else {
				console.log('[PERSIST-CAP-DIRECT] Capacities successfully saved to Gun');
			}
		});
	} catch (error) {
		console.error('[PERSIST-CAP-DIRECT] Error serializing or storing capacities:', error);
	}
}

/**
 * Persist recipient-specific shares for efficient lookup
 */
function persistRecipientShares() {
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
function manifest() {
	console.log('[MANIFEST] Loading data from Gun');

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

	user.get('capacities').once((capacitiesData: any) => {
		console.log('[MANIFEST] Capacities data received:', capacitiesData ? 
			(typeof capacitiesData === 'string' ? 
				`String of length ${capacitiesData.length}` : 
				`Object with ${Object.keys(capacitiesData).length} keys`) : 
			'null');
		
		if (capacitiesData) {
			try {
				// Handle both stringified and object formats
				let parsedCapacities;
				if (typeof capacitiesData === 'object') {
					console.log('[MANIFEST] Capacities already in object format');
					parsedCapacities = capacitiesData;
				} else {
					console.log('[MANIFEST] Parsing capacities from string');
					parsedCapacities = JSON.parse(capacitiesData);
				}
				
				console.log('[MANIFEST] Setting capacities store with', 
					Object.keys(parsedCapacities).length, 'capacities');
				userCapacities.set(parsedCapacities);
			} catch (err) {
				console.error('[MANIFEST] Error parsing capacities data:', err);
			}
		} else {
			console.log('[MANIFEST] No capacities data found');
		}
	});

	loadRecipientShares();
}

/**
 * Validates and heals tree data to ensure it has a valid structure
 * @param treeData The raw tree data from Gun
 * @returns A valid RootNode, either from the provided data or newly created
 */
function healTree(treeData: any): RootNode | null {
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
function loadRecipientShares() {
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
function getUserSharesFromAllProviders(userId: string) {
	// This function would be used to query the network for shares
	// from other users who have the current user as a recipient

	// For demo/local usage - just return the local user's recipient map
	const recipients = get(recipientSharesMap);
	return recipients[userId] || {};
}

// Export persistence functions
export {
	userTree,
	userSogf,
	userProviderShares,
	userCapacities,
	nodesMap,
	recipientSharesMap,
	persist,
	persistSogf,
	persistCapacities,
	persistProviderShares,
	persistRecipientShares,
	manifest,
	healTree,
	recalculateFromTree,
	recalculateFromCapacities,
	getUserSharesFromAllProviders
};
