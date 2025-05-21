import Gun from 'gun';
import 'gun/sea';
import 'gun/axe';
import { get, writable, derived, type Writable, type Readable } from 'svelte/store';
import type { RootNode, CapacitiesCollection, Node, ShareMap } from '$lib/protocol/protocol';
import {
	createRootNode,
	sharesOfGeneralFulfillmentMap,
	providerShares,
	calculateRecipientShares,
	getReceiverShares
} from '$lib/protocol/protocol';

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

// Public Space
// For Public Templates
export const publicTemplates: Writable<Record<string, any>> = writable({}); // Tree Templates (An Array of stringified JSONs)

/*
if we are properly updating the userCapacities writeable we should be automatically running the persistance logic
*/

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
export function recalculateFromCapacities() {
	const tree = get(userTree);
	const capacities = get(userCapacities);
	const nodeMap = get(nodesMap);

	if (!tree || !capacities) return;

	// Only recalculate recipient shares
	Object.values(capacities).forEach((capacity) => {
		if (capacity.owner_id === tree.id) {
			calculateRecipientShares(capacity, tree, nodeMap);
		}
	});

	userCapacities.set(capacities);
	persistCapacities();
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
	});

	loadRecipientShares();
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