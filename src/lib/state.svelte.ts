import Gun from 'gun';
import 'gun/sea';
import 'gun/axe';
import 'gun/lib/yson.js';
import { get, writable, derived, type Writable, type Readable } from 'svelte/store';
import {
	createRootNode,
	sharesOfGeneralFulfillmentMap,
	calculateRecipientShares,
	getReceiverShares,
	getAllContributorsFromTree,
	normalizeShareMap
} from '$lib/protocol';
import {
	type RootNode,
	type CapacitiesCollection,
	type Node,
	type ShareMap,
	type NonRootNode,
	type RecognitionCache,
	CapacitySchema,
	CapacitiesCollectionSchema,
	RecognitionCacheSchema,
	ShareMapSchema
} from '$lib/schema';
import { parseTree, parseCapacities, parseShareMap, parseRecognitionCache } from '$lib/validation';

// User Space
export const userTree: Writable<RootNode | null> = writable(null);
export const userSogf: Writable<ShareMap | null> = writable(null);
export const userCapacities: Writable<CapacitiesCollection | null> = writable(null);
export const nodesMap: Writable<Record<string, Node>> = writable({});
export const recipientSharesMap: Writable<Record<string, any>> = writable({});

export const contributors = writable<string[]>([]); // those we recognize in our tree, will be used to calculate mutual-recognition and populate our providerShares
export const allTimeContributors = writable<string[]>([]); // all contributors who have ever been added (for explicit 0 values in SOGF)
export const mutualContributors = writable<string[]>([]); // those with whom we have mutual-recognition with

// Cache for recognition values - maps contributor ID to {ourShare, theirShare}
export const recognitionCache = writable<RecognitionCache>({});
// whenever we update ourShare we will recalculate all mutual-recognition values
// we will update theirShare using a gun subscription

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

// Public Space
// For Public Templates
export const publicTemplates: Writable<Record<string, any>> = writable({}); // Tree Templates (An Array of stringified JSONs)

// Loading state flags to prevent recalculation loops
export const isLoadingCapacities = writable(false);
export const isLoadingTree = writable(false);
export const isRecalculatingCapacities = writable(false);
export const isRecalculatingTree = writable(false);

// Database
export const gun = Gun();

export const usersList = gun.get('freely-associating-players');

// Cache for user names to avoid repeated Gun lookups
export const userNamesCache = writable<Record<string, string>>({});

/**
 * Get user name from Gun with caching
 * @param userId The user ID to look up
 * @returns Promise that resolves to the user's display name
 */
export async function getUserName(userId: string): Promise<string> {
	// Check cache first
	const cache = get(userNamesCache);
	if (cache[userId]) {
		return cache[userId];
	}

	// Look up from freely-associating-players first
	try {
		const pubUser: any = await new Promise((resolve) => {
			usersList.get(userId).once(resolve);
		});

		if (pubUser && pubUser.name) {
			userNamesCache.update((cache) => ({
				...cache,
				[userId]: pubUser.name
			}));
			return pubUser.name;
		}
	} catch (error) {
		console.log(
			`[USER-NAME] Could not fetch from freely-associating-players for ${userId}:`,
			error
		);
	}

	// If not found, try the user's protected space
	try {
		const alias: any = await new Promise((resolve) => {
			gun.get(`~${userId}`).get('alias').once(resolve);
		});

		if (alias && typeof alias === 'string') {
			userNamesCache.update((cache) => ({
				...cache,
				[userId]: alias
			}));
			return alias;
		}
	} catch (error) {
		console.log(`[USER-NAME] Could not fetch alias for user ${userId}:`, error);
	}

	// Fallback to truncated ID
	const fallbackName = userId.substring(0, 8) + '...';
	userNamesCache.update((cache) => ({
		...cache,
		[userId]: fallbackName
	}));
	return fallbackName;
}

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
		// Parse the tree using our validation utility
		const parsedTree = parseTree(treeData);

		if (parsedTree && user.is?.pub) {
			userTree.set(parsedTree);
		} else if (user.is?.pub) {
			// No tree exists, create a new one
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

			// Update all-time contributors list (union of current and historical contributors)
			const currentAllTime = get(allTimeContributors);
			const newAllTime = [...new Set([...currentAllTime, ...allContributorIds])];
			if (newAllTime.length !== currentAllTime.length) {
				console.log('[RECALC] Updating all-time contributors:', {
					previous: currentAllTime.length,
					current: newAllTime.length,
					new: newAllTime.filter((id) => !currentAllTime.includes(id))
				});
				allTimeContributors.set(newAllTime);
			}
		} catch (error) {
			console.error('[RECALC] Error collecting contributors:', error);
		}

		// Step 1: Calculate SOGF
		try {
			console.log('[RECALC] Calculating SOGF...');
			const contributorsList = get(contributors);
			const allTimeContributorsList = get(allTimeContributors);

			// Use all-time contributors for SOGF calculation to ensure removed contributors get 0 values
			const sogf = sharesOfGeneralFulfillmentMap(tree, nodeMap, allTimeContributorsList);
			userSogf.set(sogf);
			console.log('[RECALC] SOGF calculation complete with', Object.keys(sogf).length, 'entries');

			// Log explicit 0 values for removed contributors
			const removedContributors = allTimeContributorsList.filter(
				(id) => !contributorsList.includes(id)
			);
			if (removedContributors.length > 0) {
				console.log(
					'[RECALC] Removed contributors with explicit 0 values:',
					removedContributors.map((id) => `${id}: ${sogf[id] || 0}`)
				);
			}

			// Update recognition cache with our share values for ALL all-time contributors
			allTimeContributorsList.forEach((contributorId) => {
				// Get our share from SOGF (will be 0 for removed contributors)
				const ourShare = sogf[contributorId] || 0;
				console.log(
					`[RECALC] Processing contributor ${contributorId}: ourShare=${ourShare.toFixed(4)}`
				);

				// Always update the cache with current ourShare (including explicit 0s)
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

		// Step 2: Update mutual contributors list
		try {
			console.log('[RECALC] Updating mutual contributors...');
			// Get current mutual recognition values
			const mutualValues = get(mutualRecognition);
			const contributorsList = get(contributors);

			// FIXED: Only include contributors with actual mutual recognition > 0
			const mutualList = Object.entries(mutualValues)
				.filter(([_, value]) => value > 0)
				.map(([contributorId, _]) => contributorId);

			// Fallback: if no mutual recognition yet, use empty array (not all contributors)
			const finalMutualList = mutualList.length > 0 ? mutualList : [];

			mutualContributors.set(finalMutualList);
			console.log(
				'[RECALC] Found',
				finalMutualList.length,
				'mutual contributors with recognition > 0'
			);
		} catch (error) {
			console.error('[RECALC] Error updating mutual contributors:', error);
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
		try {
			persistAllTimeContributors();
		} catch (e) {
			console.error('[PERSIST] Error persisting all-time contributors:', e);
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
		// Ensure we're storing in the user's protected space using the correct pattern
		// This follows the Gun guide's recommendation for user-specific data
		user.get('sogf').put(structuredClone(sogfValue));
	}
}

export function persistProviderShares() {
	// Store provider shares
	const shares = get(providerShares);
	if (shares && Object.keys(shares).length > 0) {
		user.get('providerShares').put(structuredClone(shares));
	}
}

export function persistAllTimeContributors() {
	const allTimeContributorsList = get(allTimeContributors);
	if (allTimeContributorsList && allTimeContributorsList.length > 0) {
		console.log('[PERSIST] Persisting all-time contributors:', allTimeContributorsList.length);
		user.get('allTimeContributors').put(JSON.stringify(allTimeContributorsList));
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

	// Load tree
	user.get('tree').once((treeData: any) => {
		console.log(
			'[MANIFEST] Raw tree data from Gun:',
			typeof treeData === 'string' ? `String of length ${treeData.length}` : 'Object:',
			treeData
		);

		// Parse the tree using our validation utility
		const parsedTree = parseTree(treeData);

		if (parsedTree && user.is?.pub) {
			console.log('[MANIFEST] Loaded tree with children count:', parsedTree.children?.length || 0);
			if (parsedTree.children?.length > 0) {
				console.log('[MANIFEST] First child:', parsedTree.children[0]);
			}
			userTree.set(parsedTree);
		} else if (user.is?.pub) {
			// No valid tree data found, create a new one
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
			// Parse SOGF data with validation
			const validatedSogf = parseShareMap(sogfData);
			userSogf.set(validatedSogf);
		}
	});

	// Set loading flag before loading capacities
	isLoadingCapacities.set(true);

	user.get('capacities').once((capacitiesData: any) => {
		console.log('[MANIFEST] Loading capacities data...');
		if (capacitiesData) {
			// Parse capacities with validation
			const validatedCapacities = parseCapacities(capacitiesData);
			console.log('[MANIFEST] Loaded capacities count:', Object.keys(validatedCapacities).length);
			userCapacities.set(validatedCapacities);
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

	// Load all-time contributors
	loadAllTimeContributors();
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

	// Persist the updated cache
	persistRecognitionCache();

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
			// Parse recognition cache with validation
			const validatedCache = parseRecognitionCache(data);
			console.log(
				`[MANIFEST] Loaded recognition cache with ${Object.keys(validatedCache).length} entries`
			);
			recognitionCache.set(validatedCache);
		}
	});
}

/**
 * Load all-time contributors from Gun
 */
export function loadAllTimeContributors() {
	user.get('allTimeContributors').once((data: any) => {
		if (data) {
			try {
				const contributorsList = typeof data === 'string' ? JSON.parse(data) : data;
				if (Array.isArray(contributorsList)) {
					console.log(
						`[MANIFEST] Loaded all-time contributors: ${contributorsList.length} entries`
					);
					allTimeContributors.set(contributorsList);
				}
			} catch (error) {
				console.error('[MANIFEST] Error parsing all-time contributors:', error);
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

	console.log(`[NETWORK] Existing cache entry for ${contributorId}:`, existing);

	// Update the cache immediately with new theirShare
	recognitionCache.update((cache) => {
		if (existing) {
			// Update only theirShare in existing entry
			console.log(`[NETWORK] Updating existing entry for ${contributorId}`);
			cache[contributorId] = {
				...cache[contributorId],
				theirShare,
				timestamp: Date.now()
			};
		} else {
			// Create new entry with default ourShare of 0
			console.log(`[NETWORK] Creating new entry for ${contributorId} with ourShare=0`);
			cache[contributorId] = {
				ourShare: 0, // We don't know our share yet
				theirShare,
				timestamp: Date.now()
			};
		}

		console.log(`[NETWORK] Updated cache entry for ${contributorId}:`, cache[contributorId]);
		return cache;
	});

	// Persist the updated cache
	persistRecognitionCache();

	// Log the updated cache and force reactivity check
	const updatedCache = get(recognitionCache);
	const updatedMutual = get(mutualRecognition);
	const updatedProvider = get(providerShares);

	console.log(`[NETWORK] Cache after network update from ${contributorId}:`, updatedCache);
	console.log(`[NETWORK] Mutual recognition after update:`, updatedMutual);
	console.log(`[NETWORK] Provider shares after update:`, updatedProvider);
}

/**
 * Set up network subscriptions for contributor's SOGF
 * @param contributorId The ID of the contributor to subscribe to
 */
export function subscribeToContributorSOGF(contributorId: string) {
	console.log(`[NETWORK] Setting up subscription to ${contributorId}'s SOGF`);

	// FIXED: Use the correct Gun pattern for accessing other users' protected data
	// The guide shows user data is stored under gun.get(`~${public_key}`)
	const contributorSogf = gun.get(`~${contributorId}`).get('sogf');

	// Add timeout protection as recommended by the Gun guide
	// to prevent hanging when data doesn't exist
	let hasReceived = false;
	const timeout = setTimeout(() => {
		if (!hasReceived) {
			console.log(`[NETWORK] Timeout waiting for SOGF from ${contributorId}`);
		}
	}, 10000); // 10 second timeout

	// Subscribe to changes
	contributorSogf.on((sogfData: any) => {
		hasReceived = true;
		clearTimeout(timeout);

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

// Watch for changes to contributors and subscribe to get their SOGF data
contributors.subscribe((allContributors) => {
	if (!allContributors.length) return;

	// Only run this if we're authenticated
	if (!user.is?.pub) return;

	console.log(
		`[NETWORK] Contributors changed, now have ${allContributors.length}, setting up subscriptions`
	);

	// Get current recognition cache to check which ones we're already subscribed to
	const cache = get(recognitionCache);

	// Subscribe to ALL contributors to get their SOGF data
	// This allows us to receive theirShare values and determine who's mutual
	allContributors.forEach((contributorId) => {
		if (
			!cache[contributorId] ||
			cache[contributorId].timestamp < Date.now() - 24 * 60 * 60 * 1000
		) {
			// Subscribe if we don't have data or it's older than 24 hours
			console.log(`[NETWORK] Setting up subscription for contributor: ${contributorId}`);
			subscribeToContributorSOGF(contributorId);
		} else {
			console.log(`[NETWORK] Already have recent data for ${contributorId}, skipping subscription`);
		}
	});
});

/**
 * Debug function to log current state of all recognition-related stores
 */
export function debugRecognitionState() {
	console.log('=== RECOGNITION DEBUG STATE ===');

	const currentContributors = get(contributors);
	const currentMutual = get(mutualContributors);
	const currentCache = get(recognitionCache);
	const currentMutualRecognition = get(mutualRecognition);
	const currentProviderShares = get(providerShares);
	const currentSogf = get(userSogf);

	console.log('Contributors:', currentContributors);
	console.log('Mutual Contributors:', currentMutual);
	console.log('User SOGF:', currentSogf);

	// Detailed recognition cache breakdown
	console.log('=== DETAILED RECOGNITION CACHE ===');
	Object.entries(currentCache).forEach(([contributorId, entry]) => {
		console.log(`${contributorId}:`, {
			ourShare: entry.ourShare,
			theirShare: entry.theirShare,
			mutual: Math.min(entry.ourShare, entry.theirShare),
			timestamp: new Date(entry.timestamp).toISOString()
		});
	});

	console.log('Mutual Recognition:', currentMutualRecognition);
	console.log('Provider Shares:', currentProviderShares);
	console.log('=== END DEBUG ===');
}

/**
 * Debug function to manually trigger subscriptions for all contributors
 */
export function debugTriggerSubscriptions() {
	console.log('[DEBUG] Manually triggering subscriptions for all contributors');
	const allContributors = get(contributors);

	allContributors.forEach((contributorId) => {
		console.log(`[DEBUG] Setting up subscription for: ${contributorId}`);
		subscribeToContributorSOGF(contributorId);
	});

	console.log(`[DEBUG] Set up ${allContributors.length} subscriptions`);
}

// Expose debug functions to global scope for console access
if (typeof window !== 'undefined') {
	(window as any).debugRecognition = debugRecognitionState;
	(window as any).debugTriggerSubscriptions = debugTriggerSubscriptions;
}
