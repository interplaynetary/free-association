import Gun from 'gun';
import 'gun/sea';
import 'gun/axe';
import 'gun/lib/yson.js';
import 'gun/lib/radix'; // Required for radix tree logic
import 'gun/lib/radisk'; // Required for chunked disk storage
import 'gun/lib/store'; // Bridges GUN storage adapter logic
import 'gun/lib/rindexed'; // IndexedDB adapter for RAD in browser
import 'gun/lib/webrtc.js';

import { writable, get } from 'svelte/store';
import { createRootNode } from '$lib/protocol';
import { parseTree, parseCapacities, parseShareMap, parseRecognitionCache } from '$lib/validation';
import {
	userTree,
	userSogf,
	userCapacities,
	recognitionCache,
	isLoadingTree,
	isLoadingCapacities,
	recipientSharesMap,
	providerShares
} from './core.svelte';

async function requestPersistentStorage() {
	if (typeof navigator !== 'undefined' && navigator?.storage?.persist) {
		try {
			const isPersisted = await navigator.storage.persist();
			console.log('Persistent storage granted?', isPersisted);
			return isPersisted;
		} catch (e) {
			console.error('Error requesting persistent storage:', e);
		}
	} else {
		console.warn('Persistent storage API not available (SSR or unsupported browser).');
	}
}

requestPersistentStorage();

// Database
export const gun = Gun({
	peers: ['http://localhost:8765/gun'],
	localStorage: false
	//radisk: false
});

export const usersList = gun.get('freely-associating-players');

// Cache for user names to avoid repeated Gun lookups
export const userNamesCache = writable<Record<string, string>>({});

/**
 * Get user name from Gun with caching
 * @param userId The user ID to look up
 * @returns Promise that resolves to the user's display name
 */
export async function getUserName(userId: string): Promise<string> {
	// Look up from freely-associating-players first
	try {
		const pubUser: any = await new Promise((resolve) => {
			usersList.get(userId).once(resolve);
		});

		if (pubUser && pubUser.name) {
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
			return alias;
		}
	} catch (error) {
		console.log(`[USER-NAME] Could not fetch alias for user ${userId}:`, error);
	}

	// Fallback to truncated ID
	return userId.substring(0, 8) + '...';
}

export let user = gun.user().recall({ sessionStorage: true });

// Current User's username
export const username = writable('');
export const userpub = writable('');

// Check if user is already authenticated after recall
if (typeof window !== 'undefined') {
	// Small delay to allow recall to complete
	setTimeout(() => {
		if (user?.is?.pub) {
			console.log('[RECALL] User already authenticated after recall:', user.is.alias);
			// Set the user state
			username.set(user.is.alias);
			userpub.set(user.is.pub);
			usersList.get(user.is?.pub).put({
				name: user.is.alias,
				lastSeen: Date.now()
			});

			// Load the data since gun.on('auth') won't fire for recalled sessions
			manifest();
		} else {
			console.log('[RECALL] No authenticated user found after recall');
		}
	}, 100);
}

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

/*
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
*/

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

export function persistCapacities() {
	const userCapacitiesValue = get(userCapacities);
	if (userCapacitiesValue) {
		console.log('[PERSIST] Starting capacities persistence...');
		console.log('[PERSIST] Capacities count:', Object.keys(userCapacitiesValue).length);

		try {
			// First create a deep clone to avoid any reactivity issues
			const capacitiesClone = structuredClone(userCapacitiesValue);

			// Log the data being saved
			console.log('[PERSIST] Saving capacities:', capacitiesClone);

			// Then serialize to JSON
			const capacitiesJson = JSON.stringify(capacitiesClone);
			console.log('[PERSIST] Serialized capacities length:', capacitiesJson.length);

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
		}
	}
}

export function persistRecipientShares() {
	// This will need to be implemented with access to the recipient shares
	// For now, we'll handle this in the calculations module
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

			// Reset loading flag after tree is loaded
			isLoadingTree.set(false);

			// Trigger recalculation by setting the tree again after loading flag is cleared
			// This ensures SOGF is calculated from the current tree state
			setTimeout(() => {
				console.log('[MANIFEST] Triggering tree recalculation after load');
				userTree.set(parsedTree);
			}, 50);
		} else if (user.is?.pub) {
			// No valid tree data found, create a new one
			console.log('[MANIFEST] No valid tree data found, creating initial tree');
			const newTree = createRootNode(user.is.pub, user.is?.alias || 'My Root', user.is.pub);
			user.get('tree').put(JSON.stringify(newTree));
			userTree.set(newTree);

			// Reset loading flag after tree is loaded
			isLoadingTree.set(false);
		} else {
			console.log('[MANIFEST] No tree data found and no user to create one');
			// Reset loading flag even if no tree
			isLoadingTree.set(false);
		}
	});

	// Load SOGF - needed for network synchronization even though it will be recalculated
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
			console.log('[MANIFEST] Capacities data:', capacitiesData);
			const validatedCapacities = parseCapacities(capacitiesData);
			console.log('[MANIFEST] Loaded capacities count:', Object.keys(validatedCapacities).length);
			console.log('[MANIFEST] Post-Validation capacities:', validatedCapacities);
			userCapacities.set(validatedCapacities);
		} else {
			console.log('[MANIFEST] No capacities data found');
			// Initialize with empty object if no data exists
			/*
			const emptyCapacities = {};
			userCapacities.set(emptyCapacities);

			// Persist the empty capacities to Gun so they're available on recall
			console.log('[MANIFEST] Persisting initial empty capacities to Gun...');
			user.get('capacities').put(JSON.stringify(emptyCapacities), (ack: { err?: any }) => {
				if (ack.err) {
					console.error('[MANIFEST] Error saving initial capacities to Gun:', ack.err);
				} else {
					console.log('[MANIFEST] Initial empty capacities saved to Gun');
				}
			});
			*/
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
 * Convert a string duration like '30m', '2h', '1d', '1w', '3mo' to milliseconds
 * @param {string} durationStr
 * @returns {number} Duration in milliseconds
 */
function parseDuration(durationStr: string): number {
	const durationRegex = /^(\d+)(s|m|h|d|w|mo)$/i;
	const match = durationStr.match(durationRegex);

	if (!match) {
		throw new Error(`Invalid duration format: '${durationStr}'`);
	}

	const value = parseInt(match[1], 10);
	const unit = match[2].toLowerCase();

	const unitToMs: Record<string, number> = {
		s: 1000,
		m: 60 * 1000,
		h: 60 * 60 * 1000,
		d: 24 * 60 * 60 * 1000,
		w: 7 * 24 * 60 * 60 * 1000,
		mo: 30 * 24 * 60 * 60 * 1000 // Approximate month as 30 days
	};

	return value * unitToMs[unit];
}

/**
 * Prune users from the usersList who haven't been seen within the specified duration
 * @param {string} inactivityThreshold - Duration string like '30m', '1h', '2d', etc.
 */
export function clearUsersList(inactivityThreshold: string = '30m') {
	let inactivityThresholdMs: number;
	try {
		inactivityThresholdMs = parseDuration(inactivityThreshold);
	} catch (error) {
		console.error(`[PRUNE] ${error}`);
		return;
	}

	console.log(`[PRUNE] Starting to prune users inactive for more than ${inactivityThreshold}...`);

	const thresholdTime = Date.now() - inactivityThresholdMs;

	usersList.once((data: any) => {
		if (data) {
			const userIds = Object.keys(data);
			console.log(`[PRUNE] Found ${userIds.length} users to check`);

			let prunedCount = 0;
			let checkedCount = 0;

			userIds.forEach((userId) => {
				usersList.get(userId).once((userData: any) => {
					checkedCount++;

					if (userData && userData.lastSeen) {
						const lastSeen = userData.lastSeen;

						if (lastSeen < thresholdTime) {
							usersList.get(userId).put(null, (ack: any) => {
								if (ack.err) {
									console.error(`[PRUNE] Error removing inactive user ${userId}:`, ack.err);
								} else {
									console.log(
										`[PRUNE] Removed inactive user ${userId} (last seen: ${new Date(lastSeen).toLocaleString()})`
									);
									prunedCount++;
								}
							});
						} else {
							console.log(
								`[PRUNE] User ${userId} is active (last seen: ${new Date(lastSeen).toLocaleString()})`
							);
						}
					} else {
						console.log(`[PRUNE] User ${userId} has no lastSeen data, removing...`);
						usersList.get(userId).put(null, (ack: any) => {
							if (ack.err) {
								console.error(`[PRUNE] Error removing user without lastSeen ${userId}:`, ack.err);
							} else {
								console.log(`[PRUNE] Removed user without lastSeen data: ${userId}`);
								prunedCount++;
							}
						});
					}

					if (checkedCount === userIds.length) {
						console.log(
							`[PRUNE] Pruning complete. Checked ${checkedCount} users, removed ${prunedCount} inactive users.`
						);
					}
				});
			});
		} else {
			console.log('[PRUNE] No users found to prune');
		}
	});
}

// Expose to window for debugging
if (typeof window !== 'undefined') {
	(window as any).clearUsersList = clearUsersList;
	console.log('[DEBUG] clearUsersList function exposed to window');
}
