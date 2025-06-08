import Gun from 'gun';
import SEA from 'gun/sea';
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
	isLoadingSogf,
	isLoadingRecognitionCache,
	providerShares,
	contributorCapacityShares
} from './core.svelte';

if (typeof Gun.SEA === 'undefined') {
	Gun.SEA = SEA;
}

// I wonder if it would be required 

/*
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
*/

export const GUN = Gun

// Database
export const gun = Gun({
	peers: [
		//'http://localhost:8765/gun',
		'https://gun-manhattan.herokuapp.com/gun',
		'https://peer.wallie.io/gun',
		'https://gun.defucc.me/gun'
	],
	localStorage: false
	//radisk: false
});

// Authentication state store
export const isAuthenticating = writable(true);

export let user = gun.user().recall({ sessionStorage: true });

// SEA.throw = true

// Current User's username
export const username = writable('');
export const userpub = writable('');

export const usersList = gun.get('freely-associating-players');

// Cache for user names to avoid repeated Gun lookups
export const userNamesCache = writable<Record<string, string>>({});

// Store for tracking all user IDs
export const userIds = writable<string[]>([]);

// Centralized reactive subscription to usersList
function setupUsersListSubscription() {
	if (typeof window === 'undefined') return; // Only run in browser

	console.log('[USERS] Setting up centralized usersList subscription');

	// Track current users to detect additions/removals
	const currentUsers = new Map<string, any>();

	// Subscribe to all changes in the usersList
	usersList.map().on((userData: any, userId: string) => {
		console.log(`[USERS] User update: ${userId}`, userData);

		if (!userId || userId === '_') return; // Skip invalid keys

		if (userData === null || userData === undefined) {
			// User was removed
			console.log(`[USERS] User removed: ${userId}`);
			currentUsers.delete(userId);

			// Update userNamesCache to remove this user
			userNamesCache.update((cache) => {
				const { [userId]: _, ...rest } = cache;
				return rest;
			});
		} else {
			// User was added or updated
			console.log(`[USERS] User added/updated: ${userId}`, userData);
			currentUsers.set(userId, userData);

			// Get the user's alias and update cache
			const userName = userData.name;
			if (userName && typeof userName === 'string') {
				// Update userNamesCache with the name from usersList
				userNamesCache.update((cache) => ({
					...cache,
					[userId]: userName
				}));
			} else {
				// Try to get alias from user's protected space
				gun
					.get(`~${userId}`)
					.get('alias')
					.once((alias: any) => {
						if (alias && typeof alias === 'string') {
							console.log(`[USERS] Got alias from protected space for ${userId}: ${alias}`);
							userNamesCache.update((cache) => ({
								...cache,
								[userId]: alias
							}));
						}
					});
			}
		}

		// Update userIds store with current user list
		const allUserIds = Array.from(currentUsers.keys());
		console.log(`[USERS] Updating userIds store with ${allUserIds.length} users`);
		userIds.set(allUserIds);
	});
}

export async function getUserName(userId: string) {
	// First check the reactive cache
	const cache = get(userNamesCache);
	if (cache[userId]) {
		return cache[userId];
	}

	// If not in cache, try to get it and update cache
	// Look up from freely-associating-players first
	try {
		const name = usersList
			.get(userId)
			.get('name')
			.once((data: string) => {
				return data;
			});

		if (name && typeof name === 'string') {
			// Update cache
			userNamesCache.update((cache) => ({
				...cache,
				[userId]: name
			}));
			return name;
		}
	} catch (error) {
		console.log(
			`[USER-NAME] Could not fetch from freely-associating-players for ${userId}:`,
			error
		);
	}

	// If not found, try the user's protected space
	try {
		const alias = gun
			.get(`~${userId}`)
			.get('alias')
			.once((data: any) => {
				return data;
			});
		if (alias && typeof alias === 'string') {
			// Update cache
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
	return fallbackName;
}

// Initialize the centralized users subscription
if (typeof window !== 'undefined') {
	setupUsersListSubscription();
}

// Check if user is already authenticated after recall
if (typeof window !== 'undefined') {
	const checkAuth = async () => {
		try {
			// Set authenticating state
			isAuthenticating.set(true);

			// Wait for a small delay to allow recall to complete
			await new Promise((resolve) => setTimeout(resolve, 100));

			// Check for SEA keys first as it's the most reliable way to verify authentication
			if (user._.sea) {
				console.log('[RECALL] User authenticated via SEA check:', user.is.alias);
				// Set the user state
				username.set(user.is.alias);
				userpub.set(user.is.pub);
				usersList.get(user.is?.pub).put({
					name: user.is.alias,
					lastSeen: Date.now()
				});

				// Load the data since gun.on('auth') won't fire for recalled sessions
				manifest();
				return;
			}

			// Fallback check for user.is.pub if SEA check fails
			if (user?.is?.pub) {
				console.log('[RECALL] User authenticated via pub check:', user.is.alias);
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
				// Clear the stores to ensure consistent state
				username.set('');
				userpub.set('');
			}
		} catch (error) {
			console.error('[RECALL] Error during authentication check:', error);
			// Clear the stores on error
			username.set('');
			userpub.set('');
		} finally {
			// Clear authenticating state
			isAuthenticating.set(false);
		}
	};

	// Start the authentication check
	checkAuth();
}

gun.on('auth', async () => {
	try {
		// Set authenticating state
		isAuthenticating.set(true);

		// Check for SEA keys first as it's the most reliable way to verify authentication
		if (user._.sea) {
			console.log('[AUTH] User authenticated via SEA check:', user.is.alias);
			username.set(user.is.alias);
			userpub.set(user.is?.pub);
			usersList.get(user.is?.pub).put({
				name: user.is.alias,
				lastSeen: Date.now()
			});

			console.log(`signed in as ${user.is.alias}`);
			console.log(`userPub: ${user.is?.pub}`);

			// Load existing user data
			manifest();
			return;
		}

		if (user.is.alias) {
			console.log('[AUTH] User authenticated via alias check:', user.is.alias);
			username.set(user.is.alias);
			userpub.set(user.is?.pub);
			usersList.get(user.is?.pub).put({
				name: user.is.alias,
				lastSeen: Date.now()
			});

			console.log(`signed in as ${user.is.alias}`);
			console.log(`userPub: ${user.is?.pub}`);

			// Load existing user data
			manifest();
		} else {
			throw new Error('Authentication failed - no alias found');
		}
	} catch (error) {
		console.error('[AUTH] Error during authentication:', error);
		// Clear the stores on error
		username.set('');
		userpub.set('');
	} finally {
		// Clear authenticating state
		isAuthenticating.set(false);
	}
});

export function login(username: string, password: string) {
	user.auth(username, password, ({ err }: { err: any }) => err && alert(err));
}

export function signup(username: string, password: string) {
	gun.get(`~@${username}`).once(
		(data: any) => {
			if (data) {
				console.log('[SIGNUP] checking alias user data', data);
				alert('Username already taken');
				return;
			}

			user.create(username, password, ({ err }: { err: any }) => {
				if (err) {
					alert(err);
				} else {
					login(username, password);
				}
			});
		},
		{ wait: 2000 }
	);
}

export async function signout() {
	user.leave();
	while (user._.sea != null) {
		await new Promise(requestAnimationFrame);
	}
	username.set('');
	userpub.set('');
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

		console.log('[PERSIST] Full persistence complete');
	} catch (error) {
		console.error('[PERSIST] Critical error persisting data to Gun:', error);
	}
}

export function persistTree() {
	// Don't persist while loading
	if (get(isLoadingTree)) {
		console.log('[PERSIST] Skipping tree persistence because tree is being loaded');
		return;
	}

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
	// Don't persist while loading
	if (get(isLoadingSogf)) {
		console.log('[PERSIST] Skipping SOGF persistence because SOGF is being loaded');
		return;
	}

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
	// Don't persist while loading
	if (get(isLoadingCapacities)) {
		console.log('[PERSIST] Skipping capacities persistence because capacities are being loaded');
		return;
	}

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

/**
 * Persist contributor capacity shares to gun
 */
export function persistContributorCapacityShares() {
	const ourId = user.is?.pub;
	if (!ourId) {
		console.log('[PERSIST] No user ID available, cannot persist contributor capacity shares');
		return;
	}

	const shares = get(contributorCapacityShares);
	console.log('[PERSIST] Persisting contributor capacity shares:', shares);

	// For each contributor, store their shares under their path
	Object.entries(shares).forEach(([contributorId, capacityShares]) => {
		// Store under contributorId/capacityShares/{ourId}
		user
			.get('capacityShares')
			.get(contributorId)
			.put(JSON.stringify(capacityShares), (ack: any) => {
				if (ack.err) {
					console.error(
						`[PERSIST] Error persisting capacity shares for contributor ${contributorId}:`,
						ack.err
					);
				} else {
					console.log(
						`[PERSIST] Successfully persisted capacity shares for contributor ${contributorId}`
					);
				}
			});
	});
}

// Monitoring: Check connectivity with gun.back('opt.peers') and WebRTC stats:
async function monitorWebRTC(pc: any) {
	const report = await pc.getStats();
	for (let dict of report.values()) {
		console.log(`${dict.type}: id=${dict.id}, timestamp=${dict.timestamp}`);
	}
}

/**
 * Load the application state from Gun
 */
export function manifest() {
	console.log('[MANIFEST] Loading data from Gun');

	// Set loading flag before loading tree
	isLoadingTree.set(true);

	// Load tree with fallback timeout for distributed sync
	let treeLoaded = false;

	const handleTreeData = (treeData: any, isRetry = false) => {
		const attemptLabel = isRetry ? 'retry' : 'initial';
		console.log(
			`[MANIFEST] Raw tree data from Gun (${attemptLabel}):`,
			typeof treeData === 'string' ? `String of length ${treeData.length}` : 'Object:',
			treeData
		);

		// Process valid data
		if (treeData !== undefined && treeData !== null) {
			const parsedTree = parseTree(treeData);

			if (parsedTree && user.is?.pub) {
				console.log(
					'[MANIFEST] Loaded tree with children count:',
					parsedTree.children?.length || 0
				);
				if (parsedTree.children?.length > 0) {
					console.log('[MANIFEST] First child:', parsedTree.children[0]);
				}
				userTree.set(parsedTree);
				treeLoaded = true;
				isLoadingTree.set(false);

				// Trigger recalculation after load
				setTimeout(() => {
					console.log('[MANIFEST] Triggering tree recalculation after load');
					userTree.set(parsedTree);
				}, 50);
				return;
			}
		}

		// Create new tree only after retry attempt or if we have valid user context
		if (isRetry && user.is?.pub && !treeLoaded) {
			console.log('[MANIFEST] No valid tree data found after retry, creating initial tree');
			const newTree = createRootNode(get(userpub), get(username), get(userpub));
			userTree.set(newTree);
			user.get('tree').put(JSON.stringify(newTree));
			isLoadingTree.set(false);
		} else if (!isRetry && (treeData === undefined || treeData === null)) {
			// First attempt with undefined - wait and retry once
			console.log('[MANIFEST] Tree data undefined, retrying in 2s...');
			setTimeout(() => {
				if (!treeLoaded) {
					user.get('tree').once((retryData: any) => handleTreeData(retryData, true));
				}
			}, 2000);
		} else if (isRetry) {
			console.log('[MANIFEST] No tree data available and no user context');
			isLoadingTree.set(false);
		}
	};

	// Initial tree load attempt
	user.get('tree').once(handleTreeData);

	// Load SOGF - needed for network synchronization even though it will be recalculated // is it truly needed?
	isLoadingSogf.set(true);
	user.get('sogf').once((sogfData: any) => {
		if (sogfData) {
			// Parse SOGF data with validation
			const validatedSogf = parseShareMap(sogfData);
			userSogf.set(validatedSogf);
		}
		isLoadingSogf.set(false);
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
		}

		// Reset loading flag after capacities are loaded
		isLoadingCapacities.set(false);
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
