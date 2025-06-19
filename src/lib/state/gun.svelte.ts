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
import { parseShareMap, parseRecognitionCache } from '$lib/validation';
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
	contributorCapacityShares,
	userDesiredComposeFrom,
	userDesiredComposeInto
} from './core.svelte';
import { initializeUserDataSubscriptions } from './network.svelte';

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

export const GUN = Gun;

// Database
export const gun = new Gun({
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

export let user: any;

if (typeof window !== 'undefined') {
	user = gun.user().recall({ sessionStorage: true });
} else {
	user = { _: { sea: null }, is: null };
}

// SEA.throw = true (do not use this in production)

// Current User's username
export const username = writable('');
export const userpub = writable('');

export const usersList = gun.get('freely-associating-players');

// Cache for user names to avoid repeated Gun lookups
export const userNamesCache = writable<Record<string, string>>({});

// Store for tracking all user IDs
export const userIds = writable<string[]>([]);

export async function getUserName(userId: string) {
	// First check the reactive cache
	const cache = get(userNamesCache);
	if (cache[userId]) {
		return cache[userId];
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

// Check if user is already authenticated after recall
if (typeof window !== 'undefined') {
	const checkAuth = async () => {
		try {
			// Set authenticating state
			isAuthenticating.set(true);

			// Wait for a small delay to allow recall to complete
			await new Promise((resolve) => setTimeout(resolve, 100));

			// Check for SEA keys first as it's the most reliable way to verify authentication
			if (user._.sea && user.is?.alias) {
				console.log('[RECALL] User authenticated via SEA check:', user.is.alias);

				// Get the actual alias and pub from Gun storage instead of trusting user.is
				user.get('alias').once((actualAlias: any) => {
					user.get('pub').once((actualPub: any) => {
						const aliasToUse = actualAlias || user.is.alias;
						const pubToUse = actualPub || user.is.pub;
						console.log('[RECALL] Using alias from storage:', aliasToUse);
						console.log('[RECALL] Using pub from storage:', pubToUse?.slice(0, 20) + '...');
						username.set(aliasToUse);
						userpub.set(pubToUse);
						usersList.get(pubToUse).put({
							name: aliasToUse,
							lastSeen: Date.now()
						});
					});
				});

				// Load the data since gun.on('auth') won't fire for recalled sessions
				initializeUserDataSubscriptions();
				return;
			}

			// Fallback check for user.is.pub if SEA check fails
			if (user?.is?.pub && user.is?.alias) {
				console.log('[RECALL] User authenticated via pub check:', user.is.alias);

				// Get the actual alias and pub from Gun storage instead of trusting user.is
				user.get('alias').once((actualAlias: any) => {
					user.get('pub').once((actualPub: any) => {
						const aliasToUse = actualAlias || user.is.alias;
						const pubToUse = actualPub || user.is.pub;
						console.log('[RECALL] Using alias from storage (pub fallback):', aliasToUse);
						console.log(
							'[RECALL] Using pub from storage (pub fallback):',
							pubToUse?.slice(0, 20) + '...'
						);
						username.set(aliasToUse);
						userpub.set(pubToUse);
						usersList.get(pubToUse).put({
							name: aliasToUse,
							lastSeen: Date.now()
						});
					});
				});

				// Load the data since gun.on('auth') won't fire for recalled sessions
				initializeUserDataSubscriptions();
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

			// Get the actual alias and pub from Gun storage instead of trusting user.is
			user.get('alias').once((actualAlias: any) => {
				user.get('pub').once((actualPub: any) => {
					const aliasToUse = actualAlias || user.is.alias;
					const pubToUse = actualPub || user.is.pub;
					console.log('[AUTH] Using alias from storage:', aliasToUse);
					console.log('[AUTH] Using pub from storage:', pubToUse?.slice(0, 20) + '...');
					username.set(aliasToUse);
					userpub.set(pubToUse);
					usersList.get(pubToUse).put({
						name: aliasToUse,
						lastSeen: Date.now()
					});

					console.log(`signed in as ${aliasToUse}`);
					console.log(`userPub: ${pubToUse}`);

					// Load existing user data
					initializeUserDataSubscriptions();
				});
			});

			// Remove these unreliable console.log statements
			// console.log(`signed in as ${user.is.alias}`);
			// console.log(`userPub: ${user.is?.pub}`);

			// Don't call manifest() here anymore - it's called inside the callback above
			return;
		}

		if (user.is.alias) {
			console.log('[AUTH] User authenticated via alias check:', user.is.alias);

			// Get the actual alias and pub from Gun storage instead of trusting user.is
			user.get('alias').once((actualAlias: any) => {
				user.get('pub').once((actualPub: any) => {
					const aliasToUse = actualAlias || user.is.alias;
					const pubToUse = actualPub || user.is.pub;
					console.log('[AUTH] Using alias from storage (fallback):', aliasToUse);
					console.log('[AUTH] Using pub from storage (fallback):', pubToUse?.slice(0, 20) + '...');
					username.set(aliasToUse);
					userpub.set(pubToUse);
					usersList.get(pubToUse).put({
						name: aliasToUse,
						lastSeen: Date.now()
					});

					console.log(`signed in as ${aliasToUse} (fallback)`);
					console.log(`userPub: ${pubToUse} (fallback)`);

					// Load existing user data
					initializeUserDataSubscriptions();
				});
			});

			// Remove these unreliable console.log statements
			// console.log(`signed in as ${user.is.alias}`);
			// console.log(`userPub: ${user.is?.pub}`);

			// Don't call initializeUserDataSubscriptions() here anymore - it's called inside the callback above
			return;
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

/**
 * Persist user's desired compose-from to Gun
 */
export function persistUserDesiredComposeFrom() {
	const userDesiredComposeFromValue = get(userDesiredComposeFrom);
	if (!userDesiredComposeFromValue || Object.keys(userDesiredComposeFromValue).length === 0) {
		console.log('[PERSIST] No user desired compose-from data to persist');
		return;
	}

	console.log('[PERSIST] Starting user desired compose-from persistence...');
	console.log('[PERSIST] User desired compose-from:', userDesiredComposeFromValue);

	try {
		// Create a deep clone to avoid reactivity issues
		const composeFromClone = structuredClone(userDesiredComposeFromValue);

		// Serialize to JSON
		const composeFromJson = JSON.stringify(composeFromClone);
		console.log('[PERSIST] Serialized user desired compose-from length:', composeFromJson.length);

		// Store in Gun under the expected path that network subscribers use
		user.get('desiredComposeFrom').put(composeFromJson, (ack: { err?: any }) => {
			if (ack.err) {
				console.error('[PERSIST] Error saving user desired compose-from to Gun:', ack.err);
			} else {
				console.log('[PERSIST] User desired compose-from successfully saved to Gun');
			}
		});
	} catch (error) {
		console.error('[PERSIST] Error serializing user desired compose-from:', error);
	}
}

/**
 * Persist user's desired compose-into to Gun
 */
export function persistUserDesiredComposeInto() {
	const userDesiredComposeIntoValue = get(userDesiredComposeInto);
	if (!userDesiredComposeIntoValue || Object.keys(userDesiredComposeIntoValue).length === 0) {
		console.log('[PERSIST] No user desired compose-into data to persist');
		return;
	}

	console.log('[PERSIST] Starting user desired compose-into persistence...');
	console.log('[PERSIST] User desired compose-into:', userDesiredComposeIntoValue);

	try {
		// Create a deep clone to avoid reactivity issues
		const composeIntoClone = structuredClone(userDesiredComposeIntoValue);

		// Serialize to JSON
		const composeIntoJson = JSON.stringify(composeIntoClone);
		console.log('[PERSIST] Serialized user desired compose-into length:', composeIntoJson.length);

		// Store in Gun under the expected path that network subscribers use
		user.get('desiredComposeInto').put(composeIntoJson, (ack: { err?: any }) => {
			if (ack.err) {
				console.error('[PERSIST] Error saving user desired compose-into to Gun:', ack.err);
			} else {
				console.log('[PERSIST] User desired compose-into successfully saved to Gun');
			}
		});
	} catch (error) {
		console.error('[PERSIST] Error serializing user desired compose-into:', error);
	}
}

// Monitoring: Check connectivity with gun.back('opt.peers') and WebRTC stats:
async function monitorWebRTC(pc: any) {
	const report = await pc.getStats();
	for (let dict of report.values()) {
		console.log(`${dict.type}: id=${dict.id}, timestamp=${dict.timestamp}`);
	}
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

/**
 * Change the name of the user's tree root node
 * @param {string} newName - The new name for the tree
 */
export function changeTreeName(newName: string) {
	console.log(`[TREE-NAME] Changing tree name to: ${newName}`);

	const currentTree = get(userTree);
	if (!currentTree) {
		console.error('[TREE-NAME] No tree found to rename');
		return;
	}

	// Update the tree name
	const updatedTree = {
		...currentTree,
		name: newName
	};

	// Update the store
	userTree.set(updatedTree);

	// Persist the changes
	persistTree();

	console.log(`[TREE-NAME] Tree name changed to: ${newName}`);
}

/**
 * Fix corrupted names in the usersList where names were saved as public keys
 * This function checks each user in the usersList and corrects names that match their pubkey
 */
export function fixCorruptedUserListNames() {
	console.log('[USERS-FIX] Starting to fix corrupted names in usersList...');

	// Get current usersList data
	usersList.once((usersData: any) => {
		if (!usersData) {
			console.log('[USERS-FIX] No users data found');
			return;
		}

		const userIds = Object.keys(usersData).filter((id) => id !== '_');
		console.log(`[USERS-FIX] Found ${userIds.length} users to check for corrupted names`);

		let checkedCount = 0;
		let fixedCount = 0;

		userIds.forEach((userId) => {
			const userData = usersData[userId];

			if (!userData || !userData.name) {
				console.log(`[USERS-FIX] User ${userId} has no name data, skipping`);
				checkedCount++;
				return;
			}

			// Check if the name is corrupted (equals the userId/pubkey)
			const isCorrupted = userData.name === userId;

			if (isCorrupted) {
				console.log(
					`[USERS-FIX] Found corrupted name for user ${userId.substring(0, 20)}... (name equals pubkey)`
				);

				// Try to get the correct alias from their protected space
				gun
					.get(`~${userId}`)
					.get('alias')
					.once((alias: any) => {
						checkedCount++;

						if (alias && typeof alias === 'string' && alias !== userId) {
							console.log(
								`[USERS-FIX] Fixing user ${userId.substring(0, 20)}... with correct alias: ${alias}`
							);

							// Update the usersList with the correct name
							usersList.get(userId).put(
								{
									...userData,
									name: alias,
									lastSeen: Date.now(), // Update timestamp to show it was fixed
									fixed: true // Mark as fixed
								},
								(ack: any) => {
									if (ack.err) {
										console.error(`[USERS-FIX] Error fixing user ${userId}:`, ack.err);
									} else {
										console.log(`[USERS-FIX] Successfully fixed user ${userId} -> ${alias}`);
										fixedCount++;
									}

									// Log completion when all users have been processed
									if (checkedCount === userIds.length) {
										console.log(
											`[USERS-FIX] Completed fixing corrupted names. Checked ${checkedCount} users, fixed ${fixedCount} corrupted names.`
										);
									}
								}
							);
						} else {
							console.log(
								`[USERS-FIX] Could not get valid alias for user ${userId.substring(0, 20)}..., keeping current name`
							);

							// Log completion when all users have been processed
							if (checkedCount === userIds.length) {
								console.log(
									`[USERS-FIX] Completed fixing corrupted names. Checked ${checkedCount} users, fixed ${fixedCount} corrupted names.`
								);
							}
						}
					});
			} else {
				console.log(
					`[USERS-FIX] User ${userId.substring(0, 20)}... has valid name: ${userData.name}`
				);
				checkedCount++;

				// Log completion when all users have been processed
				if (checkedCount === userIds.length) {
					console.log(
						`[USERS-FIX] Completed fixing corrupted names. Checked ${checkedCount} users, fixed ${fixedCount} corrupted names.`
					);
				}
			}
		});

		// Handle case where no users need checking
		if (userIds.length === 0) {
			console.log('[USERS-FIX] No users to check');
		}
	});
}

// Expose to window for debugging
if (typeof window !== 'undefined') {
	(window as any).clearUsersList = clearUsersList;
	(window as any).changeTreeName = changeTreeName;
	(window as any).fixCorruptedUserListNames = fixCorruptedUserListNames;
	console.log(
		'[DEBUG] clearUsersList, changeTreeName, and fixCorruptedUserListNames functions exposed to window'
	);
}

// secrets would be given to us?
// We dont need writeable what we need is to be able to store the secrets in our private user-space only readable by the user (encrypted)
// use pubkey to decrypt our own user-space? (or does gun do it automatically)

var pair = user._.sea;

export function saveSecret(pubkey: string, secret: string) {
	// encrypt our own user-space
	user.get('secrets').get(pubkey).put(SEA.encrypt(secret, pair));
}

export function getSecret(pubkey: string) {
	// decrypt from our own user-space
	return user
		.get('secrets')
		.get(pubkey)
		.once((secret: string) => {
			return SEA.decrypt(secret, pair);
		});
}

interface Message {
	who: string;
	what: string;
	when: number;
	whopub?: string; // Match ChatMessage interface
	path?: string;
}

export function sendMessage(path: string, pubkey: string[], message: Message) {
	// pubKey -> messages
	user.get('messages').get(pubkey).set(message);
}

export function readMessages(fromPubKey: string, path: string, pubkeys: string[]) {
	// ourKey -> messages
	gun
		.get(`~${fromPubKey}`)
		.get('messages')
		.get(get(userpub))
		.map()
		.once((message: any) => {
			return SEA.decrypt(getSecret(fromPubKey), pair);
		});
}
