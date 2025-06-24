import { get } from 'svelte/store';
import { gun, user, username, userpub, userIds, userNamesCache, usersList } from './gun.svelte';
import {
	contributors,
	mutualContributors,
	recognitionCache,
	networkCapacities,
	networkCapacityShares,
	userTree,
	userCapacities,
	isLoadingTree,
	isLoadingCapacities
} from './core.svelte';
import {
	userDesiredComposeFrom,
	userDesiredComposeInto,
	networkDesiredComposeFrom,
	networkDesiredComposeInto,
} from '$lib/state/protocol/compose.svelte';
import type { CapacitiesCollection } from '$lib/schema';
import { recalculateFromTree } from './calculations.svelte';
import { parseCapacities, parseTree } from '$lib/validation';

/**
 * Subscribe to our own tree data to detect external changes
 * Only updates local store if the incoming data differs from current state
 * Also handles initial tree creation when no data exists
/**
 * Subscribe to our own tree data to detect external changes
 * Only updates local store if the incoming data differs from current state
 */
export function subscribeToOwnTree() {
	const ourId = user.is?.pub;
	if (!ourId) {
		console.log('[NETWORK] Cannot subscribe to own tree - not authenticated');
		return;
	}

	console.log('[NETWORK] Setting up subscription to own tree data');

	// Subscribe to changes in our own tree
	user.get('tree').on((treeData: any) => {
		if (!treeData) {
			console.log('[NETWORK] No tree data in subscription, skipping');
			return;
		}

		console.log('[NETWORK] Received tree update from network');

		// Set loading flag to prevent persistence loop
		isLoadingTree.set(true);

		// Parse the incoming tree data
		const incomingTree = parseTree(treeData);
		if (!incomingTree) {
			console.log('[NETWORK] Failed to parse incoming tree data');
			isLoadingTree.set(false);
			return;
		}

		// Get current tree state
		const currentTree = get(userTree);
		if (!currentTree) {
			console.log('[NETWORK] No current tree, updating with network data');
			userTree.set(incomingTree);
			isLoadingTree.set(false);
			recalculateFromTree();
			return;
		}

		// Compare trees to see if they're different
		const currentTreeJson = JSON.stringify(currentTree);
		const incomingTreeJson = JSON.stringify(incomingTree);

		if (currentTreeJson === incomingTreeJson) {
			console.log('[NETWORK] Incoming tree matches current tree, ignoring update');
			isLoadingTree.set(false);
			return;
		}

		console.log('[NETWORK] Tree data changed, updating local store');
		console.log('[NETWORK] Current tree children count:', currentTree.children.length);
		console.log('[NETWORK] Incoming tree children count:', incomingTree.children.length);

		// Update the local tree with the new data
		userTree.set(incomingTree);

		// Clear loading flag after update
		isLoadingTree.set(false);
		recalculateFromTree();
	});
}

/**
 * Subscribe to our own capacities data
 */
export function subscribeToOwnCapacities() {
	const ourId = user.is?.pub;
	if (!ourId) {
		console.log('[NETWORK] Cannot subscribe to own capacities - not authenticated');
		return;
	}

	console.log('[NETWORK] Setting up subscription to own capacities data');

	user.get('capacities').on((capacitiesData: any) => {
		if (!capacitiesData) {
			console.log('[NETWORK] No capacities data found');
			isLoadingCapacities.set(false);
			return;
		}

		console.log('[NETWORK] Received capacities update from network');

		// Set loading flag to prevent persistence loop
		isLoadingCapacities.set(true);

		// Parse and validate capacities
		const validatedCapacities = parseCapacities(capacitiesData);
		if (!validatedCapacities) {
			console.error('[NETWORK] Failed to validate own capacities data');
			isLoadingCapacities.set(false);
			return;
		}

		// Get current capacities state
		const currentCapacities = get(userCapacities);

		// Compare to avoid unnecessary updates
		if (
			currentCapacities &&
			JSON.stringify(currentCapacities) === JSON.stringify(validatedCapacities)
		) {
			console.log('[NETWORK] Incoming capacities match current capacities, ignoring update');
			isLoadingCapacities.set(false);
			return;
		}

		console.log('[NETWORK] Capacities data changed, updating local store');
		console.log('[NETWORK] Loaded capacities count:', Object.keys(validatedCapacities).length);

		userCapacities.set(validatedCapacities);
		isLoadingCapacities.set(false);
	});
}

/**
 * Subscribe to our own desired compose-from data
 */
export function subscribeToOwnDesiredComposeFrom() {
	const ourId = user.is?.pub;
	if (!ourId) {
		console.log('[NETWORK] Cannot subscribe to own desired compose-from - not authenticated');
		return;
	}

	console.log('[NETWORK] Setting up subscription to own desired compose-from data');

	user.get('desiredComposeFrom').on((composeFromData: any) => {
		if (!composeFromData) {
			console.log('[NETWORK] No desired compose-from data found');
			return;
		}

		console.log('[NETWORK] Received desired compose-from update from network');

		try {
			// Parse and validate the compose-from data
			let parsedComposeFrom: Record<string, Record<string, number>>;
			if (typeof composeFromData === 'string') {
				parsedComposeFrom = JSON.parse(composeFromData);
			} else {
				parsedComposeFrom = composeFromData;
			}

			// Validate the structure - now expecting absolute units (positive numbers)
			const validatedComposeFrom: Record<string, Record<string, number>> = {};
			Object.entries(parsedComposeFrom).forEach(([capacityId, desires]) => {
				if (typeof desires === 'object' && desires !== null) {
					const validatedDesires: Record<string, number> = {};
					Object.entries(desires).forEach(([targetCapacityId, amount]) => {
						// Accept any positive number (absolute units, not limited to 0-1 range)
						if (typeof amount === 'number' && amount >= 0) {
							validatedDesires[targetCapacityId] = amount;
						}
					});
					if (Object.keys(validatedDesires).length > 0) {
						validatedComposeFrom[capacityId] = validatedDesires;
					}
				}
			});

			// Get current state and compare
			const currentComposeFrom = get(userDesiredComposeFrom);
			if (
				currentComposeFrom &&
				JSON.stringify(currentComposeFrom) === JSON.stringify(validatedComposeFrom)
			) {
				console.log(
					'[NETWORK] Incoming compose-from matches current compose-from, ignoring update'
				);
				return;
			}

			console.log('[NETWORK] Desired compose-from data changed, updating local store');
			userDesiredComposeFrom.set(validatedComposeFrom);
		} catch (error) {
			console.error('[NETWORK] Error parsing own desired compose-from:', error);
		}
	});
}

/**
 * Subscribe to our own desired compose-into data
 */
export function subscribeToOwnDesiredComposeInto() {
	const ourId = user.is?.pub;
	if (!ourId) {
		console.log('[NETWORK] Cannot subscribe to own desired compose-into - not authenticated');
		return;
	}

	console.log('[NETWORK] Setting up subscription to own desired compose-into data');

	user.get('desiredComposeInto').on((composeIntoData: any) => {
		if (!composeIntoData) {
			console.log('[NETWORK] No desired compose-into data found');
			return;
		}

		console.log('[NETWORK] Received desired compose-into update from network');

		try {
			// Parse and validate the compose-into data
			let parsedComposeInto: Record<string, Record<string, number>>;
			if (typeof composeIntoData === 'string') {
				parsedComposeInto = JSON.parse(composeIntoData);
			} else {
				parsedComposeInto = composeIntoData;
			}

			// Validate the structure - now expecting absolute units (positive numbers)
			const validatedComposeInto: Record<string, Record<string, number>> = {};
			Object.entries(parsedComposeInto).forEach(([capacityId, desires]) => {
				if (typeof desires === 'object' && desires !== null) {
					const validatedDesires: Record<string, number> = {};
					Object.entries(desires).forEach(([targetCapacityId, amount]) => {
						// Accept any positive number (absolute units, not limited to 0-1 range)
						if (typeof amount === 'number' && amount >= 0) {
							validatedDesires[targetCapacityId] = amount;
						}
					});
					if (Object.keys(validatedDesires).length > 0) {
						validatedComposeInto[capacityId] = validatedDesires;
					}
				}
			});

			// Get current state and compare
			const currentComposeInto = get(userDesiredComposeInto);
			if (
				currentComposeInto &&
				JSON.stringify(currentComposeInto) === JSON.stringify(validatedComposeInto)
			) {
				console.log(
					'[NETWORK] Incoming compose-into matches current compose-into, ignoring update'
				);
				return;
			}

			console.log('[NETWORK] Desired compose-into data changed, updating local store');
			userDesiredComposeInto.set(validatedComposeInto);
		} catch (error) {
			console.error('[NETWORK] Error parsing own desired compose-into:', error);
		}
	});
}

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
			// console.log(`[USERS] User added/updated: ${userId}`, userData);
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
		// console.log(`[USERS] Updating userIds store with ${allUserIds.length} users`);
		userIds.set(allUserIds);
	});
}

/**
 * Initialize all user data subscriptions
 */
export function initializeUserDataSubscriptions() {
	if (!user.is?.pub) {
		console.log('[NETWORK] Cannot initialize subscriptions - not authenticated');
		return;
	}

	console.log('[NETWORK] Initializing all user data subscriptions');
	setupUsersListSubscription();
	// Set up all subscriptions (each handles its own loading state)
	subscribeToOwnTree();
	subscribeToOwnCapacities();
	// Note: SOGF is calculated from tree, not loaded from storage
	subscribeToOwnDesiredComposeFrom();
	subscribeToOwnDesiredComposeInto();
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

	// Log the updated cache and force reactivity check
	const updatedCache = get(recognitionCache);

	console.log(`[NETWORK] Cache after network update from ${contributorId}:`, updatedCache);
}

function setupTimeoutProtection(contributorId: string, dataType: string) {
	let hasReceived = false;
	const timeout = setTimeout(() => {
		if (!hasReceived) {
			console.log(`[NETWORK] Timeout waiting for ${dataType} from ${contributorId}`);
		}
	}, 10000); // 10 second timeout

	return {
		markReceived: () => {
			hasReceived = true;
			clearTimeout(timeout);
		},
		hasReceived
	};
}

/**
 * Set up network subscriptions for contributor's SOGF
 * @param contributorId The ID of the contributor to subscribe to
 */
export function subscribeToContributorSOGF(contributorId: string) {
	console.log(`[NETWORK] Setting up subscription to ${contributorId}'s SOGF`);

	const contributorSogf = gun.get(`~${contributorId}`).get('sogf');

	// Subscribe to changes
	contributorSogf.on((sogfData: any) => {
		if (!sogfData) return;

		console.log(`[NETWORK] Received SOGF update from ${contributorId}`);

		// Get our user ID
		const ourId = user.is?.pub;
		if (!ourId) return;

		// Extract our share from their SOGF
		const theirShare = sogfData[ourId] || 0;

		// Check if this share matches our current cache
		const currentCache = get(recognitionCache);
		const existingEntry = currentCache[contributorId];
		const isUnchanged = existingEntry && existingEntry.theirShare === theirShare;

		if (isUnchanged) {
			console.log(
				`[NETWORK] Ignoring duplicate SOGF update from contributor ${contributorId}: share=${theirShare}`
			);
			return;
		}

		// Update our cache with their share for us
		updateTheirShareFromNetwork(contributorId, theirShare);
	});
}

export function subscribeToContributorCapacities(contributorId: string) {
	console.log(`[NETWORK] Setting up subscription to ${contributorId}'s capacities`);

	const contributorCapacities = gun.get(`~${contributorId}`).get('capacities');

	// Subscribe to changes
	contributorCapacities.on((capacitiesData: any) => {
		if (!capacitiesData) return;

		console.log(`[NETWORK] Received capacities update from ${contributorId}:`, capacitiesData);

		// Get our user ID
		const ourId = user.is?.pub;
		if (!ourId) return;

		// Parse and validate the capacities data
		const validatedCapacities = parseCapacities(capacitiesData);
		if (!validatedCapacities || Object.keys(validatedCapacities).length === 0) {
			console.error('[NETWORK] Failed to validate capacities from contributor:', contributorId);
			return;
		}

		// Check if these capacities match our current network state
		const currentNetworkCapacities = get(networkCapacities)[contributorId] || {};
		const isUnchanged =
			JSON.stringify(validatedCapacities) === JSON.stringify(currentNetworkCapacities);

		if (isUnchanged) {
			console.log(
				`[NETWORK] Ignoring duplicate capacities update for contributor ${contributorId}`
			);
			return;
		}

		// Update the networkCapacities store with this contributor's validated capacities
		networkCapacities.update((currentNetworkCapacities) => {
			const updated = { ...currentNetworkCapacities };
			updated[contributorId] = validatedCapacities;
			console.log(
				`[NETWORK] Updated networkCapacities for ${contributorId}: added ${Object.keys(validatedCapacities).length} capacities`
			);
			return updated;
		});
	});
}

/**
 * Subscribe to our capacity shares from a contributor
 * @param contributorId The contributor to subscribe to
 */
export function subscribeToContributorCapacityShares(contributorId: string) {
	const ourId = user.is?.pub;
	if (!ourId) {
		console.log('[NETWORK] Cannot subscribe to capacity shares - not authenticated');
		return;
	}

	console.log(
		`[NETWORK] Setting up capacity shares subscription for contributor: ${contributorId}`
	);

	// Subscribe to our capacity shares from this contributor
	gun
		.get(`~${contributorId}`)
		.get('capacityShares')
		.get(ourId)
		.on((shares: any) => {
			if (!shares) {
				console.log(`[NETWORK] No capacity shares from contributor ${contributorId}`);
				networkCapacityShares.update((current) => {
					const { [contributorId]: _, ...rest } = current;
					return rest;
				});
				return;
			}

			try {
				// Parse and validate the shares
				let parsedShares: Record<string, number>;
				if (typeof shares === 'string') {
					parsedShares = JSON.parse(shares);
				} else {
					parsedShares = shares;
				}

				// Validate that all shares are numbers between 0 and 1
				const validatedShares: Record<string, number> = {};
				Object.entries(parsedShares).forEach(([capacityId, share]) => {
					if (typeof share === 'number' && share >= 0 && share <= 1) {
						validatedShares[capacityId] = share;
					} else {
						console.warn(`[NETWORK] Invalid share value for capacity ${capacityId}:`, share);
					}
				});

				// Check if these shares match our current network state
				const currentNetworkShares = get(networkCapacityShares)[contributorId] || {};
				const isUnchanged =
					JSON.stringify(validatedShares) === JSON.stringify(currentNetworkShares);

				if (isUnchanged) {
					console.log(`[NETWORK] Ignoring duplicate update for contributor ${contributorId}`);
					return;
				}

				console.log(
					`[NETWORK] Received new capacity shares from contributor ${contributorId}:`,
					validatedShares
				);

				// Update the store with the validated shares
				networkCapacityShares.update((current) => ({
					...current,
					[contributorId]: validatedShares
				}));
			} catch (error) {
				console.error(
					`[NETWORK] Error parsing/validating capacity shares from contributor ${contributorId}:`,
					error
				);
			}
		});
}

/**
 * Subscribe to a contributor's desired compose-from (they enhance their capacity from ours)
 * @param contributorId The contributor to subscribe to
 */
export function subscribeToContributorDesiredComposeFrom(contributorId: string) {
	console.log(`[NETWORK] Setting up subscription to ${contributorId}'s desired compose-from`);

	const contributorCompositions = gun.get(`~${contributorId}`).get('desiredComposeFrom');

	// Subscribe to changes
	contributorCompositions.on((compositionsData: any) => {
		if (!compositionsData) {
			console.log(`[NETWORK] No desired compositions from contributor ${contributorId}`);
			networkDesiredComposeFrom.update((current) => {
				const { [contributorId]: _, ...rest } = current;
				return rest;
			});
			return;
		}

		console.log(
			`[NETWORK] Received desired compositions update from ${contributorId}:`,
			compositionsData
		);

		try {
			// Parse and validate the compositions data
			let parsedCompositions: Record<string, Record<string, number>>;
			if (typeof compositionsData === 'string') {
				parsedCompositions = JSON.parse(compositionsData);
			} else {
				parsedCompositions = compositionsData;
			}

			// Validate the structure - now expecting absolute units (positive numbers)
			const validatedCompositions: Record<string, Record<string, number>> = {};
			Object.entries(parsedCompositions).forEach(([capacityId, desires]) => {
				if (typeof desires === 'object' && desires !== null) {
					const validatedDesires: Record<string, number> = {};
					Object.entries(desires).forEach(([targetCapacityId, amount]) => {
						// Accept any positive number (absolute units, not limited to 0-1 range)
						if (typeof amount === 'number' && amount >= 0) {
							validatedDesires[targetCapacityId] = amount;
						}
					});
					if (Object.keys(validatedDesires).length > 0) {
						validatedCompositions[capacityId] = validatedDesires;
					}
				}
			});

			// Check if these compositions match our current network state
			const currentNetworkCompositions = get(networkDesiredComposeFrom)[contributorId] || {};
			const isUnchanged =
				JSON.stringify(validatedCompositions) === JSON.stringify(currentNetworkCompositions);

			if (isUnchanged) {
				console.log(
					`[NETWORK] Ignoring duplicate desired compositions update for contributor ${contributorId}`
				);
				return;
			}

			// Update the networkDesiredComposeFrom store
			networkDesiredComposeFrom.update((current) => ({
				...current,
				[contributorId]: validatedCompositions
			}));

			console.log(
				`[NETWORK] Updated desired compositions for ${contributorId}: ${Object.keys(validatedCompositions).length} capacities with composition desires`
			);
		} catch (error) {
			console.error(
				`[NETWORK] Error parsing/validating desired compositions from contributor ${contributorId}:`,
				error
			);
		}
	});
}

/**
 * Subscribe to a contributor's desired compose-into (they want to enhance our capacities)
 * @param contributorId The contributor to subscribe to
 */
export function subscribeToContributorDesiredComposeInto(contributorId: string) {
	console.log(`[NETWORK] Setting up subscription to ${contributorId}'s desired compose-into`);

	const contributorComposeInto = gun.get(`~${contributorId}`).get('desiredComposeInto');

	// Subscribe to changes
	contributorComposeInto.on((composeIntoData: any) => {
		if (!composeIntoData) {
			console.log(`[NETWORK] No desired compose-into from contributor ${contributorId}`);
			networkDesiredComposeInto.update((current) => {
				const { [contributorId]: _, ...rest } = current;
				return rest;
			});
			return;
		}

		console.log(
			`[NETWORK] Received desired compose-into update from ${contributorId}:`,
			composeIntoData
		);

		try {
			// Parse and validate the compose-into data
			let parsedComposeInto: Record<string, Record<string, number>>;
			if (typeof composeIntoData === 'string') {
				parsedComposeInto = JSON.parse(composeIntoData);
			} else {
				parsedComposeInto = composeIntoData;
			}

			// Validate the structure - now expecting absolute units (positive numbers)
			const validatedComposeInto: Record<string, Record<string, number>> = {};
			Object.entries(parsedComposeInto).forEach(([capacityId, desires]) => {
				if (typeof desires === 'object' && desires !== null) {
					const validatedDesires: Record<string, number> = {};
					Object.entries(desires).forEach(([targetCapacityId, amount]) => {
						// Accept any positive number (absolute units, not limited to 0-1 range)
						if (typeof amount === 'number' && amount >= 0) {
							validatedDesires[targetCapacityId] = amount;
						}
					});
					if (Object.keys(validatedDesires).length > 0) {
						validatedComposeInto[capacityId] = validatedDesires;
					}
				}
			});

			// Check if these compose-into desires match our current network state
			const currentNetworkComposeInto = get(networkDesiredComposeInto)[contributorId] || {};
			const isUnchanged =
				JSON.stringify(validatedComposeInto) === JSON.stringify(currentNetworkComposeInto);

			if (isUnchanged) {
				console.log(
					`[NETWORK] Ignoring duplicate desired compose-into update for contributor ${contributorId}`
				);
				return;
			}

			// Update the networkDesiredComposeInto store
			networkDesiredComposeInto.update((current) => ({
				...current,
				[contributorId]: validatedComposeInto
			}));

			console.log(
				`[NETWORK] Updated desired compose-into for ${contributorId}: ${Object.keys(validatedComposeInto).length} capacities with compose-into desires`
			);
		} catch (error) {
			console.error(
				`[NETWORK] Error parsing/validating desired compose-into from contributor ${contributorId}:`,
					error
				);
			}
		});
}

// Watch for changes to contributors and subscribe to get their SOGF data
contributors.subscribe((allContributors) => {
	if (!allContributors.length) {
		console.log('[NETWORK] No contributors to subscribe to');
		return;
	}

	// Only run this if we're authenticated
	if (!user.is?.pub) {
		console.log('[NETWORK] Cannot subscribe to contributors - not authenticated');
		return;
	}

	console.log(
		`[NETWORK] Contributors changed, now have ${allContributors.length}, setting up subscriptions`
	);

	// Subscribe to ALL contributors to get their current SOGF data
	// We always re-subscribe to ensure we have the most current theirShare values
	// This is especially important when contributors are re-added after being removed
	allContributors.forEach((contributorId) => {
		try {
			console.log(`[NETWORK] Setting up subscription for contributor: ${contributorId}`);
			subscribeToContributorSOGF(contributorId);
		} catch (error) {
			console.error(`[NETWORK] Error subscribing to contributor ${contributorId}:`, error);
		}
	});
});

// Watch for changes to mutual contributors and subscribe to get their capacity data and shares
mutualContributors.subscribe((currentMutualContributors) => {
	if (!currentMutualContributors.length) {
		// If no mutual contributors, clear all network capacities, shares, and compositions
		networkCapacities.set({});
		networkCapacityShares.set({});
		networkDesiredComposeFrom.set({});
		networkDesiredComposeInto.set({});
		return;
	}

	// Only run this if we're authenticated
	if (!user.is?.pub) {
		console.log('[NETWORK] Cannot subscribe to mutual contributors - not authenticated');
		return;
	}

	console.log(
		`[NETWORK] Mutual contributors changed, now have ${currentMutualContributors.length}, setting up subscriptions`
	);

	// Clean up capacities from contributors who are no longer mutual contributors
	networkCapacities.update((currentNetworkCapacities) => {
		if (!currentNetworkCapacities) return {};

		const cleaned: Record<string, CapacitiesCollection> = {};
		Object.entries(currentNetworkCapacities).forEach(([contributorId, capacities]) => {
			// Keep capacities from current mutual contributors only
			if (currentMutualContributors.includes(contributorId)) {
				cleaned[contributorId] = capacities;
			}
		});

		console.log(
			`[NETWORK] Cleaned network capacities: kept ${Object.keys(cleaned).length} contributors from ${
				Object.keys(currentNetworkCapacities).length
			} total`
		);
		return cleaned;
	});

	// Clean up shares from contributors who are no longer mutual contributors
	networkCapacityShares.update((current) => {
		const cleaned: Record<string, Record<string, number>> = {};
		Object.entries(current).forEach(([contributorId, shares]) => {
			// Keep shares from current mutual contributors only
			if (currentMutualContributors.includes(contributorId)) {
				cleaned[contributorId] = shares;
			}
		});

		console.log(
			`[NETWORK] Cleaned capacity shares: kept ${Object.keys(cleaned).length} contributors from ${
				Object.keys(current).length
			} total`
		);
		return cleaned;
	});

	// Clean up desired compositions from contributors who are no longer mutual contributors
	networkDesiredComposeFrom.update((current) => {
		const cleaned: Record<string, Record<string, Record<string, number>>> = {};
		Object.entries(current).forEach(([contributorId, compositions]) => {
			// Keep compositions from current mutual contributors only
			if (currentMutualContributors.includes(contributorId)) {
				cleaned[contributorId] = compositions;
			}
		});

		console.log(
			`[NETWORK] Cleaned desired compositions: kept ${Object.keys(cleaned).length} contributors from ${
				Object.keys(current).length
			} total`
		);
		return cleaned;
	});

	// Clean up desired compose-into from contributors who are no longer mutual contributors
	networkDesiredComposeInto.update((current) => {
		const cleaned: Record<string, Record<string, Record<string, number>>> = {};
		Object.entries(current).forEach(([contributorId, composeInto]) => {
			// Keep compose-into from current mutual contributors only
			if (currentMutualContributors.includes(contributorId)) {
				cleaned[contributorId] = composeInto;
			}
		});

		console.log(
			`[NETWORK] Cleaned desired compose-into: kept ${Object.keys(cleaned).length} contributors from ${
				Object.keys(current).length
			} total`
		);
		return cleaned;
	});

	// Subscribe to mutual contributors for capacity data, shares, and both composition directions
	currentMutualContributors.forEach((contributorId) => {
		try {
			console.log(`[NETWORK] Setting up subscriptions for mutual contributor: ${contributorId}`);
			// Set up capacity subscription
			subscribeToContributorCapacities(contributorId);
			// Set up capacity shares subscription
			subscribeToContributorCapacityShares(contributorId);
			// Set up desired compose-from subscription (they enhance their capacity from ours)
			subscribeToContributorDesiredComposeFrom(contributorId);
			// Set up desired compose-into subscription (they enhance our capacity using theirs)
			subscribeToContributorDesiredComposeInto(contributorId);
		} catch (error) {
			console.error(`[NETWORK] Error subscribing to mutual contributor ${contributorId}:`, error);
		}
	});
});

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
