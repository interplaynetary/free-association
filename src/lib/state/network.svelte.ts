import { get } from 'svelte/store';
import { gun, user, userPub, usersList } from './gun.svelte';
import {
	userPubKeys,
	userNamesOrAliasesCache,
	userAliasesCache,
	resolveToPublicKey
} from '$lib/state/users.svelte';
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
import { userContacts, isLoadingContacts } from './users.svelte';
import {
	userDesiredComposeFrom,
	userDesiredComposeInto,
	networkDesiredComposeFrom,
	networkDesiredComposeInto
} from '$lib/state/compose.svelte';
import type { CapacitiesCollection } from '$lib/schema';
import { ContactsCollectionSchema } from '$lib/schema';
import { recalculateFromTree } from './calculations.svelte';
import { parseCapacities, parseTree, parseUserComposition } from '$lib/validation';

/**
 * Subscribe to our own tree data to detect external changes
 * Only updates local store if the incoming data differs from current state
 * Also handles initial tree creation when no data exists
/**
 * Subscribe to our own tree data to detect external changes
 * Only updates local store if the incoming data differs from current state
 */
export function subscribeToOwnTree() {
	let ourId: string;
	try {
		ourId = get(userPub);
		if (!ourId) {
			console.log('[NETWORK] Cannot subscribe to own tree - not authenticated');
			return;
		}
	} catch (error) {
		console.log('[NETWORK] Cannot subscribe to own tree - userPub not initialized');
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
	let ourId: string;
	try {
		ourId = get(userPub);
		if (!ourId) {
			console.log('[NETWORK] Cannot subscribe to own capacities - not authenticated');
			return;
		}
	} catch (error) {
		console.log('[NETWORK] Cannot subscribe to own capacities - userPub not initialized');
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
	let ourId: string;
	try {
		ourId = get(userPub);
		if (!ourId) {
			console.log('[NETWORK] Cannot subscribe to own desired compose-from - not authenticated');
			return;
		}
	} catch (error) {
		console.log('[NETWORK] Cannot subscribe to own desired compose-from - userPub not initialized');
		return;
	}

	console.log('[NETWORK] Setting up subscription to own desired compose-from data');

	user.get('desiredComposeFrom').on((composeFromData: any) => {
		if (!composeFromData) {
			console.log('[NETWORK] No desired compose-from data found');
			return;
		}

		console.log('[NETWORK] Received desired compose-from update from network');

		// Parse and validate using Zod schema
		const validatedComposeFrom = parseUserComposition(composeFromData);

		// Get current state and compare
		const currentComposeFrom = get(userDesiredComposeFrom);
		if (
			currentComposeFrom &&
			JSON.stringify(currentComposeFrom) === JSON.stringify(validatedComposeFrom)
		) {
			console.log('[NETWORK] Incoming compose-from matches current compose-from, ignoring update');
			return;
		}

		console.log('[NETWORK] Desired compose-from data changed, updating local store');
		userDesiredComposeFrom.set(validatedComposeFrom);
	});
}

/**
 * Subscribe to our own desired compose-into data
 */
export function subscribeToOwnDesiredComposeInto() {
	let ourId: string;
	try {
		ourId = get(userPub);
		if (!ourId) {
			console.log('[NETWORK] Cannot subscribe to own desired compose-into - not authenticated');
			return;
		}
	} catch (error) {
		console.log('[NETWORK] Cannot subscribe to own desired compose-into - userPub not initialized');
		return;
	}

	console.log('[NETWORK] Setting up subscription to own desired compose-into data');

	user.get('desiredComposeInto').on((composeIntoData: any) => {
		if (!composeIntoData) {
			console.log('[NETWORK] No desired compose-into data found');
			return;
		}

		console.log('[NETWORK] Received desired compose-into update from network');

		// Parse and validate using Zod schema
		const validatedComposeInto = parseUserComposition(composeIntoData);

		// Get current state and compare
		const currentComposeInto = get(userDesiredComposeInto);
		if (
			currentComposeInto &&
			JSON.stringify(currentComposeInto) === JSON.stringify(validatedComposeInto)
		) {
			console.log('[NETWORK] Incoming compose-into matches current compose-into, ignoring update');
			return;
		}

		console.log('[NETWORK] Desired compose-into data changed, updating local store');
		userDesiredComposeInto.set(validatedComposeInto);
	});
}

/**
 * Subscribe to our own contacts data
 */
export function subscribeToOwnContacts() {
	let ourId: string;
	try {
		ourId = get(userPub);
		if (!ourId) {
			console.log('[NETWORK] Cannot subscribe to own contacts - not authenticated');
			return;
		}
	} catch (error) {
		console.log('[NETWORK] Cannot subscribe to own contacts - userPub not initialized');
		return;
	}

	console.log('[NETWORK] Setting up subscription to own contacts data');

	user.get('contacts').on((contactsData: any) => {
		if (!contactsData) {
			console.log('[NETWORK] No contacts data found');
			userContacts.set({});
			isLoadingContacts.set(false);
			return;
		}

		console.log('[NETWORK] Received contacts update from network');

		// Set loading flag to prevent persistence loop
		isLoadingContacts.set(true);

		try {
			// Parse JSON string if needed
			const parsedData = typeof contactsData === 'string' ? JSON.parse(contactsData) : contactsData;

			// Parse and validate contacts
			const validatedContacts = ContactsCollectionSchema.parse(parsedData);
			console.log('[NETWORK] Validated contacts:', validatedContacts);

			// Get current contacts state
			const currentContacts = get(userContacts);

			// Compare to avoid unnecessary updates
			if (JSON.stringify(currentContacts) === JSON.stringify(validatedContacts)) {
				console.log('[NETWORK] Incoming contacts match current contacts, ignoring update');
				isLoadingContacts.set(false);
				return;
			}

			console.log('[NETWORK] Contacts data changed, updating local store');
			console.log('[NETWORK] Loaded contacts count:', Object.keys(validatedContacts).length);

			userContacts.set(validatedContacts);
		} catch (error) {
			console.error('[NETWORK] Error loading contacts:', error);
			// On error, don't update the store
		} finally {
			isLoadingContacts.set(false);
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
	usersList.map().on((userData: any, pubKey: string) => {
		//console.log(`[USERS] User update: ${userId}`, userData);

		if (!pubKey || pubKey === '_') return; // Skip invalid keys

		if (userData === null || userData === undefined) {
			// User was removed from usersList (they went offline or left the shared space)
			//console.log(`[USERS] User removed from usersList: ${userId}`);
			currentUsers.delete(pubKey);

			// Note: We intentionally don't remove from userNamesCache here
			// because the cache serves a different purpose (performance optimization)
			// and this user might still be referenced in our tree as a contributor
		} else {
			// User was added or updated
			// console.log(`[USERS] User added/updated: ${userId}`, userData);
			currentUsers.set(pubKey, userData);

			// Get the user's alias and update cache
			const userName = userData.name;
			if (userName && typeof userName === 'string') {
				// Update userNamesCache with the name from usersList
				userAliasesCache.update((cache) => ({
					...cache,
					[pubKey]: userName
				}));
			} else {
				// Try to get alias from user's protected space using Gun's user system
				gun
					.user(pubKey) // Use Gun's user system
					.get('alias')
					.once((alias: any) => {
						if (alias && typeof alias === 'string') {
							console.log(`[USERS] Got alias from protected space for ${pubKey}: ${alias}`);
							userAliasesCache.update((cache) => ({
								...cache,
								[pubKey]: alias
							}));
						}
					});
			}
		}

		// Update userIds store with current user list
		const allUserIds = Array.from(currentUsers.keys());
		// console.log(`[USERS] Updating userIds store with ${allUserIds.length} users`);
		userPubKeys.set(allUserIds);
	});
}

/**
 * Initialize all user data subscriptions
 */
export function initializeUserDataSubscriptions() {
	try {
		if (!userPub || !get(userPub)) {
			console.log('[NETWORK] Cannot initialize subscriptions - not authenticated');
			return;
		}
	} catch (error) {
		console.log('[NETWORK] Cannot initialize subscriptions - userPub not initialized');
		return;
	}

	console.log('[NETWORK] Initializing all user data subscriptions');
	setupUsersListSubscription();
	// Set up all subscriptions (each handles its own loading state)
	subscribeToOwnTree();
	subscribeToOwnCapacities();
	subscribeToOwnDesiredComposeFrom();
	subscribeToOwnDesiredComposeInto();
	subscribeToOwnContacts();
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
 * @param contributorId The ID of the contributor to subscribe to (can be contactId or pubKey)
 */
export function subscribeToContributorSOGF(contributorId: string) {
	console.log(`[NETWORK] Setting up subscription to ${contributorId}'s SOGF`);

	// Resolve contact ID to public key if needed
	const pubKey = resolveToPublicKey(contributorId);
	if (!pubKey) {
		console.warn(
			`[NETWORK] Cannot subscribe to SOGF for ${contributorId}: no public key available`
		);
		return;
	}

	const contributorSogf = gun.user(pubKey).get('sogf'); // Use Gun's user system

	// Subscribe to changes
	contributorSogf.on((sogfData: any) => {
		if (!sogfData) return;

		console.log(`[NETWORK] Received SOGF update from ${contributorId}`);

		// Get our user ID
		let ourId: string;
		try {
			ourId = get(userPub);
			if (!ourId) return;
		} catch (error) {
			console.log(`[NETWORK] Cannot get userPub in SOGF subscription for ${contributorId}`);
			return;
		}

		// Extract our share from their SOGF
		const theirShare = sogfData[ourId] || 0;

		// Check if this share matches our current cache
		const currentCache = get(recognitionCache);
		const existingEntry = currentCache[contributorId];
		const isUnchanged = existingEntry && existingEntry.theirShare === theirShare;

		if (isUnchanged) {
			/*console.log(
				`[NETWORK] Ignoring duplicate SOGF update from contributor ${contributorId}: share=${theirShare}`
			);*/
			return;
		}

		// Update our cache with their share for us
		updateTheirShareFromNetwork(contributorId, theirShare);
	});
}

export function subscribeToContributorCapacities(contributorId: string) {
	console.log(`[NETWORK] Setting up subscription to ${contributorId}'s capacities`);

	// Resolve contact ID to public key if needed
	const pubKey = resolveToPublicKey(contributorId);
	if (!pubKey) {
		console.warn(
			`[NETWORK] Cannot subscribe to capacities for ${contributorId}: no public key available`
		);
		return;
	}

	const contributorCapacities = gun.user(pubKey).get('capacities'); // Use Gun's user system

	// Subscribe to changes
	contributorCapacities.on((capacitiesData: any) => {
		if (!capacitiesData) return;

		console.log(`[NETWORK] Received capacities update from ${contributorId}:`, capacitiesData);

		// Get our user ID
		let ourId: string;
		try {
			ourId = get(userPub);
			if (!ourId) return;
		} catch (error) {
			console.log(`[NETWORK] Cannot get userPub in capacities subscription for ${contributorId}`);
			return;
		}

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
			/*console.log(
				`[NETWORK] Ignoring duplicate capacities update for contributor ${contributorId}`
			);*/
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
 * @param contributorId The contributor to subscribe to (can be contactId or pubKey)
 */
export function subscribeToContributorCapacityShares(contributorId: string) {
	let ourId: string;
	try {
		ourId = get(userPub);
		if (!ourId) {
			console.log('[NETWORK] Cannot subscribe to capacity shares - not authenticated');
			return;
		}
	} catch (error) {
		console.log('[NETWORK] Cannot subscribe to capacity shares - userPub not initialized');
		return;
	}

	console.log(
		`[NETWORK] Setting up capacity shares subscription for contributor: ${contributorId}`
	);

	// Resolve contact ID to public key if needed
	const pubKey = resolveToPublicKey(contributorId);
	if (!pubKey) {
		console.warn(
			`[NETWORK] Cannot subscribe to capacity shares for ${contributorId}: no public key available`
		);
		return;
	}

	// Subscribe to our capacity shares from this contributor
	gun
		.user(pubKey) // Use Gun's user system
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
					//console.log(`[NETWORK] Ignoring duplicate update for contributor ${contributorId}`);
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
 * @param contributorId The contributor to subscribe to (can be contactId or pubKey)
 */
export function subscribeToContributorDesiredComposeFrom(contributorId: string) {
	console.log(`[NETWORK] Setting up subscription to ${contributorId}'s desired compose-from`);

	// Resolve contact ID to public key if needed
	const pubKey = resolveToPublicKey(contributorId);
	if (!pubKey) {
		console.warn(
			`[NETWORK] Cannot subscribe to desired compose-from for ${contributorId}: no public key available`
		);
		return;
	}

	const contributorCompositions = gun.user(pubKey).get('desiredComposeFrom'); // Use Gun's user system

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

		// Parse and validate using Zod schema
		const validatedCompositions = parseUserComposition(compositionsData);

		// Check if these compositions match our current network state
		const currentNetworkCompositions = get(networkDesiredComposeFrom)[contributorId] || {};
		const isUnchanged =
			JSON.stringify(validatedCompositions) === JSON.stringify(currentNetworkCompositions);

		if (isUnchanged) {
			/*console.log(
				`[NETWORK] Ignoring duplicate desired compositions update for contributor ${contributorId}`
			);*/
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
	});
}

/**
 * Subscribe to a contributor's desired compose-into (they want to enhance our capacities)
 * @param contributorId The contributor to subscribe to (can be contactId or pubKey)
 */
export function subscribeToContributorDesiredComposeInto(contributorId: string) {
	console.log(`[NETWORK] Setting up subscription to ${contributorId}'s desired compose-into`);

	// Resolve contact ID to public key if needed
	const pubKey = resolveToPublicKey(contributorId);
	if (!pubKey) {
		console.warn(
			`[NETWORK] Cannot subscribe to desired compose-into for ${contributorId}: no public key available`
		);
		return;
	}

	const contributorComposeInto = gun.user(pubKey).get('desiredComposeInto'); // Use Gun's user system

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

		// Parse and validate using Zod schema
		const validatedComposeInto = parseUserComposition(composeIntoData);

		// Check if these compose-into desires match our current network state
		const currentNetworkComposeInto = get(networkDesiredComposeInto)[contributorId] || {};
		const isUnchanged =
			JSON.stringify(validatedComposeInto) === JSON.stringify(currentNetworkComposeInto);

		if (isUnchanged) {
			/*console.log(
				`[NETWORK] Ignoring duplicate desired compose-into update for contributor ${contributorId}`
			);*/
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
	});
}

/**
 * Generic subscription manager for handling delta-based subscription updates
 */
class SubscriptionManager {
	private activeSubscriptions: Set<string>;
	private subscriptionType: string;

	constructor(subscriptionType: string) {
		this.activeSubscriptions = new Set();
		this.subscriptionType = subscriptionType;
	}

	/**
	 * Update subscriptions based on new contributor list
	 */
	updateSubscriptions(
		newContributors: string[],
		subscribeFn: (contributorId: string) => void,
		onAdd?: (contributorId: string) => void,
		onRemove?: (contributorId: string) => void
	) {
		if (!newContributors.length) {
			console.log(`[NETWORK] No ${this.subscriptionType} contributors to subscribe to`);
			this.activeSubscriptions.clear();
			return;
		}

		console.log(
			`[NETWORK] ${this.subscriptionType} contributors changed, now have ${newContributors.length}, managing subscriptions`
		);

		// Calculate deltas
		const newSet = new Set(newContributors);
		const toAdd = newContributors.filter((id) => !this.activeSubscriptions.has(id));
		const toRemove = Array.from(this.activeSubscriptions).filter((id) => !newSet.has(id));

		// Handle removals
		toRemove.forEach((contributorId) => {
			console.log(`[NETWORK] Removing ${this.subscriptionType} subscription for: ${contributorId}`);
			this.activeSubscriptions.delete(contributorId);
			onRemove?.(contributorId);
		});

		// Handle additions
		toAdd.forEach((contributorId) => {
			try {
				console.log(`[NETWORK] Adding ${this.subscriptionType} subscription for: ${contributorId}`);
				subscribeFn(contributorId);
				this.activeSubscriptions.add(contributorId);
				onAdd?.(contributorId);
			} catch (error) {
				console.error(
					`[NETWORK] Error subscribing to ${this.subscriptionType} contributor ${contributorId}:`,
					error
				);
			}
		});

		console.log(
			`[NETWORK] ${this.subscriptionType} subscriptions: +${toAdd.length} -${toRemove.length} (total: ${this.activeSubscriptions.size})`
		);
	}

	has(contributorId: string): boolean {
		return this.activeSubscriptions.has(contributorId);
	}

	add(contributorId: string): void {
		this.activeSubscriptions.add(contributorId);
	}

	get size(): number {
		return this.activeSubscriptions.size;
	}
}

/**
 * Helper to clean up multiple stores by filtering out removed contributors
 */
function cleanupNetworkStores(
	removedContributors: string[],
	currentContributors: string[],
	stores: Array<{ store: any; name: string }>
) {
	if (removedContributors.length === 0) return;

	console.log(
		`[NETWORK] Cleaning up data for removed contributors: ${removedContributors.join(', ')}`
	);

	stores.forEach(({ store, name }) => {
		store.update((current: Record<string, any>) => {
			const cleaned: Record<string, any> = {};
			Object.entries(current).forEach(([contributorId, data]) => {
				if (currentContributors.includes(contributorId)) {
					cleaned[contributorId] = data;
				}
			});
			console.log(`[NETWORK] Cleaned ${name}: kept ${Object.keys(cleaned).length} contributors`);
			return cleaned;
		});
	});
}

/**
 * Set up all mutual contributor subscriptions for a single contributor
 */
function setupMutualContributorSubscriptions(contributorId: string) {
	subscribeToContributorCapacities(contributorId);
	subscribeToContributorCapacityShares(contributorId);
	subscribeToContributorDesiredComposeFrom(contributorId);
	subscribeToContributorDesiredComposeInto(contributorId);
}

// Create subscription managers
const sogfManager = new SubscriptionManager('SOGF');
const mutualManager = new SubscriptionManager('mutual');

// Watch for changes to contributors and subscribe to get their SOGF data
contributors.subscribe((allContributors) => {
	// Only run this if we're authenticated
	try {
		if (!userPub || !get(userPub)) {
			console.log('[NETWORK] Cannot subscribe to contributors - not authenticated');
			return;
		}
	} catch (error) {
		console.log('[NETWORK] Cannot subscribe to contributors - userPub not initialized');
		return;
	}

	sogfManager.updateSubscriptions(allContributors, subscribeToContributorSOGF);
});

// Watch for changes to mutual contributors and subscribe to get their capacity data and shares
mutualContributors.subscribe((currentMutualContributors) => {
	// Only run this if we're authenticated
	try {
		if (!userPub || !get(userPub)) {
			console.log('[NETWORK] Cannot subscribe to mutual contributors - not authenticated');
			return;
		}
	} catch (error) {
		console.log('[NETWORK] Cannot subscribe to mutual contributors - userPub not initialized');
		return;
	}

	if (!currentMutualContributors.length) {
		// Clear all network stores
		networkCapacities.set({});
		networkCapacityShares.set({});
		networkDesiredComposeFrom.set({});
		networkDesiredComposeInto.set({});
		return;
	}

	// Define network stores for cleanup
	const networkStores = [
		{ store: networkCapacities, name: 'networkCapacities' },
		{ store: networkCapacityShares, name: 'networkCapacityShares' },
		{ store: networkDesiredComposeFrom, name: 'networkDesiredComposeFrom' },
		{ store: networkDesiredComposeInto, name: 'networkDesiredComposeInto' }
	];

	mutualManager.updateSubscriptions(
		currentMutualContributors,
		setupMutualContributorSubscriptions,
		undefined, // onAdd callback not needed
		(contributorId) =>
			cleanupNetworkStores([contributorId], currentMutualContributors, networkStores)
	);
});

/**
 * Debug function to manually trigger subscriptions for all contributors
 */
export function debugTriggerSubscriptions() {
	console.log('[DEBUG] Manually triggering subscriptions for all contributors');
	const allContributors = get(contributors);

	allContributors.forEach((contributorId) => {
		if (!sogfManager.has(contributorId)) {
			console.log(`[DEBUG] Setting up subscription for: ${contributorId}`);
			subscribeToContributorSOGF(contributorId);
			sogfManager.add(contributorId);
		} else {
			console.log(`[DEBUG] Already subscribed to: ${contributorId}`);
		}
	});

	console.log(`[DEBUG] Active subscriptions: ${sogfManager.size}/${allContributors.length}`);
}
