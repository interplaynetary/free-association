import { get } from 'svelte/store';
import { userTree, nodesMap, userNetworkCapacitiesWithShares } from '$lib/state/core.svelte';
import { userIds, userNamesCache, getUserName } from '$lib/state/gun.svelte';
import { getSubtreeContributorMap, findNodeById } from '$lib/protocol';

// await gun.user(data).get('pub') we can use this instead of .once

// Generic dropdown data provider interface
export interface DropdownItem {
	id: string;
	name: string;
	color?: string;
	metadata?: any;
}

export interface DropdownDataProvider {
	items: DropdownItem[];
	loading: boolean;
	search: (query: string) => void;
	refresh: () => void;
}

// Users data provider for dropdowns
export function createUsersDataProvider(excludeIds: string[] = []): DropdownDataProvider {
	let items = $state<DropdownItem[]>([]);
	let loading = $state(false); // Start as not loading since we use reactive stores
	let currentQuery = $state('');

	function loadUsers(searchQuery: string = '') {
		// Get current reactive data
		const onlineUserIds = get(userIds);
		const namesCache = get(userNamesCache);

		// Show all users we have cached names for (both online and offline)
		const allUserIds = [...new Set([...onlineUserIds, ...Object.keys(namesCache)])];

		console.log(`[USERS-PROVIDER] Loading users with query: "${searchQuery}"`);
		console.log(`[USERS-PROVIDER] Online user IDs:`, onlineUserIds);
		console.log(`[USERS-PROVIDER] All cached user IDs:`, allUserIds);
		console.log(`[USERS-PROVIDER] Names cache:`, namesCache);

		// Filter and create dropdown items
		const filteredItems: DropdownItem[] = [];

		allUserIds.forEach((userId) => {
			// Skip if in excluded list
			if (excludeIds.includes(userId)) return;

			// Get user name from cache
			const userName = namesCache[userId] || userId;

			// Apply search filter if it exists
			if (searchQuery) {
				const searchLower = searchQuery.toLowerCase();
				const nameLower = userName.toLowerCase();
				const idLower = userId.toLowerCase();

				if (!nameLower.includes(searchLower) && !idLower.includes(searchLower)) {
					return;
				}
			}

			// Add to filtered items
			filteredItems.push({
				id: userId,
				name: userName,
				color: undefined // Will be handled by the UI component
			});
		});

		console.log(`[USERS-PROVIDER] Filtered items:`, filteredItems);
		items = filteredItems;
	}

	function search(query: string) {
		currentQuery = query;
		loadUsers(query);
	}

	function refresh() {
		loadUsers(currentQuery);
	}

	// Set up reactive subscriptions to the centralized stores
	$effect(() => {
		// Watch for changes in userIds or userNamesCache
		const onlineUserIds = get(userIds);
		const namesCache = get(userNamesCache);

		// Reload users when either online users or cache changes
		loadUsers(currentQuery);
	});

	// Initial load
	loadUsers();

	return {
		get items() {
			return items;
		},
		get loading() {
			return loading;
		},
		search,
		refresh
	};
}

// Subtrees data provider for dropdowns
export function createSubtreesDataProvider(): DropdownDataProvider {
	let subtreeItems = $state<DropdownItem[]>([]);
	let subtreeLoading = $state(true);
	let subtreeQuery = $state('');

	function loadSubtrees(searchQuery: string = '') {
		subtreeLoading = true;
		subtreeQuery = searchQuery;

		try {
			const tree = get(userTree);
			const nodeMap = get(nodesMap);

			if (!tree || !nodeMap) {
				subtreeItems = [];
				subtreeLoading = false;
				return;
			}

			// Get subtree contributor map
			const subtreeMap = getSubtreeContributorMap(tree);

			// Convert to dropdown items and filter by search query
			const allItems = Object.entries(subtreeMap)
				.map(([subtreeId, contributorRecord]) => {
					// Convert contributorRecord to array of contributor IDs
					const contributors = Object.keys(contributorRecord);

					// Find the node to get its name
					const node = findNodeById(tree, subtreeId);
					const name = node?.name || subtreeId;

					return {
						id: subtreeId,
						name,
						metadata: {
							contributorCount: contributors.length,
							contributors
						}
					};
				})
				.filter((item) => item.metadata.contributorCount > 0); // Only subtrees with contributors

			// Apply search filter
			if (searchQuery.trim()) {
				const query = searchQuery.toLowerCase();
				subtreeItems = allItems.filter((item) => item.name.toLowerCase().includes(query));
			} else {
				subtreeItems = allItems;
			}

			subtreeLoading = false;
		} catch (error) {
			console.error('Error loading subtrees:', error);
			subtreeItems = [];
			subtreeLoading = false;
		}
	}

	// Load initial data
	loadSubtrees();

	return {
		get items() {
			return subtreeItems;
		},
		get loading() {
			return subtreeLoading;
		},
		search: loadSubtrees,
		refresh: () => loadSubtrees(subtreeQuery)
	};
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
	const recipients = get(userNetworkCapacitiesWithShares);
	return recipients[userId] || {};
}

// Capacities data provider for composition dropdowns
export function createCapacitiesDataProvider(excludeCapacityId?: string): DropdownDataProvider {
	let items = $state<DropdownItem[]>([]);
	let loading = $state(false);
	let currentQuery = $state('');
	async function loadCapacities(searchQuery: string = '') {
		try {
			// Get current network capacities
			const networkCapacities = get(userNetworkCapacitiesWithShares);

			if (!networkCapacities) {
				items = [];
				return;
			}

			// Convert to dropdown items and filter by search query
			const itemPromises = Object.entries(networkCapacities)
				.filter(([capacityId]) => capacityId !== excludeCapacityId) // Exclude specified capacity
				.map(async ([capacityId, capacity]) => {
					const ownerName = await getUserName(capacity.owner_id || '');
					return {
						id: capacityId,
						name: `${capacity.emoji || '📦'} ${capacity.name} (${ownerName})`,
						metadata: capacity
					};
				});

			const allItems = await Promise.all(itemPromises);

			// Apply search filter
			if (searchQuery.trim()) {
				const query = searchQuery.toLowerCase();
				items = allItems.filter(
					(item) => item.name.toLowerCase().includes(query) || item.id.toLowerCase().includes(query)
				);
			} else {
				items = allItems;
			}
		} catch (error) {
			console.error('Error loading capacities:', error);
			items = [];
		}
	}

	function search(query: string) {
		currentQuery = query;
		Promise.resolve(loadCapacities(query));
	}

	function refresh() {
		Promise.resolve(loadCapacities(currentQuery));
	}

	// Set up reactive subscription to network capacities
	$effect(() => {
		const networkCapacities = get(userNetworkCapacitiesWithShares);
		// Reload capacities when network data changes
		loadCapacities(currentQuery);
	});

	// Initial load
	loadCapacities();

	return {
		get items() {
			return items;
		},
		get loading() {
			return loading;
		},
		search,
		refresh
	};
}

// All network capacities data provider (for compose-into)
export function createAllNetworkCapacitiesDataProvider(
	excludeCapacityId?: string
): DropdownDataProvider {
	let items = $state<DropdownItem[]>([]);
	let loading = $state(false);
	let currentQuery = $state('');

	async function loadCapacities(searchQuery: string = '') {
		try {
			// Import networkCapacities here to avoid circular dependency
			const { networkCapacities } = await import('$lib/state/core.svelte');
			const allNetworkCapacities = get(networkCapacities);

			if (!allNetworkCapacities) {
				items = [];
				return;
			}

			// Flatten all capacities from all users
			const itemPromises: Promise<DropdownItem>[] = [];

			Object.entries(allNetworkCapacities).forEach(([userId, userCapacities]) => {
				Object.entries(userCapacities).forEach(([capacityId, capacity]) => {
					// Skip excluded capacity
					if (capacityId === excludeCapacityId) return;

					itemPromises.push(
						(async () => {
							const ownerName = await getUserName(userId);
							return {
								id: capacityId,
								name: `${capacity.emoji || '📦'} ${capacity.name} (${ownerName})`,
								metadata: {
									...capacity,
									owner_id: userId,
									provider_id: userId
								}
							};
						})()
					);
				});
			});

			const allItems = await Promise.all(itemPromises);

			// Apply search filter
			if (searchQuery.trim()) {
				const query = searchQuery.toLowerCase();
				items = allItems.filter(
					(item) => item.name.toLowerCase().includes(query) || item.id.toLowerCase().includes(query)
				);
			} else {
				items = allItems;
			}
		} catch (error) {
			console.error('Error loading all network capacities:', error);
			items = [];
		}
	}

	function search(query: string) {
		currentQuery = query;
		Promise.resolve(loadCapacities(query));
	}

	function refresh() {
		Promise.resolve(loadCapacities(currentQuery));
	}

	// Set up reactive subscription to all network capacities
	$effect(() => {
		// Re-import to get current value
		import('$lib/state/core.svelte').then(({ networkCapacities }) => {
			const allNetworkCapacities = get(networkCapacities);
			// Reload capacities when network data changes
			loadCapacities(currentQuery);
		});
	});

	// Initial load
	loadCapacities();

	return {
		get items() {
			return items;
		},
		get loading() {
			return loading;
		},
		search,
		refresh
	};
}
