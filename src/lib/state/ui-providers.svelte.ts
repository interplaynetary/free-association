import { get } from 'svelte/store';
import { userTree, nodesMap, userNetworkCapacitiesWithShares } from './core.svelte';
import { usersList } from './gun.svelte';
import { getSubtreeContributorMap } from '$lib/protocol';

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
	let loading = $state(true);
	let currentQuery = $state('');

	async function loadUsers(searchQuery: string = '') {
		loading = true;
		items = [];

		try {
			// Use a Map to collect unique users and avoid duplicates
			const usersMap = new Map<string, DropdownItem>();

			// Use Gun to fetch users
			usersList.map().once((userData: any, userId: string) => {
				// Skip if this is not a valid user entry or if in excluded list
				if (!userData || !userId || excludeIds.includes(userId)) return;

				// Get user name, fallback to user ID if name not available
				const userName = userData.name || userId;

				// Apply search filter if it exists
				if (searchQuery) {
					const searchLower = searchQuery.toLowerCase();
					const nameLower = userName.toLowerCase();
					const idLower = userId.toLowerCase();

					if (!nameLower.includes(searchLower) && !idLower.includes(searchLower)) {
						return;
					}
				}

				// Add to users map (this automatically handles duplicates)
				usersMap.set(userId, {
					id: userId,
					name: userName,
					color: undefined // Will be handled by the UI component
				});
			});

			// Convert map to array after a short delay to ensure all callbacks have fired
			setTimeout(() => {
				items = Array.from(usersMap.values());
				loading = false;
			}, 500);
		} catch (error) {
			console.error('Error fetching users from Gun:', error);
			loading = false;
		}
	}

	function search(query: string) {
		currentQuery = query;
		loadUsers(query);
	}

	function refresh() {
		loadUsers(currentQuery);
	}

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
			const subtreeMap = getSubtreeContributorMap(tree, nodeMap);

			// Convert to dropdown items and filter by search query
			const allItems = Object.entries(subtreeMap)
				.filter(([_, { contributors }]) => contributors.length > 0) // Only subtrees with contributors
				.map(([subtreeId, { name, contributors }]) => ({
					id: subtreeId,
					name: name || subtreeId,
					metadata: {
						contributorCount: contributors.length,
						contributors
					}
				}));

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
