import { get, derived } from 'svelte/store';
import {
	userTree,
	nodesMap,
	userNetworkCapacitiesWithShares,
	networkCapacities
} from '$lib/state/core.svelte';
import { userIds, userNamesCache } from '$lib/state/gun.svelte';
import { getSubtreeContributorMap, findNodeById } from '$lib/protocol';

// Simple reactive users data provider
export function createUsersDataProvider(excludeIds: string[] = []) {
	return derived([userIds, userNamesCache], ([$userIds, $userNamesCache]) => {
		if (!$userIds) {
			return [];
		}

		// Show all users we have cached names for (both online and offline)
		const allUserIds = [...new Set([...$userIds, ...Object.keys($userNamesCache)])];

		return allUserIds
			.filter((userId) => !excludeIds.includes(userId))
			.map((userId) => ({
				id: userId,
				name: $userNamesCache[userId] || userId,
				metadata: { userId }
			}));
	});
}

// Simple reactive subtrees data provider
export function createSubtreesDataProvider() {
	return derived([userTree, nodesMap], ([$userTree, $nodesMap]) => {
		if (!$userTree || !$nodesMap) {
			return [];
		}

		const items: Array<{
			id: string;
			name: string;
			metadata: { contributorCount: number; contributors: string[] };
		}> = [];

		// Add all nodes from the tree as potential subtree filters
		Object.values($nodesMap).forEach((node) => {
			if (node.type === 'RootNode') return; // Skip root nodes

			items.push({
				id: node.id,
				name: node.name,
				metadata: {
					contributorCount: 0,
					contributors: []
				}
			});
		});

		return items;
	});
}

// Simple reactive capacities data provider
export function createCapacitiesDataProvider(excludeCapacityId?: string) {
	return derived([userNetworkCapacitiesWithShares], ([$userNetworkCapacitiesWithShares]) => {
		if (!$userNetworkCapacitiesWithShares) {
			return [];
		}

		// Convert to dropdown items
		const items = Object.entries($userNetworkCapacitiesWithShares)
			.filter(([capacityId]) => capacityId !== excludeCapacityId)
			.map(([capacityId, capacity]) => ({
				id: capacityId,
				name: `${capacity.emoji || '📦'} ${capacity.name}`,
				metadata: capacity
			}));

		return items;
	});
}

// Simple reactive all network capacities data provider
export function createAllNetworkCapacitiesDataProvider(excludeCapacityId?: string) {
	return derived([networkCapacities], ([$networkCapacities]) => {
		if (!$networkCapacities) {
			return [];
		}

		// Flatten all capacities from all users
		const items: Array<{ id: string; name: string; metadata: any }> = [];

		Object.entries($networkCapacities).forEach(([userId, userCapacities]) => {
			Object.entries(userCapacities).forEach(([capacityId, capacity]) => {
				// Skip excluded capacity
				if (capacityId === excludeCapacityId) return;

				items.push({
					id: capacityId,
					name: `${capacity.emoji || '📦'} ${capacity.name}`,
					metadata: {
						...capacity,
						owner_id: userId,
						provider_id: userId
					}
				});
			});
		});

		return items;
	});
}

// Simple reactive contributors data provider
export function createContributorsDataProvider() {
	return derived([userIds, userNamesCache], ([$userIds, $userNamesCache]) => {
		if (!$userIds) {
			return [];
		}

		return $userIds.map((userId) => ({
			id: userId,
			name: $userNamesCache[userId] || userId,
			metadata: { userId }
		}));
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

	// For demo/local usage - just return empty object
	return {};
}

// Legacy aliases for backward compatibility (will be removed)
export const createReactiveCapacitiesDataProvider = createCapacitiesDataProvider;
export const createReactiveAllNetworkCapacitiesDataProvider =
	createAllNetworkCapacitiesDataProvider;
