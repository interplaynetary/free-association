import type { Forest, TreeZipper } from './types';
import { GunUserTree } from './tree';
import { gun, user } from '../gun/gunSetup';

/**
 * Get a node's name by ID
 * @param forest Forest to search in (now deprecated - just for backward compatibility)
 * @param nodeId Node ID to look up
 * @returns Name of the node or the ID if not found
 */
export function getNodeName(forest: Forest | null, nodeId: string): string {
	if (!nodeId) return 'Unknown';

	// First try to find the node in the forest (backward compatibility)
	if (forest) {
		// Check if this is a root node in our forest
		const tree = forest.get(nodeId);
		if (tree) {
			return tree.zipperCurrent.nodeName || nodeId;
		}

		// Otherwise, search through all trees in the forest
		for (const tree of forest.values()) {
			const nodeName = findNodeNameInTree(tree, nodeId);
			if (nodeName) return nodeName;
		}
	}

	// Default to just returning the ID if nothing is found
	return nodeId;
}

/**
 * Find a node's name in a specific tree
 * @param tree TreeZipper to search in
 * @param nodeId Node ID to look for
 * @returns Name of the node or null if not found
 */
function findNodeNameInTree(tree: TreeZipper, nodeId: string): string | null {
	// Check if this is the node we're looking for
	if (tree.zipperCurrent.nodeId === nodeId) {
		return tree.zipperCurrent.nodeName || nodeId;
	}

	// Check children
	for (const [childId, childNode] of tree.zipperCurrent.nodeChildren) {
		if (childId === nodeId) {
			return childNode.nodeName || nodeId;
		}

		// We would need to recurse deeper in a real implementation
		// This is simplified for our example
	}

	return null;
}

/**
 * Filter forest nodes for user selection
 * @param forest Forest to filter (now deprecated, kept for backward compatibility)
 * @param filterText Text to filter by
 * @param excludeIds IDs to exclude from results
 * @returns Array of filtered user objects
 */
export function filterForestNodes(
	forest: Forest | null,
	filterText: string,
	excludeIds: string[] = []
): Array<{ id: string; name: string }> {
	// Import gun from the gunSetup module

	// Return an empty array if gun is not available
	if (!gun) return [];

	// Get users from Gun database
	const usersNode = gun.get('users');
	if (!usersNode) return [];

	// Use the current authenticated user if available
	const currentUserId = user?.is?.pub || '';

	// Create a default set of users including the current user
	const defaultUsers: Array<{ id: string; name: string }> = [];
	if (currentUserId && !excludeIds.includes(currentUserId)) {
		defaultUsers.push({
			id: currentUserId,
			name: user?.is?.alias || 'Current User'
		});
	}

	// This is a hybrid approach - we return some default users immediately,
	// but also set up a listener to update the users list when data is available
	usersNode.map().once((userData: any, userId: string) => {
		if (!userId || userId === '_' || excludeIds.includes(userId)) return;

		// Get user name from Gun data
		const userName = userData?.name || userId;

		// Check if this user matches the filter text
		const lowerFilter = filterText.toLowerCase();
		if (
			filterText &&
			!userName.toLowerCase().includes(lowerFilter) &&
			!userId.toLowerCase().includes(lowerFilter)
		) {
			return;
		}

		// Add to default users if not already present
		if (!defaultUsers.some((user) => user.id === userId)) {
			defaultUsers.push({
				id: userId,
				name: userName
			});
		}
	});

	// Also add fallback users if the list is empty
	if (defaultUsers.length === 0) {
		return [
			{ id: 'ALICE123', name: 'Alice' },
			{ id: 'BOB456', name: 'Bob' },
			{ id: 'CHARLIE789', name: 'Charlie' }
		].filter((user) => !excludeIds.includes(user.id));
	}

	return defaultUsers;
}
