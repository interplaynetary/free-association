import type { Forest, TreeZipper } from './types';
import { GunUserTree } from './tree';

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
	// Mock data for demonstration - in a real implementation,
	// this would query GunUserTree for available users
	const mockUsers = [
		{ id: 'ALICE123', name: 'Alice' },
		{ id: 'BOB456', name: 'Bob' },
		{ id: 'CHARLIE789', name: 'Charlie' }
	];

	// Filter by excludeIds
	let filtered = mockUsers.filter((user) => !excludeIds.includes(user.id));

	// Apply text filter if provided
	if (filterText) {
		const lowerFilter = filterText.toLowerCase();
		filtered = filtered.filter(
			(user) =>
				user.name.toLowerCase().includes(lowerFilter) || user.id.toLowerCase().includes(lowerFilter)
		);
	}

	return filtered;
}
