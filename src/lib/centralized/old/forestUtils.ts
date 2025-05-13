import type { Forest, TreeZipper } from '../types';

/**
 * Gets the name of a node by ID from the forest
 * @param forest The forest to look in
 * @param id The ID of the node
 * @returns The name of the node, or the ID if not found
 */
export function getNodeName(forest: Forest, id: string): string {
	const zipper = forest.get(id);
	if (zipper) {
		return zipper.zipperCurrent.nodeName || id;
	}
	return id;
}

/**
 * Filters the forest based on a search text
 * @param forest The forest to filter
 * @param searchText The text to search for
 * @param excludeIds Optional array of IDs to exclude from results
 * @returns Array of filtered nodes with id and name
 */
export function filterForestNodes(
	forest: Forest,
	searchText: string = '',
	excludeIds: string[] = []
): Array<{ id: string; name: string }> {
	const lowerSearch = searchText.toLowerCase();

	return Array.from(forest.entries())
		.filter(([id, zipper]) => {
			// Skip excluded IDs
			if (excludeIds.includes(id)) return false;

			// If no search text, include all
			if (!lowerSearch) return true;

			// Check if name or ID contains the search text
			const name = (zipper.zipperCurrent.nodeName || '').toLowerCase();
			return name.includes(lowerSearch) || id.toLowerCase().includes(lowerSearch);
		})
		.map(([id, zipper]) => ({
			id,
			name: zipper.zipperCurrent.nodeName || id
		}));
}

/**
 * Gets the child nodes of a zipper
 * @param zipper The zipper to get children from
 * @returns Array of child nodes with id and name
 */
export function getChildNodes(zipper: TreeZipper): Array<{ id: string; name: string }> {
	if (!zipper) return [];

	const childEntries = Array.from(zipper.zipperCurrent.nodeChildren.entries());

	return childEntries.map(([id, node]) => ({
		id,
		name: node.nodeName || id
	}));
}
