import { type Node } from '$lib/schema'
import { getDescendants, findNodeById, getPathToNode } from '$lib/protocol'

/**
 * Tree Search Functions
 */

// Interface for search results
export interface SearchResult {
	node: Node;
	path: string[];
	score: number;
	pathNames: string[];
}

// Enhanced fuzzy matching function
function fuzzyMatch(text: string, query: string): boolean {
	let queryIndex = 0;
	for (let i = 0; i < text.length && queryIndex < query.length; i++) {
		if (text[i] === query[queryIndex]) {
			queryIndex++;
		}
	}
	return queryIndex === query.length;
}

// Calculate search relevance score
function calculateSearchScore(nodeName: string, query: string): number {
	const lowerNodeName = nodeName.toLowerCase();
	const lowerQuery = query.toLowerCase().trim();

	// Exact match gets highest score
	if (lowerNodeName === lowerQuery) {
		return 100;
	}
	// Starts with query gets high score
	else if (lowerNodeName.startsWith(lowerQuery)) {
		return 80;
	}
	// Contains query gets medium score
	else if (lowerNodeName.includes(lowerQuery)) {
		return 60;
	}
	// Fuzzy match gets lower score
	else if (fuzzyMatch(lowerNodeName, lowerQuery)) {
		return 30;
	}

	return 0;
}

// Search through a tree for nodes matching a query
export function searchTree(tree: Node, query: string, maxResults: number = 10): SearchResult[] {
	if (!query.trim()) return [];

	const results: SearchResult[] = [];
	const allNodes = [tree, ...getDescendants(tree)];

	for (const node of allNodes) {
		const score = calculateSearchScore(node.name, query);

		if (score > 0) {
			// Get the full path to this node
			const path = getPathToNode(tree, node.id) || [node.id];

			// Convert path IDs to names for display
			const pathNames = path.map((id) => {
				const pathNode = findNodeById(tree, id);
				return pathNode ? pathNode.name : id;
			});

			// Boost score for shorter names (more specific matches)
			const adjustedScore = score + Math.max(0, 20 - node.name.length);

			results.push({
				node,
				path,
				score: adjustedScore,
				pathNames
			});
		}
	}

	// Sort by score (highest first) and return top results
	return results.sort((a, b) => b.score - a.score).slice(0, maxResults);
}

// Find the best navigation path to show a target node
export function getBestNavigationPath(tree: Node, targetNodeId: string): string[] {
	// Get the full path to the target node
	const fullPath = getPathToNode(tree, targetNodeId);
	if (!fullPath) return [];

	// For navigation, we want the path excluding the target node itself
	// This represents the path we should navigate to in order to see the target node
	return fullPath.slice(0, -1);
}

// Search and get navigation-ready results
export function searchTreeForNavigation(
	tree: Node,
	query: string,
	maxResults: number = 10
): Array<{
	node: Node;
	navigationPath: string[];
	displayPath: string;
	score: number;
}> {
	const searchResults = searchTree(tree, query, maxResults);

	return searchResults.map((result) => ({
		node: result.node,
		navigationPath: getBestNavigationPath(tree, result.node.id),
		displayPath: result.pathNames.slice(1).join(' › '), // Skip root for display
		score: result.score
	}));
}

