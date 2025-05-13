import type { Forest, TreeZipper, Node } from './types';
import { createZipperFromPersistedNode } from './new';
import { emptyCache } from './cache';
import { emptyPersistentCache } from './persist';

// Add a TreeZipper to the forest (pure function)
export function addToForest(forest: Forest, zipper: TreeZipper): Forest {
	const id = zipper.zipperCurrent.nodeId;
	const newForest = new Map(forest);
	newForest.set(id, zipper);
	return newForest;
}

// Merge multiple forests (mirrors Haskell's mergeContributors)
export function mergeForests(forests: Forest[]): Forest {
	// Use reduce to fold over the forests (mirrors Haskell's foldl')
	return forests.reduce((result, forest) => {
		const newForest = new Map(result);

		// Prefer newer entries (matches Haskell implementation)
		for (const [id, zipper] of forest) {
			newForest.set(id, zipper);
		}

		return newForest;
	}, new Map<string, TreeZipper>());
}

// The ? operator (mirrors Haskell's ?)
export function maybeOr<T>(maybeValue: T | null | undefined, defaultValue: T): T {
	return maybeValue !== null && maybeValue !== undefined ? maybeValue : defaultValue;
}

// Helper to find a zipper in the forest
export function getZipper(forest: Forest, nodeId: string): TreeZipper | null {
	return forest.get(nodeId) || null;
}

// Create an empty zipper with default values
export function createEmptyZipper(id: string): TreeZipper {
	// Create a basic node
	const emptyNode: Node = {
		nodeId: id,
		nodeName: id,
		nodePoints: 0,
		nodeChildren: new Map(),
		nodeContributors: new Set(),
		nodeManualFulfillment: null,
		nodeCapacities: new Map(),
		nodeCapacityShares: new Map(),
		nodePersistentCache: emptyPersistentCache(),
		nodeTransientCache: emptyCache()
	};

	// Convert to zipper
	return createZipperFromPersistedNode(emptyNode);
}
