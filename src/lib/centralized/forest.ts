import type { Forest, TreeZipper } from './types';
import { makePoints, createRootNode } from './node';
import { emptyCache } from './cache';

// Add a TreeZipper to the forest
export function addToForest(forest: Forest, zipper: TreeZipper): Forest {
	const id = zipper.zipperCurrent.nodeId;
	const newForest = new Map(forest);
	newForest.set(id, zipper);
	return newForest;
}

// Merge multiple forests into one (newer entries take precedence)
export function mergeForests(forests: Forest[]): Forest {
	if (forests.length === 0) return new Map();

	return forests.reduce((result, forest) => {
		// For each forest, add all its entries to the result
		const newForest = new Map(result);

		for (const [id, zipper] of forest) {
			newForest.set(id, zipper);
		}

		return newForest;
	}, new Map<string, TreeZipper>());
}

// Create a TreeZipper from a node ID and forest
export function getZipper(forest: Forest, nodeId: string): TreeZipper | null {
	return forest.get(nodeId) || null;
}

// Create an empty zipper with default values
export function createEmptyZipper(id: string): TreeZipper {
	const emptyNode = createRootNode(id, id, makePoints(0), [], null);

	return {
		zipperCurrent: emptyNode,
		zipperContext: null
	};
}

// Create an example forest with three interconnected nodes
export function createExampleForest(): { forest: Forest; ci: Forest } {
	// Create root nodes with mutual contributors
	const aliceNode = createRootNode('alice', 'Alice', makePoints(100), ['bob', 'charlie'], null);
	const bobNode = createRootNode('bob', 'Bob', makePoints(100), ['alice', 'charlie'], null);
	const charlieNode = createRootNode('charlie', 'Charlie', makePoints(100), ['alice', 'bob'], null);

	// Create TreeZippers from the nodes
	const aliceRoot: TreeZipper = { zipperCurrent: aliceNode, zipperContext: null };
	const bobRoot: TreeZipper = { zipperCurrent: bobNode, zipperContext: null };
	const charlieRoot: TreeZipper = { zipperCurrent: charlieNode, zipperContext: null };

	// Build the forest and contributor index
	const forest: Forest = new Map();
	forest.set('alice', aliceRoot);
	forest.set('bob', bobRoot);
	forest.set('charlie', charlieRoot);

	// Create a separate contributor index (in this case, identical to the forest)
	const ci: Forest = new Map(forest);

	return { forest, ci };
}
