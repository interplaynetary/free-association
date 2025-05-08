import type { Forest, TreeZipper } from './types';
import { makePoints, createRootNode, addChild, enterChild } from './node';
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

// Create an example forest with three interconnected nodes and their children
export function createExampleForest(): { forest: Forest; ci: Forest } {
	// Create root nodes with mutual contributors
	const aliceNode = createRootNode('alice', 'Alice', makePoints(100), ['bob', 'charlie'], null);
	const bobNode = createRootNode('bob', 'Bob', makePoints(100), ['alice', 'charlie'], null);
	const charlieNode = createRootNode('charlie', 'Charlie', makePoints(100), ['alice', 'bob'], null);
	const davidNode = createRootNode('david', 'David', makePoints(100), ['alice', 'bob'], null);
	const eveNode = createRootNode('eve', 'Eve', makePoints(100), ['bob', 'charlie'], null);

	// Create TreeZippers from the nodes
	let aliceRoot: TreeZipper = { zipperCurrent: aliceNode, zipperContext: null };
	let bobRoot: TreeZipper = { zipperCurrent: bobNode, zipperContext: null };
	let charlieRoot: TreeZipper = { zipperCurrent: charlieNode, zipperContext: null };
	let davidRoot: TreeZipper = { zipperCurrent: davidNode, zipperContext: null };
	let eveRoot: TreeZipper = { zipperCurrent: eveNode, zipperContext: null };

	// Add first level children to Alice
	aliceRoot = addChild('alice-project1', makePoints(40), ['bob', 'david'], null, aliceRoot);
	aliceRoot = addChild('alice-project2', makePoints(30), ['charlie', 'eve'], null, aliceRoot);
	aliceRoot = addChild(
		'alice-project3',
		makePoints(30),
		['bob', 'charlie', 'david'],
		null,
		aliceRoot
	);

	// Add second level children to Alice's first project
	const aliceProject1 = enterChild('alice-project1', aliceRoot);
	if (aliceProject1) {
		let updatedAliceProject1 = addChild(
			'alice-subproject1',
			makePoints(20),
			['bob', 'charlie'],
			null,
			aliceProject1
		);

		// Add the updated child back to Alice's root
		const updatedChildren = new Map(aliceRoot.zipperCurrent.nodeChildren);
		updatedChildren.set('alice-project1', updatedAliceProject1.zipperCurrent);
		aliceRoot = {
			zipperCurrent: {
				...aliceRoot.zipperCurrent,
				nodeChildren: updatedChildren
			},
			zipperContext: aliceRoot.zipperContext
		};
	}

	// Add first level children to Bob
	bobRoot = addChild('bob-project1', makePoints(35), ['alice', 'david'], null, bobRoot);
	bobRoot = addChild('bob-project2', makePoints(35), ['charlie', 'eve'], null, bobRoot);
	bobRoot = addChild('bob-project3', makePoints(30), ['alice', 'charlie', 'eve'], null, bobRoot);

	// Add first level children to Charlie
	charlieRoot = addChild('charlie-project1', makePoints(35), ['alice', 'david'], null, charlieRoot);
	charlieRoot = addChild('charlie-project2', makePoints(35), ['bob', 'eve'], null, charlieRoot);
	charlieRoot = addChild(
		'charlie-project3',
		makePoints(30),
		['alice', 'bob', 'david'],
		null,
		charlieRoot
	);

	// Add first level children to David
	davidRoot = addChild('david-project1', makePoints(40), ['alice', 'bob'], null, davidRoot);
	davidRoot = addChild('david-project2', makePoints(30), ['charlie', 'eve'], null, davidRoot);
	davidRoot = addChild('david-project3', makePoints(30), ['bob', 'charlie'], null, davidRoot);

	// Add first level children to Eve
	eveRoot = addChild('eve-project1', makePoints(30), ['alice', 'david'], null, eveRoot);
	eveRoot = addChild('eve-project2', makePoints(30), ['bob', 'charlie'], null, eveRoot);
	eveRoot = addChild('eve-project3', makePoints(40), ['alice', 'bob', 'charlie'], null, eveRoot);

	// Build the forest and contributor index
	const forest: Forest = new Map();
	forest.set('alice', aliceRoot);
	forest.set('bob', bobRoot);
	forest.set('charlie', charlieRoot);
	forest.set('david', davidRoot);
	forest.set('eve', eveRoot);

	// Create a separate contributor index (in this case, identical to the forest)
	const ci: Forest = new Map(forest);

	return { forest, ci };
}
