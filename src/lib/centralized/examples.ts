// Gun Tree Examples

import { GunUserTree, enterChild, exitToParent, updateNodePersistentCache } from './tree';
import type { TreeZipper } from './types';

/**
 * This file contains example usage of the GunUserTree class
 * to demonstrate how to use the Gun tree functionality with TreeZipper pattern
 */

// Example 1: Create a tree and navigate it
async function createAndNavigateTree() {
	// Initialize GunUserTree
	await GunUserTree.initialize();

	// Create a root node
	const rootTree = await GunUserTree.createRootNode('Root Node', 100);
	const rootId = rootTree.zipperCurrent.nodeId;

	// Add some children
	const child1 = await GunUserTree.addChild(rootId, 'Child 1', 50, 'child1');
	await GunUserTree.saveNode(child1!);

	const child2 = await GunUserTree.addChild(rootId, 'Child 2', 30, 'child2');
	await GunUserTree.saveNode(child2!);

	// Add a child to Child 1
	const grandchild1 = await GunUserTree.addChild('child1', 'Grandchild 1', 20, 'grandchild1');
	await GunUserTree.saveNode(grandchild1!);

	// Load the full tree with all children
	let currentTree = await GunUserTree.loadFullTree();
	if (!currentTree) return;

	// Navigate down to Child 1 (using pure functions)
	let navigatedTree = enterChild('child1', currentTree);
	console.log('Current node:', navigatedTree?.zipperCurrent.nodeName);

	// Navigate down to Grandchild 1
	navigatedTree = enterChild('grandchild1', navigatedTree!);
	console.log('Current node:', navigatedTree?.zipperCurrent.nodeName);

	// Navigate back up to Child 1
	navigatedTree = exitToParent(navigatedTree!);
	console.log('Current node:', navigatedTree?.zipperCurrent.nodeName);

	// Manually navigate to Child 2 (by loading it directly)
	const child2Tree = await GunUserTree.fetchNode('child2');
	console.log('Current node:', child2Tree?.zipperCurrent.nodeName);
}

// Example 2: Saving and loading trees
async function savingAndLoadingTree() {
	// Initialize GunUserTree
	await GunUserTree.initialize();

	// Create a root node if it doesn't exist
	let rootTree = await GunUserTree.loadFullTree();

	if (!rootTree) {
		rootTree = await GunUserTree.createRootNode('My Tasks', 100);
		await GunUserTree.saveNode(rootTree);
	}

	const rootId = rootTree.zipperCurrent.nodeId;

	// Add a child if none exist
	if (rootTree.zipperCurrent.nodeChildren.size === 0) {
		const child = await GunUserTree.addChild(rootId, 'New Task', 50, 'task1');
		if (child) {
			await GunUserTree.saveNode(child);
		}
	}

	// Reload the full tree to see our changes
	rootTree = await GunUserTree.loadFullTree();

	// Print the tree structure
	console.log('Tree loaded:');
	printTree(rootTree);
}

// Example 3: Working with cache data
async function workingWithCacheData() {
	// Initialize GunUserTree
	await GunUserTree.initialize();

	// Load the user's tree
	let rootTree = await GunUserTree.loadFullTree();
	if (!rootTree) return;

	// Load cache data
	rootTree = await GunUserTree.loadCacheData(rootTree);

	// Add sample cache data if none exists
	if (!rootTree.zipperCurrent.nodePersistentCache.pcSogfMap) {
		// Create sample data
		const updatedTree = updateNodePersistentCache(
			(pc) => ({
				...pc,
				pcSogfMap: new Map([
					['user2', 0.3],
					['user3', 0.7]
				]),
				pcProviderShares: new Map([
					[
						1,
						new Map([
							['user2', 0.25],
							['user3', 0.75]
						])
					],
					[
						2,
						new Map([
							['user4', 0.5],
							['user5', 0.5]
						])
					]
				])
			}),
			rootTree
		);

		// Save the cache data
		await GunUserTree.saveCacheData(updatedTree);
		console.log('Sample cache data saved');
	} else {
		console.log('Existing cache data found:');

		// Display SOGF map
		if (rootTree.zipperCurrent.nodePersistentCache.pcSogfMap) {
			console.log(
				'SOGF Map:',
				Object.fromEntries(rootTree.zipperCurrent.nodePersistentCache.pcSogfMap)
			);
		}

		// Display provider shares
		const shares = rootTree.zipperCurrent.nodePersistentCache.pcProviderShares;
		for (const [depth, depthShares] of shares.entries()) {
			console.log(`Provider shares at depth ${depth}:`, Object.fromEntries(depthShares));
		}
	}
}

// Example 4: Performing calculations
async function performingCalculations() {
	// Initialize GunUserTree
	await GunUserTree.initialize();

	// Load the full tree
	const rootTree = await GunUserTree.loadFullTree();
	if (!rootTree) return;

	// Create a map of trees for calculations
	const trees = new Map<string, TreeZipper>();
	trees.set(rootTree.zipperCurrent.nodeId, rootTree);

	// Add children to the map
	for (const [childId, childNode] of rootTree.zipperCurrent.nodeChildren.entries()) {
		const childTree = await GunUserTree.fetchNode(childId);
		if (childTree) {
			trees.set(childId, childTree);
		}
	}

	// Perform calculations
	console.log('Root node weight:', GunUserTree.calculateWeight(rootTree));
	console.log('Root node fulfillment:', GunUserTree.calculateFulfillment(rootTree));
	console.log('Root node desire:', GunUserTree.calculateDesire(rootTree));

	// If we have at least two nodes, calculate mutual fulfillment
	const treeIds = Array.from(trees.keys());
	if (treeIds.length >= 2) {
		const nodeA = trees.get(treeIds[0])!;
		const nodeB = trees.get(treeIds[1])!;

		console.log(
			`Mutual fulfillment between ${nodeA.zipperCurrent.nodeName} and ${nodeB.zipperCurrent.nodeName}:`,
			GunUserTree.calculateMutualFulfillment(nodeA, nodeB, trees)
		);
	}
}

// Helper function to print tree structure
function printTree(tree: TreeZipper | null, indent = 0) {
	if (!tree) return;

	const node = tree.zipperCurrent;
	console.log(
		' '.repeat(indent) +
			`- ${node.nodeName} (${node.nodePoints} pts, contributors: ${Array.from(node.nodeContributors).join(', ') || 'none'})`
	);

	// Print children recursively
	for (const [childId, childNode] of node.nodeChildren.entries()) {
		const childTree: TreeZipper = {
			zipperCurrent: childNode,
			zipperContext: null // We don't need context for printing
		};
		printTree(childTree, indent + 2);
	}
}

// Export the examples
export {
	createAndNavigateTree,
	savingAndLoadingTree,
	workingWithCacheData,
	performingCalculations
};
