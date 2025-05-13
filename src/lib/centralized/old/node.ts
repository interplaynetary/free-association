import type { Node, TreeZipper, Ctx, NavigationPath, PersistentCache } from './types';
import { emptyCache } from './cache';
import { emptyPersistentCache } from './persist';

// Pure function to create a root node
export function createRootNode(
	id: string,
	name: string,
	pts: number,
	contribs: string[],
	manual: number | null
): Node {
	return {
		nodeId: id,
		nodeName: name,
		nodePoints: pts,
		nodeChildren: new Map(),
		nodeContributors: new Set(contribs),
		nodeManualFulfillment: clampManual(manual),
		nodeCapacities: new Map(),
		nodeCapacityShares: new Map(),
		// Separate persistent and transient caches
		nodePersistentCache: emptyPersistentCache(),
		nodeTransientCache: emptyCache()
	};
}

// Helper function to clamp manual fulfillment between 0 and 1
function clampManual(manual: number | null): number | null {
	if (manual === null) return null;
	return Math.max(0, Math.min(1, manual));
}

// ==== Tree Zipper Navigation ====

// Core navigation: enter a child node
export function enterChild(childId: string, zipper: TreeZipper): TreeZipper | null {
	const current = zipper.zipperCurrent;
	const child = current.nodeChildren.get(childId);

	if (!child) return null;

	// Create new context with immutable updates
	const siblings = new Map(current.nodeChildren);
	siblings.delete(childId);

	const ctx: Ctx = {
		ctxParent: current,
		ctxSiblings: siblings,
		ctxAncestors: zipper.zipperContext
			? [zipper.zipperContext, ...zipper.zipperContext.ctxAncestors]
			: []
	};

	return {
		zipperCurrent: child,
		zipperContext: ctx
	};
}

// Core navigation: return to parent node
export function exitToParent(zipper: TreeZipper): TreeZipper | null {
	if (!zipper.zipperContext) return null;

	const ctx = zipper.zipperContext;
	const parent = ctx.ctxParent;
	const siblings = ctx.ctxSiblings;
	const ancestors = ctx.ctxAncestors;

	// Immutably update parent children
	const updatedChildren = new Map(siblings);
	updatedChildren.set(zipper.zipperCurrent.nodeId, zipper.zipperCurrent);

	const newParent: Node = {
		...parent,
		nodeChildren: updatedChildren
	};

	return {
		zipperCurrent: newParent,
		zipperContext: ancestors.length > 0 ? ancestors[0] : null
	};
}

// Enhanced navigation: enter a sibling node
export function enterSibling(name: string, zipper: TreeZipper): TreeZipper | null {
	const parent = exitToParent(zipper);
	if (!parent) return null;
	return enterChild(name, parent);
}

// Enhanced navigation: go to root node
export function goToRoot(zipper: TreeZipper): TreeZipper {
	let current = zipper;
	let parent = exitToParent(current);

	// Recursive structure mirroring Haskell version
	while (parent) {
		current = parent;
		parent = exitToParent(current);
	}

	return current;
}

// Pure function to modify a node
export function modifyNode(modify: (node: Node) => Node, zipper: TreeZipper): TreeZipper {
	return {
		...zipper,
		zipperCurrent: modify(zipper.zipperCurrent)
	};
}

// Get all siblings of the current node
export function getSiblings(zipper: TreeZipper): string[] {
	const parent = exitToParent(zipper);
	if (!parent) return [];
	return Array.from(parent.zipperCurrent.nodeChildren.keys());
}

// Safe navigation with path
export function followPath(path: NavigationPath, zipper: TreeZipper): TreeZipper | null {
	// Base case - empty path returns current zipper (mirrors Haskell pattern matching)
	if (path.length === 0) return zipper;

	// Recursive case - follow first element, then rest of path
	const [nextId, ...restPath] = path;
	const nextNode = enterChild(nextId, zipper);

	if (!nextNode) return null;
	return followPath(restPath, nextNode);
}

// Get the current path from root
export function getCurrentPath(zipper: TreeZipper): NavigationPath {
	// Mirror Haskell's recursive approach with accumulator
	function getPath(z: TreeZipper, acc: string[]): string[] {
		const parent = exitToParent(z);
		if (!parent) return acc;
		return getPath(parent, [z.zipperCurrent.nodeId, ...acc]);
	}

	return getPath(zipper, []);
}

// Add a child node (pure function)
export function addChild(
	name: string,
	pts: number,
	contribs: string[],
	manual: number | null,
	zipper: TreeZipper
): TreeZipper | null {
	// Don't allow adding children to nodes with contributors
	if (zipper.zipperCurrent.nodeContributors.size > 0) {
		return null;
	}

	// Create the child node
	const newChild = createRootNode(name, name, pts, contribs, manual);

	// Immutably update the parent's children
	return modifyNode((node) => {
		const updatedChildren = new Map(node.nodeChildren);
		updatedChildren.set(name, newChild);

		return {
			...node,
			nodeChildren: updatedChildren,
			// Clear both caches since structure has changed
			nodePersistentCache: emptyPersistentCache(),
			nodeTransientCache: emptyCache()
		};
	}, zipper);
}

// Add contributors to a node and recursively delete its subtree
export function addContributors(contribs: string[], zipper: TreeZipper): TreeZipper {
	return modifyNode((node) => {
		return {
			...node,
			nodeContributors: new Set(contribs),
			nodeChildren: new Map(), // Delete subtree
			nodePersistentCache: emptyPersistentCache(),
			nodeTransientCache: emptyCache()
		};
	}, zipper);
}

// Helper to recursively delete a subtree
export function deleteSubtree(zipper: TreeZipper): TreeZipper {
	return modifyNode((node) => {
		return {
			...node,
			nodeChildren: new Map(),
			nodePersistentCache: emptyPersistentCache(),
			nodeTransientCache: emptyCache()
		};
	}, zipper);
}

// Run a cache computation on a node's transient cache
export function runNodeCache<A>(
	computation: (node: Node) => [A, Node['nodeTransientCache']],
	node: Node
): [A, Node] {
	const [result, newCache] = computation(node);
	return [result, { ...node, nodeTransientCache: newCache }];
}

// Run a cache computation on a zipper
export function withNodeCache<A>(
	computation: (zipper: TreeZipper) => [A, Node['nodeTransientCache']],
	zipper: TreeZipper
): A {
	const [result, _] = computation(zipper);
	return result;
}

// Update a node's persistent cache
export function updateNodePersistentCache(
	updater: (pc: PersistentCache) => PersistentCache,
	zipper: TreeZipper
): TreeZipper {
	return modifyNode((node) => {
		return {
			...node,
			nodePersistentCache: updater(node.nodePersistentCache)
		};
	}, zipper);
}

// Create a zipper from a persisted node
export function createZipperFromPersistedNode(node: Node): TreeZipper {
	return {
		zipperCurrent: node,
		zipperContext: null
	};
}
