import type {
	CapacityInventory,
	CapacityShares,
	Node,
	Points,
	TreeZipper,
	Ctx,
	NavigationPath
} from './types';
import { emptyCache } from './cache';

// Node creation and management functions
export function createRootNode(
	id: string,
	name: string,
	pts: Points,
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
		nodeCache: emptyCache()
	};
}

// Helper function to clamp manual fulfillment between 0 and 1
function clampManual(manual: number | null): number | null {
	if (manual === null) return null;
	return Math.max(0, Math.min(1, manual));
}

// Get points value from Points object
export function getPoints(pts: Points): number {
	return pts.value;
}

// Create a Points object
export function makePoints(value: number): Points {
	return { value };
}

// ==== Tree Zipper Navigation ====

// Core navigation functions
export function enterChild(childId: string, zipper: TreeZipper): TreeZipper | null {
	const current = zipper.zipperCurrent;
	const child = current.nodeChildren.get(childId);

	if (!child) {
		return null;
	}

	// Create a new map without the child we're focusing on
	const siblings = new Map(current.nodeChildren);
	siblings.delete(childId);

	// Create new context with current node as parent
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

export function exitToParent(zipper: TreeZipper): TreeZipper | null {
	if (zipper.zipperContext === null) {
		return null;
	}

	const ctx = zipper.zipperContext;
	const parent = ctx.ctxParent;
	const siblings = ctx.ctxSiblings;
	const ancestors = ctx.ctxAncestors;

	// Create a new children map with the current node reinserted
	const updatedChildren = new Map(siblings);
	updatedChildren.set(zipper.zipperCurrent.nodeId, zipper.zipperCurrent);

	// Create updated parent with the current node as child
	const newParent: Node = {
		...parent,
		nodeChildren: updatedChildren
	};

	// Create new zipper with parent as focus
	return {
		zipperCurrent: newParent,
		zipperContext: ancestors.length > 0 ? ancestors[0] : null
	};
}

// Enhanced navigation functions
export function enterSibling(name: string, zipper: TreeZipper): TreeZipper | null {
	const parent = exitToParent(zipper);
	if (!parent) return null;
	return enterChild(name, parent);
}

export function goToRoot(zipper: TreeZipper): TreeZipper {
	let current = zipper;
	let parent = exitToParent(current);

	while (parent) {
		current = parent;
		parent = exitToParent(current);
	}

	return current;
}

// Modify the current node safely
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

// Safe navigation with breadcrumbs
export function followPath(path: NavigationPath, zipper: TreeZipper): TreeZipper | null {
	if (path.length === 0) return zipper;

	const [nextId, ...remainingPath] = path;
	const nextNode = enterChild(nextId, zipper);

	if (!nextNode) return null;
	return followPath(remainingPath, nextNode);
}

// Get the current path from root
export function getCurrentPath(zipper: TreeZipper): NavigationPath {
	const path: string[] = [];
	let current = zipper;

	while (current.zipperContext) {
		path.unshift(current.zipperCurrent.nodeId);
		const parent = exitToParent(current);
		if (!parent) break;
		current = parent;
	}

	return path;
}

// Add child node to the tree
export function addChild(
	name: string,
	pts: Points,
	contribs: string[],
	manual: number | null,
	zipper: TreeZipper
): TreeZipper {
	const current = zipper.zipperCurrent;

	// Create the new child
	const newChild: Node = {
		nodeId: name,
		nodeName: name,
		nodePoints: pts,
		nodeChildren: new Map(),
		nodeContributors: new Set(contribs),
		nodeManualFulfillment: clampManual(manual),
		nodeCapacities: new Map(),
		nodeCapacityShares: new Map(),
		nodeCache: emptyCache()
	};

	// Create a new children map with the new child
	const updatedChildren = new Map(current.nodeChildren);
	updatedChildren.set(name, newChild);

	// Update the current node's children
	const updatedCurrent: Node = {
		...current,
		nodeChildren: updatedChildren,
		// Clear the cache as structure has changed
		nodeCache: emptyCache()
	};

	return {
		...zipper,
		zipperCurrent: updatedCurrent
	};
}
