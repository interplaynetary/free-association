import type { Node, TreeZipper, Context } from './core';

export function createTreeZipper(node: Node): TreeZipper {
	return {
		current: node,
		context: undefined
	};
}

export function enterChild(childId: string, zipper: TreeZipper): TreeZipper | undefined {
	const child = zipper.current.children.get(childId);
	if (!child) {
		return undefined;
	}

	const siblings = new Map(zipper.current.children);
	siblings.delete(childId);

	const newContext: Context = {
		parent: zipper.current,
		siblings,
		ancestors: zipper.context ? [zipper.context, ...zipper.context.ancestors] : []
	};

	return {
		current: child,
		context: newContext
	};
}

export function exitToParent(zipper: TreeZipper): TreeZipper | undefined {
	if (!zipper.context) {
		return undefined;
	}

	const { parent, siblings, ancestors } = zipper.context;

	// Update parent's children map with current node
	const updatedChildren = new Map(siblings);
	updatedChildren.set(zipper.current.id, zipper.current);

	const updatedParent: Node = {
		...parent,
		children: updatedChildren
	};

	return {
		current: updatedParent,
		context: ancestors.length > 0 ? ancestors[0] : undefined
	};
}

export function enterSibling(siblingId: string, zipper: TreeZipper): TreeZipper | undefined {
	const parent = exitToParent(zipper);
	if (!parent) {
		return undefined;
	}
	return enterChild(siblingId, parent);
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

export function modifyNode(modifier: (node: Node) => Node, zipper: TreeZipper): TreeZipper {
	return {
		...zipper,
		current: modifier(zipper.current)
	};
}

export function getSiblings(zipper: TreeZipper): string[] {
	const parent = exitToParent(zipper);
	if (!parent) {
		return [];
	}
	return Array.from(parent.current.children.keys());
}

export type NavigationPath = string[];

export function followPath(path: NavigationPath, zipper: TreeZipper): TreeZipper | undefined {
	return path.reduce<TreeZipper | undefined>(
		(current, step) => current && enterChild(step, current),
		zipper
	);
}

export function getCurrentPath(zipper: TreeZipper): NavigationPath {
	const path: string[] = [];
	let current = zipper;

	while (current.context) {
		path.unshift(current.current.id);
		const parent = exitToParent(current);
		if (!parent) break;
		current = parent;
	}

	return path;
}
