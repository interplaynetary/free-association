import { GunNode } from '../gun/GunNode';
import type {
	Node,
	RootNode,
	NonRootNode,
	TreeZipper,
	Ctx,
	NavigationPath,
	ShareMap
} from './types';
import { user } from '../gun/user';

/**
 * Tree class for managing tree structures using Gun within a user's secure space
 * Following the Haskell reference implementation of the Free-Association protocol
 */
export class Tree {
	/**
	 * Create a root node in the user's tree
	 * @param name Node name
	 * @param manual Manual fulfillment value
	 * @returns Promise resolving to a TreeZipper
	 */
	static async createRootNode(name: string, manual: number | null = null): Promise<TreeZipper> {
		// Ensure user is authenticated
		if (!user.is?.pub) {
			throw new Error('User must be authenticated to create a root node');
		}

		const userPub = user.is.pub;
		const nodeId = userPub; // Root node ID is the user's public key

		// Create root node in user space
		const rootNode = new GunNode(['tree', 'root']);

		// Store node data
		await rootNode.put({
			nodeId,
			nodeName: name,
			nodeType: 'root',
			nodeManualFulfillment: clampManual(manual)
		});

		// Create an in-memory representation
		const node: RootNode = {
			type: 'root',
			nodeId,
			nodeName: name,
			nodeChildren: new Map(),
			nodeManualFulfillment: clampManual(manual),
			nodeCapacities: new Map(),
			nodeCapacityShares: new Map(),
			nodeSOGFMap: null,
			nodeProviderSharesMap: new Map()
		};

		// Return as a zipper
		return {
			zipperCurrent: node,
			zipperContext: null
		};
	}

	/**
	 * Load the full tree structure for the current user
	 * @returns Promise resolving to a TreeZipper with the full tree
	 */
	static async loadFullTree(): Promise<TreeZipper | null> {
		// Ensure user is authenticated
		if (!user.is?.pub) {
			throw new Error('User must be authenticated to load the full tree');
		}

		const userPub = user.is.pub;

		// First load the root node
		const rootZipper = await Tree.fetchNode(userPub);
		if (!rootZipper) return null;

		// Then load all children recursively
		await Tree.loadChildrenRecursively(rootZipper);

		return rootZipper;
	}

	/**
	 * Recursively load all children for a node
	 * @param zipper TreeZipper to load children for
	 */
	private static async loadChildrenRecursively(zipper: TreeZipper): Promise<void> {
		const node = zipper.zipperCurrent;
		const childrenIds = Array.from(node.nodeChildren.keys());

		// Load each child's data and its children
		for (const childId of childrenIds) {
			const childZipper = await Tree.fetchNode(childId);
			if (childZipper) {
				// Update the placeholder with actual data
				node.nodeChildren.set(childId, childZipper.zipperCurrent);

				// Recursively load this child's children
				await Tree.loadChildrenRecursively(childZipper);
			}
		}
	}

	/**
	 * Add a child node to the current node
	 * @param parentId Parent node ID
	 * @param name Child node name
	 * @param pts Points value
	 * @param childId Child node ID (optional, will generate unique ID if not provided)
	 * @param contribs Contributors
	 * @param manual Manual fulfillment value
	 * @returns Promise resolving to the child TreeZipper
	 */
	static async addChild(
		parentId: string,
		name: string,
		pts: number,
		childId?: string,
		contribs: string[] = [],
		manual: number | null = null
	): Promise<TreeZipper | null> {
		// Ensure user is authenticated
		if (!user.is?.pub) {
			throw new Error('User must be authenticated to add a child node');
		}

		// Generate unique ID if not provided
		const actualChildId =
			childId || `node_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`;

		// Get parent node
		const parentNode = await Tree.fetchNode(parentId);
		if (!parentNode) return null;

		// Don't allow adding children to nodes with contributors if parent is a non-root node
		const parent = parentNode.zipperCurrent;
		if (parent.type === 'non-root' && parent.nodeContributors.size > 0) {
			return null;
		}

		// Convert contributors array to Gun-friendly object
		const contributorsObj: Record<string, boolean> = {};
		contribs.forEach((contrib: string) => {
			contributorsObj[contrib] = true;
		});

		// Create child node in Gun under the nodes collection
		const childNode = new GunNode(['tree', 'nodes', actualChildId]);

		await childNode.put({
			nodeId: actualChildId,
			nodeName: name,
			nodePoints: pts,
			nodeType: 'non-root',
			nodeContributors: contributorsObj,
			nodeManualFulfillment: clampManual(manual)
		});

		// Create the edge from parent to child by adding a reference
		const parentRef =
			parentId === user.is.pub
				? new GunNode(['tree', 'root', 'children'])
				: new GunNode(['tree', 'nodes', parentId, 'children']);

		await parentRef.get(actualChildId).put({ '#': `~${user.is.pub}/tree/nodes/${actualChildId}` });

		// Create child node for in-memory representation
		const childNodeObj: NonRootNode = {
			type: 'non-root',
			nodeId: actualChildId,
			nodeName: name,
			nodePoints: pts,
			nodeChildren: new Map(),
			nodeContributors: new Set(contribs),
			nodeManualFulfillment: clampManual(manual)
		};

		// Create context for the zipper
		const ctx: Ctx = {
			ctxParent: parentNode.zipperCurrent,
			ctxSiblings: new Map(), // We're not loading siblings here for efficiency
			ctxAncestors: parentNode.zipperContext
				? [parentNode.zipperContext, ...parentNode.zipperContext.ctxAncestors]
				: []
		};

		// Return as a zipper
		return {
			zipperCurrent: childNodeObj,
			zipperContext: ctx
		};
	}

	/**
	 * Fetch a node by ID
	 * @param nodeId Node ID
	 * @returns Promise resolving to a TreeZipper
	 */
	static async fetchNode(nodeId: string): Promise<TreeZipper | null> {
		try {
			// Ensure user is authenticated
			if (!user.is?.pub) {
				throw new Error('User must be authenticated to fetch a node');
			}

			// Determine if we're fetching the root node or a child node
			const isRoot = nodeId === user.is.pub;
			const nodePath = isRoot ? ['tree', 'root'] : ['tree', 'nodes', nodeId];

			const nodeRef = new GunNode(nodePath);
			const nodeData = await nodeRef.once();

			if (!nodeData) {
				return null;
			}

			// Parse the children references
			const childrenRef = isRoot
				? new GunNode(['tree', 'root', 'children'])
				: new GunNode(['tree', 'nodes', nodeId, 'children']);

			const childrenData = await childrenRef.once();
			const nodeChildren = new Map<string, Node>();

			// Check if node is a root or non-root node
			const isRootNode = isRoot || nodeData.nodeType === 'root';

			if (childrenData) {
				// Load each child reference
				const childIds = Object.keys(childrenData).filter(
					(id) => id && id !== '_' && !id.startsWith('>')
				);

				// Create placeholders for children
				for (const id of childIds) {
					const placeholder: NonRootNode = {
						type: 'non-root',
						nodeId: id,
						nodeName: id, // Placeholder, will be loaded when needed
						nodePoints: 0,
						nodeChildren: new Map(),
						nodeContributors: new Set(),
						nodeManualFulfillment: null
					};
					nodeChildren.set(id, placeholder);
				}
			}

			// Convert contributors from Gun object to Set
			const contributorsData = nodeData.nodeContributors || {};
			const contributors = new Set(
				Object.keys(contributorsData).filter((id) => contributorsData[id])
			);

			// Create the appropriate node type
			let node: Node;

			if (isRootNode) {
				// Create a RootNode
				node = {
					type: 'root',
					nodeId: nodeData.nodeId || nodeId,
					nodeName: nodeData.nodeName || nodeId,
					nodeChildren,
					nodeManualFulfillment:
						nodeData.nodeManualFulfillment !== undefined ? nodeData.nodeManualFulfillment : null,
					nodeCapacities: new Map(), // Will be loaded separately if needed
					nodeCapacityShares: new Map(), // Will be loaded separately if needed
					nodeSOGFMap: null, // Will be calculated when needed
					nodeProviderSharesMap: new Map() // Will be calculated when needed
				};
			} else {
				// Create a NonRootNode
				node = {
					type: 'non-root',
				nodeId: nodeData.nodeId || nodeId,
					nodeName: nodeData.nodeName || nodeId,
				nodePoints: nodeData.nodePoints || 0,
					nodeChildren,
				nodeContributors: contributors,
					nodeManualFulfillment:
						nodeData.nodeManualFulfillment !== undefined ? nodeData.nodeManualFulfillment : null
				};
			}

			return {
				zipperCurrent: node,
				zipperContext: null // Context is only set when navigating
			};
		} catch (error) {
			console.error('Error fetching node:', error);
			return null;
		}
	}

	/**
	 * Save a node back to the database
	 * @param zipper TreeZipper containing the node to save
	 */
	static async saveNode(zipper: TreeZipper): Promise<void> {
		try {
		// Ensure user is authenticated
		if (!user.is?.pub) {
			throw new Error('User must be authenticated to save a node');
		}

		const node = zipper.zipperCurrent;
		const isRoot = node.nodeId === user.is.pub;
		const nodePath = isRoot ? ['tree', 'root'] : ['tree', 'nodes', node.nodeId];
		const nodeRef = new GunNode(nodePath);

			// Save node properties
			const saveData: Record<string, any> = {
			nodeId: node.nodeId,
			nodeName: node.nodeName,
				nodeType: node.type,
			nodeManualFulfillment: node.nodeManualFulfillment
			};

			if (node.type === 'non-root') {
				saveData.nodePoints = node.nodePoints;
				
				// Convert contributors Set to object
				const contributorsObj: Record<string, boolean> = {};
				node.nodeContributors.forEach((id) => {
					contributorsObj[id] = true;
				});
				saveData.nodeContributors = contributorsObj;
			}

			// Save the node data
			await nodeRef.put(saveData);
			
			// CRITICAL ADDITION: Now properly update the children references
			const childrenRef = isRoot
				? new GunNode(['tree', 'root', 'children'])
				: new GunNode(['tree', 'nodes', node.nodeId, 'children']);
			
			// Clear existing children references first
			await childrenRef.put(null);
			
			// Add references for each child
			for (const [childId, childNode] of node.nodeChildren) {
				// Create soul reference for this child
				const childSoul = `~${user.is.pub}/tree/nodes/${childId}`;
				await childrenRef.get(childId).put({ '#': childSoul });
			}
		} catch (error) {
			console.error('Error saving node:', error);
			throw error;
		}
	}

	/**
	 * Delete a node and its children
	 * @param nodeId ID of the node to delete
	 * @returns Promise resolving to true if successful
	 */
	static async deleteNode(nodeId: string): Promise<boolean> {
		try {
		// Ensure user is authenticated
		if (!user.is?.pub) {
			throw new Error('User must be authenticated to delete a node');
		}

			// Cannot delete root node
			if (nodeId === user.is.pub) {
				throw new Error('Cannot delete root node');
			}

			// Delete the node
			const nodeRef = new GunNode(['tree', 'nodes', nodeId]);
			await nodeRef.put(null);

			return true;
		} catch (error) {
			console.error('Error deleting node:', error);
			return false;
		}
	}

	static async fetchNodeWithChildren(nodeId: string): Promise<TreeZipper | null> {
		const nodeZipper = await Tree.fetchNode(nodeId);
		if (!nodeZipper) return null;
		
		// Load children recursively up to specified depth
		await Tree.loadChildrenRecursively(nodeZipper);
		
		return nodeZipper;
	}
}

// ==== Navigation Functions ====

/**
 * Helper function to clamp manual fulfillment value between 0 and 1
 */
function clampManual(manual: number | null): number | null {
	if (manual === null) return null;
	return Math.max(0, Math.min(1, manual));
}

/**
 * Enter a child node from the current node
 * @param childId Child ID to enter
 * @param zipper Current zipper
 * @returns New zipper positioned at the child, or null if child not found
 */
export function enterChild(childId: string, zipper: TreeZipper): TreeZipper | null {
	const current = zipper.zipperCurrent;
	const childMap = current.nodeChildren;

	if (!childMap.has(childId)) {
		return null;
	}

	const child = childMap.get(childId)!;
	const siblings = new Map(childMap);
	siblings.delete(childId);

	const newCtx: Ctx = {
		ctxParent: current,
		ctxSiblings: siblings,
		ctxAncestors: zipper.zipperContext
			? [zipper.zipperContext, ...zipper.zipperContext.ctxAncestors]
			: []
	};

	return {
		zipperCurrent: child,
		zipperContext: newCtx
	};
}

/**
 * Exit from current node to its parent
 * @param zipper Current zipper
 * @returns New zipper positioned at the parent, or null if at root
 */
export function exitToParent(zipper: TreeZipper): TreeZipper | null {
	const ctx = zipper.zipperContext;
	if (!ctx) {
		return null; // Already at root
	}

	const current = zipper.zipperCurrent;
	const parent = ctx.ctxParent;
	const siblings = ctx.ctxSiblings;
	const ancestors = ctx.ctxAncestors;

	// Add current node back to parent's children
	const updatedChildren = new Map(siblings);
	updatedChildren.set(current.nodeId, current);

	// Create updated parent with current node added back to its children
	const updatedParent = {
		...parent,
		nodeChildren: updatedChildren
	};

	// Safely handle ancestors
	const newContext = ancestors.length > 0 ? ancestors[0] : null;

	return {
		zipperCurrent: updatedParent,
		zipperContext: newContext
	};
}

/**
 * Navigate to the root of the tree
 * @param zipper Current zipper
 * @returns Zipper positioned at the root
 */
export function goToRoot(zipper: TreeZipper): TreeZipper {
	let current = zipper;
	while (true) {
		const parent = exitToParent(current);
		if (!parent) {
			return current; // Already at root
		}
		current = parent;
	}
}

/**
 * Get all descendants of a node, including the node itself
 * @param zipper Node to get descendants for
 * @returns Array of zippers for all descendants
 */
export function getAllDescendants(zipper: TreeZipper): TreeZipper[] {
	const result: TreeZipper[] = [zipper];
	const current = zipper.zipperCurrent;

	// Process all children
	for (const [childId, _] of current.nodeChildren) {
		const childZipper = enterChild(childId, zipper);
		if (childZipper) {
			result.push(...getAllDescendants(childZipper));
		}
	}

	return result;
}

/**
 * Get only the descendants (excluding the node itself)
 * @param zipper Node to get descendants for
 * @returns Array of zippers for descendants
 */
export function descendants(zipper: TreeZipper): TreeZipper[] {
	const allDescendants = getAllDescendants(zipper);
	return allDescendants.slice(1); // Remove the first element (self)
}

/**
 * Get all immediate children of a node
 * @param zipper Node to get children for
 * @returns Array of child zippers
 */
export function children(zipper: TreeZipper): TreeZipper[] {
	const result: TreeZipper[] = [];
	const current = zipper.zipperCurrent;

	for (const [childId, _] of current.nodeChildren) {
		const childZipper = enterChild(childId, zipper);
		if (childZipper) {
			result.push(childZipper);
		}
	}

	return result;
}

/**
 * Follow a path from the current node
 * @param path Array of node IDs to follow
 * @param zipper Starting zipper
 * @returns Resulting zipper or null if path invalid
 */
export function followPath(path: NavigationPath, zipper: TreeZipper): TreeZipper | null {
	if (path.length === 0) {
		return zipper;
	}

	const [first, ...rest] = path;
	const child = enterChild(first, zipper);

	if (!child) {
		return null;
	}

	return followPath(rest, child);
}

/**
 * Get the current path from root
 * @param zipper Current zipper
 * @returns Array of node IDs representing the path
 */
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
