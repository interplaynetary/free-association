import { GunNode } from '../gun/GunNode';
import type {
	Node,
	TreeZipper,
	Ctx,
	NavigationPath,
	PersistentCache,
	Cache,
	CacheValue
} from './types';
import { emptyCache, cacheLookup, cacheInsert } from './cache';
import { weight, fulfilled, desire, mutualFulfillment } from './calculations';
import { user, recallUser } from '../gun/gunSetup';

export function emptyPersistentCache(): PersistentCache {
	return {
		pcSogfMap: null,
		pcProviderShares: new Map()
	};
}

/**
 * GunUserTree class for managing tree structures using Gun within a user's secure space
 * Uses GunNode for persistence and TreeZipper pattern for navigation
 */
export class GunUserTree {
	/**
	 * Initialize GunUserTree and ensure user authentication
	 * @returns Promise that resolves when initialization is complete
	 */
	static async initialize(): Promise<void> {
		// Ensure user is authenticated before proceeding
		await recallUser();

		if (!user.is?.pub) {
			throw new Error('User must be authenticated to use GunUserTree');
		}
	}

	/**
	 * Create a root node in the user's tree
	 * @param name Node name
	 * @param pts Points value
	 * @param contribs Contributors (defaults to current user)
	 * @param manual Manual fulfillment value
	 * @returns Promise resolving to a TreeZipper
	 */
	static async createRootNode(
		name: string,
		pts: number,
		contribs?: string[],
		manual: number | null = null
	): Promise<TreeZipper> {
		// Ensure user is authenticated
		if (!user.is?.pub) {
			throw new Error('User must be authenticated to create a root node');
		}

		const userPub = user.is.pub;
		const nodeId = userPub; // Root node ID is the user's public key

		// Use contributors or default to current user
		const contributors = contribs || [userPub];

		// Create root node in user space
		const rootNode = new GunNode(['tree', 'root']);

		// Convert contributors array to Gun-friendly object
		const contributorsObj: Record<string, boolean> = {};
		contributors.forEach((contrib) => {
			contributorsObj[contrib] = true;
		});

		// Store node data
		await rootNode.put({
			nodeId,
			nodeName: name,
			nodePoints: pts,
			nodeContributors: contributorsObj, // Use object instead of array
			nodeManualFulfillment: clampManual(manual)
		});

		// Create an in-memory representation
		const node: Node = {
			nodeId,
			nodeName: name,
			nodePoints: pts,
			nodeChildren: new Map(),
			nodeContributors: new Set(contributors),
			nodeManualFulfillment: clampManual(manual),
			nodeCapacities: new Map(),
			nodeCapacityShares: new Map(),
			nodePersistentCache: emptyPersistentCache(),
			nodeTransientCache: emptyCache()
		};

		// Return as a zipper
		return {
			zipperCurrent: node,
			zipperContext: null
		};
	}

	/**
	 * Add a child node to the current node
	 * @param parentId Parent node ID
	 * @param childId Child node ID (optional, will generate unique ID if not provided)
	 * @param name Child node name
	 * @param pts Points value
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
		const parentNode = await GunUserTree.fetchNode(parentId);
		if (!parentNode) return null;

		// Don't allow adding children to nodes with contributors
		if (parentNode.zipperCurrent.nodeContributors.size > 0) {
			return null;
		}

		// Convert contributors array to Gun-friendly object
		const contributorsObj: Record<string, boolean> = {};
		contribs.forEach((contrib) => {
			contributorsObj[contrib] = true;
		});

		// Create child node in Gun under the nodes collection
		const childNode = new GunNode(['tree', 'nodes', actualChildId]);

		await childNode.put({
			nodeId: actualChildId,
			nodeName: name,
			nodePoints: pts,
			nodeContributors: contributorsObj, // Use object instead of array
			nodeManualFulfillment: clampManual(manual)
		});

		// Create the edge from parent to child by adding a reference
		// Find the parent node reference based on if it's root or a regular node
		const parentRef =
			parentId === user.is.pub
				? new GunNode(['tree', 'root', 'children'])
				: new GunNode(['tree', 'nodes', parentId, 'children']);

		await parentRef.get(actualChildId).put({ '#': `~${user.is.pub}/tree/nodes/${actualChildId}` });

		// Create child node for in-memory representation
		const childNodeObj: Node = {
			nodeId: actualChildId,
			nodeName: name,
			nodePoints: pts,
			nodeChildren: new Map(),
			nodeContributors: new Set(contribs),
			nodeManualFulfillment: clampManual(manual),
			nodeCapacities: new Map(),
			nodeCapacityShares: new Map(),
			nodePersistentCache: emptyPersistentCache(),
			nodeTransientCache: emptyCache()
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

			if (!nodeData) return null;

			// Convert contributors object to a Set
			const contributors = new Set<string>();
			if (nodeData.nodeContributors) {
				// If it's an array (old format), use it directly
				if (Array.isArray(nodeData.nodeContributors)) {
					nodeData.nodeContributors.forEach((contrib: string) => contributors.add(contrib));
				}
				// If it's an object (new format), use keys where value is true
				else if (typeof nodeData.nodeContributors === 'object') {
					Object.entries(nodeData.nodeContributors).forEach(([key, value]) => {
						if (value === true && key !== '_') {
							contributors.add(key);
						}
					});
				}
			}

			// Convert data to Node structure
			const node: Node = {
				nodeId: nodeData.nodeId || nodeId,
				nodeName: nodeData.nodeName || '',
				nodePoints: nodeData.nodePoints || 0,
				nodeChildren: new Map(),
				nodeContributors: contributors,
				nodeManualFulfillment: clampManual(nodeData.nodeManualFulfillment),
				nodeCapacities: new Map(),
				nodeCapacityShares: new Map(),
				nodePersistentCache: emptyPersistentCache(),
				nodeTransientCache: emptyCache()
			};

			return {
				zipperCurrent: node,
				zipperContext: null
			};
		} catch (error) {
			console.error('Error fetching node:', error);
			return null;
		}
	}

	/**
	 * Load full tree structure with children
	 * @param maxDepth Maximum depth to load (default: 5)
	 * @returns Promise resolving to a fully loaded TreeZipper
	 */
	static async loadFullTree(maxDepth = 5): Promise<TreeZipper | null> {
		// Ensure user is authenticated
		if (!user.is?.pub) {
			throw new Error('User must be authenticated to load a tree');
		}

		const visited = new Set<string>();

		const loadNodeRecursive = async (id: string, depth = 0): Promise<Node | null> => {
			try {
				if (!id || visited.has(id) || depth > maxDepth) return null;
				visited.add(id);

				const zipper = await GunUserTree.fetchNode(id);
				if (!zipper) return null;

				const node = zipper.zipperCurrent;

				// Load children - different path based on node type
				const isRoot = id === user?.is?.pub;
				const childrenPath = isRoot
					? ['tree', 'root', 'children']
					: ['tree', 'nodes', id, 'children'];

				try {
					const childrenRef = new GunNode(childrenPath);
					const children = await childrenRef.once();

					// No children or invalid data, return the node as is
					if (!children || typeof children !== 'object') return node;

					const loadedChildren = new Map<string, Node>();

					// Process each child reference
					const childPromises = Object.keys(children)
						.filter((key) => key !== '_') // Skip Gun metadata
						.map(async (childId) => {
							try {
								// Extract the actual node ID from the reference or use the key
								const childData = children[childId];
								let actualChildId = childId;

								if (childData && typeof childData === 'object' && childData['#']) {
									const parts = childData['#'].split('/');
									actualChildId = parts.length > 0 ? parts[parts.length - 1] : childId;
								}

								const child = await loadNodeRecursive(actualChildId, depth + 1);
								if (child) {
									loadedChildren.set(childId, child);
								}
							} catch (childError) {
								console.error(`Error loading child node ${childId}:`, childError);
								// Continue with other children even if one fails
							}
						});

					// Wait for all child loads to complete (or fail)
					await Promise.all(childPromises);

					// Create the node with loaded children
					return {
						...node,
						nodeChildren: loadedChildren
					};
				} catch (childrenError) {
					console.error(`Error loading children for node ${id}:`, childrenError);
					// Return the node without children if we can't load them
					return node;
				}
			} catch (nodeError) {
				console.error(`Error in loadNodeRecursive for node ${id}:`, nodeError);
				return null;
			}
		};

		try {
			// Start loading from the root node (user's public key)
			const rootNode = await loadNodeRecursive(user.is.pub);
			if (!rootNode) return null;

			return {
				zipperCurrent: rootNode,
				zipperContext: null
			};
		} catch (error) {
			console.error('Error loading full tree:', error);
			return null;
		}
	}

	/**
	 * Load SOGF map and provider shares from the user's tree
	 * @param zipper TreeZipper to update with cache data
	 * @returns Updated TreeZipper with cache data
	 */
	static async loadCacheData(zipper: TreeZipper): Promise<TreeZipper> {
		// Ensure user is authenticated
		if (!user.is?.pub) {
			throw new Error('User must be authenticated to load cache data');
		}

		try {
			// Load SOGF map
			const sogfRef = new GunNode(['tree', 'persistentCache', 'sogfMap']);
			const sogfData = await sogfRef.once();

			let pcSogfMap: Map<string, number> | null = null;
			if (sogfData && sogfData.map) {
				const mapData = JSON.parse(sogfData.map);
				pcSogfMap = new Map(Object.entries(mapData));
			}

			// Load provider shares
			const sharesRef = new GunNode(['tree', 'providerShares']);
			const sharesData = await sharesRef.once();

			const pcProviderShares = new Map<number, Map<string, number>>();

			if (sharesData) {
				// Process each depth entry
				for (const depthKey of Object.keys(sharesData).filter((k) => k !== '_')) {
					const depth = parseInt(depthKey, 10);
					if (isNaN(depth)) continue;

					const depthData = await sharesRef.get(depthKey).once();
					if (depthData && depthData.map) {
						const mapData = JSON.parse(depthData.map);
						pcProviderShares.set(depth, new Map(Object.entries(mapData)));
					}
				}
			}

			// Update zipper with cache data
			return updateNodePersistentCache(
				() => ({
					pcSogfMap,
					pcProviderShares
				}),
				zipper
			);
		} catch (error) {
			console.error('Error loading cache data:', error);
			return zipper; // Return original zipper on error
		}
	}

	/**
	 * Save a node to Gun (persist changes)
	 * @param zipper TreeZipper containing node to save
	 * @returns Promise resolving when saved
	 */
	static async saveNode(zipper: TreeZipper): Promise<void> {
		// Ensure user is authenticated
		if (!user.is?.pub) {
			throw new Error('User must be authenticated to save a node');
		}

		const node = zipper.zipperCurrent;
		const isRoot = node.nodeId === user.is.pub;

		// Determine the correct path for this node
		const nodePath = isRoot ? ['tree', 'root'] : ['tree', 'nodes', node.nodeId];
		const nodeRef = new GunNode(nodePath);

		// Convert contributors Set to Gun-friendly object
		const contributorsObj: Record<string, boolean> = {};
		node.nodeContributors.forEach((contrib) => {
			contributorsObj[contrib] = true;
		});

		// Save basic node data
		await nodeRef.put({
			nodeId: node.nodeId,
			nodeName: node.nodeName,
			nodePoints: node.nodePoints,
			nodeContributors: contributorsObj, // Use object instead of array
			nodeManualFulfillment: node.nodeManualFulfillment
		});

		// If this node has children references, save those too
		if (node.nodeChildren.size > 0) {
			const childrenPath = isRoot
				? ['tree', 'root', 'children']
				: ['tree', 'nodes', node.nodeId, 'children'];
			const childrenRef = new GunNode(childrenPath);

			// Save each child reference
			for (const [childKey, childNode] of node.nodeChildren.entries()) {
				await childrenRef.get(childKey).put({
					'#': `~${user.is.pub}/tree/nodes/${childNode.nodeId}`
				});
			}
		}
	}

	
	/**
	 * Delete a node and its children from Gun database
	 * @param nodeId ID of the node to delete
	 * @returns Promise resolving to true if deletion was successful
	 */
	static async deleteNode(nodeId: string): Promise<boolean> {
		console.log('[GUN FLOW] deleteNode started:', nodeId);

		// Ensure user is authenticated
		if (!user.is?.pub) {
			console.error('[GUN FLOW] User not authenticated');
			throw new Error('User must be authenticated to delete a node');
		}

		try {
			// Don't allow deleting the root node
			if (nodeId === user.is.pub) {
				console.error('[GUN FLOW] Cannot delete root node');
				return false;
			}

			// Get node reference
			const nodePath = ['tree', 'nodes', nodeId];
			const nodeRef = new GunNode(nodePath);

			// Get node data to check for children
			const nodeData = await nodeRef.once();

			if (!nodeData) {
				console.error('[GUN FLOW] Node not found:', nodeId);
				return false;
			}

			// First, recursively delete all children
			// Get children references
			const childrenRef = new GunNode([...nodePath, 'children']);
			const children = await childrenRef.once();

			if (children && typeof children === 'object') {
				// Process each child
				const childPromises = Object.keys(children)
					.filter((key) => key !== '_') // Skip Gun metadata
					.map(async (childId) => {
						try {
							// Extract the actual node ID from the reference
							const childData = children[childId];
							let actualChildId = childId;

							if (childData && typeof childData === 'object' && childData['#']) {
								const parts = childData['#'].split('/');
								actualChildId = parts.length > 0 ? parts[parts.length - 1] : childId;
							}

							// Recursively delete this child
							console.log(`[GUN FLOW] Deleting child ${actualChildId} of node ${nodeId}`);
							await GunUserTree.deleteNode(actualChildId);
						} catch (error) {
							console.error(`[GUN FLOW] Error deleting child ${childId}:`, error);
						}
					});

				// Wait for all child deletions to complete
				await Promise.all(childPromises);
			}

			// Now delete the node itself
			console.log(`[GUN FLOW] Deleting node data for ${nodeId}`);

			// 1. Delete children references
			await childrenRef.put(null);

			// 2. Delete the node data
			await nodeRef.put(null);

			console.log(`[GUN FLOW] Node ${nodeId} deleted successfully`);
			return true;
		} catch (error) {
			console.error('[GUN FLOW] Error deleting node:', error);
			return false;
		}
	}

	/**
	 * Save cache data (SOGF map and provider shares)
	 * @param zipper TreeZipper containing cache data to save
	 * @returns Promise resolving when saved
	 */
	static async saveCacheData(zipper: TreeZipper): Promise<void> {
		// Ensure user is authenticated
		if (!user.is?.pub) {
			throw new Error('User must be authenticated to save cache data');
		}

		const node = zipper.zipperCurrent;

		// Save persistent cache data if needed
		if (node.nodePersistentCache.pcSogfMap) {
			const cacheRef = new GunNode(['tree', 'persistentCache']);
			await cacheRef.get('sogfMap').put({
				map: JSON.stringify(Object.fromEntries(node.nodePersistentCache.pcSogfMap))
			});
		}

		// Save provider shares
		if (node.nodePersistentCache.pcProviderShares.size > 0) {
			const sharesRef = new GunNode(['tree', 'providerShares']);

			for (const [depth, shares] of node.nodePersistentCache.pcProviderShares.entries()) {
				await sharesRef.get(`${depth}`).put({
					map: JSON.stringify(Object.fromEntries(shares))
				});
			}
		}
	}

	/**
	 * Calculate fulfillment for a node
	 * @param zipper TreeZipper to calculate for
	 * @returns Fulfillment value (0-1)
	 */
	static calculateFulfillment(zipper: TreeZipper): number {
		return fulfilled(zipper);
	}

	/**
	 * Calculate weight for a node
	 * @param zipper TreeZipper to calculate for
	 * @returns Weight value (0-1)
	 */
	static calculateWeight(zipper: TreeZipper): number {
		return weight(zipper);
	}

	/**
	 * Calculate desire for a node
	 * @param zipper TreeZipper to calculate for
	 * @returns Desire value (0-1)
	 */
	static calculateDesire(zipper: TreeZipper): number {
		return desire(zipper);
	}

	/**
	 * Calculate mutual fulfillment between two nodes
	 * @param a First node
	 * @param b Second node
	 * @param trees Map of tree zippers by ID
	 * @returns Mutual fulfillment value (0-1)
	 */
	static calculateMutualFulfillment(
		a: TreeZipper,
		b: TreeZipper,
		trees: Map<string, TreeZipper>
	): number {
		return mutualFulfillment(trees, a, b);
	}
}

// Helper functions (kept outside the class for compatibility with calculations.ts)

// Helper function to clamp manual fulfillment between 0 and 1
function clampManual(manual: number | null): number | null {
	if (manual === null) return null;
	return Math.max(0, Math.min(1, manual));
}

/**
 * Pure function to enter a child node
 * @param childId Child node ID
 * @param zipper Current TreeZipper
 * @returns New TreeZipper or null if child not found
 */
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

/**
 * Pure function to exit to parent node
 * @param zipper Current TreeZipper
 * @returns New TreeZipper or null if at root
 */
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

/**
 * Update a node's persistent cache
 * @param updater Function that transforms the persistent cache
 * @param zipper TreeZipper to update
 * @returns Updated TreeZipper
 */
export function updateNodePersistentCache(
	updater: (pc: PersistentCache) => PersistentCache,
	zipper: TreeZipper
): TreeZipper {
	const node = zipper.zipperCurrent;
	return {
		...zipper,
		zipperCurrent: {
			...node,
			nodePersistentCache: updater(node.nodePersistentCache)
		}
	};
}

