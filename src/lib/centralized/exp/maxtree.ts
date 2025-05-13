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

export function emptyPersistentCache(): PersistentCache {
	return {
		pcSogfMap: null,
		pcProviderShares: new Map()
	};
}


/**
 * GunTree class for managing tree structures using Gun
 * Uses GunNode for persistence and TreeZipper pattern for navigation
 */
export class GunTree {
	private rootPath: string[];

	/**
	 * Create a new GunTree
	 * @param rootPath Base path for the tree in Gun
	 */
	constructor(rootPath: string[] = ['trees']) {
		this.rootPath = rootPath;
	}

	/**
	 * Get a user's tree by their soul (public key or ID)
	 * @param userSoul User's soul in the Gun database
	 * @returns Promise resolving to the loaded TreeZipper or null if not found
	 */
	async getUserTree(userSoul: string, maxDepth = 5): Promise<TreeZipper | null> {
		return this.loadFullNode(userSoul, maxDepth);
	}

	/**
	 * Create a root node in the Gun database
	 * @param id Node identifier
	 * @param name Node name
	 * @param pts Points value
	 * @param contribs Contributors
	 * @param manual Manual fulfillment value
	 * @returns Promise resolving to a TreeZipper
	 */
	async createRootNode(
		id: string,
		name: string,
		pts: number,
		contribs: string[],
		manual: number | null
	): Promise<TreeZipper> {
		// Create node in Gun
		const nodePath = [...this.rootPath, id];
		const nodeRef = new GunNode(nodePath);

		// Store node data
		await nodeRef.put({
			nodeId: id,
			nodeName: name,
			nodePoints: pts,
			nodeContributors: contribs,
			nodeManualFulfillment: this.clampManual(manual)
		});

		// Create an in-memory representation
		const node: Node = {
			nodeId: id,
			nodeName: name,
			nodePoints: pts,
			nodeChildren: new Map(),
			nodeContributors: new Set(contribs),
			nodeManualFulfillment: this.clampManual(manual),
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
	 * Helper function to clamp manual fulfillment between 0 and 1
	 * @param manual The manual fulfillment value
	 * @returns Clamped value or null
	 */
	private clampManual(manual: number | null): number | null {
		if (manual === null) return null;
		return Math.max(0, Math.min(1, manual));
	}

	/**
	 * Add a child node to the current node
	 * @param parentId Parent node ID
	 * @param childId Child node ID
	 * @param name Child node name
	 * @param pts Points value
	 * @param contribs Contributors
	 * @param manual Manual fulfillment value
	 * @returns Promise resolving to the child TreeZipper
	 */
	async addChild(
		parentId: string,
		childId: string,
		name: string,
		pts: number,
		contribs: string[] = [],
		manual: number | null = null
	): Promise<TreeZipper | null> {
		// Get parent node
		const parentNode = await this.fetchNode(parentId);
		if (!parentNode) return null;

		// Don't allow adding children to nodes with contributors
		if (parentNode.zipperCurrent.nodeContributors.size > 0) {
			return null;
		}

		// Create child node in Gun
		const childPath = [...this.rootPath, childId];
		const childRef = new GunNode(childPath);

		await childRef.put({
			nodeId: childId,
			nodeName: name,
			nodePoints: pts,
			nodeContributors: contribs,
			nodeManualFulfillment: this.clampManual(manual)
		});

		// Create the edge from parent to child
		const childrenPath = [...this.rootPath, parentId, 'children'];
		const childrenRef = new GunNode(childrenPath);
		await childrenRef.get(childId).put({ '#': childId });

		// Create child node for in-memory representation
		const childNode: Node = {
			nodeId: childId,
			nodeName: name,
			nodePoints: pts,
			nodeChildren: new Map(),
			nodeContributors: new Set(contribs),
			nodeManualFulfillment: this.clampManual(manual),
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
			zipperCurrent: childNode,
			zipperContext: ctx
		};
	}

	/**
	 * Fetch a node by ID
	 * @param nodeId Node ID
	 * @returns Promise resolving to a TreeZipper
	 */
	async fetchNode(nodeId: string): Promise<TreeZipper | null> {
		try {
			const nodePath = [...this.rootPath, nodeId];
			const nodeRef = new GunNode(nodePath);
			const nodeData = await nodeRef.once();

			if (!nodeData) return null;

			// Convert data to Node structure
			const node: Node = {
				nodeId: nodeData.nodeId || nodeId,
				nodeName: nodeData.nodeName || '',
				nodePoints: nodeData.nodePoints || 0,
				nodeChildren: new Map(),
				nodeContributors: new Set(nodeData.nodeContributors || []),
				nodeManualFulfillment: this.clampManual(nodeData.nodeManualFulfillment),
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
	 * Load full node structure with children
	 * @param nodeId Root node ID
	 * @param maxDepth Maximum depth to load
	 * @returns Promise resolving to a fully loaded TreeZipper
	 */
	async loadFullNode(nodeId: string, maxDepth = 5): Promise<TreeZipper | null> {
		const visited = new Set<string>();

		const loadNodeRecursive = async (id: string, depth = 0): Promise<Node | null> => {
			if (visited.has(id) || depth > maxDepth) return null;
			visited.add(id);

			const zipper = await this.fetchNode(id);
			if (!zipper) return null;

			const node = zipper.zipperCurrent;

			// Load children
			const childrenPath = [...this.rootPath, id, 'children'];
			const childrenRef = new GunNode(childrenPath);

			const children = await new Promise<Record<string, any>>((resolve) => {
				childrenRef
					.once()
					.then((data) => {
						resolve(data || {});
					})
					.catch(() => resolve({}));
			});

			const loadedChildren = new Map<string, Node>();

			// Process each child reference
			const childPromises = Object.keys(children)
				.filter((key) => key !== '_') // Skip Gun metadata
				.map(async (childId) => {
					const child = await loadNodeRecursive(childId, depth + 1);
					if (child) {
						loadedChildren.set(childId, child);
					}
				});

			await Promise.all(childPromises);

			// Load SOGF map if exists
			let pcSogfMap: Map<string, number> | null = null;
			const sogfPath = [...this.rootPath, id, 'persistentCache', 'sogfMap'];
			const sogfRef = new GunNode(sogfPath);

			try {
				const sogfData = await sogfRef.once();
				if (sogfData && sogfData.map) {
					const mapData = JSON.parse(sogfData.map);
					pcSogfMap = new Map(Object.entries(mapData));
				}
			} catch (error) {
				console.error('Error loading SOGF map:', error);
			}

			// Load provider shares if exists
			const pcProviderShares = new Map<number, Map<string, number>>();

			const sharesPath = [...this.rootPath, id, 'providerShares'];
			const sharesRef = new GunNode(sharesPath);

			try {
				const sharesData = await sharesRef.once();
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
			} catch (error) {
				console.error('Error loading provider shares:', error);
			}

			// Create the node with loaded children and cache data
			return {
				...node,
				nodeChildren: loadedChildren,
				nodePersistentCache: {
					pcSogfMap,
					pcProviderShares
				},
				nodeTransientCache: emptyCache()
			};
		};

		const rootNode = await loadNodeRecursive(nodeId);
		if (!rootNode) return null;

		return {
			zipperCurrent: rootNode,
			zipperContext: null
		};
	}

	/**
	 * Enter a child node (pure function)
	 * @param childId Child node ID
	 * @param zipper Current TreeZipper
	 * @returns New TreeZipper or null if child not found
	 */
	navigateToChild(childId: string, zipper: TreeZipper): TreeZipper | null {
		return enterChild(childId, zipper);
	}

	/**
	 * Return to parent node (pure function)
	 * @param zipper Current TreeZipper
	 * @returns New TreeZipper or null if at root
	 */
	navigateToParent(zipper: TreeZipper): TreeZipper | null {
		return exitToParent(zipper);
	}

	/**
	 * Go to root node (pure function)
	 * @param zipper Current TreeZipper
	 * @returns Root TreeZipper
	 */
	navigateToRoot(zipper: TreeZipper): TreeZipper {
		let current = zipper;
		let parent = exitToParent(current);

		while (parent) {
			current = parent;
			parent = exitToParent(current);
		}

		return current;
	}

	/**
	 * Follow a path from current node (pure function)
	 * @param path Navigation path
	 * @param zipper Current TreeZipper
	 * @returns Resulting TreeZipper or null if path invalid
	 */
	navigatePath(path: NavigationPath, zipper: TreeZipper): TreeZipper | null {
		if (path.length === 0) return zipper;

		const [nextId, ...restPath] = path;
		const nextNode = enterChild(nextId, zipper);

		if (!nextNode) return null;
		return this.navigatePath(restPath, nextNode);
	}

	/**
	 * Save a node to Gun (persist changes)
	 * @param zipper TreeZipper containing node to save
	 * @returns Promise resolving when saved
	 */
	async saveNode(zipper: TreeZipper): Promise<void> {
		const node = zipper.zipperCurrent;
		const nodePath = [...this.rootPath, node.nodeId];
		const nodeRef = new GunNode(nodePath);

		// Save basic node data
		await nodeRef.put({
			nodeId: node.nodeId,
			nodeName: node.nodeName,
			nodePoints: node.nodePoints,
			nodeContributors: Array.from(node.nodeContributors),
			nodeManualFulfillment: node.nodeManualFulfillment
		});

		// Save persistent cache data if needed
		if (node.nodePersistentCache.pcSogfMap) {
			const cacheRef = new GunNode([...nodePath, 'persistentCache']);
			await cacheRef.get('sogfMap').put({
				map: JSON.stringify(Object.fromEntries(node.nodePersistentCache.pcSogfMap))
			});
		}

		// Save provider shares
		if (node.nodePersistentCache.pcProviderShares.size > 0) {
			const sharesRef = new GunNode([...nodePath, 'providerShares']);

			for (const [depth, shares] of node.nodePersistentCache.pcProviderShares.entries()) {
				await sharesRef.get(`${depth}`).put({
					map: JSON.stringify(Object.fromEntries(shares))
				});
			}
		}
	}

	/**
	 * Update a node's persistent cache
	 * @param zipper TreeZipper to update
	 * @param updater Function to update the cache
	 * @returns Updated TreeZipper
	 */
	updateCache(zipper: TreeZipper, updater: (pc: PersistentCache) => PersistentCache): TreeZipper {
		return updateNodePersistentCache(updater, zipper);
	}

	/**
	 * Calculate fulfillment for a node
	 * @param zipper TreeZipper to calculate for
	 * @returns Fulfillment value (0-1)
	 */
	calculateFulfillment(zipper: TreeZipper): number {
		return fulfilled(zipper);
	}

	/**
	 * Calculate weight for a node
	 * @param zipper TreeZipper to calculate for
	 * @returns Weight value (0-1)
	 */
	calculateWeight(zipper: TreeZipper): number {
		return weight(zipper);
	}

	/**
	 * Calculate desire for a node
	 * @param zipper TreeZipper to calculate for
	 * @returns Desire value (0-1)
	 */
	calculateDesire(zipper: TreeZipper): number {
		return desire(zipper);
	}

	/**
	 * Calculate mutual fulfillment between two nodes
	 * @param a First node
	 * @param b Second node
	 * @param trees Map of tree zippers by ID
	 * @returns Mutual fulfillment value (0-1)
	 */
	calculateMutualFulfillment(a: TreeZipper, b: TreeZipper, trees: Map<string, TreeZipper>): number {
		return mutualFulfillment(trees, a, b);
	}
}

// ==== Standalone functions for compatibility with calculations.ts ====

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
 * Run a cache computation on a node's transient cache
 * @param computation Function that computes a value and returns it with an updated cache
 * @param node The node to run the computation on
 * @returns The computed value and an updated node
 */
export function runNodeCache<A>(
	computation: (node: Node) => [A, Node['nodeTransientCache']],
	node: Node
): [A, Node] {
	const [result, newCache] = computation(node);
	return [result, { ...node, nodeTransientCache: newCache }];
}

/**
 * Run a cache computation on a zipper
 * @param computation Function that computes a value and returns it with an updated cache
 * @param zipper The zipper to run the computation on
 * @returns Just the computed result (cache update is discarded)
 */
export function withNodeCache<A>(
	computation: (zipper: TreeZipper) => [A, Node['nodeTransientCache']],
	zipper: TreeZipper
): A {
	const [result, _] = computation(zipper);
	return result;
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

/**
 * Create a zipper from a persisted node
 * @param node The node to create a zipper from
 * @returns A TreeZipper with the node as current
 */
export function createZipperFromPersistedNode(node: Node): TreeZipper {
	return {
		zipperCurrent: node,
		zipperContext: null
	};
}

/**
 * Example function to demonstrate how to use GunTree with calculations
 */
export async function exampleUsage() {
	// Create Gun tree manager
	const gunTree = new GunTree(['trees']);

	// Create a map to hold tree zippers for calculations
	const trees = new Map<string, TreeZipper>();

	// Create a root node for a tree
	const rootZipper = await gunTree.createRootNode('tree1', 'Root Node', 100, [], null);

	// Store the root node in our map
	trees.set('tree1', rootZipper);

	// Add children to the tree
	const child1 = await gunTree.addChild('tree1', 'child1', 'Child Node 1', 50);
	const child2 = await gunTree.addChild('tree1', 'child2', 'Child Node 2', 25);

	// Add a contribution node (leaf)
	const contributor1 = await gunTree.addChild('child1', 'contrib1', 'Contributor 1', 20, [
		'user1',
		'user2'
	]);

	if (child1) trees.set('child1', child1);
	if (child2) trees.set('child2', child2);
	if (contributor1) trees.set('contrib1', contributor1);

	// Examples of calculations
	if (rootZipper) {
		// Get the weight of the root node (should be 1.0)
		const rootWeight = gunTree.calculateWeight(rootZipper);
		console.log('Root weight:', rootWeight);

		// Get the fulfillment of the root node
		const rootFulfillment = gunTree.calculateFulfillment(rootZipper);
		console.log('Root fulfillment:', rootFulfillment);

		// Get the desire of the root node
		const rootDesire = gunTree.calculateDesire(rootZipper);
		console.log('Root desire:', rootDesire);
	}

	// Example of mutual fulfillment calculation between nodes
	if (contributor1 && child2) {
		const mutual = gunTree.calculateMutualFulfillment(contributor1, child2, trees);
		console.log('Mutual fulfillment:', mutual);
	}

	// Save changes to the database
	if (rootZipper) await gunTree.saveNode(rootZipper);
	if (child1) await gunTree.saveNode(child1);
	if (child2) await gunTree.saveNode(child2);
	if (contributor1) await gunTree.saveNode(contributor1);

	// Load a tree by soul (user ID)
	const userTree = await gunTree.getUserTree('user1');
	if (userTree) {
		console.log('Loaded user tree:', userTree.zipperCurrent.nodeName);
	}

	return { gunTree, trees };
}
