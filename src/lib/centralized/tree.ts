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
import { user, recallUser, gun } from '../gun/gunSetup';

export function emptyPersistentCache(): PersistentCache {
	return {
		pcSogfMap: null,
		pcProviderShares: new Map()
	};
}

// Add throttling and caching mechanism for tree loading
let lastTreeLoadTime = 0;
let cachedTree: TreeZipper | null = null;
const TREE_LOAD_THROTTLE_MS = 1000; // Only reload once per second at most

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

		// Root nodes should not have contributors
		// The root node represents the user's tree, not a contributor node

		// Create root node in user space
		const rootNode = new GunNode(['tree', 'root']);

		// Store node data with empty contributors
		await rootNode.put({
			nodeId,
			nodeName: name,
			nodePoints: pts,
			nodeContributors: {}, // Empty object for root node
			nodeManualFulfillment: clampManual(manual)
		});

		// Create an in-memory representation
		const node: Node = {
			nodeId,
			nodeName: name,
			nodePoints: pts,
			nodeChildren: new Map(),
			nodeContributors: new Set(), // Empty set for root node
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
		console.log('[GUN FLOW] addChild started:', { parentId, name, pts, childId });

		// Ensure user is authenticated
		if (!user.is?.pub) {
			console.error('[GUN FLOW] User not authenticated');
			throw new Error('User must be authenticated to add a child node');
		}

		// Generate unique ID if not provided
		const actualChildId =
			childId || `node_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`;
		console.log('[GUN FLOW] Using childId:', actualChildId);

		// Get parent node
		console.log('[GUN FLOW] Fetching parent node:', parentId);
		const parentNode = await GunUserTree.fetchNode(parentId);
		if (!parentNode) {
			console.error('[GUN FLOW] Parent node not found');
			return null;
		}
		console.log('[GUN FLOW] Parent node fetched', {
			parentId: parentNode.zipperCurrent.nodeId,
			parentName: parentNode.zipperCurrent.nodeName
		});

		// Don't allow adding children to nodes with contributors
		if (parentNode.zipperCurrent.nodeContributors.size > 0) {
			console.log(
				'[GUN FLOW] Cannot add child to node with contributors:',
				Array.from(parentNode.zipperCurrent.nodeContributors)
			);
			return null;
		}

		// Convert contributors array to Gun-friendly object
		const contributorsObj: Record<string, boolean> = {};
		contribs.forEach((contrib) => {
			contributorsObj[contrib] = true;
		});
		console.log('[GUN FLOW] Converted contributors to Gun object:', contributorsObj);

		// Create child node in Gun under the nodes collection
		console.log('[GUN FLOW] Creating child node in Gun database');
		const childNode = new GunNode(['tree', 'nodes', actualChildId]);

		await childNode.put({
			nodeId: actualChildId,
			nodeName: name,
			nodePoints: pts,
			nodeContributors: contributorsObj, // Use object instead of array
			nodeManualFulfillment: clampManual(manual)
		});
		console.log('[GUN FLOW] Child node data saved to Gun');

		// Create the edge from parent to child by adding a reference
		// Find the parent node reference based on if it's root or a regular node
		const parentRef =
			parentId === user.is.pub
				? new GunNode(['tree', 'root', 'children'])
				: new GunNode(['tree', 'nodes', parentId, 'children']);

		console.log('[GUN FLOW] Creating parent->child reference in Gun');
		const childSoul = `~${user.is.pub}/tree/nodes/${actualChildId}`;
		await parentRef.get(actualChildId).put({ '#': childSoul });
		console.log('[GUN FLOW] Parent->child reference created with soul:', childSoul);

		// Verify the child reference was created
		try {
			const childrenCheck = await parentRef.once();
			console.log(
				'[GUN FLOW] Verifying child reference:',
				childrenCheck && childrenCheck[actualChildId]
					? 'Found'
					: 'Not found - this may cause persistence issues'
			);

			if (!childrenCheck || !childrenCheck[actualChildId]) {
				console.warn('[GUN FLOW] Child reference may not have been properly created');
			}
		} catch (err) {
			console.error('[GUN FLOW] Error verifying child reference:', err);
		}

		// Create child node for in-memory representation
		console.log('[GUN FLOW] Creating in-memory child node representation');
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
		console.log('[GUN FLOW] Creating zipper context');
		const ctx: Ctx = {
			ctxParent: parentNode.zipperCurrent,
			ctxSiblings: new Map(), // We're not loading siblings here for efficiency
			ctxAncestors: parentNode.zipperContext
				? [parentNode.zipperContext, ...parentNode.zipperContext.ctxAncestors]
				: []
		};

		// Return as a zipper
		console.log('[GUN FLOW] Returning new child zipper');
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

			console.log(`[GUN FLOW] Raw nodeContributors:`, JSON.stringify(nodeData.nodeContributors));

			let resolvedContributors = nodeData.nodeContributors;
			if (
				nodeData.nodeContributors &&
				typeof nodeData.nodeContributors === 'object' &&
				nodeData.nodeContributors['#']
			) {
				// It's a soul reference, we need to resolve it
				const soul = nodeData.nodeContributors['#'];
				console.log(`[GUN FLOW] Resolving contributors reference:`, soul);

				// Use Gun directly with the soul to get the data
				const contributorsData = await new Promise<any>((resolve) => {
					gun.get(soul).once((data: any) => resolve(data));
				});

				if (contributorsData) {
					resolvedContributors = contributorsData;
					console.log(`[GUN FLOW] Resolved contributors:`, JSON.stringify(contributorsData));
				}
			}

			// For root nodes, ensure no contributors
			if (isRoot) {
				resolvedContributors = {};
			}

			// Convert data to Node structure
			const node: Node = {
				nodeId: nodeData.nodeId || nodeId,
				nodeName: nodeData.nodeName || '',
				nodePoints: nodeData.nodePoints || 0,
				nodeChildren: new Map(),
				nodeContributors: isRoot
					? new Set()
					: new Set(
							Array.isArray(resolvedContributors)
								? resolvedContributors
								: resolvedContributors && typeof resolvedContributors === 'object'
									? Object.keys(resolvedContributors).filter(
											(key) => key !== '_' && resolvedContributors[key] === true
										)
									: []
						),
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
	 * Load the full tree starting from root node
	 * @param maxDepth Maximum depth to traverse (default: 10)
	 * @param forceReload Force reload even if cache is recent
	 * @returns Promise resolving to TreeZipper
	 */
	static async loadFullTree(maxDepth = 10, forceReload = false): Promise<TreeZipper | null> {
		console.log(
			'[GUN FLOW] loadFullTree started, maxDepth:',
			maxDepth,
			'forceReload:',
			forceReload
		);

		// Ensure user is authenticated
		if (!user.is?.pub) {
			console.error('[GUN FLOW] loadFullTree: User not authenticated');
			throw new Error('User must be authenticated to load the full tree');
		}

		// Check if we have a recent cached tree and don't need a force reload
		const now = Date.now();
		if (!forceReload && cachedTree && now - lastTreeLoadTime < TREE_LOAD_THROTTLE_MS) {
			console.log('[GUN FLOW] Using cached tree, age:', now - lastTreeLoadTime, 'ms');
			return cachedTree;
		}

		// Set up a visited set to prevent cycles
		const visited = new Set<string>();
		console.log('[GUN FLOW] Loading tree for user:', user.is.pub);

		// Recursive function to load a node and its children
		const loadNodeRecursive = async (id: string, depth = 0): Promise<Node | null> => {
			try {
				if (!id || visited.has(id) || depth > maxDepth) return null;
				visited.add(id);
				console.log(`[GUN FLOW] loadNodeRecursive depth ${depth} for node:`, id);

				// Initialize the node data
				let node: Node;

				// Determine if this is the root node or a child node
				const isRoot = id === user?.is?.pub;

				// Load the node data from Gun
				const nodePath = isRoot ? ['tree', 'root'] : ['tree', 'nodes', id];
				const nodeRef = new GunNode(nodePath);
				console.log(`[GUN FLOW] Loading node data from path:`, nodePath);

				const nodeData = await nodeRef.once();
				if (!nodeData) {
					console.log(`[GUN FLOW] No data found for node:`, id);
					return null; // Node doesn't exist
				}

				console.log(`[GUN FLOW] Raw nodeContributors:`, JSON.stringify(nodeData.nodeContributors));

				console.log(`[GUN FLOW] Loaded node data:`, {
					id: nodeData.nodeId,
					name: nodeData.nodeName,
					points: nodeData.nodePoints,
					manual: nodeData.nodeManualFulfillment
				});

				let resolvedContributors = nodeData.nodeContributors;
				if (
					nodeData.nodeContributors &&
					typeof nodeData.nodeContributors === 'object' &&
					nodeData.nodeContributors['#']
				) {
					// It's a soul reference, we need to resolve it
					const soul = nodeData.nodeContributors['#'];
					console.log(`[GUN FLOW] Resolving contributors reference:`, soul);

					// Use Gun directly with the soul to get the data
					const contributorsData = await new Promise<any>((resolve) => {
						gun.get(soul).once((data: any) => resolve(data));
					});

					if (contributorsData) {
						resolvedContributors = contributorsData;
						console.log(`[GUN FLOW] Resolved contributors:`, JSON.stringify(contributorsData));
					}
				}

				// For root nodes, ensure no contributors
				if (isRoot) {
					resolvedContributors = {};
				}

				// Convert contributors object to Set
				const contributors = new Set<string>();
				if (!isRoot && resolvedContributors) {
					// If it's an array (old format), use it directly
					if (Array.isArray(resolvedContributors)) {
						resolvedContributors.forEach((contrib: string) => contributors.add(contrib));
					}
					// If it's an object (new format), use keys where value is true
					else if (typeof resolvedContributors === 'object') {
						Object.entries(resolvedContributors).forEach(([key, value]) => {
							if (value === true && key !== '_') {
								contributors.add(key);
							}
						});
					}
				}
				console.log(`[GUN FLOW] Converted contributors:`, Array.from(contributors));

				// Create a node object
				node = {
					nodeId: nodeData.nodeId || id,
					nodeName: nodeData.nodeName || 'Unnamed Node',
					nodePoints: nodeData.nodePoints || 0,
					nodeChildren: new Map(),
					nodeContributors: contributors,
					nodeManualFulfillment: clampManual(nodeData.nodeManualFulfillment),
					nodeCapacities: new Map(),
					nodeCapacityShares: new Map(),
					nodePersistentCache: emptyPersistentCache(),
					nodeTransientCache: emptyCache()
				};

				// Load children
				const childrenPath = isRoot
					? ['tree', 'root', 'children']
					: ['tree', 'nodes', id, 'children'];

				try {
					const childrenRef = new GunNode(childrenPath);
					const children = await childrenRef.once();

					// No children or invalid data, return the node as is
					if (!children || typeof children !== 'object') return node;
					console.log(
						`[GUN FLOW] Found children for node ${id}, count:`,
						Object.keys(children).filter((key) => key !== '_').length
					);

					const loadedChildren = new Map<string, Node>();

					// Process children asynchronously
					const childPromises = Object.keys(children)
						.filter((key) => key !== '_') // Skip Gun metadata
						.map(async (childId) => {
							try {
								// Extract the actual node ID from the reference or use the key
								const childData = children[childId];
								let actualChildId = childId;

								if (childData && typeof childData === 'object' && childData['#']) {
									// Extract the actual node ID from the soul reference
									const soul = childData['#'];
									console.log(`[GUN FLOW] Resolving child reference soul:`, soul);

									// Soul format is typically ~USER_PUB/tree/nodes/NODE_ID
									const parts = soul.split('/');
									actualChildId = parts.length > 0 ? parts[parts.length - 1] : childId;
									console.log(`[GUN FLOW] Extracted child ID from soul:`, actualChildId);
								}
								console.log(`[GUN FLOW] Processing child ${childId}, resolved ID:`, actualChildId);

								const child = await loadNodeRecursive(actualChildId, depth + 1);
								if (child) {
									loadedChildren.set(childId, child);
									console.log(`[GUN FLOW] Added child ${actualChildId} to node ${id}`);
								}
							} catch (childError) {
								console.error(`[GUN FLOW] Error loading child node ${childId}:`, childError);
								// Continue with other children even if one fails
							}
						});

					// Wait for all child loads to complete (or fail)
					await Promise.all(childPromises);
					console.log(`[GUN FLOW] All children loaded for node ${id}, count:`, loadedChildren.size);

					// Update the node with loaded children
					return {
						...node,
						nodeChildren: loadedChildren
					};
				} catch (childrenError) {
					console.error(`[GUN FLOW] Error loading children for node ${id}:`, childrenError);
					// Return the node without children if we can't load them
					return node;
				}
			} catch (nodeError) {
				console.error(`[GUN FLOW] Error in loadNodeRecursive for node ${id}:`, nodeError);
				return null;
			}
		};

		try {
			// Start loading from the root node (user's public key)
			console.log('[GUN FLOW] Starting tree load from root:', user.is.pub);
			const rootNode = await loadNodeRecursive(user.is.pub);
			if (!rootNode) {
				console.error('[GUN FLOW] Failed to load root node');
				return null;
			}
			console.log('[GUN FLOW] Tree loaded successfully, node count:', visited.size);

			// Update cache
			cachedTree = {
				zipperCurrent: rootNode,
				zipperContext: null
			};
			lastTreeLoadTime = now;

			// Return in zipper format
			return cachedTree;
		} catch (error) {
			console.error('[GUN FLOW] Error loading full tree:', error);
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
		console.log('[GUN FLOW] saveNode started', {
			nodeId: zipper.zipperCurrent.nodeId,
			nodeName: zipper.zipperCurrent.nodeName
		});

		// Ensure user is authenticated
		if (!user.is?.pub) {
			console.error('[GUN FLOW] User not authenticated');
			throw new Error('User must be authenticated to save a node');
		}

		const node = zipper.zipperCurrent;
		const isRoot = node.nodeId === user.is.pub;
		console.log('[GUN FLOW] Saving node, isRoot:', isRoot);

		// Determine the correct path for this node
		const nodePath = isRoot ? ['tree', 'root'] : ['tree', 'nodes', node.nodeId];
		const nodeRef = new GunNode(nodePath);
		console.log('[GUN FLOW] Node path in Gun:', nodePath);

		// Convert contributors Set to Gun-friendly object
		const contributorsObj: Record<string, boolean> = {};
		node.nodeContributors.forEach((contrib) => {
			contributorsObj[contrib] = true;
		});
		console.log('[GUN FLOW] Contributors:', contributorsObj);

		// Save basic node data
		console.log('[GUN FLOW] Saving basic node data');
		await nodeRef.put({
			nodeId: node.nodeId,
			nodeName: node.nodeName,
			nodePoints: node.nodePoints,
			nodeContributors: contributorsObj, // Use object instead of array
			nodeManualFulfillment: node.nodeManualFulfillment
		});
		console.log('[GUN FLOW] Basic node data saved');

		// If this node has children references, save those too
		if (node.nodeChildren.size > 0) {
			console.log('[GUN FLOW] Node has children, count:', node.nodeChildren.size);
			const childrenPath = isRoot
				? ['tree', 'root', 'children']
				: ['tree', 'nodes', node.nodeId, 'children'];
			const childrenRef = new GunNode(childrenPath);
			console.log('[GUN FLOW] Children path in Gun:', childrenPath);

			// Save each child reference
			let childCount = 0;
			for (const [childKey, childNode] of node.nodeChildren.entries()) {
				console.log('[GUN FLOW] Saving child reference:', childKey, '→', childNode.nodeId);
				await childrenRef.get(childKey).put({
					'#': `~${user.is.pub}/tree/nodes/${childNode.nodeId}`
				});
				childCount++;
			}
			console.log('[GUN FLOW] All child references saved, count:', childCount);
		} else {
			console.log('[GUN FLOW] Node has no children to save');
		}

		console.log('[GUN FLOW] saveNode completed for:', node.nodeId);
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
