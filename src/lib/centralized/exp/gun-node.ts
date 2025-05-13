import type { Node, TreeZipper, Ctx, NavigationPath, PersistentCache, ShareMap } from '../types';
import { emptyCache } from '../cache';

// Define Gun type to fix typescript errors
type GunUser = any;
type GunInstance = any;

// Create an empty persistent cache
export function emptyPersistentCache(): PersistentCache {
	return {
		pcSogfMap: null,
		pcProviderShares: new Map()
	};
}

// Initialize Gun
export function initGun(relayUrls: string[] = ['https://gun-manhattan.herokuapp.com/gun']): {
	gun: GunInstance;
	user: GunUser;
} {
	const Gun = (window as any).Gun;
	const gun = Gun(relayUrls);
	const user = gun.user();
	return { gun, user };
}

// Authenticate a Gun user
export async function authenticateUser(
	user: GunUser,
	alias: string,
	passphrase: string
): Promise<boolean> {
	return new Promise((resolve) => {
		user.auth(alias, passphrase, (ack: any) => {
			resolve(!!ack.sea);
		});
	});
}

// Create user account
export async function createUserAccount(
	user: GunUser,
	alias: string,
	passphrase: string
): Promise<boolean> {
	return new Promise((resolve) => {
		user.create(alias, passphrase, (ack: any) => {
			resolve(!ack.err);
		});
	});
}

// === Node Operations ===

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
		nodePersistentCache: emptyPersistentCache(),
		nodeTransientCache: emptyCache()
	};
}

// Helper function to clamp manual fulfillment between 0 and 1
function clampManual(manual: number | null): number | null {
	if (manual === null) return null;
	return Math.max(0, Math.min(1, manual));
}

// Save a node to Gun
export async function saveNode(user: GunUser, node: Node): Promise<void> {
	await user
		.get('nodes')
		.get(node.nodeId)
		.put({
			id: node.nodeId,
			name: node.nodeName,
			points: node.nodePoints,
			contributors: JSON.stringify(Array.from(node.nodeContributors)),
			manualFulfillment: node.nodeManualFulfillment
		});

	// Save children relationships
	for (const [childId, _] of node.nodeChildren) {
		await saveChildRelation(user, node.nodeId, childId);
	}
}

// Save parent-child relationship
export async function saveChildRelation(
	user: GunUser,
	parentId: string,
	childId: string
): Promise<void> {
	await user.get('edges').get(`${parentId}_${childId}`).put({
		parent: parentId,
		child: childId
	});
}

// Load a node from Gun
export async function loadNode(user: GunUser, nodeId: string): Promise<Node | null> {
	return new Promise((resolve) => {
		user
			.get('nodes')
			.get(nodeId)
			.once((data: any) => {
				if (!data) {
					resolve(null);
					return;
				}

				// Create basic node
				const node = createRootNode(
					data.id,
					data.name,
					data.points,
					JSON.parse(data.contributors || '[]'),
					data.manualFulfillment
				);

				// Load children
				loadChildren(user, node).then((nodeWithChildren) => {
					// Load persistent cache
					loadPersistentCache(user, nodeWithChildren).then((completeNode) => {
						resolve(completeNode);
					});
				});
			});
	});
}

// Load children for a node
async function loadChildren(user: GunUser, node: Node): Promise<Node> {
	return new Promise((resolve) => {
		const children = new Map();
		let pending = 0;
		let completed = false;

		// Helper to check if all children are loaded
		const checkComplete = () => {
			if (completed && pending === 0) {
				resolve({
					...node,
					nodeChildren: children
				});
			}
		};

		// Find all edges where this node is the parent
		user
			.get('edges')
			.map()
			.once((edge: any) => {
				if (edge && edge.parent === node.nodeId) {
					pending++;

					// Load the child node
					loadNode(user, edge.child).then((childNode) => {
						if (childNode) {
							children.set(childNode.nodeId, childNode);
						}
						pending--;
						checkComplete();
					});
				}
			});

		// Handle case of no children
		setTimeout(() => {
			completed = true;
			checkComplete();
		}, 100);
	});
}

// === Cache Operations ===

// Save persistent cache for a node
export async function savePersistentCache(user: GunUser, node: Node): Promise<void> {
	const pc = node.nodePersistentCache;

	// Save SOGF if it exists
	if (pc.pcSogfMap) {
		const sogfObj = Object.fromEntries(pc.pcSogfMap);
		await user
			.get('sogf')
			.get(node.nodeId)
			.put({
				map: JSON.stringify(sogfObj)
			});
	}

	// Save provider shares for each depth
	for (const [depth, shares] of pc.pcProviderShares.entries()) {
		const sharesObj = Object.fromEntries(shares);
		await user
			.get('providerShares')
			.get(`${node.nodeId}_${depth}`)
			.put({
				nodeId: node.nodeId,
				depth,
				map: JSON.stringify(sharesObj)
			});
	}
}

// Load persistent cache for a node
export async function loadPersistentCache(user: GunUser, node: Node): Promise<Node> {
	// Load SOGF map
	const sogfMap = await new Promise<Map<string, number> | null>((resolve) => {
		user
			.get('sogf')
			.get(node.nodeId)
			.once((data: any) => {
				if (data && data.map) {
					resolve(new Map(Object.entries(JSON.parse(data.map))));
				} else {
					resolve(null);
				}
			});

		// Handle timeout
		setTimeout(() => resolve(null), 500);
	});

	// Load provider shares for depths 1-5
	const providerSharesMap = new Map<number, Map<string, number>>();

	for (let depth = 1; depth <= 5; depth++) {
		const shares = await new Promise<Map<string, number> | null>((resolve) => {
			user
				.get('providerShares')
				.get(`${node.nodeId}_${depth}`)
				.once((data: any) => {
					if (data && data.map) {
						resolve(new Map(Object.entries(JSON.parse(data.map))));
					} else {
						resolve(null);
					}
				});

			// Handle timeout
			setTimeout(() => resolve(null), 500);
		});

		if (shares && shares.size > 0) {
			providerSharesMap.set(depth, shares);
		}
	}

	// Update the node with loaded cache
	return {
		...node,
		nodePersistentCache: {
			pcSogfMap: sogfMap,
			pcProviderShares: providerSharesMap
		}
	};
}

// === Tree Building and Management ===

// Create a root node and persist it
export async function createPersistentRoot(
	user: GunUser,
	id: string,
	name: string,
	pts: number,
	contribs: string[] = [],
	manual: number | null = null
): Promise<Node> {
	// Create in-memory node
	const node = createRootNode(id, name, pts, contribs, manual);

	// Persist to Gun
	await saveNode(user, node);

	// Register as a root
	await user.get('roots').get(id).put(true);

	return node;
}

// Build entire tree from root
export async function buildTreeFromRoot(user: GunUser, rootId: string): Promise<Node | null> {
	return loadNode(user, rootId);
}

// Get all root nodes for a user
export async function getAllRoots(user: GunUser): Promise<string[]> {
	return new Promise((resolve) => {
		const roots: string[] = [];
		user
			.get('roots')
			.map()
			.once((value: any, key: string) => {
				if (value === true && key !== '_') {
					roots.push(key);
				}
			});

		// Return after a timeout to ensure we've collected all roots
		setTimeout(() => {
			resolve(roots);
		}, 100);
	});
}

// ==== Tree Zipper Navigation ====

// Create a zipper from a node
export function createZipperFromNode(node: Node): TreeZipper {
	return {
		zipperCurrent: node,
		zipperContext: null
	};
}

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

// Pure function to modify a node
export function modifyNode(modify: (node: Node) => Node, zipper: TreeZipper): TreeZipper {
	return {
		...zipper,
		zipperCurrent: modify(zipper.zipperCurrent)
	};
}

// Modify a node and persist changes
export async function modifyNodeAndPersist(
	user: GunUser,
	modify: (node: Node) => Node,
	zipper: TreeZipper
): Promise<TreeZipper> {
	// First modify in memory
	const updatedZipper = modifyNode(modify, zipper);

	// Persist the changes
	await saveNode(user, updatedZipper.zipperCurrent);

	return updatedZipper;
}

// Add a child node (pure function)
export function addChild(
	childId: string,
	childName: string,
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
	const newChild = createRootNode(childId, childName, pts, contribs, manual);

	// Immutably update the parent's children
	return modifyNode((node) => {
		const updatedChildren = new Map(node.nodeChildren);
		updatedChildren.set(childId, newChild);

		return {
			...node,
			nodeChildren: updatedChildren,
			// Clear both caches since structure has changed
			nodePersistentCache: emptyPersistentCache(),
			nodeTransientCache: emptyCache()
		};
	}, zipper);
}

// Add a child node and persist
export async function addChildAndPersist(
	user: GunUser,
	parentZipper: TreeZipper,
	childId: string,
	childName: string,
	childPoints: number,
	childContribs: string[] = [],
	childManual: number | null = null
): Promise<TreeZipper | null> {
	// First add child in memory
	const updatedZipper = addChild(
		childId,
		childName,
		childPoints,
		childContribs,
		childManual,
		parentZipper
	);

	if (!updatedZipper) return null; // Addition failed

	// Save the child node
	await saveNode(user, updatedZipper.zipperCurrent.nodeChildren.get(childId)!);

	// Create the relationship
	await saveChildRelation(user, parentZipper.zipperCurrent.nodeId, childId);

	return updatedZipper;
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

// Add contributors and persist changes
export async function addContributorsAndPersist(
	user: GunUser,
	zipper: TreeZipper,
	contributors: string[]
): Promise<TreeZipper> {
	// First modify in memory
	const updatedZipper = addContributors(contributors, zipper);

	// Save the updated node
	await saveNode(user, updatedZipper.zipperCurrent);

	// Delete any child edges from the database
	const nodeId = zipper.zipperCurrent.nodeId;
	const edges = await user.get('edges').map().once();
	for (const key in edges) {
		if (key === '_') continue; // Skip Gun metadata
		const edge = edges[key];
		if (edge && edge.parent === nodeId) {
			await user.get('edges').get(key).put(null); // Delete edge
		}
	}

	return updatedZipper;
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

// Update a node's persistent cache and persist to database
export async function updateNodePersistentCacheAndPersist(
	user: GunUser,
	updater: (pc: PersistentCache) => PersistentCache,
	zipper: TreeZipper
): Promise<TreeZipper> {
	// First update in memory
	const updatedZipper = updateNodePersistentCache(updater, zipper);

	// Save the cache to database
	await savePersistentCache(user, updatedZipper.zipperCurrent);

	return updatedZipper;
}

// Subscribe to updates to a tree
export function subscribeToTree(
	user: GunUser,
	rootId: string,
	callback: (update: any) => void
): () => void {
	// Track subscriptions to cleanup later
	const subscriptions: (() => void)[] = [];

	// Subscribe to node changes
	const nodeUnsub = user
		.get('nodes')
		.map()
		.on((node: any, key: string) => {
			if (node) {
				callback({
					type: 'nodeChanged',
					nodeId: key,
					data: node
				});
			}
		});
	subscriptions.push(nodeUnsub);

	// Subscribe to edge changes
	const edgeUnsub = user
		.get('edges')
		.map()
		.on((edge: any) => {
			if (edge && (edge.parent === rootId || edge.child === rootId)) {
				callback({
					type: 'edgeChanged',
					nodeId: edge.parent,
					data: edge
				});
			}
		});
	subscriptions.push(edgeUnsub);

	// Return cleanup function
	return () => {
		subscriptions.forEach((unsub) => unsub());
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

// Get all siblings of the current node
export function getSiblings(zipper: TreeZipper): string[] {
	const parent = exitToParent(zipper);
	if (!parent) return [];
	return Array.from(parent.zipperCurrent.nodeChildren.keys());
}

// Safe navigation with path
export function followPath(path: NavigationPath, zipper: TreeZipper): TreeZipper | null {
	// Base case - empty path returns current zipper
	if (path.length === 0) return zipper;

	// Recursive case - follow first element, then rest of path
	const [nextId, ...restPath] = path;
	const nextNode = enterChild(nextId, zipper);

	if (!nextNode) return null;
	return followPath(restPath, nextNode);
}

// Follow a path and load the complete node
export async function followPathAndLoad(
	user: GunUser,
	rootId: string,
	path: NavigationPath
): Promise<TreeZipper | null> {
	// First load the root
	const rootNode = await buildTreeFromRoot(user, rootId);
	if (!rootNode) return null;

	// Create zipper and follow path
	const rootZipper = createZipperFromNode(rootNode);
	return followPath(path, rootZipper);
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
