// gun-tree.ts
import type { Node, PersistentCache, ShareMap } from '../types';

// Define Gun type to fix typescript errors
type GunUser = any;
type GunInstance = any;

// Create an empty persistent cache (previously in persistance.ts)
export function emptyPersistentCache(): PersistentCache {
	return {
		pcSogfMap: null,
		pcProviderShares: new Map()
	};
}

// Helpers to access persistent cache
export function getSogfMap(pc: PersistentCache): ShareMap | null {
	return pc.pcSogfMap;
}

export function getProviderShares(depth: number, pc: PersistentCache): ShareMap | null {
	return pc.pcProviderShares.get(depth) || null;
}

// Helpers to update persistent cache
export function setSogfMap(sm: ShareMap, pc: PersistentCache): PersistentCache {
	return {
		...pc,
		pcSogfMap: sm
	};
}

export function setProviderShares(
	depth: number,
	sm: ShareMap,
	pc: PersistentCache
): PersistentCache {
	const newMap = new Map(pc.pcProviderShares);
	newMap.set(depth, sm);
	return {
		...pc,
		pcProviderShares: newMap
	};
}

// Initialize Gun
function initGun(relayUrls: string[] = ['https://gun-manhattan.herokuapp.com/gun']): {
	gun: GunInstance;
	user: GunUser;
} {
	const Gun = (window as any).Gun;
	const gun = Gun(relayUrls);
	const user = gun.user();
	return { gun, user };
}

// === Node / Tree Structure ===

// Save a node
function saveNode(
	user: GunUser,
	nodeId: string,
	node: {
		name: string;
		points: number;
		contributors: string[];
		manualFulfillment: number | null;
	}
) {
	return user
		.get('nodes')
		.get(nodeId)
		.put({
			id: nodeId,
			name: node.name,
			points: node.points,
			contributors: JSON.stringify(node.contributors),
			manualFulfillment: node.manualFulfillment
		});
}

// Save a parent-child relationship
function saveChildRelation(user: GunUser, parentId: string, childId: string) {
	return user.get('edges').get(`${parentId}_${childId}`).put({
		parent: parentId,
		child: childId
	});
}

// Load a node
function loadNode(user: GunUser, nodeId: string): Promise<Partial<Node> | null> {
	return new Promise((resolve) => {
		user
			.get('nodes')
			.get(nodeId)
			.once((data: any) => {
				if (data) {
					// Transform the Gun data back to our structure
					resolve({
						nodeId: data.id,
						nodeName: data.name,
						nodePoints: data.points,
						nodeContributors: new Set(JSON.parse(data.contributors || '[]')),
						nodeManualFulfillment: data.manualFulfillment
					});
				} else {
					resolve(null);
				}
			});
	});
}

// Get all children of a node
function getChildren(user: GunUser, nodeId: string): Promise<string[]> {
	return new Promise((resolve) => {
		const children: string[] = [];
		user
			.get('edges')
			.map()
			.once((edge: any) => {
				if (edge && edge.parent === nodeId) {
					children.push(edge.child);
				}
			});
		resolve(children);
	});
}

// === Caches ===

// Save SOGF (Shares of General Fulfillment)
function saveSogf(user: GunUser, nodeId: string, sogfMap: Record<string, number>) {
	return user
		.get('sogf')
		.get(nodeId)
		.put({
			map: JSON.stringify(sogfMap)
		});
}

// Load SOGF
function loadSogf(user: GunUser, nodeId: string): Promise<Record<string, number>> {
	return new Promise((resolve) => {
		user
			.get('sogf')
			.get(nodeId)
			.once((data: any) => {
				if (data && data.map) {
					resolve(JSON.parse(data.map));
				} else {
					resolve({});
				}
			});
	});
}

// Save Provider Shares for a specific depth
function saveProviderShares(
	user: GunUser,
	nodeId: string,
	depth: number,
	sharesMap: Record<string, number>
) {
	return user
		.get('providerShares')
		.get(`${nodeId}_${depth}`)
		.put({
			nodeId,
			depth,
			map: JSON.stringify(sharesMap)
		});
}

// Load Provider Shares for a specific depth
function loadProviderShares(
	user: GunUser,
	nodeId: string,
	depth: number
): Promise<Record<string, number>> {
	return new Promise((resolve) => {
		user
			.get('providerShares')
			.get(`${nodeId}_${depth}`)
			.once((data: any) => {
				if (data && data.map) {
					resolve(JSON.parse(data.map));
				} else {
					resolve({});
				}
			});
	});
}

// === Tree Reconstruction ===

// Build a tree structure starting from a root node
async function buildTreeFromRoot(
	user: GunUser,
	rootId: string,
	maxDepth = 10
): Promise<Node | null> {
	const visited = new Set<string>();

	async function buildNode(nodeId: string, currentDepth = 0): Promise<Node | null> {
		if (visited.has(nodeId) || currentDepth > maxDepth) return null;
		visited.add(nodeId);

		const node = await loadNode(user, nodeId);
		if (!node) return null;

		const childIds = await getChildren(user, nodeId);
		const children = new Map<string, Node>();

		for (const childId of childIds) {
			const child = await buildNode(childId, currentDepth + 1);
			if (child) {
				children.set(childId, child as Node);
			}
		}

		// Load cached data
		const sogfMap = await loadSogf(user, nodeId);
		const providerSharesMap = new Map<number, Record<string, number>>();

		// Load provider shares for typical depths (1-5)
		for (let d = 1; d <= 5; d++) {
			const shares = await loadProviderShares(user, nodeId, d);
			if (Object.keys(shares).length > 0) {
				providerSharesMap.set(d, shares);
			}
		}

		// Construct node with all data
		return {
			...node,
			nodeChildren: children,
			nodeCapacities: new Map(),
			nodeCapacityShares: new Map(),
			nodePersistentCache: {
				pcSogfMap: sogfMap ? new Map(Object.entries(sogfMap)) : null,
				pcProviderShares: new Map(
					Array.from(providerSharesMap.entries()).map(([depth, shares]) => [
						depth,
						new Map(Object.entries(shares))
					])
				)
			},
			nodeTransientCache: {
				cacheMap: new Map(),
				cacheHits: 0,
				cacheMisses: 0
			}
		} as Node;
	}

	return buildNode(rootId);
}

// Create a root node
async function createRoot(
	user: GunUser,
	id: string,
	name: string,
	points: number
): Promise<Partial<Node> | null> {
	await saveNode(user, id, {
		name,
		points,
		contributors: [],
		manualFulfillment: null
	});

	// Initialize empty caches
	await saveSogf(user, id, {});

	// Return the node
	return await loadNode(user, id);
}

// === Traversal Functions ===

// Add a child to a node
async function addChild(
	user: GunUser,
	parentId: string,
	childId: string,
	name: string,
	points: number
): Promise<void> {
	// Create the child node
	await saveNode(user, childId, {
		name,
		points,
		contributors: [],
		manualFulfillment: null
	});

	// Create the edge
	await saveChildRelation(user, parentId, childId);

	// Initialize empty caches
	await saveSogf(user, childId, {});
}

// Subscribe to tree changes
type TreeChangeCallback = (data: { type: string; nodeId: string; data: any }) => void;

function subscribeToTree(user: GunUser, rootId: string, callback: TreeChangeCallback): () => void {
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

// === Compatibility Functions with Old Persistence System ===

// Adapter for legacy code that expects a persistence monad
export type PersistM<A> = Promise<A>;

// Update a node with its persistent cache
export async function updateNodeWithPersistentCache(user: GunUser, node: Node): Promise<Node> {
	// Load the persistent cache for this node
	const sogf = await loadSogf(user, node.nodeId);

	// Convert from Record to Map for SOGF
	const sogfMap = sogf ? new Map(Object.entries(sogf)) : null;

	// Load provider shares for depths 1-5
	const providerSharesMap = new Map<number, ShareMap>();
	for (let d = 1; d <= 5; d++) {
		const shares = await loadProviderShares(user, node.nodeId, d);
		if (Object.keys(shares).length > 0) {
			providerSharesMap.set(d, new Map(Object.entries(shares)));
		}
	}

	// Create the persistent cache
	const pc: PersistentCache = {
		pcSogfMap: sogfMap,
		pcProviderShares: providerSharesMap
	};

	// Return node with updated cache
	return {
		...node,
		nodePersistentCache: pc
	};
}

// Save a node's persistent cache
export async function savePersistentCache(user: GunUser, node: Node): Promise<void> {
	const pc = node.nodePersistentCache;

	// Save SOGF if it exists
	if (pc.pcSogfMap) {
		const sogfObj = Object.fromEntries(pc.pcSogfMap);
		await saveSogf(user, node.nodeId, sogfObj);
	}

	// Save provider shares for each depth
	for (const [depth, shares] of pc.pcProviderShares.entries()) {
		const sharesObj = Object.fromEntries(shares);
		await saveProviderShares(user, node.nodeId, depth, sharesObj);
	}
}

// Load a node's persistent cache
export async function loadPersistentCache(user: GunUser, nodeId: string): Promise<PersistentCache> {
	// Load SOGF
	const sogf = await loadSogf(user, nodeId);
	const sogfMap = sogf ? new Map(Object.entries(sogf)) : null;

	// Load provider shares for depths 1-5
	const providerSharesMap = new Map<number, ShareMap>();
	for (let d = 1; d <= 5; d++) {
		const shares = await loadProviderShares(user, nodeId, d);
		if (Object.keys(shares).length > 0) {
			providerSharesMap.set(d, new Map(Object.entries(shares)));
		}
	}

	return {
		pcSogfMap: sogfMap,
		pcProviderShares: providerSharesMap
	};
}

// === Exports ===
export {
	initGun,
	saveNode,
	loadNode,
	saveChildRelation,
	getChildren,
	saveSogf,
	loadSogf,
	saveProviderShares,
	loadProviderShares,
	buildTreeFromRoot,
	createRoot,
	addChild,
	subscribeToTree
};
