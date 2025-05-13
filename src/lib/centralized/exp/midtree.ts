import type { Node, PersistentCache, ShareMap } from '../types';

// Define Gun types
type GunUser = any;
type GunInstance = any;

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

// Create a root node (equivalent to createRootNode)
export async function createRoot(
	user: GunUser,
	id: string,
	name: string,
	points: number,
	contributors: string[] = [],
	manualFulfillment: number | null = null
): Promise<string> {
	await user
		.get('nodes')
		.get(id)
		.put({
			id,
			name,
			points,
			contributors: JSON.stringify(contributors),
			manualFulfillment:
				manualFulfillment === null ? null : Math.max(0, Math.min(1, manualFulfillment))
		});

	// Initialize empty caches
	await user
		.get('sogf')
		.get(id)
		.put({ map: JSON.stringify({}) });

	return id;
}

// Add a child node (equivalent to addChild in node.ts)
export async function addChild(
	user: GunUser,
	parentId: string,
	childId: string,
	name: string,
	points: number,
	contributors: string[] = [],
	manualFulfillment: number | null = null
): Promise<string | null> {
	// First check if parent has contributors
	const parent = await new Promise<any>((resolve) => {
		user.get('nodes').get(parentId).once(resolve);
	});

	if (parent && JSON.parse(parent.contributors || '[]').length > 0) {
		return null; // Can't add children to nodes with contributors
	}

	// Create the child node
	await user
		.get('nodes')
		.get(childId)
		.put({
			id: childId,
			name,
			points,
			contributors: JSON.stringify(contributors),
			manualFulfillment:
				manualFulfillment === null ? null : Math.max(0, Math.min(1, manualFulfillment))
		});

	// Create the parent-child relationship
	await user.get('edges').get(`${parentId}_${childId}`).put({
		parent: parentId,
		child: childId
	});

	// Initialize empty caches
	await user
		.get('sogf')
		.get(childId)
		.put({ map: JSON.stringify({}) });

	return childId;
}

// Add contributors to a node (equivalent to addContributors)
export async function addContributors(
	user: GunUser,
	nodeId: string,
	contributors: string[]
): Promise<boolean> {
	// Get current node data
	const node = await new Promise<any>((resolve) => {
		user.get('nodes').get(nodeId).once(resolve);
	});

	if (!node) return false;

	// Update contributors
	await user
		.get('nodes')
		.get(nodeId)
		.put({
			...node,
			contributors: JSON.stringify(contributors)
		});

	// Delete all children
	const edges = await new Promise<any[]>((resolve) => {
		const result: any[] = [];
		user
			.get('edges')
			.map()
			.once((edge: any) => {
				if (edge && edge.parent === nodeId) {
					result.push(edge);
				}
				setTimeout(() => resolve(result), 100); // Small timeout to collect results
			});
	});

	// Remove each edge
	for (const edge of edges) {
		await user.get('edges').get(`${edge.parent}_${edge.child}`).put(null);
	}

	// Reset caches
	await user
		.get('sogf')
		.get(nodeId)
		.put({ map: JSON.stringify({}) });

	return true;
}

// Delete a subtree (equivalent to deleteSubtree)
export async function deleteSubtree(user: GunUser, nodeId: string): Promise<boolean> {
	// Delete all children edges
	const edges = await new Promise<any[]>((resolve) => {
		const result: any[] = [];
		user
			.get('edges')
			.map()
			.once((edge: any) => {
				if (edge && edge.parent === nodeId) {
					result.push(edge);
				}
				setTimeout(() => resolve(result), 100); // Small timeout to collect results
			});
	});

	// Remove each edge
	for (const edge of edges) {
		await user.get('edges').get(`${edge.parent}_${edge.child}`).put(null);
	}

	// Reset caches
	await user
		.get('sogf')
		.get(nodeId)
		.put({ map: JSON.stringify({}) });

	return true;
}

// Load a node (equivalent to getting the current node in a zipper)
export async function loadNode(user: GunUser, nodeId: string): Promise<Partial<Node> | null> {
	return new Promise((resolve) => {
		user
			.get('nodes')
			.get(nodeId)
			.once((data: any) => {
				if (data) {
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

// Get a node's children (equivalent to getting children from zipper)
export async function getChildren(user: GunUser, nodeId: string): Promise<string[]> {
	return new Promise((resolve) => {
		const children: string[] = [];
		user
			.get('edges')
			.map()
			.once((edge: any) => {
				if (edge && edge.parent === nodeId) {
					children.push(edge.child);
				}
				setTimeout(() => resolve(children), 100); // Small timeout to collect results
			});
	});
}

// Navigation: Get parent node ID
export async function getParentId(user: GunUser, nodeId: string): Promise<string | null> {
	return new Promise((resolve) => {
		let found = false;
		user
			.get('edges')
			.map()
			.once((edge: any) => {
				if (edge && edge.child === nodeId) {
					found = true;
					resolve(edge.parent);
				}
				// If we've checked all edges and found none
				setTimeout(() => {
					if (!found) resolve(null);
				}, 100);
			});
	});
}

// Navigation: Follow a path (equivalent to followPath)
export async function followPath(
	user: GunUser,
	rootId: string,
	path: string[]
): Promise<string | null> {
	let currentId = rootId;

	for (const childId of path) {
		// Check if the child relationship exists
		const exists = await new Promise<boolean>((resolve) => {
			user
				.get('edges')
				.get(`${currentId}_${childId}`)
				.once((edge: any) => {
					resolve(!!edge);
				});
		});

		if (!exists) return null;
		currentId = childId;
	}

	return currentId;
}

// Get path from root (equivalent to getCurrentPath)
export async function getPathFromRoot(user: GunUser, nodeId: string): Promise<string[]> {
	const path: string[] = [];
	let currentId = nodeId;

	while (true) {
		const parentId = await getParentId(user, currentId);
		if (!parentId) break;

		path.unshift(currentId);
		currentId = parentId;
	}

	return path;
}

// Get siblings (equivalent to getSiblings)
export async function getSiblings(user: GunUser, nodeId: string): Promise<string[]> {
	const parentId = await getParentId(user, nodeId);
	if (!parentId) return [];

	return getChildren(user, parentId);
}

// Update node persistent cache (SOGF map)
export async function updateSogfCache(
	user: GunUser,
	nodeId: string,
	sogfMap: Record<string, number>
): Promise<void> {
	await user
		.get('sogf')
		.get(nodeId)
		.put({
			map: JSON.stringify(sogfMap)
		});
}

// Update provider shares cache
export async function updateProviderShares(
	user: GunUser,
	nodeId: string,
	depth: number,
	sharesMap: Record<string, number>
): Promise<void> {
	await user
		.get('providerShares')
		.get(`${nodeId}_${depth}`)
		.put({
			nodeId,
			depth,
			map: JSON.stringify(sharesMap)
		});
}

// Build a tree from a node
export async function buildTreeFromNode(
	user: GunUser,
	nodeId: string,
	maxDepth = 5
): Promise<Node | null> {
	const visited = new Set<string>();

	async function buildNode(id: string, currentDepth = 0): Promise<Node | null> {
		if (visited.has(id) || currentDepth > maxDepth) return null;
		visited.add(id);

		const nodeData = await loadNode(user, id);
		if (!nodeData) return null;

		const childIds = await getChildren(user, id);
		const children = new Map<string, Node>();

		for (const childId of childIds) {
			const child = await buildNode(childId, currentDepth + 1);
			if (child) {
				children.set(childId, child as Node);
			}
		}

		// Load cached data
		const sogfMap = await new Promise<Record<string, number>>((resolve) => {
			user
				.get('sogf')
				.get(id)
				.once((data: any) => {
					if (data && data.map) {
						resolve(JSON.parse(data.map));
					} else {
						resolve({});
					}
				});
		});

		const providerSharesMap = new Map<number, Record<string, number>>();

		// Load provider shares for typical depths
		for (let d = 1; d <= 5; d++) {
			const shares = await new Promise<Record<string, number>>((resolve) => {
				user
					.get('providerShares')
					.get(`${id}_${d}`)
					.once((data: any) => {
						if (data && data.map) {
							resolve(JSON.parse(data.map));
						} else {
							resolve({});
						}
					});
			});

			if (Object.keys(shares).length > 0) {
				providerSharesMap.set(d, shares);
			}
		}

		return {
			...nodeData,
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

	return buildNode(nodeId);
}

// Subscribe to tree changes
export function subscribeToNode(
	user: GunUser,
	nodeId: string,
	callback: (node: Partial<Node> | null) => void
): () => void {
	const unsub = user
		.get('nodes')
		.get(nodeId)
		.on((data: any) => {
			if (data) {
				callback({
					nodeId: data.id,
					nodeName: data.name,
					nodePoints: data.points,
					nodeContributors: new Set(JSON.parse(data.contributors || '[]')),
					nodeManualFulfillment: data.manualFulfillment
				});
			} else {
				callback(null);
			}
		});

	return unsub;
}

// Subscribe to children changes
export function subscribeToChildren(
	user: GunUser,
	nodeId: string,
	callback: (childIds: string[]) => void
): () => void {
	const children: string[] = [];
	let timeoutId: any = null;

	const unsub = user
		.get('edges')
		.map()
		.on((edge: any) => {
			if (edge && edge.parent === nodeId) {
				if (!children.includes(edge.child)) {
					children.push(edge.child);

					// Debounce to avoid calling callback too frequently
					clearTimeout(timeoutId);
					timeoutId = setTimeout(() => callback([...children]), 50);
				}
			}
		});

	return unsub;
}

// Clean utility that combines multiple Gun operations
export async function updateNode(
	user: GunUser,
	nodeId: string,
	updates: {
		name?: string;
		points?: number;
		contributors?: string[];
		manualFulfillment?: number | null;
	}
): Promise<boolean> {
	// Get current node data
	const node = await new Promise<any>((resolve) => {
		user.get('nodes').get(nodeId).once(resolve);
	});

	if (!node) return false;

	// Prepare update object
	const updatedNode = { ...node };

	if (updates.name !== undefined) updatedNode.name = updates.name;
	if (updates.points !== undefined) updatedNode.points = updates.points;
	if (updates.contributors !== undefined)
		updatedNode.contributors = JSON.stringify(updates.contributors);
	if (updates.manualFulfillment !== undefined) {
		updatedNode.manualFulfillment =
			updates.manualFulfillment === null
				? null
				: Math.max(0, Math.min(1, updates.manualFulfillment));
	}

	// Update the node
	await user.get('nodes').get(nodeId).put(updatedNode);

	return true;
}
