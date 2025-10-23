/**
 * Free Association Protocol - Gun.js Implementation
 */

// Gun and SEA type definitions
interface IGunChain {
	get: (key: string) => IGunChain;
	put: (data: any, cb?: any, opt?: any) => IGunChain;
	once: (cb: (data: any) => void) => IGunChain;
	on: (cb: (data: any, key: string) => void) => { off: () => void };
	map: () => IGunChain;
	set: (data: any, cb?: (ack: { err?: string; ok?: number }) => void) => IGunChain;
	_: { get?: string; '#'?: string; [key: string]: any };
}

interface IGunUser {
	create: (username: string, password: string, cb?: (ack: any) => void) => IGunUser;
	auth: (username: string, password: string, cb?: (ack: any) => void) => IGunUser;
	leave: () => IGunUser;
	get: (key: string) => IGunChain;
	is?: { pub: string; alias: string };
}

// Define node types
interface GunNode {
	id: string;
	name: string;
	type: string;
	manual_fulfillment?: number;
	points?: number;
	parent_id?: string;
	contributor_ids?: string[];
	children?: any[];
	[key: string]: any;
}

// Utility for generating random IDs
const randomId = (): string => {
	return Math.random().toString(36).substring(2, 15) + Math.random().toString(36).substring(2, 15);
};

// Authenticate user
export async function authenticate(username: string, password: string): Promise<any> {
	return new Promise((resolve) => {
		user.auth(username, password, (ack) => resolve(ack));
	});
}

// Initialize a user's tree (called after authentication)
export function initializeUserTree(name: string): IGunChain {
	if (!user.is) {
		throw new Error('User must be authenticated before initializing tree');
	}

	const rootNode = {
		id: user.is.pub,
		name: name,
		type: 'RootNode',
		user_id: user.is.pub,
		capacities: [],
		created_at: new Date().toISOString(),
		updated_at: new Date().toISOString(),
		children: [] // Will be handled by Gun as references
	};

	// Store under user's protected space
	user.get('tree').put(rootNode);
	return user.get('tree');
}

// Export types for external use
export type ShareMap = Record<string, number>;

/**
 * Tree Navigation Functions
 */

// Find a node by ID starting from a tree root
export function findNodeById(treeRef: IGunChain, nodeId: string): Promise<IGunChain | null> {
	return new Promise((resolve) => {
		treeRef.once((node) => {
			if (node && node.id === nodeId) {
				resolve(treeRef);
				return;
			}

			// Not a match, check children
			let found = false;

			treeRef
				.get('children')
				.map()
				.once((childRef) => {
					if (found || !childRef || !childRef['#']) return;

					const childNodeRef = gun.get(childRef['#']);

					findNodeById(childNodeRef, nodeId).then((result) => {
						if (result) {
							found = true;
							resolve(result);
						}
					});
				});

			// Set timeout to resolve null if not found
			setTimeout(() => {
				if (!found) resolve(null);
			}, 1000);
		});
	});
}

// Get all descendants of a node and collect them with a callback
export function getDescendants(
	nodeRef: IGunChain,
	callback: (node: GunNode, nodeRef: IGunChain) => void
): void {
	nodeRef
		.get('children')
		.map()
		.once((childRef) => {
			if (!childRef || !childRef['#']) return;

			const childNodeRef = gun.get(childRef['#']);

			childNodeRef.once((child) => {
				if (child) {
					callback(child, childNodeRef);
					getDescendants(childNodeRef, callback);
				}
			});
		});
}

// Collect all descendants into an array
export function collectDescendants(nodeRef: IGunChain): Promise<GunNode[]> {
	return new Promise((resolve) => {
		const descendants: GunNode[] = [];

		const checkComplete = () => {
			setTimeout(() => resolve(descendants), 500);
		};

		nodeRef
			.get('children')
			.map()
			.once((childRef) => {
				if (!childRef || !childRef['#']) return checkComplete();

				const childNodeRef = gun.get(childRef['#']);

				childNodeRef.once((child) => {
					if (child) {
						descendants.push(child);

						// Recursively get this child's descendants
						collectDescendants(childNodeRef).then((childDescendants) => {
							descendants.push(...childDescendants);
							checkComplete();
						});
					} else {
						checkComplete();
					}
				});
			});

		// Handle case with no children
		setTimeout(() => resolve(descendants), 500);
	});
}

// Find parent node reference
export function findParentNode(treeRef: IGunChain, nodeId: string): Promise<IGunChain | null> {
	return new Promise((resolve) => {
		let found = false;

		treeRef
			.get('children')
			.map()
			.once((childRef) => {
				if (found || !childRef || !childRef['#']) return;

				const childNodeRef = gun.get(childRef['#']);

				childNodeRef.once((child) => {
					if (child && child.id === nodeId) {
						found = true;
						resolve(treeRef);
						return;
					}

					// If not found, look in this child's children
					findParentNode(childNodeRef, nodeId).then((result) => {
						if (result) {
							found = true;
							resolve(result);
						}
					});
				});
			});

		// If still not found after checking, resolve null
		setTimeout(() => {
			if (!found) resolve(null);
		}, 1000);
	});
}

// Get path from root to a specific node
export function getPathToNode(treeRef: IGunChain, nodeId: string): Promise<string[]> {
	return new Promise(async (resolve) => {
		const paths: Array<{ path: string[]; node: IGunChain }> = [{ path: [], node: treeRef }];
		let visited = new Set<string>();

		while (paths.length > 0) {
			const { path, node } = paths.shift()!;

			// Get node data
			const nodeData = await new Promise<any>((res) => node.once(res));

			// Check if this is the node we're looking for
			if (nodeData && nodeData.id === nodeId) {
				resolve([...path, nodeId]);
				return;
			}

			// Mark as visited
			const nodePath = node._?.get || '';
			if (visited.has(nodePath)) continue;
			visited.add(nodePath);

			// Check children
			node
				.get('children')
				.map()
				.once((childRef) => {
					if (!childRef || !childRef['#']) return;

					const childNodeRef = gun.get(childRef['#']);
					paths.push({ path: [...path, nodeData?.id], node: childNodeRef });
				});
		}

		// If no path found
		resolve([]);
	});
}

/**
 * Core Calculations
 */

// Calculate total points from all children
export function totalChildPoints(nodeRef: IGunChain): Promise<number> {
	return new Promise((resolve) => {
		let total = 0;
		let childCount = 0;
		let processedCount = 0;

		nodeRef.get('children').once((children) => {
			if (!children) {
				resolve(0);
				return;
			}

			nodeRef
				.get('children')
				.map()
				.once((childRef) => {
					if (!childRef || !childRef['#']) return;
					childCount++;

					const childNodeRef = gun.get(childRef['#']);

					childNodeRef.once((child) => {
						processedCount++;

						if (child && child.type === 'NonRootNode' && typeof child.points === 'number') {
							total += child.points;
						}

						if (processedCount === childCount) {
							resolve(total);
						}
					});
				});

			// If no children were processed, resolve with 0
			setTimeout(() => {
				if (childCount === 0) resolve(0);
			}, 100);
		});
	});
}

// Calculate a node's weight
export function weight(nodeRef: IGunChain, treeRef: IGunChain): Promise<number> {
	return new Promise(async (resolve) => {
		const node = await new Promise<GunNode>((res) => nodeRef.once(res));

		// Root node always has weight 1.0
		if (node.type === 'RootNode') {
			resolve(1.0);
			return;
		}

		// Find parent node
		const parentRef = await findParentNode(treeRef, node.id);
		if (!parentRef) {
			resolve(1.0); // No parent found, assume root weight
			return;
		}

		// Get parent's total points
		const totalPoints = await totalChildPoints(parentRef);

		// Calculate this node's contribution to parent
		const nodePoints = typeof node.points === 'number' ? node.points : 0;
		const nodeContribution = totalPoints === 0 ? 0 : nodePoints / totalPoints;

		// Get parent's weight recursively
		const parentWeight = await weight(parentRef, treeRef);

		// Weight is recursive - multiply by parent's weight
		resolve(nodeContribution * parentWeight);
	});
}

// Check if a node is a contribution node
export function isContribution(nodeRef: IGunChain): Promise<boolean> {
	return new Promise((resolve) => {
		nodeRef.once((node) => {
			if (!node) {
				resolve(false);
				return;
			}

			if (node.type === 'RootNode') {
				resolve(false);
				return;
			}

			// Check if it has contributors
			nodeRef.get('contributor_ids').once((contributorIds) => {
				resolve(Array.isArray(contributorIds) && contributorIds.length > 0);
			});
		});
	});
}

// Calculate a node's share of its parent
export function shareOfParent(nodeRef: IGunChain, treeRef: IGunChain): Promise<number> {
	return new Promise(async (resolve) => {
		const node = await new Promise<GunNode>((res) => nodeRef.once(res));

		// Root node has 100% share
		if (node.type === 'RootNode') {
			resolve(1.0);
			return;
		}

		// Find parent
		const parentRef = await findParentNode(treeRef, node.id);
		if (!parentRef) {
			resolve(1.0); // Safety check
			return;
		}

		// Calculate share for non-root nodes
		const parentTotal = await totalChildPoints(parentRef);
		if (node.type !== 'NonRootNode' || typeof node.points !== 'number') {
			resolve(0);
			return;
		}

		resolve(parentTotal === 0 ? 0 : node.points / parentTotal);
	});
}

// Calculate the desire (unfulfilled need) of a node
export function desire(nodeRef: IGunChain, treeRef: IGunChain): Promise<number> {
	return new Promise(async (resolve) => {
		const fulfillmentValue = await fulfilled(nodeRef, treeRef);
		resolve(1.0 - fulfillmentValue);
	});
}

/**
 * Tree Modification API
 */

// Add a child node to a parent node
export function addChildNode(
	parentRef: IGunChain,
	name: string,
	points: number,
	contributorIds: string[] = []
): Promise<IGunChain> {
	return new Promise((resolve) => {
		// Generate a unique ID for the child
		const childId = randomId();

		// Create child node in a separate node
		const childNodePath = `nodes/${childId}`;
		const childNode = {
			id: childId,
			name: name,
			type: 'NonRootNode',
			points: points,
			parent_id: parentRef._.get || childId,
			contributor_ids: contributorIds,
			manual_fulfillment: undefined,
			children: []
		};

		// Store the child node
		const childRef = gun.get(childNodePath).put(childNode);

		// Add reference to parent's children
		parentRef.get('children').set(childRef, (ack: { err?: string }) => {
			if (ack.err) console.error(ack.err);
			resolve(childRef);
		});
	});
}

// Add contributors to a node
export function addContributors(nodeRef: IGunChain, contributorIds: string[]): Promise<any> {
	return new Promise((resolve) => {
		nodeRef.once((node) => {
			if (!node) {
				resolve({ error: 'Node not found' });
				return;
			}

			// Only non-root nodes can have contributors
			if (node.type !== 'NonRootNode') {
				resolve({ error: 'Only non-root nodes can have contributors' });
				return;
			}

			// Add contributors and clear children
			nodeRef.get('contributor_ids').put(contributorIds);
			deleteSubtree(nodeRef).then(() => {
				resolve(true);
			});
		});
	});
}

// Delete a subtree (remove all children)
export function deleteSubtree(nodeRef: IGunChain): Promise<boolean> {
	return new Promise((resolve) => {
		const childRefs: IGunChain[] = [];

		nodeRef
			.get('children')
			.map()
			.once((childRef) => {
				if (childRef && childRef['#']) {
					childRefs.push(gun.get(childRef['#']));
				}
			});

		// Clear the children array from the node
		nodeRef.get('children').put([]);

		// Recursively delete all children
		for (const childRef of childRefs) {
			deleteSubtree(childRef);
		}

		resolve(true);
	});
}

// Update manual fulfillment value
export function updateManualFulfillment(nodeRef: IGunChain, value?: number): Promise<IGunChain> {
	// Validate and normalize between 0 and 1
	const normalizedValue = value === undefined ? undefined : Math.max(0, Math.min(1, value));

	return new Promise((resolve) => {
		nodeRef.get('manual_fulfillment').put(normalizedValue);
		resolve(nodeRef);
	});
}

/**
 * Capacity Functions
 */

// Add a capacity to a user's tree
export function addCapacity(
	userTreeRef: IGunChain,
	capacity: any
): Promise<IGunChain | { error: string }> {
	return new Promise((resolve) => {
		// Generate ID if not provided
		const capacityId = capacity.id || randomId();

		// Create capacity object
		const capacityData = {
			...capacity,
			id: capacityId,
			shares: []
		};

		// Create capacity node
		const capacityRef = gun.get(`capacity_${capacityId}`).put(capacityData);

		// Add reference to user's tree
		userTreeRef.get('capacities').set(capacityRef, (ack: { err?: string }) => {
			if (ack.err) {
				resolve({ error: ack.err });
			} else {
				resolve(capacityRef);
			}
		});
	});
}

// Add a capacity share
export function addCapacityShare(
	capacityRef: IGunChain,
	recipientId: string,
	percentage: number
): Promise<IGunChain | { error: string }> {
	return new Promise((resolve) => {
		// Get capacity data first
		capacityRef.once((capacity) => {
			if (!capacity) {
				resolve({ error: 'Capacity not found' });
				return;
			}

			// Compute quantity based on percentage
			const quantity = typeof capacity.quantity === 'number' ? capacity.quantity : 0;
			const computedQuantity = Math.floor((quantity * percentage) / 100);

			// Create share object
			const shareId = randomId();
			const shareData = {
				id: shareId,
				capacity_id: capacity.id,
				recipient_id: recipientId,
				share_percentage: percentage,
				computed_quantity: computedQuantity
			};

			// Create share node
			const shareRef = gun.get(`share_${shareId}`).put(shareData);

			// Add reference to capacity's shares
			capacityRef.get('shares').set(shareRef, (ack: { err?: string }) => {
				if (ack.err) {
					resolve({ error: ack.err });
				} else {
					resolve(shareRef);
				}
			});
		});
	});
}

/**
 * Utility Functions
 */

// Get a node's full data as a regular object
export function gunNodeToObject(nodeRef: IGunChain): Promise<GunNode> {
	return new Promise((resolve) => {
		nodeRef.once((node) => {
			if (!node) {
				resolve(null as any);
				return;
			}

			const result: GunNode = { ...node, children: [] };

			let childCount = 0;
			let processedCount = 0;

			// Handle children collection separately
			nodeRef
				.get('children')
				.map()
				.once((childRef) => {
					if (!childRef || !childRef['#']) return;
					childCount++;

					gun.get(childRef['#']).once((child) => {
						if (child) {
							result.children = result.children || [];
							result.children.push(child);
						}

						processedCount++;
						if (processedCount === childCount) {
							resolve(result);
						}
					});
				});

			// Resolve if no children
			setTimeout(() => {
				if (childCount === 0) {
					resolve(result);
				}
			}, 100);
		});
	});
}

// Normalize a ShareMap so values sum to 1
export function normalizeShareMap(map: ShareMap): ShareMap {
	const total = Object.values(map).reduce((sum, v) => sum + v, 0);

	if (total === 0) return map;

	const normalizedMap: ShareMap = {};
	for (const [key, value] of Object.entries(map)) {
		normalizedMap[key] = value / total;
	}

	return normalizedMap;
}

/**
 * Filter System for Share Maps
 */

// Filter shares based on a predicate function
export function filterShareMap(
	shareMap: ShareMap,
	predicate: (nodeId: string, share: number) => boolean
): ShareMap {
	const filteredMap: ShareMap = {};

	for (const [nodeId, share] of Object.entries(shareMap)) {
		if (predicate(nodeId, share)) {
			filteredMap[nodeId] = share;
		}
	}

	return normalizeShareMap(filteredMap);
}

// Common predicates for filtering share maps

// Keep only nodes in the include list
export function includeFilter(includeList: string[]): (nodeId: string, share: number) => boolean {
	return (nodeId: string) => includeList.includes(nodeId);
}

// Remove nodes in the exclude list
export function excludeFilter(excludeList: string[]): (nodeId: string, share: number) => boolean {
	return (nodeId: string) => !excludeList.includes(nodeId);
}

// Keep only nodes with shares above a threshold
export function thresholdFilter(minShare: number): (nodeId: string, share: number) => boolean {
	return (_: string, share: number) => share >= minShare;
}

// Combine multiple filters using AND logic
export function combineFilters(
	...filters: ((nodeId: string, share: number) => boolean)[]
): (nodeId: string, share: number) => boolean {
	return (nodeId: string, share: number) => {
		return filters.every((filter) => filter(nodeId, share));
	};
}

// Apply a jsonLogic rule to filter a share map
export function applyJsonLogicFilter(
	shareMap: ShareMap,
	rule: any,
	nodeContext: Record<string, any> = {}
): ShareMap {
	// Create a predicate that evaluates the rule for each node
	const predicate = (nodeId: string, share: number) => {
		// Create the data object for jsonLogic to evaluate
		const data = {
			nodeId,
			share,
			node: nodeContext[nodeId] || null
		};

		// Apply the rule using jsonLogic and ensure boolean return
		const result = jsonLogic.apply(rule, data);
		return Boolean(result);
	};

	// Use our existing filter system
	return filterShareMap(shareMap, predicate);
}

// Get normalized share map for applying filters
export function getShareMap(sharesRef: IGunChain): Promise<ShareMap> {
	return new Promise((resolve) => {
		const shareMap: ShareMap = {};

		// Get all shares
		sharesRef.map().once((shareRef) => {
			if (!shareRef || !shareRef['#']) return;

			// Get actual share node
			gun.get(shareRef['#']).once((share) => {
				if (share && share.recipient_id && typeof share.share_percentage === 'number') {
					shareMap[share.recipient_id] = share.share_percentage;
				}
			});
		});

		// Wait a bit to collect all shares, then normalize
		setTimeout(() => {
			resolve(normalizeShareMap(shareMap));
		}, 500);
	});
}

// Calculate mutual fulfillment contribution of a contributor to a target node
export function shareOfGeneralFulfillment(
	targetRef: IGunChain,
	contributorId: string
): Promise<number> {
	return new Promise(async (resolve) => {
		// Get target node and all descendants
		const targetData = await gunNodeToObject(targetRef);
		const descendants = await collectDescendants(targetRef);
		const allNodes = [targetData, ...descendants];

		// Find all nodes where this contributor is listed
		const contributingNodes: Array<{ node: GunNode; ref: IGunChain }> = [];

		for (const node of allNodes) {
			if (node.type === 'RootNode') continue;

			// Check if contributor is in the list
			if (node.contributor_ids?.includes(contributorId)) {
				// Create a reference to this node
				const nodeRef = gun.get(`nodes/${node.id}`);
				contributingNodes.push({ node, ref: nodeRef });
			}
		}

		// Calculate weighted contribution for each node
		const weightedContributions = await Promise.all(
			contributingNodes.map(async ({ node, ref }) => {
				const nodeWeight = await weight(ref, targetRef);
				const nodeFulfillment = await fulfilled(ref, targetRef);
				const contributorCount = node.contributor_ids?.length || 1;

				return (nodeWeight * nodeFulfillment) / contributorCount;
			})
		);

		// Sum the weighted contributions
		const total = weightedContributions.reduce((sum, w) => sum + w, 0);
		resolve(total);
	});
}

// Get normalized shares of general fulfillment map
export function sharesOfGeneralFulfillmentMap(rootRef: IGunChain): Promise<ShareMap> {
	return new Promise(async (resolve) => {
		const sharesMap: ShareMap = {};

		// Get root node data
		const rootNode = await gunNodeToObject(rootRef);

		// Get all user roots from the gun database
		gun
			.get('users')
			.map()
			.once((userRef) => {
				if (!userRef || !userRef['#']) return;

				gun
					.get(userRef['#'])
					.get('tree')
					.once((tree) => {
						if (tree && tree['#']) {
							gun.get(tree['#']).once((rootData) => {
								if (rootData && rootData.id && rootData.id !== rootNode.id) {
									const contributorId = rootData.id;

									// Calculate share of general fulfillment
									shareOfGeneralFulfillment(rootRef, contributorId).then((share) => {
										if (share > 0) {
											sharesMap[contributorId] = share;
										}
									});
								}
							});
						}
					});
			});

		// Wait a bit to collect results, then normalize
		setTimeout(() => {
			resolve(normalizeShareMap(sharesMap));
		}, 1000);
	});
}

// Calculate mutual fulfillment between two nodes
export function mutualFulfillment(nodeARef: IGunChain, nodeBRef: IGunChain): Promise<number> {
	return new Promise(async (resolve) => {
		// Get share maps
		const sharesFromA = await sharesOfGeneralFulfillmentMap(nodeARef);
		const sharesFromB = await sharesOfGeneralFulfillmentMap(nodeBRef);

		// Get nodes data
		const nodeA = await gunNodeToObject(nodeARef);
		const nodeB = await gunNodeToObject(nodeBRef);

		// Extract shares with safe lookup
		const shareFromAToB = sharesFromA[nodeB.id] ?? 0;
		const shareFromBToA = sharesFromB[nodeA.id] ?? 0;

		// Mutual fulfillment is the minimum of shares each gives to the other
		resolve(Math.min(shareFromAToB, shareFromBToA));
	});
}

// Calculate provider-centric shares based on depth
export function providerShares(providerRef: IGunChain, depth: number): Promise<ShareMap> {
	return new Promise(async (resolve) => {
		// Direct contributor shares based on mutual fulfillment
		const getDirectContributorShares = async (): Promise<ShareMap> => {
			const contributorShares: ShareMap = {};

			// Get provider node data
			const provider = await gunNodeToObject(providerRef);

			// Get all descendants
			const descendants = await collectDescendants(providerRef);
			const allNodes = [provider, ...descendants];

			// Get all contributor IDs from all non-root nodes in the tree
			const allContributorIds = new Set<string>();
			for (const node of allNodes) {
				if (node.type === 'NonRootNode' && node.contributor_ids) {
					for (const contributorId of node.contributor_ids) {
						allContributorIds.add(contributorId);
					}
				}
			}

			// For each contributor, calculate mutual fulfillment
			for (const contributorId of allContributorIds) {
				const contributorRef = gun.get(`~${contributorId}`);

				// Calculate mutual fulfillment
				const mf = await mutualFulfillment(providerRef, contributorRef);
				if (mf > 0) {
					contributorShares[contributorId] = mf;
				}
			}

			return normalizeShareMap(contributorShares);
		};

		// Helper function to combine transitive shares
		const combineTransitiveShares = async (currentShares: ShareMap): Promise<ShareMap> => {
			const resultMap: ShareMap = {};

			for (const [recipientId, share] of Object.entries(currentShares)) {
				const recipientRef = gun.get(`~${recipientId}`);

				// Get recipient's direct shares
				const recipientDirectShares = await providerShares(recipientRef, 1);

				for (const [subRecipientId, subShare] of Object.entries(recipientDirectShares)) {
					const weightedShare = share * subShare;
					resultMap[subRecipientId] = (resultMap[subRecipientId] || 0) + weightedShare;
				}
			}

			return normalizeShareMap(resultMap);
		};

		// Main logic based on depth
		if (depth === 1) {
			resolve(await getDirectContributorShares());
		} else {
			let currentShares = await getDirectContributorShares();

			// Iteratively combine transitive shares
			for (let i = 2; i <= depth; i++) {
				currentShares = await combineTransitiveShares(currentShares);
			}

			resolve(currentShares);
		}
	});
}

// Compute quantity share based on capacity and percentage
export function computeQuantityShare(capacity: any, percentage: number): number {
	// Extract constraints
	const quantity = typeof capacity.quantity === 'number' ? capacity.quantity : 0;
	const maxNatural = typeof capacity.max_natural_div === 'number' ? capacity.max_natural_div : 1;
	const maxPercent =
		typeof capacity.max_percentage_div === 'number' ? capacity.max_percentage_div : 1;

	// Apply percentage divisibility constraint
	const rawQuantity = Math.round(quantity * percentage);
	const percentConstrained =
		percentage > maxPercent ? Math.round(quantity * maxPercent) : rawQuantity;

	// Apply natural number divisibility constraint
	const naturalConstrained = Math.floor(percentConstrained / maxNatural) * maxNatural;

	return naturalConstrained;
}

// Create a new capacity share
export function createCapacityShare(
	capacityId: string,
	recipientId: string,
	percentage: number,
	capacity: any
): any {
	return {
		id: randomId(),
		capacity_id: capacityId,
		recipient_id: recipientId,
		share_percentage: percentage,
		computed_quantity: computeQuantityShare(capacity, percentage)
	};
}

// Get a receiver's share from a specific capacity provider
export function receiverShareFrom(
	receiverRef: IGunChain,
	providerRef: IGunChain,
	capacityRef: IGunChain,
	maxDepth: number
): Promise<number> {
	return new Promise(async (resolve) => {
		// Get receiver data
		const receiver = await gunNodeToObject(receiverRef);

		// Get shares from the provider
		const providerShareMap = await providerShares(providerRef, maxDepth);

		// Return share or 0 if not found
		resolve(providerShareMap[receiver.id] ?? 0);
	});
}

// Helper to convert a recurrence end object to a string value
export function getRecurrenceEndValue(recurrenceEnd: any): string | null {
	if (!recurrenceEnd) return null;

	if (recurrenceEnd.type === 'EndsOn' && recurrenceEnd.date) {
		return new Date(recurrenceEnd.date).toISOString().split('T')[0];
	} else if (recurrenceEnd.type === 'EndsAfter' && recurrenceEnd.count) {
		return recurrenceEnd.count.toString();
	}

	return null;
}

// Validate and normalize manual fulfillment values
export function validateManualFulfillment(value: number | undefined): number | undefined {
	if (value === undefined) return undefined;
	return Math.max(0, Math.min(1, value)); // Clamp between 0 and 1
}

// Create a non-root node (without adding to parent)
export function createNonRootNode(
	id: string,
	name: string,
	parentId: string,
	pts: number,
	contributorIds: string[] = [],
	manual?: number
): any {
	return {
		id,
		name,
		type: 'NonRootNode',
		points: pts,
		parent_id: parentId,
		children: [],
		contributor_ids: contributorIds,
		manual_fulfillment: validateManualFulfillment(manual)
	};
}

// Helper to find and update a node directly in a tree
export function updateNodeById(
	treeRef: IGunChain,
	nodeId: string,
	updater: (nodeRef: IGunChain) => void
): Promise<boolean> {
	return new Promise(async (resolve) => {
		// Try to find the node
		const nodeRef = await findNodeById(treeRef, nodeId);

		if (nodeRef) {
			// Apply the updater function
			updater(nodeRef);
			resolve(true);
			return;
		}

		resolve(false);
	});
}

// Update node points
export function updatePoints(nodeRef: IGunChain, points: number): Promise<IGunChain> {
	return new Promise((resolve) => {
		nodeRef.once((node) => {
			if (node && node.type === 'NonRootNode') {
				nodeRef.get('points').put(points);
			}
			resolve(nodeRef);
		});
	});
}

// Update node name
export function updateName(nodeRef: IGunChain, name: string): Promise<IGunChain> {
	return new Promise((resolve) => {
		nodeRef.get('name').put(name);
		resolve(nodeRef);
	});
}

// Filter by categories using a mapping of nodeId to categories
export function categoryFilter(
	nodeCategories: Record<string, string[]>,
	categories: string[]
): (nodeId: string, share: number) => boolean {
	return (nodeId: string, _: number) => {
		const nodeCategory = nodeCategories[nodeId] || [];
		return nodeCategory.some((category) => categories.includes(category));
	};
}

// Compose multiple filters and apply them to a map
export function composeFilters(
	map: ShareMap,
	...predicates: Array<(nodeId: string, share: number) => boolean>
): ShareMap {
	// Convert predicates to a single combined predicate
	const combinedPredicate = combineFilters(...predicates);

	// Use filterShareMap with the combined predicate
	return filterShareMap(map, combinedPredicate);
}

// Helper to filter capacity shares using the capacity's filter rule
export function applyCapacityFilter(capacity: any, shareMap: ShareMap): Promise<ShareMap> {
	return new Promise(async (resolve) => {
		// If no filter rule exists, return the original map normalized
		if (!capacity.filter_rule) {
			resolve(normalizeShareMap({ ...shareMap }));
			return;
		}

		// Collect node context for filtering
		const nodeContext: Record<string, any> = {};
		for (const nodeId of Object.keys(shareMap)) {
			// Try to find the node in the system
			const nodeRef = gun.get(`~${nodeId}`);
			if (nodeRef) {
				const node = await new Promise<any>((res) => nodeRef.once(res));
				if (node) {
					nodeContext[nodeId] = node;
				}
			}
		}

		// Apply the jsonLogic filter
		const filteredMap = applyJsonLogicFilter(shareMap, capacity.filter_rule, nodeContext);
		resolve(filteredMap);
	});
}
