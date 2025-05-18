/**
 * Free Association Protocol
 */

// Type definitions that match API response structures
export interface BaseNode {
	id: string;
	name: string;
	type: string;
	manual_fulfillment?: number;
	children: Node[];
}

export interface RootNode extends BaseNode {
	type: 'RootNode';
	user_id: string;
	capacities: Capacity[];
	created_at: string;
	updated_at: string;
}

export interface NonRootNode extends BaseNode {
	type: 'NonRootNode';
	points: number;
	parent_id: string;
	contributors: Node[];
}

export type Node = RootNode | NonRootNode;

export interface Capacity {
	id: string;
	name: string;
	quantity: number;
	unit: string;
	share_depth: number;
	expanded: boolean;
	location_type: string;
	all_day: boolean;
	recurrence?: string;
	custom_recurrence_repeat_every?: number;
	custom_recurrence_repeat_unit?: string;
	custom_recurrence_end_type?: string;
	custom_recurrence_end_value?: string;
	start_date?: string;
	start_time?: string;
	end_date?: string;
	end_time?: string;
	time_zone: string;
	max_natural_div: number;
	max_percentage_div: number;
	hidden_until_request_accepted: boolean;
	owner_id: string;
	shares: CapacityShare[];
}

export interface CapacityShare {
	id: string;
	share_percentage: number;
	computed_quantity: number;
	capacity_id: string;
	recipient_id: string;
}

export type ShareMap = Record<string, number>;

/**
 * Tree Navigation Functions
 */

// Find a node by ID in a tree
export function findNodeById(tree: Node, nodeId: string): Node | null {
	if (tree.id === nodeId) {
		return tree;
	}

	for (const child of tree.children) {
		const found = findNodeById(child, nodeId);
		if (found) {
			return found;
		}
	}

	return null;
}

// Get path from root to a specific node
export function getPathToNode(tree: Node, nodeId: string, path: string[] = []): string[] | null {
	if (tree.id === nodeId) {
		return [...path, tree.id];
	}

	for (const child of tree.children) {
		const childPath = getPathToNode(child, nodeId, [...path, tree.id]);
		if (childPath) {
			return childPath;
		}
	}

	return null;
}

// Get all descendants of a node (flattened array)
export function getDescendants(node: Node): Node[] {
	const result: Node[] = [];

	function traverse(n: Node) {
		for (const child of n.children) {
			result.push(child);
			traverse(child);
		}
	}

	traverse(node);
	return result;
}

// Get parent of a node in a tree
export function getParentNode(tree: Node, nodeId: string): Node | null {
	for (const child of tree.children) {
		if (child.id === nodeId) {
			return tree;
		}

		const parent = getParentNode(child, nodeId);
		if (parent) {
			return parent;
		}
	}

	return null;
}

/**
 * Core Calculations
 */

// Calculate total points from all children
export function totalChildPoints(node: Node): number {
	return node.children
		.filter((child) => child.type === 'NonRootNode')
		.reduce((sum, child) => sum + ((child as NonRootNode).points || 0), 0);
}

// Calculate a node's weight
export function weight(node: Node, tree: Node): number {
	// Root node always has weight 1.0
	if (node.type === 'RootNode' || node.id === tree.id) {
		return 1.0;
	}

	const parent = getParentNode(tree, node.id);
	if (!parent) {
		return 1.0; // Safety check
	}

	if (node.type !== 'NonRootNode') {
		return 1.0; // Should not happen, but for safety
	}

	// Get parent's total points
	const total = totalChildPoints(parent);

	// Calculate this node's contribution to parent (safely handling division by zero)
	const nodeContribution = total === 0 ? 0 : node.points / total;

	// Weight is recursive - multiply by parent's weight
	const parentWeight = weight(parent, tree);

	return nodeContribution * parentWeight;
}

// Calculate a node's share of its parent
export function shareOfParent(node: Node, tree: Node): number {
	// Root node has 100% share
	if (node.type === 'RootNode' || node.id === tree.id) {
		return 1.0;
	}

	const parent = getParentNode(tree, node.id);
	if (!parent) {
		return 1.0; // Safety check
	}

	// Calculate share for non-root nodes
	const parentTotal = totalChildPoints(parent);
	if (node.type !== 'NonRootNode') {
		return 0; // Should never happen for valid tree structure
	}

	return parentTotal === 0 ? 0 : node.points / parentTotal;
}

// Check if a node is a contribution node (has contributors and is not root)
export function isContribution(node: Node): boolean {
	return node.type === 'NonRootNode' && (node as NonRootNode).contributors?.length > 0;
}

// Calculate the fulfillment value for a node
export function fulfilled(node: Node, tree: Node): number {
	// Helper predicates and values
	const hasManualFulfillment = node.manual_fulfillment !== undefined;
	const isLeafNode = node.children.length === 0;
	const isContribNode = isContribution(node);
	const hasContribChildren = node.children.some((child) => isContribution(child));
	const hasNonContribChildren = node.children.some((child) => !isContribution(child));

	// Safely extract manual fulfillment value with a default
	const getManualValue = node.manual_fulfillment ?? 0.0;

	// Leaf contribution node
	if (isLeafNode && isContribNode) {
		return 1.0;
	}

	// Leaf non-contribution node
	if (isLeafNode) {
		return 0.0;
	}

	// Non-leaf node with manual fulfillment for contribution children only
	if (hasManualFulfillment && hasContribChildren && !hasNonContribChildren) {
		return getManualValue;
	}

	// For other nodes, calculate weighted average fulfillment from children
	const childWeights = node.children.map((child) => weight(child, tree));
	const childFulfillments = node.children.map((child) => fulfilled(child, tree));

	const weightedSum = childWeights.reduce((sum, w, i) => sum + w * childFulfillments[i], 0);
	const totalWeight = childWeights.reduce((sum, w) => sum + w, 0);

	return totalWeight === 0 ? 0 : weightedSum / totalWeight;
}

// Calculate the desire (unfulfilled need) of a node
export function desire(node: Node, tree: Node): number {
	return 1.0 - fulfilled(node, tree);
}

/**
 * Mutual Fulfillment
 */

// Calculate a node's share of general fulfillment to another node
export function shareOfGeneralFulfillment(
	target: Node,
	contributor: Node,
	nodesMap: Record<string, Node>
): number {
	const contributorId = contributor.id;

	// Find all nodes where this contributor is listed
	const targetAndDescendants = [target, ...getDescendants(target)];
	const contributingNodes = targetAndDescendants.filter((node) => {
		if (node.type === 'RootNode') return false; // Root nodes can't have contributors

		return (
			(node as NonRootNode).contributors.some((c) => c.id === contributorId) && isContribution(node)
		);
	});

	// Calculate weighted contribution for each node
	const weightedContributions = contributingNodes.map((node) => {
		const targetTree = nodesMap[target.id];
		const nodeWeight = weight(node, targetTree);
		const nodeFulfillment = fulfilled(node, targetTree);

		// Get contributor count
		const contributorCount =
			node.type === 'RootNode' ? 1 : (node as NonRootNode).contributors.length;

		return (nodeWeight * nodeFulfillment) / contributorCount;
	});

	// Sum the weighted contributions
	return weightedContributions.reduce((sum, w) => sum + w, 0);
}

// Get normalized shares of general fulfillment map
export function sharesOfGeneralFulfillmentMap(
	rootNode: Node,
	nodesMap: Record<string, Node>
): ShareMap {
	// Calculate all contributor shares
	const sharesMap: ShareMap = {};

	// Collect all potential contributors (all other root nodes)
	const contributorIds = Object.keys(nodesMap).filter((id) => id !== rootNode.id);

	for (const contributorId of contributorIds) {
		const contributor = nodesMap[contributorId];
		if (!contributor) continue;

		const share = shareOfGeneralFulfillment(rootNode, contributor, nodesMap);
		if (share > 0) {
			sharesMap[contributorId] = share;
		}
	}

	// Normalize the shares
	return normalizeShareMap(sharesMap);
}

// Calculate mutual fulfillment between two nodes
export function mutualFulfillment(
	nodeA: Node,
	nodeB: Node,
	nodesMap: Record<string, Node>
): number {
	// Get share maps
	const sharesFromA = sharesOfGeneralFulfillmentMap(nodeA, nodesMap);
	const sharesFromB = sharesOfGeneralFulfillmentMap(nodeB, nodesMap);

	// Extract shares with safe lookup (defaults to 0)
	const shareFromAToB = sharesFromA[nodeB.id] ?? 0;
	const shareFromBToA = sharesFromB[nodeA.id] ?? 0;

	// Mutual fulfillment is the minimum of shares each gives to the other
	return Math.min(shareFromAToB, shareFromBToA);
}

// Calculate provider-centric shares
export function providerShares(
	provider: Node,
	depth: number,
	nodesMap: Record<string, Node>
): ShareMap {
	if (depth === 1) {
		// Direct contributor shares based on mutual fulfillment
		return directContributorShares();
	} else {
		// Transitive shares for depth > 1
		return transitiveShares(depth);
	}

	// Helper function to calculate direct contributor shares
	function directContributorShares(): ShareMap {
		const contributorShares: ShareMap = {};

		// Find all contributors in the entire tree
		const allNodes = [provider, ...getDescendants(provider)];

		// Get all contributor IDs from all non-root nodes in the tree
		const allContributorIds = new Set<string>();
		for (const node of allNodes) {
			if (node.type === 'NonRootNode') {
				for (const contributor of (node as NonRootNode).contributors) {
					allContributorIds.add(contributor.id);
				}
			}
		}

		// Calculate mutual fulfillment for each contributor
		for (const contributorId of allContributorIds) {
			const contributor = nodesMap[contributorId];
			if (!contributor) continue;

			const mf = mutualFulfillment(provider, contributor, nodesMap);
			if (mf > 0) {
				contributorShares[contributorId] = mf;
			}
		}

		return normalizeShareMap(contributorShares);
	}

	// Helper function to calculate transitive shares
	function transitiveShares(d: number): ShareMap {
		let currentShares = directContributorShares();

		// Iteratively combine transitive shares
		for (let i = 2; i <= d; i++) {
			currentShares = combineTransitiveShares(currentShares);
		}

		return currentShares;
	}

	// Helper function to combine transitive shares
	function combineTransitiveShares(currentShares: ShareMap): ShareMap {
		const resultMap: ShareMap = {};

		for (const [recipientId, share] of Object.entries(currentShares)) {
			const recipient = nodesMap[recipientId];
			if (!recipient) continue;

			const recipientDirectShares = providerShares(recipient, 1, nodesMap);

			for (const [subRecipientId, subShare] of Object.entries(recipientDirectShares)) {
				const weightedShare = share * subShare;
				resultMap[subRecipientId] = (resultMap[subRecipientId] || 0) + weightedShare;
			}
		}

		return normalizeShareMap(resultMap);
	}
}

/**
 * Capacity Utilities
 */

// Compute quantity share based on capacity and percentage
export function computeQuantityShare(capacity: Capacity, percentage: number): number {
	const rawQuantity = Math.round(capacity.quantity * percentage);
	const maxNatural = capacity.max_natural_div;
	const maxPercent = capacity.max_percentage_div;

	// Apply percentage divisibility constraint
	const percentConstrained =
		percentage > maxPercent ? Math.round(capacity.quantity * maxPercent) : rawQuantity;

	// Apply natural number divisibility constraint
	const naturalConstrained = Math.floor(percentConstrained / maxNatural) * maxNatural;

	return naturalConstrained;
}

// Create a new capacity share
export function createCapacityShare(
	capacityId: string,
	recipientId: string,
	percentage: number,
	capacity: Capacity
): CapacityShare {
	return {
		id: crypto.randomUUID(), // Generate random ID for new shares
		capacity_id: capacityId,
		recipient_id: recipientId,
		share_percentage: percentage,
		computed_quantity: computeQuantityShare(capacity, percentage)
	};
}

// Get a receiver's share from a specific capacity provider
export function receiverShareFrom(
	receiver: Node,
	provider: Node,
	capacity: Capacity,
	maxDepth: number,
	nodesMap: Record<string, Node>
): number {
	// Get shares from the provider
	const providerShareMap = providerShares(provider, maxDepth, nodesMap);
	return providerShareMap[receiver.id] ?? 0;
}

/**
 * Utility Functions
 */

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

/**
 * Tree Modification API (Mutable)
 */

// Validate and normalize manual fulfillment values
export function validateManualFulfillment(value: number | undefined): number | undefined {
	if (value === undefined) return undefined;
	return Math.max(0, Math.min(1, value)); // Clamp between 0 and 1
}

// Create a new root node
export function createRootNode(
	id: string,
	name: string,
	userId: string,
	manual?: number
): RootNode {
	return {
		id,
		name,
		type: 'RootNode',
		user_id: userId,
		children: [],
		manual_fulfillment: validateManualFulfillment(manual),
		capacities: [],
		created_at: new Date().toISOString(),
		updated_at: new Date().toISOString()
	};
}

// Create a non-root node
export function createNonRootNode(
	id: string,
	name: string,
	parentId: string,
	pts: number,
	contributors: Node[] = [],
	manual?: number
): NonRootNode {
	return {
		id,
		name,
		type: 'NonRootNode',
		points: pts,
		parent_id: parentId,
		children: [],
		contributors,
		manual_fulfillment: validateManualFulfillment(manual)
	};
}

// Directly add a child to a node (mutates the parent)
export function addChild(
	parentNode: Node,
	id: string,
	name: string,
	points: number,
	contributors: Node[] = [],
	manual?: number
): void {
	// Don't allow adding children to nodes with contributors
	if (parentNode.type === 'NonRootNode' && parentNode.contributors.length > 0) {
		throw new Error('Cannot add children to nodes with contributors');
	}

	// Create the new child
	const newChild = createNonRootNode(id, name, parentNode.id, points, contributors, manual);

	// Directly add the child to the parent's children array
	parentNode.children.push(newChild);
}

// Add contributors to a node and clear its children (mutates the node)
export function addContributors(node: Node, contributors: Node[]): void {
	if (node.type === 'NonRootNode') {
		// Update contributors and clear children
		node.contributors = [...contributors];
		node.children = [];
	} else {
		// Root nodes can't have contributors, but we still clear children
		node.children = [];
	}
}

// Delete all children from a node (mutates the node)
export function deleteSubtree(node: Node): void {
	node.children = [];
}

// Update the manual fulfillment of a node (mutates the node)
export function updateManualFulfillment(node: Node, value: number | undefined): void {
	node.manual_fulfillment = validateManualFulfillment(value);
}

// Add a capacity to a node (mutates the node)
export function addCapacity(node: RootNode, capacity: Capacity): void {
	node.capacities.push(capacity);
}

// Add a capacity share to a node (mutates the node)
export function addCapacityShare(node: RootNode, share: CapacityShare): void {
	// Find the matching capacity if it exists
	const capacity = node.capacities.find((cap) => cap.id === share.capacity_id);

	if (capacity) {
		// Add the share to the capacity's shares array
		capacity.shares.push(share);
	}
}

// Helper to find and update a node directly in a tree (mutates the tree)
export function updateNodeById(
	tree: Node,
	nodeId: string,
	updater: (node: Node) => void
): boolean {
	// Check if this is the node to update
	if (tree.id === nodeId) {
		updater(tree);
		return true;
	}

	// Otherwise search through children
	for (const child of tree.children) {
		if (updateNodeById(child, nodeId, updater)) {
			return true;
		}
	}

	return false;
}

// Update node points (mutates the node)
export function updatePoints(node: NonRootNode, points: number): void {
	node.points = points;
}

// Update node name (mutates the node)
export function updateName(node: Node, name: string): void {
	node.name = name;
}
