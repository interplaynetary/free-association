/**
 * Free Association Protocol
 */
import type {
	Node,
	RootNode,
	NonRootNode,
	Capacity,
	CapacitiesCollection,
	ShareMap,
	ProviderCapacity,
	RecipientCapacity,
	BaseCapacity,
	Commitment,
	CommitmentsCollection
} from '$lib/schema';
import {
	filter,
	normalizeShareMap,
	applyCapacityFilter,
	Rules,
	type FilterContext
} from '$lib/filters';

// Type definitions are now imported from schema.ts instead of being defined here

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
		.filter((child: Node) => child.type === 'NonRootNode')
		.reduce((sum: number, child: NonRootNode) => sum + (child.points || 0), 0);
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
	return node.type === 'NonRootNode' && (node as NonRootNode).contributor_ids?.length > 0;
}

// Calculate the fulfillment value for a node
export function fulfilled(node: Node, tree: Node): number {
	// Helper predicates and values
	const hasManualFulfillment = node.manual_fulfillment !== undefined;
	const isLeafNode = node.children.length === 0;
	const isContribNode = isContribution(node);
	const hasContribChildren = node.children.some((child: Node) => isContribution(child));
	const hasNonContribChildren = node.children.some((child: Node) => !isContribution(child));

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
	const childWeights = node.children.map((child: Node) => weight(child, tree));
	const childFulfillments = node.children.map((child: Node) => fulfilled(child, tree));

	const weightedSum = childWeights.reduce(
		(sum: number, w: number, i: number) => sum + w * childFulfillments[i],
		0
	);
	const totalWeight = childWeights.reduce((sum: number, w: number) => sum + w, 0);

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
	contributorId: string,
	nodesMap: Record<string, Node>
): number {
	// Find all nodes where this contributor is listed
	const targetAndDescendants = [target, ...getDescendants(target)];
	const contributingNodes = targetAndDescendants.filter((node) => {
		if (node.type === 'RootNode') return false; // Root nodes can't have contributors

		return (node as NonRootNode).contributor_ids.includes(contributorId) && isContribution(node);
	});

	// Calculate weighted contribution for each node
	const weightedContributions = contributingNodes.map((node) => {
		// Use the target node itself as the tree for weight calculations
		const nodeWeight = weight(node, target);
		const nodeFulfillment = fulfilled(node, target);

		// Get contributor count
		const contributorCount =
			node.type === 'RootNode' ? 1 : (node as NonRootNode).contributor_ids.length;

		return (nodeWeight * nodeFulfillment) / contributorCount;
	});

	// Sum the weighted contributions
	return weightedContributions.reduce((sum, w) => sum + w, 0);
}

/**
 * Extract all unique contributor IDs from a tree
 */
export function getAllContributorsFromTree(tree: Node): string[] {
	const allContributorIds = new Set<string>();
	const allNodes = [tree, ...getDescendants(tree)];

	for (const node of allNodes) {
		if (node.type === 'NonRootNode') {
			for (const contributorId of (node as NonRootNode).contributor_ids) {
				allContributorIds.add(contributorId);
			}
		}
	}

	return [...allContributorIds];
}

// Get normalized shares of general fulfillment map
export function sharesOfGeneralFulfillmentMap(
	rootNode: Node,
	nodesMap: Record<string, Node>,
	specificContributors?: string[]
): ShareMap {
	// Calculate all contributor shares
	const sharesMap: ShareMap = {};

	// Use specific contributors if provided, otherwise consider all nodes except root
	const contributorIds =
		specificContributors || Object.keys(nodesMap).filter((id) => id !== rootNode.id);

	for (const contributorId of contributorIds) {
		const share = shareOfGeneralFulfillment(rootNode, contributorId, nodesMap);
		// FIXED: Include all contributors, even those with 0 shares
		// This ensures Gun properly updates network data when contributors are removed
		sharesMap[contributorId] = share;
	}

	// Normalize the shares (this will handle the case where all shares are 0)
	return normalizeShareMap(sharesMap);
}

// Calculate mutual fulfillment between two nodes
export function mutualFulfillment(
	nodeA: Node,
	nodeB: Node,
	nodesMap: Record<string, Node>,
	cachedRecognition?: { ourShare: number; theirShare: number } | null,
	directMutualValue?: number
): number {
	// Use direct mutual value if provided (from the derived store)
	if (directMutualValue !== undefined) {
		return directMutualValue;
	}

	// Use cached values if provided (for network lookups)
	if (cachedRecognition) {
		return Math.min(cachedRecognition.ourShare, cachedRecognition.theirShare);
	}

	// Otherwise calculate locally
	// Get share maps
	const sharesFromA = sharesOfGeneralFulfillmentMap(nodeA, nodesMap);
	const sharesFromB = sharesOfGeneralFulfillmentMap(nodeB, nodesMap);

	// Extract shares with safe lookup (defaults to 0)
	const shareFromAToB = sharesFromA[nodeB.id] ?? 0;
	const shareFromBToA = sharesFromB[nodeA.id] ?? 0;

	// Mutual fulfillment is the minimum of shares each gives to the other
	return Math.min(shareFromAToB, shareFromBToA);
}

// Calculate provider-centric shares (simplified to direct shares only)
export function providerShares(
	provider: Node,
	nodesMap: Record<string, Node>,
	specificContributors?: string[]
): ShareMap {
	// Calculate direct contributor shares based on mutual fulfillment
	const contributorShares: ShareMap = {};

	// Use specific contributors if provided, otherwise find all contributors in the tree
	const contributorIds = specificContributors || getAllContributorsFromTree(provider);

	// Calculate mutual fulfillment for each contributor
	for (const contributorId of contributorIds) {
		// Note: Contributors don't need to exist in nodesMap since they are external user IDs
		// The shareOfGeneralFulfillment function will find nodes that reference this contributorId

		const share = shareOfGeneralFulfillment(provider, contributorId, nodesMap);
		// FIXED: Include all contributors, even those with 0 shares
		// This ensures consistency with SOGF and proper network updates
		contributorShares[contributorId] = share;
	}

	return normalizeShareMap(contributorShares);
}

// Get a receiver's share from a specific capacity provider
export function receiverGeneralShareFrom(
	receiver: Node,
	provider: Node,
	capacity: Capacity,
	nodesMap: Record<string, Node>
): number {
	// Get shares from the provider
	const providerShareMap = providerShares(provider, nodesMap);
	return providerShareMap[receiver.id] ?? 0;
}

/**
 * Capacity Utilities
 */

// Constrain a share percentage according to max_percentage_div and max_natural_div
export function constrainSharePercentage(capacity: BaseCapacity, percentage: number): number {
	const maxPercent = capacity.max_percentage_div || 1;
	const maxNatural = capacity.max_natural_div || 1;
	const quantity = capacity.quantity || 0;

	// If quantity is 0, no meaningful constraints can be applied
	if (quantity === 0) {
		return percentage;
	}

	// Calculate valid percentages based on natural divisibility
	// Example: max_natural_div=5, quantity=20 → naturalStep=0.25 → valid percentages: 0, 0.25, 0.5, 0.75, 1.0
	const naturalStep = maxNatural / quantity;

	// Calculate valid percentages based on percentage divisibility
	// Example: max_percentage_div=0.25 → valid percentages: 0, 0.25, 0.5, 0.75, 1.0
	const percentStep = maxPercent;

	// Use the more restrictive constraint (smaller step size)
	const effectiveStep = Math.min(naturalStep, percentStep);

	// Round to the nearest valid step
	const constrainedPercentage = Math.round(percentage / effectiveStep) * effectiveStep;

	// Ensure we don't exceed 100%
	return Math.min(constrainedPercentage, 1.0);
}

// Apply percentage divisibility constraints to a share map
export function constrainShareMap(capacity: BaseCapacity, shareMap: ShareMap): ShareMap {
	const constrainedMap: ShareMap = {};

	// Apply constraints to each share
	for (const [recipientId, share] of Object.entries(shareMap)) {
		const constrainedShare = constrainSharePercentage(capacity, share);
		if (constrainedShare > 0) {
			// Only include non-zero shares
			constrainedMap[recipientId] = constrainedShare;
		}
	}

	// Normalize to ensure they still sum to 1.0
	return normalizeShareMap(constrainedMap);
}

// Compute quantity share based on capacity and percentage
export function computeQuantityShare(capacity: BaseCapacity, percentage: number): number {
	const rawQuantity = Math.round((capacity.quantity || 0) * percentage);
	const maxNatural = capacity.max_natural_div || 1;
	const maxPercent = capacity.max_percentage_div || 1;

	// Apply percentage divisibility constraint
	const percentConstrained =
		percentage > maxPercent ? Math.round((capacity.quantity || 0) * maxPercent) : rawQuantity;

	// Apply natural number divisibility constraint
	const naturalConstrained = Math.floor(percentConstrained / maxNatural) * maxNatural;

	return naturalConstrained;
}

// Create a new capacity share
export function createRecipientCapacity(
	baseCapacity: BaseCapacity,
	providerId: string,
	percentage: number
): RecipientCapacity {
	return {
		...baseCapacity,
		share_percentage: percentage,
		computed_quantity: computeQuantityShare(baseCapacity, percentage),
		provider_id: providerId
	};
}

// Add a capacity to a collection
export function addCapacity(capacities: CapacitiesCollection, capacity: Capacity): void {
	capacities[capacity.id] = capacity;
}

// Add a capacity share to a capacity in a collection
export function addCapacityShare(capacities: CapacitiesCollection, share: RecipientCapacity): void {
	// Find the matching capacity if it exists
	const capacity = capacities[share.id];

	if (capacity && 'recipient_shares' in capacity) {
		// Add the share to the capacity's recipient_shares map
		capacity.recipient_shares[share.provider_id] = share.share_percentage;
	}
}

/**
 * Utility Functions
 */

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
export function validateManualFulfillment(value: number | undefined): number | null {
	return value === undefined ? null : Math.max(0, Math.min(1, value)); // Clamp between 0 and 1
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
	contributorIds: string[] = [],
	manual?: number
): NonRootNode {
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

// Directly add a child to a node (mutates the parent)
export function addChild(
	parentNode: Node,
	id: string,
	name: string,
	points: number,
	contributorIds: string[] = [],
	manual?: number
): void {
	// Don't allow adding children to nodes with contributors
	if (parentNode.type === 'NonRootNode' && (parentNode as NonRootNode).contributor_ids.length > 0) {
		throw new Error('Cannot add children to nodes with contributors');
	}

	// Create the new child
	const newChild = createNonRootNode(id, name, parentNode.id, points, contributorIds, manual);

	// Directly add the child to the parent's children array
	parentNode.children.push(newChild);
}

// Add contributors to a node and clear its children (mutates the node)
export function addContributors(node: Node, contributorIds: string[]): void {
	if (node.type === 'NonRootNode') {
		// Update contributors and clear children
		(node as NonRootNode).contributor_ids = [...contributorIds];
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

// Helper to find and update a node directly in a tree (mutates the tree)
export function updateNodeById(tree: Node, nodeId: string, updater: (node: Node) => void): boolean {
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

// Calculate recipient shares for a provider capacity
export function calculateRecipientShares(
	capacity: ProviderCapacity,
	provider: Node,
	nodesMap: Record<string, Node>,
	subtreeContributorMap?: Record<string, Record<string, boolean>>
): void {
	// Get raw shares based on provider
	const rawShares = providerShares(provider, nodesMap);

	// Apply capacity filter if one exists
	const context: FilterContext = {
		node: nodesMap,
		subtreeContributors: subtreeContributorMap
	};
	const filteredShares = applyCapacityFilter(capacity, rawShares, context);

	// Apply percentage divisibility constraints
	const constrainedShares = constrainShareMap(capacity, filteredShares);

	// Store the constrained shares in the capacity
	capacity.recipient_shares = constrainedShares;
}

// Get all capacities where the receiver has shares from a provider
export function getReceiverCapacities(
	receiver: Node,
	provider: Node,
	capacities: CapacitiesCollection,
	nodesMap: Record<string, Node>
): ProviderCapacity[] {
	// Filter capacities owned by the provider and ensure they are provider capacities
	const providerCapacities = Object.values(capacities).filter(
		(capacity): capacity is ProviderCapacity =>
			capacity.owner_id === provider.id && 'recipient_shares' in capacity
	);

	// Ensure all capacities have calculated recipient shares
	providerCapacities.forEach((capacity) => {
		calculateRecipientShares(capacity, provider, nodesMap);
	});

	// Return capacities where the receiver has a share
	return providerCapacities.filter(
		(capacity) =>
			capacity.recipient_shares[receiver.id] !== undefined &&
			capacity.recipient_shares[receiver.id] > 0
	);
}

// Get a receiver's shares across all capacities of a provider
export function getReceiverShares(
	receiver: Node,
	provider: Node,
	capacities: CapacitiesCollection,
	nodesMap: Record<string, Node>
): Record<string, { capacity: ProviderCapacity; share: number; quantity: number }> {
	// Get all capacities where receiver has shares
	const receiverCapacities = getReceiverCapacities(receiver, provider, capacities, nodesMap);

	// Create a map of capacity ID to share information
	const sharesMap: Record<string, { capacity: ProviderCapacity; share: number; quantity: number }> =
		{};

	receiverCapacities.forEach((capacity) => {
		const share = capacity.recipient_shares[receiver.id] || 0;
		sharesMap[capacity.id] = {
			capacity,
			share,
			quantity: computeQuantityShare(capacity, share)
		};
	});

	return sharesMap;
}

// Get a map of subtree names to their contributor lists
export function getSubtreeContributorMap(
	tree: Node,
	nodesMap: Record<string, Node>
): Record<string, Record<string, boolean>> {
	const subtreeMap: Record<string, Record<string, boolean>> = {};

	// Helper to get contributors from a subtree
	function getContributorsInSubtree(node: Node): string[] {
		const contributorIds = new Set<string>();
		const subtreeNodes = [node, ...getDescendants(node)];

		subtreeNodes.forEach((n) => {
			if (n.type === 'NonRootNode') {
				(n as NonRootNode).contributor_ids.forEach((id) => contributorIds.add(id));
			}
		});

		return [...contributorIds];
	}

	// Include the root
	subtreeMap[tree.id] = Object.fromEntries(getContributorsInSubtree(tree).map((id) => [id, true]));

	// Include all child subtrees
	const allDescendants = getDescendants(tree);
	allDescendants.forEach((node) => {
		subtreeMap[node.id] = Object.fromEntries(
			getContributorsInSubtree(node).map((id) => [id, true])
		);
	});

	return subtreeMap;
}

/**
 * Commitment Functions
 */

// Create a new commitment
export function createCommitment(
	id: string,
	name: string,
	percentage: number,
	recipientId: string,
	committerId: string,
	description?: string,
	tags: string[] = []
): Commitment {
	return {
		id,
		name,
		description,
		percentage: Math.max(0, Math.min(1, percentage)), // Clamp between 0 and 1
		recipient_id: recipientId,
		committer_id: committerId,
		created_at: new Date().toISOString(),
		updated_at: new Date().toISOString(),
		active: true,
		tags
	};
}

// Update an existing commitment
export function updateCommitment(
	commitment: Commitment,
	updates: Partial<Pick<Commitment, 'name' | 'description' | 'percentage' | 'active' | 'tags'>>
): Commitment {
	return {
		...commitment,
		...updates,
		percentage: updates.percentage !== undefined 
			? Math.max(0, Math.min(1, updates.percentage)) 
			: commitment.percentage,
		updated_at: new Date().toISOString()
	};
}

// Get all active commitments by a user
export function getCommitmentsByCommitter(
	commitments: CommitmentsCollection,
	committerId: string,
	activeOnly: boolean = true
): Commitment[] {
	return Object.values(commitments).filter(
		commitment => 
			commitment.committer_id === committerId && 
			(!activeOnly || commitment.active)
	);
}

// Get all active commitments to a user
export function getCommitmentsToRecipient(
	commitments: CommitmentsCollection,
	recipientId: string,
	activeOnly: boolean = true
): Commitment[] {
	return Object.values(commitments).filter(
		commitment => 
			commitment.recipient_id === recipientId && 
			(!activeOnly || commitment.active)
	);
}

// Calculate total percentage committed by a user
export function getTotalCommittedPercentage(
	commitments: CommitmentsCollection,
	committerId: string
): number {
	const userCommitments = getCommitmentsByCommitter(commitments, committerId);
	return userCommitments.reduce((total, commitment) => total + commitment.percentage, 0);
}

// Calculate remaining percentage available for new commitments
export function getRemainingCommitmentCapacity(
	commitments: CommitmentsCollection,
	committerId: string
): number {
	const totalCommitted = getTotalCommittedPercentage(commitments, committerId);
	return Math.max(0, 1 - totalCommitted);
}

// Calculate the committed share of recognition for a specific recipient
export function getCommittedShareForRecipient(
	commitments: CommitmentsCollection,
	committerId: string,
	recipientId: string
): number {
	const userCommitments = getCommitmentsByCommitter(commitments, committerId);
	return userCommitments
		.filter(commitment => commitment.recipient_id === recipientId)
		.reduce((total, commitment) => total + commitment.percentage, 0);
}

// Apply commitments to modify share distribution
export function applyCommitments(
	baseShares: ShareMap,
	commitments: CommitmentsCollection,
	committerId: string
): ShareMap {
	const modifiedShares: ShareMap = { ...baseShares };
	const userCommitments = getCommitmentsByCommitter(commitments, committerId);
	
	// Apply each commitment
	userCommitments.forEach(commitment => {
		const currentShare = modifiedShares[commitment.recipient_id] || 0;
		modifiedShares[commitment.recipient_id] = currentShare + commitment.percentage;
	});
	
	// Normalize to ensure total doesn't exceed 1.0
	return normalizeShareMap(modifiedShares);
}

// Validate that a new commitment won't exceed 100% total
export function validateCommitment(
	commitments: CommitmentsCollection,
	committerId: string,
	newPercentage: number,
	excludeCommitmentId?: string
): { valid: boolean; error?: string; availableCapacity: number } {
	const existingCommitments = getCommitmentsByCommitter(commitments, committerId)
		.filter(c => c.id !== excludeCommitmentId);
	
	const totalExisting = existingCommitments.reduce((sum, c) => sum + c.percentage, 0);
	const availableCapacity = 1 - totalExisting;
	
	if (newPercentage > availableCapacity) {
		return {
			valid: false,
			error: `Commitment of ${(newPercentage * 100).toFixed(1)}% exceeds available capacity of ${(availableCapacity * 100).toFixed(1)}%`,
			availableCapacity
		};
	}
	
	return { valid: true, availableCapacity };
}

// Add a commitment to a collection
export function addCommitment(
	commitments: CommitmentsCollection,
	commitment: Commitment
): boolean {
	const validation = validateCommitment(commitments, commitment.committer_id, commitment.percentage);
	
	if (!validation.valid) {
		return false;
	}
	
	commitments[commitment.id] = commitment;
	return true;
}

// Remove a commitment from a collection
export function removeCommitment(
	commitments: CommitmentsCollection,
	commitmentId: string
): boolean {
	if (commitments[commitmentId]) {
		delete commitments[commitmentId];
		return true;
	}
	return false;
}

// Deactivate a commitment instead of removing it (for audit trail)
export function deactivateCommitment(
	commitments: CommitmentsCollection,
	commitmentId: string
): boolean {
	const commitment = commitments[commitmentId];
	if (commitment) {
		commitments[commitmentId] = updateCommitment(commitment, { active: false });
		return true;
	}
	return false;
}

// Re-export filter-related functions
export { filter, normalizeShareMap, applyCapacityFilter, Rules };
