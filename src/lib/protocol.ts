/**
 * Free Association Protocol
 */

// Import json-logic-js with type assertion
// @ts-ignore
import jsonLogic from 'json-logic-js';
import type {
	Node,
	RootNode,
	NonRootNode,
	Capacity,
	CapacitiesCollection,
	ShareMap,
	ProviderCapacity,
	RecipientCapacity,
	BaseCapacity
} from '$lib/schema';

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

// lets implement the filters that normalize
/*
Filters can be added on top of the sharesMap for a particular Surplus, and the filtered map can then be normalized to provide a distribution. This is useful for example to provide to mutual-contributors to general-self-actualization, who satisfy a specific criteria. For example, a filter could distribute shares only to people who you recognize as contributing in a particular category, or all those except those in a block-list. We should be able to pass in any predicate into the filter function
*/

// Filter shares based on a predicate function
export function filterShareMap(
	shareMap: ShareMap,
	predicate: (nodeId: string, share: number) => boolean
): ShareMap {
	// Create a new map with only entries that satisfy the predicate
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
	return (nodeId: string, _: number) => includeList.includes(nodeId);
}

// Remove nodes in the exclude list
export function excludeFilter(excludeList: string[]): (nodeId: string, share: number) => boolean {
	return (nodeId: string, _: number) => !excludeList.includes(nodeId);
}

// Keep only nodes with shares above a threshold
export function thresholdFilter(minShare: number): (nodeId: string, share: number) => boolean {
	return (_: string, share: number) => share >= minShare;
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

// Combine multiple filters using AND logic
export function combineFilters(
	...filters: ((nodeId: string, share: number) => boolean)[]
): (nodeId: string, share: number) => boolean {
	return (nodeId: string, share: number) => {
		return filters.every((filter) => filter(nodeId, share));
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

/**
 * JSON Logic Filter System
 * This allows storing filter rules in a portable JSON format
 */

// Check if a contributor appears in a specific subtree
export function isInSubtree(
	contributorId: string,
	subtreeRootId: string,
	nodesMap: Record<string, Node>
): boolean {
	const subtreeRoot = nodesMap[subtreeRootId];
	if (!subtreeRoot) return false;

	// Get all nodes in the subtree (including the root)
	const subtreeNodes = [subtreeRoot, ...getDescendants(subtreeRoot)];

	// Check if the contributor appears in any node in the subtree
	return subtreeNodes.some((node) => {
		if (node.type === 'NonRootNode') {
			return (node as NonRootNode).contributor_ids.includes(contributorId);
		}
		return false;
	});
}

// Get all contributors that appear in a specific subtree
export function getContributorsInSubtree(
	subtreeRootId: string,
	nodesMap: Record<string, Node>
): string[] {
	const subtreeRoot = nodesMap[subtreeRootId];
	if (!subtreeRoot) return [];

	const contributorIds = new Set<string>();
	const subtreeNodes = [subtreeRoot, ...getDescendants(subtreeRoot)];

	subtreeNodes.forEach((node) => {
		if (node.type === 'NonRootNode') {
			(node as NonRootNode).contributor_ids.forEach((id) => contributorIds.add(id));
		}
	});

	return [...contributorIds];
}

// Get a map of subtree names to their contributor lists
export function getSubtreeContributorMap(
	tree: Node,
	nodesMap: Record<string, Node>
): Record<string, { name: string; contributors: string[] }> {
	const subtreeMap: Record<string, { name: string; contributors: string[] }> = {};

	// Include the root
	subtreeMap[tree.id] = {
		name: tree.name,
		contributors: getContributorsInSubtree(tree.id, nodesMap)
	};

	// Include all child subtrees
	const allDescendants = getDescendants(tree);
	allDescendants.forEach((node) => {
		subtreeMap[node.id] = {
			name: node.name,
			contributors: getContributorsInSubtree(node.id, nodesMap)
		};
	});

	return subtreeMap;
}

// Apply a jsonLogic rule to filter a share map
export function applyJsonLogicFilter(
	shareMap: ShareMap,
	rule: any,
	nodeContext: Record<string, Node> = {},
	subtreeContributorMap?: Record<string, Record<string, boolean>>
): ShareMap {
	// Create a predicate that evaluates the rule for each node
	const predicate = (nodeId: string, share: number) => {
		// Create the data object for jsonLogic to evaluate
		const data = {
			nodeId,
			share,
			node: nodeContext[nodeId] || null,
			// Add subtree contributor lookup for efficient filtering
			subtreeContributors: subtreeContributorMap || {}
		};

		// Apply the rule using jsonLogic and ensure boolean return
		// Add type assertion to handle the unknown return type from jsonLogic
		const result = jsonLogic.apply(rule, data) as boolean;
		return Boolean(result);
	};

	// Use our existing filter system
	return filterShareMap(shareMap, predicate);
}

// Helper to filter capacity shares using the capacity's filter rule
export function applyCapacityFilter(
	capacity: BaseCapacity,
	shareMap: ShareMap,
	nodeContext: Record<string, Node> = {},
	subtreeContributorMap?: Record<string, Record<string, boolean>>
): ShareMap {
	// If no filter rule exists, return the original map normalized
	if (!capacity.filter_rule) {
		return normalizeShareMap({ ...shareMap });
	}

	console.log('[FILTER-DEBUG] Applying filter:', capacity.filter_rule);
	console.log('[FILTER-DEBUG] Subtree contributor map:', subtreeContributorMap);
	console.log('[FILTER-DEBUG] Share map before filter:', shareMap);

	// Apply the jsonLogic filter
	const result = applyJsonLogicFilter(shareMap, capacity.filter_rule, nodeContext, subtreeContributorMap);
	
	console.log('[FILTER-DEBUG] Share map after filter:', result);
	return result;
}

// Common predicates as JsonLogic rules
export const FilterRules = {
	// Include only specified nodeIds
	includeNodes: (nodeIds: string[]): any => ({
		in: [{ var: 'nodeId' }, nodeIds]
	}),

	// Exclude specified nodeIds
	excludeNodes: (nodeIds: string[]): any => ({
		'!': { in: [{ var: 'nodeId' }, nodeIds] }
	}),

	// Only include nodes with share above threshold
	aboveThreshold: (threshold: number): any => ({
		'>=': [{ var: 'share' }, threshold]
	}),

	// Filter by node category (assuming node.categories exists in context)
	byCategory: (categories: string[]): any => ({
		some: [{ var: 'node.categories' }, { in: [{ var: '' }, categories] }]
	}),

	// Filter by contributors that appear in specific subtrees
	inSubtrees: (subtreeIds: string[]): any => ({
		some: [
			subtreeIds,
			{
				'!!': { var: ['subtreeContributors', { var: '' }, { var: 'nodeId' }] }
			}
		]
	}),

	// Filter by contributors that appear in a specific subtree
	inSubtree: (subtreeId: string): any => ({
		in: [{ var: 'nodeId' }, { var: ['subtreeContributors', subtreeId] }]
	}),

	// Combine multiple rules with AND
	and: (...rules: any[]): any => ({
		and: rules
	}),

	// Combine multiple rules with OR
	or: (...rules: any[]): any => ({
		or: rules
	})
};

/**
 * Example usage:
 *
 * // Create a capacity with a filter rule that only distributes to contributors
 * // who appear in specific subtrees (categories) of your tree
 * const capacity: Capacity = {
 *   // ... other capacity fields
 *   filter_rule: FilterRules.and(
 *     FilterRules.excludeNodes(['blocked-user-1', 'blocked-user-2']),
 *     FilterRules.aboveThreshold(0.05),
 *     FilterRules.inSubtree('art-projects-subtree-id') // Only contributors from art projects
 *   )
 * };
 *
 * // Or filter by multiple subtrees
 * const multiSubtreeCapacity: Capacity = {
 *   // ... other capacity fields
 *   filter_rule: FilterRules.inSubtrees(['art-projects-id', 'music-projects-id'])
 * };
 *
 * // Apply the filter to distribute the capacity
 * const tree = get(userTree);
 * const nodesMap = get(nodesMap);
 * const subtreeMap = get(subtreeContributorMap);
 * const shareMap = providerShares(provider, nodesMap);
 * const filteredShares = applyCapacityFilter(capacity, shareMap, nodesMap, subtreeMap);
 */

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
	const filteredShares = applyCapacityFilter(capacity, rawShares, nodesMap, subtreeContributorMap);

	// Store the filtered shares in the capacity
	capacity.recipient_shares = filteredShares;
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
