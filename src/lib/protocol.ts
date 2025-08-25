/**
 * Free Association Protocol
 *
 * This protocol implements a clean, intuitive node architecture:
 *
 * CONTRIBUTION NODES: Represent actual work done by people
 * - Have positive contributors who delivered the work
 * - Can have manual_fulfillment to assess work quality (defaults to 100%)
 * - Can have anti-contributors who hampered the work quality
 * - Anti-contributors only effective when manual_fulfillment < 100% (creates desire)
 *
 * NON-CONTRIBUTION NODES: Represent structural decomposition
 * - No direct contributors (abstract organization of work)
 * - Fulfillment calculated from weighted children (structural completion)
 * - Cannot have manual_fulfillment (would break recursive calculation)
 * - Cannot have anti-contributors (people don't hamper abstract structure)
 *
 * This clean separation preserves both semantic clarity and mathematical elegance.
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
	UserSlotQuantities
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
	if (node.type === 'RootNode') return false;

	const nonRootNode = node as NonRootNode;
	return nonRootNode.contributor_ids?.length > 0;
}

// Calculate the fulfillment value for a node
export function fulfilled(node: Node, tree: Node): number {
	// Helper predicates and values
	const isLeafNode = node.children.length === 0;
	const isContribNode = isContribution(node);

	// Leaf contribution node - allow manual fulfillment override
	if (isLeafNode && isContribNode) {
		return node.manual_fulfillment ?? 1.0;
	}

	// Leaf non-contribution node
	if (isLeafNode) {
		return 0.0;
	}

	// Non-leaf nodes: calculate weighted average fulfillment from children
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

/**
 * Helper Functions for Recognition Calculation
 */

// Resolve contributor ID to public key if resolver provided
function resolveContributorId(
	contributorId: string,
	resolveToPublicKey?: (id: string) => string | undefined
): string {
	return resolveToPublicKey ? resolveToPublicKey(contributorId) || contributorId : contributorId;
}

// Get unique resolved contributor IDs from a list
function getUniqueResolvedIds(
	contributorIds: string[],
	resolveToPublicKey?: (id: string) => string | undefined
): string[] {
	const resolvedIds = contributorIds.map((id) => resolveContributorId(id, resolveToPublicKey));
	return [...new Set(resolvedIds)];
}

// Check if a contributor appears in a list of contributor IDs
function contributorAppearsIn(
	targetContributorId: string,
	contributorIds: string[],
	resolveToPublicKey?: (id: string) => string | undefined
): boolean {
	return contributorIds.some(
		(id) => resolveContributorId(id, resolveToPublicKey) === targetContributorId
	);
}

// Calculate contributor share for a single node
function calculateNodeContributorShare(
	node: NonRootNode,
	contributorId: string,
	nodeWeight: number,
	nodeValue: number, // fulfillment for positive, desire for negative
	contributorIds: string[],
	resolveToPublicKey?: (id: string) => string | undefined
): number {
	const uniqueContributorIds = getUniqueResolvedIds(contributorIds, resolveToPublicKey);
	const contributorCount = uniqueContributorIds.length;
	return (nodeWeight * nodeValue) / contributorCount;
}

// Calculate a node's share of general fulfillment to another node
export function shareOfGeneralFulfillment(
	target: Node,
	contributorId: string,
	nodesMap: Record<string, Node>,
	resolveToPublicKey?: (id: string) => string | undefined
): number {
	const resolvedContributorId = resolveContributorId(contributorId, resolveToPublicKey);
	const targetAndDescendants = [target, ...getDescendants(target)];

	// Step 1: Calculate total influence pools
	let P_total = 0; // Total positive influence from contribution nodes
	let N_total = 0; // Total anti influence from contribution nodes with anti-contributors

	for (const node of targetAndDescendants) {
		if (node.type === 'NonRootNode') {
			const nodeWeight = weight(node, target);
			const nodeFulfillment = fulfilled(node, target);
			const nodeDesire = desire(node, target);

			// Positive influence: contribution nodes
			if (isContribution(node)) {
				P_total += nodeWeight * nodeFulfillment;

				// Anti influence: contribution nodes with anti-contributors
				const antiContributorIds = (node as NonRootNode).anti_contributors_ids || [];
				if (antiContributorIds.length > 0) {
					N_total += nodeWeight * nodeDesire;
				}
			}
		}
	}

	// Step 2: Calculate recognition pools
	const totalInfluence = P_total + N_total;
	const positivePool = totalInfluence > 0 ? P_total / totalInfluence : 0;
	const antiPool = totalInfluence > 0 ? N_total / totalInfluence : 0;

	// Step 3: Find relevant nodes and calculate shares
	let rawPositiveShare = 0;
	let rawAntiShare = 0;

	for (const node of targetAndDescendants) {
		if (node.type === 'NonRootNode') {
			const nonRootNode = node as NonRootNode;
			const nodeWeight = weight(node, target);
			const nodeFulfillment = fulfilled(node, target);
			const nodeDesire = desire(node, target);

			// Process positive contributions
			if (
				isContribution(node) &&
				contributorAppearsIn(resolvedContributorId, nonRootNode.contributor_ids, resolveToPublicKey)
			) {
				rawPositiveShare += calculateNodeContributorShare(
					nonRootNode,
					resolvedContributorId,
					nodeWeight,
					nodeFulfillment,
					nonRootNode.contributor_ids,
					resolveToPublicKey
				);
			}

			// Process negative contributions (only on contribution nodes)
			if (isContribution(node)) {
				const antiContributorIds = nonRootNode.anti_contributors_ids || [];
				if (
					antiContributorIds.length > 0 &&
					contributorAppearsIn(resolvedContributorId, antiContributorIds, resolveToPublicKey)
				) {
					rawAntiShare += calculateNodeContributorShare(
						nonRootNode,
						resolvedContributorId,
						nodeWeight,
						nodeDesire,
						antiContributorIds,
						resolveToPublicKey
					);
				}
			}
		}
	}

	// Step 4: Apply pool-bounded recognition
	const boundedPositiveShare = P_total > 0 ? (rawPositiveShare / P_total) * positivePool : 0;
	const boundedAntiShare = N_total > 0 ? (rawAntiShare / N_total) * antiPool : 0;

	return boundedPositiveShare - boundedAntiShare;
}

/**
 * Extract all unique contributor IDs from a tree (both positive and negative contributors)
 * If resolveToPublicKey is provided, resolves contact IDs to public keys
 */
export function getAllContributorsFromTree(
	tree: Node,
	resolveToPublicKey?: (id: string) => string | undefined
): string[] {
	const allContributorIds = new Set<string>();
	const allNodes = [tree, ...getDescendants(tree)];

	for (const node of allNodes) {
		if (node.type === 'NonRootNode') {
			const nonRootNode = node as NonRootNode;

			// Add positive contributors from contribution nodes
			if (isContribution(node)) {
				const resolvedPositiveIds = getUniqueResolvedIds(
					nonRootNode.contributor_ids,
					resolveToPublicKey
				);
				resolvedPositiveIds.forEach((id) => allContributorIds.add(id));

				// Add anti-contributors from contribution nodes
				const antiContributorIds = nonRootNode.anti_contributors_ids || [];
				const resolvedAntiIds = getUniqueResolvedIds(antiContributorIds, resolveToPublicKey);
				resolvedAntiIds.forEach((id) => allContributorIds.add(id));
			}
		}
	}

	return [...allContributorIds];
}

// Get normalized shares of general fulfillment map
export function sharesOfGeneralFulfillmentMap(
	rootNode: Node,
	nodesMap: Record<string, Node>,
	specificContributors?: string[],
	resolveToPublicKey?: (id: string) => string | undefined
): ShareMap {
	// Calculate all contributor shares
	const sharesMap: ShareMap = {};

	// Use specific contributors if provided, otherwise get contributors from tree
	const contributorIds =
		specificContributors || getAllContributorsFromTree(rootNode, resolveToPublicKey);

	// If specificContributors were provided, resolve them and filter out unresolvable ones
	const resolvedContributorIds = contributorIds
		.map((id) => (resolveToPublicKey ? resolveToPublicKey(id) || id : id))
		.filter((id) => {
			// Filter out contact IDs that couldn't be resolved to public keys
			// This prevents meaningless contact IDs from being included in network data
			if (id.startsWith('contact_') && resolveToPublicKey) {
				const resolved = resolveToPublicKey(id);
				if (!resolved || resolved === id) {
					console.warn(`[SOGF] Excluding unresolvable contact ID ${id} from SOGF calculation`);
					return false;
				}
			}
			return true;
		});

	// Remove duplicates after resolution and filtering
	const uniqueContributorIds = [...new Set(resolvedContributorIds)];

	console.log(`[SOGF] Calculating shares for ${uniqueContributorIds.length} valid contributors`);

	for (const contributorId of uniqueContributorIds) {
		const share = shareOfGeneralFulfillment(rootNode, contributorId, nodesMap, resolveToPublicKey);
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
	directMutualValue?: number,
	resolveToPublicKey?: (id: string) => string | undefined
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
	const sharesFromA = sharesOfGeneralFulfillmentMap(nodeA, nodesMap, undefined, resolveToPublicKey);
	const sharesFromB = sharesOfGeneralFulfillmentMap(nodeB, nodesMap, undefined, resolveToPublicKey);

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
	specificContributors?: string[],
	resolveToPublicKey?: (id: string) => string | undefined
): ShareMap {
	// Calculate direct contributor shares based on mutual fulfillment
	const contributorShares: ShareMap = {};

	// Use specific contributors if provided, otherwise find all contributors in the tree
	const contributorIds =
		specificContributors || getAllContributorsFromTree(provider, resolveToPublicKey);

	// If specificContributors were provided, resolve them too
	const resolvedContributorIds = contributorIds.map((id) =>
		resolveToPublicKey ? resolveToPublicKey(id) || id : id
	);

	// Remove duplicates after resolution
	const uniqueContributorIds = [...new Set(resolvedContributorIds)];

	// Calculate mutual fulfillment for each contributor
	for (const contributorId of uniqueContributorIds) {
		// Note: Contributors don't need to exist in nodesMap since they are external user IDs
		// The shareOfGeneralFulfillment function will find nodes that reference this contributorId

		const share = shareOfGeneralFulfillment(provider, contributorId, nodesMap, resolveToPublicKey);
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
	nodesMap: Record<string, Node>,
	resolveToPublicKey?: (id: string) => string | undefined
): number {
	// Get shares from the provider
	const providerShareMap = providerShares(provider, nodesMap, undefined, resolveToPublicKey);
	return providerShareMap[receiver.id] ?? 0;
}

/**
 * Capacity Utilities
 */

// Legacy function - kept for backward compatibility but no longer applies artificial constraints
// Real constraints are now applied at the slot level where quantities actually exist
export function constrainSharePercentage(capacity: BaseCapacity, percentage: number): number {
	// Simply return the percentage - no artificial constraints at share level
	return Math.min(percentage, 1.0);
}

// Simple discrete allocation: distribute integer units proportionally
export function allocateDiscreteUnits(shareMap: ShareMap, totalUnits: number): ShareMap {
	if (totalUnits === 0) return {};

	// Filter recipients with positive shares
	const recipients = Object.entries(shareMap).filter(([_, share]) => share > 0);
	if (recipients.length === 0) return {};

	// Phase 1: Allocate integer units proportionally (round down)
	const allocation: Record<string, number> = {};
	let totalAllocated = 0;

	for (const [recipientId, share] of recipients) {
		const allocatedUnits = Math.floor(share * totalUnits);
		if (allocatedUnits > 0) {
			allocation[recipientId] = allocatedUnits;
			totalAllocated += allocatedUnits;
		}
	}

	// Phase 2: Distribute remainder units to highest fractional parts
	const remainderUnits = totalUnits - totalAllocated;
	if (remainderUnits > 0) {
		// Calculate fractional parts for each recipient
		const fractionalParts = recipients
			.map(([recipientId, share]) => ({
				recipientId,
				fractionalPart: (share * totalUnits) % 1
			}))
			.filter(({ fractionalPart }) => fractionalPart > 0)
			.sort((a, b) => b.fractionalPart - a.fractionalPart);

		// Give 1 unit each to recipients with highest fractional parts
		for (let i = 0; i < Math.min(remainderUnits, fractionalParts.length); i++) {
			const { recipientId } = fractionalParts[i];
			allocation[recipientId] = (allocation[recipientId] || 0) + 1;
		}
	}

	// Convert back to percentages
	const result: ShareMap = {};
	for (const [recipientId, units] of Object.entries(allocation)) {
		if (units > 0) {
			result[recipientId] = units / totalUnits;
		}
	}

	return result;
}

// Apply capacity-level filtering only (no artificial percentage constraints)
// Real constraints are applied at the slot level where they belong
export function constrainShareMap(capacity: BaseCapacity, shareMap: ShareMap): ShareMap {
	// Only apply logical filtering here - no artificial percentage constraints
	// The real constraints (max_natural_div, max_percentage_div) are applied
	// at the slot level where quantities actually exist
	return normalizeShareMap(shareMap);
}

// Allocate discrete units for a specific slot using mutual fulfillment proportions
export function allocateSlotDiscreteUnits(
	slot: { id: string; quantity: number },
	shareMap: ShareMap
): Record<string, number> {
	// Use the discrete allocation algorithm for this specific slot
	const slotShareMap = allocateDiscreteUnits(shareMap, slot.quantity);

	// Convert back to actual quantities for this slot
	const result: Record<string, number> = {};
	for (const [recipientId, share] of Object.entries(slotShareMap)) {
		result[recipientId] = Math.round(share * slot.quantity);
	}

	return result;
}

// Compute quantity share for a single availability slot
// The percentage parameter is already discretely allocated at the capacity level,
// so we just apply it proportionally with natural divisibility constraints
export function computeSlotQuantityShare(
	slot: { id: string; quantity: number },
	capacity: BaseCapacity,
	percentage: number
): { slot_id: string; quantity: number } {
	const maxNatural = capacity.max_natural_div || 1;

	// Apply the pre-calculated discrete share percentage directly to this slot
	const rawQuantity = percentage * slot.quantity;

	// Apply natural divisibility constraint (round down to valid increments)
	const constrainedQuantity = Math.floor(rawQuantity / maxNatural) * maxNatural;

	return {
		slot_id: slot.id,
		quantity: constrainedQuantity
	};
}

// Compute quantity shares for a single slot distributed among multiple recipients
// This is the main function that should be used for proper discrete allocation
export function computeSlotQuantitySharesForRecipients(
	slot: { id: string; quantity: number },
	shareMap: ShareMap,
	capacity: BaseCapacity
): Record<string, { slot_id: string; quantity: number }> {
	const maxNatural = capacity.max_natural_div || 1;
	const maxPercent = capacity.max_percentage_div || 1;

	// Use slot-level discrete allocation
	const allocation = allocateSlotDiscreteUnits(slot, shareMap);

	// Convert to the expected format
	const result: Record<string, { slot_id: string; quantity: number }> = {};
	for (const [recipientId, quantity] of Object.entries(allocation)) {
		if (quantity > 0) {
			result[recipientId] = {
				slot_id: slot.id,
				quantity: quantity
			};
		}
	}

	return result;
}

// Compute quantity shares for all availability slots in a capacity (single recipient)
export function computeQuantityShares(
	capacity: BaseCapacity,
	percentage: number
): Array<{ slot_id: string; quantity: number }> {
	if (!capacity.availability_slots || !Array.isArray(capacity.availability_slots)) {
		console.log('[COMPUTE-QUANTITIES] No availability_slots found, returning empty array');
		return [];
	}

	return capacity.availability_slots.map((slot) =>
		computeSlotQuantityShare(slot, capacity, percentage)
	);
}

// Compute quantity shares for all slots distributed among multiple recipients
// This is the main function for calculating recipient shares across all slots
export function computeAllSlotQuantityShares(
	capacity: BaseCapacity,
	shareMap: ShareMap
): Record<string, Array<{ slot_id: string; quantity: number }>> {
	if (!capacity.availability_slots || !Array.isArray(capacity.availability_slots)) {
		console.log('[COMPUTE-ALL-SLOT-QUANTITIES] No availability_slots found, returning empty');
		return {};
	}

	const result: Record<string, Array<{ slot_id: string; quantity: number }>> = {};

	// Initialize result structure for all recipients
	for (const recipientId of Object.keys(shareMap)) {
		result[recipientId] = [];
	}

	// Process each slot independently with discrete allocation
	for (const slot of capacity.availability_slots) {
		const slotAllocations = computeSlotQuantitySharesForRecipients(slot, shareMap, capacity);

		// Add each recipient's allocation for this slot
		for (const [recipientId, allocation] of Object.entries(slotAllocations)) {
			if (!result[recipientId]) {
				result[recipientId] = [];
			}
			result[recipientId].push(allocation);
		}
	}

	return result;
}

// Legacy function for backward compatibility - returns total across all slots
export function computeQuantityShare(capacity: BaseCapacity, percentage: number): number {
	const slotShares = computeQuantityShares(capacity, percentage);
	return slotShares.reduce((total, slot) => total + slot.quantity, 0);
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
		computed_quantities: computeQuantityShares(baseCapacity, percentage),
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
export function createRootNode(id: string, name: string, manual?: number): RootNode {
	return {
		id,
		name,
		type: 'RootNode',
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
	antiContributorIds: string[] = [],
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
		anti_contributors_ids: antiContributorIds,
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
	antiContributorIds: string[] = [],
	manual?: number
): void {
	// Don't allow adding children to nodes with contributors or anti-contributors
	if (
		parentNode.type === 'NonRootNode' &&
		((parentNode as NonRootNode).contributor_ids.length > 0 ||
			((parentNode as NonRootNode).anti_contributors_ids || []).length > 0)
	) {
		throw new Error('Cannot add children to nodes with contributors or anti-contributors');
	}

	// Anti-contributors can be placed on:
	// 1. Contribution nodes (nodes with positive contributors)
	// 2. Empty leaf nodes (no contributors and no children) - but this doesn't apply to addChild since we're creating a new node
	if (antiContributorIds.length > 0 && contributorIds.length === 0) {
		throw new Error(
			'Anti-contributors can only be placed on contribution nodes (nodes with positive contributors).'
		);
	}

	// Create the new child
	const newChild = createNonRootNode(
		id,
		name,
		parentNode.id,
		points,
		contributorIds,
		antiContributorIds,
		manual
	);

	// Directly add the child to the parent's children array
	parentNode.children.push(newChild);
}

// Add contributors/anti-contributors to a node and clear its children (mutates the node)
export function addContributors(
	node: Node,
	contributorIds: string[] = [],
	antiContributorIds: string[] = []
): void {
	if (node.type === 'NonRootNode') {
		// Anti-contributors can be placed on:
		// 1. Contribution nodes (nodes with positive contributors)
		// 2. Empty leaf nodes (no contributors and no children)
		const hasContributors = contributorIds.length > 0;
		const isEmptyLeaf =
			node.children.length === 0 && (node as NonRootNode).contributor_ids.length === 0;

		if (antiContributorIds.length > 0 && !hasContributors && !isEmptyLeaf) {
			throw new Error(
				'Anti-contributors can only be placed on contribution nodes (nodes with positive contributors) or empty leaf nodes.'
			);
		}

		// Update contributors and anti-contributors, and clear children
		(node as NonRootNode).contributor_ids = [...contributorIds];
		(node as NonRootNode).anti_contributors_ids = [...antiContributorIds];
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
	subtreeContributorMap?: Record<string, Record<string, boolean>>,
	resolveToPublicKey?: (id: string) => string | undefined
): Record<string, UserSlotQuantities> {
	// Get raw shares based on provider
	const rawShares = providerShares(provider, nodesMap, undefined, resolveToPublicKey);

	// Apply capacity filter if one exists
	const context: FilterContext = {
		subtreeContributors: subtreeContributorMap || {}
	};
	const filteredShares = applyCapacityFilter(capacity, rawShares, context);

	// Apply discrete allocation constraints per slot to get actual recipient shares
	const discreteShares = calculateDiscreteRecipientShares(capacity, filteredShares);

	// Store the discrete shares in the capacity (for backward compatibility)
	capacity.recipient_shares = discreteShares;

	// Calculate actual slot quantities for each recipient
	const recipientSlotQuantities = calculateRecipientSlotQuantities(capacity, filteredShares);

	return recipientSlotQuantities;
}

// DEPRECATED: This function is no longer used in the new architecture
// Discrete allocation is now handled per-slot and actual quantities are stored,
// eliminating the need to average percentages across slots
function calculateDiscreteRecipientShares(capacity: BaseCapacity, shareMap: ShareMap): ShareMap {
	console.warn(
		'[DEPRECATED] calculateDiscreteRecipientShares is deprecated. Use calculateRecipientSlotQuantities instead.'
	);

	if (!capacity.availability_slots || capacity.availability_slots.length === 0) {
		return {};
	}

	const maxNatural = capacity.max_natural_div || 1;
	const maxPercent = capacity.max_percentage_div || 1;

	// Calculate what each recipient would get across all slots using discrete allocation
	const recipientIds = Object.keys(shareMap).filter((id) => shareMap[id] > 0);
	if (recipientIds.length === 0) {
		return {};
	}

	// For each recipient, calculate their average share across all slots
	const discreteShares: ShareMap = {};

	for (const recipientId of recipientIds) {
		let totalRecipientUnits = 0;
		let totalSlotUnits = 0;

		// Calculate discrete allocation for each slot
		for (const slot of capacity.availability_slots) {
			const slotAllocation = allocateDiscreteUnits(shareMap, slot.quantity);
			const recipientShare = slotAllocation[recipientId] || 0;
			const recipientUnits = Math.round(recipientShare * slot.quantity);

			totalRecipientUnits += recipientUnits;
			totalSlotUnits += slot.quantity;
		}

		// Calculate average share percentage across all slots
		if (totalSlotUnits > 0) {
			discreteShares[recipientId] = totalRecipientUnits / totalSlotUnits;
		}
	}

	return discreteShares;
}

// Calculate actual slot quantities for each recipient using discrete allocation
// This is the new approach that stores actual units per slot, not averaged percentages
export function calculateRecipientSlotQuantities(
	capacity: BaseCapacity,
	shareMap: ShareMap
): Record<string, UserSlotQuantities> {
	if (!capacity.availability_slots || capacity.availability_slots.length === 0) {
		return {};
	}

	const maxNatural = capacity.max_natural_div || 1;
	const maxPercent = capacity.max_percentage_div || 1;

	const recipientSlotQuantities: Record<string, UserSlotQuantities> = {};

	// Get all recipients with shares
	const recipientIds = Object.keys(shareMap).filter((id) => shareMap[id] > 0);

	// Initialize result structure
	for (const recipientId of recipientIds) {
		recipientSlotQuantities[recipientId] = {};
	}

	// For each slot, do discrete allocation and store actual quantities
	for (const slot of capacity.availability_slots) {
		// Do discrete allocation for this slot
		const slotAllocation = allocateDiscreteUnits(shareMap, slot.quantity);

		// Convert percentages to actual quantities for each recipient
		for (const [recipientId, sharePercent] of Object.entries(slotAllocation)) {
			const actualQuantity = Math.round(sharePercent * slot.quantity);

			if (actualQuantity > 0) {
				if (!recipientSlotQuantities[recipientId][capacity.id]) {
					recipientSlotQuantities[recipientId][capacity.id] = {};
				}
				recipientSlotQuantities[recipientId][capacity.id][slot.id] = actualQuantity;
			}
		}
	}

	return recipientSlotQuantities;
}

// Get all capacities where the receiver has shares from a provider
export function getReceiverCapacities(
	receiver: Node,
	provider: Node,
	capacities: CapacitiesCollection,
	nodesMap: Record<string, Node>,
	resolveToPublicKey?: (id: string) => string | undefined
): ProviderCapacity[] {
	// Filter capacities owned by the provider and ensure they are provider capacities
	const providerCapacities = Object.values(capacities).filter(
		(capacity): capacity is ProviderCapacity =>
			capacity.owner_id === provider.id && 'recipient_shares' in capacity
	);

	// Ensure all capacities have calculated recipient shares
	providerCapacities.forEach((capacity) => {
		calculateRecipientShares(capacity, provider, nodesMap, undefined, resolveToPublicKey);
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
	nodesMap: Record<string, Node>,
	resolveToPublicKey?: (id: string) => string | undefined
): Record<string, { capacity: ProviderCapacity; share: number; quantity: number }> {
	// Get all capacities where receiver has shares
	const receiverCapacities = getReceiverCapacities(
		receiver,
		provider,
		capacities,
		nodesMap,
		resolveToPublicKey
	);

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
	resolveToPublicKey?: (id: string) => string | undefined
): Record<string, Record<string, boolean>> {
	const subtreeMap: Record<string, Record<string, boolean>> = {};

	// Helper to get contributors from a subtree
	function getContributorsInSubtree(node: Node): string[] {
		const contributorIds = new Set<string>();
		const subtreeNodes = [node, ...getDescendants(node)];

		subtreeNodes.forEach((n) => {
			if (n.type === 'NonRootNode') {
				(n as NonRootNode).contributor_ids.forEach((id) => {
					// Resolve to public key if resolver is provided
					const resolvedId = resolveToPublicKey ? resolveToPublicKey(id) || id : id;
					contributorIds.add(resolvedId);
				});
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

// Get a map of subtree names to their anti-contributor lists
export function getSubtreeAntiContributorMap(
	tree: Node,
	resolveToPublicKey?: (id: string) => string | undefined
): Record<string, Record<string, boolean>> {
	const subtreeMap: Record<string, Record<string, boolean>> = {};

	// Helper to get anti-contributors from contribution nodes in a subtree
	function getAntiContributorsInSubtree(node: Node): string[] {
		const antiContributorIds = new Set<string>();
		const subtreeNodes = [node, ...getDescendants(node)];

		subtreeNodes.forEach((n) => {
			if (n.type === 'NonRootNode' && isContribution(n)) {
				const nodeAntiContributorIds = (n as NonRootNode).anti_contributors_ids || [];
				nodeAntiContributorIds.forEach((id) => {
					const resolvedId = resolveToPublicKey ? resolveToPublicKey(id) || id : id;
					antiContributorIds.add(resolvedId);
				});
			}
		});

		return [...antiContributorIds];
	}

	// Include the root
	subtreeMap[tree.id] = Object.fromEntries(
		getAntiContributorsInSubtree(tree).map((id) => [id, true])
	);

	// Include all child subtrees
	const allDescendants = getDescendants(tree);
	allDescendants.forEach((node) => {
		subtreeMap[node.id] = Object.fromEntries(
			getAntiContributorsInSubtree(node).map((id) => [id, true])
		);
	});

	return subtreeMap;
}

/**
 * Node Reordering Functions
 */

// Extract a subtree from its current location (returns the extracted subtree)
export function extractSubtree(tree: Node, nodeId: string): Node | null {
	// Can't extract the root node
	if (tree.id === nodeId) {
		return null;
	}

	// Find the parent of the node to extract
	const parent = getParentNode(tree, nodeId);
	if (!parent) {
		return null;
	}

	// Find the node in parent's children and extract it
	const nodeIndex = parent.children.findIndex((child) => child.id === nodeId);
	if (nodeIndex === -1) {
		return null;
	}

	// Remove and return the subtree
	const extractedNode = parent.children.splice(nodeIndex, 1)[0];
	return extractedNode;
}

// Insert a subtree at a new location (as child of target node)
export function insertSubtree(tree: Node, targetNodeId: string, subtree: Node): boolean {
	// Find the target node
	const targetNode = findNodeById(tree, targetNodeId);
	if (!targetNode) {
		return false;
	}

	// Update the subtree's parent_id if it's a NonRootNode
	if (subtree.type === 'NonRootNode') {
		(subtree as NonRootNode).parent_id = targetNodeId;
	}

	// Add the subtree as a child
	targetNode.children.push(subtree);
	return true;
}

// Check if moving a node would create a cycle (moving node to its own descendant)
export function wouldCreateCycle(tree: Node, nodeId: string, targetNodeId: string): boolean {
	// Can't move a node to itself
	if (nodeId === targetNodeId) {
		return true;
	}

	// Find the node to move
	const nodeToMove = findNodeById(tree, nodeId);
	if (!nodeToMove) {
		return false;
	}

	// Check if target is a descendant of the node to move
	const descendants = getDescendants(nodeToMove);
	return descendants.some((descendant) => descendant.id === targetNodeId);
}

// Reorder a node by moving it to a new parent
export function reorderNode(tree: Node, nodeId: string, newParentId: string): boolean {
	// Validate the move
	if (wouldCreateCycle(tree, nodeId, newParentId)) {
		return false;
	}

	// Can't move root node
	if (tree.id === nodeId) {
		return false;
	}

	// Can't move to a node that has contributors (contributors can't have children)
	const targetNode = findNodeById(tree, newParentId);
	if (targetNode && isContribution(targetNode)) {
		return false;
	}

	// Extract the subtree
	const subtree = extractSubtree(tree, nodeId);
	if (!subtree) {
		return false;
	}

	// Calculate appropriate points for the node in its new context
	// This ensures consistent sizing relative to new siblings
	if (subtree.type === 'NonRootNode') {
		const newPoints = calculateNodePoints(targetNode!);
		updatePoints(subtree as NonRootNode, newPoints);
	}

	// Insert at new location
	const success = insertSubtree(tree, newParentId, subtree);
	if (!success) {
		// If insertion failed, we need to put the subtree back
		// This is a bit tricky since we've already removed it
		// For now, just return false - in a real implementation you'd want to restore it
		return false;
	}

	return true;
}

/**
 * Node Point Calculation Functions
 */

// Calculate appropriate points for a node based on its siblings
// This uses the same logic as when creating new nodes
export function calculateNodePoints(parentNode: Node): number {
	// No siblings - set to 100 points
	if (!parentNode.children || parentNode.children.length === 0) {
		return 100;
	}

	// Has siblings - use 20% of total points
	const currentLevelPoints = parentNode.children.reduce((sum: number, child: Node) => {
		// Only count NonRootNode points
		return sum + (child.type === 'NonRootNode' ? (child as NonRootNode).points : 0);
	}, 0);

	const points = Math.max(1, currentLevelPoints * 0.2);
	return points;
}

// Re-export filter-related functions
export { filter, normalizeShareMap, applyCapacityFilter, Rules };
