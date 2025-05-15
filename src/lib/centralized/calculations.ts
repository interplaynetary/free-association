import type { Forest, Node, TreeZipper, ShareMap } from './types';
import {
	enterChild,
	exitToParent,
	goToRoot,
	children,
	getAllDescendants,
	descendants
} from './tree';
import { normalizeShareMap } from './utils';

// ==== Core Calculations ====

/**
 * Calculate total points from all immediate children
 * @param z TreeZipper to calculate for
 * @returns Total points from all children
 */
export function totalChildPoints(z: TreeZipper): number {
	// Sum the points of all immediate children
	return children(z).reduce((total, child) => {
		const childNode = child.zipperCurrent;
		return childNode.type === 'non-root' ? total + childNode.nodePoints : total;
	}, 0);
	}

/**
 * Calculate a node's weight (recursively)
 * @param z TreeZipper to calculate for
 * @returns Weight value (0-1)
 */
export function weight(z: TreeZipper): number {
	// Root node always has weight 1.0
	if (!z.zipperContext) {
		return 1.0;
	}

	// Get parent's contribution and weight
		const parent = exitToParent(z);
	if (!parent) {
		return 1.0; // Default to 1.0 if parent cannot be found
	}

	// Get total points of parent's children
		const total = totalChildPoints(parent);

	// Calculate this node's contribution to parent
	const current = z.zipperCurrent;
	const nodeContribution =
		total === 0 ? 0 : current.type === 'non-root' ? current.nodePoints / total : 0;

	// Weight is recursive - multiply by parent's weight
		const parentWeight = weight(parent);
	return nodeContribution * parentWeight;
	}

/**
 * Calculate a node's share of its parent
 * @param zipper TreeZipper to calculate for
 * @returns Share value (0-1)
 */
export function shareOfParent(zipper: TreeZipper): number {
	// Root node has 100% share
	const parent = exitToParent(zipper);
	if (!parent) return 1.0;

	// Calculate share for non-root nodes
	const total = totalChildPoints(parent);
	if (total === 0) return 0;

	const current = zipper.zipperCurrent;
	return current.type === 'non-root' ? current.nodePoints / total : 0;
}

/**
 * Check if a node is a contribution node
 * @param zipper TreeZipper to check
 * @returns true if node is a contribution node
 */
export function isContribution(zipper: TreeZipper): boolean {
	const node = zipper.zipperCurrent;
	return (
		node.type === 'non-root' && node.nodeContributors.size > 0 && zipper.zipperContext !== null
	);
}

/**
 * Check if a node has direct contribution children
 * @param zipper TreeZipper to check
 * @returns true if node has contribution children
 */
export function hasDirectContributionChild(zipper: TreeZipper): boolean {
	return children(zipper).some(isContribution);
}

/**
 * Check if a node has non-contribution children
 * @param zipper TreeZipper to check
 * @returns true if node has non-contribution children
 */
export function hasNonContributionChild(zipper: TreeZipper): boolean {
	return children(zipper).some((child) => !isContribution(child));
		}

/**
 * Calculate the proportion of total child points from contribution children
 * @param z TreeZipper to calculate for
 * @returns Weight value (0-1)
 */
export function contributionChildrenWeight(z: TreeZipper): number {
	// Get all child zippers
	const childZippers = children(z);

	// Partition children into contribution and non-contribution nodes
	const contribChildren = childZippers.filter(isContribution);

	// Calculate weights for each category
	const contribWeights = contribChildren.map(weight);
	const allWeights = childZippers.map(weight);

	// Sum the weights (safely handle empty lists)
	const contribWeightSum = contribWeights.reduce((a, b) => a + b, 0);
	const totalWeightSum = allWeights.reduce((a, b) => a + b, 0);

	// Handle division by zero
	return totalWeightSum === 0 ? 0 : contribWeightSum / totalWeightSum;
}

/**
 * Sum fulfillment from children matching a predicate
 * @param predicate Function to test children
 * @param z TreeZipper to calculate for
 * @returns Weighted fulfillment sum
 */
export function childrenFulfillment(predicate: (z: TreeZipper) => boolean, z: TreeZipper): number {
	return children(z)
		.filter(predicate)
		.reduce((sum, child) => sum + fulfilled(child) * shareOfParent(child), 0);
}

/**
 * Calculate the fulfillment from contribution children
 * @param z TreeZipper to calculate for
 * @returns Fulfillment value (0-1)
 */
export function contributionChildrenFulfillment(z: TreeZipper): number {
	return childrenFulfillment(isContribution, z);
}

/**
 * Calculate the fulfillment from non-contribution children
 * @param z TreeZipper to calculate for
 * @returns Fulfillment value (0-1)
 */
export function nonContributionChildrenFulfillment(z: TreeZipper): number {
	// Get all child zippers
	const childZippers = children(z);

	// Filter to non-contribution children only
	const nonContribChildren = childZippers.filter((child) => !isContribution(child));

	if (nonContribChildren.length === 0) return 0;

	// Get weight and fulfillment for each child
	const childWeights = nonContribChildren.map(weight);
	const childFulfillments = nonContribChildren.map(fulfilled);

	// Calculate weighted sum and total weight
	let weightedSum = 0;
	let totalWeight = 0;

	for (let i = 0; i < nonContribChildren.length; i++) {
		weightedSum += childWeights[i] * childFulfillments[i];
		totalWeight += childWeights[i];
	}

	// Safely handle division by zero
	return totalWeight === 0 ? 0 : weightedSum / totalWeight;
}

/**
 * Safely get contributor instances from the forest
 * @param forest Forest of nodes
 * @param contribId Contributor ID to search for
 * @returns Set of TreeZippers for the contributor
 */
export function getContributorInstances(forest: Forest, contribId: string): Set<TreeZipper> {
	const result = new Set<TreeZipper>();
	const contrib = forest.get(contribId);

	if (contrib) {
		result.add(contrib);
	}

	return result;
}

/**
 * Safely get a contributor node
 * @param forest Forest of nodes
 * @param contribId Contributor ID to search for
 * @returns TreeZipper for the contributor or null
 */
export function getContributorNode(forest: Forest, contribId: string): TreeZipper | null {
	return forest.get(contribId) || null;
}

/**
 * Calculate the fulfillment of a node
 * @param z TreeZipper to calculate for
 * @returns Fulfillment value (0-1)
 */
export function fulfilled(z: TreeZipper): number {
	const current = z.zipperCurrent;

	// Helper predicates
	const hasManualFulfillment = current.nodeManualFulfillment !== null;
	const isLeafNode = current.nodeChildren.size === 0;
	const isContribNode = isContribution(z);
	const hasContribChildren = hasDirectContributionChild(z);
	const hasNonContribChildren = hasNonContributionChild(z);

	// Safely extract manual fulfillment value with a default
	const getManualValue =
		current.nodeManualFulfillment !== null ? current.nodeManualFulfillment : 0.0;

	// Calculate the manual contribution share
	const manualContribShare = () => {
			const contribWeight = contributionChildrenWeight(z);
			const nonContribFulfillment = nonContributionChildrenFulfillment(z);
		return getManualValue * contribWeight + nonContribFulfillment * (1.0 - contribWeight);
	};

	// Main fulfillment calculation with pattern matching
	if (isLeafNode && isContribNode) {
		// Leaf contribution node
		return 1.0;
	} else if (isLeafNode) {
		// Leaf non-contribution node
		return 0.0;
	} else if (hasManualFulfillment && hasContribChildren && !hasNonContribChildren) {
		// Non-leaf node with manual fulfillment for contribution children only
		return getManualValue;
	} else if (hasManualFulfillment && hasContribChildren) {
		// Non-leaf node with mixed children types and manual fulfillment
		return manualContribShare();
	} else {
		// Other nodes (aggregate from children)
		return children(z).reduce((sum, child) => sum + fulfilled(child) * shareOfParent(child), 0);
	}
}

/**
 * Calculate the desire (unfulfilled need) of a node
 * @param zipper TreeZipper to calculate for
 * @returns Desire value (0-1)
 */
export function desire(zipper: TreeZipper): number {
	return 1.0 - fulfilled(zipper);
}

/**
 * Calculate the share of general fulfillment between target and contributor
 * @param ci Forest of contributor nodes
 * @param target Target TreeZipper
 * @param contributor Contributor TreeZipper
 * @returns Share value (0-1)
 */
export function shareOfGeneralFulfillment(
	ci: Forest,
	target: TreeZipper,
	contributor: TreeZipper
): number {
	const contributorId = contributor.zipperCurrent.nodeId;
	const rootContributor = ci.get(contributorId);

	if (!rootContributor) return 0;

	// Find only nodes where this contributor is listed
	const allNodes = [target, ...descendants(target)];
	const contributingNodes = allNodes.filter((node) => {
		const current = node.zipperCurrent;
		if (current.type !== 'non-root') return false;
		return current.nodeContributors.has(contributorId) && isContribution(node);
	});

	// Calculate weighted contribution for each node
	return contributingNodes.reduce((sum, node) => {
		const nodeWeight = weight(node);
		const nodeFulfillment = fulfilled(node);
		const contributorCount =
			node.zipperCurrent.type === 'non-root' ? node.zipperCurrent.nodeContributors.size : 1;

		return sum + (nodeWeight * nodeFulfillment) / contributorCount;
	}, 0);
}

/**
 * Generate a normalized map of shares-of-general-fulfillment for a node
 * @param ci Forest of contributor nodes
 * @param z TreeZipper to calculate for
 * @returns Map of contributor IDs to share values
 */
export function sharesOfGeneralFulfillmentMap(ci: Forest, z: TreeZipper): ShareMap {
	const current = z.zipperCurrent;

	// For non-root nodes, get the root's map
	if (current.type === 'non-root') {
		return sharesOfGeneralFulfillmentMap(ci, goToRoot(z));
	}

	// For root nodes, check if there's a cached map
	if (current.type === 'root' && current.nodeSOGFMap !== null) {
		return current.nodeSOGFMap;
	}

	// Calculate all contributor shares
	const contributions: Array<[string, number]> = [];
	ci.forEach((contributorZipper, contributorId) => {
		const share = shareOfGeneralFulfillment(ci, z, contributorZipper);
		if (share > 0) {
			contributions.push([contributorId, share]);
		}
	});

	// Create the map and normalize it
	const rawShareMap = new Map(contributions);
	const normalizedMap = normalizeShareMap(rawShareMap);

	// If this is a root node, update the cached map
	if (current.type === 'root') {
		current.nodeSOGFMap = normalizedMap;
	}

	return normalizedMap;
}

/**
 * Calculate depth-1 provider shares (direct contributions)
 * @param ci Forest of contributor nodes
 * @param provider Provider TreeZipper
 * @returns Map of recipient IDs to share values
 */
function directContributorShares(ci: Forest, provider: TreeZipper): ShareMap {
	// Get all nodes in the provider's tree
	const allNodes = getAllDescendants(provider);

	// Extract all contributor IDs from all nodes
	const allContributorIds = new Set<string>();
	allNodes.forEach((node) => {
		const current = node.zipperCurrent;
		if (current.type === 'non-root') {
			current.nodeContributors.forEach((id) => allContributorIds.add(id));
		}
	});

	// Get valid contributors that exist in the forest
	const validContributors: TreeZipper[] = [];
	allContributorIds.forEach((id) => {
		const contributor = ci.get(id);
		if (contributor) {
			validContributors.push(contributor);
		}
	});

	// Calculate mutual fulfillment for each contributor
	const contributorShares: Array<[string, number]> = [];
	validContributors.forEach((c) => {
		const id = c.zipperCurrent.nodeId;
		const mf = mutualFulfillment(ci, provider, c);
		if (mf > 0) {
			contributorShares.push([id, mf]);
}
	});

	// Create and return the map
	return new Map(contributorShares);
}

/**
 * Calculate provider shares at a specific depth
 * @param ci Forest of contributor nodes
 * @param provider Provider TreeZipper
 * @param depth Depth level
 * @returns Map of recipient IDs to share values
 */
export function providerShares(ci: Forest, provider: TreeZipper, depth: number): ShareMap {
	const current = provider.zipperCurrent;

	// For non-root nodes, go to the root and calculate from there
	if (current.type === 'non-root') {
		return providerShares(ci, goToRoot(provider), depth);
	}

	// For root nodes, check if there's a cached map for this depth
	if (current.type === 'root' && current.nodeProviderSharesMap.has(depth)) {
		return current.nodeProviderSharesMap.get(depth)!;
	}

	// Calculate a fresh map
	let shareMap: ShareMap;

	if (depth === 1) {
		// Direct contributions
		shareMap = directContributorShares(ci, provider);
	} else {
		// Transitive contributions
		shareMap = calculateTransitiveShares(ci, provider, depth);
	}

	// Normalize the map
	const normalizedMap = normalizeShareMap(shareMap);

	// If this is a root node, update the cached map
	if (current.type === 'root') {
		current.nodeProviderSharesMap.set(depth, normalizedMap);
	}

	return normalizedMap;
}

/**
 * Calculate transitive shares for depth > 1
 * @param ci Forest of contributor nodes
 * @param provider Provider TreeZipper
 * @param maxDepth Maximum depth
 * @returns Map of recipient IDs to share values
 */
function calculateTransitiveShares(ci: Forest, provider: TreeZipper, maxDepth: number): ShareMap {
	// Start with depth 1 shares
	let currentShares = directContributorShares(ci, provider);

	// For each additional depth level
	for (let d = 2; d <= maxDepth; d++) {
		const nextShares = new Map<string, number>();

		// For each current recipient
		for (const [recipientId, share] of currentShares.entries()) {
			const recipient = ci.get(recipientId);
			if (!recipient) continue;

			// Get the recipient's direct shares
			const recipientDirectShares = providerShares(ci, recipient, 1);

			// Weight the shares by the current share
			for (const [subRecipientId, subShare] of recipientDirectShares.entries()) {
				const weightedShare = subShare * share;
				nextShares.set(subRecipientId, (nextShares.get(subRecipientId) || 0) + weightedShare);
			}
		}

		currentShares = nextShares;
	}

	return currentShares;
}

/**
 * Calculate mutual fulfillment between two nodes
 * @param ci Forest of contributor nodes
 * @param a First TreeZipper
 * @param b Second TreeZipper
 * @returns Mutual fulfillment value (0-1)
 */
export function mutualFulfillment(ci: Forest, a: TreeZipper, b: TreeZipper): number {
	const aId = a.zipperCurrent.nodeId;
	const bId = b.zipperCurrent.nodeId;

	// Get share maps
	const sharesFromA = sharesOfGeneralFulfillmentMap(ci, a);
	const sharesFromB = sharesOfGeneralFulfillmentMap(ci, b);

	// Extract shares with safe lookup (defaults to 0)
	const shareFromAToB = sharesFromA.get(bId) || 0;
	const shareFromBToA = sharesFromB.get(aId) || 0;

	// Mutual fulfillment is the minimum of shares each gives to the other
	return Math.min(shareFromAToB, shareFromBToA);
}

/**
 * Get a receiver's share from a specific capacity provider
 * @param ci Forest of contributor nodes
 * @param receiver Receiver TreeZipper
 * @param provider Provider TreeZipper
 * @param capacity Capacity object
 * @param maxDepth Maximum depth for share calculation
 * @returns Share value (0-1)
 */
export function receiverShareFrom(
	ci: Forest,
	receiver: TreeZipper,
	provider: TreeZipper,
	capacity: any,
	maxDepth: number
): number {
	// Get shares from the provider's root node
	const providerRoot = goToRoot(provider);
	const providerShareMap = providerShares(ci, providerRoot, maxDepth);
	const receiverId = receiver.zipperCurrent.nodeId;

	return providerShareMap.get(receiverId) || 0;
}
