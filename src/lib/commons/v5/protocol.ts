/**
 * Free Association Protocol v5 - Global Recognition Model
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
 *
 * V5 ARCHITECTURE:
 * - All schemas imported from ./schemas.ts (Multi-dimensional slots, ITC-based)
 * - Tree operations for recognition/priority calculations
 * - Multi-dimensional slot-native two-tier allocation
 * - GLOBAL recognition (same MR for all types, tree structure encodes type preferences)
 * - Hierarchical availability windows (yearly → monthly → weekly → daily)
 * - Filter utilities for capacity/need constraints
 * - No round coordination (event-driven)
 * - ITC causality tracking
 */

// All types from v5 schemas
import type {
	Node,
	RootNode,
	NonRootNode,
	NodeDataStorage,
	ShareMap,
	ResourceMetadata,
	AvailabilitySlot,
	NeedSlot,
	Commitment,
	TwoTierAllocationState,
	SlotAllocationRecord,
	ITCStamp,
	GlobalRecognitionWeights,
	NeedType,
	AvailabilityWindow,
	Contributor
} from './schemas';

// Filter utilities (from main lib)
import {
	filter,
	normalizeShareMap,
	applyCapacityFilter,
	Rules,
	type FilterContext
} from '$lib/filters';

// Slot matching utilities (v5 - multi-dimensional with hierarchical availability windows)
import {
	timeRangesOverlap,
	locationsCompatible,
	slotsCompatible
} from './match.svelte';

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
	return nonRootNode.contributors?.length > 0;
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

// Get unique resolved contributor IDs from a list (V5: works with Contributor objects)
function getUniqueResolvedIds(
	contributors: Contributor[],
	resolveToPublicKey?: (id: string) => string | undefined
): string[] {
	const resolvedIds = contributors.map((c) => resolveContributorId(c.id, resolveToPublicKey));
	return [...new Set(resolvedIds)];
}

// Calculate total points for a set of contributors
function totalContributorPoints(contributors: Contributor[]): number {
	return contributors.reduce((sum, c) => sum + c.points, 0);
}

// Find a contributor in a list and return their entry (V5: works with Contributor objects)
function findContributor(
	targetContributorId: string,
	contributors: Contributor[],
	resolveToPublicKey?: (id: string) => string | undefined
): Contributor | undefined {
	return contributors.find(
		(c) => resolveContributorId(c.id, resolveToPublicKey) === targetContributorId
	);
}

// Calculate contributor share for a single node (V5: weighted by contributor points)
function calculateNodeContributorShare(
	node: NonRootNode,
	contributorId: string,
	nodeWeight: number,
	nodeValue: number, // fulfillment for positive, desire for negative
	contributors: Contributor[],
	resolveToPublicKey?: (id: string) => string | undefined
): number {
	// Find this contributor's entry
	const contributor = findContributor(contributorId, contributors, resolveToPublicKey);
	if (!contributor) return 0;
	
	// Calculate total points across all contributors
	const totalPoints = totalContributorPoints(contributors);
	if (totalPoints === 0) return 0;
	
	// Share = (node's weight × node's value) × (contributor's points / total points)
	return (nodeWeight * nodeValue) * (contributor.points / totalPoints);
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
				const antiContributors = (node as NonRootNode).anti_contributors || [];
				if (antiContributors.length > 0) {
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

		// Process positive contributions (V5: weighted by contributor points)
		if (isContribution(node)) {
			const contributors = nonRootNode.contributors || [];
			const contributor = findContributor(resolvedContributorId, contributors, resolveToPublicKey);
			if (contributor) {
				rawPositiveShare += calculateNodeContributorShare(
					nonRootNode,
					resolvedContributorId,
					nodeWeight,
					nodeFulfillment,
					contributors,
					resolveToPublicKey
				);
			}
		}

		// Process negative contributions (V5: weighted by anti-contributor points)
		if (isContribution(node)) {
			const antiContributors = nonRootNode.anti_contributors || [];
			const antiContributor = findContributor(resolvedContributorId, antiContributors, resolveToPublicKey);
			if (antiContributor) {
				rawAntiShare += calculateNodeContributorShare(
					nonRootNode,
					resolvedContributorId,
					nodeWeight,
					nodeDesire,
					antiContributors,
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
 * V5: Works with weighted Contributor objects
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
				const contributors = nonRootNode.contributors || [];
				const resolvedPositiveIds = getUniqueResolvedIds(
					contributors,
					resolveToPublicKey
				);
				resolvedPositiveIds.forEach((id) => allContributorIds.add(id));

				// Add anti-contributors from contribution nodes
				const antiContributors = nonRootNode.anti_contributors || [];
				const resolvedAntiIds = getUniqueResolvedIds(antiContributors, resolveToPublicKey);
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

	// Resolve contributors to public keys when possible, but preserve contact IDs without public keys
	const resolvedContributorIds = contributorIds
		.map((id) => {
			if (resolveToPublicKey) {
				const resolved = resolveToPublicKey(id);
				// If we got a valid resolution (not null and not the same ID), use it
				if (resolved && resolved !== id) {
					console.log(`[SOGF] Resolved ${id} → ${resolved}`);
					return resolved;
				}
				// If it's a contact ID that couldn't be resolved, keep the contact ID
				if (id.startsWith('contact_')) {
					console.log(`[SOGF] Preserving unresolvable contact ID ${id} (no public key available)`);
					return id;
				}
			}
			// For public keys or when no resolver provided, return as-is
			return id;
		})
		// Only filter out truly invalid IDs (empty strings, null, undefined)
		.filter((id) => id && id.trim() !== '');

	// Remove duplicates after resolution and filtering
	const uniqueContributorIds = [...new Set(resolvedContributorIds)];

	console.log(
		`[SOGF] Calculating shares for ${uniqueContributorIds.length} valid contributors:`,
		uniqueContributorIds
	);

	for (const contributorId of uniqueContributorIds) {
		const share = shareOfGeneralFulfillment(rootNode, contributorId, nodesMap, resolveToPublicKey);
		// Include all contributors, even those with 0 shares
		// This ensures network data properly updates when contributors are removed
		sharesMap[contributorId] = share;

		// Log each contributor's share for debugging
		const contributorType = contributorId.startsWith('contact_') ? 'contact' : 'pubkey';
		console.log(`[SOGF] ${contributorType} ${contributorId}: ${share.toFixed(4)} share`);
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
		const share = shareOfGeneralFulfillment(provider, contributorId, nodesMap, resolveToPublicKey);
		// Include all contributors, even those with 0 shares
		contributorShares[contributorId] = share;
	}

	return normalizeShareMap(contributorShares);
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

// Create a non-root node (V5: with weighted contributors)
export function createNonRootNode(
	id: string,
	name: string,
	parentId: string,
	pts: number,
	contributors: Contributor[] = [],
	antiContributors: Contributor[] = [],
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
		anti_contributors: antiContributors,
		manual_fulfillment: validateManualFulfillment(manual)
	};
}

// Directly add a child to a node (V5: with weighted contributors, mutates the parent)
export function addChild(
	parentNode: Node,
	id: string,
	name: string,
	points: number,
	contributors: Contributor[] = [],
	antiContributors: Contributor[] = [],
	manual?: number
): void {
	// Don't allow adding children to nodes with contributors or anti-contributors
	if (
		parentNode.type === 'NonRootNode' &&
		((parentNode as NonRootNode).contributors.length > 0 ||
			((parentNode as NonRootNode).anti_contributors || []).length > 0)
	) {
		throw new Error('Cannot add children to nodes with contributors or anti-contributors');
	}

	// Anti-contributors can be placed on:
	// 1. Contribution nodes (nodes with positive contributors)
	// 2. Empty leaf nodes (no contributors and no children) - but this doesn't apply to addChild since we're creating a new node
	if (antiContributors.length > 0 && contributors.length === 0) {
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
		contributors,
		antiContributors,
		manual
	);

	// Directly add the child to the parent's children array
	parentNode.children.push(newChild);
}

/**
 * Add contributors/anti-contributors to a node and clear its children (V5: weighted contributors)
 * 
 * Default points strategy:
 * - New contributors get average points of existing contributors
 * - If no existing contributors, default to 100 points each
 * 
 * @param node - Node to add contributors to
 * @param contributorIds - IDs of contributors to add
 * @param antiContributorIds - IDs of anti-contributors to add
 * @param contributorPoints - Optional: explicit points for each contributor (if not provided, uses average)
 * @param antiContributorPoints - Optional: explicit points for each anti-contributor
 */
export function addContributors(
	node: Node,
	contributorIds: string[] = [],
	antiContributorIds: string[] = [],
	contributorPoints?: number[],
	antiContributorPoints?: number[]
): void {
	if (node.type === 'NonRootNode') {
		const nonRootNode = node as NonRootNode;
		
		// Anti-contributors can be placed on:
		// 1. Contribution nodes (nodes with positive contributors)
		// 2. Empty leaf nodes (no contributors and no children)
		const hasContributors = contributorIds.length > 0;
		const isEmptyLeaf =
			node.children.length === 0 && nonRootNode.contributors.length === 0;

		if (antiContributorIds.length > 0 && !hasContributors && !isEmptyLeaf) {
			throw new Error(
				'Anti-contributors can only be placed on contribution nodes (nodes with positive contributors) or empty leaf nodes.'
			);
		}

		// Calculate default points for new contributors
		const existingContributors = nonRootNode.contributors || [];
		const avgPoints = existingContributors.length > 0
			? totalContributorPoints(existingContributors) / existingContributors.length
			: 100; // Default to 100 if no existing contributors

		// Build contributor objects with points
		const contributors: Contributor[] = contributorIds.map((id, index) => ({
			id,
			points: contributorPoints?.[index] ?? avgPoints
		}));

		// Build anti-contributor objects with points
		const antiContributors: Contributor[] = antiContributorIds.map((id, index) => ({
			id,
			points: antiContributorPoints?.[index] ?? avgPoints
		}));

		// Update contributors and anti-contributors, and clear children
		nonRootNode.contributors = contributors;
		nonRootNode.anti_contributors = antiContributors;
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

/**
 * Update contributor points (V5: weighted contributors)
 * 
 * @param node - Node containing the contributor
 * @param contributorId - ID of contributor to update
 * @param newPoints - New point value
 * @param isAntiContributor - Whether this is an anti-contributor (default: false)
 */
export function updateContributorPoints(
	node: Node,
	contributorId: string,
	newPoints: number,
	isAntiContributor: boolean = false
): boolean {
	if (node.type !== 'NonRootNode') return false;
	
	const nonRootNode = node as NonRootNode;
	const contributors = isAntiContributor 
		? (nonRootNode.anti_contributors || [])
		: (nonRootNode.contributors || []);
	
	const contributor = contributors.find(c => c.id === contributorId);
	if (!contributor) return false;
	
	contributor.points = Math.max(0, newPoints); // Ensure non-negative
	return true;
}

/**
 * Get contributor points (V5: weighted contributors)
 * 
 * @param node - Node containing the contributor
 * @param contributorId - ID of contributor
 * @param isAntiContributor - Whether this is an anti-contributor (default: false)
 * @returns Point value, or undefined if not found
 */
export function getContributorPoints(
	node: Node,
	contributorId: string,
	isAntiContributor: boolean = false
): number | undefined {
	if (node.type !== 'NonRootNode') return undefined;
	
	const nonRootNode = node as NonRootNode;
	const contributors = isAntiContributor 
		? (nonRootNode.anti_contributors || [])
		: (nonRootNode.contributors || []);
	
	const contributor = contributors.find(c => c.id === contributorId);
	return contributor?.points;
}

/**
 * Subtree Analysis Functions
 */

// Get a map of subtree names to their contributor lists (V5: weighted contributors)
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
				const contributors = (n as NonRootNode).contributors || [];
				contributors.forEach((contributor) => {
					// Resolve to public key if resolver is provided
					const resolvedId = resolveToPublicKey 
						? resolveToPublicKey(contributor.id) || contributor.id 
						: contributor.id;
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

// Get a map of subtree names to their anti-contributor lists (V5: weighted anti-contributors)
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
				const antiContributors = (n as NonRootNode).anti_contributors || [];
				antiContributors.forEach((antiContributor) => {
					const resolvedId = resolveToPublicKey 
						? resolveToPublicKey(antiContributor.id) || antiContributor.id 
						: antiContributor.id;
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

/**
 * Calculate appropriate points for a node based on its siblings (V5)
 * 
 * Default points strategy (consistent with contributor points):
 * - No siblings: 100 points (standard default)
 * - Has siblings: average points of existing siblings
 * 
 * This ensures:
 * - New nodes don't drastically shift sibling weights
 * - Intuitive defaults (similar contribution level as peers)
 * - Consistent with contributor point defaults
 * 
 * Example: If siblings have [50, 30, 20] points, new node gets 33.33 (average)
 */
export function calculateNodePoints(parentNode: Node): number {
	// No siblings - set to 100 points
	if (!parentNode.children || parentNode.children.length === 0) {
		return 100;
	}

	// Has siblings - use average of sibling points
	const siblingPoints = parentNode.children
		.filter((child: Node) => child.type === 'NonRootNode')
		.map((child: Node) => (child as NonRootNode).points);
	
	if (siblingPoints.length === 0) {
		return 100; // Safety: no valid siblings
	}
	
	const totalPoints = siblingPoints.reduce((sum: number, pts: number) => sum + pts, 0);
	const averagePoints = totalPoints / siblingPoints.length;
	
	return Math.max(1, Math.round(averagePoints)); // Round to nearest integer, min 1
}

// Re-export filter-related functions
export { filter, normalizeShareMap, applyCapacityFilter, Rules };

/**
 * CAPACITY ANALYSIS UTILITIES
 *
 * Functions for analyzing capacity allocations, slot quantities, and availability.
 * These utilities work with the efficient allocation algorithm data.
 */

/**
 * Get allocated quantity for a specific slot from the efficient algorithm
 */
export function getSlotAllocatedQuantity(capacity: any, slotId: string): number {
	const slot = capacity.availability_slots?.find((s: any) => s.id === slotId);
	return slot?.allocated_quantity || 0;
}

/**
 * Get available quantity for a specific slot
 */
export function getSlotAvailableQuantity(capacity: any, slotId: string): number {
	const slot = capacity.availability_slots?.find((s: any) => s.id === slotId);
	return slot?.available_quantity || slot?.quantity || 0;
}

/**
 * Get count of slots with allocated quantities
 */
export function getAllocatedSlotCount(capacity: any): number {
	if (!capacity.availability_slots || !Array.isArray(capacity.availability_slots)) {
		return 0;
	}
	return capacity.availability_slots.filter((slot: any) => (slot.allocated_quantity || 0) > 0)
		.length;
}

/**
 * Get total number of slots available
 */
export function getTotalSlotCount(capacity: any): number {
	return capacity.availability_slots?.length || 0;
}

/**
 * Calculate total allocated quantity across all slots
 */
export function getTotalAllocated(capacity: any): number {
	if (!capacity.availability_slots || !Array.isArray(capacity.availability_slots)) {
		return 0;
	}
	return capacity.availability_slots.reduce((total: number, slot: any) => {
		return total + (slot.allocated_quantity || 0);
	}, 0);
}

/**
 * Calculate total available quantity across all slots
 */
export function getTotalAvailable(capacity: any): number {
	if (!capacity.availability_slots || !Array.isArray(capacity.availability_slots)) {
		return 0;
	}
	return capacity.availability_slots.reduce((total: number, slot: any) => {
		return total + (slot.available_quantity || slot.quantity || 0);
	}, 0);
}

/**
 * SLOT UTILITY FUNCTIONS
 *
 * General utilities for working with availability slots.
 */

/**
 * Check if a slot is recurring (v5 - considers availability_window)
 */
export function isSlotRecurring(slot: any): boolean {
	// V5: Check for recurrence field or presence of availability_window
	if (slot.availability_window) {
		return true; // Availability windows are used for recurring patterns
	}
	return slot.recurrence && slot.recurrence !== 'Does not repeat' && slot.recurrence !== null;
}

/**
 * Get display string for slot recurrence
 */
export function getRecurrenceDisplay(slot: any): string {
	return slot.recurrence || 'Does not repeat';
}

/**
 * Check if a slot is in the past
 */
export function isSlotInPast(slot: any): boolean {
	if (!slot.start_date) return false;

	const now = new Date();
	const slotDate = new Date(slot.start_date);

	// If it's all day, compare dates only
	if (slot.all_day) {
		const today = new Date(now.getFullYear(), now.getMonth(), now.getDate());
		const slotDateOnly = new Date(slotDate.getFullYear(), slotDate.getMonth(), slotDate.getDate());
		return slotDateOnly < today;
	}

	// If it has time, create full datetime
	if (slot.start_time) {
		const [hours, minutes] = slot.start_time.split(':');
		slotDate.setHours(parseInt(hours), parseInt(minutes));
	}

	return slotDate < now;
}

/**
 * TIME AND DATE FORMATTING UTILITIES
 *
 * Functions for formatting time and date displays in slot information.
 */

/**
 * Safely extract time from potentially malformed time strings
 */
export function safeExtractTime(timeValue: string | null | undefined): string | undefined {
	if (!timeValue) return undefined;

	// If it's already in HH:MM format, return as-is
	if (/^\d{2}:\d{2}$/.test(timeValue)) {
		return timeValue;
	}

	// If it's an ISO datetime string, extract just the time part
	if (timeValue.includes('T')) {
		try {
			const date = new Date(timeValue);
			return date.toTimeString().substring(0, 5); // Get HH:MM from "HH:MM:SS GMT..."
		} catch (e) {
			console.warn('Failed to parse time:', timeValue);
			return undefined;
		}
	}

	// If it's some other format, try to extract time
	console.warn('Unknown time format:', timeValue);
	return undefined;
}

/**
 * Format time without leading zeros (08:30 → 8:30)
 */
export function formatTimeClean(timeStr: string): string {
	if (!timeStr) return timeStr;

	const [hours, minutes] = timeStr.split(':');
	const cleanHours = parseInt(hours).toString(); // Remove leading zero
	return `${cleanHours}:${minutes}`;
}

/**
 * Format date for display with smart labels
 */
export function formatDateForDisplay(date: Date): string {
	const today = new Date();
	const tomorrow = new Date(today);
	tomorrow.setDate(tomorrow.getDate() + 1);

	if (date.toDateString() === today.toDateString()) {
		return 'Today';
	} else if (date.toDateString() === tomorrow.toDateString()) {
		return 'Tomorrow';
	} else {
		return date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' });
	}
}

/**
 * Format slot time display - comprehensive time formatting for slots
 */
export function formatSlotTimeDisplay(slot: any): string {
	// Clean the time values
	const rawStartTime = safeExtractTime(slot.start_time);
	const rawEndTime = safeExtractTime(slot.end_time);

	// Format times without leading zeros
	const cleanStartTime = rawStartTime ? formatTimeClean(rawStartTime) : '';
	const cleanEndTime = rawEndTime ? formatTimeClean(rawEndTime) : '';

	// Get recurrence display
	const recurrenceDisplay =
		slot.recurrence && slot.recurrence !== 'Does not repeat' ? slot.recurrence : '';

	// Handle "All day" case first
	if (slot.all_day) {
		const startDate = slot.start_date ? new Date(slot.start_date) : null;
		const endDate = slot.end_date ? new Date(slot.end_date) : null;

		let timeStr = '';
		if (startDate && endDate && startDate.getTime() !== endDate.getTime()) {
			// Multi-day all-day event
			const startStr = formatDateForDisplay(startDate);
			const endStr = formatDateForDisplay(endDate);
			timeStr = `${startStr} - ${endStr}, All day`;
		} else if (startDate) {
			// Single day all-day event
			const dateStr = formatDateForDisplay(startDate);
			timeStr = `${dateStr}, All day`;
		} else {
			timeStr = 'All day';
		}

		// Add recurrence if present
		return recurrenceDisplay ? `${timeStr} (${recurrenceDisplay})` : timeStr;
	}

	// Handle timed slots
	const startDate = slot.start_date ? new Date(slot.start_date) : null;
	const endDate = slot.end_date ? new Date(slot.end_date) : null;

	let timeStr = '';
	if (startDate) {
		const startDateStr = formatDateForDisplay(startDate);

		// Check if we have an end date and it's different from start date
		if (endDate && startDate.getTime() !== endDate.getTime()) {
			// Multi-day timed event
			const endDateStr = formatDateForDisplay(endDate);
			const startTimeStr = cleanStartTime || '';
			const endTimeStr = cleanEndTime || '';

			if (startTimeStr && endTimeStr) {
				timeStr = `${startDateStr}, ${startTimeStr} - ${endDateStr}, ${endTimeStr}`;
			} else if (startTimeStr) {
				timeStr = `${startDateStr}, ${startTimeStr} - ${endDateStr}`;
			} else {
				timeStr = `${startDateStr} - ${endDateStr}`;
			}
		} else {
			// Single day or no end date
			if (cleanStartTime) {
				const timeRange = cleanEndTime ? `${cleanStartTime}-${cleanEndTime}` : cleanStartTime;
				timeStr = `${startDateStr}, ${timeRange}`;
			} else {
				timeStr = startDateStr;
			}
		}
	} else if (cleanStartTime) {
		// Just time, no date
		timeStr = cleanEndTime ? `${cleanStartTime}-${cleanEndTime}` : cleanStartTime;
	} else {
		timeStr = 'No time set';
	}

	// Add recurrence if present
	return recurrenceDisplay ? `${timeStr} (${recurrenceDisplay})` : timeStr;
}

/**
 * Format slot location display - show complete address
 */
export function formatSlotLocationDisplay(slot: any): string {
	if (slot.location_type === 'Specific') {
		// Build complete address from components
		const addressParts = [];

		if (slot.street_address) {
			addressParts.push(slot.street_address);
		}

		if (slot.city) {
			addressParts.push(slot.city);
		}

		if (slot.state_province) {
			addressParts.push(slot.state_province);
		}

		if (slot.postal_code) {
			addressParts.push(slot.postal_code);
		}

		if (slot.country) {
			addressParts.push(slot.country);
		}

		// If we have address components, join them with commas
		if (addressParts.length > 0) {
			return addressParts.join(', ');
		}

		// Fall back to coordinates if no address components
		if (slot.latitude && slot.longitude) {
			return `${slot.latitude.toFixed(4)}, ${slot.longitude.toFixed(4)}`;
		}
	} else if (slot.location_type === 'Online') {
		// Show online link or generic "Online" text
		if (slot.online_link) {
			// If it looks like a URL, show a shortened version
			if (slot.online_link.startsWith('http')) {
				try {
					const url = new URL(slot.online_link);
					return `Online (${url.hostname})`;
				} catch {
					return 'Online (link provided)';
				}
			}
			// If it's text, show truncated version
			return `Online (${slot.online_link.length > 30 ? slot.online_link.substring(0, 30) + '...' : slot.online_link})`;
		}
		return 'Online';
	}

	return slot.location_type || 'No location';
}

/**
 * Get slot sort value for different sort criteria
 */
export function getSlotSortValue(slot: any, sortBy: string): number | string {
	switch (sortBy) {
		case 'time':
			if (!slot.start_date) return '9999-12-31';
			return slot.start_date;
		case 'location':
			return formatSlotLocationDisplay(slot).toLowerCase();
		case 'quantity':
			return slot.allocated_quantity || 0;
		default:
			return 0;
	}
}

/**
 * ═══════════════════════════════════════════════════════════════════════
 * ADVANCED SCHEMA UTILITIES (v2)
 * 
 * Functions for working with the advanced ResourceMetadata-based schemas
 * from schema-v2.ts. These complement the tree-based functions
 * for slot-native two-tier allocation.
 * ═══════════════════════════════════════════════════════════════════════
 */

/**
 * Extract resource metadata from a slot (v5 - slot-native)
 * Works with AvailabilitySlot and NeedSlot types
 */
export function extractResourceMetadata(
	resource: AvailabilitySlot | NeedSlot | any
): ResourceMetadata {
	return {
		name: resource.name || '',
		emoji: resource.emoji,
		unit: resource.unit,
		description: resource.description,
		resource_type: resource.resource_type,
		filter_rule: resource.filter_rule,
		hidden_until_request_accepted: resource.hidden_until_request_accepted
	};
}

/**
 * Check if a slot has a filter rule (v5 - slot-native only)
 */
export function hasFilterRule(
	slot: AvailabilitySlot | NeedSlot
): boolean {
	return slot.filter_rule !== undefined && slot.filter_rule !== null;
}

/**
 * Get effective filter rule for a slot (v5 - slot-native only)
 */
export function getEffectiveFilterRule(
	slot: AvailabilitySlot | NeedSlot
): any | null {
	return slot.filter_rule !== undefined && slot.filter_rule !== null 
		? slot.filter_rule 
		: null;
}

/**
 * Check if two slots have compatible time constraints (v5)
 * Delegates to match.svelte.ts for hierarchical availability window support
 */
export function areSlotsTimeCompatible(
	availabilitySlot: AvailabilitySlot,
	needSlot: NeedSlot
): boolean {
	return timeRangesOverlap(availabilitySlot, needSlot);
}

/**
 * Check if two slots have compatible location constraints (v5)
 * Delegates to match.svelte.ts for comprehensive location matching
 */
export function areSlotsLocationCompatible(
	availabilitySlot: AvailabilitySlot,
	needSlot: NeedSlot
): boolean {
	return locationsCompatible(availabilitySlot, needSlot);
}

/**
 * Create a commitment from recognition shares and capacity/need declarations (v5)
 * Helper for building Commitment objects for the multi-dimensional allocation system
 * 
 * V5 uses GLOBAL recognition: same MR value for all need types
 * Type preferences are encoded in recognition tree structure, not in separate MR values
 */
export function createCommitment(
	globalRecognitionWeights: GlobalRecognitionWeights,
	capacitySlots?: AvailabilitySlot[],
	needSlots?: NeedSlot[],
	itcStamp?: ITCStamp
): Commitment {
	return {
		capacity_slots: capacitySlots,
		need_slots: needSlots,
		global_recognition_weights: globalRecognitionWeights,
		timestamp: Date.now(),
		itcStamp: itcStamp!  // Required in v5
	};
}

/**
 * Extract allocation records for a specific recipient from allocation state
 * Useful for analyzing what a recipient received across all slots
 */
export function getAllocationRecordsForRecipient(
	allocationState: TwoTierAllocationState,
	recipientPubkey: string
): SlotAllocationRecord[] {
	return allocationState.slot_allocations.filter(
		(record) => record.recipient_pubkey === recipientPubkey
	);
}

/**
 * Extract allocation records for a specific availability slot
 * Useful for analyzing how a slot was distributed
 */
export function getAllocationRecordsForSlot(
	allocationState: TwoTierAllocationState,
	slotId: string
): SlotAllocationRecord[] {
	return allocationState.slot_allocations.filter(
		(record) => record.availability_slot_id === slotId
	);
}

/**
 * Calculate total quantity allocated in mutual tier for a slot
 */
export function getMutualTierTotal(
	allocationState: TwoTierAllocationState,
	slotId: string
): number {
	return allocationState.slot_allocations
		.filter((record) => record.availability_slot_id === slotId && record.tier === 'mutual')
		.reduce((sum, record) => sum + record.quantity, 0);
}

/**
 * Calculate total quantity allocated in non-mutual tier for a slot
 */
export function getNonMutualTierTotal(
	allocationState: TwoTierAllocationState,
	slotId: string
): number {
	return allocationState.slot_allocations
		.filter((record) => record.availability_slot_id === slotId && record.tier === 'non-mutual')
		.reduce((sum, record) => sum + record.quantity, 0);
}

/**
 * Get mutual tier recipients for a slot
 */
export function getMutualTierRecipients(
	allocationState: TwoTierAllocationState,
	slotId: string
): string[] {
	const recipients = new Set<string>();
	allocationState.slot_allocations
		.filter((record) => record.availability_slot_id === slotId && record.tier === 'mutual')
		.forEach((record) => recipients.add(record.recipient_pubkey));
	return Array.from(recipients);
}

/**
 * Check if a recipient is in the mutual tier for a slot
 */
export function isRecipientMutual(
	allocationState: TwoTierAllocationState,
	slotId: string,
	recipientPubkey: string
): boolean {
	return allocationState.slot_allocations.some(
		(record) =>
			record.availability_slot_id === slotId &&
			record.recipient_pubkey === recipientPubkey &&
			record.tier === 'mutual'
	);
}

/**
 * Re-export schema types for convenience (v5 - global recognition model)
 */
export type {
	Node,
	RootNode,
	NonRootNode,
	NodeDataStorage,
	ShareMap,
	ResourceMetadata,
	AvailabilitySlot,
	NeedSlot,
	Commitment,
	TwoTierAllocationState,
	SlotAllocationRecord,
	ITCStamp,
	GlobalRecognitionWeights,
	NeedType,
	AvailabilityWindow,
	Contributor
};

