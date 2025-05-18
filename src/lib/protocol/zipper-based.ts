// NOTE: This is a centralized implementation of Free-Association protocol
// It serves as a reference implementation for decentralized/p2p/distributed implementations
// In this file, "Forest" is our centralized stand-in for looking up a peer's Tree over the network
// As each peer has their own tree (which only they can write to)

/**
 * Core Data Types
 */

// Enums
enum LocationType {
	Undefined = 'Undefined',
	LiveLocation = 'LiveLocation',
	Specific = 'Specific'
}

enum RecurrenceUnit {
	Days = 'Days',
	Weeks = 'Weeks',
	Months = 'Months',
	Years = 'Years'
}

// Union types for discriminated unions
type RecurrenceEnd =
	| { type: 'Never' }
	| { type: 'EndsOn'; date: Date }
	| { type: 'EndsAfter'; count: number };

// Node type represented as either a root or non-root node
interface BaseNode {
	id: string;
	name: string;
	children: Map<string, Node>;
	manualFulfillment?: number;
}

interface RootNode extends BaseNode {
	type: 'RootNode';
	capacities: CapacityInventory;
	capacityShares: CapacityShares;
	SOGFMap?: ShareMap;
	providerSharesMap: Map<number, ShareMap>;
}

interface NonRootNode extends BaseNode {
	type: 'NonRootNode';
	points: number;
	contributors: Set<string>;
}

type Node = RootNode | NonRootNode;

interface Ctx {
	ctxParent: Node;
	ctxSiblings: Map<string, Node>;
	ctxAncestors: Ctx[];
}

interface TreeZipper {
	zipperCurrent: Node;
	zipperContext?: Ctx;
}

type Forest = Map<string, TreeZipper>;
type NavigationPath = string[];
type ShareMap = Map<string, number>;
type VisitedSet = Set<string>;

// Capacity related interfaces
interface CustomRecurrence {
	repeatEvery: number;
	repeatUnit: RecurrenceUnit;
	recurrenceEnd: RecurrenceEnd;
}

interface SpaceTimeCoordinates {
	locationType: LocationType;
	allDay: boolean;
	recurrence?: string;
	customRecurrence?: CustomRecurrence;
	startDate?: Date;
	startTime?: Date;
	endDate?: Date;
	endTime?: Date;
	timeZone: string;
}

interface MaxDivisibility {
	naturalDiv: number;
	percentageDiv: number;
}

interface Capacity {
	capacityId: string;
	capacityName: string;
	quantity: number;
	unit: string;
	shareDepth: number;
	expanded: boolean;
	coordinates: SpaceTimeCoordinates;
	maxDivisibility: MaxDivisibility;
	hiddenUntilRequestAccepted: boolean;
}

interface CapacityShare {
	targetCapacity: Capacity;
	sharePercentage: number;
	computedQuantity: number; // Derived from percentage * capacity quantity, respecting maxDivisibility
}

type CapacityInventory = Map<string, Capacity>;
type CapacityShares = Map<string, CapacityShare>;

/**
 * Zipper Navigation
 */

// Core navigation
function enterChild(childId: string, zipper: TreeZipper): TreeZipper | undefined {
	const child = zipper.zipperCurrent.children.get(childId);
	if (!child) return undefined;

	const siblings = new Map(zipper.zipperCurrent.children);
	siblings.delete(childId);

	const newCtx: Ctx = {
		ctxParent: zipper.zipperCurrent,
		ctxSiblings: siblings,
		ctxAncestors: zipper.zipperContext
			? [zipper.zipperContext, ...zipper.zipperContext.ctxAncestors]
			: []
	};

	return {
		zipperCurrent: child,
		zipperContext: newCtx
	};
}

function exitToParent(zipper: TreeZipper): TreeZipper | undefined {
	if (!zipper.zipperContext) return undefined;

	const ctx = zipper.zipperContext;
	const newChildren = new Map(ctx.ctxSiblings);
	newChildren.set(zipper.zipperCurrent.id, zipper.zipperCurrent);

	// Create a new parent with the updated children
	const newParent = { ...ctx.ctxParent, children: newChildren };

	// Handle ancestors
	const newContext = ctx.ctxAncestors.length > 0 ? ctx.ctxAncestors[0] : undefined;

	return {
		zipperCurrent: newParent,
		zipperContext: newContext
	};
}

// Enhanced navigation functions
function enterSibling(name: string, zipper: TreeZipper): TreeZipper | undefined {
	const parent = exitToParent(zipper);
	return parent ? enterChild(name, parent) : undefined;
}

function goToRoot(zipper: TreeZipper): TreeZipper {
	const parent = exitToParent(zipper);
	if (!parent) return zipper; // Already at root
	return goToRoot(parent);
}

// Modify the current node safely
function modifyNode(f: (node: Node) => Node, zipper: TreeZipper): TreeZipper {
	return { ...zipper, zipperCurrent: f(zipper.zipperCurrent) };
}

// Get all siblings of the current node
function getSiblings(zipper: TreeZipper): string[] {
	const parent = exitToParent(zipper);
	if (!parent) return [];
	return Array.from(parent.zipperCurrent.children.keys());
}

// Safe navigation with breadcrumbs
function followPath(path: NavigationPath, zipper: TreeZipper): TreeZipper | undefined {
	if (path.length === 0) return zipper;

	const [first, ...rest] = path;
	const nextZipper = enterChild(first, zipper);
	if (!nextZipper) return undefined;

	return followPath(rest, nextZipper);
}

// Get the current path from root
function getCurrentPath(zipper: TreeZipper): NavigationPath {
	const getPath = (z: TreeZipper, acc: string[]): string[] => {
		const parent = exitToParent(z);
		if (!parent) return acc;
		return getPath(parent, [z.zipperCurrent.id, ...acc]);
	};

	return getPath(zipper, []);
}

// Enhanced tree traversal helpers
function children(zipper: TreeZipper): TreeZipper[] {
	return Array.from(zipper.zipperCurrent.children.keys())
		.map((cid) => enterChild(cid, zipper))
		.filter((z): z is TreeZipper => z !== undefined);
}

function foldChildren<T>(f: (acc: T, child: TreeZipper) => T, initial: T, zipper: TreeZipper): T {
	return children(zipper).reduce(f, initial);
}

function mapChildren<T>(f: (child: TreeZipper) => T, zipper: TreeZipper): T[] {
	return children(zipper).map(f);
}

function anyChild(predicate: (child: TreeZipper) => boolean, zipper: TreeZipper): boolean {
	return children(zipper).some(predicate);
}

function allChildren(predicate: (child: TreeZipper) => boolean, zipper: TreeZipper): boolean {
	return children(zipper).every(predicate);
}

function descendants(zipper: TreeZipper): TreeZipper[] {
	return getAllDescendants(zipper).slice(1);
}

function getAllDescendants(zipper: TreeZipper): TreeZipper[] {
	return [zipper, ...children(zipper).flatMap(getAllDescendants)];
}

/**
 * Tree Modification API
 */
function addToForest(forest: Forest, zipper: TreeZipper): Forest {
	const newForest = new Map(forest);
	newForest.set(zipper.zipperCurrent.id, zipper);
	return newForest;
}

function createRootNode(id: string, name: string, manual?: number): RootNode {
	return {
		type: 'RootNode',
		id: id,
		name: name,
		children: new Map(),
		manualFulfillment: manual !== undefined ? Math.max(0, Math.min(1, manual)) : undefined,
		capacities: new Map(),
		capacityShares: new Map(),
		SOGFMap: undefined,
		providerSharesMap: new Map()
	};
}

function createNonRootNode(
	id: string,
	name: string,
	pts: number,
	contribs: string[],
	manual?: number
): NonRootNode {
	return {
		type: 'NonRootNode',
		id: id,
		name: name,
		points: pts,
		children: new Map(),
		contributors: new Set(contribs),
		manualFulfillment: manual !== undefined ? Math.max(0, Math.min(1, manual)) : undefined
	};
}

// Add a child and update caches
function addChild(
	name: string,
	pts: number,
	contribs: string[],
	manual: number | undefined,
	zipper: TreeZipper
): TreeZipper | undefined {
	const current = zipper.zipperCurrent;

	// Don't allow adding children to nodes with contributors
	if (current.type === 'NonRootNode' && current.contributors.size > 0) {
		return undefined;
	}

	const newChild = createNonRootNode(name, name, pts, contribs, manual);

	// Create a new node with the updated children
	let updatedCurrent: Node;

	if (current.type === 'RootNode') {
		const newChildren = new Map(current.children);
		newChildren.set(name, newChild);

		updatedCurrent = {
			...current,
			children: newChildren,
			SOGFMap: undefined,
			providerSharesMap: new Map()
		};
	} else {
		const newChildren = new Map(current.children);
		newChildren.set(name, newChild);

		updatedCurrent = {
			...current,
			children: newChildren
		};
	}

	return {
		...zipper,
		zipperCurrent: updatedCurrent
	};
}

// Add contributors to a node and recursively delete its subtree
function addContributors(contribs: string[], zipper: TreeZipper): TreeZipper {
	return modifyNode((node) => {
		if (node.type === 'RootNode') {
			return {
				...node,
				children: new Map(),
				SOGFMap: undefined,
				providerSharesMap: new Map()
			};
		} else {
			return {
				...node,
				contributors: new Set(contribs),
				children: new Map()
			};
		}
	}, zipper);
}

// Helper to recursively delete a subtree
function deleteSubtree(zipper: TreeZipper): TreeZipper {
	return modifyNode((node) => {
		if (node.type === 'RootNode') {
			return {
				...node,
				children: new Map(),
				SOGFMap: undefined,
				providerSharesMap: new Map()
			};
		} else {
			return {
				...node,
				children: new Map()
			};
		}
	}, zipper);
}

/**
 * Core Calculations
 */

// Calculate total points from all children
function totalChildPoints(zipper: TreeZipper): number {
	return children(zipper)
		.filter((child) => child.zipperCurrent.type === 'NonRootNode')
		.reduce((sum, child) => sum + (child.zipperCurrent as NonRootNode).points, 0);
}

// Calculate a node's weight with caching
function weight(zipper: TreeZipper): number {
	if (!zipper.zipperContext) {
		// Root node always has weight 1.0
		return 1.0;
	}

	// Non-root nodes recursive weight calculation
	const parent = exitToParent(zipper);
	if (!parent) {
		// Guard against missing parent (shouldn't happen with valid zippers)
		return 1.0;
	}

	const current = zipper.zipperCurrent;
	if (current.type !== 'NonRootNode') {
		return 1.0; // Root nodes always have weight 1.0 (should not happen here)
	}

	// Get parent's total points
	const total = totalChildPoints(parent);

	// Calculate this node's contribution to parent (safely handling division by zero)
	const nodeContribution = total === 0 ? 0 : current.points / total;

	// Weight is recursive - multiply by parent's weight
	const parentWeight = weight(parent);

	return nodeContribution * parentWeight;
}

// Calculate a node's share of its parent
function shareOfParent(zipper: TreeZipper): number {
	const parent = exitToParent(zipper);

	// Root node has 100% share
	if (!parent) return 1.0;

	// Calculate share for non-root nodes
	const parentTotal = totalChildPoints(parent);

	if (zipper.zipperCurrent.type !== 'NonRootNode') {
		return 0; // Should never happen for valid tree structure
	}

	const currentPoints = zipper.zipperCurrent.points;

	return parentTotal === 0 ? 0 : currentPoints / parentTotal;
}

// Check if a node is a contribution node (has contributors and is not root)
function isContribution(zipper: TreeZipper): boolean {
	const current = zipper.zipperCurrent;

	if (current.type === 'RootNode') {
		return false; // Root nodes are never contribution nodes
	}

	return current.contributors.size > 0 && zipper.zipperContext !== undefined;
}

// Check if a node has direct contribution children
function hasDirectContributionChild(zipper: TreeZipper): boolean {
	return anyChild(isContribution, zipper);
}

// Check if a node has non-contribution children
function hasNonContributionChild(zipper: TreeZipper): boolean {
	return !allChildren(isContribution, zipper);
}

// Calculate the proportion of total child points from contribution children
function contributionChildrenWeight(zipper: TreeZipper): number {
	// Get all child zippers
	const childZippers = children(zipper);

	// Partition children into contribution and non-contribution nodes
	const contribChildren = childZippers.filter(isContribution);

	// Calculate weights for each category
	const contribWeights = contribChildren.map(weight);
	const allWeights = childZippers.map(weight);

	// Sum the weights (safely handle empty lists)
	const contribWeightSum = contribWeights.reduce((sum, w) => sum + w, 0);
	const totalWeightSum = allWeights.reduce((sum, w) => sum + w, 0);

	// Handle division by zero
	return totalWeightSum === 0 ? 0 : contribWeightSum / totalWeightSum;
}

// Sum fulfillment from children matching a predicate
function childrenFulfillment(pred: (child: TreeZipper) => boolean, zipper: TreeZipper): number {
	return children(zipper)
		.filter(pred)
		.reduce((sum, child) => sum + fulfilled(child) * shareOfParent(child), 0);
}

// Calculate the fulfillment from contribution children
function contributionChildrenFulfillment(zipper: TreeZipper): number {
	return childrenFulfillment(isContribution, zipper);
}

// Calculate the fulfillment from non-contribution children
function nonContributionChildrenFulfillment(zipper: TreeZipper): number {
	// Get all child zippers
	const childZippers = children(zipper);

	// Filter to non-contribution children only
	const nonContribChildren = childZippers.filter((child) => !isContribution(child));

	if (nonContribChildren.length === 0) return 0;

	// Get weight and fulfillment for each child
	const childWeights = nonContribChildren.map(weight);
	const childFulfillments = nonContribChildren.map(fulfilled);

	// Calculate weighted sum and total weight
	const weightedSum = childWeights.reduce((sum, w, i) => sum + w * childFulfillments[i], 0);
	const totalWeight = childWeights.reduce((sum, w) => sum + w, 0);

	// Return weighted average
	return totalWeight === 0 ? 0 : weightedSum / totalWeight;
}

// Safely get instances of a contributor
function getContributorInstances(contribIndex: Forest, contribId: string): Set<TreeZipper> {
	const contributor = contribIndex.get(contribId);
	return contributor ? new Set([contributor]) : new Set();
}

// Safely get a contributor node
function getContributorNode(contribIndex: Forest, contribId: string): TreeZipper | undefined {
	return contribIndex.get(contribId);
}

// Calculate fulfillment with caching
function fulfilled(zipper: TreeZipper): number {
	const current = zipper.zipperCurrent;

	// Helper predicates and values
	const hasManualFulfillment = current.manualFulfillment !== undefined;
	const isLeafNode = current.children.size === 0;
	const isContribNode = isContribution(zipper);
	const hasContribChildren = hasDirectContributionChild(zipper);
	const hasNonContribChildren = hasNonContributionChild(zipper);

	// Safely extract manual fulfillment value with a default
	const getManualValue = current.manualFulfillment ?? 0.0;

	// Calculate the manual contribution share
	const manualContribShare = (): number => {
		const contribWeight = contributionChildrenWeight(zipper);
		const nonContribFulfillment = nonContributionChildrenFulfillment(zipper);
		return getManualValue * contribWeight + nonContribFulfillment * (1.0 - contribWeight);
	};

	// Main fulfillment calculation with pattern matching

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

	// Non-leaf node with mixed children types and manual fulfillment
	if (hasManualFulfillment && hasContribChildren) {
		return manualContribShare();
	}

	// Other nodes (aggregate from children)
	return children(zipper).reduce((sum, child) => sum + fulfilled(child) * shareOfParent(child), 0);
}

// Calculate the desire (unfulfilled need) of a node
function desire(zipper: TreeZipper): number {
	return 1.0 - fulfilled(zipper);
}

/**
 * Mutual Fulfillment
 */

// Calculate a node's share of general fulfillment to another node
function shareOfGeneralFulfillment(
	ci: Forest,
	target: TreeZipper,
	contributor: TreeZipper
): number {
	const contributorId = contributor.zipperCurrent.id;
	const rootContributor = ci.get(contributorId);

	if (!rootContributor) return 0;

	const contribId = rootContributor.zipperCurrent.id;

	// Find only nodes where this contributor is listed
	const contributingNodes = [target, ...descendants(target)].filter((node) => {
		const current = node.zipperCurrent;
		if (current.type === 'RootNode') return false; // Root nodes can't have contributors
		return current.contributors.has(contribId) && isContribution(node);
	});

	// Calculate weighted contribution for each node
	const weightedContributions = contributingNodes.map((node) => {
		const nodeWeight = weight(node);
		const nodeFulfillment = fulfilled(node);

		// Get contributor count safely
		const contributorCount =
			node.zipperCurrent.type === 'RootNode'
				? 1 // Should never happen but default to 1
				: node.zipperCurrent.contributors.size;

		return (nodeWeight * nodeFulfillment) / contributorCount;
	});

	// Sum the weighted contributions
	return weightedContributions.reduce((sum, w) => sum + w, 0);
}

// Get normalized shares of general fulfillment map
function sharesOfGeneralFulfillmentMap(ci: Forest, zipper: TreeZipper): ShareMap {
	// For non-root nodes, go to the root and calculate from there
	if (zipper.zipperCurrent.type === 'NonRootNode') {
		return sharesOfGeneralFulfillmentMap(ci, goToRoot(zipper));
	}

	// Root nodes can have cached share maps
	const current = zipper.zipperCurrent as RootNode;
	if (current.SOGFMap) {
		return current.SOGFMap;
	}

	// Calculate all contributor shares
	const contributorShares = Array.from(ci.entries())
		.map(([id, contributor]) => {
			const share = shareOfGeneralFulfillment(ci, zipper, contributor);
			return [id, share] as [string, number];
		})
		.filter(([_, share]) => share > 0);

	// Create and normalize the raw share map
	const rawShareMap = new Map(contributorShares);
	const freshMap = normalizeShareMap(rawShareMap);

	// Update the node with the new value (mutating the original object)
	current.SOGFMap = freshMap;

	return freshMap;
}

// Calculate mutual fulfillment between two nodes
function mutualFulfillment(ci: Forest, a: TreeZipper, b: TreeZipper): number {
	// Get node IDs
	const aId = a.zipperCurrent.id;
	const bId = b.zipperCurrent.id;

	// Get share maps
	const sharesFromA = sharesOfGeneralFulfillmentMap(ci, a);
	const sharesFromB = sharesOfGeneralFulfillmentMap(ci, b);

	// Extract shares with safe lookup (defaults to 0)
	const shareFromAToB = sharesFromA.get(bId) ?? 0;
	const shareFromBToA = sharesFromB.get(aId) ?? 0;

	// Mutual fulfillment is the minimum of shares each gives to the other
	return Math.min(shareFromAToB, shareFromBToA);
}

// Find the path to the highest mutual fulfillment node
function findHighestMutualPath(
	ci: Forest,
	root: TreeZipper,
	target: TreeZipper
): NavigationPath | undefined {
	const allPaths = [root, ...descendants(root)].map(getCurrentPath);

	// Calculate score for each path
	const pathScores = allPaths.map((path) => {
		const node = followPath(path, root);
		const score = node ? mutualFulfillment(ci, node, target) : 0;
		return [path, score] as [NavigationPath, number];
	});

	// Find the path with the maximum score
	if (pathScores.length === 0) return undefined;

	let maxPath = pathScores[0][0];
	let maxScore = pathScores[0][1];

	for (let i = 1; i < pathScores.length; i++) {
		if (pathScores[i][1] > maxScore) {
			maxPath = pathScores[i][0];
			maxScore = pathScores[i][1];
		}
	}

	return maxPath;
}

// Get all nodes with mutual fulfillment above a threshold
function getHighMutualNodes(ci: Forest, z: TreeZipper, threshold: number): TreeZipper[] {
	return [z, ...descendants(z)].filter((node) => mutualFulfillment(ci, z, node) > threshold);
}

// Calculate mutual fulfillment between all pairs in a forest
function calculateForestMutualFulfillment(ci: Forest, forest: Forest): [string, string, number][] {
	const result: [string, string, number][] = [];

	const forestEntries = Array.from(forest.entries());

	for (let i = 0; i < forestEntries.length; i++) {
		const [id1, z1] = forestEntries[i];

		for (let j = i + 1; j < forestEntries.length; j++) {
			const [id2, z2] = forestEntries[j];

			// Only calculate each pair once (id1 < id2)
			if (id1 < id2) {
				const mf = mutualFulfillment(ci, z1, z2);
				result.push([id1, id2, mf]);
			}
		}
	}

	return result;
}

/**
 * Share Map Utilities
 */

// Generic function to normalize a map so that its values sum to 1
function normalizeMap<K>(map: Map<K, number>): Map<K, number> {
	const total = Array.from(map.values()).reduce((sum, v) => sum + v, 0);

	if (total === 0) return map;

	const normalizedMap = new Map<K, number>();
	for (const [key, value] of map.entries()) {
		normalizedMap.set(key, value / total);
	}

	return normalizedMap;
}

// Type-specific version of normalizeMap for ShareMap
function normalizeShareMap(map: ShareMap): ShareMap {
	return normalizeMap(map);
}

// Calculate and update provider-centric shares
function providerShares(ci: Forest, provider: TreeZipper, depth: number): ShareMap {
	// For non-root nodes, go to the root and calculate from there
	if (provider.zipperCurrent.type === 'NonRootNode') {
		return providerShares(ci, goToRoot(provider), depth);
	}

	const current = provider.zipperCurrent as RootNode;

	// Use stored map if available
	const cachedMap = current.providerSharesMap.get(depth);
	if (cachedMap) {
		return cachedMap;
	}

	// Calculate fresh map
	let freshMap: ShareMap;

	if (depth === 1) {
		// Direct contributor shares based on mutual fulfillment
		freshMap = directContributorShares();
	} else {
		// Transitive shares for depth > 1
		freshMap = transitiveShares(depth);
	}

	// Normalize and cache the result
	const normalizedMap = normalizeShareMap(freshMap);
	current.providerSharesMap.set(depth, normalizedMap);

	return normalizedMap;

	// Helper function to calculate direct contributor shares
	function directContributorShares(): ShareMap {
		// Find all contributors in the entire tree
		const allContributingNodes = [provider, ...descendants(provider)];

		// Get all contributor IDs from all non-root nodes in the tree
		const allContributorIds = new Set<string>();
		for (const node of allContributingNodes) {
			if (node.zipperCurrent.type === 'NonRootNode') {
				for (const cid of node.zipperCurrent.contributors) {
					allContributorIds.add(cid);
				}
			}
		}

		// Get valid contributors that exist in the forest
		const validContributors: TreeZipper[] = [];
		for (const cid of allContributorIds) {
			const contributor = ci.get(cid);
			if (contributor) {
				validContributors.push(contributor);
			}
		}

		// Calculate mutual fulfillment for each contributor
		const contributorMutualFulfillments = validContributors
			.map((c) => {
				const mf = mutualFulfillment(ci, provider, c);
				return [c.zipperCurrent.id, mf] as [string, number];
			})
			.filter(([_, mf]) => mf > 0);

		return new Map(contributorMutualFulfillments);
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
		const resultMap = new Map<string, number>();

		for (const [recipientId, share] of currentShares.entries()) {
			const recipientZ = ci.get(recipientId);
			if (!recipientZ) continue;

			const recipientDirectShares = providerShares(ci, recipientZ, 1);

			for (const [subRecipientId, subShare] of recipientDirectShares.entries()) {
				const weightedShare = share * subShare;
				const currentShare = resultMap.get(subRecipientId) ?? 0;
				resultMap.set(subRecipientId, currentShare + weightedShare);
			}
		}

		return resultMap;
	}
}

/**
 * Capacity Utilities
 */

// Simplified interface functions that use providerShares
function directShare(ci: Forest, provider: TreeZipper, recipientId: string): number {
	return providerShares(ci, provider, 1).get(recipientId) ?? 0;
}

// Get a receiver's share from a specific capacity provider
function receiverShareFrom(
	ci: Forest,
	receiver: TreeZipper,
	provider: TreeZipper,
	capacity: Capacity,
	maxDepth: number
): number {
	// Get shares from the provider's root node
	const providerRoot = getRoot(provider);
	const providerShareMap = providerShares(ci, providerRoot, maxDepth);
	const receiverId = receiver.zipperCurrent.id;

	return providerShareMap.get(receiverId) ?? 0;
}

// Get a person's total share in a specific capacity
function getPersonalCapacityShare(ci: Forest, person: TreeZipper, capacity: Capacity): number {
	// Find all owners of this capacity
	const capacityOwners = Array.from(ci.values()).filter((owner) => {
		const rootNode = getRoot(owner).zipperCurrent;
		if (rootNode.type !== 'RootNode') return false;
		return rootNode.capacities.has(capacity.capacityId);
	});

	// Calculate direct shares from each owner
	const directShares = capacityOwners.map((owner) =>
		receiverShareFrom(ci, person, owner, capacity, 2)
	);

	// Return the maximum share (or 0 if no shares)
	return directShares.length === 0 ? 0 : Math.max(...directShares);
}

// Compute quantity share based on capacity and percentage
function computeQuantityShare(cap: Capacity, percentage: number): number {
	const rawQuantity = Math.round(cap.quantity * percentage);
	const maxNatural = cap.maxDivisibility.naturalDiv;
	const maxPercent = cap.maxDivisibility.percentageDiv;

	// Apply percentage divisibility constraint
	const percentConstrained =
		percentage > maxPercent ? Math.round(cap.quantity * maxPercent) : rawQuantity;

	// Apply natural number divisibility constraint
	const naturalConstrained = Math.floor(percentConstrained / maxNatural) * maxNatural;

	return naturalConstrained;
}

// Create a new capacity share
function createCapacityShare(cap: Capacity, percentage: number): CapacityShare {
	return {
		targetCapacity: cap,
		sharePercentage: percentage,
		computedQuantity: computeQuantityShare(cap, percentage)
	};
}

// Add a capacity to a node's inventory (only works on root nodes)
function addCapacity(cap: Capacity, zipper: TreeZipper): TreeZipper {
	if (zipper.zipperCurrent.type !== 'RootNode') {
		// For non-root nodes, this operation is not allowed
		return zipper;
	}

	// Create a new node with the updated capacities
	const current = zipper.zipperCurrent;
	const newCapacities = new Map(current.capacities);
	newCapacities.set(cap.capacityId, cap);

	return {
		...zipper,
		zipperCurrent: {
			...current,
			capacities: newCapacities
		}
	};
}

// Add a share in another node's capacity (only works on root nodes)
function addCapacityShare(shareId: string, share: CapacityShare, zipper: TreeZipper): TreeZipper {
	if (zipper.zipperCurrent.type !== 'RootNode') {
		// For non-root nodes, this operation is not allowed
		return zipper;
	}

	// Create a new node with the updated capacity shares
	const current = zipper.zipperCurrent;
	const newShares = new Map(current.capacityShares);
	newShares.set(shareId, share);

	return {
		...zipper,
		zipperCurrent: {
			...current,
			capacityShares: newShares
		}
	};
}

// Update computed quantities for all capacity shares in a node
function updateComputedQuantities(zipper: TreeZipper): TreeZipper {
	if (zipper.zipperCurrent.type !== 'RootNode') {
		// Non-root nodes don't have capacity shares
		return zipper;
	}

	const current = zipper.zipperCurrent;
	const updatedShares = new Map<string, CapacityShare>();

	for (const [key, share] of current.capacityShares.entries()) {
		updatedShares.set(key, {
			...share,
			computedQuantity: computeQuantityShare(share.targetCapacity, share.sharePercentage)
		});
	}

	return {
		...zipper,
		zipperCurrent: {
			...current,
			capacityShares: updatedShares
		}
	};
}

// Helper functions to access root-level data from any node

// Get the root zipper for any node
function getRoot(zipper: TreeZipper): TreeZipper {
	return goToRoot(zipper);
}

// Get capacity inventory for any node (by accessing root)
function getNodeCapacities(zipper: TreeZipper): CapacityInventory {
	const root = getRoot(zipper).zipperCurrent;
	return root.type === 'RootNode' ? root.capacities : new Map();
}

// Get capacity shares for any node (by accessing root)
function getNodeCapacityShares(zipper: TreeZipper): CapacityShares {
	const root = getRoot(zipper).zipperCurrent;
	return root.type === 'RootNode' ? root.capacityShares : new Map();
}

// Get SOGF map for any node (by accessing root)
function getNodeSOGFMap(zipper: TreeZipper): ShareMap | undefined {
	const root = getRoot(zipper).zipperCurrent;
	return root.type === 'RootNode' ? root.SOGFMap : undefined;
}

// Get provider shares map for any node (by accessing root)
function getNodeProviderSharesMap(zipper: TreeZipper): Map<number, ShareMap> {
	const root = getRoot(zipper).zipperCurrent;
	return root.type === 'RootNode' ? root.providerSharesMap : new Map();
}

/**
 * Example Usage
 */
function exampleForest(): [Forest, Forest] {
	// Create example capacities with safe date handling
	const roomCapacity: Capacity = {
		capacityId: 'room1',
		capacityName: 'Spare Room',
		quantity: 10,
		unit: 'room',
		shareDepth: 2,
		expanded: true,
		coordinates: {
			locationType: LocationType.Specific,
			allDay: true,
			timeZone: 'UTC'
		},
		maxDivisibility: {
			naturalDiv: 1,
			percentageDiv: 0.1 // Maximum 10% share
		},
		hiddenUntilRequestAccepted: false
	};

	const pieCapacity: Capacity = {
		capacityId: 'pie1',
		capacityName: 'Apple Pie',
		quantity: 8,
		unit: 'slices',
		shareDepth: 3,
		expanded: true,
		coordinates: {
			locationType: LocationType.Specific,
			allDay: true,
			timeZone: 'UTC'
		},
		maxDivisibility: {
			naturalDiv: 1, // Can't split a slice
			percentageDiv: 0.125 // Minimum share is one slice (1/8)
		},
		hiddenUntilRequestAccepted: false
	};

	// Create roots with mutual contributors (must be RootNodes to store capacities)
	const aliceNode = createRootNode('alice', 'Alice');
	const bobNode = createRootNode('bob', 'Bob');
	const charlieNode = createRootNode('charlie', 'Charlie');

	const aliceRoot: TreeZipper = { zipperCurrent: aliceNode };
	const bobRoot: TreeZipper = { zipperCurrent: bobNode };
	const charlieRoot: TreeZipper = { zipperCurrent: charlieNode };

	// Add capacities to roots
	const aliceWithCapacity = addCapacity(roomCapacity, aliceRoot);
	const bobWithCapacity = addCapacity(pieCapacity, bobRoot);

	// Create capacity shares
	const aliceRoomShare = createCapacityShare(roomCapacity, 0.5); // 50% share of room
	const bobPieShare = createCapacityShare(pieCapacity, 0.25); // 25% share of pie (2 slices)

	// Build trees with children and add capacity shares
	const aliceWithChild = addChild(
		'alice_child',
		30,
		['bob', 'charlie'],
		undefined,
		aliceWithCapacity
	);
	if (!aliceWithChild) throw new Error('Failed to add child to Alice');

	const bobWithChild = addChild('bob_child', 40, ['alice', 'charlie'], undefined, bobWithCapacity);
	if (!bobWithChild) throw new Error('Failed to add child to Bob');

	const bobWithShare = addCapacityShare('alice_room', aliceRoomShare, bobWithChild);

	const charlieWithChild = addChild('charlie_child', 50, ['alice', 'bob'], undefined, charlieRoot);
	if (!charlieWithChild) throw new Error('Failed to add child to Charlie');

	// Add children to the child nodes (second level of the tree)
	let aliceWithGrandchild = aliceWithChild;
	const aliceChildZ = enterChild('alice_child', aliceWithChild);
	if (aliceChildZ) {
		const aliceGrandchildZ = addChild('alice_grandchild', 15, ['bob'], undefined, aliceChildZ);
		if (aliceGrandchildZ) {
			const parentZ = exitToParent(aliceGrandchildZ);
			if (parentZ) aliceWithGrandchild = parentZ;
		}
	}

	let bobWithGrandchild = bobWithShare;
	const bobChildZ = enterChild('bob_child', bobWithShare);
	if (bobChildZ) {
		const bobGrandchildZ = addChild('bob_grandchild', 20, ['alice'], undefined, bobChildZ);
		if (bobGrandchildZ) {
			const parentZ = exitToParent(bobGrandchildZ);
			if (parentZ) bobWithGrandchild = parentZ;
		}
	}

	let charlieWithGrandchild = charlieWithChild;
	const charlieChildZ = enterChild('charlie_child', charlieWithChild);
	if (charlieChildZ) {
		const charlieGrandchildZ = addChild(
			'charlie_grandchild',
			25,
			['alice', 'bob'],
			undefined,
			charlieChildZ
		);
		if (charlieGrandchildZ) {
			const parentZ = exitToParent(charlieGrandchildZ);
			if (parentZ) charlieWithGrandchild = parentZ;
		}
	}

	const charlieWithShare = addCapacityShare('bob_pie', bobPieShare, charlieWithGrandchild);

	// Create forest and contributor index
	let forest: Forest = new Map();
	forest = addToForest(forest, aliceWithGrandchild);
	forest = addToForest(forest, bobWithGrandchild);
	forest = addToForest(forest, charlieWithShare);

	const ci: Forest = new Map([
		['alice', aliceWithGrandchild],
		['bob', bobWithGrandchild],
		['charlie', charlieWithShare]
	]);

	return [forest, ci];
}

// Example main function
function main() {
	const [forest, ci] = exampleForest();

	const alice = forest.get('alice');
	const bob = forest.get('bob');
	const charlie = forest.get('charlie');

	if (!alice || !bob || !charlie) {
		console.error('Failed to get nodes from forest');
		return;
	}

	// Print fulfillment values
	console.log('Mutual Fulfillment Values:');
	console.log(`Alice <-> Bob: ${mutualFulfillment(ci, alice, bob)}`);
	console.log(`Alice <-> Charlie: ${mutualFulfillment(ci, alice, charlie)}`);
	console.log(`Bob <-> Charlie: ${mutualFulfillment(ci, bob, charlie)}`);

	// Print capacity information
	console.log('\nCapacity Information:');

	// Print Alice's room capacity
	console.log("Alice's Room Capacity:");
	const aliceCapacities = getNodeCapacities(alice);
	const aliceRoom = aliceCapacities.get('room1');

	if (aliceRoom) {
		console.log(`  Quantity: ${aliceRoom.quantity} ${aliceRoom.unit}`);

		// Show provider-centric shares (Alice's perspective)
		console.log('\nProvider-centric shares (Alice distributing to network):');
		const aliceShares = providerShares(ci, alice, 2);
		const totalShares = Array.from(aliceShares.values()).reduce((sum, v) => sum + v, 0);
		const bobShare = (aliceShares.get('bob') ?? 0) * 100;
		const charlieShare = (aliceShares.get('charlie') ?? 0) * 100;
		const otherShares = 100 - bobShare - charlieShare;

		console.log(`  Total shares distributed: ${totalShares * 100}%`);
		console.log(`  Bob's share from Alice: ${bobShare}%`);
		console.log(`  Charlie's share from Alice: ${charlieShare}%`);
		console.log(`  Remaining shares (distributed to other network members): ${otherShares}%`);

		// Break down where the remaining shares went (if any)
		const remainingRecipients = Array.from(aliceShares.entries()).filter(
			([id, _]) => id !== 'bob' && id !== 'charlie'
		);

		if (remainingRecipients.length > 0) {
			console.log('  Other recipients:');
			for (const [id, share] of remainingRecipients) {
				console.log(`    ${id}: ${share * 100}%`);
			}
		}

		// Show receiver-centric shares
		console.log('\nReceiver perspective (shares received from Alice):');
		console.log(
			`  Bob's share of Alice's room: ${receiverShareFrom(ci, bob, alice, aliceRoom, 2) * 100}%`
		);
		console.log(
			`  Charlie's share of Alice's room: ${receiverShareFrom(ci, charlie, alice, aliceRoom, 2) * 100}%`
		);

		// Show computed quantities
		console.log('\nComputed quantities for capacity shares:');
		const bobRoomQty = Math.round(
			aliceRoom.quantity * receiverShareFrom(ci, bob, alice, aliceRoom, 2)
		);
		const charlieRoomQty = Math.round(
			aliceRoom.quantity * receiverShareFrom(ci, charlie, alice, aliceRoom, 2)
		);
		console.log(`  Bob's portion: ${bobRoomQty} ${aliceRoom.unit}`);
		console.log(`  Charlie's portion: ${charlieRoomQty} ${aliceRoom.unit}`);
	} else {
		console.log('  No room capacity found');
	}
}
/*
interface Collective {
	type: 'Collective';
	id: string;
	members: Array<Node | Collective>;
	weights: Map<string, number>; // Maps member IDs to their weight in the collective
}

type Entity = Node | Collective | RootNode;

// Enhanced with Memoization & Caching

type EntityID = string;

// Helper function to check if entity is a collective
function isCollective(entity: Entity): entity is Collective {
	return (entity as Collective).type === 'Collective';
}

// Global cache stores
const mfCache = new Map<EntityID, Map<EntityID, number>>();
const weightCache = new Map<EntityID, Map<EntityID, number>>();

// Cache key generator
function getCacheKey(a: EntityID, b: EntityID): string {
	return `${a}<=>${b}`;
}

// Enhanced mutual fulfillment with caching
function mutualFulfillmentC(ci: Forest, a: Entity, b: Entity): number {
	const key = getCacheKey(a.id, b.id);

	// Check cache
	const cached = mfCache.get(a.id)?.get(b.id);
	if (cached !== undefined) return cached;

	// Calculate fresh value
	let result = 0;

	if (!isCollective(a) && !isCollective(b)) {
		result = mutualFulfillment(ci, a, b);
	} else {
		result = collectiveMF(ci, a, b);
	}

	// Update cache
	if (!mfCache.has(a.id)) mfCache.set(a.id, new Map());
	mfCache.get(a.id)!.set(b.id, result);

	return result;
}

// Optimized weight calculation
function getCollectiveWeights(entity: Entity): [Map<string, number>, number] {
	if (!isCollective(entity)) {
		return [new Map([[entity.id, 1.0]]), 1.0];
	}

	// Check weight cache
	if (weightCache.has(entity.id)) {
		const weights = weightCache.get(entity.id)!;
		const total = Array.from(weights.values()).reduce((sum, w) => sum + w, 0);
		return [weights, total];
	}

	// Calculate fresh weights
	const weights = new Map<string, number>();
	let total = 0;

	for (const member of entity.members) {
		const subCollective = createSubCollective(entity, member);
		const mf = mutualFulfillmentC(ci, member, subCollective);
		weights.set(member.id, mf);
		total += mf;
	}

	// Normalize and cache
	const normalized = new Map(Array.from(weights.entries()).map(([id, w]) => [id, w / total]));

	weightCache.set(entity.id, normalized);
	return [normalized, total];
}

// Optimized sub-collective creation
const subCollectiveCache = new Map<string, Collective>();

function createSubCollective(collective: Collective, exclude: Entity): Collective {
	const cacheKey = `${collective.id}-${exclude.id}`;

	if (subCollectiveCache.has(cacheKey)) {
		return subCollectiveCache.get(cacheKey)!;
	}

	const sub = {
		...collective,
		id: cacheKey,
		members: collective.members.filter((m) => m.id !== exclude.id),
		weights: new Map()
	};

	subCollectiveCache.set(cacheKey, sub);
	return sub;
}

// Enhanced collective capacity calculation
const capacityCache = new Map<EntityID, number>();

function collectiveCapacity(ci: Forest, collective: Collective): number {
	if (capacityCache.has(collective.id)) {
		return capacityCache.get(collective.id)!;
	}

	let total = 0;
	const memberMFs = new Map<string, number>();
	let mfSum = 0;

	// Pre-calculate all MF values
	for (const member of collective.members) {
		const mf = mutualFulfillmentC(ci, member, collective);
		memberMFs.set(member.id, mf);
		mfSum += mf;
	}

	for (const member of collective.members) {
		const mf = memberMFs.get(member.id)!;
		const phi = mf / mfSum;

		if (isCollective(member)) {
			total += collectiveCapacity(ci, member) * phi;
		} else {
			const capacities = getNodeCapacities(member);
			const memberCap = Array.from(capacities.values()).reduce((acc, cap) => acc + cap.quantity, 0);
			total += memberCap * phi;
		}
	}

	capacityCache.set(collective.id, total);
	return total;
}

*/
// Export necessary functions and types
export {
	// Core types
	type Node,
	type RootNode,
	type NonRootNode,
	type TreeZipper,
	type Forest,
	type NavigationPath,
	type LocationType,
	type RecurrenceUnit,
	type RecurrenceEnd,
	type ShareMap,
	type Capacity,
	type CapacityShare,
	type CapacityInventory,
	type CapacityShares,

	// Core navigation and tree functions
	enterChild,
	exitToParent,
	goToRoot,
	getCurrentPath,
	modifyNode,
	children,
	descendants,
	createRootNode,
	createNonRootNode,
	addChild,
	addContributors,
	addToForest,

	// Calculation functions
	weight,
	shareOfParent,
	fulfilled,
	desire,
	mutualFulfillment,
	shareOfGeneralFulfillment,
	sharesOfGeneralFulfillmentMap,
	providerShares,

	// Capacity functions
	computeQuantityShare,
	createCapacityShare,
	addCapacity,
	addCapacityShare,
	receiverShareFrom,
	getPersonalCapacityShare,
	updateComputedQuantities,

	// Examples and utilities
	exampleForest,
	main
};
