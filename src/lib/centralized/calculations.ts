import type { Forest, Node, TreeZipper, Cache, ShareMap, PersistentCache } from './types';
import {
	cacheLookup,
	cacheInsert,
	withCacheM,
	Cacheable,
	fromFloat,
	fromInt,
	toFloat,
	toInt,
	toStringList,
	fromStringList,
	fromShareMap,
	toShareMap
} from './cache';
import { enterChild, exitToParent, updateNodePersistentCache } from './tree';
import { normalizeShareMap } from './utils';

// Cache key generation functions
export function weightCacheKey(nodeId: string): string {
	return `${nodeId}_weight`;
}

export function fulfillmentCacheKey(nodeId: string): string {
	return `${nodeId}_fulfillment`;
}

export function mutualCacheKey(nodeId1: string, nodeId2: string): string {
	return nodeId1 < nodeId2 ? `${nodeId1}_mutual_${nodeId2}` : `${nodeId2}_mutual_${nodeId1}`;
}

export function descendantsCacheKey(nodeId: string): string {
	return `${nodeId}_descendants`;
}

export function totalPointsCacheKey(nodeId: string): string {
	return `${nodeId}_total_points`;
}

// ==== Helper functions below were already implemented in new.ts ====
// So we'll import them from there instead of redefining them here

// Calculate total points from children with caching
export function totalChildPoints(z: TreeZipper): number {
	const current = z.zipperCurrent;
	const cacheKey = totalPointsCacheKey(current.nodeId);

	function computeTotal(): number {
		let total = 0;
		for (const [_, child] of current.nodeChildren) {
			total += child.nodePoints;
		}
		return total;
	}

	// Use the node's transient cache
	const [result, _] = withCacheM(
		cacheKey,
		fromInt,
		toInt,
		computeTotal,
		current.nodeTransientCache,
		undefined
	);

	return result;
}

// Calculate node weight with caching
export function weight(z: TreeZipper): number {
	const current = z.zipperCurrent;
	const cacheKey = weightCacheKey(current.nodeId);

	// Root node has weight 1.0
	if (!z.zipperContext) {
		return 1.0;
	}

	function computeWeight(): number {
		const parent = exitToParent(z);
		if (!parent) return 0;

		const total = totalChildPoints(parent);
		if (total === 0) return 0;

		const currentPoints = current.nodePoints;
		const parentWeight = weight(parent);

		return (currentPoints / total) * parentWeight;
	}

	// Use the node's transient cache
	const [result, _] = withCacheM(
		cacheKey,
		fromFloat,
		toFloat,
		computeWeight,
		current.nodeTransientCache,
		undefined
	);

	return result;
}

// Calculate share of parent's total points
export function shareOfParent(zipper: TreeZipper): number {
	// Root node has 100% share
	const parent = exitToParent(zipper);
	if (!parent) return 1.0;

	const total = totalChildPoints(parent);
	if (total === 0) return 0;

	const currentPoints = zipper.zipperCurrent.nodePoints;
	return currentPoints / total;
}

// Check if a node is a contribution node
export function isContribution(zipper: TreeZipper): boolean {
	const node = zipper.zipperCurrent;
	return node.nodeContributors.size > 0 && zipper.zipperContext !== null;
}

// Check if a node has direct contribution children
export function hasDirectContributionChild(zipper: TreeZipper): boolean {
	const current = zipper.zipperCurrent;
	const childIds = Array.from(current.nodeChildren.keys());

	for (const id of childIds) {
		const childZipper = enterChild(id, zipper);
		if (childZipper && isContribution(childZipper)) {
			return true;
		}
	}

	return false;
}

// Check if a node has non-contribution children
export function hasNonContributionChild(zipper: TreeZipper): boolean {
	const current = zipper.zipperCurrent;
	const childIds = Array.from(current.nodeChildren.keys());

	for (const id of childIds) {
		const childZipper = enterChild(id, zipper);
		if (childZipper && !isContribution(childZipper)) {
			return true;
		}
	}

	return false;
}

// Calculate the proportion of total child points from contribution children
export function contributionChildrenWeight(zipper: TreeZipper): number {
	const current = zipper.zipperCurrent;
	const childIds = Array.from(current.nodeChildren.keys());

	let contribWeight = 0;
	let totalWeight = 0;

	for (const id of childIds) {
		const childZipper = enterChild(id, zipper);
		if (!childZipper) continue;

		const w = weight(childZipper);
		totalWeight += w;

		if (isContribution(childZipper)) {
			contribWeight += w;
		}
	}

	if (totalWeight === 0) return 0;
	return contribWeight / totalWeight;
}

// Sum fulfillment from children matching a predicate
export function childrenFulfillment(
	predicate: (z: TreeZipper) => boolean,
	zipper: TreeZipper
): number {
	const current = zipper.zipperCurrent;
	const childIds = Array.from(current.nodeChildren.keys());
	let sum = 0;

	for (const id of childIds) {
		const childZipper = enterChild(id, zipper);
		if (!childZipper) continue;

		if (predicate(childZipper)) {
			sum += fulfilled(childZipper) * shareOfParent(childZipper);
		}
	}

	return sum;
}

// Calculate the fulfillment from contribution children
export function contributionChildrenFulfillment(zipper: TreeZipper): number {
	return childrenFulfillment(isContribution, zipper);
}

// Calculate the fulfillment from non-contribution children
export function nonContributionChildrenFulfillment(zipper: TreeZipper): number {
	const current = zipper.zipperCurrent;
	const childIds = Array.from(current.nodeChildren.keys());

	let weightedSum = 0;
	let totalWeight = 0;

	for (const id of childIds) {
		const childZipper = enterChild(id, zipper);
		if (!childZipper || isContribution(childZipper)) continue;

		const w = weight(childZipper);
		const f = fulfilled(childZipper);

		weightedSum += w * f;
		totalWeight += w;
	}

	if (totalWeight === 0) return 0;
	return weightedSum / totalWeight;
}

// Safely get instances of a contributor
export function getContributorInstances(forest: Forest, contribId: string): Set<TreeZipper> {
	const contributorZipper = forest.get(contribId);
	if (!contributorZipper) {
		// Return empty set if not found
		return new Set();
	}
	return new Set([contributorZipper]);
}

// Safely get a contributor node
export function getContributorNode(forest: Forest, contribId: string): TreeZipper | null {
	const instances = getContributorInstances(forest, contribId);
	return instances.size > 0 ? Array.from(instances)[0] : null;
}

// Calculate fulfillment with caching
export function fulfilled(z: TreeZipper): number {
	const current = z.zipperCurrent;
	const cacheKey = fulfillmentCacheKey(current.nodeId);

	function computeFulfillment(): number {
		// Leaf with no children
		if (current.nodeChildren.size === 0) {
			return isContribution(z) ? 1.0 : 0.0;
		}

		// Node with manual fulfillment and direct contribution children
		if (current.nodeManualFulfillment !== null && hasDirectContributionChild(z)) {
			if (!hasNonContributionChild(z)) {
				return current.nodeManualFulfillment;
			}

			// Mixed contribution and non-contribution children
			const contribWeight = contributionChildrenWeight(z);
			const nonContribFulfillment = nonContributionChildrenFulfillment(z);

			return (
				current.nodeManualFulfillment * contribWeight +
				nonContribFulfillment * (1.0 - contribWeight)
			);
		}

		// Standard case: average of children weighted by their share
		let total = 0;
		for (const id of Array.from(current.nodeChildren.keys())) {
			const childZipper = enterChild(id, z);
			if (!childZipper) continue;

			total += fulfilled(childZipper) * shareOfParent(childZipper);
		}

		return total;
	}

	// Use the node's transient cache
	const [result, _] = withCacheM(
		cacheKey,
		fromFloat,
		toFloat,
		computeFulfillment,
		current.nodeTransientCache,
		undefined
	);

	return result;
}

// Calculate the desire (unfulfilled need) of a node
export function desire(zipper: TreeZipper): number {
	return 1.0 - fulfilled(zipper);
}

// Get all descendants with caching
export function getAllDescendantsCached(z: TreeZipper): TreeZipper[] {
	const current = z.zipperCurrent;
	const currentId = current.nodeId;
	const cacheKey = descendantsCacheKey(currentId);

	// Try to get from cache first
	const lookupResult = cacheLookup(cacheKey, current.nodeTransientCache);

	if (lookupResult) {
		const [value, _] = lookupResult;
		const ids = toStringList(value);

		if (ids !== null) {
			// We have the ids cached, but still need to rebuild the actual tree
			return [z, ...getAllDescendants(z).slice(1)];
		}
	}

	// Not in cache, compute and cache
	const descendants = getAllDescendants(z).slice(1);
	const descendantIds = descendants.map((d) => d.zipperCurrent.nodeId);

	// Update cache
	const newCache = cacheInsert(cacheKey, fromStringList(descendantIds), current.nodeTransientCache);

	// We don't modify the node here since it would require us to return a modified zipper
	// This is fine for read-only operations

	return [z, ...descendants];
}

// Get all descendants (including self)
export function getAllDescendants(z: TreeZipper): TreeZipper[] {
	const result: TreeZipper[] = [z];
	const current = z.zipperCurrent;

	for (const childId of current.nodeChildren.keys()) {
		const child = enterChild(childId, z);
		if (child) {
			result.push(...getAllDescendants(child));
		}
	}

	return result;
}

//----------------------
// Mutual Fulfillment
//----------------------

// Calculate share of general fulfillment
export function shareOfGeneralFulfillment(
	ci: Forest,
	target: TreeZipper,
	contributor: TreeZipper
): number {
	const contribId = contributor.zipperCurrent.nodeId;
	const rootContributor = ci.get(contribId);

	if (!rootContributor) return 0;

	// Get all descendant nodes (including self)
	const allNodes = getAllDescendantsCached(target);

	// Filter for contribution nodes that include this contributor
	const contributingNodes = allNodes.filter((node) => {
		const current = node.zipperCurrent;
		return current.nodeContributors.has(contribId) && isContribution(node);
	});

	// Calculate total contribution from these nodes
	let total = 0;
	let weightedTotal = 0;

	for (const node of contributingNodes) {
		const w = weight(node);
		const f = fulfilled(node);
		const c = node.zipperCurrent.nodeContributors.size;

		total += w * f;
		weightedTotal += (w * f) / c;
	}

	return weightedTotal;
}

//------------------------------------------
// Shares-of-General-Fulfillment Map
//------------------------------------------

// Get the normalized shares-of-general-fulfillment map
export function sharesOfGeneralFulfillmentMap(ci: Forest, z: TreeZipper): ShareMap {
	// First check persistent cache
	const current = z.zipperCurrent;
	const persistentCache = current.nodePersistentCache;

	// Get SOGF map from persistent cache
	const sogfMap = persistentCache.pcSogfMap;
	if (sogfMap !== null) {
		// Always normalize on read to ensure normalization
		return normalizeShareMap(sogfMap);
	}

	// Compute and store in persistent cache
	const pairs: [string, number][] = [];

	// Calculate share for each contributor
	for (const [id, contributor] of ci.entries()) {
		const share = shareOfGeneralFulfillment(ci, z, contributor);
		if (share > 0) {
			pairs.push([id, share]);
		}
	}

	// Create and normalize the map
	const rawMap = new Map(pairs);
	const normMap = normalizeShareMap(rawMap);

	// Update persistent cache and return
	const updatedCache = {
		...persistentCache,
		pcSogfMap: normMap
	};

	// Update the persistent cache
	const updatedZipper = updateNodePersistentCache(() => updatedCache, z);

	return normMap;
}

//------------------------------------------
// Provider Shares
//------------------------------------------

// Get provider shares for a specific depth
export function providerShares(ci: Forest, provider: TreeZipper, depth: number): ShareMap {
	// First check persistent cache
	const current = provider.zipperCurrent;
	const persistentCache = current.nodePersistentCache;

	// Get provider shares from persistent cache
	const sharesMap = persistentCache.pcProviderShares.get(depth);
	if (sharesMap !== null && sharesMap !== undefined) {
		// Always normalize on read
		return normalizeShareMap(sharesMap);
	}

	// Compute shares for requested depth
	let result: ShareMap;

	if (depth <= 1) {
		result = depth1Shares(ci, provider);
	} else {
		result = depthNShares(ci, provider, depth);
	}

	// Normalize
	const normResult = normalizeShareMap(result);

	// Update persistent cache
	const updatedProviderShares = new Map(persistentCache.pcProviderShares);
	updatedProviderShares.set(depth, normResult);

	const updatedCache = {
		...persistentCache,
		pcProviderShares: updatedProviderShares
	};

	// Update the persistent cache
	const updatedZipper = updateNodePersistentCache(() => updatedCache, provider);

	return normResult;
}

// Calculate direct (depth-1) shares
function depth1Shares(ci: Forest, provider: TreeZipper): ShareMap {
	// Get all descendant nodes (including self)
	const allNodes = getAllDescendantsCached(provider);

	// Get all unique contributor IDs across the tree
	const allContribs = new Set<string>();
	for (const node of allNodes) {
		for (const contrib of node.zipperCurrent.nodeContributors) {
			allContribs.add(contrib);
		}
	}

	// Get valid contributors (those that exist in ci)
	const validContribs: TreeZipper[] = [];
	for (const id of allContribs) {
		const contrib = ci.get(id);
		if (contrib) {
			validContribs.push(contrib);
		}
	}

	// Calculate mutual fulfillment for each contributor
	const pairs: [string, number][] = [];
	for (const contrib of validContribs) {
		const id = contrib.zipperCurrent.nodeId;
		const mf = mutualFulfillment(ci, provider, contrib);
		if (mf > 0) {
			pairs.push([id, mf]);
		}
	}

	return new Map(pairs);
}

// Calculate deeper provider shares
function depthNShares(ci: Forest, provider: TreeZipper, depth: number): ShareMap {
	let result = depth1Shares(ci, provider);

	// Iteratively build up deeper shares
	for (let d = 2; d <= depth; d++) {
		const recipients = Array.from(result.keys());
		let newResult = new Map<string, number>();

		// For each recipient in the previous level
		for (const rid of recipients) {
			const recipientZ = ci.get(rid);
			if (!recipientZ) continue;

			const recipientShare = result.get(rid) || 0;

			// Get the recipient's depth-1 shares
			const recipientDepth1 = providerShares(ci, recipientZ, 1);

			// Weight by recipient's share
			for (const [id, share] of recipientDepth1.entries()) {
				const weightedShare = share * recipientShare;
				const current = newResult.get(id) || 0;
				newResult.set(id, current + weightedShare);
			}
		}

		result = newResult;
	}

	return result;
}

// Calculate mutual fulfillment between two nodes
export function mutualFulfillment(ci: Forest, a: TreeZipper, b: TreeZipper): number {
	const aId = a.zipperCurrent.nodeId;
	const bId = b.zipperCurrent.nodeId;

	const sharesA = sharesOfGeneralFulfillmentMap(ci, a);
	const sharesB = sharesOfGeneralFulfillmentMap(ci, b);

	const aToB = sharesA.get(bId) || 0;
	const bToA = sharesB.get(aId) || 0;

	return Math.min(aToB, bToA);
}

// Get a receiver's share from a specific capacity provider
export function receiverShareFrom(
	ci: Forest,
	receiver: TreeZipper,
	provider: TreeZipper,
	capacity: any,
	maxDepth: number
): number {
	const providerShareMap = providerShares(ci, provider, maxDepth);
	const receiverId = receiver.zipperCurrent.nodeId;

	return providerShareMap.get(receiverId) || 0;
}
