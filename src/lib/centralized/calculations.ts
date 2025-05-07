import type { Forest, Node, TreeZipper, Points, Cache } from './types';
import {
	cacheLookup,
	cacheInsert,
	fromFloat,
	fromInt,
	toFloat,
	toInt,
	toStringList,
	fromStringList
} from './cache';
import { enterChild, exitToParent, getPoints } from './node';

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

// Calculate total points from all children
export function totalChildPoints(zipper: TreeZipper): number {
	const current = zipper.zipperCurrent;
	const cache = current.nodeCache;
	const cacheKey = totalPointsCacheKey(current.nodeId);

	const lookupResult = cacheLookup(cacheKey, cache);

	if (lookupResult) {
		const [value, updatedCache] = lookupResult;
		const total = toInt(value);

		if (total !== null) {
			return total;
		}

		return computeAndCache(updatedCache, cacheKey);
	}

	return computeAndCache(cache, cacheKey);

	function computeAndCache(cacheInstance: Cache<string>, key: string): number {
		// Calculate sum of children's points
		let total = 0;
		for (const [_, child] of current.nodeChildren) {
			total += getPoints(child.nodePoints);
		}

		// Update cache
		const newCache = cacheInsert(key, fromInt(total), cacheInstance);
		current.nodeCache = newCache;

		return total;
	}
}

// Calculate a node's weight with caching
export function weight(zipper: TreeZipper): number {
	const current = zipper.zipperCurrent;
	const cacheKey = weightCacheKey(current.nodeId);

	// Root node has weight 1.0
	if (!zipper.zipperContext) {
		return 1.0;
	}

	const lookupResult = cacheLookup(cacheKey, current.nodeCache);

	if (lookupResult) {
		const [value, updatedCache] = lookupResult;
		const cachedWeight = toFloat(value);

		if (cachedWeight !== null) {
			return cachedWeight;
		}

		// Compute if cache miss or wrong type
		const w = computeWeight(current);
		const newCache = cacheInsert(cacheKey, fromFloat(w), updatedCache);
		current.nodeCache = newCache;
		return w;
	}

	// Compute if not in cache
	const w = computeWeight(current);
	const newCache = cacheInsert(cacheKey, fromFloat(w), current.nodeCache);
	current.nodeCache = newCache;
	return w;

	function computeWeight(node: Node): number {
		const parentZipper = exitToParent(zipper);
		if (!parentZipper) return 0;

		const total = totalChildPoints(parentZipper);
		if (total === 0) return 0;

		const currentPoints = getPoints(node.nodePoints);
		const parentWeight = weight(parentZipper);

		return (currentPoints / total) * parentWeight;
	}
}

// Calculate share of parent's total points
export function shareOfParent(zipper: TreeZipper): number {
	// Root node has 100% share
	const parent = exitToParent(zipper);
	if (!parent) return 1.0;

	const total = totalChildPoints(parent);
	if (total === 0) return 0;

	const currentPoints = getPoints(zipper.zipperCurrent.nodePoints);
	return currentPoints / total;
}

// Check if a node is a contribution node
export function isContribution(forest: Forest, zipper: TreeZipper): boolean {
	const node = zipper.zipperCurrent;
	return node.nodeContributors.size > 0 && zipper.zipperContext !== null;
}

// Check if a node has direct contribution children
export function hasDirectContributionChild(forest: Forest, zipper: TreeZipper): boolean {
	const current = zipper.zipperCurrent;
	const childIds = Array.from(current.nodeChildren.keys());

	for (const id of childIds) {
		const childZipper = enterChild(id, zipper);
		if (childZipper && isContribution(forest, childZipper)) {
			return true;
		}
	}

	return false;
}

// Check if a node has non-contribution children
export function hasNonContributionChild(forest: Forest, zipper: TreeZipper): boolean {
	const current = zipper.zipperCurrent;
	const childIds = Array.from(current.nodeChildren.keys());

	for (const id of childIds) {
		const childZipper = enterChild(id, zipper);
		if (childZipper && !isContribution(forest, childZipper)) {
			return true;
		}
	}

	return false;
}

// Calculate the proportion of total child points from contribution children
export function contributionChildrenWeight(forest: Forest, zipper: TreeZipper): number {
	const current = zipper.zipperCurrent;
	const childIds = Array.from(current.nodeChildren.keys());

	let contribWeight = 0;
	let totalWeight = 0;

	for (const id of childIds) {
		const childZipper = enterChild(id, zipper);
		if (!childZipper) continue;

		const w = weight(childZipper);
		totalWeight += w;

		if (isContribution(forest, childZipper)) {
			contribWeight += w;
		}
	}

	if (totalWeight === 0) return 0;
	return contribWeight / totalWeight;
}

// Sum fulfillment from children matching a predicate
export function childrenFulfillment(
	forest: Forest,
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
			sum += fulfilled(forest, childZipper) * shareOfParent(childZipper);
		}
	}

	return sum;
}

// Calculate the fulfillment from contribution children
export function contributionChildrenFulfillment(forest: Forest, zipper: TreeZipper): number {
	return childrenFulfillment(forest, (z) => isContribution(forest, z), zipper);
}

// Calculate the fulfillment from non-contribution children
export function nonContributionChildrenFulfillment(forest: Forest, zipper: TreeZipper): number {
	const current = zipper.zipperCurrent;
	const childIds = Array.from(current.nodeChildren.keys());

	let weightedSum = 0;
	let totalWeight = 0;

	for (const id of childIds) {
		const childZipper = enterChild(id, zipper);
		if (!childZipper || isContribution(forest, childZipper)) continue;

		const w = weight(childZipper);
		const f = fulfilled(forest, childZipper);

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
export function fulfilled(forest: Forest, zipper: TreeZipper): number {
	const current = zipper.zipperCurrent;
	const cacheKey = fulfillmentCacheKey(current.nodeId);
	const cache = current.nodeCache;

	const lookupResult = cacheLookup(cacheKey, cache);

	if (lookupResult) {
		const [value, updatedCache] = lookupResult;
		const cachedFulfillment = toFloat(value);

		if (cachedFulfillment !== null) {
			return cachedFulfillment;
		}

		const f = computeFulfillment(current);
		const newCache = cacheInsert(cacheKey, fromFloat(f), updatedCache);
		current.nodeCache = newCache;
		return f;
	}

	const f = computeFulfillment(current);
	const newCache = cacheInsert(cacheKey, fromFloat(f), cache);
	current.nodeCache = newCache;
	return f;

	function computeFulfillment(node: Node): number {
		// Leaf contribution nodes have 100% fulfillment
		if (node.nodeChildren.size === 0) {
			return isContribution(forest, zipper) ? 1.0 : 0.0;
		}

		// If manual fulfillment is set and has direct contribution children
		if (node.nodeManualFulfillment !== null && hasDirectContributionChild(forest, zipper)) {
			if (!hasNonContributionChild(forest, zipper)) {
				return node.nodeManualFulfillment;
			}

			// Calculate weighted fulfillment between contribution and non-contribution children
			const contribWeight = contributionChildrenWeight(forest, zipper);
			const nonContribFulfillment = nonContributionChildrenFulfillment(forest, zipper);

			return (
				node.nodeManualFulfillment * contribWeight + nonContribFulfillment * (1.0 - contribWeight)
			);
		}

		// Otherwise, calculate weighted sum of children's fulfillment
		let sum = 0;
		const childIds = Array.from(node.nodeChildren.keys());

		for (const id of childIds) {
			const childZipper = enterChild(id, zipper);
			if (!childZipper) continue;

			sum += fulfilled(forest, childZipper) * shareOfParent(childZipper);
		}

		return sum;
	}
}

// Calculate the desire (unfulfilled need) of a node
export function desire(forest: Forest, zipper: TreeZipper): number {
	return 1.0 - fulfilled(forest, zipper);
}

// Get all descendants of a node with caching
export function getAllDescendantsCached(zipper: TreeZipper): TreeZipper[] {
	const current = zipper.zipperCurrent;
	const currentId = current.nodeId;
	const cache = current.nodeCache;
	const cacheKey = descendantsCacheKey(currentId);

	const lookupResult = cacheLookup(cacheKey, cache);

	if (lookupResult) {
		const [value, updatedCache] = lookupResult;
		const descendantIds = toStringList(value);

		if (descendantIds !== null) {
			// We only cache the IDs, so we need to actually get the descendants
			return [zipper, ...getAllDescendants(zipper)];
		}

		return computeAndCache(cache, cacheKey);
	}

	return computeAndCache(cache, cacheKey);

	function computeAndCache(cacheInstance: Cache<string>, key: string): TreeZipper[] {
		const descendants = getAllDescendants(zipper);
		const descendantIds = descendants.map((z) => z.zipperCurrent.nodeId);

		const newCache = cacheInsert(key, fromStringList(descendantIds), cacheInstance);
		current.nodeCache = newCache;

		return [zipper, ...descendants];
	}
}

// Get all descendants of a node
export function getAllDescendants(zipper: TreeZipper): TreeZipper[] {
	const result: TreeZipper[] = [];
	const childIds = Array.from(zipper.zipperCurrent.nodeChildren.keys());

	for (const id of childIds) {
		const childZipper = enterChild(id, zipper);
		if (!childZipper) continue;

		result.push(childZipper);
		result.push(...getAllDescendants(childZipper));
	}

	return result;
}
