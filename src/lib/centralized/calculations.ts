import type { Forest, Node, TreeZipper, Points, Cache } from './types';
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

// Helper to run a cache computation on a node (mirrors runNodeCache)
export function runNodeCache<A>(
	computation: (n: Node) => [A, Node['nodeCache']],
	node: Node
): [A, Node] {
	const [result, newCache] = computation(node);
	return [result, { ...node, nodeCache: newCache }];
}

// Mirror the Haskell withNodeCache higher-order function
export function withNodeCache<A>(
	computation: (z: TreeZipper) => (n: Node) => [A, Node['nodeCache']],
	z: TreeZipper
): A {
	const current = z.zipperCurrent;
	const [result, _] = runNodeCache(computation(z), current);
	return result;
}

// Calculate total points from children with caching
export function totalChildPoints(z: TreeZipper): number {
	return withNodeCache(
		(zipper) => (node) => {
			const cacheKey = totalPointsCacheKey(node.nodeId);

			return withCacheM(
				cacheKey,
				Cacheable.fromInt,
				Cacheable.toInt,
				() => {
					let total = 0;
					for (const [_, child] of node.nodeChildren) {
						total += getPoints(child.nodePoints);
					}
					return total;
				},
				node.nodeCache,
				undefined
			);
		},
		z
	);
}

// Calculate node weight with caching
export function weight(z: TreeZipper): number {
	return withNodeCache(
		(zipper) => (node) => {
			const cacheKey = weightCacheKey(node.nodeId);

			// Root node has weight 1.0
			if (!zipper.zipperContext) {
				return [1.0, node.nodeCache];
			}

			return withCacheM(
				cacheKey,
				Cacheable.fromFloat,
				Cacheable.toFloat,
				() => {
					const parent = exitToParent(zipper);
					if (!parent) return 0;

					const total = totalChildPoints(parent);
					if (total === 0) return 0;

					const currentPoints = getPoints(node.nodePoints);
					const parentWeight = weight(parent);

					return (currentPoints / total) * parentWeight;
				},
				node.nodeCache,
				undefined
			);
		},
		z
	);
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
export function fulfilled(forest: Forest, z: TreeZipper): number {
	return withNodeCache(
		(zipper) => (node) => {
			const cacheKey = fulfillmentCacheKey(node.nodeId);

			return withCacheM(
				cacheKey,
				Cacheable.fromFloat,
				Cacheable.toFloat,
				() => computeFulfillment(node),
				node.nodeCache,
				undefined
			);

			function computeFulfillment(n: Node): number {
				// Leaf contribution nodes have 100% fulfillment
				if (n.nodeChildren.size === 0) {
					return isContribution(forest, z) ? 1.0 : 0.0;
				}

				// Manual fulfillment with contribution children
				if (n.nodeManualFulfillment !== null && hasDirectContributionChild(forest, z)) {
					if (!hasNonContributionChild(forest, z)) {
						return n.nodeManualFulfillment;
					}

					// Weighted fulfillment calculation
					const contribWeight = contributionChildrenWeight(forest, z);
					const nonContribFulfillment = nonContributionChildrenFulfillment(forest, z);

					return (
						n.nodeManualFulfillment * contribWeight + nonContribFulfillment * (1.0 - contribWeight)
					);
				}

				// Sum children's weighted fulfillment
				let sum = 0;
				for (const childId of n.nodeChildren.keys()) {
					const childZipper = enterChild(childId, z);
					if (!childZipper) continue;

					sum += fulfilled(forest, childZipper) * shareOfParent(childZipper);
				}

				return sum;
			}
		},
		z
	);
}

// Calculate the desire (unfulfilled need) of a node
export function desire(forest: Forest, zipper: TreeZipper): number {
	return 1.0 - fulfilled(forest, zipper);
}

// Get all descendants with caching
export function getAllDescendantsCached(z: TreeZipper): TreeZipper[] {
	return [z, ...withNodeCache(getAllDescendantsCachedM, z)];
}

// Monadic version that mirrors Haskell's version
function getAllDescendantsCachedM(z: TreeZipper): (node: Node) => [TreeZipper[], Cache<string>] {
	return (node) => {
		const currentId = node.nodeId;
		const cacheKey = descendantsCacheKey(currentId);

		const [cachedIds, updatedCache] = withCacheM(
			cacheKey,
			Cacheable.fromStringList,
			Cacheable.toStringList,
			() => {
				const descendants = getAllDescendants(z);
				return descendants.map((d) => d.zipperCurrent.nodeId);
			},
			node.nodeCache,
			undefined
		);

		// Return the proper tuple structure
		return [getAllDescendants(z), updatedCache];
	};
}

// Get all descendants (mirrors Haskell's concatMap pattern)
export function getAllDescendants(z: TreeZipper): TreeZipper[] {
	return Array.from(z.zipperCurrent.nodeChildren.keys()).flatMap((id) => {
		const child = enterChild(id, z);
		if (!child) return [];
		return [child, ...getAllDescendants(child)];
	});
}
