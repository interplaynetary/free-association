import type { Forest, ProviderSharesCache, ShareMap, TreeZipper, VisitedSet } from './types';
import {
	cacheLookup,
	cacheInsert,
	fromFloat,
	fromShareMap,
	toFloat,
	toShareMap,
	withCacheM,
	Cacheable
} from './cache';
import {
	mutualCacheKey,
	getAllDescendantsCached,
	weight,
	fulfilled,
	withNodeCache
} from './calculations';
import { enterChild } from './node';

// Calculate share of general fulfillment between nodes
export function shareOfGeneralFulfillment(
	forest: Forest,
	target: TreeZipper,
	contributor: TreeZipper
): number {
	const contribId = contributor.zipperCurrent.nodeId;
	const rootContributor = forest.get(contribId);

	if (!rootContributor) return 0;

	// Find all nodes that include this contributor
	const allDescendants = getAllDescendantsCached(target);

	// Only consider contribution nodes (non-root with contributors)
	const contributingNodes = allDescendants.filter(
		(node) => node.zipperCurrent.nodeContributors.has(contribId) && node.zipperContext !== null
	);

	if (contributingNodes.length === 0) return 0;

	// Calculate weighted total using functional reduction (mirrors Haskell sum)
	return contributingNodes.reduce((total, node) => {
		const nodeWeight = weight(node);
		const nodeFulfillment = fulfilled(forest, node);
		const contributorCount = node.zipperCurrent.nodeContributors.size;

		// Divide by contributor count (mirrors Haskell division)
		return total + (nodeWeight * nodeFulfillment) / contributorCount;
	}, 0);
}

// Calculate mutual fulfillment between two nodes with caching
export function mutualFulfillment(forest: Forest, a: TreeZipper, b: TreeZipper): number {
	return withNodeCache(
		(zipper) => (node) => {
			const cacheKey = mutualCacheKey(node.nodeId, b.zipperCurrent.nodeId);

			return withCacheM(
				cacheKey,
				Cacheable.fromFloat,
				Cacheable.toFloat,
				() => {
					const aToB = shareOfGeneralFulfillment(forest, a, b);
					const bToA = shareOfGeneralFulfillment(forest, b, a);
					return Math.min(aToB, bToA); // Mirror Haskell's min function
				},
				node.nodeCache,
				undefined
			);
		},
		a
	);
}

// Core share calculation function that handles all depths
export function providerShares(forest: Forest, provider: TreeZipper, depth: number): ShareMap {
	// For depth <= 1, just return initial shares
	if (depth <= 1) return calculateInitialShares();

	// Initial state
	const initialShares = calculateInitialShares();
	const initialVisited: VisitedSet = new Set();

	// Process each depth (matches Haskell's foldl')
	let [finalShares, _] = Array.from({ length: depth - 1 }, (_, i) => i + 2).reduce(
		(acc, _) => processDepth(forest, acc[0], acc[1]),
		[initialShares, initialVisited] as [ShareMap, VisitedSet]
	);

	return normalizeShares(finalShares);

	// Helper function to calculate initial shares
	function calculateInitialShares(): ShareMap {
		const contributors = provider.zipperCurrent.nodeContributors;

		// Fix type issues with proper annotations
		const validContributors = Array.from(contributors)
			.map<[string, number]>((id) => {
				const contributor = forest.get(id);
				if (!contributor) return [id, 0];
				return [id, mutualFulfillment(forest, provider, contributor)];
			})
			.filter(([_, val]) => val > 0);

		const total = validContributors.reduce((sum, [_, val]) => sum + val, 0);

		if (total === 0) return new Map();

		return new Map(validContributors.map(([id, val]) => [id, val / total]));
	}
}

// Process additional depths for transitive share calculation
function processDepth(
	forest: Forest,
	shares: ShareMap,
	visited: VisitedSet
): [ShareMap, VisitedSet] {
	const recipients = Array.from(shares.keys());
	const unvisitedRecipients = recipients.filter((id) => !visited.has(id));

	// Fold over unvisited recipients (mirrors Haskell's foldl')
	return unvisitedRecipients.reduce(
		(acc, recipientId) => processRecipient(forest, acc[0], acc[1], recipientId),
		[shares, visited] as [ShareMap, VisitedSet]
	);
}

// Process a single recipient for transitive shares
function processRecipient(
	forest: Forest,
	currentShares: ShareMap,
	currentVisited: VisitedSet,
	recipientId: string
): [ShareMap, VisitedSet] {
	// Skip if conditions aren't met (mirrors Haskell's pattern guards)
	if (!currentShares.has(recipientId)) return [currentShares, currentVisited];

	const recipient = forest.get(recipientId);
	if (!recipient) return [currentShares, currentVisited];

	const recipientShare = currentShares.get(recipientId)!;

	// Mark as visited (immutable update)
	const newVisited = new Set(currentVisited).add(recipientId);

	// Get unvisited connections
	const connections = recipient.zipperCurrent.nodeContributors;
	const unvisitedConnections = Array.from(connections).filter((id) => !newVisited.has(id));

	if (unvisitedConnections.length === 0) return [currentShares, newVisited];

	// Calculate transitive shares
	const transitiveShares = providerShares(forest, recipient, 1);

	// Weight shares by recipient's share (mirrors Haskell's map operation)
	const weightedShares = new Map(
		Array.from(transitiveShares.entries()).map(([id, share]) => [id, share * recipientShare])
	);

	// Merge with current shares (mirrors Haskell's Map.unionWith)
	const result = new Map(currentShares);
	for (const [id, share] of weightedShares) {
		result.set(id, (result.get(id) || 0) + share);
	}

	return [result, newVisited];
}

// Normalize shares to sum to 1.0
function normalizeShares(shares: ShareMap): ShareMap {
	const total = Array.from(shares.values()).reduce((sum, share) => sum + share, 0);

	// If no valid shares, return as is (mirrors Haskell's if-then-else)
	if (total <= 0) return shares;

	// Map each share to its normalized value (mirrors Haskell's Map.map)
	return new Map(Array.from(shares.entries()).map(([id, share]) => [id, share / total]));
}

// Provider shares calculation with caching
export function providerSharesCached(
	cache: ProviderSharesCache,
	forest: Forest,
	provider: TreeZipper,
	maxDepth: number
): [ShareMap, ProviderSharesCache] {
	const providerId = provider.zipperCurrent.nodeId;
	const key = `${providerId}_${maxDepth}`;

	const [shares, updatedCache] = withCacheM(
		key,
		Cacheable.fromShareMap,
		Cacheable.toShareMap,
		() => providerShares(forest, provider, maxDepth),
		cache.sharesCache,
		undefined
	);

	return [shares, { sharesCache: updatedCache }];
}

// Simplified interface functions
export function directShare(forest: Forest, provider: TreeZipper, recipientId: string): number {
	return providerShares(forest, provider, 1).get(recipientId) || 0;
}

// Get a receiver's share from a specific provider
export function receiverShareFrom(
	forest: Forest,
	receiver: TreeZipper,
	provider: TreeZipper,
	maxDepth: number
): number {
	const providerShareMap = providerShares(forest, provider, maxDepth);
	return providerShareMap.get(receiver.zipperCurrent.nodeId) || 0;
}
