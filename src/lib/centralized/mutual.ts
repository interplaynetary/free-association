import type { Forest, ProviderSharesCache, ShareMap, TreeZipper, VisitedSet } from './types';
import { cacheLookup, cacheInsert, fromFloat, fromShareMap, toFloat, toShareMap } from './cache';
import { mutualCacheKey, getAllDescendantsCached, weight, fulfilled } from './calculations';
import { enterChild } from './node';

// Calculate share of general fulfillment between nodes
export function shareOfGeneralFulfillment(
	forest: Forest,
	target: TreeZipper,
	contributor: TreeZipper
): number {
	const contribId = contributor.zipperCurrent.nodeId;
	const targetId = target.zipperCurrent.nodeId;
	console.log(`[DEBUG] Calculating shareOfGeneralFulfillment from ${contribId} to ${targetId}`);

	const rootContributor = forest.get(contribId);

	if (!rootContributor) {
		console.log(`[DEBUG] Root contributor ${contribId} not found in forest`);
		return 0;
	}

	// Find all nodes that include this contributor
	const allDescendants = getAllDescendantsCached(target);
	console.log(`[DEBUG] Found ${allDescendants.length} descendants for ${targetId}`);

	// Only consider contribution nodes (non-root with contributors)
	const contributingNodes = allDescendants.filter((node) => {
		const nodeContribs = node.zipperCurrent.nodeContributors;
		return nodeContribs.has(contribId) && node.zipperContext !== null;
	});
	console.log(`[DEBUG] Found ${contributingNodes.length} nodes with ${contribId} as contributor`);

	if (contributingNodes.length === 0) {
		console.log(`[DEBUG] No contributing nodes found for ${contribId} in ${targetId}'s tree`);
		return 0;
	}

	// Calculate total contribution from these nodes with weighting by number of contributors
	let weightedTotal = 0;

	for (const node of contributingNodes) {
		const nodeId = node.zipperCurrent.nodeId;
		const nodeWeight = weight(node);
		const nodeFulfillment = fulfilled(forest, node);
		const contributorCount = node.zipperCurrent.nodeContributors.size;

		console.log(
			`[DEBUG] Node ${nodeId}: weight=${nodeWeight}, fulfillment=${nodeFulfillment}, contributors=${contributorCount}`
		);

		// Divide each node's contribution by its number of contributors
		const contribution = (nodeWeight * nodeFulfillment) / contributorCount;
		weightedTotal += contribution;

		console.log(
			`[DEBUG] Contribution from ${nodeId}: ${contribution}, running total: ${weightedTotal}`
		);
	}

	console.log(
		`[DEBUG] Final shareOfGeneralFulfillment from ${contribId} to ${targetId}: ${weightedTotal}`
	);
	return weightedTotal;
}

// Calculate mutual fulfillment between two nodes with caching
export function mutualFulfillment(forest: Forest, a: TreeZipper, b: TreeZipper): number {
	const aNode = a.zipperCurrent;
	const bNode = b.zipperCurrent;
	const aId = aNode.nodeId;
	const bId = bNode.nodeId;

	console.log(`[DEBUG] Calculating mutual fulfillment between ${aId} and ${bId}`);
	const cacheKey = mutualCacheKey(aNode.nodeId, bNode.nodeId);

	const lookupResult = cacheLookup(cacheKey, aNode.nodeCache);

	if (lookupResult) {
		const [value, updatedCache] = lookupResult;
		const mutual = toFloat(value);

		if (mutual !== null) {
			console.log(`[DEBUG] Cache hit for mutual fulfillment between ${aId} and ${bId}: ${mutual}`);
			return mutual;
		}

		console.log(
			`[DEBUG] Cache miss for mutual fulfillment between ${aId} and ${bId}, computing...`
		);
		const result = compute();
		const newCache = cacheInsert(cacheKey, fromFloat(result), updatedCache);
		aNode.nodeCache = newCache;
		console.log(`[DEBUG] Computed mutual fulfillment between ${aId} and ${bId}: ${result}`);
		return result;
	}

	console.log(
		`[DEBUG] No cache entry for mutual fulfillment between ${aId} and ${bId}, computing...`
	);
	const result = compute();
	const newCache = cacheInsert(cacheKey, fromFloat(result), aNode.nodeCache);
	aNode.nodeCache = newCache;
	console.log(`[DEBUG] Computed mutual fulfillment between ${aId} and ${bId}: ${result}`);
	return result;

	function compute(): number {
		console.log(`[DEBUG] Computing mutual fulfillment between ${aId} and ${bId}`);
		const aToB = shareOfGeneralFulfillment(forest, a, b);
		const bToA = shareOfGeneralFulfillment(forest, b, a);
		console.log(`[DEBUG] aToB: ${aToB}, bToA: ${bToA}`);
		return Math.min(aToB, bToA);
	}
}

// Core share calculation function that handles all depths
export function providerShares(forest: Forest, provider: TreeZipper, depth: number): ShareMap {
	const providerId = provider.zipperCurrent.nodeId;
	console.log(`[DEBUG] Calculating provider shares for ${providerId} at depth ${depth}`);

	if (depth <= 1) {
		console.log(`[DEBUG] Using direct shares calculation for ${providerId} (depth <= 1)`);
		return calculateInitialShares();
	}

	let shares = calculateInitialShares();
	console.log(`[DEBUG] Initial shares for ${providerId}:`, Object.fromEntries(shares.entries()));
	let visited: VisitedSet = new Set();

	// Process each depth level
	for (let i = 2; i <= depth; i++) {
		console.log(`[DEBUG] Processing depth ${i} for ${providerId}`);
		[shares, visited] = processNextDepth(shares, visited);
		console.log(`[DEBUG] Shares after depth ${i}:`, Object.fromEntries(shares.entries()));
		console.log(`[DEBUG] Visited after depth ${i}:`, Array.from(visited));
	}

	const normalizedShares = normalizeShares(shares);
	console.log(
		`[DEBUG] Final normalized shares for ${providerId}:`,
		Object.fromEntries(normalizedShares.entries())
	);
	return normalizedShares;

	// Helper functions
	function calculateInitialShares(): ShareMap {
		const contributors = provider.zipperCurrent.nodeContributors;
		console.log(
			`[DEBUG] ${providerId} has ${contributors.size} contributors:`,
			Array.from(contributors)
		);
		const validContributors: TreeZipper[] = [];

		// Find valid contributors in the forest
		for (const id of contributors) {
			const contributor = forest.get(id);
			if (contributor) {
				validContributors.push(contributor);
				console.log(`[DEBUG] Found valid contributor ${id} in forest`);
			} else {
				console.log(`[DEBUG] Contributor ${id} not found in forest`);
			}
		}

		if (validContributors.length === 0) {
			console.log(`[DEBUG] No valid contributors found for ${providerId}`);
			return new Map();
		}

		// Calculate mutual values
		const mutualValues: [string, number][] = validContributors.map((c) => {
			const id = c.zipperCurrent.nodeId;
			const mutual = mutualFulfillment(forest, provider, c);
			console.log(`[DEBUG] Mutual fulfillment between ${providerId} and ${id}: ${mutual}`);
			return [id, mutual];
		});

		// Calculate total
		const total = mutualValues.reduce((sum, [_, value]) => sum + value, 0);
		console.log(`[DEBUG] Total mutual fulfillment for ${providerId}: ${total}`);

		if (total === 0) {
			console.log(`[DEBUG] Total mutual fulfillment is 0, returning empty share map`);
			return new Map();
		}

		// Create normalized share map
		const shareMap = new Map<string, number>();
		for (const [id, value] of mutualValues) {
			const share = value / total;
			shareMap.set(id, share);
			console.log(`[DEBUG] Share of ${id} from ${providerId}: ${share} (${value}/${total})`);
		}

		return shareMap;
	}

	function processNextDepth(
		currentShares: ShareMap,
		currentVisited: VisitedSet
	): [ShareMap, VisitedSet] {
		const recipients = Array.from(currentShares.keys());
		const unvisitedRecipients = recipients.filter((id) => !currentVisited.has(id));

		let result: ShareMap = new Map(currentShares);
		let newVisited: VisitedSet = new Set(currentVisited);

		for (const recipientId of unvisitedRecipients) {
			const processed = processRecipient(result, newVisited, recipientId);
			result = processed[0];
			newVisited = processed[1];
		}

		return [result, newVisited];
	}

	function processRecipient(
		currentShares: ShareMap,
		currentVisited: VisitedSet,
		recipientId: string
	): [ShareMap, VisitedSet] {
		// Skip if no share or recipient not in forest
		if (!currentShares.has(recipientId)) {
			return [currentShares, currentVisited];
		}

		const recipient = forest.get(recipientId);
		if (!recipient) {
			return [currentShares, currentVisited];
		}

		// Get current share value
		const recipientShare = currentShares.get(recipientId)!;

		// Mark as visited
		const newVisited = new Set(currentVisited);
		newVisited.add(recipientId);

		// Get unvisited connections
		const connections = recipient.zipperCurrent.nodeContributors;
		const unvisitedConnections = Array.from(connections).filter((id) => !newVisited.has(id));

		if (unvisitedConnections.length === 0) {
			return [currentShares, newVisited];
		}

		// Calculate transitive shares
		const transitiveShares = providerShares(forest, recipient, 1);

		// Create weighted shares
		const weightedShares: ShareMap = new Map();
		for (const [id, share] of transitiveShares) {
			weightedShares.set(id, share * recipientShare);
		}

		// Merge with current shares
		const result = new Map(currentShares);
		for (const [id, share] of weightedShares) {
			result.set(id, (result.get(id) || 0) + share);
		}

		return [result, newVisited];
	}

	function normalizeShares(shares: ShareMap): ShareMap {
		const total = Array.from(shares.values()).reduce((sum, share) => sum + share, 0);

		if (total <= 0) return shares;

		const normalized = new Map<string, number>();
		for (const [id, share] of shares) {
			normalized.set(id, share / total);
		}

		return normalized;
	}
}

// Provider-centric share calculation with caching
export function providerSharesCached(
	cache: ProviderSharesCache,
	forest: Forest,
	provider: TreeZipper,
	maxDepth: number
): [ShareMap, ProviderSharesCache] {
	const providerId = provider.zipperCurrent.nodeId;
	const key = `${providerId}_${maxDepth}`;
	const currentCache = cache.sharesCache;

	console.log(`[DEBUG] Looking up cached provider shares for ${providerId} at depth ${maxDepth}`);

	const lookupResult = cacheLookup(key, currentCache);

	if (lookupResult) {
		const [value, updatedCache] = lookupResult;
		const shares = toShareMap(value);

		if (shares !== null) {
			console.log(`[DEBUG] Cache hit for ${providerId} at depth ${maxDepth}`);
			return [shares, { sharesCache: updatedCache }];
		}

		console.log(`[DEBUG] Cache miss for ${providerId} at depth ${maxDepth}, computing...`);
		return computeAndCache(key, currentCache);
	}

	console.log(`[DEBUG] No cache entry for ${providerId} at depth ${maxDepth}, computing...`);
	return computeAndCache(key, currentCache);

	function computeAndCache(
		key: string,
		cacheInstance: typeof currentCache
	): [ShareMap, ProviderSharesCache] {
		const shares = providerShares(forest, provider, maxDepth);
		const newCache = cacheInsert(key, fromShareMap(shares), cacheInstance);
		console.log(
			`[DEBUG] Computed shares for ${providerId} at depth ${maxDepth}:`,
			Object.fromEntries(shares.entries())
		);
		return [shares, { sharesCache: newCache }];
	}
}

// Simplified interface functions that use providerShares
export function directShare(forest: Forest, provider: TreeZipper, recipientId: string): number {
	const shares = providerShares(forest, provider, 1);
	return shares.get(recipientId) || 0;
}

// Get a receiver's share from a specific capacity provider
export function receiverShareFrom(
	forest: Forest,
	receiver: TreeZipper,
	provider: TreeZipper,
	maxDepth: number
): number {
	const providerShareMap = providerShares(forest, provider, maxDepth);
	const receiverId = receiver.zipperCurrent.nodeId;
	return providerShareMap.get(receiverId) || 0;
}
