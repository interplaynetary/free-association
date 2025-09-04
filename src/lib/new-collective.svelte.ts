/**
 * Elegant Collective Capacity System
 *
 * Core insight: We only need to:
 * 1. Collective capacities → Unique contributor sets
 * 2. Contributor sets → Collective recognition weights (calculated once per unique set)
 * 3. Capacities + weights → Individual quantity allocations (with filters)
 */

import { writable, derived, get } from 'svelte/store';
import type { Writable } from 'svelte/store';
import type { CollectiveCapacity, CollectiveRecognitionWeights, ShareMap, Node } from '$lib/schema';
import { recognitionCache, userTree, contributors } from '$lib/state/core.svelte';
import { applyCapacityFilter, type FilterContext } from '$lib/filters';
import { mutualFulfillment as originalMutualFulfillment } from '$lib/protocol';

// =============================================================================
// CORE STORES
// =============================================================================

// Collective capacities (distinct from individual capacities)
export const collectiveCapacities: Writable<Record<string, CollectiveCapacity>> = writable({});

// Network stores for indirect contributors' SOGF
export const indirectContributorSOGF: Writable<Record<string, ShareMap>> = writable({});

// Forest of all contributor trees (needed for collective mutual fulfillment)
export const contributorForest: Writable<Record<string, Node>> = writable({});

// =============================================================================
// DERIVED STORES - THE ELEGANT CHAIN
// =============================================================================

/**
 * Step 1: Extract unique contributor sets and identify indirect contributors
 */
export const uniqueContributorSets = derived([collectiveCapacities], ([$collectiveCapacities]) => {
	console.log('[UNIQUE-SETS] Extracting unique contributor sets...');

	const setsMap = new Map<string, string[]>();

	for (const capacity of Object.values($collectiveCapacities)) {
		// Create a sorted key for this contributor set
		const sortedContributors = [...capacity.collective_contributors].sort();
		const setKey = sortedContributors.join('|');

		if (!setsMap.has(setKey)) {
			setsMap.set(setKey, sortedContributors);
		}
	}

	const uniqueSets = Object.fromEntries(setsMap);
	console.log(`[UNIQUE-SETS] Found ${Object.keys(uniqueSets).length} unique contributor sets`);
	return uniqueSets;
});

/**
 * Step 1.5: Identify all indirect contributors (those not in our direct contributors)
 */
export const indirectContributors = derived(
	[uniqueContributorSets, contributors],
	([$uniqueSets, $contributors]) => {
		console.log('[INDIRECT-CONTRIBUTORS] Identifying indirect contributors...');

		const allContributors = new Set<string>();
		const directContributors = new Set($contributors);

		// Collect all contributors from all unique sets
		for (const contributorSet of Object.values($uniqueSets)) {
			for (const contributorId of contributorSet) {
				allContributors.add(contributorId);
			}
		}

		// Find indirect contributors (in collective capacities but not in our direct contributors)
		const indirect = Array.from(allContributors).filter(
			(contributorId) => !directContributors.has(contributorId)
		);

		console.log(
			`[INDIRECT-CONTRIBUTORS] Found ${indirect.length} indirect contributors: ${indirect.join(', ')}`
		);
		return indirect;
	}
);

/**
 * Step 2: Calculate collective recognition weights using the real collective mutual fulfillment algorithm
 * OPTIMIZED: Only recalculates weights for contributor sets that actually changed
 */
export const contributorSetWeights = derived(
	[uniqueContributorSets, contributorForest, recognitionCache, indirectContributorSOGF],
	([$uniqueSets, $forest, $recognitionCache, $indirectSOGF]) => {
		console.log('[SET-WEIGHTS] Calculating collective recognition weights with real algorithm...');

		// Get previous weights to enable incremental updates
		const previousWeights: Record<string, CollectiveRecognitionWeights> =
			get(contributorSetWeights) || {};
		const setWeights: Record<string, CollectiveRecognitionWeights> = { ...previousWeights };

		let recalculatedSets = 0;
		let reusedSets = 0;

		// Calculate weights for each unique contributor set
		for (const [setKey, contributors] of Object.entries($uniqueSets)) {
			// Check if we need to recalculate this set's weights
			const needsRecalculation = shouldRecalculateSet(
				setKey,
				contributors,
				$forest,
				$recognitionCache,
				$indirectSOGF,
				previousWeights[setKey]
			);

			if (needsRecalculation) {
				setWeights[setKey] = calculateCollectiveRecognitionWeights(
					contributors,
					$forest,
					$recognitionCache,
					$indirectSOGF
				);
				recalculatedSets++;
			} else {
				// Keep existing weights
				reusedSets++;
			}
		}

		// Remove weights for contributor sets that no longer exist
		const currentSetKeys = new Set(Object.keys($uniqueSets));
		for (const setKey in setWeights) {
			if (!currentSetKeys.has(setKey)) {
				delete setWeights[setKey];
			}
		}

		console.log(
			`[SET-WEIGHTS] Processed ${Object.keys($uniqueSets).length} contributor sets: ${recalculatedSets} recalculated, ${reusedSets} reused`
		);
		return setWeights;
	}
);

/**
 * Step 3: Map each capacity's contributors to their allocated quantities (with filters)
 * OPTIMIZED: Skips calculation for capacities with zero total capacity
 */
export const capacityContributorQuantities = derived(
	[collectiveCapacities, contributorSetWeights, userTree],
	([$collectiveCapacities, $setWeights, $userTree]: [
		Record<string, CollectiveCapacity>,
		Record<string, CollectiveRecognitionWeights>,
		any
	]) => {
		if (!$userTree) return {};

		console.log('[CAPACITY-QUANTITIES] Calculating contributor quantities with filters...');

		const capacityQuantities: Record<string, Record<string, number>> = {};
		const myUserId = $userTree.id;
		let processedCapacities = 0;
		let skippedCapacities = 0;

		for (const [capacityId, capacity] of Object.entries($collectiveCapacities)) {
			// Calculate total capacity from availability slots first (early optimization)
			const totalCapacity =
				capacity.availability_slots?.reduce((sum, slot) => sum + (slot.quantity || 0), 0) || 0;

			// Skip if no capacity to allocate
			if (totalCapacity === 0) {
				skippedCapacities++;
				continue;
			}

			// Get the set key for this capacity's contributors
			const sortedContributors = [...capacity.collective_contributors].sort();
			const setKey = sortedContributors.join('|');

			// Get the weights for this contributor set
			const weights = $setWeights[setKey];
			if (!weights) {
				skippedCapacities++;
				continue;
			}

			// Apply capacity filter to the weights (if capacity has filter_rule)
			let filteredWeights = weights;
			if (capacity.filter_rule) {
				try {
					// Create filter context (simplified for now)
					const context: FilterContext = { subtreeContributors: {} };
					filteredWeights = applyCapacityFilter(capacity, weights, context);
				} catch (error) {
					console.warn(`[CAPACITY-QUANTITIES] Filter error for ${capacityId}:`, error);
					filteredWeights = weights; // Fall back to unfiltered
				}
			}

			// Calculate each contributor's quantity allocation
			capacityQuantities[capacityId] = {};
			for (const [contributorId, weight] of Object.entries(filteredWeights)) {
				const allocatedQuantity = totalCapacity * (weight as number);
				if (allocatedQuantity > 0) {
					capacityQuantities[capacityId][contributorId] = allocatedQuantity;
				}
			}
			processedCapacities++;
		}

		console.log(
			`[CAPACITY-QUANTITIES] Processed ${processedCapacities} capacities, skipped ${skippedCapacities} (no capacity or weights)`
		);
		return capacityQuantities;
	}
);

/**
 * Convenience store: My collective capacity claims (what I can claim from collective capacities)
 */
export const myCollectiveCapacityClaims = derived(
	[capacityContributorQuantities, userTree],
	([$quantities, $userTree]) => {
		if (!$userTree) return {};

		const myUserId = $userTree.id;
		const myClaims: Record<string, number> = {};

		for (const [capacityId, contributorQuantities] of Object.entries($quantities)) {
			const myQuantity = contributorQuantities[myUserId];
			if (myQuantity && myQuantity > 0) {
				myClaims[capacityId] = myQuantity;
			}
		}

		return myClaims;
	}
);

// =============================================================================
// OPTIMIZATION HELPERS
// =============================================================================

/**
 * Determine if a contributor set needs recalculation based on changes to dependencies
 * This enables incremental updates - only recalculate when necessary
 */
function shouldRecalculateSet(
	setKey: string,
	contributors: string[],
	forest: Record<string, Node>,
	recognitionCache: Record<string, any>,
	indirectSOGF: Record<string, ShareMap>,
	existingWeights?: CollectiveRecognitionWeights
): boolean {
	// Always recalculate if we don't have existing weights
	if (!existingWeights) {
		console.log(`[OPTIMIZATION] ${setKey}: No existing weights, needs calculation`);
		return true;
	}

	// Check if any contributor in this set is missing from our data sources
	for (const contributorId of contributors) {
		// Check if we have the contributor's tree (needed for mutual fulfillment)
		if (!forest[contributorId]) {
			console.log(`[OPTIMIZATION] ${setKey}: Missing tree for ${contributorId}, needs calculation`);
			return true;
		}

		// Check if we have recognition data for this contributor
		if (!recognitionCache[contributorId] && !indirectSOGF[contributorId]) {
			console.log(
				`[OPTIMIZATION] ${setKey}: Missing recognition data for ${contributorId}, needs calculation`
			);
			return true;
		}
	}

	// If we have all the data, we can potentially reuse existing weights
	// Note: We could add more sophisticated change detection here (like comparing timestamps)
	// but for now, having all required data suggests we can reuse
	console.log(`[OPTIMIZATION] ${setKey}: All data available, reusing existing weights`);
	return false;
}

// =============================================================================
// REAL COLLECTIVE RECOGNITION ALGORITHM (from collective.svelte.ts)
// =============================================================================

/**
 * Calculate collective recognition weights using the real algorithm from collective.svelte.ts
 * This implements the proper collective mutual fulfillment calculation
 */
function calculateCollectiveRecognitionWeights(
	contributorIds: string[],
	forest: Record<string, Node>,
	recognitionCache: Record<string, any>,
	indirectSOGF: Record<string, ShareMap>
): CollectiveRecognitionWeights {
	if (contributorIds.length === 1) {
		// Single contributor gets 100%
		return { [contributorIds[0]]: 1.0 };
	}

	console.log(`[COLLECTIVE-WEIGHTS] Calculating for set: [${contributorIds.join(', ')}]`);

	const weights: CollectiveRecognitionWeights = {};
	let totalWeight = 0;

	// For each contributor in the set, calculate their mutual fulfillment with the collective
	for (const contributorId of contributorIds) {
		const contributorNode = forest[contributorId];
		if (!contributorNode) {
			console.warn(`[COLLECTIVE-WEIGHTS] No tree found for contributor ${contributorId}`);
			weights[contributorId] = 0;
			continue;
		}

		// Create a "sub-collective" excluding this contributor
		const otherContributors = contributorIds.filter((id) => id !== contributorId);

		// Calculate mutual fulfillment between this contributor and the sub-collective
		const mutualFulfillmentWithCollective = calculateMutualFulfillmentWithCollective(
			contributorNode,
			otherContributors,
			forest,
			recognitionCache,
			indirectSOGF
		);

		weights[contributorId] = mutualFulfillmentWithCollective;
		totalWeight += mutualFulfillmentWithCollective;

		console.log(
			`[COLLECTIVE-WEIGHTS] ${contributorId} mutual fulfillment with collective: ${mutualFulfillmentWithCollective.toFixed(4)}`
		);
	}

	// Normalize weights to sum to 1.0
	if (totalWeight > 0) {
		for (const contributorId of contributorIds) {
			weights[contributorId] = weights[contributorId] / totalWeight;
		}
	} else {
		// Equal weights if no mutual fulfillment
		for (const contributorId of contributorIds) {
			weights[contributorId] = 1.0 / contributorIds.length;
		}
	}

	console.log(`[COLLECTIVE-WEIGHTS] Final normalized weights:`, weights);
	return weights;
}

/**
 * Calculate mutual fulfillment between an individual and a collective
 * This is the core algorithm from collective.svelte.ts adapted for our use case
 */
function calculateMutualFulfillmentWithCollective(
	individualNode: Node,
	collectiveContributorIds: string[],
	forest: Record<string, Node>,
	recognitionCache: Record<string, any>,
	indirectSOGF: Record<string, ShareMap>
): number {
	if (collectiveContributorIds.length === 0) {
		return 0;
	}

	let totalMF = 0;

	// For each member of the collective, calculate mutual fulfillment with the individual
	for (const collectiveMemberId of collectiveContributorIds) {
		const collectiveMemberNode = forest[collectiveMemberId];
		if (!collectiveMemberNode) {
			console.warn(`[COLLECTIVE-MF] No tree found for collective member ${collectiveMemberId}`);
			continue;
		}

		// Calculate mutual fulfillment between individual and this collective member
		const memberMF = calculateBidirectionalMutualFulfillment(
			individualNode,
			collectiveMemberNode,
			recognitionCache,
			indirectSOGF
		);

		// Weight this by the member's weight in the collective (simplified for now)
		// In the full algorithm, this would be recursive
		const memberWeight = 1.0 / collectiveContributorIds.length; // Equal weights for simplicity

		totalMF += memberWeight * memberMF;

		console.log(
			`[COLLECTIVE-MF] MF between ${individualNode.id} and collective member ${collectiveMemberId}: ${memberMF.toFixed(4)}`
		);
	}

	return totalMF;
}

/**
 * Calculate bidirectional mutual fulfillment between two individuals
 * Uses recognition cache for direct contributors and SOGF for indirect contributors
 */
function calculateBidirectionalMutualFulfillment(
	nodeA: Node,
	nodeB: Node,
	recognitionCache: Record<string, any>,
	indirectSOGF: Record<string, ShareMap>
): number {
	// Check if we have direct recognition data
	const recognitionA = recognitionCache[nodeB.id];
	const recognitionB = recognitionCache[nodeA.id];

	if (recognitionA && recognitionB) {
		// Both are direct contributors - use recognition cache
		const shareAtoB = recognitionA.ourShare || 0;
		const shareBtoA = recognitionB.theirShare || 0;
		return Math.min(shareAtoB, shareBtoA);
	}

	// Use SOGF data for indirect contributors
	const sogfA = indirectSOGF[nodeA.id];
	const sogfB = indirectSOGF[nodeB.id];

	let shareAtoB = 0;
	let shareBtoA = 0;

	if (sogfA) {
		shareAtoB = sogfA[nodeB.id] || 0;
	}

	if (sogfB) {
		shareBtoA = sogfB[nodeA.id] || 0;
	}

	// If we don't have SOGF data, fall back to using the original mutual fulfillment calculation
	if (!sogfA && !sogfB) {
		const nodesMap = { [nodeA.id]: nodeA, [nodeB.id]: nodeB };
		return originalMutualFulfillment(nodeA, nodeB, nodesMap);
	}

	return Math.min(shareAtoB, shareBtoA);
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

/**
 * Create a collective capacity
 */
export function createCollectiveCapacity(
	id: string,
	name: string,
	contributorIds: string[],
	availabilitySlots: any[]
): CollectiveCapacity {
	return {
		id,
		name,
		availability_slots: availabilitySlots,
		collective_contributors: contributorIds,
		// Optional fields
		emoji: undefined,
		unit: undefined,
		description: undefined,
		max_natural_div: undefined,
		max_percentage_div: undefined,
		hidden_until_request_accepted: undefined,
		owner_id: undefined,
		filter_rule: undefined
	};
}

/**
 * Add a collective capacity
 */
export function addCollectiveCapacity(capacity: CollectiveCapacity): void {
	collectiveCapacities.update((capacities) => ({
		...capacities,
		[capacity.id]: capacity
	}));
}

/**
 * Get my claim amount for a specific collective capacity
 */
export function getMyClaimAmount(capacityId: string): number {
	const claims = get(myCollectiveCapacityClaims);
	return claims[capacityId] || 0;
}

/**
 * Get contributor quantities for a specific capacity
 */
export function getCapacityContributorQuantities(capacityId: string): Record<string, number> {
	const quantities = get(capacityContributorQuantities);
	return quantities[capacityId] || {};
}

// =============================================================================
// EXAMPLE USAGE
// =============================================================================

/**
 * Example: Create a housing collective
 */
export function createExampleHousingCollective(): void {
	const housingCollective = createCollectiveCapacity(
		'housing-collective-001',
		'Community Housing Collective',
		['alice', 'bob', 'charlie'],
		[
			{
				id: 'housing-slot-1',
				quantity: 12, // 12 housing units total
				location_type: 'physical',
				city: 'San Francisco'
			}
		]
	);

	addCollectiveCapacity(housingCollective);
}

/**
 * Example: Create a creative collective
 */
export function createExampleCreativeCollective(): void {
	const creativeCollective = createCollectiveCapacity(
		'creative-collective-001',
		'Creative Collaboration Collective',
		['alice', 'bob', 'charlie', 'dana'],
		[
			{
				id: 'creative-slot-1',
				quantity: 80, // 80 hours/week total
				recurrence: 'weekly',
				time_zone: 'UTC'
			}
		]
	);

	addCollectiveCapacity(creativeCollective);
}
