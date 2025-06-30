import { writable, derived } from 'svelte/store';
import type { UserComposition, NetworkComposition } from '$lib/schema';
import {
	userCapacities,
	userNetworkCapacitiesWithShares,
	contributorCapacityShares,
	networkCapacities
} from '$lib/state/core.svelte';

// COMPOSE-FROM MODEL:
// mapping our capacityIds to the capacityIds of our userNetworkCapacitiesWithShares that we want to compose from
// userDesiredComposeFrom represents: "I want to composeInto MY capacity FROM THEIR capacity (via the share I have in their capacity)"
// Structure: myCapacityId → theirCapacityId → desiredAbsoluteUnits
// Example: "cooking-skill" → "ingredients-supply" → 25 means:
//   "I want to compose 25 units FROM their ingredients supply INTO my cooking skill"
//   Note: This only works because I already have some share percentage in their ingredients capacity
export const userDesiredComposeFrom = writable<UserComposition>({});

// COMPOSE-INTO MODEL (opposite direction):
// userDesiredComposeInto represents: "I want to composeInto THEIR capacity using MY capacity (via the share they have in my capacity)"
// Structure: myCapacityId → theirCapacityId → desiredAbsoluteUnits
// Example: "cooking-skill" → "meal-service" → 30 means:
//   "I want to compose 30 units of my cooking skill INTO their meal service"
//   Note: This only works because they already have some share percentage in my cooking capacity
export const userDesiredComposeInto = writable<UserComposition>({});

// NETWORK COMPOSE-FROM MODEL:
// networkDesiredComposeFrom represents what others want to composeInto via their shares
// Structure: userId → theirCapacityId → ourCapacityId → desiredAbsoluteUnits
// Example: "alice" → "ingredients-supply" → "cooking-skill" → 20 means:
//   "Alice wants to compose 20 units FROM our cooking skill INTO her ingredients supply"
//   Note: This only works because Alice already has some share percentage in our cooking capacity
export const networkDesiredComposeFrom = writable<NetworkComposition>({});

// NETWORK COMPOSE-INTO MODEL (opposite direction):
// networkDesiredComposeInto represents what others want to compose into our capacities via our shares
// Structure: userId → theirCapacityId → ourCapacityId → desiredAbsoluteUnits
// Example: "alice" → "ingredients-supply" → "cooking-skill" → 15 means:
//   "Alice wants to compose 15 units of her ingredients INTO our cooking skill"
//   Note: This only works because we already have some share percentage in Alice's ingredients capacity
export const networkDesiredComposeInto = writable<NetworkComposition>({});

// FEASIBLE COMPOSE-FROM: Constrains raw desires by share access and resource allocation
// Step 1: Convert absolute unit desires to share-constrained feasible units
// Step 2: Apply resource allocation constraints when total desires exceed available resources
export const feasibleComposeFrom = derived(
	[userDesiredComposeFrom, userNetworkCapacitiesWithShares],
	([$userDesiredComposeFrom, $userNetworkCapacitiesWithShares]) => {
		console.log(
			'[FEASIBLE-COMPOSE-FROM] Calculating feasible compose-from with absolute units and resource allocation...'
		);

		// Step 1: Individual share-based feasibility check
		const shareConstrainedDesires: UserComposition = {};

		Object.entries($userDesiredComposeFrom).forEach(([ourCapacityId, ourDesires]) => {
			const constrainedDesires: Record<string, number> = {};

			Object.entries(ourDesires).forEach(([theirCapacityId, desiredAbsoluteUnits]) => {
				const networkCapacity = $userNetworkCapacitiesWithShares[theirCapacityId];

				if (!networkCapacity) {
					console.warn(`[FEASIBLE-COMPOSE-FROM] No network capacity found for: ${theirCapacityId}`);
					return;
				}

				// Get our available units from this capacity (computed_quantity is already share-adjusted)
				const ourAvailableUnits = (networkCapacity as any).computed_quantity || 0;
				const shareConstrainedUnits = Math.min(desiredAbsoluteUnits, ourAvailableUnits);

				if (shareConstrainedUnits > 0) {
					constrainedDesires[theirCapacityId] = shareConstrainedUnits;

					if (shareConstrainedUnits < desiredAbsoluteUnits) {
						console.log(
							`[FEASIBLE-COMPOSE-FROM] Share-constrained ${ourCapacityId} → ${theirCapacityId}: desired ${desiredAbsoluteUnits} → ${shareConstrainedUnits} units (limited by available ${ourAvailableUnits} units)`
						);
					}
				}
			});

			if (Object.keys(constrainedDesires).length > 0) {
				shareConstrainedDesires[ourCapacityId] = constrainedDesires;
			}
		});

		// Step 2: Resource allocation constraints - group by source capacity
		const sourceCapacityDemands: Record<
			string,
			{
				totalDemand: number;
				available: number;
				consumers: Array<{ ourCapacityId: string; desiredUnits: number }>;
			}
		> = {};

		// Group all desires by source capacity
		Object.entries(shareConstrainedDesires).forEach(([ourCapacityId, ourDesires]) => {
			Object.entries(ourDesires).forEach(([theirCapacityId, desiredUnits]) => {
				if (!sourceCapacityDemands[theirCapacityId]) {
					const networkCapacity = $userNetworkCapacitiesWithShares[theirCapacityId];
					const availableUnits = (networkCapacity as any)?.computed_quantity || 0;

					sourceCapacityDemands[theirCapacityId] = {
						totalDemand: 0,
						available: availableUnits,
						consumers: []
					};
				}

				sourceCapacityDemands[theirCapacityId].totalDemand += desiredUnits;
				sourceCapacityDemands[theirCapacityId].consumers.push({
					ourCapacityId,
					desiredUnits
				});
			});
		});

		// Step 3: Apply proportional scaling when total demand exceeds available resources
		const finalFeasible: UserComposition = {};

		Object.entries(sourceCapacityDemands).forEach(([theirCapacityId, demandInfo]) => {
			const { totalDemand, available, consumers } = demandInfo;

			let scalingFactor = 1.0;
			if (totalDemand > available) {
				scalingFactor = available / totalDemand;
				console.log(
					`[FEASIBLE-COMPOSE-FROM] Resource allocation scaling for ${theirCapacityId}: ${totalDemand} demanded > ${available} available, scaling by ${scalingFactor.toFixed(3)}`
				);
			}

			// Apply scaling to each consumer
			consumers.forEach(({ ourCapacityId, desiredUnits }) => {
				const finalFeasibleUnits = desiredUnits * scalingFactor;

				if (!finalFeasible[ourCapacityId]) {
					finalFeasible[ourCapacityId] = {};
				}
				finalFeasible[ourCapacityId][theirCapacityId] = finalFeasibleUnits;

				if (scalingFactor < 1.0) {
					console.log(
						`[FEASIBLE-COMPOSE-FROM] Resource-allocated ${ourCapacityId} → ${theirCapacityId}: ${desiredUnits} → ${finalFeasibleUnits.toFixed(1)} units`
					);
				}
			});
		});

		console.log(
			`[FEASIBLE-COMPOSE-FROM] Generated resource-allocated feasible compositions for ${Object.keys(finalFeasible).length} capacities`
		);
		return finalFeasible;
	}
);

// FEASIBLE COMPOSE-INTO: Constrains raw desires by recipient share and our capacity limits
// Step 1: Convert absolute unit desires to recipient-share-constrained feasible units
// Step 2: Apply resource allocation constraints when total desires exceed our capacity
export const feasibleComposeInto = derived(
	[userDesiredComposeInto, contributorCapacityShares, networkCapacities, userCapacities],
	([$userDesiredComposeInto, $contributorCapacityShares, $networkCapacities, $userCapacities]) => {
		console.log(
			'[FEASIBLE-COMPOSE-INTO] Calculating feasible compose-into with absolute units and resource allocation...'
		);

		// Step 1: Individual recipient-share-based feasibility check
		const shareConstrainedDesires: UserComposition = {};

		Object.entries($userDesiredComposeInto).forEach(([ourCapacityId, ourDesires]) => {
			const constrainedDesires: Record<string, number> = {};

			Object.entries(ourDesires).forEach(([theirCapacityId, desiredAbsoluteUnits]) => {
				// Find who owns the target capacity
				const providerId = Object.keys($networkCapacities).find(
					(id) => $networkCapacities[id] && $networkCapacities[id][theirCapacityId]
				);

				if (!providerId) {
					console.warn(
						`[FEASIBLE-COMPOSE-INTO] No provider found for capacity: ${theirCapacityId}`
					);
					return;
				}

				// Get our capacity info to calculate their share-based limit
				const ourCapacity = $userCapacities?.[ourCapacityId];
				if (!ourCapacity) {
					console.warn(`[FEASIBLE-COMPOSE-INTO] Our capacity not found: ${ourCapacityId}`);
					return;
				}

				// Check how much share they have in our capacity (this constrains how much they can receive)
				const theirShareInOurCapacity =
					$contributorCapacityShares[providerId]?.[ourCapacityId] || 0;
				const ourCapacityQuantity = ourCapacity.quantity || 0;
				const maxUnitsTheyCanReceive = ourCapacityQuantity * theirShareInOurCapacity;

				const shareConstrainedUnits = Math.min(desiredAbsoluteUnits, maxUnitsTheyCanReceive);

				if (shareConstrainedUnits > 0) {
					constrainedDesires[theirCapacityId] = shareConstrainedUnits;

					if (shareConstrainedUnits < desiredAbsoluteUnits) {
						console.log(
							`[FEASIBLE-COMPOSE-INTO] Share-constrained ${ourCapacityId} → ${theirCapacityId}: desired ${desiredAbsoluteUnits} → ${shareConstrainedUnits} units (limited by ${(theirShareInOurCapacity * 100).toFixed(1)}% recipient share = ${maxUnitsTheyCanReceive} max units)`
						);
					}
				}
			});

			if (Object.keys(constrainedDesires).length > 0) {
				shareConstrainedDesires[ourCapacityId] = constrainedDesires;
			}
		});

		// Step 2: Resource allocation constraints - group by our source capacity
		const ourCapacityDemands: Record<
			string,
			{
				totalDemand: number;
				available: number;
				recipients: Array<{ theirCapacityId: string; desiredUnits: number }>;
			}
		> = {};

		// Group all desires by our source capacity
		Object.entries(shareConstrainedDesires).forEach(([ourCapacityId, ourDesires]) => {
			Object.entries(ourDesires).forEach(([theirCapacityId, desiredUnits]) => {
				if (!ourCapacityDemands[ourCapacityId]) {
					const ourCapacity = $userCapacities?.[ourCapacityId];
					const availableUnits = ourCapacity?.quantity || 0;

					ourCapacityDemands[ourCapacityId] = {
						totalDemand: 0,
						available: availableUnits,
						recipients: []
					};
				}

				ourCapacityDemands[ourCapacityId].totalDemand += desiredUnits;
				ourCapacityDemands[ourCapacityId].recipients.push({
					theirCapacityId,
					desiredUnits
				});
			});
		});

		// Step 3: Apply proportional scaling when total demand exceeds our capacity
		const finalFeasible: UserComposition = {};

		Object.entries(ourCapacityDemands).forEach(([ourCapacityId, demandInfo]) => {
			const { totalDemand, available, recipients } = demandInfo;

			let scalingFactor = 1.0;
			if (totalDemand > available) {
				scalingFactor = available / totalDemand;
				console.log(
					`[FEASIBLE-COMPOSE-INTO] Resource allocation scaling for our ${ourCapacityId}: ${totalDemand} demanded > ${available} available, scaling by ${scalingFactor.toFixed(3)}`
				);
			}

			// Apply scaling to each recipient
			recipients.forEach(({ theirCapacityId, desiredUnits }) => {
				const finalFeasibleUnits = desiredUnits * scalingFactor;

				if (!finalFeasible[ourCapacityId]) {
					finalFeasible[ourCapacityId] = {};
				}
				finalFeasible[ourCapacityId][theirCapacityId] = finalFeasibleUnits;

				if (scalingFactor < 1.0) {
					console.log(
						`[FEASIBLE-COMPOSE-INTO] Resource-allocated ${ourCapacityId} → ${theirCapacityId}: ${desiredUnits} → ${finalFeasibleUnits.toFixed(1)} units`
					);
				}
			});
		});

		console.log(
			`[FEASIBLE-COMPOSE-INTO] Generated resource-allocated feasible compositions for ${Object.keys(finalFeasible).length} capacities`
		);
		return finalFeasible;
	}
);

// Helper function to calculate how well two desired amounts align (absolute units)
function calculateDesireAlignment(ourDesire: number, theirDesire: number): number {
	if (ourDesire === 0 || theirDesire === 0) return 0;

	// Calculate the ratio between desires - works the same for absolute units
	const ratio = Math.min(ourDesire, theirDesire) / Math.max(ourDesire, theirDesire);

	// Return a score between 0 and 1 where 1 = perfect alignment
	return ratio;
}

// STEP 1: MUTUAL DESIRE (Aspirational - based on raw desires)
// TYPE 1: Mutual Desire for Our Capacities
// Both we and they want to enhance our capacity using their capacity (before constraints)
export const mutualDesireOurCapacities = derived(
	[userDesiredComposeFrom, networkDesiredComposeInto, userNetworkCapacitiesWithShares],
	([$userDesiredComposeFrom, $networkDesiredComposeInto, $userNetworkCapacitiesWithShares]) => {
		console.log('[MUTUAL-DESIRE-OURS] Calculating mutual desires for our capacities...');

		if (!$userDesiredComposeFrom || !$networkDesiredComposeInto) {
			console.log('[MUTUAL-DESIRE-OURS] No composition data available');
			return {};
		}

		const mutualDesires: Record<
			string,
			{
				ourCapacityId: string;
				theirCapacityId: string;
				ourDesiredAmount: number;
				theirDesiredAmount: number;
				providerId: string;
				desireViability: number;
			}
		> = {};

		Object.entries($userDesiredComposeFrom).forEach(([ourCapacityId, ourDesires]) => {
			Object.entries(ourDesires).forEach(([theirCapacityId, ourDesiredAmount]) => {
				const networkCapacity = $userNetworkCapacitiesWithShares[theirCapacityId];
				const providerId = (networkCapacity as any)?.provider_id;

				if (!providerId) {
					console.warn(`[MUTUAL-DESIRE-OURS] No provider found for capacity: ${theirCapacityId}`);
					return;
				}

				// Check if they also want to compose their capacity into our capacity
				const theirDesireForOurCapacity =
					$networkDesiredComposeInto[providerId]?.[theirCapacityId]?.[ourCapacityId];
				if (!theirDesireForOurCapacity) return;

				const desireViability = calculateDesireAlignment(
					ourDesiredAmount,
					theirDesireForOurCapacity
				);

				const compositionKey = `${ourCapacityId}:${theirCapacityId}:${providerId}`;
				mutualDesires[compositionKey] = {
					ourCapacityId,
					theirCapacityId,
					ourDesiredAmount,
					theirDesiredAmount: theirDesireForOurCapacity,
					providerId,
					desireViability
				};

				console.log(
					`[MUTUAL-DESIRE-OURS] Found mutual desire: ${compositionKey} (desire viability: ${desireViability.toFixed(2)})`
				);
			});
		});

		console.log(
			`[MUTUAL-DESIRE-OURS] Found ${Object.keys(mutualDesires).length} mutual desires for our capacities`
		);
		return mutualDesires;
	}
);

// TYPE 2: Mutual Desire for Their Capacities
// Both we and they want to enhance their capacity using our capacity (before constraints)
export const mutualDesireTheirCapacities = derived(
	[userDesiredComposeInto, networkDesiredComposeFrom, networkCapacities],
	([$userDesiredComposeInto, $networkDesiredComposeFrom, $networkCapacities]) => {
		console.log('[MUTUAL-DESIRE-THEIRS] Calculating mutual desires for their capacities...');

		if (!$userDesiredComposeInto || !$networkDesiredComposeFrom) {
			console.log('[MUTUAL-DESIRE-THEIRS] No composition data available');
			return {};
		}

		const mutualDesires: Record<
			string,
			{
				ourCapacityId: string;
				theirCapacityId: string;
				ourDesiredAmount: number;
				theirDesiredAmount: number;
				providerId: string;
				desireViability: number;
			}
		> = {};

		Object.entries($userDesiredComposeInto).forEach(([ourCapacityId, ourDesires]) => {
			Object.entries(ourDesires).forEach(([theirCapacityId, ourDesiredAmount]) => {
				// Find the provider who owns this capacity
				const providerId = Object.keys($networkCapacities).find(
					(id) => $networkCapacities[id] && $networkCapacities[id][theirCapacityId]
				);

				if (!providerId) {
					console.warn(`[MUTUAL-DESIRE-THEIRS] No provider found for capacity: ${theirCapacityId}`);
					return;
				}

				// Check if they also want to compose our capacity into their capacity
				const theirDesireForOurCapacity =
					$networkDesiredComposeFrom[providerId]?.[theirCapacityId]?.[ourCapacityId];
				if (!theirDesireForOurCapacity) return;

				const desireViability = calculateDesireAlignment(
					ourDesiredAmount,
					theirDesireForOurCapacity
				);

				const compositionKey = `${ourCapacityId}:${theirCapacityId}:${providerId}`;
				mutualDesires[compositionKey] = {
					ourCapacityId,
					theirCapacityId,
					ourDesiredAmount,
					theirDesiredAmount: theirDesireForOurCapacity,
					providerId,
					desireViability
				};

				console.log(
					`[MUTUAL-DESIRE-THEIRS] Found mutual desire: ${compositionKey} (desire viability: ${desireViability.toFixed(2)})`
				);
			});
		});

		console.log(
			`[MUTUAL-DESIRE-THEIRS] Found ${Object.keys(mutualDesires).length} mutual desires for their capacities`
		);
		return mutualDesires;
	}
);

// STEP 2: MUTUAL FEASIBLE (Realistic - constrained by shares)
// TYPE 1: Mutual Feasible Enhancement of Our Capacities
// Takes mutual desires and constrains them by actual share access
export const mutualFeasibleOurCapacities = derived(
	[mutualDesireOurCapacities, feasibleComposeFrom],
	([$mutualDesireOurCapacities, $feasibleComposeFrom]) => {
		console.log(
			'[MUTUAL-FEASIBLE-OURS] Calculating feasible mutual enhancements of our capacities...'
		);

		const mutualFeasible: Record<
			string,
			{
				ourCapacityId: string;
				theirCapacityId: string;
				ourDesiredAmount: number;
				theirDesiredAmount: number;
				ourFeasibleAmount: number;
				providerId: string;
				desireViability: number;
				feasibleViability: number;
				constraintRatio: number;
			}
		> = {};

		Object.entries($mutualDesireOurCapacities).forEach(([compositionKey, mutualDesire]) => {
			const { ourCapacityId, theirCapacityId } = mutualDesire;

			// Get our feasible amount (constrained by our shares)
			const ourFeasibleAmount = $feasibleComposeFrom[ourCapacityId]?.[theirCapacityId] || 0;

			if (ourFeasibleAmount > 0) {
				const feasibleViability = calculateDesireAlignment(
					ourFeasibleAmount,
					mutualDesire.theirDesiredAmount
				);

				const constraintRatio = ourFeasibleAmount / mutualDesire.ourDesiredAmount;

				mutualFeasible[compositionKey] = {
					...mutualDesire,
					ourFeasibleAmount,
					feasibleViability,
					constraintRatio
				};

				console.log(
					`[MUTUAL-FEASIBLE-OURS] ${compositionKey}: desired ${mutualDesire.ourDesiredAmount.toFixed(3)} → feasible ${ourFeasibleAmount.toFixed(3)} (${(constraintRatio * 100).toFixed(1)}% achievable)`
				);
			}
		});

		console.log(
			`[MUTUAL-FEASIBLE-OURS] Found ${Object.keys(mutualFeasible).length} feasible mutual enhancements of our capacities`
		);
		return mutualFeasible;
	}
);

// TYPE 2: Mutual Feasible Enhancement of Their Capacities
// Takes mutual desires and constrains them by recipient share access
export const mutualFeasibleTheirCapacities = derived(
	[mutualDesireTheirCapacities, feasibleComposeInto],
	([$mutualDesireTheirCapacities, $feasibleComposeInto]) => {
		console.log(
			'[MUTUAL-FEASIBLE-THEIRS] Calculating feasible mutual enhancements of their capacities...'
		);

		const mutualFeasible: Record<
			string,
			{
				ourCapacityId: string;
				theirCapacityId: string;
				ourDesiredAmount: number;
				theirDesiredAmount: number;
				ourFeasibleAmount: number;
				providerId: string;
				desireViability: number;
				feasibleViability: number;
				constraintRatio: number;
			}
		> = {};

		Object.entries($mutualDesireTheirCapacities).forEach(([compositionKey, mutualDesire]) => {
			const { ourCapacityId, theirCapacityId } = mutualDesire;

			// Get our feasible amount (constrained by their shares in our capacity)
			const ourFeasibleAmount = $feasibleComposeInto[ourCapacityId]?.[theirCapacityId] || 0;

			if (ourFeasibleAmount > 0) {
				const feasibleViability = calculateDesireAlignment(
					ourFeasibleAmount,
					mutualDesire.theirDesiredAmount
				);

				const constraintRatio = ourFeasibleAmount / mutualDesire.ourDesiredAmount;

				mutualFeasible[compositionKey] = {
					...mutualDesire,
					ourFeasibleAmount,
					feasibleViability,
					constraintRatio
				};

				console.log(
					`[MUTUAL-FEASIBLE-THEIRS] ${compositionKey}: desired ${mutualDesire.ourDesiredAmount.toFixed(3)} → feasible ${ourFeasibleAmount.toFixed(3)} (${(constraintRatio * 100).toFixed(1)}% achievable)`
				);
			}
		});

		console.log(
			`[MUTUAL-FEASIBLE-THEIRS] Found ${Object.keys(mutualFeasible).length} feasible mutual enhancements of their capacities`
		);
		return mutualFeasible;
	}
);

// =============================================================================
// CONSTRAINT METADATA STORES
// =============================================================================

// Constraint metadata interface
interface ConstraintMetadata {
	feasibleAmount: number;
	constraintType: 'share_limit' | 'resource_competition' | 'no_constraint';
	scalingFactor: number;
	shareConstrainedAmount: number;
	availableAmount: number;
	totalDemandOnSource: number;
	competitorCount: number;
}

// COMPOSE-FROM CONSTRAINT METADATA
// Tracks detailed constraint information for each compose-from relationship
export const feasibleComposeFromMetadata = derived(
	[userDesiredComposeFrom, userNetworkCapacitiesWithShares],
	([$userDesiredComposeFrom, $userNetworkCapacitiesWithShares]) => {
		console.log('[FEASIBLE-METADATA-FROM] Calculating constraint metadata for compose-from...');

		const metadata: Record<string, Record<string, ConstraintMetadata>> = {};

		// Step 1: Calculate share-constrained amounts
		const shareConstrainedDesires: Record<string, Record<string, number>> = {};

		Object.entries($userDesiredComposeFrom).forEach(([ourCapacityId, ourDesires]) => {
			Object.entries(ourDesires).forEach(([theirCapacityId, desiredAbsoluteUnits]) => {
				const networkCapacity = $userNetworkCapacitiesWithShares[theirCapacityId];
				if (!networkCapacity) return;

				const ourAvailableUnits = (networkCapacity as any).computed_quantity || 0;
				const shareConstrainedUnits = Math.min(desiredAbsoluteUnits, ourAvailableUnits);

				if (!shareConstrainedDesires[ourCapacityId]) {
					shareConstrainedDesires[ourCapacityId] = {};
				}
				shareConstrainedDesires[ourCapacityId][theirCapacityId] = shareConstrainedUnits;
			});
		});

		// Step 2: Calculate resource allocation demands
		const sourceCapacityDemands: Record<
			string,
			{
				totalDemand: number;
				available: number;
				consumers: Array<{ ourCapacityId: string; desiredUnits: number }>;
			}
		> = {};

		Object.entries(shareConstrainedDesires).forEach(([ourCapacityId, ourDesires]) => {
			Object.entries(ourDesires).forEach(([theirCapacityId, desiredUnits]) => {
				if (!sourceCapacityDemands[theirCapacityId]) {
					const networkCapacity = $userNetworkCapacitiesWithShares[theirCapacityId];
					const availableUnits = (networkCapacity as any)?.computed_quantity || 0;

					sourceCapacityDemands[theirCapacityId] = {
						totalDemand: 0,
						available: availableUnits,
						consumers: []
					};
				}

				sourceCapacityDemands[theirCapacityId].totalDemand += desiredUnits;
				sourceCapacityDemands[theirCapacityId].consumers.push({
					ourCapacityId,
					desiredUnits
				});
			});
		});

		// Step 3: Generate metadata for each composition
		Object.entries(sourceCapacityDemands).forEach(([theirCapacityId, demandInfo]) => {
			const { totalDemand, available, consumers } = demandInfo;
			const scalingFactor = totalDemand > available ? available / totalDemand : 1.0;
			const competitorCount = consumers.length;

			consumers.forEach(({ ourCapacityId, desiredUnits }) => {
				const finalFeasibleUnits = desiredUnits * scalingFactor;
				const shareConstrainedAmount = desiredUnits;

				// Determine constraint type
				let constraintType: 'share_limit' | 'resource_competition' | 'no_constraint';
				if (scalingFactor < 1.0) {
					constraintType = 'resource_competition';
				} else if (
					shareConstrainedAmount < ($userDesiredComposeFrom[ourCapacityId]?.[theirCapacityId] || 0)
				) {
					constraintType = 'share_limit';
				} else {
					constraintType = 'no_constraint';
				}

				if (!metadata[ourCapacityId]) {
					metadata[ourCapacityId] = {};
				}

				metadata[ourCapacityId][theirCapacityId] = {
					feasibleAmount: finalFeasibleUnits,
					constraintType,
					scalingFactor,
					shareConstrainedAmount,
					availableAmount: available,
					totalDemandOnSource: totalDemand,
					competitorCount
				};
			});
		});

		console.log(
			`[FEASIBLE-METADATA-FROM] Generated metadata for ${Object.keys(metadata).length} capacity compositions`
		);
		return metadata;
	}
);

// COMPOSE-INTO CONSTRAINT METADATA
// Tracks detailed constraint information for each compose-into relationship
export const feasibleComposeIntoMetadata = derived(
	[userDesiredComposeInto, contributorCapacityShares, networkCapacities, userCapacities],
	([$userDesiredComposeInto, $contributorCapacityShares, $networkCapacities, $userCapacities]) => {
		console.log('[FEASIBLE-METADATA-INTO] Calculating constraint metadata for compose-into...');

		const metadata: Record<string, Record<string, ConstraintMetadata>> = {};

		// Step 1: Calculate share-constrained amounts
		const shareConstrainedDesires: Record<string, Record<string, number>> = {};

		Object.entries($userDesiredComposeInto).forEach(([ourCapacityId, ourDesires]) => {
			Object.entries(ourDesires).forEach(([theirCapacityId, desiredAbsoluteUnits]) => {
				const providerId = Object.keys($networkCapacities).find(
					(id) => $networkCapacities[id] && $networkCapacities[id][theirCapacityId]
				);
				if (!providerId) return;

				const ourCapacity = $userCapacities?.[ourCapacityId];
				if (!ourCapacity) return;

				const theirShareInOurCapacity =
					$contributorCapacityShares[providerId]?.[ourCapacityId] || 0;
				const ourCapacityQuantity = ourCapacity.quantity || 0;
				const maxUnitsTheyCanReceive = ourCapacityQuantity * theirShareInOurCapacity;
				const shareConstrainedUnits = Math.min(desiredAbsoluteUnits, maxUnitsTheyCanReceive);

				if (!shareConstrainedDesires[ourCapacityId]) {
					shareConstrainedDesires[ourCapacityId] = {};
				}
				shareConstrainedDesires[ourCapacityId][theirCapacityId] = shareConstrainedUnits;
			});
		});

		// Step 2: Calculate resource allocation demands
		const ourCapacityDemands: Record<
			string,
			{
				totalDemand: number;
				available: number;
				recipients: Array<{ theirCapacityId: string; desiredUnits: number }>;
			}
		> = {};

		Object.entries(shareConstrainedDesires).forEach(([ourCapacityId, ourDesires]) => {
			Object.entries(ourDesires).forEach(([theirCapacityId, desiredUnits]) => {
				if (!ourCapacityDemands[ourCapacityId]) {
					const ourCapacity = $userCapacities?.[ourCapacityId];
					const availableUnits = ourCapacity?.quantity || 0;

					ourCapacityDemands[ourCapacityId] = {
						totalDemand: 0,
						available: availableUnits,
						recipients: []
					};
				}

				ourCapacityDemands[ourCapacityId].totalDemand += desiredUnits;
				ourCapacityDemands[ourCapacityId].recipients.push({
					theirCapacityId,
					desiredUnits
				});
			});
		});

		// Step 3: Generate metadata for each composition
		Object.entries(ourCapacityDemands).forEach(([ourCapacityId, demandInfo]) => {
			const { totalDemand, available, recipients } = demandInfo;
			const scalingFactor = totalDemand > available ? available / totalDemand : 1.0;
			const competitorCount = recipients.length;

			recipients.forEach(({ theirCapacityId, desiredUnits }) => {
				const finalFeasibleUnits = desiredUnits * scalingFactor;
				const shareConstrainedAmount = desiredUnits;

				// Determine constraint type
				let constraintType: 'share_limit' | 'resource_competition' | 'no_constraint';
				if (scalingFactor < 1.0) {
					constraintType = 'resource_competition';
				} else if (
					shareConstrainedAmount < ($userDesiredComposeInto[ourCapacityId]?.[theirCapacityId] || 0)
				) {
					constraintType = 'share_limit';
				} else {
					constraintType = 'no_constraint';
				}

				if (!metadata[ourCapacityId]) {
					metadata[ourCapacityId] = {};
				}

				metadata[ourCapacityId][theirCapacityId] = {
					feasibleAmount: finalFeasibleUnits,
					constraintType,
					scalingFactor,
					shareConstrainedAmount,
					availableAmount: available,
					totalDemandOnSource: totalDemand,
					competitorCount
				};
			});
		});

		console.log(
			`[FEASIBLE-METADATA-INTO] Generated metadata for ${Object.keys(metadata).length} capacity compositions`
		);
		return metadata;
	}
);
