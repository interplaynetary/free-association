/**
 * Mutual-Priority Allocation Algorithm with Adaptive Damping
 * 
 * TWO-TIER ALLOCATION SYSTEM:
 * 
 * TIER 1: MUTUAL RECOGNITION (Priority)
 *   - MR(You, Them) = min(Your recognition of Them, Their recognition of You)
 *   - Allocated FIRST based on MR × ActiveNeed
 *   - Respects bilateral recognition relationships
 * 
 * TIER 2: NON-MUTUAL (Leftover Capacity)
 *   - One-way recognition (Provider → Recipient only)
 *   - Gets only REMAINING capacity after mutual needs met
 *   - Allocated proportionally by one-way recognition weight
 * 
 * ADAPTIVE DAMPING:
 *   - Prevents oscillations during iterative convergence
 *   - ActiveNeed = ResidualNeed × DampingFactor
 *   - DampingFactor = 0.5 (oscillating), 0.8 (moderate), 1.0 (smooth)
 *   - Automatically detects oscillation patterns in over-allocation history
 *   - Converges faster with damping than without
 * 
 * ALLOCATION CAPPING:
 *   - All allocations capped by recipient's actual residual_need
 *   - Prevents over-allocation and ensures contractiveness
 *   - Critical for Banach Fixed-Point Theorem to apply
 *   - Excess capacity can be redistributed to other recipients
 * 
 * DENOMINATOR FLOOR:
 *   - Denominators bounded by DENOMINATOR_FLOOR (0.0001) to prevent division by zero
 *   - Ensures Lipschitz continuity even when sums approach zero
 *   - Satisfies S_p(r) ≥ S_min > 0 condition from contraction theorem
 * 
 * FILTER SUPPORT:
 *   - Capacity-specific filters (e.g., location, skill requirements)
 *   - Recipients only eligible if they pass the filter
 *   - Specific-Share computed with filter weights
 * 
 * KEY FORMULAS:
 * 
 * Mutual-Recognition(You, Them) = min(
 *   Their-share-of-Your-total-recognition,
 *   Your-share-of-Their-total-recognition
 * )
 * 
 * Active-Need(You) = Residual-Need(You) × Damping-Factor(You)
 * 
 * General-Share(You, Provider) = 
 *   MR(You, Provider) / Σ MR(Provider, Each-Recipient)
 * 
 * Specific-Share(You, Provider, Capacity) = 
 *   General-Share(You, Provider) × Filter(You, Capacity)
 *   ──────────────────────────────────────────────────────────────
 *   Σ [General-Share(R, Provider) × Filter(R, Capacity)]
 * 
 * USAGE:
 * 
 * 1. After receiving allocations, update commitment:
 *    const updated = updateCommitmentDamping(commitment, totalReceived);
 * 
 * 2. Publish updated commitment with damping info:
 *    publishMyCommitment(updated);
 * 
 * 3. System automatically uses damping_factor in next allocation round
 */

import { writable, derived, get } from 'svelte/store';
import type { Writable, Readable } from 'svelte/store';

// ═══════════════════════════════════════════════════════════════════
// CONSTANTS
// ═══════════════════════════════════════════════════════════════════

export const STALE_THRESHOLD_MS = 60000; // 60 seconds
export const CONVERGENCE_EPSILON = 0.001;
export const ROUND_GOSSIP_INTERVAL_MS = 5000; // 5 seconds
export const ROUND_ADVANCEMENT_THRESHOLD = 0.5; // 50%
export const DENOMINATOR_FLOOR = 0.0001; // Minimum denominator to prevent division by zero

// ═══════════════════════════════════════════════════════════════════
// TYPES
// ═══════════════════════════════════════════════════════════════════

interface VectorClock {
	[pubKey: string]: number;
}

interface RoundState {
	pubKey: string;
	round: number;
	vectorClock: VectorClock;
	timestamp: number;
}

interface Commitment {
	residual_need: number;
	stated_need: number;
	capacity?: number;
	mr_values?: Record<string, number>; // My MR with others (computed from bilateral minimum)
	recognition_weights?: Record<string, number>; // My one-way recognition (% of my 100%)
	capacity_filters?: Record<string, CapacityFilter>; // Filters per capacity type
	damping_factor?: number; // Adaptive damping (0.5, 0.8, or 1.0)
	over_allocation_history?: number[]; // Last 3 over-allocations for oscillation detection
	timestamp: number;
	vectorClock?: VectorClock;
	round?: number;
}

interface CapacityFilter {
	filter_fn: string; // Serialized filter function or criteria
	required_attributes?: Record<string, any>; // e.g., { location: "SF", skill: "rust" }
}

interface TwoTierAllocationState {
	// Tier 1: Mutual recognition denominators
	mutualDenominator: Record<string, number>; // capacity_id -> Σ[MR × ResidualNeed] (mutual only)
	
	// Tier 2: Non-mutual denominators
	nonMutualDenominator: Record<string, number>; // capacity_id -> Σ[Weight × ResidualNeed] (non-mutual only)
	
	// Computed allocations by tier
	mutualAllocations: Record<string, Record<string, number>>; // capacity_id -> { recipient_id -> amount }
	nonMutualAllocations: Record<string, Record<string, number>>;
	
	timestamp: number;
}

// ═══════════════════════════════════════════════════════════════════
// RAW DATA STORES
// ═══════════════════════════════════════════════════════════════════

export const myCommitment: Writable<Commitment | null> = writable(null);
export const myAllocationState: Writable<TwoTierAllocationState | null> = writable(null);
export const myRecognitionWeights: Writable<Record<string, number>> = writable({}); // One-way: pubKey -> % of my 100%

export const networkCommitments: Writable<Record<string, Commitment>> = writable({});
export const networkAllocationStates: Writable<Record<string, TwoTierAllocationState>> = writable({});
export const networkRecognitionWeights: Writable<Record<string, Record<string, number>>> = writable({});

// ═══════════════════════════════════════════════════════════════════
// VECTOR CLOCK-BASED ROUND COORDINATION
// ═══════════════════════════════════════════════════════════════════

export const myVectorClock: Writable<VectorClock> = writable({});
export const myCurrentRound: Writable<number> = writable(0);
export const networkRoundStates: Writable<Record<string, RoundState>> = writable({});

// ═══════════════════════════════════════════════════════════════════
// MUTUAL RECOGNITION COMPUTATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Compute bilateral Mutual Recognition
 * MR(Me, Them) = min(My recognition of Them, Their recognition of Me)
 */
export function computeMutualRecognition(
	myPub: string,
	theirPub: string,
	myWeights: Record<string, number>,
	theirWeights: Record<string, number>
): number {
	const myRecOfThem = myWeights[theirPub] || 0;
	const theirRecOfMe = theirWeights[myPub] || 0;
	
	// Bilateral minimum
	const mr = Math.min(myRecOfThem, theirRecOfMe);
	
	if (mr > 0) {
		console.log(`[MR] ${myPub.slice(0,8)}↔${theirPub.slice(0,8)}: min(${myRecOfThem.toFixed(3)}, ${theirRecOfMe.toFixed(3)}) = ${mr.toFixed(3)}`);
	}
	
	return mr;
}

/**
 * Compute all MR values for a given participant
 * Returns: { pubKey -> MR value }
 * 
 * NOTE: Self-recognition is fully supported!
 * If myWeights[myPub] > 0, then MR(Me, Me) = myWeights[myPub]
 * This allows self-investment/self-care in the recognition economy.
 */
export function computeAllMutualRecognition(
	myPub: string,
	myWeights: Record<string, number>,
	networkWeights: Record<string, Record<string, number>>
): Record<string, number> {
	const mrValues: Record<string, number> = {};
	
	// For everyone I recognize (INCLUDING MYSELF if myWeights[myPub] exists)
	// Self-recognition: MR(Me, Me) = min(myWeights[myPub], myWeights[myPub]) = myWeights[myPub]
	for (const theirPub in myWeights) {
		if (myWeights[theirPub] > 0) {
			const theirWeights = networkWeights[theirPub] || {};
			mrValues[theirPub] = computeMutualRecognition(myPub, theirPub, myWeights, theirWeights);
		}
	}
	
	// For everyone who recognizes me (even if I don't recognize them)
	// Skip myPub here to avoid duplicate processing (already handled above)
	for (const theirPub in networkWeights) {
		if (theirPub === myPub) continue; // Already processed in first loop
		
		const theirWeights = networkWeights[theirPub];
		if (theirWeights[myPub] > 0 && !mrValues[theirPub]) {
			mrValues[theirPub] = computeMutualRecognition(myPub, theirPub, myWeights, theirWeights);
		}
	}
	
	console.log(`[MR-COMPUTE] Computed ${Object.keys(mrValues).length} MR values for ${myPub.slice(0, 8)}`);
	return mrValues;
}

// ═══════════════════════════════════════════════════════════════════
// HELPER STORES (must be declared before derived stores that use them)
// ═══════════════════════════════════════════════════════════════════

/**
 * My public key
 */
export const myPubKey: Writable<string | null> = writable(null);

/**
 * Derived store: My MR values with all participants
 */
export const myMutualRecognition: Readable<Record<string, number>> = derived(
	[myPubKey, myRecognitionWeights, networkRecognitionWeights],
	([$myPubKey, $myRecognitionWeights, $networkRecognitionWeights]) => {
		if (!$myPubKey) return {};
		
		return computeAllMutualRecognition($myPubKey, $myRecognitionWeights, $networkRecognitionWeights);
	}
);

// ═══════════════════════════════════════════════════════════════════
// DERIVED SUBGROUPS (Two-Tier: Mutual vs Non-Mutual)
// ═══════════════════════════════════════════════════════════════════

/**
 * GROUP 1A: My Mutual Beneficiaries
 * People with whom I have mutual recognition (MR > 0)
 * Priority allocation tier
 */
export const myMutualBeneficiaries: Readable<string[]> = derived(
	[myMutualRecognition],
	([$myMutualRecognition]) => {
		return Object.entries($myMutualRecognition)
			.filter(([_, mr]) => mr > 0)
			.map(([pubKey, _]) => pubKey);
	}
);

/**
 * GROUP 1B: My Non-Mutual Beneficiaries
 * People I recognize (one-way) but MR = 0
 * Gets only leftover capacity after mutual needs met
 */
export const myNonMutualBeneficiaries: Readable<string[]> = derived(
	[myRecognitionWeights, myMutualRecognition],
	([$myRecognitionWeights, $myMutualRecognition]) => {
		return Object.entries($myRecognitionWeights)
			.filter(([pubKey, weight]) => {
				return weight > 0 && ($myMutualRecognition[pubKey] || 0) === 0;
			})
			.map(([pubKey, _]) => pubKey);
	}
);

/**
 * GROUP 2A: Mutual Providers (Priority for receiving)
 * Providers with mutual recognition who have capacity
 * I receive from them with priority
 */
export const mutualProvidersForMe: Readable<string[]> = derived(
	[networkCommitments, networkAllocationStates, myMutualRecognition],
	([$networkCommitments, $networkAllocationStates, $myMutualRecognition]) => {
		const providers: string[] = [];
		const now = Date.now();
		
		for (const pubKey in $networkCommitments) {
			const commitment = $networkCommitments[pubKey];
			const allocationState = $networkAllocationStates[pubKey];
			
			// Freshness check
			const isFresh = commitment.timestamp && (now - commitment.timestamp < STALE_THRESHOLD_MS);
			if (!isFresh) continue;
			
			// Must have capacity
			const hasCapacity = commitment.capacity && commitment.capacity > 0;
			if (!hasCapacity) continue;
			
			// Must have mutual recognition
			const mr = $myMutualRecognition[pubKey] || 0;
			if (mr > 0) {
				providers.push(pubKey);
			}
		}
		
		return providers;
	}
);

/**
 * GROUP 2B: Non-Mutual Providers (Receive leftover only)
 * Providers who recognize me (one-way) but MR = 0
 * I only receive their leftover capacity
 */
export const nonMutualProvidersForMe: Readable<string[]> = derived(
	[networkCommitments, networkAllocationStates, myMutualRecognition],
	([$networkCommitments, $networkAllocationStates, $myMutualRecognition]) => {
		const providers: string[] = [];
		const now = Date.now();
		
		for (const pubKey in $networkCommitments) {
			const commitment = $networkCommitments[pubKey];
			
			// Freshness check
			const isFresh = commitment.timestamp && (now - commitment.timestamp < STALE_THRESHOLD_MS);
			if (!isFresh) continue;
			
			// Must have capacity
			const hasCapacity = commitment.capacity && commitment.capacity > 0;
			if (!hasCapacity) continue;
			
			// Must recognize me (one-way)
			const recognizesMe = commitment.mr_values && Object.keys(commitment.mr_values).includes(get(myPubKey) || '');
			
			// But no mutual recognition
			const mr = $myMutualRecognition[pubKey] || 0;
			
			if (recognizesMe && mr === 0) {
				providers.push(pubKey);
			}
		}
		
		return providers;
	}
);

/**
 * GROUP 3: Mutual Beneficiaries With Needs
 * Mutual partners who still have residual needs
 * These contribute to TIER 1 denominator
 */
export const mutualBeneficiariesWithNeeds: Readable<string[]> = derived(
	[myMutualBeneficiaries, networkCommitments],
	([$myMutualBeneficiaries, $networkCommitments]) => {
		const now = Date.now();
		
		return $myMutualBeneficiaries.filter(pubKey => {
			const commitment = $networkCommitments[pubKey];
			if (!commitment) return false;
			
			// Freshness check
			const isFresh = commitment.timestamp && (now - commitment.timestamp < STALE_THRESHOLD_MS);
			if (!isFresh) return false;
			
			// Must have residual need
			return commitment.residual_need > 0;
		});
	}
);

/**
 * GROUP 4: Non-Mutual Beneficiaries With Needs
 * Non-mutual recipients who still have residual needs
 * These contribute to TIER 2 denominator
 */
export const nonMutualBeneficiariesWithNeeds: Readable<string[]> = derived(
	[myNonMutualBeneficiaries, networkCommitments],
	([$myNonMutualBeneficiaries, $networkCommitments]) => {
		const now = Date.now();
		
		return $myNonMutualBeneficiaries.filter(pubKey => {
			const commitment = $networkCommitments[pubKey];
			if (!commitment) return false;
			
			// Freshness check
			const isFresh = commitment.timestamp && (now - commitment.timestamp < STALE_THRESHOLD_MS);
			if (!isFresh) return false;
			
			// Must have residual need
			return commitment.residual_need > 0;
		});
	}
);

/**
 * GROUP 5: All Mutual Partners
 * Union of mutual beneficiaries and mutual providers
 */
export const allMutualPartners: Readable<string[]> = derived(
	[myMutualBeneficiaries, mutualProvidersForMe],
	([$myMutualBeneficiaries, $mutualProvidersForMe]) => {
		const partners = new Set<string>([...$myMutualBeneficiaries, ...$mutualProvidersForMe]);
		return Array.from(partners);
	}
);

/**
 * GROUP 6: Active Participants (fresh commitments)
 */
export const activeParticipants: Readable<string[]> = derived(
	[networkCommitments],
	([$networkCommitments]) => {
		const now = Date.now();
		
		return Object.entries($networkCommitments)
			.filter(([_, commitment]) => {
				const isFresh = commitment.timestamp && (now - commitment.timestamp < STALE_THRESHOLD_MS);
				return isFresh;
			})
			.map(([pubKey, _]) => pubKey);
	}
);

// ═══════════════════════════════════════════════════════════════════
// CONVERGENCE TRACKING
// ═══════════════════════════════════════════════════════════════════

export const previousMutualDenominators: Writable<Record<string, Record<string, number>>> = writable({});
export const previousNonMutualDenominators: Writable<Record<string, Record<string, number>>> = writable({});

export const hasSystemConverged: Readable<boolean> = derived(
	[myAllocationState, previousMutualDenominators, previousNonMutualDenominators],
	([$myAllocationState, $previousMutualDenominators, $previousNonMutualDenominators]) => {
		if (!$myAllocationState) return false;
		
		const myPub = get(myPubKey);
		if (!myPub) return false;
		
		const prevMutual = $previousMutualDenominators[myPub];
		const prevNonMutual = $previousNonMutualDenominators[myPub];
		
		if (!prevMutual || !prevNonMutual) return false;
		
		// Check mutual denominator stability
		for (const capacityId in $myAllocationState.mutualDenominator) {
			const current = $myAllocationState.mutualDenominator[capacityId];
			const previous = prevMutual[capacityId] || 0;
			
			if (Math.abs(current - previous) > CONVERGENCE_EPSILON) {
				return false;
			}
		}
		
		// Check non-mutual denominator stability
		for (const capacityId in $myAllocationState.nonMutualDenominator) {
			const current = $myAllocationState.nonMutualDenominator[capacityId];
			const previous = prevNonMutual[capacityId] || 0;
			
			if (Math.abs(current - previous) > CONVERGENCE_EPSILON) {
				return false;
			}
		}
		
		return true;
	}
);

/**
 * Derived store: Participants with oscillating allocations (damping_factor < 1.0)
 */
export const oscillatingParticipants: Readable<string[]> = derived(
	[networkCommitments],
	([$networkCommitments]) => {
		return Object.entries($networkCommitments)
			.filter(([_, commitment]) => {
				const factor = commitment.damping_factor || 1.0;
				return factor < 1.0; // Oscillating or moderately dampened
			})
			.map(([pubKey, _]) => pubKey);
	}
);

// ═══════════════════════════════════════════════════════════════════
// TWO-TIER ALLOCATION COMPUTATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Compute Two-Tier Allocation State for a Provider
 * 
 * TIER 1: Allocate to mutual partners first (based on MRD - Mutual Recognition Distribution)
 * TIER 2: Allocate remaining capacity to non-mutual (based on renormalized one-way recognition)
 * 
 * KEY: Each tier uses renormalized shares that sum to 100% within that tier
 */
export function computeTwoTierAllocation(
	providerPubKey: string,
	capacity: number,
	capacityId: string,
	myMRValues: Record<string, number>, // My MR with all participants
	myWeights: Record<string, number>, // My one-way recognition weights
	networkCommitments: Record<string, Commitment>,
	capacityFilter?: CapacityFilter
): TwoTierAllocationState {
	
	console.log(`[TWO-TIER] Computing allocation for ${providerPubKey.slice(0,8)}, capacity: ${capacity}`);
	
	// ────────────────────────────────────────────────────────────────
	// STEP 1: Calculate Total Recognition by Tier
	// ────────────────────────────────────────────────────────────────
	
	// Total Mutual Recognition (sum of all MR values)
	let totalMutualRecognition = 0;
	for (const recipientPub in myMRValues) {
		const mr = myMRValues[recipientPub];
		if (mr > 0) {
			totalMutualRecognition += mr;
		}
	}
	
	// Total Non-Mutual Recognition (sum of one-way weights for non-mutual)
	let totalNonMutualRecognition = 0;
	for (const recipientPub in myWeights) {
		const weight = myWeights[recipientPub];
		const mr = myMRValues[recipientPub] || 0;
		if (weight > 0 && mr === 0) {
			totalNonMutualRecognition += weight;
		}
	}
	
	console.log(`[RECOGNITION] Total mutual: ${totalMutualRecognition.toFixed(3)}, Total non-mutual: ${totalNonMutualRecognition.toFixed(3)}`);
	
	// ────────────────────────────────────────────────────────────────
	// TIER 1: MUTUAL RECOGNITION ALLOCATION (Priority)
	// Using MRD (Mutual Recognition Distribution) = MR / TotalMutualRecognition
	// ────────────────────────────────────────────────────────────────
	
	let mutualDenominator = 0;
	const mutualNumerators: Record<string, number> = {};
	
	if (totalMutualRecognition > 0) {
		for (const recipientPub in myMRValues) {
			const mr = myMRValues[recipientPub];
			if (mr === 0) continue; // Skip non-mutual
			
			const commitment = networkCommitments[recipientPub];
			if (!commitment || commitment.residual_need <= 0) continue;
			
			// Apply filter if present
			if (capacityFilter && !passesFilter(recipientPub, commitment, capacityFilter)) {
				continue;
			}
			
			// MRD = MR / TotalMutualRecognition (renormalized share of mutual recognition)
			const mrd = mr / totalMutualRecognition;
			
			// Apply damping to residual need
			const dampingFactor = commitment.damping_factor || 1.0;
			const activeNeed = commitment.residual_need * dampingFactor;
			
			// Numerator = MRD × ActiveNeed (damped)
			const numerator = mrd * activeNeed;
			mutualNumerators[recipientPub] = numerator;
			mutualDenominator += numerator;
			
			console.log(`[TIER-1] ${recipientPub.slice(0,8)}: MR=${mr.toFixed(3)}, MRD=${mrd.toFixed(3)}, Need=${commitment.residual_need}, Damp=${dampingFactor.toFixed(2)}, Active=${activeNeed.toFixed(2)}`);
		}
	}
	
	// Apply floor to denominator to prevent division by zero
	const safeMutualDenominator = Math.max(mutualDenominator, DENOMINATOR_FLOOR);
	
	console.log(`[TIER-1] Mutual denominator: ${mutualDenominator.toFixed(2)}, ${Object.keys(mutualNumerators).length} mutual recipients`);
	
	// Allocate to mutual partners (WITH CAPPING BY NEED)
	const mutualAllocations: Record<string, number> = {};
	let mutualCapacityUsed = 0;
	
	for (const recipientPub in mutualNumerators) {
		if (mutualDenominator > 0) {
			const rawAllocation = capacity * mutualNumerators[recipientPub] / safeMutualDenominator;
			const commitment = networkCommitments[recipientPub];
			
			// CAP BY ACTUAL RESIDUAL NEED (critical for contractiveness!)
			const cappedAllocation = Math.min(rawAllocation, commitment.residual_need);
			
			mutualAllocations[recipientPub] = cappedAllocation;
			mutualCapacityUsed += cappedAllocation;
			
			if (rawAllocation > commitment.residual_need) {
				console.log(`[TIER-1] Capped allocation for ${recipientPub.slice(0,8)}: ${rawAllocation.toFixed(2)} → ${cappedAllocation.toFixed(2)} (need: ${commitment.residual_need})`);
			}
		}
	}
	
	console.log(`[TIER-1] Allocated ${mutualCapacityUsed.toFixed(2)}/${capacity} to mutual partners`);
	
	// ────────────────────────────────────────────────────────────────
	// TIER 2: NON-MUTUAL ALLOCATION (Leftover Capacity)
	// Using Renormalized Non-Mutual Distribution = Weight / TotalNonMutualRecognition
	// ────────────────────────────────────────────────────────────────
	
	const remainingCapacity = capacity - mutualCapacityUsed;
	
	let nonMutualDenominator = 0;
	const nonMutualNumerators: Record<string, number> = {};
	
	if (remainingCapacity > 0 && totalNonMutualRecognition > 0) {
		for (const recipientPub in myWeights) {
			const weight = myWeights[recipientPub];
			if (weight === 0) continue;
			
			// Skip if mutual (already handled in tier 1)
			const mr = myMRValues[recipientPub] || 0;
			if (mr > 0) continue;
			
			const commitment = networkCommitments[recipientPub];
			if (!commitment || commitment.residual_need <= 0) continue;
			
			// Apply filter if present
			if (capacityFilter && !passesFilter(recipientPub, commitment, capacityFilter)) {
				continue;
			}
			
			// Renormalized share = Weight / TotalNonMutualRecognition
			const renormalizedShare = weight / totalNonMutualRecognition;
			
			// Apply damping to residual need
			const dampingFactor = commitment.damping_factor || 1.0;
			const activeNeed = commitment.residual_need * dampingFactor;
			
			// Numerator = RenormalizedShare × ActiveNeed (damped)
			const numerator = renormalizedShare * activeNeed;
			nonMutualNumerators[recipientPub] = numerator;
			nonMutualDenominator += numerator;
			
			console.log(`[TIER-2] ${recipientPub.slice(0,8)}: Weight=${weight.toFixed(3)}, Renormalized=${renormalizedShare.toFixed(3)}, Need=${commitment.residual_need}, Damp=${dampingFactor.toFixed(2)}, Active=${activeNeed.toFixed(2)}`);
		}
	}
	
	// Apply floor to denominator to prevent division by zero
	const safeNonMutualDenominator = Math.max(nonMutualDenominator, DENOMINATOR_FLOOR);
	
	console.log(`[TIER-2] Non-mutual denominator: ${nonMutualDenominator.toFixed(2)}, ${Object.keys(nonMutualNumerators).length} non-mutual recipients`);
	
	// Allocate remaining capacity to non-mutual recipients (WITH CAPPING BY NEED)
	const nonMutualAllocations: Record<string, number> = {};
	let nonMutualCapacityUsed = 0;
	
	for (const recipientPub in nonMutualNumerators) {
		if (nonMutualDenominator > 0 && remainingCapacity > 0) {
			const rawAllocation = remainingCapacity * nonMutualNumerators[recipientPub] / safeNonMutualDenominator;
			const commitment = networkCommitments[recipientPub];
			
			// CAP BY ACTUAL RESIDUAL NEED (critical for contractiveness!)
			const cappedAllocation = Math.min(rawAllocation, commitment.residual_need);
			
			nonMutualAllocations[recipientPub] = cappedAllocation;
			nonMutualCapacityUsed += cappedAllocation;
			
			if (rawAllocation > commitment.residual_need) {
				console.log(`[TIER-2] Capped allocation for ${recipientPub.slice(0,8)}: ${rawAllocation.toFixed(2)} → ${cappedAllocation.toFixed(2)} (need: ${commitment.residual_need})`);
			}
		}
	}
	
	console.log(`[TIER-2] Allocated ${nonMutualCapacityUsed.toFixed(2)}/${remainingCapacity.toFixed(2)} remaining capacity to non-mutual recipients`);
	
	return {
		mutualDenominator: { [capacityId]: mutualDenominator },
		nonMutualDenominator: { [capacityId]: nonMutualDenominator },
		mutualAllocations: { [capacityId]: mutualAllocations },
		nonMutualAllocations: { [capacityId]: nonMutualAllocations },
		timestamp: Date.now()
	};
}

/**
 * Check if a recipient passes the capacity filter
 */
function passesFilter(recipientPub: string, commitment: Commitment, filter: CapacityFilter): boolean {
	// TODO: Implement actual filter logic based on filter.required_attributes
	// For now, return true (no filtering)
	return true;
}

// ═══════════════════════════════════════════════════════════════════
// ADAPTIVE DAMPING
// ═══════════════════════════════════════════════════════════════════

/**
 * Detect oscillation pattern in over-allocation history
 * Returns true if pattern is: up-down-up OR down-up-down
 */
function detectOscillation(history: number[]): boolean {
	if (history.length < 3) return false;
	
	const recent = history.slice(-3);
	
	// Check for oscillation (up-down-up or down-up-down)
	const upDownUp = recent[0] < recent[1] && recent[1] > recent[2];
	const downUpDown = recent[0] > recent[1] && recent[1] < recent[2];
	
	return upDownUp || downUpDown;
}

/**
 * Detect smooth convergence (monotonically decreasing)
 */
function detectSmoothConvergence(history: number[]): boolean {
	if (history.length < 3) return false;
	
	const recent = history.slice(-3);
	
	// Check if monotonically decreasing
	return recent[0] >= recent[1] && recent[1] >= recent[2];
}

/**
 * Compute adaptive damping factor based on over-allocation history
 * 
 * @param history - Last 3+ over-allocation values
 * @returns Damping factor: 0.5 (oscillating), 1.0 (smooth), 0.8 (otherwise)
 */
export function computeDampingFactor(history: number[]): number {
	if (history.length < 3) {
		return 1.0; // Default full speed initially
	}
	
	if (detectOscillation(history)) {
		return 0.5; // Slow down oscillations
	}
	
	if (detectSmoothConvergence(history)) {
		return 1.0; // Full speed when converging smoothly
	}
	
	return 0.8; // Moderate damping otherwise
}

/**
 * Update commitment with new over-allocation data and recompute damping factor
 * 
 * @param commitment - Current commitment
 * @param totalReceived - Total allocation received this round
 * @returns Updated commitment with new damping factor and history
 */
export function updateCommitmentDamping(
	commitment: Commitment,
	totalReceived: number
): Commitment {
	const overAllocation = Math.max(0, totalReceived - commitment.stated_need);
	
	// Update history (keep last 3)
	const history = commitment.over_allocation_history || [];
	history.push(overAllocation);
	if (history.length > 3) {
		history.shift(); // Remove oldest
	}
	
	// Compute new damping factor
	const dampingFactor = computeDampingFactor(history);
	
	console.log(`[DAMPING] Over-alloc: ${overAllocation.toFixed(2)}, History: [${history.map(h => h.toFixed(1)).join(', ')}], Factor: ${dampingFactor.toFixed(2)}`);
	
	return {
		...commitment,
		damping_factor: dampingFactor,
		over_allocation_history: history
	};
}

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION MANAGEMENT
// ═══════════════════════════════════════════════════════════════════

export const activeSubscriptions = {
	mutualPartners: writable<Set<string>>(new Set()),
	nonMutualBeneficiaries: writable<Set<string>>(new Set()),
	nonMutualProviders: writable<Set<string>>(new Set()),
	allParticipants: writable<Set<string>>(new Set())
};

export const subscriptionSummary: Readable<{
	mutualPartners: number;
	nonMutualBeneficiaries: number;
	nonMutualProviders: number;
	total: number;
}> = derived(
	[
		activeSubscriptions.mutualPartners,
		activeSubscriptions.nonMutualBeneficiaries,
		activeSubscriptions.nonMutualProviders
	],
	([$mutualPartners, $nonMutualBeneficiaries, $nonMutualProviders]) => {
		const total = $mutualPartners.size + $nonMutualBeneficiaries.size + $nonMutualProviders.size;
		return {
			mutualPartners: $mutualPartners.size,
			nonMutualBeneficiaries: $nonMutualBeneficiaries.size,
			nonMutualProviders: $nonMutualProviders.size,
			total
		};
	}
);

// ═══════════════════════════════════════════════════════════════════
// VECTOR CLOCK OPERATIONS
// ═══════════════════════════════════════════════════════════════════

function compareVectorClocks(vc1: VectorClock, vc2: VectorClock): number {
	let vc1Greater = false;
	let vc2Greater = false;
	
	const allParticipants = new Set([...Object.keys(vc1), ...Object.keys(vc2)]);
	
	for (const pubKey of allParticipants) {
		const v1 = vc1[pubKey] || 0;
		const v2 = vc2[pubKey] || 0;
		
		if (v1 > v2) vc1Greater = true;
		if (v2 > v1) vc2Greater = true;
	}
	
	if (vc1Greater && !vc2Greater) return 1;
	if (vc2Greater && !vc1Greater) return -1;
	return 0;
}

function mergeVectorClocks(vc1: VectorClock, vc2: VectorClock): VectorClock {
	const merged: VectorClock = { ...vc1 };
	
	for (const pubKey in vc2) {
		merged[pubKey] = Math.max(merged[pubKey] || 0, vc2[pubKey]);
	}
	
	return merged;
}

export function incrementMyVectorClock() {
	const myPub = get(myPubKey);
	if (!myPub) return;
	
	myVectorClock.update(vc => ({
		...vc,
		[myPub]: (vc[myPub] || 0) + 1
	}));
}

export function updateVectorClockFromPeer(peerPubKey: string, peerVectorClock: VectorClock) {
	myVectorClock.update(myVC => {
		const merged = mergeVectorClocks(myVC, peerVectorClock);
		
		const myPub = get(myPubKey);
		if (myPub) {
			merged[myPub] = (merged[myPub] || 0) + 1;
		}
		
		return merged;
	});
}

function shouldAdvanceRound(): boolean {
	const myPub = get(myPubKey);
	const myRound = get(myCurrentRound);
	const roundStates = get(networkRoundStates);
	const active = get(activeParticipants);
	
	if (!myPub || active.length === 0) return false;
	
	let ahead = 0;
	let total = 0;
	
	for (const pubKey of active) {
		if (pubKey === myPub) continue;
		
		const state = roundStates[pubKey];
		if (state && state.round > myRound) {
			ahead++;
		}
		total++;
	}
	
	if (total === 0) return false;
	
	const ratio = ahead / total;
	return ratio >= ROUND_ADVANCEMENT_THRESHOLD;
}

export function advanceToNextRound() {
	const myPub = get(myPubKey);
	if (!myPub) return;
	
	const newRound = get(myCurrentRound) + 1;
	myCurrentRound.set(newRound);
	
	incrementMyVectorClock();
	
	console.log(`[VECTOR-CLOCK] Advanced to round ${newRound}`);
	
	publishMyRoundState();
}

export function publishMyRoundState() {
	const myPub = get(myPubKey);
	if (!myPub || !holsterUser) return;
	
	const roundState: RoundState = {
		pubKey: myPub,
		round: get(myCurrentRound),
		vectorClock: get(myVectorClock),
		timestamp: Date.now()
	};
	
	holsterUser.get('roundState').put(roundState);
	
	console.log(`[VECTOR-CLOCK] Published round state:`, roundState);
}

export function handlePeerRoundState(peerPubKey: string, roundState: RoundState) {
	networkRoundStates.update(states => ({
		...states,
		[peerPubKey]: roundState
	}));
	
	updateVectorClockFromPeer(peerPubKey, roundState.vectorClock);
	
	if (shouldAdvanceRound()) {
		advanceToNextRound();
	}
}

// ═══════════════════════════════════════════════════════════════════
// STATE UPDATE FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

export function setMyPubKey(pubKey: string) {
	myPubKey.set(pubKey);
	
	myVectorClock.update(vc => ({
		...vc,
		[pubKey]: 0
	}));
}

export function updateMyRecognitionWeights(weights: Record<string, number>) {
	myRecognitionWeights.set(weights);
}

export function updateMyCommitment(commitment: Commitment) {
	myCommitment.set(commitment);
}

export function updateNetworkCommitment(pubKey: string, commitment: Commitment) {
	networkCommitments.update(commitments => ({
		...commitments,
		[pubKey]: commitment
	}));
}

export function updateNetworkAllocationState(pubKey: string, allocationState: TwoTierAllocationState) {
	// Store previous for convergence tracking
	const current = get(networkAllocationStates)[pubKey];
	if (current) {
		const myPub = get(myPubKey);
		if (myPub) {
			previousMutualDenominators.update(prev => ({
				...prev,
				[pubKey]: current.mutualDenominator
			}));
			previousNonMutualDenominators.update(prev => ({
				...prev,
				[pubKey]: current.nonMutualDenominator
			}));
		}
	}
	
	networkAllocationStates.update(states => ({
		...states,
		[pubKey]: allocationState
	}));
}

export function updateNetworkRecognitionWeights(pubKey: string, weights: Record<string, number>) {
	networkRecognitionWeights.update(allWeights => ({
		...allWeights,
		[pubKey]: weights
	}));
}

export function clearParticipantData(pubKey: string) {
	networkCommitments.update(commitments => {
		const { [pubKey]: _, ...rest } = commitments;
		return rest;
	});
	
	networkAllocationStates.update(states => {
		const { [pubKey]: _, ...rest } = states;
		return rest;
	});
	
	networkRecognitionWeights.update(weights => {
		const { [pubKey]: _, ...rest } = weights;
		return rest;
	});
}

// ═══════════════════════════════════════════════════════════════════
// HOLSTER SUBSCRIPTION MANAGEMENT
// ═══════════════════════════════════════════════════════════════════

import { debounce } from '$lib/utils/debounce';

let holsterUser: any = null;

export function initializeHolsterUser(user: any) {
	holsterUser = user;
	console.log('[MUTUAL-PRIORITY] Holster user initialized');
}

/**
 * Subscribe to mutual partners (full data exchange)
 * Data needed: commitments, recognition weights, allocation states
 */
const debouncedSyncMutualSubscriptions = debounce((partners: string[]) => {
	if (!holsterUser) return;
	
	const currentSubs = get(activeSubscriptions.mutualPartners);
	const partnerSet = new Set(partners);
	
	// Remove old subscriptions
	currentSubs.forEach(pubKey => {
		if (!partnerSet.has(pubKey)) {
			activeSubscriptions.mutualPartners.update(subs => {
				const newSubs = new Set(subs);
				newSubs.delete(pubKey);
				return newSubs;
			});
			console.log(`[MUTUAL] Removed ${pubKey.slice(0, 20)}... from tracking`);
		}
	});
	
	// Add new subscriptions
	partners.forEach(pubKey => {
		if (!currentSubs.has(pubKey)) {
			// Subscribe to all their data
			holsterUser.get([pubKey, 'commitments']).on((data: any) => {
				if (data) updateNetworkCommitment(pubKey, data);
			}, true);
			
			holsterUser.get([pubKey, 'recognitionWeights']).on((data: any) => {
				if (data) updateNetworkRecognitionWeights(pubKey, data);
			}, true);
			
			holsterUser.get([pubKey, 'allocationStates']).on((data: any) => {
				if (data) updateNetworkAllocationState(pubKey, data);
			}, true);
			
			activeSubscriptions.mutualPartners.update(subs => {
				const newSubs = new Set(subs);
				newSubs.add(pubKey);
				return newSubs;
			});
			
			console.log(`[MUTUAL] Subscribed to ${pubKey.slice(0, 20)}... full data`);
		}
	});
}, 100);

/**
 * Subscribe to non-mutual beneficiaries (commitments only)
 */
const debouncedSyncNonMutualBeneficiarySubscriptions = debounce((beneficiaries: string[]) => {
	if (!holsterUser) return;
	
	const currentSubs = get(activeSubscriptions.nonMutualBeneficiaries);
	const beneficiarySet = new Set(beneficiaries);
	const mutualSet = new Set(get(allMutualPartners));
	
	// Remove old subscriptions
	currentSubs.forEach(pubKey => {
		if (!beneficiarySet.has(pubKey) && !mutualSet.has(pubKey)) {
			activeSubscriptions.nonMutualBeneficiaries.update(subs => {
				const newSubs = new Set(subs);
				newSubs.delete(pubKey);
				return newSubs;
			});
		}
	});
	
	// Add new subscriptions
	beneficiaries.forEach(pubKey => {
		if (mutualSet.has(pubKey)) return; // Skip if mutual (already subscribed)
		
		if (!currentSubs.has(pubKey)) {
			holsterUser.get([pubKey, 'commitments']).on((data: any) => {
				if (data) updateNetworkCommitment(pubKey, data);
			}, true);
			
			activeSubscriptions.nonMutualBeneficiaries.update(subs => {
				const newSubs = new Set(subs);
				newSubs.add(pubKey);
				return newSubs;
			});
			
			console.log(`[NON-MUTUAL-BENEFICIARY] Subscribed to ${pubKey.slice(0, 20)}... commitments`);
		}
	});
}, 100);

/**
 * Subscribe to non-mutual providers (commitments + allocation states)
 */
const debouncedSyncNonMutualProviderSubscriptions = debounce((providers: string[]) => {
	if (!holsterUser) return;
	
	const currentSubs = get(activeSubscriptions.nonMutualProviders);
	const providerSet = new Set(providers);
	const mutualSet = new Set(get(allMutualPartners));
	
	// Remove old subscriptions
	currentSubs.forEach(pubKey => {
		if (!providerSet.has(pubKey) && !mutualSet.has(pubKey)) {
			activeSubscriptions.nonMutualProviders.update(subs => {
				const newSubs = new Set(subs);
				newSubs.delete(pubKey);
				return newSubs;
			});
		}
	});
	
	// Add new subscriptions
	providers.forEach(pubKey => {
		if (mutualSet.has(pubKey)) return; // Skip if mutual
		
		if (!currentSubs.has(pubKey)) {
			holsterUser.get([pubKey, 'commitments']).on((data: any) => {
				if (data) updateNetworkCommitment(pubKey, data);
			}, true);
			
			holsterUser.get([pubKey, 'allocationStates']).on((data: any) => {
				if (data) updateNetworkAllocationState(pubKey, data);
			}, true);
			
			activeSubscriptions.nonMutualProviders.update(subs => {
				const newSubs = new Set(subs);
				newSubs.add(pubKey);
				return newSubs;
			});
			
			console.log(`[NON-MUTUAL-PROVIDER] Subscribed to ${pubKey.slice(0, 20)}... commitments & allocations`);
		}
	});
}, 100);

/**
 * Subscribe to round states from active participants
 */
const debouncedSyncRoundStateSubscriptions = debounce((participants: string[]) => {
	if (!holsterUser) return;
	
	const myPub = get(myPubKey);
	if (!myPub) return;
	
	const currentSubs = get(activeSubscriptions.allParticipants);
	const participantSet = new Set(participants);
	
	// Remove old subscriptions
	currentSubs.forEach(pubKey => {
		if (!participantSet.has(pubKey)) {
			activeSubscriptions.allParticipants.update(subs => {
				const newSubs = new Set(subs);
				newSubs.delete(pubKey);
				return newSubs;
			});
		}
	});
	
	// Add new subscriptions
	participants.forEach(pubKey => {
		if (pubKey === myPub) return; // Skip myself
		
		if (!currentSubs.has(pubKey)) {
			holsterUser.get([pubKey, 'roundState']).on((roundState: any) => {
				if (roundState && roundState.round !== undefined) {
					handlePeerRoundState(pubKey, roundState);
				}
			}, true);
			
			activeSubscriptions.allParticipants.update(subs => {
				const newSubs = new Set(subs);
				newSubs.add(pubKey);
				return newSubs;
			});
		}
	});
}, 100);

// ═══════════════════════════════════════════════════════════════════
// INITIALIZATION
// ═══════════════════════════════════════════════════════════════════

export function initializeAlgorithmSubscriptions() {
	console.log('[MUTUAL-PRIORITY] Initializing algorithm-driven subscriptions...');
	
	// Priority: Subscribe to mutual partners (full data)
	allMutualPartners.subscribe(debouncedSyncMutualSubscriptions);
	
	// Secondary: Subscribe to non-mutual beneficiaries (commitments only)
	myNonMutualBeneficiaries.subscribe(debouncedSyncNonMutualBeneficiarySubscriptions);
	
	// Secondary: Subscribe to non-mutual providers (commitments + allocations)
	nonMutualProvidersForMe.subscribe(debouncedSyncNonMutualProviderSubscriptions);
	
	// Round coordination
	activeParticipants.subscribe(debouncedSyncRoundStateSubscriptions);
	
	// Start round state gossip
	startRoundStateGossip();
	
	console.log('[MUTUAL-PRIORITY] Algorithm-driven subscriptions initialized');
}

let roundStateGossipInterval: ReturnType<typeof setInterval> | null = null;

function startRoundStateGossip() {
	if (roundStateGossipInterval) {
		clearInterval(roundStateGossipInterval);
	}
	
	publishMyRoundState();
	
	roundStateGossipInterval = setInterval(() => {
		publishMyRoundState();
	}, ROUND_GOSSIP_INTERVAL_MS);
}

export function cleanupAlgorithmSubscriptions() {
	console.log('[MUTUAL-PRIORITY] Cleaning up subscriptions...');
	
	if (roundStateGossipInterval) {
		clearInterval(roundStateGossipInterval);
		roundStateGossipInterval = null;
	}
	
	activeSubscriptions.mutualPartners.set(new Set());
	activeSubscriptions.nonMutualBeneficiaries.set(new Set());
	activeSubscriptions.nonMutualProviders.set(new Set());
	activeSubscriptions.allParticipants.set(new Set());
}

// ═══════════════════════════════════════════════════════════════════
// PUBLISHING MY STATE
// ═══════════════════════════════════════════════════════════════════

export function publishMyCommitment(commitment: Commitment) {
	incrementMyVectorClock();
	
	// Compute MR values before publishing
	const myPub = get(myPubKey);
	if (myPub) {
		const mrValues = get(myMutualRecognition);
		commitment.mr_values = mrValues;
		commitment.recognition_weights = get(myRecognitionWeights);
	}
	
	const commitmentWithClock: Commitment = {
		...commitment,
		vectorClock: get(myVectorClock),
		round: get(myCurrentRound),
		timestamp: Date.now()
	};
	
	myCommitment.set(commitmentWithClock);
	
	if (holsterUser) {
		holsterUser.get('commitments').put(commitmentWithClock);
		console.log('[PUBLISH] Published commitment with MR values:', {
			residualNeed: commitmentWithClock.residual_need,
			capacity: commitmentWithClock.capacity,
			mutualCount: Object.keys(commitmentWithClock.mr_values || {}).length,
			round: commitmentWithClock.round
		});
	}
}

export function publishMyRecognitionWeights(weights: Record<string, number>) {
	updateMyRecognitionWeights(weights);
	
	if (holsterUser) {
		holsterUser.get('recognitionWeights').put(weights);
		console.log('[PUBLISH] Published recognition weights:', {
			count: Object.keys(weights).length,
			total: Object.values(weights).reduce((sum, w) => sum + w, 0)
		});
	}
}

export function getCausallyConsistentCommitments(): Record<string, Commitment> {
	const myVC = get(myVectorClock);
	const allCommitments = get(networkCommitments);
	const snapshot: Record<string, Commitment> = {};
	
	for (const [pubKey, commitment] of Object.entries(allCommitments)) {
		if (!commitment.vectorClock || compareVectorClocks(commitment.vectorClock, myVC) <= 0) {
			snapshot[pubKey] = commitment;
		}
	}
	
	return snapshot;
}

// ═══════════════════════════════════════════════════════════════════
// DEBUG / LOGGING
// ═══════════════════════════════════════════════════════════════════

export function logSubgroupState() {
	const oscillating = get(oscillatingParticipants);
	console.log('[SUBGROUPS] Current state:', {
		mutualBeneficiaries: get(myMutualBeneficiaries).length,
		nonMutualBeneficiaries: get(myNonMutualBeneficiaries).length,
		mutualProviders: get(mutualProvidersForMe).length,
		nonMutualProviders: get(nonMutualProvidersForMe).length,
		mutualBeneficiariesWithNeeds: get(mutualBeneficiariesWithNeeds).length,
		nonMutualBeneficiariesWithNeeds: get(nonMutualBeneficiariesWithNeeds).length,
		allMutualPartners: get(allMutualPartners).length,
		activeParticipants: get(activeParticipants).length,
		oscillatingParticipants: oscillating.length,
		subscriptionSummary: get(subscriptionSummary),
		isConverged: get(hasSystemConverged)
	});
	
	// Log damping details for oscillating participants
	if (oscillating.length > 0) {
		console.log('[DAMPING] Oscillating participants:');
		const commitments = get(networkCommitments);
		oscillating.forEach(pubKey => {
			const c = commitments[pubKey];
			if (c) {
				console.log(`  ${pubKey.slice(0, 20)}...: factor=${c.damping_factor?.toFixed(2)}, history=[${c.over_allocation_history?.map(h => h.toFixed(1)).join(', ')}]`);
			}
		});
	}
}

if (typeof window !== 'undefined') {
	(window as any).debugMutualPriority = logSubgroupState;
	(window as any).computeTwoTierAllocation = computeTwoTierAllocation;
	(window as any).updateCommitmentDamping = updateCommitmentDamping;
	(window as any).computeDampingFactor = computeDampingFactor;
}

