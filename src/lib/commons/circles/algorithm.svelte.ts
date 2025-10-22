/**
 * Mutual-Priority Allocation Algorithm (Schema-Driven Architecture)
 * 
 * Refactored to use:
 * - Zod schemas from allocation-schemas.ts
 * - Holster stores from allocation-holster.svelte.ts
 * - Clean separation: algorithm logic vs data persistence
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
 * 
 * ALLOCATION CAPPING:
 *   - All allocations capped by recipient's actual residual_need
 *   - Ensures contractiveness (Banach Fixed-Point Theorem)
 * 
 * DENOMINATOR FLOOR:
 *   - Bounded by DENOMINATOR_FLOOR to prevent division by zero
 *   - Ensures Lipschitz continuity
 */

import { derived, get } from 'svelte/store';
import type { Readable } from 'svelte/store';

// Import types from schemas (single source of truth)
import type {
	Commitment,
	TwoTierAllocationState,
	CapacityFilter,
	VectorClock,
	RoundState
} from './schemas';

// Import Holster-backed stores (P2P synchronized)
import {
	myCommitmentStore,
	myAllocationStateStore,
	myRecognitionWeightsStore,
	myRoundStateStore,
	networkCommitments,
	networkAllocationStates,
	networkRecognitionWeights,
	networkRoundStates,
	getNetworkCommitmentsRecord,
	getNetworkAllocationStatesRecord,
	getNetworkRecognitionWeightsRecord
} from './stores.svelte';

// Import holster user pub key
import { holsterUserPub } from '$lib/state/holster.svelte';

// ═══════════════════════════════════════════════════════════════════
// CONSTANTS
// ═══════════════════════════════════════════════════════════════════

export const STALE_THRESHOLD_MS = 60000; // 60 seconds
export const CONVERGENCE_EPSILON = 0.001;
export const ROUND_GOSSIP_INTERVAL_MS = 5000; // 5 seconds
export const ROUND_ADVANCEMENT_THRESHOLD = 0.5; // 50%
export const DENOMINATOR_FLOOR = 0.0001; // Minimum denominator to prevent division by zero

// ═══════════════════════════════════════════════════════════════════
// HELPER STORES
// ═══════════════════════════════════════════════════════════════════

/**
 * My public key (from holster authentication)
 * Auto-updated when user logs in/out
 */
export const myPubKey = holsterUserPub;

/**
 * My MR values with all participants
 * Computed from my recognition weights + their recognition weights
 */
export const myMutualRecognition: Readable<Record<string, number>> = derived(
	[myRecognitionWeightsStore],
	([$myRecognitionWeights]) => {
		if (!$myRecognitionWeights) return {};
		
		const myPub = get(myPubKey);
		if (!myPub) return {};
		
		const networkWeights = getNetworkRecognitionWeightsRecord();
		return computeAllMutualRecognition(myPub, $myRecognitionWeights, networkWeights);
	}
);

// ═══════════════════════════════════════════════════════════════════
// DERIVED SUBGROUPS (Algorithm-Driven)
// ═══════════════════════════════════════════════════════════════════

/**
 * My Mutual Beneficiaries
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
 * My Non-Mutual Beneficiaries
 * People I recognize (one-way) but MR = 0
 * Gets only leftover capacity after mutual needs met
 */
export const myNonMutualBeneficiaries: Readable<string[]> = derived(
	[myRecognitionWeightsStore, myMutualRecognition],
	([$myRecognitionWeights, $myMutualRecognition]) => {
		if (!$myRecognitionWeights) return [];
		
		return Object.entries($myRecognitionWeights)
			.filter(([pubKey, weight]) => {
				return weight > 0 && ($myMutualRecognition[pubKey] || 0) === 0;
			})
			.map(([pubKey, _]) => pubKey);
	}
);

/**
 * Mutual Providers (who allocate to me with priority)
 * Providers with mutual recognition who have capacity
 */
export const mutualProvidersForMe: Readable<string[]> = derived(
	[myMutualRecognition],
	([$myMutualRecognition]) => {
		const providers: string[] = [];
		const now = Date.now();
		const commitments = getNetworkCommitmentsRecord();
		
		for (const pubKey in commitments) {
			const commitment = commitments[pubKey];
			
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
 * Non-Mutual Providers (who allocate leftover to me)
 * Providers who recognize me (one-way) but MR = 0
 */
export const nonMutualProvidersForMe: Readable<string[]> = derived(
	[myPubKey, myMutualRecognition],
	([$myPubKey, $myMutualRecognition]) => {
		if (!$myPubKey) return [];
		
		const providers: string[] = [];
		const now = Date.now();
		const commitments = getNetworkCommitmentsRecord();
		
		for (const pubKey in commitments) {
			const commitment = commitments[pubKey];
			
			// Freshness check
			const isFresh = commitment.timestamp && (now - commitment.timestamp < STALE_THRESHOLD_MS);
			if (!isFresh) continue;
			
			// Must have capacity
			const hasCapacity = commitment.capacity && commitment.capacity > 0;
			if (!hasCapacity) continue;
			
			// Must recognize me (one-way)
			const recognizesMe = commitment.mr_values && Object.keys(commitment.mr_values).includes($myPubKey);
			
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
 * Mutual Beneficiaries With Needs
 * Mutual partners who still have residual needs
 * These contribute to TIER 1 denominator
 */
export const mutualBeneficiariesWithNeeds: Readable<string[]> = derived(
	[myMutualBeneficiaries],
	([$myMutualBeneficiaries]) => {
		const now = Date.now();
		const commitments = getNetworkCommitmentsRecord();
		
		return $myMutualBeneficiaries.filter(pubKey => {
			const commitment = commitments[pubKey];
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
 * Non-Mutual Beneficiaries With Needs
 * Non-mutual recipients who still have residual needs
 * These contribute to TIER 2 denominator
 */
export const nonMutualBeneficiariesWithNeeds: Readable<string[]> = derived(
	[myNonMutualBeneficiaries],
	([$myNonMutualBeneficiaries]) => {
		const now = Date.now();
		const commitments = getNetworkCommitmentsRecord();
		
		return $myNonMutualBeneficiaries.filter(pubKey => {
			const commitment = commitments[pubKey];
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
 * All Mutual Partners
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
 * Active Participants (fresh commitments)
 */
export const activeParticipants: Readable<string[]> = derived(
	[myCommitmentStore], // Triggers recompute on any commitment change
	() => {
		const now = Date.now();
		const commitments = getNetworkCommitmentsRecord();
		
		return Object.entries(commitments)
			.filter(([_, commitment]) => {
				const isFresh = commitment.timestamp && (now - commitment.timestamp < STALE_THRESHOLD_MS);
				return isFresh;
			})
			.map(([pubKey, _]) => pubKey);
	}
);

/**
 * Oscillating Participants (damping_factor < 1.0)
 */
export const oscillatingParticipants: Readable<string[]> = derived(
	[myCommitmentStore],
	() => {
		const commitments = getNetworkCommitmentsRecord();
		return Object.entries(commitments)
			.filter(([_, commitment]) => {
				const factor = commitment.damping_factor || 1.0;
				return factor < 1.0;
			})
			.map(([pubKey, _]) => pubKey);
	}
);

// ═══════════════════════════════════════════════════════════════════
// VECTOR CLOCK & ROUND COORDINATION
// ═══════════════════════════════════════════════════════════════════

/**
 * My Vector Clock
 * Tracks causal relationships with other participants
 */
export const myVectorClock = derived(
	[myRoundStateStore],
	([$myRoundState]) => $myRoundState?.vectorClock || {}
);

/**
 * My Current Round
 * Current iteration of the allocation algorithm
 */
export const myCurrentRound = derived(
	[myRoundStateStore],
	([$myRoundState]) => $myRoundState?.round || 0
);

/**
 * Compare two vector clocks
 * Returns: 1 if vc1 > vc2, -1 if vc2 > vc1, 0 if concurrent
 */
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
	
	if (vc1Greater && !vc2Greater) return 1; // vc1 happened after vc2
	if (vc2Greater && !vc1Greater) return -1; // vc2 happened after vc1
	return 0; // concurrent
}

/**
 * Merge two vector clocks
 * Takes the maximum value for each participant
 */
function mergeVectorClocks(vc1: VectorClock, vc2: VectorClock): VectorClock {
	const merged: VectorClock = { ...vc1 };
	
	for (const pubKey in vc2) {
		merged[pubKey] = Math.max(merged[pubKey] || 0, vc2[pubKey]);
	}
	
	return merged;
}

/**
 * Increment my vector clock
 * Call before publishing any state change
 */
export async function incrementMyVectorClock() {
	const myPub = get(myPubKey);
	if (!myPub) return;
	
	const currentState = get(myRoundStateStore);
	const currentVC = currentState?.vectorClock || {};
	
	const newVC: VectorClock = {
		...currentVC,
		[myPub]: (currentVC[myPub] || 0) + 1
	};
	
	await myRoundStateStore.set({
		pubKey: myPub,
		round: currentState?.round || 0,
		vectorClock: newVC,
		timestamp: Date.now()
	});
}

/**
 * Update vector clock from peer
 * Call when receiving data from another participant
 */
export async function updateVectorClockFromPeer(peerPubKey: string, peerVectorClock: VectorClock) {
	const myPub = get(myPubKey);
	if (!myPub) return;
	
	const currentState = get(myRoundStateStore);
	const myVC = currentState?.vectorClock || {};
	
	// Merge peer's clock with mine
	const merged = mergeVectorClocks(myVC, peerVectorClock);
	
	// Increment my own counter after merge
	merged[myPub] = (merged[myPub] || 0) + 1;
	
	await myRoundStateStore.set({
		pubKey: myPub,
		round: currentState?.round || 0,
		vectorClock: merged,
		timestamp: Date.now()
	});
}

/**
 * Check if we should advance to the next round
 * Returns true if >= 50% of active participants are ahead of us
 */
function shouldAdvanceRound(): boolean {
	const myPub = get(myPubKey);
	const myRound = get(myCurrentRound);
	const active = get(activeParticipants);
	
	if (!myPub || active.length === 0) return false;
	
	let ahead = 0;
	let total = 0;
	
	for (const pubKey of active) {
		if (pubKey === myPub) continue;
		
		const state = networkRoundStates.get(pubKey);
		if (state && state.round > myRound) {
			ahead++;
		}
		total++;
	}
	
	if (total === 0) return false;
	
	const ratio = ahead / total;
	return ratio >= ROUND_ADVANCEMENT_THRESHOLD;
}

/**
 * Advance to the next round
 * Call when >= 50% of participants have moved ahead
 */
export async function advanceToNextRound() {
	const myPub = get(myPubKey);
	if (!myPub) return;
	
	const currentState = get(myRoundStateStore);
	const newRound = (currentState?.round || 0) + 1;
	
	// Increment vector clock
	const currentVC = currentState?.vectorClock || {};
	const newVC: VectorClock = {
		...currentVC,
		[myPub]: (currentVC[myPub] || 0) + 1
	};
	
	await myRoundStateStore.set({
		pubKey: myPub,
		round: newRound,
		vectorClock: newVC,
		timestamp: Date.now()
	});
	
	console.log(`[VECTOR-CLOCK] Advanced to round ${newRound}`);
}

/**
 * Publish my round state
 * Called periodically for gossip-based coordination
 */
export async function publishMyRoundState() {
	const myPub = get(myPubKey);
	if (!myPub) return;
	
	const currentState = get(myRoundStateStore);
	if (!currentState) {
		// Initialize if doesn't exist
		await myRoundStateStore.set({
			pubKey: myPub,
			round: 0,
			vectorClock: { [myPub]: 0 },
			timestamp: Date.now()
		});
	} else {
		// Just update timestamp (triggers persistence)
		await myRoundStateStore.set({
			...currentState,
			timestamp: Date.now()
		});
	}
}

/**
 * Handle peer round state update
 * Updates our vector clock and checks if we should advance
 */
export async function handlePeerRoundState(peerPubKey: string, roundState: RoundState) {
	// Store peer's round state
	networkRoundStates.set(peerPubKey, roundState);
	
	// Update our vector clock from peer
	await updateVectorClockFromPeer(peerPubKey, roundState.vectorClock);
	
	// Check if we should advance round
	if (shouldAdvanceRound()) {
		await advanceToNextRound();
	}
}

/**
 * Get causally consistent snapshot of commitments
 * Only includes commitments that happened-before or concurrent with our clock
 */
export function getCausallyConsistentCommitments(): Record<string, Commitment> {
	const myVC = get(myVectorClock);
	const allCommitments = getNetworkCommitmentsRecord();
	const snapshot: Record<string, Commitment> = {};
	
	for (const [pubKey, commitment] of Object.entries(allCommitments)) {
		// Include if no vector clock (legacy) or if causally consistent
		if (!commitment.vectorClock || compareVectorClocks(commitment.vectorClock, myVC) <= 0) {
			snapshot[pubKey] = commitment;
		}
	}
	
	return snapshot;
}

// ═══════════════════════════════════════════════════════════════════
// CONVERGENCE TRACKING
// ═══════════════════════════════════════════════════════════════════

/**
 * Previous denominators for convergence tracking
 * Stores denominators from the previous round
 */
const previousMutualDenominators = new Map<string, Record<string, number>>();
const previousNonMutualDenominators = new Map<string, Record<string, number>>();

/**
 * Update previous denominators tracking
 * Call this after computing new allocation state
 */
export function updatePreviousDenominators(allocationState: TwoTierAllocationState) {
	const myPub = get(myPubKey);
	if (!myPub) return;
	
	previousMutualDenominators.set(myPub, { ...allocationState.mutualDenominator });
	previousNonMutualDenominators.set(myPub, { ...allocationState.nonMutualDenominator });
}

/**
 * Check if system has converged
 * Compares current denominators with previous round
 * 
 * Converged when all denominators change by less than CONVERGENCE_EPSILON
 */
export const hasSystemConverged: Readable<boolean> = derived(
	[myAllocationStateStore, myPubKey],
	([$myAllocationState, $myPubKey]) => {
		if (!$myAllocationState || !$myPubKey) return false;
		
		const prevMutual = previousMutualDenominators.get($myPubKey);
		const prevNonMutual = previousNonMutualDenominators.get($myPubKey);
		
		// Need previous denominators to check convergence
		if (!prevMutual || !prevNonMutual) return false;
		
		// Check mutual denominator stability
		for (const capacityId in $myAllocationState.mutualDenominator) {
			const current = $myAllocationState.mutualDenominator[capacityId];
			const previous = prevMutual[capacityId] || 0;
			
			if (Math.abs(current - previous) > CONVERGENCE_EPSILON) {
				return false; // Still changing
			}
		}
		
		// Check non-mutual denominator stability
		for (const capacityId in $myAllocationState.nonMutualDenominator) {
			const current = $myAllocationState.nonMutualDenominator[capacityId];
			const previous = prevNonMutual[capacityId] || 0;
			
			if (Math.abs(current - previous) > CONVERGENCE_EPSILON) {
				return false; // Still changing
			}
		}
		
		return true; // All denominators stable
	}
);

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
	for (const theirPub in myWeights) {
		if (myWeights[theirPub] > 0) {
			const theirWeights = networkWeights[theirPub] || {};
			mrValues[theirPub] = computeMutualRecognition(myPub, theirPub, myWeights, theirWeights);
		}
	}
	
	// For everyone who recognizes me (even if I don't recognize them)
	for (const theirPub in networkWeights) {
		if (theirPub === myPub) continue; // Already processed
		
		const theirWeights = networkWeights[theirPub];
		if (theirWeights[myPub] > 0 && !mrValues[theirPub]) {
			mrValues[theirPub] = computeMutualRecognition(myPub, theirPub, myWeights, theirWeights);
		}
	}
	
	console.log(`[MR-COMPUTE] Computed ${Object.keys(mrValues).length} MR values for ${myPub.slice(0, 8)}`);
	return mrValues;
}

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
 * 
 * Filters can require specific attributes (e.g., location, skills)
 * Only recipients matching all required attributes receive allocation
 */
function passesFilter(recipientPub: string, commitment: Commitment, filter: CapacityFilter): boolean {
	if (!filter.required_attributes || Object.keys(filter.required_attributes).length === 0) {
		return true; // No filter requirements, pass by default
	}
	
	// Check if commitment has the required attributes
	// Note: In a full implementation, commitment would include a `attributes` field
	// For now, we check if filter_fn can be evaluated
	
	try {
		// If filter_fn is a serialized function, we could eval it
		// But for security, we'll use a simple attribute matching approach
		
		// For MVP: Accept all until we have recipient attribute data
		// In production, this would check commitment.attributes against filter.required_attributes
		console.log(`[FILTER] Checking filter for ${recipientPub.slice(0, 8)}...`, filter.required_attributes);
		
		return true; // TODO: Implement attribute matching when recipient attributes are available
	} catch (error) {
		console.error('[FILTER] Error evaluating filter:', error);
		return false; // Reject on filter errors for safety
	}
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
// HIGH-LEVEL API (Uses Holster Stores)
// ═══════════════════════════════════════════════════════════════════

/**
 * Compute and publish my allocation state
 * 
 * Call this when:
 * - Network commitments change
 * - My capacity changes
 * - Round advances
 */
export async function computeAndPublishAllocations() {
	const myPub = get(myPubKey);
	if (!myPub) {
		console.warn('[ALLOCATION] Cannot compute: no pubKey');
		return;
	}
	
	const myCommitment = get(myCommitmentStore);
	if (!myCommitment || !myCommitment.capacity) {
		console.warn('[ALLOCATION] Cannot compute: no capacity');
		return;
	}
	
	const myWeights = get(myRecognitionWeightsStore);
	if (!myWeights) {
		console.warn('[ALLOCATION] Cannot compute: no recognition weights');
		return;
	}
	
	// Get network data
	const commitments = getNetworkCommitmentsRecord();
	const networkWeights = getNetworkRecognitionWeightsRecord();
	
	// Compute MR values
	const mrValues = computeAllMutualRecognition(myPub, myWeights, networkWeights);
	
	// Compute allocations
	const allocationState = computeTwoTierAllocation(
		myPub,
		myCommitment.capacity,
		'default',
		mrValues,
		myWeights,
		commitments
	);
	
	// Update convergence tracking
	updatePreviousDenominators(allocationState);
	
	// Publish to network
	await myAllocationStateStore.set(allocationState);
	
	console.log('[ALLOCATION] Computed and published allocation state');
}

/**
 * Update my commitment with damping and publish
 * 
 * Call this after receiving allocations from providers
 */
export async function updateAndPublishCommitment(totalReceived: number) {
	const commitment = get(myCommitmentStore);
	if (!commitment) {
		console.warn('[ALLOCATION] Cannot update: no commitment');
		return;
	}
	
	const updated = updateCommitmentDamping(commitment, totalReceived);
	await myCommitmentStore.set(updated);
	
	console.log('[ALLOCATION] Updated and published commitment with damping');
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
		isConverged: get(hasSystemConverged)
	});
	
	// Log damping details for oscillating participants
	if (oscillating.length > 0) {
		console.log('[DAMPING] Oscillating participants:');
		const commitments = getNetworkCommitmentsRecord();
		oscillating.forEach(pubKey => {
			const c = commitments[pubKey];
			if (c) {
				console.log(`  ${pubKey.slice(0, 20)}...: factor=${c.damping_factor?.toFixed(2)}, history=[${c.over_allocation_history?.map(h => h.toFixed(1)).join(', ')}]`);
			}
		});
	}
}

if (typeof window !== 'undefined') {
	(window as any).debugAllocation = logSubgroupState;
	(window as any).computeTwoTierAllocation = computeTwoTierAllocation;
	(window as any).updateCommitmentDamping = updateCommitmentDamping;
	(window as any).computeDampingFactor = computeDampingFactor;
}

// ═══════════════════════════════════════════════════════════════════
// ALGORITHM-DRIVEN SUBSCRIPTION MANAGEMENT
// ═══════════════════════════════════════════════════════════════════

import { debounce } from '$lib/utils/debounce';
import {
	subscribeToFullParticipant,
	subscribeToCommitment,
	subscribeToAllocationState,
	subscribeToRecognitionWeights,
	subscribeToRoundState,
	unsubscribeFromParticipant
} from './stores.svelte';

/**
 * Reactive subscription management
 * Auto-subscribes/unsubscribes based on algorithm's needs
 */
const debouncedSyncMutualSubscriptions = debounce((partners: string[]) => {
	console.log(`[ALGO-SUB] Syncing ${partners.length} mutual partners`);
	
	// For mutual partners, subscribe to FULL data
	partners.forEach(pubKey => {
		subscribeToFullParticipant(pubKey);
	});
	
	// TODO: Unsubscribe from removed partners (need to track previous state)
}, 100);

const debouncedSyncNonMutualBeneficiarySubscriptions = debounce((beneficiaries: string[]) => {
	console.log(`[ALGO-SUB] Syncing ${beneficiaries.length} non-mutual beneficiaries`);
	
	// For non-mutual beneficiaries, only need commitments (to see their needs)
	beneficiaries.forEach(pubKey => {
		subscribeToCommitment(pubKey);
	});
}, 100);

const debouncedSyncNonMutualProviderSubscriptions = debounce((providers: string[]) => {
	console.log(`[ALGO-SUB] Syncing ${providers.length} non-mutual providers`);
	
	// For non-mutual providers, need commitments + allocation states
	providers.forEach(pubKey => {
		subscribeToCommitment(pubKey);
		subscribeToAllocationState(pubKey);
	});
}, 100);

const debouncedSyncRoundStateSubscriptions = debounce((participants: string[]) => {
	console.log(`[ALGO-SUB] Syncing ${participants.length} round states`);
	
	// Subscribe to round states from all active participants
	participants.forEach(pubKey => {
		const myPub = get(myPubKey);
		if (pubKey !== myPub) { // Don't subscribe to ourselves
			subscribeToRoundState(pubKey);
		}
	});
}, 100);

// ═══════════════════════════════════════════════════════════════════
// INITIALIZATION & CLEANUP
// ═══════════════════════════════════════════════════════════════════

let roundStateGossipInterval: ReturnType<typeof setInterval> | null = null;

/**
 * Start periodic round state gossip
 * Publishes our round state every 5 seconds for P2P coordination
 */
function startRoundStateGossip() {
	if (roundStateGossipInterval) {
		clearInterval(roundStateGossipInterval);
	}
	
	// Publish immediately
	publishMyRoundState();
	
	// Then periodically
	roundStateGossipInterval = setInterval(() => {
		publishMyRoundState();
	}, ROUND_GOSSIP_INTERVAL_MS);
	
	console.log('[ALGO] Started round state gossip');
}

/**
 * Stop round state gossip
 */
function stopRoundStateGossip() {
	if (roundStateGossipInterval) {
		clearInterval(roundStateGossipInterval);
		roundStateGossipInterval = null;
	}
	console.log('[ALGO] Stopped round state gossip');
}

/**
 * Initialize algorithm-driven subscriptions
 * 
 * Call this after:
 * - Holster authentication
 * - Store initialization (initializeAllocationStores)
 * 
 * This sets up reactive subscription management based on algorithm needs.
 */
export function initializeAlgorithmSubscriptions() {
	console.log('[ALGO] Initializing algorithm-driven subscriptions...');
	
	// Priority: Subscribe to mutual partners (full data exchange)
	allMutualPartners.subscribe(debouncedSyncMutualSubscriptions);
	
	// Secondary: Subscribe to non-mutual beneficiaries (commitments only)
	myNonMutualBeneficiaries.subscribe(debouncedSyncNonMutualBeneficiarySubscriptions);
	
	// Secondary: Subscribe to non-mutual providers (commitments + allocations)
	nonMutualProvidersForMe.subscribe(debouncedSyncNonMutualProviderSubscriptions);
	
	// Round coordination: Subscribe to all active participants' round states
	activeParticipants.subscribe(debouncedSyncRoundStateSubscriptions);
	
	// Start round state gossip
	startRoundStateGossip();
	
	console.log('[ALGO] Algorithm-driven subscriptions initialized ✓');
}

/**
 * Cleanup algorithm subscriptions
 * 
 * Call this before:
 * - Logout
 * - Component unmount
 */
export function cleanupAlgorithmSubscriptions() {
	console.log('[ALGO] Cleaning up algorithm subscriptions...');
	
	// Stop round state gossip
	stopRoundStateGossip();
	
	console.log('[ALGO] Algorithm subscriptions cleaned up ✓');
}

// ═══════════════════════════════════════════════════════════════════
// ENHANCED PUBLISHING FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Publish my commitment (enhanced with vector clocks & MR)
 * 
 * Automatically:
 * - Increments vector clock
 * - Computes MR values
 * - Adds recognition weights
 * - Persists to Holster
 */
export async function publishMyCommitment(commitment: Commitment) {
	// Increment vector clock
	await incrementMyVectorClock();
	
	const myPub = get(myPubKey);
	if (!myPub) {
		console.warn('[PUBLISH] Cannot publish: no pubKey');
		return;
	}
	
	// Compute MR values before publishing
	const mrValues = get(myMutualRecognition);
	const recognitionWeights = get(myRecognitionWeightsStore);
	
	// Add metadata
	const enrichedCommitment: Commitment = {
		...commitment,
		mr_values: mrValues,
		recognition_weights: recognitionWeights || {},
		vectorClock: get(myVectorClock),
		round: get(myCurrentRound),
		timestamp: Date.now()
	};
	
	// Persist via Holster store (auto-validated)
	await myCommitmentStore.set(enrichedCommitment);
	
	console.log('[PUBLISH] Published commitment:', {
		residualNeed: enrichedCommitment.residual_need,
		capacity: enrichedCommitment.capacity,
		mutualCount: Object.keys(enrichedCommitment.mr_values || {}).length,
		round: enrichedCommitment.round
	});
}

/**
 * Publish my recognition weights
 */
export async function publishMyRecognitionWeights(weights: Record<string, number>) {
	await myRecognitionWeightsStore.set(weights);
	
	console.log('[PUBLISH] Published recognition weights:', {
		count: Object.keys(weights).length,
		total: Object.values(weights).reduce((sum, w) => sum + w, 0).toFixed(2)
	});
}

