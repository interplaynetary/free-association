/**
 * Algorithm-Driven Subscriptions for Denominator-Centric Fulfillment
 * 
 * Natural subgroups emerge from the algorithm's mathematical structure:
 * - Active Contributors: Providers with Denominator > 0 and Capacity > 0
 * - Active Recipients: Recipients with Residual-Need > 0 and in some numerator
 * - Mutual Recognition Pairs: Fixed recognition relationships (MR > 0)
 * - Satisfied/Saturated: Participants at rest (denominator or need = 0)
 */

import { writable, derived, get } from 'svelte/store';
import type { Writable, Readable } from 'svelte/store';

// ═══════════════════════════════════════════════════════════════════
// CONSTANTS
// ═══════════════════════════════════════════════════════════════════

/**
 * Staleness threshold for dropout detection
 * Participants with commitments older than this are considered dropped out
 * and excluded from active subgroups
 */
export const STALE_THRESHOLD_MS = 60000; // 60 seconds

/**
 * Convergence epsilon
 * Denominators must change by less than this to be considered converged
 */
export const CONVERGENCE_EPSILON = 0.001;

/**
 * Round gossip interval
 * How often participants gossip their current round state
 */
export const ROUND_GOSSIP_INTERVAL_MS = 5000; // 5 seconds

/**
 * Round advancement threshold
 * Participant advances round when this % of active participants are ahead
 */
export const ROUND_ADVANCEMENT_THRESHOLD = 0.5; // 50%

// ═══════════════════════════════════════════════════════════════════
// TYPES
// ═══════════════════════════════════════════════════════════════════

/**
 * Vector Clock for causal ordering
 * Maps participant pubKey → their logical clock value
 */
interface VectorClock {
	[pubKey: string]: number;
}

/**
 * Round State published by each participant
 */
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
	mr_values?: Record<string, number>;
	timestamp: number;
	vectorClock?: VectorClock; // Causal ordering for commitment
	round?: number; // Round when commitment was published
}

interface ProviderAllocationState {
	totalWeightedNeed: Record<string, number>; // capacity_id -> total recognition-weighted need from all recipients
	timestamp: number;
}

// Legacy type alias for backwards compatibility
type DenominatorData = ProviderAllocationState;

interface AllocationState {
	allocated_amount: number;
	recipient_id: string;
	capacity_id: string;
	timestamp: number;
}

interface ParticipantState {
	pubKey: string;
	commitment: Commitment | null;
	denominators: DenominatorData | null;
	allocations: Record<string, AllocationState> | null;
}

// ═══════════════════════════════════════════════════════════════════
// RAW DATA STORES (populated by Holster subscriptions)
// ═══════════════════════════════════════════════════════════════════

/**
 * My own data (always available)
 */
export const myCommitment: Writable<Commitment | null> = writable(null);
export const myAllocationState: Writable<ProviderAllocationState | null> = writable(null); // My total weighted needs from recipients
export const myRecognitionWeights: Writable<Record<string, number>> = writable({}); // pubKey -> MR value (how much I recognize them)

/**
 * Network data from other participants
 * Key: participant pubKey
 */
export const networkCommitments: Writable<Record<string, Commitment>> = writable({});
export const networkProviderStates: Writable<Record<string, ProviderAllocationState>> = writable({}); // Their total weighted needs
export const networkRecognitionWeights: Writable<Record<string, Record<string, number>>> = writable({}); // Their MR values

// ═══════════════════════════════════════════════════════════════════
// VECTOR CLOCK-BASED ROUND COORDINATION (No Coordinators!)
// ═══════════════════════════════════════════════════════════════════

/**
 * My vector clock
 * Tracks my logical time and observed times of other participants
 */
export const myVectorClock: Writable<VectorClock> = writable({});

/**
 * My current round number
 * Derived from my vector clock and network round states
 */
export const myCurrentRound: Writable<number> = writable(0);

/**
 * Network round states from other participants
 * Key: participant pubKey
 */
export const networkRoundStates: Writable<Record<string, RoundState>> = writable({});

// ═══════════════════════════════════════════════════════════════════
// DERIVED SUBGROUPS (emerge from algorithm structure)
// ═══════════════════════════════════════════════════════════════════

/**
 * GROUP 1: My Beneficiaries (people I recognize and might allocate to)
 * People I recognize - I need their residual needs to compute my total weighted need
 * 
 * Formula: TotalWeightedNeed(Me) = Σ[RecognitionWeight(Me→R) × ResidualNeed(R)] for all R I recognize
 * Data needed: Their commitments (residual_need, stated_need)
 */
export const myBeneficiaries: Readable<string[]> = derived(
	[myRecognitionWeights],
	([$myRecognitionWeights]) => {
		return Object.keys($myRecognitionWeights).filter(pubKey => $myRecognitionWeights[pubKey] > 0);
	}
);

// Legacy alias
export const myTreeMembers = myBeneficiaries;

/**
 * GROUP 2: Providers Who Might Allocate To Me
 * People who recognize ME and have capacity - I need their total weighted need to compute my share
 * 
 * Formula: MyShare(Provider) = Capacity(Provider) × RecognitionWeight(Provider→Me) × MyResidualNeed / TotalWeightedNeed(Provider)
 * Data needed: Their commitments (capacity), their total weighted need from all recipients
 */
export const providersWhoRecognizeMe: Readable<string[]> = derived(
	[networkCommitments, networkProviderStates],
	([$networkCommitments, $networkProviderStates]) => {
		const providers: string[] = [];
		const now = Date.now();
		
		for (const pubKey in $networkCommitments) {
			const commitment = $networkCommitments[pubKey];
			const providerState = $networkProviderStates[pubKey];
			
			// Check freshness - exclude stale commitments (dropout detection)
			const isFresh = commitment.timestamp && (now - commitment.timestamp < STALE_THRESHOLD_MS);
			if (!isFresh) {
				console.log(`[DROPOUT] Excluding ${pubKey.slice(0, 20)}... - stale commitment (${now - commitment.timestamp}ms old)`);
				continue;
			}
			
			// Has capacity AND has computed their total weighted need
			const hasCapacity = commitment.capacity && commitment.capacity > 0;
			const hasComputedWeightedNeeds = providerState && Object.values(providerState.totalWeightedNeed).some(need => need > 0);
			
			// Recognizes me (has my pubKey in their MR values)
			const recognizesMe = commitment.mr_values && Object.keys(commitment.mr_values).includes(get(myPubKey) || '');
			
			if (hasCapacity && hasComputedWeightedNeeds && recognizesMe) {
				providers.push(pubKey);
			}
		}
		
		return providers;
	}
);

// Legacy alias
export const activeContributors = providersWhoRecognizeMe;

/**
 * GROUP 3: Beneficiaries With Unmet Needs
 * People I recognize who still have residual needs - they contribute to my total weighted need
 * 
 * Formula: RecognitionWeightedNeed(R) = RecognitionWeight(Me→R) × ResidualNeed(R)
 * Data needed: Their commitments (residual_need)
 */
export const beneficiariesWithNeeds: Readable<string[]> = derived(
	[myBeneficiaries, networkCommitments],
	([$myBeneficiaries, $networkCommitments]) => {
		const now = Date.now();
		
		return $myBeneficiaries.filter(pubKey => {
			const commitment = $networkCommitments[pubKey];
			
			// Exclude if no commitment or stale (dropout detection)
			if (!commitment) return false;
			
			const isFresh = commitment.timestamp && (now - commitment.timestamp < STALE_THRESHOLD_MS);
			if (!isFresh) {
				console.log(`[DROPOUT] Excluding beneficiary ${pubKey.slice(0, 20)}... - stale commitment`);
				return false;
			}
			
			// Must have residual need to be active
			return commitment.residual_need > 0;
		});
	}
);

// Legacy alias
export const activeRecipients = beneficiariesWithNeeds;

/**
 * GROUP 4: Mutual Recognition Pairs (bidirectional)
 * People I recognize who also recognize me - full coordination needed
 */
export const mutualRecognitionPairs: Readable<string[]> = derived(
	[myBeneficiaries, providersWhoRecognizeMe],
	([$myBeneficiaries, $providersWhoRecognizeMe]) => {
		const myBeneficiariesSet = new Set($myBeneficiaries);
		return $providersWhoRecognizeMe.filter(pubKey => myBeneficiariesSet.has(pubKey));
	}
);

// Legacy alias
export const mutualContributors = mutualRecognitionPairs;

/**
 * GROUP 5: Satisfied Beneficiaries (no remaining need)
 * People I recognize whose needs are fully met - no longer contribute to my weighted need
 */
export const satisfiedBeneficiaries: Readable<string[]> = derived(
	[myBeneficiaries, networkCommitments],
	([$myBeneficiaries, $networkCommitments]) => {
		return $myBeneficiaries.filter(pubKey => {
			const commitment = $networkCommitments[pubKey];
			return commitment && commitment.residual_need === 0;
		});
	}
);

// Legacy alias
export const satisfiedRecipients = satisfiedBeneficiaries;

/**
 * GROUP 6: Providers With Zero Demand (no weighted needs)
 * Providers whose total weighted need is zero - nobody needs their capacity
 */
export const providersWithoutDemand: Readable<string[]> = derived(
	[networkProviderStates],
	([$networkProviderStates]) => {
		const idle: string[] = [];
		
		for (const pubKey in $networkProviderStates) {
			const providerState = $networkProviderStates[pubKey];
			const allZero = Object.values(providerState.totalWeightedNeed).every(need => need === 0);
			
			if (allZero) {
				idle.push(pubKey);
			}
		}
		
		return idle;
	}
);

// Legacy alias
export const saturatedProviders = providersWithoutDemand;

/**
 * GROUP 7: All Known Participants
 * Union of everyone we know about (for discovery)
 */
export const allKnownParticipants: Readable<string[]> = derived(
	[myTreeMembers, networkCommitments],
	([$myTreeMembers, $networkCommitments]) => {
		const known = new Set<string>($myTreeMembers);
		Object.keys($networkCommitments).forEach(pubKey => known.add(pubKey));
		return Array.from(known);
	}
);

/**
 * GROUP 8: Active Participants (fresh commitments only)
 * All participants with recent activity - excludes dropouts
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

/**
 * Track previous round's total weighted needs to detect convergence
 * When total weighted need stops changing, allocation shares have converged
 */
export const previousTotalWeightedNeeds: Writable<Record<string, Record<string, number>>> = writable({});

export const hasSystemConverged: Readable<boolean> = derived(
	[myAllocationState, previousTotalWeightedNeeds],
	([$myAllocationState, $previousTotalWeightedNeeds]) => {
		if (!$myAllocationState) return false;
		
		const myPub = get(myPubKey);
		if (!myPub) return false;
		
		const previousWeightedNeeds = $previousTotalWeightedNeeds[myPub];
		if (!previousWeightedNeeds) return false;
		
		// Check if my total weighted needs have stabilized across all capacities
		for (const capacityId in $myAllocationState.totalWeightedNeed) {
			const currentWeightedNeed = $myAllocationState.totalWeightedNeed[capacityId];
			const previousWeightedNeed = previousWeightedNeeds[capacityId] || 0;
			
			const change = Math.abs(currentWeightedNeed - previousWeightedNeed);
			if (change > CONVERGENCE_EPSILON) {
				return false; // Still changing - not converged
			}
		}
		
		return true; // All stable - converged!
	}
);

// Legacy aliases
export const previousDenominators = previousTotalWeightedNeeds;
export const isConverged = hasSystemConverged;

/**
 * Track which beneficiaries have oscillating needs (for damping)
 */
export const oscillatingBeneficiaries: Writable<Set<string>> = writable(new Set());

// Legacy alias
export const oscillatingRecipients = oscillatingBeneficiaries;

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION MANAGEMENT
// ═══════════════════════════════════════════════════════════════════

/**
 * Track active subscriptions by group
 */
export const activeSubscriptions = {
	beneficiaries: writable<Set<string>>(new Set()),
	contributors: writable<Set<string>>(new Set()),
	mutual: writable<Set<string>>(new Set()),
	coordinators: writable<Set<string>>(new Set())
};

/**
 * Subscription state summary (for debugging)
 */
export const subscriptionSummary: Readable<{
	beneficiaries: number;
	contributors: number;
	mutual: number;
	total: number;
}> = derived(
	[
		activeSubscriptions.beneficiaries,
		activeSubscriptions.contributors,
		activeSubscriptions.mutual
	],
	([$beneficiaries, $contributors, $mutual]) => {
		return {
			beneficiaries: $beneficiaries.size,
			contributors: $contributors.size,
			mutual: $mutual.size,
			total: $beneficiaries.size + $contributors.size - $mutual.size // Subtract overlap
		};
	}
);

// ═══════════════════════════════════════════════════════════════════
// VECTOR CLOCK OPERATIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Compare two vector clocks
 * Returns: -1 if vc1 < vc2, 0 if concurrent, 1 if vc1 > vc2
 */
function compareVectorClocks(vc1: VectorClock, vc2: VectorClock): number {
	let vc1Greater = false;
	let vc2Greater = false;
	
	// Get all participants from both clocks
	const allParticipants = new Set([...Object.keys(vc1), ...Object.keys(vc2)]);
	
	for (const pubKey of allParticipants) {
		const v1 = vc1[pubKey] || 0;
		const v2 = vc2[pubKey] || 0;
		
		if (v1 > v2) vc1Greater = true;
		if (v2 > v1) vc2Greater = true;
	}
	
	if (vc1Greater && !vc2Greater) return 1;  // vc1 > vc2
	if (vc2Greater && !vc1Greater) return -1; // vc1 < vc2
	return 0; // Concurrent
}

/**
 * Merge two vector clocks (take maximum of each entry)
 */
function mergeVectorClocks(vc1: VectorClock, vc2: VectorClock): VectorClock {
	const merged: VectorClock = { ...vc1 };
	
	for (const pubKey in vc2) {
		merged[pubKey] = Math.max(merged[pubKey] || 0, vc2[pubKey]);
	}
	
	return merged;
}

/**
 * Increment my vector clock entry
 */
export function incrementMyVectorClock() {
	const myPub = get(myPubKey);
	if (!myPub) return;
	
	myVectorClock.update(vc => ({
		...vc,
		[myPub]: (vc[myPub] || 0) + 1
	}));
}

/**
 * Update my vector clock when receiving data from another participant
 */
export function updateVectorClockFromPeer(peerPubKey: string, peerVectorClock: VectorClock) {
	myVectorClock.update(myVC => {
		// Merge with peer's clock
		const merged = mergeVectorClocks(myVC, peerVectorClock);
		
		// Increment my own entry
		const myPub = get(myPubKey);
		if (myPub) {
			merged[myPub] = (merged[myPub] || 0) + 1;
		}
		
		return merged;
	});
}

/**
 * Check if I should advance to next round based on network round states
 */
function shouldAdvanceRound(): boolean {
	const myPub = get(myPubKey);
	const myRound = get(myCurrentRound);
	const roundStates = get(networkRoundStates);
	const active = get(activeParticipants);
	
	if (!myPub || active.length === 0) return false;
	
	// Count how many active participants are ahead of me
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
	
	// Advance if threshold % of peers are ahead
	const ratio = ahead / total;
	return ratio >= ROUND_ADVANCEMENT_THRESHOLD;
}

/**
 * Advance to next round
 */
export function advanceToNextRound() {
	const myPub = get(myPubKey);
	if (!myPub) return;
	
	// Increment my round
	const newRound = get(myCurrentRound) + 1;
	myCurrentRound.set(newRound);
	
	// Increment my vector clock
	incrementMyVectorClock();
	
	console.log(`[VECTOR-CLOCK] Advanced to round ${newRound}`, {
		vectorClock: get(myVectorClock)
	});
	
	// Publish my new round state
	publishMyRoundState();
}

/**
 * Publish my current round state to network
 */
export function publishMyRoundState() {
	const myPub = get(myPubKey);
	if (!myPub || !holsterUser) return;
	
	const roundState: RoundState = {
		pubKey: myPub,
		round: get(myCurrentRound),
		vectorClock: get(myVectorClock),
		timestamp: Date.now()
	};
	
	// Publish to my user-space
	holsterUser.get('roundState').put(roundState);
	
	console.log(`[VECTOR-CLOCK] Published round state:`, roundState);
}

/**
 * Handle incoming round state from peer
 */
export function handlePeerRoundState(peerPubKey: string, roundState: RoundState) {
	// Update network round states
	networkRoundStates.update(states => ({
		...states,
		[peerPubKey]: roundState
	}));
	
	// Update my vector clock from peer
	updateVectorClockFromPeer(peerPubKey, roundState.vectorClock);
	
	// Check if I should advance round
	if (shouldAdvanceRound()) {
		advanceToNextRound();
	}
}

// ═══════════════════════════════════════════════════════════════════
// HELPER STORES (for implementation)
// ═══════════════════════════════════════════════════════════════════

/**
 * My public key (needed for filtering)
 */
export const myPubKey: Writable<string | null> = writable(null);

/**
 * Set my public key (called after authentication)
 */
export function setMyPubKey(pubKey: string) {
	myPubKey.set(pubKey);
	
	// Initialize my vector clock entry
	myVectorClock.update(vc => ({
		...vc,
		[pubKey]: 0
	}));
}

/**
 * Update my recognition weights (when I change who I recognize)
 */
export function updateMyRecognitionWeights(weights: Record<string, number>) {
	myRecognitionWeights.set(weights);
}

// Legacy alias
export const updateMyTree = updateMyRecognitionWeights;

/**
 * Update my commitment (when my needs or capacity change)
 */
export function updateMyCommitment(commitment: Commitment) {
	myCommitment.set(commitment);
}

/**
 * Update network commitment (from subscription)
 */
export function updateNetworkCommitment(pubKey: string, commitment: Commitment) {
	networkCommitments.update(commitments => ({
		...commitments,
		[pubKey]: commitment
	}));
}

/**
 * Update a provider's total weighted need state (from subscription)
 */
export function updateProviderTotalWeightedNeed(pubKey: string, providerState: ProviderAllocationState) {
	// Store previous for convergence tracking
	const current = get(networkProviderStates)[pubKey];
	if (current) {
		previousTotalWeightedNeeds.update(prev => ({
			...prev,
			[pubKey]: current.totalWeightedNeed
		}));
	}
	
	networkProviderStates.update(states => ({
		...states,
		[pubKey]: providerState
	}));
}

// Legacy alias
export const updateNetworkDenominators = updateProviderTotalWeightedNeed;

/**
 * Update a participant's recognition weights (from subscription)
 */
export function updateNetworkRecognitionWeights(pubKey: string, weights: Record<string, number>) {
	networkRecognitionWeights.update(allWeights => ({
		...allWeights,
		[pubKey]: weights
	}));
}

// Legacy alias
export const updateNetworkTree = updateNetworkRecognitionWeights;

/**
 * Clear participant data (when they leave or become inactive)
 */
export function clearParticipantData(pubKey: string) {
	networkCommitments.update(commitments => {
		const { [pubKey]: _, ...rest } = commitments;
		return rest;
	});
	
	networkProviderStates.update(providerStates => {
		const { [pubKey]: _, ...rest } = providerStates;
		return rest;
	});
	
	networkRecognitionWeights.update(recognitionWeights => {
		const { [pubKey]: _, ...rest } = recognitionWeights;
		return rest;
	});
}

// ═══════════════════════════════════════════════════════════════════
// LOGGING / DEBUGGING
// ═══════════════════════════════════════════════════════════════════

/**
 * Debug: Log current subgroup state
 */
export function logSubgroupState() {
	console.log('[SUBGROUPS] Current state:', {
		myTreeMembers: get(myTreeMembers).length,
		activeContributors: get(activeContributors).length,
		activeRecipients: get(activeRecipients).length,
		mutualContributors: get(mutualContributors).length,
		satisfiedRecipients: get(satisfiedRecipients).length,
		saturatedProviders: get(saturatedProviders).length,
		activeParticipants: get(activeParticipants).length,
		allKnown: get(allKnownParticipants).length,
		subscriptionSummary: get(subscriptionSummary),
		isConverged: get(isConverged)
	});
}

if (typeof window !== 'undefined') {
	(window as any).debugSubgroups = logSubgroupState;
}

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION MANAGER (following network.svelte.ts pattern)
// ═══════════════════════════════════════════════════════════════════

import { debounce } from '$lib/utils/debounce';

/**
 * Holster user instance (will be set by initialization)
 */
let holsterUser: any = null;

export function initializeHolsterUser(user: any) {
	holsterUser = user;
	console.log('[ELEGANT] Holster user initialized');
}

/**
 * Subscribe to my beneficiaries (people I recognize)
 * Data needed: commitments (residual_need, stated_need)
 * Why: To compute my total recognition-weighted need as provider
 * Formula: TotalWeightedNeed = Σ[RecognitionWeight(me→beneficiary) × ResidualNeed(beneficiary)]
 */
const debouncedSyncBeneficiarySubscriptions = debounce((members: string[]) => {
	if (!holsterUser) {
		console.warn('[ELEGANT] Cannot subscribe - holsterUser not initialized');
		return;
	}
	
	const currentSubs = get(activeSubscriptions.beneficiaries);
	const memberSet = new Set(members);
	const mutualSet = new Set(get(mutualContributors));
	
	// Remove subscriptions for members no longer in tree
	currentSubs.forEach(pubKey => {
		if (!memberSet.has(pubKey) && !mutualSet.has(pubKey)) {
			// Don't actually unsubscribe (Holster doesn't have clean unsubscribe)
			// Just remove from tracking
			activeSubscriptions.beneficiaries.update(subs => {
				const newSubs = new Set(subs);
				newSubs.delete(pubKey);
				return newSubs;
			});
			console.log(`[BENEFICIARY] Removed ${pubKey.slice(0, 20)}... from tracking`);
		}
	});
	
	// Add subscriptions for new members
	members.forEach(pubKey => {
		// Skip if already subscribed as mutual
		if (mutualSet.has(pubKey)) {
			return;
		}
		
		if (!currentSubs.has(pubKey)) {
			// Subscribe to their commitments
			holsterUser.get([pubKey, 'commitments']).on((data: any) => {
				if (data) {
					updateNetworkCommitment(pubKey, data);
					console.log(`[BENEFICIARY] Received commitment from ${pubKey.slice(0, 20)}...`, {
						residualNeed: data.residual_need,
						statedNeed: data.stated_need
					});
				}
			}, true);
			
			activeSubscriptions.beneficiaries.update(subs => {
				const newSubs = new Set(subs);
				newSubs.add(pubKey);
				return newSubs;
			});
			
			console.log(`[BENEFICIARY] Subscribed to ${pubKey.slice(0, 20)}... commitments`);
		}
	});
}, 100);

/**
 * Subscribe to providers who recognize me
 * Data needed: commitments (capacity), their total weighted needs
 * Why: To compute my share of their capacity
 * Formula: MyShare = Capacity × RecognitionWeight(provider→me) × MyResidualNeed / TotalWeightedNeed(provider)
 */
const debouncedSyncContributorSubscriptions = debounce((contributors: string[]) => {
	if (!holsterUser) {
		console.warn('[ELEGANT] Cannot subscribe - holsterUser not initialized');
		return;
	}
	
	const currentSubs = get(activeSubscriptions.contributors);
	const contributorSet = new Set(contributors);
	const mutualSet = new Set(get(mutualContributors));
	
	// Remove subscriptions for people who no longer contribute
	currentSubs.forEach(pubKey => {
		if (!contributorSet.has(pubKey) && !mutualSet.has(pubKey)) {
			activeSubscriptions.contributors.update(subs => {
				const newSubs = new Set(subs);
				newSubs.delete(pubKey);
				return newSubs;
			});
			console.log(`[CONTRIBUTOR] Removed ${pubKey.slice(0, 20)}... from tracking`);
		}
	});
	
	// Add subscriptions for new contributors
	contributors.forEach(pubKey => {
		// Skip if already subscribed as mutual
		if (mutualSet.has(pubKey)) {
			return;
		}
		
		if (!currentSubs.has(pubKey)) {
			// Subscribe to their commitments (for capacity)
			holsterUser.get([pubKey, 'commitments']).on((data: any) => {
				if (data) {
					updateNetworkCommitment(pubKey, data);
					console.log(`[CONTRIBUTOR] Received commitment from ${pubKey.slice(0, 20)}...`, {
						capacity: data.capacity
					});
				}
			}, true);
			
			// Subscribe to their total weighted needs (to compute my share)
			holsterUser.get([pubKey, 'denominators']).on((data: any) => {
				if (data) {
					updateProviderTotalWeightedNeed(pubKey, data);
					console.log(`[PROVIDER] Received total weighted needs from ${pubKey.slice(0, 20)}...`);
				}
			}, true);
			
			activeSubscriptions.contributors.update(subs => {
				const newSubs = new Set(subs);
				newSubs.add(pubKey);
				return newSubs;
			});
			
			console.log(`[CONTRIBUTOR] Subscribed to ${pubKey.slice(0, 20)}... capacity & denominators`);
		}
	});
}, 100);

/**
 * Subscribe to mutual recognition pairs (bidirectional)
 * Data needed: everything (commitments, total weighted needs, recognition weights, allocations, compose)
 * Why: Full coordination when we recognize each other - both provider and recipient roles
 */
const debouncedSyncMutualSubscriptions = debounce((mutualList: string[]) => {
	if (!holsterUser) {
		console.warn('[ELEGANT] Cannot subscribe - holsterUser not initialized');
		return;
	}
	
	const currentSubs = get(activeSubscriptions.mutual);
	const mutualSet = new Set(mutualList);
	
	// Remove subscriptions for people no longer mutual
	currentSubs.forEach(pubKey => {
		if (!mutualSet.has(pubKey)) {
			activeSubscriptions.mutual.update(subs => {
				const newSubs = new Set(subs);
				newSubs.delete(pubKey);
				return newSubs;
			});
			console.log(`[MUTUAL] Removed ${pubKey.slice(0, 20)}... from tracking`);
		}
	});
	
	// Add subscriptions for new mutual contributors
	mutualList.forEach(pubKey => {
		if (!currentSubs.has(pubKey)) {
			// Subscribe to ALL their data
			
			// Commitments
			holsterUser.get([pubKey, 'commitments']).on((data: any) => {
				if (data) {
					updateNetworkCommitment(pubKey, data);
				}
			}, true);
			
			// Their total weighted needs
			holsterUser.get([pubKey, 'denominators']).on((data: any) => {
				if (data) {
					updateProviderTotalWeightedNeed(pubKey, data);
				}
			}, true);
			
			// Their recognition weights
			holsterUser.get([pubKey, 'tree']).on((data: any) => {
				if (data) {
					updateNetworkRecognitionWeights(pubKey, data);
				}
			}, true);
			
			// Allocation states
			holsterUser.get([pubKey, 'allocationStates']).on((data: any) => {
				if (data) {
					console.log(`[MUTUAL] Received allocations from ${pubKey.slice(0, 20)}...`);
					// Store allocation states (implement as needed)
				}
			}, true);
			
			// Compose data (if needed)
			holsterUser.get([pubKey, 'desiredSlotComposeFrom']).on((data: any) => {
				if (data) {
					console.log(`[MUTUAL] Received compose-from from ${pubKey.slice(0, 20)}...`);
					// Store compose data (implement as needed)
				}
			}, true);
			
			holsterUser.get([pubKey, 'desiredSlotComposeInto']).on((data: any) => {
				if (data) {
					console.log(`[MUTUAL] Received compose-into from ${pubKey.slice(0, 20)}...`);
					// Store compose data (implement as needed)
				}
			}, true);
			
			activeSubscriptions.mutual.update(subs => {
				const newSubs = new Set(subs);
				newSubs.add(pubKey);
				return newSubs;
			});
			
			console.log(`[MUTUAL] Subscribed to ${pubKey.slice(0, 20)}... full data`);
		}
	});
}, 100);

/**
 * Subscribe to round states from active participants
 * Data needed: round number, vector clock
 * Why: Coordinate round advancement via gossip
 */
const debouncedSyncRoundStateSubscriptions = debounce((participants: string[]) => {
	if (!holsterUser) {
		console.warn('[ELEGANT-CLOCK] Cannot subscribe - holsterUser not initialized');
		return;
	}
	
	const myPub = get(myPubKey);
	if (!myPub) return;
	
	const currentSubs = get(activeSubscriptions.coordinators); // Reuse for round state tracking
	const participantSet = new Set(participants);
	
	// Remove subscriptions for inactive participants
	currentSubs.forEach(pubKey => {
		if (!participantSet.has(pubKey)) {
			activeSubscriptions.coordinators.update(subs => {
				const newSubs = new Set(subs);
				newSubs.delete(pubKey);
				return newSubs;
			});
			console.log(`[VECTOR-CLOCK] Removed round state subscription for ${pubKey.slice(0, 20)}...`);
		}
	});
	
	// Add subscriptions for new participants
	participants.forEach(pubKey => {
		// Skip myself
		if (pubKey === myPub) return;
		
		if (!currentSubs.has(pubKey)) {
			// Subscribe to their round state
			holsterUser.get([pubKey, 'roundState']).on((roundState: any) => {
				if (roundState && roundState.round !== undefined) {
					console.log(`[VECTOR-CLOCK] Received round state from ${pubKey.slice(0, 20)}...`, {
						round: roundState.round,
						vectorClock: roundState.vectorClock
					});
					handlePeerRoundState(pubKey, roundState);
				}
			}, true);
			
			activeSubscriptions.coordinators.update(subs => {
				const newSubs = new Set(subs);
				newSubs.add(pubKey);
				return newSubs;
			});
			
			console.log(`[VECTOR-CLOCK] Subscribed to ${pubKey.slice(0, 20)}... round state`);
		}
	});
}, 100);

// ═══════════════════════════════════════════════════════════════════
// REACTIVE SUBSCRIPTION SETUP (following network.svelte.ts pattern)
// ═══════════════════════════════════════════════════════════════════

/**
 * Initialize subscriptions based on derived stores
 * Call this after authentication
 */
export function initializeAlgorithmSubscriptions() {
	console.log('[ELEGANT-CLOCK] Initializing algorithm-driven subscriptions with vector clocks...');
	
	// Subscribe to beneficiaries (my tree members)
	myTreeMembers.subscribe(debouncedSyncBeneficiarySubscriptions);
	
	// Subscribe to contributors
	activeContributors.subscribe(debouncedSyncContributorSubscriptions);
	
	// Subscribe to mutual contributors
	mutualContributors.subscribe(debouncedSyncMutualSubscriptions);
	
	// Subscribe to round states from ALL active participants (vector clock gossip)
	activeParticipants.subscribe(debouncedSyncRoundStateSubscriptions);
	
	// Start periodic round state gossip
	startRoundStateGossip();
	
	console.log('[ELEGANT-CLOCK] Algorithm-driven subscriptions initialized');
}

/**
 * Periodic round state gossip
 * Publishes my round state at regular intervals
 */
let roundStateGossipInterval: ReturnType<typeof setInterval> | null = null;

function startRoundStateGossip() {
	// Clear existing interval if any
	if (roundStateGossipInterval) {
		clearInterval(roundStateGossipInterval);
	}
	
	// Publish immediately
	publishMyRoundState();
	
	// Then publish periodically
	roundStateGossipInterval = setInterval(() => {
		publishMyRoundState();
	}, ROUND_GOSSIP_INTERVAL_MS);
	
	console.log(`[VECTOR-CLOCK] Started round state gossip (every ${ROUND_GOSSIP_INTERVAL_MS}ms)`);
}

/**
 * Cleanup subscriptions (if needed)
 */
export function cleanupAlgorithmSubscriptions() {
	console.log('[ELEGANT-CLOCK] Cleaning up subscriptions...');
	
	// Stop round state gossip
	if (roundStateGossipInterval) {
		clearInterval(roundStateGossipInterval);
		roundStateGossipInterval = null;
	}
	
	activeSubscriptions.beneficiaries.set(new Set());
	activeSubscriptions.contributors.set(new Set());
	activeSubscriptions.mutual.set(new Set());
	activeSubscriptions.coordinators.set(new Set());
}

// ═══════════════════════════════════════════════════════════════════
// PUBLISHING MY STATE WITH VECTOR CLOCKS
// ═══════════════════════════════════════════════════════════════════

/**
 * Publish my commitment (needs, capacity, recognition weights) with vector clock
 * This tells the network my current state so providers can compute my share
 */
export function publishMyCommitment(commitment: Commitment) {
	// Increment my vector clock (marking this as a new event)
	incrementMyVectorClock();
	
	// Add vector clock and round to commitment for causal ordering
	const commitmentWithClock: Commitment = {
		...commitment,
		vectorClock: get(myVectorClock),
		round: get(myCurrentRound),
		timestamp: Date.now()
	};
	
	// Update local store
	myCommitment.set(commitmentWithClock);
	
	// Publish to network so others can see my needs
	if (holsterUser) {
		holsterUser.get('commitments').put(commitmentWithClock);
		console.log('[PUBLISH] Published my needs & capacity:', {
			residualNeed: commitmentWithClock.residual_need,
			capacity: commitmentWithClock.capacity,
			round: commitmentWithClock.round
		});
	}
}

/**
 * Get causally-consistent snapshot of commitments
 * Only includes commitments that happened-before or concurrent with my view
 * This ensures I don't use "future" data when computing allocations
 */
export function getCausallyConsistentCommitments(): Record<string, Commitment> {
	const myVC = get(myVectorClock);
	const allCommitments = get(networkCommitments);
	const snapshot: Record<string, Commitment> = {};
	
	for (const [pubKey, commitment] of Object.entries(allCommitments)) {
		// Include if commitment has no vector clock (old data) or is causally before/concurrent
		if (!commitment.vectorClock || compareVectorClocks(commitment.vectorClock, myVC) <= 0) {
			snapshot[pubKey] = commitment;
		} else {
			console.log(`[CAUSAL-ORDER] Excluding ${pubKey.slice(0, 20)}... - future commitment (haven't seen their events yet)`);
		}
	}
	
	return snapshot;
}

// Legacy alias
export const getCommitmentsSnapshot = getCausallyConsistentCommitments;

// ═══════════════════════════════════════════════════════════════════
// USAGE EXAMPLE
// ═══════════════════════════════════════════════════════════════════

/*

## Integration Example (Vector Clock Version)

```typescript
import {
	initializeHolsterUser,
	initializeAlgorithmSubscriptions,
	setMyPubKey,
	updateMyTree,
	publishMyCommitment,
	advanceToNextRound,
	getCommitmentsSnapshot,
	myTreeMembers,
	activeContributors,
	mutualContributors,
	myCurrentRound,
	myVectorClock,
	subscriptionSummary,
	isConverged
} from '$lib/commons/elegant-clock.svelte';
import { holsterUser } from '$lib/state/holster.svelte';

// After Holster authentication:
async function onHolsterAuthenticated() {
	// 1. Initialize Holster user
	initializeHolsterUser(holsterUser);
	
	// 2. Set my public key (also initializes vector clock)
	setMyPubKey(holsterUser.is.pub);
	
	// 3. Initialize algorithm-driven subscriptions with vector clock gossip
	// This will reactively subscribe based on tree and recognition changes
	// AND start gossiping round state
	initializeAlgorithmSubscriptions();
	
	// 4. Update my tree (this triggers beneficiary subscriptions)
	updateMyTree({
		'alice_pub_key': 0.3,
		'bob_pub_key': 0.7
	});
}

// Watch subscription state in components:
$: console.log('Subscription summary:', $subscriptionSummary);
// Output: { beneficiaries: 2, contributors: 3, mutual: 1, total: 4 }

// Watch convergence:
$: if ($isConverged) {
	console.log('System has converged!');
}

// Watch vector clock state:
$: console.log('My vector clock:', $myVectorClock);
$: console.log('My current round:', $myCurrentRound);

// When needs/capacity change:
function onNeedsChanged(residual_need: number, stated_need: number, capacity: number) {
	publishMyCommitment({
		residual_need,
		stated_need,
		capacity,
		mr_values: $myTree,
		timestamp: Date.now()
	});
	// Vector clock automatically incremented and included
}

// When computing allocations (use causal snapshot):
function computeAllocations() {
	// Get commitments that are causally consistent with my view
	const snapshot = getCommitmentsSnapshot();
	
	// Compute denominators using snapshot
	// ...
}

// Manually advance round (usually automatic via gossip):
function manualAdvance() {
	advanceToNextRound();
	// Vector clock incremented, round state published
}
```

## Key Benefits

1. **No Coordinators**: Vector clocks eliminate centralized round coordinators
2. **Algorithm-Driven**: Subscriptions emerge from actual data dependencies
3. **Reactive**: Svelte stores automatically recompute when dependencies change
4. **Causal Consistency**: Vector clocks ensure proper event ordering
5. **Eventually Consistent**: System converges even with network delays
6. **Efficient**: Debounced updates prevent subscription thrashing
7. **Natural Bounds**: Limited by Dunbar's number (~150-200 max)
8. **Clean Separation**: 
   - Beneficiaries: Need their residual needs (for my denominator)
   - Contributors: Need their capacities and denominators (for my allocations)
   - Mutual: Need everything (bidirectional coordination)
   - Round States: Gossip from all participants (vector clock sync)

## Vector Clock-Based Coordination

### What Problem Does This Solve?

**Problem with Coordinators:**
```
❌ Single point of failure
❌ Requires designated coordinators
❌ Centralized control
❌ Coordinator dropout breaks system
```

**Solution with Vector Clocks:**
```
✅ Fully decentralized
✅ No designated roles
✅ Self-coordinating via gossip
✅ Resilient to any participant dropout
```

### How Vector Clocks Work

**Each participant maintains a vector clock:**
```typescript
myVectorClock = {
  'alice_pub': 5,  // Alice's logical time from my perspective
  'bob_pub': 3,    // Bob's logical time from my perspective
  'me_pub': 7      // My own logical time
}
```

**Clock Operations:**

1. **Increment on local event** (publish commitment, advance round):
   ```typescript
   myVectorClock[myPubKey]++
   ```

2. **Merge on receiving peer data**:
   ```typescript
   myVectorClock = max(myVectorClock, peerVectorClock)
   myVectorClock[myPubKey]++
   ```

3. **Compare for causal ordering**:
   ```typescript
   if (vc1 ≤ vc2): event1 happened-before event2
   if (vc1 || vc2): events are concurrent
   ```

### Round Advancement via Gossip

**Each participant:**
1. Periodically gossips their `(round, vectorClock)` state
2. Subscribes to all active participants' round states
3. Advances round when 50%+ of peers are ahead

**Example:**
```typescript
// I'm at round 3
myCurrentRound = 3

// I observe:
networkRoundStates = {
  'alice_pub': { round: 4, vectorClock: {...} },
  'bob_pub': { round: 4, vectorClock: {...} },
  'carol_pub': { round: 3, vectorClock: {...} },
  'dave_pub': { round: 5, vectorClock: {...} }
}

// 3 out of 4 peers are ahead (75% > 50% threshold)
// → I advance to round 4
advanceToNextRound()
```

### Causal Consistency in Allocations

**Problem:** What if I receive a commitment from the "future"?

```
My vector clock: { alice: 5, bob: 3, me: 7 }
Commitment from Alice: { vectorClock: { alice: 8, bob: 4, me: 6 } }

Alice's clock shows she's "ahead" of me in her own time
But she's also observed events I haven't seen yet (bob: 4 vs my bob: 3)
```

**Solution:** Only use commitments that are causally consistent:

```typescript
// When computing allocations:
const snapshot = getCommitmentsSnapshot()
// Only includes commitments where:
// - commitment.vectorClock ≤ myVectorClock (causally before)
// - OR commitment.vectorClock || myVectorClock (concurrent - OK to use)

// Excludes "future" commitments where:
// - commitment.vectorClock > myVectorClock (they've seen events I haven't)
```

### Gossip Protocol

**Round State Structure:**
```typescript
interface RoundState {
  pubKey: string           // Who published this
  round: number            // Their current round
  vectorClock: VectorClock // Their logical time
  timestamp: number        // Wall clock time
}
```

**Gossip Interval:**
```typescript
const ROUND_GOSSIP_INTERVAL_MS = 5000 // Every 5 seconds

// Each participant publishes:
holsterUser.get('roundState').put({
  pubKey: myPubKey,
  round: myCurrentRound,
  vectorClock: myVectorClock,
  timestamp: Date.now()
})

// Everyone subscribes to everyone's round state:
holsterUser.get([peerPubKey, 'roundState']).on(state => {
  handlePeerRoundState(peerPubKey, state)
})
```

### Example: Round Advancement Flow

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Alice     │     │     Bob     │     │   Carol     │
│  Round 3    │     │   Round 4   │     │   Round 3   │
└─────────────┘     └─────────────┘     └─────────────┘
       │                   │                   │
       │ [Gossip]          │                   │
       ├──────────────────>│                   │
       │ round=3, vc={...} │                   │
       │                   │                   │
       │                   │ [Gossip]          │
       │                   ├──────────────────>│
       │                   │ round=4, vc={...} │
       │                   │                   │
       │                   │<──────────────────┤
       │                   │ round=3, vc={...} │
       │                   │                   │
       │                   │ [Check threshold] │
       │                   │ 50% ahead → stay  │
       │                   │                   │
       │ [Gossip]          │                   │
       ├────────────────────────────────────────>
       │ round=3, vc={...} │                   │
       │                   │                   │
       │                   │                   │ [Check threshold]
       │                   │                   │ 50% ahead → advance!
       │                   │                   │
       │                   │<──────────────────┤
       │<──────────────────┤ round=4, vc={...} │
       │ round=4, vc={...} │                   │
       │                   │                   │
       │ [Check threshold] │                   │
       │ 67% ahead → advance!                  │
       │                   │                   │
       ├──────────────────>│                   │
       │ round=4, vc={...} │                   │
       │                   ├──────────────────>│
       │                   │ round=4, vc={...} │
       │                   │                   │
       │      All at Round 4 - SYNCHRONIZED    │
       └───────────────────┴───────────────────┘
```

### Dropout Resilience

**Scenario: Bob drops out mid-round**

```typescript
// Before dropout:
networkRoundStates = {
  'alice_pub': { round: 4 },
  'bob_pub': { round: 4 },   // ← Bob drops here
  'carol_pub': { round: 3 },
  'dave_pub': { round: 4 }
}

// After 60s (staleness timeout):
activeParticipants = ['alice_pub', 'carol_pub', 'dave_pub']
// Bob excluded from activeParticipants

// Round advancement now uses 3 participants instead of 4
// System continues without Bob
```

**Recovery: Bob rejoins**

```typescript
// Bob publishes fresh round state:
{
  pubKey: 'bob_pub',
  round: 4,  // Catches up by observing others
  vectorClock: { // merged from others },
  timestamp: Date.now()  // Fresh timestamp
}

// Bob immediately reappears in activeParticipants
// System seamlessly includes Bob again
```

## Subscription Flow

```
User edits tree
  ↓
myTree store updates
  ↓
myTreeMembers derived store recomputes
  ↓
debouncedSyncBeneficiarySubscriptions triggers
  ↓
Subscribe to new members, unsubscribe from removed
  ↓
Receive commitment data via Holster
  ↓
networkCommitments updates
  ↓
activeContributors derived store recomputes (detects who recognizes me)
  ↓
debouncedSyncContributorSubscriptions triggers
  ↓
...and so on
```

## Data Flow

```
Holster subscriptions → Raw stores (networkCommitments, networkDenominators)
                              ↓
                        Derived stores (myTreeMembers, activeContributors, etc.)
                              ↓
                        Subscription managers (sync functions)
                              ↓
                        Holster subscriptions (reactive loop)
```

*/
