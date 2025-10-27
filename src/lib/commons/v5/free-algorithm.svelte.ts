/**
 * Free-Association Algorithm - Reactive Implementation
 * 
 * This follows the intuitive naming from free-association.md:
 * - Your-Need, Your-Active-Need, Your-Final-Allocation
 * - Provider's-Available-Capacity, Mutual-Recognition-Share
 * - Damping-Factor, Over-Allocation-History
 * 
 * Implemented as reactive Svelte stores for automatic recomputation.
 * 
 * Key Insight (V5): 
 * - Recognition is GLOBAL (same MR value for all need types)
 * - Type preferences encoded in recognition tree structure
 */

import { derived, writable, get } from 'svelte/store';
import type { Readable, Writable } from 'svelte/store';

// Import v5 schemas and stores
import type {
	Commitment,
	NeedSlot,
	AvailabilitySlot,
	GlobalRecognitionWeights,
	SlotAllocationRecord
} from './schemas';

import {
	myCommitmentStore,
	networkCommitments,
	getAllCommitmentsRecord,
	getNetworkRecognitionWeightsRecord,
	holsterUserPub,
	networkNeedsIndex,
	type SpaceTimeIndex
} from './stores.svelte';

import { normalizeGlobalRecognitionWeights } from './schemas';
import { slotsCompatible, passesSlotFilters, type FilterContext, getTimeBucketKey, getLocationBucketKey } from './match.svelte';

// Import ITC functions for causal consistency
import {
    type Stamp as ITCStamp,
	seed as itcSeed,
	event as itcEvent,
	join as itcJoin,
	leq as itcLeq,
	equals as itcEquals,
	toString as itcToString
} from '../utils/itc';

// ═══════════════════════════════════════════════════════════════════
// ITC STATE (CAUSAL CONSISTENCY FOR PEER-TO-PEER)
// ═══════════════════════════════════════════════════════════════════

/**
 * My ITC Stamp - Tracks causal history of my state changes
 * 
 * In plain English: "What version of my state am I on?"
 * Ensures everyone sees consistent history across the peer-to-peer network
 */
let myITCStamp: ITCStamp = itcSeed();

/**
 * Get My Current ITC Stamp
 */
export function getMyITCStamp(): ITCStamp {
	return myITCStamp;
}

/**
 * Increment My ITC Stamp (call when I make a state change)
 */
export function incrementMyITCStamp(): void {
	myITCStamp = itcEvent(myITCStamp);
	console.log(`[ITC] My stamp updated: ${itcToString(myITCStamp)}`);
}

/**
 * Merge ITC Stamp from Peer (call when receiving updates)
 */
export function mergeITCStampFromPeer(peerStamp: ITCStamp): void {
	const oldStamp = myITCStamp;
	myITCStamp = itcJoin(myITCStamp, peerStamp);
	
	if (!itcEquals(oldStamp, myITCStamp)) {
		console.log(`[ITC] Merged peer stamp: ${itcToString(myITCStamp)}`);
	}
}

/**
 * Check if Peer Update is Stale (already seen)
 */
export function isPeerUpdateStale(peerStamp: ITCStamp): boolean {
	return itcLeq(peerStamp, myITCStamp) && itcEquals(peerStamp, myITCStamp);
}

/**
 * Get Causally Consistent Commitments
 * Only includes commitments we've causally seen
 */
export function getCausallyConsistentCommitments(): Record<string, Commitment> {
	const allCommitments = getAllCommitmentsRecord();
	const snapshot: Record<string, Commitment> = {};
	
	for (const [pubKey, commitment] of Object.entries(allCommitments)) {
		if (!commitment.itcStamp || itcLeq(commitment.itcStamp, myITCStamp)) {
			snapshot[pubKey] = commitment;
		}
	}
	
	return snapshot;
}

// ═══════════════════════════════════════════════════════════════════
// PART I: MY IDENTITY & RECOGNITION
// ═══════════════════════════════════════════════════════════════════

/**
 * My Public Key (identity in the network)
 * Re-exported from stores for convenience
 */
export const myPublicKey = holsterUserPub;

/**
 * My Recognition of Others
 * "I recognize Alice 30%, Bob 40%, Carol 30%" (must sum to 100%)
 * 
 * V5: Extracted from commitment (recognition stored in commitment.global_recognition_weights)
 */
export const myRecognitionOfOthers: Readable<GlobalRecognitionWeights> = derived(
	[myCommitmentStore],
	([$commitment]) => {
		if (!$commitment?.global_recognition_weights) return {};
		return normalizeGlobalRecognitionWeights($commitment.global_recognition_weights);
	}
);

/**
 * Others' Recognition of Me
 * "Alice recognizes me 50%, Bob recognizes me 60%"
 */
export const othersRecognitionOfMe: Readable<Record<string, GlobalRecognitionWeights>> = derived(
	[], // Updated when networkRecognitionWeights changes
	() => getNetworkRecognitionWeightsRecord()
);

/**
 * My Mutual Recognition with Everyone (Global - same for all types)
 * Mutual-Recognition(Me, Alice) = min(My-Recognition-of-Alice, Alice's-Recognition-of-Me)
 */
export const myMutualRecognition: Readable<GlobalRecognitionWeights> = derived(
	[myPublicKey, myRecognitionOfOthers, othersRecognitionOfMe],
	([$myPub, $myRec, $othersRec]) => {
		if (!$myPub) return {};
		
		const mutualRec: GlobalRecognitionWeights = {};
		
		// For everyone I recognize
		for (const theirPub in $myRec) {
			const myRecOfThem = $myRec[theirPub] || 0;
			const theirRecOfMe = $othersRec[theirPub]?.[$myPub] || 0;
			mutualRec[theirPub] = Math.min(myRecOfThem, theirRecOfMe);
		}
		
		// For everyone who recognizes me (that I didn't already check)
		for (const theirPub in $othersRec) {
			if (mutualRec[theirPub] !== undefined) continue; // Already computed
			const theirRecOfMe = $othersRec[theirPub]?.[$myPub] || 0;
			const myRecOfThem = $myRec[theirPub] || 0;
			mutualRec[theirPub] = Math.min(myRecOfThem, theirRecOfMe);
		}
		
		return mutualRec;
	}
);

// ═══════════════════════════════════════════════════════════════════
// PART II: NEEDS & CAPACITY
// ═══════════════════════════════════════════════════════════════════

/**
 * My Current Needs (by type)
 * "I need 40 meals, 10 hours of tutoring, 2 checkups"
 */
export const myCurrentNeeds: Readable<Record<string, number>> = derived(
	[myCommitmentStore],
	([$commitment]) => {
		if (!$commitment?.need_slots) return {};
		
		const needsByType: Record<string, number> = {};
		for (const slot of $commitment.need_slots) {
			const typeId = slot.need_type_id;
			needsByType[typeId] = (needsByType[typeId] || 0) + slot.quantity;
		}
		
		return needsByType;
	}
);

/**
 * My Available Capacity (by type)
 * "I can provide 100 meals, 20 hours of tutoring"
 */
export const myAvailableCapacity: Readable<Record<string, number>> = derived(
	[myCommitmentStore],
	([$commitment]) => {
		if (!$commitment?.capacity_slots) return {};
		
		const capacityByType: Record<string, number> = {};
		for (const slot of $commitment.capacity_slots) {
			const typeId = slot.need_type_id;
			capacityByType[typeId] = (capacityByType[typeId] || 0) + slot.quantity;
		}
		
		return capacityByType;
	}
);

// ═══════════════════════════════════════════════════════════════════
// PART III: DAMPING (SELF-CORRECTION)
// ═══════════════════════════════════════════════════════════════════

/**
 * Over-Allocation History (per type)
 * Tracks how much excess we received in the last 3 allocations
 */
export const overAllocationHistory: Writable<Record<string, number[]>> = writable({});

/**
 * Damping Factor (per type)
 * 1.0 = full speed (smooth convergence)
 * 0.8 = medium speed (default)
 * 0.5 = slow down (oscillation detected)
 */
export const dampingFactors: Readable<Record<string, number>> = derived(
	[overAllocationHistory],
	([$history]) => {
		const factors: Record<string, number> = {};
		
		for (const [typeId, history] of Object.entries($history)) {
			if (history.length < 3) {
				factors[typeId] = 0.8; // Medium speed by default
				continue;
			}
			
			// Check last 3 entries for oscillation
			const recent = history.slice(-3);
			const upDownUp = recent[0] < recent[1] && recent[1] > recent[2];
			const downUpDown = recent[0] > recent[1] && recent[1] < recent[2];
			
			if (upDownUp || downUpDown) {
				factors[typeId] = 0.5; // Slow down (oscillation)
			} else {
				// Check for smooth convergence (monotonically decreasing)
				const isSmooth = recent[0] >= recent[1] && recent[1] >= recent[2];
				factors[typeId] = isSmooth ? 1.0 : 0.8; // Full speed or medium
			}
		}
		
		return factors;
	}
);

/**
 * My Active Needs (damped)
 * Active-Need = Stated-Need × Damping-Factor
 */
export const myActiveNeeds: Readable<Record<string, number>> = derived(
	[myCurrentNeeds, dampingFactors],
	([$needs, $factors]) => {
		const activeNeeds: Record<string, number> = {};
		
		for (const [typeId, need] of Object.entries($needs)) {
			const factor = $factors[typeId] || 0.8; // Default medium speed
			activeNeeds[typeId] = need * factor;
		}
		
		return activeNeeds;
	}
);

// ═══════════════════════════════════════════════════════════════════
// SYSTEM STATE (CONVERGENCE TRACKING)
// ═══════════════════════════════════════════════════════════════════

/**
 * System State - Tracks everyone's needs and capacity over time
 * 
 * In plain English: "What's the current state of the whole network?"
 * Used to track convergence: Are needs decreasing? How fast?
 */
interface SystemStateSnapshot {
	/** Everyone's needs by type: { alice: { food: 40 }, bob: { tutoring: 10 } } */
	needsByPersonAndType: Record<string, Record<string, number>>;
	
	/** Everyone's capacity by type: { kitchen: { food: 100 }, teacher: { tutoring: 20 } } */
	capacityByPersonAndType: Record<string, Record<string, number>>;
	
	/** When this snapshot was taken */
	timestamp: number;
	
	/** Which iteration (how many times have we allocated?) */
	iteration: number;
	
	/** ITC stamp for causal consistency */
	itcStamp: ITCStamp;
}

/**
 * Current System State
 */
let currentSystemState: SystemStateSnapshot = {
	needsByPersonAndType: {},
	capacityByPersonAndType: {},
	timestamp: Date.now(),
	iteration: 0,
	itcStamp: itcSeed()
};

/**
 * Previous System State (for comparing: are we converging?)
 */
let previousSystemState: SystemStateSnapshot | null = null;

/**
 * Get Current System State
 */
export function getCurrentSystemState(): SystemStateSnapshot {
	return currentSystemState;
}

/**
 * Update System State from Network
 * Rebuild the state snapshot from current commitments
 */
export function updateSystemStateFromNetwork(): void {
	const commitments = getCausallyConsistentCommitments();
	
	const needsByPersonAndType: Record<string, Record<string, number>> = {};
	const capacityByPersonAndType: Record<string, Record<string, number>> = {};
	
	// Aggregate needs by person and type
	for (const [pubKey, commitment] of Object.entries(commitments)) {
		if (commitment.need_slots && commitment.need_slots.length > 0) {
			const needsByType: Record<string, number> = {};
			for (const slot of commitment.need_slots) {
				const typeId = slot.need_type_id;
				needsByType[typeId] = (needsByType[typeId] || 0) + slot.quantity;
			}
			needsByPersonAndType[pubKey] = needsByType;
		}
		
		// Aggregate capacity by person and type
		if (commitment.capacity_slots && commitment.capacity_slots.length > 0) {
			const capacityByType: Record<string, number> = {};
			for (const slot of commitment.capacity_slots) {
				const typeId = slot.need_type_id;
				capacityByType[typeId] = (capacityByType[typeId] || 0) + slot.quantity;
			}
			capacityByPersonAndType[pubKey] = capacityByType;
		}
	}
	
	// Store previous state for convergence tracking
	previousSystemState = { ...currentSystemState };
	
	// Update current state
	currentSystemState = {
		needsByPersonAndType,
		capacityByPersonAndType,
		timestamp: Date.now(),
		iteration: currentSystemState.iteration + 1,
		itcStamp: getMyITCStamp()
	};
	
	const peopleCount = Object.keys(needsByPersonAndType).length;
	const typeCount = new Set(
		Object.values(needsByPersonAndType).flatMap(needs => Object.keys(needs))
	).size;
	
	console.log(`[STATE] Updated: ${peopleCount} people, ${typeCount} need types, iteration ${currentSystemState.iteration}`);
}

// ═══════════════════════════════════════════════════════════════════
// CONVERGENCE METRICS (ARE WE GETTING CLOSER TO ZERO NEEDS?)
// ═══════════════════════════════════════════════════════════════════

/**
 * Total Need Magnitude - How much total need is in the system?
 * 
 * In plain English: "Add up everyone's needs across all types"
 * Formula: sqrt(sum of all needs squared) - Frobenius norm
 */
export function computeTotalNeedMagnitude(state: SystemStateSnapshot): number {
	let sumSquares = 0;
	
	for (const needsByType of Object.values(state.needsByPersonAndType)) {
		for (const need of Object.values(needsByType)) {
			sumSquares += need ** 2;
		}
	}
	
	return Math.sqrt(sumSquares);
}

/**
 * Contraction Rate - How fast are needs shrinking?
 * 
 * In plain English: "What fraction of needs remain after one iteration?"
 * - If 0.8: needs shrunk by 20% (good!)
 * - If 1.0: needs stayed the same (no progress)
 * - If > 1.0: needs grew (bad! shouldn't happen)
 */
export function computeContractionRate(
	currentMagnitude: number,
	previousMagnitude: number
): number {
	if (previousMagnitude < 0.001) return 0;
	return currentMagnitude / previousMagnitude;
}

/**
 * Percent Needs Met - What fraction of people are fully satisfied?
 * 
 * In plain English: "What % of people have zero needs?"
 */
export function computePercentNeedsMet(state: SystemStateSnapshot): number {
	let totalPeople = 0;
	let satisfiedPeople = 0;
	
	for (const needsByType of Object.values(state.needsByPersonAndType)) {
		totalPeople++;
		
		// Check if all their needs are near zero
		const allNeedsMet = Object.values(needsByType).every(need => need < 0.001);
		if (allNeedsMet) {
			satisfiedPeople++;
		}
	}
	
	if (totalPeople === 0) return 100;
	return (satisfiedPeople / totalPeople) * 100;
}

/**
 * Universal Satisfaction Achieved? - Are ALL needs zero?
 * 
 * In plain English: "Is everyone completely satisfied?"
 */
export function checkUniversalSatisfaction(state: SystemStateSnapshot): boolean {
	for (const needsByType of Object.values(state.needsByPersonAndType)) {
		for (const need of Object.values(needsByType)) {
			if (need > 0.001) {
				return false; // Someone still has unmet needs
			}
		}
	}
	return true; // Everyone satisfied!
}

/**
 * Iterations to Convergence - How many more iterations until everyone satisfied?
 * 
 * In plain English: "If we keep going at this rate, how long until needs hit zero?"
 */
export function estimateIterationsToConvergence(
	currentMagnitude: number,
	contractionRate: number
): number | null {
	if (contractionRate >= 1) return null; // Not converging
	if (contractionRate <= 0) return 0; // Already there
	if (currentMagnitude < 0.001) return 0; // Already there
	
	// Formula: log(target/current) / log(rate)
	const targetMagnitude = 0.001; // Close enough to zero
	const iterations = Math.log(targetMagnitude / currentMagnitude) / Math.log(contractionRate);
	
	return Math.max(0, Math.ceil(iterations));
}

/**
 * Convergence Summary - All metrics in one place
 */
export interface ConvergenceSummary {
	/** Total magnitude of all needs (Frobenius norm) */
	totalNeedMagnitude: number;
	
	/** Previous magnitude (for comparison) */
	previousNeedMagnitude: number;
	
	/** Contraction rate (should be < 1) */
	contractionRate: number;
	
	/** Are needs below threshold? */
	isConverged: boolean;
	
	/** % of people with all needs met */
	percentNeedsMet: number;
	
	/** Are ALL needs zero? */
	universalSatisfaction: boolean;
	
	/** Estimated iterations remaining */
	iterationsToConvergence: number | null;
	
	/** Which iteration are we on? */
	currentIteration: number;
	
	/** Response time for this iteration (ms) */
	responseLatency: number;
	
	/** Worst-case participant's total need magnitude */
	maxPersonNeed?: number;
	
	/** Distribution inequality (variance of needs across participants) */
	needVariance?: number;
	
	/** Number of participants with unchanging needs (stuck) */
	peopleStuck?: number;
}

/**
 * Compute maximum need across all participants
 * Returns the worst-case participant's total need magnitude
 */
export function computeMaxPersonNeed(state: SystemStateSnapshot): number {
	let maxNeed = 0;
	
	for (const needsByType of Object.values(state.needsByPersonAndType)) {
		// Compute this person's total need across all types (Frobenius norm)
		let personNeedSquared = 0;
		for (const need of Object.values(needsByType)) {
			personNeedSquared += need ** 2;
		}
		const personNeed = Math.sqrt(personNeedSquared);
		maxNeed = Math.max(maxNeed, personNeed);
	}
	
	return maxNeed;
}

/**
 * Compute variance of need distribution
 * High variance = some people have much more unmet need than others
 */
export function computeNeedVariance(state: SystemStateSnapshot): number {
	const personNeeds: number[] = [];
	
	for (const needsByType of Object.values(state.needsByPersonAndType)) {
		// Compute this person's total need
		let personNeedSquared = 0;
		for (const need of Object.values(needsByType)) {
			personNeedSquared += need ** 2;
		}
		personNeeds.push(Math.sqrt(personNeedSquared));
	}
	
	if (personNeeds.length === 0) return 0;
	
	// Compute mean
	const mean = personNeeds.reduce((sum, need) => sum + need, 0) / personNeeds.length;
	
	// Compute variance
	const variance = personNeeds.reduce((sum, need) => sum + (need - mean) ** 2, 0) / personNeeds.length;
	
	return variance;
}

/**
 * Count how many participants have unchanging needs (stuck)
 * A participant is "stuck" if their need hasn't changed since last iteration
 */
export function computePeopleStuck(
	currentState: SystemStateSnapshot,
	previousState: SystemStateSnapshot | null
): number {
	if (!previousState) return 0;
	
	let stuckCount = 0;
	const epsilon = 0.001; // Small threshold for "no change"
	
	for (const [person, currentNeeds] of Object.entries(currentState.needsByPersonAndType)) {
		const previousNeeds = previousState.needsByPersonAndType[person];
		if (!previousNeeds) continue;
		
		// Compute current and previous total need for this person
		let currentTotal = 0;
		let previousTotal = 0;
		
		for (const [type, need] of Object.entries(currentNeeds)) {
			currentTotal += need ** 2;
			previousTotal += (previousNeeds[type] || 0) ** 2;
		}
		
		currentTotal = Math.sqrt(currentTotal);
		previousTotal = Math.sqrt(previousTotal);
		
		// If need hasn't changed (within epsilon) and is still > 0, person is stuck
		if (Math.abs(currentTotal - previousTotal) < epsilon && currentTotal > epsilon) {
			stuckCount++;
		}
	}
	
	return stuckCount;
}

/**
 * Compute Full Convergence Summary
 */
export function computeConvergenceSummary(
	currentState: SystemStateSnapshot,
	previousState: SystemStateSnapshot | null,
	iterationStartTime: number
): ConvergenceSummary {
	const currentMagnitude = computeTotalNeedMagnitude(currentState);
	const previousMagnitude = previousState 
		? computeTotalNeedMagnitude(previousState)
		: currentMagnitude * 2; // Assume we started twice as high
	
	const contractionRate = computeContractionRate(currentMagnitude, previousMagnitude);
	const isConverged = currentMagnitude < 0.001;
	const percentNeedsMet = computePercentNeedsMet(currentState);
	const universalSatisfaction = checkUniversalSatisfaction(currentState);
	const iterationsToConvergence = estimateIterationsToConvergence(currentMagnitude, contractionRate);
	
	// Additional distribution metrics
	const maxPersonNeed = computeMaxPersonNeed(currentState);
	const needVariance = computeNeedVariance(currentState);
	const peopleStuck = computePeopleStuck(currentState, previousState);
	
	const now = Date.now();
	const responseLatency = now - iterationStartTime;
	
	console.log(`[CONVERGENCE-DETAIL] Max need: ${maxPersonNeed.toFixed(2)}, Variance: ${needVariance.toFixed(2)}, Stuck: ${peopleStuck}`);
	
	return {
		totalNeedMagnitude: currentMagnitude,
		previousNeedMagnitude: previousMagnitude,
		contractionRate,
		isConverged,
		percentNeedsMet,
		universalSatisfaction,
		iterationsToConvergence,
		currentIteration: currentState.iteration,
		responseLatency,
		// Additional metrics
		maxPersonNeed,
		needVariance,
		peopleStuck
	};
}

// ═══════════════════════════════════════════════════════════════════
// SPATIAL/TEMPORAL OPTIMIZATION (Using Reactive Indexes)
// ═══════════════════════════════════════════════════════════════════

/**
 * Get candidate recipients for a capacity slot using spatial/temporal indexes
 * 
 * This is O(k) where k = size of filtered set, instead of O(N) for full scan
 * 
 * @param capacitySlot - The capacity slot to find recipients for
 * @param needsIndex - The reactive needs index
 * @returns Set of pubKeys that potentially need this capacity
 */
export function getCandidateRecipients(
	capacitySlot: AvailabilitySlot,
	needsIndex: SpaceTimeIndex
): Set<string> {
	const typeId = capacitySlot.need_type_id;
	const locationKey = getLocationBucketKey(capacitySlot);
	const timeKey = getTimeBucketKey(capacitySlot);
	
	// Strategy: Use most specific index available
	
	// 1. Try full composite (most specific)
	const fullKey = `${typeId}|${locationKey}|${timeKey}`;
	if (needsIndex.byAll.has(fullKey)) {
		console.log(`[INDEX-LOOKUP] Full composite hit: ${needsIndex.byAll.get(fullKey)!.size} candidates`);
		return needsIndex.byAll.get(fullKey)!;
	}
	
	// 2. Try type + location
	const typeLocKey = `${typeId}|${locationKey}`;
	if (needsIndex.byTypeAndLocation.has(typeLocKey)) {
		console.log(`[INDEX-LOOKUP] Type+Location hit: ${needsIndex.byTypeAndLocation.get(typeLocKey)!.size} candidates`);
		return needsIndex.byTypeAndLocation.get(typeLocKey)!;
	}
	
	// 3. Try type + time
	const typeTimeKey = `${typeId}|${timeKey}`;
	if (needsIndex.byTypeAndTime.has(typeTimeKey)) {
		console.log(`[INDEX-LOOKUP] Type+Time hit: ${needsIndex.byTypeAndTime.get(typeTimeKey)!.size} candidates`);
		return needsIndex.byTypeAndTime.get(typeTimeKey)!;
	}
	
	// 4. Fall back to type only
	if (needsIndex.byType.has(typeId)) {
		console.log(`[INDEX-LOOKUP] Type-only hit: ${needsIndex.byType.get(typeId)!.size} candidates`);
		return needsIndex.byType.get(typeId)!;
	}
	
	// 5. No candidates found
	console.log(`[INDEX-LOOKUP] No candidates found for ${typeId}`);
	return new Set();
}

// ═══════════════════════════════════════════════════════════════════
// PART IV: ALLOCATION COMPUTATION (TWO-TIER SYSTEM)
// ═══════════════════════════════════════════════════════════════════

/**
 * Compute allocations when I'm the provider
 * Following free-association.md naming and logic
 * 
 * Tier 1: Mutual Recognition First
 * - Allocate to those with MR > 0 who need this type
 * - Normalize MR shares over this filtered set
 * 
 * Tier 2: Generous Giving Second
 * - Allocate remaining capacity to those I recognize (but not mutual)
 * - Normalize recognition shares over this filtered set
 */
export const myAllocationsAsProvider: Readable<{
	allocations: SlotAllocationRecord[];
	totalsByTypeAndRecipient: Record<string, Record<string, number>>;
	convergence: ConvergenceSummary | null;
	slotDenominators: Record<string, { mutual: number; nonMutual: number; need_type_id: string }>;
}> = derived<
	[typeof myPublicKey, typeof myMutualRecognition, typeof myRecognitionOfOthers, typeof myCommitmentStore],
	{ allocations: SlotAllocationRecord[]; totalsByTypeAndRecipient: Record<string, Record<string, number>>; convergence: ConvergenceSummary | null; slotDenominators: Record<string, { mutual: number; nonMutual: number; need_type_id: string }> }
>(
	[
		myPublicKey,
		myMutualRecognition,
		myRecognitionOfOthers,
		myCommitmentStore
	],
	([
		$myPub,
		$myMR,
		$myRec,
		$myCommitment
	]) => {
		if (!$myPub || !$myCommitment?.capacity_slots) {
			return { allocations: [], totalsByTypeAndRecipient: {}, convergence: null, slotDenominators: {} };
		}
		
		// Track iteration start time for convergence metrics
		const iterationStartTime = Date.now();
		
	const allocations: SlotAllocationRecord[] = [];
		const totalsByTypeAndRecipient: Record<string, Record<string, number>> = {};
		const slotDenominators: Record<string, { mutual: number; nonMutual: number; need_type_id: string }> = {};
		
		// Process each capacity slot independently
		for (const capacitySlot of $myCommitment.capacity_slots) {
			const typeId = capacitySlot.need_type_id;
			const providersAvailableCapacity = capacitySlot.quantity;
			
			if (providersAvailableCapacity <= 0) continue;
			
			// Initialize totals for this type
			if (!totalsByTypeAndRecipient[typeId]) {
				totalsByTypeAndRecipient[typeId] = {};
			}
			
		// ────────────────────────────────────────────────────────────
		// STEP 0: Build Compatibility Matrix (Space-Time-Type Matching)
		// ────────────────────────────────────────────────────────────
		// For each recipient, find ALL need slots that are compatible with this capacity slot
		// Compatible means: time overlaps + location matches + type matches
		
		const compatibleRecipients = new Map<string, NeedSlot[]>();
		
		// OPTIMIZATION: Use spatial/temporal index for O(1) candidate lookup
		const needsIndexValue = get(networkNeedsIndex);
		const candidatePubKeys = getCandidateRecipients(capacitySlot, needsIndexValue);
		
		console.log(`[ALLOCATION-LOOP] Using indexed lookup: ${candidatePubKeys.size} candidates (was O(N) scan)`);
		
		// Get ALL commitments (including our own for self-allocation!)
		const allCommitments = getAllCommitmentsRecord();
		
		// Only check candidates from index (O(k) instead of O(N))
		for (const recipientPub of candidatePubKeys) {
			const recipientCommitment = allCommitments[recipientPub];
			if (!recipientCommitment || !recipientCommitment.need_slots) continue;
			
			// Find compatible need slots (checks time + location + type)
			const matchingNeedSlots = recipientCommitment.need_slots.filter(needSlot => {
				return needSlot.need_type_id === typeId && slotsCompatible(needSlot, capacitySlot);
			});
			
			if (matchingNeedSlots.length === 0) continue;
			
			// Check bilateral filters (capacity filter + need filter)
			const providerMR = $myMR[recipientPub] || 0;
			
			const providerContext: FilterContext = {
				pubKey: $myPub,
				commitment: $myCommitment,
				mutualRecognition: providerMR,
				attributes: ($myCommitment as any)?.attributes || {}
			};
			
			const recipientContext: FilterContext = {
				pubKey: recipientPub,
				commitment: recipientCommitment,
				mutualRecognition: providerMR,
				attributes: (recipientCommitment as any)?.attributes || {}
			};
			
			// Filter out slots that don't pass bilateral filters
			const validSlots: NeedSlot[] = [];
			for (const needSlot of matchingNeedSlots) {
				if (passesSlotFilters(needSlot, capacitySlot, providerContext, recipientContext)) {
					validSlots.push(needSlot);
				}
			}
			
			if (validSlots.length > 0) {
				compatibleRecipients.set(recipientPub, validSlots);
			}
		}
			
			if (compatibleRecipients.size === 0) continue; // No compatible recipients for this slot
			
			// ────────────────────────────────────────────────────────────
			// TIER 1: MUTUAL RECOGNITION ALLOCATION
			// ────────────────────────────────────────────────────────────
			
			// Find recipients with mutual recognition who have compatible slots
			const eligibleRecipients: Array<{
				pubKey: string;
				need: number;
				mutualRecognitionShare: number;
				activeNeed: number;
				needSlots: NeedSlot[];
			}> = [];
			
			let totalMutualRecognition = 0;
			let tier1Denominator = 0;
			
			// Step 1: Calculate Mutual-Recognition-Share for each compatible recipient
			for (const [recipientPub, needSlots] of compatibleRecipients.entries()) {
				const mutualRec = $myMR[recipientPub] || 0;
				if (mutualRec <= 0) continue; // Only mutual recognition in Tier 1
				
				totalMutualRecognition += mutualRec;
			}
			
			// Process Tier 1 ONLY if there are mutual recipients
			// Do NOT skip the slot! We need to fall through to Tier 2
			let capacityUsedInTier1 = 0;
			
			if (totalMutualRecognition > 0) {
				// Calculate shares and numerators
			for (const [recipientPub, needSlots] of compatibleRecipients.entries()) {
				const mutualRec = $myMR[recipientPub] || 0;
				if (mutualRec <= 0) continue;
				
				// Your-Mutual-Recognition-Share = Your-MR / sum-of-all-MR (filtered)
				const mutualRecognitionShare = mutualRec / totalMutualRecognition;
				
				// Calculate total need from compatible slots
				let totalNeed = 0;
				for (const slot of needSlots) {
					totalNeed += slot.quantity;
				}
				
				// Get per-type damping factor from recipient's commitment
				const recipientCommitment = allCommitments[recipientPub];
				const dampingFactor = recipientCommitment.multi_dimensional_damping?.damping_factors?.[typeId]
					|| recipientCommitment.multi_dimensional_damping?.global_damping_factor
					|| 1.0; // Default to no damping if not specified
				
				const activeNeed = totalNeed * dampingFactor; // Your-Active-Need = Your-Need × Damping-Factor
				
				eligibleRecipients.push({
					pubKey: recipientPub,
					need: totalNeed,
					mutualRecognitionShare,
					activeNeed,
					needSlots
				});
			}
			
			// Step 2: Calculate denominator (sum of MR-Share × Active-Need)
			let denominator = 0;
			for (const recipient of eligibleRecipients) {
				denominator += recipient.mutualRecognitionShare * recipient.activeNeed;
			}
			
		// HYBRID APPROACH: Use relative epsilon but floor instead of skip
		// This combines safety (relative epsilon) with completeness (always allocate)
		const MIN_RELATIVE_DENOMINATOR = 0.001;
		const minSafeDenominator = providersAvailableCapacity * MIN_RELATIVE_DENOMINATOR;
		
		if (denominator < minSafeDenominator) {
			console.warn(`[ALLOCATION-TIER1] Denominator ${denominator.toFixed(6)} too small, flooring to ${minSafeDenominator.toFixed(6)}`);
			denominator = minSafeDenominator; // Floor instead of skip
		}
			
				// Step 3: Allocate to each recipient (TIER 1: MUTUAL)
			
			for (const recipient of eligibleRecipients) {
				// Your-Raw-Allocation = Capacity × (Your-MR-Share × Your-Active-Need) / denominator
				const yourRawAllocation =
					providersAvailableCapacity *
					(recipient.mutualRecognitionShare * recipient.activeNeed) /
					denominator;
				
			// Your-Final-Allocation = min(Raw-Allocation, Your-Actual-Need)
			const yourFinalAllocation = Math.min(yourRawAllocation, recipient.need);
			
			if (yourFinalAllocation > 0) {
				// PROPORTIONAL DISTRIBUTION across compatible need slots
				// Instead of FIFO (which is order-dependent), distribute proportionally
				const totalCompatibleNeed = recipient.needSlots.reduce((sum, slot) => sum + slot.quantity, 0);
				let actuallyAllocated = 0;
				
				for (const needSlot of recipient.needSlots) {
					// Each slot gets a proportion based on its share of total need
					const proportion = needSlot.quantity / totalCompatibleNeed;
					const slotAllocation = Math.min(
						needSlot.quantity,
						yourFinalAllocation * proportion
					);
					
					if (slotAllocation > 0) {
			allocations.push({
							availability_slot_id: capacitySlot.id,
							recipient_pubkey: recipient.pubKey,
				recipient_need_slot_id: needSlot.id,
				quantity: slotAllocation,
							need_type_id: typeId,
				time_compatible: true,
				location_compatible: true,
				tier: 'mutual'
			});
			
						actuallyAllocated += slotAllocation;
						capacityUsedInTier1 += slotAllocation;
					}
				}
				
				// Track totals
				if (!totalsByTypeAndRecipient[typeId][recipient.pubKey]) {
					totalsByTypeAndRecipient[typeId][recipient.pubKey] = 0;
				}
				totalsByTypeAndRecipient[typeId][recipient.pubKey] += actuallyAllocated;
			}
		}
		
		// Store Tier 1 denominator for this slot
		tier1Denominator = denominator;
	} // End of Tier 1 if block
	
	// Store initial slot denominator (will update after Tier 2)
	slotDenominators[capacitySlot.id] = {
		mutual: tier1Denominator,
		nonMutual: 0, // Will be updated after Tier 2
		need_type_id: typeId
	};
	
	// ────────────────────────────────────────────────────────────
	// TIER 2: NON-MUTUAL RECOGNITION (GENEROUS GIVING)
	// ────────────────────────────────────────────────────────────
	
	const remainingCapacity = providersAvailableCapacity - capacityUsedInTier1;
	
	if (remainingCapacity > 0.0001) {
		// Find recipients with one-way recognition (I recognize them, but not mutual)
		// who have compatible slots
		const nonMutualEligibleRecipients: Array<{
			pubKey: string;
			need: number;
			recognitionShare: number;
			activeNeed: number;
			needSlots: NeedSlot[];
		}> = [];
		
		let totalNonMutualRecognition = 0;
		
		// Step 1: Calculate recognition shares (filtered for compatible slots)
		for (const [recipientPub, needSlots] of compatibleRecipients.entries()) {
			const mutualRec = $myMR[recipientPub] || 0;
			if (mutualRec > 0) continue; // Skip mutual (already allocated in Tier 1)
			
			const myRecOfThem = $myRec[recipientPub] || 0;
			if (myRecOfThem <= 0) continue; // I don't recognize them
			
			totalNonMutualRecognition += myRecOfThem;
		}
		
		if (totalNonMutualRecognition > 0) {
			// Calculate shares and numerators
			for (const [recipientPub, needSlots] of compatibleRecipients.entries()) {
				const mutualRec = $myMR[recipientPub] || 0;
				if (mutualRec > 0) continue;
				
				const myRecOfThem = $myRec[recipientPub] || 0;
				if (myRecOfThem <= 0) continue;
				
				// Your-Recognition-Share = Your-Recognition / sum-of-all-recognition (filtered)
				const recognitionShare = myRecOfThem / totalNonMutualRecognition;
				
				// Calculate total need from compatible slots
				let totalNeed = 0;
				for (const slot of needSlots) {
					totalNeed += slot.quantity;
				}
				
				// Get per-type damping factor from recipient's commitment
				const recipientCommitment = allCommitments[recipientPub];
				const dampingFactor = recipientCommitment.multi_dimensional_damping?.damping_factors?.[typeId]
					|| recipientCommitment.multi_dimensional_damping?.global_damping_factor
					|| 1.0; // Default to no damping if not specified
				
				const activeNeed = totalNeed * dampingFactor; // Your-Active-Need = Your-Need × Damping-Factor
				
				nonMutualEligibleRecipients.push({
					pubKey: recipientPub,
					need: totalNeed,
					recognitionShare,
					activeNeed,
					needSlots
				});
			}
			
		// Step 2: Calculate denominator (sum of Recognition-Share × Active-Need)
		let tier2Denominator = 0;
		for (const recipient of nonMutualEligibleRecipients) {
			tier2Denominator += recipient.recognitionShare * recipient.activeNeed;
		}
		
		// Update slot denominator with Tier 2 info
		if (slotDenominators[capacitySlot.id]) {
			slotDenominators[capacitySlot.id].nonMutual = tier2Denominator;
		}
		
	// HYBRID APPROACH for Tier 2: Use relative epsilon but floor instead of skip
	const MIN_RELATIVE_DENOMINATOR = 0.001;
	const minSafeDenominatorT2 = remainingCapacity * MIN_RELATIVE_DENOMINATOR;
	
	if (tier2Denominator < minSafeDenominatorT2) {
		console.warn(`[ALLOCATION-TIER2] Denominator ${tier2Denominator.toFixed(6)} too small, flooring to ${minSafeDenominatorT2.toFixed(6)}`);
		tier2Denominator = minSafeDenominatorT2; // Floor instead of skip
	}
	
	if (tier2Denominator > 0) { // Always try to allocate if we have recipients
				// Step 3: Allocate from remaining capacity
				for (const recipient of nonMutualEligibleRecipients) {
					// Your-Raw-Allocation = Remaining-Capacity × (Your-Rec-Share × Your-Active-Need) / denominator
					const yourRawAllocation =
						remainingCapacity *
						(recipient.recognitionShare * recipient.activeNeed) /
						tier2Denominator;
					
				// Your-Final-Allocation = min(Raw-Allocation, Your-Actual-Need)
				const yourFinalAllocation = Math.min(yourRawAllocation, recipient.need);
				
				if (yourFinalAllocation > 0) {
					// PROPORTIONAL DISTRIBUTION across compatible need slots (Tier 2)
					const totalCompatibleNeed = recipient.needSlots.reduce((sum, slot) => sum + slot.quantity, 0);
					let actuallyAllocated = 0;
					
					for (const needSlot of recipient.needSlots) {
						// Each slot gets a proportion based on its share of total need
						const proportion = needSlot.quantity / totalCompatibleNeed;
						const slotAllocation = Math.min(
							needSlot.quantity,
							yourFinalAllocation * proportion
						);
						
						if (slotAllocation > 0) {
			allocations.push({
								availability_slot_id: capacitySlot.id,
								recipient_pubkey: recipient.pubKey,
				recipient_need_slot_id: needSlot.id,
				quantity: slotAllocation,
								need_type_id: typeId,
				time_compatible: true,
				location_compatible: true,
				tier: 'non-mutual'
			});
			
							actuallyAllocated += slotAllocation;
						}
					}
					
					// Track totals
					if (!totalsByTypeAndRecipient[typeId][recipient.pubKey]) {
						totalsByTypeAndRecipient[typeId][recipient.pubKey] = 0;
					}
					totalsByTypeAndRecipient[typeId][recipient.pubKey] += actuallyAllocated;
				}
				}
			}
		}
	}
		} // End of capacity slot loop
		
		// Update system state from network (before computing convergence)
		updateSystemStateFromNetwork();
		
		// Compute convergence metrics
		const convergence = computeConvergenceSummary(
			currentSystemState,
			previousSystemState,
			iterationStartTime
		);
		
		// Increment ITC stamp (we made allocations)
		incrementMyITCStamp();
		
		// Log convergence status
		console.log(`[CONVERGENCE] Iteration ${convergence.currentIteration}: ` +
			`magnitude=${convergence.totalNeedMagnitude.toFixed(3)}, ` +
			`rate=${convergence.contractionRate.toFixed(3)}, ` +
			`${convergence.percentNeedsMet.toFixed(0)}% satisfied, ` +
			`${convergence.universalSatisfaction ? '✅ UNIVERSAL SATISFACTION' : `~${convergence.iterationsToConvergence} iterations remaining`}`
		);
		
		// Log slot denominators for debugging
		console.log(`[SLOT-DENOMINATORS] ${Object.keys(slotDenominators).length} capacity slots processed:`,
			Object.entries(slotDenominators).map(([id, info]) => 
				`${id.slice(0, 8)}[${info.need_type_id}]: MR=${info.mutual.toFixed(2)}, NonMR=${info.nonMutual.toFixed(2)}`
			).join(', ')
		);
		
		return { allocations, totalsByTypeAndRecipient, convergence, slotDenominators };
	}
);

// ═══════════════════════════════════════════════════════════════════
// PART V: NEED UPDATE LAW
// ═══════════════════════════════════════════════════════════════════

/**
 * Total I've received (across all providers, by type)
 * This would be computed by aggregating allocations from all providers
 */
export const totalReceivedByType: Writable<Record<string, number>> = writable({});

/**
 * My Needs at Next Step
 * Your-Need-at-Next-Step = max(0, Your-Current-Need - Total-You-Received)
 */
export const myNeedsAtNextStep: Readable<Record<string, number>> = derived(
	[myCurrentNeeds, totalReceivedByType],
	([$currentNeeds, $received]) => {
		const nextNeeds: Record<string, number> = {};
		
		for (const [typeId, currentNeed] of Object.entries($currentNeeds)) {
			const received = $received[typeId] || 0;
			nextNeeds[typeId] = Math.max(0, currentNeed - received);
		}
		
		return nextNeeds;
	}
);

/**
 * Update my commitment with new needs (apply the update law)
 */
export function applyNeedUpdateLaw() {
	const nextNeeds = get(myNeedsAtNextStep);
	const currentCommitment = get(myCommitmentStore);
	
	if (!currentCommitment) return;
	
	// Update need slots with new quantities
	const updatedNeedSlots = currentCommitment.need_slots?.map(slot => ({
		...slot,
		quantity: nextNeeds[slot.need_type_id] || 0
	}));
	
	// Update commitment
	myCommitmentStore.set({
		...currentCommitment,
		need_slots: updatedNeedSlots
	});
}

/**
 * Record allocation received (to update over-allocation history)
 */
export function recordAllocationReceived(typeId: string, amount: number) {
	const currentNeeds = get(myCurrentNeeds);
	const currentNeed = currentNeeds[typeId] || 0;
	
	// Over-allocation is how much excess we received
	const overAllocation = Math.max(0, amount - currentNeed);
	
	// Update history
	overAllocationHistory.update(history => {
		const typeHistory = history[typeId] || [];
	return {
			...history,
			[typeId]: [...typeHistory, overAllocation].slice(-3) // Keep last 3
		};
	});
	
	// Update total received
	totalReceivedByType.update(totals => ({
		...totals,
		[typeId]: (totals[typeId] || 0) + amount
	}));
}

// ═══════════════════════════════════════════════════════════════════
// PART VI: CONVERGENCE DETECTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Universal Satisfaction Achieved?
 * True when all needs are met (all needs at or below epsilon)
 */
export const universalSatisfactionAchieved: Readable<boolean> = derived(
	[myCurrentNeeds],
	([$needs]) => {
		const epsilon = 0.001;
		
		for (const need of Object.values($needs)) {
			if (need > epsilon) return false;
		}
		
		return true;
	}
);

/**
 * Total Need Magnitude (Euclidean norm across all types)
 * ||N_vec|| = sqrt(food² + healthcare² + tutoring² + ...)
 */
export const totalNeedMagnitude: Readable<number> = derived(
	[myCurrentNeeds],
	([$needs]) => {
		let sumOfSquares = 0;
		for (const need of Object.values($needs)) {
			sumOfSquares += need * need;
		}
		return Math.sqrt(sumOfSquares);
	}
);

// ═══════════════════════════════════════════════════════════════════
// PUBLISHING FUNCTIONS (UPDATE NETWORK WITH MY STATE)
// ═══════════════════════════════════════════════════════════════════

/**
 * Publish My Commitment to the Network
 * 
 * In plain English: "Tell everyone about my needs and capacity"
 * Enriches with ITC stamp and recognition for causal consistency
 */
export async function publishMyCommitment(commitment: Commitment): Promise<void> {
	// Increment ITC stamp (we're making a state change)
	incrementMyITCStamp();
	
	const myPub = get(myPublicKey);
	if (!myPub) {
		console.warn('[PUBLISH] Cannot publish: no public key');
		return;
	}
	
	// Get current mutual recognition and normalized recognition weights
	const mrValues = get(myMutualRecognition);
	const recWeights = get(myRecognitionOfOthers);
	
	// Normalize recognition weights before publishing
	const normalizedWeights = normalizeGlobalRecognitionWeights(recWeights);
	
	// Enrich commitment with stamps and recognition
	const enrichedCommitment: Commitment = {
		...commitment,
		global_mr_values: mrValues,
		global_recognition_weights: normalizedWeights,
		itcStamp: getMyITCStamp(),
		timestamp: Date.now()
	};
	
	// Publish to network
	await myCommitmentStore.set(enrichedCommitment);
	
	console.log(`[PUBLISH] Published commitment with ITC stamp ${itcToString(getMyITCStamp())}`);
}

/**
 * Publish My Recognition Weights to the Network
 * 
 * In plain English: "Tell everyone how much I recognize them"
 * Enforces normalization (must sum to 100%)
 * 
 * V5: Updates recognition in commitment (no separate recognition store)
 */
export async function publishMyRecognitionWeights(weights: GlobalRecognitionWeights): Promise<void> {
	// Enforce normalization
	const normalizedWeights = normalizeGlobalRecognitionWeights(weights);
	
	// Validate sum
	const sum = Object.values(normalizedWeights).reduce((a, b) => a + b, 0);
	if (Math.abs(sum - 1.0) > 0.001) {
		console.warn(`[PUBLISH] Recognition weights sum to ${sum.toFixed(4)}, not 1.0. Normalizing...`);
	}
	
	// Get current commitment and update recognition
	const currentCommitment = get(myCommitmentStore);
	if (!currentCommitment) {
		console.warn('[PUBLISH] Cannot publish recognition: no commitment exists');
		return;
	}
	
	// Update commitment with new recognition weights
	const updatedCommitment: Commitment = {
		...currentCommitment,
		global_recognition_weights: normalizedWeights,
		timestamp: Date.now()
	};
	
	// Publish updated commitment
	await publishMyCommitment(updatedCommitment);
	
	const recipientCount = Object.keys(normalizedWeights).length;
	console.log(`[PUBLISH] Published recognition weights for ${recipientCount} people (in commitment)`);
}

/**
 * Update Commitment with Damping History
 * 
 * In plain English: "Save my over-allocation history for next time"
 * Persistence enables adaptive damping across sessions
 */
export async function updateCommitmentWithDampingHistory(
	totalReceivedByType: Record<string, number>
): Promise<void> {
	const myCommit = get(myCommitmentStore);
	if (!myCommit) return;
	
	const damping = myCommit.multi_dimensional_damping || {
		damping_factors: {},
		damping_history: {},
		global_damping_factor: 1.0
	};
	
	// Calculate stated needs by type
	const statedNeedByType: Record<string, number> = {};
	if (myCommit.need_slots) {
		for (const slot of myCommit.need_slots) {
			const typeId = slot.need_type_id;
			statedNeedByType[typeId] = (statedNeedByType[typeId] || 0) + slot.quantity;
		}
	}
	
	// Update damping history for each type
	for (const [typeId, totalReceived] of Object.entries(totalReceivedByType)) {
		const statedNeed = statedNeedByType[typeId] || 0;
		const overAllocation = Math.max(0, totalReceived - statedNeed);
		
		// Add to history
		const history = damping.damping_history[typeId] || [];
		history.push({
			need_type_id: typeId,
			overAllocation,
			timestamp: Date.now()
		});
		
		// Keep last 3 entries
		damping.damping_history[typeId] = history.slice(-3);
		
		// Recompute damping factor
		// Oscillating? 0.5, Smooth? 1.0, Otherwise? 0.8
		const recentHistory = damping.damping_history[typeId];
		let dampingFactor = 0.8; // Default: moderate
		
		if (recentHistory.length >= 3) {
			const values = recentHistory.map(h => h.overAllocation);
			const isOscillating = (values[0] < values[1] && values[1] > values[2]) ||
			                      (values[0] > values[1] && values[1] < values[2]);
			const isSmooth = values[0] >= values[1] && values[1] >= values[2];
			
			if (isOscillating) dampingFactor = 0.5;
			else if (isSmooth) dampingFactor = 1.0;
		}
		
		damping.damping_factors[typeId] = dampingFactor;
		
		console.log(`[DAMPING] Type ${typeId}: factor=${dampingFactor.toFixed(2)}, over=${overAllocation.toFixed(2)}`);
	}
	
	// Compute global damping as average
	const factors = Object.values(damping.damping_factors);
	damping.global_damping_factor = factors.length > 0
		? factors.reduce((a, b) => a + b, 0) / factors.length
		: 1.0;
	
	// Update commitment
	const updatedCommitment: Commitment = {
		...myCommit,
		multi_dimensional_damping: damping
	};
	
	await publishMyCommitment(updatedCommitment);
}

// ═══════════════════════════════════════════════════════════════════
// EXPORTS FOR DEBUGGING
// ═══════════════════════════════════════════════════════════════════

if (typeof window !== 'undefined') {
	(window as any).freeAlgorithm = {
		// Stores
		myPublicKey,
		myRecognitionOfOthers,
		myMutualRecognition,
		myCurrentNeeds,
		myActiveNeeds,
		myAvailableCapacity,
		myAllocationsAsProvider,
		myNeedsAtNextStep,
		universalSatisfactionAchieved,
		totalNeedMagnitude,
		dampingFactors,
		
		// ITC Functions
		getMyITCStamp,
		incrementMyITCStamp,
		mergeITCStampFromPeer,
		isPeerUpdateStale,
		getCausallyConsistentCommitments,
		
		// System State
		getCurrentSystemState,
		updateSystemStateFromNetwork,
		
	// Convergence Metrics
	computeTotalNeedMagnitude,
	computeContractionRate,
	computePercentNeedsMet,
	checkUniversalSatisfaction,
	estimateIterationsToConvergence,
	computeConvergenceSummary,
	computeMaxPersonNeed,
	computeNeedVariance,
	computePeopleStuck,
		
		// Spatial/Temporal Optimization
		networkNeedsIndex,
		getCandidateRecipients,
		
		// Publishing
		publishMyCommitment,
		publishMyRecognitionWeights,
		updateCommitmentWithDampingHistory
	};
	
	console.log('[FREE-ALGORITHM] Debug interface available at window.freeAlgorithm');
}

