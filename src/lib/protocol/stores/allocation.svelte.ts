/**
 * Free-Association Algorithm - Reactive Wrapper (Svelte Stores)
 * 
 * This is a THIN REACTIVE WRAPPER around the pure functions in allocation.ts
 * 
 * Architecture:
 * - allocation.ts: Pure functions (single source of truth for logic)
 * - allocation.svelte.ts: Reactive stores that call pure functions
 * 
 * Benefits:
 * - No code duplication
 * - Single source of truth for algorithm logic
 * - Pure functions are testable
 * - Reactive layer is simple and maintainable
 */

import { derived, writable, get } from 'svelte/store';
import type { Readable, Writable } from 'svelte/store';

// ═══════════════════════════════════════════════════════════════════
// IMPORT PURE ALGORITHM FUNCTIONS (Single Source of Truth!)
// ═══════════════════════════════════════════════════════════════════

import { holsterUserPub } from '$lib/network/holster.svelte'

import {
	// System State
	createInitialState,
	buildSystemState,
	type SystemStateSnapshot,
	
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
	type ConvergenceSummary,
	
	// Damping
	computeDampingFactors,
	updateOverAllocationHistory,
	
	// Mutual Recognition
	computeMutualRecognition,
	
	// Allocation
	computeAllocations,
	type AllocationResult,
	
	// Need Update
	applyNeedUpdateLaw,
	
	// Divisibility Constraints (Pure Functions - Single Source of Truth)
	applyDivisibilityConstraints,
	meetsMinimumAllocation,
	redistributeRemainders
} from '@playnet/free-association/allocation';

// Import v5 schemas and stores
import type {
	Commitment,
	NeedSlot,
	AvailabilitySlot,
	GlobalRecognitionWeights,
	SlotAllocationRecord,
	MultiDimensionalDamping,
	PerTypeDampingHistoryEntry
} from '@playnet/free-association/schemas';

import { normalizeGlobalRecognitionWeights } from '@playnet/free-association/schemas';

import {
	myCommitmentStore,
	networkCommitments,
	getAllCommitmentsRecord,
	getNetworkRecognitionWeightsRecord,
	networkNeedsIndex,
	networkRecognitionWeights,
	myRecognitionWeights,
	myMutualRecognition as myMutualRecognitionFromStores,
	networkAllocations,
	type SpaceTimeIndex
} from './stores.svelte';
import {slotsCompatible, passesSlotFilters, type FilterContext, getTimeBucketKey, getLocationBucketKey } from '@playnet/free-association/utils/match';
import { deepEqual } from '@playnet/free-association/utils/memoize';

// Import../../commons/v5/matchnctions for causal consistency
import {
    type Stamp as ITCStamp,
	seed as itcSeed,
	event as itcEvent,
	join as itcJoin,
	leq as itcLeq,
	equals as itcEquals,
	toString as itcToString
} from '$lib/utils/primitives/itc';

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
	const myPub = get(myPublicKey);
	
	console.log('[CAUSALLY-CONSISTENT] Processing', Object.keys(allCommitments).length, 'commitments, my pub:', myPub ? myPub.slice(0,20)+'...' : 'none');
	
	for (const [pubKey, commitment] of Object.entries(allCommitments)) {
		// Always include our own commitment (self-trust)
		// This prevents the "commitment from future" issue where stored commitment
		// has itcStamp > myITCStamp after fresh initialization
		if (pubKey === myPub) {
			console.log('[CAUSALLY-CONSISTENT] ✅ Including MY commitment (self-trust bypass)');
			snapshot[pubKey] = commitment;
			continue;
		}
		
		// For others, enforce ITC causality
		if (!commitment.itcStamp || itcLeq(commitment.itcStamp, myITCStamp)) {
			console.log('[CAUSALLY-CONSISTENT] ✅ Including', pubKey.slice(0,20)+'...', '(causally consistent)');
			snapshot[pubKey] = commitment;
		} else {
			console.log('[CAUSALLY-CONSISTENT] ⏭️  Skipping', pubKey.slice(0,20)+'...', '(from future)');
		}
	}
	
	console.log('[CAUSALLY-CONSISTENT] Returning', Object.keys(snapshot).length, 'commitments');
	
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
	[networkRecognitionWeights],
	([$networkWeights]) => {
		// Convert Map<string, GlobalRecognitionWeights> to Record<string, GlobalRecognitionWeights>
		const record: Record<string, GlobalRecognitionWeights> = {};
		for (const [pubKey, weights] of $networkWeights.entries()) {
			record[pubKey] = weights;
		}
		return record;
	}
);

// myMutualRecognition is imported from stores.svelte (already computed there with our fix!)
// Re-export for API compatibility
export const myMutualRecognition = myMutualRecognitionFromStores;

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
 * Over-Allocation History (per type) - SCHEMA-ALIGNED
 * 
 * Uses PerTypeDampingHistoryEntry format for full schema compliance.
 * Tracks over-allocation with timestamps for oscillation detection.
 * 
 * Format: Record<typeId, Array<{need_type_id, overAllocation, timestamp}>>
 */
export const overAllocationHistory: Writable<Record<string, PerTypeDampingHistoryEntry[]>> = writable({});

/**
 * Damping Factor (per type)
 * 1.0 = full speed (smooth convergence)
 * 0.8 = medium speed (default)
 * 0.5 = slow down (oscillation detected)
 * 
 * ✅ Uses pure function from allocation.ts
 */
export const dampingFactors: Readable<Record<string, number>> = derived(
	[overAllocationHistory],
	([$history]) => {
		// ✅ Call pure function (single source of truth!)
		return computeDampingFactors($history);
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
 * Current System State
 */
let currentSystemState: SystemStateSnapshot = createInitialState();

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
 * Get Previous System State
 */
export function getPreviousSystemState(): SystemStateSnapshot | null {
	return previousSystemState;
}

/**
 * Update System State from Network
 * Rebuild the state snapshot from current commitments
 * 
 * ✅ Uses pure function from allocation.ts
 */
export function updateSystemStateFromNetwork(): void {
	const commitments = getCausallyConsistentCommitments();
	
	// Store previous state for convergence tracking
	previousSystemState = { ...currentSystemState };
	
	// ✅ Call pure function (single source of truth!)
	currentSystemState = buildSystemState(commitments, currentSystemState);
	
	const peopleCount = Object.keys(currentSystemState.needsByPersonAndType).length;
	const typeCount = new Set(
		Object.values(currentSystemState.needsByPersonAndType).flatMap(needs => Object.keys(needs))
	).size;
	
	console.log(`[STATE] Updated: ${peopleCount} people, ${typeCount} need types, iteration ${currentSystemState.iteration}`);
}

// ═══════════════════════════════════════════════════════════════════
// RE-EXPORT CONVERGENCE METRICS (from pure functions)
// ═══════════════════════════════════════════════════════════════════

// Re-export for API compatibility
export {
	computeTotalNeedMagnitude,
	computeContractionRate,
	computePercentNeedsMet,
	checkUniversalSatisfaction,
	estimateIterationsToConvergence,
	computeConvergenceSummary,
	computeMaxPersonNeed,
	computeNeedVariance,
	computePeopleStuck,
	applyNeedUpdateLaw,
};

// ═══════════════════════════════════════════════════════════════════
// NOTE: Divisibility constraint functions are imported from allocation.ts
// See imports at top of file - following Single Source of Truth architecture
// ═══════════════════════════════════════════════════════════════════

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
// PART IV: ALLOCATION COMPUTATION (REACTIVE WRAPPER)
// ═══════════════════════════════════════════════════════════════════

/**
 * Memoization cache for myAllocationsAsProvider
 * Stores last inputs and result to avoid recomputing when inputs haven't changed
 */
let lastAllocationInputs: {
	myPub: string | null;
	myMR: Record<string, number>;
	myRec: GlobalRecognitionWeights;
	myCommitment: Commitment | null;
	allCommitments: Record<string, Commitment>;
} | null = null;

let lastAllocationResult: {
	allocations: SlotAllocationRecord[];
	totalsByTypeAndRecipient: Record<string, Record<string, number>>;
	convergence: ConvergenceSummary | null;
	slotDenominators: Record<string, { mutual: number; nonMutual: number; need_type_id: string }>;
} | null = null;

/**
 * Compute allocations when I'm the provider
 * 
 * ✅ REACTIVE WRAPPER around computeAllocations from allocation.ts
 * 
 * This wraps the pure allocation algorithm with Svelte reactivity:
 * - Monitors my mutual recognition, recognition, and commitment
 * - Calls pure allocation function when inputs change
 * - Includes spatial/temporal filtering via indexes
 * - ✅ MEMOIZED: Uses deep equality checks to avoid recomputing when inputs haven't changed
 * 
 * The actual allocation logic is in allocation.ts (single source of truth!)
 */
export const myAllocationsAsProvider: Readable<{
	allocations: SlotAllocationRecord[];
	totalsByTypeAndRecipient: Record<string, Record<string, number>>;
	convergence: ConvergenceSummary | null;
	slotDenominators: Record<string, { mutual: number; nonMutual: number; need_type_id: string }>;
}> = derived<
	[typeof myPublicKey, typeof myMutualRecognition, typeof myRecognitionOfOthers, typeof myCommitmentStore, typeof networkCommitments],
	{ allocations: SlotAllocationRecord[]; totalsByTypeAndRecipient: Record<string, Record<string, number>>; convergence: ConvergenceSummary | null; slotDenominators: Record<string, { mutual: number; nonMutual: number; need_type_id: string }> }
>(
	[
		myPublicKey,
		myMutualRecognition,
		myRecognitionOfOthers,
		myCommitmentStore,
		networkCommitments
	],
	([
		$myPub,
		$myMR,
		$myRec,
		$myCommitment,
		$networkCommitments
	]) => {
		if (!$myPub || !$myCommitment?.capacity_slots) {
			return { allocations: [], totalsByTypeAndRecipient: {}, convergence: null, slotDenominators: {} };
		}
		
		// Get ALL commitments (including our own for potential self-allocation)
		const allCommitments = getAllCommitmentsRecord();
		
		// Log network commitments count for debugging
		const networkCommitmentCount = Object.keys(allCommitments).filter(k => k !== $myPub).length;
		console.log(`[ALLOCATION-PROVIDER] Computing allocations with ${networkCommitmentCount} network commitments`);
		
		// ✅ MEMOIZATION: Check if inputs actually changed using deep equality
		// NOTE: We exclude itcStamp, timestamp, and _updatedAt from commitment comparison
		// because they're metadata, not allocation inputs. This prevents infinite loops
		// where metadata changes trigger re-computation even when actual data is unchanged.
		const { itcStamp: _, timestamp: __, ..._commitmentWithoutMetadata } = $myCommitment as any;
		const commitmentWithoutMetadata = _commitmentWithoutMetadata;
		
		// Also strip metadata from allCommitments for comparison
		const allCommitmentsWithoutMetadata: Record<string, any> = {};
		for (const [pubKey, commitment] of Object.entries(allCommitments)) {
			const { itcStamp, timestamp, _updatedAt, ...rest } = commitment as any;
			allCommitmentsWithoutMetadata[pubKey] = rest;
		}
		
		const currentInputs = {
			myPub: $myPub,
			myMR: $myMR,
			myRec: $myRec,
			myCommitment: commitmentWithoutMetadata,
			allCommitments: allCommitmentsWithoutMetadata
		};
		
		if (lastAllocationInputs && lastAllocationResult) {
			// Check if inputs are deeply equal
			const pubEqual = lastAllocationInputs.myPub === currentInputs.myPub;
			const mrEqual = deepEqual(lastAllocationInputs.myMR, currentInputs.myMR);
			const recEqual = deepEqual(lastAllocationInputs.myRec, currentInputs.myRec);
			const commitmentEqual = deepEqual(lastAllocationInputs.myCommitment, currentInputs.myCommitment);
			const allCommitmentsEqual = deepEqual(lastAllocationInputs.allCommitments, currentInputs.allCommitments);
			
			if (pubEqual && mrEqual && recEqual && commitmentEqual && allCommitmentsEqual) {
				console.log('[MEMOIZATION] ✅ Reusing allocation result (inputs unchanged)');
				return lastAllocationResult;
			} else {
				console.log('[MEMOIZATION] ❌ Inputs changed:', {
					pubEqual,
					mrEqual,
					recEqual,
					commitmentEqual,
					allCommitmentsEqual
				});
			}
		}
		
		// Track iteration start time for convergence metrics
		const iterationStartTime = Date.now();
		
		// OPTIMIZATION: Use spatial/temporal index for O(k) candidate lookup
		const needsIndexValue = get(networkNeedsIndex);
		
		// Update system state from network (before computing allocations)
		updateSystemStateFromNetwork();
		
		// ✅ CALL PURE FUNCTION (Single Source of Truth!)
		// This replaces ~480 lines of duplicated allocation logic with a single function call
		const allocationResult = computeAllocations(
			$myPub,
			$myCommitment.capacity_slots,
			$myRec,
			$myMR,
			allCommitments,
			currentSystemState,
			previousSystemState,
			needsIndexValue
		);
		
		const { allocations, totalsByTypeAndRecipient, slotDenominators } = allocationResult;
		
		// NOTE: The following ~480 lines of manual allocation logic have been removed
		// and replaced with the pure function call above. The pure function in allocation.ts
		// now implements proportional multi-pass redistribution for both Tier 1 and Tier 2.
		
		/*
		// ════════════════════════════════════════════════════════════════════════
		// OLD CODE (REMOVED - ~480 lines deleted):
		// ════════════════════════════════════════════════════════════════════════
		// Previously, allocation logic was duplicated here with inline implementation.
		// This violated the "single source of truth" architecture stated in file header.
		// 
		// The old code:
		// - Looped through each capacity slot
		// - Built compatibility matrices
		// - Implemented Tier 1 & Tier 2 allocation with sequential capping
		// - Used tier1RemainingCapacity guards to prevent overallocation
		// - Did NOT implement multi-pass redistribution
		//
		// Now replaced with pure function call above that:
		// - Implements proportional multi-pass redistribution
		// - Guarantees no FIFO bias (all allocations calculated simultaneously)
		// - Automatically redistributes when recipients are capped at needs
		// - Eliminates need for compensating capacity guards
		// - Maintains single source of truth in allocation.ts
		// ════════════════════════════════════════════════════════════════════════
		*/
		
		// Convergence computation
		
		// ✅ Call pure function for convergence metrics (single source of truth!)
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
		
		// ✅ MEMOIZATION: Store result for next time
		const result = { allocations, totalsByTypeAndRecipient, convergence, slotDenominators };
		lastAllocationInputs = currentInputs;
		lastAllocationResult = result;
		
		return result;
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
 * 
 * ✅ Uses pure function from allocation.ts
 */
export const myNeedsAtNextStep: Readable<Record<string, number>> = derived(
	[myCurrentNeeds, totalReceivedByType],
	([$currentNeeds, $received]) => {
		// ✅ Call pure function (single source of truth!)
		return applyNeedUpdateLaw($currentNeeds, $received);
	}
);

/**
 * Update my commitment with new needs (apply the update law)
 */
export function applyNeedUpdateLawToCommitment() {
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
 * 
 * ✅ Uses pure function from allocation.ts
 */
export function recordAllocationReceived(typeId: string, amount: number, providerPub?: string) {
	const currentNeeds = get(myCurrentNeeds);
	const currentNeed = currentNeeds[typeId] || 0;
	
	// Over-allocation is how much excess we received
	const overAllocation = Math.max(0, amount - currentNeed);
	
	// Update history using pure function
	overAllocationHistory.update(history => {
		// ✅ Call pure function (single source of truth!)
		return updateOverAllocationHistory(
			history,
			{ [typeId]: amount },
			currentNeeds
		);
	});
	
	// Update total received
	totalReceivedByType.update(totals => ({
		...totals,
		[typeId]: (totals[typeId] || 0) + amount
	}));
	
	// Log tracking (optional provider info)
	if (providerPub) {
		console.log(
			`[ALLOCATION-RECEIVED] ${amount.toFixed(2)} ${typeId} from ${providerPub.slice(0, 20)}...`
		);
	}
}

/**
 * Update commitment with computed dampening state (SCHEMA-ALIGNED)
 * 
 * Computes damping factors from history and updates the commitment's
 * multi_dimensional_damping field per MultiDimensionalDampingSchema.
 * 
 * This makes the dampening state transparent and portable across the network.
 */
export function updateCommitmentDampeningState() {
	const history = get(overAllocationHistory);
	
	// Compute damping factors from history
	const dampingFactors = computeDampingFactors(history);
	
	// Compute global damping factor (average of all types)
	const factors = Object.values(dampingFactors);
	const globalDampingFactor = factors.length > 0
		? factors.reduce((sum, f) => sum + f, 0) / factors.length
		: 1.0;
	
	// ✅ Build MultiDimensionalDamping object per schema
	const dampingState: MultiDimensionalDamping = {
		damping_factors: dampingFactors,
		damping_history: history,
		global_damping_factor: globalDampingFactor
	};
	
	// Update commitment (preserve timestamp)
	myCommitmentStore.update(c => {
		if (!c) return c;
		return {
			...c,
			multi_dimensional_damping: dampingState,
			timestamp: c.timestamp || Date.now() // Preserve existing timestamp
		};
	});
	
	console.log(
		`[DAMPENING-STATE] Updated commitment: ` +
		`${Object.keys(dampingFactors).length} types, ` +
		`global=${globalDampingFactor.toFixed(2)}`
	);
}

/**
 * Enable automatic remaining need tracking (README.md line 312)
 * 
 * ✅ PHASE 2: RECIPIENT-SIDE AUTO-UPDATE
 * 
 * Subscribes to network allocations and automatically:
 * 1. Tracks allocations received (recordAllocationReceived)
 * 2. Computes remaining need (myNeedsAtNextStep)
 * 3. Updates and publishes commitment (applyNeedUpdateLawToCommitment)
 * 
 * This enables the coordination mechanism described in README.md:
 * - Recipients automatically reduce their published need
 * - Providers see updated (remaining) needs, not stale declared needs
 * - Over-allocation is temporary and self-correcting
 * - System converges through parallel, independent updates
 * 
 * Call this once during app initialization.
 * 
 * @returns Unsubscribe function
 */
export function enableAutoRemainingNeedTracking(): () => void {
	console.log('[AUTO-NEED-TRACKING] 🚀 Enabling automatic remaining need tracking');
	
	let debounceTimer: ReturnType<typeof setTimeout> | null = null;
	let isProcessing = false;
	
	/**
	 * Debounced apply function
	 * Batches multiple allocations received in short time window
	 */
	const debouncedApply = () => {
		if (debounceTimer) {
			clearTimeout(debounceTimer);
		}
		
		debounceTimer = setTimeout(() => {
			if (isProcessing) {
				console.log('[AUTO-NEED-TRACKING] ⏭️  Skipped: already processing');
				return;
			}
			
			isProcessing = true;
			
			try {
				// Get current remaining needs
				const nextNeeds = get(myNeedsAtNextStep);
				const needCount = Object.keys(nextNeeds).length;
				const hasRemainingNeed = Object.values(nextNeeds).some(n => n > 0);
				
				console.log(
					`[AUTO-NEED-TRACKING] 📊 Remaining needs: ${needCount} types, ` +
					`has remaining: ${hasRemainingNeed}`
				);
				
				// ✅ Update dampening state in commitment (schema-aligned)
				updateCommitmentDampeningState();
				
				// Apply the update law to commitment
				applyNeedUpdateLawToCommitment();
				console.log('[AUTO-NEED-TRACKING] ✅ Applied need update law and published dampening state');
			} catch (error) {
				console.error('[AUTO-NEED-TRACKING] ❌ Error applying update law:', error);
			} finally {
				isProcessing = false;
			}
		}, 500); // 500ms debounce
	};
	
	// Subscribe to network allocations field store (fine-grained reactivity!)
	const unsubscribe = networkAllocations.subscribe(($allocationsMap) => {
		const myPub = get(holsterUserPub);
		if (!myPub) {
			// Not logged in yet
			return;
		}
		
		let receivedCount = 0;
		let totalReceived = 0;
		
		// Check each provider's allocations
		for (const [providerPubKey, allocations] of $allocationsMap.entries()) {
			if (!allocations || !Array.isArray(allocations)) continue;
			
			// Filter for allocations to me
			for (const allocation of allocations) {
				if (allocation.recipient_pubkey === myPub) {
					// Track this allocation
					recordAllocationReceived(
						allocation.need_type_id,
						allocation.quantity,
						providerPubKey
					);
					
					receivedCount++;
					totalReceived += allocation.quantity;
				}
			}
		}
		
		// If we received any allocations, trigger debounced update
		if (receivedCount > 0) {
			console.log(
				`[AUTO-NEED-TRACKING] 📥 Processing ${receivedCount} allocations ` +
				`(total: ${totalReceived.toFixed(2)})...`
			);
			debouncedApply();
		}
	});
	
	console.log('[AUTO-NEED-TRACKING] ✅ Enabled automatic need tracking');
	
	return () => {
		unsubscribe();
		if (debounceTimer) {
			clearTimeout(debounceTimer);
		}
		console.log('[AUTO-NEED-TRACKING] ⏸️  Disabled automatic need tracking');
	};
}

// ═══════════════════════════════════════════════════════════════════
// PART VI: CONVERGENCE DETECTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Universal Satisfaction Achieved?
 * True when all needs are met (all needs at or below epsilon)
 * 
 * ✅ Uses pure function from allocation.ts
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
	
	// Get fresh recognition weights
	const freshRecWeights = get(myRecognitionOfOthers);
	
	// FALLBACK STRATEGY: Trust previous values until network proves otherwise
	// If network is slow/incomplete, preserve previous recognition data
	const recWeights = Object.keys(freshRecWeights).length > 0
		? freshRecWeights
		: (commitment.global_recognition_weights || {});
	
	// Log fallback usage for debugging
	if (Object.keys(freshRecWeights).length === 0 && commitment.global_recognition_weights && Object.keys(commitment.global_recognition_weights).length > 0) {
		console.log(`[PUBLISH] Network slow - preserving ${Object.keys(commitment.global_recognition_weights).length} previous recognition weights`);
	}
	
	// Normalize recognition weights before publishing
	const normalizedWeights = normalizeGlobalRecognitionWeights(recWeights);
	
	// Enrich commitment with stamps and recognition
	const enrichedCommitment: Commitment = {
		...commitment,
		global_recognition_weights: normalizedWeights,
		// Preserve cache (updated by network subscriber in stores.svelte.ts)
		others_recognition_of_me: commitment.others_recognition_of_me,
		itcStamp: getMyITCStamp(),
		timestamp: Date.now()
	};
	
	// Publish to network
	await myCommitmentStore.set(enrichedCommitment);
	
	const cacheCount = Object.keys(commitment.others_recognition_of_me || {}).length;
	console.log(`[PUBLISH] Published commitment with ITC stamp ${itcToString(getMyITCStamp())}, Rec count: ${Object.keys(normalizedWeights).length}, Cache: ${cacheCount}`);
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
		
		// ✅ Use pure function to compute damping factor (single source of truth!)
		// Pass structured history directly (schema-aligned)
		const factors = computeDampingFactors({ [typeId]: damping.damping_history[typeId] });
		damping.damping_factors[typeId] = factors[typeId];
		
		console.log(`[DAMPING] Type ${typeId}: factor=${damping.damping_factors[typeId].toFixed(2)}, over=${overAllocation.toFixed(2)}`);
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
// AUTO-PUBLISH ALLOCATIONS TO NETWORK
// ═══════════════════════════════════════════════════════════════════

/**
 * Auto-update commitment with computed allocations
 * 
 * Watches myAllocationsAsProvider and publishes slot_allocations to commitment
 * This enables recipients to see incoming allocations for transparency
 * 
 * WHY THIS MATTERS:
 * - Recipients can see who's allocating to their needs
 * - Audit trail for allocation flows
 * - Debugging and trust building
 * - Complete transparency in the network
 */
export function enableAutoAllocationPublishing(): () => void {
	console.log('[AUTO-PUBLISH-ALLOC] 🚀 Enabling automatic allocation publishing');
	
	let debounceTimer: ReturnType<typeof setTimeout> | null = null;
	let isPublishing = false; // Prevent cascading updates
	let lastPublishedHash: string | null = null; // Track what we last published to prevent re-publishing same data
	
	const unsubAllocations = myAllocationsAsProvider.subscribe((allocResult) => {
		// Debounce rapid changes
		if (debounceTimer) {
			clearTimeout(debounceTimer);
		}
		
		debounceTimer = setTimeout(() => {
			// Check INSIDE the callback to prevent multiple queued callbacks from running
			if (isPublishing) {
				console.log('[AUTO-PUBLISH-ALLOC] ⏭️  Skipped: already publishing');
				return;
			}
			
			isPublishing = true;
			
			const currentCommitment = get(myCommitmentStore);
			if (!currentCommitment) {
				console.log('[AUTO-PUBLISH-ALLOC] ⏭️  Skipped: no commitment available');
				isPublishing = false;
				return;
			}
			
			// Check if allocations actually changed using hash
			const newAllocs = allocResult.allocations;
			const newAllocsHash = JSON.stringify(newAllocs);
			
			// Fast check: Same as what we last published?
			if (lastPublishedHash === newAllocsHash) {
				console.log('[AUTO-PUBLISH-ALLOC] ⏭️  Skipped: already published this exact allocation set');
				isPublishing = false;
				return;
			}
			
			// Slower check: Same as what's in the commitment?
			const currentAllocs = currentCommitment.slot_allocations || [];
			try {
				const currentJson = JSON.stringify(currentAllocs);
				
				if (currentJson === newAllocsHash) {
					console.log('[AUTO-PUBLISH-ALLOC] ⏭️  Skipped: allocations unchanged in commitment');
					// Update our hash to match current state
					lastPublishedHash = newAllocsHash;
					isPublishing = false;
					return;
				}
			} catch (error) {
				console.warn('[AUTO-PUBLISH-ALLOC] ⚠️  Equality check failed, proceeding with update:', error);
			}
			
			// Update commitment with new allocations
			const updatedCommitment: Commitment = {
				...currentCommitment,
				slot_allocations: newAllocs,
				timestamp: Date.now()
			};
			
			myCommitmentStore.set(updatedCommitment);
			
			// Update hash to prevent re-publishing
			lastPublishedHash = newAllocsHash;
			
			const mutualCount = newAllocs.filter(a => a.tier === 'mutual').length;
			const nonMutualCount = newAllocs.filter(a => a.tier === 'non-mutual').length;
			console.log(`[AUTO-PUBLISH-ALLOC] ✅ Published ${newAllocs.length} allocations to network (${mutualCount} mutual, ${nonMutualCount} non-mutual)`);
			
			isPublishing = false;
		}, 100); // 100ms debounce
	});
	
	return () => {
		if (debounceTimer) clearTimeout(debounceTimer);
		unsubAllocations();
		console.log('[AUTO-PUBLISH-ALLOC] ⏸️  Disabled automatic allocation publishing');
	};
}

// ═══════════════════════════════════════════════════════════════════
// EXPORTS FOR DEBUGGING
// ═══════════════════════════════════════════════════════════════════

// Initialize debug exports after a delay to ensure all stores are initialized
// This prevents "Cannot access uninitialized variable" errors on iOS Safari
if (typeof window !== 'undefined') {
	setTimeout(() => {
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
			getPreviousSystemState,
			updateSystemStateFromNetwork,
			
			// Convergence Metrics (from pure functions)
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
	}, 0);
}
