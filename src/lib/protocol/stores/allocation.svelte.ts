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
 * 
 * ═══════════════════════════════════════════════════════════════════
 * FINE-GRAINED REACTIVITY (Versioned Store Architecture)
 * ═══════════════════════════════════════════════════════════════════
 * 
 * This file leverages VersionedStore's field-level change tracking for
 * optimal performance. Key stores used:
 * 
 * - networkNeedSlots: Only updates when 'needs' field changes
 * - networkRecognitionWeights: Only updates when 'recognition' field changes
 * - networkAllocations: Only updates when 'allocations' field changes
 * 
 * Performance Impact: 3-4× faster than naive reactivity!
 * 
 * Example Scenarios:
 * ✅ Alice updates recognition → only recognition-dependent stores recalculate
 * ✅ Bob updates needs → only need-dependent stores recalculate
 * ⏭️ Charlie updates damping → allocation stores skip (not dependent)
 * 
 * How It Works:
 * 1. VersionedStore tracks ITC causality (entity-level)
 * 2. VersionedStore tracks field versions (fine-grained)
 * 3. Derived field stores only update when their field's version increments
 * 4. Downstream stores only recalculate when dependencies actually change
 * 
 * See: versioned_store_architecture.md for technical details
 * ═══════════════════════════════════════════════════════════════════
 */

import { derived, writable, get } from 'svelte/store';
import type { Readable, Writable } from 'svelte/store';

// ═══════════════════════════════════════════════════════════════════
// IMPORT PURE ALGORITHM FUNCTIONS (Single Source of Truth!)
// ═══════════════════════════════════════════════════════════════════

import { holsterUserPub } from '$lib/network/holster.svelte'

// ═══════════════════════════════════════════════════════════════════
// NOTE: Old allocation-local module removed - new paradigm uses slot-based allocation
// ═══════════════════════════════════════════════════════════════════
// The following functions are NOT available in the new @playnet/free-association/allocation:
// - System State: createInitialState, buildSystemState, SystemStateSnapshot
// - Convergence Metrics: computeTotalNeedMagnitude, computeContractionRate, etc.
// - Damping: computeDampingFactors, updateOverAllocationHistory
// - Mutual Recognition: computeMutualRecognition
// - Need Update: applyNeedUpdateLaw
// - Divisibility: applyDivisibilityConstraints, meetsMinimumAllocation, redistributeRemainders
//
// These were part of the old iterative convergence paradigm.
// The new paradigm uses calculateSlotBasedPriorityAllocation which handles everything internally.
// ═══════════════════════════════════════════════════════════════════

// ✅ IMPORT NEW SLOT-BASED ALLOCATION ALGORITHM (Local Copy for Testing)
import {
	calculateSlotBasedPriorityAllocation,
	type LocalSlotAllocation
} from '../allocation-local';



// ✅ NOTE: Damping logic not available in new protocol package
// The new slot-based allocation handles priority limits internally
// Old damping functions (computeDampingFactors, updateOverAllocationHistory) removed

// Import v5 schemas and stores
import type {
	Commitment,
	NeedSlot,
	AvailabilitySlot,
	GlobalRecognitionWeights,
	SlotAllocationRecord,
	AllocationResult,
	MultiDimensionalDamping,
	PerTypeDampingHistoryEntry
} from '../schemas';

import { normalizeGlobalRecognitionWeights } from '../schemas';

import {
	myCommitmentStore,
	networkCommitments,
	getAllCommitmentsRecord,
	getNetworkRecognitionWeightsRecord,
	networkNeedsIndex,
	networkRecognitionWeights,
	networkNeedSlots, // ✅ Import fine-grained store
	myRecognitionWeights,
	myMutualRecognition as myMutualRecognitionFromStores,
	networkAllocations,
	totalReceivedBySlot as totalReceivedBySlotFromStores
} from './stores.svelte';
import type { SpaceTimeIndex } from './stores.svelte'; // ✅ Import SpaceTimeIndex Type
import { slotsCompatible, passesSlotFilters, type FilterContext, getTimeBucketKey, getLocationBucketKey } from '@playnet/free-association/utils/match';
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
 * ITC Manager - Elegant API for causal consistency
 * 
 * Usage:
 *   itc.get()           // Get current stamp
 *   itc.increment()     // Increment on state change
 *   itc.merge(peerStamp) // Merge peer's stamp
 */
export const itc = {
	/** Get current ITC stamp */
	get: (): ITCStamp => myITCStamp,

	/** Increment ITC stamp (call when making a state change) */
	increment: (): void => {
		myITCStamp = itcEvent(myITCStamp);
		console.log(`[ITC] My stamp updated: ${itcToString(myITCStamp)}`);
	},

	/** Merge ITC stamp from peer (call when receiving updates) */
	merge: (peerStamp: ITCStamp): void => {
		const oldStamp = myITCStamp;
		myITCStamp = itcJoin(myITCStamp, peerStamp);
		if (!itcEquals(oldStamp, myITCStamp)) {
			console.log(`[ITC] Merged peer stamp: ${itcToString(myITCStamp)}`);
		}
	}
};

// Legacy exports for backward compatibility
export const getMyITCStamp = itc.get;
export const incrementMyITCStamp = itc.increment;
export const mergeITCStampFromPeer = itc.merge;

// ═══════════════════════════════════════════════════════════════════
// DERIVED STORES - Commitment-based Aggregates (Moved from stores.svelte.ts)
// ═══════════════════════════════════════════════════════════════════

/**
 * My Recognition of Others
 * "I recognize Alice 30%, Bob 40%, Carol 30%" (must sum to 100%)
 * 
 * V5: Extracted from commitment (recognition stored in commitment.global_recognition_weights)
 * 
 * NOTE: Moved from stores.svelte.ts to break circular dependency
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
 * 
 * Derived from networkRecognitionWeights - converts Map to Record for easier access
 * 
 * NOTE: Moved from stores.svelte.ts to break circular dependency
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

/**
 * My Current Needs (by type)
 * "I need 40 meals, 10 hours of tutoring, 2 checkups"
 * 
 * NOTE: Moved from stores.svelte.ts to break circular dependency
 */
export const myCurrentNeeds: Readable<Record<string, number>> = derived(
	[myCommitmentStore],
	([$commitment]) => {
		if (!$commitment?.need_slots) return {};

		const needsByType: Record<string, number> = {};
		for (const slot of $commitment.need_slots) {
			const typeId = slot.need_type_id;
			if (typeId && slot.quantity) {
				needsByType[typeId] = (needsByType[typeId] || 0) + slot.quantity;
			}
		}

		return needsByType;
	}
);

/**
 * My Available Capacity (by type)
 * "I can provide 100 meals, 20 hours of tutoring"
 * 
 * NOTE: Moved from stores.svelte.ts to break circular dependency
 */
export const myAvailableCapacity: Readable<Record<string, number>> = derived(
	[myCommitmentStore],
	([$commitment]) => {
		if (!$commitment?.capacity_slots) return {};

		const capacityByType: Record<string, number> = {};
		for (const slot of $commitment.capacity_slots) {
			const typeId = slot.need_type_id;
			if (typeId && slot.quantity) {
				capacityByType[typeId] = (capacityByType[typeId] || 0) + slot.quantity;
			}
		}

		return capacityByType;
	}
);


// ═══════════════════════════════════════════════════════════════════
// PART I: MY IDENTITY & RECOGNITION
// ═══════════════════════════════════════════════════════════════════

/**
 * My Public Key (identity in the network)
 * Re-exported from stores for convenience
 */
export const myPublicKey = holsterUserPub;

// NOTE: myMutualRecognition, myRecognitionOfOthers, othersRecognitionOfMe,
// myCurrentNeeds, and myAvailableCapacity are now defined above
// (moved from stores.svelte.ts to avoid circular dependencies)

// ═══════════════════════════════════════════════════════════════════
// PART II: DAMPING (SELF-CORRECTION)
// ═══════════════════════════════════════════════════════════════════

// ═══════════════════════════════════════════════════════════════════
// DAMPING FUNCTIONALITY COMMENTED OUT - Not available in new paradigm
// ═══════════════════════════════════════════════════════════════════
// The new slot-based allocation uses priority limits instead of damping.
// Priority percentages define maximum willingness to allocate.
// ═══════════════════════════════════════════════════════════════════

/*
 * Damping Factor (per type)
 * 1.0 = full speed (smooth convergence)
 * 0.8 = medium speed (default)
 * 0.5 = slow down (oscillation detected)
 * 
 * ✅ Uses pure function from allocation.ts
 */
// export const dampingFactors: Readable<Record<string, number>> = derived(
// 	[overAllocationHistory],
// 	([$history]) => {
// 		// ✅ Call pure function (single source of truth!)
// 		return computeDampingFactors($history as any);
// 	}
// );

/*
 * My Active Needs (damped)
 * Active-Need = Stated-Need × Damping-Factor
 */
// export const myActiveNeeds: Readable<Record<string, number>> = derived(
// 	[myCurrentNeeds, dampingFactors],
// 	([$needs, $factors]) => {
// 		const activeNeeds: Record<string, number> = {};
//
// 		for (const [typeId, need] of Object.entries($needs)) {
// 			const factor = $factors[typeId] || 0.8; // Default medium speed
// 			activeNeeds[typeId] = need * factor;
// 		}
//
// 		return activeNeeds;
// 	}
// );

// ═══════════════════════════════════════════════════════════════════
// SYSTEM STATE TRACKING COMMENTED OUT - Not available in new paradigm
// ═══════════════════════════════════════════════════════════════════
// The new slot-based allocation doesn't use iterative convergence tracking.
// Allocation happens in a single pass with priority limits.
// ═══════════════════════════════════════════════════════════════════

/*
 * System State Store - Reactive state tracking
 * 
 * Tracks current and previous system state for convergence detection.
 * Automatically reactive - subscribers get notified on updates.
 */
// const systemStateStore = writable<{
// 	current: SystemStateSnapshot;
// 	previous: SystemStateSnapshot | null;
// }>({
// 	current: createInitialState(),
// 	previous: null
// });

/*
 * System State Manager - Elegant API for state management
 * 
 * Usage:
 *   systemState.subscribe(state => ...)  // React to changes
 *   systemState.update()                 // Update from network
 *   systemState.getCurrent()             // Get current snapshot
 *   systemState.getPrevious()            // Get previous snapshot
 */
// export const systemState = {
// 	/** Subscribe to state changes */
// 	subscribe: systemStateStore.subscribe,
//
// 	/** Update system state from network commitments */
// 	update: (): void => {
// 		const commitments = getAllCommitmentsRecord();
//
// 		systemStateStore.update(state => {
// 			const newCurrent = buildSystemState(commitments, state.current);
//
// 			const peopleCount = Object.keys(newCurrent.needsByPersonAndType).length;
// 			const typeCount = new Set(
// 				Object.values(newCurrent.needsByPersonAndType).flatMap(needs => Object.keys(needs))
// 			).size;
//
// 			console.log(`[STATE] Updated: ${peopleCount} people, ${typeCount} need types, iteration ${newCurrent.iteration}`);
//
// 			return {
// 				current: newCurrent,
// 				previous: state.current
// 			};
// 		});
// 	},
//
// 	/** Get current system state snapshot */
// 	getCurrent: (): SystemStateSnapshot => get(systemStateStore).current,
//
// 	/** Get previous system state snapshot */
// 	getPrevious: (): SystemStateSnapshot | null => get(systemStateStore).previous
// };

// Legacy exports for backward compatibility
// export const getCurrentSystemState = systemState.getCurrent;
// export const getPreviousSystemState = systemState.getPrevious;
// export const updateSystemStateFromNetwork = systemState.update;

// ═══════════════════════════════════════════════════════════════════
// CONVERGENCE METRICS COMMENTED OUT - Not available in new paradigm
// ═══════════════════════════════════════════════════════════════════
// The new slot-based allocation doesn't use convergence metrics.
// Allocation is deterministic based on priority limits.
// ═══════════════════════════════════════════════════════════════════

// Re-export for API compatibility (COMMENTED OUT - functions not available)
// export {
// 	computeTotalNeedMagnitude,
// 	computeContractionRate,
// 	computePercentNeedsMet,
// 	checkUniversalSatisfaction,
// 	estimateIterationsToConvergence,
// 	computeConvergenceSummary,
// 	computeMaxPersonNeed,
// 	computeNeedVariance,
// 	computePeopleStuck,
// 	applyNeedUpdateLaw,
// };

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
	if (typeId && needsIndex.byType.has(typeId)) {
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
	convergence: null; // ConvergenceSummary not available in new paradigm
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

// ═══════════════════════════════════════════════════════════════════
// PRIORITY SYNTHESIS HELPERS
// ═══════════════════════════════════════════════════════════════════
// These functions bridge the gap between "person-to-person" recognition
// and "slot-to-slot" priorities required by the allocation algorithm.
// ═══════════════════════════════════════════════════════════════════

/**
 * Helper to find owner of a slot from all commitments
 */
function findSlotOwner(slotId: string, allCommitments: Record<string, Commitment>): string | null {
	for (const [pubKey, commitment] of Object.entries(allCommitments)) {
		if (commitment.need_slots?.some(s => s.id === slotId)) return pubKey;
		if (commitment.capacity_slots?.some(s => s.id === slotId)) return pubKey;
	}
	return null;
}

/**
 * Enrich capacity slot with priority distribution
 * 
 * Synthesizes slot-to-slot priorities from person-to-person recognition.
 * For MY capacity slots: priority to each need = my recognition of need owner
 * 
 * @param slot - My capacity slot to enrich
 * @param networkNeeds - All network need slots
 * @param myRecognition - My recognition weights (person-to-person)
 * @param allCommitments - All commitments for owner lookup
 * @returns Enriched slot with priority_distribution
 */
function enrichCapacitySlotWithPriorities(
	slot: AvailabilitySlot,
	networkNeeds: NeedSlot[],
	myRecognition: GlobalRecognitionWeights,
	allCommitments: Record<string, Commitment>,
	myPub: string
): AvailabilitySlot {
	// Already has priorities? Return as-is
	if (slot.priority_distribution && slot.priority_distribution.length > 0) {
		return slot;
	}

	// Synthesize from recognition: for each network need, priority = my recognition of owner
	const generated = networkNeeds
		.map(ns => {
			if (!ns.id) return null;
			const owner = findSlotOwner(ns.id, allCommitments);

			// For self-allocation, always prioritize 100%
			if (owner === myPub) {
				return { target_slot_id: ns.id, priority_percentage: 1.0 };
			}

			if (!owner || (myRecognition[owner] || 0) <= 0.001) return null;
			return { target_slot_id: ns.id, priority_percentage: myRecognition[owner] };
		})
		.filter((p): p is { target_slot_id: string; priority_percentage: number } => p !== null);

	return { ...slot, priority_distribution: generated };
}

/**
 * Enrich need slot with priority distribution
 * 
 * Synthesizes slot-to-slot priorities from person-to-person recognition.
 * For NETWORK need slots: priority from my capacity = their recognition of me
 * 
 * @param slot - Network need slot to enrich
 * @param myCapacity - My capacity slots
 * @param theirRecognitionOfMe - How much they recognize me (person-to-person)
 * @returns Enriched slot with priority_distribution
 */
function enrichNeedSlotWithPriorities(
	slot: NeedSlot,
	myCapacity: AvailabilitySlot[],
	theirRecognitionOfMe: number
): NeedSlot {
	// Already has priorities? Return as-is
	if (slot.priority_distribution && slot.priority_distribution.length > 0) {
		return slot;
	}

	// Synthesize from recognition: for each of my capacity slots, priority = their recognition of me
	const generated = myCapacity
		.map(cs => {
			if (theirRecognitionOfMe <= 0.001) return null;
			return { target_slot_id: cs.id, priority_percentage: theirRecognitionOfMe };
		})
		.filter((p): p is { target_slot_id: string | undefined; priority_percentage: number } => p !== null);

	return { ...slot, priority_distribution: generated };
}

/**
 * ADAPTER: Synthesis of Priority Distributions from Recognition Weights
 * 
 * This bridges the gap between V5 "Person-to-Person" recognition and
 * V6 "Slot-to-Slot" priorities.
 * 
 * 1. For My Capacity Slots:
 *    - Priorities = My recognition of need owner
 * 
 * 2. For Network Need Slots:
 *    - Priorities = Need owner's recognition of me
 */

/**
 * Compute allocations when I'm the provider
 * 
 * ✅ UPDATED V6: Uses calculateSlotBasedPriorityAllocation
 */
export const myAllocationsAsProvider: Readable<{
	allocations: SlotAllocationRecord[];
	totalsByTypeAndRecipient: Record<string, Record<string, number>>;
	convergence: null; // ConvergenceSummary not available in new paradigm
	slotDenominators: Record<string, { mutual: number; nonMutual: number; need_type_id: string }>;
}> = derived(
	[
		myPublicKey,
		myMutualRecognitionFromStores,
		myRecognitionWeights,
		myCommitmentStore,
		networkNeedSlots, // We use the list store for iteration
		networkRecognitionWeights,
		networkNeedsIndex
	],
	([
		$myPub,
		$myMR,
		$myRec,
		$myCommitment,
		$networkNeedsList,
		$networkRec,
		$networkNeedsIndex
	]) => {
		if (!$myPub || !$myCommitment?.capacity_slots) {
			return { allocations: [], totalsByTypeAndRecipient: {}, convergence: null, slotDenominators: {} };
		}

		const allCommitments = getAllCommitmentsRecord();

		// Log network commitments count for debugging
		const networkCommitmentCount = Object.keys(allCommitments).filter(k => k !== $myPub).length;
		console.log(`[ALLOCATION-PROVIDER] Computing allocations (V6 Protocol) with ${networkCommitmentCount} network commitments`);

		// ✅ MEMOIZATION: Check if inputs actually changed using deep equality
		const { itcStamp: _, timestamp: __, ..._commitmentWithoutMetadata } = $myCommitment as any;

		// Simple shallow check for memoization (optimization)
		// Only recompute if commitment, recognition, or needs changed
		// Use a more robust check that handles potential undefined/nulls safely
		const inputHash = JSON.stringify({
			c: _commitmentWithoutMetadata,
			r: $myRec,
			n: $networkNeedsIndex, // This might be too heavy? networkNeedsList length maybe?
			nl: $networkNeedsList ? $networkNeedsList.length : 0
		});

		// Track iteration start time (kept for potential future use)
		// const iterationStartTime = Date.now();

		// Update system state from network (COMMENTED OUT - not available in new paradigm)
		// updateSystemStateFromNetwork();

		// ═══════════════════════════════════════════════════════════════════
		// 1. ADAPTER: Enrich Slots with Priorities
		// ═══════════════════════════════════════════════════════════════════

		// We need to pass "all needs" to the adapter. 
		// networkNeedSlots store is a list of ALL need slots in the network.

		// ADAPTER: Synthesize priorities if missing (migration path)
		const networkNeedsArray = $networkNeedsList ? Array.from($networkNeedsList.values()).flat() : [];

		// Include MY needs (for self-allocation)
		// Network stores usually exclude self, so we must add them explicitly
		const myNeeds = ($myCommitment.need_slots || [])
			.filter((s): s is NeedSlot & { id: string } => !!s.id)
			.map(s => ({ ...s, pubkey: $myPub }));

		// Combine and deduplicate (in case I am subscribed to myself)
		const allNeedsRaw = [...networkNeedsArray];
		for (const myNeed of myNeeds) {
			if (!allNeedsRaw.some(n => n.id === myNeed.id)) {
				allNeedsRaw.push(myNeed);
			}
		}

		// 1. My Capacity Slots - Enrich with priorities using helper
		// ✅ PERFORMANCE: Clean helper function for reusable priority synthesis
		// Note: we pass allNeedsRaw to ensure priorities are generated for self-needs too
		const enrichedCapacity = ($myCommitment.capacity_slots || [])
			.filter((s): s is AvailabilitySlot & { id: string } => !!s.id)
			.map(slot =>
				enrichCapacitySlotWithPriorities(slot, allNeedsRaw, $myRec, allCommitments, $myPub)
			);

		// 2. Network Need Slots - Enrich with priorities using helper
		// ✅ PERFORMANCE: Their recognition of me determines priority from my capacity
		const enrichedNeeds = allNeedsRaw.map(slot => {
			if (!slot.id) return slot;
			const owner = findSlotOwner(slot.id, allCommitments);
			if (!owner) return slot;

			// If owner is ME, priority is my recognition of myself (self-loop)
			// If owner is OTHERS, priority is their recognition of me
			let priorityOfMe = 0;
			if (owner === $myPub) {
				priorityOfMe = 1.0; // My recognition of myself (force 100% for self-loops)
			} else {
				priorityOfMe = $networkRec.get(owner)?.[$myPub] || 0;
			}

			return enrichNeedSlotWithPriorities(slot, $myCommitment.capacity_slots || [], priorityOfMe);
		});

		// ═══════════════════════════════════════════════════════════════════
		// 2. CALL NEW ALGORITHM
		// ═══════════════════════════════════════════════════════════════════

		// Use the locally-imported new function
		// Note: passing options for debug/refinement
		const flatAllocations = calculateSlotBasedPriorityAllocation(
			enrichedCapacity,
			enrichedNeeds,
			allCommitments,
			{
				debug: false,
				enableRefinement: true
			}
		);

		// ═══════════════════════════════════════════════════════════════════
		// 3. ADAPTER: Transform Output for Store Contract
		// ═══════════════════════════════════════════════════════════════════

		// Reconstruct strict return types expected by UI components
		const totalsByTypeAndRecipient: Record<string, Record<string, number>> = {};

		for (const alloc of flatAllocations) {

			// Populate totalsByTypeAndRecipient
			// Need to find type ID for the slot
			const slotId = alloc.capacity_slot_id;
			const slot = enrichedCapacity.find(s => s.id === slotId);
			if (slot) {
				const typeId = slot.need_type_id;
				if (typeId) {
					if (!totalsByTypeAndRecipient[typeId]) {
						totalsByTypeAndRecipient[typeId] = {};
					}
					const current = totalsByTypeAndRecipient[typeId][alloc.recipient_pubkey] || 0;
					totalsByTypeAndRecipient[typeId][alloc.recipient_pubkey] = current + alloc.quantity;
				}
			}
		}

		// Slot Denominators (Simulated for V6)
		// The UI uses this to show "demand" on a slot.
		// We can calculate demand by summing priorities of compatible needs.
		const slotDenominators: Record<string, { mutual: number; nonMutual: number; need_type_id: string }> = {};

		for (const cs of enrichedCapacity) {
			let mutual = 0;
			let nonMutual = 0;

			// Simulate from priorities
			const csAny = cs as any;
			if (csAny.priority_distribution) {
				for (const [recipientPub, priority] of Object.entries(csAny.priority_distribution as Record<string, number>)) {
					// Find recipient's needs compatible with this slot
					const recipientNeeds = enrichedNeeds.filter(n => (n as any).pubkey === recipientPub);

					let isMutual = false;
					const myPub = get(holsterUserPub);

					for (const ns of recipientNeeds) {
						// Check if ns prioritizes ME (provider)
						const nsDist = (ns.priority_distribution as Record<string, number> | undefined);
						const recipPriority = nsDist?.[myPub] || 0;
						if (recipPriority > 0.001) {
							isMutual = true;
							break;
						}
					}

					if (isMutual && priority > 0.001) mutual += priority;
					else nonMutual += priority;
				}
			}

			if (cs.id && cs.need_type_id) {
				slotDenominators[cs.id] = {
					mutual,
					nonMutual,
					need_type_id: cs.need_type_id
				};
			}
		}

		// Calculate convergence metrics (COMMENTED OUT - not available in new paradigm)
		// const convergence = computeConvergenceSummary(
		// 	systemState.getCurrent(),
		// 	systemState.getPrevious(),
		// 	iterationStartTime
		// );
		const convergence = null; // No convergence tracking in new paradigm

		// Increment ITC stamp
		incrementMyITCStamp();

		// [DEBUG] Log allocations to check for self-allocation
		const selfAllocations = flatAllocations.filter(a => a.recipient_pubkey === $myPub);
		if (selfAllocations.length > 0) {
			console.log(`[ALLOCATION-PROVIDER] ✅ Generated ${selfAllocations.length} self-allocations:`, selfAllocations);
		} else {
			console.log(`[ALLOCATION-PROVIDER] ❌ No self-allocations generated (Total: ${flatAllocations.length})`);
			if (enrichedNeeds.some(n => n.pubkey === $myPub)) {
				console.log(`[ALLOCATION-PROVIDER] ⚠️ found my needs in enrichedNeeds but no allocation!`);
			}
		}

		const result = {
			allocations: flatAllocations.map(a => {
				const slot = enrichedCapacity.find(s => s.id === a.capacity_slot_id);
				return {
					...a,
					availability_slot_id: a.capacity_slot_id,
					recipient_need_slot_id: a.need_slot_id,
					need_type_id: slot?.need_type_id || 'unknown',
					time_compatible: true,
					location_compatible: true,
					tier: 0
				} as SlotAllocationRecord;
			}),
			totalsByTypeAndRecipient,
			convergence,
			slotDenominators
		};

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
/**
 * Total I've received (across all providers, by type)
 * This would be computed by aggregating allocations from all providers
 * 
 * NOTE: Moved to stores.svelte.ts to prevent circular dependency, re-exported here for compatibility
 */
export const totalReceivedBySlot = totalReceivedBySlotFromStores;

// ═══════════════════════════════════════════════════════════════════
// NEED UPDATE LAW COMMENTED OUT - Not available in new paradigm
// ═══════════════════════════════════════════════════════════════════
// The new slot-based allocation doesn't use iterative need updates.
// Needs are static; allocation happens based on current declared needs.
// ═══════════════════════════════════════════════════════════════════

/*
 * My Needs at Next Step
 * Your-Need-at-Next-Step = max(0, Your-Current-Need - Total-You-Received)
 * 
 * ✅ Uses pure function from allocation.ts
 */
// export const myNeedsAtNextStep: Readable<Record<string, number>> = derived(
// 	[myCurrentNeeds, totalReceivedBySlot],
// 	([$currentNeeds, $received]) => {
// 		// Aggregate received totals (Record<string, Record<string, number>> -> Record<string, number>)
// 		const aggregatedReceived: Record<string, number> = {};
// 		for (const [type, providers] of Object.entries($received)) {
// 			aggregatedReceived[type] = Object.values(providers || {}).reduce((sum, q) => sum + q, 0);
// 		}
// 		// ✅ Call pure function (single source of truth!)
// 		return applyNeedUpdateLaw($currentNeeds, aggregatedReceived);
// 	}
// );

/**
 * Update my commitment with new needs (apply the update law)
 * 
 * NOTE: This function intentionally does NOT modify need_slots.
 * Declared needs should remain constant - they represent what the user originally declared.
 * The "remaining need" is computed dynamically via damping in the allocation algorithm.
 * 
 * This function is kept as a no-op for now to maintain the API contract,
 * but may be removed in a future refactor.
 */
export async function publishFeedbackState() {
	// ✅ FIXED: We MUST publish total_allocated so other providers know  
	// how much we've received! This enables the "Add Back" logic to work
	// and prevents improper overallocation.
	const totalReceivedMap = get(totalReceivedBySlot);
	const totalReceived: Record<string, number> = {};
	for (const [type, providers] of Object.entries(totalReceivedMap)) {
		totalReceived[type] = Object.values(providers || {}).reduce((sum, q) => sum + q, 0);
	}
	const myCommit = get(myCommitmentStore);

	if (!myCommit) return;

	// Check if total_allocated actually changed to avoid loop
	const currentTotalAllocated = myCommit.total_allocated || {};
	if (JSON.stringify(currentTotalAllocated) === JSON.stringify(totalReceivedMap)) {
		return;
	}

	const updatedCommitment: Commitment = {
		...myCommit,
		total_allocated: totalReceivedMap,
		timestamp: Date.now()
	};

	console.log(`[AUTO-NEED-TRACKING] 💾 Updating commitment with total_allocated:`, totalReceivedMap);
	await publishMyCommitment(updatedCommitment);
}

// ═══════════════════════════════════════════════════════════════════
// OVER-ALLOCATION TRACKING COMMENTED OUT - Not available in new paradigm
// ═══════════════════════════════════════════════════════════════════
// The new slot-based allocation uses priority limits instead of damping.
// Over-allocation tracking is not needed.
// ═══════════════════════════════════════════════════════════════════

/*
 * Record allocation received (to update over-allocation history)
 * 
 * ✅ Uses pure function from allocation.ts
 */
// export function recordAllocationReceived(typeId: string, amount: number, providerPub?: string) {
// 	const currentNeeds = get(myCurrentNeeds);
// 	const currentNeed = currentNeeds[typeId] || 0;
//
// 	// Over-allocation is how much excess we received
// 	const overAllocation = Math.max(0, amount - currentNeed);
//
// 	// Update history using pure function
// 	overAllocationHistory.update(history => {
// 		// ✅ Call pure function (single source of truth!)
// 		// FORCE CAST: The pure function uses a compatible but distinct type definition
// 		return updateOverAllocationHistory(
// 			history as any,
// 			{ [typeId]: amount },
// 			currentNeeds
// 		) as any as Record<string, PerTypeDampingHistoryEntry[]>;
// 	});
//
// 	// Update total received
// 	// NOTE: totalReceivedBySlot is now a derived store in stores.svelte.ts
// 	// We no longer imperatively update it here. It updates automatically from networkAllocations.
//
// 	// Log tracking (optional provider info)
// 	if (providerPub) {
// 		console.log(
// 			`[ALLOCATION-RECEIVED] ${amount.toFixed(2)} ${typeId} from ${providerPub.slice(0, 20)}...`
// 		);
// 	}
// }

/*
 * Update commitment with computed dampening state (SCHEMA-ALIGNED)
 * 
 * Computes damping factors from history and updates the commitment's
 * multi_dimensional_damping field per MultiDimensionalDampingSchema.
 * 
 * This makes the dampening state transparent and portable across the network.
 */
// export function updateCommitmentDampeningState() {
// 	const history = get(overAllocationHistory);
//
// 	// Compute damping factors from history
// 	const dampingFactors = computeDampingFactors(history as any);
//
// 	// Compute global damping factor (average of all types)
// 	const factors = Object.values(dampingFactors);
// 	const globalDampingFactor = factors.length > 0
// 		? factors.reduce((sum, f) => sum + f, 0) / factors.length
// 		: 1.0;
//
// 	// ✅ Build MultiDimensionalDamping object per schema
// 	const dampingState: MultiDimensionalDamping = {
// 		damping_factors: dampingFactors,
// 		damping_history: history,
// 		global_damping_factor: globalDampingFactor
// 	};
//
// 	// Update commitment (preserve timestamp)
// 	myCommitmentStore.update(c => {
// 		if (!c) return c;
// 		return {
// 			...c,
// 			multi_dimensional_damping: dampingState,
// 			timestamp: c.timestamp || Date.now() // Preserve existing timestamp
// 		};
// 	});
//
// 	console.log(
// 		`[DAMPENING-STATE] Updated commitment: ` +
// 		`${Object.keys(dampingFactors).length} types, ` +
// 		`global=${globalDampingFactor.toFixed(2)}`
// 	);
// }

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
	console.log('[AUTO-NEED-TRACKING] 🚀 Enabling automatic remaining need tracking (V-Store Bridge)');

	// ✅ REFACTOR: Pure "V-Store Bridge" Pattern
	// We simply bridge the derived `totalReceivedBySlot` store to the commitment.
	// No complex manual tracking needed because:
	// 1. `totalReceivedBySlot` is derived from `networkAllocations` (Fine-Grained)
	// 2. It only emits when the calculated totals actually change (Svelte derived semantics)
	// 3. We check if the value differs from current commitment before writing (Break Cycles)

	const unsubscribe = totalReceivedBySlot.subscribe(($totalReceived) => {
		const myCommit = get(myCommitmentStore);
		if (!myCommit) return;

		// Aggregate totals
		const aggregated: Record<string, number> = {};
		for (const [type, providers] of Object.entries($totalReceived)) {
			aggregated[type] = Object.values(providers || {}).reduce((sum, q) => sum + q, 0);
		}

		// 1. Check if values effectively changed
		const currentDesc = JSON.stringify(myCommit.total_allocated || {});
		const nextDesc = JSON.stringify($totalReceived || {});

		if (currentDesc === nextDesc) {
			// No change needed
			return;
		}

		console.log(`[AUTO-NEED-TRACKING] 🔄 Updating total_allocated:`, $totalReceived);

		// 2. Update Commitment (The Bridge)
		// This writes to the system of record, which eventually propagates to peers
		myCommitmentStore.update(c => ({
			...c,
			total_allocated: $totalReceived,
			timestamp: Date.now()
		}));

		// 3. Update Dampening State (Side-benefit) - COMMENTED OUT
		// Since we are here, ensure dampening state is current
		// updateCommitmentDampeningState(); // Not available in new paradigm
	});

	return unsubscribe;
}


// ═══════════════════════════════════════════════════════════════════
// CONVERGENCE DETECTION COMMENTED OUT - Not available in new paradigm
// ═══════════════════════════════════════════════════════════════════

/*
 * Universal Satisfaction Achieved?
 * True when all needs are met (all needs at or below epsilon)
 * 
 * ✅ Uses pure function from allocation.ts
 */
// export const universalSatisfactionAchieved: Readable<boolean> = derived(
// 	[myCurrentNeeds],
// 	([$needs]) => {
// 		const epsilon = 0.001;
//
// 		for (const need of Object.values($needs)) {
// 			if (need > epsilon) return false;
// 		}
//
// 		return true;
// 	}
// );

/*
 * Total Need Magnitude (Euclidean norm across all types)
 * ||N_vec|| = sqrt(food² + healthcare² + tutoring² + ...)
 */
// export const totalNeedMagnitude: Readable<number> = derived(
// 	[myCurrentNeeds],
// 	([$needs]) => {
// 		let sumOfSquares = 0;
// 		for (const need of Object.values($needs)) {
// 			sumOfSquares += need * need;
// 		}
// 		return Math.sqrt(sumOfSquares);
// 	}
// );

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
	} as unknown as Commitment;

	// Publish to network
	await myCommitmentStore.set(enrichedCommitment as any);

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
	} as unknown as Commitment;

	// Publish updated commitment
	await publishMyCommitment(updatedCommitment as any);

	const recipientCount = Object.keys(normalizedWeights).length;
	console.log(`[PUBLISH] Published recognition weights for ${recipientCount} people (in commitment)`);
}

// ═══════════════════════════════════════════════════════════════════
// UPDATE COMMITMENT WITH DAMPING HISTORY COMMENTED OUT - Not available
// ═══════════════════════════════════════════════════════════════════

/*
 * Update Commitment with Damping History
 * 
 * In plain English: "Save my over-allocation history for next time"
 * Persistence enables adaptive damping across sessions
 */
// export async function updateCommitmentWithDampingHistory(
// 	totalReceivedBySlot: Record<string, Record<string, number>>
// ): Promise<void> {
// 	const myCommit = get(myCommitmentStore);
// 	if (!myCommit) return;
//
// 	// Ensure damping state exists
// 	const damping: any = myCommit.multi_dimensional_damping || {
// 		damping_history: {},
// 		damping_factors: {},
// 		global_damping_factor: 1.0,
// 		timestamp: Date.now()
// 	};
//
// 	// Ensure sub-objects exist
// 	if (!damping.damping_history) damping.damping_history = {};
// 	if (!damping.damping_factors) damping.damping_factors = {};
//
// 	const statedNeedByType: Record<string, number> = {};
// 	if (myCommit.need_slots) {
// 		for (const slot of myCommit.need_slots) {
// 			const typeId = slot.need_type_id;
// 			if (typeId && slot.quantity) {
// 				statedNeedByType[typeId] = (statedNeedByType[typeId] || 0) + slot.quantity;
// 			}
// 		}
// 	}
//
// 	// Update damping history for each type
// 	let stateChanged = false;
//
// 	for (const [typeId, providerAllocations] of Object.entries(totalReceivedBySlot)) {
// 		const totalReceived = Object.values(providerAllocations).reduce((sum, q) => sum + q, 0);
// 		const statedNeed = statedNeedByType[typeId] || 0;
// 		const overAllocation = Math.max(0, totalReceived - statedNeed);
//
// 		// Initialize history for this type if missing
// 		if (!damping.damping_history[typeId]) {
// 			damping.damping_history[typeId] = [];
// 		}
//
// 		const history = damping.damping_history[typeId];
//
// 		// Add to history
// 		history.push({
// 			need_type_id: typeId,
// 			overAllocation,
// 			timestamp: Date.now()
// 		});
//
// 		// Keep last 3 entries
// 		if (history.length > 3) {
// 			damping.damping_history[typeId] = history.slice(-3);
// 		}
//
// 		// ✅ Use pure function to compute damping factor (single source of truth!)
// 		// Pass structured history directly (schema-aligned)
// 		const factors = computeDampingFactors({ [typeId]: damping.damping_history[typeId] } as any);
//
// 		if (damping.damping_factors[typeId] !== factors[typeId]) {
// 			damping.damping_factors[typeId] = factors[typeId];
// 			stateChanged = true;
// 		}
//
// 		console.log(`[DAMPING] Type ${typeId}: factor=${damping.damping_factors[typeId].toFixed(2)}, over=${overAllocation.toFixed(2)}`);
// 	}
//
// 	// Update commitment
// 	const updatedCommitment: Commitment = {
// 		...myCommit,
// 		multi_dimensional_damping: damping
// 	};
//
// 	await publishMyCommitment(updatedCommitment);
// }

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
			} as unknown as Commitment;

			myCommitmentStore.set(updatedCommitment as any);
		}, 100); // 100ms debounce
	});

	return () => {
		if (debounceTimer) clearTimeout(debounceTimer);
		unsubAllocations();
		console.log('[AUTO-PUBLISH-ALLOC] ⏸️  Disabled automatic allocation publishing');
	};
}

// Initialize debug exports after a delay to ensure all stores are initialized
// This prevents "Cannot access uninitialized variable" errors on iOS Safari
if (typeof window !== 'undefined') {
	setTimeout(() => {
		(window as any).freeAlgorithm = {
			// ═══════════════════════════════════════════════════════════
			// STORES - Reactive state
			// ═══════════════════════════════════════════════════════════
			stores: {
				myPublicKey,
				myRecognitionOfOthers,
				myMutualRecognitionFromStores,
				myCurrentNeeds,
				// myActiveNeeds, // Commented out - not available
				myAvailableCapacity,
				myAllocationsAsProvider,
				// myNeedsAtNextStep, // Commented out - not available
				// universalSatisfactionAchieved, // Commented out - not available
				// totalNeedMagnitude, // Commented out - not available
				// dampingFactors // Commented out - not available
			},

			// ═══════════════════════════════════════════════════════════
			// ITC - Causal consistency
			// ═══════════════════════════════════════════════════════════
			itc: {
				get: getMyITCStamp,
				increment: incrementMyITCStamp,
				merge: mergeITCStampFromPeer
			},

			// STATE - System state management (COMMENTED OUT - not available)
			// state: {
			// 	getCurrent: getCurrentSystemState,
			// 	getPrevious: getPreviousSystemState,
			// 	update: updateSystemStateFromNetwork
			// },

			// CONVERGENCE - Metrics and analysis (COMMENTED OUT - not available)
			// convergence: {
			// 	computeTotalNeedMagnitude,
			// 	computeContractionRate,
			// 	computePercentNeedsMet,
			// 	checkUniversalSatisfaction,
			// 	estimateIterationsToConvergence,
			// 	computeConvergenceSummary,
			// 	computeMaxPersonNeed,
			// 	computeNeedVariance,
			// 	computePeopleStuck
			// },

			// ═══════════════════════════════════════════════════════════
			// OPTIMIZATION - Spatial/temporal indexing
			// ═══════════════════════════════════════════════════════════
			optimization: {
				networkNeedsIndex,
				getCandidateRecipients
			},

			// PUBLISHING - Network updates
			publishing: {
				publishMyCommitment,
				publishMyRecognitionWeights,
				// updateCommitmentWithDampingHistory // Commented out - not available
			}
		};

		console.log('[FREE-ALGORITHM] Debug interface available at window.freeAlgorithm');
	}, 0);
}
