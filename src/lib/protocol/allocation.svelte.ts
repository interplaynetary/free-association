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
	applyNeedUpdateLaw
} from '$lib/protocol/allocation';

// Import v5 schemas and stores
import type {
	Commitment,
	NeedSlot,
	AvailabilitySlot,
	GlobalRecognitionWeights,
	SlotAllocationRecord,
} from '$lib/protocol/schemas';

import { normalizeGlobalRecognitionWeights } from '$lib/protocol/schemas';

import {
	myCommitmentStore,
	networkCommitments,
	getAllCommitmentsRecord,
	getNetworkRecognitionWeightsRecord,
	networkNeedsIndex,
	networkRecognitionWeights,
	myRecognitionWeights,
	myMutualRecognition as myMutualRecognitionFromStores,
	type SpaceTimeIndex
} from '$lib/protocol/stores.svelte';
import {slotsCompatible, passesSlotFilters, type FilterContext, getTimeBucketKey, getLocationBucketKey } from '$lib/protocol/utils/match';

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
 * Over-Allocation History (per type)
 * Tracks how much excess we received in the last 3 allocations
 */
export const overAllocationHistory: Writable<Record<string, number[]>> = writable({});

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
};

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
 * Compute allocations when I'm the provider
 * 
 * ✅ REACTIVE WRAPPER around computeAllocations from allocation.ts
 * 
 * This wraps the pure allocation algorithm with Svelte reactivity:
 * - Monitors my mutual recognition, recognition, and commitment
 * - Calls pure allocation function when inputs change
 * - Includes spatial/temporal filtering via indexes
 * 
 * The actual allocation logic is in allocation.ts (single source of truth!)
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
		
		// Get ALL commitments (including our own for potential self-allocation)
		const allCommitments = getAllCommitmentsRecord();
		
		// OPTIMIZATION: Use spatial/temporal index for O(k) candidate lookup
		const needsIndexValue = get(networkNeedsIndex);
		
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
			
			const compatibleRecipients = new Map<string, NeedSlot[]>();
			
			// Get candidate recipients from index (O(k) instead of O(N))
			const candidatePubKeys = getCandidateRecipients(capacitySlot, needsIndexValue);
			
			console.log(`[ALLOCATION-LOOP] Using indexed lookup: ${candidatePubKeys.size} candidates (was O(N) scan)`);
			
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
					attributes: ('attributes' in $myCommitment && $myCommitment.attributes) ? $myCommitment.attributes : {}
				};
				
				const recipientContext: FilterContext = {
					pubKey: recipientPub,
					commitment: recipientCommitment,
					mutualRecognition: providerMR,
					attributes: ('attributes' in recipientCommitment && recipientCommitment.attributes) ? recipientCommitment.attributes : {}
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
			
			let capacityUsedInTier1 = 0;
			
			if (totalMutualRecognition > 0) {
				// Calculate shares and numerators
				for (const [recipientPub, needSlots] of compatibleRecipients.entries()) {
					const mutualRec = $myMR[recipientPub] || 0;
					if (mutualRec <= 0) continue;
					
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
						|| 1.0;
					
					const activeNeed = totalNeed * dampingFactor;
					
					eligibleRecipients.push({
						pubKey: recipientPub,
						need: totalNeed,
						mutualRecognitionShare,
						activeNeed,
						needSlots
					});
				}
				
				// Step 2: Calculate denominator
				let denominator = 0;
				for (const recipient of eligibleRecipients) {
					denominator += recipient.mutualRecognitionShare * recipient.activeNeed;
				}
				
				// Safety check for tiny denominators
				const MIN_RELATIVE_DENOMINATOR = 0.001;
				const minSafeDenominator = providersAvailableCapacity * MIN_RELATIVE_DENOMINATOR;
				
				if (denominator < minSafeDenominator) {
					console.warn(`[ALLOCATION-TIER1] Denominator ${denominator.toFixed(6)} too small, flooring to ${minSafeDenominator.toFixed(6)}`);
					denominator = minSafeDenominator;
				}
				
				// Step 3: Allocate to each recipient (TIER 1: MUTUAL)
				for (const recipient of eligibleRecipients) {
					const yourRawAllocation =
						providersAvailableCapacity *
						(recipient.mutualRecognitionShare * recipient.activeNeed) /
						denominator;
					
					const yourFinalAllocation = Math.min(yourRawAllocation, recipient.need);
					
					if (yourFinalAllocation > 0) {
						// PROPORTIONAL DISTRIBUTION across compatible need slots
						const totalCompatibleNeed = recipient.needSlots.reduce((sum, slot) => sum + slot.quantity, 0);
						let actuallyAllocated = 0;
						
						for (const needSlot of recipient.needSlots) {
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
				
				tier1Denominator = denominator;
			}
			
			// ────────────────────────────────────────────────────────────
			// TIER 2: NON-MUTUAL RECOGNITION (GENEROUS GIVING)
			// ────────────────────────────────────────────────────────────
			
			const remainingCapacity = providersAvailableCapacity - capacityUsedInTier1;
			let tier2Denominator = 0;
			
			if (remainingCapacity > 0.0001) {
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
					if (myRecOfThem <= 0) continue;
					
					totalNonMutualRecognition += myRecOfThem;
				}
				
				if (totalNonMutualRecognition > 0) {
					// Calculate shares and numerators
					for (const [recipientPub, needSlots] of compatibleRecipients.entries()) {
						const mutualRec = $myMR[recipientPub] || 0;
						if (mutualRec > 0) continue;
						
						const myRecOfThem = $myRec[recipientPub] || 0;
						if (myRecOfThem <= 0) continue;
						
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
							|| 1.0;
						
						const activeNeed = totalNeed * dampingFactor;
						
						nonMutualEligibleRecipients.push({
							pubKey: recipientPub,
							need: totalNeed,
							recognitionShare,
							activeNeed,
							needSlots
						});
					}
					
					// Step 2: Calculate denominator
					tier2Denominator = 0;
					for (const recipient of nonMutualEligibleRecipients) {
						tier2Denominator += recipient.recognitionShare * recipient.activeNeed;
					}
					
					// Safety check for tiny denominators
					const MIN_RELATIVE_DENOMINATOR = 0.001;
					const minSafeDenominatorT2 = remainingCapacity * MIN_RELATIVE_DENOMINATOR;
					
					if (tier2Denominator < minSafeDenominatorT2) {
						console.warn(`[ALLOCATION-TIER2] Denominator ${tier2Denominator.toFixed(6)} too small, flooring to ${minSafeDenominatorT2.toFixed(6)}`);
						tier2Denominator = minSafeDenominatorT2;
					}
					
					if (tier2Denominator > 0) {
						// Step 3: Allocate from remaining capacity
						for (const recipient of nonMutualEligibleRecipients) {
							const yourRawAllocation =
								remainingCapacity *
								(recipient.recognitionShare * recipient.activeNeed) /
								tier2Denominator;
							
							const yourFinalAllocation = Math.min(yourRawAllocation, recipient.need);
							
							if (yourFinalAllocation > 0) {
								// PROPORTIONAL DISTRIBUTION across compatible need slots (Tier 2)
								const totalCompatibleNeed = recipient.needSlots.reduce((sum, slot) => sum + slot.quantity, 0);
								let actuallyAllocated = 0;
								
								for (const needSlot of recipient.needSlots) {
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
			
			// Store denominators for this slot
			slotDenominators[capacitySlot.id] = {
				mutual: tier1Denominator,
				nonMutual: tier2Denominator,
				need_type_id: typeId
			};
		} // End of capacity slot loop
		
		// Update system state from network (before computing convergence)
		updateSystemStateFromNetwork();
		
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
export function recordAllocationReceived(typeId: string, amount: number) {
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
		
		// ✅ Use pure function to compute damping factor (single source of truth!)
		const historyValues = damping.damping_history[typeId].map(h => h.overAllocation);
		const factors = computeDampingFactors({ [typeId]: historyValues });
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
}
