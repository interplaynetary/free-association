/**
 * Free Association Protocol v5 - Pure Algorithm Core
 * 
 * This is the pure, environment-agnostic implementation of the allocation algorithm.
 * No Svelte stores, no browser APIs, no side effects - just pure functions.
 * 
 * Can be used by:
 * - Svelte frontend (via free-algorithm.svelte.ts wrapper)
 * - Terminal/CLI applications
 * - Server-side processing
 * - Tests
 * 
 * Architecture:
 * - Pure functions only (no global state)
 * - Explicit dependencies (passed as parameters)
 * - Immutable data (returns new objects, never mutates input)
 * - Type-safe (full TypeScript types from schemas.ts)
 * - Uses Zod schemas for validation
 */

import type {
	Commitment,
	NeedSlot,
	AvailabilitySlot,
	GlobalRecognitionWeights,
	SlotAllocationRecord,
	ITCStamp,
	SystemState,
	ConvergenceMetrics,
	ConvergenceSummary as ZodConvergenceSummary,
	AllocationResult,
	MultiDimensionalDamping
} from './schemas';

import {
	normalizeGlobalRecognitionWeights,
	SystemStateSchema,
	ConvergenceMetricsSchema,
	ConvergenceSummarySchema,
	AllocationResultSchema
} from './schemas';

import { slotsCompatible } from './match';

// Import ITC for causality tracking
import {
	type Stamp as ITCStampType,
	seed as itcSeed,
	event as itcEvent,
	join as itcJoin,
	leq as itcLeq,
	equals as itcEquals
} from '../utils/itc';

// ═══════════════════════════════════════════════════════════════════
// SIMPLIFIED TYPES (Bridge between full schemas and algorithm needs)
// ═══════════════════════════════════════════════════════════════════

/**
 * Simplified System State for Algorithm
 * 
 * Note: The full SystemState from schemas.ts is complex with per-type vectors.
 * For the pure algorithm, we use this simplified view focused on aggregates.
 * Can be converted to/from full SystemState as needed.
 */
export interface SystemStateSnapshot {
	/** Everyone's needs by type: { alice: { food: 40 }, bob: { tutoring: 10 } } */
	needsByPersonAndType: Record<string, Record<string, number>>;
	
	/** Everyone's capacity by type: { kitchen: { food: 100 }, teacher: { tutoring: 20 } } */
	capacityByPersonAndType: Record<string, Record<string, number>>;
	
	/** When this snapshot was taken */
	timestamp: number;
	
	/** Which iteration (how many times have we allocated?) */
	iteration: number;
	
	/** ITC stamp for causal consistency */
	itcStamp: ITCStampType;
}

/**
 * Simplified Convergence Summary for Algorithm Output
 * 
 * Note: This is now defined in schemas.ts as ConvergenceSummarySchema.
 * Re-exported here for convenience.
 */
export type ConvergenceSummary = ZodConvergenceSummary;

/**
 * Damping History per type
 * 
 * Note: Simpler than full MultiDimensionalDamping from schemas.ts
 * Focused on just what the pure algorithm needs.
 */
export interface DampingState {
	/** Over-allocation history per type: { food: [0.2, 0.15, 0.1] } */
	overAllocationHistory: Record<string, number[]>;
	
	/** Current damping factors per type: { food: 0.8, tutoring: 1.0 } */
	dampingFactors: Record<string, number>;
}

// Re-export AllocationResult from schemas for consistency
export type { AllocationResult };

// ═══════════════════════════════════════════════════════════════════
// SYSTEM STATE OPERATIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Create initial system state
 */
export function createInitialState(): SystemStateSnapshot {
	return {
		needsByPersonAndType: {},
		capacityByPersonAndType: {},
		timestamp: Date.now(),
		iteration: 0,
		itcStamp: itcSeed()
	};
}

/**
 * Build system state from network commitments
 * 
 * @param commitments - All commitments from the network
 * @param previousState - Previous state (for iteration tracking)
 * @returns New system state snapshot
 */
export function buildSystemState(
	commitments: Record<string, Commitment>,
	previousState?: SystemStateSnapshot
): SystemStateSnapshot {
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
	
	return {
		needsByPersonAndType,
		capacityByPersonAndType,
		timestamp: Date.now(),
		iteration: previousState ? previousState.iteration + 1 : 0,
		itcStamp: previousState?.itcStamp || itcSeed()
	};
}

// ═══════════════════════════════════════════════════════════════════
// CONVERGENCE METRICS (PURE FUNCTIONS)
// ═══════════════════════════════════════════════════════════════════

/**
 * Total Need Magnitude - How much total need is in the system?
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
 * Returns: fraction of needs remaining after one iteration
 * - 0.8 = needs shrunk by 20% (good!)
 * - 1.0 = needs stayed the same (no progress)
 * - >1.0 = needs grew (bad!)
 */
export function computeContractionRate(
	currentMagnitude: number,
	previousMagnitude: number
): number {
	if (previousMagnitude < 0.001) return 0;
	return currentMagnitude / previousMagnitude;
}

/**
 * Percent People Satisfied - What fraction of people are fully satisfied?
 * 
 * IMPORTANT: This is a binary per-person metric. A person is only counted
 * if ALL their needs are met (< 0.001). This will be 0% until convergence.
 * 
 * For allocation progress, use percentNeedReduction instead.
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
 * Percent Need Reduction - How much has the total need magnitude decreased?
 * 
 * This shows actual allocation progress: (previous - current) / previous * 100
 * Returns 0-100 showing what % of the previous need has been fulfilled.
 */
export function computePercentNeedReduction(
	currentMagnitude: number,
	previousMagnitude: number
): number {
	if (previousMagnitude < 0.001) return 100; // All needs met
	const reduction = previousMagnitude - currentMagnitude;
	return Math.max(0, Math.min(100, (reduction / previousMagnitude) * 100));
}

/**
 * Universal Satisfaction - Are ALL needs zero?
 */
export function checkUniversalSatisfaction(state: SystemStateSnapshot): boolean {
	for (const needsByType of Object.values(state.needsByPersonAndType)) {
		for (const need of Object.values(needsByType)) {
			if (need > 0.001) {
				return false;
			}
		}
	}
	return true;
}

/**
 * Estimate Iterations to Convergence
 * Returns: estimated iterations until needs hit zero, or null if not converging
 */
export function estimateIterationsToConvergence(
	currentMagnitude: number,
	contractionRate: number
): number | null {
	if (contractionRate >= 1) return null; // Not converging
	if (contractionRate <= 0) return 0; // Already there
	if (currentMagnitude < 0.001) return 0; // Already there
	
	const targetMagnitude = 0.001;
	const iterations = Math.log(targetMagnitude / currentMagnitude) / Math.log(contractionRate);
	
	return Math.max(0, Math.ceil(iterations));
}

/**
 * Compute maximum need across all participants
 */
export function computeMaxPersonNeed(state: SystemStateSnapshot): number {
	let maxNeed = 0;
	
	for (const needsByType of Object.values(state.needsByPersonAndType)) {
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
		let personNeedSquared = 0;
		for (const need of Object.values(needsByType)) {
			personNeedSquared += need ** 2;
		}
		personNeeds.push(Math.sqrt(personNeedSquared));
	}
	
	if (personNeeds.length === 0) return 0;
	
	const mean = personNeeds.reduce((sum, need) => sum + need, 0) / personNeeds.length;
	const variance = personNeeds.reduce((sum, need) => sum + (need - mean) ** 2, 0) / personNeeds.length;
	
	return variance;
}

/**
 * Count how many participants have unchanging needs (stuck)
 */
export function computePeopleStuck(
	currentState: SystemStateSnapshot,
	previousState: SystemStateSnapshot | null
): number {
	if (!previousState) return 0;
	
	let stuckCount = 0;
	const epsilon = 0.001;
	
	for (const [person, currentNeeds] of Object.entries(currentState.needsByPersonAndType)) {
		const previousNeeds = previousState.needsByPersonAndType[person];
		if (!previousNeeds) continue;
		
		let currentTotal = 0;
		let previousTotal = 0;
		
		for (const [type, need] of Object.entries(currentNeeds)) {
			currentTotal += need ** 2;
			previousTotal += (previousNeeds[type] || 0) ** 2;
		}
		
		currentTotal = Math.sqrt(currentTotal);
		previousTotal = Math.sqrt(previousTotal);
		
		if (Math.abs(currentTotal - previousTotal) < epsilon && currentTotal > epsilon) {
			stuckCount++;
		}
	}
	
	return stuckCount;
}

/**
 * Compute full convergence summary
 */
export function computeConvergenceSummary(
	currentState: SystemStateSnapshot,
	previousState: SystemStateSnapshot | null,
	iterationStartTime: number
): ConvergenceSummary {
	const currentMagnitude = computeTotalNeedMagnitude(currentState);
	const previousMagnitude = previousState 
		? computeTotalNeedMagnitude(previousState)
		: currentMagnitude * 2;
	
	const contractionRate = computeContractionRate(currentMagnitude, previousMagnitude);
	const isConverged = currentMagnitude < 0.001;
	const percentNeedsMet = computePercentNeedsMet(currentState);
	const percentNeedReduction = computePercentNeedReduction(currentMagnitude, previousMagnitude);
	const universalSatisfaction = checkUniversalSatisfaction(currentState);
	const iterationsToConvergence = estimateIterationsToConvergence(currentMagnitude, contractionRate);
	
	const maxPersonNeed = computeMaxPersonNeed(currentState);
	const needVariance = computeNeedVariance(currentState);
	const peopleStuck = computePeopleStuck(currentState, previousState);
	
	const now = Date.now();
	const responseLatency = now - iterationStartTime;
	
	return {
		totalNeedMagnitude: currentMagnitude,
		previousNeedMagnitude: previousMagnitude,
		contractionRate,
		isConverged,
		percentNeedsMet,
		percentNeedReduction,
		universalSatisfaction,
		iterationsToConvergence,
		currentIteration: currentState.iteration,
		responseLatency,
		maxPersonNeed,
		needVariance,
		peopleStuck
	};
}

// ═══════════════════════════════════════════════════════════════════
// DAMPING (SELF-CORRECTION)
// ═══════════════════════════════════════════════════════════════════

/**
 * Compute damping factors from over-allocation history
 * 
 * @param history - Over-allocation history per type
 * @returns Damping factors per type (0.5 = slow, 0.8 = medium, 1.0 = full speed)
 */
export function computeDampingFactors(
	history: Record<string, number[]>
): Record<string, number> {
	const factors: Record<string, number> = {};
	
	for (const [typeId, hist] of Object.entries(history)) {
		if (hist.length < 3) {
			factors[typeId] = 0.8; // Medium speed by default
			continue;
		}
		
		// Check last 3 entries for oscillation
		const recent = hist.slice(-3);
		const upDownUp = recent[0] < recent[1] && recent[1] > recent[2];
		const downUpDown = recent[0] > recent[1] && recent[1] < recent[2];
		
		if (upDownUp || downUpDown) {
			factors[typeId] = 0.5; // Slow down (oscillation)
		} else {
			const isSmooth = recent[0] >= recent[1] && recent[1] >= recent[2];
			factors[typeId] = isSmooth ? 1.0 : 0.8;
		}
	}
	
	return factors;
}

/**
 * Update over-allocation history with new received amounts
 * 
 * @param history - Current history
 * @param received - Amount received this iteration per type
 * @param needs - Current needs per type
 * @returns Updated history
 */
export function updateOverAllocationHistory(
	history: Record<string, number[]>,
	received: Record<string, number>,
	needs: Record<string, number>
): Record<string, number[]> {
	const newHistory: Record<string, number[]> = { ...history };
	
	for (const [typeId, receivedAmount] of Object.entries(received)) {
		const need = needs[typeId] || 0;
		const overAllocation = Math.max(0, receivedAmount - need);
		
		if (!newHistory[typeId]) {
			newHistory[typeId] = [];
		}
		
		newHistory[typeId] = [...newHistory[typeId], overAllocation].slice(-10); // Keep last 10
	}
	
	return newHistory;
}

// ═══════════════════════════════════════════════════════════════════
// MUTUAL RECOGNITION COMPUTATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Compute mutual recognition between me and others
 * MR(A,B) = min(A's recognition of B, B's recognition of A)
 * 
 * @param myRecognition - My recognition weights: { alice: 0.3, bob: 0.4 }
 * @param othersRecognition - Others' recognition of me: { alice: { me: 0.5 }, bob: { me: 0.6 } }
 * @param myPubKey - My public key
 * @returns Mutual recognition: { alice: 0.3, bob: 0.4 }
 */
export function computeMutualRecognition(
	myRecognition: GlobalRecognitionWeights,
	othersRecognition: Record<string, GlobalRecognitionWeights>,
	myPubKey: string
): Record<string, number> {
	const mutual: Record<string, number> = {};
	
	for (const [otherPubKey, myRecOfThem] of Object.entries(myRecognition)) {
		const theirRecOfMe = othersRecognition[otherPubKey]?.[myPubKey] || 0;
		mutual[otherPubKey] = Math.min(myRecOfThem, theirRecOfMe);
	}
	
	return mutual;
}

// ═══════════════════════════════════════════════════════════════════
// ALLOCATION COMPUTATION (TWO-TIER SYSTEM)
// ═══════════════════════════════════════════════════════════════════

/**
 * Find compatible recipients for a capacity slot
 * Returns map of pubKey -> compatible need slots
 */
function findCompatibleRecipients(
	capacitySlot: AvailabilitySlot,
	allCommitments: Record<string, Commitment>,
	myPubKey: string
): Map<string, NeedSlot[]> {
	const compatible = new Map<string, NeedSlot[]>();
	const typeId = capacitySlot.need_type_id;
	
	for (const [recipientPub, commitment] of Object.entries(allCommitments)) {
		if (recipientPub === myPubKey) continue; // Don't allocate to myself
		
		if (!commitment.need_slots) continue;
		
		const compatibleSlots: NeedSlot[] = [];
		for (const needSlot of commitment.need_slots) {
			if (needSlot.need_type_id !== typeId) continue;
			
			// Check slot compatibility (time, location, etc.)
			if (slotsCompatible(capacitySlot, needSlot)) {
				compatibleSlots.push(needSlot);
			}
		}
		
		if (compatibleSlots.length > 0) {
			compatible.set(recipientPub, compatibleSlots);
		}
	}
	
	return compatible;
}

/**
 * Compute allocations for my capacity slots
 * 
 * @param myPubKey - My public key
 * @param myCapacitySlots - My available capacity slots
 * @param myRecognition - My recognition of others
 * @param mutualRecognition - Mutual recognition with others
 * @param allCommitments - All network commitments
 * @param currentState - Current system state
 * @param previousState - Previous system state
 * @returns Allocation result
 */
export function computeAllocations(
	myPubKey: string,
	myCapacitySlots: AvailabilitySlot[],
	myRecognition: GlobalRecognitionWeights,
	mutualRecognition: Record<string, number>,
	allCommitments: Record<string, Commitment>,
	currentState: SystemStateSnapshot,
	previousState: SystemStateSnapshot | null
): AllocationResult {
	const iterationStartTime = Date.now();
	const allocations: SlotAllocationRecord[] = [];
	const slotDenominators: Record<string, { mutual: number; nonMutual: number; need_type_id: string }> = {};
	const totalsByTypeAndRecipient: Record<string, Record<string, number>> = {};
	
	// Process each capacity slot
	for (const capacitySlot of myCapacitySlots) {
		const typeId = capacitySlot.need_type_id;
		const providersAvailableCapacity = capacitySlot.quantity;
		
		if (!totalsByTypeAndRecipient[typeId]) {
			totalsByTypeAndRecipient[typeId] = {};
		}
		
		// Find compatible recipients
		const compatibleRecipients = findCompatibleRecipients(capacitySlot, allCommitments, myPubKey);
		
		if (compatibleRecipients.size === 0) continue;
		
		// ────────────────────────────────────────────────────────────
		// TIER 1: MUTUAL RECOGNITION (BIDIRECTIONAL CARE)
		// ────────────────────────────────────────────────────────────
		
		let capacityUsedInTier1 = 0;
		let tier1Denominator = 0;
		
		// Find mutually-recognized recipients
		const mutualEligibleRecipients: Array<{
			pubKey: string;
			need: number;
			mutualRecShare: number;
			activeNeed: number;
			needSlots: NeedSlot[];
		}> = [];
		
		let totalMutualRecognition = 0;
		
		// Calculate mutual recognition shares
		for (const [recipientPub, needSlots] of compatibleRecipients.entries()) {
			const mutualRec = mutualRecognition[recipientPub] || 0;
			if (mutualRec <= 0) continue;
			
			totalMutualRecognition += mutualRec;
		}
		
		if (totalMutualRecognition > 0) {
			// Calculate shares and numerators
			for (const [recipientPub, needSlots] of compatibleRecipients.entries()) {
				const mutualRec = mutualRecognition[recipientPub] || 0;
				if (mutualRec <= 0) continue;
				
				const mutualRecShare = mutualRec / totalMutualRecognition;
				
				let totalNeed = 0;
				for (const slot of needSlots) {
					totalNeed += slot.quantity;
				}
				
				// Get damping factor
				const recipientCommitment = allCommitments[recipientPub];
				const dampingFactor = recipientCommitment.multi_dimensional_damping?.damping_factors?.[typeId]
					|| recipientCommitment.multi_dimensional_damping?.global_damping_factor
					|| 1.0;
				
				const activeNeed = totalNeed * dampingFactor;
				
				mutualEligibleRecipients.push({
					pubKey: recipientPub,
					need: totalNeed,
					mutualRecShare,
					activeNeed,
					needSlots
				});
			}
			
			// Calculate denominator
			tier1Denominator = mutualEligibleRecipients.reduce(
				(sum, r) => sum + r.mutualRecShare * r.activeNeed,
				0
			);
			
			// Safety check for tiny denominators
			const MIN_RELATIVE_DENOMINATOR = 0.001;
			const minSafeDenominator = providersAvailableCapacity * MIN_RELATIVE_DENOMINATOR;
			
			if (tier1Denominator < minSafeDenominator && tier1Denominator > 0) {
				tier1Denominator = minSafeDenominator;
			}
			
			if (tier1Denominator > 0) {
				// Allocate to mutual recipients
				for (const recipient of mutualEligibleRecipients) {
					const yourRawAllocation =
						providersAvailableCapacity *
						(recipient.mutualRecShare * recipient.activeNeed) /
						tier1Denominator;
					
					const yourFinalAllocation = Math.min(yourRawAllocation, recipient.need);
					
					if (yourFinalAllocation > 0) {
						// Proportional distribution across need slots
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
						
						if (!totalsByTypeAndRecipient[typeId][recipient.pubKey]) {
							totalsByTypeAndRecipient[typeId][recipient.pubKey] = 0;
						}
						totalsByTypeAndRecipient[typeId][recipient.pubKey] += actuallyAllocated;
					}
				}
			}
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
			
			// Calculate non-mutual recognition shares
			for (const [recipientPub, needSlots] of compatibleRecipients.entries()) {
				const mutualRec = mutualRecognition[recipientPub] || 0;
				if (mutualRec > 0) continue; // Skip mutual (already allocated)
				
				const myRecOfThem = myRecognition[recipientPub] || 0;
				if (myRecOfThem <= 0) continue;
				
				totalNonMutualRecognition += myRecOfThem;
			}
			
			if (totalNonMutualRecognition > 0) {
				for (const [recipientPub, needSlots] of compatibleRecipients.entries()) {
					const mutualRec = mutualRecognition[recipientPub] || 0;
					if (mutualRec > 0) continue;
					
					const myRecOfThem = myRecognition[recipientPub] || 0;
					if (myRecOfThem <= 0) continue;
					
					const recognitionShare = myRecOfThem / totalNonMutualRecognition;
					
					let totalNeed = 0;
					for (const slot of needSlots) {
						totalNeed += slot.quantity;
					}
					
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
				
				tier2Denominator = nonMutualEligibleRecipients.reduce(
					(sum, r) => sum + r.recognitionShare * r.activeNeed,
					0
				);
				
				const MIN_RELATIVE_DENOMINATOR = 0.001;
				const minSafeDenominatorT2 = remainingCapacity * MIN_RELATIVE_DENOMINATOR;
				
				if (tier2Denominator < minSafeDenominatorT2 && tier2Denominator > 0) {
					tier2Denominator = minSafeDenominatorT2;
				}
				
				if (tier2Denominator > 0) {
					for (const recipient of nonMutualEligibleRecipients) {
						const yourRawAllocation =
							remainingCapacity *
							(recipient.recognitionShare * recipient.activeNeed) /
							tier2Denominator;
						
						const yourFinalAllocation = Math.min(yourRawAllocation, recipient.need);
						
						if (yourFinalAllocation > 0) {
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
	}
	
	// Compute convergence metrics
	const convergence = computeConvergenceSummary(
		currentState,
		previousState,
		iterationStartTime
	);
	
	return {
		allocations,
		slotDenominators,
		totalsByTypeAndRecipient,
		convergence
	};
}

// ═══════════════════════════════════════════════════════════════════
// NEED UPDATE LAW (CONVERGENCE DYNAMICS)
// ═══════════════════════════════════════════════════════════════════

/**
 * Apply need update law: N_next = N_current - Received
 * 
 * @param currentNeeds - Current needs per type
 * @param received - Amount received per type
 * @returns Updated needs
 */
export function applyNeedUpdateLaw(
	currentNeeds: Record<string, number>,
	received: Record<string, number>
): Record<string, number> {
	const nextNeeds: Record<string, number> = {};
	
	// Update existing needs
	for (const [typeId, need] of Object.entries(currentNeeds)) {
		const receivedAmount = received[typeId] || 0;
		nextNeeds[typeId] = Math.max(0, need - receivedAmount);
	}
	
	return nextNeeds;
}
