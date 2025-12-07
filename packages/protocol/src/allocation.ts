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

import { slotsCompatible, getTimeBucketKey, getLocationBucketKey } from './utils/match';
import { createMemoCache, createMemoCacheWithKey, hashObject } from './utils/memoize';

// Import distribution calculation functions
import {
	type DistributionResult,
	calculateTwoTierMutualRecognitionDistribution,
	calculateMutualRecognitionDistribution,
	calculateCollectiveRecognitionDistribution,
	calculateEqualSharesDistribution,
	createCustomDistribution,
	computeMutualRecognition
} from './distribution';

// Import unified filter system for compliance filters
import type {
	ComplianceFilter
} from './filters/types';

import {
	evaluateComplianceFilter,
	getRemainingRoom
} from './filters/compliance';

// Re-export for backward compatibility
export type { DistributionResult };
export {
	calculateTwoTierMutualRecognitionDistribution,
	calculateMutualRecognitionDistribution,
	calculateCollectiveRecognitionDistribution,
	calculateEqualSharesDistribution,
	createCustomDistribution,
	computeMutualRecognition
};

// Import ITC for causality tracking
import {
	type Stamp as ITCStampType,
	seed as itcSeed,
	event as itcEvent,
	join as itcJoin,
	leq as itcLeq,
	equals as itcEquals
} from './itc';

// ═══════════════════════════════════════════════════════════════════
// SIMPLIFIED TYPES (Bridge between full schemas and algorithm needs)
// ═══════════════════════════════════════════════════════════════════

/**
 * Spatial/Temporal Index for O(k) recipient lookups
 * 
 * Instead of scanning all N participants, use indexes to find only
 * the k participants who might match (by type, location, time).
 */
export interface SpaceTimeIndex {
	/** Type-based index: need_type_id -> Set<pubKey> */
	byType: Map<string, Set<string>>;
	
	/** Location-based index: location_bucket -> Set<pubKey> */
	byLocation: Map<string, Set<string>>;
	
	/** Time-based index: time_bucket -> Set<pubKey> */
	byTime: Map<string, Set<string>>;
	
	/** Composite index: "type|location" -> Set<pubKey> */
	byTypeAndLocation: Map<string, Set<string>>;
	
	/** Composite index: "type|time" -> Set<pubKey> */
	byTypeAndTime: Map<string, Set<string>>;
	
	/** Full composite: "type|location|time" -> Set<pubKey> */
	byAll: Map<string, Set<string>>;
}

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
function _buildSystemState(
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

// Memoized version of buildSystemState
// Note: timestamp and iteration are excluded from cache key since they're time-dependent
export const buildSystemState = createMemoCacheWithKey(
	(commitments: Record<string, Commitment>, previousState?: SystemStateSnapshot) => {
		// Call original function but regenerate timestamp/iteration
		const state = _buildSystemState(commitments, previousState);
		return {
			...state,
			timestamp: Date.now(),
			iteration: previousState ? previousState.iteration + 1 : 0
		};
	},
	(commitments, previousState) => 
		`${hashObject(commitments)}:${previousState ? `${previousState.iteration}:${hashObject(previousState.needsByPersonAndType)}:${hashObject(previousState.capacityByPersonAndType)}` : 'null'}`,
	20 // Cache up to 20 system state builds
);

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
 * Compute damping factors from over-allocation history (SCHEMA-ALIGNED)
 * 
 * Works with PerTypeDampingHistoryEntry format from schemas.ts.
 * Analyzes oscillation patterns to determine appropriate dampening.
 * 
 * @param history - Structured history per type (with timestamps)
 * @returns Damping factors per type (0.5 = slow, 0.8 = medium, 1.0 = full speed)
 */
function _computeDampingFactors(
	history: Record<string, Array<{ need_type_id: string; overAllocation: number; timestamp: number }>>
): Record<string, number> {
	const factors: Record<string, number> = {};
	
	for (const [typeId, hist] of Object.entries(history)) {
		if (hist.length < 3) {
			factors[typeId] = 0.8; // Medium speed by default
			continue;
		}
		
		// Check last 3 entries for oscillation (extract overAllocation values)
		const recent = hist.slice(-3).map(entry => entry.overAllocation);
		const upDownUp = recent[0] < recent[1] && recent[1] > recent[2];
		const downUpDown = recent[0] > recent[1] && recent[1] < recent[2];
		
		if (upDownUp || downUpDown) {
			factors[typeId] = 0.5; // Slow down (oscillation detected)
		} else {
			const isSmooth = recent[0] >= recent[1] && recent[1] >= recent[2];
			factors[typeId] = isSmooth ? 1.0 : 0.8;
		}
	}
	
	return factors;
}

// Memoized version of computeDampingFactors
export const computeDampingFactors = createMemoCache(
	_computeDampingFactors,
	50 // Cache up to 50 damping factor computations
);

/**
 * Update over-allocation history with new received amounts (SCHEMA-ALIGNED)
 * 
 * Uses PerTypeDampingHistoryEntry format from schemas.ts for full alignment.
 * This enables proper timestamp tracking and structured history.
 * 
 * @param history - Current history (per-type arrays of structured entries)
 * @param received - Amount received this iteration per type
 * @param needs - Current needs per type
 * @returns Updated history with structured entries
 */
export function updateOverAllocationHistory(
	history: Record<string, Array<{ need_type_id: string; overAllocation: number; timestamp: number }>>,
	received: Record<string, number>,
	needs: Record<string, number>
): Record<string, Array<{ need_type_id: string; overAllocation: number; timestamp: number }>> {
	const newHistory = { ...history };
	const timestamp = Date.now();
	
	for (const [typeId, receivedAmount] of Object.entries(received)) {
		const need = needs[typeId] || 0;
		const overAllocation = Math.max(0, receivedAmount - need);
		
		if (!newHistory[typeId]) {
			newHistory[typeId] = [];
		}
		
		// ✅ Create structured entry per PerTypeDampingHistoryEntrySchema
		const entry = {
			need_type_id: typeId,
			overAllocation,
			timestamp
		};
		
		newHistory[typeId] = [...newHistory[typeId], entry].slice(-10); // Keep last 10
	}
	
	return newHistory;
}

// ═══════════════════════════════════════════════════════════════════
// ALLOCATION COMPUTATION (TWO-TIER SYSTEM)
// ═══════════════════════════════════════════════════════════════════

/**
 * Apply divisibility constraints to an allocation quantity
 * 
 * @param rawQuantity - The calculated allocation amount
 * @param sharePercentage - What percentage of total capacity this represents
 * @param capacitySlot - The capacity slot with divisibility constraints
 * @returns Constrained quantity respecting both natural and percentage divisibility
 */
export function applyDivisibilityConstraints(
	rawQuantity: number,
	sharePercentage: number,
	capacitySlot: AvailabilitySlot
): number {
	const PERCENTAGE_EPSILON = 0.0001; // Tolerance for floating-point percentage comparisons
	const maxNatural = capacitySlot.max_natural_div || 1;
	const minPercent = capacitySlot.min_allocation_percentage || 0.0;
	
	// 1. Apply percentage constraint (prevent over-fragmentation)
	// If this recipient's share is below the minimum threshold, reject it
	if (minPercent > PERCENTAGE_EPSILON && sharePercentage < minPercent - PERCENTAGE_EPSILON) {
		return 0;
	}
	
	// 2. Apply natural divisibility constraint (round to whole units)
	// e.g., if max_natural_div=1 (whole rooms), 2.7 becomes 2
	const naturalConstrained = Math.floor(rawQuantity / maxNatural) * maxNatural;
	
	return naturalConstrained;
}

/**
 * Check if a recipient's allocation would be too small given divisibility constraints
 * 
 * @param allocation - The proposed allocation quantity
 * @param capacitySlot - The capacity slot with divisibility constraints
 * @returns true if allocation meets minimum thresholds
 */
export function meetsMinimumAllocation(
	allocation: number,
	capacitySlot: AvailabilitySlot
): boolean {
	const PERCENTAGE_EPSILON = 0.0001; // Tolerance for floating-point percentage comparisons
	const maxNatural = capacitySlot.max_natural_div || 1;
	const minPercent = capacitySlot.min_allocation_percentage || 0.0;
	
	// Must be at least one natural unit
	if (allocation < maxNatural) return false;
	
	// Must meet minimum percentage threshold
	// This ensures we don't fragment capacity beyond the provider's preference
	const sharePercentage = allocation / capacitySlot.quantity;
	
	// If min_allocation_percentage is set, enforce it as the threshold
	// Otherwise, accept any allocation ≥ 1 natural unit
	if (minPercent > PERCENTAGE_EPSILON) {
		return sharePercentage >= minPercent - PERCENTAGE_EPSILON;
	}
	
	return allocation >= maxNatural;
}

/**
 * Redistribute remainder capacity using Largest Remainder Method
 * 
 * When divisibility constraints cause rounding, we lose fractional capacity.
 * This function redistributes leftover units to recipients with largest remainders,
 * and distributes proportionally across each recipient's slots.
 * 
 * Example: 10 rooms, recipients get 4.7, 3.2, 2.1 → floor to 4, 3, 2 = 9 rooms
 * Remainder: 1 room left. Give to recipient with largest fraction (0.7) → 5, 3, 2 = 10 ✅
 * 
 * If recipient has multiple slots, distribute proportionally:
 * - Slot A: 6 units (60%), Slot B: 4 units (40%)
 * - Gets 5 extra units → Slot A gets 3 (60% of 5), Slot B gets 2 (40% of 5)
 * 
 * **Complexity Analysis**:
 * - Time: O(r log r + r×s) where r = recipients with remainders, s = avg slots per recipient
 *   - Phase 1 (remainder-based): O(r log r) for sorting + O(r×s) for distribution
 *   - Phase 2 (recognition-based fallback): O(r log r) for sorting + O(r×s) for distribution
 *   - Overall: O(r log r + r×s) which is efficient for typical client-side values
 * - Space: O(r + s) for temporary arrays and maps
 * - **Client-Side Performance**: Acceptable with typical values (r < 100, s < 10)
 * 
 * **Future Optimization**: If allocation runs multiple times with same inputs, consider memoization.
 * 
 * @param allocations - Array of allocation records to potentially increase
 * @param remainders - Map of recipient -> remainder (fractional part lost to rounding)
 * @param capacityUsed - How much capacity has been allocated so far
 * @param totalCapacity - Total available capacity
 * @param maxNatural - Natural divisibility unit
 * @param capacitySlot - The capacity slot being allocated
 * @returns Updated capacity used after redistribution
 */
export function redistributeRemainders(
	allocations: SlotAllocationRecord[],
	remainders: Map<string, number>,
	capacityUsed: number,
	totalCapacity: number,
	maxNatural: number,
	capacitySlot: AvailabilitySlot
): number {
	// Calculate leftover capacity (must be at least one natural unit)
	const leftoverCapacity = totalCapacity - capacityUsed;
	const unitsToDistribute = Math.floor(leftoverCapacity / maxNatural);
	
	if (unitsToDistribute < 1) {
		return capacityUsed; // No whole units to redistribute
	}
	
	// Sort recipients by remainder size (descending)
	const recipientsByRemainder = Array.from(remainders.entries())
		.filter(([_, remainder]) => remainder > 0)
		.sort((a, b) => b[1] - a[1]); // Largest remainder first
	
	let unitsDistributed = 0;
	
	// Give units to recipients with largest remainders
	for (const [recipientPub, remainder] of recipientsByRemainder) {
		if (unitsDistributed >= unitsToDistribute) break;
		
		// Find this recipient's allocation records
		const recipientAllocations = allocations.filter(a => a.recipient_pubkey === recipientPub);
		
		if (recipientAllocations.length === 0) continue;
		
		// Count how many units this recipient should get
		// (They get at least 1, but might get more if we have many leftover units)
		const recipientUnits = Math.min(
			Math.floor(unitsToDistribute / recipientsByRemainder.length),
			unitsToDistribute - unitsDistributed
		);
		const actualUnits = Math.max(1, recipientUnits);
		
		// Distribute units proportionally across recipient's slots
		if (recipientAllocations.length === 1) {
			// Simple case: only one slot
			recipientAllocations[0].quantity += actualUnits * maxNatural;
		} else {
			// Multiple slots: distribute proportionally using Largest Remainder Method again!
			const totalAllocated = recipientAllocations.reduce((sum, a) => sum + a.quantity, 0);
			
			// Calculate ideal fractional allocation per slot
			const slotRemainders: Array<{ alloc: SlotAllocationRecord; remainder: number; ideal: number }> = [];
			let integerUnitsDistributed = 0;
			
			for (const alloc of recipientAllocations) {
				const proportion = alloc.quantity / totalAllocated;
				const idealUnits = actualUnits * proportion;
				const integerUnits = Math.floor(idealUnits);
				const remainderFraction = idealUnits - integerUnits;
				
				// Give integer part immediately
				alloc.quantity += integerUnits * maxNatural;
				integerUnitsDistributed += integerUnits;
				
				slotRemainders.push({ alloc, remainder: remainderFraction, ideal: idealUnits });
			}
			
			// Distribute remaining units by largest remainder
			const leftoverUnits = actualUnits - integerUnitsDistributed;
			if (leftoverUnits > 0) {
				slotRemainders.sort((a, b) => b.remainder - a.remainder);
				
				for (let i = 0; i < leftoverUnits && i < slotRemainders.length; i++) {
					slotRemainders[i].alloc.quantity += maxNatural;
				}
			}
		}
		
		unitsDistributed += actualUnits;
		
		console.log(
			`[REMAINDER-REDISTRIBUTION] Gave ${actualUnits * maxNatural} unit(s) to ${recipientPub} ` +
			`across ${recipientAllocations.length} slot(s) (remainder: ${remainder.toFixed(3)})`
		);
	}
	
	// If we still have leftover capacity but ran out of recipients with remainders,
	// distribute remaining capacity proportionally by recognition shares
	if (unitsDistributed < unitsToDistribute && allocations.length > 0) {
		const remainingUnits = unitsToDistribute - unitsDistributed;
		
		console.log(
			`[REMAINDER-REDISTRIBUTION] Still have ${remainingUnits * maxNatural} units left ` +
			`after exhausting remainders. Distributing by allocation proportion...`
		);
		
		// Calculate total allocated (as proxy for recognition share)
		const recipientTotals = new Map<string, number>();
		for (const alloc of allocations) {
			const current = recipientTotals.get(alloc.recipient_pubkey) || 0;
			recipientTotals.set(alloc.recipient_pubkey, current + alloc.quantity);
		}
		
		const totalAllocated = Array.from(recipientTotals.values()).reduce((sum, val) => sum + val, 0);
		
		// Sort recipients by their allocation size (proportional to recognition)
		const recipientsByAllocation = Array.from(recipientTotals.entries())
			.sort((a, b) => b[1] - a[1]); // Largest allocation first
		
		// Distribute remaining units proportionally using Largest Remainder Method
		const recipientShares: Array<{ pubKey: string; ideal: number; integer: number; remainder: number }> = [];
		let extraUnitsDistributed = 0;
		
		for (const [recipientPub, allocated] of recipientsByAllocation) {
			const proportion = allocated / totalAllocated;
			const idealUnits = remainingUnits * proportion;
			const integerUnits = Math.floor(idealUnits);
			const remainderFraction = idealUnits - integerUnits;
			
			// Find this recipient's allocations
			const recipientAllocations = allocations.filter(a => a.recipient_pubkey === recipientPub);
			
			if (recipientAllocations.length > 0 && integerUnits > 0) {
				// Distribute integer part proportionally across slots
				if (recipientAllocations.length === 1) {
					recipientAllocations[0].quantity += integerUnits * maxNatural;
				} else {
					// Distribute across multiple slots proportionally
					const recipientTotal = recipientAllocations.reduce((sum, a) => sum + a.quantity, 0);
					for (const alloc of recipientAllocations) {
						const slotProportion = alloc.quantity / recipientTotal;
						const slotUnits = Math.floor(integerUnits * slotProportion);
						alloc.quantity += slotUnits * maxNatural;
					}
				}
				extraUnitsDistributed += integerUnits;
			}
			
			if (remainderFraction > 0) {
				recipientShares.push({ pubKey: recipientPub, ideal: idealUnits, integer: integerUnits, remainder: remainderFraction });
			}
		}
		
		// Distribute final fractional units by largest remainder
		const leftoverExtraUnits = remainingUnits - extraUnitsDistributed;
		if (leftoverExtraUnits > 0 && recipientShares.length > 0) {
			recipientShares.sort((a, b) => b.remainder - a.remainder);
			
			for (let i = 0; i < leftoverExtraUnits && i < recipientShares.length; i++) {
				const recipientAllocations = allocations.filter(a => a.recipient_pubkey === recipientShares[i].pubKey);
				if (recipientAllocations.length > 0) {
					// Give to the recipient's largest slot
					recipientAllocations.sort((a, b) => b.quantity - a.quantity);
					recipientAllocations[0].quantity += maxNatural;
					extraUnitsDistributed++;
				}
			}
		}
		
		unitsDistributed += extraUnitsDistributed;
		
		console.log(
			`[REMAINDER-REDISTRIBUTION] Distributed ${extraUnitsDistributed * maxNatural} additional units ` +
			`based on recognition-proportional allocation`
		);
	}
	
	const redistributedCapacity = unitsDistributed * maxNatural;
	console.log(
		`[REMAINDER-REDISTRIBUTION] Total distributed: ${redistributedCapacity} leftover capacity ` +
		`across ${Math.ceil(unitsDistributed / maxNatural)} recipient grants`
	);
	
	return capacityUsed + redistributedCapacity;
}

/**
 * Get candidate recipients using spatial/temporal index (O(k) instead of O(N))
 * 
 * @param capacitySlot - The capacity slot to find recipients for
 * @param needsIndex - The spatial/temporal index (optional)
 * @returns Set of pubKeys that potentially need this capacity
 */
function getCandidateRecipients(
	capacitySlot: AvailabilitySlot,
	needsIndex?: SpaceTimeIndex
): Set<string> {
	if (!needsIndex) {
		// No index provided - will do full scan
		return new Set();
	}
	
	const typeId = capacitySlot.need_type_id;
	const locationKey = getLocationBucketKey(capacitySlot);
	const timeKey = getTimeBucketKey(capacitySlot);
	
	// Strategy: Use most specific index available
	
	// 1. Try full composite (most specific)
	const fullKey = `${typeId}|${locationKey}|${timeKey}`;
	if (needsIndex.byAll.has(fullKey)) {
		return needsIndex.byAll.get(fullKey)!;
	}
	
	// 2. Try type + location
	const typeLocKey = `${typeId}|${locationKey}`;
	if (needsIndex.byTypeAndLocation.has(typeLocKey)) {
		return needsIndex.byTypeAndLocation.get(typeLocKey)!;
	}
	
	// 3. Try type + time
	const typeTimeKey = `${typeId}|${timeKey}`;
	if (needsIndex.byTypeAndTime.has(typeTimeKey)) {
		return needsIndex.byTypeAndTime.get(typeTimeKey)!;
	}
	
	// 4. Fall back to type only
	if (needsIndex.byType.has(typeId)) {
		return needsIndex.byType.get(typeId)!;
	}
	
	// 5. No candidates found
	return new Set();
}

/**
 * Find compatible recipients for a capacity slot
 * Returns map of pubKey -> compatible need slots
 * 
 * @param capacitySlot - The capacity slot
 * @param allCommitments - All network commitments
 * @param myPubKey - My public key (to exclude self)
 * @param needsIndex - Optional spatial/temporal index for O(k) lookups
 */
function _findCompatibleRecipients(
	capacitySlot: AvailabilitySlot,
	allCommitments: Record<string, Commitment>,
	myPubKey: string,
	needsIndex?: SpaceTimeIndex
): Map<string, NeedSlot[]> {
	const compatible = new Map<string, NeedSlot[]>();
	const typeId = capacitySlot.need_type_id;
	
	console.log(`[FIND-COMPATIBLE] Searching for recipients for capacity slot ${capacitySlot.id.slice(0,20)}... (type: ${typeId})`);
	
	// Get candidate recipients from index (O(k)) or full scan (O(N))
	const candidates = getCandidateRecipients(capacitySlot, needsIndex);
	
	// If no index provided, fall back to full scan
	const recipientsToCheck = candidates.size > 0 
		? Array.from(candidates)
		: Object.keys(allCommitments);
	
	console.log(`[FIND-COMPATIBLE] Checking ${recipientsToCheck.length} potential recipients`);
	
	for (const recipientPub of recipientsToCheck) {
		// NOTE: Self-allocation is ALLOWED! Self-care is valid care.
		// Mutual recognition with yourself is valid recognition.
		
		const commitment = allCommitments[recipientPub];
		if (!commitment?.need_slots) {
			console.log(`[FIND-COMPATIBLE]   ${recipientPub.slice(0,20)}... - SKIP: no need slots`);
			continue;
		}
		
		console.log(`[FIND-COMPATIBLE]   ${recipientPub.slice(0,20)}... - checking ${commitment.need_slots.length} need slots`);
		
		const compatibleSlots: NeedSlot[] = [];
		for (const needSlot of commitment.need_slots) {
			if (needSlot.need_type_id !== typeId) {
				console.log(`[FIND-COMPATIBLE]     ${needSlot.id.slice(0,20)}... - SKIP: type mismatch (${needSlot.need_type_id} !== ${typeId})`);
				continue;
			}
			
			// Check slot compatibility (time, location, etc.)
			const isCompatible = slotsCompatible(capacitySlot, needSlot);
			console.log(`[FIND-COMPATIBLE]     ${needSlot.id.slice(0,20)}... - slotsCompatible: ${isCompatible}`);
			if (isCompatible) {
				compatibleSlots.push(needSlot);
			}
		}
		
		if (compatibleSlots.length > 0) {
			console.log(`[FIND-COMPATIBLE]   ✅ ${recipientPub.slice(0,20)}... - FOUND ${compatibleSlots.length} compatible slots`);
			compatible.set(recipientPub, compatibleSlots);
		} else {
			console.log(`[FIND-COMPATIBLE]   ❌ ${recipientPub.slice(0,20)}... - NO compatible slots`);
		}
	}
	
	console.log(`[FIND-COMPATIBLE] Result: ${compatible.size} recipients with compatible needs`);
	
	return compatible;
}

// Memoized version of findCompatibleRecipients
const findCompatibleRecipients = createMemoCacheWithKey(
	_findCompatibleRecipients,
	(capacitySlot, allCommitments, myPubKey, needsIndex) => 
		`${capacitySlot.id}:${hashObject(allCommitments)}:${myPubKey}:${needsIndex ? 'indexed' : 'full'}`,
	50 // Cache up to 50 capacity slot lookups
);

/**
 * Compute allocations for my capacity slots
 * 
 * **Algorithm Overview**:
 * 1. For each capacity slot, find compatible recipients (with optional spatial/temporal index for O(k) lookup)
 * 2. Tier 1: Allocate based on mutual recognition using multi-pass proportional redistribution
 * 3. Tier 2: Allocate remaining capacity based on one-way recognition (also multi-pass)
 * 4. Apply divisibility constraints (natural units + percentage limits)
 * 5. Redistribute remainders using Largest Remainder Method
 * 
 * **Multi-Pass Proportional Algorithm**:
 * - All recipients' allocations calculated simultaneously (same denominator = true proportionality)
 * - Recipients who are satisfied (capped at need) removed from next pass
 * - Excess capacity automatically redistributes to unsatisfied recipients
 * - Guarantees no FIFO bias, maintains proportional allocation per README formula
 * 
 * **Complexity Analysis**:
 * - Time: O(C × P × R × S) where C = capacity slots, P = passes (typically 2-3), R = recipients, S = avg slots per recipient
 *   - Finding compatible recipients: O(C × k) with index or O(C × R) without
 *   - Multi-pass allocation: O(P × R × S) per capacity slot
 *   - Remainder redistribution: O(R log R + R×S) per capacity slot
 *   - Overall: O(C × P × R × S) which is acceptable (P is small constant)
 * - Space: O(C × R × S) for allocation records
 * - **Client-Side Performance**: Acceptable with typical values (C < 20, P < 5, R < 100, S < 10)
 * 
 * **Performance Notes**:
 * - Spatial/temporal indexing reduces effective R for compatibility checks (O(k) instead of O(N))
 * - Multi-pass typically converges in 2-3 passes (most recipients satisfied in pass 1)
 * - Divisibility constraints prevent over-fragmentation
 * - Remainder redistribution ensures near-100% capacity utilization
 * - ✅ Memoization: findCompatibleRecipients is memoized for repeated calls
 * 
 * @param myPubKey - My public key
 * @param myCapacitySlots - My available capacity slots
 * @param myRecognition - My recognition of others
 * @param mutualRecognition - Mutual recognition with others
 * @param allCommitments - All network commitments
 * @param currentState - Current system state
 * @param previousState - Previous system state
 * @param needsIndex - Optional spatial/temporal index for O(k) recipient lookups
 * @returns Allocation result
 */
/**
 * Generic allocation engine that accepts pre-computed distribution
 * 
 * This is the unified allocation engine that works with any distribution strategy.
 * It performs slot-level matching, applies compliance filters, handles divisibility
 * constraints, and uses multi-pass proportional allocation.
 * 
 * ✅ DAMPENING SUPPORT: Accepts activeNeedsByRecipient to use damped needs instead
 * of declared needs. This prevents oscillation per README.md line 283-298.
 * 
 * @param myPubKey - Provider's public key
 * @param myCapacitySlots - Provider's availability slots
 * @param distribution - Pre-computed distribution (who gets what share)
 * @param allCommitments - All participants' commitments
 * @param needsIndex - Optional spatial/temporal index for efficient recipient lookup
 * @param recipientFilters - Optional compliance filters per recipient
 * @param activeNeedsByRecipient - Optional damped needs (activeNeed = declaredNeed × dampingFactor)
 * @returns Allocation result with slot allocations and metadata
 */
export function allocateWithDistribution(
	myPubKey: string,
	myCapacitySlots: AvailabilitySlot[],
	distribution: DistributionResult,
	allCommitments: Record<string, Commitment>,
	needsIndex?: SpaceTimeIndex,
	recipientFilters?: Map<string, ComplianceFilter>,
	activeNeedsByRecipient?: Record<string, Record<string, number>>  // ✅ Damped needs (activeNeed = declaredNeed × dampingFactor)
): AllocationResult {
	const iterationStartTime = Date.now();
	const allocations: SlotAllocationRecord[] = [];
	const slotDenominators: Record<string, { mutual: number; nonMutual: number; need_type_id: string }> = {};
	const totalsByTypeAndRecipient: Record<string, Record<string, number>> = {};
	
	console.log(`[ALLOCATE-WITH-DISTRIBUTION] Starting allocation with ${distribution.method} distribution`);
	console.log(`[ALLOCATE-WITH-DISTRIBUTION] Recipients:`, Object.keys(distribution.shares).length);
	
	// Process each capacity slot
	for (const capacitySlot of myCapacitySlots) {
		const typeId = capacitySlot.need_type_id;
		const providersAvailableCapacity = capacitySlot.quantity;
		
		if (!totalsByTypeAndRecipient[typeId]) {
			totalsByTypeAndRecipient[typeId] = {};
		}
		
		// Find compatible recipients (using spatial/temporal index if provided)
		const compatibleRecipients = findCompatibleRecipients(capacitySlot, allCommitments, myPubKey, needsIndex);
		
		if (compatibleRecipients.size === 0) continue;
		
		console.log(`[ALLOCATE-WITH-DISTRIBUTION] Slot ${capacitySlot.id.slice(0,20)}: ${compatibleRecipients.size} compatible recipients`);
		
		// ────────────────────────────────────────────────────────────
		// SINGLE-PASS PROPORTIONAL ALLOCATION (DISTRIBUTION-BASED)
		// ────────────────────────────────────────────────────────────
		
		const CAPACITY_EPSILON = 0.0001;
		let capacityUsed = 0;
		
		// Build list of eligible recipients with distribution shares
		const eligibleRecipients: Array<{
			pubKey: string;
			totalNeed: number;
			remainingNeed: number;
			distributionShare: number;
			needSlots: NeedSlot[];
			tier: 'mutual' | 'non-mutual';
		}> = [];
		
		// Determine tiers from distribution metadata
		const tier1Recipients = new Map<string, number>(); // recipientId -> share
		const tier2Recipients = new Map<string, number>();
		
		if (distribution.tiers) {
			// Two-tier distribution - use tier-specific shares
			for (const recipientId in distribution.tiers.tier1) {
				if (distribution.tiers.tier1[recipientId] > 0) {
					tier1Recipients.set(recipientId, distribution.tiers.tier1[recipientId]);
				}
			}
			for (const recipientId in distribution.tiers.tier2) {
				if (distribution.tiers.tier2[recipientId] > 0) {
					tier2Recipients.set(recipientId, distribution.tiers.tier2[recipientId]);
				}
			}
			
		}
		
		// Build eligible recipients with tier-aware shares
		// For two-tier: allocate Tier 1 first, then Tier 2 gets remainder
		for (const [recipientPub, needSlots] of compatibleRecipients.entries()) {
			// ✅ Use active (damped) needs if provided, otherwise use declared needs
			let totalNeed = 0;
			if (activeNeedsByRecipient && activeNeedsByRecipient[recipientPub]) {
				// Sum all active needs for this recipient for this type
				const typeId = capacitySlot.need_type_id;
				totalNeed = activeNeedsByRecipient[recipientPub][typeId] || 0;
			} else {
				// Fallback: use declared needs from slots
				for (const slot of needSlots) {
					totalNeed += slot.quantity;
				}
			}
			
			// Check if in tier1 first (priority)
			if (tier1Recipients.has(recipientPub)) {
				eligibleRecipients.push({
					pubKey: recipientPub,
					totalNeed,
					remainingNeed: totalNeed,
					distributionShare: tier1Recipients.get(recipientPub)!,
					needSlots,
					tier: 'mutual'
				});
			} else if (tier2Recipients.has(recipientPub)) {
				eligibleRecipients.push({
					pubKey: recipientPub,
					totalNeed,
					remainingNeed: totalNeed,
					distributionShare: tier2Recipients.get(recipientPub)!,
					needSlots,
					tier: 'non-mutual'
				});
			} else {
				// No tier info - use combined share
				const share = distribution.shares[recipientPub] || 0;
				if (share > 0) {
					eligibleRecipients.push({
						pubKey: recipientPub,
						totalNeed,
						remainingNeed: totalNeed,
						distributionShare: share,
						needSlots,
						tier: 'mutual'
					});
				}
			}
		}
		
		// Sort by tier (mutual first) to ensure Tier 1 gets priority
		eligibleRecipients.sort((a, b) => {
			if (a.tier === 'mutual' && b.tier === 'non-mutual') return -1;
			if (a.tier === 'non-mutual' && b.tier === 'mutual') return 1;
			return 0;
		});
		
		if (eligibleRecipients.length === 0) continue;
		
		// ═══════════════════════════════════════════════════════════════
		// MULTI-PASS PROPORTIONAL ALLOCATION
		// ═══════════════════════════════════════════════════════════════
		
		let unsatisfiedRecipients = [...eligibleRecipients];
		let remainingCapacity = providersAvailableCapacity;
		let passCount = 0;
		const maxPasses = 10;
		
		console.log(`[ALLOCATE-WITH-DISTRIBUTION] Starting multi-pass: capacity=${remainingCapacity}, recipients=${unsatisfiedRecipients.length}`);
		
		while (remainingCapacity > CAPACITY_EPSILON && unsatisfiedRecipients.length > 0 && passCount < maxPasses) {
			passCount++;
			console.log(`[ALLOCATE-WITH-DISTRIBUTION] Pass ${passCount}: capacity=${remainingCapacity.toFixed(2)}, unsatisfied=${unsatisfiedRecipients.length}`);
			
			// TWO-TIER LOGIC: Only allocate to current tier until exhausted
			const currentTier = unsatisfiedRecipients[0]?.tier || 'mutual';
			const currentTierRecipients = unsatisfiedRecipients.filter(r => r.tier === currentTier);
			
			console.log(`[ALLOCATE-WITH-DISTRIBUTION]   Current tier: ${currentTier}, recipients: ${currentTierRecipients.length}`);
			
			// PHASE 1: Calculate denominator with only current tier unsatisfied recipients
			let denominator = currentTierRecipients.reduce(
				(sum, r) => sum + r.distributionShare,
				0
			);
			
			if (denominator < CAPACITY_EPSILON) break;
			
			// Safety check for tiny denominators
			const MIN_RELATIVE_DENOMINATOR = 0.001;
			const minSafeDenominator = remainingCapacity * MIN_RELATIVE_DENOMINATOR;
			if (denominator < minSafeDenominator) {
				denominator = minSafeDenominator;
			}
			
			// PHASE 2: Calculate ALL proportional allocations BEFORE capping (only current tier)
			const proportionalAllocations = currentTierRecipients.map(recipient => {
				const rawAllocation = remainingCapacity * 
					recipient.distributionShare / denominator;
				
				// Apply compliance filter if present
				let filterLimit = Infinity;
				if (recipientFilters && recipientFilters.has(recipient.pubKey)) {
					const filter = recipientFilters.get(recipient.pubKey)!;
					const currentTotal = totalsByTypeAndRecipient[typeId]?.[recipient.pubKey] || 0;
					const recipientCommitment = allCommitments[recipient.pubKey];
					
					filterLimit = evaluateComplianceFilter(filter, {
						pubKey: recipient.pubKey,
						currentTotal,
						proposedAmount: rawAllocation,
						commitment: recipientCommitment,
						mutualRecognition: 0, // Not used in distribution-based allocation
						attributes: (recipientCommitment as any)?.attributes || {}
					});
					
					filterLimit = Math.max(0, filterLimit - currentTotal);
				}
				
				return {
					recipient,
					rawAllocation,
					cappedAllocation: Math.min(rawAllocation, recipient.remainingNeed, filterLimit)
				};
			});
			
			// PHASE 3: Apply allocations and track satisfaction
			let capacityUsedThisPass = 0;
			const nowSatisfied: typeof unsatisfiedRecipients = [];
			
			for (const { recipient, rawAllocation, cappedAllocation } of proportionalAllocations) {
				if (cappedAllocation <= CAPACITY_EPSILON) continue;
				
				// Calculate share percentage for divisibility checks
				const recipientSharePercentage = cappedAllocation / providersAvailableCapacity;
				
				// Apply divisibility constraints
				const constrainedAllocation = applyDivisibilityConstraints(
					cappedAllocation,
					recipientSharePercentage,
					capacitySlot
				);
				
				// Check if allocation meets minimum threshold
				if (!meetsMinimumAllocation(constrainedAllocation, capacitySlot)) {
					continue;
				}
				
				// Proportional distribution across need slots
				const totalCompatibleNeed = recipient.needSlots.reduce((sum, slot) => sum + slot.quantity, 0);
				let actuallyAllocated = 0;
				
				for (const needSlot of recipient.needSlots) {
					const proportion = needSlot.quantity / totalCompatibleNeed;
					let slotAllocation = Math.min(
						needSlot.quantity,
						constrainedAllocation * proportion
					);
					
					// Apply natural unit rounding
					const maxNaturalDiv = capacitySlot.max_natural_div || 1;
					slotAllocation = Math.floor(slotAllocation / maxNaturalDiv) * maxNaturalDiv;
					
					if (slotAllocation > 0) {
						allocations.push({
							quantity: slotAllocation,
							need_type_id: typeId,
							availability_slot_id: capacitySlot.id,
							recipient_pubkey: recipient.pubKey,
							time_compatible: true,
							location_compatible: true,
							tier: recipient.tier,
							recipient_need_slot_id: needSlot.id
						});
						
						actuallyAllocated += slotAllocation;
					}
				}
				
				// Update tracking
				capacityUsedThisPass += actuallyAllocated;
				capacityUsed += actuallyAllocated;
				
				if (!totalsByTypeAndRecipient[typeId][recipient.pubKey]) {
					totalsByTypeAndRecipient[typeId][recipient.pubKey] = 0;
				}
				totalsByTypeAndRecipient[typeId][recipient.pubKey] += actuallyAllocated;
				
				// Update remaining need
				recipient.remainingNeed -= actuallyAllocated;
				
				// Check if satisfied
				if (recipient.remainingNeed <= CAPACITY_EPSILON) {
					nowSatisfied.push(recipient);
				}
			}
			
			// Update capacity and recipients for next pass
			remainingCapacity -= capacityUsedThisPass;
			unsatisfiedRecipients = unsatisfiedRecipients.filter(r => !nowSatisfied.includes(r));
			
			// Exit if no progress made in current tier
			if (nowSatisfied.length === 0 && capacityUsedThisPass < CAPACITY_EPSILON) {
				// If we're stuck in current tier, move to next tier (if any)
				const remainingTiers = unsatisfiedRecipients.filter(r => r.tier !== currentTier);
				if (remainingTiers.length === 0) {
					break; // No more tiers, exit
				}
				// Continue to next tier in next pass
			}
		}
		
		// Store denominator for tracking
		slotDenominators[capacitySlot.id] = {
			mutual: tier1Recipients.size,
			nonMutual: tier2Recipients.size,
			need_type_id: typeId
		};
	}
	
	const executionTime = Date.now() - iterationStartTime;
	
	console.log(`[ALLOCATE-WITH-DISTRIBUTION] Complete: ${allocations.length} allocations in ${executionTime}ms`);
	
	// Compute empty convergence (allocateWithDistribution doesn't track state)
	const emptyConvergence = {
		totalNeedMagnitude: 0,
		previousNeedMagnitude: 0,
		contractionRate: 0,
		isConverged: false,
		percentNeedsMet: 0,
		universalSatisfaction: false,
		iterationsToConvergence: null,
		maxPersonNeed: 0,
		needVariance: 0,
		peopleStuck: 0,
		executionTimeMs: executionTime,
		currentIteration: 0,
		responseLatency: executionTime
	};
	
	return {
		allocations,
		slotDenominators,
		totalsByTypeAndRecipient,
		convergence: emptyConvergence
	};
}

/**
 * Compute allocations with optional compliance filters
 * 
 * This function now delegates to allocateWithDistribution() after calculating
 * the two-tier distribution (mutual + non-mutual recognition).
 * 
 * @param myPubKey - Provider's public key
 * @param myCapacitySlots - Provider's availability slots
 * @param myRecognition - Provider's recognition of others
 * @param mutualRecognition - Mutual recognition scores
 * @param allCommitments - All participants' commitments
 * @param currentState - Current system state
 * @param previousState - Previous system state (for convergence tracking)
 * @param needsIndex - Optional spatial/temporal index for efficient recipient lookup
 * @param recipientFilters - Optional compliance filters per recipient (blocked, capped, unlimited)
 * @returns Allocation result with slot allocations and metadata
 */
export function computeAllocations(
	myPubKey: string,
	myCapacitySlots: AvailabilitySlot[],
	myRecognition: GlobalRecognitionWeights,
	mutualRecognition: Record<string, number>,
	allCommitments: Record<string, Commitment>,
	currentState: SystemStateSnapshot,
	previousState: SystemStateSnapshot | null,
	needsIndex?: SpaceTimeIndex,
	recipientFilters?: Map<string, ComplianceFilter>
): AllocationResult {
	const iterationStartTime = Date.now();
	
	// ✅ PHASE 1: Extract dampening factors and compute active needs (README.md line 291)
	// Formula: activeNeed = declaredNeed × dampingFactor
	const activeNeedsByRecipient: Record<string, Record<string, number>> = {};
	
	console.log('[DAMPENING] Extracting dampening factors from commitments...');
	
	for (const [recipientPub, commitment] of Object.entries(allCommitments)) {
		if (!commitment.need_slots || commitment.need_slots.length === 0) continue;
		
		const activeNeeds: Record<string, number> = {};
		
		// Get global damping factor (fallback)
		const globalDamping = commitment.multi_dimensional_damping?.global_damping_factor || 1.0;
		
		// Get type-specific damping factors
		const typeDampingFactors = commitment.multi_dimensional_damping?.damping_factors || {};
		
		// Apply dampening to each need slot
		for (const needSlot of commitment.need_slots) {
			const typeId = needSlot.need_type_id;
			const declaredNeed = needSlot.quantity;
			
			// Type-specific takes precedence, fallback to global
			const dampingFactor = typeDampingFactors[typeId] || globalDamping;
			
			// Apply dampening formula
			const activeNeed = declaredNeed * dampingFactor;
			
			// Aggregate by type (in case of multiple slots of same type)
			activeNeeds[typeId] = (activeNeeds[typeId] || 0) + activeNeed;
			
			// Log dampening when it's actually applied (factor < 1.0)
			if (dampingFactor < 1.0) {
				console.log(
					`[DAMPENING] ${recipientPub.slice(0,20)}...[${typeId}]: ` +
					`declared=${declaredNeed.toFixed(2)}, damping=${dampingFactor.toFixed(2)}, ` +
					`active=${activeNeed.toFixed(2)}`
				);
			}
		}
		
		activeNeedsByRecipient[recipientPub] = activeNeeds;
	}
	
	const recipientsWithDampening = Object.values(activeNeedsByRecipient)
		.filter(needs => Object.values(needs).some((_, idx, arr) => {
			const commitment = allCommitments[Object.keys(activeNeedsByRecipient)[idx]];
			return (commitment?.multi_dimensional_damping?.global_damping_factor || 1.0) < 1.0;
		})).length;
	
	console.log(
		`[DAMPENING] Processed ${Object.keys(activeNeedsByRecipient).length} recipients, ` +
		`${recipientsWithDampening} with dampening applied`
	);
	
	// Calculate two-tier mutual recognition distribution manually
	// We already have mutualRecognition computed, so build distribution directly
	const tier1Shares: Record<string, number> = {};
	const tier2Shares: Record<string, number> = {};
	const allShares: Record<string, number> = {};
	
	let totalTier1Recognition = 0;
	let totalTier2Recognition = 0;
	
	// Classify recipients into tiers based on mutual recognition
	// Include self for self-allocation (time-shifting)
	for (const [recipientId, mr] of Object.entries(mutualRecognition)) {
		if (mr > 0) {
			// Tier 1: Mutual recognition (including self)
			tier1Shares[recipientId] = mr;
			totalTier1Recognition += mr;
		} else if (recipientId !== myPubKey) {
			// Tier 2: Check if I recognize them (one-way, excluding self)
			const myRecOfThem = myRecognition[recipientId] || 0;
			if (myRecOfThem > 0) {
				tier2Shares[recipientId] = myRecOfThem;
				totalTier2Recognition += myRecOfThem;
			}
		}
	}
	
	// Normalize tier 1 shares
	if (totalTier1Recognition > 0) {
		for (const recipientId in tier1Shares) {
			const normalized = tier1Shares[recipientId] / totalTier1Recognition;
			tier1Shares[recipientId] = normalized;
			allShares[recipientId] = normalized;
		}
	}
	
	// Normalize tier 2 shares
	if (totalTier2Recognition > 0) {
		for (const recipientId in tier2Shares) {
			const normalized = tier2Shares[recipientId] / totalTier2Recognition;
			tier2Shares[recipientId] = normalized;
			// Only add to allShares if not already in tier1
			if (!(recipientId in allShares)) {
				allShares[recipientId] = normalized;
			}
		}
	}
	
	const distribution: DistributionResult = {
		shares: allShares,
		method: 'two-tier',
		tiers: {
			tier1: tier1Shares,
			tier2: tier2Shares
		},
		metadata: {
			timestamp: Date.now()
		}
	};
	
	// Delegate to generic allocation engine with active (damped) needs
	const result = allocateWithDistribution(
		myPubKey,
		myCapacitySlots,
		distribution,
		allCommitments,
		needsIndex,
		recipientFilters,
		activeNeedsByRecipient  // ✅ Pass damped needs to allocation engine
	);
	
	// Compute convergence metrics
	const convergence = computeConvergenceSummary(
		currentState,
		previousState,
		iterationStartTime
	);
	
	return {
		allocations: result.allocations,
		slotDenominators: result.slotDenominators,
		totalsByTypeAndRecipient: result.totalsByTypeAndRecipient,
		convergence
	};
}

// OLD IMPLEMENTATION (PRESERVED FOR REFERENCE - CAN BE REMOVED)
/*
export function computeAllocationsOld(
	myPubKey: string,
	myCapacitySlots: AvailabilitySlot[],
	myRecognition: GlobalRecognitionWeights,
	mutualRecognition: Record<string, number>,
	allCommitments: Record<string, Commitment>,
	currentState: SystemStateSnapshot,
	previousState: SystemStateSnapshot | null,
	needsIndex?: SpaceTimeIndex,
	recipientFilters?: Map<string, ComplianceFilter>
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
		
		// Find compatible recipients (using spatial/temporal index if provided)
		const compatibleRecipients = findCompatibleRecipients(capacitySlot, allCommitments, myPubKey, needsIndex);
		
		if (compatibleRecipients.size === 0) continue;
		
		console.log(`[TIER-1] Starting allocation for capacity slot ${capacitySlot.id.slice(0,20)}...`);
		console.log(`[TIER-1] Compatible recipients: ${compatibleRecipients.size}`);
		
		// ────────────────────────────────────────────────────────────
		// TIER 1: MUTUAL RECOGNITION (PROPORTIONAL MULTI-PASS)
		// ────────────────────────────────────────────────────────────
		
		const CAPACITY_EPSILON = 0.0001;
		let capacityUsedInTier1 = 0;
		let tier1Denominator = 0;
		
		// Build initial list of mutually-recognized recipients with remainingNeed tracking
		const mutualEligibleRecipients: Array<{
			pubKey: string;
			totalNeed: number;
			remainingNeed: number;
			mutualRecShare: number;
			activeNeed: number;
			needSlots: NeedSlot[];
		}> = [];
		
		let totalMutualRecognition = 0;
		
		// Calculate mutual recognition shares
		for (const [recipientPub, needSlots] of compatibleRecipients.entries()) {
			const mutualRec = mutualRecognition[recipientPub] || 0;
			console.log(`[TIER-1]   Recipient ${recipientPub.slice(0,20)}...: MR = ${mutualRec}`);
			if (mutualRec <= 0) continue;
			
			totalMutualRecognition += mutualRec;
		}
		
		console.log(`[TIER-1] Total mutual recognition: ${totalMutualRecognition}`);
		
		if (totalMutualRecognition > 0) {
			console.log(`[TIER-1] Building eligible recipients list...`);
			// Build initial recipient list with needs and recognition shares
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
				
				console.log(`[TIER-1]     ${recipientPub.slice(0,20)}...: need=${totalNeed}, share=${mutualRecShare.toFixed(3)}, damping=${dampingFactor}, activeNeed=${activeNeed}`);
				
				mutualEligibleRecipients.push({
					pubKey: recipientPub,
					totalNeed,
					remainingNeed: totalNeed, // Track how much they still need
					mutualRecShare,
					activeNeed,
					needSlots
				});
			}
			
			console.log(`[TIER-1] Eligible recipients: ${mutualEligibleRecipients.length}`);
			
			// ═══════════════════════════════════════════════════════════════
			// MULTI-PASS PROPORTIONAL ALLOCATION
			// ═══════════════════════════════════════════════════════════════
			
			let unsatisfiedRecipients = [...mutualEligibleRecipients];
			let remainingCapacity = providersAvailableCapacity;
			let passCount = 0;
			const maxPasses = 10;
			
			console.log(`[TIER-1] Starting multi-pass allocation: capacity=${remainingCapacity}, recipients=${unsatisfiedRecipients.length}`);
			
			while (remainingCapacity > CAPACITY_EPSILON && unsatisfiedRecipients.length > 0 && passCount < maxPasses) {
				passCount++;
				console.log(`[TIER-1] Pass ${passCount}: capacity=${remainingCapacity.toFixed(2)}, unsatisfied=${unsatisfiedRecipients.length}`);
				
				// PHASE 1: Calculate denominator with only unsatisfied recipients
				// FIX: Use recognition ONLY for proportional split (need only caps, doesn't affect proportion)
				let denominator = unsatisfiedRecipients.reduce(
					(sum, r) => sum + r.mutualRecShare,
					0
				);
				
				console.log(`[TIER-1]   Denominator: ${denominator.toFixed(4)}`);
				
				if (denominator < CAPACITY_EPSILON) break;
				
				// Safety check for tiny denominators
				const MIN_RELATIVE_DENOMINATOR = 0.001;
				const minSafeDenominator = remainingCapacity * MIN_RELATIVE_DENOMINATOR;
				if (denominator < minSafeDenominator) {
					denominator = minSafeDenominator;
				}
				
				// Store final denominator for tracking
				if (passCount === 1) {
					tier1Denominator = denominator;
				}
				
			// PHASE 2: Calculate ALL proportional allocations BEFORE capping
			// KEY: Everyone calculated against SAME denominator = true proportionality
			// Recognition determines proportion, activeNeed only used for capping
			const proportionalAllocations = unsatisfiedRecipients.map(recipient => {
				const rawAllocation = remainingCapacity * 
					recipient.mutualRecShare / denominator;
				
				// Apply compliance filter if present (blocked, capped, unlimited)
				let filterLimit = Infinity;
				if (recipientFilters && recipientFilters.has(recipient.pubKey)) {
					const filter = recipientFilters.get(recipient.pubKey)!;
					const currentTotal = totalsByTypeAndRecipient[typeId]?.[recipient.pubKey] || 0;
					const recipientCommitment = allCommitments[recipient.pubKey];
					
					filterLimit = evaluateComplianceFilter(filter, {
						pubKey: recipient.pubKey,
						currentTotal,
						proposedAmount: rawAllocation,
						commitment: recipientCommitment,
						mutualRecognition: mutualRecognition[recipient.pubKey],
						attributes: (recipientCommitment as any)?.attributes || {}
					});
					
					// filterLimit is the TOTAL allowed, so subtract what's already allocated
					filterLimit = Math.max(0, filterLimit - currentTotal);
					
					console.log(`[TIER-1]     ${recipient.pubKey.slice(0,20)}...: raw=${rawAllocation.toFixed(2)}, need cap=${recipient.remainingNeed}, filter cap=${filterLimit === Infinity ? 'unlimited' : filterLimit.toFixed(2)}`);
				} else {
					console.log(`[TIER-1]     ${recipient.pubKey.slice(0,20)}...: raw=${rawAllocation.toFixed(2)}, cap at ${recipient.remainingNeed}`);
				}
				
				return {
					recipient,
					rawAllocation,
					cappedAllocation: Math.min(rawAllocation, recipient.remainingNeed, filterLimit)
				};
			});
				
				console.log(`[TIER-1]   Calculated ${proportionalAllocations.length} allocations`);
				
				// PHASE 3: Apply allocations and track satisfaction
				let capacityUsedThisPass = 0;
				const nowSatisfied: typeof unsatisfiedRecipients = [];
				const tier1Remainders = new Map<string, number>();
				
				for (const { recipient, rawAllocation, cappedAllocation } of proportionalAllocations) {
					console.log(`[TIER-1]     Processing ${recipient.pubKey.slice(0,20)}...: capped=${cappedAllocation.toFixed(2)}`);
					if (cappedAllocation <= CAPACITY_EPSILON) {
						console.log(`[TIER-1]       SKIP: too small (${cappedAllocation} <= ${CAPACITY_EPSILON})`);
						continue;
					}
					
					// Calculate share percentage for divisibility checks
					const recipientSharePercentage = cappedAllocation / providersAvailableCapacity;
					
					// Apply divisibility constraints
					const constrainedAllocation = applyDivisibilityConstraints(
						cappedAllocation,
						recipientSharePercentage,
						capacitySlot
					);
					
					console.log(`[TIER-1]       After divisibility: constrained=${constrainedAllocation.toFixed(2)} (from ${cappedAllocation.toFixed(2)})`);
					
					// Track remainder for redistribution
					const maxNatural = capacitySlot.max_natural_div || 1;
					const remainder = (cappedAllocation - constrainedAllocation) / maxNatural;
					if (remainder > 0) {
						tier1Remainders.set(recipient.pubKey, remainder);
					}
					
					// Check if allocation meets minimum threshold
					const meetsMin = meetsMinimumAllocation(constrainedAllocation, capacitySlot);
					console.log(`[TIER-1]       Meets minimum: ${meetsMin}`);
					if (!meetsMin) {
						console.log(`[TIER-1]       SKIP: doesn't meet minimum allocation threshold`);
						continue;
					}
					
					// Proportional distribution across need slots
					const totalCompatibleNeed = recipient.needSlots.reduce((sum, slot) => sum + slot.quantity, 0);
					let actuallyAllocated = 0;
					
					console.log(`[TIER-1]       Distributing ${constrainedAllocation.toFixed(2)} across ${recipient.needSlots.length} need slots`);
					
					for (const needSlot of recipient.needSlots) {
						const proportion = needSlot.quantity / totalCompatibleNeed;
						let slotAllocation = Math.min(
							needSlot.quantity,
							constrainedAllocation * proportion
						);
						
						console.log(`[TIER-1]         Slot ${needSlot.id.slice(0,20)}...: before rounding=${slotAllocation.toFixed(2)}`);
						
						// Apply natural unit rounding
						slotAllocation = Math.floor(slotAllocation / maxNatural) * maxNatural;
						
						console.log(`[TIER-1]         After rounding (maxNatural=${maxNatural}): ${slotAllocation.toFixed(2)}`);
						
						if (slotAllocation > 0) {
							console.log(`[TIER-1]         ✅ ALLOCATED ${slotAllocation.toFixed(2)} to slot!`);
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
						} else {
							console.log(`[TIER-1]         ❌ SKIP: slotAllocation is 0 after rounding`);
						}
					}
					
					console.log(`[TIER-1]       Total actually allocated: ${actuallyAllocated.toFixed(2)}`);
					
					if (actuallyAllocated > 0) {
						capacityUsedThisPass += actuallyAllocated;
						recipient.remainingNeed -= actuallyAllocated;
						
						// Track totals
						if (!totalsByTypeAndRecipient[typeId][recipient.pubKey]) {
							totalsByTypeAndRecipient[typeId][recipient.pubKey] = 0;
						}
						totalsByTypeAndRecipient[typeId][recipient.pubKey] += actuallyAllocated;
						
						// Satisfied if capped below raw allocation OR need fully met
						if (cappedAllocation < rawAllocation - CAPACITY_EPSILON || 
							recipient.remainingNeed <= CAPACITY_EPSILON) {
							nowSatisfied.push(recipient);
						}
					}
				}
				
				// Remainder redistribution within this pass
				if (tier1Remainders.size > 0) {
					const maxNatural = capacitySlot.max_natural_div || 1;
					const beforeRedistribution = capacityUsedThisPass;
					capacityUsedThisPass = redistributeRemainders(
						allocations,
						tier1Remainders,
						capacityUsedInTier1 + capacityUsedThisPass,
						providersAvailableCapacity,
						maxNatural,
						capacitySlot
					) - capacityUsedInTier1;
					
					// Update totals if redistribution added capacity
					if (capacityUsedThisPass > beforeRedistribution) {
						for (const [recipientPub, _] of tier1Remainders) {
							const recipientAllocations = allocations.filter(
								a => a.recipient_pubkey === recipientPub && a.tier === 'mutual'
							);
							const recipientTotal = recipientAllocations.reduce((sum, a) => sum + a.quantity, 0);
							totalsByTypeAndRecipient[typeId][recipientPub] = recipientTotal;
						}
					}
				}
				
				capacityUsedInTier1 += capacityUsedThisPass;
				remainingCapacity -= capacityUsedThisPass;
				
				// PHASE 4: Remove satisfied recipients for next pass
				unsatisfiedRecipients = unsatisfiedRecipients.filter(r => !nowSatisfied.includes(r));
				
				// Exit if no progress made
				if (nowSatisfied.length === 0 && capacityUsedThisPass < CAPACITY_EPSILON) break;
			}
		}
		
		// ────────────────────────────────────────────────────────────
		// TIER 2: NON-MUTUAL RECOGNITION (PROPORTIONAL MULTI-PASS)
		// ────────────────────────────────────────────────────────────
		
		let remainingCapacityAfterTier1 = providersAvailableCapacity - capacityUsedInTier1;
		let tier2Denominator = 0;
		let capacityUsedInTier2 = 0;
		
		if (remainingCapacityAfterTier1 > CAPACITY_EPSILON) {
			// Build initial list of non-mutually-recognized recipients
			const nonMutualEligibleRecipients: Array<{
				pubKey: string;
				totalNeed: number;
				remainingNeed: number;
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
				// Build initial recipient list
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
						totalNeed,
						remainingNeed: totalNeed,
						recognitionShare,
						activeNeed,
						needSlots
					});
				}
				
				// ═══════════════════════════════════════════════════════════════
				// MULTI-PASS PROPORTIONAL ALLOCATION (TIER 2)
				// ═══════════════════════════════════════════════════════════════
				
				let unsatisfiedRecipients = [...nonMutualEligibleRecipients];
				let remainingCapacity = remainingCapacityAfterTier1;
				let passCount = 0;
				const maxPasses = 10;
				
				while (remainingCapacity > CAPACITY_EPSILON && unsatisfiedRecipients.length > 0 && passCount < maxPasses) {
					passCount++;
					
					// PHASE 1: Calculate denominator
					// FIX: Use recognition ONLY for proportional split (need only caps, doesn't affect proportion)
					let denominator = unsatisfiedRecipients.reduce(
						(sum, r) => sum + r.recognitionShare,
						0
					);
					
					if (denominator < CAPACITY_EPSILON) break;
					
					// Safety check for tiny denominators
					const MIN_RELATIVE_DENOMINATOR = 0.001;
					const minSafeDenominator = remainingCapacity * MIN_RELATIVE_DENOMINATOR;
					if (denominator < minSafeDenominator) {
						denominator = minSafeDenominator;
					}
					
					// Store final denominator for tracking
					if (passCount === 1) {
						tier2Denominator = denominator;
					}
					
				// PHASE 2: Calculate ALL proportional allocations BEFORE capping
				// Recognition determines proportion, activeNeed only used for capping
				const proportionalAllocations = unsatisfiedRecipients.map(recipient => {
					const rawAllocation = remainingCapacity * 
						recipient.recognitionShare / denominator;
					
					// Apply compliance filter if present (blocked, capped, unlimited)
					let filterLimit = Infinity;
					if (recipientFilters && recipientFilters.has(recipient.pubKey)) {
						const filter = recipientFilters.get(recipient.pubKey)!;
						const currentTotal = totalsByTypeAndRecipient[typeId]?.[recipient.pubKey] || 0;
						const recipientCommitment = allCommitments[recipient.pubKey];
						
						filterLimit = evaluateComplianceFilter(filter, {
							pubKey: recipient.pubKey,
							currentTotal,
							proposedAmount: rawAllocation,
							commitment: recipientCommitment,
							mutualRecognition: 0, // No mutual recognition in Tier 2
							attributes: (recipientCommitment as any)?.attributes || {}
						});
						
						// filterLimit is the TOTAL allowed, so subtract what's already allocated
						filterLimit = Math.max(0, filterLimit - currentTotal);
					}
					
					return {
						recipient,
						rawAllocation,
						cappedAllocation: Math.min(rawAllocation, recipient.remainingNeed, filterLimit)
					};
				});
					
					// PHASE 3: Apply allocations and track satisfaction
					let capacityUsedThisPass = 0;
					const nowSatisfied: typeof unsatisfiedRecipients = [];
					const tier2Remainders = new Map<string, number>();
					
					for (const { recipient, rawAllocation, cappedAllocation } of proportionalAllocations) {
						if (cappedAllocation <= CAPACITY_EPSILON) continue;
						
						// Calculate share percentage for divisibility checks
						const recipientSharePercentage = cappedAllocation / providersAvailableCapacity;
						
						// Apply divisibility constraints
						const constrainedAllocation = applyDivisibilityConstraints(
							cappedAllocation,
							recipientSharePercentage,
							capacitySlot
						);
						
						// Track remainder for redistribution
						const maxNatural = capacitySlot.max_natural_div || 1;
						const remainder = (cappedAllocation - constrainedAllocation) / maxNatural;
						if (remainder > 0) {
							tier2Remainders.set(recipient.pubKey, remainder);
						}
						
						// Check if allocation meets minimum threshold
						if (!meetsMinimumAllocation(constrainedAllocation, capacitySlot)) {
							continue;
						}
						
						// Proportional distribution across need slots
						const totalCompatibleNeed = recipient.needSlots.reduce((sum, slot) => sum + slot.quantity, 0);
						let actuallyAllocated = 0;
						
						for (const needSlot of recipient.needSlots) {
							const proportion = needSlot.quantity / totalCompatibleNeed;
							let slotAllocation = Math.min(
								needSlot.quantity,
								constrainedAllocation * proportion
							);
							
							// Apply natural unit rounding
							slotAllocation = Math.floor(slotAllocation / maxNatural) * maxNatural;
							
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
						
						if (actuallyAllocated > 0) {
							capacityUsedThisPass += actuallyAllocated;
							recipient.remainingNeed -= actuallyAllocated;
							
							// Track totals
							if (!totalsByTypeAndRecipient[typeId][recipient.pubKey]) {
								totalsByTypeAndRecipient[typeId][recipient.pubKey] = 0;
							}
							totalsByTypeAndRecipient[typeId][recipient.pubKey] += actuallyAllocated;
							
							// Satisfied if capped below raw allocation OR need fully met
							if (cappedAllocation < rawAllocation - CAPACITY_EPSILON || 
								recipient.remainingNeed <= CAPACITY_EPSILON) {
								nowSatisfied.push(recipient);
							}
						}
					}
					
					// Remainder redistribution within this pass
					if (tier2Remainders.size > 0) {
						const maxNatural = capacitySlot.max_natural_div || 1;
						const beforeRedistribution = capacityUsedThisPass;
						capacityUsedThisPass = redistributeRemainders(
							allocations,
							tier2Remainders,
							capacityUsedInTier1 + capacityUsedInTier2 + capacityUsedThisPass,
							providersAvailableCapacity,
							maxNatural,
							capacitySlot
						) - capacityUsedInTier1 - capacityUsedInTier2;
						
						// Update totals if redistribution added capacity
						if (capacityUsedThisPass > beforeRedistribution) {
							for (const [recipientPub, _] of tier2Remainders) {
								const recipientAllocations = allocations.filter(
									a => a.recipient_pubkey === recipientPub && a.tier === 'non-mutual'
								);
								const recipientTotal = recipientAllocations.reduce((sum, a) => sum + a.quantity, 0);
								totalsByTypeAndRecipient[typeId][recipientPub] = recipientTotal;
							}
						}
					}
					
					capacityUsedInTier2 += capacityUsedThisPass;
					remainingCapacity -= capacityUsedThisPass;
					
					// PHASE 4: Remove satisfied recipients for next pass
					unsatisfiedRecipients = unsatisfiedRecipients.filter(r => !nowSatisfied.includes(r));
					
					// Exit if no progress made
					if (nowSatisfied.length === 0 && capacityUsedThisPass < CAPACITY_EPSILON) break;
				}
			}
		}
		
		// ✅ CAPACITY PROTECTION: Final safety check for this slot
		const totalCapacityUsed = capacityUsedInTier1 + capacityUsedInTier2;
		if (totalCapacityUsed > providersAvailableCapacity + CAPACITY_EPSILON) {
			console.error(
				`[ALLOCATION-PROTECTION-ERROR] Over-allocated! Capacity: ${providersAvailableCapacity.toFixed(2)}, ` +
				`Used: ${totalCapacityUsed.toFixed(2)}, ` +
				`Excess: ${(totalCapacityUsed - providersAvailableCapacity).toFixed(2)} ` +
				`for type ${typeId}, slot ${capacitySlot.id.slice(0, 8)}`
			);
			throw new Error(`Over-allocation detected: ${totalCapacityUsed.toFixed(2)} > ${providersAvailableCapacity.toFixed(2)}`);
		} else if (totalCapacityUsed > providersAvailableCapacity - CAPACITY_EPSILON) {
			// Log successful full allocation
			console.log(
				`[ALLOCATION-PROTECTION] ✅ Fully allocated capacity for type ${typeId}: ` +
				`${totalCapacityUsed.toFixed(2)} / ${providersAvailableCapacity.toFixed(2)} ` +
				`(${((totalCapacityUsed / providersAvailableCapacity) * 100).toFixed(1)}%)`
			);
		} else {
			console.log(
				`[ALLOCATION-PROTECTION] ⚠️ Partial allocation for type ${typeId}: ` +
				`${totalCapacityUsed.toFixed(2)} / ${providersAvailableCapacity.toFixed(2)} ` +
				`(${((totalCapacityUsed / providersAvailableCapacity) * 100).toFixed(1)}%) - ` +
				`${(providersAvailableCapacity - totalCapacityUsed).toFixed(2)} unused`
			);
		}
		
		// Store denominators for this slot
		slotDenominators[capacitySlot.id] = {
			mutual: tier1Denominator,
			nonMutual: tier2Denominator,
			need_type_id: typeId
		};
	}
	
	// (old implementation removed - see above)
}
*/

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
