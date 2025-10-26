/**
 * Mutual-Priority Allocation Algorithm v5 - Multi-Dimensional Framework
 * 
 * Implements the complete multi-dimensional framework from multi-grammar.md:
 * ✅ Multi-dimensional need vectors N⃗_i(t) = [N_i^1(t), ..., N_i^m(t)]^T (D3')
 * ✅ Multi-dimensional capacity vectors C⃗_j(t) = [C_j^1(t), ..., C_j^m(t)]^T (D4')
 * ✅ Type-specific mutual recognition MR^k(A, B) (D1')
 * ✅ Per-type recognition weight normalization (D2')
 * ✅ Type-specific two-tier allocation (E1'-E11')
 * ✅ Per-type adaptive damping α_k(t) (E12'-E15')
 * ✅ Multi-dimensional contraction mapping T: ℝ^{n×m} → ℝ^{n×m} (E17'-E18')
 * ✅ Frobenius norm convergence tracking (Theorem 1', 3)
 * ✅ Per-type heaven condition (E41')
 * ✅ Multi-dimensional freedom metric (E45')
 * ✅ Provider specialization by need type
 * ✅ ITC causal consistency (E39-E40)
 * ✅ Slot-native space-time extension with type matching (E27'-E30')
 * 
 * v5 vs v2 differences:
 * - Vector needs and capacities instead of scalar
 * - Independent allocation per need type k
 * - Frobenius norm ||N⃗⃗(t)||_F for system convergence
 * - Per-type convergence metrics and damping
 * - Type matching in slot compatibility
 * - Provider specialization support
 */

import { derived, get } from 'svelte/store';
import type { Readable, Writable } from 'svelte/store';

// Import v4 schemas (multi-dimensional)
import type {
	Commitment,
	TwoTierAllocationState,
	AvailabilitySlot,
	NeedSlot,
	SlotAllocationRecord,
	ITCStamp,
	PerTypeDampingHistoryEntry,
	MultiDimensionalNeedState,
	MultiDimensionalCapacityState,
	PerTypeNeedState,
	PerTypeCapacityState,
	SystemState,
	ConvergenceMetrics,
	PerTypeConvergenceMetrics,
	PerTypeAllocationTotals,
	MultiDimensionalRecognition,
	MultiDimensionalDamping,
	NeedType
} from './schemas';


// Import ITC functions
import { 
	seed as itcSeed,
	event as itcEvent,
	join as itcJoin,
	leq as itcLeq,
	equals as itcEquals,
	toString as itcToString
} from '../utils/itc';

// Import slot matching utilities (extended for type matching)
import { 
	slotsCompatible, 
	passesSlotFilters, 
	getTimeBucketKey,
	getLocationBucketKey,
	type FilterContext 
} from './match.svelte';

// ═══════════════════════════════════════════════════════════════════
// V4 EXTENSIONS: MULTI-DIMENSIONAL TYPES
// ═══════════════════════════════════════════════════════════════════

/**
 * Allocation Operator Result (Multi-Dimensional)
 */
export interface AllocationOperatorResult {
	allocations: TwoTierAllocationState;
	updatedNeedVector: Record<string, MultiDimensionalNeedState>;   // N⃗⃗(t+1)
	convergenceMetrics: ConvergenceMetrics;
}

// ═══════════════════════════════════════════════════════════════════
// PLACEHOLDER: Store imports (to be implemented)
// ═══════════════════════════════════════════════════════════════════

import { writable } from 'svelte/store';

const myCommitmentStore = writable<Commitment | null>(null);
const myAllocationStateStore = writable<TwoTierAllocationState | null>(null);
const myRecognitionWeightsStore = writable<MultiDimensionalRecognition | null>(null);
const networkCommitments = new Map<string, Commitment>();
const networkRecognitionWeights = new Map<string, MultiDimensionalRecognition>();

function getNetworkCommitmentsRecord(): Record<string, Commitment> {
	return Object.fromEntries(networkCommitments);
}

function getNetworkRecognitionWeightsRecord(): Record<string, MultiDimensionalRecognition> {
	return Object.fromEntries(networkRecognitionWeights);
}

const holsterUserPub = writable<string | null>(null);

// ═══════════════════════════════════════════════════════════════════
// CONSTANTS (from multi-grammar.md)
// ═══════════════════════════════════════════════════════════════════

export const STALE_THRESHOLD_MS = 60000;           // 60 seconds
export const CONVERGENCE_EPSILON = 0.001;          // ε for ||N⃗⃗(t) - N⃗⃗*|| < ε
export const DENOMINATOR_FLOOR = 0.0001;           // ε for Lipschitz continuity
export const DAMPING_HISTORY_WINDOW_MS = 30000;    // 30 seconds
export const DAMPING_HISTORY_MAX_COUNT = 3;        // Last 3 entries

// Damping factors (E14')
export const DAMPING_OSCILLATING = 0.5;            // α_k when oscillating
export const DAMPING_SMOOTH = 1.0;                 // α_k when smooth convergence
export const DAMPING_MODERATE = 0.8;               // α_k otherwise

// ═══════════════════════════════════════════════════════════════════
// ITC STATE (Causal Consistency)
// ═══════════════════════════════════════════════════════════════════

let myITCStamp: ITCStamp = itcSeed();

export function getMyITCStamp(): ITCStamp {
	return myITCStamp;
}

export function incrementMyITCStamp(): void {
	myITCStamp = itcEvent(myITCStamp);
	console.log(`[ITC-V4] Incremented stamp: ${itcToString(myITCStamp)}`);
}

export function mergeITCStampFromPeer(peerStamp: ITCStamp): void {
	const oldStamp = myITCStamp;
	myITCStamp = itcJoin(myITCStamp, peerStamp);
	
	if (!itcEquals(oldStamp, myITCStamp)) {
		console.log(`[ITC-V4] Merged stamp: ${itcToString(myITCStamp)}`);
	}
}

export function isPeerUpdateStale(peerStamp: ITCStamp): boolean {
	return itcLeq(peerStamp, myITCStamp) && itcEquals(peerStamp, myITCStamp);
}

export function getCausallyConsistentCommitments(): Record<string, Commitment> {
	const allCommitments = getNetworkCommitmentsRecord();
	const snapshot: Record<string, Commitment> = {};
	
	for (const [pubKey, commitment] of Object.entries(allCommitments)) {
		if (!commitment.itcStamp || itcLeq(commitment.itcStamp, myITCStamp)) {
			snapshot[pubKey] = commitment;
		}
	}
	
	return snapshot;
}

// ═══════════════════════════════════════════════════════════════════
// SYSTEM STATE MANAGEMENT (N⃗⃗(t) and C⃗⃗(t))
// ═══════════════════════════════════════════════════════════════════

/**
 * Current system state vector (multi-dimensional)
 */
let currentSystemState: SystemState = {
	needVector: {},
	capacityVector: {},
	timestamp: Date.now(),
	iteration: 0,
	itcStamp: itcSeed()
};

/**
 * Previous system state (for convergence tracking)
 */
let previousSystemState: SystemState | null = null;

/**
 * Get current system state
 */
export function getCurrentSystemState(): SystemState {
	return currentSystemState;
}

/**
 * Initialize or update multi-dimensional need state from commitment (D3')
 * N⃗_i(t) = [N_i^1(t), N_i^2(t), ..., N_i^m(t)]^T
 */
export function initializeMultiDimensionalNeedState(
	pubKey: string,
	commitment: Commitment
): MultiDimensionalNeedState {
	const needsByType: Record<string, PerTypeNeedState> = {};
	
	// Group need slots by type
	if (commitment.need_slots) {
		for (const slot of commitment.need_slots) {
			const typeId = slot.need_type_id; // Required field
			
			if (!needsByType[typeId]) {
				needsByType[typeId] = {
					need_type_id: typeId,
					residualNeed: 0,
					maxNeed: 0,
					lastAllocationReceived: 0
				};
			}
			
			needsByType[typeId].maxNeed += slot.quantity;
			
			// Preserve residual if exists
			const existing = currentSystemState.needVector[pubKey]?.needsByType[typeId];
			if (existing) {
				needsByType[typeId].residualNeed = existing.residualNeed;
			} else {
				needsByType[typeId].residualNeed = needsByType[typeId].maxNeed;
			}
		}
	}
	
	// Compute total norms
	let totalResidualNeed = 0;
	let totalMaxNeed = 0;
	for (const state of Object.values(needsByType)) {
		totalResidualNeed += state.residualNeed ** 2;
		totalMaxNeed += state.maxNeed ** 2;
	}
	
	return {
		pubKey,
		needsByType,
		timestamp: Date.now(),
		totalResidualNeed: Math.sqrt(totalResidualNeed),
		totalMaxNeed: Math.sqrt(totalMaxNeed)
	};
}

/**
 * Initialize or update multi-dimensional capacity state from commitment (D4')
 * C⃗_j(t) = [C_j^1(t), C_j^2(t), ..., C_j^m(t)]^T
 */
export function initializeMultiDimensionalCapacityState(
	pubKey: string,
	commitment: Commitment
): MultiDimensionalCapacityState {
	const capacitiesByType: Record<string, PerTypeCapacityState> = {};
	
	// Group capacity slots by type
	if (commitment.capacity_slots) {
		for (const slot of commitment.capacity_slots) {
			const typeId = slot.need_type_id; // Required field
			
			if (!capacitiesByType[typeId]) {
				capacitiesByType[typeId] = {
					need_type_id: typeId,
					availableCapacity: 0,
					maxCapacity: 0,
					lastAllocationGiven: 0
				};
			}
			
			capacitiesByType[typeId].maxCapacity += slot.quantity;
			
			// Preserve available if exists
			const existing = currentSystemState.capacityVector[pubKey]?.capacitiesByType[typeId];
			if (existing) {
				capacitiesByType[typeId].availableCapacity = existing.availableCapacity;
			} else {
				capacitiesByType[typeId].availableCapacity = capacitiesByType[typeId].maxCapacity;
			}
		}
	}
	
	// Compute total norms
	let totalAvailableCapacity = 0;
	let totalMaxCapacity = 0;
	for (const state of Object.values(capacitiesByType)) {
		totalAvailableCapacity += state.availableCapacity ** 2;
		totalMaxCapacity += state.maxCapacity ** 2;
	}
	
	return {
		pubKey,
		capacitiesByType,
		timestamp: Date.now(),
		totalAvailableCapacity: Math.sqrt(totalAvailableCapacity),
		totalMaxCapacity: Math.sqrt(totalMaxCapacity)
	};
}

/**
 * Update system state from network commitments
 * Builds N⃗⃗(t) and C⃗⃗(t) from current commitments
 */
export function updateSystemStateFromNetwork(): void {
	const commitments = getCausallyConsistentCommitments();
	
	const newNeedVector: Record<string, MultiDimensionalNeedState> = {};
	const newCapacityVector: Record<string, MultiDimensionalCapacityState> = {};
	
	for (const [pubKey, commitment] of Object.entries(commitments)) {
		// Update need state
		const needState = initializeMultiDimensionalNeedState(pubKey, commitment);
		if (Object.keys(needState.needsByType).length > 0) {
			newNeedVector[pubKey] = needState;
		}
		
		// Update capacity state
		const capacityState = initializeMultiDimensionalCapacityState(pubKey, commitment);
		if (Object.keys(capacityState.capacitiesByType).length > 0) {
			newCapacityVector[pubKey] = capacityState;
		}
	}
	
	// Store previous state for convergence tracking
	previousSystemState = { ...currentSystemState };
	
	// Update current state
	currentSystemState = {
		needVector: newNeedVector,
		capacityVector: newCapacityVector,
		timestamp: Date.now(),
		iteration: currentSystemState.iteration + 1,
		itcStamp: getMyITCStamp()
	};
	
	const needCount = Object.keys(newNeedVector).length;
	const typeCount = Object.values(newNeedVector).reduce((max, state) => 
		Math.max(max, Object.keys(state.needsByType).length), 0
	);
	
	console.log(`[STATE-V4] Updated system state: ${needCount} participants, ${typeCount} need types`);
}

// ═══════════════════════════════════════════════════════════════════
// RECOGNITION WEIGHT NORMALIZATION (D2')
// ═══════════════════════════════════════════════════════════════════

/**
 * Normalize type-specific recognition weights to sum to 1.0
 * Enforces: ∀ participant A, type k: Σ_i R_A^k(i) = 1.0
 */
export function normalizeTypeSpecificRecognitionWeights(
	weightsByType: MultiDimensionalRecognition
): MultiDimensionalRecognition {
	const normalized: MultiDimensionalRecognition = {};
	
	for (const [typeId, weights] of Object.entries(weightsByType)) {
		normalized[typeId] = normalizeRecognitionWeights(weights);
	}
	
	return normalized;
}

/**
 * Normalize recognition weights to sum to 1.0 (per type)
 */
export function normalizeRecognitionWeights(weights: Record<string, number>): Record<string, number> {
	const entries = Object.entries(weights);
	
	if (entries.length === 0) {
		return {};
	}
	
	// Calculate sum
	const sum = entries.reduce((acc, [_, weight]) => acc + weight, 0);
	
	// If sum is zero or very small, return equal weights
	if (sum < 0.0001) {
		const equalWeight = 1.0 / entries.length;
		return Object.fromEntries(entries.map(([key, _]) => [key, equalWeight]));
	}
	
	// Normalize to sum to 1.0
	const normalized: Record<string, number> = {};
	for (const [key, weight] of entries) {
		normalized[key] = weight / sum;
	}
	
	return normalized;
}

/**
 * Validate that recognition weights sum to 1.0 (within epsilon)
 */
export function validateRecognitionWeights(weights: Record<string, number>): boolean {
	const sum = Object.values(weights).reduce((a, b) => a + b, 0);
	const isValid = Math.abs(sum - 1.0) < CONVERGENCE_EPSILON;
	
	if (!isValid) {
		console.warn(`[VALIDATE-V4] Recognition weights sum to ${sum.toFixed(4)}, not 1.0`);
	}
	
	return isValid;
}

// ═══════════════════════════════════════════════════════════════════
// MUTUAL RECOGNITION COMPUTATION (D1', E19')
// ═══════════════════════════════════════════════════════════════════

/**
 * Compute type-specific bilateral Mutual Recognition (D1')
 * MR^k(A, B) = min(R_A^k(B), R_B^k(A))
 * 
 * Symmetric property (E19'): MR^k(A, B) = MR^k(B, A)
 */
export function computeTypeSpecificMutualRecognition(
	myPub: string,
	theirPub: string,
	typeId: string,
	myWeightsByType: MultiDimensionalRecognition,
	theirWeightsByType: MultiDimensionalRecognition
): number {
	const myWeights = myWeightsByType[typeId] || {};
	const theirWeights = theirWeightsByType[typeId] || {};
	
	const myRecOfThem = myWeights[theirPub] || 0;
	const theirRecOfMe = theirWeights[myPub] || 0;
	
	return Math.min(myRecOfThem, theirRecOfMe);
}

/**
 * Compute all type-specific MR values for a given participant
 */
export function computeAllTypeSpecificMutualRecognition(
	myPub: string,
	myWeightsByType: MultiDimensionalRecognition,
	networkWeightsByType: Record<string, MultiDimensionalRecognition>
): MultiDimensionalRecognition {
	const mrValuesByType: MultiDimensionalRecognition = {};
	
	// For each need type
	const allTypes = new Set<string>();
	Object.keys(myWeightsByType).forEach(t => allTypes.add(t));
	Object.values(networkWeightsByType).forEach(weights => 
		Object.keys(weights).forEach(t => allTypes.add(t))
	);
	
	for (const typeId of allTypes) {
		const mrValues: Record<string, number> = {};
		const myWeights = myWeightsByType[typeId] || {};
		
		// For everyone I recognize for this type
		for (const theirPub in myWeights) {
			if (myWeights[theirPub] > 0) {
				const theirWeightsByType = networkWeightsByType[theirPub] || {};
				mrValues[theirPub] = computeTypeSpecificMutualRecognition(
					myPub, theirPub, typeId, myWeightsByType, theirWeightsByType
				);
			}
		}
		
		// For everyone who recognizes me for this type
		for (const theirPub in networkWeightsByType) {
			if (theirPub === myPub) continue;
			
			const theirWeightsByType = networkWeightsByType[theirPub];
			const theirWeightsForType = theirWeightsByType[typeId] || {};
			
			if (theirWeightsForType[myPub] > 0 && !mrValues[theirPub]) {
				mrValues[theirPub] = computeTypeSpecificMutualRecognition(
					myPub, theirPub, typeId, myWeightsByType, theirWeightsByType
				);
			}
		}
		
		if (Object.keys(mrValues).length > 0) {
			mrValuesByType[typeId] = mrValues;
		}
	}
	
	return mrValuesByType;
}

// ═══════════════════════════════════════════════════════════════════
// HELPER STORES
// ═══════════════════════════════════════════════════════════════════

export const myPubKey = holsterUserPub;

/**
 * My multi-dimensional MR values (with normalization)
 */
export const myMultiDimensionalMutualRecognition: Readable<MultiDimensionalRecognition> = derived(
	[myRecognitionWeightsStore],
	([$myRecognitionWeights]) => {
		if (!$myRecognitionWeights) return {};
		
		const myPub = get(myPubKey);
		if (!myPub) return {};
		
		// Normalize weights first (D2')
		const normalizedWeights = normalizeTypeSpecificRecognitionWeights($myRecognitionWeights);
		
		const networkWeights = getNetworkRecognitionWeightsRecord();
		return computeAllTypeSpecificMutualRecognition(myPub, normalizedWeights, networkWeights);
	}
);

// ═══════════════════════════════════════════════════════════════════
// ADAPTIVE DAMPING (E12'-E15', E22-E23)
// ═══════════════════════════════════════════════════════════════════

/**
 * Detect oscillation pattern (E13')
 */
function detectOscillation(history: PerTypeDampingHistoryEntry[]): boolean {
	if (history.length < 3) return false;
	
	const recent = history.slice(-3).map(h => h.overAllocation);
	
	const upDownUp = recent[0] < recent[1] && recent[1] > recent[2];
	const downUpDown = recent[0] > recent[1] && recent[1] < recent[2];
	
	return upDownUp || downUpDown;
}

/**
 * Detect smooth convergence (monotonically decreasing)
 */
function detectSmoothConvergence(history: PerTypeDampingHistoryEntry[]): boolean {
	if (history.length < 3) return false;
	
	const recent = history.slice(-3).map(h => h.overAllocation);
	
	return recent[0] >= recent[1] && recent[1] >= recent[2];
}

/**
 * Compute adaptive damping factor per type (E14')
 * α_k(t) = {
 *   0.5   if oscillating(H_i^k(t))
 *   1.0   if smooth_convergence(H_i^k(t))
 *   0.8   otherwise
 * }
 */
export function computeDampingFactor(history: PerTypeDampingHistoryEntry[]): number {
	if (history.length < 3) {
		return DAMPING_SMOOTH;
	}
	
	const now = Date.now();
	
	// Hybrid: time-based if available, else count-based (Theorem 4)
	const timeFiltered = history.filter(h => now - h.timestamp < DAMPING_HISTORY_WINDOW_MS);
	
	const relevantHistory = timeFiltered.length >= 3
		? timeFiltered.slice(-DAMPING_HISTORY_MAX_COUNT)
		: history.slice(-DAMPING_HISTORY_MAX_COUNT);
	
	if (detectOscillation(relevantHistory)) {
		return DAMPING_OSCILLATING;
	}
	
	if (detectSmoothConvergence(relevantHistory)) {
		return DAMPING_SMOOTH;
	}
	
	return DAMPING_MODERATE;
}

/**
 * Update commitment with per-type damping (E12'-E15')
 */
export function updateCommitmentMultiDimensionalDamping(
	commitment: Commitment,
	totalReceivedByType: Record<string, number>
): Commitment {
	const damping: MultiDimensionalDamping = commitment.multi_dimensional_damping || {
		damping_factors: {},
		damping_history: {},
		global_damping_factor: 1.0
	};
	
	// Group needs by type
	const statedNeedByType: Record<string, number> = {};
	if (commitment.need_slots) {
		for (const slot of commitment.need_slots) {
			const typeId = slot.need_type_id; // Required field
			statedNeedByType[typeId] = (statedNeedByType[typeId] || 0) + slot.quantity;
		}
	}
	
	// Update damping per type
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
		
		// Filter by time and count
		const now = Date.now();
		const timeFiltered = history.filter(h => now - h.timestamp < DAMPING_HISTORY_WINDOW_MS);
		const filteredHistory = timeFiltered.length >= DAMPING_HISTORY_MAX_COUNT
			? timeFiltered.slice(-DAMPING_HISTORY_MAX_COUNT)
			: history.slice(-DAMPING_HISTORY_MAX_COUNT);
		
		damping.damping_history[typeId] = filteredHistory;
		
		// Compute damping factor for this type
		const dampingFactor = computeDampingFactor(filteredHistory);
		damping.damping_factors[typeId] = dampingFactor;
		
		console.log(`[DAMPING-V4] Type ${typeId}: α_k=${dampingFactor.toFixed(2)}, over=${overAllocation.toFixed(2)}`);
	}
	
	// Compute global damping as average
	const factors = Object.values(damping.damping_factors);
	damping.global_damping_factor = factors.length > 0
		? factors.reduce((a, b) => a + b, 0) / factors.length
		: 1.0;
	
	return {
		...commitment,
		multi_dimensional_damping: damping
	};
}

// ═══════════════════════════════════════════════════════════════════
// CONVERGENCE METRICS (Theorem 1', 3, E34-E37, E41'-E43, E45')
// ═══════════════════════════════════════════════════════════════════

/**
 * Compute Euclidean norm of per-type need vector
 * ||N⃗^k(t)|| = √(Σ_i (N_i^k(t))²)
 */
export function computePerTypeNeedVectorNorm(
	needVector: Record<string, MultiDimensionalNeedState>,
	typeId: string
): number {
	let sumSquares = 0;
	for (const needState of Object.values(needVector)) {
		const perTypeState = needState.needsByType[typeId];
		if (perTypeState) {
			sumSquares += perTypeState.residualNeed ** 2;
		}
	}
	return Math.sqrt(sumSquares);
}

/**
 * Compute Frobenius norm of multi-dimensional need matrix (Theorem 1')
 * ||N⃗⃗(t)||_F = √(Σ_k ||N⃗^k(t)||²)
 */
export function computeFrobeniusNorm(
	needVector: Record<string, MultiDimensionalNeedState>
): number {
	let sumSquares = 0;
	for (const needState of Object.values(needVector)) {
		for (const perTypeState of Object.values(needState.needsByType)) {
			sumSquares += perTypeState.residualNeed ** 2;
		}
	}
	return Math.sqrt(sumSquares);
}

/**
 * Compute per-type contraction constant
 * k_k = ||N⃗^k(t+1)|| / ||N⃗^k(t)||
 */
export function computePerTypeContractionConstant(
	currentNorm: number,
	previousNorm: number
): number {
	if (previousNorm < CONVERGENCE_EPSILON) {
		return 0;
	}
	return currentNorm / previousNorm;
}

/**
 * Check Heaven condition per type (E41')
 * Heaven^k(t) ⟺ ∀i: N_i^k(t) = 0
 */
export function checkPerTypeHeavenCondition(
	needVector: Record<string, MultiDimensionalNeedState>,
	typeId: string
): boolean {
	for (const needState of Object.values(needVector)) {
		const perTypeState = needState.needsByType[typeId];
		if (perTypeState && perTypeState.residualNeed > CONVERGENCE_EPSILON) {
			return false;
		}
	}
	return true;
}

/**
 * Check multi-dimensional Heaven condition (E41')
 * Heaven(t) ⟺ ∀i,k: N_i^k(t) = 0
 */
export function checkMultiDimensionalHeavenCondition(
	needVector: Record<string, MultiDimensionalNeedState>
): boolean {
	for (const needState of Object.values(needVector)) {
		for (const perTypeState of Object.values(needState.needsByType)) {
			if (perTypeState.residualNeed > CONVERGENCE_EPSILON) {
				return false;
			}
		}
	}
	return true;
}

/**
 * Compute percent of needs met per type
 */
export function computePerTypePercentNeedsMet(
	needVector: Record<string, MultiDimensionalNeedState>,
	typeId: string
): number {
	const participants = Object.values(needVector).filter(state => 
		state.needsByType[typeId] !== undefined
	);
	
	if (participants.length === 0) return 100;
	
	let metCount = 0;
	for (const needState of participants) {
		const perTypeState = needState.needsByType[typeId];
		if (perTypeState && perTypeState.residualNeed < CONVERGENCE_EPSILON) {
			metCount++;
		}
	}
	
	return (metCount / participants.length) * 100;
}

/**
 * Compute overall percent of needs met (across all types)
 */
export function computeOverallPercentNeedsMet(
	needVector: Record<string, MultiDimensionalNeedState>
): number {
	let totalNeeds = 0;
	let metNeeds = 0;
	
	for (const needState of Object.values(needVector)) {
		for (const perTypeState of Object.values(needState.needsByType)) {
			totalNeeds++;
			if (perTypeState.residualNeed < CONVERGENCE_EPSILON) {
				metNeeds++;
			}
		}
	}
	
	if (totalNeeds === 0) return 100;
	return (metNeeds / totalNeeds) * 100;
}

/**
 * Get all need types from system state
 */
export function getAllNeedTypes(state: SystemState): string[] {
	const types = new Set<string>();
	
	for (const needState of Object.values(state.needVector)) {
		for (const typeId of Object.keys(needState.needsByType)) {
			types.add(typeId);
		}
	}
	
	for (const capacityState of Object.values(state.capacityVector)) {
		for (const typeId of Object.keys(capacityState.capacitiesByType)) {
			types.add(typeId);
		}
	}
	
	return Array.from(types);
}

/**
 * Compute full convergence metrics (multi-dimensional)
 */
export function computeConvergenceMetrics(
	currentState: SystemState,
	previousState: SystemState | null,
	iterationStartTime: number
): ConvergenceMetrics {
	const currentFrobeniusNorm = computeFrobeniusNorm(currentState.needVector);
	const previousFrobeniusNorm = previousState 
		? computeFrobeniusNorm(previousState.needVector)
		: currentFrobeniusNorm * 2;
	
	const contractionConstant = previousFrobeniusNorm > CONVERGENCE_EPSILON
		? currentFrobeniusNorm / previousFrobeniusNorm
		: 0;
	
	// Per-type metrics
	const allTypes = getAllNeedTypes(currentState);
	const perTypeMetrics: Record<string, PerTypeConvergenceMetrics> = {};
	let maxContractionConstant = 0;
	
	for (const typeId of allTypes) {
		const currentNorm = computePerTypeNeedVectorNorm(currentState.needVector, typeId);
		const previousNorm = previousState
			? computePerTypeNeedVectorNorm(previousState.needVector, typeId)
			: currentNorm * 2;
		
		const k_k = computePerTypeContractionConstant(currentNorm, previousNorm);
		maxContractionConstant = Math.max(maxContractionConstant, k_k);
		
		const isConverged = currentNorm < CONVERGENCE_EPSILON;
		const heavenAchieved = checkPerTypeHeavenCondition(currentState.needVector, typeId);
		const percentNeedsMet = computePerTypePercentNeedsMet(currentState.needVector, typeId);
		
		let iterationsToConvergence: number | null = null;
		if (k_k > 0 && k_k < 1) {
			const estimatedIterations = Math.log(CONVERGENCE_EPSILON / currentNorm) / Math.log(k_k);
			iterationsToConvergence = Math.max(0, Math.ceil(estimatedIterations));
		} else if (isConverged) {
			iterationsToConvergence = 0;
		}
		
		const convergenceRate = k_k > 0 ? -Math.log(k_k) : 0;
		
		perTypeMetrics[typeId] = {
			need_type_id: typeId,
			needVectorNorm: currentNorm,
			needVectorNormPrevious: previousNorm,
			contractionConstant: k_k,
			isConverged,
			iterationsToConvergence,
			convergenceRate,
			heavenAchieved,
			percentNeedsMet
		};
	}
	
	const isConverged = currentFrobeniusNorm < CONVERGENCE_EPSILON;
	const heavenAchieved = checkMultiDimensionalHeavenCondition(currentState.needVector);
	const percentNeedsMet = computeOverallPercentNeedsMet(currentState.needVector);
	
	// Timing metrics
	const now = Date.now();
	const responseLatency = now - iterationStartTime;
	const timeSincePrevious = previousState 
		? now - previousState.timestamp
		: 1000;
	const iterationFrequency = 1000 / timeSincePrevious;
	
	// Estimate iterations to convergence (Theorem 3)
	let iterationsToConvergence: number | null = null;
	if (contractionConstant > 0 && contractionConstant < 1) {
		const estimatedIterations = Math.log(CONVERGENCE_EPSILON / currentFrobeniusNorm) / Math.log(contractionConstant);
		iterationsToConvergence = Math.max(0, Math.ceil(estimatedIterations));
	} else if (isConverged) {
		iterationsToConvergence = 0;
	}
	
	const convergenceRate = contractionConstant > 0 ? -Math.log(contractionConstant) : 0;
	
	// Freedom metric (E45' - Frobenius norm)
	const freedomMetric = currentFrobeniusNorm;
	
	return {
		frobeniusNorm: currentFrobeniusNorm,
		frobeniusNormPrevious: previousFrobeniusNorm,
		contractionConstant,
		perTypeMetrics,
		isConverged,
		iterationsToConvergence,
		convergenceRate,
		responseLatency,
		lastIterationTime: now,
		iterationFrequency,
		heavenAchieved,
		percentNeedsMet,
		freedomMetric
	};
}

// ═══════════════════════════════════════════════════════════════════
// FILTER CONTEXT BUILDING
// ═══════════════════════════════════════════════════════════════════

function buildFilterContext(
	pubKey: string,
	commitment: Commitment,
	mutualRecognitionValue?: number
): FilterContext {
	return {
		pubKey,
		commitment: commitment as any,
		mutualRecognition: mutualRecognitionValue,
		attributes: (commitment as any).attributes || {}
	};
}

// ═══════════════════════════════════════════════════════════════════
// NOTE: Type matching (E28') is now built into slotsCompatible() in match.svelte.ts
// The v5 slotsCompatible() checks: type + time + location compatibility
// ═══════════════════════════════════════════════════════════════════
// MULTI-DIMENSIONAL ALLOCATION OPERATOR T: ℝ^{n×m} → ℝ^{n×m}
// (E1'-E11', E16'-E18')
// ═══════════════════════════════════════════════════════════════════

/**
 * Apply multi-dimensional allocation operator T
 * T: ℝ^{n×m} → ℝ^{n×m}
 * T(N⃗⃗(t)) = N⃗⃗(t+1)
 * 
 * Implements:
 * 1. Type-specific two-tier allocation (E1'-E11')
 * 2. Per-type need update law (E17'): N_i^k(t+1) = max(0, N_i^k(t) - A_total^k(i, t))
 * 3. Multi-dimensional contraction (Theorem 1')
 * 
 * Each need type evolves independently (key insight for contractiveness)
 */
export function applyMultiDimensionalAllocationOperator(
	providerPubKey: string,
	providerCommitment: Commitment,
	myMRValuesByType: MultiDimensionalRecognition,
	myWeightsByType: MultiDimensionalRecognition,
	networkCommitments: Record<string, Commitment>,
	currentNeedVector: Record<string, MultiDimensionalNeedState>
): AllocationOperatorResult {
	const iterationStartTime = Date.now();
	
	console.log(`[OPERATOR-V4] Applying T to N⃗⃗(t), ||N⃗⃗||_F = ${computeFrobeniusNorm(currentNeedVector).toFixed(3)}`);
	
	// ────────────────────────────────────────────────────────────────
	// STEP 1: Compute slot-native allocations per type (E1'-E11')
	// ────────────────────────────────────────────────────────────────
	
	const allocations = computeMultiDimensionalSlotNativeAllocation(
		providerPubKey,
		providerCommitment,
		myMRValuesByType,
		myWeightsByType,
		networkCommitments
	);
	
	// ────────────────────────────────────────────────────────────────
	// STEP 2: Apply per-type need update law (E17')
	// N_i^k(t+1) = max(0, N_i^k(t) - A_total^k(i, t))
	// ────────────────────────────────────────────────────────────────
	
	const updatedNeedVector: Record<string, MultiDimensionalNeedState> = {};
	
	// Copy current need state
	for (const [pubKey, needState] of Object.entries(currentNeedVector)) {
		updatedNeedVector[pubKey] = {
			...needState,
			needsByType: { ...needState.needsByType }
		};
	}
	
	// Apply allocations per type (E16'-E17')
	const recipientTotalsByType = allocations.recipient_totals_by_type || {};
	
	for (const [typeId, recipientTotals] of Object.entries(recipientTotalsByType)) {
		for (const [recipientPub, totalReceived] of Object.entries(recipientTotals)) {
			const needState = updatedNeedVector[recipientPub];
			
			if (needState && needState.needsByType[typeId]) {
				const currentNeed = needState.needsByType[typeId];
				
				// E17': The contraction mapping per type
				const newResidualNeed = Math.max(0, currentNeed.residualNeed - totalReceived);
				
				needState.needsByType[typeId] = {
					...currentNeed,
					residualNeed: newResidualNeed,
					lastAllocationReceived: totalReceived
				};
				
				console.log(`[UPDATE-V4] ${recipientPub.slice(0, 8)}.${typeId}: N=${currentNeed.residualNeed.toFixed(2)} - A=${totalReceived.toFixed(2)} = ${newResidualNeed.toFixed(2)}`);
			}
		}
	}
	
	// Recompute total norms for each participant
	for (const needState of Object.values(updatedNeedVector)) {
		let totalResidualNeed = 0;
		let totalMaxNeed = 0;
		
		for (const perTypeState of Object.values(needState.needsByType)) {
			totalResidualNeed += perTypeState.residualNeed ** 2;
			totalMaxNeed += perTypeState.maxNeed ** 2;
		}
		
		needState.totalResidualNeed = Math.sqrt(totalResidualNeed);
		needState.totalMaxNeed = Math.sqrt(totalMaxNeed);
		needState.timestamp = Date.now();
	}
	
	// ────────────────────────────────────────────────────────────────
	// STEP 3: Compute convergence metrics (Frobenius norm)
	// ────────────────────────────────────────────────────────────────
	
	const tempState: SystemState = {
		...currentSystemState,
		needVector: updatedNeedVector,
		timestamp: Date.now()
	};
	
	const convergenceMetrics = computeConvergenceMetrics(
		tempState,
		previousSystemState,
		iterationStartTime
	);
	
	console.log(`[METRICS-V4] k_max=${convergenceMetrics.contractionConstant.toFixed(3)}, Heaven=${convergenceMetrics.heavenAchieved}, Freedom=${convergenceMetrics.freedomMetric.toFixed(3)}`);
	
	return {
		allocations,
		updatedNeedVector,
		convergenceMetrics
	};
}

/**
 * Compute multi-dimensional slot-native allocation
 * Implements E1'-E11' with slot-native extensions (E27'-E30')
 * 
 * Key: Allocations happen independently per need type k
 */
export function computeMultiDimensionalSlotNativeAllocation(
	providerPubKey: string,
	providerCommitment: Commitment,
	myMRValuesByType: MultiDimensionalRecognition,
	myWeightsByType: MultiDimensionalRecognition,
	networkCommitments: Record<string, Commitment>
): TwoTierAllocationState {
	
	console.log(`[ALLOC-V4] Computing multi-dimensional allocation for ${providerPubKey.slice(0,8)}`);
	
	const slotAllocations: SlotAllocationRecord[] = [];
	const slotDenominators: Record<string, { mutual: number; nonMutual: number; need_type_id: string }> = {};
	const recipientTotalsByType: PerTypeAllocationTotals = {};
	
	if (!providerCommitment.capacity_slots || providerCommitment.capacity_slots.length === 0) {
		return {
			slot_denominators: {},
			slot_allocations: [],
			recipient_totals_by_type: {},
			timestamp: Date.now()
		};
	}
	
	// Group capacity slots by type for independent processing
	const slotsByType = new Map<string, AvailabilitySlot[]>();
	for (const slot of providerCommitment.capacity_slots) {
		const typeId = slot.need_type_id; // Required field
		if (!slotsByType.has(typeId)) {
			slotsByType.set(typeId, []);
		}
		slotsByType.get(typeId)!.push(slot);
	}
	
	// Process each need type independently (CRITICAL for multi-dimensional framework)
	for (const [typeId, capacitySlots] of slotsByType.entries()) {
		const myMRValues = myMRValuesByType[typeId] || {};
		const myWeights = myWeightsByType[typeId] || {};
		
		// Initialize type-specific totals
		if (!recipientTotalsByType[typeId]) {
			recipientTotalsByType[typeId] = {};
		}
		
		// Process this type's allocations
		const typeResult = computePerTypeSlotAllocation(
			providerPubKey,
			capacitySlots,
			typeId,
			myMRValues,
			myWeights,
			networkCommitments
		);
		
		// Merge results
		slotAllocations.push(...typeResult.slot_allocations);
		Object.assign(slotDenominators, typeResult.slot_denominators);
		
		// Merge recipient totals for this type
		const typeRecipientTotals = typeResult.recipient_totals_by_type[typeId] || {};
		for (const [recipientPub, amount] of Object.entries(typeRecipientTotals)) {
			recipientTotalsByType[typeId][recipientPub] = 
				(recipientTotalsByType[typeId][recipientPub] || 0) + amount;
		}
	}
	
	console.log(`[ALLOC-V4] Complete: ${slotAllocations.length} allocations across ${slotsByType.size} types`);
	
	return {
		slot_denominators: slotDenominators,
		slot_allocations: slotAllocations,
		recipient_totals_by_type: recipientTotalsByType,
		timestamp: Date.now()
	};
}

/**
 * Compute allocation for a single need type
 * This is where the type-specific allocation equations (E1'-E11') are applied
 */
function computePerTypeSlotAllocation(
	providerPubKey: string,
	capacitySlots: AvailabilitySlot[],
	typeId: string,
	myMRValues: Record<string, number>,
	myWeights: Record<string, number>,
	networkCommitments: Record<string, Commitment>
): TwoTierAllocationState {
	
	const slotAllocations: SlotAllocationRecord[] = [];
	const slotDenominators: Record<string, { mutual: number; nonMutual: number; need_type_id: string }> = {};
	const recipientTotalsByType: Record<string, number> = {};
	
	// Build compatibility matrix (only for this type)
	const compatibilityMatrix = new Map<string, Map<string, NeedSlot[]>>();
	
	for (const availSlot of capacitySlots) {
		const compatibleRecipients = new Map<string, NeedSlot[]>();
		
		for (const [recipientPub, recipientCommitment] of Object.entries(networkCommitments)) {
			if (recipientPub === providerPubKey) continue;
			if (!recipientCommitment.need_slots) continue;
			
			// Filter need slots for this specific type
			// Note: Explicit type check is an optimization to avoid calling slotsCompatible()
			// on wrong-type slots. slotsCompatible() checks type + time + location (v5)
			const matchingNeedSlots = recipientCommitment.need_slots.filter(needSlot => {
				return needSlot.need_type_id === typeId && slotsCompatible(needSlot, availSlot);
			});
			
			if (matchingNeedSlots.length > 0) {
				// Check filters
				// Note: Provider commitment with single slot for filter evaluation
				const singleSlotCommitment: Commitment = {
					capacity_slots: [availSlot],
					itcStamp: {},
					timestamp: Date.now()
				};
				
				const providerMR = myMRValues[recipientPub] || 0;
				
				const providerContext = buildFilterContext(
					providerPubKey,
					singleSlotCommitment,
					providerMR
				);
				
				const recipientContext = buildFilterContext(
					recipientPub,
					recipientCommitment,
					providerMR
				);
				
				const validSlots: NeedSlot[] = [];
				for (const needSlot of matchingNeedSlots) {
					if (passesSlotFilters(needSlot, availSlot, providerContext, recipientContext)) {
						validSlots.push(needSlot);
					}
				}
				
				if (validSlots.length > 0) {
					compatibleRecipients.set(recipientPub, validSlots);
				}
			}
		}
		
		compatibilityMatrix.set(availSlot.id, compatibleRecipients);
	}
	
	// Process each capacity slot for this type
	for (const availSlot of capacitySlots) {
		const slotQuantity = availSlot.quantity;
		if (slotQuantity <= 0) continue;
		
		const compatibleRecipients = compatibilityMatrix.get(availSlot.id) || new Map();
		if (compatibleRecipients.size === 0) continue;
		
		// Classify as mutual or non-mutual
		const mutualRecipients = new Map<string, NeedSlot[]>();
		const nonMutualRecipients = new Map<string, NeedSlot[]>();
		
		for (const [recipientPub, needSlots] of compatibleRecipients.entries()) {
			const mr = myMRValues[recipientPub] || 0;
			const weight = myWeights[recipientPub] || 0;
			
			if (mr > 0) {
				mutualRecipients.set(recipientPub, needSlots);
			} else if (weight > 0) {
				nonMutualRecipients.set(recipientPub, needSlots);
			}
		}
		
		if (mutualRecipients.size === 0 && nonMutualRecipients.size === 0) continue;
		
		// ────────────────────────────────────────────────────────────
		// TIER 1: MUTUAL RECOGNITION ALLOCATION (E1'-E5')
		// ────────────────────────────────────────────────────────────
		
		let mutualDenominator = 0;
		const mutualNumerators = new Map<string, number>();
		
		let totalMutualRecognition = 0;
		for (const recipientPub of mutualRecipients.keys()) {
			const mr = myMRValues[recipientPub] || 0;
			if (mr > 0) totalMutualRecognition += mr;
		}
		
		if (totalMutualRecognition > 0) {
			for (const [recipientPub, needSlots] of mutualRecipients.entries()) {
				const mr = myMRValues[recipientPub] || 0;
				if (mr === 0) continue;
				
				const recipientCommitment = networkCommitments[recipientPub];
				
				let totalNeed = 0;
				for (const needSlot of needSlots) {
					totalNeed += needSlot.quantity;
				}
				if (totalNeed <= 0) continue;
				
				// E1': MRD_j^k(i) = MR^k(j, i) / Σ_l MR^k(j, l)
				const mrd = mr / totalMutualRecognition;
				
				// E2': Num_mutual^k(j→i, t) = MRD_j^k(i) × N_i^{k,active}(t)
				// Get per-type damping factor
				const dampingFactor = recipientCommitment.multi_dimensional_damping?.damping_factors[typeId] 
					|| recipientCommitment.multi_dimensional_damping?.global_damping_factor
					|| 1.0;
				
				const activeNeed = totalNeed * dampingFactor; // E15': N_i^{k,active} = N_i^k × α_k
				
				const numerator = mrd * activeNeed;
				mutualNumerators.set(recipientPub, numerator);
				mutualDenominator += numerator;
			}
		}
		
		// E3': Denom_mutual^k(j, t) = max(ε, Σ_i Num_mutual^k(j→i, t))
		const safeMutualDenominator = Math.max(mutualDenominator, DENOMINATOR_FLOOR);
		let mutualCapacityUsed = 0;
		
		for (const [recipientPub, needSlots] of mutualRecipients.entries()) {
			const numerator = mutualNumerators.get(recipientPub);
			if (!numerator || numerator === 0) continue;
			
			let totalNeed = 0;
			for (const needSlot of needSlots) {
				totalNeed += needSlot.quantity;
			}
			
			// E4': A_mutual^{k,raw}(j→i, t) = C_j^k(t) × [Num / Denom]
			const rawAllocation = slotQuantity * numerator / safeMutualDenominator;
			
			// E5': A_mutual^k(j→i, t) = min(A_mutual^{k,raw}, N_i^k(t))
			// CRITICAL: Per-type capping ensures contractiveness
			const cappedAllocation = Math.min(rawAllocation, totalNeed);
			
			if (cappedAllocation > 0) {
				let remainingAllocation = cappedAllocation;
				
				for (const needSlot of needSlots) {
					if (remainingAllocation <= 0) break;
					
					const slotAllocation = Math.min(needSlot.quantity, remainingAllocation);
					
					slotAllocations.push({
						availability_slot_id: availSlot.id,
						recipient_pubkey: recipientPub,
						recipient_need_slot_id: needSlot.id,
						quantity: slotAllocation,
						need_type_id: typeId, // v4: track type
						time_compatible: true,
						location_compatible: true,
						tier: 'mutual'
					});
					
					remainingAllocation -= slotAllocation;
					mutualCapacityUsed += slotAllocation;
					recipientTotalsByType[recipientPub] = (recipientTotalsByType[recipientPub] || 0) + slotAllocation;
				}
			}
		}
		
		// ────────────────────────────────────────────────────────────
		// TIER 2: NON-MUTUAL ALLOCATION (E6'-E11')
		// ────────────────────────────────────────────────────────────
		
		// E6': C_j^{k,remaining}(t) = C_j^k(t) - Σ_i A_mutual^k(j→i, t)
		const remainingCapacity = slotQuantity - mutualCapacityUsed;
		
		if (remainingCapacity <= 0.0001 || nonMutualRecipients.size === 0) {
			slotDenominators[availSlot.id] = {
				mutual: mutualDenominator,
				nonMutual: 0,
				need_type_id: typeId
			};
			continue;
		}
		
		let nonMutualDenominator = 0;
		const nonMutualNumerators = new Map<string, number>();
		
		let totalNonMutualRecognition = 0;
		for (const recipientPub of nonMutualRecipients.keys()) {
			const weight = myWeights[recipientPub] || 0;
			if (weight > 0) totalNonMutualRecognition += weight;
		}
		
		if (remainingCapacity > 0 && totalNonMutualRecognition > 0) {
			for (const [recipientPub, needSlots] of nonMutualRecipients.entries()) {
				const weight = myWeights[recipientPub] || 0;
				if (weight === 0) continue;
				
				const recipientCommitment = networkCommitments[recipientPub];
				
				let totalNeed = 0;
				for (const needSlot of needSlots) {
					totalNeed += needSlot.quantity;
				}
				if (totalNeed <= 0) continue;
				
				// E7': S_j^k(i) = R_j^k(i) / Σ_l R_j^k(l)
				const renormalizedShare = weight / totalNonMutualRecognition;
				
				// E8': Num_nonmutual^k(j→i, t) = S_j^k(i) × N_i^{k,active}(t)
				const dampingFactor = recipientCommitment.multi_dimensional_damping?.damping_factors[typeId]
					|| recipientCommitment.multi_dimensional_damping?.global_damping_factor
					|| 1.0;
				
				const activeNeed = totalNeed * dampingFactor;
				
				const numerator = renormalizedShare * activeNeed;
				nonMutualNumerators.set(recipientPub, numerator);
				nonMutualDenominator += numerator;
			}
		}
		
		// E9': Denom_nonmutual^k(j, t) = max(ε, Σ_i Num_nonmutual^k(j→i, t))
		const safeNonMutualDenominator = Math.max(nonMutualDenominator, DENOMINATOR_FLOOR);
		
		for (const [recipientPub, needSlots] of nonMutualRecipients.entries()) {
			const numerator = nonMutualNumerators.get(recipientPub);
			if (!numerator || numerator === 0) continue;
			
			let totalNeed = 0;
			for (const needSlot of needSlots) {
				totalNeed += needSlot.quantity;
			}
			
			// E10': A_nonmutual^{k,raw}(j→i, t) = C_j^{k,remaining}(t) × [Num / Denom]
			const rawAllocation = remainingCapacity * numerator / safeNonMutualDenominator;
			
			// E11': A_nonmutual^k(j→i, t) = min(A_nonmutual^{k,raw}, residual need)
			const cappedAllocation = Math.min(rawAllocation, totalNeed);
			
			if (cappedAllocation > 0) {
				let remainingAllocation = cappedAllocation;
				
				for (const needSlot of needSlots) {
					if (remainingAllocation <= 0) break;
					
					const slotAllocation = Math.min(needSlot.quantity, remainingAllocation);
					
					slotAllocations.push({
						availability_slot_id: availSlot.id,
						recipient_pubkey: recipientPub,
						recipient_need_slot_id: needSlot.id,
						quantity: slotAllocation,
						need_type_id: typeId, // v4: track type
						time_compatible: true,
						location_compatible: true,
						tier: 'non-mutual'
					});
					
					remainingAllocation -= slotAllocation;
					recipientTotalsByType[recipientPub] = (recipientTotalsByType[recipientPub] || 0) + slotAllocation;
				}
			}
		}
		
		slotDenominators[availSlot.id] = {
			mutual: mutualDenominator,
			nonMutual: nonMutualDenominator,
			need_type_id: typeId
		};
	}
	
	return {
		slot_denominators: slotDenominators,
		slot_allocations: slotAllocations,
		recipient_totals_by_type: { [typeId]: recipientTotalsByType },
		timestamp: Date.now()
	};
}

// ═══════════════════════════════════════════════════════════════════
// REACTIVE ALLOCATION COMPUTATION (v4: Multi-Dimensional Iteration)
// ═══════════════════════════════════════════════════════════════════

/**
 * Reactive allocation with full multi-dimensional operator application
 * Implements T^t(N⃗⃗(0)) → N⃗⃗* = 0⃗⃗ (E42-E43)
 */
export const myAllocationsReactive: Readable<AllocationOperatorResult | null> = derived<
	[
		typeof myCommitmentStore,
		typeof myMultiDimensionalMutualRecognition,
		typeof myRecognitionWeightsStore,
		typeof myPubKey
	],
	AllocationOperatorResult | null
>(
	[myCommitmentStore, myMultiDimensionalMutualRecognition, myRecognitionWeightsStore, myPubKey],
	([$myCommit, $mr, $weights, $myPubKey], set) => {
		if (!$myCommit || !$weights || !$myPubKey) {
			set(null);
			return;
		}
		
		// Only compute if I have capacity slots
		if (!$myCommit.capacity_slots || $myCommit.capacity_slots.length === 0) {
			set(null);
			return;
		}
		
		// Update system state from network
		updateSystemStateFromNetwork();
		
		// Get network data
		const commitments = getCausallyConsistentCommitments();
		
		// Apply multi-dimensional allocation operator T (E18')
		const result = applyMultiDimensionalAllocationOperator(
			$myPubKey,
			$myCommit,
			$mr,
			normalizeTypeSpecificRecognitionWeights($weights), // Enforce D2'
			commitments,
			currentSystemState.needVector
		);
		
		// Update system state with new need vector
		previousSystemState = { ...currentSystemState };
		currentSystemState = {
			...currentSystemState,
			needVector: result.updatedNeedVector,
			timestamp: Date.now(),
			iteration: currentSystemState.iteration + 1
		};
		
		// Increment ITC stamp
		incrementMyITCStamp();
		
		// Add ITC stamp and convergence flag to result
		result.allocations.itcStamp = getMyITCStamp();
		result.allocations.converged = result.convergenceMetrics.isConverged;
		result.allocations.timestamp = Date.now();
		
		set(result);
		
		// Log key metrics
		console.log('[REACTIVE-V4] Multi-dimensional iteration complete:', {
			iteration: currentSystemState.iteration,
			frobeniusNorm: result.convergenceMetrics.frobeniusNorm.toFixed(3),
			k_max: result.convergenceMetrics.contractionConstant.toFixed(3),
			heaven: result.convergenceMetrics.heavenAchieved,
			pctMet: result.convergenceMetrics.percentNeedsMet.toFixed(1) + '%',
			types: Object.keys(result.convergenceMetrics.perTypeMetrics).length
		});
	},
	null
);

// ═══════════════════════════════════════════════════════════════════
// PUBLISHING FUNCTIONS (v4: With Multi-Dimensional Recognition)
// ═══════════════════════════════════════════════════════════════════

/**
 * Publish my commitment (with normalized multi-dimensional recognition weights)
 */
export async function publishMyCommitment(commitment: Commitment) {
	incrementMyITCStamp();
	
	const myPub = get(myPubKey);
	if (!myPub) {
		console.warn('[PUBLISH-V4] Cannot publish: no pubKey');
		return;
	}
	
	const mrValuesByType = get(myMultiDimensionalMutualRecognition);
	const recognitionWeightsByType = get(myRecognitionWeightsStore);
	
	// Normalize recognition weights (D2')
	const normalizedWeightsByType = recognitionWeightsByType 
		? normalizeTypeSpecificRecognitionWeights(recognitionWeightsByType)
		: {};
	
	const enrichedCommitment: Commitment = {
		...commitment,
		mr_values_by_type: mrValuesByType,
		recognition_weights_by_type: normalizedWeightsByType,
		itcStamp: getMyITCStamp(),
		timestamp: Date.now()
	};
	
	await myCommitmentStore.set(enrichedCommitment);
	
	console.log('[PUBLISH-V4] Published multi-dimensional commitment');
}

/**
 * Publish my recognition weights (with normalization per type)
 */
export async function publishMyRecognitionWeights(weightsByType: MultiDimensionalRecognition) {
	// Enforce normalization (D2')
	const normalizedWeightsByType = normalizeTypeSpecificRecognitionWeights(weightsByType);
	
	await myRecognitionWeightsStore.set(normalizedWeightsByType);
	
	const typeCount = Object.keys(normalizedWeightsByType).length;
	console.log(`[PUBLISH-V4] Published multi-dimensional recognition weights: ${typeCount} types`);
}

// ═══════════════════════════════════════════════════════════════════
// DEBUG / LOGGING
// ═══════════════════════════════════════════════════════════════════

export function logSystemState() {
	const state = getCurrentSystemState();
	const frobeniusNorm = computeFrobeniusNorm(state.needVector);
	const heaven = checkMultiDimensionalHeavenCondition(state.needVector);
	const pctMet = computeOverallPercentNeedsMet(state.needVector);
	const types = getAllNeedTypes(state);
	
	console.log('[SYSTEM-V4] Multi-dimensional state:', {
		iteration: state.iteration,
		participants: Object.keys(state.needVector).length,
		needTypes: types.length,
		frobeniusNorm: frobeniusNorm.toFixed(3),
		heavenAchieved: heaven,
		percentNeedsMet: pctMet.toFixed(1) + '%',
		itcStamp: itcToString(state.itcStamp)
	});
	
	// Log per-type norms
	console.log('[TYPES-V4]');
	for (const typeId of types) {
		const norm = computePerTypeNeedVectorNorm(state.needVector, typeId);
		const typeHeaven = checkPerTypeHeavenCondition(state.needVector, typeId);
		const typePctMet = computePerTypePercentNeedsMet(state.needVector, typeId);
		console.log(`  ${typeId}: ||N⃗^k|| = ${norm.toFixed(3)}, heaven=${typeHeaven}, ${typePctMet.toFixed(0)}% met`);
	}
	
	// Log individual participants
	console.log('[PARTICIPANTS-V4]');
	for (const [pubKey, needState] of Object.entries(state.needVector)) {
		const participantNorm = needState.totalResidualNeed || 0;
		const maxNorm = needState.totalMaxNeed || 1;
		const pct = ((1 - participantNorm / maxNorm) * 100).toFixed(0);
		console.log(`  ${pubKey.slice(0, 8)}: ||N⃗_i|| = ${participantNorm.toFixed(2)} (${pct}% met)`);
	}
}

export function logConvergenceMetrics(metrics: ConvergenceMetrics) {
	console.log('[CONVERGENCE-V4] Multi-dimensional:', {
		frobeniusNorm: metrics.frobeniusNorm.toFixed(3),
		frobeniusNormPrev: metrics.frobeniusNormPrevious.toFixed(3),
		k_max: metrics.contractionConstant.toFixed(3),
		converged: metrics.isConverged,
		iterationsRemaining: metrics.iterationsToConvergence,
		rate: metrics.convergenceRate.toFixed(3),
		latency: metrics.responseLatency.toFixed(0) + 'ms',
		frequency: metrics.iterationFrequency.toFixed(1) + ' Hz',
		heaven: metrics.heavenAchieved,
		pctMet: metrics.percentNeedsMet.toFixed(1) + '%',
		freedom: metrics.freedomMetric.toFixed(3),
		types: Object.keys(metrics.perTypeMetrics).length
	});
	
	// Log per-type metrics
	console.log('[PER-TYPE-V4]');
	for (const [typeId, typeMetrics] of Object.entries(metrics.perTypeMetrics)) {
		console.log(`  ${typeId}:`, {
			norm: typeMetrics.needVectorNorm.toFixed(3),
			k_k: typeMetrics.contractionConstant.toFixed(3),
			converged: typeMetrics.isConverged,
			heaven: typeMetrics.heavenAchieved,
			pctMet: typeMetrics.percentNeedsMet.toFixed(0) + '%'
		});
	}
}

if (typeof window !== 'undefined') {
	(window as any).debugAllocationV4 = logSystemState;
	(window as any).debugConvergenceV4 = (result: AllocationOperatorResult) => {
		logConvergenceMetrics(result.convergenceMetrics);
	};
	(window as any).computeAllocationV4 = applyMultiDimensionalAllocationOperator;
	(window as any).normalizeWeightsV4 = normalizeTypeSpecificRecognitionWeights;
	(window as any).getCurrentSystemStateV4 = getCurrentSystemState;
	(window as any).getMyITCStampV4 = getMyITCStamp;
	(window as any).getAllNeedTypesV4 = () => getAllNeedTypes(getCurrentSystemState());
}
