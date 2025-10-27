/**
 * Free-Association Algorithm v4 - Multi-Dimensional Framework
 * Public API exports
 * 
 * Mathematical framework from multi-grammar.md:
 * - Multi-dimensional needs: N⃗_i(t) = [N_i^1(t), ..., N_i^m(t)]^T
 * - Multi-dimensional capacities: C⃗_j(t) = [C_j^1(t), ..., C_j^m(t)]^T
 * - Type-specific mutual recognition: MR^k(A, B)
 * - Frobenius norm convergence: ||N⃗⃗(t)||_F
 * - Per-type damping: α_k(t)
 * - Provider specialization
 */

// ═══════════════════════════════════════════════════════════════════
// SCHEMAS & TYPES
// ═══════════════════════════════════════════════════════════════════

export type {
	// Core types (re-exported from v2)
	ShareMap,
	ITCId,
	ITCEvent,
	ITCStamp,
	ResourceMetadata,
	NodeDataStorage,
	NonRootNode,
	RootNode,
	Node,
	
	// Multi-dimensional types
	NeedType,
	AvailabilitySlot,
	NeedSlot,
	TypeSpecificRecognition,
	MultiDimensionalRecognition,
	PerTypeDampingHistoryEntry,
	MultiDimensionalDamping,
	Commitment,
	SlotAllocationRecord,
	PerTypeAllocationTotals,
	TwoTierAllocationState,
	
	// Multi-dimensional state vectors
	PerTypeNeedState,
	MultiDimensionalNeedState,
	PerTypeCapacityState,
	MultiDimensionalCapacityState,
	SystemState,
	
	// Convergence metrics
	PerTypeConvergenceMetrics,
	ConvergenceMetrics
} from '../../v5/schemas';

export {
	// Schemas (for validation)
	NeedTypeSchema,
	AvailabilitySlotSchema,
	NeedSlotSchema,
	TypeSpecificRecognitionSchema,
	MultiDimensionalRecognitionSchema,
	PerTypeDampingHistoryEntrySchema,
	MultiDimensionalDampingSchema,
	CommitmentSchema,
	SlotAllocationRecordSchema,
	PerTypeAllocationTotalsSchema,
	TwoTierAllocationStateSchema,
	PerTypeNeedStateSchema,
	MultiDimensionalNeedStateSchema,
	PerTypeCapacityStateSchema,
	MultiDimensionalCapacityStateSchema,
	SystemStateSchema,
	PerTypeConvergenceMetricsSchema,
	ConvergenceMetricsSchema,
	
	// Validation helpers
	parseCommitment,
	parseAllocationState,
	parseSystemState,
	parseConvergenceMetrics
} from '../../v5/schemas';

export type {
	AllocationOperatorResult
} from './algorithm.svelte';

// ═══════════════════════════════════════════════════════════════════
// CORE ALGORITHM FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

export {
	// Constants
	CONVERGENCE_EPSILON,
	DENOMINATOR_FLOOR,
	DAMPING_OSCILLATING,
	DAMPING_SMOOTH,
	DAMPING_MODERATE,
	DAMPING_HISTORY_WINDOW_MS,
	DAMPING_HISTORY_MAX_COUNT,
	
	// ITC functions
	getMyITCStamp,
	incrementMyITCStamp,
	mergeITCStampFromPeer,
	isPeerUpdateStale,
	getCausallyConsistentCommitments,
	
	// System state management
	getCurrentSystemState,
	updateSystemStateFromNetwork,
	initializeMultiDimensionalNeedState,
	initializeMultiDimensionalCapacityState,
	
	// Recognition weight normalization (D2')
	normalizeRecognitionWeights,
	normalizeTypeSpecificRecognitionWeights,
	validateRecognitionWeights,
	
	// Type-specific mutual recognition (D1', E19')
	computeTypeSpecificMutualRecognition,
	computeAllTypeSpecificMutualRecognition,
	
	// Adaptive damping (E12'-E15')
	computeDampingFactor,
	updateCommitmentMultiDimensionalDamping,
	
	// Convergence metrics (Theorem 1', 3)
	computePerTypeNeedVectorNorm,
	computeFrobeniusNorm,
	computePerTypeContractionConstant,
	checkPerTypeUniversalSatisfactionCondition,
	checkMultiDimensionalUniversalSatisfactionCondition,
	computePerTypePercentNeedsMet,
	computeOverallPercentNeedsMet,
	getAllNeedTypes,
	computeConvergenceMetrics,
	
	// Multi-dimensional allocation operator (E1'-E18')
	applyMultiDimensionalAllocationOperator,
	computeMultiDimensionalSlotNativeAllocation,
	
	// Publishing functions
	publishMyCommitment,
	publishMyRecognitionWeights,
	
	// Debug/logging
	logSystemState,
	logConvergenceMetrics
} from './algorithm.svelte';

// ═══════════════════════════════════════════════════════════════════
// STORES
// ═══════════════════════════════════════════════════════════════════

export {
	myPubKey,
	myMultiDimensionalMutualRecognition,
	myAllocationsReactive
} from './algorithm.svelte';

// ═══════════════════════════════════════════════════════════════════
// DOCUMENTATION GUIDE
// ═══════════════════════════════════════════════════════════════════

/**
 * Quick Start Guide
 * 
 * ## Multi-Dimensional Framework
 * 
 * v4 extends the allocation algorithm to support multiple independent need types:
 * 
 * ### 1. Define Need Types
 * ```typescript
 * const needTypes = {
 *   food: { id: 'food', name: 'Food', unit: 'meals' },
 *   healthcare: { id: 'healthcare', name: 'Healthcare', unit: 'hours' },
 *   education: { id: 'education', name: 'Education', unit: 'hours' }
 * };
 * ```
 * 
 * ### 2. Create Type-Specific Slots
 * ```typescript
 * const capacitySlots = [
 *   { id: 's1', quantity: 100, need_type_id: 'food', name: 'Meals' },
 *   { id: 's2', quantity: 50, need_type_id: 'healthcare', name: 'Consultations' }
 * ];
 * 
 * const needSlots = [
 *   { id: 'n1', quantity: 20, need_type_id: 'food', name: 'Meals needed' },
 *   { id: 'n2', quantity: 10, need_type_id: 'education', name: 'Tutoring hours' }
 * ];
 * ```
 * 
 * ### 3. Set Type-Specific Recognition
 * ```typescript
 * const recognitionWeightsByType = {
 *   food: {
 *     'alice_pub': 0.8,  // Alice is recognized for food provision
 *     'bob_pub': 0.2
 *   },
 *   healthcare: {
 *     'doctor_pub': 0.9,  // Doctor is recognized for healthcare
 *     'nurse_pub': 0.1
 *   }
 * };
 * ```
 * 
 * ### 4. Monitor Convergence
 * ```typescript
 * myAllocationsReactive.subscribe(result => {
 *   if (!result) return;
 *   
 *   // System-level metrics (Frobenius norm)
 *   console.log('||N⃗⃗||_F =', result.convergenceMetrics.frobeniusNorm);
 *   console.log('UniversalSatisfaction achieved:', result.convergenceMetrics.universalSatisfactionAchieved);
 *   
 *   // Per-type metrics
 *   for (const [typeId, metrics] of Object.entries(result.convergenceMetrics.perTypeMetrics)) {
 *     console.log(`Type ${typeId}:`);
 *     console.log('  ||N⃗^k|| =', metrics.needVectorNorm);
 *     console.log('  k_k =', metrics.contractionConstant);
 *     console.log('  UniversalSatisfaction^k =', metrics.universalSatisfactionAchieved);
 *   }
 * });
 * ```
 * 
 * ## Key Concepts
 * 
 * ### Provider Specialization
 * Providers can specialize in specific need types by offering capacity slots
 * with matching `need_type_id`. Recognition weights can vary per type, allowing
 * expertise-based recognition.
 * 
 * ### Independent Convergence
 * Each need type k evolves independently with its own:
 * - Contraction constant k_k < 1
 * - Damping factor α_k ∈ [0.5, 1.0]
 * - UniversalSatisfaction condition (∀i: N_i^k = 0)
 * 
 * ### System Convergence
 * The system as a whole converges at the rate of the slowest type:
 * k_max = max_k{k_k}
 * 
 * Overall convergence uses Frobenius norm:
 * ||N⃗⃗(t)||_F = √(Σ_k ||N⃗^k(t)||²)
 * 
 * ## Mathematical Properties
 * 
 * ### Theorem 1': Multi-Dimensional Contractiveness
 * The operator T is contractive in Frobenius norm:
 * ||N⃗⃗(t+1)||_F ≤ k_max ||N⃗⃗(t)||_F where k_max < 1
 * 
 * ### Theorem 2': Multi-Dimensional Fixed-Point
 * The system converges to unique equilibrium:
 * N⃗⃗* = 0⃗⃗ (all needs of all types met)
 * 
 * ### Theorem 3: Exponential Convergence
 * ||N⃗⃗(t)||_F ≤ k_max^t ||N⃗⃗(0)||_F
 * 
 * ## Pure Multi-Dimensional Design
 * 
 * v4 is a pure multi-dimensional framework with no backward compatibility layers.
 * All slots MUST have a `need_type_id` field specifying which need type they provide/request.
 * All recognition weights are organized by type.
 * All damping factors are computed independently per type.
 * 
 * This ensures the mathematical elegance of the framework and prevents
 * ambiguity about need types.
 * 
 * ## See Also
 * 
 * - multi-grammar.md - Complete mathematical framework
 * - multi-dimensional.md - Detailed multi-dimensional reference
 * - CHEATSHEET.md - Quick reference for developers
 * - README.md - Equation mapping and examples
 */
