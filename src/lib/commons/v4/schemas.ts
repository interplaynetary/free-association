/**
 * Allocation Schemas v4 - Multi-Dimensional Framework
 * 
 * Key Changes from v2:
 * - Multi-dimensional needs: N⃗_i(t) = [N_i^1(t), N_i^2(t), ..., N_i^m(t)]^T
 * - Multi-dimensional capacities: C⃗_j(t) = [C_j^1(t), C_j^2(t), ..., C_j^m(t)]^T
 * - Type-specific mutual recognition: MR^k(A, B)
 * - Per-type damping factors: α_k(t)
 * - Frobenius norm convergence metrics
 * - Provider specialization by need type
 * 
 * Maintains compatibility with v2:
 * - ITC stamps for causality tracking
 * - Event-driven architecture
 * - Slot-native allocation
 */

import * as z from 'zod';

// Re-export v2 schemas that don't change
export {
	IdSchema,
	NameSchema,
	PointsSchema,
	PercentageSchema,
	ShareMapSchema,
	ITCIdSchema,
	ITCEventSchema,
	ITCStampSchema,
	ResourceMetadataSchema,
	NodeDataStorageSchema,
	NonRootNodeSchema,
	RootNodeSchema,
	NodeSchema
} from '../v2/schemas';

// Re-export v2 types that don't change
export type {
	ShareMap,
	ITCId,
	ITCEvent,
	ITCStamp,
	ResourceMetadata,
	NodeDataStorage,
	NonRootNode,
	RootNode,
	Node
} from '../v2/schemas';

// ═══════════════════════════════════════════════════════════════════
// NEED TYPE SYSTEM
// ═══════════════════════════════════════════════════════════════════

/**
 * Need Type Definition
 * Examples: food, housing, healthcare, education, transportation, childcare
 */
export const NeedTypeSchema = z.object({
	id: z.string().min(1),
	name: z.string().min(1),
	description: z.string().optional(),
	unit: z.string().default('units'),
	emoji: z.string().optional(),
	
	// Substitutability weights (for advanced extensions)
	substitution_weights: z.record(z.string(), z.number()).optional(),
	
	// Complementarity ratios (for advanced extensions)
	complementary_types: z.array(z.string()).optional(),
	complementary_ratios: z.record(z.string(), z.number()).optional()
});

export type NeedType = z.infer<typeof NeedTypeSchema>;

// ═══════════════════════════════════════════════════════════════════
// MULTI-DIMENSIONAL SLOTS
// ═══════════════════════════════════════════════════════════════════

/**
 * Availability Slot - Multi-Dimensional (Pure)
 * Each slot MUST specify which need type k it provides
 */
export const AvailabilitySlotSchema = z.object({
	id: z.string().min(1),
	quantity: z.number().gte(0),
	
	// REQUIRED: Type specification for multi-dimensional framework
	need_type_id: z.string().min(1), // k in C_j^k(t)
	
	// Resource metadata
	name: z.string(),
	emoji: z.string().optional(),
	unit: z.string().optional(),
	description: z.string().optional(),
	resource_type: z.string().optional(),
	filter_rule: z.any().optional(),
	hidden_until_request_accepted: z.boolean().optional(),
	
	// Timing constraints
	advance_notice_hours: z.number().gte(0).optional(),
	booking_window_hours: z.number().gte(0).optional(),
	
	// Time pattern
	all_day: z.boolean().optional(),
	recurrence: z.string().nullable().optional(),
	custom_recurrence_repeat_every: z.number().nullable().optional(),
	custom_recurrence_repeat_unit: z.string().nullable().optional(),
	custom_recurrence_end_type: z.string().nullable().optional(),
	custom_recurrence_end_value: z.string().nullable().optional(),
	start_date: z.string().nullable().optional(),
	start_time: z.string().nullable().optional(),
	end_date: z.string().nullable().optional(),
	end_time: z.string().nullable().optional(),
	time_zone: z.string().optional(),
	
	// Location
	location_type: z.string().optional(),
	longitude: z.number().min(-180).max(180).optional(),
	latitude: z.number().min(-90).max(90).optional(),
	street_address: z.string().optional(),
	city: z.string().optional(),
	state_province: z.string().optional(),
	postal_code: z.string().optional(),
	country: z.string().optional(),
	online_link: z.string().url().or(z.string().length(0)).optional(),
	
	// Hierarchical & coordination
	parent_slot_id: z.string().optional(),
	mutual_agreement_required: z.boolean().default(false).optional(),
	priority: z.number().optional()
});

export type AvailabilitySlot = z.infer<typeof AvailabilitySlotSchema>;

/**
 * Need Slot - Multi-Dimensional (Pure)
 * Each slot MUST specify which need type k it requests
 */
export const NeedSlotSchema = z.object({
	id: z.string().min(1),
	quantity: z.number().gte(0),
	
	// REQUIRED: Type specification for multi-dimensional framework
	need_type_id: z.string().min(1), // k in N_i^k(t)
	
	// Resource metadata
	name: z.string(),
	emoji: z.string().optional(),
	unit: z.string().optional(),
	description: z.string().optional(),
	resource_type: z.string().optional(),
	filter_rule: z.any().optional(),
	hidden_until_request_accepted: z.boolean().optional(),
	
	// Timing constraints
	advance_notice_hours: z.number().gte(0).optional(),
	booking_window_hours: z.number().gte(0).optional(),
	
	// Time pattern
	all_day: z.boolean().optional(),
	recurrence: z.string().nullable().optional(),
	custom_recurrence_repeat_every: z.number().nullable().optional(),
	custom_recurrence_repeat_unit: z.string().nullable().optional(),
	custom_recurrence_end_type: z.string().nullable().optional(),
	custom_recurrence_end_value: z.string().nullable().optional(),
	start_date: z.string().nullable().optional(),
	start_time: z.string().nullable().optional(),
	end_date: z.string().nullable().optional(),
	end_time: z.string().nullable().optional(),
	time_zone: z.string().optional(),
	
	// Location
	location_type: z.string().optional(),
	longitude: z.number().min(-180).max(180).optional(),
	latitude: z.number().min(-90).max(90).optional(),
	street_address: z.string().optional(),
	city: z.string().optional(),
	state_province: z.string().optional(),
	postal_code: z.string().optional(),
	country: z.string().optional(),
	online_link: z.string().url().or(z.string().length(0)).optional(),
	
	// Hierarchical & coordination
	parent_slot_id: z.string().optional(),
	mutual_agreement_required: z.boolean().default(false).optional(),
	priority: z.number().optional()
});

export type NeedSlot = z.infer<typeof NeedSlotSchema>;

// ═══════════════════════════════════════════════════════════════════
// MULTI-DIMENSIONAL RECOGNITION
// ═══════════════════════════════════════════════════════════════════

/**
 * Type-Specific Recognition Weights
 * R_A^k(B) for each need type k
 */
export const TypeSpecificRecognitionSchema = z.object({
	need_type_id: z.string().min(1),
	weights: z.record(z.string(), z.number().nonnegative()) // pubKey -> R_A^k(B)
});

export type TypeSpecificRecognition = z.infer<typeof TypeSpecificRecognitionSchema>;

/**
 * Multi-Dimensional Recognition Weights
 * Map from need type to recognition weights
 */
export const MultiDimensionalRecognitionSchema = z.record(
	z.string(), // need_type_id
	z.record(z.string(), z.number().nonnegative()) // pubKey -> weight
);

export type MultiDimensionalRecognition = z.infer<typeof MultiDimensionalRecognitionSchema>;

// ═══════════════════════════════════════════════════════════════════
// MULTI-DIMENSIONAL DAMPING
// ═══════════════════════════════════════════════════════════════════

/**
 * Per-Type Damping History Entry (E12')
 * H_i^k(t) = [h_i^k(t-2), h_i^k(t-1), h_i^k(t)]
 */
export const PerTypeDampingHistoryEntrySchema = z.object({
	need_type_id: z.string().min(1),
	overAllocation: z.number(),
	timestamp: z.number().int().positive()
});

export type PerTypeDampingHistoryEntry = z.infer<typeof PerTypeDampingHistoryEntrySchema>;

/**
 * Multi-Dimensional Damping State
 * Tracks damping per need type
 */
export const MultiDimensionalDampingSchema = z.object({
	// Per-type damping factors (α_k)
	damping_factors: z.record(z.string(), z.number().min(0).max(1)),
	
	// Per-type damping history
	damping_history: z.record(z.string(), z.array(PerTypeDampingHistoryEntrySchema)),
	
	// Global damping factor (backward compatibility)
	global_damping_factor: z.number().min(0).max(1).default(1.0)
});

export type MultiDimensionalDamping = z.infer<typeof MultiDimensionalDampingSchema>;

// ═══════════════════════════════════════════════════════════════════
// COMMITMENT SCHEMA (v4 - Multi-Dimensional)
// ═══════════════════════════════════════════════════════════════════

/**
 * Commitment - Pure Multi-Dimensional Framework
 * 
 * v4 Pure Implementation:
 * - All slots have required need_type_id
 * - Recognition weights organized by type
 * - Per-type damping factors and history
 * - No backward compatibility layers
 */
export const CommitmentSchema = z.object({
	// Multi-dimensional capacity & needs (slot-native)
	capacity_slots: z.array(AvailabilitySlotSchema).optional(),
	need_slots: z.array(NeedSlotSchema).optional(),
	
	// Type-specific recognition (R_A^k)
	recognition_weights_by_type: MultiDimensionalRecognitionSchema.optional(),
	mr_values_by_type: MultiDimensionalRecognitionSchema.optional(),
	
	// Causality tracking (ITC)
	itcStamp: z.any(), // ITCStampSchema imported from v2
	timestamp: z.number().int().positive(),
	
	// Per-type adaptive damping (α_k)
	multi_dimensional_damping: MultiDimensionalDampingSchema.optional()
});

export type Commitment = z.infer<typeof CommitmentSchema>;

// ═══════════════════════════════════════════════════════════════════
// MULTI-DIMENSIONAL ALLOCATION STATE
// ═══════════════════════════════════════════════════════════════════

/**
 * Slot Allocation Record - Extended with need type
 */
export const SlotAllocationRecordSchema = z.object({
	availability_slot_id: z.string().min(1),
	recipient_pubkey: z.string(),
	recipient_need_slot_id: z.string().optional(),
	quantity: z.number().nonnegative(),
	
	// MULTI-DIMENSIONAL: Type information
	need_type_id: z.string().min(1),
	
	// Compatibility
	time_compatible: z.boolean(),
	location_compatible: z.boolean(),
	tier: z.enum(['mutual', 'non-mutual'])
});

export type SlotAllocationRecord = z.infer<typeof SlotAllocationRecordSchema>;

/**
 * Per-Type Allocation Totals
 * A_total^k(i, t) for each recipient i and type k
 */
export const PerTypeAllocationTotalsSchema = z.record(
	z.string(), // need_type_id
	z.record(z.string(), z.number().nonnegative()) // pubKey -> total received
);

export type PerTypeAllocationTotals = z.infer<typeof PerTypeAllocationTotalsSchema>;

/**
 * Two-Tier Allocation State - Pure Multi-Dimensional
 * 
 * v4 Pure Implementation:
 * - Per-type allocation totals (A_total^k(i, t))
 * - Per-type convergence tracking
 * - Frobenius norm metrics
 * - No scalar fallbacks
 */
export const TwoTierAllocationStateSchema = z.object({
	// Slot-level allocations with type tracking
	slot_denominators: z.record(
		z.string(),
		z.object({
			mutual: z.number().nonnegative(),
			nonMutual: z.number().nonnegative(),
			need_type_id: z.string().min(1) // Required: type per slot
		})
	),
	slot_allocations: z.array(SlotAllocationRecordSchema),
	
	// Per-type allocation totals (A_total^k(i, t))
	recipient_totals_by_type: PerTypeAllocationTotalsSchema,
	
	// Per-type convergence tracking
	converged: z.boolean().optional(),
	convergence_by_type: z.record(z.string(), z.boolean()).optional(),
	
	convergenceHistory: z.array(z.object({
		denominatorDelta: z.number(),
		timestamp: z.number().int().positive()
	})).optional(),
	
	// Causality tracking (ITC)
	itcStamp: z.any().optional(), // ITCStampSchema
	timestamp: z.number().int().positive()
});

export type TwoTierAllocationState = z.infer<typeof TwoTierAllocationStateSchema>;

// ═══════════════════════════════════════════════════════════════════
// MULTI-DIMENSIONAL STATE VECTORS
// ═══════════════════════════════════════════════════════════════════

/**
 * Need State Per Type (D3')
 * N_i^k(t) ∈ [0, N_i^k_max]
 */
export const PerTypeNeedStateSchema = z.object({
	need_type_id: z.string().min(1),
	residualNeed: z.number().nonnegative(),    // N_i^k(t)
	maxNeed: z.number().nonnegative(),          // N_i^k_max
	lastAllocationReceived: z.number().nonnegative().default(0)
});

export type PerTypeNeedState = z.infer<typeof PerTypeNeedStateSchema>;

/**
 * Multi-Dimensional Need State Vector (D3')
 * N⃗_i(t) = [N_i^1(t), N_i^2(t), ..., N_i^m(t)]^T
 */
export const MultiDimensionalNeedStateSchema = z.object({
	pubKey: z.string().min(1),
	needsByType: z.record(z.string(), PerTypeNeedStateSchema), // need_type_id -> state
	timestamp: z.number().int().positive(),
	
	// Computed metrics
	totalResidualNeed: z.number().nonnegative().optional(),    // ||N⃗_i(t)||
	totalMaxNeed: z.number().nonnegative().optional()          // ||N⃗_i^max||
});

export type MultiDimensionalNeedState = z.infer<typeof MultiDimensionalNeedStateSchema>;

/**
 * Capacity State Per Type (D4')
 * C_j^k(t) ∈ [0, C_j^k_max]
 */
export const PerTypeCapacityStateSchema = z.object({
	need_type_id: z.string().min(1),
	availableCapacity: z.number().nonnegative(),   // C_j^k(t)
	maxCapacity: z.number().nonnegative(),          // C_j^k_max
	lastAllocationGiven: z.number().nonnegative().default(0)
});

export type PerTypeCapacityState = z.infer<typeof PerTypeCapacityStateSchema>;

/**
 * Multi-Dimensional Capacity State Vector (D4')
 * C⃗_j(t) = [C_j^1(t), C_j^2(t), ..., C_j^m(t)]^T
 */
export const MultiDimensionalCapacityStateSchema = z.object({
	pubKey: z.string().min(1),
	capacitiesByType: z.record(z.string(), PerTypeCapacityStateSchema), // need_type_id -> state
	timestamp: z.number().int().positive(),
	
	// Computed metrics
	totalAvailableCapacity: z.number().nonnegative().optional(), // ||C⃗_j(t)||
	totalMaxCapacity: z.number().nonnegative().optional()        // ||C⃗_j^max||
});

export type MultiDimensionalCapacityState = z.infer<typeof MultiDimensionalCapacityStateSchema>;

/**
 * System State Vector (E18')
 * N⃗⃗(t) = [N⃗_1(t), N⃗_2(t), ..., N⃗_n(t)]^T (n participants × m need types)
 */
export const SystemStateSchema = z.object({
	needVector: z.record(z.string(), MultiDimensionalNeedStateSchema),     // N⃗⃗(t)
	capacityVector: z.record(z.string(), MultiDimensionalCapacityStateSchema), // C⃗⃗(t)
	timestamp: z.number().int().positive(),
	iteration: z.number().int().nonnegative(),
	itcStamp: z.any() // ITCStampSchema
});

export type SystemState = z.infer<typeof SystemStateSchema>;

// ═══════════════════════════════════════════════════════════════════
// MULTI-DIMENSIONAL CONVERGENCE METRICS
// ═══════════════════════════════════════════════════════════════════

/**
 * Per-Type Convergence Metrics
 * Tracks convergence independently for each need type k
 */
export const PerTypeConvergenceMetricsSchema = z.object({
	need_type_id: z.string().min(1),
	
	// Per-type vector metrics
	needVectorNorm: z.number().nonnegative(),                 // ||N⃗^k(t)||
	needVectorNormPrevious: z.number().nonnegative(),         // ||N⃗^k(t-1)||
	contractionConstant: z.number().nonnegative(),            // k_k < 1
	
	// Per-type convergence
	isConverged: z.boolean(),
	iterationsToConvergence: z.number().int().nullable(),
	convergenceRate: z.number(),
	
	// Per-type heaven condition
	heavenAchieved: z.boolean(),                              // ∀i: N_i^k(t) = 0
	percentNeedsMet: z.number().min(0).max(100)               // % with N_i^k = 0
});

export type PerTypeConvergenceMetrics = z.infer<typeof PerTypeConvergenceMetricsSchema>;

/**
 * Multi-Dimensional Convergence Metrics (Theorem 1', 3)
 * Tracks convergence across all dimensions using Frobenius norm
 */
export const ConvergenceMetricsSchema = z.object({
	// System-level metrics (Frobenius norm)
	frobeniusNorm: z.number().nonnegative(),                  // ||N⃗⃗(t)||_F
	frobeniusNormPrevious: z.number().nonnegative(),          // ||N⃗⃗(t-1)||_F
	contractionConstant: z.number().nonnegative(),            // k_max = max_k k_k
	
	// Per-type metrics
	perTypeMetrics: z.record(z.string(), PerTypeConvergenceMetricsSchema),
	
	// Overall convergence
	isConverged: z.boolean(),                                 // All types converged
	iterationsToConvergence: z.number().int().nullable(),
	convergenceRate: z.number(),                              // Overall exponential rate
	
	// Timing metrics (E34-E37)
	responseLatency: z.number().nonnegative(),
	lastIterationTime: z.number().int().positive(),
	iterationFrequency: z.number().nonnegative(),
	
	// Heaven condition (E41' - multi-dimensional)
	heavenAchieved: z.boolean(),                              // ∀i,k: N_i^k(t) = 0
	percentNeedsMet: z.number().min(0).max(100),              // % across all types
	
	// Freedom metric (E45' - Frobenius norm)
	freedomMetric: z.number().nonnegative()                   // lim(t→∞) ||N⃗⃗(t)||_F
});

export type ConvergenceMetrics = z.infer<typeof ConvergenceMetricsSchema>;

// ═══════════════════════════════════════════════════════════════════
// VALIDATION HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Validate and parse multi-dimensional commitment
 */
export function parseCommitment(data: unknown): Commitment | null {
	const result = CommitmentSchema.safeParse(data);
	if (!result.success) {
		console.warn('[SCHEMA-V4] Invalid commitment:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Validate and parse multi-dimensional allocation state
 */
export function parseAllocationState(data: unknown): TwoTierAllocationState | null {
	const result = TwoTierAllocationStateSchema.safeParse(data);
	if (!result.success) {
		console.warn('[SCHEMA-V4] Invalid allocation state:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Validate and parse multi-dimensional system state
 */
export function parseSystemState(data: unknown): SystemState | null {
	const result = SystemStateSchema.safeParse(data);
	if (!result.success) {
		console.warn('[SCHEMA-V4] Invalid system state:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Validate and parse convergence metrics
 */
export function parseConvergenceMetrics(data: unknown): ConvergenceMetrics | null {
	const result = ConvergenceMetricsSchema.safeParse(data);
	if (!result.success) {
		console.warn('[SCHEMA-V4] Invalid convergence metrics:', result.error);
		return null;
	}
	return result.data;
}


