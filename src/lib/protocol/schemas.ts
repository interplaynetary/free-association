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

// ═══════════════════════════════════════════════════════════════════
// CORE SCHEMAS (from v2, made independent)
// ═══════════════════════════════════════════════════════════════════

export const IdSchema = z.string().min(1);
export const NameSchema = z.string().min(1);
export const PointsSchema = z.number().gte(0);
export const PercentageSchema = z.number().gte(0).lte(1);

/**
 * ShareMap - Maps entity IDs to their percentage shares
 * Used for recognition and allocation calculations
 */
export const ShareMapSchema = z.record(IdSchema, PercentageSchema);

export type ShareMap = z.infer<typeof ShareMapSchema>;

// ═══════════════════════════════════════════════════════════════════
// ITC CAUSALITY SCHEMAS (Replaces Vector Clocks)
// ═══════════════════════════════════════════════════════════════════

/**
 * ITC Id Component
 * Can be: 0 (null), 1 (full), or {l, r} (split)
 */
export const ITCIdSchema: z.ZodType<0 | 1 | { l: any; r: any }> = z.union([
	z.literal(0),
	z.literal(1),
	z.lazy(() => z.object({
		l: ITCIdSchema,
		r: ITCIdSchema
	}))
]);

/**
 * ITC Event Component
 * Can be: number (counter) or {n, l, r} (tree node)
 */
export const ITCEventSchema: z.ZodType<number | { n: number; l: any; r: any }> = z.union([
	z.number(),
	z.lazy(() => z.object({
		n: z.number(),
		l: ITCEventSchema,
		r: ITCEventSchema
	}))
]);

/**
 * ITC Stamp - Compact causality tracking
 * Replaces VectorClock with O(log n) space complexity
 */
export const ITCStampSchema = z.object({
	id: ITCIdSchema,
	event: ITCEventSchema
});

export type ITCId = z.infer<typeof ITCIdSchema>;
export type ITCEvent = z.infer<typeof ITCEventSchema>;
export type ITCStamp = z.infer<typeof ITCStampSchema>;

// ═══════════════════════════════════════════════════════════════════
// RESOURCE METADATA
// ═══════════════════════════════════════════════════════════════════

/**
 * Resource Metadata - Common fields for slots, capacities, needs
 */
export const ResourceMetadataSchema = z.object({
	name: z.string(),
	emoji: z.optional(z.string()),
	unit: z.optional(z.string()),
	description: z.optional(z.string()),
	resource_type: z.optional(z.string()),
	filter_rule: z.optional(z.nullable(z.any())),
	hidden_until_request_accepted: z.optional(z.boolean())
});

export type ResourceMetadata = z.infer<typeof ResourceMetadataSchema>;

// ═══════════════════════════════════════════════════════════════════
// TREE SCHEMAS (for recognition and priority trees)
// ═══════════════════════════════════════════════════════════════════

/**
 * Contributor - Represents a person who contributed to a node
 * 
 * Points determine the contributor's share of recognition from this node.
 * If a node has contributors [Alice: 50pts, Bob: 30pts], Alice gets 50/80 = 62.5% of the node's recognition.
 * 
 * This is analogous to how child node points work, but for contributors instead of subtasks.
 */
export const ContributorSchema = z.object({
	id: IdSchema,
	points: PointsSchema
});

export type Contributor = z.infer<typeof ContributorSchema>;

/**
 * Node Data Storage - Reactive Store Pattern for Tree Nodes
 * 
 * Each node becomes a mini-store that can:
 * - Store typed data (validated with schema)
 * - Track its Holster subscription path
 * - Maintain sync timestamps
 * - Manage loading/persisting state
 * - Subscribe to specific network data
 */
export const NodeDataStorageSchema = z.object({
	/** Arbitrary data stored at this node */
	data: z.optional(z.any()),
	
	/** Holster path this node subscribes to */
	holster_path: z.optional(z.string()),
	
	/** Schema type identifier for validation */
	data_schema_type: z.optional(z.string()),
	
	/** Timestamp when this node's data was last updated locally */
	data_updated_at: z.optional(z.number().int().positive()),
	
	/** Whether this node is currently loading data from network */
	is_loading: z.optional(z.boolean()),
	
	/** Whether this node is currently persisting data to network */
	is_persisting: z.optional(z.boolean()),
	
	/** Last network timestamp seen (for conflict resolution) */
	last_network_timestamp: z.optional(z.number().int().positive()),
	
	/** Whether to auto-persist changes to this node's data */
	auto_persist: z.optional(z.boolean().default(true)),
	
	/** Debounce time for persistence (ms) */
	persist_debounce_ms: z.optional(z.number().gte(0).default(0)),
	
	/** Optional pubkey to subscribe to (if subscribing to another user's data) */
	subscribe_to_user: z.optional(z.string()),
	
	/** Custom comparison function name for equality checking */
	equality_check: z.optional(z.string())
});

/**
 * Non-Root Node - Represents a node in a recognition/priority tree
 * 
 * V5 WEIGHTED CONTRIBUTORS:
 * - contributors: Array of {id, points} - each contributor has weighted share
 * - anti_contributors: Array of {id, points} - each anti-contributor has weighted share
 * - Share = contributor.points / sum(all contributor points)
 * 
 * Example: contributors: [{alice, 50}, {bob, 30}] → alice gets 50/80, bob gets 30/80
 */
export const NonRootNodeSchema = z.object({
	id: IdSchema,
	name: NameSchema,
	type: z.literal('NonRootNode'),
	manual_fulfillment: z.nullable(z.number()),
	children: z.array(z.any()), // Recursive reference
	points: PointsSchema,
	parent_id: IdSchema,
	contributors: z.array(ContributorSchema).default([]),
	anti_contributors: z.array(ContributorSchema).default([]),
	storage: z.optional(NodeDataStorageSchema)
});

/**
 * Root Node - Top-level node of a recognition/priority tree
 */
export const RootNodeSchema = z.object({
	id: IdSchema,
	name: NameSchema,
	type: z.literal('RootNode'),
	manual_fulfillment: z.nullable(z.number()),
	children: z.array(z.any()), // Recursive reference
	created_at: z.string(),
	updated_at: z.string(),
	storage: z.optional(NodeDataStorageSchema)
});

/**
 * Node - Union type for any node in a tree
 */
export const NodeSchema = z.union([RootNodeSchema, NonRootNodeSchema]);

export type NodeDataStorage = z.infer<typeof NodeDataStorageSchema>;
export type NonRootNode = z.infer<typeof NonRootNodeSchema>;
export type RootNode = z.infer<typeof RootNodeSchema>;
export type Node = z.infer<typeof NodeSchema>;

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
// AVAILABILITY WINDOW SYSTEM (for precise recurrence matching)
// ═══════════════════════════════════════════════════════════════════

/**
 * Time Range within a day
 * Example: { start_time: '09:00', end_time: '17:00' }
 */
export const TimeRangeSchema = z.object({
	start_time: z.string(), // HH:MM format
	end_time: z.string()     // HH:MM format
});

export type TimeRange = z.infer<typeof TimeRangeSchema>;

/**
 * Days of the week (for weekly/monthly recurrence)
 */
export const DayOfWeekSchema = z.enum([
	'monday',
	'tuesday', 
	'wednesday',
	'thursday',
	'friday',
	'saturday',
	'sunday'
]);

export type DayOfWeek = z.infer<typeof DayOfWeekSchema>;

/**
 * Day Schedule - Associates specific days with specific time ranges
 * 
 * This allows expressing patterns like:
 * - "Monday & Friday: 9am-12pm, Tuesday: 2pm-5pm"
 * - "Weekends: 10am-6pm, Weekdays: 9am-5pm"
 */
export const DayScheduleSchema = z.object({
	days: z.array(DayOfWeekSchema),
	time_ranges: z.array(TimeRangeSchema)
});

export type DaySchedule = z.infer<typeof DayScheduleSchema>;

/**
 * Week Schedule - Associates specific weeks of a month with day/time patterns
 * 
 * Allows expressing:
 * - "First and third week: Monday-Friday 9-5"
 * - "Second week: Tuesday only 2-4"
 */
export const WeekScheduleSchema = z.object({
	weeks: z.array(z.number().int().min(1).max(5)),  // 1-5 (which weeks)
	day_schedules: z.array(DayScheduleSchema)
});

export type WeekSchedule = z.infer<typeof WeekScheduleSchema>;

/**
 * Month Schedule - Associates a specific month with week/day/time patterns
 * 
 * Allows expressing:
 * - "February: all weeks, Monday/Wednesday 9-12"
 * - "September: first week only, all weekdays 10-5"
 * - "October: second week Tuesday 2-4, fourth week Monday/Wednesday 9-12"
 */
export const MonthScheduleSchema = z.object({
	month: z.number().int().min(1).max(12),  // 1-12 (January-December)
	
	// OPTION 1: Week-specific patterns within this month (most flexible)
	week_schedules: z.array(WeekScheduleSchema).optional(),
	
	// OPTION 2: Simple day schedules for all weeks in this month
	day_schedules: z.array(DayScheduleSchema).optional(),
	
	// OPTION 3: Same times every day, all weeks in this month
	time_ranges: z.array(TimeRangeSchema).optional()
});

export type MonthSchedule = z.infer<typeof MonthScheduleSchema>;

/**
 * Availability Window - Hierarchical definition of recurring availability
 * 
 * THREE LEVELS OF SPECIFICITY:
 * 
 * LEVEL 1 (Most Specific): Month-specific patterns
 *   month_schedules: [
 *     { month: 2, day_schedules: [...] },           // February: specific days/times
 *     { month: 9, week_schedules: [                 // September: week-specific
 *       { weeks: [1], day_schedules: [...] }
 *     ]},
 *     { month: 10, week_schedules: [                // October: multiple week patterns
 *       { weeks: [2], day_schedules: [{ days: ['tuesday'], ... }] },
 *       { weeks: [4], day_schedules: [...] }
 *     ]}
 *   ]
 * 
 * LEVEL 2 (Week-Specific): Week/day patterns (no month distinction)
 *   week_schedules: [
 *     { weeks: [1, 3], day_schedules: [...] }       // First & third week
 *   ]
 * 
 * LEVEL 3 (Simple): Day patterns (all weeks, all months)
 *   day_schedules: [
 *     { days: ['monday', 'friday'], time_ranges: [...] }
 *   ]
 * 
 * LEVEL 4 (Simplest): Time ranges (all days, all weeks, all months)
 *   time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
 * 
 * Priority: month_schedules > week_schedules > day_schedules > time_ranges
 */
export const AvailabilityWindowSchema = z.object({
	// LEVEL 1: Month-specific patterns (for yearly recurrence)
	month_schedules: z.array(MonthScheduleSchema).optional(),
	
	// LEVEL 2: Week-specific patterns (for monthly recurrence)
	week_schedules: z.array(WeekScheduleSchema).optional(),
	
	// LEVEL 3: Day-specific patterns (for weekly/daily recurrence)
	day_schedules: z.array(DayScheduleSchema).optional(),
	
	// LEVEL 4: Simple time ranges (same for all days/weeks/months)
	time_ranges: z.array(TimeRangeSchema).optional()
});

export type AvailabilityWindow = z.infer<typeof AvailabilityWindowSchema>;

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
	
	// Divisibility constraints (prevents over-fragmentation)
	max_natural_div: z.number().gte(1).optional(), // Max natural divisions (e.g., can't divide a person)
	max_percentage_div: PercentageSchema.optional(), // Max % divisibility (e.g., can't give less than 10%)
	
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
	
	// Recurrence pattern
	recurrence: z.enum(['daily', 'weekly', 'monthly', 'yearly']).nullable().optional(),
	
	// Date range (for one-time slots or to bound recurring patterns)
	start_date: z.string().nullable().optional(),  // ISO date string
	end_date: z.string().nullable().optional(),    // ISO date string (for recurring, when pattern ends)
	
	// Timezone
	time_zone: z.string().optional(),
	
	// Structured availability window (REQUIRED for recurring, optional for one-time)
	// Defines exactly when during each recurrence period the slot is available
	availability_window: AvailabilityWindowSchema.optional(),
	
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
	
	// Divisibility constraints (prevents over-fragmentation)
	max_natural_div: z.number().gte(1).optional(), // Max natural divisions (e.g., can't divide a person)
	max_percentage_div: PercentageSchema.optional(), // Max % divisibility (e.g., can't accept less than 10%)
	
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
	
	// Recurrence pattern
	recurrence: z.enum(['daily', 'weekly', 'monthly', 'yearly']).nullable().optional(),
	
	// Date range (for one-time needs or to bound recurring patterns)
	start_date: z.string().nullable().optional(),  // ISO date string
	end_date: z.string().nullable().optional(),    // ISO date string (for recurring, when pattern ends)
	
	// Timezone
	time_zone: z.string().optional(),
	
	// Structured availability window (REQUIRED for recurring, optional for one-time)
	// Defines exactly when the need occurs during each recurrence period
	availability_window: AvailabilityWindowSchema.optional(),
	
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
// GLOBAL RECOGNITION (PURE MODEL)
// ═══════════════════════════════════════════════════════════════════

/**
 * Global Recognition Weights
 *
 * R_A(B) = A's global recognition of B as community member
 * Constraint: Σ_i R_A(i) = 1.0
 *
 * Computed from recognition trees (see protocol.ts sharesOfGeneralFulfillmentMap)
 * Trees naturally encode type-specific preferences through their structure.
 *
 * Example:
 *   MyRecognitionTree
 *   ├─ Healthcare (70 points)
 *   │  └─ Dr. Smith contributes → 0.56 global share
 *   └─ Food (30 points)
 *      └─ Alice contributes → 0.24 global share
 *
 * Result: { dr_smith_pub: 0.56, alice_pub: 0.24, ... }
 */
export const GlobalRecognitionWeightsSchema = z.record(
	z.string(), // pubKey
	z.number().nonnegative() // global recognition weight
);

export type GlobalRecognitionWeights = z.infer<typeof GlobalRecognitionWeightsSchema>;

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
	global_damping_factor: z.number().min(0).max(1)
});

export type MultiDimensionalDamping = z.infer<typeof MultiDimensionalDampingSchema>;

// ═══════════════════════════════════════════════════════════════════
// SLOT ALLOCATION RECORD (Must be defined before Commitment)
// ═══════════════════════════════════════════════════════════════════

/**
 * Slot Allocation Record - Extended with need type
 * 
 * Represents a single allocation from a provider's capacity slot to a recipient's need slot
 * This is published in commitments for transparency
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

// ═══════════════════════════════════════════════════════════════════
// COMMITMENT SCHEMA (v4 - Multi-Dimensional)
// ═══════════════════════════════════════════════════════════════════

/**
 * Commitment - Pure Global Recognition Model
 *
 * v5 Implementation:
 * - All slots have required need_type_id for type-specific allocation
 * - Global recognition: MR(A, B) = min(R_A(B), R_B(A)) - same for all types
 * - Type preferences expressed through recognition tree structure (protocol.ts)
 * - Per-type damping factors and history
 */
export const CommitmentSchema = z.object({
	// Multi-dimensional capacity & needs (slot-native)
	capacity_slots: z.array(AvailabilitySlotSchema).optional(),
	need_slots: z.array(NeedSlotSchema).optional(),
	
	// Allocation records (what I'm giving to others from my capacity)
	// Published so recipients can see incoming allocations for transparency
	slot_allocations: z.array(SlotAllocationRecordSchema).optional(),
	
	// Global recognition: MR(A, B) = min(R_A(B), R_B(A))
	// Computed from recognition trees via sharesOfGeneralFulfillmentMap()
	// Note: .nullable() allows null from Holster (empty objects → null)
	global_recognition_weights: GlobalRecognitionWeightsSchema.nullable().optional(),
	global_mr_values: GlobalRecognitionWeightsSchema.nullable().optional(),
	
	// Causality tracking (ITC)
	itcStamp: z.any(), // ITCStampSchema
	timestamp: z.number().int().positive(),
	
	// Per-type adaptive damping (α_k)
	multi_dimensional_damping: MultiDimensionalDampingSchema.nullable().optional()
});

export type Commitment = z.infer<typeof CommitmentSchema>;

// ═══════════════════════════════════════════════════════════════════
// MULTI-DIMENSIONAL ALLOCATION STATE
// ═══════════════════════════════════════════════════════════════════

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
	
	// Per-type universalSatisfaction condition
	universalSatisfactionAchieved: z.boolean(),                              // ∀i: N_i^k(t) = 0
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
	
	// UniversalSatisfaction condition (E41' - multi-dimensional)
	universalSatisfactionAchieved: z.boolean(),                              // ∀i,k: N_i^k(t) = 0
	percentNeedsMet: z.number().min(0).max(100),              // % across all types
	
	// Freedom metric (E45' - Frobenius norm)
	freedomMetric: z.number().nonnegative(),                   // lim(t→∞) ||N⃗⃗(t)||_F
	
	// Additional distribution metrics (detect edge cases)
	maxPersonNeed: z.number().nonnegative().optional(),        // Worst-case participant ||N⃗_i||
	needVariance: z.number().nonnegative().optional(),         // Distribution inequality
	peopleStuck: z.number().int().nonnegative().optional()     // How many with unchanging needs
});

export type ConvergenceMetrics = z.infer<typeof ConvergenceMetricsSchema>;

// ═══════════════════════════════════════════════════════════════════
// ALLOCATION RESULT SCHEMA (for pure algorithm output)
// ═══════════════════════════════════════════════════════════════════

/**
 * Simplified Convergence Summary Schema
 * Used by pure algorithm - simpler than full ConvergenceMetrics
 */
export const ConvergenceSummarySchema = z.object({
	totalNeedMagnitude: z.number().nonnegative(),
	previousNeedMagnitude: z.number().nonnegative(),
	contractionRate: z.number().nonnegative(),
	isConverged: z.boolean(),
	percentNeedsMet: z.number().min(0).max(100), // % of people fully satisfied (binary)
	percentNeedReduction: z.number().min(0).max(100).optional(), // % of need magnitude reduced
	universalSatisfaction: z.boolean(),
	iterationsToConvergence: z.number().int().nullable(),
	currentIteration: z.number().int().nonnegative(),
	responseLatency: z.number().nonnegative(),
	maxPersonNeed: z.number().nonnegative().optional(),
	needVariance: z.number().nonnegative().optional(),
	peopleStuck: z.number().int().nonnegative().optional()
});

export type ConvergenceSummary = z.infer<typeof ConvergenceSummarySchema>;

/**
 * Allocation Result - Output from allocation computation
 * Contains all computed allocations, denominators, and convergence metrics
 * 
 * Note: Uses simplified ConvergenceSummary (not full ConvergenceMetrics)
 * for the pure algorithm. Can be converted to full ConvergenceMetrics if needed.
 */
export const AllocationResultSchema = z.object({
	/** Computed slot allocations */
	allocations: z.array(SlotAllocationRecordSchema),
	
	/** Denominator for each capacity slot (for transparency) */
	slotDenominators: z.record(
		z.string(),
		z.object({
			mutual: z.number().nonnegative(),
			nonMutual: z.number().nonnegative(),
			need_type_id: z.string().min(1)
		})
	),
	
	/** Total allocated by type and recipient (for tracking) */
	totalsByTypeAndRecipient: z.record(
		z.string(), // need_type_id
		z.record(z.string(), z.number().nonnegative()) // pubKey -> total
	),
	
	/** Simplified convergence summary for this iteration */
	convergence: ConvergenceSummarySchema
});

export type AllocationResult = z.infer<typeof AllocationResultSchema>;

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

/**
 * Validate and parse allocation result
 */
export function parseAllocationResult(data: unknown): AllocationResult | null {
	const result = AllocationResultSchema.safeParse(data);
	if (!result.success) {
		console.warn('[SCHEMA-V4] Invalid allocation result:', result.error);
		return null;
	}
	return result.data;
}

// ═══════════════════════════════════════════════════════════════════
// RECOGNITION HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Normalize global recognition weights to sum to 1.0
 * 
 * CRITICAL: If all weights are zero, returns empty object (no recognition)
 * This respects explicit non-recognition rather than auto-assigning equal weights
 */
export function normalizeGlobalRecognitionWeights(
	weights: GlobalRecognitionWeights
): GlobalRecognitionWeights {
	const entries = Object.entries(weights);

	if (entries.length === 0) {
		return {};
	}

	const sum = entries.reduce((acc, [_, weight]) => acc + weight, 0);

	// If sum is zero or very small, return empty (explicit non-recognition)
	// DO NOT auto-assign equal weights - zero means zero!
	if (sum < 0.0001) {
		console.warn('[NORMALIZE] All recognition weights are zero or near-zero - treating as no recognition');
		return {}; // Respect explicit non-recognition
	}

	// Normalize to sum to 1.0
	const normalized: GlobalRecognitionWeights = {};
	for (const [key, weight] of entries) {
		normalized[key] = weight / sum;
	}

	return normalized;
}

/**
 * Validate that recognition weights sum to 1.0 (within epsilon)
 */
export function validateGlobalRecognitionWeights(
	weights: GlobalRecognitionWeights,
	epsilon: number = 0.001
): boolean {
	const sum = Object.values(weights).reduce((a, b) => a + b, 0);
	return Math.abs(sum - 1.0) < epsilon;
}

// ═══════════════════════════════════════════════════════════════════
// CONTACTS SCHEMA (Legacy Compatibility)
// ═══════════════════════════════════════════════════════════════════

/**
 * Contact - Represents a user contact
 */
export const ContactSchema = z.object({
	contact_id: z.string(),
	name: z.string(),
	public_key: z.string().optional(),
	emoji: z.string().optional(),
	notes: z.string().optional(),
	created_at: z.number().optional(),  // Gun timestamp
	updated_at: z.number().optional(),  // Gun timestamp
	_updatedAt: z.number().optional()   // Holster timestamp
});

export type Contact = z.infer<typeof ContactSchema>;

/**
 * Contacts Collection - Map of contact_id to Contact
 */
export const ContactsCollectionSchema = z.record(z.string(), ContactSchema);
export type ContactsCollection = z.infer<typeof ContactsCollectionSchema>;
export type ContactsCollectionData = ContactsCollection;  // Alias for compatibility

// ═══════════════════════════════════════════════════════════════════
// CHAT SCHEMA (Legacy Compatibility)
// ═══════════════════════════════════════════════════════════════════

/**
 * Chat Read State - Tracks read status for a chat with another user
 */
export const ChatReadStateSchema = z.object({
	lastRead: z.number().int().positive(),
	lastReadTimestamp: z.number().int().positive().optional(),  // Alias for Gun compatibility
	updatedAt: z.number().optional(),   // Gun timestamp
	_updatedAt: z.number().optional()   // Holster timestamp
});

export type ChatReadState = z.infer<typeof ChatReadStateSchema>;

/**
 * Chat Read States - Map of user public key to read state
 */
export const ChatReadStatesSchema = z.record(z.string(), ChatReadStateSchema);
export type ChatReadStates = z.infer<typeof ChatReadStatesSchema>;

// ═══════════════════════════════════════════════════════════════════
// LEGACY TYPE ALIASES (For backward compatibility during migration)
// ═══════════════════════════════════════════════════════════════════

/**
 * Legacy type aliases - Components use Commitment, but some state files
 * may still reference these old names during migration
 */
export type BaseCapacity = Commitment;
export type ProviderCapacity = Commitment & { id?: string };  // Some legacy code expects id
export type RecipientCapacity = Commitment;
export type CapacitiesCollection = Record<string, Commitment>;


