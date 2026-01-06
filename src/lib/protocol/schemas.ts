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
 * ITC (Interval Tree Clocks) Types
 * 
 * We use the actual ITC types and a passthrough schema for validation.
 * This avoids type mismatches between Zod's inferred types and the real ITC types.
 */
import type { Id as ITCId, Event as ITCEvent, Stamp as ITCStamp } from './itc.js';

// Use passthrough schemas that accept the ITC types directly
export const ITCIdSchema = z.any() as z.ZodType<ITCId>;
export const ITCEventSchema = z.any() as z.ZodType<ITCEvent>;
export const ITCStampSchema = z.any() as z.ZodType<ITCStamp>;

// Re-export the actual ITC types
export type { ITCId, ITCEvent, ITCStamp };

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
 * Contributor - Represents a person or organization who contributed to a node
 * 
 * The ID can be:
 * - Public key (base64 string) - individual contributor
 * - contact_id (string starting with "contact_") - named contact
 * - org_id (string starting with "org_") - organization contributor
 * 
 * Points determine the contributor's share of recognition from this node.
 * If a node has contributors [Alice: 50pts, Bob: 30pts], Alice gets 50/80 = 62.5% of the node's recognition.
 * 
 * This is analogous to how child node points work, but for contributors instead of subtasks.
 */
export const ContributorSchema = z.object({
	id: IdSchema, // Can be pubkey, contact_id, or org_id
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
 * Base Slot Schema - Common fields for both capacity and need slots
 * 
 * This schema contains all fields shared between AvailabilitySlot and NeedSlot.
 * Using Zod v4's composition features to eliminate duplication.
 */
export const BaseSlotSchema = z.object({
	// Core identification
	id: z.string().min(1),
	quantity: z.number().gte(0),
	type_id: z.string().min(1), // k in C_j^k(t) or N_i^k(t)

	// Divisibility constraints (prevents over-fragmentation)
	max_natural_div: z.number().gte(1).optional(), // Max natural divisions (e.g., can't divide a person)
	min_allocation_percentage: PercentageSchema.optional(), // Min % per allocation (e.g., don't allocate less than 10%)

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
	priority: z.number().optional(),

	// Collective capacity/need: Who can provide/needs this (pubkeys or org_ids)
	// Empty/undefined = just me, populated = collective effort/need
	// Supports org_ids (e.g., "org_abc123") which resolve to member lists recursively
	members: z.array(z.string()).optional(),

	// Priority distribution for slot-based alloc (Specific to this slot)
	// Person-to-person recognition weights specific to THIS slot
	// Overrides global_recognition_weights for this slot if specified
	// Same structure as global_recognition_weights: Record<pubkey, priority>
	priority_distribution: z.record(z.string(), z.number().min(0).max(1)).optional()
});

export type BaseSlot = z.infer<typeof BaseSlotSchema>;

/**
 * Availability Slot - Multi-Dimensional (Pure)
 * Each slot MUST specify which need type k it provides
 * 
 * Extends BaseSlotSchema with all common slot fields.
 */
export const AvailabilitySlotSchema = BaseSlotSchema;

export type AvailabilitySlot = z.infer<typeof AvailabilitySlotSchema>;

/**
 * Need Slot - Multi-Dimensional (Pure)
 * Each slot MUST specify which need type k it requests
 * 
 * Extends BaseSlotSchema with all common slot fields.
 */
export const NeedSlotSchema = BaseSlotSchema;

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
// N-TIER DISTRIBUTION SUPPORT
// ═══════════════════════════════════════════════════════════════════

/**
 * Tier Definition for N-Tier Distributions
 * 
 * Enables arbitrary number of allocation tiers with configurable priority ordering.
 * Lower priority numbers are allocated first (priority 0 = highest priority).
 * 
 * Example:
 * [
 *   { priority: 0, shares: { alice: 0.6, bob: 0.4 }, label: 'mutual-recognition' },
 *   { priority: 1, shares: { carol: 0.7, dave: 0.3 }, label: 'non-mutual-recognition' },
 *   { priority: 2, shares: { eve: 1.0 }, label: 'fallback' }
 * ]
 */
export const TierDefinitionSchema = z.object({
	/** Priority determines allocation order (lower = higher priority, allocated first) */
	priority: z.number().int().nonnegative(),

	/** Recipient shares within this tier (normalized to sum ≤ 1.0) */
	shares: z.record(z.string(), z.number().nonnegative()),

	/** Optional label for this tier (e.g., 'mutual-recognition', 'community-members') */
	label: z.string().optional()
});

export type TierDefinition = z.infer<typeof TierDefinitionSchema>;

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
	provider_pubkey: z.string().optional(),
	recipient_need_slot_id: z.string().optional(),
	quantity: z.number().nonnegative(),

	// MULTI-DIMENSIONAL: Type information
	type_id: z.string().min(1),

	// Compatibility
	time_compatible: z.boolean(),
	location_compatible: z.boolean(),
	// Distributed IPF Metadata
	withinPriorityLimit: z.boolean(),
	fromSurplus: z.boolean(),

	// Deprecated (Keep for backward compatibility if needed, but making optional/nullable)
	tier: z.union([z.number().int().nonnegative(), z.string()]).optional(),

	// DISTRIBUTED IPF: Priority Score (K_pr)
	// Persisted so recipients can reconstruct accurate Total Seed values
	seed_value: z.number().nonnegative().optional()
});

export type SlotAllocationRecord = z.infer<typeof SlotAllocationRecordSchema>;

// ═══════════════════════════════════════════════════════════════════
// COMMITMENT SCHEMA (v4 - Multi-Dimensional)
// ═══════════════════════════════════════════════════════════════════

/**
 * Commitment - Pure Global Recognition Model
 *
 * v5 Implementation:
 * - All slots have required type_id for type-specific allocation
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

	// LOCAL CACHE: Others' full recognition weights (cached from network commitments)
	// Updated when network proves otherwise (local-first!)
	// Format: { theirPubKey: { allPubKeys: weights, ... } }
	// We store their FULL weights, then extract their recognition of me when computing MR
	others_recognition_of_me: z.record(
		z.string(), // theirPubKey
		GlobalRecognitionWeightsSchema // Their full recognition weights (normalized)
	).nullable().optional(),

	// Distance-based allocation tracking
	// Total allocations received FROM EACH PROVIDER
	// Format: { type_id: { provider_pubkey: quantity } }
	// This allows providers to see what the recipient is receiving from OTHER providers
	// without needing to subscribe to those providers' commitments
	total_allocated: z.record(
		z.string(), // type_id
		z.record(z.string(), z.number().nonnegative()) // provider_pubkey → quantity
	).optional(),

	// Distance from need (can be negative for over-allocation)
	// Formula: distance = declared_need - sum(total_allocated[type_id].values())
	// Positive = under-allocated, Negative = over-allocated, Zero = perfect
	// Format: { type_id: distance }
	distance_from_need: z.record(z.string(), z.number()).optional(),

	// Causality tracking (ITC)
	itcStamp: z.any(), // ITCStampSchema
	timestamp: z.number().int().positive(),

	// DISTRIBUTED IPF: Constraint Factors (y_r)
	// Published by recipients to signal saturation level to providers.
	// Map: need_slot_id -> scaling_factor (0.0 to 1.0)
	// y_r = 1.0 (Under-saturated, send full proposal)
	// y_r < 1.0 (Over-saturated, scale down proposal)
	constraint_scaling_factors: z.record(z.string(), z.number().min(0).max(1)).optional(),

	// DISTRIBUTED IPF: Competition Metrics (total_seed)
	// Published by recipients to signal competition level to providers.
	// Map: need_slot_id -> total_seed (sum of all K_pr from proposals)
	// Allows providers to calculate fair share: (my_K_pr / total_seed) × need
	// Enables priority-aware allocation without provider-to-provider subscriptions
	total_seed_by_need: z.record(z.string(), z.number().nonnegative()).optional()
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
	z.string(), // type_id
	z.record(z.string(), z.number().nonnegative()) // pubKey -> total received
);

export type PerTypeAllocationTotals = z.infer<typeof PerTypeAllocationTotalsSchema>;



// ═══════════════════════════════════════════════════════════════════
// MULTI-DIMENSIONAL STATE VECTORS
// ═══════════════════════════════════════════════════════════════════

/**
 * Need State Per Type (D3')
 * N_i^k(t) ∈ [0, N_i^k_max]
 */
export const PerTypeNeedStateSchema = z.object({
	type_id: z.string().min(1),
	residualNeed: z.number().nonnegative(),    // N_i^k(t)
	maxNeed: z.number().nonnegative(),          // N_i^k_max
	lastAllocationReceived: z.number().nonnegative().default(0)
});

export type PerTypeNeedState = z.infer<typeof PerTypeNeedStateSchema>;





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
			type_id: z.string().min(1)
		})
	),

	/** Total allocated by type and recipient (for tracking) */
	totalsByTypeAndRecipient: z.record(
		z.string(), // type_id
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
// CONTACT SCHEMA (Simple Address Book)
// ═══════════════════════════════════════════════════════════════════

/**
 * Contact Schema - Person entities
 * 
 * Contacts are UUIDs for people that include:
 * - contact_id: UUID identifier (can be shared across network)
 * - name: Display name
 * - public_key: Optional link to network identity (pubkey)
 * - emoji: Optional visual identifier
 * - notes: Optional notes
 * 
 * Everything else (skills, location, email, etc.) is stored as attributes
 * in the attribute recognition system.
 */
export const ContactSchema = z.object({
	contact_id: z.string(),
	name: z.string(),
	public_key: z.string().optional(),
	emoji: z.string().optional(),
	notes: z.string().optional(),
	created_at: z.number().optional(),
	updated_at: z.number().optional(),
	_updatedAt: z.number().optional()
});

export type Contact = z.infer<typeof ContactSchema>;

export const ContactsCollectionSchema = z.preprocess(
	(data: any) => {
		if (data && typeof data === 'object') {
			const { _updatedAt, ...rest } = data;
			return rest;
		}
		return data;
	},
	z.record(z.string(), ContactSchema)
);

export type ContactsCollectionData = z.infer<typeof ContactsCollectionSchema>;

// ═══════════════════════════════════════════════════════════════════
// ORGANIZATION SCHEMA (Simple Entity with Names)
// ═══════════════════════════════════════════════════════════════════

/**
 * Organization Schema - Group entities
 * 
 * Organizations are UUIDs for groups/collectives that include:
 * - org_id: UUID identifier (can be shared across network)
 * - names: Multi-language names (e.g., {en: "Community Garden", es: "Jardín Comunitario"})
 * - emoji: Optional visual identifier
 * - description: Optional description
 * 
 * Everything else (membership, location, website, etc.) is stored as attributes
 * in the attribute recognition system.
 */
export const OrganizationSchema = z.object({
	org_id: z.string(),
	names: z.record(z.string(), z.string()),
	emoji: z.string().optional(),
	description: z.string().optional(),
	created_at: z.number().optional(),
	updated_at: z.number().optional(),
	_updatedAt: z.number().optional()
});

export type Organization = z.infer<typeof OrganizationSchema>;

export const OrganizationsCollectionSchema = z.record(z.string(), OrganizationSchema);

export type OrganizationsCollection = z.infer<typeof OrganizationsCollectionSchema>;


// ═══════════════════════════════════════════════════════════════════
// ATTRIBUTE TYPE SCHEMAS
// ═══════════════════════════════════════════════════════════════════

/**
 * Membership List - Array of member identifiers
 * Used for organization membership, collective capacities, etc.
 */
export const MembershipListSchema = z.array(z.string().min(1));
export type MembershipList = z.infer<typeof MembershipListSchema>;

/**
 * Skill Value - Represents proficiency in a skill
 */
export const SkillValueSchema = z.object({
	level: z.number().int().min(1).max(10), // 1-10 proficiency scale
	years: z.number().nonnegative().optional(), // Years of experience
	description: z.string().optional(),
	verified: z.boolean().default(false),
	endorsements: z.array(z.string()).default([]) // Pubkeys who endorse this skill
});
export type SkillValue = z.infer<typeof SkillValueSchema>;

/**
 * Location Value - Represents a physical or online location
 */
export const LocationValueSchema = z.object({
	city: z.string().optional(),
	state_province: z.string().optional(),
	country: z.string().optional(),
	coords: z.tuple([
		z.number().min(-90).max(90),  // latitude
		z.number().min(-180).max(180) // longitude
	]).optional(),
	postal_code: z.string().optional(),
	street_address: z.string().optional(),
	online: z.boolean().optional() // True if remote/online
});
export type LocationValue = z.infer<typeof LocationValueSchema>;

// ═══════════════════════════════════════════════════════════════════
// ATTRIBUTE RECOGNITION SYSTEM
// ═══════════════════════════════════════════════════════════════════

/**
 * Attribute Value - Single attribute recognition with metadata
 * 
 * Stores any attribute of any entity with provenance tracking and ITC causality.
 * Subscription data is written INTO this structure with ITC conflict resolution.
 * 
 * Resolution Logic (via source_pubkey presence):
 * - source_pubkey present + matches subscription config → came from subscription
 * - source_pubkey present + matches entity_id → self-declaration
 * - source_pubkey absent (undefined) → our local recognition
 * 
 * Examples:
 * - Subscribed: { value: ["pubkey1", "pubkey2"], source_pubkey: "pubkey_alice", ... }
 * - Self: { value: [...], source_pubkey: "pubkey_bob", ... } (where entity is pubkey_bob)
 * - Local: { value: [...], source_pubkey: undefined, ... }
 */
export const AttributeValueSchema = z.object({
	value: z.any(), // Flexible - array, object, primitive
	source_pubkey: z.string().optional(), // Who declared this (undefined = our local recognition)
	confidence: z.number().min(0).max(1).default(1.0), // 0-1, default 1.0
	timestamp: z.number().int().positive(),
	itcStamp: ITCStampSchema.optional() // ITC causality tracking per attribute
});

export type AttributeValue = z.infer<typeof AttributeValueSchema>;

/**
 * Attribute Recognitions Collection - User's attribute recognitions storage
 * 
 * UNIFIED STORAGE: Contains BOTH local recognitions AND subscribed data!
 * Uses ITC conflict resolution (like others_recognition_of_me in stores.svelte.ts).
 * 
 * Nested structure: entity_id → attribute_name → AttributeValue
 * Includes collection-level ITC for causality tracking across all attributes.
 * 
 * When subscription data arrives:
 * - ITC check: Skip if causally stale
 * - ITC merge: Join stamps if concurrent
 * - Write with source_pubkey set to source
 * 
 * Manual edits win if causally newer (ITC handles this)!
 * 
 * Example:
 * {
 *   "org_abc123": {
 *     "membership": { 
 *       value: ["pubkey1", "pubkey2"], 
 *       source_pubkey: "pubkey_alice",  // ← From subscription
 *       itcStamp: {...}
 *     }
 *   },
 *   "pubkey_bob": {
 *     "need:housing": { 
 *       value: {...}, 
 *       source_pubkey: undefined,  // ← Our local recognition
 *       itcStamp: {...}
 *     }
 *   },
 *   _itcStamp: {...},
 *   _timestamp: 1234567890
 * }
 */
export const AttributeRecognitionsCollectionSchema = z.object({
	// Entity attributes (dynamic keys)
}).catchall(
	z.union([
		z.record(z.string(), AttributeValueSchema), // entity_id → attributes
		ITCStampSchema, // _itcStamp
		z.number().int().positive() // _timestamp
	])
).and(z.object({
	_itcStamp: ITCStampSchema.optional(), // Collection-level ITC
	_timestamp: z.number().int().positive().optional() // Collection-level timestamp
}));

export type AttributeRecognitionsCollection = z.infer<typeof AttributeRecognitionsCollectionSchema>;

/**
 * Attribute Subscriptions - Source mappings for attribute resolution
 * 
 * Maps entity_id → attribute_name → source_pubkey
 * Specifies where to subscribe for each entity's attributes.
 * 
 * Resolution priority (2b): specified_source → entity's_pubkey → our_manual_override
 * 
 * Example:
 * {
 *   "org_abc123": {
 *     "membership": "pubkey_alice"  // Subscribe to Alice's view of org membership
 *   },
 *   "pubkey_bob": {
 *     "capacity:food": "pubkey_bob"  // Subscribe to Bob's own capacity declaration
 *   }
 * }
 */
export const AttributeSubscriptionsSchema = z.record(
	z.string(), // entity_id
	z.record(z.string(), z.string()) // attribute_name → source_pubkey
);

export type AttributeSubscriptions = z.infer<typeof AttributeSubscriptionsSchema>;

/**
 * Entity ID Mappings - UUID/contact_id to pubkey resolution
 * 
 * Optional mapping for resolving local identifiers to public keys.
 * Enables using friendly names/UUIDs locally while publishing to network with pubkeys.
 * 
 * Example:
 * {
 *   "contact_alice_123": "pubkey_abc...",
 *   "uuid_def_456": "pubkey_xyz..."
 * }
 */
export const EntityIdMappingsSchema = z.record(
	z.string(), // uuid or contact_id
	z.string() // resolved pubkey
);

export type EntityIdMappings = z.infer<typeof EntityIdMappingsSchema>;

// ═══════════════════════════════════════════════════════════════════
// UNIFIED CORE CONCEPTS
// ═══════════════════════════════════════════════════════════════════

/**
 * Members - Unified array of identifiers
 * 
 * Used across multiple contexts:
 * - Organization membership lists
 * - Slot members (collective capacities/needs)
 * - Filter conditions (who to include)
 * 
 * Can contain:
 * - Public keys (base64 strings) - individual members
 * - org_ids (strings starting with "org_") - organizations (recursive!)
 * - contact_ids (strings starting with "contact_") - contacts
 */
export const MembersSchema = z.array(z.string());
export type Members = z.infer<typeof MembersSchema>;

// ═══════════════════════════════════════════════════════════════════
// UNIFIED SLOT FILTER & SUBSCRIPTION SCHEMAS
// ═══════════════════════════════════════════════════════════════════

/**
 * Slot Filter - Unified filter for capacity AND need slots
 * 
 * Single filter can apply to capacity slots, need slots, or both!
 * Multiple filters are combined with OR (union) - match ANY enabled filter.
 * 
 * Examples:
 * - Filter for capacities that include me: { applies_to: 'capacity', must_include_me: true }
 * - Filter for food needs from Alice: { applies_to: 'need', source_pubkeys: ['alice...'], type_ids: ['food'] }
 * - Filter for any slots with org involvement: { applies_to: 'both', must_include_ids: ['org_abc123'] }
 */
export const SlotFilterSchema = z.object({
	filter_id: z.string(),
	name: z.string(), // Human-readable name
	enabled: z.boolean().default(true),

	// NEW: What type of slots this filter applies to
	applies_to: z.enum(['capacity', 'need', 'both']).default('both'),

	// Filter conditions (all optional, undefined = don't filter on this)
	source_pubkeys: z.array(z.string()).optional(), // Only from these users
	type_ids: z.array(z.string()).optional(), // Only these types
	must_include_me: z.boolean().optional(), // Only if I'm in members
	must_include_ids: MembersSchema.optional(), // Only if includes these IDs (pubkeys, orgs, contacts) - UNIFIED!
	location_max_distance_km: z.number().optional(), // Max distance from my location
	min_quantity: z.number().optional(), // Minimum quantity

	created_at: z.number().optional(),
	updated_at: z.number().optional()
});

export type SlotFilter = z.infer<typeof SlotFilterSchema>;

/**
 * Slot Filters Collection - Map of filter_id to SlotFilter
 */
export const SlotFiltersCollectionSchema = z.record(z.string(), SlotFilterSchema);
export type SlotFiltersCollection = z.infer<typeof SlotFiltersCollectionSchema>;

/**
 * Slot Subscriptions - Unified subscriptions for capacity AND need slots
 * 
 * Maps pubkey → what to subscribe to from that user
 * Can subscribe to their capacity, needs, or both!
 * 
 * Examples:
 * - Subscribe to Alice's capacities only: { 'alice_pub': { capacity: true, needs: false } }
 * - Subscribe to Bob's needs only: { 'bob_pub': { capacity: false, needs: true } }
 * - Subscribe to Carol's both: { 'carol_pub': { capacity: true, needs: true } }
 */
export const SlotSubscriptionsSchema = z.record(
	z.string(), // pubkey
	z.object({
		capacity: z.boolean().default(false),
		needs: z.boolean().default(false)
	})
);
export type SlotSubscriptions = z.infer<typeof SlotSubscriptionsSchema>;


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


