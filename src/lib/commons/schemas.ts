/**
 * Allocation Schemas v2 - ITC & Event-Driven Architecture
 * 
 * Key Changes from v1:
 * - ITC stamps replace vector clocks (O(log n) vs O(n) space)
 * - No rounds (event-driven, not round-based)
 * - Time-based damping history (not round-indexed)
 * - Convergence tracking with continuous monitoring
 * 
 * Using Zod v4 for:
 * - Type safety with runtime validation
 * - Automatic TypeScript type inference
 * - Data validation at persistence boundaries
 */

import * as z from 'zod';

// ═══════════════════════════════════════════════════════════════════
// CORE SCHEMAS
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

// ═══════════════════════════════════════════════════════════════════
// RESOURCE & SLOT SCHEMAS
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

/**
 * Availability Slot - Time/location/quantity of available capacity
 */
export const AvailabilitySlotSchema = ResourceMetadataSchema.extend({
	id: IdSchema,
	quantity: z.number().gte(0),
	
	// Timing constraints
	advance_notice_hours: z.optional(z.number().gte(0)),
	booking_window_hours: z.optional(z.number().gte(0)),
	
	// Time pattern
	all_day: z.optional(z.boolean()),
	recurrence: z.optional(z.nullable(z.string())),
	custom_recurrence_repeat_every: z.optional(z.nullable(z.number())),
	custom_recurrence_repeat_unit: z.optional(z.nullable(z.string())),
	custom_recurrence_end_type: z.optional(z.nullable(z.string())),
	custom_recurrence_end_value: z.optional(z.nullable(z.string())),
	start_date: z.optional(z.nullable(z.string())),
	start_time: z.optional(z.nullable(z.string())),
	end_date: z.optional(z.nullable(z.string())),
	end_time: z.optional(z.nullable(z.string())),
	time_zone: z.optional(z.string()),
	
	// Location
	location_type: z.optional(z.string()),
	longitude: z.optional(z.number().min(-180).max(180)),
	latitude: z.optional(z.number().min(-90).max(90)),
	street_address: z.optional(z.string()),
	city: z.optional(z.string()),
	state_province: z.optional(z.string()),
	postal_code: z.optional(z.string()),
	country: z.optional(z.string()),
	online_link: z.optional(z.string().url().or(z.string().length(0))),
	
	// Hierarchical & coordination
	parent_slot_id: z.optional(IdSchema),
	mutual_agreement_required: z.optional(z.boolean().default(false)),
	priority: z.optional(z.number())
});

/**
 * Need Slot - Time/location/quantity of needed capacity
 */
export const NeedSlotSchema = ResourceMetadataSchema.extend({
	id: IdSchema,
	quantity: z.number().gte(0),
	
	// Timing constraints
	advance_notice_hours: z.optional(z.number().gte(0)),
	booking_window_hours: z.optional(z.number().gte(0)),
	
	// Time pattern
	all_day: z.optional(z.boolean()),
	recurrence: z.optional(z.nullable(z.string())),
	custom_recurrence_repeat_every: z.optional(z.nullable(z.number())),
	custom_recurrence_repeat_unit: z.optional(z.nullable(z.string())),
	custom_recurrence_end_type: z.optional(z.nullable(z.string())),
	custom_recurrence_end_value: z.optional(z.nullable(z.string())),
	start_date: z.optional(z.nullable(z.string())),
	start_time: z.optional(z.nullable(z.string())),
	end_date: z.optional(z.nullable(z.string())),
	end_time: z.optional(z.nullable(z.string())),
	time_zone: z.optional(z.string()),
	
	// Location
	location_type: z.optional(z.string()),
	longitude: z.optional(z.number().min(-180).max(180)),
	latitude: z.optional(z.number().min(-90).max(90)),
	street_address: z.optional(z.string()),
	city: z.optional(z.string()),
	state_province: z.optional(z.string()),
	postal_code: z.optional(z.string()),
	country: z.optional(z.string()),
	online_link: z.optional(z.string().url().or(z.string().length(0))),
	
	// Hierarchical & coordination
	parent_slot_id: z.optional(IdSchema),
	mutual_agreement_required: z.optional(z.boolean().default(false)),
	priority: z.optional(z.number())
});

// ═══════════════════════════════════════════════════════════════════
// BASE CAPACITY & NEED SCHEMAS
// ═══════════════════════════════════════════════════════════════════

/**
 * Base Capacity - A participant's available resources with slots
 * Extends ResourceMetadataSchema for consistent resource description
 */
export const BaseCapacitySchema = ResourceMetadataSchema.extend({
	id: IdSchema,
	availability_slots: z.array(AvailabilitySlotSchema),
	owner_id: z.optional(IdSchema),
	provider_id: z.optional(IdSchema)
});

/**
 * Base Need - A participant's resource needs with slots
 * Extends ResourceMetadataSchema for consistent resource description
 */
export const BaseNeedSchema = ResourceMetadataSchema.extend({
	id: IdSchema,
	need_slots: z.array(NeedSlotSchema),
	declarer_id: IdSchema,
	status: z.enum(['open', 'partially-fulfilled', 'fulfilled']).default('open'),
	fulfilled_amount: z.number().gte(0).default(0),
	tags: z.array(z.string()).optional()
});

// ═══════════════════════════════════════════════════════════════════
// TREE SCHEMAS (for recognition and priority trees)
// ═══════════════════════════════════════════════════════════════════

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
 */
export const NonRootNodeSchema = z.object({
	id: IdSchema,
	name: NameSchema,
	type: z.literal('NonRootNode'),
	manual_fulfillment: z.nullable(z.number()),
	children: z.array(z.any()), // Recursive reference
	points: PointsSchema,
	parent_id: IdSchema,
	contributor_ids: z.array(IdSchema),
	anti_contributors_ids: z.array(IdSchema),
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

// ═══════════════════════════════════════════════════════════════════
// COMMITMENT SCHEMA (v2 - ITC & Event-Driven)
// ═══════════════════════════════════════════════════════════════════

/**
 * Damping History Entry - Time-based (not round-based)
 */
export const DampingHistoryEntrySchema = z.object({
	overAllocation: z.number(),
	timestamp: z.number().int().positive()
});

/**
 * Commitment - Participant's declaration
 * 
 * v2 Changes:
 * - itcStamp replaces vectorClock
 * - No round field (event-driven)
 * - damping_history is time-stamped
 */
export const CommitmentSchema = z.object({
	// Capacity & needs (slot-native)
	capacity_slots: z.array(AvailabilitySlotSchema).optional(),
	need_slots: z.array(NeedSlotSchema).optional(),
	
	// Recognition
	recognition_weights: z.record(z.string(), z.number().nonnegative()).optional(),
	mr_values: z.record(z.string(), z.number().nonnegative()).optional(),
	
	// Causality tracking (ITC - v2)
	itcStamp: ITCStampSchema,
	timestamp: z.number().int().positive(),
	
	// Adaptive damping (time-based - v2)
	damping_factor: z.number().min(0).max(1).default(1.0),
	damping_history: z.array(DampingHistoryEntrySchema).optional(),
	
	// Legacy (deprecated - for migration)
	over_allocation_history: z.array(z.number().nonnegative()).optional()
});

// ═══════════════════════════════════════════════════════════════════
// ALLOCATION STATE SCHEMA (v2 - ITC & Convergence)
// ═══════════════════════════════════════════════════════════════════

/**
 * Slot Allocation Record - Single slot-to-recipient allocation
 */
export const SlotAllocationRecordSchema = z.object({
	availability_slot_id: IdSchema,
	recipient_pubkey: z.string(),
	recipient_need_slot_id: IdSchema.optional(),
	quantity: z.number().nonnegative(),
	time_compatible: z.boolean(),
	location_compatible: z.boolean(),
	tier: z.enum(['mutual', 'non-mutual'])
});

/**
 * Convergence History Entry - Continuous monitoring (not round-based)
 */
export const ConvergenceHistoryEntrySchema = z.object({
	denominatorDelta: z.number(),
	timestamp: z.number().int().positive()
});

/**
 * Two-Tier Allocation State - Slot-native allocations
 * 
 * v2 Changes:
 * - converged flag (continuous monitoring)
 * - convergenceHistory with timestamps
 * - itcStamp replaces vectorClock
 * - No round field
 */
export const TwoTierAllocationStateSchema = z.object({
	// Slot-level allocations
	slot_denominators: z.record(
		IdSchema,
		z.object({
			mutual: z.number().nonnegative(),
			nonMutual: z.number().nonnegative()
		})
	),
	slot_allocations: z.array(SlotAllocationRecordSchema),
	recipient_totals: z.record(z.string(), z.number().nonnegative()),
	
	// Convergence tracking (v2)
	converged: z.boolean().optional(),
	convergenceHistory: z.array(ConvergenceHistoryEntrySchema).optional(),
	
	// Causality tracking (ITC - v2)
	itcStamp: ITCStampSchema.optional(),
	timestamp: z.number().int().positive()
});

// ═══════════════════════════════════════════════════════════════════
// TYPE EXPORTS
// ═══════════════════════════════════════════════════════════════════

// Core types
export type ShareMap = z.infer<typeof ShareMapSchema>;

// ITC types
export type ITCId = z.infer<typeof ITCIdSchema>;
export type ITCEvent = z.infer<typeof ITCEventSchema>;
export type ITCStamp = z.infer<typeof ITCStampSchema>;

// Resource & slot types
export type ResourceMetadata = z.infer<typeof ResourceMetadataSchema>;
export type AvailabilitySlot = z.infer<typeof AvailabilitySlotSchema>;
export type NeedSlot = z.infer<typeof NeedSlotSchema>;

// Capacity & need types
export type BaseCapacity = z.infer<typeof BaseCapacitySchema>;
export type BaseNeed = z.infer<typeof BaseNeedSchema>;

// Tree types
export type NodeDataStorage = z.infer<typeof NodeDataStorageSchema>;
export type NonRootNode = z.infer<typeof NonRootNodeSchema>;
export type RootNode = z.infer<typeof RootNodeSchema>;
export type Node = z.infer<typeof NodeSchema>;

// Commitment types
export type DampingHistoryEntry = z.infer<typeof DampingHistoryEntrySchema>;
export type Commitment = z.infer<typeof CommitmentSchema>;

// Allocation state types
export type SlotAllocationRecord = z.infer<typeof SlotAllocationRecordSchema>;
export type ConvergenceHistoryEntry = z.infer<typeof ConvergenceHistoryEntrySchema>;
export type TwoTierAllocationState = z.infer<typeof TwoTierAllocationStateSchema>;

// ═══════════════════════════════════════════════════════════════════
// VALIDATION HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Validate and parse commitment from network
 */
export function parseCommitment(data: unknown): Commitment | null {
	const result = CommitmentSchema.safeParse(data);
	if (!result.success) {
		console.warn('[SCHEMA-V2] Invalid commitment:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Validate and parse allocation state from network
 */
export function parseAllocationState(data: unknown): TwoTierAllocationState | null {
	const result = TwoTierAllocationStateSchema.safeParse(data);
	if (!result.success) {
		console.warn('[SCHEMA-V2] Invalid allocation state:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Validate ITC stamp
 */
export function parseITCStamp(data: unknown): ITCStamp | null {
	const result = ITCStampSchema.safeParse(data);
	if (!result.success) {
		console.warn('[SCHEMA-V2] Invalid ITC stamp:', result.error);
		return null;
	}
	return result.data;
}

