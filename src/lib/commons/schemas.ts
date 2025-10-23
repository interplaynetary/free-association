/**
 * Zod Schemas for Mutual-Priority Allocation Algorithm
 * 
 * Using Zod v4 for:
 * - Type safety with runtime validation
 * - Automatic TypeScript type inference
 * - Data validation at persistence boundaries
 * - Schema-driven Holster integration
 */

import * as z from 'zod';

// ═══════════════════════════════════════════════════════════════════
// CORE SCHEMAS
// ═══════════════════════════════════════════════════════════════════

// Basic types
export const IdSchema = z.string().min(1);
export const NameSchema = z.string().min(1);
export const PercentageSchema = z.number().gte(0).lte(1);

/**
 * Vector Clock for decentralized coordination
 * Maps pubKey → logical timestamp
 */
export const VectorClockSchema = z.record(z.string(), z.number());

/**
 * Round State for gossip-based coordination
 */
export const RoundStateSchema = z.object({
	pubKey: z.string(),
	round: z.number().int().nonnegative(),
	vectorClock: VectorClockSchema,
	timestamp: z.number().int().positive()
});

/**
 * Capacity Filter for location/skill-based constraints
 */
export const CapacityFilterSchema = z.object({
	filter_fn: z.string(), // Serialized filter function or criteria
	required_attributes: z.record(z.string(), z.unknown()).optional()
});

// ═══════════════════════════════════════════════════════════════════
// SLOT SCHEMAS (for capacity and need matching)
// ═══════════════════════════════════════════════════════════════════

/**
 * Availability Slot - Represents a specific time/location/quantity of available capacity
 */
export const AvailabilitySlotSchema = z.object({
	id: IdSchema, // Unique identifier for this slot
	quantity: z.number().gte(0),

	// Timing constraints
	advance_notice_hours: z.optional(z.number().gte(0)),
	booking_window_hours: z.optional(z.number().gte(0)),

	// Time pattern fields
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

	// Location fields
	location_type: z.optional(z.string()),
	longitude: z.optional(z.number().min(-180).max(180)),
	latitude: z.optional(z.number().min(-90).max(90)),
	street_address: z.optional(z.string()),
	city: z.optional(z.string()),
	state_province: z.optional(z.string()),
	postal_code: z.optional(z.string()),
	country: z.optional(z.string()),
	online_link: z.optional(z.string().url().or(z.string().length(0))),

	// Hierarchical relationship for subset allocation
	parent_slot_id: z.optional(IdSchema),

	// Mutual agreement for coordination
	mutual_agreement_required: z.optional(z.boolean().default(false)),

	// Slot-specific metadata
	priority: z.optional(z.number())
});

/**
 * Need Slot - Represents a specific time/location/quantity of needed capacity
 */
export const NeedSlotSchema = z.object({
	id: IdSchema, // Unique identifier for this slot
	quantity: z.number().gte(0),

	// Timing constraints
	advance_notice_hours: z.optional(z.number().gte(0)),
	booking_window_hours: z.optional(z.number().gte(0)),

	// Time pattern fields
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

	// Location fields
	location_type: z.optional(z.string()),
	longitude: z.optional(z.number().min(-180).max(180)),
	latitude: z.optional(z.number().min(-90).max(90)),
	street_address: z.optional(z.string()),
	city: z.optional(z.string()),
	state_province: z.optional(z.string()),
	postal_code: z.optional(z.string()),
	country: z.optional(z.string()),
	online_link: z.optional(z.string().url().or(z.string().length(0))),

	// Hierarchical relationship for subset needs
	parent_slot_id: z.optional(IdSchema),

	// Mutual agreement for coordination
	mutual_agreement_required: z.optional(z.boolean().default(false)),

	// Slot-specific metadata
	priority: z.optional(z.number())
});

/**
 * Base Capacity - A participant's available resources with slots
 */
export const BaseCapacitySchema = z.object({
	id: IdSchema,
	name: z.string(),
	emoji: z.optional(z.string()),
	unit: z.optional(z.string()),
	description: z.optional(z.string()),
	
	// Multiple availability slots
	availability_slots: z.array(AvailabilitySlotSchema),
	
	// Owner/provider
	owner_id: z.optional(IdSchema),
	provider_id: z.optional(IdSchema),
	
	// Metadata
	hidden_until_request_accepted: z.optional(z.boolean()),
	filter_rule: z.optional(z.nullable(z.any()))
});

/**
 * Base Need - A participant's resource needs with slots
 */
export const BaseNeedSchema = z.object({
	id: IdSchema,
	name: z.string(),
	emoji: z.optional(z.string()),
	unit: z.optional(z.string()),
	description: z.optional(z.string()),
	
	// Multiple need slots
	need_slots: z.array(NeedSlotSchema),
	
	// Declarer
	declarer_id: IdSchema,
	
	// Status tracking
	status: z.enum(['open', 'partially-fulfilled', 'fulfilled']).default('open'),
	fulfilled_amount: z.number().gte(0).default(0),
	tags: z.array(z.string()).optional(),
	
	// Metadata
	hidden_until_request_accepted: z.optional(z.boolean()),
	filter_rule: z.optional(z.nullable(z.any()))
});

/**
 * Commitment - A participant's slot-based declaration
 * 
 * SLOT-NATIVE DESIGN:
 * - Each slot is a mini "capacity" or "need" with its own quantity
 * - Provider declares availability slots (what they can provide)
 * - Recipients declare need slots (what they need)
 * - Algorithm allocates each availability slot using two-tier logic
 * - Same recognition-based allocation, just applied per-slot
 * 
 * Published by each participant to declare:
 * - Their availability slots (if provider)
 * - Their need slots (if recipient)
 * - Their recognition weights (MR values)
 * - Adaptive damping state
 */
export const CommitmentSchema = z.object({
	// SLOT-BASED CAPACITY (if provider)
	// Array of availability slots with quantities
	capacity_slots: z.array(AvailabilitySlotSchema).optional(),
	
	// SLOT-BASED NEEDS (if recipient)
	// Array of need slots with quantities
	need_slots: z.array(NeedSlotSchema).optional(),
	
	// Recognition (mutual and one-way)
	mr_values: z.record(z.string(), z.number().nonnegative()).optional(),
	recognition_weights: z.record(z.string(), z.number().nonnegative()).optional(),
	
	// Capacity filters (which recipients can access which slots)
	capacity_filters: z.record(z.string(), CapacityFilterSchema).optional(),
	
	// Adaptive damping (applied to need slot quantities)
	damping_factor: z.number().min(0).max(1).optional(),
	over_allocation_history: z.array(z.number().nonnegative()).max(3).optional(),
	
	// Metadata
	timestamp: z.number().int().positive(),
	vectorClock: VectorClockSchema.optional(),
	round: z.number().int().nonnegative().optional()
});

/**
 * Slot Allocation Record - Records one slot allocating to one recipient
 * 
 * Each record represents:
 * - Which availability slot is providing
 * - Which recipient is receiving
 * - How much quantity is allocated
 * - Whether time/location were compatible
 * - Which tier (mutual vs non-mutual)
 */
export const SlotAllocationRecordSchema = z.object({
	// Provider's availability slot
	availability_slot_id: IdSchema,
	
	// Recipient info
	recipient_pubkey: z.string(),
	recipient_need_slot_id: IdSchema.optional(), // Which need slot (if matched to specific need)
	
	// Allocation amount
	quantity: z.number().nonnegative(),
	
	// Compatibility flags (for transparency)
	time_compatible: z.boolean(),
	location_compatible: z.boolean(),
	
	// Which tier was this allocation from?
	tier: z.enum(['mutual', 'non-mutual'])
});

/**
 * Two-Tier Allocation State - SLOT-NATIVE
 * 
 * Published by providers after computing slot-level allocations.
 * 
 * DESIGN: Each availability slot runs two-tier allocation independently
 * - Find compatible recipients for that slot
 * - Mutual recipients compete in Tier 1 for the slot's quantity
 * - Non-mutual recipients get remaining quantity in Tier 2
 * - Recognition weights determine shares within each tier
 * 
 * Same recognition logic, just applied per-slot instead of aggregate.
 */
export const TwoTierAllocationStateSchema = z.object({
	// SLOT-LEVEL DENOMINATORS
	// Map: slotId → { mutual: number, nonMutual: number }
	slot_denominators: z.record(
		IdSchema,
		z.object({
			mutual: z.number().nonnegative(),
			nonMutual: z.number().nonnegative()
		})
	),
	
	// SLOT-LEVEL ALLOCATION RECORDS
	// Array of all slot-to-recipient allocations
	slot_allocations: z.array(SlotAllocationRecordSchema),
	
	// SUMMARY BY RECIPIENT (aggregated view for convenience)
	// Map: recipientPubkey → total quantity allocated across all slots
	recipient_totals: z.record(z.string(), z.number().nonnegative()),
	
	// Metadata
	timestamp: z.number().int().positive()
});

// ═══════════════════════════════════════════════════════════════════
// HOLSTER STORAGE SCHEMAS
// ═══════════════════════════════════════════════════════════════════

/**
 * Holster storage format with timestamp
 * Wraps any data with _updatedAt for conflict resolution
 */
export function withTimestamp<T extends z.ZodTypeAny>(schema: T) {
	return z.object({
		_updatedAt: z.number().int().positive(),
		data: schema
	});
}

/**
 * Commitment with Holster timestamp wrapper
 */
export const StoredCommitmentSchema = withTimestamp(CommitmentSchema);

/**
 * Allocation State with Holster timestamp wrapper
 */
export const StoredAllocationStateSchema = withTimestamp(TwoTierAllocationStateSchema);

/**
 * Recognition Weights with Holster timestamp wrapper
 */
export const StoredRecognitionWeightsSchema = withTimestamp(
	z.record(z.string(), z.number().nonnegative())
);

// ═══════════════════════════════════════════════════════════════════
// TYPE EXPORTS (inferred from schemas)
// ═══════════════════════════════════════════════════════════════════

// Slot and capacity/need types
export type AvailabilitySlot = z.infer<typeof AvailabilitySlotSchema>;
export type NeedSlot = z.infer<typeof NeedSlotSchema>;
export type BaseCapacity = z.infer<typeof BaseCapacitySchema>;
export type BaseNeed = z.infer<typeof BaseNeedSchema>;
export type SlotAllocationRecord = z.infer<typeof SlotAllocationRecordSchema>;

// Allocation algorithm types
export type VectorClock = z.infer<typeof VectorClockSchema>;
export type RoundState = z.infer<typeof RoundStateSchema>;
export type CapacityFilter = z.infer<typeof CapacityFilterSchema>;
export type Commitment = z.infer<typeof CommitmentSchema>;
export type TwoTierAllocationState = z.infer<typeof TwoTierAllocationStateSchema>;

// Holster storage types
export type StoredCommitment = z.infer<typeof StoredCommitmentSchema>;
export type StoredAllocationState = z.infer<typeof StoredAllocationStateSchema>;
export type StoredRecognitionWeights = z.infer<typeof StoredRecognitionWeightsSchema>;

// ═══════════════════════════════════════════════════════════════════
// VALIDATION HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Validate and parse commitment data from network
 * Returns null if invalid (with console warning)
 */
export function parseCommitment(data: unknown): Commitment | null {
	const result = CommitmentSchema.safeParse(data);
	if (!result.success) {
		console.warn('[ALLOCATION-SCHEMA] Invalid commitment:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Validate and parse allocation state from network
 * Returns null if invalid (with console warning)
 */
export function parseAllocationState(data: unknown): TwoTierAllocationState | null {
	const result = TwoTierAllocationStateSchema.safeParse(data);
	if (!result.success) {
		console.warn('[ALLOCATION-SCHEMA] Invalid allocation state:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Validate and parse recognition weights from network
 * Returns null if invalid (with console warning)
 */
export function parseRecognitionWeights(data: unknown): Record<string, number> | null {
	const schema = z.record(z.string(), z.number().nonnegative());
	const result = schema.safeParse(data);
	if (!result.success) {
		console.warn('[ALLOCATION-SCHEMA] Invalid recognition weights:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Validate and parse round state from network
 * Returns null if invalid (with console warning)
 */
export function parseRoundState(data: unknown): RoundState | null {
	const result = RoundStateSchema.safeParse(data);
	if (!result.success) {
		console.warn('[ALLOCATION-SCHEMA] Invalid round state:', result.error);
		return null;
	}
	return result.data;
}

// ═══════════════════════════════════════════════════════════════════
// SCHEMA METADATA (for generic Holster utilities)
// ═══════════════════════════════════════════════════════════════════

/**
 * Registry of schemas with their Holster paths
 * Used by generic Holster store utilities
 */
export const AllocationSchemas = {
	commitment: {
		schema: CommitmentSchema,
		storedSchema: StoredCommitmentSchema,
		holsterPath: 'commitment',
		parser: parseCommitment
	},
	allocationState: {
		schema: TwoTierAllocationStateSchema,
		storedSchema: StoredAllocationStateSchema,
		holsterPath: 'allocationState',
		parser: parseAllocationState
	},
	recognitionWeights: {
		schema: z.record(z.string(), z.number().nonnegative()),
		storedSchema: StoredRecognitionWeightsSchema,
		holsterPath: 'recognitionWeights',
		parser: parseRecognitionWeights
	},
	roundState: {
		schema: RoundStateSchema,
		storedSchema: withTimestamp(RoundStateSchema),
		holsterPath: 'roundState',
		parser: parseRoundState
	}
} as const;

export type AllocationSchemaKey = keyof typeof AllocationSchemas;

