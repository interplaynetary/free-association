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
export const PointsSchema = z.number().gte(0);
export const PercentageSchema = z.number().gte(0).lte(1);

/**
 * ShareMap - Maps entity IDs to their percentage shares
 * Used for recognition and allocation calculations
 */
export const ShareMapSchema = z.record(IdSchema, PercentageSchema);

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
// TREE SCHEMAS (for recognition and priority trees)
// ═══════════════════════════════════════════════════════════════════

// ═══════════════════════════════════════════════════════════════════
// DECLARATIVE REACTIVE COMPUTATION SCHEMAS
// ═══════════════════════════════════════════════════════════════════

/**
 * Variable Binding - Declares where a variable gets its value
 * 
 * Types:
 * - 'value': Static literal value
 * - 'subscription': Subscribe to a Holster path (reactive, uses .on())
 * - 'fetch': One-time fetch from Holster path (uses .get())
 * - 'local': Read from local state
 * - 'derived': Result from a previous computation
 */
export const VariableBindingSchema = z.discriminatedUnion('type', [
	// Static value
	z.object({
		type: z.literal('value'),
		value: z.any()
	}),
	
	// Subscribe to Holster path (reactive, .on())
	z.object({
		type: z.literal('subscription'),
		holster_path: z.string(),
		schema_type: z.string(),
		subscribe_to_user: z.optional(z.string()), // For cross-user subscriptions
		default_value: z.optional(z.any()) // Fallback if subscription empty
	}),
	
	// One-time fetch from Holster path (non-reactive, .get())
	z.object({
		type: z.literal('fetch'),
		holster_path: z.string(),
		schema_type: z.string(),
		fetch_from_user: z.optional(z.string()), // For cross-user fetch
		default_value: z.optional(z.any()), // Fallback if fetch returns null
		wait_ms: z.optional(z.number().gte(0).default(100)) // Wait time for response
	}),
	
	// Read from local state path
	z.object({
		type: z.literal('local'),
		state_path: z.string(), // e.g., 'myData.field.subfield'
		default_value: z.optional(z.any())
	}),
	
	// Reference a derived computation result
	z.object({
		type: z.literal('derived'),
		computation_id: z.string(), // Which computation produces this
		output_key: z.string(), // Which output from that computation
		default_value: z.optional(z.any())
	})
]);

/**
 * Output Binding - Declares where to store computation results
 * 
 * Types:
 * - 'holster': Persist to Holster path
 * - 'local': Store in local state
 * - 'memory': Keep in memory only (for derived computations)
 */
export const OutputBindingSchema = z.discriminatedUnion('type', [
	// Persist to Holster
	z.object({
		type: z.literal('holster'),
		holster_path: z.string(),
		schema_type: z.optional(z.string()), // For validation
		persist_debounce_ms: z.optional(z.number().gte(0))
	}),
	
	// Store in local state
	z.object({
		type: z.literal('local'),
		state_path: z.string()
	}),
	
	// Keep in memory (for chaining computations)
	z.object({
		type: z.literal('memory')
	})
]);

/**
 * Computation Definition - Declarative reactive computation
 * 
 * Defines:
 * - Input variables (and where they come from)
 * - Computation function (as serialized string or named function)
 * - Output bindings (where to store results)
 * 
 * Example:
 * ```typescript
 * {
 *   id: 'mutual_recognition',
 *   inputs: {
 *     myCommitment: { type: 'subscription', holster_path: 'allocation/commitment', ... },
 *     theirCommitment: { type: 'subscription', holster_path: '~{pubkey}/allocation/commitment', ... }
 *   },
 *   compute_fn: 'computeMutualRecognition', // Named function from registry
 *   outputs: {
 *     mutualRecognition: { type: 'holster', holster_path: 'results/mr' }
 *   }
 * }
 * ```
 */
export const ComputationSchema = z.object({
	id: IdSchema,
	
	// Input variable declarations
	inputs: z.record(z.string(), VariableBindingSchema),
	
	// Computation function (named function from registry or serialized code)
	compute_fn: z.string(),
	
	// Local variables that can be bound within computation closure
	local_bindings: z.optional(z.record(z.string(), VariableBindingSchema)),
	
	// Output mappings (resultKey → where to store)
	outputs: z.record(z.string(), OutputBindingSchema),
	
	// Execution control
	debounce_ms: z.optional(z.number().gte(0).default(0)), // Debounce re-computation
	enabled: z.optional(z.boolean().default(true)), // Can disable computation
	depends_on: z.optional(z.array(z.string())), // Explicit computation dependencies (for ordering)
	
	// Metadata
	description: z.optional(z.string()),
	version: z.optional(z.string())
});

/**
 * Reactive Computation Graph - Complete dataflow specification
 * 
 * Declares:
 * - All variables and their sources
 * - All computations and dependencies
 * - Output destinations
 * 
 * The runtime will:
 * - Set up subscriptions for all 'subscription' bindings
 * - React to changes and trigger computations
 * - Store results according to output bindings
 * - Handle computation ordering based on dependencies
 * 
 * Example use case: Mutual recognition calculation
 * ```typescript
 * {
 *   variables: {
 *     myTree: { type: 'subscription', holster_path: 'tree', schema_type: 'RootNode' },
 *     theirTree: { type: 'subscription', holster_path: '~{pubkey}/tree', schema_type: 'RootNode' }
 *   },
 *   computations: [
 *     {
 *       id: 'recognition',
 *       inputs: { myTree: {}, theirTree: {} }, // References variables
 *       compute_fn: 'shareOfGeneralFulfillment',
 *       outputs: { share: { type: 'holster', holster_path: 'recognition/share' } }
 *     }
 *   ]
 * }
 * ```
 */
export const ReactiveComputationGraphSchema = z.object({
	id: IdSchema,
	
	// Global variable declarations
	variables: z.record(z.string(), VariableBindingSchema),
	
	// Computation pipeline
	computations: z.array(ComputationSchema),
	
	// Metadata
	version: z.optional(z.string()),
	description: z.optional(z.string()),
	
	// Program hash - if provided, all holster paths are prefixed with this
	// If not provided, computed automatically from program structure
	// Used to namespace program data: ~pubkey/<program_hash>/<holster_path>
	program_hash: z.optional(z.string())
});

// ═══════════════════════════════════════════════════════════════════
// COMPUTATION PROVENANCE SCHEMAS
// ═══════════════════════════════════════════════════════════════════

/**
 * Input Provenance - tracks where an input came from
 */
export const InputProvenanceSchema = z.object({
	source: z.enum(['value', 'subscription', 'fetch', 'local', 'derived']),
	path: z.string().optional(),
	contentHash: z.string(), // SHA-256 hash of the input data
	provenance: z.string().optional() // Reference to parent computation provenance ID
});

/**
 * Output Provenance - tracks what was produced
 */
export const OutputProvenanceSchema = z.object({
	path: z.string(),
	contentHash: z.string() // SHA-256 hash of the output data
});

/**
 * Computation Provenance - complete lineage of a computation execution
 * 
 * This enables:
 * - Verifying computation legitimacy
 * - Tracking data lineage
 * - Reproducing computations
 * - Byzantine fault tolerance
 */
export const ComputationProvenanceSchema = z.object({
	// Unique identifier for this provenance record
	id: z.string(),
	
	// Peer causality (when/who)
	vectorClock: VectorClockSchema,
	executedBy: z.string(), // pubkey of executor
	timestamp: z.number().int().positive(),
	
	// Computation identity (what)
	programHash: z.string(),      // Which program
	computationId: z.string(),    // Which computation within program
	computationHash: z.string(),  // Hash of the computation definition (for version tracking)
	
	// Input provenance (what data was used)
	inputs: z.record(z.string(), InputProvenanceSchema),
	
	// Output provenance (what was produced)
	outputs: z.record(z.string(), OutputProvenanceSchema),
	
	// Deterministic verification hash
	// Hash of (computationHash + sorted input hashes)
	// Allows others to verify: "If I had these inputs, would I get this output?"
	deterministicHash: z.string(),
	
	// Optional: parent provenance IDs (for lineage tracking)
	parents: z.array(z.string()).optional()
});

/**
 * Versioned Program Data with Provenance
 * Stored at: ~pubkey/<program_hash>/<holster_path>/<provenance_signature>
 */
export const VersionedProgramDataSchema = z.object({
	// The actual data
	data: z.any(),
	
	// Full provenance record
	provenance: ComputationProvenanceSchema,
	
	// Storage metadata
	_updatedAt: z.number().int().positive()
});

/**
 * Node Data Storage - Reactive Store Pattern for Tree Nodes
 * 
 * Each node becomes a mini-store that can:
 * - Store typed data (validated with schema)
 * - Track its Holster subscription path
 * - Maintain sync timestamps
 * - Manage loading/persisting state
 * - Subscribe to specific network data
 * - Define reactive computation graphs
 * 
 * This mirrors the guide's architecture where each data point has:
 * - A clear source (holster_path)
 * - A schema for validation (data_schema_type)
 * - Reactive updates (is_loading, is_persisting)
 * - Conflict resolution (last_network_timestamp)
 * - Declarative computations (computation_graph)
 */
export const NodeDataStorageSchema = z.object({
	/** Arbitrary data stored at this node (validated by schema identified in data_schema_type) */
	data: z.optional(z.any()),
	
	/** Holster path this node subscribes to (e.g., '~{pubkey}/commitment', 'capacity/slot-123') */
	holster_path: z.optional(z.string()),
	
	/** Schema type identifier for validation (e.g., 'Commitment', 'AvailabilitySlot', 'BaseCapacity') */
	data_schema_type: z.optional(z.string()),
	
	/** Timestamp when this node's data was last updated locally */
	data_updated_at: z.optional(z.number().int().positive()),
	
	/** Whether this node is currently loading data from network */
	is_loading: z.optional(z.boolean()),
	
	/** Whether this node is currently persisting data to network */
	is_persisting: z.optional(z.boolean()),
	
	/** Last network timestamp seen (for conflict resolution - network wins if newer) */
	last_network_timestamp: z.optional(z.number().int().positive()),
	
	/** Whether to auto-persist changes to this node's data (default: true) */
	auto_persist: z.optional(z.boolean().default(true)),
	
	/** Debounce time for persistence (ms, default: 0 = immediate) */
	persist_debounce_ms: z.optional(z.number().gte(0).default(0)),
	
	/** Optional pubkey to subscribe to (if subscribing to another user's data) */
	subscribe_to_user: z.optional(z.string()),
	
	/** Custom comparison function name for equality checking */
	equality_check: z.optional(z.string()),
	
	/** Declarative reactive computation graph (optional) */
	computation_graph: z.optional(ReactiveComputationGraphSchema)
});

/**
 * Non-Root Node - Represents a node in a recognition/priority tree
 * 
 * Can be either:
 * - CONTRIBUTION NODE: Has contributors (represents actual work)
 * - STRUCTURAL NODE: No contributors (represents decomposition)
 * 
 * REACTIVE CAPABILITIES:
 * Each node can optionally store data and manage its own Holster subscriptions,
 * making it a self-contained reactive store within the tree structure.
 * 
 * Example use cases:
 * - Node represents a capacity → storage.data contains BaseCapacity, holster_path = 'capacity/{id}'
 * - Node represents a user's commitment → storage.data contains Commitment, holster_path = '~{pubkey}/commitment'
 * - Node represents an allocation result → storage.data contains TwoTierAllocationState
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
	
	// Reactive store capabilities
	storage: z.optional(NodeDataStorageSchema)
});

/**
 * Root Node - Top-level node of a recognition/priority tree
 * 
 * REACTIVE CAPABILITIES:
 * Like non-root nodes, root nodes can store data and manage subscriptions,
 * enabling the entire tree to be a distributed reactive data structure.
 * 
 * Example use cases:
 * - Root node represents a user's overall allocation state
 * - Root node tracks network-wide coordination data
 * - Root node subscribes to collective recognition data
 */
export const RootNodeSchema = z.object({
	id: IdSchema,
	name: NameSchema,
	type: z.literal('RootNode'),
	manual_fulfillment: z.nullable(z.number()),
	children: z.array(z.any()), // Recursive reference
	created_at: z.string(),
	updated_at: z.string(),
	
	// Reactive store capabilities
	storage: z.optional(NodeDataStorageSchema)
});

/**
 * Node - Union type for any node in a tree
 */
export const NodeSchema = z.union([RootNodeSchema, NonRootNodeSchema]);

// ═══════════════════════════════════════════════════════════════════
// SLOT SCHEMAS (for capacity and need matching)
// ═══════════════════════════════════════════════════════════════════

/**
 * Resource Metadata - Common fields for describing a resource
 * Shared by slots, capacities, and needs
 * 
 * FILTER LOGIC:
 * - Capacity/Availability filters: Who can RECEIVE from this resource
 * - Need filters: Who can PROVIDE to fulfill this need
 * - Compatible match requires: Provider passes need filter AND Recipient passes capacity filter
 */
export const ResourceMetadataSchema = z.object({
	name: z.string(),
	emoji: z.optional(z.string()),
	unit: z.optional(z.string()),
	description: z.optional(z.string()),
	resource_type: z.optional(z.string()), // e.g., "tutoring", "housing", "money"
	
	// Filters (applicable to capacities/needs at any level)
	filter_rule: z.optional(z.nullable(z.any())), // Serialized filter function or criteria
	hidden_until_request_accepted: z.optional(z.boolean()) // Privacy filter
});

/**
 * Availability Slot - Represents a specific time/location/quantity of available capacity
 * Fully self-describing with resource metadata
 */
export const AvailabilitySlotSchema = ResourceMetadataSchema.extend({
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
 * Fully self-describing with resource metadata
 */
export const NeedSlotSchema = ResourceMetadataSchema.extend({
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
 * Extends ResourceMetadataSchema for consistent resource description
 * 
 * FILTER INHERITANCE:
 * - Inherits filter_rule from ResourceMetadataSchema (capacity-wide filter)
 * - Each availability_slot can also have its own filter_rule (slot-specific override)
 * - Slot filter takes precedence over capacity-wide filter if both exist
 */
export const BaseCapacitySchema = ResourceMetadataSchema.extend({
	id: IdSchema,
	
	// Multiple availability slots (each can have own filter)
	availability_slots: z.array(AvailabilitySlotSchema),
	
	// Owner/provider
	owner_id: z.optional(IdSchema),
	provider_id: z.optional(IdSchema)
});

/**
 * Base Need - A participant's resource needs with slots
 * Extends ResourceMetadataSchema for consistent resource description
 * 
 * FILTER INHERITANCE:
 * - Inherits filter_rule from ResourceMetadataSchema (need-wide filter)
 * - Each need_slot can also have its own filter_rule (slot-specific override)
 * - Slot filter takes precedence over need-wide filter if both exist
 * - Need filters specify who can PROVIDE to fulfill this need
 */
export const BaseNeedSchema = ResourceMetadataSchema.extend({
	id: IdSchema,
	
	// Multiple need slots (each can have own filter)
	need_slots: z.array(NeedSlotSchema),
	
	// Declarer
	declarer_id: IdSchema,
	
	// Status tracking
	status: z.enum(['open', 'partially-fulfilled', 'fulfilled']).default('open'),
	fulfilled_amount: z.number().gte(0).default(0),
	tags: z.array(z.string()).optional()
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
 * FILTER ARCHITECTURE:
 * - Filters are now on slots themselves (via ResourceMetadataSchema)
 * - Each capacity_slot has filter_rule (who can receive from this slot)
 * - Each need_slot has filter_rule (who can provide to this slot)
 * - Matching requires provider passes need filter AND recipient passes capacity filter
 * 
 * Published by each participant to declare:
 * - Their availability slots (if provider)
 * - Their need slots (if recipient)
 * - Their recognition weights (MR values)
 * - Adaptive damping state
 */
export const CommitmentSchema = z.object({
	// SLOT-BASED CAPACITY (if provider)
	// Array of availability slots with quantities (each slot can have its own filter)
	capacity_slots: z.array(AvailabilitySlotSchema).optional(),
	
	// SLOT-BASED NEEDS (if recipient)
	// Array of need slots with quantities (each slot can have its own filter)
	need_slots: z.array(NeedSlotSchema).optional(),
	
	// Recognition (mutual and one-way)
	mr_values: z.record(z.string(), z.number().nonnegative()).optional(),
	recognition_weights: z.record(z.string(), z.number().nonnegative()).optional(),
	
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

// Reactive computation types
export type VariableBinding = z.infer<typeof VariableBindingSchema>;
export type OutputBinding = z.infer<typeof OutputBindingSchema>;
export type Computation = z.infer<typeof ComputationSchema>;
export type ReactiveComputationGraph = z.infer<typeof ReactiveComputationGraphSchema>;

// Tree types
export type Node = z.infer<typeof NodeSchema>;
export type RootNode = z.infer<typeof RootNodeSchema>;
export type NonRootNode = z.infer<typeof NonRootNodeSchema>;
export type NodeDataStorage = z.infer<typeof NodeDataStorageSchema>;
export type ShareMap = z.infer<typeof ShareMapSchema>;

// Resource metadata (shared by slots, capacities, and needs)
export type ResourceMetadata = z.infer<typeof ResourceMetadataSchema>;

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

// Computation provenance types
export type InputProvenance = z.infer<typeof InputProvenanceSchema>;
export type OutputProvenance = z.infer<typeof OutputProvenanceSchema>;
export type ComputationProvenance = z.infer<typeof ComputationProvenanceSchema>;
export type VersionedProgramData = z.infer<typeof VersionedProgramDataSchema>;

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

