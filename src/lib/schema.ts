/**
 * Schema definitions using zod
 */
import { z } from 'zod/v4';

// Basic types
export const IdSchema = z.string().min(1);
export const NameSchema = z.string().min(1);
export const PointsSchema = z.number().gte(0);
export const PercentageSchema = z.number().gte(0).lte(1);

// ShareMap - a record of node IDs to percentage values
export const ShareMapSchema = z.record(IdSchema, PercentageSchema);

export const NonRootNodeSchema = z.object({
	id: IdSchema,
	name: NameSchema,
	type: z.literal('NonRootNode'),
	manual_fulfillment: z.nullable(z.number()),
	children: z.array(z.any()),
	points: PointsSchema,
	parent_id: IdSchema,
	contributor_ids: z.array(IdSchema),
	anti_contributors_ids: z.array(IdSchema)
});

export const RootNodeSchema = z.object({
	id: IdSchema,
	name: NameSchema,
	type: z.literal('RootNode'),
	manual_fulfillment: z.nullable(z.number()),
	children: z.array(z.any()),
	created_at: z.string(),
	updated_at: z.string()
});

// Union type for Node (either RootNode or NonRootNode)
export const NodeSchema = z.union([RootNodeSchema, NonRootNodeSchema]);

// Availability Slot Schema - unified allocation model
export const AvailabilitySlotSchema = z.object({
	id: IdSchema, // Unique identifier for this slot
	quantity: z.number().gte(0),

	// Timing constraints (for edge cases - replaces manual claiming)
	advance_notice_hours: z.optional(z.number().gte(0)), // Required notice for booking
	booking_window_hours: z.optional(z.number().gte(0)), // How far in advance can book

	// Time pattern fields (moved from BaseCapacity)
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

	// Location fields (moved from BaseCapacity - each slot can have different location)
	location_type: z.optional(z.string()),
	longitude: z.optional(z.number().min(-180).max(180)),
	latitude: z.optional(z.number().min(-90).max(90)),
	street_address: z.optional(z.string()),
	city: z.optional(z.string()),
	state_province: z.optional(z.string()),
	postal_code: z.optional(z.string()),
	country: z.optional(z.string()),

	// Hierarchical relationship for subset allocation
	parent_slot_id: z.optional(IdSchema), // If this is a subset of another slot

	// Mutual agreement for coordination
	mutual_agreement_required: z.optional(z.boolean().default(false)), // Requires explicit bilateral consent

	// Slot-specific metadata
	priority: z.optional(z.number()) // For conflict resolution
});

// Base capacity schema without any share information
export const BaseCapacitySchema = z.object({
	id: IdSchema,
	name: z.string(),
	emoji: z.optional(z.string()),
	unit: z.optional(
		z.string().regex(/^$|^(?!\s*\d+\.?\d*\s*$).+$/, {
			message: 'Unit must be text, not a number (e.g., "hours", "kg", "items")'
		})
	),
	description: z.optional(z.string()),
	max_natural_div: z.optional(z.number().gte(1)),
	max_percentage_div: z.optional(PercentageSchema),

	hidden_until_request_accepted: z.optional(z.boolean()),
	owner_id: z.optional(IdSchema),
	filter_rule: z.optional(z.nullable(z.any())),

	// Multiple availability slots instead of single quantity/time/location
	availability_slots: z.array(AvailabilitySlotSchema)

	// use-conditions: effects (possibly triggers)
});

// Provider perspective - includes recipient shares
export const ProviderCapacitySchema = z.object({
	...BaseCapacitySchema.shape,
	recipient_shares: z.record(IdSchema, PercentageSchema)
});

// Slot-specific computed quantity
export const SlotComputedQuantitySchema = z.object({
	slot_id: IdSchema,
	quantity: z.number().gte(0)
});

// Slot claim tracking
export const SlotClaimSchema = z.object({
	id: IdSchema, // Unique claim ID
	capacity_id: IdSchema,
	slot_id: IdSchema,
	claimer_id: IdSchema,
	desired_quantity: z.number().gte(0),
	feasible_quantity: z.number().gte(0), // After credit and competition constraints
	allocated_quantity: z.number().gte(0), // Actually allocated amount
	status: z.enum(['pending', 'confirmed', 'cancelled']),
	claim_timestamp: z.string(),
	fulfillment_timestamp: z.optional(z.string())
});

// User's desired slot claims (mirrors userDesiredComposeFrom structure)
export const UserSlotClaimsSchema = z.record(IdSchema, z.record(IdSchema, z.number().gte(0)));
// Structure: capacityId → slotId → desiredQuantity

// Network slot claims (all users' claims)
export const NetworkSlotClaimsSchema = z.record(IdSchema, UserSlotClaimsSchema);
// Structure: userId → capacityId → slotId → desiredQuantity

// Recipient perspective - includes our share info
export const RecipientCapacitySchema = z.object({
	...BaseCapacitySchema.shape,
	// share_id: IdSchema
	share_percentage: PercentageSchema,
	computed_quantities: z.array(SlotComputedQuantitySchema), // Per-slot computed quantities
	provider_id: IdSchema
	// reciever_id: IdSchema
});

// Union type for Capacity (either provider or recipient perspective)
export const CapacitySchema = z.union([ProviderCapacitySchema, RecipientCapacitySchema]);

// CapacitiesCollection schema
export const CapacitiesCollectionSchema = z.record(IdSchema, CapacitySchema);

// Recognition cache entry schema
export const RecognitionCacheEntrySchema = z.object({
	ourShare: PercentageSchema,
	theirShare: PercentageSchema,
	timestamp: z.number()
});

// Recognition cache schema
export const RecognitionCacheSchema = z.record(IdSchema, RecognitionCacheEntrySchema);

// Re-export the infer type for convenience
export type RootNode = z.infer<typeof RootNodeSchema>;
export type NonRootNode = z.infer<typeof NonRootNodeSchema>;
export type Node = z.infer<typeof NodeSchema>;
export type AvailabilitySlot = z.infer<typeof AvailabilitySlotSchema>;
export type SlotComputedQuantity = z.infer<typeof SlotComputedQuantitySchema>;
export type SlotClaim = z.infer<typeof SlotClaimSchema>;
export type UserSlotClaims = z.infer<typeof UserSlotClaimsSchema>;
export type NetworkSlotClaims = z.infer<typeof NetworkSlotClaimsSchema>;
export type BaseCapacity = z.infer<typeof BaseCapacitySchema>;
export type ProviderCapacity = z.infer<typeof ProviderCapacitySchema>;
export type RecipientCapacity = z.infer<typeof RecipientCapacitySchema>;
export type Capacity = z.infer<typeof CapacitySchema>;
export type CapacitiesCollection = z.infer<typeof CapacitiesCollectionSchema>;
export type ShareMap = z.infer<typeof ShareMapSchema>;
export type RecognitionCacheEntry = z.infer<typeof RecognitionCacheEntrySchema>;
export type RecognitionCache = z.infer<typeof RecognitionCacheSchema>;

// Composition desire schema - maps target capacity ID to desired absolute units
export const CompositionDesireSchema = z.record(IdSchema, z.number().gte(0));

// User composition schema - maps source capacity ID to composition desires
// Structure: sourceCapacityId → targetCapacityId → absoluteUnits
export const UserCompositionSchema = z.record(IdSchema, CompositionDesireSchema);

// Network composition schema - maps user ID to their composition data
// Structure: userId → sourceCapacityId → targetCapacityId → absoluteUnits
export const NetworkCompositionSchema = z.record(IdSchema, UserCompositionSchema);

// Export types
export type CompositionDesire = z.infer<typeof CompositionDesireSchema>;
export type UserComposition = z.infer<typeof UserCompositionSchema>;
export type NetworkComposition = z.infer<typeof NetworkCompositionSchema>;

// Slot-aware composition schemas - enables slot-to-slot composition
// Structure: targetCapacityId → targetSlotId → desiredAbsoluteUnits
export const SlotCompositionDesireSchema = z.record(
	IdSchema,
	z.record(IdSchema, z.number().gte(0))
);

// User slot composition schema - maps source slots to target slots
// Structure: sourceCapacityId → sourceSlotId → targetCapacityId → targetSlotId → absoluteUnits
export const UserSlotCompositionSchema = z.record(
	IdSchema,
	z.record(IdSchema, SlotCompositionDesireSchema)
);

// Network slot composition schema - maps user ID to their slot composition data
// Structure: userId → sourceCapacityId → sourceSlotId → targetCapacityId → targetSlotId → absoluteUnits
export const NetworkSlotCompositionSchema = z.record(IdSchema, UserSlotCompositionSchema);

// Export slot composition types
export type SlotCompositionDesire = z.infer<typeof SlotCompositionDesireSchema>;
export type UserSlotComposition = z.infer<typeof UserSlotCompositionSchema>;
export type NetworkSlotComposition = z.infer<typeof NetworkSlotCompositionSchema>;

// Contact schema - simplified to just essential fields
export const ContactSchema = z.object({
	contact_id: IdSchema,
	name: NameSchema, // User-assigned name
	public_key: z.optional(z.string()), // Their Gun public key (if they have one)
	created_at: z.string(),
	updated_at: z.string()
});

// ContactsCollection schema - indexed by contact_id
export const ContactsCollectionSchema = z.record(IdSchema, ContactSchema);

// Export contact types
export type Contact = z.infer<typeof ContactSchema>;
export type ContactsCollection = z.infer<typeof ContactsCollectionSchema>;

// Chat read state schema - tracks last read timestamp for each chat
export const ChatReadStateSchema = z.object({
	chatId: IdSchema,
	lastReadTimestamp: z.number(),
	updatedAt: z.number()
});

// Chat read states collection schema - indexed by chatId
export const ChatReadStatesSchema = z.record(IdSchema, ChatReadStateSchema);

// Export chat read state types
export type ChatReadState = z.infer<typeof ChatReadStateSchema>;
export type ChatReadStates = z.infer<typeof ChatReadStatesSchema>;

// Collective Tree Schema
// it should have a root node (the id being the hash of the ids of the contributors)
// it should be a merge of all the jsons of all the contributors
// except that when we merge the children have weights in %'s not points!
// we took the child's % of the siblings of a parent along a path across all userTrees and merged the %'s weighted by that user's share of collective recognition!

// Collective Node Schema - similar to Node but with percentage-based children
export const CollectiveNonRootNodeSchema = z.object({
	id: IdSchema,
	name: NameSchema,
	type: z.literal('CollectiveNonRootNode'),
	manual_fulfillment: z.nullable(z.number()),
	children: z.array(z.any()), // Will be CollectiveNode instances
	weight_percentage: PercentageSchema, // Percentage weight instead of points
	parent_id: IdSchema,
	contributor_ids: z.array(IdSchema),
	// Additional collective-specific fields
	source_contributors: z.record(IdSchema, PercentageSchema), // Maps contributor ID to their weight in this node
	merged_from_nodes: z.array(IdSchema) // Original node IDs that were merged to create this
});

export const CollectiveRootNodeSchema = z.object({
	id: IdSchema, // Hash of all contributor IDs
	name: NameSchema,
	type: z.literal('CollectiveRootNode'),
	manual_fulfillment: z.nullable(z.number()),
	children: z.array(z.any()), // Will be CollectiveNode instances
	created_at: z.string(),
	updated_at: z.string(),
	// Collective-specific fields
	contributors: z.array(IdSchema), // All contributor IDs
	contributor_weights: z.record(IdSchema, PercentageSchema), // Each contributor's recognition share
	source_trees: z.record(IdSchema, z.any()) // Maps contributor ID to their original tree
});

// Union type for CollectiveNode
export const CollectiveNodeSchema = z.union([
	CollectiveRootNodeSchema,
	CollectiveNonRootNodeSchema
]);

// Collective Tree Schema - the complete structure
export const CollectiveTreeSchema = z.object({
	id: IdSchema, // Hash of contributor IDs
	root: CollectiveRootNodeSchema,
	contributors: z.array(IdSchema),
	recognition_matrix: z.record(IdSchema, z.record(IdSchema, PercentageSchema)), // Mutual recognition between contributors
	creation_timestamp: z.string(),
	last_updated: z.string(),
	// Metadata about the merge process
	merge_algorithm_version: z.string(),
	total_nodes_merged: z.number(),
	merge_conflicts: z.optional(
		z.array(
			z.object({
				path: z.string(),
				contributors: z.array(IdSchema),
				resolution: z.string()
			})
		)
	)
});

// Tree merge configuration schema
export const TreeMergeConfigSchema = z.object({
	contributor_trees: z.record(IdSchema, NodeSchema), // Maps contributor ID to their tree
	recognition_shares: z.record(IdSchema, PercentageSchema), // Each contributor's recognition weight
	merge_strategy: z.enum(['weighted_average', 'consensus_threshold', 'priority_based']),
	conflict_resolution: z.enum(['skip', 'merge', 'prioritize_highest_weight']),
	minimum_weight_threshold: z.optional(PercentageSchema), // Minimum weight to include a node
	name_collision_strategy: z.enum(['append_contributor', 'weighted_priority', 'manual_resolve'])
});

// Export collective types
export type CollectiveRootNode = z.infer<typeof CollectiveRootNodeSchema>;
export type CollectiveNonRootNode = z.infer<typeof CollectiveNonRootNodeSchema>;
export type CollectiveNode = z.infer<typeof CollectiveNodeSchema>;
export type CollectiveTree = z.infer<typeof CollectiveTreeSchema>;
export type TreeMergeConfig = z.infer<typeof TreeMergeConfigSchema>;

// Utility type for tree merging operations
export const TreeMergeResultSchema = z.object({
	collective_tree: CollectiveTreeSchema,
	merge_stats: z.object({
		total_contributors: z.number(),
		nodes_merged: z.number(),
		conflicts_resolved: z.number(),
		execution_time_ms: z.number()
	}),
	warnings: z.array(z.string()),
	errors: z.array(z.string())
});

export type TreeMergeResult = z.infer<typeof TreeMergeResultSchema>;
