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
	contributor_ids: z.array(IdSchema)
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

// Base capacity schema without any share information
export const BaseCapacitySchema = z.object({
	id: IdSchema,
	name: z.string(),
	emoji: z.optional(z.string()),
	quantity: z.optional(z.number().gte(0)),
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

	// these should be part of a availability schema?
	// availability?
	location_type: z.optional(z.string()),
	longitude: z.optional(z.number().min(-180).max(180)),
	latitude: z.optional(z.number().min(-90).max(90)),

	// Address fields as alternative to coordinates
	street_address: z.optional(z.string()),
	city: z.optional(z.string()),
	state_province: z.optional(z.string()),
	postal_code: z.optional(z.string()),
	country: z.optional(z.string()),

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
	time_zone: z.optional(z.string())

	// use-conditions: effects (possibly triggers)
});

// Provider perspective - includes recipient shares
export const ProviderCapacitySchema = z.object({
	...BaseCapacitySchema.shape,
	recipient_shares: z.record(IdSchema, PercentageSchema)
});

// Recipient perspective - includes our share info
export const RecipientCapacitySchema = z.object({
	...BaseCapacitySchema.shape,
	// share_id: IdSchema
	share_percentage: PercentageSchema,
	computed_quantity: z.number().gte(0),
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
