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

// We'll use a simpler approach without circular references for schema validation
// Node schemas will validate most properties but not the recursive children structure
export const BaseNodeSchema = z.object({
	id: IdSchema,
	name: NameSchema,
	type: z.string(),
	manual_fulfillment: z.nullable(z.number()),
	children: z.array(z.any()) // This accepts any array for children
});

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
	user_id: IdSchema,
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
	unit: z.optional(z.string()),
	location_type: z.optional(z.string()),
	longitude: z.optional(z.number().min(-180).max(180)),
	latitude: z.optional(z.number().min(-90).max(90)),
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
	max_natural_div: z.optional(z.number().gte(1)),
	max_percentage_div: z.optional(PercentageSchema),
	hidden_until_request_accepted: z.optional(z.boolean()),
	owner_id: z.optional(IdSchema),
	filter_rule: z.optional(z.nullable(z.any()))
});

// Provider perspective - includes recipient shares
export const ProviderCapacitySchema = z.object({
	...BaseCapacitySchema.shape,
	recipient_shares: z.record(IdSchema, PercentageSchema)
});

// Recipient perspective - includes our share info
export const RecipientCapacitySchema = z.object({
	...BaseCapacitySchema.shape,
	share_percentage: PercentageSchema,
	computed_quantity: z.number().gte(0),
	provider_id: IdSchema
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
