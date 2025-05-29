/**
 * Schema definitions using zod-mini
 */
import { z } from 'zod/v4-mini';

// Basic types
export const IdSchema = z.string().check(z.minLength(1));
export const NameSchema = z.string().check(z.minLength(1));
export const PointsSchema = z.number().check(z.gte(0));
export const PercentageSchema = z.number().check(z.gte(0), z.lte(1));

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

// CapacityShare schema
export const CapacityShareSchema = z.object({
	id: IdSchema,
	share_percentage: PercentageSchema,
	computed_quantity: z.number().check(z.gte(0)),
	capacity_id: IdSchema,
	recipient_id: IdSchema
});

// Capacity schema
export const CapacitySchema = z.object({
	id: IdSchema,
	name: z.string(),
	quantity: z.number().check(z.gte(0)),
	unit: z.string(),
	location_type: z.string(),
	all_day: z.boolean(),
	recurrence: z.nullable(z.string()),
	custom_recurrence_repeat_every: z.nullable(z.number()),
	custom_recurrence_repeat_unit: z.nullable(z.string()),
	custom_recurrence_end_type: z.nullable(z.string()),
	custom_recurrence_end_value: z.nullable(z.string()),
	start_date: z.nullable(z.string()),
	start_time: z.nullable(z.string()),
	end_date: z.nullable(z.string()),
	end_time: z.nullable(z.string()),
	time_zone: z.string(),
	max_natural_div: z.number().check(z.gte(1)),
	max_percentage_div: PercentageSchema,
	hidden_until_request_accepted: z.boolean(),
	owner_id: IdSchema,
	shares: z.array(CapacityShareSchema),
	filter_rule: z.nullable(z.any()),
	recipient_shares: z.nullable(z.record(IdSchema, PercentageSchema))
});

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
export type CapacityShare = z.infer<typeof CapacityShareSchema>;
export type Capacity = z.infer<typeof CapacitySchema>;
export type CapacitiesCollection = z.infer<typeof CapacitiesCollectionSchema>;
export type ShareMap = z.infer<typeof ShareMapSchema>;
export type RecognitionCacheEntry = z.infer<typeof RecognitionCacheEntrySchema>;
export type RecognitionCache = z.infer<typeof RecognitionCacheSchema>;
