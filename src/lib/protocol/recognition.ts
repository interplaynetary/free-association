import * as z from 'zod';
import { IdSchema, NameSchema, PointsSchema, PercentageSchema, ITCStampSchema } from './resources';

// ═══════════════════════════════════════════════════════════════════
// CONTRIBUTOR SCHEMAS
// ═══════════════════════════════════════════════════════════════════

export const ContributorSchema = z.object({
    id: IdSchema,
    points: PointsSchema
});

export type Contributor = z.infer<typeof ContributorSchema>;

// ═══════════════════════════════════════════════════════════════════
// TREE SCHEMAS
// ═══════════════════════════════════════════════════════════════════

export const NodeDataStorageSchema = z.object({
    data: z.optional(z.any()),
    holster_path: z.optional(z.string()),
    data_schema_type: z.optional(z.string()),
    data_updated_at: z.optional(z.number().int().positive()),
    is_loading: z.optional(z.boolean()),
    is_persisting: z.optional(z.boolean()),
    last_network_timestamp: z.optional(z.number().int().positive()),
    auto_persist: z.optional(z.boolean().default(true)),
    persist_debounce_ms: z.optional(z.number().gte(0).default(0)),
    subscribe_to_user: z.optional(z.string()),
    equality_check: z.optional(z.string())
});

export type NodeDataStorage = z.infer<typeof NodeDataStorageSchema>;

export const NonRootNodeSchema = z.object({
    id: IdSchema,
    name: NameSchema,
    type: z.literal('NonRootNode'),
    manual_fulfillment: z.number().optional(),
    children: z.array(z.any()),
    points: PointsSchema,
    parent_id: IdSchema,
    contributors: z.array(ContributorSchema).default([]),
    anti_contributors: z.array(ContributorSchema).default([]),
    storage: z.optional(NodeDataStorageSchema)
});

export type NonRootNode = z.infer<typeof NonRootNodeSchema>;

export const RootNodeSchema = z.object({
    id: IdSchema,
    name: NameSchema,
    type: z.literal('RootNode'),
    manual_fulfillment: z.number().optional(),
    children: z.array(z.any()),
    created_at: z.string(),
    updated_at: z.string(),
    storage: z.optional(NodeDataStorageSchema)
});

export type RootNode = z.infer<typeof RootNodeSchema>;

export const NodeSchema = z.union([RootNodeSchema, NonRootNodeSchema]);
export type Node = z.infer<typeof NodeSchema>;

// ═══════════════════════════════════════════════════════════════════
// GLOBAL RECOGNITION
// ═══════════════════════════════════════════════════════════════════

export const ShareMapSchema = z.record(IdSchema, PercentageSchema);
export type ShareMap = z.infer<typeof ShareMapSchema>;

export const GlobalRecognitionWeightsSchema = z.record(
    z.string(),
    z.number().nonnegative()
);

export type GlobalRecognitionWeights = z.infer<typeof GlobalRecognitionWeightsSchema>;

// ═══════════════════════════════════════════════════════════════════
// ATTRIBUTE RECOGNITION SYSTEM
// ═══════════════════════════════════════════════════════════════════

export const MembershipListSchema = z.array(z.string().min(1));
export type MembershipList = z.infer<typeof MembershipListSchema>;

export const SkillValueSchema = z.object({
    level: z.number().int().min(1).max(10),
    years: z.number().nonnegative().optional(),
    description: z.string().optional(),
    verified: z.boolean().default(false),
    endorsements: z.array(z.string()).default([])
});

export type SkillValue = z.infer<typeof SkillValueSchema>;

export const LocationValueSchema = z.object({
    city: z.string().optional(),
    state_province: z.string().optional(),
    country: z.string().optional(),
    coords: z.tuple([
        z.number().min(-90).max(90),
        z.number().min(-180).max(180)
    ]).optional(),
    postal_code: z.string().optional(),
    street_address: z.string().optional(),
    online: z.boolean().optional()
});

export type LocationValue = z.infer<typeof LocationValueSchema>;

export const AttributeValueSchema = z.object({
    value: z.any(),
    source_pubkey: z.string().optional(),
    confidence: z.number().min(0).max(1).default(1.0),
    timestamp: z.number().int().positive(),
    itcStamp: ITCStampSchema.optional()
});

export type AttributeValue = z.infer<typeof AttributeValueSchema>;

export const AttributeRecognitionsCollectionSchema = z.object({
    // Entity attributes (dynamic keys)
}).catchall(
    z.union([
        z.record(z.string(), AttributeValueSchema),
        ITCStampSchema,
        z.number().int().positive()
    ])
).and(z.object({
    _itcStamp: ITCStampSchema.optional(),
    _timestamp: z.number().int().positive().optional()
}));

export type AttributeRecognitionsCollection = z.infer<typeof AttributeRecognitionsCollectionSchema>;

export const AttributeSubscriptionsSchema = z.record(
    z.string(),
    z.record(z.string(), z.string())
);

export type AttributeSubscriptions = z.infer<typeof AttributeSubscriptionsSchema>;

export const EntityIdMappingsSchema = z.record(
    z.string(),
    z.string()
);

export type EntityIdMappings = z.infer<typeof EntityIdMappingsSchema>;

// ═══════════════════════════════════════════════════════════════════
// HELPERS
// ═══════════════════════════════════════════════════════════════════

export function normalizeGlobalRecognitionWeights(
    weights: GlobalRecognitionWeights
): GlobalRecognitionWeights {
    const entries = Object.entries(weights);

    if (entries.length === 0) {
        return {};
    }

    const sum = entries.reduce((acc, [_, weight]) => acc + weight, 0);

    if (sum < 0.0001) {
        console.warn('[NORMALIZE] All recognition weights are zero or near-zero - treating as no recognition');
        return {};
    }

    const normalized: GlobalRecognitionWeights = {};
    for (const [key, weight] of entries) {
        normalized[key] = weight / sum;
    }

    return normalized;
}

export function validateGlobalRecognitionWeights(
    weights: GlobalRecognitionWeights,
    epsilon: number = 0.001
): boolean {
    const sum = Object.values(weights).reduce((a, b) => a + b, 0);
    return Math.abs(sum - 1.0) < epsilon;
}
