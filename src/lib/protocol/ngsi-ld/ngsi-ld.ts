import { z } from "zod";

// --- 0. Shared Primitives ---

/**
 * URI / IRI string with semantic description.
 * Used for IDs, Types, and references.
 */
export const IRI = z.string().min(1).describe("IRI / URI");
export type IRI = string;

// --- 1. Literal Value Schema ---

/**
 * TypeScript Type definition for LiteralValue to avoid circular inference.
 */
export type LiteralValue =
    | string
    | number
    | boolean
    | null
    | LiteralValue[]
    | { [key: string]: LiteralValue };

/**
 * Primitive leaf values in the NGSI-LD graph.
 * Recursion is halted here (except for array/record recursion which is structural, not attribute-based).
 */
export const LiteralValue: z.ZodType<LiteralValue> = z.lazy(() =>
    z.union([
        z.string(),
        z.number(),
        z.boolean(),
        z.null(),
        z.array(LiteralValue),
        z.record(LiteralValue),
    ]),
);

// --- 2. Explicit Type Definitions ---

export type Attribute = Property | Relationship;

/**
 * A map of attributes, supporting parallelism via arrays.
 * Key: Attribute name (e.g., "temperature", "locatedIn")
 * Value: Single Attribute or Array of Attributes (different datasetIds)
 */
export type AttributeMap = Record<string, Attribute | Attribute[]>;

/**
 * Shared metadata fields for all NGSI-LD attributes (Properties and Relationships).
 * These fields establish provenance, temporal validity, and graph structure.
 */
export interface BaseAttribute {
    // --- Temporal & Provenance (The "Core 3") ---

    /**
     * When the value was measured in the real world.
     * Crucial for sensor data, events, and historical queries.
     * @example "2026-01-22T10:12:00Z"
     */
    observedAt?: string;

    /**
     * When this attribute instance was created in the system.
     * System time, immutable.
     */
    createdAt?: string;

    /**
     * When this attribute instance last changed.
     * System time, useful for sync and caching.
     */
    modifiedAt?: string;

    // --- Identity & Semantics ---

    /**
     * Distinguishes parallel instances of the same attribute.
     * REQUIRED if you have multiple sources for the same property (e.g. "sensorA" vs "sensorB").
     * datasetId = "according to X" (Logical grouping)
     * @example "urn:ngsi-ld:Dataset:sensorA"
     */
    datasetId?: string;

    /**
     * Stable identity for this specific attribute node in the graph.
     * Rarely needed unless doing advanced provenance or RDF linking.
     * instanceId = "this node right here" (Physical node identity)
     */
    instanceId?: string;

    /**
     * Explicit unit code (UN/CEFACT).
     * @example "CEL" for Celsius
     */
    unitCode?: string;

    // --- Validity / Temporal Scope ---

    /**
     * When the attribute stops being valid.
     * Useful for leases, temporary keys, or authorizations.
     */
    expires?: string;

    /**
     * Validity start time.
     * Especially useful for Relationships (e.g. "employedBy" since 2020).
     */
    start?: string;

    /**
     * Validity end time.
     */
    end?: string;

    // --- Recursion ---

    /**
     * Nested attributes (Properties of Properties, or Properties of Relationships).
     * This enables the infinite property graph.
     */
    attributes?: AttributeMap;
}

/**
 * A Property represents a tangible value (state) associated with an entity.
 */
export interface Property extends BaseAttribute {
    type: "Property";
    value: LiteralValue;
}

/**
 * A Relationship represents a directed link to another Entity.
 */
export interface Relationship extends BaseAttribute {
    type: "Relationship";
    /**
     * The target Entity ID (URI/IRI).
     * @example "urn:ngsi-ld:Room:101"
     */
    object: string;
}

// --- 3. Recursive Schema Definition (Zod) ---

// Helper for temporal consistency checks (start <= end)
const temporalRefinement = (data: { start?: string; end?: string }) => {
    if (data.start && data.end) {
        return new Date(data.start) <= new Date(data.end);
    }
    return true;
};
const temporalRefinementMsg = { message: "`start` must be <= `end`" };

// Helper for generic BaseAttribute fields
const BaseAttributeShape = {
    observedAt: z.string().datetime().describe("Real-world measurement time").optional(),
    createdAt: z.string().datetime().describe("System creation time").optional(),
    modifiedAt: z.string().datetime().describe("System modification time").optional(),

    datasetId: IRI.describe("Logical source identifier (parallelism)").optional(),
    instanceId: IRI.describe("Physical node identifier (graph ID)").optional(),
    unitCode: z.string().describe("UN/CEFACT unit code").optional(),

    expires: z.string().datetime().describe("Validity expiration").optional(),
    start: z.string().datetime().describe("Validity start").optional(),
    end: z.string().datetime().describe("Validity end").optional(),
};

// We define AttributeOrArray lazy wrapper first to use inside schemas
export const AttributeOrArray: z.ZodType<Attribute | Attribute[]> = z.lazy(() =>
    z.union([Attribute, z.array(Attribute)]),
);

/**
 * Schema for Property (with temporal validation using refine)
 */
export const PropertySchema = z
    .object({
        type: z.literal("Property"),
        value: LiteralValue,
        ...BaseAttributeShape,
        attributes: z.record(AttributeOrArray).describe("Nested attributes").optional(),
    })
    .refine(temporalRefinement, temporalRefinementMsg);

/**
 * Schema for Relationship (with temporal validation)
 */
export const RelationshipSchema = z
    .object({
        type: z.literal("Relationship"),
        object: IRI.describe("Target Entity ID (URI)"),
        ...BaseAttributeShape,
        attributes: z.record(AttributeOrArray).describe("Nested attributes").optional(),
    })
    .refine(temporalRefinement, temporalRefinementMsg);

// Using z.lazy for the recursive Attribute type
// Switched to z.union because PropertySchema/RelationshipSchema are ZodEffects (refined)
// and z.discriminatedUnion only supports raw ZodObjects.
export const Attribute: z.ZodType<Attribute> = z.lazy(() =>
    z.union([PropertySchema, RelationshipSchema]),
);

// --- 4. Entity Schema ---

/**
 * An NGSI-LD Entity.
 * The fundamental node in the graph.
 */
export const Entity = z.object({
    id: IRI.describe("Unique Entity URI"),
    type: z.string().describe("Entity Type/Class"),

    // Entity-level metadata
    createdAt: z.string().datetime().describe("Entity creation time").optional(),
    modifiedAt: z.string().datetime().describe("Entity modification time").optional(),

    attributes: z.record(AttributeOrArray).describe("Map of Properties and Relationships"),
});

export type Entity = z.infer<typeof Entity>;
