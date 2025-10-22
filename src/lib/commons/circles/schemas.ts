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

/**
 * Commitment - A participant's declaration of needs, capacity, and recognition
 * 
 * Published by each participant to declare:
 * - Their current residual need
 * - Their stated need (unchanging reference)
 * - Their available capacity (if provider)
 * - Their recognition weights (MR values)
 * - Adaptive damping state
 */
export const CommitmentSchema = z.object({
	// Need state
	residual_need: z.number().nonnegative(),
	stated_need: z.number().nonnegative(),
	
	// Capacity (if provider)
	capacity: z.number().nonnegative().optional(),
	
	// Recognition (mutual and one-way)
	mr_values: z.record(z.string(), z.number().nonnegative()).optional(),
	recognition_weights: z.record(z.string(), z.number().nonnegative()).optional(),
	
	// Capacity filters
	capacity_filters: z.record(z.string(), CapacityFilterSchema).optional(),
	
	// Adaptive damping
	damping_factor: z.number().min(0).max(1).optional(),
	over_allocation_history: z.array(z.number().nonnegative()).max(3).optional(),
	
	// Metadata
	timestamp: z.number().int().positive(),
	vectorClock: VectorClockSchema.optional(),
	round: z.number().int().nonnegative().optional()
});

/**
 * Two-Tier Allocation State - Result of allocation computation
 * 
 * Published by providers after computing allocations:
 * - Denominators for both tiers
 * - Computed allocations for both tiers
 */
export const TwoTierAllocationStateSchema = z.object({
	// Tier 1: Mutual recognition denominators
	mutualDenominator: z.record(z.string(), z.number().nonnegative()),
	
	// Tier 2: Non-mutual denominators
	nonMutualDenominator: z.record(z.string(), z.number().nonnegative()),
	
	// Computed allocations by tier
	mutualAllocations: z.record(z.string(), z.record(z.string(), z.number().nonnegative())),
	nonMutualAllocations: z.record(z.string(), z.record(z.string(), z.number().nonnegative())),
	
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

