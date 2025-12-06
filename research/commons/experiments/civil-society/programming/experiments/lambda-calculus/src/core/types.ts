/**
 * Lambda Calculus Implementation: Core Types and Zod Schemas
 * 
 * This module defines the core types for the Recognition Calculus (λ-R)
 * with Zod schemas for runtime validation and type inference.
 */

import { z } from 'zod';
import type { SparseRecognitionGraph } from '../sparse/types';

// ============================================================================
// Base Types
// ============================================================================

/**
 * Entity: Base entity type
 * Entities are identified by unique IDs and have optional metadata
 */
export const EntitySchema = z.object({
  id: z.string(),
  name: z.string().optional(),
  metadata: z.record(z.unknown()).optional(),
  lastActive: z.number().optional(), // timestamp
});

export type Entity = z.infer<typeof EntitySchema>;

/**
 * Real: Real numbers (represented as JavaScript numbers)
 */
export const RealSchema = z.number();
export type Real = z.infer<typeof RealSchema>;

/**
 * Universe: Set of all entities
 */
export const UniverseSchema = z.set(EntitySchema);
export type Universe = z.infer<typeof UniverseSchema>;

// ============================================================================
// Distribution Type
// ============================================================================

/**
 * Distribution: A probability distribution over entities
 * Represented as a normalized map from entity IDs to probabilities
 */
export const DistributionSchema = z.object({
  weights: z.record(z.string(), RealSchema),
  total: RealSchema,
});

export type Distribution = z.infer<typeof DistributionSchema>;

/**
 * Helper: Create a distribution from weights
 */
export function createDistribution(weights: Record<string, Real>): Distribution {
  const total = Object.values(weights).reduce((sum, w) => sum + w, 0);
  return { weights, total };
}

/**
 * Helper: Normalize a distribution
 */
export function normalize(weights: Record<string, Real>): Distribution {
  const total = Object.values(weights).reduce((sum, w) => sum + w, 0);
  if (total === 0) {
    return { weights: {}, total: 0 };
  }
  const normalized = Object.fromEntries(
    Object.entries(weights).map(([id, w]) => [id, w / total])
  );
  return { weights: normalized, total: 1 };
}

/**
 * Helper: Create a Dirac delta distribution (point mass)
 */
export function diracDelta(entityId: string): Distribution {
  return { weights: { [entityId]: 1 }, total: 1 };
}

/**
 * Helper: Get probability for an entity in a distribution
 */
export function getProb(dist: Distribution, entityId: string): Real {
  if (dist.total === 0) return 0;
  return (dist.weights[entityId] || 0) / dist.total;
}

// ============================================================================
// Recognition Matrix Type
// ============================================================================

/**
 * RecognitionMatrix: Maps entity pairs to recognition values
 * R(e, f) represents how much entity e recognizes entity f
 */
export const RecognitionMatrixSchema = z.object({
  // Nested Record representation (semi-sparse - only stores defined keys)
  matrix: z.record(z.string(), z.record(z.string(), RealSchema)),
});

/**
 * Recognition Matrix type with optional sparse representation
 * 
 * Can contain either:
 * - matrix: Record-based representation (existing format)
 * - sparse: True sparse representation using Maps (new format)
 * - both: For transition/compatibility
 */
export type RecognitionMatrix = z.infer<typeof RecognitionMatrixSchema> & {
  sparse?: SparseRecognitionGraph;
};

/**
 * Helper: Get recognition value
 * Works with both Record and sparse representations
 */
export function getRecognition(
  matrix: RecognitionMatrix,
  fromId: string,
  toId: string
): Real {
  // Prefer sparse if available
  if (matrix.sparse) {
    const fromEdges = matrix.sparse.edges.get(fromId);
    return fromEdges?.get(toId) ?? 0;
  }
  // Fall back to Record representation
  return matrix.matrix[fromId]?.[toId] ?? 0;
}

/**
 * Helper: Check if matrix has sparse representation
 */
export function hasSparseRepresentation(matrix: RecognitionMatrix): boolean {
  return matrix.sparse !== undefined;
}

/**
 * Helper: Convert matrix to sparse representation if not already
 */
export function ensureSparseRepresentation(matrix: RecognitionMatrix): RecognitionMatrix {
  if (matrix.sparse) {
    return matrix;
  }
  
  // Convert Record to sparse
  const { toSparse } = require('../sparse/types');
  return {
    ...matrix,
    sparse: toSparse(matrix.matrix)
  };
}

/**
 * Helper: Set recognition value
 */
export function setRecognition(
  matrix: RecognitionMatrix,
  fromId: string,
  toId: string,
  value: Real
): RecognitionMatrix {
  return {
    matrix: {
      ...matrix.matrix,
      [fromId]: {
        ...matrix.matrix[fromId],
        [toId]: value,
      },
    },
  };
}

// ============================================================================
// Filter Type
// ============================================================================

/**
 * Filter: A function that filters a set based on predicates
 */
export type FilterFn<T> = (predicate: (item: T) => boolean, set: Set<T>) => Set<T>;

export const FilterTypeSchema = z.enum(['attribute', 'mrd', 'time', 'composite']);
export type FilterType = z.infer<typeof FilterTypeSchema>;

export const FilterSchema = z.object({
  type: FilterTypeSchema,
  name: z.string(),
  // Parameters depend on filter type
  params: z.record(z.unknown()),
});

export type Filter = z.infer<typeof FilterSchema>;

// ============================================================================
// Limit Type
// ============================================================================

/**
 * Limit: A function that transforms a distribution
 */
export type LimitFn = (dist: Distribution) => Distribution;

export const LimitTypeSchema = z.enum(['cap', 'floor', 'progressive', 'type']);
export type LimitType = z.infer<typeof LimitTypeSchema>;

export const LimitSchema = z.object({
  type: LimitTypeSchema,
  name: z.string(),
  // Parameters depend on limit type
  params: z.record(z.unknown()),
});

export type Limit = z.infer<typeof LimitSchema>;

// ============================================================================
// Share Type
// ============================================================================

export const ShareTypeSchema = z.enum(['MRS', 'SCMRS', 'SCRMRS']);
export type ShareType = z.infer<typeof ShareTypeSchema>;

// ============================================================================
// Collective Type
// ============================================================================

export const CollectiveSchema = z.object({
  id: z.string(),
  members: z.set(z.string()), // entity IDs
  filters: z.array(FilterSchema),
  limits: z.array(LimitSchema),
  shareType: ShareTypeSchema,
  metadata: z.record(z.unknown()).optional(),
});

export type Collective = z.infer<typeof CollectiveSchema>;

// ============================================================================
// Commons Type
// ============================================================================

export const CommonsSchema = z.object({
  id: z.string(),
  condition: z.function().args(EntitySchema).returns(z.boolean()),
  threshold: RealSchema,
  resources: RealSchema,
  members: z.set(z.string()), // entity IDs
  filters: z.array(FilterSchema),
  limits: z.array(LimitSchema),
  metadata: z.record(z.unknown()).optional(),
});

export type Commons = z.infer<typeof CommonsSchema>;

// ============================================================================
// Provider and Recipient Types
// ============================================================================

export const ProviderSchema = z.object({
  entity: EntitySchema,
  capacity: RealSchema,
  limits: z.array(LimitSchema),
});

export type Provider = z.infer<typeof ProviderSchema>;

export const RecipientSchema = z.object({
  entity: EntitySchema,
  need: RealSchema,
  filters: z.array(FilterSchema),
});

export type Recipient = z.infer<typeof RecipientSchema>;

// ============================================================================
// Allocation Type
// ============================================================================

/**
 * Allocation: Maps provider-recipient pairs to allocation amounts
 */
export const AllocationSchema = z.object({
  allocations: z.record(z.string(), z.record(z.string(), RealSchema)),
});

export type Allocation = z.infer<typeof AllocationSchema>;

/**
 * Helper: Get allocation amount
 */
export function getAllocation(
  allocation: Allocation,
  providerId: string,
  recipientId: string
): Real {
  return allocation.allocations[providerId]?.[recipientId] || 0;
}

/**
 * Helper: Set allocation amount
 */
export function setAllocation(
  allocation: Allocation,
  providerId: string,
  recipientId: string,
  amount: Real
): Allocation {
  return {
    allocations: {
      ...allocation.allocations,
      [providerId]: {
        ...allocation.allocations[providerId],
        [recipientId]: amount,
      },
    },
  };
}

// ============================================================================
// Hyper-Collective Type (Recursive)
// ============================================================================

export type HyperCollective =
  | { type: 'base'; entity: Entity }
  | { type: 'collective'; members: Set<HyperCollective>; weights?: Map<HyperCollective, Real> };

// Note: Recursive Zod schemas are complex, so we use z.any() for now
// Full validation can be done with custom validators if needed
export const HyperCollectiveSchema: z.ZodType<HyperCollective> = z.any() as z.ZodType<HyperCollective>;

// ============================================================================
// System State Type
// ============================================================================

export const SystemStateSchema = z.object({
  universe: z.set(EntitySchema),
  recognitionMatrix: RecognitionMatrixSchema,
  collectives: z.array(CollectiveSchema),
  commons: z.array(CommonsSchema),
  hyperCollectives: z.array(HyperCollectiveSchema),
  allocations: AllocationSchema,
  filters: z.array(FilterSchema),
  limits: z.array(LimitSchema),
  timestamp: z.number(),
  metadata: z.record(z.unknown()).optional(),
});

export type SystemState = z.infer<typeof SystemStateSchema>;

// ============================================================================
// Configuration Types
// ============================================================================

export const ConfigSchema = z.object({
  learningRate: RealSchema.default(0.1),
  convergenceThreshold: RealSchema.default(0.001),
  maxIterations: z.number().int().positive().default(100),
  mrdLeavingFactor: RealSchema.default(0.5),
});

export type Config = z.infer<typeof ConfigSchema>;

