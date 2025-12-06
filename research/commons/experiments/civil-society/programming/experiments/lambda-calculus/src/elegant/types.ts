/**
 * Elegant Lambda Calculus Types (λ-R Compliant)
 * 
 * Type definitions that exactly match the λ-R specification:
 * - Filter τ = (τ → Bool) → Set τ → Set τ
 * - Limit τ = Dist τ → Dist τ
 * - Collective τ with proper structure
 * - Commons τ with proper structure
 * - SystemState with proper structure
 */

import type { Entity as CoreEntity, Real, Distribution, RecognitionMatrix } from '../core/types';

// ============================================================================
// Core λ-R Types (Re-exported from core)
// ============================================================================

/**
 * Entity type (Base entity type in λ-R)
 */
export type Entity = CoreEntity;

/**
 * Real numbers
 */
export type { Real };

/**
 * Distribution over τ (Dist τ in λ-R)
 */
export type { Distribution };

/**
 * Recognition Matrix
 */
export type { RecognitionMatrix };

// ============================================================================
// Filter Type (λ-R Specification)
// ============================================================================

/**
 * Filter τ = (τ → Bool) → Set τ → Set τ
 * 
 * From spec: Basic filter constructors take predicates and sets
 */
export type FilterPredicate<T> = (x: T) => boolean;
export type FilterFunction<T> = (set: Set<T>) => Set<T>;

/**
 * Type-level Filter (matches spec exactly)
 */
export type Filter<T> = (predicate: FilterPredicate<T>) => (set: Set<T>) => Set<T>;

/**
 * Entity filter specialized type
 */
export type EntityFilter = Filter<Entity>;

/**
 * Simple filter (ignores predicate parameter - practical variant)
 */
export type SimpleFilter<T> = (set: Set<T>) => Set<T>;

// ============================================================================
// Limit Type (λ-R Specification)
// ============================================================================

/**
 * Limit τ = Dist τ → Dist τ
 * 
 * From spec: Limits transform distributions
 */
export type Limit = (dist: Distribution) => Distribution;

/**
 * Limit constructors from spec
 */
export type LimitConstructor = (param: Real) => Limit;

// ============================================================================
// Collective Type (λ-R Specification)
// ============================================================================

/**
 * ShareType from spec
 */
export type ShareType = 'MRS' | 'SCMRS' | 'SCRMRS';

/**
 * Collective τ = {
 *   members : Set τ,
 *   filters : List (Filter τ),
 *   limits : List (Limit τ),
 *   share_type : ShareType
 * }
 * 
 * Matches λ-R spec exactly
 */
export interface Collective {
  id: string;
  members: Set<Entity>;
  filters: SimpleFilter<Entity>[];
  limits: Limit[];
  shareType: ShareType;
  metadata?: Record<string, unknown>;
}

/**
 * Provider type from spec
 */
export interface Provider {
  entity: Entity;
  capacity: Real;
  limits: Limit[];
}

/**
 * Recipient type from spec
 */
export interface Recipient {
  entity: Entity;
  need: Real;
  filters: SimpleFilter<Entity>[];
}

// ============================================================================
// Commons Type (λ-R Specification)
// ============================================================================

/**
 * Commons τ = {
 *   condition : τ → Bool,
 *   threshold : Real,
 *   resources : Real,
 *   members : Set τ,
 *   filters : List (Filter τ),
 *   limits : List (Limit τ)
 * }
 * 
 * Matches λ-R spec exactly
 */
export interface Commons {
  id: string;
  condition: (entity: Entity) => boolean;
  threshold: Real;
  resources: Record<string, Real>;  // Multiple resources (extension)
  members: Set<Entity>;
  filters: SimpleFilter<Entity>[];
  limits: Limit[];
  metadata?: Record<string, unknown>;
}

// ============================================================================
// HyperCollective Type (λ-R Specification)
// ============================================================================

/**
 * data HyperCollective τ where
 *   Base : Entity → HyperCollective τ
 *   Collective : Set (HyperCollective τ) → HyperCollective τ
 * 
 * Recursive structure from spec
 */
export type HyperCollective =
  | { type: 'base'; entity: Entity }
  | { type: 'collective'; members: Set<HyperCollective>; weights?: Map<HyperCollective, Real> };

// ============================================================================
// Allocation Types (λ-R Specification)
// ============================================================================

/**
 * Allocation : Entity → Entity → Real
 */
export type Allocation = (provider: Entity) => (recipient: Entity) => Real;

/**
 * Allocation result (extension for practical use)
 */
export interface AllocationResult {
  allocations: Distribution;
  iterations: number;
  converged: boolean;
}

// ============================================================================
// System State Type (λ-R Specification)
// ============================================================================

/**
 * type SystemState = {
 *   universe : Set Entity,
 *   recognition_matrix : Entity → Dist Entity,
 *   collectives : List (Collective Entity),
 *   commons : List (Commons Entity),
 *   hyper_collectives : List (HyperCollective Entity),
 *   allocations : Allocation,
 *   filters : List (Filter Entity),
 *   limits : List (Limit Entity)
 * }
 * 
 * Matches λ-R spec exactly
 */
export interface SystemState {
  universe: Set<Entity>;
  recognitionMatrix: RecognitionMatrix;
  collectives: Collective[];
  commons: Commons[];
  hyperCollectives?: HyperCollective[];
  allocations?: Allocation;
  filters?: SimpleFilter<Entity>[];
  limits?: Limit[];
  timestamp: Real;
  metadata?: Record<string, unknown>;
}

// ============================================================================
// Function Types (λ-R Primitives)
// ============================================================================

/**
 * Capacity function: Entity → Real
 */
export type CapacityFn = (entity: Entity) => Real;

/**
 * Need function: Entity → Real
 */
export type NeedFn = (entity: Entity) => Real;

/**
 * Predicate: τ → Bool
 */
export type Predicate<T> = (x: T) => boolean;

/**
 * Score function: τ → Real
 */
export type ScoreFn<T> = (x: T) => Real;

// ============================================================================
// Type Constructors (λ-R Style)
// ============================================================================

/**
 * Create a filter from a predicate (matches spec)
 */
export type FilterConstructor<T> = (predicate: Predicate<T>) => SimpleFilter<T>;

/**
 * Compose filters (matches spec)
 */
export type FilterComposer<T> = (f1: SimpleFilter<T>) => (f2: SimpleFilter<T>) => SimpleFilter<T>;

/**
 * Compose limits (matches spec)
 */
export type LimitComposer = (l1: Limit) => (l2: Limit) => Limit;

// ============================================================================
// Evolution Types
// ============================================================================

/**
 * System evolution function: SystemState → SystemState
 */
export type SystemEvolution = (state: SystemState) => SystemState;

/**
 * Commons evolution function: Commons → Commons
 */
export type CommonsEvolution = (commons: Commons) => Commons;

/**
 * Collective evolution function: Collective → Collective
 */
export type CollectiveEvolution = (collective: Collective) => Collective;

// ============================================================================
// Utility Types
// ============================================================================

/**
 * Convert Entity array to Set (utility)
 */
export const entitiesToSet = (entities: Entity[]): Set<Entity> => new Set(entities);

/**
 * Convert Set to Entity array (utility)
 */
export const setToEntities = (set: Set<Entity>): Entity[] => Array.from(set);

/**
 * Extract entity IDs from set
 */
export const entityIds = (entities: Set<Entity>): Set<string> => 
  new Set(Array.from(entities).map(e => e.id));

/**
 * Find entity by ID in set
 */
export const findEntityById = (id: string) => (entities: Set<Entity>): Entity | undefined =>
  Array.from(entities).find(e => e.id === id);

// ============================================================================
// Type Guards
// ============================================================================

/**
 * Type guard for HyperCollective base
 */
export const isHyperCollectiveBase = (hc: HyperCollective): hc is { type: 'base'; entity: Entity } =>
  hc.type === 'base';

/**
 * Type guard for HyperCollective collective
 */
export const isHyperCollectiveCollective = (
  hc: HyperCollective
): hc is { type: 'collective'; members: Set<HyperCollective>; weights?: Map<HyperCollective, Real> } =>
  hc.type === 'collective';

// ============================================================================
// Export all types
// ============================================================================

export type {
  // Re-exports from core
  CoreEntity,
};

