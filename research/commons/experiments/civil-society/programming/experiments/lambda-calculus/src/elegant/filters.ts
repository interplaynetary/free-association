/**
 * Elegant Lambda Calculus: Filter System
 * 
 * Fully curried filter implementation following lambda calculus principles:
 * - Filter τ = (τ → Bool) → Set τ → Set τ
 * - Full currying for composition
 * - Point-free style where possible
 */

import type { Entity, Real, RecognitionMatrix } from '../core/types';
import { mrd as mrdCore } from '../core/recognition';
import { pipe, compose2 } from './combinators';

// ============================================================================
// Filter Type (Pure Functional)
// ============================================================================

/**
 * Filter type following lambda calculus spec:
 * Filter τ = (τ → Bool) → Set τ → Set τ
 */
export type Filter<T> = (predicate: (x: T) => boolean) => (set: Set<T>) => Set<T>;

/**
 * Entity filter specialized type
 */
export type EntityFilter = Filter<Entity>;

// ============================================================================
// Basic Filter Constructors (Curried)
// ============================================================================

/**
 * Attribute filter: (τ → Bool) → Filter τ
 * Following spec: attr_filter = λpred:τ→Bool. λs:Set τ. filter pred s
 */
export const attrFilter = <T>(predicate: (x: T) => boolean): Filter<T> =>
  (_ignoredPredicate: (x: T) => boolean) =>
  (set: Set<T>): Set<T> => {
    const result = new Set<T>();
    for (const item of set) {
      if (predicate(item)) {
        result.add(item);
      }
    }
    return result;
  };

/**
 * Simplified attribute filter (ignores predicate parameter)
 */
export const attr = <T>(predicate: (x: T) => boolean) =>
  (set: Set<T>): Set<T> => {
    const result = new Set<T>();
    for (const item of set) {
      if (predicate(item)) {
        result.add(item);
      }
    }
    return result;
  };

/**
 * MRD filter: Real → RecognitionMatrix → Set Entity → Set Entity
 * Following spec: mrd_filter = λθ:Real. λs:Set Entity. filter (λe. MRD_s e ≥ θ) s
 */
export const mrdFilter = (threshold: Real) =>
  (matrix: RecognitionMatrix) =>
  (set: Set<Entity>): Set<Entity> => {
    const entityIds = new Set(Array.from(set).map(e => e.id));
    const result = new Set<Entity>();
    
    for (const entity of set) {
      const entityMRD = mrdCore(matrix, entity.id, entityIds);
      if (entityMRD >= threshold) {
        result.add(entity);
      }
    }
    
    return result;
  };

/**
 * Time filter: Real → Set Entity → Set Entity
 * Following spec: time_filter = λt:Real. λs:Set Entity. filter (λe. last_active(e) ≥ t) s
 */
export const timeFilter = (minTimestamp: Real) =>
  (set: Set<Entity>): Set<Entity> => {
    const result = new Set<Entity>();
    for (const entity of set) {
      const lastActive = entity.lastActive ?? 0;
      if (lastActive >= minTimestamp) {
        result.add(entity);
      }
    }
    return result;
  };

// ============================================================================
// Filter Composition (Following Lambda Calculus)
// ============================================================================

/**
 * Compose filters: Filter τ → Filter τ → Filter τ
 * Following spec: compose_filters = λf₁. λf₂. λpred. λs. f₁ pred (f₂ pred s)
 */
export const composeFilters = <T>(f1: Filter<T>) =>
  (f2: Filter<T>): Filter<T> =>
  (predicate: (x: T) => boolean) =>
  (set: Set<T>): Set<T> =>
    f1(predicate)(f2(predicate)(set));

/**
 * Compose multiple filters
 */
export const composeFilterList = <T>(filters: Filter<T>[]): Filter<T> | null => {
  if (filters.length === 0) return null;
  if (filters.length === 1) return filters[0];
  return filters.reduce((acc, curr) => composeFilters(acc)(curr));
};

/**
 * Sequential filter composition (simpler - ignores predicate)
 */
export const seqFilter = <T>(f1: (set: Set<T>) => Set<T>) =>
  (f2: (set: Set<T>) => Set<T>) =>
  (set: Set<T>): Set<T> =>
    f2(f1(set));

// ============================================================================
// Specialized Filters (Curried)
// ============================================================================

/**
 * ID filter: Set String → Entity → Set Entity → Set Entity
 */
export const idFilter = (ids: Set<string>) =>
  attr<Entity>((entity) => ids.has(entity.id));

/**
 * Metadata filter: String → Unknown → Set Entity → Set Entity
 */
export const metadataFilter = (key: string) =>
  (value: unknown) =>
  attr<Entity>((entity) => entity.metadata?.[key] === value);

/**
 * Name pattern filter: RegExp → Set Entity → Set Entity
 */
export const nameFilter = (pattern: RegExp) =>
  attr<Entity>((entity) => pattern.test(entity.name || ''));

/**
 * Top N filter: Number → (Entity → Real) → Set Entity → Set Entity
 */
export const topN = (n: number) =>
  (scoreFn: (entity: Entity) => Real) =>
  (set: Set<Entity>): Set<Entity> => {
    const sorted = Array.from(set).sort((a, b) => scoreFn(b) - scoreFn(a));
    return new Set(sorted.slice(0, n));
  };

/**
 * Bottom N filter: Number → (Entity → Real) → Set Entity → Set Entity
 */
export const bottomN = (n: number) =>
  (scoreFn: (entity: Entity) => Real) =>
  (set: Set<Entity>): Set<Entity> => {
    const sorted = Array.from(set).sort((a, b) => scoreFn(a) - scoreFn(b));
    return new Set(sorted.slice(0, n));
  };

/**
 * Percentile filter: Real → Real → (Entity → Real) → Set Entity → Set Entity
 */
export const percentile = (minPct: Real) =>
  (maxPct: Real) =>
  (scoreFn: (entity: Entity) => Real) =>
  (set: Set<Entity>): Set<Entity> => {
    const sorted = Array.from(set).sort((a, b) => scoreFn(a) - scoreFn(b));
    const minIdx = Math.floor((minPct / 100) * sorted.length);
    const maxIdx = Math.ceil((maxPct / 100) * sorted.length);
    return new Set(sorted.slice(minIdx, maxIdx));
  };

// ============================================================================
// Logical Filter Combinators (Curried)
// ============================================================================

/**
 * AND filter: (Set → Set) → (Set → Set) → Set → Set
 */
export const andFilter = <T>(f1: (s: Set<T>) => Set<T>) =>
  (f2: (s: Set<T>) => Set<T>) =>
  (set: Set<T>): Set<T> => {
    const result1 = f1(set);
    const result2 = f2(set);
    const intersection = new Set<T>();
    for (const item of result1) {
      if (result2.has(item)) {
        intersection.add(item);
      }
    }
    return intersection;
  };

/**
 * OR filter: (Set → Set) → (Set → Set) → Set → Set
 */
export const orFilter = <T>(f1: (s: Set<T>) => Set<T>) =>
  (f2: (s: Set<T>) => Set<T>) =>
  (set: Set<T>): Set<T> => {
    const result1 = f1(set);
    const result2 = f2(set);
    return new Set([...result1, ...result2]);
  };

/**
 * NOT filter: (Set → Set) → Set → Set
 */
export const notFilter = <T>(f: (s: Set<T>) => Set<T>) =>
  (set: Set<T>): Set<T> => {
    const filtered = f(set);
    const complement = new Set<T>();
    for (const item of set) {
      if (!filtered.has(item)) {
        complement.add(item);
      }
    }
    return complement;
  };

// ============================================================================
// Higher-Order Filter Operations
// ============================================================================

/**
 * Apply filter: Filter τ → Set τ → Set τ
 */
export const applyFilter = <T>(filter: Filter<T>) =>
  (predicate: (x: T) => boolean) =>
  (set: Set<T>): Set<T> =>
    filter(predicate)(set);

/**
 * Apply simple filter (no predicate)
 */
export const apply = <T>(filterFn: (s: Set<T>) => Set<T>) =>
  (set: Set<T>): Set<T> =>
    filterFn(set);

/**
 * Apply multiple filters sequentially
 */
export const applyFilters = <T>(filters: Array<(s: Set<T>) => Set<T>>) =>
  (set: Set<T>): Set<T> =>
    filters.reduce((acc, filter) => filter(acc), set);

// ============================================================================
// Utility Filters (Curried)
// ============================================================================

/**
 * Identity filter: Set → Set
 */
export const identity = <T>() =>
  (set: Set<T>): Set<T> =>
    set;

/**
 * Empty filter: Set → Set
 */
export const empty = <T>() =>
  (_set: Set<T>): Set<T> =>
    new Set<T>();

/**
 * Threshold filter by score: Real → (T → Real) → Set T → Set T
 */
export const threshold = <T>(minScore: Real) =>
  (scoreFn: (x: T) => Real) =>
  (set: Set<T>): Set<T> => {
    const result = new Set<T>();
    for (const item of set) {
      if (scoreFn(item) >= minScore) {
        result.add(item);
      }
    }
    return result;
  };

/**
 * Range filter: Real → Real → (T → Real) → Set T → Set T
 */
export const range = <T>(min: Real) =>
  (max: Real) =>
  (scoreFn: (x: T) => Real) =>
  (set: Set<T>): Set<T> => {
    const result = new Set<T>();
    for (const item of set) {
      const score = scoreFn(item);
      if (score >= min && score <= max) {
        result.add(item);
      }
    }
    return result;
  };

// ============================================================================
// Export curried operations
// ============================================================================

export const curriedFilters = {
  attr,
  mrdFilter,
  timeFilter,
  idFilter,
  metadataFilter,
  nameFilter,
  topN,
  bottomN,
  percentile,
  andFilter,
  orFilter,
  notFilter,
  threshold,
  range,
  identity,
  empty,
};

