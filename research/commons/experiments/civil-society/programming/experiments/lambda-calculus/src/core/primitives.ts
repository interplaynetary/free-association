/**
 * Lambda Calculus Implementation: Primitive Operations
 * 
 * This module implements primitive operations for:
 * - Mathematical operations
 * - Set operations
 * - Distribution operations
 */

import type { Real, Distribution, Entity } from './types';
import { normalize, getProb } from './types';

// ============================================================================
// Mathematical Operations
// ============================================================================

export const add = (a: Real, b: Real): Real => a + b;
export const subtract = (a: Real, b: Real): Real => a - b;
export const multiply = (a: Real, b: Real): Real => a * b;
export const divide = (a: Real, b: Real): Real => (b !== 0 ? a / b : 0);
export const min = (a: Real, b: Real): Real => Math.min(a, b);
export const max = (a: Real, b: Real): Real => Math.max(a, b);
export const pow = (base: Real, exponent: Real): Real => Math.pow(base, exponent);

export const gte = (a: Real, b: Real): boolean => a >= b;
export const lte = (a: Real, b: Real): boolean => a <= b;
export const eq = (a: Real, b: Real): boolean => Math.abs(a - b) < 1e-10;
export const gt = (a: Real, b: Real): boolean => a > b;
export const lt = (a: Real, b: Real): boolean => a < b;

// ============================================================================
// Set Operations
// ============================================================================

/**
 * Check if element is in set
 */
export function has<T>(set: Set<T>, element: T): boolean {
  return set.has(element);
}

/**
 * Check if set1 is subset of set2
 */
export function isSubset<T>(set1: Set<T>, set2: Set<T>): boolean {
  for (const elem of set1) {
    if (!set2.has(elem)) return false;
  }
  return true;
}

/**
 * Union of sets
 */
export function union<T>(set1: Set<T>, set2: Set<T>): Set<T> {
  return new Set([...set1, ...set2]);
}

/**
 * Intersection of sets
 */
export function intersection<T>(set1: Set<T>, set2: Set<T>): Set<T> {
  const result = new Set<T>();
  for (const elem of set1) {
    if (set2.has(elem)) result.add(elem);
  }
  return result;
}

/**
 * Set difference (set1 \ set2)
 */
export function difference<T>(set1: Set<T>, set2: Set<T>): Set<T> {
  const result = new Set<T>();
  for (const elem of set1) {
    if (!set2.has(elem)) result.add(elem);
  }
  return result;
}

/**
 * Cardinality (size) of set
 */
export function cardinality<T>(set: Set<T>): Real {
  return set.size;
}

/**
 * Filter set by predicate
 */
export function filterSet<T>(predicate: (item: T) => boolean, set: Set<T>): Set<T> {
  const result = new Set<T>();
  for (const elem of set) {
    if (predicate(elem)) result.add(elem);
  }
  return result;
}

/**
 * Map over set
 */
export function mapSet<T, U>(fn: (item: T) => U, set: Set<T>): Set<U> {
  const result = new Set<U>();
  for (const elem of set) {
    result.add(fn(elem));
  }
  return result;
}

// ============================================================================
// Distribution Operations
// ============================================================================

/**
 * Create a distribution from a function over a domain
 */
export function createDistFromFunction<T>(
  fn: (item: T) => Real,
  domain: Set<T>,
  getId: (item: T) => string
): Distribution {
  const weights: Record<string, Real> = {};
  for (const item of domain) {
    const weight = fn(item);
    if (weight > 0) {
      weights[getId(item)] = weight;
    }
  }
  return normalize(weights);
}

/**
 * Expectation of a function under a distribution
 */
export function expectation(
  fn: (entityId: string) => Real,
  dist: Distribution
): Real {
  let sum = 0;
  for (const [id, weight] of Object.entries(dist.weights)) {
    sum += fn(id) * (weight / dist.total);
  }
  return sum;
}

/**
 * Map a distribution through a function
 */
export function mapDist(
  fn: (entityId: string) => string,
  dist: Distribution
): Distribution {
  const newWeights: Record<string, Real> = {};
  for (const [id, weight] of Object.entries(dist.weights)) {
    const newId = fn(id);
    newWeights[newId] = (newWeights[newId] || 0) + weight;
  }
  return normalize(newWeights);
}

/**
 * Scale all weights in a distribution by a constant
 */
export function scaleDist(scale: Real, dist: Distribution): Distribution {
  const newWeights: Record<string, Real> = {};
  for (const [id, weight] of Object.entries(dist.weights)) {
    newWeights[id] = weight * scale;
  }
  return { weights: newWeights, total: dist.total * scale };
}

/**
 * Add two distributions (convex combination with equal weights)
 */
export function addDist(dist1: Distribution, dist2: Distribution): Distribution {
  const newWeights: Record<string, Real> = { ...dist1.weights };
  for (const [id, weight] of Object.entries(dist2.weights)) {
    newWeights[id] = (newWeights[id] || 0) + weight;
  }
  return normalize(newWeights);
}

/**
 * Combine distributions with weights
 */
export function combineDists(dists: Distribution[], weights: Real[]): Distribution {
  if (dists.length !== weights.length || dists.length === 0) {
    return { weights: {}, total: 0 };
  }

  const combined: Record<string, Real> = {};
  for (let i = 0; i < dists.length; i++) {
    const dist = dists[i];
    const weight = weights[i];
    for (const [id, w] of Object.entries(dist.weights)) {
      combined[id] = (combined[id] || 0) + (w / dist.total) * weight;
    }
  }
  return normalize(combined);
}

/**
 * Apply a pointwise transformation to distribution weights
 */
export function transformDist(
  fn: (weight: Real, entityId: string) => Real,
  dist: Distribution
): Distribution {
  const newWeights: Record<string, Real> = {};
  for (const [id, weight] of Object.entries(dist.weights)) {
    const normalizedWeight = weight / dist.total;
    const transformed = fn(normalizedWeight, id);
    if (transformed > 0) {
      newWeights[id] = transformed;
    }
  }
  return normalize(newWeights);
}

// ============================================================================
// Summation Operations
// ============================================================================

/**
 * Sum over a set
 */
export function sumOver<T>(fn: (item: T) => Real, set: Set<T>): Real {
  let sum = 0;
  for (const item of set) {
    sum += fn(item);
  }
  return sum;
}

/**
 * Sum over distribution
 */
export function sumOverDist(fn: (entityId: string) => Real, dist: Distribution): Real {
  return expectation(fn, dist);
}

/**
 * Sum over pairs from two sets
 */
export function sumOverPairs<T, U>(
  fn: (item1: T, item2: U) => Real,
  set1: Set<T>,
  set2: Set<U>
): Real {
  let sum = 0;
  for (const item1 of set1) {
    for (const item2 of set2) {
      sum += fn(item1, item2);
    }
  }
  return sum;
}

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Create entity ID from entity
 */
export function entityId(entity: Entity): string {
  return entity.id;
}

/**
 * Find entity by ID in a set
 */
export function findEntityById(entities: Set<Entity>, id: string): Entity | undefined {
  for (const entity of entities) {
    if (entity.id === id) return entity;
  }
  return undefined;
}

/**
 * Convert set of entities to set of IDs
 */
export function entitiesToIds(entities: Set<Entity>): Set<string> {
  return mapSet(entityId, entities);
}

/**
 * Average of values
 */
export function average(values: Real[]): Real {
  if (values.length === 0) return 0;
  return values.reduce((sum, v) => sum + v, 0) / values.length;
}

/**
 * Clamp value between min and max
 */
export function clamp(value: Real, minVal: Real, maxVal: Real): Real {
  return Math.max(minVal, Math.min(maxVal, value));
}

