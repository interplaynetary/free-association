/**
 * Lambda Calculus Implementation: Filter System
 * 
 * This module implements the filter system for:
 * - Attribute filters
 * - MRD filters
 * - Time filters
 * - Filter composition
 */

import type { Entity, Real, Filter, RecognitionMatrix } from './types';
import { filterSet, entitiesToIds } from './primitives';
import { mrd } from './recognition';

// ============================================================================
// Filter Type Definitions
// ============================================================================

export type FilterFunction = (entities: Set<Entity>) => Set<Entity>;

// ============================================================================
// Basic Filter Constructors
// ============================================================================

/**
 * Attribute filter: Filter entities based on a predicate
 */
export function attrFilter(
  name: string,
  predicate: (entity: Entity) => boolean
): { filter: Filter; fn: FilterFunction } {
  const filter: Filter = {
    type: 'attribute',
    name,
    params: { predicate: predicate.toString() },
  };

  const fn: FilterFunction = (entities: Set<Entity>) => {
    return filterSet(predicate, entities);
  };

  return { filter, fn };
}

/**
 * MRD filter: Filter entities based on Mutual Recognition Density threshold
 * Keeps entities with MRD >= threshold
 */
export function mrdFilter(
  name: string,
  threshold: Real,
  matrix: RecognitionMatrix,
  referenceSet: Set<string>
): { filter: Filter; fn: FilterFunction } {
  const filter: Filter = {
    type: 'mrd',
    name,
    params: { threshold },
  };

  const fn: FilterFunction = (entities: Set<Entity>) => {
    const entityIds = entitiesToIds(entities);
    return filterSet((entity) => {
      const entityMRD = mrd(matrix, entity.id, referenceSet);
      return entityMRD >= threshold;
    }, entities);
  };

  return { filter, fn };
}

/**
 * Time filter: Filter entities based on last active timestamp
 * Keeps entities active since minTimestamp
 */
export function timeFilter(
  name: string,
  minTimestamp: Real
): { filter: Filter; fn: FilterFunction } {
  const filter: Filter = {
    type: 'time',
    name,
    params: { minTimestamp },
  };

  const fn: FilterFunction = (entities: Set<Entity>) => {
    return filterSet((entity) => {
      const lastActive = entity.lastActive ?? 0;
      return lastActive >= minTimestamp;
    }, entities);
  };

  return { filter, fn };
}

// ============================================================================
// Filter Composition
// ============================================================================

/**
 * Compose two filters sequentially
 * The result applies f1 first, then f2 to the result
 */
export function composeFilters(
  filter1: { filter: Filter; fn: FilterFunction },
  filter2: { filter: Filter; fn: FilterFunction }
): { filter: Filter; fn: FilterFunction } {
  const filter: Filter = {
    type: 'composite',
    name: `${filter1.filter.name}_then_${filter2.filter.name}`,
    params: {
      filter1: filter1.filter,
      filter2: filter2.filter,
    },
  };

  const fn: FilterFunction = (entities: Set<Entity>) => {
    const intermediate = filter1.fn(entities);
    return filter2.fn(intermediate);
  };

  return { filter, fn };
}

/**
 * Compose multiple filters sequentially
 */
export function composeFilterList(
  filters: Array<{ filter: Filter; fn: FilterFunction }>
): { filter: Filter; fn: FilterFunction } | null {
  if (filters.length === 0) return null;
  if (filters.length === 1) return filters[0];

  return filters.reduce((acc, curr) => composeFilters(acc, curr));
}

// ============================================================================
// Specialized Filters
// ============================================================================

/**
 * Create a filter that keeps entities with specific IDs
 */
export function idFilter(name: string, ids: Set<string>): { filter: Filter; fn: FilterFunction } {
  return attrFilter(name, (entity) => ids.has(entity.id));
}

/**
 * Create a filter that keeps entities with specific metadata
 */
export function metadataFilter(
  name: string,
  key: string,
  value: unknown
): { filter: Filter; fn: FilterFunction } {
  return attrFilter(name, (entity) => entity.metadata?.[key] === value);
}

/**
 * Create a filter based on entity name pattern
 */
export function namePatternFilter(
  name: string,
  pattern: RegExp
): { filter: Filter; fn: FilterFunction } {
  return attrFilter(name, (entity) => pattern.test(entity.name || ''));
}

/**
 * Create a filter that keeps top N entities by some metric
 */
export function topNFilter(
  name: string,
  n: number,
  scoreFn: (entity: Entity) => Real
): { filter: Filter; fn: FilterFunction } {
  const filter: Filter = {
    type: 'attribute',
    name,
    params: { n, type: 'topN' },
  };

  const fn: FilterFunction = (entities: Set<Entity>) => {
    const sorted = Array.from(entities).sort((a, b) => scoreFn(b) - scoreFn(a));
    return new Set(sorted.slice(0, n));
  };

  return { filter, fn };
}

/**
 * Create a filter that keeps bottom N entities by some metric
 */
export function bottomNFilter(
  name: string,
  n: number,
  scoreFn: (entity: Entity) => Real
): { filter: Filter; fn: FilterFunction } {
  const filter: Filter = {
    type: 'attribute',
    name,
    params: { n, type: 'bottomN' },
  };

  const fn: FilterFunction = (entities: Set<Entity>) => {
    const sorted = Array.from(entities).sort((a, b) => scoreFn(a) - scoreFn(b));
    return new Set(sorted.slice(0, n));
  };

  return { filter, fn };
}

/**
 * Create a filter that keeps entities within a percentile range
 */
export function percentileFilter(
  name: string,
  minPercentile: Real,
  maxPercentile: Real,
  scoreFn: (entity: Entity) => Real
): { filter: Filter; fn: FilterFunction } {
  const filter: Filter = {
    type: 'attribute',
    name,
    params: { minPercentile, maxPercentile, type: 'percentile' },
  };

  const fn: FilterFunction = (entities: Set<Entity>) => {
    const sorted = Array.from(entities).sort((a, b) => scoreFn(a) - scoreFn(b));
    const minIdx = Math.floor((minPercentile / 100) * sorted.length);
    const maxIdx = Math.ceil((maxPercentile / 100) * sorted.length);
    return new Set(sorted.slice(minIdx, maxIdx));
  };

  return { filter, fn };
}

// ============================================================================
// Logical Filter Combinators
// ============================================================================

/**
 * Create an AND filter (intersection of two filters)
 */
export function andFilter(
  name: string,
  filter1: { filter: Filter; fn: FilterFunction },
  filter2: { filter: Filter; fn: FilterFunction }
): { filter: Filter; fn: FilterFunction } {
  const filter: Filter = {
    type: 'composite',
    name,
    params: {
      operation: 'and',
      filter1: filter1.filter,
      filter2: filter2.filter,
    },
  };

  const fn: FilterFunction = (entities: Set<Entity>) => {
    const result1 = filter1.fn(entities);
    const result2 = filter2.fn(entities);
    const intersection = new Set<Entity>();
    for (const entity of result1) {
      if (result2.has(entity)) {
        intersection.add(entity);
      }
    }
    return intersection;
  };

  return { filter, fn };
}

/**
 * Create an OR filter (union of two filters)
 */
export function orFilter(
  name: string,
  filter1: { filter: Filter; fn: FilterFunction },
  filter2: { filter: Filter; fn: FilterFunction }
): { filter: Filter; fn: FilterFunction } {
  const filter: Filter = {
    type: 'composite',
    name,
    params: {
      operation: 'or',
      filter1: filter1.filter,
      filter2: filter2.filter,
    },
  };

  const fn: FilterFunction = (entities: Set<Entity>) => {
    const result1 = filter1.fn(entities);
    const result2 = filter2.fn(entities);
    return new Set([...result1, ...result2]);
  };

  return { filter, fn };
}

/**
 * Create a NOT filter (complement of a filter)
 */
export function notFilter(
  name: string,
  filterToNegate: { filter: Filter; fn: FilterFunction }
): { filter: Filter; fn: FilterFunction } {
  const filter: Filter = {
    type: 'composite',
    name,
    params: {
      operation: 'not',
      filter: filterToNegate.filter,
    },
  };

  const fn: FilterFunction = (entities: Set<Entity>) => {
    const filtered = filterToNegate.fn(entities);
    const complement = new Set<Entity>();
    for (const entity of entities) {
      if (!filtered.has(entity)) {
        complement.add(entity);
      }
    }
    return complement;
  };

  return { filter, fn };
}

// ============================================================================
// Filter Application
// ============================================================================

/**
 * Apply a filter to a set of entities
 */
export function applyFilter(
  filterFn: FilterFunction,
  entities: Set<Entity>
): Set<Entity> {
  return filterFn(entities);
}

/**
 * Apply multiple filters in sequence
 */
export function applyFilters(
  filters: FilterFunction[],
  entities: Set<Entity>
): Set<Entity> {
  return filters.reduce((acc, filter) => filter(acc), entities);
}

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Identity filter (returns all entities unchanged)
 */
export function identityFilter(name: string = 'identity'): { filter: Filter; fn: FilterFunction } {
  const filter: Filter = {
    type: 'attribute',
    name,
    params: {},
  };

  const fn: FilterFunction = (entities: Set<Entity>) => entities;

  return { filter, fn };
}

/**
 * Empty filter (returns no entities)
 */
export function emptyFilter(name: string = 'empty'): { filter: Filter; fn: FilterFunction } {
  const filter: Filter = {
    type: 'attribute',
    name,
    params: {},
  };

  const fn: FilterFunction = () => new Set<Entity>();

  return { filter, fn };
}

/**
 * Create a filter from a boolean predicate
 */
export function predicateFilter(
  name: string,
  predicate: (entity: Entity) => boolean
): { filter: Filter; fn: FilterFunction } {
  return attrFilter(name, predicate);
}

