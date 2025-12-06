/**
 * Lambda Calculus Implementation: Limit System
 * 
 * This module implements the limit system for:
 * - Cap limits (maximum allocation)
 * - Floor limits (minimum allocation)
 * - Progressive limits (power-law transformation)
 * - Type limits (weighted by entity type)
 * - Limit composition
 */

import type { Real, Distribution, Limit } from './types';
import { normalize, getProb } from './types';
import { min, max, pow, clamp } from './primitives';

// ============================================================================
// Limit Type Definitions
// ============================================================================

export type LimitFunction = (dist: Distribution) => Distribution;

// ============================================================================
// Basic Limit Constructors
// ============================================================================

/**
 * Cap limit: Limits maximum allocation to any entity
 * Normalizes after capping
 */
export function capLimit(name: string, cap: Real): { limit: Limit; fn: LimitFunction } {
  const limit: Limit = {
    type: 'cap',
    name,
    params: { cap },
  };

  const fn: LimitFunction = (dist: Distribution) => {
    const capped: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const normalizedWeight = weight / dist.total;
      capped[id] = min(normalizedWeight, cap);
    }
    return normalize(capped);
  };

  return { limit, fn };
}

/**
 * Floor limit: Ensures minimum allocation to any entity
 * Normalizes after applying floor
 */
export function floorLimit(name: string, floor: Real): { limit: Limit; fn: LimitFunction } {
  const limit: Limit = {
    type: 'floor',
    name,
    params: { floor },
  };

  const fn: LimitFunction = (dist: Distribution) => {
    const floored: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const normalizedWeight = weight / dist.total;
      floored[id] = max(normalizedWeight, floor);
    }
    return normalize(floored);
  };

  return { limit, fn };
}

/**
 * Progressive limit: Applies power-law transformation
 * α < 1: More egalitarian (reduces inequality)
 * α > 1: More concentrated (increases inequality)
 * α = 1: No change
 */
export function progressiveLimit(
  name: string,
  alpha: Real
): { limit: Limit; fn: LimitFunction } {
  const limit: Limit = {
    type: 'progressive',
    name,
    params: { alpha },
  };

  const fn: LimitFunction = (dist: Distribution) => {
    const transformed: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const normalizedWeight = weight / dist.total;
      transformed[id] = pow(normalizedWeight, alpha);
    }
    return normalize(transformed);
  };

  return { limit, fn };
}

/**
 * Type limit: Weights allocation by entity type or category
 * Multiplies distribution by type-specific weights
 */
export function typeLimit(
  name: string,
  weights: Map<string, Real>
): { limit: Limit; fn: LimitFunction } {
  const limit: Limit = {
    type: 'type',
    name,
    params: { weights: Object.fromEntries(weights) },
  };

  const fn: LimitFunction = (dist: Distribution) => {
    const weighted: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const normalizedWeight = weight / dist.total;
      const typeWeight = weights.get(id) ?? 1;
      weighted[id] = normalizedWeight * typeWeight;
    }
    return normalize(weighted);
  };

  return { limit, fn };
}

// ============================================================================
// Limit Composition
// ============================================================================

/**
 * Compose two limits sequentially
 * The result applies limit1 first, then limit2 to the result
 */
export function composeLimits(
  limit1: { limit: Limit; fn: LimitFunction },
  limit2: { limit: Limit; fn: LimitFunction }
): { limit: Limit; fn: LimitFunction } {
  const limit: Limit = {
    type: 'type', // Use 'type' as generic composite
    name: `${limit1.limit.name}_then_${limit2.limit.name}`,
    params: {
      limit1: limit1.limit,
      limit2: limit2.limit,
    },
  };

  const fn: LimitFunction = (dist: Distribution) => {
    const intermediate = limit1.fn(dist);
    return limit2.fn(intermediate);
  };

  return { limit, fn };
}

/**
 * Compose multiple limits sequentially
 */
export function composeLimitList(
  limits: Array<{ limit: Limit; fn: LimitFunction }>
): { limit: Limit; fn: LimitFunction } | null {
  if (limits.length === 0) return null;
  if (limits.length === 1) return limits[0];

  return limits.reduce((acc, curr) => composeLimits(acc, curr));
}

// ============================================================================
// Specialized Limits
// ============================================================================

/**
 * Range limit: Clamps allocation within a range
 */
export function rangeLimit(
  name: string,
  minVal: Real,
  maxVal: Real
): { limit: Limit; fn: LimitFunction } {
  const limit: Limit = {
    type: 'cap',
    name,
    params: { min: minVal, max: maxVal },
  };

  const fn: LimitFunction = (dist: Distribution) => {
    const clamped: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const normalizedWeight = weight / dist.total;
      clamped[id] = clamp(normalizedWeight, minVal, maxVal);
    }
    return normalize(clamped);
  };

  return { limit, fn };
}

/**
 * Top-K limit: Only allocates to top K entities by weight
 */
export function topKLimit(name: string, k: number): { limit: Limit; fn: LimitFunction } {
  const limit: Limit = {
    type: 'cap',
    name,
    params: { k },
  };

  const fn: LimitFunction = (dist: Distribution) => {
    // Sort entries by weight (descending)
    const sorted = Object.entries(dist.weights).sort(([, a], [, b]) => b - a);
    const topK = sorted.slice(0, k);
    
    const filtered: Record<string, Real> = {};
    for (const [id, weight] of topK) {
      filtered[id] = weight / dist.total;
    }
    
    return normalize(filtered);
  };

  return { limit, fn };
}

/**
 * Threshold limit: Only allocates to entities above threshold
 */
export function thresholdLimit(
  name: string,
  threshold: Real
): { limit: Limit; fn: LimitFunction } {
  const limit: Limit = {
    type: 'floor',
    name,
    params: { threshold },
  };

  const fn: LimitFunction = (dist: Distribution) => {
    const filtered: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const normalizedWeight = weight / dist.total;
      if (normalizedWeight >= threshold) {
        filtered[id] = normalizedWeight;
      }
    }
    return normalize(filtered);
  };

  return { limit, fn };
}

/**
 * Proportional cap: Limits based on proportion of total
 * No entity can receive more than proportion * total
 */
export function proportionalCapLimit(
  name: string,
  proportion: Real
): { limit: Limit; fn: LimitFunction } {
  return capLimit(name, proportion);
}

/**
 * Absolute cap: Limits to absolute value (not proportion)
 * Useful when distribution represents actual resources
 */
export function absoluteCapLimit(
  name: string,
  absoluteCap: Real
): { limit: Limit; fn: LimitFunction } {
  const limit: Limit = {
    type: 'cap',
    name,
    params: { absoluteCap },
  };

  const fn: LimitFunction = (dist: Distribution) => {
    const capped: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      capped[id] = min(weight, absoluteCap);
    }
    // Don't normalize - preserve absolute values
    const total = Object.values(capped).reduce((sum, w) => sum + w, 0);
    return { weights: capped, total };
  };

  return { limit, fn };
}

/**
 * Gini coefficient based limit: Reduces inequality to target Gini
 * Not fully implemented - would require iterative adjustment
 */
export function giniLimit(name: string, targetGini: Real): { limit: Limit; fn: LimitFunction } {
  const limit: Limit = {
    type: 'progressive',
    name,
    params: { targetGini },
  };

  // Simplified implementation: use progressive limit with estimated alpha
  // More egalitarian for lower target Gini
  const alpha = 0.5 + targetGini * 0.5; // Rough approximation
  
  const fn: LimitFunction = (dist: Distribution) => {
    const transformed: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const normalizedWeight = weight / dist.total;
      transformed[id] = pow(normalizedWeight, alpha);
    }
    return normalize(transformed);
  };

  return { limit, fn };
}

/**
 * Pareto limit: Ensures Pareto efficiency
 * In this context, prevents any allocation that would harm distribution
 */
export function paretoLimit(
  name: string,
  baselineDist: Distribution
): { limit: Limit; fn: LimitFunction } {
  const limit: Limit = {
    type: 'type',
    name,
    params: { baseline: baselineDist },
  };

  const fn: LimitFunction = (dist: Distribution) => {
    const adjusted: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const normalizedWeight = weight / dist.total;
      const baselineWeight = getProb(baselineDist, id);
      // Ensure no entity gets less than baseline
      adjusted[id] = max(normalizedWeight, baselineWeight);
    }
    return normalize(adjusted);
  };

  return { limit, fn };
}

// ============================================================================
// Limit Application
// ============================================================================

/**
 * Apply a limit to a distribution
 */
export function applyLimit(limitFn: LimitFunction, dist: Distribution): Distribution {
  return limitFn(dist);
}

/**
 * Apply multiple limits in sequence
 */
export function applyLimits(limits: LimitFunction[], dist: Distribution): Distribution {
  return limits.reduce((acc, limit) => limit(acc), dist);
}

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Identity limit (returns distribution unchanged)
 */
export function identityLimit(name: string = 'identity'): { limit: Limit; fn: LimitFunction } {
  const limit: Limit = {
    type: 'type',
    name,
    params: {},
  };

  const fn: LimitFunction = (dist: Distribution) => dist;

  return { limit, fn };
}

/**
 * Zero limit (returns empty distribution)
 */
export function zeroLimit(name: string = 'zero'): { limit: Limit; fn: LimitFunction } {
  const limit: Limit = {
    type: 'cap',
    name,
    params: {},
  };

  const fn: LimitFunction = () => ({ weights: {}, total: 0 });

  return { limit, fn };
}

/**
 * Scale limit: Multiplies all weights by a constant
 */
export function scaleLimit(name: string, scale: Real): { limit: Limit; fn: LimitFunction } {
  const limit: Limit = {
    type: 'type',
    name,
    params: { scale },
  };

  const fn: LimitFunction = (dist: Distribution) => {
    const scaled: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      scaled[id] = weight * scale;
    }
    return { weights: scaled, total: dist.total * scale };
  };

  return { limit, fn };
}

/**
 * Calculate actual Gini coefficient of a distribution
 */
export function calculateGini(dist: Distribution): Real {
  const weights = Object.values(dist.weights).map((w) => w / dist.total);
  if (weights.length === 0) return 0;
  if (weights.length === 1) return 0;

  weights.sort((a, b) => a - b);
  
  let sum = 0;
  for (let i = 0; i < weights.length; i++) {
    sum += (2 * (i + 1) - weights.length - 1) * weights[i];
  }
  
  const n = weights.length;
  const mean = weights.reduce((a, b) => a + b, 0) / n;
  
  return sum / (n * n * mean);
}

