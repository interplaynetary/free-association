/**
 * Elegant Lambda Calculus: Limit System
 * 
 * Fully curried limit implementation following lambda calculus principles:
 * - Limit τ = Dist τ → Dist τ
 * - Full currying for composition
 * - Point-free style where possible
 */

import type { Real, Distribution } from '../core/types';
import { normalize, getProb } from '../core/types';
import { pipe, compose2 } from './combinators';

// ============================================================================
// Limit Type (Pure Functional)
// ============================================================================

/**
 * Limit type following lambda calculus spec:
 * Limit τ = Dist τ → Dist τ
 */
export type Limit = (dist: Distribution) => Distribution;

// ============================================================================
// Basic Limit Constructors (Curried)
// ============================================================================

/**
 * Cap limit: Real → Limit
 * Following spec: cap_limit = λc:Real. λd:Dist τ. normalize(λx. min(d(x), c))
 */
export const cap = (maximum: Real): Limit =>
  (dist: Distribution): Distribution => {
    const capped: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const normalizedWeight = weight / dist.total;
      capped[id] = Math.min(normalizedWeight, maximum);
    }
    return normalize(capped);
  };

/**
 * Floor limit: Real → Limit
 * Following spec: floor_limit = λf:Real. λd:Dist τ. normalize(λx. max(d(x), f))
 */
export const floor = (minimum: Real): Limit =>
  (dist: Distribution): Distribution => {
    const floored: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const normalizedWeight = weight / dist.total;
      floored[id] = Math.max(normalizedWeight, minimum);
    }
    return normalize(floored);
  };

/**
 * Progressive limit: Real → Limit
 * Following spec: progressive_limit = λα:Real. λd:Dist τ. normalize(λx. d(x)^α)
 * 
 * α < 1: More egalitarian (reduces inequality)
 * α > 1: More concentrated (increases inequality)
 * α = 1: Identity
 */
export const progressive = (alpha: Real): Limit =>
  (dist: Distribution): Distribution => {
    const transformed: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const normalizedWeight = weight / dist.total;
      transformed[id] = Math.pow(normalizedWeight, alpha);
    }
    return normalize(transformed);
  };

/**
 * Type limit: (String → Real) → Limit
 * Following spec: type_limit = λweights:τ→Real. λd:Dist τ. normalize(λx. d(x) × weights(x))
 */
export const typeLimit = (weights: (id: string) => Real): Limit =>
  (dist: Distribution): Distribution => {
    const weighted: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const normalizedWeight = weight / dist.total;
      const typeWeight = weights(id);
      weighted[id] = normalizedWeight * typeWeight;
    }
    return normalize(weighted);
  };

/**
 * Type limit from map: Map → Limit
 */
export const typeLimitMap = (weightsMap: Map<string, Real>): Limit =>
  typeLimit((id) => weightsMap.get(id) ?? 1);

// ============================================================================
// Limit Composition (Following Lambda Calculus)
// ============================================================================

/**
 * Compose limits: Limit → Limit → Limit
 * Following spec: compose_limits = λl₁:Limit. λl₂:Limit. λd:Dist. l₁(l₂(d))
 */
export const composeLimits = (l1: Limit) =>
  (l2: Limit): Limit =>
  (dist: Distribution): Distribution =>
    l1(l2(dist));

/**
 * Compose multiple limits
 */
export const composeLimitList = (limits: Limit[]): Limit | null => {
  if (limits.length === 0) return null;
  if (limits.length === 1) return limits[0];
  return limits.reduce((acc, curr) => composeLimits(acc)(curr));
};

/**
 * Sequential composition (left-to-right)
 */
export const seq = (l1: Limit) =>
  (l2: Limit): Limit =>
  (dist: Distribution): Distribution =>
    l2(l1(dist));

// ============================================================================
// Specialized Limits (Curried)
// ============================================================================

/**
 * Range limit: Real → Real → Limit
 */
export const range = (min: Real) =>
  (max: Real): Limit =>
  (dist: Distribution): Distribution => {
    const clamped: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const normalizedWeight = weight / dist.total;
      const clampedWeight = Math.max(min, Math.min(max, normalizedWeight));
      clamped[id] = clampedWeight;
    }
    return normalize(clamped);
  };

/**
 * Top K limit: Number → Limit
 */
export const topK = (k: number): Limit =>
  (dist: Distribution): Distribution => {
    const sorted = Object.entries(dist.weights)
      .sort(([, a], [, b]) => b - a)
      .slice(0, k);
    
    const filtered: Record<string, Real> = Object.fromEntries(sorted);
    return normalize(filtered);
  };

/**
 * Threshold limit: Real → Limit
 */
export const thresholdLimit = (minWeight: Real): Limit =>
  (dist: Distribution): Distribution => {
    const filtered: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const normalizedWeight = weight / dist.total;
      if (normalizedWeight >= minWeight) {
        filtered[id] = normalizedWeight;
      }
    }
    return normalize(filtered);
  };

/**
 * Proportional cap: Real → Limit
 */
export const proportionalCap = (proportion: Real): Limit =>
  cap(proportion);

/**
 * Absolute cap: Real → Limit
 * (doesn't normalize - preserves absolute values)
 */
export const absoluteCap = (maximum: Real): Limit =>
  (dist: Distribution): Distribution => {
    const capped: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      capped[id] = Math.min(weight, maximum);
    }
    const total = Object.values(capped).reduce((sum, w) => sum + w, 0);
    return { weights: capped, total };
  };

/**
 * Gini coefficient based limit: Real → Limit
 */
export const gini = (targetGini: Real): Limit => {
  const alpha = 0.5 + targetGini * 0.5; // Approximation
  return progressive(alpha);
};

/**
 * Pareto limit: Distribution → Limit
 */
export const pareto = (baseline: Distribution): Limit =>
  (dist: Distribution): Distribution => {
    const adjusted: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const normalizedWeight = weight / dist.total;
      const baselineWeight = getProb(baseline, id);
      adjusted[id] = Math.max(normalizedWeight, baselineWeight);
    }
    return normalize(adjusted);
  };

// ============================================================================
// Limit Application (Following Lambda Calculus)
// ============================================================================

/**
 * Apply limit: Limit → Distribution → Distribution
 * Following spec: apply_limit = λl:Limit. λd:Dist. l d
 */
export const apply = (limit: Limit) =>
  (dist: Distribution): Distribution =>
    limit(dist);

/**
 * Apply multiple limits in sequence
 */
export const applyLimits = (limits: Limit[]) =>
  (dist: Distribution): Distribution =>
    limits.reduce((acc, limit) => limit(acc), dist);

// ============================================================================
// Utility Limits (Curried)
// ============================================================================

/**
 * Identity limit: Limit
 */
export const identityLimit: Limit = (dist) => dist;

/**
 * Zero limit: Limit
 */
export const zeroLimit: Limit = () => ({ weights: {}, total: 0 });

/**
 * Scale limit: Real → Limit
 */
export const scale = (factor: Real): Limit =>
  (dist: Distribution): Distribution => {
    const scaled: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      scaled[id] = weight * factor;
    }
    return { weights: scaled, total: dist.total * factor };
  };

/**
 * Normalize limit: Limit
 */
export const normalizeLimit: Limit = (dist) => normalize(dist.weights);

// ============================================================================
// Metrics
// ============================================================================

/**
 * Calculate Gini coefficient: Distribution → Real
 */
export const calculateGini = (dist: Distribution): Real => {
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
  
  if (mean === 0) return 0;
  return sum / (n * n * mean);
};

// ============================================================================
// Export curried operations
// ============================================================================

export const curriedLimits = {
  cap,
  floor,
  progressive,
  typeLimit,
  typeLimitMap,
  range,
  topK,
  thresholdLimit,
  proportionalCap,
  absoluteCap,
  gini,
  pareto,
  scale,
  identityLimit,
  zeroLimit,
  normalizeLimit,
};

