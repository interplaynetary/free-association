/**
 * Elegant Lambda Calculus Implementation: Recognition System
 * 
 * This module demonstrates a more elegant, functional implementation
 * following lambda calculus principles:
 * - Fully curried functions
 * - Point-free style
 * - Function composition
 * - Reader monad for context threading
 */

import type { Real, Distribution, RecognitionMatrix } from '../core/types';
import { normalize, getRecognition, diracDelta, getProb, hasSparseRepresentation } from '../core/types';
import type { Reader } from './combinators';
import { curry2, curry3, pipe, compose2, runReader, bindReader, mapReader } from './combinators';
import { sparseMutual, sparseTMR, sparseMRS, sparseMRD } from '../sparse/operations';
import type { SparseRecognitionGraph } from '../sparse/types';

// ============================================================================
// Core Recognition Functions (Curried)
// ============================================================================

/**
 * Get recognition: Entity → Matrix → Universe → Distribution
 * Fully curried for partial application
 * Uses sparse operations when available for better performance
 */
export const recognition = (matrix: RecognitionMatrix) => 
  (entityId: string) => 
  (universe: Set<string>): Distribution => {
    // Use sparse representation if available (much faster!)
    if (matrix.sparse) {
      const { SparseOps } = require('../sparse/types');
      const outgoing = SparseOps.outgoing(matrix.sparse, entityId);
      const weights: Record<string, Real> = {};
      
      for (const [targetId, value] of outgoing) {
        if (universe.has(targetId) && value > 0) {
          weights[targetId] = value;
        }
      }
      return normalize(weights);
    }
    
    // Fallback to Record-based implementation
    const weights: Record<string, Real> = {};
    for (const targetId of universe) {
      const r = getRecognition(matrix, entityId, targetId);
      if (r > 0) weights[targetId] = r;
    }
    return normalize(weights);
  };

/**
 * Mutual recognition: Matrix → EntityA → EntityB → Real
 * mutual(a, b) = min(R(a, b), R(b, a))
 * Uses sparse operations when available (O(1) vs O(log n))
 */
export const mutual = (matrix: RecognitionMatrix) => 
  (entityAId: string) => 
  (entityBId: string): Real => {
    // Use sparse implementation if available (faster!)
    if (matrix.sparse) {
      return sparseMutual(matrix.sparse)(entityAId)(entityBId);
    }
    
    // Fallback to Record-based implementation
    return Math.min(
      getRecognition(matrix, entityAId, entityBId),
      getRecognition(matrix, entityBId, entityAId)
    );
  };

/**
 * Symmetric mutual: Creates symmetric mutual recognition function
 */
export const mutualSym = (matrix: RecognitionMatrix) => {
  const m = mutual(matrix);
  return (a: string) => (b: string): Real => m(a)(b);  // Same as m(b)(a)
};

// ============================================================================
// Total Mutual Recognition (TMR)
// ============================================================================

/**
 * TMR: Matrix → Entity → Universe → Real
 * Point-free style using composition
 * Uses sparse operations when available (O(k) vs O(n) where k << n)
 */
export const tmr = (matrix: RecognitionMatrix) => 
  (entityId: string) => 
  (universe: Set<string>): Real => {
    // Use sparse implementation if available (much faster!)
    if (matrix.sparse) {
      return sparseTMR(matrix.sparse)(entityId)(universe);
    }
    
    // Fallback to Record-based implementation
    const m = mutual(matrix)(entityId);
    return Array.from(universe).reduce((sum, targetId) => sum + m(targetId), 0);
  };

/**
 * TMR for all entities: Matrix → Universe → Map
 */
export const tmrForAll = (matrix: RecognitionMatrix) => 
  (universe: Set<string>): Map<string, Real> => {
    const t = tmr(matrix);
    return new Map(
      Array.from(universe).map(id => [id, t(id)(universe)])
    );
  };

// ============================================================================
// Mutual Recognition Share (MRS)
// ============================================================================

/**
 * MRS: Matrix → Entity → Universe → Distribution
 * Following lambda calculus spec exactly
 */
export const mrs = (matrix: RecognitionMatrix) => 
  (entityId: string) => 
  (universe: Set<string>): Distribution => {
    // Use sparse implementation if available (O(k) vs O(n))
    if (matrix.sparse) {
      const result = sparseMRS(matrix.sparse)(entityId)(universe);
      // If distribution is empty, return Dirac delta
      if (Object.keys(result.distribution).length === 0) {
        return diracDelta(entityId);
      }
      return result;
    }
    
    // Fallback to Record-based implementation
    const totalMR = tmr(matrix)(entityId)(universe);
    
    // If TMR = 0, return Dirac delta (self-distribution)
    if (totalMR === 0) return diracDelta(entityId);
    
    // Otherwise, normalize mutual recognition
    const m = mutual(matrix)(entityId);
    const weights: Record<string, Real> = {};
    for (const targetId of universe) {
      const mr = m(targetId);
      if (mr > 0) weights[targetId] = mr;
    }
    return normalize(weights);
  };

/**
 * MRS value: Matrix → From → To → Universe → Real
 * Get specific MRS value between two entities
 */
export const mrsValue = (matrix: RecognitionMatrix) => 
  (entityId: string) => 
  (targetId: string) => 
  (universe: Set<string>): Real => {
    const dist = mrs(matrix)(entityId)(universe);
    return getProb(dist, targetId);
  };

/**
 * MRS matrix: Matrix → Universe → Map<Entity, Distribution>
 */
export const mrsMatrix = (matrix: RecognitionMatrix) => 
  (universe: Set<string>): Map<string, Distribution> => {
    const m = mrs(matrix);
    return new Map(
      Array.from(universe).map(id => [id, m(id)(universe)])
    );
  };

// ============================================================================
// Mutual Recognition Density (MRD)
// ============================================================================

/**
 * Average MR: Matrix → Entities → Real
 * According to LAMBDA.md: avg_mr(s) = (Σ_{e,f∈s} mutual e f) / |s|
 */
export const averageMR = (matrix: RecognitionMatrix) => 
  (entities: Set<string>): Real => {
    if (entities.size === 0) return 0;
    const m = mutual(matrix);
    const entitiesArray = Array.from(entities);
    const sum = entitiesArray.reduce((acc, a) => 
      acc + entitiesArray.reduce((innerAcc, b) => 
        innerAcc + m(a)(b), 0), 0);
    return sum / entities.size;
  };

/**
 * MRD: Matrix → Entity → Entities → Real
 * MRD_S(e) = (Σ_{f ∈ S} mutual(e, f)) / average_mr(S)
 */
export const mrd = (matrix: RecognitionMatrix) => 
  (entityId: string) => 
  (entities: Set<string>): Real => {
    // Use sparse implementation if available (O(k*|C|) vs O(|C|²))
    if (matrix.sparse) {
      return sparseMRD(matrix.sparse)(entityId)(entities);
    }
    
    // Fallback to Record-based implementation
    const avgMR = averageMR(matrix)(entities);
    if (avgMR === 0) return 0;
    
    const m = mutual(matrix)(entityId);
    const sumMR = Array.from(entities).reduce((sum, targetId) => 
      sum + m(targetId), 0);
    return sumMR / avgMR;
  };

/**
 * MRD for all: Matrix → Entities → Map<Entity, Real>
 */
export const mrdForAll = (matrix: RecognitionMatrix) => 
  (entities: Set<string>): Map<string, Real> => {
    const m = mrd(matrix);
    return new Map(
      Array.from(entities).map(id => [id, m(id)(entities)])
    );
  };

// ============================================================================
// Recognition Updates (with Reader Monad)
// ============================================================================

/**
 * Context for recognition operations
 */
export interface RecognitionContext {
  matrix: RecognitionMatrix;
  universe: Set<string>;
  learningRate?: Real;
}

/**
 * Recognition operations in Reader monad
 */
export type RecognitionReader<A> = Reader<RecognitionContext, A>;

/**
 * Lift recognition to Reader monad
 */
export const recognitionR = (entityId: string): RecognitionReader<Distribution> => 
  (ctx) => recognition(ctx.matrix)(entityId)(ctx.universe);

/**
 * Lift mutual to Reader monad
 */
export const mutualR = (entityA: string) => 
  (entityB: string): RecognitionReader<Real> => 
  (ctx) => mutual(ctx.matrix)(entityA)(entityB);

/**
 * Lift TMR to Reader monad
 */
export const tmrR = (entityId: string): RecognitionReader<Real> => 
  (ctx) => tmr(ctx.matrix)(entityId)(ctx.universe);

/**
 * Lift MRS to Reader monad
 */
export const mrsR = (entityId: string): RecognitionReader<Distribution> => 
  (ctx) => mrs(ctx.matrix)(entityId)(ctx.universe);

/**
 * Lift MRD to Reader monad
 */
export const mrdR = (entityId: string) => 
  (entities: Set<string>): RecognitionReader<Real> => 
  (ctx) => mrd(ctx.matrix)(entityId)(entities);

/**
 * Update recognition using Reader monad
 */
export const updateRecognitionR = (entityId: string) => 
  (targetId: string) => 
  (benefit: Real): RecognitionReader<RecognitionMatrix> => 
  (ctx) => {
    const lr = ctx.learningRate ?? 0.1;
    const currentRecognition = getRecognition(ctx.matrix, entityId, targetId);
    const currentToTarget = getRecognition(ctx.matrix, targetId, entityId);
    
    const gradient = currentRecognition <= currentToTarget ? benefit : 0;
    const newRecognition = currentRecognition + lr * gradient;
    
    return {
      matrix: {
        ...ctx.matrix.matrix,
        [entityId]: {
          ...ctx.matrix.matrix[entityId],
          [targetId]: Math.max(0, newRecognition),
        },
      },
    };
  };

// ============================================================================
// Point-Free Combinators
// ============================================================================

/**
 * Compose two recognition operations
 */
export const composeMR = <A, B, C>(
  f: (matrix: RecognitionMatrix) => (a: A) => B,
  g: (b: B) => C
) => 
  (matrix: RecognitionMatrix) => 
  (a: A): C => 
    g(f(matrix)(a));

/**
 * Map over recognition results
 */
export const mapRecognition = <A, B>(
  f: (a: A) => B,
  recOp: (matrix: RecognitionMatrix) => (id: string) => (universe: Set<string>) => A
) => 
  (matrix: RecognitionMatrix) => 
  (id: string) => 
  (universe: Set<string>): B => 
    f(recOp(matrix)(id)(universe));

/**
 * Filter recognition distribution by predicate
 */
export const filterRecognition = (
  predicate: (id: string, weight: Real) => boolean
) => 
  (dist: Distribution): Distribution => {
    const filtered: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      if (predicate(id, weight / dist.total)) {
        filtered[id] = weight;
      }
    }
    return normalize(filtered);
  };

// ============================================================================
// Higher-Order Recognition Operations
// ============================================================================

/**
 * Fold over recognition values
 */
export const foldRecognition = <A>(
  f: (acc: A, id: string, weight: Real) => A,
  initial: A
) => 
  (dist: Distribution): A => {
    let acc = initial;
    for (const [id, weight] of Object.entries(dist.weights)) {
      acc = f(acc, id, weight / dist.total);
    }
    return acc;
  };

/**
 * Threshold recognition: Keep only values above threshold
 */
export const thresholdRecognition = (threshold: Real) => 
  filterRecognition((_, weight) => weight >= threshold);

/**
 * Top-K recognition: Keep only top K entities
 */
export const topKRecognition = (k: number) => 
  (dist: Distribution): Distribution => {
    const sorted = Object.entries(dist.weights)
      .sort(([, a], [, b]) => b - a)
      .slice(0, k);
    const filtered: Record<string, Real> = Object.fromEntries(sorted);
    return normalize(filtered);
  };

// ============================================================================
// Elegant Initialization
// ============================================================================

/**
 * Initialize uniform recognition: Universe → Matrix
 * Point-free, declarative style
 */
export const uniformRecognitionMatrix = (entities: Set<string>): RecognitionMatrix => {
  const n = entities.size;
  if (n === 0) return { matrix: {} };
  
  const uniformWeight = 1 / n;
  const entitiesArray = Array.from(entities);
  
  return {
    matrix: Object.fromEntries(
      entitiesArray.map(fromId => [
        fromId,
        Object.fromEntries(
          entitiesArray.map(toId => [toId, uniformWeight])
        )
      ])
    )
  };
};

/**
 * Normalize recognition matrix: Matrix → Universe → Matrix
 */
export const normalizeRecognitionMatrix = (matrix: RecognitionMatrix) => 
  (entities: Set<string>): RecognitionMatrix => ({
    matrix: Object.fromEntries(
      Array.from(entities).map(entityId => {
        const weights: Record<string, Real> = {};
        for (const targetId of entities) {
          const r = getRecognition(matrix, entityId, targetId);
          if (r > 0) weights[targetId] = r;
        }
        const dist = normalize(weights);
        return [entityId, dist.weights];
      })
    )
  });

// ============================================================================
// Elegant Queries
// ============================================================================

/**
 * Get all non-zero recognitions: Matrix → Entity → Map
 */
export const getRecognitions = (matrix: RecognitionMatrix) => 
  (entityId: string): Map<string, Real> => 
    new Map(
      Object.entries(matrix.matrix[entityId] || {})
        .filter(([, value]) => value > 0)
    );

/**
 * Total recognition received: Matrix → Target → Universe → Real
 */
export const receivedRecognition = (matrix: RecognitionMatrix) => 
  (targetId: string) => 
  (entities: Set<string>): Real => 
    Array.from(entities).reduce(
      (sum, fromId) => sum + getRecognition(matrix, fromId, targetId),
      0
    );

// ============================================================================
// Export curried versions for convenience
// ============================================================================

export const curriedOps = {
  recognition,
  mutual,
  tmr,
  mrs,
  mrd,
  averageMR,
  mrsValue,
  uniformRecognitionMatrix,
  normalizeRecognitionMatrix,
  getRecognitions,
  receivedRecognition,
};

