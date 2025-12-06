/**
 * Lambda Calculus Implementation: Recognition System
 * 
 * This module implements the core recognition operations:
 * - Recognition distributions
 * - Mutual recognition
 * - Total Mutual Recognition (TMR)
 * - Mutual Recognition Share (MRS)
 * - Mutual Recognition Density (MRD)
 */

import type { Entity, Real, Distribution, RecognitionMatrix } from './types';
import { normalize, getRecognition, diracDelta, getProb } from './types';
import { min, sumOver, entitiesToIds } from './primitives';

// ============================================================================
// Recognition Distribution
// ============================================================================

/**
 * Get recognition distribution for an entity
 * Returns a normalized distribution of how entity e recognizes others
 */
export function recognition(
  matrix: RecognitionMatrix,
  entityId: string,
  universe: Set<string>
): Distribution {
  const weights: Record<string, Real> = {};
  
  for (const targetId of universe) {
    const r = getRecognition(matrix, entityId, targetId);
    if (r > 0) {
      weights[targetId] = r;
    }
  }
  
  return normalize(weights);
}

// ============================================================================
// Mutual Recognition
// ============================================================================

/**
 * Calculate mutual recognition between two entities
 * mutual(a, b) = min(R(a, b), R(b, a))
 */
export function mutual(
  matrix: RecognitionMatrix,
  entityAId: string,
  entityBId: string
): Real {
  const rab = getRecognition(matrix, entityAId, entityBId);
  const rba = getRecognition(matrix, entityBId, entityAId);
  return min(rab, rba);
}

/**
 * Calculate mutual recognition matrix for a set of entities
 * Returns a map of entity pairs to mutual recognition values
 */
export function mutualMatrix(
  matrix: RecognitionMatrix,
  entities: Set<string>
): Map<string, Map<string, Real>> {
  const result = new Map<string, Map<string, Real>>();
  
  for (const a of entities) {
    const row = new Map<string, Real>();
    for (const b of entities) {
      row.set(b, mutual(matrix, a, b));
    }
    result.set(a, row);
  }
  
  return result;
}

// ============================================================================
// Total Mutual Recognition (TMR)
// ============================================================================

/**
 * Calculate Total Mutual Recognition for an entity
 * TMR(e) = Σ_{f ∈ universe} mutual(e, f)
 */
export function tmr(
  matrix: RecognitionMatrix,
  entityId: string,
  universe: Set<string>
): Real {
  return sumOver((targetId) => mutual(matrix, entityId, targetId), universe);
}

/**
 * Calculate TMR for all entities in a set
 */
export function tmrForAll(
  matrix: RecognitionMatrix,
  entities: Set<string>
): Map<string, Real> {
  const result = new Map<string, Real>();
  for (const entityId of entities) {
    result.set(entityId, tmr(matrix, entityId, entities));
  }
  return result;
}

// ============================================================================
// Mutual Recognition Share (MRS)
// ============================================================================

/**
 * Calculate Mutual Recognition Share distribution for an entity
 * MRS(e) is a distribution over entities based on mutual recognition
 * 
 * If TMR(e) > 0:
 *   MRS(e)(f) = mutual(e, f) / TMR(e)
 * Else:
 *   MRS(e) = δ(e)  [Dirac delta - all weight on self]
 */
export function mrs(
  matrix: RecognitionMatrix,
  entityId: string,
  universe: Set<string>
): Distribution {
  const totalMR = tmr(matrix, entityId, universe);
  
  if (totalMR === 0) {
    return diracDelta(entityId);
  }
  
  const weights: Record<string, Real> = {};
  for (const targetId of universe) {
    const mr = mutual(matrix, entityId, targetId);
    if (mr > 0) {
      weights[targetId] = mr;
    }
  }
  
  return normalize(weights);
}

/**
 * Get MRS value for a specific target entity
 */
export function mrsValue(
  matrix: RecognitionMatrix,
  entityId: string,
  targetId: string,
  universe: Set<string>
): Real {
  const dist = mrs(matrix, entityId, universe);
  return getProb(dist, targetId);
}

/**
 * Calculate MRS matrix for all entities
 * Returns a map from entity ID to its MRS distribution
 */
export function mrsMatrix(
  matrix: RecognitionMatrix,
  entities: Set<string>
): Map<string, Distribution> {
  const result = new Map<string, Distribution>();
  for (const entityId of entities) {
    result.set(entityId, mrs(matrix, entityId, entities));
  }
  return result;
}

// ============================================================================
// Mutual Recognition Density (MRD)
// ============================================================================

/**
 * Calculate average mutual recognition for a set
 */
export function averageMR(
  matrix: RecognitionMatrix,
  entities: Set<string>
): Real {
  if (entities.size === 0) return 0;
  
  let sum = 0;
  let count = 0;
  
  for (const a of entities) {
    for (const b of entities) {
      sum += mutual(matrix, a, b);
      count++;
    }
  }
  
  return count > 0 ? sum / count : 0;
}

/**
 * Calculate Mutual Recognition Density for an entity within a set
 * MRD_S(e) = (Σ_{f ∈ S} mutual(e, f)) / average_mr(S)
 */
export function mrd(
  matrix: RecognitionMatrix,
  entityId: string,
  entities: Set<string>
): Real {
  const avgMR = averageMR(matrix, entities);
  if (avgMR === 0) return 0;
  
  const sumMR = sumOver((targetId) => mutual(matrix, entityId, targetId), entities);
  return sumMR / avgMR;
}

/**
 * Calculate MRD for all entities in a set
 */
export function mrdForAll(
  matrix: RecognitionMatrix,
  entities: Set<string>
): Map<string, Real> {
  const result = new Map<string, Real>();
  const avgMR = averageMR(matrix, entities);
  
  if (avgMR === 0) {
    for (const entityId of entities) {
      result.set(entityId, 0);
    }
    return result;
  }
  
  for (const entityId of entities) {
    const sumMR = sumOver((targetId) => mutual(matrix, entityId, targetId), entities);
    result.set(entityId, sumMR / avgMR);
  }
  
  return result;
}

// ============================================================================
// Recognition Updates
// ============================================================================

/**
 * Update recognition based on benefit received
 * This implements a learning rule where entities increase recognition
 * towards those who benefit them
 */
export function updateRecognition(
  matrix: RecognitionMatrix,
  entityId: string,
  targetId: string,
  benefit: Real,
  learningRate: Real = 0.1
): RecognitionMatrix {
  const currentRecognition = getRecognition(matrix, entityId, targetId);
  const currentToTarget = getRecognition(matrix, targetId, entityId);
  
  // Only update if not already at or above reciprocal level
  const gradient = currentRecognition <= currentToTarget ? benefit : 0;
  const newRecognition = currentRecognition + learningRate * gradient;
  
  return {
    matrix: {
      ...matrix.matrix,
      [entityId]: {
        ...matrix.matrix[entityId],
        [targetId]: Math.max(0, newRecognition),
      },
    },
  };
}

/**
 * Normalize all recognition distributions in a matrix
 * Ensures each entity's outgoing recognition sums to 1
 */
export function normalizeRecognitionMatrix(
  matrix: RecognitionMatrix,
  entities: Set<string>
): RecognitionMatrix {
  const newMatrix: Record<string, Record<string, Real>> = {};
  
  for (const entityId of entities) {
    const weights: Record<string, Real> = {};
    for (const targetId of entities) {
      const r = getRecognition(matrix, entityId, targetId);
      if (r > 0) {
        weights[targetId] = r;
      }
    }
    
    const dist = normalize(weights);
    newMatrix[entityId] = dist.weights;
  }
  
  return { matrix: newMatrix };
}

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Initialize a uniform recognition matrix
 * All entities recognize all others equally
 */
export function uniformRecognitionMatrix(entities: Set<string>): RecognitionMatrix {
  const n = entities.size;
  if (n === 0) return { matrix: {} };
  
  const uniformWeight = 1 / n;
  const matrix: Record<string, Record<string, Real>> = {};
  
  for (const fromId of entities) {
    matrix[fromId] = {};
    for (const toId of entities) {
      matrix[fromId][toId] = uniformWeight;
    }
  }
  
  return { matrix };
}

/**
 * Initialize a recognition matrix from entity objects
 */
export function uniformRecognitionMatrixFromEntities(entities: Set<Entity>): RecognitionMatrix {
  return uniformRecognitionMatrix(entitiesToIds(entities));
}

/**
 * Get all non-zero recognition values for an entity
 */
export function getRecognitions(
  matrix: RecognitionMatrix,
  entityId: string
): Map<string, Real> {
  const result = new Map<string, Real>();
  const row = matrix.matrix[entityId];
  
  if (row) {
    for (const [targetId, value] of Object.entries(row)) {
      if (value > 0) {
        result.set(targetId, value);
      }
    }
  }
  
  return result;
}

/**
 * Calculate total recognition given to an entity by others
 */
export function receivedRecognition(
  matrix: RecognitionMatrix,
  targetId: string,
  entities: Set<string>
): Real {
  return sumOver((fromId) => getRecognition(matrix, fromId, targetId), entities);
}

