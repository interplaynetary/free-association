/**
 * Tests for Elegant Recognition System
 */

import { describe, it, expect, beforeEach } from 'vitest';
import type { Entity, RecognitionMatrix, Distribution } from '../../core/types';
import {
  recognition,
  mutual,
  mutualSym,
  tmr,
  tmrForAll,
  mrs,
  mrsValue,
  mrsMatrix,
  averageMR,
  mrd,
  mrdForAll,
  recognitionR,
  mutualR,
  tmrR,
  mrsR,
  mrdR,
  updateRecognitionR,
  filterRecognition,
  foldRecognition,
  thresholdRecognition,
  topKRecognition,
  uniformRecognitionMatrix,
  normalizeRecognitionMatrix,
  getRecognitions,
  receivedRecognition,
  type RecognitionContext,
} from '../recognition';
import { runReader } from '../combinators';

describe('Elegant Recognition System', () => {
  let matrix: RecognitionMatrix;
  let entities: Set<string>;

  beforeEach(() => {
    entities = new Set(['alice', 'bob', 'charlie']);
    matrix = uniformRecognitionMatrix(entities);
  });

  describe('Basic Recognition', () => {
    it('recognition - returns normalized distribution', () => {
      const dist = recognition(matrix)('alice')(entities);
      
      expect(dist.total).toBeCloseTo(1, 5);
      expect(Object.keys(dist.weights).length).toBe(3);
      
      // Uniform recognition initially
      expect(dist.weights['alice']).toBeCloseTo(1/3, 5);
      expect(dist.weights['bob']).toBeCloseTo(1/3, 5);
      expect(dist.weights['charlie']).toBeCloseTo(1/3, 5);
    });

    it('recognition - currying allows partial application', () => {
      const recognitionInMatrix = recognition(matrix);
      const aliceRecognition = recognitionInMatrix('alice');
      const dist = aliceRecognition(entities);
      
      expect(dist.total).toBeCloseTo(1, 5);
    });
  });

  describe('Mutual Recognition', () => {
    it('mutual - symmetric property', () => {
      const mrAB = mutual(matrix)('alice')('bob');
      const mrBA = mutual(matrix)('bob')('alice');
      
      expect(mrAB).toBe(mrBA);
      expect(mrAB).toBeCloseTo(1/3, 5);
    });

    it('mutual - curried for partial application', () => {
      const mutualInMatrix = mutual(matrix);
      const aliceMutual = mutualInMatrix('alice');
      
      const mrBob = aliceMutual('bob');
      const mrCharlie = aliceMutual('charlie');
      
      expect(mrBob).toBeCloseTo(1/3, 5);
      expect(mrCharlie).toBeCloseTo(1/3, 5);
    });

    it('mutual - with asymmetric recognition', () => {
      // Alice recognizes Bob more
      matrix = {
        matrix: {
          'alice': { 'alice': 0.2, 'bob': 0.6, 'charlie': 0.2 },
          'bob': { 'alice': 0.3, 'bob': 0.4, 'charlie': 0.3 },
          'charlie': { 'alice': 1/3, 'bob': 1/3, 'charlie': 1/3 },
        }
      };
      
      const mrAB = mutual(matrix)('alice')('bob');
      expect(mrAB).toBeCloseTo(0.3, 5); // min(0.6, 0.3)
    });

    it('mutualSym - creates symmetric function', () => {
      const m = mutualSym(matrix);
      expect(m('alice')('bob')).toBe(m('bob')('alice'));
    });
  });

  describe('Total Mutual Recognition (TMR)', () => {
    it('tmr - sums mutual recognition', () => {
      const aliceTMR = tmr(matrix)('alice')(entities);
      
      // Uniform: 3 entities × 1/3 each
      expect(aliceTMR).toBeCloseTo(1, 5);
    });

    it('tmr - with asymmetric recognition', () => {
      matrix = {
        matrix: {
          'alice': { 'alice': 0.1, 'bob': 0.7, 'charlie': 0.2 },
          'bob': { 'alice': 0.3, 'bob': 0.4, 'charlie': 0.3 },
          'charlie': { 'alice': 0.5, 'bob': 0.2, 'charlie': 0.3 },
        }
      };
      
      const aliceTMR = tmr(matrix)('alice')(entities);
      const mrAliceAlice = mutual(matrix)('alice')('alice');
      const mrAliceBob = mutual(matrix)('alice')('bob');
      const mrAliceCharlie = mutual(matrix)('alice')('charlie');
      
      expect(aliceTMR).toBeCloseTo(mrAliceAlice + mrAliceBob + mrAliceCharlie, 5);
    });

    it('tmrForAll - calculates for all entities', () => {
      const tmrMap = tmrForAll(matrix)(entities);
      
      expect(tmrMap.size).toBe(3);
      expect(tmrMap.get('alice')).toBeCloseTo(1, 5);
      expect(tmrMap.get('bob')).toBeCloseTo(1, 5);
      expect(tmrMap.get('charlie')).toBeCloseTo(1, 5);
    });
  });

  describe('Mutual Recognition Share (MRS)', () => {
    it('mrs - returns normalized distribution', () => {
      const dist = mrs(matrix)('alice')(entities);
      
      expect(dist.total).toBeCloseTo(1, 5);
      
      // Should match mutual recognition distribution
      const aliceMutual = mutual(matrix)('alice');
      const mrBob = aliceMutual('bob');
      const mrCharlie = aliceMutual('charlie');
      const total = tmr(matrix)('alice')(entities);
      
      expect(dist.weights['bob'] / dist.total).toBeCloseTo(mrBob / total, 5);
      expect(dist.weights['charlie'] / dist.total).toBeCloseTo(mrCharlie / total, 5);
    });

    it('mrs - Dirac delta when TMR is zero', () => {
      // Create isolated entity
      matrix = {
        matrix: {
          'alice': { 'alice': 0, 'bob': 0, 'charlie': 0 },
          'bob': { 'alice': 0, 'bob': 1, 'charlie': 0 },
          'charlie': { 'alice': 0, 'bob': 0, 'charlie': 1 },
        }
      };
      
      const dist = mrs(matrix)('alice')(entities);
      
      // Should be self-distribution
      expect(dist.weights['alice']).toBeCloseTo(1, 5);
      expect(dist.weights['bob'] || 0).toBeCloseTo(0, 5);
      expect(dist.weights['charlie'] || 0).toBeCloseTo(0, 5);
    });

    it('mrsValue - gets specific value', () => {
      const value = mrsValue(matrix)('alice')('bob')(entities);
      
      const dist = mrs(matrix)('alice')(entities);
      const expected = (dist.weights['bob'] || 0) / dist.total;
      
      expect(value).toBeCloseTo(expected, 5);
    });

    it('mrsMatrix - calculates for all entities', () => {
      const mrsMap = mrsMatrix(matrix)(entities);
      
      expect(mrsMap.size).toBe(3);
      
      for (const [id, dist] of mrsMap) {
        expect(dist.total).toBeCloseTo(1, 5);
      }
    });
  });

  describe('Mutual Recognition Density (MRD)', () => {
    it('averageMR - calculates average', () => {
      const avg = averageMR(matrix)(entities);
      expect(avg).toBeGreaterThan(0);
      // Per LAMBDA.md: avg_mr(s) = (Σ_{e,f∈s} mutual e f) / |s|
      // For uniform 3x3: sum = 9 * (1/3) = 3, avg = 3 / 3 = 1
      expect(avg).toBeCloseTo(1, 5);
    });

    it('mrd - calculates density', () => {
      const aliceMRD = mrd(matrix)('alice')(entities);
      
      // In uniform case, all MRDs should be 1
      expect(aliceMRD).toBeCloseTo(1, 5);
    });

    it('mrd - with varied recognition', () => {
      matrix = {
        matrix: {
          'alice': { 'alice': 0.1, 'bob': 0.7, 'charlie': 0.2 },
          'bob': { 'alice': 0.3, 'bob': 0.4, 'charlie': 0.3 },
          'charlie': { 'alice': 0.2, 'bob': 0.2, 'charlie': 0.6 },
        }
      };
      
      const aliceMRD = mrd(matrix)('alice')(entities);
      const bobMRD = mrd(matrix)('bob')(entities);
      
      expect(aliceMRD).toBeGreaterThan(0);
      expect(bobMRD).toBeGreaterThan(0);
    });

    it('mrd - returns 0 when average MR is 0', () => {
      matrix = {
        matrix: {
          'alice': { 'alice': 0, 'bob': 0, 'charlie': 0 },
          'bob': { 'alice': 0, 'bob': 0, 'charlie': 0 },
          'charlie': { 'alice': 0, 'bob': 0, 'charlie': 0 },
        }
      };
      
      const aliceMRD = mrd(matrix)('alice')(entities);
      expect(aliceMRD).toBe(0);
    });

    it('mrdForAll - calculates for all entities', () => {
      const mrdMap = mrdForAll(matrix)(entities);
      
      expect(mrdMap.size).toBe(3);
      
      // In uniform case, all should be ~1
      for (const [id, value] of mrdMap) {
        expect(value).toBeCloseTo(1, 5);
      }
    });
  });

  describe('Reader Monad Operations', () => {
    let context: RecognitionContext;

    beforeEach(() => {
      context = {
        matrix,
        universe: entities,
        learningRate: 0.1,
      };
    });

    it('recognitionR - lifts to Reader', () => {
      const reader = recognitionR('alice');
      const dist: Distribution = runReader(context)(reader);
      
      expect(dist.total).toBeCloseTo(1, 5);
    });

    it('mutualR - lifts to Reader', () => {
      const reader = mutualR('alice')('bob');
      const mr = runReader(context)(reader);
      
      expect(mr).toBeCloseTo(1/3, 5);
    });

    it('tmrR - lifts to Reader', () => {
      const reader = tmrR('alice');
      const total = runReader(context)(reader);
      
      expect(total).toBeCloseTo(1, 5);
    });

    it('mrsR - lifts to Reader', () => {
      const reader = mrsR('alice');
      const dist: Distribution = runReader(context)(reader);
      
      expect(dist.total).toBeCloseTo(1, 5);
    });

    it('mrdR - lifts to Reader', () => {
      const reader = mrdR('alice')(entities);
      const density = runReader(context)(reader);
      
      expect(density).toBeCloseTo(1, 5);
    });

    it('updateRecognitionR - updates with learning', () => {
      const reader = updateRecognitionR('alice')('bob')(0.5);
      const newMatrix: RecognitionMatrix = runReader(context)(reader);
      
      const oldValue = matrix.matrix['alice']['bob'];
      const newValue = newMatrix.matrix['alice']['bob'];
      
      // Should have increased
      expect(newValue).toBeGreaterThanOrEqual(oldValue);
    });
  });

  describe('Higher-Order Operations', () => {
    it('filterRecognition - filters by predicate', () => {
      matrix = {
        matrix: {
          'alice': { 'alice': 0.1, 'bob': 0.7, 'charlie': 0.2 },
          'bob': { 'alice': 1/3, 'bob': 1/3, 'charlie': 1/3 },
          'charlie': { 'alice': 1/3, 'bob': 1/3, 'charlie': 1/3 },
        }
      };
      
      const dist = recognition(matrix)('alice')(entities);
      const filtered = filterRecognition((id, weight) => weight > 0.3)(dist);
      
      // Only bob should remain (0.7 > 0.3)
      expect(filtered.weights['bob']).toBeGreaterThan(0);
      expect(filtered.weights['alice']).toBeUndefined();
      expect(filtered.weights['charlie']).toBeUndefined();
    });

    it('foldRecognition - folds over distribution', () => {
      const dist = recognition(matrix)('alice')(entities);
      const sum = foldRecognition((acc, id, weight) => acc + weight, 0)(dist);
      
      expect(sum).toBeCloseTo(1, 5);
    });

    it('thresholdRecognition - filters by threshold', () => {
      matrix = {
        matrix: {
          'alice': { 'alice': 0.1, 'bob': 0.7, 'charlie': 0.2 },
          'bob': { 'alice': 1/3, 'bob': 1/3, 'charlie': 1/3 },
          'charlie': { 'alice': 1/3, 'bob': 1/3, 'charlie': 1/3 },
        }
      };
      
      const dist = recognition(matrix)('alice')(entities);
      const filtered = thresholdRecognition(0.3)(dist);
      
      expect(filtered.weights['bob']).toBeGreaterThan(0);
    });

    it('topKRecognition - keeps top K', () => {
      matrix = {
        matrix: {
          'alice': { 'alice': 0.1, 'bob': 0.7, 'charlie': 0.2 },
          'bob': { 'alice': 1/3, 'bob': 1/3, 'charlie': 1/3 },
          'charlie': { 'alice': 1/3, 'bob': 1/3, 'charlie': 1/3 },
        }
      };
      
      const dist = recognition(matrix)('alice')(entities);
      const top2 = topKRecognition(2)(dist);
      
      expect(Object.keys(top2.weights).length).toBe(2);
      expect(top2.weights['bob']).toBeGreaterThan(0); // Highest
      expect(top2.weights['charlie']).toBeGreaterThan(0); // Second
    });
  });

  describe('Matrix Operations', () => {
    it('uniformRecognitionMatrix - creates uniform', () => {
      const uniform = uniformRecognitionMatrix(entities);
      
      expect(Object.keys(uniform.matrix).length).toBe(3);
      
      for (const fromId of entities) {
        for (const toId of entities) {
          expect(uniform.matrix[fromId][toId]).toBeCloseTo(1/3, 5);
        }
      }
    });

    it('normalizeRecognitionMatrix - normalizes all rows', () => {
      matrix = {
        matrix: {
          'alice': { 'alice': 1, 'bob': 2, 'charlie': 3 },
          'bob': { 'alice': 4, 'bob': 5, 'charlie': 6 },
          'charlie': { 'alice': 7, 'bob': 8, 'charlie': 9 },
        }
      };
      
      const normalized = normalizeRecognitionMatrix(matrix)(entities);
      
      // Each row should sum to 1
      for (const fromId of entities) {
        let sum = 0;
        for (const toId of entities) {
          sum += normalized.matrix[fromId][toId];
        }
        expect(sum).toBeCloseTo(1, 5);
      }
    });

    it('getRecognitions - gets non-zero values', () => {
      matrix = {
        matrix: {
          'alice': { 'alice': 0, 'bob': 0.7, 'charlie': 0.3 },
          'bob': { 'alice': 1/3, 'bob': 1/3, 'charlie': 1/3 },
          'charlie': { 'alice': 1/3, 'bob': 1/3, 'charlie': 1/3 },
        }
      };
      
      const recognitions = getRecognitions(matrix)('alice');
      
      expect(recognitions.size).toBe(2); // Only bob and charlie
      expect(recognitions.has('bob')).toBe(true);
      expect(recognitions.has('charlie')).toBe(true);
      expect(recognitions.has('alice')).toBe(false);
    });

    it('receivedRecognition - calculates total received', () => {
      matrix = {
        matrix: {
          'alice': { 'alice': 0.1, 'bob': 0.6, 'charlie': 0.3 },
          'bob': { 'alice': 0.2, 'bob': 0.5, 'charlie': 0.3 },
          'charlie': { 'alice': 0.4, 'bob': 0.3, 'charlie': 0.3 },
        }
      };
      
      const bobReceived = receivedRecognition(matrix)('bob')(entities);
      
      // Sum of alice->bob, bob->bob, charlie->bob
      const expected = 0.6 + 0.5 + 0.3;
      expect(bobReceived).toBeCloseTo(expected, 5);
    });
  });

  describe('Composition and Currying Benefits', () => {
    it('partial application creates specialized functions', () => {
      const mutualInMatrix = mutual(matrix);
      const aliceMutual = mutualInMatrix('alice');
      
      // Can reuse aliceMutual for multiple targets
      const mrBob = aliceMutual('bob');
      const mrCharlie = aliceMutual('charlie');
      
      expect(mrBob).toBeCloseTo(1/3, 5);
      expect(mrCharlie).toBeCloseTo(1/3, 5);
    });

    it('can compose recognition operations', () => {
      const getMRS = mrs(matrix);
      const getAliceMRS = getMRS('alice');
      const aliceDist = getAliceMRS(entities);
      
      const filtered = thresholdRecognition(0.2)(aliceDist);
      const top2 = topKRecognition(2)(filtered);
      
      expect(top2.total).toBeGreaterThan(0);
    });

    it('Reader monad eliminates context passing', () => {
      const context: RecognitionContext = {
        matrix,
        universe: entities,
        learningRate: 0.1,
      };
      
      // Define computation without passing context
      const computation = recognitionR('alice');
      
      // Run with different contexts
      const result1: Distribution = runReader(context)(computation);
      const result2: Distribution = runReader({ ...context, learningRate: 0.2 })(computation);
      
      expect(result1.total).toBeCloseTo(1, 5);
      expect(result2.total).toBeCloseTo(1, 5);
    });
  });
});

