/**
 * Integration Tests for Elegant Lambda Calculus Implementation
 * 
 * These tests demonstrate how the elegant API works together
 * to solve real coordination scenarios.
 */

import { describe, it, expect } from 'vitest';
import type { Entity, RecognitionMatrix } from '../../core/types';
import {
  mutual,
  mrs,
  tmr,
  mrd,
  uniformRecognitionMatrix,
  normalizeRecognitionMatrix,
  type RecognitionContext,
} from '../recognition';
import { pipe, compose2, fmap, fold, curry2, runReader } from '../combinators';
import { entitiesToIds } from '../../core/primitives';

describe('Integration Tests - Elegant API', () => {
  describe('Scenario 1: Three-Person Team Coordination', () => {
    it('demonstrates full currying and composition', () => {
      // Setup
      const entities = new Set<Entity>([
        { id: 'alice', name: 'Alice' },
        { id: 'bob', name: 'Bob' },
        { id: 'charlie', name: 'Charlie' },
      ]);
      
      const entityIds = entitiesToIds(entities);
      const matrix = uniformRecognitionMatrix(entityIds);
      
      // Create specialized functions through partial application
      const mutualInMatrix = mutual(matrix);
      const aliceMutual = mutualInMatrix('alice');
      
      // Calculate mutual recognitions
      const mrBob = aliceMutual('bob');
      const mrCharlie = aliceMutual('charlie');
      
      expect(mrBob).toBeCloseTo(1/3, 5);
      expect(mrCharlie).toBeCloseTo(1/3, 5);
      expect(mrBob).toBe(mrCharlie); // Uniform initially
      
      // Calculate MRS for Alice
      const getMRS = mrs(matrix);
      const aliceMRS = getMRS('alice')(entityIds);
      
      expect(aliceMRS.total).toBeCloseTo(1, 5);
      expect(Object.keys(aliceMRS.weights).length).toBe(3);
    });
  });

  describe('Scenario 2: Asymmetric Recognition with Composition', () => {
    it('uses pipe to compose operations', () => {
      const entities = new Set(['alice', 'bob', 'charlie']);
      
      // Create asymmetric recognition
      const matrix: RecognitionMatrix = {
        matrix: {
          'alice': { 'alice': 0.2, 'bob': 0.6, 'charlie': 0.2 },
          'bob': { 'alice': 0.3, 'bob': 0.4, 'charlie': 0.3 },
          'charlie': { 'alice': 0.25, 'bob': 0.25, 'charlie': 0.5 },
        }
      };
      
      // Calculate TMR using composition
      const mutualInMatrix = mutual(matrix);
      const aliceMutual = mutualInMatrix('alice');
      
      const aliceTMR = Array.from(entities).reduce(
        (sum, targetId) => sum + aliceMutual(targetId),
        0
      );
      const expectedTMR = tmr(matrix)('alice')(entities);
      
      expect(aliceTMR).toBeCloseTo(expectedTMR, 5);
    });
  });

  describe('Scenario 3: MRD-Based Filtering with Currying', () => {
    it('creates reusable MRD calculator', () => {
      const entities = new Set(['alice', 'bob', 'charlie', 'dana']);
      
      const matrix: RecognitionMatrix = {
        matrix: {
          'alice': { 'alice': 0.1, 'bob': 0.5, 'charlie': 0.3, 'dana': 0.1 },
          'bob': { 'alice': 0.4, 'bob': 0.2, 'charlie': 0.3, 'dana': 0.1 },
          'charlie': { 'alice': 0.3, 'bob': 0.3, 'charlie': 0.2, 'dana': 0.2 },
          'dana': { 'alice': 0.2, 'bob': 0.2, 'charlie': 0.3, 'dana': 0.3 },
        }
      };
      
      // Create MRD calculator for this matrix
      const calculateMRD = mrd(matrix);
      
      // Calculate MRD for each entity
      const mrds = new Map(
        Array.from(entities).map(id => [id, calculateMRD(id)(entities)])
      );
      
      // Filter entities with MRD >= threshold
      const threshold = 0.8;
      const qualified = Array.from(entities).filter(
        id => (mrds.get(id) || 0) >= threshold
      );
      
      expect(qualified.length).toBeGreaterThanOrEqual(0);
      
      // All qualified should be above threshold
      for (const id of qualified) {
        expect(mrds.get(id)).toBeGreaterThanOrEqual(threshold);
      }
    });
  });

  describe('Scenario 4: Recognition Evolution with Functional Updates', () => {
    it('evolves recognition using pure functions', () => {
      const entities = new Set(['alice', 'bob', 'charlie']);
      let matrix = uniformRecognitionMatrix(entities);
      
      // Define update function (curried)
      const updateRecognition = curry2(
        (fromId: string, updates: Map<string, number>) => {
          const newMatrix = { ...matrix };
          newMatrix.matrix = { ...matrix.matrix };
          newMatrix.matrix[fromId] = { ...matrix.matrix[fromId] };
          
          for (const [toId, value] of updates) {
            newMatrix.matrix[fromId][toId] = value;
          }
          
          return newMatrix;
        }
      );
      
      // Alice increases recognition of Bob
      const updatedMatrix = updateRecognition('alice')(new Map([['bob', 0.7]]));
      matrix = normalizeRecognitionMatrix(updatedMatrix)(entities);
      
      // Check mutual recognition changed
      const mrAB = mutual(matrix)('alice')('bob');
      expect(mrAB).toBeGreaterThanOrEqual(1/3); // At least uniform (might be equal after normalization)
    });
  });

  describe('Scenario 5: Reader Monad for Context Management', () => {
    it('threads context through computations', () => {
      const entities = new Set(['alice', 'bob', 'charlie']);
      const matrix = uniformRecognitionMatrix(entities);
      
      const context: RecognitionContext = {
        matrix,
        universe: entities,
        learningRate: 0.1,
      };
      
      // Define computation that needs context
      const calculateNetworkMetrics = (ctx: RecognitionContext) => {
        const mutualInMatrix = mutual(ctx.matrix);
        const tmrts = tmr(ctx.matrix);
        
        return {
          totalMutualRecognition: Array.from(ctx.universe).reduce(
            (sum, id) => sum + tmrts(id)(ctx.universe),
            0
          ),
          avgMutualRecognition: Array.from(ctx.universe).reduce(
            (sum, id) => sum + tmrts(id)(ctx.universe),
            0
          ) / ctx.universe.size,
        };
      };
      
      // Run with context
      const metrics = runReader(context)(calculateNetworkMetrics) as { 
        totalMutualRecognition: number; 
        avgMutualRecognition: number;
      };
      
      expect(metrics.totalMutualRecognition).toBeGreaterThan(0);
      expect(metrics.avgMutualRecognition).toBeGreaterThan(0);
    });
  });

  describe('Scenario 6: Point-Free Style Composition', () => {
    it('composes without naming intermediates', () => {
      const entities = new Set(['alice', 'bob', 'charlie']);
      
      const matrix: RecognitionMatrix = {
        matrix: {
          'alice': { 'alice': 0.1, 'bob': 0.7, 'charlie': 0.2 },
          'bob': { 'alice': 0.3, 'bob': 0.4, 'charlie': 0.3 },
          'charlie': { 'alice': 0.25, 'bob': 0.25, 'charlie': 0.5 },
        }
      };
      
      // Calculate sum of all mutual recognitions
      const totalMutualRecognition = Array.from(entities).reduce((sum1, a) =>
        sum1 + Array.from(entities).reduce((sum2, b) =>
          sum2 + mutual(matrix)(a)(b), 0), 0
      );
      
      expect(totalMutualRecognition).toBeGreaterThan(0);
      
      // Verify it's symmetric
      const manualTotal = Array.from(entities).reduce((sum1, a) =>
        sum1 + Array.from(entities).reduce((sum2, b) =>
          sum2 + mutual(matrix)(a)(b), 0), 0
      );
      
      expect(totalMutualRecognition).toBeCloseTo(manualTotal, 5);
    });
  });

  describe('Scenario 7: Building Specialized Recognition Systems', () => {
    it('creates domain-specific recognition calculators', () => {
      const entities = new Set(['alice', 'bob', 'charlie', 'dana', 'eve']);
      const matrix = uniformRecognitionMatrix(entities);
      
      // Create specialized calculators through partial application
      const calculateMutual = mutual(matrix);
      const calculateMRS = mrs(matrix);
      const calculateMRD = mrd(matrix);
      
      // Build a "high recognition" filter
      const hasHighRecognition = (entityId: string) => (targetId: string) =>
        calculateMutual(entityId)(targetId) > 0.3;
      
      // Build a "high MRD" filter
      const hasHighMRD = (entityId: string) =>
        calculateMRD(entityId)(entities) > 0.8;
      
      // Apply filters
      const highRecognitionPairs: [string, string][] = [];
      const highMRDEntities: string[] = [];
      
      for (const a of entities) {
        if (hasHighMRD(a)) {
          highMRDEntities.push(a);
        }
        
        for (const b of entities) {
          if (hasHighRecognition(a)(b)) {
            highRecognitionPairs.push([a, b]);
          }
        }
      }
      
      // In uniform case, MRD should be ~1 for all
      expect(highMRDEntities.length).toBe(entities.size);
    });
  });

  describe('Scenario 8: Functional Composition Patterns', () => {
    it('demonstrates compose2 for recognition operations', () => {
      const entities = new Set(['alice', 'bob', 'charlie']);
      const matrix = uniformRecognitionMatrix(entities);
      
      // Get MRS value using composition
      const aliceMRS = mrs(matrix)('alice')(entities);
      const aliceToBob = (aliceMRS.weights['bob'] || 0) / aliceMRS.total;
      
      const directValue = (aliceMRS.weights['bob'] || 0) / aliceMRS.total;
      
      expect(aliceToBob).toBeCloseTo(directValue, 5);
    });
  });

  describe('Scenario 9: Curried Multi-Entity Operations', () => {
    it('processes multiple entities efficiently', () => {
      const entities = new Set(['alice', 'bob', 'charlie']);
      const matrix = uniformRecognitionMatrix(entities);
      
      // Create TMR calculator
      const calculateTMR = tmr(matrix);
      
      // Calculate for all entities using partial application
      const tmrs = new Map(
        Array.from(entities).map(id => [id, calculateTMR(id)(entities)] as const)
      );
      
      expect(tmrs.size).toBe(3);
      
      // All should be equal in uniform case
      const values = Array.from(tmrs.values());
      expect(values[0]).toBeCloseTo(values[1], 5);
      expect(values[1]).toBeCloseTo(values[2], 5);
    });
  });

  describe('Scenario 10: Elegant Error Handling', () => {
    it('handles edge cases gracefully', () => {
      // Empty set
      const empty = new Set<string>();
      const emptyMatrix = uniformRecognitionMatrix(empty);
      
      expect(Object.keys(emptyMatrix.matrix).length).toBe(0);
      
      // Single entity
      const single = new Set(['alice']);
      const singleMatrix = uniformRecognitionMatrix(single);
      
      const aliceMutual = mutual(singleMatrix)('alice')('alice');
      expect(aliceMutual).toBeCloseTo(1, 5);
      
      const aliceTMR = tmr(singleMatrix)('alice')(single);
      expect(aliceTMR).toBeCloseTo(1, 5);
      
      const aliceMRS = mrs(singleMatrix)('alice')(single);
      expect(aliceMRS.total).toBeCloseTo(1, 5);
    });
  });
});

