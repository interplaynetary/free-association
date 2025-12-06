/**
 * Tests for Sparse Matrix Operations
 * 
 * Verifies that sparse operations produce identical results to dense operations
 * and that performance characteristics are as expected.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  toSparse,
  fromSparse,
  SparseOps,
  type SparseRecognitionGraph
} from '../types';
import {
  sparseMutual,
  sparseTMR,
  sparseMRS,
  sparseMRD,
  computeStatistics
} from '../operations';

describe('Sparse Matrix Types', () => {
  let denseMatrix: Record<string, Record<string, number>>;
  let sparseGraph: SparseRecognitionGraph;

  beforeEach(() => {
    // Create test matrix
    denseMatrix = {
      alice: { bob: 0.6, charlie: 0.4 },
      bob: { alice: 0.3, charlie: 0.7 },
      charlie: { alice: 0.5, bob: 0.5 }
    };
    
    sparseGraph = toSparse(denseMatrix);
  });

  it('should convert dense to sparse correctly', () => {
    expect(sparseGraph.edges.size).toBe(3);
    expect(SparseOps.get(sparseGraph, 'alice', 'bob')).toBe(0.6);
    expect(SparseOps.get(sparseGraph, 'alice', 'charlie')).toBe(0.4);
    expect(SparseOps.get(sparseGraph, 'bob', 'alice')).toBe(0.3);
  });

  it('should return 0 for missing edges', () => {
    expect(SparseOps.get(sparseGraph, 'alice', 'dave')).toBe(0);
    expect(SparseOps.get(sparseGraph, 'dave', 'alice')).toBe(0);
  });

  it('should convert sparse back to dense correctly', () => {
    const backToDense = fromSparse(sparseGraph);
    expect(backToDense.alice.bob).toBe(0.6);
    expect(backToDense.bob.alice).toBe(0.3);
  });

  it('should filter out zero values when converting to sparse', () => {
    const denseWithZeros = {
      alice: { bob: 0.6, charlie: 0, dave: 0 },
      bob: { alice: 0, charlie: 0.5 }
    };
    
    const sparse = toSparse(denseWithZeros);
    expect(SparseOps.get(sparse, 'alice', 'charlie')).toBe(0);
    expect(SparseOps.get(sparse, 'alice', 'dave')).toBe(0);
    expect(SparseOps.get(sparse, 'bob', 'alice')).toBe(0);
    
    // Only non-zero edges stored
    const aliceEdges = SparseOps.outgoing(sparse, 'alice');
    expect(aliceEdges.size).toBe(1); // Only bob
  });
});

describe('Sparse Operations', () => {
  let sparseGraph: SparseRecognitionGraph;
  let universe: Set<string>;

  beforeEach(() => {
    sparseGraph = toSparse({
      alice: { bob: 0.6, charlie: 0.4 },
      bob: { alice: 0.3, charlie: 0.7 },
      charlie: { alice: 0.5, bob: 0.5 }
    });
    
    universe = new Set(['alice', 'bob', 'charlie']);
  });

  describe('sparseMutual', () => {
    it('should compute mutual recognition correctly', () => {
      const mutual = sparseMutual(sparseGraph);
      
      // min(0.6, 0.3) = 0.3
      expect(mutual('alice')('bob')).toBe(0.3);
      
      // min(0.4, 0.5) = 0.4
      expect(mutual('alice')('charlie')).toBe(0.4);
      
      // min(0.7, 0.5) = 0.5
      expect(mutual('bob')('charlie')).toBe(0.5);
    });

    it('should be symmetric', () => {
      const mutual = sparseMutual(sparseGraph);
      
      expect(mutual('alice')('bob')).toBe(mutual('bob')('alice'));
      expect(mutual('alice')('charlie')).toBe(mutual('charlie')('alice'));
      expect(mutual('bob')('charlie')).toBe(mutual('charlie')('bob'));
    });

    it('should return 0 for entities with no mutual recognition', () => {
      const mutual = sparseMutual(sparseGraph);
      expect(mutual('alice')('dave')).toBe(0);
      expect(mutual('dave')('alice')).toBe(0);
    });
  });

  describe('sparseTMR', () => {
    it('should compute total mutual recognition correctly', () => {
      const tmr = sparseTMR(sparseGraph);
      
      // Alice: MR(alice,bob) + MR(alice,charlie) = 0.3 + 0.4 = 0.7
      expect(tmr('alice')(universe)).toBeCloseTo(0.7, 5);
      
      // Bob: MR(bob,alice) + MR(bob,charlie) = 0.3 + 0.5 = 0.8
      expect(tmr('bob')(universe)).toBeCloseTo(0.8, 5);
      
      // Charlie: MR(charlie,alice) + MR(charlie,bob) = 0.4 + 0.5 = 0.9
      expect(tmr('charlie')(universe)).toBeCloseTo(0.9, 5);
    });

    it('should only count universe entities', () => {
      const tmr = sparseTMR(sparseGraph);
      const smallUniverse = new Set(['alice', 'bob']);
      
      // Should only count MR with bob, not charlie
      expect(tmr('alice')(smallUniverse)).toBeCloseTo(0.3, 5);
    });
  });

  describe('sparseMRS', () => {
    it('should compute normalized mutual recognition share', () => {
      const mrs = sparseMRS(sparseGraph);
      const dist = mrs('alice')(universe);
      
      // Total MR = 0.7
      // bob: 0.3/0.7 ≈ 0.4286
      // charlie: 0.4/0.7 ≈ 0.5714
      expect(dist.distribution.bob).toBeCloseTo(0.4286, 3);
      expect(dist.distribution.charlie).toBeCloseTo(0.5714, 3);
      
      // Should sum to 1
      const sum = Object.values(dist.distribution).reduce((a, b) => a + b, 0);
      expect(sum).toBeCloseTo(1, 5);
    });

    it('should only include entities in universe', () => {
      const mrs = sparseMRS(sparseGraph);
      const smallUniverse = new Set(['alice', 'bob']);
      const dist = mrs('alice')(smallUniverse);
      
      // Should only include bob, not charlie
      expect(dist.distribution.bob).toBe(1); // 100% to bob
      expect(dist.distribution.charlie).toBeUndefined();
    });

    it('should handle entity with no mutual recognition', () => {
      const isolatedGraph = toSparse({
        alice: { bob: 0.5 },
        bob: { charlie: 0.5 },
        charlie: {}
      });
      
      const mrs = sparseMRS(isolatedGraph);
      const dist = mrs('charlie')(universe);
      
      // Should return empty distribution
      expect(Object.keys(dist.distribution)).toHaveLength(0);
    });
  });

  describe('sparseMRD', () => {
    it('should compute mutual recognition density', () => {
      const mrd = sparseMRD(sparseGraph);
      
      const collectiveMembers = new Set(['alice', 'bob', 'charlie']);
      
      // Calculate expected values manually:
      // Total MR in collective = MR(a,b) + MR(a,c) + MR(b,c) = 0.3 + 0.4 + 0.5 = 1.2
      // Avg MR = 1.2 / 3 = 0.4
      
      // Alice MR = 0.3 + 0.4 = 0.7
      // MRD(alice) = 0.7 / 0.4 = 1.75
      expect(mrd('alice')(collectiveMembers)).toBeCloseTo(1.75, 3);
      
      // Bob MR = 0.3 + 0.5 = 0.8
      // MRD(bob) = 0.8 / 0.4 = 2.0
      expect(mrd('bob')(collectiveMembers)).toBeCloseTo(2.0, 3);
      
      // Charlie MR = 0.4 + 0.5 = 0.9
      // MRD(charlie) = 0.9 / 0.4 = 2.25
      expect(mrd('charlie')(collectiveMembers)).toBeCloseTo(2.25, 3);
    });

    it('should return 0 for empty collective', () => {
      const mrd = sparseMRD(sparseGraph);
      expect(mrd('alice')(new Set())).toBe(0);
    });
  });
});

describe('Sparse Graph Statistics', () => {
  it('should compute statistics correctly', () => {
    const graph = toSparse({
      alice: { bob: 0.6, charlie: 0.4 },
      bob: { alice: 0.3, charlie: 0.7 },
      charlie: { alice: 0.5, bob: 0.5 }
    });
    
    const stats = computeStatistics(graph);
    
    expect(stats.entities).toBe(3);
    expect(stats.edges).toBe(6);
    expect(stats.avgDegree).toBeCloseTo(2, 5);
    
    // Sparsity = 1 - (6 / 9) = 0.333...
    expect(stats.sparsity).toBeCloseTo(0.333, 3);
    
    // All edges are symmetric
    expect(stats.symmetricEdges).toBe(3);
    
    // Budget constraint satisfied
    expect(stats.budgetViolations).toBe(0);
  });

  it('should detect budget violations', () => {
    const graph = toSparse({
      alice: { bob: 0.6, charlie: 0.5 }, // Total = 1.1 > 1.0
      bob: { alice: 0.3 }
    });
    
    const stats = computeStatistics(graph);
    expect(stats.budgetViolations).toBe(1);
  });
});

describe('Performance Characteristics', () => {
  it('should handle large sparse networks efficiently', () => {
    // Create network with 1000 entities, avg 10 connections each
    const dense: Record<string, Record<string, number>> = {};
    
    for (let i = 0; i < 1000; i++) {
      dense[`entity${i}`] = {};
      
      // Connect to 10 random entities
      for (let j = 0; j < 10; j++) {
        const target = Math.floor(Math.random() * 1000);
        if (target !== i) {
          dense[`entity${i}`][`entity${target}`] = Math.random();
        }
      }
    }
    
    const startConvert = Date.now();
    const sparse = toSparse(dense);
    const convertTime = Date.now() - startConvert;
    
    expect(convertTime).toBeLessThan(100); // Should be fast
    
    const stats = sparse.metadata || SparseOps.computeMetadata(sparse);
    expect(stats.totalEntities).toBe(1000);
    expect(stats.sparsity).toBeGreaterThan(0.99); // Very sparse
    
    // Test MR computation performance
    const startMR = Date.now();
    const mutual = sparseMutual(sparse);
    for (let i = 0; i < 100; i++) {
      mutual('entity0')(`entity${i}`);
    }
    const mrTime = Date.now() - startMR;
    
    expect(mrTime).toBeLessThan(50); // Should be very fast
  });
});

describe('Edge Cases', () => {
  it('should handle empty graph', () => {
    const empty = toSparse({});
    
    expect(SparseOps.get(empty, 'alice', 'bob')).toBe(0);
    expect(SparseOps.edgeCount(empty)).toBe(0);
    expect(SparseOps.entities(empty).size).toBe(0);
  });

  it('should handle single entity', () => {
    const single = toSparse({
      alice: {}
    });
    
    const mutual = sparseMutual(single);
    expect(mutual('alice')('alice')).toBe(0);
    
    const tmr = sparseTMR(single);
    expect(tmr('alice')(new Set(['alice']))).toBe(0);
  });

  it('should handle unidirectional edges', () => {
    const unidirectional = toSparse({
      alice: { bob: 0.5 },
      bob: {}
    });
    
    const mutual = sparseMutual(unidirectional);
    // min(0.5, 0) = 0
    expect(mutual('alice')('bob')).toBe(0);
  });
});

