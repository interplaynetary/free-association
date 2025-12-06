/**
 * Tests for Serialization
 */

import { describe, it, expect } from 'vitest';
import {
  serializeSparseGraph,
  deserializeSparseGraph,
  serializeDistribution,
  deserializeDistribution,
  hashSet,
  mrCacheKey,
  tmrCacheKey,
  mrsCacheKey,
  mrdCacheKey
} from '../serialization';
import { toSparse } from '../../src/sparse/types';
import type { Distribution } from '../../src/sparse/operations';

describe('Sparse Graph Serialization', () => {
  it('should serialize sparse graph correctly', () => {
    const graph = toSparse({
      alice: { bob: 0.6, charlie: 0.4 },
      bob: { alice: 0.3 }
    });
    
    const serialized = serializeSparseGraph(graph);
    
    expect(serialized.type).toBe('sparse-graph');
    expect(serialized.edges).toHaveLength(3);
    expect(serialized.edges).toContainEqual(['alice', 'bob', 0.6]);
    expect(serialized.edges).toContainEqual(['alice', 'charlie', 0.4]);
    expect(serialized.edges).toContainEqual(['bob', 'alice', 0.3]);
  });

  it('should deserialize sparse graph correctly', () => {
    const serialized = {
      type: 'sparse-graph' as const,
      edges: [
        ['alice', 'bob', 0.6],
        ['alice', 'charlie', 0.4],
        ['bob', 'alice', 0.3]
      ] as [string, string, number][]
    };
    
    const graph = deserializeSparseGraph(serialized);
    
    expect(graph.edges.size).toBe(2); // alice and bob
    expect(graph.edges.get('alice')?.get('bob')).toBe(0.6);
    expect(graph.edges.get('alice')?.get('charlie')).toBe(0.4);
    expect(graph.edges.get('bob')?.get('alice')).toBe(0.3);
  });

  it('should round-trip serialize/deserialize', () => {
    const original = toSparse({
      alice: { bob: 0.6, charlie: 0.4 },
      bob: { alice: 0.3, charlie: 0.7 },
      charlie: { bob: 0.5 }
    });
    
    const serialized = serializeSparseGraph(original);
    const deserialized = deserializeSparseGraph(serialized);
    
    // Check all edges match
    for (const [from, to, amount] of serialized.edges) {
      expect(deserialized.edges.get(from)?.get(to)).toBe(amount);
    }
  });

  it('should filter out zero values', () => {
    const graph = toSparse({
      alice: { bob: 0.5, charlie: 0 },
      bob: { alice: 0 }
    });
    
    const serialized = serializeSparseGraph(graph);
    
    // Should only have alice -> bob
    expect(serialized.edges).toHaveLength(1);
    expect(serialized.edges[0]).toEqual(['alice', 'bob', 0.5]);
  });
});

describe('Distribution Serialization', () => {
  it('should serialize distribution correctly', () => {
    const dist: Distribution = {
      distribution: {
        alice: 0.4,
        bob: 0.3,
        charlie: 0.3
      }
    };
    
    const serialized = serializeDistribution(dist);
    
    expect(serialized.type).toBe('distribution');
    expect(serialized.weights).toHaveLength(3);
    expect(serialized.weights).toContainEqual(['alice', 0.4]);
    expect(serialized.weights).toContainEqual(['bob', 0.3]);
  });

  it('should deserialize distribution correctly', () => {
    const serialized = {
      type: 'distribution' as const,
      weights: [
        ['alice', 0.4],
        ['bob', 0.3],
        ['charlie', 0.3]
      ] as [string, number][]
    };
    
    const dist = deserializeDistribution(serialized);
    
    expect(dist.distribution.alice).toBe(0.4);
    expect(dist.distribution.bob).toBe(0.3);
    expect(dist.distribution.charlie).toBe(0.3);
  });

  it('should filter out zero probabilities', () => {
    const dist: Distribution = {
      distribution: {
        alice: 0.5,
        bob: 0,
        charlie: 0.5
      }
    };
    
    const serialized = serializeDistribution(dist);
    
    expect(serialized.weights).toHaveLength(2);
    expect(serialized.weights.map(w => w[0])).not.toContain('bob');
  });
});

describe('Hash and Cache Keys', () => {
  it('should hash Set deterministically', () => {
    const set1 = new Set(['alice', 'bob', 'charlie']);
    const set2 = new Set(['charlie', 'bob', 'alice']); // Different order
    
    // Should produce same hash (sorted)
    expect(hashSet(set1)).toBe(hashSet(set2));
  });

  it('should create symmetric MR cache key', () => {
    // Key should be same regardless of order
    expect(mrCacheKey('alice', 'bob')).toBe(mrCacheKey('bob', 'alice'));
    expect(mrCacheKey('alice', 'bob')).toBe('mr:alice:bob');
  });

  it('should create TMR cache key', () => {
    const universe = new Set(['alice', 'bob', 'charlie']);
    const key = tmrCacheKey('alice', universe);
    
    expect(key).toContain('tmr:alice:');
    expect(key).toContain('alice,bob,charlie'); // Sorted
  });

  it('should create MRS cache key', () => {
    const universe = new Set(['bob', 'alice', 'charlie']);
    const key = mrsCacheKey('alice', universe);
    
    expect(key).toContain('mrs:alice:');
  });

  it('should create MRD cache key', () => {
    const collective = new Set(['alice', 'bob']);
    const key = mrdCacheKey('charlie', collective);
    
    expect(key).toContain('mrd:charlie:');
  });
});

