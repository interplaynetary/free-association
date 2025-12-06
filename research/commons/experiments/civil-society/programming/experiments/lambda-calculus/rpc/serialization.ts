/**
 * Serialization for Sparse Recognition Graphs
 * 
 * Optimized for network transmission - converts Maps to arrays
 * and ensures only non-zero edges are sent.
 */

import type { SparseRecognitionGraph } from '../src/sparse/types';
import { SparseOps } from '../src/sparse/types';
import type { SerializedSparseGraph, SerializedDistribution } from './types';
import type { Distribution } from '../src/sparse/operations';

/**
 * Serialize sparse graph for network transmission
 * Converts Map structure to array of [from, to, amount] tuples
 * 
 * For 10k entities with 500k edges:
 * - Map representation: ~6MB in memory
 * - Serialized: ~6MB as JSON
 * - Gzipped: ~2MB over network
 */
export function serializeSparseGraph(graph: SparseRecognitionGraph): SerializedSparseGraph {
  const edges: Array<[string, string, number]> = [];
  
  // Convert Map structure to array
  for (const [from, to, amount] of SparseOps.edges(graph)) {
    if (amount > 0) {  // Only non-zero edges
      edges.push([from, to, amount]);
    }
  }
  
  return {
    type: 'sparse-graph',
    edges,
    metadata: graph.metadata
  };
}

/**
 * Deserialize sparse graph from network data
 * Reconstructs Map structure from array of tuples
 */
export function deserializeSparseGraph(data: SerializedSparseGraph): SparseRecognitionGraph {
  const edges = new Map<string, Map<string, number>>();
  
  for (const [from, to, amount] of data.edges) {
    if (!edges.has(from)) {
      edges.set(from, new Map());
    }
    edges.get(from)!.set(to, amount);
  }
  
  return {
    edges,
    metadata: data.metadata
  };
}

/**
 * Serialize distribution for network transmission
 * Only sends non-zero probabilities
 */
export function serializeDistribution(dist: Distribution): SerializedDistribution {
  const weights: Array<[string, number]> = [];
  
  for (const [entityId, prob] of Object.entries(dist.distribution)) {
    if (prob > 0) {
      weights.push([entityId, prob]);
    }
  }
  
  return {
    type: 'distribution',
    weights
  };
}

/**
 * Deserialize distribution from network data
 */
export function deserializeDistribution(data: SerializedDistribution): Distribution {
  const distribution: Record<string, number> = {};
  
  for (const [entityId, prob] of data.weights) {
    distribution[entityId] = prob;
  }
  
  return { distribution };
}

/**
 * Compute hash of a Set for cache keys
 * Deterministic - sorted before hashing
 */
export function hashSet(set: Set<string>): string {
  const sorted = Array.from(set).sort();
  return sorted.join(',');
}

/**
 * Create cache key for MR
 */
export function mrCacheKey(a: string, b: string): string {
  // Symmetric key - always use same order
  const [first, second] = a < b ? [a, b] : [b, a];
  return `mr:${first}:${second}`;
}

/**
 * Create cache key for TMR
 */
export function tmrCacheKey(entity: string, universe: Set<string>): string {
  return `tmr:${entity}:${hashSet(universe)}`;
}

/**
 * Create cache key for MRS
 */
export function mrsCacheKey(entity: string, universe: Set<string>): string {
  return `mrs:${entity}:${hashSet(universe)}`;
}

/**
 * Create cache key for MRD
 */
export function mrdCacheKey(entity: string, collective: Set<string>): string {
  return `mrd:${entity}:${hashSet(collective)}`;
}

/**
 * Estimate serialized size of sparse graph
 * Useful for monitoring network usage
 */
export function estimateSerializedSize(graph: SparseRecognitionGraph): {
  bytes: number;
  edges: number;
  estimatedGzipped: number;
} {
  const edgeCount = SparseOps.edgeCount(graph);
  
  // Each edge: [from, to, amount]
  // Rough estimate: 40 bytes per edge (JSON overhead + strings + number)
  const bytes = edgeCount * 40;
  
  // Gzip typically achieves 3-4× compression on JSON
  const estimatedGzipped = bytes / 3.5;
  
  return {
    bytes,
    edges: edgeCount,
    estimatedGzipped
  };
}

