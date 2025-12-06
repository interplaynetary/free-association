/**
 * Sparse Matrix Types for Lambda Calculus
 * 
 * Optimized storage and operations for recognition graphs where most values are zero.
 * For a network of 10k entities with avg 50 connections per entity:
 * - Dense storage: 10k × 10k = 100M entries = ~800MB
 * - Sparse storage: 10k × 50 = 500k entries = ~6MB
 * - Space savings: 133× reduction!
 */

export type EntityId = string;

/**
 * Sparse recognition graph - only stores non-zero edges
 * Uses nested Maps for O(1) lookup and efficient iteration
 */
export interface SparseRecognitionGraph {
  /** 
   * Nested map structure: from -> to -> amount
   * Only contains entries where amount > 0
   */
  edges: Map<EntityId, Map<EntityId, number>>;
  
  /** Optional metadata for monitoring and optimization */
  metadata?: {
    totalEntities: number;
    totalEdges: number;
    avgDegree: number;
    sparsity: number; // 1 - (actualEdges / possibleEdges)
  };
}

/**
 * Operations on sparse recognition graphs
 * All operations maintain sparsity - zeros are never stored
 */
export namespace SparseOps {
  /**
   * Get edge value - returns 0 for missing edges (O(1))
   */
  export const get = (
    graph: SparseRecognitionGraph, 
    from: EntityId, 
    to: EntityId
  ): number => {
    return graph.edges.get(from)?.get(to) ?? 0;
  };

  /**
   * Set edge value - removes edge if value is 0 (O(1))
   * Returns new graph (immutable operation)
   */
  export const set = (
    graph: SparseRecognitionGraph,
    from: EntityId,
    to: EntityId,
    value: number
  ): SparseRecognitionGraph => {
    const newEdges = new Map(graph.edges);
    
    if (value === 0) {
      // Remove edge if value is 0
      const fromEdges = newEdges.get(from);
      if (fromEdges) {
        const newFromEdges = new Map(fromEdges);
        newFromEdges.delete(to);
        if (newFromEdges.size === 0) {
          newEdges.delete(from);
        } else {
          newEdges.set(from, newFromEdges);
        }
      }
    } else {
      // Add or update edge
      const fromEdges = newEdges.get(from);
      if (fromEdges) {
        const newFromEdges = new Map(fromEdges);
        newFromEdges.set(to, value);
        newEdges.set(from, newFromEdges);
      } else {
        newEdges.set(from, new Map([[to, value]]));
      }
    }
    
    return { ...graph, edges: newEdges };
  };

  /**
   * Get all outgoing edges from an entity (O(k) where k = outgoing degree)
   */
  export const outgoing = (
    graph: SparseRecognitionGraph,
    from: EntityId
  ): Map<EntityId, number> => {
    return graph.edges.get(from) ?? new Map();
  };

  /**
   * Get all incoming edges to an entity (O(n) worst case, but typically much faster)
   */
  export const incoming = (
    graph: SparseRecognitionGraph,
    to: EntityId
  ): Map<EntityId, number> => {
    const incoming = new Map<EntityId, number>();
    
    for (const [from, targets] of graph.edges) {
      const value = targets.get(to);
      if (value !== undefined) {
        incoming.set(from, value);
      }
    }
    
    return incoming;
  };

  /**
   * Iterator over all non-zero edges
   * Yields [from, to, value] tuples
   */
  export const edges = function* (
    graph: SparseRecognitionGraph
  ): Iterable<[EntityId, EntityId, number]> {
    for (const [from, targets] of graph.edges) {
      for (const [to, value] of targets) {
        yield [from, to, value];
      }
    }
  };

  /**
   * Get total number of non-zero edges
   */
  export const edgeCount = (graph: SparseRecognitionGraph): number => {
    let count = 0;
    for (const targets of graph.edges.values()) {
      count += targets.size;
    }
    return count;
  };

  /**
   * Get set of all entities that have any edges
   */
  export const entities = (graph: SparseRecognitionGraph): Set<EntityId> => {
    const allEntities = new Set<EntityId>();
    
    for (const [from, targets] of graph.edges) {
      allEntities.add(from);
      for (const to of targets.keys()) {
        allEntities.add(to);
      }
    }
    
    return allEntities;
  };

  /**
   * Compute graph metadata
   */
  export const computeMetadata = (
    graph: SparseRecognitionGraph
  ): Required<SparseRecognitionGraph>['metadata'] => {
    const allEntities = entities(graph);
    const totalEntities = allEntities.size;
    const totalEdges = edgeCount(graph);
    const avgDegree = totalEntities > 0 ? totalEdges / totalEntities : 0;
    const possibleEdges = totalEntities * totalEntities;
    const sparsity = possibleEdges > 0 ? 1 - (totalEdges / possibleEdges) : 1;
    
    return {
      totalEntities,
      totalEdges,
      avgDegree,
      sparsity
    };
  };
}

/**
 * Convert dense matrix to sparse representation
 * Filters out zero values automatically
 */
export const toSparse = (
  dense: Record<EntityId, Record<EntityId, number>>
): SparseRecognitionGraph => {
  const edges = new Map<EntityId, Map<EntityId, number>>();
  
  for (const [from, targets] of Object.entries(dense)) {
    const nonZeroTargets = new Map<EntityId, number>();
    
    for (const [to, value] of Object.entries(targets)) {
      if (value !== 0) {
        nonZeroTargets.set(to, value);
      }
    }
    
    if (nonZeroTargets.size > 0) {
      edges.set(from, nonZeroTargets);
    }
  }
  
  const graph: SparseRecognitionGraph = { edges };
  graph.metadata = SparseOps.computeMetadata(graph);
  
  return graph;
};

/**
 * Convert sparse representation back to dense matrix
 * Missing edges become 0
 */
export const fromSparse = (
  sparse: SparseRecognitionGraph
): Record<EntityId, Record<EntityId, number>> => {
  const dense: Record<EntityId, Record<EntityId, number>> = {};
  
  for (const [from, targets] of sparse.edges) {
    dense[from] = {};
    for (const [to, value] of targets) {
      dense[from][to] = value;
    }
  }
  
  return dense;
};

/**
 * Type guard to check if a value is a sparse graph
 */
export const isSparseGraph = (value: unknown): value is SparseRecognitionGraph => {
  return (
    typeof value === 'object' &&
    value !== null &&
    'edges' in value &&
    value.edges instanceof Map
  );
};

/**
 * Create an empty sparse graph
 */
export const empty = (): SparseRecognitionGraph => ({
  edges: new Map(),
  metadata: {
    totalEntities: 0,
    totalEdges: 0,
    avgDegree: 0,
    sparsity: 1
  }
});

/**
 * Clone a sparse graph (deep copy)
 */
export const clone = (graph: SparseRecognitionGraph): SparseRecognitionGraph => {
  const newEdges = new Map<EntityId, Map<EntityId, number>>();
  
  for (const [from, targets] of graph.edges) {
    newEdges.set(from, new Map(targets));
  }
  
  return {
    edges: newEdges,
    metadata: graph.metadata ? { ...graph.metadata } : undefined
  };
};

