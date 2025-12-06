/**
 * Sparse Matrix Operations for Lambda Calculus
 * 
 * All operations work efficiently with sparse representations:
 * - Only iterate over non-zero edges
 * - O(k) complexity where k = number of connections, not O(n) where n = total entities
 * - Memory-efficient for large networks
 */

import type { SparseRecognitionGraph, EntityId } from './types';
import { SparseOps } from './types';

/**
 * Distribution type (same as core)
 */
export interface Distribution {
  distribution: Record<EntityId, number>;
}

/**
 * Sparse mutual recognition - computes min(R_ab, R_ba)
 * O(1) complexity - just two map lookups
 */
export const sparseMutual = (graph: SparseRecognitionGraph) => 
  (a: EntityId) => (b: EntityId): number => {
    const R_ab = SparseOps.get(graph, a, b);
    const R_ba = SparseOps.get(graph, b, a);
    return Math.min(R_ab, R_ba);
  };

/**
 * Sparse Total Mutual Recognition - sum of all mutual recognitions for entity
 * O(k) where k = entity's outgoing degree
 */
export const sparseTMR = (graph: SparseRecognitionGraph) =>
  (entity: EntityId) => (universe: Set<EntityId>): number => {
    const outgoing = SparseOps.outgoing(graph, entity);
    let totalMR = 0;
    
    // Only iterate entities we have recognition with (sparse!)
    for (const [otherId, R_ab] of outgoing) {
      if (!universe.has(otherId)) continue;
      
      const R_ba = SparseOps.get(graph, otherId, entity);
      const mr = Math.min(R_ab, R_ba);
      totalMR += mr;
    }
    
    return totalMR;
  };

/**
 * Sparse Mutual Recognition Share - normalized mutual recognition distribution
 * O(k) where k = entity's outgoing degree
 * 
 * Returns only non-zero entries in the distribution
 */
export const sparseMRS = (graph: SparseRecognitionGraph) =>
  (entity: EntityId) => (universe: Set<EntityId>): Distribution => {
    const outgoing = SparseOps.outgoing(graph, entity);
    const mrs: Record<EntityId, number> = {};
    let totalMR = 0;
    
    // Compute mutual recognitions (only for entities with edges)
    for (const [otherId, R_ab] of outgoing) {
      if (!universe.has(otherId)) continue;
      
      const R_ba = SparseOps.get(graph, otherId, entity);
      const mr = Math.min(R_ab, R_ba);
      
      if (mr > 0) {
        mrs[otherId] = mr;
        totalMR += mr;
      }
    }
    
    // Normalize
    if (totalMR > 0) {
      for (const id in mrs) {
        mrs[id] /= totalMR;
      }
    }
    
    return { distribution: mrs };
  };

/**
 * Sparse Mutual Recognition Density - entity's integration with collective
 * O(k * |C|) where k = entity's connections, |C| = collective size
 * 
 * MRD_C(i) = (|C| * sum of MR with collective) / (sum of all MR in collective)
 */
export const sparseMRD = (graph: SparseRecognitionGraph) =>
  (entity: EntityId) => (collectiveMembers: Set<EntityId>): number => {
    if (collectiveMembers.size === 0) return 0;
    
    let entityMRSum = 0;
    let totalMRSum = 0;
    
    const outgoing = SparseOps.outgoing(graph, entity);
    
    // Compute entity's MR with collective members
    for (const memberId of collectiveMembers) {
      const R_em = outgoing.get(memberId) ?? 0;
      const R_me = SparseOps.get(graph, memberId, entity);
      const mr = Math.min(R_em, R_me);
      entityMRSum += mr;
    }
    
    // Compute total MR between all collective members
    for (const memberId of collectiveMembers) {
      const memberOutgoing = SparseOps.outgoing(graph, memberId);
      
      for (const otherMemberId of collectiveMembers) {
        if (memberId === otherMemberId) continue;
        
        const R_mo = memberOutgoing.get(otherMemberId) ?? 0;
        const R_om = SparseOps.get(graph, otherMemberId, memberId);
        const mr = Math.min(R_mo, R_om);
        totalMRSum += mr;
      }
    }
    
    // Average MR in collective
    const avgMR = collectiveMembers.size > 0 ? totalMRSum / collectiveMembers.size : 0;
    
    // MRD = entity's MR share relative to average
    return avgMR === 0 ? 0 : entityMRSum / avgMR;
  };

/**
 * Sparse Relative Mutual Recognition - MR relative to entity's total MR
 * O(k) where k = entity's connections
 */
export const sparseRMR = (graph: SparseRecognitionGraph) =>
  (entity: EntityId) => (otherId: EntityId) => (universe: Set<EntityId>): number => {
    const outgoing = SparseOps.outgoing(graph, entity);
    
    // Compute total MR
    let totalMR = 0;
    for (const [targetId, R_ab] of outgoing) {
      if (!universe.has(targetId)) continue;
      
      const R_ba = SparseOps.get(graph, targetId, entity);
      const mr = Math.min(R_ab, R_ba);
      totalMR += mr;
    }
    
    if (totalMR === 0) return 0;
    
    // Get MR with specific entity
    const R_ab = outgoing.get(otherId) ?? 0;
    const R_ba = SparseOps.get(graph, otherId, entity);
    const mr = Math.min(R_ab, R_ba);
    
    return mr / totalMR;
  };

/**
 * Get all entities with non-zero mutual recognition
 * O(k) where k = entity's connections
 */
export const getMutualRecognitionPairs = (graph: SparseRecognitionGraph) =>
  (entity: EntityId): Array<[EntityId, number]> => {
    const outgoing = SparseOps.outgoing(graph, entity);
    const pairs: Array<[EntityId, number]> = [];
    
    for (const [otherId, R_ab] of outgoing) {
      const R_ba = SparseOps.get(graph, otherId, entity);
      const mr = Math.min(R_ab, R_ba);
      
      if (mr > 0) {
        pairs.push([otherId, mr]);
      }
    }
    
    return pairs;
  };

/**
 * Check budget constraint for an entity
 * Returns true if total allocations <= 1.0
 */
export const checkBudgetConstraint = (graph: SparseRecognitionGraph) =>
  (entity: EntityId): { valid: boolean; total: number } => {
    const outgoing = SparseOps.outgoing(graph, entity);
    let total = 0;
    
    for (const amount of outgoing.values()) {
      total += amount;
    }
    
    return {
      valid: total <= 1.0,
      total
    };
  };

/**
 * Get entities that violate budget constraint
 * Useful for validation
 */
export const findBudgetViolations = (
  graph: SparseRecognitionGraph
): Array<{ entity: EntityId; total: number }> => {
  const violations: Array<{ entity: EntityId; total: number }> = [];
  
  for (const entity of SparseOps.entities(graph)) {
    const { valid, total } = checkBudgetConstraint(graph)(entity);
    if (!valid) {
      violations.push({ entity, total });
    }
  }
  
  return violations;
};

/**
 * Batch compute MRS for multiple entities
 * More efficient than calling sparseMRS multiple times
 */
export const batchSparseMRS = (graph: SparseRecognitionGraph) =>
  (entities: EntityId[]) => (universe: Set<EntityId>): Map<EntityId, Distribution> => {
    const results = new Map<EntityId, Distribution>();
    
    for (const entity of entities) {
      const mrs = sparseMRS(graph)(entity)(universe);
      results.set(entity, mrs);
    }
    
    return results;
  };

/**
 * Compute recognition matrix statistics
 * Useful for monitoring and debugging
 */
export const computeStatistics = (
  graph: SparseRecognitionGraph
): {
  entities: number;
  edges: number;
  avgDegree: number;
  sparsity: number;
  maxOutDegree: number;
  maxInDegree: number;
  budgetViolations: number;
  symmetricEdges: number;
} => {
  const allEntities = SparseOps.entities(graph);
  const entities = allEntities.size;
  const edges = SparseOps.edgeCount(graph);
  
  let maxOutDegree = 0;
  let maxInDegree = 0;
  let symmetricEdges = 0;
  
  // Compute max out-degree
  for (const entity of allEntities) {
    const outDegree = SparseOps.outgoing(graph, entity).size;
    maxOutDegree = Math.max(maxOutDegree, outDegree);
    
    const inDegree = SparseOps.incoming(graph, entity).size;
    maxInDegree = Math.max(maxInDegree, inDegree);
  }
  
  // Count symmetric edges (where both R_ab and R_ba exist)
  for (const [from, to, _] of SparseOps.edges(graph)) {
    if (SparseOps.get(graph, to, from) > 0) {
      symmetricEdges++;
    }
  }
  
  const avgDegree = entities > 0 ? edges / entities : 0;
  const possibleEdges = entities * entities;
  const sparsity = possibleEdges > 0 ? 1 - (edges / possibleEdges) : 1;
  const budgetViolations = findBudgetViolations(graph).length;
  
  return {
    entities,
    edges,
    avgDegree,
    sparsity,
    maxOutDegree,
    maxInDegree,
    budgetViolations,
    symmetricEdges: symmetricEdges / 2 // Divide by 2 since we count each pair twice
  };
};

