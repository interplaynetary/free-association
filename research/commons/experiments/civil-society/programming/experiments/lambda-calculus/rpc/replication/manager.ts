/**
 * Recognition-Based Replication Manager
 * 
 * Selective replication based on MRS/MRD:
 * - Entities with higher MRS get priority for replication
 * - Storage quota limits how much can be replicated
 * - CRDT-style conflict resolution with vector clocks
 */

import type { SparseRecognitionGraph, EntityId } from '../../src/sparse/types';
import { SparseOps } from '../../src/sparse/types';
import type { ReplicationPolicy, ReplicationState } from '../capacity/types';
import type { Distribution } from '../../src/sparse/operations';
import type { VectorClock } from '../types';

/**
 * Replication strategy
 */
export type ReplicationStrategy = 
  | 'full'           // Replicate everything from selected entities
  | 'partial'        // Replicate only high-recognition edges
  | 'selective';     // Replicate based on MRS threshold

/**
 * Replication manager
 */
export class ReplicationManager {
  private entityId: string;
  private state: ReplicationState;
  private policy: ReplicationPolicy;
  private strategy: ReplicationStrategy;
  
  // Replicated graphs from other entities
  private replicas = new Map<EntityId, {
    graph: SparseRecognitionGraph;
    vectorClock: VectorClock;
    lastSync: number;
    bytesUsed: number;
  }>();

  constructor(
    entityId: string,
    policy: ReplicationPolicy,
    strategy: ReplicationStrategy = 'selective'
  ) {
    this.entityId = entityId;
    this.policy = policy;
    this.strategy = strategy;
    
    this.state = {
      entityId,
      replicatedFrom: new Map(),
      replicatedBy: new Set(),
      totalReplicationBytes: 0
    };
  }

  /**
   * Decide what to replicate based on MRS
   * 
   * @param mrs - Mutual Recognition Share distribution
   * @param availableStorage - Storage quota available (bytes)
   * @returns Set of entity IDs to replicate from
   */
  selectReplicationTargets(
    mrs: Distribution,
    availableStorage: number
  ): Set<EntityId> {
    const targets = new Set<EntityId>();
    
    // Sort by MRS value (highest first)
    const sorted = Object.entries(mrs.distribution)
      .sort(([, a], [, b]) => b - a);
    
    let estimatedBytes = 0;
    const avgBytesPerEntity = 50 * 40; // Assume avg 50 edges × 40 bytes per edge
    
    for (const [entityId, mrsValue] of sorted) {
      // Check MRS threshold
      if (mrsValue < this.policy.minMRS) break;
      
      // Check storage limit
      if (estimatedBytes + avgBytesPerEntity > availableStorage) break;
      
      // Check max replicas
      if (targets.size >= this.policy.maxReplicas) break;
      
      targets.add(entityId);
      estimatedBytes += avgBytesPerEntity;
    }
    
    return targets;
  }

  /**
   * Replicate graph from remote entity
   * 
   * @param fromEntityId - Entity to replicate from
   * @param graph - Their sparse recognition graph
   * @param vectorClock - Their vector clock
   * @param storageQuota - Max storage we can use
   * @returns Bytes used for this replica
   */
  async replicateFrom(
    fromEntityId: EntityId,
    graph: SparseRecognitionGraph,
    vectorClock: VectorClock,
    storageQuota: number
  ): Promise<number> {
    // Check if we're already replicating
    const existing = this.replicas.get(fromEntityId);
    
    // Filter graph based on strategy
    const filtered = this.filterGraph(graph);
    
    // Estimate size
    const bytesUsed = this.estimateGraphSize(filtered);
    
    // Check storage quota
    const currentTotal = this.state.totalReplicationBytes;
    if (currentTotal + bytesUsed > storageQuota) {
      throw new Error(`Storage quota exceeded: ${currentTotal + bytesUsed} > ${storageQuota}`);
    }
    
    // Store replica
    this.replicas.set(fromEntityId, {
      graph: filtered,
      vectorClock: { ...vectorClock },
      lastSync: Date.now(),
      bytesUsed
    });
    
    // Update state
    if (existing) {
      // Replace existing - adjust byte count
      this.state.totalReplicationBytes -= existing.bytesUsed;
    }
    
    this.state.totalReplicationBytes += bytesUsed;
    this.state.replicatedFrom.set(fromEntityId, {
      lastSyncTime: Date.now(),
      bytesReplicated: bytesUsed,
      edgeCount: SparseOps.edgeCount(filtered)
    });
    
    return bytesUsed;
  }

  /**
   * Filter graph based on replication strategy
   */
  private filterGraph(graph: SparseRecognitionGraph): SparseRecognitionGraph {
    switch (this.strategy) {
      case 'full':
        // Replicate everything
        return graph;
        
      case 'partial':
        // Only replicate edges above threshold (e.g., > 0.3)
        const threshold = 0.3;
        const filteredEdges = new Map<EntityId, Map<EntityId, number>>();
        
        for (const [from, targets] of graph.edges) {
          const filteredTargets = new Map<EntityId, number>();
          
          for (const [to, amount] of targets) {
            if (amount >= threshold) {
              filteredTargets.set(to, amount);
            }
          }
          
          if (filteredTargets.size > 0) {
            filteredEdges.set(from, filteredTargets);
          }
        }
        
        return {
          edges: filteredEdges,
          metadata: SparseOps.computeMetadata({ edges: filteredEdges })
        };
        
      case 'selective':
        // Replicate based on policy
        // For now, same as partial
        return this.filterGraph(graph); // Reuse partial logic
        
      default:
        return graph;
    }
  }

  /**
   * Estimate graph size in bytes
   */
  private estimateGraphSize(graph: SparseRecognitionGraph): number {
    const edgeCount = SparseOps.edgeCount(graph);
    // Each edge: ~40 bytes (2 entity IDs + amount + overhead)
    return edgeCount * 40;
  }

  /**
   * Merge replicated graphs into a unified view
   * Useful for querying across multiple replicas
   */
  getMergedGraph(): SparseRecognitionGraph {
    const merged = new Map<EntityId, Map<EntityId, number>>();
    
    // Merge all replicas
    for (const replica of this.replicas.values()) {
      for (const [from, targets] of replica.graph.edges) {
        if (!merged.has(from)) {
          merged.set(from, new Map());
        }
        
        const mergedTargets = merged.get(from)!;
        for (const [to, amount] of targets) {
          // Take max value if conflict (or could use vector clock)
          const existing = mergedTargets.get(to) ?? 0;
          mergedTargets.set(to, Math.max(existing, amount));
        }
      }
    }
    
    return {
      edges: merged,
      metadata: SparseOps.computeMetadata({ edges: merged })
    };
  }

  /**
   * Get replicated graph from specific entity
   */
  getReplicaFrom(entityId: EntityId): SparseRecognitionGraph | null {
    return this.replicas.get(entityId)?.graph ?? null;
  }

  /**
   * Remove replica
   */
  removeReplica(entityId: EntityId): void {
    const replica = this.replicas.get(entityId);
    if (replica) {
      this.state.totalReplicationBytes -= replica.bytesUsed;
      this.state.replicatedFrom.delete(entityId);
      this.replicas.delete(entityId);
    }
  }

  /**
   * Update replication policy
   */
  updatePolicy(policy: Partial<ReplicationPolicy>): void {
    this.policy = { ...this.policy, ...policy };
  }

  /**
   * Get replication state
   */
  getState(): ReplicationState {
    return { ...this.state };
  }

  /**
   * Get replication statistics
   */
  getStats(): {
    replicas: number;
    totalBytes: number;
    avgBytesPerReplica: number;
    totalEdges: number;
    avgEdgesPerReplica: number;
  } {
    let totalEdges = 0;
    
    for (const replica of this.replicas.values()) {
      totalEdges += SparseOps.edgeCount(replica.graph);
    }
    
    return {
      replicas: this.replicas.size,
      totalBytes: this.state.totalReplicationBytes,
      avgBytesPerReplica: this.replicas.size > 0 
        ? this.state.totalReplicationBytes / this.replicas.size 
        : 0,
      totalEdges,
      avgEdgesPerReplica: this.replicas.size > 0
        ? totalEdges / this.replicas.size
        : 0
    };
  }
}

