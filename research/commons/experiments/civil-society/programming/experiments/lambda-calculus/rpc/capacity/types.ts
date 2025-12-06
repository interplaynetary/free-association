/**
 * Recognition-Based Capacity Allocation Types
 * 
 * Resources (compute, storage, bandwidth) are allocated proportionally
 * to mutual recognition between entities. Higher recognition = more capacity.
 */

export interface CapacityQuota {
  // Maximum operations per time window
  computeOpsPerSecond: number;
  
  // Maximum storage for replicated data (bytes)
  storageBytes: number;
  
  // Maximum bandwidth per time window (bytes/sec)
  bandwidthBytesPerSecond: number;
  
  // Derived from mutual recognition
  recognitionBasis: number;
}

/**
 * Capacity allocation strategy
 */
export type AllocationStrategy = 
  | 'proportional'      // Linear: capacity ∝ MR
  | 'quadratic'         // Quadratic: capacity ∝ MR²
  | 'threshold'         // Step function: 0 below threshold, full above
  | 'progressive';      // Progressive: f(MR) with diminishing returns

/**
 * Resource consumption record
 */
export interface ResourceUsage {
  entityId: string;
  timestamp: number;
  
  // Compute
  computeOps: number;
  computeTimeMs: number;
  
  // Storage
  storageBytesUsed: number;
  storageItemsCount: number;
  
  // Bandwidth
  bandwidthBytesIn: number;
  bandwidthBytesOut: number;
}

/**
 * Rate limit violation
 */
export interface RateLimitViolation {
  entityId: string;
  resourceType: 'compute' | 'storage' | 'bandwidth';
  requested: number;
  available: number;
  quota: number;
  timestamp: number;
}

/**
 * Replication policy
 */
export interface ReplicationPolicy {
  // Which entities to replicate from
  replicateFrom: Set<string>;
  
  // Selective replication based on MRS
  minMRS: number;  // Only replicate entities with MRS >= this
  
  // Max replicated entities
  maxReplicas: number;
  
  // Strategy for selecting what to replicate
  strategy: 'highest-mrs' | 'highest-mrd' | 'all-above-threshold';
}

/**
 * Replication state
 */
export interface ReplicationState {
  entityId: string;
  
  // What we're replicating
  replicatedFrom: Map<string, {
    lastSyncTime: number;
    bytesReplicated: number;
    edgeCount: number;
  }>;
  
  // What's being replicated from us
  replicatedBy: Set<string>;
  
  // Total storage used for replication
  totalReplicationBytes: number;
}

