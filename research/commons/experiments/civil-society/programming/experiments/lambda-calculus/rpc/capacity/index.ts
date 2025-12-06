/**
 * Recognition-Based Capacity Allocation
 * 
 * Resources are allocated proportionally to mutual recognition.
 * Implements rate limiting for compute, storage, and bandwidth.
 */

export type {
  CapacityQuota,
  AllocationStrategy,
  ResourceUsage,
  RateLimitViolation,
  ReplicationPolicy,
  ReplicationState
} from './types';

export { ComputeRateLimiter } from './rate-limiter';
export { StorageQuotaManager } from './storage-quota';
export { BandwidthThrottle } from './bandwidth-throttle';

