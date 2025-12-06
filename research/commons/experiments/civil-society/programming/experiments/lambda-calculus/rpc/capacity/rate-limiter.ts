/**
 * Recognition-Based Rate Limiter
 * 
 * Limits compute operations based on mutual recognition.
 * Higher mutual recognition = more compute capacity.
 */

import type { CapacityQuota, AllocationStrategy, ResourceUsage, RateLimitViolation } from './types';

/**
 * Compute rate limiter
 * Enforces recognition-based limits on RPC calls
 */
export class ComputeRateLimiter {
  private baseQuota: CapacityQuota;
  private strategy: AllocationStrategy;
  
  // Track usage per entity
  private usage = new Map<string, {
    window: number[];        // Timestamps of operations in current window
    totalOps: number;
    totalTimeMs: number;
  }>();
  
  private windowSize = 1000;  // 1 second window

  constructor(
    baseQuota: CapacityQuota,
    strategy: AllocationStrategy = 'proportional'
  ) {
    this.baseQuota = baseQuota;
    this.strategy = strategy;
  }

  /**
   * Calculate capacity quota for an entity based on mutual recognition
   * 
   * @param mutualRecognition - MR value between 0 and 1
   * @returns Quota allocated to this entity
   */
  calculateQuota(mutualRecognition: number): CapacityQuota {
    let factor: number;
    
    switch (this.strategy) {
      case 'proportional':
        // Linear: quota = base * MR
        factor = mutualRecognition;
        break;
        
      case 'quadratic':
        // Quadratic: quota = base * MR²
        // Rewards higher recognition exponentially
        factor = mutualRecognition * mutualRecognition;
        break;
        
      case 'threshold':
        // Step function: 0 below 0.5, full above
        factor = mutualRecognition >= 0.5 ? 1.0 : 0.0;
        break;
        
      case 'progressive':
        // Progressive with diminishing returns: f(x) = x * (2 - x)
        // Smooth curve that rewards recognition but with diminishing returns
        factor = mutualRecognition * (2 - mutualRecognition);
        break;
        
      default:
        factor = mutualRecognition;
    }
    
    return {
      computeOpsPerSecond: this.baseQuota.computeOpsPerSecond * factor,
      storageBytes: this.baseQuota.storageBytes * factor,
      bandwidthBytesPerSecond: this.baseQuota.bandwidthBytesPerSecond * factor,
      recognitionBasis: mutualRecognition
    };
  }

  /**
   * Check if an entity can perform an operation
   * 
   * @param entityId - Remote entity making the request
   * @param mutualRecognition - MR with this entity
   * @param estimatedTimeMs - Estimated computation time
   * @returns true if allowed, false if rate limited
   */
  async checkComputeLimit(
    entityId: string,
    mutualRecognition: number,
    estimatedTimeMs: number = 100
  ): Promise<{ allowed: boolean; quota: CapacityQuota; violation?: RateLimitViolation }> {
    const quota = this.calculateQuota(mutualRecognition);
    const now = Date.now();
    
    // Get or create usage tracking
    let entityUsage = this.usage.get(entityId);
    if (!entityUsage) {
      entityUsage = {
        window: [],
        totalOps: 0,
        totalTimeMs: 0
      };
      this.usage.set(entityId, entityUsage);
    }
    
    // Clean up old operations outside window
    const windowStart = now - this.windowSize;
    entityUsage.window = entityUsage.window.filter(ts => ts > windowStart);
    
    // Check if this operation would exceed quota
    const currentOps = entityUsage.window.length;
    
    if (currentOps >= quota.computeOpsPerSecond) {
      // Rate limited!
      const violation: RateLimitViolation = {
        entityId,
        resourceType: 'compute',
        requested: currentOps + 1,
        available: quota.computeOpsPerSecond,
        quota: quota.computeOpsPerSecond,
        timestamp: now
      };
      
      return { allowed: false, quota, violation };
    }
    
    // Allowed - record operation
    entityUsage.window.push(now);
    entityUsage.totalOps++;
    entityUsage.totalTimeMs += estimatedTimeMs;
    
    return { allowed: true, quota };
  }

  /**
   * Get current usage for an entity
   */
  getUsage(entityId: string): ResourceUsage | null {
    const usage = this.usage.get(entityId);
    if (!usage) return null;
    
    const now = Date.now();
    const windowStart = now - this.windowSize;
    const recentOps = usage.window.filter(ts => ts > windowStart);
    
    return {
      entityId,
      timestamp: now,
      computeOps: recentOps.length,
      computeTimeMs: usage.totalTimeMs,
      storageBytesUsed: 0,
      storageItemsCount: 0,
      bandwidthBytesIn: 0,
      bandwidthBytesOut: 0
    };
  }

  /**
   * Get statistics for all entities
   */
  getStats(): Map<string, { ops: number; totalOps: number; totalTime: number }> {
    const stats = new Map();
    const now = Date.now();
    const windowStart = now - this.windowSize;
    
    for (const [entityId, usage] of this.usage) {
      const recentOps = usage.window.filter(ts => ts > windowStart);
      stats.set(entityId, {
        ops: recentOps.length,
        totalOps: usage.totalOps,
        totalTime: usage.totalTimeMs
      });
    }
    
    return stats;
  }

  /**
   * Cleanup old usage data
   */
  cleanup(maxAge: number = 60000): void {
    const cutoff = Date.now() - maxAge;
    
    for (const [entityId, usage] of this.usage) {
      // Remove if no recent operations
      if (usage.window.length === 0 || usage.window[usage.window.length - 1] < cutoff) {
        this.usage.delete(entityId);
      }
    }
  }
}

