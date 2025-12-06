/**
 * Recognition-Based Storage Quota
 * 
 * Limits storage used for replication based on mutual recognition.
 * Entities with higher MR can replicate more data.
 */

import type { CapacityQuota, AllocationStrategy, RateLimitViolation } from './types';

/**
 * Storage quota manager
 */
export class StorageQuotaManager {
  private baseQuota: CapacityQuota;
  private strategy: AllocationStrategy;
  
  // Track storage usage per entity
  private usage = new Map<string, {
    bytesUsed: number;
    itemCount: number;
    lastUpdated: number;
  }>();

  constructor(
    baseQuota: CapacityQuota,
    strategy: AllocationStrategy = 'proportional'
  ) {
    this.baseQuota = baseQuota;
    this.strategy = strategy;
  }

  /**
   * Calculate storage quota based on mutual recognition
   * Same algorithm as rate limiter for consistency
   */
  calculateQuota(mutualRecognition: number): CapacityQuota {
    let factor: number;
    
    switch (this.strategy) {
      case 'proportional':
        factor = mutualRecognition;
        break;
      case 'quadratic':
        factor = mutualRecognition * mutualRecognition;
        break;
      case 'threshold':
        factor = mutualRecognition >= 0.5 ? 1.0 : 0.0;
        break;
      case 'progressive':
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
   * Check if entity can store more data
   */
  async checkStorageLimit(
    entityId: string,
    mutualRecognition: number,
    additionalBytes: number
  ): Promise<{ allowed: boolean; quota: CapacityQuota; violation?: RateLimitViolation }> {
    const quota = this.calculateQuota(mutualRecognition);
    
    let usage = this.usage.get(entityId);
    if (!usage) {
      usage = {
        bytesUsed: 0,
        itemCount: 0,
        lastUpdated: Date.now()
      };
      this.usage.set(entityId, usage);
    }
    
    const newTotal = usage.bytesUsed + additionalBytes;
    
    if (newTotal > quota.storageBytes) {
      // Storage quota exceeded
      const violation: RateLimitViolation = {
        entityId,
        resourceType: 'storage',
        requested: newTotal,
        available: quota.storageBytes - usage.bytesUsed,
        quota: quota.storageBytes,
        timestamp: Date.now()
      };
      
      return { allowed: false, quota, violation };
    }
    
    return { allowed: true, quota };
  }

  /**
   * Record storage usage
   */
  recordStorage(entityId: string, bytes: number, items: number = 1): void {
    let usage = this.usage.get(entityId);
    if (!usage) {
      usage = {
        bytesUsed: 0,
        itemCount: 0,
        lastUpdated: Date.now()
      };
      this.usage.set(entityId, usage);
    }
    
    usage.bytesUsed += bytes;
    usage.itemCount += items;
    usage.lastUpdated = Date.now();
  }

  /**
   * Free storage for an entity
   */
  freeStorage(entityId: string, bytes: number, items: number = 1): void {
    const usage = this.usage.get(entityId);
    if (usage) {
      usage.bytesUsed = Math.max(0, usage.bytesUsed - bytes);
      usage.itemCount = Math.max(0, usage.itemCount - items);
      usage.lastUpdated = Date.now();
    }
  }

  /**
   * Get current storage usage
   */
  getUsage(entityId: string): { bytes: number; items: number; quota: number } | null {
    const usage = this.usage.get(entityId);
    if (!usage) return null;
    
    return {
      bytes: usage.bytesUsed,
      items: usage.itemCount,
      quota: this.baseQuota.storageBytes
    };
  }

  /**
   * Get total storage used across all entities
   */
  getTotalUsage(): { bytes: number; items: number; entities: number } {
    let totalBytes = 0;
    let totalItems = 0;
    
    for (const usage of this.usage.values()) {
      totalBytes += usage.bytesUsed;
      totalItems += usage.itemCount;
    }
    
    return {
      bytes: totalBytes,
      items: totalItems,
      entities: this.usage.size
    };
  }

  /**
   * Calculate how much storage is allocated but not used
   */
  getFragmentation(): number {
    const total = this.getTotalUsage();
    const maxPossible = this.baseQuota.storageBytes * this.usage.size;
    
    if (maxPossible === 0) return 0;
    return 1 - (total.bytes / maxPossible);
  }
}

