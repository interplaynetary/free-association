/**
 * Recognition-Based Bandwidth Throttle
 * 
 * Limits network bandwidth based on mutual recognition.
 * Implements token bucket algorithm for smooth rate limiting.
 */

import type { CapacityQuota, AllocationStrategy, RateLimitViolation } from './types';

/**
 * Token bucket for bandwidth throttling
 */
class TokenBucket {
  private tokens: number;
  private lastRefill: number;
  
  constructor(
    private capacity: number,     // Max tokens (bytes)
    private refillRate: number    // Tokens per second
  ) {
    this.tokens = capacity;
    this.lastRefill = Date.now();
  }

  /**
   * Refill tokens based on elapsed time
   */
  private refill(): void {
    const now = Date.now();
    const elapsed = (now - this.lastRefill) / 1000; // seconds
    
    const tokensToAdd = elapsed * this.refillRate;
    this.tokens = Math.min(this.capacity, this.tokens + tokensToAdd);
    this.lastRefill = now;
  }

  /**
   * Try to consume tokens
   * @returns true if tokens available, false otherwise
   */
  tryConsume(amount: number): boolean {
    this.refill();
    
    if (this.tokens >= amount) {
      this.tokens -= amount;
      return true;
    }
    
    return false;
  }

  /**
   * Get available tokens
   */
  getAvailable(): number {
    this.refill();
    return this.tokens;
  }

  /**
   * Update capacity and refill rate
   */
  updateLimits(capacity: number, refillRate: number): void {
    this.capacity = capacity;
    this.refillRate = refillRate;
    this.tokens = Math.min(this.tokens, capacity);
  }
}

/**
 * Bandwidth throttle manager
 */
export class BandwidthThrottle {
  private baseQuota: CapacityQuota;
  private strategy: AllocationStrategy;
  
  // Token buckets per entity
  private buckets = new Map<string, {
    incoming: TokenBucket;
    outgoing: TokenBucket;
  }>();
  
  // Track total bandwidth usage
  private totalBandwidth = {
    in: 0,
    out: 0
  };

  constructor(
    baseQuota: CapacityQuota,
    strategy: AllocationStrategy = 'proportional'
  ) {
    this.baseQuota = baseQuota;
    this.strategy = strategy;
  }

  /**
   * Calculate bandwidth quota based on mutual recognition
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
   * Get or create token buckets for an entity
   */
  private getBuckets(entityId: string, quota: CapacityQuota): {
    incoming: TokenBucket;
    outgoing: TokenBucket;
  } {
    let buckets = this.buckets.get(entityId);
    
    if (!buckets) {
      buckets = {
        incoming: new TokenBucket(
          quota.bandwidthBytesPerSecond,
          quota.bandwidthBytesPerSecond
        ),
        outgoing: new TokenBucket(
          quota.bandwidthBytesPerSecond,
          quota.bandwidthBytesPerSecond
        )
      };
      this.buckets.set(entityId, buckets);
    } else {
      // Update limits if recognition changed
      buckets.incoming.updateLimits(
        quota.bandwidthBytesPerSecond,
        quota.bandwidthBytesPerSecond
      );
      buckets.outgoing.updateLimits(
        quota.bandwidthBytesPerSecond,
        quota.bandwidthBytesPerSecond
      );
    }
    
    return buckets;
  }

  /**
   * Check if incoming data can be received
   */
  async checkIncomingLimit(
    entityId: string,
    mutualRecognition: number,
    bytes: number
  ): Promise<{ allowed: boolean; quota: CapacityQuota; violation?: RateLimitViolation }> {
    const quota = this.calculateQuota(mutualRecognition);
    const buckets = this.getBuckets(entityId, quota);
    
    const allowed = buckets.incoming.tryConsume(bytes);
    
    if (!allowed) {
      const violation: RateLimitViolation = {
        entityId,
        resourceType: 'bandwidth',
        requested: bytes,
        available: buckets.incoming.getAvailable(),
        quota: quota.bandwidthBytesPerSecond,
        timestamp: Date.now()
      };
      
      return { allowed: false, quota, violation };
    }
    
    this.totalBandwidth.in += bytes;
    return { allowed: true, quota };
  }

  /**
   * Check if outgoing data can be sent
   */
  async checkOutgoingLimit(
    entityId: string,
    mutualRecognition: number,
    bytes: number
  ): Promise<{ allowed: boolean; quota: CapacityQuota; violation?: RateLimitViolation }> {
    const quota = this.calculateQuota(mutualRecognition);
    const buckets = this.getBuckets(entityId, quota);
    
    const allowed = buckets.outgoing.tryConsume(bytes);
    
    if (!allowed) {
      const violation: RateLimitViolation = {
        entityId,
        resourceType: 'bandwidth',
        requested: bytes,
        available: buckets.outgoing.getAvailable(),
        quota: quota.bandwidthBytesPerSecond,
        timestamp: Date.now()
      };
      
      return { allowed: false, quota, violation };
    }
    
    this.totalBandwidth.out += bytes;
    return { allowed: true, quota };
  }

  /**
   * Get current bandwidth availability
   */
  getAvailableBandwidth(entityId: string): {
    incoming: number;
    outgoing: number;
  } | null {
    const buckets = this.buckets.get(entityId);
    if (!buckets) return null;
    
    return {
      incoming: buckets.incoming.getAvailable(),
      outgoing: buckets.outgoing.getAvailable()
    };
  }

  /**
   * Get total bandwidth usage
   */
  getTotalBandwidth(): { in: number; out: number } {
    return { ...this.totalBandwidth };
  }

  /**
   * Get bandwidth stats for all entities
   */
  getStats(): Map<string, { inAvailable: number; outAvailable: number }> {
    const stats = new Map();
    
    for (const [entityId, buckets] of this.buckets) {
      stats.set(entityId, {
        inAvailable: buckets.incoming.getAvailable(),
        outAvailable: buckets.outgoing.getAvailable()
      });
    }
    
    return stats;
  }
}

