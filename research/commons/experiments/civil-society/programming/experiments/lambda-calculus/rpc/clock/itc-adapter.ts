/**
 * Interval Tree Clock Adapter for RPC System
 * 
 * Replaces simple vector clocks with ITC for better decentralization:
 * - No need to track all participants
 * - Dynamic fork/join without global coordination
 * - Space-efficient: O(log n) instead of O(n)
 * - Perfect for P2P networks where entities come and go
 */

import { seed, event, fork, join, peek, leq, equals, concurrent, clone, type Stamp } from '../../itc';

/**
 * ITC Clock wrapper for RPC use
 * 
 * Provides convenient API for:
 * - Local event incrementing
 * - Forking when creating peer connections
 * - Merging when receiving updates
 * - Causality comparison
 */
export class ITClock {
  private stamp: Stamp;

  constructor(stamp?: Stamp) {
    this.stamp = stamp || seed();
  }

  // ============================================================================
  // Core Operations
  // ============================================================================

  /**
   * Increment clock on local event
   * Call this when making local changes
   */
  increment(): void {
    this.stamp = event(this.stamp);
  }

  /**
   * Fork clock when creating new peer connection
   * Returns new clock for the peer while keeping one for self
   * 
   * This is key for decentralization - no global coordination needed!
   */
  fork(): ITClock {
    const [s1, s2] = fork(this.stamp);
    this.stamp = s1;  // Keep s1 for self
    return new ITClock(s2);  // Give s2 to peer
  }

  /**
   * Merge with remote clock when receiving update
   * Preserves causality information
   */
  merge(other: Stamp): void {
    this.stamp = join(this.stamp, other);
  }

  /**
   * Create peek stamp for sending in messages
   * Null id but same event - peer can't modify our id
   */
  peek(): Stamp {
    return peek(this.stamp);
  }

  // ============================================================================
  // Causality Comparison
  // ============================================================================

  /**
   * Check if this clock happened before another
   * Useful for determining update order
   */
  happensBefore(other: Stamp): boolean {
    return leq(this.stamp, other) && !leq(other, this.stamp);
  }

  /**
   * Check if this clock happened after another
   */
  happensAfter(other: Stamp): boolean {
    return leq(other, this.stamp) && !leq(this.stamp, other);
  }

  /**
   * Check if two clocks are equal (same causality)
   */
  equals(other: Stamp): boolean {
    return equals(this.stamp, other);
  }

  /**
   * Check if two clocks are concurrent (no causal relationship)
   */
  isConcurrent(other: Stamp): boolean {
    return concurrent(this.stamp, other);
  }

  /**
   * General comparison - returns causal relationship
   */
  compare(other: Stamp): 'before' | 'after' | 'equal' | 'concurrent' {
    if (this.equals(other)) return 'equal';
    if (this.happensBefore(other)) return 'before';
    if (this.happensAfter(other)) return 'after';
    return 'concurrent';
  }

  // ============================================================================
  // Serialization
  // ============================================================================

  /**
   * Get underlying stamp for serialization
   */
  serialize(): Stamp {
    return this.stamp;
  }

  /**
   * Create from serialized stamp
   */
  static deserialize(stamp: Stamp): ITClock {
    return new ITClock(stamp);
  }

  /**
   * Clone this clock
   */
  clone(): ITClock {
    return new ITClock(clone(this.stamp));
  }

  // ============================================================================
  // Utility
  // ============================================================================

  /**
   * Get string representation for debugging
   */
  toString(): string {
    return `ITClock(${JSON.stringify(this.stamp)})`;
  }

  /**
   * Create a seed clock (for new entities)
   */
  static seed(): ITClock {
    return new ITClock(seed());
  }
}

/**
 * Resolve conflict using ITC causality
 * 
 * @returns Which update to apply
 */
export function resolveITCConflict(
  localStamp: Stamp,
  remoteStamp: Stamp,
  localValue: number,
  remoteValue: number
): {
  resolution: 'local' | 'remote' | 'merge';
  value: number;
} {
  const clock = new ITClock(localStamp);
  const relationship = clock.compare(remoteStamp);

  switch (relationship) {
    case 'before':
      // Remote is newer - use remote
      return { resolution: 'remote', value: remoteValue };

    case 'after':
      // Local is newer - use local
      return { resolution: 'local', value: localValue };

    case 'equal':
      // Same causality - should be same value
      return { resolution: 'local', value: localValue };

    case 'concurrent':
      // Concurrent updates - merge (take max for recognition)
      return { 
        resolution: 'merge', 
        value: Math.max(localValue, remoteValue) 
      };
  }
}

/**
 * Batch resolve conflicts for multiple edges
 */
export function batchResolveConflicts(
  localStamp: Stamp,
  remoteStamp: Stamp,
  localEdges: Map<string, Map<string, number>>,
  remoteEdges: Map<string, Map<string, number>>
): Map<string, Map<string, number>> {
  const merged = new Map<string, Map<string, number>>();

  // Get all entity pairs
  const allPairs = new Set<string>();
  
  for (const [from, targets] of localEdges) {
    for (const to of targets.keys()) {
      allPairs.add(`${from}:${to}`);
    }
  }
  
  for (const [from, targets] of remoteEdges) {
    for (const to of targets.keys()) {
      allPairs.add(`${from}:${to}`);
    }
  }

  // Resolve each edge
  for (const pair of allPairs) {
    const [from, to] = pair.split(':');
    
    const localValue = localEdges.get(from)?.get(to) ?? 0;
    const remoteValue = remoteEdges.get(from)?.get(to) ?? 0;

    const { value } = resolveITCConflict(
      localStamp,
      remoteStamp,
      localValue,
      remoteValue
    );

    if (value > 0) {
      if (!merged.has(from)) {
        merged.set(from, new Map());
      }
      merged.get(from)!.set(to, value);
    }
  }

  return merged;
}

