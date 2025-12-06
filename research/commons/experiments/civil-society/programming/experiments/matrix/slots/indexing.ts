/**
 * Space-Time Indexing for O(k) Recipient Lookups
 * 
 * Instead of scanning all N participants, use indexes to find only the k
 * participants who might match (by type, location, time).
 */

import type { NeedSlot, AvailabilitySlot, Location } from './schemas.js';

/**
 * Space-Time Index
 * 
 * Provides O(k) lookups instead of O(N) scans where k << N.
 */
export class SpaceTimeIndex {
  private byType: Map<string, Set<string>> = new Map();
  private byLocation: Map<string, Set<string>> = new Map();
  private byTime: Map<string, Set<string>> = new Map();
  
  /**
   * Index a slot
   */
  addSlot(slot: NeedSlot | AvailabilitySlot): void {
    const participantId = slot.participantId;
    
    // Index by type
    if (!this.byType.has(slot.need_type_id)) {
      this.byType.set(slot.need_type_id, new Set());
    }
    this.byType.get(slot.need_type_id)!.add(participantId);
    
    // Index by location bucket
    if (slot.location) {
      const locBucket = this.getLocationBucket(slot.location);
      if (!this.byLocation.has(locBucket)) {
        this.byLocation.set(locBucket, new Set());
      }
      this.byLocation.get(locBucket)!.add(participantId);
    }
    
    // Index by time bucket
    const timeBucket = this.getTimeBucket(slot);
    if (!this.byTime.has(timeBucket)) {
      this.byTime.set(timeBucket, new Set());
    }
    this.byTime.get(timeBucket)!.add(participantId);
  }
  
  /**
   * Find participants matching a need
   */
  findMatching(need: NeedSlot): Set<string> {
    // Get candidates by type (most restrictive filter)
    const typeMatches = this.byType.get(need.need_type_id);
    if (!typeMatches || typeMatches.size === 0) {
      return new Set();
    }
    
    // Further filter by location if specified
    if (need.location) {
      const locBucket = this.getLocationBucket(need.location);
      const locMatches = this.byLocation.get(locBucket);
      if (locMatches) {
        // Intersect with type matches
        return new Set([...typeMatches].filter(id => locMatches.has(id)));
      }
    }
    
    return typeMatches;
  }
  
  /**
   * Get location bucket for indexing
   */
  private getLocationBucket(loc: Location): string {
    if (loc.type === 'online') return 'online';
    if (loc.city) return `city:${loc.city}`;
    if (loc.country) return `country:${loc.country}`;
    if (loc.latitude && loc.longitude) {
      // Grid-based bucketing (10km grid)
      const latBucket = Math.floor(loc.latitude / 0.1);
      const lonBucket = Math.floor(loc.longitude / 0.1);
      return `grid:${latBucket},${lonBucket}`;
    }
    return 'unknown';
  }
  
  /**
   * Get time bucket for indexing
   */
  private getTimeBucket(slot: NeedSlot | AvailabilitySlot): string {
    if (slot.recurrence) {
      return `recur:${slot.recurrence}`;
    }
    if (slot.start_date) {
      // Weekly bucket
      const date = new Date(slot.start_date);
      const weekNum = Math.floor(date.getTime() / (7 * 24 * 60 * 60 * 1000));
      return `week:${weekNum}`;
    }
    return 'anytime';
  }
  
  /**
   * Clear index
   */
  clear(): void {
    this.byType.clear();
    this.byLocation.clear();
    this.byTime.clear();
  }
  
  /**
   * Get statistics
   */
  getStats(): {
    totalTypes: number;
    totalLocations: number;
    totalTimes: number;
    avgParticipantsPerType: number;
  } {
    let totalParticipants = 0;
    for (const set of this.byType.values()) {
      totalParticipants += set.size;
    }
    
    return {
      totalTypes: this.byType.size,
      totalLocations: this.byLocation.size,
      totalTimes: this.byTime.size,
      avgParticipantsPerType: this.byType.size > 0 ? totalParticipants / this.byType.size : 0
    };
  }
}

