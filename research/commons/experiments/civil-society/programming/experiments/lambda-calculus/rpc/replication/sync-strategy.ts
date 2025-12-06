/**
 * Replication Sync Strategies
 * 
 * Different strategies for syncing replicated data:
 * - Pull: Periodically fetch updates from peers
 * - Push: Receive updates pushed from peers
 * - Hybrid: Combination of both
 */

import type { EntityId } from '../../src/sparse/types';
import type { SyncUpdate, ITCStamp } from '../types';
import { ITClock, batchResolveConflicts } from '../clock';

export type SyncMode = 'pull' | 'push' | 'hybrid';

/**
 * Sync schedule
 */
export interface SyncSchedule {
  mode: SyncMode;
  interval: number;       // ms between syncs (for pull mode)
  batchSize: number;      // max updates per sync
  priority: 'recognition' | 'time' | 'size';  // What to sync first
}

/**
 * Sync coordinator
 */
export class SyncCoordinator {
  private schedule: SyncSchedule;
  private lastSync = new Map<EntityId, number>();
  private pendingUpdates = new Map<EntityId, SyncUpdate[]>();

  constructor(schedule: SyncSchedule) {
    this.schedule = schedule;
  }

  /**
   * Check if entity needs sync
   */
  needsSync(entityId: EntityId): boolean {
    const lastSync = this.lastSync.get(entityId) ?? 0;
    const now = Date.now();
    
    return (now - lastSync) >= this.schedule.interval;
  }

  /**
   * Get entities that need sync, sorted by priority
   */
  getSyncQueue(
    mrs: Map<EntityId, number>,  // MRS values for prioritization
    now: number = Date.now()
  ): EntityId[] {
    const queue: Array<{ id: EntityId; priority: number }> = [];
    
    for (const [entityId, mrsValue] of mrs) {
      if (!this.needsSync(entityId)) continue;
      
      let priority: number;
      
      switch (this.schedule.priority) {
        case 'recognition':
          // Higher MRS = higher priority
          priority = mrsValue;
          break;
          
        case 'time':
          // Longer since last sync = higher priority
          const lastSync = this.lastSync.get(entityId) ?? 0;
          priority = now - lastSync;
          break;
          
        case 'size':
          // More pending updates = higher priority
          const pending = this.pendingUpdates.get(entityId)?.length ?? 0;
          priority = pending;
          break;
          
        default:
          priority = mrsValue;
      }
      
      queue.push({ id: entityId, priority });
    }
    
    // Sort by priority (highest first)
    queue.sort((a, b) => b.priority - a.priority);
    
    // Return top N based on batch size
    return queue.slice(0, this.schedule.batchSize).map(item => item.id);
  }

  /**
   * Mark entity as synced
   */
  markSynced(entityId: EntityId): void {
    this.lastSync.set(entityId, Date.now());
  }

  /**
   * Queue update for push mode
   */
  queueUpdate(entityId: EntityId, update: SyncUpdate): void {
    let updates = this.pendingUpdates.get(entityId);
    if (!updates) {
      updates = [];
      this.pendingUpdates.set(entityId, updates);
    }
    updates.push(update);
  }

  /**
   * Get pending updates for entity
   */
  getPendingUpdates(entityId: EntityId): SyncUpdate[] {
    return this.pendingUpdates.get(entityId) ?? [];
  }

  /**
   * Clear pending updates after sync
   */
  clearPending(entityId: EntityId): void {
    this.pendingUpdates.delete(entityId);
  }

  /**
   * Update sync schedule
   */
  updateSchedule(schedule: Partial<SyncSchedule>): void {
    this.schedule = { ...this.schedule, ...schedule };
  }

  /**
   * Get sync statistics
   */
  getStats(): {
    entitiesTracked: number;
    pendingUpdates: number;
    avgTimeSinceSync: number;
  } {
    const now = Date.now();
    let totalTimeSinceSync = 0;
    let totalPending = 0;
    
    for (const [entityId, lastSync] of this.lastSync) {
      totalTimeSinceSync += now - lastSync;
      totalPending += this.pendingUpdates.get(entityId)?.length ?? 0;
    }
    
    return {
      entitiesTracked: this.lastSync.size,
      pendingUpdates: totalPending,
      avgTimeSinceSync: this.lastSync.size > 0 
        ? totalTimeSinceSync / this.lastSync.size
        : 0
    };
  }
}

/**
 * Conflict resolution for replicated data
 */
export class ConflictResolver {
  /**
   * Resolve conflict using vector clocks
   * 
   * Returns:
   * - 'local' if local version is newer
   * - 'remote' if remote version is newer
   * - 'concurrent' if concurrent (need merge)
   */
  resolveVectorClock(
    local: VectorClock,
    remote: VectorClock
  ): 'local' | 'remote' | 'concurrent' {
    let localNewer = false;
    let remoteNewer = false;
    
    // Get all entity IDs from both clocks
    const allIds = new Set([
      ...Object.keys(local),
      ...Object.keys(remote)
    ]);
    
    for (const id of allIds) {
      const localTime = local[id] ?? 0;
      const remoteTime = remote[id] ?? 0;
      
      if (localTime > remoteTime) {
        localNewer = true;
      } else if (remoteTime > localTime) {
        remoteNewer = true;
      }
    }
    
    if (localNewer && !remoteNewer) return 'local';
    if (remoteNewer && !localNewer) return 'remote';
    return 'concurrent';
  }

  /**
   * Merge concurrent updates
   * For recognition values, take the maximum (favor higher recognition)
   */
  mergeConcurrent(
    localValue: number,
    remoteValue: number,
    strategy: 'max' | 'min' | 'avg' = 'max'
  ): number {
    switch (strategy) {
      case 'max':
        return Math.max(localValue, remoteValue);
      case 'min':
        return Math.min(localValue, remoteValue);
      case 'avg':
        return (localValue + remoteValue) / 2;
      default:
        return Math.max(localValue, remoteValue);
    }
  }

  /**
   * Merge vector clocks
   * Takes maximum timestamp for each entity
   */
  mergeVectorClocks(local: VectorClock, remote: VectorClock): VectorClock {
    const merged: VectorClock = { ...local };
    
    for (const [id, timestamp] of Object.entries(remote)) {
      merged[id] = Math.max(merged[id] ?? 0, timestamp);
    }
    
    return merged;
  }
}

