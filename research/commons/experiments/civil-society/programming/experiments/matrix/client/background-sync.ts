/**
 * Background Sync Manager
 * 
 * Handles optimistic updates and eventual consistency:
 * - Queue pending operations
 * - Retry failed syncs
 * - Conflict resolution
 * - Network status monitoring
 * 
 * Features:
 * - Exponential backoff
 * - Priority queue
 * - Batch operations
 * - Event notifications
 */

export interface SyncOperation {
  id: string;
  type: 'recognition' | 'allocation' | 'commitment';
  operation: () => Promise<void>;
  retries: number;
  maxRetries: number;
  priority: number;
  timestamp: number;
}

export interface SyncStatus {
  pending: number;
  syncing: number;
  failed: number;
  succeeded: number;
  isOnline: boolean;
  lastSync: number | null;
}

export type SyncEventType = 'sync-start' | 'sync-success' | 'sync-failure' | 'sync-complete' | 'online' | 'offline';

export type SyncEventHandler = (event: SyncEventType, data?: any) => void;

/**
 * Background Sync Queue
 * 
 * Manages operations that need to sync to server.
 * Automatically retries on failure with exponential backoff.
 */
export class BackgroundSyncManager {
  private queue: SyncOperation[] = [];
  private syncing = new Set<string>();
  private failed = new Map<string, SyncOperation>();
  private succeeded = new Set<string>();
  private isOnline = typeof navigator !== 'undefined' ? navigator.onLine : true;
  private lastSync: number | null = null;
  private eventHandlers: Set<SyncEventHandler> = new Set();
  private syncInterval: ReturnType<typeof setInterval> | null = null;
  
  constructor(
    private readonly syncIntervalMs: number = 5000,
    private readonly batchSize: number = 10
  ) {
    this.setupNetworkListeners();
  }
  
  /**
   * Setup network status listeners
   */
  private setupNetworkListeners(): void {
    // Only setup listeners in browser environment
    if (typeof window !== 'undefined') {
      window.addEventListener('online', () => {
        console.log('[BACKGROUND-SYNC] 🌐 Online');
        this.isOnline = true;
        this.emit('online');
        this.processPendingOperations();
      });
      
      window.addEventListener('offline', () => {
        console.log('[BACKGROUND-SYNC] 📵 Offline');
        this.isOnline = false;
        this.emit('offline');
      });
    }
  }
  
  /**
   * Start background sync
   */
  start(): void {
    if (this.syncInterval) return;
    
    console.log('[BACKGROUND-SYNC] ▶️ Started');
    
    this.syncInterval = setInterval(() => {
      this.processPendingOperations();
    }, this.syncIntervalMs);
    
    // Process immediately
    this.processPendingOperations();
  }
  
  /**
   * Stop background sync
   */
  stop(): void {
    if (this.syncInterval) {
      clearInterval(this.syncInterval);
      this.syncInterval = null;
      console.log('[BACKGROUND-SYNC] ⏸️ Stopped');
    }
  }
  
  /**
   * Add operation to sync queue
   */
  async enqueue(operation: Omit<SyncOperation, 'retries' | 'timestamp'>): Promise<string> {
    const syncOp: SyncOperation = {
      ...operation,
      retries: 0,
      timestamp: Date.now()
    };
    
    this.queue.push(syncOp);
    
    // Sort by priority (higher first)
    this.queue.sort((a, b) => b.priority - a.priority);
    
    console.log(`[BACKGROUND-SYNC] ➕ Queued operation: ${syncOp.id} (priority: ${syncOp.priority})`);
    
    // Process immediately if online
    if (this.isOnline) {
      await this.processPendingOperations();
    }
    
    return syncOp.id;
  }
  
  /**
   * Process pending operations
   */
  private async processPendingOperations(): Promise<void> {
    if (!this.isOnline) {
      console.log('[BACKGROUND-SYNC] 📵 Offline, skipping sync');
      return;
    }
    
    if (this.queue.length === 0) {
      return;
    }
    
    console.log(`[BACKGROUND-SYNC] 🔄 Processing ${this.queue.length} pending operations`);
    
    // Take batch
    const batch = this.queue.splice(0, this.batchSize);
    
    for (const op of batch) {
      if (this.syncing.has(op.id)) continue;
      
      this.syncing.add(op.id);
      this.emit('sync-start', op);
      
      try {
        await op.operation();
        
        this.syncing.delete(op.id);
        this.succeeded.add(op.id);
        this.lastSync = Date.now();
        
        console.log(`[BACKGROUND-SYNC] ✅ Synced: ${op.id}`);
        this.emit('sync-success', op);
        
      } catch (error) {
        console.error(`[BACKGROUND-SYNC] ❌ Failed: ${op.id}`, error);
        
        this.syncing.delete(op.id);
        op.retries++;
        
        if (op.retries < op.maxRetries) {
          // Exponential backoff
          const delay = Math.min(1000 * Math.pow(2, op.retries), 30000);
          console.log(`[BACKGROUND-SYNC] 🔁 Retry ${op.retries}/${op.maxRetries} in ${delay}ms: ${op.id}`);
          
          setTimeout(() => {
            this.queue.push(op);
            this.processPendingOperations();
          }, delay);
          
        } else {
          this.failed.set(op.id, op);
          this.emit('sync-failure', { op, error });
          console.error(`[BACKGROUND-SYNC] 💀 Max retries exceeded: ${op.id}`);
        }
      }
    }
    
    if (this.queue.length === 0 && this.syncing.size === 0) {
      this.emit('sync-complete');
    }
  }
  
  /**
   * Retry failed operation
   */
  async retryFailed(id: string): Promise<void> {
    const op = this.failed.get(id);
    if (!op) {
      throw new Error(`Operation ${id} not found in failed queue`);
    }
    
    this.failed.delete(id);
    op.retries = 0;
    this.queue.push(op);
    
    await this.processPendingOperations();
  }
  
  /**
   * Retry all failed operations
   */
  async retryAllFailed(): Promise<void> {
    const failedOps = Array.from(this.failed.values());
    this.failed.clear();
    
    for (const op of failedOps) {
      op.retries = 0;
      this.queue.push(op);
    }
    
    await this.processPendingOperations();
  }
  
  /**
   * Clear all pending operations
   */
  clear(): void {
    this.queue = [];
    this.syncing.clear();
    this.failed.clear();
    this.succeeded.clear();
    console.log('[BACKGROUND-SYNC] 🧹 Cleared all operations');
  }
  
  /**
   * Get sync status
   */
  getStatus(): SyncStatus {
    return {
      pending: this.queue.length,
      syncing: this.syncing.size,
      failed: this.failed.size,
      succeeded: this.succeeded.size,
      isOnline: this.isOnline,
      lastSync: this.lastSync
    };
  }
  
  /**
   * Add event handler
   */
  on(handler: SyncEventHandler): () => void {
    this.eventHandlers.add(handler);
    return () => this.eventHandlers.delete(handler);
  }
  
  /**
   * Emit event
   */
  private emit(event: SyncEventType, data?: any): void {
    for (const handler of this.eventHandlers) {
      try {
        handler(event, data);
      } catch (error) {
        console.error('[BACKGROUND-SYNC] Event handler error:', error);
      }
    }
  }
}

/**
 * Create a sync operation
 */
export function createSyncOperation(
  id: string,
  type: SyncOperation['type'],
  operation: () => Promise<void>,
  priority: number = 0,
  maxRetries: number = 3
): Omit<SyncOperation, 'retries' | 'timestamp'> {
  return {
    id,
    type,
    operation,
    maxRetries,
    priority
  };
}

