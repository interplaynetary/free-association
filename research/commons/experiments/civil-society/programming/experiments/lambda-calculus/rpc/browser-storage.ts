/**
 * Browser Storage using IndexedDB
 * 
 * Provides persistent storage for sparse recognition graphs in the browser.
 * Features:
 * - Sparse edge storage (only non-zero values)
 * - Composite key index [from, to] for O(1) lookups
 * - Sync queue for offline operations
 * - Audit log for debugging
 * - Efficient bulk operations
 */

import type { SparseRecognitionGraph, EntityId } from '../src/sparse/types';
import { SparseOps, empty as emptySparseGraph } from '../src/sparse/types';
import type { SyncQueueItem, SyncOperation } from './types';
import type { Stamp as ITCStamp } from '../itc';
import { ITClock } from './clock';

const DB_NAME = 'FreeAssociationLambdaCalculus';
const DB_VERSION = 1;

/**
 * IndexedDB store names
 */
export const STORES = {
  RECOGNITION_EDGES: 'recognitionEdges',
  ENTITIES: 'entities',
  SYNC_QUEUE: 'syncQueue',
  AUDIT_LOG: 'auditLog',
  VECTOR_CLOCKS: 'vectorClocks'
} as const;

/**
 * Recognition edge record in IndexedDB
 */
export interface RecognitionEdgeRecord {
  fromId: string;
  toId: string;
  amount: number;
  timestamp: number;
  version: number;
}

/**
 * Entity record in IndexedDB
 */
export interface EntityRecord {
  id: string;
  publicKey?: string;
  metadata?: Record<string, unknown>;
  createdAt: number;
  updatedAt: number;
}

/**
 * Audit log entry
 */
export interface AuditLogEntry {
  id?: number;
  timestamp: number;
  type: 'allocate' | 'revoke' | 'sync';
  entityId: string;
  data: unknown;
}

/**
 * Browser storage class using IndexedDB
 */
export class BrowserStorage {
  private db: IDBDatabase | null = null;
  private localEntityId: string;
  private clock: ITClock;

  constructor(localEntityId: string, initialClock?: ITCStamp) {
    this.localEntityId = localEntityId;
    this.clock = initialClock ? new ITClock(initialClock) : ITClock.seed();
  }

  /**
   * Initialize IndexedDB connection
   */
  async initialize(): Promise<void> {
    return new Promise((resolve, reject) => {
      const request = indexedDB.open(DB_NAME, DB_VERSION);

      request.onerror = () => reject(request.error);
      request.onsuccess = () => {
        this.db = request.result;
        resolve();
      };

      request.onupgradeneeded = (event) => {
        const db = (event.target as IDBOpenDBRequest).result;

        // Recognition edges store
        if (!db.objectStoreNames.contains(STORES.RECOGNITION_EDGES)) {
          const edgeStore = db.createObjectStore(STORES.RECOGNITION_EDGES, {
            keyPath: ['fromId', 'toId']
          });
          edgeStore.createIndex('fromId', 'fromId', { unique: false });
          edgeStore.createIndex('toId', 'toId', { unique: false });
          edgeStore.createIndex('timestamp', 'timestamp', { unique: false });
        }

        // Entities store
        if (!db.objectStoreNames.contains(STORES.ENTITIES)) {
          db.createObjectStore(STORES.ENTITIES, { keyPath: 'id' });
        }

        // Sync queue store
        if (!db.objectStoreNames.contains(STORES.SYNC_QUEUE)) {
          const syncStore = db.createObjectStore(STORES.SYNC_QUEUE, {
            keyPath: 'id',
            autoIncrement: true
          });
          syncStore.createIndex('synced', 'synced', { unique: false });
          syncStore.createIndex('timestamp', 'operation.timestamp', { unique: false });
        }

        // Audit log store
        if (!db.objectStoreNames.contains(STORES.AUDIT_LOG)) {
          const auditStore = db.createObjectStore(STORES.AUDIT_LOG, {
            keyPath: 'id',
            autoIncrement: true
          });
          auditStore.createIndex('timestamp', 'timestamp', { unique: false });
          auditStore.createIndex('entityId', 'entityId', { unique: false });
        }

        // ITC stamps store
        if (!db.objectStoreNames.contains(STORES.VECTOR_CLOCKS)) {
          // Keep same store name for compatibility, but store ITC stamps
          db.createObjectStore(STORES.VECTOR_CLOCKS, { keyPath: 'entityId' });
        }
      };
    });
  }

  // ============================================================================
  // Recognition Edge Operations (Sparse)
  // ============================================================================

  /**
   * Get recognition edge value
   * Returns 0 if edge doesn't exist
   */
  async getRecognitionEdge(from: EntityId, to: EntityId): Promise<number> {
    if (!this.db) throw new Error('Database not initialized');

    return new Promise((resolve, reject) => {
      const tx = this.db!.transaction(STORES.RECOGNITION_EDGES, 'readonly');
      const store = tx.objectStore(STORES.RECOGNITION_EDGES);
      const request = store.get([from, to]);

      request.onsuccess = () => {
        const record = request.result as RecognitionEdgeRecord | undefined;
        resolve(record?.amount ?? 0);
      };
      request.onerror = () => reject(request.error);
    });
  }

  /**
   * Set recognition edge value
   * Removes edge if amount is 0 (maintains sparsity)
   */
  async setRecognitionEdge(from: EntityId, to: EntityId, amount: number): Promise<void> {
    if (!this.db) throw new Error('Database not initialized');

    // Increment ITC clock
    this.clock.increment();

    return new Promise((resolve, reject) => {
      const tx = this.db!.transaction(
        [STORES.RECOGNITION_EDGES, STORES.AUDIT_LOG],
        'readwrite'
      );

      if (amount === 0) {
        // Remove edge to maintain sparsity
        const request = tx.objectStore(STORES.RECOGNITION_EDGES).delete([from, to]);
        request.onerror = () => reject(request.error);
      } else {
        // Add or update edge
        const record: RecognitionEdgeRecord = {
          fromId: from,
          toId: to,
          amount,
          timestamp: Date.now(),
          version: 0  // Version not needed with ITC
        };

        const request = tx.objectStore(STORES.RECOGNITION_EDGES).put(record);
        request.onerror = () => reject(request.error);
      }

      // Add to audit log
      const auditEntry: AuditLogEntry = {
        timestamp: Date.now(),
        type: 'allocate',
        entityId: from,
        data: { to, amount }
      };
      tx.objectStore(STORES.AUDIT_LOG).add(auditEntry);

      tx.oncomplete = () => resolve();
      tx.onerror = () => reject(tx.error);
    });
  }

  /**
   * Get all outgoing edges from an entity
   * Returns only non-zero edges (sparse!)
   */
  async getOutgoingEdges(from: EntityId): Promise<Map<EntityId, number>> {
    if (!this.db) throw new Error('Database not initialized');

    return new Promise((resolve, reject) => {
      const tx = this.db!.transaction(STORES.RECOGNITION_EDGES, 'readonly');
      const store = tx.objectStore(STORES.RECOGNITION_EDGES);
      const index = store.index('fromId');
      const request = index.getAll(from);

      request.onsuccess = () => {
        const edges = new Map<EntityId, number>();
        const records = request.result as RecognitionEdgeRecord[];
        
        for (const record of records) {
          if (record.amount > 0) {
            edges.set(record.toId, record.amount);
          }
        }
        
        resolve(edges);
      };
      request.onerror = () => reject(request.error);
    });
  }

  /**
   * Get all incoming edges to an entity
   * Returns only non-zero edges (sparse!)
   */
  async getIncomingEdges(to: EntityId): Promise<Map<EntityId, number>> {
    if (!this.db) throw new Error('Database not initialized');

    return new Promise((resolve, reject) => {
      const tx = this.db!.transaction(STORES.RECOGNITION_EDGES, 'readonly');
      const store = tx.objectStore(STORES.RECOGNITION_EDGES);
      const index = store.index('toId');
      const request = index.getAll(to);

      request.onsuccess = () => {
        const edges = new Map<EntityId, number>();
        const records = request.result as RecognitionEdgeRecord[];
        
        for (const record of records) {
          if (record.amount > 0) {
            edges.set(record.fromId, record.amount);
          }
        }
        
        resolve(edges);
      };
      request.onerror = () => reject(request.error);
    });
  }

  /**
   * Load entire sparse graph from IndexedDB
   */
  async loadSparseGraph(): Promise<SparseRecognitionGraph> {
    if (!this.db) throw new Error('Database not initialized');

    return new Promise((resolve, reject) => {
      const tx = this.db!.transaction(STORES.RECOGNITION_EDGES, 'readonly');
      const store = tx.objectStore(STORES.RECOGNITION_EDGES);
      const request = store.getAll();

      request.onsuccess = () => {
        const edges = new Map<EntityId, Map<EntityId, number>>();
        const records = request.result as RecognitionEdgeRecord[];
        
        for (const record of records) {
          if (!edges.has(record.fromId)) {
            edges.set(record.fromId, new Map());
          }
          if (record.amount > 0) {
            edges.get(record.fromId)!.set(record.toId, record.amount);
          }
        }
        
        const graph: SparseRecognitionGraph = { edges };
        graph.metadata = SparseOps.computeMetadata(graph);
        
        resolve(graph);
      };
      request.onerror = () => reject(request.error);
    });
  }

  /**
   * Save entire sparse graph to IndexedDB
   * Replaces all existing edges
   */
  async saveSparseGraph(graph: SparseRecognitionGraph): Promise<void> {
    if (!this.db) throw new Error('Database not initialized');

    return new Promise((resolve, reject) => {
      const tx = this.db!.transaction(STORES.RECOGNITION_EDGES, 'readwrite');
      const store = tx.objectStore(STORES.RECOGNITION_EDGES);

      // Clear existing edges
      store.clear();

      // Add all non-zero edges
      for (const [from, to, amount] of SparseOps.edges(graph)) {
        const record: RecognitionEdgeRecord = {
          fromId: from,
          toId: to,
          amount,
          timestamp: Date.now(),
          version: this.vectorClock[this.localEntityId] || 0
        };
        store.add(record);
      }

      tx.oncomplete = () => resolve();
      tx.onerror = () => reject(tx.error);
    });
  }

  // ============================================================================
  // Sync Queue Operations
  // ============================================================================

  /**
   * Add operation to sync queue
   * Used for offline-first operation
   */
  async queueSync(operation: SyncOperation): Promise<number> {
    if (!this.db) throw new Error('Database not initialized');

    return new Promise((resolve, reject) => {
      const tx = this.db!.transaction(STORES.SYNC_QUEUE, 'readwrite');
      const store = tx.objectStore(STORES.SYNC_QUEUE);

      const item: SyncQueueItem = {
        operation,
        synced: false,
        attempts: 0
      };

      const request = store.add(item);
      request.onsuccess = () => resolve(request.result as number);
      request.onerror = () => reject(request.error);
    });
  }

  /**
   * Get all unsynced operations
   */
  async getSyncQueue(): Promise<SyncQueueItem[]> {
    if (!this.db) throw new Error('Database not initialized');

    return new Promise((resolve, reject) => {
      const tx = this.db!.transaction(STORES.SYNC_QUEUE, 'readonly');
      const store = tx.objectStore(STORES.SYNC_QUEUE);
      const index = store.index('synced');
      const request = index.getAll(false);

      request.onsuccess = () => resolve(request.result);
      request.onerror = () => reject(request.error);
    });
  }

  /**
   * Mark operation as synced
   */
  async markSynced(id: number): Promise<void> {
    if (!this.db) throw new Error('Database not initialized');

    return new Promise((resolve, reject) => {
      const tx = this.db!.transaction(STORES.SYNC_QUEUE, 'readwrite');
      const store = tx.objectStore(STORES.SYNC_QUEUE);
      const getRequest = store.get(id);

      getRequest.onsuccess = () => {
        const item = getRequest.result as SyncQueueItem;
        if (item) {
          item.synced = true;
          store.put(item);
        }
      };

      tx.oncomplete = () => resolve();
      tx.onerror = () => reject(tx.error);
    });
  }

  /**
   * Remove synced operations older than specified time
   */
  async cleanupSyncQueue(olderThan: number = Date.now() - 24 * 60 * 60 * 1000): Promise<number> {
    if (!this.db) throw new Error('Database not initialized');

    return new Promise((resolve, reject) => {
      const tx = this.db!.transaction(STORES.SYNC_QUEUE, 'readwrite');
      const store = tx.objectStore(STORES.SYNC_QUEUE);
      const index = store.index('synced');
      const request = index.openCursor(true); // Only synced

      let deleted = 0;

      request.onsuccess = (event) => {
        const cursor = (event.target as IDBRequest).result;
        if (cursor) {
          const item = cursor.value as SyncQueueItem;
          if (item.operation.timestamp < olderThan) {
            cursor.delete();
            deleted++;
          }
          cursor.continue();
        }
      };

      tx.oncomplete = () => resolve(deleted);
      tx.onerror = () => reject(tx.error);
    });
  }

  // ============================================================================
  // Entity Operations
  // ============================================================================

  /**
   * Save entity metadata
   */
  async saveEntity(entity: EntityRecord): Promise<void> {
    if (!this.db) throw new Error('Database not initialized');

    return new Promise((resolve, reject) => {
      const tx = this.db!.transaction(STORES.ENTITIES, 'readwrite');
      const store = tx.objectStore(STORES.ENTITIES);
      
      const record = {
        ...entity,
        updatedAt: Date.now()
      };
      
      store.put(record);

      tx.oncomplete = () => resolve();
      tx.onerror = () => reject(tx.error);
    });
  }

  /**
   * Get entity metadata
   */
  async getEntity(id: EntityId): Promise<EntityRecord | null> {
    if (!this.db) throw new Error('Database not initialized');

    return new Promise((resolve, reject) => {
      const tx = this.db!.transaction(STORES.ENTITIES, 'readonly');
      const store = tx.objectStore(STORES.ENTITIES);
      const request = store.get(id);

      request.onsuccess = () => resolve(request.result || null);
      request.onerror = () => reject(request.error);
    });
  }

  // ============================================================================
  // ITC Clock Operations
  // ============================================================================

  /**
   * Get ITC stamp for an entity
   */
  async getITCStamp(entityId: EntityId): Promise<ITCStamp | null> {
    if (!this.db) throw new Error('Database not initialized');

    return new Promise((resolve, reject) => {
      const tx = this.db!.transaction(STORES.VECTOR_CLOCKS, 'readonly');
      const store = tx.objectStore(STORES.VECTOR_CLOCKS);
      const request = store.get(entityId);

      request.onsuccess = () => {
        const record = request.result;
        resolve(record?.stamp || null);
      };
      request.onerror = () => reject(request.error);
    });
  }

  /**
   * Update ITC stamp
   */
  async updateITCStamp(entityId: EntityId, stamp: ITCStamp): Promise<void> {
    if (!this.db) throw new Error('Database not initialized');

    return new Promise((resolve, reject) => {
      const tx = this.db!.transaction(STORES.VECTOR_CLOCKS, 'readwrite');
      const store = tx.objectStore(STORES.VECTOR_CLOCKS);

      store.put({ entityId, stamp });

      tx.oncomplete = () => resolve();
      tx.onerror = () => reject(tx.error);
    });
  }

  /**
   * Increment local ITC clock
   */
  incrementClock(): void {
    this.clock.increment();
  }

  /**
   * Get current local ITC stamp
   */
  getLocalStamp(): ITCStamp {
    return this.clock.serialize();
  }

  /**
   * Merge ITC stamps (for CRDT)
   */
  mergeStamp(remote: ITCStamp): void {
    this.clock.merge(remote);
  }

  /**
   * Fork clock for new peer
   */
  forkClock(): ITClock {
    return this.clock.fork();
  }

  /**
   * Get clock instance
   */
  getClock(): ITClock {
    return this.clock;
  }

  // ============================================================================
  // Audit Log Operations
  // ============================================================================

  /**
   * Add entry to audit log
   */
  async addAuditLog(entry: Omit<AuditLogEntry, 'id'>): Promise<number> {
    if (!this.db) throw new Error('Database not initialized');

    return new Promise((resolve, reject) => {
      const tx = this.db!.transaction(STORES.AUDIT_LOG, 'readwrite');
      const store = tx.objectStore(STORES.AUDIT_LOG);
      const request = store.add(entry);

      request.onsuccess = () => resolve(request.result as number);
      request.onerror = () => reject(request.error);
    });
  }

  /**
   * Get audit log entries for an entity
   */
  async getAuditLog(entityId: EntityId, limit: number = 100): Promise<AuditLogEntry[]> {
    if (!this.db) throw new Error('Database not initialized');

    return new Promise((resolve, reject) => {
      const tx = this.db!.transaction(STORES.AUDIT_LOG, 'readonly');
      const store = tx.objectStore(STORES.AUDIT_LOG);
      const index = store.index('entityId');
      const request = index.getAll(entityId, limit);

      request.onsuccess = () => resolve(request.result);
      request.onerror = () => reject(request.error);
    });
  }

  // ============================================================================
  // Bulk Operations
  // ============================================================================

  /**
   * Batch update multiple edges (more efficient than individual updates)
   */
  async batchUpdateEdges(updates: Array<{from: EntityId; to: EntityId; amount: number}>): Promise<void> {
    if (!this.db) throw new Error('Database not initialized');

    this.clock.increment();

    return new Promise((resolve, reject) => {
      const tx = this.db!.transaction(STORES.RECOGNITION_EDGES, 'readwrite');
      const store = tx.objectStore(STORES.RECOGNITION_EDGES);

      for (const {from, to, amount} of updates) {
        if (amount === 0) {
          store.delete([from, to]);
        } else {
          const record: RecognitionEdgeRecord = {
            fromId: from,
            toId: to,
            amount,
            timestamp: Date.now(),
            version: 0  // Version not needed with ITC
          };
          store.put(record);
        }
      }

      tx.oncomplete = () => resolve();
      tx.onerror = () => reject(tx.error);
    });
  }

  // ============================================================================
  // Cleanup
  // ============================================================================

  /**
   * Close database connection
   */
  close(): void {
    if (this.db) {
      this.db.close();
      this.db = null;
    }
  }

  /**
   * Delete entire database
   */
  static async deleteDatabase(): Promise<void> {
    return new Promise((resolve, reject) => {
      const request = indexedDB.deleteDatabase(DB_NAME);
      request.onsuccess = () => resolve();
      request.onerror = () => reject(request.error);
    });
  }
}

