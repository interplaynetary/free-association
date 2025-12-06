/**
 * Persistent Cache Layer (IndexedDB)
 * 
 * Provides offline-first storage for:
 * - Network state snapshots
 * - Recognition matrices
 * - Allocation results
 * - Participant data
 * 
 * Features:
 * - Works offline
 * - Automatic expiration
 * - Versioned storage
 * - Transaction support
 */

import type { Commitment, NeedSlot, AvailabilitySlot, SlotAllocationRecord } from '../schemas';

const DB_NAME = 'free-association-cache';
const DB_VERSION = 1;

export interface NetworkStateSnapshot {
  participantCount: number;
  recognitionMatrix: number[][];
  participantIdToIndex: Record<string, number>;
  participantIndexToId: Record<number, string>;
  timestamp: number;
  version: number;
}

export interface CachedAllocation {
  capacitySlotId: string;
  allocations: SlotAllocationRecord[];
  timestamp: number;
  ttl: number;
}

export interface CachedComputation {
  key: string;
  value: any;
  timestamp: number;
  ttl?: number;
}

/**
 * IndexedDB Persistent Cache
 * 
 * Stores data locally for offline access.
 * Automatically manages expiration and versioning.
 */
export class PersistentCache {
  private db: IDBDatabase | null = null;
  private readonly dbName: string;
  private readonly version: number;
  
  constructor(dbName: string = DB_NAME, version: number = DB_VERSION) {
    this.dbName = dbName;
    this.version = version;
  }
  
  /**
   * Initialize IndexedDB connection
   */
  async initialize(): Promise<void> {
    return new Promise((resolve, reject) => {
      const request = indexedDB.open(this.dbName, this.version);
      
      request.onerror = () => reject(request.error);
      request.onsuccess = () => {
        this.db = request.result;
        console.log('[PERSISTENT-CACHE] ✅ Initialized');
        resolve();
      };
      
      request.onupgradeneeded = (event) => {
        const db = (event.target as IDBOpenDBRequest).result;
        
        // Network state store
        if (!db.objectStoreNames.contains('network-state')) {
          db.createObjectStore('network-state', { keyPath: 'timestamp' });
        }
        
        // Commitments store
        if (!db.objectStoreNames.contains('commitments')) {
          const commitmentStore = db.createObjectStore('commitments', { keyPath: 'pubKey' });
          commitmentStore.createIndex('timestamp', 'timestamp');
        }
        
        // Allocations store
        if (!db.objectStoreNames.contains('allocations')) {
          const allocationStore = db.createObjectStore('allocations', { keyPath: 'capacitySlotId' });
          allocationStore.createIndex('timestamp', 'timestamp');
        }
        
        // Computations store (memoization overflow)
        if (!db.objectStoreNames.contains('computations')) {
          const computationStore = db.createObjectStore('computations', { keyPath: 'key' });
          computationStore.createIndex('timestamp', 'timestamp');
        }
        
        console.log('[PERSISTENT-CACHE] 📦 Database upgraded');
      };
    });
  }
  
  /**
   * Save network state snapshot
   */
  async saveNetworkState(snapshot: NetworkStateSnapshot): Promise<void> {
    if (!this.db) throw new Error('Database not initialized');
    
    return new Promise((resolve, reject) => {
      const transaction = this.db!.transaction(['network-state'], 'readwrite');
      const store = transaction.objectStore('network-state');
      
      const request = store.put(snapshot);
      
      request.onsuccess = () => {
        console.log('[PERSISTENT-CACHE] 💾 Saved network state');
        resolve();
      };
      request.onerror = () => reject(request.error);
    });
  }
  
  /**
   * Load latest network state snapshot
   */
  async loadNetworkState(): Promise<NetworkStateSnapshot | null> {
    if (!this.db) throw new Error('Database not initialized');
    
    return new Promise((resolve, reject) => {
      const transaction = this.db!.transaction(['network-state'], 'readonly');
      const store = transaction.objectStore('network-state');
      
      // Get all snapshots, sorted by timestamp
      const request = store.openCursor(null, 'prev');
      
      request.onsuccess = () => {
        const cursor = request.result;
        if (cursor) {
          console.log('[PERSISTENT-CACHE] 📖 Loaded network state');
          resolve(cursor.value as NetworkStateSnapshot);
        } else {
          resolve(null);
        }
      };
      request.onerror = () => reject(request.error);
    });
  }
  
  /**
   * Save commitment
   */
  async saveCommitment(pubKey: string, commitment: Commitment): Promise<void> {
    if (!this.db) throw new Error('Database not initialized');
    
    return new Promise((resolve, reject) => {
      const transaction = this.db!.transaction(['commitments'], 'readwrite');
      const store = transaction.objectStore('commitments');
      
      const request = store.put({ 
        pubKey, 
        commitment,
        timestamp: Date.now()
      });
      
      request.onsuccess = () => resolve();
      request.onerror = () => reject(request.error);
    });
  }
  
  /**
   * Load commitment
   */
  async loadCommitment(pubKey: string): Promise<Commitment | null> {
    if (!this.db) throw new Error('Database not initialized');
    
    return new Promise((resolve, reject) => {
      const transaction = this.db!.transaction(['commitments'], 'readonly');
      const store = transaction.objectStore('commitments');
      
      const request = store.get(pubKey);
      
      request.onsuccess = () => {
        const result = request.result;
        resolve(result ? result.commitment : null);
      };
      request.onerror = () => reject(request.error);
    });
  }
  
  /**
   * Load all commitments
   */
  async loadAllCommitments(): Promise<Record<string, Commitment>> {
    if (!this.db) throw new Error('Database not initialized');
    
    return new Promise((resolve, reject) => {
      const transaction = this.db!.transaction(['commitments'], 'readonly');
      const store = transaction.objectStore('commitments');
      
      const request = store.getAll();
      
      request.onsuccess = () => {
        const commitments: Record<string, Commitment> = {};
        for (const item of request.result) {
          commitments[item.pubKey] = item.commitment;
        }
        console.log(`[PERSISTENT-CACHE] 📖 Loaded ${Object.keys(commitments).length} commitments`);
        resolve(commitments);
      };
      request.onerror = () => reject(request.error);
    });
  }
  
  /**
   * Save allocation result
   */
  async saveAllocation(allocation: CachedAllocation): Promise<void> {
    if (!this.db) throw new Error('Database not initialized');
    
    return new Promise((resolve, reject) => {
      const transaction = this.db!.transaction(['allocations'], 'readwrite');
      const store = transaction.objectStore('allocations');
      
      const request = store.put(allocation);
      
      request.onsuccess = () => resolve();
      request.onerror = () => reject(request.error);
    });
  }
  
  /**
   * Load allocation result
   */
  async loadAllocation(capacitySlotId: string): Promise<CachedAllocation | null> {
    if (!this.db) throw new Error('Database not initialized');
    
    return new Promise((resolve, reject) => {
      const transaction = this.db!.transaction(['allocations'], 'readonly');
      const store = transaction.objectStore('allocations');
      
      const request = store.get(capacitySlotId);
      
      request.onsuccess = () => {
        const result = request.result;
        
        // Check TTL
        if (result && Date.now() - result.timestamp < result.ttl) {
          resolve(result);
        } else {
          resolve(null);
        }
      };
      request.onerror = () => reject(request.error);
    });
  }
  
  /**
   * Save computation result (memoization overflow to disk)
   */
  async saveComputation(key: string, value: any, ttl?: number): Promise<void> {
    if (!this.db) throw new Error('Database not initialized');
    
    return new Promise((resolve, reject) => {
      const transaction = this.db!.transaction(['computations'], 'readwrite');
      const store = transaction.objectStore('computations');
      
      const request = store.put({
        key,
        value,
        timestamp: Date.now(),
        ttl
      });
      
      request.onsuccess = () => resolve();
      request.onerror = () => reject(request.error);
    });
  }
  
  /**
   * Load computation result
   */
  async loadComputation(key: string): Promise<any | null> {
    if (!this.db) throw new Error('Database not initialized');
    
    return new Promise((resolve, reject) => {
      const transaction = this.db!.transaction(['computations'], 'readonly');
      const store = transaction.objectStore('computations');
      
      const request = store.get(key);
      
      request.onsuccess = () => {
        const result = request.result;
        
        // Check TTL
        if (result) {
          if (!result.ttl || Date.now() - result.timestamp < result.ttl) {
            resolve(result.value);
          } else {
            resolve(null);
          }
        } else {
          resolve(null);
        }
      };
      request.onerror = () => reject(request.error);
    });
  }
  
  /**
   * Clean expired entries
   */
  async cleanExpired(): Promise<number> {
    if (!this.db) throw new Error('Database not initialized');
    
    let cleaned = 0;
    const now = Date.now();
    
    // Clean allocations
    const allocationsTx = this.db.transaction(['allocations'], 'readwrite');
    const allocationsStore = allocationsTx.objectStore('allocations');
    const allocationsRequest = allocationsStore.openCursor();
    
    await new Promise<void>((resolve) => {
      allocationsRequest.onsuccess = () => {
        const cursor = allocationsRequest.result;
        if (cursor) {
          const allocation = cursor.value;
          if (now - allocation.timestamp > allocation.ttl) {
            cursor.delete();
            cleaned++;
          }
          cursor.continue();
        } else {
          resolve();
        }
      };
    });
    
    // Clean computations
    const computationsTx = this.db.transaction(['computations'], 'readwrite');
    const computationsStore = computationsTx.objectStore('computations');
    const computationsRequest = computationsStore.openCursor();
    
    await new Promise<void>((resolve) => {
      computationsRequest.onsuccess = () => {
        const cursor = computationsRequest.result;
        if (cursor) {
          const computation = cursor.value;
          if (computation.ttl && now - computation.timestamp > computation.ttl) {
            cursor.delete();
            cleaned++;
          }
          cursor.continue();
        } else {
          resolve();
        }
      };
    });
    
    if (cleaned > 0) {
      console.log(`[PERSISTENT-CACHE] 🧹 Cleaned ${cleaned} expired entries`);
    }
    
    return cleaned;
  }
  
  /**
   * Clear all cached data
   */
  async clear(): Promise<void> {
    if (!this.db) throw new Error('Database not initialized');
    
    const stores = ['network-state', 'commitments', 'allocations', 'computations'];
    const transaction = this.db.transaction(stores, 'readwrite');
    
    for (const storeName of stores) {
      transaction.objectStore(storeName).clear();
    }
    
    return new Promise((resolve, reject) => {
      transaction.oncomplete = () => {
        console.log('[PERSISTENT-CACHE] 🧹 Cleared all data');
        resolve();
      };
      transaction.onerror = () => reject(transaction.error);
    });
  }
  
  /**
   * Get cache statistics
   */
  async getStats(): Promise<{
    networkStateCount: number;
    commitmentsCount: number;
    allocationsCount: number;
    computationsCount: number;
  }> {
    if (!this.db) throw new Error('Database not initialized');
    
    const stats = {
      networkStateCount: 0,
      commitmentsCount: 0,
      allocationsCount: 0,
      computationsCount: 0
    };
    
    // Count network states
    const networkStateTx = this.db.transaction(['network-state'], 'readonly');
    stats.networkStateCount = await new Promise((resolve) => {
      const request = networkStateTx.objectStore('network-state').count();
      request.onsuccess = () => resolve(request.result);
    });
    
    // Count commitments
    const commitmentsTx = this.db.transaction(['commitments'], 'readonly');
    stats.commitmentsCount = await new Promise((resolve) => {
      const request = commitmentsTx.objectStore('commitments').count();
      request.onsuccess = () => resolve(request.result);
    });
    
    // Count allocations
    const allocationsTx = this.db.transaction(['allocations'], 'readonly');
    stats.allocationsCount = await new Promise((resolve) => {
      const request = allocationsTx.objectStore('allocations').count();
      request.onsuccess = () => resolve(request.result);
    });
    
    // Count computations
    const computationsTx = this.db.transaction(['computations'], 'readonly');
    stats.computationsCount = await new Promise((resolve) => {
      const request = computationsTx.objectStore('computations').count();
      request.onsuccess = () => resolve(request.result);
    });
    
    return stats;
  }
  
  /**
   * Close database connection
   */
  close(): void {
    if (this.db) {
      this.db.close();
      this.db = null;
      console.log('[PERSISTENT-CACHE] 🔒 Closed');
    }
  }
}

