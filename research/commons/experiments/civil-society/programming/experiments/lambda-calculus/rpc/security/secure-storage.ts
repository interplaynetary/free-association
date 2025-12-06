/**
 * Secure Storage - Automatic Signature Handling
 * 
 * Wraps BrowserStorage to automatically sign/verify all state updates.
 * This is transparent - developers just use normal storage APIs!
 */

import { BrowserStorage } from '../browser-storage';
import type { SecureContext } from './secure-context';
import type { EntityId } from '../types';
import type { SignedStateUpdate } from '../identity/signing';

/**
 * Secure Storage - Storage with Built-In Signatures
 * 
 * This extends BrowserStorage to automatically handle signatures.
 * ALL state updates are signed before storage!
 * 
 * @example
 * const storage = new SecureStorage(entityId, ctx);
 * await storage.initialize();
 * 
 * // This automatically signs the update!
 * await storage.storeRecognition('bob', 0.8);
 * 
 * // This automatically verifies signatures!
 * const recognition = await storage.getRecognition('alice', 'bob');
 */
export class SecureStorage {
  private storage: BrowserStorage;
  private secureContext: SecureContext;
  private entityId: EntityId;

  constructor(entityId: EntityId, secureContext: SecureContext) {
    this.entityId = entityId;
    this.secureContext = secureContext;
    this.storage = new BrowserStorage(entityId);
  }

  /**
   * Initialize storage
   */
  async initialize(): Promise<void> {
    await this.storage.initialize();
  }

  /**
   * Store recognition allocation (AUTOMATICALLY SIGNED)
   * 
   * This is the key method - it automatically signs!
   * 
   * @example
   * await secureStorage.storeRecognition('bob', 0.8);
   * // Behind the scenes:
   * // 1. Creates update { from: 'alice', to: 'bob', value: 0.8, ... }
   * // 2. Signs with private key
   * // 3. Stores { ...update, signature, publicKey }
   */
  async storeRecognition(to: EntityId, value: number): Promise<void> {
    // Create and sign the update
    const signedUpdate = await this.secureContext.signUpdate(to, value);

    // Store the signed update
    await this.storeSignedUpdate(signedUpdate);
  }

  /**
   * Get recognition allocation (AUTOMATICALLY VERIFIED)
   * 
   * @example
   * const value = await secureStorage.getRecognition('alice', 'bob');
   * // Behind the scenes:
   * // 1. Loads signed update from storage
   * // 2. Verifies signature
   * // 3. Returns value only if valid
   */
  async getRecognition(from: EntityId, to: EntityId): Promise<number | undefined> {
    const signedUpdate = await this.getSignedUpdate(from, to);
    
    if (!signedUpdate) {
      return undefined;
    }

    // Verify the signature
    const isValid = await this.secureContext.verify(signedUpdate);
    
    if (!isValid) {
      console.error('Invalid signature on stored update!', signedUpdate);
      // Optionally delete corrupted data
      await this.deleteUpdate(from, to);
      return undefined;
    }

    return signedUpdate.value;
  }

  /**
   * Get all recognition allocations from an entity (VERIFIED)
   */
  async getAllRecognitionsFrom(from: EntityId): Promise<Map<EntityId, number>> {
    const signedUpdates = await this.getAllSignedUpdatesFrom(from);
    const verified = new Map<EntityId, number>();

    for (const update of signedUpdates) {
      const isValid = await this.secureContext.verify(update);
      
      if (isValid) {
        verified.set(update.to, update.value);
      } else {
        console.error('Invalid signature found in storage:', update);
      }
    }

    return verified;
  }

  /**
   * Store a signed update (low-level)
   */
  private async storeSignedUpdate(signedUpdate: SignedStateUpdate): Promise<void> {
    const key = this.makeKey(signedUpdate.from, signedUpdate.to);
    await this.storage.setItem(key, JSON.stringify(signedUpdate));
  }

  /**
   * Get a signed update (low-level)
   */
  private async getSignedUpdate(
    from: EntityId,
    to: EntityId
  ): Promise<SignedStateUpdate | undefined> {
    const key = this.makeKey(from, to);
    const data = await this.storage.getItem(key);
    
    if (!data) {
      return undefined;
    }

    try {
      return JSON.parse(data) as SignedStateUpdate;
    } catch (error) {
      console.error('Failed to parse signed update:', error);
      return undefined;
    }
  }

  /**
   * Get all signed updates from an entity
   */
  private async getAllSignedUpdatesFrom(from: EntityId): Promise<SignedStateUpdate[]> {
    const prefix = `recognition:${from}:`;
    const keys = await this.storage.keys();
    const updates: SignedStateUpdate[] = [];

    for (const key of keys) {
      if (key.startsWith(prefix)) {
        const data = await this.storage.getItem(key);
        if (data) {
          try {
            updates.push(JSON.parse(data));
          } catch (error) {
            console.error('Failed to parse stored update:', error);
          }
        }
      }
    }

    return updates;
  }

  /**
   * Delete an update
   */
  private async deleteUpdate(from: EntityId, to: EntityId): Promise<void> {
    const key = this.makeKey(from, to);
    await this.storage.removeItem(key);
  }

  /**
   * Make storage key
   */
  private makeKey(from: EntityId, to: EntityId): string {
    return `recognition:${from}:${to}`;
  }

  /**
   * Export all signed updates (for sync/backup)
   */
  async exportSignedUpdates(): Promise<SignedStateUpdate[]> {
    const keys = await this.storage.keys();
    const updates: SignedStateUpdate[] = [];

    for (const key of keys) {
      if (key.startsWith('recognition:')) {
        const data = await this.storage.getItem(key);
        if (data) {
          try {
            const update = JSON.parse(data);
            // Verify before exporting
            if (await this.secureContext.verify(update)) {
              updates.push(update);
            }
          } catch (error) {
            console.error('Failed to parse stored update:', error);
          }
        }
      }
    }

    return updates;
  }

  /**
   * Import signed updates (for restoration)
   * 
   * Automatically verifies all signatures!
   */
  async importSignedUpdates(updates: SignedStateUpdate[]): Promise<{
    imported: number;
    rejected: number;
  }> {
    let imported = 0;
    let rejected = 0;

    for (const update of updates) {
      const isValid = await this.secureContext.verify(update);
      
      if (isValid) {
        await this.storeSignedUpdate(update);
        imported++;
      } else {
        console.error('Rejected invalid update:', update);
        rejected++;
      }
    }

    return { imported, rejected };
  }

  /**
   * Clear all storage
   */
  async clear(): Promise<void> {
    await this.storage.clear();
  }
}

/**
 * Create a SecureStorage instance
 * 
 * This is the recommended way to create secure storage.
 * 
 * @example
 * const storage = await createSecureStorage(entityId, keypair);
 * // Now use storage.storeRecognition(), storage.getRecognition(), etc.
 */
export async function createSecureStorage(
  entityId: EntityId,
  secureContext: SecureContext
): Promise<SecureStorage> {
  const storage = new SecureStorage(entityId, secureContext);
  await storage.initialize();
  return storage;
}

