/**
 * SecureStorage Tests
 * 
 * Test auto-signing storage layer
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { SecureStorage, createSecureStorage } from '../secure-storage';
import { SecureContext } from '../secure-context';
import { generateKeypair } from '../../identity/keypair';

describe('SecureStorage', () => {
  let keypair: any;
  let ctx: SecureContext;
  let storage: SecureStorage;

  beforeEach(async () => {
    keypair = await generateKeypair();
    ctx = await SecureContext.create(keypair, 'alice');
    storage = new SecureStorage('alice', ctx);
    await storage.initialize();
  });

  afterEach(async () => {
    await storage.clear();
  });

  describe('Auto-signing on write', () => {
    it('automatically signs when storing recognition', async () => {
      await storage.storeRecognition('bob', 0.8);

      const value = await storage.getRecognition('alice', 'bob');
      
      expect(value).toBe(0.8);
    });

    it('includes signature and publicKey in stored data', async () => {
      await storage.storeRecognition('bob', 0.8);

      // Export to check the signed data
      const updates = await storage.exportSignedUpdates();
      
      expect(updates).toHaveLength(1);
      expect(updates[0].signature).toBeDefined();
      expect(updates[0].publicKey).toBe(keypair.publicKey);
      expect(updates[0].from).toBe('alice');
      expect(updates[0].to).toBe('bob');
      expect(updates[0].value).toBe(0.8);
    });

    it('stores multiple updates independently', async () => {
      await storage.storeRecognition('bob', 0.8);
      await storage.storeRecognition('charlie', 0.6);
      await storage.storeRecognition('dave', 0.4);

      const bobValue = await storage.getRecognition('alice', 'bob');
      const charlieValue = await storage.getRecognition('alice', 'charlie');
      const daveValue = await storage.getRecognition('alice', 'dave');

      expect(bobValue).toBe(0.8);
      expect(charlieValue).toBe(0.6);
      expect(daveValue).toBe(0.4);
    });
  });

  describe('Auto-verification on read', () => {
    it('verifies signatures when loading', async () => {
      await storage.storeRecognition('bob', 0.8);
      
      const value = await storage.getRecognition('alice', 'bob');
      
      expect(value).toBe(0.8);
    });

    it('rejects corrupted data', async () => {
      await storage.storeRecognition('bob', 0.8);
      
      // Manually tamper with the stored data
      const updates = await storage.exportSignedUpdates();
      const tampered = {
        ...updates[0],
        value: 0.1  // Changed!
      };
      
      // Import tampered data
      const result = await storage.importSignedUpdates([tampered]);
      
      expect(result.imported).toBe(0);
      expect(result.rejected).toBe(1);
    });

    it('handles missing signatures gracefully', async () => {
      await storage.storeRecognition('bob', 0.8);
      
      const value = await storage.getRecognition('alice', 'nonexistent');
      
      expect(value).toBeUndefined();
    });

    it('returns undefined for non-existent data', async () => {
      const value = await storage.getRecognition('alice', 'bob');
      
      expect(value).toBeUndefined();
    });
  });

  describe('Import/Export', () => {
    it('exports only verified updates', async () => {
      await storage.storeRecognition('bob', 0.8);
      await storage.storeRecognition('charlie', 0.6);

      const updates = await storage.exportSignedUpdates();
      
      expect(updates).toHaveLength(2);
      
      // All should be verified (have valid signatures)
      for (const update of updates) {
        const isValid = await ctx.verify(update);
        expect(isValid).toBe(true);
      }
    });

    it('imports with verification', async () => {
      const signedUpdate = await ctx.signUpdate('bob', 0.8);
      
      const result = await storage.importSignedUpdates([signedUpdate]);
      
      expect(result.imported).toBe(1);
      expect(result.rejected).toBe(0);
      
      const value = await storage.getRecognition('alice', 'bob');
      expect(value).toBe(0.8);
    });

    it('reports rejected updates', async () => {
      const validUpdate = await ctx.signUpdate('bob', 0.8);
      const invalidUpdate = {
        ...validUpdate,
        signature: 'fake_signature',
        to: 'charlie'
      };
      
      const result = await storage.importSignedUpdates([
        validUpdate,
        invalidUpdate
      ]);
      
      expect(result.imported).toBe(1);
      expect(result.rejected).toBe(1);
    });

    it('handles batch imports', async () => {
      const updates = await Promise.all([
        ctx.signUpdate('bob', 0.8),
        ctx.signUpdate('charlie', 0.6),
        ctx.signUpdate('dave', 0.4)
      ]);
      
      const result = await storage.importSignedUpdates(updates);
      
      expect(result.imported).toBe(3);
      expect(result.rejected).toBe(0);
      
      const allRecognitions = await storage.getAllRecognitionsFrom('alice');
      expect(allRecognitions.size).toBe(3);
    });
  });

  describe('getAllRecognitionsFrom', () => {
    it('returns all verified recognitions from an entity', async () => {
      await storage.storeRecognition('bob', 0.8);
      await storage.storeRecognition('charlie', 0.6);
      await storage.storeRecognition('dave', 0.4);

      const all = await storage.getAllRecognitionsFrom('alice');
      
      expect(all.size).toBe(3);
      expect(all.get('bob')).toBe(0.8);
      expect(all.get('charlie')).toBe(0.6);
      expect(all.get('dave')).toBe(0.4);
    });

    it('returns empty map for entity with no recognitions', async () => {
      const all = await storage.getAllRecognitionsFrom('alice');
      
      expect(all.size).toBe(0);
    });
  });

  describe('createSecureStorage factory', () => {
    it('creates and initializes storage', async () => {
      const newStorage = await createSecureStorage('bob', ctx);
      
      expect(newStorage).toBeDefined();
      
      // Should be able to use immediately
      await newStorage.storeRecognition('alice', 0.5);
      const value = await newStorage.getRecognition('bob', 'alice');
      
      expect(value).toBe(0.5);
      
      await newStorage.clear();
    });
  });

  describe('Clear storage', () => {
    it('clears all stored data', async () => {
      await storage.storeRecognition('bob', 0.8);
      await storage.storeRecognition('charlie', 0.6);
      
      await storage.clear();
      
      const bobValue = await storage.getRecognition('alice', 'bob');
      const charlieValue = await storage.getRecognition('alice', 'charlie');
      
      expect(bobValue).toBeUndefined();
      expect(charlieValue).toBeUndefined();
    });
  });
});

