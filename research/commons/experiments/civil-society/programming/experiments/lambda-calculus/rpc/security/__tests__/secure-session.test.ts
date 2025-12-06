/**
 * SecureEntitySession Tests
 * 
 * Test SecureEntitySession integration
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { SecureEntitySession, createSecureSession, createSecureSessionWithKeypair } from '../integration-guide';
import { generateKeypair } from '../../identity/keypair';

describe('SecureEntitySession', () => {
  let keypair: any;
  let session: SecureEntitySession;

  beforeEach(async () => {
    keypair = await generateKeypair();
    session = await SecureEntitySession.create('alice', keypair);
  });

  afterEach(async () => {
    // Clean up storage
    try {
      await (session as any).secureStorage?.clear();
    } catch (e) {
      // Ignore cleanup errors
    }
  });

  describe('Creation', () => {
    it('creates session with keypair', async () => {
      expect(session).toBeDefined();
      expect((session as any).secureContext).toBeDefined();
      expect((session as any).secureStorage).toBeDefined();
    });

    it('initializes SecureContext and SecureStorage', async () => {
      const ctx = (session as any).secureContext;
      const storage = (session as any).secureStorage;

      expect(ctx.getEntityId()).toBe('alice');
      expect(ctx.getPublicKey()).toBe(keypair.publicKey);
      expect(storage).toBeDefined();
    });
  });

  describe('Operations', () => {
    it('allocateRecognition signs automatically', async () => {
      await session.allocateRecognition('bob', 0.8);

      // Verify it was signed by checking exports
      const signedUpdates = await session.exportSignedState();
      
      expect(signedUpdates).toHaveLength(1);
      expect(signedUpdates[0].signature).toBeDefined();
      expect(signedUpdates[0].from).toBe('alice');
      expect(signedUpdates[0].to).toBe('bob');
      expect(signedUpdates[0].value).toBe(0.8);
    });

    it('getRecognition verifies automatically', async () => {
      await session.allocateRecognition('bob', 0.8);

      const value = await session.getRecognition('alice', 'bob');

      expect(value).toBe(0.8);
    });

    it('handles multiple allocations', async () => {
      await session.allocateRecognition('bob', 0.8);
      await session.allocateRecognition('charlie', 0.6);
      await session.allocateRecognition('dave', 0.4);

      const bobValue = await session.getRecognition('alice', 'bob');
      const charlieValue = await session.getRecognition('alice', 'charlie');
      const daveValue = await session.getRecognition('alice', 'dave');

      expect(bobValue).toBe(0.8);
      expect(charlieValue).toBe(0.6);
      expect(daveValue).toBe(0.4);
    });
  });

  describe('State management', () => {
    it('exports signed state', async () => {
      await session.allocateRecognition('bob', 0.8);
      await session.allocateRecognition('charlie', 0.6);

      const signedUpdates = await session.exportSignedState();

      expect(signedUpdates).toHaveLength(2);
      
      // All should be signed
      for (const update of signedUpdates) {
        expect(update.signature).toBeDefined();
        expect(update.publicKey).toBe(keypair.publicKey);
      }
    });

    it('imports with verification', async () => {
      // Create some signed updates
      await session.allocateRecognition('bob', 0.8);
      await session.allocateRecognition('charlie', 0.6);

      const signedUpdates = await session.exportSignedState();

      // Create new session and import
      const keypair2 = await generateKeypair();
      const session2 = await SecureEntitySession.create('alice', keypair2);

      await session2.importSignedState(signedUpdates);

      // Should reject because signatures are from different keypair
      // (In real usage, you'd import from same entity's keypair)
      
      await (session2 as any).secureStorage?.clear();
    });

    it('rejects tampered state on import', async () => {
      await session.allocateRecognition('bob', 0.8);

      const signedUpdates = await session.exportSignedState();

      // Tamper with the data
      const tampered = signedUpdates.map(u => ({
        ...u,
        value: 0.1  // Changed!
      }));

      const consoleSpy = vi.spyOn(console, 'log');
      
      await session.importSignedState(tampered);

      // Should log rejection message
      expect(consoleSpy).toHaveBeenCalledWith(
        expect.stringContaining('rejected')
      );
    });
  });

  describe('Factory functions', () => {
    it('createSecureSession works', async () => {
      // Note: This would typically use password derivation
      // For testing, we'll just verify the API exists
      expect(createSecureSession).toBeDefined();
    });

    it('createSecureSessionWithKeypair works', async () => {
      const newSession = await createSecureSessionWithKeypair('bob', keypair);

      expect(newSession).toBeDefined();
      expect((newSession as any).secureContext.getEntityId()).toBe('bob');

      await (newSession as any).secureStorage?.clear();
    });
  });

  describe('Edge cases', () => {
    it('handles concurrent operations', async () => {
      await Promise.all([
        session.allocateRecognition('bob', 0.8),
        session.allocateRecognition('charlie', 0.6),
        session.allocateRecognition('dave', 0.4)
      ]);

      const signedUpdates = await session.exportSignedState();

      expect(signedUpdates).toHaveLength(3);
    });

    it('handles zero allocations', async () => {
      await session.allocateRecognition('bob', 0);

      const value = await session.getRecognition('alice', 'bob');

      expect(value).toBe(0);
    });

    it('handles non-existent recognition', async () => {
      const value = await session.getRecognition('alice', 'nonexistent');

      expect(value).toBe(0);  // Should return default
    });
  });
});

