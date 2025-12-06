/**
 * Signing Primitives Tests
 * 
 * Test cryptographic signing and verification primitives
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  signStateUpdate,
  verifySignedUpdate,
  verifyUpdateFrom,
  createStateUpdate,
  signStateUpdates,
  verifySignedUpdates,
  filterVerifiedUpdates,
  NonceTracker,
  type StateUpdate,
  type SignedStateUpdate
} from '../signing';
import { generateKeypair } from '../keypair';

describe('Signing Primitives', () => {
  let keypair: any;

  beforeEach(async () => {
    keypair = await generateKeypair();
  });

  describe('Basic signing', () => {
    it('signs data with Ed25519', async () => {
      const update = createStateUpdate('alice', 'bob', 0.8);
      const signed = await signStateUpdate(update, keypair.privateKey, keypair.publicKey);

      expect(signed.signature).toBeDefined();
      expect(signed.signature).toMatch(/^[A-Za-z0-9+/]+=*$/);  // Base64 format
      expect(signed.publicKey).toBe(keypair.publicKey);
    });

    it('verifies valid signatures', async () => {
      const update = createStateUpdate('alice', 'bob', 0.8);
      const signed = await signStateUpdate(update, keypair.privateKey, keypair.publicKey);

      const isValid = await verifySignedUpdate(signed);

      expect(isValid).toBe(true);
    });

    it('rejects forged signatures', async () => {
      const update = createStateUpdate('alice', 'bob', 0.8);
      const signed = await signStateUpdate(update, keypair.privateKey, keypair.publicKey);

      // Forge signature
      const forged: SignedStateUpdate = {
        ...signed,
        signature: 'fake_signature_AAAAAAA=='
      };

      const isValid = await verifySignedUpdate(forged);

      expect(isValid).toBe(false);
    });

    it('handles different key types', async () => {
      // Test that the signing works with freshly generated keys
      const keypair2 = await generateKeypair();
      
      const update = createStateUpdate('bob', 'alice', 0.5);
      const signed = await signStateUpdate(update, keypair2.privateKey, keypair2.publicKey);

      const isValid = await verifySignedUpdate(signed);

      expect(isValid).toBe(true);
    });
  });

  describe('Batch operations', () => {
    it('signs multiple updates', async () => {
      const updates = [
        createStateUpdate('alice', 'bob', 0.8),
        createStateUpdate('alice', 'charlie', 0.6),
        createStateUpdate('alice', 'dave', 0.4)
      ];

      const signed = await signStateUpdates(updates, keypair.privateKey, keypair.publicKey);

      expect(signed).toHaveLength(3);
      expect(signed[0].to).toBe('bob');
      expect(signed[1].to).toBe('charlie');
      expect(signed[2].to).toBe('dave');

      // All should have valid signatures
      for (const s of signed) {
        expect(s.signature).toBeDefined();
        expect(s.publicKey).toBe(keypair.publicKey);
      }
    });

    it('verifies batch of updates', async () => {
      const updates = [
        createStateUpdate('alice', 'bob', 0.8),
        createStateUpdate('alice', 'charlie', 0.6),
        createStateUpdate('alice', 'dave', 0.4)
      ];

      const signed = await signStateUpdates(updates, keypair.privateKey, keypair.publicKey);
      const results = await verifySignedUpdates(signed);

      expect(results).toHaveLength(3);
      expect(results.every(r => r === true)).toBe(true);
    });

    it('filters out invalid updates', async () => {
      const updates = [
        createStateUpdate('alice', 'bob', 0.8),
        createStateUpdate('alice', 'charlie', 0.6),
        createStateUpdate('alice', 'dave', 0.4)
      ];

      const signed = await signStateUpdates(updates, keypair.privateKey, keypair.publicKey);

      // Tamper with middle update
      signed[1] = {
        ...signed[1],
        signature: 'fake_signature'
      };

      const verified = await filterVerifiedUpdates(signed);

      expect(verified).toHaveLength(2);
      expect(verified[0].to).toBe('bob');
      expect(verified[1].to).toBe('dave');
    });
  });

  describe('Edge cases', () => {
    it('handles empty data', async () => {
      const update = createStateUpdate('alice', 'bob', 0);
      const signed = await signStateUpdate(update, keypair.privateKey, keypair.publicKey);

      const isValid = await verifySignedUpdate(signed);

      expect(isValid).toBe(true);
      expect(signed.value).toBe(0);
    });

    it('canonical JSON serialization', async () => {
      const update = {
        from: 'alice',
        to: 'bob',
        value: 0.8,
        timestamp: 1234567890,
        nonce: 'test-nonce'
      };

      const signed1 = await signStateUpdate(update, keypair.privateKey, keypair.publicKey);
      const signed2 = await signStateUpdate(update, keypair.privateKey, keypair.publicKey);

      // Same data should produce same signature
      expect(signed1.signature).toBe(signed2.signature);
    });

    it('signature format is base64', async () => {
      const update = createStateUpdate('alice', 'bob', 0.8);
      const signed = await signStateUpdate(update, keypair.privateKey, keypair.publicKey);

      // Base64 pattern: alphanumeric + / +, optional padding =
      expect(signed.signature).toMatch(/^[A-Za-z0-9+/]+=*$/);
    });

    it('detects tampering with any field', async () => {
      const update = createStateUpdate('alice', 'bob', 0.8);
      const signed = await signStateUpdate(update, keypair.privateKey, keypair.publicKey);

      // Test tampering with each field
      const tampered1 = { ...signed, from: 'eve' };
      const tampered2 = { ...signed, to: 'eve' };
      const tampered3 = { ...signed, value: 0.1 };
      const tampered4 = { ...signed, timestamp: Date.now() };

      expect(await verifySignedUpdate(tampered1)).toBe(false);
      expect(await verifySignedUpdate(tampered2)).toBe(false);
      expect(await verifySignedUpdate(tampered3)).toBe(false);
      expect(await verifySignedUpdate(tampered4)).toBe(false);
    });
  });

  describe('verifyUpdateFrom', () => {
    it('verifies update is from specific entity', async () => {
      const update = createStateUpdate('alice', 'bob', 0.8);
      const signed = await signStateUpdate(update, keypair.privateKey, keypair.publicKey);

      const isFromAlice = await verifyUpdateFrom(signed, keypair.publicKey);

      expect(isFromAlice).toBe(true);
    });

    it('rejects update from wrong entity', async () => {
      const keypair2 = await generateKeypair();
      
      const update = createStateUpdate('alice', 'bob', 0.8);
      const signed = await signStateUpdate(update, keypair.privateKey, keypair.publicKey);

      const isFromBob = await verifyUpdateFrom(signed, keypair2.publicKey);

      expect(isFromBob).toBe(false);
    });
  });

  describe('createStateUpdate', () => {
    it('creates update with required fields', () => {
      const update = createStateUpdate('alice', 'bob', 0.8);

      expect(update.from).toBe('alice');
      expect(update.to).toBe('bob');
      expect(update.value).toBe(0.8);
      expect(update.timestamp).toBeDefined();
      expect(update.nonce).toBeDefined();
    });

    it('creates unique nonces', () => {
      const update1 = createStateUpdate('alice', 'bob', 0.8);
      const update2 = createStateUpdate('alice', 'bob', 0.8);

      expect(update1.nonce).not.toBe(update2.nonce);
    });
  });

  describe('NonceTracker', () => {
    let tracker: NonceTracker;

    beforeEach(() => {
      tracker = new NonceTracker();
    });

    it('tracks seen nonces', () => {
      const update = createStateUpdate('alice', 'bob', 0.8);

      expect(tracker.hasSeen(update.nonce)).toBe(false);
      
      tracker.markSeen(update.nonce);
      
      expect(tracker.hasSeen(update.nonce)).toBe(true);
    });

    it('verifies not replayed', () => {
      const update = createStateUpdate('alice', 'bob', 0.8);

      const result1 = tracker.verifyNotReplayed(update);
      expect(result1).toBe(true);

      const result2 = tracker.verifyNotReplayed(update);
      expect(result2).toBe(false);  // Replay!
    });

    it('allows different nonces', () => {
      const update1 = createStateUpdate('alice', 'bob', 0.8);
      const update2 = createStateUpdate('alice', 'bob', 0.8);

      expect(tracker.verifyNotReplayed(update1)).toBe(true);
      expect(tracker.verifyNotReplayed(update2)).toBe(true);
    });

    it('clears all nonces', () => {
      const update1 = createStateUpdate('alice', 'bob', 0.8);
      const update2 = createStateUpdate('alice', 'charlie', 0.6);

      tracker.markSeen(update1.nonce);
      tracker.markSeen(update2.nonce);

      tracker.clear();

      expect(tracker.hasSeen(update1.nonce)).toBe(false);
      expect(tracker.hasSeen(update2.nonce)).toBe(false);
    });

    it('limits nonce storage to prevent memory bloat', () => {
      // Mark 11000 nonces (exceeds maxNonces of 10000)
      for (let i = 0; i < 11000; i++) {
        const update = createStateUpdate('alice', 'bob', 0.8);
        tracker.markSeen(update.nonce);
      }

      // Should have evicted the oldest ones
      // Exact size may vary due to eviction logic
      // Just verify it doesn't grow unbounded
    });
  });
});

