/**
 * SecureContext Tests
 * 
 * Test the core security primitive - signing, verification, and replay protection
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { SecureContext, SecureContextManager } from '../secure-context';
import { generateKeypair } from '../../identity/keypair';
import type { SignedStateUpdate } from '../../identity/signing';

describe('SecureContext', () => {
  let keypair: any;
  let ctx: SecureContext;

  beforeEach(async () => {
    keypair = await generateKeypair();
    ctx = await SecureContext.create(keypair, 'alice');
  });

  describe('Signing operations', () => {
    it('signs state updates with private key', async () => {
      const signed = await ctx.signUpdate('bob', 0.8);

      expect(signed).toBeDefined();
      expect(signed.signature).toBeDefined();
      expect(signed.publicKey).toBe(keypair.publicKey);
      expect(signed.from).toBe('alice');
      expect(signed.to).toBe('bob');
      expect(signed.value).toBe(0.8);
    });

    it('creates updates with nonces', async () => {
      const signed1 = await ctx.signUpdate('bob', 0.8);
      const signed2 = await ctx.signUpdate('bob', 0.8);

      expect(signed1.nonce).toBeDefined();
      expect(signed2.nonce).toBeDefined();
      expect(signed1.nonce).not.toBe(signed2.nonce);
    });

    it('rejects invalid keypairs', async () => {
      const invalidKeypair = { publicKey: '', privateKey: '' };

      await expect(
        SecureContext.create(invalidKeypair, 'alice')
      ).rejects.toThrow('Invalid keypair');
    });
  });

  describe('Verification', () => {
    it('verifies valid signatures', async () => {
      const signed = await ctx.signUpdate('bob', 0.8);
      const isValid = await ctx.verify(signed);

      expect(isValid).toBe(true);
    });

    it('rejects invalid signatures', async () => {
      const signed = await ctx.signUpdate('bob', 0.8);
      
      // Tamper with signature
      const tampered: SignedStateUpdate = {
        ...signed,
        signature: 'fake_signature_xyz'
      };

      const isValid = await ctx.verify(tampered);
      expect(isValid).toBe(false);
    });

    it('detects tampered data', async () => {
      const signed = await ctx.signUpdate('bob', 0.8);
      
      // Tamper with value (but keep original signature)
      const tampered: SignedStateUpdate = {
        ...signed,
        value: 0.1  // Changed from 0.8!
      };

      const isValid = await ctx.verify(tampered);
      expect(isValid).toBe(false);
    });
  });

  describe('Replay protection', () => {
    it('tracks nonces correctly', async () => {
      const signed = await ctx.signUpdate('bob', 0.8);
      
      // First verification should succeed
      const isValid1 = await ctx.verify(signed);
      expect(isValid1).toBe(true);
      
      // Second verification should fail (replay)
      const isValid2 = await ctx.verify(signed);
      expect(isValid2).toBe(false);
    });

    it('rejects replayed updates', async () => {
      const signed = await ctx.signUpdate('bob', 0.8);
      
      await ctx.verify(signed);  // First time: OK
      const isReplayed = await ctx.verify(signed);  // Second time: REJECT
      
      expect(isReplayed).toBe(false);
    });

    it('allows same update with different nonce', async () => {
      const signed1 = await ctx.signUpdate('bob', 0.8);
      const signed2 = await ctx.signUpdate('bob', 0.8);
      
      const isValid1 = await ctx.verify(signed1);
      const isValid2 = await ctx.verify(signed2);
      
      expect(isValid1).toBe(true);
      expect(isValid2).toBe(true);
    });
  });

  describe('Edge cases', () => {
    it('handles concurrent signing', async () => {
      const results = await Promise.all([
        ctx.signUpdate('bob', 0.8),
        ctx.signUpdate('charlie', 0.6),
        ctx.signUpdate('dave', 0.4)
      ]);

      expect(results).toHaveLength(3);
      expect(results[0].to).toBe('bob');
      expect(results[1].to).toBe('charlie');
      expect(results[2].to).toBe('dave');
      
      // All should have unique nonces
      const nonces = results.map(r => r.nonce);
      const uniqueNonces = new Set(nonces);
      expect(uniqueNonces.size).toBe(3);
    });

    it('exports keypair safely', () => {
      console.warn = vi.fn();  // Mock console.warn
      
      const exported = ctx.dangerouslyExportKeypair();
      
      expect(exported.publicKey).toBe(keypair.publicKey);
      expect(exported.privateKey).toBe(keypair.privateKey);
      expect(console.warn).toHaveBeenCalledWith(
        expect.stringContaining('SECURITY WARNING')
      );
    });

    it('provides public key safely', () => {
      const publicKey = ctx.getPublicKey();
      
      expect(publicKey).toBe(keypair.publicKey);
    });

    it('provides entity ID', () => {
      const entityId = ctx.getEntityId();
      
      expect(entityId).toBe('alice');
    });

    it('can reset nonces', async () => {
      const signed = await ctx.signUpdate('bob', 0.8);
      
      await ctx.verify(signed);  // Marks nonce as seen
      
      ctx.resetNonces();
      
      // Should be able to verify again after reset
      const isValid = await ctx.verify(signed);
      expect(isValid).toBe(true);
    });
  });

  describe('SecureContextManager', () => {
    beforeEach(() => {
      SecureContextManager.clear();
    });

    it('manages singleton contexts', async () => {
      const ctx1 = await SecureContextManager.getOrCreate('alice', keypair);
      const ctx2 = await SecureContextManager.getOrCreate('alice', keypair);
      
      expect(ctx1).toBe(ctx2);  // Same instance
    });

    it('creates different contexts for different entities', async () => {
      const keypair2 = await generateKeypair();
      
      const ctx1 = await SecureContextManager.getOrCreate('alice', keypair);
      const ctx2 = await SecureContextManager.getOrCreate('bob', keypair2);
      
      expect(ctx1).not.toBe(ctx2);  // Different instances
    });

    it('retrieves existing context', async () => {
      await SecureContextManager.getOrCreate('alice', keypair);
      
      const ctx = SecureContextManager.get('alice');
      
      expect(ctx).toBeDefined();
      expect(ctx?.getEntityId()).toBe('alice');
    });

    it('removes context', async () => {
      await SecureContextManager.getOrCreate('alice', keypair);
      
      SecureContextManager.remove('alice');
      
      const ctx = SecureContextManager.get('alice');
      expect(ctx).toBeUndefined();
    });

    it('clears all contexts', async () => {
      const keypair2 = await generateKeypair();
      
      await SecureContextManager.getOrCreate('alice', keypair);
      await SecureContextManager.getOrCreate('bob', keypair2);
      
      SecureContextManager.clear();
      
      expect(SecureContextManager.get('alice')).toBeUndefined();
      expect(SecureContextManager.get('bob')).toBeUndefined();
    });
  });
});

