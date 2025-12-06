/**
 * Secure Context - Low-Level Security Primitive
 * 
 * This is the foundational security layer that holds the keypair
 * and provides automatic signing/verification for the entire system.
 * 
 * ALL security flows through this single abstraction!
 */

import type { KeyPair } from '../identity/keypair';
import type { EntityId } from '../types';
import {
  signStateUpdate,
  verifySignedUpdate,
  verifyUpdateFrom,
  createStateUpdate,
  NonceTracker,
  type StateUpdate,
  type SignedStateUpdate
} from '../identity/signing';

/**
 * Secure Context - The Security Root
 * 
 * This holds your private key and provides signing primitives.
 * NEVER expose the private key directly!
 * 
 * @example
 * const ctx = await SecureContext.create(keypair, 'alice');
 * 
 * // Automatically creates and signs updates
 * const signed = await ctx.signUpdate('bob', 0.8);
 * 
 * // Verifies incoming updates
 * const isValid = await ctx.verify(signedUpdate);
 */
export class SecureContext {
  private keypair: KeyPair;
  private entityId: EntityId;
  private nonceTracker: NonceTracker;

  private constructor(keypair: KeyPair, entityId: EntityId) {
    this.keypair = keypair;
    this.entityId = entityId;
    this.nonceTracker = new NonceTracker();
  }

  /**
   * Create a SecureContext
   * 
   * This is the ONLY way to create one - ensures keypair is validated.
   */
  static async create(keypair: KeyPair, entityId: EntityId): Promise<SecureContext> {
    // Validate keypair
    if (!keypair.publicKey || !keypair.privateKey) {
      throw new Error('Invalid keypair: missing keys');
    }

    return new SecureContext(keypair, entityId);
  }

  /**
   * Create and sign a state update
   * 
   * This is the PRIMARY method for creating signed updates.
   * Use this everywhere instead of manual signing!
   * 
   * @example
   * const signed = await ctx.signUpdate('bob', 0.8);
   * // Automatically includes: signature, nonce, timestamp, publicKey
   */
  async signUpdate(to: EntityId, value: number): Promise<SignedStateUpdate> {
    const update = createStateUpdate(this.entityId, to, value);
    return await signStateUpdate(update, this.keypair.privateKey, this.keypair.publicKey);
  }

  /**
   * Sign an arbitrary update (advanced)
   */
  async signCustomUpdate(update: StateUpdate): Promise<SignedStateUpdate> {
    return await signStateUpdate(update, this.keypair.privateKey, this.keypair.publicKey);
  }

  /**
   * Verify a signed update
   * 
   * Checks:
   * - Signature is valid
   * - Not a replay (nonce tracking)
   * 
   * @example
   * const isValid = await ctx.verify(signedUpdate);
   * if (!isValid) {
   *   throw new Error('Invalid or replayed update!');
   * }
   */
  async verify(signedUpdate: SignedStateUpdate): Promise<boolean> {
    // Check signature
    const isValidSignature = await verifySignedUpdate(signedUpdate);
    if (!isValidSignature) {
      return false;
    }

    // Check for replay
    const isNotReplayed = this.nonceTracker.verifyNotReplayed(signedUpdate);
    if (!isNotReplayed) {
      console.error('Replay attack detected:', signedUpdate);
      return false;
    }

    return true;
  }

  /**
   * Verify update is from a specific entity
   */
  async verifyFrom(signedUpdate: SignedStateUpdate, expectedPublicKey: string): Promise<boolean> {
    // First verify signature
    if (!await this.verify(signedUpdate)) {
      return false;
    }

    // Then check it's from expected entity
    return await verifyUpdateFrom(signedUpdate, expectedPublicKey);
  }

  /**
   * Sign arbitrary data (for authentication challenges, etc.)
   */
  async signData(data: any): Promise<string> {
    const enc = new TextEncoder();
    const dataBytes = enc.encode(JSON.stringify(data));
    
    const privateKeyJwk = JSON.parse(this.keypair.privateKey);
    const cryptoKey = await crypto.subtle.importKey(
      'jwk',
      privateKeyJwk,
      { name: 'Ed25519' } as any,
      true,
      ['sign']
    );
    
    const signatureBytes = await crypto.subtle.sign(
      { name: 'Ed25519' } as any,
      cryptoKey,
      dataBytes
    );
    
    return btoa(String.fromCharCode(...new Uint8Array(signatureBytes)));
  }

  /**
   * Get public key (safe to expose)
   */
  getPublicKey(): string {
    return this.keypair.publicKey;
  }

  /**
   * Get entity ID
   */
  getEntityId(): EntityId {
    return this.entityId;
  }

  /**
   * Reset nonce tracker (useful for testing)
   */
  resetNonces(): void {
    this.nonceTracker.clear();
  }

  /**
   * DANGEROUS: Export keypair
   * 
   * Only use for backup/migration!
   * Never transmit the private key!
   */
  dangerouslyExportKeypair(): KeyPair {
    console.warn('⚠️ SECURITY WARNING: Private key exported!');
    return { ...this.keypair };
  }
}

/**
 * Secure Context Manager - Singleton Pattern
 * 
 * Ensures only one SecureContext exists per entity session.
 */
export class SecureContextManager {
  private static contexts = new Map<EntityId, SecureContext>();

  /**
   * Get or create a secure context
   */
  static async getOrCreate(
    entityId: EntityId,
    keypair: KeyPair
  ): Promise<SecureContext> {
    if (!this.contexts.has(entityId)) {
      const ctx = await SecureContext.create(keypair, entityId);
      this.contexts.set(entityId, ctx);
    }
    return this.contexts.get(entityId)!;
  }

  /**
   * Get existing context
   */
  static get(entityId: EntityId): SecureContext | undefined {
    return this.contexts.get(entityId);
  }

  /**
   * Remove context (on logout)
   */
  static remove(entityId: EntityId): void {
    this.contexts.delete(entityId);
  }

  /**
   * Clear all contexts
   */
  static clear(): void {
    this.contexts.clear();
  }
}

