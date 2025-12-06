/**
 * Security Integration Guide
 * 
 * Shows how to integrate SecureContext at every layer.
 * Copy this pattern for your implementation!
 */

import { EntitySession } from '../entity-session';
import { SecureContext, SecureContextManager } from './secure-context';
import { SecureStorage } from './secure-storage';
import type { KeyPair } from '../identity/keypair';
import type { EntityId } from '../types';
import type { SignedStateUpdate } from '../identity/signing';

// ============================================================================
// LAYER 1: EntitySession Integration
// ============================================================================

/**
 * Secure Entity Session - EntitySession with Built-In Signing
 * 
 * This extends EntitySession to automatically sign all operations.
 * 
 * @example
 * const session = await SecureEntitySession.create('alice', keypair);
 * 
 * // This automatically signs the update!
 * await session.allocateRecognition('bob', 0.8);
 */
export class SecureEntitySession extends EntitySession {
  private secureContext!: SecureContext;
  private secureStorage!: SecureStorage;

  private constructor(config: any) {
    super(config);
  }

  /**
   * Create a secure session
   * 
   * This is THE recommended way to create sessions!
   * 
   * @example
   * const session = await SecureEntitySession.create('alice', keypair);
   */
  static async create(
    entityId: EntityId,
    keypair: KeyPair
  ): Promise<SecureEntitySession> {
    // Create secure context
    const secureContext = await SecureContext.create(keypair, entityId);
    
    // Create secure storage
    const secureStorage = new SecureStorage(entityId, secureContext);
    await secureStorage.initialize();
    
    // Create session
    const session = new SecureEntitySession({ entityId });
    session.secureContext = secureContext;
    session.secureStorage = secureStorage;
    
    await session.initialize();
    
    return session;
  }

  /**
   * Allocate recognition (AUTOMATICALLY SIGNED)
   * 
   * Override to use SecureStorage instead of regular storage.
   */
  async allocateRecognition(to: EntityId, amount: number): Promise<void> {
    // Validate amount
    if (amount < 0 || amount > 1) {
      throw new Error(`Invalid amount: ${amount} (must be 0-1)`);
    }

    // Store with automatic signing
    await this.secureStorage.storeRecognition(to, amount);
    
    // Broadcast to peers (also signed)
    await this.broadcastSignedUpdate(to, amount);
    
    // Clear cache
    this.invalidateCache(to);
  }

  /**
   * Get recognition (AUTOMATICALLY VERIFIED)
   */
  async getRecognition(from: EntityId, to: EntityId): Promise<number> {
    // Try cache first
    const cached = this.getCached(from, to);
    if (cached !== undefined) {
      return cached;
    }

    // Get from secure storage (auto-verifies)
    const value = await this.secureStorage.getRecognition(from, to);
    
    if (value === undefined) {
      return 0;  // Default
    }

    // Cache the verified value
    this.cache(from, to, value);
    
    return value;
  }

  /**
   * Broadcast signed update to replicas
   */
  private async broadcastSignedUpdate(to: EntityId, amount: number): Promise<void> {
    // Create signed update
    const signedUpdate = await this.secureContext.signUpdate(to, amount);
    
    // Send to all connected peers/replicas
    // (Transport layer will handle additional message signing)
    for (const transport of this.getTransports()) {
      try {
        await transport.send('storeSignedUpdate', [signedUpdate]);
      } catch (error) {
        console.error('Failed to broadcast to transport:', error);
      }
    }
  }

  /**
   * Receive and verify signed update from peer
   */
  async receiveSignedUpdate(signedUpdate: SignedStateUpdate): Promise<void> {
    // Verify signature
    const isValid = await this.secureContext.verify(signedUpdate);
    
    if (!isValid) {
      console.error('Received invalid signed update!', signedUpdate);
      return;
    }

    // Only store updates FROM this entity
    if (signedUpdate.from !== this.entityId) {
      console.error('Received update not from this entity!');
      return;
    }

    // Store (already signed)
    await this.secureStorage.storeRecognition(signedUpdate.to, signedUpdate.value);
    
    // Invalidate cache
    this.invalidateCache(signedUpdate.to);
  }

  /**
   * Export state for backup/sync (all signed!)
   */
  async exportSignedState(): Promise<SignedStateUpdate[]> {
    return await this.secureStorage.exportSignedUpdates();
  }

  /**
   * Import state (auto-verifies all signatures!)
   */
  async importSignedState(updates: SignedStateUpdate[]): Promise<void> {
    const result = await this.secureStorage.importSignedUpdates(updates);
    console.log(`Imported ${result.imported} updates, rejected ${result.rejected} invalid`);
  }

  // Helper methods (simplified)
  private getCached(_from: EntityId, _to: EntityId): number | undefined {
    return undefined; // Implement with actual cache
  }

  private cache(_from: EntityId, _to: EntityId, _value: number): void {
    // Implement with actual cache
  }

  private invalidateCache(_to: EntityId): void {
    // Implement with actual cache
  }

  private getTransports(): any[] {
    return []; // Get from parent class
  }
}

// ============================================================================
// LAYER 2: Transport Integration
// ============================================================================

/**
 * Secure Transport Base Class
 * 
 * Automatically signs all outgoing messages and verifies incoming.
 */
export abstract class SecureTransport {
  protected secureContext: SecureContext;

  constructor(secureContext: SecureContext) {
    this.secureContext = secureContext;
  }

  /**
   * Send a message (AUTOMATICALLY SIGNED)
   */
  async send(method: string, params: any[]): Promise<void> {
    // Create message
    const message = {
      method,
      params,
      timestamp: Date.now(),
      nonce: crypto.randomUUID()
    };

    // Sign the entire message
    const signature = await this.secureContext.signData(message);

    // Send signed message
    const signedMessage = {
      ...message,
      signature,
      publicKey: this.secureContext.getPublicKey()
    };

    await this.sendRaw(JSON.stringify(signedMessage));
  }

  /**
   * Receive a message (AUTOMATICALLY VERIFIED)
   */
  async receive(rawMessage: string): Promise<any> {
    const signedMessage = JSON.parse(rawMessage);

    // Extract signature and message
    const { signature, publicKey, ...message } = signedMessage;

    // Verify signature
    const isValid = await this.verifyMessageSignature(message, signature, publicKey);

    if (!isValid) {
      console.error('Invalid message signature!', signedMessage);
      throw new Error('Message signature verification failed');
    }

    // Process verified message
    return this.handleVerifiedMessage(message);
  }

  /**
   * Verify message signature
   */
  private async verifyMessageSignature(
    message: any,
    signature: string,
    publicKey: string
  ): Promise<boolean> {
    const enc = new TextEncoder();
    const dataBytes = enc.encode(JSON.stringify(message));
    
    try {
      const publicKeyJwk = JSON.parse(publicKey);
      const cryptoKey = await crypto.subtle.importKey(
        'jwk',
        publicKeyJwk,
        { name: 'Ed25519' } as any,
        true,
        ['verify']
      );
      
      const signatureBytes = Uint8Array.from(atob(signature), c => c.charCodeAt(0));
      
      return await crypto.subtle.verify(
        { name: 'Ed25519' } as any,
        cryptoKey,
        signatureBytes,
        dataBytes
      );
    } catch (error) {
      console.error('Signature verification error:', error);
      return false;
    }
  }

  /**
   * Abstract methods to implement per transport
   */
  protected abstract sendRaw(message: string): Promise<void>;
  protected abstract handleVerifiedMessage(message: any): Promise<any>;
}

// ============================================================================
// LAYER 3: State Restoration Integration
// ============================================================================

/**
 * Restore state with automatic verification
 * 
 * This wraps the login flow to verify all signatures.
 */
export async function secureLogin(
  email: string,
  password: string
): Promise<SecureEntitySession> {
  // Derive keypair from password
  const { deriveKeypair } = await import('../identity/keypair');
  const keypair = await deriveKeypair(password, email);

  // Derive entity ID from public key
  const entityId = deriveEntityId(keypair.publicKey);

  // Fetch state fragments from replicas
  const fragments = await fetchFragmentsFromReplicas(keypair.publicKey);

  // Create secure context for verification
  const secureContext = await SecureContext.create(keypair, entityId);

  // Verify ALL updates
  const verifiedUpdates: SignedStateUpdate[] = [];
  let rejectedCount = 0;

  for (const fragment of fragments) {
    for (const update of fragment.signedUpdates) {
      const isValid = await secureContext.verify(update);
      
      if (isValid) {
        verifiedUpdates.push(update);
      } else {
        console.error('Rejected invalid update:', update);
        rejectedCount++;
      }
    }
  }

  console.log(`Verified ${verifiedUpdates.length} updates, rejected ${rejectedCount}`);

  // Create secure session
  const session = await SecureEntitySession.create(entityId, keypair);

  // Import only verified updates
  await session.importSignedState(verifiedUpdates);

  return session;
}

// ============================================================================
// FACTORY FUNCTIONS - ONE-LINE CREATION
// ============================================================================

/**
 * Create secure session from password
 * 
 * This is THE recommended way to create sessions!
 * 
 * @example
 * const session = await createSecureSession('alice@example.com', 'password');
 * // Everything is automatically signed and verified!
 */
export async function createSecureSession(
  email: string,
  password: string
): Promise<SecureEntitySession> {
  return await secureLogin(email, password);
}

/**
 * Create secure session with existing keypair
 */
export async function createSecureSessionWithKeypair(
  entityId: EntityId,
  keypair: KeyPair
): Promise<SecureEntitySession> {
  return await SecureEntitySession.create(entityId, keypair);
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

function deriveEntityId(publicKey: string): EntityId {
  // Derive entity ID from public key
  // (Simplified - implement proper derivation)
  return publicKey.slice(0, 16);
}

async function fetchFragmentsFromReplicas(_publicKey: string): Promise<any[]> {
  // Fetch state fragments from replicas
  // (Simplified - implement actual discovery and fetch)
  return [];
}

