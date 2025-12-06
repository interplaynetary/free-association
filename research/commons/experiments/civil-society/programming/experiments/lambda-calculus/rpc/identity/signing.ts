/**
 * Cryptographic Signing for State Updates
 * 
 * Ensures state updates are:
 * - Authentic (really from the entity)
 * - Unmodified (integrity)
 * - Non-repudiable (can't deny)
 * - Replay-protected (nonces)
 */

import type { EntityId } from '../types';

/**
 * State update that needs to be signed
 */
export interface StateUpdate {
  from: EntityId;
  to: EntityId;
  value: number;
  timestamp: number;
  nonce: string;  // Prevents replay attacks
}

/**
 * Cryptographically signed state update
 */
export interface SignedStateUpdate extends StateUpdate {
  signature: string;   // Base64-encoded signature
  publicKey: string;   // Signer's public key (for verification)
}

/**
 * Sign a state update with a private key
 * 
 * @example
 * const update = {
 *   from: 'alice',
 *   to: 'bob',
 *   value: 0.8,
 *   timestamp: Date.now(),
 *   nonce: crypto.randomUUID()
 * };
 * 
 * const signed = await signStateUpdate(update, privateKey, publicKey);
 * // Now signed.signature cryptographically proves Alice made this update
 */
export async function signStateUpdate(
  update: StateUpdate,
  privateKey: string,
  publicKey: string
): Promise<SignedStateUpdate> {
  const signature = await signData(update, privateKey);
  
  return {
    ...update,
    signature,
    publicKey
  };
}

/**
 * Verify a signed state update
 * 
 * Returns true if:
 * - Signature is valid
 * - Update hasn't been modified
 * - Signer's public key matches
 * 
 * @example
 * const isValid = await verifySignedUpdate(signedUpdate);
 * if (!isValid) {
 *   console.error('FORGED UPDATE DETECTED!');
 *   blacklistReplica(replicaId);
 * }
 */
export async function verifySignedUpdate(
  signedUpdate: SignedStateUpdate
): Promise<boolean> {
  const { signature, publicKey, ...update } = signedUpdate;
  
  try {
    return await verifyData(update, signature, publicKey);
  } catch (error) {
    console.error('Signature verification error:', error);
    return false;
  }
}

/**
 * Verify that a specific entity signed this update
 * 
 * Use when you know who SHOULD have signed it.
 * 
 * @example
 * // Alice's recognition allocation should be signed by Alice
 * const isFromAlice = await verifyUpdateFrom(update, alicePublicKey);
 */
export async function verifyUpdateFrom(
  signedUpdate: SignedStateUpdate,
  expectedPublicKey: string
): Promise<boolean> {
  // Check public key matches
  if (signedUpdate.publicKey !== expectedPublicKey) {
    console.error('Public key mismatch!');
    return false;
  }
  
  // Check signature is valid
  return await verifySignedUpdate(signedUpdate);
}

/**
 * Create a state update with automatic nonce
 */
export function createStateUpdate(
  from: EntityId,
  to: EntityId,
  value: number
): StateUpdate {
  return {
    from,
    to,
    value,
    timestamp: Date.now(),
    nonce: crypto.randomUUID()
  };
}

/**
 * Batch sign multiple updates
 * 
 * Useful for initial sync or bulk operations.
 */
export async function signStateUpdates(
  updates: StateUpdate[],
  privateKey: string,
  publicKey: string
): Promise<SignedStateUpdate[]> {
  return await Promise.all(
    updates.map(update => signStateUpdate(update, privateKey, publicKey))
  );
}

/**
 * Batch verify multiple updates
 * 
 * Returns array of booleans (true = valid, false = invalid)
 */
export async function verifySignedUpdates(
  signedUpdates: SignedStateUpdate[]
): Promise<boolean[]> {
  return await Promise.all(
    signedUpdates.map(update => verifySignedUpdate(update))
  );
}

/**
 * Filter out invalid updates
 * 
 * Only returns cryptographically verified updates.
 * 
 * @example
 * const fragments = await fetchFromReplicas();
 * const verified = await filterVerifiedUpdates(fragments);
 * // Now we KNOW these updates are authentic!
 */
export async function filterVerifiedUpdates(
  signedUpdates: SignedStateUpdate[]
): Promise<SignedStateUpdate[]> {
  const verifications = await verifySignedUpdates(signedUpdates);
  
  return signedUpdates.filter((_, i) => verifications[i]);
}

// ============================================================================
// Low-Level Signing Primitives
// ============================================================================

/**
 * Sign arbitrary data with a private key
 * @private
 */
async function signData(data: any, privateKey: string): Promise<string> {
  const enc = new TextEncoder();
  const dataBytes = enc.encode(canonicalStringify(data));
  
  // Import private key
  const privateKeyJwk = JSON.parse(privateKey);
  const cryptoKey = await crypto.subtle.importKey(
    'jwk',
    privateKeyJwk,
    { name: 'Ed25519' } as any,  // Ed25519 not in all TS versions yet
    true,
    ['sign']
  );
  
  // Sign the data
  const signatureBytes = await crypto.subtle.sign(
    { name: 'Ed25519' } as any,
    cryptoKey,
    dataBytes
  );
  
  // Return base64-encoded signature
  return btoa(String.fromCharCode(...new Uint8Array(signatureBytes)));
}

/**
 * Verify a signature on arbitrary data
 * @private
 */
async function verifyData(
  data: any,
  signature: string,
  publicKey: string
): Promise<boolean> {
  const enc = new TextEncoder();
  const dataBytes = enc.encode(canonicalStringify(data));
  
  // Import public key
  const publicKeyJwk = JSON.parse(publicKey);
  const cryptoKey = await crypto.subtle.importKey(
    'jwk',
    publicKeyJwk,
    { name: 'Ed25519' } as any,
    true,
    ['verify']
  );
  
  // Decode signature from base64
  const signatureBytes = Uint8Array.from(atob(signature), c => c.charCodeAt(0));
  
  // Verify the signature
  return await crypto.subtle.verify(
    { name: 'Ed25519' } as any,
    cryptoKey,
    signatureBytes,
    dataBytes
  );
}

/**
 * Canonical JSON stringify
 * 
 * Ensures consistent byte representation for signing.
 * Sorts object keys alphabetically.
 * 
 * @private
 */
function canonicalStringify(obj: any): string {
  if (obj === null || obj === undefined) {
    return JSON.stringify(obj);
  }
  
  if (typeof obj !== 'object') {
    return JSON.stringify(obj);
  }
  
  if (Array.isArray(obj)) {
    return '[' + obj.map(canonicalStringify).join(',') + ']';
  }
  
  // Sort keys for canonical representation
  const keys = Object.keys(obj).sort();
  const pairs = keys.map(key => {
    return JSON.stringify(key) + ':' + canonicalStringify(obj[key]);
  });
  
  return '{' + pairs.join(',') + '}';
}

// ============================================================================
// Replay Attack Protection
// ============================================================================

/**
 * Nonce tracker to prevent replay attacks
 * 
 * In production, this should be persisted to storage.
 */
export class NonceTracker {
  private seenNonces = new Set<string>();
  private maxNonces = 10000;  // Prevent memory bloat
  
  /**
   * Check if a nonce has been seen before
   */
  hasSeen(nonce: string): boolean {
    return this.seenNonces.has(nonce);
  }
  
  /**
   * Mark a nonce as seen
   */
  markSeen(nonce: string): void {
  // Evict oldest if we're at max
  if (this.seenNonces.size >= this.maxNonces) {
    const first = this.seenNonces.values().next().value;
    if (first !== undefined) {
      this.seenNonces.delete(first);
    }
  }
    
    this.seenNonces.add(nonce);
  }
  
  /**
   * Verify update hasn't been replayed
   */
  verifyNotReplayed(update: StateUpdate): boolean {
    if (this.hasSeen(update.nonce)) {
      console.error('Replay attack detected!', update);
      return false;
    }
    
    this.markSeen(update.nonce);
    return true;
  }
  
  /**
   * Clear all nonces (useful for testing)
   */
  clear(): void {
    this.seenNonces.clear();
  }
}

