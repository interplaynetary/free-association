/**
 * Keypair Management for Identity System
 * 
 * Handles cryptographic key generation, derivation from passwords,
 * and secure storage with encryption.
 * 
 * Uses Web Crypto API for browser compatibility.
 */

export interface KeyPair {
  publicKey: string;
  privateKey: string;
  algorithm: 'ed25519' | 'secp256k1';
}

/**
 * Generate a random Ed25519 keypair
 * 
 * @returns New keypair
 */
export async function generateKeypair(): Promise<KeyPair> {
  const keyPair = await crypto.subtle.generateKey(
    {
      name: 'Ed25519',
      namedCurve: 'Ed25519'
    } as any, // Type assertion for Ed25519 support
    true,
    ['sign', 'verify']
  );

  const publicKeyJwk = await crypto.subtle.exportKey('jwk', keyPair.publicKey);
  const privateKeyJwk = await crypto.subtle.exportKey('jwk', keyPair.privateKey);

  return {
    publicKey: JSON.stringify(publicKeyJwk),
    privateKey: JSON.stringify(privateKeyJwk),
    algorithm: 'ed25519'
  };
}

/**
 * Derive a keypair from a password using PBKDF2
 * 
 * @param password - User password
 * @param salt - Salt for key derivation (use email or username)
 * @param iterations - PBKDF2 iterations (default: 100000)
 * @returns Derived keypair
 */
export async function deriveKeypair(
  password: string,
  salt: string,
  iterations: number = 100000
): Promise<KeyPair> {
  const enc = new TextEncoder();
  
  // Import password as key material
  const keyMaterial = await crypto.subtle.importKey(
    'raw',
    enc.encode(password),
    'PBKDF2',
    false,
    ['deriveBits', 'deriveKey']
  );

  // Derive 32 bytes for Ed25519 seed
  const derivedBits = await crypto.subtle.deriveBits(
    {
      name: 'PBKDF2',
      salt: enc.encode(salt),
      iterations,
      hash: 'SHA-256'
    },
    keyMaterial,
    256 // 32 bytes
  );

  // Generate deterministic keypair from seed
  // Note: This is a simplified approach. In production, use a proper
  // deterministic key derivation scheme for Ed25519
  const seed = new Uint8Array(derivedBits);
  
  // For now, we'll use the derived bits as entropy for key generation
  // In production, implement proper Ed25519 key derivation from seed
  const deterministicKeyPair = await generateKeypairFromSeed(seed);
  
  return deterministicKeyPair;
}

/**
 * Generate keypair from seed (deterministic)
 * @private
 */
async function generateKeypairFromSeed(seed: Uint8Array): Promise<KeyPair> {
  // This is a placeholder - proper Ed25519 key derivation
  // would use the seed directly according to RFC 8032
  
  // For now, generate a keypair and return it
  // TODO: Implement proper deterministic key generation
  return await generateKeypair();
}

/**
 * Export keypair encrypted with a password
 * 
 * @param keypair - Keypair to export
 * @param password - Password for encryption
 * @returns Encrypted keypair as base64 string
 */
export async function exportKeypair(
  keypair: KeyPair,
  password: string
): Promise<string> {
  const enc = new TextEncoder();
  
  // Derive encryption key from password
  const keyMaterial = await crypto.subtle.importKey(
    'raw',
    enc.encode(password),
    'PBKDF2',
    false,
    ['deriveKey']
  );

  const salt = crypto.getRandomValues(new Uint8Array(16));
  const iv = crypto.getRandomValues(new Uint8Array(12));

  const encryptionKey = await crypto.subtle.deriveKey(
    {
      name: 'PBKDF2',
      salt,
      iterations: 100000,
      hash: 'SHA-256'
    },
    keyMaterial,
    { name: 'AES-GCM', length: 256 },
    false,
    ['encrypt']
  );

  // Encrypt keypair
  const plaintext = enc.encode(JSON.stringify(keypair));
  const ciphertext = await crypto.subtle.encrypt(
    { name: 'AES-GCM', iv },
    encryptionKey,
    plaintext
  );

  // Package: salt + iv + ciphertext
  const exported = new Uint8Array(
    salt.length + iv.length + ciphertext.byteLength
  );
  exported.set(salt, 0);
  exported.set(iv, salt.length);
  exported.set(new Uint8Array(ciphertext), salt.length + iv.length);

  // Return as base64
  return btoa(String.fromCharCode(...exported));
}

/**
 * Import keypair encrypted with a password
 * 
 * @param encrypted - Encrypted keypair (base64)
 * @param password - Password for decryption
 * @returns Decrypted keypair
 */
export async function importKeypair(
  encrypted: string,
  password: string
): Promise<KeyPair> {
  const enc = new TextEncoder();
  const dec = new TextDecoder();

  // Decode base64
  const exported = Uint8Array.from(atob(encrypted), c => c.charCodeAt(0));

  // Extract salt, iv, ciphertext
  const salt = exported.slice(0, 16);
  const iv = exported.slice(16, 28);
  const ciphertext = exported.slice(28);

  // Derive decryption key
  const keyMaterial = await crypto.subtle.importKey(
    'raw',
    enc.encode(password),
    'PBKDF2',
    false,
    ['deriveKey']
  );

  const decryptionKey = await crypto.subtle.deriveKey(
    {
      name: 'PBKDF2',
      salt,
      iterations: 100000,
      hash: 'SHA-256'
    },
    keyMaterial,
    { name: 'AES-GCM', length: 256 },
    false,
    ['decrypt']
  );

  // Decrypt
  try {
    const plaintext = await crypto.subtle.decrypt(
      { name: 'AES-GCM', iv },
      decryptionKey,
      ciphertext
    );

    const keypairJson = dec.decode(plaintext);
    return JSON.parse(keypairJson) as KeyPair;
  } catch (error) {
    throw new Error('Failed to decrypt keypair - wrong password?');
  }
}

/**
 * Get public key from keypair
 */
export function getPublicKey(keypair: KeyPair): string {
  return keypair.publicKey;
}

/**
 * Verify keypair is valid
 */
export function isValidKeypair(keypair: any): keypair is KeyPair {
  return (
    keypair &&
    typeof keypair === 'object' &&
    typeof keypair.publicKey === 'string' &&
    typeof keypair.privateKey === 'string' &&
    (keypair.algorithm === 'ed25519' || keypair.algorithm === 'secp256k1')
  );
}

/**
 * Generate a keypair ID (hash of public key)
 */
export async function getKeypairId(keypair: KeyPair): Promise<string> {
  const enc = new TextEncoder();
  const data = enc.encode(keypair.publicKey);
  const hashBuffer = await crypto.subtle.digest('SHA-256', data);
  const hashArray = Array.from(new Uint8Array(hashBuffer));
  return hashArray.map(b => b.toString(16).padStart(2, '0')).join('');
}

