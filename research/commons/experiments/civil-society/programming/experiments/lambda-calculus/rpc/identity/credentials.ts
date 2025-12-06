/**
 * Credentials & Signatures for Identity Verification
 * 
 * Handles challenge-response authentication and capability tokens.
 */

import type { KeyPair } from './keypair';

export interface Challenge {
  nonce: string;
  timestamp: number;
  issuer: string;
}

export interface Credential {
  type: 'challenge' | 'capability';
  publicKey: string;
  signature: string;
  challenge?: Challenge;
  capability?: CapabilityToken;
}

export interface CapabilityToken {
  issuer: string;
  subject: string;
  permissions: string[];
  exp

iration: number;
  issued: number;
}

/**
 * Create a challenge for authentication
 */
export function createChallenge(issuer: string): Challenge {
  const nonce = crypto.randomUUID();
  return {
    nonce,
    timestamp: Date.now(),
    issuer
  };
}

/**
 * Sign a challenge to prove identity
 */
export async function signChallenge(
  challenge: Challenge,
  privateKey: string
): Promise<string> {
  const enc = new TextEncoder();
  
  // Import private key
  const privateKeyJwk = JSON.parse(privateKey);
  const cryptoKey = await crypto.subtle.importKey(
    'jwk',
    privateKeyJwk,
    {
      name: 'Ed25519',
      namedCurve: 'Ed25519'
    } as any,
    false,
    ['sign']
  );

  // Sign challenge
  const message = enc.encode(JSON.stringify(challenge));
  const signature = await crypto.subtle.sign(
    { name: 'Ed25519' } as any,
    cryptoKey,
    message
  );

  // Return as base64
  return btoa(String.fromCharCode(...new Uint8Array(signature)));
}

/**
 * Verify a signed challenge
 */
export async function verifyChallenge(
  challenge: Challenge,
  signature: string,
  publicKey: string
): Promise<boolean> {
  try {
    const enc = new TextEncoder();
    
    // Import public key
    const publicKeyJwk = JSON.parse(publicKey);
    const cryptoKey = await crypto.subtle.importKey(
      'jwk',
      publicKeyJwk,
      {
        name: 'Ed25519',
        namedCurve: 'Ed25519'
      } as any,
      false,
      ['verify']
    );

    // Decode signature
    const signatureBytes = Uint8Array.from(atob(signature), c => c.charCodeAt(0));

    // Verify
    const message = enc.encode(JSON.stringify(challenge));
    return await crypto.subtle.verify(
      { name: 'Ed25519' } as any,
      cryptoKey,
      signatureBytes,
      message
    );
  } catch (error) {
    console.error('Challenge verification failed:', error);
    return false;
  }
}

/**
 * Create a capability token (JWT-like)
 */
export async function createCapability(
  issuer: string,
  subject: string,
  permissions: string[],
  privateKey: string,
  expirationMs: number = 24 * 60 * 60 * 1000 // 24 hours
): Promise<string> {
  const token: CapabilityToken = {
    issuer,
    subject,
    permissions,
    expiration: Date.now() + expirationMs,
    issued: Date.now()
  };

  // Sign token
  const signature = await signCapability(token, privateKey);

  // Package as base64
  const payload = btoa(JSON.stringify(token));
  return `${payload}.${signature}`;
}

/**
 * Sign a capability token
 * @private
 */
async function signCapability(
  token: CapabilityToken,
  privateKey: string
): Promise<string> {
  const enc = new TextEncoder();
  
  // Import private key
  const privateKeyJwk = JSON.parse(privateKey);
  const cryptoKey = await crypto.subtle.importKey(
    'jwk',
    privateKeyJwk,
    {
      name: 'Ed25519',
      namedCurve: 'Ed25519'
    } as any,
    false,
    ['sign']
  );

  // Sign token
  const message = enc.encode(JSON.stringify(token));
  const signature = await crypto.subtle.sign(
    { name: 'Ed25519' } as any,
    cryptoKey,
    message
  );

  return btoa(String.fromCharCode(...new Uint8Array(signature)));
}

/**
 * Verify and decode a capability token
 */
export async function verifyCapability(
  tokenString: string,
  issuerPublicKey: string
): Promise<CapabilityToken | null> {
  try {
    const [payloadB64, signatureB64] = tokenString.split('.');
    
    // Decode token
    const token: CapabilityToken = JSON.parse(atob(payloadB64));

    // Check expiration
    if (Date.now() > token.expiration) {
      return null;
    }

    // Verify signature
    const enc = new TextEncoder();
    const publicKeyJwk = JSON.parse(issuerPublicKey);
    const cryptoKey = await crypto.subtle.importKey(
      'jwk',
      publicKeyJwk,
      {
        name: 'Ed25519',
        namedCurve: 'Ed25519'
      } as any,
      false,
      ['verify']
    );

    const signatureBytes = Uint8Array.from(atob(signatureB64), c => c.charCodeAt(0));
    const message = enc.encode(JSON.stringify(token));
    
    const valid = await crypto.subtle.verify(
      { name: 'Ed25519' } as any,
      cryptoKey,
      signatureBytes,
      message
    );

    return valid ? token : null;
  } catch (error) {
    console.error('Capability verification failed:', error);
    return null;
  }
}

/**
 * Check if capability has permission
 */
export function hasPermission(
  capability: CapabilityToken,
  permission: string
): boolean {
  return capability.permissions.includes(permission) ||
         capability.permissions.includes('*');
}

/**
 * Create credential from challenge
 */
export async function createCredential(
  keypair: KeyPair,
  challenge: Challenge
): Promise<Credential> {
  const signature = await signChallenge(challenge, keypair.privateKey);
  
  return {
    type: 'challenge',
    publicKey: keypair.publicKey,
    signature,
    challenge
  };
}

/**
 * Verify credential
 */
export async function verifyCredential(
  credential: Credential
): Promise<boolean> {
  if (credential.type === 'challenge' && credential.challenge) {
    return await verifyChallenge(
      credential.challenge,
      credential.signature,
      credential.publicKey
    );
  }
  
  if (credential.type === 'capability' && credential.capability) {
    const token = await verifyCapability(
      `${btoa(JSON.stringify(credential.capability))}.${credential.signature}`,
      credential.publicKey
    );
    return token !== null;
  }
  
  return false;
}

