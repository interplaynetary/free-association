/**
 * Security Module - Low-Level Integration
 * 
 * This is the foundational security layer for the entire system.
 * Import from here to get automatic signing/verification everywhere!
 */

// Core primitives
export { SecureContext, SecureContextManager } from './secure-context';
export { SecureStorage, createSecureStorage } from './secure-storage';

// High-level integration
export {
  SecureEntitySession,
  SecureTransport,
  secureLogin,
  createSecureSession,
  createSecureSessionWithKeypair
} from './integration-guide';

// Re-export signing utilities
export {
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
} from '../identity/signing';

