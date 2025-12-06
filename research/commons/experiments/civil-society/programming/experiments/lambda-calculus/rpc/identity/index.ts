/**
 * Identity Management - Elegant Exports
 * 
 * One-stop import for all identity utilities:
 * - Keypair generation & derivation
 * - Challenge-response authentication
 * - Capability tokens
 */

// Keypair management
export {
  generateKeypair,
  deriveKeypair,
  exportKeypair,
  importKeypair,
  getPublicKey,
  isValidKeypair,
  getKeypairId,
  type KeyPair
} from './keypair';

// Credentials & authentication
export {
  createChallenge,
  signChallenge,
  verifyChallenge,
  createCapability,
  verifyCapability,
  hasPermission,
  createCredential,
  verifyCredential,
  type Challenge,
  type Credential,
  type CapabilityToken
} from './credentials';

// State update signing & verification
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
} from './signing';
