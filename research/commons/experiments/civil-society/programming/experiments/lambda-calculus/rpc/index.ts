/**
 * Lambda Calculus RPC System
 * 
 * Symmetric peer-to-peer RPC implementation for Free Association.
 * Enables browser-based, offline-first, P2P coordination.
 * 
 * NEW: Cap'n Web Inspired Elegance!
 * - RpcTarget base class pattern
 * - TypeScript interface-first design (EntityAPI)
 * - Simple JSON serialization
 * - One-line setup with auto-initialization
 * - ITC instead of vector clocks
 * - HTTP batch mode for lightweight operations
 * 
 * Quick Start:
 * ```typescript
 * import { newWebSocketSession, type EntityAPI } from '@free-association/lambda-calculus/rpc';
 * 
 * let api: EntityAPI = newWebSocketSession('alice', 'wss://relay.example.com');
 * await api.initialize();
 * let mr = await api.getMutualRecognition('bob');
 * ```
 */

// Cap'n Web Patterns (START HERE!)
export { RpcTarget, isRpcTarget, type RpcStub } from './rpc-target';
export type { EntityAPI, EntitySyncAPI, EntityFullAPI, RecognitionUpdate } from './api';

// Server-side (for building relays)
export { RelayServer, createRelayServer } from './relay-server';

// Simple API
export {
  newWebSocketSession,
  newPostMessageSession,
  createHttpBatchSession,
  type SimpleSession
} from './simple-api';

// ITC Clocks (better than vector clocks!)
export { ITClock, resolveITCConflict, batchResolveConflicts } from './clock';
export type { ITCStamp } from './types';

// Simple JSON (replaces complex serialization)
export { RpcJSON, estimateSize } from './json-rpc';

// Unified Transport
export type { Transport } from './transport';
export {
  createWebSocketTransport,
  createPostMessageTransport,
  createHttpTransport,
  createLocalTransport
} from './transport';

// Core components
export { EntitySession, type EntitySessionConfig } from './entity-session';
export { PeerConnection, createP2PConnection, type PeerConnectionConfig } from './peer-connection';
export { BrowserStorage, STORES } from './browser-storage';
export { RecognitionCache, DEFAULT_CACHE_CONFIG, type CacheConfig } from './cache';
export { CapabilityManager } from './capability-manager';

// Types
export type {
  EntityId,
  SparseRecognitionGraph,
  Distribution,
  Credential,
  Proof,
  EntityIdentity,
  SerializedSparseGraph,
  SerializedDistribution,
  SyncOperation,
  SyncUpdate,
  SyncCallback,
  SyncQueueItem,
  CacheEntry,
  CacheKey,
  TransportType,
  TransportConfig,
  ConnectionState,
  ConnectionInfo,
  ExportEntry,
  ImportEntry,
  RpcError,
  AuthenticationError,
  BudgetConstraintError,
  SyncError
} from './types';

// Serialization
export {
  serializeSparseGraph,
  deserializeSparseGraph,
  serializeDistribution,
  deserializeDistribution,
  hashSet,
  mrCacheKey,
  tmrCacheKey,
  mrsCacheKey,
  mrdCacheKey,
  estimateSerializedSize
} from './serialization';

// Transports
export {
  WebSocketTransport,
  PostMessageTransport,
  WebRTCTransport,
  createWebSocketTransport,
  createIframeTransport,
  createWorkerTransport,
  createWebRTCTransport,
  SimpleSignaling
} from './transports';

export type {
  Transport,
  TransportMessage,
  TransportOptions
} from './transports/types';

// Capacity Management (Recognition-Based)
export type {
  CapacityQuota,
  AllocationStrategy,
  ResourceUsage,
  RateLimitViolation,
  ReplicationPolicy,
  ReplicationState
} from './capacity/types';

export {
  ComputeRateLimiter,
  StorageQuotaManager,
  BandwidthThrottle
} from './capacity';

// Replication
export {
  ReplicationManager,
  type ReplicationStrategy,
  SyncCoordinator,
  ConflictResolver,
  type SyncMode,
  type SyncSchedule
} from './replication';

// Elegant Features (Promise Pipelining, Record-Replay)
export {
  PipelinePromise,
  createPipelinePromise,
  replayInstructions,
  type ReplayInstruction
} from './elegant';

// Re-export sparse operations for convenience
export {
  sparseMutual,
  sparseTMR,
  sparseMRS,
  sparseMRD,
  sparseRMR,
  getMutualRecognitionPairs,
  checkBudgetConstraint,
  findBudgetViolations,
  batchSparseMRS,
  computeStatistics
} from '../src/sparse/operations';

export {
  SparseOps,
  toSparse,
  fromSparse,
  isSparseGraph,
  empty as emptySparseGraph,
  clone as cloneSparseGraph
} from '../src/sparse/types';

// ============================================
// Elegant State Restoration (NEW!)
// ============================================

// Identity Management
export {
  generateKeypair,
  deriveKeypair,
  exportKeypair,
  importKeypair,
  getPublicKey,
  isValidKeypair,
  getKeypairId,
  type KeyPair,
  type KeyAlgorithm
} from './identity/keypair';

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
} from './identity/credentials';

// One-Line Login (The Main Entry Point!)
export { login, type LoginOptions } from './restoration/login';

// State Restoration (Advanced Use)
export {
  type StateFragment,
  type ReplicaInfo,
  ReplicaNode,
  DiscoveryClient
} from './restoration/discovery';

export {
  StateProxy,
  createStateProxy,
  type StateProxyOptions
} from './restoration/state-proxy';

export {
  RestorationBatch
} from './restoration/batch';

export {
  mergeFragments,
  resolveConflict,
  type RecognitionEdge,
  type ReconstructedState
} from './restoration/reconstruct';

// Merkle Verification
export {
  buildMerkleTree,
  getMerkleRoot,
  verifyMerkleProof,
  type MerkleNode,
  type MerkleProof
} from './verification/merkle';

// ============================================
// Elegant Utilities (NEW - DRY Compliance!)
// ============================================

// Factory Functions - One-line object creation
export {
  createStorage,
  createCache,
  createClock,
  createClockFrom,
  createKeypair,
  createKeypairFrom,
  createSession,
  createSessionWith,
  createTestSession
} from './factories';

// Unified Error Types
export {
  // Base
  RpcError,
  isRpcError,
  toRpcError,
  createErrorResponse,
  
  // Method & Target
  MethodNotFoundError,
  InvalidMethodError,
  TargetNotFoundError,
  
  // Session & Entity
  SessionNotFoundError,
  EntityNotFoundError,
  SessionAlreadyExistsError,
  
  // Authentication
  AuthenticationError,
  InvalidCredentialError,
  ChallengeVerificationError,
  
  // State & Sync
  SyncError,
  StateRestorationError,
  MerkleVerificationError,
  NoReplicasFoundError,
  
  // Budget & Allocation
  BudgetConstraintError,
  AllocationError,
  
  // Network & Transport
  TransportError,
  NetworkError,
  TimeoutError,
  
  // Serialization
  SerializationError,
  DeserializationError,
  
  // Validation
  ValidationError,
  InvalidParameterError
} from './errors';

// Server Middleware - DRY server setup
export {
  createServerMiddleware,
  createRelayServerWithMiddleware,
  createWorkersWebSocketAdapter,
  createNodeWebSocketAdapter,
  createBunWebSocketAdapter,
  createWorkersHttpRequest,
  createNodeHttpRequest,
  toWorkersResponse,
  toNodeResponse,
  type ServerMiddleware
} from './server/middleware';

export {
  dispatchRpcCall,
  dispatchRpcBatch,
  type RpcRequest,
  type RpcResponse,
  type RpcBatchRequest,
  type RpcBatchResponse
} from './server/rpc-dispatcher';

export {
  handleRelayMessage,
  type WebSocketAdapter,
  type MessageType,
  type RelayMessage
} from './server/message-handler';

export {
  handleHttpRpcBatch,
  handleStatsRequest,
  type HttpRequest,
  type HttpResponse
} from './server/http-handler';

// ============================================
// Security Layer (NEW - Complete Integration!)
// ============================================

// Security primitives
export {
  SecureContext,
  SecureContextManager,
  SecureStorage,
  createSecureStorage,
  SecureEntitySession,
  SecureTransport,
  secureLogin,
  createSecureSession,
  createSecureSessionWithKeypair
} from './security';

