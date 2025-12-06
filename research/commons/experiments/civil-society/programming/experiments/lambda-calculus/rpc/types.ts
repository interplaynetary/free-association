/**
 * RPC Types for Lambda Calculus
 * 
 * Type definitions for Cap'n Web RPC integration including:
 * - Authentication credentials
 * - Sync operations
 * - Network serialization types
 * - Capability management types
 */

// Re-export sparse types for convenience
export type {
  EntityId,
  SparseRecognitionGraph
} from '../src/sparse/types';

export type { Distribution } from '../src/sparse/operations';

// ============================================================================
// Authentication & Identity
// ============================================================================

/**
 * Cryptographic proof types for symmetric authentication
 */
export type Proof = {
  entityId: string;
  timestamp: number;
  signature: string;
};

/**
 * Credential types for authentication
 * Symmetric protocol - both peers can use any credential type
 */
export type Credential =
  | {
      type: 'pubkey';
      publicKey: string;        // Ed25519/secp256k1 public key
      signature: string;        // Signature of challenge
      challenge: string;        // Challenge string
    }
  | {
      type: 'password';
      hash: string;            // bcrypt/argon2 hash
      salt: string;
    }
  | {
      type: 'did';
      did: string;             // Decentralized identifier (did:key:...)
      proof: string;           // DID verification proof
    }
  | {
      type: 'oauth';
      provider: string;        // 'github', 'google', etc.
      token: string;
      tokenType: string;
    };

/**
 * Entity identity information
 */
export interface EntityIdentity {
  id: string;
  publicKey?: string;
  did?: string;
  metadata?: {
    name?: string;
    avatar?: string;
    createdAt?: number;
  };
}

// ============================================================================
// Network Serialization
// ============================================================================

/**
 * Serialized sparse graph for network transmission
 * Array of [from, to, amount] tuples
 */
export interface SerializedSparseGraph {
  type: 'sparse-graph';
  edges: Array<[string, string, number]>;
  metadata?: {
    totalEntities: number;
    totalEdges: number;
    avgDegree: number;
    sparsity: number;
  };
}

/**
 * Serialized distribution for network transmission
 */
export interface SerializedDistribution {
  type: 'distribution';
  weights: Array<[string, number]>;
}

// ============================================================================
// Sync Operations
// ============================================================================

/**
 * Interval Tree Clock for CRDT-style conflict resolution
 * Superior to vector clocks for decentralized P2P:
 * - No global participant list needed
 * - Dynamic fork/join
 * - Space-efficient: O(log n) instead of O(n)
 */
export type { Stamp as ITCStamp } from '../itc';

/**
 * Sync operation types (with ITC for causality)
 */
export type SyncOperation =
  | {
      type: 'allocate';
      fromId: string;
      toId: string;
      amount: number;
      timestamp: number;
      clock: Stamp;  // ITC stamp instead of vector clock
    }
  | {
      type: 'revoke';
      fromId: string;
      toId: string;
      timestamp: number;
      clock: Stamp;
    }
  | {
      type: 'batch';
      operations: SyncOperation[];
      timestamp: number;
      clock: Stamp;
    };

/**
 * Sync update - sent over RPC when recognition changes
 */
export interface SyncUpdate {
  entityId: string;
  operation: SyncOperation;
  checksum?: string;  // For verifying integrity
}

/**
 * Sync callback - function signature for update subscriptions
 */
export type SyncCallback = (update: SyncUpdate) => void | Promise<void>;

/**
 * Sync queue item (for offline operations)
 */
export interface SyncQueueItem {
  id?: number;          // Auto-incremented by IndexedDB
  operation: SyncOperation;
  synced: boolean;
  attempts: number;
  lastAttempt?: number;
  error?: string;
}

// ============================================================================
// Cache Types
// ============================================================================

/**
 * Cache entry with TTL
 */
export interface CacheEntry<T> {
  value: T;
  timestamp: number;
  ttl: number;          // Time to live in milliseconds
  hits: number;         // For LRU tracking
}

/**
 * Cache key types for different operations
 */
export type CacheKey =
  | `mr:${string}:${string}`              // Mutual recognition
  | `tmr:${string}:${string}`             // Total MR (entity:universe hash)
  | `mrs:${string}:${string}`             // MRS (entity:universe hash)
  | `mrd:${string}:${string}`;            // MRD (entity:collective hash)

// ============================================================================
// RPC Session Types
// ============================================================================

/**
 * Transport types supported
 */
export type TransportType = 'websocket' | 'postmessage' | 'webrtc' | 'http-batch';

/**
 * Transport configuration
 */
export interface TransportConfig {
  type: TransportType;
  url?: string;          // For WebSocket/HTTP
  target?: Window | Worker | MessagePort; // For postMessage
  signal?: RTCDataChannel; // For WebRTC
  options?: {
    reconnect?: boolean;
    reconnectDelay?: number;
    heartbeatInterval?: number;
  };
}

/**
 * Connection state
 */
export type ConnectionState = 'connecting' | 'connected' | 'authenticated' | 'disconnected' | 'error';

/**
 * Connection info
 */
export interface ConnectionInfo {
  localEntityId: string;
  remoteEntityId?: string;
  state: ConnectionState;
  transport: TransportType;
  connectedAt?: number;
  authenticatedAt?: number;
  lastActivity?: number;
}

// ============================================================================
// Capability References
// ============================================================================

/**
 * Export table entry
 * What this peer has exported to remote peer
 */
export interface ExportEntry {
  id: number;           // Negative for local objects, positive for push results
  target: unknown;      // The actual object/function
  type: 'object' | 'function' | 'value';
  refCount: number;     // For garbage collection
}

/**
 * Import table entry
 * What this peer has received from remote peer
 */
export interface ImportEntry {
  id: number;           // Negative for remote objects, positive for pull results
  stub: unknown;        // RPC stub
  type: 'object' | 'function' | 'value';
  lastUsed: number;     // For garbage collection
}

// ============================================================================
// Error Types
// ============================================================================

/**
 * RPC error types
 */
export class RpcError extends Error {
  constructor(
    message: string,
    public code: string,
    public details?: unknown
  ) {
    super(message);
    this.name = 'RpcError';
  }
}

export class AuthenticationError extends RpcError {
  constructor(message: string, details?: unknown) {
    super(message, 'AUTH_ERROR', details);
    this.name = 'AuthenticationError';
  }
}

export class BudgetConstraintError extends RpcError {
  constructor(message: string, details?: unknown) {
    super(message, 'BUDGET_ERROR', details);
    this.name = 'BudgetConstraintError';
  }
}

export class SyncError extends RpcError {
  constructor(message: string, details?: unknown) {
    super(message, 'SYNC_ERROR', details);
    this.name = 'SyncError';
  }
}

// ============================================================================
// Utility Types
// ============================================================================

/**
 * Hash function for creating cache/sync keys
 */
export type HashFunction = (input: string | Set<string>) => string;

/**
 * Async iterable for streaming updates
 */
export type UpdateStream = AsyncIterable<SyncUpdate>;

/**
 * Conflict resolution strategy
 */
export type ConflictResolution = 
  | 'last-write-wins'
  | 'vector-clock'
  | 'custom';

/**
 * Conflict resolver function
 */
export type ConflictResolver = (
  local: SyncOperation,
  remote: SyncOperation
) => SyncOperation;

