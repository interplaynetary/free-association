/**
 * Unified Error Types - DRY Error Handling
 * 
 * Consistent, typed errors across the entire RPC system.
 * Serializable for network transmission.
 */

/**
 * Base RPC error with code and details
 * 
 * All RPC errors extend this class.
 */
export class RpcError extends Error {
  public readonly code: string;
  public readonly details?: any;

  constructor(message: string, code: string, details?: any) {
    super(message);
    this.name = 'RpcError';
    this.code = code;
    this.details = details;

    // Maintain proper stack trace (only in V8)
    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, this.constructor);
    }
  }

  /**
   * Serialize for network transmission
   */
  toJSON() {
    return {
      error: this.message,
      code: this.code,
      details: this.details
    };
  }

  /**
   * Convert to RPC response format
   */
  toRpcResponse(id?: string | number) {
    return {
      id,
      error: this.message,
      code: this.code,
      details: this.details
    };
  }
}

// ============================================================================
// Method & Target Errors
// ============================================================================

export class MethodNotFoundError extends RpcError {
  constructor(method: string) {
    super(
      `Method not found: ${method}`,
      'METHOD_NOT_FOUND',
      { method }
    );
    this.name = 'MethodNotFoundError';
  }
}

export class InvalidMethodError extends RpcError {
  constructor(method: string, reason: string) {
    super(
      `Invalid method '${method}': ${reason}`,
      'INVALID_METHOD',
      { method, reason }
    );
    this.name = 'InvalidMethodError';
  }
}

export class TargetNotFoundError extends RpcError {
  constructor(targetId: string) {
    super(
      `Target not found: ${targetId}`,
      'TARGET_NOT_FOUND',
      { targetId }
    );
    this.name = 'TargetNotFoundError';
  }
}

// ============================================================================
// Session & Entity Errors
// ============================================================================

export class SessionNotFoundError extends RpcError {
  constructor(entityId: string) {
    super(
      `Session not found: ${entityId}`,
      'SESSION_NOT_FOUND',
      { entityId }
    );
    this.name = 'SessionNotFoundError';
  }
}

export class EntityNotFoundError extends RpcError {
  constructor(entityId: string) {
    super(
      `Entity not found: ${entityId}`,
      'ENTITY_NOT_FOUND',
      { entityId }
    );
    this.name = 'EntityNotFoundError';
  }
}

export class SessionAlreadyExistsError extends RpcError {
  constructor(entityId: string) {
    super(
      `Session already exists: ${entityId}`,
      'SESSION_EXISTS',
      { entityId }
    );
    this.name = 'SessionAlreadyExistsError';
  }
}

// ============================================================================
// Authentication Errors
// ============================================================================

export class AuthenticationError extends RpcError {
  constructor(reason: string, details?: any) {
    super(
      `Authentication failed: ${reason}`,
      'AUTH_FAILED',
      details
    );
    this.name = 'AuthenticationError';
  }
}

export class InvalidCredentialError extends RpcError {
  constructor(credentialType: string) {
    super(
      `Invalid credential: ${credentialType}`,
      'INVALID_CREDENTIAL',
      { credentialType }
    );
    this.name = 'InvalidCredentialError';
  }
}

export class ChallengeVerificationError extends RpcError {
  constructor(reason: string) {
    super(
      `Challenge verification failed: ${reason}`,
      'CHALLENGE_FAILED',
      { reason }
    );
    this.name = 'ChallengeVerificationError';
  }
}

// ============================================================================
// State & Sync Errors
// ============================================================================

export class SyncError extends RpcError {
  constructor(reason: string, details?: any) {
    super(
      `Sync failed: ${reason}`,
      'SYNC_FAILED',
      details
    );
    this.name = 'SyncError';
  }
}

export class StateRestorationError extends RpcError {
  constructor(reason: string, details?: any) {
    super(
      `State restoration failed: ${reason}`,
      'RESTORATION_FAILED',
      details
    );
    this.name = 'StateRestorationError';
  }
}

export class MerkleVerificationError extends RpcError {
  constructor(expectedRoot: string, actualRoot: string) {
    super(
      `Merkle verification failed: expected ${expectedRoot}, got ${actualRoot}`,
      'MERKLE_FAILED',
      { expectedRoot, actualRoot }
    );
    this.name = 'MerkleVerificationError';
  }
}

export class NoReplicasFoundError extends RpcError {
  constructor(publicKey: string) {
    super(
      `No replicas found for ${publicKey}`,
      'NO_REPLICAS',
      { publicKey }
    );
    this.name = 'NoReplicasFoundError';
  }
}

// ============================================================================
// Budget & Allocation Errors
// ============================================================================

export class BudgetConstraintError extends RpcError {
  constructor(entityId: string, required: number, available: number) {
    super(
      `Budget constraint violated for ${entityId}: required ${required}, available ${available}`,
      'BUDGET_CONSTRAINT',
      { entityId, required, available }
    );
    this.name = 'BudgetConstraintError';
  }
}

export class AllocationError extends RpcError {
  constructor(reason: string, details?: any) {
    super(
      `Allocation failed: ${reason}`,
      'ALLOCATION_FAILED',
      details
    );
    this.name = 'AllocationError';
  }
}

// ============================================================================
// Network & Transport Errors
// ============================================================================

export class TransportError extends RpcError {
  constructor(transportType: string, reason: string) {
    super(
      `Transport error (${transportType}): ${reason}`,
      'TRANSPORT_ERROR',
      { transportType, reason }
    );
    this.name = 'TransportError';
  }
}

export class NetworkError extends RpcError {
  constructor(reason: string, details?: any) {
    super(
      `Network error: ${reason}`,
      'NETWORK_ERROR',
      details
    );
    this.name = 'NetworkError';
  }
}

export class TimeoutError extends RpcError {
  constructor(operation: string, timeoutMs: number) {
    super(
      `Operation timed out: ${operation} (${timeoutMs}ms)`,
      'TIMEOUT',
      { operation, timeoutMs }
    );
    this.name = 'TimeoutError';
  }
}

// ============================================================================
// Serialization Errors
// ============================================================================

export class SerializationError extends RpcError {
  constructor(reason: string, data?: any) {
    super(
      `Serialization failed: ${reason}`,
      'SERIALIZATION_FAILED',
      { reason, data: data ? String(data).slice(0, 100) : undefined }
    );
    this.name = 'SerializationError';
  }
}

export class DeserializationError extends RpcError {
  constructor(reason: string, data?: any) {
    super(
      `Deserialization failed: ${reason}`,
      'DESERIALIZATION_FAILED',
      { reason, data: data ? String(data).slice(0, 100) : undefined }
    );
    this.name = 'DeserializationError';
  }
}

// ============================================================================
// Validation Errors
// ============================================================================

export class ValidationError extends RpcError {
  constructor(field: string, reason: string) {
    super(
      `Validation failed for '${field}': ${reason}`,
      'VALIDATION_FAILED',
      { field, reason }
    );
    this.name = 'ValidationError';
  }
}

export class InvalidParameterError extends RpcError {
  constructor(param: string, expected: string, received: any) {
    super(
      `Invalid parameter '${param}': expected ${expected}, got ${typeof received}`,
      'INVALID_PARAMETER',
      { param, expected, received: String(received).slice(0, 50) }
    );
    this.name = 'InvalidParameterError';
  }
}

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Check if an error is an RpcError
 */
export function isRpcError(error: any): error is RpcError {
  return error instanceof RpcError;
}

/**
 * Convert any error to RpcError
 */
export function toRpcError(error: unknown): RpcError {
  if (isRpcError(error)) {
    return error;
  }

  if (error instanceof Error) {
    return new RpcError(error.message, 'UNKNOWN_ERROR', {
      originalName: error.name,
      stack: error.stack
    });
  }

  return new RpcError(String(error), 'UNKNOWN_ERROR');
}

/**
 * Create error response for RPC
 */
export function createErrorResponse(error: unknown, id?: string | number) {
  const rpcError = toRpcError(error);
  return rpcError.toRpcResponse(id);
}

