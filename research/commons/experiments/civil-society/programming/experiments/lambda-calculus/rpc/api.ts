/**
 * EntityAPI - TypeScript Interface for Recognition RPC
 * 
 * Defines the complete API for entity recognition operations.
 * Use this interface for type-safe RPC calls.
 * 
 * Based on Cap'n Web's interface-first design pattern:
 * ```typescript
 * // Client gets full type safety
 * let api: EntityAPI = newWebSocketSession('alice', 'wss://...');
 * let mr = await api.getMutualRecognition('bob');
 * 
 * // Server implements the interface
 * class EntitySession extends RpcTarget implements EntityAPI {
 *   // Implementation
 * }
 * ```
 */

/**
 * Core Recognition API
 * 
 * All methods return Promises for async RPC.
 */
export interface EntityAPI {
  /**
   * Get mutual recognition between this entity and target
   */
  getMutualRecognition(targetId: string): Promise<number>;

  /**
   * Get Mutual Recognition Set (MRS) for a set of entities
   * Returns average mutual recognition for each entity with all others
   */
  getMRS(entityIds: string[]): Promise<Record<string, number>>;

  /**
   * Get Mutual Recognition Distribution (MRD) for a set of entities
   * Returns distribution of recognition values
   */
  getMRD(entityIds: string[]): Promise<Record<string, number>>;

  /**
   * Allocate recognition to another entity
   */
  allocateRecognition(targetId: string, amount: number): Promise<void>;

  /**
   * Get all allocations this entity has made
   */
  getMyAllocations(): Promise<Array<{ targetId: string; amount: number }>>;

  /**
   * Verify identity with cryptographic proof
   */
  verifyIdentity(proof: unknown): Promise<boolean>;
}

/**
 * Recognition update notification
 */
export interface RecognitionUpdate {
  type: 'allocate' | 'revoke';
  fromId: string;
  toId: string;
  amount?: number;
  timestamp: number;
}

/**
 * Extended API with sync operations
 */
export interface EntitySyncAPI extends EntityAPI {
  /**
   * Receive sync update from peer
   */
  receiveSyncUpdate(update: unknown): Promise<void>;

  /**
   * Subscribe to recognition updates (natural callback style)
   * 
   * @param callback - Function called when recognition changes
   * @returns Promise that resolves when subscription is active
   * 
   * @example
   * ```typescript
   * // Simple subscription
   * await api.subscribe(update => {
   *   console.log('Recognition changed:', update);
   * });
   * ```
   */
  subscribe(callback: (update: RecognitionUpdate) => void): Promise<void>;

  /**
   * Unsubscribe from updates
   */
  unsubscribe(callback: (update: RecognitionUpdate) => void): void;
}

/**
 * Full API with storage access
 */
export interface EntityFullAPI extends EntitySyncAPI {
  /**
   * Get underlying storage
   */
  getStorage(): unknown;

  /**
   * Get cache instance
   */
  getCache(): unknown;

  /**
   * Get ITC clock
   */
  getClock(): unknown;

  /**
   * Fork clock for new peer
   */
  forkClock(): unknown;
}

