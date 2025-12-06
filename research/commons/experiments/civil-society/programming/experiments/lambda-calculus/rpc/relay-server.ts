/**
 * RelayServer - Cap'n Web Style Server Implementation
 * 
 * Connects peers and facilitates recognition-based coordination.
 * Can be deployed to Cloudflare Workers, Node.js, Bun, etc.
 * 
 * Usage:
 * ```typescript
 * // Cloudflare Worker
 * export default {
 *   fetch(request) {
 *     return newWorkersRpcResponse(request, new RelayServer());
 *   }
 * }
 * ```
 */

import { RpcTarget } from './rpc-target';
import { EntitySession } from './entity-session';
import type { EntityId, SyncUpdate } from './types';
import type { RecognitionUpdate } from './api';
import type { StateFragment, ReplicaInfo } from './restoration/discovery';
import { buildMerkleTree, getMerkleRoot } from './verification/merkle';
import type { RecognitionEdge } from './restoration/reconstruct';
import { createChallenge, verifyChallenge, type Challenge } from './identity/credentials';
import { AuthenticationError } from './errors';

/**
 * Relay Server - connects peers for P2P coordination
 * 
 * Extends RpcTarget so it can be called over RPC!
 */
export class RelayServer extends RpcTarget {
  private sessions = new Map<EntityId, EntitySession>();
  private connections = new Map<EntityId, Set<EntityId>>();
  private subscriptions = new Map<EntityId, Set<(update: RecognitionUpdate) => void>>();
  private replicas = new Map<string, ReplicaInfo[]>(); // publicKey -> list of replica info
  private challenges = new Map<string, Challenge>(); // nonce -> challenge (for authentication)

  /**
   * Register a new peer
   * 
   * @param entityId - Unique entity identifier
   * @returns EntitySession capability for this peer
   */
  async register(entityId: EntityId): Promise<EntitySession> {
    // Check if already registered
    if (this.sessions.has(entityId)) {
      throw new Error(`Entity ${entityId} already registered`);
    }

    // Create new session (auto-initializes!)
    const session = new EntitySession(entityId);
    
    // Subscribe to updates for broadcasting
    await session.subscribe((update) => {
      this.broadcastUpdate(entityId, update);
    });

    this.sessions.set(entityId, session);
    this.connections.set(entityId, new Set());
    this.subscriptions.set(entityId, new Set());

    console.log(`Registered entity: ${entityId}`);
    
    return session; // Returned by reference (capability!)
  }

  /**
   * Connect two peers for bidirectional communication
   * 
   * @param fromId - First entity
   * @param toId - Second entity
   */
  async connect(fromId: EntityId, toId: EntityId): Promise<void> {
    if (!this.sessions.has(fromId)) {
      throw new Error(`Entity ${fromId} not registered`);
    }
    if (!this.sessions.has(toId)) {
      throw new Error(`Entity ${toId} not registered`);
    }

    // Bidirectional connection
    this.connections.get(fromId)!.add(toId);
    this.connections.get(toId)!.add(fromId);

    console.log(`Connected: ${fromId} <-> ${toId}`);
  }

  /**
   * Disconnect two peers
   */
  async disconnect(fromId: EntityId, toId: EntityId): Promise<void> {
    this.connections.get(fromId)?.delete(toId);
    this.connections.get(toId)?.delete(fromId);

    console.log(`Disconnected: ${fromId} <-> ${toId}`);
  }

  /**
   * Get session for an entity (if registered)
   * 
   * @param entityId - Entity to look up
   * @returns EntitySession or null
   */
  getSession(entityId: EntityId): EntitySession | null {
    return this.sessions.get(entityId) || null;
  }

  /**
   * Get list of connected peers for an entity
   */
  getConnections(entityId: EntityId): EntityId[] {
    return Array.from(this.connections.get(entityId) || []);
  }

  /**
   * Get list of all registered entities
   */
  listEntities(): EntityId[] {
    return Array.from(this.sessions.keys());
  }

  /**
   * Broadcast update to connected peers
   * @private
   */
  private broadcastUpdate(fromId: EntityId, update: RecognitionUpdate): void {
    const connectedPeers = this.connections.get(fromId);
    
    if (!connectedPeers) return;

    for (const peerId of connectedPeers) {
      const callbacks = this.subscriptions.get(peerId);
      
      if (callbacks) {
        for (const callback of callbacks) {
          try {
            callback(update);
          } catch (error) {
            console.error(`Error broadcasting to ${peerId}:`, error);
          }
        }
      }
    }
  }

  /**
   * Subscribe to updates for a specific entity
   * 
   * @param entityId - Entity to subscribe to
   * @param callback - Function to call on updates
   */
  async subscribe(
    entityId: EntityId,
    callback: (update: RecognitionUpdate) => void
  ): Promise<void> {
    if (!this.subscriptions.has(entityId)) {
      this.subscriptions.set(entityId, new Set());
    }
    
    this.subscriptions.get(entityId)!.add(callback);
  }

  /**
   * Unregister an entity and cleanup
   */
  async unregister(entityId: EntityId): Promise<void> {
    // Disconnect from all peers
    const connectedPeers = this.connections.get(entityId);
    if (connectedPeers) {
      for (const peerId of connectedPeers) {
        await this.disconnect(entityId, peerId);
      }
    }

    // Cleanup
    const session = this.sessions.get(entityId);
    if (session) {
      await session.close();
    }

    this.sessions.delete(entityId);
    this.connections.delete(entityId);
    this.subscriptions.delete(entityId);

    console.log(`Unregistered entity: ${entityId}`);
  }

  /**
   * Get server stats
   */
  getStats() {
    return {
      totalEntities: this.sessions.size,
      totalConnections: Array.from(this.connections.values())
        .reduce((sum, set) => sum + set.size, 0) / 2, // Divide by 2 for bidirectional
      entities: this.listEntities()
    };
  }

  // ============================================
  // Authentication Methods (Cap'n Web Challenge-Response)
  // ============================================

  /**
   * Create an authentication challenge
   * 
   * The client must sign this challenge with their private key
   * to prove they own the corresponding public key.
   * 
   * @returns Challenge object with nonce and issuer
   * 
   * @example
   * // Client flow:
   * const challenge = await relay.createChallenge();
   * const signature = await mySecureContext.signData(challenge);
   * const session = await relay.authenticate(challenge, signature, myPublicKey);
   */
  createChallenge(): Challenge {
    const challenge = createChallenge('relay-server');
    this.challenges.set(challenge.nonce, challenge);
    
    // Clean up old challenges after 5 minutes
    setTimeout(() => {
      this.challenges.delete(challenge.nonce);
    }, 5 * 60 * 1000);
    
    console.log(`Created authentication challenge: ${challenge.nonce}`);
    return challenge;
  }

  /**
   * Authenticate a client and return their EntitySession capability
   * 
   * This is the Cap'n Web pattern for authentication:
   * 1. Client requests challenge
   * 2. Client signs challenge with private key
   * 3. Server verifies signature
   * 4. Server returns session capability (unforgeable reference)
   * 
   * @param challenge - The challenge that was signed
   * @param signature - Base64-encoded signature
   * @param publicKey - Client's public key (JWK format)
   * @returns EntitySession capability for the authenticated entity
   * @throws AuthenticationError if verification fails
   * 
   * @example
   * const challenge = await relay.createChallenge();
   * const signature = await secureContext.signData(challenge);
   * const session = await relay.authenticate(challenge, signature, publicKey);
   * // Now the client has a capability to call methods on their session!
   */
  async authenticate(
    challenge: Challenge,
    signature: string,
    publicKey: string
  ): Promise<EntitySession> {
    // Verify the challenge was issued by us
    const storedChallenge = this.challenges.get(challenge.nonce);
    if (!storedChallenge) {
      throw new AuthenticationError('Unknown or expired challenge');
    }

    // Verify the challenge matches
    if (storedChallenge.entityId !== challenge.entityId ||
        storedChallenge.timestamp !== challenge.timestamp) {
      throw new AuthenticationError('Challenge mismatch');
    }

    // Verify the signature
    const isValid = await verifyChallenge(challenge, signature, publicKey);
    if (!isValid) {
      throw new AuthenticationError('Invalid signature');
    }

    // Clean up the used challenge (prevent replay)
    this.challenges.delete(challenge.nonce);

    // Derive entityId from publicKey
    // In a real system, you'd use a proper derivation (e.g., hash of public key)
    const entityId = `entity-${publicKey.substring(0, 16)}`;

    // Get or create session
    let session = this.sessions.get(entityId);
    if (!session) {
      console.log(`Creating new authenticated session for ${entityId}`);
      session = new EntitySession(entityId);
      
      // Subscribe to updates for broadcasting
      await session.subscribe((update) => {
        this.broadcastUpdate(entityId, update);
      });

      this.sessions.set(entityId, session);
      this.connections.set(entityId, new Set());
      this.subscriptions.set(entityId, new Set());
    } else {
      console.log(`Returning existing session for ${entityId}`);
    }

    // Return the session capability
    return session;
  }

  /**
   * Shutdown server and cleanup all sessions
   */
  async shutdown(): Promise<void> {
    console.log('Shutting down relay server...');
    
    for (const entityId of this.sessions.keys()) {
      await this.unregister(entityId);
    }
    
    this.challenges.clear();
    
    console.log('Relay server shutdown complete');
  }

  // ============================================
  // State Replication Methods (for elegant login)
  // ============================================

  /**
   * Get state fragment for a specific entity
   * This allows replicas to serve state for restoration
   * 
   * @param publicKey - The public key of the entity whose state is requested
   * @returns State fragment containing recognition edges
   */
  async getStateFor(publicKey: string): Promise<StateFragment> {
    // Find the session for this public key
    // In a real system, publicKey would map to an entityId
    // For simplicity, we'll assume the entityId is the publicKey (or a derivation of it)
    const session = this.sessions.get(publicKey);
    
    if (!session) {
      console.warn(`No session found for public key: ${publicKey}`);
      return {
        entityId: publicKey,
        recognitionEdges: [],
      };
    }

    // Get all recognition edges from the session
    // This is a simplified version - in a real system, you'd have methods
    // to export the full recognition graph as edges with ITC timestamps
    const recognitionEdges: RecognitionEdge[] = [];
    
    // For now, return a mock fragment
    // In a production system, EntitySession would have a method like `exportRecognitionEdges()`
    console.log(`Providing state fragment for ${publicKey}`);
    
    return {
      entityId: publicKey,
      recognitionEdges,
    };
  }

  /**
   * Register a replica that can serve state for an entity
   * 
   * @param entityId - The entity ID of the replica
   * @param publicKey - The public key this replica stores state for
   * @param address - Network address of the replica (e.g., WebSocket URL)
   */
  async registerReplica(entityId: string, publicKey: string, address: string): Promise<void> {
    if (!this.replicas.has(publicKey)) {
      this.replicas.set(publicKey, []);
    }

    const replicaInfo: ReplicaInfo = {
      entityId,
      publicKey: entityId, // The replica's own public key
      address,
      reputation: 1.0, // Initial reputation
    };

    this.replicas.get(publicKey)!.push(replicaInfo);
    console.log(`Registered replica ${entityId} for ${publicKey} at ${address}`);
  }

  /**
   * Find replicas that can serve state for a given public key
   * 
   * @param publicKey - The public key to find replicas for
   * @returns Array of replica information
   */
  async findReplicas(publicKey: string): Promise<ReplicaInfo[]> {
    const replicas = this.replicas.get(publicKey) || [];
    console.log(`Found ${replicas.length} replicas for ${publicKey}`);
    return replicas;
  }

  /**
   * Get Merkle root for an entity's state
   * This allows clients to verify state integrity
   * 
   * @param publicKey - The public key of the entity
   * @returns Merkle root hash
   */
  async getMerkleRootFor(publicKey: string): Promise<string> {
    const stateFragment = await this.getStateFor(publicKey);
    const merkleTree = buildMerkleTree(stateFragment.recognitionEdges);
    return getMerkleRoot(merkleTree);
  }

  /**
   * Update replica reputation based on correctness
   * This is a simple reputation system to incentivize honest replicas
   * 
   * @param entityId - The replica entity ID
   * @param publicKey - The public key the replica serves
   * @param delta - Change in reputation (-1 to +1)
   */
  async updateReplicaReputation(
    entityId: string,
    publicKey: string,
    delta: number
  ): Promise<void> {
    const replicas = this.replicas.get(publicKey);
    if (!replicas) return;

    const replica = replicas.find(r => r.entityId === entityId);
    if (!replica) return;

    replica.reputation = Math.max(0, Math.min(1, replica.reputation + delta));
    console.log(`Updated reputation for ${entityId}: ${replica.reputation}`);
  }
}

/**
 * Create a relay server instance
 */
export function createRelayServer(): RelayServer {
  return new RelayServer();
}

