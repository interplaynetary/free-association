/**
 * Elegant Discovery - Cap'n Web Style
 * 
 * ReplicaNode: RpcTarget for state replicas
 * DiscoveryClient: Pipeline-optimized replica discovery
 */

import { RpcTarget } from '../rpc-target';
import type { EntityId } from '../types';
import type { ITCStamp } from '../clock';

/**
 * State fragment from a replica
 */
export interface StateFragment {
  entityId: string;
  edges: Map<EntityId, Map<EntityId, number>>; // Recognition edges
  timestamp: ITCStamp;
  merkleRoot: string;
  replicaId: string;
}

/**
 * Replica metadata
 */
export interface ReplicaInfo {
  id: string;
  address: string;
  reputation: number;
  lastSeen: number;
}

/**
 * ReplicaNode - RpcTarget representing a state replica
 * 
 * Follows Cap'n Web pattern - can be passed by reference over RPC.
 */
export class ReplicaNode extends RpcTarget {
  private replicaId: string;
  private address: string;
  private stateStore: Map<string, StateFragment>;
  private reputation: number;

  constructor(replicaId: string, address: string) {
    super();
    this.replicaId = replicaId;
    this.address = address;
    this.stateStore = new Map();
    this.reputation = 1.0; // Start with full reputation
  }

  /**
   * Get state fragment for a public key
   */
  async getStateFor(publicKey: string): Promise<StateFragment> {
    const fragment = this.stateStore.get(publicKey);
    if (!fragment) {
      throw new Error(`No state found for ${publicKey} on replica ${this.replicaId}`);
    }
    return fragment;
  }

  /**
   * Get the Merkle root for verification
   */
  async getMerkleRoot(): Promise<string> {
    // Return the Merkle root of all stored states
    // This is a simplified version - real implementation would compute from state
    return 'mock-merkle-root-' + this.replicaId;
  }

  /**
   * Get replica reputation score (0.0 to 1.0)
   */
  async getReputation(): Promise<number> {
    return this.reputation;
  }

  /**
   * Get MRS (Mutual Recognition Set) value for a public key
   * This is used for replica selection
   */
  async getMRS(publicKey: string): Promise<number> {
    // Return how much this replica recognizes the entity
    // In a real implementation, this would be computed from the recognition graph
    return 0.8; // Mock value
  }

  /**
   * Get replica info
   */
  getInfo(): ReplicaInfo {
    return {
      id: this.replicaId,
      address: this.address,
      reputation: this.reputation,
      lastSeen: Date.now()
    };
  }

  /**
   * Store state fragment (for testing/setup)
   * @internal
   */
  _storeState(publicKey: string, fragment: StateFragment): void {
    this.stateStore.set(publicKey, fragment);
  }

  /**
   * Update reputation (for Byzantine detection)
   * @internal
   */
  _updateReputation(delta: number): void {
    this.reputation = Math.max(0, Math.min(1, this.reputation + delta));
  }
}

/**
 * DiscoveryClient - Pipeline-optimized replica discovery
 * 
 * Uses Cap'n Web promise pipelining to minimize round trips.
 */
export class DiscoveryClient extends RpcTarget {
  private discoveryPeers: string[];
  private knownReplicas: Map<string, ReplicaNode>;

  constructor(discoveryPeers: string[]) {
    super();
    this.discoveryPeers = discoveryPeers;
    this.knownReplicas = new Map();
  }

  /**
   * Find replicas for a public key
   * 
   * Returns promise immediately for pipelining.
   */
  async findReplicas(publicKey: string): Promise<ReplicaNode[]> {
    // In a real implementation, this would query discovery peers
    // For now, return mock replicas
    const replicas: ReplicaNode[] = [];

    for (let i = 0; i < 3; i++) {
      const replicaId = `replica-${i}`;
      const address = `wss://replica-${i}.example.com`;
      
      let replica = this.knownReplicas.get(replicaId);
      if (!replica) {
        replica = new ReplicaNode(replicaId, address);
        this.knownReplicas.set(replicaId, replica);
      }

      replicas.push(replica);
    }

    return replicas;
  }

  /**
   * Get best replica based on reputation and MRS
   * 
   * Pipeline-optimized: doesn't need to await replicas first.
   */
  async getBestReplica(publicKey: string): Promise<ReplicaNode> {
    const replicas = await this.findReplicas(publicKey);

    // Score each replica: reputation * MRS
    const scores = await Promise.all(
      replicas.map(async (replica) => ({
        replica,
        score: (await replica.getReputation()) * (await replica.getMRS(publicKey))
      }))
    );

    // Return highest scoring replica
    const best = scores.reduce((best, current) =>
      current.score > best.score ? current : best
    );

    return best.replica;
  }

  /**
   * Get state fragments from all replicas
   * 
   * Pipeline-optimized: fetches in parallel.
   */
  async getFragments(publicKey: string): Promise<StateFragment[]> {
    const replicas = await this.findReplicas(publicKey);

    // Fetch fragments in parallel
    const fragmentPromises = replicas.map(replica =>
      replica.getStateFor(publicKey).catch(err => {
        console.warn(`Failed to get state from replica ${replica.getInfo().id}:`, err);
        return null;
      })
    );

    const fragments = await Promise.all(fragmentPromises);
    
    // Filter out failures
    return fragments.filter((f): f is StateFragment => f !== null);
  }

  /**
   * Get Merkle roots from all replicas for verification
   */
  async getMerkleRoots(publicKey: string): Promise<Map<string, string>> {
    const replicas = await this.findReplicas(publicKey);

    const rootPromises = replicas.map(async (replica) => ({
      replicaId: replica.getInfo().id,
      root: await replica.getMerkleRoot()
    }));

    const roots = await Promise.all(rootPromises);

    return new Map(roots.map(r => [r.replicaId, r.root]));
  }

  /**
   * Register a new replica (for testing/setup)
   */
  registerReplica(replica: ReplicaNode): void {
    this.knownReplicas.set(replica.getInfo().id, replica);
  }

  /**
   * Get all known replicas
   */
  getKnownReplicas(): ReplicaNode[] {
    return Array.from(this.knownReplicas.values());
  }
}

/**
 * Create a discovery client with default peers
 */
export function createDiscoveryClient(
  discoveryPeers?: string[]
): DiscoveryClient {
  const defaultPeers = [
    'wss://discovery1.freeassociation.network',
    'wss://discovery2.freeassociation.network',
    'wss://discovery3.freeassociation.network'
  ];

  return new DiscoveryClient(discoveryPeers || defaultPeers);
}

