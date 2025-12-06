/**
 * Free Association Protocol - Practical Peer-to-Peer Implementation
 * 
 * This demonstrates a REAL peer-to-peer network where:
 * - Each node runs both client AND server
 * - Nodes discover each other via coordinator
 * - Capacity transfers happen peer-to-peer
 * - Network is resilient to node failures
 */

import { RpcTarget, type RpcStub } from 'capnweb';
import {
  RecognitionBudget,
  NetworkState,
  AuthenticatedParticipant,
  Collective,
  ParticipantGoal,
  type ParticipantId,
  type CollectiveId,
  type GoalId,
  type Credential,
  type ICapacityEventCallback,
  type IRecognitionEventCallback,
  type ICollectiveEventCallback
} from '../protocol.js';

// ============================================================================
// PEER NODE (Both Client AND Server)
// ============================================================================

/**
 * A peer node in the Free Association network
 * 
 * Each node:
 * - Exports its API at RPC ID 0 (acts as server)
 * - Connects to other nodes (acts as client)
 * - Maintains local state
 * - Syncs with network state
 */
class FreeAssociationNode extends RpcTarget {
  // Local state
  private readonly participantId: ParticipantId;
  private readonly localNetwork: NetworkState;
  private session: AuthenticatedParticipant;
  
  // Peer connections
  private peers: Map<ParticipantId, any> = new Map();
  
  // Event callbacks
  private eventCallbacks: Set<any> = new Set();
  
  constructor(participantId: ParticipantId, initialCapacity: number = 1000) {
    super();
    this.participantId = participantId;
    
    // Each node maintains its own network view
    this.localNetwork = new NetworkState(100);
    
    // Create authenticated session
    this.session = new AuthenticatedParticipant(
      participantId,
      this.localNetwork,
      initialCapacity
    );
    
    console.log(`[${participantId}] Node initialized`);
  }
  
  // ========== Peer-to-Peer Connection Methods ==========
  
  /**
   * Connect to another peer node
   * SYMMETRIC: Either node can initiate
   */
  async connectToPeer(peerStub: any, peerId: ParticipantId): Promise<string> {
    this.peers.set(peerId, peerStub);
    
    // Register peer in local network view
    this.localNetwork.registerParticipant(peerId);
    
    console.log(`[${this.participantId}] Connected to peer: ${peerId}`);
    return `Connected: ${this.participantId} ↔ ${peerId}`;
  }
  
  /**
   * Get list of connected peers
   */
  async listPeers(): Promise<ParticipantId[]> {
    return Array.from(this.peers.keys());
  }
  
  /**
   * Get peer stub
   */
  async getPeer(peerId: ParticipantId): Promise<any | null> {
    return this.peers.get(peerId) || null;
  }
  
  // ========== Recognition Allocation (Peer-to-Peer) ==========
  
  /**
   * Allocate recognition to peer
   * Updates local network state and notifies peer
   */
  async allocateRecognitionToPeer(targetId: ParticipantId, amount: number): Promise<boolean> {
    const budget = await this.session.getRecognitionBudget();
    const success = await budget.allocateRecognition(targetId, amount);
    
    // Sync to matrix
    this.localNetwork.syncToMatrix();
    
    // Notify peer (peer-to-peer RPC!)
    const peerStub = this.peers.get(targetId);
    if (peerStub) {
      await peerStub.onRecognitionReceived(this.participantId, amount);
    }
    
    console.log(`[${this.participantId}] Allocated ${amount} recognition to ${targetId}`);
    
    // Notify local subscribers
    await this.notifyLocalCallbacks('recognition_allocated', targetId, amount);
    
    return success;
  }
  
  /**
   * Receive recognition from peer (callback from peer)
   */
  async onRecognitionReceived(fromId: ParticipantId, amount: number): Promise<void> {
    console.log(`[${this.participantId}] ⭐ Received ${amount} recognition from ${fromId}`);
    
    // Notify local subscribers
    await this.notifyLocalCallbacks('recognition_received', fromId, amount);
  }
  
  // ========== Capacity Transfers (Peer-to-Peer) ==========
  
  /**
   * Send capacity to peer
   * Direct peer-to-peer transfer with MR calculation
   */
  async sendCapacityToPeer(recipientId: ParticipantId, requestedAmount: number): Promise<number> {
    // Compute mutual recognition
    const mr = this.localNetwork.computeMutualRecognition(this.participantId, recipientId);
    
    // Allocate capacity (MR-proportional)
    const allocated = await this.session.allocateCapacity(recipientId, requestedAmount);
    
    // Transfer to peer (peer-to-peer RPC!)
    const peerStub = this.peers.get(recipientId);
    if (peerStub) {
      await peerStub.receiveCapacityFromPeer(this.participantId, allocated);
    }
    
    console.log(`[${this.participantId}] 💸 Sent ${allocated.toFixed(2)} capacity to ${recipientId} (MR=${mr.toFixed(3)})`);
    
    return allocated;
  }
  
  /**
   * Receive capacity from peer (callback from peer)
   */
  async receiveCapacityFromPeer(fromId: ParticipantId, amount: number): Promise<void> {
    await this.session.receiveCapacity(fromId, amount);
    
    console.log(`[${this.participantId}] 📥 Received ${amount.toFixed(2)} capacity from ${fromId}`);
    
    // Notify local subscribers
    await this.notifyLocalCallbacks('capacity_received', fromId, amount);
  }
  
  // ========== Collective Operations ==========
  
  /**
   * Create or join a collective
   */
  async joinCollective(collectiveId: CollectiveId, threshold: number = 0.5): Promise<boolean> {
    try {
      const collective = await this.session.joinCollective(collectiveId);
      console.log(`[${this.participantId}] ✅ Joined collective: ${collectiveId}`);
      
      // Notify peers in collective
      await this.broadcastToCollective(collectiveId, 'member_joined');
      
      return true;
    } catch (error: any) {
      console.log(`[${this.participantId}] ❌ Failed to join ${collectiveId}: ${error.message}`);
      return false;
    }
  }
  
  /**
   * Broadcast to all members of a collective
   */
  private async broadcastToCollective(collectiveId: CollectiveId, event: string): Promise<void> {
    // In real implementation, would query collective members and notify each
    console.log(`[${this.participantId}] Broadcasting '${event}' to ${collectiveId}`);
  }
  
  // ========== Goal Management ==========
  
  /**
   * Create a goal with beneficial set
   */
  async createGoal(goalId: GoalId, beneficialParticipantIds: ParticipantId[]): Promise<any> {
    const goal = await this.session.getGoal(goalId, beneficialParticipantIds);
    console.log(`[${this.participantId}] 🎯 Created goal ${goalId} with ${beneficialParticipantIds.length} beneficial participants`);
    return goal;
  }
  
  // ========== Network Queries ==========
  
  /**
   * Compute mutual recognition with peer
   */
  async computeMutualRecognitionWith(peerId: ParticipantId): Promise<number> {
    this.localNetwork.syncToMatrix();
    return this.localNetwork.computeMutualRecognition(this.participantId, peerId);
  }
  
  /**
   * Get current capacity
   */
  async getCapacity(): Promise<number> {
    return this.session.getCapacity();
  }
  
  /**
   * Get participant ID
   */
  async getParticipantId(): Promise<ParticipantId> {
    return this.participantId;
  }
  
  /**
   * Get network statistics
   */
  async getNetworkStats(): Promise<{
    participantId: ParticipantId;
    capacity: number;
    peers: number;
    recognitionAllocated: number;
  }> {
    const budget = await this.session.getRecognitionBudget();
    const allocated = await budget.getTotalAllocated();
    
    return {
      participantId: this.participantId,
      capacity: await this.session.getCapacity(),
      peers: this.peers.size,
      recognitionAllocated: allocated
    };
  }
  
  // ========== Event Handling ==========
  
  /**
   * Subscribe to node events
   */
  async subscribe(callback: any): Promise<void> {
    this.eventCallbacks.add(callback);
  }
  
  /**
   * Unsubscribe from events
   */
  async unsubscribe(callback: any): Promise<void> {
    this.eventCallbacks.delete(callback);
  }
  
  /**
   * Notify local event callbacks
   */
  private async notifyLocalCallbacks(event: string, ...args: any[]): Promise<void> {
    for (const callback of this.eventCallbacks) {
      try {
        if (typeof callback[event] === 'function') {
          await callback[event](...args);
        }
      } catch (error) {
        console.error(`[${this.participantId}] Error in callback:`, error);
      }
    }
  }
}

// ============================================================================
// COORDINATOR (Lightweight Discovery Service)
// ============================================================================

/**
 * Coordinator for peer discovery
 * 
 * IMPORTANT: This is lightweight and optional!
 * - Only used for initial peer discovery
 * - Doesn't handle capacity transfers
 * - Can go offline after peers connect
 */
class PeerCoordinator extends RpcTarget {
  private nodes: Map<ParticipantId, any> = new Map();
  
  /**
   * Register a node
   */
  async registerNode(participantId: ParticipantId, nodeStub: any): Promise<void> {
    this.nodes.set(participantId, nodeStub);
    console.log(`[Coordinator] ✓ Registered node: ${participantId}`);
  }
  
  /**
   * Find a node by ID
   */
  async findNode(participantId: ParticipantId): Promise<any | null> {
    return this.nodes.get(participantId) || null;
  }
  
  /**
   * List all registered nodes
   */
  async listNodes(): Promise<Array<{ id: ParticipantId; stub: any }>> {
    return Array.from(this.nodes.entries()).map(([id, stub]) => ({ id, stub }));
  }
  
  /**
   * Get network statistics
   */
  async getNetworkStats(): Promise<{
    totalNodes: number;
    nodeList: ParticipantId[];
  }> {
    return {
      totalNodes: this.nodes.size,
      nodeList: Array.from(this.nodes.keys())
    };
  }
}

// ============================================================================
// NODE EVENT LISTENER (Client-side callback handler)
// ============================================================================

/**
 * Event listener for node events
 * This runs on the "client" side and receives callbacks from nodes
 */
class NodeEventListener extends RpcTarget {
  private eventLog: Array<{ type: string; data: any; timestamp: number }> = [];
  
  async recognition_received(fromId: ParticipantId, amount: number): Promise<void> {
    this.eventLog.push({
      type: 'recognition_received',
      data: { fromId, amount },
      timestamp: Date.now()
    });
  }
  
  async recognition_allocated(toId: ParticipantId, amount: number): Promise<void> {
    this.eventLog.push({
      type: 'recognition_allocated',
      data: { toId, amount },
      timestamp: Date.now()
    });
  }
  
  async capacity_received(fromId: ParticipantId, amount: number): Promise<void> {
    this.eventLog.push({
      type: 'capacity_received',
      data: { fromId, amount },
      timestamp: Date.now()
    });
  }
  
  async getEventLog(): Promise<typeof this.eventLog> {
    return [...this.eventLog];
  }
}

// ============================================================================
// EXAMPLE: Build a Peer-to-Peer Network
// ============================================================================

async function demonstratePeerToPeerNetwork() {
  console.log("\n" + "=".repeat(80));
  console.log("PRACTICAL PEER-TO-PEER NETWORK DEMONSTRATION");
  console.log("=".repeat(80) + "\n");
  
  // ========== Setup Phase ==========
  
  console.log("PHASE 1: Setup Coordinator (optional discovery service)\n");
  
  const coordinator = new PeerCoordinator();
  const coordinatorStub: any = coordinator;
  
  console.log("✓ Coordinator running\n");
  
  // ========== Create Peer Nodes ==========
  
  console.log("PHASE 2: Create Peer Nodes\n");
  
  const alice = new FreeAssociationNode("alice@example.com", 1000);
  const bob = new FreeAssociationNode("bob@example.com", 1000);
  const carol = new FreeAssociationNode("carol@example.com", 1000);
  
  // Simulate RPC stubs (in real deployment, these would be WebSocket connections)
  const aliceStub: any = alice;
  const bobStub: any = bob;
  const carolStub: any = carol;
  
  console.log();
  
  // ========== Register with Coordinator ==========
  
  console.log("PHASE 3: Register Nodes with Coordinator\n");
  
  await coordinatorStub.registerNode("alice@example.com", aliceStub);
  await coordinatorStub.registerNode("bob@example.com", bobStub);
  await coordinatorStub.registerNode("carol@example.com", carolStub);
  
  const coordStats = await coordinatorStub.getNetworkStats();
  console.log(`\n✓ Coordinator has ${coordStats.totalNodes} registered nodes\n`);
  
  // ========== Peer Discovery ==========
  
  console.log("PHASE 4: Peer Discovery (via Coordinator)\n");
  
  // Alice discovers Bob and Carol
  const bobFromCoord: any = await coordinatorStub.findNode("bob@example.com");
  const carolFromCoord: any = await coordinatorStub.findNode("carol@example.com");
  console.log("[Alice] Discovered Bob and Carol\n");
  
  // Bob discovers Alice and Carol
  const aliceFromCoord: any = await coordinatorStub.findNode("alice@example.com");
  const carolFromCoord2: any = await coordinatorStub.findNode("carol@example.com");
  console.log("[Bob] Discovered Alice and Carol\n");
  
  // Carol discovers Alice and Bob
  const aliceFromCoord2: any = await coordinatorStub.findNode("alice@example.com");
  const bobFromCoord2: any = await coordinatorStub.findNode("bob@example.com");
  console.log("[Carol] Discovered Alice and Bob\n");
  
  // ========== Establish Peer-to-Peer Connections ==========
  
  console.log("PHASE 5: Establish Direct Peer-to-Peer Connections\n");
  console.log("(Coordinator no longer needed for these connections!)\n");
  
  // Full mesh network
  await alice.connectToPeer(bobStub, "bob@example.com");
  await alice.connectToPeer(carolStub, "carol@example.com");
  
  await bob.connectToPeer(aliceStub, "alice@example.com");
  await bob.connectToPeer(carolStub, "carol@example.com");
  
  await carol.connectToPeer(aliceStub, "alice@example.com");
  await carol.connectToPeer(bobStub, "bob@example.com");
  
  console.log("\n✓ Full mesh network established (3 nodes, 6 directed connections)\n");
  
  // ========== Recognition Allocation (Peer-to-Peer) ==========
  
  console.log("PHASE 6: Recognition Allocation (Direct Peer-to-Peer)\n");
  
  await alice.allocateRecognitionToPeer("bob@example.com", 0.6);
  await alice.allocateRecognitionToPeer("carol@example.com", 0.4);
  console.log();
  
  await bob.allocateRecognitionToPeer("alice@example.com", 0.5);
  await bob.allocateRecognitionToPeer("carol@example.com", 0.5);
  console.log();
  
  await carol.allocateRecognitionToPeer("alice@example.com", 0.3);
  await carol.allocateRecognitionToPeer("bob@example.com", 0.7);
  console.log();
  
  // ========== Compute Mutual Recognition ==========
  
  console.log("PHASE 7: Compute Mutual Recognition (Local Computation)\n");
  
  const mr_alice_bob = await alice.computeMutualRecognitionWith("bob@example.com");
  const mr_alice_carol = await alice.computeMutualRecognitionWith("carol@example.com");
  const mr_bob_carol = await bob.computeMutualRecognitionWith("carol@example.com");
  
  console.log(`MR(Alice, Bob)   = ${mr_alice_bob.toFixed(3)}`);
  console.log(`MR(Alice, Carol) = ${mr_alice_carol.toFixed(3)}`);
  console.log(`MR(Bob, Carol)   = ${mr_bob_carol.toFixed(3)}\n`);
  
  // ========== Capacity Transfers (Peer-to-Peer) ==========
  
  console.log("PHASE 8: Capacity Transfers (Direct Peer-to-Peer, NO coordinator!)\n");
  
  console.log("Transfer 1: Alice → Bob");
  const alice_to_bob = await alice.sendCapacityToPeer("bob@example.com", 200);
  console.log(`  Actual transfer: ${alice_to_bob.toFixed(2)} (200 × ${mr_alice_bob.toFixed(3)} MR)\n`);
  
  console.log("Transfer 2: Bob → Carol");
  const bob_to_carol = await bob.sendCapacityToPeer("carol@example.com", 150);
  console.log(`  Actual transfer: ${bob_to_carol.toFixed(2)} (150 × ${mr_bob_carol.toFixed(3)} MR)\n`);
  
  console.log("Transfer 3: Carol → Alice");
  const carol_to_alice = await carol.sendCapacityToPeer("alice@example.com", 100);
  console.log(`  Actual transfer: ${carol_to_alice.toFixed(2)} (100 × ${mr_alice_carol.toFixed(3)} MR)\n`);
  
  // ========== Network Statistics ==========
  
  console.log("PHASE 9: Network Statistics\n");
  
  const aliceStats = await alice.getNetworkStats();
  const bobStats = await bob.getNetworkStats();
  const carolStats = await carol.getNetworkStats();
  
  console.log("Alice:", aliceStats);
  console.log("Bob:  ", bobStats);
  console.log("Carol:", carolStats);
  console.log();
  
  // ========== Summary ==========
  
  console.log("=".repeat(80));
  console.log("SUMMARY: What Just Happened");
  console.log("=".repeat(80) + "\n");
  
  console.log("1. Coordinator (Lightweight):");
  console.log("   - Only used for initial discovery");
  console.log("   - Doesn't handle capacity transfers");
  console.log("   - Could go offline now, peers still work!\n");
  
  console.log("2. Peer-to-Peer Connections:");
  console.log("   - Direct connections between nodes");
  console.log("   - Each node is BOTH client AND server");
  console.log("   - Symmetric protocol (no architectural distinction)\n");
  
  console.log("3. Recognition & Capacity:");
  console.log("   - Recognition allocated peer-to-peer");
  console.log("   - MR computed locally at each node");
  console.log("   - Capacity transferred directly (coordinator NOT involved!)\n");
  
  console.log("4. Scalability:");
  console.log(`   - Coordinator: O(n) storage for node registry`);
  console.log(`   - Each node: O(connections) storage`);
  console.log(`   - Transfers: O(1) network hops (direct!)\n`);
  
  console.log("5. Resilience:");
  console.log("   - Coordinator failure: Peers continue operating");
  console.log("   - Node failure: Network continues with remaining peers");
  console.log("   - No single point of failure\n");
  
  console.log("=".repeat(80) + "\n");
}

// ============================================================================
// REAL DEPLOYMENT: Production Setup
// ============================================================================

async function realDeploymentGuide() {
  console.log("\n" + "=".repeat(80));
  console.log("REAL DEPLOYMENT GUIDE");
  console.log("=".repeat(80) + "\n");
  
  console.log("FILE STRUCTURE:\n");
  console.log("  research/matrix/");
  console.log("    ├── protocol.ts          # Core implementation");
  console.log("    ├── node-server.ts       # Run a peer node");
  console.log("    ├── coordinator.ts       # Optional coordinator");
  console.log("    └── wrangler.toml       # Cloudflare Workers config\n");
  
  console.log("NODE SERVER (node-server.ts):\n");
  console.log("  import { FreeAssociationNode } from './example-peer-to-peer.js';");
  console.log("  import { newWorkersRpcResponse } from 'capnweb';");
  console.log("");
  console.log("  const node = new FreeAssociationNode(");
  console.log("    process.env.PARTICIPANT_ID || 'node@example.com',");
  console.log("    1000");
  console.log("  );");
  console.log("");
  console.log("  export default {");
  console.log("    fetch(request: Request) {");
  console.log("      return newWorkersRpcResponse(request, node);");
  console.log("    }");
  console.log("  };\n");
  
  console.log("DEPLOY TO CLOUDFLARE WORKERS:\n");
  console.log("  # Each participant runs their own node");
  console.log("  wrangler deploy --name alice-node");
  console.log("  wrangler deploy --name bob-node");
  console.log("  wrangler deploy --name carol-node\n");
  
  console.log("CLIENT CONNECTION:\n");
  console.log("  import { newWebSocketRpcSession } from 'capnweb';");
  console.log("");
  console.log("  // Connect to your node");
  console.log("  const myNode = newWebSocketRpcSession('wss://alice-node.example.workers.dev');");
  console.log("");
  console.log("  // Discover peer");
  console.log("  const coordinator = newWebSocketRpcSession('wss://coordinator.example.workers.dev');");
  console.log("  const bobStub = await coordinator.findNode('bob@example.com');");
  console.log("");
  console.log("  // Connect to peer");
  console.log("  await myNode.connectToPeer(bobStub, 'bob@example.com');");
  console.log("");
  console.log("  // Transfer capacity peer-to-peer!");
  console.log("  await myNode.sendCapacityToPeer('bob@example.com', 100);\n");
  
  console.log("=".repeat(80) + "\n");
}

// ============================================================================
// Run Everything
// ============================================================================

async function main() {
  try {
    await demonstratePeerToPeerNetwork();
    await realDeploymentGuide();
    
    console.log("✓ All demonstrations completed!\n");
  } catch (error) {
    console.error("Error:", error);
  }
}

// Uncomment to run:
// main();

export {
  FreeAssociationNode,
  PeerCoordinator,
  NodeEventListener,
  main
};
