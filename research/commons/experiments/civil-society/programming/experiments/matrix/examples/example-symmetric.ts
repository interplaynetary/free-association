/**
 * Free Association Protocol - Fully Symmetric Example
 * 
 * Demonstrates how Cap'n Web's symmetric protocol enables any instance
 * to be BOTH client AND server simultaneously.
 * 
 * From rpc.md:
 * "Since Cap'n Web is a symmetric protocol, there is no well-defined 'client' 
 * or 'server' at the protocol level. There are just two parties exchanging 
 * messages across a connection."
 */

import { RpcTarget, newWebSocketRpcSession, type RpcStub } from 'capnweb';
import {
  ParticipantServer,
  AuthenticatedParticipant,
  type IAuthenticatedParticipant,
  type ICapacityEventCallback,
  type IRecognitionEventCallback,
  type ICollectiveEventCallback,
  type ParticipantId,
  type CollectiveId
} from '../protocol.js';

// ============================================================================
// CLIENT-SIDE RPC TARGETS (Clients are also servers!)
// ============================================================================

/**
 * Client-side event handler
 * 
 * This extends RpcTarget, making it RPC-accessible!
 * The "server" can call methods on this "client" object.
 */
class ParticipantEventHandler extends RpcTarget 
  implements ICapacityEventCallback, IRecognitionEventCallback, ICollectiveEventCallback {
  
  private readonly participantId: ParticipantId;
  private readonly eventLog: Array<{type: string, data: any, timestamp: number}> = [];
  
  constructor(participantId: ParticipantId) {
    super();
    this.participantId = participantId;
  }
  
  /**
   * SERVER → CLIENT RPC: Notify capacity received
   */
  async onCapacityReceived(fromId: ParticipantId, amount: number): Promise<void> {
    console.log(`  [${this.participantId}] 📥 RPC FROM SERVER: Received ${amount.toFixed(2)} from ${fromId}`);
    this.eventLog.push({
      type: 'capacity_received',
      data: { fromId, amount },
      timestamp: Date.now()
    });
  }
  
  /**
   * SERVER → CLIENT RPC: Notify capacity allocated
   */
  async onCapacityAllocated(toId: ParticipantId, amount: number): Promise<void> {
    console.log(`  [${this.participantId}] 💸 RPC FROM SERVER: Allocated ${amount.toFixed(2)} to ${toId}`);
    this.eventLog.push({
      type: 'capacity_allocated',
      data: { toId, amount },
      timestamp: Date.now()
    });
  }
  
  /**
   * SERVER → CLIENT RPC: Notify recognition received
   */
  async onRecognitionReceived(fromId: ParticipantId, amount: number): Promise<void> {
    console.log(`  [${this.participantId}] ⭐ RPC FROM SERVER: ${fromId} recognized me with ${amount}`);
    this.eventLog.push({
      type: 'recognition_received',
      data: { fromId, amount },
      timestamp: Date.now()
    });
  }
  
  /**
   * SERVER → CLIENT RPC: Notify recognition allocated
   */
  async onRecognitionAllocated(toId: ParticipantId, amount: number): Promise<void> {
    console.log(`  [${this.participantId}] ⭐ RPC FROM SERVER: I allocated ${amount} to ${toId}`);
    this.eventLog.push({
      type: 'recognition_allocated',
      data: { toId, amount },
      timestamp: Date.now()
    });
  }
  
  /**
   * SERVER → CLIENT RPC: Notify member joined collective
   */
  async onMemberJoined(collectiveId: CollectiveId, memberId: ParticipantId): Promise<void> {
    console.log(`  [${this.participantId}] 👥 RPC FROM SERVER: ${memberId} joined ${collectiveId}`);
    this.eventLog.push({
      type: 'member_joined',
      data: { collectiveId, memberId },
      timestamp: Date.now()
    });
  }
  
  /**
   * SERVER → CLIENT RPC: Notify member left collective
   */
  async onMemberLeft(collectiveId: CollectiveId, memberId: ParticipantId): Promise<void> {
    console.log(`  [${this.participantId}] 👥 RPC FROM SERVER: ${memberId} left ${collectiveId}`);
    this.eventLog.push({
      type: 'member_left',
      data: { collectiveId, memberId },
      timestamp: Date.now()
    });
  }
  
  /**
   * SERVER → CLIENT RPC: Notify collective updated
   */
  async onCollectiveUpdated(collectiveId: CollectiveId, members: ParticipantId[]): Promise<void> {
    console.log(`  [${this.participantId}] 👥 RPC FROM SERVER: ${collectiveId} now has ${members.length} members`);
    this.eventLog.push({
      type: 'collective_updated',
      data: { collectiveId, memberCount: members.length },
      timestamp: Date.now()
    });
  }
  
  /**
   * CLIENT exposes this method - SERVER can call it to query client state!
   */
  async getEventLog(): Promise<typeof this.eventLog> {
    return [...this.eventLog];
  }
  
  /**
   * CLIENT exposes this method - SERVER can ping client
   */
  async ping(): Promise<string> {
    return `pong from ${this.participantId}`;
  }
}

// ============================================================================
// EXAMPLE 1: Full Symmetric Bidirectional Communication
// ============================================================================

async function fullSymmetricExample() {
  console.log("\n" + "=".repeat(70));
  console.log("EXAMPLE 1: FULL SYMMETRIC BIDIRECTIONAL COMMUNICATION");
  console.log("=".repeat(70) + "\n");
  
  console.log("Setting up...\n");
  
  // In real usage:
  // 1. Server runs: wrangler dev (exports ParticipantServer at ID 0)
  // 2. Client connects: newWebSocketRpcSession(url)
  // 3. Client ALSO exports RpcTarget at ID 0 (ParticipantEventHandler)
  
  // Simulated for demo purposes:
  const server = new ParticipantServer();
  const api = server as any as RpcStub<typeof server>;
  
  // CLIENT-SIDE: Create event handler (extends RpcTarget!)
  const aliceHandler = new ParticipantEventHandler("alice@example.com");
  const bobHandler = new ParticipantEventHandler("bob@example.com");
  
  console.log("Step 1: Alice authenticates (CLIENT → SERVER RPC)");
  const aliceSession = await api.authenticate("alice@example.com", {
    type: "password",
    data: "alicePassword123"
  });
  console.log("  ✓ Alice authenticated\n");
  
  console.log("Step 2: Bob authenticates (CLIENT → SERVER RPC)");
  const bobSession = await api.authenticate("bob@example.com", {
    type: "password",
    data: "bobPassword123"
  });
  console.log("  ✓ Bob authenticated\n");
  
  console.log("Step 3: Alice subscribes with callback (CLIENT passes RpcTarget to SERVER)");
  const aliceBudget = await aliceSession.getRecognitionBudget();
  await aliceBudget.subscribe(aliceHandler as any as RpcStub<IRecognitionEventCallback>);
  await aliceSession.subscribeToCapacityEvents(aliceHandler as any as RpcStub<ICapacityEventCallback>);
  console.log("  ✓ Alice subscribed (server has capability to call alice!)\n");
  
  console.log("Step 4: Bob subscribes with callback (CLIENT passes RpcTarget to SERVER)");
  const bobBudget = await bobSession.getRecognitionBudget();
  await bobBudget.subscribe(bobHandler as any as RpcStub<IRecognitionEventCallback>);
  await bobSession.subscribeToCapacityEvents(bobHandler as any as RpcStub<ICapacityEventCallback>);
  console.log("  ✓ Bob subscribed (server has capability to call bob!)\n");
  
  console.log("Step 5: Alice allocates recognition to Bob (CLIENT → SERVER RPC)");
  console.log("         Server notifies Alice's callback (SERVER → CLIENT RPC!)");
  await aliceBudget.allocateRecognition("bob@example.com", 0.6);
  console.log();
  
  console.log("Step 6: Bob allocates recognition to Alice (CLIENT → SERVER RPC)");
  console.log("         Server notifies Bob's callback (SERVER → CLIENT RPC!)");
  await bobBudget.allocateRecognition("alice@example.com", 0.5);
  console.log();
  
  console.log("Step 7: Alice sends capacity to Bob");
  console.log("         Server notifies BOTH Alice's AND Bob's callbacks!");
  const allocated = await aliceSession.allocateCapacity("bob@example.com", 100);
  await bobSession.receiveCapacity("alice@example.com", allocated);
  console.log();
  
  console.log("Step 8: Server queries client state (SERVER → CLIENT RPC!)");
  const aliceEvents = await (aliceHandler as any).getEventLog();
  const bobEvents = await (bobHandler as any).getEventLog();
  console.log(`  ✓ Server retrieved Alice's event log: ${aliceEvents.length} events`);
  console.log(`  ✓ Server retrieved Bob's event log: ${bobEvents.length} events\n`);
  
  console.log("Step 9: Server pings clients (SERVER → CLIENT RPC!)");
  const alicePing = await (aliceHandler as any).ping();
  const bobPing = await (bobHandler as any).ping();
  console.log(`  ✓ Alice responded: "${alicePing}"`);
  console.log(`  ✓ Bob responded: "${bobPing}"\n`);
  
  console.log("=".repeat(70));
  console.log("ANALYSIS:");
  console.log("- CLIENT → SERVER: allocateRecognition, allocateCapacity, etc.");
  console.log("- SERVER → CLIENT: onRecognitionAllocated, onCapacityReceived, etc.");
  console.log("- Truly bidirectional: both sides call each other!");
  console.log("- No architectural distinction between 'client' and 'server'");
  console.log("=".repeat(70) + "\n");
}

// ============================================================================
// EXAMPLE 2: Peer-to-Peer (No Central Server)
// ============================================================================

/**
 * Minimal peer participant (both client AND server)
 * 
 * This is the SAME class running on both sides!
 */
class PeerParticipant extends RpcTarget {
  private readonly participantId: ParticipantId;
  private capacity: number;
  private recognitionAllocations: Map<ParticipantId, number> = new Map();
  private peerConnections: Map<ParticipantId, any> = new Map();
  
  constructor(participantId: ParticipantId, capacity: number = 1000) {
    super();
    this.participantId = participantId;
    this.capacity = capacity;
  }
  
  // ========== Methods callable by PEERS (symmetric!) ==========
  
  /**
   * Connect to another peer
   * SYMMETRIC: Either peer can initiate connection
   */
  async connectToPeer(peerStub: any): Promise<string> {
    const peerId = await peerStub.getParticipantId();
    this.peerConnections.set(peerId, peerStub);
    return `Connected: ${this.participantId} ↔ ${peerId}`;
  }
  
  /**
   * Allocate recognition to peer
   * PEER A → PEER B: "I recognize you with X"
   */
  async allocateRecognitionToPeer(targetId: ParticipantId, amount: number): Promise<boolean> {
    if (amount < 0 || amount > 1) throw new Error("Recognition must be in [0,1]");
    
    const currentTotal = Array.from(this.recognitionAllocations.values())
      .reduce((sum, val) => sum + val, 0);
    const existing = this.recognitionAllocations.get(targetId) || 0;
    
    if (currentTotal - existing + amount > 1.0001) {
      throw new Error("Budget violation");
    }
    
    this.recognitionAllocations.set(targetId, amount);
    
    // Notify the peer (PEER A → PEER B RPC!)
    const peerStub = this.peerConnections.get(targetId);
    if (peerStub) {
      await peerStub.onRecognitionReceived(this.participantId, amount);
    }
    
    return true;
  }
  
  /**
   * Receive recognition from peer
   * PEER B ← PEER A: "You recognized me!"
   */
  async onRecognitionReceived(fromId: ParticipantId, amount: number): Promise<void> {
    console.log(`    [${this.participantId}] ⭐ Peer ${fromId} recognizes me with ${amount}`);
  }
  
  /**
   * Send capacity to peer (with MR calculation)
   * PEER A → PEER B: Direct transfer
   */
  async sendCapacityToPeer(recipientStub: any, amount: number): Promise<number> {
    // Get mutual recognition (both peers query each other!)
    const recipientId = await recipientStub.getParticipantId();
    const myRecToThem = this.recognitionAllocations.get(recipientId) || 0;
    const theirRecToMe = await recipientStub.getRecognitionTo(this.participantId);
    const mr = Math.min(myRecToThem, theirRecToMe);
    
    // Flow proportional to MR (Axiom 3)
    const actualAmount = amount * mr;
    
    if (actualAmount > this.capacity) {
      throw new Error(`Insufficient capacity: ${actualAmount} > ${this.capacity}`);
    }
    
    this.capacity -= actualAmount;
    
    // Call peer to receive (PEER A → PEER B RPC!)
    await recipientStub.receiveCapacity(this.participantId, actualAmount);
    
    return actualAmount;
  }
  
  /**
   * Receive capacity from peer
   * PEER B ← PEER A: Direct receipt
   */
  async receiveCapacity(fromId: ParticipantId, amount: number): Promise<void> {
    this.capacity += amount;
    console.log(`    [${this.participantId}] 📥 Received ${amount.toFixed(2)} capacity from ${fromId}`);
  }
  
  /**
   * Get recognition I allocated to another peer
   */
  async getRecognitionTo(targetId: ParticipantId): Promise<number> {
    return this.recognitionAllocations.get(targetId) || 0;
  }
  
  /**
   * Get my participant ID
   */
  async getParticipantId(): Promise<ParticipantId> {
    return this.participantId;
  }
  
  /**
   * Get my current capacity
   */
  async getCapacity(): Promise<number> {
    return this.capacity;
  }
  
  /**
   * List my peer connections
   */
  async listPeers(): Promise<ParticipantId[]> {
    return Array.from(this.peerConnections.keys());
  }
}

// ============================================================================
// EXAMPLE 2A: Peer-to-Peer Network
// ============================================================================

async function peerToPeerExample() {
  console.log("\n" + "=".repeat(70));
  console.log("EXAMPLE 2: PEER-TO-PEER NETWORK (No central server!)");
  console.log("=".repeat(70) + "\n");
  
  console.log("Creating peers (each is BOTH client AND server)...\n");
  
  // Create 3 peers - each extends RpcTarget
  const alice = new PeerParticipant("alice@example.com", 1000);
  const bob = new PeerParticipant("bob@example.com", 1000);
  const carol = new PeerParticipant("carol@example.com", 1000);
  
  // In real usage:
  // const aliceStub = newWebSocketRpcSession("ws://alice-node:8787/api");
  // const bobStub = newWebSocketRpcSession("ws://bob-node:8787/api");
  // const carolStub = newWebSocketRpcSession("ws://carol-node:8787/api");
  
  // Simulated stubs for demo
  const aliceStub = alice as any as RpcStub<PeerParticipant>;
  const bobStub = bob as any as RpcStub<PeerParticipant>;
  const carolStub = carol as any as RpcStub<PeerParticipant>;
  
  console.log("Step 1: Establish peer connections (symmetric!)");
  await alice.connectToPeer(bobStub);
  await alice.connectToPeer(carolStub);
  await bob.connectToPeer(aliceStub);
  await bob.connectToPeer(carolStub);
  await carol.connectToPeer(aliceStub);
  await carol.connectToPeer(bobStub);
  console.log("  ✓ Full mesh network established\n");
  
  console.log("Step 2: Allocate recognition (peer-to-peer)");
  await alice.allocateRecognitionToPeer("bob@example.com", 0.6);
  await alice.allocateRecognitionToPeer("carol@example.com", 0.4);
  console.log();
  
  await bob.allocateRecognitionToPeer("alice@example.com", 0.5);
  await bob.allocateRecognitionToPeer("carol@example.com", 0.5);
  console.log();
  
  await carol.allocateRecognitionToPeer("alice@example.com", 0.3);
  await carol.allocateRecognitionToPeer("bob@example.com", 0.7);
  console.log();
  
  console.log("Step 3: Direct capacity transfers (peer-to-peer!)");
  console.log("  Alice → Bob:");
  const alice_to_bob = await alice.sendCapacityToPeer(bobStub, 200);
  console.log(`    Sent: ${alice_to_bob.toFixed(2)} (200 × MR)\n`);
  
  console.log("  Bob → Carol:");
  const bob_to_carol = await bob.sendCapacityToPeer(carolStub, 150);
  console.log(`    Sent: ${bob_to_carol.toFixed(2)} (150 × MR)\n`);
  
  console.log("  Carol → Alice:");
  const carol_to_alice = await carol.sendCapacityToPeer(aliceStub, 100);
  console.log(`    Sent: ${carol_to_alice.toFixed(2)} (100 × MR)\n`);
  
  console.log("Final capacities:");
  console.log(`  Alice: ${await alice.getCapacity()}`);
  console.log(`  Bob: ${await bob.getCapacity()}`);
  console.log(`  Carol: ${await carol.getCapacity()}\n`);
  
  console.log("=".repeat(70));
  console.log("KEY INSIGHT:");
  console.log("- Each peer runs THE SAME CODE (PeerParticipant)");
  console.log("- Each peer is BOTH client (calls others) AND server (receives calls)");
  console.log("- No architectural distinction - completely symmetric!");
  console.log("- Transfers happen peer-to-peer, no intermediary!");
  console.log("=".repeat(70) + "\n");
}

// ============================================================================
// EXAMPLE 2B: Hybrid Architecture
// ============================================================================

/**
 * Network coordinator (for discovery only)
 * Participants connect peer-to-peer after discovery
 */
class NetworkCoordinator extends RpcTarget {
  private participants: Map<ParticipantId, any> = new Map();
  
  async registerPeer(id: ParticipantId, stub: any): Promise<void> {
    this.participants.set(id, stub);
    console.log(`  [Coordinator] ✓ Registered ${id}`);
  }
  
  async findPeer(id: ParticipantId): Promise<any | null> {
    return this.participants.get(id) || null;
  }
  
  async listPeers(): Promise<ParticipantId[]> {
    return Array.from(this.participants.keys());
  }
}

async function hybridExample() {
  console.log("\n" + "=".repeat(70));
  console.log("EXAMPLE 3: HYBRID ARCHITECTURE");
  console.log("=".repeat(70) + "\n");
  
  console.log("Architecture:");
  console.log("  Coordinator: Discovery only (who's available?)");
  console.log("  Peers: Direct communication after discovery\n");
  
  // Coordinator
  const coordinator = new NetworkCoordinator();
  const coordinatorStub: any = coordinator;
  
  // Peers
  const alice = new PeerParticipant("alice@example.com");
  const bob = new PeerParticipant("bob@example.com");
  
  const aliceStub = alice as any as RpcStub<PeerParticipant>;
  const bobStub = bob as any as RpcStub<PeerParticipant>;
  
  console.log("Step 1: Register with coordinator");
  await coordinatorStub.registerPeer("alice@example.com", aliceStub);
  await coordinatorStub.registerPeer("bob@example.com", bobStub);
  console.log();
  
  console.log("Step 2: Alice discovers Bob via coordinator");
  const bobFromCoordinator: any = await coordinatorStub.findPeer("bob@example.com");
  console.log("  ✓ Alice got Bob's stub from coordinator\n");
  
  console.log("Step 3: Alice connects DIRECTLY to Bob (peer-to-peer!)");
  if (bobFromCoordinator) {
    await alice.connectToPeer(bobFromCoordinator);
    await bob.connectToPeer(aliceStub);
    console.log("  ✓ Peer-to-peer connection established\n");
    
    console.log("Step 4: Direct transfer (coordinator NOT involved!)");
    await alice.allocateRecognitionToPeer("bob@example.com", 0.7);
    await bob.allocateRecognitionToPeer("alice@example.com", 0.6);
    console.log();
    
    const transferred = await alice.sendCapacityToPeer(bobFromCoordinator, 100);
    console.log(`  ✓ Transferred ${transferred.toFixed(2)} capacity peer-to-peer\n`);
  }
  
  console.log("=".repeat(70));
  console.log("KEY INSIGHT:");
  console.log("- Coordinator only for discovery");
  console.log("- After discovery, peers talk directly");
  console.log("- Scalable: coordinator doesn't handle capacity transfers");
  console.log("- Resilient: peers can continue even if coordinator goes down");
  console.log("=".repeat(70) + "\n");
}

// ============================================================================
// EXAMPLE 3: Export Table Demonstration
// ============================================================================

async function exportTableExample() {
  console.log("\n" + "=".repeat(70));
  console.log("EXAMPLE 4: EXPORT TABLE MECHANICS");
  console.log("=".repeat(70) + "\n");
  
  console.log("From rpc.md:");
  console.log("'Each side maintains an export table... Each entry has a signed");
  console.log(" integer ID... IDs can be negative... an ID is never reused.'\n");
  
  console.log("Initial state:");
  console.log("  Alice's exports: [0] = PeerParticipant");
  console.log("  Bob's exports:   [0] = PeerParticipant\n");
  
  const alice = new PeerParticipant("alice@example.com");
  const bob = new PeerParticipant("bob@example.com");
  
  const aliceStub = alice as any as RpcStub<PeerParticipant>;
  const bobStub = bob as any as RpcStub<PeerParticipant>;
  
  console.log("Step 1: Alice calls Bob");
  await aliceStub.connectToPeer(bobStub);
  console.log("  Alice's exports: [0] = Alice,  [-1] = bobStub");
  console.log("  Bob's imports:   [0] = Bob,    [Alice:-1] = aliceStub");
  console.log("  Bob can now call methods on Alice!\n");
  
  console.log("Step 2: Bob calls Alice back");
  await bobStub.connectToPeer(aliceStub);
  console.log("  Bob's exports:   [0] = Bob,    [-1] = aliceStub");
  console.log("  Alice's imports: [0] = Alice,  [Bob:-1] = bobStub");
  console.log("  Fully bidirectional!\n");
  
  console.log("Step 3: Alice allocates recognition (creates callback)");
  console.log("  If Alice passes a callback to Bob:");
  console.log("  Alice's exports: [0] = Alice, [-1] = bobStub, [-2] = callback");
  console.log("  Bob's imports:   [Alice:-2] = callback stub");
  console.log("  Bob can call the callback anytime!\n");
  
  console.log("=".repeat(70));
  console.log("KEY INSIGHT:");
  console.log("- Export table IDs are UNFORGEABLE (assigned by each side)");
  console.log("- Negative IDs = objects you passed to them");
  console.log("- Positive IDs = results of their method calls");
  console.log("- This makes capability security automatic!");
  console.log("=".repeat(70) + "\n");
}

// ============================================================================
// REAL DEPLOYMENT EXAMPLE
// ============================================================================

async function realDeploymentGuide() {
  console.log("\n" + "=".repeat(70));
  console.log("REAL DEPLOYMENT: How to run this in production");
  console.log("=".repeat(70) + "\n");
  
  console.log("OPTION 1: Centralized (Cloudflare Workers)\n");
  console.log("  Server:");
  console.log("    wrangler.toml:");
  console.log("      name = 'free-association'");
  console.log("      main = 'research/matrix/example-server.ts'\n");
  console.log("    Deploy: wrangler deploy");
  console.log("    URL: https://free-association.your-subdomain.workers.dev/api\n");
  
  console.log("  Client:");
  console.log("    import { newWebSocketRpcSession } from 'capnweb';");
  console.log("    const api = newWebSocketRpcSession('wss://free-association...');\n");
  
  console.log("OPTION 2: Peer-to-Peer (Each participant runs a node)\n");
  console.log("  Alice's node:");
  console.log("    const alice = new PeerParticipant('alice@example.com');");
  console.log("    const server = createWebSocketServer(alice); // Exports at ID 0");
  console.log("    server.listen(8787);\n");
  
  console.log("  Bob's node:");
  console.log("    const bob = new PeerParticipant('bob@example.com');");
  console.log("    const server = createWebSocketServer(bob);");
  console.log("    server.listen(8787);");
  console.log("    const aliceStub = newWebSocketRpcSession('ws://alice:8787');");
  console.log("    await bob.connectToPeer(aliceStub);\n");
  
  console.log("OPTION 3: Hybrid (Coordinator + Peer-to-Peer)\n");
  console.log("  Coordinator:");
  console.log("    Lightweight - just discovery");
  console.log("    Returns peer stubs");
  console.log("    Doesn't handle capacity transfers\n");
  
  console.log("  Peers:");
  console.log("    Find each other via coordinator");
  console.log("    Connect directly after discovery");
  console.log("    Transfer capacity peer-to-peer\n");
  
  console.log("=".repeat(70) + "\n");
}

// ============================================================================
// Run All Examples
// ============================================================================

async function main() {
  try {
    await fullSymmetricExample();
    await peerToPeerExample();
    await hybridExample();
    await exportTableExample();
    await realDeploymentGuide();
    
    console.log("✓ All symmetric protocol examples completed!\n");
  } catch (error) {
    console.error("Error:", error);
  }
}

// Uncomment to run:
// main();

export {
  ParticipantEventHandler,
  PeerParticipant,
  NetworkCoordinator,
  main
};

