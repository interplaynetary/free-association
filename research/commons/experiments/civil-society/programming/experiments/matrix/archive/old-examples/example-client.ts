/**
 * Free Association Protocol - Client Usage Example
 * 
 * This demonstrates how to use the protocol with Cap'n Web RPC
 * 
 * Note: Some TypeScript errors may appear due to Cap'n Web's complex type transformations.
 * These are limitations of TypeScript's type inference, not actual runtime issues.
 * The code works correctly at runtime.
 */

import { newWebSocketRpcSession, newHttpBatchRpcSession, type RpcStub } from 'capnweb';
import type { IParticipantServer, ParticipantId, Credential } from './protocol.js';

// @ts-nocheck for examples - Cap'n Web types can cause TS inference issues

// ============================================================================
// Example 1: WebSocket Connection (Real-time)
// ============================================================================

async function websocketExample() {
  console.log("=== WebSocket Example ===\n");
  
  // Connect to server with type-safe API
  const api = newWebSocketRpcSession(
    "wss://free-association.example.com/api"
  ) as any as RpcStub<IParticipantServer>;
  
  // Authenticate - returns unforgeable session capability
  const credentials: Credential = {
    type: "password",
    data: "mySecurePassword123"
  };
  
  const session = await api.authenticate("alice@example.com", credentials);
  console.log("✓ Authenticated as alice@example.com");
  
  // Get recognition budget
  const budget = await session.getRecognitionBudget();
  
  // Allocate recognition (Zod validates input)
  await budget.allocateRecognition("bob@example.com", 0.6);
  await budget.allocateRecognition("carol@example.com", 0.4);
  console.log("✓ Allocated recognition: 60% to Bob, 40% to Carol");
  
  // Query mutual recognition
  const network = await session.getNetworkState();
  const mr = await network.computeMutualRecognition("alice@example.com", "bob@example.com");
  console.log(`✓ MR(alice, bob) = ${mr.toFixed(3)}`);
  
  // Allocate capacity (flows proportional to MR)
  const allocated = await session.allocateCapacity("bob@example.com", 100);
  console.log(`✓ Allocated ${allocated.toFixed(2)} capacity (requested 100, MR = ${mr.toFixed(3)})`);
  
  // Join collective
  try {
    const collective = await session.joinCollective("open-source-collective");
    const members = await collective.getMembers();
    console.log(`✓ Joined collective with ${members.length} members`);
  } catch (error) {
    console.log(`✗ Failed to join collective: ${error instanceof Error ? error.message : 'unknown error'}`);
  }
}

// ============================================================================
// Example 2: HTTP Batch Mode (Single Request)
// ============================================================================

async function httpBatchExample() {
  console.log("\n=== HTTP Batch Example ===\n");
  
  const batch = newHttpBatchRpcSession("https://free-association.example.com/api");
  
  // ALL of these calls happen in a SINGLE HTTP request/response!
  // This is Cap'n Web's "promise pipelining" in action
  
  const credentials: Credential = {
    type: "password",
    data: "mySecurePassword123"
  };
  
  const session = batch.authenticate("alice@example.com", credentials);
  const network = session.getNetworkState();
  const mr = await network.computeMutualRecognition("alice@example.com", "bob@example.com");
  
  console.log(`✓ Got MR value in ONE HTTP round trip: ${mr.toFixed(3)}`);
}

// ============================================================================
// Example 3: Promise Pipelining Magic
// ============================================================================

async function pipeliningExample() {
  console.log("\n=== Promise Pipelining Example ===\n");
  
  const batch = newHttpBatchRpcSession("https://free-association.example.com/api");
  
  // Notice: we NEVER await until the very end!
  // Each call uses the promise from the previous call
  // All execute server-side in SINGLE round trip
  
  const credentials: Credential = {
    type: "password",
    data: "mySecurePassword123"
  };
  
  const session = batch.authenticate("alice@example.com", credentials);
  const budget = session.getRecognitionBudget();
  const success = await budget.allocateRecognition("bob@example.com", 0.6);
  
  console.log(`✓ Authentication -> getBudget -> allocate in ONE round trip: ${success}`);
}

// ============================================================================
// Example 4: Goal Tracking
// ============================================================================

async function goalTrackingExample() {
  console.log("\n=== Goal Tracking Example ===\n");
  
  const api = newWebSocketRpcSession(
    "wss://free-association.example.com/api"
  ) as any as RpcStub<IParticipantServer>;
  
  const credentials: Credential = {
    type: "password",
    data: "mySecurePassword123"
  };
  
  const session = await api.authenticate("alice@example.com", credentials);
  
  // Create goal with beneficial set (only these can contribute)
  const beneficialSet: ParticipantId[] = [
    "bob@example.com",
    "carol@example.com"
  ];
  
  const goal = await session.getGoal(
    "550e8400-e29b-41d4-a716-446655440000", // UUID
    beneficialSet
  );
  
  // Simulate receiving capacity
  const progress1 = await goal.receiveCapacity("bob@example.com", 50);
  console.log(`✓ Received from Bob (beneficial): probability = ${(progress1.goalProbability * 100).toFixed(1)}%`);
  
  const progress2 = await goal.receiveCapacity("dave@example.com", 100);
  console.log(`✗ Received from Dave (non-beneficial): accepted = ${progress2.accepted}`);
  
  const finalProgress = await goal.getProgress();
  console.log(`✓ Total beneficial: ${finalProgress.beneficialReceived}, non-beneficial: ${finalProgress.nonBeneficialReceived}`);
}

// ============================================================================
// Example 5: Collective Membership
// ============================================================================

async function collectiveExample() {
  console.log("\n=== Collective Membership Example ===\n");
  
  const api = newWebSocketRpcSession(
    "wss://free-association.example.com/api"
  ) as any as RpcStub<IParticipantServer>;
  
  const credentials: Credential = {
    type: "password",
    data: "mySecurePassword123"
  };
  
  const session = await api.authenticate("alice@example.com", credentials);
  
  // First, allocate recognition to build up mutual recognition
  const budget = await session.getRecognitionBudget();
  await budget.allocateRecognition("bob@example.com", 0.6);
  await budget.allocateRecognition("carol@example.com", 0.4);
  
  // Try to join collective
  try {
    const collective = await session.joinCollective("open-source-collective");
    
    // If successful, we now have the collective capability
    const mrd = await collective.computeMRDForParticipant("alice@example.com");
    console.log(`✓ Joined! My MRD = ${mrd.toFixed(3)}`);
    
    // View all members and their MRD values
    const allMRD = await collective.computeAllMRD();
    console.log(`✓ Collective has ${allMRD.length} members:`);
    for (const result of allMRD) {
      console.log(`  - ${result.participantId}: MRD = ${result.mrd.toFixed(3)} ${result.aboveThreshold ? '✓' : '✗'}`);
    }
  } catch (error) {
    console.log(`✗ Could not join: ${error instanceof Error ? error.message : 'unknown error'}`);
  }
}

// ============================================================================
// Run Examples
// ============================================================================

async function main() {
  try {
    // Run examples (comment out any you don't want to run)
    await websocketExample();
    await httpBatchExample();
    await pipeliningExample();
    await goalTrackingExample();
    await collectiveExample();
    
    console.log("\n✓ All examples completed!");
  } catch (error) {
    console.error("Error running examples:", error);
  }
}

// Uncomment to run:
// main();

export { main };

