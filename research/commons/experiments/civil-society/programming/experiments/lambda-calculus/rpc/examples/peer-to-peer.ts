/**
 * Peer-to-Peer Example
 * 
 * Demonstrates symmetric P2P connection between two entities.
 * Both Alice and Bob:
 * - Connect to each other
 * - Authenticate mutually
 * - Allocate recognition
 * - Query each other's data
 */

import { PeerConnection, createP2PConnection } from '../peer-connection';
import type { Credential } from '../types';

/**
 * Example: Basic P2P connection
 */
export async function basicP2PExample() {
  console.log('=== Basic P2P Example ===\n');

  // Alice connects
  const alice = await createP2PConnection('alice', 'websocket', 'ws://localhost:8080');
  console.log('Alice connected');

  // Bob connects (to same relay)
  const bob = await createP2PConnection('bob', 'websocket', 'ws://localhost:8080');
  console.log('Bob connected\n');

  // Mutual authentication
  const aliceProof: Credential = {
    type: 'pubkey',
    publicKey: 'alice-public-key',
    signature: 'alice-signature',
    challenge: 'mutual-challenge-123'
  };

  const bobProof: Credential = {
    type: 'pubkey',
    publicKey: 'bob-public-key',
    signature: 'bob-signature',
    challenge: 'mutual-challenge-123'
  };

  await alice.mutualAuthenticate(aliceProof);
  await bob.mutualAuthenticate(bobProof);
  
  console.log('✓ Mutual authentication successful\n');

  // Alice allocates recognition to Bob
  await alice.getLocalSession().allocateRecognition('bob', 0.6);
  console.log('Alice allocated 0.6 to Bob');

  // Bob allocates recognition to Alice
  await bob.getLocalSession().allocateRecognition('alice', 0.8);
  console.log('Bob allocated 0.8 to Alice\n');

  // Query mutual recognition (should be min(0.6, 0.8) = 0.6)
  const mrFromAlice = await alice.getRemoteSession().getMutualRecognition('bob');
  const mrFromBob = await bob.getRemoteSession().getMutualRecognition('alice');

  console.log(`Mutual Recognition (Alice's view): ${mrFromAlice}`);
  console.log(`Mutual Recognition (Bob's view): ${mrFromBob}\n`);

  // Check budget status
  const aliceBudget = await alice.getLocalSession().getBudgetStatus();
  const bobBudget = await bob.getLocalSession().getBudgetStatus();

  console.log('Alice budget:', aliceBudget);
  console.log('Bob budget:', bobBudget);

  // Cleanup
  await alice.disconnect();
  await bob.disconnect();
}

/**
 * Example: Multi-entity network
 */
export async function multiEntityNetworkExample() {
  console.log('\n=== Multi-Entity Network Example ===\n');

  // Create 5 entities
  const entityIds = ['alice', 'bob', 'charlie', 'diana', 'eve'];
  const connections: PeerConnection[] = [];

  // Connect all entities
  for (const entityId of entityIds) {
    const conn = await createP2PConnection(entityId, 'websocket', 'ws://localhost:8080');
    connections.push(conn);
    console.log(`${entityId} connected`);
  }

  console.log('\nAll entities connected!\n');

  // Each entity allocates recognition to others
  // Alice allocates evenly
  await connections[0].getLocalSession().allocateRecognition('bob', 0.25);
  await connections[0].getLocalSession().allocateRecognition('charlie', 0.25);
  await connections[0].getLocalSession().allocateRecognition('diana', 0.25);
  await connections[0].getLocalSession().allocateRecognition('eve', 0.25);
  console.log('Alice allocated evenly to all');

  // Bob prefers Charlie and Diana
  await connections[1].getLocalSession().allocateRecognition('alice', 0.2);
  await connections[1].getLocalSession().allocateRecognition('charlie', 0.4);
  await connections[1].getLocalSession().allocateRecognition('diana', 0.4);
  console.log('Bob allocated to Alice (0.2), Charlie (0.4), Diana (0.4)');

  // Charlie prefers Alice and Bob
  await connections[2].getLocalSession().allocateRecognition('alice', 0.5);
  await connections[2].getLocalSession().allocateRecognition('bob', 0.5);
  console.log('Charlie allocated to Alice (0.5), Bob (0.5)\n');

  // Query MRS (Mutual Recognition Share) for Alice
  const aliceMRS = await connections[0].getLocalSession().getMRS(entityIds);
  console.log('Alice MRS:', aliceMRS);

  // Query TMR (Total Mutual Recognition) for Bob
  const bobTMR = await connections[1].getLocalSession().getTMR(entityIds);
  console.log('Bob TMR:', bobTMR);

  // Cleanup
  for (const conn of connections) {
    await conn.disconnect();
  }
}

/**
 * Example: Subscribe to updates
 */
export async function subscribeToUpdatesExample() {
  console.log('\n=== Subscribe to Updates Example ===\n');

  // Alice and Bob connect
  const alice = await createP2PConnection('alice');
  const bob = await createP2PConnection('bob');

  // Alice subscribes to Bob's updates
  await alice.getRemoteSession().subscribeSyncUpdates((update) => {
    console.log('[Alice received update from Bob]:', update);
  });

  // Bob subscribes to Alice's updates
  await bob.getRemoteSession().subscribeSyncUpdates((update) => {
    console.log('[Bob received update from Alice]:', update);
  });

  console.log('Subscriptions established\n');

  // Alice makes changes - Bob will be notified
  await alice.getLocalSession().allocateRecognition('bob', 0.7);
  console.log('Alice allocated 0.7 to Bob');

  // Bob makes changes - Alice will be notified
  await bob.getLocalSession().allocateRecognition('alice', 0.5);
  console.log('Bob allocated 0.5 to Alice\n');

  // Wait a bit for updates to propagate
  await new Promise(resolve => setTimeout(resolve, 1000));

  // Cleanup
  await alice.disconnect();
  await bob.disconnect();
}

/**
 * Run all examples
 */
export async function runP2PExamples() {
  try {
    await basicP2PExample();
    await multiEntityNetworkExample();
    await subscribeToUpdatesExample();
    
    console.log('\n✓ All P2P examples completed successfully!');
  } catch (error) {
    console.error('Error running examples:', error);
  }
}

// Run if executed directly
if (typeof require !== 'undefined' && require.main === module) {
  runP2PExamples();
}

