/**
 * Offline-First Example
 * 
 * Demonstrates offline-first operation with sync queue.
 * Shows how operations are queued when offline and synced when back online.
 */

import { PeerConnection, createP2PConnection } from '../peer-connection';
import { BrowserStorage } from '../browser-storage';

/**
 * Example: Offline operations with sync queue
 */
export async function offlineOperationsExample() {
  console.log('=== Offline Operations Example ===\n');

  // Create storage for Alice
  const storage = new BrowserStorage('alice');
  await storage.initialize();

  // Alice connects
  const alice = await createP2PConnection('alice', 'websocket', 'ws://localhost:8080');
  console.log('Alice connected\n');

  // Make some allocations while online
  await alice.getLocalSession().allocateRecognition('bob', 0.4);
  await alice.getLocalSession().allocateRecognition('charlie', 0.3);
  console.log('Alice made allocations while online:');
  console.log('  - Bob: 0.4');
  console.log('  - Charlie: 0.3\n');

  // Simulate going offline
  console.log('📵 Alice goes offline...\n');
  await alice.disconnect();

  // Make changes while offline - they get queued
  try {
    await storage.setRecognitionEdge('alice', 'bob', 0.6); // Update
    await storage.setRecognitionEdge('alice', 'diana', 0.4); // New
    console.log('Alice made changes while offline:');
    console.log('  - Updated Bob: 0.6 (was 0.4)');
    console.log('  - Added Diana: 0.4');
    
    // Queue sync operations
    await storage.queueSync({
      type: 'allocate',
      fromId: 'alice',
      toId: 'bob',
      amount: 0.6,
      timestamp: Date.now(),
      vectorClock: storage.getLocalVectorClock()
    });
    
    await storage.queueSync({
      type: 'allocate',
      fromId: 'alice',
      toId: 'diana',
      amount: 0.4,
      timestamp: Date.now(),
      vectorClock: storage.getLocalVectorClock()
    });
    
    console.log('Changes queued for sync\n');
  } catch (error) {
    console.error('Error making offline changes:', error);
  }

  // Check sync queue
  const queue = await storage.getSyncQueue();
  console.log(`Sync queue has ${queue.length} pending operations\n`);

  // Reconnect - simulate coming back online
  console.log('📶 Alice reconnects...\n');
  const aliceReconnected = await createP2PConnection('alice', 'websocket', 'ws://localhost:8080');

  // Process sync queue
  console.log('Processing sync queue...');
  const synced = await aliceReconnected.getLocalSession().processSyncQueue(async (update) => {
    console.log('  Syncing:', update);
    // In real implementation, would send to remote peer
  });

  console.log(`✓ Synced ${synced} operations\n`);

  // Verify allocations
  const allocations = await aliceReconnected.getLocalSession().getMyAllocations();
  console.log('Alice final allocations:', allocations);

  // Cleanup
  await aliceReconnected.disconnect();
  storage.close();
}

/**
 * Example: Conflict resolution with vector clocks
 */
export async function conflictResolutionExample() {
  console.log('\n=== Conflict Resolution Example ===\n');

  // Both Alice and Bob start with same state
  const aliceStorage = new BrowserStorage('alice');
  const bobStorage = new BrowserStorage('bob');
  await aliceStorage.initialize();
  await bobStorage.initialize();

  // Set initial state
  await aliceStorage.setRecognitionEdge('alice', 'charlie', 0.5);
  await bobStorage.setRecognitionEdge('alice', 'charlie', 0.5);
  console.log('Initial state: Alice → Charlie = 0.5\n');

  // Both go offline and make conflicting changes
  console.log('Both Alice and Bob go offline and make changes...\n');

  // Alice updates to 0.7
  await aliceStorage.setRecognitionEdge('alice', 'charlie', 0.7);
  console.log('Alice (offline): Changed to 0.7');
  const aliceVClock = aliceStorage.getLocalVectorClock();
  console.log('  Vector clock:', aliceVClock);

  // Bob updates to 0.6
  await bobStorage.setRecognitionEdge('alice', 'charlie', 0.6);
  console.log('Bob (offline): Changed to 0.6');
  const bobVClock = bobStorage.getLocalVectorClock();
  console.log('  Vector clock:', bobVClock);

  console.log('\n⚠️  Conflict detected! Same edge updated by both\n');

  // They reconnect and sync
  console.log('Reconnecting and merging vector clocks...\n');

  // Merge vector clocks (CRDT-style)
  aliceStorage.mergeVectorClock(bobVClock);
  bobStorage.mergeVectorClock(aliceVClock);

  const mergedVClock = aliceStorage.getLocalVectorClock();
  console.log('Merged vector clock:', mergedVClock);
  console.log('Resolution strategy: Last-write-wins based on vector clock\n');

  // In real implementation, would use vector clock to resolve
  // For this demo, we just show the concept
  const aliceTimestamp = aliceVClock.alice || 0;
  const bobTimestamp = bobVClock.alice || 0;

  if (aliceTimestamp > bobTimestamp) {
    console.log('✓ Alice wins: Value is 0.7');
  } else if (bobTimestamp > aliceTimestamp) {
    console.log('✓ Bob wins: Value is 0.6');
  } else {
    console.log('✓ Tie: Use custom resolution (e.g., higher value wins)');
  }

  // Cleanup
  aliceStorage.close();
  bobStorage.close();
}

/**
 * Example: Batch sync after extended offline period
 */
export async function batchSyncExample() {
  console.log('\n=== Batch Sync Example ===\n');

  const storage = new BrowserStorage('alice');
  await storage.initialize();

  // Simulate many offline operations
  console.log('Alice makes 10 changes while offline...\n');

  const changes = [
    { to: 'bob', amount: 0.2 },
    { to: 'charlie', amount: 0.15 },
    { to: 'diana', amount: 0.1 },
    { to: 'eve', amount: 0.15 },
    { to: 'frank', amount: 0.1 },
    { to: 'bob', amount: 0.25 }, // Update Bob
    { to: 'grace', amount: 0.05 },
    { to: 'charlie', amount: 0.2 }, // Update Charlie
    { to: 'henry', amount: 0.1 },
    { to: 'diana', amount: 0.15 }, // Update Diana
  ];

  // Queue all operations
  for (const change of changes) {
    await storage.setRecognitionEdge('alice', change.to, change.amount);
    await storage.queueSync({
      type: 'allocate',
      fromId: 'alice',
      toId: change.to,
      amount: change.amount,
      timestamp: Date.now(),
      vectorClock: storage.getLocalVectorClock()
    });
    console.log(`  Queued: ${change.to} → ${change.amount}`);
  }

  console.log(`\n${changes.length} operations queued\n`);

  // Reconnect
  console.log('Alice reconnects and syncs...\n');
  const alice = await createP2PConnection('alice');

  // Batch sync (more efficient than individual syncs)
  const queue = await storage.getSyncQueue();
  console.log(`Processing ${queue.length} queued operations...`);

  let synced = 0;
  for (const item of queue) {
    // In real implementation, would batch these
    console.log(`  Syncing: ${item.operation.fromId} → ${item.operation.type === 'allocate' ? (item.operation as any).toId : '?'}`);
    if (item.id) {
      await storage.markSynced(item.id);
      synced++;
    }
  }

  console.log(`\n✓ Synced ${synced} operations in batch\n`);

  // Verify final state
  const finalAllocations = await storage.getOutgoingEdges('alice');
  console.log('Final allocations:');
  for (const [to, amount] of finalAllocations) {
    console.log(`  ${to}: ${amount}`);
  }

  // Cleanup
  await alice.disconnect();
  storage.close();
}

/**
 * Run all offline examples
 */
export async function runOfflineExamples() {
  try {
    await offlineOperationsExample();
    await conflictResolutionExample();
    await batchSyncExample();
    
    console.log('\n✓ All offline examples completed successfully!');
  } catch (error) {
    console.error('Error running offline examples:', error);
  }
}

// Run if executed directly
if (typeof require !== 'undefined' && require.main === module) {
  runOfflineExamples();
}

