/**
 * Local-First Client Example
 * 
 * Demonstrates the magic of:
 * 1. Memoization (instant responses)
 * 2. IndexedDB (offline support)
 * 3. Background sync (eventual consistency)
 * 
 * Run this example:
 * ```bash
 * bun run research/matrix/example-local-first.ts
 * ```
 */

import { newWebSocketRpcSession } from 'capnweb';
import type { RpcStub } from 'capnweb';
import type { IParticipantServer } from '../rpc/interfaces';
import { LocalFirstClient } from '../client';

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 1: Basic Usage
// ═══════════════════════════════════════════════════════════════════

async function basicExample() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 1: Basic Usage');
  console.log('════════════════════════════════════════════════\n');
  
  // Connect to RPC server
  const serverStub = newWebSocketRpcSession<IParticipantServer>(
    'wss://your-server.workers.dev/api'
  );
  
  // Create local-first client
  const client = new LocalFirstClient(serverStub, {
    enableMemoization: true,
    enablePersistentCache: true,
    enableBackgroundSync: true
  });
  
  // Initialize (loads cached data)
  await client.initialize();
  
  // Authenticate
  await client.authenticate('alice@example.com', {
    type: 'password',
    data: 'secretPassword123'
  });
  
  // ✨ FIRST CALL: Server (slow)
  console.time('First call (server)');
  const mr1 = await client.getMutualRecognition('alice@example.com', 'bob@example.com');
  console.timeEnd('First call (server)');
  console.log(`MR(alice, bob) = ${mr1}\n`);
  
  // ✨ SECOND CALL: Memoized (instant!)
  console.time('Second call (memoized)');
  const mr2 = await client.getMutualRecognition('alice@example.com', 'bob@example.com');
  console.timeEnd('Second call (memoized)');
  console.log(`MR(alice, bob) = ${mr2}\n`);
  
  // Results should be identical
  console.log(`Same result: ${mr1 === mr2} ✅\n`);
  
  client.close();
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 2: Optimistic Updates
// ═══════════════════════════════════════════════════════════════════

async function optimisticExample() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 2: Optimistic Updates');
  console.log('════════════════════════════════════════════════\n');
  
  const serverStub = newWebSocketRpcSession<IParticipantServer>(
    'wss://your-server.workers.dev/api'
  );
  
  const client = new LocalFirstClient(serverStub, {
    enableOptimistic: true
  });
  
  await client.initialize();
  await client.authenticate('alice@example.com', {
    type: 'password',
    data: 'secretPassword123'
  });
  
  // ⚡ INSTANT UI UPDATE
  console.log('Allocating recognition (optimistic)...');
  console.time('Optimistic allocation');
  
  const result = await client.allocateRecognitionOptimistic('bob@example.com', 0.6);
  
  console.timeEnd('Optimistic allocation');
  console.log(`Immediate: ${result.immediate}`);
  console.log(`Syncing in background: ${result.syncing}\n`);
  
  // Check sync status
  const status = client.getSyncStatus();
  console.log('Sync Status:', status);
  
  // Wait for sync to complete
  console.log('\nWaiting for background sync...');
  await new Promise(resolve => setTimeout(resolve, 6000));
  
  const finalStatus = client.getSyncStatus();
  console.log('Final Sync Status:', finalStatus);
  
  client.close();
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 3: Offline Support
// ═══════════════════════════════════════════════════════════════════

async function offlineExample() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 3: Offline Support');
  console.log('════════════════════════════════════════════════\n');
  
  const serverStub = newWebSocketRpcSession<IParticipantServer>(
    'wss://your-server.workers.dev/api'
  );
  
  const client = new LocalFirstClient(serverStub);
  
  await client.initialize();
  await client.authenticate('alice@example.com', {
    type: 'password',
    data: 'secretPassword123'
  });
  
  // Fetch data while online
  console.log('📡 Online: Fetching data from server...');
  const mrOnline = await client.getMutualRecognition('alice@example.com', 'bob@example.com');
  console.log(`MR (online) = ${mrOnline}\n`);
  
  // Simulate going offline
  console.log('📵 Simulating offline mode...');
  console.log('(In real app, this would be: navigator.onLine = false)\n');
  
  // Data still available from cache!
  console.log('📖 Offline: Reading from cache...');
  const mrOffline = await client.getMutualRecognition('alice@example.com', 'bob@example.com');
  console.log(`MR (offline) = ${mrOffline}`);
  console.log(`✅ Works offline! Same value: ${mrOnline === mrOffline}\n`);
  
  client.close();
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 4: Cache Statistics
// ═══════════════════════════════════════════════════════════════════

async function cacheStatsExample() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 4: Cache Statistics');
  console.log('════════════════════════════════════════════════\n');
  
  const serverStub = newWebSocketRpcSession<IParticipantServer>(
    'wss://your-server.workers.dev/api'
  );
  
  const client = new LocalFirstClient(serverStub);
  
  await client.initialize();
  await client.authenticate('alice@example.com', {
    type: 'password',
    data: 'secretPassword123'
  });
  
  // Make several calls
  console.log('Making several MR calls...\n');
  
  await client.getMutualRecognition('alice@example.com', 'bob@example.com');
  await client.getMutualRecognition('alice@example.com', 'carol@example.com');
  await client.getMutualRecognition('alice@example.com', 'bob@example.com'); // Cache hit!
  await client.getMutualRecognition('alice@example.com', 'carol@example.com'); // Cache hit!
  await client.getMutualRecognition('bob@example.com', 'carol@example.com');
  
  // Get cache statistics
  const stats = await client.getCacheStats();
  
  console.log('📊 Cache Statistics:');
  console.log('═══════════════════════\n');
  
  console.log('Memoization Cache:');
  console.log(`  Mutual Recognition:`);
  console.log(`    Size: ${stats.memoization.mutualRecognition.size}`);
  console.log(`    Total Hits: ${stats.memoization.mutualRecognition.totalHits}`);
  console.log(`    Avg Hits: ${stats.memoization.mutualRecognition.avgHits.toFixed(2)}\n`);
  
  console.log('Persistent Cache:');
  console.log(`  Network States: ${stats.persistent.networkStateCount}`);
  console.log(`  Commitments: ${stats.persistent.commitmentsCount}`);
  console.log(`  Allocations: ${stats.persistent.allocationsCount}`);
  console.log(`  Computations: ${stats.persistent.computationsCount}\n`);
  
  client.close();
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 5: Performance Comparison
// ═══════════════════════════════════════════════════════════════════

async function performanceExample() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 5: Performance Comparison');
  console.log('════════════════════════════════════════════════\n');
  
  const serverStub = newWebSocketRpcSession<IParticipantServer>(
    'wss://your-server.workers.dev/api'
  );
  
  const client = new LocalFirstClient(serverStub);
  
  await client.initialize();
  await client.authenticate('alice@example.com', {
    type: 'password',
    data: 'secretPassword123'
  });
  
  console.log('🏃 Running performance test...\n');
  
  // Test 1: First call (server)
  const start1 = performance.now();
  await client.getMutualRecognition('alice@example.com', 'bob@example.com');
  const time1 = performance.now() - start1;
  
  console.log(`First call (server):     ${time1.toFixed(2)}ms`);
  
  // Test 2: Memoized calls
  const memoizedTimes: number[] = [];
  for (let i = 0; i < 10; i++) {
    const start = performance.now();
    await client.getMutualRecognition('alice@example.com', 'bob@example.com');
    memoizedTimes.push(performance.now() - start);
  }
  
  const avgMemoized = memoizedTimes.reduce((a, b) => a + b, 0) / memoizedTimes.length;
  console.log(`Memoized (avg of 10):    ${avgMemoized.toFixed(2)}ms`);
  
  // Speedup
  const speedup = time1 / avgMemoized;
  console.log(`\n✨ Speedup: ${speedup.toFixed(0)}× faster!`);
  
  client.close();
}

// ═══════════════════════════════════════════════════════════════════
// RUN ALL EXAMPLES
// ═══════════════════════════════════════════════════════════════════

async function runAllExamples() {
  try {
    await basicExample();
    await optimisticExample();
    await offlineExample();
    await cacheStatsExample();
    await performanceExample();
    
    console.log('\n════════════════════════════════════════════════');
    console.log('✅ All examples completed successfully!');
    console.log('════════════════════════════════════════════════\n');
    
  } catch (error) {
    console.error('❌ Error running examples:', error);
    process.exit(1);
  }
}

// Run if executed directly
if (import.meta.main) {
  runAllExamples();
}

export {
  basicExample,
  optimisticExample,
  offlineExample,
  cacheStatsExample,
  performanceExample
};

