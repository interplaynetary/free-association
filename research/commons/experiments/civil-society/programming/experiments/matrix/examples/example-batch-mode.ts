/**
 * HTTP Batch Mode Examples
 * 
 * Demonstrates lightweight batch queries without WebSocket overhead.
 * 
 * Run:
 * ```bash
 * bun run research/matrix/example-batch-mode.ts
 * ```
 */

import { LocalFirstBatchClient, createBatchClient } from '../client/batch-client';

const SERVER_URL = 'https://your-server.workers.dev/api';

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 1: Basic Batch Mode
// ═══════════════════════════════════════════════════════════════════

async function basicBatchMode() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 1: Basic Batch Mode');
  console.log('════════════════════════════════════════════════\n');
  
  console.log('Creating batch client (HTTP, no WebSocket)...');
  const batch = new LocalFirstBatchClient(SERVER_URL);
  await batch.initialize();
  
  console.log('Authenticating...');
  await batch.authenticate('alice@example.com', {
    type: 'password',
    data: 'secret123'
  });
  
  console.log('Making batch queries...\n');
  console.time('Batch queries');
  
  // Make multiple calls (all batched in single HTTP request)
  const mr1 = batch.getMutualRecognition('alice@example.com', 'bob@example.com');
  const mr2 = batch.getMutualRecognition('alice@example.com', 'carol@example.com');
  const total = batch.computeTotalMR('alice@example.com');
  
  // Await all results (single HTTP round trip!)
  const [mr1Result, mr2Result, totalResult] = await Promise.all([mr1, mr2, total]);
  
  console.timeEnd('Batch queries');
  
  console.log('\nResults:');
  console.log(`  MR(alice, bob): ${mr1Result}`);
  console.log(`  MR(alice, carol): ${mr2Result}`);
  console.log(`  Total MR(alice): ${totalResult}`);
  
  console.log('\n✅ 3 queries in 1 HTTP request!');
  
  batch.close();
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 2: Batch with Memoization
// ═══════════════════════════════════════════════════════════════════

async function batchWithMemoization() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 2: Batch with Memoization');
  console.log('════════════════════════════════════════════════\n');
  
  const batch = new LocalFirstBatchClient(SERVER_URL, {
    enableMemoization: true,
    memoizationTTL: 60000 // 1 minute
  });
  
  await batch.initialize();
  await batch.authenticate('alice@example.com', {
    type: 'password',
    data: 'secret123'
  });
  
  console.log('First batch (from server)...');
  console.time('First batch');
  
  const mr1 = await batch.getMutualRecognition('alice@example.com', 'bob@example.com');
  
  console.timeEnd('First batch');
  console.log(`  Result: ${mr1}\n`);
  
  console.log('Second batch (from cache)...');
  console.time('Second batch');
  
  const mr2 = await batch.getMutualRecognition('alice@example.com', 'bob@example.com');
  
  console.timeEnd('Second batch');
  console.log(`  Result: ${mr2}\n`);
  
  console.log('✨ Second call instant (from memoization)!');
  
  // Check cache stats
  const stats = batch.getCacheStats();
  console.log('\nCache Stats:');
  console.log(`  Size: ${stats.memoization.size}`);
  console.log(`  Total Hits: ${stats.memoization.totalHits}`);
  console.log(`  Avg Hits: ${stats.memoization.avgHits.toFixed(2)}`);
  
  batch.close();
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 3: Quick Batch Helper
// ═══════════════════════════════════════════════════════════════════

async function quickBatchHelper() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 3: Quick Batch Helper');
  console.log('════════════════════════════════════════════════\n');
  
  console.log('Creating batch with auto-authentication...');
  
  const batch = await createBatchClient(
    SERVER_URL,
    'alice@example.com',
    { type: 'password', data: 'secret123' }
  );
  
  console.log('Making queries...');
  
  const [mr, total] = await Promise.all([
    batch.getMutualRecognition('alice@example.com', 'bob@example.com'),
    batch.computeTotalMR('alice@example.com')
  ]);
  
  console.log('\nResults:');
  console.log(`  MR: ${mr}`);
  console.log(`  Total: ${total}`);
  
  console.log('\n✅ Super simple one-liner setup!');
  
  batch.close();
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 4: Batch vs WebSocket Comparison
// ═══════════════════════════════════════════════════════════════════

async function batchVsWebSocket() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 4: Batch vs WebSocket Comparison');
  console.log('════════════════════════════════════════════════\n');
  
  console.log('📊 When to Use Each:\n');
  
  console.log('✅ Use HTTP Batch Mode:');
  console.log('  • One-time queries');
  console.log('  • Simple read operations');
  console.log('  • No real-time updates needed');
  console.log('  • Lower overhead');
  console.log('  • Easier deployment (standard HTTP)');
  
  console.log('\n✅ Use WebSocket Mode:');
  console.log('  • Ongoing connection');
  console.log('  • Real-time updates');
  console.log('  • Bidirectional communication');
  console.log('  • Frequent calls');
  console.log('  • Lower latency per call\n');
  
  // Demonstrate batch efficiency
  console.log('Example: Dashboard Metrics Query\n');
  
  const batch = await createBatchClient(
    SERVER_URL,
    'alice@example.com',
    { type: 'password', data: 'secret123' }
  );
  
  console.time('Load dashboard');
  
  const metrics = await Promise.all([
    batch.getMutualRecognition('alice@example.com', 'bob@example.com'),
    batch.getMutualRecognition('alice@example.com', 'carol@example.com'),
    batch.getMutualRecognition('alice@example.com', 'dave@example.com'),
    batch.computeTotalMR('alice@example.com'),
    batch.computeMRS('alice@example.com', 'bob@example.com')
  ]);
  
  console.timeEnd('Load dashboard');
  
  console.log(`\nLoaded 5 metrics in single HTTP request!`);
  console.log('Perfect for dashboard/analytics use cases ✨');
  
  batch.close();
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 5: Batch with Persistent Cache
// ═══════════════════════════════════════════════════════════════════

async function batchWithPersistentCache() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 5: Batch with Persistent Cache');
  console.log('════════════════════════════════════════════════\n');
  
  const batch = new LocalFirstBatchClient(SERVER_URL, {
    enableMemoization: true,
    enablePersistentCache: true
  });
  
  await batch.initialize();
  await batch.authenticate('alice@example.com', {
    type: 'password',
    data: 'secret123'
  });
  
  console.log('First run (fetches from server, caches persistently)...');
  const mr1 = await batch.getMutualRecognition('alice@example.com', 'bob@example.com');
  console.log(`  Result: ${mr1}\n`);
  
  console.log('Simulating app reload...');
  batch.clearCache(); // Clear memory cache
  console.log('Memory cache cleared.\n');
  
  console.log('Second run (loads from IndexedDB)...');
  const mr2 = await batch.getMutualRecognition('alice@example.com', 'bob@example.com');
  console.log(`  Result: ${mr2}\n`);
  
  console.log('✅ Persistent cache survives app reloads!');
  console.log('Works offline too! 📵');
  
  batch.close();
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 6: Multiple Batch Clients
// ═══════════════════════════════════════════════════════════════════

async function multipleBatchClients() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 6: Multiple Batch Clients');
  console.log('════════════════════════════════════════════════\n');
  
  console.log('Creating multiple batch clients for different users...\n');
  
  const aliceBatch = await createBatchClient(
    SERVER_URL,
    'alice@example.com',
    { type: 'password', data: 'secret123' }
  );
  
  const bobBatch = await createBatchClient(
    SERVER_URL,
    'bob@example.com',
    { type: 'password', data: 'secret456' }
  );
  
  console.log('Querying from both perspectives...');
  
  const [aliceToBob, bobToAlice] = await Promise.all([
    aliceBatch.getMutualRecognition('alice@example.com', 'bob@example.com'),
    bobBatch.getMutualRecognition('bob@example.com', 'alice@example.com')
  ]);
  
  console.log('\nResults:');
  console.log(`  Alice's view of MR with Bob: ${aliceToBob}`);
  console.log(`  Bob's view of MR with Alice: ${bobToAlice}`);
  console.log(`  Symmetric: ${aliceToBob === bobToAlice ? '✅ Yes!' : '❌ No'}`);
  
  aliceBatch.close();
  bobBatch.close();
}

// ═══════════════════════════════════════════════════════════════════
// RUN ALL EXAMPLES
// ═══════════════════════════════════════════════════════════════════

async function runAllExamples() {
  try {
    await basicBatchMode();
    await batchWithMemoization();
    await quickBatchHelper();
    await batchVsWebSocket();
    await batchWithPersistentCache();
    await multipleBatchClients();
    
    console.log('\n════════════════════════════════════════════════');
    console.log('✅ All batch mode examples completed!');
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
  basicBatchMode,
  batchWithMemoization,
  quickBatchHelper,
  batchVsWebSocket,
  batchWithPersistentCache,
  multipleBatchClients
};

