/**
 * Fluent API Examples
 * 
 * Demonstrates elegant method chaining with pipelining and memoization.
 * 
 * Run:
 * ```bash
 * bun run research/matrix/example-fluent-api.ts
 * ```
 */

import { newWebSocketRpcSession } from 'capnweb';
import type { RpcStub } from 'capnweb';
import type { IParticipantServer } from '../rpc/interfaces';
import { createPipelinedClient } from '../client/pipelined-client';

const SERVER_URL = 'wss://your-server.workers.dev/api';

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 1: Fluent Chaining
// ═══════════════════════════════════════════════════════════════════

async function fluentChaining() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 1: Fluent Chaining');
  console.log('════════════════════════════════════════════════\n');
  
  const api = newWebSocketRpcSession<IParticipantServer>(SERVER_URL);
  
  console.log('❌ OLD WAY (verbose):');
  console.log(`
const session = await api.authenticate(id, creds);
const network = await session.getNetworkState();
const mr = await network.computeMutualRecognition("alice", "bob");
  `.trim());
  
  console.log('\n✅ NEW WAY (fluent):');
  console.log(`
const mr = await api
  .authenticate(id, creds)
  .getNetworkState()
  .computeMutualRecognition("alice", "bob");
  `.trim());
  
  console.log('\n📊 Result:');
  
  const mr = await api
    .authenticate('alice@example.com', {
      type: 'password',
      data: 'secret123'
    })
    .getNetworkState()
    .computeMutualRecognition('alice@example.com', 'bob@example.com');
  
  console.log(`  MR(alice, bob) = ${mr}`);
  console.log('\n✨ Much cleaner, single round trip!');
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 2: Fluent + Memoization
// ═══════════════════════════════════════════════════════════════════

async function fluentWithMemoization() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 2: Fluent + Memoization');
  console.log('════════════════════════════════════════════════\n');
  
  const api = newWebSocketRpcSession<IParticipantServer>(SERVER_URL);
  const pipelinedApi = createPipelinedClient(api);
  
  console.log('First call (server)...');
  console.time('First fluent call');
  
  const mr1 = await pipelinedApi
    .authenticate('alice@example.com', {
      type: 'password',
      data: 'secret123'
    })
    .getNetworkState()
    .computeMutualRecognition('alice@example.com', 'bob@example.com');
  
  console.timeEnd('First fluent call');
  console.log(`  Result: ${mr1}\n`);
  
  console.log('Second call (memoized)...');
  console.time('Second fluent call');
  
  const mr2 = await pipelinedApi
    .authenticate('alice@example.com', {
      type: 'password',
      data: 'secret123'
    })
    .getNetworkState()
    .computeMutualRecognition('alice@example.com', 'bob@example.com');
  
  console.timeEnd('Second fluent call');
  console.log(`  Result: ${mr2}\n`);
  
  console.log('✨ Fluent + memoized = best of both worlds!');
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 3: Parallel Fluent Chains
// ═══════════════════════════════════════════════════════════════════

async function parallelFluentChains() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 3: Parallel Fluent Chains');
  console.log('════════════════════════════════════════════════\n');
  
  const api = newWebSocketRpcSession<IParticipantServer>(SERVER_URL);
  
  console.log('Starting parallel fluent operations...');
  console.time('Parallel fluent');
  
  // Share the authentication
  const session = api.authenticate('alice@example.com', {
    type: 'password',
    data: 'secret123'
  });
  
  const network = session.getNetworkState();
  
  // Multiple parallel chains from same point
  const results = await Promise.all([
    network.computeMutualRecognition('alice@example.com', 'bob@example.com'),
    network.computeMutualRecognition('alice@example.com', 'carol@example.com'),
    network.computeMutualRecognition('bob@example.com', 'carol@example.com'),
    network.computeTotalMR('alice@example.com')
  ]);
  
  console.timeEnd('Parallel fluent');
  
  console.log('\nResults:');
  console.log(`  MR(alice, bob): ${results[0]}`);
  console.log(`  MR(alice, carol): ${results[1]}`);
  console.log(`  MR(bob, carol): ${results[2]}`);
  console.log(`  Total MR(alice): ${results[3]}`);
  
  console.log('\n✨ 4 queries in 1 round trip with elegant syntax!');
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 4: Branching Chains
// ═══════════════════════════════════════════════════════════════════

async function branchingChains() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 4: Branching Chains');
  console.log('════════════════════════════════════════════════\n');
  
  const api = newWebSocketRpcSession<IParticipantServer>(SERVER_URL);
  
  console.log('Creating branching chains...');
  
  // Authenticate once
  const session = api.authenticate('alice@example.com', {
    type: 'password',
    data: 'secret123'
  });
  
  // Branch 1: Network queries
  const networkBranch = session.getNetworkState();
  const mr = networkBranch.computeMutualRecognition('alice@example.com', 'bob@example.com');
  const total = networkBranch.computeTotalMR('alice@example.com');
  
  // Branch 2: Budget queries
  const budgetBranch = session.getRecognitionBudget();
  const recognition = budgetBranch.getRecognitionTo('bob@example.com');
  
  // Await all branches
  const [mrResult, totalResult, recognitionResult] = await Promise.all([
    mr,
    total,
    recognition
  ]);
  
  console.log('\nResults:');
  console.log(`  MR: ${mrResult}`);
  console.log(`  Total: ${totalResult}`);
  console.log(`  Recognition: ${recognitionResult}`);
  
  console.log('\n✨ Branching chains enable complex parallel queries!');
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 5: Real-World Pattern - Dashboard
// ═══════════════════════════════════════════════════════════════════

async function dashboardPattern() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 5: Real-World - Dashboard');
  console.log('════════════════════════════════════════════════\n');
  
  const api = newWebSocketRpcSession<IParticipantServer>(SERVER_URL);
  const pipelinedApi = createPipelinedClient(api);
  
  console.log('Loading dashboard metrics...');
  console.time('Dashboard load');
  
  // Authenticate
  const session = pipelinedApi.authenticate('alice@example.com', {
    type: 'password',
    data: 'secret123'
  });
  
  // Get state branches
  const network = session.getNetworkState();
  const budget = session.getRecognitionBudget();
  
  // Query all metrics in parallel (fluent syntax!)
  const metrics = await Promise.all([
    // Mutual recognition metrics
    network.computeMutualRecognition('alice@example.com', 'bob@example.com'),
    network.computeMutualRecognition('alice@example.com', 'carol@example.com'),
    network.computeTotalMR('alice@example.com'),
    
    // Budget metrics
    budget.getTotalAllocated(),
    budget.getRecognitionTo('bob@example.com'),
    budget.getRecognitionTo('carol@example.com')
  ]);
  
  console.timeEnd('Dashboard load');
  
  console.log('\nDashboard Metrics:');
  console.log('  Mutual Recognition:');
  console.log(`    • With Bob: ${metrics[0]}`);
  console.log(`    • With Carol: ${metrics[1]}`);
  console.log(`    • Total: ${metrics[2]}`);
  console.log('  Budget:');
  console.log(`    • Total Allocated: ${metrics[3]}`);
  console.log(`    • To Bob: ${metrics[4]}`);
  console.log(`    • To Carol: ${metrics[5]}`);
  
  console.log('\n✅ 6 metrics loaded in 1 round trip!');
  console.log('✨ Perfect for real-world dashboards!');
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 6: Error Handling in Fluent Chains
// ═══════════════════════════════════════════════════════════════════

async function errorHandling() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 6: Error Handling');
  console.log('════════════════════════════════════════════════\n');
  
  const api = newWebSocketRpcSession<IParticipantServer>(SERVER_URL);
  
  console.log('Testing error handling in fluent chains...\n');
  
  try {
    const result = await api
      .authenticate('invalid@example.com', {
        type: 'password',
        data: 'wrong'
      })
      .getNetworkState()
      .computeMutualRecognition('alice@example.com', 'bob@example.com');
    
    console.log(`Result: ${result}`);
    
  } catch (error) {
    console.log('✅ Error caught successfully!');
    console.log(`   Error: ${error instanceof Error ? error.message : error}`);
  }
  
  console.log('\n💡 Fluent chains work great with try/catch!');
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 7: Comparison Summary
// ═══════════════════════════════════════════════════════════════════

async function comparisonSummary() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 7: Comparison Summary');
  console.log('════════════════════════════════════════════════\n');
  
  console.log('📊 Fluent API Benefits:\n');
  
  console.log('1️⃣ Readability:');
  console.log('  Before: 5 lines, 3 variables');
  console.log('  After:  3 lines, 0 temp variables');
  
  console.log('\n2️⃣ Performance:');
  console.log('  Before: 3 round trips');
  console.log('  After:  1 round trip (3× faster)');
  
  console.log('\n3️⃣ Maintainability:');
  console.log('  Before: Hard to refactor chain');
  console.log('  After:  Easy to modify chain');
  
  console.log('\n4️⃣ Type Safety:');
  console.log('  Before: ✅ TypeScript types');
  console.log('  After:  ✅ TypeScript types (same!)');
  
  console.log('\n5️⃣ Memoization:');
  console.log('  Before: Manual cache checking');
  console.log('  After:  Automatic (transparent)');
  
  console.log('\n✨ Fluent API = Better DX + Better Performance!');
}

// ═══════════════════════════════════════════════════════════════════
// RUN ALL EXAMPLES
// ═══════════════════════════════════════════════════════════════════

async function runAllExamples() {
  try {
    await fluentChaining();
    await fluentWithMemoization();
    await parallelFluentChains();
    await branchingChains();
    await dashboardPattern();
    await errorHandling();
    await comparisonSummary();
    
    console.log('\n════════════════════════════════════════════════');
    console.log('✅ All fluent API examples completed!');
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
  fluentChaining,
  fluentWithMemoization,
  parallelFluentChains,
  branchingChains,
  dashboardPattern,
  errorHandling,
  comparisonSummary
};

