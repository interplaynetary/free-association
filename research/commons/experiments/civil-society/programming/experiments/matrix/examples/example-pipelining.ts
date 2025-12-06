/**
 * Promise Pipelining Examples
 * 
 * Demonstrates Cap'n Web's promise pipelining for single-round-trip operations.
 * 
 * Run:
 * ```bash
 * bun run research/matrix/example-pipelining.ts
 * ```
 */

import { newWebSocketRpcSession } from 'capnweb';
import type { RpcStub } from 'capnweb';
import type { IParticipantServer } from '../rpc/interfaces';
import { createPipelinedClient, pipeline } from '../client/pipelined-client';

const SERVER_URL = 'wss://your-server.workers.dev/api';

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 1: Basic Pipelining
// ═══════════════════════════════════════════════════════════════════

async function basicPipelining() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 1: Basic Pipelining');
  console.log('════════════════════════════════════════════════\n');
  
  const api = newWebSocketRpcSession<IParticipantServer>(SERVER_URL);
  
  console.log('❌ OLD WAY (3 round trips):');
  console.time('Without pipelining');
  
  const session1 = await api.authenticate('alice@example.com', {
    type: 'password',
    data: 'secret123'
  });
  const network1 = await session1.getNetworkState();
  const mr1 = await network1.computeMutualRecognition('alice@example.com', 'bob@example.com');
  
  console.timeEnd('Without pipelining');
  console.log(`Result: ${mr1}\n`);
  
  console.log('✅ NEW WAY (1 round trip with pipelining):');
  console.time('With pipelining');
  
  // Don't await intermediate calls!
  const session2 = api.authenticate('alice@example.com', {
    type: 'password',
    data: 'secret123'
  });
  const network2 = session2.getNetworkState();
  const mr2 = await network2.computeMutualRecognition('alice@example.com', 'bob@example.com');
  
  console.timeEnd('With pipelining');
  console.log(`Result: ${mr2}\n`);
  
  console.log(`⚡ Same result, but ${3}× fewer round trips!`);
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 2: Pipelining with Memoization
// ═══════════════════════════════════════════════════════════════════

async function pipelinedWithMemoization() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 2: Pipelining + Memoization');
  console.log('════════════════════════════════════════════════\n');
  
  const api = newWebSocketRpcSession<IParticipantServer>(SERVER_URL);
  const pipelinedApi = createPipelinedClient(api);
  
  console.log('First call (server)...');
  console.time('First pipelined call');
  
  const session1 = pipelinedApi.authenticate('alice@example.com', {
    type: 'password',
    data: 'secret123'
  });
  const network1 = session1.getNetworkState();
  const mr1 = await network1.computeMutualRecognition('alice@example.com', 'bob@example.com');
  
  console.timeEnd('First pipelined call');
  console.log(`Result: ${mr1}\n`);
  
  console.log('Second call (memoized)...');
  console.time('Second pipelined call');
  
  const session2 = pipelinedApi.authenticate('alice@example.com', {
    type: 'password',
    data: 'secret123'
  });
  const network2 = session2.getNetworkState();
  const mr2 = await network2.computeMutualRecognition('alice@example.com', 'bob@example.com');
  
  console.timeEnd('Second pipelined call');
  console.log(`Result: ${mr2}\n`);
  
  console.log('✨ Second call retrieved from cache (instant)!');
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 3: Pipeline Builder Pattern
// ═══════════════════════════════════════════════════════════════════

async function pipelineBuilder() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 3: Pipeline Builder Pattern');
  console.log('════════════════════════════════════════════════\n');
  
  const api = newWebSocketRpcSession<IParticipantServer>(SERVER_URL);
  
  console.log('Building pipeline...');
  
  const result = await pipeline(api)
    .call('authenticate', 'alice@example.com', {
      type: 'password',
      data: 'secret123'
    })
    .call('getNetworkState')
    .call('computeMutualRecognition', 'alice@example.com', 'bob@example.com')
    .execute();
  
  console.log(`Result: ${result}`);
  console.log('✅ All 3 calls executed in single round trip!');
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 4: Parallel Pipelined Calls
// ═══════════════════════════════════════════════════════════════════

async function parallelPipelined() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 4: Parallel Pipelined Calls');
  console.log('════════════════════════════════════════════════\n');
  
  const api = newWebSocketRpcSession<IParticipantServer>(SERVER_URL);
  
  console.log('Starting parallel pipelined calls...');
  console.time('Parallel pipelined');
  
  // Setup authentication once
  const session = api.authenticate('alice@example.com', {
    type: 'password',
    data: 'secret123'
  });
  const network = session.getNetworkState();
  
  // Make multiple parallel pipelined calls
  const mrAliceBob = network.computeMutualRecognition('alice@example.com', 'bob@example.com');
  const mrAliceCarol = network.computeMutualRecognition('alice@example.com', 'carol@example.com');
  const mrBobCarol = network.computeMutualRecognition('bob@example.com', 'carol@example.com');
  const totalAlice = network.computeTotalMR('alice@example.com');
  
  // Await all at once
  const [mr1, mr2, mr3, total] = await Promise.all([
    mrAliceBob,
    mrAliceCarol,
    mrBobCarol,
    totalAlice
  ]);
  
  console.timeEnd('Parallel pipelined');
  
  console.log('\nResults:');
  console.log(`  MR(alice, bob): ${mr1}`);
  console.log(`  MR(alice, carol): ${mr2}`);
  console.log(`  MR(bob, carol): ${mr3}`);
  console.log(`  Total MR(alice): ${total}`);
  
  console.log('\n✨ 4 computations + authentication in just 1 round trip!');
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 5: Complex Pipeline Chain
// ═══════════════════════════════════════════════════════════════════

async function complexPipeline() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 5: Complex Pipeline Chain');
  console.log('════════════════════════════════════════════════\n');
  
  const api = newWebSocketRpcSession<IParticipantServer>(SERVER_URL);
  
  console.log('Executing complex pipeline...');
  console.time('Complex pipeline');
  
  // Authenticate
  const session = api.authenticate('alice@example.com', {
    type: 'password',
    data: 'secret123'
  });
  
  // Get budget (pipelined)
  const budget = session.getRecognitionBudget();
  
  // Check current recognition (pipelined)
  const currentRecognition = budget.getRecognitionTo('bob@example.com');
  
  // Get network state (parallel pipeline)
  const network = session.getNetworkState();
  
  // Compute MR (parallel pipeline)
  const mr = network.computeMutualRecognition('alice@example.com', 'bob@example.com');
  
  // Await final results
  const [current, mrValue] = await Promise.all([currentRecognition, mr]);
  
  console.timeEnd('Complex pipeline');
  
  console.log('\nResults:');
  console.log(`  Current recognition to Bob: ${current}`);
  console.log(`  Mutual recognition with Bob: ${mrValue}`);
  
  console.log('\n✨ 5 operations in 1 round trip!');
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 6: Performance Comparison
// ═══════════════════════════════════════════════════════════════════

async function performanceComparison() {
  console.log('\n════════════════════════════════════════════════');
  console.log('EXAMPLE 6: Performance Comparison');
  console.log('════════════════════════════════════════════════\n');
  
  const api = newWebSocketRpcSession<IParticipantServer>(SERVER_URL);
  
  // Test 1: Sequential (awaiting each call)
  console.log('Test 1: Sequential calls (await each)');
  console.time('Sequential');
  
  const s1 = await api.authenticate('alice@example.com', {
    type: 'password',
    data: 'secret123'
  });
  const n1 = await s1.getNetworkState();
  const mr1 = await n1.computeMutualRecognition('alice@example.com', 'bob@example.com');
  const mr2 = await n1.computeMutualRecognition('alice@example.com', 'carol@example.com');
  const total1 = await n1.computeTotalMR('alice@example.com');
  
  console.timeEnd('Sequential');
  console.log(`  Result: ${mr1}, ${mr2}, ${total1}\n`);
  
  // Test 2: Pipelined (single round trip)
  console.log('Test 2: Pipelined calls (no await intermediates)');
  console.time('Pipelined');
  
  const s2 = api.authenticate('alice@example.com', {
    type: 'password',
    data: 'secret123'
  });
  const n2 = s2.getNetworkState();
  const mr3 = n2.computeMutualRecognition('alice@example.com', 'bob@example.com');
  const mr4 = n2.computeMutualRecognition('alice@example.com', 'carol@example.com');
  const total2 = n2.computeTotalMR('alice@example.com');
  
  const [mr3Result, mr4Result, total2Result] = await Promise.all([mr3, mr4, total2]);
  
  console.timeEnd('Pipelined');
  console.log(`  Result: ${mr3Result}, ${mr4Result}, ${total2Result}\n`);
  
  console.log('📊 Summary:');
  console.log('  Sequential: 5 round trips');
  console.log('  Pipelined: 1 round trip');
  console.log('  Speedup: 5× fewer round trips! ⚡');
}

// ═══════════════════════════════════════════════════════════════════
// RUN ALL EXAMPLES
// ═══════════════════════════════════════════════════════════════════

async function runAllExamples() {
  try {
    await basicPipelining();
    await pipelinedWithMemoization();
    await pipelineBuilder();
    await parallelPipelined();
    await complexPipeline();
    await performanceComparison();
    
    console.log('\n════════════════════════════════════════════════');
    console.log('✅ All pipelining examples completed!');
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
  basicPipelining,
  pipelinedWithMemoization,
  pipelineBuilder,
  parallelPipelined,
  complexPipeline,
  performanceComparison
};

