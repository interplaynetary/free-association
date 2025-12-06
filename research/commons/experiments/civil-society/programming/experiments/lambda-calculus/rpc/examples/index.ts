/**
 * RPC Examples Index
 * 
 * Comprehensive examples demonstrating the Lambda Calculus RPC system.
 */

export * from './peer-to-peer';
export * from './offline-sync';
export * from './collective-coordination';

// Run all examples
export async function runAllExamples() {
  const { runP2PExamples } = await import('./peer-to-peer');
  const { runOfflineExamples } = await import('./offline-sync');
  const { runCollectiveExamples } = await import('./collective-coordination');

  console.log('╔════════════════════════════════════════════════════════════╗');
  console.log('║  Lambda Calculus RPC Examples                              ║');
  console.log('║  Symmetric Peer-to-Peer Protocol                           ║');
  console.log('╚════════════════════════════════════════════════════════════╝\n');

  await runP2PExamples();
  await runOfflineExamples();
  await runCollectiveExamples();

  console.log('\n╔════════════════════════════════════════════════════════════╗');
  console.log('║  ✓ All examples completed successfully!                    ║');
  console.log('╚════════════════════════════════════════════════════════════╝');
}

