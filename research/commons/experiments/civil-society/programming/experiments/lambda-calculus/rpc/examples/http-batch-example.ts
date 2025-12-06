/**
 * HTTP Batch Mode Example
 * 
 * Demonstrates Cap'n Web inspired HTTP batch mode:
 * - Multiple calls in single HTTP request
 * - Lightweight (no WebSocket needed)
 * - Perfect for simple operations
 */

import { createHttpBatchSession } from '../simple-api';

export async function runHttpBatchExample() {
  console.log('=== HTTP Batch Mode Example ===\n');

  // ============================================================================
  // Create Batch Session (one line!)
  // ============================================================================

  console.log('Creating HTTP batch session...');

  const batch = createHttpBatchSession('https://api.example.com/rpc');

  // ============================================================================
  // Queue Multiple Calls
  // ============================================================================

  console.log('Queuing multiple calls...');

  const p1 = batch.getMRS(['alice', 'bob', 'charlie']);
  const p2 = batch.getMRD(['alice', 'bob']);
  const p3 = batch.getMutualRecognition('charlie');

  console.log('✓ Calls queued (not sent yet)');

  // ============================================================================
  // Execute Batch (single HTTP request!)
  // ============================================================================

  console.log('Executing batch...');

  const [mrs, mrd, mr] = await Promise.all([p1, p2, p3]);

  console.log('✓ All results received in single HTTP round trip!');
  console.log('MRS:', mrs);
  console.log('MRD:', mrd);
  console.log('MR:', mr);

  // ============================================================================
  // New Batch (batch is done after first await)
  // ============================================================================

  console.log('\nCreating new batch for more calls...');

  const batch2 = createHttpBatchSession('https://api.example.com/rpc');

  const result = await batch2.allocateRecognition('bob', 0.5);

  console.log('✓ New batch executed');
  console.log('Result:', result);

  console.log('\n=== HTTP Batch Example Complete ===');
}

/**
 * Comparison: Before vs After
 */
export function showComparison() {
  console.log(`
=== BEFORE (Complex Setup) ===

const storage = new BrowserStorage('alice');
await storage.initialize();
const cache = new RecognitionCache();
const session = new EntitySession({ entityId: 'alice', storage, cache });
const capMgr = new CapabilityManager();
capMgr.exportMain(session);
const transport = new WebSocketTransport('wss://...');
// ... 20 more lines ...

const result = await session.getMRS(['alice', 'bob']);

=== AFTER (Elegant!) ===

let api = newWebSocketSession('alice', 'wss://relay.example.com');
let result = await api.getMRS(['alice', 'bob']);

=== EVEN MORE ELEGANT (HTTP Batch) ===

let batch = createHttpBatchSession('https://api.example.com');
let p1 = batch.getMRS(['alice']);
let p2 = batch.getMRD(['bob']);
let [mrs, mrd] = await Promise.all([p1, p2]);
// → Single HTTP request!
  `);
}

