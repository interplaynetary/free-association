/**
 * Collective Coordination Example
 * 
 * Demonstrates multi-entity collective formation and coordination using:
 * - MRS (Mutual Recognition Share)
 * - MRD (Mutual Recognition Density)
 * - Collective formation thresholds
 * - Sparse operations for efficiency
 */

import { PeerConnection, createP2PConnection } from '../peer-connection';

/**
 * Example: Form a collective based on MRD threshold
 */
export async function formCollectiveExample() {
  console.log('=== Form Collective Example ===\n');

  // Create 5 entities
  const entityIds = ['alice', 'bob', 'charlie', 'diana', 'eve'];
  const connections = new Map<string, PeerConnection>();

  // Connect all entities
  console.log('Connecting entities...\n');
  for (const id of entityIds) {
    const conn = await createP2PConnection(id);
    connections.set(id, conn);
    console.log(`  ✓ ${id} connected`);
  }

  console.log('\nAllocating recognition...\n');

  // Create a tight-knit group: Alice, Bob, Charlie
  // They all recognize each other highly
  await connections.get('alice')!.getLocalSession().allocateRecognition('bob', 0.4);
  await connections.get('alice')!.getLocalSession().allocateRecognition('charlie', 0.4);
  
  await connections.get('bob')!.getLocalSession().allocateRecognition('alice', 0.5);
  await connections.get('bob')!.getLocalSession().allocateRecognition('charlie', 0.4);
  
  await connections.get('charlie')!.getLocalSession().allocateRecognition('alice', 0.45);
  await connections.get('charlie')!.getLocalSession().allocateRecognition('bob', 0.45);

  console.log('  Tight-knit group formed: Alice ↔ Bob ↔ Charlie');

  // Diana and Eve have weaker connections
  await connections.get('diana')!.getLocalSession().allocateRecognition('alice', 0.2);
  await connections.get('diana')!.getLocalSession().allocateRecognition('eve', 0.3);
  
  await connections.get('eve')!.getLocalSession().allocateRecognition('diana', 0.3);

  console.log('  Weaker connections: Diana → Alice, Diana ↔ Eve\n');

  // Calculate MRD for each entity with potential collective [alice, bob, charlie]
  const collectiveMembers = ['alice', 'bob', 'charlie'];
  console.log('Calculating MRD for potential collective [alice, bob, charlie]...\n');

  const mrdResults = new Map<string, number>();
  for (const id of entityIds) {
    const conn = connections.get(id)!;
    const mrd = await conn.getLocalSession().getMRD(collectiveMembers);
    mrdResults.set(id, mrd);
    console.log(`  ${id}: MRD = ${mrd.toFixed(3)}`);
  }

  // Determine collective membership based on MRD threshold
  const MRD_THRESHOLD = 1.5;
  console.log(`\nMRD Threshold: ${MRD_THRESHOLD}`);
  console.log('Collective members (MRD >= threshold):\n');

  const collective: string[] = [];
  for (const [id, mrd] of mrdResults) {
    if (mrd >= MRD_THRESHOLD) {
      collective.push(id);
      console.log(`  ✓ ${id} (MRD: ${mrd.toFixed(3)})`);
    } else {
      console.log(`  ✗ ${id} (MRD: ${mrd.toFixed(3)} - below threshold)`);
    }
  }

  console.log(`\nFinal collective: [${collective.join(', ')}]`);

  // Cleanup
  for (const conn of connections.values()) {
    await conn.disconnect();
  }
}

/**
 * Example: Collective resource allocation
 */
export async function collectiveResourceAllocationExample() {
  console.log('\n=== Collective Resource Allocation Example ===\n');

  // Form a collective of 4 entities
  const collectiveIds = ['alice', 'bob', 'charlie', 'diana'];
  const connections = new Map<string, PeerConnection>();

  console.log('Forming collective...\n');
  for (const id of collectiveIds) {
    const conn = await createP2PConnection(id);
    connections.set(id, conn);
  }

  // Allocate mutual recognition (symmetric for simplicity)
  await connections.get('alice')!.getLocalSession().allocateRecognition('bob', 0.3);
  await connections.get('alice')!.getLocalSession().allocateRecognition('charlie', 0.3);
  await connections.get('alice')!.getLocalSession().allocateRecognition('diana', 0.3);

  await connections.get('bob')!.getLocalSession().allocateRecognition('alice', 0.3);
  await connections.get('bob')!.getLocalSession().allocateRecognition('charlie', 0.3);
  await connections.get('bob')!.getLocalSession().allocateRecognition('diana', 0.3);

  await connections.get('charlie')!.getLocalSession().allocateRecognition('alice', 0.25);
  await connections.get('charlie')!.getLocalSession().allocateRecognition('bob', 0.25);
  await connections.get('charlie')!.getLocalSession().allocateRecognition('diana', 0.25);

  await connections.get('diana')!.getLocalSession().allocateRecognition('alice', 0.35);
  await connections.get('diana')!.getLocalSession().allocateRecognition('bob', 0.35);
  await connections.get('diana')!.getLocalSession().allocateRecognition('charlie', 0.35);

  console.log('Recognition allocated (symmetric network)\n');

  // Calculate MRS (share of mutual recognition) for each entity
  console.log('Calculating MRS (Mutual Recognition Share)...\n');

  const mrsResults = new Map<string, any>();
  for (const id of collectiveIds) {
    const conn = connections.get(id)!;
    const mrs = await conn.getLocalSession().getMRS(collectiveIds);
    mrsResults.set(id, mrs);
    
    console.log(`${id} MRS:`);
    for (const [targetId, share] of Object.entries(mrs.distribution || {})) {
      console.log(`  → ${targetId}: ${(share as number * 100).toFixed(1)}%`);
    }
    console.log();
  }

  // Allocate hypothetical resource based on MRS
  const RESOURCE_AMOUNT = 100; // 100 units to distribute
  console.log(`Distributing ${RESOURCE_AMOUNT} units based on MRS...\n`);

  // Use Alice's MRS as distribution basis
  const aliceMRS = mrsResults.get('alice')!;
  for (const [targetId, share] of Object.entries(aliceMRS.distribution || {})) {
    const allocation = (share as number) * RESOURCE_AMOUNT;
    console.log(`  ${targetId}: ${allocation.toFixed(1)} units (${(share as number * 100).toFixed(1)}%)`);
  }

  // Cleanup
  for (const conn of connections.values()) {
    await conn.disconnect();
  }
}

/**
 * Example: Dynamic collective membership
 */
export async function dynamicCollectiveExample() {
  console.log('\n=== Dynamic Collective Example ===\n');

  // Start with 3 entities
  const initialIds = ['alice', 'bob', 'charlie'];
  const connections = new Map<string, PeerConnection>();

  console.log('Initial collective: [alice, bob, charlie]\n');
  for (const id of initialIds) {
    const conn = await createP2PConnection(id);
    connections.set(id, conn);
  }

  // Initial allocations
  await connections.get('alice')!.getLocalSession().allocateRecognition('bob', 0.5);
  await connections.get('alice')!.getLocalSession().allocateRecognition('charlie', 0.5);
  await connections.get('bob')!.getLocalSession().allocateRecognition('alice', 0.5);
  await connections.get('bob')!.getLocalSession().allocateRecognition('charlie', 0.5);
  await connections.get('charlie')!.getLocalSession().allocateRecognition('alice', 0.5);
  await connections.get('charlie')!.getLocalSession().allocateRecognition('bob', 0.5);

  console.log('Initial recognition established\n');

  // Calculate initial MRS
  for (const id of initialIds) {
    const mrs = await connections.get(id)!.getLocalSession().getMRS(initialIds);
    console.log(`${id} MRS:`, mrs);
  }

  // Diana joins
  console.log('\n→ Diana joins the collective\n');
  const dianaConn = await createP2PConnection('diana');
  connections.set('diana', dianaConn);

  // Diana allocates to existing members
  await dianaConn.getLocalSession().allocateRecognition('alice', 0.4);
  await dianaConn.getLocalSession().allocateRecognition('bob', 0.3);
  await dianaConn.getLocalSession().allocateRecognition('charlie', 0.3);
  console.log('Diana allocated recognition to existing members');

  // Existing members allocate to Diana
  await connections.get('alice')!.getLocalSession().allocateRecognition('diana', 0.3);
  await connections.get('bob')!.getLocalSession().allocateRecognition('diana', 0.3);
  await connections.get('charlie')!.getLocalSession().allocateRecognition('diana', 0.3);
  console.log('Existing members recognized Diana\n');

  // Recalculate MRS with Diana
  const updatedIds = [...initialIds, 'diana'];
  console.log('Updated MRS with Diana:');
  for (const id of updatedIds) {
    const mrs = await connections.get(id)!.getLocalSession().getMRS(updatedIds);
    const totalShare = Object.values(mrs.distribution || {}).reduce((sum: number, val) => sum + (val as number), 0);
    console.log(`  ${id}: ${Object.keys(mrs.distribution || {}).length} connections, ${(totalShare * 100).toFixed(1)}% total`);
  }

  // Check MRD to see if Diana integrates well
  console.log('\nChecking Diana MRD (integration metric)...');
  const dianaMRD = await dianaConn.getLocalSession().getMRD(updatedIds);
  console.log(`Diana MRD: ${dianaMRD.toFixed(3)}`);

  if (dianaMRD >= 1.0) {
    console.log('✓ Diana is well-integrated (MRD >= 1.0)');
  } else {
    console.log('⚠ Diana needs more recognition to integrate (MRD < 1.0)');
  }

  // Cleanup
  for (const conn of connections.values()) {
    await conn.disconnect();
  }
}

/**
 * Example: Sparse collective (large network)
 */
export async function sparseCollectiveExample() {
  console.log('\n=== Sparse Collective Example (Large Network) ===\n');

  // Simulate a large network
  const ENTITY_COUNT = 50;
  const AVG_CONNECTIONS = 5;

  console.log(`Simulating network with ${ENTITY_COUNT} entities...`);
  console.log(`Average connections per entity: ${AVG_CONNECTIONS}\n`);

  const connections = new Map<string, PeerConnection>();

  // Create entities (just a few for demo)
  const sampledIds = ['alice', 'bob', 'charlie', 'diana', 'eve'];
  for (const id of sampledIds) {
    const conn = await createP2PConnection(id);
    connections.set(id, conn);
  }

  // Random sparse allocations
  console.log('Creating sparse recognition network...\n');
  
  // Alice connects to 5 random entities
  await connections.get('alice')!.getLocalSession().allocateRecognition('bob', 0.2);
  await connections.get('alice')!.getLocalSession().allocateRecognition('charlie', 0.2);
  await connections.get('alice')!.getLocalSession().allocateRecognition('diana', 0.2);
  
  // Bob connects to different set
  await connections.get('bob')!.getLocalSession().allocateRecognition('alice', 0.3);
  await connections.get('bob')!.getLocalSession().allocateRecognition('eve', 0.3);

  console.log('Sparse network created');

  // Calculate MRD for a potential collective [alice, bob, charlie, diana]
  const potentialCollective = ['alice', 'bob', 'charlie', 'diana'];
  console.log(`\nEvaluating potential collective: [${potentialCollective.join(', ')}]\n`);

  for (const id of sampledIds) {
    const conn = connections.get(id)!;
    const mrd = await conn.getLocalSession().getMRD(potentialCollective);
    console.log(`${id} MRD with collective: ${mrd.toFixed(3)}`);
  }

  // Performance note
  console.log('\n📊 Performance Note:');
  console.log('  - Sparse operations only iterate non-zero edges');
  console.log('  - For 10k entities with avg 50 connections:');
  console.log('    * Dense: 100M operations');
  console.log('    * Sparse: ~500k operations (200× faster!)');

  // Cleanup
  for (const conn of connections.values()) {
    await conn.disconnect();
  }
}

/**
 * Run all collective examples
 */
export async function runCollectiveExamples() {
  try {
    await formCollectiveExample();
    await collectiveResourceAllocationExample();
    await dynamicCollectiveExample();
    await sparseCollectiveExample();
    
    console.log('\n✓ All collective examples completed successfully!');
  } catch (error) {
    console.error('Error running collective examples:', error);
  }
}

// Run if executed directly
if (typeof require !== 'undefined' && require.main === module) {
  runCollectiveExamples();
}

