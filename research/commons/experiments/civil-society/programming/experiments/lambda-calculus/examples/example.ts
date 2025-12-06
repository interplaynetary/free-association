/**
 * Lambda Calculus Implementation - Example Usage
 * 
 * This file demonstrates how to use the Recognition Calculus (λ-R) implementation
 * to model coordination scenarios.
 */

import {
  // Types
  type Entity,
  type SystemState,
  
  // System initialization and evolution
  initializeSystem,
  evolveSystem,
  evolveSystemUntilConvergence,
  
  // Filters
  attrFilter,
  timeFilter,
  andFilter,
  
  // Limits
  capLimit,
  progressiveLimit,
  topKLimit,
  
  // Collectives
  formCollective,
  scmrs,
  scrmrs,
  
  // Commons
  formCommons,
  evolveCommons,
  allocateCommons,
  
  // Allocation
  allocateCapacity,
  
  // Recognition
  mutual,
  mrs,
  mrd,
  
  // Utilities
  linearBenefitFunction,
  calculateSystemMetrics,
} from './index';

// ============================================================================
// Example 1: Simple Three-Entity Coordination
// ============================================================================

export function example1_SimpleCoordination() {
  console.log('=== Example 1: Simple Coordination ===\n');

  // Create three entities
  const entities = new Set<Entity>([
    { id: 'alice', name: 'Alice' },
    { id: 'bob', name: 'Bob' },
    { id: 'charlie', name: 'Charlie' },
  ]);

  // Initialize system
  const system = initializeSystem(entities);
  console.log('Initialized system with 3 entities');

  // Calculate initial mutual recognition (all equal initially)
  const mr_ab = mutual(system.recognitionMatrix, 'alice', 'bob');
  console.log(`Mutual recognition between Alice and Bob: ${mr_ab.toFixed(4)}`);

  return system;
}

// ============================================================================
// Example 2: Team Collective with Filters
// ============================================================================

export function example2_TeamCollective() {
  console.log('\n=== Example 2: Team Collective ===\n');

  // Create entities with metadata
  const entities = new Set<Entity>([
    { id: 'alice', name: 'Alice', metadata: { team: 'engineering', years: 5 } },
    { id: 'bob', name: 'Bob', metadata: { team: 'engineering', years: 3 } },
    { id: 'charlie', name: 'Charlie', metadata: { team: 'design', years: 4 } },
    { id: 'dana', name: 'Dana', metadata: { team: 'engineering', years: 7 } },
  ]);

  const system = initializeSystem(entities);

  // Create filter for engineering team
  const { filter: teamFilterDef, fn: teamFilter } = attrFilter(
    'engineering-team',
    (entity) => entity.metadata?.team === 'engineering'
  );

  // Create fairness limit
  const { limit: fairLimitDef, fn: fairLimit } = capLimit('fair', 0.35);

  // Form collective
  const collective = formCollective(
    'eng-team',
    entities,
    [teamFilter],
    [fairLimit],
    [teamFilterDef],
    [fairLimitDef],
    'SCMRS'
  );

  console.log(`Collective formed with ${collective.members.size} members`);
  console.log('Members:', Array.from(collective.members));

  // Calculate SCMRS
  const distribution = scmrs(system.recognitionMatrix, collective);
  console.log('\nSCMRS distribution:');
  for (const [id, weight] of Object.entries(distribution.weights)) {
    console.log(`  ${id}: ${((weight / distribution.total) * 100).toFixed(2)}%`);
  }

  return { system, collective };
}

// ============================================================================
// Example 3: Commons Formation and Evolution
// ============================================================================

export function example3_CommonsEvolution() {
  console.log('\n=== Example 3: Commons Evolution ===\n');

  // Create entities
  const entities = new Set<Entity>([
    { id: 'alice', name: 'Alice', metadata: { contributor: true } },
    { id: 'bob', name: 'Bob', metadata: { contributor: true } },
    { id: 'charlie', name: 'Charlie', metadata: { contributor: false } },
    { id: 'dana', name: 'Dana', metadata: { contributor: true } },
  ]);

  const system = initializeSystem(entities);

  // Form commons for contributors with MRD threshold
  const commons = formCommons(
    'open-source-commons',
    (entity) => entity.metadata?.contributor === true,
    0.5, // MRD threshold
    entities,
    system.recognitionMatrix,
    [],
    [],
    [],
    [],
    1000 // Initial resources
  );

  console.log(`Commons formed with ${commons.members.size} members`);
  console.log('Members:', Array.from(commons.members));
  console.log(`Resources: ${commons.resources}`);

  // Allocate commons resources
  const allocation = allocateCommons(commons, system.recognitionMatrix, []);
  console.log('\nResource allocation:');
  for (const [id, amount] of Object.entries(allocation.weights)) {
    console.log(`  ${id}: ${amount.toFixed(2)}`);
  }

  // Evolve commons
  const evolved = evolveCommons(commons, system.recognitionMatrix, entities);
  console.log(`\nAfter evolution: ${evolved.members.size} members`);

  return { system, commons };
}

// ============================================================================
// Example 4: Capacity Allocation
// ============================================================================

export function example4_CapacityAllocation() {
  console.log('\n=== Example 4: Capacity Allocation ===\n');

  // Create entities
  const entities = new Set<Entity>([
    { id: 'alice', name: 'Alice' },
    { id: 'bob', name: 'Bob' },
    { id: 'charlie', name: 'Charlie' },
    { id: 'dana', name: 'Dana' },
  ]);

  const system = initializeSystem(entities);

  // Define providers (entities with capacity)
  const providers = [
    { entity: { id: 'alice', name: 'Alice' }, capacity: 100, limits: [] },
    { entity: { id: 'bob', name: 'Bob' }, capacity: 150, limits: [] },
  ];

  // Define recipients (entities with needs)
  const recipients = [
    { entity: { id: 'charlie', name: 'Charlie' }, need: 80, filters: [] },
    { entity: { id: 'dana', name: 'Dana' }, need: 120, filters: [] },
  ];

  // Allocate capacity using MRS
  const allocation = allocateCapacity(
    providers,
    recipients,
    system.recognitionMatrix,
    new Set(['alice', 'bob', 'charlie', 'dana']),
    'MRS',
    new Map(),
    new Map()
  );

  console.log('Allocation results:');
  for (const [providerId, recipientAllocs] of Object.entries(allocation.allocations)) {
    console.log(`\n  From ${providerId}:`);
    for (const [recipientId, amount] of Object.entries(recipientAllocs)) {
      console.log(`    → ${recipientId}: ${amount.toFixed(2)}`);
    }
  }

  return { system, allocation };
}

// ============================================================================
// Example 5: System Evolution with Learning
// ============================================================================

export function example5_SystemEvolution() {
  console.log('\n=== Example 5: System Evolution ===\n');

  // Create entities
  const entities = new Set<Entity>([
    { id: 'alice', name: 'Alice' },
    { id: 'bob', name: 'Bob' },
    { id: 'charlie', name: 'Charlie' },
  ]);

  let system = initializeSystem(entities, {
    learningRate: 0.1,
    convergenceThreshold: 0.001,
  });

  console.log('Initial system state:');
  console.log(calculateSystemMetrics(system));

  // Define evolution context
  const providers = [
    { entity: { id: 'alice', name: 'Alice' }, capacity: 100, limits: [] },
  ];

  const recipients = [
    { entity: { id: 'bob', name: 'Bob' }, need: 60, filters: [] },
    { entity: { id: 'charlie', name: 'Charlie' }, need: 40, filters: [] },
  ];

  const context = {
    providers,
    recipients,
    shareType: 'MRS' as const,
    recipientFilterFns: new Map(),
    providerLimitFns: new Map(),
    commonsLimitFns: new Map(),
    benefitFunction: linearBenefitFunction(1.0),
  };

  // Evolve for 10 steps
  console.log('\nEvolving system for 10 steps...');
  for (let i = 0; i < 10; i++) {
    system = evolveSystem(system, context);
    
    if (i % 3 === 0) {
      const mr_ab = mutual(system.recognitionMatrix, 'alice', 'bob');
      const mr_ac = mutual(system.recognitionMatrix, 'alice', 'charlie');
      console.log(`Step ${i}: MR(alice,bob)=${mr_ab.toFixed(4)}, MR(alice,charlie)=${mr_ac.toFixed(4)}`);
    }
  }

  console.log('\nFinal system state:');
  console.log(calculateSystemMetrics(system));

  return system;
}

// ============================================================================
// Example 6: Convergence Testing
// ============================================================================

export function example6_Convergence() {
  console.log('\n=== Example 6: Convergence Testing ===\n');

  // Create entities
  const entities = new Set<Entity>([
    { id: 'alice', name: 'Alice' },
    { id: 'bob', name: 'Bob' },
    { id: 'charlie', name: 'Charlie' },
    { id: 'dana', name: 'Dana' },
  ]);

  const system = initializeSystem(entities);

  // Define evolution context
  const providers = [
    { entity: { id: 'alice', name: 'Alice' }, capacity: 100, limits: [] },
    { entity: { id: 'bob', name: 'Bob' }, capacity: 100, limits: [] },
  ];

  const recipients = [
    { entity: { id: 'charlie', name: 'Charlie' }, need: 100, filters: [] },
    { entity: { id: 'dana', name: 'Dana' }, need: 100, filters: [] },
  ];

  const context = {
    providers,
    recipients,
    shareType: 'MRS' as const,
    recipientFilterFns: new Map(),
    providerLimitFns: new Map(),
    commonsLimitFns: new Map(),
    benefitFunction: linearBenefitFunction(0.5),
  };

  // Evolve until convergence
  console.log('Evolving until convergence...');
  const { state, iterations, converged } = evolveSystemUntilConvergence(
    system,
    context,
    100 // max iterations
  );

  console.log(`\nConverged: ${converged}`);
  console.log(`Iterations: ${iterations}`);
  console.log('Final metrics:', calculateSystemMetrics(state));

  return { state, iterations, converged };
}

// ============================================================================
// Run all examples
// ============================================================================

export function runAllExamples() {
  example1_SimpleCoordination();
  example2_TeamCollective();
  example3_CommonsEvolution();
  example4_CapacityAllocation();
  example5_SystemEvolution();
  example6_Convergence();
  
  console.log('\n=== All examples completed ===\n');
}

// Uncomment to run examples:
// runAllExamples();

