/**
 * Lambda Calculus Implementation: System State and Evolution
 * 
 * This module implements:
 * - System state initialization
 * - System evolution over time
 * - Convergence detection
 * - Complete system orchestration
 */

import type {
  Entity,
  Real,
  SystemState,
  RecognitionMatrix,
  Config,
  Provider,
  Recipient,
  ShareType,
  Allocation,
} from './types';
import { entitiesToIds } from './primitives';
import { uniformRecognitionMatrix, updateRecognition, normalizeRecognitionMatrix } from './recognition';
import { evolveCommons, allocateCommons } from './commons';
import { evolveCollective } from './collective';
import { allocateCapacity, DEFAULT_ALLOCATION_CONFIG } from './allocation';
import type { FilterFunction } from './filters';
import type { LimitFunction } from './limits';

// ============================================================================
// System Initialization
// ============================================================================

/**
 * Initialize a new system with uniform recognition
 */
export function initializeSystem(
  entities: Set<Entity>,
  config?: Partial<Config>
): SystemState {
  const entityIds = entitiesToIds(entities);
  const recognitionMatrix = uniformRecognitionMatrix(entityIds);

  return {
    universe: entities,
    recognitionMatrix,
    collectives: [],
    commons: [],
    hyperCollectives: [],
    allocations: { allocations: {} },
    filters: [],
    limits: [],
    timestamp: Date.now(),
    metadata: {
      config: {
        learningRate: config?.learningRate ?? 0.1,
        convergenceThreshold: config?.convergenceThreshold ?? 0.001,
        maxIterations: config?.maxIterations ?? 100,
        mrdLeavingFactor: config?.mrdLeavingFactor ?? 0.5,
      },
    },
  };
}

/**
 * Initialize system with custom recognition matrix
 */
export function initializeSystemWithRecognition(
  entities: Set<Entity>,
  recognitionMatrix: RecognitionMatrix,
  config?: Partial<Config>
): SystemState {
  return {
    universe: entities,
    recognitionMatrix,
    collectives: [],
    commons: [],
    hyperCollectives: [],
    allocations: { allocations: {} },
    filters: [],
    limits: [],
    timestamp: Date.now(),
    metadata: {
      config: {
        learningRate: config?.learningRate ?? 0.1,
        convergenceThreshold: config?.convergenceThreshold ?? 0.001,
        maxIterations: config?.maxIterations ?? 100,
        mrdLeavingFactor: config?.mrdLeavingFactor ?? 0.5,
      },
    },
  };
}

// ============================================================================
// System Evolution
// ============================================================================

export interface EvolutionContext {
  providers: Provider[];
  recipients: Recipient[];
  shareType: ShareType;
  recipientFilterFns: Map<string, FilterFunction[]>;
  providerLimitFns: Map<string, LimitFunction[]>;
  commonsLimitFns: Map<string, LimitFunction[]>;
  benefitFunction?: (fromId: string, toId: string, amount: Real) => Real;
}

/**
 * Evolve the system by one time step
 */
export function evolveSystem(
  state: SystemState,
  context: EvolutionContext
): SystemState {
  const config = state.metadata?.config as Config ?? {
    learningRate: 0.1,
    convergenceThreshold: 0.001,
    maxIterations: 100,
    mrdLeavingFactor: 0.5,
  };

  const entityIds = entitiesToIds(state.universe);

  // Step 1: Update collectives based on MRD
  const updatedCollectives = state.collectives.map((collective) =>
    evolveCollective(
      state.recognitionMatrix,
      collective,
      0.5, // join threshold
      config.mrdLeavingFactor, // leave threshold
      entityIds
    )
  );

  // Step 2: Update commons
  const updatedCommons = state.commons.map((commons) =>
    evolveCommons(
      commons,
      state.recognitionMatrix,
      state.universe,
      config.mrdLeavingFactor
    )
  );

  // Step 3: Allocate capacity
  const allocations = allocateCapacity(
    context.providers,
    context.recipients,
    state.recognitionMatrix,
    entityIds,
    context.shareType,
    context.recipientFilterFns,
    context.providerLimitFns,
    {
      maxIterations: config.maxIterations,
      convergenceThreshold: config.convergenceThreshold,
    }
  );

  // Step 4: Update recognition based on benefits received
  let updatedRecognition = state.recognitionMatrix;

  if (context.benefitFunction) {
    // Apply learning updates
    for (const [providerId, providerAlloc] of Object.entries(allocations.allocations)) {
      for (const [recipientId, amount] of Object.entries(providerAlloc)) {
        const benefit = context.benefitFunction(providerId, recipientId, amount);
        updatedRecognition = updateRecognition(
          updatedRecognition,
          recipientId,
          providerId,
          benefit,
          config.learningRate
        );
      }
    }

    // Normalize recognition matrix
    updatedRecognition = normalizeRecognitionMatrix(updatedRecognition, entityIds);
  }

  return {
    ...state,
    recognitionMatrix: updatedRecognition,
    collectives: updatedCollectives,
    commons: updatedCommons,
    allocations,
    timestamp: Date.now(),
  };
}

/**
 * Evolve system for multiple iterations
 */
export function evolveSystemMultiple(
  state: SystemState,
  context: EvolutionContext,
  iterations: number
): SystemState {
  let currentState = state;
  for (let i = 0; i < iterations; i++) {
    currentState = evolveSystem(currentState, context);
  }
  return currentState;
}

/**
 * Evolve system until convergence
 */
export function evolveSystemUntilConvergence(
  state: SystemState,
  context: EvolutionContext,
  maxIterations: number = 1000
): { state: SystemState; iterations: number; converged: boolean } {
  let currentState = state;
  let previousState = state;

  for (let i = 0; i < maxIterations; i++) {
    currentState = evolveSystem(currentState, context);

    // Check convergence
    if (hasConverged(previousState, currentState)) {
      return { state: currentState, iterations: i + 1, converged: true };
    }

    previousState = currentState;
  }

  return { state: currentState, iterations: maxIterations, converged: false };
}

// ============================================================================
// Convergence Detection
// ============================================================================

/**
 * Check if system has converged
 */
export function hasConverged(
  state1: SystemState,
  state2: SystemState,
  threshold: Real = 0.001
): boolean {
  const recognitionChange = calculateRecognitionChange(
    state1.recognitionMatrix,
    state2.recognitionMatrix,
    entitiesToIds(state1.universe)
  );

  return recognitionChange < threshold;
}

/**
 * Calculate total change in recognition matrix
 */
function calculateRecognitionChange(
  matrix1: RecognitionMatrix,
  matrix2: RecognitionMatrix,
  entities: Set<string>
): Real {
  let totalChange = 0;
  let count = 0;

  for (const fromId of entities) {
    for (const toId of entities) {
      const r1 = matrix1.matrix[fromId]?.[toId] ?? 0;
      const r2 = matrix2.matrix[fromId]?.[toId] ?? 0;
      totalChange += Math.abs(r2 - r1);
      count++;
    }
  }

  return count > 0 ? totalChange / count : 0;
}

// ============================================================================
// System Queries
// ============================================================================

/**
 * Get entity by ID
 */
export function getEntity(state: SystemState, entityId: string): Entity | undefined {
  for (const entity of state.universe) {
    if (entity.id === entityId) return entity;
  }
  return undefined;
}

/**
 * Get all entities
 */
export function getAllEntities(state: SystemState): Entity[] {
  return Array.from(state.universe);
}

/**
 * Get collective by ID
 */
export function getCollective(state: SystemState, collectiveId: string) {
  return state.collectives.find((c) => c.id === collectiveId);
}

/**
 * Get commons by ID
 */
export function getCommons(state: SystemState, commonsId: string) {
  return state.commons.find((c) => c.id === commonsId);
}

/**
 * Get all collectives an entity belongs to
 */
export function getEntityCollectives(state: SystemState, entityId: string) {
  return state.collectives.filter((c) => c.members.has(entityId));
}

/**
 * Get all commons an entity belongs to
 */
export function getEntityCommonsFromSystem(state: SystemState, entityId: string) {
  return state.commons.filter((c) => c.members.has(entityId));
}

// ============================================================================
// System Modifications
// ============================================================================

/**
 * Add entity to system
 */
export function addEntity(state: SystemState, entity: Entity): SystemState {
  const newUniverse = new Set(state.universe);
  newUniverse.add(entity);

  // Initialize recognition for new entity (uniform)
  const entityIds = entitiesToIds(newUniverse);
  const updatedRecognition = normalizeRecognitionMatrix(
    state.recognitionMatrix,
    entityIds
  );

  return {
    ...state,
    universe: newUniverse,
    recognitionMatrix: updatedRecognition,
  };
}

/**
 * Remove entity from system
 */
export function removeEntity(state: SystemState, entityId: string): SystemState {
  const newUniverse = new Set<Entity>();
  for (const entity of state.universe) {
    if (entity.id !== entityId) {
      newUniverse.add(entity);
    }
  }

  // Remove from collectives
  const updatedCollectives = state.collectives.map((collective) => {
    const newMembers = new Set<string>();
    for (const memberId of collective.members) {
      if (memberId !== entityId) {
        newMembers.add(memberId);
      }
    }
    return { ...collective, members: newMembers };
  });

  // Remove from commons
  const updatedCommons = state.commons.map((commons) => {
    const newMembers = new Set<string>();
    for (const memberId of commons.members) {
      if (memberId !== entityId) {
        newMembers.add(memberId);
      }
    }
    return { ...commons, members: newMembers };
  });

  // Remove from recognition matrix
  const newMatrix: Record<string, Record<string, Real>> = {};
  for (const [fromId, row] of Object.entries(state.recognitionMatrix.matrix)) {
    if (fromId !== entityId) {
      newMatrix[fromId] = {};
      for (const [toId, value] of Object.entries(row)) {
        if (toId !== entityId) {
          newMatrix[fromId][toId] = value;
        }
      }
    }
  }

  return {
    ...state,
    universe: newUniverse,
    recognitionMatrix: { matrix: newMatrix },
    collectives: updatedCollectives,
    commons: updatedCommons,
  };
}

/**
 * Update entity in system
 */
export function updateEntity(state: SystemState, entity: Entity): SystemState {
  const newUniverse = new Set<Entity>();
  for (const e of state.universe) {
    if (e.id === entity.id) {
      newUniverse.add(entity);
    } else {
      newUniverse.add(e);
    }
  }

  return {
    ...state,
    universe: newUniverse,
  };
}

// ============================================================================
// System Metrics
// ============================================================================

/**
 * Calculate system-wide metrics
 */
export function calculateSystemMetrics(state: SystemState) {
  const entityIds = entitiesToIds(state.universe);

  return {
    entityCount: state.universe.size,
    collectiveCount: state.collectives.length,
    commonsCount: state.commons.length,
    totalCollectiveMembers: state.collectives.reduce(
      (sum, c) => sum + c.members.size,
      0
    ),
    totalCommonsMembers: state.commons.reduce(
      (sum, c) => sum + c.members.size,
      0
    ),
    totalCommonsResources: state.commons.reduce(
      (sum, c) => sum + c.resources,
      0
    ),
    averageCollectiveSize: state.collectives.length > 0
      ? state.collectives.reduce((sum, c) => sum + c.members.size, 0) /
        state.collectives.length
      : 0,
    averageCommonsSize: state.commons.length > 0
      ? state.commons.reduce((sum, c) => sum + c.members.size, 0) /
        state.commons.length
      : 0,
  };
}

/**
 * Calculate recognition sparsity (proportion of zero entries)
 */
export function calculateRecognitionSparsity(state: SystemState): Real {
  const entityIds = entitiesToIds(state.universe);
  const totalEntries = entityIds.size * entityIds.size;
  
  if (totalEntries === 0) return 0;

  let zeroCount = 0;
  for (const fromId of entityIds) {
    for (const toId of entityIds) {
      const value = state.recognitionMatrix.matrix[fromId]?.[toId] ?? 0;
      if (value === 0) {
        zeroCount++;
      }
    }
  }

  return zeroCount / totalEntries;
}

// ============================================================================
// System Serialization
// ============================================================================

/**
 * Export system state to JSON-serializable format
 */
export function exportSystemState(state: SystemState) {
  return {
    universe: Array.from(state.universe),
    recognitionMatrix: state.recognitionMatrix,
    collectives: state.collectives.map((c) => ({
      ...c,
      members: Array.from(c.members),
    })),
    commons: state.commons.map((c) => ({
      ...c,
      members: Array.from(c.members),
      condition: undefined, // Cannot serialize functions
    })),
    hyperCollectives: state.hyperCollectives,
    allocations: state.allocations,
    filters: state.filters,
    limits: state.limits,
    timestamp: state.timestamp,
    metadata: state.metadata,
  };
}

/**
 * Import system state from JSON format
 * Note: Conditions and functions need to be re-attached after import
 */
export function importSystemState(
  data: ReturnType<typeof exportSystemState>,
  conditions: Map<string, (entity: Entity) => boolean>
): SystemState {
  return {
    universe: new Set(data.universe),
    recognitionMatrix: data.recognitionMatrix,
    collectives: data.collectives.map((c) => ({
      ...c,
      members: new Set(c.members),
    })),
    commons: data.commons.map((c) => ({
      ...c,
      members: new Set(c.members),
      condition: conditions.get(c.id) || (() => true),
    })),
    hyperCollectives: data.hyperCollectives,
    allocations: data.allocations,
    filters: data.filters,
    limits: data.limits,
    timestamp: data.timestamp,
    metadata: data.metadata,
  };
}

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Create a simple benefit function based on allocation amount
 */
export function linearBenefitFunction(scale: Real = 1): (fromId: string, toId: string, amount: Real) => Real {
  return (_fromId, _toId, amount) => amount * scale;
}

/**
 * Create a benefit function with diminishing returns
 */
export function diminishingBenefitFunction(alpha: Real = 0.5): (fromId: string, toId: string, amount: Real) => Real {
  return (_fromId, _toId, amount) => Math.pow(amount, alpha);
}

/**
 * Create a threshold benefit function (only benefits above threshold)
 */
export function thresholdBenefitFunction(
  threshold: Real,
  scale: Real = 1
): (fromId: string, toId: string, amount: Real) => Real {
  return (_fromId, _toId, amount) => (amount >= threshold ? (amount - threshold) * scale : 0);
}

