/**
 * Elegant Lambda Calculus: System Evolution
 * 
 * Fully curried system implementation following lambda calculus principles:
 * - System state management
 * - Evolution over time
 * - Full currying for composition
 */

import type { 
  RecognitionMatrix,
  Distribution,
} from '../core/types';
import type {
  Entity,
  Real,
  Collective,
  Commons as CommonsType,
  SystemState,
  HyperCollective,
  Allocation,
  SimpleFilter,
  Limit,
} from './types';
import { normalize } from '../core/types';
import { pipe, runState } from './combinators';
import type { State } from './combinators';
import { updateRecognitionR } from './recognition';
import { allocateCapacity, type CapacityFn, type NeedFn } from './allocation';
import { evolveCommons } from './commons';

// ============================================================================
// System Types (Pure Functional)
// ============================================================================

/**
 * System evolution function
 * Evolve: SystemState → Real → SystemState
 */
export type SystemEvolution = 
  (state: SystemState) =>
  (deltaTime: Real) =>
  SystemState;

/**
 * System initialization function (λ-R spec)
 * initialize_system : Set Entity → SystemState
 */
export type SystemInit = 
  (universe: Set<Entity>) =>
  (matrix: RecognitionMatrix) =>
  SystemState;

// ============================================================================
// System Initialization (Fully Curried)
// ============================================================================

/**
 * Initialize system (λ-R spec):
 * initialize_system : Set Entity → SystemState
 */
export const initSystem: SystemInit = 
  (universe: Set<Entity>) =>
  (matrix: RecognitionMatrix): SystemState => ({
    universe,
    recognitionMatrix: matrix,
    collectives: [],
    commons: [],
    hyperCollectives: [],
    timestamp: Date.now(),
  });

/**
 * Initialize with metadata: Set Entity → RecognitionMatrix → Record → SystemState
 */
export const initSystemWithMetadata = 
  (universe: Set<Entity>) =>
  (matrix: RecognitionMatrix) =>
  (metadata: Record<string, unknown>): SystemState => ({
    universe,
    recognitionMatrix: matrix,
    collectives: [],
    commons: [],
    hyperCollectives: [],
    timestamp: Date.now(),
    metadata,
  });

// ============================================================================
// System Queries (Curried)
// ============================================================================

/**
 * Get universe: SystemState → Set Entity (λ-R spec)
 */
export const getUniverse = (state: SystemState): Set<Entity> =>
  state.universe;

/**
 * Get recognition matrix: SystemState → RecognitionMatrix
 */
export const getRecognitionMatrix = (state: SystemState): RecognitionMatrix =>
  state.recognitionMatrix;

/**
 * Get collectives: SystemState → Collective[]
 */
export const getCollectives = (state: SystemState): Collective[] =>
  state.collectives;

/**
 * Get commons: SystemState → Commons[]
 */
export const getCommons = (state: SystemState): CommonsType[] =>
  state.commons;

/**
 * Get timestamp: SystemState → Real
 */
export const getTimestamp = (state: SystemState): Real =>
  state.timestamp;

/**
 * Find entity by ID: String → SystemState → Entity | undefined
 */
export const findEntity = 
  (id: string) =>
  (state: SystemState): Entity | undefined =>
    Array.from(state.universe).find(e => e.id === id);

/**
 * Find collective by ID: String → SystemState → Collective | undefined
 */
export const findCollective = 
  (id: string) =>
  (state: SystemState): Collective | undefined =>
    state.collectives.find(c => c.id === id);

/**
 * Find commons by ID: String → SystemState → Commons | undefined
 */
export const findCommons = 
  (id: string) =>
  (state: SystemState): CommonsType | undefined =>
    state.commons.find(c => c.id === id);

// ============================================================================
// System Transformations (Curried)
// ============================================================================

/**
 * Add entity: Entity → SystemState → SystemState
 */
export const addEntity = 
  (entity: Entity) =>
  (state: SystemState): SystemState => {
    const newUniverse = new Set(state.universe);
    newUniverse.add(entity);
    return {
      ...state,
      universe: newUniverse,
    };
  };

/**
 * Remove entity: String → SystemState → SystemState
 */
export const removeEntity = 
  (entityId: string) =>
  (state: SystemState): SystemState => {
    const newUniverse = new Set(
      Array.from(state.universe).filter(e => e.id !== entityId)
    );
    return {
      ...state,
      universe: newUniverse,
    };
  };

/**
 * Update entity: Entity → SystemState → SystemState
 */
export const updateEntity = 
  (entity: Entity) =>
  (state: SystemState): SystemState => {
    const newUniverse = new Set(
      Array.from(state.universe).map(e => e.id === entity.id ? entity : e)
    );
    return {
      ...state,
      universe: newUniverse,
    };
  };

/**
 * Add collective: Collective → SystemState → SystemState
 */
export const addCollective = 
  (collective: Collective) =>
  (state: SystemState): SystemState => ({
    ...state,
    collectives: [...state.collectives, collective],
  });

/**
 * Remove collective: String → SystemState → SystemState
 */
export const removeCollective = 
  (collectiveId: string) =>
  (state: SystemState): SystemState => ({
    ...state,
    collectives: state.collectives.filter(c => c.id !== collectiveId),
  });

/**
 * Add commons: Commons → SystemState → SystemState
 */
export const addCommons = 
  (commons: CommonsType) =>
  (state: SystemState): SystemState => ({
    ...state,
    commons: [...state.commons, commons],
  });

/**
 * Remove commons: String → SystemState → SystemState
 */
export const removeCommons = 
  (commonsId: string) =>
  (state: SystemState): SystemState => ({
    ...state,
    commons: state.commons.filter(c => c.id !== commonsId),
  });

/**
 * Update recognition matrix: RecognitionMatrix → SystemState → SystemState
 */
export const setRecognitionMatrix = 
  (matrix: RecognitionMatrix) =>
  (state: SystemState): SystemState => ({
    ...state,
    recognitionMatrix: matrix,
  });

/**
 * Update timestamp: Real → SystemState → SystemState
 */
export const setTimestamp = 
  (timestamp: Real) =>
  (state: SystemState): SystemState => ({
    ...state,
    timestamp,
  });

// ============================================================================
// System Evolution (Fully Curried)
// ============================================================================

/**
 * Evolve system: SystemState → Real → SystemState
 * 
 * Following spec:
 * 1. Update timestamp
 * 2. Process recognition updates
 * 3. Update collectives
 * 4. Update commons
 * 5. Allocate resources
 */
export const evolveSystem: SystemEvolution = 
  (state: SystemState) =>
  (deltaTime: Real): SystemState => {
    // Update timestamp
    const newTimestamp = state.timestamp + deltaTime;
    
    // System evolves naturally (could add recognition decay, etc.)
    return {
      ...state,
      timestamp: newTimestamp,
    };
  };

/**
 * Evolve with recognition update: Entity → Entity → Real → SystemState → SystemState
 */
export const evolveWithRecognition = 
  (recognizer: Entity) =>
  (recognized: Entity) =>
  (amount: Real) =>
  (state: SystemState): SystemState => {
    // Update recognition matrix
    const oldValue = state.recognitionMatrix.matrix[recognizer.id]?.[recognized.id] || 0;
    const newValue = oldValue + amount;
    
    const newMatrix: RecognitionMatrix = {
      matrix: {
        ...state.recognitionMatrix.matrix,
        [recognizer.id]: {
          ...state.recognitionMatrix.matrix[recognizer.id],
          [recognized.id]: newValue,
        },
      },
    };
    
    return setRecognitionMatrix(newMatrix)(state);
  };

/**
 * Evolve with allocation: CapacityFn → NeedFn → SystemState → SystemState
 */
export const evolveWithAllocation = 
  (capacityFn: CapacityFn) =>
  (needFn: NeedFn) =>
  (state: SystemState): SystemState => {
    // Get all members from collectives
    const allMembers = new Set<Entity>();
    
    for (const collective of state.collectives) {
      for (const member of collective.members) {
        allMembers.add(member);
      }
    }
    
    // If no collectives, use universe
    const providers = allMembers.size > 0 ? allMembers : state.universe;
    const recipients = allMembers.size > 0 ? allMembers : state.universe;
    
    // Allocate capacity
    const result = allocateCapacity(state.recognitionMatrix)
      (providers)
      (recipients)
      (capacityFn)
      (needFn)
      (100)
      (0.001);
    
    // Update commons with allocations
    const updatedCommons = state.commons.map(commons => 
      evolveCommons(commons)(result.allocations)
    );
    
    return {
      ...state,
      commons: updatedCommons,
    };
  };

/**
 * Evolve step by step: SystemState → Real → CapacityFn → NeedFn → SystemState
 */
export const evolveStep = 
  (state: SystemState) =>
  (deltaTime: Real) =>
  (capacityFn: CapacityFn) =>
  (needFn: NeedFn): SystemState => {
    const evolved = evolveSystem(state)(deltaTime);
    return evolveWithAllocation(capacityFn)(needFn)(evolved);
  };

// ============================================================================
// System Metrics (Curried)
// ============================================================================

/**
 * Total entities: SystemState → Number (λ-R spec)
 */
export const totalEntities = (state: SystemState): number =>
  state.universe.size;

/**
 * Total collectives: SystemState → Number
 */
export const totalCollectives = (state: SystemState): number =>
  state.collectives.length;

/**
 * Total commons: SystemState → Number
 */
export const totalCommons = (state: SystemState): number =>
  state.commons.length;

/**
 * Total recognition: SystemState → Real
 */
export const totalRecognition = (state: SystemState): Real => {
  let total = 0;
  const matrix = state.recognitionMatrix.matrix;
  for (const recognizer of Object.values(matrix)) {
    for (const amount of Object.values(recognizer)) {
      total += amount;
    }
  }
  return total;
};

/**
 * Average recognition: SystemState → Real
 */
export const avgRecognition = (state: SystemState): Real => {
  const total = totalRecognition(state);
  const count = state.universe.size;
  return count > 0 ? total / (count * count) : 0;
};

/**
 * Network density: SystemState → Real
 */
export const networkDensity = (state: SystemState): Real => {
  const matrix = state.recognitionMatrix.matrix;
  let connections = 0;
  let possible = 0;
  
  for (const recognizer of Object.keys(matrix)) {
    for (const recognized of Object.keys(matrix)) {
      if (recognizer !== recognized) {
        possible++;
        if ((matrix[recognizer]?.[recognized] || 0) > 0) {
          connections++;
        }
      }
    }
  }
  
  return possible > 0 ? connections / possible : 0;
};

// ============================================================================
// System Convergence (Curried)
// ============================================================================

/**
 * Check convergence: SystemState → SystemState → Real → Bool
 */
export const hasConverged = 
  (oldState: SystemState) =>
  (newState: SystemState) =>
  (threshold: Real): boolean => {
    const oldTotal = totalRecognition(oldState);
    const newTotal = totalRecognition(newState);
    const change = Math.abs(newTotal - oldTotal);
    return change < threshold;
  };

/**
 * Iterate until convergence: SystemState → (SystemState → SystemState) → Real → Number → SystemState
 */
export const iterateUntilConvergence = 
  (initialState: SystemState) =>
  (evolveFn: (state: SystemState) => SystemState) =>
  (threshold: Real) =>
  (maxIterations: number): SystemState => {
    let state = initialState;
    
    for (let i = 0; i < maxIterations; i++) {
      const nextState = evolveFn(state);
      
      if (hasConverged(state)(nextState)(threshold)) {
        return nextState;
      }
      
      state = nextState;
    }
    
    return state;
  };

// ============================================================================
// System Snapshots (Curried)
// ============================================================================

/**
 * Take snapshot: SystemState → SystemState
 */
export const takeSnapshot = (state: SystemState): SystemState => ({
  ...state,
  metadata: {
    ...state.metadata,
    snapshotTime: Date.now(),
  },
});

/**
 * Compare states: SystemState → SystemState → Record
 */
export const compareStates = 
  (state1: SystemState) =>
  (state2: SystemState): Record<string, unknown> => ({
    entityDiff: state2.universe.size - state1.universe.size,
    collectiveDiff: state2.collectives.length - state1.collectives.length,
    commonsDiff: state2.commons.length - state1.commons.length,
    recognitionDiff: totalRecognition(state2) - totalRecognition(state1),
    timeDiff: state2.timestamp - state1.timestamp,
  });

// ============================================================================
// System Utilities (Curried)
// ============================================================================

/**
 * Clone system: SystemState → SystemState
 */
export const cloneSystem = (state: SystemState): SystemState => ({
  universe: new Set(state.universe),
  recognitionMatrix: {
    matrix: JSON.parse(JSON.stringify(state.recognitionMatrix.matrix)),
  },
  collectives: [...state.collectives],
  commons: [...state.commons],
  hyperCollectives: state.hyperCollectives ? [...state.hyperCollectives] : [],
  timestamp: state.timestamp,
  metadata: state.metadata ? { ...state.metadata } : undefined,
});

/**
 * Reset timestamp: SystemState → SystemState
 */
export const resetTimestamp = (state: SystemState): SystemState => ({
  ...state,
  timestamp: Date.now(),
});

/**
 * Clear metadata: SystemState → SystemState
 */
export const clearMetadata = (state: SystemState): SystemState => ({
  ...state,
  metadata: {},
});

// ============================================================================
// Export curried operations
// ============================================================================

export const curriedSystem = {
  initSystem,
  initSystemWithMetadata,
  getUniverse,
  getRecognitionMatrix,
  getCollectives,
  getCommons,
  getTimestamp,
  findEntity,
  findCollective,
  findCommons,
  addEntity,
  removeEntity,
  updateEntity,
  addCollective,
  removeCollective,
  addCommons,
  removeCommons,
  setRecognitionMatrix,
  setTimestamp,
  evolveSystem,
  evolveWithRecognition,
  evolveWithAllocation,
  evolveStep,
  totalEntities,
  totalCollectives,
  totalCommons,
  totalRecognition,
  avgRecognition,
  networkDensity,
  hasConverged,
  iterateUntilConvergence,
  takeSnapshot,
  compareStates,
  cloneSystem,
  resetTimestamp,
  clearMetadata,
};

