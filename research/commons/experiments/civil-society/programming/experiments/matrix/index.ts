/**
 * Free Association Protocol - Elegant Matrix Implementation
 * 
 * Main entry point for the refactored elegant architecture.
 * 
 * Architecture:
 * - core/: Pure mathematical operations (RS, MR, MRS, collectives)
 * - slots/: Multi-dimensional slot system (type, time, location)
 * - allocation/: Enhanced allocation engine (damping, divisibility, convergence)
 * - rpc/: Elegant RPC interfaces (focused, subscription-based)
 * 
 * Example:
 * ```typescript
 * import { MatrixComputer, AllocationEngine } from './index.js';
 * 
 * // Create matrix computer
 * const matrices = new MatrixComputer(100);
 * 
 * // Set recognition
 * matrices.setRecognition(0, 1, 0.6).setRecognition(0, 2, 0.4);
 * 
 * // Compute matrices (fluent interface!)
 * const mrs = matrices.computeRS().computeMR().computeMRS();
 * 
 * // Allocate using enhanced engine
 * const result = AllocationEngine.allocate(
 *   needSlots,
 *   availabilitySlots,
 *   participantShares
 * );
 * ```
 */

// ═══════════════════════════════════════════════════════════════════
// CORE EXPORTS
// ═══════════════════════════════════════════════════════════════════

export {
  MatrixComputer,
  MatrixResult,
  computeMatrices
} from './core/matrix-operations.js';

export {
  CollectiveComputer,
  createCollectiveComputer
} from './core/collective-operations.js';

export {
  Sparse,
  SparsePerf,
  SparseCompare,
  type SparseMatrix
} from './sparse-matrix.js';

// ═══════════════════════════════════════════════════════════════════
// SLOT SYSTEM EXPORTS
// ═══════════════════════════════════════════════════════════════════

export * from './slots/schemas.js';

export {
  TimeMatching,
  LocationMatching,
  ComplianceFilters,
  SlotMatching
} from './slots/matching.js';

export {
  SpaceTimeIndex
} from './slots/indexing.js';

// ═══════════════════════════════════════════════════════════════════
// ALLOCATION SYSTEM EXPORTS
// ═══════════════════════════════════════════════════════════════════

export {
  DampeningSystem,
  type DampingState,
  type MultiTypeDamping
} from './allocation/damping.js';

export {
  DivisibilityConstraints,
  LargestRemainderMethod
} from './allocation/divisibility.js';

export {
  ConvergenceTracker,
  type ConvergenceMetrics
} from './allocation/convergence.js';

export {
  AllocationEngine,
  type AllocationResult
} from './allocation/engine.js';

// ═══════════════════════════════════════════════════════════════════
// RPC INTERFACE EXPORTS
// ═══════════════════════════════════════════════════════════════════

export * from './rpc/interfaces.js';

// ═══════════════════════════════════════════════════════════════════
// LOCAL-FIRST CLIENT EXPORTS
// ═══════════════════════════════════════════════════════════════════

export {
  // Main clients
  LocalFirstClient,
  type LocalFirstClientOptions,
  LocalFirstBatchClient,
  createBatchClient,
  type BatchClientOptions,
  
  // Promise pipelining
  PipelinedRpcWrapper,
  createPipelinedClient,
  pipeline,
  PipelineBuilder,
  
  // Memoization
  memoize,
  createMemoizedMethod,
  LRUCache,
  hashObject,
  createCacheKey,
  type CacheEntry,
  type MemoOptions,
  
  // Persistent cache
  PersistentCache,
  type NetworkStateSnapshot,
  type CachedAllocation,
  type CachedComputation,
  
  // Background sync
  BackgroundSyncManager,
  createSyncOperation,
  type SyncOperation,
  type SyncStatus,
  type SyncEventType,
  type SyncEventHandler
} from './client/index.js';

// ═══════════════════════════════════════════════════════════════════
// CONVENIENCE FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Quick start: Create a complete allocation system
 * 
 * @param maxParticipants - Maximum number of participants
 * @returns Matrix computer ready to use
 */
export function createAllocationSystem(maxParticipants: number = 1000) {
  const matrices = new MatrixComputer(maxParticipants);
  
  return {
    matrices,
    
    /** Set recognition (fluent) */
    setRecognition(i: number, j: number, value: number) {
      matrices.setRecognition(i, j, value);
      return this;
    },
    
    /** Compute all matrices */
    compute() {
      const RS = matrices.computeRS();
      const MR = RS.computeMR();
      const MRS = MR.computeMRS();
      const totalMR = MR.computeTotalMR();
      
      return { RS, MR, MRS, totalMR };
    },
    
    /** Allocate using enhanced engine */
    allocate(
      needSlots: NeedSlot[],
      availabilitySlots: AvailabilitySlot[],
      participantShares: Map<string, number>
    ) {
      return AllocationEngine.allocate(
        needSlots,
        availabilitySlots,
        participantShares
      );
    }
  };
}

// Re-export core classes for internal use
import { MatrixComputer as MC } from './core/matrix-operations.js';
import type { MatrixResult } from './core/matrix-operations.js';
import type { NeedSlot, AvailabilitySlot } from './slots/schemas.js';
const MatrixComputer = MC;

