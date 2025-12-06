/**
 * Elegant Lambda Calculus: Capacity Allocation System
 * 
 * Fully curried allocation implementation following lambda calculus principles:
 * - Iterative allocation algorithm
 * - Full currying for composition
 * - Monadic state management
 */

import type { 
  Entity, 
  Real, 
  RecognitionMatrix, 
  Distribution,
  Collective,
} from '../core/types';
import { normalize, getProb } from '../core/types';
import { pipe, runState } from './combinators';
import type { State } from './combinators';
import { scmrs } from './collective';

// ============================================================================
// Allocation Types (Pure Functional)
// ============================================================================

/**
 * Capacity function: Entity → Real
 */
export type CapacityFn = (entity: Entity) => Real;

/**
 * Need function: Entity → Real
 */
export type NeedFn = (entity: Entity) => Real;

/**
 * Allocation state
 */
export interface AllocationState {
  allocations: Map<string, Real>;
  remaining: Map<string, Real>;
  satisfied: Set<string>;
  iteration: number;
}

/**
 * Allocation result
 */
export interface AllocationResult {
  allocations: Distribution;
  iterations: number;
  converged: boolean;
}

// ============================================================================
// Allocation Algorithm (Fully Curried)
// ============================================================================

/**
 * Initialize allocation state: Set Entity → CapacityFn → AllocationState
 */
export const initAllocationState = 
  (providers: Set<Entity>) =>
  (capacityFn: CapacityFn): AllocationState => {
    const remaining = new Map<string, Real>();
    for (const provider of providers) {
      remaining.set(provider.id, capacityFn(provider));
    }
    
    return {
      allocations: new Map(),
      remaining,
      satisfied: new Set(),
      iteration: 0,
    };
  };

/**
 * Allocate capacity: RecognitionMatrix → Set Entity → Set Entity → CapacityFn → NeedFn → AllocationResult
 * 
 * Following spec:
 * 1. Initialize: For each provider p, remaining[p] = capacity(p)
 * 2. For each recipient r:
 *    a. Calculate MRS distribution over providers
 *    b. Allocate according to MRS, limited by remaining capacity
 *    c. Update remaining capacity
 * 3. Iterate until convergence or max iterations
 */
export const allocateCapacity = 
  (matrix: RecognitionMatrix) =>
  (providers: Set<Entity>) =>
  (recipients: Set<Entity>) =>
  (capacityFn: CapacityFn) =>
  (needFn: NeedFn) =>
  (maxIterations: number = 100) =>
  (convergenceThreshold: Real = 0.001): AllocationResult => {
    // Initialize state
    let state = initAllocationState(providers)(capacityFn);
    let converged = false;
    
    // Iterate until convergence
    for (let i = 0; i < maxIterations; i++) {
      const prevAllocations = new Map(state.allocations);
      
      // For each recipient
      for (const recipient of recipients) {
        const need = needFn(recipient);
        if (need <= 0) continue;
        
        // Get current allocation
        const currentAlloc = state.allocations.get(recipient.id) || 0;
        const stillNeeded = Math.max(0, need - currentAlloc);
        
        if (stillNeeded <= convergenceThreshold) {
          state.satisfied.add(recipient.id);
          continue;
        }
        
        // Calculate MRS distribution
        const mrsDist = scmrs(matrix)(providers)(recipients)([])([])(recipient);
        
        // Allocate according to MRS, limited by remaining capacity
        let allocated = 0;
        for (const [providerId, weight] of Object.entries(mrsDist.weights)) {
          const remaining = state.remaining.get(providerId) || 0;
          const desired = (weight / mrsDist.total) * stillNeeded;
          const actual = Math.min(desired, remaining);
          
          allocated += actual;
          state.remaining.set(providerId, remaining - actual);
        }
        
        // Update allocation
        state.allocations.set(recipient.id, currentAlloc + allocated);
      }
      
      // Check convergence
      let maxChange = 0;
      for (const [id, alloc] of state.allocations) {
        const prev = prevAllocations.get(id) || 0;
        const change = Math.abs(alloc - prev);
        maxChange = Math.max(maxChange, change);
      }
      
      state.iteration = i + 1;
      
      if (maxChange < convergenceThreshold) {
        converged = true;
        break;
      }
    }
    
    // Convert to distribution
    const weights: Record<string, Real> = {};
    for (const [id, alloc] of state.allocations) {
      weights[id] = alloc;
    }
    
    return {
      allocations: normalize(weights),
      iterations: state.iteration,
      converged,
    };
  };

// ============================================================================
// Allocation Strategies (Curried)
// ============================================================================

/**
 * Equal allocation: Set Entity → Real → Distribution
 */
export const equalAllocation = 
  (recipients: Set<Entity>) =>
  (totalCapacity: Real): Distribution => {
    const perRecipient = totalCapacity / recipients.size;
    const weights: Record<string, Real> = {};
    for (const recipient of recipients) {
      weights[recipient.id] = perRecipient;
    }
    return normalize(weights);
  };

/**
 * Proportional allocation: NeedFn → Set Entity → Real → Distribution
 */
export const proportionalAllocation = 
  (needFn: NeedFn) =>
  (recipients: Set<Entity>) =>
  (totalCapacity: Real): Distribution => {
    const totalNeed = Array.from(recipients).reduce(
      (sum, r) => sum + needFn(r),
      0
    );
    
    if (totalNeed === 0) {
      return equalAllocation(recipients)(totalCapacity);
    }
    
    const weights: Record<string, Real> = {};
    for (const recipient of recipients) {
      const need = needFn(recipient);
      weights[recipient.id] = (need / totalNeed) * totalCapacity;
    }
    
    return normalize(weights);
  };

/**
 * Priority allocation: (Entity → Real) → Set Entity → Real → Distribution
 */
export const priorityAllocation = 
  (priorityFn: (entity: Entity) => Real) =>
  (recipients: Set<Entity>) =>
  (totalCapacity: Real): Distribution => {
    // Sort by priority
    const sorted = Array.from(recipients).sort(
      (a, b) => priorityFn(b) - priorityFn(a)
    );
    
    const weights: Record<string, Real> = {};
    let remaining = totalCapacity;
    
    for (const recipient of sorted) {
      if (remaining <= 0) break;
      const priority = priorityFn(recipient);
      const allocated = Math.min(priority, remaining);
      weights[recipient.id] = allocated;
      remaining -= allocated;
    }
    
    return normalize(weights);
  };

// ============================================================================
// Allocation Constraints (Curried)
// ============================================================================

/**
 * Min allocation: Real → Distribution → Distribution
 */
export const minAllocation = 
  (minimum: Real) =>
  (dist: Distribution): Distribution => {
    const adjusted: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const normalized = weight / dist.total;
      adjusted[id] = Math.max(minimum, normalized);
    }
    return normalize(adjusted);
  };

/**
 * Max allocation: Real → Distribution → Distribution
 */
export const maxAllocation = 
  (maximum: Real) =>
  (dist: Distribution): Distribution => {
    const adjusted: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const normalized = weight / dist.total;
      adjusted[id] = Math.min(maximum, normalized);
    }
    return normalize(adjusted);
  };

/**
 * Cap total allocation: Real → Distribution → Distribution
 */
export const capTotal = 
  (cap: Real) =>
  (dist: Distribution): Distribution => {
    if (dist.total <= cap) return dist;
    const factor = cap / dist.total;
    const capped: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      capped[id] = weight * factor;
    }
    return { weights: capped, total: cap };
  };

// ============================================================================
// Allocation Queries (Curried)
// ============================================================================

/**
 * Get allocation for entity: Distribution → String → Real
 */
export const getAllocation = 
  (dist: Distribution) =>
  (entityId: string): Real =>
    getProb(dist, entityId);

/**
 * Total allocated: Distribution → Real
 */
export const totalAllocated = (dist: Distribution): Real =>
  dist.total;

/**
 * Count recipients: Distribution → Number
 */
export const countRecipients = (dist: Distribution): number =>
  Object.keys(dist.weights).length;

/**
 * Average allocation: Distribution → Real
 */
export const avgAllocation = (dist: Distribution): Real => {
  const count = countRecipients(dist);
  return count > 0 ? dist.total / count : 0;
};

/**
 * Max allocated: Distribution → Real
 */
export const maxAllocated = (dist: Distribution): Real => {
  const values = Object.values(dist.weights);
  return values.length > 0 ? Math.max(...values) : 0;
};

/**
 * Min allocated: Distribution → Real
 */
export const minAllocated = (dist: Distribution): Real => {
  const values = Object.values(dist.weights);
  return values.length > 0 ? Math.min(...values) : 0;
};

// ============================================================================
// Allocation Metrics (Curried)
// ============================================================================

/**
 * Satisfaction rate: NeedFn → Set Entity → Distribution → Real
 */
export const satisfactionRate = 
  (needFn: NeedFn) =>
  (recipients: Set<Entity>) =>
  (allocation: Distribution): Real => {
    let totalNeed = 0;
    let totalSatisfied = 0;
    
    for (const recipient of recipients) {
      const need = needFn(recipient);
      const allocated = getAllocation(allocation)(recipient.id);
      totalNeed += need;
      totalSatisfied += Math.min(need, allocated);
    }
    
    return totalNeed > 0 ? totalSatisfied / totalNeed : 1;
  };

/**
 * Utilization rate: CapacityFn → Set Entity → Distribution → Real
 */
export const utilizationRate = 
  (capacityFn: CapacityFn) =>
  (providers: Set<Entity>) =>
  (allocation: Distribution): Real => {
    const totalCapacity = Array.from(providers).reduce(
      (sum, p) => sum + capacityFn(p),
      0
    );
    
    return totalCapacity > 0 ? allocation.total / totalCapacity : 0;
  };

/**
 * Fairness (Gini coefficient): Distribution → Real
 */
export const fairness = (dist: Distribution): Real => {
  const values = Object.values(dist.weights).map(w => w / dist.total);
  if (values.length === 0) return 1;
  if (values.length === 1) return 1;
  
  values.sort((a, b) => a - b);
  
  let sum = 0;
  for (let i = 0; i < values.length; i++) {
    sum += (2 * (i + 1) - values.length - 1) * values[i];
  }
  
  const n = values.length;
  const mean = values.reduce((a, b) => a + b, 0) / n;
  
  if (mean === 0) return 1;
  const gini = sum / (n * n * mean);
  
  // Return 1 - gini so that higher is more fair
  return 1 - gini;
};

// ============================================================================
// Allocation Transformations (Curried)
// ============================================================================

/**
 * Redistribute: Distribution → (String → Real) → Distribution
 */
export const redistribute = 
  (dist: Distribution) =>
  (adjustmentFn: (id: string) => Real): Distribution => {
    const adjusted: Record<string, Real> = {};
    for (const [id, weight] of Object.entries(dist.weights)) {
      const adjustment = adjustmentFn(id);
      adjusted[id] = weight * adjustment;
    }
    return normalize(adjusted);
  };

/**
 * Transfer: String → String → Real → Distribution → Distribution
 */
export const transfer = 
  (fromId: string) =>
  (toId: string) =>
  (amount: Real) =>
  (dist: Distribution): Distribution => {
    const from = dist.weights[fromId] || 0;
    const to = dist.weights[toId] || 0;
    const actualAmount = Math.min(amount, from);
    
    return normalize({
      ...dist.weights,
      [fromId]: from - actualAmount,
      [toId]: to + actualAmount,
    });
  };

// ============================================================================
// Export curried operations
// ============================================================================

export const curriedAllocation = {
  allocateCapacity,
  equalAllocation,
  proportionalAllocation,
  priorityAllocation,
  minAllocation,
  maxAllocation,
  capTotal,
  getAllocation,
  totalAllocated,
  countRecipients,
  avgAllocation,
  maxAllocated,
  minAllocated,
  satisfactionRate,
  utilizationRate,
  fairness,
  redistribute,
  transfer,
};

