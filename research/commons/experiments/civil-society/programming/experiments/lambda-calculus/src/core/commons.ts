/**
 * Lambda Calculus Implementation: Commons Formation and Evolution
 * 
 * This module implements:
 * - Commons formation based on conditions and thresholds
 * - Commons evolution (membership changes over time)
 * - Resource allocation within commons
 */

import type {
  Entity,
  Real,
  Distribution,
  Commons,
  RecognitionMatrix,
  Filter,
  Limit,
} from './types';
import { normalize } from './types';
import { filterSet, entitiesToIds, sumOver } from './primitives';
import { mutual, mrd, averageMR } from './recognition';
import type { FilterFunction } from './filters';
import type { LimitFunction } from './limits';
import { applyLimits } from './limits';

// ============================================================================
// Commons Formation
// ============================================================================

/**
 * Form a commons based on a condition and MRD threshold
 */
export function formCommons(
  id: string,
  condition: (entity: Entity) => boolean,
  threshold: Real,
  universe: Set<Entity>,
  matrix: RecognitionMatrix,
  filterFns: FilterFunction[],
  limitFns: LimitFunction[],
  filters: Filter[],
  limits: Limit[],
  initialResources: Real = 0,
  metadata?: Record<string, unknown>
): Commons {
  // Get candidates that match condition
  const candidates = filterSet(condition, universe);
  const candidateIds = entitiesToIds(candidates);
  const universeIds = entitiesToIds(universe);

  // Filter by MRD threshold
  const members = new Set<string>();
  for (const entityId of candidateIds) {
    const entityMRD = mrd(matrix, entityId, universeIds);
    if (entityMRD >= threshold) {
      members.add(entityId);
    }
  }

  return {
    id,
    condition,
    threshold,
    resources: initialResources,
    members,
    filters,
    limits,
    metadata,
  };
}

// ============================================================================
// Commons Evolution
// ============================================================================

/**
 * Evolve a commons by updating membership based on MRD
 * 
 * Rules:
 * - New members join if MRD >= threshold
 * - Current members leave if MRD < leavingFactor * threshold
 */
export function evolveCommons(
  commons: Commons,
  matrix: RecognitionMatrix,
  universe: Set<Entity>,
  leavingFactor: Real = 0.5
): Commons {
  const universeIds = entitiesToIds(universe);
  const currentMembers = commons.members;
  const threshold = commons.threshold;
  const leavingThreshold = leavingFactor * threshold;

  // Calculate MRD for current members (relative to current membership)
  const avgMRInCommons = averageMR(matrix, currentMembers);

  const newMembers = new Set<string>();

  // Check current members - keep if MRD >= leaving threshold
  for (const memberId of currentMembers) {
    const memberMR = sumOver((otherId) => mutual(matrix, memberId, otherId), currentMembers);
    const memberMRD = avgMRInCommons > 0 ? memberMR / avgMRInCommons : 0;

    if (memberMRD >= leavingThreshold) {
      newMembers.add(memberId);
    }
  }

  // Check candidates from universe
  const candidates = filterSet(commons.condition, universe);
  const candidateIds = entitiesToIds(candidates);

  for (const candidateId of candidateIds) {
    if (!currentMembers.has(candidateId)) {
      // Calculate MRD relative to current members
      const candidateMR = sumOver(
        (memberId) => mutual(matrix, candidateId, memberId),
        currentMembers
      );
      const candidateMRD = avgMRInCommons > 0 ? candidateMR / avgMRInCommons : 0;

      if (candidateMRD >= threshold) {
        newMembers.add(candidateId);
      }
    }
  }

  return {
    ...commons,
    members: newMembers,
  };
}

/**
 * Evolve multiple commons simultaneously
 */
export function evolveMultipleCommons(
  commonsList: Commons[],
  matrix: RecognitionMatrix,
  universe: Set<Entity>,
  leavingFactor: Real = 0.5
): Commons[] {
  return commonsList.map((commons) =>
    evolveCommons(commons, matrix, universe, leavingFactor)
  );
}

// ============================================================================
// Commons Resource Allocation
// ============================================================================

/**
 * Allocate commons resources based on MRD
 * Higher MRD receives proportionally more resources
 */
export function allocateCommons(
  commons: Commons,
  matrix: RecognitionMatrix,
  limitFns: LimitFunction[]
): Distribution {
  const members = commons.members;
  const resources = commons.resources;

  if (members.size === 0 || resources === 0) {
    return { weights: {}, total: 0 };
  }

  // Calculate MRD for each member
  const mrds: Record<string, Real> = {};
  let totalMRD = 0;

  for (const memberId of members) {
    const memberMRD = mrd(matrix, memberId, members);
    mrds[memberId] = memberMRD;
    totalMRD += memberMRD;
  }

  // Base allocation proportional to MRD
  const baseWeights: Record<string, Real> = {};
  if (totalMRD > 0) {
    for (const memberId of members) {
      baseWeights[memberId] = mrds[memberId] / totalMRD;
    }
  } else {
    // Uniform allocation if no MRD
    const uniform = 1 / members.size;
    for (const memberId of members) {
      baseWeights[memberId] = uniform;
    }
  }

  // Apply limits
  const limitedDist = applyLimits(limitFns, normalize(baseWeights));

  // Scale by available resources
  const scaledWeights: Record<string, Real> = {};
  for (const [id, weight] of Object.entries(limitedDist.weights)) {
    scaledWeights[id] = (weight / limitedDist.total) * resources;
  }

  return { weights: scaledWeights, total: resources };
}

/**
 * Allocate resources from multiple commons to recipients
 * Returns a map of commons ID to distribution
 */
export function allocateMultipleCommons(
  commonsList: Commons[],
  matrix: RecognitionMatrix,
  limitFnsMap: Map<string, LimitFunction[]>
): Map<string, Distribution> {
  const result = new Map<string, Distribution>();

  for (const commons of commonsList) {
    const limitFns = limitFnsMap.get(commons.id) || [];
    const distribution = allocateCommons(commons, matrix, limitFns);
    result.set(commons.id, distribution);
  }

  return result;
}

// ============================================================================
// Commons Resource Management
// ============================================================================

/**
 * Add resources to a commons
 */
export function addResources(commons: Commons, amount: Real): Commons {
  return {
    ...commons,
    resources: commons.resources + amount,
  };
}

/**
 * Remove resources from a commons
 */
export function removeResources(commons: Commons, amount: Real): Commons {
  return {
    ...commons,
    resources: Math.max(0, commons.resources - amount),
  };
}

/**
 * Set resources for a commons
 */
export function setResources(commons: Commons, amount: Real): Commons {
  return {
    ...commons,
    resources: Math.max(0, amount),
  };
}

/**
 * Transfer resources between commons
 */
export function transferResources(
  fromCommons: Commons,
  toCommons: Commons,
  amount: Real
): { from: Commons; to: Commons } {
  const actualAmount = Math.min(amount, fromCommons.resources);

  return {
    from: removeResources(fromCommons, actualAmount),
    to: addResources(toCommons, actualAmount),
  };
}

// ============================================================================
// Commons Membership Queries
// ============================================================================

/**
 * Check if an entity is a member of a commons
 */
export function isCommonsMember(commons: Commons, entityId: string): boolean {
  return commons.members.has(entityId);
}

/**
 * Get commons size
 */
export function commonsSize(commons: Commons): number {
  return commons.members.size;
}

/**
 * Get all commons an entity is a member of
 */
export function getEntityCommons(
  entityId: string,
  commonsList: Commons[]
): Commons[] {
  return commonsList.filter((commons) => isCommonsMember(commons, entityId));
}

/**
 * Get common members between two commons
 */
export function getCommonMembers(commons1: Commons, commons2: Commons): Set<string> {
  const common = new Set<string>();
  for (const member of commons1.members) {
    if (commons2.members.has(member)) {
      common.add(member);
    }
  }
  return common;
}

// ============================================================================
// Commons Metrics
// ============================================================================

/**
 * Calculate average MRD of commons members
 */
export function averageCommonsMRD(
  commons: Commons,
  matrix: RecognitionMatrix
): Real {
  if (commons.members.size === 0) return 0;

  let totalMRD = 0;
  for (const memberId of commons.members) {
    totalMRD += mrd(matrix, memberId, commons.members);
  }

  return totalMRD / commons.members.size;
}

/**
 * Calculate total mutual recognition within commons
 */
export function totalCommonsMR(
  commons: Commons,
  matrix: RecognitionMatrix
): Real {
  let total = 0;
  for (const member1 of commons.members) {
    for (const member2 of commons.members) {
      total += mutual(matrix, member1, member2);
    }
  }
  return total;
}

/**
 * Calculate cohesion of a commons (average mutual recognition)
 */
export function commonsCohesion(
  commons: Commons,
  matrix: RecognitionMatrix
): Real {
  if (commons.members.size === 0) return 0;
  const total = totalCommonsMR(commons, matrix);
  return total / (commons.members.size * commons.members.size);
}

/**
 * Calculate commons stability (proportion of members above threshold)
 */
export function commonsStability(
  commons: Commons,
  matrix: RecognitionMatrix
): Real {
  if (commons.members.size === 0) return 0;

  let aboveThreshold = 0;
  for (const memberId of commons.members) {
    const memberMRD = mrd(matrix, memberId, commons.members);
    if (memberMRD >= commons.threshold) {
      aboveThreshold++;
    }
  }

  return aboveThreshold / commons.members.size;
}

// ============================================================================
// Commons Comparison and Merging
// ============================================================================

/**
 * Merge two commons into one
 */
export function mergeCommons(
  id: string,
  commons1: Commons,
  commons2: Commons,
  newCondition: (entity: Entity) => boolean,
  newThreshold: Real
): Commons {
  const members = new Set([...commons1.members, ...commons2.members]);
  const resources = commons1.resources + commons2.resources;
  const filters = [...commons1.filters, ...commons2.filters];
  const limits = [...commons1.limits, ...commons2.limits];

  return {
    id,
    condition: newCondition,
    threshold: newThreshold,
    resources,
    members,
    filters,
    limits,
  };
}

/**
 * Split a commons based on a new condition
 */
export function splitCommons(
  id1: string,
  id2: string,
  commons: Commons,
  condition1: (entity: Entity) => boolean,
  condition2: (entity: Entity) => boolean,
  entities: Set<Entity>,
  resourceSplit: Real = 0.5 // Proportion to first commons
): { commons1: Commons; commons2: Commons } {
  const members1 = new Set<string>();
  const members2 = new Set<string>();

  for (const entity of entities) {
    if (commons.members.has(entity.id)) {
      if (condition1(entity)) {
        members1.add(entity.id);
      }
      if (condition2(entity)) {
        members2.add(entity.id);
      }
    }
  }

  const commons1: Commons = {
    id: id1,
    condition: condition1,
    threshold: commons.threshold,
    resources: commons.resources * resourceSplit,
    members: members1,
    filters: commons.filters,
    limits: commons.limits,
  };

  const commons2: Commons = {
    id: id2,
    condition: condition2,
    threshold: commons.threshold,
    resources: commons.resources * (1 - resourceSplit),
    members: members2,
    filters: commons.filters,
    limits: commons.limits,
  };

  return { commons1, commons2 };
}

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Create a universal commons (all entities can join)
 */
export function universalCommons(
  id: string,
  threshold: Real,
  universe: Set<Entity>,
  matrix: RecognitionMatrix,
  filters: Filter[] = [],
  limits: Limit[] = []
): Commons {
  return formCommons(
    id,
    () => true,
    threshold,
    universe,
    matrix,
    [],
    [],
    filters,
    limits
  );
}

/**
 * Create a conditional commons (based on metadata)
 */
export function conditionalCommons(
  id: string,
  metadataKey: string,
  metadataValue: unknown,
  threshold: Real,
  universe: Set<Entity>,
  matrix: RecognitionMatrix,
  filters: Filter[] = [],
  limits: Limit[] = []
): Commons {
  const condition = (entity: Entity) => entity.metadata?.[metadataKey] === metadataValue;
  
  return formCommons(
    id,
    condition,
    threshold,
    universe,
    matrix,
    [],
    [],
    filters,
    limits
  );
}

