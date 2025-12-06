/**
 * Lambda Calculus Implementation: Collective Formation
 * 
 * This module implements:
 * - Collective formation from entity sets
 * - SCMRS (Sub-Collective Mutual Recognition Share)
 * - SCRMRS (Sub-Collective Recognized Mutual Recognition Share)
 * - Hyper-collective support
 */

import type {
  Entity,
  Real,
  Distribution,
  Collective,
  RecognitionMatrix,
  ShareType,
  Filter,
  Limit,
  HyperCollective,
} from './types';
import { normalize, getProb, diracDelta } from './types';
import { mutual, mrs } from './recognition';
import { sumOver, sumOverPairs, min as minOp, entitiesToIds } from './primitives';
import type { FilterFunction } from './filters';
import type { LimitFunction } from './limits';
import { applyFilters } from './filters';
import { applyLimits } from './limits';

// ============================================================================
// Collective Formation
// ============================================================================

/**
 * Form a collective from a set of entities with filters and limits
 */
export function formCollective(
  id: string,
  entities: Set<Entity>,
  filterFns: FilterFunction[],
  limitFns: LimitFunction[],
  filters: Filter[],
  limits: Limit[],
  shareType: ShareType,
  metadata?: Record<string, unknown>
): Collective {
  // Apply filters to get members
  const filtered = applyFilters(filterFns, entities);
  const memberIds = entitiesToIds(filtered);

  return {
    id,
    members: memberIds,
    filters,
    limits,
    shareType,
    metadata,
  };
}

// ============================================================================
// SCMRS (Sub-Collective Mutual Recognition Share)
// ============================================================================

/**
 * Calculate SCMRS for a collective
 * SCMRS distributes based on mutual recognition within the collective
 * 
 * SCMRS(e) = (Σ_{f ∈ members} mutual(e, f)) / (Σ_{e,f ∈ members} mutual(e, f))
 */
export function scmrs(
  matrix: RecognitionMatrix,
  collective: Collective
): Distribution {
  const members = collective.members;
  
  if (members.size === 0) {
    return { weights: {}, total: 0 };
  }

  // Calculate total mutual recognition in the collective
  const totalMR = sumOverPairs(
    (eId, fId) => mutual(matrix, eId, fId),
    members,
    members
  );

  if (totalMR === 0) {
    // Uniform distribution if no mutual recognition
    const uniformWeight = 1 / members.size;
    const weights: Record<string, Real> = {};
    for (const id of members) {
      weights[id] = uniformWeight;
    }
    return { weights, total: 1 };
  }

  // Calculate each entity's share
  const weights: Record<string, Real> = {};
  for (const eId of members) {
    const entityMR = sumOver((fId) => mutual(matrix, eId, fId), members);
    weights[eId] = entityMR / totalMR;
  }

  return normalize(weights);
}

/**
 * Calculate SCMRS with limits applied
 */
export function scmrsWithLimits(
  matrix: RecognitionMatrix,
  collective: Collective,
  limitFns: LimitFunction[]
): Distribution {
  const baseDist = scmrs(matrix, collective);
  return applyLimits(limitFns, baseDist);
}

// ============================================================================
// SCRMRS (Sub-Collective Recognized Mutual Recognition Share)
// ============================================================================

/**
 * Calculate SCRMRS for a collective
 * SCRMRS distributes based on how the collective recognizes each entity
 * 
 * SCRMRS(e) = (Σ_{f ∈ members} MRS(f)(e)) / |members|
 */
export function scrmrs(
  matrix: RecognitionMatrix,
  collective: Collective,
  universe: Set<string>
): Distribution {
  const members = collective.members;
  
  if (members.size === 0) {
    return { weights: {}, total: 0 };
  }

  // Calculate MRS for each member
  const weights: Record<string, Real> = {};
  
  // For each potential recipient in universe
  for (const eId of universe) {
    let sum = 0;
    // Sum how much each member recognizes this entity via MRS
    for (const fId of members) {
      const mrsDist = mrs(matrix, fId, universe);
      sum += getProb(mrsDist, eId);
    }
    const share = sum / members.size;
    if (share > 0) {
      weights[eId] = share;
    }
  }

  return normalize(weights);
}

/**
 * Calculate SCRMRS with limits applied
 */
export function scrmrsWithLimits(
  matrix: RecognitionMatrix,
  collective: Collective,
  universe: Set<string>,
  limitFns: LimitFunction[]
): Distribution {
  const baseDist = scrmrs(matrix, collective, universe);
  return applyLimits(limitFns, baseDist);
}

// ============================================================================
// Collective Share Calculation (Generic)
// ============================================================================

/**
 * Calculate distribution for a collective based on its share type
 */
export function collectiveShare(
  matrix: RecognitionMatrix,
  collective: Collective,
  universe: Set<string>,
  limitFns: LimitFunction[]
): Distribution {
  switch (collective.shareType) {
    case 'MRS':
      // For MRS at collective level, use average of member MRS
      return collectiveMRS(matrix, collective, universe, limitFns);
    case 'SCMRS':
      return scmrsWithLimits(matrix, collective, limitFns);
    case 'SCRMRS':
      return scrmrsWithLimits(matrix, collective, universe, limitFns);
    default:
      return { weights: {}, total: 0 };
  }
}

/**
 * Calculate collective-level MRS (average of member MRS)
 */
function collectiveMRS(
  matrix: RecognitionMatrix,
  collective: Collective,
  universe: Set<string>,
  limitFns: LimitFunction[]
): Distribution {
  const members = collective.members;
  
  if (members.size === 0) {
    return { weights: {}, total: 0 };
  }

  const weights: Record<string, Real> = {};
  
  // Average MRS across all members
  for (const targetId of universe) {
    let sum = 0;
    for (const memberId of members) {
      const mrsDist = mrs(matrix, memberId, universe);
      sum += getProb(mrsDist, targetId);
    }
    const avg = sum / members.size;
    if (avg > 0) {
      weights[targetId] = avg;
    }
  }

  const baseDist = normalize(weights);
  return applyLimits(limitFns, baseDist);
}

// ============================================================================
// Hyper-Collective Support
// ============================================================================

/**
 * Calculate mutual recognition between two hyper-collectives
 */
export function mutualHC(
  matrix: RecognitionMatrix,
  hc1: HyperCollective,
  hc2: HyperCollective
): Real {
  if (hc1.type === 'base' && hc2.type === 'base') {
    return mutual(matrix, hc1.entity.id, hc2.entity.id);
  }

  if (hc1.type === 'collective' && hc2.type === 'collective') {
    let sum = 0;
    const members1 = Array.from(hc1.members);
    const members2 = Array.from(hc2.members);
    
    for (const x of members1) {
      for (const y of members2) {
        const weight1 = hc1.weights?.get(x) ?? 1 / members1.length;
        const weight2 = hc2.weights?.get(y) ?? 1 / members2.length;
        sum += weight1 * weight2 * mutualHC(matrix, x, y);
      }
    }
    
    return sum;
  }

  return 0;
}

/**
 * Calculate recognition distribution for a hyper-collective
 */
export function recognitionHC(
  matrix: RecognitionMatrix,
  hc: HyperCollective,
  universe: Set<string>
): Distribution {
  if (hc.type === 'base') {
    // Lift base entity recognition
    const entityId = hc.entity.id;
    const weights: Record<string, Real> = {};
    for (const targetId of universe) {
      weights[targetId] = mutual(matrix, entityId, targetId);
    }
    return normalize(weights);
  }

  // Collective: aggregate member recognitions
  const members = Array.from(hc.members);
  const aggregated: Record<string, Real> = {};
  
  for (const targetId of universe) {
    let sum = 0;
    for (const member of members) {
      const weight = hc.weights?.get(member) ?? 1 / members.length;
      const memberRecognition = recognitionHC(matrix, member, universe);
      sum += weight * getProb(memberRecognition, targetId);
    }
    if (sum > 0) {
      aggregated[targetId] = sum;
    }
  }
  
  return normalize(aggregated);
}

// ============================================================================
// Collective Evolution
// ============================================================================

/**
 * Update collective membership based on MRD
 * Entities can join or leave based on MRD thresholds
 */
export function evolveCollective(
  matrix: RecognitionMatrix,
  collective: Collective,
  joinThreshold: Real,
  leaveThreshold: Real,
  candidates: Set<string>
): Collective {
  const currentMembers = collective.members;
  
  // Calculate average MR for current members
  let avgMR = 0;
  if (currentMembers.size > 0) {
    avgMR = sumOverPairs(
      (a, b) => mutual(matrix, a, b),
      currentMembers,
      currentMembers
    ) / currentMembers.size;
  }

  const newMembers = new Set<string>();

  // Keep members above leave threshold
  for (const memberId of currentMembers) {
    const memberMR = sumOver((otherId) => mutual(matrix, memberId, otherId), currentMembers);
    const mrd = avgMR > 0 ? memberMR / avgMR : 0;
    
    if (mrd >= leaveThreshold) {
      newMembers.add(memberId);
    }
  }

  // Add candidates above join threshold
  for (const candidateId of candidates) {
    if (!currentMembers.has(candidateId)) {
      const candidateMR = sumOver(
        (memberId) => mutual(matrix, candidateId, memberId),
        currentMembers
      );
      const mrd = avgMR > 0 ? candidateMR / avgMR : 0;
      
      if (mrd >= joinThreshold) {
        newMembers.add(candidateId);
      }
    }
  }

  return {
    ...collective,
    members: newMembers,
  };
}

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Check if an entity is a member of a collective
 */
export function isMember(collective: Collective, entityId: string): boolean {
  return collective.members.has(entityId);
}

/**
 * Get collective size
 */
export function collectiveSize(collective: Collective): number {
  return collective.members.size;
}

/**
 * Merge two collectives
 */
export function mergeCollectives(
  id: string,
  collective1: Collective,
  collective2: Collective,
  shareType: ShareType
): Collective {
  const members = new Set([...collective1.members, ...collective2.members]);
  const filters = [...collective1.filters, ...collective2.filters];
  const limits = [...collective1.limits, ...collective2.limits];

  return {
    id,
    members,
    filters,
    limits,
    shareType,
  };
}

/**
 * Get intersection of two collectives
 */
export function intersectCollectives(
  id: string,
  collective1: Collective,
  collective2: Collective,
  shareType: ShareType
): Collective {
  const members = new Set<string>();
  for (const member of collective1.members) {
    if (collective2.members.has(member)) {
      members.add(member);
    }
  }

  return {
    id,
    members,
    filters: [...collective1.filters, ...collective2.filters],
    limits: [...collective1.limits, ...collective2.limits],
    shareType,
  };
}

/**
 * Create a base hyper-collective from an entity
 */
export function baseHC(entity: Entity): HyperCollective {
  return { type: 'base', entity };
}

/**
 * Create a collective hyper-collective from members
 */
export function collectiveHC(
  members: Set<HyperCollective>,
  weights?: Map<HyperCollective, Real>
): HyperCollective {
  return { type: 'collective', members, weights };
}

