/**
 * Elegant Lambda Calculus: Commons System
 * 
 * Fully curried commons implementation following lambda calculus principles:
 * - Commons formation and evolution
 * - Resource allocation to commons
 * - Full currying for composition
 */

import type { 
  Distribution,
} from '../core/types';
import type {
  Entity,
  Real,
  Commons as CommonsType,
  HyperCollective,
  SimpleFilter,
  Limit,
  Predicate,
} from './types';
import { normalize } from '../core/types';
import { pipe } from './combinators';

// ============================================================================
// Commons Types (Pure Functional)
// ============================================================================

/**
 * Commons formation function (λ-R spec)
 * form_commons : (τ → Bool) → Real → List (Filter τ) → List (Limit τ) → Commons τ
 */
export type CommonsFormation = 
  (id: string) =>
  (condition: Predicate<Entity>) =>
  (threshold: Real) =>
  (filters: SimpleFilter<Entity>[]) =>
  (limits: Limit[]) =>
  CommonsType;

/**
 * Commons evolution function
 * EvolveCommons: Commons → Distribution → Commons
 */
export type CommonsEvolution = 
  (commons: CommonsType) =>
  (allocation: Distribution) =>
  CommonsType;

// ============================================================================
// Commons Formation (Fully Curried)
// ============================================================================

/**
 * Create commons (λ-R spec):
 * form_commons : (τ → Bool) → Real → List (Filter τ) → List (Limit τ) → Commons τ
 */
export const createCommons: CommonsFormation = 
  (id: string) =>
  (condition: Predicate<Entity>) =>
  (threshold: Real) =>
  (filters: SimpleFilter<Entity>[]) =>
  (limits: Limit[]): CommonsType => ({
    id,
    condition,
    threshold,
    resources: {},
    members: new Set(), // Initially empty, populated by evolution
    filters,
    limits,
  });

/**
 * Create commons with initial members: String → Predicate → Real → Set Entity → Filters → Limits → Commons
 */
export const createCommonsWithMembers = 
  (id: string) =>
  (condition: Predicate<Entity>) =>
  (threshold: Real) =>
  (initialMembers: Set<Entity>) =>
  (filters: SimpleFilter<Entity>[]) =>
  (limits: Limit[]): CommonsType => ({
    id,
    condition,
    threshold,
    resources: {},
    members: initialMembers,
    filters,
    limits,
  });

// ============================================================================
// Commons Queries (Curried)
// ============================================================================

/**
 * Get members: Commons → Set Entity
 */
export const getMembers = (commons: CommonsType): Set<Entity> =>
  new Set(commons.members);

/**
 * Get threshold: Commons → Real (λ-R spec)
 */
export const getThreshold = (commons: CommonsType): Real =>
  commons.threshold;

/**
 * Get condition: Commons → (Entity → Bool) (λ-R spec)
 */
export const getCondition = (commons: CommonsType): Predicate<Entity> =>
  commons.condition;

/**
 * Get resources: Commons → Record
 */
export const getResources = (commons: CommonsType): Record<string, Real> =>
  commons.resources;

/**
 * Get total resources: Commons → Real
 */
export const getTotalResources = (commons: CommonsType): Real =>
  Object.values(commons.resources).reduce((sum, val) => sum + val, 0);

/**
 * Is member: Commons → Entity → Bool
 */
export const isMember = 
  (commons: CommonsType) =>
  (entity: Entity): boolean =>
    Array.from(commons.members).some((e: Entity) => e.id === entity.id);

/**
 * Member count: Commons → Number
 */
export const memberCount = (commons: CommonsType): number =>
  Array.from(commons.members).length;

// ============================================================================
// Commons Transformations (Curried)
// ============================================================================

/**
 * Add member: Entity → Commons → Commons
 */
export const addMember = 
  (entity: Entity) =>
  (commons: CommonsType): CommonsType => {
    const newMembers = new Set(commons.members);
    newMembers.add(entity);
    return {
      ...commons,
      members: newMembers,
    };
  };

/**
 * Remove member: Entity → Commons → Commons
 */
export const removeMember = 
  (entity: Entity) =>
  (commons: CommonsType): CommonsType => {
    const newMembers = new Set(commons.members);
    newMembers.delete(entity);
    return {
      ...commons,
      members: newMembers,
    };
  };

/**
 * Set threshold: Real → Commons → Commons (λ-R spec)
 */
export const setThreshold = 
  (threshold: Real) =>
  (commons: CommonsType): CommonsType => ({
    ...commons,
    threshold,
  });

/**
 * Add resource: String → Real → Commons → Commons
 */
export const addResource = 
  (resourceId: string) =>
  (amount: Real) =>
  (commons: CommonsType): CommonsType => ({
    ...commons,
    resources: {
      ...commons.resources,
      [resourceId]: (commons.resources[resourceId] || 0) + amount,
    },
  });

/**
 * Remove resource: String → Real → Commons → Commons
 */
export const removeResource = 
  (resourceId: string) =>
  (amount: Real) =>
  (commons: CommonsType): CommonsType => ({
    ...commons,
    resources: {
      ...commons.resources,
      [resourceId]: Math.max(0, (commons.resources[resourceId] || 0) - amount),
    },
  });

/**
 * Set resource: String → Real → Commons → Commons
 */
export const setResource = 
  (resourceId: string) =>
  (amount: Real) =>
  (commons: CommonsType): CommonsType => ({
    ...commons,
    resources: {
      ...commons.resources,
      [resourceId]: amount,
    },
  });

// ============================================================================
// Commons Evolution (Fully Curried)
// ============================================================================

/**
 * Evolve commons: Commons → Distribution → Commons
 * 
 * Following spec: Commons evolves by receiving allocations
 */
export const evolveCommons: CommonsEvolution = 
  (commons: CommonsType) =>
  (allocation: Distribution): CommonsType => {
    // Add allocation to commons resources
    const updatedResources = { ...commons.resources };
    for (const [entityId, amount] of Object.entries(allocation.weights)) {
      updatedResources[entityId] = (updatedResources[entityId] || 0) + amount;
    }
    
    return {
      ...commons,
      resources: updatedResources,
    };
  };

/**
 * Evolve with threshold update: Commons → Distribution → Real → Commons
 */
export const evolveWithThreshold = 
  (commons: CommonsType) =>
  (allocation: Distribution) =>
  (newThreshold: Real): CommonsType => {
    const evolved = evolveCommons(commons)(allocation);
    return setThreshold(newThreshold)(evolved);
  };

// ============================================================================
// Commons Allocation (Curried)
// ============================================================================

/**
 * Allocate from commons: Commons → Distribution → Commons
 * Removes resources according to distribution
 */
export const allocateFromCommons = 
  (commons: CommonsType) =>
  (distribution: Distribution): CommonsType => {
    const updatedResources = { ...commons.resources };
    for (const [entityId, amount] of Object.entries(distribution.weights)) {
      const current = updatedResources[entityId] || 0;
      updatedResources[entityId] = Math.max(0, current - amount);
    }
    
    return {
      ...commons,
      resources: updatedResources,
    };
  };

/**
 * Distribute commons resources: Commons → Set Entity → Distribution
 * Distributes available resources equally among entities
 */
export const distributeResources = 
  (commons: CommonsType) =>
  (entities: Set<Entity>): Distribution => {
    const totalResources = getTotalResources(commons);
    const entityCount = entities.size;
    
    if (entityCount === 0) {
      return { weights: {}, total: 0 };
    }
    
    const perEntity = totalResources / entityCount;
    const weights: Record<string, Real> = {};
    for (const entity of entities) {
      weights[entity.id] = perEntity;
    }
    
    return normalize(weights);
  };

// ============================================================================
// HyperCollective Operations (Curried)
// ============================================================================

/**
 * Create hyper-collective from entities: String → Set Entity → HyperCollective
 */
export const createHyperFromEntities = 
  (id: string) =>
  (entities: Set<Entity>): HyperCollective => ({
    type: 'collective',
    members: new Set(Array.from(entities).map(e => ({ type: 'base' as const, entity: e }))),
  });

/**
 * Create hyper-collective from nested: String → Set HyperCollective → HyperCollective
 */
export const createHyperFromNested = 
  (id: string) =>
  (nested: Set<HyperCollective>): HyperCollective => ({
    type: 'collective',
    members: nested,
  });

/**
 * Get all entities from hyper: HyperCollective → Set Entity
 */
export const getHyperEntities = (hyper: HyperCollective): Set<Entity> => {
  const allEntities = new Set<Entity>();
  
  const extractEntities = (hc: HyperCollective): void => {
    if (hc.type === 'base') {
      allEntities.add(hc.entity);
    } else {
      for (const member of hc.members) {
        extractEntities(member);
      }
    }
  };
  
  extractEntities(hyper);
  return allEntities;
};

/**
 * Count members in hyper: HyperCollective → Number
 */
export const getHyperMemberCount = (hyper: HyperCollective): Real => {
  if (hyper.type === 'base') {
    return 1;
  } else {
    return hyper.members.size;
  }
};

// ============================================================================
// Commons Metrics (Curried)
// ============================================================================

/**
 * Check if meets threshold: Entity → Commons → Bool (λ-R spec helper)
 */
export const meetsThreshold = 
  (entity: Entity) =>
  (commons: CommonsType): boolean =>
    commons.condition(entity);

/**
 * Average per member: Commons → Real
 */
export const avgPerMember = (commons: CommonsType): Real => {
  const total = getTotalResources(commons);
  const count = memberCount(commons);
  return count > 0 ? total / count : 0;
};

/**
 * Resources per member: Commons → Real
 */
export const resourcesPerMember = (commons: CommonsType): Real => {
  const total = getTotalResources(commons);
  const count = memberCount(commons);
  return count > 0 ? total / count : 0;
};

// ============================================================================
// Commons Composition (Curried)
// ============================================================================

/**
 * Merge commons: Commons → Commons → Commons
 */
export const mergeCommons = 
  (c1: CommonsType) =>
  (c2: CommonsType): CommonsType => {
    const mergedMembers = new Set([...c1.members, ...c2.members]);
    const mergedResources = { ...c1.resources };
    
    for (const [key, value] of Object.entries(c2.resources)) {
      mergedResources[key] = (mergedResources[key] || 0) + value;
    }
    
    return {
      id: `${c1.id}-${c2.id}`,
      condition: (e: Entity) => c1.condition(e) || c2.condition(e),
      threshold: Math.max(c1.threshold, c2.threshold),
      members: mergedMembers,
      filters: [...c1.filters, ...c2.filters],
      limits: [...c1.limits, ...c2.limits],
      resources: mergedResources,
      metadata: { ...c1.metadata, ...c2.metadata },
    };
  };

/**
 * Split commons: Commons → Real → [Commons, Commons]
 * Splits by resource ratio
 */
export const splitCommons = 
  (commons: CommonsType) =>
  (ratio: Real): [CommonsType, CommonsType] => {
    const split1Resources: Record<string, Real> = {};
    const split2Resources: Record<string, Real> = {};
    
    for (const [key, value] of Object.entries(commons.resources)) {
      split1Resources[key] = value * ratio;
      split2Resources[key] = value * (1 - ratio);
    }
    
    const membersArray = Array.from(commons.members);
    const splitIndex = Math.floor(membersArray.length * ratio);
    const members1 = new Set(membersArray.slice(0, splitIndex));
    const members2 = new Set(membersArray.slice(splitIndex));
    
    return [
      {
        ...commons,
        id: `${commons.id}-1`,
        members: members1,
        resources: split1Resources,
      },
      {
        ...commons,
        id: `${commons.id}-2`,
        members: members2,
        resources: split2Resources,
      },
    ];
  };

// ============================================================================
// Export curried operations
// ============================================================================

export const curriedCommons = {
  createCommons,
  createCommonsWithMembers,
  getMembers,
  getThreshold,
  getCondition,
  getResources,
  getTotalResources,
  isMember,
  memberCount,
  meetsThreshold,
  addMember,
  removeMember,
  setThreshold,
  addResource,
  removeResource,
  setResource,
  evolveCommons,
  evolveWithThreshold,
  allocateFromCommons,
  distributeResources,
  createHyperFromEntities,
  createHyperFromNested,
  getHyperEntities,
  getHyperMemberCount,
  avgPerMember,
  resourcesPerMember,
  mergeCommons,
  splitCommons,
};

