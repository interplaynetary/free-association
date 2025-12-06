/**
 * Elegant Lambda Calculus: Collective System
 * 
 * Fully curried collective implementation following lambda calculus principles:
 * - SCMRS = (S, C, Θ, Λ) → Entity → Set Entity → Dist Entity
 * - Full currying for composition
 * - Monadic state management
 */

import type { 
  RecognitionMatrix, 
  Distribution,
} from '../core/types';
import type {
  Entity,
  Real,
  Collective,
  Provider,
  Recipient,
  SimpleFilter,
  Limit,
  ShareType,
} from './types';
import { normalize } from '../core/types';
import { pipe } from './combinators';
import { mrs as mrsElegant } from './recognition';

// ============================================================================
// Collective Types (Pure Functional)
// ============================================================================

/**
 * Filter function: Set Entity → Set Entity (matches λ-R spec)
 */
export type FilterFn = SimpleFilter<Entity>;

/**
 * Limit function: Distribution → Distribution (matches λ-R spec)
 */
export type LimitFn = Limit;

/**
 * Collective Formation Function
 * SCMRS: RecognitionMatrix → Set Entity → Set Entity → FilterFn[] → LimitFn[] → Entity → Dist Entity
 */
export type CollectiveFormation = 
  (matrix: RecognitionMatrix) =>
  (providers: Set<Entity>) =>
  (recipients: Set<Entity>) =>
  (filters: FilterFn[]) =>
  (limits: LimitFn[]) =>
  (entity: Entity) =>
  Distribution;

// ============================================================================
// SCMRS: Selective Collective MRS (Fully Curried)
// ============================================================================

/**
 * SCMRS following spec:
 * SCMRS(S,C,Θ,Λ)(e) = Λ(MRS_S(C'(e)))
 * where C'(e) = Θ(C ∩ providers(e))
 */
export const scmrs: CollectiveFormation =
  (matrix: RecognitionMatrix) =>
  (providers: Set<Entity>) =>
  (recipients: Set<Entity>) =>
  (filters: FilterFn[]) =>
  (limits: LimitFn[]) =>
  (entity: Entity): Distribution => {
    // C'(e) = Θ(C ∩ providers(e))
    // Filter the collective for this entity
    let filteredProviders = new Set(providers);
    for (const filter of filters) {
      filteredProviders = filter(filteredProviders);
    }
    
    // MRS_S(C'(e))
    const mrsForEntity = mrsElegant(matrix)(entity.id);
    const providerIds = new Set(Array.from(filteredProviders).map(e => e.id));
    const dist = mrsForEntity(providerIds);
    
    // Λ(MRS_S(C'(e)))
    let limitedDist = dist;
    for (const limit of limits) {
      limitedDist = limit(limitedDist);
    }
    
    return limitedDist;
  };

// ============================================================================
// SCRMRS: Selective Collective Relative MRS (Fully Curried)
// ============================================================================

/**
 * SCRMRS following spec:
 * SCRMRS(S,C,Θ,Λ)(e) = Λ(normalize(MRS_S(C'(e)) / Σ_{c∈C'} TMR(c)))
 */
export const scrmrs: CollectiveFormation =
  (matrix: RecognitionMatrix) =>
  (providers: Set<Entity>) =>
  (recipients: Set<Entity>) =>
  (filters: FilterFn[]) =>
  (limits: LimitFn[]) =>
  (entity: Entity): Distribution => {
    // C'(e) = Θ(C ∩ providers(e))
    let filteredProviders = new Set(providers);
    for (const filter of filters) {
      filteredProviders = filter(filteredProviders);
    }
    
    // Calculate MRS
    const mrsForEntity = mrsElegant(matrix)(entity.id);
    const providerIds = new Set(Array.from(filteredProviders).map(e => e.id));
    const mrsDist = mrsForEntity(providerIds);
    
    // Calculate TMR for each provider
    const relativized: Record<string, Real> = {};
    for (const provider of filteredProviders) {
      const providerId = provider.id;
      const mrsValue = mrsDist.weights[providerId] || 0;
      
      // Calculate TMR(c) = Σ_{e∈S} R(e,c)
      let tmr = 0;
      for (const id of providerIds) {
        tmr += matrix.matrix[id]?.[providerId] || 0;
      }
      
      // Avoid division by zero
      if (tmr > 0) {
        relativized[providerId] = mrsValue / tmr;
      } else {
        relativized[providerId] = 0;
      }
    }
    
    // Normalize and apply limits
    let dist = normalize(relativized);
    for (const limit of limits) {
      dist = limit(dist);
    }
    
    return dist;
  };

// ============================================================================
// Collective Formation Helpers (Curried)
// ============================================================================

/**
 * Create collective: String → Set Entity → FilterFn[] → LimitFn[] → ShareType → Collective
 * Matches λ-R spec: form_collective
 */
export const createCollective = 
  (id: string) =>
  (members: Set<Entity>) =>
  (filters: FilterFn[]) =>
  (limits: LimitFn[]) =>
  (shareType: ShareType = 'SCMRS'): Collective => ({
    id,
    members,
    filters,
    limits,
    shareType,
  });

/**
 * Calculate collective distribution: RecognitionMatrix → Collective → Entity → Distribution
 * Uses the collective's shareType to determine which calculation to use
 */
export const calculateCollectiveDistribution = 
  (matrix: RecognitionMatrix) =>
  (collective: Collective) =>
  (entity: Entity): Distribution => {
    const members = collective.members;
    const filters = collective.filters;
    const limits = collective.limits;
    
    // Use specified share type
    if (collective.shareType === 'SCMRS') {
      return scmrs(matrix)(members)(members)(filters)(limits)(entity);
    } else if (collective.shareType === 'SCRMRS') {
      return scrmrs(matrix)(members)(members)(filters)(limits)(entity);
    } else {
      // Default to SCMRS
      return scmrs(matrix)(members)(members)(filters)(limits)(entity);
    }
  };

/**
 * Calculate collective MRS: RecognitionMatrix → Collective → Entity → Distribution
 */
export const calculateCollectiveMRS = calculateCollectiveDistribution;

/**
 * Calculate collective RMRS: RecognitionMatrix → Collective → Entity → Distribution
 */
export const calculateCollectiveRMRS = calculateCollectiveDistribution;

// ============================================================================
// Collective Queries (Curried)
// ============================================================================

/**
 * Get members: Collective → Set Entity (λ-R spec)
 */
export const getMembers = (collective: Collective): Set<Entity> =>
  collective.members;

/**
 * Is member: Collective → Entity → Bool (λ-R spec)
 */
export const isMember = 
  (collective: Collective) =>
  (entity: Entity): boolean =>
    collective.members.has(entity);

/**
 * Member count: Collective → Real
 */
export const memberCount = (collective: Collective): Real =>
  collective.members.size;

// ============================================================================
// Collective Transformations (Curried)
// ============================================================================

/**
 * Add member: Entity → Collective → Collective (λ-R spec)
 */
export const addMember = 
  (entity: Entity) =>
  (collective: Collective): Collective => {
    const newMembers = new Set(collective.members);
    newMembers.add(entity);
    return {
      ...collective,
      members: newMembers,
    };
  };

/**
 * Remove member: Entity → Collective → Collective (λ-R spec)
 */
export const removeMember = 
  (entity: Entity) =>
  (collective: Collective): Collective => {
    const newMembers = new Set(collective.members);
    newMembers.delete(entity);
    return {
      ...collective,
      members: newMembers,
    };
  };

// ============================================================================
// Collective Aggregation (Curried)
// ============================================================================

/**
 * Total collective value: Collective → (Entity → Real) → Real
 */
export const totalValue = 
  (collective: Collective) =>
  (valueFn: (e: Entity) => Real): Real =>
    Array.from(collective.members).reduce((sum, entity) => sum + valueFn(entity), 0);

/**
 * Average collective value: Collective → (Entity → Real) → Real
 */
export const avgValue = 
  (collective: Collective) =>
  (valueFn: (e: Entity) => Real): Real => {
    const total = totalValue(collective)(valueFn);
    const count = collective.members.size;
    return count > 0 ? total / count : 0;
  };

// ============================================================================
// Export curried operations
// ============================================================================

export const curriedCollective = {
  scmrs,
  scrmrs,
  createCollective,
  calculateCollectiveMRS,
  calculateCollectiveRMRS,
  getMembers,
  isMember,
  memberCount,
  addMember,
  removeMember,
  totalValue,
  avgValue,
};

