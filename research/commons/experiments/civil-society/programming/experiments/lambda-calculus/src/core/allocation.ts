/**
 * Lambda Calculus Implementation: Capacity Allocation System
 * 
 * This module implements:
 * - Iterative allocation algorithm
 * - Provider-recipient matching with filters and limits
 * - Support for MRS, SCMRS, and SCRMRS share types
 */

import type {
  Entity,
  Real,
  Distribution,
  Allocation,
  Provider,
  Recipient,
  RecognitionMatrix,
  ShareType,
  Collective,
} from './types';
import { getAllocation, setAllocation, getProb } from './types';
import { min, entitiesToIds } from './primitives';
import { mrs } from './recognition';
import { scmrs, scrmrs, formCollective } from './collective';
import type { FilterFunction } from './filters';
import type { LimitFunction } from './limits';
import { applyFilters } from './filters';
import { applyLimits } from './limits';

// ============================================================================
// Allocation Configuration
// ============================================================================

export interface AllocationConfig {
  maxIterations: number;
  convergenceThreshold: Real;
}

export const DEFAULT_ALLOCATION_CONFIG: AllocationConfig = {
  maxIterations: 100,
  convergenceThreshold: 0.001,
};

// ============================================================================
// Core Allocation Algorithm
// ============================================================================

/**
 * Allocate capacity from providers to recipients
 * Uses iterative algorithm until convergence
 */
export function allocateCapacity(
  providers: Provider[],
  recipients: Recipient[],
  matrix: RecognitionMatrix,
  universe: Set<string>,
  shareType: ShareType,
  recipientFilterFns: Map<string, FilterFunction[]>,
  providerLimitFns: Map<string, LimitFunction[]>,
  config: AllocationConfig = DEFAULT_ALLOCATION_CONFIG
): Allocation {
  // Initialize needs and capacities
  const needs = new Map<string, Real>();
  const capacities = new Map<string, Real>();

  for (const recipient of recipients) {
    needs.set(recipient.entity.id, recipient.need);
  }

  for (const provider of providers) {
    capacities.set(provider.entity.id, provider.capacity);
  }

  // Initialize allocation
  let allocation: Allocation = { allocations: {} };

  // Iterative allocation
  for (let iteration = 0; iteration < config.maxIterations; iteration++) {
    const newAllocation = allocationStep(
      providers,
      recipients,
      matrix,
      universe,
      shareType,
      needs,
      capacities,
      recipientFilterFns,
      providerLimitFns
    );

    // Check convergence
    const totalChange = calculateAllocationChange(allocation, newAllocation);

    allocation = newAllocation;

    if (totalChange < config.convergenceThreshold) {
      break;
    }

    // Update needs and capacities
    updateNeedsAndCapacities(allocation, providers, recipients, needs, capacities);
  }

  return allocation;
}

/**
 * Single step of allocation algorithm
 */
function allocationStep(
  providers: Provider[],
  recipients: Recipient[],
  matrix: RecognitionMatrix,
  universe: Set<string>,
  shareType: ShareType,
  needs: Map<string, Real>,
  capacities: Map<string, Real>,
  recipientFilterFns: Map<string, FilterFunction[]>,
  providerLimitFns: Map<string, LimitFunction[]>
): Allocation {
  const allocation: Allocation = { allocations: {} };

  for (const provider of providers) {
    const providerId = provider.entity.id;
    const providerCapacity = capacities.get(providerId) || 0;

    if (providerCapacity <= 0) continue;

    // Filter eligible recipients
    const eligibleRecipients = filterEligibleRecipients(
      provider,
      recipients,
      recipientFilterFns.get(providerId) || []
    );

    if (eligibleRecipients.length === 0) continue;

    // Calculate share distribution
    const shareDist = calculateShare(
      provider,
      eligibleRecipients,
      matrix,
      universe,
      shareType
    );

    // Apply provider limits
    const limitedDist = applyProviderLimits(
      shareDist,
      providerLimitFns.get(providerId) || []
    );

    // Allocate to recipients
    for (const recipient of eligibleRecipients) {
      const recipientId = recipient.entity.id;
      const recipientNeed = needs.get(recipientId) || 0;

      if (recipientNeed <= 0) continue;

      const share = getProb(limitedDist, recipientId);
      const proposedAllocation = providerCapacity * share;
      const actualAllocation = min(proposedAllocation, recipientNeed);

      if (actualAllocation > 0) {
        allocation.allocations[providerId] = allocation.allocations[providerId] || {};
        allocation.allocations[providerId][recipientId] = actualAllocation;
      }
    }
  }

  return allocation;
}

/**
 * Filter recipients based on provider's filters
 */
function filterEligibleRecipients(
  provider: Provider,
  recipients: Recipient[],
  filterFns: FilterFunction[]
): Recipient[] {
  const recipientEntities = new Set(recipients.map((r) => r.entity));
  const filtered = applyFilters(filterFns, recipientEntities);
  
  return recipients.filter((r) => filtered.has(r.entity));
}

/**
 * Calculate share distribution based on share type
 */
function calculateShare(
  provider: Provider,
  recipients: Recipient[],
  matrix: RecognitionMatrix,
  universe: Set<string>,
  shareType: ShareType
): Distribution {
  const providerId = provider.entity.id;

  switch (shareType) {
    case 'MRS':
      return mrs(matrix, providerId, universe);

    case 'SCMRS': {
      // Form temporary collective from recipients
      const recipientIds = new Set(recipients.map((r) => r.entity.id));
      const collective: Collective = {
        id: 'temp',
        members: recipientIds,
        filters: [],
        limits: [],
        shareType: 'SCMRS',
      };
      return scmrs(matrix, collective);
    }

    case 'SCRMRS': {
      // Form temporary collective from recipients
      const recipientIds = new Set(recipients.map((r) => r.entity.id));
      const collective: Collective = {
        id: 'temp',
        members: recipientIds,
        filters: [],
        limits: [],
        shareType: 'SCRMRS',
      };
      return scrmrs(matrix, collective, universe);
    }

    default:
      return { weights: {}, total: 0 };
  }
}

/**
 * Apply provider's limits to share distribution
 */
function applyProviderLimits(
  distribution: Distribution,
  limitFns: LimitFunction[]
): Distribution {
  return applyLimits(limitFns, distribution);
}

/**
 * Update needs and capacities based on allocations
 */
function updateNeedsAndCapacities(
  allocation: Allocation,
  providers: Provider[],
  recipients: Recipient[],
  needs: Map<string, Real>,
  capacities: Map<string, Real>
): void {
  // Reset needs and capacities
  for (const recipient of recipients) {
    needs.set(recipient.entity.id, recipient.need);
  }
  for (const provider of providers) {
    capacities.set(provider.entity.id, provider.capacity);
  }

  // Subtract allocations
  for (const [providerId, allocations] of Object.entries(allocation.allocations)) {
    let totalAllocated = 0;
    for (const [recipientId, amount] of Object.entries(allocations)) {
      totalAllocated += amount;
      const currentNeed = needs.get(recipientId) || 0;
      needs.set(recipientId, Math.max(0, currentNeed - amount));
    }
    const currentCapacity = capacities.get(providerId) || 0;
    capacities.set(providerId, Math.max(0, currentCapacity - totalAllocated));
  }
}

/**
 * Calculate total change between two allocations
 */
function calculateAllocationChange(
  allocation1: Allocation,
  allocation2: Allocation
): Real {
  let totalChange = 0;

  // Check all providers in both allocations
  const allProviders = new Set([
    ...Object.keys(allocation1.allocations),
    ...Object.keys(allocation2.allocations),
  ]);

  for (const providerId of allProviders) {
    const alloc1 = allocation1.allocations[providerId] || {};
    const alloc2 = allocation2.allocations[providerId] || {};

    const allRecipients = new Set([...Object.keys(alloc1), ...Object.keys(alloc2)]);

    for (const recipientId of allRecipients) {
      const amount1 = alloc1[recipientId] || 0;
      const amount2 = alloc2[recipientId] || 0;
      totalChange += Math.abs(amount2 - amount1);
    }
  }

  return totalChange;
}

// ============================================================================
// Allocation Queries
// ============================================================================

/**
 * Get total allocated by a provider
 */
export function getTotalAllocated(allocation: Allocation, providerId: string): Real {
  const providerAlloc = allocation.allocations[providerId];
  if (!providerAlloc) return 0;

  return Object.values(providerAlloc).reduce((sum, amount) => sum + amount, 0);
}

/**
 * Get total received by a recipient
 */
export function getTotalReceived(allocation: Allocation, recipientId: string): Real {
  let total = 0;
  for (const providerAlloc of Object.values(allocation.allocations)) {
    total += providerAlloc[recipientId] || 0;
  }
  return total;
}

/**
 * Get all providers allocating to a recipient
 */
export function getProvidersFor(allocation: Allocation, recipientId: string): string[] {
  const providers: string[] = [];
  for (const [providerId, providerAlloc] of Object.entries(allocation.allocations)) {
    if (providerAlloc[recipientId] && providerAlloc[recipientId] > 0) {
      providers.push(providerId);
    }
  }
  return providers;
}

/**
 * Get all recipients receiving from a provider
 */
export function getRecipientsFor(allocation: Allocation, providerId: string): string[] {
  const providerAlloc = allocation.allocations[providerId];
  if (!providerAlloc) return [];

  return Object.keys(providerAlloc).filter((recipientId) => providerAlloc[recipientId] > 0);
}

// ============================================================================
// Allocation Metrics
// ============================================================================

/**
 * Calculate allocation efficiency (proportion of capacity allocated)
 */
export function allocationEfficiency(
  allocation: Allocation,
  providers: Provider[]
): Real {
  const totalCapacity = providers.reduce((sum, p) => sum + p.capacity, 0);
  if (totalCapacity === 0) return 0;

  let totalAllocated = 0;
  for (const providerId of Object.keys(allocation.allocations)) {
    totalAllocated += getTotalAllocated(allocation, providerId);
  }

  return totalAllocated / totalCapacity;
}

/**
 * Calculate satisfaction rate (proportion of needs met)
 */
export function satisfactionRate(
  allocation: Allocation,
  recipients: Recipient[]
): Real {
  const totalNeed = recipients.reduce((sum, r) => sum + r.need, 0);
  if (totalNeed === 0) return 1;

  let totalReceived = 0;
  for (const recipient of recipients) {
    totalReceived += getTotalReceived(allocation, recipient.entity.id);
  }

  return min(1, totalReceived / totalNeed);
}

/**
 * Calculate allocation fairness (Gini coefficient)
 */
export function allocationFairness(
  allocation: Allocation,
  recipients: Recipient[]
): Real {
  const amounts: Real[] = recipients.map((r) =>
    getTotalReceived(allocation, r.entity.id)
  );

  if (amounts.length === 0) return 0;
  if (amounts.length === 1) return 0;

  amounts.sort((a, b) => a - b);

  let sum = 0;
  for (let i = 0; i < amounts.length; i++) {
    sum += (2 * (i + 1) - amounts.length - 1) * amounts[i];
  }

  const n = amounts.length;
  const mean = amounts.reduce((a, b) => a + b, 0) / n;

  if (mean === 0) return 0;

  return sum / (n * n * mean);
}

// ============================================================================
// Allocation Transformations
// ============================================================================

/**
 * Merge multiple allocations
 */
export function mergeAllocations(allocations: Allocation[]): Allocation {
  const merged: Allocation = { allocations: {} };

  for (const allocation of allocations) {
    for (const [providerId, providerAlloc] of Object.entries(allocation.allocations)) {
      merged.allocations[providerId] = merged.allocations[providerId] || {};
      for (const [recipientId, amount] of Object.entries(providerAlloc)) {
        merged.allocations[providerId][recipientId] =
          (merged.allocations[providerId][recipientId] || 0) + amount;
      }
    }
  }

  return merged;
}

/**
 * Filter allocation by providers
 */
export function filterAllocationByProviders(
  allocation: Allocation,
  providerIds: Set<string>
): Allocation {
  const filtered: Allocation = { allocations: {} };

  for (const [providerId, providerAlloc] of Object.entries(allocation.allocations)) {
    if (providerIds.has(providerId)) {
      filtered.allocations[providerId] = providerAlloc;
    }
  }

  return filtered;
}

/**
 * Filter allocation by recipients
 */
export function filterAllocationByRecipients(
  allocation: Allocation,
  recipientIds: Set<string>
): Allocation {
  const filtered: Allocation = { allocations: {} };

  for (const [providerId, providerAlloc] of Object.entries(allocation.allocations)) {
    filtered.allocations[providerId] = {};
    for (const [recipientId, amount] of Object.entries(providerAlloc)) {
      if (recipientIds.has(recipientId)) {
        filtered.allocations[providerId][recipientId] = amount;
      }
    }
  }

  return filtered;
}

/**
 * Scale allocation by a factor
 */
export function scaleAllocation(allocation: Allocation, scale: Real): Allocation {
  const scaled: Allocation = { allocations: {} };

  for (const [providerId, providerAlloc] of Object.entries(allocation.allocations)) {
    scaled.allocations[providerId] = {};
    for (const [recipientId, amount] of Object.entries(providerAlloc)) {
      scaled.allocations[providerId][recipientId] = amount * scale;
    }
  }

  return scaled;
}

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Create an empty allocation
 */
export function emptyAllocation(): Allocation {
  return { allocations: {} };
}

/**
 * Check if allocation is empty
 */
export function isAllocationEmpty(allocation: Allocation): boolean {
  return Object.keys(allocation.allocations).length === 0;
}

/**
 * Get total amount in allocation
 */
export function getTotalAllocation(allocation: Allocation): Real {
  let total = 0;
  for (const providerAlloc of Object.values(allocation.allocations)) {
    for (const amount of Object.values(providerAlloc)) {
      total += amount;
    }
  }
  return total;
}

/**
 * Create allocation matrix as 2D array for visualization
 */
export function allocationToMatrix(
  allocation: Allocation,
  providerIds: string[],
  recipientIds: string[]
): Real[][] {
  const matrix: Real[][] = [];

  for (const providerId of providerIds) {
    const row: Real[] = [];
    for (const recipientId of recipientIds) {
      row.push(getAllocation(allocation, providerId, recipientId));
    }
    matrix.push(row);
  }

  return matrix;
}

