/**
 * Enhanced Allocation Engine
 * 
 * Integrates all advanced features:
 * - Multi-dimensional matching (type, time, location)
 * - Dampening for oscillation prevention
 * - Divisibility constraints
 * - Largest remainder method
 * - Convergence tracking
 * - Space-time indexing
 */

import type { NeedSlot, AvailabilitySlot, SlotAllocationRecord } from '../slots/schemas.js';
import type { MultiTypeDamping, DampingState } from './damping.js';
import type { ConvergenceMetrics } from './convergence.js';
import { DampeningSystem } from './damping.js';
import { DivisibilityConstraints, LargestRemainderMethod } from './divisibility.js';
import { ConvergenceTracker } from './convergence.js';
import { SpaceTimeIndex } from '../slots/indexing.js';
import { SlotMatching } from '../slots/matching.js';

/**
 * Allocation Engine Result
 */
export interface AllocationResult {
  allocations: SlotAllocationRecord[];
  metrics: ConvergenceMetrics;
  updatedDamping: MultiTypeDamping;
  converged: boolean;
}

/**
 * Enhanced Allocation Engine
 * 
 * Coordinates all allocation features to produce optimal, stable allocations.
 */
export class AllocationEngine {
  /**
   * Allocate capacity to needs using MRS-based distribution
   * 
   * @param needSlots - Recipient need slots
   * @param availabilitySlots - Provider availability slots
   * @param participantShares - MRS shares for each provider (participantId -> share)
   * @param dampingState - Current damping state (optional, for oscillation prevention)
   * @param previousMetrics - Metrics from previous iteration (optional, for convergence)
   * @returns Allocation result with records, metrics, updated damping, and convergence status
   */
  static allocate(
    needSlots: NeedSlot[],
    availabilitySlots: AvailabilitySlot[],
    participantShares: Map<string, number>,
    dampingState?: MultiTypeDamping,
    previousMetrics?: ConvergenceMetrics
  ): AllocationResult {
    const allocations: SlotAllocationRecord[] = [];
    const updatedDamping: MultiTypeDamping = { ...(dampingState || {}) };
    
    // Build space-time index for efficient matching (O(k) lookups)
    const index = new SpaceTimeIndex();
    for (const availSlot of availabilitySlots) {
      index.addSlot(availSlot);
    }
    
    // Track remaining capacity for each slot
    const remainingCapacity = new Map<string, number>();
    for (const availSlot of availabilitySlots) {
      remainingCapacity.set(availSlot.id, availSlot.quantity);
    }
    
    // Group needs by type for per-type damping
    const needsByType = new Map<string, NeedSlot[]>();
    for (const need of needSlots) {
      if (!needsByType.has(need.need_type_id)) {
        needsByType.set(need.need_type_id, []);
      }
      needsByType.get(need.need_type_id)!.push(need);
    }
    
    // Allocate for each type
    for (const [typeId, typeNeeds] of needsByType) {
      // Get damping state for this type
      const typeDamping = updatedDamping[typeId] || DampeningSystem.createInitialState();
      
      // Process each need in this type
      for (const need of typeNeeds) {
        // Find compatible providers using index (O(k) instead of O(N))
        const candidateProviderIds = index.findMatching(need);
        const compatibleSlots = availabilitySlots.filter(avail =>
          candidateProviderIds.has(avail.participantId) &&
          SlotMatching.slotsCompatible(need, avail) &&
          (remainingCapacity.get(avail.id) || 0) > 0
        );
        
        if (compatibleSlots.length === 0) continue;
        
        // Calculate distribution shares for compatible providers
        const providerShares: Record<string, number> = {};
        let totalShare = 0;
        
        for (const availSlot of compatibleSlots) {
          const share = participantShares.get(availSlot.participantId) || 0;
          providerShares[availSlot.id] = share;
          totalShare += share;
        }
        
        // Normalize shares
        if (totalShare > 0) {
          for (const slotId in providerShares) {
            providerShares[slotId] /= totalShare;
          }
        }
        
        // Calculate raw allocations (proportional to shares)
        const rawAllocations: Record<string, number> = {};
        for (const availSlot of compatibleSlots) {
          const share = providerShares[availSlot.id] || 0;
          const rawAmount = need.quantity * share;
          
          // Apply damping to prevent oscillation
          const dampedAmount = DampeningSystem.applyDamping(
            rawAmount,
            typeDamping.dampingFactor
          );
          
          // Apply divisibility constraints
          const availableCapacity = remainingCapacity.get(availSlot.id) || 0;
          const minAllocation = DivisibilityConstraints.getMinimumAllocation(
            availableCapacity,
            availSlot.divisibility
          );
          
          // Skip if below minimum
          if (dampedAmount < minAllocation) continue;
          
          // Round to natural units
          const roundedAmount = DivisibilityConstraints.roundToNaturalUnit(
            dampedAmount,
            availableCapacity,
            availSlot.divisibility
          );
          
          rawAllocations[availSlot.id] = Math.min(roundedAmount, availableCapacity);
        }
        
        // Apply largest remainder method for fair indivisible allocation
        const totalRawAllocation = Object.values(rawAllocations).reduce((sum, val) => sum + val, 0);
        const targetQuantity = Math.min(need.quantity, totalRawAllocation);
        
        const finalAllocations = LargestRemainderMethod.allocate(
          providerShares,
          Math.floor(targetQuantity)
        );
        
        // Create allocation records
        let allocatedToNeed = 0;
        for (const availSlot of compatibleSlots) {
          const allocatedQty = finalAllocations[availSlot.id] || 0;
          if (allocatedQty === 0) continue;
          
          allocations.push({
            needSlotId: need.id,
            availabilitySlotId: availSlot.id,
            providerId: availSlot.participantId,
            recipientId: need.participantId,
            allocatedQuantity: allocatedQty,
            timestamp: Date.now()
          });
          
          // Update remaining capacity
          const remaining = remainingCapacity.get(availSlot.id)! - allocatedQty;
          remainingCapacity.set(availSlot.id, remaining);
          allocatedToNeed += allocatedQty;
        }
        
        // Update damping state for this type
        updatedDamping[typeId] = DampeningSystem.updateDampingState(
          typeDamping,
          allocatedToNeed,
          need.quantity
        );
      }
    }
    
    // Calculate convergence metrics
    const metrics = ConvergenceTracker.calculateMetrics(
      needSlots,
      availabilitySlots,
      allocations
    );
    
    const converged = ConvergenceTracker.hasConverged(
      metrics,
      previousMetrics
    );
    
    return {
      allocations,
      metrics,
      updatedDamping,
      converged
    };
  }
}

