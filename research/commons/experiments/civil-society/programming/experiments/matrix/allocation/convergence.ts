/**
 * Convergence Tracking System
 * 
 * Tracks allocation progress and convergence toward equilibrium.
 */

import type { NeedSlot, AvailabilitySlot, SlotAllocationRecord } from '../slots/schemas.js';

export interface ConvergenceMetrics {
  totalNeed: number;
  totalCapacity: number;
  totalAllocated: number;
  satisfactionRate: number;
  allocationEfficiency: number;
  changeFromPrevious?: number;
}

/**
 * Convergence Tracker
 * 
 * Monitors allocation progress and determines when equilibrium is reached.
 */
export class ConvergenceTracker {
  /**
   * Calculate convergence metrics
   */
  static calculateMetrics(
    needSlots: NeedSlot[],
    availabilitySlots: AvailabilitySlot[],
    allocations: SlotAllocationRecord[]
  ): ConvergenceMetrics {
    const totalNeed = needSlots.reduce((sum, slot) => sum + slot.quantity, 0);
    const totalCapacity = availabilitySlots.reduce((sum, slot) => sum + slot.quantity, 0);
    const totalAllocated = allocations.reduce((sum, alloc) => sum + alloc.allocatedQuantity, 0);
    
    const satisfactionRate = totalNeed > 0 ? totalAllocated / totalNeed : 1.0;
    const allocationEfficiency = totalCapacity > 0 ? totalAllocated / totalCapacity : 0;
    
    return {
      totalNeed,
      totalCapacity,
      totalAllocated,
      satisfactionRate,
      allocationEfficiency
    };
  }
  
  /**
   * Check if allocation has converged
   */
  static hasConverged(
    currentMetrics: ConvergenceMetrics,
    previousMetrics?: ConvergenceMetrics,
    threshold: number = 0.01
  ): boolean {
    if (!previousMetrics) return false;
    
    const change = Math.abs(
      currentMetrics.satisfactionRate - previousMetrics.satisfactionRate
    );
    
    return change < threshold;
  }
  
  /**
   * Calculate change from previous iteration
   */
  static calculateChange(
    currentMetrics: ConvergenceMetrics,
    previousMetrics?: ConvergenceMetrics
  ): number {
    if (!previousMetrics) return 0;
    
    return currentMetrics.satisfactionRate - previousMetrics.satisfactionRate;
  }
}

