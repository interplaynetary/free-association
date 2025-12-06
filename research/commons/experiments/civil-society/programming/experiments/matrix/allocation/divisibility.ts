/**
 * Divisibility Constraints System
 * 
 * Prevents over-fragmentation by enforcing:
 * 1. Maximum natural divisions (e.g., can't divide a person)
 * 2. Minimum allocation percentages (e.g., don't allocate <10%)
 */

import type { Divisibility } from '../slots/schemas.js';

export class DivisibilityConstraints {
  /**
   * Check if an allocation satisfies divisibility constraints
   */
  static satisfiesConstraints(
    requestedAmount: number,
    totalAvailable: number,
    constraints?: Divisibility
  ): boolean {
    if (!constraints) return true;
    
    if (constraints.min_allocation_percentage) {
      const percentage = requestedAmount / totalAvailable;
      if (percentage < constraints.min_allocation_percentage) {
        return false;
      }
    }
    
    return true;
  }
  
  /**
   * Get minimum allowed allocation based on constraints
   */
  static getMinimumAllocation(
    totalAvailable: number,
    constraints?: Divisibility
  ): number {
    if (!constraints) return 0;
    
    if (constraints.min_allocation_percentage) {
      return totalAvailable * constraints.min_allocation_percentage;
    }
    
    return 0;
  }
  
  /**
   * Round allocation to satisfy natural division constraints
   */
  static roundToNaturalUnit(
    amount: number,
    totalAvailable: number,
    constraints?: Divisibility
  ): number {
    if (!constraints || !constraints.max_natural_div) {
      return amount;
    }
    
    const unitSize = totalAvailable / constraints.max_natural_div;
    return Math.round(amount / unitSize) * unitSize;
  }
}

/**
 * Largest Remainder Method for Fair Integer Allocation
 * 
 * When distributing indivisible items, uses the Largest Remainder Method
 * to fairly allocate remainders (standard method used in electoral systems).
 */
export class LargestRemainderMethod {
  /**
   * Allocate integer quantities fairly using largest remainder
   * 
   * @param shares - Proportional shares (sum to 1.0)
   * @param totalQuantity - Total integer quantity to allocate
   * @returns Integer allocations that sum exactly to totalQuantity
   */
  static allocate(
    shares: Record<string, number>,
    totalQuantity: number
  ): Record<string, number> {
    const result: Record<string, number> = {};
    const remainders: Array<{ id: string; remainder: number }> = [];
    
    let allocatedSoFar = 0;
    
    // Step 1: Allocate integer parts
    for (const [id, share] of Object.entries(shares)) {
      const exactAmount = share * totalQuantity;
      const integerPart = Math.floor(exactAmount);
      const remainder = exactAmount - integerPart;
      
      result[id] = integerPart;
      allocatedSoFar += integerPart;
      
      if (remainder > 0) {
        remainders.push({ id, remainder });
      }
    }
    
    // Step 2: Distribute remaining units to largest remainders
    const remaining = totalQuantity - allocatedSoFar;
    remainders.sort((a, b) => b.remainder - a.remainder);
    
    for (let i = 0; i < remaining && i < remainders.length; i++) {
      result[remainders[i].id]++;
    }
    
    return result;
  }
}

