/**
 * Dampening System for Oscillation Prevention
 * 
 * When allocations repeatedly overshoot needs, damping reduces the
 * allocation rate to achieve convergence.
 */

export interface DampingState {
  overAllocationHistory: number[];
  dampingFactor: number;
}

export type MultiTypeDamping = Record<string, DampingState>;

/**
 * Dampening System
 * 
 * Prevents oscillation in allocation by tracking over-allocation history
 * and adjusting future allocations accordingly.
 */
export class DampeningSystem {
  private static readonly HISTORY_WINDOW = 5;
  private static readonly SENSITIVITY = 0.5;
  private static readonly MIN_DAMPING = 0.1;
  
  /**
   * Calculate damping factor based on over-allocation history
   * 
   * Formula: damping_factor = max(0.1, 1 - (avg_recent_overshoot × 0.5))
   */
  static calculateDampingFactor(overAllocationHistory: number[]): number {
    if (overAllocationHistory.length === 0) return 1.0;
    
    const recentHistory = overAllocationHistory.slice(-this.HISTORY_WINDOW);
    const avgOvershoot = recentHistory.reduce((sum, val) => sum + val, 0) / recentHistory.length;
    
    const dampingFactor = Math.max(
      this.MIN_DAMPING,
      1 - (avgOvershoot * this.SENSITIVITY)
    );
    
    return dampingFactor;
  }
  
  /**
   * Update damping state with new allocation results
   */
  static updateDampingState(
    state: DampingState,
    allocated: number,
    need: number
  ): DampingState {
    const overshoot = need > 0 ? Math.max(0, (allocated - need) / need) : 0;
    
    const newHistory = [...state.overAllocationHistory, overshoot];
    if (newHistory.length > this.HISTORY_WINDOW) {
      newHistory.shift();
    }
    
    return {
      overAllocationHistory: newHistory,
      dampingFactor: this.calculateDampingFactor(newHistory)
    };
  }
  
  /**
   * Apply damping to allocation amounts
   */
  static applyDamping(rawAllocation: number, dampingFactor: number): number {
    return rawAllocation * dampingFactor;
  }
  
  /**
   * Create initial damping state
   */
  static createInitialState(): DampingState {
    return {
      overAllocationHistory: [],
      dampingFactor: 1.0
    };
  }
}

