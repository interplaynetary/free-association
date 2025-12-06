/**
 * Collective Operations for Free Association Protocol
 * 
 * Implements collective-level computations:
 * - SCMRS (Synthetic-Collective-MR-Shares): Weighted collective allocation
 * - SCRMRS (Equal-voice version): Democratic collective allocation
 * - MRD (Mutual-Recognition-Density): Integration metric
 * - Membership determination
 */

import { Sparse, type SparseMatrix } from '../sparse-matrix.js';
import type { MatrixResult } from './matrix-operations.js';

/**
 * Collective Computer
 * 
 * Computes collective-level metrics and allocations.
 */
export class CollectiveComputer {
  constructor(
    private MR: SparseMatrix,
    private n: number
  ) {}
  
  /**
   * Compute Mutual Recognition within Collective (m_C)
   * 
   * Formula: (m_C)_i = Σ_{j∈C} MR_ij
   * 
   * For each participant, sum their mutual recognition with collective members.
   */
  computeRecognitionWithinCollective(collectiveIndices: number[]): number[] {
    const m_C: number[] = Array(this.n).fill(0);
    const collectiveSet = new Set(collectiveIndices);
    
    for (const [i, row] of this.MR.entries()) {
      let sum = 0;
      for (const [j, value] of row.entries()) {
        if (collectiveSet.has(j)) {
          sum += value;
        }
      }
      m_C[i] = sum;
    }
    
    return m_C;
  }
  
  /**
   * Compute Total Pool within Collective (T_C)
   * 
   * Formula: T_C = Σ_{i∈C} Σ_{j∈C} MR_ij
   */
  computeTotalPool(collectiveIndices: number[]): number {
    const collectiveSet = new Set(collectiveIndices);
    let T_C = 0;
    
    for (const i of collectiveIndices) {
      const row = this.MR.get(i);
      if (!row) continue;
      
      for (const [j, value] of row.entries()) {
        if (collectiveSet.has(j)) {
          T_C += value;
        }
      }
    }
    
    return T_C;
  }
  
  /**
   * Compute SCMRS (Weighted version)
   * 
   * Formula: s_i = (m_C)_i / T_C
   * 
   * Participants with stronger relationships to collective get higher shares.
   */
  computeSCMRS_weighted(collectiveIndices: number[]): number[] {
    const m_C = this.computeRecognitionWithinCollective(collectiveIndices);
    const T_C = this.computeTotalPool(collectiveIndices);
    const s: number[] = Array(this.n).fill(0);
    
    if (T_C === 0) return s;
    
    for (const i of collectiveIndices) {
      s[i] = m_C[i] / T_C;
    }
    
    return s;
  }
  
  /**
   * Compute SCRMRS (Equal-voice version)
   * 
   * Formula: s_i = (1/|C|) * Σ_{j∈C} MRS_ji
   * 
   * Each collective member has equal voting weight.
   * Requires MRS matrix (not MR).
   */
  computeSCRMRS_equal(
    collectiveIndices: number[],
    MRS: SparseMatrix
  ): number[] {
    const s: number[] = Array(this.n).fill(0);
    const C_size = collectiveIndices.length;
    
    if (C_size === 0) return s;
    
    for (const j of collectiveIndices) {
      const row = MRS.get(j);
      if (!row) continue;
      
      for (const [i, value] of row.entries()) {
        s[i] += value / C_size;
      }
    }
    
    return s;
  }
  
  /**
   * Compute Average MR in Collective
   * 
   * Formula: m̄_C = T_C / |C|
   */
  computeAverageMR(collectiveIndices: number[]): number {
    const T_C = this.computeTotalPool(collectiveIndices);
    const C_size = collectiveIndices.length;
    
    if (C_size === 0) return 0;
    
    return T_C / C_size;
  }
  
  /**
   * Compute Mutual-Recognition-Density (MRD)
   * 
   * Formula: MRD_C(i) = (|C| * (m_C)_i) / T_C
   * 
   * Measures how well integrated participant i is with collective C.
   * - MRD ≈ 1: Average integration
   * - MRD > 1: Above-average integration
   * - MRD < 1: Below-average integration
   */
  computeMRD(
    collectiveIndices: number[],
    participantIndex: number
  ): number {
    const m_C = this.computeRecognitionWithinCollective(collectiveIndices);
    const T_C = this.computeTotalPool(collectiveIndices);
    const C_size = collectiveIndices.length;
    
    if (T_C === 0 || C_size === 0) return 0;
    
    return (C_size * m_C[participantIndex]) / T_C;
  }
  
  /**
   * Compute MRD for all participants
   */
  computeAllMRD(collectiveIndices: number[]): number[] {
    const mrd: number[] = Array(this.n).fill(0);
    
    for (let i = 0; i < this.n; i++) {
      mrd[i] = this.computeMRD(collectiveIndices, i);
    }
    
    return mrd;
  }
  
  /**
   * Determine membership based on MRD threshold
   * 
   * @param collectiveIndices - Current collective members
   * @param threshold - MRD threshold (typically 0.5)
   * @param model - "collective" (rising bar) or "commons" (stable bar)
   */
  determineMembership(
    collectiveIndices: number[],
    threshold: number = 0.5,
    model: 'collective' | 'commons' = 'collective'
  ): number[] {
    if (model === 'collective') {
      // Collective model: rising bar
      // Only existing members can remain if they meet threshold
      const mrd = this.computeAllMRD(collectiveIndices);
      return collectiveIndices.filter(i => mrd[i] >= threshold);
    } else {
      // Commons model: stable bar
      // Anyone meeting threshold can join
      const allIndices = Array.from({ length: this.n }, (_, i) => i);
      const mrd = this.computeAllMRD(allIndices);
      return allIndices.filter(i => mrd[i] >= threshold);
    }
  }
}

/**
 * Create collective computer from matrix result
 */
export function createCollectiveComputer(
  mrResult: MatrixResult,
  n: number
): CollectiveComputer {
  return new CollectiveComputer(mrResult.getMatrix(), n);
}

