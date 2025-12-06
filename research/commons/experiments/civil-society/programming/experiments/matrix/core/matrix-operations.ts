/**
 * Core Matrix Operations for Free Association Protocol
 * 
 * Pure mathematical operations on sparse matrices:
 * - RS (Recognition-Shares): Row-normalized recognition
 * - MR (Mutual-Recognition): Element-wise min of reciprocal recognition
 * - MRS (Mutual-Recognition-Shares): Row-normalized mutual recognition
 * - Total MR: Sum of mutual recognition per participant
 * 
 * All operations use sparse matrix optimization for O(e) complexity
 * where e = number of edges (non-zero recognitions).
 */

import { Sparse, type SparseMatrix } from '../sparse-matrix.js';

/**
 * Free Association Matrix Computer
 * 
 * Fluent interface for matrix computations:
 * ```typescript
 * const mrs = matrices
 *   .setRecognition(0, 1, 0.6)
 *   .setRecognition(0, 2, 0.4)
 *   .computeRS()
 *   .computeMR()
 *   .computeMRS();
 * ```
 */
export class MatrixComputer {
  private n: number;
  private R: SparseMatrix;
  
  constructor(maxParticipants: number) {
    this.n = maxParticipants;
    this.R = Sparse.create();
  }
  
  /**
   * Set recognition (fluent interface)
   */
  setRecognition(i: number, j: number, value: number): this {
    if (i < 0 || i >= this.n || j < 0 || j >= this.n) {
      throw new Error(`Index out of bounds: (${i}, ${j})`);
    }
    if (value < 0 || value > 1) {
      throw new Error(`Recognition value must be in [0, 1]: ${value}`);
    }
    Sparse.set(this.R, i, j, value);
    return this;
  }
  
  /**
   * Get recognition value
   */
  getRecognition(i: number, j: number): number {
    return Sparse.get(this.R, i, j);
  }
  
  /**
   * Get the underlying sparse matrix (for advanced use)
   */
  getMatrix(): SparseMatrix {
    return this.R;
  }
  
  /**
   * Validate budget constraint: each row sums to 1 (Axiom 1)
   */
  validateBudgetConstraint(tolerance: number = 0.0001): boolean {
    for (const [i, row] of this.R.entries()) {
      let sum = 0;
      for (const value of row.values()) {
        sum += value;
      }
      if (Math.abs(sum - 1.0) > tolerance) {
        return false;
      }
    }
    return true;
  }
  
  /**
   * Compute Recognition-Shares (RS)
   * 
   * Formula: RS_ij = R_ij / Σ_k R_ik
   * 
   * Row-normalize R so each row sums to 1.
   * Returns a new MatrixResult that can be further transformed.
   */
  computeRS(): MatrixResult {
    const RS = Sparse.rowNormalize(this.R);
    return new MatrixResult(RS, this.n);
  }
}

/**
 * Matrix Result - Represents computed matrix that can be further transformed
 * 
 * Fluent interface for chaining operations:
 * ```typescript
 * const mrs = matrices.computeRS()
 *   .computeMR()
 *   .computeMRS();
 * ```
 */
export class MatrixResult {
  constructor(
    private matrix: SparseMatrix,
    private n: number
  ) {}
  
  /**
   * Get the underlying sparse matrix
   */
  getMatrix(): SparseMatrix {
    return this.matrix;
  }
  
  /**
   * Get value at (i, j)
   */
  get(i: number, j: number): number {
    return Sparse.get(this.matrix, i, j);
  }
  
  /**
   * Convert to dense array (for display/testing)
   */
  toDense(): number[][] {
    return Sparse.toDense(this.matrix, this.n);
  }
  
  /**
   * Compute Mutual-Recognition from RS
   * 
   * Formula: MR_ij = min(RS_ij, RS_ji)
   */
  computeMR(): MatrixResult {
    const RS_T = Sparse.transpose(this.matrix);
    const MR = Sparse.elementWiseMin(this.matrix, RS_T);
    return new MatrixResult(MR, this.n);
  }
  
  /**
   * Compute Total MR vector
   * 
   * Formula: t_i = Σ_j MR_ij
   */
  computeTotalMR(): number[] {
    const t: number[] = Array(this.n).fill(0);
    
    for (const [i, row] of this.matrix.entries()) {
      let sum = 0;
      for (const value of row.values()) {
        sum += value;
      }
      t[i] = sum;
    }
    
    return t;
  }
  
  /**
   * Compute Mutual-Recognition-Shares from MR
   * 
   * Formula: MRS_ij = MR_ij / t_i
   * 
   * This assumes the current matrix IS MR (from computeMR()).
   * For proper usage: matrices.computeRS().computeMR().computeMRS()
   */
  computeMRS(): MatrixResult {
    const t = this.computeTotalMR();
    const MRS = Sparse.create();
    
    for (const [i, row] of this.matrix.entries()) {
      if (t[i] === 0) continue;
      
      for (const [j, value] of row.entries()) {
        Sparse.set(MRS, i, j, value / t[i]);
      }
    }
    
    return new MatrixResult(MRS, this.n);
  }
  
  /**
   * Verify symmetry property (for MR validation)
   */
  verifySymmetry(tolerance: number = 1e-10): boolean {
    for (const [i, row] of this.matrix.entries()) {
      for (const [j, value_ij] of row.entries()) {
        const value_ji = Sparse.get(this.matrix, j, i);
        if (Math.abs(value_ij - value_ji) > tolerance) {
          return false;
        }
      }
    }
    return true;
  }
  
  /**
   * Verify row normalization (each row sums to 1)
   */
  verifyRowNormalization(tolerance: number = 0.0001): boolean {
    for (const [i, row] of this.matrix.entries()) {
      let sum = 0;
      for (const value of row.values()) {
        sum += value;
      }
      if (Math.abs(sum - 1.0) > tolerance) {
        return false;
      }
    }
    return true;
  }
  
  /**
   * Extract column as array (useful for allocation)
   */
  getColumn(j: number): number[] {
    const column: number[] = Array(this.n).fill(0);
    
    for (const [i] of this.matrix.entries()) {
      column[i] = Sparse.get(this.matrix, i, j);
    }
    
    return column;
  }
  
  /**
   * Extract row as array
   */
  getRow(i: number): number[] {
    const row: number[] = Array(this.n).fill(0);
    const sparseRow = this.matrix.get(i);
    
    if (sparseRow) {
      for (const [j, value] of sparseRow.entries()) {
        row[j] = value;
      }
    }
    
    return row;
  }
}

/**
 * Convenience function for quick matrix computations
 */
export function computeMatrices(
  recognitionMatrix: number[][]
): {
  RS: MatrixResult;
  MR: MatrixResult;
  MRS: MatrixResult;
  totalMR: number[];
} {
  const n = recognitionMatrix.length;
  const computer = new MatrixComputer(n);
  
  // Set recognition matrix
  for (let i = 0; i < n; i++) {
    for (let j = 0; j < n; j++) {
      if (recognitionMatrix[i][j] > 0) {
        computer.setRecognition(i, j, recognitionMatrix[i][j]);
      }
    }
  }
  
  // Compute all matrices
  const RS = computer.computeRS();
  const MR = RS.computeMR();
  const MRS = MR.computeMRS();
  const totalMR = MR.computeTotalMR();
  
  return { RS, MR, MRS, totalMR };
}

