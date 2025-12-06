/**
 * Sparse Matrix Implementation for Free Association Protocol
 * 
 * Memory-efficient sparse matrix storage using Map-based adjacency lists.
 * Only stores non-zero recognition values.
 * 
 * Memory complexity: O(e) where e = number of edges (recognition relationships)
 * Computation complexity: O(e) for most operations instead of O(n²)
 * 
 * For typical social networks where each person recognizes 3-50 others:
 * - Dense matrix: 8 MB for 1000 participants (1M floats)
 * - Sparse matrix: 80 KB for 1000 participants (10K floats)
 * - Efficiency gain: 100× less memory, 100× faster operations
 */

/**
 * Sparse Matrix Storage
 * 
 * Structure: Map<row, Map<col, value>>
 * Only stores non-zero values
 * 
 * Example:
 * ```
 * Dense: [[0, 0.6, 0.4], [0.3, 0, 0.7], [0.5, 0.5, 0]]
 * Sparse: {
 *   0: { 1: 0.6, 2: 0.4 },
 *   1: { 0: 0.3, 2: 0.7 },
 *   2: { 0: 0.5, 1: 0.5 }
 * }
 * Memory: 6 entries instead of 9 (33% savings)
 * For real networks: typically 95%+ savings
 * ```
 */
export type SparseMatrix = Map<number, Map<number, number>>;

/**
 * Sparse matrix utilities
 */
export class SparseMatrixOps {
  /**
   * Create empty sparse matrix
   */
  static create(): SparseMatrix {
    return new Map();
  }
  
  /**
   * Set value in sparse matrix
   * Automatically removes zero entries to maintain sparsity
   */
  static set(matrix: SparseMatrix, row: number, col: number, value: number): void {
    if (value === 0 || Math.abs(value) < 1e-10) {
      // Remove zero entries
      const rowMap = matrix.get(row);
      if (rowMap) {
        rowMap.delete(col);
        if (rowMap.size === 0) {
          matrix.delete(row);
        }
      }
      return;
    }
    
    // Set non-zero value
    if (!matrix.has(row)) {
      matrix.set(row, new Map());
    }
    matrix.get(row)!.set(col, value);
  }
  
  /**
   * Get value from sparse matrix
   * Returns 0 for unset entries
   */
  static get(matrix: SparseMatrix, row: number, col: number): number {
    return matrix.get(row)?.get(col) || 0;
  }
  
  /**
   * Get entire row (returns empty map if row doesn't exist)
   */
  static getRow(matrix: SparseMatrix, row: number): Map<number, number> {
    return matrix.get(row) || new Map();
  }
  
  /**
   * Compute row sum (only iterates non-zero entries)
   */
  static rowSum(matrix: SparseMatrix, row: number): number {
    const rowMap = matrix.get(row);
    if (!rowMap) return 0;
    
    let sum = 0;
    for (const value of rowMap.values()) {
      sum += value;
    }
    return sum;
  }
  
  /**
   * Count non-zero entries
   */
  static countNonZero(matrix: SparseMatrix): number {
    let count = 0;
    for (const row of matrix.values()) {
      count += row.size;
    }
    return count;
  }
  
  /**
   * Get memory usage statistics
   */
  static getStats(matrix: SparseMatrix, n: number): {
    entries: number;
    maxEntries: number;
    density: number;
    memoryBytes: number;
    memoryKB: string;
    sparsity: string;
  } {
    const entries = this.countNonZero(matrix);
    const maxEntries = n * n;
    const density = maxEntries > 0 ? entries / maxEntries : 0;
    const memoryBytes = entries * 8; // 8 bytes per float64
    
    return {
      entries,
      maxEntries,
      density,
      memoryBytes,
      memoryKB: `${(memoryBytes / 1024).toFixed(2)} KB`,
      sparsity: `${((1 - density) * 100).toFixed(2)}%`
    };
  }
  
  /**
   * Convert dense array to sparse matrix
   */
  static fromDense(dense: number[][]): SparseMatrix {
    const sparse = this.create();
    
    for (let i = 0; i < dense.length; i++) {
      for (let j = 0; j < dense[i].length; j++) {
        if (dense[i][j] !== 0) {
          this.set(sparse, i, j, dense[i][j]);
        }
      }
    }
    
    return sparse;
  }
  
  /**
   * Convert sparse matrix to dense array (for debugging)
   */
  static toDense(sparse: SparseMatrix, n: number): number[][] {
    const dense = Array.from({ length: n }, () => Array(n).fill(0));
    
    for (const [i, row] of sparse.entries()) {
      for (const [j, value] of row.entries()) {
        dense[i][j] = value;
      }
    }
    
    return dense;
  }
  
  /**
   * Clone sparse matrix
   */
  static clone(matrix: SparseMatrix): SparseMatrix {
    const cloned = this.create();
    
    for (const [i, row] of matrix.entries()) {
      cloned.set(i, new Map(row));
    }
    
    return cloned;
  }
  
  /**
   * Element-wise minimum of two sparse matrices
   * Used for computing MR = min(RS, RS^T)
   */
  static elementWiseMin(A: SparseMatrix, B: SparseMatrix): SparseMatrix {
    const result = this.create();
    
    // Iterate over A's entries
    for (const [i, rowA] of A.entries()) {
      for (const [j, valueA] of rowA.entries()) {
        // Get corresponding value from B
        const valueB = this.get(B, i, j);
        
        // Only store if both are non-zero
        if (valueB > 0) {
          const minValue = Math.min(valueA, valueB);
          this.set(result, i, j, minValue);
        }
      }
    }
    
    return result;
  }
  
  /**
   * Transpose sparse matrix
   * Efficiently swaps rows and columns
   */
  static transpose(matrix: SparseMatrix): SparseMatrix {
    const transposed = this.create();
    
    for (const [i, row] of matrix.entries()) {
      for (const [j, value] of row.entries()) {
        this.set(transposed, j, i, value);
      }
    }
    
    return transposed;
  }
  
  /**
   * Row-normalize sparse matrix
   * Each row sums to 1 (for RS computation)
   */
  static rowNormalize(matrix: SparseMatrix): SparseMatrix {
    const normalized = this.create();
    
    for (const [i, row] of matrix.entries()) {
      const sum = Array.from(row.values()).reduce((acc, val) => acc + val, 0);
      
      if (sum === 0) continue; // Skip zero rows
      
      for (const [j, value] of row.entries()) {
        this.set(normalized, i, j, value / sum);
      }
    }
    
    return normalized;
  }
  
  /**
   * Compute column sums (for collective operations)
   */
  static columnSums(matrix: SparseMatrix, n: number): number[] {
    const sums = Array(n).fill(0);
    
    for (const row of matrix.values()) {
      for (const [j, value] of row.entries()) {
        sums[j] += value;
      }
    }
    
    return sums;
  }
  
  /**
   * Extract submatrix for collective indices
   */
  static extractSubmatrix(matrix: SparseMatrix, indices: number[]): SparseMatrix {
    const indicesSet = new Set(indices);
    const submatrix = this.create();
    
    for (const [i, row] of matrix.entries()) {
      if (!indicesSet.has(i)) continue;
      
      for (const [j, value] of row.entries()) {
        if (indicesSet.has(j)) {
          this.set(submatrix, i, j, value);
        }
      }
    }
    
    return submatrix;
  }
}

/**
 * Performance monitoring
 */
export class SparseMatrixPerformance {
  private static operationTimes: Map<string, number[]> = new Map();
  
  static startTimer(): () => number {
    const start = performance.now();
    return () => performance.now() - start;
  }
  
  static recordOperation(operation: string, durationMs: number): void {
    if (!this.operationTimes.has(operation)) {
      this.operationTimes.set(operation, []);
    }
    this.operationTimes.get(operation)!.push(durationMs);
  }
  
  static getStats(operation: string): {
    count: number;
    totalMs: number;
    avgMs: number;
    minMs: number;
    maxMs: number;
  } | null {
    const times = this.operationTimes.get(operation);
    if (!times || times.length === 0) return null;
    
    return {
      count: times.length,
      totalMs: times.reduce((sum, t) => sum + t, 0),
      avgMs: times.reduce((sum, t) => sum + t, 0) / times.length,
      minMs: Math.min(...times),
      maxMs: Math.max(...times)
    };
  }
  
  static getAllStats(): Record<string, ReturnType<typeof this.getStats>> {
    const stats: Record<string, ReturnType<typeof this.getStats>> = {};
    for (const [operation, _] of this.operationTimes) {
      stats[operation] = this.getStats(operation);
    }
    return stats;
  }
  
  static reset(): void {
    this.operationTimes.clear();
  }
}

/**
 * Sparse matrix comparison utilities
 */
export class SparseMatrixComparison {
  /**
   * Compare memory usage: Dense vs Sparse
   */
  static compareMemory(n: number, edges: number): {
    dense: { bytes: number; mb: string };
    sparse: { bytes: number; kb: string };
    savings: { factor: number; percentage: string };
  } {
    const denseBytes = n * n * 8; // 8 bytes per float64
    const sparseBytes = edges * 8;
    
    return {
      dense: {
        bytes: denseBytes,
        mb: `${(denseBytes / (1024 * 1024)).toFixed(2)} MB`
      },
      sparse: {
        bytes: sparseBytes,
        kb: `${(sparseBytes / 1024).toFixed(2)} KB`
      },
      savings: {
        factor: denseBytes / sparseBytes,
        percentage: `${((1 - sparseBytes / denseBytes) * 100).toFixed(2)}%`
      }
    };
  }
  
  /**
   * Estimate performance gain
   */
  static estimateSpeedup(n: number, avgDegree: number): {
    operations: { dense: number; sparse: number };
    speedup: number;
  } {
    const denseOps = n * n;
    const sparseOps = n * avgDegree;
    
    return {
      operations: {
        dense: denseOps,
        sparse: sparseOps
      },
      speedup: denseOps / sparseOps
    };
  }
}

/**
 * Export utilities for convenience
 */
export const Sparse = SparseMatrixOps;
export const SparsePerf = SparseMatrixPerformance;
export const SparseCompare = SparseMatrixComparison;

