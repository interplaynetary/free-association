/**
 * Sparse Matrix Module for Lambda Calculus
 * 
 * Provides efficient storage and operations for sparse recognition graphs.
 * Use this for large-scale networks where most recognition values are zero.
 */

export type {
  EntityId,
  SparseRecognitionGraph
} from './types';

export {
  SparseOps,
  toSparse,
  fromSparse,
  isSparseGraph,
  empty,
  clone
} from './types';

export type {
  Distribution
} from './operations';

export {
  sparseMutual,
  sparseTMR,
  sparseMRS,
  sparseMRD,
  sparseRMR,
  getMutualRecognitionPairs,
  checkBudgetConstraint,
  findBudgetViolations,
  batchSparseMRS,
  computeStatistics
} from './operations';

