/**
 * Elegant Lambda Calculus: Main Export
 * 
 * Fully curried, point-free lambda calculus implementation
 * Matches λ-R specification exactly
 */

// Export types (λ-R compliant)
export * from './types';

// Export combinators
export * from './combinators';

// Export recognition
export * from './recognition';

// Export modules as namespaces
export * as filters from './filters';
export * as limits from './limits';
export * as collective from './collective';
export * as commons from './commons';
export * as allocation from './allocation';
export * as system from './system';

// Re-export for convenience
import * as combinators from './combinators';
import * as recognition from './recognition';
import { curriedFilters } from './filters';
import { curriedLimits } from './limits';
import { curriedCollective } from './collective';
import { curriedCommons } from './commons';
import { curriedAllocation } from './allocation';
import { curriedSystem } from './system';

export const elegant = {
  ...combinators,
  ...recognition,
  ...curriedFilters,
  ...curriedLimits,
  ...curriedCollective,
  ...curriedCommons,
  ...curriedAllocation,
  ...curriedSystem,
};

export default elegant;
