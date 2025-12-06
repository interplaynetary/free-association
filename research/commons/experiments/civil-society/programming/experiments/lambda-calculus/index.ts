/**
 * Lambda Calculus Implementation of Free-Association Framework
 * 
 * This is the main entry point for the λ-R (Recognition Calculus) implementation.
 * It provides a complete, type-safe implementation of the Free-Association Framework
 * using TypeScript and Zod schemas.
 * 
 * @module lambda-calculus
 */

// ============================================================================
// Core Types and Schemas
// ============================================================================

export * from './src/core/types';

// ============================================================================
// Primitive Operations
// ============================================================================

export * from './src/core/primitives';

// ============================================================================
// Recognition System
// ============================================================================

export * from './src/core/recognition';

// ============================================================================
// Filter System
// ============================================================================

export * from './src/core/filters';

// ============================================================================
// Limit System
// ============================================================================

export * from './src/core/limits';

// ============================================================================
// Collective Formation
// ============================================================================

export * from './src/core/collective';

// ============================================================================
// Commons Formation and Evolution
// ============================================================================

export * from './src/core/commons';

// ============================================================================
// Capacity Allocation
// ============================================================================

export * from './src/core/allocation';

// ============================================================================
// System State and Evolution
// ============================================================================

export * from './src/core/system';

// ============================================================================
// Elegant Lambda Calculus API
// ============================================================================

// Export elegant implementation (fully curried, point-free, monadic)
export * as elegant from './src/elegant';

// ============================================================================
// Version Information
// ============================================================================

export const VERSION = '1.0.0';
export const LAMBDA_CALCULUS_VERSION = 'λ-R v1.0';

// ============================================================================
// Quick Start Helpers
// ============================================================================

/**
 * Create a simple coordination system with default configuration
 */
export function createSimpleSystem(entityIds: string[]) {
  const entities = new Set(
    entityIds.map((id) => ({
      id,
      name: id,
    }))
  );

  return {
    entities,
  };
}

// ============================================================================
// Documentation Links
// ============================================================================

/**
 * Documentation and references
 */
export const DOCS = {
  readme: '../../../LAMBDA.md',
  protocol: '../../../PROTOCOL.md',
  theory: '../../../docs/technical/mathematics.md',
};

