/**
 * Free Association Protocol - Core
 * 
 * Pure, framework-agnostic protocol logic.
 * This package contains all the core allocation algorithm, 
 * recognition calculations, and data structures.
 * 
 * Dependencies:
 * - zod (schema validation)
 * - Standard JavaScript/TypeScript only
 * 
 * No framework dependencies - can be used in:
 * - Node.js servers
 * - Browser applications
 * - CLI tools
 * - Any JavaScript environment
 */

// Core schemas and types
export * from './schemas';

// Core algorithm functions
export * from './allocation';
export * from './distribution';
export * from './tree';

// Configuration
export * from './config';

// Utilities
export * from './utils/contributors';
export * from './utils/commitments';
export * from './utils/slots';
export * from './utils/match';
export * from './utils/memoize';
export * from './utils/needTypes';
export * from './utils/capacity-filters';

// Filters
export * from './filters/filters';
export * from './filters/compliance';
export * from './filters/eligibility';
export * from './filters/types';
export * from './filters/treeSearch';

// Attributes
export * from './attributes/attribute-recognition';
export * from './attributes/attribute-types';

// Collective
export * from './collective/collective-membership';
export * from './collective/collective-recognition';
export * from './collective/schemas';

