/**
 * Free Association Circles Module
 * 
 * Exports:
 * - Schemas (Zod types and validation)
 * - Stores (Holster-backed P2P data stores)
 * - Algorithm (allocation computation)
 * - Matching (slot compatibility)
 * - Visualization (CircleVisualization component)
 */

// Schemas and types
export * from './schemas';

// Stores
export * from './stores.svelte';

// Algorithm
export * from './algorithm.svelte';

// Slot matching
export * from './match.svelte';

// Visualization component
export { default as CircleVisualization } from './CircleVisualization.svelte';

