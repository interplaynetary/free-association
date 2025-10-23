/**
 * Free Association Circles Module
 * 
 * Exports:
 * - Schemas (Zod types and validation)
 * - Stores (Holster-backed P2P data stores)
 * - Algorithm (allocation computation)
 * - Matching (slot compatibility)
 * - Visualization (Visualization component)
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
export { default as CircleVisualization } from './Visualization.svelte';

