/**
 * Free Association Circles Module
 * 
 * Exports:
 * - Schemas (Zod types and validation)
 * - Protocol (tree manipulation, recognition, allocation)
 * - Stores (Holster-backed P2P data stores)
 * - Node Stores (Dynamic store factory from NodeDataStorage configs)
 * - Reactive Computation (Declarative dataflow execution)
 * - Program Hashing (Content-addressable program indexing)
 * - RDL Validator (JSON Schema validation for RDL)
 * - ITC (Interval Tree Clocks for dynamic causality tracking)
 * - Algorithm (allocation computation)
 * - Matching (slot compatibility)
 * - Visualization (Visualization component)
 * 
 * V5 UPDATE: Now using global recognition model with event-driven allocation
 */

import './v5/holster.svelte'; // This initializes Holster (migration Phase 1)

// Schemas and types (V5) - Safe for SSR
export * from './v5/schemas';

// Protocol functions (V5) - Safe for SSR
export * from './v5/tree';

// Interval Tree Clocks (ITC) - Safe for SSR
export * from './utils/itc';

// Slot matching (V5) - Safe for SSR  
export * from './v5/match.svelte';

// Browser-only exports (require Holster, reactive stores, D3)
export * from './v5/stores.svelte';
export * from './v5/free-algorithm.svelte';
export * from './compute/node-store.svelte';
export * from './compute/compute.svelte';
export * from './compute/program-hash.svelte';
export { default as Visualization } from './components/Visualization.svelte';

