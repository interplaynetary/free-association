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
 */

// Schemas and types
export * from './v2/schemas';

// Protocol functions
export * from './v2/protocol';

// Stores
export * from './stores.svelte';

// Node store factory and manager
export * from './compute/node-store.svelte';

// Reactive computation runtime
export * from './compute/compute.svelte';

// Program hashing and indexing
export * from './compute/program-hash.svelte';

// RDL validator
export * from './compute/rdl-validator';

// Interval Tree Clocks (ITC)
export * from './utils/itc';

// Algorithm
export * from './algorithm.svelte';

// Slot matching
export * from './match.svelte';

// Visualization component
export { default as Visualization } from './components/Visualization.svelte';

