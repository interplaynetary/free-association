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

import '$lib/network/holster.svelte'; // This initializes Holster (migration Phase 1)

// Schemas and types (V5) - Safe for SSR
export * from '$lib/protocol/schemas';

// Protocol functions (V5) - Safe for SSR
export * from '$lib/protocol/tree';

// Interval Tree Clocks (ITC) - Safe for SSR
export * from '$lib/utils/primitives/itc';

// Slot matching (V5) - Safe for SSR  
export * from '$lib/protocol/utils/match';

export * from '$lib/protocol/allocation';


// Browser-only exports (require Holster, reactive stores, D3)
export * from '$lib/protocol/stores.svelte';
export * from '$lib/protocol/allocation.svelte';
