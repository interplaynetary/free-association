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

// Schemas and types (V5) - Safe for SSR
export * from '$lib/protocol/schemas';

// Protocol functions (V5) - Safe for SSR
export * from '$lib/protocol/tree';

// Interval Tree Clocks (ITC) - Safe for SSR
export * from '$lib/utils/primitives/itc';

// Slot matching (V5) - Safe for SSR  
export * from '$lib/protocol/utils/match';

// Re-export allocation functions (excluding types already exported from schemas)
export {
	createInitialState,
	buildSystemState,
	computeTotalNeedMagnitude,
	computeContractionRate,
	computePercentNeedsMet,
	checkUniversalSatisfaction,
	estimateIterationsToConvergence,
	computeConvergenceSummary,
	computeMaxPersonNeed,
	computeNeedVariance,
	computePeopleStuck,
	computeDampingFactors,
	updateOverAllocationHistory,
	computeMutualRecognition,
	computeAllocations,
	applyNeedUpdateLaw,
	applyDivisibilityConstraints,
	meetsMinimumAllocation,
	redistributeRemainders,
	type SystemStateSnapshot,
	type AllocationResult
} from '$lib/protocol/allocation';

// NOTE: Browser-only exports (stores.svelte, allocation.svelte) are NOT exported here
// to prevent premature initialization on iOS Safari. Import them directly when needed:
// import { myRecognitionTreeStore } from '$lib/protocol/stores.svelte'
// import { myAllocationsAsProvider } from '$lib/protocol/allocation.svelte'
