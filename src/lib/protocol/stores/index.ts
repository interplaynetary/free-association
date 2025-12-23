/**
 * Free Association Protocol - Svelte Stores
 * 
 * Reactive Svelte wrappers around the pure protocol core.
 * These stores provide real-time synchronization via Holster P2P.
 * 
 * Dependencies:
 * - svelte (reactive stores)
 * - @free-association/protocol/core (pure protocol logic)
 * 
 * Use these stores in Svelte applications for reactive updates.
 * 
 * NOTE: This module only exports Svelte stores and reactive wrappers.
 * For pure protocol types and functions, import from '@playnet/free-association' or the main protocol index.
 */

// Main protocol stores
export {
	// Core data stores
	myRecognitionTreeStore,
	myCommitmentStore,
	myAllocationStateStore,

	// Network data stores
	networkCommitments,
	networkRecognitionWeights,
	networkAllocations,
	networkNeedsIndex,
	networkCapacityIndex,

	// Derived stores
	myRecognitionWeights,
	myMutualRecognition,

	// Helper functions
	getAllCommitmentsRecord,
	getNetworkRecognitionWeightsRecord,
	initializeAllocationStores
} from './stores.svelte';

// Note: SpaceTimeIndex is already exported from core/allocation, don't duplicate

// Allocation stores
export {
	myActiveNeedsByType,
	myOverAllocationHistory,
	myDampingFactors,
	systemState,
	allocationsStore,
	myDampedNeeds,
	allocationDebugInfo,
	// Derived stores moved from stores.svelte.ts to break circular dependency
	myRecognitionOfOthers,
	othersRecognitionOfMe,
	myCurrentNeeds,
	myAvailableCapacity
} from './allocation.svelte';

// Attribute stores
export {
	myAttributeRecognitions,
	myAttributeSubscriptions,
	createAttributeStore
	// Note: subscribeToAttribute is already exported from core/attributes, don't duplicate
} from './attributes.svelte';

// Collective stores (re-export only stores, not pure functions from core)
export {
	collectiveTrees,
	collectiveTreeStore,
	createCollectiveTreeStore,
	deleteCollectiveTreeStore
} from './collective-tree.svelte';

// Filter stores
export * from './filters/objectFiltering.svelte';
export * from './filters/space.svelte';
export * from './filters/time.svelte';
export * from './filters/capacitySpecific.svelte';

