/**
 * ZK Module - Public API
 * 
 * Zero-knowledge proofs for provenance verification.
 * Standalone (doesn't require Mina blockchain).
 * 
 * Usage:
 * ```typescript
 * import { initializeZkSystem, proveAllocation } from './zk';
 * 
 * // Initialize once (takes 1-5 minutes)
 * await initializeZkSystem();
 * 
 * // Prove allocation without revealing MR values
 * const { proof, allocatedAmount } = await proveAllocation({
 *   recipientId: 'alice',
 *   mrValue: 0.35,        // PRIVATE!
 *   mrSum: 1.0,           // PRIVATE!
 *   totalCapacity: 100    // PRIVATE!
 * });
 * ```
 */

// System initialization
export {
	initializeZkSystem,
	isZkReady,
	getZkStatus
} from './zk-provenance.svelte';

// Event integrity proofs
export {
	proveEventIntegrity,
	verifyEventIntegrity
} from './zk-provenance.svelte';

// Allocation proofs (killer app!)
export {
	proveAllocation,
	verifyAllocation
} from './zk-provenance.svelte';

// Recursive chain proofs
export {
	proveEventChain,
	verifyChain
} from './zk-provenance.svelte';

// Tree-based rollup
export {
	proveAllocationRollup,
	verifyRollup
} from './zk-provenance.svelte';

// Utilities
export {
	exportProof,
	importProof
} from './zk-provenance.svelte';

// Low-level programs (for advanced usage)
export {
	EventIntegrityProgram,
	EventChainProgram,
	AllocationProgram,
	AllocationRollupProgram,
	compileZkPrograms,
	verifyProof
} from './zk-programs';

// Proof types
export type {
	EventIntegrityProof,
	EventChainProof,
	AllocationProof,
	AllocationRollupProof
} from './zk-programs';

// DAG with MerkleTree & MerkleList (✅ AVAILABLE NOW in v2.10.0)
export {
	// DAG construction
	DAGMerkleTree,
	EventList,
	createDAG,
	createEventList,
	hashToField,
	hashToBigInt,
	
	// Provable circuits
	DAGMembershipProgram,
	EventListMembershipProgram,
	DAGLineageProgram,
	compileDAGCircuits,
	DAGWitness25,
	
	// Proof types
	type DAGMembershipProof,
	type EventListMembershipProof,
	type DAGLineageProof
} from './old/zk-dag-available';

// DAG integration
export {
	// Initialization
	initializeDAGCircuits,
	areDAGCircuitsReady,
	
	// DAG construction
	buildDAGFromEvents,
	buildEventListFromEvents,
	
	// Provable operations
	proveMembershipInDAG,
	proveMembershipInList,
	proveLineage,
	
	// Off-chain queries
	queryEventAtIndex,
	queryDAGRoot,
	getDAGStats,
	getEventListStats,
	getDAGStatus
} from './old/zk-dag-integration-available.svelte';

// DAG with MerkleMap (✅ BEST OPTION - Key-value, unlimited capacity)
export {
	// DAG construction
	DAGMap,
	createDAGMap,
	
	// Provable circuits
	DAGMapMembershipProgram,
	DAGMapLineageProgram,
	DAGMapUpdateProgram,
	compileDAGMapCircuits,
	
	// Proof types
	type DAGMapMembershipProof,
	type DAGMapLineageProof,
	type DAGMapUpdateProof
} from './zk-dag-map';

// DAG Map integration
export {
	// Initialization
	initializeDAGMapCircuits,
	areDAGMapCircuitsReady,
	
	// DAG construction
	buildDAGMapFromEvents,
	
	// Provable operations
	proveMembershipInDAGMap,
	proveLineageInDAGMap,
	
	// Off-chain queries
	queryParentInDAGMap,
	queryDAGMapRoot,
	getDAGMapStats,
	getDAGMapStatus
} from './zk-dag-map-integration.svelte';

// Note: IndexedMerkleMap (similar to MerkleMap) requires o1js v2.12+
// See DAG-FUTURE.md for details
