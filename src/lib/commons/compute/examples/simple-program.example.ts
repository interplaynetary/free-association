/**
 * Example: Simple Reactive Program with Runtime Manager
 * 
 * This example shows how easy it is to deploy a reactive computation program
 * using the new ComputeRuntimeManager (Phase 2).
 * 
 * Everything is automatic:
 * - Program registration in /programs/registry/
 * - Program activation in /programs/active/
 * - Subscription tracking in /subscriptions/
 * - Output tracking in /compute/{hash}/outputs/
 * - Provenance tracking in /compute/{hash}/provenance/
 */

import { registerComputationFunction } from '../compute.svelte';
import { deployReactiveProgram, type ComputeRuntimeManager } from '../runtime-manager.svelte';
import type { ReactiveComputationGraph } from '../schema';

// ═══════════════════════════════════════════════════════════════════
// STEP 1: Define computation functions
// ═══════════════════════════════════════════════════════════════════

/**
 * Compute mutual recognition between two commitments
 */
function computeMutualRecognition(inputs: any): any {
	const { myCommitment, theirCommitment } = inputs;
	
	// Get recognition weights
	const myRecognition = myCommitment?.recognition_weights?.[theirCommitment?.pubkey] || 0;
	const theirRecognition = theirCommitment?.recognition_weights?.[myCommitment?.pubkey] || 0;
	
	// Compute mutual recognition (geometric mean)
	const mutualRecognition = Math.sqrt(myRecognition * theirRecognition);
	
	console.log('[COMPUTE] Mutual recognition:', {
		my: myRecognition,
		their: theirRecognition,
		mutual: mutualRecognition
	});
	
	return {
		mutualRecognition,
		myRecognition,
		theirRecognition
	};
}

/**
 * Allocate resources based on mutual recognition
 */
function allocateByMR(inputs: any): any {
	const { mutualRecognition, myCapacity, theirNeed } = inputs;
	
	// Simple allocation: capacity * MR weight
	const allocation = myCapacity * mutualRecognition;
	const actualAllocation = Math.min(allocation, theirNeed);
	
	console.log('[COMPUTE] Allocation:', {
		capacity: myCapacity,
		need: theirNeed,
		mr: mutualRecognition,
		allocated: actualAllocation
	});
	
	return {
		allocation: actualAllocation,
		remaining: myCapacity - actualAllocation
	};
}

// Register functions in the global registry
registerComputationFunction('computeMutualRecognition', computeMutualRecognition);
registerComputationFunction('allocateByMR', allocateByMR);

// ═══════════════════════════════════════════════════════════════════
// STEP 2: Define reactive computation graph
// ═══════════════════════════════════════════════════════════════════

const simpleAllocationProgram: ReactiveComputationGraph = {
	id: 'simple_allocation_example',
	version: '1.0.0',
	description: 'Simple allocation based on mutual recognition',
	
	// Variables (where data comes from)
	variables: {
		myCommitment: {
			type: 'subscription',
			holster_path: 'allocation/commitment',
			schema_type: 'Commitment',
			default_value: null
		},
		theirCommitment: {
			type: 'subscription',
			holster_path: 'allocation/commitment',
			schema_type: 'Commitment',
			subscribe_to_user: 'PEER_PUBKEY_HERE', // Replace with actual peer
			default_value: null
		}
	},
	
	// Computations (what to compute)
	computations: [
		{
			id: 'compute_mr',
			inputs: {
				myCommitment: { type: 'local', state_path: 'myCommitment' },
				theirCommitment: { type: 'local', state_path: 'theirCommitment' }
			},
			compute_fn: 'computeMutualRecognition',
			outputs: {
				mutualRecognition: {
					type: 'memory' // Keep in memory for next computation
				},
				myRecognition: {
					type: 'memory'
				},
				theirRecognition: {
					type: 'memory'
				}
			},
			debounce_ms: 100 // Debounce rapid changes
		},
		{
			id: 'allocate',
			depends_on: ['compute_mr'], // Run after MR computation
			inputs: {
				mutualRecognition: {
					type: 'derived',
					computation_id: 'compute_mr',
					output_key: 'mutualRecognition'
				},
				myCapacity: { type: 'value', value: 100 }, // Static value
				theirNeed: { type: 'value', value: 50 }
			},
			compute_fn: 'allocateByMR',
			outputs: {
				allocation: {
					type: 'holster',
					holster_path: 'allocation/result',
					persist_debounce_ms: 200
				},
				remaining: {
					type: 'local',
					state_path: 'remainingCapacity'
				}
			}
		}
	]
};

// ═══════════════════════════════════════════════════════════════════
// STEP 3: Deploy the program (ONE LINE!)
// ═══════════════════════════════════════════════════════════════════

/**
 * Deploy the simple allocation program
 * 
 * This single function call:
 * - Registers the program in /programs/registry/{hash}/
 * - Activates it in /programs/active/{hash}
 * - Initializes the runtime
 * - Executes computations
 * - Enables reactivity (auto-recompute on changes)
 * - Tracks all subscriptions in /subscriptions/
 * - Tracks all outputs in /compute/{hash}/outputs/
 * - Tracks all provenance in /compute/{hash}/provenance/
 */
export async function deploySimpleAllocationProgram(): Promise<ComputeRuntimeManager> {
	console.log('====================================');
	console.log('Deploying Simple Allocation Program');
	console.log('====================================');
	
	const manager = await deployReactiveProgram(simpleAllocationProgram, {
		enableProvenance: true,
		enableLineageTracking: true,
		description: 'Example program demonstrating Phase 2 runtime manager'
	});
	
	console.log('\n✅ Program deployed successfully!');
	console.log('\nStatus:', manager.getStatus());
	console.log('\nThe program will now:');
	console.log('  • Watch for changes to commitments');
	console.log('  • Automatically recompute MR and allocations');
	console.log('  • Persist results to Holster');
	console.log('  • Track full provenance chain');
	console.log('\nCheck user space diagnostics:');
	console.log('  getUserSpaceDiagnostics()');
	console.log('  debugUserSpace()');
	console.log('====================================\n');
	
	return manager;
}

// ═══════════════════════════════════════════════════════════════════
// USAGE EXAMPLES
// ═══════════════════════════════════════════════════════════════════

/**
 * Example 1: Deploy and let it run reactively
 */
export async function example1_DeployAndRun() {
	const manager = await deploySimpleAllocationProgram();
	
	// Program is now running reactively!
	// It will automatically recompute when commitments change.
	
	// Get current status
	console.log('Status:', manager.getStatus());
	
	// Get computation results
	const mrResult = manager.getComputationResult('compute_mr', 'mutualRecognition');
	const allocation = manager.getComputationResult('allocate', 'allocation');
	
	console.log('MR:', mrResult);
	console.log('Allocation:', allocation);
}

/**
 * Example 2: Deploy, get results, then stop
 */
export async function example2_DeployGetResultsStop() {
	const manager = await deploySimpleAllocationProgram();
	
	// Wait a bit for reactive computations
	await new Promise(resolve => setTimeout(resolve, 1000));
	
	// Get all results
	const results = manager.getComputationResults('allocate');
	console.log('All results:', results);
	
	// Get provenance
	const provenance = manager.getProvenance('allocate');
	console.log('Provenance:', provenance);
	
	// Stop (and deactivate)
	await manager.stop(true);
	console.log('Program stopped and deactivated');
}

/**
 * Example 3: Deploy, update variables, see reactive changes
 */
export async function example3_ReactiveUpdates() {
	const manager = await deploySimpleAllocationProgram();
	
	// Update a variable (if it's a 'value' type)
	// manager.setVariable('myCapacity', 200);
	
	// Or update local state
	manager.updateLocalState({
		myCapacity: 200,
		theirNeed: 75
	});
	
	// Trigger manual recomputation
	await manager.execute();
	
	// Get updated results
	const allocation = manager.getComputationResult('allocate', 'allocation');
	console.log('Updated allocation:', allocation);
}

/**
 * Example 4: Query provenance and versions
 */
export async function example4_ProvenanceQuery() {
	const manager = await deploySimpleAllocationProgram();
	
	// Wait for computation
	await new Promise(resolve => setTimeout(resolve, 500));
	
	// Get latest result from canonical path
	const latest = await manager.getLatest('allocation/result');
	console.log('Latest allocation:', latest);
	
	// Get all versions (history)
	const versions = await manager.getAllVersions('allocation/result');
	console.log('All versions:', versions);
	
	// Verify result against provenance
	const provenance = manager.getProvenance('allocate');
	if (provenance) {
		const isValid = await manager.verifyComputationResult(
			'allocate',
			latest?.data,
			provenance
		);
		console.log('Result verified:', isValid);
	}
}

// ═══════════════════════════════════════════════════════════════════
// THE POWER OF PHASE 2
// ═══════════════════════════════════════════════════════════════════

/**
 * Compare: Before vs After Phase 2
 * 
 * BEFORE Phase 2 (manual):
 * ```typescript
 * const runtime = new ComputationGraphRuntime(graph);
 * await runtime.initialize();
 * await runtime.execute();
 * runtime.enableReactivity();
 * // No automatic registration
 * // No automatic activation
 * // No subscription tracking
 * // No centralized output tracking
 * ```
 * 
 * AFTER Phase 2 (automatic):
 * ```typescript
 * const manager = await deployReactiveProgram(graph);
 * // ✅ Registered in /programs/registry/
 * // ✅ Activated in /programs/active/
 * // ✅ Subscriptions tracked in /subscriptions/
 * // ✅ Outputs tracked in /compute/{hash}/outputs/
 * // ✅ Provenance tracked in /compute/{hash}/provenance/
 * // ✅ Reactive mode enabled
 * // ✅ Ready to use!
 * ```
 * 
 * ONE LINE vs MANY LINES + AUTOMATIC SPACE INTEGRATION
 */

