/**
 * Reactive Computation System - Usage Examples
 * 
 * Demonstrates the declarative dataflow programming system for:
 * - Variable bindings (values, subscriptions, local state, derived)
 * - Computation graphs with dependencies
 * - Output persistence (Holster, local state, memory)
 */

import type { ReactiveComputationGraph } from '../v1/schemas';
import {
	registerComputationFunction,
	executeComputationGraph,
	ComputationGraphRuntime,
	createReactiveComputation
} from '../compute.svelte';

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 1: Simple Value Computation
// ═══════════════════════════════════════════════════════════════════

/**
 * Register computation functions first
 */
registerComputationFunction('add', ({ a, b }: { a: number; b: number }) => {
	return { sum: a + b };
});

registerComputationFunction('multiply', ({ a, b }: { a: number; b: number }) => {
	return { product: a * b };
});

/**
 * Simple computation: (2 + 3) * 4 = 20
 */
export const simpleComputationGraph: ReactiveComputationGraph = {
	id: 'simple-math',
	
	variables: {
		// Static values
		x: { type: 'value', value: 2 },
		y: { type: 'value', value: 3 },
		z: { type: 'value', value: 4 }
	},
	
	computations: [
		{
			id: 'step1',
			inputs: {
				a: { type: 'value', value: 2 }, // Inline binding
				b: { type: 'value', value: 3 }
			},
			compute_fn: 'add',
			outputs: {
				sum: { type: 'memory' } // Keep in memory for next step
			}
		},
		{
			id: 'step2',
			inputs: {
				a: { type: 'derived', computation_id: 'step1', output_key: 'sum' },
				b: { type: 'value', value: 4 }
			},
			compute_fn: 'multiply',
			outputs: {
				product: { type: 'local', state_path: 'results.final' }
			},
			depends_on: ['step1'] // Explicit dependency
		}
	]
};

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 2: Mutual Recognition Computation
// ═══════════════════════════════════════════════════════════════════

/**
 * Register mutual recognition function
 */
registerComputationFunction('computeMutualRecognition', ({ myTree, theirTree }: any) => {
	// Simplified MR calculation
	const myRecognition = 0.7; // Mock: should calculate from trees
	const theirRecognition = 0.6;
	const mutualRecognition = Math.min(myRecognition, theirRecognition);
	
	return {
		myShare: myRecognition,
		theirShare: theirRecognition,
		mutualRecognition
	};
});

/**
 * Mutual recognition graph - subscribes to two trees and computes MR
 */
export function createMutualRecognitionGraph(theirPubkey: string): ReactiveComputationGraph {
	return {
		id: 'mutual-recognition',
		
		variables: {
			// Subscribe to my tree
			myTree: {
				type: 'subscription',
				holster_path: 'tree',
				schema_type: 'RootNode',
				default_value: null
			},
			
			// Subscribe to their tree
			theirTree: {
				type: 'subscription',
				holster_path: 'tree',
				schema_type: 'RootNode',
				subscribe_to_user: theirPubkey,
				default_value: null
			}
		},
		
		computations: [
			{
				id: 'compute-mr',
				inputs: {
					myTree: { type: 'subscription', holster_path: 'tree', schema_type: 'RootNode' },
					theirTree: {
						type: 'subscription',
						holster_path: 'tree',
						schema_type: 'RootNode',
						subscribe_to_user: theirPubkey
					}
				},
				compute_fn: 'computeMutualRecognition',
				outputs: {
					// Store each result in Holster
					myShare: {
						type: 'holster',
						holster_path: `recognition/${theirPubkey}/my_share`,
						schema_type: 'Number'
					},
					theirShare: {
						type: 'holster',
						holster_path: `recognition/${theirPubkey}/their_share`,
						schema_type: 'Number'
					},
					mutualRecognition: {
						type: 'holster',
						holster_path: `recognition/${theirPubkey}/mutual`,
						schema_type: 'Number'
					}
				},
				description: 'Compute mutual recognition between me and another user'
			}
		]
	};
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 3: Multi-Step Pipeline with Dependencies
// ═══════════════════════════════════════════════════════════════════

registerComputationFunction('filterData', ({ data, threshold }: any) => {
	const filtered = data.filter((x: number) => x > threshold);
	return { filtered };
});

registerComputationFunction('aggregateData', ({ filtered }: any) => {
	const sum = filtered.reduce((a: number, b: number) => a + b, 0);
	const avg = sum / filtered.length;
	return { sum, avg, count: filtered.length };
});

registerComputationFunction('formatResults', ({ sum, avg, count }: any) => {
	return {
		report: `Processed ${count} items: Sum=${sum}, Avg=${avg.toFixed(2)}`
	};
});

export const pipelineGraph: ReactiveComputationGraph = {
	id: 'data-pipeline',
	
	variables: {
		// Input data
		rawData: { type: 'value', value: [1, 5, 10, 15, 20, 25, 30] },
		threshold: { type: 'value', value: 10 }
	},
	
	computations: [
		// Step 1: Filter
		{
			id: 'filter',
			inputs: {
				data: { type: 'value', value: [1, 5, 10, 15, 20, 25, 30] },
				threshold: { type: 'value', value: 10 }
			},
			compute_fn: 'filterData',
			outputs: {
				filtered: { type: 'memory' }
			}
		},
		
		// Step 2: Aggregate
		{
			id: 'aggregate',
			inputs: {
				filtered: { type: 'derived', computation_id: 'filter', output_key: 'filtered' }
			},
			compute_fn: 'aggregateData',
			outputs: {
				sum: { type: 'memory' },
				avg: { type: 'memory' },
				count: { type: 'memory' }
			},
			depends_on: ['filter']
		},
		
		// Step 3: Format
		{
			id: 'format',
			inputs: {
				sum: { type: 'derived', computation_id: 'aggregate', output_key: 'sum' },
				avg: { type: 'derived', computation_id: 'aggregate', output_key: 'avg' },
				count: { type: 'derived', computation_id: 'aggregate', output_key: 'count' }
			},
			compute_fn: 'formatResults',
			outputs: {
				report: {
					type: 'local',
					state_path: 'pipeline.finalReport'
				}
			},
			depends_on: ['aggregate']
		}
	]
};

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 4: Allocation Calculation with Subscriptions
// ═══════════════════════════════════════════════════════════════════

registerComputationFunction('computeAllocation', ({ myCapacity, theirNeed, mutualRecognition }: any) => {
	// Simplified allocation logic
	const totalAvailable = myCapacity?.total || 0;
	const theirRequest = theirNeed?.amount || 0;
	const mrShare = mutualRecognition || 0;
	
	const allocated = Math.min(theirRequest, totalAvailable * mrShare);
	
	return {
		allocated,
		remaining: totalAvailable - allocated,
		fulfillmentRate: theirRequest > 0 ? allocated / theirRequest : 0
	};
});

export function createAllocationGraph(recipientPubkey: string): ReactiveComputationGraph {
	return {
		id: 'allocation-computation',
		
		variables: {
			// My capacity (from Holster)
			myCapacity: {
				type: 'subscription',
				holster_path: 'capacity/main',
				schema_type: 'BaseCapacity',
				default_value: { total: 0 }
			},
			
			// Their need (from their Holster)
			theirNeed: {
				type: 'subscription',
				holster_path: 'needs/primary',
				schema_type: 'BaseNeed',
				subscribe_to_user: recipientPubkey,
				default_value: { amount: 0 }
			},
			
			// Pre-computed MR (from previous graph)
			mutualRecognition: {
				type: 'subscription',
				holster_path: `recognition/${recipientPubkey}/mutual`,
				schema_type: 'Number',
				default_value: 0
			}
		},
		
		computations: [
			{
				id: 'allocate',
				inputs: {
					myCapacity: {
						type: 'subscription',
						holster_path: 'capacity/main',
						schema_type: 'BaseCapacity'
					},
					theirNeed: {
						type: 'subscription',
						holster_path: 'needs/primary',
						schema_type: 'BaseNeed',
						subscribe_to_user: recipientPubkey
					},
					mutualRecognition: {
						type: 'subscription',
						holster_path: `recognition/${recipientPubkey}/mutual`,
						schema_type: 'Number'
					}
				},
				compute_fn: 'computeAllocation',
				outputs: {
					allocated: {
						type: 'holster',
						holster_path: `allocation/${recipientPubkey}/amount`,
						schema_type: 'Number',
						persist_debounce_ms: 100
					},
					remaining: {
						type: 'local',
						state_path: 'allocation.remaining'
					},
					fulfillmentRate: {
						type: 'holster',
						holster_path: `allocation/${recipientPubkey}/rate`,
						schema_type: 'Number'
					}
				},
				description: 'Compute allocation for recipient based on capacity, need, and MR'
			}
		]
	};
}

// ═══════════════════════════════════════════════════════════════════
// USAGE EXAMPLES
// ═══════════════════════════════════════════════════════════════════

/**
 * Example 1: Execute simple computation
 */
export async function runSimpleExample() {
	console.log('=== Simple Computation Example ===');
	
	const runtime = await executeComputationGraph(simpleComputationGraph);
	
	// Get results
	const result = runtime.getComputationResult('step2', 'product');
	console.log('Result:', result); // Should be 20
	
	await runtime.cleanup();
}

/**
 * Example 2: Execute mutual recognition
 */
export async function runMutualRecognitionExample(theirPubkey: string) {
	console.log('=== Mutual Recognition Example ===');
	
	const graph = createMutualRecognitionGraph(theirPubkey);
	const runtime = await executeComputationGraph(graph);
	
	// Get MR value
	const mr = runtime.getComputationResult('compute-mr', 'mutualRecognition');
	console.log('Mutual Recognition:', mr);
	
	// Runtime keeps subscriptions active for reactivity
	return runtime; // Cleanup when done
}

/**
 * Example 3: Execute pipeline
 */
export async function runPipelineExample() {
	console.log('=== Data Pipeline Example ===');
	
	const runtime = await executeComputationGraph(pipelineGraph);
	
	// Get final report
	const report = runtime.getComputationResult('format', 'report');
	console.log('Report:', report);
	
	await runtime.cleanup();
}

/**
 * Example 4: Execute allocation
 */
export async function runAllocationExample(recipientPubkey: string) {
	console.log('=== Allocation Computation Example ===');
	
	const graph = createAllocationGraph(recipientPubkey);
	const runtime = await executeComputationGraph(graph);
	
	// Get allocation results
	const allocated = runtime.getComputationResult('allocate', 'allocated');
	const rate = runtime.getComputationResult('allocate', 'fulfillmentRate');
	
	console.log('Allocated:', allocated);
	console.log('Fulfillment Rate:', (rate * 100).toFixed(1) + '%');
	
	// Keep runtime alive for reactive updates
	return runtime;
}

// ═══════════════════════════════════════════════════════════════════
// INTERACTIVE RUNTIME EXAMPLE
// ═══════════════════════════════════════════════════════════════════

/**
 * Example: Interactive runtime with variable updates
 */
export async function runInteractiveExample() {
	console.log('=== Interactive Runtime Example ===');
	
	const runtime = new ComputationGraphRuntime(simpleComputationGraph);
	await runtime.initialize();
	
	// Initial execution
	await runtime.execute();
	console.log('Initial result:', runtime.getComputationResult('step2', 'product'));
	
	// Update variable
	runtime.setVariable('x', 10);
	await runtime.execute();
	console.log('After update:', runtime.getComputationResult('step2', 'product'));
	
	await runtime.cleanup();
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 5: Fetch Binding (One-Time Get)
// ═══════════════════════════════════════════════════════════════════

/**
 * Demonstrates the difference between fetch (one-time) and subscription (reactive)
 */
registerComputationFunction('compareData', ({ fetchedData, subscribedData }: any) => {
	return {
		fetched: fetchedData,
		subscribed: subscribedData,
		areSame: JSON.stringify(fetchedData) === JSON.stringify(subscribedData)
	};
});

export function createFetchVsSubscriptionGraph(userPubkey: string): ReactiveComputationGraph {
	return {
		id: 'fetch-vs-subscription',
		
		variables: {
			// One-time fetch - gets current value, doesn't update
			userTreeOnce: {
				type: 'fetch',
				holster_path: 'tree',
				schema_type: 'TreeNode',
				fetch_from_user: userPubkey,
				wait_ms: 200,
				default_value: null
			},
			
			// Reactive subscription - updates when data changes
			userTreeLive: {
				type: 'subscription',
				holster_path: 'tree',
				schema_type: 'TreeNode',
				subscribe_to_user: userPubkey,
				default_value: null
			}
		},
		
		computations: [
			{
				id: 'compare',
				inputs: {
					fetchedData: { type: 'fetch', holster_path: 'tree', schema_type: 'TreeNode', fetch_from_user: userPubkey, wait_ms: 200 },
					subscribedData: { type: 'subscription', holster_path: 'tree', schema_type: 'TreeNode', subscribe_to_user: userPubkey }
				},
				compute_fn: 'compareData',
				outputs: {
					fetched: { type: 'local', state_path: 'comparison.fetched' },
					subscribed: { type: 'local', state_path: 'comparison.subscribed' },
					areSame: { type: 'memory' }
				}
			}
		]
	};
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 6: Reactive Mode with Auto-Recomputation
// ═══════════════════════════════════════════════════════════════════

/**
 * Example: Reactive computation (auto-recompute on changes)
 */
export async function runReactiveExample() {
	console.log('=== Reactive Mode Example ===');
	
	const runtime = new ComputationGraphRuntime(createMutualRecognitionGraph('peer-pubkey'));
	await runtime.initialize();
	await runtime.execute();
	
	console.log('Initial result:', runtime.getComputationResults('compute-mr'));
	
	// Enable reactive mode - computation will auto-rerun when subscriptions change
	runtime.enableReactivity();
	console.log('Reactive mode enabled - will auto-recompute when subscriptions change');
	
	// The computation will automatically re-run when myTree or theirTree subscriptions update
	
	// Wait a bit to see reactive updates...
	await new Promise(resolve => setTimeout(resolve, 5000));
	
	// Disable reactivity and cleanup
	runtime.disableReactivity();
	await runtime.cleanup();
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 7: Debounced Computation
// ═══════════════════════════════════════════════════════════════════

/**
 * Search computation that debounces user input
 */
registerComputationFunction('search', ({ query, threshold }: { query: string; threshold: number }) => {
	// Mock search implementation
	const results = query.length >= threshold 
		? [`Result for "${query}" 1`, `Result for "${query}" 2`]
		: [];
	
	return { results, resultCount: results.length };
});

export const debouncedSearchGraph: ReactiveComputationGraph = {
	id: 'debounced-search',
	
	variables: {
		searchQuery: { type: 'value', value: '' },
		minLength: { type: 'value', value: 3 }
	},
	
	computations: [
		{
			id: 'search',
			inputs: {
				query: { type: 'local', state_path: 'searchQuery', default_value: '' },
				threshold: { type: 'value', value: 3 }
			},
			compute_fn: 'search',
			debounce_ms: 300, // Wait 300ms after last input change
			outputs: {
				results: { type: 'local', state_path: 'searchResults' },
				resultCount: { type: 'memory' }
			}
		}
	]
};

/**
 * Example: Debounced computation
 */
export async function runDebouncedExample() {
	console.log('=== Debounced Computation Example ===');
	
	const runtime = new ComputationGraphRuntime(debouncedSearchGraph);
	await runtime.initialize();
	
	// Simulate rapid typing
	runtime.updateLocalState({ searchQuery: 't' });
	await runtime.execute();
	
	runtime.updateLocalState({ searchQuery: 'te' });
	await runtime.execute();
	
	runtime.updateLocalState({ searchQuery: 'tes' });
	await runtime.execute();
	
	runtime.updateLocalState({ searchQuery: 'test' });
	await runtime.execute();
	
	// Wait for debounce to complete
	await new Promise(resolve => setTimeout(resolve, 400));
	
	console.log('Search result:', runtime.getComputationResults('search'));
	await runtime.cleanup();
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 8: Local Bindings
// ═══════════════════════════════════════════════════════════════════

/**
 * Demonstrates local bindings - variables scoped to a single computation
 */
registerComputationFunction('computeWithConstants', ({ input, PI, E }: any) => {
	return {
		circleArea: PI * input * input,
		exponential: Math.pow(E, input)
	};
});

export const localBindingsGraph: ReactiveComputationGraph = {
	id: 'local-bindings',
	
	variables: {
		radius: { type: 'value', value: 5 }
	},
	
	computations: [
		{
			id: 'compute',
			inputs: {
				input: { type: 'value', value: 5 }
			},
			local_bindings: {
				// These variables are only available within this computation
				PI: { type: 'value', value: Math.PI },
				E: { type: 'value', value: Math.E }
			},
			compute_fn: 'computeWithConstants',
			outputs: {
				circleArea: { type: 'memory' },
				exponential: { type: 'memory' }
			}
		}
	]
};

/**
 * Example: Local bindings
 */
export async function runLocalBindingsExample() {
	console.log('=== Local Bindings Example ===');
	
	const runtime = await executeComputationGraph(localBindingsGraph);
	console.log('Circle area:', runtime.getComputationResult('compute', 'circleArea'));
	console.log('Exponential:', runtime.getComputationResult('compute', 'exponential'));
	await runtime.cleanup();
}

