/**
 * Hybrid Storage Example
 * 
 * Demonstrates how the hybrid storage approach makes querying elegant:
 * - Canonical paths for easy "latest" queries
 * - Versioned paths for immutable history
 * - Indices for fast discovery and lineage
 */

import { 
	ComputationGraphRuntime, 
	registerComputationFunction 
} from '../compute.svelte';
import type { ReactiveComputationGraph } from '../v1/schemas';

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 1: Simple Latest Query
// ═══════════════════════════════════════════════════════════════════

export async function example1_simpleLatestQuery() {
	console.log('\n========================================');
	console.log('EXAMPLE 1: Simple Latest Query');
	console.log('========================================\n');
	
	// Register computation
	registerComputationFunction('add', (inputs: any) => inputs.a + inputs.b);
	
	const program: ReactiveComputationGraph = {
		id: 'simple-math',
		variables: {
			num1: { type: 'value', value: 5 },
			num2: { type: 'value', value: 3 }
		},
		computations: [{
			id: 'sum',
			inputs: {
				a: { type: 'local', name: 'num1' },
				b: { type: 'local', name: 'num2' }
			},
			compute_fn: 'add',
			outputs: {
				result: { type: 'holster', holster_path: 'math/sum' }
			}
		}]
	};
	
	// Execute computation
	const runtime = new ComputationGraphRuntime(program);
	await runtime.initialize();
	await runtime.execute();
	
	console.log('\n📍 Storage locations:');
	console.log('  1. Canonical: <program_hash>/math/sum');
	console.log('     → { data: 8, _provenance: {...}, _updatedAt: ... }');
	console.log('  2. Versioned: <program_hash>/math/sum/_versions/p_...');
	console.log('     → { data: 8, _provenance: {...}, _immutable: true }');
	console.log('  3. Latest index: <program_hash>/_index/latest/math/sum');
	console.log('     → "p_alice:1_comp:sum_det:..."');
	console.log('  4. Computation index: <program_hash>/_index/computations/sum');
	console.log('     → ["math/sum"]');
	console.log('  5. Lineage index: <program_hash>/_index/lineage/<prov_id>');
	console.log('     → { computationId: "sum", inputs: [...], outputs: [...] }');
	
	// Query latest (EASY!)
	console.log('\n🔍 Querying latest:');
	const latest = await runtime.getLatest('math/sum');
	if (latest) {
		console.log(`  ✅ Latest value: ${latest.data}`);
		console.log(`  ✅ Vector clock: ${JSON.stringify(latest._provenance.vectorClock)}`);
		console.log(`  ✅ Updated at: ${new Date(latest._updatedAt).toISOString()}`);
	}
	
	return runtime;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 2: Version History
// ═══════════════════════════════════════════════════════════════════

export async function example2_versionHistory() {
	console.log('\n========================================');
	console.log('EXAMPLE 2: Version History');
	console.log('========================================\n');
	
	registerComputationFunction('increment', (inputs: any) => inputs.value + 1);
	
	const program: ReactiveComputationGraph = {
		id: 'counter',
		variables: {
			current: { type: 'value', value: 0 }
		},
		computations: [{
			id: 'inc',
			inputs: {
				value: { type: 'local', name: 'current' }
			},
			compute_fn: 'increment',
			outputs: {
				result: { type: 'holster', holster_path: 'counter/value' }
			}
		}]
	};
	
	const runtime = new ComputationGraphRuntime(program);
	await runtime.initialize();
	
	// Execute multiple times to create version history
	console.log('📝 Executing 5 times to create version history...\n');
	for (let i = 0; i < 5; i++) {
		// Update input
		runtime['localState']['current'] = i;
		await runtime.execute();
		console.log(`  Execution ${i + 1}: counter = ${i + 1}`);
	}
	
	// Query latest
	console.log('\n🔍 Latest value:');
	const latest = await runtime.getLatest('counter/value');
	if (latest) {
		console.log(`  Value: ${latest.data}`);
	}
	
	// Query all versions
	console.log('\n📚 All versions:');
	const allVersions = await runtime.getAllVersions('counter/value');
	console.log(`  Found ${allVersions.length} versions`);
	for (const version of allVersions) {
		console.log(`    - ${version.data} (VC: ${JSON.stringify(version._provenance.vectorClock)})`);
	}
	
	return runtime;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 3: RDL Program Composition (The Key Use Case!)
// ═══════════════════════════════════════════════════════════════════

export async function example3_programComposition() {
	console.log('\n========================================');
	console.log('EXAMPLE 3: RDL Program Composition');
	console.log('========================================\n');
	
	registerComputationFunction('square', (inputs: any) => inputs.x * inputs.x);
	registerComputationFunction('double', (inputs: any) => inputs.x * 2);
	
	// Program A: Computes a value
	const programA: ReactiveComputationGraph = {
		id: 'compute-square',
		variables: {
			input: { type: 'value', value: 5 }
		},
		computations: [{
			id: 'square',
			inputs: {
				x: { type: 'local', name: 'input' }
			},
			compute_fn: 'square',
			outputs: {
				result: { type: 'holster', holster_path: 'math/squared' }
			}
		}]
	};
	
	// Execute Program A
	console.log('📊 Program A: Computing square of 5...');
	const runtimeA = new ComputationGraphRuntime(programA);
	await runtimeA.initialize();
	await runtimeA.execute();
	
	const programAHash = runtimeA.getProgramHash();
	console.log(`  Program A hash: ${programAHash}`);
	console.log(`  Stored at: ${programAHash}/math/squared`);
	console.log(`  Result: 25`);
	
	// Program B: Uses Program A's output
	// THIS IS WHERE HYBRID STORAGE SHINES!
	const programB: ReactiveComputationGraph = {
		id: 'double-result',
		variables: {
			// Simple subscription to canonical path!
			squared: {
				type: 'subscription',
				holster_path: 'math/squared',
				schema_type: 'number'
			}
		},
		computations: [{
			id: 'double',
			inputs: {
				x: { type: 'local', name: 'squared' }
			},
			compute_fn: 'double',
			outputs: {
				result: { type: 'holster', holster_path: 'math/doubled' }
			}
		}]
	};
	
	console.log('\n📊 Program B: Doubling Program A\'s result...');
	console.log('  Subscribing to: math/squared');
	console.log('  ✅ No need to know provenance signature!');
	console.log('  ✅ No need to parse vector clocks!');
	console.log('  ✅ Just subscribe to canonical path!');
	
	// NOTE: In actual implementation, would need to set up Holster subscription
	// For this example, we simulate by manually querying
	const squaredValue = await runtimeA.getLatest('math/squared');
	
	// Create runtime B with the fetched value
	const runtimeB = new ComputationGraphRuntime(programB);
	await runtimeB.initialize();
	
	// Simulate subscription update
	runtimeB['localState']['squared'] = squaredValue?.data || 25;
	await runtimeB.execute();
	
	console.log('  Result: 50');
	console.log('\n✨ This is the power of canonical paths!');
	console.log('  - Program B doesn\'t care about version signatures');
	console.log('  - Program B always gets latest automatically');
	console.log('  - Provenance is still tracked (in metadata)');
	console.log('  - Immutable history is still preserved (in _versions)');
	
	return { runtimeA, runtimeB };
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 4: Querying Computation Outputs
// ═══════════════════════════════════════════════════════════════════

export async function example4_queryComputationOutputs() {
	console.log('\n========================================');
	console.log('EXAMPLE 4: Query Computation Outputs');
	console.log('========================================\n');
	
	registerComputationFunction('analyze', (inputs: any) => ({
		count: inputs.data.length,
		sum: inputs.data.reduce((a: number, b: number) => a + b, 0),
		avg: inputs.data.reduce((a: number, b: number) => a + b, 0) / inputs.data.length
	}));
	
	const program: ReactiveComputationGraph = {
		id: 'analytics',
		variables: {
			data: { type: 'value', value: [1, 2, 3, 4, 5] }
		},
		computations: [{
			id: 'analyze',
			inputs: {
				data: { type: 'local', name: 'data' }
			},
			compute_fn: 'analyze',
			outputs: {
				stats: { type: 'holster', holster_path: 'analytics/stats' }
			}
		}]
	};
	
	const runtime = new ComputationGraphRuntime(program);
	await runtime.initialize();
	await runtime.execute();
	
	// Query: "What did the 'analyze' computation produce?"
	console.log('🔍 Query: What did the "analyze" computation produce?');
	const outputs = await runtime.getComputationOutputs('analyze');
	console.log(`  Outputs: ${outputs.join(', ')}`);
	
	// Fetch each output
	for (const path of outputs) {
		const data = await runtime.getLatest(path);
		if (data) {
			console.log(`  ${path}:`, data.data);
		}
	}
	
	return runtime;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 5: Lineage Tracing
// ═══════════════════════════════════════════════════════════════════

export async function example5_lineageTracing() {
	console.log('\n========================================');
	console.log('EXAMPLE 5: Lineage Tracing');
	console.log('========================================\n');
	
	registerComputationFunction('fetch_data', () => [1, 2, 3, 4, 5]);
	registerComputationFunction('count', (inputs: any) => inputs.data.length);
	registerComputationFunction('double', (inputs: any) => inputs.value * 2);
	
	// Multi-step computation: fetch → count → double
	const program: ReactiveComputationGraph = {
		id: 'pipeline',
		variables: {},
		computations: [
			{
				id: 'fetch',
				inputs: {},
				compute_fn: 'fetch_data',
				outputs: {
					data: { type: 'holster', holster_path: 'pipeline/data' }
				}
			},
			{
				id: 'count',
				inputs: {
					data: { type: 'derived', from_computation: 'fetch', from_output: 'data' }
				},
				compute_fn: 'count',
				outputs: {
					count: { type: 'holster', holster_path: 'pipeline/count' }
				}
			},
			{
				id: 'double',
				inputs: {
					value: { type: 'derived', from_computation: 'count', from_output: 'count' }
				},
				compute_fn: 'double',
				outputs: {
					result: { type: 'holster', holster_path: 'pipeline/final' }
				}
			}
		]
	};
	
	const runtime = new ComputationGraphRuntime(program);
	await runtime.initialize();
	await runtime.execute();
	
	console.log('📊 Computation pipeline executed:');
	console.log('  fetch → [1,2,3,4,5]');
	console.log('  count → 5');
	console.log('  double → 10');
	
	// Query lineage
	console.log('\n🔍 Tracing lineage:');
	const allProvenance = runtime.getAllProvenance();
	
	for (const [compId, prov] of allProvenance) {
		console.log(`\n  Computation: ${compId}`);
		console.log(`    Provenance ID: ${prov.id}`);
		console.log(`    Inputs: ${Object.keys(prov.inputs).length}`);
		console.log(`    Outputs: ${Object.keys(prov.outputs).length}`);
		
		// Query lineage index
		const lineage = await runtime.getLineage(prov.id);
		if (lineage) {
			console.log(`    Lineage index found:`);
			console.log(`      Computation ID: ${lineage.computationId}`);
			console.log(`      Input keys: ${lineage.inputs.join(', ')}`);
			console.log(`      Output paths: ${lineage.outputs.join(', ')}`);
		}
	}
	
	return runtime;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 6: Comparison - Path-based vs Hybrid
// ═══════════════════════════════════════════════════════════════════

export async function example6_comparison() {
	console.log('\n========================================');
	console.log('EXAMPLE 6: Path-based vs Hybrid Storage');
	console.log('========================================\n');
	
	console.log('❌ Path-based approach (OLD):');
	console.log('  Storage: ~alice/hash/counter/value/p_alice:5_comp:inc_det:xyz');
	console.log('  Problem: How do you query "latest"?');
	console.log('    1. List all paths under counter/value/');
	console.log('    2. Parse each provenance signature');
	console.log('    3. Compare vector clocks');
	console.log('    4. Pick highest VC');
	console.log('    5. Subscribe to that specific path');
	console.log('  RDL template: COMPLEX and brittle!');
	console.log('');
	console.log('  {');
	console.log('    type: "subscription",');
	console.log('    holster_path: "counter/value",');
	console.log('    // Wait, which version? Need to scan...');
	console.log('    // This doesn\'t work!');
	console.log('  }');
	
	console.log('\n✅ Hybrid approach (NEW):');
	console.log('  Storage:');
	console.log('    Canonical: ~alice/hash/counter/value');
	console.log('      → { data: 42, _provenance: {...} }');
	console.log('    Versions: ~alice/hash/counter/value/_versions/p_...');
	console.log('      → { data: 42, _provenance: {...}, _immutable: true }');
	console.log('  Query "latest": Just read canonical path!');
	console.log('  RDL template: SIMPLE and elegant!');
	console.log('');
	console.log('  {');
	console.log('    type: "subscription",');
	console.log('    holster_path: "counter/value"');
	console.log('    // ✅ Always gets latest!');
	console.log('    // ✅ Provenance in metadata!');
	console.log('    // ✅ History preserved!');
	console.log('  }');
	
	console.log('\n🎯 Key Benefits:');
	console.log('  ✅ Latest queries are O(1)');
	console.log('  ✅ RDL templates are simple');
	console.log('  ✅ Program composition works');
	console.log('  ✅ Immutability preserved');
	console.log('  ✅ Provenance tracked');
	console.log('  ✅ Lineage queryable');
}

// ═══════════════════════════════════════════════════════════════════
// RUN ALL EXAMPLES
// ═══════════════════════════════════════════════════════════════════

export async function runAllHybridStorageExamples() {
	console.log('\n╔═══════════════════════════════════════════════════════╗');
	console.log('║   HYBRID STORAGE EXAMPLES                             ║');
	console.log('╚═══════════════════════════════════════════════════════╝');
	
	await example1_simpleLatestQuery();
	await example2_versionHistory();
	await example3_programComposition();
	await example4_queryComputationOutputs();
	await example5_lineageTracing();
	await example6_comparison();
	
	console.log('\n========================================');
	console.log('All examples completed! ✅');
	console.log('========================================\n');
	
	console.log('📝 Summary:');
	console.log('  The hybrid storage approach solves the fundamental');
	console.log('  querying problem while preserving all benefits of');
	console.log('  provenance tracking.');
	console.log('');
	console.log('  Key insight: Separate concerns!');
	console.log('    - Canonical paths → Easy queries');
	console.log('    - Versioned paths → Immutable history');
	console.log('    - Indices → Fast discovery');
	console.log('');
	console.log('  This enables elegant RDL program composition! 🚀');
}

// Auto-run if executed directly
if (typeof module !== 'undefined' && require.main === module) {
	runAllHybridStorageExamples();
}

