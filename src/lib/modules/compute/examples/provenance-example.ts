/**
 * Computation Provenance Examples
 * 
 * Demonstrates how computation provenance tracking works in RDL programs.
 * 
 * What Provenance Tracks:
 * - Input content hashes (what data was used)
 * - Computation hash (what logic was executed)
 * - Output content hashes (what was produced)
 * - Vector clock (peer causality)
 * - Deterministic hash (verification)
 * 
 * This enables:
 * - Verifying computation legitimacy
 * - Tracking data lineage
 * - Reproducing computations
 * - Byzantine fault tolerance
 */

import {
	ComputationGraphRuntime,
	registerComputationFunction
} from '../compute.svelte';
import type { ReactiveComputationGraph, ComputationProvenance } from '../v1/schemas';
import { hashContent, parseProvenanceSignature } from '../program-hash.svelte';

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 1: Basic Provenance Tracking
// ═══════════════════════════════════════════════════════════════════

export async function example1_basicProvenance() {
	console.log('\n========================================');
	console.log('EXAMPLE 1: Basic Provenance Tracking');
	console.log('========================================\n');

	// Register computation functions
	registerComputationFunction('add', (inputs: any) => {
		return inputs.a + inputs.b;
	});

	// Define a simple computation
	const program: ReactiveComputationGraph = {
		id: 'simple-add',
		variables: {
			num1: { type: 'value', value: 5 },
			num2: { type: 'value', value: 3 }
		},
		computations: [
			{
				id: 'sum',
				inputs: {
					a: { type: 'local', name: 'num1' },
					b: { type: 'local', name: 'num2' }
				},
				compute_fn: 'add',
				outputs: {
					result: { type: 'mesh', mesh_path: 'math/sum' }
				}
			}
		]
	};

	// Create runtime (provenance enabled by default)
	const runtime = new ComputationGraphRuntime(program);
	await runtime.initialize();
	await runtime.execute();

	// Get provenance
	const provenance = runtime.getProvenance('sum');
	if (!provenance) {
		console.log('❌ No provenance found');
		return;
	}

	console.log('📋 Provenance Record:');
	console.log(`  Computation ID: ${provenance.computationId}`);
	console.log(`  Executed by: ${provenance.executedBy}`);
	console.log(`  Timestamp: ${new Date(provenance.timestamp).toISOString()}`);
	console.log(`  Program Hash: ${provenance.programHash}`);
	console.log(`  Computation Hash: ${provenance.computationHash}`);
	console.log(`  Deterministic Hash: ${provenance.deterministicHash}`);
	console.log(`  Vector Clock: ${JSON.stringify(provenance.vectorClock)}`);
	console.log('\n  Inputs:');
	for (const [name, input] of Object.entries(provenance.inputs)) {
		console.log(`    ${name}: ${input.contentHash} (${input.source})`);
	}
	console.log('\n  Outputs:');
	for (const [name, output] of Object.entries(provenance.outputs)) {
		console.log(`    ${name}: ${output.contentHash} → ${output.path}`);
	}

	return runtime;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 2: Verifying Computation Results
// ═══════════════════════════════════════════════════════════════════

export async function example2_verification() {
	console.log('\n========================================');
	console.log('EXAMPLE 2: Verifying Computation Results');
	console.log('========================================\n');

	registerComputationFunction('multiply', (inputs: any) => {
		return inputs.x * inputs.y;
	});

	const program: ReactiveComputationGraph = {
		id: 'multiply-program',
		variables: {
			factor1: { type: 'value', value: 7 },
			factor2: { type: 'value', value: 6 }
		},
		computations: [
			{
				id: 'product',
				inputs: {
					x: { type: 'local', name: 'factor1' },
					y: { type: 'local', name: 'factor2' }
				},
				compute_fn: 'multiply',
				outputs: {
					result: { type: 'memory' }
				}
			}
		]
	};

	const runtime = new ComputationGraphRuntime(program);
	await runtime.initialize();
	await runtime.execute();

	const provenance = runtime.getProvenance('product');
	if (!provenance) {
		console.log('❌ No provenance found');
		return;
	}

	// Test 1: Verify correct result
	console.log('✅ Test 1: Verify correct result (42)');
	const validResult = 42;
	const isValid = await runtime.verifyComputationResult('product', validResult, provenance);
	console.log(`   Result: ${isValid ? '✅ VALID' : '❌ INVALID'}\n`);

	// Test 2: Verify incorrect result
	console.log('❌ Test 2: Verify incorrect result (100)');
	const invalidResult = 100;
	const isInvalid = await runtime.verifyComputationResult('product', invalidResult, provenance);
	console.log(`   Result: ${isInvalid ? '✅ VALID' : '❌ INVALID (as expected)'}\n`);

	return runtime;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 3: Content Hash Verification
// ═══════════════════════════════════════════════════════════════════

export async function example3_contentHashing() {
	console.log('\n========================================');
	console.log('EXAMPLE 3: Content Hash Verification');
	console.log('========================================\n');

	// Demonstrate deterministic content hashing
	const data1 = { count: 100, name: 'Alice' };
	const data2 = { count: 100, name: 'Alice' };
	const data3 = { count: 100, name: 'Bob' };

	const hash1 = hashContent(data1);
	const hash2 = hashContent(data2);
	const hash3 = hashContent(data3);

	console.log('Same data → Same hash:');
	console.log(`  data1: ${hash1}`);
	console.log(`  data2: ${hash2}`);
	console.log(`  Match: ${hash1 === hash2 ? '✅ YES' : '❌ NO'}\n`);

	console.log('Different data → Different hash:');
	console.log(`  data1: ${hash1}`);
	console.log(`  data3: ${hash3}`);
	console.log(`  Different: ${hash1 !== hash3 ? '✅ YES' : '❌ NO'}\n`);

	// Demonstrate that key order doesn't matter
	const unordered = { name: 'Alice', count: 100 };
	const ordered = { count: 100, name: 'Alice' };
	const hashUnordered = hashContent(unordered);
	const hashOrdered = hashContent(ordered);

	console.log('Key order doesn\'t affect hash (deterministic):');
	console.log(`  {name, count}: ${hashUnordered}`);
	console.log(`  {count, name}: ${hashOrdered}`);
	console.log(`  Match: ${hashUnordered === hashOrdered ? '✅ YES' : '❌ NO'}\n`);
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 4: Multi-Step Computation Lineage
// ═══════════════════════════════════════════════════════════════════

export async function example4_lineageTracking() {
	console.log('\n========================================');
	console.log('EXAMPLE 4: Multi-Step Computation Lineage');
	console.log('========================================\n');

	registerComputationFunction('square', (inputs: any) => inputs.x * inputs.x);
	registerComputationFunction('double', (inputs: any) => inputs.x * 2);
	registerComputationFunction('add', (inputs: any) => inputs.a + inputs.b);

	// Create a multi-step computation: (5² + 3) * 2
	const program: ReactiveComputationGraph = {
		id: 'multi-step',
		variables: {
			input: { type: 'value', value: 5 },
			constant: { type: 'value', value: 3 }
		},
		computations: [
			{
				id: 'step1-square',
				inputs: {
					x: { type: 'local', name: 'input' }
				},
				compute_fn: 'square',
				outputs: {
					result: { type: 'memory' }
				}
			},
			{
				id: 'step2-add',
				inputs: {
					a: { type: 'derived', from_computation: 'step1-square', from_output: 'result' },
					b: { type: 'local', name: 'constant' }
				},
				compute_fn: 'add',
				outputs: {
					result: { type: 'memory' }
				}
			},
			{
				id: 'step3-double',
				inputs: {
					x: { type: 'derived', from_computation: 'step2-add', from_output: 'result' }
				},
				compute_fn: 'double',
				outputs: {
					result: { type: 'mesh', mesh_path: 'result/final' }
				}
			}
		]
	};

	const runtime = new ComputationGraphRuntime(program);
	await runtime.initialize();
	await runtime.execute();

	// Show provenance for each step
	const allProvenance = runtime.getAllProvenance();

	console.log('Computation Chain:');
	console.log('  5² → 25');
	console.log('  25 + 3 → 28');
	console.log('  28 * 2 → 56\n');

	console.log('Provenance Records:\n');
	for (const [compId, prov] of allProvenance) {
		console.log(`📋 ${compId}:`);
		console.log(`   Deterministic Hash: ${prov.deterministicHash}`);
		console.log(`   Inputs: ${Object.keys(prov.inputs).length}`);
		console.log(`   Outputs: ${Object.keys(prov.outputs).length}`);

		// Show input sources
		for (const [name, input] of Object.entries(prov.inputs)) {
			const source = input.source === 'derived' ? `derived from ${input.provenance || 'parent'}` : input.source;
			console.log(`     Input ${name}: ${source} → ${input.contentHash.substring(0, 8)}...`);
		}
		console.log('');
	}

	return runtime;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 5: Provenance Signature Parsing
// ═══════════════════════════════════════════════════════════════════

export async function example5_signatureParsing() {
	console.log('\n========================================');
	console.log('EXAMPLE 5: Provenance Signature Parsing');
	console.log('========================================\n');

	registerComputationFunction('identity', (inputs: any) => inputs.value);

	const program: ReactiveComputationGraph = {
		id: 'signature-demo',
		variables: {
			data: { type: 'value', value: 'test' }
		},
		computations: [
			{
				id: 'identity-comp',
				inputs: {
					value: { type: 'local', name: 'data' }
				},
				compute_fn: 'identity',
				outputs: {
					result: { type: 'mesh', mesh_path: 'signatures/demo' }
				}
			}
		]
	};

	const runtime = new ComputationGraphRuntime(program);
	await runtime.initialize();
	await runtime.execute();

	const provenance = runtime.getProvenance('identity-comp');
	if (!provenance) {
		console.log('❌ No provenance found');
		return;
	}

	// Show provenance signature structure
	console.log('Provenance Structure:');
	console.log(`  ID: ${provenance.id}`);
	console.log(`  Vector Clock: ${JSON.stringify(provenance.vectorClock)}`);
	console.log(`  Computation ID: ${provenance.computationId}`);
	console.log(`  Deterministic Hash: ${provenance.deterministicHash}`);
	console.log('');

	// This would create a signature like: p_alice:5_comp:identity_det:7a3f2b1c
	console.log('When stored in Mesh, data path includes provenance:');
	console.log(`  ~<pubkey>/<program_hash>/signatures/demo/p_<vc>_comp:<id>_det:<hash>`);
	console.log('');

	// Example: Parse a provenance signature
	const exampleSig = 'p_alice123:5_bob45678:3_comp:identity_det:7a3f2b1c';
	console.log(`Parsing example signature: ${exampleSig}`);
	const parsed = parseProvenanceSignature(exampleSig);
	if (parsed) {
		console.log('  Parsed components:');
		console.log(`    Vector Clock: ${JSON.stringify(parsed.vectorClock)}`);
		console.log(`    Computation ID: ${parsed.computationId}`);
		console.log(`    Deterministic Hash: ${parsed.deterministicHash}`);
	}
	console.log('');

	return runtime;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 6: Disable Provenance (Performance Comparison)
// ═══════════════════════════════════════════════════════════════════

export async function example6_provenanceToggle() {
	console.log('\n========================================');
	console.log('EXAMPLE 6: Provenance Toggle (Performance)');
	console.log('========================================\n');

	registerComputationFunction('heavyCompute', (inputs: any) => {
		// Simulate some computation
		let result = 0;
		for (let i = 0; i < 1000; i++) {
			result += i;
		}
		return result + inputs.value;
	});

	const program: ReactiveComputationGraph = {
		id: 'perf-test',
		variables: {
			input: { type: 'value', value: 100 }
		},
		computations: [
			{
				id: 'compute',
				inputs: {
					value: { type: 'local', name: 'input' }
				},
				compute_fn: 'heavyCompute',
				outputs: {
					result: { type: 'memory' }
				}
			}
		]
	};

	// With provenance
	console.log('⚡ With Provenance Tracking:');
	const startWith = Date.now();
	const runtimeWith = new ComputationGraphRuntime(program, { enableProvenance: true });
	await runtimeWith.initialize();
	await runtimeWith.execute();
	const timeWith = Date.now() - startWith;
	const hasProvenance = runtimeWith.getProvenance('compute') !== undefined;
	console.log(`  Time: ${timeWith}ms`);
	console.log(`  Provenance Created: ${hasProvenance ? '✅ YES' : '❌ NO'}\n`);

	// Without provenance
	console.log('⚡ Without Provenance Tracking:');
	const startWithout = Date.now();
	const runtimeWithout = new ComputationGraphRuntime(program, { enableProvenance: false });
	await runtimeWithout.initialize();
	await runtimeWithout.execute();
	const timeWithout = Date.now() - startWithout;
	const noProvenance = runtimeWithout.getProvenance('compute') === undefined;
	console.log(`  Time: ${timeWithout}ms`);
	console.log(`  Provenance Created: ${noProvenance ? '❌ NO' : '✅ YES'}\n`);

	console.log(`Overhead: ~${timeWith - timeWithout}ms (${((timeWith - timeWithout) / timeWithout * 100).toFixed(1)}%)`);
}

// ═══════════════════════════════════════════════════════════════════
// RUN ALL EXAMPLES
// ═══════════════════════════════════════════════════════════════════

export async function runAllProvenanceExamples() {
	console.log('\n╔═══════════════════════════════════════════════════════╗');
	console.log('║   COMPUTATION PROVENANCE EXAMPLES                     ║');
	console.log('╚═══════════════════════════════════════════════════════╝');

	await example1_basicProvenance();
	await example2_verification();
	await example3_contentHashing();
	await example4_lineageTracking();
	await example5_signatureParsing();
	await example6_provenanceToggle();

	console.log('\n========================================');
	console.log('All examples completed! ✅');
	console.log('========================================\n');
}

// Auto-run if executed directly
if (typeof module !== 'undefined' && require.main === module) {
	runAllProvenanceExamples();
}

