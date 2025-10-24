/**
 * Program Hashing Examples
 * 
 * Demonstrates how RDL programs are automatically namespaced by their hash.
 * All program data is stored at: ~pubkey/<program_hash>/<data_path>
 * 
 * This enables:
 * - Data isolation between programs
 * - Program versioning (different hash = different namespace)
 * - Multiple instances of the same program
 * - Predictable data organization
 */

import type { ReactiveComputationGraph } from '../v1/schemas';
import {
	registerComputationFunction,
	executeComputationGraph,
	ComputationGraphRuntime
} from '../compute.svelte';
import {
	hashProgram,
	getProgramHash,
	verifyProgramHash,
	prefixHolsterPath,
	registerProgram,
	getProgramByHash,
	listRegisteredPrograms
} from '../program-hash.svelte';

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 1: Automatic Program Hashing
// ═══════════════════════════════════════════════════════════════════

/**
 * Define a simple counter program
 * 
 * This program:
 * - Subscribes to a counter at "counter"
 * - Increments it
 * - Stores result back to "counter"
 * 
 * WITHOUT program hashing:
 *   Data would be at: ~pubkey/counter
 * 
 * WITH program hashing:
 *   Data will be at: ~pubkey/<hash>/counter
 */

registerComputationFunction('increment', ({ value }: { value: number }) => {
	return { result: value + 1 };
});

export const counterProgram: ReactiveComputationGraph = {
	id: 'simple-counter',
	
	variables: {
		currentCount: {
			type: 'subscription',
			holster_path: 'counter',  // Will become: <hash>/counter
			schema_type: 'Number',
			default_value: 0
		}
	},
	
	computations: [
		{
			id: 'increment',
			inputs: {
				value: { type: 'subscription', holster_path: 'counter', schema_type: 'Number', default_value: 0 }
			},
			compute_fn: 'increment',
			outputs: {
				result: {
					type: 'holster',
					holster_path: 'counter',  // Will become: <hash>/counter
					schema_type: 'Number'
				}
			}
		}
	]
};

/**
 * Example: Run counter with automatic hashing
 */
export async function runAutoHashedCounter() {
	console.log('=== Automatic Program Hashing Example ===\n');
	
	// Create runtime - program is automatically hashed
	const runtime = new ComputationGraphRuntime(counterProgram);
	const hash = runtime.getProgramHash();
	
	console.log(`Program ID: ${counterProgram.id}`);
	console.log(`Program Hash: ${hash}`);
	console.log(`Counter will be stored at: ~pubkey/${hash}/counter\n`);
	
	await runtime.initialize();
	await runtime.execute();
	
	console.log(`✓ Counter incremented and stored at: ${hash}/counter`);
	
	await runtime.cleanup();
	return hash;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 2: Multiple Program Instances
// ═══════════════════════════════════════════════════════════════════

/**
 * Run multiple instances of the same program
 * Each instance gets the same hash, so they share data!
 */
export async function runMultipleInstances() {
	console.log('=== Multiple Program Instances Example ===\n');
	
	// Instance 1
	const runtime1 = new ComputationGraphRuntime(counterProgram);
	const hash1 = runtime1.getProgramHash();
	console.log(`Instance 1 hash: ${hash1}`);
	
	// Instance 2 - same program structure = same hash
	const runtime2 = new ComputationGraphRuntime(counterProgram);
	const hash2 = runtime2.getProgramHash();
	console.log(`Instance 2 hash: ${hash2}`);
	
	console.log(`\nSame hash? ${hash1 === hash2 ? 'YES ✓' : 'NO ✗'}`);
	console.log('Both instances will read/write the same data!\n');
	
	await runtime1.cleanup();
	await runtime2.cleanup();
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 3: Program Versioning
// ═══════════════════════════════════════════════════════════════════

/**
 * Version 2 of the counter - increments by 2 instead of 1
 * Different logic = different hash = different data namespace
 */

registerComputationFunction('incrementBy2', ({ value }: { value: number }) => {
	return { result: value + 2 };
});

export const counterProgramV2: ReactiveComputationGraph = {
	id: 'simple-counter',  // Same ID...
	
	variables: {
		currentCount: {
			type: 'subscription',
			holster_path: 'counter',
			schema_type: 'Number',
			default_value: 0
		}
	},
	
	computations: [
		{
			id: 'increment',
			inputs: {
				value: { type: 'subscription', holster_path: 'counter', schema_type: 'Number', default_value: 0 }
			},
			compute_fn: 'incrementBy2',  // ...but different function!
			outputs: {
				result: {
					type: 'holster',
					holster_path: 'counter',
					schema_type: 'Number'
				}
			}
		}
	]
};

/**
 * Example: Version isolation
 */
export async function demonstrateVersioning() {
	console.log('=== Program Versioning Example ===\n');
	
	const hash1 = hashProgram(counterProgram);
	const hash2 = hashProgram(counterProgramV2);
	
	console.log(`Counter V1 hash: ${hash1}`);
	console.log(`Counter V2 hash: ${hash2}`);
	console.log(`\nDifferent hash? ${hash1 !== hash2 ? 'YES ✓' : 'NO ✗'}`);
	console.log('\nV1 data is at: ~pubkey/' + hash1 + '/counter');
	console.log('V2 data is at: ~pubkey/' + hash2 + '/counter');
	console.log('\nVersions are completely isolated!\n');
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 4: Manual Hash Override
// ═══════════════════════════════════════════════════════════════════

/**
 * You can manually specify a program hash
 * Useful for:
 * - Custom versioning schemes
 * - Readable/semantic namespace names
 * - Migrating to new hashing algorithms
 */

export const counterProgramWithManualHash: ReactiveComputationGraph = {
	id: 'simple-counter',
	
	// Manual hash override
	program_hash: 'counter-v1.0.0',
	
	variables: {
		currentCount: {
			type: 'subscription',
			holster_path: 'counter',  // Will become: counter-v1.0.0/counter
			schema_type: 'Number',
			default_value: 0
		}
	},
	
	computations: [
		{
			id: 'increment',
			inputs: {
				value: { type: 'subscription', holster_path: 'counter', schema_type: 'Number', default_value: 0 }
			},
			compute_fn: 'increment',
			outputs: {
				result: {
					type: 'holster',
					holster_path: 'counter',  // Will become: counter-v1.0.0/counter
					schema_type: 'Number'
				}
			}
		}
	]
};

/**
 * Example: Manual hash
 */
export async function runWithManualHash() {
	console.log('=== Manual Hash Override Example ===\n');
	
	const runtime = new ComputationGraphRuntime(counterProgramWithManualHash);
	const hash = runtime.getProgramHash();
	
	console.log(`Program ID: ${counterProgramWithManualHash.id}`);
	console.log(`Manual Hash: ${hash}`);
	console.log(`Counter will be stored at: ~pubkey/${hash}/counter`);
	console.log('\nThis allows semantic versioning in the data path!\n');
	
	await runtime.cleanup();
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 5: Program Registry
// ═══════════════════════════════════════════════════════════════════

/**
 * Example: Program registry operations
 */
export function demonstrateRegistry() {
	console.log('=== Program Registry Example ===\n');
	
	// Programs are auto-registered when runtime is created
	// But you can also register manually
	const hash1 = registerProgram(counterProgram);
	const hash2 = registerProgram(counterProgramV2);
	
	console.log('Registered programs:');
	console.log(`- ${counterProgram.id}: ${hash1}`);
	console.log(`- ${counterProgramV2.id}: ${hash2}\n`);
	
	// List all registered programs
	const allHashes = listRegisteredPrograms();
	console.log(`Total registered: ${allHashes.length} programs`);
	
	// Retrieve a program by hash
	const retrieved = getProgramByHash(hash1);
	console.log(`\nRetrieved program: ${retrieved?.id}`);
	
	// Verify hash matches structure
	const isValid = verifyProgramHash(counterProgram);
	console.log(`Hash verification: ${isValid ? 'VALID ✓' : 'INVALID ✗'}\n`);
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 6: Path Prefixing
// ═══════════════════════════════════════════════════════════════════

/**
 * Example: Path manipulation utilities
 */
export function demonstratePathPrefixing() {
	console.log('=== Path Prefixing Example ===\n');
	
	const programHash = 'abc123def456';
	
	// Prefix paths
	console.log('Original path: "tree"');
	console.log(`Prefixed path: "${prefixHolsterPath(programHash, 'tree')}"`);
	console.log();
	
	console.log('Original path: "nodes/node1/data"');
	console.log(`Prefixed path: "${prefixHolsterPath(programHash, 'nodes/node1/data')}"`);
	console.log();
	
	console.log('This prefixing happens automatically for all holster paths!');
	console.log('You define paths as: "tree"');
	console.log(`Runtime uses: "${programHash}/tree"\n`);
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 7: Cross-User Program Data
// ═══════════════════════════════════════════════════════════════════

/**
 * Access another user's program data
 * 
 * Format: ~theirPubkey/<program_hash>/<data_path>
 */

registerComputationFunction('merge', ({ mine, theirs }: any) => {
	return {
		merged: {
			myCount: mine || 0,
			theirCount: theirs || 0,
			total: (mine || 0) + (theirs || 0)
		}
	};
});

export function createCrossUserProgram(theirPubkey: string): ReactiveComputationGraph {
	return {
		id: 'cross-user-counter',
		
		variables: {
			myCount: {
				type: 'subscription',
				holster_path: 'counter',  // My data: ~me/<hash>/counter
				schema_type: 'Number',
				default_value: 0
			},
			theirCount: {
				type: 'subscription',
				holster_path: 'counter',  // Their data: ~them/<hash>/counter
				schema_type: 'Number',
				subscribe_to_user: theirPubkey,  // Cross-user access
				default_value: 0
			}
		},
		
		computations: [
			{
				id: 'merge',
				inputs: {
					mine: { type: 'subscription', holster_path: 'counter', schema_type: 'Number', default_value: 0 },
					theirs: { type: 'subscription', holster_path: 'counter', schema_type: 'Number', subscribe_to_user: theirPubkey, default_value: 0 }
				},
				compute_fn: 'merge',
				outputs: {
					merged: {
						type: 'local',
						state_path: 'mergedCounters'
					}
				}
			}
		]
	};
}

/**
 * Example: Cross-user data access
 */
export async function runCrossUserExample() {
	console.log('=== Cross-User Program Data Example ===\n');
	
	const theirPubkey = 'peer-pubkey-here';
	const program = createCrossUserProgram(theirPubkey);
	
	const runtime = new ComputationGraphRuntime(program);
	const hash = runtime.getProgramHash();
	
	console.log(`Program hash: ${hash}\n`);
	console.log('Data paths:');
	console.log(`  My counter: ~me/${hash}/counter`);
	console.log(`  Their counter: ~${theirPubkey}/${hash}/counter\n`);
	console.log('Both users run the SAME program (same hash)');
	console.log('But each has their own data namespace!\n');
	
	await runtime.cleanup();
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 8: Data Organization Benefits
// ═══════════════════════════════════════════════════════════════════

/**
 * Demonstrate the organizational benefits
 */
export function demonstrateBenefits() {
	console.log('=== Data Organization Benefits ===\n');
	
	console.log('WITHOUT program hashing:');
	console.log('  ~pubkey/tree         ← Which program owns this?');
	console.log('  ~pubkey/counter      ← Is this from app A or B?');
	console.log('  ~pubkey/nodes/node1  ← Name collision risk!');
	console.log();
	
	console.log('WITH program hashing:');
	console.log('  ~pubkey/abc123/tree      ← Program abc123');
	console.log('  ~pubkey/abc123/counter   ← Program abc123');
	console.log('  ~pubkey/def456/tree      ← Program def456 (different tree!)');
	console.log('  ~pubkey/def456/counter   ← Program def456 (different counter!)');
	console.log();
	
	console.log('Benefits:');
	console.log('  ✓ No naming collisions');
	console.log('  ✓ Clear program ownership');
	console.log('  ✓ Version isolation');
	console.log('  ✓ Easy data cleanup (delete whole directory)');
	console.log('  ✓ Multiple program instances share data correctly');
	console.log('  ✓ Predictable data layout\n');
}

// ═══════════════════════════════════════════════════════════════════
// RUN ALL EXAMPLES
// ═══════════════════════════════════════════════════════════════════

/**
 * Run all examples
 */
export async function runAllProgramHashExamples() {
	await runAutoHashedCounter();
	console.log('\n' + '='.repeat(70) + '\n');
	
	await runMultipleInstances();
	console.log('\n' + '='.repeat(70) + '\n');
	
	await demonstrateVersioning();
	console.log('='.repeat(70) + '\n');
	
	await runWithManualHash();
	console.log('='.repeat(70) + '\n');
	
	demonstrateRegistry();
	console.log('='.repeat(70) + '\n');
	
	demonstratePathPrefixing();
	console.log('='.repeat(70) + '\n');
	
	await runCrossUserExample();
	console.log('='.repeat(70) + '\n');
	
	demonstrateBenefits();
}

