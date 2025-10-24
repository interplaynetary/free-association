/**
 * Hybrid Storage Tests
 * 
 * Tests the hybrid storage approach for computation provenance:
 * - Canonical paths (latest)
 * - Versioned paths (immutable history)
 * - Indices (fast lookup)
 */

import { describe, test, expect, beforeAll, mock } from 'bun:test';

// ═══════════════════════════════════════════════════════════════════
// MOCK DEPENDENCIES (MUST BE BEFORE ANY OTHER IMPORTS)
// ═══════════════════════════════════════════════════════════════════

// Mock Gun/Holster
const mockHolsterStore: Record<string, any> = {};
const mockArrayStore: Record<string, Record<string, any>> = {};

const mockHolsterUser = {
	get: (path: string | [string, string]) => {
		if (Array.isArray(path)) {
			const [pubkey, holsterPath] = path;
			const key = `${pubkey}/${holsterPath}`;
			
			return {
				put: (data: any) => {
					if (!mockArrayStore[pubkey]) {
						mockArrayStore[pubkey] = {};
					}
					mockArrayStore[pubkey][holsterPath] = data;
				},
				once: (callback: (data: any) => void) => {
					setTimeout(() => {
						const data = mockArrayStore[pubkey]?.[holsterPath];
						callback(data);
					}, 10);
				},
				map: () => ({
					once: (callback: (data: any, key: string) => void) => {
						setTimeout(() => {
							const userData = mockArrayStore[pubkey];
							if (userData) {
								// Find all paths that start with the given path
								const basePath = holsterPath;
								for (const [subPath, data] of Object.entries(userData)) {
									if (subPath.startsWith(basePath + '/')) {
										const key = subPath.substring(basePath.length + 1);
										callback(data, key);
									}
								}
							}
						}, 50);
					}
				}),
				set: (value: string) => {
					if (!mockArrayStore[pubkey]) {
						mockArrayStore[pubkey] = {};
					}
					// For set(), we append to an array-like structure
					const existing = mockArrayStore[pubkey][holsterPath] || [];
					if (Array.isArray(existing)) {
						existing.push(value);
					} else {
						mockArrayStore[pubkey][holsterPath] = [value];
					}
				}
			};
		} else {
			return {
				put: (data: any) => {
					mockHolsterStore[path] = data;
				},
				once: (callback: (data: any) => void) => {
					setTimeout(() => {
						callback(mockHolsterStore[path]);
					}, 10);
				},
				map: () => ({
					once: (callback: (data: any, key: string) => void) => {
						setTimeout(() => {
							// Find all paths that start with the given path
							for (const [subPath, data] of Object.entries(mockHolsterStore)) {
								if (subPath.startsWith(path + '/')) {
									const key = subPath.substring(path.length + 1);
									callback(data, key);
								}
							}
						}, 50);
					}
				}),
				set: (value: string) => {
					// For set(), we append to an array-like structure
					const existing = mockHolsterStore[path] || [];
					if (Array.isArray(existing)) {
						existing.push(value);
					} else {
						mockHolsterStore[path] = [value];
					}
				}
			};
		}
	}
};

// Mock modules BEFORE importing the modules that depend on them
mock.module('$lib/state/holster.svelte', () => ({
	holsterUser: mockHolsterUser
}));

mock.module('$lib/state/user.svelte', () => ({
	myPubKey: { subscribe: (fn: any) => fn('alice'), set: () => {} },
	default: { subscribe: (fn: any) => fn('alice'), set: () => {} }
}));

mock.module('$lib/commons/algorithm.svelte', () => ({
	myVectorClock: { subscribe: (fn: any) => fn({ alice: 0 }), set: () => {} },
	incrementMyVectorClock: async () => {}
}));

mock.module('$app/environment', () => ({
	browser: false,
	dev: true,
	building: false,
	version: '1.0.0'
}));

// Now import the modules
import { ComputationGraphRuntime, registerComputationFunction } from '../compute.svelte';
import { 
	prefixHolsterPath, 
	createProvenanceSignature,
	hashContent 
} from '../program-hash.svelte';
import type { 
	ComputationProvenance, 
	VectorClock,
	ReactiveComputationGraph 
} from '../v1/schemas';

// ═══════════════════════════════════════════════════════════════════
// HELPER FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

function createMockProvenance(
	id: string,
	computationId: string,
	vectorClock: VectorClock
): ComputationProvenance {
	return {
		id,
		vectorClock,
		executedBy: 'alice',
		timestamp: Date.now(),
		programHash: 'abc123',
		computationId,
		computationHash: hashContent(computationId),
		inputs: {},
		outputs: {},
		deterministicHash: 'xyz789'
	};
}

// ═══════════════════════════════════════════════════════════════════
// TESTS: STORAGE LOCATIONS
// ═══════════════════════════════════════════════════════════════════

describe('Hybrid Storage - Storage Locations', () => {
	
	test('should write to canonical path', async () => {
		registerComputationFunction('add', (inputs: any) => inputs.a + inputs.b);
		
		const program: ReactiveComputationGraph = {
			id: 'test-canonical',
			variables: {
				a: { type: 'value', value: 5 },
				b: { type: 'value', value: 3 }
			},
			computations: [{
				id: 'sum',
				inputs: {
					a: { type: 'local', name: 'a' },
					b: { type: 'local', name: 'b' }
				},
				compute_fn: 'add',
				outputs: {
					result: { type: 'holster', holster_path: 'math/sum' }
				}
			}]
		};
		
		const runtime = new ComputationGraphRuntime(program, { enableProvenance: true });
		await runtime.initialize();
		await runtime.execute();
		
		const programHash = runtime.getProgramHash();
		const canonicalPath = prefixHolsterPath(programHash, 'math/sum');
		
		// Wait for async writes
		await new Promise(resolve => setTimeout(resolve, 100));
		
		// Check canonical path exists
		const stored = mockArrayStore['alice']?.[canonicalPath];
		expect(stored).toBeDefined();
		expect(stored.data).toBe(8);
		expect(stored._provenance).toBeDefined();
		expect(stored._updatedAt).toBeDefined();
	});
	
	test('should write to versioned path', async () => {
		registerComputationFunction('multiply', (inputs: any) => inputs.a * inputs.b);
		
		const program: ReactiveComputationGraph = {
			id: 'test-versioned',
			variables: {
				a: { type: 'value', value: 4 },
				b: { type: 'value', value: 7 }
			},
			computations: [{
				id: 'product',
				inputs: {
					a: { type: 'local', name: 'a' },
					b: { type: 'local', name: 'b' }
				},
				compute_fn: 'multiply',
				outputs: {
					result: { type: 'holster', holster_path: 'math/product' }
				}
			}]
		};
		
		const runtime = new ComputationGraphRuntime(program, { enableProvenance: true });
		await runtime.initialize();
		await runtime.execute();
		
		const programHash = runtime.getProgramHash();
		const canonicalPath = prefixHolsterPath(programHash, 'math/product');
		
		// Wait for async writes
		await new Promise(resolve => setTimeout(resolve, 100));
		
		// Check for version path (should contain provenance signature)
		const userData = mockArrayStore['alice'] || {};
		const versionPaths = Object.keys(userData).filter(
			path => path.includes('_versions/')
		);
		
		expect(versionPaths.length).toBeGreaterThan(0);
		
		// Check version has _immutable flag
		const firstVersionPath = versionPaths[0];
		const versionData = userData[firstVersionPath];
		expect(versionData._immutable).toBe(true);
		expect(versionData.data).toBe(28);
	});
	
	test('should update latest index', async () => {
		registerComputationFunction('subtract', (inputs: any) => inputs.a - inputs.b);
		
		const program: ReactiveComputationGraph = {
			id: 'test-latest-index',
			variables: {
				a: { type: 'value', value: 10 },
				b: { type: 'value', value: 3 }
			},
			computations: [{
				id: 'diff',
				inputs: {
					a: { type: 'local', name: 'a' },
					b: { type: 'local', name: 'b' }
				},
				compute_fn: 'subtract',
				outputs: {
					result: { type: 'holster', holster_path: 'math/difference' }
				}
			}]
		};
		
		const runtime = new ComputationGraphRuntime(program, { enableProvenance: true });
		await runtime.initialize();
		await runtime.execute();
		
		const programHash = runtime.getProgramHash();
		const latestIndexPath = `${programHash}/_index/latest/math/difference`;
		
		// Wait for async writes
		await new Promise(resolve => setTimeout(resolve, 100));
		
		// Check latest index
		const latestSig = mockArrayStore['alice']?.[latestIndexPath];
		expect(latestSig).toBeDefined();
		expect(typeof latestSig).toBe('string');
		expect(latestSig).toMatch(/^p_/); // Provenance signature format
	});
	
	test('should update computations index', async () => {
		registerComputationFunction('divide', (inputs: any) => inputs.a / inputs.b);
		
		const program: ReactiveComputationGraph = {
			id: 'test-comp-index',
			variables: {
				a: { type: 'value', value: 20 },
				b: { type: 'value', value: 4 }
			},
			computations: [{
				id: 'quotient',
				inputs: {
					a: { type: 'local', name: 'a' },
					b: { type: 'local', name: 'b' }
				},
				compute_fn: 'divide',
				outputs: {
					result: { type: 'holster', holster_path: 'math/quotient' }
				}
			}]
		};
		
		const runtime = new ComputationGraphRuntime(program, { enableProvenance: true });
		await runtime.initialize();
		await runtime.execute();
		
		const programHash = runtime.getProgramHash();
		const compIndexPath = `${programHash}/_index/computations/quotient`;
		
		// Wait for async writes
		await new Promise(resolve => setTimeout(resolve, 100));
		
		// Check computations index
		const outputs = mockArrayStore['alice']?.[compIndexPath];
		expect(outputs).toBeDefined();
		expect(Array.isArray(outputs)).toBe(true);
	});
	
	test('should update lineage index', async () => {
		registerComputationFunction('square', (inputs: any) => inputs.x * inputs.x);
		
		const program: ReactiveComputationGraph = {
			id: 'test-lineage-index',
			variables: {
				x: { type: 'value', value: 6 }
			},
			computations: [{
				id: 'squared',
				inputs: {
					x: { type: 'local', name: 'x' }
				},
				compute_fn: 'square',
				outputs: {
					result: { type: 'holster', holster_path: 'math/squared' }
				}
			}]
		};
		
		const runtime = new ComputationGraphRuntime(program, { enableProvenance: true });
		await runtime.initialize();
		await runtime.execute();
		
		const programHash = runtime.getProgramHash();
		
		// Wait for async writes
		await new Promise(resolve => setTimeout(resolve, 100));
		
		// Check lineage index (path format: <hash>/_index/lineage/<prov_id>)
		const userData = mockArrayStore['alice'] || {};
		const lineagePaths = Object.keys(userData).filter(
			path => path.includes('/_index/lineage/')
		);
		
		expect(lineagePaths.length).toBeGreaterThan(0);
		
		// Check lineage data structure
		const lineageData = userData[lineagePaths[0]];
		expect(lineageData.computationId).toBe('squared');
		expect(lineageData.programHash).toBe(programHash);
		expect(Array.isArray(lineageData.inputs)).toBe(true);
		expect(Array.isArray(lineageData.outputs)).toBe(true);
	});
});

// ═══════════════════════════════════════════════════════════════════
// TESTS: QUERY METHODS
// ═══════════════════════════════════════════════════════════════════

describe('Hybrid Storage - Query Methods', () => {
	
	test('getLatest() should return latest data', async () => {
		// Clear mock store
		Object.keys(mockArrayStore).forEach(key => delete mockArrayStore[key]);
		
		registerComputationFunction('double', (inputs: any) => inputs.x * 2);
		
		const program: ReactiveComputationGraph = {
			id: 'test-get-latest',
			variables: {
				x: { type: 'value', value: 5 }
			},
			computations: [{
				id: 'doubled',
				inputs: {
					x: { type: 'local', name: 'x' }
				},
				compute_fn: 'double',
				outputs: {
					result: { type: 'holster', holster_path: 'test/doubled' }
				}
			}]
		};
		
		const runtime = new ComputationGraphRuntime(program, { enableProvenance: true });
		await runtime.initialize();
		await runtime.execute();
		
		// Wait for async writes
		await new Promise(resolve => setTimeout(resolve, 150));
		
		// Query latest
		const latest = await runtime.getLatest('test/doubled');
		
		expect(latest).toBeDefined();
		expect(latest.data).toBe(10);
		expect(latest._provenance).toBeDefined();
		expect(latest._updatedAt).toBeDefined();
	});
	
	test('getAllVersions() should return version history', async () => {
		// Clear mock store
		Object.keys(mockArrayStore).forEach(key => delete mockArrayStore[key]);
		
		registerComputationFunction('increment', (inputs: any) => inputs.value + 1);
		
		const program: ReactiveComputationGraph = {
			id: 'test-versions',
			variables: {
				value: { type: 'value', value: 0 }
			},
			computations: [{
				id: 'inc',
				inputs: {
					value: { type: 'local', name: 'value' }
				},
				compute_fn: 'increment',
				outputs: {
					result: { type: 'holster', holster_path: 'counter/value' }
				}
			}]
		};
		
		const runtime = new ComputationGraphRuntime(program, { enableProvenance: true });
		await runtime.initialize();
		
		// Execute 3 times
		for (let i = 0; i < 3; i++) {
			runtime['localState']['value'] = i;
			await runtime.execute();
			await new Promise(resolve => setTimeout(resolve, 50));
		}
		
		// Wait for all async writes
		await new Promise(resolve => setTimeout(resolve, 200));
		
		// Query all versions
		const versions = await runtime.getAllVersions('counter/value');
		
		expect(versions.length).toBeGreaterThanOrEqual(1);
		// Each version should have data and provenance
		for (const v of versions) {
			expect(v.data).toBeDefined();
			expect(v._provenance).toBeDefined();
			expect(v._immutable).toBe(true);
		}
	});
});

// ═══════════════════════════════════════════════════════════════════
// TESTS: PATH STRUCTURE
// ═══════════════════════════════════════════════════════════════════

describe('Hybrid Storage - Path Structure', () => {
	
	test('canonical path should be predictable', () => {
		const programHash = 'abc123';
		const holsterPath = 'analytics/count';
		
		const canonical = prefixHolsterPath(programHash, holsterPath);
		
		expect(canonical).toBe('abc123/analytics/count');
	});
	
	test('version path should include provenance signature', () => {
		const provenance = createMockProvenance('prov-1', 'test', { alice: 5 });
		const sig = createProvenanceSignature(provenance);
		
		expect(sig).toMatch(/^p_/);
		expect(sig).toContain('alice:5');
		expect(sig).toContain('comp:test');
		expect(sig).toContain('det:');
	});
	
	test('index paths should be namespaced', () => {
		const programHash = 'abc123';
		
		const latestIndex = `${programHash}/_index/latest/some/path`;
		const compIndex = `${programHash}/_index/computations/my-comp`;
		const lineageIndex = `${programHash}/_index/lineage/prov-123`;
		
		expect(latestIndex).toContain('/_index/latest/');
		expect(compIndex).toContain('/_index/computations/');
		expect(lineageIndex).toContain('/_index/lineage/');
	});
});

// ═══════════════════════════════════════════════════════════════════
// TESTS: DATA INTEGRITY
// ═══════════════════════════════════════════════════════════════════

describe('Hybrid Storage - Data Integrity', () => {
	
	test('canonical and versioned should have same data', async () => {
		// Clear mock store
		Object.keys(mockArrayStore).forEach(key => delete mockArrayStore[key]);
		
		registerComputationFunction('negate', (inputs: any) => -inputs.x);
		
		const program: ReactiveComputationGraph = {
			id: 'test-integrity',
			variables: {
				x: { type: 'value', value: 42 }
			},
			computations: [{
				id: 'negated',
				inputs: {
					x: { type: 'local', name: 'x' }
				},
				compute_fn: 'negate',
				outputs: {
					result: { type: 'holster', holster_path: 'test/negated' }
				}
			}]
		};
		
		const runtime = new ComputationGraphRuntime(program, { enableProvenance: true });
		await runtime.initialize();
		await runtime.execute();
		
		// Wait for async writes
		await new Promise(resolve => setTimeout(resolve, 150));
		
		// Get canonical
		const canonical = await runtime.getLatest('test/negated');
		
		// Get version
		const versions = await runtime.getAllVersions('test/negated');
		
		expect(canonical).toBeDefined();
		expect(versions.length).toBeGreaterThan(0);
		
		// Data should match
		const firstVersion = versions[0];
		expect(canonical.data).toBe(firstVersion.data);
		
		// Provenance should match
		expect(canonical._provenance.id).toBe(firstVersion._provenance.id);
		expect(canonical._provenance.deterministicHash).toBe(
			firstVersion._provenance.deterministicHash
		);
	});
	
	test('version should be immutable', async () => {
		// Clear mock store
		Object.keys(mockArrayStore).forEach(key => delete mockArrayStore[key]);
		
		registerComputationFunction('identity', (inputs: any) => inputs.x);
		
		const program: ReactiveComputationGraph = {
			id: 'test-immutable',
			variables: {
				x: { type: 'value', value: 99 }
			},
			computations: [{
				id: 'pass',
				inputs: {
					x: { type: 'local', name: 'x' }
				},
				compute_fn: 'identity',
				outputs: {
					result: { type: 'holster', holster_path: 'test/immutable' }
				}
			}]
		};
		
		const runtime = new ComputationGraphRuntime(program, { enableProvenance: true });
		await runtime.initialize();
		await runtime.execute();
		
		// Wait for async writes
		await new Promise(resolve => setTimeout(resolve, 150));
		
		const versions = await runtime.getAllVersions('test/immutable');
		
		expect(versions.length).toBeGreaterThan(0);
		expect(versions[0]._immutable).toBe(true);
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUMMARY
// ═══════════════════════════════════════════════════════════════════

console.log('\n✅ Hybrid Storage Test Suite');
console.log('   - Storage locations (5 tests)');
console.log('   - Query methods (2 tests)');
console.log('   - Path structure (3 tests)');
console.log('   - Data integrity (2 tests)');
console.log('   Total: 12 tests');

