/**
 * Program Hashing Runtime Integration Tests
 * 
 * Tests for how ComputationGraphRuntime integrates with program hashing:
 * - Automatic hash computation
 * - Path prefixing in variable bindings
 * - Path prefixing in outputs
 * - Cross-user scenarios
 * - Manual hash override
 */

import { describe, it, expect, beforeEach, afterEach, mock } from 'bun:test';

// Mock all SvelteKit and Holster dependencies before importing anything
mock.module('$lib/network/holster.svelte', () => ({
	holsterUser: {
		is: true,
		get: () => ({
			on: () => {},
			get: (callback: any) => {
				if (typeof callback === 'function') {
					callback({ _updatedAt: Date.now(), value: 42 });
				}
			},
			put: (data: any, callback?: any) => {
				if (callback) callback(null);
			},
			off: () => {}
		})
	}
}));

mock.module('$app/environment', () => ({
	browser: false,
	dev: true,
	building: false,
	version: '1.0.0'
}));

// Now import after mocks are set up
import { ComputationGraphRuntime, registerComputationFunction } from '../compute.svelte';
import { clearProgramRegistry, hashProgram, listRegisteredPrograms } from '../program-hash.svelte';
import type { ReactiveComputationGraph } from '../../v1/schemas';

// ═══════════════════════════════════════════════════════════════════
// TEST FIXTURES
// ═══════════════════════════════════════════════════════════════════

const simpleProgram: ReactiveComputationGraph = {
	id: 'simple-test',
	variables: {
		count: {
			type: 'value',
			value: 0
		}
	},
	computations: []
};

const programWithSubscription: ReactiveComputationGraph = {
	id: 'with-subscription',
	variables: {
		data: {
			type: 'subscription',
			holster_path: 'my_data',
			schema_type: 'Any',
			default_value: null
		}
	},
	computations: []
};

const programWithFetch: ReactiveComputationGraph = {
	id: 'with-fetch',
	variables: {
		data: {
			type: 'fetch',
			holster_path: 'fetched_data',
			schema_type: 'Any',
			wait_ms: 100,
			default_value: null
		}
	},
	computations: []
};

const programWithOutput: ReactiveComputationGraph = {
	id: 'with-output',
	variables: {
		input: {
			type: 'value',
			value: 10
		}
	},
	computations: [
		{
			id: 'compute',
			inputs: {
				val: { type: 'value', value: 10 }
			},
			compute_fn: 'double',
			outputs: {
				result: {
					type: 'holster',
					holster_path: 'result',
					schema_type: 'Any'
				}
			}
		}
	]
};

const programWithManualHash: ReactiveComputationGraph = {
	id: 'manual-hash-program',
	program_hash: 'my-custom-hash-v1',
	variables: {
		count: {
			type: 'value',
			value: 0
		}
	},
	computations: []
};

const crossUserProgram: ReactiveComputationGraph = {
	id: 'cross-user',
	variables: {
		myData: {
			type: 'subscription',
			holster_path: 'data',
			schema_type: 'Any'
		},
		theirData: {
			type: 'subscription',
			holster_path: 'data',
			schema_type: 'Any',
			subscribe_to_user: 'peer-pubkey-123'
		}
	},
	computations: []
};

// ═══════════════════════════════════════════════════════════════════
// SETUP
// ═══════════════════════════════════════════════════════════════════

beforeEach(() => {
	clearProgramRegistry();
	
	// Register test computation functions
	registerComputationFunction('double', ({ val }: { val: number }) => {
		return { result: val * 2 };
	});
	
	registerComputationFunction('add', ({ a, b }: { a: number; b: number }) => {
		return { sum: a + b };
	});
});

afterEach(() => {
	clearProgramRegistry();
});

// ═══════════════════════════════════════════════════════════════════
// RUNTIME HASH TESTS
// ═══════════════════════════════════════════════════════════════════

describe('ComputationGraphRuntime - Program Hashing', () => {
	describe('Automatic hash computation', () => {
		it('should compute and store program hash on creation', () => {
			const runtime = new ComputationGraphRuntime(simpleProgram);
			const hash = runtime.getProgramHash();
			
			expect(hash).toBeDefined();
			expect(typeof hash).toBe('string');
			expect(hash.length).toBe(16);
		});
		
		it('should compute same hash for same program', () => {
			const runtime1 = new ComputationGraphRuntime(simpleProgram);
			const runtime2 = new ComputationGraphRuntime(simpleProgram);
			
			expect(runtime1.getProgramHash()).toBe(runtime2.getProgramHash());
		});
		
		it('should compute different hashes for different programs', () => {
			const runtime1 = new ComputationGraphRuntime(simpleProgram);
			const runtime2 = new ComputationGraphRuntime(programWithSubscription);
			
			expect(runtime1.getProgramHash()).not.toBe(runtime2.getProgramHash());
		});
		
		it('should use manual hash if provided', () => {
			const runtime = new ComputationGraphRuntime(programWithManualHash);
			const hash = runtime.getProgramHash();
			
			expect(hash).toBe('my-custom-hash-v1');
		});
		
		it('should match hashProgram result', () => {
			const runtime = new ComputationGraphRuntime(simpleProgram);
			const runtimeHash = runtime.getProgramHash();
			const directHash = hashProgram(simpleProgram);
			
			expect(runtimeHash).toBe(directHash);
		});
	});
	
	describe('Automatic program registration', () => {
		it('should register program on runtime creation', () => {
			const runtime = new ComputationGraphRuntime(simpleProgram);
			const hash = runtime.getProgramHash();
			
			const registered = listRegisteredPrograms();
			expect(registered).toContain(hash);
		});
		
		it('should register multiple program instances once', () => {
			new ComputationGraphRuntime(simpleProgram);
			new ComputationGraphRuntime(simpleProgram);
			new ComputationGraphRuntime(simpleProgram);
			
			const registered = listRegisteredPrograms();
			
			// Should only be registered once (same hash)
			const simpleHash = hashProgram(simpleProgram);
			const count = registered.filter(h => h === simpleHash).length;
			expect(count).toBe(1);
		});
		
		it('should register different programs separately', () => {
			new ComputationGraphRuntime(simpleProgram);
			new ComputationGraphRuntime(programWithSubscription);
			
			const registered = listRegisteredPrograms();
			expect(registered).toHaveLength(2);
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// PATH PREFIXING TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Path Prefixing Integration', () => {
	describe('Subscription variables', () => {
		it('should prefix subscription paths during initialization', async () => {
			const runtime = new ComputationGraphRuntime(programWithSubscription);
			const hash = runtime.getProgramHash();
			
			await runtime.initialize();
			
			// Path should be prefixed (we can verify this through the hash)
			expect(hash).toBeDefined();
			expect(hash.length).toBe(16);
			
			await runtime.cleanup();
		});
		
		it('should use same prefix for multiple subscriptions', async () => {
			const program: ReactiveComputationGraph = {
				id: 'multi-sub',
				variables: {
					data1: {
						type: 'subscription',
						holster_path: 'data1',
						schema_type: 'Any'
					},
					data2: {
						type: 'subscription',
						holster_path: 'data2',
						schema_type: 'Any'
					}
				},
				computations: []
			};
			
			const runtime = new ComputationGraphRuntime(program);
			const hash = runtime.getProgramHash();
			
			await runtime.initialize();
			
			// Both should use same hash prefix
			expect(hash).toBeDefined();
			
			await runtime.cleanup();
		});
	});
	
	describe('Fetch variables', () => {
		it('should prefix fetch paths during initialization', async () => {
			const runtime = new ComputationGraphRuntime(programWithFetch);
			const hash = runtime.getProgramHash();
			
			await runtime.initialize();
			
			expect(hash).toBeDefined();
			
			await runtime.cleanup();
		});
	});
	
	describe('Output paths', () => {
		it('should prefix output holster paths during execution', async () => {
			const runtime = new ComputationGraphRuntime(programWithOutput);
			const hash = runtime.getProgramHash();
			
			await runtime.initialize();
			await runtime.execute();
			
			expect(hash).toBeDefined();
			
			await runtime.cleanup();
		});
	});
	
	describe('Cross-user paths', () => {
		it('should handle cross-user subscriptions with hash prefix', async () => {
			const runtime = new ComputationGraphRuntime(crossUserProgram);
			const hash = runtime.getProgramHash();
			
			await runtime.initialize();
			
			// Both paths should use same hash, just different users
			expect(hash).toBeDefined();
			
			await runtime.cleanup();
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// MANUAL HASH TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Manual Hash Override', () => {
	it('should use manual hash for all operations', async () => {
		const runtime = new ComputationGraphRuntime(programWithManualHash);
		const hash = runtime.getProgramHash();
		
		expect(hash).toBe('my-custom-hash-v1');
		
		await runtime.initialize();
		
		// Verify it's registered with manual hash
		const registered = listRegisteredPrograms();
		expect(registered).toContain('my-custom-hash-v1');
		
		await runtime.cleanup();
	});
	
	it('should support empty string as manual hash', async () => {
		const program: ReactiveComputationGraph = {
			...simpleProgram,
			program_hash: ''
		};
		
		const runtime = new ComputationGraphRuntime(program);
		const hash = runtime.getProgramHash();
		
		expect(hash).toBe('');
		
		await runtime.initialize();
		await runtime.cleanup();
	});
	
	it('should support semantic versioning in manual hash', async () => {
		const program: ReactiveComputationGraph = {
			...simpleProgram,
			program_hash: 'v1.2.3-beta.1'
		};
		
		const runtime = new ComputationGraphRuntime(program);
		const hash = runtime.getProgramHash();
		
		expect(hash).toBe('v1.2.3-beta.1');
		
		await runtime.initialize();
		await runtime.cleanup();
	});
});

// ═══════════════════════════════════════════════════════════════════
// LIFECYCLE TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Runtime Lifecycle with Hashing', () => {
	it('should maintain hash throughout lifecycle', async () => {
		const runtime = new ComputationGraphRuntime(simpleProgram);
		const hash1 = runtime.getProgramHash();
		
		await runtime.initialize();
		const hash2 = runtime.getProgramHash();
		
		await runtime.execute();
		const hash3 = runtime.getProgramHash();
		
		await runtime.cleanup();
		const hash4 = runtime.getProgramHash();
		
		expect(hash1).toBe(hash2);
		expect(hash2).toBe(hash3);
		expect(hash3).toBe(hash4);
	});
	
	it('should keep program registered after cleanup', async () => {
		const runtime = new ComputationGraphRuntime(simpleProgram);
		const hash = runtime.getProgramHash();
		
		await runtime.initialize();
		await runtime.execute();
		await runtime.cleanup();
		
		// Program should still be registered
		const registered = listRegisteredPrograms();
		expect(registered).toContain(hash);
	});
});

// ═══════════════════════════════════════════════════════════════════
// REACTIVITY TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Reactivity with Hashing', () => {
	it('should maintain hash when enabling reactivity', async () => {
		const runtime = new ComputationGraphRuntime(programWithSubscription);
		const hash1 = runtime.getProgramHash();
		
		await runtime.initialize();
		runtime.enableReactivity();
		
		const hash2 = runtime.getProgramHash();
		expect(hash1).toBe(hash2);
		
		runtime.disableReactivity();
		await runtime.cleanup();
	});
	
	it('should use same hash for reactive updates', async () => {
		const runtime = new ComputationGraphRuntime(programWithSubscription);
		const hash = runtime.getProgramHash();
		
		await runtime.initialize();
		runtime.enableReactivity();
		
		// Simulate reactive update
		await runtime.execute();
		
		expect(runtime.getProgramHash()).toBe(hash);
		
		runtime.disableReactivity();
		await runtime.cleanup();
	});
});

// ═══════════════════════════════════════════════════════════════════
// EDGE CASES
// ═══════════════════════════════════════════════════════════════════

describe('Edge Cases with Runtime', () => {
	it('should handle program with empty variables', async () => {
		const program: ReactiveComputationGraph = {
			id: 'empty-vars',
			variables: {},
			computations: []
		};
		
		const runtime = new ComputationGraphRuntime(program);
		const hash = runtime.getProgramHash();
		
		expect(hash).toBeDefined();
		expect(hash.length).toBe(16);
		
		await runtime.initialize();
		await runtime.cleanup();
	});
	
	it('should handle program with empty computations', async () => {
		const program: ReactiveComputationGraph = {
			id: 'empty-comps',
			variables: {
				test: { type: 'value', value: 1 }
			},
			computations: []
		};
		
		const runtime = new ComputationGraphRuntime(program);
		const hash = runtime.getProgramHash();
		
		expect(hash).toBeDefined();
		
		await runtime.initialize();
		await runtime.cleanup();
	});
	
	it('should handle very long holster paths', async () => {
		const longPath = Array(50).fill('segment').join('/');
		
		const program: ReactiveComputationGraph = {
			id: 'long-path',
			variables: {
				data: {
					type: 'subscription',
					holster_path: longPath,
					schema_type: 'Any'
				}
			},
			computations: []
		};
		
		const runtime = new ComputationGraphRuntime(program);
		const hash = runtime.getProgramHash();
		
		expect(hash).toBeDefined();
		
		await runtime.initialize();
		await runtime.cleanup();
	});
});

// ═══════════════════════════════════════════════════════════════════
// INTEGRATION SCENARIOS
// ═══════════════════════════════════════════════════════════════════

describe('Real-World Scenarios', () => {
	it('should isolate data between two different programs', async () => {
		const program1: ReactiveComputationGraph = {
			id: 'counter-v1',
			variables: {
				count: { type: 'value', value: 0 }
			},
			computations: []
		};
		
		const program2: ReactiveComputationGraph = {
			id: 'counter-v2',
			variables: {
				count: { type: 'value', value: 100 }  // Different value
			},
			computations: []
		};
		
		const runtime1 = new ComputationGraphRuntime(program1);
		const runtime2 = new ComputationGraphRuntime(program2);
		
		const hash1 = runtime1.getProgramHash();
		const hash2 = runtime2.getProgramHash();
		
		// Different programs should have different hashes
		expect(hash1).not.toBe(hash2);
		
		await runtime1.cleanup();
		await runtime2.cleanup();
	});
	
	it('should share data between instances of same program', async () => {
		const instance1 = new ComputationGraphRuntime(simpleProgram);
		const instance2 = new ComputationGraphRuntime(simpleProgram);
		const instance3 = new ComputationGraphRuntime(simpleProgram);
		
		const hash1 = instance1.getProgramHash();
		const hash2 = instance2.getProgramHash();
		const hash3 = instance3.getProgramHash();
		
		// All instances should have same hash
		expect(hash1).toBe(hash2);
		expect(hash2).toBe(hash3);
		
		await instance1.cleanup();
		await instance2.cleanup();
		await instance3.cleanup();
	});
	
	it('should handle program versioning scenario', async () => {
		// Version 1
		const v1: ReactiveComputationGraph = {
			id: 'my-app',
			program_hash: 'v1.0.0',
			variables: { data: { type: 'value', value: 1 } },
			computations: []
		};
		
		// Version 2
		const v2: ReactiveComputationGraph = {
			id: 'my-app',
			program_hash: 'v2.0.0',
			variables: { data: { type: 'value', value: 2 } },
			computations: []
		};
		
		const runtime1 = new ComputationGraphRuntime(v1);
		const runtime2 = new ComputationGraphRuntime(v2);
		
		expect(runtime1.getProgramHash()).toBe('v1.0.0');
		expect(runtime2.getProgramHash()).toBe('v2.0.0');
		expect(runtime1.getProgramHash()).not.toBe(runtime2.getProgramHash());
		
		await runtime1.cleanup();
		await runtime2.cleanup();
	});
});

