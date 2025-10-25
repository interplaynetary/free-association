/**
 * Runtime Manager Tests
 * 
 * Tests for ComputeRuntimeManager - the high-level API for deploying
 * and managing reactive computation programs.
 * 
 * Coverage:
 * - Program registration and activation
 * - Lifecycle management (start/stop/restart)
 * - Variable and computation result access
 * - Provenance tracking
 * - Status queries
 * - Deployment convenience functions
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';

import { 
	ComputeRuntimeManager,
	deployProgram,
	deployReactiveProgram
} from '../runtime-manager.svelte';
import { registerComputationFunction } from '../compute.svelte';
import type { ReactiveComputationGraph } from '../schema';
import { get } from 'svelte/store';

// ═══════════════════════════════════════════════════════════════════
// TEST SETUP
// ═══════════════════════════════════════════════════════════════════

// Mock holster user
vi.mock('$lib/state/holster.svelte', () => ({
	holsterUser: {
		is: { pub: '0'.repeat(64) },
		get: vi.fn(() => ({
			put: vi.fn((data, callback) => callback && callback()),
			once: vi.fn((callback) => callback(null)),
			on: vi.fn(() => () => {})
		}))
	},
	holsterUserPub: { subscribe: vi.fn(), set: vi.fn(), update: vi.fn() }
}));

// Mock holster utils
vi.mock('$lib/utils/holsterData', () => ({
	writeAtPath: vi.fn((user, path, data, callback) => callback && callback()),
	readAtPath: vi.fn((user, path, callback) => callback(null)),
	listenAtPath: vi.fn(() => () => {})
}));

// Register test computation functions
registerComputationFunction('add', (inputs: { a: number; b: number }) => ({
	sum: inputs.a + inputs.b
}));

registerComputationFunction('multiply', (inputs: { x: number; y: number }) => ({
	product: inputs.x * inputs.y
}));

registerComputationFunction('identity', (inputs: { value: any }) => ({
	result: inputs.value
}));

// ═══════════════════════════════════════════════════════════════════
// TEST PROGRAMS
// ═══════════════════════════════════════════════════════════════════

const simpleProgram: ReactiveComputationGraph = {
	id: 'simple_test_program',
	version: '1.0.0',
	description: 'Simple test program',
	variables: {
		a: { type: 'value', value: 10 },
		b: { type: 'value', value: 20 }
	},
	computations: [{
		id: 'add_numbers',
		inputs: {
			a: { type: 'local', state_path: 'a' },
			b: { type: 'local', state_path: 'b' }
		},
		compute_fn: 'add',
		outputs: {
			sum: { type: 'memory' }
		},
		debounce_ms: 0,
		enabled: true
	}]
};

const chainedProgram: ReactiveComputationGraph = {
	id: 'chained_test_program',
	version: '1.0.0',
	description: 'Program with chained computations',
	variables: {
		a: { type: 'value', value: 5 },
		b: { type: 'value', value: 3 }
	},
	computations: [
		{
			id: 'first',
			inputs: {
				a: { type: 'value', value: 5 },
				b: { type: 'value', value: 3 }
			},
			compute_fn: 'add',
			outputs: {
				sum: { type: 'memory' }
			},
			debounce_ms: 0,
			enabled: true
		},
		{
			id: 'second',
			depends_on: ['first'],
			inputs: {
				x: { type: 'derived', computation_id: 'first', output_key: 'sum' },
				y: { type: 'value', value: 2 }
			},
			compute_fn: 'multiply',
			outputs: {
				product: { type: 'memory' }
			},
			debounce_ms: 0,
			enabled: true
		}
	]
};

// ═══════════════════════════════════════════════════════════════════
// CONSTRUCTOR & INITIALIZATION TESTS
// ═══════════════════════════════════════════════════════════════════

describe('ComputeRuntimeManager - Constructor', () => {
	it('creates manager with default options', () => {
		const manager = new ComputeRuntimeManager(simpleProgram);
		
		expect(manager).toBeDefined();
		expect(manager.getProgramId()).toBe('simple_test_program');
		expect(manager.getProgramHash()).toBeDefined();
	});
	
	it('accepts custom options', () => {
		const manager = new ComputeRuntimeManager(simpleProgram, {
			enableProvenance: false,
			autoActivate: false,
			enableReactivity: true,
			version: '2.0.0',
			description: 'Custom description'
		});
		
		expect(manager).toBeDefined();
		const status = manager.getStatus();
		expect(status.isRunning).toBe(false);
	});
	
	it('computes program hash from graph', () => {
		const manager = new ComputeRuntimeManager(simpleProgram);
		const hash = manager.getProgramHash();
		
		expect(hash).toBeDefined();
		expect(hash.length).toBeGreaterThan(0);
	});
	
	it('uses provided program_hash if available', () => {
		const programWithHash: ReactiveComputationGraph = {
			...simpleProgram,
			program_hash: 'custom_hash_123'
		};
		
		const manager = new ComputeRuntimeManager(programWithHash);
		expect(manager.getProgramHash()).toBe('custom_hash_123');
	});
});

// ═══════════════════════════════════════════════════════════════════
// LIFECYCLE MANAGEMENT TESTS
// ═══════════════════════════════════════════════════════════════════

describe('ComputeRuntimeManager - Lifecycle', () => {
	let manager: ComputeRuntimeManager;
	
	beforeEach(() => {
		manager = new ComputeRuntimeManager(simpleProgram, {
			autoActivate: false,
			enableProvenance: true
		});
	});
	
	afterEach(async () => {
		if (manager) {
			await manager.stop().catch(() => {});
		}
	});
	
	it('starts successfully', async () => {
		await manager.start();
		
		const status = manager.getStatus();
		expect(status.isRunning).toBe(true);
		expect(status.isRegistered).toBe(true);
	});
	
	it('executes computations on start', async () => {
		await manager.start();
		
		const result = manager.getComputationResult('add_numbers', 'sum');
		expect(result).toBe(30); // 10 + 20
	});
	
	it('stops successfully', async () => {
		await manager.start();
		await manager.stop();
		
		const status = manager.getStatus();
		expect(status.isRunning).toBe(false);
	});
	
	it('restarts successfully', async () => {
		await manager.start();
		
		const beforeRestart = manager.getComputationResult('add_numbers', 'sum');
		expect(beforeRestart).toBe(30);
		
		await manager.restart();
		
		const afterRestart = manager.getComputationResult('add_numbers', 'sum');
		expect(afterRestart).toBe(30);
		
		const status = manager.getStatus();
		expect(status.isRunning).toBe(true);
	});
	
	it('handles start when already running', async () => {
		await manager.start();
		
		// Should not throw
		await expect(manager.start()).resolves.not.toThrow();
	});
	
	it('handles stop when not running', async () => {
		// Should not throw
		await expect(manager.stop()).resolves.not.toThrow();
	});
	
	it('activates program if autoActivate is true', async () => {
		const autoManager = new ComputeRuntimeManager(simpleProgram, {
			autoActivate: true
		});
		
		await autoManager.start();
		
		const status = autoManager.getStatus();
		expect(status.isActive).toBe(true);
		
		await autoManager.stop();
	});
	
	it('deactivates program when stopped with deactivate flag', async () => {
		await manager.start();
		await manager.activate();
		
		expect(manager.getStatus().isActive).toBe(true);
		
		await manager.stop(true); // deactivate = true
		
		expect(manager.getStatus().isActive).toBe(false);
	});
});

// ═══════════════════════════════════════════════════════════════════
// VARIABLE & RESULT ACCESS TESTS
// ═══════════════════════════════════════════════════════════════════

describe('ComputeRuntimeManager - Variables & Results', () => {
	let manager: ComputeRuntimeManager;
	
	beforeEach(async () => {
		manager = new ComputeRuntimeManager(simpleProgram);
		await manager.start();
	});
	
	afterEach(async () => {
		await manager.stop().catch(() => {});
	});
	
	it('gets variable values', () => {
		const a = manager.getVariable('a');
		const b = manager.getVariable('b');
		
		expect(a).toBe(10);
		expect(b).toBe(20);
	});
	
	it('sets variable values', () => {
		manager.setVariable('a', 100);
		
		const newA = manager.getVariable('a');
		expect(newA).toBe(100);
	});
	
	it('gets computation results', () => {
		const sum = manager.getComputationResult('add_numbers', 'sum');
		expect(sum).toBe(30);
	});
	
	it('gets all computation results', () => {
		const results = manager.getComputationResults('add_numbers');
		
		expect(results).toHaveProperty('sum');
		expect(results.sum).toBe(30);
	});
	
	it('returns null for non-existent variable', () => {
		const nonExistent = manager.getVariable('does_not_exist');
		expect(nonExistent).toBeNull();
	});
	
	it('returns null for non-existent computation', () => {
		const nonExistent = manager.getComputationResult('does_not_exist', 'result');
		expect(nonExistent).toBeNull();
	});
});

// ═══════════════════════════════════════════════════════════════════
// CHAINED COMPUTATION TESTS
// ═══════════════════════════════════════════════════════════════════

describe('ComputeRuntimeManager - Chained Computations', () => {
	let manager: ComputeRuntimeManager;
	
	beforeEach(async () => {
		manager = new ComputeRuntimeManager(chainedProgram);
		await manager.start();
	});
	
	afterEach(async () => {
		await manager.stop().catch(() => {});
	});
	
	it('executes chained computations in order', () => {
		const firstResult = manager.getComputationResult('first', 'sum');
		const secondResult = manager.getComputationResult('second', 'product');
		
		expect(firstResult).toBe(8); // 5 + 3
		expect(secondResult).toBe(16); // 8 * 2
	});
	
	it('handles derived inputs correctly', () => {
		// First computation: 5 + 3 = 8
		// Second computation: 8 * 2 = 16
		const product = manager.getComputationResult('second', 'product');
		expect(product).toBe(16);
	});
});

// ═══════════════════════════════════════════════════════════════════
// REACTIVITY TESTS
// ═══════════════════════════════════════════════════════════════════

describe('ComputeRuntimeManager - Reactivity', () => {
	let manager: ComputeRuntimeManager;
	
	beforeEach(async () => {
		manager = new ComputeRuntimeManager(simpleProgram, {
			enableReactivity: false // Start disabled
		});
		await manager.start();
	});
	
	afterEach(async () => {
		await manager.stop().catch(() => {});
	});
	
	it('enables reactivity', () => {
		manager.enableReactivity();
		
		const status = manager.getStatus();
		expect(status.isReactive).toBe(true);
	});
	
	it('disables reactivity', () => {
		manager.enableReactivity();
		expect(manager.getStatus().isReactive).toBe(true);
		
		manager.disableReactivity();
		expect(manager.getStatus().isReactive).toBe(false);
	});
	
	it('starts with reactivity if option is true', async () => {
		const reactiveManager = new ComputeRuntimeManager(simpleProgram, {
			enableReactivity: true
		});
		
		await reactiveManager.start();
		
		expect(reactiveManager.getStatus().isReactive).toBe(true);
		
		await reactiveManager.stop();
	});
});

// ═══════════════════════════════════════════════════════════════════
// PROVENANCE TESTS
// ═══════════════════════════════════════════════════════════════════

describe('ComputeRuntimeManager - Provenance', () => {
	let manager: ComputeRuntimeManager;
	
	beforeEach(async () => {
		manager = new ComputeRuntimeManager(simpleProgram, {
			enableProvenance: true
		});
		await manager.start();
	});
	
	afterEach(async () => {
		await manager.stop().catch(() => {});
	});
	
	it('tracks provenance when enabled', () => {
		const provenance = manager.getProvenance('add_numbers');
		
		expect(provenance).toBeDefined();
		if (provenance) {
			expect(provenance.computationId).toBe('add_numbers');
			expect(provenance.inputs).toBeDefined();
			expect(provenance.outputs).toBeDefined();
			expect(provenance.itcStamp).toBeDefined();
		}
	});
	
	it('gets all provenance records', () => {
		const allProvenance = manager.getAllProvenance();
		
		expect(allProvenance.size).toBeGreaterThan(0);
		expect(allProvenance.has('add_numbers')).toBe(true);
	});
	
	it('does not track provenance when disabled', async () => {
		const noProvManager = new ComputeRuntimeManager(simpleProgram, {
			enableProvenance: false
		});
		
		await noProvManager.start();
		
		const provenance = noProvManager.getProvenance('add_numbers');
		expect(provenance).toBeUndefined();
		
		await noProvManager.stop();
	});
});

// ═══════════════════════════════════════════════════════════════════
// STATUS & DIAGNOSTICS TESTS
// ═══════════════════════════════════════════════════════════════════

describe('ComputeRuntimeManager - Status', () => {
	it('reports correct initial status', () => {
		const manager = new ComputeRuntimeManager(simpleProgram);
		const status = manager.getStatus();
		
		expect(status.isRunning).toBe(false);
		expect(status.isRegistered).toBe(false);
		expect(status.isActive).toBe(false);
		expect(status.isReactive).toBe(false);
		expect(status.programHash).toBeDefined();
		expect(status.programId).toBe('simple_test_program');
	});
	
	it('updates status after start', async () => {
		const manager = new ComputeRuntimeManager(simpleProgram);
		await manager.start();
		
		const status = manager.getStatus();
		expect(status.isRunning).toBe(true);
		expect(status.isRegistered).toBe(true);
		
		await manager.stop();
	});
	
	it('gets program graph', () => {
		const manager = new ComputeRuntimeManager(simpleProgram);
		const graph = manager.getGraph();
		
		expect(graph).toEqual(simpleProgram);
	});
	
	it('gets registry entry', () => {
		const manager = new ComputeRuntimeManager(simpleProgram);
		const entry = manager.getRegistryEntry();
		
		// May be null if not yet registered
		// After start, should be defined
	});
	
	it('gets runtime instance', () => {
		const manager = new ComputeRuntimeManager(simpleProgram);
		const runtime = manager.getRuntime();
		
		expect(runtime).toBeDefined();
	});
});

// ═══════════════════════════════════════════════════════════════════
// CONVENIENCE FUNCTION TESTS
// ═══════════════════════════════════════════════════════════════════

describe('ComputeRuntimeManager - Convenience Functions', () => {
	it('deploys program with deployProgram', async () => {
		const manager = await deployProgram(simpleProgram, {
			enableProvenance: true
		});
		
		expect(manager).toBeDefined();
		expect(manager.getStatus().isRunning).toBe(true);
		
		const result = manager.getComputationResult('add_numbers', 'sum');
		expect(result).toBe(30);
		
		await manager.stop();
	});
	
	it('deploys reactive program with deployReactiveProgram', async () => {
		const manager = await deployReactiveProgram(simpleProgram, {
			enableProvenance: true
		});
		
		expect(manager).toBeDefined();
		expect(manager.getStatus().isRunning).toBe(true);
		expect(manager.getStatus().isReactive).toBe(true);
		
		await manager.stop();
	});
	
	it('deployProgram uses default options', async () => {
		const manager = await deployProgram(simpleProgram);
		
		const status = manager.getStatus();
		expect(status.isRunning).toBe(true);
		expect(status.isReactive).toBe(false); // Default is false for deployProgram
		
		await manager.stop();
	});
	
	it('deployReactiveProgram enables reactivity by default', async () => {
		const manager = await deployReactiveProgram(simpleProgram);
		
		const status = manager.getStatus();
		expect(status.isRunning).toBe(true);
		expect(status.isReactive).toBe(true); // Default is true for deployReactiveProgram
		
		await manager.stop();
	});
});

// ═══════════════════════════════════════════════════════════════════
// ERROR HANDLING TESTS
// ═══════════════════════════════════════════════════════════════════

describe('ComputeRuntimeManager - Error Handling', () => {
	it('handles invalid program gracefully', async () => {
		const invalidProgram = {
			id: 'invalid',
			variables: {},
			computations: []
		} as any;
		
		// Constructor doesn't validate, but execution should handle empty computations
		const manager = new ComputeRuntimeManager(invalidProgram);
		
		// Start should succeed even with no computations
		await expect(manager.start()).resolves.not.toThrow();
		
		// Results should be empty
		expect(manager.getComputationResults('invalid')).toEqual({});
		
		await manager.stop();
	});
	
	it('handles execution errors gracefully', async () => {
		// Register a function that throws
		registerComputationFunction('throws', () => {
			throw new Error('Test error');
		});
		
		const errorProgram: ReactiveComputationGraph = {
			id: 'error_program',
			variables: {},
			computations: [{
				id: 'comp',
				inputs: { x: { type: 'value', value: 1 } },
				compute_fn: 'throws',
				outputs: { result: { type: 'memory' } },
				debounce_ms: 0,
				enabled: true
			}]
		};
		
		const manager = new ComputeRuntimeManager(errorProgram);
		
		// Should not throw, but result should be null/undefined
		await expect(manager.start()).resolves.not.toThrow();
		
		const result = manager.getComputationResult('comp', 'result');
		expect(result).toBeNull();
		
		await manager.stop();
	});
});

// ═══════════════════════════════════════════════════════════════════
// INTEGRATION TESTS
// ═══════════════════════════════════════════════════════════════════

describe('ComputeRuntimeManager - Integration', () => {
	it('handles complex multi-step workflow', async () => {
		const manager = new ComputeRuntimeManager(chainedProgram, {
			enableProvenance: true,
			enableReactivity: false
		});
		
		// 1. Start
		await manager.start();
		expect(manager.getStatus().isRunning).toBe(true);
		
		// 2. Check initial results
		const initial = manager.getComputationResult('second', 'product');
		expect(initial).toBe(16); // (5+3)*2
		
		// 3. Get provenance
		const prov = manager.getProvenance('second');
		expect(prov).toBeDefined();
		
		// 4. Activate
		await manager.activate();
		expect(manager.getStatus().isActive).toBe(true);
		
		// 5. Execute again
		await manager.execute();
		
		// 6. Check results unchanged
		const afterExecute = manager.getComputationResult('second', 'product');
		expect(afterExecute).toBe(16);
		
		// 7. Stop
		await manager.stop(true);
		expect(manager.getStatus().isRunning).toBe(false);
		expect(manager.getStatus().isActive).toBe(false);
	});
});

