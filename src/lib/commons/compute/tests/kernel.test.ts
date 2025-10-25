/**
 * User Space Kernel Tests
 * 
 * Tests for the user space kernel (kernel.svelte.ts) - the layer that manages
 * the complete user space structure with schema-enforced namespaces.
 * 
 * Coverage:
 * - Program registry management
 * - Compute namespace operations
 * - Subscription tracking (bidirectional)
 * - Node storage
 * - Causality (ITC) management
 * - Allocation stores
 * - Tree stores
 * - Initialization and cleanup
 * - Diagnostics
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';

import { get } from 'svelte/store';
import {
	// Program namespace
	programRegistryStore,
	registerProgram,
	getProgram,
	activateProgram,
	deactivateProgram,
	
	// Compute namespace
	writeVariable,
	writeComputationResult,
	writeOutput,
	writeProvenance,
	
	// Subscription namespace
	outboundSubscriptionsStore,
	inboundSubscriptionsStore,
	registerLocalSubscription,
	registerPeerSubscription,
	registerInboundSubscription,
	
	// Node namespace
	nodesStore,
	writeNode,
	readNode,
	
	// Causality namespace
	myITCStampStore,
	peerITCStampsStore,
	writePeerITCStamp,
	subscribeToPeerITCStamp,
	
	// Allocation namespace
	myCommitmentStore,
	myAllocationStateStore,
	networkAllocationsStore,
	subscribeToPeerAllocation,
	
	// Trees namespace
	myTreeStore,
	networkTreesStore,
	subscribeToPeerTree,
	
	// Lifecycle
	initializeUserSpaceStores,
	cleanupUserSpaceStores,
	getUserSpaceDiagnostics,
	
	UserSpacePaths
} from '../kernel.svelte';

import type { 
	ProgramRegistryEntry,
	NodeEntry,
	ProvenanceEntry,
	OutputValue
} from '../kernel';

import { seed as itcSeed } from '../../utils/itc';
import type { 
	ITCStamp,
	Commitment,
	TwoTierAllocationState
} from '../../v2/schemas';

// ═══════════════════════════════════════════════════════════════════
// TEST SETUP & MOCKS
// ═══════════════════════════════════════════════════════════════════

// Mock holster - inline to avoid hoisting issues
vi.mock('$lib/state/holster.svelte', () => ({
	holsterUser: {
		is: { pub: '0'.repeat(64) },
		get: vi.fn(() => ({
			put: vi.fn((data, callback) => callback && callback()),
			once: vi.fn((callback) => callback(null)),
			on: vi.fn(() => () => {}),
			map: vi.fn(() => ({
				once: vi.fn()
			}))
		}))
	},
	holsterUserPub: { subscribe: vi.fn(), set: vi.fn(), update: vi.fn() }
}));

// Mock holster utils - inline to avoid hoisting issues
vi.mock('$lib/utils/holsterData', () => ({
	writeAtPath: vi.fn((user, path, data, callback) => callback && callback()),
	readAtPath: vi.fn((user, path, callback) => callback(null)),
	listenAtPath: vi.fn(() => () => {})
}));

// Get references to mocked modules for test access
let mockHolsterUser: any;
let mockWriteAtPath: any;
let mockReadAtPath: any;
let mockListenAtPath: any;

beforeEach(async () => {
	// Import mocked modules
	const holsterModule = await import('$lib/state/holster.svelte');
	const holsterDataModule = await import('$lib/utils/holsterData');
	
	mockHolsterUser = holsterModule.holsterUser;
	mockWriteAtPath = vi.mocked(holsterDataModule.writeAtPath);
	mockReadAtPath = vi.mocked(holsterDataModule.readAtPath);
	mockListenAtPath = vi.mocked(holsterDataModule.listenAtPath);
	
	// Reset mocks
	vi.clearAllMocks();
	
	// Reset default mock implementations
	mockWriteAtPath.mockImplementation((user: any, path: any, data: any, callback: any) => callback && callback());
	mockReadAtPath.mockImplementation((user: any, path: any, callback: any) => callback(null));
	mockListenAtPath.mockImplementation(() => () => {});
});

// Mock createStore
vi.mock('../../utils/store.svelte', () => ({
	createStore: vi.fn((config) => ({
		initialize: vi.fn(),
		cleanup: vi.fn(),
		subscribe: vi.fn(),
		set: vi.fn(),
		update: vi.fn()
	}))
}));

// ═══════════════════════════════════════════════════════════════════
// TEST DATA
// ═══════════════════════════════════════════════════════════════════

const testProgramHash = 'test_program_hash_123';
const testPeerPubKey = '1'.repeat(64);

const testProgramEntry: ProgramRegistryEntry = {
	definition: {
		id: 'test_program',
		variables: {},
		computations: [{
			id: 'comp',
			inputs: { x: { type: 'value', value: 1 } },
			compute_fn: 'fn',
			outputs: { y: { type: 'memory' } },
			debounce_ms: 0,
			enabled: true
		}]
	},
	metadata: {
		version: '1.0.0',
		description: 'Test program',
		created_at: Date.now(),
		updated_at: Date.now()
	},
	status: {
		active: false,
		enabled: true
	}
};

const testNodeEntry: NodeEntry = {
	node: {
		type: 'RootNode',
		id: 'root',
		name: 'Test Root',
		children: [],
		manual_fulfillment: null,
		created_at: new Date().toISOString(),
		updated_at: new Date().toISOString()
	},
	storage: undefined
};

const testITCStamp: ITCStamp = itcSeed();

// ═══════════════════════════════════════════════════════════════════
// PROGRAM REGISTRY TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Kernel - Program Registry', () => {
	beforeEach(() => {
		// Clear stores
		programRegistryStore.set(new Map());
		mockWriteAtPath.mockClear();
	});
	
	it('registers a new program', async () => {
		await registerProgram(testProgramHash, testProgramEntry);
		
		const registry = get(programRegistryStore);
		expect(registry.has(testProgramHash)).toBe(true);
		expect(registry.get(testProgramHash)).toEqual(testProgramEntry);
		
		expect(mockWriteAtPath).toHaveBeenCalled();
	});
	
	it('gets a registered program', async () => {
		await registerProgram(testProgramHash, testProgramEntry);
		
		const retrieved = getProgram(testProgramHash);
		expect(retrieved).toEqual(testProgramEntry);
	});
	
	it('returns null for non-existent program', () => {
		const retrieved = getProgram('nonexistent');
		expect(retrieved).toBeNull();
	});
	
	it('activates a program', async () => {
		await registerProgram(testProgramHash, testProgramEntry);
		await activateProgram(testProgramHash);
		
		const program = getProgram(testProgramHash);
		expect(program?.status.active).toBe(true);
		
		expect(mockWriteAtPath).toHaveBeenCalledWith(
			expect.anything(),
			expect.arrayContaining(['programs', 'active', testProgramHash]),
			expect.objectContaining({ registry_path: expect.any(String) }),
			expect.any(Function)
		);
	});
	
	it('deactivates a program', async () => {
		await registerProgram(testProgramHash, testProgramEntry);
		await activateProgram(testProgramHash);
		await deactivateProgram(testProgramHash);
		
		const program = getProgram(testProgramHash);
		expect(program?.status.active).toBe(false);
		
		expect(mockWriteAtPath).toHaveBeenCalledWith(
			expect.anything(),
			expect.arrayContaining(['programs', 'inactive', testProgramHash]),
			expect.objectContaining({ registry_path: expect.any(String) }),
			expect.any(Function)
		);
	});
	
	it('throws error when not authenticated', async () => {
		mockHolsterUser.is = null as any;
		
		await expect(registerProgram(testProgramHash, testProgramEntry))
			.rejects.toThrow('Not authenticated');
		
		mockHolsterUser.is = { pub: '0'.repeat(64) };
	});
});

// ═══════════════════════════════════════════════════════════════════
// COMPUTE NAMESPACE TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Kernel - Compute Namespace', () => {
	beforeEach(() => {
		mockWriteAtPath.mockClear();
	});
	
	it('writes a variable', async () => {
		await writeVariable(testProgramHash, 'myVar', 42);
		
		expect(mockWriteAtPath).toHaveBeenCalledWith(
			expect.anything(),
			expect.arrayContaining(['compute', testProgramHash, 'variables', 'myVar']),
			42,
			expect.any(Function)
		);
	});
	
	it('writes a computation result', async () => {
		const result = { sum: 100 };
		await writeComputationResult(testProgramHash, 'comp_id', result);
		
		expect(mockWriteAtPath).toHaveBeenCalledWith(
			expect.anything(),
			expect.arrayContaining(['compute', testProgramHash, 'state', 'computations', 'comp_id']),
			expect.objectContaining({ result }),
			expect.any(Function)
		);
	});
	
	it('writes an output value', async () => {
		await writeOutput(testProgramHash, 'output_key', 123, 'path/to/output');
		
		expect(mockWriteAtPath).toHaveBeenCalledWith(
			expect.anything(),
			expect.arrayContaining(['compute', testProgramHash, 'outputs', 'output_key']),
			expect.objectContaining({
				value: 123,
				holster_path: 'path/to/output',
				updated_at: expect.any(Number)
			}),
			expect.any(Function)
		);
	});
	
	it('writes provenance', async () => {
		const provenance: ProvenanceEntry = {
			record: {
				id: 'prov_1',
				itcStamp: testITCStamp,
				executedBy: '0'.repeat(64),
				timestamp: Date.now(),
				programHash: testProgramHash,
				computationId: 'comp',
				computationHash: 'hash',
				inputs: {},
				outputs: {},
				deterministicHash: 'det_hash'
			},
			signature: 'signature'
		};
		
		await writeProvenance(testProgramHash, 'prov_1', provenance);
		
		expect(mockWriteAtPath).toHaveBeenCalledWith(
			expect.anything(),
			expect.arrayContaining(['compute', testProgramHash, 'provenance', 'prov_1']),
			provenance,
			expect.any(Function)
		);
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION NAMESPACE TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Kernel - Subscription Namespace', () => {
	beforeEach(() => {
		outboundSubscriptionsStore.set({ local: new Map(), peers: new Map() });
		inboundSubscriptionsStore.set(new Map());
	});
	
	it('registers local subscription', async () => {
		await registerLocalSubscription('path/to/data', 'DataSchema', 'callback_1');
		
		const subs = get(outboundSubscriptionsStore);
		expect(subs.local.has('path/to/data')).toBe(true);
		
		const sub = subs.local.get('path/to/data');
		expect(sub?.schema_type).toBe('DataSchema');
		expect(sub?.subscribers).toContain('callback_1');
	});
	
	it('adds multiple subscribers to same path', async () => {
		await registerLocalSubscription('path/to/data', 'DataSchema', 'callback_1');
		await registerLocalSubscription('path/to/data', 'DataSchema', 'callback_2');
		
		const subs = get(outboundSubscriptionsStore);
		const sub = subs.local.get('path/to/data');
		
		expect(sub?.subscribers).toHaveLength(2);
		expect(sub?.subscribers).toContain('callback_1');
		expect(sub?.subscribers).toContain('callback_2');
	});
	
	it('registers peer subscription', async () => {
		await registerPeerSubscription(testPeerPubKey, 'path/to/peer/data', 'PeerSchema', 'callback_1');
		
		const subs = get(outboundSubscriptionsStore);
		expect(subs.peers.has(testPeerPubKey)).toBe(true);
		
		const peerSubs = subs.peers.get(testPeerPubKey);
		expect(peerSubs?.has('path/to/peer/data')).toBe(true);
		
		const sub = peerSubs?.get('path/to/peer/data');
		expect(sub?.schema_type).toBe('PeerSchema');
		expect(sub?.last_synced).toBeDefined();
	});
	
	it('registers inbound subscription', async () => {
		await registerInboundSubscription(testPeerPubKey, 'my/data/path');
		
		const subs = get(inboundSubscriptionsStore);
		expect(subs.has(testPeerPubKey)).toBe(true);
		expect(subs.get(testPeerPubKey)).toContain('my/data/path');
	});
	
	it('tracks multiple inbound subscriptions per peer', async () => {
		await registerInboundSubscription(testPeerPubKey, 'path1');
		await registerInboundSubscription(testPeerPubKey, 'path2');
		
		const subs = get(inboundSubscriptionsStore);
		const peerSubs = subs.get(testPeerPubKey);
		
		expect(peerSubs).toHaveLength(2);
		expect(peerSubs).toContain('path1');
		expect(peerSubs).toContain('path2');
	});
});

// ═══════════════════════════════════════════════════════════════════
// NODE NAMESPACE TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Kernel - Node Namespace', () => {
	beforeEach(() => {
		nodesStore.set(new Map());
		mockWriteAtPath.mockClear();
		mockReadAtPath.mockClear();
	});
	
	it('writes a node', async () => {
		await writeNode('node_1', testNodeEntry);
		
		const nodes = get(nodesStore);
		expect(nodes.has('node_1')).toBe(true);
		expect(nodes.get('node_1')).toEqual(testNodeEntry);
		
		expect(mockWriteAtPath).toHaveBeenCalled();
	});
	
	it('reads a node', async () => {
		// Mock readAtPath to return valid data
		mockReadAtPath.mockImplementationOnce((user: any, path: any, callback: any) => {
			callback(testNodeEntry);
		});
		
		const node = await readNode('node_1');
		
		expect(mockReadAtPath).toHaveBeenCalled();
		expect(node).toEqual(testNodeEntry);
	});
	
	it('returns null for non-existent node', async () => {
		mockReadAtPath.mockImplementationOnce((user: any, path: any, callback: any) => {
			callback(null);
		});
		
		const node = await readNode('nonexistent');
		expect(node).toBeNull();
	});
	
	it('returns null for invalid node data', async () => {
		mockReadAtPath.mockImplementationOnce((user: any, path: any, callback: any) => {
			callback({ invalid: 'data' });
		});
		
		const node = await readNode('invalid');
		expect(node).toBeNull();
	});
});

// ═══════════════════════════════════════════════════════════════════
// CAUSALITY NAMESPACE TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Kernel - Causality Namespace', () => {
	beforeEach(() => {
		peerITCStampsStore.set(new Map());
		mockWriteAtPath.mockClear();
	});
	
	it('writes peer ITC stamp', async () => {
		await writePeerITCStamp(testPeerPubKey, testITCStamp);
		
		const stamps = get(peerITCStampsStore);
		expect(stamps.has(testPeerPubKey)).toBe(true);
		
		const entry = stamps.get(testPeerPubKey);
		expect(entry?.itc_stamp).toEqual(testITCStamp);
		expect(entry?.last_seen).toBeDefined();
		
		expect(mockWriteAtPath).toHaveBeenCalled();
	});
	
	it('subscribes to peer ITC stamp', () => {
		const callback = vi.fn();
		const unsubscribe = subscribeToPeerITCStamp(testPeerPubKey, callback);
		
		expect(mockListenAtPath).toHaveBeenCalled();
		expect(typeof unsubscribe).toBe('function');
	});
	
	it('handles invalid peer ITC stamp data', () => {
		const callback = vi.fn();
		
		mockListenAtPath.mockImplementationOnce((user: any, path: any, dataCallback: any) => {
			// Simulate invalid data
			dataCallback({ invalid: 'stamp' });
			return () => {};
		});
		
		subscribeToPeerITCStamp(testPeerPubKey, callback);
		
		expect(callback).toHaveBeenCalledWith(null);
	});
});

// ═══════════════════════════════════════════════════════════════════
// ALLOCATION NAMESPACE TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Kernel - Allocation Namespace', () => {
	beforeEach(() => {
		networkAllocationsStore.set(new Map());
		mockListenAtPath.mockClear();
	});
	
	it('subscribes to peer allocation', () => {
		const unsubscribe = subscribeToPeerAllocation(testPeerPubKey);
		
		// Should subscribe to both commitment and state
		expect(mockListenAtPath).toHaveBeenCalledTimes(2);
		expect(typeof unsubscribe).toBe('function');
	});
	
	it('updates network allocations on commitment data', () => {
		const testCommitment: Commitment = {
			timestamp: Date.now(),
			itcStamp: testITCStamp,
			damping_factor: 1.0
		};
		
		mockListenAtPath.mockImplementation((user: any, path: any, callback: any) => {
			if (path.includes('commitment')) {
				callback(testCommitment);
			}
			return () => {};
		});
		
		subscribeToPeerAllocation(testPeerPubKey);
		
		const allocations = get(networkAllocationsStore);
		const peerData = allocations.get(testPeerPubKey);
		
		expect(peerData?.commitment).toBeDefined();
	});
});

// ═══════════════════════════════════════════════════════════════════
// TREES NAMESPACE TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Kernel - Trees Namespace', () => {
	beforeEach(() => {
		networkTreesStore.set(new Map());
		mockListenAtPath.mockClear();
	});
	
	it('subscribes to peer tree', () => {
		const unsubscribe = subscribeToPeerTree(testPeerPubKey);
		
		expect(mockListenAtPath).toHaveBeenCalled();
		expect(typeof unsubscribe).toBe('function');
	});
	
	it('removes peer tree on null data', () => {
		mockListenAtPath.mockImplementation((user: any, path: any, callback: any) => {
			callback(null);
			return () => {};
		});
		
		// Add a tree first
		networkTreesStore.update(map => {
			map.set(testPeerPubKey, testNodeEntry.node as any);
			return map;
		});
		
		subscribeToPeerTree(testPeerPubKey);
		
		const trees = get(networkTreesStore);
		expect(trees.has(testPeerPubKey)).toBe(false);
	});
});

// ═══════════════════════════════════════════════════════════════════
// LIFECYCLE TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Kernel - Lifecycle', () => {
	it('initializes user space stores', () => {
		// Should not throw
		expect(() => initializeUserSpaceStores()).not.toThrow();
	});
	
	it('cleans up user space stores', async () => {
		// Add some data
		programRegistryStore.set(new Map([['test', testProgramEntry]]));
		nodesStore.set(new Map([['node1', testNodeEntry]]));
		
		await cleanupUserSpaceStores();
		
		// All stores should be cleared
		expect(get(programRegistryStore).size).toBe(0);
		expect(get(nodesStore).size).toBe(0);
	});
	
	it('handles cleanup when not authenticated', async () => {
		mockHolsterUser.is = null as any;
		
		await expect(cleanupUserSpaceStores()).resolves.not.toThrow();
		
		mockHolsterUser.is = { pub: '0'.repeat(64) };
	});
});

// ═══════════════════════════════════════════════════════════════════
// DIAGNOSTICS TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Kernel - Diagnostics', () => {
	beforeEach(async () => {
		await cleanupUserSpaceStores();
	});
	
	it('returns empty diagnostics initially', () => {
		const diagnostics = getUserSpaceDiagnostics();
		
		expect(diagnostics.programs.registered).toBe(0);
		expect(diagnostics.subscriptions.outbound_local).toBe(0);
		expect(diagnostics.subscriptions.outbound_peers).toBe(0);
		expect(diagnostics.nodes.total).toBe(0);
		expect(diagnostics.causality.peer_stamps).toBe(0);
		expect(diagnostics.allocation.network_peers).toBe(0);
		expect(diagnostics.trees.network_trees).toBe(0);
	});
	
	it('tracks registered programs', async () => {
		await registerProgram('prog1', testProgramEntry);
		await registerProgram('prog2', testProgramEntry);
		
		const diagnostics = getUserSpaceDiagnostics();
		expect(diagnostics.programs.registered).toBe(2);
	});
	
	it('tracks subscriptions', async () => {
		await registerLocalSubscription('path1', 'Schema', 'cb1');
		await registerPeerSubscription(testPeerPubKey, 'path2', 'Schema', 'cb2');
		
		const diagnostics = getUserSpaceDiagnostics();
		expect(diagnostics.subscriptions.outbound_local).toBe(1);
		expect(diagnostics.subscriptions.outbound_peers).toBe(1);
	});
	
	it('tracks nodes', async () => {
		await writeNode('node1', testNodeEntry);
		await writeNode('node2', testNodeEntry);
		
		const diagnostics = getUserSpaceDiagnostics();
		expect(diagnostics.nodes.total).toBe(2);
	});
	
	it('tracks peer ITC stamps', async () => {
		await writePeerITCStamp('peer1', testITCStamp);
		await writePeerITCStamp('peer2', testITCStamp);
		
		const diagnostics = getUserSpaceDiagnostics();
		expect(diagnostics.causality.peer_stamps).toBe(2);
	});
});

// ═══════════════════════════════════════════════════════════════════
// USER SPACE PATHS TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Kernel - UserSpacePaths', () => {
	const testPubKey = '0'.repeat(64);
	
	it('generates correct program registry path', () => {
		const path = UserSpacePaths.programRegistry(testPubKey, 'hash123');
		expect(path).toContain('programs/registry/hash123');
	});
	
	it('generates correct program active path', () => {
		const path = UserSpacePaths.programActive(testPubKey, 'hash123');
		expect(path).toContain('programs/active/hash123');
	});
	
	it('generates correct compute variable path', () => {
		const path = UserSpacePaths.computeVariable(testPubKey, 'prog', 'var');
		expect(path).toContain('compute/prog/state/variables/var');
	});
	
	it('generates correct compute output path', () => {
		const path = UserSpacePaths.computeOutput(testPubKey, 'prog', 'out');
		expect(path).toContain('compute/prog/outputs/out');
	});
	
	it('generates correct node path', () => {
		const path = UserSpacePaths.node(testPubKey, 'node1');
		expect(path).toContain('nodes/node1');
	});
	
	it('generates correct peer ITC stamp path', () => {
		const path = UserSpacePaths.peerITCStamp(testPubKey, testPeerPubKey);
		expect(path).toContain('causality/peer_stamps/');
	});
});

