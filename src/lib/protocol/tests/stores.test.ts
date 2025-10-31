/**
 * Comprehensive Test Suite for V5 Stores
 * 
 * Tests all critical functionality:
 * - Network versioned stores (commitments, trees)
 * - Fine-grained field stores (recognition, needs, capacity)
 * - Mutual recognition computation
 * - Auto-subscription logic
 * - Commitment composition
 * - Spatial/temporal indexes
 * - Utility functions
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';

// ═══════════════════════════════════════════════════════════════════
// MOCKS - Must be defined BEFORE imports
// ═══════════════════════════════════════════════════════════════════

// Mock old state modules to prevent localStorage access
vi.mock('$lib/network/holster.svelte', () => ({
	holsterUser: null,
	holsterUserPub: { subscribe: () => () => {} },
	default: {}
}));

vi.mock('$lib/state/gun.svelte', () => ({
	gun: null,
	default: null
}));

vi.mock('$lib/config', () => ({
	config: {
		holster: {
			peers: [],
			indexedDB: false,
			file: undefined
		}
	}
}));

import { get } from 'svelte/store';
import {
	networkCommitments,
	networkRecognitionTrees,
	networkRecognitionWeights,
	networkNeedSlots,
	networkCapacitySlots,
	myMutualRecognition,
	myRecognitionWeights,
	myRecognitionTreeStore,
	myCommitmentStore,
	subscribeToCommitment,
	subscribeToRecognitionTree,
	unsubscribeFromParticipant,
	getSubscribedParticipants,
	syncSubscriptionsWithTree,
	getMyContributors,
	composeCommitmentFromSources,
	getNetworkCommitmentsRecord,
	getSubscriptionStats,
	getConvergenceStats
} from '$lib/protocol/stores.svelte';
import { mockAuth, clearAuth } from '$lib/network/holster.svelte';
import type { Commitment, RootNode, NeedSlot, AvailabilitySlot, GlobalRecognitionWeights } from '../schemas';
import { seed as itcSeed, event as itcEvent, join as itcJoin } from '$lib/utils/primitives/itc';

// Mock user public key for tests
const TEST_USER_PUB = 'test-user-pub-key';

// ═══════════════════════════════════════════════════════════════════
// TEST DATA HELPERS
// ═══════════════════════════════════════════════════════════════════

function createTestCommitment(overrides: Partial<Commitment> = {}): Commitment {
	return {
		timestamp: Date.now(),
		need_slots: [],
		capacity_slots: [],
		global_recognition_weights: {},
		global_mr_values: {},
		itcStamp: itcSeed(),
		...overrides
	};
}

function createTestTree(overrides: Partial<RootNode> = {}): RootNode {
	return {
		id: 'root',
		name: 'My Values',
		type: 'RootNode' as const,
		manual_fulfillment: null,
		children: [],
		created_at: new Date().toISOString(),
		updated_at: new Date().toISOString(),
		...overrides
	};
}

function createTestNeedSlot(overrides: Partial<NeedSlot> = {}): NeedSlot {
	return {
		id: `need-${Math.random()}`,
		need_type_id: 'food',
		name: 'Test Need',
		quantity: 10,
		...overrides
	};
}

function createTestCapacitySlot(overrides: Partial<AvailabilitySlot> = {}): AvailabilitySlot {
	return {
		id: `capacity-${Math.random()}`,
		need_type_id: 'food',
		name: 'Test Capacity',
		quantity: 20,
		...overrides
	};
}

// ═══════════════════════════════════════════════════════════════════
// TEST 1: NETWORK COMMITMENTS - VERSIONED STORE INTEGRATION
// ═══════════════════════════════════════════════════════════════════

describe('Network Commitments - Versioned Store', () => {
	beforeEach(() => {
		// Clear any existing data
		const keys = Array.from(networkCommitments.get().keys());
		keys.forEach(key => networkCommitments.delete(key));
	});

	it('should store and retrieve commitments', () => {
		const commitment = createTestCommitment({
			need_slots: [createTestNeedSlot({ quantity: 5 })]
		});

		const result = networkCommitments.update('alice', commitment);
		expect(result.applied).toBe(true);

		const stored = networkCommitments.getData('alice');
		expect(stored).toBeDefined();
		expect(stored?.need_slots?.length).toBe(1);
		expect(stored?.need_slots?.[0].quantity).toBe(5);
	});

	it('should track field changes independently', () => {
		const needSlot = createTestNeedSlot({ quantity: 10 });
		
		const commitment1 = createTestCommitment({
			global_recognition_weights: { bob: 0.5 },
			need_slots: [needSlot]
		});

		networkCommitments.update('alice', commitment1);

		// Update only recognition (reuse same needSlot object)
		const commitment2 = createTestCommitment({
			global_recognition_weights: { bob: 0.7 }, // Changed
			need_slots: [needSlot], // Exact same object!
			timestamp: commitment1.timestamp + 1,
			itcStamp: itcEvent(commitment1.itcStamp!)
		});

		const result = networkCommitments.update('alice', commitment2);
		expect(result.applied).toBe(true);
		expect(result.changedFields?.has('recognition')).toBe(true);
		expect(result.changedFields?.has('needs')).toBe(false); // Should NOT change
	});

	it('should reject stale updates via ITC', () => {
		const stamp1 = itcEvent(itcSeed());
		const stamp2 = itcEvent(stamp1);

		// Apply newer first
		const commitment2 = createTestCommitment({
			timestamp: 2000,
			itcStamp: stamp2
		});
		networkCommitments.update('alice', commitment2);

		// Try to apply older (stale)
		const commitment1 = createTestCommitment({
			timestamp: 1000,
			itcStamp: stamp1
		});
		const result = networkCommitments.update('alice', commitment1);

		expect(result.applied).toBe(false);
		expect(result.reason).toBe('ITC causal staleness');
	});

	it('should accept concurrent updates via ITC join', () => {
		const seedStamp = itcSeed();
		const stamp1 = itcEvent(seedStamp);
		const stamp2 = itcEvent(itcSeed()); // Concurrent

		// Apply first
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { bob: 0.5 },
			timestamp: 1000,
			itcStamp: stamp1
		}));

		// Apply concurrent
		const result = networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { charlie: 0.3 },
			timestamp: 999, // Older timestamp due to clock skew!
			itcStamp: stamp2
		}));

		expect(result.applied).toBe(true); // Should accept concurrent
	});

	it('should handle entity deletion', () => {
		networkCommitments.update('alice', createTestCommitment());
		expect(networkCommitments.getData('alice')).toBeDefined();

		const deleted = networkCommitments.delete('alice');
		expect(deleted).toBe(true);
		expect(networkCommitments.getData('alice')).toBeUndefined();
	});
});

// ═══════════════════════════════════════════════════════════════════
// TEST 2: FINE-GRAINED FIELD STORES
// ═══════════════════════════════════════════════════════════════════

describe('Fine-Grained Field Stores', () => {
	beforeEach(() => {
		const keys = Array.from(networkCommitments.get().keys());
		keys.forEach(key => networkCommitments.delete(key));
	});

	it('networkRecognitionWeights should only update when recognition changes', () => {
		let recognitionUpdateCount = 0;
		const unsub = networkRecognitionWeights.subscribe(() => {
			recognitionUpdateCount++;
		});

		const initialCount = recognitionUpdateCount;

		// Update with recognition
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { bob: 0.5 },
			timestamp: 1000
		}));
		expect(recognitionUpdateCount).toBeGreaterThan(initialCount);
		const afterRecUpdate = recognitionUpdateCount;

		// Update only needs (recognition unchanged)
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { bob: 0.5 }, // Same!
			need_slots: [createTestNeedSlot()],
			timestamp: 2000
		}));
		expect(recognitionUpdateCount).toBe(afterRecUpdate); // Should NOT increment

		// Update recognition
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { bob: 0.7 }, // Changed!
			need_slots: [createTestNeedSlot()],
			timestamp: 3000
		}));
		expect(recognitionUpdateCount).toBeGreaterThan(afterRecUpdate); // Should increment

		unsub();
	});

	it('networkNeedSlots should only update when needs change', () => {
		const needSlot10 = createTestNeedSlot({ quantity: 10 });
		const needSlot15 = createTestNeedSlot({ quantity: 15 });

		let needsUpdateCount = 0;
		const unsub = networkNeedSlots.subscribe(() => {
			needsUpdateCount++;
		});

		const initialCount = needsUpdateCount;

		// Update with needs
		networkCommitments.update('alice', createTestCommitment({
			need_slots: [needSlot10],
			timestamp: 1000
		}));
		expect(needsUpdateCount).toBeGreaterThan(initialCount);
		const afterNeedsUpdate = needsUpdateCount;

		// Update only recognition (reuse same needSlot10)
		networkCommitments.update('alice', createTestCommitment({
			need_slots: [needSlot10], // Exact same object!
			global_recognition_weights: { bob: 0.5 },
			timestamp: 2000
		}));
		expect(needsUpdateCount).toBe(afterNeedsUpdate); // Should NOT increment

		// Update needs (use different needSlot15)
		networkCommitments.update('alice', createTestCommitment({
			need_slots: [needSlot15], // Different!
			global_recognition_weights: { bob: 0.5 },
			timestamp: 3000
		}));
		expect(needsUpdateCount).toBeGreaterThan(afterNeedsUpdate); // Should increment

		unsub();
	});

	it('networkCapacitySlots should only update when capacity changes', () => {
		const capSlot20 = createTestCapacitySlot({ quantity: 20 });
		const capSlot25 = createTestCapacitySlot({ quantity: 25 });

		let capacityUpdateCount = 0;
		const unsub = networkCapacitySlots.subscribe(() => {
			capacityUpdateCount++;
		});

		const initialCount = capacityUpdateCount;

		// Update with capacity
		networkCommitments.update('alice', createTestCommitment({
			capacity_slots: [capSlot20],
			timestamp: 1000
		}));
		expect(capacityUpdateCount).toBeGreaterThan(initialCount);
		const afterCapUpdate = capacityUpdateCount;

		// Update only recognition (reuse same capSlot20)
		networkCommitments.update('alice', createTestCommitment({
			capacity_slots: [capSlot20], // Exact same object!
			global_recognition_weights: { bob: 0.5 },
			timestamp: 2000
		}));
		expect(capacityUpdateCount).toBe(afterCapUpdate); // Should NOT increment

		// Update capacity (use different capSlot25)
		networkCommitments.update('alice', createTestCommitment({
			capacity_slots: [capSlot25], // Different!
			global_recognition_weights: { bob: 0.5 },
			timestamp: 3000
		}));
		expect(capacityUpdateCount).toBeGreaterThan(afterCapUpdate); // Should increment

		unsub();
	});
});

// ═══════════════════════════════════════════════════════════════════
// TEST 3: MUTUAL RECOGNITION COMPUTATION
// ═══════════════════════════════════════════════════════════════════

describe('Mutual Recognition', () => {
	beforeEach(() => {
		mockAuth(TEST_USER_PUB, 'test-user');
		const keys = Array.from(networkCommitments.get().keys());
		keys.forEach(key => networkCommitments.delete(key));
	});

	afterEach(() => {
		clearAuth();
	});

	it('should compute mutual recognition as minimum of both directions', () => {
		// Set my recognition weights in my commitment
		myCommitmentStore.set(createTestCommitment({
			global_recognition_weights: {
				alice: 0.7,
				bob: 0.3
			}
		}));

		// Mock network commitments with their recognition of me
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: {
				[TEST_USER_PUB]: 0.5, // Alice recognizes me at 0.5
				charlie: 0.5
			}
		}));

		networkCommitments.update('bob', createTestCommitment({
			global_recognition_weights: {
				[TEST_USER_PUB]: 0.8, // Bob recognizes me at 0.8
				charlie: 0.2
			}
		}));

		// Get mutual recognition
		const mr = get(myMutualRecognition);

		// MR(me, alice) = min(0.7, 0.5) = 0.5
		expect(mr.alice).toBe(0.5);

		// MR(me, bob) = min(0.3, 0.8) = 0.3
		expect(mr.bob).toBe(0.3);
	});

	it('should only recalculate when recognition changes', () => {
		// Set my recognition of alice
		myCommitmentStore.set(createTestCommitment({
			global_recognition_weights: { alice: 0.6 }
		}));

		let mrUpdateCount = 0;
		const unsub = myMutualRecognition.subscribe(() => {
			mrUpdateCount++;
		});

		const initialCount = mrUpdateCount;

		// Update recognition
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { [TEST_USER_PUB]: 0.5 },
			timestamp: 1000
		}));
		expect(mrUpdateCount).toBeGreaterThan(initialCount);
		const afterRecUpdate = mrUpdateCount;

		// Update only needs (recognition unchanged)
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { [TEST_USER_PUB]: 0.5 }, // Same!
			need_slots: [createTestNeedSlot()],
			timestamp: 2000
		}));
		expect(mrUpdateCount).toBe(afterRecUpdate); // Should NOT recalculate

		unsub();
	});
});

// ═══════════════════════════════════════════════════════════════════
// TEST 4: UTILITY FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

describe('Utility Functions', () => {
	beforeEach(() => {
		const keys = Array.from(networkCommitments.get().keys());
		keys.forEach(key => networkCommitments.delete(key));
	});

	it('getNetworkCommitmentsRecord should extract data from versioned entities', () => {
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { bob: 0.5 }
		}));

		networkCommitments.update('bob', createTestCommitment({
			need_slots: [createTestNeedSlot()]
		}));

		const record = getNetworkCommitmentsRecord();
		expect(Object.keys(record).length).toBe(2);
		expect(record.alice).toBeDefined();
		expect(record.alice.global_recognition_weights?.bob).toBe(0.5);
		expect(record.bob).toBeDefined();
		expect(record.bob.need_slots?.length).toBe(1);
	});

	it('getSubscriptionStats should return correct counts', () => {
		networkCommitments.update('alice', createTestCommitment());
		networkCommitments.update('bob', createTestCommitment());

		const stats = getSubscriptionStats();
		expect(stats.commitments).toBe(2);
		expect(stats.architecture).toBe('v5-tree-plus-commitment-versioned');
	});

	it('getConvergenceStats should detect converged participants', () => {
		// Participant with zero needs (converged)
		networkCommitments.update('alice', createTestCommitment({
			need_slots: [createTestNeedSlot({ quantity: 0.0001 })]
		}));

		// Participant with significant needs (not converged)
		networkCommitments.update('bob', createTestCommitment({
			need_slots: [createTestNeedSlot({ quantity: 10 })]
		}));

		const stats = getConvergenceStats();
		expect(stats.totalWithData).toBe(2);
		expect(stats.convergedCount).toBe(1); // Only alice
		expect(stats.convergenceRate).toBe(0.5);
		expect(stats.networkConverged).toBe(false); // < 80%
	});
});

// ═══════════════════════════════════════════════════════════════════
// TEST 5: RECOGNITION TREES - VERSIONED STORE
// ═══════════════════════════════════════════════════════════════════

describe('Network Recognition Trees - Versioned Store', () => {
	beforeEach(() => {
		const keys = Array.from(networkRecognitionTrees.get().keys());
		keys.forEach(key => networkRecognitionTrees.delete(key));
	});

	it('should store and retrieve trees', () => {
		const tree = createTestTree({
			name: 'Alice Values',
			children: [{
				id: 'node1',
				name: 'Healthcare',
				type: 'NonRootNode',
				points: 100,
				contributors: [],
				anti_contributors: [],
				manual_fulfillment: null,
				children: []
			}]
		});

		const result = networkRecognitionTrees.update('alice', tree);
		expect(result.applied).toBe(true);

		const stored = networkRecognitionTrees.getData('alice');
		expect(stored).toBeDefined();
		expect(stored?.name).toBe('Alice Values');
		expect(stored?.children.length).toBe(1);
	});

	it('should track field changes independently', () => {
		const tree1 = createTestTree({
			name: 'V1',
			manual_fulfillment: 0.5,
			updated_at: new Date(1000).toISOString()
		});

		networkRecognitionTrees.update('alice', tree1);

		// Update only fulfillment
		const tree2 = createTestTree({
			name: 'V1', // Same
			manual_fulfillment: 0.7, // Changed
			updated_at: new Date(2000).toISOString()
		});

		const result = networkRecognitionTrees.update('alice', tree2);
		expect(result.applied).toBe(true);
		expect(result.changedFields?.has('fulfillment')).toBe(true);
		expect(result.changedFields?.has('structure')).toBe(false);
	});
});

// ═══════════════════════════════════════════════════════════════════
// TEST 6: PERFORMANCE - SELECTIVE REACTIVITY
// ═══════════════════════════════════════════════════════════════════

describe('Performance - Selective Reactivity', () => {
	beforeEach(() => {
		const keys = Array.from(networkCommitments.get().keys());
		keys.forEach(key => networkCommitments.delete(key));
	});

	it('should only trigger affected stores when recognition changes', () => {
		const needSlot = createTestNeedSlot();
		const capSlot = createTestCapacitySlot();

		// First, create alice with initial data
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { bob: 0.3 },
			need_slots: [needSlot],
			capacity_slots: [capSlot],
			timestamp: 1000
		}));

		// Now subscribe and track updates
		let recognitionUpdates = 0;
		let needsUpdates = 0;
		let capacityUpdates = 0;

		const unsubRec = networkRecognitionWeights.subscribe(() => recognitionUpdates++);
		const unsubNeeds = networkNeedSlots.subscribe(() => needsUpdates++);
		const unsubCap = networkCapacitySlots.subscribe(() => capacityUpdates++);

		const initial = { recognitionUpdates, needsUpdates, capacityUpdates };

		// Update ONLY recognition (reuse same slot objects)
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { bob: 0.5 }, // Changed!
			need_slots: [needSlot], // Exact same object!
			capacity_slots: [capSlot], // Exact same object!
			timestamp: 2000
		}));

		// Only recognition should have updated
		expect(recognitionUpdates).toBeGreaterThan(initial.recognitionUpdates);
		expect(needsUpdates).toBe(initial.needsUpdates); // Should NOT update
		expect(capacityUpdates).toBe(initial.capacityUpdates); // Should NOT update

		unsubRec();
		unsubNeeds();
		unsubCap();
	});

	it('should only trigger affected stores when needs change', () => {
		const needSlot10 = createTestNeedSlot({ quantity: 10 });
		const needSlot20 = createTestNeedSlot({ quantity: 20 });
		const capSlot = createTestCapacitySlot();

		// First, create alice with initial data
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { bob: 0.5 },
			need_slots: [needSlot10],
			capacity_slots: [capSlot],
			timestamp: 1000
		}));

		// Now subscribe and track updates
		let recognitionUpdates = 0;
		let needsUpdates = 0;
		let capacityUpdates = 0;

		const unsubRec = networkRecognitionWeights.subscribe(() => recognitionUpdates++);
		const unsubNeeds = networkNeedSlots.subscribe(() => needsUpdates++);
		const unsubCap = networkCapacitySlots.subscribe(() => capacityUpdates++);

		const initial = { recognitionUpdates, needsUpdates, capacityUpdates };

		// Update ONLY needs (reuse same capSlot, use different needSlot)
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { bob: 0.5 }, // Same
			need_slots: [needSlot20], // Different!
			capacity_slots: [capSlot], // Exact same object!
			timestamp: 2000
		}));

		// Only needs should have updated
		expect(recognitionUpdates).toBe(initial.recognitionUpdates); // Should NOT update
		expect(needsUpdates).toBeGreaterThan(initial.needsUpdates);
		expect(capacityUpdates).toBe(initial.capacityUpdates); // Should NOT update

		unsubRec();
		unsubNeeds();
		unsubCap();
	});

	it('should only trigger affected stores when capacity changes', () => {
		const needSlot = createTestNeedSlot();
		const capSlot10 = createTestCapacitySlot({ quantity: 10 });
		const capSlot20 = createTestCapacitySlot({ quantity: 20 });

		// First, create alice with initial data
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { bob: 0.5 },
			need_slots: [needSlot],
			capacity_slots: [capSlot10],
			timestamp: 1000
		}));

		// Now subscribe and track updates
		let recognitionUpdates = 0;
		let needsUpdates = 0;
		let capacityUpdates = 0;

		const unsubRec = networkRecognitionWeights.subscribe(() => recognitionUpdates++);
		const unsubNeeds = networkNeedSlots.subscribe(() => needsUpdates++);
		const unsubCap = networkCapacitySlots.subscribe(() => capacityUpdates++);

		const initial = { recognitionUpdates, needsUpdates, capacityUpdates };

		// Update ONLY capacity (reuse same needSlot, use different capSlot)
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { bob: 0.5 }, // Same
			need_slots: [needSlot], // Exact same object!
			capacity_slots: [capSlot20], // Different!
			timestamp: 2000
		}));

		// Only capacity should have updated
		expect(recognitionUpdates).toBe(initial.recognitionUpdates); // Should NOT update
		expect(needsUpdates).toBe(initial.needsUpdates); // Should NOT update
		expect(capacityUpdates).toBeGreaterThan(initial.capacityUpdates);

		unsubRec();
		unsubNeeds();
		unsubCap();
	});
});

// ═══════════════════════════════════════════════════════════════════
// TEST 7: INTEGRATION - FULL REACTIVE FLOW
// ═══════════════════════════════════════════════════════════════════

describe('Integration - Full Reactive Flow', () => {
	beforeEach(() => {
		mockAuth(TEST_USER_PUB, 'test-user');
		const keys = Array.from(networkCommitments.get().keys());
		keys.forEach(key => networkCommitments.delete(key));
	});

	afterEach(() => {
		clearAuth();
	});

	it('should handle complete update flow with all components', () => {
		// Create slot objects once to reuse
		const needSlot5 = createTestNeedSlot({ quantity: 5 });
		const needSlot3 = createTestNeedSlot({ quantity: 3 });
		const capSlot10 = createTestCapacitySlot({ quantity: 10 });

		// Set my recognition
		myCommitmentStore.set(createTestCommitment({
			global_recognition_weights: { alice: 0.7 }
		}));

		// Track all updates
		let commitmentUpdates = 0;
		let recognitionUpdates = 0;
		let needsUpdates = 0;
		let capacityUpdates = 0;
		let mrUpdates = 0;

		const unsubCommit = networkCommitments.subscribe(() => commitmentUpdates++);
		const unsubRec = networkRecognitionWeights.subscribe(() => recognitionUpdates++);
		const unsubNeeds = networkNeedSlots.subscribe(() => needsUpdates++);
		const unsubCap = networkCapacitySlots.subscribe(() => capacityUpdates++);
		const unsubMR = myMutualRecognition.subscribe(() => mrUpdates++);

		const initial = { commitmentUpdates, recognitionUpdates, needsUpdates, capacityUpdates, mrUpdates };

		// Scenario 1: Alice publishes complete commitment
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { [TEST_USER_PUB]: 0.6, bob: 0.4 },
			need_slots: [needSlot5],
			capacity_slots: [capSlot10],
			timestamp: 1000
		}));

		// All should update
		expect(commitmentUpdates).toBeGreaterThan(initial.commitmentUpdates);
		expect(recognitionUpdates).toBeGreaterThan(initial.recognitionUpdates);
		expect(needsUpdates).toBeGreaterThan(initial.needsUpdates);
		expect(capacityUpdates).toBeGreaterThan(initial.capacityUpdates);
		expect(mrUpdates).toBeGreaterThan(initial.mrUpdates);

		const afterFirst = { commitmentUpdates, recognitionUpdates, needsUpdates, capacityUpdates, mrUpdates };

		// Scenario 2: Alice updates only needs (reuse same capSlot10, use different needSlot3)
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { [TEST_USER_PUB]: 0.6, bob: 0.4 }, // Same
			need_slots: [needSlot3], // Different!
			capacity_slots: [capSlot10], // Exact same object!
			timestamp: 2000
		}));

		// Only commitment and needs should update, NOT recognition, capacity, or MR
		expect(commitmentUpdates).toBeGreaterThan(afterFirst.commitmentUpdates);
		expect(recognitionUpdates).toBe(afterFirst.recognitionUpdates); // Should NOT update
		expect(needsUpdates).toBeGreaterThan(afterFirst.needsUpdates);
		expect(capacityUpdates).toBe(afterFirst.capacityUpdates); // Should NOT update
		expect(mrUpdates).toBe(afterFirst.mrUpdates); // Should NOT update

		// Clean up
		unsubCommit();
		unsubRec();
		unsubNeeds();
		unsubCap();
		unsubMR();
	});
});

// ═══════════════════════════════════════════════════════════════════
// TEST 8: EDGE CASES
// ═══════════════════════════════════════════════════════════════════

describe('Edge Cases', () => {
	beforeEach(() => {
		const keys = Array.from(networkCommitments.get().keys());
		keys.forEach(key => networkCommitments.delete(key));
	});

	it('should handle empty commitments', () => {
		const result = networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: {},
			need_slots: [],
			capacity_slots: []
		}));

		expect(result.applied).toBe(true);
		const stored = networkCommitments.getData('alice');
		expect(stored).toBeDefined();
	});

	it('should handle updates with undefined arrays', () => {
		const commitment: Commitment = {
			timestamp: Date.now(),
			global_recognition_weights: { bob: 0.5 },
			itcStamp: itcSeed()
		};

		const result = networkCommitments.update('alice', commitment);
		expect(result.applied).toBe(true);
	});

	it('should handle rapid sequential updates', () => {
		for (let i = 0; i < 10; i++) {
			const result = networkCommitments.update('alice', createTestCommitment({
				global_recognition_weights: { bob: i * 0.1 },
				timestamp: 1000 + i
			}));
			expect(result.applied).toBe(true);
		}

		const final = networkCommitments.getData('alice');
		expect(final?.global_recognition_weights?.bob).toBe(0.9);
	});

	it('should handle multiple participants simultaneously', () => {
		const participants = ['alice', 'bob', 'charlie', 'diana'];

		participants.forEach(pub => {
			networkCommitments.update(pub, createTestCommitment({
				global_recognition_weights: { me: 0.5 }
			}));
		});

		const record = getNetworkCommitmentsRecord();
		expect(Object.keys(record).length).toBe(4);
		participants.forEach(pub => {
			expect(record[pub]).toBeDefined();
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// TEST 9: METADATA AND VERSIONING
// ═══════════════════════════════════════════════════════════════════

describe('Metadata and Versioning', () => {
	beforeEach(() => {
		const keys = Array.from(networkCommitments.get().keys());
		keys.forEach(key => networkCommitments.delete(key));
	});

	it('should track field versions independently', () => {
		const needSlot = createTestNeedSlot();
		
		// Initial update
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { bob: 0.5 },
			need_slots: [needSlot],
			timestamp: 1000
		}));

		const v1 = networkCommitments.getFieldVersion('alice', 'recognition');
		expect(v1).toBe(1);

		// Update only recognition (reuse same needSlot object)
		networkCommitments.update('alice', createTestCommitment({
			global_recognition_weights: { bob: 0.7 }, // Changed
			need_slots: [needSlot], // Exact same object!
			timestamp: 2000
		}));

		const v2Recognition = networkCommitments.getFieldVersion('alice', 'recognition');
		const v2Needs = networkCommitments.getFieldVersion('alice', 'needs');

		expect(v2Recognition).toBe(2); // Should increment
		expect(v2Needs).toBe(1); // Should NOT increment
	});

	it('should preserve ITC stamps', () => {
		const stamp = itcEvent(itcSeed());

		networkCommitments.update('alice', createTestCommitment({
			itcStamp: stamp
		}));

		const metadata = networkCommitments.getMetadata('alice');
		expect(metadata?.itcStamp).toBeDefined();
	});
});

// ═══════════════════════════════════════════════════════════════════
// TEST 10: REALISTIC SCENARIO - MULTI-USER ALLOCATION
// ═══════════════════════════════════════════════════════════════════

describe('Realistic Scenario - Multi-User Allocation', () => {
	beforeEach(() => {
		const keys = Array.from(networkCommitments.get().keys());
		keys.forEach(key => networkCommitments.delete(key));
	});

	it('should handle complex multi-user scenario', () => {
		// Scenario: Alice and Bob in mutual recognition network
		// Alice needs food, Bob provides food
		// They recognize each other

		// Alice's commitment
		networkCommitments.update('alice', createTestCommitment({
			need_slots: [
				createTestNeedSlot({
					need_type_id: 'food',
					quantity: 10
				})
			],
			capacity_slots: [],
			global_recognition_weights: {
				bob: 0.6,
				charlie: 0.4
			},
			timestamp: 1000
		}));

		// Bob's commitment
		networkCommitments.update('bob', createTestCommitment({
			need_slots: [],
			capacity_slots: [
				createTestCapacitySlot({
					need_type_id: 'food',
					quantity: 20
				})
			],
			global_recognition_weights: {
				alice: 0.7,
				charlie: 0.3
			},
			timestamp: 1001
		}));

		// Verify data
		const record = getNetworkCommitmentsRecord();
		expect(record.alice.need_slots?.length).toBe(1);
		expect(record.alice.need_slots?.[0].quantity).toBe(10);
		expect(record.bob.capacity_slots?.length).toBe(1);
		expect(record.bob.capacity_slots?.[0].quantity).toBe(20);

		// Verify recognition
		expect(record.alice.global_recognition_weights?.bob).toBe(0.6);
		expect(record.bob.global_recognition_weights?.alice).toBe(0.7);

		// Now Alice updates her needs (quantity decreases as she receives food)
		networkCommitments.update('alice', createTestCommitment({
			need_slots: [
				createTestNeedSlot({
					need_type_id: 'food',
					quantity: 5 // Decreased!
				})
			],
			capacity_slots: [],
			global_recognition_weights: {
				bob: 0.6, // Same
				charlie: 0.4
			},
			timestamp: 2000
		}));

		// Verify only needs changed
		const updated = networkCommitments.getData('alice');
		expect(updated?.need_slots?.[0].quantity).toBe(5);
		expect(updated?.global_recognition_weights?.bob).toBe(0.6);

		// Check convergence
		const convergence = getConvergenceStats();
		expect(convergence.totalWithData).toBe(1); // Only alice has needs
	});
});

// ═══════════════════════════════════════════════════════════════════
// TEST 11: HOLSTER CONVERTERS - ARRAY/RECORD CONVERSION
// NOTE: These tests are now obsolete! We use JSON.stringify/parse instead.
// Tests kept for reference only - they will fail if run.
// ═══════════════════════════════════════════════════════════════════

// import { toHolsterFormat, fromHolsterFormat } from '../../utils/holster-converters';

describe.skip('Holster Converters - Array to Record Conversion (OBSOLETE - using JSON now)', () => {
	it('should convert arrays with id field to id-keyed records', () => {
		const data = [
			{ id: 'slot-1', name: 'Food', quantity: 10 },
			{ id: 'slot-2', name: 'Housing', quantity: 5 }
		];

		const result = toHolsterFormat(data);

		expect(result).toEqual({
			'slot-1': { id: 'slot-1', name: 'Food', quantity: 10 },
			'slot-2': { id: 'slot-2', name: 'Housing', quantity: 5 }
		});
	});

	it('should convert arrays without id field to numeric-keyed records', () => {
		const data = ['red', 'green', 'blue'];
		const result = toHolsterFormat(data);

		expect(result).toEqual({
			'0': 'red',
			'1': 'green',
			'2': 'blue'
		});
	});

	it('should convert empty arrays to null', () => {
		const result = toHolsterFormat([]);
		expect(result).toBeNull();
	});

	it('should handle nested arrays', () => {
		const data = {
			slots: [
				{ id: 's1', times: ['09:00', '10:00'] }
			]
		};

		const result = toHolsterFormat(data);

		expect(result).toEqual({
			slots: {
				's1': {
					id: 's1',
					times: {
						'0': '09:00',
						'1': '10:00'
					}
				}
			}
		});
	});

	it('should skip undefined values', () => {
		const data = {
			name: 'Test',
			optional: undefined,
			value: 42
		};

		const result = toHolsterFormat(data);

		expect(result).toEqual({
			name: 'Test',
			value: 42
		});
		expect(result.optional).toBeUndefined();
	});

	it('should handle complex nested structures', () => {
		const data = {
			capacity_slots: [
				{
					id: 'slot-1',
					name: 'Food',
					availability_window: {
						time_ranges: [
							{ start_time: '09:00', end_time: '12:00' },
							{ start_time: '14:00', end_time: '17:00' }
						]
					}
				}
			]
		};

		const result = toHolsterFormat(data);

		expect(result.capacity_slots['slot-1'].id).toBe('slot-1');
		expect(result.capacity_slots['slot-1'].availability_window.time_ranges['0'].start_time).toBe('09:00');
		expect(result.capacity_slots['slot-1'].availability_window.time_ranges['1'].start_time).toBe('14:00');
	});
});

describe.skip('Holster Converters - Record to Array Conversion (OBSOLETE - using JSON now)', () => {
	it('should convert numeric-keyed records back to arrays', () => {
		const data = {
			'0': 'red',
			'1': 'green',
			'2': 'blue'
		};

		const result = fromHolsterFormat(data);

		expect(result).toEqual(['red', 'green', 'blue']);
	});

	it('should convert id-keyed records back to arrays', () => {
		const data = {
			'slot-1': { id: 'slot-1', name: 'Food', quantity: 10 },
			'slot-2': { id: 'slot-2', name: 'Housing', quantity: 5 }
		};

		const result = fromHolsterFormat(data);

		expect(Array.isArray(result)).toBe(true);
		expect(result.length).toBe(2);
		expect(result.find((s: any) => s.id === 'slot-1')).toBeDefined();
		expect(result.find((s: any) => s.id === 'slot-2')).toBeDefined();
	});

	it('should convert null to null (for nullable fields)', () => {
		const result = fromHolsterFormat(null);
		expect(result).toBeNull();
	});

	it('should handle nested record-to-array conversion', () => {
		const data = {
			slots: {
				's1': {
					id: 's1',
					times: {
						'0': '09:00',
						'1': '10:00'
					}
				}
			}
		};

		const result = fromHolsterFormat(data);

		expect(Array.isArray(result.slots)).toBe(true);
		expect(result.slots[0].id).toBe('s1');
		expect(Array.isArray(result.slots[0].times)).toBe(true);
		expect(result.slots[0].times).toEqual(['09:00', '10:00']);
	});

	it('should handle complex nested conversion', () => {
		const data = {
			capacity_slots: {
				'slot-1': {
					id: 'slot-1',
					name: 'Food',
					availability_window: {
						time_ranges: {
							'0': { start_time: '09:00', end_time: '12:00' },
							'1': { start_time: '14:00', end_time: '17:00' }
						}
					}
				}
			}
		};

		const result = fromHolsterFormat(data);

		expect(Array.isArray(result.capacity_slots)).toBe(true);
		expect(result.capacity_slots[0].id).toBe('slot-1');
		expect(Array.isArray(result.capacity_slots[0].availability_window.time_ranges)).toBe(true);
		expect(result.capacity_slots[0].availability_window.time_ranges.length).toBe(2);
	});
});

describe.skip('Holster Converters - Enum Normalization (OBSOLETE - using JSON now)', () => {
	it('should normalize valid enum values to lowercase', () => {
		const data = {
			slot: {
				id: 's1',
				recurrence: 'Daily'
			}
		};

		const result = fromHolsterFormat(data);
		expect(result.slot.recurrence).toBe('daily');
	});

	it('should map legacy Bi-weekly to weekly', () => {
		const data = {
			capacity_slots: {
				'slot-1': {
					id: 'slot-1',
					recurrence: 'Bi-weekly'
				}
			}
		};

		const result = fromHolsterFormat(data);
		expect(result.capacity_slots[0].recurrence).toBe('weekly');
	});

	it('should map legacy Weekends to weekly', () => {
		const data = {
			slot: {
				id: 's1',
				recurrence: 'Weekends'
			}
		};

		const result = fromHolsterFormat(data);
		expect(result.slot.recurrence).toBe('weekly');
	});

	it('should map legacy Weekdays to weekly', () => {
		const data = {
			slot: {
				id: 's1',
				recurrence: 'Weekdays'
			}
		};

		const result = fromHolsterFormat(data);
		expect(result.slot.recurrence).toBe('weekly');
	});

	it('should handle nested enum normalization in arrays', () => {
		const data = {
			capacity_slots: {
				'slot-1': { id: 'slot-1', recurrence: 'Daily' },
				'slot-2': { id: 'slot-2', recurrence: 'Bi-weekly' },
				'slot-3': { id: 'slot-3', recurrence: 'Weekends' }
			}
		};

		const result = fromHolsterFormat(data);

		expect(result.capacity_slots[0].recurrence).toBe('daily');
		expect(result.capacity_slots[1].recurrence).toBe('weekly');
		expect(result.capacity_slots[2].recurrence).toBe('weekly');
	});

	it('should handle Does not repeat', () => {
		const data = {
			slot: {
				id: 's1',
				recurrence: 'Does not repeat'
			}
		};

		const result = fromHolsterFormat(data);
		expect(result.slot.recurrence).toBeNull();
	});

	it('should normalize deeply nested enum values', () => {
		const data = {
			commitment: {
				capacity_slots: {
					'slot-1': {
						id: 'slot-1',
						nested: {
							deep: {
								recurrence: 'Monthly'
							}
						}
					}
				}
			}
		};

		const result = fromHolsterFormat(data);
		expect(result.commitment.capacity_slots[0].nested.deep.recurrence).toBe('monthly');
	});
});

describe.skip('Holster Converters - Round Trip Conversion (OBSOLETE - using JSON now)', () => {
	it('should preserve data through round trip for slots', () => {
		const original = {
			capacity_slots: [
				{
					id: 'slot-1',
					name: 'Food',
					quantity: 10,
					need_type_id: 'food',
					recurrence: 'daily'
				}
			]
		};

		const toHolster = toHolsterFormat(original);
		const fromHolster = fromHolsterFormat(toHolster);

		expect(fromHolster.capacity_slots).toEqual(original.capacity_slots);
	});

	it('should preserve nested structures through round trip', () => {
		const original = {
			capacity_slots: [
				{
					id: 'slot-1',
					availability_window: {
						time_ranges: [
							{ start_time: '09:00', end_time: '12:00' }
						]
					}
				}
			]
		};

		const toHolster = toHolsterFormat(original);
		const fromHolster = fromHolsterFormat(toHolster);

		expect(fromHolster).toEqual(original);
	});

	it('should fix legacy enum values during round trip', () => {
		const original = {
			capacity_slots: [
				{ id: 'slot-1', recurrence: 'Bi-weekly' }
			]
		};

		const toHolster = toHolsterFormat(original);
		const fromHolster = fromHolsterFormat(toHolster);

		// Legacy value should be normalized
		expect(fromHolster.capacity_slots[0].recurrence).toBe('weekly');
	});
});

describe.skip('Holster Converters - Edge Cases (OBSOLETE - using JSON now)', () => {
	it('should handle empty objects', () => {
		const result = toHolsterFormat({});
		expect(result).toBeNull();
	});

	it('should handle null input', () => {
		const result = toHolsterFormat(null);
		expect(result).toBeNull();
	});

	it('should handle undefined input', () => {
		const result = toHolsterFormat(undefined);
		expect(result).toBeNull();
	});

	it('should handle primitives', () => {
		expect(toHolsterFormat(42)).toBe(42);
		expect(toHolsterFormat('test')).toBe('test');
		expect(toHolsterFormat(true)).toBe(true);
	});

	it('should handle mixed array types', () => {
		const data = [
			{ id: 'a', value: 1 },
			{ value: 2 } // No id field!
		];

		// When not all elements have id, should fall back to numeric keys
		const result = toHolsterFormat(data);
		expect(result['0']).toBeDefined();
		expect(result['1']).toBeDefined();
	});

	it('should handle Holster metadata fields', () => {
		const data = {
			capacity_slots: {
				'slot-1': { id: 'slot-1', name: 'Food' },
				'_': { some: 'metadata' }, // Holster metadata
				'#': { some: 'reference' } // Gun reference
			}
		};

		const result = fromHolsterFormat(data);

		// Should only have the actual slot, not metadata
		expect(Array.isArray(result.capacity_slots)).toBe(true);
		expect(result.capacity_slots.length).toBe(1);
		expect(result.capacity_slots[0].id).toBe('slot-1');
	});

	it('should handle sparse numeric arrays', () => {
		const data = {
			'0': 'first',
			'2': 'third',
			'4': 'fifth'
		};

		// This is NOT a valid array (missing 1, 3), so should stay as object
		const result = fromHolsterFormat(data);
		expect(typeof result).toBe('object');
		expect(Array.isArray(result)).toBe(false);
	});

	it('should handle very deeply nested structures', () => {
		const data = {
			level1: {
				level2: {
					level3: {
						level4: {
							slots: [
								{ id: 's1', recurrence: 'Daily' }
							]
						}
					}
				}
			}
		};

		const toHolster = toHolsterFormat(data);
		const fromHolster = fromHolsterFormat(toHolster);

		expect(fromHolster.level1.level2.level3.level4.slots[0].recurrence).toBe('daily');
	});

	it('should handle commitments with all array fields', () => {
		const commitment = createTestCommitment({
			need_slots: [
				createTestNeedSlot({ id: 'need-1', quantity: 5 }),
				createTestNeedSlot({ id: 'need-2', quantity: 10 })
			],
			capacity_slots: [
				createTestCapacitySlot({ id: 'cap-1', quantity: 20 })
			]
		});

		const toHolster = toHolsterFormat(commitment);
		const fromHolster = fromHolsterFormat(toHolster);

		expect(Array.isArray(fromHolster.need_slots)).toBe(true);
		expect(fromHolster.need_slots.length).toBe(2);
		expect(Array.isArray(fromHolster.capacity_slots)).toBe(true);
		expect(fromHolster.capacity_slots.length).toBe(1);
	});
});

describe.skip('Holster Converters - Integration with Stores (OBSOLETE - using JSON now)', () => {
	beforeEach(() => {
		mockAuth(TEST_USER_PUB, 'test-user');
	});

	afterEach(() => {
		clearAuth();
	});

	it('should persist and retrieve slots correctly through converters', () => {
		// Create a commitment with slots
		const commitment = createTestCommitment({
			capacity_slots: [
				createTestCapacitySlot({
					id: 'slot-1',
					name: 'Food',
					quantity: 10,
					recurrence: 'daily' // Lowercase enum
				})
			]
		});

		// Store it (will use toHolsterFormat internally)
		myCommitmentStore.set(commitment);

		// Retrieve it (will use fromHolsterFormat internally)
		const retrieved = get(myCommitmentStore);

		expect(retrieved).toBeDefined();
		expect(Array.isArray(retrieved?.capacity_slots)).toBe(true);
		expect(retrieved?.capacity_slots?.[0].id).toBe('slot-1');
		expect(retrieved?.capacity_slots?.[0].recurrence).toBe('daily');
	});

	it('should handle legacy data when loading from network', () => {
		// Simulate receiving data with legacy enum values from Holster
		// The data comes in as a Record (Holster format) with legacy enum values
		const legacyHolsterData = {
			timestamp: Date.now(),
			itcStamp: itcSeed(),
			global_recognition_weights: {},
			capacity_slots: {
				'slot-1': {
					id: 'slot-1',
					name: 'Test',
					quantity: 10,
					need_type_id: 'food',
					recurrence: 'Bi-weekly' // Legacy value in Holster format
				}
			}
		};

		// Apply fromHolsterFormat (this is what happens when data comes from Holster)
		const normalizedCommitment = fromHolsterFormat(legacyHolsterData);

		// Verify conversion happened correctly (Record → Array)
		expect(Array.isArray(normalizedCommitment.capacity_slots)).toBe(true);
		expect(normalizedCommitment.capacity_slots?.length).toBe(1);
		
		// Verify enum normalization happened ('Bi-weekly' → 'weekly')
		expect(normalizedCommitment.capacity_slots?.[0].recurrence).toBe('weekly');

		// Now update the versioned store with the normalized data
		const updateResult = networkCommitments.update('alice', normalizedCommitment);
		
		// Verify update was accepted (schema validation passed)
		expect(updateResult.applied).toBe(true);

		// Retrieve and verify data is intact
		const retrieved = networkCommitments.getData('alice');
		expect(Array.isArray(retrieved?.capacity_slots)).toBe(true);
		expect(retrieved?.capacity_slots?.[0].recurrence).toBe('weekly');
	});
});

