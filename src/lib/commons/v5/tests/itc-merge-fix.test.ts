/**
 * ITC Merge Fix - Data Loss Prevention Tests
 * 
 * Tests that the ITC merge fix prevents data loss in concurrent scenarios
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { get } from 'svelte/store';
import { 
	seed as itcSeed, 
	event as itcEvent, 
	leq as itcLeq,
	equals as itcEquals,
	join as itcJoin,
	type Stamp as ITCStamp 
} from '../../utils/itc';
import { 
	networkCommitments,
	composeCommitmentFromSources,
	myCommitmentStore,
	myNeedSlotsStore,
	setMyNeedSlots // ✅ NEW: Helper to update need slots
} from '../stores.svelte';
import type { Commitment, NeedSlot } from '../schemas';

describe('ITC Merge Fix - Data Loss Prevention', () => {
	beforeEach(() => {
		// Clear stores
		const keys = Array.from(networkCommitments.get().keys());
		keys.forEach(key => networkCommitments.delete(key));
	});
	
	function createTestCommitment(partial: Partial<Commitment>): Commitment {
		return {
			need_slots: [],
			capacity_slots: [],
			global_recognition_weights: {},
			global_mr_values: {},
			timestamp: Date.now(),
			itcStamp: itcSeed(),
			...partial
		};
	}
	
	it('should merge network ITCs when composing commitment', () => {
		// Simulate network state: Alice and Bob have published commitments
		const aliceITC = itcEvent(itcSeed());
		const bobITC = itcEvent(itcEvent(itcSeed()));
		
		networkCommitments.update('alice_pub', createTestCommitment({
			itcStamp: aliceITC,
			timestamp: 1000
		}));
		
		networkCommitments.update('bob_pub', createTestCommitment({
			itcStamp: bobITC,
			timestamp: 1001
		}));
		
		// User has local commitment AND local data (needed for composition)
		myCommitmentStore.set(createTestCommitment({
			itcStamp: itcEvent(itcSeed()),
			timestamp: 999
		}));
		
		// Add local need slots so composition has data to work with
		setMyNeedSlots([
			{ 
				id: 'local_need', 
				name: 'Local Need', 
				quantity: 50, 
				need_type_id: 'food' 
			} as NeedSlot
		]);
		
		// Compose new commitment
		const composed = composeCommitmentFromSources();
		
		expect(composed).not.toBeNull();
		if (!composed) return;
		
		// ✅ Composed ITC should include Alice's ITC
		expect(itcLeq(aliceITC, composed.itcStamp)).toBe(true);
		
		// ✅ Composed ITC should include Bob's ITC
		expect(itcLeq(bobITC, composed.itcStamp)).toBe(true);
		
		console.log('[TEST] ✅ Composed ITC includes all network history');
	});
	
	it('should prevent data loss scenario: local edit during network update', async () => {
		// SCENARIO: Alice edits locally while Bob's update arrives
		
		// t=0: Alice has initial commitment
		const aliceInitialITC = itcEvent(itcSeed());
		myCommitmentStore.set(createTestCommitment({
			need_slots: [],
			itcStamp: aliceInitialITC,
			timestamp: 1000
		}));
		
		// t=20: Bob's commitment arrives
		const bobITC = itcEvent(itcEvent(itcSeed()));
		networkCommitments.update('bob_pub', createTestCommitment({
			need_slots: [
				{
					id: 'bob_need',
					name: 'Bob Food',
					quantity: 100,
					need_type_id: 'food'
				} as NeedSlot
			],
			itcStamp: bobITC,
			timestamp: 1020
		}));
		
		// t=100: Alice edits locally (adds need)
		setMyNeedSlots([
			{
				id: 'alice_need',
				name: 'Alice Housing',
				quantity: 50,
				need_type_id: 'housing'
			} as NeedSlot
		]);
		
		// Compose Alice's new commitment
		const aliceNewCommitment = composeCommitmentFromSources();
		
		expect(aliceNewCommitment).not.toBeNull();
		if (!aliceNewCommitment) return;
		
		// ✅ Alice's new ITC should include Bob's ITC
		expect(itcLeq(bobITC, aliceNewCommitment.itcStamp)).toBe(true);
		
		// ✅ Bob should NOT reject Alice's update as stale
		// Simulate Bob receiving Alice's update
		const bobReceivesResult = networkCommitments.update('alice_pub', aliceNewCommitment);
		expect(bobReceivesResult.applied).toBe(true);
		expect(bobReceivesResult.reason).not.toBe('ITC causal staleness');
		
		console.log('[TEST] ✅ Local edit during network update: No data loss');
	});
	
	it('should handle concurrent device edits without data loss', () => {
		// SCENARIO: Same user editing from laptop and phone simultaneously
		
		// Device 1 (laptop): Edit recognition
		const laptopITC = itcEvent(itcSeed());
		const laptopCommitment = createTestCommitment({
			global_recognition_weights: { bob: 0.5 },
			itcStamp: laptopITC,
			timestamp: 1000
		});
		
		// Device 2 (phone): Edit needs (concurrent!)
		const phoneITC = itcEvent(itcSeed());
		const phoneCommitment = createTestCommitment({
			need_slots: [
				{ 
					id: 'food', 
					name: 'Food', 
					quantity: 100, 
					need_type_id: 'food' 
				} as NeedSlot
			],
			itcStamp: phoneITC,
			timestamp: 999  // Older timestamp (clock skew!)
		});
		
		// Both updates arrive at server
		networkCommitments.update('device_laptop', laptopCommitment);
		networkCommitments.update('device_phone', phoneCommitment);
		
		// User on device 3 sees both updates
		myCommitmentStore.set(createTestCommitment({
			itcStamp: itcSeed(),
			timestamp: 900
		}));
		
		// User edits
		setMyNeedSlots([
			{ 
				id: 'housing', 
				name: 'Housing', 
				quantity: 50, 
				need_type_id: 'housing' 
			} as NeedSlot
		]);
		
		const composed = composeCommitmentFromSources();
		expect(composed).not.toBeNull();
		if (!composed) return;
		
		// ✅ Should include both device ITCs
		expect(itcLeq(laptopITC, composed.itcStamp)).toBe(true);
		expect(itcLeq(phoneITC, composed.itcStamp)).toBe(true);
		
		console.log('[TEST] ✅ Concurrent device edits: Both preserved');
	});
	
	it('should not lose updates in rapid network activity', () => {
		// SCENARIO: Multiple network updates arrive rapidly
		
		const stamp1 = itcEvent(itcSeed());
		const stamp2 = itcEvent(itcEvent(itcSeed()));
		const stamp3 = itcEvent(itcEvent(itcEvent(itcSeed())));
		
		// Rapid network updates
		networkCommitments.update('user1', createTestCommitment({
			itcStamp: stamp1,
			timestamp: 1000
		}));
		
		networkCommitments.update('user2', createTestCommitment({
			itcStamp: stamp2,
			timestamp: 1010
		}));
		
		networkCommitments.update('user3', createTestCommitment({
			itcStamp: stamp3,
			timestamp: 1020
		}));
		
		// User composes during this activity
		myCommitmentStore.set(createTestCommitment({
			itcStamp: itcSeed(),
			timestamp: 990
		}));
		
		const composed = composeCommitmentFromSources();
		expect(composed).not.toBeNull();
		if (!composed) return;
		
		// ✅ Should include ALL network ITCs
		expect(itcLeq(stamp1, composed.itcStamp)).toBe(true);
		expect(itcLeq(stamp2, composed.itcStamp)).toBe(true);
		expect(itcLeq(stamp3, composed.itcStamp)).toBe(true);
		
		// ✅ All users should accept this update
		const result1 = networkCommitments.update('test_user', composed);
		expect(result1.applied).toBe(true);
		
		console.log('[TEST] ✅ Rapid network activity: All ITCs merged');
	});
	
	it('should increment ITC after merge (not before)', () => {
		// Network has stamp_A
		const stampA = itcEvent(itcSeed());
		networkCommitments.update('alice', createTestCommitment({
			itcStamp: stampA,
			timestamp: 1000
		}));
		
		// Local has stamp_B
		const stampB = itcEvent(itcSeed());
		myCommitmentStore.set(createTestCommitment({
			itcStamp: stampB,
			timestamp: 1001
		}));
		
		// Compose (should merge then increment)
		const composed = composeCommitmentFromSources();
		expect(composed).not.toBeNull();
		if (!composed) return;
		
		// Manual merge + increment to compare
		const manualMerged = itcJoin(stampB, stampA);
		const manualIncremented = itcEvent(manualMerged);
		
		// ✅ Should equal: merge(B, A) then increment
		expect(itcEquals(composed.itcStamp, manualIncremented)).toBe(true);
		
		console.log('[TEST] ✅ ITC incremented after merge (correct order)');
	});
	
	it('should handle empty network state gracefully', () => {
		// No network commitments
		expect(networkCommitments.get().size).toBe(0);
		
		// Local commitment
		myCommitmentStore.set(createTestCommitment({
			itcStamp: itcEvent(itcSeed()),
			timestamp: 1000
		}));
		
		// Compose should still work
		const composed = composeCommitmentFromSources();
		expect(composed).not.toBeNull();
		if (!composed) return;
		
		// ✅ Should have valid ITC (incremented from local)
		expect(composed.itcStamp).toBeDefined();
		expect(composed.itcStamp.id).toBeDefined();
		expect(composed.itcStamp.event).toBeDefined();
		
		console.log('[TEST] ✅ Empty network state: Handled gracefully');
	});
	
	it('should handle null local ITC gracefully', () => {
		// Network has stamps
		const stampA = itcEvent(itcSeed());
		networkCommitments.update('alice', createTestCommitment({
			itcStamp: stampA,
			timestamp: 1000
		}));
		
		// Local commitment with NO ITC
		myCommitmentStore.set(createTestCommitment({
			itcStamp: null as any,  // Simulate missing ITC
			timestamp: 1001
		}));
		
		// Compose should create seed ITC and merge
		const composed = composeCommitmentFromSources();
		expect(composed).not.toBeNull();
		if (!composed) return;
		
		// ✅ Should have valid ITC (seed + merge + increment)
		expect(composed.itcStamp).toBeDefined();
		expect(itcLeq(stampA, composed.itcStamp)).toBe(true);
		
		console.log('[TEST] ✅ Null local ITC: Created seed and merged');
	});
});

