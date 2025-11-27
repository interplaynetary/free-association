/**
 * Attribute Recognition - Fine-Grained Derived Stores Tests
 * 
 * Tests for deriveAttribute, deriveEntityAttribute, and deriveEntity functions.
 * These provide fine-grained reactivity similar to VersionedStore.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
	updateAttributeInCollection,
	removeAttributeFromCollection,
	type AttributeValue
} from './attribute-recognition';
import {
	deriveAttribute,
	deriveEntityAttribute,
	deriveEntity,
	myAttributeRecognitions
} from './attribute-recognition.svelte';

// ═══════════════════════════════════════════════════════════════════
// FINE-GRAINED DERIVED STORES TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Fine-Grained Derived Stores', () => {
	beforeEach(() => {
		// Reset store
		myAttributeRecognitions.set({ _timestamp: Date.now() });
	});
	
	describe('deriveAttribute', () => {
		it('should only update when specific attribute changes', () => {
			const membershipStore = deriveAttribute('membership');
			const callbackHistory: any[] = [];
			
			const unsubscribe = membershipStore.subscribe((map: Map<string, AttributeValue>) => {
				callbackHistory.push(Array.from(map.entries()));
			});
			
			// Initial state (empty)
			expect(callbackHistory).toHaveLength(1);
			expect(callbackHistory[0]).toEqual([]);
			
			// Add membership for org_1
			let collection = { _timestamp: Date.now() };
			collection = updateAttributeInCollection(collection, 'org_1', 'membership', ['alice', 'bob']);
			myAttributeRecognitions.set(collection);
			
			// Should trigger update
			expect(callbackHistory).toHaveLength(2);
			expect(callbackHistory[1]).toHaveLength(1);
			expect(callbackHistory[1][0][0]).toBe('org_1'); // entity_id
			expect(callbackHistory[1][0][1].value).toEqual(['alice', 'bob']);
			
			// Add different attribute to same entity (should NOT trigger)
			collection = updateAttributeInCollection(collection, 'org_1', 'location', { city: 'Berlin' });
			myAttributeRecognitions.set(collection);
			
			// Should NOT trigger (still length 2)
			expect(callbackHistory).toHaveLength(2);
			
			// Add membership for different entity (should trigger)
			collection = updateAttributeInCollection(collection, 'org_2', 'membership', ['charlie']);
			myAttributeRecognitions.set(collection);
			
			// Should trigger update
			expect(callbackHistory).toHaveLength(3);
			expect(callbackHistory[2]).toHaveLength(2); // Now 2 entities
			
			unsubscribe();
		});
		
		it('should handle attribute deletion', () => {
			const nameStore = deriveAttribute('name');
			const callbackHistory: any[] = [];
			
			const unsubscribe = nameStore.subscribe((map: Map<string, AttributeValue>) => {
				callbackHistory.push(map.size);
			});
			
			// Initial: 0 entities
			expect(callbackHistory[callbackHistory.length - 1]).toBe(0);
			
			// Add name for entity_1
			let collection = { _timestamp: Date.now() };
			collection = updateAttributeInCollection(collection, 'entity_1', 'name', 'Alice');
			myAttributeRecognitions.set(collection);
			
			// Now 1 entity
			expect(callbackHistory[callbackHistory.length - 1]).toBe(1);
			
			// Remove name attribute
			collection = removeAttributeFromCollection(collection, 'entity_1', 'name');
			myAttributeRecognitions.set(collection);
			
			// Back to 0
			expect(callbackHistory[callbackHistory.length - 1]).toBe(0);
			
			unsubscribe();
		});
		
		it('should track multiple entities independently', () => {
			const capacityStore = deriveAttribute('capacity:food');
			let latestMap: Map<string, AttributeValue> = new Map();
			
			const unsubscribe = capacityStore.subscribe((map: Map<string, AttributeValue>) => {
				latestMap = map;
			});
			
			let collection = { _timestamp: Date.now() };
			
			// Add capacity for 3 entities
			collection = updateAttributeInCollection(collection, 'alice', 'capacity:food', [{ id: 's1', quantity: 100 }]);
			collection = updateAttributeInCollection(collection, 'bob', 'capacity:food', [{ id: 's2', quantity: 200 }]);
			collection = updateAttributeInCollection(collection, 'charlie', 'capacity:food', [{ id: 's3', quantity: 300 }]);
			myAttributeRecognitions.set(collection);
			
			expect(latestMap.size).toBe(3);
			expect(latestMap.get('alice')?.value).toEqual([{ id: 's1', quantity: 100 }]);
			expect(latestMap.get('bob')?.value).toEqual([{ id: 's2', quantity: 200 }]);
			expect(latestMap.get('charlie')?.value).toEqual([{ id: 's3', quantity: 300 }]);
			
			unsubscribe();
		});
	});
	
	describe('deriveEntityAttribute', () => {
		it('should only update when specific entity+attribute changes', () => {
			const aliceMembershipStore = deriveEntityAttribute('org_alice', 'membership');
			const callbackHistory: any[] = [];
			
			const unsubscribe = aliceMembershipStore.subscribe((attr: AttributeValue | undefined) => {
				callbackHistory.push(attr?.value);
			});
			
			// Initial: undefined
			expect(callbackHistory).toHaveLength(1);
			expect(callbackHistory[0]).toBeUndefined();
			
			// Add membership for different entity (should NOT trigger)
			let collection = { _timestamp: Date.now() };
			collection = updateAttributeInCollection(collection, 'org_bob', 'membership', ['x', 'y']);
			myAttributeRecognitions.set(collection);
			
			// Should NOT trigger
			expect(callbackHistory).toHaveLength(1);
			
			// Add different attribute for our entity (should NOT trigger)
			collection = updateAttributeInCollection(collection, 'org_alice', 'location', { city: 'Berlin' });
			myAttributeRecognitions.set(collection);
			
			// Should NOT trigger
			expect(callbackHistory).toHaveLength(1);
			
			// Add membership for our entity (should trigger!)
			collection = updateAttributeInCollection(collection, 'org_alice', 'membership', ['alice', 'bob']);
			myAttributeRecognitions.set(collection);
			
			// Should trigger
			expect(callbackHistory).toHaveLength(2);
			expect(callbackHistory[1]).toEqual(['alice', 'bob']);
			
			// Update membership for our entity (should trigger)
			// ✅ No delay needed - ITC will always be different on update!
			collection = updateAttributeInCollection(collection, 'org_alice', 'membership', ['alice', 'bob', 'charlie']);
			myAttributeRecognitions.set(collection);
			
			// Should trigger
			expect(callbackHistory).toHaveLength(3);
			expect(callbackHistory[2]).toEqual(['alice', 'bob', 'charlie']);
			
			unsubscribe();
		});
		
		it('should return undefined when attribute does not exist', () => {
			const nameStore = deriveEntityAttribute('entity_1', 'name');
			let latestValue: AttributeValue | undefined;
			
			const unsubscribe = nameStore.subscribe((attr: AttributeValue | undefined) => {
				latestValue = attr;
			});
			
			// Initially undefined
			expect(latestValue).toBeUndefined();
			
			// Add different attribute
			let collection = { _timestamp: Date.now() };
			collection = updateAttributeInCollection(collection, 'entity_1', 'email', 'test@example.com');
			myAttributeRecognitions.set(collection);
			
			// Still undefined
			expect(latestValue).toBeUndefined();
			
			// Add the attribute we're watching
			collection = updateAttributeInCollection(collection, 'entity_1', 'name', 'Alice');
			myAttributeRecognitions.set(collection);
			
			// Now defined
			expect(latestValue?.value).toBe('Alice');
			
			unsubscribe();
		});
		
		it('should handle attribute deletion', () => {
			const nameStore = deriveEntityAttribute('entity_1', 'name');
			let latestValue: AttributeValue | undefined;
			
			const unsubscribe = nameStore.subscribe((attr: AttributeValue | undefined) => {
				latestValue = attr;
			});
			
			// Add attribute
			let collection = { _timestamp: Date.now() };
			collection = updateAttributeInCollection(collection, 'entity_1', 'name', 'Alice');
			myAttributeRecognitions.set(collection);
			
			expect(latestValue?.value).toBe('Alice');
			
			// Delete attribute
			collection = removeAttributeFromCollection(collection, 'entity_1', 'name');
			myAttributeRecognitions.set(collection);
			
			// Back to undefined
			expect(latestValue).toBeUndefined();
			
			unsubscribe();
		});
	});
	
	describe('deriveEntity', () => {
		it('should only update when any attribute of specific entity changes', () => {
			const aliceStore = deriveEntity('entity_alice');
			const callbackHistory: any[] = [];
			
			const unsubscribe = aliceStore.subscribe((attrs: Record<string, AttributeValue>) => {
				callbackHistory.push(Object.keys(attrs).length);
			});
			
			// Initial: no attributes
			expect(callbackHistory).toHaveLength(1);
			expect(callbackHistory[0]).toBe(0);
			
			// Add attribute to different entity (should NOT trigger)
			let collection = { _timestamp: Date.now() };
			collection = updateAttributeInCollection(collection, 'entity_bob', 'name', 'Bob');
			myAttributeRecognitions.set(collection);
			
			// Should NOT trigger
			expect(callbackHistory).toHaveLength(1);
			
			// Add attribute to our entity (should trigger)
			collection = updateAttributeInCollection(collection, 'entity_alice', 'name', 'Alice');
			myAttributeRecognitions.set(collection);
			
			// Should trigger - now 1 attribute
			expect(callbackHistory).toHaveLength(2);
			expect(callbackHistory[1]).toBe(1);
			
			// Add another attribute to our entity (should trigger)
			// ✅ No delay needed - ITC will always be different!
			collection = updateAttributeInCollection(collection, 'entity_alice', 'email', 'alice@example.com');
			myAttributeRecognitions.set(collection);
			
			// Should trigger - now 2 attributes
			expect(callbackHistory).toHaveLength(3);
			expect(callbackHistory[2]).toBe(2);
			
			// Update existing attribute (should trigger)
			// ✅ No delay needed - ITC will always be different!
			collection = updateAttributeInCollection(collection, 'entity_alice', 'name', 'Alice Smith');
			myAttributeRecognitions.set(collection);
			
			// Should trigger - still 2 attributes
			expect(callbackHistory).toHaveLength(4);
			expect(callbackHistory[3]).toBe(2);
			
			unsubscribe();
		});
		
		it('should provide all attributes of entity', () => {
			const orgStore = deriveEntity('org_123');
			let latestAttrs: Record<string, AttributeValue> = {};
			
			const unsubscribe = orgStore.subscribe((attrs: Record<string, AttributeValue>) => {
				latestAttrs = attrs;
			});
			
			// Add multiple attributes
			let collection = { _timestamp: Date.now() };
			collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice', 'bob']);
			collection = updateAttributeInCollection(collection, 'org_123', 'location', { city: 'Berlin' });
			collection = updateAttributeInCollection(collection, 'org_123', 'public_key', 'org_pub_123');
			myAttributeRecognitions.set(collection);
			
			// Should have all 3 attributes
			expect(Object.keys(latestAttrs)).toHaveLength(3);
			expect(latestAttrs.membership?.value).toEqual(['alice', 'bob']);
			expect(latestAttrs.location?.value).toEqual({ city: 'Berlin' });
			expect(latestAttrs.public_key?.value).toBe('org_pub_123');
			
			unsubscribe();
		});
		
		it('should return empty object when entity has no attributes', () => {
			const entityStore = deriveEntity('entity_1');
			let latestAttrs: Record<string, AttributeValue> = {};
			
			const unsubscribe = entityStore.subscribe((attrs: Record<string, AttributeValue>) => {
				latestAttrs = attrs;
			});
			
			// Initially empty
			expect(Object.keys(latestAttrs)).toHaveLength(0);
			
			// Add attributes to different entity
			let collection = { _timestamp: Date.now() };
			collection = updateAttributeInCollection(collection, 'entity_2', 'name', 'Bob');
			myAttributeRecognitions.set(collection);
			
			// Still empty
			expect(Object.keys(latestAttrs)).toHaveLength(0);
			
			unsubscribe();
		});
	});
	
	describe('Fine-Grained Reactivity Performance', () => {
		it('should prevent unnecessary subscriptions from firing', () => {
			// Create multiple derived stores
			const membershipStore = deriveAttribute('membership');
			const aliceNameStore = deriveEntityAttribute('entity_alice', 'name');
			const bobStore = deriveEntity('entity_bob');
			
			let membershipCallCount = 0;
			let aliceNameCallCount = 0;
			let bobCallCount = 0;
			
			const unsub1 = membershipStore.subscribe(() => membershipCallCount++);
			const unsub2 = aliceNameStore.subscribe(() => aliceNameCallCount++);
			const unsub3 = bobStore.subscribe(() => bobCallCount++);
			
			// Initial calls
			expect(membershipCallCount).toBe(1);
			expect(aliceNameCallCount).toBe(1);
			expect(bobCallCount).toBe(1);
			
			let collection = { _timestamp: Date.now() };
			
			// Add Alice's name - should only trigger aliceNameStore
			collection = updateAttributeInCollection(collection, 'entity_alice', 'name', 'Alice');
			myAttributeRecognitions.set(collection);
			
			expect(membershipCallCount).toBe(1); // ✅ Not triggered
			expect(aliceNameCallCount).toBe(2); // ✅ Triggered
			expect(bobCallCount).toBe(1); // ✅ Not triggered
			
			// Add Bob's email - should only trigger bobStore
			collection = updateAttributeInCollection(collection, 'entity_bob', 'email', 'bob@example.com');
			myAttributeRecognitions.set(collection);
			
			expect(membershipCallCount).toBe(1); // ✅ Not triggered
			expect(aliceNameCallCount).toBe(2); // ✅ Not triggered
			expect(bobCallCount).toBe(2); // ✅ Triggered
			
			// Add membership for org - should only trigger membershipStore
			collection = updateAttributeInCollection(collection, 'org_1', 'membership', ['alice', 'bob']);
			myAttributeRecognitions.set(collection);
			
			expect(membershipCallCount).toBe(2); // ✅ Triggered
			expect(aliceNameCallCount).toBe(2); // ✅ Not triggered
			expect(bobCallCount).toBe(2); // ✅ Not triggered
			
			unsub1();
			unsub2();
			unsub3();
		});
	});
});

