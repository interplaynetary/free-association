/**
 * Attribute Recognition System - Tests
 * 
 * Tests for the generalized attribute recognition system.
 * 
 * Coverage:
 * - Core recognition functions
 * - Attribute resolution with priority logic
 * - Type-specific parsing and validation
 * - Backward compatibility adapters
 * - ITC causality tracking
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
	recognizeAttribute,
	updateAttributeInCollection,
	getAttributeFromCollection,
	removeAttributeFromCollection,
	getAllAttributesForEntity,
	getEntitiesWithAttribute,
	subscribeToAttribute,
	unsubscribeFromAttribute,
	getSubscriptionSource,
	resolveEntityId,
	addEntityIdMapping,
	mergeAttributeCollections
} from './attribute-recognition';
// Note: Resolution functions now in attribute-recognition.svelte.ts (unified storage)
import {
	parseMembershipAttribute,
	parseCapacityAttribute,
	parseNeedAttribute,
	parseSkillAttribute,
	parseLocationAttribute,
	detectAttributeType,
	extractNeedType,
	createCapacityAttributeName,
	createNeedAttributeName
} from './attribute-types';
// Backward compatibility adapters removed - pure implementation only!
import type {
	AttributeRecognitionsCollection,
	AttributeSubscriptions,
	EntityIdMappings
} from '$lib/protocol/schemas';

// ═══════════════════════════════════════════════════════════════════
// CORE RECOGNITION TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Core Recognition Functions', () => {
	it('should recognize an attribute with default values', () => {
		const attr = recognizeAttribute('org_123', 'membership', ['alice', 'bob']);
		
		expect(attr.value).toEqual(['alice', 'bob']);
		expect(attr.confidence).toBe(1.0);
		expect(attr.source_pubkey).toBeUndefined();
		expect(attr.timestamp).toBeGreaterThan(0);
		expect(attr.itcStamp).toBeDefined();
	});
	
	it('should recognize an attribute with source and confidence', () => {
		const attr = recognizeAttribute(
			'pubkey_bob',
			'capacity:food',
			[{ id: 'slot1', quantity: 100 }],
			'pubkey_bob',
			0.8
		);
		
		expect(attr.source_pubkey).toBe('pubkey_bob');
		expect(attr.confidence).toBe(0.8);
	});
	
	it('should throw error for invalid confidence', () => {
		expect(() => {
			recognizeAttribute('org_123', 'membership', ['alice'], undefined, 1.5);
		}).toThrow('Confidence must be between 0 and 1');
	});
	
	it('should update attribute in collection', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice', 'bob']);
		
		const attr = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr?.value).toEqual(['alice', 'bob']);
		expect(collection._itcStamp).toBeDefined();
	});
	
	it('should get attribute from collection', () => {
		const collection = updateAttributeInCollection(
			{ _timestamp: Date.now() },
			'org_123',
			'membership',
			['alice']
		);
		
		const attr = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr?.value).toEqual(['alice']);
	});
	
	it('should return undefined for non-existent attribute', () => {
		const collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		const attr = getAttributeFromCollection(collection, 'org_999', 'membership');
		expect(attr).toBeUndefined();
	});
	
	it('should remove attribute from collection', () => {
		let collection = updateAttributeInCollection(
			{ _timestamp: Date.now() },
			'org_123',
			'membership',
			['alice']
		);
		
		collection = removeAttributeFromCollection(collection, 'org_123', 'membership');
		
		const attr = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr).toBeUndefined();
	});
	
	it('should get all attributes for an entity', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice']);
		collection = updateAttributeInCollection(collection, 'org_123', 'location', { city: 'Berlin' });
		
		const attrs = getAllAttributesForEntity(collection, 'org_123');
		expect(Object.keys(attrs)).toContain('membership');
		expect(Object.keys(attrs)).toContain('location');
	});
	
	it('should get entities with a specific attribute', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice']);
		collection = updateAttributeInCollection(collection, 'org_456', 'membership', ['bob']);
		collection = updateAttributeInCollection(collection, 'org_789', 'location', { city: 'Berlin' });
		
		const entities = getEntitiesWithAttribute(collection, 'membership');
		expect(entities).toContain('org_123');
		expect(entities).toContain('org_456');
		expect(entities).not.toContain('org_789');
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Subscription Functions', () => {
	it('should subscribe to an attribute', () => {
		const subs: AttributeSubscriptions = {};
		const updated = subscribeToAttribute(subs, 'org_123', 'membership', 'pubkey_alice');
		
		expect(updated['org_123']?.['membership']).toBe('pubkey_alice');
	});
	
	it('should unsubscribe from an attribute', () => {
		let subs: AttributeSubscriptions = {};
		subs = subscribeToAttribute(subs, 'org_123', 'membership', 'pubkey_alice');
		subs = unsubscribeFromAttribute(subs, 'org_123', 'membership');
		
		expect(subs['org_123']).toBeUndefined();
	});
	
	it('should get subscription source', () => {
		const subs: AttributeSubscriptions = {
			'org_123': {
				'membership': 'pubkey_alice'
			}
		};
		
		const source = getSubscriptionSource(subs, 'org_123', 'membership');
		expect(source).toBe('pubkey_alice');
	});
});

// ═══════════════════════════════════════════════════════════════════
// ENTITY ID RESOLUTION TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Entity ID Resolution', () => {
	it('should resolve entity ID from mappings', () => {
		const mappings: EntityIdMappings = {
			'contact_alice': 'pubkey_alice_full'
		};
		
		const resolved = resolveEntityId('contact_alice', mappings);
		expect(resolved).toBe('pubkey_alice_full');
	});
	
	it('should return original ID if no mapping exists', () => {
		const mappings: EntityIdMappings = {};
		const resolved = resolveEntityId('pubkey_bob', mappings);
		expect(resolved).toBe('pubkey_bob');
	});
	
	it('should add entity ID mapping', () => {
		const mappings: EntityIdMappings = {};
		const updated = addEntityIdMapping(mappings, 'contact_alice', 'pubkey_alice');
		
		expect(updated['contact_alice']).toBe('pubkey_alice');
	});
});

// ═══════════════════════════════════════════════════════════════════
// RESOLUTION TESTS (Unified Storage)
// ═══════════════════════════════════════════════════════════════════

describe('Resolution with Unified Storage', () => {
	it('should resolve subscription data (has matching source_pubkey)', () => {
		// Subscription data was written to myAttributeRecognitions with source_pubkey
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(
			collection,
			'org_123',
			'membership',
			['alice', 'bob'],
			'pubkey_alice',  // ← Source from subscription
			1.0
		);
		
		// Check it has the right source
		const attr = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr?.value).toEqual(['alice', 'bob']);
		expect(attr?.source_pubkey).toBe('pubkey_alice');
	});
	
	it('should resolve local recognition (no source_pubkey)', () => {
		// Local recognition has no source_pubkey
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(
			collection,
			'org_123',
			'membership',
			['charlie'],
			undefined,  // ← No source = local
			1.0
		);
		
		const attr = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr?.value).toEqual(['charlie']);
		expect(attr?.source_pubkey).toBeUndefined();
	});
	
	it('should return undefined for not found', () => {
		const collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		const attr = getAttributeFromCollection(collection, 'org_999', 'membership');
		expect(attr).toBeUndefined();
	});
	
	it('should distinguish subscription from self-declaration', () => {
		// Self-declaration: source_pubkey matches entity_id
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(
			collection,
			'pubkey_bob',
			'capacity:food',
			[{ id: 'slot1', quantity: 100 }],
			'pubkey_bob',  // ← Self-declaration
			1.0
		);
		
		const attr = getAttributeFromCollection(collection, 'pubkey_bob', 'capacity:food');
		expect(attr?.source_pubkey).toBe('pubkey_bob');
		// Resolution type would be determined by comparing source_pubkey with entity_id
	});
});

// ═══════════════════════════════════════════════════════════════════
// TYPE-SPECIFIC PARSING TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Type-Specific Parsing', () => {
	describe('Membership Attributes', () => {
		it('should parse valid membership array', () => {
			const members = parseMembershipAttribute(['alice', 'bob', 'charlie']);
			expect(members).toEqual(['alice', 'bob', 'charlie']);
		});
		
		it('should deduplicate membership array', () => {
			const members = parseMembershipAttribute(['alice', 'bob', 'alice']);
			expect(members).toEqual(['alice', 'bob']);
		});
		
		it('should throw error for invalid membership', () => {
			expect(() => parseMembershipAttribute('not-an-array')).toThrow();
			expect(() => parseMembershipAttribute([123])).toThrow();
		});
	});
	
	describe('Skill Attributes', () => {
		it('should parse valid skill', () => {
			const skill = parseSkillAttribute({
				level: 8,
				years: 5,
				description: 'Expert TypeScript developer'
			});
			
			expect(skill.level).toBe(8);
			expect(skill.years).toBe(5);
		});
		
		it('should throw error for invalid skill level', () => {
			expect(() => parseSkillAttribute({ level: 11 })).toThrow();
			expect(() => parseSkillAttribute({ level: 0 })).toThrow();
		});
	});
	
	describe('Location Attributes', () => {
		it('should parse valid location', () => {
			const location = parseLocationAttribute({
				city: 'Berlin',
				country: 'Germany',
				coords: [52.5200, 13.4050]
			});
			
			expect(location.city).toBe('Berlin');
			expect(location.coords).toEqual([52.5200, 13.4050]);
		});
		
		it('should throw error for invalid coordinates', () => {
			expect(() => parseLocationAttribute({ coords: [100, 200] })).toThrow();
		});
	});
	
	describe('Attribute Type Detection', () => {
		it('should detect membership type', () => {
			expect(detectAttributeType('membership')).toBe('membership');
		});
		
		it('should detect capacity type', () => {
			expect(detectAttributeType('capacity:food')).toBe('capacity');
		});
		
		it('should detect need type', () => {
			expect(detectAttributeType('need:housing')).toBe('need');
		});
		
		it('should detect skill type', () => {
			expect(detectAttributeType('skill:javascript')).toBe('skill');
		});
		
		it('should extract need type', () => {
			expect(extractNeedType('capacity:food')).toBe('food');
			expect(extractNeedType('need:housing')).toBe('housing');
			expect(extractNeedType('membership')).toBeUndefined();
		});
		
		it('should create attribute names', () => {
			expect(createCapacityAttributeName('food')).toBe('capacity:food');
			expect(createNeedAttributeName('housing')).toBe('need:housing');
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// BACKWARD COMPATIBILITY REMOVED - Pure attribute implementation! ✨
// ═══════════════════════════════════════════════════════════════════
// All adapter tests removed - no longer needed with pure implementation

// ═══════════════════════════════════════════════════════════════════
// COLLECTION MERGE TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Collection Merging', () => {
	it('should merge two attribute collections', () => {
		const collection1: AttributeRecognitionsCollection = { _timestamp: 1000 };
		const updated1 = updateAttributeInCollection(collection1, 'org_123', 'membership', ['alice']);
		
		const collection2: AttributeRecognitionsCollection = { _timestamp: 2000 };
		const updated2 = updateAttributeInCollection(collection2, 'org_456', 'membership', ['bob']);
		
		const merged = mergeAttributeCollections(updated1, updated2);
		
		const members123 = getAttributeFromCollection(merged, 'org_123', 'membership');
		const members456 = getAttributeFromCollection(merged, 'org_456', 'membership');
		
		expect(members123?.value).toEqual(['alice']);
		expect(members456?.value).toEqual(['bob']);
		expect(merged._timestamp).toBeGreaterThanOrEqual(2000); // Uses max of both timestamps
	});
	
	it('should prefer newer values when merging', () => {
		const olderTimestamp = Date.now() - 10000;
		const newerTimestamp = Date.now();
		
		const collection1: AttributeRecognitionsCollection = { _timestamp: olderTimestamp };
		const updated1 = updateAttributeInCollection(collection1, 'org_123', 'membership', ['old_data']);
		
		const collection2: AttributeRecognitionsCollection = { _timestamp: newerTimestamp };
		const updated2 = updateAttributeInCollection(collection2, 'org_123', 'membership', ['new_data']);
		
		const merged = mergeAttributeCollections(updated1, updated2);
		
		const members = getAttributeFromCollection(merged, 'org_123', 'membership');
		expect(members?.value).toEqual(['new_data']); // Newer value wins
	});
});

// ═══════════════════════════════════════════════════════════════════
// ITC CONFLICT RESOLUTION TESTS
// ═══════════════════════════════════════════════════════════════════

describe('ITC Causality and Conflict Resolution', () => {
	it('should handle causally ordered updates correctly', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// First update
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice']);
		const attr1 = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr1?.value).toEqual(['alice']);
		
		// Second update (causally later)
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice', 'bob']);
		const attr2 = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr2?.value).toEqual(['alice', 'bob']);
	});
	
	it('should preserve ITC stamps across updates', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice']);
		const attr1 = getAttributeFromCollection(collection, 'org_123', 'membership');
		const itc1 = attr1?.itcStamp;
		expect(itc1).toBeDefined();
		
		// Update should increment ITC
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice', 'bob']);
		const attr2 = getAttributeFromCollection(collection, 'org_123', 'membership');
		const itc2 = attr2?.itcStamp;
		expect(itc2).toBeDefined();
		expect(itc2).not.toEqual(itc1); // ITC changed
	});
	
	it('should handle multiple entities with independent ITC stamps', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice']);
		collection = updateAttributeInCollection(collection, 'org_456', 'membership', ['bob']);
		collection = updateAttributeInCollection(collection, 'org_789', 'membership', ['charlie']);
		
		const attr123 = getAttributeFromCollection(collection, 'org_123', 'membership');
		const attr456 = getAttributeFromCollection(collection, 'org_456', 'membership');
		const attr789 = getAttributeFromCollection(collection, 'org_789', 'membership');
		
		expect(attr123?.itcStamp).toBeDefined();
		expect(attr456?.itcStamp).toBeDefined();
		expect(attr789?.itcStamp).toBeDefined();
		
		// Each entity should have its own ITC stamp
		expect(attr123?.value).toEqual(['alice']);
		expect(attr456?.value).toEqual(['bob']);
		expect(attr789?.value).toEqual(['charlie']);
	});
	
	it('should handle multiple attributes per entity with independent ITC', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice']);
		collection = updateAttributeInCollection(collection, 'org_123', 'location', { city: 'Berlin' });
		collection = updateAttributeInCollection(collection, 'org_123', 'description', 'A test org');
		
		const membership = getAttributeFromCollection(collection, 'org_123', 'membership');
		const location = getAttributeFromCollection(collection, 'org_123', 'location');
		const description = getAttributeFromCollection(collection, 'org_123', 'description');
		
		expect(membership?.value).toEqual(['alice']);
		expect(location?.value).toEqual({ city: 'Berlin' });
		expect(description?.value).toBe('A test org');
		
		// Each attribute should have its own ITC
		expect(membership?.itcStamp).toBeDefined();
		expect(location?.itcStamp).toBeDefined();
		expect(description?.itcStamp).toBeDefined();
	});
});

// ═══════════════════════════════════════════════════════════════════
// EDGE CASES AND ERROR HANDLING
// ═══════════════════════════════════════════════════════════════════

describe('Edge Cases and Error Handling', () => {
	it('should handle empty entity IDs', () => {
		expect(() => {
			updateAttributeInCollection({ _timestamp: Date.now() }, '', 'membership', ['alice']);
		}).not.toThrow(); // Empty IDs are technically valid (though not recommended)
	});
	
	it('should handle empty attribute names', () => {
		expect(() => {
			updateAttributeInCollection({ _timestamp: Date.now() }, 'org_123', '', ['alice']);
		}).not.toThrow(); // Empty attribute names are technically valid
	});
	
	it('should handle null and undefined values', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		collection = updateAttributeInCollection(collection, 'org_123', 'nullable', null);
		const nullAttr = getAttributeFromCollection(collection, 'org_123', 'nullable');
		expect(nullAttr?.value).toBeNull();
		
		collection = updateAttributeInCollection(collection, 'org_123', 'undefinable', undefined);
		const undefinedAttr = getAttributeFromCollection(collection, 'org_123', 'undefinable');
		expect(undefinedAttr?.value).toBeUndefined();
	});
	
	it('should handle complex nested objects', () => {
		const complexValue = {
			nested: {
				deeply: {
					values: [1, 2, 3],
					more: { data: 'test' }
				}
			},
			array: [{ id: 1 }, { id: 2 }]
		};
		
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'org_123', 'complex', complexValue);
		
		const attr = getAttributeFromCollection(collection, 'org_123', 'complex');
		expect(attr?.value).toEqual(complexValue);
	});
	
	it('should handle very large arrays', () => {
		const largeArray = Array.from({ length: 10000 }, (_, i) => `member_${i}`);
		
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', largeArray);
		
		const attr = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr?.value).toHaveLength(10000);
		expect(attr?.value[0]).toBe('member_0');
		expect(attr?.value[9999]).toBe('member_9999');
	});
	
	it('should handle special characters in entity IDs', () => {
		const specialIds = [
			'org:special:id',
			'contact@email.com',
			'org#with#hash',
			'contact_with_üñíçödé'
		];
		
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		for (const id of specialIds) {
			collection = updateAttributeInCollection(collection, id, 'test', 'value');
			const attr = getAttributeFromCollection(collection, id, 'test');
			expect(attr?.value).toBe('value');
		}
	});
	
	it('should handle overwriting existing attributes', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice']);
		expect(getAttributeFromCollection(collection, 'org_123', 'membership')?.value).toEqual(['alice']);
		
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['bob']);
		expect(getAttributeFromCollection(collection, 'org_123', 'membership')?.value).toEqual(['bob']);
		
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['charlie', 'dave']);
		expect(getAttributeFromCollection(collection, 'org_123', 'membership')?.value).toEqual(['charlie', 'dave']);
	});
	
	it('should maintain collection integrity after many operations', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Perform many mixed operations
		for (let i = 0; i < 100; i++) {
			const entityId = `org_${i % 10}`;
			const attrName = `attr_${i % 5}`;
			collection = updateAttributeInCollection(collection, entityId, attrName, i);
		}
		
		// Remove some
		for (let i = 0; i < 50; i++) {
			const entityId = `org_${i % 10}`;
			const attrName = `attr_${i % 5}`;
			if (i % 3 === 0) {
				collection = removeAttributeFromCollection(collection, entityId, attrName);
			}
		}
		
		// Verify collection is still valid
		expect(collection._timestamp).toBeDefined();
		expect(typeof collection._timestamp).toBe('number');
		
		// Should still be able to add new attributes
		collection = updateAttributeInCollection(collection, 'org_new', 'test', 'works');
		expect(getAttributeFromCollection(collection, 'org_new', 'test')?.value).toBe('works');
	});
});

// ═══════════════════════════════════════════════════════════════════
// SOURCE AND CONFIDENCE TRACKING
// ═══════════════════════════════════════════════════════════════════

describe('Source and Confidence Tracking', () => {
	it('should track source_pubkey correctly', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Local recognition (no source)
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice'], undefined);
		let attr = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr?.source_pubkey).toBeUndefined();
		
		// Source from another user
		collection = updateAttributeInCollection(collection, 'org_456', 'membership', ['bob'], 'pubkey_alice');
		attr = getAttributeFromCollection(collection, 'org_456', 'membership');
		expect(attr?.source_pubkey).toBe('pubkey_alice');
		
		// Self-declaration
		collection = updateAttributeInCollection(collection, 'pubkey_bob', 'capacity:food', [{ id: 'slot1' }], 'pubkey_bob');
		attr = getAttributeFromCollection(collection, 'pubkey_bob', 'capacity:food');
		expect(attr?.source_pubkey).toBe('pubkey_bob');
	});
	
	it('should track confidence levels correctly', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Default confidence (1.0)
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice']);
		let attr = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr?.confidence).toBe(1.0);
		
		// Custom confidence
		collection = updateAttributeInCollection(collection, 'org_456', 'membership', ['bob'], undefined, 0.8);
		attr = getAttributeFromCollection(collection, 'org_456', 'membership');
		expect(attr?.confidence).toBe(0.8);
		
		// Low confidence
		collection = updateAttributeInCollection(collection, 'org_789', 'membership', ['charlie'], undefined, 0.1);
		attr = getAttributeFromCollection(collection, 'org_789', 'membership');
		expect(attr?.confidence).toBe(0.1);
	});
	
	it('should validate confidence bounds', () => {
		const collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		expect(() => {
			updateAttributeInCollection(collection, 'org_123', 'membership', ['alice'], undefined, -0.1);
		}).toThrow('Confidence must be between 0 and 1');
		
		expect(() => {
			updateAttributeInCollection(collection, 'org_123', 'membership', ['alice'], undefined, 1.1);
		}).toThrow('Confidence must be between 0 and 1');
		
		// Edge values should work
		expect(() => {
			updateAttributeInCollection(collection, 'org_123', 'membership', ['alice'], undefined, 0.0);
		}).not.toThrow();
		
		expect(() => {
			updateAttributeInCollection(collection, 'org_123', 'membership', ['alice'], undefined, 1.0);
		}).not.toThrow();
	});
	
	it('should track timestamps', () => {
		const beforeTime = Date.now();
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice']);
		const afterTime = Date.now();
		
		const attr = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr?.timestamp).toBeGreaterThanOrEqual(beforeTime);
		expect(attr?.timestamp).toBeLessThanOrEqual(afterTime);
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION DATA MERGING TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Subscription Data Merging', () => {
	it('should distinguish local vs subscription data by source_pubkey', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Local recognition (no source_pubkey)
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['local_member'], undefined);
		const localAttr = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(localAttr?.source_pubkey).toBeUndefined();
		
		// Subscription data (has source_pubkey)
		collection = updateAttributeInCollection(collection, 'org_456', 'membership', ['sub_member'], 'pubkey_alice');
		const subAttr = getAttributeFromCollection(collection, 'org_456', 'membership');
		expect(subAttr?.source_pubkey).toBe('pubkey_alice');
	});
	
	it('should handle self-declaration (source_pubkey = entity_id)', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Self-declaration: Bob declares his own capacity
		collection = updateAttributeInCollection(
			collection,
			'pubkey_bob',
			'capacity:food',
			[{ id: 'slot1', quantity: 100 }],
			'pubkey_bob'  // ← Same as entity_id
		);
		
		const attr = getAttributeFromCollection(collection, 'pubkey_bob', 'capacity:food');
		expect(attr?.source_pubkey).toBe('pubkey_bob');
		expect(attr?.value).toEqual([{ id: 'slot1', quantity: 100 }]);
	});
	
	it('should support multiple sources for different attributes of same entity', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Alice declares org membership
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice', 'bob'], 'pubkey_alice');
		
		// Bob declares org location
		collection = updateAttributeInCollection(collection, 'org_123', 'location', { city: 'Berlin' }, 'pubkey_bob');
		
		// We declare org description locally
		collection = updateAttributeInCollection(collection, 'org_123', 'description', 'A great org', undefined);
		
		const membership = getAttributeFromCollection(collection, 'org_123', 'membership');
		const location = getAttributeFromCollection(collection, 'org_123', 'location');
		const description = getAttributeFromCollection(collection, 'org_123', 'description');
		
		expect(membership?.source_pubkey).toBe('pubkey_alice');
		expect(location?.source_pubkey).toBe('pubkey_bob');
		expect(description?.source_pubkey).toBeUndefined();
	});
	
	it('should handle subscription updates to same attribute', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Initial subscription data
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice'], 'pubkey_alice');
		let attr = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr?.value).toEqual(['alice']);
		expect(attr?.source_pubkey).toBe('pubkey_alice');
		
		// Update from same source
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice', 'bob'], 'pubkey_alice');
		attr = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr?.value).toEqual(['alice', 'bob']);
		expect(attr?.source_pubkey).toBe('pubkey_alice');
	});
	
	it('should handle source change (different user declares same attribute)', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Alice declares org membership
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice'], 'pubkey_alice');
		let attr = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr?.source_pubkey).toBe('pubkey_alice');
		
		// Bob declares org membership (overwrite)
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['bob'], 'pubkey_bob');
		attr = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr?.value).toEqual(['bob']);
		expect(attr?.source_pubkey).toBe('pubkey_bob');
	});
	
	it('should handle local override of subscription data', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Subscription data from Alice
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice'], 'pubkey_alice');
		let attr = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr?.source_pubkey).toBe('pubkey_alice');
		
		// Local override (we have different view)
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['bob', 'charlie'], undefined);
		attr = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr?.value).toEqual(['bob', 'charlie']);
		expect(attr?.source_pubkey).toBeUndefined(); // Now local
	});
});

// ═══════════════════════════════════════════════════════════════════
// MULTI-ENTITY OPERATIONS
// ═══════════════════════════════════════════════════════════════════

describe('Multi-Entity Operations', () => {
	it('should handle batch entity creation', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		const entities = [
			{ id: 'org_1', attr: 'membership', value: ['alice'] },
			{ id: 'org_2', attr: 'membership', value: ['bob'] },
			{ id: 'org_3', attr: 'membership', value: ['charlie'] },
			{ id: 'org_4', attr: 'membership', value: ['dave'] },
			{ id: 'org_5', attr: 'membership', value: ['eve'] }
		];
		
		for (const entity of entities) {
			collection = updateAttributeInCollection(collection, entity.id, entity.attr, entity.value);
		}
		
		// Verify all entities exist
		const allWithMembership = getEntitiesWithAttribute(collection, 'membership');
		expect(allWithMembership).toHaveLength(5);
		expect(allWithMembership).toContain('org_1');
		expect(allWithMembership).toContain('org_5');
	});
	
	it('should handle querying across multiple entity types', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Mix of contacts, orgs, and pubkeys with location attribute
		collection = updateAttributeInCollection(collection, 'contact_alice', 'location', { city: 'Berlin' });
		collection = updateAttributeInCollection(collection, 'org_garden', 'location', { city: 'Berlin' });
		collection = updateAttributeInCollection(collection, 'pubkey_bob', 'location', { city: 'Munich' });
		
		const allWithLocation = getEntitiesWithAttribute(collection, 'location');
		expect(allWithLocation).toHaveLength(3);
		expect(allWithLocation).toContain('contact_alice');
		expect(allWithLocation).toContain('org_garden');
		expect(allWithLocation).toContain('pubkey_bob');
	});
	
	it('should handle different attribute types across entities', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Organization: membership + location
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice', 'bob']);
		collection = updateAttributeInCollection(collection, 'org_123', 'location', { city: 'Berlin' });
		
		// Person: skills + location
		collection = updateAttributeInCollection(collection, 'contact_alice', 'skill:javascript', { level: 8 });
		collection = updateAttributeInCollection(collection, 'contact_alice', 'location', { city: 'Berlin' });
		
		// Verify entity-specific attributes (returns AttributeValue objects, not raw values)
		const orgAttrs = getAllAttributesForEntity(collection, 'org_123');
		const personAttrs = getAllAttributesForEntity(collection, 'contact_alice');
		
		expect(orgAttrs.membership).toBeDefined();
		expect(orgAttrs['skill:javascript']).toBeUndefined();
		
		expect(personAttrs['skill:javascript']).toBeDefined();
		expect(personAttrs.membership).toBeUndefined();
		
		// Both have location (check the value property)
		expect((orgAttrs.location as any).value).toEqual({ city: 'Berlin' });
		expect((personAttrs.location as any).value).toEqual({ city: 'Berlin' });
	});
});

// ═══════════════════════════════════════════════════════════════════
// CHANGE DETECTION TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Change Detection', () => {
	it('should detect value changes with deepEquals', () => {
		const { deepEquals } = require('./attribute-recognition');
		
		// Primitives
		expect(deepEquals(5, 5)).toBe(true);
		expect(deepEquals(5, 6)).toBe(false);
		expect(deepEquals('hello', 'hello')).toBe(true);
		expect(deepEquals('hello', 'world')).toBe(false);
		
		// Objects
		expect(deepEquals({ a: 1 }, { a: 1 })).toBe(true);
		expect(deepEquals({ a: 1 }, { a: 2 })).toBe(false);
		expect(deepEquals({ a: 1, b: 2 }, { b: 2, a: 1 })).toBe(true); // Order independent
		
		// Arrays
		expect(deepEquals([1, 2, 3], [1, 2, 3])).toBe(true);
		expect(deepEquals([1, 2, 3], [1, 2, 4])).toBe(false);
		expect(deepEquals([1, 2, 3], [1, 2])).toBe(false);
		
		// Nested structures
		expect(deepEquals({ a: { b: [1, 2] } }, { a: { b: [1, 2] } })).toBe(true);
		expect(deepEquals({ a: { b: [1, 2] } }, { a: { b: [1, 3] } })).toBe(false);
		
		// Date objects
		const date1 = new Date('2024-01-01');
		const date2 = new Date('2024-01-01');
		const date3 = new Date('2024-01-02');
		expect(deepEquals(date1, date2)).toBe(true);
		expect(deepEquals(date1, date3)).toBe(false);
		
		// Map objects
		const map1 = new Map([['a', 1], ['b', 2]]);
		const map2 = new Map([['a', 1], ['b', 2]]);
		const map3 = new Map([['a', 1], ['b', 3]]);
		expect(deepEquals(map1, map2)).toBe(true);
		expect(deepEquals(map1, map3)).toBe(false);
		
		// Set objects
		const set1 = new Set([1, 2, 3]);
		const set2 = new Set([1, 2, 3]);
		const set3 = new Set([1, 2, 4]);
		expect(deepEquals(set1, set2)).toBe(true);
		expect(deepEquals(set1, set3)).toBe(false);
		
		// Null and undefined
		expect(deepEquals(null, null)).toBe(true);
		expect(deepEquals(undefined, undefined)).toBe(true);
		expect(deepEquals(null, undefined)).toBe(false);
		expect(deepEquals(null, 0)).toBe(false);
	});
	
	it('should prevent unnecessary updates when value unchanged', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Initial update
		collection = updateAttributeInCollection(
			collection,
			'entity_1',
			'name',
			'Alice'
		);
		
		const firstAttr = getAttributeFromCollection(collection, 'entity_1', 'name');
		expect(firstAttr?.value).toBe('Alice');
		const firstTimestamp = firstAttr?.timestamp;
		const firstITC = firstAttr?.itcStamp;
		
		// Wait a bit to ensure timestamp would change
		const waitTime = 5;
		const startTime = Date.now();
		while (Date.now() - startTime < waitTime) {
			// busy wait
		}
		
		// Update with SAME value
		collection = updateAttributeInCollection(
			collection,
			'entity_1',
			'name',
			'Alice' // Same value!
		);
		
		const secondAttr = getAttributeFromCollection(collection, 'entity_1', 'name');
		
		// ✅ Value should be unchanged (same reference even!)
		expect(secondAttr?.value).toBe('Alice');
		
		// ✅ Timestamp should be updated (metadata changed)
		expect(secondAttr?.timestamp).toBeGreaterThan(firstTimestamp!);
		
		// ✅ ITC should be incremented (causality progressed)
		expect(secondAttr?.itcStamp).not.toEqual(firstITC);
	});
	
	it('should trigger update when value changes', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Initial update
		collection = updateAttributeInCollection(
			collection,
			'entity_1',
			'name',
			'Alice'
		);
		
		const firstAttr = getAttributeFromCollection(collection, 'entity_1', 'name');
		expect(firstAttr?.value).toBe('Alice');
		const firstTimestamp = firstAttr?.timestamp;
		
		// Update with DIFFERENT value
		collection = updateAttributeInCollection(
			collection,
			'entity_1',
			'name',
			'Bob' // Different value!
		);
		
		const secondAttr = getAttributeFromCollection(collection, 'entity_1', 'name');
		
		// ✅ Value should be changed
		expect(secondAttr?.value).toBe('Bob');
		
		// ✅ Timestamp should be updated
		expect(secondAttr?.timestamp).toBeGreaterThanOrEqual(firstTimestamp!);
	});
	
	it('should detect object value changes', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Initial object
		collection = updateAttributeInCollection(
			collection,
			'entity_1',
			'location',
			{ city: 'Berlin', country: 'Germany' }
		);
		
		const firstAttr = getAttributeFromCollection(collection, 'entity_1', 'location');
		const firstValue = firstAttr?.value;
		
		// Update with SAME object content (different reference)
		collection = updateAttributeInCollection(
			collection,
			'entity_1',
			'location',
			{ city: 'Berlin', country: 'Germany' }
		);
		
		const secondAttr = getAttributeFromCollection(collection, 'entity_1', 'location');
		
		// ✅ Value should be same content (but might be different reference)
		expect(secondAttr?.value).toEqual({ city: 'Berlin', country: 'Germany' });
		
		// ✅ Deep equality detected - value reference should be preserved
		expect(secondAttr?.value).toBe(firstValue);
	});
	
	it('should detect array value changes', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Initial array
		collection = updateAttributeInCollection(
			collection,
			'org_1',
			'membership',
			['alice', 'bob', 'charlie']
		);
		
		const firstAttr = getAttributeFromCollection(collection, 'org_1', 'membership');
		const firstValue = firstAttr?.value;
		
		// Update with SAME array content (different reference)
		collection = updateAttributeInCollection(
			collection,
			'org_1',
			'membership',
			['alice', 'bob', 'charlie']
		);
		
		const secondAttr = getAttributeFromCollection(collection, 'org_1', 'membership');
		
		// ✅ Value should be same content (and same reference - change detection worked!)
		expect(secondAttr?.value).toEqual(['alice', 'bob', 'charlie']);
		expect(secondAttr?.value).toBe(firstValue);
		
		// Update with DIFFERENT array content
		collection = updateAttributeInCollection(
			collection,
			'org_1',
			'membership',
			['alice', 'bob', 'dave']
		);
		
		const thirdAttr = getAttributeFromCollection(collection, 'org_1', 'membership');
		
		// ✅ Value should be changed
		expect(thirdAttr?.value).toEqual(['alice', 'bob', 'dave']);
		expect(thirdAttr?.value).not.toBe(firstValue);
	});
	
	it('should work with custom equality checkers', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Custom equality checker that only compares array length
		const lengthOnlyEquals = (a: any, b: any): boolean => {
			if (!Array.isArray(a) || !Array.isArray(b)) return false;
			return a.length === b.length;
		};
		
		// Initial value
		collection = updateAttributeInCollection(
			collection,
			'entity_1',
			'tags',
			['tag1', 'tag2']
		);
		
		const firstAttr = getAttributeFromCollection(collection, 'entity_1', 'tags');
		const firstValue = firstAttr?.value;
		
		// Update with different content but same length
		collection = updateAttributeInCollection(
			collection,
			'entity_1',
			'tags',
			['tag3', 'tag4'],
			undefined,
			1.0,
			undefined,
			lengthOnlyEquals // Custom checker
		);
		
		const secondAttr = getAttributeFromCollection(collection, 'entity_1', 'tags');
		
		// ✅ Value should be unchanged (custom equality checker said they're equal!)
		expect(secondAttr?.value).toBe(firstValue);
		expect(secondAttr?.value).toEqual(['tag1', 'tag2']); // Still original value!
	});
	
	it('should handle complex nested structures', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		const complexValue = {
			users: [
				{ id: 'alice', roles: ['admin', 'editor'] },
				{ id: 'bob', roles: ['viewer'] }
			],
			metadata: {
				created: new Date('2024-01-01'),
				tags: new Set(['urgent', 'important'])
			}
		};
		
		// Initial value
		collection = updateAttributeInCollection(
			collection,
			'entity_1',
			'config',
			complexValue
		);
		
		const firstAttr = getAttributeFromCollection(collection, 'entity_1', 'config');
		const firstValue = firstAttr?.value;
		
		// Update with identical structure (different reference)
		const identicalValue = {
			users: [
				{ id: 'alice', roles: ['admin', 'editor'] },
				{ id: 'bob', roles: ['viewer'] }
			],
			metadata: {
				created: new Date('2024-01-01'),
				tags: new Set(['urgent', 'important'])
			}
		};
		
		collection = updateAttributeInCollection(
			collection,
			'entity_1',
			'config',
			identicalValue
		);
		
		const secondAttr = getAttributeFromCollection(collection, 'entity_1', 'config');
		
		// ✅ Value should be preserved (deep equality detected)
		expect(secondAttr?.value).toBe(firstValue);
		
		// Update with different structure
		const changedValue = {
			users: [
				{ id: 'alice', roles: ['admin'] }, // Changed!
				{ id: 'bob', roles: ['viewer'] }
			],
			metadata: {
				created: new Date('2024-01-01'),
				tags: new Set(['urgent', 'important'])
			}
		};
		
		collection = updateAttributeInCollection(
			collection,
			'entity_1',
			'config',
			changedValue
		);
		
		const thirdAttr = getAttributeFromCollection(collection, 'entity_1', 'config');
		
		// ✅ Value should be changed
		expect(thirdAttr?.value).not.toBe(firstValue);
		expect(thirdAttr?.value.users[0].roles).toEqual(['admin']);
	});
});


