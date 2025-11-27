/**
 * Unified Entity System - Tests
 * 
 * Tests for the unified entity/attribute system that treats all entities
 * (contacts, organizations, persons) as generic UUID → attributes mappings.
 * 
 * Coverage:
 * - Entity conceptual model
 * - Entity ID resolution patterns
 * - Attribute naming conventions
 * 
 * Note: Full integration tests with stores are in attribute-recognition.test.ts
 * These tests focus on the conceptual model and pure function behavior.
 */

import { describe, it, expect } from 'vitest';
import type { AttributeRecognitionsCollection } from '$lib/protocol/schemas';
import {
	updateAttributeInCollection,
	getAttributeFromCollection,
	getAllAttributesForEntity,
	getEntitiesWithAttribute
} from '$lib/protocol/attributes/attribute-recognition';

describe('Entity Attribute Operations', () => {
	it('should set entity attribute using core functions', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice', 'bob']);
		
		const attr = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attr?.value).toEqual(['alice', 'bob']);
	});

	it('should get entity attribute using core functions', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice', 'bob']);
		collection = updateAttributeInCollection(collection, 'org_123', 'location', { city: 'Berlin' });
		
		expect(getAttributeFromCollection(collection, 'org_123', 'membership')?.value).toEqual(['alice', 'bob']);
		expect(getAttributeFromCollection(collection, 'org_123', 'location')?.value).toEqual({ city: 'Berlin' });
	});

	it('should return undefined for non-existent attribute', () => {
		const collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		const attr = getAttributeFromCollection(collection, 'org_999', 'membership');
		expect(attr).toBeUndefined();
	});

	it('should get all attributes for an entity', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice']);
		collection = updateAttributeInCollection(collection, 'org_123', 'location', { city: 'Berlin' });
		collection = updateAttributeInCollection(collection, 'org_123', 'description', 'A test org');
		
		const attrs = getAllAttributesForEntity(collection, 'org_123');
		expect(Object.keys(attrs)).toContain('membership');
		expect(Object.keys(attrs)).toContain('location');
		expect(Object.keys(attrs)).toContain('description');
	});

	it('should return empty object for entity with no attributes', () => {
		const collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		const attrs = getAllAttributesForEntity(collection, 'org_999');
		expect(attrs).toEqual({});
	});
});

describe('Entity ID Resolution Patterns', () => {
	it('should use contact_ prefix for person entities', () => {
		const contactId = 'contact_alice';
		expect(contactId.startsWith('contact_')).toBe(true);
	});

	it('should use org_ prefix for organization entities', () => {
		const orgId = 'org_garden';
		expect(orgId.startsWith('org_')).toBe(true);
	});

	it('should use pubkeys directly for network entities', () => {
		const pubkey = 'pubkey_direct_123';
		expect(pubkey.startsWith('contact_')).toBe(false);
		expect(pubkey.startsWith('org_')).toBe(false);
	});

	it('should recognize UUIDs as generic entity identifiers', () => {
		// Both contact_id and org_id are UUIDs - just naming conventions
		const contactId = 'contact_550e8400-e29b-41d4-a716-446655440000';
		const orgId = 'org_550e8400-e29b-41d4-a716-446655440001';
		
		expect(contactId.includes('-')).toBe(true); // UUID format
		expect(orgId.includes('-')).toBe(true); // UUID format
	});
});

describe('Entity Queries', () => {
	it('should get all entities with specific attribute', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice']);
		collection = updateAttributeInCollection(collection, 'org_456', 'membership', ['bob']);
		collection = updateAttributeInCollection(collection, 'org_789', 'location', { city: 'Berlin' });
		
		const entities = getEntitiesWithAttribute(collection, 'membership');
		expect(entities).toContain('org_123');
		expect(entities).toContain('org_456');
		expect(entities).not.toContain('org_789');
	});

	it('should return empty array when no entities have attribute', () => {
		const collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		const entities = getEntitiesWithAttribute(collection, 'nonexistent');
		expect(entities).toEqual([]);
	});

	it('should handle multiple attributes per entity', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice']);
		collection = updateAttributeInCollection(collection, 'org_123', 'capacity:food', [{ id: 'slot1' }]);
		collection = updateAttributeInCollection(collection, 'org_123', 'need:housing', [{ id: 'need1' }]);
		
		expect(getEntitiesWithAttribute(collection, 'membership')).toContain('org_123');
		expect(getEntitiesWithAttribute(collection, 'capacity:food')).toContain('org_123');
		expect(getEntitiesWithAttribute(collection, 'need:housing')).toContain('org_123');
	});
});

describe('Unified Entity Model', () => {
	it('should treat contact_id as generic entity', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'contact_alice', 'name', 'Alice');
		collection = updateAttributeInCollection(collection, 'contact_alice', 'email', 'alice@example.com');
		collection = updateAttributeInCollection(collection, 'contact_alice', 'skill:javascript', { level: 8 });
		
		const attrs = getAllAttributesForEntity(collection, 'contact_alice');
		expect((attrs.name as any).value).toBe('Alice');
		expect((attrs.email as any).value).toBe('alice@example.com');
		expect((attrs['skill:javascript'] as any).value).toEqual({ level: 8 });
	});

	it('should treat org_id as generic entity', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'org_garden', 'name:en', 'Community Garden');
		collection = updateAttributeInCollection(collection, 'org_garden', 'membership', ['alice', 'bob']);
		collection = updateAttributeInCollection(collection, 'org_garden', 'description', 'A collaborative space');
		
		const attrs = getAllAttributesForEntity(collection, 'org_garden');
		expect((attrs['name:en'] as any).value).toBe('Community Garden');
		expect((attrs.membership as any).value).toEqual(['alice', 'bob']);
		expect((attrs.description as any).value).toBe('A collaborative space');
	});

	it('should treat pubkey as generic entity', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'pubkey_bob', 'capacity:food', [{ id: 'slot1', quantity: 100 }]);
		collection = updateAttributeInCollection(collection, 'pubkey_bob', 'need:housing', [{ id: 'need1', quantity: 1 }]);
		
		const attrs = getAllAttributesForEntity(collection, 'pubkey_bob');
		expect((attrs['capacity:food'] as any).value).toEqual([{ id: 'slot1', quantity: 100 }]);
		expect((attrs['need:housing'] as any).value).toEqual([{ id: 'need1', quantity: 1 }]);
	});

	it('should support entity semantic inference from attributes', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Person entity (has public_key attribute)
		collection = updateAttributeInCollection(collection, 'contact_alice', 'public_key', 'pubkey_alice_full');
		collection = updateAttributeInCollection(collection, 'contact_alice', 'name', 'Alice');
		
		const personAttrs = getAllAttributesForEntity(collection, 'contact_alice');
		expect((personAttrs.public_key as any).value).toBe('pubkey_alice_full'); // → Person
		expect((personAttrs.name as any).value).toBe('Alice');
		
		// Organization entity (has membership attribute)
		collection = updateAttributeInCollection(collection, 'org_garden', 'membership', ['alice', 'bob']);
		collection = updateAttributeInCollection(collection, 'org_garden', 'name:en', 'Garden');
		
		const orgAttrs = getAllAttributesForEntity(collection, 'org_garden');
		expect((orgAttrs.membership as any).value).toEqual(['alice', 'bob']); // → Organization
		expect((orgAttrs['name:en'] as any).value).toBe('Garden');
	});
});

describe('Complex Attribute Values', () => {
	it('should handle array attributes', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice', 'bob', 'charlie']);
		expect(getAttributeFromCollection(collection, 'org_123', 'membership')?.value).toEqual(['alice', 'bob', 'charlie']);
	});

	it('should handle object attributes', () => {
		const location = { city: 'Berlin', country: 'Germany', coords: [52.52, 13.40] };
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'org_123', 'location', location);
		expect(getAttributeFromCollection(collection, 'org_123', 'location')?.value).toEqual(location);
	});

	it('should handle nested object attributes', () => {
		const capacity = {
			slots: [
				{ id: 'slot1', quantity: 100, unit: 'kg' },
				{ id: 'slot2', quantity: 50, unit: 'kg' }
			],
			metadata: {
				updated: Date.now(),
				verified: true
			}
		};
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'pubkey_bob', 'capacity:food', capacity);
		expect(getAttributeFromCollection(collection, 'pubkey_bob', 'capacity:food')?.value).toEqual(capacity);
	});

	it('should handle primitive attributes', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'contact_alice', 'name', 'Alice');
		collection = updateAttributeInCollection(collection, 'contact_alice', 'age', 30);
		collection = updateAttributeInCollection(collection, 'contact_alice', 'verified', true);
		
		expect(getAttributeFromCollection(collection, 'contact_alice', 'name')?.value).toBe('Alice');
		expect(getAttributeFromCollection(collection, 'contact_alice', 'age')?.value).toBe(30);
		expect(getAttributeFromCollection(collection, 'contact_alice', 'verified')?.value).toBe(true);
	});
});

describe('Attribute Overwriting', () => {
	it('should overwrite existing attribute', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice']);
		expect(getAttributeFromCollection(collection, 'org_123', 'membership')?.value).toEqual(['alice']);
		
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['bob']);
		expect(getAttributeFromCollection(collection, 'org_123', 'membership')?.value).toEqual(['bob']);
		
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice', 'bob', 'charlie']);
		expect(getAttributeFromCollection(collection, 'org_123', 'membership')?.value).toEqual(['alice', 'bob', 'charlie']);
	});

	it('should allow changing attribute type', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		collection = updateAttributeInCollection(collection, 'org_123', 'data', 'string value');
		expect(getAttributeFromCollection(collection, 'org_123', 'data')?.value).toBe('string value');
		
		collection = updateAttributeInCollection(collection, 'org_123', 'data', 42);
		expect(getAttributeFromCollection(collection, 'org_123', 'data')?.value).toBe(42);
		
		collection = updateAttributeInCollection(collection, 'org_123', 'data', ['array', 'value']);
		expect(getAttributeFromCollection(collection, 'org_123', 'data')?.value).toEqual(['array', 'value']);
		
		collection = updateAttributeInCollection(collection, 'org_123', 'data', { object: 'value' });
		expect(getAttributeFromCollection(collection, 'org_123', 'data')?.value).toEqual({ object: 'value' });
	});
});

describe('Multiple Entities and Attributes', () => {
	it('should handle many entities with many attributes', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// Create 10 organizations with various attributes
		for (let i = 0; i < 10; i++) {
			const orgId = `org_${i}`;
			collection = updateAttributeInCollection(collection, orgId, 'membership', [`member_${i}_1`, `member_${i}_2`]);
			collection = updateAttributeInCollection(collection, orgId, 'location', { city: `City${i}` });
			collection = updateAttributeInCollection(collection, orgId, 'description', `Description for org ${i}`);
		}
		
		// Verify all entities exist
		const orgsWithMembership = getEntitiesWithAttribute(collection, 'membership');
		expect(orgsWithMembership).toHaveLength(10);
		
		// Verify specific entity
		const org5Attrs = getAllAttributesForEntity(collection, 'org_5');
		expect((org5Attrs.membership as any).value).toEqual(['member_5_1', 'member_5_2']);
		expect((org5Attrs.location as any).value).toEqual({ city: 'City5' });
		expect((org5Attrs.description as any).value).toBe('Description for org 5');
	});

	it('should handle entity isolation', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice']);
		collection = updateAttributeInCollection(collection, 'org_456', 'membership', ['bob']);
		
		// Changes to one entity should not affect others
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['charlie']);
		
		expect(getAttributeFromCollection(collection, 'org_123', 'membership')?.value).toEqual(['charlie']);
		expect(getAttributeFromCollection(collection, 'org_456', 'membership')?.value).toEqual(['bob']);
	});
});

describe('Attribute System Integration', () => {
	it('should maintain metadata through core functions', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice']);
		
		const attr = getAttributeFromCollection(collection, 'org_123', 'membership');
		
		expect(attr?.confidence).toBeDefined();
		expect(attr?.timestamp).toBeDefined();
		expect(attr?.itcStamp).toBeDefined();
	});

	it('should maintain ITC causality through operations', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice']);
		const itc1 = getAttributeFromCollection(collection, 'org_123', 'membership')?.itcStamp;
		
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice', 'bob']);
		const itc2 = getAttributeFromCollection(collection, 'org_123', 'membership')?.itcStamp;
		
		expect(itc1).toBeDefined();
		expect(itc2).toBeDefined();
		expect(itc2).not.toEqual(itc1); // ITC should advance
	});
	
	it('should support source tracking', () => {
		let collection: AttributeRecognitionsCollection = { _timestamp: Date.now() };
		
		// With source
		collection = updateAttributeInCollection(collection, 'org_123', 'membership', ['alice'], 'pubkey_alice');
		const attrWithSource = getAttributeFromCollection(collection, 'org_123', 'membership');
		expect(attrWithSource?.source_pubkey).toBe('pubkey_alice');
		
		// Without source (local)
		collection = updateAttributeInCollection(collection, 'org_456', 'membership', ['bob'], undefined);
		const attrLocal = getAttributeFromCollection(collection, 'org_456', 'membership');
		expect(attrLocal?.source_pubkey).toBeUndefined();
	});
});

