/**
 * Attribute Types Tests - Custom Equality Checkers
 */

import { describe, it, expect } from 'vitest';
import {
	membershipEquals,
	slotArrayEquals,
	getEqualityChecker
} from './attribute-types';
import type { AvailabilitySlot } from '$lib/protocol/schemas';

// ═══════════════════════════════════════════════════════════════════
// MEMBERSHIP EQUALITY TESTS
// ═══════════════════════════════════════════════════════════════════

describe('membershipEquals', () => {
	it('should return true for identical arrays', () => {
		expect(membershipEquals(['alice', 'bob'], ['alice', 'bob'])).toBe(true);
	});
	
	it('should return true for order-independent arrays', () => {
		expect(membershipEquals(['alice', 'bob', 'charlie'], ['charlie', 'alice', 'bob'])).toBe(true);
		expect(membershipEquals(['bob', 'alice'], ['alice', 'bob'])).toBe(true);
	});
	
	it('should return false for different lengths', () => {
		expect(membershipEquals(['alice'], ['alice', 'bob'])).toBe(false);
		expect(membershipEquals(['alice', 'bob'], ['alice'])).toBe(false);
	});
	
	it('should return false for different members', () => {
		expect(membershipEquals(['alice', 'bob'], ['alice', 'charlie'])).toBe(false);
		expect(membershipEquals(['alice'], ['bob'])).toBe(false);
	});
	
	it('should handle empty arrays', () => {
		expect(membershipEquals([], [])).toBe(true);
		expect(membershipEquals([], ['alice'])).toBe(false);
	});
	
	it('should handle non-array inputs', () => {
		expect(membershipEquals('not an array', ['alice'])).toBe(false);
		expect(membershipEquals(['alice'], 'not an array')).toBe(false);
		expect(membershipEquals(null, ['alice'])).toBe(false);
		expect(membershipEquals(['alice'], undefined)).toBe(false);
	});
	
	it('should handle duplicate members (treat as set)', () => {
		// NOTE: This depends on implementation - Sets handle duplicates
		expect(membershipEquals(['alice', 'alice'], ['alice'])).toBe(false);
	});
});

// ═══════════════════════════════════════════════════════════════════
// SLOT ARRAY EQUALITY TESTS
// ═══════════════════════════════════════════════════════════════════

describe('slotArrayEquals', () => {
	const slot1: AvailabilitySlot = {
		id: 'slot_1',
		need_type_id: 'food',
		quantity: 100,
		time_constraint: { start: 1000, end: 2000 }
	};
	
	const slot2: AvailabilitySlot = {
		id: 'slot_2',
		need_type_id: 'housing',
		quantity: 50,
		location_constraint: { lat: 52.52, lon: 13.405 }
	};
	
	const slot3: AvailabilitySlot = {
		id: 'slot_3',
		need_type_id: 'transport',
		quantity: 75
	};
	
	it('should return true for identical slot arrays', () => {
		expect(slotArrayEquals([slot1], [slot1])).toBe(true);
		expect(slotArrayEquals([slot1, slot2], [slot1, slot2])).toBe(true);
	});
	
	it('should return true for equal slots with different references', () => {
		const slot1Copy = {
			id: 'slot_1',
			need_type_id: 'food',
			quantity: 100,
			time_constraint: { start: 1000, end: 2000 }
		};
		
		expect(slotArrayEquals([slot1], [slot1Copy])).toBe(true);
	});
	
	it('should return false for different quantities', () => {
		const slot1Modified = { ...slot1, quantity: 200 };
		expect(slotArrayEquals([slot1], [slot1Modified])).toBe(false);
	});
	
	it('should return false for different need types', () => {
		const slot1Modified = { ...slot1, need_type_id: 'housing' };
		expect(slotArrayEquals([slot1], [slot1Modified])).toBe(false);
	});
	
	it('should return false for different time constraints', () => {
		const slot1Modified = {
			...slot1,
			time_constraint: { start: 1000, end: 3000 } // Different end time
		};
		expect(slotArrayEquals([slot1], [slot1Modified])).toBe(false);
	});
	
	it('should return false for different location constraints', () => {
		const slot2Modified = {
			...slot2,
			location_constraint: { lat: 52.52, lon: 14.0 } // Different longitude
		};
		expect(slotArrayEquals([slot2], [slot2Modified])).toBe(false);
	});
	
	it('should return false for different array lengths', () => {
		expect(slotArrayEquals([slot1], [slot1, slot2])).toBe(false);
		expect(slotArrayEquals([slot1, slot2], [slot1])).toBe(false);
	});
	
	it('should return false for missing slots by ID', () => {
		expect(slotArrayEquals([slot1], [slot2])).toBe(false);
		expect(slotArrayEquals([slot1, slot2], [slot1, slot3])).toBe(false);
	});
	
	it('should handle empty arrays', () => {
		expect(slotArrayEquals([], [])).toBe(true);
		expect(slotArrayEquals([], [slot1])).toBe(false);
	});
	
	it('should handle non-array inputs', () => {
		expect(slotArrayEquals('not an array', [slot1])).toBe(false);
		expect(slotArrayEquals([slot1], null)).toBe(false);
	});
	
	it('should be order-independent (compares by ID map)', () => {
		expect(slotArrayEquals([slot1, slot2, slot3], [slot3, slot1, slot2])).toBe(true);
		expect(slotArrayEquals([slot2, slot1], [slot1, slot2])).toBe(true);
	});
	
	it('should handle slots with missing optional fields', () => {
		const slotMinimal1: AvailabilitySlot = {
			id: 'slot_min',
			need_type_id: 'food',
			quantity: 100
		};
		
		const slotMinimal2: AvailabilitySlot = {
			id: 'slot_min',
			need_type_id: 'food',
			quantity: 100
		};
		
		expect(slotArrayEquals([slotMinimal1], [slotMinimal2])).toBe(true);
	});
});

// ═══════════════════════════════════════════════════════════════════
// GET EQUALITY CHECKER TESTS
// ═══════════════════════════════════════════════════════════════════

describe('getEqualityChecker', () => {
	it('should return membershipEquals for membership attributes', () => {
		const checker = getEqualityChecker('membership');
		expect(checker).toBe(membershipEquals);
		
		// Test it works
		expect(checker?.(['alice', 'bob'], ['bob', 'alice'])).toBe(true);
	});
	
	it('should return slotArrayEquals for capacity attributes', () => {
		const checker = getEqualityChecker('capacity:food');
		expect(checker).toBe(slotArrayEquals);
		
		const slot: AvailabilitySlot = {
			id: 's1',
			need_type_id: 'food',
			quantity: 100
		};
		
		// Test it works
		expect(checker?.([slot], [slot])).toBe(true);
	});
	
	it('should return slotArrayEquals for need attributes', () => {
		const checker = getEqualityChecker('need:housing');
		expect(checker).toBe(slotArrayEquals);
	});
	
	it('should return undefined for other attribute types', () => {
		expect(getEqualityChecker('name')).toBeUndefined();
		expect(getEqualityChecker('location')).toBeUndefined();
		expect(getEqualityChecker('skill:javascript')).toBeUndefined();
		expect(getEqualityChecker('public_key')).toBeUndefined();
		expect(getEqualityChecker('unknown_type')).toBeUndefined();
	});
	
	it('should handle edge cases', () => {
		expect(getEqualityChecker('')).toBeUndefined();
		
		// NOTE: 'capacity:' and 'need:' without type still match the prefix pattern
		// This is technically correct - they're still capacity/need attributes
		expect(getEqualityChecker('capacity:')).toBe(slotArrayEquals);
		expect(getEqualityChecker('need:')).toBe(slotArrayEquals);
	});
});

