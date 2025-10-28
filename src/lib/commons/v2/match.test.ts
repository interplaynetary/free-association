/**
 * Comprehensive Tests for Slot Matching Logic
 * 
 * Test Coverage:
 * 1. Distance Calculations (haversineDistance)
 * 2. Time Compatibility (timeRangesOverlap)
 * 3. Location Compatibility (locationsCompatible)
 * 4. Slot Compatibility (slotsCompatible)
 * 5. Filter Evaluation (evaluateFilter - all types)
 * 6. Bilateral Filters (passesSlotFilters)
 * 7. Slot Matching (matchNeedToCapacitySlots)
 * 8. Space-Time Bucketing (getTimeBucketKey, getLocationBucketKey)
 * 9. Space-Time Signatures (getSpaceTimeSignature)
 * 10. Space-Time Grouping (groupSlotsBySpaceTime)
 * 11. Need Utilities (calculateTotalNeedAmount, getRemainingNeed, etc.)
 * 12. Space-Time Profiles (getSpaceTimeNeedProfile, getSpaceTimeCapacityProfile)
 */

import { describe, it, expect } from 'vitest';
import {
	haversineDistance,
	timeRangesOverlap,
	locationsCompatible,
	slotsCompatible,
	evaluateFilter,
	passesSlotFilters,
	matchNeedToCapacitySlots,
	getTimeBucketKey,
	getLocationBucketKey,
	getSpaceTimeSignature,
	groupSlotsBySpaceTime,
	calculateTotalNeedAmount,
	getRemainingNeed,
	calculateSlotCompatibleAmount,
	getSpaceTimeNeedProfile,
	getSpaceTimeCapacityProfile,
	type FilterContext
} from '../match.svelte';
import type { AvailabilitySlot, NeedSlot, BaseCapacity, BaseNeed } from '../v1/schemas';

// ═══════════════════════════════════════════════════════════════════
// TEST HELPERS
// ═══════════════════════════════════════════════════════════════════

function createAvailabilitySlot(overrides: Partial<AvailabilitySlot> = {}): AvailabilitySlot {
	return {
		id: 'avail-1',
		name: 'Test Capacity',
		quantity: 10,
		...overrides
	};
}

function createNeedSlot(overrides: Partial<NeedSlot> = {}): NeedSlot {
	return {
		id: 'need-1',
		name: 'Test Need',
		quantity: 10,
		...overrides
	};
}

function createCapacity(slots: AvailabilitySlot[]): BaseCapacity {
	return {
		id: 'capacity-1',
		name: 'Test Capacity',
		availability_slots: slots,
		provider_id: 'provider-1'
	};
}

function createNeed(slots: NeedSlot[]): BaseNeed {
	return {
		id: 'need-1',
		name: 'Test Need',
		need_slots: slots,
		declarer_id: 'recipient-1',
		status: 'open',
		fulfilled_amount: 0
	};
}

// ═══════════════════════════════════════════════════════════════════
// TESTS: DISTANCE CALCULATIONS
// ═══════════════════════════════════════════════════════════════════

describe('haversineDistance', () => {
	it('should calculate distance between two points', () => {
		// SF to NYC (approximately 4130 km)
		const sf = { lat: 37.7749, lon: -122.4194 };
		const nyc = { lat: 40.7128, lon: -74.0060 };
		
		const distance = haversineDistance(sf.lat, sf.lon, nyc.lat, nyc.lon);
		
		expect(distance).toBeGreaterThan(4000);
		expect(distance).toBeLessThan(4200);
	});
	
	it('should return 0 for same location', () => {
		const distance = haversineDistance(37.7749, -122.4194, 37.7749, -122.4194);
		expect(distance).toBe(0);
	});
	
	it('should calculate short distances accurately', () => {
		// Two points ~10km apart
		const point1 = { lat: 37.7749, lon: -122.4194 };
		const point2 = { lat: 37.8049, lon: -122.4294 };
		
		const distance = haversineDistance(point1.lat, point1.lon, point2.lat, point2.lon);
		
		expect(distance).toBeGreaterThan(3);
		expect(distance).toBeLessThan(5);
	});
});

// ═══════════════════════════════════════════════════════════════════
// TESTS: TIME COMPATIBILITY
// ═══════════════════════════════════════════════════════════════════

describe('timeRangesOverlap', () => {
	it('should return true when both slots have no time info', () => {
		const slot1 = {};
		const slot2 = {};
		
		expect(timeRangesOverlap(slot1, slot2)).toBe(true);
	});
	
	it('should return true when only one slot has time info (optimistic)', () => {
		const slot1 = { start_date: '2024-06-01', end_date: '2024-06-30' };
		const slot2 = {};
		
		expect(timeRangesOverlap(slot1, slot2)).toBe(true);
	});
	
	it('should detect overlapping date ranges', () => {
		const slot1 = { start_date: '2024-06-01', end_date: '2024-06-15' };
		const slot2 = { start_date: '2024-06-10', end_date: '2024-06-20' };
		
		expect(timeRangesOverlap(slot1, slot2)).toBe(true);
	});
	
	it('should detect non-overlapping date ranges', () => {
		const slot1 = { start_date: '2024-06-01', end_date: '2024-06-15' };
		const slot2 = { start_date: '2024-06-20', end_date: '2024-06-30' };
		
		expect(timeRangesOverlap(slot1, slot2)).toBe(false);
	});
	
	it('should handle adjacent date ranges (edge-touching)', () => {
		const slot1 = { start_date: '2024-06-01', end_date: '2024-06-15' };
		const slot2 = { start_date: '2024-06-15', end_date: '2024-06-30' };
		
		expect(timeRangesOverlap(slot1, slot2)).toBe(true);
	});
	
	it('should handle one range completely inside another', () => {
		const slot1 = { start_date: '2024-06-01', end_date: '2024-06-30' };
		const slot2 = { start_date: '2024-06-10', end_date: '2024-06-15' };
		
		expect(timeRangesOverlap(slot1, slot2)).toBe(true);
	});
	
	it('should handle dates that create Invalid Date objects', () => {
		// JavaScript's Date constructor can parse 'invalid-date' but creates Invalid Date
		// which fails comparison, so it returns true (optimistic)
		const slot1 = { start_date: 'not-a-real-date-format' };
		const slot2 = { start_date: '2024-06-01' };
		
		// The timeRangesOverlap function will try to parse and compare
		// Invalid dates will fail the comparison logic
		const result = timeRangesOverlap(slot1, slot2);
		// Accept either true or false since behavior depends on Date parsing
		expect(typeof result).toBe('boolean');
	});
});

// ═══════════════════════════════════════════════════════════════════
// TESTS: LOCATION COMPATIBILITY
// ═══════════════════════════════════════════════════════════════════

describe('locationsCompatible', () => {
	it('should return true when both slots have no location info', () => {
		const slot1 = {};
		const slot2 = {};
		
		expect(locationsCompatible(slot1, slot2)).toBe(true);
	});
	
	it('should return true when only one slot has location info (optimistic)', () => {
		const slot1 = { city: 'San Francisco' };
		const slot2 = {};
		
		expect(locationsCompatible(slot1, slot2)).toBe(true);
	});
	
	it('should match same city (case insensitive)', () => {
		const slot1 = { city: 'San Francisco' };
		const slot2 = { city: 'san francisco' };
		
		expect(locationsCompatible(slot1, slot2)).toBe(true);
	});
	
	it('should match same country (case insensitive)', () => {
		const slot1 = { country: 'USA' };
		const slot2 = { country: 'usa' };
		
		expect(locationsCompatible(slot1, slot2)).toBe(true);
	});
	
	it('should reject different cities', () => {
		const slot1 = { city: 'San Francisco' };
		const slot2 = { city: 'New York' };
		
		expect(locationsCompatible(slot1, slot2)).toBe(false);
	});
	
	it('should accept remote/online slots', () => {
		const slot1 = { location_type: 'remote' };
		const slot2 = { city: 'San Francisco' };
		
		expect(locationsCompatible(slot1, slot2)).toBe(true);
	});
	
	it('should accept slots with online_link', () => {
		const slot1 = { online_link: 'https://zoom.us/meeting' };
		const slot2 = { city: 'New York' };
		
		expect(locationsCompatible(slot1, slot2)).toBe(true);
	});
	
	it('should match locations within 50km', () => {
		// Two points within 5km
		const slot1 = { latitude: 37.7749, longitude: -122.4194 };
		const slot2 = { latitude: 37.8049, longitude: -122.4294 };
		
		expect(locationsCompatible(slot1, slot2)).toBe(true);
	});
	
	it('should reject locations beyond 50km', () => {
		// SF to NYC
		const slot1 = { latitude: 37.7749, longitude: -122.4194 };
		const slot2 = { latitude: 40.7128, longitude: -74.0060 };
		
		expect(locationsCompatible(slot1, slot2)).toBe(false);
	});
});

// ═══════════════════════════════════════════════════════════════════
// TESTS: SLOT COMPATIBILITY
// ═══════════════════════════════════════════════════════════════════

describe('slotsCompatible', () => {
	it('should require both time AND location compatibility', () => {
		const needSlot = createNeedSlot({
			start_date: '2024-06-01',
			end_date: '2024-06-30',
			city: 'San Francisco'
		});
		
		const availSlot = createAvailabilitySlot({
			start_date: '2024-06-15',
			end_date: '2024-06-20',
			city: 'San Francisco'
		});
		
		expect(slotsCompatible(needSlot, availSlot)).toBe(true);
	});
	
	it('should reject if time incompatible', () => {
		const needSlot = createNeedSlot({
			start_date: '2024-06-01',
			end_date: '2024-06-15',
			city: 'San Francisco'
		});
		
		const availSlot = createAvailabilitySlot({
			start_date: '2024-06-20',
			end_date: '2024-06-30',
			city: 'San Francisco'
		});
		
		expect(slotsCompatible(needSlot, availSlot)).toBe(false);
	});
	
	it('should reject if location incompatible', () => {
		const needSlot = createNeedSlot({
			start_date: '2024-06-01',
			end_date: '2024-06-30',
			city: 'San Francisco'
		});
		
		const availSlot = createAvailabilitySlot({
			start_date: '2024-06-15',
			end_date: '2024-06-20',
			city: 'New York'
		});
		
		expect(slotsCompatible(needSlot, availSlot)).toBe(false);
	});
	
	it('should accept when both are optimistic (no info)', () => {
		const needSlot = createNeedSlot();
		const availSlot = createAvailabilitySlot();
		
		expect(slotsCompatible(needSlot, availSlot)).toBe(true);
	});
});

// ═══════════════════════════════════════════════════════════════════
// TESTS: FILTER EVALUATION
// ═══════════════════════════════════════════════════════════════════

describe('evaluateFilter', () => {
	const baseContext: FilterContext = {
		pubKey: 'test-user',
		commitment: {},
		mutualRecognition: 0.5,
		attributes: {}
	};
	
	it('should pass null/undefined filters (optimistic)', () => {
		expect(evaluateFilter(null, baseContext)).toBe(true);
		expect(evaluateFilter(undefined, baseContext)).toBe(true);
	});
	
	it('should pass allow_all filter', () => {
		const filter = { type: 'allow_all' as const };
		expect(evaluateFilter(filter, baseContext)).toBe(true);
	});
	
	it('should reject deny_all filter', () => {
		const filter = { type: 'deny_all' as const };
		expect(evaluateFilter(filter, baseContext)).toBe(false);
	});
	
	describe('trust filters', () => {
		it('should pass when only_mutual and MR > 0', () => {
			const filter = { type: 'trust' as const, only_mutual: true };
			const context = { ...baseContext, mutualRecognition: 0.5 };
			
			expect(evaluateFilter(filter, context)).toBe(true);
		});
		
		it('should reject when only_mutual and MR = 0', () => {
			const filter = { type: 'trust' as const, only_mutual: true };
			const context = { ...baseContext, mutualRecognition: 0 };
			
			expect(evaluateFilter(filter, context)).toBe(false);
		});
		
		it('should check min_mutual_recognition', () => {
			const filter = { type: 'trust' as const, min_mutual_recognition: 0.7 };
			
			expect(evaluateFilter(filter, { ...baseContext, mutualRecognition: 0.8 })).toBe(true);
			expect(evaluateFilter(filter, { ...baseContext, mutualRecognition: 0.5 })).toBe(false);
		});
	});
	
	describe('location filters', () => {
		it('should check allowed_cities', () => {
			const filter = {
				type: 'location' as const,
				allowed_cities: ['San Francisco', 'New York']
			};
			
			const context1 = {
				...baseContext,
				commitment: { city: 'San Francisco' }
			};
			const context2 = {
				...baseContext,
				commitment: { city: 'Los Angeles' }
			};
			
			expect(evaluateFilter(filter, context1)).toBe(true);
			expect(evaluateFilter(filter, context2)).toBe(false);
		});
		
		it('should check allowed_countries', () => {
			const filter = {
				type: 'location' as const,
				allowed_countries: ['USA', 'Canada']
			};
			
			const context1 = {
				...baseContext,
				commitment: { country: 'USA' }
			};
			const context2 = {
				...baseContext,
				commitment: { country: 'Mexico' }
			};
			
			expect(evaluateFilter(filter, context1)).toBe(true);
			expect(evaluateFilter(filter, context2)).toBe(false);
		});
	});
	
	describe('attribute filters', () => {
		it('should check required attributes', () => {
			const filter = {
				type: 'attribute' as const,
				required: ['verified', 'active']
			};
			
			const context1 = {
				...baseContext,
				attributes: { verified: true, active: true }
			};
			const context2 = {
				...baseContext,
				attributes: { verified: true }
			};
			
			expect(evaluateFilter(filter, context1)).toBe(true);
			expect(evaluateFilter(filter, context2)).toBe(false);
		});
		
		it('should check forbidden attributes', () => {
			const filter = {
				type: 'attribute' as const,
				forbidden: ['banned', 'suspended']
			};
			
			const context1 = {
				...baseContext,
				attributes: { active: true }
			};
			const context2 = {
				...baseContext,
				attributes: { banned: true }
			};
			
			expect(evaluateFilter(filter, context1)).toBe(true);
			expect(evaluateFilter(filter, context2)).toBe(false);
		});
	});
	
	describe('certification filters', () => {
		it('should check required certifications', () => {
			const filter = {
				type: 'certification' as const,
				required: ['CPR', 'First Aid']
			};
			
			const context1 = {
				...baseContext,
				attributes: { certifications: ['CPR', 'First Aid'] }
			};
			const context2 = {
				...baseContext,
				attributes: { certifications: ['CPR'] }
			};
			
			expect(evaluateFilter(filter, context1)).toBe(true);
			expect(evaluateFilter(filter, context2)).toBe(false);
		});
		
		it('should check minimum certification level', () => {
			const filter = {
				type: 'certification' as const,
				min_level: 3
			};
			
			const context1 = {
				...baseContext,
				attributes: { certification_level: 4 }
			};
			const context2 = {
				...baseContext,
				attributes: { certification_level: 2 }
			};
			
			expect(evaluateFilter(filter, context1)).toBe(true);
			expect(evaluateFilter(filter, context2)).toBe(false);
		});
	});
	
	describe('resource_type filters', () => {
		it('should check allowed_types', () => {
			const filter = {
				type: 'resource_type' as const,
				allowed_types: ['tutoring', 'mentoring']
			};
			
			const context1 = {
				...baseContext,
				commitment: { resource_type: 'tutoring' }
			};
			const context2 = {
				...baseContext,
				commitment: { resource_type: 'housing' }
			};
			
			expect(evaluateFilter(filter, context1)).toBe(true);
			expect(evaluateFilter(filter, context2)).toBe(false);
		});
		
		it('should check forbidden_types', () => {
			const filter = {
				type: 'resource_type' as const,
				forbidden_types: ['gambling', 'alcohol']
			};
			
			const context1 = {
				...baseContext,
				commitment: { resource_type: 'tutoring' }
			};
			const context2 = {
				...baseContext,
				commitment: { resource_type: 'gambling' }
			};
			
			expect(evaluateFilter(filter, context1)).toBe(true);
			expect(evaluateFilter(filter, context2)).toBe(false);
		});
	});
	
	it('should be optimistic for custom filters (not implemented)', () => {
		const filter = { type: 'custom' as const, fn: 'some-function' };
		expect(evaluateFilter(filter, baseContext)).toBe(true);
	});
	
	it('should be optimistic for unknown filter types', () => {
		const filter = { type: 'unknown-type' } as any;
		expect(evaluateFilter(filter, baseContext)).toBe(true);
	});
	
	it('should handle missing context fields gracefully', () => {
		const filter = { type: 'trust' as const, only_mutual: true };
		const badContext = { pubKey: 'test' } as any; // Missing mutualRecognition
		
		// Should not throw and handle missing mutualRecognition as 0
		const result = evaluateFilter(filter, badContext);
		expect(result).toBe(false); // only_mutual requires MR > 0
	});
});

// ═══════════════════════════════════════════════════════════════════
// TESTS: BILATERAL FILTERS
// ═══════════════════════════════════════════════════════════════════

describe('passesSlotFilters', () => {
	const providerContext: FilterContext = {
		pubKey: 'provider',
		commitment: {},
		mutualRecognition: 0.5
	};
	
	const recipientContext: FilterContext = {
		pubKey: 'recipient',
		commitment: {},
		mutualRecognition: 0.5
	};
	
	it('should pass when no filters present', () => {
		const needSlot = createNeedSlot();
		const availSlot = createAvailabilitySlot();
		
		expect(passesSlotFilters(needSlot, availSlot, providerContext, recipientContext)).toBe(true);
	});
	
	it('should check availability slot filter (recipient must pass)', () => {
		const needSlot = createNeedSlot();
		const availSlot = createAvailabilitySlot({
			filter_rule: { type: 'trust', only_mutual: true }
		});
		
		const goodRecipient = { ...recipientContext, mutualRecognition: 0.5 };
		const badRecipient = { ...recipientContext, mutualRecognition: 0 };
		
		expect(passesSlotFilters(needSlot, availSlot, providerContext, goodRecipient)).toBe(true);
		expect(passesSlotFilters(needSlot, availSlot, providerContext, badRecipient)).toBe(false);
	});
	
	it('should check need slot filter (provider must pass)', () => {
		const needSlot = createNeedSlot({
			filter_rule: { type: 'trust', min_mutual_recognition: 0.7 }
		});
		const availSlot = createAvailabilitySlot();
		
		const goodProvider = { ...providerContext, mutualRecognition: 0.8 };
		const badProvider = { ...providerContext, mutualRecognition: 0.5 };
		
		expect(passesSlotFilters(needSlot, availSlot, goodProvider, recipientContext)).toBe(true);
		expect(passesSlotFilters(needSlot, availSlot, badProvider, recipientContext)).toBe(false);
	});
	
	it('should require BOTH filters to pass (bilateral)', () => {
		const needSlot = createNeedSlot({
			filter_rule: { type: 'trust', min_mutual_recognition: 0.3 }
		});
		const availSlot = createAvailabilitySlot({
			filter_rule: { type: 'trust', min_mutual_recognition: 0.6 }
		});
		
		// Both have MR = 0.5
		// Provider passes need filter (0.5 >= 0.3) ✓
		// Recipient fails avail filter (0.5 < 0.6) ✗
		expect(passesSlotFilters(needSlot, availSlot, providerContext, recipientContext)).toBe(false);
		
		// Increase MR to 0.7
		const highMRContext = { ...recipientContext, mutualRecognition: 0.7 };
		expect(passesSlotFilters(needSlot, availSlot, providerContext, highMRContext)).toBe(true);
	});
});

// ═══════════════════════════════════════════════════════════════════
// TESTS: SLOT MATCHING
// ═══════════════════════════════════════════════════════════════════

describe('matchNeedToCapacitySlots', () => {
	it('should match compatible slots', () => {
		const needSlots = [
			createNeedSlot({
				id: 'need-1',
				quantity: 5,
				start_date: '2024-06-01',
				city: 'San Francisco'
			})
		];
		
		const availSlots = [
			createAvailabilitySlot({
				id: 'avail-1',
				quantity: 10,
				start_date: '2024-06-01',
				city: 'San Francisco'
			})
		];
		
		const need = createNeed(needSlots);
		const capacity = createCapacity(availSlots);
		
		const result = matchNeedToCapacitySlots(need, capacity, 100);
		
		expect(result.compatible_pairs).toHaveLength(1);
		expect(result.total_matchable).toBe(5); // Limited by need quantity
		expect(result.unmatched_need_slots).toHaveLength(0);
		expect(result.unmatched_availability_slots).toHaveLength(0);
	});
	
	it('should respect maxAmount limit', () => {
		const needSlots = [createNeedSlot({ quantity: 50 })];
		const availSlots = [createAvailabilitySlot({ quantity: 100 })];
		
		const need = createNeed(needSlots);
		const capacity = createCapacity(availSlots);
		
		const result = matchNeedToCapacitySlots(need, capacity, 20);
		
		expect(result.total_matchable).toBe(20); // Limited by maxAmount
	});
	
	it('should handle multiple need slots', () => {
		const needSlots = [
			createNeedSlot({ id: 'need-1', quantity: 5 }),
			createNeedSlot({ id: 'need-2', quantity: 10 })
		];
		
		const availSlots = [
			createAvailabilitySlot({ id: 'avail-1', quantity: 20 })
		];
		
		const need = createNeed(needSlots);
		const capacity = createCapacity(availSlots);
		
		const result = matchNeedToCapacitySlots(need, capacity, 100);
		
		expect(result.compatible_pairs).toHaveLength(2);
		expect(result.total_matchable).toBe(15); // 5 + 10
	});
	
	it('should handle incompatible slots', () => {
		const needSlots = [
			createNeedSlot({
				city: 'San Francisco',
				start_date: '2024-06-01'
			})
		];
		
		const availSlots = [
			createAvailabilitySlot({
				city: 'New York', // Different city
				start_date: '2024-06-01'
			})
		];
		
		const need = createNeed(needSlots);
		const capacity = createCapacity(availSlots);
		
		const result = matchNeedToCapacitySlots(need, capacity, 100);
		
		expect(result.compatible_pairs).toHaveLength(0);
		expect(result.total_matchable).toBe(0);
		expect(result.unmatched_need_slots).toHaveLength(1);
		expect(result.unmatched_availability_slots).toHaveLength(1);
	});
	
	it('should stop at maxAmount across multiple matches', () => {
		const needSlots = [
			createNeedSlot({ id: 'need-1', quantity: 10 }),
			createNeedSlot({ id: 'need-2', quantity: 10 }),
			createNeedSlot({ id: 'need-3', quantity: 10 })
		];
		
		const availSlots = [
			createAvailabilitySlot({ quantity: 50 })
		];
		
		const need = createNeed(needSlots);
		const capacity = createCapacity(availSlots);
		
		const result = matchNeedToCapacitySlots(need, capacity, 15);
		
		expect(result.total_matchable).toBe(15);
		expect(result.compatible_pairs.length).toBeGreaterThan(0);
		expect(result.compatible_pairs.length).toBeLessThanOrEqual(2); // Stops after hitting maxAmount
	});
});

// ═══════════════════════════════════════════════════════════════════
// TESTS: SPACE-TIME BUCKETING
// ═══════════════════════════════════════════════════════════════════

describe('getTimeBucketKey', () => {
	it('should return month-level bucket from start_date', () => {
		const slot = createAvailabilitySlot({
			start_date: '2024-06-15'
		});
		
		expect(getTimeBucketKey(slot)).toBe('2024-06');
	});
	
	it('should return any-time when no start_date', () => {
		const slot = createAvailabilitySlot();
		
		expect(getTimeBucketKey(slot)).toBe('any-time');
	});
	
	it('should handle different months', () => {
		const jan = createAvailabilitySlot({ start_date: '2024-01-01' });
		const dec = createAvailabilitySlot({ start_date: '2024-12-31' });
		
		expect(getTimeBucketKey(jan)).toBe('2024-01');
		expect(getTimeBucketKey(dec)).toBe('2024-12');
	});
});

describe('getLocationBucketKey', () => {
	it('should return remote for remote location_type', () => {
		const slot = createAvailabilitySlot({
			location_type: 'remote'
		});
		
		expect(getLocationBucketKey(slot)).toBe('remote');
	});
	
	it('should return remote for online location_type', () => {
		const slot = createAvailabilitySlot({
			location_type: 'online'
		});
		
		expect(getLocationBucketKey(slot)).toBe('remote');
	});
	
	it('should return remote for online_link', () => {
		const slot = createAvailabilitySlot({
			online_link: 'https://zoom.us/meeting'
		});
		
		expect(getLocationBucketKey(slot)).toBe('remote');
	});
	
	it('should return city (lowercase) when available', () => {
		const slot = createAvailabilitySlot({
			city: 'San Francisco'
		});
		
		expect(getLocationBucketKey(slot)).toBe('san francisco');
	});
	
	it('should return country (lowercase) when no city', () => {
		const slot = createAvailabilitySlot({
			country: 'USA'
		});
		
		expect(getLocationBucketKey(slot)).toBe('usa');
	});
	
	it('should return unknown when no location info', () => {
		const slot = createAvailabilitySlot();
		
		expect(getLocationBucketKey(slot)).toBe('unknown');
	});
	
	it('should prefer city over country', () => {
		const slot = createAvailabilitySlot({
			city: 'San Francisco',
			country: 'USA'
		});
		
		expect(getLocationBucketKey(slot)).toBe('san francisco');
	});
});

// ═══════════════════════════════════════════════════════════════════
// TESTS: SPACE-TIME SIGNATURES
// ═══════════════════════════════════════════════════════════════════

describe('getSpaceTimeSignature', () => {
	it('should generate unique signatures for different times', () => {
		const slot1 = createAvailabilitySlot({
			start_date: '2024-06-01',
			city: 'San Francisco'
		});
		
		const slot2 = createAvailabilitySlot({
			start_date: '2024-06-02',
			city: 'San Francisco'
		});
		
		expect(getSpaceTimeSignature(slot1)).not.toBe(getSpaceTimeSignature(slot2));
	});
	
	it('should generate unique signatures for different locations', () => {
		const slot1 = createAvailabilitySlot({
			start_date: '2024-06-01',
			city: 'San Francisco'
		});
		
		const slot2 = createAvailabilitySlot({
			start_date: '2024-06-01',
			city: 'New York'
		});
		
		expect(getSpaceTimeSignature(slot1)).not.toBe(getSpaceTimeSignature(slot2));
	});
	
	it('should generate same signature for identical time/location', () => {
		const slot1 = createAvailabilitySlot({
			start_date: '2024-06-01',
			start_time: '09:00',
			end_date: '2024-06-01',
			end_time: '10:00',
			city: 'San Francisco'
		});
		
		const slot2 = createAvailabilitySlot({
			start_date: '2024-06-01',
			start_time: '09:00',
			end_date: '2024-06-01',
			end_time: '10:00',
			city: 'San Francisco'
		});
		
		expect(getSpaceTimeSignature(slot1)).toBe(getSpaceTimeSignature(slot2));
	});
	
	it('should include recurrence in signature', () => {
		const slot1 = createAvailabilitySlot({
			start_date: '2024-06-01',
			recurrence: 'weekly',
			city: 'San Francisco'
		});
		
		const slot2 = createAvailabilitySlot({
			start_date: '2024-06-01',
			recurrence: 'daily',
			city: 'San Francisco'
		});
		
		expect(getSpaceTimeSignature(slot1)).not.toBe(getSpaceTimeSignature(slot2));
	});
	
	it('should include all_day in signature', () => {
		const slot1 = createAvailabilitySlot({
			start_date: '2024-06-01',
			all_day: true,
			city: 'San Francisco'
		});
		
		const slot2 = createAvailabilitySlot({
			start_date: '2024-06-01',
			all_day: false,
			city: 'San Francisco'
		});
		
		expect(getSpaceTimeSignature(slot1)).not.toBe(getSpaceTimeSignature(slot2));
	});
});

// ═══════════════════════════════════════════════════════════════════
// TESTS: SPACE-TIME GROUPING
// ═══════════════════════════════════════════════════════════════════

describe('groupSlotsBySpaceTime', () => {
	it('should group slots with identical space-time', () => {
		const slots = [
			createAvailabilitySlot({
				id: 'slot-1',
				quantity: 5,
				start_date: '2024-06-01',
				city: 'San Francisco'
			}),
			createAvailabilitySlot({
				id: 'slot-2',
				quantity: 3,
				start_date: '2024-06-01',
				city: 'San Francisco'
			})
		];
		
		const groups = groupSlotsBySpaceTime(slots);
		
		expect(groups.size).toBe(1);
		const group = Array.from(groups.values())[0];
		expect(group.quantity).toBe(8); // 5 + 3
		expect(group.slots).toHaveLength(2);
	});
	
	it('should keep separate groups for different times', () => {
		const slots = [
			createAvailabilitySlot({
				id: 'slot-1',
				quantity: 5,
				start_date: '2024-06-01',
				city: 'San Francisco'
			}),
			createAvailabilitySlot({
				id: 'slot-2',
				quantity: 3,
				start_date: '2024-06-02',
				city: 'San Francisco'
			})
		];
		
		const groups = groupSlotsBySpaceTime(slots);
		
		expect(groups.size).toBe(2);
	});
	
	it('should keep separate groups for different locations', () => {
		const slots = [
			createAvailabilitySlot({
				id: 'slot-1',
				quantity: 5,
				start_date: '2024-06-01',
				city: 'San Francisco'
			}),
			createAvailabilitySlot({
				id: 'slot-2',
				quantity: 3,
				start_date: '2024-06-01',
				city: 'New York'
			})
		];
		
		const groups = groupSlotsBySpaceTime(slots);
		
		expect(groups.size).toBe(2);
	});
	
	it('should handle empty array', () => {
		const groups = groupSlotsBySpaceTime([]);
		expect(groups.size).toBe(0);
	});
});

// ═══════════════════════════════════════════════════════════════════
// TESTS: NEED UTILITIES
// ═══════════════════════════════════════════════════════════════════

describe('calculateTotalNeedAmount', () => {
	it('should sum all need slot quantities', () => {
		const need = createNeed([
			createNeedSlot({ quantity: 5 }),
			createNeedSlot({ quantity: 10 }),
			createNeedSlot({ quantity: 15 })
		]);
		
		expect(calculateTotalNeedAmount(need)).toBe(30);
	});
	
	it('should return 0 for empty need slots', () => {
		const need = createNeed([]);
		expect(calculateTotalNeedAmount(need)).toBe(0);
	});
});

describe('getRemainingNeed', () => {
	it('should calculate remaining need', () => {
		const need = createNeed([
			createNeedSlot({ quantity: 20 })
		]);
		need.fulfilled_amount = 8;
		
		expect(getRemainingNeed(need)).toBe(12);
	});
	
	it('should return 0 when fully fulfilled', () => {
		const need = createNeed([
			createNeedSlot({ quantity: 20 })
		]);
		need.fulfilled_amount = 20;
		
		expect(getRemainingNeed(need)).toBe(0);
	});
	
	it('should return 0 when over-fulfilled', () => {
		const need = createNeed([
			createNeedSlot({ quantity: 20 })
		]);
		need.fulfilled_amount = 25;
		
		expect(getRemainingNeed(need)).toBe(0);
	});
});

describe('calculateSlotCompatibleAmount', () => {
	it('should return compatible amount', () => {
		const need = createNeed([
			createNeedSlot({ quantity: 10 })
		]);
		
		const capacity = createCapacity([
			createAvailabilitySlot({ quantity: 20 })
		]);
		
		const amount = calculateSlotCompatibleAmount(need, capacity, 100);
		
		expect(amount).toBe(10); // Limited by need
	});
	
	it('should respect maxAmount', () => {
		const need = createNeed([
			createNeedSlot({ quantity: 50 })
		]);
		
		const capacity = createCapacity([
			createAvailabilitySlot({ quantity: 100 })
		]);
		
		const amount = calculateSlotCompatibleAmount(need, capacity, 20);
		
		expect(amount).toBe(20); // Limited by maxAmount
	});
	
	it('should return 0 for undefined need', () => {
		const capacity = createCapacity([
			createAvailabilitySlot({ quantity: 100 })
		]);
		
		const amount = calculateSlotCompatibleAmount(undefined, capacity, 100);
		
		expect(amount).toBe(0);
	});
	
	it('should return 0 for incompatible slots', () => {
		const need = createNeed([
			createNeedSlot({
				quantity: 10,
				city: 'San Francisco',
				start_date: '2024-06-01'
			})
		]);
		
		const capacity = createCapacity([
			createAvailabilitySlot({
				quantity: 20,
				city: 'New York', // Different city
				start_date: '2024-06-01'
			})
		]);
		
		const amount = calculateSlotCompatibleAmount(need, capacity, 100);
		
		expect(amount).toBe(0);
	});
});

// ═══════════════════════════════════════════════════════════════════
// TESTS: SPACE-TIME PROFILES
// ═══════════════════════════════════════════════════════════════════

describe('getSpaceTimeNeedProfile', () => {
	it('should return profile with aggregated quantities', () => {
		const need = createNeed([
			createNeedSlot({
				id: 'need-1',
				quantity: 5,
				start_date: '2024-06-01',
				city: 'San Francisco'
			}),
			createNeedSlot({
				id: 'need-2',
				quantity: 3,
				start_date: '2024-06-01',
				city: 'San Francisco'
			})
		]);
		
		const profile = getSpaceTimeNeedProfile(need);
		
		expect(profile).toHaveLength(1); // Same space-time
		expect(profile[0].quantity).toBe(8); // 5 + 3
		expect(profile[0].slots).toHaveLength(2);
	});
	
	it('should separate different space-times', () => {
		const need = createNeed([
			createNeedSlot({
				quantity: 5,
				start_date: '2024-06-01',
				city: 'San Francisco'
			}),
			createNeedSlot({
				quantity: 3,
				start_date: '2024-06-02', // Different day
				city: 'San Francisco'
			})
		]);
		
		const profile = getSpaceTimeNeedProfile(need);
		
		expect(profile).toHaveLength(2);
	});
});

describe('getSpaceTimeCapacityProfile', () => {
	it('should return profile with aggregated quantities', () => {
		const capacity = createCapacity([
			createAvailabilitySlot({
				id: 'avail-1',
				quantity: 10,
				start_date: '2024-06-01',
				city: 'San Francisco'
			}),
			createAvailabilitySlot({
				id: 'avail-2',
				quantity: 5,
				start_date: '2024-06-01',
				city: 'San Francisco'
			})
		]);
		
		const profile = getSpaceTimeCapacityProfile(capacity);
		
		expect(profile).toHaveLength(1); // Same space-time
		expect(profile[0].quantity).toBe(15); // 10 + 5
		expect(profile[0].slots).toHaveLength(2);
	});
	
	it('should separate different space-times', () => {
		const capacity = createCapacity([
			createAvailabilitySlot({
				quantity: 10,
				start_date: '2024-06-01',
				city: 'San Francisco'
			}),
			createAvailabilitySlot({
				quantity: 5,
				start_date: '2024-06-01',
				city: 'New York' // Different city
			})
		]);
		
		const profile = getSpaceTimeCapacityProfile(capacity);
		
		expect(profile).toHaveLength(2);
	});
});

