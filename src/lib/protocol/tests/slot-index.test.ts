/**
 * H3 Spatial Indexing - Index Data Structures Tests (Phase 2)
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
	SlotIndex,
	buildSlotIndex,
	findMatchesIndexed,
	TemporalIndex,
	SpatialIndex,
	TypeIndex
} from '../slot-index';
import type { AvailabilitySlot, NeedSlot } from '../resources';

describe('Temporal Index', () => {
	let temporalIndex: TemporalIndex;

	beforeEach(() => {
		temporalIndex = new TemporalIndex();
	});

	it('should index one-time slots by month', () => {
		const slot: any = {
			id: 'slot-1',
			type_id: 'tutoring',
			quantity: 10,
			start_date: '2024-06-15',
			name: 'Tutoring'
		};

		temporalIndex.insert(slot);
		expect(temporalIndex.size).toBe(1);

		const results = temporalIndex.query({
			start: new Date('2024-06-01'),
			end: new Date('2024-06-30')
		});
		expect(results).toContainEqual(slot);
	});

	it('should index recurring slots separately', () => {
		const recurringSlot: any = {
			id: 'slot-2',
			type_id: 'tutoring',
			quantity: 10,
			recurrence: 'weekly',
			name: 'Weekly Tutoring'
		};

		temporalIndex.insert(recurringSlot);
		expect(temporalIndex.size).toBe(1);

		const results = temporalIndex.query({
			start: new Date('2024-01-01'),
			end: new Date('2024-12-31')
		});
		expect(results).toContainEqual(recurringSlot);
	});

	it('should remove slots correctly', () => {
		const slot: any = {
			id: 'slot-3',
			type_id: 'tutoring',
			quantity: 10,
			start_date: '2024-06-15',
			name: 'Tutoring'
		};

		temporalIndex.insert(slot);
		expect(temporalIndex.size).toBe(1);

		temporalIndex.remove(slot);
		expect(temporalIndex.size).toBe(0);
	});
});

describe('Spatial Index', () => {
	let spatialIndex: SpatialIndex;

	beforeEach(() => {
		spatialIndex = new SpatialIndex();
	});

	it('should index slots by H3 cell', () => {
		const slot: any = {
			id: 'slot-1',
			type_id: 'tutoring',
			quantity: 10,
			latitude: 37.7749,
			longitude: -122.4194,
			start_date: '2024-06-15',
			name: 'SF Tutoring'
		};

		spatialIndex.insert(slot);
		expect(spatialIndex.size).toBe(1);
		expect(spatialIndex.cellCount).toBe(1);
	});

	it('should query slots within radius', () => {
		const sfSlot: any = {
			id: 'slot-1',
			type_id: 'tutoring',
			quantity: 10,
			latitude: 37.7749,
			longitude: -122.4194,
			h3_index: '8728308280fffff',
			start_date: '2024-06-15',
			name: 'SF Tutoring'
		};

		spatialIndex.insert(sfSlot);

		const results = spatialIndex.query('8728308280fffff', 50);
		expect(results).toContainEqual(sfSlot);
	});

	it('should include remote slots in all queries', () => {
		const remoteSlot: any = {
			id: 'remote-1',
			type_id: 'tutoring',
			quantity: 10,
			location_type: 'remote',
			online_link: 'https://zoom.us',
			start_date: '2024-06-15',
			name: 'Remote Tutoring'
		};

		spatialIndex.insert(remoteSlot);

		const results = spatialIndex.query('8728308280fffff', 50);
		expect(results).toContainEqual(remoteSlot);
	});
});

describe('Type Index', () => {
	let typeIndex: TypeIndex;

	beforeEach(() => {
		typeIndex = new TypeIndex();
	});

	it('should index slots by type', () => {
		const tutoringSlot: any = {
			id: 'slot-1',
			type_id: 'tutoring',
			quantity: 10,
			latitude: 37.7749,
			longitude: -122.4194,
			start_date: '2024-06-15',
			name: 'Tutoring'
		};

		const grocerySlot: any = {
			id: 'slot-2',
			type_id: 'groceries',
			quantity: 5,
			latitude: 37.7749,
			longitude: -122.4194,
			start_date: '2024-06-15',
			name: 'Groceries'
		};

		typeIndex.insert(tutoringSlot);
		typeIndex.insert(grocerySlot);

		expect(typeIndex.size).toBe(2);
		expect(typeIndex.typeCount).toBe(2);

		const tutoringResults = typeIndex.query('tutoring');
		expect(tutoringResults).toHaveLength(1);
		expect(tutoringResults[0].id).toBe('slot-1');

		const groceryResults = typeIndex.query('groceries');
		expect(groceryResults).toHaveLength(1);
		expect(groceryResults[0].id).toBe('slot-2');
	});
});

describe('Slot Index (Composite)', () => {
	let index: SlotIndex;

	beforeEach(() => {
		index = new SlotIndex();
	});

	it('should build index from array of slots', () => {
		const slots: any[] = [
			{
				id: 'slot-1',
				type_id: 'tutoring',
				quantity: 10,
				latitude: 37.7749,
				longitude: -122.4194,
				start_date: '2024-06-15',
				name: 'SF Tutoring'
			},
			{
				id: 'slot-2',
				type_id: 'tutoring',
				quantity: 5,
				latitude: 37.7599,
				longitude: -122.4148,
				start_date: '2024-06-15',
				name: 'Mission Tutoring'
			},
			{
				id: 'slot-3',
				type_id: 'groceries',
				quantity: 20,
				latitude: 37.7749,
				longitude: -122.4194,
				start_date: '2024-06-15',
				name: 'SF Groceries'
			}
		];

		index.build(slots);

		const stats = index.getStats();
		expect(stats.totalSlots).toBe(3);
		expect(stats.typeCount).toBe(2);
	});

	it('should query matching slots for a need', () => {
		const capacitySlots: any[] = [
			{
				id: 'capacity-1',
				type_id: 'tutoring',
				quantity: 10,
				latitude: 37.7749,
				longitude: -122.4194,
				start_date: '2024-06-15',
				name: 'SF Tutoring'
			},
			{
				id: 'capacity-2',
				type_id: 'tutoring',
				quantity: 5,
				latitude: 40.7128,
				longitude: -74.0060,
				start_date: '2024-06-15',
				name: 'NYC Tutoring'
			},
			{
				id: 'capacity-3',
				type_id: 'groceries',
				quantity: 20,
				latitude: 37.7749,
				longitude: -122.4194,
				start_date: '2024-06-15',
				name: 'SF Groceries'
			}
		];

		index.build(capacitySlots);

		const needSlot: any = {
			id: 'need-1',
			type_id: 'tutoring',
			quantity: 3,
			latitude: 37.7610,
			longitude: -122.4160,
			start_date: '2024-06-15',
			search_radius_km: 50,
			name: 'Need Tutoring'
		};

		const matches = index.query(needSlot);

		// Should match SF tutoring (same city, same type)
		// Should NOT match NYC tutoring (different city, outside radius)
		// Should NOT match SF groceries (different type)
		expect(matches).toHaveLength(1);
		expect(matches[0].id).toBe('capacity-1');
	});

	it('should match remote capacity with any geographic need', () => {
		const capacitySlots: any[] = [
			{
				id: 'remote-1',
				type_id: 'tutoring',
				quantity: 10,
				location_type: 'remote',
				online_link: 'https://zoom.us',
				start_date: '2024-06-15',
				name: 'Remote Tutoring'
			}
		];

		index.build(capacitySlots);

		const tokyoNeed: any = {
			id: 'need-tokyo',
			type_id: 'tutoring',
			quantity: 3,
			latitude: 35.6762,
			longitude: 139.6503,
			start_date: '2024-06-15',
			name: 'Tokyo Student'
		};

		const matches = index.query(tokyoNeed);
		expect(matches).toHaveLength(1);
		expect(matches[0].id).toBe('remote-1');
	});

	it('should handle incremental updates', () => {
		const slot1: any = {
			id: 'slot-1',
			type_id: 'tutoring',
			quantity: 10,
			latitude: 37.7749,
			longitude: -122.4194,
			start_date: '2024-06-15',
			name: 'Tutoring 1'
		};

		const slot2: any = {
			id: 'slot-2',
			type_id: 'tutoring',
			quantity: 5,
			latitude: 37.7749,
			longitude: -122.4194,
			start_date: '2024-06-15',
			name: 'Tutoring 2'
		};

		// Insert
		index.insert(slot1);
		expect(index.getStats().totalSlots).toBe(1);

		index.insert(slot2);
		expect(index.getStats().totalSlots).toBe(2);

		// Remove
		index.remove(slot1);
		expect(index.getStats().totalSlots).toBe(1);

		const remaining = index.getAllSlots();
		expect(remaining).toHaveLength(1);
		expect(remaining[0].id).toBe('slot-2');
	});
});

describe('Performance Comparison', () => {
	it('should be faster than brute force for large datasets', () => {
		// Generate 1000 capacity slots
		const capacitySlots: any[] = [];
		for (let i = 0; i < 1000; i++) {
			capacitySlots.push({
				id: `capacity-${i}`,
				type_id: i % 10 === 0 ? 'tutoring' : 'other',
				quantity: 10,
				latitude: 37.7749 + (Math.random() - 0.5) * 0.1,
				longitude: -122.4194 + (Math.random() - 0.5) * 0.1,
				start_date: '2024-06-15',
				name: `Capacity ${i}`
			});
		}

		// Build index
		const startBuild = performance.now();
		const index = buildSlotIndex(capacitySlots);
		const buildTime = performance.now() - startBuild;

		// Query with index
		const needSlot: any = {
			id: 'need-1',
			type_id: 'tutoring',
			quantity: 3,
			latitude: 37.7749,
			longitude: -122.4194,
			start_date: '2024-06-15',
			search_radius_km: 50,
			name: 'Need'
		};

		const startQuery = performance.now();
		const matches = findMatchesIndexed(needSlot, index);
		const queryTime = performance.now() - startQuery;

		console.log(`Build time: ${buildTime.toFixed(2)}ms`);
		console.log(`Query time: ${queryTime.toFixed(2)}ms`);
		console.log(`Matches found: ${matches.length}`);

		// Index build should be fast (<100ms for 1000 slots)
		expect(buildTime).toBeLessThan(100);

		// Query should be very fast (<10ms)
		expect(queryTime).toBeLessThan(10);

		// Should find some matches
		expect(matches.length).toBeGreaterThan(0);
	});
});
