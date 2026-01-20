/**
 * H3 Spatial Indexing - Integration Test
 * 
 * Verifies that H3-based spatial matching works correctly
 */

import { describe, it, expect } from 'vitest';
import * as h3 from 'h3-js';
import {
	computeH3Index,
	ensureH3Index,
	getCellsInRadius,
	cellsCompatible,
	getResolutionFromZoom,
	DEFAULT_H3_RESOLUTION,
	DEFAULT_SEARCH_RADIUS_KM,
	REMOTE_H3_INDEX
} from '../spatial';

describe('H3 Spatial Indexing', () => {
	describe('computeH3Index', () => {
		it('should compute H3 index from coordinates', () => {
			const slot = {
				latitude: 37.7749, // San Francisco
				longitude: -122.4194
			};

			const h3Index = computeH3Index(slot);
			expect(h3Index).toBeTruthy();
			expect(typeof h3Index).toBe('string');
			expect(h3.isValidCell(h3Index)).toBe(true);
			expect(h3.getResolution(h3Index)).toBe(DEFAULT_H3_RESOLUTION);
		});

		it('should use custom resolution when provided', () => {
			const slot = {
				latitude: 37.7749,
				longitude: -122.4194,
				h3_resolution: 5
			};

			const h3Index = computeH3Index(slot);
			expect(h3.getResolution(h3Index)).toBe(5);
		});

		it('should return REMOTE_H3_INDEX for remote slots', () => {
			const remoteSlot = {
				location_type: 'remote',
				latitude: 37.7749,
				longitude: -122.4194
			};

			const h3Index = computeH3Index(remoteSlot);
			expect(h3Index).toBe(REMOTE_H3_INDEX);
		});

		it('should return REMOTE_H3_INDEX for online slots', () => {
			const onlineSlot = {
				online_link: 'https://zoom.us/meeting',
				latitude: 37.7749,
				longitude: -122.4194
			};

			const h3Index = computeH3Index(onlineSlot);
			expect(h3Index).toBe(REMOTE_H3_INDEX);
		});

		it('should throw error for slots without coordinates', () => {
			const slot = {
				city: 'San Francisco'
			};

			expect(() => computeH3Index(slot)).toThrow();
		});
	});

	describe('cellsCompatible', () => {
		it('should match cells at same location', () => {
			const sfCell = h3.latLngToCell(37.7749, -122.4194, 7); // SF
			expect(cellsCompatible(sfCell, sfCell, 50)).toBe(true);
		});

		it('should match cells within search radius', () => {
			const sfCell = h3.latLngToCell(37.7749, -122.4194, 7); // SF downtown
			const missionCell = h3.latLngToCell(37.7599, -122.4148, 7); // SF Mission (~2km away)

			expect(cellsCompatible(sfCell, missionCell, 50)).toBe(true);
		});

		it('should not match cells outside search radius', () => {
			const sfCell = h3.latLngToCell(37.7749, -122.4194, 7); // SF
			const nycCell = h3.latLngToCell(40.7128, -74.0060, 7); // NYC

			expect(cellsCompatible(sfCell, nycCell, 50)).toBe(false);
		});

		it('should always match remote cells', () => {
			const sfCell = h3.latLngToCell(37.7749, -122.4194, 7);
			const nycCell = h3.latLngToCell(40.7128, -74.0060, 7);

			expect(cellsCompatible(REMOTE_H3_INDEX, sfCell, 50)).toBe(true);
			expect(cellsCompatible(sfCell, REMOTE_H3_INDEX, 50)).toBe(true);
			expect(cellsCompatible(REMOTE_H3_INDEX, nycCell, 50)).toBe(true);
		});
	});

	describe('getCellsInRadius', () => {
		it('should return cells covering radius', () => {
			const centerCell = h3.latLngToCell(37.7749, -122.4194, 7);
			const cells = getCellsInRadius(centerCell, 10); // 10km radius

			expect(cells.length).toBeGreaterThan(1);
			expect(cells).toContain(centerCell);
		});

		it('should return only remote cell for remote slots', () => {
			const cells = getCellsInRadius(REMOTE_H3_INDEX, 50);
			expect(cells).toEqual([REMOTE_H3_INDEX]);
		});
	});

	describe('getResolutionFromZoom', () => {
		it('should return appropriate resolution for zoom levels', () => {
			expect(getResolutionFromZoom(1)).toBe(0); // World view
			expect(getResolutionFromZoom(5)).toBe(3); // Country view
			expect(getResolutionFromZoom(10)).toBe(6); // City view
			expect(getResolutionFromZoom(12)).toBe(8); // Neighborhood view
			expect(getResolutionFromZoom(15)).toBe(10); // Street view
		});
	});

	describe('ensureH3Index', () => {
		it('should compute H3 index if missing', () => {
			const slot: any = {
				id: 'test-slot',
				latitude: 37.7749,
				longitude: -122.4194
			};

			ensureH3Index(slot);
			expect(slot.h3_index).toBeTruthy();
			expect(h3.isValidCell(slot.h3_index)).toBe(true);
		});

		it('should not recompute if H3 index exists', () => {
			const existingIndex = h3.latLngToCell(37.7749, -122.4194, 7);
			const slot: any = {
				id: 'test-slot',
				latitude: 37.7749,
				longitude: -122.4194,
				h3_index: existingIndex
			};

			ensureH3Index(slot);
			expect(slot.h3_index).toBe(existingIndex);
		});
	});

	describe('Real-world scenarios', () => {
		it('should match tutoring slots in same neighborhood', () => {
			// Provider offers tutoring in SF Mission
			const providerSlot = {
				id: 'provider-1',
				type_id: 'tutoring',
				latitude: 37.7599,
				longitude: -122.4148,
				search_radius_km: 5
			};

			// Student needs tutoring in SF Mission (500m away)
			const needSlot = {
				id: 'need-1',
				type_id: 'tutoring',
				latitude: 37.7610,
				longitude: -122.4160,
				search_radius_km: 5
			};

			const providerH3 = computeH3Index(providerSlot);
			const needH3 = computeH3Index(needSlot);

			expect(cellsCompatible(providerH3, needH3, 5)).toBe(true);
		});

		it('should not match slots in different cities', () => {
			// Provider in SF
			const providerSlot = {
				id: 'provider-1',
				latitude: 37.7749,
				longitude: -122.4194
			};

			// Need in LA
			const needSlot = {
				id: 'need-1',
				latitude: 34.0522,
				longitude: -118.2437
			};

			const providerH3 = computeH3Index(providerSlot);
			const needH3 = computeH3Index(needSlot);

			expect(cellsCompatible(providerH3, needH3, 50)).toBe(false);
		});

		it('should match remote capacity with any geographic need', () => {
			// Remote tutoring capacity
			const remoteCapacity = {
				id: 'remote-tutor',
				location_type: 'remote',
				online_link: 'https://zoom.us/meeting'
			};

			// Need in Tokyo
			const tokyoNeed = {
				id: 'tokyo-student',
				latitude: 35.6762,
				longitude: 139.6503
			};

			const remoteH3 = computeH3Index(remoteCapacity);
			const tokyoH3 = computeH3Index(tokyoNeed);

			expect(cellsCompatible(remoteH3, tokyoH3, 50)).toBe(true);
		});
	});
});
