/**
 * H3 Spatial Indexing Utilities
 * 
 * Provides utilities for H3-based spatial indexing of slots:
 * - Computing H3 cell IDs from coordinates
 * - Resolution selection based on zoom/density
 * - Distance estimation and radius queries
 * - Spatial compatibility checking
 */

import * as h3 from 'h3-js';
import type { AvailabilitySlot, NeedSlot } from './resources';

// ═══════════════════════════════════════════════════════════════════
// CONSTANTS
// ═══════════════════════════════════════════════════════════════════

/**
 * Default H3 resolution for slot indexing
 * Resolution 7 = ~1.22km average hexagon edge length
 * Good balance for city-scale matching
 */
export const DEFAULT_H3_RESOLUTION = 7;

/**
 * Default search radius in kilometers
 * 50km is reasonable for most service areas
 */
export const DEFAULT_SEARCH_RADIUS_KM = 50;

/**
 * Special H3 index for remote/online slots
 * These slots match any geographic location
 */
export const REMOTE_H3_INDEX = 'remote';

/**
 * Average hexagon edge lengths in kilometers for each H3 resolution
 * Source: https://h3geo.org/docs/core-library/restable/
 */
export const H3_EDGE_LENGTHS_KM: readonly number[] = [
	1107.71,  // res 0: continent scale
	418.68,   // res 1: country scale
	158.24,   // res 2: large region
	59.81,    // res 3: metro area
	22.61,    // res 4: city
	8.54,     // res 5: district
	3.23,     // res 6: neighborhood
	1.22,     // res 7: small area (DEFAULT)
	0.461,    // res 8: block
	0.174,    // res 9: building
	0.066,    // res 10: precise location
	0.025,    // res 11
	0.009,    // res 12
	0.003,    // res 13
	0.001,    // res 14
	0.0005    // res 15: ultra-precise
];

/**
 * Average hexagon area in square kilometers for each H3 resolution
 */
export const H3_AREAS_KM2: readonly number[] = [
	4250547.0,  // res 0
	607220.0,   // res 1
	86745.0,    // res 2
	12393.0,    // res 3
	1770.0,     // res 4
	252.9,      // res 5
	36.1,       // res 6
	5.16,       // res 7 (DEFAULT)
	0.74,       // res 8
	0.10,       // res 9
	0.015,      // res 10
	0.002,      // res 11
	0.0003,     // res 12
	0.00004,    // res 13
	0.000006,   // res 14
	0.0000009   // res 15
];

// ═══════════════════════════════════════════════════════════════════
// H3 INDEX COMPUTATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Compute H3 index for a slot
 * 
 * @param slot - Slot with location information
 * @param resolution - Optional H3 resolution (defaults to slot.h3_resolution or DEFAULT_H3_RESOLUTION)
 * @returns H3 cell ID string, or REMOTE_H3_INDEX for remote slots
 */
export function computeH3Index(
	slot: {
		latitude?: number;
		longitude?: number;
		location_type?: string;
		online_link?: string;
		h3_resolution?: number;
	},
	resolution?: number
): string {
	// Check if slot is remote/online
	if (isRemoteSlot(slot)) {
		return REMOTE_H3_INDEX;
	}

	// Require coordinates for geographic slots
	if (slot.latitude === undefined || slot.longitude === undefined) {
		throw new Error('Cannot compute H3 index: slot has no coordinates and is not remote');
	}

	// Use provided resolution, or slot's resolution, or default
	const res = resolution ?? slot.h3_resolution ?? DEFAULT_H3_RESOLUTION;

	// Validate resolution
	if (res < 0 || res > 15) {
		throw new Error(`Invalid H3 resolution: ${res} (must be 0-15)`);
	}

	// Compute H3 cell ID
	return h3.latLngToCell(slot.latitude, slot.longitude, res);
}

/**
 * Check if a slot is remote/online (no geographic location)
 */
export function isRemoteSlot(slot: {
	location_type?: string;
	online_link?: string;
}): boolean {
	return (
		slot.location_type?.toLowerCase().includes('remote') ||
		slot.location_type?.toLowerCase().includes('online') ||
		!!slot.online_link
	);
}

/**
 * Ensure slot has H3 index computed
 * Mutates the slot to add h3_index if missing
 * 
 * @param slot - Slot to ensure has H3 index
 * @returns The slot with h3_index populated
 */
export function ensureH3Index<T extends AvailabilitySlot | NeedSlot>(slot: T): T {
	if (!slot.h3_index) {
		try {
			slot.h3_index = computeH3Index(slot);
		} catch (e) {
			console.warn('[H3] Failed to compute H3 index for slot:', slot.id, e);
			// Set to remote as fallback
			slot.h3_index = REMOTE_H3_INDEX;
		}
	}
	return slot;
}

// ═══════════════════════════════════════════════════════════════════
// RESOLUTION SELECTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Get optimal H3 resolution based on map zoom level
 * 
 * @param zoom - Map zoom level (0-20)
 * @returns H3 resolution (0-15)
 */
export function getResolutionFromZoom(zoom: number): number {
	if (zoom < 2) return 0;
	if (zoom < 3) return 1;
	if (zoom < 4.5) return 2;
	if (zoom < 6) return 3;
	if (zoom < 7.5) return 4;
	if (zoom < 9) return 5;
	if (zoom < 10.5) return 6;
	if (zoom < 12) return 7;
	if (zoom < 13.5) return 8;
	if (zoom < 15) return 9;
	if (zoom < 16.5) return 10;
	if (zoom < 18) return 11;
	if (zoom < 20) return 12;
	return 13;
}

/**
 * Get optimal H3 resolution based on data density
 * 
 * @param density - Approximate number of slots per square km
 * @returns H3 resolution (0-15)
 */
export function getResolutionFromDensity(density: number): number {
	// High density (>100 slots/km²): use small hexagons
	if (density > 100) return 8; // ~0.74 km²
	
	// Medium density (10-100 slots/km²): use medium hexagons
	if (density > 10) return 7; // ~5.16 km²
	
	// Low density (<10 slots/km²): use large hexagons
	return 6; // ~36.1 km²
}

// ═══════════════════════════════════════════════════════════════════
// DISTANCE & RADIUS QUERIES
// ═══════════════════════════════════════════════════════════════════

/**
 * Get the number of H3 grid rings needed to cover a radius
 * 
 * @param radiusKm - Desired radius in kilometers
 * @param resolution - H3 resolution level
 * @returns Number of rings (k-rings) needed
 */
export function getGridRingsForRadius(radiusKm: number, resolution: number): number {
	const edgeLengthKm = H3_EDGE_LENGTHS_KM[resolution];
	// Each ring adds approximately one edge length to the radius
	// Add 1 to ensure full coverage (conservative estimate)
    if (edgeLengthKm !== undefined && edgeLengthKm > 0) {
	return Math.ceil(radiusKm / edgeLengthKm) + 1;
    } else {
        throw new Error(`Invalid H3 resolution: ${resolution}`);
    }
}

/**
 * Get all H3 cells within a radius of a center cell
 * 
 * @param centerCell - H3 cell ID at center
 * @param radiusKm - Radius in kilometers
 * @returns Array of H3 cell IDs covering the radius
 */
export function getCellsInRadius(centerCell: string, radiusKm: number): string[] {
	// Special case: remote cells match everything
	if (centerCell === REMOTE_H3_INDEX) {
		return [REMOTE_H3_INDEX];
	}

	const resolution = h3.getResolution(centerCell);
	const rings = getGridRingsForRadius(radiusKm, resolution);
	
	try {
		return h3.gridDisk(centerCell, rings);
	} catch (e) {
		console.warn('[H3] Failed to compute grid disk:', e);
		return [centerCell]; // Fallback to just the center cell
	}
}

/**
 * Get H3 cells covering a geographic area defined by bounds
 * 
 * @param bounds - Geographic bounds {north, south, east, west}
 * @param resolution - H3 resolution level
 * @returns Array of H3 cell IDs covering the area
 */
export function getCellsInBounds(
	bounds: { north: number; south: number; east: number; west: number },
	resolution: number
): string[] {
	const polygon = [
		[bounds.west, bounds.south],
		[bounds.west, bounds.north],
		[bounds.east, bounds.north],
		[bounds.east, bounds.south],
		[bounds.west, bounds.south]
	];

	try {
		return h3.polygonToCells(polygon, resolution, true);
	} catch (e) {
		console.warn('[H3] Failed to compute polygon cells:', e);
		return [];
	}
}

// ═══════════════════════════════════════════════════════════════════
// SPATIAL COMPATIBILITY
// ═══════════════════════════════════════════════════════════════════

/**
 * Check if two H3 cells are spatially compatible (within search radius)
 * 
 * @param cell1 - First H3 cell ID
 * @param cell2 - Second H3 cell ID
 * @param searchRadiusKm - Maximum distance in kilometers
 * @returns true if cells are within search radius
 */
export function cellsCompatible(
	cell1: string,
	cell2: string,
	searchRadiusKm: number = DEFAULT_SEARCH_RADIUS_KM
): boolean {
	// Remote cells are compatible with everything
	if (cell1 === REMOTE_H3_INDEX || cell2 === REMOTE_H3_INDEX) {
		return true;
	}

	// Get the coarser resolution (larger hexagons)
	const res1 = h3.getResolution(cell1);
	const res2 = h3.getResolution(cell2);
	const coarserRes = Math.min(res1, res2);

	// Convert both to coarser resolution for comparison
	const parent1 = res1 === coarserRes ? cell1 : h3.cellToParent(cell1, coarserRes);
	const parent2 = res2 === coarserRes ? cell2 : h3.cellToParent(cell2, coarserRes);

	// If same cell at coarser resolution, they're definitely compatible
	if (parent1 === parent2) {
		return true;
	}

	// Check grid distance
	try {
		const distance = h3.gridDistance(parent1, parent2);
		const rings = getGridRingsForRadius(searchRadiusKm, coarserRes);
		return distance <= rings;
	} catch (e) {
		// If grid distance fails (e.g., cells on different faces), fall back to geographic distance
		const [lat1, lng1] = h3.cellToLatLng(cell1);
		const [lat2, lng2] = h3.cellToLatLng(cell2);
		const distKm = haversineDistance(lat1, lng1, lat2, lng2);
		return distKm <= searchRadiusKm;
	}
}

/**
 * Calculate great-circle distance between two points using Haversine formula
 * 
 * @param lat1 - Latitude of first point (degrees)
 * @param lng1 - Longitude of first point (degrees)
 * @param lat2 - Latitude of second point (degrees)
 * @param lng2 - Longitude of second point (degrees)
 * @returns Distance in kilometers
 */
export function haversineDistance(
	lat1: number,
	lng1: number,
	lat2: number,
	lng2: number
): number {
	const R = 6371; // Earth's radius in km
	const dLat = ((lat2 - lat1) * Math.PI) / 180;
	const dLng = ((lng2 - lng1) * Math.PI) / 180;
	const a =
		Math.sin(dLat / 2) * Math.sin(dLat / 2) +
		Math.cos((lat1 * Math.PI) / 180) *
		Math.cos((lat2 * Math.PI) / 180) *
		Math.sin(dLng / 2) *
		Math.sin(dLng / 2);
	const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
	return R * c;
}

// ═══════════════════════════════════════════════════════════════════
// UTILITY FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Get human-readable description of H3 resolution
 */
export function getResolutionDescription(resolution: number): string {
	const descriptions = [
		'Continent scale (~4.2M km²)',
		'Country scale (~607K km²)',
		'Large region (~87K km²)',
		'Metro area (~12K km²)',
		'City (~1.8K km²)',
		'District (~253 km²)',
		'Neighborhood (~36 km²)',
		'Small area (~5 km²)',
		'Block (~0.7 km²)',
		'Building (~0.1 km²)',
		'Precise location (~15K m²)',
		'Very precise (~2K m²)',
		'Ultra precise (~300 m²)',
		'Extremely precise (~40 m²)',
		'Sub-meter (~6 m²)',
		'Centimeter (~1 m²)'
	];
	return descriptions[resolution] || 'Unknown resolution';
}

/**
 * Format H3 cell ID for display
 */
export function formatH3Cell(cell: string): string {
	if (cell === REMOTE_H3_INDEX) {
		return 'Remote/Online';
	}
	const res = h3.getResolution(cell);
	return `${cell.slice(0, 8)}... (res ${res})`;
}
