/**
 * Location-Based Sorting and Proximity Search System
 *
 * This module provides sophisticated spatial querying capabilities with:
 * - Distance calculations using Haversine formula
 * - Travel time estimation for different transportation modes
 * - Proximity-based sorting and filtering
 * - Geographic grouping and categorization
 * - Support for multiple location data formats
 *
 * @example Location-Based Sorting
 * ```typescript
 * const sorted = sortObjectsByProximity(objects, {
 *   lat: 37.7749, lng: -122.4194, // San Francisco
 *   travelMode: 'walking',
 *   maxDistance: 5000 // 5km radius
 * });
 *
 * const nearby = findObjectsWithinTravelTime(objects, {
 *   lat: 37.7749, lng: -122.4194,
 *   travelMode: 'driving',
 *   maxTravelTimeMinutes: 30
 * });
 * ```
 */

// ===== TYPES & INTERFACES =====

export interface LocationPoint {
	lat: number;
	lng: number;
}

export interface LocationSortConfig {
	lat: number;
	lng: number;
	travelMode?: 'walking' | 'driving' | 'transit' | 'air'; // Travel mode for time estimation
	maxDistance?: number; // Maximum distance in meters (filter)
	speedKmh?: number; // Custom speed in km/h (overrides travelMode defaults)
}

export interface LocationResult {
	objectId: string;
	object: any;
	distance: number; // meters
	travelTime: number; // minutes
	location: LocationPoint;
}

// ===== LOCATION UTILITIES =====

/**
 * Calculate distance between two points using Haversine formula
 */
export const calculateDistance = (lat1: number, lng1: number, lat2: number, lng2: number): number => {
	const R = 6371000; // Earth's radius in meters
	const φ1 = (lat1 * Math.PI) / 180;
	const φ2 = (lat2 * Math.PI) / 180;
	const Δφ = ((lat2 - lat1) * Math.PI) / 180;
	const Δλ = ((lng2 - lng1) * Math.PI) / 180;

	const a =
		Math.sin(Δφ / 2) * Math.sin(Δφ / 2) +
		Math.cos(φ1) * Math.cos(φ2) * Math.sin(Δλ / 2) * Math.sin(Δλ / 2);
	const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

	return R * c; // Distance in meters
};

/**
 * Extract location from object
 */
export const extractLocation = (obj: any): LocationPoint | null => {
	// Direct coordinates
	if (obj.latitude != null && obj.longitude != null) {
		return { lat: obj.latitude, lng: obj.longitude };
	}

	// Nested coordinates
	if (obj.location?.lat != null && obj.location?.lng != null) {
		return { lat: obj.location.lat, lng: obj.location.lng };
	}

	// TODO: Could add geocoding for address-based locations
	// For now, return null if no coordinates available
	return null;
};

/**
 * Estimate travel time based on distance and mode
 */
export const estimateTravelTime = (distance: number, mode: string, customSpeed?: number): number => {
	if (customSpeed) {
		return (distance / 1000 / customSpeed) * 60; // Convert to minutes
	}

	// Default speeds in km/h
	const speeds = {
		walking: 5,
		driving: 40, // City driving average
		transit: 25, // Public transit average
		air: 800 // Flight speed (for very long distances)
	};

	const speed = speeds[mode as keyof typeof speeds] || speeds.walking;
	return (distance / 1000 / speed) * 60; // Convert to minutes
};

// ===== LOCATION SORTING FUNCTIONS =====

/**
 * Sort objects by proximity to a reference location
 */
export function sortObjectsByProximity(
	objects: Record<string, any>,
	config: LocationSortConfig
): LocationResult[] {
	const results: LocationResult[] = [];

	Object.entries(objects).forEach(([objectId, obj]) => {
		const location = extractLocation(obj);
		if (!location) return; // Skip objects without location

		const distance = calculateDistance(config.lat, config.lng, location.lat, location.lng);

		// Apply distance filter if specified
		if (config.maxDistance && distance > config.maxDistance) return;

		const travelTime = estimateTravelTime(
			distance,
			config.travelMode || 'walking',
			config.speedKmh
		);

		results.push({
			objectId,
			object: obj,
			distance,
			travelTime,
			location
		});
	});

	// Sort by distance (closest first)
	return results.sort((a, b) => a.distance - b.distance);
}

/**
 * Find objects within a certain travel time
 */
export function findObjectsWithinTravelTime(
	objects: Record<string, any>,
	config: LocationSortConfig & { maxTravelTimeMinutes: number }
): LocationResult[] {
	const sorted = sortObjectsByProximity(objects, config);
	return sorted.filter((result) => result.travelTime <= config.maxTravelTimeMinutes);
}

/**
 * Group objects by travel time ranges
 */
export function groupObjectsByTravelTime(
	objects: Record<string, any>,
	config: LocationSortConfig,
	timeRanges: number[] = [5, 15, 30, 60] // minutes
): Record<string, LocationResult[]> {
	const sorted = sortObjectsByProximity(objects, config);
	const groups: Record<string, LocationResult[]> = {};

	// Initialize groups
	timeRanges.forEach((time, index) => {
		const label = index === 0 ? `0-${time}min` : `${timeRanges[index - 1]}-${time}min`;
		groups[label] = [];
	});
	groups[`${timeRanges[timeRanges.length - 1]}min+`] = [];

	// Categorize results
	sorted.forEach((result) => {
		let categorized = false;

		for (let i = 0; i < timeRanges.length; i++) {
			const maxTime = timeRanges[i];
			const minTime = i === 0 ? 0 : timeRanges[i - 1];

			if (result.travelTime <= maxTime && result.travelTime > minTime) {
				const label = i === 0 ? `0-${maxTime}min` : `${minTime}-${maxTime}min`;
				groups[label].push(result);
				categorized = true;
				break;
			}
		}

		if (!categorized) {
			groups[`${timeRanges[timeRanges.length - 1]}min+`].push(result);
		}
	});

	return groups;
} 