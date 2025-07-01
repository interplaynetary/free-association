/**
 * Utility functions for displaying capacities as map markers
 */
import type { Capacity, CapacitiesCollection } from '$lib/schema';
import { geocodeCapacityAddress, formatAddress } from './geocoding';

export interface CapacityMarkerData {
	id: string;
	capacity: Capacity;
	lnglat: { lng: number; lat: number };
	source: 'coordinates' | 'geocoded'; // Track how we got the coordinates
}

/**
 * Filters capacities that have valid geographic coordinates
 */
export function getCapacitiesWithCoordinates(
	capacities: CapacitiesCollection
): CapacityMarkerData[] {
	const markers: CapacityMarkerData[] = [];

	Object.entries(capacities).forEach(([id, capacity]) => {
		if (hasValidCoordinates(capacity)) {
			markers.push({
				id,
				capacity,
				lnglat: { lng: capacity.longitude!, lat: capacity.latitude! },
				source: 'coordinates'
			});
		}
	});

	return markers;
}

/**
 * Gets capacities that have addresses but no coordinates
 */
export function getCapacitiesWithAddresses(
	capacities: CapacitiesCollection
): Array<{ id: string; capacity: Capacity; address: string }> {
	const addressCapacities: Array<{ id: string; capacity: Capacity; address: string }> = [];

	Object.entries(capacities).forEach(([id, capacity]) => {
		// Only include if has address but no valid coordinates
		if (!hasValidCoordinates(capacity) && hasValidAddress(capacity)) {
			const address = formatAddress({
				street_address: capacity.street_address,
				city: capacity.city,
				state_province: capacity.state_province,
				postal_code: capacity.postal_code,
				country: capacity.country
			});

			if (address.trim()) {
				addressCapacities.push({
					id,
					capacity,
					address
				});
			}
		}
	});

	return addressCapacities;
}

/**
 * Geocodes capacities with addresses to get their coordinates
 */
export async function geocodeCapacitiesWithAddresses(
	capacities: CapacitiesCollection
): Promise<CapacityMarkerData[]> {
	const addressCapacities = getCapacitiesWithAddresses(capacities);
	const geocodedMarkers: CapacityMarkerData[] = [];

	// Process geocoding with some delay to respect rate limits
	for (const { id, capacity, address } of addressCapacities) {
		try {
			console.log(`[Geocoding] Attempting to geocode: ${address}`);
			const results = await geocodeCapacityAddress({
				street_address: capacity.street_address,
				city: capacity.city,
				state_province: capacity.state_province,
				postal_code: capacity.postal_code,
				country: capacity.country
			});

			if (results.length > 0) {
				const result = results[0]; // Use the first (best) result
				geocodedMarkers.push({
					id,
					capacity,
					lnglat: { lng: result.longitude, lat: result.latitude },
					source: 'geocoded'
				});
				console.log(
					`[Geocoding] Success for ${capacity.name}: ${result.latitude}, ${result.longitude}`
				);
			} else {
				console.warn(`[Geocoding] No results for ${capacity.name}: ${address}`);
			}
		} catch (error) {
			console.warn(`[Geocoding] Failed for ${capacity.name}: ${address}`, error);
		}
	}

	return geocodedMarkers;
}

/**
 * Get all capacity markers (both with coordinates and geocoded from addresses)
 */
export async function getAllCapacityMarkers(
	capacities: CapacitiesCollection
): Promise<CapacityMarkerData[]> {
	// Get capacities that already have coordinates
	const coordinateMarkers = getCapacitiesWithCoordinates(capacities);

	// Geocode capacities that have addresses but no coordinates
	const geocodedMarkers = await geocodeCapacitiesWithAddresses(capacities);

	// Combine and return
	return [...coordinateMarkers, ...geocodedMarkers];
}

/**
 * Checks if a capacity has valid geographic coordinates
 */
export function hasValidCoordinates(capacity: Capacity): boolean {
	return (
		typeof capacity.latitude === 'number' &&
		typeof capacity.longitude === 'number' &&
		capacity.latitude >= -90 &&
		capacity.latitude <= 90 &&
		capacity.longitude >= -180 &&
		capacity.longitude <= 180
	);
}

/**
 * Checks if a capacity has a valid address
 */
export function hasValidAddress(capacity: Capacity): boolean {
	return !!(
		capacity.street_address ||
		capacity.city ||
		capacity.state_province ||
		capacity.postal_code ||
		capacity.country
	);
}

/**
 * Updates a capacity's coordinates from a marker drag event
 */
export function updateCapacityCoordinates(
	capacity: Capacity,
	lnglat: { lng: number; lat: number }
): Capacity {
	return {
		...capacity,
		longitude: lnglat.lng,
		latitude: lnglat.lat
	};
}

/**
 * Creates popup content for a capacity marker
 */
export function formatCapacityPopupContent(capacity: Capacity): {
	title: string;
	details: Array<{ label: string; value: string }>;
} {
	const details: Array<{ label: string; value: string }> = [];

	if (capacity.quantity !== undefined) {
		details.push({
			label: 'Quantity',
			value: `${capacity.quantity}${capacity.unit ? ' ' + capacity.unit : ''}`
		});
	}

	if (capacity.location_type) {
		details.push({
			label: 'Location Type',
			value: capacity.location_type
		});

		// Add specific location information if available
		if (capacity.location_type === 'Specific') {
			// Check if address is available first
			if (
				capacity.street_address ||
				capacity.city ||
				capacity.state_province ||
				capacity.postal_code ||
				capacity.country
			) {
				const addressParts = [
					capacity.street_address,
					capacity.city,
					capacity.state_province,
					capacity.postal_code,
					capacity.country
				].filter(Boolean);

				if (addressParts.length > 0) {
					details.push({
						label: 'Address',
						value: addressParts.join(', ')
					});
				}
			}

			// Show coordinates if available (in addition to or instead of address)
			if (capacity.latitude !== undefined && capacity.longitude !== undefined) {
				details.push({
					label: 'Coordinates',
					value: `${capacity.latitude.toFixed(6)}, ${capacity.longitude.toFixed(6)}`
				});
			}
		}
	}

	if (capacity.start_date || capacity.start_time) {
		const startInfo = [capacity.start_date, capacity.start_time].filter(Boolean).join(' ');
		if (startInfo) {
			details.push({
				label: 'Start',
				value: startInfo
			});
		}
	}

	if (capacity.end_date || capacity.end_time) {
		const endInfo = [capacity.end_date, capacity.end_time].filter(Boolean).join(' ');
		if (endInfo) {
			details.push({
				label: 'End',
				value: endInfo
			});
		}
	}

	// Handle both ProviderCapacity and RecipientCapacity types
	if (
		'recipient_shares' in capacity &&
		capacity.recipient_shares &&
		Object.keys(capacity.recipient_shares).length > 0
	) {
		const shareCount = Object.keys(capacity.recipient_shares).length;
		details.push({
			label: 'Recipients',
			value: `${shareCount} share${shareCount === 1 ? '' : 's'}`
		});
	} else if ('share_percentage' in capacity) {
		details.push({
			label: 'Your Share',
			value: `${(capacity.share_percentage * 100).toFixed(1)}%`
		});
		if (capacity.computed_quantity !== undefined) {
			details.push({
				label: 'Your Quantity',
				value: `${capacity.computed_quantity}${capacity.unit ? ' ' + capacity.unit : ''}`
			});
		}
	}

	return {
		title: `${capacity.emoji || '📍'} ${capacity.name}`,
		details
	};
}
