/**
 * Utility functions for displaying capacities as map markers
 */
import type { Capacity, CapacitiesCollection } from '$lib/schema';

export interface CapacityMarkerData {
	id: string;
	capacity: Capacity;
	lnglat: { lng: number; lat: number };
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
				lnglat: { lng: capacity.longitude!, lat: capacity.latitude! }
			});
		}
	});

	return markers;
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
		title: `${capacity.emoji ? capacity.emoji + ' ' : ''}${capacity.name}`,
		details
	};
}
