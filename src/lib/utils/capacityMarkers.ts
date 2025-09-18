/**
 * Utility functions for displaying capacities as map markers
 *
 * Shows ALL available capacities on the map, regardless of current allocation status.
 * This allows users to discover new capacities and express desires for unallocated ones.
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
		// Handle both timestamped and direct capacity data
		const actualCapacity = (capacity as any)?.data || capacity;

		// Safety check: ensure availability_slots exists and is an array
		if (!actualCapacity.availability_slots || !Array.isArray(actualCapacity.availability_slots)) {
			return; // Skip this capacity if it doesn't have properly initialized slots
		}

		// Find the first slot with valid coordinates
		const slotWithCoords = actualCapacity.availability_slots.find(
			(slot: any) =>
				typeof slot.latitude === 'number' &&
				typeof slot.longitude === 'number' &&
				slot.latitude >= -90 &&
				slot.latitude <= 90 &&
				slot.longitude >= -180 &&
				slot.longitude <= 180
		);

		if (slotWithCoords) {
			markers.push({
				id,
				capacity: actualCapacity,
				lnglat: { lng: slotWithCoords.longitude!, lat: slotWithCoords.latitude! },
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
		// Handle both timestamped and direct capacity data
		const actualCapacity = (capacity as any)?.data || capacity;

		// Safety check: ensure availability_slots exists and is an array
		if (!actualCapacity.availability_slots || !Array.isArray(actualCapacity.availability_slots)) {
			return; // Skip this capacity if it doesn't have properly initialized slots
		}

		// Only include if has address but no valid coordinates
		if (!hasValidCoordinates(actualCapacity) && hasValidAddress(actualCapacity)) {
			// Find the first slot with address information
			const slotWithAddress = actualCapacity.availability_slots.find(
				(slot: any) =>
					!!(
						slot.street_address ||
						slot.city ||
						slot.state_province ||
						slot.postal_code ||
						slot.country
					)
			);

			if (slotWithAddress) {
				const address = formatAddress({
					street_address: slotWithAddress.street_address,
					city: slotWithAddress.city,
					state_province: slotWithAddress.state_province,
					postal_code: slotWithAddress.postal_code,
					country: slotWithAddress.country
				});

				if (address.trim()) {
					addressCapacities.push({
						id,
						capacity: actualCapacity,
						address
					});
				}
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

			// Safety check: ensure availability_slots exists and is an array
			if (!capacity.availability_slots || !Array.isArray(capacity.availability_slots)) {
				console.warn(`[Geocoding] Skipping capacity ${id} - no valid slots`);
				continue;
			}

			// Find the first slot with address information
			const slotWithAddress = capacity.availability_slots.find(
				(slot) =>
					!!(
						slot.street_address ||
						slot.city ||
						slot.state_province ||
						slot.postal_code ||
						slot.country
					)
			);

			if (!slotWithAddress) continue;

			const results = await geocodeCapacityAddress({
				street_address: slotWithAddress.street_address,
				city: slotWithAddress.city,
				state_province: slotWithAddress.state_province,
				postal_code: slotWithAddress.postal_code,
				country: slotWithAddress.country
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
 * Checks if a capacity has valid geographic coordinates (from any slot)
 */
export function hasValidCoordinates(capacity: Capacity): boolean {
	// Handle both timestamped and direct capacity data
	const actualCapacity = (capacity as any)?.data || capacity;

	// Safety check: ensure availability_slots exists and is an array
	if (!actualCapacity.availability_slots || !Array.isArray(actualCapacity.availability_slots)) {
		return false; // No valid coordinates if slots aren't properly initialized
	}

	return actualCapacity.availability_slots.some(
		(slot: any) =>
			typeof slot.latitude === 'number' &&
			typeof slot.longitude === 'number' &&
			slot.latitude >= -90 &&
			slot.latitude <= 90 &&
			slot.longitude >= -180 &&
			slot.longitude <= 180
	);
}

/**
 * Checks if a capacity has a valid address (from any slot)
 */
export function hasValidAddress(capacity: Capacity): boolean {
	// Handle both timestamped and direct capacity data
	const actualCapacity = (capacity as any)?.data || capacity;

	// Safety check: ensure availability_slots exists and is an array
	if (!actualCapacity.availability_slots || !Array.isArray(actualCapacity.availability_slots)) {
		return false; // No valid address if slots aren't properly initialized
	}

	return actualCapacity.availability_slots.some(
		(slot: any) =>
			!!(
				slot.street_address ||
				slot.city ||
				slot.state_province ||
				slot.postal_code ||
				slot.country
			)
	);
}

/**
 * Updates a capacity's coordinates from a marker drag event
 * Note: This updates the coordinates of the first slot with location data
 */
export function updateCapacityCoordinates(
	capacity: Capacity,
	lnglat: { lng: number; lat: number }
): Capacity {
	// Safety check: ensure availability_slots exists and is an array
	if (!capacity.availability_slots || !Array.isArray(capacity.availability_slots)) {
		// If no slots exist, create a default slot with the coordinates
		return {
			...capacity,
			availability_slots: [
				{
					id: `slot-${Date.now()}`,
					quantity: 1,
					longitude: lnglat.lng,
					latitude: lnglat.lat,
					location_type: 'Specific'
				}
			]
		};
	}

	// Find the first slot with coordinates or create updated slots
	const updatedSlots = capacity.availability_slots.map((slot, index) => {
		// Update the first slot that has coordinates, or the first slot if none have coordinates
		if (index === 0 || (slot.latitude !== undefined && slot.longitude !== undefined)) {
			return {
				...slot,
				longitude: lnglat.lng,
				latitude: lnglat.lat
			};
		}
		return slot;
	});

	return {
		...capacity,
		availability_slots: updatedSlots
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

	// Handle both timestamped and direct capacity data
	const actualCapacity = (capacity as any)?.data || capacity;

	// Safety check: ensure availability_slots exists and is an array
	if (!actualCapacity.availability_slots || !Array.isArray(actualCapacity.availability_slots)) {
		// Return basic info if no slots are initialized
		return {
			title: actualCapacity.name || 'Capacity',
			details: [
				{
					label: 'Status',
					value: 'No availability slots defined'
				}
			]
		};
	}

	// Show total quantity across all slots
	const totalQuantity = actualCapacity.availability_slots.reduce(
		(sum: number, slot: any) => sum + slot.quantity,
		0
	);
	if (totalQuantity > 0) {
		details.push({
			label: 'Total Quantity',
			value: `${totalQuantity}${actualCapacity.unit ? ' ' + actualCapacity.unit : ''}`
		});
	}

	// Show location information from first slot with location data
	const slotWithLocation = actualCapacity.availability_slots.find(
		(slot: any) => slot.location_type
	);
	if (slotWithLocation?.location_type) {
		details.push({
			label: 'Location Type',
			value: slotWithLocation.location_type
		});

		// Add specific location information if available
		if (slotWithLocation.location_type === 'Specific') {
			// Check if address is available first
			const addressParts = [
				slotWithLocation.street_address,
				slotWithLocation.city,
				slotWithLocation.state_province,
				slotWithLocation.postal_code,
				slotWithLocation.country
			].filter(Boolean);

			if (addressParts.length > 0) {
				details.push({
					label: 'Address',
					value: addressParts.join(', ')
				});
			}

			// Show coordinates if available (in addition to or instead of address)
			if (slotWithLocation.latitude !== undefined && slotWithLocation.longitude !== undefined) {
				details.push({
					label: 'Coordinates',
					value: `${slotWithLocation.latitude.toFixed(6)}, ${slotWithLocation.longitude.toFixed(6)}`
				});
			}
		}
	}

	// Show time information from slots (aggregate or show multiple if different)
	const slotsWithTime = actualCapacity.availability_slots.filter(
		(slot: any) => slot.start_date || slot.start_time || slot.end_date || slot.end_time
	);

	if (slotsWithTime.length > 0) {
		// If multiple slots have different time patterns, show count
		if (slotsWithTime.length > 1) {
			details.push({
				label: 'Time Slots',
				value: `${slotsWithTime.length} different time patterns`
			});
		} else {
			// Show details of single time pattern
			const slot = slotsWithTime[0];
			if (slot.start_date || slot.start_time) {
				const startInfo = [slot.start_date, slot.start_time].filter(Boolean).join(' ');
				if (startInfo) {
					details.push({
						label: 'Start',
						value: startInfo
					});
				}
			}

			if (slot.end_date || slot.end_time) {
				const endInfo = [slot.end_date, slot.end_time].filter(Boolean).join(' ');
				if (endInfo) {
					details.push({
						label: 'End',
						value: endInfo
					});
				}
			}
		}
	}

	// Handle both ProviderCapacity and capacity with allocation data
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
	}

	// Show allocation status from efficient algorithm
	const slotsWithAllocations =
		actualCapacity.availability_slots?.filter(
			(slot: any) => slot.allocated_quantity && slot.allocated_quantity > 0
		) || [];

	// Calculate total allocated vs total available
	const totalAllocated = slotsWithAllocations.reduce(
		(sum: number, slot: any) => sum + (slot.allocated_quantity || 0),
		0
	);

	const totalAvailable =
		actualCapacity.availability_slots?.reduce(
			(sum: number, slot: any) => sum + (slot.available_quantity || slot.quantity || 0),
			0
		) || 0;

	// Always show total available capacity
	if (totalAvailable > 0) {
		details.push({
			label: 'Total Available',
			value: `${totalAvailable}${actualCapacity.unit ? ' ' + actualCapacity.unit : ''}`
		});
	}

	// Show your current allocation if you have any
	if (totalAllocated > 0) {
		details.push({
			label: 'Your Current Allocation',
			value: `${totalAllocated}${actualCapacity.unit ? ' ' + actualCapacity.unit : ''}`
		});

		// Show remaining available
		const remaining = totalAvailable - totalAllocated;
		if (remaining > 0) {
			details.push({
				label: 'Still Available',
				value: `${remaining}${actualCapacity.unit ? ' ' + actualCapacity.unit : ''}`
			});
		}
	} else {
		// No current allocation - show as fully available for desire expression
		details.push({
			label: 'Status',
			value: 'Available for allocation (express desire to get some!)'
		});
	}

	return {
		title: `${actualCapacity.emoji || '📍'} ${actualCapacity.name}`,
		details
	};
}
