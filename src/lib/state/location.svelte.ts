import { writable, derived } from 'svelte/store';
import type { Writable } from 'svelte/store';
import { userCapacities, computedProviderAllocations } from './core.svelte';
import type { LiveLocationData as SchemaLiveLocationData, NetworkLiveLocations } from '$lib/schema';

// Re-export type from schema for convenience
export type LiveLocationData = SchemaLiveLocationData;

// Location tracking state
export const currentLocation: Writable<LiveLocationData | null> = writable(null);
export const isLocationTracking = writable(false);
export const locationError = writable<string | null>(null);

// Block list for live location access
export const liveLocationBlockList: Writable<string[]> = writable([]);

// Network live locations from contributors
export const networkLiveLocations: Writable<NetworkLiveLocations> = writable({});
export const isLoadingLiveLocation = writable(false);
export const isLoadingBlockList = writable(false);

// Update location from geolocation events
export function updateLocation(coords: GeolocationCoordinates, timestamp?: number) {
	const locationData: LiveLocationData = {
		latitude: coords.latitude,
		longitude: coords.longitude,
		accuracy: coords.accuracy,
		altitude: coords.altitude,
		altitudeAccuracy: coords.altitudeAccuracy,
		heading: coords.heading,
		speed: coords.speed,
		timestamp: timestamp || Date.now()
	};

	currentLocation.set(locationData);
	locationError.set(null);

	console.log('[LIVE-LOCATION] Updated:', locationData);
}

// Set location tracking state
export function setLocationTracking(isTracking: boolean) {
	isLocationTracking.set(isTracking);
}

// Set location error
export function setLocationError(error: string | null) {
	locationError.set(error);
	if (error) {
		console.error('[LIVE-LOCATION] Error:', error);
	}
}

// Derived store for location coordinates as [lng, lat] (MapLibre format)
export const currentLocationLngLat = derived(currentLocation, ($location) => {
	if (!$location) return null;
	return [$location.longitude, $location.latitude] as [number, number];
});

// Derived store for location text display
export const currentLocationText = derived(currentLocation, ($location) => {
	if (!$location) return 'No location';
	return `${$location.latitude.toFixed(6)}, ${$location.longitude.toFixed(6)}`;
});

// Derived store of those who have access to our live-location:
// those who have allocations in our provider capacities from the efficient algorithm.
export const liveLocationAccessList = derived(
	[userCapacities, computedProviderAllocations],
	([$userCapacities, $computedProviderAllocations]) => {
		if (!$userCapacities || !$computedProviderAllocations) {
			return [];
		}

		const accessList: string[] = [];

		// Iterate through all our provider capacities
		Object.entries($userCapacities).forEach(([capacityId, capacity]) => {
			// If this is a provider capacity (our capacity), check allocations
			if ('recipient_shares' in capacity) {
				// Get allocation results for this capacity from the efficient algorithm
				const capacityAllocations = $computedProviderAllocations[capacityId];
				if (capacityAllocations) {
					// Check all slots for recipients with final allocations
					Object.values(capacityAllocations).forEach((slotAllocation) => {
						Object.keys(slotAllocation.final_allocations || {}).forEach((recipientId) => {
							const allocation = slotAllocation.final_allocations[recipientId] || 0;
							if (allocation > 0 && !accessList.includes(recipientId)) {
								accessList.push(recipientId);
							}
						});
					});
				}
			}
		});

		console.log('[LIVE-LOCATION-ACCESS] Recipients with live-location access:', accessList);
		return accessList;
	}
);

// Filtered access list that excludes blocked users
export const filteredLiveLocationAccessList = derived(
	[liveLocationAccessList, liveLocationBlockList],
	([$accessList, $blockList]) => {
		const filteredList = $accessList.filter((userId) => !$blockList.includes(userId));
		console.log(
			'[LIVE-LOCATION-ACCESS] Filtered access list (excluding blocked users):',
			filteredList
		);
		return filteredList;
	}
);
