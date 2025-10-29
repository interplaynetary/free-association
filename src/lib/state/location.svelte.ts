import { writable, derived } from 'svelte/store';
import type { Writable } from 'svelte/store';
// V5: Import from v5 stores
import { myCapacitySlotsStore } from '$lib/commons/v5/stores.svelte';

// Live location data interface
export interface LiveLocationData {
	latitude: number;
	longitude: number;
	accuracy?: number;
	altitude?: number | null;
	altitudeAccuracy?: number | null;
	heading?: number | null;
	speed?: number | null;
	timestamp: number;
}

// Location tracking state
export const currentLocation: Writable<LiveLocationData | null> = writable(null);
export const isLocationTracking = writable(false);
export const locationError = writable<string | null>(null);

// Block list for live location access
export const liveLocationBlockList: Writable<string[]> = writable([]);

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
// V5: Based on capacity slots (simplified until v5 allocation algorithm is fully integrated)
// TODO: Integrate with v5 allocation algorithm when available
export const liveLocationAccessList = derived(
	[myCapacitySlotsStore],
	([$myCapacitySlots]) => {
		if (!$myCapacitySlots || $myCapacitySlots.length === 0) {
			return [];
		}

		// V5 TODO: This should be computed from allocation results
		// For now, return empty array (access control disabled until v5 allocation integration)
		// When v5 allocation is integrated, this should check:
		// 1. Who received allocations from my capacity slots
		// 2. Filter to only those with active/valid allocations
		// 3. Return their public keys
		
		console.log('[LIVE-LOCATION-ACCESS] V5 allocation integration pending - access list empty');
		return [];
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
