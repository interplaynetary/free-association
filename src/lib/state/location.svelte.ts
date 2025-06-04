import { writable, derived } from 'svelte/store';
import type { Writable } from 'svelte/store';

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
