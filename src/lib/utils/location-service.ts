/**
 * Location Aggregation Service
 * 
 * Aggregates location data from multiple sources:
 * 1. Capacity/need slot locations (most specific)
 * 2. User profile location (general area)
 * 3. Browser Geolocation API (real-time, requires permission)
 */

import type { AvailabilitySlot, NeedSlot } from '$lib/commons/v5/schemas';
import type { QuestLocation } from '$lib/commons/v5/quest-schemas';

export interface LocationSource {
	source: 'slot' | 'profile' | 'geolocation';
	priority: number;
	location: QuestLocation;
}

/**
 * Extract location from capacity/need slots
 */
export function extractSlotLocations(
	capacities: AvailabilitySlot[] = [],
	needs: NeedSlot[] = []
): LocationSource[] {
	const locations: LocationSource[] = [];
	
	// Extract from capacities
	for (const capacity of capacities) {
		if (capacity.city || capacity.latitude || capacity.longitude) {
			locations.push({
				source: 'slot',
				priority: 3, // Highest priority
				location: {
					city: capacity.city,
					state_province: capacity.state_province,
					country: capacity.country,
					latitude: capacity.latitude,
					longitude: capacity.longitude,
					online: capacity.location_type === 'online' || !!capacity.online_link
				}
			});
		}
	}
	
	// Extract from needs
	for (const need of needs) {
		if (need.city || need.latitude || need.longitude) {
			locations.push({
				source: 'slot',
				priority: 3,
				location: {
					city: need.city,
					state_province: need.state_province,
					country: need.country,
					latitude: need.latitude,
					longitude: need.longitude,
					online: need.location_type === 'online' || !!need.online_link
				}
			});
		}
	}
	
	return locations;
}

/**
 * Get user profile location (if stored)
 * This would typically come from user settings
 */
export function getProfileLocation(): LocationSource | null {
	// Check if user has profile location in localStorage
	try {
		const storedLocation = localStorage.getItem('user_profile_location');
		if (storedLocation) {
			const location = JSON.parse(storedLocation);
			return {
				source: 'profile',
				priority: 2, // Medium priority
				location
			};
		}
	} catch (err) {
		console.error('[LOCATION-SERVICE] Failed to get profile location:', err);
	}
	
	return null;
}

/**
 * Get browser geolocation (requires user permission)
 */
export async function getBrowserGeolocation(): Promise<LocationSource | null> {
	if (typeof window === 'undefined' || !navigator.geolocation) {
		return null;
	}
	
	return new Promise((resolve) => {
		navigator.geolocation.getCurrentPosition(
			(position) => {
				resolve({
					source: 'geolocation',
					priority: 1, // Lower priority (less specific)
					location: {
						latitude: position.coords.latitude,
						longitude: position.coords.longitude,
						online: false
					}
				});
			},
			(error) => {
				console.log('[LOCATION-SERVICE] Geolocation error:', error.message);
				resolve(null);
			},
			{
				timeout: 5000,
				maximumAge: 300000 // Cache for 5 minutes
			}
		);
	});
}

/**
 * Reverse geocode coordinates to get city/country
 * Uses Nominatim (OpenStreetMap) free API
 */
export async function reverseGeocode(
	latitude: number,
	longitude: number
): Promise<Partial<QuestLocation> | null> {
	try {
		const response = await fetch(
			`https://nominatim.openstreetmap.org/reverse?lat=${latitude}&lon=${longitude}&format=json&accept-language=en`,
			{
				headers: {
					'User-Agent': 'FreeAssociation/1.0'
				}
			}
		);
		
		if (!response.ok) {
			return null;
		}
		
		const data = await response.json();
		
		return {
			city: data.address?.city || data.address?.town || data.address?.village,
			state_province: data.address?.state,
			country: data.address?.country,
			latitude,
			longitude
		};
	} catch (err) {
		console.error('[LOCATION-SERVICE] Reverse geocode error:', err);
		return null;
	}
}

/**
 * Aggregate all location sources and return prioritized list
 */
export async function aggregateLocations(
	capacities: AvailabilitySlot[] = [],
	needs: NeedSlot[] = [],
	includeGeolocation: boolean = true
): Promise<QuestLocation[]> {
	const sources: LocationSource[] = [];
	
	// 1. Get slot locations (highest priority)
	const slotLocations = extractSlotLocations(capacities, needs);
	sources.push(...slotLocations);
	
	// 2. Get profile location (medium priority)
	const profileLocation = getProfileLocation();
	if (profileLocation) {
		sources.push(profileLocation);
	}
	
	// 3. Get browser geolocation (lowest priority)
	if (includeGeolocation) {
		try {
			const geoLocation = await getBrowserGeolocation();
			if (geoLocation) {
				// Enhance with reverse geocoding if we only have coordinates
				if (geoLocation.location.latitude && geoLocation.location.longitude) {
					const enhanced = await reverseGeocode(
						geoLocation.location.latitude,
						geoLocation.location.longitude
					);
					if (enhanced) {
						geoLocation.location = { ...geoLocation.location, ...enhanced };
					}
				}
				sources.push(geoLocation);
			}
		} catch (err) {
			console.error('[LOCATION-SERVICE] Failed to get geolocation:', err);
		}
	}
	
	// Sort by priority (highest first) and remove duplicates
	sources.sort((a, b) => b.priority - a.priority);
	
	// Deduplicate based on city/country combination
	const uniqueLocations = new Map<string, QuestLocation>();
	for (const source of sources) {
		const key = `${source.location.city || 'unknown'}-${source.location.country || 'unknown'}`;
		if (!uniqueLocations.has(key)) {
			uniqueLocations.set(key, source.location);
		}
	}
	
	return Array.from(uniqueLocations.values());
}

/**
 * Save profile location to localStorage
 */
export function saveProfileLocation(location: QuestLocation): void {
	try {
		localStorage.setItem('user_profile_location', JSON.stringify(location));
	} catch (err) {
		console.error('[LOCATION-SERVICE] Failed to save profile location:', err);
	}
}

/**
 * Clear profile location
 */
export function clearProfileLocation(): void {
	try {
		localStorage.removeItem('user_profile_location');
	} catch (err) {
		console.error('[LOCATION-SERVICE] Failed to clear profile location:', err);
	}
}

