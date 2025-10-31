/**
 * Efficient geocoding cache system for capacity persistence
 *
 * This module provides:
 * 1. Address change detection to avoid unnecessary geocoding
 * 2. Persistent cache to avoid duplicate API calls across sessions
 * 3. Efficient batch processing for multiple addresses
 * 4. Integration with the existing geocoding utilities
 */

import { geocodeCapacityAddress, formatAddress } from './geocoding';
import type { AvailabilitySlot } from '$lib/protocol/schemas';

interface AddressComponents {
	street_address?: string;
	city?: string;
	state_province?: string;
	postal_code?: string;
	country?: string;
}

interface CachedGeocodeResult {
	latitude: number;
	longitude: number;
	timestamp: number; // When this was cached
	display_name: string;
}

interface AddressHash {
	hash: string;
	components: AddressComponents;
}

/**
 * Global in-memory cache for geocoding results
 * Key: address hash, Value: geocoding result
 */
const geocodingCache = new Map<string, CachedGeocodeResult>();

/**
 * Cache expiration time (7 days in milliseconds)
 * After this time, we'll re-geocode to get fresh data
 */
const CACHE_EXPIRATION_MS = 7 * 24 * 60 * 60 * 1000;

/**
 * Generate a consistent hash for address components
 * This helps us identify when an address has actually changed
 */
function hashAddressComponents(components: AddressComponents): string {
	// Normalize components by trimming whitespace and converting to lowercase
	const normalized = {
		street_address: components.street_address?.trim().toLowerCase() || '',
		city: components.city?.trim().toLowerCase() || '',
		state_province: components.state_province?.trim().toLowerCase() || '',
		postal_code: components.postal_code?.trim().toLowerCase() || '',
		country: components.country?.trim().toLowerCase() || ''
	};

	// Create a consistent string representation
	const addressString = [
		normalized.street_address,
		normalized.city,
		normalized.state_province,
		normalized.postal_code,
		normalized.country
	].join('|');

	// Simple hash function (could be replaced with crypto.subtle.digest for better hashing)
	let hash = 0;
	for (let i = 0; i < addressString.length; i++) {
		const char = addressString.charCodeAt(i);
		hash = (hash << 5) - hash + char;
		hash = hash & hash; // Convert to 32-bit integer
	}

	return hash.toString(36); // Base-36 for shorter strings
}

/**
 * Check if an address has changed by comparing current components with cached coordinates
 */
function hasAddressChanged(
	currentComponents: AddressComponents,
	existingLatitude?: number,
	existingLongitude?: number
): { changed: boolean; addressHash: string } {
	const addressHash = hashAddressComponents(currentComponents);

	// If no existing coordinates, address is "new"
	if (existingLatitude === undefined || existingLongitude === undefined) {
		return { changed: true, addressHash };
	}

	// Check if we have this address hash cached
	const cached = geocodingCache.get(addressHash);
	if (cached) {
		// Compare with existing coordinates (with small tolerance for floating point precision)
		const latMatch = Math.abs(cached.latitude - existingLatitude) < 0.0001;
		const lngMatch = Math.abs(cached.longitude - existingLongitude) < 0.0001;

		if (latMatch && lngMatch) {
			// Address hasn't changed
			return { changed: false, addressHash };
		}
	}

	return { changed: true, addressHash };
}

/**
 * Check if cached result is still valid (not expired)
 */
function isCacheValid(cached: CachedGeocodeResult): boolean {
	return Date.now() - cached.timestamp < CACHE_EXPIRATION_MS;
}

/**
 * Load geocoding cache from localStorage on startup
 */
function loadCacheFromStorage(): void {
	if (typeof window === 'undefined') return; // SSR check

	try {
		const stored = localStorage.getItem('geocoding-cache');
		if (stored) {
			const parsedCache = JSON.parse(stored) as Record<string, CachedGeocodeResult>;

			// Only load non-expired entries
			Object.entries(parsedCache).forEach(([hash, result]) => {
				if (isCacheValid(result)) {
					geocodingCache.set(hash, result);
				}
			});

			console.log(
				`[GEOCODING-CACHE] Loaded ${geocodingCache.size} valid cached entries from storage`
			);
		}
	} catch (error) {
		console.warn('[GEOCODING-CACHE] Failed to load cache from storage:', error);
	}
}

/**
 * Save geocoding cache to localStorage
 */
function saveCacheToStorage(): void {
	if (typeof window === 'undefined') return; // SSR check

	try {
		const cacheObject: Record<string, CachedGeocodeResult> = {};
		geocodingCache.forEach((value, key) => {
			if (isCacheValid(value)) {
				cacheObject[key] = value;
			}
		});

		localStorage.setItem('geocoding-cache', JSON.stringify(cacheObject));
		console.log(`[GEOCODING-CACHE] Saved ${Object.keys(cacheObject).length} entries to storage`);
	} catch (error) {
		console.warn('[GEOCODING-CACHE] Failed to save cache to storage:', error);
	}
}

/**
 * Geocode an address with caching
 */
async function geocodeWithCache(
	components: AddressComponents,
	addressHash: string
): Promise<CachedGeocodeResult | null> {
	// Check cache first
	const cached = geocodingCache.get(addressHash);
	if (cached && isCacheValid(cached)) {
		console.log(`[GEOCODING-CACHE] Cache hit for address hash: ${addressHash}`);
		return cached;
	}

	// Check if we have any address components to geocode
	const hasAddressData = Object.values(components).some((value) => value && value.trim());
	if (!hasAddressData) {
		console.log('[GEOCODING-CACHE] No address data to geocode');
		return null;
	}

	try {
		console.log(`[GEOCODING-CACHE] Cache miss, geocoding address hash: ${addressHash}`);
		console.log('[GEOCODING-CACHE] Address components:', components);

		const results = await geocodeCapacityAddress(components);

		if (results.length > 0) {
			const result = results[0]; // Take the first (best) result
			const cachedResult: CachedGeocodeResult = {
				latitude: result.latitude,
				longitude: result.longitude,
				timestamp: Date.now(),
				display_name: result.display_name
			};

			// Cache the result
			geocodingCache.set(addressHash, cachedResult);

			// Save to localStorage periodically
			saveCacheToStorage();

			console.log(`[GEOCODING-CACHE] Cached geocoding result for ${addressHash}:`, cachedResult);
			return cachedResult;
		} else {
			console.log(`[GEOCODING-CACHE] No geocoding results found for address hash: ${addressHash}`);
			return null;
		}
	} catch (error) {
		console.error(`[GEOCODING-CACHE] Geocoding failed for address hash ${addressHash}:`, error);
		return null;
	}
}

/**
 * Process a single slot's location data, geocoding if needed
 */
async function processSlotLocation(slot: AvailabilitySlot): Promise<AvailabilitySlot> {
	// Only process slots with specific location type
	if (slot.location_type !== 'Specific') {
		return slot;
	}

	const addressComponents: AddressComponents = {
		street_address: slot.street_address,
		city: slot.city,
		state_province: slot.state_province,
		postal_code: slot.postal_code,
		country: slot.country
	};

	// Check if address has changed
	const { changed, addressHash } = hasAddressChanged(
		addressComponents,
		slot.latitude,
		slot.longitude
	);

	if (!changed) {
		console.log(`[GEOCODING-CACHE] Address unchanged for slot ${slot.id}, skipping geocoding`);
		return slot;
	}

	console.log(`[GEOCODING-CACHE] Address changed for slot ${slot.id}, geocoding...`);

	// Geocode with caching
	const geocodeResult = await geocodeWithCache(addressComponents, addressHash);

	if (geocodeResult) {
		// Return slot with updated coordinates
		return {
			...slot,
			latitude: geocodeResult.latitude,
			longitude: geocodeResult.longitude
		};
	} else {
		// Geocoding failed, return original slot
		console.warn(
			`[GEOCODING-CACHE] Geocoding failed for slot ${slot.id}, keeping original coordinates`
		);
		return slot;
	}
}

/**
 * Process all slots in a capacity, geocoding addresses that have changed
 */
export async function processCapacityLocations(capacity: any): Promise<any> {
	if (!capacity.availability_slots || !Array.isArray(capacity.availability_slots)) {
		return capacity;
	}

	console.log(
		`[GEOCODING-CACHE] Processing ${capacity.availability_slots.length} slots for capacity ${capacity.id}`
	);

	// Process slots in parallel but with some rate limiting to respect Nominatim's limits
	const processedSlots: AvailabilitySlot[] = [];

	for (const slot of capacity.availability_slots) {
		try {
			const processedSlot = await processSlotLocation(slot);
			processedSlots.push(processedSlot);
		} catch (error) {
			console.error(`[GEOCODING-CACHE] Error processing slot ${slot.id}:`, error);
			// Keep original slot on error
			processedSlots.push(slot);
		}
	}

	return {
		...capacity,
		availability_slots: processedSlots
	};
}

/**
 * Process all capacities, geocoding addresses that have changed
 */
export async function processCapacitiesLocations(
	capacities: Record<string, any>
): Promise<Record<string, any>> {
	console.log(
		`[GEOCODING-CACHE] Processing locations for ${Object.keys(capacities).length} capacities`
	);

	const processedCapacities: Record<string, any> = {};

	// Process capacities sequentially to respect rate limits
	for (const [capacityId, capacity] of Object.entries(capacities)) {
		try {
			processedCapacities[capacityId] = await processCapacityLocations(capacity);
		} catch (error) {
			console.error(`[GEOCODING-CACHE] Error processing capacity ${capacityId}:`, error);
			// Keep original capacity on error
			processedCapacities[capacityId] = capacity;
		}
	}

	console.log(
		`[GEOCODING-CACHE] Completed processing ${Object.keys(processedCapacities).length} capacities`
	);
	return processedCapacities;
}

/**
 * Initialize the geocoding cache system
 * Call this when the app starts
 */
export function initializeGeocodingCache(): void {
	loadCacheFromStorage();
	console.log('[GEOCODING-CACHE] Geocoding cache system initialized');
}

/**
 * Clear the geocoding cache (useful for testing or manual cache reset)
 */
export function clearGeocodingCache(): void {
	geocodingCache.clear();
	if (typeof window !== 'undefined') {
		localStorage.removeItem('geocoding-cache');
	}
	console.log('[GEOCODING-CACHE] Geocoding cache cleared');
}

/**
 * Get cache statistics for debugging
 */
export function getGeocodingCacheStats(): {
	size: number;
	validEntries: number;
	expiredEntries: number;
} {
	let validEntries = 0;
	let expiredEntries = 0;

	geocodingCache.forEach((value) => {
		if (isCacheValid(value)) {
			validEntries++;
		} else {
			expiredEntries++;
		}
	});

	return {
		size: geocodingCache.size,
		validEntries,
		expiredEntries
	};
}
