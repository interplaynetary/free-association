/**
 * Geocoding utilities using Nominatim (OpenStreetMap)
 * Free service - please use responsibly per Nominatim usage policy
 */

export interface GeocodeResult {
	latitude: number;
	longitude: number;
	display_name: string;
	address?: {
		house_number?: string;
		road?: string;
		city?: string;
		state?: string;
		postcode?: string;
		country?: string;
	};
}

export interface ReverseGeocodeResult {
	display_name: string;
	address: {
		house_number?: string;
		road?: string;
		city?: string;
		state?: string;
		postcode?: string;
		country?: string;
	};
}

const NOMINATIM_BASE_URL = 'https://nominatim.openstreetmap.org';

// Rate limiting: Nominatim requires max 1 request per second
let lastRequestTime = 0;
const MIN_REQUEST_INTERVAL = 1000; // 1 second

async function rateLimitedFetch(url: string): Promise<Response> {
	const now = Date.now();
	const timeSinceLastRequest = now - lastRequestTime;

	if (timeSinceLastRequest < MIN_REQUEST_INTERVAL) {
		const waitTime = MIN_REQUEST_INTERVAL - timeSinceLastRequest;
		await new Promise((resolve) => setTimeout(resolve, waitTime));
	}

	lastRequestTime = Date.now();

	return fetch(url, {
		headers: {
			'User-Agent': 'Free-Association-App/1.0 (contact: your-email@domain.com)' // Replace with actual contact
		}
	});
}

/**
 * Convert address to coordinates (forward geocoding)
 */
export async function geocodeAddress(address: string): Promise<GeocodeResult[]> {
	if (!address.trim()) {
		throw new Error('Address is required');
	}

	const params = new URLSearchParams({
		q: address,
		format: 'jsonv2',
		addressdetails: '1',
		limit: '5'
	});

	try {
		const response = await rateLimitedFetch(`${NOMINATIM_BASE_URL}/search?${params}`);

		if (!response.ok) {
			throw new Error(`Nominatim API error: ${response.status}`);
		}

		const data = await response.json();

		return data.map((result: any) => ({
			latitude: parseFloat(result.lat),
			longitude: parseFloat(result.lon),
			display_name: result.display_name,
			address: result.address
		}));
	} catch (error) {
		console.error('Geocoding error:', error);
		throw new Error('Failed to geocode address');
	}
}

/**
 * Geocode capacity address fields to coordinates
 */
export async function geocodeCapacityAddress(addressComponents: {
	street_address?: string;
	city?: string;
	state_province?: string;
	postal_code?: string;
	country?: string;
}): Promise<GeocodeResult[]> {
	const addressString = formatAddress(addressComponents);

	if (!addressString.trim()) {
		throw new Error('No address components provided');
	}

	return geocodeAddress(addressString);
}

/**
 * Convert coordinates to address (reverse geocoding)
 */
export async function reverseGeocode(
	latitude: number,
	longitude: number
): Promise<ReverseGeocodeResult> {
	if (!isValidCoordinate(latitude, longitude)) {
		throw new Error('Invalid coordinates');
	}

	const params = new URLSearchParams({
		lat: latitude.toString(),
		lon: longitude.toString(),
		format: 'jsonv2',
		addressdetails: '1'
	});

	try {
		const response = await rateLimitedFetch(`${NOMINATIM_BASE_URL}/reverse?${params}`);

		if (!response.ok) {
			throw new Error(`Nominatim API error: ${response.status}`);
		}

		const data = await response.json();

		if (data.error) {
			throw new Error(`Nominatim error: ${data.error}`);
		}

		return {
			display_name: data.display_name,
			address: data.address || {}
		};
	} catch (error) {
		console.error('Reverse geocoding error:', error);
		throw new Error('Failed to reverse geocode coordinates');
	}
}

/**
 * Validate coordinates
 */
export function isValidCoordinate(latitude: number, longitude: number): boolean {
	return (
		typeof latitude === 'number' &&
		typeof longitude === 'number' &&
		latitude >= -90 &&
		latitude <= 90 &&
		longitude >= -180 &&
		longitude <= 180 &&
		!isNaN(latitude) &&
		!isNaN(longitude)
	);
}

/**
 * Parse address components into our capacity schema format
 */
export function parseAddressComponents(address: ReverseGeocodeResult['address']) {
	return {
		street_address: [address.house_number, address.road].filter(Boolean).join(' ') || undefined,
		city: address.city || undefined,
		state_province: address.state || undefined,
		postal_code: address.postcode || undefined,
		country: address.country || undefined
	};
}

/**
 * Format address components into a single string
 */
export function formatAddress(components: {
	street_address?: string;
	city?: string;
	state_province?: string;
	postal_code?: string;
	country?: string;
}): string {
	const parts = [
		components.street_address,
		components.city,
		components.state_province,
		components.postal_code,
		components.country
	].filter(Boolean);

	return parts.join(', ');
}
