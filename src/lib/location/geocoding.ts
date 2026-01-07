/**
 * Geocoding utilities using Nominatim (OpenStreetMap)
 * Free service - please use responsibly per Nominatim usage policy
 */

console.log('[TRACE] src/lib/location/geocoding.ts: <module scope>');

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
	console.log('[TRACE] [ENTER] src/lib/location/geocoding.ts: rateLimitedFetch', { url });
	const now = Date.now();
	const timeSinceLastRequest = now - lastRequestTime;

	console.log(
		'[Geocoding] 🚨 PRODUCTION DEBUG: rateLimitedFetch called, timeSinceLastRequest:',
		timeSinceLastRequest
	);

	if (timeSinceLastRequest < MIN_REQUEST_INTERVAL) {
		const waitTime = MIN_REQUEST_INTERVAL - timeSinceLastRequest;
		console.log('[Geocoding] 🚨 PRODUCTION DEBUG: Rate limiting, waiting', waitTime, 'ms');
		console.log('[TRACE] [STEP] src/lib/location/geocoding.ts: rateLimitedFetch -> waiting for rate limit', { waitTime });
		await new Promise((resolve) => setTimeout(resolve, waitTime));
	}

	lastRequestTime = Date.now();

	console.log('[Geocoding] 🚨 PRODUCTION DEBUG: About to fetch URL:', url);
	console.log('[Geocoding] 🚨 PRODUCTION DEBUG: Environment check:', {
		hostname: typeof window !== 'undefined' ? window.location.hostname : 'SSR',
		protocol: typeof window !== 'undefined' ? window.location.protocol : 'SSR',
		userAgent: typeof navigator !== 'undefined' ? navigator.userAgent.substring(0, 50) : 'SSR'
	});

	try {
		console.log('[TRACE] [STEP] src/lib/location/geocoding.ts: rateLimitedFetch -> executing fetch');
		const response = await fetch(url, {
			headers: {
				'User-Agent':
					'PlayNet Free Association Map (https://github.com/playnet-org/free-association)'
			}
		});
		console.log(
			'[Geocoding] 🚨 PRODUCTION DEBUG: Fetch completed, response status:',
			response.status
		);
		console.log('[TRACE] [EXIT] src/lib/location/geocoding.ts: rateLimitedFetch', { status: response.status, ok: response.ok });
		return response;
	} catch (fetchError) {
		console.error('[Geocoding] 🚨 PRODUCTION DEBUG: Fetch failed:', fetchError);
		console.error('[Geocoding] 🚨 PRODUCTION DEBUG: Fetch error details:', {
			name: fetchError instanceof Error ? fetchError.name : 'Unknown',
			message: fetchError instanceof Error ? fetchError.message : String(fetchError)
		});
		console.log('[TRACE] [EXIT] src/lib/location/geocoding.ts: rateLimitedFetch (error)', { error: fetchError });
		throw fetchError;
	}
}

/**
 * Convert address to coordinates (forward geocoding)
 */
export async function geocodeAddress(address: string): Promise<GeocodeResult[]> {
	console.log('[TRACE] [ENTER] src/lib/location/geocoding.ts: geocodeAddress', { address });
	console.log('[Geocoding] 🚨 PRODUCTION DEBUG: geocodeAddress called with:', address);

	if (!address.trim()) {
		console.log('[Geocoding] 🚨 PRODUCTION DEBUG: Empty address provided');
		console.log('[TRACE] [EXIT] src/lib/location/geocoding.ts: geocodeAddress (empty address error)');
		throw new Error('Address is required');
	}

	const params = new URLSearchParams({
		q: address,
		format: 'jsonv2',
		addressdetails: '1',
		limit: '5'
	});

	const requestUrl = `${NOMINATIM_BASE_URL}/search?${params}`;
	console.log('[Geocoding] 🚨 PRODUCTION DEBUG: Request URL:', requestUrl);

	try {
		console.log('[Geocoding] 🚨 PRODUCTION DEBUG: About to call rateLimitedFetch');
		console.log('[TRACE] [STEP] src/lib/location/geocoding.ts: geocodeAddress -> calling rateLimitedFetch');
		const response = await rateLimitedFetch(requestUrl);
		console.log(
			'[Geocoding] 🚨 PRODUCTION DEBUG: Response received, status:',
			response.status,
			'ok:',
			response.ok
		);

		if (!response.ok) {
			console.log('[Geocoding] 🚨 PRODUCTION DEBUG: Response not ok, throwing error');
			console.log('[TRACE] [EXIT] src/lib/location/geocoding.ts: geocodeAddress (API error)', { status: response.status });
			throw new Error(`Nominatim API error: ${response.status}`);
		}

		console.log('[Geocoding] 🚨 PRODUCTION DEBUG: About to parse JSON response');
		const data = await response.json();
		console.log(
			'[Geocoding] 🚨 PRODUCTION DEBUG: JSON parsed, result count:',
			Array.isArray(data) ? data.length : 'not array'
		);

		const results = data.map((result: any) => ({
			latitude: parseFloat(result.lat),
			longitude: parseFloat(result.lon),
			display_name: result.display_name,
			address: result.address
		}));

		console.log('[Geocoding] 🚨 PRODUCTION DEBUG: Returning', results.length, 'results');
		console.log('[TRACE] [EXIT] src/lib/location/geocoding.ts: geocodeAddress (success)', { count: results.length });
		return results;
	} catch (error) {
		console.error('[Geocoding] 🚨 PRODUCTION DEBUG: Geocoding error:', error);
		console.error('[Geocoding] 🚨 PRODUCTION DEBUG: Error details:', {
			name: error instanceof Error ? error.name : 'Unknown',
			message: error instanceof Error ? error.message : String(error),
			stack: error instanceof Error ? error.stack : undefined
		});
		console.log('[TRACE] [EXIT] src/lib/location/geocoding.ts: geocodeAddress (failure)', { error });
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
	console.log('[TRACE] [ENTER] src/lib/location/geocoding.ts: geocodeCapacityAddress', addressComponents);
	const addressString = formatAddress(addressComponents);

	if (!addressString.trim()) {
		console.log('[TRACE] [EXIT] src/lib/location/geocoding.ts: geocodeCapacityAddress (no components error)');
		throw new Error('No address components provided');
	}

	console.log('[TRACE] [STEP] src/lib/location/geocoding.ts: geocodeCapacityAddress -> calling geocodeAddress', { addressString });
	try {
		const result = await geocodeAddress(addressString);
		console.log('[TRACE] [EXIT] src/lib/location/geocoding.ts: geocodeCapacityAddress (success)');
		return result;
	} catch (err) {
		console.log('[TRACE] [EXIT] src/lib/location/geocoding.ts: geocodeCapacityAddress (forwarding error)');
		throw err;
	}
}

/**
 * Convert coordinates to address (reverse geocoding)
 */
export async function reverseGeocode(
	latitude: number,
	longitude: number
): Promise<ReverseGeocodeResult> {
	console.log('[TRACE] [ENTER] src/lib/location/geocoding.ts: reverseGeocode', { latitude, longitude });
	if (!isValidCoordinate(latitude, longitude)) {
		console.log('[TRACE] [EXIT] src/lib/location/geocoding.ts: reverseGeocode (invalid coords)');
		throw new Error('Invalid coordinates');
	}

	const params = new URLSearchParams({
		lat: latitude.toString(),
		lon: longitude.toString(),
		format: 'jsonv2',
		addressdetails: '1'
	});

	try {
		console.log('[TRACE] [STEP] src/lib/location/geocoding.ts: reverseGeocode -> calling rateLimitedFetch');
		const response = await rateLimitedFetch(`${NOMINATIM_BASE_URL}/reverse?${params}`);

		if (!response.ok) {
			console.log('[TRACE] [EXIT] src/lib/location/geocoding.ts: reverseGeocode (API error)', { status: response.status });
			throw new Error(`Nominatim API error: ${response.status}`);
		}

		const data = await response.json();

		if (data.error) {
			console.log('[TRACE] [EXIT] src/lib/location/geocoding.ts: reverseGeocode (nominatim error)', { error: data.error });
			throw new Error(`Nominatim error: ${data.error}`);
		}

		console.log('[TRACE] [EXIT] src/lib/location/geocoding.ts: reverseGeocode (success)');
		return {
			display_name: data.display_name,
			address: data.address || {}
		};
	} catch (error) {
		console.error('Reverse geocoding error:', error);
		console.log('[TRACE] [EXIT] src/lib/location/geocoding.ts: reverseGeocode (failure)', { error });
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
