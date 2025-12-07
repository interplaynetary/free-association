/**
 * Configuration for Holster peer connections and data API
 * Uses environment variables from .env files
 *
 * Test-friendly: Provides safe defaults in test environments (no localStorage access)
 * Browser-friendly: Uses environment variables or localStorage overrides in browser
 */

// ═══════════════════════════════════════════════════════════════════
// HOLSTER CONFIGURATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Get Holster configuration
 *
 * In tests: Returns safe defaults (no localStorage access)
 * In browser: Uses environment variables or localStorage overrides
 */
function getHolsterConfig() {
	// Test environment - return safe defaults
	if (import.meta.env.VITEST || typeof window === 'undefined') {
		return {
			peers: [],
			indexedDB: false,
			file: undefined
		};
	}

	// Browser environment - use env vars with localStorage overrides
	try {
		return {
			peers: (
				import.meta.env.VITE_HOLSTER_PEERS ||
				(typeof localStorage !== 'undefined' ? localStorage.getItem('holster_peers') : null) ||
				'wss://holster.haza.website'
			).split(','),
			indexedDB: import.meta.env.VITE_HOLSTER_INDEXEDDB !== 'false',
			file: import.meta.env.VITE_HOLSTER_FILE || undefined
		};
	} catch (error) {
		console.warn('[CONFIG] Error accessing config, using defaults:', error);
		return {
			peers: ['wss://holster.haza.website'],
			indexedDB: true,
			file: undefined
		};
	}
}

/**
 * Configuration Object
 */
export const config = {
	holster: getHolsterConfig(),
	dataApi: {
		url: import.meta.env.VITE_DATA_API_URL || 'http://localhost:8767'
	}
};
