/**
 * V5 Configuration - Test-Friendly
 * 
 * Provides minimal config needed for V5 stores and Holster integration.
 * Safe to import in test environments.
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
		console.warn('[CONFIG-V5] Error accessing config, using defaults:', error);
		return {
			peers: ['wss://holster.haza.website'],
			indexedDB: true,
			file: undefined
		};
	}
}

/**
 * V5 Configuration Object
 */
export const config = {
	holster: getHolsterConfig()
};
