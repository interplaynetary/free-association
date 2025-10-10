// Configuration for Gun and Holster peer connections
// Uses environment variables from .env files

export const config = {
  gun: {
    peers: [
      // Use environment variable or fallback to defaults
      import.meta.env.VITE_GUN_PEER_URL || 'http://localhost:8765/gun',
      // Keep single external peer as fallback (matching current branch)
      'https://104.248.129.153/gun'
    ],
    localStorage: false,
    radisk: true
  },
  holster: {
    peers: [
      // Use environment variable or fallback to default
      import.meta.env.VITE_HOLSTER_PEER_URL || 'wss://holster.haza.website'
    ],
    indexedDB: true,
    file: 'holster-data'
  },
  dataApi: {
    url: import.meta.env.VITE_DATA_API_URL || 'http://localhost:8767'
  }
};

// ============================================================================
// Feature Flags for Gradual Holster Migration
// ============================================================================

/**
 * These flags enable toggling between Gun and Holster implementations
 * for each module during the migration process.
 *
 * Flags can be set via:
 * 1. Environment variables (VITE_USE_HOLSTER_*)
 * 2. localStorage (USE_HOLSTER_*)
 *
 * Example: localStorage.setItem('USE_HOLSTER_CONTACTS', 'true')
 */

/**
 * Use Holster for contacts management
 * - When true: uses contacts-holster.svelte.ts
 * - When false: uses Gun-based contacts in users.svelte.ts
 */
export const USE_HOLSTER_CONTACTS =
	import.meta.env.VITE_USE_HOLSTER_CONTACTS === 'true' ||
	(typeof localStorage !== 'undefined' &&
		localStorage.getItem('USE_HOLSTER_CONTACTS') === 'true');

/**
 * Use Holster for capacities management
 * - When true: uses capacities-holster.svelte.ts
 * - When false: uses Gun-based capacities in core.svelte.ts
 */
export const USE_HOLSTER_CAPACITIES =
	import.meta.env.VITE_USE_HOLSTER_CAPACITIES === 'true' ||
	(typeof localStorage !== 'undefined' &&
		localStorage.getItem('USE_HOLSTER_CAPACITIES') === 'true');

/**
 * Use Holster for tree management (not yet implemented)
 */
export const USE_HOLSTER_TREE =
	import.meta.env.VITE_USE_HOLSTER_TREE === 'true' ||
	(typeof localStorage !== 'undefined' && localStorage.getItem('USE_HOLSTER_TREE') === 'true');

/**
 * Use Holster for chat (not yet implemented)
 */
export const USE_HOLSTER_CHAT =
	import.meta.env.VITE_USE_HOLSTER_CHAT === 'true' ||
	(typeof localStorage !== 'undefined' && localStorage.getItem('USE_HOLSTER_CHAT') === 'true');

/**
 * Use Holster for recognition/SOGF data (not yet implemented)
 */
export const USE_HOLSTER_RECOGNITION =
	import.meta.env.VITE_USE_HOLSTER_RECOGNITION === 'true' ||
	(typeof localStorage !== 'undefined' &&
		localStorage.getItem('USE_HOLSTER_RECOGNITION') === 'true');

// Log active flags in development
if (import.meta.env.DEV && typeof window !== 'undefined') {
	console.log('[CONFIG] Holster Migration Flags:', {
		USE_HOLSTER_CONTACTS,
		USE_HOLSTER_CAPACITIES,
		USE_HOLSTER_TREE,
		USE_HOLSTER_CHAT,
		USE_HOLSTER_RECOGNITION
	});

	// Add toggle utilities to window
	(window as any).toggleHolster = {
		contacts: () => {
			const current = localStorage.getItem('USE_HOLSTER_CONTACTS') === 'true';
			localStorage.setItem('USE_HOLSTER_CONTACTS', (!current).toString());
			console.log(`[TOGGLE] Holster contacts: ${!current}`);
			console.log('[TOGGLE] Reload page to apply changes');
		},
		capacities: () => {
			const current = localStorage.getItem('USE_HOLSTER_CAPACITIES') === 'true';
			localStorage.setItem('USE_HOLSTER_CAPACITIES', (!current).toString());
			console.log(`[TOGGLE] Holster capacities: ${!current}`);
			console.log('[TOGGLE] Reload page to apply changes');
		},
		tree: () => {
			const current = localStorage.getItem('USE_HOLSTER_TREE') === 'true';
			localStorage.setItem('USE_HOLSTER_TREE', (!current).toString());
			console.log(`[TOGGLE] Holster tree: ${!current}`);
			console.log('[TOGGLE] Reload page to apply changes');
		},
		chat: () => {
			const current = localStorage.getItem('USE_HOLSTER_CHAT') === 'true';
			localStorage.setItem('USE_HOLSTER_CHAT', (!current).toString());
			console.log(`[TOGGLE] Holster chat: ${!current}`);
			console.log('[TOGGLE] Reload page to apply changes');
		},
		recognition: () => {
			const current = localStorage.getItem('USE_HOLSTER_RECOGNITION') === 'true';
			localStorage.setItem('USE_HOLSTER_RECOGNITION', (!current).toString());
			console.log(`[TOGGLE] Holster recognition: ${!current}`);
			console.log('[TOGGLE] Reload page to apply changes');
		},
		status: () => {
			console.log('[TOGGLE] Current Holster flags:', {
				contacts: localStorage.getItem('USE_HOLSTER_CONTACTS') === 'true',
				capacities: localStorage.getItem('USE_HOLSTER_CAPACITIES') === 'true',
				tree: localStorage.getItem('USE_HOLSTER_TREE') === 'true',
				chat: localStorage.getItem('USE_HOLSTER_CHAT') === 'true',
				recognition: localStorage.getItem('USE_HOLSTER_RECOGNITION') === 'true'
			});
		}
	};

	console.log('[CONFIG] Toggle utilities available: window.toggleHolster');
	console.log('[CONFIG] Example: window.toggleHolster.contacts()');
	console.log('[CONFIG] Check status: window.toggleHolster.status()');
}
