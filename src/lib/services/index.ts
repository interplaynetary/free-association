/**
 * Services Index - Auto-initialize all global services
 *
 * Import this file to automatically initialize all global services.
 * Services are designed as singletons and will only initialize once.
 */

import { viewportService } from './viewport.svelte';
import { navigationService } from './navigation.svelte';
import { initializeGeocodingCache } from '$lib/utils/location/geocodingCache';

// Services are auto-initialized when imported
console.log('[SERVICES] Global services initialized');
console.log('[TRACE] src/lib/services/index.ts: <module scope>');

// Initialize geocoding cache system
initializeGeocodingCache();

// Export services for direct access if needed
export { viewportService, navigationService };
