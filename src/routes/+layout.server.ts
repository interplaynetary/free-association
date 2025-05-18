import type { LayoutServerLoad } from './$types';

// Set this to false for Auth.js to work properly
export const ssr = true;

/**
 * Server load function to provide session data to all routes
 */
export const load: LayoutServerLoad = async (event) => {
	// Get session from Auth.js
	return {
		session: await event.locals.auth()
	};
};
