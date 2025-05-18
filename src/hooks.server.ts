import { handle as authHandle, register } from './auth';
import type { Handle } from '@sveltejs/kit';
import { sequence } from '@sveltejs/kit/hooks';

/**
 * Custom handle function for API requests and other server-side operations
 */
const apiHandle: Handle = async ({ event, resolve }) => {
	// Special handling for API routes to ensure proper content type
	if (event.url.pathname.startsWith('/api/')) {
		// Set content type to application/json for all API responses
		const response = await resolve(event, {
			transformPageChunk: ({ html }) => html
		});

		// Return response with content-type header
		if (!response.headers.has('content-type')) {
			response.headers.set('content-type', 'application/json');
		}

		return response;
	}

	// For non-API routes, continue with standard processing
	return resolve(event);
};

/**
 * Combine multiple handlers using sequence
 * This ensures they run in the correct order
 */
export const handle: Handle = sequence(authHandle, register, apiHandle);
