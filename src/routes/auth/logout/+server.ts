import { redirect } from '@sveltejs/kit';
import type { RequestHandler } from './$types';

/**
 * Logout handler - automatically redirects to the home page
 */
export const GET: RequestHandler = async (event) => {
	// Get the auth function from locals
	const authFunction = await event.locals.auth();
	
	// Sign out the user via Auth.js
	await authFunction?.signOut();
	
	// Redirect to home page
	throw redirect(303, '/');
};
