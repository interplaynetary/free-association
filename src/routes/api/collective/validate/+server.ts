/**
 * API Endpoint: Validate Collective Recognition Callbacks
 * 
 * GET /api/collective/validate
 * 
 * Tests the callbacks to verify they can fetch data correctly.
 * Useful for debugging setup issues.
 */

import { json } from '@sveltejs/kit';
import { validateCallbacks } from '$lib/server/collective/callbacks';
import type { RequestHandler } from './$types';

export const GET: RequestHandler = async () => {
	try {
		console.log('[API] Running callbacks validation...');
		
		const validation = await validateCallbacks();
		
		return json({
			success: validation.success,
			results: validation.results,
			errors: validation.errors,
			message: validation.success 
				? 'All callbacks validated successfully' 
				: `Validation completed with ${validation.errors.length} errors`
		});
	} catch (error) {
		console.error('[API] Callbacks validation failed:', error);
		return json(
			{
				success: false,
				error: 'Callbacks validation failed',
				details: error instanceof Error ? error.message : String(error)
			},
			{ status: 500 }
		);
	}
};

