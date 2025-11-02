/**
 * API Endpoint: Validate Collective Tree Callbacks
 * 
 * GET /api/collective-tree/validate
 */

import { json } from '@sveltejs/kit';
import { validateCollectiveTreeCallbacks } from '$lib/server/collective-tree';
import type { RequestHandler } from './$types';

export const GET: RequestHandler = async () => {
	try {
		console.log('[API] Running collective tree callbacks validation...');
		
		const validation = await validateCollectiveTreeCallbacks();
		
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

