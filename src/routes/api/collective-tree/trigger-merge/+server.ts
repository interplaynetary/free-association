/**
 * API Endpoint: Manually Trigger Tree Merge
 * 
 * POST /api/collective-tree/trigger-merge
 */

import { json } from '@sveltejs/kit';
import { triggerTreeMerge } from '$lib/server/collective-tree';
import type { RequestHandler } from './$types';

export const POST: RequestHandler = async () => {
	try {
		console.log('[API] Manual tree merge triggered');
		
		await triggerTreeMerge();
		
		return json({
			success: true,
			message: 'Tree merge computation completed'
		});
	} catch (error) {
		console.error('[API] Manual tree merge failed:', error);
		return json(
			{
				success: false,
				error: 'Tree merge failed',
				details: error instanceof Error ? error.message : String(error)
			},
			{ status: 500 }
		);
	}
};

