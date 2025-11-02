/**
 * API Endpoint: Manually Trigger Collective Recognition
 * 
 * POST /api/collective-tree/trigger-recognition
 */

import { json } from '@sveltejs/kit';
import { triggerCollectiveRecognition } from '$lib/server/collective-tree';
import type { RequestHandler } from './$types';

export const POST: RequestHandler = async () => {
	try {
		console.log('[API] Manual collective recognition triggered');
		
		await triggerCollectiveRecognition();
		
		return json({
			success: true,
			message: 'Collective recognition computation completed'
		});
	} catch (error) {
		console.error('[API] Manual collective recognition failed:', error);
		return json(
			{
				success: false,
				error: 'Collective recognition failed',
				details: error instanceof Error ? error.message : String(error)
			},
			{ status: 500 }
		);
	}
};

