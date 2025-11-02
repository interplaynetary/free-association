/**
 * API Endpoint: Get Collective Tree Scheduler Status
 * 
 * GET /api/collective-tree/status
 */

import { json } from '@sveltejs/kit';
import { getCollectiveTreeSchedulerStatus } from '$lib/server/collective-tree';
import type { RequestHandler } from './$types';

export const GET: RequestHandler = async () => {
	try {
		const status = getCollectiveTreeSchedulerStatus();
		
		return json({
			success: true,
			status
		});
	} catch (error) {
		console.error('[API] Failed to get collective tree scheduler status:', error);
		return json(
			{
				success: false,
				error: 'Failed to get scheduler status'
			},
			{ status: 500 }
		);
	}
};

