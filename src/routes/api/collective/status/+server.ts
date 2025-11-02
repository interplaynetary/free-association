/**
 * API Endpoint: Get Collective Recognition Scheduler Status
 * 
 * GET /api/collective/status
 * 
 * Returns the current status of the scheduler including:
 * - Running state
 * - Last run times
 * - Run counts
 * - Configuration
 */

import { json } from '@sveltejs/kit';
import { getSchedulerStatus } from '$lib/server/collective';
import type { RequestHandler } from './$types';

export const GET: RequestHandler = async () => {
	try {
		const status = getSchedulerStatus();
		
		return json({
			success: true,
			status
		});
	} catch (error) {
		console.error('[API] Failed to get scheduler status:', error);
		return json(
			{
				success: false,
				error: 'Failed to get scheduler status'
			},
			{ status: 500 }
		);
	}
};

