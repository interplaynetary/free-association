/**
 * API Endpoint: Manually Trigger Collective Capacity Allocation
 * 
 * POST /api/collective-tree/trigger-allocation
 */

import { json } from '@sveltejs/kit';
import { triggerCapacityAllocation } from '$lib/server/collective-tree';
import type { RequestHandler } from './$types';

export const POST: RequestHandler = async () => {
	try {
		console.log('[API] Manual capacity allocation triggered');
		
		await triggerCapacityAllocation();
		
		return json({
			success: true,
			message: 'Capacity allocation computation completed'
		});
	} catch (error) {
		console.error('[API] Manual capacity allocation failed:', error);
		return json(
			{
				success: false,
				error: 'Capacity allocation failed',
				details: error instanceof Error ? error.message : String(error)
			},
			{ status: 500 }
		);
	}
};

