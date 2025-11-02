/**
 * API Endpoint: Manually Trigger Allocation Computation
 * 
 * POST /api/collective/trigger-allocation
 * 
 * Manually triggers an allocation computation cycle, bypassing the schedule.
 * Useful for testing and immediate updates.
 * 
 * Authentication recommended in production!
 */

import { json } from '@sveltejs/kit';
import { triggerAllocationComputation } from '$lib/server/collective';
import type { RequestHandler } from './$types';

export const POST: RequestHandler = async ({ request }) => {
	try {
		// TODO: Add authentication check here
		// const session = await getSession(request);
		// if (!session || !session.user.isAdmin) {
		//   return json({ success: false, error: 'Unauthorized' }, { status: 401 });
		// }
		
		console.log('[API] Manual allocation computation triggered');
		
		await triggerAllocationComputation();
		
		return json({
			success: true,
			message: 'Allocation computation completed'
		});
	} catch (error) {
		console.error('[API] Manual allocation computation failed:', error);
		return json(
			{
				success: false,
				error: 'Allocation computation failed',
				details: error instanceof Error ? error.message : String(error)
			},
			{ status: 500 }
		);
	}
};

