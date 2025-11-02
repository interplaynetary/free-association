/**
 * API Endpoint: Manually Trigger Membership Computation
 * 
 * POST /api/collective/trigger-membership
 * 
 * Manually triggers a membership computation cycle, bypassing the schedule.
 * Useful for testing and immediate updates.
 * 
 * Authentication recommended in production!
 */

import { json } from '@sveltejs/kit';
import { triggerMembershipComputation } from '$lib/server/collective';
import type { RequestHandler } from './$types';

export const POST: RequestHandler = async ({ request }) => {
	try {
		// TODO: Add authentication check here
		// const session = await getSession(request);
		// if (!session || !session.user.isAdmin) {
		//   return json({ success: false, error: 'Unauthorized' }, { status: 401 });
		// }
		
		console.log('[API] Manual membership computation triggered');
		
		await triggerMembershipComputation();
		
		return json({
			success: true,
			message: 'Membership computation completed'
		});
	} catch (error) {
		console.error('[API] Manual membership computation failed:', error);
		return json(
			{
				success: false,
				error: 'Membership computation failed',
				details: error instanceof Error ? error.message : String(error)
			},
			{ status: 500 }
		);
	}
};

