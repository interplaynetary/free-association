import { json } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { prisma } from '$lib/server/prisma';
import { nanoid } from 'nanoid';

/**
 * POST /api/tree/{treeId}/capacities
 * Add a new capacity to a node
 */
export const POST: RequestHandler = async ({ params, request, locals }) => {
	try {
		const { treeId } = params;

		if (!treeId) {
			return json(
				{
					success: false,
					error: 'Tree ID is required'
				},
				{ status: 400 }
			);
		}

		// User must be authenticated
		if (!locals.user) {
			return json(
				{
					success: false,
					error: 'Authentication required'
				},
				{ status: 401 }
			);
		}

		// Check if tree/node exists and if it's a root node
		const treeNode = await prisma.node.findUnique({
			where: {
				id: treeId,
				type: 'RootNode'
			},
			select: {
				id: true,
				user_id: true
			}
		});

		if (!treeNode) {
			return json(
				{
					success: false,
					error: 'Tree not found'
				},
				{ status: 404 }
			);
		}

		// Check if user owns the tree
		if (treeNode.user_id !== locals.user.id) {
			return json(
				{
					success: false,
					error: 'Access denied'
				},
				{ status: 403 }
			);
		}

		// Parse request body
		const body = await request.json();

		// Validate required fields for capacity
		const requiredFields = ['capacityName', 'quantity', 'unit', 'shareDepth'];
		const missingFields = requiredFields.filter((field) => body[field] === undefined);

		if (missingFields.length > 0) {
			return json(
				{
					success: false,
					error: `Missing required fields: ${missingFields.join(', ')}`
				},
				{ status: 400 }
			);
		}

		// Generate a unique ID for the capacity if not provided
		const capacityId = body.capacityId || nanoid();

		// Create the capacity using Prisma
		const capacity = await prisma.capacity.create({
			data: {
				id: capacityId,
				name: body.capacityName,
				quantity: body.quantity,
				unit: body.unit,
				share_depth: body.shareDepth,
				expanded: body.expanded || false,
				location_type: body.coordinates?.locationType || 'Undefined',
				all_day: body.coordinates?.allDay || false,
				recurrence: body.coordinates?.recurrence,
				custom_recurrence_repeat_every: body.coordinates?.customRecurrence?.repeatEvery,
				custom_recurrence_repeat_unit: body.coordinates?.customRecurrence?.repeatUnit,
				custom_recurrence_end_type: body.coordinates?.customRecurrence?.recurrenceEnd?.type,
				custom_recurrence_end_value: getRecurrenceEndValue(
					body.coordinates?.customRecurrence?.recurrenceEnd
				),
				start_date: body.coordinates?.startDate
					? new Date(body.coordinates.startDate).toISOString().split('T')[0]
					: null,
				start_time: body.coordinates?.startTime
					? new Date(`1970-01-01T${body.coordinates.startTime}Z`).toISOString().split('T')[1]
					: null,
				end_date: body.coordinates?.endDate
					? new Date(body.coordinates.endDate).toISOString().split('T')[0]
					: null,
				end_time: body.coordinates?.endTime
					? new Date(`1970-01-01T${body.coordinates.endTime}Z`).toISOString().split('T')[1]
					: null,
				time_zone: body.coordinates?.timeZone || 'UTC',
				max_natural_div: body.maxDivisibility?.naturalDiv || 1,
				max_percentage_div: body.maxDivisibility?.percentageDiv || 1.0,
				hidden_until_request_accepted: body.hiddenUntilRequestAccepted || false,
				owner: {
					connect: { id: treeId }
				}
			}
		});

		return json(
			{
				success: true,
				data: {
					id: capacityId,
					name: body.capacityName,
					quantity: body.quantity,
					unit: body.unit,
					treeId
				}
			},
			{ status: 201 }
		);
	} catch (error) {
		console.error(`Error adding capacity to tree ${params.treeId}:`, error);

		return json(
			{
				success: false,
				error: 'Failed to add capacity'
			},
			{ status: 500 }
		);
	}
};

/**
 * Helper function to get the recurrence end value based on the type
 */
function getRecurrenceEndValue(recurrenceEnd: any): string | null {
	if (!recurrenceEnd) return null;

	if (recurrenceEnd.type === 'EndsOn' && recurrenceEnd.date) {
		return new Date(recurrenceEnd.date).toISOString().split('T')[0];
	} else if (recurrenceEnd.type === 'EndsAfter' && recurrenceEnd.count) {
		return recurrenceEnd.count.toString();
	}

	return null;
}
