import { json } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { prisma } from '$lib/server/prisma';

/**
 * DELETE /api/tree/{treeId}/capacities/{capacityId}
 * Delete a capacity from a tree
 */
export const DELETE: RequestHandler = async ({ params, locals }) => {
	try {
		const { treeId, capacityId } = params;

		if (!treeId || !capacityId) {
			return json(
				{
					success: false,
					error: 'Tree ID and Capacity ID are required'
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

		// Get the tree node to check if it exists and is a root node
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

		// Check if capacity exists and belongs to this tree
		const capacity = await prisma.capacity.findUnique({
			where: {
				id: capacityId,
				owner_id: treeId
			}
		});

		if (!capacity) {
			return json(
				{
					success: false,
					error: 'Capacity not found'
				},
				{ status: 404 }
			);
		}

		// Delete the capacity
		await prisma.capacity.delete({
			where: { id: capacityId }
		});

		return json({
			success: true,
			message: 'Capacity deleted successfully'
		});
	} catch (error) {
		console.error(
			`Error deleting capacity ${params.capacityId} from tree ${params.treeId}:`,
			error
		);

		return json(
			{
				success: false,
				error: 'Failed to delete capacity'
			},
			{ status: 500 }
		);
	}
};
