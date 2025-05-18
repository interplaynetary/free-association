import { json } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { prisma } from '$lib/server/prisma';

/**
 * GET /api/tree/{treeId}/shares-of-general-fulfillment
 * Get SOGF Map for a tree
 * Returns Map<string, number> (normalized shares)
 */
export const GET: RequestHandler = async ({ params, locals }) => {
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

		// Get the cached SOGF shares using Prisma
		const cachedShares = await prisma.shareCache.findMany({
			where: {
				provider_id: treeId,
				share_type: 'SOGF',
				depth: 0 // Default depth for SOGF is 0
			},
			select: {
				provider_id: true,
				recipient_id: true,
				share_value: true
			}
		});

		// Convert to map format for the response
		const sharesMap: Record<string, number> = {};
		for (const share of cachedShares) {
			sharesMap[share.recipient_id] = share.share_value;
		}

		return json({
			success: true,
			data: sharesMap
		});
	} catch (error) {
		console.error(`Error fetching SOGF for tree ${params.treeId}:`, error);

		return json(
			{
				success: false,
				error: 'Failed to fetch shares of general fulfillment'
			},
			{ status: 500 }
		);
	}
};
