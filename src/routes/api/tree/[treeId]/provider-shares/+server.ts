import { json } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { prisma } from '$lib/server/prisma';

/**
 * GET /api/tree/{treeId}/provider-shares
 * Get provider-centric shares for a node
 * Parameters:
 *  - depth: number
 * Returns: Map<string, number> (normalized shares)
 */
export const GET: RequestHandler = async ({ params, url, locals }) => {
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
				type: true,
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

		// Get depth parameter from query string
		const depth = parseInt(url.searchParams.get('depth') || '1', 10);
		if (isNaN(depth) || depth < 1) {
			return json(
				{
					success: false,
					error: 'Invalid depth parameter, must be a positive integer'
				},
				{ status: 400 }
			);
		}

		// Get the cached provider shares using Prisma
		const cachedShares = await prisma.shareCache.findMany({
			where: {
				provider_id: treeId,
				share_type: 'ProviderShare',
				depth: depth
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
		console.error(`Error fetching provider shares for tree ${params.treeId}:`, error);

		return json(
			{
				success: false,
				error: 'Failed to fetch provider shares'
			},
			{ status: 500 }
		);
	}
};
