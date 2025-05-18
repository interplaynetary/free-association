import { json } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { prisma } from '$lib/server/prisma';

/**
 * GET /api/forest
 * Get all roots (returns all root nodeIds)
 */
export const GET: RequestHandler = async () => {
	try {
		// Get all root nodes
		const rootNodes = await prisma.node.findMany({
			where: {
				type: 'RootNode'
			},
			select: {
				id: true
			}
		});

		// Return just the IDs for the forest endpoint
		const rootIds = rootNodes.map((node: { id: string }) => node.id);

		return json({
			success: true,
			data: rootIds
		});
	} catch (error) {
		console.error('Error fetching forest:', error);

		return json(
			{
				success: false,
				error: 'Failed to fetch forest'
			},
			{ status: 500 }
		);
	}
};
