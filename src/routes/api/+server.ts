import { json } from '@sveltejs/kit';
import type { RequestHandler } from './$types';
import { prisma } from '$lib/server/prisma';

/**
 * POST /api
 * Create a new root node
 */
export const POST: RequestHandler = async ({ request, locals }) => {
	try {
		// Get Auth.js session
		const session = await locals.auth();

		// User must be authenticated to create nodes
		if (!session?.user) {
			return json(
				{
					success: false,
					error: 'Authentication required'
				},
				{ status: 401 }
			);
		}

		const body = await request.json();

		// Validate required fields
		if (!body.id || !body.name) {
			return json(
				{
					success: false,
					error: 'Missing required fields: id, name'
				},
				{ status: 400 }
			);
		}

		// Create a new root node using Prisma
		const rootNode = await prisma.node.create({
			data: {
				id: body.id,
				name: body.name,
				type: 'RootNode',
				manual_fulfillment: body.manualFulfillment,
				user_id: session.user.id
			}
		});

		return json(
			{
				success: true,
				data: rootNode
			},
			{ status: 201 }
		);
	} catch (error) {
		console.error('Error creating root node:', error);

		// Handle unique constraint errors
		if (error instanceof Error && error.message.includes('Unique constraint')) {
			return json(
				{
					success: false,
					error: 'A node with this ID already exists'
				},
				{ status: 400 }
			);
		}

		return json(
			{
				success: false,
				error: 'Failed to create root node'
			},
			{ status: 500 }
		);
	}
};
