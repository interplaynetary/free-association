import { json } from '@sveltejs/kit';
import type { RequestHandler } from './$types';
import { prisma } from '$lib/server/prisma';

/**
 * GET /api/{nodeId}
 * Get a specific node by ID
 * Returns the node with its immediate children
 */
export const GET: RequestHandler = async ({ params, locals }) => {
	try {
		const { nodeId } = params;

		// Check if node exists
		const node = await prisma.node.findUnique({
			where: { id: nodeId },
			include: {
				children: true,
				capacities: {
					include: {
						shares: true
					}
				}
			}
		});

		if (!node) {
			return json(
				{
					success: false,
					error: 'Node not found'
				},
				{ status: 404 }
			);
		}

		// Get Auth.js session
		const session = await locals.auth();

		// Check if user has access to this node
		if (!session?.user) {
			return json(
				{
					success: false,
					error: 'Authentication required'
				},
				{ status: 401 }
			);
		}

		// For root nodes, check specific ownership
		// For other nodes, we'll be more permissive for now - we'll check ownership hierarchy later
		if (node.type === 'RootNode' && node.user_id !== session.user.id) {
			console.log(
				`Access denied for user ${session.user.id} to root node ${nodeId} owned by ${node.user_id}`
			);
			return json(
				{
					success: false,
					error: 'Access denied'
				},
				{ status: 403 }
			);
		}

		return json({
			success: true,
			data: node
		});
	} catch (error) {
		console.error(`Error fetching node ${params.nodeId}:`, error);

		return json(
			{
				success: false,
				error: 'Failed to fetch node'
			},
			{ status: 500 }
		);
	}
};

/**
 * PUT /api/{nodeId}
 * Update a node's properties
 * Returns the updated node
 */
export const PUT: RequestHandler = async ({ params, request, locals }) => {
	try {
		const { nodeId } = params;

		// Get Auth.js session
		const session = await locals.auth();
		console.log(`PUT request for node ${nodeId} by user ${session?.user?.id}`);

		// User must be authenticated
		if (!session?.user) {
			return json(
				{
					success: false,
					error: 'Authentication required'
				},
				{ status: 401 }
			);
		}

		// Check if node exists
		const node = await prisma.node.findUnique({
			where: { id: nodeId }
		});

		if (!node) {
			console.log(`Node ${nodeId} not found`);
			return json(
				{
					success: false,
					error: 'Node not found'
				},
				{ status: 404 }
			);
		}

		// For now, we'll allow updates to all nodes for the logged-in user
		// In a more sophisticated system, you would verify the node hierarchy
		// and ensure the user owns the root node containing this node

		// For debug - log the ownership info
		console.log(`Node ${nodeId} owned by user ${node.user_id}, session user: ${session.user.id}`);

		// Parse request body
		const body = await request.json();

		// Update the node
		const updatedNode = await prisma.node.update({
			where: { id: nodeId },
			data: {
				// Only update fields that are provided
				...(body.name !== undefined ? { name: body.name } : {}),
				...(body.points !== undefined ? { points: body.points } : {}),
				...(body.manualFulfillment !== undefined
					? { manual_fulfillment: body.manualFulfillment }
					: {})
			}
		});

		// If contributors array is provided
		if (body.contributors !== undefined && Array.isArray(body.contributors)) {
			// First disconnect all contributors
			await prisma.node.update({
				where: { id: nodeId },
				data: {
					contributors: {
						disconnect: await prisma.node.findMany({
							where: { contributed_to: { some: { id: nodeId } } },
							select: { id: true }
						})
					}
				}
			});

			// Then connect new contributors
			if (body.contributors.length > 0) {
				await prisma.node.update({
					where: { id: nodeId },
					data: {
						contributors: {
							connect: body.contributors.map((id: string) => ({ id }))
						}
					}
				});
			}
		}

		return json({
			success: true,
			data: updatedNode
		});
	} catch (error) {
		console.error(`Error updating node ${params.nodeId}:`, error);

		return json(
			{
				success: false,
				error: 'Failed to update node'
			},
			{ status: 500 }
		);
	}
};

/**
 * DELETE /api/{nodeId}
 * Delete a node and its children
 */
export const DELETE: RequestHandler = async ({ params, locals }) => {
	try {
		const { nodeId } = params;

		// Get Auth.js session
		const session = await locals.auth();
		console.log(`DELETE request for node ${nodeId} by user ${session?.user?.id}`);

		// User must be authenticated
		if (!session?.user) {
			return json(
				{
					success: false,
					error: 'Authentication required'
				},
				{ status: 401 }
			);
		}

		// Check if node exists
		const node = await prisma.node.findUnique({
			where: { id: nodeId }
		});

		if (!node) {
			console.log(`Node ${nodeId} not found`);
			return json(
				{
					success: false,
					error: 'Node not found'
				},
				{ status: 404 }
			);
		}

		// Don't allow deleting root nodes
		if (node.type === 'RootNode') {
			console.log(`Attempted to delete root node ${nodeId}`);
			return json(
				{
					success: false,
					error: 'Cannot delete root node'
				},
				{ status: 400 }
			);
		}

		// For debug - log the ownership info
		console.log(`Node ${nodeId} owned by user ${node.user_id}, session user: ${session.user.id}`);

		// For now, we'll allow deletes for all nodes for the logged-in user
		// In a more sophisticated system, you would verify the node hierarchy
		// and ensure the user owns the root node containing this node

		// Delete all child nodes recursively
		await deleteNodeAndChildren(nodeId);

		return json({
			success: true,
			message: 'Node deleted successfully'
		});
	} catch (error) {
		console.error(`Error deleting node ${params.nodeId}:`, error);

		return json(
			{
				success: false,
				error: 'Failed to delete node'
			},
			{ status: 500 }
		);
	}
};

/**
 * Helper function to recursively delete a node and all its children
 */
async function deleteNodeAndChildren(nodeId: string) {
	// Get all child nodes
	const childNodes = await prisma.node.findMany({
		where: { parent_id: nodeId },
		select: { id: true }
	});

	// Recursively delete children
	for (const child of childNodes) {
		await deleteNodeAndChildren(child.id);
	}

	// Delete the node itself
	await prisma.node.delete({
		where: { id: nodeId }
	});
}
