import { error } from '@sveltejs/kit';
import type { PageServerLoad } from './$types';
import { prisma } from '$lib/server/prisma';

export const load: PageServerLoad = async ({ params, locals }) => {
	const { nodeId } = params;

	// Get Auth.js session
	const session = await locals.auth();

	// Verify the node exists
	try {
		const node = await prisma.node.findUnique({
			where: { id: nodeId }
		});

		if (!node) {
			throw error(404, 'Node not found');
		}

		// Make sure user has access to this node
		if (session?.user?.id !== node.user_id) {
			throw error(403, 'Access denied');
		}

		// Return minimal data to confirm node exists
		// Full data will be loaded by the client
		return {
			nodeId: node.id,
			nodeName: node.name
		};
	} catch (e) {
		// If there's an error with Prisma, return a 500
		console.error('Error loading node:', e);
		throw error(500, 'Error loading node data');
	}
};
 