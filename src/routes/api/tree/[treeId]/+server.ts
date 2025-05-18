import { json } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { prisma } from '$lib/server/prisma';
import { error } from '@sveltejs/kit';

/**
 * GET /api/tree/{treeId}
 * Get an entire tree by ID
 * Returns the complete tree structure with all nodes
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

		// Get Auth.js session
		const session = await locals.auth();

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

		// Check if tree exists and if it's a root node
		const rootNode = await prisma.node.findUnique({
			where: {
				id: treeId,
				type: 'RootNode'
			}
		});

		if (!rootNode) {
			return json(
				{
					success: false,
					error: 'Tree not found'
				},
				{ status: 404 }
			);
		}

		// Check if user owns the tree
		if (rootNode.user_id !== session.user.id) {
			return json(
				{
					success: false,
					error: 'Access denied'
				},
				{ status: 403 }
			);
		}

		// Fetch the entire tree structure recursively
		const treeWithChildren = await getTreeWithChildren(treeId);

		return json({
			success: true,
			data: treeWithChildren
		});
	} catch (err) {
		console.error(`Error fetching tree ${params.treeId}:`, err);

		return json(
			{
				success: false,
				error: 'Failed to fetch tree'
			},
			{ status: 500 }
		);
	}
};

/**
 * PUT /api/tree/{treeId}
 * Update an entire tree
 * Expects the complete tree structure in the request body
 */
export const PUT: RequestHandler = async ({ params, request, locals }) => {
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

		// Get Auth.js session
		const session = await locals.auth();

		// User must be authenticated
		if (!session?.user || !session.user.id) {
			return json(
				{
					success: false,
					error: 'Authentication required'
				},
				{ status: 401 }
			);
		}

		// Check if tree exists
		const rootNode = await prisma.node.findUnique({
			where: {
				id: treeId,
				type: 'RootNode'
			}
		});

		if (!rootNode) {
			return json(
				{
					success: false,
					error: 'Tree not found'
				},
				{ status: 404 }
			);
		}

		// Check if user owns the tree
		if (rootNode.user_id !== session.user.id) {
			return json(
				{
					success: false,
					error: 'Access denied'
				},
				{ status: 403 }
			);
		}

		// Parse the request body
		const treeData = await request.json();

		// Validate that the tree ID in the body matches the URL parameter
		if (treeData.id !== treeId) {
			return json(
				{
					success: false,
					error: 'Tree ID in body does not match URL parameter'
				},
				{ status: 400 }
			);
		}

		// Validate that it's a root node
		if (treeData.type !== 'RootNode') {
			return json(
				{
					success: false,
					error: 'Provided data is not a root node (tree)'
				},
				{ status: 400 }
			);
		}

		// Save the tree data using a transaction
		await saveTreeData(treeData, session.user.id);

		return json({
			success: true,
			message: 'Tree updated successfully'
		});
	} catch (err) {
		console.error(`Error updating tree ${params.treeId}:`, err);

		return json(
			{
				success: false,
				error: 'Failed to update tree'
			},
			{ status: 500 }
		);
	}
};

/**
 * Helper function to recursively get a tree with all its children
 */
async function getTreeWithChildren(nodeId: string) {
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
		return null;
	}

	interface NodeWithId {
		id: string;
	}

	// Recursively get children
	const childrenWithGrandchildren = await Promise.all(
		node.children.map((child: NodeWithId) => getTreeWithChildren(child.id))
	);

	// Filter out null children (should never happen if the DB is consistent)
	const validChildren = childrenWithGrandchildren.filter((child) => child !== null);

	// Add contributors for each node
	const nodeWithContributors = await prisma.node.findUnique({
		where: { id: node.id },
		include: { contributors: true }
	});

	return {
		...node,
		children: validChildren,
		contributors: nodeWithContributors?.contributors || []
	};
}

/**
 * Helper function to save a tree and all its children recursively
 */
async function saveTreeData(treeData: any, userId: string) {
	// Start a transaction to ensure all updates are atomic
	return prisma.$transaction(async (prismaTransaction: typeof prisma) => {
		// Update the root node
		await prismaTransaction.node.update({
			where: { id: treeData.id },
			data: {
				name: treeData.name,
				manual_fulfillment: treeData.manualFulfillment,
				user_id: userId
			}
		});

		// Process children recursively
		if (treeData.children && Object.keys(treeData.children).length > 0) {
			await Promise.all(
				Object.entries(treeData.children).map(async ([childId, childData]: [string, any]) => {
					// Check if child already exists
					const existingChild = await prismaTransaction.node.findUnique({
						where: { id: childId }
					});

					if (existingChild) {
						// Update existing child
						await updateNodeRecursively(prismaTransaction, childId, childData, treeData.id);
					} else {
						// Create new child
						await createNodeRecursively(prismaTransaction, childId, childData, treeData.id);
					}
				})
			);
		}

		// Process capacities if present
		if (treeData.capacities && Object.keys(treeData.capacities).length > 0) {
			await Promise.all(
				Object.entries(treeData.capacities).map(
					async ([capacityId, capacityData]: [string, any]) => {
						// Check if capacity already exists
						const existingCapacity = await prismaTransaction.capacity.findUnique({
							where: { id: capacityId }
						});

						if (existingCapacity) {
							// Update existing capacity
							await updateCapacity(prismaTransaction, capacityId, capacityData as any);
						} else {
							// Create new capacity
							await createCapacity(prismaTransaction, capacityId, capacityData as any, treeData.id);
						}
					}
				)
			);
		}
	});
}

/**
 * Helper function to update a node and its children recursively
 */
async function updateNodeRecursively(
	tx: typeof prisma,
	nodeId: string,
	nodeData: any,
	parentId: string
) {
	// Update the node
	await tx.node.update({
		where: { id: nodeId },
		data: {
			name: nodeData.name,
			type: nodeData.type,
			points: nodeData.points,
			manual_fulfillment: nodeData.manualFulfillment,
			parent: {
				connect: { id: parentId }
			}
		}
	});

	// Update contributors if present
	if (nodeData.contributors && nodeData.contributors.size > 0) {
		// Disconnect all existing contributors
		await tx.node.update({
			where: { id: nodeId },
			data: {
				contributors: {
					disconnect: await tx.node.findMany({
						where: { contributed_to: { some: { id: nodeId } } },
						select: { id: true }
					})
				}
			}
		});

		// Connect new contributors
		const contributors = Array.from<string>(nodeData.contributors as Set<string>);
		await tx.node.update({
			where: { id: nodeId },
			data: {
				contributors: {
					connect: contributors.map((contributorId: string) => ({
						id: contributorId
					}))
				}
			}
		});
	}

	// Process children recursively
	if (nodeData.children && Object.keys(nodeData.children).length > 0) {
		await Promise.all(
			Object.entries(nodeData.children).map(async ([childId, childData]: [string, any]) => {
				// Check if child already exists
				const existingChild = await tx.node.findUnique({
					where: { id: childId }
				});

				if (existingChild) {
					// Update existing child
					await updateNodeRecursively(tx, childId, childData, nodeId);
				} else {
					// Create new child
					await createNodeRecursively(tx, childId, childData, nodeId);
				}
			})
		);
	}
}

/**
 * Helper function to create a node and its children recursively
 */
async function createNodeRecursively(
	tx: typeof prisma,
	nodeId: string,
	nodeData: any,
	parentId: string
) {
	// Create the node
	await tx.node.create({
		data: {
			id: nodeId,
			name: nodeData.name,
			type: nodeData.type,
			points: nodeData.points,
			manual_fulfillment: nodeData.manualFulfillment,
			parent: {
				connect: { id: parentId }
			}
		}
	});

	// Add contributors if present
	if (nodeData.contributors && nodeData.contributors.size > 0) {
		const contributors = Array.from<string>(nodeData.contributors as Set<string>);
		await tx.node.update({
			where: { id: nodeId },
			data: {
				contributors: {
					connect: contributors.map((contributorId: string) => ({
						id: contributorId
					}))
				}
			}
		});
	}

	// Process children recursively
	if (nodeData.children && Object.keys(nodeData.children).length > 0) {
		await Promise.all(
			Object.entries(nodeData.children).map(async ([childId, childData]: [string, any]) => {
				await createNodeRecursively(tx, childId, childData, nodeId);
			})
		);
	}
}

/**
 * Helper function to update a capacity
 */
async function updateCapacity(tx: typeof prisma, capacityId: string, capacityData: any) {
	await tx.capacity.update({
		where: { id: capacityId },
		data: {
			name: capacityData.capacityName,
			quantity: capacityData.quantity,
			unit: capacityData.unit,
			share_depth: capacityData.shareDepth,
			expanded: capacityData.expanded,
			location_type: capacityData.coordinates.locationType,
			all_day: capacityData.coordinates.allDay,
			recurrence: capacityData.coordinates.recurrence,
			custom_recurrence_repeat_every: capacityData.coordinates.customRecurrence?.repeatEvery,
			custom_recurrence_repeat_unit: capacityData.coordinates.customRecurrence?.repeatUnit,
			custom_recurrence_end_type: capacityData.coordinates.customRecurrence?.recurrenceEnd?.type,
			custom_recurrence_end_value: getRecurrenceEndValue(
				capacityData.coordinates.customRecurrence?.recurrenceEnd
			),
			start_date: capacityData.coordinates.startDate
				? new Date(capacityData.coordinates.startDate).toISOString().split('T')[0]
				: null,
			start_time: capacityData.coordinates.startTime
				? new Date(capacityData.coordinates.startTime).toISOString().split('T')[1]
				: null,
			end_date: capacityData.coordinates.endDate
				? new Date(capacityData.coordinates.endDate).toISOString().split('T')[0]
				: null,
			end_time: capacityData.coordinates.endTime
				? new Date(capacityData.coordinates.endTime).toISOString().split('T')[1]
				: null,
			time_zone: capacityData.coordinates.timeZone,
			max_natural_div: capacityData.maxDivisibility.naturalDiv,
			max_percentage_div: capacityData.maxDivisibility.percentageDiv,
			hidden_until_request_accepted: capacityData.hiddenUntilRequestAccepted
		}
	});

	// Update capacity shares if present
	if (capacityData.shares && Object.keys(capacityData.shares).length > 0) {
		// Process each share
		await Promise.all(
			Object.entries(capacityData.shares).map(async ([shareId, shareData]: [string, any]) => {
				// Check if share already exists
				const existingShare = await tx.capacityShare.findUnique({
					where: { id: shareId }
				});

				if (existingShare) {
					// Update existing share
					await tx.capacityShare.update({
						where: { id: shareId },
						data: {
							share_percentage: shareData.sharePercentage,
							computed_quantity: shareData.computedQuantity,
							recipient: {
								connect: { id: shareData.targetCapacity.recipient_id }
							}
						}
					});
				} else {
					// Create new share
					await tx.capacityShare.create({
						data: {
							id: shareId,
							share_percentage: shareData.sharePercentage,
							computed_quantity: shareData.computedQuantity,
							capacity: {
								connect: { id: capacityId }
							},
							recipient: {
								connect: { id: shareData.targetCapacity.recipient_id }
							}
						}
					});
				}
			})
		);
	}
}

/**
 * Helper function to create a capacity
 */
async function createCapacity(
	tx: typeof prisma,
	capacityId: string,
	capacityData: any,
	ownerId: string
) {
	await tx.capacity.create({
		data: {
			id: capacityId,
			name: capacityData.capacityName,
			quantity: capacityData.quantity,
			unit: capacityData.unit,
			share_depth: capacityData.shareDepth,
			expanded: capacityData.expanded,
			location_type: capacityData.coordinates.locationType,
			all_day: capacityData.coordinates.allDay,
			recurrence: capacityData.coordinates.recurrence,
			custom_recurrence_repeat_every: capacityData.coordinates.customRecurrence?.repeatEvery,
			custom_recurrence_repeat_unit: capacityData.coordinates.customRecurrence?.repeatUnit,
			custom_recurrence_end_type: capacityData.coordinates.customRecurrence?.recurrenceEnd?.type,
			custom_recurrence_end_value: getRecurrenceEndValue(
				capacityData.coordinates.customRecurrence?.recurrenceEnd
			),
			start_date: capacityData.coordinates.startDate
				? new Date(capacityData.coordinates.startDate).toISOString().split('T')[0]
				: null,
			start_time: capacityData.coordinates.startTime
				? new Date(capacityData.coordinates.startTime).toISOString().split('T')[1]
				: null,
			end_date: capacityData.coordinates.endDate
				? new Date(capacityData.coordinates.endDate).toISOString().split('T')[0]
				: null,
			end_time: capacityData.coordinates.endTime
				? new Date(capacityData.coordinates.endTime).toISOString().split('T')[1]
				: null,
			time_zone: capacityData.coordinates.timeZone,
			max_natural_div: capacityData.maxDivisibility.naturalDiv,
			max_percentage_div: capacityData.maxDivisibility.percentageDiv,
			hidden_until_request_accepted: capacityData.hiddenUntilRequestAccepted,
			owner: {
				connect: { id: ownerId }
			}
		}
	});

	// Create capacity shares if present
	if (capacityData.shares && Object.keys(capacityData.shares).length > 0) {
		// Process each share
		await Promise.all(
			Object.entries(capacityData.shares).map(async ([shareId, shareData]: [string, any]) => {
				await tx.capacityShare.create({
					data: {
						id: shareId,
						share_percentage: shareData.sharePercentage,
						computed_quantity: shareData.computedQuantity,
						capacity: {
							connect: { id: capacityId }
						},
						recipient: {
							connect: { id: shareData.targetCapacity.recipient_id }
						}
					}
				});
			})
		);
	}
}

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
