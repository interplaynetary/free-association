import type { Capacity, CapacityShare, Forest, TreeZipper, RootNode, NonRootNode } from './types';
import { receiverShareFrom } from './calculations';

/**
 * Compute the quantity of a capacity share based on percentage and capacity constraints
 * @param cap Capacity object
 * @param percentage Share percentage (0-1)
 * @returns Computed quantity respecting divisibility constraints
 */
export function computeQuantityShare(cap: Capacity, percentage: number): number {
	const rawQuantity = Math.round(cap.quantity * percentage);
	const maxNatural = cap.maxDivisibility.naturalDiv;
	const maxPercent = cap.maxDivisibility.percentageDiv;

	// Apply percentage divisibility constraint
	const percentConstrained =
		percentage > maxPercent ? Math.round(cap.quantity * maxPercent) : rawQuantity;

	// Apply natural number divisibility constraint
	const naturalConstrained = Math.floor(percentConstrained / maxNatural) * maxNatural;

	return naturalConstrained;
}

/**
 * Create a new capacity share
 * @param cap Capacity to share
 * @param percentage Share percentage (0-1)
 * @returns Capacity share object
 */
export function createCapacityShare(cap: Capacity, percentage: number): CapacityShare {
	return {
		targetCapacity: cap,
		sharePercentage: percentage,
		computedQuantity: computeQuantityShare(cap, percentage)
	};
}

/**
 * Add a capacity to a node's inventory (only works on root nodes)
 * @param cap Capacity to add
 * @param zipper TreeZipper with node to modify
 * @returns Updated TreeZipper
 */
export function addCapacity(cap: Capacity, zipper: TreeZipper): TreeZipper {
	const node = zipper.zipperCurrent;

	// Only root nodes can have capacities
	if (node.type !== 'root') {
		return zipper;
	}

	// Create updated root node with the new capacity
	const updatedNode: RootNode = {
		...node,
		nodeCapacities: new Map(node.nodeCapacities)
	};

	// Add the capacity
	updatedNode.nodeCapacities.set(cap.capacityId, cap);

	return {
		...zipper,
		zipperCurrent: updatedNode
	};
}

/**
 * Add a share in another node's capacity (only works on root nodes)
 * @param shareId Unique ID for the share
 * @param share Capacity share object
 * @param zipper TreeZipper with node to modify
 * @returns Updated TreeZipper
 */
export function addCapacityShare(
	shareId: string,
	share: CapacityShare,
	zipper: TreeZipper
): TreeZipper {
	const node = zipper.zipperCurrent;

	// Only root nodes can have capacity shares
	if (node.type !== 'root') {
		return zipper;
	}

	// Create updated root node with the new share
	const updatedNode: RootNode = {
		...node,
		nodeCapacityShares: new Map(node.nodeCapacityShares)
	};

	// Add the share
	updatedNode.nodeCapacityShares.set(shareId, share);

	return {
		...zipper,
		zipperCurrent: updatedNode
	};
}

/**
 * Update computed quantities for all capacity shares in a node
 * @param zipper TreeZipper with node to update
 * @returns Updated TreeZipper
 */
export function updateComputedQuantities(zipper: TreeZipper): TreeZipper {
	const node = zipper.zipperCurrent;

	// Only root nodes have capacity shares
	if (node.type !== 'root') {
		return zipper;
	}

	// Create updated shares map
	const updatedShares = new Map();

	// Update each share with recomputed quantity
	for (const [id, share] of node.nodeCapacityShares) {
		updatedShares.set(id, {
			...share,
			computedQuantity: computeQuantityShare(share.targetCapacity, share.sharePercentage)
		});
	}

	// Return updated zipper
	return {
		...zipper,
		zipperCurrent: {
			...node,
			nodeCapacityShares: updatedShares
		}
	};
}

/**
 * Get a person's total share in a specific capacity
 * @param forest Forest of all nodes
 * @param person TreeZipper for the person
 * @param capacityId ID of the capacity
 * @returns Share value (0-1)
 */
export function getPersonalCapacityShare(
	forest: Forest,
	person: TreeZipper,
	capacityId: string
): number {
	// Find all capacity owners with this capacity
	const capacityOwners: TreeZipper[] = [];

	for (const [_, owner] of forest) {
		const ownerNode = owner.zipperCurrent;
		if (ownerNode.type === 'root' && ownerNode.nodeCapacities.has(capacityId)) {
			capacityOwners.push(owner);
		}
	}

	if (capacityOwners.length === 0) return 0;

	// Calculate share from each capacity owner
	const shares = capacityOwners.map((owner) => {
		const ownerNode = owner.zipperCurrent;
		// Safe type check even though we filtered above
		if (ownerNode.type !== 'root') return 0;

		const cap = ownerNode.nodeCapacities.get(capacityId);
		if (!cap) return 0; // Should never happen due to our filter above
		return receiverShareFrom(forest, person, owner, cap, 2);
	});

	// Return the maximum share (from the most direct provider)
	return Math.max(...shares);
}

/**
 * Retrieve a capacity from a node by ID
 * @param zipper TreeZipper with node
 * @param capacityId ID of the capacity to retrieve
 * @returns Capacity object or null if not found or node doesn't have capacities
 */
export function getCapacity(zipper: TreeZipper, capacityId: string): Capacity | null {
	const node = zipper.zipperCurrent;

	if (node.type !== 'root') {
		return null;
	}

	return node.nodeCapacities.get(capacityId) || null;
}
