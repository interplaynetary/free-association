import type { Capacity, CapacityShare, Forest, TreeZipper } from './types';
import { modifyNode } from './node';
import { receiverShareFrom } from './mutual';

// Compute the quantity of a capacity share based on percentage and capacity constraints
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

// Create a new capacity share
export function createCapacityShare(cap: Capacity, percentage: number): CapacityShare {
	return {
		targetCapacity: cap,
		sharePercentage: percentage,
		computedQuantity: computeQuantityShare(cap, percentage)
	};
}

// Add a capacity to a node's inventory
export function addCapacity(cap: Capacity, zipper: TreeZipper): TreeZipper {
	return modifyNode((node) => {
		const updatedCapacities = new Map(node.nodeCapacities);
		updatedCapacities.set(cap.capacityId, cap);

		return {
			...node,
			nodeCapacities: updatedCapacities
		};
	}, zipper);
}

// Add a share in another node's capacity
export function addCapacityShare(
	shareId: string,
	share: CapacityShare,
	zipper: TreeZipper
): TreeZipper {
	return modifyNode((node) => {
		const updatedShares = new Map(node.nodeCapacityShares);
		updatedShares.set(shareId, share);

		return {
			...node,
			nodeCapacityShares: updatedShares
		};
	}, zipper);
}

// Update computed quantities for all capacity shares in a node
export function updateComputedQuantities(zipper: TreeZipper): TreeZipper {
	return modifyNode((node) => {
		const updatedShares = new Map();

		// Update each share with recomputed quantity
		for (const [id, share] of node.nodeCapacityShares) {
			updatedShares.set(id, {
				...share,
				computedQuantity: computeQuantityShare(share.targetCapacity, share.sharePercentage)
			});
		}

		return {
			...node,
			nodeCapacityShares: updatedShares
		};
	}, zipper);
}

// Get a person's total share in a specific capacity
export function getPersonalCapacityShare(
	forest: Forest,
	person: TreeZipper,
	capacityId: string
): number {
	// Find all capacity owners with this capacity
	const capacityOwners: TreeZipper[] = [];

	for (const [_, owner] of forest) {
		if (owner.zipperCurrent.nodeCapacities.has(capacityId)) {
			capacityOwners.push(owner);
		}
	}

	if (capacityOwners.length === 0) return 0;

	// Calculate share from each capacity owner
	const shares = capacityOwners.map((owner) => {
		const cap = owner.zipperCurrent.nodeCapacities.get(capacityId);
		if (!cap) return 0; // Should never happen due to our filter above
		return receiverShareFrom(forest, person, owner, 2);
	});

	// Return the maximum share (from the most direct provider)
	return Math.max(...shares);
}

// Retrieve a capacity from a node by ID
export function getCapacity(zipper: TreeZipper, capacityId: string): Capacity | null {
	return zipper.zipperCurrent.nodeCapacities.get(capacityId) || null;
}
