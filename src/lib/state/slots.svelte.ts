/**
 * Slot Allocation System - Re-exports from core.svelte.ts
 *
 * MOVED TO AVOID CIRCULAR DEPENDENCY:
 * The slot allocation stores have been moved to core.svelte.ts to break the circular dependency:
 * core.svelte.ts ← persistence.svelte.ts ← compose.svelte.ts ← slots.svelte.ts
 *
 * This file now serves as a re-export module for backward compatibility.
 */

// RE-EXPORT SLOT ALLOCATION AND COMPOSITION STORES FROM CORE.SVELTE.TS
export {
	userDesiredSlotClaims,
	networkDesiredSlotClaims,
	userDesiredSlotComposeFrom,
	userDesiredSlotComposeInto,
	networkDesiredSlotComposeFrom,
	networkDesiredSlotComposeInto,
	slotAllocationAnalysis,
	feasibleSlotClaims,
	allocatedSlots,
	allocatedSlotAmounts,
	slotClaimMetadata
} from './core.svelte';

// Helper functions for backward compatibility (if needed by components)
export function getSlotById(capacity: any, slotId: string) {
	return capacity.availability_slots?.find((slot: any) => slot.id === slotId);
}

export function getTotalComputedQuantity(capacity: any): number {
	if (capacity.computed_quantities && Array.isArray(capacity.computed_quantities)) {
		return capacity.computed_quantities.reduce(
			(sum: number, slot: any) => sum + (slot.quantity || 0),
			0
		);
	}
	return 0;
}
