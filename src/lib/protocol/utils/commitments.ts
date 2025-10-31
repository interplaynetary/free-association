/**
 * Commitment Utilities
 * 
 * Pure utility functions for working with commitments and allocations.
 * Helpers for creating, analyzing, and querying allocation data.
 */

import type {
	Commitment,
	GlobalRecognitionWeights,
	AvailabilitySlot,
	NeedSlot,
	ITCStamp,
	TwoTierAllocationState,
	SlotAllocationRecord
} from '$lib/protocol/schemas';

// ═══════════════════════════════════════════════════════════════════
// COMMITMENT CREATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Create a commitment from recognition shares and capacity/need declarations
 * 
 * V5 uses GLOBAL recognition: same MR value for all need types
 * Type preferences are encoded in recognition tree structure
 */
export function createCommitment(
	globalRecognitionWeights: GlobalRecognitionWeights,
	capacitySlots?: AvailabilitySlot[],
	needSlots?: NeedSlot[],
	itcStamp?: ITCStamp
): Commitment {
	return {
		capacity_slots: capacitySlots,
		need_slots: needSlots,
		global_recognition_weights: globalRecognitionWeights,
		timestamp: Date.now(),
		itcStamp: itcStamp!
	};
}

// ═══════════════════════════════════════════════════════════════════
// ALLOCATION ANALYSIS
// ═══════════════════════════════════════════════════════════════════

/**
 * Extract allocation records for a specific recipient
 * Useful for analyzing what a recipient received across all slots
 */
export function getAllocationRecordsForRecipient(
	allocationState: TwoTierAllocationState,
	recipientPubkey: string
): SlotAllocationRecord[] {
	return allocationState.slot_allocations.filter(
		(record) => record.recipient_pubkey === recipientPubkey
	);
}

/**
 * Extract allocation records for a specific availability slot
 * Useful for analyzing how a slot was distributed
 */
export function getAllocationRecordsForSlot(
	allocationState: TwoTierAllocationState,
	slotId: string
): SlotAllocationRecord[] {
	return allocationState.slot_allocations.filter(
		(record) => record.availability_slot_id === slotId
	);
}

/**
 * Calculate total quantity allocated in mutual tier for a slot
 */
export function getMutualTierTotal(
	allocationState: TwoTierAllocationState,
	slotId: string
): number {
	return allocationState.slot_allocations
		.filter((record) => record.availability_slot_id === slotId && record.tier === 'mutual')
		.reduce((sum, record) => sum + record.quantity, 0);
}

/**
 * Calculate total quantity allocated in non-mutual tier for a slot
 */
export function getNonMutualTierTotal(
	allocationState: TwoTierAllocationState,
	slotId: string
): number {
	return allocationState.slot_allocations
		.filter((record) => record.availability_slot_id === slotId && record.tier === 'non-mutual')
		.reduce((sum, record) => sum + record.quantity, 0);
}

/**
 * Get mutual tier recipients for a slot
 */
export function getMutualTierRecipients(
	allocationState: TwoTierAllocationState,
	slotId: string
): string[] {
	const recipients = new Set<string>();
	allocationState.slot_allocations
		.filter((record) => record.availability_slot_id === slotId && record.tier === 'mutual')
		.forEach((record) => recipients.add(record.recipient_pubkey));
	return Array.from(recipients);
}

/**
 * Check if a recipient is in the mutual tier for a slot
 */
export function isRecipientMutual(
	allocationState: TwoTierAllocationState,
	slotId: string,
	recipientPubkey: string
): boolean {
	return allocationState.slot_allocations.some(
		(record) =>
			record.availability_slot_id === slotId &&
			record.recipient_pubkey === recipientPubkey &&
			record.tier === 'mutual'
	);
}

