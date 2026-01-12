import type { NeedSlot, AvailabilitySlot } from '$lib/protocol/schemas';

/**
 * Check if a NeedSlot and AvailabilitySlot are compatible.
 * 
 * Basic compatibility requires:
 * 1. Matching type_id
 * 2. (Optional) Matching time availability (if strictly enforced here, usually handled by solver)
 * 3. (Optional) Location proximity (if strictly enforced)
 * 
 * For this implementation, we focus on type_id and lenient property matching.
 */
export function slotsCompatible(need: NeedSlot, capacity: AvailabilitySlot): boolean {
    // 1. Type ID must match
    if (need.type_id !== capacity.type_id) {
        return false;
    }

    // 2. Strict equality checks for testing (can be relaxed)
    // If specific matching criteria are needed, add them here.
    // For now, we assume type_id match is sufficient for "Hard Compatibility".
    // Usage constraints (time, location, quantity) are soft constraints handled by scoring.

    return true;
}
