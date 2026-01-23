
import { describe, it, expect } from 'vitest';
import { calculateSlotBasedPriorityAllocation } from '../../docs/experimental/allocation-local';

// Types mock
type AvailabilitySlot = any;
type NeedSlot = any;
type Commitment = any;

// --------------------------------------------------------------------------
// Copied from allocation.svelte.ts (lines ~508-550)
// --------------------------------------------------------------------------
function findSlotOwner(slotId: string, allCommitments: Record<string, Commitment>): string | null {
    for (const [pubKey, commitment] of Object.entries(allCommitments)) {
        if (commitment.need_slots?.some((s: any) => s.id === slotId)) return pubKey;
        if (commitment.capacity_slots?.some((s: any) => s.id === slotId)) return pubKey;
    }
    return null;
}

function enrichCapacitySlotWithPriorities(
    slot: AvailabilitySlot,
    networkNeeds: NeedSlot[],
    myRecognition: Record<string, number>,
    allCommitments: Record<string, Commitment>
): AvailabilitySlot {
    // Already has priorities? Return as-is
    if (slot.priority_distribution && slot.priority_distribution.length > 0) {
        return slot;
    }

    // Synthesize from recognition: for each network need, priority = my recognition of owner
    const generated = networkNeeds
        .map(ns => {
            if (!ns.id) return null;
            const owner = findSlotOwner(ns.id, allCommitments);
            // For self-allocation, always prioritize 100%
            if (owner === myPub) {
                return { target_slot_id: ns.id, priority_percentage: 1.0 };
            }

            if (!owner || (myRecognition[owner] || 0) <= 0.001) return null;
            return { target_slot_id: ns.id, priority_percentage: myRecognition[owner] };
        })
        .filter((p): p is { target_slot_id: string; priority_percentage: number } => p !== null);

    return { ...slot, priority_distribution: generated };
}

function enrichNeedSlotWithPriorities(
    slot: NeedSlot,
    myCapacity: AvailabilitySlot[],
    theirRecognitionOfMe: number
): NeedSlot {
    // Already has priorities? Return as-is
    if (slot.priority_distribution && slot.priority_distribution.length > 0) {
        return slot;
    }

    // Synthesize from recognition: for each of my capacity slots, priority = their recognition of me
    const generated = myCapacity
        .map(cs => {
            if (theirRecognitionOfMe <= 0.001) return null;
            return { target_slot_id: cs.id, priority_percentage: theirRecognitionOfMe };
        })
        .filter((p): p is { target_slot_id: string | undefined; priority_percentage: number } => p !== null);

    return { ...slot, priority_distribution: generated };
}

// --------------------------------------------------------------------------
// TEST
// --------------------------------------------------------------------------

const myPub = "user1";
const myNeedId = "need1";
const myCapId = "cap1";
const resourceTypeId = "type1";

const myNeedSlot = {
    id: myNeedId,
    type_id: resourceTypeId,
    quantity: 1, // LOW QUANTITY
    start_date: "2024-01-01",
    // IMPORTANT: Match compatibility fields
    location_type: "anywhere",
    priority_distribution: undefined
};

const myCapSlot = {
    id: myCapId,
    type_id: resourceTypeId,
    quantity: 1, // LOW QUANTITY
    start_date: "2024-01-01",
    location_type: "anywhere",
    min_atomic_size: 1, // ENFORCE INTEGER UNITS
    priority_distribution: undefined
};

const allCommitments = {
    [myPub]: {
        pubkey: myPub,
        need_slots: [myNeedSlot],
        capacity_slots: [myCapSlot]
    }
}

describe('Self Allocation Enrichment + Algorithm', () => {
    it('should generate priorities and allocate correctly', () => {
        // 1. Setup
        const myRecognition = {
            [myPub]: 0.0774 // 7.74% self recognition from logs
        };

        // 2. Enrich Capacity
        // Note: allocation.svelte passes 'allNeedsRaw' which includes my needs
        const networkNeeds = [myNeedSlot];

        const enrichedCap = enrichCapacitySlotWithPriorities(
            { ...myCapSlot },
            networkNeeds,
            myRecognition,
            allCommitments
        );

        console.log("Enriched Cap Priority Dist:", enrichedCap.priority_distribution);
        expect(enrichedCap.priority_distribution).toHaveLength(1);
        expect(enrichedCap.priority_distribution[0].priority_percentage).toBe(1.0);

        // 3. Enrich Need
        // Note: In allocation.svelte 'theirRecognitionOfMe' logic
        // if owner === myPub, priorityOfMe = myRec[owner]
        const priorityOfMe = myRecognition[myPub];

        const enrichedNeed = enrichNeedSlotWithPriorities(
            { ...myNeedSlot },
            [{ ...enrichedCap }], // It uses myCapacity array
            priorityOfMe
        );

        console.log("Enriched Need Priority Dist:", enrichedNeed.priority_distribution);


        // 4. Run Allocation
        const allocations = calculateSlotBasedPriorityAllocation(
            [enrichedCap],
            [enrichedNeed],
            allCommitments,
            { debug: true }
        );

        console.log("Allocations:", allocations);

        // 5. Expectation
        // Bisect logic:
        // Raw Limit: 0.0774 * 1 = 0.0774.
        // If Divisibility applied:
        // Should round UP to 1 via Largest Remainder (since surplus = 1 - 0 = 1, and this need is compatible).
        // UNLESS 'redistributeSurplus' logic fails to include it?

        const alloc = allocations.find(a => a.need_slot_id === myNeedId && a.capacity_slot_id === myCapId);

        expect(alloc).toBeDefined();
        // expect(alloc?.quantity).toBeCloseTo(1.0, 1);
        // LOG the result to see what happens
        if (alloc) {
            console.log("ALLOCATED QUANTITY:", alloc.quantity);
        }
    });
});
