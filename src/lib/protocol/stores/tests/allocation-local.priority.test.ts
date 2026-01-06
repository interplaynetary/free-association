
import { describe, it, expect } from 'bun:test';
import { getSlotPriority } from '../../allocation-local';
import type { AvailabilitySlot, NeedSlot, Commitment } from '@playnet/free-association/schemas';

describe('getSlotPriority (Optimized Person-to-Person)', () => {

    const ownerPubkey = 'pubkey-owner';
    const otherPersonPubkey = 'pubkey-other';

    // Mock Commitment Object (Single User)
    const ownerCommitment: Commitment = {
        capacity_slots: [],
        need_slots: [],
        global_recognition_weights: {
            [otherPersonPubkey]: 0.5 // Default global recognition
        },
        timestamp: Date.now()
    };

    it('should return 0 if slot has no ID', () => {
        const slot: any = {};
        const priority = getSlotPriority(slot, otherPersonPubkey, ownerCommitment);
        expect(priority).toBe(0);
    });

    it('should use explicit slot-specific priority if defined', () => {
        const slot: AvailabilitySlot = {
            id: 'slot-1',
            name: 'Slot with Explicit Priority',
            quantity: 10,
            priority_distribution: {
                [otherPersonPubkey]: 0.9 // Explicit override
            }
        };

        // Should ignore global (0.5) from commitment and use explicit (0.9)
        const priority = getSlotPriority(slot, otherPersonPubkey, ownerCommitment);
        expect(priority).toBe(0.9);
    });

    it('should fall back to global recognition weights from passed commitment', () => {
        const slot: AvailabilitySlot = {
            id: 'slot-2',
            name: 'Slot with Implicit Priority',
            quantity: 10
            // No priority_distribution
        };

        // Should use global default (0.5)
        const priority = getSlotPriority(slot, otherPersonPubkey, ownerCommitment);
        expect(priority).toBe(0.5);
    });

    it('should return 0 if priority is explicitly set to 0 even if global exists', () => {
        const slot: AvailabilitySlot = {
            id: 'slot-3',
            name: 'Slot with Explicit Zero',
            quantity: 10,
            priority_distribution: {
                [otherPersonPubkey]: 0 // Explicitly 0
            }
        };

        // Should use explicit (0), not global (0.5)
        const priority = getSlotPriority(slot, otherPersonPubkey, ownerCommitment);
        expect(priority).toBe(0);
    });

    it('should return 0 if commitment is undefined (owner not found)', () => {
        const slot: AvailabilitySlot = {
            id: 'slot-orphan',
            name: 'Orphan Slot',
            quantity: 10
        };

        // Pass undefined as commitment
        const priority = getSlotPriority(slot, otherPersonPubkey, undefined);
        expect(priority).toBe(0);
    });

    it('should return 0 if commitment has no global weights', () => {
        const emptyCommitment: Commitment = {
            capacity_slots: [],
            need_slots: [],
            timestamp: Date.now()
        };

        const slot: AvailabilitySlot = { id: 's1', name: 's1', quantity: 10 };
        const priority = getSlotPriority(slot, otherPersonPubkey, emptyCommitment);
        expect(priority).toBe(0);
    });

});
