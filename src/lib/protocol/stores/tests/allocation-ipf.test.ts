
import { describe, it, expect } from 'vitest';
import {
    calculateIPFAllocation
} from './allocation-ipf';
import {
    calculateSeedValue
} from './ipf-core';
import type { AvailabilitySlot, NeedSlot, Commitment } from '@playnet/free-association/schemas';

// --- MOCK FACTORIES ---

const createCapacitySlot = (id: string, quantity: number, overrides: Partial<AvailabilitySlot> = {}): AvailabilitySlot => ({
    id,
    name: `Capacity ${id}`,
    quantity,
    type_id: 'type1', // IPF requires types notionally
    ...overrides
});

const createNeedSlot = (id: string, quantity: number, overrides: Partial<NeedSlot> = {}): NeedSlot => ({
    id,
    name: `Need ${id}`,
    quantity,
    type_id: 'type1',
    ...overrides
});

const createCommitment = (pubkey: string, overrides: Partial<Commitment> = {}): Commitment => ({
    capacity_slots: [],
    need_slots: [],
    timestamp: Date.now(),
    itcStamp: {},
    global_recognition_weights: {},
    ...overrides
});

describe('Centralized IPF Allocation (Sinkhorn)', () => {

    describe('1. Core Math (Seed Calculation)', () => {
        it('should calculate seed P * R^gamma', () => {
            const cs = createCapacitySlot('c1', 100, {
                priority_distribution: { 'r1': 0.8 }
            });
            const ns = createNeedSlot('n1', 100, {
                priority_distribution: { 'p1': 0.5 }
            });
            const commitments = {
                'p1': createCommitment('p1', { capacity_slots: [cs] }),
                'r1': createCommitment('r1', { need_slots: [ns] })
            };

            // P=0.8, R=0.5, Gamma=0.5
            // Seed = (0.8 + eps) * (0.5 + eps)^0.5
            const seed = calculateSeedValue(cs, ns, commitments, 0, 0.5);
            expect(seed).toBeCloseTo(0.8 * Math.sqrt(0.5), 5);
        });

        it('should return 0 if incompatible', () => {
            // Mock incompatibility via type mismatch if we were using real matcher, 
            // but calculateSeed calls slotsCompatible. 
            // Here we assume defaults are compatible.
            // Let's force priority 0.
            const cs = createCapacitySlot('c1', 100);
            const ns = createNeedSlot('n1', 100);

            // Should be 0 without epsilon
            const seed = calculateSeedValue(cs, ns, {}, 0, 0.5);
            expect(seed).toBe(0);
        });
    });

    describe('2. Single Provider / Single Recipient', () => {
        it('should allocate min(Cap, Need) with full priority', () => {
            // Cap 100, Need 50 -> 50
            const cs = createCapacitySlot('c1', 100, { priority_distribution: { 'r1': 1.0 } });
            const ns = createNeedSlot('n1', 50, { priority_distribution: { 'p1': 1.0 } });
            const commitments = {
                'p1': createCommitment('p1', { capacity_slots: [cs] }),
                'r1': createCommitment('r1', { need_slots: [ns] })
            };

            const res = calculateIPFAllocation([cs], [ns], commitments);
            expect(res.length).toBe(1);
            expect(res[0].quantity).toBeCloseTo(50);
        });

        it('should allocate min(Cap, Need) when Cap < Need', () => {
            // Cap 50, Need 100 -> 50
            const cs = createCapacitySlot('c1', 50, { priority_distribution: { 'r1': 1.0 } });
            const ns = createNeedSlot('n1', 100, { priority_distribution: { 'p1': 1.0 } });
            const commitments = {
                'p1': createCommitment('p1', { capacity_slots: [cs] }),
                'r1': createCommitment('r1', { need_slots: [ns] })
            };

            const res = calculateIPFAllocation([cs], [ns], commitments);
            expect(res.length).toBe(1);
            expect(res[0].quantity).toBeCloseTo(50);
        });
    });

    describe('3. Multi-Recipient Constraints', () => {
        it('should split capacity proportionally to need if over-subscribed', () => {
            // Cap 100.
            // N1 Need 100. N2 Need 100.
            // Priorities equal.
            const cs = createCapacitySlot('c1', 100, { priority_distribution: { 'r1': 1.0, 'r2': 1.0 } });
            const n1 = createNeedSlot('n1', 100, { priority_distribution: { 'p1': 1.0 } });
            const n2 = createNeedSlot('n2', 100, { priority_distribution: { 'p1': 1.0 } });

            const commitments = {
                'p1': createCommitment('p1', { capacity_slots: [cs] }),
                'r1': createCommitment('r1', { need_slots: [n1] }),
                'r2': createCommitment('r2', { need_slots: [n2] })
            };

            const res = calculateIPFAllocation([cs], [n1, n2], commitments);

            const q1 = res.find(r => r.need_slot_id === 'n1')?.quantity || 0;
            const q2 = res.find(r => r.need_slot_id === 'n2')?.quantity || 0;

            // Total 100 distributed. 50/50 split.
            expect(q1 + q2).toBeCloseTo(100);
            expect(q1).toBeCloseTo(50);
            expect(q2).toBeCloseTo(50);
        });
    });

    describe('4. Complex Topology', () => {
        it('should converge to Pareto efficient state in cycle', () => {
            // C1 (100) -> N1, N2
            // C2 (100) -> N2, N3
            // N1, N2, N3 Need 100.
            // Expected: N1(50), N3(50), N2(100 from both).

            const c1 = createCapacitySlot('c1', 100, { priority_distribution: { 'n1': 1, 'n2': 1 } }); // Prioritizing Need IDs directly for simplicity of test override logic if supported, 
            // but standard logic looks up pubkey. The mock `findOwner` is key.
            // Let's use pubkeys.
            // p1 -> r1 (n1 owner), r2 (n2 owner)
            // p2 -> r2, r3 (n3 owner)

            // Updating priorities:
            // C1 prioritizes R1, R2 equally.
            // C2 prioritizes R2, R3 equally.
            const c1_actual = createCapacitySlot('c1', 100, { priority_distribution: { 'r1': 1, 'r2': 1 } });
            const c2_actual = createCapacitySlot('c2', 100, { priority_distribution: { 'r2': 1, 'r3': 1 } });

            const n1 = createNeedSlot('n1', 100, { priority_distribution: { 'p1': 1 } });
            const n2 = createNeedSlot('n2', 100, { priority_distribution: { 'p1': 1, 'p2': 1 } });
            const n3 = createNeedSlot('n3', 100, { priority_distribution: { 'p2': 1 } });

            const commitments = {
                'p1': createCommitment('p1', { capacity_slots: [c1_actual] }),
                'p2': createCommitment('p2', { capacity_slots: [c2_actual] }),
                'r1': createCommitment('r1', { need_slots: [n1] }),
                'r2': createCommitment('r2', { need_slots: [n2] }),
                'r3': createCommitment('r3', { need_slots: [n3] })
            };

            const res = calculateIPFAllocation([c1_actual, c2_actual], [n1, n2, n3], commitments);

            const receivedN1 = res.filter(r => r.need_slot_id === 'n1').reduce((s, r) => s + r.quantity, 0);
            const receivedN2 = res.filter(r => r.need_slot_id === 'n2').reduce((s, r) => s + r.quantity, 0);
            const receivedN3 = res.filter(r => r.need_slot_id === 'n3').reduce((s, r) => s + r.quantity, 0);

            // C1 splits 50/50 between R1/R2? No.
            // R2 has pull from TWO providers.
            // R1 has pull from ONE.
            // In Sinkhorn, flow moves where demand is highest.
            // R2 needs 100 total. R1 needs 100 total.

            // Equilibrium:
            // R2 is "more competitive".
            // If C1 gives 50 to R1, 50 to R2.
            // C2 gives 50 to R3, 50 to R2.
            // R2 gets 100 total. Perfect.
            // R1 gets 50 (Under-satisfied).
            // R3 gets 50 (Under-satisfied).
            // Total Supply 200. Total Demand 300. 
            // This is the fairest proportional outcome.

            expect(receivedN1).toBeCloseTo(50, 0);
            expect(receivedN3).toBeCloseTo(50, 0);
            expect(receivedN2).toBeCloseTo(100, 0);
        });
    });

    describe('5. Asymmetric Influence (Gamma)', () => {
        it('Gamma=0 (Provider Dictatorship)', () => {
            // Recipient Preference should be ignored.
            const cs = createCapacitySlot('c1', 100, { priority_distribution: { 'r1': 1.0 } });
            // Recipient has 0 priority for provider (e.g. hates them)
            const ns = createNeedSlot('n1', 100, { priority_distribution: { 'p1': 0.0 } });

            const commitments = {
                'p1': createCommitment('p1', { capacity_slots: [cs] }),
                'r1': createCommitment('r1', { need_slots: [ns] })
            };

            // With Gamma=0, Seed = (P+eps) * (R+eps)^0 = P+eps.
            // Flow should happen despite R=0.
            const res = calculateIPFAllocation([cs], [ns], commitments, { gamma: 0, epsilon: 1e-6 });

            expect(res[0].quantity).toBeCloseTo(100);
        });
    });
});
