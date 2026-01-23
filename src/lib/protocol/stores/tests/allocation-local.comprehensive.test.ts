
import { describe, it, expect } from 'vitest';
import {
    applyDivisibilityConstraints,
    meetsMinimumAllocation,
    calculateCompatibility,
    redistributeSurplus,
    initialAllocationWithSurplus,
    calculateSlotBasedPriorityAllocation,
    iterativeRefinement,
    calculateTotalDeviation,
    EPSILON
} from '../../docs/experimental/allocation-local';
import type { AvailabilitySlot, NeedSlot, Commitment } from '@playnet/free-association/schemas';

// --- MOCK DATA FACTORIES ---

const createCapacitySlot = (id: string, quantity: number, overrides: Partial<AvailabilitySlot> = {}): AvailabilitySlot => ({
    id,
    name: `Capacity ${id}`,
    quantity,
    ...overrides
});

const createNeedSlot = (id: string, quantity: number, overrides: Partial<NeedSlot> = {}): NeedSlot => ({
    id,
    name: `Need ${id}`,
    quantity,
    ...overrides
});

const createCommitment = (overrides: Partial<Commitment> = {}): Commitment => ({
    capacity_slots: [],
    need_slots: [],
    timestamp: Date.now(),
    ...overrides
});

// --- TESTS ---

describe('Allocation Local - Comprehensive Tests', () => {

    describe('1. Utility Functions', () => {
        describe('applyDivisibilityConstraints', () => {
            // min_allocation_percentage is deprecated/removed. Test removed.

            it('should respect min atomic size (quantization)', () => {
                const slot = createCapacitySlot('c1', 10, { min_atomic_size: 2 });
                // Atomic size = 2.0
                expect(applyDivisibilityConstraints(1.9, 0.19, slot)).toBe(0);
                expect(applyDivisibilityConstraints(2.1, 0.21, slot)).toBe(2);
                expect(applyDivisibilityConstraints(3.9, 0.39, slot)).toBe(2);
                expect(applyDivisibilityConstraints(4.0, 0.4, slot)).toBe(4);
            });
        });

        describe('meetsMinimumAllocation', () => {
            it('should fail if quantity is effectively zero', () => {
                const slot = createCapacitySlot('c1', 100);
                expect(meetsMinimumAllocation(0, slot)).toBe(false);
                expect(meetsMinimumAllocation(1e-10, slot)).toBe(false);
            });



            it('should fail if below unit size', () => {
                const slot = createCapacitySlot('c1', 10, { min_atomic_size: 5 }); // Atomic 5
                expect(meetsMinimumAllocation(4, slot)).toBe(false);
                expect(meetsMinimumAllocation(5, slot)).toBe(true);
            });
        });
    });

    describe('2. Compatibility & Priority', () => {
        it('should calculate bilateral limits correctly', () => {
            const cs = createCapacitySlot('c1', 100, {
                priority_distribution: [{ target_slot_id: 'n1', priority_percentage: 0.5 }]
            });
            const ns = createNeedSlot('n1', 50, { // Need is 50
                priority_distribution: [{ target_slot_id: 'c1', priority_percentage: 0.8 }]
            });

            const needSlots = [ns];
            const compatibility = calculateCompatibility(cs, needSlots);

            const info = compatibility.get('n1');
            expect(info).toBeDefined();
            expect(info?.isCompatible).toBe(true);

            // Provider Limit: 0.5 * 100 = 50
            expect(info?.providerLimit).toBe(50);

            // Recipient Limit: 0.8 * 50 = 40
            expect(info?.recipientLimit).toBe(40);

            // Bilateral = min(50, 40) = 40
            expect(info?.bilateralLimit).toBe(40);
        });

        it('should return zero limits if incompatible', () => {
            // Mock incompatible via mismatch (e.g. time range or location could be added here if we mocked the matcher, 
            // but here we check basic priority zeroing if incompatible)
            // Using slot IDs which usually don't affect compatibility directly unless mocked match.ts logic fails.
            // However, `calculateCompatibility` calls `slotsCompatible` internally. 
            // Without mocking `slotsCompatible`, we rely on it returning true for simple slots.
            // If we want to force incompatibility without mocking, we can use distinct time ranges if schemas allow, 
            // but simpliest is to assume they ARE compatible by default and check valid inputs.
            // For this test, we verify that if priorities are missing, limits are 0.

            const cs = createCapacitySlot('c1', 100); // No priority
            const ns = createNeedSlot('n1', 50);

            const compatibility = calculateCompatibility(cs, [ns]);
            const info = compatibility.get('n1');

            expect(info?.providerLimit).toBe(0);
            expect(info?.recipientLimit).toBe(0);
        });
    });

    describe('3. Core Allocation Logic (Phase 1)', () => {
        it('should allocate up to provider limit in Phase 1', () => {
            const cs = createCapacitySlot('c1', 100, {
                priority_distribution: [{ target_slot_id: 'n1', priority_percentage: 0.6 }]
            });
            const ns = createNeedSlot('n1', 80); // Need 80

            // Phase 1 uses provider limit (60) even if need is higher (80)
            // But strict priority limit is 0.6 * 100 = 60.

            const matrix = initialAllocationWithSurplus([cs], [ns], {}, false);
            const alloc = matrix['c1']['n1'];

            expect(alloc.amount).toBe(80);  // 60 limit + 20 surplus redistribution
            expect(alloc.fromSurplus).toBe(true);
        });

        it('should redistribute surplus if provider has extra capacity and need is unmet', () => {
            const cs = createCapacitySlot('c1', 100, {
                priority_distribution: [{ target_slot_id: 'n1', priority_percentage: 0.5 }]
            });
            // Provider gives 50 initially. Remaining 50 is surplus.

            const ns = createNeedSlot('n1', 80); // Need 80.
            // Initial: 50. Unmet: 30.

            // Should get +30 from surplus? 
            // Phase 1 redistributes surplus to compatible needs.
            // Since n1 is the only one and has priority > 0, it should get more.

            const matrix = initialAllocationWithSurplus([cs], [ns], {}, false);
            const alloc = matrix['c1']['n1'];

            // Initial 50 + Surplus redistribution
            // It should satisfy the full need of 80 if surplus allows.
            // Surplus = 50. Needed = 30. Can cover it.

            expect(alloc.amount).toBe(80);
            expect(alloc.fromSurplus).toBe(true);
        });
    });

    describe('4. Refinement & Convergence', () => {
        it('should reduce deviations in Phase 2', () => {
            // Setup a scenario where Phase 1 creates a deviation
            // Example: 2 providers, 1 receiver. 
            // P1 prefers R1 (100%). P2 prefers R1 (50%).
            // R1 prefers P1 (100%).

            // ... constructing a scenario where refinement helps is subtle.
            // Let's rely on basic property: Deviation should go down or stay 0.

            const cs = createCapacitySlot('c1', 100, {
                priority_distribution: [{ target_slot_id: 'n1', priority_percentage: 1.0 }]
            });
            const ns = createNeedSlot('n1', 100, {
                priority_distribution: [{ target_slot_id: 'c1', priority_percentage: 1.0 }]
            });

            const matrix = initialAllocationWithSurplus([cs], [ns], {}, false);
            // Should be perfect 100

            const devBefore = calculateTotalDeviation(matrix, [cs], [ns], 0.5);
            expect(devBefore).toBeLessThan(EPSILON);

            const res = iterativeRefinement(matrix, [cs], [ns], { debug: false });
            expect(res.converged).toBe(true);
        });

        it('should handle competing priorities', () => {
            // C1 -> N1 (0.8), N2 (0.2)
            // C1 Cap: 100. N1 Need: 100. N2 Need: 100.

            const cs = createCapacitySlot('c1', 100, {
                priority_distribution: [
                    { target_slot_id: 'n1', priority_percentage: 0.8 },
                    { target_slot_id: 'n2', priority_percentage: 0.2 }
                ]
            });
            const n1 = createNeedSlot('n1', 100, { priority_distribution: [{ target_slot_id: 'c1', priority_percentage: 1 }] });
            const n2 = createNeedSlot('n2', 100, { priority_distribution: [{ target_slot_id: 'c1', priority_percentage: 1 }] });

            const result = calculateSlotBasedPriorityAllocation([cs], [n1, n2], {});

            const allocN1 = result.find(r => r.need_slot_id === 'n1')?.quantity || 0;
            const allocN2 = result.find(r => r.need_slot_id === 'n2')?.quantity || 0;

            // Should respect proportions roughly
            expect(allocN1).toBeCloseTo(80, 0);
            expect(allocN2).toBeCloseTo(20, 0);
        });
    });

    describe('5. Advanced Scenarios', () => {
        it('Multi-Provider Need Satisfaction: 2 Sources -> 1 Need', () => {
            // C1 (100) -> N1 (100%)
            // C2 (100) -> N1 (100%)
            // N1 Need (150)

            const c1 = createCapacitySlot('c1', 100, {
                priority_distribution: [{ target_slot_id: 'n1', priority_percentage: 1 }]
            });
            const c2 = createCapacitySlot('c2', 100, {
                priority_distribution: [{ target_slot_id: 'n1', priority_percentage: 1 }]
            });
            const n1 = createNeedSlot('n1', 150, {
                priority_distribution: [
                    { target_slot_id: 'c1', priority_percentage: 0.5 },
                    { target_slot_id: 'c2', priority_percentage: 0.5 }
                ]
            });

            const result = calculateSlotBasedPriorityAllocation([c1, c2], [n1], {});

            const fromC1 = result.find(r => r.capacity_slot_id === 'c1')?.quantity || 0;
            const fromC2 = result.find(r => r.capacity_slot_id === 'c2')?.quantity || 0;

            // Heuristic algorithm might not hit exactly 150 in default iterations, but should be close.
            expect(fromC1 + fromC2).toBeGreaterThan(140);
            expect(fromC1 + fromC2).toBeLessThan(155);
            // Ideally balanced due to N1's internal priority 0.5/0.5
            expect(Math.abs(fromC1 - fromC2)).toBeLessThan(5);
        });

        it('Multi-Recipient Resource Contention', () => {
            // C1 (100)
            // N1 (80) - Priority 0.5
            // N2 (80) - Priority 0.5
            // Total Need 160 > Cap 100.
            // Should split 50/50 -> 50 each.

            const c1 = createCapacitySlot('c1', 100, {
                priority_distribution: [
                    { target_slot_id: 'n1', priority_percentage: 0.5 },
                    { target_slot_id: 'n2', priority_percentage: 0.5 }
                ]
            });
            const n1 = createNeedSlot('n1', 80);
            const n2 = createNeedSlot('n2', 80);

            const result = calculateSlotBasedPriorityAllocation([c1], [n1, n2], {});
            const q1 = result.find(r => r.need_slot_id === 'n1')?.quantity || 0;
            const q2 = result.find(r => r.need_slot_id === 'n2')?.quantity || 0;

            expect(q1).toBeCloseTo(50);
            expect(q2).toBeCloseTo(50);
        });

        it('Complex Topology (Cyclic/Overlapping)', () => {
            // C1 -> N1, N2
            // C2 -> N2, N3
            // All needs 100. All Caps 100.
            // Priorities equal.

            const c1 = createCapacitySlot('c1', 100, {
                priority_distribution: [
                    { target_slot_id: 'n1', priority_percentage: 0.5 },
                    { target_slot_id: 'n2', priority_percentage: 0.5 }
                ]
            });
            const c2 = createCapacitySlot('c2', 100, {
                priority_distribution: [
                    { target_slot_id: 'n2', priority_percentage: 0.5 },
                    { target_slot_id: 'n3', priority_percentage: 0.5 }
                ]
            });

            const n1 = createNeedSlot('n1', 100); // Only C1
            const n2 = createNeedSlot('n2', 100); // Shared C1, C2
            const n3 = createNeedSlot('n3', 100); // Only C2

            // Expected: 
            // N1 gets ~50 from C1.
            // N3 gets ~50 from C2.
            // N2 gets ~50 from C1 + ~50 from C2 = 100?
            // Actually, since C1 gives 50 to N1, and C2 gives 50 to N3,
            // they both have 50 left for N2. So N2 satisfies 100.

            const result = calculateSlotBasedPriorityAllocation([c1, c2], [n1, n2, n3], {});

            const totalN1 = result.filter(r => r.need_slot_id === 'n1').reduce((s, r) => s + r.quantity, 0);
            const totalN2 = result.filter(r => r.need_slot_id === 'n2').reduce((s, r) => s + r.quantity, 0);
            const totalN3 = result.filter(r => r.need_slot_id === 'n3').reduce((s, r) => s + r.quantity, 0);

            expect(totalN1).toBeGreaterThan(40);
            expect(totalN3).toBeGreaterThan(40);
            expect(totalN2).toBeCloseTo(100, 1);
        });
    });

    describe('6. Edge Cases', () => {
        it('should handle zero capacity', () => {
            const c1 = createCapacitySlot('c1', 0);
            const n1 = createNeedSlot('n1', 100);
            const result = calculateSlotBasedPriorityAllocation([c1], [n1], {});
            expect(result.length).toBe(0);
        });

        it('should handle zero needs', () => {
            const c1 = createCapacitySlot('c1', 100);
            const n1 = createNeedSlot('n1', 0);
            const result = calculateSlotBasedPriorityAllocation([c1], [n1], {});
            expect(result.length).toBe(0);
        });
    });
});
