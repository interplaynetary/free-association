
import { describe, it, expect } from 'vitest';
import {
    updateProviderState,
    updateRecipientState,
    generateFlowProposals,
    type DistributedIPFState,
    type FlowProposal
} from './allocation-ipf-distributed';
import type { AvailabilitySlot, NeedSlot, Commitment } from '@playnet/free-association/schemas';

// --- MOCK FACTORIES (Copied from centralized test for consistency) ---

const createCapacitySlot = (id: string, quantity: number, overrides: Partial<AvailabilitySlot> = {}): AvailabilitySlot => ({
    id,
    name: `Capacity ${id}`,
    quantity,
    type_id: 'type1',
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

describe('Distributed IPF Allocation (Simulation)', () => {

    it('should converge to the same result as centralized: 2 Providers -> 2 Recipients', () => {
        // SCENARIO: The "X" Shape (Cycle)
        // P1 (100) -> R1 (Priority 1), R2 (Priority 1)
        // P2 (100) -> R2 (Priority 1), R3 (Priority 1)
        // Needs: R1(100), R2(100), R3(100)

        // Expected Centralized Result:
        // R1: 50 (from P1)
        // R2: 100 (50 from P1, 50 from P2)
        // R3: 50 (from P2)

        // --- SETUP ---
        const cs1 = createCapacitySlot('c1', 100, { priority_distribution: { 'r1': 1, 'r2': 1 } });
        const cs2 = createCapacitySlot('c2', 100, { priority_distribution: { 'r2': 1, 'r3': 1 } });

        const n1 = createNeedSlot('n1', 100, { priority_distribution: { 'p1': 1 } });
        const n2 = createNeedSlot('n2', 100, { priority_distribution: { 'p1': 1, 'p2': 1 } });
        const n3 = createNeedSlot('n3', 100, { priority_distribution: { 'p2': 1 } });

        const commitments = {
            'p1': createCommitment('p1', { capacity_slots: [cs1] }),
            'p2': createCommitment('p2', { capacity_slots: [cs2] }),
            'r1': createCommitment('r1', { need_slots: [n1] }),
            'r2': createCommitment('r2', { need_slots: [n2] }),
            'r3': createCommitment('r3', { need_slots: [n3] })
        };

        // --- AGENT STATE INITIALIZATION ---
        // P1 State
        let p1State: DistributedIPFState = {
            rowScalings: { 'c1': 1.0 },
            colScalings: {},
            cachedRemoteScalings: { 'n1': 1.0, 'n2': 1.0 }, // Assume everyone open initially
            totalSeedsByNeed: {} // Will be populated by recipients
        };

        // P2 State
        let p2State: DistributedIPFState = {
            rowScalings: { 'c2': 1.0 },
            colScalings: {},
            cachedRemoteScalings: { 'n2': 1.0, 'n3': 1.0 },
            totalSeedsByNeed: {}
        };

        // R1, R2, R3 State (Receivers only)
        let r1State: DistributedIPFState = { rowScalings: {}, colScalings: { 'n1': 1.0 }, cachedRemoteScalings: {}, totalSeedsByNeed: {} };
        let r2State: DistributedIPFState = { rowScalings: {}, colScalings: { 'n2': 1.0 }, cachedRemoteScalings: {}, totalSeedsByNeed: {} };
        let r3State: DistributedIPFState = { rowScalings: {}, colScalings: { 'n3': 1.0 }, cachedRemoteScalings: {}, totalSeedsByNeed: {} };

        // --- SIMULATION LOOP ---
        const iterations = 50;
        let lastDiff = 0;

        for (let i = 0; i < iterations; i++) {
            // 1. Providers Update & Generate Proposals
            // P1 sees R1, R2
            p1State = updateProviderState([cs1], [n1, n2], commitments, p1State);
            const p1Proposals = generateFlowProposals([cs1], [n1, n2], commitments, p1State);

            // P2 sees R2, R3
            p2State = updateProviderState([cs2], [n2, n3], commitments, p2State);
            const p2Proposals = generateFlowProposals([cs2], [n2, n3], commitments, p2State);

            // 2. Network Transmission (Aggregating Proposals)
            // Need N1 receives from P1
            const propsForN1 = p1Proposals.filter(p => p.need_slot_id === 'n1');

            // Need N2 receives from P1 + P2
            const propsForN2 = [
                ...p1Proposals.filter(p => p.need_slot_id === 'n2'),
                ...p2Proposals.filter(p => p.need_slot_id === 'n2')
            ];

            // Need N3 receives from P2
            const propsForN3 = p2Proposals.filter(p => p.need_slot_id === 'n3');

            // 3. Recipients Update State
            r1State = updateRecipientState([n1], propsForN1, r1State);
            r2State = updateRecipientState([n2], propsForN2, r2State);
            r3State = updateRecipientState([n3], propsForN3, r3State);

            // 4. Network Transmission (Broadcasting Scalings)
            // P1 hears from R1, R2
            p1State.cachedRemoteScalings['n1'] = r1State.colScalings['n1'];
            p1State.cachedRemoteScalings['n2'] = r2State.colScalings['n2'];

            // P2 hears from R2, R3
            p2State.cachedRemoteScalings['n2'] = r2State.colScalings['n2'];
            p2State.cachedRemoteScalings['n3'] = r3State.colScalings['n3'];
        }

        // --- VERIFICATION ---
        // Calculate Final Flows based on final states
        const finalP1 = generateFlowProposals([cs1], [n1, n2], commitments, p1State);
        const finalP2 = generateFlowProposals([cs2], [n2, n3], commitments, p2State);

        const flowN1 = finalP1.find(p => p.need_slot_id === 'n1')?.proposed_quantity || 0;
        const flowN2_fromP1 = finalP1.find(p => p.need_slot_id === 'n2')?.proposed_quantity || 0;
        const flowN2_fromP2 = finalP2.find(p => p.need_slot_id === 'n2')?.proposed_quantity || 0;
        const flowN3 = finalP2.find(p => p.need_slot_id === 'n3')?.proposed_quantity || 0;

        console.log('Final Distributed Flows:', {
            N1: flowN1,
            N2: flowN2_fromP1 + flowN2_fromP2,
            N3: flowN3
        });

        // N1 should be close to 50
        expect(flowN1).toBeCloseTo(50, 0);

        // N2 should be close to 100
        expect(flowN2_fromP1 + flowN2_fromP2).toBeCloseTo(100, 0);

        // N3 should be close to 50
        expect(flowN3).toBeCloseTo(50, 0);
    });

    it('should handle single provider over-demand correctly', () => {
        // P1(100) -> N1(100), N2(100).
        // Should split 50/50.
        const cs = createCapacitySlot('c1', 100, { priority_distribution: { 'r1': 1, 'r2': 1 } });
        const n1 = createNeedSlot('n1', 100, { priority_distribution: { 'p1': 1 } });
        const n2 = createNeedSlot('n2', 100, { priority_distribution: { 'p1': 1 } });

        const commitments = {
            'p1': createCommitment('p1', { capacity_slots: [cs] }),
            'r1': createCommitment('r1', { need_slots: [n1] }),
            'r2': createCommitment('r2', { need_slots: [n2] })
        };

        let pState: DistributedIPFState = { rowScalings: { 'c1': 1 }, colScalings: {}, cachedRemoteScalings: {}, totalSeedsByNeed: {} };
        let r1State: DistributedIPFState = { rowScalings: {}, colScalings: { 'n1': 1 }, cachedRemoteScalings: {}, totalSeedsByNeed: {} };
        let r2State: DistributedIPFState = { rowScalings: {}, colScalings: { 'n2': 1 }, cachedRemoteScalings: {}, totalSeedsByNeed: {} };

        for (let i = 0; i < 20; i++) {
            pState = updateProviderState([cs], [n1, n2], commitments, pState);
            const props = generateFlowProposals([cs], [n1, n2], commitments, pState);

            r1State = updateRecipientState([n1], props.filter(p => p.need_slot_id === 'n1'), r1State);
            r2State = updateRecipientState([n2], props.filter(p => p.need_slot_id === 'n2'), r2State);

            pState.cachedRemoteScalings['n1'] = r1State.colScalings['n1'];
            pState.cachedRemoteScalings['n2'] = r2State.colScalings['n2'];
        }

        const finalProps = generateFlowProposals([cs], [n1, n2], commitments, pState);
        const q1 = finalProps.find(p => p.need_slot_id === 'n1')?.proposed_quantity || 0;
        const q2 = finalProps.find(p => p.need_slot_id === 'n2')?.proposed_quantity || 0;

        expect(q1).toBeCloseTo(50);
        expect(q2).toBeCloseTo(50);
    });

    it('should not over-allocate when single provider has excess capacity', () => {
        // CRITICAL TEST: P1(200) -> R1(50)
        // Expected: Allocation should converge to 50 (need satisfaction), NOT 200
        // This is the bug reported in production logs
        const cs = createCapacitySlot('c1', 200, { priority_distribution: { 'r1': 1 } });
        const n1 = createNeedSlot('n1', 50, { priority_distribution: { 'p1': 1 } });

        const commitments = {
            'p1': createCommitment('p1', { capacity_slots: [cs] }),
            'r1': createCommitment('r1', { need_slots: [n1] })
        };

        let pState: DistributedIPFState = { rowScalings: { 'c1': 1 }, colScalings: {}, cachedRemoteScalings: {}, totalSeedsByNeed: {} };
        let r1State: DistributedIPFState = { rowScalings: {}, colScalings: { 'n1': 1 }, cachedRemoteScalings: {}, totalSeedsByNeed: {} };

        // Simulate convergence
        for (let i = 0; i < 20; i++) {
            pState = updateProviderState([cs], [n1], commitments, pState);
            const props = generateFlowProposals([cs], [n1], commitments, pState);

            r1State = updateRecipientState([n1], props, r1State);
            pState.cachedRemoteScalings['n1'] = r1State.colScalings['n1'];
            pState.totalSeedsByNeed['n1'] = r1State.totalSeedsByNeed['n1'];

            // Debug logging for first few iterations
            if (i < 5) {
                const currentFlow = props.find(p => p.need_slot_id === 'n1')?.proposed_quantity || 0;
                console.log(`Iteration ${i}: y_r=${r1State.colScalings['n1']?.toFixed(4)}, x_p=${pState.rowScalings['c1']?.toFixed(4)}, flow=${currentFlow.toFixed(2)}`);
            }
        }

        const finalProps = generateFlowProposals([cs], [n1], commitments, pState);
        const finalFlow = finalProps.find(p => p.need_slot_id === 'n1')?.proposed_quantity || 0;

        console.log('Final state:', {
            y_r: r1State.colScalings['n1'],
            x_p: pState.rowScalings['c1'],
            finalFlow,
            expected: 50
        });

        // CRITICAL ASSERTION: Should allocate exactly the need (50), not the full capacity (200)
        expect(finalFlow).toBeCloseTo(50, 0);
        expect(finalFlow).toBeLessThanOrEqual(50.1); // Allow tiny numerical error
    });

    it('should under-allocate if totalSeeds is inflated (Ghost Provider scenario)', () => {
        // SCENARIO: P1(100) -> R1(50).
        // But R1 reports totalSeed = 2.0 (as if P2 is there), but P2 is missing/not sending.
        // P1 should calculate fairShare = (1/2)*50 = 25.
        // P1 sends 25.
        // R1 receives 25. Total < Need.

        const cs = createCapacitySlot('c1', 100, { priority_distribution: { 'r1': 1 } });
        const n1 = createNeedSlot('n1', 50, { priority_distribution: { 'p1': 1 } });

        const commitments = {
            'p1': createCommitment('p1', { capacity_slots: [cs] }),
            'r1': createCommitment('r1', { need_slots: [n1] })
        };

        let pState: DistributedIPFState = { rowScalings: { 'c1': 1 }, colScalings: {}, cachedRemoteScalings: {}, totalSeedsByNeed: {} };
        let r1State: DistributedIPFState = { rowScalings: {}, colScalings: { 'n1': 1 }, cachedRemoteScalings: {}, totalSeedsByNeed: { 'n1': 2.0 } }; // INFLATED SEED

        // Simulate loop where R1 keeps reporting inflated seed
        for (let i = 0; i < 10; i++) {
            // Provider sees inflated seed
            pState.totalSeedsByNeed['n1'] = 2.0;

            pState = updateProviderState([cs], [n1], commitments, pState);
            const props = generateFlowProposals([cs], [n1], commitments, pState);

            // Recipient update normally sums incoming seeds. 
            // BUT here we force the "Ghost" to remain in the state to simulate the bug.
            const realRState = updateRecipientState([n1], props, r1State);
            r1State = {
                ...realRState,
                // Force ghost seed persistence
                totalSeedsByNeed: { 'n1': realRState.totalSeedsByNeed['n1'] + 1.0 }
            };

            pState.cachedRemoteScalings['n1'] = r1State.colScalings['n1'];
        }

        const finalProps = generateFlowProposals([cs], [n1], commitments, pState);
        const finalFlow = finalProps.find(p => p.need_slot_id === 'n1')?.proposed_quantity || 0;

        console.log('Ghost Provider Scenario - Final Flow:', finalFlow);

        // Expectation: Under-allocation occurs
        // P1 fairShare = 25. Capacity=100. Target=25.
        // x_p targets 25. 
        // y_r calculation:
        //   R1 receives 25. Need 50. y_r = 50/25 = 2.0.
        // P1 sees y_r = 2.0.
        // P1 denominator = k*y_r = 1*2 = 2.
        // P1 target = 25.
        // x_p = 25/2 = 12.5.
        // Allocation = k * x_p * y_r = 1 * 12.5 * 2 = 25.

        // It stabilizes at 25 (Half of need).
        expect(finalFlow).toBeCloseTo(25, 0);
    });
});
