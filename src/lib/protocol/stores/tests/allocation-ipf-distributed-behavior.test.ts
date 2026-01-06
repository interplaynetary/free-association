
import { describe, it, expect } from 'vitest';
import {
    updateProviderState,
    updateRecipientState,
    generateFlowProposals,
    type DistributedIPFState,
    type FlowProposal
} from '../../allocation-ipf-distributed';
import type { AvailabilitySlot, NeedSlot, Commitment } from '../../schemas';

// ═══════════════════════════════════════════════════════════════════
// HELPER FACTORIES
// ═══════════════════════════════════════════════════════════════════

const createCapacitySlot = (id: string, quantity: number, overrides: Partial<AvailabilitySlot> = {}): AvailabilitySlot => ({
    id,
    name: `Capacity ${id}`,
    quantity,
    type_id: 'type1',
    // Default to equal interest if not specified, or allow specific priorities
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
    global_recognition_weights: {}, // Default recognition (1.0 implied for unlisted in some logic, but usually 0 if strict)
    ...overrides
});

// Helper to run simulation loop
function runSimulation(
    commitments: Record<string, Commitment>,
    slots: { capacity: AvailabilitySlot[], needs: NeedSlot[] },
    iterations: number = 20,
    gamma: number = 0.5
) {
    // Initialize states
    const states: Record<string, DistributedIPFState> = {};

    // Providers
    for (const slot of slots.capacity) {
        // Assume provider ID is roughly derived from pubkey for this sim. 
        // In reality, state is per-agent. We'll track state by Agent Pubkey.
        // We need to know which agent owns which slot. 
        // Let's assume the commitments map tells us ownership.
        // But for simplicity in this helper, we'll track state by 'p1', 'r1' keys matching commitments.
    }

    // Actually, distinct states per agent is better.
    // Let's deduce agent IDs from commitments for the slots.
    const agentBySlotId: Record<string, string> = {};
    for (const [agentId, comm] of Object.entries(commitments)) {
        if (!states[agentId]) {
            states[agentId] = { rowScalings: {}, colScalings: {}, cachedRemoteScalings: {}, totalSeedsByNeed: {} };
        }
        comm.capacity_slots?.forEach(s => agentBySlotId[s.id] = agentId);
        comm.need_slots?.forEach(s => agentBySlotId[s.id] = agentId);

        // Init row/col scalings
        comm.capacity_slots?.forEach(s => states[agentId].rowScalings[s.id] = 1.0);
        comm.need_slots?.forEach(s => states[agentId].colScalings[s.id] = 1.0);
    }

    for (let i = 0; i < iterations; i++) {
        // 1. Providers Update & Generate
        const allProposals: FlowProposal[] = [];
        const providerAgents = new Set(slots.capacity.map(s => agentBySlotId[s.id]));

        for (const pid of providerAgents) {
            const providerSlots = commitments[pid].capacity_slots || [];
            // Provider sees ALL needs in the network (in this full-knowledge sim)
            states[pid] = updateProviderState(providerSlots, slots.needs, commitments, states[pid], 1e-6, gamma);
            const proposals = generateFlowProposals(providerSlots, slots.needs, commitments, states[pid], 1e-6, gamma);
            allProposals.push(...proposals);
        }

        // 2. Recipients Update
        const recipientAgents = new Set(slots.needs.map(s => agentBySlotId[s.id]));

        for (const rid of recipientAgents) {
            const recipientNeeds = commitments[rid].need_slots || [];
            // Filter proposals for this recipient
            const relevantProposals = allProposals.filter(p => recipientNeeds.some(n => n.id === p.need_slot_id));
            states[rid] = updateRecipientState(recipientNeeds, relevantProposals, states[rid]);
        }

        // 3. Network Sync (Broadcasting Scalings)
        // In distributed system, P reads R's scaling from R's commitment/message.
        // Here we instantly propagate R's colScalings to P's cachedRemoteScalings.
        for (const pid of providerAgents) {
            for (const rid of recipientAgents) {
                const rState = states[rid];
                // P caches all col scalings from R
                for (const [needId, scale] of Object.entries(rState.colScalings)) {
                    states[pid].cachedRemoteScalings[needId] = scale;

                    // Also propagate seeds if needed for some logic
                    if (rState.totalSeedsByNeed[needId]) {
                        states[pid].totalSeedsByNeed[needId] = rState.totalSeedsByNeed[needId];
                    }
                }
            }
        }
    }

    // Generate final proposals
    const finalProposals: FlowProposal[] = [];
    const providerAgents = new Set(slots.capacity.map(s => agentBySlotId[s.id]));
    for (const pid of providerAgents) {
        const providerSlots = commitments[pid].capacity_slots || [];
        finalProposals.push(...generateFlowProposals(providerSlots, slots.needs, commitments, states[pid], 1e-6, gamma));
    }
    return finalProposals;
}

// ═══════════════════════════════════════════════════════════════════
// TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Distributed IPF Behavior Specification', () => {

    it('Scenario 1: Baseline Convergence (1 Provider, 1 Need, Equal Capacity)', () => {
        const c1 = createCapacitySlot('c1', 100, { priority_distribution: { 'r1': 1 } });
        const n1 = createNeedSlot('n1', 100, { priority_distribution: { 'p1': 1 } });

        const commitments = {
            'p1': createCommitment('p1', { capacity_slots: [c1] }),
            'r1': createCommitment('r1', { need_slots: [n1] })
        };

        const proposals = runSimulation(commitments, { capacity: [c1], needs: [n1] });
        const flow = proposals[0].proposed_quantity;

        expect(flow).toBeCloseTo(100, 0);
    });

    it('Scenario 2: Proportional Fairness - Provider Priority', () => {
        // P1(100) -> N1 (High Priority 0.8), N2 (Low Priority 0.2)
        // Both Needs are large enough (100 each) to absorb the flow.

        const c1 = createCapacitySlot('c1', 100, {
            // Normalized Priority: r1=0.8, r2=0.2
            priority_distribution: { 'r1': 0.8, 'r2': 0.2 }
        });
        const n1 = createNeedSlot('n1', 100, { priority_distribution: { 'p1': 1 } });
        const n2 = createNeedSlot('n2', 100, { priority_distribution: { 'p1': 1 } });

        const commitments = {
            'p1': createCommitment('p1', { capacity_slots: [c1] }),
            'r1': createCommitment('r1', { need_slots: [n1] }),
            'r2': createCommitment('r2', { need_slots: [n2] })
        };

        const proposals = runSimulation(commitments, { capacity: [c1], needs: [n1, n2] });

        const flowToN1 = proposals.find(p => p.need_slot_id === 'n1')?.proposed_quantity || 0;
        const flowToN2 = proposals.find(p => p.need_slot_id === 'n2')?.proposed_quantity || 0;

        // Expect roughly 80/20 split
        expect(flowToN1).toBeCloseTo(80, 0);
        expect(flowToN2).toBeCloseTo(20, 0);
    });

    it('Scenario 3: Proportional Fairness - Recipient Preference', () => {
        // P1(100), P2(100) -> R1(100)
        // R1 prefers P1 (0.8) over P2 (0.2)
        // Note: Recipient preference is typically expressed via `global_recognition_weights` or specific slot priority in this system.
        // Assuming the protocol uses `global_recognition_weights` for this "gamma" factor or `priority_distribution` on the need slot?
        // Looking at schema/implementation: NeedSlot has `priority_distribution` mapping provider_ids to weights.

        const c1 = createCapacitySlot('c1', 100, { priority_distribution: { 'r1': 1 } });
        const c2 = createCapacitySlot('c2', 100, { priority_distribution: { 'r1': 1 } });

        const n1 = createNeedSlot('n1', 100, {
            // Recipient Preference: p1=0.8, p2=0.2
            priority_distribution: { 'p1': 0.8, 'p2': 0.2 }
        });

        const commitments = {
            'p1': createCommitment('p1', { capacity_slots: [c1] }),
            'p2': createCommitment('p2', { capacity_slots: [c2] }),
            'r1': createCommitment('r1', { need_slots: [n1] })
        };

        // Use gamma=1.0 for linear proportionality
        const proposals = runSimulation(commitments, { capacity: [c1, c2], needs: [n1] }, 20, 1.0);

        const flowFromP1 = proposals.find(p => p.provider_pubkey === 'p1')?.proposed_quantity || 0;
        const flowFromP2 = proposals.find(p => p.provider_pubkey === 'p2')?.proposed_quantity || 0;

        // Total Demand = 100. Supply = 200.
        // Recipient constraint acts as the bottleneck.
        // W-IPF should respect the seeded preference ratios in the column scaling.
        // Expected: P1 gets ~80, P2 gets ~20.

        expect(flowFromP1 + flowFromP2).toBeCloseTo(100, 0); // Need satisfied

        // This checks if the Recipient's "Pull" preference effectively allocates the scarcity (Need capacity)
        expect(flowFromP1).toBeCloseTo(80, 0);
        expect(flowFromP2).toBeCloseTo(20, 0);
    });

    it('Scenario 4: Displacement (Hydraulic Equilibrium)', () => {
        // P_Low(100) -> Need(10). Initially fills it.
        // P_High(100) -> Need(10). Enters.
        // Expect P_High to displace P_Low.

        const cLow = createCapacitySlot('c_low', 100, { priority_distribution: { 'r1': 0.2 } });
        const cHigh = createCapacitySlot('c_high', 100, { priority_distribution: { 'r1': 0.8 } });
        const n1 = createNeedSlot('n1', 10, { priority_distribution: { 'p_low': 1, 'p_high': 1 } });

        const commitments = {
            'p_low': createCommitment('p_low', { capacity_slots: [cLow] }),
            'p_high': createCommitment('p_high', { capacity_slots: [cHigh] }),
            'r1': createCommitment('r1', { need_slots: [n1] })
        };

        const proposals = runSimulation(commitments, { capacity: [cLow, cHigh], needs: [n1] }, 30);

        const flowLow = proposals.find(p => p.provider_pubkey === 'p_low')?.proposed_quantity || 0;
        const flowHigh = proposals.find(p => p.provider_pubkey === 'p_high')?.proposed_quantity || 0;

        // Total Need = 10.
        // Ratio of Priorities: 0.8 vs 0.2 (4:1) => Seed Ratio sqrt(4):1 = 2:1 (if gamma=0.5)
        // Or if we use priorities as-is? 
        // Seed = (Priority + eps) * (1)^gamma.
        // So Seed Ratio IS Priority Ratio (4:1).

        // Expected: P_High gets ~8, P_Low gets ~2.

        expect(flowLow + flowHigh).toBeCloseTo(10, 0); // Need cap respected
        expect(flowHigh).toBeGreaterThan(6); // Significant majority
        expect(flowLow).toBeLessThan(4); // Displaced
    });

    it('Scenario 5: Hidden Demand Discovery (Epsilon Activation)', () => {
        // P(100) has Priority 0 for N(100).
        // Should still allocate due to epsilon if no better options.

        const c1 = createCapacitySlot('c1', 100, { priority_distribution: { 'r1': 0.0 } }); // Explicit 0 priority
        const n1 = createNeedSlot('n1', 100);

        const commitments = {
            'p1': createCommitment('p1', { capacity_slots: [c1] }),
            'r1': createCommitment('r1', { need_slots: [n1] })
        };

        const proposals = runSimulation(commitments, { capacity: [c1], needs: [n1] });
        const flow = proposals[0]?.proposed_quantity || 0;

        // Even with priority 0, epsilon ensures connectivity
        // Since it's the ONLY need, Row Scaling will scale that tiny seed up to full capacity (100)
        expect(flow).toBeCloseTo(100, 0);
    });

    it('Scenario 6: Global Clamping (Distributed Constraint Enforcement)', () => {
        // P1(80), P2(80) -> R(100).
        // Total Supply 160 > Need 100.
        // Both should be scaled down.

        const c1 = createCapacitySlot('c1', 80, { priority_distribution: { 'r1': 1 } });
        const c2 = createCapacitySlot('c2', 80, { priority_distribution: { 'r1': 1 } });
        const n1 = createNeedSlot('n1', 100);

        const commitments = {
            'p1': createCommitment('p1', { capacity_slots: [c1] }),
            'p2': createCommitment('p2', { capacity_slots: [c2] }),
            'r1': createCommitment('r1', { need_slots: [n1] })
        };

        const proposals = runSimulation(commitments, { capacity: [c1, c2], needs: [n1] });

        const flow1 = proposals.find(p => p.provider_pubkey === 'p1')?.proposed_quantity || 0;
        const flow2 = proposals.find(p => p.provider_pubkey === 'p2')?.proposed_quantity || 0;

        expect(flow1 + flow2).toBeCloseTo(100, 0);
        // Equal priority/preference => Equal split
        expect(flow1).toBeCloseTo(50, 0);
        expect(flow2).toBeCloseTo(50, 0);
    });

    it('Scenario 7: Multi-Dimensional Constraints (Cycle Convergence)', () => {
        // The X-Shape / Cycle
        // P1(100) -> R1(100), R2(100)
        // P2(100) -> R2(100), R3(100)
        // R2 is the bottleneck (200 supply, 100 demand)
        // R1, R3 are sinks.
        // Expected: P1 sends ~50 to R1, ~50 to R2. P2 sends ~50 to R2, ~50 to R3.

        const c1 = createCapacitySlot('c1', 100, { priority_distribution: { 'r1': 1, 'r2': 1 } });
        const c2 = createCapacitySlot('c2', 100, { priority_distribution: { 'r2': 1, 'r3': 1 } });

        const n1 = createNeedSlot('n1', 100, { priority_distribution: { 'p1': 1 } });
        const n2 = createNeedSlot('n2', 100, { priority_distribution: { 'p1': 1, 'p2': 1 } });
        const n3 = createNeedSlot('n3', 100, { priority_distribution: { 'p2': 1 } });

        const commitments = {
            'p1': createCommitment('p1', { capacity_slots: [c1] }),
            'p2': createCommitment('p2', { capacity_slots: [c2] }),
            'r1': createCommitment('r1', { need_slots: [n1] }),
            'r2': createCommitment('r2', { need_slots: [n2] }),
            'r3': createCommitment('r3', { need_slots: [n3] })
        };

        const proposals = runSimulation(commitments, { capacity: [c1, c2], needs: [n1, n2, n3] }, 50);

        const flowN1 = proposals.find(p => p.need_slot_id === 'n1')?.proposed_quantity || 0;
        const flowN3 = proposals.find(p => p.need_slot_id === 'n3')?.proposed_quantity || 0;

        const flowN2_P1 = proposals.find(p => p.need_slot_id === 'n2' && p.provider_pubkey === 'p1')?.proposed_quantity || 0;
        const flowN2_P2 = proposals.find(p => p.need_slot_id === 'n2' && p.provider_pubkey === 'p2')?.proposed_quantity || 0;

        // R2 constraint (100) forces P1 and P2 to divert half their capacity to R1 and R3 respectively
        expect(flowN2_P1 + flowN2_P2).toBeCloseTo(100, 0);
        expect(flowN1).toBeCloseTo(50, 0);
        expect(flowN3).toBeCloseTo(50, 0);
    });

    it('Scenario 8: Self-Allocation (Pure Self-Sufficiency)', () => {
        // Agent is both provider AND recipient
        // Should allocate 100% to self (full need satisfaction)
        const c1 = createCapacitySlot('c1', 100);
        const n1 = createNeedSlot('n1', 100);

        const commitments = {
            'agent1': createCommitment('agent1', {
                capacity_slots: [c1],
                need_slots: [n1],
                global_recognition_weights: { 'agent1': 0.3696 } // Self-recognition
            })
        };

        const proposals = runSimulation(commitments, { capacity: [c1], needs: [n1] });
        const flow = proposals[0]?.proposed_quantity || 0;

        // Should allocate 100% to self (full need satisfaction)
        expect(flow).toBeCloseTo(100, 0);
    });

    it('Scenario 9: Self-Allocation with External Provider (Recognition-Based)', () => {
        // THE USER'S EXACT SCENARIO
        // Needer is both provider AND recipient
        // Other provider also provides
        // Recognition weights determine allocation ratio
        const cNeeder = createCapacitySlot('c_needer', 100);
        const cOther = createCapacitySlot('c_other', 100);
        const n1 = createNeedSlot('n1', 100);

        const commitments = {
            'needer': createCommitment('needer', {
                capacity_slots: [cNeeder],
                need_slots: [n1],
                global_recognition_weights: {
                    'needer': 0.3696,  // Self-recognition
                    'other': 0.1537    // Recognition of other
                }
            }),
            'other': createCommitment('other', {
                capacity_slots: [cOther],
                global_recognition_weights: {
                    'needer': 1.0  // Full recognition of needer
                }
            })
        };

        const proposals = runSimulation(commitments, { capacity: [cNeeder, cOther], needs: [n1] });

        const flowFromNeeder = proposals.find(p => p.provider_pubkey === 'needer')?.proposed_quantity || 0;
        const flowFromOther = proposals.find(p => p.provider_pubkey === 'other')?.proposed_quantity || 0;
        const totalFlow = flowFromNeeder + flowFromOther;

        console.log(`[TEST-SCENARIO-9] Needer provides: ${flowFromNeeder.toFixed(2)}, Other provides: ${flowFromOther.toFixed(2)}, Total: ${totalFlow.toFixed(2)}`);

        // CRITICAL ASSERTIONS:
        // 1. Full need satisfaction (not 59.95!)
        expect(totalFlow).toBeCloseTo(100, 1);

        // 2. Allocation ratio matches recognition ratio
        // Recognition ratio: 0.3696 / (0.3696 + 0.1537) ≈ 0.706 (70.6%)
        const expectedNeederRatio = 0.3696 / (0.3696 + 0.1537);
        const actualNeederRatio = flowFromNeeder / totalFlow;

        console.log(`[TEST-SCENARIO-9] Expected needer ratio: ${(expectedNeederRatio * 100).toFixed(1)}%, Actual: ${(actualNeederRatio * 100).toFixed(1)}%`);

        expect(actualNeederRatio).toBeCloseTo(expectedNeederRatio, 1);
        expect(flowFromNeeder).toBeCloseTo(70.6, 1); // ~71
        expect(flowFromOther).toBeCloseTo(29.4, 1);  // ~29
    });

    it('Scenario 10: Full Capacity Utilization (Abundance)', () => {
        // Total capacity (200) > Total need (100)
        // Should satisfy need 100%, not under-allocate due to fair-share bug
        const c1 = createCapacitySlot('c1', 100);
        const c2 = createCapacitySlot('c2', 100);
        const n1 = createNeedSlot('n1', 100);

        const commitments = {
            'p1': createCommitment('p1', { capacity_slots: [c1] }),
            'p2': createCommitment('p2', { capacity_slots: [c2] }),
            'r1': createCommitment('r1', { need_slots: [n1] })
        };

        const proposals = runSimulation(commitments, { capacity: [c1, c2], needs: [n1] });
        const totalFlow = proposals.reduce((sum, p) => sum + p.proposed_quantity, 0);

        // Should satisfy 100% of need (not under-allocate)
        expect(totalFlow).toBeCloseTo(100, 0);
    });

});
