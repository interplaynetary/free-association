/**
 * Distributed IPF Allocation Protocol
 * 
 * Implements the "Distributed Recipient Broadcast" protocol where agents
 * coordinate asynchronously by exchanging scaling factors.
 * 
 * Unlike `allocation-ipf.ts` (which is a centralized solver), this module
 * provides functions for a single agent to:
 * 1. Process incoming "Constraint Factors" (y_r).
 * 2. Update their own "Row Scaling" (x_p).
 * 3. Calculate outgoing flow proposals.
 * 4. Process incoming flow proposals.
 * 5. Update their own "Constraint Factor" (y_r).
 * 
 * Mathematical Basis:
 * A_pr = K_pr * x_p * y_r
 * 
 * - Provider (p) controls x_p (Row Scaling) to satisfy Capacity.
 * - Recipient (r) controls y_r (Column Scaling) to satisfy Need.
 */

import type {
    AvailabilitySlot,
    NeedSlot,
    Commitment
} from './schemas.js';

import {
    calculateSeedValue,
    calculateScalingFactor,
    calculateConstraintFactor,
    findOwner
} from './ipf-core.js';

// ═══════════════════════════════════════════════════════════════════
// TYPES
// ═══════════════════════════════════════════════════════════════════

export interface DistributedIPFState {
    /** 
     * My Row Scaling Factors (x_p)
     * How much I need to scale my offers to stay within capacity.
     * x_p = Capacity / Sum(K_pr * y_r)
     */
    rowScalings: Record<string, number>; // capacity_slot_id -> x_p

    /**
     * My Column Scaling Factors (y_r)
     * How much others need to scale their offers to not overflow my need.
     * y_r = min(1, Need / Sum(IncomingFlow_proposed))
     */
    colScalings: Record<string, number>; // need_slot_id -> y_r

    /**
     * Cache of others' scaling factors (y_r) received from network.
     * This represents my view of the recipients' state.
     */
    cachedRemoteScalings: Record<string, number>; // recipient_need_slot_id -> y_r

    /**
     * Cache of total seed values (Σ K_pr) from recipients.
     * Published by recipients to enable priority-aware fair-share calculation.
     * Allows providers to calculate: fairShare = (my_K_pr / total_seed) × need
     */
    totalSeedsByNeed: Record<string, number>; // need_slot_id -> total_seed
}

export interface FlowProposal {
    capacity_slot_id: string;
    need_slot_id: string;
    provider_pubkey: string;  // ME
    recipient_pubkey: string; // THEM
    proposed_quantity: number; // K_pr * x_p * y_r

    /**
     * Seed value (K_pr) for this proposal.
     * Recipient aggregates these to calculate total_seed for competition metrics.
     * Enables priority-aware allocation without provider-to-provider subscriptions.
     */
    seed_value: number; // K_pr = (ProviderPriority + ε) × (RecipientPreference + ε)^γ
}

// ═══════════════════════════════════════════════════════════════════
// PROVIDER LOGIC
// ═══════════════════════════════════════════════════════════════════

/**
 * Step 1: Provider Update (Row Scaling)
 * 
 * Given my capacities and the latest signals (y_r) from recipients,
 * update my row scaling factors (x_p).
 * 
 * x_p = C_p / Σ_r (K_pr * y_r)
 */
export function updateProviderState(
    capacitySlots: AvailabilitySlot[],
    knownNeeds: NeedSlot[], // All needs I am aware of (my "Local View")
    allCommitments: Record<string, Commitment>,
    state: DistributedIPFState,
    epsilon: number = 1e-6,
    gamma: number = 0.5
): DistributedIPFState {
    const nextState = { ...state, rowScalings: {} as Record<string, number> }; // Start fresh - only add scalings for current capacity slots

    for (const cs of capacitySlots) {
        if (!cs.id) continue;

        // Standard IPF Row Scaling: x_p = C_p / Σ_r (K_pr × y_r)
        // 
        // IPF/Sinkhorn converges to the unique matrix that:
        // 1. Satisfies row constraints: Σ_r A_pr = C_p (full capacity utilization)
        // 2. Satisfies column constraints: Σ_p A_pr ≤ N_r (need limits via y_r)
        // 3. Minimizes KL-divergence from seed matrix (respects priorities)
        //
        // The "fair-share" concept belongs in market equilibrium models, not IPF.
        // IPF naturally handles competition through the seed matrix and column scaling (y_r).

        let denominator = 0;
        for (const ns of knownNeeds) {
            if (!ns.id) continue;

            const k_pr = calculateSeedValue(cs, ns, allCommitments, epsilon, gamma);
            if (k_pr <= 0) continue;

            const y_r = state.cachedRemoteScalings[ns.id] ?? 1.0;
            denominator += k_pr * y_r;
        }

        // Scale to actual capacity (not artificial "fair share")
        nextState.rowScalings[cs.id] = calculateScalingFactor(cs.quantity || 0, denominator, epsilon);
    }

    return nextState;
}

/**
 * Step 2: Generate Outgoing Proposals
 * 
 * Based on updated x_p, calculate flows to send to recipients.
 * A_pr = K_pr * x_p * y_r
 */
export function generateFlowProposals(
    capacitySlots: AvailabilitySlot[],
    knownNeeds: NeedSlot[],
    allCommitments: Record<string, Commitment>,
    state: DistributedIPFState,
    epsilon: number = 1e-6,
    gamma: number = 0.5
): FlowProposal[] {
    const proposals: FlowProposal[] = [];

    console.log(`[GENERATE-PROPOSALS] Starting with ${capacitySlots.length} capacity slots, ${knownNeeds.length} needs`);

    for (const cs of capacitySlots) {
        if (!cs.id) continue;
        const x_p = state.rowScalings[cs.id] || 0;
        if (x_p === 0) continue;

        const providerPubkey = findOwner(cs.id, allCommitments) || 'unknown';

        console.log(`[GENERATE-PROPOSALS] Processing capacity slot ${cs.id.slice(0, 10)}... (x_p=${x_p.toFixed(4)})`);

        for (const ns of knownNeeds) {
            if (!ns.id) continue;

            const k_pr = calculateSeedValue(cs, ns, allCommitments, epsilon, gamma);
            if (k_pr <= 0) {
                console.log(`[GENERATE-PROPOSALS]   Need ${ns.id.slice(0, 10)}... - INCOMPATIBLE (k_pr=${k_pr})`);
                continue;
            }

            const y_r = state.cachedRemoteScalings[ns.id] ?? 1.0;
            const rawQuantity = k_pr * x_p * y_r;

            // Calculate allocation using IPF formula: A_pr = K_pr * x_p * y_r
            // The y_r factor (from recipient) ensures Sum(A_pr) <= Need_r across all providers
            // No per-provider clamping needed - trust the distributed coordination
            const quantity = rawQuantity;

            console.log(`[GENERATE-PROPOSALS]   Need ${ns.id.slice(0, 10)}... - k_pr=${k_pr.toFixed(4)}, y_r=${y_r.toFixed(4)}, raw=${rawQuantity.toFixed(2)}, final=${quantity.toFixed(2)}`);

            if (quantity > epsilon) {
                const recipientPubkey = findOwner(ns.id, allCommitments) || 'unknown';
                proposals.push({
                    capacity_slot_id: cs.id,
                    need_slot_id: ns.id,
                    provider_pubkey: providerPubkey,
                    recipient_pubkey: recipientPubkey,
                    proposed_quantity: quantity,
                    seed_value: k_pr
                });
            }
        }
    }

    console.log(`[GENERATE-PROPOSALS] Generated ${proposals.length} proposals`);

    return proposals;
}

// ═══════════════════════════════════════════════════════════════════
// RECIPIENT LOGIC
// ═══════════════════════════════════════════════════════════════════

/**
 * Step 3: Recipient Update (Column Scaling)
 * 
 * Calculate my constraint factor y_r based on incoming flow proposals.
 * 
 * y_r_new = min(1, Need / TotalProposed)
 * 
 * Note: Strictly speaking in Sinkhorn, y_r is updated iteratively.
 * But practically, we can just clamp the sum of incoming proposals.
 * If providers sent K*x*y_old, then TotalReceived = Sum(K*x*y_old).
 * We want TotalReceived_New <= Need.
 * So y_new = y_old * min(1, Need / TotalReceived).
 * 
 * However, to be robust to async, we can just publish "Saturation Level":
 * Saturation = TotalProposed / Need
 * y_target = 1 / Saturation (clamped at 1)
 */
export function updateRecipientState(
    needSlots: NeedSlot[],
    incomingProposals: FlowProposal[], // Aggregated from network
    state: DistributedIPFState,
    epsilon: number = 1e-6
): DistributedIPFState {
    const nextState = {
        ...state,
        colScalings: {} as Record<string, number>, // Start fresh - only add scalings for current need slots
        totalSeedsByNeed: {} as Record<string, number> // Start fresh - only add seeds for current need slots
    };

    // Group proposals by need slot
    const proposalsByNeed: Record<string, number> = {};
    const seedsByNeed: Record<string, number> = {};

    // CRITICAL: Include ALL proposals (including self) in y_r calculation
    // This is correct IPF behavior - clamping must account for total demand
    // The "circular dependency" concern is invalid: y_r is a broadcast signal
    // that affects all providers equally, enabling proper displacement
    for (const p of incomingProposals) {
        // Sum all proposals for clamping calculation
        const current = proposalsByNeed[p.need_slot_id] || 0;
        proposalsByNeed[p.need_slot_id] = current + p.proposed_quantity;

        // Sum all seeds for competition metrics
        const seedCurrent = seedsByNeed[p.need_slot_id] || 0;
        seedsByNeed[p.need_slot_id] = seedCurrent + p.seed_value;
    }

    for (const ns of needSlots) {
        if (!ns.id) continue;

        const totalProposed = proposalsByNeed[ns.id] || 0;
        const needCap = ns.quantity || 0;

        // Calculate column scaling (y_r)
        const y_r = calculateConstraintFactor(needCap, totalProposed, epsilon);
        nextState.colScalings[ns.id] = y_r;

        console.log(`[Y_R-CALC] Need ${ns.id.slice(0, 10)}... needCap=${needCap.toFixed(2)}, totalProposed=${totalProposed.toFixed(2)}, y_r=${y_r.toFixed(4)}`);

        // Store total seed for provider fair-share calculation
        nextState.totalSeedsByNeed[ns.id] = seedsByNeed[ns.id] || epsilon;
    }

    return nextState;
}


// ═══════════════════════════════════════════════════════════════════
// HELPER FUNCTIONS 
// ═══════════════════════════════════════════════════════════════════
