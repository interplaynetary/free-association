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
}

export interface FlowProposal {
    capacity_slot_id: string;
    need_slot_id: string;
    provider_pubkey: string;  // ME
    recipient_pubkey: string; // THEM
    proposed_quantity: number; // K_pr * x_p (Note: usually we send K*x, recipient applies y)

    // In strict Sinkhorn, we might send K*x. 
    // If we send K*x*y (fully agreed), we need to know y. 
    // Protocol: Sender calculates A_proposed = K * x_p * y_r(cached).
    // Recipient sums these.
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
    const nextState = { ...state, rowScalings: { ...state.rowScalings } };

    for (const cs of capacitySlots) {
        if (!cs.id) continue;

        let denominator = 0;

        // Sum over all potential recipients
        for (const ns of knownNeeds) {
            if (!ns.id) continue;

            // 1. Calculate Seed K_pr
            const k_pr = calculateSeedValue(cs, ns, allCommitments, epsilon, gamma);
            if (k_pr <= 0) continue;

            // 2. Get Recipient's Constraint Factor (y_r) from cache
            // Default to 1.0 if unknown (optimistic start)
            const y_r = state.cachedRemoteScalings[ns.id] ?? 1.0;

            denominator += k_pr * y_r;
        }

        // 3. Update x_p via Core Util
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

    for (const cs of capacitySlots) {
        if (!cs.id) continue;
        const x_p = state.rowScalings[cs.id] || 0;
        if (x_p === 0) continue;

        const providerPubkey = findOwner(cs.id, allCommitments) || 'unknown';

        for (const ns of knownNeeds) {
            if (!ns.id) continue;

            const k_pr = calculateSeedValue(cs, ns, allCommitments, epsilon, gamma);
            if (k_pr <= 0) continue;

            const y_r = state.cachedRemoteScalings[ns.id] ?? 1.0;
            const rawQuantity = k_pr * x_p * y_r;

            // CRITICAL FIX: Clamp to need satisfaction
            // Even if IPF scaling suggests more, we should never allocate more than the recipient needs
            const needQuantity = ns.quantity || 0;
            const quantity = Math.min(rawQuantity, needQuantity);

            if (quantity > epsilon) {
                const recipientPubkey = findOwner(ns.id, allCommitments) || 'unknown';
                proposals.push({
                    capacity_slot_id: cs.id,
                    need_slot_id: ns.id,
                    provider_pubkey: providerPubkey,
                    recipient_pubkey: recipientPubkey,
                    proposed_quantity: quantity
                });
            }
        }
    }

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
    const nextState = { ...state, colScalings: { ...state.colScalings } };

    // Group proposals by need slot
    const proposalsByNeed: Record<string, number> = {};
    for (const p of incomingProposals) {
        const current = proposalsByNeed[p.need_slot_id] || 0;
        proposalsByNeed[p.need_slot_id] = current + p.proposed_quantity;
    }

    for (const ns of needSlots) {
        if (!ns.id) continue;

        const totalProposed = proposalsByNeed[ns.id] || 0;
        const needCap = ns.quantity || 0;

        // Use Core Util for robust calculation
        nextState.colScalings[ns.id] = calculateConstraintFactor(needCap, totalProposed, epsilon);
    }

    return nextState;
}


// ═══════════════════════════════════════════════════════════════════
// HELPER FUNCTIONS 
// ═══════════════════════════════════════════════════════════════════
