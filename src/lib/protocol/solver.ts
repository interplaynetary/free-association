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
    NeedSlot
} from './schemas.js';

import {
    calculateSeedValue,
    calculateScalingFactor,
    calculateConstraintFactor,
    findOwner,
    type ResourceOwner,
    type RecognitionSource
} from './ipf-core.js';

import { buildSlotIndex } from './slot-index.js';

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
 * x_p = min(
 *    Capacity / Σ_r (K_pr * y_r),
 *    min_r ( FairShare_pr / (K_pr * y_r) )  <-- Fair Share Capping
 * )
 */
export function updateProviderState(
    capacitySlots: AvailabilitySlot[],
    knownNeeds: NeedSlot[], // All needs I am aware of (my "Local View")
    context: Record<string, ResourceOwner & RecognitionSource>,
    state: DistributedIPFState,
    epsilon: number = 1e-6,
    gamma: number = 0.5
): DistributedIPFState {
    const nextState = { ...state, rowScalings: {} as Record<string, number> }; // Start fresh - only add scalings for current capacity slots

    // INDEX: Build Demand Index from knownNeeds for efficient lookup (O(N))
    const demandIndex = buildSlotIndex(knownNeeds);

    for (const cs of capacitySlots) {
        if (!cs.id) continue;

        // 1. Calculate Denominator for Capacity Constraint
        // Denominator = Σ_r (K_pr * y_r)
        // x_p_capacity = Capacity / Denominator
        
        let denominator = 0;
        let minXpForFairShare = Number.POSITIVE_INFINITY;

        // QUERY: Use index to find ONLY compatible needs (O(k))
        const candidates = demandIndex.query(cs) as NeedSlot[];

        for (const ns of candidates) {
            if (!ns.id) continue;

            const k_pr = calculateSeedValue(cs, ns, context, epsilon, gamma);
            if (k_pr <= 0) continue;

            const y_r = state.cachedRemoteScalings[ns.id] ?? 1.0;
            const term = k_pr * y_r;
            denominator += term;

            // 2. Calculate Fair Share Constraint
            // FairShare_pr = (K_pr / ΣK_all) * NeedQuantity
            // x_p_fairshare <= FairShare_pr / (K_pr * y_r)
            const totalSeed = state.totalSeedsByNeed[ns.id];
            
            // Only apply fair share if we have info about competition (totalSeed)
            if (totalSeed && totalSeed > 0 && ns.quantity) {
                const fairShare = (k_pr / totalSeed) * ns.quantity;
                
                // Avoid division by zero
                if (term > epsilon) {
                    const xpLimit = fairShare / term;
                    if (xpLimit < minXpForFairShare) {
                        minXpForFairShare = xpLimit;
                    }
                }
            }
        }

        // Calculate base x_p from capacity
        const xpCapacity = calculateScalingFactor(cs.quantity || 0, denominator, epsilon);

        // Apply Fair Share Cap
        // We take the minimum of Capacity-based scaling and FairShare-based scaling
        const finalXp = Math.min(xpCapacity, minXpForFairShare);

        nextState.rowScalings[cs.id] = finalXp;
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
    context: Record<string, ResourceOwner & RecognitionSource>,
    state: DistributedIPFState,
    epsilon: number = 1e-6,
    gamma: number = 0.5
): FlowProposal[] {
    const proposals: FlowProposal[] = [];

    // INDEX: Build Demand Index from knownNeeds (O(N))
    const demandIndex = buildSlotIndex(knownNeeds);
    const knownNeedsCount = knownNeeds.length;

    for (const cs of capacitySlots) {
        if (!cs.id) continue;
        const x_p = state.rowScalings[cs.id] || 0;
        if (x_p === 0) continue;

        const providerPubkey = findOwner(cs.id, context) || 'unknown';

        // QUERY: Use index to find ONLY compatible needs
        const candidates = demandIndex.query(cs) as NeedSlot[];
        
        // Log optimization metric if debugging (optional)
        // const candidateCount = candidates.length;
        // console.log(`[GENERATE-PROPOSALS] Index pruned ${knownNeedsCount} needs -> ${candidateCount} candidates`);

        for (const ns of candidates) {
            if (!ns.id) continue;

            const k_pr = calculateSeedValue(cs, ns, context, epsilon, gamma);
            if (k_pr <= 0) continue;

            const y_r = state.cachedRemoteScalings[ns.id] ?? 1.0;
            const rawQuantity = k_pr * x_p * y_r;

            const quantity = rawQuantity; // Trust distributed coordination

            if (quantity > epsilon) {
                const recipientPubkey = findOwner(ns.id, context) || 'unknown';
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

        // Store total seed for provider fair-share calculation
        nextState.totalSeedsByNeed[ns.id] = seedsByNeed[ns.id] || epsilon;
    }

    return nextState;
}

// ═══════════════════════════════════════════════════════════════════
// HELPER FUNCTIONS 
// ═══════════════════════════════════════════════════════════════════
