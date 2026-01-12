/**
 * IPF Core Utilities
 * 
 * Shared mathematical functions for Iterative Proportional Fitting.
 * Used by both Centralized (allocation-ipf.ts) and Distributed (allocation-ipf-distributed.ts) solvers.
 */

import type {
    AvailabilitySlot,
    NeedSlot,
    Commitment
} from './schemas.js';

import {
    slotsCompatible
} from '$lib/utils/match';

// ═══════════════════════════════════════════════════════════════════
// TYPES
// ═══════════════════════════════════════════════════════════════════

export interface IPFSharedOptions {
    /** Recipient influence weight (0 = Provider Dictates, 1 = Symmetric) */
    gamma?: number;

    /** "Hidden Demand" potential - min value to prevent zero divisions/locks */
    epsilon?: number;
}

// ═══════════════════════════════════════════════════════════════════
// CORE MATH
// ═══════════════════════════════════════════════════════════════════

/**
 * Calculate the "Seed Value" (K_pr) representing the raw physical intent.
 * 
 * Formula: K_pr = (ProviderPriority + ε) * (RecipientPreference + ε)^γ
 * 
 * @param cs Capacity Slot
 * @param ns Need Slot
 * @param allCommitments Lookup for global recognition weights
 * @param epsilon Small constant for connectivity
 * @param gamma Recipient influence exponent
 */
export function calculateSeedValue(
    cs: AvailabilitySlot,
    ns: NeedSlot,
    allCommitments: Record<string, Commitment>,
    epsilon: number = 1e-6,
    gamma: number = 0.5
): number {
    // 1. Hard Compatibility Check
    const compatible = slotsCompatible(ns, cs);
    if (!compatible) {
        console.log(`[SEED-VALUE-DEBUG] ❌ INCOMPATIBLE: need=${ns.id?.slice(0, 15)}... capacity=${cs.id?.slice(0, 15)}...`);
        console.log(`[SEED-VALUE-DEBUG]   Need type_id: ${ns.type_id}, Capacity type_id: ${cs.type_id}`);
        console.log(`[SEED-VALUE-DEBUG]   Need:`, { id: ns.id, type_id: ns.type_id, quantity: ns.quantity });
        console.log(`[SEED-VALUE-DEBUG]   Capacity:`, { id: cs.id, type_id: cs.type_id, quantity: cs.quantity });
        return 0;
    }

    const providerPubkey = findOwner(cs.id!, allCommitments) || 'unknown';
    const recipientPubkey = findOwner(ns.id!, allCommitments) || 'unknown';

    // 2. Get Priorities (0.0 - 1.0)
    const providerPriority = getSlotPriority(cs, recipientPubkey, allCommitments[providerPubkey]);
    const recipientPriority = getSlotPriority(ns, providerPubkey, allCommitments[recipientPubkey]);

    console.log(`[SEED-CALC] Provider ${providerPubkey.slice(0, 10)}... → Recipient ${recipientPubkey.slice(0, 10)}...`);
    console.log(`[SEED-CALC]   providerPriority=${providerPriority.toFixed(4)} (provider's recognition of recipient)`);
    console.log(`[SEED-CALC]   recipientPriority=${recipientPriority.toFixed(4)} (recipient's recognition of provider)`);
    console.log(`[SEED-CALC]   Provider's global_recognition_weights:`, allCommitments[providerPubkey]?.global_recognition_weights);

    // 3. Compute Potential (Symmetric IPF Seed)
    // K_pr = (p_provider + ε)^γ × (p_recipient + ε)^(1-γ)
    // When γ=0.5: both parties contribute equally (symmetric)
    // When γ=1.0: provider dictates (provider priority dominates)
    // When γ=0.0: recipient dictates (recipient preference dominates)
    const providerTerm = Math.pow(providerPriority + epsilon, gamma);
    const recipientTerm = Math.pow(recipientPriority + epsilon, 1.0 - gamma);
    const result = providerTerm * recipientTerm;

    console.log(`[SEED-CALC]   providerTerm=${providerTerm.toFixed(6)}, recipientTerm=${recipientTerm.toFixed(6)}, k_pr=${result.toFixed(4)}`);

    return result;
}

/**
 * Calculate the Scaling Factor required to satisfy a Row Constraint (Capacity).
 * 
 * x_p = Capacity / Sum(Offers)
 * 
 * @param capacity Total available capacity
 * @param currentSum Sum of current tentative flows (or K*values)
 * @returns The scaling factor to multiply flows by
 */
export function calculateScalingFactor(
    limit: number,
    currentSum: number,
    epsilon: number = 1e-6
): number {
    // If sum is tiny (less than epsilon squared), treat as zero demand
    // We use epsilon^2 because valid "Hidden Demand" seeds are O(epsilon).
    // If we filtered at epsilon, we'd kill the hidden demand connectivity.
    if (currentSum <= epsilon * epsilon) return 0;
    return limit / currentSum;
}

/**
 * Calculate the Damped Scaling Factor for a Column Constraint (Need).
 * 
 * y_r = min(1, Need / Sum(Proposals))
 * 
 * @param need Total need requested
 * @param currentSum Sum of current proposed flows
 * @returns Scaling factor (0.0 - 1.0)
 */
export function calculateConstraintFactor(
    need: number,
    currentSum: number,
    epsilon: number = 1e-6
): number {
    if (currentSum <= epsilon) return 1.0; // Under-saturated, fully open
    if (need <= epsilon) return 0.0; // No need, fully closed

    // If over-subscribed, scale down. If under, report 1.0 (open).
    if (currentSum > need) {
        return need / currentSum;
    }
    return 1.0;
}

// ═══════════════════════════════════════════════════════════════════
// HELPER UTILS
// ═══════════════════════════════════════════════════════════════════

export function findOwner(slotId: string, commitments: Record<string, Commitment>): string | undefined {
    for (const pubkey in commitments) {
        const c = commitments[pubkey];
        const capacityMatch = c.capacity_slots?.find(s => s.id === slotId);
        if (capacityMatch) return pubkey;
        const needMatch = c.need_slots?.find(s => s.id === slotId);
        if (needMatch) return pubkey;
    }
    return undefined;
}

export function getSlotPriority(
    slot: AvailabilitySlot | NeedSlot,
    personPubkey: string,
    commitment?: Commitment
): number {
    if (!slot.id) return 0;

    // Explicit priority in slot
    if (slot.priority_distribution && !Array.isArray(slot.priority_distribution)) {
        const val = (slot.priority_distribution as Record<string, number>)[personPubkey];
        if (val !== undefined) return val;
    }

    // Global recognition fallback
    if (commitment && commitment.global_recognition_weights) {
        return commitment.global_recognition_weights[personPubkey] || 0;
    }

    return 0;
}
