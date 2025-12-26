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
} from '@playnet/free-association/utils/match';

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
    if (!slotsCompatible(ns, cs)) return 0;

    const providerPubkey = findOwner(cs.id!, allCommitments) || 'unknown';
    const recipientPubkey = findOwner(ns.id!, allCommitments) || 'unknown';

    // 2. Get Priorities (0.0 - 1.0)
    const providerPriority = getSlotPriority(cs, recipientPubkey, allCommitments[providerPubkey]);
    const recipientPriority = getSlotPriority(ns, providerPubkey, allCommitments[recipientPubkey]);

    // 3. Compute Potential
    const push = providerPriority + epsilon;
    const pull = Math.pow(recipientPriority + epsilon, gamma);

    return push * pull;
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
    if (currentSum <= epsilon) return 0; // No demand, close value
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
