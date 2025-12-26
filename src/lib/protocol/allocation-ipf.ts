/**
 * IPF-Based Allocation Algorithm (Symmetric/Asymmetric Hybrid)
 * 
 * Implements a Weighted Iterative Proportional Fitting (W-IPF) approach to resource allocation.
 * 
 * Goal:
 * Replaced heuristic "Deviational Optimization" with a rigorous Sinkhorn-Knopp style algorithm.
 */

import type {
    AvailabilitySlot,
    NeedSlot,
    Commitment
} from '@playnet/free-association/schemas';

import {
    calculateSeedValue,
    findOwner,
    getSlotPriority
} from './ipf-core.js';

// ═══════════════════════════════════════════════════════════════════
// TYPES
// ═══════════════════════════════════════════════════════════════════

export interface SlotAllocationRecord {
    capacity_slot_id: string;
    need_slot_id: string;
    provider_pubkey: string;
    recipient_pubkey: string;
    recipient_node_id?: string;
    quantity: number;
    withinPriorityLimit: boolean; // Retained for schema verification
    fromSurplus: boolean;         // Retained
}

export interface IPFAllocationOptions {
    /** Recipient influence weight (0 = Provider Dictates, 1 = Symmetric) */
    gamma?: number;

    /** "Hidden Demand" potential - base connectivity for compatible slots validation */
    epsilon?: number;

    /** Convergence threshold */
    convergenceThreshold?: number;

    /** Max iterations */
    maxIterations?: number;

    debug?: boolean;
}

// ═══════════════════════════════════════════════════════════════════
// CONSTANTS
// ═══════════════════════════════════════════════════════════════════

const DEFAULT_GAMMA = 0.5;
const DEFAULT_EPSILON = 1e-6; // Small potential to allow flow to "wake up"
const DEFAULT_THRESHOLD = 0.001;
const DEFAULT_MAX_ITER = 100;

// ═══════════════════════════════════════════════════════════════════
// MAIN ENTRY POINT
// ═══════════════════════════════════════════════════════════════════

export function calculateIPFAllocation(
    capacitySlots: AvailabilitySlot[],
    needSlots: NeedSlot[],
    allCommitments: Record<string, Commitment>,
    options?: IPFAllocationOptions
): SlotAllocationRecord[] {

    const debug = options?.debug || false;
    const gamma = options?.gamma ?? DEFAULT_GAMMA;
    const epsilon = options?.epsilon ?? DEFAULT_EPSILON;
    const threshold = options?.convergenceThreshold ?? DEFAULT_THRESHOLD;
    const maxIter = options?.maxIterations ?? DEFAULT_MAX_ITER;

    if (debug) console.log(`[IPF] Starting: ${capacitySlots.length} caps, ${needSlots.length} needs. γ=${gamma}`);

    // 1. Construct Seed Matrix
    // matrix[csId][nsId] = number
    const matrix: Record<string, Record<string, number>> = {};

    // Initialize structure and seed
    for (const cs of capacitySlots) {
        if (!cs.id) continue;
        matrix[cs.id] = {};

        for (const ns of needSlots) {
            if (!ns.id) continue;

            const seed = calculateSeedValue(cs, ns, allCommitments, epsilon, gamma);

            // Only populate if > 0 (compatible)
            if (seed > 0) {
                matrix[cs.id][ns.id] = seed;
            }
        }
    }

    // 2. Run IPF Loop
    let converged = false;
    let iterations = 0;

    while (!converged && iterations < maxIter) {
        iterations++;
        let maxDiff = 0;

        // --- ROW SCALING (Provider Force) ---
        // Providers generally want to allocate FULL capacity if there are ANY takers.
        for (const cs of capacitySlots) {
            if (!cs.id) continue;
            const row = matrix[cs.id];
            if (!row) continue;

            const currentSum = sumValues(row);

            if (currentSum > epsilon) {
                // Force to Capacity
                const scale = (cs.quantity || 0) / currentSum;

                // Optimization: Don't scale if already close
                if (Math.abs(scale - 1) > threshold) {
                    for (const nsId in row) {
                        const oldVal = row[nsId];
                        if (oldVal !== undefined) {
                            const newVal = oldVal * scale;
                            row[nsId] = newVal;
                            maxDiff = Math.max(maxDiff, Math.abs(newVal - oldVal));
                        }
                    }
                }
            }
        }

        // --- COLUMN SCALING (Recipient Clamp) ---
        // Recipients cannot take more than Need.
        for (const ns of needSlots) {
            if (!ns.id) continue;
            // Sum column
            let colSum = 0;
            for (const csId in matrix) {
                const val = matrix[csId][ns.id];
                if (val !== undefined) {
                    colSum += val;
                }
            }

            if (colSum > epsilon) {
                // Clamp to Need
                // We only scale DOWN. If colSum < Need, we accept the shortage.
                if (colSum > (ns.quantity || 0)) {
                    const scale = (ns.quantity || 0) / colSum;

                    if (Math.abs(scale - 1) > threshold) {
                        for (const csId in matrix) {
                            const val = matrix[csId][ns.id];
                            if (val !== undefined && val > epsilon) {
                                const newVal = val * scale;
                                matrix[csId][ns.id] = newVal;
                                maxDiff = Math.max(maxDiff, Math.abs(newVal - val));
                            }
                        }
                    }
                }
            }
        }

        if (maxDiff < threshold) {
            converged = true;
        }
    }

    if (debug) {
        console.log(`[IPF] ${converged ? 'Converged' : 'Stopped'} after ${iterations} iterations`);
    }

    // 3. Format Output
    const records: SlotAllocationRecord[] = [];

    for (const cs of capacitySlots) {
        if (!cs.id || !matrix[cs.id]) continue;
        const providerPubKey = findOwner(cs.id, allCommitments) || 'unknown';

        for (const ns of needSlots) {
            if (!ns.id) continue;
            const amount = matrix[cs.id][ns.id];
            if (amount !== undefined && amount > threshold) {
                const recipientPubKey = findOwner(ns.id, allCommitments) || 'unknown';

                // Back-calculate checks
                const providerPriority = getSlotPriority(cs, recipientPubKey, allCommitments[providerPubKey]);
                const priorityLimit = providerPriority * (cs.quantity || 0);
                const withinPriorityLimit = amount <= priorityLimit + threshold;
                const fromSurplus = !withinPriorityLimit;

                records.push({
                    capacity_slot_id: cs.id,
                    need_slot_id: ns.id,
                    provider_pubkey: providerPubKey,
                    recipient_pubkey: recipientPubKey,
                    quantity: amount,
                    withinPriorityLimit,
                    fromSurplus
                });
            }
        }
    }

    return records;
}

function sumValues(obj: Record<string, number>): number {
    let sum = 0;
    for (const key in obj) {
        sum += obj[key];
    }
    return sum;
}
