/**
 * Priority-Based Allocation Algorithm (Slot-Based)
 * 
 * Implements the "Priority Limits with Proportional Surplus Redistribution" algorithm.
 * 
 * Core Philosophy:
 * - Priorities define LIMITS on willingness to allocate at the SLOT level
 * - CapacitySlot priority % × capacity = maximum willing to give to specific need slot
 * - Unused allocation returns to capacity slot's pool
 * - Surplus redistributed proportionally among compatible need slots with unmet needs
 * 
 * Key Differences from Abstract Model:
 * - Granularity: CapacitySlot <-> NeedSlot (instead of Provider <-> Recipient)
 * - Compatibility: Checked via slotsCompatible() (time, location, type)
 * - Priorities: Distributed specific to each slot
 */

import type {
    AvailabilitySlot,
    NeedSlot
} from '@playnet/free-association/schemas';

import {
    findOwner,
    type ResourceOwner
} from '../../ipf-core.js';

import {
    slotsCompatible
} from '$lib/protocol/match';

// ═══════════════════════════════════════════════════════════════════
// TYPES
// ═══════════════════════════════════════════════════════════════════

export interface SlotAllocationRecord {
    capacity_slot_id: string;
    need_slot_id: string;
    provider_pubkey: string;
    recipient_pubkey: string;
    recipient_node_id?: string; // Optional in some schemas, adding for safety if needed, check definition
    quantity: number;
    withinPriorityLimit: boolean;
    fromSurplus: boolean;
}

// Alias for backward compatibility with local code
export type LocalSlotAllocation = SlotAllocationRecord;

// ═══════════════════════════════════════════════════════════════════
// CONSTANTS
// ═══════════════════════════════════════════════════════════════════

export const EPSILON = 0.0001;
export const MAX_REFINEMENT_ITERATIONS = 100;
export const CONVERGENCE_THRESHOLD = 0.01;
export const MAX_ADJUSTMENT_PER_ITERATION = 0.1; // 10% of current allocation

// ═══════════════════════════════════════════════════════════════════
// UTILITY FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Redistribute remainder using Largest Remainder Method
 */
export function redistributeRemainder(
    targets: Map<string, number>,
    remainingCapacity: number,
    totalCapacity: number,
    shares: Map<string, number>,
    minAtomicSize: number,
    debug: boolean
): number {

    const unitSize = minAtomicSize > EPSILON ? minAtomicSize : 1;
    const remainingUnits = Math.floor(remainingCapacity / unitSize);

    if (remainingUnits === 0) return 0;

    if (debug) {
        console.log(`[REMAINDER-REDISTRIBUTION] ${remainingUnits} units of ${unitSize.toFixed(2)} to redistribute`);
    }

    // Calculate ideal allocation and remainders
    const totalAllocated = Array.from(targets.values()).reduce((sum, v) => sum + v, 0);
    const remainders: Array<{ recipient: string; remainder: number; allocated: number }> = [];

    for (const [recipient, allocated] of targets.entries()) {
        let proportion = 0;
        if (shares && shares.has(recipient)) {
            proportion = shares.get(recipient)!;
        } else if (totalAllocated > EPSILON) {
            proportion = allocated / totalAllocated;
        }

        const idealUnits = remainingUnits * proportion;
        const integerUnits = Math.floor(idealUnits);
        const remainder = idealUnits - integerUnits;

        // Add base integer units (fix for re-quantization scenarios)
        if (integerUnits > 0) {
            const current = targets.get(recipient) || 0;
            targets.set(recipient, current + integerUnits * unitSize);
        }

        if (remainder > EPSILON) {
            remainders.push({ recipient, remainder, allocated });
        }
    }

    // Sort by largest remainder
    remainders.sort((a, b) => b.remainder - a.remainder);

    // Calculate units to distribute (remaining - used base units)
    const usedBaseUnits = Array.from(targets.values()).reduce((sum, v) => sum + (v / unitSize), 0);
    const unitsToDistribute = remainingUnits - usedBaseUnits;

    // Distribute units to recipients with largest remainders
    let unitsDistributed = 0;
    for (let i = 0; i < Math.min(unitsToDistribute, remainders.length); i++) {
        const { recipient } = remainders[i];
        const current = targets.get(recipient) || 0;
        targets.set(recipient, current + unitSize);
        unitsDistributed++;
    }

    return (usedBaseUnits + unitsDistributed) * unitSize;
}

// ═══════════════════════════════════════════════════════════════════
// OPTIONS
// ═══════════════════════════════════════════════════════════════════

export interface PriorityAllocationOptions {
    /** Enable Phase 2 iterative refinement (default: true) */
    enableRefinement?: boolean;

    /** Maximum iterations for Phase 2 (default: 100) */
    maxRefinementIterations?: number;

    /** Weight for provider vs recipient deviations in Phase 2 (default: 0.5) */
    alpha?: number;

    /** Enable detailed logging */
    debug?: boolean;

    // ═══════════════════════════════════════════════════════════════════
    // SIMULATED ANNEALING OPTIONS
    // ═══════════════════════════════════════════════════════════════════

    /** Enable simulated annealing to escape local minima (default: false) */
    enableAnnealing?: boolean;

    /** Initial temperature for annealing - higher = more willing to accept worse solutions (default: 0.1) */
    initialTemperature?: number;

    /** Cooling rate - how quickly temperature decreases (default: 0.95) */
    coolingRate?: number;

    /** Trigger annealing after this many stuck iterations (default: 10) */
    annealEveryNIterations?: number;

    /** Maximum number of annealing escape attempts (default: 5) */
    maxAnnealingAttempts?: number;
}

// ═══════════════════════════════════════════════════════════════════
// INTERNAL TYPES
// ═══════════════════════════════════════════════════════════════════

// Matrix: ProviderSlotID -> RecipientSlotID -> Allocation Record
interface SlotAllocationMatrix {
    [capacitySlotId: string]: {
        [needSlotId: string]: {
            amount: number;
            providerPubKey: string;
            recipientPubKey: string;
            withinPriorityLimit: boolean;
            fromSurplus: boolean;
        };
    };
}

interface CompatibilityInfo {
    isCompatible: boolean;
    providerLimit: number;
    recipientLimit: number;
    bilateralLimit: number;
    providerPriority: number;
    recipientPriority: number;
}

// ═══════════════════════════════════════════════════════════════════
// MAIN ENTRY POINT
// ═══════════════════════════════════════════════════════════════════

/**
 * Calculate slot-based priority allocation
 * 
 * @param capacitySlots - Available capacity slots
 * @param needSlots - Requested need slots
 * @param allCommitments - Network commitments (for pubkey lookup)
 * @param options - Configuration options
 */
export function calculateSlotBasedPriorityAllocation(
    capacitySlots: AvailabilitySlot[],
    needSlots: NeedSlot[],
    resourcesMap: Record<string, ResourceOwner>,
    options?: PriorityAllocationOptions
): SlotAllocationRecord[] {

    const debug = options?.debug || false;

    if (debug) {
        console.log(`[PRIORITY-ALLOC] Starting with ${capacitySlots.length} capacity slots, ${needSlots.length} need slots`);
    }

    // Phase 1: Initial allocation with priority limits and surplus redistribution
    const allocationMatrix = initialAllocationWithSurplus(
        capacitySlots,
        needSlots,
        resourcesMap,
        debug
    );

    // Phase 2: Optional iterative refinement
    let converged = false;
    let iterations = 0;

    const enableRefinement = options?.enableRefinement ?? true;

    if (enableRefinement) {
        const refinementResult = iterativeRefinement(
            allocationMatrix,
            capacitySlots,
            needSlots,
            options
        );
        iterations = refinementResult.iterations;
        converged = refinementResult.converged;
    }

    // Final Divisibility Check: Re-apply constraints if Phase 2 shifted things off-grid
    for (const cs of capacitySlots) {
        if (cs.min_atomic_size && cs.min_atomic_size > EPSILON) {
            // Check if any allocation is invalid
            let clean = true;
            const unit = cs.min_atomic_size;
            const matrixRow = allocationMatrix[cs.id];

            for (const nsId in matrixRow) {
                const amt = matrixRow[nsId].amount;
                if (amt > EPSILON && Math.abs(amt % unit) > EPSILON && Math.abs((amt % unit) - unit) > EPSILON) {
                    clean = false;
                    break;
                }
            }

            if (!clean) {
                if (debug) console.log(`[FINAL-DIVISIBILITY] Re-applying constraints for ${cs.id}`);

                // Current total used
                let totalUsed = 0;
                const targets = new Map<string, number>();
                for (const nsId in matrixRow) {
                    if (matrixRow[nsId].amount > EPSILON) {
                        targets.set(nsId, matrixRow[nsId].amount);
                        totalUsed += matrixRow[nsId].amount;
                    }
                }

                // Shares = current distribution
                const shares = new Map<string, number>();
                for (const [id, amt] of targets.entries()) {
                    shares.set(id, amt / totalUsed);
                    targets.set(id, 0); // Zero out for clean reconstruction from base
                    matrixRow[id].amount = 0; // Reset matrix
                }

                redistributeRemainder(
                    targets, // This map is updated by function
                    totalUsed, // Amount to distribute (Full amount here for re-quantization)
                    cs.quantity,
                    shares,
                    cs.min_atomic_size,
                    debug
                );

                // Write back targets to matrix
                for (const [id, amt] of targets.entries()) {
                    matrixRow[id].amount = amt;
                }
            }
        }
    }

    // Convert matrix to flat records
    return flattenMatrix(allocationMatrix);
}

/**
 * Apply divisibility constraints to a raw quantity
 * 
 * @param rawQuantity - The raw calculated quantity
 * @param sharePercentage - The percentage of total capacity this represents
 * @param slot - The capacity slot with constraints
 * @returns The constrained quantity (rounded down to nearest unit)
 */
export function applyDivisibilityConstraints(
    rawQuantity: number,
    sharePercentage: number,
    slot: AvailabilitySlot
): number {
    const minAtomic = slot.min_atomic_size || 0;

    // If no atomic size, return raw
    if (minAtomic <= EPSILON) return rawQuantity;

    // If smaller than one unit, return 0
    if (rawQuantity < minAtomic - EPSILON) {
        return 0;
    }

    // Round down to nearest unit
    const units = Math.floor((rawQuantity + EPSILON) / minAtomic);
    return units * minAtomic;
}

/**
 * Check if a quantity meets minimum allocation requirements
 * 
 * @param quantity - The quantity to check
 * @param slot - The capacity slot with constraints
 * @returns True if meets minimums
 */
export function meetsMinimumAllocation(
    quantity: number,
    slot: AvailabilitySlot
): boolean {
    if (quantity <= EPSILON) return false;

    // Check unit size check
    const minAtomic = slot.min_atomic_size || 0;

    if (minAtomic > EPSILON && quantity < minAtomic - EPSILON) {
        return false;
    }

    return true;
}


// ═══════════════════════════════════════════════════════════════════
// PHASE 1: INITIAL ALLOCATION WITH SURPLUS REDISTRIBUTION
// ═══════════════════════════════════════════════════════════════════

export function initialAllocationWithSurplus(
    capacitySlots: AvailabilitySlot[],
    needSlots: NeedSlot[],
    resourcesMap: Record<string, ResourceOwner>,
    debug: boolean
): SlotAllocationMatrix {

    const matrix: SlotAllocationMatrix = {};

    // Initialize matrix
    for (const cs of capacitySlots) {
        matrix[cs.id] = {};
        for (const ns of needSlots) {
            // Find pubkeys from slots
            const providerPubKey = findOwner(cs.id, resourcesMap) || 'unknown';
            const recipientPubKey = findOwner(ns.id, resourcesMap) || 'unknown';

            matrix[cs.id][ns.id] = {
                amount: 0,
                providerPubKey,
                recipientPubKey,
                withinPriorityLimit: true,
                fromSurplus: false
            };
        }
    }

    // Process each capacity slot
    for (const cs of capacitySlots) {
        if (debug) {
            console.log(`\n[PHASE-1] Capacity Slot ${cs.id.slice(0, 10)}: capacity=${cs.quantity}`);
        }

        // Step 1: Calculate compatibility and bilateral limits
        const compatibility = calculateCompatibility(cs, needSlots);

        // Step 2: Tentative allocation
        const tentativeAllocations = performTentativeAllocation(cs, needSlots, compatibility, matrix, debug);

        // Step 3: Calculate surplus
        const totalTentative = Object.values(tentativeAllocations).reduce((sum, a) => sum + a, 0);
        let surplus = cs.quantity - totalTentative;

        if (debug) {
            console.log(`[PHASE-1]   Tentative total: ${totalTentative.toFixed(2)}, Surplus: ${surplus.toFixed(2)}`);
        }

        // Apply tentative allocations
        for (const [nsId, amount] of Object.entries(tentativeAllocations)) {
            if (matrix[cs.id] && matrix[cs.id][nsId]) {
                matrix[cs.id][nsId].amount = amount;
            }
        }

        // Step 4: Redistribute surplus
        if (surplus > EPSILON) {
            surplus = redistributeSurplus(
                cs,
                needSlots,
                matrix,
                compatibility,
                surplus,
                debug
            );
        }

        // Step 5: Divisibility constraints (Least Remainder)
        if (surplus > EPSILON && cs.min_atomic_size && cs.min_atomic_size > EPSILON) {
            surplus = applyLeastRemainderMethod(
                cs,
                matrix,
                surplus,
                debug
            );
        }
    }

    return matrix;
}

export function calculateCompatibility(
    cs: AvailabilitySlot,
    needSlots: NeedSlot[]
): Map<string, CompatibilityInfo> {
    const compatibility = new Map<string, CompatibilityInfo>();

    for (const ns of needSlots) {
        // 1. Check basic compatibility using match.ts logic
        const isCompatible = slotsCompatible(ns, cs);

        if (!isCompatible) {
            compatibility.set(ns.id, {
                isCompatible: false,
                providerLimit: 0,
                recipientLimit: 0,
                bilateralLimit: 0,
                providerPriority: 0,
                recipientPriority: 0
            });
            continue;
        }

        // 2. Get Priorities
        // Provider -> NeedSlot
        // Provider -> NeedSlot
        let providerPriority = 0;
        if (Array.isArray(cs.priority_distribution)) {
            providerPriority = cs.priority_distribution.find(
                p => p.target_slot_id === ns.id
            )?.priority_percentage || 0;
        }

        // Recipient -> CapacitySlot
        let recipientPriority = 0;
        if (Array.isArray(ns.priority_distribution)) {
            recipientPriority = ns.priority_distribution.find(
                p => p.target_slot_id === cs.id
            )?.priority_percentage || 0;
        }

        // 3. Calculate Limits
        const providerLimit = providerPriority * cs.quantity;
        const recipientLimit = recipientPriority * ns.quantity;
        const bilateralLimit = Math.min(providerLimit, recipientLimit);

        compatibility.set(ns.id, {
            isCompatible: true,
            providerLimit,
            recipientLimit,
            bilateralLimit,
            providerPriority,
            recipientPriority
        });
    }

    return compatibility;
}

export function performTentativeAllocation(
    cs: AvailabilitySlot,
    needSlots: NeedSlot[],
    compatibility: Map<string, CompatibilityInfo>,
    currentMatrix: SlotAllocationMatrix,
    debug: boolean
): Record<string, number> {
    const allocations: Record<string, number> = {};
    let totalTentative = 0;

    for (const ns of needSlots) {
        const info = compatibility.get(ns.id)!;
        if (!info.isCompatible) continue;

        // Calculate already received from other CS
        let alreadyReceived = 0;
        for (const otherCsId in currentMatrix) {
            alreadyReceived += currentMatrix[otherCsId][ns.id]?.amount || 0;
        }
        const remainingNeed = Math.max(0, ns.quantity - alreadyReceived);

        // Use Provider Limit (ignore Recipient Limit in Phase 1)
        // Ensure we don't exceed remaining need
        const amount = Math.min(info.providerLimit, remainingNeed);
        allocations[ns.id] = amount;
        totalTentative += amount;
    }

    // Handle over-subscription
    if (totalTentative > cs.quantity + EPSILON) {
        const scale = cs.quantity / totalTentative;
        for (const key in allocations) {
            allocations[key] *= scale;
        }
    }

    return allocations;
}

export function redistributeSurplus(
    cs: AvailabilitySlot,
    needSlots: NeedSlot[],
    matrix: SlotAllocationMatrix,
    compatibility: Map<string, CompatibilityInfo>,
    surplus: number,
    debug: boolean
): number {

    // Identify needs with UNMET need
    const unmetNeeds: Array<{ nsId: string; priority: number; unmet: number }> = [];

    for (const ns of needSlots) {
        const info = compatibility.get(ns.id)!;
        if (!info.isCompatible || info.providerPriority < EPSILON) continue;

        // Calculate total received across ALL capacity slots
        let totalReceived = 0;
        for (const cKey in matrix) {
            totalReceived += matrix[cKey][ns.id]?.amount || 0;
        }

        const unmet = Math.max(0, ns.quantity - totalReceived);

        // NOTE: In Phase 1, we ignore recipient-side limits for surplus redistribution
        // We only care about absolute unmet need. Phase 2 will correct proportions.

        // Only include if they have unmet needs
        if (unmet > EPSILON) {
            unmetNeeds.push({
                nsId: ns.id,
                priority: info.providerPriority,
                unmet
            });
        }
    }

    if (unmetNeeds.length === 0) return surplus;

    // Distribute proportionally
    const totalPriority = unmetNeeds.reduce((sum, item) => sum + item.priority, 0);
    if (totalPriority < EPSILON) return surplus;

    let distributed = 0;

    for (const item of unmetNeeds) {
        const share = item.priority / totalPriority;
        const potential = surplus * share;

        const actual = Math.min(potential, item.unmet);

        if (actual > EPSILON) {
            matrix[cs.id][item.nsId].amount += actual;
            matrix[cs.id][item.nsId].fromSurplus = true;
            distributed += actual;
        }
    }

    return surplus - distributed;
}

export function getNeedQuantity(needSlots: NeedSlot[], nsId: string): number {
    return needSlots.find(n => n.id === nsId)?.quantity || 0;
}

export function applyLeastRemainderMethod(
    cs: AvailabilitySlot,
    matrix: SlotAllocationMatrix,
    surplus: number,
    debug: boolean
): number {
    const minAtomic = cs.min_atomic_size || 1;
    const targets = new Map<string, number>();

    // Collect current allocations
    let total = 0;
    for (const [nsId, record] of Object.entries(matrix[cs.id])) {
        if (record.amount > EPSILON) {
            targets.set(nsId, record.amount);
            total += record.amount;
        }
    }

    const shares = new Map<string, number>();
    for (const [id, amt] of targets.entries()) {
        shares.set(id, amt / total);
    }

    // Pass shares explicitly although applyLeastRemainderMethod currently derives them?
    const redistributed = redistributeRemainder(
        targets,
        surplus,
        cs.quantity,
        undefined as any, // Standard mode: derive shares from targets
        minAtomic,
        debug
    );

    // Write back
    for (const [nsId, amt] of targets.entries()) {
        matrix[cs.id][nsId].amount = amt;
    }

    return surplus - redistributed;
}

// ═══════════════════════════════════════════════════════════════════
// SIMULATED ANNEALING HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Generate a random neighbor solution by perturbing allocations
 * 
 * Strategy: Randomly adjust 1-3 allocations by ±10-30% to explore nearby solutions
 * 
 * @param matrix - Current allocation matrix
 * @returns Perturbed neighbor matrix
 */
export function generateRandomNeighbor(matrix: SlotAllocationMatrix): SlotAllocationMatrix {
    const neighbor: SlotAllocationMatrix = {};

    // Deep copy the matrix
    for (const csId in matrix) {
        neighbor[csId] = {};
        for (const nsId in matrix[csId]) {
            neighbor[csId][nsId] = { ...matrix[csId][nsId] };
        }
    }

    // Get all non-zero allocations
    const allocations: Array<{ csId: string; nsId: string }> = [];
    for (const csId in matrix) {
        for (const nsId in matrix[csId]) {
            if (matrix[csId][nsId].amount > EPSILON) {
                allocations.push({ csId, nsId });
            }
        }
    }

    if (allocations.length === 0) return neighbor;

    // Perturb 1-3 random allocations
    const perturbations = Math.min(1 + Math.floor(Math.random() * 3), allocations.length);

    for (let i = 0; i < perturbations; i++) {
        const idx = Math.floor(Math.random() * allocations.length);
        const { csId, nsId } = allocations[idx];

        const current = neighbor[csId][nsId].amount;
        // Random perturbation: ±10-30% of current value
        const perturbationFactor = 0.1 + Math.random() * 0.2; // 10-30%
        const perturbation = (Math.random() < 0.5 ? -1 : 1) * perturbationFactor * current;

        neighbor[csId][nsId].amount = Math.max(0, current + perturbation);
    }

    return neighbor;
}

/**
 * Calculate total deviation as a loss metric
 * Lower deviation = better alignment with priorities
 * 
 * @param matrix - Allocation matrix
 * @param capacitySlots - Capacity slots
 * @param needSlots - Need slots
 * @param alpha - Weight for provider vs recipient deviations
 * @returns Total absolute deviation
 */
export function calculateTotalDeviation(
    matrix: SlotAllocationMatrix,
    capacitySlots: AvailabilitySlot[],
    needSlots: NeedSlot[],
    alpha: number
): number {
    const deviations = calculateDeviations(matrix, capacitySlots, needSlots, alpha);

    let totalDev = 0;
    for (const row of Object.values(deviations)) {
        for (const val of Object.values(row)) {
            totalDev += Math.abs(val);
        }
    }

    return totalDev;
}

/**
 * Attempt to escape local minimum using simulated annealing
 * 
 * Metaphor: "Take an occasional hop to find better valleys"
 * 
 * @param matrix - Current allocation matrix
 * @param currentDeviation - Current total deviation (loss)
 * @param capacitySlots - Capacity slots
 * @param needSlots - Need slots
 * @param alpha - Weight for provider vs recipient deviations
 * @param temperature - Current temperature (higher = more willing to accept worse solutions)
 * @param debug - Enable debug logging
 * @returns Result with new matrix, deviation, and whether it was accepted
 */
export function tryAnnealingEscape(
    matrix: SlotAllocationMatrix,
    currentDeviation: number,
    capacitySlots: AvailabilitySlot[],
    needSlots: NeedSlot[],
    alpha: number,
    temperature: number,
    debug: boolean
): { matrix: SlotAllocationMatrix; deviation: number; accepted: boolean } {

    // Generate random neighbor
    const neighbor = generateRandomNeighbor(matrix);

    // Ensure neighbor respects capacity constraints
    // (Simple projection: scale down if over capacity)
    for (const cs of capacitySlots) {
        let totalAllocated = 0;
        for (const nsId in neighbor[cs.id] || {}) {
            totalAllocated += neighbor[cs.id][nsId].amount;
        }

        if (totalAllocated > cs.quantity + EPSILON) {
            const scale = cs.quantity / totalAllocated;
            if (neighbor[cs.id]) {
                for (const nsId in neighbor[cs.id]) {
                    neighbor[cs.id][nsId].amount *= scale;
                }
            }
        }
    }

    // Calculate neighbor's deviation
    const neighborDeviation = calculateTotalDeviation(neighbor, capacitySlots, needSlots, alpha);

    // Metropolis criterion: accept if better, or with probability exp(-ΔE/T) if worse
    const deviationDifference = neighborDeviation - currentDeviation;

    if (deviationDifference < 0) {
        // Always accept improvements
        if (debug) {
            console.log(`  🔼 Annealing: Found better solution! (Δ=${deviationDifference.toFixed(6)})`);
        }
        return { matrix: neighbor, deviation: neighborDeviation, accepted: true };
    } else {
        // Accept worse solutions with probability exp(-ΔE/T)
        const acceptanceProbability = Math.exp(-deviationDifference / temperature);

        if (Math.random() < acceptanceProbability) {
            if (debug) {
                console.log(`  🔀 Annealing: Accepted worse solution (Δ=+${deviationDifference.toFixed(6)}, P=${acceptanceProbability.toFixed(4)})`);
            }
            return { matrix: neighbor, deviation: neighborDeviation, accepted: true };
        } else {
            return { matrix, deviation: currentDeviation, accepted: false };
        }
    }
}

// ═══════════════════════════════════════════════════════════════════
// PHASE 2: ITERATIVE REFINEMENT (WITH OPTIONAL ANNEALING)
// ═══════════════════════════════════════════════════════════════════

export function iterativeRefinement(
    matrix: SlotAllocationMatrix,
    capacitySlots: AvailabilitySlot[],
    needSlots: NeedSlot[],
    options?: PriorityAllocationOptions
): { iterations: number; converged: boolean; annealingStats?: { attempts: number; successes: number; escapes: number } } {

    const maxIter = options?.maxRefinementIterations || MAX_REFINEMENT_ITERATIONS;
    const alpha = options?.alpha ?? 0.5;
    const debug = options?.debug || false;

    // Annealing parameters
    const enableAnnealing = options?.enableAnnealing || false;
    const initialTemperature = options?.initialTemperature || 0.1;
    const coolingRate = options?.coolingRate || 0.95;
    const annealEveryNIterations = options?.annealEveryNIterations || 10;
    const maxAnnealingAttempts = options?.maxAnnealingAttempts || 5;

    let temperature = initialTemperature;
    let stuckIterations = 0;
    let previousDeviation = calculateTotalDeviation(matrix, capacitySlots, needSlots, alpha);

    const annealingStats = {
        attempts: 0,
        successes: 0,
        escapes: 0
    };

    let iterations = 0;
    let converged = false;

    if (debug && enableAnnealing) {
        console.log(`🌡️  Simulated annealing enabled (initial temp: ${temperature.toFixed(4)})`);
    }

    while (iterations < maxIter && !converged) {
        iterations++;
        const deviations = calculateDeviations(matrix, capacitySlots, needSlots, alpha);

        let totalDev = 0;
        // Sum absolute deviations
        for (const row of Object.values(deviations)) {
            for (const val of Object.values(row)) {
                totalDev += Math.abs(val);
            }
        }

        if (totalDev < CONVERGENCE_THRESHOLD) {
            converged = true;
            if (debug) console.log(`✅ Converged at iteration ${iterations} (deviation: ${totalDev.toFixed(6)})`);
            break;
        }

        // Check if we're making progress
        const improvement = previousDeviation - totalDev;
        if (improvement < CONVERGENCE_THRESHOLD * 0.1) {
            stuckIterations++;
        } else {
            stuckIterations = 0;
        }
        previousDeviation = totalDev;

        // Try gradient descent adjustment
        const adjustment = makeAdjustments(matrix, deviations, capacitySlots, needSlots);

        // Enforce need limits (Global Clamp) to resolve any overshoots
        const clamped = enforceNeedLimits(matrix, needSlots);

        if (!adjustment && !clamped) {
            // No adjustments possible - might be stuck
            stuckIterations = Math.max(stuckIterations, annealEveryNIterations);
        }

        // === SIMULATED ANNEALING ESCAPE ===
        if (enableAnnealing &&
            stuckIterations >= annealEveryNIterations &&
            annealingStats.attempts < maxAnnealingAttempts &&
            temperature > 0.01) {

            annealingStats.attempts++;

            if (debug) {
                console.log(`  🔥 Attempting annealing escape #${annealingStats.attempts} (stuck for ${stuckIterations} iterations, temp: ${temperature.toFixed(4)})`);
            }

            const annealingResult = tryAnnealingEscape(
                matrix,
                totalDev,
                capacitySlots,
                needSlots,
                alpha,
                temperature,
                debug
            );

            if (annealingResult.accepted) {
                annealingStats.successes++;

                // Check if this was an "escape" (accepted worse solution)
                if (annealingResult.deviation > totalDev) {
                    annealingStats.escapes++;
                    if (debug) console.log(`  🎯 ESCAPE: Accepted worse solution to escape local minimum`);
                }

                // Update matrix with annealed solution
                for (const csId in annealingResult.matrix) {
                    for (const nsId in annealingResult.matrix[csId]) {
                        matrix[csId][nsId] = annealingResult.matrix[csId][nsId];
                    }
                }

                previousDeviation = annealingResult.deviation;
                stuckIterations = 0; // Reset stuck counter

                // Cool down temperature
                temperature *= coolingRate;
                if (debug) console.log(`  ❄️  Temperature cooled to ${temperature.toFixed(4)}`);
            } else {
                if (debug) console.log(`  ❌ Annealing: Neighbor rejected`);
            }
        }

        // Check if we should give up
        if (stuckIterations > annealEveryNIterations * 2 &&
            (!enableAnnealing || annealingStats.attempts >= maxAnnealingAttempts)) {
            if (debug) console.log(`⚠️  Stopping: stuck for ${stuckIterations} iterations, no more annealing attempts`);
            break;
        }
    }

    if (debug && enableAnnealing) {
        console.log(`\n📊 Annealing Statistics:`);
        console.log(`   Attempts: ${annealingStats.attempts}`);
        console.log(`   Successes: ${annealingStats.successes}`);
        console.log(`   Escapes from local minima: ${annealingStats.escapes}`);
        console.log(`   Final temperature: ${temperature.toFixed(4)}`);
    }

    return {
        iterations,
        converged,
        ...(enableAnnealing ? { annealingStats } : {})
    };
}

export function calculateDeviations(
    matrix: SlotAllocationMatrix,
    capacitySlots: AvailabilitySlot[],
    needSlots: NeedSlot[],
    alpha: number
): Record<string, Record<string, number>> {

    const deviations: Record<string, Record<string, number>> = {};

    // 1. Provider (Capacity) Perspective
    for (const cs of capacitySlots) {
        deviations[cs.id] = {};

        let totalGiven = 0;
        // Identify ALL potential recipients (entries in matrix)
        const recipients: string[] = [];

        for (const nsId in matrix[cs.id]) {
            recipients.push(nsId);
            const amt = matrix[cs.id][nsId].amount;
            if (amt > EPSILON) {
                totalGiven += amt;
            }
        }

        if (totalGiven < EPSILON) continue;

        // Calculate Ideal Shares based on ALL potential needs
        let totalPriorityAmongServed = 0;
        const priorities: Record<string, number> = {};

        for (const nsId of recipients) {
            let priority = 0;
            if (Array.isArray(cs.priority_distribution)) {
                priority = cs.priority_distribution.find(
                    p => p.target_slot_id === nsId
                )?.priority_percentage || 0;
            }
            priorities[nsId] = priority;
            totalPriorityAmongServed += priority;
        }

        if (totalPriorityAmongServed > EPSILON) {
            for (const nsId of recipients) {
                const currentAmount = matrix[cs.id][nsId].amount;
                const actual = currentAmount / totalGiven;

                let ideal = 0;
                // Safe lookup
                if (Array.isArray(cs.priority_distribution)) {
                    const p = cs.priority_distribution.find(
                        p => p.target_slot_id === nsId
                    );
                    if (p) ideal = p.priority_percentage / totalPriorityAmongServed;
                }

                deviations[cs.id][nsId] = (actual - ideal) * alpha;
            }
        }
    }

    // 2. Recipient (Need) Perspective
    for (const ns of needSlots) {
        let totalReceived = 0;
        // Identify ALL potential sources (entries in matrix)
        const sources: string[] = [];

        for (const csId in matrix) {
            const entry = matrix[csId]?.[ns.id];
            if (entry) { // Exists means compatible
                sources.push(csId);
                totalReceived += entry.amount;
            }
        }

        if (totalReceived < EPSILON) continue;

        // Ideal shares - Consider ALL sources
        let totalPriorityAmongSources = 0;
        const priorities: Record<string, number> = {};

        for (const csId of sources) {
            const priority = ns.priority_distribution?.find(
                p => p.target_slot_id === csId
            )?.priority_percentage || 0;
            priorities[csId] = priority;
            totalPriorityAmongSources += priority;
        }

        if (totalPriorityAmongSources > EPSILON) {
            for (const csId of sources) {
                const currentAmount = matrix[csId][ns.id].amount;
                const actual = currentAmount / totalReceived;
                const ideal = priorities[csId] / totalPriorityAmongSources;

                if (!deviations[csId]) deviations[csId] = {};
                const current = deviations[csId][ns.id] || 0;
                deviations[csId][ns.id] = current + (actual - ideal) * (1 - alpha);
            }
        }
    }

    return deviations;
}

export function makeAdjustments(
    matrix: SlotAllocationMatrix,
    deviations: Record<string, Record<string, number>>,
    capacitySlots: AvailabilitySlot[],
    needSlots: NeedSlot[]
): boolean {
    let adjustmentsMade = false;

    // Adjust per capacity slot
    for (const csId in deviations) {
        const row = deviations[csId];

        // Identify over/under allocated
        const over: string[] = [];
        const under: string[] = [];

        for (const nsId in row) {
            if (row[nsId] > EPSILON) over.push(nsId);
            else if (row[nsId] < -EPSILON) under.push(nsId);
        }

        if (over.length === 0 && under.length === 0) continue;

        // Calculate available capacity (unused)
        let totalAllocated = 0;
        for (const nsId in matrix[csId]) {
            totalAllocated += matrix[csId][nsId].amount;
        }

        // Find capacity slot quantity
        const cs = capacitySlots.find(c => c.id === csId);
        if (!cs) continue;

        // Pool starts with unused capacity
        // We limit how much unused capacity we use per iteration to avoid oscillation? 
        // Or just use what's needed.
        // Let's use up to available.
        let pool = Math.max(0, cs.quantity - totalAllocated);

        // Also add from reductions
        // Reduce over-allocated
        for (const nsId of over) {
            const reduction = Math.min(
                matrix[csId][nsId].amount * MAX_ADJUSTMENT_PER_ITERATION,
                matrix[csId][nsId].amount
            );
            if (reduction > EPSILON) {
                matrix[csId][nsId].amount -= reduction;
                pool += reduction;
                adjustmentsMade = true;
            }
        }

        // Distribute to under-allocated (weighted by magnitude of lack)
        const totalUnderMagnitude = under.reduce((sum, id) => sum + Math.abs(row[id]), 0);

        for (const nsId of under) {
            const weight = Math.abs(row[nsId]) / totalUnderMagnitude;
            const increase = pool * weight;

            if (increase > EPSILON) {
                // Allow temporary overshoot of need (will be clamped globally later)
                // This enables "squeeze in" behavior for more aligned providers
                const actualIncrease = increase;

                if (actualIncrease > EPSILON) {
                    matrix[csId][nsId].amount += actualIncrease;
                    adjustmentsMade = true;
                }
            }
        }
    }

    return adjustmentsMade;
}

/**
 * Enforce need limits by scaling down over-allocations globally
 * This runs after individual adjustments to resolve "overshoot"
 */
export function enforceNeedLimits(
    matrix: SlotAllocationMatrix,
    needSlots: NeedSlot[]
): boolean {
    let adjustmentsMade = false;

    for (const ns of needSlots) {
        if (!ns.id || ns.quantity === undefined) continue;

        // Calculate total received
        let totalReceived = 0;
        const sources: string[] = [];

        for (const csId in matrix) {
            const amt = matrix[csId]?.[ns.id]?.amount || 0;
            if (amt > EPSILON) {
                totalReceived += amt;
                sources.push(csId);
            }
        }

        // If over-allocated, scale down everyone proportionally
        if (totalReceived > ns.quantity + EPSILON) {
            const scale = ns.quantity / totalReceived;

            for (const csId of sources) {
                if (matrix[csId][ns.id]) {
                    const original = matrix[csId][ns.id].amount;
                    const newValue = original * scale;
                    matrix[csId][ns.id].amount = newValue;

                    if (Math.abs(original - newValue) > EPSILON) {
                        adjustmentsMade = true;
                    }
                }
            }
        }
    }

    return adjustmentsMade;
}

// ═══════════════════════════════════════════════════════════════════
// HELPER: FLATTEN & LOOKUP
// ═══════════════════════════════════════════════════════════════════

export function flattenMatrix(matrix: SlotAllocationMatrix): SlotAllocationRecord[] {
    const records: SlotAllocationRecord[] = [];

    for (const csId in matrix) {
        for (const nsId in matrix[csId]) {
            const entry = matrix[csId][nsId];
            if (entry.amount > EPSILON) {
                records.push({
                    capacity_slot_id: csId,
                    need_slot_id: nsId,
                    provider_pubkey: entry.providerPubKey,
                    recipient_pubkey: entry.recipientPubKey,
                    quantity: entry.amount,
                    withinPriorityLimit: entry.withinPriorityLimit,
                    fromSurplus: entry.fromSurplus
                });
            }
        }
    }

    return records;
}

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

    // 1. Explicit Priority (Schema format: Record<pubkey, number>)
    if (slot.priority_distribution && !Array.isArray(slot.priority_distribution)) {
        const val = (slot.priority_distribution as Record<string, number>)[personPubkey];
        if (val !== undefined) return val;
    }

    // 2. Fallback to Global Recognition
    if (commitment && commitment.global_recognition_weights) {
        return commitment.global_recognition_weights[personPubkey] || 0;
    }

    return 0;
}
