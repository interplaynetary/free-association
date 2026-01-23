import * as z from 'zod';

// ═══════════════════════════════════════════════════════════════════
// FEASIBILITY (The "Possible")
// ═══════════════════════════════════════════════════════════════════

export const FeasibilityScoresSchema = z.object({
    time: z.number().min(0).max(1).default(1),
    location: z.number().min(0).max(1).default(1),
    skills: z.number().min(0).max(1).default(1),
    travel: z.number().min(0).max(1).default(1),
    resources: z.number().min(0).max(1).default(1), // e.g. quantity/budget

    /**
     * Social Trust / Affinity (0-1)
     * 1.0 = High trust / Close connection
     * 0.0 = Blocked / No trust
     */
    affinity: z.number().min(0).max(1).default(1),

    /**
     * Temporal Continuity (0-1)
     * Measures fragmentation of the matching time blocks.
     * 1.0 = Perfectly contiguous (one block covers the need)
     * < 1.0 = Fragmented (multiple smaller blocks needed to satisfy quantity)
     */
    continuity: z.number().min(0).max(1).default(1)
});

export type FeasibilityScores = z.infer<typeof FeasibilityScoresSchema>;

export const FeasibilityStatusSchema = z.discriminatedUnion('type', [
    z.object({
        type: z.literal('possible'),
        /**
         * 0.0 to 1.0 score indicating confidence or safety of this feasibility.
         * 1.0 = Perfectly safe/compatible.
         * < 1.0 = Feasible but with risk (e.g. tight travel time, partial skill match).
         */
        confidence: z.number().min(0).max(1).default(1.0),

        /**
         * Explanation of risks if confidence < 1.0
         */
        risk_factors: z.array(z.string()).optional(),

        /**
         * Granular scores for each dimension (0-1)
         */
        scores: FeasibilityScoresSchema.default({})
    }),
    z.object({
        type: z.literal('impossible'),
        // Why is it impossible?
        reasons: z.array(z.enum([
            'TIME_MISMATCH',
            'SKILL_MISMATCH',
            'LOCATION_MISMATCH',
            'EXCLUSION_RULE',
            'ALREADY_COMMITTED', // E.g. double booking
            'TRAVEL_TIME_VIOLATION', // Impossible due to travel time between commitments
            'OTHER'
        ])),
        /**
         * Granular scores for each dimension (0-1).
         * Impossible dimensions will ideally be near 0.
         */
        scores: FeasibilityScoresSchema.default({})
    })
]);

export type FeasibilityStatus = z.infer<typeof FeasibilityStatusSchema>;


// ═══════════════════════════════════════════════════════════════════
// DESIRE (The "Actual" / "Wanted")
// ═══════════════════════════════════════════════════════════════════

/**
 * Represents an agent's expression of intent toward a specific match.
 * Used for both Provider (offering) and Seeker (requesting).
 */
export const DesireSchema = z.object({
    /**
     * How much quantity does this agent want to contribute/consume
     * in this specific relationship?
     */
    quantity: z.number().nonnegative(),

    /**
     * Is this a "hard" commitment?
     * If true, the agent considers this effectively "booked" or "promised".
     * If false, it acts as a "soft" preference or tentative allocation.
     */
    locked: z.boolean().default(false),

    /**
     * Relative priority/importance of this specific desire.
     * Useful for tie-breaking or sorting.
     */
    priority: z.number().optional(),

    /**
     * Timestamp of when this desire was last expressed/updated.
     */
    updated_at: z.number().optional()
});

export type Desire = z.infer<typeof DesireSchema>;


// ═══════════════════════════════════════════════════════════════════
// SLOT RELATIONSHIP (The Matrix)
// ═══════════════════════════════════════════════════════════════════

/**
 * The core edge type representing the state between a Capacity Slot and a Need Slot.
 * Captures the 4-quadrant state:
 * 1. Suggestion (Feasible, No Desire)
 * 2. Pruned (Infeasible, No Desire)
 * 3. Valid (Feasible, Mutual Desire)
 * 4. Contradiction (Infeasible, Mutual Desire)
 */
export const SlotRelationshipSchema = z.object({
    // Identity
    id: z.string(), // Unique ID for this relationship edge (e.g. hash of slot IDs)
    capacity_id: z.string(),
    need_id: z.string(),

    // The Matrix of State
    feasibility: FeasibilityStatusSchema,

    // Two-sided Desire
    provider_desire: DesireSchema.optional(), // Computed from Provider agent
    seeker_desire: DesireSchema.optional(),   // Computed from Seeker agent

    // Derived State (Convenience)
    /**
     * The effective quantity agreed upon (usually min(provider, seeker)).
     * Only meaningful if both are present.
     */
    mutual_desire_quantity: z.number().default(0),

    /**
     * High-level classification of this relationship.
     * Derived from Feasibility and Desire presence.
     */
    status: z.enum(['suggestion', 'pruned', 'valid', 'contradiction'])
});

export type SlotRelationship = z.infer<typeof SlotRelationshipSchema>;
