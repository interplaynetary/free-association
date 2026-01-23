import * as z from 'zod';

// ═══════════════════════════════════════════════════════════════════
// ALLOCATION RECORDS
// ═══════════════════════════════════════════════════════════════════

export const SlotAllocationRecordSchema = z.object({
    availability_slot_id: z.string().min(1),
    recipient_pubkey: z.string(),
    provider_pubkey: z.string().optional(),
    recipient_need_slot_id: z.string().optional(),
    quantity: z.number().nonnegative(),
    type_id: z.string().optional(),
    time_compatible: z.boolean(),
    location_compatible: z.boolean(),
    withinPriorityLimit: z.boolean(),
    fromSurplus: z.boolean(),
    tier: z.union([z.number().int().nonnegative(), z.string()]).optional(),
    seed_value: z.number().nonnegative().optional()
});

export type SlotAllocationRecord = z.infer<typeof SlotAllocationRecordSchema>;

export const TierDefinitionSchema = z.object({
    priority: z.number().int().nonnegative(),
    shares: z.record(z.string(), z.number().nonnegative()),
    label: z.string().optional()
});

export type TierDefinition = z.infer<typeof TierDefinitionSchema>;

// ═══════════════════════════════════════════════════════════════════
// ALLOCATION STATE & RESULTS
// ═══════════════════════════════════════════════════════════════════

export const PerTypeAllocationTotalsSchema = z.record(
    z.string(),
    z.record(z.string(), z.number().nonnegative())
);

export type PerTypeAllocationTotals = z.infer<typeof PerTypeAllocationTotalsSchema>;

export const PerTypeNeedStateSchema = z.object({
    type_id: z.string().min(1),
    residualNeed: z.number().nonnegative(),
    maxNeed: z.number().nonnegative(),
    lastAllocationReceived: z.number().nonnegative().default(0)
});

export type PerTypeNeedState = z.infer<typeof PerTypeNeedStateSchema>;

export const ConvergenceSummarySchema = z.object({
    totalNeedMagnitude: z.number().nonnegative(),
    previousNeedMagnitude: z.number().nonnegative(),
    contractionRate: z.number().nonnegative(),
    isConverged: z.boolean(),
    percentNeedsMet: z.number().min(0).max(100),
    percentNeedReduction: z.number().min(0).max(100).optional(),
    universalSatisfaction: z.boolean(),
    iterationsToConvergence: z.number().int().nullable(),
    currentIteration: z.number().int().nonnegative(),
    responseLatency: z.number().nonnegative(),
    maxPersonNeed: z.number().nonnegative().optional(),
    needVariance: z.number().nonnegative().optional(),
    peopleStuck: z.number().int().nonnegative().optional()
});

export type ConvergenceSummary = z.infer<typeof ConvergenceSummarySchema>;

export const AllocationResultSchema = z.object({
    allocations: z.array(SlotAllocationRecordSchema),
    slotDenominators: z.record(
        z.string(),
        z.object({
            mutual: z.number().nonnegative(),
            nonMutual: z.number().nonnegative(),
            type_id: z.string().min(1)
        })
    ),
    totalsByTypeAndRecipient: z.record(
        z.string(),
        z.record(z.string(), z.number().nonnegative())
    ),
    convergence: ConvergenceSummarySchema
});

export type AllocationResult = z.infer<typeof AllocationResultSchema>;

export function parseAllocationResult(data: unknown): AllocationResult | null {
    const result = AllocationResultSchema.safeParse(data);
    if (!result.success) {
        console.warn('[SCHEMA-V4] Invalid allocation result:', result.error);
        return null;
    }
    return result.data;
}

// ═══════════════════════════════════════════════════════════════════
// LOCAL ALLOCATION STATE (Store Schema)
// ═══════════════════════════════════════════════════════════════════

export const MyAllocationStateSchema = z.object({
    slot_allocations: z.array(SlotAllocationRecordSchema).default([]),
    total_allocated: z.record(
        z.string(),
        z.record(z.string(), z.number().nonnegative())
    ).default({}),
    distance_from_need: z.record(z.string(), z.number()).default({}),
    constraint_scaling_factors: z.record(z.string(), z.number().min(0).max(1)).default({}),
    total_seed_by_need: z.record(z.string(), z.number().nonnegative()).default({}),
    multi_dimensional_damping: z.record(z.string(), z.any()).default({})
});

export type MyAllocationState = z.infer<typeof MyAllocationStateSchema>;
