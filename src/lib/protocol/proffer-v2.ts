import { z } from 'zod';
import jsonLogic from 'json-logic-js';
import { NeedSlotSchema } from './resources';

// Reuse the Acceptance Logic from V1
const AutomaticAcceptanceSchema = z.object({
    type: z.literal('automatic'),
    rule: z.record(z.any()) // JsonLogic rule
});

const GovernedAcceptanceSchema = z.object({
    type: z.literal('governed'),
    rightHolder: z.enum(['offeror', 'other']),
    rightHolderIds: z.array(z.string()).optional()
});

export const AcceptanceLogicSchema = z.union([AutomaticAcceptanceSchema, GovernedAcceptanceSchema]);
export type AcceptanceLogic = z.infer<typeof AcceptanceLogicSchema>;

export function checkAcceptance(logic: AcceptanceLogic, context: any): boolean {
    if (logic.type === 'automatic') {
        try {
            return jsonLogic.apply(logic.rule, context) === true;
        } catch (e) {
            console.warn('JsonLogic evaluation failed:', e);
            return false;
        }
    }
    // Governed logic requires external signature/approval, so it is never "automatically" true.
    return false;
}

const SlotTimingSchema = z.enum(['proposal', 'execution', 'completion']);

// =============================================================================
// TEMPLATES & DESCRIPTIONS
// =============================================================================

const TemplatedStrictDescriptionSchema = z.object({
    type: z.literal('templated_strict'),
    requirements: z.object({
        wordCount: z.number().optional(),
        characterCount: z.number().optional(),
        format: z.string().optional()
    }),
    template: z.string()
});

const TemplatedLazyDescriptionSchema = z.object({
    type: z.literal('templated_lazy'),
    description: z.string(),
    template: z.string()
});

export const ProfferDescriptionSchema = z.union([
    TemplatedStrictDescriptionSchema,
    TemplatedLazyDescriptionSchema,
    z.string()
]);

export type ProfferDescription = z.infer<typeof ProfferDescriptionSchema>;

// =============================================================================
// PROGRESS TRACKING
// =============================================================================

export const ProgressSchema = z.object({
    requiredSlotsFilled: z.number(),
    totalRequiredSlots: z.number(),
    optionalSlotsFilled: z.number(),
    totalOptionalSlots: z.number(),
    completionPercentage: z.number().min(0).max(100)
});

export type Progress = z.infer<typeof ProgressSchema>;

// =============================================================================
// INPUT DEFINITIONS (The Content)
// =============================================================================

// 1. Generic Data Input
const GenericInputSchema = z.object({
    kind: z.literal('generic'),
    data_type: z.enum(['string', 'number', 'boolean', 'option']),
    options: z.array(z.string()).optional(),
    description: z.string().optional()
});

// 2. Resource Demand (Extracted from NeedSlot)
// NOTE: We cannot use .omit() here because NeedSlotSchema is a ZodEffect (due to .refine in resources.ts).
// So we re-define the shape required using zod.infer for type safety if needed, 
// OR we just assume the structure.
// Proffer V2 Resource Demand IS A Need Slot.
// We use the fields from NeedSlot but remove ID/Name which belong to the Slot container
const ResourceDemandSchema = NeedSlotSchema.omit({
    id: true,
    name: true,
    // Remove other slot-container-like fields if necessary, but NeedSlot is mostly content
    // We'll add the discriminator 'kind'
}).extend({
    kind: z.literal('resource').default('resource')
});


// 3. Proffer Demand (Nested Process)
const ProfferDemandSchema = z.object({
    kind: z.literal('proffer'),
    template_id: z.string().optional(), // If referencing a template
    proffer_id: z.string().optional()    // If referencing a specific existing instance
});

// Union of all Input Types
export const InputDefinitionSchema = z.union([
    GenericInputSchema,
    ResourceDemandSchema,
    ProfferDemandSchema
]);

export type InputDefinition = z.infer<typeof InputDefinitionSchema>;

// Generic allocation schema, as compose schema?
// Offer of compose
// ???????
export const ComposeSchema = z.object({
    id: z.string(),
    from: z.string(),
    to: z.string(),
    context: z.string(), // complexity explosion here
})

// Automation of Cooperation!!!
// Event -> PatternMatch -> Effect Map
// Phase -> Effect Map
// Completion -> Effect Map !
// DSLs

// =============================================================================
// SLOT CONTAINER
// =============================================================================

export const SlotSchema = z.object({
    id: z.string(),
    name: z.string(),
    description: z.string().optional(),

    // The Input Definition (What is needed?)
    input: InputDefinitionSchema,
    optional: z.boolean().default(false),
    acceptance_logic: AcceptanceLogicSchema.optional(),

    // In the case of multi-provider Need fulfillment, we need a mapping of capacity-ids -> quantities
    // Peraps also CONTEXT as to why compose (For consideration purposes)
    potential_filled_by_refs: z.record(z.string(), z.union([z.boolean(), z.number(), z.string()]).optional()).optional(),

    // In the case of multi-provider Need fulfillment, we need a mapping of capacity-ids -> quantities
    actually_filled_by_refs: z.record(z.string(), z.union([z.boolean(), z.number(), z.string()]).optional()).optional(), // ID of Capacity, Resource, or Proffer Completion Event

    // Logic & timing
    phase: SlotTimingSchema.default('proposal'), // this is currently unused, we are treating as manual,but it should be progress checking (derived)

    // State
    status: z.enum(['potential', 'actual']).default('potential'),
});

export type Slot = z.infer<typeof SlotSchema>;

// =============================================================================
// PROFFER V2
// =============================================================================

export const ProfferSchema = z.object({
    id: z.string(),
    name: z.string(),
    description: ProfferDescriptionSchema.optional(),

    // The molecular structure of needs
    slots: z.array(SlotSchema),

    // Proffer State
    status: z.enum(['potential', 'actual']).default('potential'),
    progress: ProgressSchema.optional(),

    // Metadata
    created_at: z.date(),
    updated_at: z.date(),
    // track execution?
    executed_at: z.date().optional(),
});

export type Proffer = z.infer<typeof ProfferSchema>;

// =============================================================================
// REGISTRY / MANANGER
// =============================================================================

export class ProfferManager {
    private registry: Map<string, Proffer> = new Map();
    private statusCache: Map<string, 'potential' | 'actual'> = new Map();
    private dependentsIndex: Map<string, Set<string>> = new Map(); // ChildID -> Set<ParentID>

    addProffer(proffer: Proffer): void {
        this.registry.set(proffer.id, proffer);
        this.indexDependencies(proffer);
        this.invalidateStatus(proffer.id);
    }

    updateProffer(proffer: Proffer): void {
        // Just overwrite for now, but ensure we re-index and invalidate
        this.registry.set(proffer.id, proffer);

        // Re-indexing is tricky if dependencies CHANGED (removed).
        // For now, we just Add new indices. Garbage collecting old indices is harder without diffing.
        // Assuming strict add-only or simple replacement for this prototype.
        this.indexDependencies(proffer);

        // CRITICAL: Invalidate this proffer's status (and its parents)
        this.invalidateStatus(proffer.id);
    }

    getProffer(id: string): Proffer | undefined {
        return this.registry.get(id);
    }

    getAllProffers(): Proffer[] {
        return Array.from(this.registry.values());
    }

    removeProffer(id: string): boolean {
        // We should also cleanup dependentsIndex but looking up parents is hard without iterating?
        // Actually we can iterate registry or just lazy clean.
        // For strictness, let's keep it simple for now and just delete.
        this.statusCache.delete(id);
        // We should notify parents that a dependency is gone?
        // Ideally we check usageIndex to see who relied on this.
        const parents = this.dependentsIndex.get(id);
        if (parents) {
            parents.forEach(p => this.invalidateStatus(p));
            this.dependentsIndex.delete(id);
        }
        return this.registry.delete(id);
    }

    private indexDependencies(proffer: Proffer) {
        // Who does THIS proffer depend on?
        // We iterate slots -> actually_filled_by_refs
        proffer.slots.forEach(slot => {
            if (slot.actually_filled_by_refs) {
                Object.keys(slot.actually_filled_by_refs).forEach(refId => {
                    // refId (Child) -> proffer.id (Parent)
                    if (!this.dependentsIndex.has(refId)) {
                        this.dependentsIndex.set(refId, new Set());
                    }
                    this.dependentsIndex.get(refId)?.add(proffer.id);
                });
            }
        });
    }

    // Invalidate status and climb up the dependency tree
    invalidateStatus(id: string) {
        if (this.statusCache.has(id)) {
            this.statusCache.delete(id);
        }

        // Notify parents (Dependents)
        const parents = this.dependentsIndex.get(id);
        if (parents) {
            parents.forEach(parentId => this.invalidateStatus(parentId));
        }
    }

    clear(): void {
        this.registry.clear();
    }

    // Validate DAG structure
    validateAllDAGs(): { isValid: boolean; errors: string[] } {
        const errors: string[] = [];
        this.registry.forEach((_, id) => {
            const validation = this.validateProfferDAG(id);
            if (!validation.isValid) {
                errors.push(`Proffer ${id}: ${validation.cyclePath?.join(' → ')}`);
            }
        });
        return { isValid: errors.length === 0, errors };
    }

    private validateProfferDAG(
        profferId: string,
        visited: Set<string> = new Set(),
        visiting: Set<string> = new Set(),
        path: string[] = []
    ): { isValid: boolean; cyclePath?: string[] } {
        const currentPath = [...path, profferId];

        if (visiting.has(profferId)) {
            const cycleStart = currentPath.indexOf(profferId);
            return { isValid: false, cyclePath: currentPath.slice(cycleStart) };
        }

        if (visited.has(profferId)) return { isValid: true };

        const proffer = this.getProffer(profferId);
        if (!proffer) return { isValid: false, cyclePath: [`Unknown Proffer: ${profferId}`] };

        visiting.add(profferId);

        for (const slot of proffer.slots) {
            // Check if input is a proffer reference
            if (slot.input.kind === 'proffer' && slot.input.proffer_id) {
                const nestedId = slot.input.proffer_id;
                const validation = this.validateProfferDAG(nestedId, visited, visiting, currentPath);
                if (!validation.isValid) return validation;
            }
        }

        visiting.delete(profferId);
        visited.add(profferId);

        return { isValid: true };
    }

    // Derive Slot Status based on fills
    deriveSlotStatus(slot: Slot): 'potential' | 'actual' {
        // 1. Must have references
        if (!slot.actually_filled_by_refs || Object.keys(slot.actually_filled_by_refs).length === 0) {
            return 'potential';
        }

        // 2. All references must be "actual"
        // If ref is a Proffer ID, we check its status.
        // If ref is something else (Resource?), we assume ACTUAL if it exists (for now).
        for (const refId of Object.keys(slot.actually_filled_by_refs)) {
            // Check cache FIRST if it's a known proffer (Optimization)
            if (this.statusCache.has(refId)) {
                if (this.statusCache.get(refId) === 'potential') {
                    return 'potential';
                }
                continue; // It's actual, check next ref
            }

            const potentialProffer = this.getProffer(refId);
            if (potentialProffer) {
                const childStatus = this.deriveProfferStatus(potentialProffer);
                if (childStatus === 'potential') {
                    return 'potential';
                }
            }
            // If not found in proffer registry, assume it's a leaf resource -> 'actual'
        }

        return 'actual';
    }

    // Derive Proffer Status (Memoized)
    deriveProfferStatus(proffer: Proffer): 'potential' | 'actual' {
        // 1. Check Cache
        if (this.statusCache.has(proffer.id)) {
            return this.statusCache.get(proffer.id)!;
        }

        // 2. Compute
        let status: 'potential' | 'actual' = 'actual';
        for (const slot of proffer.slots) {
            if (!slot.optional) {
                // Note: deriveSlotStatus will recursively call deriveProfferStatus for dependencies.
                // Since this DAG is validated (no cycles), this recursion terminates.
                const slotStatus = this.deriveSlotStatus(slot);
                if (slotStatus === 'potential') {
                    status = 'potential';
                    break;
                }
            }
        }

        // 3. Cache
        this.statusCache.set(proffer.id, status);
        return status;
    }

    // Calculate Progress
    calculateProgress(proffer: Proffer): Progress {
        let requiredFilled = 0;
        let totalRequired = 0;
        let optionalFilled = 0;
        let totalOptional = 0;

        let totalNestedProgress = 0;
        let nestedSlotCount = 0;

        proffer.slots.forEach(slot => {
            const slotStatus = this.deriveSlotStatus(slot);
            const isFilled = slotStatus === 'actual';

            if (slot.optional) {
                totalOptional++;
                if (isFilled) optionalFilled++;
            } else {
                totalRequired++;
                if (isFilled) requiredFilled++;
            }

            if (slot.input.kind === 'proffer' && slot.input.proffer_id) {
                const nested = this.getProffer(slot.input.proffer_id);
                if (nested && nested.progress) {
                    // Only count nested progress towards the weighted average if the slot is REQUIRED
                    if (!slot.optional) {
                        nestedSlotCount++;
                        totalNestedProgress += nested.progress.completionPercentage;
                    }
                }
            }
        });

        const basePercentage = totalRequired > 0 ? (requiredFilled / totalRequired) * 100 : 100;

        return {
            requiredSlotsFilled: requiredFilled,
            totalRequiredSlots: totalRequired,
            optionalSlotsFilled: optionalFilled,
            totalOptionalSlots: totalOptional,
            completionPercentage: Math.round(basePercentage)
        };
    }
}

export const globalProfferRegistry = new ProfferManager();
