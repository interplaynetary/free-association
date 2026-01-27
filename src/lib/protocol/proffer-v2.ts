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

// =============================================================================
// SLOT CONTAINER
// =============================================================================

export const SlotSchema = z.object({
    id: z.string(),
    name: z.string(),
    description: z.string().optional(),

    // The Input Definition (What is needed?)
    input: InputDefinitionSchema,

    // Logic & timing
    phase: SlotTimingSchema.default('proposal'),
    optional: z.boolean().default(false),
    acceptance_logic: AcceptanceLogicSchema.optional(),

    // State
    status: z.enum(['empty', 'tentative', 'filled', 'verified']).default('empty'),
    filled_by_ref: z.string().optional(), // ID of Capacity, Resource, or Proffer Completion Event
    tentative_ref: z.string().optional(),
    value: z.any().optional() // For generic inputs
});

export type Slot = z.infer<typeof SlotSchema>;

// =============================================================================
// PROFFER V2
// =============================================================================

export const ProfferV2Schema = z.object({
    id: z.string(),
    name: z.string(),
    description: ProfferDescriptionSchema.optional(),

    // The molecular structure of needs
    slots: z.array(SlotSchema),

    // Proffer State
    status: z.enum(['draft', 'offered', 'tentative', 'active', 'completed', 'cancelled']).default('draft'),
    progress: ProgressSchema.optional(),

    // Metadata
    created_at: z.date(),
    updated_at: z.date()
});

export type ProfferV2 = z.infer<typeof ProfferV2Schema>;

// =============================================================================
// REGISTRY / MANANGER
// =============================================================================

export class ProfferV2Manager {
    private registry: Map<string, ProfferV2> = new Map();

    addProffer(proffer: ProfferV2): void {
        this.registry.set(proffer.id, proffer);
    }

    getProffer(id: string): ProfferV2 | undefined {
        return this.registry.get(id);
    }

    getAllProffers(): ProfferV2[] {
        return Array.from(this.registry.values());
    }

    removeProffer(id: string): boolean {
        return this.registry.delete(id);
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

    // Calculate Progress
    calculateProgress(proffer: ProfferV2): Progress {
        let requiredFilled = 0;
        let totalRequired = 0;
        let optionalFilled = 0;
        let totalOptional = 0;

        let totalNestedProgress = 0;
        let nestedSlotCount = 0;

        proffer.slots.forEach(slot => {
            const isFilled = slot.status === 'filled' || slot.status === 'verified';

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

        let finalPercentage = basePercentage;
        if (nestedSlotCount > 0) {
            const avgNested = totalNestedProgress / nestedSlotCount;
            // Weighted 50/50 for now
            finalPercentage = (basePercentage + avgNested) / 2;
        }

        return {
            requiredSlotsFilled: requiredFilled,
            totalRequiredSlots: totalRequired,
            optionalSlotsFilled: optionalFilled,
            totalOptionalSlots: totalOptional,
            completionPercentage: Math.round(finalPercentage)
        };
    }
}

export const globalProfferV2Registry = new ProfferV2Manager();
