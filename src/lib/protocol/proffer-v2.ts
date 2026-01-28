import { z } from 'zod';
import jsonLogic from 'json-logic-js';
import { NeedSlotSchema } from './resources';


// =============================================================================
// CONTENT ADDRESSING (TEMPLATE HASHING)
// =============================================================================

// Canonicalize an object to a stable JSON string (sorted keys)
function canonicalize(obj: any): string {
    if (Array.isArray(obj)) {
        return '[' + obj.map(canonicalize).join(',') + ']';
    } else if (obj && typeof obj === 'object' && obj.constructor === Object) {
        return '{' + Object.keys(obj).sort().map(
            k => JSON.stringify(k) + ':' + canonicalize(obj[k])
        ).join(',') + '}';
    } else {
        return JSON.stringify(obj);
    }
}

// Hash a string using SHA-256 and return hex
async function sha256Hex(str: string): Promise<string> {
    if (typeof window !== 'undefined' && window.crypto?.subtle) {
        // Browser/Web Crypto API
        const buf = new TextEncoder().encode(str);
        const hashBuf = await window.crypto.subtle.digest('SHA-256', buf);
        return Array.from(new Uint8Array(hashBuf)).map(b => b.toString(16).padStart(2, '0')).join('');
    } else {
        // Node.js
        const { createHash } = await import('crypto');
        return createHash('sha256').update(str).digest('hex');
    }
}

/**
 * Generate a content-addressed template ID for a Proffer template.
 * @param proffer The Proffer template object (no instance/derived state)
 * @returns Promise<string> SHA-256 hex hash of canonicalized template
 */
export async function getProfferTemplateId(proffer: Proffer): Promise<string> {
    // Remove the id field for pure content addressing, if desired:
    // const { id, ...rest } = proffer;
    // const canonical = canonicalize(rest);
    // But if id is part of the template, keep it:
    const canonical = canonicalize(proffer);
    return sha256Hex(canonical);
}

// =============================================================================
// ACCEPTANCE LOGIC
// =============================================================================

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


// Pure Slot Template (no instance/derived state)
export const SlotSchema = z.object({
    id: z.string(),
    name: z.string(),
    description: z.string().optional(),
    input: InputDefinitionSchema,
    optional: z.boolean().default(false),
    acceptance_logic: AcceptanceLogicSchema.optional(),
    phase: SlotTimingSchema.default('proposal'),
});

export type Slot = z.infer<typeof SlotSchema>;

// SlotInstance holds instance/derived state for a slot
export const SlotInstanceSchema = z.object({
    slot_id: z.string(), // reference to SlotSchema.id
    potential_filled_by_refs: z.record(z.string(), z.union([z.boolean(), z.number(), z.string()]).optional()).optional(),
    actually_filled_by_refs: z.record(z.string(), z.union([z.boolean(), z.number(), z.string()]).optional()).optional(),
    status: z.enum(['potential', 'actual']).default('potential'),
});

export type SlotInstance = z.infer<typeof SlotInstanceSchema>;

// =============================================================================
// PROFFER V2
// =============================================================================


// Pure Proffer Template (Content Addressable)
export const ProfferSchema = z.object({
    id: z.string(),
    name: z.string(),
    description: ProfferDescriptionSchema.optional(),

    author: z.string(), // DID of the author
    offerer: z.string().optional(), // ID of Contact/Org author attests is offering

    // The molecular structure of needs
    slots: z.array(SlotSchema),
});

export type Proffer = z.infer<typeof ProfferSchema>;

// Instance Metadata Wrapper (for stateful/derived fields)

// ProfferInstance holds the template and all instance/derived state, including slot instances
export const ProfferInstanceSchema = z.object({
    proffer: ProfferSchema,
    slotInstances: z.record(z.string(), SlotInstanceSchema), // slot_id -> SlotInstance
    status: z.enum(['potential', 'actual']).default('potential'),
    progress: ProgressSchema.optional(),
    created_at: z.date(),
    updated_at: z.date(),
    executed_at: z.date().optional(),
});

export type ProfferInstance = z.infer<typeof ProfferInstanceSchema>;

// =============================================================================
// REGISTRY / MANANGER
// =============================================================================



export class ProfferManager {
    private registry: Map<string, ProfferInstance> = new Map();
    private statusCache: Map<string, 'potential' | 'actual'> = new Map();
    private dependentsIndex: Map<string, Set<string>> = new Map(); // ChildID -> Set<ParentID>

    addProfferInstance(instance: ProfferInstance): void {
        this.registry.set(instance.proffer.id, instance);
        this.indexDependencies(instance);
        this.invalidateStatus(instance.proffer.id);
    }

    updateProfferInstance(instance: ProfferInstance): void {
        this.registry.set(instance.proffer.id, instance);
        this.indexDependencies(instance);
        this.invalidateStatus(instance.proffer.id);
    }

    getProfferInstance(id: string): ProfferInstance | undefined {
        return this.registry.get(id);
    }

    getProffer(id: string): Proffer | undefined {
        return this.registry.get(id)?.proffer;
    }

    getAllProfferInstances(): ProfferInstance[] {
        return Array.from(this.registry.values());
    }

    removeProffer(id: string): boolean {
        this.statusCache.delete(id);
        const parents = this.dependentsIndex.get(id);
        if (parents) {
            parents.forEach(p => this.invalidateStatus(p));
            this.dependentsIndex.delete(id);
        }
        return this.registry.delete(id);
    }

    private indexDependencies(instance: ProfferInstance) {
        // For each slotInstance, check actually_filled_by_refs for dependencies
        Object.values(instance.slotInstances).forEach(slotInstance => {
            if (slotInstance.actually_filled_by_refs) {
                Object.keys(slotInstance.actually_filled_by_refs).forEach(refId => {
                    if (!this.dependentsIndex.has(refId)) {
                        this.dependentsIndex.set(refId, new Set());
                    }
                    this.dependentsIndex.get(refId)?.add(instance.proffer.id);
                });
            }
        });
    }

    invalidateStatus(id: string) {
        if (this.statusCache.has(id)) {
            this.statusCache.delete(id);
        }
        const parents = this.dependentsIndex.get(id);
        if (parents) {
            parents.forEach(parentId => this.invalidateStatus(parentId));
        }
    }

    clear(): void {
        this.registry.clear();
    }

    validateAllDAGs(): { isValid: boolean; errors: string[] } {
        const errors: string[] = [];
        this.registry.forEach((instance, id) => {
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

        const instance = this.getProfferInstance(profferId);
        if (!instance) return { isValid: false, cyclePath: [`Unknown Proffer: ${profferId}`] };
        const proffer = instance.proffer;

        visiting.add(profferId);

        for (const slot of proffer.slots) {
            const slotInstance = instance.slotInstances[slot.id];
            if (slot.input.kind === 'proffer' && slot.input.proffer_id) {
                const nestedId = slot.input.proffer_id;
                const validation = this.validateProfferDAG(nestedId, visited, visiting, currentPath);
                if (!validation.isValid) return validation;
            }
            // Optionally, check slotInstance for further dependencies if needed
        }

        visiting.delete(profferId);
        visited.add(profferId);

        return { isValid: true };
    }

    deriveSlotStatus(slotInstance: SlotInstance): 'potential' | 'actual' {
        if (!slotInstance.actually_filled_by_refs || Object.keys(slotInstance.actually_filled_by_refs).length === 0) {
            return 'potential';
        }
        for (const refId of Object.keys(slotInstance.actually_filled_by_refs)) {
            if (this.statusCache.has(refId)) {
                if (this.statusCache.get(refId) === 'potential') {
                    return 'potential';
                }
                continue;
            }
            const potentialProffer = this.getProffer(refId);
            if (potentialProffer) {
                const childStatus = this.deriveProfferStatus(potentialProffer);
                if (childStatus === 'potential') {
                    return 'potential';
                }
            }
        }
        return 'actual';
    }

    deriveProfferStatus(proffer: Proffer): 'potential' | 'actual' {
        if (this.statusCache.has(proffer.id)) {
            return this.statusCache.get(proffer.id)!;
        }
        const instance = this.getProfferInstance(proffer.id);
        if (!instance) return 'potential';
        let status: 'potential' | 'actual' = 'actual';
        for (const slot of proffer.slots) {
            if (!slot.optional) {
                const slotInstance = instance.slotInstances[slot.id];
                if (!slotInstance) {
                    status = 'potential';
                    break;
                }
                const slotStatus = this.deriveSlotStatus(slotInstance);
                if (slotStatus === 'potential') {
                    status = 'potential';
                    break;
                }
            }
        }
        this.statusCache.set(proffer.id, status);
        return status;
    }

    calculateProgress(proffer: Proffer): Progress {
        const instance = this.getProfferInstance(proffer.id);
        if (!instance) {
            return {
                requiredSlotsFilled: 0,
                totalRequiredSlots: proffer.slots.filter(s => !s.optional).length,
                optionalSlotsFilled: 0,
                totalOptionalSlots: proffer.slots.filter(s => s.optional).length,
                completionPercentage: 0
            };
        }
        let requiredFilled = 0;
        let totalRequired = 0;
        let optionalFilled = 0;
        let totalOptional = 0;
        let totalNestedProgress = 0;
        let nestedSlotCount = 0;
        proffer.slots.forEach(slot => {
            const slotInstance = instance.slotInstances[slot.id];
            const slotStatus = slotInstance ? this.deriveSlotStatus(slotInstance) : 'potential';
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
                if (nested) {
                    if (!slot.optional) {
                        nestedSlotCount++;
                        const nestedProgress = this.calculateProgress(nested);
                        totalNestedProgress += nestedProgress.completionPercentage;
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
