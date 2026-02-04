import { z } from 'zod';
import jsonLogic from 'json-logic-js';
import { SkillSchema } from './resources';
import { AvailabilityWindowSchema } from './time';
import { nanoid } from 'nanoid';

// =============================================================================
// ID TYPES
// =============================================================================

export const CID = z.string().regex(/^[a-f0-9]{64}$/);
export type CID = z.infer<typeof CID>;

export const NanoId = z.string().min(10).max(32);
export type NanoId = z.infer<typeof NanoId>;

// =============================================================================
// CONTENT ADDRESSING
// =============================================================================

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

async function sha256Hex(str: string): Promise<string> {
    if (typeof globalThis !== 'undefined' && globalThis.crypto?.subtle) {
        const buf = new TextEncoder().encode(str);
        const hashBuf = await globalThis.crypto.subtle.digest('SHA-256', buf);
        return Array.from(new Uint8Array(hashBuf)).map(b => b.toString(16).padStart(2, '0')).join('');
    } else {
        const { createHash } = await import('crypto');
        return createHash('sha256').update(str).digest('hex');
    }
}

// =============================================================================
// ACCEPTANCE LOGIC
// =============================================================================

const AutomaticAcceptance = z.object({
    type: z.literal('automatic'),
    rule: z.record(z.any()),
});

const GovernedAcceptance = z.object({
    type: z.literal('governed'),
    rightHolder: z.enum(['offeror', 'other']),
    rightHolderIds: z.array(z.string()).optional(),
});

export const AcceptanceLogic = z.union([AutomaticAcceptance, GovernedAcceptance]);
export type AcceptanceLogic = z.infer<typeof AcceptanceLogic>;

export function checkAcceptance(logic: AcceptanceLogic, context: any): boolean {
    if (logic.type === 'automatic') {
        try { return jsonLogic.apply(logic.rule, context) === true; }
        catch { return false; }
    }
    return false;
}

// =============================================================================
// DESCRIPTIONS
// =============================================================================

export const CommonsDescription = z.union([
    z.object({
        type: z.literal('templated_strict'),
        requirements: z.object({
            wordCount: z.number().optional(),
            characterCount: z.number().optional(),
            format: z.string().optional(),
        }),
        template: z.string(),
    }),
    z.object({
        type: z.literal('templated_lazy'),
        description: z.string(),
        template: z.string(),
    }),
    z.string(),
]);
export type CommonsDescription = z.infer<typeof CommonsDescription>;

// =============================================================================
// RESOURCE TEMPLATE / CONTEXT SPLIT
// =============================================================================
// Template: the hashable, content-addressable definition of WHAT is needed.
// Context: the instance-specific binding of WHERE/WHEN/WHO.
//
// This split is what allows a "need" to be a 1-slot commons, and the same
// abstract need ("10 hours of childcare") to be instantiated in Portland or
// Berlin without changing the template hash.

export const ResourceTemplate = z.object({
    type_id: z.string().min(1),
    quantity: z.number().gte(0),
    unit: z.string().optional(),
    emoji: z.string().optional(),
    description: z.string().optional(),
    min_atomic_size: z.number().positive().optional(),
    max_participation: z.number().int().positive().optional(),
    max_concurrency: z.number().int().positive().optional(),
    min_calendar_duration: z.number().positive().optional(),
    required_skills: z.array(SkillSchema).optional(),
    filter_rule: z.any().optional(),
    mutual_agreement_required: z.boolean().default(false).optional(),
});
export type ResourceTemplate = z.infer<typeof ResourceTemplate>;

export const ResourceContext = z.object({
    author: z.string().optional(),
    offerer: z.string().optional(),
    time_zone: z.string().optional(),
    start_date: z.string().nullable().optional(),
    end_date: z.string().nullable().optional(),
    availability_window: AvailabilityWindowSchema.optional(),
    recurrence: z.enum(['daily', 'weekly', 'monthly', 'yearly']).nullable().optional(),
    advance_notice_hours: z.number().gte(0).optional(),
    booking_window_hours: z.number().gte(0).optional(),
    search_radius_km: z.number().gte(0).optional(),
    hidden_until_request_accepted: z.boolean().optional(),
    location_type: z.string().optional(),
    longitude: z.number().min(-180).max(180).optional(),
    latitude: z.number().min(-90).max(90).optional(),
    street_address: z.string().optional(),
    city: z.string().optional(),
    state_province: z.string().optional(),
    postal_code: z.string().optional(),
    country: z.string().optional(),
    online_link: z.string().url().or(z.string().length(0)).optional(),
    h3_index: z.string().optional(),
    h3_resolution: z.number().int().min(0).max(15).optional(),
    priority: z.number().optional(),
    priority_distribution: z.record(z.string(), z.number().min(0).max(1)).optional(),
});
export type ResourceContext = z.infer<typeof ResourceContext>;

// =============================================================================
// INPUT DEFINITIONS
// =============================================================================

const InputGeneric = z.object({
    kind: z.literal('generic'),
    data_type: z.enum(['string', 'number', 'boolean', 'option']),
    options: z.array(z.string()).optional(),
    description: z.string().optional(),
});

const InputResource = ResourceTemplate.extend({
    kind: z.literal('resource').default('resource'),
});

const InputCommons = z.object({
    kind: z.literal('commons'),
    template_id: z.string().optional(),
    instance_id: z.string().optional(),
});

export const InputDefinition = z.union([InputGeneric, InputResource, InputCommons]);
export type InputDefinition = z.infer<typeof InputDefinition>;

// =============================================================================
// SLOT (TEMPLATE)
// =============================================================================

export const Slot = z.object({
    id: CID.optional(),
    name: z.string(),
    description: z.string().optional(),
    input: InputDefinition,
    optional: z.boolean().default(false),
    acceptance_logic: AcceptanceLogic.optional(),
});
export type Slot = z.infer<typeof Slot>;

export const SlotWithId = Slot.required({ id: true });
export type SlotWithId = z.infer<typeof SlotWithId>;

// =============================================================================
// SLOT (INSTANCE)
// =============================================================================
// A slot instance is the stateful side: who filled it, with what context.
// Status is LOCAL: a slot is actual when it has fills. Period.
// No transitive graph walk. The fill itself is the social fact.
//
// Keyed by instance_id (NanoId), not slot_id (CID). The same slot template
// can appear multiple times in a commons (e.g. two childcare slots for
// different days). Each gets its own instance with its own context and fills.

export const SlotInstance = z.object({
    slot_id: CID,           // what kind of slot (template reference)
    instance_id: NanoId,    // this particular instantiation (unique key)
    resource_context: ResourceContext.optional(),
    filled_by: z.record(z.string(), z.union([z.boolean(), z.number(), z.string()]).optional()).optional(),
    status: z.enum(['potential', 'actual']).default('potential'),
});
export type SlotInstance = z.infer<typeof SlotInstance>;

// =============================================================================
// COMMONS (TEMPLATE)
// =============================================================================

export const Commons = z.object({
    id: CID.optional(),
    name: z.string(),
    description: CommonsDescription.optional(),
    slots: z.array(SlotWithId),
});
export type Commons = z.infer<typeof Commons>;

export const CommonsWithId = Commons.required({ id: true });
export type CommonsWithId = z.infer<typeof CommonsWithId>;

// =============================================================================
// COMMONS (INSTANCE)
// =============================================================================

export const CommonsInstanceCore = z.object({
    instance_id: NanoId,
    commons: CommonsWithId,
    author: z.string(),
    offerer: z.string().optional(),
    slotInstances: z.record(NanoId, SlotInstance), // keyed by slot instance_id
});
export type CommonsInstanceCore = z.infer<typeof CommonsInstanceCore>;

export const Progress = z.object({
    requiredSlotsFilled: z.number(),
    totalRequiredSlots: z.number(),
    optionalSlotsFilled: z.number(),
    totalOptionalSlots: z.number(),
    completionPercentage: z.number().min(0).max(100),
});

export type Progress = z.infer<typeof Progress>;

export const CommonsInstanceDerived = z.object({
    status: z.enum(['potential', 'actual']).default('potential'),
    progress: Progress.optional(),
});

export type CommonsInstanceDerived = z.infer<typeof CommonsInstanceDerived>;

export const CommonsInstanceMeta = z.object({
    created_at: z.date(),
    updated_at: z.date(),
});

export type CommonsInstanceMeta = z.infer<typeof CommonsInstanceMeta>;

export const CommonsInstance = CommonsInstanceCore.merge(CommonsInstanceDerived).merge(CommonsInstanceMeta);
export type CommonsInstance = z.infer<typeof CommonsInstance>;

// =============================================================================
// TEMPLATE FACTORIES
// =============================================================================
// Content-addressing is one operation: normalize → strip id → canonicalize → hash.
// All templates with an `id` field use the same pattern.

async function contentAddress<T extends { id?: string }>(
    schema: z.ZodType<T>,
    data: Omit<T, 'id'>,
): Promise<{ id: CID; data: T }> {
    const normalized = schema.parse(data) as T;
    const { id, ...hashable } = normalized;
    const cid = await sha256Hex(canonicalize(hashable)) as CID;
    return { id: cid, data: { ...normalized, id: cid } };
}

export async function createSlotWithId(slotData: Omit<Slot, 'id'>): Promise<SlotWithId> {
    const { data } = await contentAddress(Slot, slotData);
    return data as SlotWithId;
}

export async function createCommonsWithId(data: Omit<Commons, 'id'>): Promise<CommonsWithId> {
    const { data: result } = await contentAddress(Commons, data);
    return result as CommonsWithId;
}

export async function createCommonsFromSlots(
    name: string,
    slots: Omit<Slot, 'id'>[],
    description?: CommonsDescription,
): Promise<CommonsWithId> {
    const slotsWithIds = await Promise.all(slots.map(createSlotWithId));
    return createCommonsWithId({ name, description, slots: slotsWithIds });
}

// =============================================================================
// INSTANCE FACTORIES
// =============================================================================

export function createSlotInstance(slot_id: CID, context?: ResourceContext): SlotInstance {
    return SlotInstance.parse({
        slot_id,
        instance_id: nanoid(),
        resource_context: context,
        status: 'potential',
    });
}

// =============================================================================
// FILL EVENT
// =============================================================================
// Fills are temporal events. The reference graph between instances can have
// cycles (reciprocity). The DAG is in the ordering of fill events, not in the
// structure of references. See: research/commons/cycles-time-and-reciprocity.md

export const FillEvent = z.object({
    id: NanoId,
    timestamp: z.date(),
    commons_instance_id: NanoId,
    slot_instance_id: NanoId,  // addresses the specific slot instance, not the template
    filled_by: z.record(z.string(), z.union([z.boolean(), z.number(), z.string()]).optional()),
    author: z.string(),
});
export type FillEvent = z.infer<typeof FillEvent>;

// =============================================================================
// MANAGER
// =============================================================================
// The manager is simple because the hard constraints are temporal (causal ordering),
// not structural (graph shape). Cycles in the reference graph are reciprocity.
// Status is local: a slot is actual when it has fills.

export class CommonsManager {
    private registry = new Map<NanoId, CommonsInstanceCore & CommonsInstanceMeta>();
    private fillLog: FillEvent[] = [];

    // --- Reference index (for queries, not for enforcement) ---
    private referencedBy = new Map<NanoId, Set<NanoId>>();

    // Template + context → instance.
    // slotContexts is parallel to commons.slots (by index), not keyed by CID,
    // because the same slot template CID can appear multiple times.
    instantiate(
        commons: CommonsWithId,
        author: string,
        slotContexts?: (ResourceContext | undefined)[],
        offerer?: string,
    ): CommonsInstance {
        const slotInstances: Record<string, SlotInstance> = {};
        for (let i = 0; i < commons.slots.length; i++) {
            const slot = commons.slots[i];
            const inst = createSlotInstance(slot.id, slotContexts?.[i]);
            slotInstances[inst.instance_id] = inst;
        }

        const core = CommonsInstanceCore.parse({
            instance_id: nanoid(),
            commons,
            author,
            offerer,
            slotInstances,
        });

        const now = new Date();
        this.registry.set(core.instance_id, { ...core, created_at: now, updated_at: now });
        this.rebuildIndex();
        return this.get(core.instance_id)!;
    }

    // Fill a slot instance. Cycles are allowed — reciprocity is not a bug.
    // The only causal constraint: the referenced thing must already exist.
    fill(
        commonsInstanceId: NanoId,
        slotInstanceId: NanoId,
        filledBy: Record<string, boolean | number | string | undefined>,
        author: string,
    ): CommonsInstance {
        const stored = this.registry.get(commonsInstanceId);
        if (!stored) throw new Error(`Instance ${commonsInstanceId} not found`);

        const slotInstance = stored.slotInstances[slotInstanceId];
        if (!slotInstance) throw new Error(`Slot instance ${slotInstanceId} not found in instance ${commonsInstanceId}`);

        // Causal constraint: referenced instances must exist
        for (const ref of Object.keys(filledBy)) {
            if (NanoId.safeParse(ref).success && !this.registry.has(ref)) {
                throw new Error(`Referenced instance ${ref} does not exist (causal violation)`);
            }
        }

        // Record the fill
        slotInstance.filled_by = { ...slotInstance.filled_by, ...filledBy };
        slotInstance.status = 'actual';
        stored.updated_at = new Date();

        // Log the event
        this.fillLog.push({
            id: nanoid() as NanoId,
            timestamp: new Date(),
            commons_instance_id: commonsInstanceId,
            slot_instance_id: slotInstanceId,
            filled_by: filledBy as Record<string, boolean | number | string>,
            author,
        });

        this.rebuildIndex();
        return this.get(commonsInstanceId)!;
    }

    // Unfill a slot instance. Direct action, no cascading.
    unfill(commonsInstanceId: NanoId, slotInstanceId: NanoId): CommonsInstance {
        const stored = this.registry.get(commonsInstanceId);
        if (!stored) throw new Error(`Instance ${commonsInstanceId} not found`);

        const slotInstance = stored.slotInstances[slotInstanceId];
        if (!slotInstance) throw new Error(`Slot instance ${slotInstanceId} not found in instance ${commonsInstanceId}`);

        slotInstance.filled_by = undefined;
        slotInstance.status = 'potential';
        stored.updated_at = new Date();

        this.rebuildIndex();
        return this.get(commonsInstanceId)!;
    }

    // --- Queries ---

    get(id: string): CommonsInstance | undefined {
        const stored = this.registry.get(id);
        if (!stored) return undefined;
        return { ...stored, ...this.computeDerived(stored) };
    }

    all(): CommonsInstance[] {
        return Array.from(this.registry.values()).map(s => ({ ...s, ...this.computeDerived(s) }));
    }

    // Who references this instance? (for UI: "these commons depend on you")
    dependentsOf(id: NanoId): NanoId[] {
        return Array.from(this.referencedBy.get(id) ?? []);
    }

    // What does this instance reference? (for UI: "you depend on these")
    dependenciesOf(id: NanoId): NanoId[] {
        const stored = this.registry.get(id);
        if (!stored) return [];
        return Array.from(this.extractRefs(stored));
    }

    // Full fill history, ordered by time
    history(): FillEvent[] {
        return [...this.fillLog];
    }

    remove(id: NanoId): boolean {
        this.registry.delete(id);
        this.rebuildIndex();
        return true;
    }

    clear() {
        this.registry.clear();
        this.referencedBy.clear();
        this.fillLog = [];
    }

    // --- Derived state ---
    // Status is LOCAL. A slot is actual when it has fills.
    // A commons is actual when all required slots are actual.
    // No transitive graph walk. No supply-chain logic.

    private computeDerived(stored: CommonsInstanceCore & CommonsInstanceMeta): CommonsInstanceDerived {
        // Build a lookup from slot template CID → optional flag
        const templateOptional = new Map<CID, boolean>();
        for (const slot of stored.commons.slots) {
            templateOptional.set(slot.id, slot.optional);
        }

        let requiredFilled = 0, totalRequired = 0;
        let optionalFilled = 0, totalOptional = 0;

        for (const inst of Object.values(stored.slotInstances)) {
            const optional = templateOptional.get(inst.slot_id) ?? false;
            const filled = inst.filled_by && Object.keys(inst.filled_by).length > 0;

            if (optional) {
                totalOptional++;
                if (filled) optionalFilled++;
            } else {
                totalRequired++;
                if (filled) requiredFilled++;
            }
        }

        return {
            status: requiredFilled === totalRequired ? 'actual' : 'potential',
            progress: {
                requiredSlotsFilled: requiredFilled,
                totalRequiredSlots: totalRequired,
                optionalSlotsFilled: optionalFilled,
                totalOptionalSlots: totalOptional,
                completionPercentage: totalRequired > 0
                    ? Math.round((requiredFilled / totalRequired) * 100)
                    : 100,
            },
        };
    }

    // --- Index ---
    // The reference index tracks who-references-whom for queries and notifications.
    // It does NOT enforce acyclicity. Cycles = reciprocity.

    private rebuildIndex() {
        this.referencedBy.clear();
        for (const [id, stored] of this.registry.entries()) {
            for (const ref of this.extractRefs(stored)) {
                if (!this.referencedBy.has(ref)) this.referencedBy.set(ref, new Set());
                this.referencedBy.get(ref)!.add(id);
            }
        }
    }

    private extractRefs(core: CommonsInstanceCore): Set<NanoId> {
        const refs = new Set<NanoId>();
        for (const inst of Object.values(core.slotInstances)) {
            if (inst.filled_by) {
                for (const ref of Object.keys(inst.filled_by)) {
                    if (NanoId.safeParse(ref).success) refs.add(ref as NanoId);
                }
            }
        }
        // Also check template-level commons references
        for (const slot of core.commons.slots) {
            if (slot.input.kind === 'commons' && slot.input.instance_id) {
                if (NanoId.safeParse(slot.input.instance_id).success) {
                    refs.add(slot.input.instance_id as NanoId);
                }
            }
        }
        return refs;
    }
}

export const commons = new CommonsManager();
