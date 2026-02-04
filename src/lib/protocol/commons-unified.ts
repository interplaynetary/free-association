import { z } from 'zod';
import jsonLogic from 'json-logic-js';
import { SkillSchema } from './resources';
import { AvailabilityWindowSchema } from './time';
import { nanoid } from 'nanoid';

// =============================================================================
// IDs
// =============================================================================

export const NanoId = z.string().min(10).max(32);
export type NanoId = z.infer<typeof NanoId>;

// =============================================================================
// ACCEPTANCE LOGIC
// =============================================================================

const AutomaticAcceptance = z.object({
    type: z.literal('automatic'),
    rule: z.any(),
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
// RESOURCE
// =============================================================================
// Everything about a resource: what it is, constraints, and context.
// No artificial separation between "definition" and "context".

export const Resource = z.object({
    // What
    type_id: z.string().min(1),
    quantity: z.number().gte(0),
    unit: z.string().optional(),
    emoji: z.string().optional(),
    description: z.string().optional(),

    // Constraints
    min_atomic_size: z.number().positive().optional(),
    max_participation: z.number().int().positive().optional(),
    max_concurrency: z.number().int().positive().optional(),
    min_calendar_duration: z.number().positive().optional(),
    required_skills: z.array(SkillSchema).optional(),
    filter_rule: z.any().optional(),
    mutual_agreement_required: z.boolean().optional(),

    // Identity
    author: z.string().optional(),
    offerer: z.string().optional(),

    // Temporal
    time_zone: z.string().optional(),
    start_date: z.string().nullable().optional(),
    end_date: z.string().nullable().optional(),
    availability_window: AvailabilityWindowSchema.optional(),
    recurrence: z.enum(['daily', 'weekly', 'monthly', 'yearly']).nullable().optional(),
    advance_notice_hours: z.number().gte(0).optional(),
    booking_window_hours: z.number().gte(0).optional(),

    // Spatial
    search_radius_km: z.number().gte(0).optional(),
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

    // Allocation
    priority_distribution: z.record(z.string(), z.number().min(0).max(1)).optional(),
});
export type Resource = z.infer<typeof Resource>;

// =============================================================================
// INPUT DEFINITIONS
// =============================================================================

const InputGeneric = z.object({
    kind: z.literal('generic'),
    data_type: z.enum(['string', 'number', 'boolean', 'option']),
    options: z.array(z.string()).optional(),
    description: z.string().optional(),
});

const InputResource = Resource.extend({
    kind: z.literal('resource'),
});

const InputCommons = z.object({
    kind: z.literal('commons'),
    commons_id: z.string().optional(),
});

export const InputDefinition = z.union([InputGeneric, InputResource, InputCommons]);
export type InputDefinition = z.infer<typeof InputDefinition>;
export type InputResource = z.infer<typeof InputResource>;
export type InputCommons = z.infer<typeof InputCommons>;
export type InputGeneric = z.infer<typeof InputGeneric>;

// Input helpers
export const input = {
    resource: (type_id: string, quantity: number, opts?: Partial<Omit<Resource, 'type_id' | 'quantity'>>) => ({
        kind: 'resource' as const,
        type_id,
        quantity,
        ...opts,
    } satisfies InputResource),

    commons: (commons_id?: string) => ({
        kind: 'commons' as const,
        commons_id,
    } satisfies InputCommons),

    generic: (data_type: 'string' | 'number' | 'boolean' | 'option', opts?: { options?: string[], description?: string }) => ({
        kind: 'generic' as const,
        data_type,
        ...opts,
    } satisfies InputGeneric),
};

// =============================================================================
// SLOT
// =============================================================================

export const Slot = z.object({
    id: NanoId,
    name: z.string(),
    description: z.string().optional(),
    input: InputDefinition,
    optional: z.boolean().default(false),
    acceptance_logic: AcceptanceLogic.optional(),
    filled_by: z.record(z.string(), z.union([z.boolean(), z.number(), z.string()])).optional(),
});
export type Slot = z.infer<typeof Slot>;

export type SlotInput = {
    name: string;
    description?: string;
    input: InputDefinition;
    optional?: boolean;
    acceptance_logic?: AcceptanceLogic;
};

// =============================================================================
// COMMONS
// =============================================================================

export const Commons = z.object({
    id: NanoId,
    name: z.string(),
    description: CommonsDescription.optional(),
    author: z.string(),
    offerer: z.string().optional(),
    slots: z.array(Slot),
    created_at: z.date(),
    updated_at: z.date(),
});
export type Commons = z.infer<typeof Commons>;

export const Progress = z.object({
    requiredSlotsFilled: z.number(),
    totalRequiredSlots: z.number(),
    optionalSlotsFilled: z.number(),
    totalOptionalSlots: z.number(),
    completionPercentage: z.number().min(0).max(100),
});
export type Progress = z.infer<typeof Progress>;

export type CommonsWithState = Commons & {
    status: 'potential' | 'actual';
    progress: Progress;
};

// =============================================================================
// MANAGER
// =============================================================================

export class CommonsManager {
    private registry = new Map<NanoId, Commons>();
    private referencedBy = new Map<NanoId, Set<NanoId>>();

    /**
     * Create a commons.
     *
     * @example
     * manager.create({
     *   name: 'Block Party',
     *   author: 'alice',
     *   slots: [{
     *     name: 'Childcare',
     *     input: input.resource('childcare', 10, { city: 'Portland' })
     *   }]
     * });
     */
    create(data: {
        name: string;
        description?: CommonsDescription;
        author: string;
        offerer?: string;
        slots: SlotInput[];
    }): CommonsWithState {
        const slots: Slot[] = data.slots.map(s => ({
            id: nanoid() as NanoId,
            name: s.name,
            description: s.description,
            input: s.input,
            optional: s.optional ?? false,
            acceptance_logic: s.acceptance_logic,
            filled_by: undefined,
        }));

        const commons: Commons = {
            id: nanoid() as NanoId,
            name: data.name,
            description: data.description,
            author: data.author,
            offerer: data.offerer,
            slots,
            created_at: new Date(),
            updated_at: new Date(),
        };

        this.registry.set(commons.id, commons);
        this.rebuildIndex();
        return this.getWithState(commons.id)!;
    }

    /** Fill a slot. Cycles allowed (reciprocity). */
    fill(
        commonsId: NanoId,
        slotId: NanoId,
        filledBy: Record<string, boolean | number | string>,
        author: string,
    ): CommonsWithState {
        const commons = this.registry.get(commonsId);
        if (!commons) throw new Error(`Commons ${commonsId} not found`);

        const slot = commons.slots.find(s => s.id === slotId);
        if (!slot) throw new Error(`Slot ${slotId} not found`);

        // Causal constraint: referenced commons must exist
        for (const ref of Object.keys(filledBy)) {
            if (NanoId.safeParse(ref).success && !this.registry.has(ref)) {
                throw new Error(`Referenced commons ${ref} does not exist`);
            }
        }

        slot.filled_by = { ...slot.filled_by, ...filledBy };
        commons.updated_at = new Date();

        this.rebuildIndex();
        return this.getWithState(commonsId)!;
    }

    /**
     * Fill by slot name (when unique).
     *
     * @example
     * manager.fillByName(commons.id, 'Childcare', { 'bob-id': true }, 'alice');
     */
    fillByName(
        commonsId: NanoId,
        slotName: string,
        filledBy: Record<string, boolean | number | string>,
        author: string,
    ): CommonsWithState {
        const commons = this.registry.get(commonsId);
        if (!commons) throw new Error(`Commons ${commonsId} not found`);

        const matches = commons.slots.filter(s => s.name === slotName);
        if (matches.length === 0) throw new Error(`No slot named "${slotName}"`);
        if (matches.length > 1) throw new Error(`Multiple slots named "${slotName}"`);

        return this.fill(commonsId, matches[0].id, filledBy, author);
    }

    /** Unfill a slot. */
    unfill(commonsId: NanoId, slotId: NanoId): CommonsWithState {
        const commons = this.registry.get(commonsId);
        if (!commons) throw new Error(`Commons ${commonsId} not found`);

        const slot = commons.slots.find(s => s.id === slotId);
        if (!slot) throw new Error(`Slot ${slotId} not found`);

        slot.filled_by = undefined;
        commons.updated_at = new Date();

        this.rebuildIndex();
        return this.getWithState(commonsId)!;
    }

    /** Unfill by name. */
    unfillByName(commonsId: NanoId, slotName: string): CommonsWithState {
        const commons = this.registry.get(commonsId);
        if (!commons) throw new Error(`Commons ${commonsId} not found`);

        const matches = commons.slots.filter(s => s.name === slotName);
        if (matches.length === 0) throw new Error(`No slot named "${slotName}"`);
        if (matches.length > 1) throw new Error(`Multiple slots named "${slotName}"`);

        return this.unfill(commonsId, matches[0].id);
    }

    // --- Queries ---

    get(id: NanoId): Commons | undefined {
        return this.registry.get(id);
    }

    getWithState(id: NanoId): CommonsWithState | undefined {
        const commons = this.registry.get(id);
        if (!commons) return undefined;
        return { ...commons, ...this.computeState(commons) };
    }

    all(): CommonsWithState[] {
        return Array.from(this.registry.values()).map(c => ({
            ...c,
            ...this.computeState(c),
        }));
    }

    dependentsOf(id: NanoId): NanoId[] {
        return Array.from(this.referencedBy.get(id) ?? []);
    }

    dependenciesOf(id: NanoId): NanoId[] {
        const commons = this.registry.get(id);
        if (!commons) return [];
        return Array.from(this.extractRefs(commons));
    }

    remove(id: NanoId): boolean {
        this.registry.delete(id);
        this.rebuildIndex();
        return true;
    }

    clear() {
        this.registry.clear();
        this.referencedBy.clear();
    }

    // --- Derived state: LOCAL status (not transitive) ---

    private computeState(commons: Commons): { status: 'potential' | 'actual'; progress: Progress } {
        let requiredFilled = 0, totalRequired = 0;
        let optionalFilled = 0, totalOptional = 0;

        for (const slot of commons.slots) {
            const filled = slot.filled_by && Object.keys(slot.filled_by).length > 0;

            if (slot.optional) {
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

    // --- Reference index ---

    private rebuildIndex() {
        this.referencedBy.clear();
        for (const [id, commons] of this.registry.entries()) {
            for (const ref of this.extractRefs(commons)) {
                if (!this.referencedBy.has(ref)) this.referencedBy.set(ref, new Set());
                this.referencedBy.get(ref)!.add(id);
            }
        }
    }

    private extractRefs(commons: Commons): Set<NanoId> {
        const refs = new Set<NanoId>();
        for (const slot of commons.slots) {
            if (slot.filled_by) {
                for (const ref of Object.keys(slot.filled_by)) {
                    if (NanoId.safeParse(ref).success) refs.add(ref as NanoId);
                }
            }
            if (slot.input.kind === 'commons' && slot.input.commons_id) {
                if (NanoId.safeParse(slot.input.commons_id).success) {
                    refs.add(slot.input.commons_id as NanoId);
                }
            }
        }
        return refs;
    }
}

export const commons = new CommonsManager();

// =============================================================================
// HELPERS
// =============================================================================

/**
 * Create a simple single-slot commons (a "need").
 *
 * @example
 * const childcare = need('Childcare', 'alice',
 *   input.resource('childcare', 10, { city: 'Portland' })
 * );
 */
export function need(
    name: string,
    author: string,
    input: InputDefinition,
    opts?: { description?: string; offerer?: string }
): CommonsWithState {
    return commons.create({
        name,
        author,
        description: opts?.description,
        offerer: opts?.offerer,
        slots: [{ name, input }],
    });
}

/**
 * Create a commons with multiple resource slots.
 *
 * @example
 * const party = resources('Block Party', 'alice', [
 *   ['Childcare', 'childcare', 10, { city: 'Portland' }],
 *   ['Food', 'food', 50, { city: 'Portland' }],
 * ]);
 */
export function resources(
    name: string,
    author: string,
    slots: Array<[name: string, type_id: string, quantity: number, opts?: Partial<Omit<Resource, 'type_id' | 'quantity'>>]>,
    opts?: { description?: CommonsDescription; offerer?: string }
): CommonsWithState {
    return commons.create({
        name,
        author,
        description: opts?.description,
        offerer: opts?.offerer,
        slots: slots.map(([slotName, type_id, quantity, resourceOpts]) => ({
            name: slotName,
            input: input.resource(type_id, quantity, resourceOpts),
        })),
    });
}
