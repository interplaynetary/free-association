import { z } from 'zod';
import jsonLogic from 'json-logic-js';
import { SkillSchema } from './resources';
import {AvailabilityWindowSchema } from './time';
import { nanoid } from 'nanoid';

// =============================================================================
// ID TYPES
// =============================================================================
// Content-addressed IDs make coordination knowledge "literally impossible to own."
// You cannot own a hash, just as you cannot own the number 7 or the Pythagorean theorem.
// If a hash represents "effective community broadband governance," then that governance
// pattern is a mathematical object—discoverable, improvable, forkable, but not ownable.
// This removes the substrate of intellectual property in coordination itself.

// Content-addressed ID (CID): 64 hex chars (sha256)
export const CID = z.string().regex(/^[a-f0-9]{64}$/);
export type CID = z.infer<typeof CID>;

// Instance ID (nanoid): 21 chars (default nanoid)
export const NanoId = z.string().min(10).max(32); // Accepts default nanoid, can adjust
export type NanoId = z.infer<typeof NanoId>;

// =============================================================================
// CONTENT ADDRESSING (TEMPLATE HASHING)
// =============================================================================
// Templates are content-addressed to ensure coordination patterns are citeable but not ownable.
// Capitalism requires "how to coordinate" remain proprietary (competitive advantage).
// Soviets required "how to coordinate" be centrally specified (bureaucratic control).
// Commons require only that coordination patterns be citeable—that we can point to a hash
// and say "this is how we're doing it, anyone can verify, anyone can use it."


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
    if (typeof globalThis !== 'undefined' && globalThis.crypto?.subtle) {
        // Browser/Web Crypto API
        const buf = new TextEncoder().encode(str);
        const hashBuf = await globalThis.crypto.subtle.digest('SHA-256', buf);
        return Array.from(new Uint8Array(hashBuf)).map(b => b.toString(16).padStart(2, '0')).join('');
    } else {
        // Node.js
        const { createHash } = await import('crypto');
        return createHash('sha256').update(str).digest('hex');
    }
}

// =============================================================================
// SLOT TEMPLATE HASHING
// =============================================================================

// Generate a content-addressed ID for a slot template
// This normalizes through Zod to apply defaults consistently, then excludes 'id' before hashing
export async function generateSlotTemplateId(slotData: Omit<z.infer<typeof Slot>, 'id'>): Promise<CID> {
    // Parse through Zod to apply defaults consistently
    const normalized = Slot.parse(slotData);

    // Explicitly exclude 'id' field (it should be undefined anyway, but be safe)
    const { id, ...hashableContent } = normalized;

    // Canonicalize and hash
    const canonical = canonicalize(hashableContent);
    return await sha256Hex(canonical) as CID;
}

// Helper to create a slot with its computed ID
export async function createSlotWithId(slotData: Omit<z.infer<typeof Slot>, 'id'>): Promise<SlotWithId> {
    const id = await generateSlotTemplateId(slotData);
    return { ...slotData, id } as SlotWithId;
}

// =============================================================================
// PROFFER TEMPLATE HASHING
// =============================================================================

// Generate a content-addressed ID for a commons template
// This normalizes through Zod to apply defaults consistently, then excludes 'id' before hashing
export async function generateCommonsTemplateId(commonsData: Omit<z.infer<typeof Commons>, 'id'>): Promise<CID> {
    // Parse through Zod to apply defaults consistently
    const normalized = Commons.parse(commonsData);

    // Explicitly exclude 'id' field
    const { id, ...hashableContent } = normalized;

    // Canonicalize and hash
    const canonical = canonicalize(hashableContent);
    return await sha256Hex(canonical) as CID;
}

// Helper to create a commons with its computed ID
export async function createCommonsWithId(commonsData: Omit<z.infer<typeof Commons>, 'id'>): Promise<CommonsWithId> {
    const id = await generateCommonsTemplateId(commonsData);
    return { ...commonsData, id } as CommonsWithId;
}

// Helper to go from raw slot data directly to a complete, hashed commons template.
// Eliminates the async dance of hashing each slot individually before assembling.
export async function createCommonsFromSlots(
    name: string,
    slots: Omit<z.infer<typeof Slot>, 'id'>[],
    description?: CommonsDescription,
): Promise<CommonsWithId> {
    const slotsWithIds = await Promise.all(slots.map(createSlotWithId));
    return createCommonsWithId({ name, description, slots: slotsWithIds });
}

// =============================================================================
// ACCEPTANCE LOGIC
// =============================================================================
// Commons make governance explicit. We have not solved governance—we have refused to hide it.
// Capitalist markets hide governance behind "voluntary exchange" (embedded in power structures).
// Soviet plans hide governance behind "scientific necessity" (bureaucratic decisions as objective).
// Commons ask: "Who decides what fits?" and provide a space for that question to be
// answered contextually, explicitly, accountably—through automatic rules or governed acceptance.

// Reuse the Acceptance Logic from V1
const AutomaticAcceptance = z.object({
    type: z.literal('automatic'),
    rule: z.record(z.any()) // JsonLogic rule
});

const GovernedAcceptance = z.object({
    type: z.literal('governed'),
    rightHolder: z.enum(['offeror', 'other']),
    rightHolderIds: z.array(z.string()).optional()
});

export const AcceptanceLogic = z.union([AutomaticAcceptance, GovernedAcceptance]);
export type AcceptanceLogic = z.infer<typeof AcceptanceLogic>;

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

const SlotTiming = z.enum(['proposal', 'execution', 'completion']);

// =============================================================================
// TEMPLATES & DESCRIPTIONS
// =============================================================================
// Templates describe communal needs (slots) rather than discrete outputs.
// This inverts the capitalist sequence:
//   NOT: Private labor → Output → Exchange → Social recognition
//   BUT: Social need → Slot → Contribution → Completion
// Labor is posited as social BEFORE it begins, not after it completes.

const TemplatedStrictDescription = z.object({
    type: z.literal('templated_strict'),
    requirements: z.object({
        wordCount: z.number().optional(),
        characterCount: z.number().optional(),
        format: z.string().optional()
    }),
    template: z.string()
});

const TemplatedLazyDescription = z.object({
    type: z.literal('templated_lazy'),
    description: z.string(),
    template: z.string()
});

export const CommonsDescription = z.union([
    TemplatedStrictDescription,
    TemplatedLazyDescription,
    z.string()
]);

export type CommonsDescription = z.infer<typeof CommonsDescription>;

// =============================================================================
// PROGRESS TRACKING
// =============================================================================

export const Progress = z.object({
    requiredSlotsFilled: z.number(),
    totalRequiredSlots: z.number(),
    optionalSlotsFilled: z.number(),
    totalOptionalSlots: z.number(),
    completionPercentage: z.number().min(0).max(100)
});

export type Progress = z.infer<typeof Progress>;

// =============================================================================
// INPUT DEFINITIONS (The Content)
// =============================================================================

// 1. Generic Data Input
const InputGeneric = z.object({
    kind: z.literal('generic'),
    data_type: z.enum(['string', 'number', 'boolean', 'option']),
    options: z.array(z.string()).optional(),
    description: z.string().optional()
});

// 2. Resource Demand — split into Template (hashable) and Context (instance-specific)
//
// BaseSlotSchema in resources.ts mixes structural identity with contextual binding.
// For content-addressing, only the structural definition of WHAT is needed gets hashed.
// WHERE/WHEN/WHO specifics live on the SlotInstance as ResourceContext.

// ResourceTemplate: the hashable, content-addressable definition of a resource need.
// These fields define WHAT is needed and the SHAPE of cooperation.
export const ResourceTemplate = z.object({
    type_id: z.string().min(1),
    quantity: z.number().gte(0),
    unit: z.string().optional(),
    emoji: z.string().optional(),
    description: z.string().optional(),

    // Throughput constraints (structural shape of cooperation)
    min_atomic_size: z.number().positive().optional(),
    max_participation: z.number().int().positive().optional(),
    max_concurrency: z.number().int().positive().optional(),
    min_calendar_duration: z.number().positive().optional(),

    // Capability requirements
    required_skills: z.array(SkillSchema).optional(),
    filter_rule: z.any().optional(),

    // Governance
    mutual_agreement_required: z.boolean().default(false).optional(),
});
export type ResourceTemplate = z.infer<typeof ResourceTemplate>;

// ResourceContext: instance-specific binding — WHERE/WHEN/WHO.
// These fields specify the particular circumstances of an instantiation.
// They do NOT affect the content-addressed hash of the template.
export const ResourceContext = z.object({
    // Identity
    author: z.string().optional(),
    offerer: z.string().optional(),

    // Time constraints
    time_zone: z.string().optional(),
    start_date: z.string().nullable().optional(),
    end_date: z.string().nullable().optional(),
    availability_window: AvailabilityWindowSchema.optional(),
    recurrence: z.enum(['daily', 'weekly', 'monthly', 'yearly']).nullable().optional(),
    advance_notice_hours: z.number().gte(0).optional(),
    booking_window_hours: z.number().gte(0).optional(),

    // Space constraints
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

    // Instance-specific weighting
    priority: z.number().optional(),
    priority_distribution: z.record(z.string(), z.number().min(0).max(1)).optional(),
});
export type ResourceContext = z.infer<typeof ResourceContext>;

const InputResource = ResourceTemplate.extend({
    kind: z.literal('resource').default('resource')
});


// 3. Commons Demand (Nested Process)
const InputCommons = z.object({
    kind: z.literal('commons'),
    template_id: z.string().optional(), // If referencing a template
    instance_id: z.string().optional()    // If referencing a specific existing instance
});

// Union of all Input Types
export const InputDefinition = z.union([
    InputGeneric,
    InputResource,
    InputCommons
]);

export type InputDefinition = z.infer<typeof InputDefinition>;

// Generic allocation schema, as compose schema?
// Offer of compose
// ???????
export const Compose = z.object({
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
// Slots are a priori social. They exist within a declared communal structure.
// The slot does not say "I will produce X and then we'll see if anyone wants it."
// It says "We need X for this communal purpose."
// Labor that fills it is already recognized as social labor, not labor that
// becomes social through subsequent exchange.


// Pure Slot Template (no instance/derived state)
// id is optional to avoid circular dependency during hashing
export const Slot = z.object({
    id: CID.optional(), // Content-addressed ID, computed from other fields
    name: z.string(),
    description: z.string().optional(),
    input: InputDefinition,
    optional: z.boolean().default(false),
    acceptance_logic: AcceptanceLogic.optional(),
});

export type Slot = z.infer<typeof Slot>;

// Slot with required ID (for use in Commons templates)
export const SlotWithId = Slot.required({ id: true });
export type SlotWithId = z.infer<typeof SlotWithId>;

// SlotInstance holds instance/derived state for a slot
export const SlotInstance = z.object({
    slot_id: CID, // Reference to the Slot template's id
    instance_id: NanoId, // Unique instance identifier
    resource_context: ResourceContext.optional(), // Instance-specific resource binding (WHERE/WHEN/WHO)
    potential_filled_by_refs: z.record(z.string(), z.union([z.boolean(), z.number(), z.string()]).optional()).optional(),
    actually_filled_by_refs: z.record(z.string(), z.union([z.boolean(), z.number(), z.string()]).optional()).optional(),
    status: z.enum(['potential', 'actual']).default('potential'),
});

export type SlotInstance = z.infer<typeof SlotInstance>;

// Helper to create a slot instance from a template slot ID + optional resource context
// Mirrors createSlotWithId on the template side
export function createSlotInstance(
    slot_id: CID,
    context?: ResourceContext
): SlotInstance {
    return SlotInstance.parse({
        slot_id,
        instance_id: nanoid(),
        resource_context: context,
        status: 'potential',
    });
}

// =============================================================================
// COMMONS
// =============================================================================
// A commons is:
//   - A template describing communal needs (slots)
//   - Content-addressed (its identity is its mathematical structure)
//   - Instantiated through contribution (filling slots)
//   - Compositional (commons reference other commons)
//   - Governed (acceptance is explicit, not market-determined)
//
// This dissolves the producer/consumer antithesis. If there are no outputs,
// there are no producers (in the sense of "those who own what they've made").
// There are only contributors to communal processes. All production is
// participation in ongoing composition.


// Pure Commons Template (Content Addressable)

// Pure Commons Template (Content Addressable, no author/offerer)
// id is optional to avoid circular dependency during hashing
export const Commons = z.object({
    id: CID.optional(), // Content-addressed ID, computed from other fields
    name: z.string(),
    description: CommonsDescription.optional(),
    slots: z.array(SlotWithId), // Slots must have IDs computed
});

export type Commons = z.infer<typeof Commons>;

// Commons with required ID (for use in instances)
export const CommonsWithId = Commons.required({ id: true });
export type CommonsWithId = z.infer<typeof CommonsWithId>;

// Instance Metadata Wrapper (for stateful/derived fields)

// CommonsInstance holds the template and all instance/derived state, including slot instances


// Core instance data (who/what/instance content)
export const CommonsInstanceCore = z.object({
    instance_id: NanoId, // unique instance id (nanoid)
    commons: CommonsWithId, // Commons must have ID computed
    author: z.string(), // DID of the author
    offerer: z.string().optional(), // ID of Contact/Org author attests is offering
    slotInstances: z.record(CID, SlotInstance), // slot template id -> SlotInstance
});

export type CommonsInstanceCore = z.infer<typeof CommonsInstanceCore>;

// Derived state (computed, not persisted)
export const CommonsInstanceDerived = z.object({
    status: z.enum(['potential', 'actual']).default('potential'),
    progress: Progress.optional(),
});
export type CommonsInstanceDerived = z.infer<typeof CommonsInstanceDerived>;

// System metadata (timestamps, audit info)
export const CommonsInstanceMeta = z.object({
    created_at: z.date(),
    updated_at: z.date(),
    executed_at: z.date().optional(),
});
export type CommonsInstanceMeta = z.infer<typeof CommonsInstanceMeta>;

// Compose the full CommonsInstance
export const CommonsInstance = CommonsInstanceCore.merge(CommonsInstanceDerived).merge(CommonsInstanceMeta);
export type CommonsInstance = z.infer<typeof CommonsInstance>;

// =============================================================================
// REGISTRY / MANANGER
// =============================================================================

class CommonsManager {
    private registry = new Map<NanoId, CommonsInstanceCore & CommonsInstanceMeta>();
    private dependentsIndex = new Map<NanoId, Set<NanoId>>();

    // The clean entry point: template + context → instance.
    // Template defines WHAT (hashable). Context binds WHERE/WHEN/WHO (per-slot).
    instantiate(
        commons: CommonsWithId,
        author: string,
        slotContexts?: Partial<Record<CID, ResourceContext>>,
        offerer?: string,
    ): CommonsInstance {
        const slotInstances: Record<string, SlotInstance> = {};
        for (const slot of commons.slots) {
            slotInstances[slot.id] = createSlotInstance(
                slot.id,
                slotContexts?.[slot.id]
            );
        }

        const core = CommonsInstanceCore.parse({
            instance_id: nanoid(),
            commons,
            author,
            offerer,
            slotInstances,
        });

        const now = new Date();
        const meta: CommonsInstanceMeta = { created_at: now, updated_at: now };
        this.addCommonsInstance(core, meta);
        return this.getCommonsInstance(core.instance_id)!;
    }

    getCommonsInstance(id: string): CommonsInstance | undefined {
        const stored = this.registry.get(id);
        if (!stored) return undefined;
        const derived = this.computeDerived(stored);
        return { ...stored, ...derived };
    }

    addCommonsInstance(core: CommonsInstanceCore, meta: CommonsInstanceMeta) {
        this.registry.set(core.instance_id, { ...core, ...meta });
        this.rebuildDependencyIndex();
    }

    updateCommonsInstance(core: CommonsInstanceCore, meta: CommonsInstanceMeta) {
        this.registry.set(core.instance_id, { ...core, ...meta });
        this.rebuildDependencyIndex();
    }

    /**
     * Unfill a slot (transition from actual → potential) and propagate changes to dependents.
     * This intelligently updates derived state for all commonss that required this slot.
     * 
     * @param commonsInstanceId - The commons instance containing the slot
     * @param slotId - The slot template ID to unfill
     * @returns Array of affected commons instance IDs (including the original)
     */
    unfillSlot(commonsInstanceId: NanoId, slotId: CID): NanoId[] {
        const stored = this.registry.get(commonsInstanceId);
        if (!stored) {
            throw new Error(`Commons instance ${commonsInstanceId} not found`);
        }

        const slotInstance = stored.slotInstances[slotId];
        if (!slotInstance) {
            throw new Error(`Slot ${slotId} not found in commons instance ${commonsInstanceId}`);
        }

        // Clear the actual fills and set status to potential
        slotInstance.actually_filled_by_refs = undefined;
        slotInstance.status = 'potential';

        // Update the stored instance
        const updatedMeta: CommonsInstanceMeta = {
            ...stored,
            updated_at: new Date()
        };
        this.updateCommonsInstance(stored, updatedMeta);

        // Recursively propagate to all dependents
        const affected = new Set<NanoId>([commonsInstanceId]);
        this.propagateUnfillToDependents(commonsInstanceId, affected);

        return Array.from(affected);
    }

    /**
     * Fill a slot (transition from potential → actual) and propagate changes to dependents.
     * This intelligently updates derived state for all commonss that can now become actual.
     * 
     * @param commonsInstanceId - The commons instance containing the slot
     * @param slotId - The slot template ID to fill
     * @param filledByRefs - References to what fills this slot (commons instance IDs or resource refs)
     * @returns Array of affected commons instance IDs (including the original)
     */
    fillSlot(
        commonsInstanceId: NanoId,
        slotId: CID,
        filledByRefs: Record<string, boolean | number | string | undefined>
    ): NanoId[] {
        const stored = this.registry.get(commonsInstanceId);
        if (!stored) {
            throw new Error(`Commons instance ${commonsInstanceId} not found`);
        }

        const slotInstance = stored.slotInstances[slotId];
        if (!slotInstance) {
            throw new Error(`Slot ${slotId} not found in commons instance ${commonsInstanceId}`);
        }

        // Tentatively apply fill
        const prevRefs = slotInstance.actually_filled_by_refs;
        const prevStatus = slotInstance.status;
        slotInstance.actually_filled_by_refs = filledByRefs;
        slotInstance.status = 'actual';

        // Validate DAG integrity before committing
        this.rebuildDependencyIndex();
        const validation = this.validateInstanceDAG(commonsInstanceId);
        if (!validation.isValid) {
            slotInstance.actually_filled_by_refs = prevRefs;
            slotInstance.status = prevStatus;
            this.rebuildDependencyIndex();
            throw new Error(`Fill would create cycle: ${validation.cyclePath?.join(' → ')}`);
        }

        // Commit
        const updatedMeta: CommonsInstanceMeta = {
            ...stored,
            updated_at: new Date()
        };
        this.updateCommonsInstance(stored, updatedMeta);

        // Recursively propagate to all dependents
        const affected = new Set<NanoId>([commonsInstanceId]);
        this.propagateFillToDependents(commonsInstanceId, affected);

        return Array.from(affected);
    }

    /**
     * Recursively propagate unfill status to all commonss that depend on the given commons.
     * This ensures derived state (status, progress) is consistent across the DAG.
     */
    private propagateUnfillToDependents(commonsInstanceId: NanoId, affected: Set<NanoId>) {
        const dependents = this.dependentsIndex.get(commonsInstanceId);
        if (!dependents) return;

        for (const dependentId of dependents) {
            if (affected.has(dependentId)) continue; // Avoid cycles

            const dependent = this.registry.get(dependentId);
            if (!dependent) continue;

            // Check if this dependent has any slots that reference the unfilled commons
            let needsUpdate = false;
            for (const [slotId, slotInstance] of Object.entries(dependent.slotInstances)) {
                // Check if this slot was filled by the unfilled commons
                if (slotInstance.actually_filled_by_refs?.[commonsInstanceId]) {
                    // Unfill this slot as well since its dependency is now only potential
                    slotInstance.actually_filled_by_refs = {
                        ...slotInstance.actually_filled_by_refs
                    };
                    delete slotInstance.actually_filled_by_refs[commonsInstanceId];

                    // If no more actual fills, mark as potential
                    if (Object.keys(slotInstance.actually_filled_by_refs).length === 0) {
                        slotInstance.actually_filled_by_refs = undefined;
                        slotInstance.status = 'potential';
                    }

                    needsUpdate = true;
                }
            }

            if (needsUpdate) {
                // Update the dependent with new timestamp
                const updatedMeta: CommonsInstanceMeta = {
                    ...dependent,
                    updated_at: new Date()
                };
                this.registry.set(dependentId, { ...dependent, ...updatedMeta });
                affected.add(dependentId);

                // Recursively propagate to dependents of this dependent
                this.propagateUnfillToDependents(dependentId, affected);
            }
        }
    }

    /**
     * Recursively propagate fill status to all commonss that depend on the given commons.
     * This checks if dependent commonss can now transition to actual status.
     */
    private propagateFillToDependents(commonsInstanceId: NanoId, affected: Set<NanoId>) {
        const dependents = this.dependentsIndex.get(commonsInstanceId);
        if (!dependents) return;

        for (const dependentId of dependents) {
            if (affected.has(dependentId)) continue; // Avoid cycles

            const dependent = this.registry.get(dependentId);
            if (!dependent) continue;

            // Check if this dependent has any slots that could now be filled
            let needsUpdate = false;
            for (const slotTemplate of dependent.commons.slots) {
                const slotInstance = dependent.slotInstances[slotTemplate.id];
                if (!slotInstance) continue;

                // Check if this slot references the filled commons and is currently potential
                if (slotInstance.status === 'potential') {
                    // Check if slot has potential fills that include the now-filled commons
                    if (slotInstance.potential_filled_by_refs?.[commonsInstanceId]) {
                        // Move from potential to actual if the dependency is now actual
                        const filledCommons = this.registry.get(commonsInstanceId);
                        if (filledCommons) {
                            const filledCommonsStatus = this.computeDerived(filledCommons).status;
                            if (filledCommonsStatus === 'actual') {
                                // Promote potential to actual
                                slotInstance.actually_filled_by_refs = {
                                    ...slotInstance.actually_filled_by_refs,
                                    [commonsInstanceId]: slotInstance.potential_filled_by_refs[commonsInstanceId]
                                };

                                // Re-derive slot status to see if it's now fully actual
                                const newSlotStatus = this.deriveSlotStatus(slotTemplate.id, dependent.slotInstances);
                                slotInstance.status = newSlotStatus;

                                needsUpdate = true;
                            }
                        }
                    }
                }
            }

            if (needsUpdate) {
                // Update the dependent with new timestamp
                const updatedMeta: CommonsInstanceMeta = {
                    ...dependent,
                    updated_at: new Date()
                };
                this.registry.set(dependentId, { ...dependent, ...updatedMeta });
                affected.add(dependentId);

                // Recursively propagate to dependents of this dependent
                this.propagateFillToDependents(dependentId, affected);
            }
        }
    }

    removeCommonsInstance(instanceId: NanoId): boolean {
        this.dependentsIndex.delete(instanceId);
        return this.registry.delete(instanceId);
    }

    clear() {
        this.registry.clear();
        this.dependentsIndex.clear();
    }

    private rebuildDependencyIndex() {
        this.dependentsIndex.clear();

        for (const [id, stored] of this.registry.entries()) {
            const deps = this.extractInstanceDependencies(stored);
            for (const dep of deps) {
                if (!this.dependentsIndex.has(dep)) {
                    this.dependentsIndex.set(dep, new Set());
                }
                this.dependentsIndex.get(dep)!.add(id);
            }
        }
    }


    getAllCommonsInstances(): CommonsInstance[] {
        return Array.from(this.registry.values()).map(stored => ({ ...stored, ...this.computeDerived(stored) }));
    }

    private extractInstanceDependencies(core: CommonsInstanceCore): Set<NanoId> {
        const deps = new Set<NanoId>();

        for (const slotTemplate of core.commons.slots) {
            const slotInstance = core.slotInstances[slotTemplate.id];
            if (!slotInstance) continue;

            // Nested commons dependency (explicit)
            if (
                slotTemplate.input.kind === 'commons' &&
                slotTemplate.input.instance_id &&
                NanoId.safeParse(slotTemplate.input.instance_id).success
            ) {
                deps.add(slotTemplate.input.instance_id as NanoId);
            }

            // Filled-by refs (implicit)
            if (slotInstance.actually_filled_by_refs) {
                for (const ref of Object.keys(slotInstance.actually_filled_by_refs)) {
                    if (NanoId.safeParse(ref).success) {
                        deps.add(ref as NanoId);
                    }
                }
            }
        }

        return deps;
    }

    validateAllDAGs() {
        const errors: string[] = [];
        for (const id of this.registry.keys()) {
            const result = this.validateInstanceDAG(id);
            if (!result.isValid) {
                errors.push(`Instance ${id}: ${result.cyclePath?.join(' → ')}`);
            }
        }
        return { isValid: errors.length === 0, errors };
    }

    private validateInstanceDAG(
        id: NanoId,
        visited = new Set<NanoId>(),
        visiting = new Set<NanoId>(),
        path: NanoId[] = []
    ): { isValid: boolean; cyclePath?: NanoId[] } {
        if (visiting.has(id)) {
            const idx = path.indexOf(id);
            return { isValid: false, cyclePath: path.slice(idx) };
        }

        if (visited.has(id)) return { isValid: true };

        visiting.add(id);
        const deps = this.extractInstanceDependencies(this.registry.get(id)!);

        for (const dep of deps) {
            if (!this.registry.has(dep)) {
                return { isValid: false, cyclePath: [id, dep] };
            }
            const res = this.validateInstanceDAG(dep, visited, visiting, [...path, id]);
            if (!res.isValid) return res;
        }

        visiting.delete(id);
        visited.add(id);
        return { isValid: true };
    }


    // --- Derived State Computation ---
    private computeDerived(stored: CommonsInstanceCore & CommonsInstanceMeta): CommonsInstanceDerived {
        const commons = stored.commons;
        const slotInstances = stored.slotInstances;
        // Status
        let status: 'potential' | 'actual' = 'actual';
        for (const slot of commons.slots) {
            if (!slot.optional) {
                const slotInstance = slotInstances[slot.id];
                if (!slotInstance) {
                    status = 'potential';
                    break;
                }
                if (this.deriveSlotStatus(slot.id, slotInstances) === 'potential') {
                    status = 'potential';
                    break;
                }
            }
        }
        // Progress
        let requiredFilled = 0;
        let totalRequired = 0;
        let optionalFilled = 0;
        let totalOptional = 0;
        commons.slots.forEach(slot => {
            const slotInstance = slotInstances[slot.id];
            const slotStatus = slotInstance ? this.deriveSlotStatus(slot.id, slotInstances) : 'potential';
            const isFilled = slotStatus === 'actual';
            if (slot.optional) {
                totalOptional++;
                if (isFilled) optionalFilled++;
            } else {
                totalRequired++;
                if (isFilled) requiredFilled++;
            }
        });
        const basePercentage = totalRequired > 0 ? (requiredFilled / totalRequired) * 100 : 100;
        return {
            status,
            progress: {
                requiredSlotsFilled: requiredFilled,
                totalRequiredSlots: totalRequired,
                optionalSlotsFilled: optionalFilled,
                totalOptionalSlots: totalOptional,
                completionPercentage: Math.round(basePercentage)
            }
        };
    }

    private deriveSlotStatus(
        slotId: string,
        slots: Record<string, SlotInstance>,
        seen = new Set<string>()
    ): 'potential' | 'actual' {
        if (seen.has(slotId)) return 'potential';
        seen.add(slotId);

        const slot = slots[slotId];
        if (!slot?.actually_filled_by_refs) return 'potential';

        for (const ref of Object.keys(slot.actually_filled_by_refs)) {
            if (slots[ref]) {
                if (this.deriveSlotStatus(ref, slots, seen) === 'potential') {
                    return 'potential';
                }
            }
        }

        return 'actual';
    }

}

export const globalCommonsRegistry = new CommonsManager();
