import { z } from 'zod';
import jsonLogic from 'json-logic-js';
import { NeedSlotSchema } from './resources';
import { nanoid } from 'nanoid';

// =============================================================================
// ID TYPES
// =============================================================================

// Content-addressed ID (CID): 64 hex chars (sha256)
export const CID = z.string().regex(/^[a-f0-9]{64}$/);
export type CID = z.infer<typeof CID>;

// Instance ID (nanoid): 21 chars (default nanoid)
export const NanoId = z.string().min(10).max(32); // Accepts default nanoid, can adjust
export type NanoId = z.infer<typeof NanoId>;

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

// Generate a content-addressed ID for a proffer template
// This normalizes through Zod to apply defaults consistently, then excludes 'id' before hashing
export async function generateProfferTemplateId(profferData: Omit<z.infer<typeof Proffer>, 'id'>): Promise<CID> {
  // Parse through Zod to apply defaults consistently
  const normalized = Proffer.parse(profferData);

  // Explicitly exclude 'id' field
  const { id, ...hashableContent } = normalized;

  // Canonicalize and hash
  const canonical = canonicalize(hashableContent);
  return await sha256Hex(canonical) as CID;
}

// Helper to create a proffer with its computed ID
export async function createProfferWithId(profferData: Omit<z.infer<typeof Proffer>, 'id'>): Promise<ProfferWithId> {
  const id = await generateProfferTemplateId(profferData);
  return { ...profferData, id } as ProfferWithId;
}

// =============================================================================
// ACCEPTANCE LOGIC
// =============================================================================

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

export const ProfferDescription = z.union([
  TemplatedStrictDescription,
  TemplatedLazyDescription,
  z.string()
]);

export type ProfferDescription = z.infer<typeof ProfferDescription>;

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

// 2. Resource Demand (Extracted from NeedSlot)
// NOTE: We cannot use .omit() here because NeedSlot is a ZodEffect (due to .refine in resources.ts).
// So we re-define the shape required using zod.infer for type safety if needed, 
// OR we just assume the structure.
// Proffer V2 Resource Demand IS A Need Slot.
// We use the fields from NeedSlot but remove ID/Name which belong to the Slot container
const InputResource = NeedSlotSchema.omit({
  id: true,
  name: true,
  // Remove other slot-container-like fields if necessary, but NeedSlot is mostly content
  // We'll add the discriminator 'kind'
}).extend({
  kind: z.literal('resource').default('resource')
});


// 3. Proffer Demand (Nested Process)
const InputProffer = z.object({
  kind: z.literal('proffer'),
  template_id: z.string().optional(), // If referencing a template
  instance_id: z.string().optional()    // If referencing a specific existing instance
});

// Union of all Input Types
export const InputDefinition = z.union([
  InputGeneric,
  InputResource,
  InputProffer
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

// Slot with required ID (for use in Proffer templates)
export const SlotWithId = Slot.required({ id: true });
export type SlotWithId = z.infer<typeof SlotWithId>;

// SlotInstance holds instance/derived state for a slot
export const SlotInstance = z.object({
  slot_id: CID, // Reference to the Slot template's id
  instance_id: NanoId, // Unique instance identifier
  potential_filled_by_refs: z.record(z.string(), z.union([z.boolean(), z.number(), z.string()]).optional()).optional(),
  actually_filled_by_refs: z.record(z.string(), z.union([z.boolean(), z.number(), z.string()]).optional()).optional(),
  status: z.enum(['potential', 'actual']).default('potential'),
});

export type SlotInstance = z.infer<typeof SlotInstance>;

// =============================================================================
// PROFFER V2
// =============================================================================


// Pure Proffer Template (Content Addressable)

// Pure Proffer Template (Content Addressable, no author/offerer)
// id is optional to avoid circular dependency during hashing
export const Proffer = z.object({
  id: CID.optional(), // Content-addressed ID, computed from other fields
  name: z.string(),
  description: ProfferDescription.optional(),
  slots: z.array(SlotWithId), // Slots must have IDs computed
});

export type Proffer = z.infer<typeof Proffer>;

// Proffer with required ID (for use in instances)
export const ProfferWithId = Proffer.required({ id: true });
export type ProfferWithId = z.infer<typeof ProfferWithId>;

// Instance Metadata Wrapper (for stateful/derived fields)

// ProfferInstance holds the template and all instance/derived state, including slot instances


// Core instance data (who/what/instance content)
export const ProfferInstanceCore = z.object({
  instance_id: NanoId, // unique instance id (nanoid)
  proffer: ProfferWithId, // Proffer must have ID computed
  author: z.string(), // DID of the author
  offerer: z.string().optional(), // ID of Contact/Org author attests is offering
  slotInstances: z.record(CID, SlotInstance), // slot template id -> SlotInstance
});

export type ProfferInstanceCore = z.infer<typeof ProfferInstanceCore>;

// Derived state (computed, not persisted)
export const ProfferInstanceDerived = z.object({
  status: z.enum(['potential', 'actual']).default('potential'),
  progress: Progress.optional(),
});
export type ProfferInstanceDerived = z.infer<typeof ProfferInstanceDerived>;

// System metadata (timestamps, audit info)
export const ProfferInstanceMeta = z.object({
  created_at: z.date(),
  updated_at: z.date(),
  executed_at: z.date().optional(),
});
export type ProfferInstanceMeta = z.infer<typeof ProfferInstanceMeta>;

// Compose the full ProfferInstance
export const ProfferInstance = ProfferInstanceCore.merge(ProfferInstanceDerived).merge(ProfferInstanceMeta);
export type ProfferInstance = z.infer<typeof ProfferInstance>;

// =============================================================================
// REGISTRY / MANANGER
// =============================================================================



class ProfferManager {
  private registry = new Map<NanoId, ProfferInstanceCore & ProfferInstanceMeta>();
  private dependentsIndex = new Map<NanoId, Set<NanoId>>();

  getProfferInstance(id: string): ProfferInstance | undefined {
    const stored = this.registry.get(id);
    if (!stored) return undefined;
    const derived = this.computeDerived(stored);
    return { ...stored, ...derived };
  }

  addProfferInstance(core: ProfferInstanceCore, meta: ProfferInstanceMeta) {
    this.registry.set(core.instance_id, { ...core, ...meta });
    this.rebuildDependencyIndex();
  }

  updateProfferInstance(core: ProfferInstanceCore, meta: ProfferInstanceMeta) {
    this.registry.set(core.instance_id, { ...core, ...meta });
    this.rebuildDependencyIndex();
  }

  removeProfferInstance(instanceId: NanoId): boolean {
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


  getAllProfferInstances(): ProfferInstance[] {
    return Array.from(this.registry.values()).map(stored => ({ ...stored, ...this.computeDerived(stored) }));
  }

  private extractInstanceDependencies(core: ProfferInstanceCore): Set<NanoId> {
    const deps = new Set<NanoId>();

    for (const slotTemplate of core.proffer.slots) {
      const slotInstance = core.slotInstances[slotTemplate.id];
      if (!slotInstance) continue;

      // Nested proffer dependency (explicit)
      if (
        slotTemplate.input.kind === 'proffer' &&
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
  private computeDerived(stored: ProfferInstanceCore & ProfferInstanceMeta): ProfferInstanceDerived {
    const proffer = stored.proffer;
    const slotInstances = stored.slotInstances;
    // Status
    let status: 'potential' | 'actual' = 'actual';
    for (const slot of proffer.slots) {
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
    proffer.slots.forEach(slot => {
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

export const globalProfferRegistry = new ProfferManager();
