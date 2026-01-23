import { z } from 'zod';
import { NeedSlotSchema } from './resources';

// Reuse the Acceptance Logic from V1
// We can assume this is stable enough to import or redefine if we want total separation.
// For V2 purity, let's redefine simply here to ensure we control the types.

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

// =============================================================================
// PROFFER V2 SLOTS
// =============================================================================

// A ProfferSlot IS A NeedSlot, plus Proffer-specific logic.
// This means every atomic part of a Proffer is a valid "Need" in the resource market.
export const ProfferSlotSchema = NeedSlotSchema.extend({
    // Logic for accepting the input (Capacity)
    // This overrides or augments the basic filter rules in NeedSlot
    acceptance_logic: AcceptanceLogicSchema.optional(),

    // Composition: This slot is satisfied by completing another Proffer
    // If present, the 'input' to this slot is the *Completion Event* of the nested proffer.
    nested_proffer_id: z.string().optional(),

    // State of the slot in the context of this specific Proffer instance
    status: z.enum(['empty', 'tentative', 'filled', 'verified']).default('empty'),

    // Link to the Capacity (or Proffer Result) that fills this slot
    filled_by_ref: z.string().optional(), // ID of Capacity or Proffer

    // Tentative booking reference (for two-phase commit)
    tentative_ref: z.string().optional()
});

export type ProfferSlot = z.infer<typeof ProfferSlotSchema>;

// =============================================================================
// PROFFER V2
// =============================================================================

export const ProfferV2Schema = z.object({
    id: z.string(),
    name: z.string(),
    description: z.string().optional(),

    // The molecular structure of needs
    // We use a flat list for simplicity, but IDs can encode hierarchy if needed (or parent_slot_id)
    slots: z.array(ProfferSlotSchema),

    // Proffer State
    status: z.enum(['draft', 'offered', 'tentative', 'active', 'completed', 'cancelled']).default('draft'),

    // Metadata
    created_at: z.date(),
    updated_at: z.date(),

    // Effects: What do we think happens when this Proffer completes?
    // This is NOT an "Output Capacity". It is a description of the state change / verifiable event.
    effects: z.array(z.object({
        type: z.string(), // e.g., "social_verification", "state_change"
        description: z.string(),
        data: z.record(z.any()).optional()
    })).optional()
});

export type ProfferV2 = z.infer<typeof ProfferV2Schema>;

// =============================================================================
// REGISTRY / MANANGER (Simplified for V2)
// =============================================================================

export class ProfferV2Manager {
    // Basic operations would go here (create, validate, fill)
    // For now, we just define the schema structure.
}
