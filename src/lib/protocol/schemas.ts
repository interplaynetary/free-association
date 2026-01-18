/**
 * Allocation Schemas v4 - Multi-Dimensional Framework
 * 
 * MODULARIZATION REFACTOR:
 * This file now serves as an integration layer, re-exporting schemas from:
 * - resources.ts (Physics: Needs, Capacities, Slots)
 * - recognition.ts (Social: Weights, Trees, Attributes)
 * - allocation.ts (Economics: Records, Results)
 */

import * as z from 'zod';
import {
	AvailabilitySlotSchema,
	NeedSlotSchema,
	ITCStampSchema
} from './resources';
import { GlobalRecognitionWeightsSchema } from './recognition';
import { SlotAllocationRecordSchema } from './allocation';

export * from './resources';
export * from './recognition';
export * from './allocation';

// ═══════════════════════════════════════════════════════════════════
// COMMITMENT SCHEMA (Integration Layer)
// ═══════════════════════════════════════════════════════════════════

/**
 * Commitment - Integrated System State
 * 
 * Aggregates Resources, Recognition, and Allocation Signals into a single
 * transmissible packet for the current network protocol.
 */
export const CommitmentSchema = z.object({
	// Resources (Physics)
	capacity_slots: z.array(AvailabilitySlotSchema).optional(),
	need_slots: z.array(NeedSlotSchema).optional(),

	// Allocation (Economics/Logic)
	slot_allocations: z.array(SlotAllocationRecordSchema).optional(),

	// Recognition (Social)
	global_recognition_weights: GlobalRecognitionWeightsSchema.nullable().optional(),

	// Social Cache
	others_recognition_of_me: z.record(
		z.string(), // theirPubKey
		GlobalRecognitionWeightsSchema // Their full recognition weights
	).nullable().optional(),

	// Allocation Signals
	total_allocated: z.record(
		z.string(), // type_id
		z.record(z.string(), z.number().nonnegative())
	).optional(),

	distance_from_need: z.record(z.string(), z.number()).optional(),

	// Causality
	itcStamp: ITCStampSchema,
	timestamp: z.number().int().positive(),

	// Negotiation Signals (IPF)
	constraint_scaling_factors: z.record(z.string(), z.number().min(0).max(1)).optional(),
	total_seed_by_need: z.record(z.string(), z.number().nonnegative()).optional(),
	multi_dimensional_damping: z.record(z.string(), z.any()).optional()
});

export type Commitment = z.infer<typeof CommitmentSchema>;

// ═══════════════════════════════════════════════════════════════════
// CHAT SCHEMA (Legacy Compatibility)
// ═══════════════════════════════════════════════════════════════════

export const ChatReadStateSchema = z.object({
	lastRead: z.number().int().positive(),
	lastReadTimestamp: z.number().int().positive().optional(),
	updatedAt: z.number().optional(),
	_updatedAt: z.number().optional()
});

export type ChatReadState = z.infer<typeof ChatReadStateSchema>;

export const ChatReadStatesSchema = z.record(z.string(), ChatReadStateSchema);
export type ChatReadStates = z.infer<typeof ChatReadStatesSchema>;

// ═══════════════════════════════════════════════════════════════════
// LEGACY TYPE ALIASES
// ═══════════════════════════════════════════════════════════════════

export type BaseCapacity = Commitment;
export type ProviderCapacity = Commitment & { id?: string };
export type RecipientCapacity = Commitment;
export type CapacitiesCollection = Record<string, Commitment>;

// ═══════════════════════════════════════════════════════════════════
// VALIDATION HELPERS
// ═══════════════════════════════════════════════════════════════════

export function parseCommitment(data: unknown): Commitment | null {
	const result = CommitmentSchema.safeParse(data);
	if (!result.success) {
		console.warn('[SCHEMA-V4] Invalid commitment:', result.error);
		return null;
	}
	return result.data;
}
