/**
 * P2P OS Kernel - RDL Extension
 * 
 * Defines RDL-specific schemas and extensions for the P2P OS kernel:
 * - RDL program storage (ReactiveComputationGraph)
 * - RDL computation state (variables, results)
 * - RDL provenance (computation execution tracking)
 * 
 * This module extends kernel-core.ts with RDL-specific functionality.
 * Other language runtimes would have their own extension modules:
 * - kernel-sql.ts (SQL queries)
 * - kernel-wasm.ts (WASM modules)
 * - etc.
 */

import * as z from 'zod';

// Import RDL-specific schemas
import {
	ReactiveComputationGraphSchema,
	ComputationProvenanceSchema
} from './schema';

// Import generic types from kernel-core
import type {
	GenericProgramDefinition,
	GenericProgramRegistryEntry,
	GenericExecutionProvenance
} from './kernel-core';

import {
	ProgramMetadataSchema,
	ProgramStatusSchema,
	GenericProgramDefinitionSchema,
	GenericProgramRegistryEntrySchema
} from './kernel-core';

// ═══════════════════════════════════════════════════════════════════
// RDL PROGRAM SCHEMAS
// ═══════════════════════════════════════════════════════════════════

/**
 * RDL Program Definition
 * 
 * Wraps ReactiveComputationGraph in the generic program structure
 */
export const RDLProgramDefinitionSchema = GenericProgramDefinitionSchema.extend({
	program_type: z.literal('RDL'),
	program_data: ReactiveComputationGraphSchema
});

/**
 * RDL Program Registry Entry
 * Path: ~{pubKey}/programs/registry/{programHash}/
 */
export const RDLProgramRegistryEntrySchema = z.object({
	definition: RDLProgramDefinitionSchema,
	metadata: ProgramMetadataSchema,
	status: ProgramStatusSchema
});

// ═══════════════════════════════════════════════════════════════════
// RDL COMPUTE SCHEMAS
// ═══════════════════════════════════════════════════════════════════

/**
 * Variable State (RDL-specific)
 * Path: ~{pubKey}/compute/{programHash}/state/variables/{variableName}
 */
export const VariableStateSchema = z.any(); // Value can be any type

/**
 * Computation Result (RDL-specific)
 * Path: ~{pubKey}/compute/{programHash}/state/computations/{computationId}/
 */
export const ComputationResultSchema = z.object({
	result: z.any(),
	last_executed: z.number().int().positive(),
	execution_count: z.number().int().nonnegative()
});

/**
 * Computation State Metadata (RDL-specific)
 * Path: ~{pubKey}/compute/{programHash}/state/metadata/
 */
export const ComputationStateMetadataSchema = z.object({
	program_started_at: z.number().int().positive(),
	last_computation: z.number().int().positive(),
	total_executions: z.number().int().nonnegative()
});

/**
 * Computation State (RDL-specific)
 * Path: ~{pubKey}/compute/{programHash}/state/
 */
export const ComputationStateSchema = z.object({
	variables: z.record(z.string(), VariableStateSchema).optional(),
	computations: z.record(z.string(), ComputationResultSchema).optional(),
	metadata: ComputationStateMetadataSchema.optional()
});

/**
 * Output Value (RDL-specific)
 * Path: ~{pubKey}/compute/{programHash}/outputs/{outputKey}/
 */
export const OutputValueSchema = z.object({
	value: z.any(),
	holster_path: z.string(),
	updated_at: z.number().int().positive()
});

/**
 * Provenance Entry (RDL-specific)
 * Path: ~{pubKey}/compute/{programHash}/provenance/{provenanceId}/
 */
export const ProvenanceEntrySchema = z.object({
	record: ComputationProvenanceSchema,
	signature: z.string() // Cryptographic signature
});

/**
 * Compute Namespace (per RDL program)
 * Path: ~{pubKey}/compute/{programHash}/
 */
export const ComputeNamespaceSchema = z.object({
	state: ComputationStateSchema.optional(),
	outputs: z.record(z.string(), OutputValueSchema).optional(),
	provenance: z.record(z.string(), ProvenanceEntrySchema).optional()
});

/**
 * All Compute Namespaces (for all RDL programs)
 * Path: ~{pubKey}/compute/
 */
export const AllComputeNamespacesSchema = z.record(z.string(), ComputeNamespaceSchema);

// ═══════════════════════════════════════════════════════════════════
// TYPE EXPORTS
// ═══════════════════════════════════════════════════════════════════

// RDL program types
export type RDLProgramDefinition = z.infer<typeof RDLProgramDefinitionSchema>;
export type RDLProgramRegistryEntry = z.infer<typeof RDLProgramRegistryEntrySchema>;

// RDL compute types
export type VariableState = z.infer<typeof VariableStateSchema>;
export type ComputationResult = z.infer<typeof ComputationResultSchema>;
export type ComputationStateMetadata = z.infer<typeof ComputationStateMetadataSchema>;
export type ComputationState = z.infer<typeof ComputationStateSchema>;
export type OutputValue = z.infer<typeof OutputValueSchema>;
export type ProvenanceEntry = z.infer<typeof ProvenanceEntrySchema>;
export type ComputeNamespace = z.infer<typeof ComputeNamespaceSchema>;
export type AllComputeNamespaces = z.infer<typeof AllComputeNamespacesSchema>;

// ═══════════════════════════════════════════════════════════════════
// CONVERSION HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Wrap an RDL program in the generic program structure
 */
export function wrapRDLProgram(
	rdlProgram: any,
	metadata?: { version?: string; program_hash?: string }
): RDLProgramDefinition {
	return {
		program_type: 'RDL',
		language_version: metadata?.version || '1.0.0',
		program_data: rdlProgram,
		program_hash: metadata?.program_hash
	};
}

/**
 * Extract RDL program from generic wrapper
 */
export function unwrapRDLProgram(
	genericProgram: GenericProgramDefinition
): any | null {
	if (genericProgram.program_type !== 'RDL') {
		console.warn('[KERNEL-RDL] Not an RDL program:', genericProgram.program_type);
		return null;
	}
	return genericProgram.program_data;
}

/**
 * Check if a generic program is an RDL program
 */
export function isRDLProgram(program: GenericProgramDefinition): boolean {
	return program.program_type === 'RDL';
}

// ═══════════════════════════════════════════════════════════════════
// VALIDATION HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Validate RDL program registry entry
 */
export function parseRDLProgramRegistryEntry(data: unknown): RDLProgramRegistryEntry | null {
	const result = RDLProgramRegistryEntrySchema.safeParse(data);
	if (!result.success) {
		console.warn('[KERNEL-RDL] Invalid RDL program registry entry:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Validate compute namespace
 */
export function parseComputeNamespace(data: unknown): ComputeNamespace | null {
	const result = ComputeNamespaceSchema.safeParse(data);
	if (!result.success) {
		console.warn('[KERNEL-RDL] Invalid compute namespace:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Validate provenance entry
 */
export function parseProvenanceEntry(data: unknown): ProvenanceEntry | null {
	const result = ProvenanceEntrySchema.safeParse(data);
	if (!result.success) {
		console.warn('[KERNEL-RDL] Invalid provenance entry:', result.error);
		return null;
	}
	return result.data;
}

// ═══════════════════════════════════════════════════════════════════
// PATH HELPERS (RDL-Specific)
// ═══════════════════════════════════════════════════════════════════

/**
 * Build Holster paths for RDL compute namespace
 */
export const RDLPaths = {
	// Compute paths
	computeState: (pubKey: string, programHash: string) => 
		`~${pubKey}/compute/${programHash}/state`,
	computeVariable: (pubKey: string, programHash: string, variableName: string) => 
		`~${pubKey}/compute/${programHash}/state/variables/${variableName}`,
	computeResult: (pubKey: string, programHash: string, computationId: string) => 
		`~${pubKey}/compute/${programHash}/state/computations/${computationId}`,
	computeOutput: (pubKey: string, programHash: string, outputKey: string) => 
		`~${pubKey}/compute/${programHash}/outputs/${outputKey}`,
	computeProvenance: (pubKey: string, programHash: string, provenanceId: string) => 
		`~${pubKey}/compute/${programHash}/provenance/${provenanceId}`
};

// ═══════════════════════════════════════════════════════════════════
// BACKWARDS COMPATIBILITY EXPORTS
// ═══════════════════════════════════════════════════════════════════

/**
 * For backwards compatibility with code that expects the old
 * ProgramRegistryEntry type (which was RDL-specific)
 */
export const ProgramRegistryEntrySchema = RDLProgramRegistryEntrySchema;
export type ProgramRegistryEntry = RDLProgramRegistryEntry;

/**
 * For backwards compatibility - parse as RDL program
 */
export function parseProgramRegistryEntry(data: unknown): RDLProgramRegistryEntry | null {
	return parseRDLProgramRegistryEntry(data);
}

