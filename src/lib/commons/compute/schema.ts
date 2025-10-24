/**
 * Compute Schemas - Declarative Reactive Computation
 * 
 * Defines schemas for:
 * - Variable bindings (where data comes from)
 * - Output bindings (where results go)
 * - Computation definitions (what to compute)
 * - Reactive computation graphs (complete dataflow)
 * - Computation provenance (execution lineage)
 * 
 * These schemas enable declarative, verifiable, reactive computations
 * that can be stored, shared, and executed across the network.
 */

import * as z from 'zod';

// Import ITC from v2
import { ITCStampSchema } from '../schemas';

// ═══════════════════════════════════════════════════════════════════
// VARIABLE BINDING SCHEMAS
// ═══════════════════════════════════════════════════════════════════

/**
 * Variable Binding - Declares where a variable gets its value
 * 
 * Types:
 * - 'value': Static literal value
 * - 'subscription': Subscribe to a Holster path (reactive, uses .on())
 * - 'fetch': One-time fetch from Holster path (uses .get())
 * - 'local': Read from local state
 * - 'derived': Result from a previous computation
 */
export const VariableBindingSchema = z.discriminatedUnion('type', [
	// Static value
	z.object({
		type: z.literal('value'),
		value: z.any()
	}),
	
	// Subscribe to Holster path (reactive, .on())
	z.object({
		type: z.literal('subscription'),
		holster_path: z.string(),
		schema_type: z.string(),
		subscribe_to_user: z.optional(z.string()), // For cross-user subscriptions
		default_value: z.optional(z.any()) // Fallback if subscription empty
	}),
	
	// One-time fetch from Holster path (non-reactive, .get())
	z.object({
		type: z.literal('fetch'),
		holster_path: z.string(),
		schema_type: z.string(),
		fetch_from_user: z.optional(z.string()), // For cross-user fetch
		default_value: z.optional(z.any()), // Fallback if fetch returns null
		wait_ms: z.optional(z.number().gte(0).default(100)) // Wait time for response
	}),
	
	// Read from local state path
	z.object({
		type: z.literal('local'),
		state_path: z.string(), // e.g., 'myData.field.subfield'
		default_value: z.optional(z.any())
	}),
	
	// Reference a derived computation result
	z.object({
		type: z.literal('derived'),
		computation_id: z.string(), // Which computation produces this
		output_key: z.string(), // Which output from that computation
		default_value: z.optional(z.any())
	})
]);

/**
 * Output Binding - Declares where to store computation results
 * 
 * Types:
 * - 'holster': Persist to Holster path
 * - 'local': Store in local state
 * - 'memory': Keep in memory only (for derived computations)
 */
export const OutputBindingSchema = z.discriminatedUnion('type', [
	// Persist to Holster
	z.object({
		type: z.literal('holster'),
		holster_path: z.string(),
		schema_type: z.optional(z.string()), // For validation
		persist_debounce_ms: z.optional(z.number().gte(0))
	}),
	
	// Store in local state
	z.object({
		type: z.literal('local'),
		state_path: z.string()
	}),
	
	// Keep in memory (for chaining computations)
	z.object({
		type: z.literal('memory')
	})
]);

// ═══════════════════════════════════════════════════════════════════
// COMPUTATION DEFINITION SCHEMAS
// ═══════════════════════════════════════════════════════════════════

/**
 * Computation Definition - Declarative reactive computation
 * 
 * Defines:
 * - Input variables (and where they come from)
 * - Computation function (as serialized string or named function)
 * - Output bindings (where to store results)
 * 
 * Example:
 * ```typescript
 * {
 *   id: 'mutual_recognition',
 *   inputs: {
 *     myCommitment: { type: 'subscription', holster_path: 'allocation/commitment', ... },
 *     theirCommitment: { type: 'subscription', holster_path: '~{pubkey}/allocation/commitment', ... }
 *   },
 *   compute_fn: 'computeMutualRecognition', // Named function from registry
 *   outputs: {
 *     mutualRecognition: { type: 'holster', holster_path: 'results/mr' }
 *   }
 * }
 * ```
 */
export const ComputationSchema = z.object({
	id: z.string().min(1),
	
	// Input variable declarations
	inputs: z.record(z.string(), VariableBindingSchema),
	
	// Computation function (named function from registry or serialized code)
	compute_fn: z.string(),
	
	// Local variables that can be bound within computation closure
	local_bindings: z.optional(z.record(z.string(), VariableBindingSchema)),
	
	// Output mappings (resultKey → where to store)
	outputs: z.record(z.string(), OutputBindingSchema),
	
	// Execution control
	debounce_ms: z.optional(z.number().gte(0).default(0)), // Debounce re-computation
	enabled: z.optional(z.boolean().default(true)), // Can disable computation
	depends_on: z.optional(z.array(z.string())), // Explicit computation dependencies (for ordering)
	
	// Metadata
	description: z.optional(z.string()),
	version: z.optional(z.string())
});

// ═══════════════════════════════════════════════════════════════════
// REACTIVE COMPUTATION GRAPH SCHEMA
// ═══════════════════════════════════════════════════════════════════

/**
 * Reactive Computation Graph - Complete dataflow specification
 * 
 * Declares:
 * - All variables and their sources
 * - All computations and dependencies
 * - Output destinations
 * 
 * The runtime will:
 * - Set up subscriptions for all 'subscription' bindings
 * - React to changes and trigger computations
 * - Store results according to output bindings
 * - Handle computation ordering based on dependencies
 * 
 * Example use case: Mutual recognition calculation
 * ```typescript
 * {
 *   variables: {
 *     myTree: { type: 'subscription', holster_path: 'tree', schema_type: 'RootNode' },
 *     theirTree: { type: 'subscription', holster_path: '~{pubkey}/tree', schema_type: 'RootNode' }
 *   },
 *   computations: [
 *     {
 *       id: 'recognition',
 *       inputs: { myTree: {}, theirTree: {} }, // References variables
 *       compute_fn: 'shareOfGeneralFulfillment',
 *       outputs: { share: { type: 'holster', holster_path: 'recognition/share' } }
 *     }
 *   ]
 * }
 * ```
 */
export const ReactiveComputationGraphSchema = z.object({
	id: z.string().min(1),
	
	// Global variable declarations
	variables: z.record(z.string(), VariableBindingSchema),
	
	// Computation pipeline
	computations: z.array(ComputationSchema),
	
	// Metadata
	version: z.optional(z.string()),
	description: z.optional(z.string()),
	
	// Program hash - if provided, all holster paths are prefixed with this
	// If not provided, computed automatically from program structure
	// Used to namespace program data: ~pubkey/<program_hash>/<holster_path>
	program_hash: z.optional(z.string())
});

// ═══════════════════════════════════════════════════════════════════
// COMPUTATION PROVENANCE SCHEMAS (V2 - ITC-BASED)
// ═══════════════════════════════════════════════════════════════════

/**
 * Input Provenance - tracks where an input came from
 */
export const InputProvenanceSchema = z.object({
	source: z.enum(['value', 'subscription', 'fetch', 'local', 'derived']),
	path: z.string().optional(),
	contentHash: z.string(), // SHA-256 hash of the input data
	provenance: z.string().optional() // Reference to parent computation provenance ID
});

/**
 * Output Provenance - tracks what was produced
 */
export const OutputProvenanceSchema = z.object({
	path: z.string(),
	contentHash: z.string() // SHA-256 hash of the output data
});

/**
 * Computation Provenance - complete lineage of a computation execution (V2 - ITC-based)
 * 
 * This enables:
 * - Verifying computation legitimacy
 * - Tracking data lineage
 * - Reproducing computations
 * - Byzantine fault tolerance
 * 
 * V2 CHANGES:
 * - Uses ITC stamps instead of vector clocks
 * - More compact causality tracking
 */
export const ComputationProvenanceSchema = z.object({
	// Unique identifier for this provenance record
	id: z.string(),
	
	// Peer causality (when/who) - V2: ITC instead of vector clock
	itcStamp: ITCStampSchema,
	executedBy: z.string(), // pubkey of executor
	timestamp: z.number().int().positive(),
	
	// Computation identity (what)
	programHash: z.string(),      // Which program
	computationId: z.string(),    // Which computation within program
	computationHash: z.string(),  // Hash of the computation definition (for version tracking)
	
	// Input provenance (what data was used)
	inputs: z.record(z.string(), InputProvenanceSchema),
	
	// Output provenance (what was produced)
	outputs: z.record(z.string(), OutputProvenanceSchema),
	
	// Deterministic verification hash
	// Hash of (computationHash + sorted input hashes)
	// Allows others to verify: "If I had these inputs, would I get this output?"
	deterministicHash: z.string(),
	
	// Optional: parent provenance IDs (for lineage tracking)
	parents: z.array(z.string()).optional()
});

/**
 * Versioned Program Data with Provenance
 * Stored at: ~pubkey/<program_hash>/<holster_path>/<provenance_signature>
 */
export const VersionedProgramDataSchema = z.object({
	// The actual data
	data: z.any(),
	
	// Full provenance record
	provenance: ComputationProvenanceSchema,
	
	// Storage metadata
	_updatedAt: z.number().int().positive()
});

// ═══════════════════════════════════════════════════════════════════
// TYPE EXPORTS
// ═══════════════════════════════════════════════════════════════════

// Variable and output binding types
export type VariableBinding = z.infer<typeof VariableBindingSchema>;
export type OutputBinding = z.infer<typeof OutputBindingSchema>;

// Computation types
export type Computation = z.infer<typeof ComputationSchema>;
export type ReactiveComputationGraph = z.infer<typeof ReactiveComputationGraphSchema>;

// Provenance types
export type InputProvenance = z.infer<typeof InputProvenanceSchema>;
export type OutputProvenance = z.infer<typeof OutputProvenanceSchema>;
export type ComputationProvenance = z.infer<typeof ComputationProvenanceSchema>;
export type VersionedProgramData = z.infer<typeof VersionedProgramDataSchema>;

// ═══════════════════════════════════════════════════════════════════
// VALIDATION HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Validate and parse a reactive computation graph
 */
export function parseComputationGraph(data: unknown): ReactiveComputationGraph | null {
	const result = ReactiveComputationGraphSchema.safeParse(data);
	if (!result.success) {
		console.warn('[COMPUTE-SCHEMA] Invalid computation graph:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Validate and parse computation provenance
 */
export function parseComputationProvenance(data: unknown): ComputationProvenance | null {
	const result = ComputationProvenanceSchema.safeParse(data);
	if (!result.success) {
		console.warn('[COMPUTE-SCHEMA] Invalid computation provenance:', result.error);
		return null;
	}
	return result.data;
}

