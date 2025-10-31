/**
 * Reactive Dataflow Language (RDL) - Canonical Zod Schema Specification
 * 
 * This is the AUTHORITATIVE specification for RDL programs.
 * All other representations (JSON Schema, TypeScript interfaces, etc.)
 * are derived from these Zod schemas.
 * 
 * RDL enables declarative, reactive, verifiable computations that can be:
 * - Stored and shared across the network
 * - Validated before execution
 * - Executed with full provenance tracking
 * - Composed into complex dataflow programs
 * 
 * @version 1.0.0
 * @specification RDL-SPEC-2025-01
 */

import * as z from 'zod';

// Import ITC from v2
import { ITCStampSchema } from '../v2/schemas';

// ═══════════════════════════════════════════════════════════════════
// RDL CORE PRIMITIVES
// ═══════════════════════════════════════════════════════════════════

/**
 * Identifier - Valid names for variables, computations, etc.
 * 
 * Rules:
 * - Must start with letter or underscore
 * - Can contain letters, numbers, underscores, hyphens
 * - Case sensitive
 * 
 * Examples: "myVar", "compute_1", "allocation-v2", "_private"
 */
export const IdentifierSchema = z.string()
	.min(1)
	.regex(/^[a-zA-Z_][a-zA-Z0-9_-]*$/, 'Must start with letter or underscore, followed by alphanumeric, underscore, or hyphen');

/**
 * Holster Path - Valid paths for Holster storage
 * 
 * Rules:
 * - Can contain alphanumeric, underscore, slash, dot, tilde, hyphen
 * - Used for both local and remote paths
 * 
 * Examples: "allocation/commitment", "data/tree", "config.json"
 */
export const HolsterPathSchema = z.string()
	.min(1)
	.regex(/^[a-zA-Z0-9_/.~-]+$/, 'Must contain only alphanumeric, underscore, slash, dot, tilde, or hyphen');

/**
 * Public Key - 64-character hexadecimal public key
 * 
 * Used for cross-user subscriptions and data access
 */
export const PubKeySchema = z.string()
	.length(64)
	.regex(/^[0-9a-fA-F]{64}$/, 'Must be 64 hexadecimal characters');

/**
 * Schema Type Name - References a registered Zod schema
 * 
 * Examples: "Commitment", "RootNode", "AllocationState"
 */
export const SchemaTypeNameSchema = z.string().min(1);

/**
 * Function Name - References a registered computation function
 * 
 * Examples: "computeMR", "allocate", "twoTierAllocation"
 */
export const FunctionNameSchema = z.string().min(1);

// ═══════════════════════════════════════════════════════════════════
// VARIABLE BINDING SCHEMAS
// ═══════════════════════════════════════════════════════════════════

/**
 * Variable Binding - Declares where a variable gets its value
 * 
 * RDL supports 5 binding types for maximum flexibility:
 * 
 * 1. **value** - Static literal (any JSON-serializable value)
 * 2. **subscription** - Reactive Holster subscription (updates automatically)
 * 3. **fetch** - One-time Holster fetch (non-reactive)
 * 4. **local** - Reference to local computation state
 * 5. **derived** - Output from a previous computation
 * 
 * Each binding type is optimized for its use case:
 * - Use 'value' for constants
 * - Use 'subscription' for reactive data (e.g., commitments, trees)
 * - Use 'fetch' for static config or initial state
 * - Use 'local' for intermediate computation state
 * - Use 'derived' to chain computations together
 */
export const VariableBindingSchema = z.discriminatedUnion('type', [
	/**
	 * VALUE BINDING - Static literal value
	 * 
	 * Use for: Constants, configuration, static parameters
	 * 
	 * Example:
	 * ```typescript
	 * { type: 'value', value: 100 }
	 * { type: 'value', value: { capacity: 50, priority: 'high' } }
	 * ```
	 */
	z.object({
		type: z.literal('value'),
		value: z.any().describe('Any JSON-serializable value (number, string, object, array, etc.)')
	}),
	
	/**
	 * SUBSCRIPTION BINDING - Reactive Holster subscription
	 * 
	 * Use for: Live data that changes over time
	 * Uses: Holster .on() for reactive updates
	 * 
	 * Example:
	 * ```typescript
	 * {
	 *   type: 'subscription',
	 *   holster_path: 'allocation/commitment',
	 *   schema_type: 'Commitment',
	 *   subscribe_to_user: 'abc123...' // Optional: subscribe to peer
	 * }
	 * ```
	 */
	z.object({
		type: z.literal('subscription'),
		holster_path: HolsterPathSchema.describe('Path in Holster to subscribe to'),
		schema_type: SchemaTypeNameSchema.describe('Name of registered Zod schema for validation'),
		subscribe_to_user: PubKeySchema.optional().describe('Optional: Subscribe to another user\'s data'),
		default_value: z.any().optional().describe('Fallback value if subscription is empty')
	}),
	
	/**
	 * FETCH BINDING - One-time Holster fetch
	 * 
	 * Use for: Initial state, configuration, static data
	 * Uses: Holster .get() for one-time retrieval
	 * 
	 * Example:
	 * ```typescript
	 * {
	 *   type: 'fetch',
	 *   holster_path: 'config/settings',
	 *   schema_type: 'Config',
	 *   wait_ms: 500
	 * }
	 * ```
	 */
	z.object({
		type: z.literal('fetch'),
		holster_path: HolsterPathSchema.describe('Path in Holster to fetch from'),
		schema_type: SchemaTypeNameSchema.describe('Name of registered Zod schema for validation'),
		fetch_from_user: PubKeySchema.optional().describe('Optional: Fetch from another user\'s data'),
		default_value: z.any().optional().describe('Fallback value if fetch returns null'),
		wait_ms: z.number().int().gte(0).default(100).describe('Milliseconds to wait for response before using default')
	}),
	
	/**
	 * LOCAL BINDING - Reference to local state
	 * 
	 * Use for: Intermediate computation state, temporary values
	 * 
	 * Example:
	 * ```typescript
	 * { type: 'local', state_path: 'myData.field.subfield' }
	 * ```
	 */
	z.object({
		type: z.literal('local'),
		state_path: HolsterPathSchema.describe('Dot-separated path in local state (e.g., "results.allocation")'),
		default_value: z.any().optional().describe('Fallback value if path doesn\'t exist')
	}),
	
	/**
	 * DERIVED BINDING - Output from previous computation
	 * 
	 * Use for: Chaining computations, building pipelines
	 * Creates explicit dependency graph
	 * 
	 * Example:
	 * ```typescript
	 * {
	 *   type: 'derived',
	 *   computation_id: 'compute_mr',
	 *   output_key: 'mutualRecognition'
	 * }
	 * ```
	 */
	z.object({
		type: z.literal('derived'),
		computation_id: IdentifierSchema.describe('ID of computation that produces this value'),
		output_key: IdentifierSchema.describe('Key of the output from that computation'),
		default_value: z.any().optional().describe('Fallback value if computation hasn\'t run')
	})
]);

/**
 * Output Binding - Declares where to store computation results
 * 
 * RDL supports 3 output types for different persistence needs:
 * 
 * 1. **holster** - Persist to Holster (network-replicated, permanent)
 * 2. **local** - Store in local state (transient, session-only)
 * 3. **memory** - Keep in memory only (for chaining computations)
 * 
 * Choose based on your persistence and visibility needs:
 * - Use 'holster' for results that need to be shared or persisted
 * - Use 'local' for intermediate state within a session
 * - Use 'memory' for ephemeral data used by downstream computations
 */
export const OutputBindingSchema = z.discriminatedUnion('type', [
	/**
	 * HOLSTER OUTPUT - Persist to network-replicated storage
	 * 
	 * Use for: Results that need to be:
	 * - Shared with other users
	 * - Persisted across sessions
	 * - Queryable by other programs
	 * 
	 * Example:
	 * ```typescript
	 * {
	 *   type: 'holster',
	 *   holster_path: 'allocation/result',
	 *   schema_type: 'AllocationState',
	 *   persist_debounce_ms: 200
	 * }
	 * ```
	 */
	z.object({
		type: z.literal('holster'),
		holster_path: HolsterPathSchema.describe('Path in Holster where result will be stored'),
		schema_type: SchemaTypeNameSchema.optional().describe('Optional: Zod schema for validation'),
		persist_debounce_ms: z.number().int().gte(0).optional().describe('Optional: Delay before persisting (for batching)')
	}),
	
	/**
	 * LOCAL OUTPUT - Store in session-local state
	 * 
	 * Use for: Temporary results within a session
	 * Data is lost when program stops
	 * 
	 * Example:
	 * ```typescript
	 * { type: 'local', state_path: 'results.intermediate' }
	 * ```
	 */
	z.object({
		type: z.literal('local'),
		state_path: HolsterPathSchema.describe('Dot-separated path in local state')
	}),
	
	/**
	 * MEMORY OUTPUT - Keep in memory for chaining
	 * 
	 * Use for: Ephemeral data passed to downstream computations
	 * Most efficient for computation pipelines
	 * 
	 * Example:
	 * ```typescript
	 * { type: 'memory' }
	 * ```
	 */
	z.object({
		type: z.literal('memory')
	})
]);

// ═══════════════════════════════════════════════════════════════════
// COMPUTATION DEFINITION SCHEMAS
// ═══════════════════════════════════════════════════════════════════

/**
 * Computation - A single declarative reactive computation step
 * 
 * A Computation is the fundamental unit of work in RDL. It declares:
 * 1. **What data it needs** (inputs)
 * 2. **How to process it** (compute_fn)
 * 3. **Where results go** (outputs)
 * 4. **When it runs** (depends_on, debounce_ms)
 * 
 * Computations are:
 * - **Declarative**: Describe WHAT, not HOW
 * - **Reactive**: Auto-recompute when inputs change
 * - **Composable**: Outputs become inputs for other computations
 * - **Verifiable**: Full provenance tracking
 * 
 * @example Simple computation
 * ```typescript
 * {
 *   id: 'compute_allocation',
 *   inputs: {
 *     capacity: { type: 'value', value: 100 },
 *     need: { type: 'subscription', holster_path: 'need', schema_type: 'Need' }
 *   },
 *   compute_fn: 'allocate',
 *   outputs: {
 *     result: { type: 'holster', holster_path: 'allocation/result' }
 *   }
 * }
 * ```
 * 
 * @example Chained computations
 * ```typescript
 * {
 *   id: 'compute_final',
 *   depends_on: ['compute_mr'],
 *   inputs: {
 *     mr: { type: 'derived', computation_id: 'compute_mr', output_key: 'value' }
 *   },
 *   compute_fn: 'processResult',
 *   outputs: {
 *     final: { type: 'holster', holster_path: 'results/final' }
 *   },
 *   debounce_ms: 200
 * }
 * ```
 */
export const ComputationSchema = z.object({
	/** Unique identifier for this computation within the program */
	id: IdentifierSchema.describe('Unique computation ID (used for references and dependencies)'),
	
	/** 
	 * Input declarations - where this computation gets its data
	 * 
	 * Key: Variable name (used in compute_fn)
	 * Value: Binding specification (where the data comes from)
	 */
	inputs: z.record(
		IdentifierSchema,
		VariableBindingSchema
	).describe('Map of input variable names to their bindings'),
	
	/** 
	 * Computation function - the actual logic to execute
	 * 
	 * Must reference a function registered via registerComputationFunction()
	 * Function receives inputs as object, returns outputs as object
	 */
	compute_fn: FunctionNameSchema.describe('Name of registered computation function'),
	
	/** 
	 * Local bindings - variables scoped to this computation's closure
	 * 
	 * Useful for:
	 * - Configuration specific to this computation
	 * - Utility data not needed by other computations
	 */
	local_bindings: z.record(
		IdentifierSchema,
		VariableBindingSchema
	).optional().describe('Optional local variables within computation scope'),
	
	/** 
	 * Output declarations - where results are stored
	 * 
	 * Key: Output name (from compute_fn return value)
	 * Value: Binding specification (where to persist it)
	 */
	outputs: z.record(
		IdentifierSchema,
		OutputBindingSchema
	).describe('Map of output keys to their persistence bindings'),
	
	// ───────────────────────────────────────────────────────────────────
	// EXECUTION CONTROL
	// ───────────────────────────────────────────────────────────────────
	
	/** 
	 * Debounce delay for re-execution
	 * 
	 * When input changes trigger re-execution, wait this many milliseconds
	 * before actually running. Useful for batching rapid changes.
	 */
	debounce_ms: z.number().int().gte(0).default(0)
		.describe('Milliseconds to debounce re-execution (default: 0 = immediate)'),
	
	/** 
	 * Whether this computation is enabled
	 * 
	 * Disabled computations are skipped during execution.
	 * Useful for A/B testing or feature flags.
	 */
	enabled: z.boolean().default(true)
		.describe('Whether computation is enabled (default: true)'),
	
	/** 
	 * Explicit dependencies on other computations
	 * 
	 * Runtime will execute these computations first.
	 * Note: Implicit dependencies from 'derived' bindings are handled automatically.
	 */
	depends_on: z.array(IdentifierSchema).optional()
		.describe('Optional list of computation IDs that must run before this one'),
	
	// ───────────────────────────────────────────────────────────────────
	// METADATA
	// ───────────────────────────────────────────────────────────────────
	
	/** Human-readable description of what this computation does */
	description: z.string().optional()
		.describe('Optional description for documentation'),
	
	/** Version identifier for this computation */
	version: z.string().optional()
		.describe('Optional version string (e.g., "1.0.0")')
});

// ═══════════════════════════════════════════════════════════════════
// REACTIVE COMPUTATION GRAPH SCHEMA (TOP-LEVEL RDL PROGRAM)
// ═══════════════════════════════════════════════════════════════════

/**
 * ReactiveComputationGraph - Complete RDL Program Specification
 * 
 * This is the TOP-LEVEL schema for an RDL program. It defines a complete,
 * self-contained, declarative dataflow application.
 * 
 * ## What is an RDL Program?
 * 
 * An RDL program is like a reactive spreadsheet:
 * - **Variables** are cells that hold data
 * - **Computations** are formulas that process data
 * - **Bindings** connect cells to data sources and outputs
 * - **Dependencies** ensure correct execution order
 * 
 * When deployed, the RDL runtime:
 * 1. Sets up all subscriptions (for reactive variables)
 * 2. Resolves all dependencies (topological sort)
 * 3. Executes computations in the correct order
 * 4. Persists outputs according to bindings
 * 5. Tracks full provenance for verification
 * 6. Auto-recomputes when inputs change (if reactive mode enabled)
 * 
 * ## Key Properties
 * 
 * - **Declarative**: Describe WHAT you want, not HOW to compute it
 * - **Reactive**: Automatically responds to data changes
 * - **Composable**: Programs can reference each other's outputs
 * - **Verifiable**: Every computation has cryptographic provenance
 * - **Network-native**: Designed for P2P coordination
 * 
 * ## Example: Simple Allocation Program
 * 
 * ```typescript
 * {
 *   id: 'allocation_v2',
 *   version: '2.0.0',
 *   description: 'Two-tier slot-native allocation',
 *   
 *   // Declare data sources
 *   variables: {
 *     myCommitment: {
 *       type: 'subscription',
 *       holster_path: 'allocation/commitment',
 *       schema_type: 'Commitment'
 *     },
 *     networkCommitments: {
 *       type: 'subscription',
 *       holster_path: 'allocation/network',
 *       schema_type: 'Object'
 *     }
 *   },
 *   
 *   // Declare computations
 *   computations: [
 *     {
 *       id: 'compute_allocation',
 *       inputs: {
 *         my: { type: 'local', state_path: 'myCommitment' },
 *         network: { type: 'local', state_path: 'networkCommitments' }
 *       },
 *       compute_fn: 'twoTierAllocation',
 *       outputs: {
 *         state: {
 *           type: 'holster',
 *           holster_path: 'allocation/state',
 *           persist_debounce_ms: 200
 *         }
 *       },
 *       debounce_ms: 100
 *     }
 *   ]
 * }
 * ```
 * 
 * ## Deployment
 * 
 * ```typescript
 * // Register computation functions
 * registerComputationFunction('twoTierAllocation', (inputs) => {
 *   return { state: computeAllocation(inputs.my, inputs.network) };
 * });
 * 
 * // Deploy program
 * const manager = await deployReactiveProgram(myProgram);
 * 
 * // Program now runs automatically!
 * ```
 * 
 * @see ComputationSchema for individual computation definitions
 * @see VariableBindingSchema for data source options
 * @see OutputBindingSchema for persistence options
 */
export const ReactiveComputationGraphSchema = z.object({
	// ───────────────────────────────────────────────────────────────────
	// IDENTITY
	// ───────────────────────────────────────────────────────────────────
	
	/** 
	 * Unique identifier for this program
	 * 
	 * Used for:
	 * - Program registration in the registry
	 * - References from other programs
	 * - Logging and diagnostics
	 */
	id: IdentifierSchema.describe('Unique program identifier'),
	
	// ───────────────────────────────────────────────────────────────────
	// DATAFLOW SPECIFICATION
	// ───────────────────────────────────────────────────────────────────
	
	/** 
	 * Global variable declarations
	 * 
	 * Variables are shared across all computations in this program.
	 * They define WHERE data comes from (subscriptions, values, etc.)
	 * 
	 * Key: Variable name (referenced in computation inputs)
	 * Value: Binding specification (where to get the data)
	 * 
	 * Think of these as "global cells" in a spreadsheet.
	 */
	variables: z.record(
		IdentifierSchema,
		VariableBindingSchema
	).describe('Global variable declarations (data sources)'),
	
	/** 
	 * Computation pipeline
	 * 
	 * The ordered list of computations to execute.
	 * Runtime will:
	 * - Resolve dependencies (explicit + implicit)
	 * - Execute in topological order
	 * - Handle reactivity (if enabled)
	 * 
	 * Think of these as "formulas" in a spreadsheet.
	 */
	computations: z.array(ComputationSchema)
		.min(1)
		.describe('List of computations in this program (must have at least one)'),
	
	// ───────────────────────────────────────────────────────────────────
	// METADATA & CONFIGURATION
	// ───────────────────────────────────────────────────────────────────
	
	/** 
	 * Program version
	 * 
	 * Semantic versioning recommended (e.g., "1.0.0")
	 * Used for:
	 * - Compatibility checking
	 * - Migration handling
	 * - Documentation
	 */
	version: z.string().optional()
		.describe('Program version (e.g., "1.0.0")'),
	
	/** 
	 * Human-readable description
	 * 
	 * Explains what this program does at a high level.
	 * Useful for documentation and program discovery.
	 */
	description: z.string().optional()
		.describe('Description of program purpose and behavior'),
	
	/** 
	 * Program hash for namespacing
	 * 
	 * If provided, all Holster paths in this program are automatically
	 * prefixed with this hash, ensuring namespace isolation.
	 * 
	 * Format: ~{pubkey}/{program_hash}/{holster_path}
	 * 
	 * If not provided, computed automatically from program structure.
	 * 
	 * Benefits:
	 * - Multiple program versions can coexist
	 * - No path conflicts between programs
	 * - Clean program isolation
	 */
	program_hash: z.string().optional()
		.describe('Optional hash for namespacing Holster paths (auto-computed if not provided)')
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

// Primitive types
export type Identifier = z.infer<typeof IdentifierSchema>;
export type HolsterPath = z.infer<typeof HolsterPathSchema>;
export type PubKey = z.infer<typeof PubKeySchema>;
export type SchemaTypeName = z.infer<typeof SchemaTypeNameSchema>;
export type FunctionName = z.infer<typeof FunctionNameSchema>;

// Binding types
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

