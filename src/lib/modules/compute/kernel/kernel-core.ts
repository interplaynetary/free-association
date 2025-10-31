/**
 * P2P OS Kernel - Core (Language-Agnostic)
 * 
 * Defines the language-agnostic infrastructure for the P2P OS kernel:
 * - Program management (generic, works with any program language)
 * - Subscription tracking (bidirectional, cross-user)
 * - Causality tracking (ITC stamps)
 * - Node storage (tree structures)
 * - Replication (encrypted peer sync)
 * 
 * This module is LANGUAGE-AGNOSTIC and should have NO dependencies on:
 * - RDL-specific schemas (ReactiveComputationGraph, etc.)
 * - Domain-specific schemas (Commitment, AllocationState, etc.)
 * 
 * Language-specific extensions go in:
 * - kernel-rdl.ts (RDL-specific)
 * - kernel-sql.ts (SQL-specific) - future
 * - kernel-wasm.ts (WASM-specific) - future
 * 
 * Domain-specific extensions go in:
 * - kernel-domain.ts (allocation, trees, etc.)
 */

import * as z from 'zod';

// Import only language-agnostic primitives
import {
	ITCStampSchema,
	RootNodeSchema,
	NonRootNodeSchema,
	NodeDataStorageSchema
} from '../v5/schemas';

// ═══════════════════════════════════════════════════════════════════
// GENERIC PROGRAM SCHEMAS (Language-Agnostic)
// ═══════════════════════════════════════════════════════════════════

/**
 * Program Metadata (language-agnostic)
 */
export const ProgramMetadataSchema = z.object({
	version: z.string().optional(),
	description: z.string().optional(),
	created_at: z.number().int().positive(),
	updated_at: z.number().int().positive()
});

/**
 * Program Status (language-agnostic)
 */
export const ProgramStatusSchema = z.object({
	active: z.boolean(),
	enabled: z.boolean()
});

/**
 * Generic Program Definition
 * 
 * This is language-agnostic. The program_data field holds the
 * language-specific definition (RDL, SQL, WASM, etc.)
 * 
 * Examples:
 * - RDL: { type: 'RDL', version: '1.0', data: <ReactiveComputationGraph> }
 * - SQL: { type: 'SQL', version: '1.0', data: <SQLQueryDefinition> }
 * - WASM: { type: 'WASM', version: '1.0', data: <WASMModuleDefinition> }
 */
export const GenericProgramDefinitionSchema = z.object({
	/** Language/runtime type: "RDL", "SQL", "WASM", etc. */
	program_type: z.string(),
	
	/** Language version: "1.0.0", etc. */
	language_version: z.string(),
	
	/** Language-specific program data (validated by language runtime) */
	program_data: z.any(),
	
	/** Optional program hash (computed from program_data) */
	program_hash: z.string().optional()
});

/**
 * Program Registry Entry (language-agnostic)
 * Path: ~{pubKey}/programs/registry/{programHash}/
 */
export const GenericProgramRegistryEntrySchema = z.object({
	definition: GenericProgramDefinitionSchema,
	metadata: ProgramMetadataSchema,
	status: ProgramStatusSchema
});

/**
 * Program Reference (for active/inactive)
 * Path: ~{pubKey}/programs/active/{programHash}
 * Path: ~{pubKey}/programs/inactive/{programHash}
 */
export const ProgramReferenceSchema = z.object({
	registry_path: z.string() // Path to registry entry
});

/**
 * Subscribed Program Entry (from peer)
 * Path: ~{pubKey}/programs/subscribed/{peerPubKey}/{programHash}/
 */
export const SubscribedProgramSchema = z.object({
	definition: GenericProgramDefinitionSchema,
	last_synced: z.number().int().positive()
});

/**
 * Programs Namespace (language-agnostic)
 * Path: ~{pubKey}/programs/
 */
export const ProgramsNamespaceSchema = z.object({
	registry: z.record(z.string(), GenericProgramRegistryEntrySchema).optional(),
	active: z.record(z.string(), ProgramReferenceSchema).optional(),
	inactive: z.record(z.string(), ProgramReferenceSchema).optional(),
	subscribed: z.record(z.string(), z.record(z.string(), SubscribedProgramSchema)).optional()
});

// ═══════════════════════════════════════════════════════════════════
// GENERIC EXECUTION PROVENANCE (Language-Agnostic)
// ═══════════════════════════════════════════════════════════════════

/**
 * Generic Input Provenance
 * Tracks where input data came from
 */
export const GenericInputProvenanceSchema = z.object({
	source: z.string(), // "value", "subscription", "fetch", "local", "derived", etc.
	path: z.string().optional(), // Holster path if applicable
	contentHash: z.string(), // Hash of input data
	provenance: z.string().optional() // Parent provenance ID if derived
});

/**
 * Generic Output Provenance
 * Tracks where output data went
 */
export const GenericOutputProvenanceSchema = z.object({
	path: z.string(), // Where output was stored
	contentHash: z.string() // Hash of output data
});

/**
 * Generic Execution Provenance
 * 
 * Language-agnostic provenance for any program execution.
 * Works for RDL computations, SQL queries, WASM function calls, etc.
 */
export const GenericExecutionProvenanceSchema = z.object({
	id: z.string(),
	itcStamp: ITCStampSchema,
	executedBy: z.string(),
	timestamp: z.number().int().positive(),
	
	// Program info
	programHash: z.string(),
	programType: z.string(), // "RDL", "SQL", "WASM"
	
	// Execution unit info (language-specific names)
	executionUnitId: z.string(), // "computation-1", "query-5", "function-main"
	executionUnitHash: z.string(),
	
	// I/O tracking
	inputs: z.record(z.string(), GenericInputProvenanceSchema),
	outputs: z.record(z.string(), GenericOutputProvenanceSchema),
	
	// Verification
	deterministicHash: z.string(),
	parents: z.array(z.string()).optional()
});

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION SCHEMAS (Language-Agnostic)
// ═══════════════════════════════════════════════════════════════════

/**
 * Local Subscription
 * Path: ~{pubKey}/subscriptions/outbound/local/{holsterPath}/
 */
export const LocalSubscriptionSchema = z.object({
	schema_type: z.string(),
	subscribers: z.array(z.string()), // Array of callback IDs
	last_value: z.any().optional()
});

/**
 * Peer Subscription
 * Path: ~{pubKey}/subscriptions/outbound/peers/{peerPubKey}/{holsterPath}/
 */
export const PeerSubscriptionSchema = z.object({
	schema_type: z.string(),
	subscribers: z.array(z.string()), // Array of callback IDs
	last_value: z.any().optional(),
	last_synced: z.number().int().positive()
});

/**
 * Outbound Subscriptions
 * Path: ~{pubKey}/subscriptions/outbound/
 */
export const OutboundSubscriptionsSchema = z.object({
	local: z.record(z.string(), LocalSubscriptionSchema).optional(),
	peers: z.record(z.string(), z.record(z.string(), PeerSubscriptionSchema)).optional()
});

/**
 * Inbound Subscription (what paths a peer is watching)
 * Path: ~{pubKey}/subscriptions/inbound/{peerPubKey}/
 */
export const InboundSubscriptionSchema = z.array(z.string()); // Array of holster paths

/**
 * Subscriptions Namespace
 * Path: ~{pubKey}/subscriptions/
 */
export const SubscriptionsNamespaceSchema = z.object({
	outbound: OutboundSubscriptionsSchema.optional(),
	inbound: z.record(z.string(), InboundSubscriptionSchema).optional()
});

// ═══════════════════════════════════════════════════════════════════
// NODE SCHEMAS (Language-Agnostic)
// ═══════════════════════════════════════════════════════════════════

/**
 * Node Entry (tree node with optional storage)
 * Path: ~{pubKey}/nodes/{nodeId}/
 */
export const NodeEntrySchema = z.object({
	node: z.union([RootNodeSchema, NonRootNodeSchema]),
	storage: NodeDataStorageSchema.optional()
});

/**
 * Nodes Namespace
 * Path: ~{pubKey}/nodes/
 */
export const NodesNamespaceSchema = z.record(z.string(), NodeEntrySchema);

// ═══════════════════════════════════════════════════════════════════
// CAUSALITY SCHEMAS (Language-Agnostic)
// ═══════════════════════════════════════════════════════════════════

/**
 * My ITC Stamp
 * Path: ~{pubKey}/causality/itc_stamp/
 */
export const MyITCStampSchema = ITCStampSchema;

/**
 * Peer ITC Stamp Entry
 * Path: ~{pubKey}/causality/peer_stamps/{peerPubKey}/
 */
export const PeerITCStampEntrySchema = z.object({
	itc_stamp: ITCStampSchema,
	last_seen: z.number().int().positive()
});

/**
 * Causality Namespace
 * Path: ~{pubKey}/causality/
 */
export const CausalityNamespaceSchema = z.object({
	itc_stamp: MyITCStampSchema.optional(),
	peer_stamps: z.record(z.string(), PeerITCStampEntrySchema).optional()
});

// ═══════════════════════════════════════════════════════════════════
// REPLICATION SCHEMAS (Language-Agnostic)
// ═══════════════════════════════════════════════════════════════════

/**
 * Encrypted Peer Replication Data
 * Path: ~{pubKey}/replication/{peerPubKey}/
 */
export const EncryptedReplicationDataSchema = z.object({
	encrypted_data: z.any() // Peer-encrypted replication data
});

/**
 * Replication Namespace
 * Path: ~{pubKey}/replication/
 */
export const ReplicationNamespaceSchema = z.record(z.string(), EncryptedReplicationDataSchema);

// ═══════════════════════════════════════════════════════════════════
// TYPE EXPORTS
// ═══════════════════════════════════════════════════════════════════

// Program types (generic)
export type ProgramMetadata = z.infer<typeof ProgramMetadataSchema>;
export type ProgramStatus = z.infer<typeof ProgramStatusSchema>;
export type GenericProgramDefinition = z.infer<typeof GenericProgramDefinitionSchema>;
export type GenericProgramRegistryEntry = z.infer<typeof GenericProgramRegistryEntrySchema>;
export type ProgramReference = z.infer<typeof ProgramReferenceSchema>;
export type SubscribedProgram = z.infer<typeof SubscribedProgramSchema>;
export type ProgramsNamespace = z.infer<typeof ProgramsNamespaceSchema>;

// Provenance types (generic)
export type GenericInputProvenance = z.infer<typeof GenericInputProvenanceSchema>;
export type GenericOutputProvenance = z.infer<typeof GenericOutputProvenanceSchema>;
export type GenericExecutionProvenance = z.infer<typeof GenericExecutionProvenanceSchema>;

// Subscription types
export type LocalSubscription = z.infer<typeof LocalSubscriptionSchema>;
export type PeerSubscription = z.infer<typeof PeerSubscriptionSchema>;
export type OutboundSubscriptions = z.infer<typeof OutboundSubscriptionsSchema>;
export type InboundSubscription = z.infer<typeof InboundSubscriptionSchema>;
export type SubscriptionsNamespace = z.infer<typeof SubscriptionsNamespaceSchema>;

// Node types
export type NodeEntry = z.infer<typeof NodeEntrySchema>;
export type NodesNamespace = z.infer<typeof NodesNamespaceSchema>;

// Causality types
export type MyITCStamp = z.infer<typeof MyITCStampSchema>;
export type PeerITCStampEntry = z.infer<typeof PeerITCStampEntrySchema>;
export type CausalityNamespace = z.infer<typeof CausalityNamespaceSchema>;

// Replication types
export type EncryptedReplicationData = z.infer<typeof EncryptedReplicationDataSchema>;
export type ReplicationNamespace = z.infer<typeof ReplicationNamespaceSchema>;

// ═══════════════════════════════════════════════════════════════════
// VALIDATION HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Validate generic program registry entry
 */
export function parseGenericProgramRegistryEntry(data: unknown): GenericProgramRegistryEntry | null {
	const result = GenericProgramRegistryEntrySchema.safeParse(data);
	if (!result.success) {
		console.warn('[KERNEL-CORE] Invalid program registry entry:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Validate subscriptions namespace
 */
export function parseSubscriptionsNamespace(data: unknown): SubscriptionsNamespace | null {
	const result = SubscriptionsNamespaceSchema.safeParse(data);
	if (!result.success) {
		console.warn('[KERNEL-CORE] Invalid subscriptions namespace:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Validate node entry
 */
export function parseNodeEntry(data: unknown): NodeEntry | null {
	const result = NodeEntrySchema.safeParse(data);
	if (!result.success) {
		console.warn('[KERNEL-CORE] Invalid node entry:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Validate causality namespace
 */
export function parseCausalityNamespace(data: unknown): CausalityNamespace | null {
	const result = CausalityNamespaceSchema.safeParse(data);
	if (!result.success) {
		console.warn('[KERNEL-CORE] Invalid causality namespace:', result.error);
		return null;
	}
	return result.data;
}

// ═══════════════════════════════════════════════════════════════════
// PATH HELPERS (Language-Agnostic)
// ═══════════════════════════════════════════════════════════════════

/**
 * Build Holster paths for language-agnostic kernel structure
 */
export const CorePaths = {
	// Program paths
	programRegistry: (pubKey: string, programHash: string) => 
		`~${pubKey}/programs/registry/${programHash}`,
	programActive: (pubKey: string, programHash: string) => 
		`~${pubKey}/programs/active/${programHash}`,
	programInactive: (pubKey: string, programHash: string) => 
		`~${pubKey}/programs/inactive/${programHash}`,
	programSubscribed: (pubKey: string, peerPubKey: string, programHash: string) => 
		`~${pubKey}/programs/subscribed/${peerPubKey}/${programHash}`,
	
	// Subscription paths
	subscriptionLocal: (pubKey: string, holsterPath: string) => 
		`~${pubKey}/subscriptions/outbound/local/${holsterPath}`,
	subscriptionPeer: (pubKey: string, peerPubKey: string, holsterPath: string) => 
		`~${pubKey}/subscriptions/outbound/peers/${peerPubKey}/${holsterPath}`,
	subscriptionInbound: (pubKey: string, peerPubKey: string) => 
		`~${pubKey}/subscriptions/inbound/${peerPubKey}`,
	
	// Node paths
	node: (pubKey: string, nodeId: string) => 
		`~${pubKey}/nodes/${nodeId}`,
	nodeStorage: (pubKey: string, nodeId: string) => 
		`~${pubKey}/nodes/${nodeId}/storage`,
	
	// Causality paths
	myITCStamp: (pubKey: string) => 
		`~${pubKey}/causality/itc_stamp`,
	peerITCStamp: (pubKey: string, peerPubKey: string) => 
		`~${pubKey}/causality/peer_stamps/${peerPubKey}`,
	
	// Replication paths
	replication: (pubKey: string, peerPubKey: string) => 
		`~${pubKey}/replication/${peerPubKey}`
};

