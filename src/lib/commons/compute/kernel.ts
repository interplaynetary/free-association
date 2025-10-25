/**
 * Holster User Space Schemas
 * 
 * Defines the canonical structure for organizing user data in Holster,
 * including reactive computation programs, execution state, provenance,
 * subscriptions, and causality tracking.
 * 
 * Based on: gitbook/user-space-structure.md
 */

import * as z from 'zod';

// Import core schemas from commons
import {
	ITCStampSchema,
	CommitmentSchema,
	TwoTierAllocationStateSchema,
	RootNodeSchema,
	NonRootNodeSchema,
	NodeDataStorageSchema
} from '../v2/schemas';

// Import compute schemas
import {
	ReactiveComputationGraphSchema,
	ComputationProvenanceSchema
} from './schema';

// ═══════════════════════════════════════════════════════════════════
// PROGRAM SCHEMAS
// ═══════════════════════════════════════════════════════════════════

/**
 * Program Metadata
 */
export const ProgramMetadataSchema = z.object({
	version: z.string().optional(),
	description: z.string().optional(),
	created_at: z.number().int().positive(),
	updated_at: z.number().int().positive()
});

/**
 * Program Status
 */
export const ProgramStatusSchema = z.object({
	active: z.boolean(),
	enabled: z.boolean()
});

/**
 * Program Registry Entry
 * Path: ~{pubKey}/programs/registry/{programHash}/
 */
export const ProgramRegistryEntrySchema = z.object({
	definition: ReactiveComputationGraphSchema,
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
 * Subscribed Program Entry
 * Path: ~{pubKey}/programs/subscribed/{peerPubKey}/{programHash}/
 */
export const SubscribedProgramSchema = z.object({
	definition: ReactiveComputationGraphSchema,
	last_synced: z.number().int().positive()
});

/**
 * Programs Namespace
 * Path: ~{pubKey}/programs/
 */
export const ProgramsNamespaceSchema = z.object({
	registry: z.record(z.string(), ProgramRegistryEntrySchema).optional(),
	active: z.record(z.string(), ProgramReferenceSchema).optional(),
	inactive: z.record(z.string(), ProgramReferenceSchema).optional(),
	subscribed: z.record(z.string(), z.record(z.string(), SubscribedProgramSchema)).optional()
});

// ═══════════════════════════════════════════════════════════════════
// COMPUTE SCHEMAS
// ═══════════════════════════════════════════════════════════════════

/**
 * Variable State
 * Path: ~{pubKey}/compute/{programHash}/state/variables/{variableName}
 */
export const VariableStateSchema = z.any(); // Value can be any type

/**
 * Computation Result
 * Path: ~{pubKey}/compute/{programHash}/state/computations/{computationId}/
 */
export const ComputationResultSchema = z.object({
	result: z.any(),
	last_executed: z.number().int().positive(),
	execution_count: z.number().int().nonnegative()
});

/**
 * Computation State Metadata
 * Path: ~{pubKey}/compute/{programHash}/state/metadata/
 */
export const ComputationStateMetadataSchema = z.object({
	program_started_at: z.number().int().positive(),
	last_computation: z.number().int().positive(),
	total_executions: z.number().int().nonnegative()
});

/**
 * Computation State
 * Path: ~{pubKey}/compute/{programHash}/state/
 */
export const ComputationStateSchema = z.object({
	variables: z.record(z.string(), VariableStateSchema).optional(),
	computations: z.record(z.string(), ComputationResultSchema).optional(),
	metadata: ComputationStateMetadataSchema.optional()
});

/**
 * Output Value
 * Path: ~{pubKey}/compute/{programHash}/outputs/{outputKey}/
 */
export const OutputValueSchema = z.object({
	value: z.any(),
	holster_path: z.string(),
	updated_at: z.number().int().positive()
});

/**
 * Provenance Entry
 * Path: ~{pubKey}/compute/{programHash}/provenance/{provenanceId}/
 */
export const ProvenanceEntrySchema = z.object({
	record: ComputationProvenanceSchema,
	signature: z.string() // Cryptographic signature
});

/**
 * Compute Namespace (per program)
 * Path: ~{pubKey}/compute/{programHash}/
 */
export const ComputeNamespaceSchema = z.object({
	state: ComputationStateSchema.optional(),
	outputs: z.record(z.string(), OutputValueSchema).optional(),
	provenance: z.record(z.string(), ProvenanceEntrySchema).optional()
});

/**
 * All Compute Namespaces
 * Path: ~{pubKey}/compute/
 */
export const AllComputeNamespacesSchema = z.record(z.string(), ComputeNamespaceSchema);

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION SCHEMAS
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
// NODE SCHEMAS
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
// CAUSALITY SCHEMAS
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
// ALLOCATION SCHEMAS (Commons-specific)
// ═══════════════════════════════════════════════════════════════════

/**
 * Network Peer Allocation
 * Path: ~{pubKey}/allocation/network/{peerPubKey}/
 */
export const NetworkPeerAllocationSchema = z.object({
	commitment: CommitmentSchema,
	allocation_state: TwoTierAllocationStateSchema
});

/**
 * Allocation Namespace
 * Path: ~{pubKey}/allocation/
 */
export const AllocationNamespaceSchema = z.object({
	commitment: CommitmentSchema.optional(),
	allocation_state: TwoTierAllocationStateSchema.optional(),
	network: z.record(z.string(), NetworkPeerAllocationSchema).optional()
});

// ═══════════════════════════════════════════════════════════════════
// TREES SCHEMAS (Priority/contribution trees)
// ═══════════════════════════════════════════════════════════════════

/**
 * Trees Namespace
 * Path: ~{pubKey}/trees/
 */
export const TreesNamespaceSchema = z.object({
	my_tree: RootNodeSchema.optional(),
	network_trees: z.record(z.string(), RootNodeSchema).optional()
});

// ═══════════════════════════════════════════════════════════════════
// REPLICATION SCHEMAS
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
// USER SPACE ROOT SCHEMA
// ═══════════════════════════════════════════════════════════════════

/**
 * Complete User Space
 * Path: ~{pubKey}/
 */
export const UserSpaceSchema = z.object({
	programs: ProgramsNamespaceSchema.optional(),
	compute: AllComputeNamespacesSchema.optional(),
	subscriptions: SubscriptionsNamespaceSchema.optional(),
	nodes: NodesNamespaceSchema.optional(),
	causality: CausalityNamespaceSchema.optional(),
	allocation: AllocationNamespaceSchema.optional(),
	trees: TreesNamespaceSchema.optional(),
	replication: ReplicationNamespaceSchema.optional()
});

// ═══════════════════════════════════════════════════════════════════
// TYPE EXPORTS
// ═══════════════════════════════════════════════════════════════════

// Program types
export type ProgramMetadata = z.infer<typeof ProgramMetadataSchema>;
export type ProgramStatus = z.infer<typeof ProgramStatusSchema>;
export type ProgramRegistryEntry = z.infer<typeof ProgramRegistryEntrySchema>;
export type ProgramReference = z.infer<typeof ProgramReferenceSchema>;
export type SubscribedProgram = z.infer<typeof SubscribedProgramSchema>;
export type ProgramsNamespace = z.infer<typeof ProgramsNamespaceSchema>;

// Compute types
export type VariableState = z.infer<typeof VariableStateSchema>;
export type ComputationResult = z.infer<typeof ComputationResultSchema>;
export type ComputationStateMetadata = z.infer<typeof ComputationStateMetadataSchema>;
export type ComputationState = z.infer<typeof ComputationStateSchema>;
export type OutputValue = z.infer<typeof OutputValueSchema>;
export type ProvenanceEntry = z.infer<typeof ProvenanceEntrySchema>;
export type ComputeNamespace = z.infer<typeof ComputeNamespaceSchema>;
export type AllComputeNamespaces = z.infer<typeof AllComputeNamespacesSchema>;

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

// Allocation types
export type NetworkPeerAllocation = z.infer<typeof NetworkPeerAllocationSchema>;
export type AllocationNamespace = z.infer<typeof AllocationNamespaceSchema>;

// Trees types
export type TreesNamespace = z.infer<typeof TreesNamespaceSchema>;

// Replication types
export type EncryptedReplicationData = z.infer<typeof EncryptedReplicationDataSchema>;
export type ReplicationNamespace = z.infer<typeof ReplicationNamespaceSchema>;

// Root type
export type UserSpace = z.infer<typeof UserSpaceSchema>;

// ═══════════════════════════════════════════════════════════════════
// VALIDATION HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Validate complete user space
 */
export function parseUserSpace(data: unknown): UserSpace | null {
	const result = UserSpaceSchema.safeParse(data);
	if (!result.success) {
		console.warn('[USER-SPACE] Invalid user space:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Validate program registry entry
 */
export function parseProgramRegistryEntry(data: unknown): ProgramRegistryEntry | null {
	const result = ProgramRegistryEntrySchema.safeParse(data);
	if (!result.success) {
		console.warn('[USER-SPACE] Invalid program registry entry:', result.error);
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
		console.warn('[USER-SPACE] Invalid compute namespace:', result.error);
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
		console.warn('[USER-SPACE] Invalid provenance entry:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Validate subscription namespace
 */
export function parseSubscriptionsNamespace(data: unknown): SubscriptionsNamespace | null {
	const result = SubscriptionsNamespaceSchema.safeParse(data);
	if (!result.success) {
		console.warn('[USER-SPACE] Invalid subscriptions namespace:', result.error);
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
		console.warn('[USER-SPACE] Invalid node entry:', result.error);
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
		console.warn('[USER-SPACE] Invalid causality namespace:', result.error);
		return null;
	}
	return result.data;
}

// ═══════════════════════════════════════════════════════════════════
// PATH HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Build Holster paths for user space structure
 */
export const UserSpacePaths = {
	// Program paths
	programRegistry: (pubKey: string, programHash: string) => 
		`~${pubKey}/programs/registry/${programHash}`,
	programActive: (pubKey: string, programHash: string) => 
		`~${pubKey}/programs/active/${programHash}`,
	programInactive: (pubKey: string, programHash: string) => 
		`~${pubKey}/programs/inactive/${programHash}`,
	programSubscribed: (pubKey: string, peerPubKey: string, programHash: string) => 
		`~${pubKey}/programs/subscribed/${peerPubKey}/${programHash}`,
	
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
		`~${pubKey}/compute/${programHash}/provenance/${provenanceId}`,
	
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
	
	// Allocation paths
	myCommitment: (pubKey: string) => 
		`~${pubKey}/allocation/commitment`,
	myAllocationState: (pubKey: string) => 
		`~${pubKey}/allocation/allocation_state`,
	peerAllocation: (pubKey: string, peerPubKey: string) => 
		`~${pubKey}/allocation/network/${peerPubKey}`,
	
	// Tree paths
	myTree: (pubKey: string) => 
		`~${pubKey}/trees/my_tree`,
	peerTree: (pubKey: string, peerPubKey: string) => 
		`~${pubKey}/trees/network_trees/${peerPubKey}`,
	
	// Replication paths
	replication: (pubKey: string, peerPubKey: string) => 
		`~${pubKey}/replication/${peerPubKey}`
};

