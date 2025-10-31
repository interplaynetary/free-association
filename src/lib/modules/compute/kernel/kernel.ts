/**
 * Holster User Space Schemas - Unified Export
 * 
 * This file provides backwards-compatible exports from the split kernel modules:
 * - kernel-core.ts (language-agnostic infrastructure)
 * - kernel-rdl.ts (RDL-specific extensions)
 * - kernel-domain.ts (domain-specific: allocation, trees)
 * 
 * ARCHITECTURE:
 * 
 * The kernel has been split into three layers:
 * 
 * 1. **kernel-core.ts** - Language-Agnostic
 *    - Program registry (works with any language)
 *    - Subscriptions (cross-user, bidirectional)
 *    - Causality (ITC stamps)
 *    - Nodes (tree structures)
 *    - Replication (encrypted peer sync)
 * 
 * 2. **kernel-rdl.ts** - RDL-Specific
 *    - RDL program storage
 *    - Computation state (variables, results)
 *    - RDL provenance
 * 
 * 3. **kernel-domain.ts** - Domain-Specific (Economic Coordination)
 *    - Allocation namespace (commitments, states)
 *    - Trees namespace (priority trees)
 * 
 * This unified export maintains backwards compatibility while allowing
 * future language extensions (kernel-sql.ts, kernel-wasm.ts, etc.)
 * 
 * Based on: gitbook/user-space-structure.md
 * See also: compute/docs/LANGUAGE-AGNOSTIC-BOUNDARIES.md
 */

import * as z from 'zod';

// ═══════════════════════════════════════════════════════════════════
// RE-EXPORT FROM KERNEL-CORE (Language-Agnostic)
// ═══════════════════════════════════════════════════════════════════

export {
	// Schemas
	ProgramMetadataSchema,
	ProgramStatusSchema,
	GenericProgramDefinitionSchema,
	GenericProgramRegistryEntrySchema,
	ProgramReferenceSchema,
	SubscribedProgramSchema,
	ProgramsNamespaceSchema,
	GenericInputProvenanceSchema,
	GenericOutputProvenanceSchema,
	GenericExecutionProvenanceSchema,
	LocalSubscriptionSchema,
	PeerSubscriptionSchema,
	OutboundSubscriptionsSchema,
	InboundSubscriptionSchema,
	SubscriptionsNamespaceSchema,
	NodeEntrySchema,
	NodesNamespaceSchema,
	MyITCStampSchema,
	PeerITCStampEntrySchema,
	CausalityNamespaceSchema,
	EncryptedReplicationDataSchema,
	ReplicationNamespaceSchema,
	
	// Types
	type ProgramMetadata,
	type ProgramStatus,
	type GenericProgramDefinition,
	type GenericProgramRegistryEntry,
	type ProgramReference,
	type SubscribedProgram,
	type ProgramsNamespace,
	type GenericInputProvenance,
	type GenericOutputProvenance,
	type GenericExecutionProvenance,
	type LocalSubscription,
	type PeerSubscription,
	type OutboundSubscriptions,
	type InboundSubscription,
	type SubscriptionsNamespace,
	type NodeEntry,
	type NodesNamespace,
	type MyITCStamp,
	type PeerITCStampEntry,
	type CausalityNamespace,
	type EncryptedReplicationData,
	type ReplicationNamespace,
	
	// Helpers
	parseGenericProgramRegistryEntry,
	parseSubscriptionsNamespace,
	parseNodeEntry,
	parseCausalityNamespace,
	
	// Paths
	CorePaths
} from './kernel-core';

// ═══════════════════════════════════════════════════════════════════
// RE-EXPORT FROM KERNEL-RDL (RDL-Specific)
// ═══════════════════════════════════════════════════════════════════

export {
	// Schemas
	RDLProgramDefinitionSchema,
	RDLProgramRegistryEntrySchema,
	VariableStateSchema,
	ComputationResultSchema,
	ComputationStateMetadataSchema,
	ComputationStateSchema,
	OutputValueSchema,
	ProvenanceEntrySchema,
	ComputeNamespaceSchema,
	AllComputeNamespacesSchema,
	
	// Backwards compatibility
	ProgramRegistryEntrySchema, // Alias for RDLProgramRegistryEntrySchema
	
	// Types
	type RDLProgramDefinition,
	type RDLProgramRegistryEntry,
	type VariableState,
	type ComputationResult,
	type ComputationStateMetadata,
	type ComputationState,
	type OutputValue,
	type ProvenanceEntry,
	type ComputeNamespace,
	type AllComputeNamespaces,
	
	// Backwards compatibility
	type ProgramRegistryEntry, // Alias for RDLProgramRegistryEntry
	
	// Helpers
	wrapRDLProgram,
	unwrapRDLProgram,
	isRDLProgram,
	parseRDLProgramRegistryEntry,
	parseComputeNamespace,
	parseProvenanceEntry,
	
	// Backwards compatibility
	parseProgramRegistryEntry, // Alias for parseRDLProgramRegistryEntry
	
	// Paths
	RDLPaths
} from './kernel-rdl';

// ═══════════════════════════════════════════════════════════════════
// RE-EXPORT FROM KERNEL-DOMAIN (Domain-Specific)
// ═══════════════════════════════════════════════════════════════════

export {
	// Schemas
	NetworkPeerAllocationSchema,
	AllocationNamespaceSchema,
	TreesNamespaceSchema,
	
	// Types
	type NetworkPeerAllocation,
	type AllocationNamespace,
	type TreesNamespace,
	
	// Helpers
	parseAllocationNamespace,
	parseTreesNamespace,
	
	// Paths
	DomainPaths
} from './kernel-domain';

// ═══════════════════════════════════════════════════════════════════
// UNIFIED USER SPACE SCHEMA
// ═══════════════════════════════════════════════════════════════════

import {
	ProgramsNamespaceSchema,
	SubscriptionsNamespaceSchema,
	NodesNamespaceSchema,
	CausalityNamespaceSchema,
	ReplicationNamespaceSchema
} from './kernel-core';

import {
	AllComputeNamespacesSchema
} from './kernel-rdl';

import {
	AllocationNamespaceSchema,
	TreesNamespaceSchema
} from './kernel-domain';

/**
 * Complete User Space (includes all namespaces)
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

export type UserSpace = z.infer<typeof UserSpaceSchema>;

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

// ═══════════════════════════════════════════════════════════════════
// UNIFIED PATH HELPERS
// ═══════════════════════════════════════════════════════════════════

import { CorePaths } from './kernel-core';
import { RDLPaths } from './kernel-rdl';
import { DomainPaths } from './kernel-domain';

/**
 * Unified path helpers for all kernel namespaces
 * 
 * Combines paths from:
 * - CorePaths (language-agnostic)
 * - RDLPaths (RDL-specific)
 * - DomainPaths (domain-specific)
 */
export const UserSpacePaths = {
	...CorePaths,
	...RDLPaths,
	...DomainPaths
};
