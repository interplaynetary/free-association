/**
 * P2P OS Kernel - Domain Extension (Economic Coordination)
 * 
 * Defines domain-specific schemas for economic coordination:
 * - Allocation namespace (commitments, allocation states)
 * - Trees namespace (priority/contribution trees)
 * 
 * This module is DOMAIN-SPECIFIC (not language-specific).
 * These schemas represent the economic coordination domain,
 * and could be used with any program language (RDL, SQL, WASM, etc.)
 * 
 * Other domains would have their own extension modules:
 * - kernel-chat.ts (messaging domain)
 * - kernel-auth.ts (authentication domain)
 * - kernel-storage.ts (file storage domain)
 * - etc.
 */

import * as z from 'zod';

// Import domain-specific schemas
import {
	CommitmentSchema,
	TwoTierAllocationStateSchema,
	RootNodeSchema
} from '../v2/schemas';

// ═══════════════════════════════════════════════════════════════════
// ALLOCATION SCHEMAS (Domain-Specific: Economic Coordination)
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
 * 
 * Stores data for the economic coordination domain:
 * - My commitment (capacity & needs)
 * - My allocation state (computed allocations)
 * - Network peer data (commitments & states from others)
 */
export const AllocationNamespaceSchema = z.object({
	commitment: CommitmentSchema.optional(),
	allocation_state: TwoTierAllocationStateSchema.optional(),
	network: z.record(z.string(), NetworkPeerAllocationSchema).optional()
});

// ═══════════════════════════════════════════════════════════════════
// TREES SCHEMAS (Domain-Specific: Priority/Contribution Trees)
// ═══════════════════════════════════════════════════════════════════

/**
 * Trees Namespace
 * Path: ~{pubKey}/trees/
 * 
 * Stores tree structures for priority-based allocation:
 * - My tree (my contributions & priorities)
 * - Network trees (peers' trees)
 */
export const TreesNamespaceSchema = z.object({
	my_tree: RootNodeSchema.optional(),
	network_trees: z.record(z.string(), RootNodeSchema).optional()
});

// ═══════════════════════════════════════════════════════════════════
// TYPE EXPORTS
// ═══════════════════════════════════════════════════════════════════

// Allocation types
export type NetworkPeerAllocation = z.infer<typeof NetworkPeerAllocationSchema>;
export type AllocationNamespace = z.infer<typeof AllocationNamespaceSchema>;

// Trees types
export type TreesNamespace = z.infer<typeof TreesNamespaceSchema>;

// ═══════════════════════════════════════════════════════════════════
// VALIDATION HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Validate allocation namespace
 */
export function parseAllocationNamespace(data: unknown): AllocationNamespace | null {
	const result = AllocationNamespaceSchema.safeParse(data);
	if (!result.success) {
		console.warn('[KERNEL-DOMAIN] Invalid allocation namespace:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Validate trees namespace
 */
export function parseTreesNamespace(data: unknown): TreesNamespace | null {
	const result = TreesNamespaceSchema.safeParse(data);
	if (!result.success) {
		console.warn('[KERNEL-DOMAIN] Invalid trees namespace:', result.error);
		return null;
	}
	return result.data;
}

// ═══════════════════════════════════════════════════════════════════
// PATH HELPERS (Domain-Specific)
// ═══════════════════════════════════════════════════════════════════

/**
 * Build Holster paths for allocation & trees namespaces
 */
export const DomainPaths = {
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
		`~${pubKey}/trees/network_trees/${peerPubKey}`
};

