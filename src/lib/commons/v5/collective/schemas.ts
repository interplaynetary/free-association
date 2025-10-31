/**
 * Collective Recognition & Allocation Schemas
 * 
 * These schemas complement the v5 core schemas with collective-specific types:
 * - Collective capacity declarations
 * - Collective need declarations
 * - Compliance filters for member allocations
 * - Recognition data for MRD membership computation
 * - Allocation computation results
 */

import * as z from 'zod';
import { 
	NeedSlotSchema, 
	AvailabilitySlotSchema,
	type Node,
	type NonRootNode,
	type RootNode,
	type AvailabilitySlot,
	type NeedSlot
} from '../schemas';

// Re-export commonly used v5 types (but not legacy aliases that we override)
export type { 
	Node, 
	NonRootNode, 
	RootNode,
	AvailabilitySlot,
	NeedSlot
} from '../schemas';

// ═══════════════════════════════════════════════════════════════════
// COMPLIANCE FILTERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Compliance Filter - Applied to individual members in collective allocations
 * 
 * Types:
 * - blocked: $0 (member receives nothing)
 * - capped: $X (member receives max of X)
 * - unlimited: No limit (member receives according to recognition share)
 * 
 * Example: External provider may cap a risky member at $10K while others are unlimited
 */
export const ComplianceFilterSchema = z.discriminatedUnion('type', [
	z.object({
		type: z.literal('blocked'),
		value: z.literal(0)
	}),
	z.object({
		type: z.literal('capped'),
		value: z.number().positive()
	}),
	z.object({
		type: z.literal('unlimited')
	})
]);

export type ComplianceFilter = z.infer<typeof ComplianceFilterSchema>;

// ═══════════════════════════════════════════════════════════════════
// COLLECTIVE CAPACITY & NEED DECLARATIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Base Need Declaration
 * 
 * Represents a member's need in a collective context.
 * Includes need slots (from v5) plus fulfillment tracking.
 */
export const BaseNeedSchema = z.object({
	id: z.string().min(1),
	declarer_id: z.string().min(1), // Member who declared this need
	need_slots: z.array(NeedSlotSchema), // Array of NeedSlot (from v5 schemas)
	fulfilled_amount: z.number().nonnegative().default(0),
	status: z.enum(['open', 'partially-fulfilled', 'fulfilled']).default('open')
});

export type BaseNeed = z.infer<typeof BaseNeedSchema>;

/**
 * Base Capacity Declaration
 * 
 * Represents a provider's capacity in a collective context.
 * Can be individual or collective (with members).
 */
export const BaseCapacitySchema = z.object({
	id: z.string().min(1),
	name: z.string().min(1), // Required: capacity name/description
	provider_id: z.string().optional(), // Provider's pubkey
	owner_id: z.string().optional(),    // Alternative to provider_id
	capacity_slots: z.array(AvailabilitySlotSchema), // Array of AvailabilitySlot (from v5 schemas)
	
	// Collective capacity fields
	members: z.array(z.string()).optional(), // Member pubkeys
	filters: z.record(z.string(), ComplianceFilterSchema).optional(), // Member filters
	
	// Dynamic membership (MRD-based)
	auto_update_members_by_mrd: z.boolean().default(false),
	mrd_threshold: z.number().min(0).max(1).optional(),
	last_membership_update: z.string().optional() // ISO timestamp
});

export type BaseCapacity = z.infer<typeof BaseCapacitySchema>;

// ═══════════════════════════════════════════════════════════════════
// ALLOCATION RECORDS
// ═══════════════════════════════════════════════════════════════════

/**
 * Allocation Record
 * 
 * Represents a single allocation from provider to recipient.
 * Created by the allocation computation algorithm.
 */
export const AllocationSchema = z.object({
	id: z.string().min(1),
	provider_id: z.string().min(1),
	recipient_id: z.string().min(1),
	source_capacity_id: z.string().min(1),
	amount: z.number().nonnegative(),
	unit: z.string(),
	recipient_collective_recognition_share: z.number().min(0).max(1),
	timestamp: z.string(), // ISO timestamp
	applied_filter: ComplianceFilterSchema,
	ideal_allocation: z.number().nonnegative(),
	was_redistributed: z.boolean().default(false)
});

export type Allocation = z.infer<typeof AllocationSchema>;

/**
 * Allocation Computation Result
 * 
 * Complete result from the collective recognition allocation algorithm.
 * Includes recognition shares, filter applications, and final allocations.
 */
export const AllocationComputationResultSchema = z.object({
	capacity_id: z.string().min(1),
	provider_id: z.string().min(1),
	total_capacity: z.number().nonnegative(),
	member_set: z.array(z.string()),
	
	// Step 1: Collective recognition computation
	collective_recognition_pool: z.number().nonnegative(),
	collective_recognition_shares: z.record(z.string(), z.number().nonnegative()),
	
	// Step 2: Ideal allocations (before filters)
	ideal_allocations: z.record(z.string(), z.number().nonnegative()),
	
	// Step 3: Filter application
	applied_filters: z.record(z.string(), ComplianceFilterSchema),
	filtered_allocations: z.record(z.string(), z.number().nonnegative()),
	blocked_members: z.array(z.string()),
	capped_members: z.array(z.string()),
	
	// Step 4: Redistribution
	unallocated_amount: z.number().nonnegative(),
	redistribution_pool: z.array(z.string()), // Members who can receive more
	redistributed_amounts: z.record(z.string(), z.number().nonnegative()),
	
	// Step 5: Final allocations
	final_allocations: z.record(z.string(), z.number().nonnegative()),
	unused_capacity: z.number().nonnegative(),
	
	// Metadata
	computation_timestamp: z.string(),
	algorithm_version: z.string(),
	
	// Optional: Slot-level detail (if using slot-based allocation)
	slot_allocations: z.array(z.any()).optional(),
	availability_slot_states: z.any().optional(),
	need_slot_states: z.any().optional(),
	
	// Optional: Dynamic membership info
	members_updated: z.boolean().optional(),
	members_added: z.array(z.string()).optional(),
	members_removed: z.array(z.string()).optional()
});

export type AllocationComputationResult = z.infer<typeof AllocationComputationResultSchema>;

// ═══════════════════════════════════════════════════════════════════
// RECOGNITION DATA (for MRD computation)
// ═══════════════════════════════════════════════════════════════════

/**
 * Recognition Data
 * 
 * Represents a directed recognition relationship for MRD computation.
 * Used by the MRD Membership Module.
 */
export const RecognitionDataSchema = z.object({
	fromId: z.string().min(1),
	toId: z.string().min(1),
	percentage: z.number().min(0).max(100), // Recognition percentage (0-100)
	timestamp: z.date()
});

export type RecognitionData = z.infer<typeof RecognitionDataSchema>;

/**
 * Membership Output
 * 
 * Result from MRD membership computation.
 */
export const MembershipOutputSchema = z.object({
	timestamp: z.date(),
	members: z.array(z.string()),
	added: z.array(z.string()),
	removed: z.array(z.string()),
	mrdScores: z.record(z.string(), z.number()),
	membershipStatus: z.record(z.string(), z.enum(['member', 'candidate', 'removed'])),
	mutualRecognitionScores: z.record(z.string(), z.number()),
	networkAverage: z.number(),
	healthMetrics: z.object({
		recognitionDensity: z.number(),
		averageMRD: z.number(),
		mrdVariance: z.number(),
		memberStability: z.number(),
		memberCount: z.number()
	})
});

export type MembershipOutput = z.infer<typeof MembershipOutputSchema>;

/**
 * Health Metrics
 * 
 * System health metrics for collective membership.
 */
export const HealthMetricsSchema = z.object({
	recognitionDensity: z.number(),
	averageMRD: z.number(),
	mrdVariance: z.number(),
	memberStability: z.number(),
	memberCount: z.number()
});

export type HealthMetrics = z.infer<typeof HealthMetricsSchema>;

// ═══════════════════════════════════════════════════════════════════
// COLLECTIVE TREE TYPES
// ═══════════════════════════════════════════════════════════════════

/**
 * Entity ID - Universal identifier for individuals or collectives
 */
export type EntityID = string;

/**
 * Capacities Collection - Maps capacity types to their declarations
 */
export type CapacitiesCollection = Record<string, {
	capacity_slots?: Array<any>;
	[key: string]: any;
}>;

/**
 * Entity - Either an individual (Node) or a Collective
 */
export type Entity = Node | Collective;

/**
 * Forest - Collection of entity trees indexed by entity ID
 */
export type Forest = Map<EntityID, Entity>;

/**
 * Collective - Synthetic entity composed of multiple members
 */
export interface Collective {
	id: EntityID;
	type: 'Collective';
	members: Entity[];
	weights: Map<string, number>;
	[key: string]: any;
}

/**
 * Node Data for Merging - Tracks contributor node with weights
 */
export interface ContributorNodeData {
	originalNode: Node;
	weightInParent: number;
	contributorWeight: number;
}

/**
 * Node Merge Data - Intermediate structure for tree merging
 */
export interface NodeMergeData {
	id: string;
	name: string;
	contributors: Map<string, ContributorNodeData>;
	children: Map<string, NodeMergeData>;
	path: string[];
}

/**
 * Proportional Node Analysis - Detailed contributor percentage breakdown
 */
export interface ProportionalNode {
	contributor_id: string;
	percentage_of_node: number;
	individual_node_percentage: number;
	contributor_collective_weight: number;
	path_weight_contribution: number;
	derivation_steps: Array<{
		level: number;
		node_id: string;
		individual_percentage: number;
		collective_weight: number;
		cumulative_path_weight: number;
	}>;
}

/**
 * Collective Node - Base type for nodes in a collective tree
 */
export type CollectiveNode = CollectiveRootNode | CollectiveNonRootNode;

/**
 * Collective Root Node - Root of a merged collective tree
 */
export interface CollectiveRootNode {
	id: string;
	name: string;
	type: 'CollectiveRootNode';
	manual_fulfillment: number | null;
	children: CollectiveNode[];
	created_at: string;
	updated_at: string;
	contributors: string[];
	contributor_weights: Record<string, number>;
	source_trees: Record<string, Node>;
}

/**
 * Collective Non-Root Node - Child node in a merged collective tree
 */
export interface CollectiveNonRootNode {
	id: string;
	name: string;
	type: 'CollectiveNonRootNode';
	manual_fulfillment: number | null;
	children: CollectiveNode[];
	weight_percentage: number;
	parent_id: string;
	contributor_ids: string[];
	source_contributors: Record<string, number>;
	merged_from_nodes: string[];
}

/**
 * Collective Tree - Complete merged tree with metadata
 */
export interface CollectiveTree {
	id: string;
	root: CollectiveRootNode;
	contributors: string[];
	recognition_matrix: Record<string, Record<string, number>>;
	creation_timestamp: string;
	last_updated: string;
	merge_algorithm_version: string;
	total_nodes_merged: number;
	merge_conflicts: Array<{
		node_id: string;
		conflict_type: string;
		resolution: string;
	}>;
}

/**
 * Tree Merge Configuration
 */
export interface TreeMergeConfig {
	contributor_trees: Record<string, Node>;
	recognition_shares?: Record<string, number>;
	merge_strategy: 'weighted_average' | 'union' | 'intersection';
	conflict_resolution: 'merge' | 'keep_first' | 'keep_last' | 'manual';
	name_collision_strategy: 'append_contributor' | 'weighted_priority' | 'manual_resolve';
}

/**
 * Tree Merge Result
 */
export interface TreeMergeResult {
	collective_tree: CollectiveTree;
	merge_stats: {
		total_contributors: number;
		nodes_merged: number;
		conflicts_resolved: number;
		execution_time_ms: number;
	};
	warnings: string[];
	errors: string[];
}

/**
 * Collective Capacity Allocation - Result of capacity allocation computation
 */
export interface CollectiveCapacityAllocation {
	collective_tree_id: string;
	total_collective_capacity: Record<string, number>;
	node_capacity_allocations: Record<string, Record<string, number>>;
	contributor_capacity_shares: Record<string, Record<string, number>>;
	allocation_efficiency: number;
	allocation_fairness: number;
}

/**
 * Tree Filter Config - Configuration for filtering collective trees
 */
export interface TreeFilterConfig {
	minimum_percentage?: number;
	minimum_quorum?: number;
	minimum_collective_recognition?: number;
	contributor_whitelist?: string[];
	contributor_blacklist?: string[];
	preserve_paths?: boolean;
}

/**
 * Filtered Tree Result - Result of tree filtering operation
 */
export interface FilteredTreeResult {
	filtered_tree: CollectiveTree;
	removed_nodes: Array<{
		node_id: string;
		node_name: string;
		reason: string;
		original_weight: number;
		contributor_count: number;
	}>;
	filter_stats: {
		original_node_count: number;
		filtered_node_count: number;
		nodes_removed: number;
		total_weight_removed: number;
		contributors_affected: string[];
	};
}

// ═══════════════════════════════════════════════════════════════════
// VALIDATION HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Parse and validate base need
 */
export function parseBaseNeed(data: unknown): BaseNeed | null {
	const result = BaseNeedSchema.safeParse(data);
	if (!result.success) {
		console.warn('[COLLECTIVE-SCHEMA] Invalid base need:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Parse and validate base capacity
 */
export function parseBaseCapacity(data: unknown): BaseCapacity | null {
	const result = BaseCapacitySchema.safeParse(data);
	if (!result.success) {
		console.warn('[COLLECTIVE-SCHEMA] Invalid base capacity:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Parse and validate allocation computation result
 */
export function parseAllocationComputationResult(data: unknown): AllocationComputationResult | null {
	const result = AllocationComputationResultSchema.safeParse(data);
	if (!result.success) {
		console.warn('[COLLECTIVE-SCHEMA] Invalid allocation computation result:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Parse and validate recognition data
 */
export function parseRecognitionData(data: unknown): RecognitionData | null {
	const result = RecognitionDataSchema.safeParse(data);
	if (!result.success) {
		console.warn('[COLLECTIVE-SCHEMA] Invalid recognition data:', result.error);
		return null;
	}
	return result.data;
}

// ═══════════════════════════════════════════════════════════════════
// MEMBERSHIP UTILITIES
// ═══════════════════════════════════════════════════════════════════

/**
 * Check if capacity membership should be updated
 * 
 * Returns true if:
 * - auto_update_members_by_mrd is enabled
 * - Either no members exist yet OR last update was > 24h ago
 */
export function shouldUpdateCapacityMembership(capacity: BaseCapacity): boolean {
	if (!capacity.auto_update_members_by_mrd) {
		return false;
	}
	
	// If no members, should update
	if (!capacity.members || capacity.members.length === 0) {
		return true;
	}
	
	// If never updated, should update
	if (!capacity.last_membership_update) {
		return true;
	}
	
	// Check if last update was > 24 hours ago
	const lastUpdate = new Date(capacity.last_membership_update);
	const now = new Date();
	const hoursSinceUpdate = (now.getTime() - lastUpdate.getTime()) / (1000 * 60 * 60);
	
	return hoursSinceUpdate > 24;
}

/**
 * Update capacity membership based on MRD computation
 * 
 * @param currentMembers - Current member list
 * @param recognitionData - Recognition data for MRD computation
 * @param threshold - MRD threshold for membership (default: 0.5)
 * @param options - Optional configuration
 * @returns Updated membership result
 */
export function updateCapacityMembership(
	currentMembers: string[],
	recognitionData: RecognitionData[],
	threshold: number = 0.5,
	options?: {
		preserveFoundingMembers?: string[];
		maxNewMembers?: number;
	}
): MembershipOutput {
	// Build MRD scores
	const recognitionMatrix = new Map<string, Map<string, number>>();
	const participants = new Set<string>();
	
	for (const rec of recognitionData) {
		if (!recognitionMatrix.has(rec.fromId)) {
			recognitionMatrix.set(rec.fromId, new Map());
		}
		recognitionMatrix.get(rec.fromId)!.set(rec.toId, rec.percentage);
		participants.add(rec.fromId);
		participants.add(rec.toId);
	}
	
	// Calculate MRD scores for each participant
	const mrdScores: Record<string, number> = {};
	const mutualRecognitionScores: Record<string, number> = {};
	
	for (const participantId of participants) {
		let totalMRD = 0;
		let totalMR = 0;
		let count = 0;
		
		for (const otherId of currentMembers) {
			if (otherId === participantId) continue;
			
			const fromParticipant = recognitionMatrix.get(participantId)?.get(otherId) || 0;
			const fromOther = recognitionMatrix.get(otherId)?.get(participantId) || 0;
			const mutualRec = Math.min(fromParticipant, fromOther);
			
			totalMR += mutualRec;
			totalMRD += mutualRec;
			count++;
		}
		
		mrdScores[participantId] = count > 0 ? totalMRD / count : 0;
		mutualRecognitionScores[participantId] = count > 0 ? totalMR / count : 0;
	}
	
	// Determine new members
	const newMembers: string[] = [];
	const added: string[] = [];
	const removed: string[] = [];
	const membershipStatus: Record<string, 'member' | 'candidate' | 'removed'> = {};
	
	// Add participants who meet threshold
	for (const [participantId, score] of Object.entries(mrdScores)) {
		if (score >= threshold) {
			newMembers.push(participantId);
			membershipStatus[participantId] = 'member';
			
			if (!currentMembers.includes(participantId)) {
				added.push(participantId);
			}
		} else {
			membershipStatus[participantId] = 'candidate';
		}
	}
	
	// Track removed members
	for (const memberId of currentMembers) {
		if (!newMembers.includes(memberId)) {
			removed.push(memberId);
			membershipStatus[memberId] = 'removed';
		}
	}
	
	// Apply options
	if (options?.preserveFoundingMembers) {
		for (const foundingMember of options.preserveFoundingMembers) {
			if (!newMembers.includes(foundingMember)) {
				newMembers.push(foundingMember);
				membershipStatus[foundingMember] = 'member';
			}
		}
	}
	
	if (options?.maxNewMembers && added.length > options.maxNewMembers) {
		// Sort by MRD score and take top N
		const sortedAdded = added.sort((a, b) => (mrdScores[b] || 0) - (mrdScores[a] || 0));
		const limitedAdded = sortedAdded.slice(0, options.maxNewMembers);
		const excludedAdded = sortedAdded.slice(options.maxNewMembers);
		
		// Remove excluded from newMembers
		for (const excluded of excludedAdded) {
			const index = newMembers.indexOf(excluded);
			if (index > -1) {
				newMembers.splice(index, 1);
				membershipStatus[excluded] = 'candidate';
			}
		}
	}
	
	// Calculate health metrics
	const scores = Object.values(mrdScores);
	const avgMRD = scores.length > 0 ? scores.reduce((a, b) => a + b, 0) / scores.length : 0;
	const mrdVariance = scores.length > 0 
		? scores.reduce((sum, score) => sum + Math.pow(score - avgMRD, 2), 0) / scores.length 
		: 0;
	
	const networkAverage = Object.values(mutualRecognitionScores).reduce((a, b) => a + b, 0) / 
		(Object.values(mutualRecognitionScores).length || 1);
	
	return {
		timestamp: new Date(),
		members: newMembers,
		added,
		removed,
		mrdScores,
		membershipStatus,
		mutualRecognitionScores,
		networkAverage,
		healthMetrics: {
			recognitionDensity: participants.size / (newMembers.length || 1),
			averageMRD: avgMRD,
			mrdVariance,
			memberStability: 1 - (added.length + removed.length) / (currentMembers.length || 1),
			memberCount: newMembers.length
		}
	};
}
