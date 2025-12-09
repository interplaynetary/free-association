/**
 * Recognition-Based Council System
 * 
 * Councils where membership emerges from mutual recognition density (MRD)
 * and resources allocate based on collective recognition shares.
 * 
 * Key Concepts:
 * - Membership: Determined by MRD computation (not explicit votes)
 * - Resources: Allocated proportionally to collective recognition
 * - Influence: Weighted by recognition depth (MRD scores)
 * - Evolution: Membership and allocation update as recognition changes
 * 
 * Architecture:
 * - Uses collective-membership.ts for MRD computation
 * - Uses collective-recognition.ts for resource allocation
 * - Integrates with organizations.svelte.ts for network identity
 * - Publishes to Holster for distributed coordination
 * 
 * Example Use Cases:
 * - Fluid resource pools among peers
 * - Grant allocation based on contribution
 * - Experimental governance structures
 * - Community resource coordination
 */

import { MRDMembershipModule } from '$lib/protocol/stores/collective-membership';
import { extractRecognitionDataFromTrees } from '$lib/protocol/stores/collective-recognition';
import type {
	RecognitionData,
	MembershipOutput
} from '$lib/protocol/collective/schemas';
import type {
	Node,
	RootNode,
	AvailabilitySlot,
	NeedSlot,
	Organization,
	Commitment,
	AllocationResult
} from '@playnet/free-association/schemas';
import type { ComplianceFilter } from '@playnet/free-association/utils/filters';
import { calculateCollectiveRecognitionDistribution } from '$lib/protocol/distribution';
import { allocateWithDistribution } from '@playnet/free-association/allocation';

// ═══════════════════════════════════════════════════════════════════
// TYPE DEFINITIONS
// ═══════════════════════════════════════════════════════════════════

export interface RecognitionCouncilConfig {
	/** Organization ID (for network identity) */
	org_id?: string;
	
	/** MRD threshold for membership (default: 0.5) */
	mrd_threshold?: number;
	
	/** Minimum mutual recognition required (default: 0.0) */
	minimum_recognition?: number;
	
	/** Bootstrap members (seed for initial computation) */
	seed_members?: string[];
	
	/** Auto-update capacity members based on MRD? */
	auto_update_capacity_members?: boolean;
	
	/** How often to update capacity membership (ms) */
	membership_update_frequency_ms?: number;
}

export interface CouncilMembershipStatus {
	members: string[];
	candidates: Array<{ id: string; mrd: number }>;
	removed: string[];
	threshold: number;
	network_average: number;
	health_metrics: {
		recognition_density: number;
		average_mrd: number;
		mrd_variance: number;
		member_stability: number;
		member_count: number;
	};
}

export interface CouncilAllocationSummary {
	/** Raw allocation result from allocation engine */
	allocation_result: AllocationResult;
	
	/** Total capacity across all slots */
	total_capacity: number;
	
	/** Total allocated to members */
	total_allocated: number;
	
	/** Remaining unused capacity */
	unused_capacity: number;
	
	/** Aggregated allocations per member */
	member_allocations: Record<string, number>;
	
	/** Collective recognition shares (0-1) */
	member_shares: Record<string, number>;
}

// ═══════════════════════════════════════════════════════════════════
// RECOGNITION COUNCIL (Pure Recognition-Based)
// ═══════════════════════════════════════════════════════════════════

/**
 * Recognition-Based Council
 * 
 * Membership emerges from mutual recognition density.
 * Resources allocate based on collective recognition shares.
 * No explicit voting - recognition IS the governance mechanism.
 * 
 * Flow:
 * 1. Members publish recognition trees (who they recognize)
 * 2. MRD computation determines membership (who has deep enough relationships)
 * 3. Council declares collective capacities (resources to allocate)
 * 4. Members declare needs (what they require)
 * 5. Allocation algorithm distributes resources proportionally to recognition
 * 6. Recognition evolves → membership and allocation update automatically
 */
export class RecognitionCouncil {
	// Identity
	protected council_id: string;
	protected org_id: string;
	protected name: string;
	
	// Recognition & Membership (emergent)
	protected mrd_module: MRDMembershipModule;
	protected recognition_data: RecognitionData[] = [];
	protected current_members: Set<string> = new Set();
	protected membership_history: MembershipOutput[] = [];
	
	// Configuration
	protected mrd_threshold: number;
	protected minimum_recognition: number;
	protected auto_update_capacity_members: boolean;
	protected membership_update_frequency_ms: number;
	
	// Resources (collective capacity/need)
	protected capacity_slots: AvailabilitySlot[] = [];
	protected need_slots: NeedSlot[] = [];
	protected member_commitments: Map<string, Commitment> = new Map();
	protected member_trees: Map<string, Node> = new Map();
	protected compliance_filters: Map<string, ComplianceFilter> = new Map();
	
	// Timestamps
	protected created_at: number;
	protected updated_at: number;
	protected last_allocation_timestamp?: string;
	
	constructor(name: string, config: RecognitionCouncilConfig = {}) {
		this.name = name;
		this.council_id = config.org_id || 
			`org_council_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
		this.org_id = this.council_id;
		
		this.mrd_threshold = config.mrd_threshold ?? 0.5;
		this.minimum_recognition = config.minimum_recognition ?? 0.0;
		this.auto_update_capacity_members = config.auto_update_capacity_members ?? true;
		this.membership_update_frequency_ms = config.membership_update_frequency_ms ?? 
			(7 * 24 * 60 * 60 * 1000); // 7 days default
		
		this.mrd_module = new MRDMembershipModule(
			this.mrd_threshold,
			this.minimum_recognition
		);
		
		// Bootstrap with seed members if provided
		if (config.seed_members && config.seed_members.length > 0) {
			this.current_members = new Set(config.seed_members);
		}
		
		this.created_at = Date.now();
		this.updated_at = Date.now();
		
		console.log(`[RECOGNITION-COUNCIL] Created: ${this.name}`, {
			org_id: this.org_id,
			mrd_threshold: this.mrd_threshold,
			seed_members: this.current_members.size
		});
	}
	
	// ═══════════════════════════════════════════════════════════════
	// GETTERS
	// ═══════════════════════════════════════════════════════════════
	
	get id(): string {
		return this.council_id;
	}
	
	get organizationId(): string {
		return this.org_id;
	}
	
	get councilName(): string {
		return this.name;
	}
	
	get members(): string[] {
		return Array.from(this.current_members);
	}
	
	get memberCount(): number {
		return this.current_members.size;
	}
	
	get threshold(): number {
		return this.mrd_threshold;
	}
	
	// ═══════════════════════════════════════════════════════════════
	// RECOGNITION & MEMBERSHIP MANAGEMENT
	// ═══════════════════════════════════════════════════════════════
	
	/**
	 * Update recognition data and recompute membership
	 * 
	 * This is the core method - as recognition evolves, membership updates automatically.
	 * 
	 * @param recognitionData - Recognition relationships (from trees or direct)
	 * @returns Membership computation output
	 */
	updateRecognition(recognitionData: RecognitionData[]): MembershipOutput {
		this.recognition_data = recognitionData;
		
		// Compute membership based on MRD
		const output = this.mrd_module.computeMembership(
			recognitionData,
			this.current_members  // Seed with current members
		);
		
		// Update current members
		const oldMembers = new Set(this.current_members);
		this.current_members = new Set(output.members);
		
		// Store in history
		this.membership_history.push(output);
		this.updated_at = Date.now();
		
		// Log changes
		if (output.added.length > 0 || output.removed.length > 0) {
			console.log(`[COUNCIL ${this.name}] Membership updated:`, {
				added: output.added,
				removed: output.removed,
				total: output.members.length,
				network_average: output.networkAverage
			});
		}
		
		return output;
	}
	
	/**
	 * Update member's recognition tree
	 * 
	 * @param memberId - Member's identifier (pubkey)
	 * @param tree - Their recognition tree
	 */
	updateMemberTree(memberId: string, tree: Node): void {
		this.member_trees.set(memberId, tree);
		this.updated_at = Date.now();
		
		console.log(`[COUNCIL ${this.name}] Updated tree for ${memberId}`);
	}
	
	/**
	 * Recompute recognition data from member trees
	 * Helper method to extract recognition relationships from trees
	 */
	recomputeRecognitionFromTrees(): MembershipOutput {
		const recognitionData = extractRecognitionDataFromTrees(this.member_trees);
		return this.updateRecognition(recognitionData);
	}
	
	/**
	 * Check if someone is a member (based on latest MRD computation)
	 */
	isMember(pubkey: string): boolean {
		return this.current_members.has(pubkey);
	}
	
	/**
	 * Get member's current MRD score
	 */
	getMRD(pubkey: string): number {
		return this.mrd_module.getMrd(pubkey);
	}
	
	/**
	 * Get member's mutual recognition score (sum with other members)
	 */
	getMutualRecognitionScore(pubkey: string): number {
		return this.mrd_module.getMutualRecognitionScore(pubkey);
	}
	
	/**
	 * Get detailed breakdown of member's integration with others
	 */
	getMemberIntegration(pubkey: string): Record<string, number> {
		return this.mrd_module.getIntegrationBreakdown(pubkey, this.recognition_data);
	}
	
	/**
	 * Get comprehensive membership status
	 */
	getMembershipStatus(): CouncilMembershipStatus {
		if (this.membership_history.length === 0) {
			return {
				members: [],
				candidates: [],
				removed: [],
				threshold: this.mrd_threshold,
				network_average: 0,
				health_metrics: {
					recognition_density: 0,
					average_mrd: 0,
					mrd_variance: 0,
					member_stability: 1,
					member_count: 0
				}
			};
		}
		
		const latest = this.membership_history[this.membership_history.length - 1];
		
		const candidates = Object.entries(latest.membershipStatus)
			.filter(([_, status]) => status === 'candidate')
			.map(([id, _]) => ({
				id,
				mrd: latest.mrdScores[id] || 0
			}))
			.sort((a, b) => b.mrd - a.mrd);
		
		return {
			members: latest.members,
			candidates,
			removed: latest.removed,
			threshold: this.mrd_threshold,
			network_average: latest.networkAverage,
			health_metrics: {
				recognition_density: latest.healthMetrics.recognitionDensity,
				average_mrd: latest.healthMetrics.averageMRD,
				mrd_variance: latest.healthMetrics.mrdVariance,
				member_stability: latest.healthMetrics.memberStability,
				member_count: latest.healthMetrics.memberCount
			}
		};
	}
	
	/**
	 * Set new MRD threshold and recompute membership
	 */
	setThreshold(newThreshold: number): MembershipOutput {
		this.mrd_module.setThreshold(newThreshold);
		this.mrd_threshold = newThreshold;
		
		// Recompute with new threshold
		return this.updateRecognition(this.recognition_data);
	}
	
	// ═══════════════════════════════════════════════════════════════
	// RESOURCE MANAGEMENT (CAPACITY & NEEDS)
	// ═══════════════════════════════════════════════════════════════
	
	/**
	 * Add collective capacity slot
	 * 
	 * This capacity is available to all members based on collective recognition.
	 * Members list auto-populates with current council members.
	 * 
	 * @param slot - Availability slot (capacity to allocate)
	 */
	addCollectiveCapacity(slot: AvailabilitySlot): void {
		// Auto-populate with current members
		const slotWithMembers: AvailabilitySlot = {
			...slot,
			members: Array.from(this.current_members)
		};
		
		this.capacity_slots.push(slotWithMembers);
		this.updated_at = Date.now();
		
		console.log(`[COUNCIL ${this.name}] Added collective capacity:`, {
			slot_id: slot.id,
			quantity: slot.quantity,
			need_type: slot.need_type_id,
			members: this.current_members.size
		});
	}
	
	/**
	 * Remove collective capacity slot
	 */
	removeCollectiveCapacity(slotId: string): boolean {
		const originalLength = this.capacity_slots.length;
		this.capacity_slots = this.capacity_slots.filter(s => s.id !== slotId);
		const removed = this.capacity_slots.length < originalLength;
		
		if (removed) {
			this.updated_at = Date.now();
			console.log(`[COUNCIL ${this.name}] Removed capacity slot: ${slotId}`);
		}
		
		return removed;
	}
	
	/**
	 * Update capacity slot members to match current council members
	 * Call this after membership changes to keep capacity in sync
	 */
	syncCapacityMembers(): void {
		const currentMembers = Array.from(this.current_members);
		
		for (const slot of this.capacity_slots) {
			slot.members = currentMembers;
		}
		
		this.updated_at = Date.now();
		console.log(`[COUNCIL ${this.name}] Synced capacity members: ${currentMembers.length}`);
	}
	
	/**
	 * Declare member's needs
	 * 
	 * @param memberId - Member identifier (pubkey)
	 * @param needSlots - Array of need slots
	 */
	declareMemberNeeds(memberId: string, needSlots: NeedSlot[]): void {
		if (!this.isMember(memberId)) {
			console.warn(`[COUNCIL ${this.name}] ${memberId} is not a member, cannot declare needs`);
			return;
		}
		
		// Create or update commitment
		const existingCommitment = this.member_commitments.get(memberId);
		
		const commitment: Commitment = {
			need_slots: needSlots,
			capacity_slots: existingCommitment?.capacity_slots || [],
			global_recognition_weights: existingCommitment?.global_recognition_weights || null,
			timestamp: Date.now()
		};
		
		this.member_commitments.set(memberId, commitment);
		this.updated_at = Date.now();
		
		console.log(`[COUNCIL ${this.name}] Needs declared by ${memberId}:`, {
			slots: needSlots.length,
			types: [...new Set(needSlots.map(s => s.need_type_id))]
		});
	}
	
	/**
	 * Remove member's commitment
	 */
	removeMemberCommitment(memberId: string): boolean {
		const removed = this.member_commitments.delete(memberId);
		if (removed) {
			this.updated_at = Date.now();
		}
		return removed;
	}
	
	/**
	 * Get member's commitment
	 */
	getMemberCommitment(memberId: string): Commitment | undefined {
		return this.member_commitments.get(memberId);
	}
	
	/**
	 * Set compliance filter for a member
	 * 
	 * Filters constrain allocation amounts:
	 * - blocked: Cannot receive ($0)
	 * - capped: Maximum amount ($X)
	 * - unlimited: No constraint
	 * 
	 * @param memberId - Member identifier
	 * @param filter - Compliance filter
	 */
	setMemberFilter(memberId: string, filter: ComplianceFilter): void {
		this.compliance_filters.set(memberId, filter);
		this.updated_at = Date.now();
		
		console.log(`[COUNCIL ${this.name}] Filter set for ${memberId}:`, filter);
	}
	
	/**
	 * Remove compliance filter for a member
	 */
	removeMemberFilter(memberId: string): boolean {
		return this.compliance_filters.delete(memberId);
	}
	
	// ═══════════════════════════════════════════════════════════════
	// ALLOCATION COMPUTATION
	// ═══════════════════════════════════════════════════════════════
	
	/**
	 * Compute resource allocations across all members
	 * 
	 * Uses elegant distribution + allocation architecture:
	 * 1. Calculate collective recognition shares (distribution.ts)
	 * 2. Allocate capacity to needs using shares (allocation.ts)
	 * 3. Apply compliance filters
	 * 4. Return detailed allocation breakdown
	 * 
	 * @returns Allocation result with slot-level transparency
	 */
	computeAllocations(): AllocationResult {
		// Sync capacity members with current council members
		this.syncCapacityMembers();
		
		// STEP 1: Calculate distribution (WHO gets WHAT share)
		const distribution = calculateCollectiveRecognitionDistribution(
			Array.from(this.current_members),
			this.member_trees
		);
		
		console.log(`[COUNCIL ${this.name}] Distribution computed:`, {
			method: distribution.method,
			recipients: Object.keys(distribution.shares).length,
			total_pool: distribution.metadata?.totalPool || 0
		});
		
		// STEP 2: Build commitments map from member commitments
		const allCommitments: Record<string, Commitment> = Object.fromEntries(
			this.member_commitments
		);
		
		// STEP 3: Run allocation engine (HOW to allocate slots)
		const result = allocateWithDistribution(
			this.council_id,  // Council as provider
			this.capacity_slots,  // Council's collective capacity
			distribution,  // Collective recognition shares
			allCommitments,  // Members' needs
			undefined,  // needsIndex (optional optimization)
			this.compliance_filters  // Compliance filters
		);
		
		this.last_allocation_timestamp = new Date().toISOString();
		
		// Calculate summary metrics
		const totalCapacity = this.capacity_slots.reduce((sum, slot) => sum + slot.quantity, 0);
		const totalAllocated = result.allocations.reduce((sum, alloc) => sum + alloc.quantity, 0);
		
		console.log(`[COUNCIL ${this.name}] Allocations computed:`, {
			total_capacity: totalCapacity,
			total_allocated: totalAllocated,
			unused: totalCapacity - totalAllocated,
			utilization: totalCapacity > 0 ? ((totalAllocated / totalCapacity) * 100).toFixed(1) + '%' : '0%',
			slot_allocations: result.allocations.length,
			members: this.current_members.size
		});
		
		return result;
	}
	
	/**
	 * Get simplified allocation summary with aggregated metrics
	 */
	getAllocationSummary(): CouncilAllocationSummary {
		const result = this.computeAllocations();
		
		// Calculate total capacity
		const totalCapacity = this.capacity_slots.reduce((sum, slot) => sum + slot.quantity, 0);
		
		// Calculate total allocated
		const totalAllocated = result.allocations.reduce((sum, alloc) => sum + alloc.quantity, 0);
		
		// Aggregate allocations per member
		const memberAllocations: Record<string, number> = {};
		for (const alloc of result.allocations) {
			const current = memberAllocations[alloc.recipient_pubkey] || 0;
			memberAllocations[alloc.recipient_pubkey] = current + alloc.quantity;
		}
		
		// Get recognition shares from distribution
		const distribution = calculateCollectiveRecognitionDistribution(
			Array.from(this.current_members),
			this.member_trees
		);
		
		return {
			allocation_result: result,
			total_capacity: totalCapacity,
			total_allocated: totalAllocated,
			unused_capacity: totalCapacity - totalAllocated,
			member_allocations: memberAllocations,
			member_shares: distribution.shares
		};
	}
	
	/**
	 * Get allocation for specific member
	 */
	getMemberAllocation(memberId: string): number {
		const result = this.computeAllocations();
		
		// Sum all allocations for this member
		return result.allocations
			.filter(alloc => alloc.recipient_pubkey === memberId)
			.reduce((sum, alloc) => sum + alloc.quantity, 0);
	}
	
	/**
	 * Get member's collective recognition share (0-1)
	 */
	getMemberRecognitionShare(memberId: string): number {
		const distribution = calculateCollectiveRecognitionDistribution(
			Array.from(this.current_members),
			this.member_trees
		);
		
		return distribution.shares[memberId] || 0;
	}
	
	// ═══════════════════════════════════════════════════════════════
	// NETWORK INTEGRATION (Holster)
	// ═══════════════════════════════════════════════════════════════
	
	/**
	 * Get council as Organization object (for publishing to network)
	 */
	asOrganization(): Organization {
		return {
			org_id: this.org_id,
			names: {
				en: this.name
			},
			emoji: '🏛️',  // Council emoji
			description: `Recognition-based council: ${this.name}`,
			created_at: this.created_at,
			updated_at: this.updated_at
		};
	}
	
	/**
	 * Export council state for persistence/network
	 */
	exportState(): {
		council_id: string;
		org_id: string;
		name: string;
		config: RecognitionCouncilConfig;
		members: string[];
		capacity_slots: AvailabilitySlot[];
		member_commitments: Array<{ member_id: string; commitment: Commitment }>;
		compliance_filters: Array<{ member_id: string; filter: ComplianceFilter }>;
		created_at: number;
		updated_at: number;
	} {
		return {
			council_id: this.council_id,
			org_id: this.org_id,
			name: this.name,
			config: {
				org_id: this.org_id,
				mrd_threshold: this.mrd_threshold,
				minimum_recognition: this.minimum_recognition,
				auto_update_capacity_members: this.auto_update_capacity_members,
				membership_update_frequency_ms: this.membership_update_frequency_ms
			},
			members: Array.from(this.current_members),
			capacity_slots: this.capacity_slots,
			member_commitments: Array.from(this.member_commitments.entries()).map(([member_id, commitment]) => ({
				member_id,
				commitment
			})),
			compliance_filters: Array.from(this.compliance_filters.entries()).map(([member_id, filter]) => ({
				member_id,
				filter
			})),
			created_at: this.created_at,
			updated_at: this.updated_at
		};
	}
	
	/**
	 * Import council state from persistence/network
	 */
	static importState(state: ReturnType<RecognitionCouncil['exportState']>): RecognitionCouncil {
		const council = new RecognitionCouncil(state.name, {
			...state.config,
			seed_members: state.members
		});
		
		council.capacity_slots = state.capacity_slots;
		
		// Restore member commitments
		for (const { member_id, commitment } of state.member_commitments) {
			council.member_commitments.set(member_id, commitment);
		}
		
		// Restore filters
		for (const { member_id, filter } of state.compliance_filters) {
			council.compliance_filters.set(member_id, filter);
		}
		
		council.created_at = state.created_at;
		council.updated_at = state.updated_at;
		
		return council;
	}
}

// ═══════════════════════════════════════════════════════════════════
// HELPER FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Create recognition-based council from scratch
 */
export function createRecognitionCouncil(
	name: string,
	config: RecognitionCouncilConfig = {}
): RecognitionCouncil {
	return new RecognitionCouncil(name, config);
}

/**
 * Create council and register as organization globally
 * 
 * @param name - Council name
 * @param config - Council configuration
 * @returns Council instance
 */
export function createAndRegisterCouncil(
	name: string,
	config: RecognitionCouncilConfig = {}
): RecognitionCouncil {
	const council = new RecognitionCouncil(name, config);
	
	console.log(`[RECOGNITION-COUNCIL] Created and registered: ${name}`, {
		org_id: council.organizationId,
		members: council.memberCount
	});
	
	return council;
}

/**
 * Helper to create recognition data from simple percentage maps
 * 
 * Example:
 * ```typescript
 * const recognition = createRecognitionData({
 *   alice: { bob: 40, charlie: 30, diana: 30 },
 *   bob: { alice: 50, charlie: 30, diana: 20 }
 * });
 * ```
 */
export function createRecognitionData(
	recognitionMap: Record<string, Record<string, number>>
): RecognitionData[] {
	const data: RecognitionData[] = [];
	const now = new Date();
	
	for (const [fromId, recognitions] of Object.entries(recognitionMap)) {
		for (const [toId, percentage] of Object.entries(recognitions)) {
			data.push({
				fromId,
				toId,
				percentage,
				timestamp: now
			});
		}
	}
	
	return data;
}

/**
 * Helper to print membership status in readable format
 */
export function printMembershipStatus(council: RecognitionCouncil): void {
	const status = council.getMembershipStatus();
	
	console.log(`\n=== ${council.councilName} - Membership Status ===`);
	console.log(`Threshold: ${status.threshold}`);
	console.log(`Network Average MRS: ${status.network_average.toFixed(2)}\n`);
	
	console.log(`Members (${status.members.length}):`);
	for (const member of status.members) {
		const mrd = council.getMRD(member);
		const mrs = council.getMutualRecognitionScore(member);
		console.log(`  ${member}: MRD=${mrd.toFixed(2)}, MRS=${mrs.toFixed(2)}`);
	}
	
	if (status.candidates.length > 0) {
		console.log(`\nCandidates (${status.candidates.length}):`);
		for (const candidate of status.candidates.slice(0, 5)) {
			console.log(`  ${candidate.id}: MRD=${candidate.mrd.toFixed(2)}`);
		}
	}
	
	console.log(`\nHealth Metrics:`);
	console.log(`  Recognition Density: ${(status.health_metrics.recognition_density * 100).toFixed(1)}%`);
	console.log(`  Average MRD: ${status.health_metrics.average_mrd.toFixed(2)}`);
	console.log(`  Member Stability: ${(status.health_metrics.member_stability * 100).toFixed(1)}%`);
	console.log(`  Member Count: ${status.health_metrics.member_count}`);
}

/**
 * Helper to print allocation results in readable format
 */
export function printAllocationSummary(council: RecognitionCouncil): void {
	const summary = council.getAllocationSummary();
	
	console.log(`\n=== ${council.councilName} - Allocation Summary ===`);
	console.log(`Total Capacity: ${summary.total_capacity}`);
	console.log(`Total Allocated: ${summary.total_allocated}`);
	console.log(`Unused: ${summary.unused_capacity}\n`);
	
	console.log(`Member Allocations:`);
	for (const [memberId, allocation] of Object.entries(summary.member_allocations)) {
		const share = summary.member_shares[memberId];
		const percentage = share ? (share * 100).toFixed(1) : '0.0';
		console.log(`  ${memberId}: ${allocation} (${percentage}% share)`);
	}
}

// Export types
export type { RecognitionData, MembershipOutput } from '$lib/protocol/collective/schemas';
export type { ComplianceFilter } from '@playnet/free-association/utils/filters';

