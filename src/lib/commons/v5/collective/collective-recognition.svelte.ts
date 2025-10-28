/**
 * Collective Recognition & Resource Allocation Protocol (V5)
 * 
 * Pure computational implementation of the collective-rec.md protocol:
 * - Need declarations (slot-based)
 * - Provider capacity declarations (slot-based)
 * - Collective-recognition-share computation (global recognition model)
 * - Compliance filter application
 * - Allocation with slot-level redistribution
 * - Union of filters (for proxy scenarios)
 * 
 * V5 UPDATES:
 * - Slot-native architecture (AvailabilitySlot, NeedSlot)
 * - Global recognition model (same MR across all need types)
 * - Hierarchical availability windows
 * - ITC causality tracking
 * - Time/location compatibility at slot level
 * 
 * Mathematical Foundation:
 * - Recognition shares guide allocation priorities
 * - Filters apply hard limits ($0, $X, Unlimited)
 * - Redistribution ensures maximum capacity utilization
 * - Transparent computation at every step
 */

import type {
	Node,
	AvailabilitySlot,
	NeedSlot
} from '../schemas';
import { mutualFulfillment } from '../protocol';
import {
	slotsCompatible as slotsCompatibleV5,
	timeRangesOverlap,
	locationsCompatible,
	haversineDistance
} from '../match.svelte';
import {
	shouldUpdateCapacityMembership,
	updateCapacityMembership,
	type BaseNeed,
	type BaseCapacity,
	type ComplianceFilter,
	type Allocation,
	type AllocationComputationResult,
	type RecognitionData
} from './schemas';


// ═══════════════════════════════════════════════════════════════════
// FILTER UTILITIES
// ═══════════════════════════════════════════════════════════════════

/**
 * Get numeric value from filter for computation
 */
export function getFilterValue(filter: ComplianceFilter): number {
	if (filter.type === 'blocked') return 0;
	if (filter.type === 'capped') return filter.value;
	return Infinity; // Unlimited
}

/**
 * Create filter from numeric value
 */
export function createFilter(value: number): ComplianceFilter {
	if (value === 0) return { type: 'blocked', value: 0 };
	if (value === Infinity || value < 0) return { type: 'unlimited' };
	return { type: 'capped', value };
}

/**
 * Union of filters - most restrictive wins
 * Used when external provider uses entity as proxy
 */
export function unionOfFilters(filter1: ComplianceFilter, filter2: ComplianceFilter): ComplianceFilter {
	const val1 = getFilterValue(filter1);
	const val2 = getFilterValue(filter2);
	return createFilter(Math.min(val1, val2));
}

// === NEED UTILITIES ===

/**
 * Calculate total need amount from need slots
 */
export function calculateTotalNeedAmount(need: BaseNeed): number {
	return need.need_slots.reduce((sum, slot) => sum + slot.quantity, 0);
}

/**
 * Calculate remaining (unfulfilled) need amount
 */
export function getRemainingNeed(need: BaseNeed): number {
	const totalNeed = calculateTotalNeedAmount(need);
	return Math.max(0, totalNeed - need.fulfilled_amount);
}

// === SLOT-LEVEL ALLOCATION STRUCTURES ===

/**
 * Represents allocation of a specific availability slot to a specific need slot
 */
export interface SlotAllocation {
	availability_slot_id: string;
	need_slot_id: string;
	recipient_id: string;
	quantity: number;
	time_compatible: boolean;
	location_compatible: boolean;
}

/**
 * Tracks remaining capacity in each availability slot
 */
export interface AvailabilitySlotState {
	slot_id: string;
	original_quantity: number;
	remaining_quantity: number;
	allocations: SlotAllocation[];
}

/**
 * Tracks fulfillment of each need slot
 */
export interface NeedSlotState {
	slot_id: string;
	need_id: string;
	recipient_id: string;
	original_quantity: number;
	fulfilled_quantity: number;
	allocations: SlotAllocation[];
}

// === SLOT MATCHING UTILITIES (V5) ===

/**
 * Check if a need slot can be fulfilled by an availability slot (v5)
 * 
 * V5 CRITICAL: Includes need_type_id matching for multi-dimensional framework!
 * Delegates to match.svelte.ts for comprehensive slot compatibility checking.
 * 
 * Checks:
 * - Type compatibility (need_type_id must match)
 * - Time compatibility (hierarchical availability windows + timezone support)
 * - Location compatibility (city/country/coordinates/online)
 */
export function slotsCompatible(
	needSlot: NeedSlot,
	availabilitySlot: AvailabilitySlot
): boolean {
	// Use v5's comprehensive slot compatibility checker
	// This includes need_type_id matching, time/location, and recurrence logic
	return slotsCompatibleV5(needSlot, availabilitySlot);
}

/**
 * Match need slots to availability slots and calculate possible allocations
 * 
 * @param need - Need with slots to fulfill
 * @param capacity - Capacity with availability slots
 * @param maxAmount - Maximum amount that can be allocated (from recognition share + filters)
 * @returns Object with matchable slots and total matchable amount
 */
export function matchNeedToCapacitySlots(
	need: BaseNeed,
	capacity: import('$lib/schema').BaseCapacity,
	maxAmount: number
): {
	compatible_pairs: Array<{
		need_slot: import('$lib/schema').NeedSlot;
		availability_slot: import('$lib/schema').AvailabilitySlot;
		matchable_quantity: number;
	}>;
	total_matchable: number;
	unmatched_need_slots: import('$lib/schema').NeedSlot[];
	unmatched_availability_slots: import('$lib/schema').AvailabilitySlot[];
} {
	const compatible_pairs: Array<{
		need_slot: import('$lib/schema').NeedSlot;
		availability_slot: import('$lib/schema').AvailabilitySlot;
		matchable_quantity: number;
	}> = [];
	const matched_need_slot_ids = new Set<string>();
	const matched_availability_slot_ids = new Set<string>();

	let totalMatchable = 0;

	// Try to match each need slot to availability slots
	for (const needSlot of need.need_slots) {
		for (const availSlot of capacity.availability_slots) {
			// Check if slots are compatible (time/location)
			if (slotsCompatible(needSlot, availSlot)) {
				// Calculate how much we can allocate from this availability slot to this need slot
				// Limited by: need slot quantity, availability slot quantity, and maxAmount
				const matchableQty = Math.min(
					needSlot.quantity,
					availSlot.quantity,
					maxAmount - totalMatchable
				);

				if (matchableQty > 0) {
					compatible_pairs.push({
						need_slot: needSlot,
						availability_slot: availSlot,
						matchable_quantity: matchableQty
					});
					matched_need_slot_ids.add(needSlot.id);
					matched_availability_slot_ids.add(availSlot.id);
					totalMatchable += matchableQty;

					// If we've hit the maxAmount, stop
					if (totalMatchable >= maxAmount) {
						break;
					}
				}
			}
		}

		// If we've hit the maxAmount, stop
		if (totalMatchable >= maxAmount) {
			break;
		}
	}

	// Collect unmatched slots
	const unmatched_need_slots = need.need_slots.filter((s) => !matched_need_slot_ids.has(s.id));
	const unmatched_availability_slots = capacity.availability_slots.filter(
		(s) => !matched_availability_slot_ids.has(s.id)
	);

	return {
		compatible_pairs,
		total_matchable: totalMatchable,
		unmatched_need_slots,
		unmatched_availability_slots
	};
}

/**
 * Calculate how much of a member's need can be fulfilled by a capacity
 * considering slot-level time/location compatibility
 * 
 * @param need - Member's need (with slots)
 * @param capacity - Provider's capacity (with availability slots)
 * @param maxAmount - Maximum allocation (from recognition + filters)
 * @returns Amount that can actually be fulfilled given slot constraints
 */
export function calculateSlotCompatibleAmount(
	need: BaseNeed | undefined,
	capacity: import('$lib/schema').BaseCapacity,
	maxAmount: number
): number {
	if (!need) return 0;

	// Match slots and return total matchable amount
	const matching = matchNeedToCapacitySlots(need, capacity, maxAmount);
	return Math.min(matching.total_matchable, getRemainingNeed(need), maxAmount);
}

// === SLOT-TO-SLOT ALLOCATION ENGINE ===

/**
 * Allocate availability slots to need slots across multiple recipients
 * This is the core slot-based allocation engine that ensures:
 * - Each availability slot is not over-allocated
 * - Allocations respect time/location compatibility
 * - Recognition shares determine PROPORTIONAL allocation (not greedy first-come)
 * - Filters are respected
 * 
 * PROPORTIONAL ALLOCATION:
 * Each recipient gets allocated UP TO their recognition share × total capacity
 * This ensures fair distribution respecting collective priorities
 * 
 * @param capacity - Provider's capacity with availability slots
 * @param needs - Map of recipient ID to their needs
 * @param recognitionShares - Map of recipient ID to their recognition share (0-1)
 * @param filters - Map of recipient ID to their compliance filter
 * @returns Detailed slot-level allocation result
 */
export function allocateSlotsToRecipients(
	capacity: import('$lib/schema').BaseCapacity,
	needs: Map<string, BaseNeed>,
	recognitionShares: Map<string, number>,
	filters: Map<string, ComplianceFilter>
): {
	slot_allocations: SlotAllocation[];
	availability_slot_states: Map<string, AvailabilitySlotState>;
	need_slot_states: Map<string, NeedSlotState>;
	member_totals: Map<string, number>;
	member_targets: Map<string, number>;
	total_allocated: number;
	total_capacity: number;
} {
	// Calculate total capacity
	const totalCapacity = capacity.availability_slots.reduce((sum, s) => sum + s.quantity, 0);

	// Calculate target allocation for each recipient (proportional to recognition)
	const memberTargets = new Map<string, number>();
	for (const [recipientId, share] of recognitionShares.entries()) {
		const filter = filters.get(recipientId) || { type: 'unlimited' };
		const filterValue = getFilterValue(filter);
		
		// Target = recognition share × total capacity, limited by filter
		const target = Math.min(share * totalCapacity, filterValue);
		memberTargets.set(recipientId, target);
	}

	// Initialize availability slot states
	const availabilitySlotStates = new Map<string, AvailabilitySlotState>();
	for (const availSlot of capacity.availability_slots) {
		availabilitySlotStates.set(availSlot.id, {
			slot_id: availSlot.id,
			original_quantity: availSlot.quantity,
			remaining_quantity: availSlot.quantity,
			allocations: []
		});
	}

	// Initialize need slot states
	const needSlotStates = new Map<string, NeedSlotState>();
	for (const [recipientId, need] of needs.entries()) {
		for (const needSlot of need.need_slots) {
			needSlotStates.set(needSlot.id, {
				slot_id: needSlot.id,
				need_id: need.id,
				recipient_id: recipientId,
				original_quantity: needSlot.quantity,
				fulfilled_quantity: 0,
				allocations: []
			});
		}
	}

	// OPTIMIZATION: Bucket availability slots by time and location for faster matching
	// This reduces compatibility checks from O(N×A) to O(N×A_bucket) where A_bucket << A
	const timeBuckets = new Map<string, string[]>(); // "2024-06" → [avail_slot_ids]
	const locationBuckets = new Map<string, string[]>(); // "Berlin" | "Remote" → [avail_slot_ids]
	
	for (const availSlot of capacity.availability_slots) {
		// Time bucket (month-level for quick filtering)
		if (availSlot.start_date) {
			const bucket = availSlot.start_date.substring(0, 7); // "2024-06"
			if (!timeBuckets.has(bucket)) timeBuckets.set(bucket, []);
			timeBuckets.get(bucket)!.push(availSlot.id);
		}
		
		// Location bucket (city/country/remote)
		const locKey = availSlot.location_type?.toLowerCase().includes('remote') || availSlot.online_link
			? 'remote'
			: availSlot.city?.toLowerCase() || availSlot.country?.toLowerCase() || 'unknown';
		if (!locationBuckets.has(locKey)) locationBuckets.set(locKey, []);
		locationBuckets.get(locKey)!.push(availSlot.id);
	}

	// OPTIMIZATION: Pre-compute compatibility matrix with bucket filtering
	// Maps need_slot_id → array of compatible availability_slot_ids
	// This avoids redundant compatibility checks across passes and recipients
	const compatibilityMatrix = new Map<string, string[]>();
	
	for (const [recipientId, need] of needs.entries()) {
		for (const needSlot of need.need_slots) {
			const compatibleAvailSlots: string[] = [];
			
			// OPTIMIZATION: Get candidate slots from time/location buckets instead of all slots
			const candidateSlotIds = new Set<string>();
			
			// Add from time bucket
			if (needSlot.start_date) {
				const bucket = needSlot.start_date.substring(0, 7);
				const slotsInTimeBucket = timeBuckets.get(bucket) || [];
				slotsInTimeBucket.forEach(id => candidateSlotIds.add(id));
			} else {
				// No time constraint - consider all slots
				capacity.availability_slots.forEach(s => candidateSlotIds.add(s.id));
			}
			
			// Filter by location bucket (intersection)
			const needLocKey = needSlot.location_type?.toLowerCase().includes('remote') || needSlot.online_link
				? 'remote'
				: needSlot.city?.toLowerCase() || needSlot.country?.toLowerCase() || 'unknown';
			const slotsInLocBucket = new Set(locationBuckets.get(needLocKey) || []);
			
			// If remote, also include all slots (remote is compatible with everything)
			if (needLocKey === 'remote' || locationBuckets.has('remote')) {
				const remoteSlots = locationBuckets.get('remote') || [];
				remoteSlots.forEach(id => candidateSlotIds.add(id));
			}
			
			// Location intersection (only if we have location info)
			if (needSlot.city || needSlot.country) {
				candidateSlotIds.forEach(id => {
					if (!slotsInLocBucket.has(id) && !locationBuckets.get('remote')?.includes(id)) {
						candidateSlotIds.delete(id);
					}
				});
			}
			
			// Now check detailed compatibility only for candidate slots (much smaller set!)
			for (const availSlotId of candidateSlotIds) {
				const availSlot = capacity.availability_slots.find(s => s.id === availSlotId)!;
				
				// Check detailed time/location compatibility
				if (slotsCompatible(needSlot, availSlot)) {
					compatibleAvailSlots.push(availSlot.id);
				}
			}
			
			compatibilityMatrix.set(needSlot.id, compatibleAvailSlots);
		}
	}

	const slotAllocations: SlotAllocation[] = [];
	const memberTotals = new Map<string, number>();

	// OPTIMIZATION: Track active availability slots (those not fully allocated)
	// Similar to activeRecipients, this shrinks the search space
	const activeAvailSlots = new Set<string>(
		capacity.availability_slots.filter(s => s.quantity > 0).map(s => s.id)
	);

	// OPTIMIZATION: Pre-filter recipients who can't receive anything
	// - Blocked by filters (target = 0)
	// - No needs declared
	// - No compatible slots (all compatibility_matrix entries empty)
	const activeRecipients = new Set<string>();
	for (const [recipientId, target] of memberTargets.entries()) {
		if (target <= 0) continue; // Blocked or no recognition
		
		const need = needs.get(recipientId);
		if (!need || need.need_slots.length === 0) continue; // No needs
		
		// Check if at least one need slot has compatible availability slots
		const hasCompatibleSlots = need.need_slots.some(
			slot => (compatibilityMatrix.get(slot.id)?.length || 0) > 0
		);
		if (!hasCompatibleSlots) continue; // No compatible slots
		
		activeRecipients.add(recipientId);
	}

	// Early exit if no one can receive
	if (activeRecipients.size === 0) {
		return {
			slot_allocations: [],
			availability_slot_states: availabilitySlotStates,
			need_slot_states: needSlotStates,
			member_totals: memberTotals,
			member_targets: memberTargets,
			total_allocated: 0,
			total_capacity: totalCapacity
		};
	}

	// PROPORTIONAL ALLOCATION: Allocate to each recipient up to their target
	// Multiple passes to handle redistribution of unused capacity
	let hasUnallocatedCapacity = true;
	let passCount = 0;
	const maxPasses = 10; // Prevent infinite loops
	let totalRemainingCapacity = totalCapacity;

	while (hasUnallocatedCapacity && passCount < maxPasses && activeRecipients.size > 0 && totalRemainingCapacity > 0 && activeAvailSlots.size > 0) {
		passCount++;
		hasUnallocatedCapacity = false;
		const passStartRemaining = totalRemainingCapacity;

		// Iterate only over active recipients (those who might still receive)
		for (const recipientId of activeRecipients) {
			const need = needs.get(recipientId)!; // Already verified in pre-filter
			const target = memberTargets.get(recipientId)!;
			const currentTotal = memberTotals.get(recipientId) || 0;
			
			// Skip if already at target (and remove from active set)
			if (currentTotal >= target) {
				activeRecipients.delete(recipientId);
				continue;
			}

			const remainingTarget = target - currentTotal;
			let recipientPassTotal = 0;
			let hasAnyUnfulfilledNeed = false;

			// Try to fulfill need slots up to remaining target
			for (const needSlot of need.need_slots) {
				const needSlotState = needSlotStates.get(needSlot.id)!;
				const remainingNeed = needSlot.quantity - needSlotState.fulfilled_quantity;
				
				if (remainingNeed <= 0) continue;
				hasAnyUnfulfilledNeed = true; // This recipient still has unfulfilled needs

				// Get pre-computed compatible availability slots for this need slot
				const compatibleAvailSlotIds = compatibilityMatrix.get(needSlot.id) || [];
				
				// Iterate only over compatible AND active slots (double-filtered!)
				for (const availSlotId of compatibleAvailSlotIds) {
					// OPTIMIZATION: Skip exhausted slots (not in active set)
					if (!activeAvailSlots.has(availSlotId)) continue;
					
					const availSlotState = availabilitySlotStates.get(availSlotId)!;
					
					if (availSlotState.remaining_quantity <= 0) {
						// OPTIMIZATION: Remove from active set when exhausted
						activeAvailSlots.delete(availSlotId);
						continue;
					}

					// Calculate how much we can allocate
					const canAllocate = Math.min(
						availSlotState.remaining_quantity,  // What's left in availability slot
						remainingNeed,                       // What's still needed
						remainingTarget - recipientPassTotal // Stay within target
					);

					if (canAllocate <= 0) continue;

					// Get the actual availability slot for metadata
					const availSlot = capacity.availability_slots.find(s => s.id === availSlotId)!;

					// Create allocation (compatibility already verified in matrix)
					const allocation: SlotAllocation = {
						availability_slot_id: availSlotId,
						need_slot_id: needSlot.id,
						recipient_id: recipientId,
						quantity: canAllocate,
						time_compatible: true, // Already verified in compatibility matrix
						location_compatible: true // Already verified in compatibility matrix
					};

					// Update states
					availSlotState.remaining_quantity -= canAllocate;
					availSlotState.allocations.push(allocation);
					needSlotState.fulfilled_quantity += canAllocate;
					needSlotState.allocations.push(allocation);
					recipientPassTotal += canAllocate;
					totalRemainingCapacity -= canAllocate; // Track global remaining capacity

					slotAllocations.push(allocation);
					hasUnallocatedCapacity = true;

					// If reached target for this pass, move to next recipient
					if (recipientPassTotal >= remainingTarget) break;
				}

				if (recipientPassTotal >= remainingTarget) break;
			}

			memberTotals.set(recipientId, currentTotal + recipientPassTotal);
			
			// OPTIMIZATION: Remove from active set if all needs are fully satisfied
			// This shrinks the iteration space in subsequent passes
			if (!hasAnyUnfulfilledNeed) {
				activeRecipients.delete(recipientId);
			}
		}

		// OPTIMIZATION: Early exit if no progress made in this pass
		// If remaining capacity didn't decrease, no point in continuing
		if (totalRemainingCapacity >= passStartRemaining * 0.9999) {
			break; // Stuck, no more allocations possible
		}

		// OPTIMIZATION: Early exit if capacity exhausted
		if (totalRemainingCapacity <= 0.0001) {
			break; // All capacity allocated
		}

		// OPTIMIZATION: Early exit if all availability slots exhausted
		if (activeAvailSlots.size === 0) {
			break; // No slots left to allocate from
		}
	}

	const totalAllocated = Array.from(memberTotals.values()).reduce((sum, amt) => sum + amt, 0);

	return {
		slot_allocations: slotAllocations,
		availability_slot_states: availabilitySlotStates,
		need_slot_states: needSlotStates,
		member_totals: memberTotals,
		member_targets: memberTargets,
		total_allocated: totalAllocated,
		total_capacity: totalCapacity
	};
}

// === DYNAMIC MEMBERSHIP FOR CAPACITIES ===

/**
 * Update capacity membership if auto-update is enabled
 * 
 * This allows any capacity to become a "collective capacity" by dynamically
 * updating its member list based on MRD computation.
 * 
 * @param capacity - Capacity that may have dynamic membership
 * @param recognitionData - All recognition data for MRD computation
 * @returns Updated capacity with new members (if updated) or original capacity
 */
export function maybeUpdateCapacityMembers(
	capacity: BaseCapacity & { provider_id?: string },
	recognitionData: RecognitionData[]
): BaseCapacity & { provider_id?: string } {
	// Check if auto-update is enabled and if update is needed
	if (!shouldUpdateCapacityMembership(capacity)) {
		return capacity; // No update needed
	}

	const threshold = capacity.mrd_threshold ?? 0.5;
	const currentMembers = capacity.members || [];
	
	// Update membership based on MRD
	const result = updateCapacityMembership(
		currentMembers,
		recognitionData,
		threshold,
		{
			// Optional: Add any special rules here
			// preserveFoundingMembers: [...], // Could preserve original members
			// maxNewMembers: 10, // Could limit growth rate
		}
	);

	// Return updated capacity
	return {
		...capacity,
		members: result.members,
		last_membership_update: result.timestamp.toISOString()
	};
}

/**
 * Helper to convert Node-based recognition to RecognitionData format
 * This is needed when we have memberTrees but need RecognitionData for MRD computation
 */
export function extractRecognitionDataFromTrees(
	memberTrees: Map<string, Node>
): RecognitionData[] {
	const recognitionData: RecognitionData[] = [];
	
	// For each member tree, extract recognition relationships
	for (const [memberId, tree] of memberTrees.entries()) {
		// Walk tree to find all recognition relationships
		// This is a simplified version - you may need to adapt based on your tree structure
		if (tree.type === 'RootNode') {
			// Extract children as recognitions
			for (const child of tree.children) {
				if (child.type === 'NonRootNode') {
					// Calculate percentage from points
					const siblingPoints = tree.children.reduce((sum, c) => {
						return sum + (c.type === 'NonRootNode' ? c.points : 0);
					}, 0);
					const percentage = siblingPoints > 0 ? (child.points / siblingPoints) * 100 : 0;
					
					recognitionData.push({
						fromId: memberId,
						toId: child.id,
						percentage: percentage,
						timestamp: new Date()
					});
				}
			}
		}
	}
	
	return recognitionData;
}

// === COLLECTIVE RECOGNITION COMPUTATION ===

/**
 * Calculate collective recognition shares within a provider's declared set
 * 
 * Mathematical Formula:
 * Pool = Σ MutualRecognition(i, j) for all pairs in set
 * Member's Share = (Σ MutualRecognition(Member, Others)) / Pool
 * 
 * @param memberSet - Array of member IDs in provider's declared set
 * @param memberTrees - Map of member ID to their recognition tree
 * @returns Map of member ID to their collective-recognition-share (0-1)
 */
export function calculateCollectiveRecognitionShares(
	memberSet: string[],
	memberTrees: Map<string, Node>
): Map<string, number> {
	const shares = new Map<string, number>();

	// Convert map to object for mutualFulfillment function
	const nodesMap = Object.fromEntries(memberTrees);

	// Calculate total mutual recognition pool
	let totalPool = 0;
	const memberRecognitionSums = new Map<string, number>();

	// Calculate sum of mutual recognitions for each member
	for (const memberId of memberSet) {
		const memberTree = memberTrees.get(memberId);
		if (!memberTree) {
			shares.set(memberId, 0);
			continue;
		}

		let memberSum = 0;
		for (const otherId of memberSet) {
			if (otherId === memberId) continue;

			const otherTree = memberTrees.get(otherId);
			if (!otherTree) continue;

			const mutualRec = mutualFulfillment(memberTree, otherTree, nodesMap);
			memberSum += mutualRec;
		}

		memberRecognitionSums.set(memberId, memberSum);
		totalPool += memberSum;
	}

	// Normalize to shares
	if (totalPool === 0) {
		// Equal shares if no mutual recognition
		const equalShare = 1.0 / memberSet.length;
		for (const memberId of memberSet) {
			shares.set(memberId, equalShare);
		}
	} else {
		for (const memberId of memberSet) {
			const memberSum = memberRecognitionSums.get(memberId) || 0;
			shares.set(memberId, memberSum / totalPool);
		}
	}

	return shares;
}

// === ALLOCATION COMPUTATION ===

/**
 * Compute allocations from a provider's capacity declaration
 * 
 * Process:
 * 0. (Optional) Update capacity membership if auto_update_members_by_mrd is enabled
 * 1. Calculate collective-recognition-shares within set
 * 2. Perform slot-to-slot allocation (respecting time/location/quantity constraints)
 * 3. Apply compliance filters at slot level
 * 4. Track which availability slots fulfill which need slots
 * 5. Prevent over-allocation of availability slots
 * 6. Return final allocations with full slot-level transparency
 * 
 * @param capacity - Provider's capacity declaration (with availability_slots)
 * @param needs - Map of member ID to their need declaration (with need_slots)
 * @param memberTrees - Map of member ID to their recognition tree
 * @param recognitionData - (Optional) Recognition data for dynamic membership updates
 * @returns Allocation computation result with full slot-level breakdown
 */
export function computeAllocations(
	capacity: BaseCapacity & {
		provider_id?: string;
		filters?: Record<string, ComplianceFilter>;
	},
	needs: Map<string, BaseNeed>,
	memberTrees: Map<string, Node>,
	recognitionData?: RecognitionData[]
): AllocationComputationResult & {
	slot_allocations?: SlotAllocation[];
	availability_slot_states?: Map<string, AvailabilitySlotState>;
	need_slot_states?: Map<string, NeedSlotState>;
	members_updated?: boolean;
	members_added?: string[];
	members_removed?: string[];
} {
	const timestamp = new Date().toISOString();
	
	// STEP 0: Update capacity membership if auto-update is enabled
	let membersUpdated = false;
	let membersAdded: string[] = [];
	let membersRemoved: string[] = [];
	
	const currentMembers = capacity.members || [];
	
	if (capacity.auto_update_members_by_mrd && recognitionData && currentMembers.length > 0) {
		const originalMembers = [...currentMembers];
		
		// Update membership based on MRD
		const threshold = capacity.mrd_threshold ?? 0.5;
		const membershipResult = updateCapacityMembership(
			currentMembers,
			recognitionData,
			threshold
		);
		
		// Update capacity's members
		capacity.members = membershipResult.members;
		capacity.last_membership_update = membershipResult.timestamp.toISOString();
		
		membersUpdated = membershipResult.added.length > 0 || membershipResult.removed.length > 0;
		membersAdded = membershipResult.added;
		membersRemoved = membershipResult.removed;
	}

	// Use updated members list
	const members = capacity.members || [];
	
	// Step 1: Calculate collective-recognition-shares
	const recognitionShares = calculateCollectiveRecognitionShares(members, memberTrees);
	const totalPool = Array.from(recognitionShares.values()).reduce((sum, share) => sum + share, 0);

	// Step 2: Prepare filters map
	const filtersMap = new Map<string, ComplianceFilter>();
	const appliedFilters: Record<string, ComplianceFilter> = {};
	const blockedMembers: string[] = [];
	const cappedMembers: string[] = [];
	
	for (const memberId of members) {
		const filter = capacity.filters?.[memberId] || { type: 'unlimited' };
		filtersMap.set(memberId, filter);
		appliedFilters[memberId] = filter;
		
		if (filter.type === 'blocked') {
			blockedMembers.push(memberId);
		} else if (filter.type === 'capped') {
			cappedMembers.push(memberId);
		}
	}

	// Step 3: Perform slot-to-slot allocation
	const slotAllocationResult = allocateSlotsToRecipients(
		capacity,
		needs,
		recognitionShares,
		filtersMap
	);

	// Step 4: Extract results from slot-level allocation
	const finalAllocations: Record<string, number> = {};
	const idealAllocations: Record<string, number> = {};
	
	// Ideal allocations = targets (recognition share × capacity, limited by filter)
	for (const memberId of members) {
		idealAllocations[memberId] = slotAllocationResult.member_targets.get(memberId) || 0;
		finalAllocations[memberId] = slotAllocationResult.member_totals.get(memberId) || 0;
	}

	const totalCapacity = slotAllocationResult.total_capacity;
	const totalAllocated = slotAllocationResult.total_allocated;
	const unusedCapacity = totalCapacity - totalAllocated;

	return {
		capacity_id: capacity.id,
		provider_id: (capacity.provider_id || capacity.owner_id || 'unknown') as string,
		total_capacity: totalCapacity,
		member_set: members,

		collective_recognition_pool: totalPool,
		collective_recognition_shares: Object.fromEntries(recognitionShares),

		ideal_allocations: idealAllocations,

		applied_filters: appliedFilters,
		filtered_allocations: finalAllocations, // In slot-based system, final = filtered (no separate redistribution)
		blocked_members: blockedMembers,
		capped_members: cappedMembers,

		unallocated_amount: unusedCapacity,
		redistribution_pool: [], // Not applicable in slot-based allocation
		redistributed_amounts: {}, // Not applicable in slot-based allocation

		final_allocations: finalAllocations,
		unused_capacity: Math.max(0, unusedCapacity),

		computation_timestamp: timestamp,
		algorithm_version: 'collective_rec_v2_slot_based',

		// Slot-level detail
		slot_allocations: slotAllocationResult.slot_allocations,
		availability_slot_states: slotAllocationResult.availability_slot_states,
		need_slot_states: slotAllocationResult.need_slot_states,
		
		// Dynamic membership info (if updated)
		members_updated: membersUpdated,
		members_added: membersAdded,
		members_removed: membersRemoved
	};
}

/**
 * Generate allocation records from computation result
 * 
 * @param computationResult - Result from computeAllocations
 * @param capacityId - ID of capacity declaration
 * @param unit - Unit of allocation
 * @returns Array of Allocation records
 */
export function generateAllocations(
	computationResult: AllocationComputationResult,
	capacityId: string,
	unit: string
): Allocation[] {
	const allocations: Allocation[] = [];
	const timestamp = new Date().toISOString();

	for (const memberId of computationResult.member_set) {
		const finalAmount = computationResult.final_allocations[memberId] || 0;
		if (finalAmount === 0) continue;

		const idealAmount = computationResult.ideal_allocations[memberId] || 0;
		const redistributedAmount = computationResult.redistributed_amounts[memberId] || 0;

		allocations.push({
			id: `${capacityId}-${memberId}-${timestamp}`,
			provider_id: computationResult.provider_id,
			recipient_id: memberId,
			source_capacity_id: computationResult.capacity_id,
			amount: finalAmount,
			unit: unit,
			recipient_collective_recognition_share:
				computationResult.collective_recognition_shares[memberId] || 0,
			timestamp: timestamp,
			applied_filter: computationResult.applied_filters[memberId],
			ideal_allocation: idealAmount,
			was_redistributed: redistributedAmount > 0
		});
	}

	return allocations;
}

// === NEED MANAGEMENT ===

/**
 * Update need fulfillment status based on allocations
 * 
 * @param need - Need declaration to update
 * @param allocations - Allocations that fulfill this need
 * @returns Updated need declaration
 */
export function updateNeedFulfillment(
	need: BaseNeed,
	allocations: Allocation[]
): BaseNeed {
	const totalFulfilled = allocations.reduce((sum, alloc) => {
		return alloc.recipient_id === need.declarer_id ? sum + alloc.amount : sum;
	}, need.fulfilled_amount);

	let status: 'open' | 'partially-fulfilled' | 'fulfilled' = 'open';
	if (totalFulfilled >= calculateTotalNeedAmount(need)) {
		status = 'fulfilled';
	} else if (totalFulfilled > 0) {
		status = 'partially-fulfilled';
	}

	return {
		...need,
		fulfilled_amount: totalFulfilled,
		status: status
	};
}

// === MULTI-PROVIDER SCENARIOS ===

/**
 * Apply union of filters for external provider using entity as proxy
 * 
 * Example:
 * - External provider filter: $50K max (their risk limit)
 * - Entity filter: $30K max (jurisdiction limit)
 * - Effective filter: min($50K, $30K) = $30K
 * 
 * @param providerCapacity - External provider's capacity declaration
 * @param entityFilters - Entity's compliance filters
 * @returns Capacity with union of filters applied
 */
export function applyFilterUnion<T extends { members?: string[]; filters?: Record<string, ComplianceFilter> }>(
	providerCapacity: T,
	entityFilters: Map<string, ComplianceFilter>
): T {
	const unionFilters: Record<string, ComplianceFilter> = {};
	const members = providerCapacity.members || [];

	for (const memberId of members) {
		const providerFilter = providerCapacity.filters?.[memberId] || { type: 'unlimited' };
		const entityFilter = entityFilters.get(memberId) || { type: 'unlimited' };

		// Union = most restrictive
		unionFilters[memberId] = unionOfFilters(providerFilter, entityFilter);
	}

	return {
		...providerCapacity,
		filters: unionFilters
	};
}

// === UTILITY FUNCTIONS ===

/**
 * Explain allocation computation in human-readable format
 */
export function explainAllocation(
	result: AllocationComputationResult,
	memberId: string
): string {
	const share = result.collective_recognition_shares[memberId] || 0;
	const ideal = result.ideal_allocations[memberId] || 0;
	const filtered = result.filtered_allocations[memberId] || 0;
	const redistributed = result.redistributed_amounts[memberId] || 0;
	const final = result.final_allocations[memberId] || 0;
	const filter = result.applied_filters[memberId];

	let explanation = `\n=== ALLOCATION BREAKDOWN: ${memberId} ===\n\n`;
	explanation += `Collective Recognition Share: ${(share * 100).toFixed(2)}%\n`;
	explanation += `Total Capacity: ${result.total_capacity}\n\n`;

	explanation += `Step 1: Ideal Allocation\n`;
	explanation += `  ${(share * 100).toFixed(2)}% × ${result.total_capacity} = ${ideal.toFixed(2)}\n\n`;

	explanation += `Step 2: Filter Application\n`;
	if (filter.type === 'blocked') {
		explanation += `  Filter: BLOCKED ($0)\n`;
		explanation += `  Filtered Allocation: $0\n\n`;
	} else if (filter.type === 'capped') {
		explanation += `  Filter: CAPPED (max $${filter.value})\n`;
		explanation += `  Filtered Allocation: min(${ideal.toFixed(2)}, ${filter.value}) = ${filtered.toFixed(2)}\n\n`;
	} else {
		explanation += `  Filter: UNLIMITED\n`;
		explanation += `  Filtered Allocation: ${filtered.toFixed(2)}\n\n`;
	}

	if (redistributed > 0) {
		explanation += `Step 3: Redistribution\n`;
		explanation += `  Additional from redistribution: ${redistributed.toFixed(2)}\n\n`;
	}

	explanation += `FINAL ALLOCATION: ${final.toFixed(2)}\n`;

	return explanation;
}

/**
 * Get allocation statistics for transparency
 */
export function getAllocationStats(result: AllocationComputationResult): {
	total_allocated: number;
	total_redistributed: number;
	members_blocked: number;
	members_capped: number;
	members_unlimited: number;
	utilization_rate: number;
} {
	const totalAllocated = Object.values(result.final_allocations).reduce(
		(sum: number, amt: number) => sum + amt,
		0
	);
	const totalRedistributed = Object.values(result.redistributed_amounts).reduce(
		(sum: number, amt: number) => sum + amt,
		0
	);

	return {
		total_allocated: totalAllocated,
		total_redistributed: totalRedistributed,
		members_blocked: result.blocked_members.length,
		members_capped: result.capped_members.length,
		members_unlimited:
			result.member_set.length - result.blocked_members.length - result.capped_members.length,
		utilization_rate: result.total_capacity > 0 ? totalAllocated / result.total_capacity : 0
	};
}

