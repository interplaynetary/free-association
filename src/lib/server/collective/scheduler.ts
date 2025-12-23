/**
 * Collective Recognition & Membership Computation Scheduler
 * 
 * Runs scheduled computations on the server:
 * - Membership updates (MRD-based)
 * - Allocation computations (collective recognition)
 * - This module is server-only and should be initialized in hooks.server.ts
 */

import { MRDMembershipModule } from '$lib/protocol/stores/collective-membership.svelte';
import { calculateSlotBasedPriorityAllocation, type SlotAllocationRecord } from '$lib/protocol/allocation-local';
import type {
	AvailabilitySlot,
	NeedSlot,
	Commitment,
	Node
} from '$lib/protocol/schemas';
import { collectiveConfig, formatInterval } from './config';

// ═══════════════════════════════════════════════════════════════════
// TYPES
// ═══════════════════════════════════════════════════════════════════

export interface RecognitionData {
	fromId: string;
	toId: string;
	percentage: number;
	timestamp: Date;
}

export interface ComputationCallbacks {
	/**
	 * Fetch all recognition data from database/holster
	 * This should return the current recognition relationships between all participants
	 */
	fetchRecognitionData: () => Promise<RecognitionData[]>;

	/**
	 * Fetch all capacities that have auto_update_members_by_mrd enabled
	 * These capacities will have their membership recomputed based on MRD
	 */
	fetchAutoUpdateCapacities: () => Promise<AvailabilitySlot[]>;

	/**
	 * Save updated capacity members after MRD computation
	 */
	saveCapacityMembers: (
		capacityId: string,
		members: string[],
		added: string[],
		removed: string[],
		timestamp: Date
	) => Promise<void>;

	/**
	 * Fetch all capacities that need allocation computation
	 */
	fetchCapacitiesForAllocation: () => Promise<AvailabilitySlot[]>;

	/**
	 * Fetch all needs for allocation matching
	 */
	fetchNeeds: () => Promise<Map<string, NeedSlot>>;

	/**
	 * Fetch member recognition trees for allocation computation
	 */
	fetchMemberTrees: (memberIds: string[]) => Promise<Map<string, Node>>;

	/**
	 * Save computed allocations to database
	 */
	saveAllocations: (
		capacityId: string,
		allocations: any // AllocationComputationResult
	) => Promise<void>;

	/**
	 * Optional: Log computation results
	 */
	logComputation?: (event: string, data: any) => Promise<void>;
}

// ═══════════════════════════════════════════════════════════════════
// SCHEDULER STATE
// ═══════════════════════════════════════════════════════════════════

interface SchedulerState {
	membershipTimer: NodeJS.Timeout | null;
	allocationTimer: NodeJS.Timeout | null;
	isRunning: boolean;
	lastMembershipRun: Date | null;
	lastAllocationRun: Date | null;
	membershipRunCount: number;
	allocationRunCount: number;
	callbacks: ComputationCallbacks | null;
}

const state: SchedulerState = {
	membershipTimer: null,
	allocationTimer: null,
	isRunning: false,
	lastMembershipRun: null,
	lastAllocationRun: null,
	membershipRunCount: 0,
	allocationRunCount: 0,
	callbacks: null
};

// ═══════════════════════════════════════════════════════════════════
// COMPUTATION FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Run membership computation for all auto-update capacities
 */
async function runMembershipComputation(): Promise<void> {
	if (!state.callbacks) {
		console.error('[COLLECTIVE-SCHEDULER] ❌ No callbacks registered');
		return;
	}

	const startTime = Date.now();
	state.membershipRunCount++;

	try {
		if (collectiveConfig.verboseLogging) {
			console.log(`[COLLECTIVE-SCHEDULER] 🔄 Starting membership computation #${state.membershipRunCount}...`);
		}

		// Fetch recognition data
		const recognitionData = await state.callbacks.fetchRecognitionData();
		if (collectiveConfig.verboseLogging) {
			console.log(`[COLLECTIVE-SCHEDULER]   → Loaded ${recognitionData.length} recognition relationships`);
		}

		// Fetch capacities with auto-update enabled
		const capacities = await state.callbacks.fetchAutoUpdateCapacities();
		if (collectiveConfig.verboseLogging) {
			console.log(`[COLLECTIVE-SCHEDULER]   → Found ${capacities.length} auto-update capacities`);
		}

		if (capacities.length === 0) {
			console.log('[COLLECTIVE-SCHEDULER] ℹ️  No capacities with auto-update enabled');
			state.lastMembershipRun = new Date();
			return;
		}

		// Create MRD module
		const mrdModule = new MRDMembershipModule(
			collectiveConfig.mrdThreshold,
			collectiveConfig.minimumMutualRecognition
		);

		// Process each capacity
		let updatedCount = 0;
		let totalAdded = 0;
		let totalRemoved = 0;

		for (const capacity of capacities) {
			const currentMembers = capacity.members || [];

			// Compute new membership
			const result = mrdModule.computeMembership(recognitionData, currentMembers);

			// Check if membership changed
			const added = result.added;
			const removed = result.removed;

			if (added.length > 0 || removed.length > 0) {
				updatedCount++;
				totalAdded += added.length;
				totalRemoved += removed.length;

				// Save updated members
				await state.callbacks.saveCapacityMembers(
					capacity.id,
					result.members,
					added,
					removed,
					result.timestamp
				);

				if (collectiveConfig.verboseLogging) {
					console.log(`[COLLECTIVE-SCHEDULER]   ✓ Updated ${capacity.id}: +${added.length} -${removed.length}`);
				}
			}
		}

		state.lastMembershipRun = new Date();
		const duration = Date.now() - startTime;

		console.log(
			`[COLLECTIVE-SCHEDULER] ✅ Membership computation completed in ${duration}ms\n` +
			`  → Processed ${capacities.length} capacities\n` +
			`  → Updated ${updatedCount} capacities\n` +
			`  → Added ${totalAdded} members, removed ${totalRemoved} members`
		);

		// Optional: Log to database
		if (state.callbacks.logComputation) {
			await state.callbacks.logComputation('membership_computation', {
				run_number: state.membershipRunCount,
				timestamp: state.lastMembershipRun,
				duration_ms: duration,
				capacities_processed: capacities.length,
				capacities_updated: updatedCount,
				members_added: totalAdded,
				members_removed: totalRemoved
			});
		}

	} catch (error) {
		console.error('[COLLECTIVE-SCHEDULER] ❌ Membership computation failed:', error);
		throw error;
	}
}

/**
 * Run allocation computation for all capacities
 */
async function runAllocationComputation(): Promise<void> {
	if (!state.callbacks) {
		console.error('[COLLECTIVE-SCHEDULER] ❌ No callbacks registered');
		return;
	}

	const startTime = Date.now();
	state.allocationRunCount++;

	try {
		if (collectiveConfig.verboseLogging) {
			console.log(`[COLLECTIVE-SCHEDULER] 🔄 Starting allocation computation #${state.allocationRunCount}...`);
		}

		// Fetch capacities
		const capacities = await state.callbacks.fetchCapacitiesForAllocation();
		if (collectiveConfig.verboseLogging) {
			console.log(`[COLLECTIVE-SCHEDULER]   → Loaded ${capacities.length} capacities`);
		}

		if (capacities.length === 0) {
			console.log('[COLLECTIVE-SCHEDULER] ℹ️  No capacities to allocate');
			state.lastAllocationRun = new Date();
			return;
		}

		// Fetch needs
		const needs = await state.callbacks.fetchNeeds();
		if (collectiveConfig.verboseLogging) {
			console.log(`[COLLECTIVE-SCHEDULER]   → Loaded ${needs.size} needs`);
		}

		// Process each capacity
		let allocationsComputed = 0;
		let totalAllocated = 0;

		for (const capacity of capacities) {
			const members = capacity.members || [];
			if (members.length === 0) continue;

			// Fetch member trees
			const memberTrees = await state.callbacks.fetchMemberTrees(members);

			// Compute allocations (using local adapter)
			const result = computeAllocations(
				capacity,
				needs,
				memberTrees
			);

			// Save allocations
			await state.callbacks.saveAllocations(capacity.id, result);

			allocationsComputed++;
			totalAllocated += result.total_allocated || 0;

			if (collectiveConfig.verboseLogging) {
				console.log(
					`[COLLECTIVE-SCHEDULER]   ✓ ${capacity.id}: ` +
					`${result.total_allocated}/${result.total_capacity} allocated`
				);
			}
		}

		state.lastAllocationRun = new Date();
		const duration = Date.now() - startTime;

		console.log(
			`[COLLECTIVE-SCHEDULER] ✅ Allocation computation completed in ${duration}ms\n` +
			`  → Processed ${capacities.length} capacities\n` +
			`  → Computed ${allocationsComputed} allocations\n` +
			`  → Total allocated: ${totalAllocated.toFixed(2)}`
		);

		// Optional: Log to database
		if (state.callbacks.logComputation) {
			await state.callbacks.logComputation('allocation_computation', {
				run_number: state.allocationRunCount,
				timestamp: state.lastAllocationRun,
				duration_ms: duration,
				capacities_processed: capacities.length,
				allocations_computed: allocationsComputed,
				total_allocated: totalAllocated
			});
		}

	} catch (error) {
		console.error('[COLLECTIVE-SCHEDULER] ❌ Allocation computation failed:', error);
		throw error;
	}
}

// ═══════════════════════════════════════════════════════════════════
// SCHEDULER CONTROL
// ═══════════════════════════════════════════════════════════════════

/**
 * Start the scheduler with provided callbacks
 */
export function startScheduler(callbacks: ComputationCallbacks): void {
	if (state.isRunning) {
		console.warn('[COLLECTIVE-SCHEDULER] ⚠️  Scheduler already running');
		return;
	}

	state.callbacks = callbacks;
	state.isRunning = true;

	console.log(
		`[COLLECTIVE-SCHEDULER] 🚀 Starting scheduler...\n` +
		`  → Membership computation: every ${formatInterval(collectiveConfig.membershipComputationInterval)}\n` +
		`  → Allocation computation: every ${formatInterval(collectiveConfig.allocationComputationInterval)}\n` +
		`  → Startup delay: ${formatInterval(collectiveConfig.startupDelay)}\n` +
		`  → MRD threshold: ${collectiveConfig.mrdThreshold}\n` +
		`  → Auto-update membership: ${collectiveConfig.autoUpdateMembership}\n` +
		`  → Auto-compute allocations: ${collectiveConfig.autoComputeAllocations}`
	);

	// Schedule with startup delay
	if (collectiveConfig.autoUpdateMembership) {
		setTimeout(() => {
			// Run immediately on startup
			runMembershipComputation().catch(console.error);

			// Then schedule recurring
			state.membershipTimer = setInterval(() => {
				runMembershipComputation().catch(console.error);
			}, collectiveConfig.membershipComputationInterval);
		}, collectiveConfig.startupDelay);
	}

	if (collectiveConfig.autoComputeAllocations) {
		setTimeout(() => {
			// Run immediately on startup
			runAllocationComputation().catch(console.error);

			// Then schedule recurring
			state.allocationTimer = setInterval(() => {
				runAllocationComputation().catch(console.error);
			}, collectiveConfig.allocationComputationInterval);
		}, collectiveConfig.startupDelay);
	}
}

/**
 * Stop the scheduler and clear all timers
 */
export function stopScheduler(): void {
	if (!state.isRunning) {
		console.warn('[COLLECTIVE-SCHEDULER] ⚠️  Scheduler not running');
		return;
	}

	console.log('[COLLECTIVE-SCHEDULER] 🛑 Stopping scheduler...');

	if (state.membershipTimer) {
		clearInterval(state.membershipTimer);
		state.membershipTimer = null;
	}

	if (state.allocationTimer) {
		clearInterval(state.allocationTimer);
		state.allocationTimer = null;
	}

	state.isRunning = false;
	state.callbacks = null;

	console.log('[COLLECTIVE-SCHEDULER] ✅ Scheduler stopped');
}

/**
 * Get scheduler status
 */
export function getSchedulerStatus() {
	return {
		isRunning: state.isRunning,
		lastMembershipRun: state.lastMembershipRun,
		lastAllocationRun: state.lastAllocationRun,
		membershipRunCount: state.membershipRunCount,
		allocationRunCount: state.allocationRunCount,
		config: collectiveConfig
	};
}

/**
 * Manually trigger membership computation (ignores schedule)
 */
export async function triggerMembershipComputation(): Promise<void> {
	console.log('[COLLECTIVE-SCHEDULER] 🔧 Manual membership computation triggered');
	await runMembershipComputation();
}

/**
 * Manually trigger allocation computation (ignores schedule)
 */
export async function triggerAllocationComputation(): Promise<void> {
	console.log('[COLLECTIVE-SCHEDULER] 🔧 Manual allocation computation triggered');
	await runAllocationComputation();
}

// ═══════════════════════════════════════════════════════════════════
// ADAPTERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Adapter to call the slot-based allocation engine
 */
function computeAllocations(
	capacity: AvailabilitySlot,
	needs: Map<string, NeedSlot>,
	memberTrees: Map<string, Node>
) {
	const needSlots = Array.from(needs.values());

	// Construct mock commitments for owner lookup
	const allCommitments: Record<string, Commitment> = {};

	// 1. Provider Commitment (The Collective)
	// We use the capacity ID as the key or a fixed 'collective' ID if not available
	const providerId = capacity.members?.[0] || 'collective';

	allCommitments[providerId] = {
		capacity_slots: [capacity],
		need_slots: [],
		timestamp: Date.now(),
		itcStamp: null
	};

	// 2. Recipient Commitments
	// Iterate needs to find owners. 
	// PROBLEM: NeedSlots don't strictly have an owner ID in the schema.
	// We assume the needs map keys MIGHT be related to owner, OR NeedSlot.members has it.
	// If not, we map them to 'unknown' which means they won't get priority boost from recognition,
	// but might still get allocated if fully compatible and no priority check fails.

	for (const [key, slot] of needs.entries()) {
		// Heuristic: Try to find owner from members
		const owner = slot.members?.[0] || 'unknown_recipient';

		if (!allCommitments[owner]) {
			allCommitments[owner] = {
				capacity_slots: [],
				need_slots: [],
				timestamp: Date.now(),
				itcStamp: null
			};
		}

		const commitment = allCommitments[owner];
		if (commitment.need_slots) {
			commitment.need_slots.push(slot);
		}
	}

	// 3. Run Allocation
	const allocations = calculateSlotBasedPriorityAllocation(
		[capacity],
		needSlots,
		allCommitments,
		{
			enableRefinement: true,
			debug: false
		}
	);

	// 4. Summarize Results
	const total_allocated = allocations.reduce((sum, a) => sum + a.quantity, 0);

	return {
		allocations,
		total_allocated,
		total_capacity: capacity.quantity
	};
}
