/**
 * Free-Association Algorithm - Reactive Wrapper (Svelte Stores)
 * 
 * This is a THIN REACTIVE WRAPPER around the pure functions in allocation-ipf-distributed.ts
 * 
 * Architecture:
 * - allocation-ipf-distributed.ts: Pure functions (single source of truth for logic)
 * - allocation.svelte.ts: Reactive stores that call pure functions and manage side-effects
 */

import { derived, get, type Readable } from 'svelte/store';
import { meshUserPub } from '$lib/network/mesh.svelte';

// ✅ IMPORT DISTRIBUTED IPF ALGORITHM
import {
	updateProviderState,
	updateRecipientState,
	generateFlowProposals,
	type DistributedIPFState,
	type FlowProposal
} from '../solver';
import { getSlotPriority } from '../ipf-core';

// Import v5 schemas and stores
import type {
	Commitment,
	SlotAllocationRecord
} from '../schemas';

import {
	myCommitmentStore,
	networkCommitments,
	getAllCommitmentsRecord,
	networkNeedSlots,
	myCurrentNeeds,
	myDistributedIPFState,
	myAllocationStateStore,
	networkAllocations,
	myRecognitionTreeStore,
	enableCommitmentPublishing
} from './stores.svelte';



// ═══════════════════════════════════════════════════════════════════
// IDENTITY & RECOGNITION DERIVED STORES
// ═══════════════════════════════════════════════════════════════════

export const myPublicKey = meshUserPub;

// ═══════════════════════════════════════════════════════════════════
// DISTRIBUTED PROTOCOL LIFECYCLE
// ═══════════════════════════════════════════════════════════════════

/**
 * Enable Distributed Allocation Loops
 * 
 * Sets up the reactive subscriptions that drive the Distributed IPF protocol.
 * 
 * Responsibilities:
 * 1. Provider Loop: Watch my capacity + network needs -> Update Provider State (x_p)
 * 2. Recipient Loop: Watch my needs + network allocations -> Update Recipient State (y_r)
 * 3. Cache Sync: Watch network commitments -> Update cached remote scalings
 * 4. Output Sync: Sync calculated allocations and factors to myCommitmentStore
 * 
 * @returns Cleanup function to stop loops
 */
export function enableDistributedAllocation() {
	console.log('[ALLOCATION] 🚀 Starting Distributed IPF loops...');

	// 1. PROVIDER LOOP
	// React to: My Capacity, My Needs (for local view), Network Commitments (for y_r)
	// Updates: myDistributedIPFState (rowScalings)
	const unsubProvider = derived(
		[myCommitmentStore, networkNeedSlots, myDistributedIPFState],
		([$myCommitment, $networkNeedSlots, $state]) => {
			if (!$myCommitment?.capacity_slots || !$myCommitment.need_slots) return;

			const myPub = get(myPublicKey);

			// UNIFIED INPUT CONSTRUCTION (Self + Others)
			// Ensure my needs are included in the universe of potential recipients
			const myNeedsWithPub = ($myCommitment.need_slots || []).map(s => ({ ...s, pubkey: myPub }));

			// networkNeedSlots is an Array, not a Map (fixed bug from previous refactor)
			// Assuming networkNeedSlots ALREADY excludes me (typical specific-store pattern),
			// we merge them. If it included me, duplication is harmless if IDs unique, 
			// but better to be safe.
			const allNeedsFlat = [...myNeedsWithPub, ...$networkNeedSlots];

			const allCommitments = getAllCommitmentsRecord();

			// Run pure provider update logic
			const nextState = updateProviderState(
				$myCommitment.capacity_slots,
				$myCommitment.need_slots,
				allCommitments,
				$state
			);

			// Update state store if changed
			if (JSON.stringify(nextState.rowScalings) !== JSON.stringify($state.rowScalings)) {
				myDistributedIPFState.update(s => ({ ...s, rowScalings: nextState.rowScalings }));
			}
		}
	).subscribe(() => { });

	// 2. RECIPIENT LOOP
	// React to: My Needs, Network Allocations (incoming proposals), My Allocations (self-proposals)
	// Updates: myDistributedIPFState (colScalings)
	const unsubRecipient = derived(
		[myCurrentNeeds, networkAllocations, myDistributedIPFState, myCommitmentStore, myAllocationsAsProvider],
		([$needsMap, $networkAllocations, $state, $myCommitment, $mySelfAllocations]) => {
			if (!$myCommitment?.need_slots) return;

			const myPub = get(myPublicKey);

			// UNIFIED INPUT CONSTRUCTION (Self + Others)
			const incomingProposals: FlowProposal[] = [];

			// 1. Add proposals from Others (Network)
			$networkAllocations.forEach((allocs, providerPubkey) => {
				if (!allocs) return;
				allocs.forEach((a: SlotAllocationRecord) => {
					// Check if this allocation is for ME
					if (a.recipient_pubkey === myPub) {
						incomingProposals.push({
							capacity_slot_id: a.availability_slot_id,
							need_slot_id: a.recipient_need_slot_id || 'unknown',
							provider_pubkey: providerPubkey,
							recipient_pubkey: a.recipient_pubkey,
							proposed_quantity: a.quantity,
							seed_value: a.seed_value || 0
						});
					}
				});
			});

			// 2. Add proposals from Myself (Self-Loop)
			// myAllocationsAsProvider contains what I *intend* to give.
			// Ideally, this should come from myCommitmentStore.slot_allocations to be consistent with 
			// "what is published", but myAllocationsAsProvider is the reactive source of that.
			// Using the store ensures we react immediately to our own capacity changes.
			if ($mySelfAllocations.allocations) {
				$mySelfAllocations.allocations.forEach((a: SlotAllocationRecord) => {
					if (a.recipient_pubkey === myPub) {
						incomingProposals.push({
							capacity_slot_id: a.availability_slot_id,
							need_slot_id: a.recipient_need_slot_id || 'unknown',
							provider_pubkey: myPub, // I am providing
							recipient_pubkey: myPub, // I am receiving
							proposed_quantity: a.quantity,
							seed_value: a.seed_value || 0
						});
					}
				});
			}

			// Run pure recipient update logic
			const nextState = updateRecipientState(
				$myCommitment.need_slots,
				incomingProposals,
				$state
			);

			// Update state store if changed
			if (JSON.stringify(nextState.colScalings) !== JSON.stringify($state.colScalings)) {
				myDistributedIPFState.update(s => ({ ...s, colScalings: nextState.colScalings }));
			}
		}
	).subscribe(() => { });

	// 3. CACHE SYNC LOOP (Network Listener)
	// React to: Network Commitments
	// Updates: myDistributedIPFState (cachedRemoteScalings)
	// Note: The pure functions might check commitments directly, but caching in state is good practice
	// for clean separation.
	const unsubCache = networkCommitments.subscribe(commitments => {
		myDistributedIPFState.update(state => {
			let changed = false;
			const newCache = { ...state.cachedRemoteScalings };

			for (const [pubKey, wrapper] of Object.entries(commitments)) {
				const commit = wrapper.data;
				if (commit.constraint_scaling_factors) {
					for (const [slotId, factor] of Object.entries(commit.constraint_scaling_factors)) {
						if (newCache[slotId] !== (factor as number)) {
							newCache[slotId] = (factor as number);
							changed = true;
						}
					}
				}
			}

			return changed ? { ...state, cachedRemoteScalings: newCache } : state;
		});
	});

	return () => {
		console.log('[ALLOCATION] 🛑 Stopping Distributed IPF loops.');
		unsubProvider();
		unsubRecipient();
		unsubCache();
	};
}


// ═══════════════════════════════════════════════════════════════════
// ALLOCATIONS OUTPUT (Provider View)
// ═══════════════════════════════════════════════════════════════════

/**
 * My Allocations (Provider View)
 * 
 * Generates the actual flow proposals (allocations) based on CURRENT state.
 * These are what get displayed in UI and synced to `slot_allocations` in commitment.
 */
export const myAllocationsAsProvider = derived(
	[myCommitmentStore, networkNeedSlots, myDistributedIPFState],
	([$myCommitment, $networkNeedSlots, $state]) => {
		if (!$myCommitment?.capacity_slots) return { allocations: [], totalsByTypeAndRecipient: {}, convergence: null, slotDenominators: {} };

		const myPub = get(myPublicKey);

		// UNIFIED INPUT CONSTRUCTION (Self + Others)
		// Fix 1: Add my own needs so I can allocate to myself
		const myNeedsWithPub = ($myCommitment.need_slots || []).map(s => ({ ...s, pubkey: myPub }));

		// Fix 2: Treat networkNeedSlots as Array (it is derived as [] in stores.svelte)
		const allNeedsFlat = [...myNeedsWithPub, ...$networkNeedSlots];

		const allCommitments = getAllCommitmentsRecord();

		// Generate Proposals
		const proposals = generateFlowProposals(
			$myCommitment.capacity_slots,
			allNeedsFlat,
			allCommitments,
			$state
		);

		// Map to SlotAllocationRecord for UI/Schema compatibility
		const allocations: SlotAllocationRecord[] = proposals.map(p => ({
			availability_slot_id: p.capacity_slot_id,
			recipient_need_slot_id: p.need_slot_id,
			quantity: p.proposed_quantity,
			recipient_pubkey: p.recipient_pubkey,
			provider_pubkey: myPub || '', // I am the provider

			// Metadata
			type_id: 'unknown', // Need to lookup type from capacity slot
			time_compatible: true,
			location_compatible: true,
			withinPriorityLimit: (() => {
				const capSlot = $myCommitment.capacity_slots?.find(s => s.id === p.capacity_slot_id);
				const providerCommit = allCommitments[myPub || ''];
				const priority = capSlot ? getSlotPriority(capSlot, p.recipient_pubkey, providerCommit) : 0;
				return priority > 0;
			})(),
			fromSurplus: false // Updated below
		}));

		// Fill in missing metadata (e.g. type_id) and surplus
		allocations.forEach(a => {
			const slot = $myCommitment.capacity_slots?.find(s => s.id === a.availability_slot_id);
			if (slot) a.type_id = slot.type_id || 'unknown';
			a.fromSurplus = !a.withinPriorityLimit;
		});

		// Compute Aggregates for UI convenience
		const totals: Record<string, Record<string, number>> = {};
		allocations.forEach(a => {
			const typeId = a.type_id || 'unknown';
			if (!totals[typeId]) totals[typeId] = {};
			const current = totals[typeId][a.recipient_pubkey] || 0;
			totals[typeId][a.recipient_pubkey] = current + a.quantity;
		});

		return {
			allocations,
			totalsByTypeAndRecipient: totals,
			convergence: null,
			slotDenominators: {}
		};
	}
);

// ═══════════════════════════════════════════════════════════════════
// AUTO-PUBLISH ALLOCATIONS TO NETWORK
// ═══════════════════════════════════════════════════════════════════

/**
 * Auto-update commitment with computed allocations
 * 
 * Watches myAllocationsAsProvider and publishes slot_allocations to commitment
 * This enables recipients to see incoming allocations for transparency
 * 
 * WHY THIS MATTERS:
 * - Recipients can see who's allocating to their needs
 * - Audit trail for allocation flows
 * - Debugging and trust building
 * - Complete transparency in the network
 */
export function enableAutoAllocationPublishing(): () => void {
	console.log('[AUTO-PUBLISH-ALLOC] 🚀 Enabling automatic allocation publishing');

	let debounceTimer: ReturnType<typeof setTimeout> | null = null;
	let isPublishing = false; // Prevent cascading updates
	let lastPublishedHash: string | null = null; // Track what we last published to prevent re-publishing same data

	const unsubAllocations = myAllocationsAsProvider.subscribe((allocResult) => {
		// Debounce rapid changes
		if (debounceTimer) {
			clearTimeout(debounceTimer);
		}

		debounceTimer = setTimeout(() => {
			// Check INSIDE the callback to prevent multiple queued callbacks from running
			if (isPublishing) {
				console.log('[AUTO-PUBLISH-ALLOC] ⏭️  Skipped: already publishing');
				isPublishing = false; // Reset flag in case of previous error
				return;
			}

			isPublishing = true;

			const currentState = get(myAllocationStateStore);
			if (!currentState) {
				console.log('[AUTO-PUBLISH-ALLOC] ⏭️  Skipped: no allocation state available');
				isPublishing = false;
				return;
			}

			// Check if allocations actually changed using hash
			const newAllocs = allocResult.allocations;
			const newAllocsHash = JSON.stringify(newAllocs);

			// Fast check: Same as what we last published?
			if (lastPublishedHash === newAllocsHash) {
				console.log('[AUTO-PUBLISH-ALLOC] ⏭️  Skipped: already published this exact allocation set');
				isPublishing = false;
				return;
			}

			// Slower check: Same as what's in the store?
			const currentAllocs = currentState.slot_allocations || [];
			try {
				const currentJson = JSON.stringify(currentAllocs);

				if (currentJson === newAllocsHash) {
					console.log('[AUTO-PUBLISH-ALLOC] ⏭️  Skipped: allocations unchanged in store');
					// Update our hash to match current state
					lastPublishedHash = newAllocsHash;
					isPublishing = false;
					return;
				}
			} catch (error) {
				console.warn('[AUTO-PUBLISH-ALLOC] ⚠️  Equality check failed, proceeding with update:', error);
			}

			// Update commitment with new allocations (Clean, no unnecessary casting)
			myAllocationStateStore.update(state => {
				const current = state || {
					slot_allocations: [],
					total_allocated: {},
					distance_from_need: {},
					constraint_scaling_factors: {},
					total_seed_by_need: {},
					multi_dimensional_damping: {}
				};
				return {
					...current,
					slot_allocations: newAllocs
				};
			});
			lastPublishedHash = newAllocsHash; // Update hash after successful publish
			isPublishing = false;
		}, 100); // 100ms debounce
	});

	return () => {
		if (debounceTimer) clearTimeout(debounceTimer);
		unsubAllocations();
		console.log('[AUTO-PUBLISH-ALLOC] ⏸️  Disabled automatic allocation publishing');
	};
}

// ═══════════════════════════════════════════════════════════════════
// INITIALIZATION (V5)
// ═══════════════════════════════════════════════════════════════════

let stopAllocationLoops: (() => void) | null = null;

/**
 * Start Allocation Service (V6)
 * 
 * Orchestrates the entire allocation engine:
 * 1. Distributed IPF Loops (Provider/Recipient/Cache)
 * 2. Auto-Composition (Tree -> Weights)
 * 3. Auto-Publishing (Allocations -> Commitment)
 */
export function startAllocationService(): () => void {
	console.log('[ALLOCATION] 🚀 Starting Allocation Service...');

	// Helper to track cleanups
	const undoLoops = enableDistributedAllocation();
	const undoPublishing = enableAutoAllocationPublishing();

	// Store globally for stopAllocationService if needed
	stopAllocationLoops = () => {
		undoLoops();
		undoPublishing();
	};

	return () => {
		console.log('[ALLOCATION] 🛑 Stopping Allocation Service');
		if (stopAllocationLoops) stopAllocationLoops();
		stopAllocationLoops = null;
	};
}

/**
 * Stop Allocation Service
 */
export function stopAllocationService() {
	if (stopAllocationLoops) {
		stopAllocationLoops();
		stopAllocationLoops = null;
	}
}

/**
 * Initialize all allocation stores (V5)
 * @deprecated Use startAllocationService() in startup.ts
 */
export async function initializeAllocationStores() {
	console.warn('[DEPRECATED] initializeAllocationStores() called. This is handled by startAllocationService().');
}

/**
 * Cleanup all allocation stores (V5)
 * @deprecated Use the callback from startAllocationService()
 */
export async function cleanupAllocationStores() {
	console.warn('[DEPRECATED] cleanupAllocationStores() called. This is handled by stopAllocationService().');
}