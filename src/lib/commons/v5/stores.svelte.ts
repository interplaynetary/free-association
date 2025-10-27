/**
 * Holster Integration for Mutual-Priority Allocation Algorithm v5
 * 
 * V5 Architecture - Pure Global Recognition Model:
 * ✅ Event-driven (no rounds)
 * ✅ ITC causality (not vector clocks)
 * ✅ Time-based damping (not round-indexed)
 * ✅ Reactive stores (auto-recompute)
 * ✅ Global MR: Same MR value for all types (tree encodes type preferences)
 * 
 * Provides P2P synchronized stores for:
 * - Commitments (needs, capacity, global recognition, damping, ITC stamps)
 * - Allocation States (denominators, allocations, convergence flags)
 * - Global Recognition Weights (normalized, used for MR computation)
 * 
 * KEY V5 CHANGE:
 * - Recognition is global (not type-specific)
 * - Type preferences encoded in recognition tree structure (protocol.ts)
 * - Network weights automatically normalized on receipt
 * 
 * Features:
 * - Schema-validated data
 * - Automatic persistence
 * - Cross-user subscriptions (for mutual contributors)
 * - Conflict resolution (timestamp-based)
 */

import { get, derived, readable, writable } from 'svelte/store';
import type { Readable, Writable } from 'svelte/store';
import { createStore } from '../utils/store.svelte';
import {
	CommitmentSchema,
	normalizeGlobalRecognitionWeights,
	type Commitment
} from './schemas';
import * as z from 'zod';
import { holsterUserPub } from '$lib/state/holster.svelte';
import { getTimeBucketKey, getLocationBucketKey } from './match.svelte';

export { holsterUserPub }

// ═══════════════════════════════════════════════════════════════════
// MY DATA STORES (V5)
// ═══════════════════════════════════════════════════════════════════

/**
 * My Commitment Store (V5)
 * 
 * This is the ONLY store needed for publishing my state!
 * 
 * Contains EVERYTHING:
 * - Capacity slots (if provider)
 * - Need slots (if recipient)
 * - Global recognition weights (who I recognize)
 * - Global MR values (mutual recognition with others)
 * - Adaptive damping state (time-based history)
 * - ITC stamp (causality tracking)
 * 
 * Design insight: Commitments capture both INPUT (my needs/capacity) 
 * and OUTPUT (state after allocation) - no separate allocation store needed!
 */
export const myCommitmentStore = createStore({
	holsterPath: 'allocation/commitment',
	schema: CommitmentSchema,
	persistDebounce: 100 // Debounce rapid updates
});

// V5: NO ROUND STATE STORE (event-driven, no rounds!)
// V5: NO ALLOCATION STATE STORE (commitments capture allocation results!)
// V5: NO SEPARATE RECOGNITION STORE (recognition in commitment!)

// ═══════════════════════════════════════════════════════════════════
// INITIALIZATION (V5)
// ═══════════════════════════════════════════════════════════════════

/**
 * Initialize all allocation stores (V5)
 * Call this after holster authentication
 */
export function initializeAllocationStores() {
	console.log('[ALLOCATION-HOLSTER-V5] Initializing commitment store...');
	
	myCommitmentStore.initialize();
	
	console.log('[ALLOCATION-HOLSTER-V5] Store initialized (single commitment store, global MR)');
}

/**
 * Cleanup all allocation stores (V5)
 * Call this before logout
 */
export async function cleanupAllocationStores() {
	console.log('[ALLOCATION-HOLSTER-V5] Cleaning up commitment store...');
	
	await myCommitmentStore.cleanup();
	
	console.log('[ALLOCATION-HOLSTER-V5] Store cleaned up');
}

// ═══════════════════════════════════════════════════════════════════
// NETWORK DATA STORES (OTHER PARTICIPANTS) - V5
// ═══════════════════════════════════════════════════════════════════

/**
 * Network Commitments (V5)
 * 
 * This is the ONLY network store needed!
 * 
 * Maps pubKey → Commitment, containing:
 * - Their needs + capacity
 * - Their recognition weights (who they recognize)
 * - Their MR values (mutual recognition)
 * - Their damping state
 * - Their ITC stamp
 * 
 * V5 PERFORMANCE: Reactive store for incremental index updates!
 * 
 * Design insight: Since commitments contain recognition, no separate 
 * recognition store needed. Since commitments capture allocation results
 * (updated needs/capacity), no separate allocation store needed!
 */
export const networkCommitments = writable<Map<string, Commitment>>(new Map());

// V5: NO NETWORK ROUND STATES (no rounds!)
// V5: NO NETWORK ALLOCATION STATES (commitments capture results!)
// V5: NO NETWORK RECOGNITION WEIGHTS (recognition in commitments!)

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION MANAGEMENT (V5)
// ═══════════════════════════════════════════════════════════════════

const activeSubscriptions = new Set<string>();

/**
 * Subscribe to a participant's commitment (V5)
 * 
 * Use for:
 * - Beneficiaries (people I allocate to) - need their need slots
 * - Providers (people who allocate to me) - need their capacity slots
 * 
 * V5: Automatically triggers reactive allocation computation
 * PERFORMANCE: Uses incremental index updates (O(M) per change)
 * 
 * CRITICAL: Normalizes their global_recognition_weights on receipt!
 * This ensures MR computation uses properly normalized distributions.
 */
export function subscribeToCommitment(pubKey: string) {
	if (activeSubscriptions.has(`${pubKey}:commitment`)) return;
	
	myCommitmentStore.subscribeToUser(pubKey, (commitment) => {
		// Update the reactive store (triggers incremental index update)
		networkCommitments.update((commitMap) => {
			const newMap = new Map(commitMap);
			
			if (commitment) {
				// CRITICAL: Normalize their recognition weights before storing
				// This ensures that when we compute MR, their recognition of us is a proper fraction
				// Example: If they recognize 10 people with values [10, 10, ...] (sum=100)
				//          and we're one of them (value=10), our share should be 10/100=0.1, not 10!
				let normalizedCommitment = commitment;
				if (commitment.global_recognition_weights) {
					normalizedCommitment = {
						...commitment,
						global_recognition_weights: normalizeGlobalRecognitionWeights(
							commitment.global_recognition_weights
						)
					};
				}
				
				newMap.set(pubKey, normalizedCommitment);
				console.log(`[ALLOCATION-HOLSTER-V5] Received commitment from ${pubKey.slice(0, 20)}... (normalized recognition, triggers incremental update)`);
		} else {
				newMap.delete(pubKey);
				console.log(`[ALLOCATION-HOLSTER-V5] Removed commitment from ${pubKey.slice(0, 20)}...`);
		}
			
			return newMap;
		});
	});
	
	activeSubscriptions.add(`${pubKey}:commitment`);
	console.log(`[ALLOCATION-HOLSTER-V5] Subscribed to ${pubKey.slice(0, 20)}... commitment`);
}

/**
 * Subscribe to full participant data (V5)
 * 
 * In V5, there's only ONE thing to subscribe to: their commitment!
 * 
 * Commitment contains:
 * - Their needs + capacity
 * - Their recognition (who they recognize, global_recognition_weights)
 * - Their MR values (mutual_recognition_values)
 * - Their damping state
 * 
 * Use for:
 * - Mutual contributors (full data exchange)
 * 
 * V5: Simplified - just commitment subscription
 */
export function subscribeToFullParticipant(pubKey: string) {
	subscribeToCommitment(pubKey);
	
	console.log(`[ALLOCATION-HOLSTER-V5] Subscribed to ${pubKey.slice(0, 20)}... (commitment contains everything)`);
}

/**
 * Unsubscribe from a participant's data (V5)
 * 
 * Note: Holster doesn't provide explicit unsubscribe,
 * so we just remove from our tracking and store
 */
export function unsubscribeFromParticipant(pubKey: string) {
	activeSubscriptions.delete(`${pubKey}:commitment`);
	
	// Update reactive store (triggers incremental index update)
	networkCommitments.update((commitMap) => {
		const newMap = new Map(commitMap);
		newMap.delete(pubKey);
		return newMap;
	});
	
	console.log(`[ALLOCATION-HOLSTER-V5] Unsubscribed from ${pubKey.slice(0, 20)}...`);
}

/**
 * Get list of all subscribed participants (V5)
 */
export function getSubscribedParticipants(): string[] {
	const pubKeys = new Set<string>();
	
	for (const key of activeSubscriptions) {
		const pubKey = key.split(':')[0];
		pubKeys.add(pubKey);
	}
	
	return Array.from(pubKeys);
}

// ═══════════════════════════════════════════════════════════════════
// UTILITY FUNCTIONS (V5)
// ═══════════════════════════════════════════════════════════════════

/**
 * Get all commitments as a Record (for algorithm compatibility)
 */
export function getNetworkCommitmentsRecord(): Record<string, Commitment> {
	const record: Record<string, Commitment> = {};
	const commitMap = get(networkCommitments); // Now a store!
	for (const [pubKey, commitment] of commitMap.entries()) {
		record[pubKey] = commitment;
	}
	return record;
}

/**
 * Get all commitments INCLUDING our own (V5)
 * This is what allocation algorithms should use for self-allocation support
 */
export function getAllCommitmentsRecord(): Record<string, Commitment> {
	const record = getNetworkCommitmentsRecord();
	
	// Include our own commitment if available
	const myCommitment = get(myCommitmentStore);
	const myPub = get(holsterUserPub);
	
	if (myCommitment && myPub) {
		record[myPub] = myCommitment;
	}
	
	return record;
}

/**
 * Get all recognition weights from network commitments
 * Extracts global_recognition_weights from each commitment
 */
export function getNetworkRecognitionWeightsRecord(): Record<string, Record<string, number>> {
	const record: Record<string, Record<string, number>> = {};
	const commitMap = get(networkCommitments);
	
	for (const [pubKey, commitment] of commitMap.entries()) {
		if (commitment.global_recognition_weights) {
			record[pubKey] = commitment.global_recognition_weights;
		}
	}
	
	return record;
}

// ═══════════════════════════════════════════════════════════════════
// SPATIAL/TEMPORAL INDEXES (Performance Optimization)
// ═══════════════════════════════════════════════════════════════════

/**
 * Spatial/Temporal Index Structure
 * Maps bucket keys to sets of pubKeys who have needs/capacity in that bucket
 */
export interface SpaceTimeIndex {
	/** Type-based index: need_type_id -> Set<pubKey> */
	byType: Map<string, Set<string>>;
	
	/** Location-based index: location_bucket -> Set<pubKey> */
	byLocation: Map<string, Set<string>>;
	
	/** Time-based index: time_bucket -> Set<pubKey> */
	byTime: Map<string, Set<string>>;
	
	/** Composite index: "type|location" -> Set<pubKey> */
	byTypeAndLocation: Map<string, Set<string>>;
	
	/** Composite index: "type|time" -> Set<pubKey> */
	byTypeAndTime: Map<string, Set<string>>;
	
	/** Full composite: "type|location|time" -> Set<pubKey> */
	byAll: Map<string, Set<string>>;
}

/**
 * INCREMENTAL INDEX UPDATE FUNCTIONS
 * O(M) complexity instead of O(N × M) for full rebuild
 */

/**
 * Remove a participant's slots from an index
 * O(M) where M = slots for this participant
 * 
 * MEMORY LEAK FIX: Also removes empty Sets to prevent memory buildup
 * After 1000 participants come and go, we don't want 1000 empty Sets!
 */
function removeFromIndex(pubKey: string, index: SpaceTimeIndex): void {
	// Remove from all index maps and clean up empty entries
	for (const [key, pubKeySet] of index.byType.entries()) {
		pubKeySet.delete(pubKey);
		if (pubKeySet.size === 0) {
			index.byType.delete(key); // Clean up empty Set
		}
	}
	
	for (const [key, pubKeySet] of index.byLocation.entries()) {
		pubKeySet.delete(pubKey);
		if (pubKeySet.size === 0) {
			index.byLocation.delete(key);
		}
	}
	
	for (const [key, pubKeySet] of index.byTime.entries()) {
		pubKeySet.delete(pubKey);
		if (pubKeySet.size === 0) {
			index.byTime.delete(key);
		}
	}
	
	for (const [key, pubKeySet] of index.byTypeAndLocation.entries()) {
		pubKeySet.delete(pubKey);
		if (pubKeySet.size === 0) {
			index.byTypeAndLocation.delete(key);
		}
	}
	
	for (const [key, pubKeySet] of index.byTypeAndTime.entries()) {
		pubKeySet.delete(pubKey);
		if (pubKeySet.size === 0) {
			index.byTypeAndTime.delete(key);
		}
	}
	
	for (const [key, pubKeySet] of index.byAll.entries()) {
		pubKeySet.delete(pubKey);
		if (pubKeySet.size === 0) {
			index.byAll.delete(key);
		}
	}
}

/**
 * Add a participant's need slots to the index
 * O(M) where M = slots for this participant
 */
function addNeedSlotsToIndex(pubKey: string, commitment: Commitment, index: SpaceTimeIndex): void {
	if (!commitment.need_slots) return;
	
	for (const needSlot of commitment.need_slots) {
		const typeId = needSlot.need_type_id;
		const locationKey = getLocationBucketKey(needSlot);
		const timeKey = getTimeBucketKey(needSlot);
		
		// Type index
		if (!index.byType.has(typeId)) {
			index.byType.set(typeId, new Set());
		}
		index.byType.get(typeId)!.add(pubKey);
		
		// Location index
		if (!index.byLocation.has(locationKey)) {
			index.byLocation.set(locationKey, new Set());
		}
		index.byLocation.get(locationKey)!.add(pubKey);
		
		// Time index
		if (!index.byTime.has(timeKey)) {
			index.byTime.set(timeKey, new Set());
		}
		index.byTime.get(timeKey)!.add(pubKey);
		
		// Composite: type + location
		const typeLocKey = `${typeId}|${locationKey}`;
		if (!index.byTypeAndLocation.has(typeLocKey)) {
			index.byTypeAndLocation.set(typeLocKey, new Set());
		}
		index.byTypeAndLocation.get(typeLocKey)!.add(pubKey);
		
		// Composite: type + time
		const typeTimeKey = `${typeId}|${timeKey}`;
		if (!index.byTypeAndTime.has(typeTimeKey)) {
			index.byTypeAndTime.set(typeTimeKey, new Set());
		}
		index.byTypeAndTime.get(typeTimeKey)!.add(pubKey);
		
		// Full composite: type + location + time
		const fullKey = `${typeId}|${locationKey}|${timeKey}`;
		if (!index.byAll.has(fullKey)) {
			index.byAll.set(fullKey, new Set());
		}
		index.byAll.get(fullKey)!.add(pubKey);
	}
}

/**
 * Add a participant's capacity slots to the index
 * O(M) where M = slots for this participant
 */
function addCapacitySlotsToIndex(pubKey: string, commitment: Commitment, index: SpaceTimeIndex): void {
	if (!commitment.capacity_slots) return;
	
	for (const capacitySlot of commitment.capacity_slots) {
		const typeId = capacitySlot.need_type_id;
		const locationKey = getLocationBucketKey(capacitySlot);
		const timeKey = getTimeBucketKey(capacitySlot);
		
		// Type index
		if (!index.byType.has(typeId)) {
			index.byType.set(typeId, new Set());
		}
		index.byType.get(typeId)!.add(pubKey);
		
		// Location index
		if (!index.byLocation.has(locationKey)) {
			index.byLocation.set(locationKey, new Set());
		}
		index.byLocation.get(locationKey)!.add(pubKey);
		
		// Time index
		if (!index.byTime.has(timeKey)) {
			index.byTime.set(timeKey, new Set());
		}
		index.byTime.get(timeKey)!.add(pubKey);
		
		// Composite: type + location
		const typeLocKey = `${typeId}|${locationKey}`;
		if (!index.byTypeAndLocation.has(typeLocKey)) {
			index.byTypeAndLocation.set(typeLocKey, new Set());
		}
		index.byTypeAndLocation.get(typeLocKey)!.add(pubKey);
		
		// Composite: type + time
		const typeTimeKey = `${typeId}|${timeKey}`;
		if (!index.byTypeAndTime.has(typeTimeKey)) {
			index.byTypeAndTime.set(typeTimeKey, new Set());
		}
		index.byTypeAndTime.get(typeTimeKey)!.add(pubKey);
		
		// Full composite: type + location + time
		const fullKey = `${typeId}|${locationKey}|${timeKey}`;
		if (!index.byAll.has(fullKey)) {
			index.byAll.set(fullKey, new Set());
		}
		index.byAll.get(fullKey)!.add(pubKey);
	}
}

/**
 * Incrementally update index for a single participant
 * O(M) instead of O(N × M) - N times faster!
 * 
 * @param pubKey - Participant whose commitment changed
 * @param commitment - New commitment (or undefined to remove)
 * @param index - Index to update
 * @param isNeedIndex - true for need slots, false for capacity slots
 */
function updateIndexForParticipant(
	pubKey: string,
	commitment: Commitment | undefined,
	index: SpaceTimeIndex,
	isNeedIndex: boolean
): void {
	// Step 1: Remove old entries for this pubKey - O(M_old)
	removeFromIndex(pubKey, index);
	
	// Step 2: Add new entries if commitment exists - O(M_new)
	if (commitment) {
		if (isNeedIndex) {
			addNeedSlotsToIndex(pubKey, commitment, index);
		} else {
			addCapacitySlotsToIndex(pubKey, commitment, index);
		}
	}
}

/**
 * Reactive index of network needs (for capacity providers to find recipients)
 * 
 * SVELTE-NATIVE REACTIVITY: No manual debouncing needed!
 * - Updates immediately when commitments change (O(M) per participant)
 * - Svelte automatically batches updates to next microtask
 * - Incremental updates prevent O(N×M) full rebuilds
 * 
 * Enables O(1) lookup instead of O(N) scan
 */
export const networkNeedsIndex: Readable<SpaceTimeIndex> = readable<SpaceTimeIndex>(
	{
		byType: new Map(),
		byLocation: new Map(),
		byTime: new Map(),
		byTypeAndLocation: new Map(),
		byTypeAndTime: new Map(),
		byAll: new Map()
	},
	(set) => {
		// Mutable index (we update it incrementally)
		const index: SpaceTimeIndex = {
			byType: new Map(),
			byLocation: new Map(),
			byTime: new Map(),
			byTypeAndLocation: new Map(),
			byTypeAndTime: new Map(),
			byAll: new Map()
		};
		
		// Track which participants have pending updates (batch within same tick)
		let pendingUpdates = new Map<string, Commitment | undefined>();
		let isUpdateScheduled = false;
		
		// Process all pending updates (called via queueMicrotask)
		const processPendingUpdates = () => {
			if (pendingUpdates.size === 0) {
				isUpdateScheduled = false;
				return;
			}
			
			// Process all pending updates
			for (const [pubKey, commitment] of pendingUpdates.entries()) {
				updateIndexForParticipant(pubKey, commitment, index, true);
			}
			
			console.log(`[NEEDS-INDEX] Batch updated ${pendingUpdates.size} participants (Svelte-native batching)`);
			pendingUpdates.clear();
			isUpdateScheduled = false;
			
			// Notify subscribers (Svelte batches this automatically)
			set({ ...index }); // Shallow copy to trigger reactivity
		};
		
		// Schedule update (uses queueMicrotask for Svelte-native batching)
		const scheduleUpdate = (pubKey: string, commitment: Commitment | undefined) => {
			pendingUpdates.set(pubKey, commitment);
			
			// Use queueMicrotask (same as Svelte's internal batching)
			// All updates in the same tick are batched together
			if (!isUpdateScheduled) {
				isUpdateScheduled = true;
				queueMicrotask(processPendingUpdates);
			}
		};
		
		// Initial build from all existing commitments
		const allCommitments = getAllCommitmentsRecord();
		for (const [pubKey, commitment] of Object.entries(allCommitments)) {
			updateIndexForParticipant(pubKey, commitment, index, true);
		}
		console.log(`[NEEDS-INDEX] Initial build: ${index.byType.size} types, ${index.byLocation.size} locations, ${index.byTime.size} times`);
		set({ ...index });
		
		// Subscribe to my commitment changes
		const unsubMyCommitment = myCommitmentStore.subscribe((myCommit) => {
			const myPub = get(holsterUserPub);
			if (myPub) {
				scheduleUpdate(myPub, myCommit || undefined);
			}
		});
		
		// Subscribe to network commitment changes
		const unsubNetwork = networkCommitments.subscribe((commitMap) => {
			// Network commitments changed - update index for changed participants
			// This gets called whenever the Map is updated
			for (const [pubKey, commitment] of commitMap.entries()) {
				scheduleUpdate(pubKey, commitment);
			}
		});
		
		return () => {
			unsubMyCommitment();
			unsubNetwork();
		};
	}
);

/**
 * Reactive index of network capacity (for recipients to find providers)
 * 
 * SVELTE-NATIVE REACTIVITY: No manual debouncing needed!
 * - Updates immediately when commitments change (O(M) per participant)
 * - Svelte automatically batches updates to next microtask
 * - Incremental updates prevent O(N×M) full rebuilds
 * 
 * Enables O(1) lookup instead of O(N) scan
 */
export const networkCapacityIndex: Readable<SpaceTimeIndex> = readable<SpaceTimeIndex>(
	{
		byType: new Map(),
		byLocation: new Map(),
		byTime: new Map(),
		byTypeAndLocation: new Map(),
		byTypeAndTime: new Map(),
		byAll: new Map()
	},
	(set) => {
		// Mutable index (we update it incrementally)
		const index: SpaceTimeIndex = {
			byType: new Map(),
			byLocation: new Map(),
			byTime: new Map(),
			byTypeAndLocation: new Map(),
			byTypeAndTime: new Map(),
			byAll: new Map()
		};
		
		// Track which participants have pending updates (batch within same tick)
		let pendingUpdates = new Map<string, Commitment | undefined>();
		let isUpdateScheduled = false;
		
		// Process all pending updates (called via queueMicrotask)
		const processPendingUpdates = () => {
			if (pendingUpdates.size === 0) {
				isUpdateScheduled = false;
				return;
			}
			
			// Process all pending updates
			for (const [pubKey, commitment] of pendingUpdates.entries()) {
				updateIndexForParticipant(pubKey, commitment, index, false); // false = capacity index
			}
			
			console.log(`[CAPACITY-INDEX] Batch updated ${pendingUpdates.size} participants (Svelte-native batching)`);
			pendingUpdates.clear();
			isUpdateScheduled = false;
			
			// Notify subscribers (Svelte batches this automatically)
			set({ ...index }); // Shallow copy to trigger reactivity
		};
		
		// Schedule update (uses queueMicrotask for Svelte-native batching)
		const scheduleUpdate = (pubKey: string, commitment: Commitment | undefined) => {
			pendingUpdates.set(pubKey, commitment);
			
			// Use queueMicrotask (same as Svelte's internal batching)
			// All updates in the same tick are batched together
			if (!isUpdateScheduled) {
				isUpdateScheduled = true;
				queueMicrotask(processPendingUpdates);
			}
		};
		
		// Initial build from all existing commitments
		const allCommitments = getAllCommitmentsRecord();
		for (const [pubKey, commitment] of Object.entries(allCommitments)) {
			updateIndexForParticipant(pubKey, commitment, index, false); // false = capacity index
		}
		console.log(`[CAPACITY-INDEX] Initial build: ${index.byType.size} types, ${index.byLocation.size} locations, ${index.byTime.size} times`);
		set({ ...index });
		
		// Subscribe to my commitment changes
		const unsubMyCommitment = myCommitmentStore.subscribe((myCommit) => {
			const myPub = get(holsterUserPub);
			if (myPub) {
				scheduleUpdate(myPub, myCommit || undefined);
			}
		});
		
		// Subscribe to network commitment changes
		const unsubNetwork = networkCommitments.subscribe((commitMap) => {
			// Network commitments changed - update index for changed participants
			// This gets called whenever the Map is updated
			for (const [pubKey, commitment] of commitMap.entries()) {
				scheduleUpdate(pubKey, commitment);
			}
		});
		
		return () => {
			unsubMyCommitment();
			unsubNetwork();
		};
	}
);

// V5: NO getNetworkRoundStatesRecord (no rounds!)

/**
 * Get subscription statistics (V5)
 */
export function getSubscriptionStats() {
	const commitMap: Map<string, Commitment> = get(networkCommitments);
	return {
		totalSubscriptions: activeSubscriptions.size,
		commitments: commitMap.size,
		uniqueParticipants: getSubscribedParticipants().length,
		// V5: Single commitment store, global recognition model, incremental index updates!
		architecture: 'v5-single-commitment-store'
	};
}

/**
 * Get convergence statistics (V5)
 * 
 * Monitors how many participants have converged
 * 
 * V5: Infers convergence from commitments (needs approaching zero)
 */
export function getConvergenceStats() {
	let convergedCount = 0;
	let totalWithData = 0;
	const epsilon = 0.001; // Convergence threshold
	
	const commitMap = get(networkCommitments);
	
	for (const [_, commitment] of commitMap.entries()) {
		if (commitment.need_slots && commitment.need_slots.length > 0) {
		totalWithData++;
			
			// Check if all needs are near zero
			const totalNeed = commitment.need_slots.reduce((sum, slot) => sum + slot.quantity, 0);
			if (totalNeed < epsilon) {
			convergedCount++;
			}
		}
	}
	
	const convergenceRate = totalWithData > 0 ? convergedCount / totalWithData : 0;
	
	return {
		convergedCount,
		totalWithData,
		convergenceRate,
		networkConverged: convergenceRate >= 0.8 // 80% threshold
	};
}

// ═══════════════════════════════════════════════════════════════════
// V5 DIAGNOSTICS
// ═══════════════════════════════════════════════════════════════════

/**
 * Get V5 architecture diagnostics
 */
export function getV5Diagnostics() {
	const stats = getSubscriptionStats();
	const convergence = getConvergenceStats();
	
	return {
		...stats,
		convergence,
		features: {
			singleCommitmentStore: true, // V5: One store for everything!
			globalMR: true, // V5: Pure global recognition model!
			eventDriven: true,
			itcCausality: true,
			timeBasedDamping: true,
			continuousMonitoring: true,
			reactiveComputation: true,
			incrementalIndexing: true, // V5: O(M) index updates!
			rounds: false, // V5: No rounds!
			vectorClocks: false, // V5: ITC instead
			typeSpecificMR: false, // V5: No type-specific MR!
			separateAllocationStore: false, // V5: Results in commitment!
			separateRecognitionStore: false // V5: Recognition in commitment!
		}
	};
}

// ═══════════════════════════════════════════════════════════════════
// WINDOW DEBUGGING (V5)
// ═══════════════════════════════════════════════════════════════════

if (typeof window !== 'undefined') {
	(window as any).debugStoresV5 = () => {
		console.log('[STORES-V5] Diagnostics:', getV5Diagnostics());
	};
	(window as any).getConvergenceStatsV5 = getConvergenceStats;
	(window as any).getSubscriptionStatsV5 = getSubscriptionStats;
}

