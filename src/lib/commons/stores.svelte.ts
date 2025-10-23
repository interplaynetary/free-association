/**
 * Holster Integration for Mutual-Priority Allocation Algorithm
 * 
 * Provides P2P synchronized stores for:
 * - Commitments (needs, capacity, recognition, damping)
 * - Allocation States (denominators, allocations)
 * - Recognition Weights (MR values, one-way weights)
 * - Round States (vector clocks, round coordination)
 * 
 * Features:
 * - Schema-validated data
 * - Automatic persistence
 * - Cross-user subscriptions (for mutual contributors)
 * - localStorage caching
 * - Conflict resolution
 */

import { createStore } from './store.svelte';
import {
	CommitmentSchema,
	TwoTierAllocationStateSchema,
	RoundStateSchema,
	AllocationSchemas,
	type Commitment,
	type TwoTierAllocationState,
	type RoundState
} from '$lib/commons/schemas';
import * as z from 'zod';

// ═══════════════════════════════════════════════════════════════════
// MY DATA STORES
// ═══════════════════════════════════════════════════════════════════

/**
 * My Commitment Store
 * 
 * Publishes my:
 * - Current residual need
 * - Stated need
 * - Available capacity (if provider)
 * - Recognition weights (MR values, one-way weights)
 * - Adaptive damping state
 */
export const myCommitmentStore = createStore({
	holsterPath: 'allocation/commitment',
	schema: CommitmentSchema,
	persistDebounce: 100 // Debounce rapid updates
});

/**
 * My Allocation State Store
 * 
 * Publishes my computed allocations:
 * - Mutual recognition denominator
 * - Non-mutual denominator
 * - Allocations by tier
 */
export const myAllocationStateStore = createStore({
	holsterPath: 'allocation/allocationState',
	schema: TwoTierAllocationStateSchema,
	persistDebounce: 100
});

/**
 * My Recognition Weights Store
 * 
 * Publishes my recognition of others:
 * - One-way recognition weights (% of my 100%)
 * - Used to compute MR with others
 */
export const myRecognitionWeightsStore = createStore({
	holsterPath: 'allocation/recognitionWeights',
	schema: z.record(z.string(), z.number().nonnegative()),
	persistDebounce: 200 // Recognition changes less frequently
});

/**
 * My Round State Store
 * 
 * Publishes my current round state:
 * - Current round number
 * - Vector clock
 * - For decentralized coordination
 */
export const myRoundStateStore = createStore({
	holsterPath: 'allocation/roundState',
	schema: RoundStateSchema,
	persistDebounce: 0 // Publish immediately
});

// ═══════════════════════════════════════════════════════════════════
// INITIALIZATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Initialize all allocation stores
 * Call this after holster authentication
 */
export function initializeAllocationStores() {
	console.log('[ALLOCATION-HOLSTER] Initializing stores...');
	
	myCommitmentStore.initialize();
	myAllocationStateStore.initialize();
	myRecognitionWeightsStore.initialize();
	myRoundStateStore.initialize();
	
	console.log('[ALLOCATION-HOLSTER] Stores initialized');
}

/**
 * Cleanup all allocation stores
 * Call this before logout
 */
export async function cleanupAllocationStores() {
	console.log('[ALLOCATION-HOLSTER] Cleaning up stores...');
	
	await Promise.all([
		myCommitmentStore.cleanup(),
		myAllocationStateStore.cleanup(),
		myRecognitionWeightsStore.cleanup(),
		myRoundStateStore.cleanup()
	]);
	
	console.log('[ALLOCATION-HOLSTER] Stores cleaned up');
}

// ═══════════════════════════════════════════════════════════════════
// NETWORK DATA STORES (OTHER PARTICIPANTS)
// ═══════════════════════════════════════════════════════════════════

/**
 * Network Commitments
 * Maps pubKey → Commitment
 * 
 * Stores commitments from all participants we're subscribed to
 */
export const networkCommitments = new Map<string, Commitment>();

/**
 * Network Allocation States
 * Maps pubKey → TwoTierAllocationState
 * 
 * Stores allocation states from providers we receive from
 */
export const networkAllocationStates = new Map<string, TwoTierAllocationState>();

/**
 * Network Recognition Weights
 * Maps pubKey → Record<string, number>
 * 
 * Stores recognition weights from participants (for MR computation)
 */
export const networkRecognitionWeights = new Map<string, Record<string, number>>();

/**
 * Network Round States
 * Maps pubKey → RoundState
 * 
 * Stores round states from coordinators/peers
 */
export const networkRoundStates = new Map<string, RoundState>();

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION MANAGEMENT
// ═══════════════════════════════════════════════════════════════════

const activeSubscriptions = new Set<string>();

/**
 * Subscribe to a participant's commitment
 * 
 * Use for:
 * - Beneficiaries (people I allocate to) - need their residual needs
 * - Providers (people who allocate to me) - need their capacity
 */
export function subscribeToCommitment(pubKey: string) {
	if (activeSubscriptions.has(`${pubKey}:commitment`)) return;
	
	myCommitmentStore.subscribeToUser(pubKey, (commitment) => {
		if (commitment) {
			networkCommitments.set(pubKey, commitment);
		} else {
			networkCommitments.delete(pubKey);
		}
	});
	
	activeSubscriptions.add(`${pubKey}:commitment`);
	console.log(`[ALLOCATION-HOLSTER] Subscribed to ${pubKey.slice(0, 20)}... commitment`);
}

/**
 * Subscribe to a provider's allocation state
 * 
 * Use for:
 * - Providers I receive from - need their denominators for expected allocation
 */
export function subscribeToAllocationState(pubKey: string) {
	if (activeSubscriptions.has(`${pubKey}:allocationState`)) return;
	
	myAllocationStateStore.subscribeToUser(pubKey, (allocationState) => {
		if (allocationState) {
			networkAllocationStates.set(pubKey, allocationState);
		} else {
			networkAllocationStates.delete(pubKey);
		}
	});
	
	activeSubscriptions.add(`${pubKey}:allocationState`);
	console.log(`[ALLOCATION-HOLSTER] Subscribed to ${pubKey.slice(0, 20)}... allocation state`);
}

/**
 * Subscribe to a participant's recognition weights
 * 
 * Use for:
 * - Computing MR values (need both parties' recognition)
 */
export function subscribeToRecognitionWeights(pubKey: string) {
	if (activeSubscriptions.has(`${pubKey}:recognitionWeights`)) return;
	
	myRecognitionWeightsStore.subscribeToUser(pubKey, (weights) => {
		if (weights) {
			networkRecognitionWeights.set(pubKey, weights);
		} else {
			networkRecognitionWeights.delete(pubKey);
		}
	});
	
	activeSubscriptions.add(`${pubKey}:recognitionWeights`);
	console.log(`[ALLOCATION-HOLSTER] Subscribed to ${pubKey.slice(0, 20)}... recognition weights`);
}

/**
 * Subscribe to a coordinator's round state
 * 
 * Use for:
 * - Round coordination (vector clock, round number)
 * 
 * Automatically handles peer round state updates (vector clock merge, round advancement)
 */
export function subscribeToRoundState(pubKey: string) {
	if (activeSubscriptions.has(`${pubKey}:roundState`)) return;
	
	myRoundStateStore.subscribeToUser(pubKey, (roundState) => {
		if (roundState) {
			networkRoundStates.set(pubKey, roundState);
			
			// Handle peer round state (vector clock merge, round advancement check)
			// Import and call from mutual-priority-allocation-refactored
			import('./algorithm.svelte').then(module => {
				module.handlePeerRoundState(pubKey, roundState);
			});
		} else {
			networkRoundStates.delete(pubKey);
		}
	});
	
	activeSubscriptions.add(`${pubKey}:roundState`);
	console.log(`[ALLOCATION-HOLSTER] Subscribed to ${pubKey.slice(0, 20)}... round state`);
}

/**
 * Subscribe to full participant data (all stores)
 * 
 * Use for:
 * - Mutual contributors (full data exchange)
 */
export function subscribeToFullParticipant(pubKey: string) {
	subscribeToCommitment(pubKey);
	subscribeToAllocationState(pubKey);
	subscribeToRecognitionWeights(pubKey);
	subscribeToRoundState(pubKey);
	
	console.log(`[ALLOCATION-HOLSTER] Subscribed to ${pubKey.slice(0, 20)}... full data`);
}

/**
 * Unsubscribe from a participant's data
 * 
 * Note: Holster doesn't provide explicit unsubscribe,
 * so we just remove from our tracking and maps
 */
export function unsubscribeFromParticipant(pubKey: string) {
	activeSubscriptions.delete(`${pubKey}:commitment`);
	activeSubscriptions.delete(`${pubKey}:allocationState`);
	activeSubscriptions.delete(`${pubKey}:recognitionWeights`);
	activeSubscriptions.delete(`${pubKey}:roundState`);
	
	networkCommitments.delete(pubKey);
	networkAllocationStates.delete(pubKey);
	networkRecognitionWeights.delete(pubKey);
	networkRoundStates.delete(pubKey);
	
	console.log(`[ALLOCATION-HOLSTER] Unsubscribed from ${pubKey.slice(0, 20)}...`);
}

/**
 * Get list of all subscribed participants
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
// UTILITY FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Get all commitments as a Record (for algorithm compatibility)
 */
export function getNetworkCommitmentsRecord(): Record<string, Commitment> {
	const record: Record<string, Commitment> = {};
	for (const [pubKey, commitment] of networkCommitments) {
		record[pubKey] = commitment;
	}
	return record;
}

/**
 * Get all allocation states as a Record
 */
export function getNetworkAllocationStatesRecord(): Record<string, TwoTierAllocationState> {
	const record: Record<string, TwoTierAllocationState> = {};
	for (const [pubKey, state] of networkAllocationStates) {
		record[pubKey] = state;
	}
	return record;
}

/**
 * Get all recognition weights as a Record
 */
export function getNetworkRecognitionWeightsRecord(): Record<string, Record<string, number>> {
	const record: Record<string, Record<string, number>> = {};
	for (const [pubKey, weights] of networkRecognitionWeights) {
		record[pubKey] = weights;
	}
	return record;
}

/**
 * Get all round states as a Record
 */
export function getNetworkRoundStatesRecord(): Record<string, RoundState> {
	const record: Record<string, RoundState> = {};
	for (const [pubKey, state] of networkRoundStates) {
		record[pubKey] = state;
	}
	return record;
}

/**
 * Get subscription statistics
 */
export function getSubscriptionStats() {
	return {
		totalSubscriptions: activeSubscriptions.size,
		commitments: networkCommitments.size,
		allocationStates: networkAllocationStates.size,
		recognitionWeights: networkRecognitionWeights.size,
		roundStates: networkRoundStates.size,
		uniqueParticipants: getSubscribedParticipants().length
	};
}

