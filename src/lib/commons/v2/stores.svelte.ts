/**
 * Holster Integration for Mutual-Priority Allocation Algorithm v2
 * 
 * V2 Architecture Changes:
 * ✅ Event-driven (no rounds)
 * ✅ ITC causality (not vector clocks)
 * ✅ Time-based damping (not round-indexed)
 * ✅ Reactive stores (auto-recompute)
 * 
 * Provides P2P synchronized stores for:
 * - Commitments (needs, capacity, recognition, damping, ITC stamps)
 * - Allocation States (denominators, allocations, convergence flags)
 * - Recognition Weights (MR values, one-way weights)
 * 
 * REMOVED from V1:
 * - Round States (no rounds!)
 * - Vector clocks (ITC instead)
 * 
 * Features:
 * - Schema-validated data
 * - Automatic persistence
 * - Cross-user subscriptions (for mutual contributors)
 * - Conflict resolution (timestamp-based)
 */

import { createStore } from '../utils/store.svelte';
import {
	CommitmentSchema,
	TwoTierAllocationStateSchema,
	type Commitment,
	type TwoTierAllocationState
} from './schemas';
import * as z from 'zod';

// ═══════════════════════════════════════════════════════════════════
// MY DATA STORES (V2)
// ═══════════════════════════════════════════════════════════════════

/**
 * My Commitment Store (V2)
 * 
 * Publishes my:
 * - Capacity slots (if provider)
 * - Need slots (if recipient)
 * - Recognition weights (MR values, one-way weights)
 * - Adaptive damping state (time-based history)
 * - ITC stamp (causality tracking)
 */
export const myCommitmentStore = createStore({
	holsterPath: 'allocation/commitment',
	schema: CommitmentSchema,
	persistDebounce: 100 // Debounce rapid updates
});

/**
 * My Allocation State Store (V2)
 * 
 * Publishes my computed allocations:
 * - Slot-level denominators (mutual + non-mutual)
 * - Slot allocation records
 * - Recipient totals
 * - Convergence flag (continuous monitoring)
 * - ITC stamp
 */
export const myAllocationStateStore = createStore({
	holsterPath: 'allocation/allocationState',
	schema: TwoTierAllocationStateSchema,
	persistDebounce: 100
});

/**
 * My Recognition Weights Store (V2)
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

// V2: NO ROUND STATE STORE (event-driven, no rounds!)

// ═══════════════════════════════════════════════════════════════════
// INITIALIZATION (V2)
// ═══════════════════════════════════════════════════════════════════

/**
 * Initialize all allocation stores (V2)
 * Call this after holster authentication
 */
export function initializeAllocationStores() {
	console.log('[ALLOCATION-HOLSTER-V2] Initializing stores...');
	
	myCommitmentStore.initialize();
	myAllocationStateStore.initialize();
	myRecognitionWeightsStore.initialize();
	
	console.log('[ALLOCATION-HOLSTER-V2] Stores initialized (event-driven, no rounds)');
}

/**
 * Cleanup all allocation stores (V2)
 * Call this before logout
 */
export async function cleanupAllocationStores() {
	console.log('[ALLOCATION-HOLSTER-V2] Cleaning up stores...');
	
	await Promise.all([
		myCommitmentStore.cleanup(),
		myAllocationStateStore.cleanup(),
		myRecognitionWeightsStore.cleanup()
	]);
	
	console.log('[ALLOCATION-HOLSTER-V2] Stores cleaned up');
}

// ═══════════════════════════════════════════════════════════════════
// NETWORK DATA STORES (OTHER PARTICIPANTS) - V2
// ═══════════════════════════════════════════════════════════════════

/**
 * Network Commitments (V2)
 * Maps pubKey → Commitment (with ITC stamps)
 * 
 * Stores commitments from all participants we're subscribed to
 */
export const networkCommitments = new Map<string, Commitment>();

/**
 * Network Allocation States (V2)
 * Maps pubKey → TwoTierAllocationState (with convergence flags)
 * 
 * Stores allocation states from providers we receive from
 */
export const networkAllocationStates = new Map<string, TwoTierAllocationState>();

/**
 * Network Recognition Weights (V2)
 * Maps pubKey → Record<string, number>
 * 
 * Stores recognition weights from participants (for MR computation)
 */
export const networkRecognitionWeights = new Map<string, Record<string, number>>();

// V2: NO NETWORK ROUND STATES (no rounds!)

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION MANAGEMENT (V2)
// ═══════════════════════════════════════════════════════════════════

const activeSubscriptions = new Set<string>();

/**
 * Subscribe to a participant's commitment (V2)
 * 
 * Use for:
 * - Beneficiaries (people I allocate to) - need their need slots
 * - Providers (people who allocate to me) - need their capacity slots
 * 
 * V2: Automatically triggers reactive allocation computation
 */
export function subscribeToCommitment(pubKey: string) {
	if (activeSubscriptions.has(`${pubKey}:commitment`)) return;
	
	myCommitmentStore.subscribeToUser(pubKey, (commitment) => {
		if (commitment) {
			networkCommitments.set(pubKey, commitment);
			
			// V2: Commitment changes trigger reactive recomputation (automatic)
			console.log(`[ALLOCATION-HOLSTER-V2] Received commitment from ${pubKey.slice(0, 20)}... (triggers reactive update)`);
		} else {
			networkCommitments.delete(pubKey);
		}
	});
	
	activeSubscriptions.add(`${pubKey}:commitment`);
	console.log(`[ALLOCATION-HOLSTER-V2] Subscribed to ${pubKey.slice(0, 20)}... commitment`);
}

/**
 * Subscribe to a provider's allocation state (V2)
 * 
 * Use for:
 * - Providers I receive from - need their denominators for expected allocation
 * 
 * V2: Includes convergence flags for monitoring
 */
export function subscribeToAllocationState(pubKey: string) {
	if (activeSubscriptions.has(`${pubKey}:allocationState`)) return;
	
	myAllocationStateStore.subscribeToUser(pubKey, (allocationState) => {
		if (allocationState) {
			networkAllocationStates.set(pubKey, allocationState);
			
			// V2: Log convergence status
			if (allocationState.converged !== undefined) {
				console.log(`[ALLOCATION-HOLSTER-V2] ${pubKey.slice(0, 20)}... converged: ${allocationState.converged}`);
			}
		} else {
			networkAllocationStates.delete(pubKey);
		}
	});
	
	activeSubscriptions.add(`${pubKey}:allocationState`);
	console.log(`[ALLOCATION-HOLSTER-V2] Subscribed to ${pubKey.slice(0, 20)}... allocation state`);
}

/**
 * Subscribe to a participant's recognition weights (V2)
 * 
 * Use for:
 * - Computing MR values (need both parties' recognition)
 * 
 * V2: Recognition changes trigger reactive MR recomputation
 */
export function subscribeToRecognitionWeights(pubKey: string) {
	if (activeSubscriptions.has(`${pubKey}:recognitionWeights`)) return;
	
	myRecognitionWeightsStore.subscribeToUser(pubKey, (weights) => {
		if (weights) {
			networkRecognitionWeights.set(pubKey, weights);
			
			// V2: Recognition changes trigger reactive recomputation (automatic)
			console.log(`[ALLOCATION-HOLSTER-V2] Received weights from ${pubKey.slice(0, 20)}... (triggers MR update)`);
		} else {
			networkRecognitionWeights.delete(pubKey);
		}
	});
	
	activeSubscriptions.add(`${pubKey}:recognitionWeights`);
	console.log(`[ALLOCATION-HOLSTER-V2] Subscribed to ${pubKey.slice(0, 20)}... recognition weights`);
}

// V2: NO subscribeToRoundState (no rounds!)

/**
 * Subscribe to full participant data (V2)
 * 
 * Use for:
 * - Mutual contributors (full data exchange)
 * 
 * V2: No round state subscription (event-driven)
 */
export function subscribeToFullParticipant(pubKey: string) {
	subscribeToCommitment(pubKey);
	subscribeToAllocationState(pubKey);
	subscribeToRecognitionWeights(pubKey);
	
	console.log(`[ALLOCATION-HOLSTER-V2] Subscribed to ${pubKey.slice(0, 20)}... full data (event-driven)`);
}

/**
 * Unsubscribe from a participant's data (V2)
 * 
 * Note: Holster doesn't provide explicit unsubscribe,
 * so we just remove from our tracking and maps
 */
export function unsubscribeFromParticipant(pubKey: string) {
	activeSubscriptions.delete(`${pubKey}:commitment`);
	activeSubscriptions.delete(`${pubKey}:allocationState`);
	activeSubscriptions.delete(`${pubKey}:recognitionWeights`);
	
	networkCommitments.delete(pubKey);
	networkAllocationStates.delete(pubKey);
	networkRecognitionWeights.delete(pubKey);
	
	console.log(`[ALLOCATION-HOLSTER-V2] Unsubscribed from ${pubKey.slice(0, 20)}...`);
}

/**
 * Get list of all subscribed participants (V2)
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
// UTILITY FUNCTIONS (V2)
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

// V2: NO getNetworkRoundStatesRecord (no rounds!)

/**
 * Get subscription statistics (V2)
 */
export function getSubscriptionStats() {
	return {
		totalSubscriptions: activeSubscriptions.size,
		commitments: networkCommitments.size,
		allocationStates: networkAllocationStates.size,
		recognitionWeights: networkRecognitionWeights.size,
		uniqueParticipants: getSubscribedParticipants().length,
		// V2: No round states
		architecture: 'v2-event-driven'
	};
}

/**
 * Get convergence statistics (V2)
 * 
 * Monitors how many participants have converged
 */
export function getConvergenceStats() {
	let convergedCount = 0;
	let totalWithData = 0;
	
	for (const [_, state] of networkAllocationStates) {
		totalWithData++;
		if (state.converged) {
			convergedCount++;
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
// V2 DIAGNOSTICS
// ═══════════════════════════════════════════════════════════════════

/**
 * Get V2 architecture diagnostics
 */
export function getV2Diagnostics() {
	const stats = getSubscriptionStats();
	const convergence = getConvergenceStats();
	
	return {
		...stats,
		convergence,
		features: {
			eventDriven: true,
			itcCausality: true,
			hybridDamping: true,
			continuousMonitoring: true,
			reactiveComputation: true,
			rounds: false, // V2: No rounds!
			vectorClocks: false // V2: ITC instead
		}
	};
}

// ═══════════════════════════════════════════════════════════════════
// WINDOW DEBUGGING (V2)
// ═══════════════════════════════════════════════════════════════════

if (typeof window !== 'undefined') {
	(window as any).debugStoresV2 = () => {
		console.log('[STORES-V2] Diagnostics:', getV2Diagnostics());
	};
	(window as any).getConvergenceStats = getConvergenceStats;
	(window as any).getSubscriptionStats = getSubscriptionStats;
}

