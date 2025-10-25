/**
 * Mutual-Priority Allocation Algorithm v2 - ITC & Event-Driven
 * 
 * v2 Architecture Changes:
 * ✅ ITC stamps replace vector clocks (O(log n) space)
 * ✅ Event-driven (no rounds, no synchronization barriers)
 * ✅ Reactive allocation computation (auto-recomputes on changes)
 * ✅ Time-based damping history (not round-indexed)
 * ✅ Continuous convergence monitoring (not once-per-round)
 * 
 * TWO-TIER ALLOCATION SYSTEM (UNCHANGED):
 * - TIER 1: Mutual recognition (priority allocation)
 * - TIER 2: Non-mutual (leftover capacity)
 * - Adaptive damping (prevents oscillations)
 * - Allocation capping (ensures contractiveness)
 */

import { derived, get } from 'svelte/store';
import type { Readable } from 'svelte/store';

// Import v2 schemas (ITC-based)
import type {
	Commitment,
	TwoTierAllocationState,
	AvailabilitySlot,
	NeedSlot,
	SlotAllocationRecord,
	ITCStamp,
	DampingHistoryEntry
} from '../v2/schemas';

// Import ITC functions
import { 
	seed as itcSeed,
	event as itcEvent,
	join as itcJoin,
	leq as itcLeq,
	equals as itcEquals,
	toString as itcToString
} from '../utils/itc';

// Import slot matching utilities
import { 
	slotsCompatible, 
	passesSlotFilters, 
	getTimeBucketKey,
	getLocationBucketKey,
	type FilterContext 
} from './match.svelte';

// ═══════════════════════════════════════════════════════════════════
// PLACEHOLDER: Store imports (to be implemented)
// ═══════════════════════════════════════════════════════════════════

// TODO: Create stores-v2.svelte.ts with ITC-based stores
// import {
// 	myCommitmentStore,
// 	myAllocationStateStore,
// 	myRecognitionWeightsStore,
// 	networkCommitments,
// 	networkAllocationStates,
// 	networkRecognitionWeights,
// 	getNetworkCommitmentsRecord,
// 	getNetworkRecognitionWeightsRecord
// } from './stores-v2.svelte';

// Temporary mock stores for development (with proper types)
import { writable } from 'svelte/store';

const myCommitmentStore = writable<Commitment | null>(null);
const myAllocationStateStore = writable<TwoTierAllocationState | null>(null);
const myRecognitionWeightsStore = writable<Record<string, number> | null>(null);
const networkCommitments = new Map<string, Commitment>();
const networkRecognitionWeights = new Map<string, Record<string, number>>();

function getNetworkCommitmentsRecord(): Record<string, Commitment> {
	return Object.fromEntries(networkCommitments);
}

function getNetworkRecognitionWeightsRecord(): Record<string, Record<string, number>> {
	return Object.fromEntries(networkRecognitionWeights);
}

// TODO: Import from holster state
const holsterUserPub = writable<string | null>(null);

// ═══════════════════════════════════════════════════════════════════
// CONSTANTS
// ═══════════════════════════════════════════════════════════════════

export const STALE_THRESHOLD_MS = 60000; // 60 seconds
export const CONVERGENCE_EPSILON = 0.001;
export const DENOMINATOR_FLOOR = 0.0001;
export const DAMPING_HISTORY_WINDOW_MS = 30000; // 30 seconds (time-based, not round-based)
export const DAMPING_HISTORY_MAX_COUNT = 3; // Keep last 3 entries max

// ═══════════════════════════════════════════════════════════════════
// ITC STATE (Replaces Vector Clock)
// ═══════════════════════════════════════════════════════════════════

/**
 * My ITC Stamp - Compact causality tracking
 * Initialized as seed stamp {1, 0}
 */
let myITCStamp: ITCStamp = itcSeed();

/**
 * Get current ITC stamp
 */
export function getMyITCStamp(): ITCStamp {
	return myITCStamp;
}

/**
 * Increment my ITC stamp (before publishing state)
 * Replaces incrementMyVectorClock
 */
export function incrementMyITCStamp(): void {
	myITCStamp = itcEvent(myITCStamp);
	console.log(`[ITC] Incremented stamp: ${itcToString(myITCStamp)}`);
}

/**
 * Merge peer's ITC stamp with mine
 * Replaces updateVectorClockFromPeer
 */
export function mergeITCStampFromPeer(peerStamp: ITCStamp): void {
	const oldStamp = myITCStamp;
	myITCStamp = itcJoin(myITCStamp, peerStamp);
	
	if (!itcEquals(oldStamp, myITCStamp)) {
		console.log(`[ITC] Merged stamp: ${itcToString(myITCStamp)}`);
	}
}

/**
 * Check if peer update is stale
 * Returns true if peerStamp ≤ myStamp (already seen)
 */
export function isPeerUpdateStale(peerStamp: ITCStamp): boolean {
	return itcLeq(peerStamp, myITCStamp) && itcEquals(peerStamp, myITCStamp);
}

/**
 * Get causally consistent commitments
 * Only includes commitments that are causally consistent with our stamp
 */
export function getCausallyConsistentCommitments(): Record<string, Commitment> {
	const allCommitments = getNetworkCommitmentsRecord();
	const snapshot: Record<string, Commitment> = {};
	
	for (const [pubKey, commitment] of Object.entries(allCommitments)) {
		// Include if no ITC stamp (legacy) or if causally consistent
		if (!commitment.itcStamp || itcLeq(commitment.itcStamp, myITCStamp)) {
			snapshot[pubKey] = commitment;
		}
	}
	
	return snapshot;
}

// ═══════════════════════════════════════════════════════════════════
// HELPER STORES
// ═══════════════════════════════════════════════════════════════════

/**
 * My public key (from holster authentication)
 */
export const myPubKey = holsterUserPub;

/**
 * My MR values with all participants
 * Computed from my recognition weights + their recognition weights
 */
export const myMutualRecognition: Readable<Record<string, number>> = derived(
	[myRecognitionWeightsStore],
	([$myRecognitionWeights]) => {
		if (!$myRecognitionWeights) return {};
		
		const myPub = get(myPubKey);
		if (!myPub) return {};
		
		const networkWeights = getNetworkRecognitionWeightsRecord();
		return computeAllMutualRecognition(myPub, $myRecognitionWeights, networkWeights);
	}
);

// ═══════════════════════════════════════════════════════════════════
// DERIVED SUBGROUPS (Algorithm-Driven)
// ═══════════════════════════════════════════════════════════════════

/**
 * My Mutual Beneficiaries - People with MR > 0
 */
export const myMutualBeneficiaries: Readable<string[]> = derived(
	[myMutualRecognition],
	([$myMutualRecognition]) => {
		return Object.entries($myMutualRecognition)
			.filter(([_, mr]) => mr > 0)
			.map(([pubKey, _]) => pubKey);
	}
);

/**
 * My Non-Mutual Beneficiaries - People I recognize (one-way)
 */
export const myNonMutualBeneficiaries: Readable<string[]> = derived(
	[myRecognitionWeightsStore, myMutualRecognition],
	([$myRecognitionWeights, $myMutualRecognition]) => {
		if (!$myRecognitionWeights) return [];
		
		return Object.entries($myRecognitionWeights)
			.filter(([pubKey, weight]) => {
				return weight > 0 && ($myMutualRecognition[pubKey] || 0) === 0;
			})
			.map(([pubKey, _]) => pubKey);
	}
);

/**
 * Mutual Providers for me
 */
export const mutualProvidersForMe: Readable<string[]> = derived(
	[myMutualRecognition],
	([$myMutualRecognition]) => {
		const providers: string[] = [];
		const now = Date.now();
		const commitments = getNetworkCommitmentsRecord();
		
		for (const pubKey in commitments) {
			const commitment = commitments[pubKey];
			
		// Freshness check
		const isFresh = commitment.timestamp && (now - commitment.timestamp < STALE_THRESHOLD_MS);
		if (!isFresh) continue;
		
			// Must have capacity slots
		const hasCapacity = commitment.capacity_slots && commitment.capacity_slots.length > 0 &&
			commitment.capacity_slots.some(slot => slot.quantity > 0);
		if (!hasCapacity) continue;
		
		// Must have mutual recognition
		const mr = $myMutualRecognition[pubKey] || 0;
		if (mr > 0) {
			providers.push(pubKey);
		}
		}
		
		return providers;
	}
);

/**
 * Non-Mutual Providers for me
 */
export const nonMutualProvidersForMe: Readable<string[]> = derived(
	[myPubKey, myMutualRecognition],
	([$myPubKey, $myMutualRecognition]) => {
		if (!$myPubKey) return [];
		
		const providers: string[] = [];
		const now = Date.now();
		const commitments = getNetworkCommitmentsRecord();
		
		for (const pubKey in commitments) {
			const commitment = commitments[pubKey];
			
		// Freshness check
		const isFresh = commitment.timestamp && (now - commitment.timestamp < STALE_THRESHOLD_MS);
		if (!isFresh) continue;
		
			// Must have capacity slots
		const hasCapacity = commitment.capacity_slots && commitment.capacity_slots.length > 0 &&
			commitment.capacity_slots.some(slot => slot.quantity > 0);
		if (!hasCapacity) continue;
		
		// Must recognize me (one-way)
		const recognizesMe = commitment.mr_values && Object.keys(commitment.mr_values).includes($myPubKey);
		
		// But no mutual recognition
		const mr = $myMutualRecognition[pubKey] || 0;
		
		if (recognizesMe && mr === 0) {
			providers.push(pubKey);
		}
		}
		
		return providers;
	}
);

/**
 * Active Participants (fresh commitments)
 */
export const activeParticipants: Readable<string[]> = derived(
	[myCommitmentStore],
	() => {
		const now = Date.now();
		const commitments = getNetworkCommitmentsRecord();
		
		return Object.entries(commitments)
			.filter(([_, commitment]) => {
				const isFresh = commitment.timestamp && (now - commitment.timestamp < STALE_THRESHOLD_MS);
				return isFresh;
			})
			.map(([pubKey, _]) => pubKey);
	}
);

/**
 * Oscillating Participants (damping_factor < 1.0)
 */
export const oscillatingParticipants: Readable<string[]> = derived(
	[myCommitmentStore],
	() => {
		const commitments = getNetworkCommitmentsRecord();
		return Object.entries(commitments)
			.filter(([_, commitment]) => {
				const factor = commitment.damping_factor || 1.0;
				return factor < 1.0;
			})
			.map(([pubKey, _]) => pubKey);
	}
);

// ═══════════════════════════════════════════════════════════════════
// CONVERGENCE TRACKING (Continuous, Not Round-Based)
// ═══════════════════════════════════════════════════════════════════

/**
 * Previous denominators for convergence tracking
 */
const previousDenominators = new Map<string, Record<string, { mutual: number; nonMutual: number }>>();

/**
 * Update previous denominators tracking
 */
export function updatePreviousDenominators(allocationState: TwoTierAllocationState) {
	const myPub = get(myPubKey);
	if (!myPub) return;
	
	previousDenominators.set(myPub, allocationState.slot_denominators);
}

/**
 * Check if system has converged (continuous monitoring)
 */
export const hasSystemConverged: Readable<boolean> = derived(
	[myAllocationStateStore, myPubKey],
	([$myAllocationState, $myPubKey]) => {
		if (!$myAllocationState || !$myPubKey) return false;
		
		const prev = previousDenominators.get($myPubKey);
		if (!prev) return false;
		
		// Check slot-level denominator stability
		for (const [slotId, denoms] of Object.entries($myAllocationState.slot_denominators)) {
			const prevDenoms = prev[slotId];
			if (!prevDenoms) continue;
			
			const mutualDelta = Math.abs(denoms.mutual - prevDenoms.mutual);
			const nonMutualDelta = Math.abs(denoms.nonMutual - prevDenoms.nonMutual);
			
			if (mutualDelta > CONVERGENCE_EPSILON || nonMutualDelta > CONVERGENCE_EPSILON) {
				return false; // Still changing
			}
		}
		
		return true; // All slot denominators stable
	}
);

// ═══════════════════════════════════════════════════════════════════
// MUTUAL RECOGNITION COMPUTATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Compute bilateral Mutual Recognition
 * MR(Me, Them) = min(My recognition of Them, Their recognition of Me)
 */
export function computeMutualRecognition(
	myPub: string,
	theirPub: string,
	myWeights: Record<string, number>,
	theirWeights: Record<string, number>
): number {
	const myRecOfThem = myWeights[theirPub] || 0;
	const theirRecOfMe = theirWeights[myPub] || 0;
	
	return Math.min(myRecOfThem, theirRecOfMe);
}

/**
 * Compute all MR values for a given participant
 */
export function computeAllMutualRecognition(
	myPub: string,
	myWeights: Record<string, number>,
	networkWeights: Record<string, Record<string, number>>
): Record<string, number> {
	const mrValues: Record<string, number> = {};
	
	// For everyone I recognize (including myself if applicable)
	for (const theirPub in myWeights) {
		if (myWeights[theirPub] > 0) {
			const theirWeights = networkWeights[theirPub] || {};
			mrValues[theirPub] = computeMutualRecognition(myPub, theirPub, myWeights, theirWeights);
		}
	}
	
	// For everyone who recognizes me
	for (const theirPub in networkWeights) {
		if (theirPub === myPub) continue;
		
		const theirWeights = networkWeights[theirPub];
		if (theirWeights[myPub] > 0 && !mrValues[theirPub]) {
			mrValues[theirPub] = computeMutualRecognition(myPub, theirPub, myWeights, theirWeights);
		}
	}
	
	return mrValues;
}

// ═══════════════════════════════════════════════════════════════════
// ADAPTIVE DAMPING (Time-Based, Not Round-Based)
// ═══════════════════════════════════════════════════════════════════

/**
 * Detect oscillation pattern in time-stamped history
 */
function detectOscillation(history: DampingHistoryEntry[]): boolean {
	if (history.length < 3) return false;
	
	const recent = history.slice(-3).map(h => h.overAllocation);
	
	// Check for oscillation (up-down-up or down-up-down)
	const upDownUp = recent[0] < recent[1] && recent[1] > recent[2];
	const downUpDown = recent[0] > recent[1] && recent[1] < recent[2];
	
	return upDownUp || downUpDown;
}

/**
 * Detect smooth convergence (monotonically decreasing)
 */
function detectSmoothConvergence(history: DampingHistoryEntry[]): boolean {
	if (history.length < 3) return false;
	
	const recent = history.slice(-3).map(h => h.overAllocation);
	
	return recent[0] >= recent[1] && recent[1] >= recent[2];
}

/**
 * Compute adaptive damping factor based on time-stamped history
 * 
 * v2: Hybrid approach for convergence guarantee
 * - Prefers time window (30s) if it contains 3+ events (responsive to recent changes)
 * - Falls back to last 3 events regardless of time (guarantees damping works)
 * 
 * This ensures:
 * 1. Fast updates: Uses recent time window (responsive)
 * 2. Slow updates: Uses event count (damping still works)
 * 3. Convergence: Always has 3 entries when needed (no damping failures)
 */
export function computeDampingFactor(history: DampingHistoryEntry[]): number {
	if (history.length < 3) {
		return 1.0; // Default full speed initially (not enough data)
	}
	
	const now = Date.now();
	
	// Try time-based filtering first (prefers recent events)
	const timeFiltered = history.filter(h => now - h.timestamp < DAMPING_HISTORY_WINDOW_MS);
	
	// HYBRID: Use time window if it has 3+ events, otherwise fall back to count
	// This guarantees we ALWAYS have 3 events to detect patterns (when history has 3+)
	const relevantHistory = timeFiltered.length >= 3
		? timeFiltered.slice(-DAMPING_HISTORY_MAX_COUNT)  // Time-based (responsive)
		: history.slice(-DAMPING_HISTORY_MAX_COUNT);      // Count-based (guaranteed)
	
	// At this point, relevantHistory.length === 3 (or history.length if < 3)
	// No need to check length again - we're guaranteed to have data
	
	if (detectOscillation(relevantHistory)) {
		return 0.5; // Slow down oscillations
	}
	
	if (detectSmoothConvergence(relevantHistory)) {
		return 1.0; // Full speed when converging smoothly
	}
	
	return 0.8; // Moderate damping otherwise
}

/**
 * Update commitment with new over-allocation data (v2: time-based with fallback)
 * 
 * History management strategy:
 * - Prefer keeping entries within 30s window (responsive to recent changes)
 * - Always keep at least last 3 entries (guarantees damping can work)
 * - This supports the hybrid damping factor computation
 */
export function updateCommitmentDamping(
	commitment: Commitment,
	totalReceived: number
): Commitment {
	// Calculate total stated need from need slots
	const statedNeed = commitment.need_slots 
		? commitment.need_slots.reduce((sum, slot) => sum + slot.quantity, 0)
		: 0;
	
	const overAllocation = Math.max(0, totalReceived - statedNeed);
	
	// Update history (time-stamped entries)
	const history = commitment.damping_history || [];
	history.push({
		overAllocation,
		timestamp: Date.now()
	});
	
	const now = Date.now();
	
	// HYBRID filtering: Keep entries in time window, OR last 3, whichever is MORE
	// This ensures we always have enough history for oscillation detection
	const timeFiltered = history.filter(h => now - h.timestamp < DAMPING_HISTORY_WINDOW_MS);
	
	const filteredHistory = timeFiltered.length >= DAMPING_HISTORY_MAX_COUNT
		? timeFiltered.slice(-DAMPING_HISTORY_MAX_COUNT)  // Use time window
		: history.slice(-DAMPING_HISTORY_MAX_COUNT);      // Use last N events
	
	// Compute new damping factor (will use same hybrid logic)
	const dampingFactor = computeDampingFactor(filteredHistory);
	
	console.log(`[DAMPING-V2] Over-alloc: ${overAllocation.toFixed(2)}, History entries: ${filteredHistory.length}, Factor: ${dampingFactor.toFixed(2)}`);
	
	return {
		...commitment,
		damping_factor: dampingFactor,
		damping_history: filteredHistory
	};
}

// ═══════════════════════════════════════════════════════════════════
// FILTER CONTEXT BUILDING
// ═══════════════════════════════════════════════════════════════════

function buildFilterContext(
	pubKey: string,
	commitment: Commitment,
	mutualRecognition?: number
): FilterContext {
	return {
		pubKey,
		commitment: commitment as any,
		mutualRecognition,
		attributes: (commitment as any).attributes || {}
	};
}

// ═══════════════════════════════════════════════════════════════════
// REACTIVE ALLOCATION COMPUTATION (v2: Event-Driven)
// ═══════════════════════════════════════════════════════════════════

/**
 * Reactive allocation computation (v2)
 * Automatically recomputes when commitments or recognition changes
 * 
 * This replaces manual computeAndPublishAllocations calls with
 * a derived store that automatically triggers on dependency changes.
 */
export const myAllocationsReactive: Readable<TwoTierAllocationState | null> = derived<
	[
		typeof myCommitmentStore,
		typeof myMutualRecognition,
		typeof myRecognitionWeightsStore,
		typeof myPubKey
	],
	TwoTierAllocationState | null
>(
	[myCommitmentStore, myMutualRecognition, myRecognitionWeightsStore, myPubKey],
	([$myCommit, $mr, $weights, $myPubKey], set) => {
		if (!$myCommit || !$weights || !$myPubKey) {
			set(null);
		return;
	}
	
		// Only compute if I have capacity slots
		if (!$myCommit.capacity_slots || $myCommit.capacity_slots.length === 0) {
			set(null);
		return;
	}
	
	// Get network data
	const commitments = getNetworkCommitmentsRecord();
	
	// Compute slot-native allocations
	const allocationState = computeAllocation(
			$myPubKey,
			$myCommit,
			$mr,
			$weights,
		commitments
	);
	
		// Check convergence (continuous)
		const converged = get(hasSystemConverged);
		
		// Update history for next check
	updatePreviousDenominators(allocationState);
	
		// Increment ITC stamp (new allocation published)
		incrementMyITCStamp();
		
		// Return with convergence flag and ITC stamp
		const result: TwoTierAllocationState = {
			...allocationState,
			converged,
			itcStamp: getMyITCStamp(),
		timestamp: Date.now()
	};
	
		set(result);
		
		// Publish to network (auto-persisted by store)
		console.log('[REACTIVE-V2] Computed and publishing allocation state');
	},
	null
);

// ═══════════════════════════════════════════════════════════════════
// SLOT-NATIVE ALLOCATION (Per-Slot Two-Tier Logic) - UNCHANGED
// ═══════════════════════════════════════════════════════════════════

/**
 * Compute slot-native two-tier allocation
 * 
 * Core allocation logic is UNCHANGED from v1.
 * Only the surrounding infrastructure (ITC, reactive, etc.) changed.
 */
export function computeAllocation(
	providerPubKey: string,
	providerCommitment: Commitment,
	myMRValues: Record<string, number>,
	myWeights: Record<string, number>,
	networkCommitments: Record<string, Commitment>
): TwoTierAllocationState {
	
	console.log(`[SLOT-NATIVE-V2] Computing allocation for ${providerPubKey.slice(0,8)}`);
	
	const slotAllocations: SlotAllocationRecord[] = [];
	const slotDenominators: Record<string, { mutual: number; nonMutual: number }> = {};
	const recipientTotals: Record<string, number> = {};
	
	// If no capacity slots, return empty allocation
	if (!providerCommitment.capacity_slots || providerCommitment.capacity_slots.length === 0) {
		return {
			slot_denominators: {},
			slot_allocations: [],
			recipient_totals: {},
			timestamp: Date.now()
		};
	}
	
	// ────────────────────────────────────────────────────────────────
	// OPTIMIZATION 1: Bucket need slots by time and location
	// ────────────────────────────────────────────────────────────────
	
	const timeBuckets = new Map<string, Array<{recipientPub: string, needSlot: NeedSlot}>>();
	const locationBuckets = new Map<string, Array<{recipientPub: string, needSlot: NeedSlot}>>();
	
	for (const [recipientPub, recipientCommitment] of Object.entries(networkCommitments)) {
		if (recipientPub === providerPubKey) continue;
		if (!recipientCommitment.need_slots || recipientCommitment.need_slots.length === 0) continue;
		
		for (const needSlot of recipientCommitment.need_slots) {
			const timeBucket = getTimeBucketKey(needSlot);
			if (!timeBuckets.has(timeBucket)) timeBuckets.set(timeBucket, []);
			timeBuckets.get(timeBucket)!.push({ recipientPub, needSlot });
			
			const locBucket = getLocationBucketKey(needSlot);
			if (!locationBuckets.has(locBucket)) locationBuckets.set(locBucket, []);
			locationBuckets.get(locBucket)!.push({ recipientPub, needSlot });
		}
	}
	
	// ────────────────────────────────────────────────────────────────
	// OPTIMIZATION 2: Pre-compute compatibility matrix
	// ────────────────────────────────────────────────────────────────
	
	const compatibilityMatrix = new Map<string, Map<string, NeedSlot[]>>();
	
	for (const availSlot of providerCommitment.capacity_slots) {
		const compatibleRecipients = new Map<string, NeedSlot[]>();
		
		const candidateSlots = new Set<{recipientPub: string, needSlot: NeedSlot}>();
		
		// Get candidates from buckets
		const availTimeBucket = getTimeBucketKey(availSlot);
		if (availTimeBucket !== 'any-time') {
			const slotsInTimeBucket = timeBuckets.get(availTimeBucket) || [];
			slotsInTimeBucket.forEach(item => candidateSlots.add(item));
		} else {
			for (const items of timeBuckets.values()) {
				items.forEach(item => candidateSlots.add(item));
			}
		}
		
		const availLocBucket = getLocationBucketKey(availSlot);
		const slotsInLocBucket = new Set(locationBuckets.get(availLocBucket) || []);
		
		if (availLocBucket === 'remote' || locationBuckets.has('remote')) {
			const remoteSlots = locationBuckets.get('remote') || [];
			remoteSlots.forEach(item => candidateSlots.add(item));
		}
		
		if (availSlot.city || availSlot.country) {
			candidateSlots.forEach(item => {
				const needLocBucket = getLocationBucketKey(item.needSlot);
				if (!slotsInLocBucket.has(item) && needLocBucket !== 'remote') {
					candidateSlots.delete(item);
				}
			});
		}
		
		// Check detailed compatibility
		for (const item of candidateSlots) {
			if (!slotsCompatible(item.needSlot, availSlot)) continue;
			
			const recipientCommitment = networkCommitments[item.recipientPub];
			if (!recipientCommitment) continue;
			
			const providerContext = buildFilterContext(
				providerPubKey,
				providerCommitment,
				myMRValues[item.recipientPub]
			);
			
			const recipientContext = buildFilterContext(
				item.recipientPub,
				recipientCommitment,
				myMRValues[item.recipientPub]
			);
			
			if (!passesSlotFilters(item.needSlot, availSlot, providerContext, recipientContext)) {
				continue;
			}
			
			if (!compatibleRecipients.has(item.recipientPub)) {
				compatibleRecipients.set(item.recipientPub, []);
			}
			compatibleRecipients.get(item.recipientPub)!.push(item.needSlot);
		}
		
		compatibilityMatrix.set(availSlot.id, compatibleRecipients);
	}
	
	// ────────────────────────────────────────────────────────────────
	// OPTIMIZATION 3: Pre-filter active recipients
	// ────────────────────────────────────────────────────────────────
	
	const activeRecipients = new Set<string>();
	for (const recipientPub of Object.keys(networkCommitments)) {
		if (recipientPub === providerPubKey) continue;
		
		const mr = myMRValues[recipientPub] || 0;
		const weight = myWeights[recipientPub] || 0;
		
		if (mr <= 0 && weight <= 0) continue;
		
		let hasCompatibleSlot = false;
		for (const compatMap of compatibilityMatrix.values()) {
			if (compatMap.has(recipientPub) && compatMap.get(recipientPub)!.length > 0) {
				hasCompatibleSlot = true;
				break;
			}
		}
		
		if (hasCompatibleSlot) {
			activeRecipients.add(recipientPub);
		}
	}
	
	if (activeRecipients.size === 0) {
		return {
			slot_denominators: {},
			slot_allocations: [],
			recipient_totals: {},
			timestamp: Date.now()
		};
	}
	
	// ────────────────────────────────────────────────────────────────
	// PROCESS EACH AVAILABILITY SLOT
	// ────────────────────────────────────────────────────────────────
	
	for (const availSlot of providerCommitment.capacity_slots) {
		const slotQuantity = availSlot.quantity;
		if (slotQuantity <= 0) continue;
		
		const compatibleRecipients = compatibilityMatrix.get(availSlot.id) || new Map();
		if (compatibleRecipients.size === 0) continue;
		
		// Classify as mutual or non-mutual
		const mutualRecipients: Map<string, NeedSlot[]> = new Map();
		const nonMutualRecipients: Map<string, NeedSlot[]> = new Map();
		
		for (const [recipientPub, needSlots] of compatibleRecipients.entries()) {
			if (!activeRecipients.has(recipientPub)) continue;
			
			const mr = myMRValues[recipientPub] || 0;
			const weight = myWeights[recipientPub] || 0;
			
			if (mr > 0) {
				mutualRecipients.set(recipientPub, needSlots);
			} else if (weight > 0) {
				nonMutualRecipients.set(recipientPub, needSlots);
			}
		}
		
		if (mutualRecipients.size === 0 && nonMutualRecipients.size === 0) continue;
		
		// ────────────────────────────────────────────────────────────
		// TIER 1: MUTUAL RECOGNITION ALLOCATION
		// ────────────────────────────────────────────────────────────
		
		let mutualDenominator = 0;
		const mutualNumerators: Map<string, number> = new Map();
		
		let totalMutualRecognition = 0;
		for (const recipientPub of mutualRecipients.keys()) {
			const mr = myMRValues[recipientPub] || 0;
			if (mr > 0) totalMutualRecognition += mr;
		}
		
		if (totalMutualRecognition > 0) {
			for (const [recipientPub, needSlots] of mutualRecipients.entries()) {
				const mr = myMRValues[recipientPub] || 0;
				if (mr === 0) continue;
				
				const recipientCommitment = networkCommitments[recipientPub];
				
				let totalNeed = 0;
				for (const needSlot of needSlots) {
					totalNeed += needSlot.quantity;
				}
				if (totalNeed <= 0) continue;
				
				const mrd = mr / totalMutualRecognition;
				const dampingFactor = recipientCommitment.damping_factor || 1.0;
				const activeNeed = totalNeed * dampingFactor;
				
				const numerator = mrd * activeNeed;
				mutualNumerators.set(recipientPub, numerator);
				mutualDenominator += numerator;
			}
		}
		
		const safeMutualDenominator = Math.max(mutualDenominator, DENOMINATOR_FLOOR);
		let mutualCapacityUsed = 0;
		
		for (const [recipientPub, needSlots] of mutualRecipients.entries()) {
			const numerator = mutualNumerators.get(recipientPub);
			if (!numerator || numerator === 0) continue;
			
			let totalNeed = 0;
			for (const needSlot of needSlots) {
				totalNeed += needSlot.quantity;
			}
			
			const rawAllocation = slotQuantity * numerator / safeMutualDenominator;
			const cappedAllocation = Math.min(rawAllocation, totalNeed);
			
			if (cappedAllocation > 0) {
				let remainingAllocation = cappedAllocation;
				
				for (const needSlot of needSlots) {
					if (remainingAllocation <= 0) break;
					
					const slotAllocation = Math.min(needSlot.quantity, remainingAllocation);
					
					slotAllocations.push({
						availability_slot_id: availSlot.id,
						recipient_pubkey: recipientPub,
						recipient_need_slot_id: needSlot.id,
						quantity: slotAllocation,
						time_compatible: true,
						location_compatible: true,
						tier: 'mutual'
					});
					
					remainingAllocation -= slotAllocation;
					mutualCapacityUsed += slotAllocation;
					recipientTotals[recipientPub] = (recipientTotals[recipientPub] || 0) + slotAllocation;
				}
			}
		}
		
		// ────────────────────────────────────────────────────────────
		// TIER 2: NON-MUTUAL ALLOCATION
		// ────────────────────────────────────────────────────────────
		
		const remainingCapacity = slotQuantity - mutualCapacityUsed;
		
		if (remainingCapacity <= 0.0001 || nonMutualRecipients.size === 0) {
			slotDenominators[availSlot.id] = {
				mutual: mutualDenominator,
				nonMutual: 0
			};
			continue;
		}
		
		let nonMutualDenominator = 0;
		const nonMutualNumerators: Map<string, number> = new Map();
		
		let totalNonMutualRecognition = 0;
		for (const recipientPub of nonMutualRecipients.keys()) {
			const weight = myWeights[recipientPub] || 0;
			if (weight > 0) totalNonMutualRecognition += weight;
		}
		
		if (remainingCapacity > 0 && totalNonMutualRecognition > 0) {
			for (const [recipientPub, needSlots] of nonMutualRecipients.entries()) {
				const weight = myWeights[recipientPub] || 0;
				if (weight === 0) continue;
				
				const recipientCommitment = networkCommitments[recipientPub];
				
				let totalNeed = 0;
				for (const needSlot of needSlots) {
					totalNeed += needSlot.quantity;
				}
				if (totalNeed <= 0) continue;
				
				const renormalizedShare = weight / totalNonMutualRecognition;
				const dampingFactor = recipientCommitment.damping_factor || 1.0;
				const activeNeed = totalNeed * dampingFactor;
				
				const numerator = renormalizedShare * activeNeed;
				nonMutualNumerators.set(recipientPub, numerator);
				nonMutualDenominator += numerator;
			}
		}
		
		const safeNonMutualDenominator = Math.max(nonMutualDenominator, DENOMINATOR_FLOOR);
		let nonMutualCapacityUsed = 0;
		
		for (const [recipientPub, needSlots] of nonMutualRecipients.entries()) {
			const numerator = nonMutualNumerators.get(recipientPub);
			if (!numerator || numerator === 0) continue;
			
			let totalNeed = 0;
			for (const needSlot of needSlots) {
				totalNeed += needSlot.quantity;
			}
			
			const rawAllocation = remainingCapacity * numerator / safeNonMutualDenominator;
			const cappedAllocation = Math.min(rawAllocation, totalNeed);
			
			if (cappedAllocation > 0) {
				let remainingAllocation = cappedAllocation;
				
				for (const needSlot of needSlots) {
					if (remainingAllocation <= 0) break;
					
					const slotAllocation = Math.min(needSlot.quantity, remainingAllocation);
					
					slotAllocations.push({
						availability_slot_id: availSlot.id,
						recipient_pubkey: recipientPub,
						recipient_need_slot_id: needSlot.id,
						quantity: slotAllocation,
						time_compatible: true,
						location_compatible: true,
						tier: 'non-mutual'
					});
					
					remainingAllocation -= slotAllocation;
					nonMutualCapacityUsed += slotAllocation;
					recipientTotals[recipientPub] = (recipientTotals[recipientPub] || 0) + slotAllocation;
				}
			}
		}
		
		slotDenominators[availSlot.id] = {
			mutual: mutualDenominator,
			nonMutual: nonMutualDenominator
		};
	}
	
	console.log(`[SLOT-NATIVE-V2] Total: ${slotAllocations.length} allocation records, ${Object.keys(recipientTotals).length} recipients`);
	
	return {
		slot_denominators: slotDenominators,
		slot_allocations: slotAllocations,
		recipient_totals: recipientTotals,
		timestamp: Date.now()
	};
}

// ═══════════════════════════════════════════════════════════════════
// PUBLISHING FUNCTIONS (v2: ITC-Enhanced)
// ═══════════════════════════════════════════════════════════════════

/**
 * Publish my commitment (with ITC stamp)
 */
export async function publishMyCommitment(commitment: Commitment) {
	// Increment ITC stamp
	incrementMyITCStamp();
	
	const myPub = get(myPubKey);
	if (!myPub) {
		console.warn('[PUBLISH-V2] Cannot publish: no pubKey');
		return;
	}
	
	// Compute MR values before publishing
	const mrValues = get(myMutualRecognition);
	const recognitionWeights = get(myRecognitionWeightsStore);
	
	// Add metadata with ITC stamp
	const enrichedCommitment: Commitment = {
		...commitment,
		mr_values: mrValues,
		recognition_weights: recognitionWeights || {},
		itcStamp: getMyITCStamp(),
		timestamp: Date.now()
	};
	
	// Persist via Holster store
	await myCommitmentStore.set(enrichedCommitment);
	
	console.log('[PUBLISH-V2] Published commitment with ITC stamp');
}

/**
 * Publish my recognition weights
 */
export async function publishMyRecognitionWeights(weights: Record<string, number>) {
	await myRecognitionWeightsStore.set(weights);
	
	console.log('[PUBLISH-V2] Published recognition weights:', {
		count: Object.keys(weights).length
	});
}

// ═══════════════════════════════════════════════════════════════════
// DEBUG / LOGGING
// ═══════════════════════════════════════════════════════════════════

export function logSubgroupState() {
	console.log('[SUBGROUPS-V2] Current state:', {
		mutualBeneficiaries: get(myMutualBeneficiaries).length,
		nonMutualBeneficiaries: get(myNonMutualBeneficiaries).length,
		mutualProviders: get(mutualProvidersForMe).length,
		nonMutualProviders: get(nonMutualProvidersForMe).length,
		activeParticipants: get(activeParticipants).length,
		oscillatingParticipants: get(oscillatingParticipants).length,
		isConverged: get(hasSystemConverged),
		itcStamp: itcToString(getMyITCStamp())
	});
}

if (typeof window !== 'undefined') {
	(window as any).debugAllocationV2 = logSubgroupState;
	(window as any).computeAllocationV2 = computeAllocation;
	(window as any).updateCommitmentDampingV2 = updateCommitmentDamping;
	(window as any).computeDampingFactorV2 = computeDampingFactor;
	(window as any).getMyITCStamp = getMyITCStamp;
	(window as any).incrementMyITCStamp = incrementMyITCStamp;
}

