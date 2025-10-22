/**
 * Algorithm-Driven Subscriptions for Denominator-Centric Fulfillment
 * 
 * Natural subgroups emerge from the algorithm's mathematical structure:
 * - Active Contributors: Providers with Denominator > 0 and Capacity > 0
 * - Active Recipients: Recipients with Residual-Need > 0 and in some numerator
 * - Mutual Recognition Pairs: Fixed recognition relationships (MR > 0)
 * - Satisfied/Saturated: Participants at rest (denominator or need = 0)
 */

import { writable, derived, get } from 'svelte/store';
import type { Writable, Readable } from 'svelte/store';

// ═══════════════════════════════════════════════════════════════════
// CONSTANTS
// ═══════════════════════════════════════════════════════════════════

/**
 * Staleness threshold for dropout detection
 * Participants with commitments older than this are considered dropped out
 * and excluded from active subgroups
 */
export const STALE_THRESHOLD_MS = 60000; // 60 seconds

/**
 * Convergence epsilon
 * Denominators must change by less than this to be considered converged
 */
export const CONVERGENCE_EPSILON = 0.001;

// ═══════════════════════════════════════════════════════════════════
// TYPES
// ═══════════════════════════════════════════════════════════════════

interface Commitment {
	residual_need: number;
	stated_need: number;
	capacity?: number;
	mr_values?: Record<string, number>;
	timestamp: number;
}

interface DenominatorData {
	values: Record<string, number>; // capacity_id -> denominator value
	timestamp: number;
}

interface AllocationState {
	allocated_amount: number;
	recipient_id: string;
	capacity_id: string;
	timestamp: number;
}

interface ParticipantState {
	pubKey: string;
	commitment: Commitment | null;
	denominators: DenominatorData | null;
	allocations: Record<string, AllocationState> | null;
}

// ═══════════════════════════════════════════════════════════════════
// RAW DATA STORES (populated by Holster subscriptions)
// ═══════════════════════════════════════════════════════════════════

/**
 * My own data (always available)
 */
export const myCommitment: Writable<Commitment | null> = writable(null);
export const myDenominators: Writable<DenominatorData | null> = writable(null);
export const myTree: Writable<Record<string, number>> = writable({}); // pubKey -> MR value

/**
 * Network data from other participants
 * Key: participant pubKey
 */
export const networkCommitments: Writable<Record<string, Commitment>> = writable({});
export const networkDenominators: Writable<Record<string, DenominatorData>> = writable({});
export const networkTrees: Writable<Record<string, Record<string, number>>> = writable({});

/**
 * Round coordination
 */
export const currentRound: Writable<number> = writable(0);
export const roundCoordinators: Writable<string[]> = writable([]);

// ═══════════════════════════════════════════════════════════════════
// DERIVED SUBGROUPS (emerge from algorithm structure)
// ═══════════════════════════════════════════════════════════════════

/**
 * GROUP 1: My Tree Members (Beneficiaries)
 * People I recognize - I need their residual needs to compute my denominator
 * 
 * Formula: Denom(Me) = Σ[MR(Me, R) × Residual(R)] for R in myTree
 * Data needed: Their commitments (residual_need, stated_need)
 */
export const myTreeMembers: Readable<string[]> = derived(
	[myTree],
	([$myTree]) => {
		return Object.keys($myTree).filter(pubKey => $myTree[pubKey] > 0);
	}
);

/**
 * GROUP 2: Active Contributors (Providers)
 * People who recognize ME and have capacity - I need their denominators to compute what I'll receive
 * 
 * Formula: Allocation(Me, P) = Capacity(P) × MR(P,Me) × Residual(Me) / Denominator(P)
 * Data needed: Their commitments (capacity), denominators
 */
export const activeContributors: Readable<string[]> = derived(
	[networkCommitments, networkDenominators],
	([$networkCommitments, $networkDenominators]) => {
		const contributors: string[] = [];
		const now = Date.now();
		
		for (const pubKey in $networkCommitments) {
			const commitment = $networkCommitments[pubKey];
			const denomData = $networkDenominators[pubKey];
			
			// Check freshness - exclude stale commitments (dropout detection)
			const isFresh = commitment.timestamp && (now - commitment.timestamp < STALE_THRESHOLD_MS);
			if (!isFresh) {
				console.log(`[DROPOUT] Excluding ${pubKey.slice(0, 20)}... - stale commitment (${now - commitment.timestamp}ms old)`);
				continue;
			}
			
			// Has capacity AND has active denominators
			const hasCapacity = commitment.capacity && commitment.capacity > 0;
			const hasDenominators = denomData && Object.values(denomData.values).some(d => d > 0);
			
			// Recognizes me (has my pubKey in their MR values)
			const recognizesMe = commitment.mr_values && Object.keys(commitment.mr_values).includes(get(myPubKey) || '');
			
			if (hasCapacity && hasDenominators && recognizesMe) {
				contributors.push(pubKey);
			}
		}
		
		return contributors;
	}
);

/**
 * GROUP 3: Active Recipients (in my denominators)
 * People in my tree with residual needs - they appear in my numerators
 * 
 * Formula: Numerator(R) = MR(Me, R) × Active-Need(R)
 * Data needed: Their commitments (residual_need)
 */
export const activeRecipients: Readable<string[]> = derived(
	[myTreeMembers, networkCommitments],
	([$myTreeMembers, $networkCommitments]) => {
		const now = Date.now();
		
		return $myTreeMembers.filter(pubKey => {
			const commitment = $networkCommitments[pubKey];
			
			// Exclude if no commitment or stale (dropout detection)
			if (!commitment) return false;
			
			const isFresh = commitment.timestamp && (now - commitment.timestamp < STALE_THRESHOLD_MS);
			if (!isFresh) {
				console.log(`[DROPOUT] Excluding recipient ${pubKey.slice(0, 20)}... - stale commitment`);
				return false;
			}
			
			// Must have residual need to be active
			return commitment.residual_need > 0;
		});
	}
);

/**
 * GROUP 4: Mutual Contributors (bidirectional recognition)
 * Intersection of myTreeMembers and activeContributors
 * Need full data exchange for coordination
 */
export const mutualContributors: Readable<string[]> = derived(
	[myTreeMembers, activeContributors],
	([$myTreeMembers, $activeContributors]) => {
		const myTreeSet = new Set($myTreeMembers);
		return $activeContributors.filter(pubKey => myTreeSet.has(pubKey));
	}
);

/**
 * GROUP 5: Satisfied Recipients (zero residual need)
 * People in my tree who are satisfied - drop out of my denominators
 */
export const satisfiedRecipients: Readable<string[]> = derived(
	[myTreeMembers, networkCommitments],
	([$myTreeMembers, $networkCommitments]) => {
		return $myTreeMembers.filter(pubKey => {
			const commitment = $networkCommitments[pubKey];
			return commitment && commitment.residual_need === 0;
		});
	}
);

/**
 * GROUP 6: Saturated Providers (zero denominator)
 * Providers whose denominators are all zero - at rest
 */
export const saturatedProviders: Readable<string[]> = derived(
	[networkDenominators],
	([$networkDenominators]) => {
		const saturated: string[] = [];
		
		for (const pubKey in $networkDenominators) {
			const denomData = $networkDenominators[pubKey];
			const allZero = Object.values(denomData.values).every(d => d === 0);
			
			if (allZero) {
				saturated.push(pubKey);
			}
		}
		
		return saturated;
	}
);

/**
 * GROUP 7: All Known Participants
 * Union of everyone we know about (for discovery)
 */
export const allKnownParticipants: Readable<string[]> = derived(
	[myTreeMembers, networkCommitments],
	([$myTreeMembers, $networkCommitments]) => {
		const known = new Set<string>($myTreeMembers);
		Object.keys($networkCommitments).forEach(pubKey => known.add(pubKey));
		return Array.from(known);
	}
);

/**
 * GROUP 8: Active Participants (fresh commitments only)
 * All participants with recent activity - excludes dropouts
 */
export const activeParticipants: Readable<string[]> = derived(
	[networkCommitments],
	([$networkCommitments]) => {
		const now = Date.now();
		
		return Object.entries($networkCommitments)
			.filter(([_, commitment]) => {
				const isFresh = commitment.timestamp && (now - commitment.timestamp < STALE_THRESHOLD_MS);
				return isFresh;
			})
			.map(([pubKey, _]) => pubKey);
	}
);

// ═══════════════════════════════════════════════════════════════════
// PARTICIPATION STATE TRACKING
// ═══════════════════════════════════════════════════════════════════

/**
 * Track convergence state by monitoring denominator changes
 */
export const previousDenominators: Writable<Record<string, Record<string, number>>> = writable({});

export const isConverged: Readable<boolean> = derived(
	[myDenominators, previousDenominators],
	([$myDenominators, $previousDenominators]) => {
		if (!$myDenominators) return false;
		
		const myPub = get(myPubKey);
		if (!myPub) return false;
		
		const prevDenoms = $previousDenominators[myPub];
		if (!prevDenoms) return false;
		
		// Check if all denominators have stabilized
		for (const capacityId in $myDenominators.values) {
			const current = $myDenominators.values[capacityId];
			const previous = prevDenoms[capacityId] || 0;
			
			if (Math.abs(current - previous) > CONVERGENCE_EPSILON) {
				return false; // Still changing
			}
		}
		
		return true; // All stable
	}
);

/**
 * Track which recipients are oscillating (for damping)
 */
export const oscillatingRecipients: Writable<Set<string>> = writable(new Set());

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION MANAGEMENT
// ═══════════════════════════════════════════════════════════════════

/**
 * Track active subscriptions by group
 */
export const activeSubscriptions = {
	beneficiaries: writable<Set<string>>(new Set()),
	contributors: writable<Set<string>>(new Set()),
	mutual: writable<Set<string>>(new Set()),
	coordinators: writable<Set<string>>(new Set())
};

/**
 * Subscription state summary (for debugging)
 */
export const subscriptionSummary: Readable<{
	beneficiaries: number;
	contributors: number;
	mutual: number;
	total: number;
}> = derived(
	[
		activeSubscriptions.beneficiaries,
		activeSubscriptions.contributors,
		activeSubscriptions.mutual
	],
	([$beneficiaries, $contributors, $mutual]) => {
		return {
			beneficiaries: $beneficiaries.size,
			contributors: $contributors.size,
			mutual: $mutual.size,
			total: $beneficiaries.size + $contributors.size - $mutual.size // Subtract overlap
		};
	}
);

// ═══════════════════════════════════════════════════════════════════
// HELPER STORES (for implementation)
// ═══════════════════════════════════════════════════════════════════

/**
 * My public key (needed for filtering)
 */
export const myPubKey: Writable<string | null> = writable(null);

/**
 * Set my public key (called after authentication)
 */
export function setMyPubKey(pubKey: string) {
	myPubKey.set(pubKey);
}

/**
 * Update my tree (when tree changes)
 */
export function updateMyTree(tree: Record<string, number>) {
	myTree.set(tree);
}

/**
 * Update my commitment (when needs or capacity change)
 */
export function updateMyCommitment(commitment: Commitment) {
	myCommitment.set(commitment);
}

/**
 * Update network commitment (from subscription)
 */
export function updateNetworkCommitment(pubKey: string, commitment: Commitment) {
	networkCommitments.update(commitments => ({
		...commitments,
		[pubKey]: commitment
	}));
}

/**
 * Update network denominators (from subscription)
 */
export function updateNetworkDenominators(pubKey: string, denominators: DenominatorData) {
	// Store previous for convergence tracking
	const current = get(networkDenominators)[pubKey];
	if (current) {
		previousDenominators.update(prev => ({
			...prev,
			[pubKey]: current.values
		}));
	}
	
	networkDenominators.update(denoms => ({
		...denoms,
		[pubKey]: denominators
	}));
}

/**
 * Update network tree (from subscription)
 */
export function updateNetworkTree(pubKey: string, tree: Record<string, number>) {
	networkTrees.update(trees => ({
		...trees,
		[pubKey]: tree
	}));
}

/**
 * Clear participant data (when they leave or become inactive)
 */
export function clearParticipantData(pubKey: string) {
	networkCommitments.update(commitments => {
		const { [pubKey]: _, ...rest } = commitments;
		return rest;
	});
	
	networkDenominators.update(denoms => {
		const { [pubKey]: _, ...rest } = denoms;
		return rest;
	});
	
	networkTrees.update(trees => {
		const { [pubKey]: _, ...rest } = trees;
		return rest;
	});
}

// ═══════════════════════════════════════════════════════════════════
// LOGGING / DEBUGGING
// ═══════════════════════════════════════════════════════════════════

/**
 * Debug: Log current subgroup state
 */
export function logSubgroupState() {
	console.log('[SUBGROUPS] Current state:', {
		myTreeMembers: get(myTreeMembers).length,
		activeContributors: get(activeContributors).length,
		activeRecipients: get(activeRecipients).length,
		mutualContributors: get(mutualContributors).length,
		satisfiedRecipients: get(satisfiedRecipients).length,
		saturatedProviders: get(saturatedProviders).length,
		activeParticipants: get(activeParticipants).length,
		allKnown: get(allKnownParticipants).length,
		subscriptionSummary: get(subscriptionSummary),
		isConverged: get(isConverged)
	});
}

if (typeof window !== 'undefined') {
	(window as any).debugSubgroups = logSubgroupState;
}

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION MANAGER (following network.svelte.ts pattern)
// ═══════════════════════════════════════════════════════════════════

import { debounce } from '$lib/utils/debounce';

/**
 * Holster user instance (will be set by initialization)
 */
let holsterUser: any = null;

export function initializeHolsterUser(user: any) {
	holsterUser = user;
	console.log('[ELEGANT] Holster user initialized');
}

/**
 * Subscribe to beneficiaries (my tree members)
 * Data needed: commitments (residual_need, stated_need)
 * Why: To compute my denominator as provider
 */
const debouncedSyncBeneficiarySubscriptions = debounce((members: string[]) => {
	if (!holsterUser) {
		console.warn('[ELEGANT] Cannot subscribe - holsterUser not initialized');
		return;
	}
	
	const currentSubs = get(activeSubscriptions.beneficiaries);
	const memberSet = new Set(members);
	const mutualSet = new Set(get(mutualContributors));
	
	// Remove subscriptions for members no longer in tree
	currentSubs.forEach(pubKey => {
		if (!memberSet.has(pubKey) && !mutualSet.has(pubKey)) {
			// Don't actually unsubscribe (Holster doesn't have clean unsubscribe)
			// Just remove from tracking
			activeSubscriptions.beneficiaries.update(subs => {
				const newSubs = new Set(subs);
				newSubs.delete(pubKey);
				return newSubs;
			});
			console.log(`[BENEFICIARY] Removed ${pubKey.slice(0, 20)}... from tracking`);
		}
	});
	
	// Add subscriptions for new members
	members.forEach(pubKey => {
		// Skip if already subscribed as mutual
		if (mutualSet.has(pubKey)) {
			return;
		}
		
		if (!currentSubs.has(pubKey)) {
			// Subscribe to their commitments
			holsterUser.get([pubKey, 'commitments']).on((data: any) => {
				if (data) {
					updateNetworkCommitment(pubKey, data);
					console.log(`[BENEFICIARY] Received commitment from ${pubKey.slice(0, 20)}...`, {
						residualNeed: data.residual_need,
						statedNeed: data.stated_need
					});
				}
			}, true);
			
			activeSubscriptions.beneficiaries.update(subs => {
				const newSubs = new Set(subs);
				newSubs.add(pubKey);
				return newSubs;
			});
			
			console.log(`[BENEFICIARY] Subscribed to ${pubKey.slice(0, 20)}... commitments`);
		}
	});
}, 100);

/**
 * Subscribe to contributors (people who recognize me)
 * Data needed: commitments (capacity), denominators
 * Why: To compute what I'll receive as recipient
 */
const debouncedSyncContributorSubscriptions = debounce((contributors: string[]) => {
	if (!holsterUser) {
		console.warn('[ELEGANT] Cannot subscribe - holsterUser not initialized');
		return;
	}
	
	const currentSubs = get(activeSubscriptions.contributors);
	const contributorSet = new Set(contributors);
	const mutualSet = new Set(get(mutualContributors));
	
	// Remove subscriptions for people who no longer contribute
	currentSubs.forEach(pubKey => {
		if (!contributorSet.has(pubKey) && !mutualSet.has(pubKey)) {
			activeSubscriptions.contributors.update(subs => {
				const newSubs = new Set(subs);
				newSubs.delete(pubKey);
				return newSubs;
			});
			console.log(`[CONTRIBUTOR] Removed ${pubKey.slice(0, 20)}... from tracking`);
		}
	});
	
	// Add subscriptions for new contributors
	contributors.forEach(pubKey => {
		// Skip if already subscribed as mutual
		if (mutualSet.has(pubKey)) {
			return;
		}
		
		if (!currentSubs.has(pubKey)) {
			// Subscribe to their commitments (for capacity)
			holsterUser.get([pubKey, 'commitments']).on((data: any) => {
				if (data) {
					updateNetworkCommitment(pubKey, data);
					console.log(`[CONTRIBUTOR] Received commitment from ${pubKey.slice(0, 20)}...`, {
						capacity: data.capacity
					});
				}
			}, true);
			
			// Subscribe to their denominators
			holsterUser.get([pubKey, 'denominators']).on((data: any) => {
				if (data) {
					updateNetworkDenominators(pubKey, data);
					console.log(`[CONTRIBUTOR] Received denominators from ${pubKey.slice(0, 20)}...`);
				}
			}, true);
			
			activeSubscriptions.contributors.update(subs => {
				const newSubs = new Set(subs);
				newSubs.add(pubKey);
				return newSubs;
			});
			
			console.log(`[CONTRIBUTOR] Subscribed to ${pubKey.slice(0, 20)}... capacity & denominators`);
		}
	});
}, 100);

/**
 * Subscribe to mutual contributors (bidirectional recognition)
 * Data needed: everything (commitments, denominators, trees, allocations, compose)
 * Why: Full coordination for bidirectional relationships
 */
const debouncedSyncMutualSubscriptions = debounce((mutualList: string[]) => {
	if (!holsterUser) {
		console.warn('[ELEGANT] Cannot subscribe - holsterUser not initialized');
		return;
	}
	
	const currentSubs = get(activeSubscriptions.mutual);
	const mutualSet = new Set(mutualList);
	
	// Remove subscriptions for people no longer mutual
	currentSubs.forEach(pubKey => {
		if (!mutualSet.has(pubKey)) {
			activeSubscriptions.mutual.update(subs => {
				const newSubs = new Set(subs);
				newSubs.delete(pubKey);
				return newSubs;
			});
			console.log(`[MUTUAL] Removed ${pubKey.slice(0, 20)}... from tracking`);
		}
	});
	
	// Add subscriptions for new mutual contributors
	mutualList.forEach(pubKey => {
		if (!currentSubs.has(pubKey)) {
			// Subscribe to ALL their data
			
			// Commitments
			holsterUser.get([pubKey, 'commitments']).on((data: any) => {
				if (data) {
					updateNetworkCommitment(pubKey, data);
				}
			}, true);
			
			// Denominators
			holsterUser.get([pubKey, 'denominators']).on((data: any) => {
				if (data) {
					updateNetworkDenominators(pubKey, data);
				}
			}, true);
			
			// Tree
			holsterUser.get([pubKey, 'tree']).on((data: any) => {
				if (data) {
					updateNetworkTree(pubKey, data);
				}
			}, true);
			
			// Allocation states
			holsterUser.get([pubKey, 'allocationStates']).on((data: any) => {
				if (data) {
					console.log(`[MUTUAL] Received allocations from ${pubKey.slice(0, 20)}...`);
					// Store allocation states (implement as needed)
				}
			}, true);
			
			// Compose data (if needed)
			holsterUser.get([pubKey, 'desiredSlotComposeFrom']).on((data: any) => {
				if (data) {
					console.log(`[MUTUAL] Received compose-from from ${pubKey.slice(0, 20)}...`);
					// Store compose data (implement as needed)
				}
			}, true);
			
			holsterUser.get([pubKey, 'desiredSlotComposeInto']).on((data: any) => {
				if (data) {
					console.log(`[MUTUAL] Received compose-into from ${pubKey.slice(0, 20)}...`);
					// Store compose data (implement as needed)
				}
			}, true);
			
			activeSubscriptions.mutual.update(subs => {
				const newSubs = new Set(subs);
				newSubs.add(pubKey);
				return newSubs;
			});
			
			console.log(`[MUTUAL] Subscribed to ${pubKey.slice(0, 20)}... full data`);
		}
	});
}, 100);

/**
 * Subscribe to round coordinators
 * Data needed: round announcements only
 * Why: Know when to participate in rounds
 */
const debouncedSyncCoordinatorSubscriptions = debounce((coordinatorList: string[]) => {
	if (!holsterUser) {
		console.warn('[ELEGANT] Cannot subscribe - holsterUser not initialized');
		return;
	}
	
	const currentSubs = get(activeSubscriptions.coordinators);
	const coordinatorSet = new Set(coordinatorList);
	
	// Add subscriptions for coordinators
	coordinatorList.forEach(pubKey => {
		if (!currentSubs.has(pubKey)) {
			// Subscribe to their round announcements
			holsterUser.get([pubKey, 'round']).on((roundData: any) => {
				if (roundData && roundData.number > get(currentRound)) {
					console.log(`[COORDINATOR] New round ${roundData.number} from ${pubKey.slice(0, 20)}...`);
					currentRound.set(roundData.number);
					// Trigger round participation (implement as needed)
				}
			}, true);
			
			activeSubscriptions.coordinators.update(subs => {
				const newSubs = new Set(subs);
				newSubs.add(pubKey);
				return newSubs;
			});
			
			console.log(`[COORDINATOR] Subscribed to ${pubKey.slice(0, 20)}... rounds`);
		}
	});
}, 100);

// ═══════════════════════════════════════════════════════════════════
// REACTIVE SUBSCRIPTION SETUP (following network.svelte.ts pattern)
// ═══════════════════════════════════════════════════════════════════

/**
 * Initialize subscriptions based on derived stores
 * Call this after authentication
 */
export function initializeAlgorithmSubscriptions() {
	console.log('[ELEGANT] Initializing algorithm-driven subscriptions...');
	
	// Subscribe to beneficiaries (my tree members)
	myTreeMembers.subscribe(debouncedSyncBeneficiarySubscriptions);
	
	// Subscribe to contributors
	activeContributors.subscribe(debouncedSyncContributorSubscriptions);
	
	// Subscribe to mutual contributors
	mutualContributors.subscribe(debouncedSyncMutualSubscriptions);
	
	// Subscribe to coordinators
	roundCoordinators.subscribe(debouncedSyncCoordinatorSubscriptions);
	
	console.log('[ELEGANT] Algorithm-driven subscriptions initialized');
}

/**
 * Cleanup subscriptions (if needed)
 */
export function cleanupAlgorithmSubscriptions() {
	console.log('[ELEGANT] Cleaning up subscriptions...');
	activeSubscriptions.beneficiaries.set(new Set());
	activeSubscriptions.contributors.set(new Set());
	activeSubscriptions.mutual.set(new Set());
	activeSubscriptions.coordinators.set(new Set());
}

// ═══════════════════════════════════════════════════════════════════
// USAGE EXAMPLE
// ═══════════════════════════════════════════════════════════════════

/*

## Integration Example

```typescript
import {
	initializeHolsterUser,
	initializeAlgorithmSubscriptions,
	setMyPubKey,
	updateMyTree,
	updateMyCommitment,
	myTreeMembers,
	activeContributors,
	mutualContributors,
	subscriptionSummary,
	isConverged
} from '$lib/commons/elegant.svelte';
import { holsterUser } from '$lib/state/holster.svelte';

// After Holster authentication:
async function onHolsterAuthenticated() {
	// 1. Initialize Holster user
	initializeHolsterUser(holsterUser);
	
	// 2. Set my public key
	setMyPubKey(holsterUser.is.pub);
	
	// 3. Initialize algorithm-driven subscriptions
	// This will reactively subscribe based on tree and recognition changes
	initializeAlgorithmSubscriptions();
	
	// 4. Update my tree (this triggers beneficiary subscriptions)
	updateMyTree({
		'alice_pub_key': 0.3,
		'bob_pub_key': 0.7
	});
	
	// 5. Round coordinators (optional, or set in component)
	roundCoordinators.set(['coordinator_pub_1', 'coordinator_pub_2']);
}

// Watch subscription state in components:
$: console.log('Subscription summary:', $subscriptionSummary);
// Output: { beneficiaries: 2, contributors: 3, mutual: 1, total: 4 }

// Watch convergence:
$: if ($isConverged) {
	console.log('System has converged!');
}

// Watch active groups:
$: console.log('My tree members:', $myTreeMembers);
$: console.log('Active contributors:', $activeContributors);
$: console.log('Mutual contributors:', $mutualContributors);

// When tree changes (user edits recognition):
function onTreeChanged(newTree) {
	updateMyTree(newTree);
	// Subscriptions automatically update via derived stores!
}
```

## Key Benefits

1. **Algorithm-Driven**: Subscriptions emerge from actual data dependencies
2. **Reactive**: Svelte stores automatically recompute when dependencies change
3. **Efficient**: Debounced updates prevent subscription thrashing
4. **Natural Bounds**: Limited by Dunbar's number (~150-200 max)
5. **Clean Separation**: 
   - Beneficiaries: Need their residual needs (for my denominator)
   - Contributors: Need their capacities and denominators (for my allocations)
   - Mutual: Need everything (bidirectional coordination)
   - Coordinators: Need only round timing

## Subscription Flow

```
User edits tree
  ↓
myTree store updates
  ↓
myTreeMembers derived store recomputes
  ↓
debouncedSyncBeneficiarySubscriptions triggers
  ↓
Subscribe to new members, unsubscribe from removed
  ↓
Receive commitment data via Holster
  ↓
networkCommitments updates
  ↓
activeContributors derived store recomputes (detects who recognizes me)
  ↓
debouncedSyncContributorSubscriptions triggers
  ↓
...and so on
```

## Data Flow

```
Holster subscriptions → Raw stores (networkCommitments, networkDenominators)
                              ↓
                        Derived stores (myTreeMembers, activeContributors, etc.)
                              ↓
                        Subscription managers (sync functions)
                              ↓
                        Holster subscriptions (reactive loop)
```

*/

/*
# Algorithm Synchronization & Data Flow (elegant.svelte.ts)

## Store Architecture from elegant.svelte.ts

### Raw Data Stores (Populated by Holster Subscriptions)

| Store | Type | Source | Updates |
|-------|------|--------|---------|
| `myCommitment` | Writable | My local state | When I change needs/capacity |
| `myDenominators` | Writable | My local computation | After computing from tree members' needs |
| `myTree` | Writable | My local state | When I update recognition |
| `networkCommitments` | Writable | Holster subscriptions | Real-time from network |
| `networkDenominators` | Writable | Holster subscriptions | Real-time from network |
| `networkTrees` | Writable | Holster subscriptions | Real-time from network |
| `currentRound` | Writable | Coordinators | When new round announced |

### Derived Subgroups (Emerge from Algorithm)

| Subgroup | Derived From | Purpose | Data Needed |
|----------|--------------|---------|-------------|
| `myTreeMembers` | `myTree` | People I recognize → compute my denominator | Their commitments (residual_need) |
| `activeContributors` | `networkCommitments`, `networkDenominators` | Providers who recognize me → compute what I receive | Their capacity, denominators |
| `activeRecipients` | `myTreeMembers`, `networkCommitments` | People in my tree with needs → appear in my numerators | Their residual_need |
| `mutualContributors` | `myTreeMembers` ∩ `activeContributors` | Bidirectional recognition → full coordination | Everything |
| `satisfiedRecipients` | `myTreeMembers`, `networkCommitments` | Zero residual need → drop from denominators | Their residual_need = 0 |
| `saturatedProviders` | `networkDenominators` | Zero denominators → at rest | All denominators = 0 |
| `isConverged` | `myDenominators`, `previousDenominators` | System equilibrium | ΔDenominator < ε |

---

## Continuous Async Operations (No Round Coordination Needed)

| Operation | Sync/Async | Data Source | Trigger | Purpose |
|-----------|------------|-------------|---------|---------|
| **Tree Management** | Async | `myTree` | User edit | Update recognition → triggers subscription changes |
| **Subscription Sync** | Async | Derived stores | `myTreeMembers`, `activeContributors` change | Maintain data connections |
| **Commitment Updates** | Async | `myCommitment` | Local state change | Publish needs/capacity to network |
| **Denominator Computation** | Async | `myTreeMembers` + `networkCommitments` | When tree members' needs change | Compute `Σ[MR × Residual]` |
| **Convergence Detection** | Async | `myDenominators` + `previousDenominators` | After denominator update | Check if `ΔDenom < ε` |

---

## Round-Synchronized Operations

### Phase Timing Breakdown

| Phase | Duration | Sync Type | Critical Data | Purpose |
|-------|----------|-----------|---------------|---------|
| **0. Round Announcement** | Instant | Pub-Sub | Round number from coordinator | All participants know round N started |
| **1. Commitment Snapshot** | 0-5s | **SYNC** | All participants' current commitments | Create consistent view for allocations |
| **2. Allocation Computation** | 5-15s | Async/Local | My tree members' residual needs (from snapshot) | Compute allocations using consistent denominator |
| **3. Allocation Publishing** | 15-20s | Async/Pub-Sub | My computed allocations | Send to recipients |
| **4. Aggregation Window** | 20-50s | Async with timeout | All providers' allocations to me | Collect total received |
| **5. Residual Update** | 50-55s | **SYNC** | My stated_need + total_received | Update residual for next round |
| **6. Convergence Check** | 55-60s | Async | Sample denominators across network | Detect if system converged |

---

## Data Dependencies by Role

### As Provider (I allocate to my tree members):

```typescript
// CONTINUOUS: Subscribe to tree members
myTreeMembers.subscribe((members) => {
  syncBeneficiarySubscriptions(members); // Get their commitments
});

// ROUND N: Allocation computation
const myDenominator = computed from {
  myTree: Record<pubKey, MR_value>,
  networkCommitments: { [pubKey]: { residual_need, stated_need } }
};

const allocations = myCapacity * (
  (MR(me, recipient) * residual_need(recipient)) / myDenominator
);

// ROUND N: Publish allocations
publishAllocations(round, allocations);
```

### As Recipient (I receive from contributors):

```typescript
// CONTINUOUS: Subscribe to contributors
activeContributors.subscribe((contributors) => {
  syncContributorSubscriptions(contributors); // Get their capacity & denominators
});

// ROUND N: Estimate expected allocations
const expectedFromProvider = 
  provider.capacity * provider.MR(me) * myResidualNeed / provider.denominator;

// ROUND N: Aggregate received
const totalReceived = aggregateAllocations(round, activeContributors);

// ROUND N: Update residual (SYNC POINT)
myResidualNeed = max(0, myStatedNeed - totalReceived);
```

---

## Critical Synchronization Points

### 1. **Commitment Snapshot** (Round Start)
```
ALL participants must use commitments from the SAME moment
→ Providers use consistent residual_need values in denominators
→ Prevents race conditions in allocation calculations

Implementation in elegant.svelte.ts:
- Round coordinator announces round N
- All participants freeze current networkCommitments
- Use frozen snapshot for allocation computation
```

### 2. **Residual Need Update** (Round End)
```
ALL recipients must update residual_need AFTER receiving ALL allocations
→ Prevents feedback loop: Provider sees updated need before sending allocation
→ Ensures denominator convergence

Implementation in elegant.svelte.ts:
- Wait for aggregation window to close
- Update myCommitment.residual_need = stated_need - total_received
- Publish updated commitment for next round
```

---

## Participant Dropout Analysis

### Scenario 1: Provider Drops Out Mid-Round

**What Happens:**
```typescript
// Round N starts
providers = [Alice, Bob, Carol]
Alice.denominator = 500
Bob.denominator = 300
Carol.denominator = 200

// Carol drops out at t=25s (after allocation phase, before publishing)
// Recipients were expecting allocations from Carol

// Aggregation phase (t=20-50s)
recipient.expectedAllocations = [Alice's, Bob's, Carol's]
recipient.receivedAllocations = [Alice's, Bob's] // Carol missing ❌

// At aggregation timeout (t=50s)
recipient.totalReceived < recipient.expectedTotal
recipient.residualNeed = stated_need - totalReceived
// residualNeed is HIGHER than expected → next round will allocate more
```

**Recovery:**
```
Round N+1:
- activeContributors derived store recomputes (Carol removed if no commitment)
- Carol drops from expectedAllocations
- Remaining providers' denominators INCREASE (Carol's capacity removed)
- Remaining providers allocate MORE to compensate
→ System converges with fewer providers
```

**Elegant.svelte.ts handles this via:**
- `activeContributors` only includes providers with recent commitments & denominators
- Timeout in aggregation phase (don't wait forever)
- Residual need naturally increases if under-allocated

### Scenario 2: Recipient Drops Out Mid-Round

**What Happens:**
```typescript
// Round N starts
myTreeMembers = [Dave, Eve, Frank]
Dave.residual_need = 100
Eve.residual_need = 50
Frank.residual_need = 75

// Eve drops out at t=30s (after allocation phase)
// I already computed and sent allocations to Eve

myDenominator(round N) = MR(me,Dave)*100 + MR(me,Eve)*50 + MR(me,Frank)*75
myAllocations = {
  Dave: allocation_dave,
  Eve: allocation_eve, // ❌ Eve won't receive this
  Frank: allocation_frank
}

// My capacity is used but Eve's allocation is wasted
```

**Recovery:**
```
Round N+1:
- Eve doesn't publish updated commitment
- networkCommitments[Eve] becomes stale
- activeRecipients derived store removes Eve (no recent commitment)
- My denominator recomputes WITHOUT Eve
- My capacity redistributes to Dave and Frank
→ No wasted capacity in next round
```

**Elegant.svelte.ts handles this via:**
- `activeRecipients` filters by recent commitment timestamp
- Stale commitments (>timeout) excluded from denominator
- `satisfiedRecipients` / dropped recipients treated identically
- Automatic rebalancing next round

### Scenario 3: Coordinator Drops Out

**What Happens:**
```typescript
// Coordinator Alice announces rounds
roundCoordinators = ['alice_pub', 'bob_pub']

// Alice drops out, no round announcement from her
// Bob is backup coordinator

// In elegant.svelte.ts:
roundCoordinators.subscribe((coordinators) => {
  coordinators.forEach(pub => {
    holsterUser.get([pub, 'round']).on(roundData => {
      if (roundData.number > currentRound) {
        currentRound.set(roundData.number); // First one wins
      }
    });
  });
});
```

**Recovery:**
```
- Multiple coordinators can announce rounds
- First announcement triggers round for all participants
- If all coordinators drop: any participant can self-coordinate
- No single point of failure
```

### Scenario 4: Participant Rejoins After Dropout

**What Happens:**
```typescript
// Carol dropped at round N
// Carol rejoins at round N+5

// Carol publishes commitment with fresh timestamp
commitment = {
  residual_need: 200,
  stated_need: 200,
  capacity: 500,
  timestamp: now()
}

// Derived stores immediately recompute:
// - If Carol is in someone's tree → activeRecipients includes her
// - If Carol has capacity & recognizes others → activeContributors includes her
// - Subscriptions sync within 100ms (debounce)
```

**Recovery:**
```
Round N+6:
- Carol appears in denominators again
- Allocations flow to/from Carol
- No special "rejoin" logic needed
→ Graceful rejoin via reactive stores
```

---

## Dropout Recovery Properties

### 1. **Self-Healing Denominators**
```
Provider drops → Removed from activeContributors → Capacity redistributes
Recipient drops → Removed from activeRecipients → Denominator shrinks

No manual cleanup needed - derived stores handle it
```

### 2. **Timeout-Based Resilience**
```
Aggregation phase has timeout (30s typical)
- Don't wait forever for missing allocations
- Update residual with whatever was received
- Under-allocation auto-corrects next round
```

### 3. **Timestamp-Based Freshness**
```typescript
// In activeContributors derived store:
const STALE_THRESHOLD = 60000; // 60 seconds

const isStale = (commitment: Commitment) => 
  Date.now() - commitment.timestamp > STALE_THRESHOLD;

// Stale commitments excluded automatically
```

### 4. **No Consensus Required**
```
Dropout is detected locally by each participant
No need for global agreement that someone dropped
Eventually consistent via reactive stores
```

---

## Data Freshness & Staleness

### Holster Subscription Guarantees

```typescript
// Holster automatically propagates data
user.get([pub, 'commitments']).on(data => {
  updateNetworkCommitment(pub, data);
}, true); // true = get current value immediately

// If pub drops:
// - No new data arrives
// - Last timestamp becomes increasingly stale
// - Derived stores filter out stale participants
```

### Staleness Detection

```typescript
// Add to elegant.svelte.ts:
export const activeParticipants = derived(
  [networkCommitments],
  ([$networkCommitments]) => {
    const now = Date.now();
    const STALE_THRESHOLD = 60000; // 60s
    
    return Object.entries($networkCommitments)
      .filter(([_, commitment]) => 
        now - commitment.timestamp < STALE_THRESHOLD
      )
      .map(([pubKey, _]) => pubKey);
  }
);
```

---

## The Beautiful Emergent Behavior

### Dropout is Indistinguishable from Satisfaction

```
Satisfied recipient: residual_need → 0 → drops from denominators
Dropped recipient: commitment.timestamp stale → excluded from denominators

Same mathematical result!
```

### System Self-Stabilizes

```
Round N: Provider drops → recipients under-allocated
Round N+1: Higher residual needs → remaining providers allocate more
Round N+2: Converges to new equilibrium

No external intervention needed
```

### Zero-Configuration Recovery

```
Participant drops → Eventually excluded by staleness timeout
Participant rejoins → Immediately included by fresh commitment
No "leave" or "join" protocol required
```

---

## Summary: What Requires Sync vs Async

### SYNC (Round-Coordinated):
1. **Commitment snapshot** - All use same starting state
2. **Residual update** - All update after aggregation completes

### ASYNC (Continuous):
1. Tree changes → subscription updates
2. Commitment publishing → network propagation
3. Denominator computation → local calculation
4. Dropout detection → staleness filtering
5. Convergence detection → local threshold check

### The Key Insight:
**Only the allocation feedback loop requires synchronization - everything else, including dropout handling, is naturally async via reactive stores!**

```
Dropout → Stale commitment → Filtered by derived store → Denominator recomputes → System rebalances
                              (All automatic, no sync needed)
```

*/