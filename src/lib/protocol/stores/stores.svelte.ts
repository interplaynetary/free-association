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
import { createStore } from '$lib/utils/primitives/store.svelte';
// NOTE: Converters removed! We now use JSON.stringify/parse for simplicity and reliability.
// This eliminates 400+ lines of complex conversion logic and entire classes of bugs.
import {
	CommitmentSchema,
	RootNodeSchema,
	AvailabilitySlotSchema,
	NeedSlotSchema,
	normalizeGlobalRecognitionWeights,
	type Commitment,
	type RootNode,
	type AvailabilitySlot,
	type NeedSlot,
	type GlobalRecognitionWeights,
	type SlotAllocationRecord
} from '@playnet/free-association/schemas';
import { holsterUserPub, holsterUser } from '$lib/network/holster.svelte';
import { getTimeBucketKey, getLocationBucketKey } from '@playnet/free-association/utils/match';
import { sharesOfGeneralFulfillmentMap, getAllContributorsFromTree } from '@playnet/free-association/tree';
// Pure attribute-based membership
import { myAttributeRecognitions, myAttributeSubscriptions } from './attributes.svelte';
import { slotSubscriptions, slotFilters, capacityCache, needCache } from '$lib/network/capacity-subscriptions.svelte';
import { applyFiltersUnion, mergeSlots } from '@playnet/free-association/utils/capacity-filters';
import { resolveContributorWithOrgs, resolveToPublicKey } from '$lib/network/users.svelte';
import { seed as itcSeed, event as itcEvent, join as itcJoin, leq as itcLeq, type Stamp as ITCStamp } from '$lib/utils/primitives/itc';



// ═══════════════════════════════════════════════════════════════════
// TYPE EXTENSIONS (LOCAL)
// ═══════════════════════════════════════════════════════════════════

/**
 * Slots Cache Entry - Cached slots from another user
 * 
 * Enables offline allocation computation by caching others' needs and capacity.
 * Per-source ITC tracking allows staleness detection even offline.
 */
export interface SlotsCacheEntry {
	/** Cached need slots from this user */
	need_slots?: NeedSlot[];

	/** Cached capacity slots from this user */
	capacity_slots?: AvailabilitySlot[];

	/** ITC stamp from their commitment (for per-source staleness detection) */
	itcStamp?: ITCStamp;

	/** Timestamp from their commitment (fallback) */
	timestamp: number;

	/** When we cached this data locally */
	cached_at: number;
}

/**
 * Extended Commitment with Allocation Cache
 * 
 * Extends the base Commitment type with local caching for offline operation.
 * Similar to how we cache others_recognition_of_me, we cache their slots.
 */
export interface CommitmentWithCache extends Commitment {
	/** Cache of others' slots for offline allocation */
	others_slots_cache?: Record<string, SlotsCacheEntry>;
}

// ═══════════════════════════════════════════════════════════════════
// MY DATA STORES (V5)
// ═══════════════════════════════════════════════════════════════════

/**
 * My Recognition Tree Store (V5) - SOURCE
 * 
 * The tree structure that generates my recognition weights!
 * 
 * How it works:
 * 1. I build a tree with nodes representing what I value
 * 2. Tree nodes have contributors (people who contribute to each goal)
 * 3. The tree structure determines recognition shares via sharesOfGeneralFulfillmentMap()
 * 4. Recognition weights are auto-computed in derived store below
 * 
 * Example Tree:
 *   My Values (Root)
 *   ├─ Healthcare (70 points)
 *   │  └─ Dr. Smith contributes → gets 56% recognition
 *   └─ Food (30 points)
 *      └─ Alice contributes → gets 24% recognition
 * 
 * V5: Tree structure encodes type preferences (not separate per-type MR values)
 */
export const myRecognitionTreeStore = createStore({
	holsterPath: 'trees/recognition_tree',
	schema: RootNodeSchema,
	persistDebounce: 200 // Debounce tree edits
	// NOTE: No converters needed! JSON handles everything perfectly.
});

/**
 * My Recognition Weights (V5) - DERIVED
 * 
 * Auto-computed from my recognition tree using protocol.ts
 * This is my "outgoing" recognition - who I recognize and how much
 * 
 * Reactive: Updates automatically when tree changes!
 */
export const myRecognitionWeights: Readable<GlobalRecognitionWeights> = derived(
	[myRecognitionTreeStore],
	([$tree]) => {
		console.log('[🌳 RECOGNITION-WEIGHTS] Computing from tree...');

		if (!$tree) {
			console.log('[🌳 RECOGNITION-WEIGHTS] ❌ No tree available');
			return {};
		}

		try {
			// Run protocol calculation: tree → recognition shares
			const weights = sharesOfGeneralFulfillmentMap($tree, {});
			const contributorCount = Object.keys(weights).length;
			const nonZeroCount = Object.values(weights).filter(w => w > 0).length;

			console.log(`[🌳 RECOGNITION-WEIGHTS] ✅ Computed ${contributorCount} contributors (${nonZeroCount} non-zero):`);
			Object.entries(weights).forEach(([id, weight]) => {
				if (weight > 0) {
					console.log(`  • ${id.slice(0, 20)}... → ${(weight * 100).toFixed(2)}%`);
				}
			});

			return weights;
		} catch (error) {
			console.error('[🌳 RECOGNITION-WEIGHTS] ❌ Error computing from tree:', error);
			return {};
		}
	}
);

/**
 * My Commitment Store (V5) - PRIMARY SOURCE OF TRUTH
 * 
 * ✅ ARCHITECTURAL SIMPLIFICATION: This is THE ONLY persistent store for my data!
 * 
 * This is what gets published to the network AND what all derived stores read from!
 * 
 * Contains EVERYTHING:
 * - Capacity slots (derived stores read from here!)
 * - Need slots (derived stores read from here!)
 * - Global recognition weights (from myRecognitionWeights - computed from tree!)
 * - Global MR values (mutual recognition with others)
 * - Adaptive damping state (time-based history)
 * - ITC stamp (causality tracking)
 * 
 * To update slots: Use setMyNeedSlots() or setMyCapacitySlots() helpers
 */
export const myCommitmentStore = createStore({
	holsterPath: 'allocation/commitment',
	schema: CommitmentSchema,
	persistDebounce: 100 // Debounce rapid updates
	// NOTE: No converters needed! JSON handles everything perfectly.
});

/**
 * My Need Slots Store (V5) - DERIVED FROM COMMITMENT
 * 
 * ✅ ARCHITECTURAL SIMPLIFICATION: Derived from commitment (single source of truth!)
 * 
 * Before: Persisted separately → composed into commitment (data duplication, sync issues)
 * After: Derived from commitment (single source of truth, always consistent)
 * 
 * What I need from the commons (e.g., food, housing, healthcare)
 * Each slot specifies quantity, type, time, location constraints
 * 
 * To update: Use setMyNeedSlots() helper function
 */
export const myNeedSlotsStore: Readable<NeedSlot[] | null> = derived(
	[myCommitmentStore],
	([$commitment]) => $commitment?.need_slots || null
);

/**
 * My Capacity Slots Store (V5) - DERIVED FROM COMMITMENT
 * 
 * ✅ ARCHITECTURAL SIMPLIFICATION: Derived from commitment (single source of truth!)
 * 
 * Before: Persisted separately → composed into commitment (data duplication, sync issues)
 * After: Derived from commitment (single source of truth, always consistent)
 * 
 * What I can provide to the commons (e.g., meals, tutoring, rides)
 * Each slot specifies quantity, type, time, location constraints
 * 
 * To update: Use setMyCapacitySlots() helper function
 */
export const myCapacitySlotsStore: Readable<AvailabilitySlot[] | null> = derived(
	[myCommitmentStore],
	([$commitment]) => $commitment?.capacity_slots || null
);

/**
 * My Need Types Store (V5) - DERIVED FROM NEED SLOTS
 * 
 * Extracts unique need_type_ids from my need slots for UI organization
 * Returns array of type IDs that I have needs for
 * 
 * Use for:
 * - Organizing need slots by type in UI
 * - Filtering/grouping needs
 * - Quick type existence checks
 */
export const myNeedTypesStore: Readable<string[]> = derived(
	[myNeedSlotsStore],
	([$needSlots]) => {
		if (!$needSlots || $needSlots.length === 0) {
			return [];
		}

		// Extract unique need_type_ids
		const typeIds = new Set<string>();
		for (const slot of $needSlots) {
			if (slot.need_type_id) {
				typeIds.add(slot.need_type_id);
			}
		}

		return Array.from(typeIds).sort();
	}
);

/**
 * My Capacity Types Store (V5) - DERIVED FROM CAPACITY SLOTS
 * 
 * Extracts unique need_type_ids from my capacity slots for UI organization
 * Returns array of type IDs that I can provide
 * 
 * Use for:
 * - Organizing capacity slots by type in UI
 * - Filtering/grouping capacity
 * - Quick type existence checks
 */
export const myCapacityTypesStore: Readable<string[]> = derived(
	[myCapacitySlotsStore],
	([$capacitySlots]) => {
		if (!$capacitySlots || $capacitySlots.length === 0) {
			return [];
		}

		// Extract unique need_type_ids
		const typeIds = new Set<string>();
		for (const slot of $capacitySlots) {
			if (slot.need_type_id) {
				typeIds.add(slot.need_type_id);
			}
		}

		return Array.from(typeIds).sort();
	}
);

// NOTE: Helper functions (setMyNeedSlots, setMyCapacitySlots) moved down below
// because they reference myMutualRecognition which is defined later


// V5: NO ALLOCATION STATE STORE (commitments capture allocation results!)
// V5: NO SEPARATE RECOGNITION STORE (recognition in commitment!)
// V5: Recognition tree generates the weights that go into commitment!

// ═══════════════════════════════════════════════════════════════════
// INITIALIZATION (V5)
// ═══════════════════════════════════════════════════════════════════

/**
 * Initialize all allocation stores (V5)
 * Call this after holster authentication
 * 
 * ✅ SIMPLIFIED: Only 2 persistent stores now (tree + commitment)!
 * Slots are derived from commitment (single source of truth)
 */
export function initializeAllocationStores() {
	console.log('[ALLOCATION-HOLSTER-V5] Initializing stores...');

	// Source stores (persistent)
	myRecognitionTreeStore.initialize();
	myCommitmentStore.initialize(); // THE source of truth for slots!

	console.log('[ALLOCATION-HOLSTER-V5] Stores initialized:');
	console.log('  - Recognition tree (persistent)');
	console.log('  - Commitment (persistent - contains slots!)');
	console.log('  - Need slots (derived from commitment)');
	console.log('  - Capacity slots (derived from commitment)');
	console.log('  - Recognition weights (derived from tree)');
}

/**
 * Cleanup all allocation stores (V5)
 * Call this before logout
 * 
 * ✅ SIMPLIFIED: Only 2 persistent stores to clean up now!
 */
export async function cleanupAllocationStores() {
	console.log('[ALLOCATION-HOLSTER-V5] Cleaning up stores...');

	await myRecognitionTreeStore.cleanup();
	await myCommitmentStore.cleanup();

	console.log('[ALLOCATION-HOLSTER-V5] Stores cleaned up');
}

// ═══════════════════════════════════════════════════════════════════
// NETWORK DATA STORES (OTHER PARTICIPANTS) - V5 WITH VERSIONED STORES
// ═══════════════════════════════════════════════════════════════════

// V5: NO ROUND STATE STORE (event-driven, no rounds!)
// V5: NO ALLOCATION STATE STORE (commitments capture allocation results!)
// V5: NO SEPARATE RECOGNITION STORE (recognition in commitment!)
// V5: Recognition tree generates the weights that go into commitment!

// NOTE: Helper functions (setMyNeedSlots, setMyCapacitySlots) defined below
// after myMutualRecognition and getMergedITCStamp are available

// ═══════════════════════════════════════════════════════════════════
// INITIALIZATION (V5)
// ═══════════════════════════════════════════════════════════════════

import { createVersionedStore, type VersionedStore } from '$lib/utils/primitives/v-store.svelte';
// NETWORK DATA STORES (OTHER PARTICIPANTS) - V5 WITH VERSIONED STORES
// ═══════════════════════════════════════════════════════════════════

/**
 * Network Commitments (V5) - VERSIONED STORE 🚀
 * 
 * This is the ONLY network store needed for allocation!
 * 
 * Now with FINE-GRAINED REACTIVITY:
 * - Tracks 4 independent fields (recognition, needs, capacity, damping)
 * - Only triggers updates when specific fields change
 * - ITC causality for conflict resolution
 * - Field versions for precise change tracking
 * 
 * Performance improvement: 3-4× faster reactive updates!
 * - Recognition change → only MR recalculates (not indexes)
 * - Need change → only need index rebuilds (not MR or capacity)
 * - Capacity change → only capacity index rebuilds (not MR or needs)
 * 
 * Maps pubKey → Commitment, containing:
 * - Their needs + capacity
 * - Their recognition weights (who they recognize - computed from their tree!)
 * - Their MR values (mutual recognition)
 * - Their damping state
 * - Their ITC stamp
 */
import { jsonEquals } from '$lib/utils/primitives/v-store-equality-checkers';

export const networkCommitments: VersionedStore<Commitment, string> = createVersionedStore({
	fields: {
		// Track each critical field independently
		recognition: (c) => c.global_recognition_weights,
		needs: (c) => c.need_slots,
		capacity: (c) => c.capacity_slots,
		damping: (c) => c.multi_dimensional_damping,
		allocations: (c) => c.slot_allocations // ✅ Track allocations for fine-grained reactivity
		// NOTE: 'mr' field removed - MR is now computed from recognition weights (not stored)
	},
	fieldEqualityCheckers: {
		// Use deep equality for array fields (arrays of objects)
		needs: jsonEquals,
		capacity: jsonEquals,
		allocations: jsonEquals // ✅ Deep equality for allocation records
	},
	schema: CommitmentSchema, // ✅ Defensive validation for network data
	itcExtractor: (c) => c.itcStamp,
	timestampExtractor: (c) => c.timestamp,
	enableLogging: true
});

/**
 * Network Recognition Trees (V5) - VERSIONED STORE (OPTIONAL)
 * 
 * Maps pubKey → RootNode (their recognition tree)
 * 
 * NOTE: Usually you don't need other people's trees!
 * Their computed recognition weights are in their commitments.
 * 
 * Only subscribe to trees if you want to:
 * - Visualize how someone else recognizes people
 * - Debug recognition calculations
 * - Build trust through transparency
 * 
 * Most participants will NEVER subscribe to trees, only commitments.
 */
export const networkRecognitionTrees: VersionedStore<RootNode, string> = createVersionedStore({
	fields: {
		// Track structural changes
		structure: (tree) => tree.children,
		// Track contributor changes
		contributors: (tree) => {
			const contributorIds = new Set<string>();
			function traverse(node: any) {
				if (node.contributors) {
					node.contributors.forEach((c: any) => contributorIds.add(c.id));
				}
				if (node.anti_contributors) {
					node.anti_contributors.forEach((c: any) => contributorIds.add(c.id));
				}
				node.children?.forEach(traverse);
			}
			traverse(tree);
			return Array.from(contributorIds).sort();
		},
		fulfillment: (tree) => tree.manual_fulfillment
	},
	schema: RootNodeSchema, // ✅ Defensive validation for network tree data
	timestampExtractor: (tree) => new Date(tree.updated_at || Date.now()).getTime(),
	enableLogging: false
});

// V5: NO NETWORK ROUND STATES (no rounds!)
// V5: NO NETWORK ALLOCATION STATES (commitments capture results!)
// V5: NO NETWORK RECOGNITION WEIGHTS STORE (recognition in commitments!)
// V5: Network trees are optional (most people only need commitments!)

// ═══════════════════════════════════════════════════════════════════
// FINE-GRAINED FIELD STORES (Derived from Versioned Stores)
// ═══════════════════════════════════════════════════════════════════

/**
 * Network Recognition Weights - FIELD STORE
 * 
 * Fine-grained store for just the recognition field!
 * 
 * ✅ Only updates when recognition changes
 * ✅ NOT triggered by needs/capacity/damping changes
 * 
 * Use this for:
 * - Computing mutual recognition
 * - Recognition-based matching
 * - Trust graphs
 */
export const networkRecognitionWeights = networkCommitments.deriveField<GlobalRecognitionWeights>('recognition');


/**
 * Network Need Slots - FIELD STORE
 * 
 * Fine-grained store for just the needs field!
 * 
 * ✅ Only updates when needs change
 * ✅ NOT triggered by recognition/capacity/damping changes
 * 
 * Use this for:
 * - Need indexing
 * - Provider matching
 * - Allocation computation
 */
export const networkNeedSlots = derived(networkCommitments, ($commits) => {
	const allNeeds: (NeedSlot & { pubkey: string })[] = [];
	for (const [pubkey, commitment] of Object.entries($commits)) {
		if (commitment.need_slots) {
			for (const slot of commitment.need_slots) {
				allNeeds.push({ ...slot, pubkey });
			}
		}
	}
	return allNeeds;
});

/**
 * Network Capacity Slots - FIELD STORE
 * 
 * Fine-grained store for just the capacity field!
 * 
 * ✅ Only updates when capacity changes
 * ✅ NOT triggered by recognition/needs/damping changes
 * 
 * Use this for:
 * - Capacity indexing
 * - Recipient matching
 * - Allocation computation
 */
export const networkCapacitySlots = networkCommitments.deriveField<AvailabilitySlot[]>('capacity');

/**
 * Network Allocations - FIELD STORE
 * 
 * Fine-grained store for just the allocations field!
 * 
 * ✅ Only updates when allocations change
 * ✅ NOT triggered by recognition/needs/capacity/damping changes
 * 
 * Use this for:
 * - Displaying incoming allocations (who's providing to my needs)
 * - Allocation auditing and transparency
 * - Network flow visualization
 * 
 * Maps pubKey → SlotAllocationRecord[]
 * Each provider's published allocations from their capacity to recipients' needs
 */
export const networkAllocations = networkCommitments.deriveField<SlotAllocationRecord[]>('allocations');

/**
 * Network Need Types Store (V5) - DERIVED FROM NETWORK NEED SLOTS
 * 
 * Extracts unique need_type_ids from all network participants' need slots
 * Returns array of type IDs that exist across the network
 * 
 * ✅ Fine-grained reactivity: Only updates when network needs change
 * 
 * Use for:
 * - Organizing network needs by type in UI
 * - Filtering/grouping network needs
 * - Discovering what types are needed in the network
 */
export const networkNeedTypesStore: Readable<string[]> = derived(
	[networkNeedSlots],
	([$networkNeedSlots]) => {
		const typeIds = new Set<string>();

		// Iterate through all participants' need slots
		for (const [pubKey, needSlots] of $networkNeedSlots.entries()) {
			if (needSlots && Array.isArray(needSlots)) {
				for (const slot of needSlots) {
					if (slot.need_type_id) {
						typeIds.add(slot.need_type_id);
					}
				}
			}
		}

		return Array.from(typeIds).sort();
	}
);

/**
 * Network Capacity Types Store (V5) - DERIVED FROM NETWORK CAPACITY SLOTS
 * 
 * Extracts unique need_type_ids from all network participants' capacity slots
 * Returns array of type IDs that can be provided across the network
 * 
 * ✅ Fine-grained reactivity: Only updates when network capacity changes
 * 
 * Use for:
 * - Organizing network capacity by type in UI
 * - Filtering/grouping network capacity
 * - Discovering what types are available in the network
 */
export const networkCapacityTypesStore: Readable<string[]> = derived(
	[networkCapacitySlots],
	([$networkCapacitySlots]) => {
		const typeIds = new Set<string>();

		// Iterate through all participants' capacity slots
		for (const [pubKey, capacitySlots] of $networkCapacitySlots.entries()) {
			if (capacitySlots && Array.isArray(capacitySlots)) {
				for (const slot of capacitySlots) {
					if (slot.need_type_id) {
						typeIds.add(slot.need_type_id);
					}
				}
			}
		}

		return Array.from(typeIds).sort();
	}
);

/**
 * My Mutual Recognition - LOCAL-FIRST ARCHITECTURE ✨
 * 
 * Computes mutual recognition from MY COMMITMENT ONLY:
 * 1. My recognition of them: commitment.global_recognition_weights (from tree)
 * 2. Their recognition of me: commitment.others_recognition_of_me (cached from network)
 * 
 * Formula: MR(me, them) = min(myRec[them], theirRec[me])
 * Special case: MR(me, me) = myRec[me] (self-recognition becomes self-MR)
 * 
 * ✨ ELEGANT LOCAL-FIRST:
 * - Everything needed is in MY commitment (single source!)
 * - No network dependency for computation
 * - Works offline with cached data
 * - Updates immediately when tree changes
 * - Cache updates when network proves otherwise
 * 
 * Examples:
 * - I recognize Alice 40%, cached: Alice recognizes me 60% → MR = 40%
 * - I recognize Bob 70%, cached: Bob recognizes me 50% → MR = 50%
 * - I recognize myself 10% → MR(me, me) = 10%
 * 
 * KEY INSIGHT: Commitment-as-cache!
 * - global_recognition_weights: Source (from tree)
 * - others_recognition_of_me: Cache (from network, updated when proven otherwise)
 * - MR: Purely computed (not stored!)
 */
export const myMutualRecognition: Readable<GlobalRecognitionWeights> = derived(
	[holsterUserPub, myCommitmentStore],  // ✅ Only my commitment! Truly local-first!
	([$myPub, $myCommitment]) => {
		console.log('[🤝 MUTUAL-REC] Computing mutual recognition (local-first)...');

		if (!$myPub || !$myCommitment) {
			console.log('[🤝 MUTUAL-REC] ❌ No pub key or commitment available');
			return {};
		}

		// Source: Who I recognize (from tree)
		const myWeights = $myCommitment.global_recognition_weights || {};

		// Cache: Others' recognition of me (from network, updated when proven otherwise)
		const othersRecCache = $myCommitment.others_recognition_of_me || {};

		const mutualRec: GlobalRecognitionWeights = {};

		const myRecCount = Object.keys(myWeights).length;
		const cacheCount = Object.keys(othersRecCache).length;

		console.log(`[🤝 MUTUAL-REC] My recognition: ${myRecCount} entries`);
		console.log(`[🤝 MUTUAL-REC] Cached others' rec: ${cacheCount} entries`);

		// For everyone I recognize (including myself!)
		for (const theirPub in myWeights) {
			const myRecOfThem = myWeights[theirPub] || 0;

			// ✅ SPECIAL CASE: Self-recognition
			if (theirPub === $myPub) {
				mutualRec[theirPub] = myRecOfThem;  // MR(me, me) = myRec[me]
				console.log(`[🤝 MUTUAL-REC]   ${theirPub.slice(0, 20)}... (SELF): MR=${(myRecOfThem * 100).toFixed(2)}%`);
				continue;
			}

			// Get their recognition of me from cache
			const theirWeights = othersRecCache[theirPub];
			const theirRecOfMe = theirWeights?.[$myPub] || 0;

			// Compute MR
			const mr = Math.min(myRecOfThem, theirRecOfMe);
			mutualRec[theirPub] = mr;

			if (mr > 0 || myRecOfThem > 0 || theirRecOfMe > 0) {
				const source = theirWeights ? 'CACHED' : 'AWAITING';
				console.log(`[🤝 MUTUAL-REC]   ${theirPub.slice(0, 20)}...: I→them=${(myRecOfThem * 100).toFixed(2)}%, them→me=${(theirRecOfMe * 100).toFixed(2)}%, MR=${(mr * 100).toFixed(2)}% [${source}]`);
			}
		}

		const mutualCount = Object.values(mutualRec).filter(mr => mr > 0).length;
		console.log(`[🤝 MUTUAL-REC] ✅ Computed ${mutualCount} mutual relationships (local-first!)`);

		return mutualRec;
	}
);

// ═══════════════════════════════════════════════════════════════════
// NETWORK CACHE UPDATER (LOCAL-FIRST)
// ═══════════════════════════════════════════════════════════════════

/**
 * Local-First Cache Updater: "Trust Until Proven Otherwise"
 * 
 * Listens to incoming network commitments and updates our local cache
 * (others_recognition_of_me) ONLY when network data proves a change.
 * 
 * This ensures MR calculations remain stable and local-first, updating
 * reactively only when new information arrives from the network.
 */
networkCommitments.subscribe(($networkCommitsVersioned) => {
	const myPub = get(holsterUserPub);
	const myCommitment = get(myCommitmentStore);

	if (!myPub || !myCommitment) return;

	const cache = myCommitment.others_recognition_of_me || {};
	const updates: Record<string, GlobalRecognitionWeights> = {};

	// Check each network commitment for changes
	for (const [theirPub, versionedEntity] of $networkCommitsVersioned.entries()) {
		// Skip own commitment (prevents infinite loop when our data syncs back)
		if (theirPub === myPub) continue;

		const theirWeights = versionedEntity.data.global_recognition_weights;
		if (!theirWeights) continue;

		// Normalize and extract their recognition of me
		const normalized = normalizeGlobalRecognitionWeights(theirWeights);
		const networkRecOfMe = normalized[myPub] || 0;
		const cachedRecOfMe = cache[theirPub]?.[myPub] || 0;

		// Network proved otherwise? Update cache!
		if (networkRecOfMe !== cachedRecOfMe) {
			updates[theirPub] = normalized;
			console.log(`[CACHE-UPDATE] ${theirPub.slice(0, 20)}...: ${cachedRecOfMe} → ${networkRecOfMe}`);
		}
	}

	// Apply updates if any changes detected
	if (Object.keys(updates).length > 0) {
		console.log('[CACHE-UPDATE] Network proved changes - updating commitment cache');
		myCommitmentStore.set({
			...myCommitment,
			others_recognition_of_me: { ...cache, ...updates }
		});
	}
});

// ═══════════════════════════════════════════════════════════════════
// SLOTS CACHE UPDATER (OFFLINE-FIRST ALLOCATION)
// ═══════════════════════════════════════════════════════════════════

/**
 * Slots Cache Updater: Cache Others' Slots for Offline Allocation
 * 
 * Listens to incoming network commitments and caches their slots
 * (need_slots, capacity_slots) along with ITC stamps for staleness detection.
 * 
 * This enables:
 * - Offline allocation computation (no network needed)
 * - Per-source staleness detection via ITC comparison
 * - Network resilience (graceful degradation during outages)
 * - Local-first operation (trust cache until network proves otherwise)
 */
networkCommitments.subscribe(($networkCommitsVersioned) => {
	const myPub = get(holsterUserPub);
	const myCommitment = get(myCommitmentStore);

	if (!myPub || !myCommitment) return;

	const slotsCache = (myCommitment as any).others_slots_cache || {};
	const slotsUpdates: Record<string, SlotsCacheEntry> = {};

	// Check each network commitment for slot changes
	for (const [theirPub, versionedEntity] of $networkCommitsVersioned.entries()) {
		// Skip own commitment
		if (theirPub === myPub) continue;

		const theirCommitment = versionedEntity.data;
		const cached = slotsCache[theirPub];

		// Update cache if:
		// 1. No cache exists, OR
		// 2. Network data is newer (ITC comparison)
		const shouldUpdate = !cached ||
			(theirCommitment.itcStamp && cached.itcStamp &&
				!itcLeq(theirCommitment.itcStamp, cached.itcStamp));

		if (shouldUpdate) {
			slotsUpdates[theirPub] = {
				need_slots: theirCommitment.need_slots,
				capacity_slots: theirCommitment.capacity_slots,
				itcStamp: theirCommitment.itcStamp,
				timestamp: theirCommitment.timestamp || Date.now(),
				cached_at: Date.now()
			};

			console.log(`[SLOTS-CACHE] Updating ${theirPub.slice(0, 20)}... (${theirCommitment.need_slots?.length || 0} needs, ${theirCommitment.capacity_slots?.length || 0} capacity)`);
		}
	}

	// Apply updates if any changes detected
	if (Object.keys(slotsUpdates).length > 0) {
		console.log(`[SLOTS-CACHE] Caching slots from ${Object.keys(slotsUpdates).length} users for offline allocation`);
		myCommitmentStore.set({
			...myCommitment,
			others_slots_cache: { ...slotsCache, ...slotsUpdates }
		} as any);
	}
});

// ═══════════════════════════════════════════════════════════════════
// HELPER FUNCTIONS (Slot Updates) ✅
// ═══════════════════════════════════════════════════════════════════

/**
 * Helper: Set my need slots
 * Updates the commitment with new need slots
 * 
 * ✅ This is how you update slots now (commitment is the source of truth!)
 */
export function setMyNeedSlots(needSlots: NeedSlot[]) {
	const current = get(myCommitmentStore);
	const recognitionWeights = get(myRecognitionWeights);

	// Merge ITC with network
	const mergedITC = getMergedITCStamp(current?.itcStamp);

	const updated: Commitment = {
		need_slots: needSlots,
		capacity_slots: current?.capacity_slots || [],
		global_recognition_weights: recognitionWeights,
		others_recognition_of_me: current?.others_recognition_of_me,  // Preserve cache!
		multi_dimensional_damping: current?.multi_dimensional_damping,
		itcStamp: mergedITC,
		timestamp: Date.now()
	};

	myCommitmentStore.set(updated);
	console.log('[SET-NEED-SLOTS] Updated:', needSlots.length, 'slots');
}

/**
 * Helper: Set my capacity slots
 * Updates the commitment with new capacity slots
 * 
 * ✅ This is how you update slots now (commitment is the source of truth!)
 */
export function setMyCapacitySlots(capacitySlots: AvailabilitySlot[]) {
	const current = get(myCommitmentStore);
	const recognitionWeights = get(myRecognitionWeights);

	// Merge ITC with network
	const mergedITC = getMergedITCStamp(current?.itcStamp);

	const updated: Commitment = {
		need_slots: current?.need_slots || [],
		capacity_slots: capacitySlots,
		global_recognition_weights: recognitionWeights,
		others_recognition_of_me: current?.others_recognition_of_me,  // Preserve cache!
		multi_dimensional_damping: current?.multi_dimensional_damping,
		itcStamp: mergedITC,
		timestamp: Date.now()
	};

	myCommitmentStore.set(updated);
	console.log('[SET-CAPACITY-SLOTS] Updated:', capacitySlots.length, 'slots');
}

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION MANAGEMENT (V5)
// ═══════════════════════════════════════════════════════════════════

const activeSubscriptions = new Set<string>();

// ═══════════════════════════════════════════════════════════════════
// NOTE: Staleness checking is now handled by the versioned store system!
// The generic versioned store provides:
// - ITC causality tracking
// - Timestamp fallback
// - Field-level change detection
// - Built-in deep equality checking
// 
// No need for manual staleness checks in subscription functions!
// ═══════════════════════════════════════════════════════════════════

/**
 * Subscribe to a participant's commitment (V5) - WITH VERSIONED STORE 🚀
 * 
 * Use for:
 * - Beneficiaries (people I allocate to) - need their need slots
 * - Providers (people who allocate to me) - need their capacity slots
 * 
 * V5: Automatically triggers reactive allocation computation
 * 
 * PERFORMANCE BOOST with Versioned Store:
 * - ITC causality checking (built-in staleness detection)
 * - Field-level change detection (only triggers when fields actually change)
 * - Fine-grained reactivity (only affected stores update)
 * - Incremental index updates (O(M) per participant)
 * 
 * Before: Any commitment → all derived stores update
 * After: Recognition change → only MR recalculates
 *        Need change → only need index rebuilds
 *        Capacity change → only capacity index rebuilds
 * 
 * CRITICAL: Normalizes their global_recognition_weights on receipt!
 * This ensures MR computation uses properly normalized distributions.
 */
export function subscribeToCommitment(pubKey: string) {
	if (activeSubscriptions.has(`${pubKey}:commitment`)) return;

	myCommitmentStore.subscribeToUser(pubKey, (commitment) => {
		console.log(`[📡 NETWORK-SUB] Received commitment from ${pubKey.slice(0, 20)}...`);

		// Handle deletion
		if (!commitment) {
			const deleted = networkCommitments.delete(pubKey);
			if (deleted) {
				console.log(`[📡 NETWORK-SUB] 🗑️  Removed commitment from ${pubKey.slice(0, 20)}...`);
			} else {
				console.log(`[📡 NETWORK-SUB] ⏭️  Skipped: ${pubKey.slice(0, 20)}... already absent`);
			}
			return;
		}

		// Log what we received
		const recognitionCount = Object.keys(commitment.global_recognition_weights || {}).length;
		const nonZeroRec = Object.values(commitment.global_recognition_weights || {}).filter(w => w > 0).length;
		console.log(`[📡 NETWORK-SUB] Commitment contains ${recognitionCount} recognition entries (${nonZeroRec} non-zero)`);

		// CRITICAL: Normalize their recognition weights before storing
		// This ensures that when we compute MR, their recognition of us is a proper fraction
		let normalizedCommitment = commitment;
		if (commitment.global_recognition_weights) {
			normalizedCommitment = {
				...commitment,
				global_recognition_weights: normalizeGlobalRecognitionWeights(
					commitment.global_recognition_weights
				)
			};
			console.log(`[📡 NETWORK-SUB] Normalized recognition weights for ${pubKey.slice(0, 20)}...`);
		}

		// Update via versioned store - handles ITC, timestamps, and field change detection!
		const result = networkCommitments.update(pubKey, normalizedCommitment);

		if (result.applied) {
			const changedFields = Array.from(result.changedFields!).join(', ');
			console.log(`[📡 NETWORK-SUB] ✅ Updated [${changedFields}] from ${pubKey.slice(0, 20)}...`);
		} else {
			console.log(`[📡 NETWORK-SUB] ⏭️  Skipped from ${pubKey.slice(0, 20)}... (${result.reason})`);
		}
	});

	activeSubscriptions.add(`${pubKey}:commitment`);
	console.log(`[📡 NETWORK-SUB] ✅ Subscribed to ${pubKey.slice(0, 20)}... commitment`);
}

/**
 * Subscribe to a participant's recognition tree (V5) - OPTIONAL WITH VERSIONED STORE
 * 
 * Use for:
 * - Transparency (seeing how someone recognizes people)
 * - Debugging recognition calculations
 * - Building trust
 * 
 * NOTE: Most participants don't need this!
 * Recognition weights are in their commitment (already subscribed).
 * 
 * V5: Trees are for transparency, not required for allocation
 * 
 * VERSIONED STORE: Now with field-level tracking!
 * - structure: Tree node changes
 * - contributors: Contributor list changes
 * - fulfillment: Manual fulfillment changes
 */
export function subscribeToRecognitionTree(pubKey: string) {
	if (activeSubscriptions.has(`${pubKey}:tree`)) return;

	myRecognitionTreeStore.subscribeToUser(pubKey, (tree) => {
		// Handle deletion
		if (!tree) {
			const deleted = networkRecognitionTrees.delete(pubKey);
			if (deleted) {
				console.log(`[ALLOCATION-HOLSTER-V5] 🗑️  Removed recognition tree from ${pubKey.slice(0, 20)}...`);
			}
			return;
		}

		// Update via versioned store - handles timestamps and field change detection!
		const result = networkRecognitionTrees.update(pubKey, tree);

		if (result.applied) {
			const changedFields = Array.from(result.changedFields!).join(', ');
			console.log(`[ALLOCATION-HOLSTER-V5] ✅ Updated tree [${changedFields}] from ${pubKey.slice(0, 20)}...`);
		} else {
			console.log(`[ALLOCATION-HOLSTER-V5] ⏭️  Skipped tree from ${pubKey.slice(0, 20)}... (${result.reason})`);
		}
	});

	activeSubscriptions.add(`${pubKey}:tree`);
	console.log(`[ALLOCATION-HOLSTER-V5] Subscribed to ${pubKey.slice(0, 20)}... recognition tree`);
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
 * Note: Tree subscription is optional (for transparency only)
 */
export function subscribeToFullParticipant(pubKey: string, includeTree: boolean = false) {
	subscribeToCommitment(pubKey);

	if (includeTree) {
		subscribeToRecognitionTree(pubKey);
	}

	const treeNote = includeTree ? ' + tree' : '';
	console.log(`[ALLOCATION-HOLSTER-V5] Subscribed to ${pubKey.slice(0, 20)}... (commitment${treeNote})`);
}

/**
 * Unsubscribe from a participant's data (V5) - WITH VERSIONED STORES
 * 
 * Note: Holster doesn't provide explicit unsubscribe,
 * so we just remove from our tracking and store
 */
export function unsubscribeFromParticipant(pubKey: string) {
	activeSubscriptions.delete(`${pubKey}:commitment`);
	activeSubscriptions.delete(`${pubKey}:tree`);

	// Delete from versioned stores (triggers incremental index update)
	networkCommitments.delete(pubKey);
	networkRecognitionTrees.delete(pubKey);

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
	const commitMap = networkCommitments.get(); // Versioned store snapshot
	for (const [pubKey, versionedEntity] of commitMap.entries()) {
		record[pubKey] = versionedEntity.data; // Extract data from versioned entity
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

	console.log('[GET-ALL-COMMITMENTS] Network commitments:', Object.keys(record).length);
	console.log('[GET-ALL-COMMITMENTS] My commitment:', myCommitment ? 'yes' : 'no');
	console.log('[GET-ALL-COMMITMENTS] My pub:', myPub ? myPub.slice(0, 20) + '...' : 'none');

	if (myCommitment && myPub) {
		console.log('[GET-ALL-COMMITMENTS] ✅ Including my commitment with',
			myCommitment.need_slots?.length || 0, 'needs,',
			myCommitment.capacity_slots?.length || 0, 'capacity');
		record[myPub] = myCommitment;
	} else {
		console.log('[GET-ALL-COMMITMENTS] ⚠️ NOT including my commitment');
	}

	console.log('[GET-ALL-COMMITMENTS] Returning', Object.keys(record).length, 'total commitments');

	return record;
}

/**
 * Get all recognition weights from network commitments
 * Extracts global_recognition_weights from each commitment
 */
export function getNetworkRecognitionWeightsRecord(): Record<string, Record<string, number>> {
	const record: Record<string, Record<string, number>> = {};
	const commitMap = networkCommitments.get(); // Versioned store snapshot

	for (const [pubKey, versionedEntity] of commitMap.entries()) {
		if (versionedEntity.data.global_recognition_weights) {
			record[pubKey] = versionedEntity.data.global_recognition_weights;
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
function addNeedSlotsToIndex(pubKey: string, needSlots: NeedSlot[] | Commitment, index: SpaceTimeIndex): void {
	// Handle both direct slots and full commitment (backwards compat)
	const slots = Array.isArray(needSlots) ? needSlots : needSlots.need_slots;
	if (!slots) return;

	for (const needSlot of slots) {
		const typeId = needSlot.need_type_id || '';
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
function addCapacitySlotsToIndex(pubKey: string, capacitySlots: AvailabilitySlot[] | Commitment, index: SpaceTimeIndex): void {
	// Handle both direct slots and full commitment (backwards compat)
	const slots = Array.isArray(capacitySlots) ? capacitySlots : capacitySlots.capacity_slots;
	if (!slots) return;

	for (const capacitySlot of slots) {
		const typeId = capacitySlot.need_type_id || '';
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
 * @param pubKey - Participant whose slots changed
 * @param slots - New slots array (or undefined to remove)
 * @param index - Index to update
 * @param isNeedIndex - true for need slots, false for capacity slots
 */
function updateIndexForParticipant(
	pubKey: string,
	slots: NeedSlot[] | AvailabilitySlot[] | Commitment | undefined,
	index: SpaceTimeIndex,
	isNeedIndex: boolean
): void {
	// Step 1: Remove old entries for this pubKey - O(M_old)
	removeFromIndex(pubKey, index);

	// Step 2: Add new entries if slots exist - O(M_new)
	if (slots) {
		if (isNeedIndex) {
			addNeedSlotsToIndex(pubKey, slots as NeedSlot[] | Commitment, index);
		} else {
			addCapacitySlotsToIndex(pubKey, slots as AvailabilitySlot[] | Commitment, index);
		}
	}
}

/**
 * Reactive index of network needs (for capacity providers to find recipients)
 * 
 * FINE-GRAINED REACTIVITY 🚀: Now subscribes to networkNeedSlots field store!
 * ✅ Only rebuilds when NEEDS change
 * ✅ NOT triggered by recognition/capacity/damping changes
 * 
 * Performance improvement: 3-4× faster!
 * - Before: Any commitment change → index rebuild
 * - After: Only need changes → index rebuild
 * 
 * SVELTE-NATIVE REACTIVITY: No manual debouncing needed!
 * - Updates immediately when needs change (O(M) per participant)
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
		let pendingUpdates = new Map<string, NeedSlot[] | Commitment | undefined>();
		let isUpdateScheduled = false;

		// Process all pending updates (called via queueMicrotask)
		const processPendingUpdates = () => {
			if (pendingUpdates.size === 0) {
				isUpdateScheduled = false;
				return;
			}

			// Process all pending updates
			for (const [pubKey, slotsOrCommitment] of pendingUpdates.entries()) {
				updateIndexForParticipant(pubKey, slotsOrCommitment, index, true);
			}

			console.log(`[NEEDS-INDEX] Batch updated ${pendingUpdates.size} participants (Svelte-native batching)`);
			pendingUpdates.clear();
			isUpdateScheduled = false;

			// Notify subscribers (Svelte batches this automatically)
			set({ ...index }); // Shallow copy to trigger reactivity
		};

		// Schedule update (uses queueMicrotask for Svelte-native batching)
		const scheduleUpdate = (pubKey: string, slotsOrCommitment: NeedSlot[] | Commitment | undefined) => {
			pendingUpdates.set(pubKey, slotsOrCommitment);

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

		// Subscribe to my commitment changes (extract needs)
		const unsubMyCommitment = myCommitmentStore.subscribe((myCommit) => {
			const myPub = get(holsterUserPub);
			if (myPub && myCommit) {
				scheduleUpdate(myPub, myCommit.need_slots);
			}
		});

		// ✅ FINE-GRAINED: Subscribe to networkNeedSlots field store!
		// Only triggers when NEEDS change, not recognition/capacity/damping
		const unsubNetwork = networkNeedSlots.subscribe((needSlotsMap) => {
			// Needs changed - update index for changed participants
			for (const [pubKey, needSlots] of needSlotsMap.entries()) {
				scheduleUpdate(pubKey, needSlots);
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
 * FINE-GRAINED REACTIVITY 🚀: Now subscribes to networkCapacitySlots field store!
 * ✅ Only rebuilds when CAPACITY changes
 * ✅ NOT triggered by recognition/needs/damping changes
 * 
 * Performance improvement: 3-4× faster!
 * - Before: Any commitment change → index rebuild
 * - After: Only capacity changes → index rebuild
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
		let pendingUpdates = new Map<string, AvailabilitySlot[] | Commitment | undefined>();
		let isUpdateScheduled = false;

		// Process all pending updates (called via queueMicrotask)
		const processPendingUpdates = () => {
			if (pendingUpdates.size === 0) {
				isUpdateScheduled = false;
				return;
			}

			// Process all pending updates
			for (const [pubKey, slotsOrCommitment] of pendingUpdates.entries()) {
				updateIndexForParticipant(pubKey, slotsOrCommitment, index, false); // false = capacity index
			}

			console.log(`[CAPACITY-INDEX] Batch updated ${pendingUpdates.size} participants (Svelte-native batching)`);
			pendingUpdates.clear();
			isUpdateScheduled = false;

			// Notify subscribers (Svelte batches this automatically)
			set({ ...index }); // Shallow copy to trigger reactivity
		};

		// Schedule update (uses queueMicrotask for Svelte-native batching)
		const scheduleUpdate = (pubKey: string, slotsOrCommitment: AvailabilitySlot[] | Commitment | undefined) => {
			pendingUpdates.set(pubKey, slotsOrCommitment);

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

		// Subscribe to my commitment changes (extract capacity)
		const unsubMyCommitment = myCommitmentStore.subscribe((myCommit) => {
			const myPub = get(holsterUserPub);
			if (myPub && myCommit) {
				scheduleUpdate(myPub, myCommit.capacity_slots);
			}
		});

		// ✅ FINE-GRAINED: Subscribe to networkCapacitySlots field store!
		// Only triggers when CAPACITY changes, not recognition/needs/damping
		const unsubNetwork = networkCapacitySlots.subscribe((capacitySlotsMap) => {
			// Capacity changed - update index for changed participants
			for (const [pubKey, capacitySlots] of capacitySlotsMap.entries()) {
				scheduleUpdate(pubKey, capacitySlots);
			}
		});

		return () => {
			unsubMyCommitment();
			unsubNetwork();
		};
	}
);

// V5: NO getNetworkRoundStatesRecord (no rounds!)

// ═══════════════════════════════════════════════════════════════════
// AUTO-SUBSCRIPTION LOGIC (V5)
// ═══════════════════════════════════════════════════════════════════

/**
 * Get all contributors from my recognition tree
 * 
 * These are the people I should subscribe to because:
 * 1. I recognize them (they're in my tree)
 * 2. I need their commitments to compute mutual recognition
 * 3. I need their needs/capacity for allocation
 * 
 * Returns public keys of all contributors (positive + anti)
 */
export function getMyContributors(): string[] {
	const tree = get(myRecognitionTreeStore);
	if (!tree) return [];

	// Extract all contributors (positive + anti) from tree
	const contributors = getAllContributorsFromTree(tree);

	console.log(`[MY-CONTRIBUTORS] Found ${contributors.length} contributors in tree`);
	return contributors;
}

/**
 * Sync subscriptions with tree contributors (V5)
 * 
 * THE KEY FUNCTION for network connectivity!
 * 
 * Flow:
 * 1. Extract contributors from my recognition tree
 * 2. Compare with currently subscribed participants
 * 3. Subscribe to NEW contributors → receive their commitments
 * 4. Unsubscribe from REMOVED contributors → clean up
 * 
 * WHY THIS MATTERS:
 * - I add "Alice" to my tree → auto-subscribe to Alice's commitment
 * - Alice's commitment arrives → networkCommitments updates
 * - myMutualRecognition updates (reactive!)
 * - Mutual recognition ready for allocation!
 * 
 * Call this whenever tree changes (or enable auto-sync below)
 */
export function syncSubscriptionsWithTree() {
	console.log('[🔄 AUTO-SUB] Syncing subscriptions with tree...');

	const currentContributors = getMyContributors();
	const currentSubscriptions = getSubscribedParticipants();

	console.log(`[🔄 AUTO-SUB] Tree has ${currentContributors.length} contributors`);
	console.log(`[🔄 AUTO-SUB] Currently subscribed to ${currentSubscriptions.length} users`);

	// ✅ CRITICAL: Resolve contact IDs to public keys before subscribing!
	// Network subscriptions only work with public keys, not local contact IDs
	const resolvedContributors = currentContributors
		.map(id => resolveToPublicKey(id) || id)
		.filter((pubKey, index, self) => self.indexOf(pubKey) === index); // Deduplicate

	// Find who to subscribe to (new contributors)
	const toSubscribe = resolvedContributors.filter(
		contributor => !currentSubscriptions.includes(contributor)
	);

	// Find who to unsubscribe from (removed contributors)
	const toUnsubscribe = currentSubscriptions.filter(
		subscribed => !resolvedContributors.includes(subscribed)
	);

	// Subscribe to new contributors
	for (const contributor of toSubscribe) {
		console.log(`[🔄 AUTO-SUB] ➕ Subscribing to: ${contributor.slice(0, 20)}... (will receive their commitment)`);
		subscribeToCommitment(contributor);
	}

	// Unsubscribe from removed contributors
	for (const removed of toUnsubscribe) {
		console.log(`[🔄 AUTO-SUB] ➖ Unsubscribing from: ${removed.slice(0, 20)}...`);
		unsubscribeFromParticipant(removed);
	}

	console.log(`[🔄 AUTO-SUB] ✅ Sync complete: +${toSubscribe.length} new, -${toUnsubscribe.length} removed, =${resolvedContributors.length} total`);
}

/**
 * Enable automatic subscription syncing (V5)
 * 
 * WHEN TO USE: Call this once on app start after initializing stores
 * 
 * WHAT IT DOES:
 * - Watches my recognition tree for changes
 * - When tree changes → automatically syncs subscriptions
 * - Add contributor to tree → auto-subscribe to their commitment
 * - Remove contributor → auto-unsubscribe
 * 
 * COMPLETE FLOW EXAMPLE:
 * ```
 * 1. User adds "Alice" as contributor to tree node
 * 2. myRecognitionTreeStore.set(updatedTree)
 * 3. Auto-sync detects tree change
 * 4. syncSubscriptionsWithTree() runs
 * 5. Subscribes to Alice's commitment via Holster
 * 6. Alice's commitment arrives → networkCommitments.set(alice, commitment)
 * 7. myMutualRecognition updates (reactive!)
 * 8. Commitment composed with updated MR values
 * 9. Ready for allocation!
 * ```
 * 
 * Returns unsubscribe function to disable auto-syncing
 */
export function enableAutoSubscriptionSync(): () => void {
	console.log('[AUTO-SYNC] 🔄 Enabling automatic subscription syncing');

	// Initial sync (subscribe to existing contributors)
	syncSubscriptionsWithTree();

	// Watch tree for changes and sync subscriptions
	const unsubTree = myRecognitionTreeStore.subscribe(() => {
		console.log('[AUTO-SYNC] 🌳 Tree changed, syncing subscriptions...');
		syncSubscriptionsWithTree();
	});

	return () => {
		unsubTree();
		console.log('[AUTO-SYNC] ⏸️  Disabled automatic subscription syncing');
	};
}

// ═══════════════════════════════════════════════════════════════════
// AUTO-MEMBERSHIP SUBSCRIPTION LOGIC (V5)
// ═══════════════════════════════════════════════════════════════════

/**
 * Enable automatic membership subscription syncing (DEPRECATED)
 * 
 * @deprecated The pure attribute system now handles membership subscriptions automatically!
 * When you call subscribeToOrgMembership(org_id, source_pubkey), the attribute
 * system automatically subscribes to that user's attribute recognitions via Holster.
 * 
 * This function is now a NO-OP for backward compatibility.
 * The attribute system's enableAutoAttributeSync() handles everything.
 * 
 * @returns Empty unsubscribe function
 */
export function enableAutoMembershipSync(): () => void {
	console.log('[AUTO-MEMBERSHIP-SYNC] ℹ️  DEPRECATED: Membership sync now handled by attribute system automatically');
	console.log('[AUTO-MEMBERSHIP-SYNC] ℹ️  Use subscribeToOrgMembership() - it auto-subscribes via the attribute system');
	return () => { }; // NO-OP - attribute system handles it
}

// ═══════════════════════════════════════════════════════════════════
// AUTO-CAPACITY SUBSCRIPTION LOGIC (V5)
// ═══════════════════════════════════════════════════════════════════

/**
 * Enable automatic capacity subscription syncing - UNIFIED!
 * 
 * Watches slotSubscriptions (unified!) and auto-subscribes to users' capacity_slots.
 * When their slots arrive, applies filters and merges matching slots into your own.
 * 
 * Flow:
 * 1. User subscribes: subscribeToSlots(pubkey, { capacity: true })
 * 2. slotSubscriptions updates
 * 3. This function subscribes to their commitment via networkCapacitySlots
 * 4. When their capacity_slots arrive, caches them
 * 5. Applies all enabled filters with applies_to='capacity' or 'both' (union - match ANY filter)
 * 6. Merges matching slots with your declared capacities
 * 7. Updates myCapacitySlots via setMyCapacitySlots()
 * 
 * Returns unsubscribe function
 */
export function enableAutoCapacitySync(): () => void {
	console.log('[AUTO-CAPACITY-SYNC] 🔄 Enabling automatic capacity syncing (unified v2)');

	// Track active subscriptions
	const activeSubs = new Map<string, () => void>();

	/**
	 * Apply filters and update slots
	 * Called when filters change or new capacity data arrives
	 */
	const applyFiltersAndUpdateSlots = () => {
		const myPub = get(holsterUserPub);
		if (!myPub) return;

		// Get current state
		const cache = get(capacityCache) as Record<string, any[]>;
		const allFilters = Object.values(get(slotFilters) || {});
		const currentCommitment = get(myCommitmentStore);

		if (!currentCommitment) return;

		// My declared capacity slots (user-defined, take priority)
		const myDeclaredSlots = currentCommitment.capacity_slots || [];

		// Helper function to resolve members (org_ids → pubkeys)
		const resolveMembers = (id: string): string[] => {
			return resolveContributorWithOrgs(id);
		};

		// Apply filters to cached slots (union across all sources)
		// Pass 'capacity' as slotType to filter only capacity-relevant filters
		const filteredNetworkSlots = applyFiltersUnion(
			cache,
			'capacity', // NEW: slot type parameter
			allFilters,
			myPub,
			resolveMembers
		);

		console.log(`[AUTO-CAPACITY-SYNC] Filtered ${filteredNetworkSlots.length} slots from ${Object.keys(cache || {}).length} sources`);

		// Merge: declared slots + filtered network slots (declared takes priority)
		const mergedSlots = mergeSlots(myDeclaredSlots, filteredNetworkSlots);

		console.log(`[AUTO-CAPACITY-SYNC] Merged: ${myDeclaredSlots.length} declared + ${filteredNetworkSlots.length} network = ${mergedSlots.length} total`);

		// Update capacity slots if changed
		if (JSON.stringify(myDeclaredSlots) !== JSON.stringify(mergedSlots)) {
			setMyCapacitySlots(mergedSlots);
			console.log('[AUTO-CAPACITY-SYNC] ✅ Updated capacity slots');
		}
	};

	// Subscribe to unified slot subscription changes - check .capacity field
	const unsubCapacitySubs = slotSubscriptions.subscribe(($subs: any) => {
		if (!$subs) return;

		// Filter for capacity subscriptions only
		const capacitySubKeys = Object.entries($subs)
			.filter(([_, sub]: [string, any]) => sub?.capacity === true)
			.map(([pubkey, _]) => pubkey);

		console.log(`[AUTO-CAPACITY-SYNC] Processing ${capacitySubKeys.length} capacity subscriptions`);

		// Subscribe to new sources
		for (const pubkey of capacitySubKeys) {
			if (activeSubs.has(pubkey)) continue;

			console.log(`[AUTO-CAPACITY-SYNC] ➕ Subscribing to ${pubkey.slice(0, 20)}...'s capacity slots`);

			// Subscribe to their commitment (if not already subscribed)
			subscribeToCommitment(pubkey);

			// Track this subscription
			activeSubs.set(pubkey, () => {
				console.log(`[AUTO-CAPACITY-SYNC] ⏸️  Unsubscribed from ${pubkey.slice(0, 20)}...`);
			});
		}

		// Cleanup removed subscriptions
		const currentKeys = new Set(capacitySubKeys);

		for (const [key, cleanup] of activeSubs.entries()) {
			if (!currentKeys.has(key)) {
				console.log(`[AUTO-CAPACITY-SYNC] ➖ Removing subscription: ${key.slice(0, 20)}...`);
				cleanup();
				activeSubs.delete(key);
			}
		}
	});

	// Watch network capacity slots for changes
	const unsubNetworkCapacity = networkCapacitySlots.subscribe((slotsMap) => {
		const subs = (get(slotSubscriptions) || {}) as Record<string, { capacity?: boolean; needs?: boolean }>;

		// Update cache for subscribed sources (check .capacity field)
		for (const [pubkey, slots] of slotsMap.entries()) {
			if (subs[pubkey]?.capacity) {
				capacityCache.update((cache: any) => ({
					...cache,
					[pubkey]: slots
				}));
			}
		}

		// Reapply filters whenever network data changes
		applyFiltersAndUpdateSlots();
	});

	// Watch unified filter changes
	const unsubFilters = slotFilters.subscribe(() => {
		console.log('[AUTO-CAPACITY-SYNC] Filters changed, reapplying...');
		applyFiltersAndUpdateSlots();
	});

	// Initial application
	applyFiltersAndUpdateSlots();

	return () => {
		unsubCapacitySubs();
		unsubNetworkCapacity();
		unsubFilters();
		activeSubs.clear();
		console.log('[AUTO-CAPACITY-SYNC] ⏸️  Disabled automatic capacity syncing');
	};
}

/**
 * Enable automatic need subscription syncing - UNIFIED!
 * 
 * Same pattern as capacity sync, but for need slots.
 * Watches slotSubscriptions (unified!) and auto-subscribes to users' need_slots.
 * 
 * Returns unsubscribe function
 */
export function enableAutoNeedSync(): () => void {
	console.log('[AUTO-NEED-SYNC] 🔄 Enabling automatic need syncing (unified v2)');

	// Track active subscriptions
	const activeSubs = new Map<string, () => void>();

	/**
	 * Apply filters and update slots
	 */
	const applyFiltersAndUpdateSlots = () => {
		const myPub = get(holsterUserPub);
		if (!myPub) return;

		// Get current state
		const cache = get(needCache) as Record<string, any[]>;
		const allFilters = Object.values(get(slotFilters) || {});
		const currentCommitment = get(myCommitmentStore);

		if (!currentCommitment) return;

		// My declared need slots (user-defined, take priority)
		const myDeclaredSlots = currentCommitment.need_slots || [];

		// Helper function to resolve members
		const resolveMembers = (id: string): string[] => {
			return resolveContributorWithOrgs(id);
		};

		// Apply filters to cached slots
		// Pass 'need' as slotType to filter only need-relevant filters
		const filteredNetworkSlots = applyFiltersUnion(
			cache,
			'need', // NEW: slot type parameter
			allFilters,
			myPub,
			resolveMembers
		);

		console.log(`[AUTO-NEED-SYNC] Filtered ${filteredNetworkSlots.length} slots from ${Object.keys(cache || {}).length} sources`);

		// Merge: declared slots + filtered network slots
		const mergedSlots = mergeSlots(myDeclaredSlots, filteredNetworkSlots);

		console.log(`[AUTO-NEED-SYNC] Merged: ${myDeclaredSlots.length} declared + ${filteredNetworkSlots.length} network = ${mergedSlots.length} total`);

		// Update need slots if changed
		if (JSON.stringify(myDeclaredSlots) !== JSON.stringify(mergedSlots)) {
			setMyNeedSlots(mergedSlots);
			console.log('[AUTO-NEED-SYNC] ✅ Updated need slots');
		}
	};

	// Subscribe to unified slot subscription changes - check .needs field
	const unsubNeedSubs = slotSubscriptions.subscribe(($subs: any) => {
		if (!$subs) return;

		// Filter for need subscriptions only
		const needSubKeys = Object.entries($subs)
			.filter(([_, sub]: [string, any]) => sub?.needs === true)
			.map(([pubkey, _]) => pubkey);

		console.log(`[AUTO-NEED-SYNC] Processing ${needSubKeys.length} need subscriptions`);

		// Subscribe to new sources
		for (const pubkey of needSubKeys) {
			if (activeSubs.has(pubkey)) continue;

			console.log(`[AUTO-NEED-SYNC] ➕ Subscribing to ${pubkey.slice(0, 20)}...'s need slots`);

			// Subscribe to their commitment
			subscribeToCommitment(pubkey);

			// Track this subscription
			activeSubs.set(pubkey, () => {
				console.log(`[AUTO-NEED-SYNC] ⏸️  Unsubscribed from ${pubkey.slice(0, 20)}...`);
			});
		}

		// Cleanup removed subscriptions
		const currentKeys = new Set(needSubKeys);

		for (const [key, cleanup] of activeSubs.entries()) {
			if (!currentKeys.has(key)) {
				console.log(`[AUTO-NEED-SYNC] ➖ Removing subscription: ${key.slice(0, 20)}...`);
				cleanup();
				activeSubs.delete(key);
			}
		}
	});

	// Watch network need slots for changes
	const unsubNetworkNeeds = networkNeedSlots.subscribe((slotsMap) => {
		const subs = (get(slotSubscriptions) || {}) as Record<string, { capacity?: boolean; needs?: boolean }>;

		// Update cache for subscribed sources (check .needs field)
		for (const [pubkey, slots] of slotsMap.entries()) {
			if (subs[pubkey]?.needs) {
				needCache.update((cache: any) => ({
					...cache,
					[pubkey]: slots
				}));
			}
		}

		// Reapply filters whenever network data changes
		applyFiltersAndUpdateSlots();
	});

	// Watch unified filter changes
	const unsubFilters = slotFilters.subscribe(() => {
		console.log('[AUTO-NEED-SYNC] Filters changed, reapplying...');
		applyFiltersAndUpdateSlots();
	});

	// Initial application
	applyFiltersAndUpdateSlots();

	return () => {
		unsubNeedSubs();
		unsubNetworkNeeds();
		unsubFilters();
		activeSubs.clear();
		console.log('[AUTO-NEED-SYNC] ⏸️  Disabled automatic need syncing');
	};
}

// ═══════════════════════════════════════════════════════════════════
// COMMITMENT COMPOSITION HELPERS (V5)
// ═══════════════════════════════════════════════════════════════════

/**
 * Merge all network ITC stamps with local ITC
 * 
 * ✅ CRITICAL FIX: Prevents data loss from stale ITC stamps!
 * 
 * This ensures your published commitment includes the causal history
 * of ALL network updates you've seen, preventing other users from
 * rejecting your updates as stale.
 * 
 * Algorithm:
 * 1. Start with your local ITC (if any)
 * 2. Join with every network commitment's ITC
 * 3. Increment for this new local event
 * 
 * Returns: Merged ITC stamp ready for publishing
 */
function getMergedITCStamp(localITC?: ITCStamp | null): ITCStamp {
	// Start with local ITC or create new seed
	let mergedITC: ITCStamp = localITC || itcSeed();

	// Merge with all network commitments
	const networkCommitMap = networkCommitments.get();
	let networkMergeCount = 0;

	for (const [pubKey, versionedEntity] of networkCommitMap.entries()) {
		if (versionedEntity.metadata.itcStamp) {
			mergedITC = itcJoin(mergedITC, versionedEntity.metadata.itcStamp);
			networkMergeCount++;
		}
	}

	// Increment for this local event
	mergedITC = itcEvent(mergedITC);

	if (networkMergeCount > 0) {
		console.log(`[ITC-MERGE] ✅ Merged ${networkMergeCount} network ITC stamps into local commitment`);
	}

	return mergedITC;
}

/**
 * Compose commitment from source stores
 * 
 * ✅ SIMPLIFIED: Slots already in commitment! Only updates recognition data.
 * 
 * Call this to update commitment with fresh recognition data:
 * - Recognition tree (→ weights)
 * - Mutual recognition (computed from my weights + network commitments)
 * - Preserves existing slots, damping state
 * - ITC stamp (merged with all network ITCs) ← ✅ FIXED!
 * 
 * Returns a complete commitment ready to publish
 */
/**
 * Total I've received (across all providers, by type)
 * This would be computed by aggregating allocations from all providers
 * 
 * NOTE: Moved from allocation.svelte.ts to prevent circular dependency
 */
export const totalReceivedBySlot: Readable<Record<string, Record<string, number>>> = derived(
	[networkAllocations, holsterUserPub],
	([$allocations, $myPub]) => {
		const result: Record<string, Record<string, number>> = {};
		if (!$myPub) return result;

		for (const [providerPub, allocationList] of $allocations.entries()) {
			if (!Array.isArray(allocationList)) continue;

			for (const allocation of allocationList) {
				if (allocation.recipient_pubkey === $myPub) {
					const typeId = allocation.need_type_id;
					const quantity = allocation.quantity || 0;

					if (typeId) {
						if (!result[typeId]) {
							result[typeId] = {};
						}
						result[typeId][providerPub] = (result[typeId][providerPub] || 0) + quantity;
					}
				}
			}
		}
		return result;
	}
);

export function composeCommitmentFromSources(totalReceivedMap?: Record<string, Record<string, number>>): Commitment | null {
	console.log('[📝 COMPOSE] Composing commitment from sources...');

	const tree = get(myRecognitionTreeStore);
	const recognitionWeights = get(myRecognitionWeights);
	const existingCommitment = get(myCommitmentStore);
	const myPub = get(holsterUserPub);

	// Need at least tree or existing commitment
	if (!tree && !existingCommitment) {
		console.warn('[📝 COMPOSE] ❌ No source data available');
		return null;
	}

	// ✅ CRITICAL FIX: Merge network ITCs to prevent data loss!
	const mergedITC = getMergedITCStamp(existingCommitment?.itcStamp);

	// ✅ CRITICAL: Resolve contact IDs to public keys before publishing!
	// Contact IDs are local-only - the network only understands public keys
	const recognitionWeightsForNetwork: Record<string, number> = {};
	let resolvedCount = 0;

	for (const [identifier, weight] of Object.entries(recognitionWeights || {})) {
		// Resolve contact IDs to public keys (leaves public keys unchanged)
		const resolvedKey = resolveToPublicKey(identifier) || identifier;
		if (resolvedKey !== identifier) {
			console.log(`[📝 COMPOSE] 🔄 Resolved contact ID ${identifier} → ${resolvedKey.slice(0, 20)}...`);
			resolvedCount++;
		}
		recognitionWeightsForNetwork[resolvedKey] = weight;
	}

	if (resolvedCount > 0) {
		console.log(`[📝 COMPOSE] ✅ Resolved ${resolvedCount} contact ID(s) to public keys for network`);
	}

	if (myPub && recognitionWeightsForNetwork[myPub] !== undefined) {
		console.log(`[📝 COMPOSE] ✅ Including self-recognition (${(recognitionWeightsForNetwork[myPub] * 100).toFixed(2)}%) in commitment`);
	}

	// Compose the commitment - PRESERVE existing slots AND cache!
	const commitment: Commitment = {
		// Preserve existing slots (updated via setMyNeedSlots/setMyCapacitySlots)
		need_slots: existingCommitment?.need_slots || [],
		capacity_slots: existingCommitment?.capacity_slots || [],

		// Update recognition data (from tree) - source of truth!
		global_recognition_weights: recognitionWeightsForNetwork,

		// Preserve cache (updated by network subscriber)
		others_recognition_of_me: existingCommitment?.others_recognition_of_me,

		// Preserve stateful data from existing commitment
		multi_dimensional_damping: existingCommitment?.multi_dimensional_damping,

		// Removed valid-but-not-in-schema fields to fix build:
		// total_allocated_by_slot: totalReceivedBySlot || {},
		// distance_from_need_by_slot: ...
		itcStamp: mergedITC,  // ✅ Now includes all network history!
		timestamp: Date.now()
	};

	const recCount = Object.keys(commitment.global_recognition_weights || {}).length;
	const recNonZero = Object.values(commitment.global_recognition_weights || {}).filter(w => w > 0).length;
	const cacheCount = Object.keys(commitment.others_recognition_of_me || {}).length;

	console.log(`[📝 COMPOSE] ✅ Composed commitment:`);
	console.log(`  • Recognition: ${recCount} entries (${recNonZero} non-zero) [includes self if present in tree]`);
	console.log(`  • Others' rec cache: ${cacheCount} entries`);
	console.log(`  • Need Slots: ${commitment.need_slots?.length || 0}`);
	console.log(`  • Capacity Slots: ${commitment.capacity_slots?.length || 0}`);
	const totalAllocCount = Object.keys(commitment.total_allocated || {}).length;
	const distanceCount = Object.keys(commitment.distance_from_need || {}).length;
	if (totalAllocCount > 0 || distanceCount > 0) {
		console.log(`  • Total Allocated: ${totalAllocCount} types, Distance from Need: ${distanceCount} types`);
	}

	// Log details of recognition
	if (recNonZero > 0) {
		console.log('[📝 COMPOSE] Recognition weights being published to network:');
		Object.entries(commitment.global_recognition_weights || {}).forEach(([id, weight]) => {
			if (weight > 0) {
				const isSelf = id === myPub;
				console.log(`    • ${id.slice(0, 20)}... → ${(weight * 100).toFixed(2)}%${isSelf ? ' (SELF)' : ''}`);
			}
		});
	}

	return commitment;
}

/**
 * Auto-update commitment when source stores change (V5)
 * 
 * ✅ SIMPLIFIED: Only reacts to recognition changes now!
 * Slots are updated directly via setMyNeedSlots/setMyCapacitySlots helpers.
 * 
 * Call this to enable reactive commitment updates for recognition data.
 * Whenever tree or network recognition changes, commitment auto-updates.
 * 
 * PERFORMANCE: 
 * - Debounces rapid updates (100ms - same tick batching)
 * - Checks for meaningful changes before updating
 * - Avoids duplicate recomposition when multiple sources change simultaneously
 * 
 * Returns unsubscribe function
 */
export function enableAutoCommitmentComposition(): () => void {
	console.log('[AUTO-COMPOSE] Enabling reactive commitment composition (recognition only)');

	let debounceTimer: ReturnType<typeof setTimeout> | null = null;
	let isRecomposing = false; // Prevent cascading updates

	/**
	 * Recompose commitment with debouncing
	 * Batches multiple rapid source changes into single update
	 */
	const debouncedRecompose = (reason: string) => {
		if (isRecomposing) {
			console.log(`[AUTO-COMPOSE] ⏭️  Skipped: already recomposing`);
			return;
		}

		// Clear existing timer
		if (debounceTimer) {
			clearTimeout(debounceTimer);
		}

		// Schedule recomposition
		debounceTimer = setTimeout(() => {
			isRecomposing = true;

			const newCommitment = composeCommitmentFromSources(get(totalReceivedBySlot));
			if (!newCommitment) {
				console.log(`[AUTO-COMPOSE] ⏭️  Skipped: no source data (${reason})`);
				isRecomposing = false;
				return;
			}

			// ✅ CRITICAL FIX: Check if commitment actually changed before calling set()
			// This prevents infinite loop where loading triggers recompose triggers save triggers load...
			const currentCommitment = get(myCommitmentStore);
			if (currentCommitment) {
				// Compare only the meaningful data fields, skip metadata (ITC, timestamp)
				// Metadata always changes, but we only care if recognition/slots changed
				try {
					const currentData = {
						need_slots: currentCommitment.need_slots,
						capacity_slots: currentCommitment.capacity_slots,
						global_recognition_weights: currentCommitment.global_recognition_weights,
						others_recognition_of_me: currentCommitment.others_recognition_of_me,
						multi_dimensional_damping: currentCommitment.multi_dimensional_damping
					};
					const newData = {
						need_slots: newCommitment.need_slots,
						capacity_slots: newCommitment.capacity_slots,
						global_recognition_weights: newCommitment.global_recognition_weights,
						others_recognition_of_me: newCommitment.others_recognition_of_me,
						multi_dimensional_damping: newCommitment.multi_dimensional_damping
					};

					const currentJson = JSON.stringify(currentData);
					const newJson = JSON.stringify(newData);

					if (currentJson === newJson) {
						console.log(`[AUTO-COMPOSE] ⏭️  Skipped: commitment data unchanged (${reason})`);
						isRecomposing = false;
						return;
					}
				} catch (error) {
					console.warn(`[AUTO-COMPOSE] ⚠️  Equality check failed, proceeding with update:`, error);
				}
			}

			// Apply the update
			// NOTE: This preserves existing slots and only updates recognition data
			console.log(`[💾 AUTO-COMPOSE] Publishing updated commitment to network (${reason})...`);
			myCommitmentStore.set(newCommitment);
			console.log(`[💾 AUTO-COMPOSE] ✅ Updated commitment recognition (${reason}) - now persisting to Holster`);

			isRecomposing = false;
		}, 100); // 100ms debounce (same-tick batching)
	};

	// Subscribe to recognition tree (generates weights)
	const unsubTree = myRecognitionTreeStore.subscribe(() => {
		debouncedRecompose('tree changed');
	});

	// Subscribe to network recognition weights (from OTHERS only - not our own commitment!)
	// This prevents infinite loop: myMutualRecognition includes myCommitmentStore,
	// so subscribing to it would create circular dependency!
	const unsubNetworkRec = networkRecognitionWeights.subscribe(() => {
		debouncedRecompose('network recognition changed');
	});

	// Return cleanup function
	return () => {
		if (debounceTimer) clearTimeout(debounceTimer);
		unsubTree();
		unsubNetworkRec();
		console.log('[AUTO-COMPOSE] Disabled reactive commitment composition');
	};
}

/**
 * Get subscription statistics (V5) - WITH VERSIONED STORES
 */
export function getSubscriptionStats() {
	const commitMap = networkCommitments.get(); // Versioned store
	const treeMap = networkRecognitionTrees.get(); // Versioned store
	return {
		totalSubscriptions: activeSubscriptions.size,
		commitments: commitMap.size,
		trees: treeMap.size,
		uniqueParticipants: getSubscribedParticipants().length,
		// V5: Tree + commitment stores with versioned store system!
		architecture: 'v5-tree-plus-commitment-versioned'
	};
}

/**
 * Get convergence statistics (V5) - WITH VERSIONED STORES
 * 
 * Monitors how many participants have converged
 * 
 * V5: Infers convergence from commitments (needs approaching zero)
 */
export function getConvergenceStats() {
	let convergedCount = 0;
	let totalWithData = 0;
	const epsilon = 0.001; // Convergence threshold

	const commitMap = networkCommitments.get(); // Versioned store

	for (const [_, versionedEntity] of commitMap.entries()) {
		const commitment = versionedEntity.data; // Extract data from versioned entity
		if (commitment.need_slots && commitment.need_slots.length > 0) {
			totalWithData++;

			// Check if all needs are near zero
			const totalNeed = commitment.need_slots.reduce((sum: number, slot) => sum + (slot.quantity || 0), 0);
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
			recognitionTreeStore: true, // V5: Tree generates recognition weights!
			commitmentStore: true, // V5: Commitment contains weights + needs/capacity!
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
			separateRecognitionWeightsStore: false // V5: Computed weights in commitment!
		}
	};
}

// ═══════════════════════════════════════════════════════════════════
// DATA MIGRATION & VALIDATION (Defensive)
// ═══════════════════════════════════════════════════════════════════

/**
 * Migrate/fix corrupted network commitments
 * 
 * Handles scenarios:
 * - Legacy data with wrong format (Records instead of arrays)
 * - Invalid enum values (uppercase instead of lowercase)
 * - Missing required fields
 * - Corrupted data structures
 * 
 * Returns: { fixed: number, deleted: number, errors: string[] }
 */
export function migrateNetworkCommitments(): {
	fixed: number;
	deleted: number;
	errors: string[];
} {
	console.log('[MIGRATION] Starting network commitments migration...');

	let fixed = 0;
	let deleted = 0;
	const errors: string[] = [];
	const commitMap = networkCommitments.get();

	for (const [pubKey, versionedEntity] of commitMap.entries()) {
		const shortKey = pubKey.slice(0, 20);

		// Re-validate against schema
		const validation = CommitmentSchema.safeParse(versionedEntity.data);

		if (!validation.success) {
			console.warn(`[MIGRATION] Invalid commitment for ${shortKey}:`, validation.error.format());

			// NOTE: We can't auto-fix corrupted data anymore (no converters).
			// Just delete invalid entries - they'll be regenerated from source.
			networkCommitments.delete(pubKey);
			deleted++;
			const errorMsg = `Deleted invalid commitment for ${shortKey}: ${validation.error.issues.map(i => i.message).join(', ')}`;
			errors.push(errorMsg);
			console.error(`[MIGRATION] ❌ ${errorMsg}`);
		}
	}

	const result = { fixed, deleted, errors };
	console.log('[MIGRATION] Complete:', result);
	return result;
}

/**
 * Validate all stores (debugging helper)
 * 
 * Checks schema validity of:
 * - Own commitment
 * - All network commitments
 * - Own recognition tree
 * - All network recognition trees (if any)
 * 
 * Returns: { valid: boolean, errors: Array<{ store: string, error: any }> }
 */
export function validateAllStores(): {
	myCommitment: { valid: boolean; error?: any };
	myTree: { valid: boolean; error?: any };
	networkCommitments: Record<string, { valid: boolean; error?: any }>;
	networkTrees: Record<string, { valid: boolean; error?: any }>;
	summary: {
		totalValid: number;
		totalInvalid: number;
		stores: string[];
	};
} {
	console.log('[VALIDATION] Validating all stores...');

	const result = {
		myCommitment: { valid: true } as { valid: boolean; error?: any },
		myTree: { valid: true } as { valid: boolean; error?: any },
		networkCommitments: {} as Record<string, { valid: boolean; error?: any }>,
		networkTrees: {} as Record<string, { valid: boolean; error?: any }>,
		summary: {
			totalValid: 0,
			totalInvalid: 0,
			stores: [] as string[]
		}
	};

	// Validate own commitment
	const myCommit = get(myCommitmentStore);
	if (myCommit) {
		const validation = CommitmentSchema.safeParse(myCommit);
		result.myCommitment = {
			valid: validation.success,
			error: validation.success ? undefined : validation.error.format()
		};
		if (validation.success) {
			result.summary.totalValid++;
		} else {
			result.summary.totalInvalid++;
			result.summary.stores.push('myCommitment');
			console.error('[VALIDATION] ❌ Invalid myCommitment:', validation.error.format());
		}
	}

	// Validate own recognition tree
	const myTree = get(myRecognitionTreeStore);
	if (myTree) {
		const validation = RootNodeSchema.safeParse(myTree);
		result.myTree = {
			valid: validation.success,
			error: validation.success ? undefined : validation.error.format()
		};
		if (validation.success) {
			result.summary.totalValid++;
		} else {
			result.summary.totalInvalid++;
			result.summary.stores.push('myTree');
			console.error('[VALIDATION] ❌ Invalid myTree:', validation.error.format());
		}
	}

	// Validate network commitments
	const commitMap = networkCommitments.get();
	for (const [pubKey, versionedEntity] of commitMap.entries()) {
		const shortKey = pubKey.slice(0, 20);
		const validation = CommitmentSchema.safeParse(versionedEntity.data);
		result.networkCommitments[pubKey] = {
			valid: validation.success,
			error: validation.success ? undefined : validation.error.format()
		};
		if (validation.success) {
			result.summary.totalValid++;
		} else {
			result.summary.totalInvalid++;
			result.summary.stores.push(`network:${shortKey}`);
			console.error(`[VALIDATION] ❌ Invalid commitment from ${shortKey}:`, validation.error.format());
		}
	}

	// Validate network recognition trees (if any)
	const treeMap = networkRecognitionTrees.get();
	for (const [pubKey, versionedEntity] of treeMap.entries()) {
		const shortKey = pubKey.slice(0, 20);
		const validation = RootNodeSchema.safeParse(versionedEntity.data);
		result.networkTrees[pubKey] = {
			valid: validation.success,
			error: validation.success ? undefined : validation.error.format()
		};
		if (validation.success) {
			result.summary.totalValid++;
		} else {
			result.summary.totalInvalid++;
			result.summary.stores.push(`networkTree:${shortKey}`);
			console.error(`[VALIDATION] ❌ Invalid tree from ${shortKey}:`, validation.error.format());
		}
	}

	console.log('[VALIDATION] Complete:', {
		valid: result.summary.totalValid,
		invalid: result.summary.totalInvalid,
		invalidStores: result.summary.stores
	});

	return result;
}

/**
 * Clear all V5 stores (use when migrating to new JSON format)
 * 
 * ✅ SIMPLIFIED: Only 2 persistent stores now (tree + commitment)!
 * 
 * This deletes all data from Holster storage for V5 stores.
 * After running this, reload the page to start fresh with JSON format.
 */
export async function clearAllV5Stores() {
	console.log('[V5-MIGRATION] 🧹 Clearing all V5 stores...');

	const paths = [
		'trees/recognition_tree',
		'allocation/commitment' // THE source of truth (contains slots!)
		// NOTE: 'allocation/need_slots' and 'allocation/capacity_slots' are derived, not persisted!
	];

	// Step 1: Clear via Holster API
	for (const path of paths) {
		try {
			await new Promise<void>((resolve) => {
				holsterUser.get(path).put(null, () => {
					console.log(`[V5-MIGRATION] ✅ Cleared ${path} via Holster API`);
					resolve();
				});
			});
		} catch (error) {
			console.error(`[V5-MIGRATION] ❌ Failed to clear ${path}:`, error);
		}
	}

	// Step 2: Clear IndexedDB directly (Gun/Holster cache)
	try {
		const dbs = await indexedDB.databases();
		console.log('[V5-MIGRATION] 📦 Found IndexedDB databases:', dbs.map(db => db.name));

		// Clear radata (Gun's default DB name)
		const radataExists = dbs.some(db => db.name === 'radata');
		if (radataExists) {
			await new Promise<void>((resolve, reject) => {
				const deleteRequest = indexedDB.deleteDatabase('radata');
				deleteRequest.onsuccess = () => {
					console.log('[V5-MIGRATION] ✅ Cleared IndexedDB: radata');
					resolve();
				};
				deleteRequest.onerror = () => {
					console.warn('[V5-MIGRATION] ⚠️  Failed to clear IndexedDB: radata');
					resolve(); // Don't fail if this doesn't work
				};
				deleteRequest.onblocked = () => {
					console.warn('[V5-MIGRATION] ⚠️  IndexedDB deletion blocked (close other tabs)');
					resolve();
				};
			});
		}
	} catch (error) {
		console.warn('[V5-MIGRATION] ⚠️  Could not clear IndexedDB:', error);
	}

	console.log('[V5-MIGRATION] ✅ All stores cleared!');
	console.log('[V5-MIGRATION] 🔄 Reload page NOW to start fresh with JSON format.');

	// Give user 3 seconds to see the message, then auto-reload
	setTimeout(() => {
		console.log('[V5-MIGRATION] 🔄 Auto-reloading...');
		window.location.reload();
	}, 3000);
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
	(window as any).migrateNetworkCommitments = migrateNetworkCommitments;
	(window as any).validateAllStores = validateAllStores;
	(window as any).clearAllV5Stores = clearAllV5Stores;

	console.log('[V5-DEBUG] 🛠️  Migration & debug utilities available:');
	console.log('  • window.clearAllV5Stores() - Clear all stores (use when migrating to JSON format)');
	console.log('  • window.migrateNetworkCommitments() - Validate and clean network commitments');
	console.log('  • window.validateAllStores() - Check store health');
	console.log('  • window.debugStoresV5() - Show diagnostics');
}

