/**
 * Holster Integration for Mutual-Priority Allocation Algorithm v5 - RPC Edition
 * 
 * Uses symmetric Cap'n Web RPC protocol for bidirectional communication.
 * 
 * V5 Architecture with RPC:
 * ✅ Event-driven (no rounds)
 * ✅ ITC causality (not vector clocks)
 * ✅ Time-based damping (not round-indexed)
 * ✅ Reactive stores (auto-recompute)
 * ✅ Global MR: Same MR value for all types (tree encodes type preferences)
 * ✅ Symmetric RPC: Both sides can call each other (Cap'n Web)
 * ✅ Object capabilities: Pass references by reference
 * ✅ Promise pipelining: Chain calls without waiting
 * 
 * Architecture:
 * - RpcTarget for server-side objects (commitment, tree, allocation state)
 * - RpcStub<Interface> for client-side proxies
 * - Bidirectional calling (client ↔ server)
 * - Type-safe with Zod validation
 */

import { get, derived, readable } from 'svelte/store';
import type { Readable } from 'svelte/store';
import { createStore } from '$lib/utils/primitives/store.svelte';
import { RpcTarget, type RpcStub, newWebSocketRpcSession, newHttpBatchRpcSession } from 'capnweb';
import { z } from 'zod';
import { RevocableRpcTarget } from './rpc-elegant';

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
} from './schemas';

import { holsterUserPub, holsterUser } from '$lib/network/holster.svelte';
import { getTimeBucketKey, getLocationBucketKey } from '$lib/protocol/utils/match';
import { sharesOfGeneralFulfillmentMap, getAllContributorsFromTree } from '$lib/protocol/tree';
import { myAttributeRecognitions, myAttributeSubscriptions } from '$lib/protocol/attributes/attribute-recognition.svelte';
import { slotSubscriptions, slotFilters, capacityCache, needCache } from '$lib/network/capacity-subscriptions.svelte';
import { applyFiltersUnion, mergeSlots } from '$lib/protocol/utils/capacity-filters';
import { resolveContributorWithOrgs, resolveToPublicKey } from '$lib/network/users.svelte';
import { seed as itcSeed, event as itcEvent, join as itcJoin, type Stamp as ITCStamp } from '$lib/utils/primitives/itc';
import { createVersionedStore, type VersionedStore } from '$lib/utils/primitives/v-store.svelte';
import { jsonEquals } from '$lib/utils/primitives/v-store-equality-checkers';

// ═══════════════════════════════════════════════════════════════════
// RPC INTERFACES (TypeScript - for type-safe clients)
// ═══════════════════════════════════════════════════════════════════

/**
 * Commitment RPC Interface
 * 
 * Symmetric protocol: Anyone can expose their commitment as an RPC target.
 * Others receive RpcStub<ICommitmentRpc> to call methods remotely.
 */
export interface ICommitmentRpc {
	/** Get current commitment snapshot */
	getCommitment(): Promise<Commitment>;

	/** Get need slots only */
	getNeedSlots(): Promise<NeedSlot[]>;

	/** Get capacity slots only */
	getCapacitySlots(): Promise<AvailabilitySlot[]>;

	/** Get recognition weights */
	getRecognitionWeights(): Promise<GlobalRecognitionWeights>;

	/** Get slot allocations (what I'm providing to others) */
	getSlotAllocations(): Promise<SlotAllocationRecord[]>;

	/** Subscribe to commitment updates (callback gets called on changes) */
	subscribeToUpdates(callback: (commitment: Commitment) => void): Promise<void>;

	/** Compute mutual recognition with another participant */
	computeMutualRecognition(otherCommitment: RpcStub<ICommitmentRpc>): Promise<number>;

	/** Get public key */
	getPubKey(): Promise<string>;
}

/**
 * Recognition Tree RPC Interface
 * 
 * Optional transparency: Expose your tree for others to audit your recognition.
 */
export interface IRecognitionTreeRpc {
	/** Get current tree snapshot */
	getTree(): Promise<RootNode>;

	/** Get all contributors from tree */
	getContributors(): Promise<string[]>;

	/** Subscribe to tree updates */
	subscribeToUpdates(callback: (tree: RootNode) => void): Promise<void>;

	/** Get recognition weights computed from tree */
	getComputedWeights(): Promise<GlobalRecognitionWeights>;
}

/**
 * Allocation Engine RPC Interface
 * 
 * Bidirectional: Provider runs allocation, recipients can query results.
 */
export interface IAllocationEngineRpc {
	/** Compute allocations from my capacity to compatible recipients */
	computeAllocations(
		recipientCommitments: RpcStub<ICommitmentRpc>[]
	): Promise<SlotAllocationRecord[]>;

	/** Get allocation for specific recipient */
	getAllocationFor(recipientPubKey: string): Promise<SlotAllocationRecord[]>;

	/** Subscribe to allocation updates */
	subscribeToAllocations(callback: (allocations: SlotAllocationRecord[]) => void): Promise<void>;
}

/**
 * Network Coordinator RPC Interface
 * 
 * Discovery service: Find participants, subscribe to commitments.
 */
export interface INetworkCoordinatorRpc {
	/** Register my commitment (returns stub for others to call) */
	registerCommitment(commitment: RpcStub<ICommitmentRpc>): Promise<void>;

	/** Discover participants with compatible needs/capacity */
	discoverParticipants(resourceTypeId: string): Promise<RpcStub<ICommitmentRpc>[]>;

	/** Get commitment stub by public key */
	getCommitment(pubKey: string): Promise<RpcStub<ICommitmentRpc> | null>;
}

// ═══════════════════════════════════════════════════════════════════
// RPC TARGETS (Server-side implementations)
// ═══════════════════════════════════════════════════════════════════

/**
 * Commitment RPC Target
 * 
 * Exposes my commitment as an RPC object that others can call.
 * Integrates with existing Svelte stores for reactivity.
 * 
 * Now with built-in revocation from RevocableRpcTarget!
 */
export class CommitmentRpcTarget extends RevocableRpcTarget implements ICommitmentRpc {
	private myPubKey: string;
	private commitmentStore: any; // Store type from primitives/store.svelte
	private updateCallbacks: Set<(commitment: Commitment) => void> = new Set();

	constructor(
		pubKey: string,
		commitmentStore: any,
		options?: {
			expiresInMs?: number;
			recipientId?: string;
		}
	) {
		super(options);
		this.myPubKey = pubKey;
		this.commitmentStore = commitmentStore;

		// Subscribe to local changes and notify RPC subscribers
		this.commitmentStore.subscribe((commitment: Commitment | null) => {
			if (commitment) {
				for (const callback of this.updateCallbacks) {
					callback(commitment);
				}
			}
		});
	}

	async getCommitment(): Promise<Commitment> {
		this.checkAccess('getCommitment');
		const commitment = get(this.commitmentStore) as Commitment | null;
		if (!commitment) {
			throw new Error('Commitment not available');
		}
		return commitment;
	}

	async getNeedSlots(): Promise<NeedSlot[]> {
		this.checkAccess('getNeedSlots');
		const commitment = await this.getCommitment();
		return commitment.need_slots || [];
	}

	async getCapacitySlots(): Promise<AvailabilitySlot[]> {
		this.checkAccess('getCapacitySlots');
		const commitment = await this.getCommitment();
		return commitment.capacity_slots || [];
	}

	async getRecognitionWeights(): Promise<GlobalRecognitionWeights> {
		this.checkAccess('getRecognitionWeights');
		const commitment = await this.getCommitment();
		return commitment.global_recognition_weights || {};
	}

	async getSlotAllocations(): Promise<SlotAllocationRecord[]> {
		this.checkAccess('getSlotAllocations');
		const commitment = await this.getCommitment();
		return commitment.slot_allocations || [];
	}

	async subscribeToUpdates(callback: (commitment: Commitment) => void): Promise<void> {
		this.checkAccess('subscribeToUpdates');

		// Validate callback is a function (Cap'n Web passes it by reference)
		if (typeof callback !== 'function') {
			throw new Error('Callback must be a function');
		}

		this.updateCallbacks.add(callback);

		// Send current state immediately
		const commitment = get(this.commitmentStore) as Commitment | null;
		if (commitment) {
			callback(commitment);
		}
	}

	async computeMutualRecognition(otherCommitment: RpcStub<ICommitmentRpc>): Promise<number> {
		this.checkAccess('computeMutualRecognition');

		// Get my recognition of them
		const myWeights = await this.getRecognitionWeights();
		const theirPubKey = await otherCommitment.getPubKey();
		const myRecOfThem = myWeights[theirPubKey] || 0;

		// Get their recognition of me (via RPC!)
		const theirWeights = await otherCommitment.getRecognitionWeights();
		const theirRecOfMe = theirWeights[this.myPubKey] || 0;

		// Compute mutual recognition
		return Math.min(myRecOfThem, theirRecOfMe);
	}

	async getPubKey(): Promise<string> {
		this.checkAccess('getPubKey');
		return this.myPubKey;
	}
}

/**
 * Recognition Tree RPC Target
 * 
 * Exposes my recognition tree for transparency (optional).
 * 
 * Now with built-in revocation from RevocableRpcTarget!
 */
export class RecognitionTreeRpcTarget extends RevocableRpcTarget implements IRecognitionTreeRpc {
	private treeStore: any; // Store type from primitives/store.svelte
	private updateCallbacks: Set<(tree: RootNode) => void> = new Set();

	constructor(
		treeStore: any,
		options?: {
			expiresInMs?: number;
			recipientId?: string;
		}
	) {
		super(options);
		this.treeStore = treeStore;

		// Subscribe to local changes
		this.treeStore.subscribe((tree: RootNode | null) => {
			if (tree) {
				for (const callback of this.updateCallbacks) {
					callback(tree);
				}
			}
		});
	}

	async getTree(): Promise<RootNode> {
		this.checkAccess('getTree');
		const tree = get(this.treeStore) as RootNode | null;
		if (!tree) {
			throw new Error('Recognition tree not available');
		}
		return tree;
	}

	async getContributors(): Promise<string[]> {
		this.checkAccess('getContributors');
		const tree = await this.getTree();
		return getAllContributorsFromTree(tree);
	}

	async subscribeToUpdates(callback: (tree: RootNode) => void): Promise<void> {
		this.checkAccess('subscribeToUpdates');
		this.updateCallbacks.add(callback);

		const tree = get(this.treeStore) as RootNode | null;
		if (tree) {
			callback(tree);
		}
	}

	async getComputedWeights(): Promise<GlobalRecognitionWeights> {
		this.checkAccess('getComputedWeights');
		const tree = await this.getTree();
		return sharesOfGeneralFulfillmentMap(tree, {});
	}
}

// ═══════════════════════════════════════════════════════════════════
// MY DATA STORES (V5 + RPC)
// ═══════════════════════════════════════════════════════════════════

/** My Recognition Tree Store (same as before) */
export const myRecognitionTreeStore = createStore({
	holsterPath: 'trees/recognition_tree',
	schema: RootNodeSchema,
	persistDebounce: 200
});

/** My Recognition Weights (derived from tree) */
export const myRecognitionWeights: Readable<GlobalRecognitionWeights> = derived(
	[myRecognitionTreeStore],
	([$tree]): GlobalRecognitionWeights => {
		if (!$tree) return {};
		try {
			return sharesOfGeneralFulfillmentMap($tree, {});
		} catch (error) {
			console.error('[RECOGNITION-WEIGHTS] Error:', error);
			return {};
		}
	}
);

/** My Commitment Store (source of truth) */
export const myCommitmentStore = createStore({
	holsterPath: 'allocation/commitment',
	schema: CommitmentSchema,
	persistDebounce: 100
});

/** My Need Slots (derived) */
export const myNeedSlotsStore: Readable<NeedSlot[] | null> = derived(
	[myCommitmentStore],
	([$commitment]) => $commitment?.need_slots || null
);

/** My Capacity Slots (derived) */
export const myCapacitySlotsStore: Readable<AvailabilitySlot[] | null> = derived(
	[myCommitmentStore],
	([$commitment]) => $commitment?.capacity_slots || null
);

/** My Need Types (derived) */
export const myResourceTypesStore: Readable<string[]> = derived(
	[myNeedSlotsStore],
	([$needSlots]) => {
		if (!$needSlots) return [];
		const typeIds = new Set<string>();
		for (const slot of $needSlots) {
			if (slot.type_id) typeIds.add(slot.type_id);
		}
		return Array.from(typeIds).sort();
	}
);

/** My Capacity Types (derived) */
export const myCapacityTypesStore: Readable<string[]> = derived(
	[myCapacitySlotsStore],
	([$capacitySlots]) => {
		if (!$capacitySlots) return [];
		const typeIds = new Set<string>();
		for (const slot of $capacitySlots) {
			if (slot.type_id) typeIds.add(slot.type_id);
		}
		return Array.from(typeIds).sort();
	}
);

// ═══════════════════════════════════════════════════════════════════
// RPC CLIENT CONNECTIONS (Symmetric Protocol)
// ═══════════════════════════════════════════════════════════════════

/**
 * RPC Connection Manager
 * 
 * Manages WebSocket RPC connections to other participants.
 * Uses Cap'n Web's symmetric protocol for bidirectional calling.
 */
export class RpcConnectionManager {
	private connections = new Map<string, RpcStub<ICommitmentRpc>>();
	private myCommitmentTarget: CommitmentRpcTarget | null = null;
	private myTreeTarget: RecognitionTreeRpcTarget | null = null;

	/**
	 * Initialize my RPC targets (so others can call me)
	 */
	initializeMyTargets(pubKey: string) {
		this.myCommitmentTarget = new CommitmentRpcTarget(pubKey, myCommitmentStore);
		this.myTreeTarget = new RecognitionTreeRpcTarget(myRecognitionTreeStore);

		console.log('[RPC-MANAGER] Initialized RPC targets for', pubKey.slice(0, 20));
	}

	/**
	 * Connect to a participant's commitment via RPC
	 * 
	 * @param pubKey - Their public key
	 * @param wsUrl - WebSocket URL (e.g., wss://their-server.com/rpc)
	 */
	async connectToParticipant(pubKey: string, wsUrl: string): Promise<RpcStub<ICommitmentRpc>> {
		if (this.connections.has(pubKey)) {
			return this.connections.get(pubKey)!;
		}

		console.log(`[RPC-MANAGER] Connecting to ${pubKey.slice(0, 20)} at ${wsUrl}`);

		// Establish symmetric WebSocket RPC session
		// Both sides can call each other!
		const session = newWebSocketRpcSession<ICommitmentRpc>(wsUrl);

		this.connections.set(pubKey, session);

		// Subscribe to their updates (bidirectional calling!)
		if (this.myCommitmentTarget) {
			await session.subscribeToUpdates(async (theirCommitment) => {
				console.log(`[RPC-MANAGER] Received update from ${pubKey.slice(0, 20)}`);

				// Update local cache (integrates with existing system)
				networkCommitments.update(pubKey, theirCommitment);
			});
		}

		return session;
	}

	/**
	 * Get my commitment RPC target (for exposing to others)
	 */
	getMyCommitmentTarget(): CommitmentRpcTarget {
		if (!this.myCommitmentTarget) {
			throw new Error('RPC targets not initialized. Call initializeMyTargets() first.');
		}
		return this.myCommitmentTarget;
	}

	/**
	 * Get my tree RPC target (for transparency)
	 */
	getMyTreeTarget(): RecognitionTreeRpcTarget {
		if (!this.myTreeTarget) {
			throw new Error('RPC targets not initialized. Call initializeMyTargets() first.');
		}
		return this.myTreeTarget;
	}

	/**
	 * Get connection stub for a participant
	 */
	getConnection(pubKey: string): RpcStub<ICommitmentRpc> | undefined {
		return this.connections.get(pubKey);
	}

	/**
	 * Disconnect from a participant
	 */
	disconnect(pubKey: string) {
		this.connections.delete(pubKey);
		console.log(`[RPC-MANAGER] Disconnected from ${pubKey.slice(0, 20)}`);
	}

	/**
	 * Disconnect from all participants
	 */
	disconnectAll() {
		this.connections.clear();
		console.log('[RPC-MANAGER] Disconnected from all participants');
	}
}

/** Global RPC connection manager instance */
export const rpcManager = new RpcConnectionManager();

// ═══════════════════════════════════════════════════════════════════
// NETWORK DATA STORES (Integrated with RPC)
// ═══════════════════════════════════════════════════════════════════

/** Network Commitments (same as before, now updated via RPC) */
export const networkCommitments: VersionedStore<Commitment, string> = createVersionedStore({
	fields: {
		recognition: (c) => c.global_recognition_weights,
		needs: (c) => c.need_slots,
		capacity: (c) => c.capacity_slots,
		damping: (c) => c.multi_dimensional_damping,
		allocations: (c) => c.slot_allocations
	},
	fieldEqualityCheckers: {
		needs: jsonEquals,
		capacity: jsonEquals,
		allocations: jsonEquals
	},
	schema: CommitmentSchema,
	itcExtractor: (c) => c.itcStamp,
	timestampExtractor: (c) => c.timestamp,
	enableLogging: true
});

/** Network Recognition Trees (optional, for transparency) */
export const networkRecognitionTrees: VersionedStore<RootNode, string> = createVersionedStore({
	fields: {
		structure: (tree) => tree.children,
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
	schema: RootNodeSchema,
	timestampExtractor: (tree) => new Date(tree.updated_at).getTime(),
	enableLogging: false
});

/** Field stores (fine-grained reactivity) */
export const networkRecognitionWeights = networkCommitments.deriveField<GlobalRecognitionWeights>('recognition');
export const networkNeedSlots = networkCommitments.deriveField<NeedSlot[]>('needs');
export const networkCapacitySlots = networkCommitments.deriveField<AvailabilitySlot[]>('capacity');
export const networkAllocations = networkCommitments.deriveField<SlotAllocationRecord[]>('allocations');

/** My Mutual Recognition (local-first, same as before) */
export const myMutualRecognition: Readable<GlobalRecognitionWeights> = derived(
	[holsterUserPub, myCommitmentStore],
	([$myPub, $myCommitment]) => {
		if (!$myPub || !$myCommitment) return {};

		const myWeights = $myCommitment.global_recognition_weights || {};
		const othersRecCache = $myCommitment.others_recognition_of_me || {};
		const mutualRec: GlobalRecognitionWeights = {};

		for (const theirPub in myWeights) {
			const myRecOfThem = myWeights[theirPub] || 0;

			// Self-recognition
			if (theirPub === $myPub) {
				mutualRec[theirPub] = myRecOfThem;
				continue;
			}

			// Mutual recognition
			const theirWeights = othersRecCache[theirPub];
			const theirRecOfMe = theirWeights?.[$myPub] || 0;
			mutualRec[theirPub] = Math.min(myRecOfThem, theirRecOfMe);
		}

		return mutualRec;
	}
);

// ═══════════════════════════════════════════════════════════════════
// INITIALIZATION
// ═══════════════════════════════════════════════════════════════════

export function initializeAllocationStores() {
	console.log('[ALLOCATION-HOLSTER-RPC] Initializing stores...');

	myRecognitionTreeStore.initialize();
	myCommitmentStore.initialize();

	// Initialize RPC targets
	const myPub = get(holsterUserPub);
	if (myPub) {
		rpcManager.initializeMyTargets(myPub);
	}

	console.log('[ALLOCATION-HOLSTER-RPC] Stores initialized with RPC support');
}

export async function cleanupAllocationStores() {
	console.log('[ALLOCATION-HOLSTER-RPC] Cleaning up stores...');

	rpcManager.disconnectAll();
	await myRecognitionTreeStore.cleanup();
	await myCommitmentStore.cleanup();

	console.log('[ALLOCATION-HOLSTER-RPC] Stores cleaned up');
}

// ═══════════════════════════════════════════════════════════════════
// RPC-AWARE SUBSCRIPTION FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Subscribe to a participant via RPC (symmetric protocol)
 * 
 * @param pubKey - Their public key
 * @param wsUrl - Their WebSocket RPC endpoint
 */
export async function subscribeToParticipantViaRpc(
	pubKey: string,
	wsUrl: string
): Promise<void> {
	console.log(`[RPC-SUB] Subscribing to ${pubKey.slice(0, 20)} via RPC`);

	// Connect via RPC (symmetric WebSocket session)
	const commitmentStub = await rpcManager.connectToParticipant(pubKey, wsUrl);

	// Get initial commitment
	const commitment = await commitmentStub.getCommitment();
	console.log(`[RPC-SUB] Received initial commitment from ${pubKey.slice(0, 20)}`);

	// Update local cache
	networkCommitments.update(pubKey, commitment);

	// RPC callback will handle future updates automatically
}

/**
 * Compute mutual recognition via RPC (demonstrates bidirectional calling)
 * 
 * @param theirPubKey - Their public key
 * @returns Mutual recognition value
 */
export async function computeMutualRecognitionViaRpc(
	theirPubKey: string
): Promise<number> {
	const myCommitmentTarget = rpcManager.getMyCommitmentTarget();
	const theirCommitmentStub = rpcManager.getConnection(theirPubKey);

	if (!theirCommitmentStub) {
		throw new Error(`Not connected to ${theirPubKey}`);
	}

	// Call my local method with their RPC stub as parameter
	// This demonstrates object capability passing!
	return await myCommitmentTarget.computeMutualRecognition(theirCommitmentStub);
}

// ═══════════════════════════════════════════════════════════════════
// HELPER FUNCTIONS (same as before)
// ═══════════════════════════════════════════════════════════════════

export function setMyNeedSlots(needSlots: NeedSlot[]) {
	const current = get(myCommitmentStore);
	const recognitionWeights = get(myRecognitionWeights);
	const mergedITC = getMergedITCStamp(current?.itcStamp);

	const updated: Commitment = {
		need_slots: needSlots,
		capacity_slots: current?.capacity_slots || [],
		global_recognition_weights: recognitionWeights,
		others_recognition_of_me: current?.others_recognition_of_me,
		multi_dimensional_damping: current?.multi_dimensional_damping,
		itcStamp: mergedITC,
		timestamp: Date.now()
	};

	myCommitmentStore.set(updated);
}

export function setMyCapacitySlots(capacitySlots: AvailabilitySlot[]) {
	const current = get(myCommitmentStore);
	const recognitionWeights = get(myRecognitionWeights);
	const mergedITC = getMergedITCStamp(current?.itcStamp);

	const updated: Commitment = {
		need_slots: current?.need_slots || [],
		capacity_slots: capacitySlots,
		global_recognition_weights: recognitionWeights,
		others_recognition_of_me: current?.others_recognition_of_me,
		multi_dimensional_damping: current?.multi_dimensional_damping,
		itcStamp: mergedITC,
		timestamp: Date.now()
	};

	myCommitmentStore.set(updated);
}

function getMergedITCStamp(localITC?: ITCStamp | null): ITCStamp {
	let mergedITC: ITCStamp = localITC || itcSeed();

	const networkCommitMap = networkCommitments.get();
	for (const [, versionedEntity] of networkCommitMap.entries()) {
		if (versionedEntity.metadata.itcStamp) {
			mergedITC = itcJoin(mergedITC, versionedEntity.metadata.itcStamp);
		}
	}

	return itcEvent(mergedITC);
}

// Export remaining functions (auto-subscription, etc. - same as original)
export {
	enableAutoSubscriptionSync,
	enableAutoCommitmentComposition,
	getSubscriptionStats,
	getV5Diagnostics
} from './stores.svelte';

// ═══════════════════════════════════════════════════════════════════
// AUTHENTICATION (Cap'n Web Pattern)
// ═══════════════════════════════════════════════════════════════════

/**
 * ProtocolAuth - Natural authentication entry point
 * 
 * Following Cap'n Web's authentication pattern:
 * - Expose this as your bootstrap capability (ID 0)
 * - login() verifies credentials and returns a revocable commitment capability
 * - The returned capability IS the session - no separate session management needed
 * 
 * Usage:
 * ```typescript
 * // Server: Expose auth endpoint
 * const auth = new ProtocolAuth();
 * export default {
 *   fetch(request) {
 *     return newWorkersRpcResponse(request, auth);
 *   }
 * };
 * 
 * // Client: Authenticate and receive capability
 * const authApi = newWebSocketRpcSession<ProtocolAuth>("wss://server.com/rpc");
 * const myAccess = await authApi.login(myPubKey, mySignature);
 * 
 * // Use authenticated capability
 * const commitment = await myAccess.getCommitment();
 * ```
 */
export class ProtocolAuth extends RpcTarget {
	/**
	 * Login and receive a revocable commitment capability
	 * 
	 * This IS the capability - no wrapper needed!
	 */
	async login(pubKey: string, signature: string): Promise<RpcStub<ICommitmentRpc>> {
		if (!this.verifySignature(pubKey, signature)) {
			throw new Error('Invalid signature');
		}

		// Return a NEW revocable capability (auto-expires in 24h)
		return new CommitmentRpcTarget(
			get(holsterUserPub) || '',
			myCommitmentStore,
			{
				recipientId: pubKey,
				expiresInMs: 24 * 60 * 60 * 1000 // 24 hours
			}
		) as any;
	}

	/**
	 * Login with read-only access (more restrictive)
	 */
	async loginReadOnly(pubKey: string, signature: string): Promise<RpcStub<ICommitmentRpc>> {
		if (!this.verifySignature(pubKey, signature)) {
			throw new Error('Invalid signature');
		}

		// Return capability with shorter expiration
		return new CommitmentRpcTarget(
			get(holsterUserPub) || '',
			myCommitmentStore,
			{
				recipientId: pubKey,
				expiresInMs: 60 * 60 * 1000 // 1 hour
			}
		) as any;
	}

	/**
	 * Get recognition tree capability (optional transparency)
	 */
	async loginWithTree(
		pubKey: string,
		signature: string
	): Promise<{
		commitment: RpcStub<ICommitmentRpc>;
		tree: RpcStub<IRecognitionTreeRpc>;
	}> {
		if (!this.verifySignature(pubKey, signature)) {
			throw new Error('Invalid signature');
		}

		return {
			commitment: new CommitmentRpcTarget(
				get(holsterUserPub) || '',
				myCommitmentStore,
				{
					recipientId: pubKey,
					expiresInMs: 24 * 60 * 60 * 1000
				}
			) as any,
			tree: new RecognitionTreeRpcTarget(
				myRecognitionTreeStore,
				{
					recipientId: pubKey,
					expiresInMs: 24 * 60 * 60 * 1000
				}
			) as any
		};
	}

	private verifySignature(pubKey: string, signature: string): boolean {
		// TODO: Implement actual signature verification
		// For now, accept all (demo mode)
		console.log(`[AUTH] Login attempt from ${pubKey.slice(0, 20)}`);
		return true;
	}
}

// ═══════════════════════════════════════════════════════════════════
// HTTP BATCH MODE (Cap'n Web Advanced Feature)
// ═══════════════════════════════════════════════════════════════════

/**
 * Quick commitment fetch using HTTP batch mode
 * 
 * Use this for one-time, stateless operations without maintaining a WebSocket connection.
 * Perfect for mobile apps, quick checks, or low-bandwidth scenarios.
 * 
 * @param url - Server HTTP endpoint
 * @param pubKey - Public key for authentication
 * @param signature - Signature for authentication
 * @returns Promise resolving to commitment data
 * 
 * @example
 * ```typescript
 * const commitment = await quickGetCommitment(
 *   "https://alice.com/rpc",
 *   myPubKey,
 *   mySignature
 * );
 * ```
 */
export async function quickGetCommitment(
	url: string,
	pubKey: string,
	signature: string
) {
	const batch = newHttpBatchRpcSession<ProtocolAuth>(url);

	// Promise pipelining - all in ONE HTTP request!
	const sessionPromise = batch.login(pubKey, signature);
	return await (sessionPromise as any).commitment();
}

/**
 * Quick needs fetch using HTTP batch mode with promise pipelining
 * 
 * Demonstrates Cap'n Web's promise pipelining: chain multiple calls in ONE round trip!
 * 
 * @param url - Server HTTP endpoint
 * @param pubKey - Public key for authentication  
 * @param signature - Signature for authentication
 * @returns Promise resolving to need slots
 * 
 * @example
 * ```typescript
 * // ONE HTTP request for login -> commitment -> needs!
 * const needs = await quickGetNeeds("https://alice.com/rpc", myPubKey, mySig);
 * ```
 */
export async function quickGetNeeds(
	url: string,
	pubKey: string,
	signature: string
): Promise<NeedSlot[]> {
	const batch = newHttpBatchRpcSession<ProtocolAuth>(url);

	// Promise pipelining - no await on sessionPromise!
	const sessionPromise = batch.login(pubKey, signature);
	const commitmentPromise = (sessionPromise as any).commitment();
	return await (commitmentPromise as any).needs();
}

/**
 * Quick batch fetch of multiple fields
 * 
 * Demonstrates HTTP batch mode with Promise.all for parallel operations.
 * 
 * @param url - Server HTTP endpoint
 * @param pubKey - Public key for authentication
 * @param signature - Signature for authentication
 * @returns Promise resolving to needs and capacity
 * 
 * @example
 * ```typescript
 * const { needs, capacity } = await quickBatchFetch(
 *   "https://alice.com/rpc",
 *   myPubKey,
 *   mySig
 * );
 * ```
 */
export async function quickBatchFetch(
	url: string,
	pubKey: string,
	signature: string
): Promise<{ needs: NeedSlot[]; capacity: AvailabilitySlot[] }> {
	const batch = newHttpBatchRpcSession<ProtocolAuth>(url);

	// Start both chains in parallel
	const sessionPromise = batch.login(pubKey, signature);
	const commitmentPromise = (sessionPromise as any).commitment();

	// Fetch both in parallel - still ONE HTTP request!
	const [needs, capacity] = await Promise.all([
		(commitmentPromise as any).needs(),
		(commitmentPromise as any).capacity()
	]);

	return { needs, capacity };
}

if (typeof window !== 'undefined') {
	(window as any).rpcManager = rpcManager;
	(window as any).ProtocolAuth = ProtocolAuth;
	(window as any).quickGetCommitment = quickGetCommitment;
	(window as any).quickGetNeeds = quickGetNeeds;
	(window as any).quickBatchFetch = quickBatchFetch;
	console.log('[RPC-DEBUG] 🛠️  RPC Manager available: window.rpcManager');
	console.log('[RPC-DEBUG] 🔐 ProtocolAuth available: window.ProtocolAuth');
	console.log('[RPC-DEBUG] ⚡ HTTP Batch functions available: quickGetCommitment, quickGetNeeds, quickBatchFetch');
}

