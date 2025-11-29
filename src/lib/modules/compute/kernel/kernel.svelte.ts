/**
 * User Space Stores - Holster Integration
 * 
 * Provides stores and utilities for managing the complete user space structure:
 * - Programs (registry, active/inactive, subscribed)
 * - Compute (state, outputs, provenance)
 * - Subscriptions (outbound/inbound tracking)
 * - Nodes (tree nodes with storage)
 * - Causality (ITC stamps)
 * - Allocation (commitments, states)
 * - Trees (priority/contribution trees)
 * - Replication (encrypted peer data)
 * 
 * Features:
 * - Type-safe with Zod validation
 * - Automatic persistence to Holster
 * - Cross-user subscriptions
 * - Path helpers for consistency
 */

import { writable, derived, get } from 'svelte/store';
import type { Writable, Readable } from 'svelte/store';
import { createStore } from '../utils/store.svelte';
import { holsterUser } from '$lib/network/holster.svelte';
import { writeAtPath, readAtPath, listenAtPath } from '$lib/utils/data/holsterData';

import {
	// Schemas
	ProgramRegistryEntrySchema,
	ProgramReferenceSchema,
	SubscribedProgramSchema,
	ComputeNamespaceSchema,
	OutputValueSchema,
	ProvenanceEntrySchema,
	LocalSubscriptionSchema,
	PeerSubscriptionSchema,
	InboundSubscriptionSchema,
	NodeEntrySchema,
	CausalityNamespaceSchema,
	MyITCStampSchema,
	PeerITCStampEntrySchema,
	AllocationNamespaceSchema,
	TreesNamespaceSchema,
	
	// Types
	type ProgramRegistryEntry,
	type ProgramReference,
	type SubscribedProgram,
	type ComputeNamespace,
	type OutputValue,
	type ProvenanceEntry,
	type LocalSubscription,
	type PeerSubscription,
	type InboundSubscription,
	type NodeEntry,
	type CausalityNamespace,
	type MyITCStamp,
	type PeerITCStampEntry,
	type AllocationNamespace,
	type TreesNamespace,
	
	// Path helpers
	UserSpacePaths
} from './kernel';

import {
	ITCStampSchema,
	CommitmentSchema,
	TwoTierAllocationStateSchema,
	RootNodeSchema,
	type ITCStamp,
	type Commitment,
	type TwoTierAllocationState,
	type RootNode
} from '../v2/schemas';

// ═══════════════════════════════════════════════════════════════════
// PROGRAMS NAMESPACE STORES
// ═══════════════════════════════════════════════════════════════════

/**
 * Program Registry Store
 * Manages the central registry of all program definitions
 */
export const programRegistryStore = writable<Map<string, ProgramRegistryEntry>>(new Map());

/**
 * Register a new program in the registry
 */
export async function registerProgram(
	programHash: string,
	entry: ProgramRegistryEntry
): Promise<void> {
	if (!holsterUser.is) {
		throw new Error('Not authenticated');
	}
	
	const myPubKey = holsterUser.is.pub;
	const path = UserSpacePaths.programRegistry(myPubKey, programHash);
	
	return new Promise((resolve, reject) => {
		writeAtPath(holsterUser, path.split('/'), entry, (err) => {
			if (err) {
				console.error('[SPACE-STORES] Error registering program:', err);
				reject(err);
			} else {
				// Update local store
				programRegistryStore.update(map => {
					map.set(programHash, entry);
					return map;
				});
				console.log(`[SPACE-STORES] Registered program: ${programHash.slice(0, 20)}...`);
				resolve();
			}
		});
	});
}

/**
 * Get a program from the registry
 */
export function getProgram(programHash: string): ProgramRegistryEntry | null {
	const registry = get(programRegistryStore);
	return registry.get(programHash) || null;
}

/**
 * Activate a program (move from inactive to active)
 */
export async function activateProgram(programHash: string): Promise<void> {
	if (!holsterUser.is) {
		throw new Error('Not authenticated');
	}
	
	const myPubKey = holsterUser.is.pub;
	const registryPath = UserSpacePaths.programRegistry(myPubKey, programHash);
	const activePath = UserSpacePaths.programActive(myPubKey, programHash);
	
	const reference: ProgramReference = { registry_path: registryPath };
	
	return new Promise((resolve, reject) => {
		writeAtPath(holsterUser, activePath.split('/'), reference, (err) => {
			if (err) {
				console.error('[SPACE-STORES] Error activating program:', err);
				reject(err);
			} else {
				// Update status in registry
				const program = getProgram(programHash);
				if (program) {
					program.status.active = true;
					registerProgram(programHash, program);
				}
				console.log(`[SPACE-STORES] Activated program: ${programHash.slice(0, 20)}...`);
				resolve();
			}
		});
	});
}

/**
 * Deactivate a program (move from active to inactive)
 */
export async function deactivateProgram(programHash: string): Promise<void> {
	if (!holsterUser.is) {
		throw new Error('Not authenticated');
	}
	
	const myPubKey = holsterUser.is.pub;
	const registryPath = UserSpacePaths.programRegistry(myPubKey, programHash);
	const inactivePath = UserSpacePaths.programInactive(myPubKey, programHash);
	
	const reference: ProgramReference = { registry_path: registryPath };
	
	return new Promise((resolve, reject) => {
		writeAtPath(holsterUser, inactivePath.split('/'), reference, (err) => {
			if (err) {
				console.error('[SPACE-STORES] Error deactivating program:', err);
				reject(err);
			} else {
				// Update status in registry
				const program = getProgram(programHash);
				if (program) {
					program.status.active = false;
					registerProgram(programHash, program);
				}
				console.log(`[SPACE-STORES] Deactivated program: ${programHash.slice(0, 20)}...`);
				resolve();
			}
		});
	});
}

// ═══════════════════════════════════════════════════════════════════
// COMPUTE NAMESPACE STORES
// ═══════════════════════════════════════════════════════════════════

/**
 * Compute State Store (per program)
 * Tracks execution state, variables, and results
 */
export function createComputeStateStore(programHash: string) {
	return createStore({
		holsterPath: UserSpacePaths.computeState(holsterUser.is?.pub || '', programHash).replace(/^~[^/]+\//, ''),
		schema: ComputeNamespaceSchema,
		persistDebounce: 50
	});
}

/**
 * Write a variable value
 */
export async function writeVariable(
	programHash: string,
	variableName: string,
	value: any
): Promise<void> {
	if (!holsterUser.is) {
		throw new Error('Not authenticated');
	}
	
	const myPubKey = holsterUser.is.pub;
	const path = UserSpacePaths.computeVariable(myPubKey, programHash, variableName);
	
	return new Promise((resolve, reject) => {
		writeAtPath(holsterUser, path.split('/'), value, (err) => {
			if (err) {
				console.error('[SPACE-STORES] Error writing variable:', err);
				reject(err);
			} else {
				resolve();
			}
		});
	});
}

/**
 * Write a computation result
 */
export async function writeComputationResult(
	programHash: string,
	computationId: string,
	result: any
): Promise<void> {
	if (!holsterUser.is) {
		throw new Error('Not authenticated');
	}
	
	const myPubKey = holsterUser.is.pub;
	const path = UserSpacePaths.computeResult(myPubKey, programHash, computationId);
	
	const resultData = {
		result,
		last_executed: Date.now(),
		execution_count: 1 // TODO: Track actual count
	};
	
	return new Promise((resolve, reject) => {
		writeAtPath(holsterUser, path.split('/'), resultData, (err) => {
			if (err) {
				console.error('[SPACE-STORES] Error writing computation result:', err);
				reject(err);
			} else {
				resolve();
			}
		});
	});
}

/**
 * Write an output value
 */
export async function writeOutput(
	programHash: string,
	outputKey: string,
	value: any,
	holsterPath: string
): Promise<void> {
	if (!holsterUser.is) {
		throw new Error('Not authenticated');
	}
	
	const myPubKey = holsterUser.is.pub;
	const path = UserSpacePaths.computeOutput(myPubKey, programHash, outputKey);
	
	const outputData: OutputValue = {
		value,
		holster_path: holsterPath,
		updated_at: Date.now()
	};
	
	return new Promise((resolve, reject) => {
		writeAtPath(holsterUser, path.split('/'), outputData, (err) => {
			if (err) {
				console.error('[SPACE-STORES] Error writing output:', err);
				reject(err);
			} else {
				resolve();
			}
		});
	});
}

/**
 * Write provenance record
 */
export async function writeProvenance(
	programHash: string,
	provenanceId: string,
	entry: ProvenanceEntry
): Promise<void> {
	if (!holsterUser.is) {
		throw new Error('Not authenticated');
	}
	
	const myPubKey = holsterUser.is.pub;
	const path = UserSpacePaths.computeProvenance(myPubKey, programHash, provenanceId);
	
	return new Promise((resolve, reject) => {
		writeAtPath(holsterUser, path.split('/'), entry, (err) => {
			if (err) {
				console.error('[SPACE-STORES] Error writing provenance:', err);
				reject(err);
			} else {
				console.log(`[SPACE-STORES] Wrote provenance: ${provenanceId}`);
				resolve();
			}
		});
	});
}

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION NAMESPACE STORES
// ═══════════════════════════════════════════════════════════════════

/**
 * Outbound Subscriptions Store
 * Tracks what we're watching (local + peers)
 */
export const outboundSubscriptionsStore = writable<{
	local: Map<string, LocalSubscription>;
	peers: Map<string, Map<string, PeerSubscription>>;
}>({
	local: new Map(),
	peers: new Map()
});

/**
 * Inbound Subscriptions Store
 * Tracks who's watching us
 */
export const inboundSubscriptionsStore = writable<Map<string, string[]>>(new Map());

/**
 * Register an outbound subscription (local)
 */
export async function registerLocalSubscription(
	holsterPath: string,
	schemaType: string,
	callbackId: string
): Promise<void> {
	outboundSubscriptionsStore.update(subs => {
		const existing = subs.local.get(holsterPath);
		if (existing) {
			if (!existing.subscribers.includes(callbackId)) {
				existing.subscribers.push(callbackId);
			}
		} else {
			subs.local.set(holsterPath, {
				schema_type: schemaType,
				subscribers: [callbackId],
				last_value: undefined
			});
		}
		return subs;
	});
	
	console.log(`[SPACE-STORES] Registered local subscription: ${holsterPath}`);
}

/**
 * Register an outbound subscription (peer)
 */
export async function registerPeerSubscription(
	peerPubKey: string,
	holsterPath: string,
	schemaType: string,
	callbackId: string
): Promise<void> {
	outboundSubscriptionsStore.update(subs => {
		if (!subs.peers.has(peerPubKey)) {
			subs.peers.set(peerPubKey, new Map());
		}
		
		const peerSubs = subs.peers.get(peerPubKey)!;
		const existing = peerSubs.get(holsterPath);
		
		if (existing) {
			if (!existing.subscribers.includes(callbackId)) {
				existing.subscribers.push(callbackId);
			}
			existing.last_synced = Date.now();
		} else {
			peerSubs.set(holsterPath, {
				schema_type: schemaType,
				subscribers: [callbackId],
				last_value: undefined,
				last_synced: Date.now()
			});
		}
		
		return subs;
	});
	
	console.log(`[SPACE-STORES] Registered peer subscription: ${peerPubKey.slice(0, 20)}.../${holsterPath}`);
}

/**
 * Register an inbound subscription (who's watching us)
 */
export async function registerInboundSubscription(
	peerPubKey: string,
	holsterPath: string
): Promise<void> {
	inboundSubscriptionsStore.update(subs => {
		const existing = subs.get(peerPubKey) || [];
		if (!existing.includes(holsterPath)) {
			existing.push(holsterPath);
		}
		subs.set(peerPubKey, existing);
		return subs;
	});
	
	console.log(`[SPACE-STORES] Registered inbound: ${peerPubKey.slice(0, 20)}... watching ${holsterPath}`);
}

// ═══════════════════════════════════════════════════════════════════
// NODE NAMESPACE STORES
// ═══════════════════════════════════════════════════════════════════

/**
 * Nodes Store
 * Maps nodeId → NodeEntry (node + optional storage)
 */
export const nodesStore = writable<Map<string, NodeEntry>>(new Map());

/**
 * Write a node entry
 */
export async function writeNode(
	nodeId: string,
	entry: NodeEntry
): Promise<void> {
	if (!holsterUser.is) {
		throw new Error('Not authenticated');
	}
	
	const myPubKey = holsterUser.is.pub;
	const path = UserSpacePaths.node(myPubKey, nodeId);
	
	return new Promise((resolve, reject) => {
		writeAtPath(holsterUser, path.split('/'), entry, (err) => {
			if (err) {
				console.error('[SPACE-STORES] Error writing node:', err);
				reject(err);
			} else {
				// Update local store
				nodesStore.update(map => {
					map.set(nodeId, entry);
					return map;
				});
				resolve();
			}
		});
	});
}

/**
 * Read a node entry
 */
export async function readNode(nodeId: string): Promise<NodeEntry | null> {
	if (!holsterUser.is) {
		throw new Error('Not authenticated');
	}
	
	const myPubKey = holsterUser.is.pub;
	const path = UserSpacePaths.node(myPubKey, nodeId);
	
	return new Promise((resolve) => {
		readAtPath(holsterUser, path.split('/'), (data) => {
			if (!data) {
				resolve(null);
				return;
			}
			
			const validation = NodeEntrySchema.safeParse(data);
			if (!validation.success) {
				console.warn('[SPACE-STORES] Invalid node data:', validation.error);
				resolve(null);
			} else {
				resolve(validation.data);
			}
		});
	});
}

/**
 * Create a store for a specific node
 */
export function createNodeStore(nodeId: string) {
	return createStore({
		holsterPath: UserSpacePaths.nodeStorage(holsterUser.is?.pub || '', nodeId).replace(/^~[^/]+\//, ''),
		schema: NodeEntrySchema,
		persistDebounce: 100
	});
}

// ═══════════════════════════════════════════════════════════════════
// CAUSALITY NAMESPACE STORES
// ═══════════════════════════════════════════════════════════════════

/**
 * My ITC Stamp Store
 */
export const myITCStampStore = createStore({
	holsterPath: 'causality/itc_stamp',
	schema: ITCStampSchema,
	persistDebounce: 0 // Immediate persistence for causality
});

/**
 * Peer ITC Stamps Store
 * Maps peerPubKey → PeerITCStampEntry
 */
export const peerITCStampsStore = writable<Map<string, PeerITCStampEntry>>(new Map());

/**
 * Write peer ITC stamp
 */
export async function writePeerITCStamp(
	peerPubKey: string,
	stamp: ITCStamp
): Promise<void> {
	if (!holsterUser.is) {
		throw new Error('Not authenticated');
	}
	
	const myPubKey = holsterUser.is.pub;
	const path = UserSpacePaths.peerITCStamp(myPubKey, peerPubKey);
	
	const entry: PeerITCStampEntry = {
		itc_stamp: stamp,
		last_seen: Date.now()
	};
	
	return new Promise((resolve, reject) => {
		writeAtPath(holsterUser, path.split('/'), entry, (err) => {
			if (err) {
				console.error('[SPACE-STORES] Error writing peer ITC stamp:', err);
				reject(err);
			} else {
				// Update local store
				peerITCStampsStore.update(map => {
					map.set(peerPubKey, entry);
					return map;
				});
				resolve();
			}
		});
	});
}

/**
 * Subscribe to peer ITC stamp
 */
export function subscribeToPeerITCStamp(
	peerPubKey: string,
	callback: (stamp: ITCStamp | null) => void
): () => void {
	if (!holsterUser.is) {
		console.error('[SPACE-STORES] Not authenticated');
		return () => {};
	}
	
	const path = `~${peerPubKey}/causality/itc_stamp`;
	
	return listenAtPath(holsterUser, path.split('/'), (data) => {
		if (!data) {
			callback(null);
			return;
		}
		
		const validation = ITCStampSchema.safeParse(data);
		if (!validation.success) {
			console.warn('[SPACE-STORES] Invalid peer ITC stamp:', validation.error);
			callback(null);
		} else {
			callback(validation.data);
		}
	}, true);
}

// ═══════════════════════════════════════════════════════════════════
// ALLOCATION NAMESPACE STORES
// ═══════════════════════════════════════════════════════════════════

/**
 * My Commitment Store
 */
export const myCommitmentStore = createStore({
	holsterPath: 'allocation/commitment',
	schema: CommitmentSchema,
	persistDebounce: 100
});

/**
 * My Allocation State Store
 */
export const myAllocationStateStore = createStore({
	holsterPath: 'allocation/allocation_state',
	schema: TwoTierAllocationStateSchema,
	persistDebounce: 100
});

/**
 * Network Allocations Store
 * Maps peerPubKey → { commitment, allocation_state }
 */
export const networkAllocationsStore = writable<Map<string, {
	commitment?: Commitment;
	allocation_state?: TwoTierAllocationState;
}>>(new Map());

/**
 * Subscribe to peer allocation
 */
export function subscribeToPeerAllocation(
	peerPubKey: string
): () => void {
	if (!holsterUser.is) {
		console.error('[SPACE-STORES] Not authenticated');
		return () => {};
	}
	
	const commitmentPath = `~${peerPubKey}/allocation/commitment`;
	const statePath = `~${peerPubKey}/allocation/allocation_state`;
	
	// Subscribe to both commitment and state
	const unsubCommitment = listenAtPath(holsterUser, commitmentPath.split('/'), (data) => {
		if (data) {
			const validation = CommitmentSchema.safeParse(data);
			if (validation.success) {
				networkAllocationsStore.update(map => {
					const existing = map.get(peerPubKey) || {};
					existing.commitment = validation.data;
					map.set(peerPubKey, existing);
					return map;
				});
			}
		}
	}, true);
	
	const unsubState = listenAtPath(holsterUser, statePath.split('/'), (data) => {
		if (data) {
			const validation = TwoTierAllocationStateSchema.safeParse(data);
			if (validation.success) {
				networkAllocationsStore.update(map => {
					const existing = map.get(peerPubKey) || {};
					existing.allocation_state = validation.data;
					map.set(peerPubKey, existing);
					return map;
				});
			}
		}
	}, true);
	
	console.log(`[SPACE-STORES] Subscribed to allocation: ${peerPubKey.slice(0, 20)}...`);
	
	// Return combined unsubscribe
	return () => {
		unsubCommitment();
		unsubState();
	};
}

// ═══════════════════════════════════════════════════════════════════
// TREES NAMESPACE STORES
// ═══════════════════════════════════════════════════════════════════

/**
 * My Tree Store
 */
export const myTreeStore = createStore({
	holsterPath: 'trees/my_tree',
	schema: RootNodeSchema,
	persistDebounce: 200
});

/**
 * Network Trees Store
 * Maps peerPubKey → RootNode
 */
export const networkTreesStore = writable<Map<string, RootNode>>(new Map());

/**
 * Subscribe to peer tree
 */
export function subscribeToPeerTree(
	peerPubKey: string
): () => void {
	if (!holsterUser.is) {
		console.error('[SPACE-STORES] Not authenticated');
		return () => {};
	}
	
	const path = `~${peerPubKey}/trees/my_tree`;
	
	return listenAtPath(holsterUser, path.split('/'), (data) => {
		if (!data) {
			networkTreesStore.update(map => {
				map.delete(peerPubKey);
				return map;
			});
			return;
		}
		
		const validation = RootNodeSchema.safeParse(data);
		if (!validation.success) {
			console.warn('[SPACE-STORES] Invalid peer tree:', validation.error);
		} else {
			networkTreesStore.update(map => {
				map.set(peerPubKey, validation.data);
				return map;
			});
		}
	}, true);
}

// ═══════════════════════════════════════════════════════════════════
// INITIALIZATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Initialize all user space stores
 * Call this after holster authentication
 */
export function initializeUserSpaceStores() {
	if (!holsterUser.is) {
		console.error('[SPACE-STORES] Cannot initialize: not authenticated');
		return;
	}
	
	console.log('[SPACE-STORES] Initializing user space stores...');
	
	// Initialize causality stores
	myITCStampStore.initialize();
	
	// Initialize allocation stores
	myCommitmentStore.initialize();
	myAllocationStateStore.initialize();
	
	// Initialize tree store
	myTreeStore.initialize();
	
	console.log('[SPACE-STORES] User space stores initialized');
}

/**
 * Cleanup all user space stores
 * Call this before logout
 */
export async function cleanupUserSpaceStores() {
	console.log('[SPACE-STORES] Cleaning up user space stores...');
	
	await Promise.all([
		myITCStampStore.cleanup(),
		myCommitmentStore.cleanup(),
		myAllocationStateStore.cleanup(),
		myTreeStore.cleanup()
	]);
	
	// Clear all maps
	programRegistryStore.set(new Map());
	outboundSubscriptionsStore.set({ local: new Map(), peers: new Map() });
	inboundSubscriptionsStore.set(new Map());
	nodesStore.set(new Map());
	peerITCStampsStore.set(new Map());
	networkAllocationsStore.set(new Map());
	networkTreesStore.set(new Map());
	
	console.log('[SPACE-STORES] User space stores cleaned up');
}

// ═══════════════════════════════════════════════════════════════════
// DIAGNOSTICS
// ═══════════════════════════════════════════════════════════════════

/**
 * Get user space diagnostics
 */
export function getUserSpaceDiagnostics() {
	const outboundSubs = get(outboundSubscriptionsStore);
	const inboundSubs = get(inboundSubscriptionsStore);
	
	return {
		programs: {
			registered: get(programRegistryStore).size
		},
		subscriptions: {
			outbound_local: outboundSubs.local.size,
			outbound_peers: outboundSubs.peers.size,
			inbound_peers: inboundSubs.size
		},
		nodes: {
			total: get(nodesStore).size
		},
		causality: {
			peer_stamps: get(peerITCStampsStore).size
		},
		allocation: {
			network_peers: get(networkAllocationsStore).size
		},
		trees: {
			network_trees: get(networkTreesStore).size
		}
	};
}

// Window debugging
if (typeof window !== 'undefined') {
	(window as any).debugUserSpace = () => {
		console.log('[USER-SPACE] Diagnostics:', getUserSpaceDiagnostics());
	};
}

// Re-export UserSpacePaths for convenience
export { UserSpacePaths };

