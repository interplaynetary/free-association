/**
 * Tree Module - Holster Implementation
 *
 * Stores tree as a flat node collection to avoid 1MB string limits
 * and enable partial tree updates.
 *
 * Storage structure:
 * {
 *   nodes: { [nodeId]: NodeData },
 *   root_id: string,
 *   _updatedAt: number
 * }
 */

import { writable, get } from 'svelte/store';
import { holsterUser } from './holster.svelte';
import type { RootNode, Node, NonRootNode } from '$lib/schema';
import { RootNodeSchema } from '$lib/schema';
import { addTimestamp, getTimestamp, shouldPersist } from '$lib/utils/holsterTimestamp';
import { userTree, isLoadingTree } from './core.svelte';

// ============================================================================
// Types
// ============================================================================

/**
 * Flat node representation for Holster storage
 * Arrays converted to objects for Holster compatibility
 * Empty arrays are omitted (not stored as empty objects)
 */
interface FlatNode {
	id: string;
	name: string;
	type: 'RootNode' | 'NonRootNode';
	manual_fulfillment: number | null;
	children_ids?: Record<string, string>; // Optional - omitted if no children
	// RootNode specific
	created_at?: string;
	updated_at?: string;
	// NonRootNode specific
	points?: number;
	parent_id?: string;
	contributor_ids?: Record<string, string>; // Optional - omitted if empty
	anti_contributors_ids?: Record<string, string>; // Optional - omitted if empty
}

/**
 * Tree storage format in Holster
 */
interface FlatTreeData {
	nodes: Record<string, FlatNode>;
	root_id: string;
}

// ============================================================================
// State
// ============================================================================

export const holsterTree = writable<RootNode | null>(null);

let lastNetworkTimestamp: number | null = null;
let treeCallback: ((data: any) => void) | null = null;
let isPersisting: boolean = false; // Lock to prevent concurrent persistence
let lastPersistedNodes: Record<string, FlatNode> = {}; // Track full node data from last persist (for incremental updates)
let isInitialized: boolean = false; // Prevent duplicate initialization
let hasReceivedRealData = false;
let defaultTreePersistTimeout: NodeJS.Timeout | null = null;

// Queue for network updates during persistence
let queuedNetworkUpdate: any = null; // Store latest network update while persisting

// Track if local changes are pending persistence
// NOTE: This is intentionally a boolean flag, not a queue of tree snapshots.
// The persistence system implements EVENTUAL CONSISTENCY - we only care about
// persisting the most recent state from userTree, not intermediate snapshots.
// Multiple rapid updates during persistence will trigger a single retry that
// reads the current userTree state, which is the desired behavior.
let hasPendingLocalChanges: boolean = false;

// ============================================================================
// Queue Processing
// ============================================================================

/**
 * Process any queued network update and pending local changes after persistence completes
 */
function processQueuedUpdate() {
	// First, process any queued network updates
	if (queuedNetworkUpdate) {
		const data = queuedNetworkUpdate;
		queuedNetworkUpdate = null; // Clear queue

		// Process the update using the same logic as subscription callback
		processNetworkUpdate(data);
	}

	// Second, retry persistence if local changes are pending
	if (hasPendingLocalChanges) {
		hasPendingLocalChanges = false; // Clear flag before retrying
		// Use setTimeout to avoid recursion and give queue a chance to settle
		setTimeout(() => {
			persistHolsterTree();
		}, 50);
	}
}

/**
 * Process a network update (shared logic for subscription and queued updates)
 */
function processNetworkUpdate(data: any) {
	if (!data) return;

	// Extract timestamp
	const networkTimestamp = getTimestamp(data);

	// Remove timestamp field
	const { _updatedAt, ...treeData } = data;

	// Extract nodes - filter out metadata fields and null values (deleted nodes)
	const nodes: Record<string, FlatNode> = {};
	if (treeData.nodes && typeof treeData.nodes === 'object') {
		for (const [key, value] of Object.entries(treeData.nodes)) {
			if (!key.startsWith('_') && value && typeof value === 'object') {
				nodes[key] = value as FlatNode;
			}
		}
	}

	// Validate we have nodes and root_id
	if (Object.keys(nodes).length === 0 || !treeData.root_id) {
		// This can happen during initial persistence when metadata arrives before all nodes
		return;
	}

	// Only update if newer
	if (!lastNetworkTimestamp || (networkTimestamp && networkTimestamp > lastNetworkTimestamp)) {
		// Track loaded nodes for incremental updates
		lastPersistedNodes = { ...nodes };

		const flatTreeData: FlatTreeData = {
			nodes,
			root_id: treeData.root_id
		};

		// Reconstruct tree from flat format
		const reconstructed = reconstructTree(flatTreeData);

		if (reconstructed) {
			// Validate reconstructed tree
			const parseResult = RootNodeSchema.safeParse(reconstructed);
			if (parseResult.success) {
				holsterTree.set(parseResult.data);
				// Sync to userTree for UI consumption
				userTree.set(parseResult.data);
				if (networkTimestamp) {
					lastNetworkTimestamp = networkTimestamp;
					// Cache the tree for faster future loads
					setCachedTree(parseResult.data, networkTimestamp);
				}
				// Data loaded successfully - stop loading
				isLoadingTree.set(false);
			} else {
				console.error('[TREE-HOLSTER] Invalid reconstructed tree:', parseResult.error);
			}
		}
	}
}

// ============================================================================
// Tree Conversion Utilities
// ============================================================================

/**
 * Deep compare two FlatNode objects to detect changes
 */
function nodesEqual(node1: FlatNode, node2: FlatNode): boolean {
	// Compare all primitive fields
	if (
		node1.id !== node2.id ||
		node1.name !== node2.name ||
		node1.type !== node2.type ||
		node1.manual_fulfillment !== node2.manual_fulfillment ||
		node1.points !== node2.points ||
		node1.parent_id !== node2.parent_id ||
		node1.created_at !== node2.created_at ||
		node1.updated_at !== node2.updated_at
	) {
		return false;
	}

	// Compare children_ids
	const keys1 = Object.keys(node1.children_ids || {}).sort();
	const keys2 = Object.keys(node2.children_ids || {}).sort();
	if (keys1.length !== keys2.length || !keys1.every((k, i) => k === keys2[i])) {
		return false;
	}

	// Compare contributor_ids
	const contrib1 = Object.keys(node1.contributor_ids || {}).sort();
	const contrib2 = Object.keys(node2.contributor_ids || {}).sort();
	if (contrib1.length !== contrib2.length || !contrib1.every((k, i) => k === contrib2[i])) {
		return false;
	}

	// Compare anti_contributors_ids
	const anti1 = Object.keys(node1.anti_contributors_ids || {}).sort();
	const anti2 = Object.keys(node2.anti_contributors_ids || {}).sort();
	if (anti1.length !== anti2.length || !anti1.every((k, i) => k === anti2[i])) {
		return false;
	}

	return true;
}

/**
 * Convert array to object using IDs as keys
 */
function arrayToObject(arr: string[]): Record<string, string> {
	const obj: Record<string, string> = {};
	for (let i = 0; i < arr.length; i++) {
		obj[arr[i]] = arr[i];
	}
	return obj;
}

/**
 * Flatten a recursive tree into a flat node collection
 */
function flattenTree(rootNode: RootNode): FlatTreeData {
	const nodes: Record<string, FlatNode> = {};

	function flattenNode(node: Node): void {
		// Convert children array to object (only if not empty)
		const childrenIds = node.children.map((child: Node) => child.id);
		const childrenIdsObj = arrayToObject(childrenIds);

		const flatNode: any = {
			id: node.id,
			name: node.name,
			type: node.type
		};

		// Only include manual_fulfillment if it has a value
		if (node.manual_fulfillment !== undefined && node.manual_fulfillment !== null) {
			flatNode.manual_fulfillment = node.manual_fulfillment;
		}

		// Only include children_ids if there are children
		if (Object.keys(childrenIdsObj).length > 0) {
			flatNode.children_ids = childrenIdsObj;
		}

		if (node.type === 'RootNode') {
			if (node.created_at) flatNode.created_at = node.created_at;
			if (node.updated_at) flatNode.updated_at = node.updated_at;
		} else {
			const nonRootNode = node as NonRootNode;

			// Only include if defined
			if (nonRootNode.points !== undefined) flatNode.points = nonRootNode.points;
			if (nonRootNode.parent_id) flatNode.parent_id = nonRootNode.parent_id;

			// Convert contributor arrays to objects (only if not empty)
			const contributorIdsObj = arrayToObject(nonRootNode.contributor_ids);
			if (Object.keys(contributorIdsObj).length > 0) {
				flatNode.contributor_ids = contributorIdsObj;
			}

			const antiContributorIdsObj = arrayToObject(nonRootNode.anti_contributors_ids);
			if (Object.keys(antiContributorIdsObj).length > 0) {
				flatNode.anti_contributors_ids = antiContributorIdsObj;
			}
		}

		nodes[node.id] = flatNode as FlatNode;

		// Recursively flatten children
		for (const child of node.children) {
			flattenNode(child);
		}
	}

	flattenNode(rootNode);

	return {
		nodes,
		root_id: rootNode.id
	};
}

/**
 * Convert object back to array
 */
function objectToArray(obj: Record<string, string> | undefined): string[] {
	if (!obj) return [];
	const arr: string[] = [];
	for (const key of Object.keys(obj)) {
		if (obj[key]) { // Only include non-null/undefined values
			arr.push(obj[key]);
		}
	}
	return arr;
}

/**
 * Reconstruct recursive tree from flat node collection
 */
function reconstructTree(flatData: FlatTreeData): RootNode | null {
	const { nodes, root_id } = flatData;

	// Try to find root node by specified ID
	let actualRootId = root_id;

	if (!nodes[root_id]) {
		console.warn('[TREE-HOLSTER] Root node not found with ID:', root_id);

		// Try to find any RootNode type in the nodes collection
		const rootNodes = Object.values(nodes).filter(node => node.type === 'RootNode');

		if (rootNodes.length === 0) {
			console.error('[TREE-HOLSTER] No RootNode found in nodes collection');
			return null;
		}

		if (rootNodes.length > 1) {
			console.warn('[TREE-HOLSTER] Multiple RootNodes found, using first one');
		}

		actualRootId = rootNodes[0].id;
		console.log('[TREE-HOLSTER] Found RootNode with ID:', actualRootId);
	}

	function buildNode(nodeId: string): Node | null {
		const flatNode = nodes[nodeId];
		if (!flatNode) {
			// Silently skip missing nodes - they may have been deleted
			// but parent still has stale reference
			return null;
		}

		// Convert children_ids object to array and recursively build
		const childrenIdsArray = objectToArray(flatNode.children_ids);
		const children: Node[] = [];
		for (const childId of childrenIdsArray) {
			const child = buildNode(childId);
			if (child) {
				children.push(child);
			}
		}

		// Construct node based on type
		if (flatNode.type === 'RootNode') {
			return {
				id: flatNode.id,
				name: flatNode.name,
				type: 'RootNode' as const,
				manual_fulfillment: flatNode.manual_fulfillment ?? null,
				children,
				created_at: flatNode.created_at!,
				updated_at: flatNode.updated_at!
			};
		} else {
			return {
				id: flatNode.id,
				name: flatNode.name,
				type: 'NonRootNode' as const,
				manual_fulfillment: flatNode.manual_fulfillment ?? null,
				children,
				points: flatNode.points!,
				parent_id: flatNode.parent_id!,
				contributor_ids: objectToArray(flatNode.contributor_ids),
				anti_contributors_ids: objectToArray(flatNode.anti_contributors_ids)
			};
		}
	}

	const rootNode = buildNode(actualRootId);
	return rootNode as RootNode | null;
}

// ============================================================================
// Subscription Management
// ============================================================================

function subscribeToTree() {
	if (!holsterUser.is) {
		console.log('[TREE-HOLSTER] Cannot subscribe: no authenticated user');
		return;
	}

	treeCallback = (data: any) => {
		if (!data) {
			if (!hasReceivedRealData) {
				console.log('[TREE-HOLSTER] Subscription returned null, waiting for network data...');

				const username = holsterUser.is?.username || 'User';
				const now = new Date().toISOString();
				const defaultTree: RootNode = {
					id: 'root',
					name: username,
					type: 'RootNode',
					manual_fulfillment: null,
					children: [],
					created_at: now,
					updated_at: now
				};

				holsterTree.set(defaultTree);
				userTree.set(defaultTree);

				defaultTreePersistTimeout = setTimeout(() => {
					console.log('[TREE-HOLSTER] No network data received, persisting default tree...');
					persistHolsterTree(defaultTree).then(() => {
						console.log('[TREE-HOLSTER] Default tree persisted');
						isLoadingTree.set(false);
					}).catch((err) => {
						console.error('[TREE-HOLSTER] Error persisting default tree:', err);
						isLoadingTree.set(false);
					});
				}, 10000);
			}
			return;
		}

		if (!hasReceivedRealData) {
			console.log('[TREE-HOLSTER] First real data received from network');
			hasReceivedRealData = true;
			if (defaultTreePersistTimeout) {
				clearTimeout(defaultTreePersistTimeout);
				defaultTreePersistTimeout = null;
			}
		}

		// QUEUE updates during persistence to prevent processing incomplete data
		if (isPersisting) {
			const networkTimestamp = getTimestamp(data);

			// Only queue if this is NOT our own write (different timestamp)
			if (networkTimestamp && networkTimestamp !== lastNetworkTimestamp) {
				console.log('[TREE-HOLSTER] External update during persistence - queueing');
				queuedNetworkUpdate = data; // Store latest update (overwrites previous)
			}
			return;
		}

		// Process update immediately if not persisting
		processNetworkUpdate(data);
	};

	holsterUser.get('tree').on(treeCallback, true);
}

// ============================================================================
// localStorage Cache
// ============================================================================

const TREE_CACHE_KEY = 'holster_tree_cache';
const TREE_CACHE_TIMESTAMP_KEY = 'holster_tree_cache_timestamp';

function getCachedTree(): RootNode | null {
	if (typeof localStorage === 'undefined') return null;

	try {
		const cached = localStorage.getItem(TREE_CACHE_KEY);
		if (!cached) return null;

		const parsed = JSON.parse(cached);
		const validation = RootNodeSchema.safeParse(parsed);

		if (validation.success) {
			const timestamp = localStorage.getItem(TREE_CACHE_TIMESTAMP_KEY);
			return validation.data;
		} else {
			console.warn('[TREE-HOLSTER] Invalid cached tree, ignoring');
			return null;
		}
	} catch (error) {
		console.error('[TREE-HOLSTER] Error reading cached tree:', error);
		return null;
	}
}

function setCachedTree(tree: RootNode, timestamp: number): void {
	if (typeof localStorage === 'undefined') return;

	try {
		localStorage.setItem(TREE_CACHE_KEY, JSON.stringify(tree));
		localStorage.setItem(TREE_CACHE_TIMESTAMP_KEY, timestamp.toString());
	} catch (error) {
		console.error('[TREE-HOLSTER] Error caching tree:', error);
	}
}

// ============================================================================
// Initialization
// ============================================================================

export function initializeHolsterTree() {
	if (!holsterUser.is) {
		console.log('[TREE-HOLSTER] Cannot initialize: no authenticated user');
		return;
	}

	if (isInitialized) {
		console.log('[TREE-HOLSTER] Already initialized, skipping duplicate call');
		return;
	}

	console.log('[TREE-HOLSTER] Initializing tree...');
	isInitialized = true;
	isLoadingTree.set(true);

	// Try to load from cache first for instant UI
	const cachedTree = getCachedTree();
	const cachedTimestamp = cachedTree
		? parseInt(localStorage.getItem(TREE_CACHE_TIMESTAMP_KEY) || '0', 10)
		: 0;

	if (cachedTree) {
		holsterTree.set(cachedTree);
		userTree.set(cachedTree);
		lastNetworkTimestamp = cachedTimestamp;

		// Initialize lastPersistedNodes from cached tree for incremental updates
		const flatCached = flattenTree(cachedTree);
		lastPersistedNodes = { ...flatCached.nodes };

		isLoadingTree.set(false);
	}

	subscribeToTree();
}

// ============================================================================
// Cleanup
// ============================================================================

/**
 * Check if persistence is currently in progress
 */
export function isTreePersisting(): boolean {
	return isPersisting;
}

/**
 * Wait for any in-flight persistence to complete
 */
export async function waitForTreePersistence(): Promise<void> {
	if (!isPersisting) return;

	console.log('[TREE-HOLSTER] Waiting for in-flight persistence to complete...');

	// Poll until persistence completes (with timeout)
	const maxWait = 20000; // 20 seconds max
	const startTime = Date.now();

	while (isPersisting && (Date.now() - startTime) < maxWait) {
		await new Promise(resolve => setTimeout(resolve, 100));
	}

	if (isPersisting) {
		console.warn('[TREE-HOLSTER] Persistence did not complete within timeout');
	} else {
		console.log('[TREE-HOLSTER] Persistence completed successfully');
	}
}

export async function cleanupHolsterTree() {
	// Wait for any in-flight persistence to complete before cleaning up
	// This ensures data is saved before logout
	await waitForTreePersistence();

	if (treeCallback && holsterUser.is) {
		holsterUser.get('tree').off(treeCallback);
		treeCallback = null;
	}
	holsterTree.set(null);
	lastNetworkTimestamp = null;
	isInitialized = false; // Reset initialization flag
	hasReceivedRealData = false;

	if (defaultTreePersistTimeout) {
		clearTimeout(defaultTreePersistTimeout);
		defaultTreePersistTimeout = null;
	}

	// Clear pending state and locks
	isPersisting = false;
	hasPendingLocalChanges = false;
	queuedNetworkUpdate = null;
	lastPersistedNodes = {};

	// Clear localStorage cache
	if (typeof localStorage !== 'undefined') {
		localStorage.removeItem(TREE_CACHE_KEY);
		localStorage.removeItem(TREE_CACHE_TIMESTAMP_KEY);
		console.log('[TREE-HOLSTER] Cleared localStorage cache');
	}

	console.log('[TREE-HOLSTER] Cleaned up');
}

/**
 * Reset initialization state (for logout/re-login in same session)
 */
export function resetInitialization() {
	console.log('[TREE-HOLSTER] Resetting initialization state');
	cleanupHolsterTree();
}

// ============================================================================
// Persistence
// ============================================================================

export async function persistHolsterTree(tree?: RootNode): Promise<void> {
	if (!holsterUser.is) {
		console.log('[TREE-HOLSTER] Not authenticated, skipping persistence');
		return;
	}

	// Check if already persisting to prevent concurrent writes
	if (isPersisting) {
		// Mark that we have pending local changes that need to be persisted.
		// This implements eventual consistency: the retry will read the most
		// recent userTree state, not this specific snapshot (which is desired).
		hasPendingLocalChanges = true;
		return;
	}

	if (get(isLoadingTree)) {
		console.log('[TREE-HOLSTER] Still loading, deferring persistence');
		setTimeout(() => {
			if (!get(isLoadingTree)) {
				persistHolsterTree(tree);
			}
		}, 500);
		return;
	}

	const treeToSave = tree || get(userTree);

	if (!treeToSave) {
		console.log('[TREE-HOLSTER] No tree to persist');
		return;
	}

	// Set lock
	isPersisting = true;
	hasPendingLocalChanges = false; // Clear pending flag since we're persisting now

	try {
		// Flatten tree to node collection
		const flatTree = flattenTree(treeToSave);

		const localTimestamp = Date.now();

		// Check if safe to persist
		if (!shouldPersist(localTimestamp, lastNetworkTimestamp)) {
			console.warn('[TREE-HOLSTER] Skipping persist - network has newer data');
			isPersisting = false; // Clear lock
			processQueuedUpdate(); // Process any queued updates
			return;
		}

		// Update lastNetworkTimestamp NOW (before writing) so our own writes
		// coming back via subscription won't be considered "newer"
		lastNetworkTimestamp = localTimestamp;

		// INCREMENTAL UPDATE: Only persist nodes that changed
		return new Promise((resolve, reject) => {
			const currentNodes = flatTree.nodes;
			const previousNodes = lastPersistedNodes;

			// Identify what changed
			const deletedNodeIds: string[] = [];
			const newOrModifiedNodeIds: string[] = [];

			// Find deleted nodes (in previous, not in current)
			for (const oldNodeId of Object.keys(previousNodes)) {
				if (!currentNodes[oldNodeId]) {
					deletedNodeIds.push(oldNodeId);
				}
			}

			// Find new or modified nodes
			for (const [nodeId, currentNode] of Object.entries(currentNodes)) {
				const previousNode = previousNodes[nodeId];
				if (!previousNode || !nodesEqual(currentNode, previousNode)) {
					newOrModifiedNodeIds.push(nodeId);
				}
			}

			// Early return if nothing changed
			if (deletedNodeIds.length === 0 && newOrModifiedNodeIds.length === 0) {
				isPersisting = false; // Clear lock
				processQueuedUpdate(); // Process any queued updates
				return resolve();
			}

			// 1. Delete removed nodes first, then store changed nodes
			let currentIndex = 0;
			const allOperations = [
				...deletedNodeIds.map(id => ({ type: 'delete' as const, id })),
				...newOrModifiedNodeIds.map(id => ({ type: 'write' as const, id }))
			];

			const persistNextOperation = () => {
				if (currentIndex >= allOperations.length) {
					// Update tracked nodes with current state
					lastPersistedNodes = { ...currentNodes };

					// Add delay before metadata to ensure all operations are fully committed
					setTimeout(() => {
						// 2. Store metadata LAST to trigger subscription with complete data
						holsterUser.get('tree').put({ root_id: flatTree.root_id, _updatedAt: localTimestamp }, (err: any) => {
							if (err) {
								console.error('[TREE-HOLSTER] Error persisting tree metadata:', err);
								isPersisting = false; // Clear lock on error
								processQueuedUpdate(); // Process any queued updates even on error
								return reject(err);
							}

							// Cache the tree for faster future loads
							if (treeToSave) {
								setCachedTree(treeToSave, localTimestamp);
							}

							isPersisting = false; // Clear lock on success

							// Process any updates that came in during persistence
							processQueuedUpdate();

							resolve();
						});
					}, 50);
					return;
				}

				const operation = allOperations[currentIndex];

				if (operation.type === 'delete') {
					// Delete node by setting to null
					holsterUser.get('tree').next('nodes').next(operation.id).put(null, (err: any) => {
						if (err) {
							console.error('[TREE-HOLSTER] Error deleting node:', operation.id, err);
							isPersisting = false; // Clear lock on error
							processQueuedUpdate(); // Process any queued updates even on error
							return reject(err);
						}

						currentIndex++;
						// Add small delay to give Holster time to process the deletion
						setTimeout(() => {
							persistNextOperation();
						}, 20);
					});
				} else {
					// Write node data
					holsterUser.get('tree').next('nodes').next(operation.id).put(currentNodes[operation.id], (err: any) => {
						if (err) {
							console.error('[TREE-HOLSTER] Error persisting node:', operation.id, err);
							isPersisting = false; // Clear lock on error
							processQueuedUpdate(); // Process any queued updates even on error
							return reject(err);
						}

						currentIndex++;
						// Add small delay to give Holster time to process the write
						setTimeout(() => {
							persistNextOperation();
						}, 20);
					});
				}
			};

			persistNextOperation();
		});
	} catch (error) {
		console.error('[TREE-HOLSTER] Error processing tree:', error);
		isPersisting = false; // Clear lock on error
		processQueuedUpdate(); // Process any queued updates even on error
		throw error;
	}
}

// ============================================================================
// Cross-User Data Fetching (for Mutual Contributors)
// ============================================================================

/**
 * Subscribe to a mutual contributor's tree from Holster
 * Used to fetch their recognition tree for collective recognition calculations
 */
export function subscribeToContributorHolsterTree(
	contributorPubKey: string,
	onUpdate: (tree: RootNode | null) => void
) {
	if (!holsterUser.is) {
		console.log(`[TREE-HOLSTER] Not authenticated, cannot subscribe to ${contributorPubKey.slice(0, 20)}...`);
		return;
	}


	// Subscribe to this contributor's tree
	holsterUser.get([contributorPubKey, 'tree']).on((treeData) => {
		if (!treeData) {
			console.log(`[TREE-HOLSTER] No tree data from ${contributorPubKey.slice(0, 20)}...`);
			// Call with null to clear
			onUpdate(null);
			return;
		}

		try {
			// Remove timestamp field
			const { _updatedAt, ...dataOnly } = treeData;

			// Extract nodes - filter out metadata fields and null values (deleted nodes)
			const nodes: Record<string, FlatNode> = {};
			if (dataOnly.nodes && typeof dataOnly.nodes === 'object') {
				for (const [key, value] of Object.entries(dataOnly.nodes)) {
					if (!key.startsWith('_') && value && typeof value === 'object') {
						nodes[key] = value as FlatNode;
					}
				}
			}

			// Validate we have nodes and root_id
			if (Object.keys(nodes).length === 0 || !dataOnly.root_id) {
				console.warn(`[TREE-HOLSTER] Invalid tree structure from ${contributorPubKey.slice(0, 20)}... - no nodes or root_id`);
				onUpdate(null);
				return;
			}

			const flatTreeData: FlatTreeData = {
				nodes,
				root_id: dataOnly.root_id
			};

			// Reconstruct tree from flat format
			const reconstructed = reconstructTree(flatTreeData);

			if (!reconstructed) {
				console.warn(`[TREE-HOLSTER] Failed to reconstruct tree from ${contributorPubKey.slice(0, 20)}...`);
				onUpdate(null);
				return;
			}

			// Validate reconstructed tree
			const parseResult = RootNodeSchema.safeParse(reconstructed);
			if (!parseResult.success) {
				console.warn(`[TREE-HOLSTER] Invalid tree from ${contributorPubKey.slice(0, 20)}...`, parseResult.error);
				onUpdate(null);
				return;
			}

			console.log(
				`[TREE-HOLSTER] Received tree from ${contributorPubKey.slice(0, 20)}...:`,
				Object.keys(nodes).length,
				'nodes'
			);

			// Call the update callback
			onUpdate(parseResult.data);
		} catch (error) {
			console.error(`[TREE-HOLSTER] Error processing tree from ${contributorPubKey.slice(0, 20)}...:`, error);
			onUpdate(null);
		}
	});
}
