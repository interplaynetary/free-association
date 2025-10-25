/**
 * Node Store Factory - Turns NodeDataStorage configs into live stores
 * 
 * Bridges the NodeDataStorage schema (declarative config) with 
 * the createStore pattern (imperative store creation).
 * 
 * Each node can declare:
 * - What schema to use (data_schema_type)
 * - Where to sync (holster_path)
 * - How to persist (auto_persist, persist_debounce_ms)
 * - Who to subscribe to (subscribe_to_user)
 * 
 * Architecture:
 * - Schema Registry: Maps type names to Zod schemas for runtime validation
 * - Store Factory: Creates stores from NodeDataStorage configs
 * - Store Manager: Manages lifecycle of multiple node stores
 */

import { createStore, type HolsterStore } from '../utils/store.svelte';
import type { NodeDataStorage } from '../v2/schemas';
import * as z from 'zod';

// Import all protocol schemas (v2)
import {
	CommitmentSchema,
	TwoTierAllocationStateSchema,
	type Commitment,
	type TwoTierAllocationState,
	type AvailabilitySlot,
	type NeedSlot,
	type BaseCapacity,
	type BaseNeed,
	type ResourceMetadata
} from '../v2/schemas';

// Import additional schemas from main schema file
import {
	AvailabilitySlotSchema,
	BaseCapacitySchema,
	ProviderCapacitySchema,
	RecipientCapacitySchema,
	CapacitySchema,
	NeedSlotSchema,
	BaseNeedSchema,
	SlotAllocationResultSchema,
	ProviderAllocationStateSchema,
	ContactSchema,
	ContactsCollectionSchema,
	ChatReadStateSchema,
	ChatReadStatesSchema,
	UserSlotCompositionSchema,
	NetworkSlotCompositionSchema,
	ComplianceFilterSchema,
	AllocationSchema,
	AllocationComputationResultSchema,
	type Contact,
	type ChatReadState
} from '$lib/schema';

// ═══════════════════════════════════════════════════════════════════
// SCHEMA REGISTRY
// ═══════════════════════════════════════════════════════════════════

/**
 * Maps data_schema_type strings to actual Zod schemas
 * Enables runtime schema lookup for dynamic store creation
 */
const SCHEMA_REGISTRY: Record<string, z.ZodTypeAny> = {
	// Commons schemas v2 (from commons/schema-v2.ts)
	'Commitment': CommitmentSchema,
	'TwoTierAllocationState': TwoTierAllocationStateSchema,
	
	// Main schemas (from lib/schema.ts)
	'AvailabilitySlot': AvailabilitySlotSchema,
	'BaseCapacity': BaseCapacitySchema,
	'ProviderCapacity': ProviderCapacitySchema,
	'RecipientCapacity': RecipientCapacitySchema,
	'Capacity': CapacitySchema,
	'NeedSlot': NeedSlotSchema,
	'BaseNeed': BaseNeedSchema,
	'SlotAllocationResult': SlotAllocationResultSchema,
	'ProviderAllocationState': ProviderAllocationStateSchema,
	'Contact': ContactSchema,
	'ContactsCollection': ContactsCollectionSchema,
	'ChatReadState': ChatReadStateSchema,
	'ChatReadStates': ChatReadStatesSchema,
	'UserSlotComposition': UserSlotCompositionSchema,
	'NetworkSlotComposition': NetworkSlotCompositionSchema,
	'ComplianceFilter': ComplianceFilterSchema,
	'Allocation': AllocationSchema,
	'AllocationComputationResult': AllocationComputationResultSchema,
	
	// Generic types for flexible storage
	'String': z.string(),
	'Number': z.number(),
	'Boolean': z.boolean(),
	'Array': z.array(z.any()),
	'Object': z.record(z.string(), z.any()),
	'Any': z.any()
};

/**
 * Register a new schema type at runtime
 * 
 * Allows extending the schema registry with custom types
 */
export function registerSchema(typeName: string, schema: z.ZodTypeAny): void {
	if (SCHEMA_REGISTRY[typeName]) {
		console.warn(`[NODE-STORE] Overwriting existing schema type: ${typeName}`);
	}
	SCHEMA_REGISTRY[typeName] = schema;
}

/**
 * Get schema by type name
 * 
 * Returns null if schema not found
 */
export function getSchema(typeName: string): z.ZodTypeAny | null {
	return SCHEMA_REGISTRY[typeName] || null;
}

/**
 * List all registered schema types
 */
export function listSchemaTypes(): string[] {
	return Object.keys(SCHEMA_REGISTRY);
}

/**
 * Check if a schema type is registered
 */
export function hasSchemaType(typeName: string): boolean {
	return typeName in SCHEMA_REGISTRY;
}

// ═══════════════════════════════════════════════════════════════════
// NODE STORE FACTORY
// ═══════════════════════════════════════════════════════════════════

/**
 * Create a store from a NodeDataStorage configuration
 * 
 * This bridges declarative config (NodeDataStorage) with imperative store creation
 * 
 * Example:
 * ```typescript
 * const nodeConfig: NodeDataStorage = {
 *   holster_path: 'allocation/commitment',
 *   data_schema_type: 'Commitment',
 *   persist_debounce_ms: 100,
 *   auto_persist: true
 * };
 * 
 * const store = createNodeStore(nodeConfig);
 * store?.initialize();
 * ```
 */
export function createNodeStore<T = any>(
	config: NodeDataStorage
): HolsterStore<T> | null {
	// Validate required fields
	if (!config.holster_path) {
		console.error('[NODE-STORE] Missing holster_path in config', config);
		return null;
	}
	
	if (!config.data_schema_type) {
		console.error('[NODE-STORE] Missing data_schema_type in config', config);
		return null;
	}
	
	// Lookup schema from registry
	const schema = getSchema(config.data_schema_type);
	if (!schema) {
		console.error(
			`[NODE-STORE] Unknown schema type: ${config.data_schema_type}`,
			'\nAvailable types:', Object.keys(SCHEMA_REGISTRY)
		);
		return null;
	}
	
	// Create store with config
	const store = createStore({
		holsterPath: config.holster_path,
		schema: schema,
		persistDebounce: config.persist_debounce_ms ?? 0
	});
	
	return store;
}

// ═══════════════════════════════════════════════════════════════════
// NODE STORE MANAGER (For Tree Integration)
// ═══════════════════════════════════════════════════════════════════

/**
 * Manages multiple node stores (for tree integration)
 * 
 * Keeps a registry of live stores by node ID
 * Handles initialization, subscription, and cleanup
 * 
 * Example:
 * ```typescript
 * const manager = new NodeStoreManager();
 * 
 * manager.registerNode('node-1', {
 *   holster_path: 'data/node-1',
 *   data_schema_type: 'Commitment'
 * });
 * 
 * await manager.initializeAll();
 * 
 * const store = manager.getStore('node-1');
 * store?.subscribe(data => console.log(data));
 * 
 * await manager.cleanupAll();
 * ```
 */
export class NodeStoreManager {
	private stores = new Map<string, HolsterStore<any>>();
	private configs = new Map<string, NodeDataStorage>();
	private subscriptions = new Map<string, ((data: any) => void)[]>();
	
	/**
	 * Register a node's data storage config
	 * 
	 * Does not create the store yet - call getStore() or initializeAll() to create
	 */
	registerNode(nodeId: string, config: NodeDataStorage): void {
		this.configs.set(nodeId, config);
	}
	
	/**
	 * Unregister a node (cleanup and remove)
	 */
	async unregisterNode(nodeId: string): Promise<void> {
		const store = this.stores.get(nodeId);
		if (store) {
			await store.cleanup();
			this.stores.delete(nodeId);
		}
		this.configs.delete(nodeId);
		this.subscriptions.delete(nodeId);
	}
	
	/**
	 * Get or create a store for a node
	 * 
	 * Creates store on-demand if config exists
	 * Auto-initializes if auto_persist is enabled
	 */
	getStore<T = any>(nodeId: string): HolsterStore<T> | null {
		// Return existing store if available
		if (this.stores.has(nodeId)) {
			return this.stores.get(nodeId)! as HolsterStore<T>;
		}
		
		// Get config
		const config = this.configs.get(nodeId);
		if (!config) {
			console.error(`[NODE-STORE-MANAGER] No config for node ${nodeId}`);
			return null;
		}
		
		// Create store
		const store = createNodeStore<T>(config);
		if (!store) {
			return null;
		}
		
		// Initialize if auto_persist is enabled (default: true)
		if (config.auto_persist !== false) {
			store.initialize();
		}
		
		// Subscribe to user if specified
		if (config.subscribe_to_user) {
			store.subscribeToUser(config.subscribe_to_user, (data: any) => {
				console.log(
					`[NODE-STORE-MANAGER] ${nodeId} received data from ${config.subscribe_to_user?.slice(0, 20)}...`,
					data
				);
				
				// Trigger any registered callbacks
				const callbacks = this.subscriptions.get(nodeId);
				if (callbacks) {
					callbacks.forEach(callback => callback(data));
				}
			});
		}
		
		// Cache and return
		this.stores.set(nodeId, store);
		return store as HolsterStore<T>;
	}
	
	/**
	 * Get the configuration for a node
	 */
	getConfig(nodeId: string): NodeDataStorage | null {
		return this.configs.get(nodeId) || null;
	}
	
	/**
	 * Update a node's configuration
	 * 
	 * If store already exists, you'll need to unregister and re-register
	 * to apply new configuration
	 */
	updateConfig(nodeId: string, config: Partial<NodeDataStorage>): void {
		const existingConfig = this.configs.get(nodeId);
		if (!existingConfig) {
			console.error(`[NODE-STORE-MANAGER] No config found for node ${nodeId}`);
			return;
		}
		
		this.configs.set(nodeId, { ...existingConfig, ...config });
		
		// Warn if store already created
		if (this.stores.has(nodeId)) {
			console.warn(
				`[NODE-STORE-MANAGER] Config updated for ${nodeId}, but store already exists.`,
				'Call unregisterNode() then registerNode() to apply changes.'
			);
		}
	}
	
	/**
	 * Subscribe to a node's data updates
	 * 
	 * Callback will be called when data changes
	 */
	subscribe(nodeId: string, callback: (data: any) => void): () => void {
		if (!this.subscriptions.has(nodeId)) {
			this.subscriptions.set(nodeId, []);
		}
		
		this.subscriptions.get(nodeId)!.push(callback);
		
		// Return unsubscribe function
		return () => {
			const callbacks = this.subscriptions.get(nodeId);
			if (callbacks) {
				const index = callbacks.indexOf(callback);
				if (index > -1) {
					callbacks.splice(index, 1);
				}
			}
		};
	}
	
	/**
	 * Initialize all registered nodes
	 * 
	 * Creates stores for all registered configs
	 */
	async initializeAll(): Promise<void> {
		console.log(`[NODE-STORE-MANAGER] Initializing ${this.configs.size} nodes...`);
		
		let successCount = 0;
		let failCount = 0;
		
		for (const [nodeId, config] of this.configs) {
			const store = this.getStore(nodeId);
			if (store) {
				successCount++;
			} else {
				failCount++;
				console.warn(`[NODE-STORE-MANAGER] Failed to create store for ${nodeId}`);
			}
		}
		
		console.log(
			`[NODE-STORE-MANAGER] Initialization complete: ${successCount} succeeded, ${failCount} failed`
		);
	}
	
	/**
	 * Cleanup all stores
	 * 
	 * Waits for in-flight persistence operations to complete
	 */
	async cleanupAll(): Promise<void> {
		console.log(`[NODE-STORE-MANAGER] Cleaning up ${this.stores.size} stores...`);
		
		await Promise.all(
			Array.from(this.stores.values()).map(store => store.cleanup())
		);
		
		this.stores.clear();
		this.configs.clear();
		this.subscriptions.clear();
		
		console.log('[NODE-STORE-MANAGER] Cleanup complete');
	}
	
	/**
	 * Get all active stores
	 */
	getAllStores(): Map<string, HolsterStore<any>> {
		return new Map(this.stores);
	}
	
	/**
	 * Get all registered node IDs
	 */
	getAllNodeIds(): string[] {
		return Array.from(this.configs.keys());
	}
	
	/**
	 * Get all active store IDs (subset of registered nodes)
	 */
	getActiveNodeIds(): string[] {
		return Array.from(this.stores.keys());
	}
	
	/**
	 * Check if a node has a config registered
	 */
	hasConfig(nodeId: string): boolean {
		return this.configs.has(nodeId);
	}
	
	/**
	 * Check if a node has a store created
	 */
	hasStore(nodeId: string): boolean {
		return this.stores.has(nodeId);
	}
	
	/**
	 * Get statistics about the manager
	 */
	getStats() {
		return {
			registeredNodes: this.configs.size,
			activeStores: this.stores.size,
			subscriptions: this.subscriptions.size,
			nodeIds: this.getAllNodeIds()
		};
	}
	
	/**
	 * Wait for all stores to finish persisting
	 */
	async waitForAllPersistence(): Promise<void> {
		await Promise.all(
			Array.from(this.stores.values()).map(store => store.waitForPersistence())
		);
	}
	
	/**
	 * Force persistence of all stores
	 */
	async persistAll(): Promise<void> {
		await Promise.all(
			Array.from(this.stores.values()).map(store => store.persist())
		);
	}
	
	// ════════════════════════════════════════════════════════════
	// CONTRIBUTOR MANAGEMENT (Convenience Methods)
	// ════════════════════════════════════════════════════════════
	
	/**
	 * Add a contributor to a managed node
	 * 
	 * @param nodeId - The managed node ID
	 * @param contributorId - The contributor pubkey/ID to add
	 * @param asAntiContributor - If true, adds as anti-contributor
	 * @param weight - Optional weight for the contributor
	 * @returns Success boolean
	 */
	addContributorToNode(
		nodeId: string,
		contributorId: string,
		asAntiContributor: boolean = false,
		weight?: number
	): boolean {
		const store = this.getStore(nodeId);
		if (!store) {
			console.error(`[NODE-STORE-MANAGER] No store found for node ${nodeId}`);
			return false;
		}
		
		// Get current node data from store
		let nodeData: any = null;
		const unsubscribe = store.subscribe((data: any) => { nodeData = data; });
		unsubscribe();
		
		if (!nodeData) {
			console.error(`[NODE-STORE-MANAGER] No data in store for node ${nodeId}`);
			return false;
		}
		
		// Add contributor
		const added = addSubscriberAsContributor(nodeData, contributorId, asAntiContributor);
		if (!added) {
			return false;
		}
		
		// Set weight if provided
		if (weight !== undefined) {
			setContributorWeight(nodeData, contributorId, weight);
		}
		
		// Update store
		store.set(nodeData);
		return true;
	}
	
	/**
	 * Add a subscriber as contributor and auto-subscribe to their data
	 * 
	 * Convenience method that:
	 * 1. Adds the pubkey to the node's contributor list
	 * 2. Updates the node config to subscribe to that pubkey
	 * 3. Re-creates the store with subscription enabled
	 * 
	 * @param nodeId - The managed node ID
	 * @param pubkey - The pubkey to subscribe to
	 * @param asAntiContributor - If true, adds as anti-contributor
	 * @param weight - Optional weight for the contributor
	 */
	async addSubscriberAsContributor(
		nodeId: string,
		pubkey: string,
		asAntiContributor: boolean = false,
		weight?: number
	): Promise<boolean> {
		const config = this.getConfig(nodeId);
		if (!config) {
			console.error(`[NODE-STORE-MANAGER] No config found for node ${nodeId}`);
			return false;
		}
		
		const store = this.getStore(nodeId);
		if (!store) {
			console.error(`[NODE-STORE-MANAGER] No store found for node ${nodeId}`);
			return false;
		}
		
		// Get current node data
		let nodeData: any = null;
		const unsubscribe = store.subscribe((data: any) => { nodeData = data; });
		unsubscribe();
		
		if (!nodeData) {
			console.error(`[NODE-STORE-MANAGER] No data in store for node ${nodeId}`);
			return false;
		}
		
		// Add contributor to node data
		const added = addSubscriberAsContributor(nodeData, pubkey, asAntiContributor);
		if (!added) {
			return false;
		}
		
		// Set weight if provided
		if (weight !== undefined) {
			setContributorWeight(nodeData, pubkey, weight);
		}
		
		// Update store data
		store.set(nodeData);
		
		// Update config to subscribe to this user
		config.subscribe_to_user = pubkey;
		
		// Unregister and re-register to apply new subscription
		await this.unregisterNode(nodeId);
		this.registerNode(nodeId, config);
		
		// Re-create store with new config
		const newStore = this.getStore(nodeId);
		if (newStore) {
			// Subscribe to user
			newStore.subscribeToUser(pubkey, (data: any) => {
				console.log(
					`[NODE-STORE-MANAGER] ${nodeId} received data from ${pubkey.slice(0, 20)}...`,
					data
				);
			});
		}
		
		console.log(
			`[NODE-STORE-MANAGER] Added ${pubkey.slice(0, 20)}... as ${asAntiContributor ? 'anti-' : ''}contributor with auto-subscribe to node ${nodeId}`
		);
		
		return true;
	}
	
	/**
	 * Remove a contributor from a managed node
	 * 
	 * @param nodeId - The managed node ID
	 * @param contributorId - The contributor to remove
	 * @param fromAntiContributors - If true, removes from anti-contributors
	 */
	removeContributorFromNode(
		nodeId: string,
		contributorId: string,
		fromAntiContributors: boolean = false
	): boolean {
		const store = this.getStore(nodeId);
		if (!store) {
			console.error(`[NODE-STORE-MANAGER] No store found for node ${nodeId}`);
			return false;
		}
		
		// Get current node data
		let nodeData: any = null;
		const unsubscribe = store.subscribe((data: any) => { nodeData = data; });
		unsubscribe();
		
		if (!nodeData) {
			console.error(`[NODE-STORE-MANAGER] No data in store for node ${nodeId}`);
			return false;
		}
		
		// Remove contributor
		const removed = removeContributor(nodeData, contributorId, fromAntiContributors);
		if (!removed) {
			return false;
		}
		
		// Update store
		store.set(nodeData);
		return true;
	}
	
	/**
	 * Set points for a managed node
	 * 
	 * @param nodeId - The managed node ID
	 * @param points - The points value
	 */
	setPointsForNode(nodeId: string, points: number): boolean {
		const store = this.getStore(nodeId);
		if (!store) {
			console.error(`[NODE-STORE-MANAGER] No store found for node ${nodeId}`);
			return false;
		}
		
		// Get current node data
		let nodeData: any = null;
		const unsubscribe = store.subscribe((data: any) => { nodeData = data; });
		unsubscribe();
		
		if (!nodeData) {
			console.error(`[NODE-STORE-MANAGER] No data in store for node ${nodeId}`);
			return false;
		}
		
		// Set points
		const success = setNodePoints(nodeData, points);
		if (!success) {
			return false;
		}
		
		// Update store
		store.set(nodeData);
		return true;
	}
	
	/**
	 * Set weight for a contributor in a managed node
	 * 
	 * @param nodeId - The managed node ID
	 * @param contributorId - The contributor ID
	 * @param weight - The weight value
	 */
	setContributorWeightForNode(
		nodeId: string,
		contributorId: string,
		weight: number
	): boolean {
		const store = this.getStore(nodeId);
		if (!store) {
			console.error(`[NODE-STORE-MANAGER] No store found for node ${nodeId}`);
			return false;
		}
		
		// Get current node data
		let nodeData: any = null;
		const unsubscribe = store.subscribe((data: any) => { nodeData = data; });
		unsubscribe();
		
		if (!nodeData) {
			console.error(`[NODE-STORE-MANAGER] No data in store for node ${nodeId}`);
			return false;
		}
		
		// Set weight
		const success = setContributorWeight(nodeData, contributorId, weight);
		if (!success) {
			return false;
		}
		
		// Update store
		store.set(nodeData);
		return true;
	}
	
	/**
	 * Get all contributors for a managed node
	 * 
	 * Returns both positive and anti-contributors
	 */
	getContributorsForNode(nodeId: string): {
		contributors: string[];
		antiContributors: string[];
		weights: ContributorWeights;
	} | null {
		const store = this.getStore(nodeId);
		if (!store) {
			return null;
		}
		
		// Get current node data
		let nodeData: any = null;
		const unsubscribe = store.subscribe((data: any) => { nodeData = data; });
		unsubscribe();
		
		if (!nodeData || nodeData.type !== 'NonRootNode') {
			return null;
		}
		
		return {
			contributors: nodeData.contributor_ids || [],
			antiContributors: nodeData.anti_contributors_ids || [],
			weights: getAllContributorWeights(nodeData)
		};
	}
}

// ═══════════════════════════════════════════════════════════════════
// SINGLETON MANAGER
// ═══════════════════════════════════════════════════════════════════

/**
 * Global singleton node store manager
 * 
 * Use this for application-wide node store management
 */
export const nodeStoreManager = new NodeStoreManager();

// ═══════════════════════════════════════════════════════════════════
// UTILITY FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Create a typed node store factory
 * 
 * Returns a function that creates stores with compile-time type checking
 * 
 * Example:
 * ```typescript
 * const createCommitmentStore = createTypedNodeStoreFactory<Commitment>('Commitment');
 * const store = createCommitmentStore('allocation/commitment', { persistDebounce: 100 });
 * ```
 */
export function createTypedNodeStoreFactory<T>(schemaType: string) {
	return (
		holsterPath: string,
		options: {
			persistDebounce?: number;
			autoPersist?: boolean;
			subscribeToUser?: string;
		} = {}
	): HolsterStore<T> | null => {
		const config: NodeDataStorage = {
			holster_path: holsterPath,
			data_schema_type: schemaType,
			persist_debounce_ms: options.persistDebounce ?? 0,
			auto_persist: options.autoPersist ?? true,
			subscribe_to_user: options.subscribeToUser
		};
		
		return createNodeStore<T>(config);
	};
}

/**
 * Validate data against a schema type
 * 
 * Useful for pre-validation before setting store data
 */
export function validateAgainstSchema(
	data: any,
	schemaType: string
): { success: boolean; data?: any; error?: z.ZodError } {
	const schema = getSchema(schemaType);
	if (!schema) {
		return {
			success: false,
			error: new z.ZodError([
				{
					code: 'custom',
					message: `Unknown schema type: ${schemaType}`,
					path: []
				}
			])
		};
	}
	
	const result = schema.safeParse(data);
	return result.success
		? { success: true, data: result.data }
		: { success: false, error: result.error };
}

// ═══════════════════════════════════════════════════════════════════
// NODE CONTRIBUTOR MANAGEMENT
// ═══════════════════════════════════════════════════════════════════

/**
 * Add a subscriber as a contributor to a node
 * 
 * Useful when subscribing to someone's data and wanting to track them
 * as a contributor to the node
 * 
 * @param node - The node to add contributor to (must be NonRootNode)
 * @param pubkey - The pubkey to add as contributor
 * @param asAntiContributor - If true, adds as anti-contributor instead
 * @returns Success boolean
 */
export function addSubscriberAsContributor(
	node: any,
	pubkey: string,
	asAntiContributor: boolean = false
): boolean {
	// Validate node type
	if (node.type !== 'NonRootNode') {
		console.error('[NODE-STORE] Can only add contributors to NonRootNode');
		return false;
	}
	
	// Initialize arrays if they don't exist
	if (!node.contributor_ids) {
		node.contributor_ids = [];
	}
	if (!node.anti_contributors_ids) {
		node.anti_contributors_ids = [];
	}
	
	if (asAntiContributor) {
		// Add as anti-contributor
		if (!node.anti_contributors_ids.includes(pubkey)) {
			node.anti_contributors_ids.push(pubkey);
			console.log(`[NODE-STORE] Added ${pubkey.slice(0, 20)}... as anti-contributor to node ${node.id}`);
			return true;
		}
	} else {
		// Add as positive contributor
		if (!node.contributor_ids.includes(pubkey)) {
			node.contributor_ids.push(pubkey);
			console.log(`[NODE-STORE] Added ${pubkey.slice(0, 20)}... as contributor to node ${node.id}`);
			return true;
		}
	}
	
	return false; // Already exists
}

/**
 * Remove a contributor from a node
 * 
 * @param node - The node to remove contributor from
 * @param pubkey - The pubkey to remove
 * @param fromAntiContributors - If true, removes from anti-contributors instead
 * @returns Success boolean
 */
export function removeContributor(
	node: any,
	pubkey: string,
	fromAntiContributors: boolean = false
): boolean {
	if (node.type !== 'NonRootNode') {
		console.error('[NODE-STORE] Can only remove contributors from NonRootNode');
		return false;
	}
	
	if (fromAntiContributors) {
		const index = node.anti_contributors_ids?.indexOf(pubkey);
		if (index !== undefined && index > -1) {
			node.anti_contributors_ids.splice(index, 1);
			console.log(`[NODE-STORE] Removed ${pubkey.slice(0, 20)}... from anti-contributors`);
			return true;
		}
	} else {
		const index = node.contributor_ids?.indexOf(pubkey);
		if (index !== undefined && index > -1) {
			node.contributor_ids.splice(index, 1);
			console.log(`[NODE-STORE] Removed ${pubkey.slice(0, 20)}... from contributors`);
			return true;
		}
	}
	
	return false;
}

/**
 * Set points for a node (NonRootNode only)
 * 
 * @param node - The node to set points on
 * @param points - The points value (must be >= 0)
 * @returns Success boolean
 */
export function setNodePoints(node: any, points: number): boolean {
	if (node.type !== 'NonRootNode') {
		console.error('[NODE-STORE] Can only set points on NonRootNode');
		return false;
	}
	
	if (points < 0) {
		console.error('[NODE-STORE] Points must be non-negative');
		return false;
	}
	
	node.points = points;
	console.log(`[NODE-STORE] Set points to ${points} for node ${node.id}`);
	return true;
}

/**
 * Contributor weight management for nodes
 * 
 * Manages a mapping of contributor → weight (points/share)
 * Stored in node's data_storage metadata
 */
export interface ContributorWeights {
	[contributorId: string]: number;
}

/**
 * Set weight for a specific contributor in a node
 * 
 * Weights are stored in node.data_storage.contributor_weights
 * Useful for weighted recognition or allocation
 * 
 * @param node - The node with contributors
 * @param contributorId - The contributor's ID (pubkey or contact_id)
 * @param weight - The weight value (typically 0-1 or point value)
 * @returns Success boolean
 */
export function setContributorWeight(
	node: any,
	contributorId: string,
	weight: number
): boolean {
	if (node.type !== 'NonRootNode') {
		console.error('[NODE-STORE] Can only set contributor weights on NonRootNode');
		return false;
	}
	
	// Verify contributor exists
	const isContributor = node.contributor_ids?.includes(contributorId);
	const isAntiContributor = node.anti_contributors_ids?.includes(contributorId);
	
	if (!isContributor && !isAntiContributor) {
		console.warn(
			`[NODE-STORE] ${contributorId.slice(0, 20)}... is not a contributor to node ${node.id}`
		);
	}
	
	// Initialize contributor_weights if needed
	if (!node.contributor_weights) {
		node.contributor_weights = {};
	}
	
	node.contributor_weights[contributorId] = weight;
	console.log(
		`[NODE-STORE] Set weight ${weight} for ${contributorId.slice(0, 20)}... in node ${node.id}`
	);
	return true;
}

/**
 * Get weight for a specific contributor
 * 
 * Returns null if no weight set
 */
export function getContributorWeight(node: any, contributorId: string): number | null {
	if (node.type !== 'NonRootNode') {
		return null;
	}
	
	return node.contributor_weights?.[contributorId] ?? null;
}

/**
 * Get all contributor weights for a node
 * 
 * Returns empty object if no weights set
 */
export function getAllContributorWeights(node: any): ContributorWeights {
	if (node.type !== 'NonRootNode') {
		return {};
	}
	
	return node.contributor_weights ?? {};
}

/**
 * Normalize contributor weights to sum to 1.0
 * 
 * Useful for converting absolute points to percentages
 */
export function normalizeContributorWeights(node: any): boolean {
	if (node.type !== 'NonRootNode' || !node.contributor_weights) {
		return false;
	}
	
	const weights = node.contributor_weights;
	const total = Object.values(weights).reduce((sum: number, w: any) => sum + (w as number), 0);
	
	if (total === 0) {
		console.warn('[NODE-STORE] Cannot normalize - total weight is 0');
		return false;
	}
	
	for (const contributorId in weights) {
		weights[contributorId] = weights[contributorId] / total;
	}
	
	console.log(`[NODE-STORE] Normalized contributor weights for node ${node.id}`);
	return true;
}

/**
 * Helper: Add subscriber as contributor and auto-subscribe to their data
 * 
 * Convenience function that both:
 * 1. Adds the pubkey as a contributor to the node
 * 2. Sets up the node config to subscribe to that pubkey's data
 * 
 * @param nodeConfig - The NodeDataStorage config to update
 * @param node - The actual node data to add contributor to
 * @param pubkey - The pubkey to subscribe to and add as contributor
 * @param weight - Optional weight for the contributor
 * @param asAntiContributor - If true, adds as anti-contributor
 */
export function addSubscriberAsContributorWithAutoSubscribe(
	nodeConfig: NodeDataStorage,
	node: any,
	pubkey: string,
	weight?: number,
	asAntiContributor: boolean = false
): boolean {
	// Add to node's contributor list
	const added = addSubscriberAsContributor(node, pubkey, asAntiContributor);
	
	if (!added) {
		return false;
	}
	
	// Set weight if provided
	if (weight !== undefined) {
		setContributorWeight(node, pubkey, weight);
	}
	
	// Update node config to subscribe to this pubkey
	nodeConfig.subscribe_to_user = pubkey;
	
	console.log(
		`[NODE-STORE] Added ${pubkey.slice(0, 20)}... as ${asAntiContributor ? 'anti-' : ''}contributor with auto-subscribe`
	);
	
	return true;
}

