import * as TreeOps from '../tree';
import type { Node } from '../types';

// Define Gun types
type GunUser = any;
type GunInstance = any;

/**
 * Wrapper class for Gun tree operations with proper subscription management
 * and a more convenient interface
 */
export class TreeWrapper {
	private user: GunUser;
	private gun: GunInstance;
	private subscriptions: Map<string, () => void> = new Map();

	/**
	 * Create a TreeWrapper instance
	 * @param relayUrls Optional relay URLs for Gun
	 */
	constructor(relayUrls?: string[]) {
		const { gun, user } = TreeOps.initGun(relayUrls);
		this.gun = gun;
		this.user = user;
	}

	/**
	 * Authenticate with credentials
	 * @param username Username
	 * @param password Password
	 * @returns Promise that resolves to authenticated user or error
	 */
	async authenticate(username: string, password: string): Promise<boolean> {
		return new Promise((resolve) => {
			this.user.auth(username, password, (ack: any) => {
				resolve(!ack.err);
			});
		});
	}

	/**
	 * Create a root node
	 * @param id Node ID
	 * @param name Node name
	 * @param points Node points
	 * @param contributors Optional contributors
	 * @param manualFulfillment Optional manual fulfillment
	 * @returns Promise that resolves to node ID
	 */
	async createRoot(
		id: string,
		name: string,
		points: number,
		contributors: string[] = [],
		manualFulfillment: number | null = null
	): Promise<string> {
		return TreeOps.createRoot(this.user, id, name, points, contributors, manualFulfillment);
	}

	/**
	 * Add a child node
	 * @param parentId Parent node ID
	 * @param childId Child node ID
	 * @param name Child node name
	 * @param points Child node points
	 * @param contributors Optional contributors
	 * @param manualFulfillment Optional manual fulfillment
	 * @returns Promise that resolves to child ID or null if failed
	 */
	async addChild(
		parentId: string,
		childId: string,
		name: string,
		points: number,
		contributors: string[] = [],
		manualFulfillment: number | null = null
	): Promise<string | null> {
		return TreeOps.addChild(
			this.user,
			parentId,
			childId,
			name,
			points,
			contributors,
			manualFulfillment
		);
	}

	/**
	 * Add contributors to a node and delete its subtree
	 * @param nodeId Node ID
	 * @param contributors Contributors to add
	 * @returns Promise that resolves to success status
	 */
	async addContributors(nodeId: string, contributors: string[]): Promise<boolean> {
		return TreeOps.addContributors(this.user, nodeId, contributors);
	}

	/**
	 * Delete a node's subtree
	 * @param nodeId Node ID
	 * @returns Promise that resolves to success status
	 */
	async deleteSubtree(nodeId: string): Promise<boolean> {
		return TreeOps.deleteSubtree(this.user, nodeId);
	}

	/**
	 * Load a node
	 * @param nodeId Node ID
	 * @returns Promise that resolves to node data or null
	 */
	async loadNode(nodeId: string): Promise<Partial<Node> | null> {
		return TreeOps.loadNode(this.user, nodeId);
	}

	/**
	 * Get a node's children
	 * @param nodeId Node ID
	 * @returns Promise that resolves to child IDs
	 */
	async getChildren(nodeId: string): Promise<string[]> {
		return TreeOps.getChildren(this.user, nodeId);
	}

	/**
	 * Get a node's parent ID
	 * @param nodeId Node ID
	 * @returns Promise that resolves to parent ID or null
	 */
	async getParentId(nodeId: string): Promise<string | null> {
		return TreeOps.getParentId(this.user, nodeId);
	}

	/**
	 * Follow a path from a root node
	 * @param rootId Root node ID
	 * @param path Path to follow
	 * @returns Promise that resolves to final node ID or null
	 */
	async followPath(rootId: string, path: string[]): Promise<string | null> {
		return TreeOps.followPath(this.user, rootId, path);
	}

	/**
	 * Get the path from root to a node
	 * @param nodeId Node ID
	 * @returns Promise that resolves to path
	 */
	async getPathFromRoot(nodeId: string): Promise<string[]> {
		return TreeOps.getPathFromRoot(this.user, nodeId);
	}

	/**
	 * Get a node's siblings
	 * @param nodeId Node ID
	 * @returns Promise that resolves to sibling IDs
	 */
	async getSiblings(nodeId: string): Promise<string[]> {
		return TreeOps.getSiblings(this.user, nodeId);
	}

	/**
	 * Update a node's SOGF cache
	 * @param nodeId Node ID
	 * @param sogfMap SOGF map
	 * @returns Promise that resolves when complete
	 */
	async updateSogfCache(nodeId: string, sogfMap: Record<string, number>): Promise<void> {
		return TreeOps.updateSogfCache(this.user, nodeId, sogfMap);
	}

	/**
	 * Update a node's provider shares cache
	 * @param nodeId Node ID
	 * @param depth Depth
	 * @param sharesMap Shares map
	 * @returns Promise that resolves when complete
	 */
	async updateProviderShares(
		nodeId: string,
		depth: number,
		sharesMap: Record<string, number>
	): Promise<void> {
		return TreeOps.updateProviderShares(this.user, nodeId, depth, sharesMap);
	}

	/**
	 * Build a tree from a node
	 * @param nodeId Root node ID
	 * @param maxDepth Maximum depth to build
	 * @returns Promise that resolves to built tree or null
	 */
	async buildTreeFromNode(nodeId: string, maxDepth = 5): Promise<Node | null> {
		return TreeOps.buildTreeFromNode(this.user, nodeId, maxDepth);
	}

	/**
	 * Subscribe to node changes
	 * @param nodeId Node ID
	 * @param callback Callback to call when node changes
	 * @returns Unique subscription ID
	 */
	subscribeToNode(nodeId: string, callback: (node: Partial<Node> | null) => void): string {
		const subscriptionId = `node_${nodeId}_${Date.now()}`;
		const unsubscribe = TreeOps.subscribeToNode(this.user, nodeId, callback);
		this.subscriptions.set(subscriptionId, unsubscribe);
		return subscriptionId;
	}

	/**
	 * Subscribe to children changes
	 * @param nodeId Node ID
	 * @param callback Callback to call when children change
	 * @returns Unique subscription ID
	 */
	subscribeToChildren(nodeId: string, callback: (childIds: string[]) => void): string {
		const subscriptionId = `children_${nodeId}_${Date.now()}`;
		const unsubscribe = TreeOps.subscribeToChildren(this.user, nodeId, callback);
		this.subscriptions.set(subscriptionId, unsubscribe);
		return subscriptionId;
	}

	/**
	 * Unsubscribe from a subscription
	 * @param subscriptionId Subscription ID
	 * @returns Whether unsubscription was successful
	 */
	unsubscribe(subscriptionId: string): boolean {
		const unsubscribe = this.subscriptions.get(subscriptionId);
		if (unsubscribe) {
			unsubscribe();
			this.subscriptions.delete(subscriptionId);
			return true;
		}
		return false;
	}

	/**
	 * Update a node with partial updates
	 * @param nodeId Node ID
	 * @param updates Updates to apply
	 * @returns Promise that resolves to success status
	 */
	async updateNode(
		nodeId: string,
		updates: {
			name?: string;
			points?: number;
			contributors?: string[];
			manualFulfillment?: number | null;
		}
	): Promise<boolean> {
		return TreeOps.updateNode(this.user, nodeId, updates);
	}

	/**
	 * Cleanup all subscriptions
	 */
	cleanup(): void {
		for (const unsubscribe of this.subscriptions.values()) {
			unsubscribe();
		}
		this.subscriptions.clear();
	}

	/**
	 * Get the Gun instance
	 * @returns Gun instance
	 */
	getGun(): GunInstance {
		return this.gun;
	}

	/**
	 * Get the Gun user
	 * @returns Gun user
	 */
	getUser(): GunUser {
		return this.user;
	}
}
