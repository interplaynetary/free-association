import { TreeWrapper } from './TreeWrapper';
import type { Node } from '../types';

/**
 * TreeNavigator provides a zipper-like navigation interface using Gun
 * It maintains a current node and navigation history to allow traversal
 * similar to the immutable zipper approach but backed by Gun
 */
export class TreeNavigator {
	private treeWrapper: TreeWrapper;
	private currentNodeId: string;
	private history: string[] = [];
	private nodeSubscriptionId: string | null = null;
	private childrenSubscriptionId: string | null = null;

	/**
	 * Create a TreeNavigator instance
	 * @param treeWrapper TreeWrapper instance
	 * @param rootNodeId ID of the root node to start navigation from
	 */
	constructor(treeWrapper: TreeWrapper, rootNodeId: string) {
		this.treeWrapper = treeWrapper;
		this.currentNodeId = rootNodeId;
	}

	/**
	 * Get the current node ID
	 * @returns Current node ID
	 */
	getCurrentNodeId(): string {
		return this.currentNodeId;
	}

	/**
	 * Load the current node data
	 * @returns Promise that resolves to current node data
	 */
	async getCurrentNode(): Promise<Partial<Node> | null> {
		return this.treeWrapper.loadNode(this.currentNodeId);
	}

	/**
	 * Enter a child node
	 * @param childId Child node ID
	 * @returns Promise that resolves to success status
	 */
	async enterChild(childId: string): Promise<boolean> {
		const children = await this.treeWrapper.getChildren(this.currentNodeId);

		if (!children.includes(childId)) {
			return false;
		}

		this.history.push(this.currentNodeId);
		this.currentNodeId = childId;
		return true;
	}

	/**
	 * Exit to the parent node
	 * @returns Promise that resolves to success status
	 */
	async exitToParent(): Promise<boolean> {
		const parentId = await this.treeWrapper.getParentId(this.currentNodeId);

		if (!parentId) {
			return false; // No parent exists (at root)
		}

		this.currentNodeId = parentId;
		if (this.history.length > 0 && this.history[this.history.length - 1] === parentId) {
			this.history.pop(); // Remove from history if it matches
		}

		return true;
	}

	/**
	 * Navigate to a sibling node
	 * @param siblingId Sibling node ID
	 * @returns Promise that resolves to success status
	 */
	async enterSibling(siblingId: string): Promise<boolean> {
		const parentId = await this.treeWrapper.getParentId(this.currentNodeId);

		if (!parentId) {
			return false; // No parent, can't have siblings
		}

		const siblings = await this.treeWrapper.getChildren(parentId);

		if (!siblings.includes(siblingId)) {
			return false; // Not a valid sibling
		}

		// No need to update history as we're staying at the same level
		this.currentNodeId = siblingId;
		return true;
	}

	/**
	 * Navigate to the root node
	 * @returns Promise that resolves when complete
	 */
	async goToRoot(): Promise<void> {
		// Find the root by traversing up until no parent exists
		let currentId = this.currentNodeId;
		let parentId = await this.treeWrapper.getParentId(currentId);

		while (parentId) {
			currentId = parentId;
			parentId = await this.treeWrapper.getParentId(currentId);
		}

		this.currentNodeId = currentId;
		this.history = []; // Clear history when we go to root
	}

	/**
	 * Follow a path from the current node
	 * @param path Path to follow
	 * @returns Promise that resolves to success status
	 */
	async followPath(path: string[]): Promise<boolean> {
		if (path.length === 0) {
			return true; // Empty path is always successful
		}

		let currentId = this.currentNodeId;

		for (const childId of path) {
			const children = await this.treeWrapper.getChildren(currentId);

			if (!children.includes(childId)) {
				return false; // Invalid path
			}

			this.history.push(currentId);
			currentId = childId;
		}

		this.currentNodeId = currentId;
		return true;
	}

	/**
	 * Get the current path from root
	 * @returns Promise that resolves to path
	 */
	async getCurrentPath(): Promise<string[]> {
		return this.treeWrapper.getPathFromRoot(this.currentNodeId);
	}

	/**
	 * Get children of the current node
	 * @returns Promise that resolves to child IDs
	 */
	async getChildren(): Promise<string[]> {
		return this.treeWrapper.getChildren(this.currentNodeId);
	}

	/**
	 * Get siblings of the current node
	 * @returns Promise that resolves to sibling IDs
	 */
	async getSiblings(): Promise<string[]> {
		return this.treeWrapper.getSiblings(this.currentNodeId);
	}

	/**
	 * Add a child to the current node
	 * @param childId Child node ID
	 * @param name Child node name
	 * @param points Child node points
	 * @param contributors Optional contributors
	 * @param manualFulfillment Optional manual fulfillment
	 * @returns Promise that resolves to child ID or null if failed
	 */
	async addChild(
		childId: string,
		name: string,
		points: number,
		contributors: string[] = [],
		manualFulfillment: number | null = null
	): Promise<string | null> {
		return this.treeWrapper.addChild(
			this.currentNodeId,
			childId,
			name,
			points,
			contributors,
			manualFulfillment
		);
	}

	/**
	 * Add contributors to the current node
	 * @param contributors Contributors to add
	 * @returns Promise that resolves to success status
	 */
	async addContributors(contributors: string[]): Promise<boolean> {
		return this.treeWrapper.addContributors(this.currentNodeId, contributors);
	}

	/**
	 * Delete the subtree of the current node
	 * @returns Promise that resolves to success status
	 */
	async deleteSubtree(): Promise<boolean> {
		return this.treeWrapper.deleteSubtree(this.currentNodeId);
	}

	/**
	 * Update the current node
	 * @param updates Updates to apply
	 * @returns Promise that resolves to success status
	 */
	async updateNode(updates: {
		name?: string;
		points?: number;
		contributors?: string[];
		manualFulfillment?: number | null;
	}): Promise<boolean> {
		return this.treeWrapper.updateNode(this.currentNodeId, updates);
	}

	/**
	 * Subscribe to changes on the current node
	 * @param callback Callback to call when node changes
	 * @returns This navigator instance for chaining
	 */
	subscribeToCurrentNode(callback: (node: Partial<Node> | null) => void): TreeNavigator {
		// Clean up any existing subscription
		if (this.nodeSubscriptionId) {
			this.treeWrapper.unsubscribe(this.nodeSubscriptionId);
		}

		this.nodeSubscriptionId = this.treeWrapper.subscribeToNode(this.currentNodeId, callback);
		return this;
	}

	/**
	 * Subscribe to changes in the current node's children
	 * @param callback Callback to call when children change
	 * @returns This navigator instance for chaining
	 */
	subscribeToCurrentNodeChildren(callback: (childIds: string[]) => void): TreeNavigator {
		// Clean up any existing subscription
		if (this.childrenSubscriptionId) {
			this.treeWrapper.unsubscribe(this.childrenSubscriptionId);
		}

		this.childrenSubscriptionId = this.treeWrapper.subscribeToChildren(
			this.currentNodeId,
			callback
		);
		return this;
	}

	/**
	 * Move to a different node
	 * @param nodeId Node ID to move to
	 * @returns Promise that resolves to success status
	 */
	async moveTo(nodeId: string): Promise<boolean> {
		// Verify node exists
		const node = await this.treeWrapper.loadNode(nodeId);
		if (!node) {
			return false;
		}

		this.history.push(this.currentNodeId);
		this.currentNodeId = nodeId;
		return true;
	}

	/**
	 * Go back in navigation history
	 * @returns Whether navigation was successful
	 */
	goBack(): boolean {
		if (this.history.length === 0) {
			return false;
		}

		this.currentNodeId = this.history.pop()!;
		return true;
	}

	/**
	 * Clean up all subscriptions
	 */
	cleanup(): void {
		if (this.nodeSubscriptionId) {
			this.treeWrapper.unsubscribe(this.nodeSubscriptionId);
			this.nodeSubscriptionId = null;
		}

		if (this.childrenSubscriptionId) {
			this.treeWrapper.unsubscribe(this.childrenSubscriptionId);
			this.childrenSubscriptionId = null;
		}
	}
}
