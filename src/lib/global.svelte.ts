import { browser } from '$app/environment';
import { get, writable, derived, type Writable, type Readable } from 'svelte/store';
import {
	type Node,
	type RootNode,
	type NonRootNode,
	findNodeById,
	getPathToNode,
	getParentNode,
	createRootNode,
	createNonRootNode,
	addChild,
	updateNodeById,
	updateName,
	updatePoints,
	updateManualFulfillment,
	deleteSubtree,
	addContributors,
	validateManualFulfillment,
	fulfilled,
	desire,
	updateNodeById as updateNode
} from '$lib/protocol/protocol';

// Auth.js user data types
export interface UserData {
	id: string; // Using Auth.js standard format
	name: string | null;
	email: string | null;
	image: string | null;
}

/**
 * Core Reactive Stores
 *
 * These stores form the foundation of our state management:
 * - currentUser: The authenticated user from Auth.js
 * - userTree: The complete tree structure for the current user
 * - currentPath: The navigation path (array of node IDs) in the tree
 */
export const currentUser: Writable<UserData | null> = writable(null);
export const userTree: Writable<RootNode | null> = writable(null);
export const currentPath: Writable<string[]> = writable([]);

/**
 * Derived Stores
 *
 * These stores are computed from the core stores and automatically update
 * when their dependencies change.
 */

// Current node ID (last element in the path)
export const currentNodeId: Readable<string | null> = derived(currentPath, ($path) =>
	$path.length > 0 ? $path[$path.length - 1] : null
);

// Current node object (found by ID in the tree)
export const currentNode: Readable<Node | null> = derived(
	[userTree, currentNodeId],
	([$tree, $nodeId]) => {
		if (!$tree || !$nodeId) return null;
		return findNodeById($tree, $nodeId);
	}
);

// Children of the current node
export const childNodes: Readable<Node[]> = derived(currentNode, ($node) =>
	$node ? $node.children : []
);

// Path information with node names for breadcrumb navigation
export const pathInfo: Readable<Array<{ id: string; name: string }>> = derived(
	[userTree, currentPath],
	([$tree, $path]) => {
		if (!$tree || $path.length === 0) return [];

		return $path.map((id) => {
			const node = findNodeById($tree, id);
			return {
				id,
				name: node ? node.name : 'Unknown'
			};
		});
	}
);

// Node fulfillment mapping
export const nodeFulfillment: Readable<Record<string, number>> = derived(userTree, ($tree) => {
	if (!$tree) return {};

	const result: Record<string, number> = {};

	// Calculate fulfillment for the root
	result[$tree.id] = 1.0; // Root nodes are always fully fulfilled

	// Process all children recursively
	function processNode(node: Node) {
		for (const child of node.children) {
			// Only calculate if the tree is not null
			if ($tree) {
				result[child.id] = fulfilled(child, $tree);
			} else {
				result[child.id] = 0; // Default value if tree is null
			}
			processNode(child);
		}
	}

	processNode($tree);
	return result;
});

// Node desire mapping (unfulfilled need)
export const nodeDesire: Readable<Record<string, number>> = derived(
	[userTree, nodeFulfillment],
	([$tree, $fulfillment]) => {
		const result: Record<string, number> = {};

		for (const [id, fulfillValue] of Object.entries($fulfillment)) {
			result[id] = 1.0 - fulfillValue;
		}

		return result;
	}
);

type ToastType = 'info' | 'success' | 'warning' | 'error';

/**
 * Global State
 *
 * A reactive object that combines:
 * 1. Derived values from stores
 * 2. UI state
 * 3. Operations that modify the tree and sync with the API
 */
export const globalState = $state({
	// UI state
	deleteMode: false,
	nodeToEdit: '',
	initializationStarted: false,
	toast: {
		visible: false,
		message: '',
		type: 'info' as ToastType,
		timeoutId: null as number | null
	},

	// Store accessors
	get tree(): RootNode | null {
		return get(userTree);
	},
	get user(): UserData | null {
		return get(currentUser);
	},
	get path(): string[] {
		return get(currentPath);
	},
	get currentNodeId(): string | null {
		return get(currentNodeId);
	},
	get currentNode(): Node | null {
		return get(currentNode);
	},
	get childNodes(): Node[] {
		return get(childNodes);
	},
	get pathInfo(): Array<{ id: string; name: string }> {
		return get(pathInfo);
	},

	// Protocol utilities
	getNodeFulfillment(nodeId: string): number {
		const fulfillmentMap = get(nodeFulfillment);
		return fulfillmentMap[nodeId] || 0;
	},

	getNodeDesire(nodeId: string): number {
		const desireMap = get(nodeDesire);
		return desireMap[nodeId] || 1.0;
	},

	/**
	 * Initialization and Authentication
	 */

	// Initialize the system
	initialize: async () => {
		// Set flag to prevent duplicate initializations
		globalState.initializationStarted = true;

		try {
			console.log('Initializing global state...');

			// Check for user session using Auth.js endpoint
			const response = await fetch('/auth/session', {
				credentials: 'include'
			});

			if (response.ok) {
				const session = await response.json();
				if (session && session.user) {
					console.log('User session found, loading user data...');

					// Set user data
					currentUser.set({
						id: session.user.id || '',
						name: session.user.name,
						email: session.user.email,
						image: session.user.image
					});

					// Load user tree
					await globalState.loadUserTree({
						id: session.user.id,
						name: session.user.name,
						email: session.user.email,
						image: session.user.image
					});

					console.log('Initialization complete');
					return true;
				}
			}
			return false;
		} catch (error) {
			console.error('Error initializing global state:', error);
			globalState.showToast('Failed to initialize application', 'error');
			return false;
		}
	},

	// Load user tree
	loadUserTree: async (userData: UserData) => {
		if (!userData) return;

		try {
			const userId = userData.id;

			try {
				// Get the user's entire tree structure using the tree API
				const response = await fetch(`/api/tree/${userId}`, {
					credentials: 'include'
				});

				if (response.ok) {
					const data = await response.json();

					if (data.success) {
						// Store the tree in the reactive store
						userTree.set(data.data);

						// Set initial path to root
						currentPath.set([userId]);

						console.log('User tree fetched successfully');
						return;
					}
				}

				// Tree not found, create a new one
				console.log('No tree found for user, creating new tree:', userData.name);

				// Create a new root node using protocol function
				const newTree = createRootNode(userId, userData.name || 'My Tree', userId);

				// Set the tree in memory first
				userTree.set(newTree);
				currentPath.set([userId]);

				// Then create via API
				try {
					const createResponse = await fetch('/api', {
						method: 'POST',
						headers: {
							'Content-Type': 'application/json'
						},
						body: JSON.stringify({
							id: userId,
							name: newTree.name
						}),
						credentials: 'include'
					});

					if (!createResponse.ok) {
						console.error('API error when creating root node');
					}
				} catch (apiError) {
					console.error('API error when creating root node:', apiError);
					// Continue - we prioritize in-memory state
				}

				console.log('Created new tree for user:', userData.name);
			} catch (fetchError) {
				console.error('Error fetching or creating user tree:', fetchError);
				globalState.showToast('Failed to load or create user tree', 'error');
				return;
			}

			globalState.showToast(`Welcome, ${userData.name || 'User'}!`, 'success');
		} catch (error) {
			console.error('Error loading user tree:', error);
			globalState.showToast('Failed to load user tree', 'error');
		}
	},

	/**
	 * Navigation Functions
	 */

	// Set path directly
	navigateToPath: async (newPath: string[]) => {
		console.log('Navigating to path', newPath);
		if (!newPath.length || !get(userTree)) return;
		// sleep for 1 second
		currentPath.set(newPath);
	},

	// Navigate to a specific index in the path
	navigateToPathIndex: (index: number) => {
		const path = get(currentPath);
		if (index < 0 || index >= path.length) return;
		if (index === path.length - 1) return; // Already at position
		currentPath.set(path.slice(0, index + 1));
	},

	// Add a node ID to the path (zoom in)
	zoomInto: (nodeId: string) => {
		const path = get(currentPath);
		const tree = get(userTree);

		if (!tree) return;

		// Make sure the node exists
		const node = findNodeById(tree, nodeId);
		if (!node) return;

		// Add the node to the path
		currentPath.set([...path, nodeId]);
	},

	// Remove the last node ID from the path (zoom out)
	zoomOut: () => {
		const path = get(currentPath);
		if (path.length <= 1) return;
		currentPath.set(path.slice(0, -1));
	},

	// Reset all state (logout)
	resetState: () => {
		userTree.set(null);
		currentPath.set([]);
		currentUser.set(null);
	},

	/**
	 * Tree Modification Functions
	 */

	// Add a new child node to the current node
	addNode: async (nodeId: string, points: number, name: string = 'New Node'): Promise<boolean> => {
		const tree = get(userTree);
		const nodeId2 = get(currentNodeId);

		if (!tree || !nodeId2) return false;

		// Find the current node in our tree
		const currentNode = findNodeById(tree, nodeId2);
		if (!currentNode) return false;

		try {
			// Update in-memory tree with protocol function
			addChild(currentNode, nodeId, name, points);

			// Update the store to trigger reactivity
			userTree.set(tree);

			// Then update the API
			try {
				await fetch(`/api`, {
					method: 'POST',
					headers: {
						'Content-Type': 'application/json'
					},
					body: JSON.stringify({
						id: nodeId,
						name: name,
						points: points,
						parentId: nodeId2
					}),
					credentials: 'include'
				});
			} catch (apiError) {
				console.error('API error when adding node:', apiError);
				// Continue - we prioritize in-memory state
			}

			return true;
		} catch (err) {
			console.error('Error adding node:', err);
			return false;
		}
	},

	// Handle UI for adding a new node
	handleAddNode: async () => {
		if (!globalState.currentNodeId) return;

		// Create a unique ID for the new node
		const newNodeId = `node_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`;
		const newNodeName = 'New Node';
		const newPoints = 10;

		try {
			const success = await globalState.addNode(newNodeId, newPoints, newNodeName);

			if (success) {
				globalState.showToast('New node created', 'success');
				setTimeout(() => globalState.setNodeToEditMode(newNodeId), 50);
			} else {
				globalState.showToast('Error creating node', 'error');
			}
		} catch (err) {
			console.error('Error in handleAddNode:', err);
			globalState.showToast('Error creating node', 'error');
		}
	},

	// Update a node's properties (combines all update operations)
	updateNode: async (
		nodeId: string,
		updates: {
			name?: string;
			points?: number;
			contributors?: string[];
			manual?: number | null;
		}
	): Promise<boolean> => {
		const tree = get(userTree);
		if (!tree) return false;

		try {
			// Update in-memory tree using protocol function
			updateNodeById(tree, nodeId, (node) => {
				// Update name if provided
				if (updates.name !== undefined) {
					updateName(node, updates.name);
				}

				// Update points if provided and node is NonRootNode
				if (updates.points !== undefined && node.type === 'NonRootNode') {
					updatePoints(node as NonRootNode, updates.points);
				}

				// Update manual fulfillment if provided
				if (updates.manual !== undefined) {
					// Convert null to undefined for the API
					const manualValue = updates.manual === null ? undefined : updates.manual;
					updateManualFulfillment(node, manualValue);
				}

				// Handle contributors if provided and node is NonRootNode
				if (updates.contributors?.length && node.type === 'NonRootNode') {
					// Find contributor nodes in the tree
					const contributorNodes: Node[] = [];

					// Convert string IDs to Node references
					for (const id of updates.contributors) {
						const contributorNode = findNodeById(tree, id);
						if (contributorNode) {
							contributorNodes.push(contributorNode);
						}
					}

					if (contributorNodes.length > 0) {
						addContributors(node, contributorNodes);
					}
				}
			});

			// Update the store to trigger reactivity
			userTree.set(tree);

			// Then update the API
			try {
				await fetch(`/api/${nodeId}`, {
					method: 'PUT',
					headers: {
						'Content-Type': 'application/json'
					},
					body: JSON.stringify({
						...(updates.name !== undefined ? { name: updates.name } : {}),
						...(updates.points !== undefined ? { points: updates.points } : {}),
						...(updates.contributors !== undefined ? { contributors: updates.contributors } : {}),
						...(updates.manual !== undefined ? { manualFulfillment: updates.manual } : {})
					}),
					credentials: 'include'
				});
			} catch (apiError) {
				console.error('API error when updating node:', apiError);
				// Continue - we prioritize in-memory state
			}

			return true;
		} catch (err) {
			console.error('Error updating node:', err);
			return false;
		}
	},

	// Delete a node and its children
	deleteNode: async (nodeId: string): Promise<boolean> => {
		const tree = get(userTree);
		const path = get(currentPath);

		if (!tree || path.length === 0) return false;

		try {
			// Don't allow deleting root node
			if (nodeId === path[0]) {
				console.error('Cannot delete root node');
				globalState.showToast('Cannot delete root node', 'error');
				return false;
			}

			// Find parent node
			const parentNode = getParentNode(tree, nodeId);
			if (!parentNode) {
				console.error('Cannot find parent node');
				return false;
			}

			// Check if we're trying to delete the current node
			const isCurrentNode = globalState.currentNodeId === nodeId;
			if (isCurrentNode) {
				// If we're on the node to delete, navigate up to parent first
				if (path.length <= 1) {
					console.error('Cannot delete current node with no parent');
					return false;
				}

				// Navigate to parent
				globalState.zoomOut();
			}

			// First update local tree
			const nodeToDelete = findNodeById(tree, nodeId);
			if (nodeToDelete) {
				// Delete all children from the node using protocol function
				deleteSubtree(nodeToDelete);

				// Remove it from its parent's children array
				const childIndex = parentNode.children.findIndex((child) => child.id === nodeId);
				if (childIndex !== -1) {
					parentNode.children.splice(childIndex, 1);
				}

				// Update the store to trigger reactivity
				userTree.set(tree);

				// Then update the API
				try {
					await fetch(`/api/${nodeId}`, {
						method: 'DELETE',
						credentials: 'include'
					});
				} catch (apiError) {
					console.error('API error when deleting node:', apiError);
					// Continue - we prioritize in-memory state
				}
			}

			return true;
		} catch (err) {
			console.error('Error in deleteNode:', err);
			return false;
		}
	},

	/**
	 * UI State Management
	 */

	// Toggle delete mode
	toggleDeleteMode: () => {
		globalState.deleteMode = !globalState.deleteMode;
		globalState.showToast(
			globalState.deleteMode
				? 'Delete mode activated. Click a node to delete it.'
				: 'Delete mode deactivated.',
			globalState.deleteMode ? 'warning' : 'info'
		);
	},

	// Set a node to edit mode (temporary state)
	setNodeToEditMode: (nodeId: string) => {
		globalState.nodeToEdit = nodeId;
		if (browser) {
			setTimeout(() => {
				if (globalState.nodeToEdit === nodeId) {
					globalState.nodeToEdit = '';
				}
			}, 1000);
		}
	},

	// Display a toast notification
	showToast: (message: string, type: ToastType = 'info') => {
		if (globalState.toast.timeoutId) {
			clearTimeout(globalState.toast.timeoutId);
		}

		globalState.toast = {
			visible: true,
			message,
			type,
			timeoutId: browser
				? (window.setTimeout(() => {
						globalState.toast.visible = false;
					}, 3000) as unknown as number)
				: null
		};
	}
});

// Initialize the system when imported, but only if not already initialized by a route
if (browser && !globalState.initializationStarted) {
	globalState.initialize();
}
