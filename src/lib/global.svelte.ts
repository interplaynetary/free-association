import {
	Tree,
	enterChild,
	exitToParent,
	goToRoot,
	getCurrentPath,
	followPath
} from '$lib/centralized/tree';
import type { TreeZipper, Forest, Node } from '$lib/centralized/types';
import { browser } from '$app/environment';
import { user, username, userpub, type UserData } from './gun/user';
import { get } from 'svelte/store';

// Future TODO:
// What is currentForest? What role does it play here? It seems ok.
// Add manualFulfillfment to the Child.svelte / Parent.svelte

type ToastType = 'info' | 'success' | 'warning' | 'error';

export interface PathItem {
	id: string;
	name: string;
}

// Add a debounce mechanism for path updates
let pathUpdateTimeout: number | null = null;
const PATH_UPDATE_DEBOUNCE_MS = 50;

export const globalState = $state({
	// Core navigation state (using our centralized system)
	currentForest: new Map() as Forest,
	currentZipper: null as TreeZipper | null,
	currentPath: [] as string[],
	get currentUser(): UserData | null {
		const pub = get(userpub);
		const alias = get(username);

		if (!pub) return null;

		return {
			pub,
			alias,
			username: alias
		};
	},

	// Enhanced path information with names
	pathInfo: [] as PathItem[],

	// UI state
	deleteMode: false,
	nodeToEdit: '',
	toast: {
		visible: false,
		message: '',
		type: 'info' as ToastType,
		timeoutId: null as number | null
	},

	// Update path information based on current path
	updatePathInfo: () => {
		// Debounce path updates to avoid multiple rapid-fire updates
		if (pathUpdateTimeout !== null) {
			clearTimeout(pathUpdateTimeout);
		}

		pathUpdateTimeout = window.setTimeout(() => {
			console.log('[FLOW] Updating path info for path:', globalState.currentPath);

			if (globalState.currentPath.length === 0) {
				globalState.pathInfo = [];
				return;
			}

			// Reset path info and rebuild
			const newPathInfo: PathItem[] = [];

			// Get the root tree
			const rootId = globalState.currentPath[0];
			const rootTree = globalState.currentForest.get(rootId);

			if (!rootTree) {
				console.error('[FLOW] Root tree not found in forest:', rootId);
				globalState.pathInfo = newPathInfo;
				return;
			}

			// Add root to path info
			newPathInfo.push({
				id: rootId,
				name: rootTree.zipperCurrent.nodeName
			});

			// Process each path segment
			if (globalState.currentPath.length > 1) {
				let currentTree = rootTree;

				// Skip the first item (root) and process each path segment
				for (let i = 1; i < globalState.currentPath.length; i++) {
					const nodeId = globalState.currentPath[i];
					const childTree = enterChild(nodeId, currentTree);

					if (!childTree) {
						console.error('[FLOW] Child not found in path:', nodeId);
						break;
					}

					// Add to path info
					newPathInfo.push({
						id: nodeId,
						name: childTree.zipperCurrent.nodeName
					});

					// Move to next level
					currentTree = childTree;
				}
			}

			// Update the global path info
			globalState.pathInfo = newPathInfo;
			console.log('[FLOW] Path info updated:', newPathInfo);

			// Clear the timeout reference
			pathUpdateTimeout = null;
		}, PATH_UPDATE_DEBOUNCE_MS);
	},

	// Navigate to a specific path (array of node IDs)
	navigateToPath: (newPath: string[]) => {
		console.log('[FLOW] navigateToPath called with:', newPath);

		if (!newPath.length) {
			console.error('[FLOW] Empty path provided to navigateToPath');
			return;
		}

		const rootId = newPath[0];
		console.log('[FLOW] Root ID from path:', rootId);

		// Get the root TreeZipper
		const rootZipper = globalState.currentForest.get(rootId);
		if (!rootZipper) {
			console.error('[FLOW] Root zipper not found in forest for ID:', rootId);
			return;
		}

		// Follow the path directly using the centralized followPath function
		const targetZipper = followPath(newPath.slice(1), rootZipper);
		if (!targetZipper) {
			console.error('[FLOW] Failed to follow path:', newPath);
			return;
		}

		// Update the global state
		globalState.currentZipper = targetZipper;
		globalState.currentPath = newPath;
		globalState.updatePathInfo();
	},

	// Navigate to a specific index in the current path
	navigateToPathIndex: (index: number) => {
		if (index < 0 || index >= globalState.currentPath.length) return;

		// If navigating to current position, no-op
		if (index === globalState.currentPath.length - 1) return;

		// Truncate the path to the desired index and navigate to it
		const newPath = globalState.currentPath.slice(0, index + 1);
		globalState.navigateToPath(newPath);
	},

	// Zoom into a specific child node
	zoomInto: (nodeId: string) => {
		if (!globalState.currentZipper) return;

		const childZipper = enterChild(nodeId, globalState.currentZipper);
		if (!childZipper) {
			console.error(`Child node not found: ${nodeId}`);
			return;
		}

		// Update current zipper
		globalState.currentZipper = childZipper;

		// Update path by adding the new node ID
		globalState.currentPath = [...globalState.currentPath, nodeId];
		globalState.updatePathInfo();
	},

	// Zoom out to parent node
	zoomOut: () => {
		if (!globalState.currentZipper || globalState.currentPath.length <= 1) return;

		// Use the exitToParent function to navigate up
		const parentZipper = exitToParent(globalState.currentZipper);
		if (!parentZipper) {
			console.error('Failed to exit to parent');
			return;
		}

		// Update current zipper
		globalState.currentZipper = parentZipper;

		// Remove the last element from the path
		globalState.currentPath = globalState.currentPath.slice(0, -1);
		globalState.updatePathInfo();
	},

	// Initialize the system with Tree
	initialize: async () => {
		try {
			// Create the forest
			const forest = new Map<string, TreeZipper>();
			globalState.currentForest = forest;

			// Setup subscription to user auth state directly from userpub
			userpub.subscribe((pub) => {
				if (pub) {
					// When user is authenticated, load their tree
					const userData = globalState.currentUser;
					if (userData) {
						globalState.loadUserTree(userData);
					}
				} else if (globalState.currentZipper) {
					// When user logs out, reset the state
					globalState.resetState();
				}
			});
		} catch (error) {
			console.error('Error initializing global state:', error);
			globalState.showToast('Failed to initialize application', 'error');
		}
	},

	// Load user tree
	loadUserTree: async (userData: UserData) => {
		if (!userData) return;

		try {
			const userPub = userData.pub;

			// First try to load the full tree
			let fullTree: TreeZipper | null = null;
			try {
				fullTree = await Tree.loadFullTree();
				console.log('Full tree loaded successfully');
			} catch (loadError) {
				console.warn('Failed to load full tree, will try alternatives:', loadError);
			}

			if (fullTree) {
				// Successfully loaded the full tree
				const forest: Forest = new Map<string, TreeZipper>();
				forest.set(userPub, fullTree);

				// Set as current tree
				globalState.currentForest = forest;
				globalState.currentZipper = fullTree;
				globalState.currentPath = [userPub];
			} else {
				// Try to fetch just the user's node
				let userTree: TreeZipper | null = null;

				try {
					userTree = await Tree.fetchNode(userPub);
					console.log('User tree fetched successfully');
				} catch (fetchError) {
					console.warn('Failed to fetch user tree:', fetchError);
				}

				if (userTree) {
					// We have the user node, use it
					const forest: Forest = new Map<string, TreeZipper>();
					forest.set(userPub, userTree);

					// Set as current tree
					globalState.currentForest = forest;
					globalState.currentZipper = userTree;
					globalState.currentPath = [userPub];
				} else {
					// No tree exists - create a new root node for this user
					console.log('No tree found for user, creating new tree:', userData.alias);

					try {
						// Create a new root node for this user
						const rootName = userData.alias || 'My Tree';

						// Create the root node with name
						const newTree = await Tree.createRootNode(rootName);

						if (!newTree) {
							throw new Error('Failed to create root node');
						}

						// Save the new node to ensure it persists
						await Tree.saveNode(newTree);

						// Add to forest and set as current
						const forest: Forest = new Map<string, TreeZipper>();
						forest.set(userPub, newTree);
						globalState.currentForest = forest;
						globalState.currentZipper = newTree;
						globalState.currentPath = [userPub];

						console.log('Created new tree for user:', userData.alias);
					} catch (createError) {
						console.error('Failed to create new tree for user:', createError);
						globalState.showToast('Failed to create new tree', 'error');
						return;
					}
				}
			}

			// Update path info
			globalState.updatePathInfo();
			globalState.showToast(`Welcome, ${userData.alias}!`, 'success');
		} catch (error) {
			console.error('Error loading user tree:', error);
			globalState.showToast('Failed to load user tree', 'error');
		}
	},

	// Reset state when user logs out
	resetState: () => {
		globalState.currentZipper = null;
		globalState.currentPath = [];
		globalState.currentForest = new Map();
		globalState.updatePathInfo();
	},

	// Add node to current zipper
	addNode: async (nodeId: string, points: number, name?: string) => {
		if (!globalState.currentZipper || !globalState.currentUser) return false;

		console.log('[FLOW] addNode started:', { nodeId, points, name });

		try {
			const parentId = globalState.currentZipper.zipperCurrent.nodeId;
			console.log('[FLOW] Adding child to parent:', parentId);

			// Add child using Tree
			const childZipper = await Tree.addChild(
				parentId,
				name || 'New Node',
				points,
				nodeId // Use provided nodeId
			);
			console.log('[FLOW] childZipper created:', childZipper?.zipperCurrent.nodeId);

			if (!childZipper) {
				console.error('[FLOW] Failed to add child node - childZipper is null');
				return false;
			}

			// Save the node
			console.log('[FLOW] Saving node to Gun:', nodeId);
			await Tree.saveNode(childZipper);
			console.log('[FLOW] Node saved to Gun');

			// Update the parent node's children directly
			const currentNode = globalState.currentZipper.zipperCurrent;
			currentNode.nodeChildren.set(nodeId, childZipper.zipperCurrent);

			// Update the parent zipper with the modified node
			globalState.currentZipper = {
				...globalState.currentZipper,
				zipperCurrent: currentNode
			};

			// Update the forest if needed
			const rootId = globalState.currentPath[0];
			if (rootId === parentId) {
				globalState.currentForest.set(rootId, globalState.currentZipper);
			}

			console.log('[FLOW] Local state updated with new node');
			return true;
		} catch (err) {
			console.error('[FLOW] Error adding node:', err);
			return false;
		}
	},

	// Handle adding a new node (implementation in Parent.svelte, but will be moved here)
	handleAddNode: () => {
		if (!globalState.currentZipper) return;

		// Create a unique ID for the new node
		const newNodeId = `node_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`;
		const newNodeName = 'New Node';

		// New nodes start with 10 points by default
		globalState
			.addNode(newNodeId, 10, newNodeName)
			.then((success) => {
				if (success) {
					globalState.showToast('New node created', 'success');

					// Set node to edit mode
					setTimeout(() => {
						globalState.setNodeToEditMode(newNodeId);
					}, 50);
				} else {
					globalState.showToast('Error creating node', 'error');
				}
			})
			.catch((err) => {
				console.error('Error in handleAddNode:', err);
				globalState.showToast('Error creating node', 'error');
			});
	},

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

	// Set a node to edit mode
	setNodeToEditMode: (nodeId: string) => {
		globalState.nodeToEdit = nodeId;

		// Auto-clear after a delay
		if (browser) {
			setTimeout(() => {
				if (globalState.nodeToEdit === nodeId) {
					globalState.nodeToEdit = '';
				}
			}, 1000);
		}
	},

	// Update node properties
	updateNode: async (
		nodeId: string,
		updates: {
			name?: string;
			points?: number;
			contributors?: string[];
			manual?: number | null;
		}
	): Promise<boolean> => {
		console.log('[FLOW] updateNode started:', { nodeId, updates });
		if (!globalState.currentZipper) return false;

		try {
			// Check if updating current node
			const isCurrentNode = globalState.currentZipper.zipperCurrent.nodeId === nodeId;
			console.log('[FLOW] Updating current node?', isCurrentNode);

			let nodeToUpdate: TreeZipper | null = null;

			if (isCurrentNode) {
				// Use current zipper directly
				nodeToUpdate = globalState.currentZipper;
			} else {
				// Find the child zipper
				const parentZipper = globalState.currentZipper;
				nodeToUpdate = enterChild(nodeId, parentZipper);
			}

			if (!nodeToUpdate) {
				console.error('[FLOW] Node not found for update:', nodeId);
				return false;
			}

			// Apply updates to the node
			console.log('[FLOW] Applying updates to node');
			const updatedNode: Node = { ...nodeToUpdate.zipperCurrent };

			if (updates.name !== undefined) {
				updatedNode.nodeName = updates.name;
			}

			if (updates.points !== undefined && updatedNode.type === 'non-root') {
				updatedNode.nodePoints = updates.points;
			}

			if (updates.contributors !== undefined && updatedNode.type === 'non-root') {
				updatedNode.nodeContributors = new Set(updates.contributors);
			}

			if (updates.manual !== undefined) {
				updatedNode.nodeManualFulfillment = updates.manual;
			}

			// Create updated zipper
			const updatedZipper: TreeZipper = {
				...nodeToUpdate,
				zipperCurrent: updatedNode
			};

			// Save the updated node
			console.log('[FLOW] Saving updated node to Gun');
			await Tree.saveNode(updatedZipper);
			console.log('[FLOW] Node updated in Gun');

			if (isCurrentNode) {
				// Update current zipper
				globalState.currentZipper = updatedZipper;
			} else {
				// Update child in parent's children map
				const currentNode = globalState.currentZipper.zipperCurrent;
				currentNode.nodeChildren.set(nodeId, updatedNode);

				// Update parent zipper
				globalState.currentZipper = {
					...globalState.currentZipper,
					zipperCurrent: currentNode
				};
			}

			// Update the forest if needed
			const rootId = globalState.currentPath[0];
			if (rootId === nodeId) {
				globalState.currentForest.set(rootId, updatedZipper);
			}

			// Update path info if name changed
			if (updates.name !== undefined) {
				globalState.updatePathInfo();
			}

			console.log('[FLOW] Node update completed successfully');
			return true;
		} catch (err) {
			console.error('[FLOW] Error updating node:', err);
			return false;
		}
	},

	// Display a toast notification
	showToast: (message: string, type: ToastType = 'info') => {
		// Clear any existing toast timeout
		if (globalState.toast.timeoutId) {
			clearTimeout(globalState.toast.timeoutId);
		}

		// Show new toast
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
	},

	// Delete a node and its children
	deleteNode: async (nodeId: string): Promise<boolean> => {
		console.log('[FLOW] deleteNode started:', nodeId);
		if (!globalState.currentZipper) return false;

		try {
			// Don't allow deleting root node
			if (nodeId === globalState.currentPath[0]) {
				console.error('[FLOW] Cannot delete root node');
				globalState.showToast('Cannot delete root node', 'error');
				return false;
			}

			// Check if we're trying to delete the current node
			const isCurrentNode = globalState.currentZipper.zipperCurrent.nodeId === nodeId;

			if (isCurrentNode) {
				// If we're on the node to delete, navigate up to parent first
				if (globalState.currentPath.length <= 1) {
					console.error('[FLOW] Cannot delete current node with no parent');
					return false;
				}

				// Navigate to parent
				console.log('[FLOW] Navigating to parent before deleting current node');
				globalState.zoomOut();
			}

			// Get the parent node
			const parentNodeId = isCurrentNode
				? globalState.currentPath[globalState.currentPath.length - 1]
				: globalState.currentZipper.zipperCurrent.nodeId;

			const parentZipper = globalState.currentZipper;

			// Verify the node exists in parent's children
			if (!parentZipper.zipperCurrent.nodeChildren.has(nodeId)) {
				console.error(`[FLOW] Node ${nodeId} not found in parent's children`);
				return false;
			}

			// Remove the child from parent's children
			const updatedChildren = new Map(parentZipper.zipperCurrent.nodeChildren);
			updatedChildren.delete(nodeId);

			const updatedParentNode = {
				...parentZipper.zipperCurrent,
				nodeChildren: updatedChildren
			};

			const updatedParentZipper = {
				...parentZipper,
				zipperCurrent: updatedParentNode
			};

			// Save the updated parent to Gun
			console.log(`[FLOW] Updating parent node ${parentNodeId} to remove child reference`);
			await Tree.saveNode(updatedParentZipper);

			// Delete the node from Gun
			console.log(`[FLOW] Deleting node ${nodeId} from Gun`);
			const deleteResult = await Tree.deleteNode(nodeId);

			if (!deleteResult) {
				console.error(`[FLOW] Failed to delete node ${nodeId} from Gun`);
				return false;
			}

			// Update the current zipper to the updated parent
			globalState.currentZipper = updatedParentZipper;

			// Update the forest if parent is root
			const rootId = globalState.currentPath[0];
			if (rootId === parentNodeId) {
				globalState.currentForest.set(rootId, updatedParentZipper);
			}

			// Force an update of path info
			globalState.updatePathInfo();

			console.log(`[FLOW] Node ${nodeId} deleted successfully`);
			return true;
		} catch (err) {
			console.error('[FLOW] Error in deleteNode:', err);
			return false;
		}
	}
});

// Listen for auth state changes to handle sign out
if (browser) {
	// Handle signout specifically by subscribing to userpub
	userpub.subscribe((pub) => {
		if (!pub && globalState.currentZipper) {
			// User logged out
			globalState.resetState();
		}
	});

	// Initialize the system when imported
	globalState.initialize();
}
