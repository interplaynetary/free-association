import {
	GunUserTree,
	enterChild,
	exitToParent,
	updateNodePersistentCache
} from '$lib/centralized/tree';
import type { TreeZipper, Forest, NavigationPath, Node, Ctx } from '$lib/centralized/types';
import { browser } from '$app/environment';

type ToastType = 'info' | 'success' | 'warning' | 'error';

export interface UserData {
	pub: string;
	alias: string;
	username?: string;
}

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
	currentUser: null as UserData | null,

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

	// Calculate the full path from the zipper, reconstructing the complete route from root to current node
	fullPathFromZipper: (zipper: TreeZipper | null): string[] => {
		if (!zipper) return [];

		// Start with the current node ID
		const path: string[] = [];
		let currentNode = zipper;

		// First, go all the way up to find the root
		let stack: TreeZipper[] = [];

		// Push the current node
		stack.push({ ...currentNode });

		// Follow context chain all the way up to the root
		while (currentNode && currentNode.zipperContext) {
			const parent = exitToParent(currentNode);
			if (!parent) break;

			stack.push({ ...parent });
			currentNode = parent;
		}

		// Now reconstruct the path from root downward
		if (stack.length > 0) {
			// Get the root (last element in our stack)
			const root = stack[stack.length - 1];
			path.push(root.zipperCurrent.nodeId);

			// For each zipper in the stack (except the root), add its ID to the path
			for (let i = stack.length - 2; i >= 0; i--) {
				const current = stack[i];
				path.push(current.zipperCurrent.nodeId);
			}
		}

		return path;
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
			const newPathInfo: { id: string; name: string }[] = [];

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

		// Start navigation from the root
		let currentZipper = rootZipper;
		let currentPath = [rootId];
		console.log('[FLOW] Starting navigation from root node:', rootId);

		// Navigate through the path, skipping the first element (root)
		for (let i = 1; i < newPath.length; i++) {
			const nodeId = newPath[i];
			console.log(`[FLOW] Navigating to path segment ${i}:`, nodeId);

			// Try to enter child node
			const childZipper = enterChild(nodeId, currentZipper);
			if (!childZipper) {
				console.error(`[FLOW] Child not found in zipper during navigation:`, nodeId);
				break;
			}

			// Update current zipper and path
			currentZipper = childZipper;
			currentPath.push(nodeId);
			console.log(`[FLOW] Successfully navigated to:`, nodeId);
		}

		// Update the global state
		console.log('[FLOW] Navigation complete, final path:', currentPath);
		globalState.currentZipper = currentZipper;
		globalState.currentPath = currentPath;
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
		globalState.currentZipper = { ...childZipper };

		// Use fullPathFromZipper to get the complete path
		const newPath = globalState.fullPathFromZipper(globalState.currentZipper);
		if (newPath.length > 0) {
			console.log('Full reconstructed path after zoom in:', newPath);
			globalState.currentPath = newPath;
		} else {
			// Fallback to appending the nodeId to the current path
			console.log('Fallback path update after zoom in:', [...globalState.currentPath, nodeId]);
			globalState.currentPath = [...globalState.currentPath, nodeId];
		}

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
		globalState.currentZipper = { ...parentZipper };

		// Reconstruct the full path
		const newPath = globalState.fullPathFromZipper(globalState.currentZipper);
		if (newPath.length > 0) {
			console.log('Full reconstructed path after zoom out:', newPath);
			globalState.currentPath = newPath;
		} else {
			// Fallback to removing the last element from the path
			console.log('Fallback path update after zoom out:', globalState.currentPath.slice(0, -1));
			globalState.currentPath = globalState.currentPath.slice(0, -1);
		}

		globalState.updatePathInfo();
	},

	// Initialize the system with GunUserTree
	initialize: async () => {
		try {
			// Initialize Gun and authenticate user
			await GunUserTree.initialize().catch((err) => {
				console.warn('User authentication warning (not critical):', err);
				// Continue execution even if auth has issues
			});

			// Load full tree, with a fallback to creating a basic tree
			let rootTree;
			try {
				rootTree = await GunUserTree.loadFullTree();
			} catch (treeError) {
				console.error('Error loading full tree:', treeError);
				// We'll create one when user logs in
			}

			// Create the forest
			const forest = new Map<string, TreeZipper>();

			if (rootTree) {
				const rootId = rootTree.zipperCurrent.nodeId;
				forest.set(rootId, rootTree);

				// Set initial state
				globalState.currentForest = forest;
				globalState.currentZipper = rootTree;
				globalState.currentPath = [rootId];

				// Load cache data
				try {
					const treeWithCache = await GunUserTree.loadCacheData(rootTree);
					forest.set(rootId, treeWithCache);
					globalState.currentZipper = treeWithCache;
				} catch (cacheError) {
					console.warn('Error loading cache data (non-critical):', cacheError);
					// Continue without cache data
				}

				// Generate path info
				globalState.updatePathInfo();
			} else {
				// No tree exists yet, we'll create one when a user logs in
				globalState.currentForest = forest;
			}

			// Check for logged in user in localStorage
			if (browser) {
				const storedUser = localStorage.getItem('centralizedUser');
				if (storedUser) {
					try {
						const userData = JSON.parse(storedUser);
						await globalState.setCurrentUser(userData).catch((err) => {
							console.error('Error setting current user from localStorage:', err);
						});
					} catch (parseError) {
						console.error('Error parsing stored user:', parseError);
						// Remove invalid stored user data
						localStorage.removeItem('centralizedUser');
					}
				}
			}
		} catch (error) {
			console.error('Error initializing GunUserTree:', error);
			globalState.showToast('Failed to initialize tree', 'error');
		}
	},

	// Set current user and update the current forest view
	setCurrentUser: async (userData: UserData | null) => {
		globalState.currentUser = userData;

		if (!userData) {
			// If logging out, just clear the current state
			globalState.currentZipper = null;
			globalState.currentPath = [];
			globalState.currentForest = new Map();
			globalState.updatePathInfo();
			return;
		}

		try {
			// Try to fetch the user's tree
			const userPub = userData.pub;

			// First check if we already have this tree in memory
			let userTree = globalState.currentForest.get(userPub);

			// If not in memory, fetch from database
			if (!userTree) {
				try {
					const fetchedTree = await GunUserTree.fetchNode(userPub);
					userTree = fetchedTree || undefined;
				} catch (fetchError) {
					console.warn('Error fetching user tree, will try to create new:', fetchError);
				}
			}

			// Create a new tree for the user if needed
			if (!userTree) {
				console.log('Creating new tree for user:', userData.alias);
				try {
					userTree = await GunUserTree.createRootNode(
						userData.alias || 'My Tree',
						100,
						[userPub] // The user is the contributor to the root node
					);

					// Save the new tree
					await GunUserTree.saveNode(userTree);
				} catch (createError) {
					console.error('Error creating root node:', createError);
					globalState.showToast('Failed to create user tree', 'error');
					return;
				}
			}

			// Try to load the full tree with children, with fallback to just the root
			let fullTree: TreeZipper | null = null;
			try {
				fullTree = await GunUserTree.loadFullTree();
			} catch (loadError) {
				console.warn('Error loading full tree, will use basic tree:', loadError);
			}

			// Create new forest
			const forest: Forest = new Map<string, TreeZipper>();

			if (fullTree) {
				// Update the forest with the full tree
				forest.set(userPub, fullTree);

				// Set as current tree
				globalState.currentForest = forest;
				globalState.currentZipper = fullTree;
				globalState.currentPath = [userPub];
			} else if (userTree) {
				// Fallback to just the user node
				// We've already checked userTree is not null, but TypeScript needs an assertion
				forest.set(userPub, userTree!);

				// Set as current tree
				globalState.currentForest = forest;
				globalState.currentZipper = userTree;
				globalState.currentPath = [userPub];
			} else {
				// This should never happen, but just in case
				console.error('No valid tree found for user:', userData);
				globalState.showToast('Failed to load or create user tree', 'error');
				return;
			}

			// Update path info
			globalState.updatePathInfo();
			globalState.showToast(`Welcome, ${userData.alias}!`, 'success');
		} catch (error) {
			console.error('Error setting current user:', error);
			globalState.showToast('Failed to load user tree', 'error');
		}
	},

	// Add node to current zipper
	addNode: async (nodeId: string, points: number, name?: string) => {
		if (!globalState.currentZipper || !globalState.currentUser) return false;

		console.log('[FLOW] addNode started:', { nodeId, points, name });

		try {
			const parentId = globalState.currentZipper.zipperCurrent.nodeId;
			console.log('[FLOW] Adding child to parent:', parentId);

			// Add child using GunUserTree
			const childZipper = await GunUserTree.addChild(
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
			await GunUserTree.saveNode(childZipper);
			console.log('[FLOW] Node saved to Gun');

			// Instead of reloading the entire tree, we'll update our current forest and zipper directly
			// This is more efficient than a full tree reload
			console.log('[FLOW] Updating current zipper and forest with new node');

			// 1. Add the child to the current node's children map
			const currentNode = globalState.currentZipper.zipperCurrent;
			currentNode.nodeChildren.set(nodeId, childZipper.zipperCurrent);

			// 2. Update the current zipper with the modified node
			const updatedCurrentZipper = {
				...globalState.currentZipper,
				zipperCurrent: currentNode
			};

			// 3. Update the forest with the modified tree
			const rootId = globalState.currentPath[0];

			// Find the root tree in the forest
			let rootTree = globalState.currentForest.get(rootId);
			if (rootTree) {
				// If we're at the root, update it directly
				if (rootId === parentId) {
					rootTree = updatedCurrentZipper;
				} else {
					// Otherwise, we need to navigate to the root and apply changes
					// This is more complex, so in this case we'll just reload the tree for consistency
					console.log('[FLOW] Node added deeper in the tree, updating root tree');
					const updatedRootTree = await GunUserTree.loadFullTree();

					if (!updatedRootTree) {
						console.error('[FLOW] Failed to reload tree after adding node');
						return false;
					}

					rootTree = updatedRootTree;
				}

				// Update the forest with the new root tree
				globalState.currentForest.set(rootId, rootTree);
			}

			// 4. Update the current zipper
			globalState.currentZipper = updatedCurrentZipper;

			// 5. No need to update the path as we're still at the same location

			console.log('[FLOW] Local state updated with new node, path:', globalState.currentPath);
			console.log('[FLOW] addNode completed successfully');
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

			if (updates.points !== undefined) {
				updatedNode.nodePoints = updates.points;
			}

			if (updates.contributors !== undefined) {
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
			await GunUserTree.saveNode(updatedZipper);
			console.log('[FLOW] Node updated in Gun');

			// Instead of reloading the entire tree, update our in-memory structures directly
			console.log('[FLOW] Updating in-memory structures');

			if (isCurrentNode) {
				// We're updating the current node
				globalState.currentZipper = updatedZipper;
			} else {
				// We're updating a child
				const currentNode = globalState.currentZipper.zipperCurrent;

				// Update the child in the current node's children map
				currentNode.nodeChildren.set(nodeId, updatedNode);

				// Update the current zipper with the modified children
				globalState.currentZipper = {
					...globalState.currentZipper,
					zipperCurrent: currentNode
				};
			}

			// Update path info if we changed a name
			if (updates.name !== undefined) {
				globalState.updatePathInfo();
			}

			// Update the forest with the changes
			const rootId = globalState.currentPath[0];
			if (rootId === nodeId) {
				// If we're updating the root, update it in the forest
				globalState.currentForest.set(rootId, updatedZipper);
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
	}
});

// Initialize the system when imported
if (browser) {
	// Don't await here - initialize asynchronously
	globalState.initialize();
}
