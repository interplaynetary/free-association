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

	// Generate path info from currentZipper and currentPath
	updatePathInfo: () => {
		if (!globalState.currentZipper) {
			globalState.pathInfo = [];
			return;
		}

		// First, ensure we have the complete path
		if (globalState.currentZipper) {
			const fullPath = globalState.fullPathFromZipper(globalState.currentZipper);

			// Only update if the path is different or empty
			if (
				fullPath.length > 0 &&
				(globalState.currentPath.length === 0 ||
					JSON.stringify(fullPath) !== JSON.stringify(globalState.currentPath))
			) {
				console.log('Updating path in updatePathInfo:', globalState.currentPath, '->', fullPath);
				globalState.currentPath = fullPath;
			}
		}

		const pathInfo: PathItem[] = [];

		// Now build the path info
		if (globalState.currentPath.length > 0) {
			// Get the root zipper
			const rootId = globalState.currentPath[0];
			const rootZipper = globalState.currentForest.get(rootId);

			if (!rootZipper) {
				globalState.pathInfo = [];
				return;
			}

			// Start with the root node
			let currentZipper = rootZipper;
			pathInfo.push({
				id: rootId,
				name: rootZipper.zipperCurrent.nodeName || rootId
			});

			// Follow the path and collect names
			for (let i = 1; i < globalState.currentPath.length; i++) {
				const childId = globalState.currentPath[i];
				const childZipper = enterChild(childId, currentZipper);

				if (!childZipper) break;

				pathInfo.push({
					id: childId,
					name: childZipper.zipperCurrent.nodeName || childId
				});

				currentZipper = childZipper;
			}
		}

		globalState.pathInfo = pathInfo;
	},

	// Navigate to a specific zipper using path IDs
	navigateToPath: (path: string[]) => {
		if (!path.length || !globalState.currentForest) return;

		const rootId = path[0];
		const rootZipper = globalState.currentForest.get(rootId);

		if (!rootZipper) {
			console.error('Root node not found:', rootId);
			return;
		}

		if (path.length === 1) {
			// Just navigating to the root
			globalState.currentZipper = { ...rootZipper };
			globalState.currentPath = [rootId];
			globalState.updatePathInfo();
			return;
		}

		// Navigate to the path
		let currentZipper = { ...rootZipper };
		let valid = true;

		// Follow the path step by step
		for (let i = 1; i < path.length; i++) {
			const childId = path[i];
			const childZipper = enterChild(childId, currentZipper);

			if (!childZipper) {
				console.error(`Child node not found: ${childId} at path index ${i}`);
				valid = false;
				break;
			}

			currentZipper = { ...childZipper };
		}

		if (valid) {
			globalState.currentZipper = currentZipper;

			// Re-compute the full path (this ensures we have the complete path)
			const fullPath = globalState.fullPathFromZipper(currentZipper);
			if (fullPath.length > 0) {
				console.log('Using reconstructed path in navigateToPath:', fullPath);
				globalState.currentPath = fullPath;
			} else {
				// Fallback to the provided path if reconstruction fails
				console.log('Using provided path in navigateToPath:', path);
				globalState.currentPath = [...path];
			}

			globalState.updatePathInfo();

			const nodeName = currentZipper.zipperCurrent.nodeName || path[path.length - 1];
		} else {
			globalState.showToast('Navigation failed', 'error');
		}
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
					userTree = await GunUserTree.fetchNode(userPub);
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

		try {
			const parentId = globalState.currentZipper.zipperCurrent.nodeId;

			// Add child using GunUserTree
			const childZipper = await GunUserTree.addChild(
				parentId,
				name || 'New Node',
				points,
				nodeId // Use provided nodeId
			);

			if (!childZipper) {
				console.error('Failed to add child node');
				return false;
			}

			// Save the node
			await GunUserTree.saveNode(childZipper);

			// Reload the current tree to reflect changes
			const updatedRootTree = await GunUserTree.loadFullTree();
			if (!updatedRootTree) return false;

			// Update forest and current zipper
			const rootId = updatedRootTree.zipperCurrent.nodeId;
			globalState.currentForest.set(rootId, updatedRootTree);

			// Navigate to the same location in the updated tree
			if (globalState.currentPath.length > 0) {
				globalState.navigateToPath(globalState.currentPath);
			} else {
				globalState.currentZipper = updatedRootTree;
				globalState.currentPath = [rootId];
				globalState.updatePathInfo();
			}

			return true;
		} catch (err) {
			console.error('Error adding node:', err);
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
			manualFulfillment?: number | null;
		}
	) => {
		if (!globalState.currentZipper) return false;

		try {
			// Find the node in the tree
			let nodeToUpdate = null;
			if (globalState.currentZipper.zipperCurrent.nodeId === nodeId) {
				nodeToUpdate = globalState.currentZipper;
			} else {
				// Try to find the node by navigating from current
				for (const [childId, _] of globalState.currentZipper.zipperCurrent.nodeChildren) {
					if (childId === nodeId) {
						nodeToUpdate = enterChild(childId, globalState.currentZipper);
						break;
					}
				}
			}

			if (!nodeToUpdate) {
				// Load the node directly
				nodeToUpdate = await GunUserTree.fetchNode(nodeId);
			}

			if (!nodeToUpdate) {
				console.error(`Node not found: ${nodeId}`);
				return false;
			}

			// Update node properties
			const updatedNode = { ...nodeToUpdate.zipperCurrent };
			if (updates.name !== undefined) updatedNode.nodeName = updates.name;
			if (updates.points !== undefined) updatedNode.nodePoints = updates.points;
			if (updates.contributors !== undefined) {
				updatedNode.nodeContributors = new Set(updates.contributors);
			}
			if (updates.manualFulfillment !== undefined) {
				updatedNode.nodeManualFulfillment = updates.manualFulfillment;
			}

			// Create updated zipper
			const updatedZipper = {
				...nodeToUpdate,
				zipperCurrent: updatedNode
			};

			// Save the updated node
			await GunUserTree.saveNode(updatedZipper);

			// Reload the full tree
			const updatedRootTree = await GunUserTree.loadFullTree();
			if (!updatedRootTree) return false;

			// Update the forest
			const rootId = updatedRootTree.zipperCurrent.nodeId;
			globalState.currentForest.set(rootId, updatedRootTree);

			// Navigate to the same location in the updated tree
			if (globalState.currentPath.length > 0) {
				globalState.navigateToPath(globalState.currentPath);
			} else {
				globalState.currentZipper = updatedRootTree;
				globalState.currentPath = [rootId];
				globalState.updatePathInfo();
			}

			return true;
		} catch (error) {
			console.error('Error updating node:', error);
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
