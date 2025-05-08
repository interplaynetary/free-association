import {
	createExampleForest,
	addToForest,
	enterChild,
	exitToParent,
	createRootNode,
	makePoints,
	followPath,
	getCurrentPath,
	addChild,
	modifyNode
} from '$lib/centralized';
import type { TreeZipper, Forest, NavigationPath, Node, Ctx } from '$lib/centralized';
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

	// Initialize the system
	initialize: () => {
		const { forest } = createExampleForest();
		globalState.currentForest = forest;

		// Set initial zipper to the first node in the forest
		const entries = Array.from(forest.entries());
		if (entries.length > 0) {
			const [rootId, rootZipper] = entries[0];
			globalState.currentZipper = rootZipper;
			globalState.currentPath = [rootId];

			// Generate path info
			globalState.updatePathInfo();
		}

		// Check for logged in user in localStorage
		if (browser) {
			const storedUser = localStorage.getItem('centralizedUser');
			if (storedUser) {
				try {
					const userData = JSON.parse(storedUser);
					globalState.setCurrentUser(userData);
				} catch (error) {
					console.error('Error parsing stored user:', error);
				}
			}
		}
	},

	// Set current user and update the current forest view
	setCurrentUser: (userData: UserData | null) => {
		globalState.currentUser = userData;

		if (!userData) {
			// If logging out, keep the current forest but reset the view to the first node
			// instead of recreating the example forest
			const entries = Array.from(globalState.currentForest.entries());
			if (entries.length > 0) {
				const [rootId, rootZipper] = entries[0];
				globalState.currentZipper = rootZipper;
				globalState.currentPath = [rootId];
				globalState.updatePathInfo();
			}
			return;
		}

		// Check if the user already exists in the current forest
		const userZipper = globalState.currentForest.get(userData.username?.toLowerCase() || '');

		if (userZipper) {
			// User exists in the current forest, navigate to their tree
			globalState.currentZipper = userZipper;
			globalState.currentPath = [userData.username?.toLowerCase() || ''];
			globalState.updatePathInfo();
		} else {
			// User doesn't exist in the current forest, create a new tree for them
			const username = userData.username?.toLowerCase() || userData.pub.toLowerCase();
			const userNode = createRootNode(
				username,
				userData.alias,
				makePoints(100),
				[], // No contributors initially
				null // No manual fulfillment
			);

			const userZipper: TreeZipper = { zipperCurrent: userNode, zipperContext: null };

			// Add new user to the existing forest instead of replacing it
			globalState.currentForest.set(username, userZipper);
			globalState.currentZipper = userZipper;
			globalState.currentPath = [username];
			globalState.updatePathInfo();
		}
	},

	// Add node to current zipper
	addNode: (nodeId: string, points: number, name?: string) => {
		if (!globalState.currentZipper) return false;

		try {
			// Add child to current zipper
			const updatedZipper = addChild(
				nodeId,
				makePoints(points),
				[], // No contributors initially
				null, // No manual fulfillment
				globalState.currentZipper
			);

			// If the node has a name, set it
			if (name) {
				const childZipper = enterChild(nodeId, updatedZipper);
				if (childZipper) {
					const namedChildZipper = modifyNode(
						(node: Node) => ({
							...node,
							nodeName: name
						}),
						childZipper
					);

					// Update parent with named child
					const updatedChildren = new Map(updatedZipper.zipperCurrent.nodeChildren);
					updatedChildren.set(nodeId, namedChildZipper.zipperCurrent);

					// Create updated parent with named child
					const updatedParentZipper = modifyNode(
						(node: Node) => ({
							...node,
							nodeChildren: updatedChildren
						}),
						updatedZipper
					);

					// Update current zipper
					globalState.currentZipper = { ...updatedParentZipper };
				} else {
					// If we couldn't enter the child, just use the updated zipper
					globalState.currentZipper = { ...updatedZipper };
				}
			} else {
				// Update current zipper
				globalState.currentZipper = { ...updatedZipper };
			}

			// Update forest with new zipper at the root level
			const rootId = globalState.currentPath[0];
			if (rootId) {
				// If the current zipper is at the root, directly update the forest
				if (globalState.currentPath.length === 1 && globalState.currentZipper) {
					globalState.currentForest = addToForest(
						globalState.currentForest,
						globalState.currentZipper
					);
				} else {
					// Follow the path from the current zipper back to root and update
					const rootZipper = globalState.currentForest.get(rootId);
					if (rootZipper && globalState.currentZipper) {
						// Follow the path to the current location
						let current = globalState.currentZipper;

						// We need to update the forest with the changes bubbling up
						globalState.currentForest = addToForest(globalState.currentForest, current);
					}
				}
			}

			// Update path info
			globalState.updatePathInfo();
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
		const newNodeId = `node-${Date.now()}`;
		const newNodeName = 'New Node';

		// New nodes start with 10 points by default
		const success = globalState.addNode(newNodeId, 10, newNodeName);

		if (success) {
			globalState.showToast('New node created', 'success');

			// Set node to edit mode
			setTimeout(() => {
				globalState.setNodeToEditMode(newNodeId);
			}, 50);
		} else {
			globalState.showToast('Error creating node', 'error');
		}
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
	globalState.initialize();
}
