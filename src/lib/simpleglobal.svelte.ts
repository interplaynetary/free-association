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

type ToastType = 'info' | 'success' | 'warning' | 'error';

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

// We will do some simple logic for getting and putting the userTree to the server

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
	navigateToPath: async (newPath: string[]) => {
		console.log('Navigating to path', newPath);
		if (!newPath.length || !get(userTree)) return;
		currentPath.set(newPath);
	},
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
