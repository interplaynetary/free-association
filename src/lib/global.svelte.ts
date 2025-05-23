import { browser } from '$app/environment';
import { get, writable, derived, type Writable, type Readable } from 'svelte/store';
import {
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
	updateNodeById as updateNode,
	isContribution
} from '$lib/protocol';
import { type Node, type RootNode, type NonRootNode } from '$lib/schema';
import { username, userpub, userTree, persist, manifest } from '$lib/state.svelte';

// GunDB user data types from gunSetup
// User identification is handled via username (alias) and userpub (public key)

type ToastType = 'info' | 'success' | 'warning' | 'error';

/**
 * Core Reactive Stores
 *
 * These stores form the foundation of our state management:
 * - username & userpub: The authenticated user from Gun (imported from gunSetup)
 * - userTree: The complete tree structure for the current user (imported from state)
 * - currentPath: The navigation path (array of node IDs) in the tree
 */
export const currentPath: Writable<string[]> = writable([]);

// Initialize currentPath when user logs in
if (browser) {
	// Watch for user authentication state changes
	let lastPub = '';
	userpub.subscribe((pub) => {
		if (pub && pub !== lastPub) {
			// User has logged in or changed - initialize path with user's pub
			currentPath.set([pub]);
			lastPub = pub;
			console.log('Initialized currentPath with user public key:', pub);
		} else if (!pub && lastPub) {
			// User has logged out - clear path
			currentPath.set([]);
			lastPub = '';
		}
	});
}

// We will use the persist/manifest functions from state.ts for sync

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

		// Prevent zooming into nodes with contributors
		if (isContribution(node)) {
			globalState.showToast('Cannot zoom into nodes with contributors', 'warning');
			return;
		}

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
