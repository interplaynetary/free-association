import { browser } from '$app/environment';
import { get, writable, derived, type Writable, type Readable } from 'svelte/store';
import toast from 'svelte-french-toast';
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
import { username, userpub, userTree } from '$lib/state.svelte';

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
	editMode: false, // Global edit mode flag
	editingNodeId: '', // ID of the node currently being edited
	initializationStarted: false,
	navigateToPath: async (newPath: string[]) => {
		// Prevent navigation when in edit mode
		if (globalState.editMode) {
			console.log('Navigation blocked: currently in edit mode');
			return;
		}

		console.log('Navigating to path', newPath);
		if (!newPath.length || !get(userTree)) return;
		currentPath.set(newPath);
	},
	navigateToPathIndex: (index: number) => {
		// Prevent navigation when in edit mode
		if (globalState.editMode) {
			console.log('Navigation blocked: currently in edit mode');
			return;
		}

		const path = get(currentPath);
		if (index < 0 || index >= path.length) return;
		if (index === path.length - 1) return; // Already at position
		currentPath.set(path.slice(0, index + 1));
	},
	// Add a node ID to the path (zoom in)
	zoomInto: (nodeId: string) => {
		// Prevent navigation when in edit mode
		if (globalState.editMode) {
			console.log('Navigation blocked: currently in edit mode');
			return;
		}

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
		// Prevent navigation when in edit mode
		if (globalState.editMode) {
			console.log('Navigation blocked: currently in edit mode');
			return;
		}

		const path = get(currentPath);
		if (path.length <= 1) return;
		currentPath.set(path.slice(0, -1));
	},
	// Reset all state (logout)
	resetState: () => {
		userTree.set(null);
		currentPath.set([]);
		globalState.editMode = false;
		globalState.editingNodeId = '';
		globalState.nodeToEdit = '';
		globalState.deleteMode = false;
	},

	/**
	 * UI State Management
	 */

	// Toggle delete mode
	toggleDeleteMode: () => {
		// Don't allow toggling delete mode when in edit mode
		if (globalState.editMode) {
			globalState.showToast('Cannot toggle delete mode while editing', 'warning');
			return;
		}

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

	// Enter edit mode for a specific node
	enterEditMode: (nodeId: string) => {
		// Don't allow editing when in delete mode
		if (globalState.deleteMode) {
			globalState.showToast('Cannot edit nodes in delete mode', 'warning');
			return false;
		}

		// If already editing a different node, exit that first
		if (globalState.editMode && globalState.editingNodeId !== nodeId) {
			globalState.exitEditMode();
		}

		globalState.editMode = true;
		globalState.editingNodeId = nodeId;
		console.log('[GLOBAL STATE] Entered edit mode for node:', nodeId);
		return true;
	},

	// Exit edit mode
	exitEditMode: () => {
		globalState.editMode = false;
		globalState.editingNodeId = '';
		globalState.nodeToEdit = '';
		console.log('[GLOBAL STATE] Exited edit mode');
	},

	// Display a toast notification using svelte-french-toast
	showToast: (message: string, type: ToastType = 'info') => {
		switch (type) {
			case 'success':
				toast.success(message);
				break;
			case 'error':
				toast.error(message);
				break;
			case 'warning':
				toast(message, {
					icon: '⚠️',
					style: 'border: 1px solid #f59e0b; background: #fef3c7; color: #92400e;'
				});
				break;
			case 'info':
			default:
				toast(message, {
					icon: 'ℹ️'
				});
				break;
		}
	}
});
