import { createExampleForest, addToForest, enterChild, exitToParent } from '$lib/centralized';
import type { TreeZipper, Forest } from '$lib/centralized';
import { browser } from '$app/environment';

type ToastType = 'info' | 'success' | 'warning' | 'error';

export const globalState = $state({
	// Core navigation state (using our centralized system)
	currentForest: new Map() as Forest,
	currentZipper: null as TreeZipper | null,
	currentPath: [] as string[],

	// UI state
	deleteMode: false,
	nodeToEdit: '',
	toast: {
		visible: false,
		message: '',
		type: 'info' as ToastType,
		timeoutId: null as number | null
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

	// Handle adding a new node (implementation is in Parent.svelte)
	handleAddNode: () => {},

	// Navigate directly to a specific path index
	navigateToPathIndex: (index: number) => {
		if (index < 0 || index >= globalState.currentPath.length) return;

		if (!globalState.currentForest) return;

		// Get the root zipper
		const rootId = globalState.currentPath[0];
		const rootZipper = globalState.currentForest.get(rootId);
		if (!rootZipper) return;

		// Start with the root
		let currentZipper = rootZipper;
		const newPath = [rootId];

		// Follow the path up to the target index
		for (let i = 1; i <= index; i++) {
			const childId = globalState.currentPath[i];
			const childZipper = enterChild(childId, currentZipper);
			if (!childZipper) break;

			currentZipper = childZipper;
			newPath.push(childId);
		}

		// Update state
		globalState.currentZipper = currentZipper;
		globalState.currentPath = newPath;

		globalState.showToast(`Navigated to ${currentZipper.zipperCurrent.nodeName || 'node'}`, 'info');
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
