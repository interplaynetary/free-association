import { createRec } from '../stores/rec.svelte';
import { get } from 'svelte/store';

export const globalState = $state({
	recStore: createRec(),
	deleteMode: false,
	nodeToEdit: '',
	toast: {
		visible: false,
		message: '',
		type: 'info' as 'info' | 'success' | 'warning' | 'error',
		timeoutId: null as number | null
	},
	toggleDeleteMode: () => {
		globalState.deleteMode = !globalState.deleteMode;
		console.log(`Delete mode: ${globalState.deleteMode ? 'activated' : 'deactivated'}`);

		// Show toast about delete mode
		
		globalState.showToast(
			globalState.deleteMode
				? 'Delete mode activated. Click a node to delete it.'
				: 'Delete mode deactivated.',
			globalState.deleteMode ? 'warning' : 'info'
		);
	},
	setNodeToEditMode: (nodeId: string) => {
		globalState.nodeToEdit = nodeId;
		// This flag will be used by the Child component to know when to enter edit mode

		// Auto-clear the edit flag after a short delay if it wasn't handled
		setTimeout(() => {
			if (globalState.nodeToEdit === nodeId) {
				globalState.nodeToEdit = '';
			}
		}, 1000);
	},
	handleAddNode: () => Promise.resolve<void>(undefined),
	zoomIntoChild: async (nodeId: string) => {
		// Get the child store
		const childStore = globalState.recStore.getChild(nodeId);

		// Update our local store
		globalState.recStore = childStore;

		console.log(`Zoomed into node ${nodeId}, new path: ${childStore.path.join('/')}`);
		globalState.showToast(`Zoomed into ${get(childStore.nameStore) || 'node'}`, 'info');
	},
	zoomOutToParent: async () => {
		// Check if we have a parent to zoom out to
		const parentStore = await globalState.recStore.getParent();
		if (parentStore) {
			// Update the store to the parent
			globalState.recStore = parentStore;
			console.log(`Zoomed out to parent, new path: ${globalState.recStore.path.join('/')}`);
			globalState.showToast(`Zoomed out to parent`, 'info');
		} else {
			console.log('Already at root level, cannot zoom out further');
			globalState.showToast('Already at top level', 'info');
		}
	},
	navigateToPathInIndex: async (index: number) => {
		// Get the path to root
		const pathToRoot = await globalState.recStore.getPathToRoot();

		// Calculate the target node from the index
		// pathToRoot is ordered from current to root, so we need to reverse the index
		const targetIndex = pathToRoot.length - 1 - index;

		if (targetIndex >= 0 && targetIndex < pathToRoot.length) {
			const target = pathToRoot[targetIndex];
			globalState.recStore = target.store;
			console.log(
				`Navigated to path index ${index}, new path: ${globalState.recStore.path.join('/')}`
			);
			globalState.showToast(`Navigated to node`, 'info');
		}
	},
	navigateToPath: () => undefined,
	navigateToSoul: () => undefined,
	showToast: (message: string, type: 'info' | 'success' | 'warning' | 'error' = 'info') => {
		// Clear any existing toast timeout
		if (globalState.toast.timeoutId) {
			clearTimeout(globalState.toast.timeoutId);
		}
	
		// Show new toast
		globalState.toast = {
			visible: true,
			message,
			type,
			timeoutId: window.setTimeout(() => {
				globalState.toast.visible = false;
			}, 3000) as unknown as number
		}; // Clear any existing toast timeout
		if (globalState.toast.timeoutId) {
			clearTimeout(globalState.toast.timeoutId);
		}
	
		// Show new toast
		globalState.toast = {
			visible: true,
			message,
			type,
			timeoutId: window.setTimeout(() => {
				globalState.toast.visible = false;
			}, 3000) as unknown as number
		};
	}
});