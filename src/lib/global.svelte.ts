import { browser } from '$app/environment';
import { get, writable, type Writable } from 'svelte/store';
import toast from 'svelte-french-toast';
import { findNodeById, isContribution, reorderNode, wouldCreateCycle } from '$lib/protocol';
import { userPub } from '$lib/state/auth.svelte';
import { userTree } from '$lib/state/core.svelte';

// 🚨 CRITICAL FIX: Import subscriptions to initialize store persistence
// This sets up the userCapacities.subscribe() and other store subscriptions
// that trigger persistence functions when data changes
import '$lib/state/subscriptions.svelte';

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
	userPub.subscribe((pub) => {
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
	recomposeMode: false,
	textEditMode: false, // Controls whether clicking node titles enters edit mode
	nodeToEdit: '',
	editMode: false, // Global edit mode flag
	recomposing: false,
	editingNodeId: '', // ID of the node currently being edited
	newlyCreatedNodeId: '', // Track newly created nodes for auto-disable behavior
	initializationStarted: false,

	// Main view state
	currentView: 'tree' as 'tree' | 'map' | 'inventory',

	// Inventory search and filter state (shared between toolbar and components)
	inventorySearchQuery: '',
	inventorySelectedProvider: 'all',
	inventorySortBy: 'name' as 'name' | 'allocated_slots' | 'total_slots' | 'provider',
	inventorySortDirection: 'asc' as 'asc' | 'desc',

	// Unified highlighting state for newly created items
	highlightedCapacities: new Set<string>(),
	highlightedSlots: new Set<string>(),

	// Map state (fullscreen now handled by FullScreenControl)

	// Map search state
	searchQuery: '',
	searchResults: [] as any[],
	isSearchMode: false,
	searchSortBy: 'relevance' as 'relevance' | 'distance',
	timeFilterBy: 'any' as 'any' | 'now' | 'next24h' | 'between',
	timeFilterStartDate: '',
	timeFilterEndDate: '',
	timeFilterStartTime: '',
	timeFilterEndTime: '',
	showTimeFilterDetails: false,
	// Drag state
	isDragging: false,
	draggedNodeId: '',
	draggedNodeName: '',
	draggedNodeColor: '',
	dragX: 0,
	dragY: 0,

	// Ephemeral fulfillment state (used by slider for visual feedback during drag)
	ephemeralFulfillmentValues: new Map<string, number>(), // nodeId -> ephemeral percentage (0-100)
	ephemeralFulfillmentCounter: 0, // Counter to trigger reactivity when ephemeral values change
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

	// Check if we can zoom out (path has more than one level)
	canZoomOut: () => {
		const path = get(currentPath);
		return path.length > 1;
	},
	// Reset all state (logout)
	resetState: () => {
		userTree.set(null);
		currentPath.set([]);
		globalState.editMode = false;
		globalState.editingNodeId = '';
		globalState.nodeToEdit = '';
		globalState.newlyCreatedNodeId = '';
		globalState.deleteMode = false;
		globalState.textEditMode = false;
		globalState.currentView = 'tree';
		globalState.inventorySearchQuery = '';
		globalState.inventorySelectedProvider = 'all';
		globalState.inventorySortBy = 'name';
		globalState.inventorySortDirection = 'asc';

		// Reset search state
		globalState.searchQuery = '';
		globalState.searchResults = [];
		globalState.isSearchMode = false;
		globalState.searchSortBy = 'relevance';
		globalState.timeFilterBy = 'any';
		globalState.timeFilterStartDate = '';
		globalState.timeFilterEndDate = '';
		globalState.timeFilterStartTime = '';
		globalState.timeFilterEndTime = '';
		globalState.showTimeFilterDetails = false;

		// Clear chat subscriptions on logout
		if (browser) {
			import('$lib/state/chat.svelte').then(({ clearAllChatSubscriptions }) => {
				clearAllChatSubscriptions();
			});
		}
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

		// Don't allow toggling delete mode when in recompose mode
		if (globalState.recomposeMode) {
			globalState.showToast('Cannot toggle delete mode while in recompose mode', 'warning');
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

	// Toggle recompose mode
	toggleRecomposeMode: () => {
		// Don't allow toggling recompose mode when in edit mode
		if (globalState.editMode) {
			globalState.showToast('Cannot toggle recompose mode while editing', 'warning');
			return;
		}

		// Don't allow toggling recompose mode when in delete mode
		if (globalState.deleteMode) {
			globalState.showToast('Cannot toggle recompose mode while in delete mode', 'warning');
			return;
		}

		globalState.recomposeMode = !globalState.recomposeMode;
		globalState.showToast(
			globalState.recomposeMode
				? 'Recompose mode activated. Drag a node into another or to a path at the top.'
				: 'Recompose mode deactivated.',
			globalState.recomposeMode ? 'info' : 'info'
		);
	},

	// Toggle text edit mode
	toggleTextEditMode: () => {
		// Don't allow toggling text edit mode when actively editing a node
		if (globalState.editMode) {
			globalState.showToast('Cannot toggle text edit mode while editing a node', 'warning');
			return;
		}

		// Don't allow toggling text edit mode when in delete mode
		if (globalState.deleteMode) {
			globalState.showToast('Cannot toggle text edit mode while in delete mode', 'warning');
			return;
		}

		// Don't allow toggling text edit mode when in recompose mode
		if (globalState.recomposeMode) {
			globalState.showToast('Cannot toggle text edit mode while in recompose mode', 'warning');
			return;
		}

		globalState.textEditMode = !globalState.textEditMode;
		globalState.showToast(
			globalState.textEditMode
				? 'Text edit mode activated. Click node titles to edit them.'
				: 'Text edit mode deactivated. Node titles are no longer editable.',
			globalState.textEditMode ? 'info' : 'info'
		);
	},

	// Set a node to edit mode (temporary state) and ensure text edit mode is enabled
	setNodeToEditMode: (nodeId: string) => {
		console.log('[GLOBAL STATE] Setting node to edit mode:', nodeId);

		// Enable text edit mode if it's not already enabled
		if (!globalState.textEditMode) {
			globalState.textEditMode = true;
			globalState.showToast('Text edit mode enabled for editing', 'info');
		}

		// Track this as a newly created node for auto-disable behavior
		globalState.newlyCreatedNodeId = nodeId;
		globalState.nodeToEdit = nodeId;

		console.log('[GLOBAL STATE] Set newlyCreatedNodeId to:', globalState.newlyCreatedNodeId);

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

		// Don't allow editing when in recompose mode
		if (globalState.recomposeMode) {
			globalState.showToast('Cannot edit nodes in recompose mode', 'warning');
			return false;
		}

		// Enable text edit mode if it's not already enabled
		if (!globalState.textEditMode) {
			globalState.textEditMode = true;
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
		console.log('[GLOBAL STATE] Exiting edit mode:', {
			editingNodeId: globalState.editingNodeId,
			newlyCreatedNodeId: globalState.newlyCreatedNodeId,
			textEditMode: globalState.textEditMode
		});

		// If we were editing a newly created node, auto-disable text edit mode
		if (
			globalState.newlyCreatedNodeId &&
			globalState.editingNodeId === globalState.newlyCreatedNodeId
		) {
			globalState.textEditMode = false;
			globalState.showToast('Text edit mode disabled', 'info');
			console.log('[GLOBAL STATE] Auto-disabled text edit mode after editing newly created node');
		} else {
			console.log('[GLOBAL STATE] Not auto-disabling text edit mode:', {
				hasNewlyCreatedNodeId: !!globalState.newlyCreatedNodeId,
				idsMatch: globalState.editingNodeId === globalState.newlyCreatedNodeId
			});
		}

		globalState.editMode = false;
		globalState.editingNodeId = '';
		globalState.nodeToEdit = '';
		globalState.newlyCreatedNodeId = ''; // Clear the newly created node tracking
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
	},

	// Drag control functions
	startDrag: (nodeId: string, nodeName: string, nodeColor: string, x: number, y: number) => {
		globalState.isDragging = true;
		globalState.draggedNodeId = nodeId;
		globalState.draggedNodeName = nodeName;
		globalState.draggedNodeColor = nodeColor;
		globalState.dragX = x;
		globalState.dragY = y;
	},

	updateDragPosition: (x: number, y: number) => {
		globalState.dragX = x;
		globalState.dragY = y;
	},

	endDrag: () => {
		globalState.isDragging = false;
		globalState.draggedNodeId = '';
		globalState.draggedNodeName = '';
		globalState.draggedNodeColor = '';
		globalState.dragX = 0;
		globalState.dragY = 0;
	},

	// Ephemeral fulfillment functions (used by slider)
	updateEphemeralFulfillment: (nodeId: string, percentage: number) => {
		// Store ephemeral percentage (0-100) for visual updates during slider drag
		globalState.ephemeralFulfillmentValues.set(nodeId, Math.max(0, Math.min(100, percentage)));
		// Increment counter to trigger reactivity
		globalState.ephemeralFulfillmentCounter++;
	},

	clearEphemeralFulfillment: (nodeId: string) => {
		globalState.ephemeralFulfillmentValues.delete(nodeId);
		// Increment counter to trigger reactivity
		globalState.ephemeralFulfillmentCounter++;
	},

	getEffectiveFulfillmentPercentage: (
		nodeId: string,
		manualFulfillment?: number,
		fulfillment?: number
	) => {
		// Check for ephemeral value first (during drag)
		if (globalState.ephemeralFulfillmentValues.has(nodeId)) {
			return globalState.ephemeralFulfillmentValues.get(nodeId)!;
		}

		// Fall back to actual values
		if (manualFulfillment !== undefined) {
			return Math.max(0, Math.min(100, manualFulfillment * 100));
		} else if (fulfillment !== undefined) {
			return Math.max(0, Math.min(100, fulfillment * 100));
		}
		return 100; // Default to 100%
	},

	// Node reordering function
	handleNodeReorder: (sourceNodeId: string, targetNodeId: string) => {
		try {
			const tree = get(userTree);
			if (!tree) {
				globalState.showToast('No tree available for reordering', 'error');
				return false;
			}

			// Don't allow moving to self
			if (sourceNodeId === targetNodeId) {
				globalState.showToast('Cannot move node to itself', 'warning');
				return false;
			}

			// Create a deep clone of the tree to ensure reactivity
			const updatedTree = structuredClone(tree);

			// Check if this would create a cycle
			if (wouldCreateCycle(updatedTree, sourceNodeId, targetNodeId)) {
				globalState.showToast('Cannot move node to its own descendant', 'warning');
				return false;
			}

			// Find the source and target nodes in the cloned tree
			const sourceNode = findNodeById(updatedTree, sourceNodeId);
			const targetNode = findNodeById(updatedTree, targetNodeId);

			if (!sourceNode) {
				globalState.showToast('Source node not found', 'error');
				return false;
			}

			if (!targetNode) {
				globalState.showToast('Target node not found', 'error');
				return false;
			}

			// Check if target node has contributors (cannot have children)
			if (isContribution(targetNode)) {
				globalState.showToast(
					`Cannot move node to "${targetNode.name}" - nodes with contributors cannot have children`,
					'warning'
				);
				return false;
			}

			// Use the protocol function to reorder the node
			const success = reorderNode(updatedTree, sourceNodeId, targetNodeId);

			if (!success) {
				globalState.showToast('Failed to reorder node', 'error');
				return false;
			}

			// Update the store with the new tree to trigger reactivity
			userTree.set(updatedTree);

			globalState.showToast(`Moved "${sourceNode.name}" to "${targetNode.name}"`, 'success');
			console.log(`[REORDER] Successfully moved ${sourceNode.name} to ${targetNode.name}`);
			return true;
		} catch (err) {
			console.error('Error in reorder process:', err);
			globalState.showToast('Error reordering node', 'error');
			return false;
		}
	},

	/**
	 * Map Search State Management
	 */

	// Update search query and trigger search mode
	updateSearchQuery: (query: string) => {
		globalState.searchQuery = query;
		globalState.isSearchMode = !!query.trim() || globalState.timeFilterBy !== 'any';
	},

	// Update time filter
	updateTimeFilter: (filter: 'any' | 'now' | 'next24h' | 'between') => {
		globalState.timeFilterBy = filter;
		globalState.showTimeFilterDetails = filter === 'between';
		globalState.isSearchMode = !!globalState.searchQuery.trim() || filter !== 'any';
	},

	// Update time filter details
	updateTimeFilterDetails: (details: {
		startDate: string;
		endDate: string;
		startTime: string;
		endTime: string;
	}) => {
		globalState.timeFilterStartDate = details.startDate;
		globalState.timeFilterEndDate = details.endDate;
		globalState.timeFilterStartTime = details.startTime;
		globalState.timeFilterEndTime = details.endTime;
	},

	// Clear search state
	clearSearch: () => {
		globalState.searchQuery = '';
		globalState.searchResults = [];
		globalState.isSearchMode = false;
		globalState.searchSortBy = 'relevance';
		globalState.timeFilterBy = 'any';
		globalState.timeFilterStartDate = '';
		globalState.timeFilterEndDate = '';
		globalState.timeFilterStartTime = '';
		globalState.timeFilterEndTime = '';
		globalState.showTimeFilterDetails = false;
	},

	// Update search results
	updateSearchResults: (results: any[]) => {
		globalState.searchResults = results;
	},

	// Update search sort
	updateSearchSort: (sortBy: 'relevance' | 'distance') => {
		globalState.searchSortBy = sortBy;
	},

	// Toggle time filter details
	toggleTimeFilterDetails: () => {
		globalState.showTimeFilterDetails = !globalState.showTimeFilterDetails;
	},

	/**
	 * Unified Highlighting System
	 */

	// Generic scroll function for any element with data attribute
	scrollToElement: (selector: string, elementType: string) => {
		if (!browser) return;

		// Function to attempt scrolling with retry logic
		const attemptScroll = (retries = 5) => {
			const element = document.querySelector(selector) as HTMLElement;

			if (element) {
				// Element found, scroll to it
				element.scrollIntoView({
					behavior: 'smooth',
					block: 'center',
					inline: 'nearest'
				});

				console.log(`[GLOBAL STATE] Successfully scrolled to ${elementType}:`, selector);
			} else if (retries > 0) {
				// Element not found yet, retry after a short delay
				setTimeout(() => attemptScroll(retries - 1), 200);
			} else {
				console.warn(
					`[GLOBAL STATE] Could not find ${elementType} element to scroll to:`,
					selector
				);
			}
		};

		// Start the scroll attempt
		setTimeout(() => attemptScroll(), 100);
	},

	// Scroll to a specific capacity element
	scrollToCapacity: (capacityId: string) => {
		globalState.scrollToElement(`[data-capacity-id="${capacityId}"]`, 'capacity');
	},

	// Scroll to a specific slot element
	scrollToSlot: (slotId: string) => {
		globalState.scrollToElement(`[data-slot-id="${slotId}"]`, 'slot');
	},

	// Add a capacity to the highlight list and scroll to it
	highlightCapacity: (capacityId: string) => {
		globalState.highlightedCapacities = new Set([...globalState.highlightedCapacities, capacityId]);

		// Scroll to the newly highlighted capacity
		globalState.scrollToCapacity(capacityId);

		// Auto-remove highlight after 3 seconds
		if (browser) {
			setTimeout(() => {
				globalState.removeCapacityHighlight(capacityId);
			}, 3000);
		}
	},

	// Add a slot to the highlight list and scroll to it
	highlightSlot: (slotId: string) => {
		globalState.highlightedSlots = new Set([...globalState.highlightedSlots, slotId]);

		// Scroll to the newly highlighted slot
		globalState.scrollToSlot(slotId);

		// Auto-remove highlight after 3 seconds
		if (browser) {
			setTimeout(() => {
				globalState.removeSlotHighlight(slotId);
			}, 3000);
		}
	},

	// Remove a specific capacity from the highlight list
	removeCapacityHighlight: (capacityId: string) => {
		const newSet = new Set(globalState.highlightedCapacities);
		newSet.delete(capacityId);
		globalState.highlightedCapacities = newSet;
	},

	// Remove a specific slot from the highlight list
	removeSlotHighlight: (slotId: string) => {
		const newSet = new Set(globalState.highlightedSlots);
		newSet.delete(slotId);
		globalState.highlightedSlots = newSet;
	},

	// Clear all capacity highlights
	clearAllCapacityHighlights: () => {
		globalState.highlightedCapacities = new Set();
	},

	// Clear all slot highlights
	clearAllSlotHighlights: () => {
		globalState.highlightedSlots = new Set();
	},

	// Clear all highlights (both capacities and slots)
	clearAllHighlights: () => {
		globalState.highlightedCapacities = new Set();
		globalState.highlightedSlots = new Set();
	},

	/**
	 * View Management
	 */
	setView: (view: 'tree' | 'map' | 'inventory') => {
		globalState.currentView = view;
	}

	/**
	 * Map fullscreen is now handled by FullScreenControl component
	 */
});
