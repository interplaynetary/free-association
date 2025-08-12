import { get } from 'svelte/store';
import {
	userSogf,
	userTree,
	userCapacities,
	isLoadingTree,
	isLoadingCapacities,
	isRecalculatingTree,
	nodesMap,
	contributorCapacityShares,
	capacitySlotQuantities
} from './core.svelte';
import { userContacts, isLoadingContacts } from './users.svelte';
import { recalculateFromTree } from './calculations.svelte';
import {
	persistTree,
	persistSogf,
	persistCapacities,
	persistContributorCapacityShares,
	persistContributorCapacitySlotQuantities,
	persistContacts,
	persistChatReadStates
} from './persistence.svelte';
import type { Node } from '$lib/schema';
import { userDesiredSlotComposeFrom, userDesiredSlotComposeInto } from './core.svelte';
import {
	persistUserDesiredSlotComposeFrom,
	persistUserDesiredSlotComposeInto
} from '$lib/state/persistence.svelte';
import { chatReadStates, isLoadingChatReadStates } from '$lib/state/chat.svelte';
import { debounce } from '$lib/utils/debounce';

// Debounced persistence functions
const debouncedPersistSogf = debounce(() => {
	console.log('[SOGF-SUB] Executing debounced SOGF persistence');
	try {
		persistSogf();
	} catch (error) {
		console.error('[SOGF-SUB] Error during debounced persistence:', error);
	}
}, 300);

const debouncedPersistCapacities = debounce(() => {
	console.log('[CAPACITIES-SUB] Executing debounced capacities persistence');
	console.log('[CAPACITIES-SUB] 🚨 DEBUG: debouncedPersistCapacities debounced function executing');

	// 🚨 SMART RACE CONDITION PROTECTION: The persistCapacities function
	// will check isLoadingCapacities and defer if needed, preventing
	// local changes from overwriting incoming network data
	try {
		console.log('[CAPACITIES-SUB] 🚨 DEBUG: About to call persistCapacities()');
		persistCapacities();
		console.log('[CAPACITIES-SUB] 🚨 DEBUG: persistCapacities() completed');
	} catch (error) {
		console.error('[CAPACITIES-SUB] Error during debounced persistence:', error);
		console.error('[CAPACITIES-SUB] 🚨 DEBUG: Error details:', {
			name: error instanceof Error ? error.name : 'Unknown',
			message: error instanceof Error ? error.message : String(error),
			stack: error instanceof Error ? error.stack : undefined
		});
	}
}, 200);

const debouncedPersistContributorCapacityShares = debounce(() => {
	console.log(
		'[CONTRIBUTOR-CAPACITY-SHARES-SUB] Executing debounced contributor capacity shares persistence'
	);
	try {
		persistContributorCapacityShares();
	} catch (error) {
		console.error('[CONTRIBUTOR-CAPACITY-SHARES-SUB] Error during debounced persistence:', error);
	}
}, 200);

const debouncedPersistUserDesiredSlotComposeFrom = debounce(() => {
	console.log('[USER-SLOT-COMPOSE-FROM-SUB] Executing debounced slot compose-from persistence');
	try {
		persistUserDesiredSlotComposeFrom();
	} catch (error) {
		console.error('[USER-SLOT-COMPOSE-FROM-SUB] Error during debounced persistence:', error);
	}
}, 250);

const debouncedPersistUserDesiredSlotComposeInto = debounce(() => {
	console.log('[USER-SLOT-COMPOSE-INTO-SUB] Executing debounced slot compose-into persistence');
	try {
		persistUserDesiredSlotComposeInto();
	} catch (error) {
		console.error('[USER-SLOT-COMPOSE-INTO-SUB] Error during debounced persistence:', error);
	}
}, 250);

const debouncedPersistContributorCapacitySlotQuantities = debounce((slotQuantities) => {
	console.log(
		'[CAPACITY-SLOT-QUANTITIES-SUB] Executing debounced capacity slot quantities persistence'
	);
	try {
		persistContributorCapacitySlotQuantities(slotQuantities);
	} catch (error) {
		console.error('[CAPACITY-SLOT-QUANTITIES-SUB] Error during debounced persistence:', error);
	}
}, 200);

const debouncedPersistContacts = debounce(() => {
	console.log('[CONTACTS-SUB] Executing debounced contacts persistence');
	if (!get(isLoadingContacts)) {
		try {
			persistContacts();
		} catch (error) {
			console.error('[CONTACTS-SUB] Error during debounced persistence:', error);
		}
	} else {
		console.log('[CONTACTS-SUB] Skipping persistence because contacts are being loaded');
	}
}, 200);

const debouncedPersistChatReadStates = debounce(() => {
	console.log('[CHAT-READ-STATES-SUB] Executing debounced chat read states persistence');
	if (!get(isLoadingChatReadStates)) {
		try {
			persistChatReadStates();
		} catch (error) {
			console.error('[CHAT-READ-STATES-SUB] Error during debounced persistence:', error);
		}
	} else {
		console.log('[CHAT-READ-STATES-SUB] Skipping persistence because read states are being loaded');
	}
}, 200);

/**
 * Update nodes map whenever the tree changes
 */
userTree.subscribe((tree) => {
	if (tree) {
		console.log('[TREE-SUB] Tree updated in store, rebuilding nodes map');
		console.log('[TREE-SUB] Tree has', tree.children.length, 'children');

		// Create a map of all nodes by ID for faster lookup
		const newNodesMap: Record<string, Node> = {};

		// Helper to traverse tree and add nodes to map
		function addNodeToMap(node: Node) {
			newNodesMap[node.id] = node;
			node.children.forEach(addNodeToMap);
		}

		addNodeToMap(tree);
		nodesMap.set(newNodesMap);

		console.log('[TREE-SUB] Rebuilt nodes map with', Object.keys(newNodesMap).length, 'nodes');

		// Force immediate tree persistence on every tree change
		// This ensures tree changes are always saved, even if recalculation fails
		console.log('[TREE-SUB] Forcing immediate tree persistence');
		persistTree();
	}
});

/**
 * Trigger persistence when SOGF changes
 */
userSogf.subscribe((sogf) => {
	if (!sogf) return;

	console.log('[SOGF-SUB] SOGF updated');
	console.log('[SOGF-SUB] SOGF contributor count:', Object.keys(sogf).length);

	// Debounced persistence function
	debouncedPersistSogf();
});

/**
 * Trigger recalculations when tree changes
 * Uses a simple debounce to prevent excessive calculations during rapid changes
 */
let treeRecalcTimer: ReturnType<typeof setTimeout> | null = null;
userTree.subscribe((tree) => {
	if (!tree) return;

	console.log('[TREE-RECALC-SUB] Tree updated, scheduling recalculation');
	console.log('[TREE-RECALC-SUB] Tree has', tree.children.length, 'children');

	// Skip recalculation if we're just loading from storage
	if (get(isLoadingTree)) {
		console.log('[TREE-RECALC-SUB] Skipping recalculation because tree is being loaded');
		return;
	}

	// Skip recalculation if this update is from a recalculation
	if (get(isRecalculatingTree)) {
		console.log('[TREE-RECALC-SUB] Skipping recalculation because update is from recalculation');
		return;
	}

	// Clear any pending recalculation
	if (treeRecalcTimer) {
		console.log('[TREE-RECALC-SUB] Clearing previous recalc timer');
		clearTimeout(treeRecalcTimer);
	}

	// Schedule a recalculation after a short delay
	console.log('[TREE-RECALC-SUB] Setting new recalc timer for 300ms');
	treeRecalcTimer = setTimeout(() => {
		console.log('[TREE-RECALC-SUB] Timer fired, running recalculation');

		// Try to run the full recalculation, but catch any errors to prevent
		// them from blocking the tree persistence
		try {
			recalculateFromTree();
		} catch (error) {
			console.error('[TREE-RECALC-SUB] Error during recalculation:', error);

			// Even if recalculation fails, ensure the tree is persisted
			console.log('[TREE-RECALC-SUB] Forcing tree persistence after error');
			persistTree();
		}

		treeRecalcTimer = null;
	}, 300); // 300ms debounce
});

/**
 * Trigger persistence when capacities change
 */
userCapacities.subscribe((capacities) => {
	console.log('[CAPACITIES-SUB] 🚨 DEBUG: userCapacities subscription triggered');
	console.log('[CAPACITIES-SUB] 🚨 DEBUG: capacities value:', capacities);
	console.log('[CAPACITIES-SUB] 🚨 DEBUG: capacities type:', typeof capacities);

	if (!capacities) {
		console.log('[CAPACITIES-SUB] 🚨 DEBUG: capacities is null/undefined, returning');
		return;
	}

	console.log('[CAPACITIES-SUB] Capacities updated');
	console.log('[CAPACITIES-SUB] Capacities count:', Object.keys(capacities).length);
	console.log('[CAPACITIES-SUB] 🚨 DEBUG: About to call debouncedPersistCapacities');

	// Debounced persistence function
	debouncedPersistCapacities();

	console.log('[CAPACITIES-SUB] 🚨 DEBUG: debouncedPersistCapacities called');
});

/**
 * Trigger persistence when contributor capacity shares change
 */
contributorCapacityShares.subscribe((contributorCapacityShares) => {
	console.log('[CONTRIBUTOR-CAPACITY-SHARES-SUB] Contributor capacity shares updated');
	console.log(
		'[CONTRIBUTOR-CAPACITY-SHARES-SUB] Contributor capacity shares count:',
		Object.keys(contributorCapacityShares).length
	);

	// Debounced persistence function
	debouncedPersistContributorCapacityShares();
});

/**
 * Subscribe to capacity slot quantities to trigger their persistence
 */
capacitySlotQuantities.subscribe((slotQuantities) => {
	console.log('[CAPACITY-SLOT-QUANTITIES-SUB] Capacity slot quantities updated');
	console.log(
		'[CAPACITY-SLOT-QUANTITIES-SUB] Slot quantities count:',
		Object.keys(slotQuantities).length
	);

	// Don't persist empty slot quantities during initialization
	if (Object.keys(slotQuantities).length === 0) {
		console.log(
			'[CAPACITY-SLOT-QUANTITIES-SUB] Skipping persistence of empty slot quantities (likely initialization)'
		);
		return;
	}

	// Debounced persistence function
	debouncedPersistContributorCapacitySlotQuantities(slotQuantities);
});

/**
 * Trigger persistence when user desired slot compose-from changes
 */
userDesiredSlotComposeFrom.subscribe((userDesiredSlotComposeFrom) => {
	console.log('[USER-SLOT-COMPOSE-FROM-SUB] User desired slot compose-from updated');
	console.log(
		'[USER-SLOT-COMPOSE-FROM-SUB] User desired slot compose-from capacities:',
		Object.keys(userDesiredSlotComposeFrom).length
	);

	// Don't persist empty slot compose-from during initialization
	if (Object.keys(userDesiredSlotComposeFrom).length === 0) {
		console.log(
			'[USER-SLOT-COMPOSE-FROM-SUB] Skipping persistence of empty slot compose-from (likely initialization)'
		);
		return;
	}

	// Debounced persistence function
	debouncedPersistUserDesiredSlotComposeFrom();
});

/**
 * Trigger persistence when user desired slot compose-into changes
 */
userDesiredSlotComposeInto.subscribe((userDesiredSlotComposeInto) => {
	console.log('[USER-SLOT-COMPOSE-INTO-SUB] User desired slot compose-into updated');
	console.log(
		'[USER-SLOT-COMPOSE-INTO-SUB] User desired slot compose-into capacities:',
		Object.keys(userDesiredSlotComposeInto).length
	);

	// Don't persist empty slot compose-into during initialization
	if (Object.keys(userDesiredSlotComposeInto).length === 0) {
		console.log(
			'[USER-SLOT-COMPOSE-INTO-SUB] Skipping persistence of empty slot compose-into (likely initialization)'
		);
		return;
	}

	// Debounced persistence function
	debouncedPersistUserDesiredSlotComposeInto();
});

/**
 * Trigger persistence when contacts change
 */
userContacts.subscribe((contacts) => {
	if (!contacts) return;

	console.log('[CONTACTS-SUB] Contacts updated');
	console.log('[CONTACTS-SUB] Contacts count:', Object.keys(contacts).length);

	// Don't persist empty contacts during initialization to avoid race condition
	// where empty store overwrites loaded data from network
	if (Object.keys(contacts).length === 0) {
		console.log('[CONTACTS-SUB] Skipping persistence of empty contacts (likely initialization)');
		return;
	}

	// Debounced persistence function
	debouncedPersistContacts();
});

/**
 * Trigger persistence when chat read states change
 */
chatReadStates.subscribe((readStates) => {
	if (!readStates) return;

	console.log('[CHAT-READ-STATES-SUB] Chat read states updated');
	console.log('[CHAT-READ-STATES-SUB] Read states count:', Object.keys(readStates).length);

	// Don't persist empty read states during initialization
	if (Object.keys(readStates).length === 0) {
		console.log(
			'[CHAT-READ-STATES-SUB] Skipping persistence of empty read states (likely initialization)'
		);
		return;
	}

	// Debounced persistence function
	debouncedPersistChatReadStates();
});

/**
 * Trigger recalculation when contacts change (for proper contact ID resolution)
 * This ensures SOGF calculations use correctly resolved public keys
 */
let contactsRecalcTimer: ReturnType<typeof setTimeout> | null = null;
userContacts.subscribe((contacts) => {
	// Only recalculate if we have a tree and contacts are not being loaded
	if (!get(userTree) || get(isLoadingContacts)) return;

	console.log(
		'[CONTACTS-RECALC-SUB] Contacts updated, scheduling recalculation for proper resolution'
	);

	// Clear any pending recalculation
	if (contactsRecalcTimer) {
		clearTimeout(contactsRecalcTimer);
	}

	// Schedule a recalculation after a short delay to ensure contact resolution works
	contactsRecalcTimer = setTimeout(() => {
		console.log('[CONTACTS-RECALC-SUB] Timer fired, running recalculation with updated contacts');
		try {
			recalculateFromTree();
		} catch (error) {
			console.error('[CONTACTS-RECALC-SUB] Error during contacts-triggered recalculation:', error);
		}
		contactsRecalcTimer = null;
	}, 100); // Short delay to allow reactive updates
});

/**
 * Trigger recalculation after all loading is complete
 * This ensures proper calculation when both tree and contacts are available
 */
function tryRecalculateAfterLoading() {
	// Check if all critical stores are done loading
	const treeLoading = get(isLoadingTree);
	const contactsLoading = get(isLoadingContacts);
	const isRecalculating = get(isRecalculatingTree);

	// Only proceed if nothing is loading and we're not already recalculating
	if (!treeLoading && !contactsLoading && !isRecalculating && get(userTree)) {
		console.log('[POST-LOAD-RECALC] All loading complete, triggering recalculation');
		setTimeout(() => {
			try {
				recalculateFromTree();
			} catch (error) {
				console.error('[POST-LOAD-RECALC] Error during post-loading recalculation:', error);
			}
		}, 200); // Allow time for reactive updates to settle
	}
}

// Watch for loading state changes to trigger post-load recalculation
isLoadingTree.subscribe((loading) => {
	if (!loading) {
		console.log('[POST-LOAD-RECALC] Tree loading completed');
		tryRecalculateAfterLoading();
	}
});

isLoadingContacts.subscribe((loading) => {
	if (!loading) {
		console.log('[POST-LOAD-RECALC] Contacts loading completed');
		tryRecalculateAfterLoading();
	}
});
