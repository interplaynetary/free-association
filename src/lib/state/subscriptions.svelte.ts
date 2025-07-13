import { get } from 'svelte/store';
import type { Node } from '$lib/schema';
import {
	userTree,
	userCapacities,
	userSogf,
	nodesMap,
	isLoadingTree,
	isLoadingCapacities,
	isRecalculatingTree,
	contributorCapacityShares
} from './core.svelte';
import { userContacts, isLoadingContacts } from './users.svelte';
import { userDesiredComposeFrom, userDesiredComposeInto } from './compose.svelte';
import { chatReadStates, isLoadingChatReadStates } from './chat.svelte';
import {
	persistTree,
	persistCapacities,
	persistSogf,
	persistContributorCapacityShares,
	persistUserDesiredComposeFrom,
	persistUserDesiredComposeInto,
	persistContacts,
	persistChatReadStates
} from './persistence.svelte';
import { recalculateFromTree } from './calculations.svelte';

/**
 * Debounce helper for subscription persistence
 */
export function debounce<T extends (...args: any[]) => void>(func: T, delay: number): T {
	let timeoutId: any;
	return ((...args: any[]) => {
		clearTimeout(timeoutId);
		timeoutId = setTimeout(() => func(...args), delay);
	}) as T;
}

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
	if (!get(isLoadingCapacities)) {
		try {
			persistCapacities();
		} catch (error) {
			console.error('[CAPACITIES-SUB] Error during debounced persistence:', error);
		}
	} else {
		console.log('[CAPACITIES-SUB] Skipping persistence because capacities are being loaded');
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

const debouncedPersistUserDesiredComposeFrom = debounce(() => {
	console.log('[USER-COMPOSE-FROM-SUB] Executing debounced compose-from persistence');
	try {
		persistUserDesiredComposeFrom();
	} catch (error) {
		console.error('[USER-COMPOSE-FROM-SUB] Error during debounced persistence:', error);
	}
}, 250);

const debouncedPersistUserDesiredComposeInto = debounce(() => {
	console.log('[USER-COMPOSE-INTO-SUB] Executing debounced compose-into persistence');
	try {
		persistUserDesiredComposeInto();
	} catch (error) {
		console.error('[USER-COMPOSE-INTO-SUB] Error during debounced persistence:', error);
	}
}, 250);

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
	if (!capacities) return;

	console.log('[CAPACITIES-SUB] Capacities updated');
	console.log('[CAPACITIES-SUB] Capacities count:', Object.keys(capacities).length);

	// Debounced persistence function
	debouncedPersistCapacities();
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
 * Trigger persistence when user desired compose-from changes
 */
userDesiredComposeFrom.subscribe((userDesiredComposeFrom) => {
	console.log('[USER-COMPOSE-FROM-SUB] User desired compose-from updated');
	console.log(
		'[USER-COMPOSE-FROM-SUB] User desired compose-from capacities:',
		Object.keys(userDesiredComposeFrom).length
	);

	// Don't persist empty compose-from during initialization
	if (Object.keys(userDesiredComposeFrom).length === 0) {
		console.log(
			'[USER-COMPOSE-FROM-SUB] Skipping persistence of empty compose-from (likely initialization)'
		);
		return;
	}

	// Debounced persistence function
	debouncedPersistUserDesiredComposeFrom();
});

/**
 * Trigger persistence when user desired compose-into changes
 */
userDesiredComposeInto.subscribe((userDesiredComposeInto) => {
	console.log('[USER-COMPOSE-INTO-SUB] User desired compose-into updated');
	console.log(
		'[USER-COMPOSE-INTO-SUB] User desired compose-into capacities:',
		Object.keys(userDesiredComposeInto).length
	);

	// Don't persist empty compose-into during initialization
	if (Object.keys(userDesiredComposeInto).length === 0) {
		console.log(
			'[USER-COMPOSE-INTO-SUB] Skipping persistence of empty compose-into (likely initialization)'
		);
		return;
	}

	// Debounced persistence function
	debouncedPersistUserDesiredComposeInto();
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
