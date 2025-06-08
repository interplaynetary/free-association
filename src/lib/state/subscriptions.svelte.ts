import { get } from 'svelte/store';
import type { Node } from '$lib/schema';
import {
	userTree,
	userCapacities,
	nodesMap,
	isLoadingTree,
	isLoadingCapacities,
	isRecalculatingTree,
	contributorCapacityShares
} from './core.svelte';
import { persistTree, persistCapacities, persistContributorCapacityShares } from './gun.svelte';
import { recalculateFromTree } from './calculations.svelte';

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

	// Force immediate capacity persistence on every change
	// This ensures capacity changes are always saved
	if (!get(isLoadingCapacities)) {
		console.log('[CAPACITIES-SUB] Persisting capacities');
		try {
			persistCapacities();
		} catch (error) {
			console.error('[CAPACITIES-SUB] Error during persistence:', error);
		}
	} else {
		console.log('[CAPACITIES-SUB] Skipping persistence because capacities are being loaded');
	}
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

	// Persist the shares to gun
	persistContributorCapacityShares();
});


