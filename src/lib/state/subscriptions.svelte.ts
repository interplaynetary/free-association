import { get } from 'svelte/store';
import type { Node } from '$lib/schema';
import {
	userTree,
	userCapacities,
	nodesMap,
	isLoadingTree,
	isLoadingCapacities,
	isRecalculatingTree,
	isRecalculatingCapacities
} from './core.svelte';
import { persistTree, persistCapacities } from './gun.svelte';
import { recalculateFromTree, recalculateFromCapacities } from './calculations.svelte';

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
 * Trigger recalculations when capacities change
 */
let capacitiesRecalcTimer: ReturnType<typeof setTimeout> | null = null;
userCapacities.subscribe((capacities) => {
	if (!capacities) return;

	console.log('[CAPACITIES-SUB] Capacities updated, scheduling recalculation');
	console.log('[CAPACITIES-SUB] Capacities data:', capacities);
	console.log('[CAPACITIES-SUB] Capacities count:', Object.keys(capacities).length);

	// Force immediate capacity persistence on every change
	// This ensures capacity changes are always saved, even if recalculation fails
	console.log('[CAPACITIES-SUB] Forcing immediate capacities persistence');
	try {
		persistCapacities();
	} catch (error) {
		console.error('[CAPACITIES-SUB] Error during immediate persistence:', error);
	}

	// Skip recalculation if we're just loading from storage
	if (get(isLoadingCapacities)) {
		console.log('[CAPACITIES-SUB] Skipping recalculation because capacities are being loaded');
		return;
	}

	// Skip recalculation if this update is from a recalculation
	if (get(isRecalculatingCapacities)) {
		console.log('[CAPACITIES-SUB] Skipping recalculation because update is from recalculation');
		return;
	}

	// Clear any pending recalculation
	if (capacitiesRecalcTimer) {
		console.log('[CAPACITIES-SUB] Clearing previous capacities recalc timer');
		clearTimeout(capacitiesRecalcTimer);
	}

	// Schedule a recalculation after a short delay
	console.log('[CAPACITIES-SUB] Setting new capacities recalc timer for 300ms');
	capacitiesRecalcTimer = setTimeout(() => {
		console.log('[CAPACITIES-SUB] Timer fired, running capacities recalculation');
		try {
			recalculateFromCapacities();
		} catch (error) {
			console.error('[CAPACITIES-SUB] Error during recalculation:', error);
			// Even if recalculation fails, ensure capacities are persisted
			console.log('[CAPACITIES-SUB] Forcing capacities persistence after error');
			persistCapacities();
		}
		capacitiesRecalcTimer = null;
	}, 300); // 300ms debounce
});
