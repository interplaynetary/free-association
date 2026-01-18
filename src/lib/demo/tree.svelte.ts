/**
 * Demo Tree Store - In-Memory / External View Only
 * 
 * Provides a transient store for viewing trees (e.g. Org Pages)
 * or for temporary demo state that should NOT be persisted to the user's main store.
 * 
 * NO LONGER PERSISTED to LocalStorage (UserStore handles that now).
 */

import type { RootNode } from '@playnet/free-association/schemas';
import { createRootNode, findNodeById } from '@playnet/free-association/tree';
import { applyTemplate } from '$lib/templates';
import { DEMO_ORGANIZATIONS } from './orgs';
import { writable, type Readable } from 'svelte/store';

// const DEMO_TREE_KEY = 'free-association-demo-tree'; // DEPRECATED - Managed by myRecognitionTreeStore

console.log('[TRACE] src/lib/demo/tree.svelte.ts: <module scope>');

/**
 * Demo tree store - reactive $state for in-memory viewing
 */
class DemoTreeStore {
	private tree = $state<RootNode | null>(null);
	private initialized = false;

	// ✅ Svelte 4 writable store that's updated whenever tree changes
	// This ensures proper reactivity for derived stores
	private treeStore = writable<RootNode | null>(null);

	constructor() {
		console.log('[TRACE] src/lib/demo/tree.svelte.ts: constructor (In-Memory)');
		// No loading from storage
	}

	/**
	 * Get the current tree value
	 */
	get current(): RootNode | null {
		return this.tree;
	}

	/**
	 * Set the tree (In-Memory Only)
	 */
	set(newTree: RootNode | null) {
		console.log('[TRACE] [ENTER] src/lib/demo/tree.svelte.ts: set');
		this.tree = newTree;
		// ✅ Update the writable store to trigger reactivity
		this.treeStore.set(newTree);
		console.log('[TRACE] [EXIT] src/lib/demo/tree.svelte.ts: set');
	}

	/**
	 * Initialize with SDG template (InMemory)
	 */
	initializeWithSDG() {
		console.log('[TRACE] [ENTER] src/lib/demo/tree.svelte.ts: initializeWithSDG');
		if (!this.tree) {
			console.log('[DEMO TREE] Initializing with SDG template (In-Memory)');
			// Create a demo root node without authentication
			const demoRootNode = createRootNode('demo_user', 'Viewing Demo');
			const populated = applyTemplate(demoRootNode, 'sdg');

			if (populated) {
				this.set(populated);
			}
		}
		console.log('[TRACE] [EXIT] src/lib/demo/tree.svelte.ts: initializeWithSDG');
	}

	/**
	 * Initialize with a custom tree (for org-specific routes)
	 * @param tree - Pre-configured RootNode tree structure
	 * @param force - If true, replace existing tree even if one exists
	 */
	initializeWithCustomTree(tree: RootNode, force: boolean = false) {
		console.log('[TRACE] [ENTER] src/lib/demo/tree.svelte.ts: initializeWithCustomTree', { force });
		if (!this.tree || force) {
			console.log('[DEMO TREE] Initializing with custom tree:', tree.name);
			this.set(tree);
		} else {
			console.log('[DEMO TREE] Tree already exists, skipping custom initialization (use force=true to override)');
		}
		console.log('[TRACE] [EXIT] src/lib/demo/tree.svelte.ts: initializeWithCustomTree');
	}

	/**
	 * Clear the demo tree
	 */
	clear() {
		console.log('[TRACE] [ENTER] src/lib/demo/tree.svelte.ts: clear');
		this.set(null);
		console.log('[TRACE] [EXIT] src/lib/demo/tree.svelte.ts: clear');
	}

	/**
	 * Check if demo tree exists and has content
	 */
	hasTree(): boolean {
		if (!this.tree) return false;
		// Check if tree has children - an empty tree should trigger re-initialization
		return (this.tree.children?.length ?? 0) > 0;
	}

	/**
	 * ✅ Get a Svelte 4 store that reactively tracks tree changes
	 * Returns the internal writable store as a Readable for subscription
	 */
	toStore(): Readable<RootNode | null> {
		// Return the writable store (as Readable) - it's updated whenever set() is called
		return { subscribe: this.treeStore.subscribe };
	}
}

// Create singleton instance
export const demoTreeStore = new DemoTreeStore();

