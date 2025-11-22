/**
 * Demo Tree Store - Local Storage Only
 * 
 * Provides a tree experience for unauthenticated users without requiring login.
 * Data is stored in browser's localStorage and not synced to Holster.
 */

import type { RootNode } from '$lib/protocol/schemas';
import { createRootNode } from '$lib/protocol/tree';
import { applyTemplate } from '$lib/templates';
import { writable, type Readable } from 'svelte/store';

const DEMO_TREE_KEY = 'free-association-demo-tree';

/**
 * Demo tree store - reactive $state for local-only tree
 */
class DemoTreeStore {
	private tree = $state<RootNode | null>(null);
	private initialized = false;
	
	// ✅ Svelte 4 writable store that's updated whenever tree changes
	// This ensures proper reactivity for derived stores
	private treeStore = writable<RootNode | null>(null);

	constructor() {
		// Load from localStorage on creation (browser only)
		if (typeof window !== 'undefined') {
			this.loadFromStorage();
		}
	}

	/**
	 * Get the current tree value
	 */
	get current(): RootNode | null {
		return this.tree;
	}

	/**
	 * Set the tree and optionally persist to localStorage
	 * @param persist - Whether to save to localStorage (default: true)
	 */
	set(newTree: RootNode | null, persist: boolean = true) {
		this.tree = newTree;
		// ✅ Update the writable store to trigger reactivity
		this.treeStore.set(newTree);
		
		if (typeof window !== 'undefined' && persist) {
			if (newTree) {
				localStorage.setItem(DEMO_TREE_KEY, JSON.stringify(newTree));
			} else {
				localStorage.removeItem(DEMO_TREE_KEY);
			}
		}
	}

	/**
	 * Load tree from localStorage
	 */
	private loadFromStorage() {
		if (this.initialized) return;
		
		try {
			const stored = localStorage.getItem(DEMO_TREE_KEY);
			if (stored) {
				const parsed = JSON.parse(stored);
				// Validate that it's a proper tree with children
				if (parsed && typeof parsed === 'object' && parsed.type === 'RootNode') {
					// Check if tree has valid children array
					if (parsed.children && Array.isArray(parsed.children) && parsed.children.length > 0) {
						this.tree = parsed;
						// ✅ Update the writable store
						this.treeStore.set(parsed);
						console.log('[DEMO TREE] Loaded from localStorage with', parsed.children.length, 'children');
					} else {
						// Tree exists but is empty - clear it
						console.log('[DEMO TREE] Found empty tree in localStorage - clearing');
						localStorage.removeItem(DEMO_TREE_KEY);
					}
				}
			}
		} catch (error) {
			console.error('[DEMO TREE] Failed to load from localStorage:', error);
			// Clear corrupted data
			localStorage.removeItem(DEMO_TREE_KEY);
		}
		
		this.initialized = true;
	}

	/**
	 * Initialize with SDG template if tree doesn't exist
	 */
	initializeWithSDG() {
		if (!this.tree) {
			console.log('[DEMO TREE] Initializing with SDG template');
			// Create a demo root node without authentication
			const demoRootNode = createRootNode('demo_user', 'Log In');
			const populated = applyTemplate(demoRootNode, 'sdg');
			if (populated) {
				this.set(populated);
			}
		}
	}

	/**
	 * Initialize with a custom tree (for org-specific routes)
	 * @param tree - Pre-configured RootNode tree structure
	 * @param force - If true, replace existing tree even if one exists
	 * @param persist - If true, save to localStorage (default: false for org routes)
	 */
	initializeWithCustomTree(tree: RootNode, force: boolean = false, persist: boolean = false) {
		if (!this.tree || force) {
			console.log('[DEMO TREE] Initializing with custom tree:', tree.name, '| persist:', persist);
			this.set(tree, persist);
		} else {
			console.log('[DEMO TREE] Tree already exists, skipping custom initialization (use force=true to override)');
		}
	}

	/**
	 * Clear the demo tree
	 */
	clear() {
		this.set(null);
	}

	/**
	 * Check if demo tree exists and has content
	 */
	hasTree(): boolean {
		if (!this.tree) return false;
		// Check if tree has children - an empty tree should trigger re-initialization
		return this.tree.children && this.tree.children.length > 0;
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

