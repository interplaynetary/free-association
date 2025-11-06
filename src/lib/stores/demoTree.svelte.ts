/**
 * Demo Tree Store - Local Storage Only
 * 
 * Provides a tree experience for unauthenticated users without requiring login.
 * Data is stored in browser's localStorage and not synced to Holster.
 */

import type { RootNode } from '$lib/protocol/schemas';
import { createRootNode } from '$lib/protocol/tree';
import { applyTemplate } from '$lib/templates';

const DEMO_TREE_KEY = 'free-association-demo-tree';

/**
 * Demo tree store - reactive $state for local-only tree
 */
class DemoTreeStore {
	private tree = $state<RootNode | null>(null);
	private initialized = false;

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
	 * Set the tree and persist to localStorage
	 */
	set(newTree: RootNode | null) {
		this.tree = newTree;
		if (typeof window !== 'undefined') {
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
			const demoRootNode = createRootNode('demo_user', 'Demo User');
			const populated = applyTemplate(demoRootNode, 'sdg');
			if (populated) {
				this.set(populated);
			}
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
}

// Create singleton instance
export const demoTreeStore = new DemoTreeStore();

