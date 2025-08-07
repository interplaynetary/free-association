import { addChild } from '$lib/protocol';
import type { RootNode, Node } from '$lib/schema';
import { userTree } from '$lib/state/core.svelte';
import { get } from 'svelte/store';

// Export the initialization function that populates an existing root node
export function populateWithExampleData(rootNode: RootNode): RootNode {
	// Maslow's Hierarchy of Needs - simplified root level structure
	addChild(rootNode, 'housing', '🏠', 34);
	addChild(rootNode, 'food', '🍝', 21);
	addChild(rootNode, 'art', '🎨', 13);
	addChild(rootNode, 'love', '💖', 8);
	addChild(rootNode, 'money', '💸', 5);

	console.log("Populated root node with Maslow's hierarchy of needs:", rootNode);
	return rootNode;
}

// Expose to window for debugging
if (typeof window !== 'undefined') {
	// Expose the original function
	(window as any).populateWithExampleData = populateWithExampleData;

	// Add a wrapper that uses current userTree if available
	(window as any).populateCurrentTreeWithExampleData = () => {
		const currentTree = get(userTree);
		if (!currentTree) {
			console.error('[DEBUG] No userTree available to populate with example data');
			return null;
		}
		console.log('[DEBUG] Populating current userTree with example data');
		const populatedTree = populateWithExampleData(currentTree);
		userTree.set(populatedTree);
		return populatedTree;
	};

	console.log(
		'[DEBUG] populateWithExampleData and populateCurrentTreeWithExampleData functions exposed to window'
	);
}
