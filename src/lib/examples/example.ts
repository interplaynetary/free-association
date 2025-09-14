import { addChild, findNodeById } from '$lib/protocol';
import type { RootNode, Node } from '$lib/schema';
import { userTree } from '$lib/state/core.svelte';
import { get } from 'svelte/store';

// Export the initialization function that populates an existing root node
export function populateWithExampleData(rootNode: RootNode): RootNode {
	// Create the main Playnet node
	addChild(rootNode, 'playnet', 'Playnet 🐟', 100);

	// Find the playnet node to add children to it
	const playnetNode = findNodeById(rootNode, 'playnet');
	if (playnetNode) {
		// Add main Playnet categories
		addChild(playnetNode, 'development', 'Development', 25);
		addChild(playnetNode, 'communications', 'Communications', 20);
		addChild(playnetNode, 'playlabs', 'Playlabs', 35);
		addChild(playnetNode, 'free-association', 'Free-Association', 20);

		// Find and populate Playlabs subcategories
		const playlabsNode = findNodeById(rootNode, 'playlabs');
		if (playlabsNode) {
			addChild(playlabsNode, 'facilitation', 'Facilitation', 20);
			addChild(playlabsNode, 'music', 'Music', 15);
			addChild(playlabsNode, 'food', 'Food', 15);
			addChild(playlabsNode, 'documentation', 'Documentation', 10);
			addChild(playlabsNode, 'invitation', 'Invitation', 10);
			addChild(playlabsNode, 'materials', 'Materials', 30);

			// Find and populate Materials subcategories
			const materialsNode = findNodeById(rootNode, 'materials');
			if (materialsNode) {
				addChild(materialsNode, 'masking-tape', 'Masking Tape', 10);
				addChild(materialsNode, 'cards', 'Cards', 35);
				addChild(materialsNode, 'markers', 'Markers', 25);
			}
		}

		// Find and populate Free-Association subcategories
		const freeAssocNode = findNodeById(rootNode, 'free-association');
		if (freeAssocNode) {
			addChild(freeAssocNode, 'fa-development', 'Development', 60);
			addChild(freeAssocNode, 'fa-communications', 'Communications', 40);
		}
	}

	console.log("Populated root node with nested Playnet structure:", rootNode);
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
