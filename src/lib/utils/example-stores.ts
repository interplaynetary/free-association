
// V5: Import from v5 stores and schemas
import type { RootNode, AvailabilitySlot } from '@playnet/free-association/schemas';
import { myRecognitionTreeStore, myCommitmentStore, myCapacitySlotsStore } from '$lib/protocol/stores/stores.svelte';
import { get } from 'svelte/store';
import { populateWithExampleData, createExampleCapacitySlots } from './example';

/**
 * V5: Populate both tree and capacity slots with example data
 */
export function populateWithFullExampleData(): void {
	console.log('[EXAMPLE] Populating with full SDG example data (v5)...');

	// V5: Populate recognition tree
	const currentTree = get(myRecognitionTreeStore);
	if (currentTree) {
		const populatedTree = populateWithExampleData(currentTree);
		myRecognitionTreeStore.set(populatedTree);
	}

	// V5: Populate capacity slots
	const exampleSlots = createExampleCapacitySlots();
	const currentSlots = get(myCapacitySlotsStore) || [];
	const newSlots: AvailabilitySlot[] = [...currentSlots, ...exampleSlots];

	myCapacitySlotsStore.set(newSlots);

	console.log(
		`[EXAMPLE] Full example data populated (v5): tree + ${exampleSlots.length} capacity slots`
	);
}

// V5: Expose to window for debugging
// Delay initialization to ensure all stores are initialized
if (typeof window !== 'undefined') {
	setTimeout(() => {
		(window as any).populateWithFullExampleData = populateWithFullExampleData;

		// V5: Add wrapper that uses current recognition tree
		(window as any).populateCurrentTreeWithExampleData = () => {
			const currentTree = get(myRecognitionTreeStore);
			if (!currentTree) {
				console.error('[DEBUG] No myRecognitionTreeStore available to populate with example data');
				return null;
			}
			console.log('[DEBUG] Populating current recognition tree with example data (v5)');
			const populatedTree = populateWithExampleData(currentTree);
			myRecognitionTreeStore.set(populatedTree);
			return populatedTree;
		};

		// V5: Add wrapper to populate everything (no userPubKey needed!)
		(window as any).populateEverything = () => {
			console.log(`[DEBUG] Populating v5 data (tree + capacity slots)...`);
			populateWithFullExampleData();
		};

		console.log('[DEBUG] V5 Example Store functions exposed to window');
	}, 1000); // Slightly longer delay to let core example functions register first
}
