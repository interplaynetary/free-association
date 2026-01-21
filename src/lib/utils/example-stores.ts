// V5: Import from v5 stores and schemas
import type { RootNode, AvailabilitySlot, NeedSlot } from '../protocol/schemas';
import {
	myRecognitionTreeStore,
	myCapacitySlotsStore,
	myNeedSlotsStore,
	setMyCapacitySlots,
	setMyNeedSlots
} from '$lib/protocol/stores/stores.svelte';
import { get } from 'svelte/store';
import { populateWithExampleData, createExampleCapacitySlotsGenerator, createExampleNeedSlotsGenerator } from '$lib/demo/capacities';

/**
 * V5: Populate recognition tree
 */
export function populateRecognitionTree(): void {
	const currentTree = get(myRecognitionTreeStore);
	if (currentTree) {
		const populatedTree = populateWithExampleData(currentTree);
		myRecognitionTreeStore.set(populatedTree);
	}
}

/**
 * V5: Populate capacity slots (Streaming)
 */
export async function populateCapacitySlots(): Promise<void> {
	const generator = createExampleCapacitySlotsGenerator();
	let batch: AvailabilitySlot[] = [];
	const BATCH_SIZE = 50;

	for (const slot of generator) {
		batch.push(slot);
		if (batch.length >= BATCH_SIZE) {
			const current = get(myCapacitySlotsStore) || [];
			setMyCapacitySlots([...current, ...batch]);
			batch = [];
			if (typeof requestAnimationFrame !== 'undefined') {
				await new Promise(resolve => requestAnimationFrame(resolve));
			}
		}
	}

	// Add remaining
	if (batch.length > 0) {
		const current = get(myCapacitySlotsStore) || [];
		setMyCapacitySlots([...current, ...batch]);
	}
}

/**
 * V5: Populate need slots (Streaming)
 */
export async function populateNeedSlots(): Promise<void> {
	const generator = createExampleNeedSlotsGenerator();
	let batch: NeedSlot[] = [];
	const BATCH_SIZE = 50;

	for (const slot of generator) {
		batch.push(slot);
		if (batch.length >= BATCH_SIZE) {
			const current = get(myNeedSlotsStore) || [];
			setMyNeedSlots([...current, ...batch]);
			batch = [];
			if (typeof requestAnimationFrame !== 'undefined') {
				await new Promise(resolve => requestAnimationFrame(resolve));
			}
		}
	}

	// Add remaining
	if (batch.length > 0) {
		const current = get(myNeedSlotsStore) || [];
		setMyNeedSlots([...current, ...batch]);
	}
}

/**
 * V5: Populate both tree and capacity/need slots with example data
 */
export function populateWithFullExampleData(): void {
	console.log('[EXAMPLE] Populating with full SDG example data (v5)...');
	populateRecognitionTree();
	populateCapacitySlots();
	populateNeedSlots();

	console.log(
		`[EXAMPLE] Full example data populated (v5)`
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
