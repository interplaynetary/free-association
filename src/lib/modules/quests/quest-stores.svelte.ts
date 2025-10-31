/**
 * Quest Stores
 * Holster-backed P2P stores for quest management
 */

import { writable, derived, get } from 'svelte/store';
import { createStore } from '$lib/utils/primitives/store.svelte';
import { holster } from '$lib/network/holster.svelte';
import {
	QuestCollectionSchema,
	QuestSharingSettingsSchema,
	type Quest,
	type QuestCollection,
	type QuestSharingSettings
} from './quest-schemas';
import { myRecognitionWeights } from '$lib/protocol/stores.svelte';

/**
 * My Main Quests Store
 * Stores primary, transformative quests
 */
export const myMainQuestsStore = createStore({
	holsterPath: 'quests/main',
	schema: QuestCollectionSchema,
	persistDebounce: 300
});

/**
 * My Side Quests Store
 * Stores smaller, achievable quests
 */
export const mySideQuestsStore = createStore({
	holsterPath: 'quests/side',
	schema: QuestCollectionSchema,
	persistDebounce: 300
});

/**
 * My Archived Quests Store
 * Stores completed or abandoned quests
 */
export const myArchivedQuestsStore = createStore({
	holsterPath: 'quests/archived',
	schema: QuestCollectionSchema,
	persistDebounce: 300
});

/**
 * Quest Sharing Settings Store
 * Controls whether user shares quests with network
 */
export const questSharingSettingsStore = createStore({
	holsterPath: 'quests/settings/sharing',
	schema: QuestSharingSettingsSchema,
	persistDebounce: 500
});

/**
 * My Shared Quests Store
 * Quests that are shared with the network (read by others)
 */
export const mySharedQuestsStore = createStore({
	holsterPath: 'quests/shared',
	schema: QuestCollectionSchema,
	persistDebounce: 300
});

/**
 * Network Quests Store
 * Quests from other players (P2P subscriptions)
 * Structured as: { [pubKey]: Quest[] }
 */
export const networkQuestsStore = writable<Record<string, QuestCollection>>({});

/**
 * Get all my active quests (main + side)
 */
export const myActiveQuests = derived(
	[myMainQuestsStore, mySideQuestsStore],
	([$main, $side]) => {
		const mainQuests = $main || [];
		const sideQuests = $side || [];
		return [...mainQuests, ...sideQuests];
	}
);

/**
 * Subscribe to a peer's shared quests
 */
export function subscribeToQuestsByPub(pub: string) {
	if (!pub) return;
	
	console.log(`[QUEST-STORE] Subscribing to quests from ${pub.slice(0, 8)}...`);
	
	const path = `${pub}/quests/shared`;
	
	// Subscribe using Holster API
	holster.get(path).on((data: any) => {
		if (!data) return;
		
		try {
			// Parse and validate
			const parsed = JSON.parse(data);
			const validated = QuestCollectionSchema.safeParse(parsed);
			
			if (validated.success) {
				networkQuestsStore.update(store => ({
					...store,
					[pub]: validated.data
				}));
				console.log(`[QUEST-STORE] ✅ Loaded ${validated.data.length} quests from ${pub.slice(0, 8)}`);
			} else {
				console.warn(`[QUEST-STORE] Invalid quest data from ${pub.slice(0, 8)}:`, validated.error);
			}
		} catch (err) {
			console.error(`[QUEST-STORE] Failed to parse quests from ${pub.slice(0, 8)}:`, err);
		}
	}, true); // Get immediately
}

/**
 * Initialize quest sharing subscriptions
 * Subscribes to quests from mutual contributors
 */
export function initializeQuestSharing() {
	// Get current sharing settings
	const settings = get(questSharingSettingsStore);
	
	if (!settings || !settings.enabled) {
		console.log('[QUEST-STORE] Quest sharing disabled');
		return;
	}
	
	// Get contributors to subscribe to
	const contributors = get(myRecognitionWeights);
	const contributorPubs = Object.keys(contributors || {});
	
	console.log(`[QUEST-STORE] Initializing quest sharing with ${contributorPubs.length} contributors`);
	
	// Subscribe to each contributor's quests
	for (const pub of contributorPubs) {
		subscribeToQuestsByPub(pub);
	}
}

/**
 * Update shared quests based on settings
 * Called when user changes sharing settings or completes quests
 */
export function updateSharedQuests() {
	const settings = get(questSharingSettingsStore);
	
	if (!settings || !settings.enabled) {
		// Clear shared quests if sharing is disabled
		mySharedQuestsStore.set([]);
		return;
	}
	
	// Get active quests
	const main = get(myMainQuestsStore) || [];
	const side = get(mySideQuestsStore) || [];
	const allQuests = [...main, ...side];
	
	// Filter out completed quests and those marked as private
	const sharedQuests = allQuests.filter(q => !q.completion?.completed);
	
	// Update shared store
	mySharedQuestsStore.set(sharedQuests);
	
	console.log(`[QUEST-STORE] Updated shared quests: ${sharedQuests.length} quests`);
}

// Expose to window for debugging
if (typeof window !== 'undefined') {
	(window as any).initializeQuestSharing = initializeQuestSharing;
	(window as any).updateSharedQuests = updateSharedQuests;
}

