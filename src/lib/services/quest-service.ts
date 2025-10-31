/**
 * Quest Generation Service
 * 
 * Client-side service for orchestrating quest generation:
 * - Gathers user data (tree, capacities, needs, location)
 * - Fetches peer quests if opt-in enabled
 * - Calls API endpoint
 * - Stores results in Holster
 */

import { get } from 'svelte/store';
import { 
	myRecognitionTreeStore,
	myCommitmentStore
} from '$lib/commons/v5/stores.svelte';
import {
	myMainQuestsStore,
	mySideQuestsStore,
	myArchivedQuestsStore,
	questSharingSettingsStore,
	networkQuestsStore,
	updateSharedQuests
} from '$lib/commons/v5/quest-stores.svelte';
import { aggregateLocations } from '$lib/utils/location-service';
import type { Quest, QuestType } from '$lib/commons/v5/quest-schemas';
import type { AvailabilitySlot, NeedSlot } from '$lib/commons/v5/schemas';
import { seed as itcSeed, event as itcEvent } from '$lib/commons/utils/itc';

export interface QuestGenerationOptions {
	maxQuests?: number;
	preferredTypes?: QuestType[];
	includeGeolocation?: boolean;
	includePeerQuests?: boolean;
}

export interface QuestGenerationResult {
	success: boolean;
	quests?: Quest[];
	mainQuests?: Quest[];
	sideQuests?: Quest[];
	error?: string;
}

/**
 * Generate quests for current user
 */
export async function generateQuests(
	options: QuestGenerationOptions = {}
): Promise<QuestGenerationResult> {
	try {
		console.log('[QUEST-SERVICE] Starting quest generation...');
		
		// 1. Gather recognition tree
		const tree = get(myRecognitionTreeStore);
		if (!tree) {
			return {
				success: false,
				error: 'No recognition tree found. Please build your value tree first.'
			};
		}
		
		// 2. Gather commitments (capacities and needs)
		const commitment = get(myCommitmentStore);
		const capacities: AvailabilitySlot[] = (commitment as any)?.availability_slots || [];
		const needs: NeedSlot[] = (commitment as any)?.need_slots || [];
		
		console.log('[QUEST-SERVICE] Gathered data:', {
			tree: tree.name,
			capacities: capacities.length,
			needs: needs.length
		});
		
		// 3. Aggregate locations
		const locations = await aggregateLocations(
			capacities,
			needs,
			options.includeGeolocation !== false
		);
		
		console.log('[QUEST-SERVICE] Aggregated locations:', locations.length);
		
		// 4. Gather peer quests if enabled
		let peerQuests: Quest[] = [];
		if (options.includePeerQuests !== false) {
			const sharingSettings = get(questSharingSettingsStore);
			if (sharingSettings?.enabled) {
				const networkQuests = get(networkQuestsStore);
				// Flatten all peer quests
				peerQuests = Object.values(networkQuests).flat();
				console.log('[QUEST-SERVICE] Including peer quests:', peerQuests.length);
			}
		}
		
		// 5. Call API endpoint
		// Note: In a real production app, you'd get auth credentials from your auth system
		// For now, the endpoint will require valid authentication (API key or JWT)
		const response = await fetch('/api/llm/quest-generation', {
			method: 'POST',
			headers: { 
				'Content-Type': 'application/json'
				// Authentication headers should be added by your auth system
				// or set in cookies/localStorage depending on your setup
			},
			credentials: 'include', // Include cookies for authentication
			body: JSON.stringify({
				recognitionTree: tree,
				capacities,
				needs,
				locations,
				peerQuests,
				maxQuests: options.maxQuests || 5,
				preferredTypes: options.preferredTypes
			})
		});
		
		if (!response.ok) {
			const errorData = await response.json().catch(() => ({ message: 'Unknown error' }));
			throw new Error(errorData.message || 'Quest generation failed');
		}
		
		const result = await response.json();
		
		if (!result.success || !result.quests) {
			throw new Error('Invalid response from quest generation API');
		}
		
		console.log('[QUEST-SERVICE] ✅ Generated', result.quests.length, 'quests');
		
		// 6. Separate into main and side quests
		const mainQuests = result.quests.filter((q: Quest) => q.type === 'main');
		const sideQuests = result.quests.filter((q: Quest) => q.type === 'side');
		
		// 7. Store in Holster
		await storeQuests(mainQuests, sideQuests);
		
		// 8. Update shared quests if sharing is enabled
		updateSharedQuests();
		
		return {
			success: true,
			quests: result.quests,
			mainQuests,
			sideQuests
		};
		
	} catch (err: any) {
		console.error('[QUEST-SERVICE] Error generating quests:', err);
		return {
			success: false,
			error: err.message || 'Quest generation failed'
		};
	}
}

/**
 * Store quests in appropriate Holster stores
 */
async function storeQuests(mainQuests: Quest[], sideQuests: Quest[]): Promise<void> {
	// Get existing quests
	const existingMain = get(myMainQuestsStore) || [];
	const existingSide = get(mySideQuestsStore) || [];
	
	// Add ITC stamps to new quests
	const stampedMain = mainQuests.map(q => ({
		...q,
		stamp: q.stamp || itcSeed(),
		_updatedAt: Date.now()
	}));
	
	const stampedSide = sideQuests.map(q => ({
		...q,
		stamp: q.stamp || itcSeed(),
		_updatedAt: Date.now()
	}));
	
	// Merge with existing (keep existing if not replaced)
	const mergedMain = [...existingMain, ...stampedMain];
	const mergedSide = [...existingSide, ...stampedSide];
	
	// Store
	myMainQuestsStore.set(mergedMain);
	mySideQuestsStore.set(mergedSide);
	
	console.log('[QUEST-SERVICE] ✅ Stored quests:', {
		main: stampedMain.length,
		side: stampedSide.length
	});
}

/**
 * Complete a quest
 */
export function completeQuest(questId: string): void {
	// Find quest in main or side
	const main = get(myMainQuestsStore) || [];
	const side = get(mySideQuestsStore) || [];
	
	const questInMain = main.find(q => q.id === questId);
	const questInSide = side.find(q => q.id === questId);
	
	if (questInMain) {
		// Update completion status
		const updated = main.map(q => 
			q.id === questId 
				? { ...q, completion: { completed: true, completedAt: Date.now() } }
				: q
		);
		myMainQuestsStore.set(updated);
	} else if (questInSide) {
		const updated = side.map(q => 
			q.id === questId 
				? { ...q, completion: { completed: true, completedAt: Date.now() } }
				: q
		);
		mySideQuestsStore.set(updated);
	}
	
	// Update shared quests
	updateSharedQuests();
	
	console.log('[QUEST-SERVICE] ✅ Completed quest:', questId);
}

/**
 * Archive a quest
 */
export function archiveQuest(questId: string): void {
	const main = get(myMainQuestsStore) || [];
	const side = get(mySideQuestsStore) || [];
	
	let questToArchive: Quest | undefined;
	
	// Find and remove from main
	const questInMain = main.find(q => q.id === questId);
	if (questInMain) {
		questToArchive = { ...questInMain, type: 'archived' };
		myMainQuestsStore.set(main.filter(q => q.id !== questId));
	}
	
	// Find and remove from side
	const questInSide = side.find(q => q.id === questId);
	if (questInSide) {
		questToArchive = { ...questInSide, type: 'archived' };
		mySideQuestsStore.set(side.filter(q => q.id !== questId));
	}
	
	if (questToArchive) {
		// Add to archived
		const archived = get(myArchivedQuestsStore) || [];
		myArchivedQuestsStore.set([...archived, questToArchive]);
		
		// Update shared quests
		updateSharedQuests();
		
		console.log('[QUEST-SERVICE] ✅ Archived quest:', questId);
	}
}

/**
 * Delete a quest permanently
 */
export function deleteQuest(questId: string): void {
	const main = get(myMainQuestsStore) || [];
	const side = get(mySideQuestsStore) || [];
	
	myMainQuestsStore.set(main.filter((q: Quest) => q.id !== questId));
	mySideQuestsStore.set(side.filter((q: Quest) => q.id !== questId));
	
	// Also remove from archived
	const archived = get(myArchivedQuestsStore) || [];
	myArchivedQuestsStore.set(archived.filter((q: Quest) => q.id !== questId));
	
	// Update shared quests
	updateSharedQuests();
	
	console.log('[QUEST-SERVICE] ✅ Deleted quest:', questId);
}

/**
 * Update quest progress
 */
export function updateQuestProgress(questId: string, progress: number): void {
	const main = get(myMainQuestsStore) || [];
	const side = get(mySideQuestsStore) || [];
	
	const questInMain = main.find(q => q.id === questId);
	if (questInMain) {
		const updated = main.map(q => 
			q.id === questId 
				? { ...q, completion: { ...q.completion, progress, completed: progress >= 1 } }
				: q
		);
		myMainQuestsStore.set(updated);
	}
	
	const questInSide = side.find(q => q.id === questId);
	if (questInSide) {
		const updated = side.map(q => 
			q.id === questId 
				? { ...q, completion: { ...q.completion, progress, completed: progress >= 1 } }
				: q
		);
		mySideQuestsStore.set(updated);
	}
	
	console.log('[QUEST-SERVICE] Updated quest progress:', questId, progress);
}

