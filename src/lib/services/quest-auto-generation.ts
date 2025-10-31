/**
 * Automatic Quest Generation Service
 * 
 * Background service that automatically generates quests on a schedule
 * or when significant changes are detected in user data.
 */

import { get } from 'svelte/store';
import { myRecognitionTreeStore, myCommitmentStore } from '$lib/protocol/stores.svelte';
import { generateQuests } from './quest-service';

export interface AutoGenerationSettings {
	enabled: boolean;
	schedule: 'daily' | 'weekly' | 'manual';
	lastGeneratedAt?: number;
	lastTreeHash?: string;
	lastCommitmentHash?: string;
}

const STORAGE_KEY = 'quest_auto_generation_settings';
const CHECK_INTERVAL = 1000 * 60 * 60; // Check every hour
const DAILY_MS = 1000 * 60 * 60 * 24;
const WEEKLY_MS = DAILY_MS * 7;

let intervalId: ReturnType<typeof setInterval> | null = null;

/**
 * Get auto-generation settings from localStorage
 */
export function getAutoGenerationSettings(): AutoGenerationSettings {
	if (typeof window === 'undefined') {
		return { enabled: false, schedule: 'weekly' };
	}
	
	try {
		const stored = localStorage.getItem(STORAGE_KEY);
		if (stored) {
			return JSON.parse(stored);
		}
	} catch (err) {
		console.error('[QUEST-AUTO] Failed to load settings:', err);
	}
	
	return { enabled: false, schedule: 'weekly' };
}

/**
 * Save auto-generation settings to localStorage
 */
export function saveAutoGenerationSettings(settings: AutoGenerationSettings): void {
	if (typeof window === 'undefined') return;
	
	try {
		localStorage.setItem(STORAGE_KEY, JSON.stringify(settings));
		console.log('[QUEST-AUTO] Settings saved:', settings);
	} catch (err) {
		console.error('[QUEST-AUTO] Failed to save settings:', err);
	}
}

/**
 * Simple hash function for detecting changes
 */
function simpleHash(obj: any): string {
	return JSON.stringify(obj).split('').reduce((a, b) => {
		a = ((a << 5) - a) + b.charCodeAt(0);
		return a & a;
	}, 0).toString(36);
}

/**
 * Check if significant changes have occurred in user data
 */
function hasSignificantChanges(): boolean {
	const settings = getAutoGenerationSettings();
	
	const tree = get(myRecognitionTreeStore);
	const commitment = get(myCommitmentStore);
	
	const currentTreeHash = tree ? simpleHash(tree) : '';
	const currentCommitmentHash = commitment ? simpleHash(commitment) : '';
	
	// If we have no previous hashes, consider it changed
	if (!settings.lastTreeHash && !settings.lastCommitmentHash) {
		return true;
	}
	
	// Check if tree or commitment has changed
	const treeChanged = settings.lastTreeHash !== currentTreeHash;
	const commitmentChanged = settings.lastCommitmentHash !== currentCommitmentHash;
	
	if (treeChanged || commitmentChanged) {
		console.log('[QUEST-AUTO] Significant changes detected:', {
			treeChanged,
			commitmentChanged
		});
		return true;
	}
	
	return false;
}

/**
 * Check if it's time to generate quests based on schedule
 */
function shouldGenerateBySchedule(): boolean {
	const settings = getAutoGenerationSettings();
	
	if (!settings.enabled) return false;
	if (settings.schedule === 'manual') return false;
	if (!settings.lastGeneratedAt) return true;
	
	const now = Date.now();
	const timeSinceLastGen = now - settings.lastGeneratedAt;
	
	if (settings.schedule === 'daily' && timeSinceLastGen >= DAILY_MS) {
		return true;
	}
	
	if (settings.schedule === 'weekly' && timeSinceLastGen >= WEEKLY_MS) {
		return true;
	}
	
	return false;
}

/**
 * Attempt automatic quest generation
 */
async function attemptAutoGeneration(): Promise<void> {
	const settings = getAutoGenerationSettings();
	
	if (!settings.enabled) return;
	
	// Check schedule
	const shouldGenBySchedule = shouldGenerateBySchedule();
	
	// Check for significant changes
	const hasChanges = hasSignificantChanges();
	
	if (!shouldGenBySchedule && !hasChanges) {
		return;
	}
	
	console.log('[QUEST-AUTO] Auto-generating quests...', {
		bySchedule: shouldGenBySchedule,
		byChanges: hasChanges
	});
	
	try {
		const result = await generateQuests({
			maxQuests: 5,
			includeGeolocation: true,
			includePeerQuests: true
		});
		
		if (result.success) {
			console.log('[QUEST-AUTO] ✅ Auto-generated', result.quests?.length || 0, 'quests');
			
			// Update settings
			const tree = get(myRecognitionTreeStore);
			const commitment = get(myCommitmentStore);
			
			saveAutoGenerationSettings({
				...settings,
				lastGeneratedAt: Date.now(),
				lastTreeHash: tree ? simpleHash(tree) : '',
				lastCommitmentHash: commitment ? simpleHash(commitment) : ''
			});
		} else {
			console.warn('[QUEST-AUTO] Auto-generation failed:', result.error);
		}
	} catch (err) {
		console.error('[QUEST-AUTO] Error during auto-generation:', err);
	}
}

/**
 * Start automatic quest generation service
 */
export function startAutoGeneration(): void {
	if (typeof window === 'undefined') return;
	if (intervalId !== null) {
		console.log('[QUEST-AUTO] Service already running');
		return;
	}
	
	const settings = getAutoGenerationSettings();
	
	if (!settings.enabled) {
		console.log('[QUEST-AUTO] Auto-generation disabled');
		return;
	}
	
	console.log('[QUEST-AUTO] Starting service with schedule:', settings.schedule);
	
	// Check immediately
	attemptAutoGeneration();
	
	// Then check periodically
	intervalId = setInterval(() => {
		attemptAutoGeneration();
	}, CHECK_INTERVAL);
	
	console.log('[QUEST-AUTO] ✅ Service started');
}

/**
 * Stop automatic quest generation service
 */
export function stopAutoGeneration(): void {
	if (intervalId !== null) {
		clearInterval(intervalId);
		intervalId = null;
		console.log('[QUEST-AUTO] Service stopped');
	}
}

/**
 * Enable auto-generation
 */
export function enableAutoGeneration(schedule: 'daily' | 'weekly' = 'weekly'): void {
	const settings = getAutoGenerationSettings();
	saveAutoGenerationSettings({
		...settings,
		enabled: true,
		schedule
	});
	startAutoGeneration();
}

/**
 * Disable auto-generation
 */
export function disableAutoGeneration(): void {
	const settings = getAutoGenerationSettings();
	saveAutoGenerationSettings({
		...settings,
		enabled: false
	});
	stopAutoGeneration();
}

/**
 * Manually trigger quest generation (updates last generated timestamp)
 */
export async function manuallyGenerateQuests(): Promise<void> {
	const result = await generateQuests({
		maxQuests: 5,
		includeGeolocation: true,
		includePeerQuests: true
	});
	
	if (result.success) {
		// Update last generation timestamp
		const settings = getAutoGenerationSettings();
		const tree = get(myRecognitionTreeStore);
		const commitment = get(myCommitmentStore);
		
		saveAutoGenerationSettings({
			...settings,
			lastGeneratedAt: Date.now(),
			lastTreeHash: tree ? simpleHash(tree) : '',
			lastCommitmentHash: commitment ? simpleHash(commitment) : ''
		});
	}
}

// Initialize on load if enabled
if (typeof window !== 'undefined') {
	const settings = getAutoGenerationSettings();
	if (settings.enabled) {
		// Delay start slightly to let app initialize
		setTimeout(() => startAutoGeneration(), 5000);
	}
	
	// Expose to window for debugging
	(window as any).questAutoGen = {
		start: startAutoGeneration,
		stop: stopAutoGeneration,
		enable: enableAutoGeneration,
		disable: disableAutoGeneration,
		manual: manuallyGenerateQuests,
		settings: getAutoGenerationSettings,
		checkChanges: hasSignificantChanges,
		checkSchedule: shouldGenerateBySchedule
	};
	
	console.log('[QUEST-AUTO] 🤖 Auto-generation utilities available at window.questAutoGen');
}

