/*
import { get } from 'svelte/store';
import {
	contributors,
	mutualContributors,
	recognitionCache,
	mutualRecognition,
	providerShares,
	userSogf
} from './core.svelte';
import { subscribeToContributorSOGF } from './network.svelte';

*/

/**
 * Debug function to log current state of all recognition-related stores

export function debugRecognitionState() {
	console.log('=== RECOGNITION DEBUG STATE ===');

	const currentContributors = get(contributors);
	const currentMutual = get(mutualContributors);
	const currentCache = get(recognitionCache);
	const currentMutualRecognition = get(mutualRecognition);
	const currentProviderShares = get(providerShares);
	const currentSogf = get(userSogf);

	console.log('Contributors:', currentContributors);
	console.log('Mutual Contributors:', currentMutual);
	console.log('User SOGF:', currentSogf);

	// Detailed recognition cache breakdown
	console.log('=== DETAILED RECOGNITION CACHE ===');
	Object.entries(currentCache).forEach(([contributorId, entry]) => {
		console.log(`${contributorId}:`, {
			ourShare: entry.ourShare,
			theirShare: entry.theirShare,
			mutual: Math.min(entry.ourShare, entry.theirShare),
			timestamp: new Date(entry.timestamp).toISOString()
		});
	});

	console.log('Mutual Recognition:', currentMutualRecognition);
	console.log('Provider Shares:', currentProviderShares);
	console.log('=== END DEBUG ===');
}

export function debugTriggerSubscriptions() {
	console.log('[DEBUG] Manually triggering subscriptions for all contributors');
	const allContributors = get(contributors);

	allContributors.forEach((contributorId) => {
		console.log(`[DEBUG] Setting up subscription for: ${contributorId}`);
		subscribeToContributorSOGF(contributorId);
	});

	console.log(`[DEBUG] Set up ${allContributors.length} subscriptions`);
}

// Expose debug functions to global scope for console access
if (typeof window !== 'undefined') {
	(window as any).debugRecognition = debugRecognitionState;
	(window as any).debugTriggerSubscriptions = debugTriggerSubscriptions;
}

*/