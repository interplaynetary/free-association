import { get } from 'svelte/store';
import { gun, user, persistRecognitionCache } from './gun.svelte';
import { contributors, mutualContributors, recognitionCache } from './core.svelte';
import { updateRecognitionCache } from './calculations.svelte';

/**
 * Update the recognition cache with a contributor's share for us from network
 * @param contributorId The ID of the contributor
 * @param theirShare Share they assign to us in their SOGF
 */
export function updateTheirShareFromNetwork(contributorId: string, theirShare: number) {
	console.log(`[NETWORK] Received share from ${contributorId}: ${theirShare.toFixed(4)}`);

	// Get current cache entry
	const cache = get(recognitionCache);
	const existing = cache[contributorId];

	console.log(`[NETWORK] Existing cache entry for ${contributorId}:`, existing);

	// Update the cache immediately with new theirShare
	recognitionCache.update((cache) => {
		if (existing) {
			// Update only theirShare in existing entry
			console.log(`[NETWORK] Updating existing entry for ${contributorId}`);
			cache[contributorId] = {
				...cache[contributorId],
				theirShare,
				timestamp: Date.now()
			};
		} else {
			// Create new entry with default ourShare of 0
			console.log(`[NETWORK] Creating new entry for ${contributorId} with ourShare=0`);
			cache[contributorId] = {
				ourShare: 0, // We don't know our share yet
				theirShare,
				timestamp: Date.now()
			};
		}

		console.log(`[NETWORK] Updated cache entry for ${contributorId}:`, cache[contributorId]);
		return cache;
	});

	// Persist the updated cache
	persistRecognitionCache();

	// Log the updated cache and force reactivity check
	const updatedCache = get(recognitionCache);

	console.log(`[NETWORK] Cache after network update from ${contributorId}:`, updatedCache);
}

/**
 * Set up network subscriptions for contributor's SOGF
 * @param contributorId The ID of the contributor to subscribe to
 */
export function subscribeToContributorSOGF(contributorId: string) {
	console.log(`[NETWORK] Setting up subscription to ${contributorId}'s SOGF`);

	// FIXED: Use the correct Gun pattern for accessing other users' protected data
	// The guide shows user data is stored under gun.get(`~${public_key}`)
	const contributorSogf = gun.get(`~${contributorId}`).get('sogf');

	// Add timeout protection as recommended by the Gun guide
	// to prevent hanging when data doesn't exist
	let hasReceived = false;
	const timeout = setTimeout(() => {
		if (!hasReceived) {
			console.log(`[NETWORK] Timeout waiting for SOGF from ${contributorId}`);
		}
	}, 10000); // 10 second timeout

	// Subscribe to changes
	contributorSogf.on((sogfData: any) => {
		hasReceived = true;
		clearTimeout(timeout);

		if (!sogfData) return;

		console.log(`[NETWORK] Received SOGF update from ${contributorId}`);

		// Get our user ID
		const ourId = user.is?.pub;
		if (!ourId) return;

		// Extract our share from their SOGF
		const theirShare = sogfData[ourId] || 0;

		// Update our cache with their share for us
		updateTheirShareFromNetwork(contributorId, theirShare);
	});
}

/**
 * Set up subscriptions for all mutual contributors
 */
export function subscribeToAllMutualContributors() {
	console.log('[NETWORK] Setting up subscriptions for all mutual contributors');

	const currentMutualContributors = get(mutualContributors);

	currentMutualContributors.forEach((contributorId) => {
		subscribeToContributorSOGF(contributorId);
	});

	console.log(`[NETWORK] Set up ${currentMutualContributors.length} subscriptions`);
}

// Watch for changes to contributors and subscribe to get their SOGF data
contributors.subscribe((allContributors) => {
	if (!allContributors.length) return;

	// Only run this if we're authenticated
	if (!user.is?.pub) return;

	console.log(
		`[NETWORK] Contributors changed, now have ${allContributors.length}, setting up subscriptions`
	);

	// Get current recognition cache to check which ones we're already subscribed to
	const cache = get(recognitionCache);

	// Subscribe to ALL contributors to get their SOGF data
	// This allows us to receive theirShare values and determine who's mutual
	allContributors.forEach((contributorId) => {
		if (
			!cache[contributorId] ||
			cache[contributorId].timestamp < Date.now() - 24 * 60 * 60 * 1000
		) {
			// Subscribe if we don't have data or it's older than 24 hours
			console.log(`[NETWORK] Setting up subscription for contributor: ${contributorId}`);
			subscribeToContributorSOGF(contributorId);
		} else {
			console.log(`[NETWORK] Already have recent data for ${contributorId}, skipping subscription`);
		}
	});
});

/**
 * Debug function to manually trigger subscriptions for all contributors
 */
export function debugTriggerSubscriptions() {
	console.log('[DEBUG] Manually triggering subscriptions for all contributors');
	const allContributors = get(contributors);

	allContributors.forEach((contributorId) => {
		console.log(`[DEBUG] Setting up subscription for: ${contributorId}`);
		subscribeToContributorSOGF(contributorId);
	});

	console.log(`[DEBUG] Set up ${allContributors.length} subscriptions`);
}
