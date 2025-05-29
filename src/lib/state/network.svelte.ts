import { get } from 'svelte/store';
import { gun, user, persistRecognitionCache } from './gun.svelte';
import {
	contributors,
	mutualContributors,
	recognitionCache,
	networkCapacities
} from './core.svelte';
import type { CapacitiesCollection } from '$lib/schema';
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

export function subscribeToContributorCapacities(contributorId: string) {
	console.log(`[NETWORK] Setting up subscription to ${contributorId}'s capacities`);

	const contributorCapacities = gun.get(`~${contributorId}`).get('capacities');

	// Add timeout protection as recommended by the Gun guide
	// to prevent hanging when data doesn't exist
	let hasReceived = false;
	const timeout = setTimeout(() => {
		if (!hasReceived) {
			console.log(`[NETWORK] Timeout waiting for capacities from ${contributorId}`);
		}
	}, 10000); // 10 second timeout

	// Subscribe to changes
	contributorCapacities.on((capacitiesData: any) => {
		hasReceived = true;
		clearTimeout(timeout);

		if (!capacitiesData) return;

		console.log(`[NETWORK] Received capacities update from ${contributorId}:`, capacitiesData);

		// Get our user ID
		const ourId = user.is?.pub;
		if (!ourId) return;

		// Parse the capacities data if it's a string
		let parsedCapacities;
		try {
			parsedCapacities =
				typeof capacitiesData === 'string' ? JSON.parse(capacitiesData) : capacitiesData;
		} catch (error) {
			console.error('[NETWORK] Error parsing capacities data:', error);
			return;
		}

		// Update the networkCapacities store with this contributor's capacities
		networkCapacities.update((currentNetworkCapacities) => {
			const updated = { ...currentNetworkCapacities };

			// Update this contributor's capacities
			updated[contributorId] = parsedCapacities;

			console.log(
				`[NETWORK] Updated networkCapacities for ${contributorId}: added ${Object.keys(parsedCapacities).length} capacities`
			);
			return updated;
		});
	});
}

// Watch for changes to contributors and subscribe to get their SOGF data
contributors.subscribe((allContributors) => {
	if (!allContributors.length) return;

	// Only run this if we're authenticated
	if (!user.is?.pub) return;

	console.log(
		`[NETWORK] Contributors changed, now have ${allContributors.length}, setting up subscriptions`
	);

	// Subscribe to ALL contributors to get their current SOGF data
	// We always re-subscribe to ensure we have the most current theirShare values
	// This is especially important when contributors are re-added after being removed
	allContributors.forEach((contributorId) => {
		console.log(`[NETWORK] Setting up subscription for contributor: ${contributorId}`);
		subscribeToContributorSOGF(contributorId);
	});
});

// Watch for changes to mutual contributors and subscribe to get their capacity data
mutualContributors.subscribe((currentMutualContributors) => {
	if (!currentMutualContributors.length) {
		// If no mutual contributors, clear all network capacities
		networkCapacities.set({});
		return;
	}

	// Only run this if we're authenticated
	if (!user.is?.pub) return;

	console.log(
		`[NETWORK] Mutual contributors changed, now have ${currentMutualContributors.length}, setting up capacity subscriptions`
	);

	// Clean up capacities from contributors who are no longer mutual contributors
	networkCapacities.update((currentNetworkCapacities) => {
		if (!currentNetworkCapacities) return {};

		const cleaned: Record<string, CapacitiesCollection> = {};
		Object.entries(currentNetworkCapacities).forEach(([contributorId, capacities]) => {
			// Keep capacities from current mutual contributors only
			if (currentMutualContributors.includes(contributorId)) {
				cleaned[contributorId] = capacities;
			}
		});

		console.log(
			`[NETWORK] Cleaned network capacities: kept ${Object.keys(cleaned).length} contributors from ${Object.keys(currentNetworkCapacities).length} total`
		);
		return cleaned;
	});

	// Subscribe to mutual contributors for capacity data only (SOGF handled by contributors subscription)
	currentMutualContributors.forEach((contributorId) => {
		console.log(
			`[NETWORK] Setting up capacity subscription for mutual contributor: ${contributorId}`
		);
		subscribeToContributorCapacities(contributorId);
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
