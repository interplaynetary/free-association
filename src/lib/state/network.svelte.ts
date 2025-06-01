import { get } from 'svelte/store';
import { gun, user, isAuthenticating } from './gun.svelte';
import {
	contributors,
	mutualContributors,
	recognitionCache,
	networkCapacities,
	networkCapacityShares
} from './core.svelte';
import type { CapacitiesCollection } from '$lib/schema';
import { updateRecognitionCache } from './calculations.svelte';
import { parseCapacities } from '$lib/validation';
import type { Writable } from 'svelte/store';

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

	// Log the updated cache and force reactivity check
	const updatedCache = get(recognitionCache);

	console.log(`[NETWORK] Cache after network update from ${contributorId}:`, updatedCache);
}

function setupTimeoutProtection(contributorId: string, dataType: string) {
	let hasReceived = false;
	const timeout = setTimeout(() => {
		if (!hasReceived) {
			console.log(`[NETWORK] Timeout waiting for ${dataType} from ${contributorId}`);
		}
	}, 10000); // 10 second timeout

	return {
		markReceived: () => {
			hasReceived = true;
			clearTimeout(timeout);
		},
		hasReceived
	};
}

/**
 * Set up network subscriptions for contributor's SOGF
 * @param contributorId The ID of the contributor to subscribe to
 */
export function subscribeToContributorSOGF(contributorId: string) {
	console.log(`[NETWORK] Setting up subscription to ${contributorId}'s SOGF`);

	const contributorSogf = gun.get(`~${contributorId}`).get('sogf');
	//const timeoutProtection = setupTimeoutProtection(contributorId, 'SOGF');

	// Subscribe to changes
	contributorSogf.on((sogfData: any) => {
		//timeoutProtection.markReceived();

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

	// Subscribe to changes
	contributorCapacities.on((capacitiesData: any) => {
		if (!capacitiesData) return;

		console.log(`[NETWORK] Received capacities update from ${contributorId}:`, capacitiesData);

		// Get our user ID
		const ourId = user.is?.pub;
		if (!ourId) return;

		// Parse and validate the capacities data
		const validatedCapacities = parseCapacities(capacitiesData);
		if (!validatedCapacities || Object.keys(validatedCapacities).length === 0) {
			console.error('[NETWORK] Failed to validate capacities from contributor:', contributorId);
			return;
		}

		// Update the networkCapacities store with this contributor's validated capacities
		networkCapacities.update((currentNetworkCapacities) => {
			const updated = { ...currentNetworkCapacities };
			updated[contributorId] = validatedCapacities;
			console.log(
				`[NETWORK] Updated networkCapacities for ${contributorId}: added ${Object.keys(validatedCapacities).length} capacities`
			);
			return updated;
		});
	});
}

/**
 * Subscribe to our capacity shares from a contributor
 * @param contributorId The contributor to subscribe to
 */
export function subscribeToContributorCapacityShares(contributorId: string) {
	const ourId = user.is?.pub;
	if (!ourId) {
		console.log('[NETWORK] Cannot subscribe to capacity shares - not authenticated');
		return;
	}

	console.log(
		`[NETWORK] Setting up capacity shares subscription for contributor: ${contributorId}`
	);

	// Subscribe to our capacity shares from this contributor
	gun
		.get(`~${contributorId}`)
		.get('capacityShares')
		.get(ourId)
		.on((shares: any) => {
			if (!shares) {
				console.log(`[NETWORK] No capacity shares from contributor ${contributorId}`);
				// Update store to remove shares for this contributor
				networkCapacityShares.update((current) => {
					const { [contributorId]: _, ...rest } = current;
					return rest;
				});
				return;
			}

			try {
				// Parse and validate the shares
				let parsedShares: Record<string, number>;
				if (typeof shares === 'string') {
					parsedShares = JSON.parse(shares);
				} else {
					parsedShares = shares;
				}

				// Validate that all shares are numbers between 0 and 1
				const validatedShares: Record<string, number> = {};
				Object.entries(parsedShares).forEach(([capacityId, share]) => {
					if (typeof share === 'number' && share >= 0 && share <= 1) {
						validatedShares[capacityId] = share;
					} else {
						console.warn(`[NETWORK] Invalid share value for capacity ${capacityId}:`, share);
					}
				});

				console.log(
					`[NETWORK] Received capacity shares from contributor ${contributorId}:`,
					validatedShares
				);

				// Update the store with the validated shares
				networkCapacityShares.update((current) => ({
					...current,
					[contributorId]: validatedShares
				}));
			} catch (error) {
				console.error(
					`[NETWORK] Error parsing/validating capacity shares from contributor ${contributorId}:`,
					error
				);
			}
		});
}

// Watch for changes to contributors and subscribe to get their SOGF data
contributors.subscribe((allContributors) => {
	// Don't process while authenticating
	if (get(isAuthenticating)) {
		console.log('[NETWORK] Skipping contributor subscription while authenticating');
		return;
	}

	if (!allContributors.length) {
		console.log('[NETWORK] No contributors to subscribe to');
		return;
	}

	// Only run this if we're authenticated
	if (!user.is?.pub) {
		console.log('[NETWORK] Cannot subscribe to contributors - not authenticated');
		return;
	}

	console.log(
		`[NETWORK] Contributors changed, now have ${allContributors.length}, setting up subscriptions`
	);

	// Subscribe to ALL contributors to get their current SOGF data
	// We always re-subscribe to ensure we have the most current theirShare values
	// This is especially important when contributors are re-added after being removed
	allContributors.forEach((contributorId) => {
		try {
			console.log(`[NETWORK] Setting up subscription for contributor: ${contributorId}`);
			subscribeToContributorSOGF(contributorId);
		} catch (error) {
			console.error(`[NETWORK] Error subscribing to contributor ${contributorId}:`, error);
		}
	});
});

// Watch for changes to mutual contributors and subscribe to get their capacity data
mutualContributors.subscribe((currentMutualContributors) => {
	// Don't process while authenticating
	if (get(isAuthenticating)) {
		console.log('[NETWORK] Skipping mutual contributor subscription while authenticating');
		return;
	}

	if (!currentMutualContributors.length) {
		// If no mutual contributors, clear all network capacities
		networkCapacities.set({});
		return;
	}

	// Only run this if we're authenticated
	if (!user.is?.pub) {
		console.log('[NETWORK] Cannot subscribe to mutual contributors - not authenticated');
		return;
	}

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
		try {
			console.log(
				`[NETWORK] Setting up capacity subscription for mutual contributor: ${contributorId}`
			);
			subscribeToContributorCapacities(contributorId);
		} catch (error) {
			console.error(`[NETWORK] Error subscribing to mutual contributor ${contributorId}:`, error);
		}
	});
});

// Watch for changes to mutual contributors and subscribe to get their capacity shares
mutualContributors.subscribe((currentMutualContributors) => {
	// Don't process while authenticating
	if (get(isAuthenticating)) {
		console.log('[NETWORK] Skipping mutual contributor subscription while authenticating');
		return;
	}

	if (!currentMutualContributors.length) {
		// If no mutual contributors, clear all capacity shares
		networkCapacityShares.set({});
		return;
	}

	// Only run this if we're authenticated
	if (!user.is?.pub) {
		console.log('[NETWORK] Cannot subscribe to mutual contributors - not authenticated');
		return;
	}

	console.log(
		`[NETWORK] Mutual contributors changed, now have ${currentMutualContributors.length}, setting up capacity shares subscriptions`
	);

	// Clean up shares from contributors who are no longer mutual contributors
	networkCapacityShares.update((current) => {
		const cleaned: Record<string, Record<string, number>> = {};
		Object.entries(current).forEach(([contributorId, shares]) => {
			// Keep shares from current mutual contributors only
			if (currentMutualContributors.includes(contributorId)) {
				cleaned[contributorId] = shares;
			}
		});

		console.log(
			`[NETWORK] Cleaned capacity shares: kept ${Object.keys(cleaned).length} contributors from ${Object.keys(current).length} total`
		);
		return cleaned;
	});

	// Subscribe to capacity shares for each mutual contributor
	currentMutualContributors.forEach((contributorId) => {
		try {
			console.log(
				`[NETWORK] Setting up capacity shares subscription for mutual contributor: ${contributorId}`
			);
			subscribeToContributorCapacityShares(contributorId);
		} catch (error) {
			console.error(
				`[NETWORK] Error subscribing to mutual contributor capacity shares ${contributorId}:`,
				error
			);
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
