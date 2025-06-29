import { get } from 'svelte/store';
import {
	userTree,
	userSogf,
	userCapacities,
	isLoadingTree,
	isLoadingCapacities,
	isLoadingSogf,
	providerShares,
	contributorCapacityShares
} from './core.svelte';
import { userDesiredComposeFrom, userDesiredComposeInto } from './protocol/compose.svelte';
import { user, userpub } from './gun.svelte';

export function persistTree() {
	// Don't persist while loading
	if (get(isLoadingTree)) {
		console.log('[PERSIST] Skipping tree persistence because tree is being loaded');
		return;
	}

	const treeValue = get(userTree);
	if (treeValue) {
		console.log('[PERSIST] Starting tree persistence...');
		console.log('[PERSIST] Tree structure before serialization:', {
			id: treeValue.id,
			childCount: treeValue.children.length
		});

		// Serialize tree for storage
		const treeJson = JSON.stringify(structuredClone(treeValue));
		console.log('[PERSIST] Serialized tree length:', treeJson.length);
		console.log('[PERSIST] Tree JSON preview:', treeJson.substring(0, 100) + '...');

		// Store in Gun
		user.get('tree').put(treeJson, (ack: { err?: any }) => {
			if (ack.err) {
				console.error('[PERSIST] Error saving tree to Gun:', ack.err);
			} else {
				console.log('[PERSIST] Tree successfully saved to Gun');
			}
		});
	}
}

export function persistSogf() {
	// Don't persist while loading
	if (get(isLoadingSogf)) {
		console.log('[PERSIST] Skipping SOGF persistence because SOGF is being loaded');
		return;
	}

	const sogfValue = get(userSogf);
	if (sogfValue) {
		// Ensure we're storing in the user's protected space using the correct pattern
		// This follows the Gun guide's recommendation for user-specific data
		user.get('sogf').put(structuredClone(sogfValue));
	}
}

export function persistProviderShares() {
	// Store provider shares
	const shares = get(providerShares);
	if (shares && Object.keys(shares).length > 0) {
		user.get('providerShares').put(structuredClone(shares));
	}
}

export function persistCapacities() {
	// Don't persist while loading
	if (get(isLoadingCapacities)) {
		console.log('[PERSIST] Skipping capacities persistence because capacities are being loaded');
		return;
	}

	const userCapacitiesValue = get(userCapacities);
	if (userCapacitiesValue) {
		console.log('[PERSIST] Starting capacities persistence...');
		console.log('[PERSIST] Capacities count:', Object.keys(userCapacitiesValue).length);

		try {
			// First create a deep clone to avoid any reactivity issues
			const capacitiesClone = structuredClone(userCapacitiesValue);

			// Log the data being saved
			console.log('[PERSIST] Saving capacities:', capacitiesClone);

			// Then serialize to JSON
			const capacitiesJson = JSON.stringify(capacitiesClone);
			console.log('[PERSIST] Serialized capacities length:', capacitiesJson.length);

			// Store in Gun with ACK callback
			user.get('capacities').put(capacitiesJson, (ack: { err?: any }) => {
				if (ack.err) {
					console.error('[PERSIST] Error saving capacities to Gun:', ack.err);
				} else {
					console.log('[PERSIST] Capacities successfully saved to Gun');
				}
			});
		} catch (error) {
			console.error('[PERSIST] Error serializing capacities:', error);
		}
	}
}

/**
 * Persist contributor capacity shares to gun
 */
export function persistContributorCapacityShares() {
	const ourId = get(userpub);
	if (!ourId) {
		console.log('[PERSIST] No user ID available, cannot persist contributor capacity shares');
		return;
	}

	const shares = get(contributorCapacityShares);
	// console.log('[PERSIST] Persisting contributor capacity shares:', shares);

	// For each contributor, store their shares under their path
	Object.entries(shares).forEach(([contributorId, capacityShares]) => {
		// Store under contributorId/capacityShares/{ourId}
		user
			.get('capacityShares')
			.get(contributorId)
			.put(JSON.stringify(capacityShares), (ack: any) => {
				if (ack.err) {
					console.error(
						`[PERSIST] Error persisting capacity shares for contributor ${contributorId}:`,
						ack.err
					);
				} else {
					/*console.log(
						`[PERSIST] Successfully persisted capacity shares for contributor ${contributorId}`
					); */
				}
			});
	});
}

/**
 * Persist user's desired compose-from to Gun
 */
export function persistUserDesiredComposeFrom() {
	const userDesiredComposeFromValue = get(userDesiredComposeFrom);
	if (!userDesiredComposeFromValue || Object.keys(userDesiredComposeFromValue).length === 0) {
		console.log('[PERSIST] No user desired compose-from data to persist');
		return;
	}

	console.log('[PERSIST] Starting user desired compose-from persistence...');
	console.log('[PERSIST] User desired compose-from:', userDesiredComposeFromValue);

	try {
		// Create a deep clone to avoid reactivity issues
		const composeFromClone = structuredClone(userDesiredComposeFromValue);

		// Serialize to JSON
		const composeFromJson = JSON.stringify(composeFromClone);
		console.log('[PERSIST] Serialized user desired compose-from length:', composeFromJson.length);

		// Store in Gun under the expected path that network subscribers use
		user.get('desiredComposeFrom').put(composeFromJson, (ack: { err?: any }) => {
			if (ack.err) {
				console.error('[PERSIST] Error saving user desired compose-from to Gun:', ack.err);
			} else {
				console.log('[PERSIST] User desired compose-from successfully saved to Gun');
			}
		});
	} catch (error) {
		console.error('[PERSIST] Error serializing user desired compose-from:', error);
	}
}

/**
 * Persist user's desired compose-into to Gun
 */
export function persistUserDesiredComposeInto() {
	const userDesiredComposeIntoValue = get(userDesiredComposeInto);
	if (!userDesiredComposeIntoValue || Object.keys(userDesiredComposeIntoValue).length === 0) {
		console.log('[PERSIST] No user desired compose-into data to persist');
		return;
	}

	console.log('[PERSIST] Starting user desired compose-into persistence...');
	console.log('[PERSIST] User desired compose-into:', userDesiredComposeIntoValue);

	try {
		// Create a deep clone to avoid reactivity issues
		const composeIntoClone = structuredClone(userDesiredComposeIntoValue);

		// Serialize to JSON
		const composeIntoJson = JSON.stringify(composeIntoClone);
		console.log('[PERSIST] Serialized user desired compose-into length:', composeIntoJson.length);

		// Store in Gun under the expected path that network subscribers use
		user.get('desiredComposeInto').put(composeIntoJson, (ack: { err?: any }) => {
			if (ack.err) {
				console.error('[PERSIST] Error saving user desired compose-into to Gun:', ack.err);
			} else {
				console.log('[PERSIST] User desired compose-into successfully saved to Gun');
			}
		});
	} catch (error) {
		console.error('[PERSIST] Error serializing user desired compose-into:', error);
	}
}
