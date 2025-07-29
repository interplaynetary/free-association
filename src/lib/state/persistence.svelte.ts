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
import { userContacts, isLoadingContacts } from './users.svelte';
import { userDesiredSlotComposeFrom, userDesiredSlotComposeInto } from './compose.svelte';
import { chatReadStates, isLoadingChatReadStates } from './chat.svelte';
import { user, userPub } from './gun.svelte';

/**
 * Check if the user object is properly initialized and has the necessary methods
 */
function isUserInitialized(): boolean {
	return !!(user && typeof user.get === 'function');
}

export function persistTree() {
	// Check if user is initialized
	if (!isUserInitialized()) {
		console.log('[PERSIST] User not initialized, skipping tree persistence');
		return;
	}

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
	// Check if user is initialized
	if (!isUserInitialized()) {
		console.log('[PERSIST] User not initialized, skipping SOGF persistence');
		return;
	}

	// Don't persist while loading
	if (get(isLoadingSogf)) {
		console.log('[PERSIST] Skipping SOGF persistence because SOGF is being loaded');
		return;
	}

	const sogfValue = get(userSogf);
	if (sogfValue) {
		console.log('[PERSIST] Starting SOGF persistence...');
		console.log('[PERSIST] SOGF data:', sogfValue);

		try {
			// Create a deep clone to avoid reactivity issues
			const sogfClone = structuredClone(sogfValue);

			// Serialize to JSON to preserve number types
			const sogfJson = JSON.stringify(sogfClone);
			console.log('[PERSIST] Serialized SOGF length:', sogfJson.length);

			// Store in Gun with ACK callback
			user.get('sogf').put(sogfJson, (ack: { err?: any }) => {
				if (ack.err) {
					console.error('[PERSIST] Error saving SOGF to Gun:', ack.err);
				} else {
					console.log('[PERSIST] SOGF successfully saved to Gun');
				}
			});
		} catch (error) {
			console.error('[PERSIST] Error serializing SOGF:', error);
		}
	}
}

export function persistProviderShares() {
	// Check if user is initialized
	if (!isUserInitialized()) {
		console.log('[PERSIST] User not initialized, skipping provider shares persistence');
		return;
	}

	// Store provider shares
	const shares = get(providerShares);
	if (shares && Object.keys(shares).length > 0) {
		user.get('providerShares').put(structuredClone(shares));
	}
}

export function persistCapacities() {
	// Check if user is initialized
	if (!isUserInitialized()) {
		console.log('[PERSIST] User not initialized, skipping capacities persistence');
		return;
	}

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
	// Check if user is initialized
	if (!isUserInitialized()) {
		console.log('[PERSIST] User not initialized, skipping contributor capacity shares persistence');
		return;
	}

	const ourId = get(userPub);
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
 * Persist user's desired slot compose-from to Gun
 */
export function persistUserDesiredSlotComposeFrom() {
	// Check if user is initialized
	if (!isUserInitialized()) {
		console.log(
			'[PERSIST] User not initialized, skipping user desired slot compose-from persistence'
		);
		return;
	}

	const userDesiredComposeFromValue = get(userDesiredSlotComposeFrom);
	if (!userDesiredComposeFromValue || Object.keys(userDesiredComposeFromValue).length === 0) {
		console.log('[PERSIST] No user desired slot compose-from data to persist');
		return;
	}

	console.log('[PERSIST] Starting user desired slot compose-from persistence...');
	console.log('[PERSIST] User desired slot compose-from:', userDesiredComposeFromValue);

	try {
		// Create a deep clone to avoid reactivity issues
		const composeFromClone = structuredClone(userDesiredComposeFromValue);

		// Serialize to JSON
		const composeFromJson = JSON.stringify(composeFromClone);
		console.log(
			'[PERSIST] Serialized user desired slot compose-from length:',
			composeFromJson.length
		);

		// Store in Gun under the expected path that network subscribers use
		user.get('desiredSlotComposeFrom').put(composeFromJson, (ack: { err?: any }) => {
			if (ack.err) {
				console.error('[PERSIST] Error saving user desired slot compose-from to Gun:', ack.err);
			} else {
				console.log('[PERSIST] User desired slot compose-from successfully saved to Gun');
			}
		});
	} catch (error) {
		console.error('[PERSIST] Error serializing user desired slot compose-from:', error);
	}
}

/**
 * Persist user's desired slot compose-into to Gun
 */
export function persistUserDesiredSlotComposeInto() {
	// Check if user is initialized
	if (!isUserInitialized()) {
		console.log(
			'[PERSIST] User not initialized, skipping user desired slot compose-into persistence'
		);
		return;
	}

	const userDesiredComposeIntoValue = get(userDesiredSlotComposeInto);
	if (!userDesiredComposeIntoValue || Object.keys(userDesiredComposeIntoValue).length === 0) {
		console.log('[PERSIST] No user desired slot compose-into data to persist');
		return;
	}

	console.log('[PERSIST] Starting user desired slot compose-into persistence...');
	console.log('[PERSIST] User desired slot compose-into:', userDesiredComposeIntoValue);

	try {
		// Create a deep clone to avoid reactivity issues
		const composeIntoClone = structuredClone(userDesiredComposeIntoValue);

		// Serialize to JSON
		const composeIntoJson = JSON.stringify(composeIntoClone);
		console.log(
			'[PERSIST] Serialized user desired slot compose-into length:',
			composeIntoJson.length
		);

		// Store in Gun under the expected path that network subscribers use
		user.get('desiredSlotComposeInto').put(composeIntoJson, (ack: { err?: any }) => {
			if (ack.err) {
				console.error('[PERSIST] Error saving user desired slot compose-into to Gun:', ack.err);
			} else {
				console.log('[PERSIST] User desired slot compose-into successfully saved to Gun');
			}
		});
	} catch (error) {
		console.error('[PERSIST] Error serializing user desired slot compose-into:', error);
	}
}

/**
 * Persist user's contacts to Gun
 */
export function persistContacts() {
	// Check if user is initialized
	if (!isUserInitialized()) {
		console.log('[PERSIST] User not initialized, skipping contacts persistence');
		return;
	}

	// Don't persist while loading
	if (get(isLoadingContacts)) {
		console.log('[PERSIST] Skipping contacts persistence because contacts are being loaded');
		return;
	}

	const contactsValue = get(userContacts);

	if (!contactsValue) {
		console.log('[PERSIST] No contacts data to persist');
		return;
	}

	// Additional safety check: don't persist empty contacts during initialization
	// to avoid race condition where empty store overwrites loaded data from network
	if (Object.keys(contactsValue).length === 0) {
		console.log('[PERSIST] Skipping persistence of empty contacts (likely initialization)');
		return;
	}

	console.log('[PERSIST] Starting contacts persistence...');
	console.log('[PERSIST] Contacts count:', Object.keys(contactsValue).length);
	console.log('[PERSIST] Contacts:', contactsValue);

	try {
		// Create a deep clone to avoid reactivity issues
		const contactsClone = structuredClone(contactsValue);

		// Serialize to JSON
		const contactsJson = JSON.stringify(contactsClone);
		console.log('[PERSIST] Serialized contacts length:', contactsJson.length);

		// Store in Gun under the expected path that network subscribers use
		user.get('contacts').put(contactsJson, (ack: { err?: any }) => {
			if (ack.err) {
				console.error('[PERSIST] Error saving contacts to Gun:', ack.err);
			} else {
				console.log('[PERSIST] Contacts successfully saved to Gun', contactsJson);
			}
		});
	} catch (error) {
		console.error('[PERSIST] Error serializing contacts:', error);
	}
}

/**
 * Persist chat read states to Gun
 */
export function persistChatReadStates() {
	// Check if user is initialized
	if (!isUserInitialized()) {
		console.log('[PERSIST] User not initialized, skipping chat read states persistence');
		return;
	}

	// Don't persist while loading
	if (get(isLoadingChatReadStates)) {
		console.log(
			'[PERSIST] Skipping chat read states persistence because read states are being loaded'
		);
		return;
	}

	const chatReadStatesValue = get(chatReadStates);

	if (!chatReadStatesValue) {
		console.log('[PERSIST] No chat read states data to persist');
		return;
	}

	// Additional safety check: don't persist empty read states during initialization
	if (Object.keys(chatReadStatesValue).length === 0) {
		console.log('[PERSIST] Skipping persistence of empty chat read states (likely initialization)');
		return;
	}

	console.log('[PERSIST] Starting chat read states persistence...');
	console.log('[PERSIST] Chat read states count:', Object.keys(chatReadStatesValue).length);

	try {
		// Create a deep clone to avoid reactivity issues
		const readStatesClone = structuredClone(chatReadStatesValue);

		// Serialize to JSON
		const readStatesJson = JSON.stringify(readStatesClone);
		console.log('[PERSIST] Serialized chat read states length:', readStatesJson.length);

		// Store in Gun with ACK callback
		user.get('chatReadStates').put(readStatesJson, (ack: { err?: any }) => {
			if (ack.err) {
				console.error('[PERSIST] Error saving chat read states to Gun:', ack.err);
			} else {
				console.log('[PERSIST] Chat read states successfully saved to Gun');
			}
		});
	} catch (error) {
		console.error('[PERSIST] Error serializing chat read states:', error);
	}
}
