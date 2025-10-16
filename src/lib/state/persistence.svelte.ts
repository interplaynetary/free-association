import { get } from 'svelte/store';
import {
	userTree,
	userSogf,
	userCapacities,
	isLoadingTree,
	isLoadingCapacities,
	isLoadingSogf,
	generalShares
} from './core.svelte';
import {
	userContacts,
	isLoadingContacts,
	resolveToPublicKey,
	resolveContactIdsInTree,
	resolveContactIdsInSlotComposition
} from './users.svelte';
import {
	userDesiredSlotComposeFrom,
	userDesiredSlotComposeInto,
	providerAllocationStates
} from './core.svelte';
import { chatReadStates, isLoadingChatReadStates } from './chat.svelte';
import { user, userPub } from './gun.svelte';
import { lastNetworkTimestamps } from './network.svelte';
import { processCapacitiesLocations } from '$lib/utils/geocodingCache';
import type { Node, NonRootNode } from '$lib/schema';
import { getGunTimestamp, compareGunTimestamps, isReliableGunTimestamp } from '$lib/utils/gunTimestamp';

// DELETED: Timestamp tracking - Gun handles all timestamp tracking via GUN.state.is()
// No need for application-level timestamp validation since Gun's CRDT handles conflicts
// ADDED: Timestamp validation before persistence to prevent stale writes

/**
 * Check if the user object is properly initialized and has the necessary methods
 */
function isUserInitialized(): boolean {
	return !!(user && typeof user.get === 'function');
}

/**
 * Helper to safely persist data with timestamp validation
 * Reads current Gun timestamp before writing to prevent overwriting newer network data
 *
 * @param path - Gun path to persist to (e.g., 'capacities')
 * @param data - Data to persist (JSON string)
 * @param localStoreTimestamp - The timestamp when we last loaded this data from network
 * @param onComplete - Optional callback after persistence completes
 */
async function safelyPersist(
	path: string,
	data: string,
	localStoreTimestamp: number | null,
	onComplete?: (err?: any) => void
): Promise<void> {
	return new Promise((resolve) => {
		// First, read current Gun data to check timestamp
		user.get(path).once((currentData: any) => {
			let shouldPersist = true;
			let warningMessage = '';

			if (currentData && typeof currentData === 'object') {
				const currentNetworkTimestamp = getGunTimestamp(currentData, path);

				if (currentNetworkTimestamp !== null && isReliableGunTimestamp(currentNetworkTimestamp)) {
					// Compare network timestamp with our last known timestamp
					if (localStoreTimestamp !== null) {
						const comparison = compareGunTimestamps(currentNetworkTimestamp, localStoreTimestamp);

						if (comparison > 0) {
							// Network has newer data than what we loaded!
							shouldPersist = false;
							warningMessage =
								`[PERSIST] BLOCKED: Network has newer data for ${path}. ` +
								`Network timestamp: ${new Date(currentNetworkTimestamp).toISOString()}, ` +
								`Our last load: ${new Date(localStoreTimestamp).toISOString()}. ` +
								`Refusing to overwrite. User should reload to see latest data.`;
						} else if (comparison === 0) {
							// Timestamps match - data hasn't changed on network
							// Check if data actually changed
							const parsedCurrentData = typeof currentData === 'string' ? currentData : JSON.stringify(currentData);
							if (parsedCurrentData === data) {
								shouldPersist = false;
								console.log(`[PERSIST] Skipping ${path}: data unchanged`);
							}
						}
						// comparison < 0 means our data is newer, proceed with persist
					}
				}
			}

			if (!shouldPersist) {
				console.error(warningMessage);
				if (onComplete) {
					onComplete(new Error('Stale data detected - refusing to overwrite'));
				}
				resolve();
				return;
			}

			// Proceed with put - Gun's CRDT handles conflict resolution
			user.get(path).put(data, (ack: { err?: any }) => {
				if (onComplete) {
					onComplete(ack.err);
				}
				resolve();
			});
		});
	});
}

export async function persistTree() {
	// Import config dynamically to avoid circular dependencies
	const { USE_HOLSTER_TREE } = await import('$lib/config');

	if (USE_HOLSTER_TREE) {
		console.log('[PERSIST] Using Holster for tree - skipping Gun persistence');
		return;
	}

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
		console.log('[PERSIST] Tree structure before resolution:', {
			id: treeValue.id,
			childCount: treeValue.children.length
		});

		// Resolve contact IDs to public keys before persistence
		console.log('[PERSIST] Resolving contact IDs to public keys...');
		const resolvedTree = resolveContactIdsInTree(treeValue);
		console.log('[PERSIST] Contact ID resolution completed');

		console.log('[PERSIST] Tree structure after resolution:', {
			id: resolvedTree.id,
			childCount: resolvedTree.children.length
		});

		// Serialize resolved tree for storage
		const treeJson = JSON.stringify(resolvedTree);
		console.log('[PERSIST] Serialized tree length:', treeJson.length);
		console.log('[PERSIST] Tree JSON preview:', treeJson.substring(0, 100) + '...');

		// Store in Gun with timestamp validation
		await safelyPersist('tree', treeJson, lastNetworkTimestamps.tree, (err) => {
			if (err) {
				console.error('[PERSIST] Error saving tree to Gun:', err);
			} else {
				console.log('[PERSIST] Tree successfully saved to Gun with resolved contact IDs');
			}
		});
	}
}

export async function persistSogf() {
	// Import config dynamically to avoid circular dependencies
	const { USE_HOLSTER_RECOGNITION } = await import('$lib/config');

	if (USE_HOLSTER_RECOGNITION) {
		console.log('[PERSIST] Using Holster for SOGF - skipping Gun persistence');
		// Holster persistence is triggered automatically via subscriptions
		return;
	}

	if (!isUserInitialized()) {
		console.log('[PERSIST] User not initialized, skipping SOGF persistence');
		return;
	}

	if (get(isLoadingSogf)) {
		console.log('[PERSIST] Skipping SOGF persistence because SOGF is being loaded');
		return;
	}

	const sogfValue = get(userSogf);
	if (sogfValue) {
		console.log('[PERSIST] Starting SOGF persistence...');

		try {
			// Store unwrapped data directly - Gun tracks timestamps via GUN.state.is()
			const sogfClone = structuredClone(sogfValue);
			const sogfJson = JSON.stringify(sogfClone);
			console.log('[PERSIST] Serialized SOGF length:', sogfJson.length);

			await safelyPersist('sogf', sogfJson, lastNetworkTimestamps.sogf, (err) => {
				if (err) {
					console.error('[PERSIST] Error saving SOGF to Gun:', err);
				} else {
					console.log('[PERSIST] SOGF successfully saved to Gun');
				}
			});
		} catch (error) {
			console.error('[PERSIST] Error processing SOGF:', error);
		}
	}
}

export function persistGeneralShares() {
	// Check if user is initialized
	if (!isUserInitialized()) {
		console.log('[PERSIST] User not initialized, skipping provider shares persistence');
		return;
	}

	// Store general shares
	const shares = get(generalShares);
	if (shares && Object.keys(shares).length > 0) {
		console.log('[PERSIST] Starting general shares persistence...');
		console.log('[PERSIST] General shares (already contains public keys):', shares);

		// Our core stores now contain public keys by design, so no filtering needed
		user.get('generalShares').put(structuredClone(shares), (ack: { err?: any }) => {
			if (ack.err) {
				console.error('[PERSIST] Error saving general shares to Gun:', ack.err);
			} else {
				console.log('[PERSIST] General shares successfully saved to Gun');
			}
		});
	}
}

export async function persistCapacities() {
	// Import config dynamically to avoid circular dependencies
	const { USE_HOLSTER_CAPACITIES } = await import('$lib/config');

	if (USE_HOLSTER_CAPACITIES) {
		console.log('[PERSIST] Using Holster for capacities - skipping Gun persistence');
		return;
	}

	if (!isUserInitialized()) {
		console.log('[PERSIST] User not initialized, skipping capacities persistence');
		return;
	}

	if (get(isLoadingCapacities)) {
		console.log('[PERSIST] Still loading capacities, deferring persistence');
		setTimeout(() => {
			if (!get(isLoadingCapacities)) {
				persistCapacities();
			}
		}, 500);
		return;
	}

	const userCapacitiesValue = get(userCapacities);
	if (!userCapacitiesValue || Object.keys(userCapacitiesValue).length === 0) {
		console.log('[PERSIST] No capacities to persist');
		return;
	}

	console.log('[PERSIST] Starting capacities persistence...');

	try {
		// Process addresses for geocoding
		const capacitiesWithCoordinates = await processCapacitiesLocations(userCapacitiesValue);

		// Store unwrapped data directly - Gun tracks timestamps via GUN.state.is()
		const capacitiesClone = structuredClone(capacitiesWithCoordinates);
		const capacitiesJson = JSON.stringify(capacitiesClone);
		console.log('[PERSIST] Serialized capacities length:', capacitiesJson.length);

		await safelyPersist('capacities', capacitiesJson, lastNetworkTimestamps.capacities, (err) => {
			if (err) {
				console.error('[PERSIST] Error saving capacities to Gun:', err);
			} else {
				console.log('[PERSIST] Capacities successfully saved to Gun');
			}
		});
	} catch (error) {
		console.error('[PERSIST] Error processing capacities:', error);
	}
}

// DELETED: persistContributorCapacityShares - Stub store no longer needs persistence
// The contributorCapacityShares store is now a compatibility bridge that returns empty object
// See core.svelte.ts for details. No subscription or persistence needed for this stub.

// DELETED: persistContributorCapacitySlotQuantities - Replaced by efficient provider-centric algorithm
// Old approach persisted recipient-computed slot quantities
// New approach: Providers compute and publish allocations directly via computedProviderAllocations

/**
 * Persist user's desired slot compose-from to Gun
 */
export async function persistUserDesiredSlotComposeFrom() {
	// Import config dynamically to avoid circular dependencies
	const { USE_HOLSTER_COMPOSE } = await import('$lib/config');

	if (USE_HOLSTER_COMPOSE) {
		console.log('[PERSIST] Using Holster for compose - skipping Gun persistence');
		// Holster persistence happens in compose-holster.svelte.ts
		return;
	}

	if (!isUserInitialized()) {
		console.log('[PERSIST] User not initialized, skipping slot compose-from persistence');
		return;
	}

	if (get(isLoadingCapacities)) {
		console.log('[PERSIST] Capacities still loading, deferring slot compose-from persistence');
		setTimeout(() => {
			if (!get(isLoadingCapacities)) {
				persistUserDesiredSlotComposeFrom();
			}
		}, 500);
		return;
	}

	const userDesiredComposeFromValue = get(userDesiredSlotComposeFrom);
	if (!userDesiredComposeFromValue || Object.keys(userDesiredComposeFromValue).length === 0) {
		console.log('[PERSIST] No slot compose-from data to persist');
		return;
	}

	// Resolve contact IDs to public keys before persistence
	const composeFromData = resolveContactIdsInSlotComposition(userDesiredComposeFromValue);
	console.log('[PERSIST] Starting slot compose-from persistence...');

	try {
		// Store unwrapped data directly - Gun tracks timestamps via GUN.state.is()
		const composeFromJson = JSON.stringify(composeFromData);

		await safelyPersist('desiredSlotComposeFrom', composeFromJson, lastNetworkTimestamps.desiredSlotComposeFrom, (err) => {
			if (err) {
				console.error('[PERSIST] Error saving slot compose-from to Gun:', err);
			} else {
				console.log('[PERSIST] Slot compose-from successfully saved to Gun');
			}
		});
	} catch (error) {
		console.error('[PERSIST] Error serializing slot compose-from:', error);
	}
}

/**
 * Persist user's desired slot compose-into to Gun
 */
export async function persistUserDesiredSlotComposeInto() {
	// Import config dynamically to avoid circular dependencies
	const { USE_HOLSTER_COMPOSE } = await import('$lib/config');

	if (USE_HOLSTER_COMPOSE) {
		console.log('[PERSIST] Using Holster for compose - skipping Gun persistence');
		// Holster persistence happens in compose-holster.svelte.ts
		return;
	}

	if (!isUserInitialized()) {
		console.log('[PERSIST] User not initialized, skipping slot compose-into persistence');
		return;
	}

	if (get(isLoadingCapacities)) {
		console.log('[PERSIST] Capacities still loading, deferring slot compose-into persistence');
		setTimeout(() => {
			if (!get(isLoadingCapacities)) {
				persistUserDesiredSlotComposeInto();
			}
		}, 500);
		return;
	}

	const userDesiredComposeIntoValue = get(userDesiredSlotComposeInto);
	if (!userDesiredComposeIntoValue || Object.keys(userDesiredComposeIntoValue).length === 0) {
		console.log('[PERSIST] No slot compose-into data to persist');
		return;
	}

	// Resolve contact IDs to public keys before persistence
	const composeIntoData = resolveContactIdsInSlotComposition(userDesiredComposeIntoValue);
	console.log('[PERSIST] Starting slot compose-into persistence...');

	try {
		// Store unwrapped data directly - Gun tracks timestamps via GUN.state.is()
		const composeIntoJson = JSON.stringify(composeIntoData);

		await safelyPersist('desiredSlotComposeInto', composeIntoJson, lastNetworkTimestamps.desiredSlotComposeInto, (err) => {
			if (err) {
				console.error('[PERSIST] Error saving slot compose-into to Gun:', err);
			} else {
				console.log('[PERSIST] Slot compose-into successfully saved to Gun');
			}
		});
	} catch (error) {
		console.error('[PERSIST] Error serializing slot compose-into:', error);
	}
}

// DELETED: persistUserDesiredSlotClaims - Replaced by unified compose-from model
// Slot claims are now persisted as compose-from-self via persistUserDesiredSlotComposeFrom

/**
 * Persist provider allocation states to Gun (for network publishing)
 */
export async function persistProviderAllocationStates() {
	// Import config dynamically to avoid circular dependencies
	const { USE_HOLSTER_ALLOCATION_STATES } = await import('$lib/config');

	if (USE_HOLSTER_ALLOCATION_STATES) {
		console.log('[PERSIST] Using Holster for allocation states - skipping Gun persistence');
		// Holster persistence happens in allocation-states-holster.svelte.ts
		return;
	}

	// Check if user is initialized
	if (!isUserInitialized()) {
		console.log('[PERSIST] User not initialized, skipping provider allocation states persistence');
		return;
	}

	const allocationStatesValue = get(providerAllocationStates);
	if (!allocationStatesValue) {
		console.log('[PERSIST] No provider allocation states data to persist');
		return;
	}

	// Don't persist empty allocation states during initialization
	if (Object.keys(allocationStatesValue).length === 0) {
		console.log(
			'[PERSIST] Skipping persistence of empty allocation states (likely initialization)'
		);
		return;
	}

	console.log('[PERSIST] Starting provider allocation states persistence...');
	console.log(
		'[PERSIST] Allocation states for',
		Object.keys(allocationStatesValue).length,
		'capacities'
	);

	try {
		// Provider allocation states don't need timestamps since they're computed fresh each time
		// They represent the current state of the provider's allocation decisions
		const allocationStatesClone = structuredClone(allocationStatesValue);

		// Serialize to JSON to preserve number types
		const allocationStatesJson = JSON.stringify(allocationStatesClone);
		console.log(
			'[PERSIST] Serialized provider allocation states length:',
			allocationStatesJson.length
		);

		// Store in Gun under the expected path that network subscribers use
		user.get('allocationStates').put(allocationStatesJson, (ack: { err?: any }) => {
			if (ack.err) {
				console.error('[PERSIST] Error saving provider allocation states to Gun:', ack.err);
			} else {
				console.log('[PERSIST] Provider allocation states successfully saved to Gun');
			}
		});
	} catch (error) {
		console.error('[PERSIST] Error serializing provider allocation states:', error);
	}
}

/**
 * Persist user's contacts to Gun
 * Note: When USE_HOLSTER_CONTACTS is true, persistence happens in users.svelte.ts
 */
export async function persistContacts() {
	// Import config dynamically to avoid circular dependencies
	const { USE_HOLSTER_CONTACTS } = await import('$lib/config');

	if (USE_HOLSTER_CONTACTS) {
		console.log('[PERSIST] Using Holster for contacts - skipping Gun persistence');
		return;
	}

	if (!isUserInitialized()) {
		console.log('[PERSIST] User not initialized, skipping contacts persistence');
		return;
	}

	if (get(isLoadingContacts)) {
		console.log('[PERSIST] Skipping contacts persistence because contacts are being loaded');
		return;
	}

	const contactsValue = get(userContacts);

	if (!contactsValue || Object.keys(contactsValue).length === 0) {
		console.log('[PERSIST] No contacts to persist');
		return;
	}

	console.log('[PERSIST] Starting contacts persistence...');

	try {
		// Store unwrapped data directly - Gun tracks timestamps via GUN.state.is()
		const contactsJson = JSON.stringify(contactsValue);

		await safelyPersist('contacts', contactsJson, lastNetworkTimestamps.contacts, (err) => {
			if (err) {
				console.error('[PERSIST] Error saving contacts to Gun:', err);
			} else {
				console.log('[PERSIST] Contacts successfully saved to Gun');
			}
		});
	} catch (error) {
		console.error('[PERSIST] Error serializing contacts:', error);
	}
}

/**
 * Persist chat read states to Gun
 */
export async function persistChatReadStates() {
	// Import config dynamically to avoid circular dependencies
	const { USE_HOLSTER_CHAT_READ_STATES } = await import('$lib/config');

	if (USE_HOLSTER_CHAT_READ_STATES) {
		console.log('[PERSIST] Using Holster for chat read states - skipping Gun persistence');
		// Holster persistence happens in chat-read-states-holster.svelte.ts
		return;
	}

	if (!isUserInitialized()) {
		console.log('[PERSIST] User not initialized, skipping chat read states persistence');
		return;
	}

	if (get(isLoadingChatReadStates)) {
		console.log('[PERSIST] Skipping chat read states persistence because read states are being loaded');
		return;
	}

	const chatReadStatesValue = get(chatReadStates);

	if (!chatReadStatesValue || Object.keys(chatReadStatesValue).length === 0) {
		console.log('[PERSIST] No chat read states to persist');
		return;
	}

	console.log('[PERSIST] Starting chat read states persistence...');

	try {
		// Store unwrapped data directly - Gun tracks timestamps via GUN.state.is()
		const chatReadStatesJson = JSON.stringify(chatReadStatesValue);

		await safelyPersist('chatReadStates', chatReadStatesJson, lastNetworkTimestamps.chatReadStates, (err) => {
			if (err) {
				console.error('[PERSIST] Error saving chat read states to Gun:', err);
			} else {
				console.log('[PERSIST] Chat read states successfully saved to Gun');
			}
		});
	} catch (error) {
		console.error('[PERSIST] Error serializing chat read states:', error);
	}
}
