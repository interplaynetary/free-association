import { get } from 'svelte/store';
import type { UserSlotQuantities } from '$lib/schema';
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
import {
	userContacts,
	isLoadingContacts,
	resolveToPublicKey,
	resolveContactIdsInTree
} from './users.svelte';
import { userDesiredSlotComposeFrom, userDesiredSlotComposeInto } from './core.svelte';
import { chatReadStates, isLoadingChatReadStates } from './chat.svelte';
import { user, userPub } from './gun.svelte';
import { processCapacitiesLocations } from '$lib/utils/geocodingCache';
import type { Node, NonRootNode } from '$lib/schema';

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

		// Store in Gun
		user.get('tree').put(treeJson, (ack: { err?: any }) => {
			if (ack.err) {
				console.error('[PERSIST] Error saving tree to Gun:', ack.err);
			} else {
				console.log('[PERSIST] Tree successfully saved to Gun with resolved contact IDs');
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
		console.log('[PERSIST] SOGF data (already contains public keys):', sogfValue);

		try {
			// Our core stores now contain public keys by design, so no filtering needed
			// This prevents feedback loops between network and persistence layers
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
			console.error('[PERSIST] Error processing SOGF:', error);
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
		console.log('[PERSIST] Starting provider shares persistence...');
		console.log('[PERSIST] Provider shares (already contains public keys):', shares);

		// Our core stores now contain public keys by design, so no filtering needed
		user.get('providerShares').put(structuredClone(shares), (ack: { err?: any }) => {
			if (ack.err) {
				console.error('[PERSIST] Error saving provider shares to Gun:', ack.err);
			} else {
				console.log('[PERSIST] Provider shares successfully saved to Gun');
			}
		});
	}
}

export async function persistCapacities() {
	if (!isUserInitialized()) {
		console.log('[PERSIST] 🚨 DEBUG: User not initialized, skipping capacities persistence');
		return;
	}

	// 🚨 CRITICAL: Check if we're still loading data from network
	// If so, defer persistence to avoid overwriting incoming network data
	if (get(isLoadingCapacities)) {
		console.log(
			'[PERSIST] 🚨 DEBUG: Still loading capacities from network, deferring persistence to avoid race condition'
		);
		// Schedule retry after loading completes
		setTimeout(() => {
			if (!get(isLoadingCapacities)) {
				console.log('[PERSIST] 🚨 DEBUG: Loading completed, retrying deferred persistence');
				persistCapacities();
			}
		}, 500);
		return;
	}

	const userCapacitiesValue = get(userCapacities);
	if (!userCapacitiesValue) {
		console.log('[PERSIST] 🚨 DEBUG: No userCapacitiesValue, skipping persistence');
		return;
	}
	if (Object.keys(userCapacitiesValue).length === 0) {
		console.log(
			'[PERSIST] 🚨 DEBUG: Skipping persistence of empty capacities (likely initialization)'
		);
		return;
	}

	console.log('[PERSIST] Starting capacities persistence...');
	console.log('[PERSIST] Capacities count:', Object.keys(userCapacitiesValue).length);

	// 🚨 DEBUG: Check what location data exists before persistence
	console.log('[PERSIST] 🚨 DEBUG: Pre-persistence location data check:');
	Object.entries(userCapacitiesValue).forEach(([capacityId, capacity]: [string, any]) => {
		if (capacity.availability_slots && capacity.availability_slots.length > 0) {
			capacity.availability_slots.forEach((slot: any, slotIndex: number) => {
				const hasLocationData =
					slot.location_type === 'Specific' ||
					slot.latitude !== undefined ||
					slot.longitude !== undefined ||
					slot.street_address ||
					slot.city ||
					slot.state_province ||
					slot.postal_code ||
					slot.country;

				if (hasLocationData) {
					console.log(
						`[PERSIST] 🚨 DEBUG: Capacity ${capacityId} (${capacity.name}) slot ${slotIndex} HAS location data:`,
						{
							slot_id: slot.id,
							location_type: slot.location_type,
							coordinates: { lat: slot.latitude, lng: slot.longitude },
							address: {
								street: slot.street_address,
								city: slot.city,
								state: slot.state_province,
								postal: slot.postal_code,
								country: slot.country
							}
						}
					);
				} else {
					console.log(
						`[PERSIST] 🚨 DEBUG: Capacity ${capacityId} (${capacity.name}) slot ${slotIndex} has NO location data`
					);
				}
			});
		} else {
			console.log(
				`[PERSIST] 🚨 DEBUG: Capacity ${capacityId} (${capacity.name}) has no availability_slots`
			);
		}
	});

	try {
		// GEOCODING STEP: Process addresses and add coordinates before persistence
		console.log('[PERSIST] 🌍 Processing addresses for geocoding...');
		const capacitiesWithCoordinates = await processCapacitiesLocations(userCapacitiesValue);
		console.log('[PERSIST] 🌍 Geocoding processing completed');

		// Our core stores now contain public keys by design, so no filtering needed
		// This prevents feedback loops between network and persistence layers
		const capacitiesClone = structuredClone(capacitiesWithCoordinates);

		// 🚨 DEBUG: Check what location data exists after cloning
		console.log('[PERSIST] 🚨 DEBUG: Post-clone location data check:');
		Object.entries(capacitiesClone).forEach(([capacityId, capacity]: [string, any]) => {
			if (capacity.availability_slots && capacity.availability_slots.length > 0) {
				capacity.availability_slots.forEach((slot: any, slotIndex: number) => {
					const hasLocationData =
						slot.location_type === 'Specific' ||
						slot.latitude !== undefined ||
						slot.longitude !== undefined ||
						slot.street_address ||
						slot.city ||
						slot.state_province ||
						slot.postal_code ||
						slot.country;

					if (hasLocationData) {
						console.log(
							`[PERSIST] 🚨 DEBUG: CLONE - Capacity ${capacityId} slot ${slotIndex} HAS location data:`,
							{
								slot_id: slot.id,
								location_type: slot.location_type,
								coordinates: { lat: slot.latitude, lng: slot.longitude },
								address: {
									street: slot.street_address,
									city: slot.city,
									state: slot.state_province,
									postal: slot.postal_code,
									country: slot.country
								}
							}
						);
					}
				});
			}
		});

		console.log('[PERSIST] Saving capacities (already contains public keys):', capacitiesClone);

		// Then serialize to JSON
		const capacitiesJson = JSON.stringify(capacitiesClone);
		console.log('[PERSIST] Serialized capacities length:', capacitiesJson.length);

		// 🚨 DEBUG: Check what's in the JSON
		try {
			const parsedBack = JSON.parse(capacitiesJson);
			console.log('[PERSIST] 🚨 DEBUG: JSON serialization check - location data preserved?');
			Object.entries(parsedBack).forEach(([capacityId, capacity]: [string, any]) => {
				if (capacity.availability_slots && capacity.availability_slots.length > 0) {
					capacity.availability_slots.forEach((slot: any, slotIndex: number) => {
						const hasLocationData =
							slot.location_type === 'Specific' ||
							slot.latitude !== undefined ||
							slot.longitude !== undefined ||
							slot.street_address ||
							slot.city ||
							slot.state_province ||
							slot.postal_code ||
							slot.country;

						if (hasLocationData) {
							console.log(
								`[PERSIST] 🚨 DEBUG: JSON - Capacity ${capacityId} slot ${slotIndex} location data preserved:`,
								{
									slot_id: slot.id,
									location_type: slot.location_type,
									coordinates: { lat: slot.latitude, lng: slot.longitude },
									address: {
										street: slot.street_address,
										city: slot.city,
										state: slot.state_province,
										postal: slot.postal_code,
										country: slot.country
									}
								}
							);
						}
					});
				}
			});
		} catch (parseError) {
			console.error('[PERSIST] 🚨 DEBUG: Error parsing JSON back:', parseError);
		}

		// Store in Gun with ACK callback
		user.get('capacities').put(capacitiesJson, (ack: { err?: any }) => {
			if (ack.err) {
				console.error('[PERSIST] Error saving capacities to Gun:', ack.err);
			} else {
				console.log('[PERSIST] Capacities successfully saved to Gun');
				console.log(
					'[PERSIST] 🚨 DEBUG: Saved to Gun successfully - location data should be preserved'
				);
			}
		});
	} catch (error) {
		console.error('[PERSIST] Error processing capacities:', error);
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
 * Persist contributor capacity slot quantities to gun
 * This stores the actual discrete unit allocations per slot for each contributor
 */
export function persistContributorCapacitySlotQuantities(
	contributorSlotQuantities: Record<string, UserSlotQuantities>
) {
	// Check if user is initialized
	if (!isUserInitialized()) {
		console.log(
			'[PERSIST] User not initialized, skipping contributor capacity slot quantities persistence'
		);
		return;
	}

	const ourId = get(userPub);
	if (!ourId) {
		console.log(
			'[PERSIST] No user ID available, cannot persist contributor capacity slot quantities'
		);
		return;
	}

	console.log(
		'[PERSIST] Persisting contributor capacity slot quantities:',
		contributorSlotQuantities
	);

	// For each contributor, store their slot quantities under their path
	Object.entries(contributorSlotQuantities).forEach(([contributorId, slotQuantities]) => {
		// Store under capacitySlotQuantities/contributorId
		user
			.get('capacitySlotQuantities')
			.get(contributorId)
			.put(JSON.stringify(slotQuantities), (ack: any) => {
				if (ack.err) {
					console.error(
						`[PERSIST] Error persisting capacity slot quantities for contributor ${contributorId}:`,
						ack.err
					);
				} else {
					console.log(
						`[PERSIST] Successfully persisted capacity slot quantities for contributor ${contributorId}`
					);
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

	// 🚨 RACE CONDITION PROTECTION: Check if capacities are still loading
	// Slot compose data depends on capacities, so wait if they're still loading
	if (get(isLoadingCapacities)) {
		console.log(
			'[PERSIST] 🚨 DEBUG: Capacities still loading, deferring slot compose-from persistence to avoid race condition'
		);
		setTimeout(() => {
			if (!get(isLoadingCapacities)) {
				console.log(
					'[PERSIST] 🚨 DEBUG: Capacities loading completed, retrying deferred slot compose-from persistence'
				);
				persistUserDesiredSlotComposeFrom();
			}
		}, 500);
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

	// 🚨 RACE CONDITION PROTECTION: Check if capacities are still loading
	// Slot compose data depends on capacities, so wait if they're still loading
	if (get(isLoadingCapacities)) {
		console.log(
			'[PERSIST] 🚨 DEBUG: Capacities still loading, deferring slot compose-into persistence to avoid race condition'
		);
		setTimeout(() => {
			if (!get(isLoadingCapacities)) {
				console.log(
					'[PERSIST] 🚨 DEBUG: Capacities loading completed, retrying deferred slot compose-into persistence'
				);
				persistUserDesiredSlotComposeInto();
			}
		}, 500);
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
