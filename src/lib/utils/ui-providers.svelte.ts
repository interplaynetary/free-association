import { derived } from 'svelte/store';
import {
	userTree,
	nodesMap,
	userNetworkCapacitiesWithSlotQuantities,
	networkCapacities,
	userCapacities
} from '$lib/state/core.svelte';
import {
	userPubKeys,
	userNamesOrAliasesCache,
	userContacts,
	getUserAlias
} from '$lib/state/users.svelte';
import { allocatedSlotAmounts } from '$lib/state/core.svelte';
import { findNodeById } from '$lib/protocol';
import { userAliasesCache } from '$lib/state/users.svelte';

// Helper function to get display name for a user
function getDisplayName(userId: string, namesCache: Record<string, string>): string {
	return namesCache[userId] || `${userId.substring(0, 8)}...`;
}

// Simplified contacts and users data provider - no more complex merging logic
export function createContactsAndUsersDataProvider(excludeIds: string[] = []) {
	return derived(
		[userContacts, userPubKeys, userNamesOrAliasesCache, userAliasesCache],
		([$userContacts, $userIds, $userNamesCache, $userAliasesCache]) => {
			console.log('[UI-PROVIDER] Data update:', {
				contactsCount: $userContacts ? Object.keys($userContacts).length : 0,
				userIdsCount: $userIds ? $userIds.length : 0,
				namesCacheCount: $userNamesCache ? Object.keys($userNamesCache).length : 0,
				aliasesCacheCount: $userAliasesCache ? Object.keys($userAliasesCache).length : 0
			});
			const items: Array<{
				id: string;
				name: string;
				metadata: {
					userId: string;
					isContact: boolean;
					contactName?: string;
					gunAlias?: string;
				};
			}> = [];

			// Simple approach: Just add all contacts and users in one unified way
			const processedIds = new Set<string>();

			// Add all contacts first (they get priority)
			if ($userContacts) {
				Object.values($userContacts).forEach((contact) => {
					if (excludeIds.includes(contact.contact_id)) return;

					// Proactively load alias for contact if they have a public key
					if (contact.public_key && !$userAliasesCache[contact.public_key]) {
						getUserAlias(contact.public_key).catch(console.error);
					}

					items.push({
						id: contact.contact_id,
						name: contact.name,
						metadata: {
							userId: contact.public_key || contact.contact_id,
							isContact: true,
							contactName: contact.name,
							gunAlias: contact.public_key ? $userAliasesCache[contact.public_key] : undefined
						}
					});

					// Mark both the contact ID and public key as processed so we don't add them again
					processedIds.add(contact.contact_id); // Add the contact ID itself
					if (contact.public_key) {
						processedIds.add(contact.public_key); // Add the public key too
					}
				});
			}

			// Add all users that aren't already added as contacts
			if ($userIds) {
				const allUserIds = [...new Set([...$userIds, ...Object.keys($userNamesCache)])];

				allUserIds.forEach((userId) => {
					if (excludeIds.includes(userId)) return;
					if (processedIds.has(userId)) return; // Skip if already added as contact

					items.push({
						id: userId,
						name: $userNamesCache[userId] || userId,
						metadata: {
							userId,
							isContact: false,
							gunAlias: $userAliasesCache[userId]
						}
					});
				});
			}

			// Sort so contacts appear first, then alphabetically
			const sortedItems = items.sort((a, b) => {
				// Contacts first
				if (a.metadata.isContact && !b.metadata.isContact) return -1;
				if (!a.metadata.isContact && b.metadata.isContact) return 1;

				// Then alphabetically by name
				return a.name.localeCompare(b.name);
			});

			console.log(
				'[UI-PROVIDER] Generated items:',
				sortedItems.map((item) => ({
					id: item.id.substring(0, 12) + '...',
					name: item.name,
					isContact: item.metadata.isContact
				}))
			);

			return sortedItems;
		}
	);
}

// Simple reactive users data provider (backward compatibility)
export function createUsersDataProvider(excludeIds: string[] = []) {
	return derived([userPubKeys, userNamesOrAliasesCache], ([$userIds, $userNamesCache]) => {
		if (!$userIds) {
			return [];
		}

		// Show all users we have cached names for (both online and offline)
		const allUserIds = [...new Set([...$userIds, ...Object.keys($userNamesCache)])];

		return allUserIds
			.filter((userId) => !excludeIds.includes(userId))
			.map((userId) => ({
				id: userId,
				name: $userNamesCache[userId] || userId,
				metadata: { userId }
			}));
	});
}

// Simple reactive subtrees data provider
export function createSubtreesDataProvider() {
	return derived([userTree, nodesMap], ([$userTree, $nodesMap]) => {
		if (!$userTree || !$nodesMap) {
			return [];
		}

		const items: Array<{
			id: string;
			name: string;
			metadata: { contributorCount: number; contributors: string[] };
		}> = [];

		// Add all nodes from the tree as potential subtree filters
		Object.values($nodesMap).forEach((node) => {
			if (node.type === 'RootNode') return; // Skip root nodes

			items.push({
				id: node.id,
				name: node.name,
				metadata: {
					contributorCount: 0,
					contributors: []
				}
			});
		});

		return items;
	});
}

// Simple reactive capacities data provider
export function createCapacitiesDataProvider(excludeCapacityId?: string) {
	return derived(
		[userNetworkCapacitiesWithSlotQuantities, userNamesOrAliasesCache],
		([$userNetworkCapacitiesWithSlotQuantities, $userNamesCache]) => {
			if (!$userNetworkCapacitiesWithSlotQuantities) {
				return [];
			}

			// Convert to dropdown items
			const items = Object.entries($userNetworkCapacitiesWithSlotQuantities)
				.filter(([capacityId]) => capacityId !== excludeCapacityId)
				.map(([capacityId, capacity]) => {
					// Get provider name from capacity metadata
					const providerId = (capacity as any).owner_id || (capacity as any).provider_id;
					const providerName = providerId ? getDisplayName(providerId, $userNamesCache) : 'Unknown';

					return {
						id: capacityId,
						name: `${capacity.emoji || '🎁'} ${capacity.name} (${providerName})`,
						metadata: capacity
					};
				});

			return items;
		}
	);
}

// Simple reactive all network capacities data provider
export function createAllNetworkCapacitiesDataProvider(excludeCapacityId?: string) {
	return derived(
		[networkCapacities, userNamesOrAliasesCache],
		([$networkCapacities, $userNamesCache]) => {
			if (!$networkCapacities) {
				return [];
			}

			// Flatten all capacities from all users
			const items: Array<{ id: string; name: string; metadata: any }> = [];

			Object.entries($networkCapacities).forEach(([userId, userCapacities]) => {
				const providerName = getDisplayName(userId, $userNamesCache);

				Object.entries(userCapacities).forEach(([capacityId, capacity]) => {
					// Skip excluded capacity
					if (capacityId === excludeCapacityId) return;

					items.push({
						id: capacityId,
						name: `${capacity.emoji || '🎁'} ${capacity.name} (${providerName})`,
						metadata: {
							...capacity,
							owner_id: userId,
							provider_id: userId
						}
					});
				});
			});

			return items;
		}
	);
}

// Simple reactive contributors data provider
export function createContributorsDataProvider() {
	return derived([userPubKeys, userNamesOrAliasesCache], ([$userIds, $userNamesCache]) => {
		if (!$userIds) {
			return [];
		}

		return $userIds.map((userId) => ({
			id: userId,
			name: $userNamesCache[userId] || userId,
			metadata: { userId }
		}));
	});
}

/**
 * Get all shares for a specific user from all providers
 * @param userId The user ID to lookup shares for
 * @returns Map of provider ID to their provided capacities and shares
 */
export function getUserSharesFromAllProviders(userId: string) {
	// This function would be used to query the network for shares
	// from other users who have the current user as a recipient

	// For demo/local usage - just return empty object
	return {};
}

// Legacy aliases for backward compatibility (will be removed)
export const createReactiveCapacitiesDataProvider = createCapacitiesDataProvider;
export const createReactiveAllNetworkCapacitiesDataProvider =
	createAllNetworkCapacitiesDataProvider;

// Simplified child contributors data provider - builds on top of base provider
export function createChildContributorsDataProvider(
	childNodeId: string | null,
	excludeIds: string[] = []
) {
	return derived(
		[createContactsAndUsersDataProvider(excludeIds), userTree],
		([$baseItems, $userTree]) => {
			// Get the contributors of the specific child node
			let childContributors: string[] = [];
			if ($userTree && childNodeId) {
				const childNode = findNodeById($userTree, childNodeId);
				if (childNode && childNode.type === 'NonRootNode') {
					childContributors = (childNode as any).contributor_ids || [];
				}
			}

			// Enhance base items with contributor metadata and prioritized sorting
			return $baseItems
				.map((item) => ({
					...item,
					metadata: {
						...item.metadata,
						isChildContributor: childContributors.includes(item.id)
					}
				}))
				.sort((a, b) => {
					// Priority: child contributors first, then contacts, then alphabetically
					const aIsChildContributor = a.metadata?.isChildContributor || false;
					const bIsChildContributor = b.metadata?.isChildContributor || false;
					const aIsContact = a.metadata?.isContact || false;
					const bIsContact = b.metadata?.isContact || false;

					if (aIsChildContributor && !bIsChildContributor) return -1;
					if (!aIsChildContributor && bIsChildContributor) return 1;
					if (aIsContact && !bIsContact) return -1;
					if (!aIsContact && bIsContact) return 1;

					return a.name.localeCompare(b.name);
				});
		}
	);
}

// Simple reactive slots data provider for composition
export function createSlotsDataProvider(capacityId?: string, excludeSlotIds: string[] = []) {
	return derived(
		[userNetworkCapacitiesWithSlotQuantities, userCapacities, userNamesOrAliasesCache],
		([$userNetworkCapacitiesWithSlotQuantities, $userCapacities, $userNamesCache]) => {
			const items: Array<{
				id: string;
				name: string;
				metadata: {
					capacityId: string;
					slotId: string;
					capacityName: string;
					providerId?: string;
					providerName?: string;
					quantity: number;
					location?: string;
					timeInfo?: string;
					isOwned: boolean;
				};
			}> = [];

			// Helper function to format slot display info
			function formatSlotInfo(slot: any): { timeInfo: string; location: string } {
				const timeParts = [];

				if (slot.start_date) {
					timeParts.push(new Date(slot.start_date).toLocaleDateString());
				}

				if (!slot.all_day && slot.start_time) {
					timeParts.push(slot.start_time);
				}

				if (slot.all_day) {
					timeParts.push('All day');
				}

				const timeInfo = timeParts.length > 0 ? timeParts.join(' ') : 'No time set';

				let location = 'No location';
				if (slot.location_type === 'Specific') {
					if (slot.street_address) {
						location = slot.street_address;
					} else if (slot.latitude && slot.longitude) {
						location = `${slot.latitude.toFixed(4)}, ${slot.longitude.toFixed(4)}`;
					}
				} else if (slot.location_type) {
					location = slot.location_type;
				}

				return { timeInfo, location };
			}

			// Add slots from user's own capacities
			if ($userCapacities) {
				Object.entries($userCapacities).forEach(([capId, capacity]) => {
					if (capacityId && capId !== capacityId) return;

					if (capacity.availability_slots && Array.isArray(capacity.availability_slots)) {
						capacity.availability_slots.forEach((slot: any) => {
							if (excludeSlotIds.includes(slot.id)) return;

							const { timeInfo, location } = formatSlotInfo(slot);
							const displayName = `${capacity.emoji || '🎁'} ${capacity.name} - ${timeInfo}`;

							items.push({
								id: slot.id,
								name: displayName,
								metadata: {
									capacityId: capId,
									slotId: slot.id,
									capacityName: capacity.name,
									quantity: slot.quantity || 0,
									location,
									timeInfo,
									isOwned: true
								}
							});
						});
					}
				});
			}

			// Add slots from network capacities (shares)
			if ($userNetworkCapacitiesWithSlotQuantities) {
				Object.entries($userNetworkCapacitiesWithSlotQuantities).forEach(([capId, capacity]) => {
					if (capacityId && capId !== capacityId) return;

					const providerId = (capacity as any).provider_id;
					const providerName = providerId ? getDisplayName(providerId, $userNamesCache) : 'Unknown';

					if (capacity.availability_slots && Array.isArray(capacity.availability_slots)) {
						capacity.availability_slots.forEach((slot: any) => {
							if (excludeSlotIds.includes(slot.id)) return;

							const { timeInfo, location } = formatSlotInfo(slot);
							const displayName = `${capacity.emoji || '🎁'} ${capacity.name} (${providerName}) - ${timeInfo}`;

							items.push({
								id: slot.id,
								name: displayName,
								metadata: {
									capacityId: capId,
									slotId: slot.id,
									capacityName: capacity.name,
									providerId,
									providerName,
									quantity: slot.quantity || 0,
									location,
									timeInfo,
									isOwned: false
								}
							});
						});
					}
				});
			}

			// Sort by capacity name, then time
			return items.sort((a, b) => {
				const capCompare = a.metadata.capacityName.localeCompare(b.metadata.capacityName);
				if (capCompare !== 0) return capCompare;
				const aTime = a.metadata.timeInfo || '';
				const bTime = b.metadata.timeInfo || '';
				return aTime.localeCompare(bTime);
			});
		}
	);
}

// Get all allocated slots for a user (for compose-from scenarios)
export function createAllocatedSlotsDataProvider(excludeSlotIds: string[] = []) {
	return derived(
		[userNetworkCapacitiesWithSlotQuantities, userNamesOrAliasesCache],
		([$userNetworkCapacitiesWithSlotQuantities, $userNamesCache]) => {
			const items: Array<{
				id: string;
				name: string;
				metadata: {
					capacityId: string;
					slotId: string;
					capacityName: string;
					quantity: number;
					allocatedAmount: number;
					location?: string;
					timeInfo?: string;
				};
			}> = [];

			if (!$userNetworkCapacitiesWithSlotQuantities) return items;

			// Helper function to format slot display info (same as above)
			function formatSlotInfo(slot: any): { timeInfo: string; location: string } {
				const timeParts = [];

				if (slot.start_date) {
					timeParts.push(new Date(slot.start_date).toLocaleDateString());
				}

				if (!slot.all_day && slot.start_time) {
					timeParts.push(slot.start_time);
				}

				if (slot.all_day) {
					timeParts.push('All day');
				}

				const timeInfo = timeParts.length > 0 ? timeParts.join(' ') : 'No time set';

				let location = 'No location';
				if (slot.location_type === 'Specific') {
					if (slot.street_address) {
						location = slot.street_address;
					} else if (slot.latitude && slot.longitude) {
						location = `${slot.latitude.toFixed(4)}, ${slot.longitude.toFixed(4)}`;
					}
				} else if (slot.location_type) {
					location = slot.location_type;
				}

				return { timeInfo, location };
			}

			// Only include slots that are allocated (we have claimed amounts from efficient algorithm)
			Object.entries($userNetworkCapacitiesWithSlotQuantities).forEach(([capId, capacity]) => {
				if (capacity.availability_slots && Array.isArray(capacity.availability_slots)) {
					const providerId = (capacity as any).provider_id;
					const providerName = providerId ? getDisplayName(providerId, $userNamesCache) : 'Unknown';

					capacity.availability_slots.forEach((slot: any) => {
						if (excludeSlotIds.includes(slot.id)) return;

						// Check if this slot is allocated (we have some amount from efficient algorithm)
						const allocatedAmount = slot.allocated_quantity || 0;
						if (allocatedAmount <= 0) return;

						const { timeInfo, location } = formatSlotInfo(slot);
						const displayName = `${capacity.emoji || '🎁'} ${capacity.name} (${providerName}) - ${timeInfo} (${allocatedAmount} available)`;

						items.push({
							id: slot.id,
							name: displayName,
							metadata: {
								capacityId: capId,
								slotId: slot.id,
								capacityName: capacity.name,
								quantity: slot.available_quantity || slot.quantity || 0,
								allocatedAmount,
								location,
								timeInfo
							}
						});
					});
				}
			});

			// Sort by capacity name, then time
			return items.sort((a, b) => {
				const capCompare = a.metadata.capacityName.localeCompare(b.metadata.capacityName);
				if (capCompare !== 0) return capCompare;
				const aTime = a.metadata.timeInfo || '';
				const bTime = b.metadata.timeInfo || '';
				return aTime.localeCompare(bTime);
			});
		}
	);
}
