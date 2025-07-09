import { get, derived } from 'svelte/store';
import {
	userTree,
	nodesMap,
	userNetworkCapacitiesWithShares,
	networkCapacities
} from '$lib/state/core.svelte';
import {
	userPubKeys,
	userNamesOrAliasesCache,
	userContacts,
	getContactByPublicKey
} from '$lib/state/users.svelte';
import { getSubtreeContributorMap, findNodeById } from '$lib/protocol';

// Helper function to get display name for a user
function getDisplayName(userId: string, namesCache: Record<string, string>): string {
	return namesCache[userId] || `${userId.substring(0, 8)}...`;
}

// Contacts and users data provider - prioritizes contacts and adds metadata
export function createContactsAndUsersDataProvider(excludeIds: string[] = []) {
	return derived(
		[userContacts, userPubKeys, userNamesOrAliasesCache],
		([$userContacts, $userIds, $userNamesCache]) => {
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

			// First, add all contacts (these get priority)
			if ($userContacts) {
				Object.values($userContacts).forEach((contact) => {
					if (excludeIds.includes(contact.contact_id)) return;

					// For contacts, use the contact ID as the ID (this allows contact-based operations)
					// But include the public key in metadata for Gun operations
					items.push({
						id: contact.contact_id,
						name: contact.name,
						metadata: {
							userId: contact.public_key || contact.contact_id,
							isContact: true,
							contactName: contact.name,
							gunAlias: contact.public_key ? $userNamesCache[contact.public_key] : undefined
						}
					});
				});
			}

			// Then add all other users (online users, cached users) that aren't already added as contacts
			if ($userIds) {
				const contactPublicKeys = Object.values($userContacts || {})
					.map((contact) => contact.public_key)
					.filter(Boolean);

				const allUserIds = [...new Set([...$userIds, ...Object.keys($userNamesCache)])];

				allUserIds.forEach((userId) => {
					if (excludeIds.includes(userId)) return;

					// Skip if this user is already added as a contact
					if (contactPublicKeys.includes(userId)) return;

					// Check if we have a contact by public key (to avoid duplicates)
					const existingContact = getContactByPublicKey(userId);
					if (existingContact) return; // Already added above

					items.push({
						id: userId,
						name: $userNamesCache[userId] || userId,
						metadata: {
							userId,
							isContact: false,
							gunAlias: $userNamesCache[userId]
						}
					});
				});
			}

			// Sort so contacts appear first, then alphabetically
			return items.sort((a, b) => {
				// Contacts first
				if (a.metadata.isContact && !b.metadata.isContact) return -1;
				if (!a.metadata.isContact && b.metadata.isContact) return 1;

				// Then alphabetically by name
				return a.name.localeCompare(b.name);
			});
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
		[userNetworkCapacitiesWithShares, userNamesOrAliasesCache],
		([$userNetworkCapacitiesWithShares, $userNamesCache]) => {
			if (!$userNetworkCapacitiesWithShares) {
				return [];
			}

			// Convert to dropdown items
			const items = Object.entries($userNetworkCapacitiesWithShares)
				.filter(([capacityId]) => capacityId !== excludeCapacityId)
				.map(([capacityId, capacity]) => {
					// Get provider name from capacity metadata
					const providerId = (capacity as any).owner_id || (capacity as any).provider_id;
					const providerName = providerId ? getDisplayName(providerId, $userNamesCache) : 'Unknown';

					return {
						id: capacityId,
						name: `${capacity.emoji || '📦'} ${capacity.name} (${providerName})`,
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
						name: `${capacity.emoji || '📦'} ${capacity.name} (${providerName})`,
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

// Child contributors prioritized data provider - prioritizes contributors of specific node
export function createChildContributorsDataProvider(
	childNodeId: string,
	excludeIds: string[] = []
) {
	return derived(
		[userTree, userContacts, userPubKeys, userNamesOrAliasesCache],
		([$userTree, $userContacts, $userIds, $userNamesCache]) => {
			const items: Array<{
				id: string;
				name: string;
				metadata: {
					userId: string;
					isContact: boolean;
					isChildContributor: boolean;
					contactName?: string;
					gunAlias?: string;
				};
			}> = [];

			// First, get the contributors of the specific child node
			let childContributors: string[] = [];
			if ($userTree && childNodeId) {
				const childNode = findNodeById($userTree, childNodeId);
				if (childNode && childNode.type === 'NonRootNode') {
					childContributors = (childNode as any).contributor_ids || [];
				}
			}

			// Track which IDs we've already added to avoid duplicates
			const addedIds = new Set<string>();

			// FIRST PRIORITY: Add child contributors
			// Follow the same pattern as the original provider
			childContributors.forEach((contributorId) => {
				if (excludeIds.includes(contributorId)) return;

				// Check if this is a contact ID (direct lookup)
				if (contributorId.startsWith('contact_') && $userContacts?.[contributorId]) {
					const contact = $userContacts[contributorId];
					if (addedIds.has(contact.contact_id)) return;

					items.push({
						id: contact.contact_id,
						name: contact.name,
						metadata: {
							userId: contact.public_key || contact.contact_id,
							isContact: true,
							isChildContributor: true,
							contactName: contact.name,
							gunAlias: contact.public_key ? $userNamesCache[contact.public_key] : undefined
						}
					});
					addedIds.add(contact.contact_id);
					if (contact.public_key) addedIds.add(contact.public_key);
				}
				// Check if this is a public key that has a contact
				else {
					const existingContact = getContactByPublicKey(contributorId);
					if (existingContact) {
						if (addedIds.has(existingContact.contact_id)) return;

						items.push({
							id: existingContact.contact_id,
							name: existingContact.name,
							metadata: {
								userId: contributorId,
								isContact: true,
								isChildContributor: true,
								contactName: existingContact.name,
								gunAlias: $userNamesCache[contributorId]
							}
						});
						addedIds.add(existingContact.contact_id);
						addedIds.add(contributorId);
					} else {
						// No contact found, treat as regular user
						if (addedIds.has(contributorId)) return;

						items.push({
							id: contributorId,
							name: $userNamesCache[contributorId] || contributorId,
							metadata: {
								userId: contributorId,
								isContact: false,
								isChildContributor: true,
								gunAlias: $userNamesCache[contributorId]
							}
						});
						addedIds.add(contributorId);
					}
				}
			});

			// SECOND PRIORITY: Add remaining contacts (not already added as child contributors)
			if ($userContacts) {
				Object.values($userContacts).forEach((contact) => {
					if (excludeIds.includes(contact.contact_id) || addedIds.has(contact.contact_id)) return;

					items.push({
						id: contact.contact_id,
						name: contact.name,
						metadata: {
							userId: contact.public_key || contact.contact_id,
							isContact: true,
							isChildContributor: false,
							contactName: contact.name,
							gunAlias: contact.public_key ? $userNamesCache[contact.public_key] : undefined
						}
					});
					addedIds.add(contact.contact_id);
					if (contact.public_key) addedIds.add(contact.public_key);
				});
			}

			// THIRD PRIORITY: Add other users (not already added as child contributors or contacts)
			if ($userIds) {
				const allUserIds = [...new Set([...$userIds, ...Object.keys($userNamesCache)])];

				allUserIds.forEach((userId) => {
					if (excludeIds.includes(userId) || addedIds.has(userId)) return;

					// Check if we have a contact by public key (to avoid duplicates)
					const existingContact = getContactByPublicKey(userId);
					if (existingContact) return; // Already added above

					items.push({
						id: userId,
						name: $userNamesCache[userId] || userId,
						metadata: {
							userId,
							isContact: false,
							isChildContributor: false,
							gunAlias: $userNamesCache[userId]
						}
					});
					addedIds.add(userId);
				});
			}

			// Sort by priority: child contributors first, then contacts, then users, alphabetically within each group
			return items.sort((a, b) => {
				// Child contributors first
				if (a.metadata.isChildContributor && !b.metadata.isChildContributor) return -1;
				if (!a.metadata.isChildContributor && b.metadata.isChildContributor) return 1;

				// Within child contributors or non-child contributors, contacts next
				if (a.metadata.isContact && !b.metadata.isContact) return -1;
				if (!a.metadata.isContact && b.metadata.isContact) return 1;

				// Then alphabetically by name
				return a.name.localeCompare(b.name);
			});
		}
	);
}
