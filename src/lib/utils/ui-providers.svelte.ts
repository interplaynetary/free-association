import { derived, get } from 'svelte/store';
// V5: Import from v5 stores and protocol
import { 
	myRecognitionTreeStore as userTree,
	myCapacitySlotsStore,
	networkCommitments,
	networkCapacitySlots,
	getNetworkCommitmentsRecord
} from '$lib/commons/v5/stores.svelte';
import { findNodeById, getDescendants } from '$lib/commons/v5/protocol';
import {
	userPubKeys,
	userNamesOrAliasesCache,
	userContacts,
	getUserAlias
} from '$lib/state/users.svelte';
import { userAliasesCache } from '$lib/state/users.svelte';
import type { Commitment, Node } from '$lib/commons/v5/schemas';

// V5: Helper to create nodesMap from tree
function createNodesMap(tree: Node | null): Record<string, Node> {
	if (!tree) return {};
	
	const map: Record<string, Node> = {};
	map[tree.id] = tree;
	
	const descendants = getDescendants(tree);
	descendants.forEach(node => {
		map[node.id] = node;
	});
	
	return map;
}

// V5: Derived nodesMap from userTree
const nodesMap = derived(userTree, ($tree) => createNodesMap($tree));

// Helper function to get display name for a user
function getDisplayName(userId: string, namesCache: Record<string, string>): string {
	if (namesCache[userId]) {
		return namesCache[userId];
	}
	// Fallback: truncate the pubkey intelligently
	// Gun pubkeys are in format: publicKey.signature
	const parts = userId.split('.');
	return (parts[0]?.substring(0, 12) || userId.substring(0, 12)) + '...';
}

// Simplified contacts and users data provider - no more complex merging logic
export function createContactsAndUsersDataProvider(excludeIds: string[] = []) {
	return derived(
		[userContacts, userPubKeys, userNamesOrAliasesCache, userAliasesCache],
		([$userContacts, $userIds, $userNamesCache, $userAliasesCache]) => {
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
						name: getDisplayName(userId, $userNamesCache),
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
				'[UI-PROVIDER-DEBUG] Generated items:',
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
		console.log('[UI-PROVIDER-DEBUG] createUsersDataProvider called:', {
			userIdsCount: $userIds?.length || 0,
			userIds: $userIds?.map((id) => id.slice(0, 20) + '...') || [],
			namesCacheCount: Object.keys($userNamesCache || {}).length,
			namesCacheKeys: Object.keys($userNamesCache || {}).map((id) => id.slice(0, 20) + '...'),
			excludeIds: excludeIds.map((id) => id.slice(0, 20) + '...')
		});

		if (!$userIds) {
			console.log('[UI-PROVIDER-DEBUG] No userIds, returning empty array');
			return [];
		}

		// Show all users we have cached names for (both online and offline)
		const allUserIds = [...new Set([...$userIds, ...Object.keys($userNamesCache)])];
		console.log('[UI-PROVIDER-DEBUG] Combined user IDs:', {
			allUserIdsCount: allUserIds.length,
			allUserIds: allUserIds.map((id) => id.slice(0, 20) + '...')
		});

		const result = allUserIds
			.filter((userId) => !excludeIds.includes(userId))
			.map((userId) => ({
				id: userId,
				name: getDisplayName(userId, $userNamesCache),
				metadata: { userId }
			}));

		console.log('[UI-PROVIDER-DEBUG] Final users result:', {
			resultCount: result.length,
			results: result.map((r) => ({
				id: r.id.slice(0, 20) + '...',
				name: r.name
			}))
		});

		return result;
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
// V5: Uses myCapacitySlotsStore (slots are the new capacities)
export function createCapacitiesDataProvider(excludeCapacityId?: string) {
	return derived(
		[myCapacitySlotsStore, userNamesOrAliasesCache],
		([$myCapacitySlots, $userNamesCache]) => {
			if (!$myCapacitySlots || $myCapacitySlots.length === 0) {
				return [];
			}

			// V5: Each slot is essentially a capacity
			const items = $myCapacitySlots
				.filter((slot) => slot.id !== excludeCapacityId)
				.map((slot) => ({
					id: slot.id,
					name: `${slot.emoji || '🎁'} ${slot.name}`,
					metadata: slot
				}));

			return items;
		}
	);
}

// Simple reactive all network capacities data provider
// V5: Uses networkCapacitySlots from v5 stores
export function createAllNetworkCapacitiesDataProvider(excludeCapacityId?: string) {
	return derived(
		[networkCapacitySlots, userNamesOrAliasesCache],
		([$networkCapacitySlots, $userNamesCache]) => {
			if (!$networkCapacitySlots) {
				return [];
			}

			// V5: networkCapacitySlots is a map of pubKey -> capacity_slots[]
			const items: Array<{ id: string; name: string; metadata: any }> = [];

			// Get all commitments to access capacity slots
			const commitments = getNetworkCommitmentsRecord();
			
			Object.entries(commitments).forEach(([userId, commitment]) => {
				if (!commitment.capacity_slots) return;
				
				const providerName = getDisplayName(userId, $userNamesCache);

				commitment.capacity_slots.forEach((slot) => {
					// Skip excluded capacity
					if (slot.id === excludeCapacityId) return;

					items.push({
						id: slot.id,
						name: `${slot.emoji || '🎁'} ${slot.name} (${providerName})`,
						metadata: {
							...slot,
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
	// Create the base provider ONCE, outside the derived
	const baseProvider = createContactsAndUsersDataProvider(excludeIds);

	return derived([baseProvider, userTree], ([$baseItems, $userTree]) => {
		// Get the contributors of the specific child node
		// V5: contributors are now {id, points} objects
		let childContributors: string[] = [];
		if ($userTree && childNodeId) {
			const childNode = findNodeById($userTree, childNodeId);
			if (childNode && childNode.type === 'NonRootNode') {
				childContributors = (childNode as any).contributors?.map((c: any) => c.id) || [];
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
	});
}

// Simple reactive slots data provider for composition
// V5: Uses myCapacitySlotsStore (slots are first-class in v5)
export function createSlotsDataProvider(capacityId?: string, excludeSlotIds: string[] = []) {
	return derived(
		[myCapacitySlotsStore, userNamesOrAliasesCache],
		([$myCapacitySlots, $userNamesCache]) => {
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

			// V5: Add slots from user's own capacity slots
			if ($myCapacitySlots && $myCapacitySlots.length > 0) {
				$myCapacitySlots.forEach((slot) => {
					if (capacityId && slot.id !== capacityId) return;
					if (excludeSlotIds.includes(slot.id)) return;

					const { timeInfo, location } = formatSlotInfo(slot);
					const displayName = `${slot.emoji || '🎁'} ${slot.name} - ${timeInfo}`;

					items.push({
						id: slot.id,
						name: displayName,
						metadata: {
							capacityId: slot.id,  // V5: slot is the capacity
							slotId: slot.id,
							capacityName: slot.name,
							quantity: slot.quantity || 0,
							location,
							timeInfo,
							isOwned: true
						}
					});
				});
			}

			// V5 TODO: Add slots from network capacities (allocated shares)
			// This should be integrated when v5 allocation algorithm is implemented
			// For now, only showing own capacity slots

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
// V5: TODO - integrate with v5 allocation algorithm
export function createAllocatedSlotsDataProvider(excludeSlotIds: string[] = []) {
	return derived(
		[myCapacitySlotsStore, userNamesOrAliasesCache],
		([$myCapacitySlots, $userNamesCache]) => {
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

			// V5 TODO: This function needs v5 allocation algorithm integration
			// For now, return empty array (allocated slots will be shown when allocation is implemented)
			if (!$myCapacitySlots) return items;

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

// Create composition targets data provider (slots + pubkeys + collectives)
// V5: Updated to use myCapacitySlotsStore
export function createCompositionTargetsDataProvider(excludeTargets: string[] = []) {
	return derived(
		[
			myCapacitySlotsStore,
			userNamesOrAliasesCache,
			userAliasesCache,
			userPubKeys
		],
		([
			$myCapacitySlots,
			$userNamesCache,
			$userAliasesCache,
			$userPubKeys
		]) => {
			const items: Array<{
				id: string;
				name: string;
				metadata: {
					type: 'slot' | 'pubkey' | 'collective';
					capacityId?: string;
					slotId?: string;
					capacityName?: string;
					providerId?: string;
					providerName?: string;
					quantity?: number;
					location?: string;
					timeInfo?: string;
					isOwned?: boolean;
					userId?: string;
				};
			}> = [];

			// Track seen items to avoid duplicates
			const seenItems = new Set<string>();

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

			// V5: Add slots from user's own capacity slots
			if ($myCapacitySlots && $myCapacitySlots.length > 0) {
				$myCapacitySlots.forEach((slot) => {
					const slotKey = slot.id;
					if (excludeTargets.includes(slotKey) || seenItems.has(slotKey)) return;

					seenItems.add(slotKey);

					const { timeInfo, location } = formatSlotInfo(slot);
					const displayName = `🕒 ${slot.emoji || '🎁'} ${slot.name} - ${timeInfo}`;

					items.push({
						id: slotKey,
						name: displayName,
						metadata: {
							type: 'slot',
							capacityId: slot.id,  // V5: slot is the capacity
							slotId: slot.id,
							capacityName: slot.name,
							quantity: slot.quantity || 0,
							location,
							timeInfo,
							isOwned: true
						}
					});
				});
			}

			// V5 TODO: Add slots from network capacities (allocated shares)
			// This should be integrated when v5 allocation algorithm is implemented

			// Add pubkey targets (users/people)
			if ($userPubKeys) {
				$userPubKeys.forEach((userId) => {
					if (excludeTargets.includes(userId) || seenItems.has(userId)) return;

					seenItems.add(userId);

					const userName =
						$userAliasesCache[userId] || $userNamesCache[userId] || `${userId.substring(0, 8)}...`;
					const displayName = `👤 ${userName}`;

					items.push({
						id: userId,
						name: displayName,
						metadata: {
							type: 'pubkey',
							userId,
							providerName: userName
						}
					});
				});
			}

			// Sort: slots first, then pubkeys, then alphabetically
			return items.sort((a, b) => {
				// Type priority: slots first, then pubkeys
				if (a.metadata.type === 'slot' && b.metadata.type !== 'slot') return -1;
				if (a.metadata.type !== 'slot' && b.metadata.type === 'slot') return 1;

				// Within same type, sort alphabetically
				return a.name.localeCompare(b.name);
			});
		}
	);
}
