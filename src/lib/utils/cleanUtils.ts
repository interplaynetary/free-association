import { get } from 'svelte/store';
import { gun, usersList, userPub, userAlias } from '$lib/state/gun.svelte';
import { userContacts, resolveToPublicKey } from '$lib/state/users.svelte';
import { userTree } from '$lib/state/core.svelte';
import { createRootNode } from '$lib/protocol';
import { populateWithExampleData } from '$lib/utils/example';
import type { Node, NonRootNode } from '$lib/schema';

/**
 * Convert a string duration like '30m', '2h', '1d', '1w', '3mo' to milliseconds
 * @param {string} durationStr
 * @returns {number} Duration in milliseconds
 */
function parseDuration(durationStr: string): number {
	const durationRegex = /^(\d+)(s|m|h|d|w|mo)$/i;
	const match = durationStr.match(durationRegex);

	if (!match) {
		throw new Error(`Invalid duration format: '${durationStr}'`);
	}

	const value = parseInt(match[1], 10);
	const unit = match[2].toLowerCase();

	const unitToMs: Record<string, number> = {
		s: 1000,
		m: 60 * 1000,
		h: 60 * 60 * 1000,
		d: 24 * 60 * 60 * 1000,
		w: 7 * 24 * 60 * 60 * 1000,
		mo: 30 * 24 * 60 * 60 * 1000 // Approximate month as 30 days
	};

	return value * unitToMs[unit];
}

/**
 * Prune users from the usersList who haven't been seen within the specified duration
 * @param {string} inactivityThreshold - Duration string like '30m', '1h', '2d', etc.
 */
export function clearUsersList(inactivityThreshold: string = '30m') {
	let inactivityThresholdMs: number;
	try {
		inactivityThresholdMs = parseDuration(inactivityThreshold);
	} catch (error) {
		console.error(`[PRUNE] ${error}`);
		return;
	}

	console.log(`[PRUNE] Starting to prune users inactive for more than ${inactivityThreshold}...`);

	const thresholdTime = Date.now() - inactivityThresholdMs;

	usersList.once((data: any) => {
		if (data) {
			const userPubKeys = Object.keys(data);
			console.log(`[PRUNE] Found ${userPubKeys.length} users to check`);

			let prunedCount = 0;
			let checkedCount = 0;

			userPubKeys.forEach((pubKey) => {
				usersList.get(pubKey).once((userData: any) => {
					checkedCount++;

					if (userData && userData.lastSeen) {
						const lastSeen = userData.lastSeen;

						if (lastSeen < thresholdTime) {
							usersList.get(pubKey).put(null, (ack: any) => {
								if (ack.err) {
									console.error(`[PRUNE] Error removing inactive user ${pubKey}:`, ack.err);
								} else {
									console.log(
										`[PRUNE] Removed inactive user ${pubKey} (last seen: ${new Date(lastSeen).toLocaleString()})`
									);
									prunedCount++;
								}
							});
						} else {
							console.log(
								`[PRUNE] User ${pubKey} is active (last seen: ${new Date(lastSeen).toLocaleString()})`
							);
						}
					} else {
						console.log(`[PRUNE] User ${pubKey} has no lastSeen data, removing...`);
						usersList.get(pubKey).put(null, (ack: any) => {
							if (ack.err) {
								console.error(`[PRUNE] Error removing user without lastSeen ${pubKey}:`, ack.err);
							} else {
								console.log(`[PRUNE] Removed user without lastSeen data: ${pubKey}`);
								prunedCount++;
							}
						});
					}

					if (checkedCount === userPubKeys.length) {
						console.log(
							`[PRUNE] Pruning complete. Checked ${checkedCount} users, removed ${prunedCount} inactive users.`
						);
					}
				});
			});
		} else {
			console.log('[PRUNE] No users found to prune');
		}
	});
}

/**
 * Change the name of the user's tree root node
 * @param {string} newName - The new name for the tree
 */
export function changeTreeName(newName: string) {
	console.log(`[TREE-NAME] Changing tree name to: ${newName}`);

	const currentTree = get(userTree);
	if (!currentTree) {
		console.error('[TREE-NAME] No tree found to rename');
		return;
	}

	// Update the tree name
	const updatedTree = {
		...currentTree,
		name: newName
	};

	// Update the store
	userTree.set(updatedTree);

	console.log(`[TREE-NAME] Tree name changed to: ${newName}`);
}

/**
 * Fix corrupted names in the usersList where names were saved as public keys
 * This function checks each user in the usersList and corrects names that match their pubkey
 */
export function fixCorruptedUserListNames() {
	console.log('[USERS-FIX] Starting to fix corrupted names in usersList...');

	// Get current usersList data
	usersList.once((usersData: any) => {
		if (!usersData) {
			console.log('[USERS-FIX] No users data found');
			return;
		}

		const userPubKeys = Object.keys(usersData).filter((pubkey) => pubkey !== '_');
		console.log(`[USERS-FIX] Found ${userPubKeys.length} users to check for corrupted names`);

		let checkedCount = 0;
		let fixedCount = 0;

		userPubKeys.forEach((pubKey) => {
			const userData = usersData[pubKey];

			if (!userData || !userData.name) {
				console.log(`[USERS-FIX] User ${pubKey} has no name data, skipping`);
				checkedCount++;
				return;
			}

			// Check if the name is corrupted (equals the userId/pubkey)
			const isCorrupted = userData.name === pubKey;

			if (isCorrupted) {
				console.log(
					`[USERS-FIX] Found corrupted name for user ${pubKey.substring(0, 20)}... (name equals pubkey)`
				);

				// Try to get the correct alias from their protected space using Gun's user system
				gun
					.user(pubKey) // Use Gun's user system
					.get('alias')
					.once((alias: any) => {
						checkedCount++;

						if (alias && typeof alias === 'string' && alias !== pubKey) {
							console.log(
								`[USERS-FIX] Fixing user ${pubKey.substring(0, 20)}... with correct alias: ${alias}`
							);

							// Update the usersList with the correct name
							usersList.get(pubKey).put(
								{
									...userData,
									alias: alias,
									lastSeen: Date.now(), // Update timestamp to show it was fixed
									fixed: true // Mark as fixed
								},
								(ack: any) => {
									if (ack.err) {
										console.error(`[USERS-FIX] Error fixing user ${pubKey}:`, ack.err);
									} else {
										console.log(`[USERS-FIX] Successfully fixed user ${pubKey} -> ${alias}`);
										fixedCount++;
									}

									// Log completion when all users have been processed
									if (checkedCount === userPubKeys.length) {
										console.log(
											`[USERS-FIX] Completed fixing corrupted names. Checked ${checkedCount} users, fixed ${fixedCount} corrupted names.`
										);
									}
								}
							);
						} else {
							console.log(
								`[USERS-FIX] Could not get valid alias for user ${pubKey.substring(0, 20)}..., keeping current name`
							);

							// Log completion when all users have been processed
							if (checkedCount === userPubKeys.length) {
								console.log(
									`[USERS-FIX] Completed fixing corrupted names. Checked ${checkedCount} users, fixed ${fixedCount} corrupted names.`
								);
							}
						}
					});
			} else {
				console.log(
					`[USERS-FIX] User ${pubKey.substring(0, 20)}... has valid alias: ${userData.alias}`
				);
				checkedCount++;

				// Log completion when all users have been processed
				if (checkedCount === userPubKeys.length) {
					console.log(
						`[USERS-FIX] Completed fixing corrupted names. Checked ${checkedCount} users, fixed ${fixedCount} corrupted names.`
					);
				}
			}
		});

		// Handle case where no users need checking
		if (userPubKeys.length === 0) {
			console.log('[USERS-FIX] No users to check');
		}
	});
}

/**
 * Create a new tree and set it as the user's current tree
 * @param {boolean} includeExampleData - Whether to populate with example data (default: true)
 */
export function createNewTree(includeExampleData: boolean = true) {
	console.log('[TREE-CREATE] Creating new tree...');

	const currentUserPub = get(userPub);
	const currentUsername = get(userAlias);

	if (!currentUserPub || !currentUsername) {
		console.error('[TREE-CREATE] Cannot create tree - user not authenticated');
		return;
	}

	// Create a new root node
	const newTree = createRootNode(currentUserPub, currentUsername);

	// Optionally populate with example data
	if (includeExampleData) {
		console.log('[TREE-CREATE] Populating new tree with example data');
		const populatedTree = populateWithExampleData(newTree);

		// Set the new tree
		userTree.set(populatedTree);

		console.log(`[TREE-CREATE] New tree created and set with ${populatedTree.children.length} child nodes`);
		return populatedTree;
	} else {
		// Set the new tree
		userTree.set(newTree);

		console.log(`[TREE-CREATE] New tree created and set with ${newTree.children.length} child nodes`);
		return newTree;
	}
}

/**
 * Update the user's tree from a JSON string
 * @param {string} jsonString - The JSON string containing the tree data
 */
export function updateTreeFromJson(jsonString: string) {
	console.log('[TREE-UPDATE] Updating tree from JSON...');

	if (!jsonString || typeof jsonString !== 'string') {
		console.error('[TREE-UPDATE] Invalid input: JSON string is required');
		return false;
	}

	try {
		// Parse the JSON string
		const parsedTree = JSON.parse(jsonString);

		// Basic validation - check if it looks like a valid tree structure
		if (!parsedTree || typeof parsedTree !== 'object') {
			console.error('[TREE-UPDATE] Invalid tree data: not an object');
			return false;
		}

		// Check for required root node properties
		if (!parsedTree.id || !parsedTree.name || !parsedTree.children) {
			console.error(
				'[TREE-UPDATE] Invalid tree data: missing required properties (id, name, children)'
			);
			return false;
		}

		// Check if children is an array
		if (!Array.isArray(parsedTree.children)) {
			console.error('[TREE-UPDATE] Invalid tree data: children must be an array');
			return false;
		}

		// Optional: Check if it's a root node (has pub property)
		if (parsedTree.pub) {
			console.log(
				`[TREE-UPDATE] Detected root node with pub: ${parsedTree.pub.substring(0, 20)}...`
			);
		}

		// Update the userTree store
		userTree.set(parsedTree);

		console.log(
			`[TREE-UPDATE] Tree successfully updated with ${parsedTree.children.length} child nodes`
		);
		console.log(`[TREE-UPDATE] Tree name: ${parsedTree.name}`);

		return true;
	} catch (error) {
		console.error('[TREE-UPDATE] Error parsing JSON:', error);
		return false;
	}
}

/**
 * Clean up orphaned contact_ids in the user's tree that no longer have corresponding contact data.
 * This function recursively traverses the tree and removes any contact_ids from contributor_ids arrays
 * that are not present in the userContacts store.
 */
export function cleanOrphanedContactIds() {
	console.log("[TREE-CLEAN] Starting to clean orphaned contact_ids in user's tree...");

	const currentTree = get(userTree);
	if (!currentTree) {
		console.error('[TREE-CLEAN] No tree found to clean');
		return false;
	}

	const userContactsData = get(userContacts);
	if (!userContactsData || Object.keys(userContactsData).length === 0) {
		console.log('[TREE-CLEAN] No userContacts data found, nothing to clean');
		return false;
	}

	const validContactIds = new Set(Object.keys(userContactsData));
	let cleanedCount = 0;
	let hasChanges = false;

	// Recursive function to clean contact_ids from a node and its children
	function cleanNodeContributors(node: Node): void {
		// Only NonRootNodes have contributor_ids
		if (node.type === 'NonRootNode') {
			const nonRootNode = node as NonRootNode;
			if (nonRootNode.contributor_ids && nonRootNode.contributor_ids.length > 0) {
				const originalLength = nonRootNode.contributor_ids.length;

				// Filter out orphaned contact_ids
				nonRootNode.contributor_ids = nonRootNode.contributor_ids.filter((contributorId) => {
					// If it's a contact_id, check if it still exists in userContacts
					if (contributorId.startsWith('contact_')) {
						const exists = validContactIds.has(contributorId);
						if (!exists) {
							console.log(
								`[TREE-CLEAN] Removing orphaned contact_id '${contributorId}' from node '${node.name}' (${node.id})`
							);
							cleanedCount++;
						}
						return exists;
					}
					// Keep all non-contact contributors (public keys)
					return true;
				});

				// Check if any contributors were removed
				if (nonRootNode.contributor_ids.length < originalLength) {
					hasChanges = true;
				}
			}
		}

		// Recursively clean all child nodes
		if (node.children && node.children.length > 0) {
			node.children.forEach(cleanNodeContributors);
		}
	}

	// Start cleaning from the root
	cleanNodeContributors(currentTree);

	// Update the tree if there were changes
	if (hasChanges) {
		console.log(`[TREE-CLEAN] Cleaned ${cleanedCount} orphaned contact_ids from tree, updating...`);
		userTree.set(currentTree);
		console.log('[TREE-CLEAN] Tree updated with cleaned contact_ids');
		return true;
	} else {
		console.log('[TREE-CLEAN] No orphaned contact_ids found, tree is clean');
		return false;
	}
}

/**
 * Deduplicate contributors in the user's tree where the same person appears as both contact ID and public key.
 * This function resolves contact IDs to public keys and removes duplicates.
 */
export function deduplicateContributors() {
	console.log("[TREE-DEDUP] Starting to deduplicate contributors in user's tree...");

	const currentTree = get(userTree);
	if (!currentTree) {
		console.error('[TREE-DEDUP] No tree found to deduplicate');
		return false;
	}

	const userContactsData = get(userContacts);
	if (!userContactsData || Object.keys(userContactsData).length === 0) {
		console.log('[TREE-DEDUP] No userContacts data found, nothing to deduplicate');
		return false;
	}

	let deduplicatedCount = 0;
	let hasChanges = false;

	// Recursive function to deduplicate contributors in a node and its children
	function deduplicateNodeContributors(node: Node): void {
		// Only NonRootNodes have contributor_ids
		if (node.type === 'NonRootNode') {
			const nonRootNode = node as NonRootNode;
			if (nonRootNode.contributor_ids && nonRootNode.contributor_ids.length > 0) {
				const originalLength = nonRootNode.contributor_ids.length;
				const originalContributors = [...nonRootNode.contributor_ids];

				// Create a set to track resolved public keys we've already seen
				const seenPublicKeys = new Set<string>();
				const deduplicatedContributors: string[] = [];

				// First pass: collect all contact IDs and their resolved public keys
				const contactIdToPublicKey = new Map<string, string>();
				const publicKeyToContactId = new Map<string, string>();

				originalContributors.forEach((contributorId) => {
					if (contributorId.startsWith('contact_')) {
						const resolvedPublicKey = resolveToPublicKey(contributorId);
						if (resolvedPublicKey) {
							contactIdToPublicKey.set(contributorId, resolvedPublicKey);
							publicKeyToContactId.set(resolvedPublicKey, contributorId);
						}
					}
				});

				// Second pass: deduplicate, preferring contact IDs over public keys
				originalContributors.forEach((contributorId) => {
					if (contributorId.startsWith('contact_')) {
						// This is a contact ID - resolve to public key to check for duplicates
						const resolvedPublicKey = resolveToPublicKey(contributorId);
						if (resolvedPublicKey) {
							if (!seenPublicKeys.has(resolvedPublicKey)) {
								seenPublicKeys.add(resolvedPublicKey);
								// Always prefer the contact ID over the public key
								deduplicatedContributors.push(contributorId);
							} else {
								// This public key was already seen - this is a duplicate contact
								console.log(
									`[TREE-DEDUP] Removing duplicate contact '${contributorId}' (resolves to '${resolvedPublicKey.substring(0, 20)}...') from node '${node.name}' (${node.id})`
								);
								deduplicatedCount++;
							}
						} else {
							// Contact ID couldn't be resolved - keep it anyway
							deduplicatedContributors.push(contributorId);
						}
					} else {
						// This is a public key - check if we have a contact ID for this person
						if (publicKeyToContactId.has(contributorId)) {
							// We have a contact ID for this person - remove the public key
							console.log(
								`[TREE-DEDUP] Removing public key '${contributorId.substring(0, 20)}...' in favor of contact ID '${publicKeyToContactId.get(contributorId)}' from node '${node.name}' (${node.id})`
							);
							deduplicatedCount++;
						} else {
							// No contact ID for this person - keep the public key if not already seen
							if (!seenPublicKeys.has(contributorId)) {
								seenPublicKeys.add(contributorId);
								deduplicatedContributors.push(contributorId);
							} else {
								// This is a duplicate public key
								console.log(
									`[TREE-DEDUP] Removing duplicate public key '${contributorId.substring(0, 20)}...' from node '${node.name}' (${node.id})`
								);
								deduplicatedCount++;
							}
						}
					}
				});

				// Update the contributor_ids array
				nonRootNode.contributor_ids = deduplicatedContributors;

				// Check if any contributors were removed
				if (nonRootNode.contributor_ids.length < originalLength) {
					hasChanges = true;
					console.log(
						`[TREE-DEDUP] Node '${node.name}' (${node.id}): ${originalLength} → ${nonRootNode.contributor_ids.length} contributors`
					);
				}
			}
		}

		// Recursively deduplicate all child nodes
		if (node.children && node.children.length > 0) {
			node.children.forEach(deduplicateNodeContributors);
		}
	}

	// Start deduplication from the root
	deduplicateNodeContributors(currentTree);

	// Update the tree if there were changes
	if (hasChanges) {
		console.log(
			`[TREE-DEDUP] Deduplicated ${deduplicatedCount} contributor entries from tree, updating...`
		);
		userTree.set(currentTree);
		console.log('[TREE-DEDUP] Tree updated with deduplicated contributors');
		return true;
	} else {
		console.log('[TREE-DEDUP] No duplicate contributors found, tree is clean');
		return false;
	}
}

// Expose to window for debugging
if (typeof window !== 'undefined') {
	(window as any).clearUsersList = clearUsersList;
	(window as any).changeTreeName = changeTreeName;
	(window as any).fixCorruptedUserListNames = fixCorruptedUserListNames;
	(window as any).createNewTree = createNewTree;
	(window as any).updateTreeFromJson = updateTreeFromJson;
	(window as any).cleanOrphanedContactIds = cleanOrphanedContactIds;
	(window as any).deduplicateContributors = deduplicateContributors;
	console.log(
		'[DEBUG] clearUsersList, changeTreeName, fixCorruptedUserListNames, createNewTree, updateTreeFromJson, cleanOrphanedContactIds, and deduplicateContributors functions exposed to window'
	);
}
