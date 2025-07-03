import { get } from 'svelte/store';
import { usersList, userTree, userpub, username } from '$lib/state.svelte';
import { gun } from '$lib/state/gun.svelte';
import { createRootNode } from '$lib/protocol';
import { populateWithExampleData } from '$lib/examples/example';

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
			const userIds = Object.keys(data);
			console.log(`[PRUNE] Found ${userIds.length} users to check`);

			let prunedCount = 0;
			let checkedCount = 0;

			userIds.forEach((userId) => {
				usersList.get(userId).once((userData: any) => {
					checkedCount++;

					if (userData && userData.lastSeen) {
						const lastSeen = userData.lastSeen;

						if (lastSeen < thresholdTime) {
							usersList.get(userId).put(null, (ack: any) => {
								if (ack.err) {
									console.error(`[PRUNE] Error removing inactive user ${userId}:`, ack.err);
								} else {
									console.log(
										`[PRUNE] Removed inactive user ${userId} (last seen: ${new Date(lastSeen).toLocaleString()})`
									);
									prunedCount++;
								}
							});
						} else {
							console.log(
								`[PRUNE] User ${userId} is active (last seen: ${new Date(lastSeen).toLocaleString()})`
							);
						}
					} else {
						console.log(`[PRUNE] User ${userId} has no lastSeen data, removing...`);
						usersList.get(userId).put(null, (ack: any) => {
							if (ack.err) {
								console.error(`[PRUNE] Error removing user without lastSeen ${userId}:`, ack.err);
							} else {
								console.log(`[PRUNE] Removed user without lastSeen data: ${userId}`);
								prunedCount++;
							}
						});
					}

					if (checkedCount === userIds.length) {
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

		const userIds = Object.keys(usersData).filter((id) => id !== '_');
		console.log(`[USERS-FIX] Found ${userIds.length} users to check for corrupted names`);

		let checkedCount = 0;
		let fixedCount = 0;

		userIds.forEach((userId) => {
			const userData = usersData[userId];

			if (!userData || !userData.name) {
				console.log(`[USERS-FIX] User ${userId} has no name data, skipping`);
				checkedCount++;
				return;
			}

			// Check if the name is corrupted (equals the userId/pubkey)
			const isCorrupted = userData.name === userId;

			if (isCorrupted) {
				console.log(
					`[USERS-FIX] Found corrupted name for user ${userId.substring(0, 20)}... (name equals pubkey)`
				);

				// Try to get the correct alias from their protected space using Gun's user system
				gun
					.user(userId) // Use Gun's user system
					.get('alias')
					.once((alias: any) => {
						checkedCount++;

						if (alias && typeof alias === 'string' && alias !== userId) {
							console.log(
								`[USERS-FIX] Fixing user ${userId.substring(0, 20)}... with correct alias: ${alias}`
							);

							// Update the usersList with the correct name
							usersList.get(userId).put(
								{
									...userData,
									name: alias,
									lastSeen: Date.now(), // Update timestamp to show it was fixed
									fixed: true // Mark as fixed
								},
								(ack: any) => {
									if (ack.err) {
										console.error(`[USERS-FIX] Error fixing user ${userId}:`, ack.err);
									} else {
										console.log(`[USERS-FIX] Successfully fixed user ${userId} -> ${alias}`);
										fixedCount++;
									}

									// Log completion when all users have been processed
									if (checkedCount === userIds.length) {
										console.log(
											`[USERS-FIX] Completed fixing corrupted names. Checked ${checkedCount} users, fixed ${fixedCount} corrupted names.`
										);
									}
								}
							);
						} else {
							console.log(
								`[USERS-FIX] Could not get valid alias for user ${userId.substring(0, 20)}..., keeping current name`
							);

							// Log completion when all users have been processed
							if (checkedCount === userIds.length) {
								console.log(
									`[USERS-FIX] Completed fixing corrupted names. Checked ${checkedCount} users, fixed ${fixedCount} corrupted names.`
								);
							}
						}
					});
			} else {
				console.log(
					`[USERS-FIX] User ${userId.substring(0, 20)}... has valid name: ${userData.name}`
				);
				checkedCount++;

				// Log completion when all users have been processed
				if (checkedCount === userIds.length) {
					console.log(
						`[USERS-FIX] Completed fixing corrupted names. Checked ${checkedCount} users, fixed ${fixedCount} corrupted names.`
					);
				}
			}
		});

		// Handle case where no users need checking
		if (userIds.length === 0) {
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

	const currentUserPub = get(userpub);
	const currentUsername = get(username);

	if (!currentUserPub || !currentUsername) {
		console.error('[TREE-CREATE] Cannot create tree - user not authenticated');
		return;
	}

	// Create a new root node
	const newTree = createRootNode(currentUserPub, currentUsername);

	// Optionally populate with example data
	if (includeExampleData) {
		console.log('[TREE-CREATE] Populating new tree with example data');
		populateWithExampleData(newTree);
	}

	// Set the new tree
	userTree.set(newTree);

	console.log(`[TREE-CREATE] New tree created and set with ${newTree.children.length} child nodes`);

	return newTree;
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

// Expose to window for debugging
if (typeof window !== 'undefined') {
	(window as any).clearUsersList = clearUsersList;
	(window as any).changeTreeName = changeTreeName;
	(window as any).fixCorruptedUserListNames = fixCorruptedUserListNames;
	(window as any).createNewTree = createNewTree;
	(window as any).updateTreeFromJson = updateTreeFromJson;
	console.log(
		'[DEBUG] clearUsersList, changeTreeName, fixCorruptedUserListNames, createNewTree, and updateTreeFromJson functions exposed to window'
	);
}
