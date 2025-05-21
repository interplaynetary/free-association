import { get, writable, derived, type Writable, type Readable } from 'svelte/store';
import { user } from '$lib/gunSetup';
import type { RootNode, CapacitiesCollection } from '$lib/protocol/protocol';
import { createRootNode } from '$lib/protocol/protocol';

const userTree: Writable<RootNode | null> = writable(null);
const userSogf = writable(null);
const userProviderShares = writable({
	1: null,
	2: null,
	3: null
});
const userCapacities: Writable<CapacitiesCollection | null> = writable(null);

/**
 * Persist the current application state to Gun
 */
function persist() {
	try {
		// Store tree as JSON string
		const treeValue = get(userTree);
		if (treeValue) {
			user.get('tree').put(JSON.stringify(structuredClone(treeValue)));
		}

		// Store SOGF directly (already a simple object)
		const sogfValue = get(userSogf);
		if (sogfValue) {
			user.get('sogf').put(structuredClone(sogfValue));
		}

		// Store provider shares by depth
		const sharesValue = get(userProviderShares);
		if (sharesValue) {
			if (sharesValue[1])
				user.get('providerShares').get('first-degree').put(structuredClone(sharesValue[1]));
			if (sharesValue[2])
				user.get('providerShares').get('second-degree').put(structuredClone(sharesValue[2]));
			if (sharesValue[3])
				user.get('providerShares').get('third-degree').put(structuredClone(sharesValue[3]));
		}
		const userCapacitiesValue = get(userCapacities);
		if (userCapacitiesValue) {
			const snapshot = $state.snapshot(userCapacitiesValue);
			user.get('capacities').put(JSON.stringify(snapshot));
		}
		/*
		Original:
		const userCapacitiesValue = get(userCapacities);
		if (userCapacitiesValue) {
			user.get('capacities').put(JSON.stringify(structuredClone(snapsuserCapacitiesValue)));
		}

		*/
	} catch (error) {
		console.error('Error persisting data to Gun:', error);
	}
}

/**
 * Load the application state from Gun
 */
function manifest() {
	console.log('Manifest: Loading data from Gun');

	// Load tree with healing
	user.get('tree').once((treeData: any) => {
		const healedTree = healTree(treeData);
		if (healedTree) {
			userTree.set(healedTree);
			console.log('Tree loaded and healed successfully', healedTree);
		} else if (user.is?.pub) {
			// If healing returned null but we have user data, create a new tree
			console.log('No valid tree data found, creating initial tree');
			const newTree = createRootNode(user.is.pub, user.is?.alias || 'My Root', user.is.pub);
			user.get('tree').put(JSON.stringify(newTree));
			userTree.set(newTree);
		} else {
			console.log('No tree data found and no user to create one');
		}
	});

	// Load SOGF
	user.get('sogf').once((sogfData: any) => {
		if (sogfData) {
			userSogf.set(sogfData);
		}
	});

	// Load provider shares
	user
		.get('providerShares')
		.get('first-degree')
		.once((shareData: any) => {
			if (shareData) {
				userProviderShares.update((shares) => ({
					...shares,
					1: shareData
				}));
			}
		});

	user
		.get('providerShares')
		.get('second-degree')
		.once((shareData: any) => {
			if (shareData) {
				userProviderShares.update((shares) => ({
					...shares,
					2: shareData
				}));
			}
		});

	user
		.get('providerShares')
		.get('third-degree')
		.once((shareData: any) => {
			if (shareData) {
				userProviderShares.update((shares) => ({
					...shares,
					3: shareData
				}));
			}
		});

	user.get('capacities').once((capacitiesData: any) => {
		if (capacitiesData) {
			try {
				// Handle both stringified and object formats
				let parsedCapacities;
				if (typeof capacitiesData === 'object') {
					parsedCapacities = capacitiesData;
				} else {
					parsedCapacities = JSON.parse(capacitiesData);
				}
				userCapacities.set(parsedCapacities);
			} catch (err) {
				console.error('Error parsing capacities data:', err);
			}
		}
	});
}

/**
 * Validates and heals tree data to ensure it has a valid structure
 * @param treeData The raw tree data from Gun
 * @returns A valid RootNode, either from the provided data or newly created
 */
function healTree(treeData: any): RootNode | null {
	// Handle no data case
	if (!treeData) {
		console.log('No tree data provided to heal');
		return null;
	}

	try {
		// Parse if it's a string
		let parsedTree;
		if (typeof treeData === 'object') {
			console.log('Tree data is already an object', treeData);
			parsedTree = treeData;
		} else {
			console.log('Tree data is a string, parsing...', treeData);
			parsedTree = JSON.parse(treeData);
		}

		// Validate the tree structure
		if (!parsedTree || !parsedTree.id || !parsedTree.type || !Array.isArray(parsedTree.children)) {
			console.error('Invalid tree structure detected, needs healing:', parsedTree);

			// Create a new tree if user is available
			if (user.is?.pub) {
				console.log('Creating new tree with user data');
				parsedTree = createRootNode(user.is.pub, user.is?.alias || 'My Root', user.is.pub);

				// Save the healed tree back to Gun
				user.get('tree').put(JSON.stringify(parsedTree));
			} else {
				console.log('Cannot heal tree - no user data available');
				return null;
			}
		}

		return parsedTree as RootNode;
	} catch (err) {
		console.error('Error parsing or healing tree data:', err);

		// Create a recovery tree if possible
		if (user.is?.pub) {
			console.log('Creating recovery tree after error');
			const newTree = createRootNode(user.is.pub, user.is?.alias || 'My Root', user.is.pub);

			// Save the recovery tree
			user.get('tree').put(JSON.stringify(newTree));
			return newTree;
		}

		return null;
	}
}

// Export persistence functions
export { userTree, userSogf, userProviderShares, userCapacities, persist, manifest, healTree };
