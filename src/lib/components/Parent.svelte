<script lang="ts">
	import { onMount } from 'svelte';
	import * as d3 from 'd3';
	import { userAlias, userPub, userTree } from '$lib/state.svelte';
	import { createChildContributorsDataProvider } from '$lib/utils/ui-providers.svelte';
	import { currentPath, globalState } from '$lib/global.svelte';
	import { type Node, type NonRootNode, type RootNode } from '$lib/schema';
	import {
		findNodeById,
		getParentNode,
		updateNodeById,
		updatePoints,
		updateName,
		fulfilled,
		addChild,
		addContributors,
		calculateNodePoints,
		getPathToNode
	} from '$lib/protocol';
	import { get } from 'svelte/store';
	import { createContact } from '$lib/state/users.svelte';
	import { updateContact, userContacts } from '$lib/state/users.svelte';
	import {
		deleteContact,
		getContactByPublicKey,
		resolveToPublicKey
	} from '$lib/state/users.svelte';
	import { createNewTree } from '$lib/utils/cleanUtils';
	import Child from '$lib/components/Child.svelte';
	import DropDown from '$lib/components/DropDown.svelte';
	import { browser } from '$app/environment';

	// Define a type for visualization data
	interface VisualizationNode {
		id: string;
		points: number;
		children: VisualizationNode[];
		nodeName?: string;
		contributors?: string[];
		fulfillment?: number;
		hasChildren?: boolean; // Flag to indicate if this node has children in the full tree
	}

	// UI state
	let labelIndex = $state(0);
	const nodeLabels = ['Node', 'Value', 'Goal', 'Dependency', 'Desire', 'Contribution'];

	// For tracking changes in d3 visualization
	let updateCounter = $state(0);

	// Function to manually trigger a UI update
	function triggerUpdate() {
		updateCounter++;
		console.log('[UI FLOW] Triggered update', updateCounter);
	}

	// UI interaction state
	let showUserDropdown = $state(false);
	let dropdownPosition = $state({ x: 0, y: 0 });
	let activeNodeId = $state<string | null>(null);

	// Create users data provider for the dropdown - simplified single provider
	let usersDataProvider = $derived.by(() => {
		// Always use the child contributors provider for consistency
		// It will handle cases where activeNodeId is null gracefully
		const provider = createChildContributorsDataProvider(activeNodeId, []);

		return provider;
	});

	// Get current node contributors for the dropdown
	let currentContributors = $derived.by(() => {
		if (!activeNodeId || !tree) return [];

		const node = findNodeById(tree, activeNodeId);
		if (!node || node.type !== 'NonRootNode') return [];

		return (node as NonRootNode).contributor_ids || [];
	});

	// Growth state
	let touchStartTime = $state(0);
	let isTouching = $state(false);
	let activeGrowthNodeId = $state<string | null>(null);
	let isGrowing = $state(false);
	let isShrinkingActive = $state(false);
	let growthInterval = $state<number | null>(null);
	let growthTimeout = $state<number | null>(null);

	// Growth constants
	const GROWTH_DELAY = 350;
	const GROWTH_TICK = 16;
	const BASE_GROWTH_RATE = 0.03;
	const BASE_SHRINK_RATE = -0.03;
	const TAP_THRESHOLD = 250;

	// Drag state (now using global state)
	let dragStartTime = $state(0);
	let initialPointerX = $state(0);
	let initialPointerY = $state(0);
	let hasMovedSignificantly = $state(false);

	// Movement threshold for distinguishing between tap and drag
	const MOVEMENT_THRESHOLD = 10; // pixels

	// Reactive store subscriptions
	const tree = $derived($userTree);
	const path = $derived($currentPath);
	const pub = $derived($userPub);

	// Helper function to get current node ID
	const currentNodeId = $derived.by(() => {
		// If the path is empty but we have a user pub, use that as the root
		if (path.length === 0 && pub) {
			return pub;
		}

		return path.length > 0 ? path[path.length - 1] : null;
	});

	// Get the current node
	const currentNode = $derived.by(() => {
		if (!tree && currentNodeId && pub && currentNodeId === pub) {
			// Initial load - wait for tree
			console.log('[UI FLOW] Waiting for tree to be loaded');
			return null;
		}

		if (!tree || !currentNodeId) return null;
		return findNodeById(tree, currentNodeId);
	});

	// Get child nodes
	const childNodes = $derived.by(() => {
		return currentNode ? currentNode.children : [];
	});

	// Format data for d3 to consume
	const packData = $derived.by(() => {
		if (!currentNode) {
			return { id: '', selfPoints: 0, children: [] };
		}

		const mappedChildren = childNodes.map((child) => ({
			id: child.id,
			points: child.type === 'NonRootNode' ? (child as NonRootNode).points : 0,
			nodeName: child.name,
			contributors: child.type === 'NonRootNode' ? (child as NonRootNode).contributor_ids : [],
			children: [] as VisualizationNode[],
			hasChildren: child.children.length > 0 // Check if this child has children in the full tree
		}));

		return {
			id: currentNode.id,
			selfPoints: currentNode.type === 'NonRootNode' ? (currentNode as NonRootNode).points : 0,
			children: mappedChildren
		};
	});

	// Create hierarchy for d3
	const hierarchyData = $derived.by(() => {
		// Force rerender when updateCounter changes
		updateCounter;

		const data = packData;

		// Create hierarchy
		const rootNode: VisualizationNode = {
			id: data.id,
			points: 0, // Don't include parent points in visualization
			children: data.children
		};

		console.log('[DEBUG] Creating hierarchy with data:', data);
		console.log('[DEBUG] Current tree state:', tree);
		console.log('[DEBUG] Current node:', currentNode);
		console.log(
			'[DEBUG] Children points:',
			data.children.map((c) => ({ name: c.nodeName, points: c.points }))
		);
		console.log(
			'[DEBUG] Original children from tree:',
			currentNode?.children?.map((c) => ({
				name: c.name,
				points: c.type === 'NonRootNode' ? c.points : 0
			}))
		);

		const hierarchy = d3.hierarchy<VisualizationNode>(rootNode, (d) => d.children);

		// Sum for sizing
		hierarchy.sum((d) => d.points);

		console.log(
			'[DEBUG] After sum - hierarchy values:',
			hierarchy.children?.map((c) => ({
				name: c.data.nodeName,
				points: c.data.points,
				value: c.value
			}))
		);

		// Sort by value for better layout
		hierarchy.sort((a, b) => b.value! - a.value!);

		console.log(
			'[DEBUG] After sort - hierarchy values:',
			hierarchy.children?.map((c) => ({
				name: c.data.nodeName,
				points: c.data.points,
				value: c.value
			}))
		);

		// Custom tile function to ensure nodes fill their container
		function customTile(
			node: d3.HierarchyRectangularNode<VisualizationNode>,
			x0: number,
			y0: number,
			x1: number,
			y1: number
		) {
			// Apply the standard binary tiling algorithm to a normalized space
			d3.treemapBinary(node, 0, 0, 1, 1);

			// Scale the output to fit the current viewport
			for (const child of node.children || []) {
				child.x0 = x0 + child.x0 * (x1 - x0);
				child.x1 = x0 + child.x1 * (x1 - x0);
				child.y0 = y0 + child.y0 * (y1 - y0);
				child.y1 = y0 + child.y1 * (y1 - y0);
			}
		}

		// Apply treemap with custom tiling
		const treemap = d3
			.treemap<VisualizationNode>()
			.tile(customTile)
			.size([1, 1])
			.padding(0.005)
			.round(false);

		const result = treemap(hierarchy);

		console.log(
			'[DEBUG] After treemap - final dimensions:',
			result.children?.map((c) => ({
				name: c.data.nodeName,
				points: c.data.points,
				value: c.value,
				width: (c.x1 - c.x0) * 100,
				height: (c.y1 - c.y0) * 100
			}))
		);

		return result;
	});

	onMount(() => {
		// Set up event listeners
		document.addEventListener('mouseup', handleGlobalTouchEnd);
		document.addEventListener('touchend', handleGlobalTouchEnd);
		document.addEventListener('touchcancel', handleGlobalTouchEnd);

		// Start label cycling interval
		const interval = setInterval(() => {
			labelIndex = (labelIndex + 1) % nodeLabels.length;
		}, 4000);

		// Setup a subscription to the userTree store to trigger updates
		const unsubscribe = userTree.subscribe((value) => {
			if (value) {
				console.log('[UI FLOW] userTree updated, triggering UI update');
				triggerUpdate();
			}
		});

		return () => {
			// Clean up timers and event listeners
			if (growthInterval !== null) clearInterval(growthInterval);
			if (growthTimeout !== null) clearTimeout(growthTimeout);
			clearInterval(interval);
			document.removeEventListener('mouseup', handleGlobalTouchEnd);
			document.removeEventListener('touchend', handleGlobalTouchEnd);
			document.removeEventListener('touchcancel', handleGlobalTouchEnd);
			// Clean up drag listeners
			document.removeEventListener('pointermove', handleDragMove);
			document.removeEventListener('pointerup', handleDragEnd);
			// Clean up recompose movement listener
			document.removeEventListener('pointermove', handleRecomposeMovement, { capture: true });
			unsubscribe();
		};
	});

	// Navigation functions
	function zoomInto(nodeId: string) {
		console.log('[UI FLOW] zoomInto called for node:', nodeId);
		globalState.zoomInto(nodeId);
		console.log('[UI FLOW] Navigation handled');
	}

	function handleCreateNewTree() {
		console.log('[UI FLOW] handleCreateNewTree started');

		// Check if user is authenticated
		if (!$userAlias || !$userPub) {
			console.log('[UI FLOW] User not authenticated, prompting to log in');
			globalState.showToast('Please log in to start playing!', 'info');
			return;
		}

		try {
			const newTree = createNewTree(true); // Include example data
			if (newTree) {
				globalState.showToast('Greetings player, welcome to playnet!', 'success');
			}
		} catch (error) {
			console.error('[UI FLOW] Error creating new tree:', error);
			globalState.showToast('Error creating new tree', 'error');
		}
	}

	async function handleAddNode() {
		console.log('[UI FLOW] handleAddNode started');

		// Don't allow adding nodes when in edit mode
		if (globalState.editMode) {
			globalState.showToast('Cannot add nodes while editing', 'warning');
			return;
		}

		// Don't allow adding nodes when in delete mode
		if (globalState.deleteMode) {
			globalState.showToast('Cannot add nodes in delete mode', 'warning');
			return;
		}

		if (!currentNodeId) {
			console.log('[UI FLOW] No currentNodeId, aborting');
			return;
		}

		// Calculate initial points for new node using the protocol function
		const newPoints = calculateNodePoints(currentNode!);
		console.log('[UI FLOW] Calculated points based on siblings:', newPoints);
		const newNodeId = `node_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`;
		const newNodeName = 'New Node'; // Default name for better UX

		console.log('[UI FLOW] Creating new node:', {
			id: newNodeId,
			points: newPoints,
			name: newNodeName
		});
		console.log('[UI FLOW] Current parent:', currentNodeId);
		console.log('[UI FLOW] Current path:', path);

		try {
			// Get the tree and current node
			if (!tree) {
				globalState.showToast('Error creating node: No tree found', 'error');
				return;
			}

			// Find the current node
			if (!currentNode) {
				globalState.showToast('Error creating node: Current node not found', 'error');
				return;
			}

			// Create a deep clone of the current tree to ensure reactivity
			const updatedTree = structuredClone(tree);

			// Find the current node in our cloned tree
			const updatedCurrentNode = findNodeById(updatedTree, currentNodeId);
			if (!updatedCurrentNode) {
				globalState.showToast(
					'Error creating node: Current node not found in updated tree',
					'error'
				);
				return;
			}

			// Add child to current node in our cloned tree
			addChild(updatedCurrentNode, newNodeId, newNodeName, newPoints);

			// Update the store with the new tree to trigger reactivity
			userTree.set(updatedTree);

			// Force update counter to trigger redraw
			triggerUpdate();

			console.log('[UI FLOW] addNode successful');
			globalState.showToast('New node created', 'success');

			// Set node to edit mode
			console.log('[UI FLOW] Setting node to edit mode:', newNodeId);
			setTimeout(() => {
				globalState.nodeToEdit = newNodeId;
				console.log('[UI FLOW] Node edit mode set');
				console.log('userTree Parent.275', $userTree);
			}, 50);
		} catch (err) {
			console.error('[UI FLOW] Error in handleAddNode:', err);
			globalState.showToast('Error creating node', 'error');
		}
	}

	// Text editing handler
	function handleTextEdit(detail: { nodeId: string; newName: string }) {
		const { nodeId, newName } = detail;

		try {
			if (!tree) return;

			// Create a clone of the tree to ensure reactivity
			const updatedTree = structuredClone(tree);

			// Update the node name in memory
			updateNodeById(updatedTree, nodeId, (node) => {
				updateName(node, newName);
			});

			// Update store to trigger reactivity
			userTree.set(updatedTree);

			// Force update
			triggerUpdate();

			// Clear edit mode
			globalState.exitEditMode();

			globalState.showToast(`Node renamed to "${newName}"`, 'success');
		} catch (err) {
			console.error(`Error updating name for node ${nodeId}:`, err);
			globalState.showToast('Error updating node name', 'error');
		}
	}

	// Contributor management
	function handleAddContributor(detail: { nodeId: string; clientX: number; clientY: number }) {
		const { nodeId, clientX, clientY } = detail;

		// Show user dropdown
		activeNodeId = nodeId;
		dropdownPosition = { x: clientX, y: clientY };
		showUserDropdown = true;
	}

	function handleRemoveContributor(detail: { nodeId: string; contributorId: string }) {
		const { nodeId, contributorId } = detail;

		try {
			if (!tree) return;

			// Create a clone of the tree to ensure reactivity
			const updatedTree = structuredClone(tree);

			// Find the node to update
			const node = findNodeById(updatedTree, nodeId);
			if (!node || node.type !== 'NonRootNode') return;

			// Get current contributor IDs and filter out the one to remove
			const updatedContributors = (node as NonRootNode).contributor_ids.filter(
				(id: string) => id !== contributorId
			);

			// Update the node's contributors
			(node as NonRootNode).contributor_ids = updatedContributors;

			// Run deduplication to ensure no duplicates exist
			deduplicateContributorsInTree(updatedTree);

			// Update the store to trigger reactivity
			userTree.set(updatedTree);

			// Force update
			triggerUpdate();

			globalState.showToast('Contributor removed successfully', 'success');
		} catch (err) {
			console.error('Error removing contributor:', err);
			globalState.showToast('Error removing contributor', 'error');
		}
	}

	function handleUserSelect(detail: { id: string; name: string; metadata?: any }) {
		const { id: selectedId, metadata } = detail;

		if (!activeNodeId) {
			console.error('No node selected for adding contributor');
			return;
		}

		// Determine the contributor ID to use:
		// - For contacts: use the contact ID (starts with "contact_")
		// - For regular users: use the public key (the id itself)
		const contributorId = metadata?.isContact ? selectedId : selectedId;

		console.log('[CONTRIBUTOR] Adding contributor:', {
			selectedId,
			contributorId,
			isContact: metadata?.isContact,
			name: detail.name
		});

		// Add the contributor
		addContributorToNode(activeNodeId, contributorId);

		// Reset dropdown
		showUserDropdown = false;
		activeNodeId = null;
	}

	function handleDropdownClose() {
		showUserDropdown = false;
		activeNodeId = null;
	}

	// Specific substitution function - replaces all instances of a public key with a contact ID
	function replacePublicKeyWithContactId(
		treeToModify: Node,
		publicKeyToReplace: string,
		newContactId: string
	): number {
		let substitutionCount = 0;

		// Recursive function to substitute public key with contact ID in a node and its children
		function substituteInNode(node: Node): void {
			// Only NonRootNodes have contributor_ids
			if (node.type === 'NonRootNode') {
				const nonRootNode = node as NonRootNode;
				if (nonRootNode.contributor_ids && nonRootNode.contributor_ids.length > 0) {
					// Replace all instances of the public key with the contact ID
					const updatedContributors = nonRootNode.contributor_ids.map((contributorId) => {
						if (contributorId === publicKeyToReplace) {
							console.log(
								`[SUBSTITUTION] Replacing public key '${publicKeyToReplace.substring(0, 20)}...' with contact ID '${newContactId}' in node '${node.name}' (${node.id})`
							);
							substitutionCount++;
							return newContactId;
						}
						return contributorId;
					});

					// Update the contributor_ids array
					nonRootNode.contributor_ids = updatedContributors;
				}
			}

			// Recursively substitute in all child nodes
			if (node.children && node.children.length > 0) {
				node.children.forEach(substituteInNode);
			}
		}

		// Start substitution from the root
		substituteInNode(treeToModify);

		if (substitutionCount > 0) {
			console.log(
				`[SUBSTITUTION] Replaced ${substitutionCount} instances of public key with contact ID '${newContactId}'`
			);
		}

		return substitutionCount;
	}

	function handleCreateContact(detail: { name: string; publicKey?: string }) {
		try {
			const newContact = createContact({
				name: detail.name,
				public_key: detail.publicKey
			});

			console.log('[CONTACT] Created new contact:', newContact);

			// If the contact has a public key, substitute all instances of that public key with the contact ID
			if (newContact.public_key && tree) {
				console.log('[CONTACT] Substituting public key with contact ID in tree:', {
					publicKey: newContact.public_key,
					contactId: newContact.contact_id
				});

				// Create a clone of the tree to ensure reactivity
				const updatedTree = structuredClone(tree);

				// Run specific substitution for this public key -> contact ID
				const substitutionChanges = replacePublicKeyWithContactId(
					updatedTree,
					newContact.public_key,
					newContact.contact_id
				);

				// Then run deduplication to clean up any remaining duplicates
				const deduplicationChanges = deduplicateContributorsInTree(updatedTree);

				// Update the tree if there were changes
				if (substitutionChanges || deduplicationChanges) {
					console.log(
						`[CONTACT] Tree updated: ${substitutionChanges} substitutions, deduplication: ${deduplicationChanges}`
					);
					userTree.set(updatedTree);
					triggerUpdate();
				}
			}

			globalState.showToast(`Contact "${detail.name}" created successfully`, 'success');

			// Automatically add the new contact as a contributor to the active node
			if (activeNodeId) {
				addContributorToNode(activeNodeId, newContact.contact_id);
			}
		} catch (error) {
			console.error('[CONTACT] Error creating contact:', error);
			globalState.showToast('Error creating contact: ' + (error as Error).message, 'error');
			throw error; // Re-throw so the dropdown can handle it
		}
	}

	function handleUpdateContact(detail: { contactId: string; name: string }) {
		try {
			updateContact(detail.contactId, { name: detail.name });

			console.log('[CONTACT] Updated contact:', detail);
			globalState.showToast(`Contact renamed to "${detail.name}"`, 'success');
		} catch (error) {
			console.error('[CONTACT] Error updating contact:', error);
			globalState.showToast('Error updating contact: ' + (error as Error).message, 'error');
			throw error; // Re-throw so the dropdown can handle it
		}
	}

	function handleDeleteContact(detail: { contactId: string; name: string; publicKey?: string }) {
		try {
			if (!tree) {
				globalState.showToast('No tree found to update', 'error');
				return;
			}

			const { contactId, name, publicKey } = detail;
			console.log('[CONTACT] Deleting contact:', { contactId, name, publicKey });

			// Create a clone of the tree to ensure reactivity
			const updatedTree = structuredClone(tree);

			// First, replace the specific contact_id with public key or remove it
			let substitutionCount = 0;
			let removalCount = 0;

			function replaceSpecificContactId(node: Node): void {
				if (node.type === 'NonRootNode') {
					const nonRootNode = node as NonRootNode;
					if (nonRootNode.contributor_ids && nonRootNode.contributor_ids.length > 0) {
						// Replace contact_id with public key, or remove if no public key
						nonRootNode.contributor_ids = nonRootNode.contributor_ids
							.map((contributorId) => {
								if (contributorId === contactId) {
									if (publicKey) {
										console.log(
											`[CONTACT] Replacing contact_id '${contactId}' with public key '${publicKey.substring(0, 20)}...' in node '${node.name}' (${node.id})`
										);
										substitutionCount++;
										return publicKey;
									} else {
										console.log(
											`[CONTACT] Contact '${contactId}' has no public key, it will be removed from node '${node.name}' (${node.id})`
										);
										removalCount++;
										return null; // Mark for removal
									}
								}
								return contributorId;
							})
							.filter((id) => id !== null) as string[]; // Remove null entries
					}
				}

				// Recursively process all child nodes
				if (node.children && node.children.length > 0) {
					node.children.forEach(replaceSpecificContactId);
				}
			}

			// Start replacement from the root
			replaceSpecificContactId(updatedTree);

			// Then run unified deduplication to clean up any remaining duplicates
			const deduplicationChanges = deduplicateContributorsInTree(updatedTree);

			// Update the tree if there were changes
			if (substitutionCount > 0 || removalCount > 0 || deduplicationChanges) {
				console.log(
					`[CONTACT] Tree updated: ${substitutionCount} substitutions, ${removalCount} removals${deduplicationChanges ? ', plus deduplication' : ''}`
				);
				userTree.set(updatedTree);
				triggerUpdate();
			}

			// Delete the contact from userContacts
			deleteContact(contactId);

			// Show success message
			let message = `Contact "${name}" deleted`;
			if (substitutionCount > 0) {
				message += ` and replaced with public key in ${substitutionCount} node(s)`;
			}
			if (removalCount > 0) {
				message += ` and removed from ${removalCount} node(s)`;
			}

			globalState.showToast(message, 'success');

			console.log('[CONTACT] Contact deletion completed successfully');
		} catch (error) {
			console.error('[CONTACT] Error deleting contact:', error);
			globalState.showToast('Error deleting contact: ' + (error as Error).message, 'error');
		}
	}

	function handleRemoveItem(detail: { id: string; name: string; metadata?: any }) {
		if (!activeNodeId) {
			console.error('No node selected for removing contributor');
			return;
		}

		// Get the contributor ID to remove
		const contributorId = detail.id;

		// If this is a contact being removed, also check if we need to remove the associated public key
		let contributorIdsToRemove = [contributorId];

		if (detail.metadata?.isContact && detail.metadata?.userId) {
			// This is a contact - also remove its public key if it exists as a contributor
			const publicKey = detail.metadata.userId;
			if (publicKey !== contributorId) {
				// Only add if it's different from the contact ID
				contributorIdsToRemove.push(publicKey);
			}
		}

		// Remove all the contributor IDs - safe to use activeNodeId here since we checked above
		contributorIdsToRemove.forEach((idToRemove) => {
			handleRemoveContributor({ nodeId: activeNodeId!, contributorId: idToRemove });
		});

		console.log('[CONTRIBUTOR] Removed contributor(s):', {
			nodeId: activeNodeId,
			contributorIds: contributorIdsToRemove,
			name: detail.name
		});
	}

	function addContributorToNode(nodeId: string, userId: string) {
		try {
			if (!tree) return;

			// Create a clone of the tree to ensure reactivity
			const updatedTree = structuredClone(tree);

			// Find the node to update
			const node = findNodeById(updatedTree, nodeId);
			if (!node || node.type !== 'NonRootNode') return;

			// Check if contributor already exists
			const hasContributor = (node as NonRootNode).contributor_ids.includes(userId);

			if (!hasContributor) {
				// Get current contributors and add the new one
				const currentContributors = [...(node as NonRootNode).contributor_ids];
				currentContributors.push(userId);

				// Use the protocol function to properly add contributors AND clear children
				// This ensures the node becomes a proper leaf contribution node
				addContributors(node, currentContributors);

				// Run deduplication to ensure no duplicates exist
				deduplicateContributorsInTree(updatedTree);

				// Update the store to trigger reactivity
				userTree.set(updatedTree);

				// Force update
				triggerUpdate();

				globalState.showToast('Contributor added successfully', 'success');
			}
		} catch (err) {
			console.error('Error adding contributor:', err);
			globalState.showToast('Error adding contributor', 'error');
		}
	}

	// Calculate node fulfillment directly using protocol
	function getNodeFulfillment(nodeId: string): number {
		if (!tree) return 0;

		const node = findNodeById(tree, nodeId);
		if (!node) return 0;

		return fulfilled(node, tree);
	}

	// Unified deduplication function - prefers contact IDs over public keys
	function deduplicateContributorsInTree(treeToModify: Node): boolean {
		let hasChanges = false;
		let deduplicatedCount = 0;

		// Recursive function to deduplicate contributors in a node and its children
		function deduplicateNodeContributors(node: Node): void {
			// Only NonRootNodes have contributor_ids
			if (node.type === 'NonRootNode') {
				const nonRootNode = node as NonRootNode;
				if (nonRootNode.contributor_ids && nonRootNode.contributor_ids.length > 0) {
					const originalLength = nonRootNode.contributor_ids.length;
					const originalContributors = [...nonRootNode.contributor_ids];

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

					// Create a set to track resolved public keys we've already seen
					const seenPublicKeys = new Set<string>();
					const deduplicatedContributors: string[] = [];

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
										`[DEDUP] Removing duplicate contact '${contributorId}' from node '${node.name}' (${node.id})`
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
									`[DEDUP] Removing public key '${contributorId.substring(0, 20)}...' in favor of contact ID '${publicKeyToContactId.get(contributorId)}' from node '${node.name}' (${node.id})`
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
										`[DEDUP] Removing duplicate public key '${contributorId.substring(0, 20)}...' from node '${node.name}' (${node.id})`
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
							`[DEDUP] Node '${node.name}' (${node.id}): ${originalLength} → ${nonRootNode.contributor_ids.length} contributors`
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
		deduplicateNodeContributors(treeToModify);

		if (hasChanges) {
			console.log(`[DEDUP] Deduplicated ${deduplicatedCount} contributor entries from tree`);
		}

		return hasChanges;
	}

	// Growth handlers
	function startGrowth(node: d3.HierarchyRectangularNode<VisualizationNode>, isShrinking = false) {
		// Don't allow growth in delete mode
		if (globalState.deleteMode) return;

		// Don't allow growth in recompose mode
		if (globalState.recomposeMode) return;

		// Don't allow growth when dragging
		if (globalState.isDragging) return;

		// Clear existing growth state
		if (growthInterval !== null) clearInterval(growthInterval);
		if (growthTimeout !== null) clearTimeout(growthTimeout);
		isGrowing = false;
		isShrinkingActive = false;

		// Only run timeouts in browser environment
		if (!browser) return;

		// Set growth delay
		growthTimeout = window.setTimeout(() => {
			// Only start growing if still touching same node
			if (isTouching && activeGrowthNodeId === node.data.id) {
				isGrowing = true;
				isShrinkingActive = isShrinking;

				growthInterval = window.setInterval(() => {
					// Stop if no longer touching
					if (!isTouching) {
						stopGrowth();
						return;
					}

					// Calculate growth rate based on current size
					let rate;
					const currentPoints = node.data.points;
					if (isShrinking) {
						// For consistent relative change, use current points * constant rate
						// This means it takes the same time to halve in size regardless of starting size
						rate = currentPoints * BASE_SHRINK_RATE;
					} else {
						// For consistent relative change, use current points * constant rate
						// This means it takes the same time to double in size regardless of starting size
						rate = currentPoints * BASE_GROWTH_RATE;
					}

					const newPoints = Math.max(1, currentPoints + rate);

					if (isNaN(newPoints)) {
						console.error('Growth calculation resulted in NaN:', {
							currentPoints,
							rate,
							isShrinking
						});
						return;
					}

					// Always update to ensure animation runs smoothly
					updateNodePoints(node, newPoints);
				}, GROWTH_TICK);
			}
		}, GROWTH_DELAY);
	}

	function stopGrowth() {
		isTouching = false;

		// Save final points if growing
		if (isGrowing && activeGrowthNodeId && hierarchyData) {
			const nodeToUpdate = hierarchyData.children?.find(
				(child: d3.HierarchyRectangularNode<VisualizationNode>) =>
					child.data.id === activeGrowthNodeId
			);

			if (nodeToUpdate && nodeToUpdate.data && nodeToUpdate.data.id) {
				saveNodePoints(nodeToUpdate.data.id, nodeToUpdate.data.points);
			}
		}

		// Reset growth state
		activeGrowthNodeId = null;
		if (growthTimeout !== null) clearTimeout(growthTimeout);
		if (growthInterval !== null) clearInterval(growthInterval);
		growthInterval = null;
		isGrowing = false;
		isShrinkingActive = false;
	}

	function updateNodePoints(node: d3.HierarchyRectangularNode<VisualizationNode>, points: number) {
		// Update node's points in hierarchy
		node.data.points = points;

		// Trigger a reactive update
		updateCounter++;
	}

	function saveNodePoints(nodeId: string, points: number) {
		try {
			if (!tree) return;

			// Create a clone of the tree to ensure reactivity
			const updatedTree = structuredClone(tree);

			// Find the node and directly update it
			const node = findNodeById(updatedTree, nodeId);
			if (!node || node.type !== 'NonRootNode') return;

			// Update points directly using protocol
			updatePoints(node as NonRootNode, points);

			// Update the store to trigger reactivity
			userTree.set(updatedTree);

			// Force update
			triggerUpdate();
		} catch (err) {
			console.error(`Error saving points for node ${nodeId}:`, err);
			globalState.showToast('Error saving node points', 'error');
		}
	}

	function handleGlobalTouchEnd(event: Event) {
		// Only handle if the touch event is within our treemap container
		const target = event.target as HTMLElement;
		const isInTreemap = target?.closest('.treemap-container') !== null;

		if (isTouching && isInTreemap) {
			stopGrowth();
			resetInteractionState();
		}
	}

	// Track active interaction to prevent duplicate events
	let activePointerId: number | null = $state(null);
	let interactionHandled = $state(false);
	let isMultiTouch = $state(false);

	// Handle touch start to detect multi-touch gestures
	function handleTouchStart(
		event: TouchEvent,
		node: d3.HierarchyRectangularNode<VisualizationNode>
	) {
		// Prevent default touch behaviors that might interfere with our drag
		event.preventDefault();

		// Detect multi-touch for shrinking
		isMultiTouch = event.touches.length === 2;
		console.log(
			'[DEBUG] TouchStart - touches:',
			event.touches.length,
			'isMultiTouch:',
			isMultiTouch
		);

		// Let the pointer event handle the main logic
		// This is just for multi-touch detection
	}

	// Handle touch end to reset multi-touch state
	function handleTouchEnd(event: TouchEvent) {
		// Prevent default touch behaviors
		event.preventDefault();

		// Reset multi-touch state when fingers are lifted
		if (event.touches.length < 2) {
			isMultiTouch = false;
		}
	}

	function handleGrowthStart(
		event: PointerEvent,
		node: d3.HierarchyRectangularNode<VisualizationNode>
	) {
		console.log(
			'[DEBUG] PointerDown - isPrimary:',
			event.isPrimary,
			'button:',
			event.button,
			'isMultiTouch:',
			isMultiTouch
		);

		// Only handle primary pointer to prevent duplicates
		if (!event.isPrimary) return;

		// Don't handle growth interactions when in edit mode
		if (globalState.editMode) {
			console.log('[DEBUG PARENT] Ignoring growth start - in edit mode');
			return;
		}

		// Check if we should start dragging immediately (non-recompose mode special clicks)
		const shouldStartDragImmediately =
			!globalState.deleteMode &&
			!globalState.recomposeMode &&
			(event.button === 1 || event.ctrlKey || event.metaKey); // Middle click / Ctrl+click in normal mode

		if (shouldStartDragImmediately) {
			startDragging(event, node);
			return;
		}

		// Check if the pointer target is a text edit field, node-text element, or contributor element
		const target = event.target as HTMLElement;
		if (
			target &&
			(target.closest('.node-text') ||
				target.closest('.node-text-edit-container') ||
				target.closest('.add-contributor-button') ||
				target.closest('.tag-container') ||
				target.closest('.contributor-container') ||
				target.closest('.node-title') ||
				target.closest('.node-title-area') ||
				target.closest('.title-segment') ||
				target.closest('.node-edit-input') ||
				target.closest('.contributors-area'))
		) {
			// Click originated from text or contributor elements, don't handle here - let Child component handle it
			console.log('[DEBUG] Ignoring pointer event on text/contributor element:', target.className);
			return;
		}

		// Prevent duplicate handling for the same interaction
		if (interactionHandled) return;

		// Set interaction tracking
		activePointerId = event.pointerId;
		interactionHandled = true;

		// Set touch state
		isTouching = true;
		touchStartTime = Date.now();
		activeGrowthNodeId = node.data.id;

		// Track initial position for movement detection
		initialPointerX = event.clientX;
		initialPointerY = event.clientY;
		hasMovedSignificantly = false;

		// Add movement listener for recompose mode drag detection
		if (globalState.recomposeMode) {
			document.addEventListener('pointermove', handleRecomposeMovement, { capture: true });
		}

		// For touch events, delay processing to allow multi-touch detection
		if (event.pointerType === 'touch') {
			// Small delay to let touch events fire and detect multi-touch
			setTimeout(() => {
				if (!interactionHandled) return; // Check if interaction was cancelled

				// Determine if shrinking (right-click or multi-touch)
				const isShrinking = event.button === 2 || isMultiTouch;
				console.log(
					'[DEBUG] Delayed processing - isShrinking:',
					isShrinking,
					'button:',
					event.button,
					'isMultiTouch:',
					isMultiTouch
				);

				// In recompose mode, don't start growth - we'll handle drag vs navigation in handleGrowthEnd
				if (!globalState.recomposeMode) {
					// Start growth with delay
					startGrowth(node, isShrinking);
				}
			}, 50); // 50ms delay to allow touch events to fire
		} else {
			// For mouse events, process immediately
			const isShrinking = event.button === 2 || isMultiTouch;
			console.log(
				'[DEBUG] Immediate processing - isShrinking:',
				isShrinking,
				'button:',
				event.button,
				'isMultiTouch:',
				isMultiTouch
			);

			// In recompose mode, don't start growth - we'll handle drag vs navigation in handleGrowthEnd
			if (!globalState.recomposeMode) {
				// Start growth with delay
				startGrowth(node, isShrinking);
			}
		}
	}

	function handleGrowthEnd(event: PointerEvent) {
		// Only handle if this is our active pointer
		if (!event.isPrimary || event.pointerId !== activePointerId) return;

		// Don't handle interactions when in edit mode
		if (globalState.editMode) {
			console.log('[DEBUG PARENT] Ignoring interaction - in edit mode');
			resetInteractionState();
			return;
		}

		// Check if the click target is a text edit field, node-text element, or contributor element
		const target = event.target as HTMLElement;
		if (
			target &&
			(target.closest('.node-text') ||
				target.closest('.node-text-edit-container') ||
				target.closest('.add-contributor-button') ||
				target.closest('.tag-container') ||
				target.closest('.contributor-container') ||
				target.closest('.node-title') ||
				target.closest('.node-title-area') ||
				target.closest('.title-segment') ||
				target.closest('.node-edit-input') ||
				target.closest('.contributors-area'))
		) {
			// Click originated from text or contributor elements, don't navigate
			resetInteractionState();
			return;
		}

		// Check if this was a short tap (for navigation) or long press (for growth)
		const touchDuration = Date.now() - touchStartTime;
		const wasGrowthEvent = touchDuration >= TAP_THRESHOLD || isGrowing;

		// Get the nodeId before stopping growth
		const nodeId = activeGrowthNodeId;

		// Always stop growth
		stopGrowth();

		// For short taps, trigger navigation, deletion, or recompose
		if (!wasGrowthEvent && nodeId) {
			if (globalState.deleteMode) {
				// Handle deletion
				handleNodeDeletion(nodeId);
			} else if (globalState.recomposeMode && !globalState.isDragging && !hasMovedSignificantly) {
				// In recompose mode, allow navigation if not currently dragging and no significant movement
				// This enables zooming even while recompose mode is active
				zoomInto(nodeId);
			} else if (globalState.recomposeMode && (globalState.isDragging || hasMovedSignificantly)) {
				// Handle recompose only if we're actually dragging or moved significantly
				handleNodeRecompose(nodeId);
			} else {
				// This was a tap, not a hold - navigate into the node
				zoomInto(nodeId);
			}
		}

		// Reset interaction state
		resetInteractionState();
	}

	// Helper function to reset interaction tracking
	function resetInteractionState() {
		activePointerId = null;
		interactionHandled = false;
		isMultiTouch = false;
		hasMovedSignificantly = false;

		// Remove recompose movement listener if it exists
		if (globalState.recomposeMode) {
			document.removeEventListener('pointermove', handleRecomposeMovement, { capture: true });
		}
	}

	// Handle movement during recompose mode to detect drag vs tap
	function handleRecomposeMovement(event: PointerEvent) {
		if (!globalState.recomposeMode || !isTouching || globalState.isDragging) return;

		// Only handle primary pointer
		if (!event.isPrimary || event.pointerId !== activePointerId) return;

		const dx = Math.abs(event.clientX - initialPointerX);
		const dy = Math.abs(event.clientY - initialPointerY);

		// If movement exceeds threshold, start dragging
		if ((dx > MOVEMENT_THRESHOLD || dy > MOVEMENT_THRESHOLD) && !hasMovedSignificantly) {
			hasMovedSignificantly = true;

			// Find the node to start dragging
			if (activeGrowthNodeId) {
				const node = hierarchyData?.children?.find((child) => child.data.id === activeGrowthNodeId);
				if (node) {
					console.log('[RECOMPOSE] Movement detected, starting drag');
					// Remove the movement listener since we're starting a drag
					document.removeEventListener('pointermove', handleRecomposeMovement, { capture: true });
					// Start dragging
					startDragging(event, node);
				}
			}
		}
	}

	// Reset interaction state when navigation occurs
	$effect(() => {
		// Watch for path changes to reset interaction state
		path;
		resetInteractionState();
	});

	function handleNodeDeletion(nodeId: string) {
		if (confirm(`Delete this node and all its children?`)) {
			try {
				if (!tree) return;

				// Create a clone of the tree to ensure reactivity
				const updatedTree = structuredClone(tree);

				// Find the parent node first
				const parentNode = getParentNode(updatedTree, nodeId);
				if (!parentNode) {
					console.error('Cannot find parent node');
					return;
				}

				// Don't allow deleting root node
				if (nodeId === path[0]) {
					console.error('Cannot delete root node');
					globalState.showToast('Cannot delete root node', 'error');
					return;
				}

				// Check if we're trying to delete the current node
				const isCurrentNode = currentNodeId === nodeId;
				if (isCurrentNode) {
					// If we're on the node to delete, navigate up to parent first
					if (path.length <= 1) {
						console.error('Cannot delete current node with no parent');
						return;
					}

					// Navigate to parent
					globalState.zoomOut();
				}

				// Find the node to delete
				const nodeToDelete = findNodeById(updatedTree, nodeId);
				if (!nodeToDelete) {
					globalState.showToast('Node not found', 'error');
					return;
				}

				// Delete the node from its parent
				const childIndex = parentNode.children.findIndex((child) => child.id === nodeId);
				if (childIndex !== -1) {
					parentNode.children.splice(childIndex, 1);
				}

				// Update the store
				userTree.set(updatedTree);

				// Force update
				triggerUpdate();

				globalState.showToast('Node deleted successfully', 'success');
			} catch (err) {
				console.error(`Error in deletion process: ${err}`);
				globalState.showToast('Error deleting node', 'error');
			}
		}
	}

	function handleNodeRecompose(nodeId: string) {
		try {
			if (!tree) {
				globalState.showToast('No tree available for recompose', 'error');
				return;
			}

			// Find the node to recompose
			const nodeToRecompose = findNodeById(tree, nodeId);
			if (!nodeToRecompose) {
				globalState.showToast('Node not found', 'error');
				return;
			}

			// For now, show a toast indicating recompose action
			// In the future, this could trigger actual recomposition logic
			globalState.showToast(`Recomposing node: ${nodeToRecompose.name}`, 'info');

			console.log('[RECOMPOSE] Recomposing node:', nodeToRecompose);
		} catch (err) {
			console.error(`Error in recompose process: ${err}`);
			globalState.showToast('Error recomposing node', 'error');
		}
	}

	// Drag functions
	function startDragging(
		event: PointerEvent,
		node: d3.HierarchyRectangularNode<VisualizationNode>
	) {
		event.preventDefault();
		event.stopPropagation();

		// Set global drag state
		globalState.startDrag(
			node.data.id,
			node.data.nodeName || 'Unnamed',
			'', // Will use computed color from nodeName
			event.clientX,
			event.clientY
		);
		dragStartTime = Date.now();

		// Initialize movement tracking if not already done
		if (initialPointerX === 0 && initialPointerY === 0) {
			initialPointerX = event.clientX;
			initialPointerY = event.clientY;
		}
		hasMovedSignificantly = true; // Mark as moved since we're dragging

		console.log('[DRAG] Started dragging node:', globalState.draggedNodeName);

		// Add global event listeners for mouse movement and end
		// Use capture phase to ensure we get the events before other handlers
		document.addEventListener('pointermove', handleDragMove, { capture: true });
		document.addEventListener('pointerup', handleDragEnd, { capture: true });
	}

	function handleDragMove(event: PointerEvent) {
		if (!globalState.isDragging) return;

		// Update drag position for both mouse and touch events when actively dragging
		if (event.isPrimary) {
			globalState.updateDragPosition(event.clientX, event.clientY);
		}
	}

	function handleDragEnd(event: PointerEvent) {
		if (!globalState.isDragging) return;

		// Only handle primary pointer events to avoid interference with scrolling
		if (!event.isPrimary) return;

		console.log('[DRAG] Ending drag');

		// Check if this was a quick click (not a real drag)
		const dragDuration = Date.now() - dragStartTime;
		const wasDragGesture = dragDuration >= 200; // Consider it a drag if held for 200ms+

		let targetNodeId: string | null = null;

		if (wasDragGesture && globalState.draggedNodeId) {
			// Find what node we're dropping on by checking the element under the cursor
			const elementUnder = document.elementFromPoint(event.clientX, event.clientY);
			// Check for both node drops and breadcrumb drops
			const dropTargetElement = elementUnder?.closest(
				'.clickable[data-node-id], .breadcrumb-item[data-node-id]'
			);

			if (dropTargetElement) {
				targetNodeId = dropTargetElement.getAttribute('data-node-id');
				if (targetNodeId && targetNodeId !== globalState.draggedNodeId) {
					const success = globalState.handleNodeReorder(globalState.draggedNodeId, targetNodeId);

					// If the drag was successful, navigate to the target location
					if (success) {
						console.log('[DRAG] Successfully moved node, navigating to target:', targetNodeId);
						// Small delay to ensure the tree update has been processed
						setTimeout(() => {
							if (targetNodeId) {
								// Get the absolute path to the target node instead of just appending
								const updatedTree = get(userTree);
								if (updatedTree) {
									const absolutePath = getPathToNode(updatedTree, targetNodeId);
									if (absolutePath) {
										globalState.navigateToPath(absolutePath);
									}
								}
							}
						}, 100);
					}
				}
			}
		}

		// Reset global drag state
		globalState.endDrag();
		dragStartTime = 0;
		initialPointerX = 0;
		initialPointerY = 0;
		hasMovedSignificantly = false;

		// Remove global event listeners
		document.removeEventListener('pointermove', handleDragMove, { capture: true });
		document.removeEventListener('pointerup', handleDragEnd, { capture: true });
	}
</script>

<div class="node-container">
	<!-- Main treemap content -->
	<div class="app-content">
		<div class="treemap-container">
			{#if !tree}
				<div class="empty-state">
					<div class="add-node-prompt">
						<button class="play-button" onclick={handleCreateNewTree}>
							<span class="play-text">Play! ✨</span>
						</button>
					</div>
				</div>
			{:else if hierarchyData && hierarchyData.children && hierarchyData.children.length > 0}
				{#each hierarchyData.children as child}
					<div
						class="clickable"
						class:deleting={globalState.deleteMode}
						class:recomposing={globalState.recomposeMode}
						class:dragging-over={globalState.isDragging &&
							globalState.draggedNodeId !== child.data.id}
						class:being-dragged={globalState.isDragging &&
							globalState.draggedNodeId === child.data.id}
						class:growing={isGrowing && activeGrowthNodeId === child.data.id && !isShrinkingActive}
						class:shrinking={isGrowing && activeGrowthNodeId === child.data.id && isShrinkingActive}
						data-node-id={child.data.id}
						style="
							position: absolute;
							left: {child.x0 * 100}%;
							top: {child.y0 * 100}%;
							width: {(child.x1 - child.x0) * 100}%;
							height: {(child.y1 - child.y0) * 100}%;
							box-sizing: border-box;
						"
						onpointerdown={(e) => handleGrowthStart(e, child)}
						onpointerup={(e) => handleGrowthEnd(e)}
						ontouchstart={(e) => handleTouchStart(e, child)}
						ontouchend={(e) => handleTouchEnd(e)}
						oncontextmenu={(e) => e.preventDefault()}
					>
						<Child
							node={{
								id: child.data.id,
								name: child.data.nodeName || 'Unnamed',
								points: child.data.points,
								contributors: child.data.contributors || [],
								fulfillment: getNodeFulfillment(child.data.id),
								hasChildren: child.data.hasChildren || false
							}}
							dimensions={{
								x0: child.x0,
								y0: child.y0,
								x1: child.x1,
								y1: child.y1
							}}
							addContributor={handleAddContributor}
							removeContributor={handleRemoveContributor}
							onTextEdit={handleTextEdit}
							shouldEdit={globalState.nodeToEdit === child.data.id}
						/>
					</div>
				{/each}
			{:else}
				<div class="empty-state">
					<div class="add-node-prompt">
						<button class="add-node-button" onclick={handleAddNode}>
							<span class="add-icon">➕</span>
							<span class="add-text">{nodeLabels[labelIndex]}</span>
						</button>
					</div>
				</div>
			{/if}
		</div>
	</div>

	<!-- User dropdown for adding contributors -->
	<DropDown
		show={showUserDropdown}
		title="Select Contributor"
		searchPlaceholder="Search users..."
		position={dropdownPosition}
		width={280}
		maxHeight={320}
		dataProvider={usersDataProvider}
		selectedIds={currentContributors}
		allowCreateContact={true}
		select={handleUserSelect}
		removeItem={handleRemoveItem}
		createContact={handleCreateContact}
		updateContact={handleUpdateContact}
		deleteContact={handleDeleteContact}
		close={handleDropdownClose}
	/>
</div>

<style>
	.node-container {
		width: 100%;
		height: 100%;
		display: flex;
		flex-direction: column;
		position: relative;
	}

	.path-navigation {
		padding: 8px;
		display: flex;
		align-items: center;
		gap: 8px;
		border-bottom: 1px solid #eee;
	}

	.path {
		display: flex;
		align-items: center;
		flex-wrap: nowrap;
		overflow-x: auto;
		scrollbar-width: none; /* Firefox */
		-ms-overflow-style: none; /* IE and Edge */
		max-width: 100%;
		padding: 4px 0;
	}

	.path::-webkit-scrollbar {
		display: none; /* Chrome, Safari, Opera */
	}

	.path-item {
		white-space: nowrap;
		padding: 2px 4px;
		border-radius: 4px;
		cursor: pointer;
		transition: background-color 0.2s;
		color: #666;
		font-size: 14px;
	}

	.path-item:hover {
		background-color: rgba(0, 0, 0, 0.05);
		color: #333;
	}

	.path-item.current {
		font-weight: bold;
		color: #2196f3;
	}

	.path-separator {
		margin: 0 4px;
		color: #999;
	}

	.app-content {
		flex: 1;
		overflow: auto;
		position: relative;
	}

	.treemap-container {
		width: 100%;
		height: 100%;
		position: absolute;
		top: 0;
		left: 0;
	}

	.treemap-container :global(.clickable) {
		cursor: pointer;
		user-select: none;
		touch-action: none; /* Disable browser touch behaviors */
		transition:
			left 0.12s linear,
			top 0.12s linear,
			width 0.12s linear,
			height 0.12s linear;
	}

	:global(.clickable.deleting) {
		cursor: not-allowed;
		opacity: 0.7;
		transition: opacity 0.2s ease;
	}

	:global(.clickable.deleting:hover) {
		opacity: 1;
	}

	:global(.clickable.recomposing) {
		cursor: pointer;
		opacity: 0.7;
		transition: opacity 0.2s ease;
	}

	:global(.clickable.recomposing:hover) {
		opacity: 1;
	}

	:global(.clickable.dragging-over) {
		cursor: pointer;
		opacity: 0.7;
		transition: opacity 0.2s ease;
	}

	:global(.clickable.dragging-over:hover) {
		opacity: 1;
		background: rgba(33, 150, 243, 0.1) !important;
		border: 2px dashed #2196f3 !important;
	}

	:global(.clickable.being-dragged) {
		opacity: 0.3;
		pointer-events: none;
	}

	:global(.clickable.growing) {
		z-index: 10;
		box-shadow: 0 0 12px rgba(0, 100, 255, 0.5);
		border: 2px solid rgba(0, 100, 255, 0.7);
	}

	:global(.clickable.shrinking) {
		z-index: 10;
		box-shadow: 0 0 12px rgba(255, 60, 60, 0.5);
		border: 2px solid rgba(255, 60, 60, 0.7);
	}

	.empty-state {
		display: flex;
		justify-content: center;
		align-items: center;
		width: 100%;
		height: 100%;
		background-color: #f9f9f9;
		border-radius: 8px;
	}

	.add-node-prompt {
		display: flex;
		flex-direction: column;
		align-items: center;
		text-align: center;
		padding: 24px;
	}

	.add-node-button {
		display: flex;
		align-items: center;
		gap: 8px;
		padding: 12px 20px;
		border: none;
		border-radius: 8px;
		background-color: #2196f3;
		color: white;
		font-size: 16px;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
		box-shadow: 0 2px 6px rgba(33, 150, 243, 0.3);
	}

	.add-node-button:hover {
		background-color: #1976d2;
		transform: translateY(-2px);
		box-shadow: 0 4px 8px rgba(33, 150, 243, 0.4);
		animation: none;
	}

	.play-button {
		display: flex;
		align-items: center;
		justify-content: center;
		padding: 18px 28px;
		border: none;
		border-radius: 8px;
		background-color: #4caf50;
		color: white;
		font-size: 20px;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s ease;
		box-shadow: 0 3px 8px rgba(76, 175, 80, 0.3);
	}

	.play-button:hover {
		background-color: #388e3c;
		transform: translateY(-2px);
		box-shadow: 0 5px 12px rgba(76, 175, 80, 0.4);
	}

	.play-text {
		font-size: 20px;
	}

	.add-icon {
		font-size: 20px;
	}

	.help-text {
		margin-top: 12px;
		color: #666;
		font-size: 14px;
	}
</style>
