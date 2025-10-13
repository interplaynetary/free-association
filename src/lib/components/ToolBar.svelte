<script lang="ts">
	import { globalState, currentPath } from '$lib/global.svelte';
	import { userTree } from '$lib/state/core.svelte';
	import { findNodeById, addChild, calculateNodePoints } from '$lib/protocol';
	import { page } from '$app/stores';
	import { get } from 'svelte/store';
	import { base } from '$app/paths';
	import { searchTreeForNavigation } from '$lib/utils/treeSearch';
	import { userAlias, userPub } from '$lib/state/gun.svelte';
	import { userCapacities, mutualContributors, userNetworkCapacitiesWithSlotQuantities } from '$lib/state/core.svelte';
	import { addCapacity as addCapacityToCollection } from '$lib/protocol';
	import { getLocalTimeZone, today } from '@internationalized/date';
	import type { ProviderCapacity, Node, NonRootNode, CapacitiesCollection } from '$lib/schema';
	import { collectiveForest } from '$lib/collective.svelte';
	import { userNamesOrAliasesCache, resolveToPublicKey, getUserName } from '$lib/state/users.svelte';
	import { derived } from 'svelte/store';
	import { fade } from 'svelte/transition';
	import {
		getColorForUserId,
		getColorForNameHash,
		getContrastTextColor
	} from '$lib/utils/colorUtils';

	// Reactive store subscriptions
	const tree = $derived($userTree);
	const path = $derived($currentPath);
	const isDeleteMode = $derived(globalState.deleteMode);
	const isRecomposeMode = $derived(globalState.recomposeMode);
	const isTextEditMode = $derived(globalState.textEditMode);

	// Route detection - properly reactive to page store changes
	const currentRoute = $derived.by(() => {
		const pathname = $page.url.pathname;
		let routeWithoutBase = pathname.startsWith(base) ? pathname.slice(base.length) : pathname;

		// Ensure we have a leading slash
		if (!routeWithoutBase.startsWith('/')) {
			routeWithoutBase = '/' + routeWithoutBase;
		}

		// If it's just the base path, return '/'
		if (routeWithoutBase === '/' || routeWithoutBase === '') {
			routeWithoutBase = '/';
		}

		return routeWithoutBase;
	});

	const isMainRoute = $derived(currentRoute === '/' || currentRoute === '');
	const isInventoryRoute = $derived(currentRoute.startsWith('/inventory'));

	const shouldShowToolbar = $derived(isMainRoute || isInventoryRoute);

	// Search state (for main route and inventory)
	let showSearchPanel = $state(false);
	let searchQuery = $state('');
	let searchPanelRef = $state<HTMLDivElement>();
	let selectedResultIndex = $state(-1);
	
	// Inventory search state
	let showInventorySearchPanel = $state(false);
	let inventorySearchPanelRef = $state<HTMLDivElement>();

	// View switcher state
	let showViewMenu = $state(false);
	let viewMenuRef = $state<HTMLDivElement>();
	let longPressTimer = $state<number | null>(null);
	let isLongPressing = $state(false);

	// Forest subtrees state (for main route)
	let showForestPanel = $state(false);
	let selectedContributorId = $state<string | null>(null);

	// Derived search results
	const searchResults = $derived(
		searchQuery.trim() && tree ? searchTreeForNavigation(tree, searchQuery) : []
	);

	// Provider names cache for inventory search
	let providerNames = $state<Record<string, string>>({});

	// Derived providers list for inventory filter
	const inventoryProviders = $derived.by(() => {
		const networkCapacities = $userNetworkCapacitiesWithSlotQuantities;
		if (!networkCapacities) return [];

		const providerMap = new Map<string, string>();
		Object.values(networkCapacities).forEach((capacity: any) => {
			if (capacity.provider_id && !providerMap.has(capacity.provider_id)) {
				const displayName = providerNames[capacity.provider_id] || capacity.provider_id;
				providerMap.set(capacity.provider_id, displayName);
			}
		});

		return Array.from(providerMap.entries())
			.map(([id, name]) => ({ id, name }))
			.sort((a, b) => a.name.localeCompare(b.name));
	});

	// Load provider names asynchronously
	$effect(() => {
		void (async () => {
			const networkCapacities = $userNetworkCapacitiesWithSlotQuantities;
			if (!networkCapacities) return;

			const uniqueProviders = [...new Set(
				Object.values(networkCapacities).map((cap: any) => cap.provider_id).filter(Boolean)
			)];

			for (const providerId of uniqueProviders) {
				if (providerId && !providerNames[providerId]) {
					try {
						const name = await getUserName(providerId);
						if (name) {
							providerNames = {
								...providerNames,
								[providerId]: name.length > 20 ? name.substring(0, 20) + '...' : name
							};
						}
					} catch (error) {
						console.warn('Failed to get provider name:', providerId, error);
					}
				}
			}
		})();
	});

	// Handle click outside to close view menu
	$effect(() => {
		if (typeof window === 'undefined') return;

		function handleClickOutside(event: MouseEvent | TouchEvent) {
			const target = event.target as HTMLElement;
			if (showViewMenu && viewMenuRef && !viewMenuRef.contains(target)) {
				const viewButton = document.querySelector('.view-cycle-button');
				if (!viewButton?.contains(target)) {
					showViewMenu = false;
				}
			}
		}

		if (showViewMenu) {
			document.addEventListener('mousedown', handleClickOutside);
			document.addEventListener('touchstart', handleClickOutside);
		}

		return () => {
			document.removeEventListener('mousedown', handleClickOutside);
			document.removeEventListener('touchstart', handleClickOutside);
		};
	});

	// Helper function to get the sequence of node names from our current path
	function getPathNodeNames(ourTree: Node | null, path: string[]): string[] {
		if (!ourTree || path.length <= 1) return [];

		const nodeNames: string[] = [];
		let currentNode = ourTree;

		// Skip the first element (root user ID) and traverse by IDs to get names
		for (let i = 1; i < path.length; i++) {
			const nodeId = path[i];
			const found = findNodeById(currentNode, nodeId);
			if (!found) return []; // Path doesn't exist in our tree
			nodeNames.push(found.name);
			currentNode = found;
		}

		return nodeNames;
	}

	// Helper function to find node by following a sequence of node names
	function findNodeByNamePath(tree: Node, nameSequence: string[]): Node | null {
		if (nameSequence.length === 0) return tree;

		let currentNode = tree;
		for (const nodeName of nameSequence) {
			// Find child with matching name
			const found = currentNode.children.find((child) => child.name === nodeName);
			if (!found) return null;
			currentNode = found;
		}
		return currentNode;
	}

	// Helper function to get subtrees (children) of a node, preserving contributor info
	function getSubtreesWithContributors(node: Node): Array<{
		id: string;
		name: string;
		points: number;
		contributors: string[];
		antiContributors: string[];
		subtree: Node;
	}> {
		return node.children.map((child) => ({
			id: child.id,
			name: child.name,
			points: child.type === 'NonRootNode' ? (child as NonRootNode).points : 0,
			contributors: child.type === 'NonRootNode' ? (child as NonRootNode).contributor_ids : [],
			antiContributors:
				child.type === 'NonRootNode' ? (child as NonRootNode).anti_contributors_ids : [],
			subtree: child
		}));
	}

	// Derived store: Contributors who have trees available at the current path
	const availableContributors = $derived.by(() => {
		const pathNodeNames = getPathNodeNames(tree, path);
		const contributors: Array<{
			id: string;
			name: string;
			hasSubtreesAtPath: boolean;
			nodeAtPath: Node | null;
		}> = [];

		for (const contributorId of $mutualContributors) {
			const contributorTree = $collectiveForest.get(contributorId);
			let nodeAtPath: Node | null = null;
			let hasSubtreesAtPath = false;

			if (contributorTree) {
				// Find the node using the sequence of names
				nodeAtPath = findNodeByNamePath(contributorTree, pathNodeNames);
				// Check if this node has children (subtrees)
				hasSubtreesAtPath = nodeAtPath ? nodeAtPath.children.length > 0 : false;
			}

			contributors.push({
				id: contributorId,
				name: get(userNamesOrAliasesCache)[contributorId] || contributorId.substring(0, 8) + '...',
				hasSubtreesAtPath,
				nodeAtPath
			});
		}

		// Filter to only show contributors who have subtrees at this path
		return contributors.filter((c) => c.hasSubtreesAtPath);
	});

	// Derived store: Subtrees for the selected contributor
	const selectedContributorSubtrees = $derived.by(() => {
		if (!selectedContributorId) return [];

		const contributor = availableContributors.find((c) => c.id === selectedContributorId);
		if (!contributor || !contributor.nodeAtPath) return [];

		return getSubtreesWithContributors(contributor.nodeAtPath);
	});

	// Recompose handler
	function handleRecompose() {
		globalState.toggleRecomposeMode();
	}

	// Text edit mode handler
	function handleTextEditMode() {
		globalState.toggleTextEditMode();
	}

	// Add new node handler
	function handleAddNode() {
		if (!tree || path.length === 0) return;

		// Get current node ID (last in path)
		const currentNodeId = path[path.length - 1];

		// Create a deep clone of the tree to ensure reactivity
		const updatedTree = structuredClone(tree);

		// Find the current node in the cloned tree
		const currentNode = findNodeById(updatedTree, currentNodeId);
		if (!currentNode) {
			globalState.showToast('Error creating node: Current node not found', 'error');
			return;
		}

		// Calculate initial points for new node using the protocol function
		const newPoints = calculateNodePoints(currentNode);
		console.log('[UI FLOW] Calculated points based on siblings:', newPoints);

		// Create a unique ID for the new node
		const newNodeId = `node_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`;
		const newNodeName = 'New Node';

		try {
			// Add the child using the protocol function (this creates and adds the node)
			addChild(
				currentNode,
				newNodeId,
				newNodeName,
				newPoints,
				[], // contributors
				[], // antiContributors
				undefined // manual fulfillment
			);

			// Update the tree in the store
			userTree.set(updatedTree);

			// Show success message
			globalState.showToast('New node added successfully', 'success');

			// Set the new node to edit mode for immediate editing
			globalState.setNodeToEditMode(newNodeId);

			console.log('[UI FLOW] Successfully added new node with ID:', newNodeId);
		} catch (error) {
			console.error('[UI FLOW] Error adding new node:', error);
			globalState.showToast('Error creating new node', 'error');
		}
	}

	// Search panel toggle (tree search)
	function toggleSearchPanel() {
		showSearchPanel = !showSearchPanel;
		if (!showSearchPanel) {
			searchQuery = '';
			selectedResultIndex = -1;
		}
	}

	// Inventory search panel toggle
	function toggleInventorySearchPanel() {
		showInventorySearchPanel = !showInventorySearchPanel;
		if (!showInventorySearchPanel) {
			globalState.inventorySearchQuery = '';
		}
	}

	// Clear all inventory filters
	function clearInventoryFilters() {
		globalState.inventorySearchQuery = '';
		globalState.inventorySelectedProvider = 'all';
		globalState.inventorySortBy = 'name';
		globalState.inventorySortDirection = 'asc';
	}

	// View switcher helpers
	const viewConfig = {
		tree: { emoji: '🌲', name: 'Tree', next: 'map' as const },
		map: { emoji: '🌍', name: 'Map', next: 'inventory' as const },
		inventory: { emoji: '📊', name: 'Inventory', next: 'tree' as const }
	};

	const currentViewConfig = $derived(viewConfig[globalState.currentView]);
	const nextViewConfig = $derived(viewConfig[currentViewConfig.next]);

	function cycleView() {
		const nextView = currentViewConfig.next;
		globalState.setView(nextView);
	}

	function handleViewPress() {
		isLongPressing = true;
		longPressTimer = window.setTimeout(() => {
			// Long press detected - show menu
			showViewMenu = true;
		}, 500); // 500ms for long press
	}

	function handleViewRelease() {
		if (longPressTimer) {
			clearTimeout(longPressTimer);
			longPressTimer = null;
		}

		// If menu didn't open (wasn't a long press), cycle the view
		if (isLongPressing && !showViewMenu) {
			cycleView();
		}

		isLongPressing = false;
	}

	function selectView(view: 'tree' | 'map' | 'inventory') {
		globalState.setView(view);
		showViewMenu = false;
	}

	// Forest panel toggle
	function toggleForestPanel() {
		showForestPanel = !showForestPanel;
		if (!showForestPanel) {
			selectedContributorId = null;
		}
	}

	// Handle contributor selection
	function selectContributor(contributorId: string | null) {
		if (contributorId === null) {
			selectedContributorId = null;
		} else {
			selectedContributorId = selectedContributorId === contributorId ? null : contributorId;
		}
	}

	// Helper function to resolve contact IDs to public keys for forest subtrees
	// This ensures we only store public keys when adding subtrees from other users
	function resolveContactIdsForForestSubtree(node: Node): Node {
		// Create a deep clone to avoid modifying the original
		const resolvedNode = structuredClone(node);

		// Helper function to resolve contributor arrays - only keep public keys
		function resolveContributorArray(contributorIds: string[]): string[] {
			return contributorIds
				.map((contributorId) => {
					// If it's already a public key (not a contact_id), keep it
					if (!contributorId.startsWith('contact_')) {
						return contributorId;
					}

					// For contact IDs, try to resolve to public key
					const resolvedPublicKey = resolveToPublicKey(contributorId);
					if (resolvedPublicKey && resolvedPublicKey !== contributorId) {
						console.log(
							`[NETWORK-SUBTREE] Resolved contact ID '${contributorId}' to public key '${resolvedPublicKey.substring(0, 20)}...'`
						);
						return resolvedPublicKey;
					}

					// If contact ID can't be resolved, exclude it from forest subtree
					// This ensures we only store public keys for forest collaboration
					console.log(
						`[NETWORK-SUBTREE] Excluding contact ID '${contributorId}' - no public key available`
					);
					return null;
				})
				.filter((id): id is string => id !== null); // Remove null entries
		}

		// Recursive function to process the tree
		function processNode(currentNode: Node): void {
			// Only NonRootNodes have contributor arrays
			if (currentNode.type === 'NonRootNode') {
				const nonRootNode = currentNode as NonRootNode;

				// Resolve contributor IDs
				if (nonRootNode.contributor_ids && nonRootNode.contributor_ids.length > 0) {
					const originalCount = nonRootNode.contributor_ids.length;
					nonRootNode.contributor_ids = resolveContributorArray(nonRootNode.contributor_ids);
					console.log(
						`[NETWORK-SUBTREE] Processed ${originalCount} → ${nonRootNode.contributor_ids.length} contributor IDs for node '${currentNode.name}'`
					);
				}

				// Resolve anti-contributor IDs
				if (nonRootNode.anti_contributors_ids && nonRootNode.anti_contributors_ids.length > 0) {
					const originalCount = nonRootNode.anti_contributors_ids.length;
					nonRootNode.anti_contributors_ids = resolveContributorArray(
						nonRootNode.anti_contributors_ids
					);
					console.log(
						`[NETWORK-SUBTREE] Processed ${originalCount} → ${nonRootNode.anti_contributors_ids.length} anti-contributor IDs for node '${currentNode.name}'`
					);
				}
			}

			// Recursively process all child nodes
			if (currentNode.children && currentNode.children.length > 0) {
				currentNode.children.forEach(processNode);
			}
		}

		// Start processing from the root
		processNode(resolvedNode);

		return resolvedNode;
	}

	// Handle adding a subtree to the current location
	function handleAddSubtree(subtreeToAdd: Node) {
		if (!tree || path.length === 0) return;

		// Get current node ID (last in path)
		const currentNodeId = path[path.length - 1];

		// Create a deep clone of the tree to ensure reactivity
		const updatedTree = structuredClone(tree);

		// Find the current node in the cloned tree
		const currentNode = findNodeById(updatedTree, currentNodeId);
		if (!currentNode) {
			globalState.showToast('Error adding subtree: Current node not found', 'error');
			return;
		}

		// Calculate initial points for new subtree using the same protocol as addNode
		const newPoints = calculateNodePoints(currentNode);

		// Create a unique ID for the new subtree root
		const newSubtreeId = `node_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`;

		try {
			// Clone the subtree to add, preserving all structure and contributor info
			const clonedSubtree = structuredClone(subtreeToAdd);

			// IMPORTANT: Resolve any contact IDs to public keys for forest subtrees
			// This ensures we only store public keys when adding subtrees from other users
			// TODO: In the future, we might resolve contact_ids of others by subscribing
			// to their stored contact_id lists to get better name resolution
			const resolvedSubtree = resolveContactIdsForForestSubtree(clonedSubtree);

			// Update the root node of the resolved subtree
			resolvedSubtree.id = newSubtreeId;
			if (resolvedSubtree.type === 'NonRootNode') {
				(resolvedSubtree as NonRootNode).points = newPoints;
			}

			// Add the resolved subtree as a child to the current node
			currentNode.children.push(resolvedSubtree);

			// Update the tree in the store
			userTree.set(updatedTree);

			// Show success message
			globalState.showToast(`Added subtree "${subtreeToAdd.name}" successfully`, 'success');

			// Close the forest panel
			toggleForestPanel();

			console.log('[TOOLBAR] Successfully added subtree with ID:', newSubtreeId);
		} catch (error) {
			console.error('[TOOLBAR] Error adding subtree:', error);
			globalState.showToast('Error adding subtree', 'error');
		}
	}

	// Create a default capacity with proper structure
	function createDefaultCapacity(): ProviderCapacity {
		const alias = $userAlias;
		const pub = $userPub;
		if (!alias || !pub) throw new Error('No user logged in');

		const todayString = today(getLocalTimeZone()).toString();
		return {
			id: crypto.randomUUID(),
			name: '',
			emoji: '',
			unit: '',
			description: '',
			max_natural_div: 1,
			max_percentage_div: 1.0,
			hidden_until_request_accepted: false,
			owner_id: pub,
			filter_rule: null,
			availability_slots: [
				{
					id: `slot-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
					quantity: 1,
					location_type: 'Undefined',
					all_day: true,
					start_date: todayString,
					start_time: null,
					end_date: null,
					end_time: null,
					time_zone: getLocalTimeZone(),
					recurrence: 'Daily',
					custom_recurrence_repeat_every: null,
					custom_recurrence_repeat_unit: null,
					custom_recurrence_end_type: null,
					custom_recurrence_end_value: null,
					latitude: undefined,
					longitude: undefined,
					street_address: undefined,
					city: undefined,
					state_province: undefined,
					postal_code: undefined,
					country: undefined
				}
			]
		};
	}

	// Add capacity to the store - following exact pattern from Capacities component
	function addCapacity(capacity: ProviderCapacity) {
		const alias = $userAlias;
		const pub = $userPub;
		if (!alias || !pub) return false;

		try {
			// Create a deep clone of current capacities
			const newCapacities: CapacitiesCollection = structuredClone($userCapacities || {});

			// Create a plain object copy of the capacity
			const plainCapacity = { ...capacity };

			// Add capacity to the collection using the protocol function
			addCapacityToCollection(newCapacities, plainCapacity);

			// Update store (Gun handles timestamps natively now)
			userCapacities.set(newCapacities);

			// Add to highlighted capacities using global state
			globalState.highlightCapacity(capacity.id);

			console.log('[TOOLBAR] Successfully added new capacity:', capacity.id);
			return true;
		} catch (error) {
			console.error('[TOOLBAR] Error adding capacity:', error);
			return false;
		}
	}

	// Create new capacity handler
	async function handleCreateCapacity() {
		const alias = $userAlias;
		const pub = $userPub;
		if (!alias || !pub) {
			globalState.showToast('Please log in to create capacities', 'error');
			return;
		}

		try {
			const newCapacity = createDefaultCapacity();

			const success = addCapacity(newCapacity);

			if (success) {
				globalState.showToast('New capacity created successfully', 'success');
			} else {
				globalState.showToast('Failed to create capacity', 'error');
			}
		} catch (error) {
			console.error('[TOOLBAR] Error creating capacity:', error);
			globalState.showToast('Error creating capacity', 'error');
		}
	}

	// Handle search result selection
	function handleSearchResultSelect(result: any) {
		// Navigate to the selected node using the path
		globalState.navigateToPath(result.navigationPath);
		toggleSearchPanel();
		globalState.showToast(`Navigated to "${result.node.name}"`, 'success');
	}
</script>

{#if shouldShowToolbar}
	<div class="toolbar-container">
		<div class="toolbar">
			{#if isMainRoute}
				<!-- Main route buttons -->
				<div class="toolbar-actions">
					<!-- View Switcher - Cycle Button with Long Press Menu -->
					<div class="view-switcher-container">
						<div class="toolbar-item">
							<button
								class="toolbar-button view-cycle-button"
								title="Tap to cycle views • Hold for menu"
								onpointerdown={handleViewPress}
								onpointerup={handleViewRelease}
								onpointercancel={handleViewRelease}
								onpointerleave={handleViewRelease}
							>
								<span class="view-emoji-container">
									{#key globalState.currentView}
										<span class="view-emoji" in:fade={{ duration: 250, delay: 100 }} out:fade={{ duration: 150 }}>
											{currentViewConfig.emoji}
										</span>
									{/key}
								</span>
							</button>
							<span class="button-caption">{currentViewConfig.name}</span>
						</div>

						<!-- Absolutely positioned separator -->
						<div class="view-separator"></div>

						<!-- View Menu (appears on long press) -->
						{#if showViewMenu}
							<div class="view-menu" bind:this={viewMenuRef}>
								<button
									class="view-menu-item"
									class:active={globalState.currentView === 'tree'}
									onclick={() => selectView('tree')}
								>
									<span class="menu-emoji">🌲</span>
									<span class="menu-label">Tree</span>
								</button>
								<button
									class="view-menu-item"
									class:active={globalState.currentView === 'map'}
									onclick={() => selectView('map')}
								>
									<span class="menu-emoji">🌍</span>
									<span class="menu-label">Map</span>
								</button>
								<button
									class="view-menu-item"
									class:active={globalState.currentView === 'inventory'}
									onclick={() => selectView('inventory')}
								>
									<span class="menu-emoji">📊</span>
									<span class="menu-label">Inventory</span>
								</button>
							</div>
						{/if}
					</div>

					<!-- Tree View Controls -->
					{#if globalState.currentView === 'tree'}
						<div class="action-controls">
							<div class="view-controls tree-controls">
							<div class="toolbar-item">
								<button class="toolbar-button add-button" title="Add new node" onclick={handleAddNode}>
									➕
								</button>
								<span class="button-caption">Add</span>
							</div>
							<div class="toolbar-item">
								<button
									class="toolbar-button edit-button"
									class:edit-active={isTextEditMode}
									title={isTextEditMode ? 'Click to turn off text edit mode' : 'Toggle text edit mode'}
									onclick={handleTextEditMode}
								>
									✏️
								</button>
								<span class="button-caption">Edit</span>
							</div>
							<div class="toolbar-item">
								<button
									class="toolbar-button recompose-button"
									class:recompose-active={isRecomposeMode}
									title={isRecomposeMode ? 'Click to turn off recompose mode' : 'Toggle recompose mode'}
									onclick={handleRecompose}
								>
									↕️
								</button>
								<span class="button-caption">Recompose</span>
							</div>

							<div class="toolbar-item">
								<button
									class="toolbar-button delete-button"
									class:delete-active={isDeleteMode}
									title={isDeleteMode ? 'Click to turn off delete mode' : 'Toggle delete mode'}
									onclick={globalState.toggleDeleteMode}
								>
									🗑️
								</button>
								<span class="button-caption">Delete</span>
							</div>

							<div class="toolbar-item">
								<button
									class="toolbar-button search-button"
									class:search-active={showSearchPanel}
									title="Search tree"
									onclick={toggleSearchPanel}
								>
									🔍
								</button>
								<span class="button-caption">Search</span>
							</div>

							<div class="toolbar-item">
								<button
									class="toolbar-button forest-button"
									class:forest-active={showForestPanel}
									title="Forest subtrees"
									onclick={toggleForestPanel}
								>
									🌳
								</button>
								<span class="button-caption">Forest</span>
							</div>
							</div>
						</div>
					{/if}

					<!-- Inventory View Controls -->
					{#if globalState.currentView === 'inventory'}
						<div class="action-controls">
							<div class="view-controls inventory-controls">
							<div class="toolbar-item">
								<button
									class="toolbar-button create-capacity-button"
									title="Create new capacity"
									onclick={handleCreateCapacity}
								>
									➕
								</button>
								<span class="button-caption">New Capacity</span>
							</div>
							<div class="toolbar-item">
								<button
									class="toolbar-button search-button"
									class:search-active={showInventorySearchPanel}
									title="Search capacities and shares"
									onclick={toggleInventorySearchPanel}
								>
									🔍
								</button>
								<span class="button-caption">Search</span>
							</div>
							</div>
						</div>
					{/if}
				</div>
			{:else if isInventoryRoute}
				<!-- Inventory route buttons -->
				<div class="toolbar-actions">
					<div class="toolbar-item">
						<button
							class="toolbar-button big-button create-capacity-button"
							title="Create new capacity"
							onclick={handleCreateCapacity}
						>
							➕
						</button>
						<span class="button-caption">New Capacity</span>
					</div>
				</div>
			{/if}
		</div>

		<!-- Forest subtrees panel for main route -->
		{#if isMainRoute && showForestPanel}
			<div class="forest-panel">
				<div class="forest-content">
					<div class="forest-body">
						{#if selectedContributorId}
							<!-- Selected contributor mode: show selected contributor on left, subtrees on right -->
							<div class="selected-contributor-section">
								{#if selectedContributorId}
									{@const selectedContributor = availableContributors.find(
										(c) => c.id === selectedContributorId
									)}
									{@const contributorColor = getColorForUserId(selectedContributorId)}
									{@const textColor = getContrastTextColor(contributorColor)}
									<button
										class="selected-contributor-item"
										style="background-color: {contributorColor}; color: {textColor}; border-color: {contributorColor};"
										onclick={() => selectContributor(null)}
										title="Click to go back to contributor selection"
									>
										<div class="contributor-name">{selectedContributor?.name}</div>
										<div class="back-hint" style="color: {textColor}; opacity: 0.8;">← Back</div>
									</button>
								{/if}
							</div>

							<div class="subtrees-section">
								<div
									class="subtrees-container"
									onwheel={(e) => {
										e.preventDefault();
										e.currentTarget.scrollLeft += e.deltaY;
									}}
								>
								{#each selectedContributorSubtrees as subtree (subtree.id)}
									{@const subtreeColor = getColorForNameHash(subtree.name, subtree.id)}
									{@const textColor = getContrastTextColor(subtreeColor)}
										<button
											class="subtree-item"
											style="background-color: {subtreeColor}; color: {textColor}; border-color: {subtreeColor};"
											onclick={() => handleAddSubtree(subtree.subtree)}
										>
											<div class="subtree-name">{subtree.name}</div>
										</button>
									{:else}
										<div class="no-subtrees">No subtrees available.</div>
									{/each}
								</div>
							</div>
						{:else}
							<!-- Contributor selection mode: horizontal scrolling contributors -->
							<div class="contributors-selection">
								<div
									class="contributors-container"
									onwheel={(e) => {
										e.preventDefault();
										e.currentTarget.scrollLeft += e.deltaY;
									}}
								>
									{#each availableContributors as contributor (contributor.id)}
										{@const contributorColor = getColorForUserId(contributor.id)}
										{@const textColor = getContrastTextColor(contributorColor)}
										<button
											class="contributor-item"
											style="background-color: {contributorColor}; color: {textColor}; border-color: {contributorColor};"
											onclick={() => selectContributor(contributor.id)}
										>
											<div class="contributor-name">{contributor.name}</div>
										</button>
									{:else}
										<div class="no-contributors">No contributors have subtrees at this path.</div>
									{/each}
								</div>
							</div>
						{/if}
					</div>
				</div>
			</div>
		{/if}
	</div>

	<!-- Search panel for main route -->
	{#if isMainRoute && showSearchPanel}
		<div class="search-panel" bind:this={searchPanelRef}>
			<div class="search-content">
				<h3>Search Tree</h3>
				<div class="search-input-container">
					<input
						type="text"
						bind:value={searchQuery}
						placeholder="Search nodes..."
						class="search-input"
					/>
				</div>

				{#if searchResults.length > 0}
					<div class="search-results">
						{#each searchResults as result, index}
							<button
								class="search-result-item"
								class:selected={index === selectedResultIndex}
								onclick={() => handleSearchResultSelect(result)}
							>
								<div class="result-name">{result.node.name}</div>
								<div class="result-path">{result.displayPath}</div>
							</button>
						{/each}
					</div>
				{:else if searchQuery.trim()}
					<div class="no-results">No results found</div>
				{/if}

				<div class="search-actions">
					<button class="close-btn" onclick={toggleSearchPanel}>Close</button>
				</div>
			</div>
		</div>
	{/if}

	<!-- Inventory search panel for main route -->
	{#if isMainRoute && showInventorySearchPanel}
		<div class="inventory-search-panel" bind:this={inventorySearchPanelRef}>
			<div class="search-content">
				<h3>Search & Filter Inventory</h3>
				
				<!-- Search Input -->
				<div class="search-input-container">
					<input
						type="text"
						bind:value={globalState.inventorySearchQuery}
						placeholder="Search capacities and shares..."
						class="search-input"
					/>
				</div>

				<!-- Filter Controls -->
				<div class="inventory-filters">
					<div class="filter-group">
						<label for="provider-filter">Provider</label>
						<select id="provider-filter" class="filter-select" bind:value={globalState.inventorySelectedProvider}>
							<option value="all">All providers ({inventoryProviders.length})</option>
							{#each inventoryProviders as provider}
								<option value={provider.id}>{provider.name}</option>
							{/each}
						</select>
					</div>

					<div class="filter-group">
						<label for="sort-by">Sort by</label>
						<select id="sort-by" class="filter-select" bind:value={globalState.inventorySortBy}>
							<option value="name">Name</option>
							<option value="allocated_slots">Allocated slots</option>
							<option value="total_slots">Total slots</option>
							<option value="provider">Provider</option>
						</select>
					</div>

					<div class="filter-group">
						<label for="sort-direction">Direction</label>
						<button
							id="sort-direction"
							class="sort-direction-btn"
							onclick={() => (globalState.inventorySortDirection = globalState.inventorySortDirection === 'asc' ? 'desc' : 'asc')}
							title="Toggle sort direction"
						>
							{globalState.inventorySortDirection === 'asc' ? '↑ Asc' : '↓ Desc'}
						</button>
					</div>
				</div>

				<!-- Clear Filters -->
				{#if globalState.inventorySearchQuery || globalState.inventorySelectedProvider !== 'all' || globalState.inventorySortBy !== 'name' || globalState.inventorySortDirection !== 'asc'}
					<button class="clear-filters-btn" onclick={clearInventoryFilters}>
						Clear all filters
					</button>
				{/if}

				<div class="search-actions">
					<button class="close-btn" onclick={toggleInventorySearchPanel}>Close</button>
				</div>
			</div>
		</div>
	{/if}
{/if}

<!-- Dragged subtree visual -->

<style>
	.toolbar-container {
		background: white;
		border-top: 1px solid #e0e0e0;
		position: relative;
		z-index: 50;
	}

	.toolbar {
		display: flex;
		justify-content: flex-start;
		align-items: center;
		padding: 8px 16px;
		background: white;
		position: relative;
		height: 62px;
		flex-shrink: 0;
	}

	.toolbar-actions {
		display: flex;
		gap: 8px;
		align-items: center;
		height: 100%;
		width: 100%;
	}

	/* View Switcher Container */
	.view-switcher-container {
		position: relative;
		display: flex;
		align-items: center;
	}

	/* Absolutely positioned separator - always in same place */
	.view-separator {
		position: absolute;
		left: 48px;
		top: 0;
		bottom: 0;
		width: 2px;
		background: rgba(33, 150, 243, 0.2);
		pointer-events: none;
	}

	.view-cycle-button {
		position: relative;
		display: flex;
		align-items: center;
		justify-content: center;
		gap: 2px;
		transition: transform 0.1s ease;
	}

	.view-cycle-button:active {
		transform: scale(0.95);
	}

	.view-emoji-container {
		position: relative;
		display: inline-block;
		width: 24px;
		height: 24px;
	}

	.view-emoji {
		position: absolute;
		top: 50%;
		left: 50%;
		transform: translate(-50%, -50%);
		font-size: 20px;
		line-height: 1;
		display: inline-block;
	}

	/* View Menu */
	.view-menu {
		position: absolute;
		bottom: calc(100% + 8px);
		left: 0;
		background: white;
		border: 1px solid #e0e0e0;
		border-radius: 8px;
		box-shadow: 0 -4px 12px rgba(0, 0, 0, 0.15);
		padding: 4px;
		z-index: 1000;
		animation: slideUp 0.2s ease-out;
		min-width: 120px;
	}

	@keyframes slideUp {
		from {
			opacity: 0;
			transform: translateY(10px);
		}
		to {
			opacity: 1;
			transform: translateY(0);
		}
	}

	.view-menu-item {
		width: 100%;
		display: flex;
		align-items: center;
		gap: 8px;
		padding: 10px 12px;
		background: none;
		border: none;
		border-radius: 4px;
		cursor: pointer;
		transition: background 0.2s ease;
		text-align: left;
	}

	.view-menu-item:hover {
		background: rgba(33, 150, 243, 0.1);
	}

	.view-menu-item.active {
		background: rgba(33, 150, 243, 0.15);
		font-weight: 600;
	}

	.menu-emoji {
		font-size: 20px;
		line-height: 1;
	}

	.menu-label {
		font-size: 14px;
		color: #333;
		font-weight: 500;
	}

	.view-menu-item.active .menu-label {
		color: #2196f3;
	}

	/* Mobile-specific view menu adjustments */
	@media (max-width: 480px) {
		.view-menu {
			position: fixed;
			bottom: 70px;
			left: 16px;
			min-width: 140px;
		}
	}

	.view-controls {
		display: flex;
		gap: 12px;
		align-items: center;
		padding: 4px 8px;
		height: 100%;
	}

	/* Center the action controls between separator and right edge */
	.action-controls {
		flex: 1;
		display: flex;
		justify-content: center;
		align-items: center;
		height: 100%;
	}

	.toolbar-item {
		display: flex;
		flex-direction: column;
		align-items: center;
		gap: 2px;
	}

	.toolbar-button {
		background: none;
		border: none;
		font-size: 20px;
		padding: 0;
		width: 30px;
		height: 30px;
		display: flex;
		align-items: center;
		justify-content: center;
		transition: transform 0.1s ease;
		cursor: pointer;
	}

	.toolbar-button:hover {
		transform: scale(1.1);
	}

	.button-caption {
		font-size: 9px;
		color: #666;
		font-weight: 500;
		text-align: center;
		white-space: nowrap;
		line-height: 1;
	}

	/* Big button style */
	.big-button {
		width: 40px !important;
		height: 40px !important;
		font-size: 24px !important;
	}

	/* Active states with animations */

	.edit-button.edit-active {
		color: #4caf50;
		background: rgba(76, 175, 80, 0.1);
		border-radius: 4px;
		animation: pulse-green 2s ease-in-out infinite;
	}

	.recompose-button.recompose-active {
		color: #1976d2;
		animation: pulse-blue 2s ease-in-out infinite;
	}

	.search-button.search-active {
		color: #1976d2;
		background: rgba(33, 150, 243, 0.1);
		border-radius: 4px;
	}

	.forest-button.forest-active {
		color: #4caf50;
		background: rgba(76, 175, 80, 0.1);
		border-radius: 4px;
	}

	.delete-button.delete-active {
		color: #d32f2f;
		animation: pulse 2s ease-in-out infinite;
	}

	/* Pulse animations */
	@keyframes pulse {
		0%,
		100% {
			box-shadow: 0 0 8px rgba(244, 67, 54, 0.3);
		}
		50% {
			box-shadow:
				0 0 16px rgba(244, 67, 54, 0.6),
				0 0 24px rgba(244, 67, 54, 0.3);
		}
	}

	@keyframes pulse-blue {
		0%,
		100% {
			box-shadow: 0 0 8px rgba(33, 150, 243, 0.3);
		}
		50% {
			box-shadow:
				0 0 16px rgba(33, 150, 243, 0.6),
				0 0 24px rgba(33, 150, 243, 0.3);
		}
	}

	@keyframes pulse-green {
		0%,
		100% {
			box-shadow: 0 0 8px rgba(76, 175, 80, 0.3);
		}
		50% {
			box-shadow:
				0 0 16px rgba(76, 175, 80, 0.6),
				0 0 24px rgba(76, 175, 80, 0.3);
		}
	}

	/* Search panel */
	.search-panel {
		position: fixed;
		bottom: 60px; /* Above toolbar */
		left: 50%;
		transform: translateX(-50%);
		background: white;
		border: 1px solid #e0e0e0;
		border-radius: 8px;
		box-shadow: 0 -4px 12px rgba(0, 0, 0, 0.15);
		width: 320px;
		max-width: 90vw;
		z-index: 1000;
	}

	.search-content {
		padding: 16px;
	}

	.search-content h3 {
		margin: 0 0 12px 0;
		font-size: 16px;
		font-weight: 600;
	}

	.search-input {
		width: 100%;
		padding: 8px 12px;
		border: 1px solid #d0d0d0;
		border-radius: 4px;
		font-size: 14px;
	}

	.search-results {
		margin-top: 12px;
		max-height: 200px;
		overflow-y: auto;
	}

	.search-result-item {
		width: 100%;
		padding: 8px;
		border: none;
		background: none;
		text-align: left;
		cursor: pointer;
		border-radius: 4px;
		margin-bottom: 4px;
	}

	.search-result-item:hover,
	.search-result-item.selected {
		background: #f5f5f5;
	}

	.result-name {
		font-weight: 500;
		margin-bottom: 2px;
	}

	.result-path {
		font-size: 12px;
		color: #666;
	}

	.no-results {
		text-align: center;
		color: #666;
		font-style: italic;
		margin-top: 12px;
	}

	.search-actions {
		margin-top: 12px;
		display: flex;
		justify-content: flex-end;
	}

	.close-btn {
		padding: 6px 12px;
		border: 1px solid #d0d0d0;
		background: white;
		border-radius: 4px;
		cursor: pointer;
		font-size: 12px;
	}

	.close-btn:hover {
		background: #f5f5f5;
	}

	/* Forest panel */
	.forest-panel {
		background: white;
		border-top: 1px solid #e0e0e0;
		box-shadow: 0 -2px 8px rgba(0, 0, 0, 0.1);
		height: 40px; /* Slightly smaller for better fit */
		overflow: hidden;
	}

	.forest-content {
		height: 100%;
		display: flex;
		flex-direction: column;
	}

	.forest-body {
		display: flex;
		flex: 1;
		min-height: 0; /* Allow shrinking */
		padding: 2px;
	}

	/* Contributors selection mode - horizontal scrolling */
	.contributors-selection {
		flex: 1;
		overflow-x: auto;
		overflow-y: hidden;
	}

	.contributors-container {
		display: flex;
		gap: 4px;
		padding: 4px;
		min-height: 30px;
		align-items: center;
		flex-wrap: nowrap;
		overflow-x: auto;
	}

	/* Selected contributor mode - left side fixed, right side scrolling */
	.selected-contributor-section {
		flex-shrink: 0;
		width: 100px; /* Smaller fixed width */
		margin-right: 4px;
		padding: 4px;
	}

	.selected-contributor-item {
		width: 100%;
		height: 26px; /* Match other items */
		padding: 2px 4px;
		border: 1px solid transparent;
		border-radius: 3px;
		cursor: pointer;
		transition: all 0.2s;
		text-align: center;
		font-size: 8px;
		overflow: hidden;
		display: flex;
		flex-direction: column;
		justify-content: center;
		align-items: center;
	}

	.selected-contributor-item:hover {
		transform: translateY(-1px);
		box-shadow: 0 2px 6px rgba(0, 0, 0, 0.15);
		opacity: 0.9;
	}

	.subtrees-section {
		flex: 1;
		overflow-x: auto;
		overflow-y: hidden;
	}

	.contributor-item {
		display: flex;
		align-items: center;
		justify-content: center;
		flex: 0 0 auto;
		width: auto;
		min-width: 0; /* Allow shrinking */
		height: 26px; /* Smaller since only showing name */
		padding: 4px 8px;
		border: 1px solid transparent;
		border-radius: 3px;
		cursor: pointer;
		transition: all 0.2s;
		text-align: center;
		font-size: 8px;
		overflow: hidden;
	}

	.contributor-item:hover {
		transform: translateY(-1px);
		box-shadow: 0 2px 6px rgba(0, 0, 0, 0.15);
		opacity: 0.9;
	}

	.contributor-name {
		font-weight: 500;
		font-size: 8px;
		line-height: 1;
		overflow: hidden;
		text-overflow: ellipsis;
		white-space: nowrap;
	}

	.back-hint {
		font-size: 6px;
		font-weight: 600;
		margin-top: 2px;
		line-height: 1;
	}

	.subtrees-container {
		display: flex;
		gap: 4px;
		overflow-x: auto;
		overflow-y: hidden;
		padding: 4px;
		height: 100%;
		align-items: center;
		flex-wrap: nowrap;
	}

	.subtree-item {
		flex: 0 0 auto;
		width: auto;
		min-width: 0; /* Allow shrinking */
		height: 26px; /* Match other items */
		padding: 4px 8px;
		border: 1px solid transparent;
		border-radius: 3px;
		cursor: pointer;
		transition: all 0.2s;
		user-select: none;
		overflow: hidden;
		display: flex;
		align-items: center;
		justify-content: center;
		text-align: center;
	}

	.subtree-item:hover {
		transform: translateY(-1px);
		box-shadow: 0 2px 6px rgba(0, 0, 0, 0.15);
		opacity: 0.9;
	}

	.subtree-item:active {
		transform: scale(0.95);
	}

	.subtree-name {
		font-weight: 600;
		font-size: 8px;
		line-height: 1;
		overflow: hidden;
		text-overflow: ellipsis;
		white-space: nowrap;
	}

	.no-contributors {
		color: #666;
		font-style: italic;
		text-align: center;
		padding: 16px 8px;
		font-size: 10px;
		min-width: 200px;
		display: flex;
		align-items: center;
		justify-content: center;
	}

	.no-subtrees {
		color: #666;
		font-style: italic;
		text-align: center;
		padding: 16px 8px;
		font-size: 10px;
		min-width: 120px;
		display: flex;
		align-items: center;
		justify-content: center;
	}

	/* Inventory search panel */
	.inventory-search-panel {
		position: fixed;
		bottom: 60px;
		left: 50%;
		transform: translateX(-50%);
		background: white;
		border: 1px solid #e0e0e0;
		border-radius: 8px;
		box-shadow: 0 -4px 12px rgba(0, 0, 0, 0.15);
		width: 400px;
		max-width: 90vw;
		max-height: 80vh;
		overflow-y: auto;
		z-index: 1000;
	}

	.inventory-filters {
		display: grid;
		grid-template-columns: 1fr 1fr auto;
		gap: 12px;
		margin-top: 16px;
		padding: 12px;
		background: #f9fafb;
		border-radius: 6px;
	}

	.filter-group {
		display: flex;
		flex-direction: column;
		gap: 4px;
	}

	.filter-group label {
		font-size: 11px;
		font-weight: 500;
		color: #6b7280;
		text-transform: uppercase;
		letter-spacing: 0.05em;
	}

	.filter-select {
		padding: 6px 8px;
		border: 1px solid #d1d5db;
		border-radius: 4px;
		font-size: 13px;
		background: white;
		color: #374151;
		cursor: pointer;
		transition: border-color 0.2s ease;
	}

	.filter-select:focus {
		outline: none;
		border-color: #3b82f6;
	}

	.sort-direction-btn {
		padding: 6px 12px;
		border: 1px solid #d1d5db;
		border-radius: 4px;
		background: white;
		color: #6b7280;
		cursor: pointer;
		font-size: 12px;
		font-weight: 600;
		transition: all 0.2s ease;
		white-space: nowrap;
		height: fit-content;
		align-self: flex-end;
	}

	.sort-direction-btn:hover {
		background: #f3f4f6;
		border-color: #9ca3af;
		color: #374151;
	}

	.clear-filters-btn {
		width: 100%;
		padding: 8px 12px;
		border: 1px solid #fca5a5;
		border-radius: 4px;
		background: #fef2f2;
		color: #dc2626;
		cursor: pointer;
		font-size: 13px;
		font-weight: 500;
		transition: all 0.2s ease;
		margin-top: 12px;
	}

	.clear-filters-btn:hover {
		background: #fee2e2;
		border-color: #f87171;
	}

	/* Mobile responsive */
	@media (max-width: 480px) {
		.toolbar {
			padding: 6px 12px;
			min-height: 48px;
		}

		.toolbar-actions {
			gap: 12px;
		}

		.toolbar-button {
			width: 28px;
			height: 28px;
			font-size: 18px;
		}

		.big-button {
			width: 36px !important;
			height: 36px !important;
			font-size: 20px !important;
		}

		.button-caption {
			font-size: 8px;
		}

		.search-panel {
			width: 280px;
		}
	}
</style>
