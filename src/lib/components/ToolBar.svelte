<script lang="ts">
	import { globalState, currentPath } from '$lib/global.svelte';
	// V5: Import from v5 stores
	import { 
		myRecognitionTreeStore as userTree,
		myCapacitySlotsStore,
		networkCommitments,
		getNetworkCommitmentsRecord,
		setMyCapacitySlots
	} from '$lib/protocol/stores/stores.svelte';
	import { findNodeById, addChild, calculateNodePoints, getAllContributorsFromTree } from '@playnet/free-association/tree';
	import { page } from '$app/state';
	import { get } from 'svelte/store';
	import { base } from '$app/paths';
	import { searchTreeForNavigation } from '@playnet/free-association/filters/treeSearch';
	import { holsterUserAlias as userAlias, holsterUserPub as userPub } from '$lib/network/holster.svelte';
	import { getLocalTimeZone, today } from '@internationalized/date';
	// Demo tree for unauthenticated users
	import type { Commitment, Node, NonRootNode, AvailabilitySlot, NeedSlot } from '@playnet/free-association/schemas';
	import { collectiveForest } from '$lib/protocol/stores/collective-tree.svelte';
    import { types } from '$lib/protocol/resource-types';
    import { myNeedSlotsStore, setMyNeedSlots } from '$lib/protocol/stores/stores.svelte';
	import TimePatternEditor from '$lib/components/slots/TimePatternEditor.svelte';
	import LocationEditor, { type LocationData } from '$lib/components/slots/LocationEditor.svelte';
	import EmojiPicker from '$lib/components/EmojiPicker.svelte';
    import { slide } from 'svelte/transition';
	
	// V5: Wrap Commitment with id for collection storage
	type CommitmentWithId = Commitment & { id: string };
	type ProviderCapacity = CommitmentWithId;
	type CapacitiesCollection = Record<string, CommitmentWithId>;
	
	// Simple helper to add capacity to collection
	function addCapacityToCollection(collection: CapacitiesCollection, capacity: CommitmentWithId): void {
		if (capacity.id) {
			collection[capacity.id] = capacity;
		}
	}
	import { userNamesOrAliasesCache, resolveToPublicKey, getUserName } from '$lib/network/users.svelte';
	import { derived } from 'svelte/store';
	import { fade } from 'svelte/transition';
	import {
		getColorForUserId,
		getColorForNameHash,
		getContrastTextColor
	} from '$lib/utils/ui/colorUtils';
	import { t } from '$lib/translations';


	console.log('[TRACE] src/lib/components/ToolBar.svelte: <module scope>');


	// V5: Create derived stores for backward compatibility
	const userCapacities = derived([myCapacitySlotsStore], ([$slots]) => {
		// V5: Convert slots array to a collection (for compatibility)
		// Each slot becomes a commitment with capacity_slots
		const collection: CapacitiesCollection = {};
		if ($slots) {
			$slots.forEach(slot => {
				const commitment: CommitmentWithId = {
					id: slot.id,
					capacity_slots: [slot as AvailabilitySlot],
					need_slots: [],
					timestamp: Date.now(),
					itcStamp: { id: 0, event: 0 }  // Placeholder ITC stamp
				};
				collection[slot.id] = commitment;
			});
		}
		return collection;
	});

	// V5: Compute mutual contributors from tree
	const mutualContributors = derived([userTree], ([$tree]) => {
		if (!$tree) return [];
		return getAllContributorsFromTree($tree);
	});

	// V5: Network capacities from commitments
	const userNetworkCapacitiesWithSlotQuantities = derived([networkCommitments], ([$networkCommitments]) => {
		// V5: Return all network commitments as-is (they have capacity_slots)
		const allCommitments = getNetworkCommitmentsRecord();
		return allCommitments;
	});

	// Use user tree for both authenticated and unauthenticated (demo) users
	// userTree handles LocalStorage persistence for demo mode
	const isAuthenticated = $derived(!!$userPub);
	const tree = $derived($userTree);
	const path = $derived($currentPath);
	const isDeleteMode = $derived(globalState.deleteMode);
	const isRecomposeMode = $derived(globalState.recomposeMode);
	const isTextEditMode = $derived(globalState.textEditMode);

	// Route detection - properly reactive to page store changes
	const currentRoute = $derived.by(() => {
		const pathname = page.url.pathname;
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

	// Helper function to clone tree safely (handles demo tree proxy issues)
	function cloneTree(treeToClone: Node): Node {
		if (isAuthenticated) {
			// Authenticated users: use structuredClone for proper cloning
			return structuredClone(treeToClone);
		} else {
			// Demo tree: use JSON serialization to avoid proxy/clone issues
			return JSON.parse(JSON.stringify(treeToClone));
		}
	}

	// Helper function to update the appropriate tree store
	function updateTreeStore(updatedTree: Node) {
		// Always update userTree (handles both Holster and LocalStorage)
		userTree.set(updatedTree);
	}

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
	function handleClickOutside(event: MouseEvent | TouchEvent) {
		const target = event.target as HTMLElement;
		if (showViewMenu && viewMenuRef && !viewMenuRef.contains(target)) {
			const viewButton = document.querySelector('.view-cycle-button');
			if (!viewButton?.contains(target)) {
				showViewMenu = false;
			}
		}
	}

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
		// V5: Extract IDs from Contributor[] arrays {id, points}
		contributors: child.type === 'NonRootNode' 
			? (child as NonRootNode).contributors.map(c => c.id) 
			: [],
		antiContributors: child.type === 'NonRootNode' 
			? ((child as NonRootNode).anti_contributors || []).map(c => c.id) 
			: [],
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
		console.log('[TRACE] [ENTER] src/lib/components/ToolBar.svelte: handleAddNode');
		if (!tree) return;

		// Get current node ID (last in path or root)
		const currentNodeId = path.length > 0 ? path[path.length - 1] : tree.id;

		// Create a deep clone of the tree to ensure reactivity
		const updatedTree = cloneTree(tree);

		// Find the current node in the cloned tree
		const currentNode = findNodeById(updatedTree, currentNodeId);
		if (!currentNode) {
			globalState.showToast($t('errors.error_occurred'), 'error');
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

			// Update the tree in the appropriate store (demo or user tree)
			updateTreeStore(updatedTree);

			// Show success message
			globalState.showToast($t('tree.node_created'), 'success');

			// Set the new node to edit mode for immediate editing
			globalState.setNodeToEditMode(newNodeId);

			console.log('[UI FLOW] Successfully added new node with ID:', newNodeId);
		} catch (error) {
			console.error('[UI FLOW] Error adding new node:', error);
			globalState.showToast($t('errors.error_occurred'), 'error');
		}
		console.log('[TRACE] [EXIT] src/lib/components/ToolBar.svelte: handleAddNode');
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
	const viewConfig = $derived({
		map: { emoji: '🌍', name: $t('toolbar.map_view'), next: 'tree' as const },
		tree: { emoji: '🌈', name: $t('toolbar.tree_view'), next: 'inventory' as const },
		inventory: { emoji: '📊', name: $t('toolbar.inventory_view'), next: 'map' as const }
	});

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
		console.log('[TRACE] [ENTER] src/lib/components/ToolBar.svelte: resolveContactIdsForForestSubtree', { nodeId: node.id });
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

		// V5: Recursive function to process the tree with Contributor[] arrays
		function processNode(currentNode: Node): void {
			// Only NonRootNodes have contributor arrays
			if (currentNode.type === 'NonRootNode') {
				const nonRootNode = currentNode as NonRootNode;

				// V5: Resolve contributor IDs (extract from Contributor[] objects, resolve, reconstruct)
				if (nonRootNode.contributors && nonRootNode.contributors.length > 0) {
					const originalCount = nonRootNode.contributors.length;
					const contributorIds = nonRootNode.contributors.map(c => c.id);
					const resolvedIds = resolveContributorArray(contributorIds);
					// Reconstruct Contributor[] array with resolved IDs, preserving points
					nonRootNode.contributors = resolvedIds.map((id, index) => ({
						id,
						points: nonRootNode.contributors[index]?.points || 100
					}));
					console.log(
						`[NETWORK-SUBTREE] Processed ${originalCount} → ${nonRootNode.contributors.length} contributor IDs for node '${currentNode.name}'`
					);
				}

				// V5: Resolve anti-contributor IDs
				if (nonRootNode.anti_contributors && nonRootNode.anti_contributors.length > 0) {
					const originalCount = nonRootNode.anti_contributors.length;
					const antiContributorIds = nonRootNode.anti_contributors.map(c => c.id);
					const resolvedIds = resolveContributorArray(antiContributorIds);
					// Reconstruct Contributor[] array with resolved IDs, preserving points
					nonRootNode.anti_contributors = resolvedIds.map((id, index) => ({
						id,
						points: nonRootNode.anti_contributors![index]?.points || 100
					}));
					console.log(
						`[NETWORK-SUBTREE] Processed ${originalCount} → ${nonRootNode.anti_contributors.length} anti-contributor IDs for node '${currentNode.name}'`
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
		console.log('[TRACE] [ENTER] src/lib/components/ToolBar.svelte: handleAddSubtree', { subtreeId: subtreeToAdd.id });
		if (!tree) return;

		// Get current node ID (last in path or root)
		const currentNodeId = path.length > 0 ? path[path.length - 1] : tree.id;

		// Create a deep clone of the tree to ensure reactivity
		const updatedTree = cloneTree(tree);

		// Find the current node in the cloned tree
		const currentNode = findNodeById(updatedTree, currentNodeId);
		if (!currentNode) {
			globalState.showToast($t('errors.error_occurred'), 'error');
			return;
		}

		// Calculate initial points for new subtree using the same protocol as addNode
		const newPoints = calculateNodePoints(currentNode);

		// Create a unique ID for the new subtree root
		const newSubtreeId = `node_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`;

		try {
			// Clone the subtree to add, preserving all structure and contributor info
			const clonedSubtree = cloneTree(subtreeToAdd);

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

			// Update the tree in the appropriate store (demo or user tree)
			updateTreeStore(updatedTree);

			// Show success message
			globalState.showToast($t('tree.node_created'), 'success');

			// Close the forest panel
			toggleForestPanel();

			console.log('[TOOLBAR] Successfully added subtree with ID:', newSubtreeId);
		} catch (error) {
			console.error('[TOOLBAR] Error adding subtree:', error);
			globalState.showToast('Error adding subtree', 'error');
		}
		console.log('[TRACE] [EXIT] src/lib/components/ToolBar.svelte: handleAddSubtree');
	}

	// Create a default capacity with proper structure


	// Add capacity to the store - V5: Add slot to myCapacitySlotsStore
	function addCapacity(capacity: ProviderCapacity) {
		console.log('[TRACE] [ENTER] src/lib/components/ToolBar.svelte: addCapacity', { id: capacity.id });
		const alias = $userAlias;
		const pub = $userPub;
		if (!alias || !pub) return false;

		try {
			// V5: Get current slots and add the new one
			const currentSlots = get(myCapacitySlotsStore) || [];
			
			// Extract the first capacity slot from the commitment (v5 structure)
			if (capacity.capacity_slots && capacity.capacity_slots.length > 0) {
				const newSlot = capacity.capacity_slots[0];
				const updatedSlots = [...currentSlots, newSlot];
				
				// Update v5 store (Holster auto-persists)
				setMyCapacitySlots(updatedSlots);
				
				// Add to highlighted capacities using global state
				globalState.highlightCapacity(capacity.id);
				
				console.log('[TOOLBAR] Successfully added new capacity slot:', capacity.id);
				return true;
			} else {
				console.error('[TOOLBAR] Capacity has no capacity_slots');
				return false;
			}
		} catch (error) {
			console.error('[TOOLBAR] Error adding capacity:', error);
			return false;
		}
	}

	// Create new capacity handler


	function handleSearchResultSelect(result: any) {
		// Navigate to the selected node using the path
		globalState.navigateToPath(result.navigationPath);
		toggleSearchPanel();
		globalState.showToast(`Navigated to "${result.node.name}"`, 'success');
	}

    // Inventory Draft State
    let draftSlot = $state({
		name: '',
		quantity: 1,
		emoji: '📦',
        type_id: 'general',
        // Expanded Draft Fields
        recurrence: undefined as string | undefined,
        availability_window: undefined as any | undefined,
        location_type: undefined as string | undefined,
        longitude: undefined as number | undefined,
        latitude: undefined as number | undefined,
        street_address: undefined as string | undefined,
        city: undefined as string | undefined,
        state_province: undefined as string | undefined,
        postal_code: undefined as string | undefined,
        country: undefined as string | undefined,
        online_link: undefined as string | undefined,
        unit: 'units'
	});
	
	const availableUnits = [
		{ id: 'units', label: 'Units', emoji: '📦' },
		{ id: 'hours', label: 'Hours', emoji: '⏱️' },
		{ id: 'kg', label: 'Kilograms', emoji: '⚖️' },
		{ id: 'liters', label: 'Liters', emoji: '💧' },
		{ id: 'USD', label: 'USD', emoji: '💲' },
		{ id: 'meters', label: 'Meters', emoji: '📏' },
		{ id: 'km', label: 'Kilometers', emoji: '🚗' },
        { id: 'days', label: 'Days', emoji: '📅' }
	];
	let showEmojiPicker = $state(false);

	// Type auto-complete state (merged into expanded panel logic)
	let filteredTypes = $derived.by(() => {
		if (!draftSlot.name.trim()) return types; 
		const lowerName = draftSlot.name.toLowerCase();
		return types.filter(t => 
			t.label.toLowerCase().includes(lowerName) || 
			t.id.toLowerCase().includes(lowerName)
		);
	});

	function handleTypeSelect(type: any) {
		draftSlot.type_id = type.id;
		draftSlot.name = type.label; // Auto-fill name with type label
		draftSlot.emoji = type.emoji;
		
		// Close panel on select
		showExpandedDraft = false;
	}

	function handleInputFocus() {
		// Auto-open expanded panel to 'type' tab
		showExpandedDraft = true;
		expandedDraftTab = 'type';
	}
	
	function handleInputBlur() {
		// No-op: let user close panel manually or by selecting
		// If we close on blur, clicking items in the panel might fail due to race conditions
	}

    function handleUnitSelect(unit: any) {
        draftSlot.unit = unit.id;
        showExpandedDraft = false;
    }

    function handleInventoryAdd() {
        if (!draftSlot.name.trim()) return;

        if (globalState.inventoryTab === 'needs') {
             const current = get(myNeedSlotsStore) || [];
             const newSlot: NeedSlot = {
                id: `need_${Date.now()}_${Math.random()}`,
                name: draftSlot.name,
                type_id: draftSlot.type_id,
                quantity: draftSlot.quantity,
                emoji: draftSlot.emoji,
                unit: draftSlot.unit || (draftSlot.type_id === 'money' ? 'USD' : 'units'),
                max_natural_div: 1,
                min_allocation_percentage: 0.01,
                recurrence: draftSlot.recurrence as any || 'monthly',
                // Expanded fields
                availability_window: draftSlot.availability_window,
                location_type: draftSlot.location_type,
                longitude: draftSlot.longitude,
                latitude: draftSlot.latitude,
                street_address: draftSlot.street_address,
                city: draftSlot.city,
                state_province: draftSlot.state_province,
                postal_code: draftSlot.postal_code,
                country: draftSlot.country,
                online_link: draftSlot.online_link
             } as NeedSlot;
             setMyNeedSlots([...current, newSlot]);
             globalState.showToast(`Added need: ${draftSlot.name}`, 'success');
        } else {
             const current = get(myCapacitySlotsStore) || [];
             const newSlot: AvailabilitySlot = {
                id: `capacity_${Date.now()}_${Math.random()}`,
                name: draftSlot.name,
                type_id: draftSlot.type_id,
                quantity: draftSlot.quantity,
                emoji: draftSlot.emoji,
                unit: draftSlot.unit || (draftSlot.type_id === 'money' ? 'USD' : 'units'),
                max_natural_div: 1,
                min_allocation_percentage: 0.01,
                recurrence: draftSlot.recurrence as any || 'monthly',
                // Expanded fields
                availability_window: draftSlot.availability_window,
                location_type: draftSlot.location_type,
                longitude: draftSlot.longitude,
                latitude: draftSlot.latitude,
                street_address: draftSlot.street_address,
                city: draftSlot.city,
                state_province: draftSlot.state_province,
                postal_code: draftSlot.postal_code,
                country: draftSlot.country,
                online_link: draftSlot.online_link
             } as AvailabilitySlot;
             setMyCapacitySlots([...current, newSlot]);
             globalState.showToast(`Added capacity: ${draftSlot.name}`, 'success');
        }

        // Reset
        draftSlot = {
            name: '',
            quantity: 1,
            emoji: '📦',
            type_id: 'general',
            // Reset expanded fields
            recurrence: undefined,
            availability_window: undefined,
            location_type: undefined,
            longitude: undefined,
            latitude: undefined,
            street_address: undefined,
            city: undefined,
            state_province: undefined,
            postal_code: undefined,
            country: undefined,
            online_link: undefined,
            unit: 'units'
        };
        // Clear search query when adding
        globalState.inventorySearchQuery = '';
        
        showExpandedDraft = false;
        expandedDraftTab = 'time';
    }

    // Expanded Draft Panel State
    let showExpandedDraft = $state(false);
    let expandedDraftTab = $state<'type' | 'time' | 'location' | 'emoji' | 'unit'>('type');

    function toggleExpandedDraft(tab: 'type' | 'time' | 'location' | 'emoji' | 'unit') {
        if (showExpandedDraft && expandedDraftTab === tab) {
            showExpandedDraft = false;
        } else {
            showExpandedDraft = true;
            expandedDraftTab = tab;
        }
    }

    function handleTimeUpdate(recurrence: string | null, window?: any) {
        draftSlot.recurrence = recurrence || undefined;
        // Check if window is empty/undefined before assigning
        if (window && (window.time_ranges || window.day_schedules || window.week_schedules || window.month_schedules)) {
            draftSlot.availability_window = window;
        } else {
            draftSlot.availability_window = undefined;
        }
    }

    function handleLocationUpdate(location: LocationData) {
        draftSlot.location_type = location.location_type;
        draftSlot.street_address = location.street_address;
        draftSlot.city = location.city;
        draftSlot.state_province = location.state_province;
        draftSlot.postal_code = location.postal_code;
        draftSlot.country = location.country;
        draftSlot.latitude = location.latitude;
        draftSlot.longitude = location.longitude;
        draftSlot.online_link = location.online_link;
    }
</script>

<svelte:document onmousedown={handleClickOutside} ontouchstart={handleClickOutside} />

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
									<span class="menu-emoji">🌈</span>
									<span class="menu-label">values</span>
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
								<button class="toolbar-button add-button" title={$t('tree.add_node')} onclick={handleAddNode}>
									➕
								</button>
								<span class="button-caption">{$t('common.add')}</span>
							</div>
							<div class="toolbar-item">
								<button
									class="toolbar-button edit-button"
									class:edit-active={isTextEditMode}
									title={isTextEditMode ? ($t('toolbar.mode_disabled') as any).replace('{mode}', $t('toolbar.text_edit_mode')) : $t('toolbar.text_edit_mode')}
									onclick={handleTextEditMode}
								>
									✏️
								</button>
								<span class="button-caption">{$t('common.edit')}</span>
							</div>
							<div class="toolbar-item">
								<button
									class="toolbar-button recompose-button"
									class:recompose-active={isRecomposeMode}
									title={isRecomposeMode ? ($t('toolbar.mode_disabled') as any).replace('{mode}', $t('toolbar.recompose')) : $t('toolbar.recompose_mode')}
									onclick={handleRecompose}
								>
									↕️
								</button>
								<span class="button-caption">{$t('toolbar.recompose')}</span>
							</div>

							<div class="toolbar-item">
								<button
									class="toolbar-button delete-button"
									class:delete-active={isDeleteMode}
									title={isDeleteMode ? ($t('toolbar.mode_disabled') as any).replace('{mode}', $t('toolbar.delete_mode')) : $t('toolbar.delete_mode')}
									onclick={globalState.toggleDeleteMode}
								>
									🗑️
								</button>
								<span class="button-caption">{$t('common.delete')}</span>
							</div>

							<div class="toolbar-item">
								<button
									class="toolbar-button search-button"
									class:search-active={showSearchPanel}
									title={$t('toolbar.search_tree')}
									onclick={toggleSearchPanel}
								>
									🔍
								</button>
								<span class="button-caption">{$t('common.search')}</span>
							</div>

							<div class="toolbar-item">
								<button
									class="toolbar-button forest-button"
									class:forest-active={showForestPanel}
									title={$t('toolbar.forest_view')}
									onclick={toggleForestPanel}
								>
								💞
								</button>
								<span class="button-caption">Playnet</span>
							</div>
							</div>
						</div>
					{/if}

					<!-- Inventory View Controls (Also shown in Map View) -->
					{#if globalState.currentView === 'inventory' || globalState.currentView === 'map'}
						<div class="action-controls inventory-controls">
							<!-- Needs/Capacity Toggle (Vertical Tabs) -->
							<div class="inventory-type-toggle">
								<button 
									class="type-tab {globalState.inventoryTab === 'needs' ? 'active' : ''}"
									onclick={() => globalState.inventoryTab = 'needs'}
									title="Needs"
								>
									🎯
								</button>
								<button 
									class="type-tab {globalState.inventoryTab === 'capacity' ? 'active' : ''}"
									onclick={() => globalState.inventoryTab = 'capacity'}
									title="Capacity"
								>
									🎁
								</button>
							</div>

							<!-- Draft Controls -->
							<div class="inventory-draft-controls">
								<!-- Type Selector (Hidden, merged into input) -->
								<!-- <select bind:value={draftSlot.type_id} class="draft-select">
									{#each types as t}
										<option value={t.id}>{t.emoji} {t.label}</option>
									{/each}
								</select> -->

								<!-- Name Input (Also acts as search) -->
								<!-- Name Input (Also acts as search) -->
								<div class="draft-input-wrapper relative">
									<!-- Popover removed in favor of Expanded Panel 'type' tab -->
									<input 
										type="text" 
										bind:value={draftSlot.name} 
                                        oninput={(e) => globalState.inventorySearchQuery = (e.target as HTMLInputElement).value}
										onfocus={handleInputFocus}
										placeholder={globalState.inventoryTab === 'needs' ? "Advocate need..." : "Share capacity..."}
										class="draft-input-name"
										onkeydown={(e) => e.key === 'Enter' && handleInventoryAdd()}
									/>
								</div>

								<!-- Quantity & Emoji -->
								<div class="draft-qty-group">
									<button 
										class="emoji-display-btn" 
										onclick={() => toggleExpandedDraft('emoji')}
										title="Choose Emoji"
									>
										{draftSlot.emoji}
									</button>
									<input 
										type="number" 
										bind:value={draftSlot.quantity} 
										min="0" 
										class="draft-input-qty"
									/>
                                    <button
                                        class="unit-btn"
                                        onclick={() => toggleExpandedDraft('unit')}
                                        title="Choose Unit"
                                    >
                                        {availableUnits.find(u => u.id === draftSlot.unit)?.label || draftSlot.unit || 'Units'}
                                    </button>
								</div>

								<!-- Expanded Draft Toggles -->
								<div class="draft-expander-group">

									
									<!-- Emoji button removed (moved to input group) -->
									<button 
										class="toolbar-button expand-btn"
										class:active={showExpandedDraft && expandedDraftTab === 'time'}
										title="Add Time Details"
										onclick={() => toggleExpandedDraft('time')}
									>
										🕐
									</button>
									<button 
										class="toolbar-button expand-btn"
										class:active={showExpandedDraft && expandedDraftTab === 'location'}
										title="Add Location"
										onclick={() => toggleExpandedDraft('location')}
									>
										📍
									</button>
								</div>
								
								<!-- Add Button -->
								<button class="toolbar-button add-inventory-btn" onclick={handleInventoryAdd}>
									➕
								</button>
							</div>

							<!-- Inventory Search (Removed in favor of unified input) -->
							<!-- <div class="search-container">
								<button class="toolbar-button search-button" title="Search Inventory" onclick={toggleInventorySearchPanel}>
									🔍
								</button>
							</div> -->
						</div>

						<!-- Expanded Draft Panel -->
						{#if showExpandedDraft}
							<div class="expanded-draft-panel" transition:slide={{ axis: 'y', duration: 200 }}>
								<div class="expanded-header">
									<h4>
										{#if expandedDraftTab === 'type'}
											Choose Type
										{:else if expandedDraftTab === 'time'}
											Time Details
										{:else if expandedDraftTab === 'location'}
											Location Details

                                        {:else if expandedDraftTab === 'unit'}
                                            Choose Unit
										{:else}
											Choose Emoji
										{/if}
									</h4>
									<button class="close-expanded-btn" onclick={() => showExpandedDraft = false}>✕</button>
								</div>
								
								<div class="expanded-content">
									{#if expandedDraftTab === 'type'}
										{@const groupedTypes = (() => {
											const groups: Record<string, typeof filteredTypes> = {};
											const categoryOrder: string[] = [];

											filteredTypes.forEach(type => {
												const cat = type.category || 'Other';
												if (!groups[cat]) {
													groups[cat] = [];
													categoryOrder.push(cat);
												}
												groups[cat].push(type);
											});

											return categoryOrder.map(cat => ({
												name: cat,
												types: groups[cat]
											}));
										})()}
										<div class="category-list">
											{#each groupedTypes as category}
												<div class="category-section">
													<h5 class="category-title">{category.name}</h5>
													<div class="type-grid">
														{#each category.types as type}
															<button 
																class="type-grid-item" 
																onclick={() => handleTypeSelect(type)}
															>
																<span class="type-emoji">{type.emoji}</span>
																<span class="type-label">{type.label}</span>
															</button>
														{/each}
													</div>
												</div>
											{/each}
											{#if filteredTypes.length === 0}
												<div class="no-results">No types found matching "{draftSlot.name}"</div>
											{/if}
										</div>
									{:else if expandedDraftTab === 'emoji'}
										<EmojiPicker onSelect={(emoji) => { draftSlot.emoji = emoji; showExpandedDraft = false; }} />
									{:else if expandedDraftTab === 'time'}
										<TimePatternEditor 
											recurrence={draftSlot.recurrence as any}
											startDate={null}
											endDate={null}
											availabilityWindow={draftSlot.availability_window}
											onUpdate={handleTimeUpdate}
										/>
									{:else if expandedDraftTab === 'location'}
										<LocationEditor 
											locationType={draftSlot.location_type}
											streetAddress={draftSlot.street_address}
											city={draftSlot.city}
											stateProvince={draftSlot.state_province}
											postalCode={draftSlot.postal_code}
											country={draftSlot.country}
											latitude={draftSlot.latitude}
											longitude={draftSlot.longitude}
											onlineLink={draftSlot.online_link}
											onUpdate={handleLocationUpdate}
										/>

                                    {:else if expandedDraftTab === 'unit'}
                                        <div class="type-grid">
                                            {#each availableUnits as unit}
                                                <button 
                                                    class="type-grid-item" 
                                                    onclick={() => handleUnitSelect(unit)}
                                                >
                                                    <span class="type-emoji">{unit.emoji}</span>
                                                    <span class="type-label">{unit.label}</span>
                                                </button>
                                            {/each}
                                        </div>
									{/if}
								</div>
							</div>
						{/if}
					{/if}
				</div>
			{/if}



		<!-- Footer links and copyright (right side) -->
		<!--<div class="toolbar-footer"> -->
			<!-- <span class="demo-version">Demo v 0.5.2</span>
			<a href="{base}/terms" class="footer-link">Terms</a>
			<span class="footer-separator">•</span>
			<a href="{base}/privacy" class="footer-link">Privacy</a>
			<span class="footer-separator">•</span> 
			<span class="copyright">© Playnet</span>--> 
		<!--</div> -->
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
		width: 100%;
		/* overflow: hidden; Removed to allow expanded panel to show */
	}

	.toolbar {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 8px 16px;
		background: white;
		position: relative;
		height: 62px;
		flex-shrink: 0;
		width: 100%;
		max-width: 100%;
		box-sizing: border-box;
	}

	.toolbar-actions {
		display: flex;
		gap: 8px;
		align-items: center;
		height: 100%;
		flex: 1;
		min-width: 0;
		overflow: hidden;
	}

	.toolbar-footer {
		display: flex;
		flex-direction: column;
		align-items: flex-end;
		gap: 2px;
		font-size: 9px;
		color: #999;
		flex-shrink: 1;
		min-width: 0;
		overflow: hidden;
	}

	.demo-version {
		color: #666;
		font-size: 10px;
		font-weight: 600;
		line-height: 1.2;
	}

	.footer-link {
		color: #999;
		text-decoration: none;
		transition: color 0.2s ease;
		line-height: 1.2;
	}

	.footer-link:hover {
		color: #666;
		text-decoration: underline;
	}

	.footer-separator {
		display: none;
	}

	.copyright {
		color: #bbb;
		font-size: 8px;
		line-height: 1.2;
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
		flex-shrink: 1;
		min-width: 0;
		overflow: hidden;
	}

	/* Center the action controls between separator and right edge */
	.action-controls {
		flex: 1;
		display: flex;
		justify-content: center;
		align-items: center;
		height: 100%;
		min-width: 0;
		overflow: hidden;
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

		.toolbar-footer {
			font-size: 7px;
			gap: 1px;
		}

		.demo-version {
			font-size: 9px;
		}

		.copyright {
			font-size: 7px;
		}
	}
	/* Inventory Controls */
	.inventory-controls {
		display: flex;
		gap: 8px;
		align-items: center;
		height: 100%;
        flex: 1;
        justify-content: center;
	}

	.inventory-type-toggle {
		display: flex;
		flex-direction: column;
		gap: 2px;
        margin-right: 8px;
        margin-left: 12px;
	}

	.type-tab {
		background: none;
		border: 1px solid transparent;
		font-size: 14px;
		padding: 0 4px;
		cursor: pointer;
		opacity: 0.5;
		transition: all 0.2s;
        line-height: 1;
        border-radius: 4px;
	}

	.type-tab:hover {
		opacity: 0.8;
        background: rgba(0,0,0,0.05);
	}

	.type-tab.active {
		opacity: 1;
		font-size: 16px;
        background: rgba(33, 150, 243, 0.1);
	}

	.inventory-draft-controls {
		display: flex;
		align-items: center;
		gap: 2px;
		background: #f5f5f5;
		padding: 0 4px;
		border-radius: 20px;
        height: 28px;
	}

	.draft-select {
		background: transparent;
		border: none;
		font-size: 10px;
		max-width: 65px;
        cursor: pointer;
        padding: 0;
        outline: none;
	}

	.draft-input-name {
		border: none;
		background: transparent;
		font-size: 11px;
		width: 100px;
		outline: none;
        padding: 0 4px;
	}

	.draft-qty-group {
		display: flex;
		align-items: center;
		background: white;
		border-radius: 10px;
		padding: 0 2px;
		border: 1px solid #ddd;
        height: 22px;
	}

	.emoji-btn {
		background: none;
		border: none;
		font-size: 12px;
		padding: 0 2px;
		cursor: pointer;
        line-height: 1;
	}

	.emoji-display-btn {
		background: none;
		border: none;
		font-size: 12px;
		padding: 0 2px;
		line-height: 1;
		display: inline-block;
        cursor: pointer;
	}

    .unit-btn {
        background: none;
        border: none;
        font-size: 10px;
        padding: 0 4px;
        line-height: 1;
        display: inline-block;
        cursor: pointer;
        color: #666;
        border-left: 1px solid #eee;
        height: 14px;
        margin-left: 2px;
        white-space: nowrap;
        max-width: 60px;
        overflow: hidden;
        text-overflow: ellipsis;
    }

    .unit-btn:hover {
        color: #333;
        background-color: #f5f5f5;
        border-radius: 2px;
    }

    .emoji-picker-container {
        position: absolute;
        bottom: 100%;
        left: 0;
        margin-bottom: 8px;
        z-index: 9999;
        min-width: 320px;
    }

	.draft-input-qty {
		width: 32px;
		border: none;
		text-align: center;
        outline: none;
        font-size: 10px;
        /* Hide spin buttons */
        -moz-appearance: textfield;
        padding: 0;
	}
    
    .draft-input-qty::-webkit-outer-spin-button,
    .draft-input-qty::-webkit-inner-spin-button {
        -webkit-appearance: none;
        margin: 0;
    }

	.add-inventory-btn {
		font-size: 12px !important;
		width: 20px !important;
		height: 20px !important;
		background: #4caf50 !important;
		color: white !important;
		border-radius: 50% !important;
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
        padding: 0 !important;
	}
    
    .relative {
        position: relative;
    }
	/* Expanded Draft Panel */
	.expanded-draft-panel {
		position: absolute;
		bottom: 100%;
		left: 0;
		right: 0;
		background: white;
		border-top: 1px solid #e2e8f0;
		box-shadow: 0 -4px 6px -1px rgba(0, 0, 0, 0.1);
		z-index: 50; /* Above toolbar */
		max-height: 80vh;
		overflow-y: auto;
		display: flex;
		flex-direction: column;
        margin-bottom: 1px; /* Separation line */
	}

	.expanded-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 0.75rem 1rem;
		border-bottom: 1px solid #f1f5f9;
        background: #f8fafc;
	}

	.expanded-header h4 {
		margin: 0;
		font-size: 0.875rem;
		font-weight: 600;
		color: #334155;
	}

	.close-expanded-btn {
		background: transparent;
		border: none;
		color: #94a3b8;
		font-size: 1.25rem;
		cursor: pointer;
		padding: 0 0.5rem;
	}

	.close-expanded-btn:hover {
		color: #64748b;
	}

	.expanded-content {
		padding: 1rem;
	}

	.draft-expander-group {
		display: flex;
		gap: 1px;
		margin-right: 4px;
        border-left: 1px solid #e2e8f0;
        padding-left: 4px;
	}

	.expand-btn {
		font-size: 12px;
		width: 22px;
		height: 22px;
		display: flex;
		align-items: center;
		justify-content: center;
		border-radius: 6px;
		background: transparent;
        opacity: 0.6;
        padding: 0;
	}

	.expand-btn:hover {
		background: #f1f5f9;
        opacity: 1;
	}

	.expand-btn.active {
		background: #e0f2fe;
		opacity: 1;
	}
	.expand-btn.active {
		background: #e0f2fe;
		opacity: 1;
	}

	.type-grid {
		display: grid;
		grid-template-columns: repeat(auto-fill, minmax(100px, 1fr));
		gap: 8px;
	}

	.type-grid-item {
		display: flex;
		flex-direction: column;
		align-items: center;
		padding: 8px;
		background: #f8fafc;
		border: 1px solid #e2e8f0;
		border-radius: 6px;
		cursor: pointer;
		transition: all 0.2s;
	}

	.type-grid-item:hover {
		background: #e0f2fe;
		border-color: #7dd3fc;
		transform: translateY(-1px);
	}

	.type-emoji {
		font-size: 20px;
		margin-bottom: 4px;
	}

	.type-label {
		font-size: 11px;
		font-weight: 500;
		color: #334155;
		text-align: center;
	}

    .category-list {
        display: flex;
        flex-direction: column;
        gap: 12px;
        padding-bottom: 12px;
    }

    .category-section {
        display: flex;
        flex-direction: column;
        gap: 6px;
    }

    .category-title {
        font-size: 11px;
        text-transform: uppercase;
        color: #888;
        font-weight: 600;
        margin: 0;
        letter-spacing: 0.5px;
        padding-left: 2px;
    }
</style>

