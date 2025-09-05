<script lang="ts">
	import { globalState, currentPath } from '$lib/global.svelte';
	import { userTree } from '$lib/state/core.svelte';
	import { findNodeById, addChild, calculateNodePoints } from '$lib/protocol';
	import { page } from '$app/stores';
	import { get } from 'svelte/store';
	import { base } from '$app/paths';
	import { goto } from '$app/navigation';
	import { tick } from 'svelte';
	import { searchTreeForNavigation } from '$lib/utils/treeSearch';
	import { userAlias, userPub } from '$lib/state/gun.svelte';
	import { userCapacities } from '$lib/state/core.svelte';
	import { addCapacity as addCapacityToCollection } from '$lib/protocol';
	import { getLocalTimeZone, today } from '@internationalized/date';
	import type { ProviderCapacity } from '$lib/schema';

	// Reactive store subscriptions
	const tree = $derived($userTree);
	const path = $derived($currentPath);
	const isDeleteMode = $derived(globalState.deleteMode);
	const isRecomposeMode = $derived(globalState.recomposeMode);

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

	// Search state (for main route)
	let showSearchPanel = $state(false);
	let searchQuery = $state('');
	let searchPanelRef = $state<HTMLDivElement>();
	let selectedResultIndex = $state(-1);

	// Derived search results
	const searchResults = $derived(
		searchQuery.trim() && tree ? searchTreeForNavigation(tree, searchQuery) : []
	);

	// Recompose handler
	function handleRecompose() {
		globalState.toggleRecomposeMode();
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

	// Search panel toggle
	function toggleSearchPanel() {
		showSearchPanel = !showSearchPanel;
		if (!showSearchPanel) {
			searchQuery = '';
			selectedResultIndex = -1;
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
			recipient_shares: {},
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
			const newCapacities = structuredClone($userCapacities || {});

			// Create a plain object copy of the capacity
			const plainCapacity = { ...capacity };

			// Add capacity to the collection using the protocol function
			addCapacityToCollection(newCapacities, plainCapacity);

			// Set the store with the new value
			userCapacities.set(newCapacities);

			console.log('[TOOLBAR] Successfully added new capacity:', capacity.id);
			return true;
		} catch (error) {
			console.error('[TOOLBAR] Error adding capacity:', error);
			return false;
		}
	}

	// Scroll to a specific capacity element
	function scrollToCapacity(capacityId: string) {
		// Function to attempt scrolling with retry logic
		const attemptScroll = (retries = 5) => {
			const capacityElement = document.querySelector(
				`[data-capacity-id="${capacityId}"]`
			) as HTMLElement;

			if (capacityElement) {
				// Element found, scroll to it
				capacityElement.scrollIntoView({
					behavior: 'smooth',
					block: 'center',
					inline: 'nearest'
				});

				// Add a subtle highlight animation
				capacityElement.style.transition = 'background-color 0.5s ease';
				capacityElement.style.backgroundColor = 'rgba(34, 197, 94, 0.1)';

				// Remove highlight after animation
				setTimeout(() => {
					capacityElement.style.backgroundColor = '';
				}, 2000);

				console.log('[TOOLBAR] Successfully scrolled to capacity:', capacityId);
			} else if (retries > 0) {
				// Element not found yet, retry after a short delay
				setTimeout(() => attemptScroll(retries - 1), 200);
			} else {
				console.warn('[TOOLBAR] Could not find capacity element to scroll to:', capacityId);
			}
		};

		// Start the scroll attempt
		setTimeout(() => attemptScroll(), 100);
	}

	// Create new capacity handler (for inventory route)
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

				// Navigate to inventory if not already there
				if (!isInventoryRoute) {
					await goto(`${base}/inventory`);
					// Wait for navigation to complete
					await tick();
				}

				// Scroll to the new capacity
				scrollToCapacity(newCapacity.id);
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
	<div class="toolbar">
		{#if isMainRoute}
			<!-- Main route buttons -->
			<div class="toolbar-actions">
				<div class="toolbar-item">
					<button class="toolbar-button add-button" title="Add new node" onclick={handleAddNode}>
						➕
					</button>
					<span class="button-caption">Add</span>
				</div>
				<!--
				<div class="toolbar-item">
					<button class="toolbar-button edit-button" title="Edit mode" onclick={handleEdit}>
						✏️
					</button>
					<span class="button-caption">Edit</span>
				</div>
            -->
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
{/if}

<style>
	.toolbar {
		display: flex;
		justify-content: center;
		align-items: center;
		padding: 8px 16px;
		background: white;
		border-top: 1px solid #e0e0e0;
		position: relative;
		z-index: 50;
		min-height: 46px;
	}

	.toolbar-actions {
		display: flex;
		gap: 16px;
		align-items: center;
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
	.recompose-button.recompose-active {
		color: #1976d2;
		animation: pulse-blue 2s ease-in-out infinite;
	}

	.search-button.search-active {
		color: #1976d2;
		background: rgba(33, 150, 243, 0.1);
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

	/* Search panel */
	.search-panel {
		position: absolute;
		bottom: 100%;
		left: 50%;
		transform: translateX(-50%);
		background: white;
		border: 1px solid #e0e0e0;
		border-radius: 8px;
		box-shadow: 0 -4px 12px rgba(0, 0, 0, 0.15);
		width: 320px;
		max-width: 90vw;
		z-index: 1000;
		margin-bottom: 8px;
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
