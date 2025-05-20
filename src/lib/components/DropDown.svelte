<script lang="ts">
	import { onMount, onDestroy } from 'svelte';
	import { getColorForUserId } from '../utils/colorUtils';
	import { browser } from '$app/environment';

	// Props using Svelte 5 runes
	let {
		title = 'Select User',
		searchPlaceholder = 'Search users...',
		position = { x: 0, y: 0 },
		width = 280,
		maxHeight = 320,
		excludeIds = [],
		filterText = '',
		show = false,
		select = (detail: { id: string; name: string }) => {},
		close = () => {}
	} = $props<{
		title?: string;
		searchPlaceholder?: string;
		position?: { x: number; y: number };
		width?: number;
		maxHeight?: number;
		excludeIds?: string[];
		filterText?: string;
		show?: boolean;
		select?: (detail: { id: string; name: string }) => void;
		close?: () => void;
	}>();

	// State
	let dropdownContainer = $state<HTMLDivElement | null>(null);
	let searchInput = $state<HTMLInputElement | null>(null);
	let resultsContainer = $state<HTMLDivElement | null>(null);
	let initialized = $state(false);
	let usersArray = $state<Array<{ id: string; name: string }>>([]);
	let loading = $state(true);
	let searchFilter = $state(filterText);

	// Update the filter when the prop changes
	$effect(() => {
		searchFilter = filterText;
	});

	// Function to update users list based on current filter
	async function updateUsersList() {
		loading = true;

		try {
			// Fetch trees (users) from forest API
			const response = await fetch('/api/forest');
			if (response.ok) {
				const data = await response.json();

				if (data.success) {
					// Filter trees based on search criteria and exclusions
					const filteredUsers = data.trees
						.filter((tree: any) => {
							// Skip excluded users
							if (excludeIds.includes(tree.id)) return false;

							// Apply search filter if exists
							if (searchFilter) {
								const searchLower = searchFilter.toLowerCase();
								const nameLower = (tree.name || '').toLowerCase();
								const idLower = tree.id.toLowerCase();

								return nameLower.includes(searchLower) || idLower.includes(searchLower);
							}

							return true;
						})
						.map((tree: any) => ({
							id: tree.id,
							name: tree.name || tree.id
						}));

					usersArray = filteredUsers;
				} else {
					usersArray = [];
					console.error('Failed to fetch trees:', data.message);
				}
			} else {
				usersArray = [];
				console.error('Failed to fetch trees, server returned:', response.status);
			}
		} catch (error) {
			console.error('Error fetching trees:', error);
			usersArray = [];
		} finally {
			loading = false;
		}
	}

	// Event handlers
	function handleClose() {
		close();
		show = false;
	}

	function handleSelect(id: string, name: string) {
		select({ id, name });
		handleClose();
	}

	// Initialize the component when shown
	function initialize() {
		if (!initialized && show) {
			// Update users list
			updateUsersList();

			// Focus search input
			if (searchInput) {
				setTimeout(() => searchInput?.focus(), 50);
			}

			adjustPosition();
			initialized = true;
		}
	}

	// Adjust position to stay in viewport
	function adjustPosition() {
		if (!dropdownContainer || !browser) return;

		const rect = dropdownContainer.getBoundingClientRect();
		const viewportWidth = browser ? window.innerWidth : 1024;
		const viewportHeight = browser ? window.innerHeight : 768;

		// Check right edge
		if (position.x + rect.width > viewportWidth - 10) {
			position.x = Math.max(10, viewportWidth - rect.width - 10);
		}

		// Check left edge
		if (position.x < 10) {
			position.x = 10;
		}

		// Check bottom edge
		if (position.y + rect.height > viewportHeight - 10) {
			// If dropdown would go above the top, position it at the top with padding
			if (position.y - rect.height < 10) {
				position.y = 10;
			} else {
				// Otherwise, position above the click
				position.y = position.y - rect.height - 10;
			}
		}
	}

	// Setup click outside listener
	function setupClickOutside() {
		if (!browser) return () => {};

		const handleClickOutside = (event: MouseEvent) => {
			if (dropdownContainer && !dropdownContainer.contains(event.target as Node)) {
				handleClose();
			}
		};

		document.addEventListener('click', handleClickOutside);
		return () => {
			document.removeEventListener('click', handleClickOutside);
		};
	}

	// Watch for search filter changes
	$effect(() => {
		if (initialized && searchFilter !== undefined) {
			updateUsersList();
		}
	});

	// Lifecycle
	onMount(() => {
		if (browser) {
			initialize();

			// Setup click outside handler
			const clickOutsideCleanup = show ? setupClickOutside() : undefined;

			return () => {
				if (clickOutsideCleanup) clickOutsideCleanup();
			};
		}
	});

	// Effect to run initialize when show changes
	$effect(() => {
		if (show && !initialized && browser) {
			initialize();
		}
	});

	// Effect to adjust position when position or show changes
	$effect(() => {
		if (show && position && dropdownContainer && browser) {
			setTimeout(adjustPosition, 0);
		}
	});
</script>

{#if show}
	<div
		class="dropdown-container"
		bind:this={dropdownContainer}
		style="
            top: {position.y}px; 
            left: {position.x}px; 
            width: {width}px; 
            max-height: {maxHeight}px;
        "
	>
		<div class="header">
			<input
				type="text"
				placeholder={searchPlaceholder}
				bind:this={searchInput}
				bind:value={searchFilter}
			/>
			<button class="close-button" onclick={handleClose}>×</button>
		</div>

		<div class="results" bind:this={resultsContainer}>
			{#if loading && usersArray.length === 0}
				<div class="message">Loading trees...</div>
			{:else if usersArray.length === 0}
				<div class="message">
					{searchFilter ? 'No matching trees found' : 'No trees available'}
				</div>
			{:else}
				{#each usersArray as item (item.id)}
					<div class="item" data-id={item.id} onclick={() => handleSelect(item.id, item.name)}>
						<div class="color-dot" style="background-color: {getColorForUserId(item.id)}"></div>
						<div class="item-name">{item.name || item.id}</div>
					</div>
				{/each}
			{/if}
		</div>
	</div>
{/if}

<style>
	.dropdown-container {
		position: fixed;
		background: white;
		border-radius: 8px;
		overflow: hidden;
		display: flex;
		flex-direction: column;
		z-index: 99999;
		box-shadow:
			0 6px 16px rgba(0, 0, 0, 0.12),
			0 3px 6px rgba(0, 0, 0, 0.08);
		opacity: 0;
		animation: fadeIn 150ms forwards;
	}

	@keyframes fadeIn {
		from {
			opacity: 0;
		}
		to {
			opacity: 1;
		}
	}

	.header {
		display: flex;
		align-items: center;
		border-bottom: 1px solid #eee;
		padding: 8px;
		background-color: #f9f9f9;
	}

	input {
		width: 100%;
		padding: 8px 12px;
		border: none;
		border-radius: 4px;
		background: #ffffff;
		box-shadow: inset 0 0 0 1px #e0e0e0;
		flex: 1;
		outline: none;
		font-size: 14px;
		color: #333;
	}

	.close-button {
		padding: 6px 8px;
		margin-left: 8px;
		cursor: pointer;
		color: #666;
		font-weight: bold;
		font-size: 16px;
		line-height: 1;
		border-radius: 4px;
		background: transparent;
		border: none;
		transition: background-color 0.2s;
	}

	.close-button:hover {
		background-color: #f0f0f0;
	}

	.results {
		overflow-y: auto;
		overflow-x: hidden;
		flex: 1;
		max-height: calc(var(--max-height, 320px) - 56px);
		scrollbar-width: thin;
		scrollbar-color: #d0d0d0 #f5f5f5;
		-webkit-overflow-scrolling: touch;
	}

	.results::-webkit-scrollbar {
		width: 6px;
		height: 6px;
	}

	.results::-webkit-scrollbar-track {
		background: #f5f5f5;
	}

	.results::-webkit-scrollbar-thumb {
		background-color: #d0d0d0;
		border-radius: 3px;
	}

	.message {
		padding: 16px;
		text-align: center;
		color: #888;
		font-size: 13px;
	}

	.item {
		padding: 10px 12px;
		cursor: pointer;
		font-size: 14px;
		border-bottom: 1px solid #f0f0f0;
		display: flex;
		align-items: center;
		transition: background 0.2s;
	}

	.item:hover {
		background-color: #f5f7fa;
	}

	.color-dot {
		width: 10px;
		height: 10px;
		border-radius: 50%;
		margin-right: 8px;
	}

	.item-name {
		flex: 1;
		color: #333;
	}
</style>
