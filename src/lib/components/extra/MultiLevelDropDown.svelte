<script lang="ts">
	import { onMount, onDestroy } from 'svelte';
	import { getColorForUserId } from '../../utils/colorUtils';
	import { browser } from '$app/environment';
	import type { Readable } from 'svelte/store';

	// Props using Svelte 5 runes
	let {
		title = 'Select Item',
		searchPlaceholder = 'Search...',
		position = { x: 0, y: 0 },
		width = 280,
		maxHeight = 320,
		dataProvider,
		filterText = '',
		show = false,
		select = (detail: { id: string; name: string; metadata?: any }) => {},
		close = () => {}
	} = $props<{
		title?: string;
		searchPlaceholder?: string;
		position?: { x: number; y: number };
		width?: number;
		maxHeight?: number;
		dataProvider: Readable<Array<{ id: string; name: string; metadata?: any }>>;
		filterText?: string;
		show?: boolean;
		select?: (detail: { id: string; name: string; metadata?: any }) => void;
		close?: () => void;
	}>();

	// State
	let dropdownContainer = $state<HTMLDivElement | null>(null);
	let searchInput = $state<HTMLInputElement | null>(null);
	let resultsContainer = $state<HTMLDivElement | null>(null);
	let initialized = $state(false);
	let searchFilter = $state(filterText);

	// Get filtered items based on search
	let filteredItems = $derived(() => {
		const items = $dataProvider || [];
		if (!searchFilter) return items;

		const searchLower = searchFilter.toLowerCase();
		return items.filter(
			(item: { id: string; name: string; metadata?: any }) =>
				item.name.toLowerCase().includes(searchLower) || item.id.toLowerCase().includes(searchLower)
		);
	});

	// Update the filter when the prop changes
	$effect(() => {
		searchFilter = filterText;
	});

	// Event handlers
	function handleClose() {
		close();
	}

	function handleSelect(id: string, name: string, metadata?: any) {
		select({ id, name, metadata });
		handleClose();
	}

	// Initialize the component when shown
	function initialize() {
		if (!initialized && show) {
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
		console.log('DropDown show effect triggered:', { show, initialized, browser });
		if (show && !initialized && browser) {
			console.log('Initializing dropdown...');
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
			{#if !filteredItems() || filteredItems().length === 0}
				<div class="message">
					{searchFilter ? 'No matching items found' : 'No items available'}
				</div>
			{:else}
				{#each filteredItems() as item (item.id)}
					<div
						class="item"
						data-id={item.id}
						onclick={() => handleSelect(item.id, item.name, item.metadata)}
					>
						<div class="color-dot" style="background-color: {getColorForUserId(item.id)}"></div>
						<div class="item-content">
							<div class="item-name">{item.name || item.id}</div>
							{#if item.metadata?.contributorCount !== undefined}
								<div class="item-meta">({item.metadata.contributorCount} contributors)</div>
							{/if}
						</div>
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

	.item-content {
		flex: 1;
	}

	.item-name {
		flex: 1;
		color: #333;
	}

	.item-meta {
		font-size: 12px;
		color: #666;
	}
</style>
