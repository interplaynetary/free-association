<script lang="ts">
	import { onMount } from 'svelte';
	import { getColorForUserId } from '../utils/colorUtils';
	import { browser } from '$app/environment';
	import BottomSheet from 'svelte-bottom-sheet';
	import type { DropdownDataProvider } from '$lib/state.svelte';

	// Props using Svelte 5 runes
	let {
		title = 'Select Item',
		searchPlaceholder = 'Search...',
		maxHeight = 0.7,
		dataProvider,
		filterText = '',
		show = false,
		select = (detail: { id: string; name: string; metadata?: any }) => {},
		close = () => {}
	} = $props<{
		title?: string;
		searchPlaceholder?: string;
		maxHeight?: number;
		dataProvider: DropdownDataProvider;
		filterText?: string;
		show?: boolean;
		select?: (detail: { id: string; name: string; metadata?: any }) => void;
		close?: () => void;
	}>();

	// State
	let searchInput = $state<HTMLInputElement | null>(null);
	let initialized = $state(false);
	let searchFilter = $state(filterText);
	let isSheetOpen = $state(show);

	// Update the sheet open state when show prop changes
	$effect(() => {
		isSheetOpen = show;
	});

	// Update the filter when the prop changes
	$effect(() => {
		searchFilter = filterText;
	});

	// Event handlers
	function handleClose() {
		isSheetOpen = false;
		close();
	}

	function handleSelect(id: string, name: string, metadata?: any) {
		select({ id, name, metadata });
		handleClose();
	}

	// Initialize when sheet opens
	function handleSheetOpen() {
		if (!initialized) {
			// Focus search input after a brief delay
			setTimeout(() => {
				if (searchInput) {
					searchInput.focus();
				}
			}, 100);
			initialized = true;
		}
	}

	// Watch for search filter changes
	$effect(() => {
		if (searchFilter !== undefined) {
			dataProvider.search(searchFilter);
		}
	});

	// Reset initialization when sheet closes
	function handleSheetClose() {
		initialized = false;
		handleClose();
	}
</script>

<BottomSheet
	bind:isSheetOpen
	settings={{
		maxHeight,
		disableClosing: false,
		position: 'bottom'
	}}
	onopen={handleSheetOpen}
	onclose={handleSheetClose}
>
	<BottomSheet.Overlay>
		<BottomSheet.Sheet>
			<BottomSheet.Handle />
			<BottomSheet.Content>
				<div class="bottomsheet-container">
					<div class="header">
						<h3 class="title">{title}</h3>
						<input
							type="text"
							placeholder={searchPlaceholder}
							bind:this={searchInput}
							bind:value={searchFilter}
							class="search-input"
						/>
					</div>

					<div class="results">
						{#if dataProvider.loading && dataProvider.items.length === 0}
							<div class="message">Loading...</div>
						{:else if dataProvider.items.length === 0}
							<div class="message">
								{searchFilter ? 'No matching items found' : 'No items available'}
							</div>
						{:else}
							{#each dataProvider.items as item (item.id)}
								<div
									class="item"
									data-id={item.id}
									onclick={() => handleSelect(item.id, item.name, item.metadata)}
								>
									<div
										class="color-dot"
										style="background-color: {item.color || getColorForUserId(item.id)}"
									></div>
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
			</BottomSheet.Content>
		</BottomSheet.Sheet>
	</BottomSheet.Overlay>
</BottomSheet>

<style>
	.bottomsheet-container {
		display: flex;
		flex-direction: column;
		height: 100%;
		max-height: 80vh;
	}

	.header {
		padding: 16px 20px 12px;
		border-bottom: 1px solid #eee;
		background-color: #f9f9f9;
		flex-shrink: 0;
	}

	.title {
		margin: 0 0 12px 0;
		font-size: 18px;
		font-weight: 600;
		color: #333;
	}

	.search-input {
		width: 100%;
		padding: 12px 16px;
		border: none;
		border-radius: 8px;
		background: #ffffff;
		box-shadow: inset 0 0 0 1px #e0e0e0;
		outline: none;
		font-size: 16px;
		color: #333;
		transition: box-shadow 0.2s;
	}

	.search-input:focus {
		box-shadow: inset 0 0 0 2px #007aff;
	}

	.results {
		overflow-y: auto;
		overflow-x: hidden;
		flex: 1;
		padding-bottom: 20px;
		scrollbar-width: thin;
		scrollbar-color: #d0d0d0 #f5f5f5;
		-webkit-overflow-scrolling: touch;
	}

	.results::-webkit-scrollbar {
		width: 6px;
	}

	.results::-webkit-scrollbar-track {
		background: #f5f5f5;
	}

	.results::-webkit-scrollbar-thumb {
		background-color: #d0d0d0;
		border-radius: 3px;
	}

	.message {
		padding: 24px 20px;
		text-align: center;
		color: #888;
		font-size: 15px;
	}

	.item {
		padding: 16px 20px;
		cursor: pointer;
		font-size: 16px;
		border-bottom: 1px solid #f0f0f0;
		display: flex;
		align-items: center;
		transition: background 0.2s;
		min-height: 60px;
	}

	.item:hover {
		background-color: #f5f7fa;
	}

	.item:active {
		background-color: #e8f0fe;
	}

	.color-dot {
		width: 12px;
		height: 12px;
		border-radius: 50%;
		margin-right: 12px;
		flex-shrink: 0;
	}

	.item-content {
		flex: 1;
		min-width: 0;
	}

	.item-name {
		color: #333;
		font-weight: 500;
		margin-bottom: 2px;
		word-wrap: break-word;
	}

	.item-meta {
		font-size: 14px;
		color: #666;
	}

	/* Mobile-specific adjustments */
	@media (max-width: 768px) {
		.header {
			padding: 12px 16px 8px;
		}

		.title {
			font-size: 16px;
			margin-bottom: 8px;
		}

		.search-input {
			padding: 10px 12px;
			font-size: 16px; /* Prevents zoom on iOS */
		}

		.item {
			padding: 12px 16px;
			min-height: 56px;
		}

		.message {
			padding: 20px 16px;
		}
	}
</style>
