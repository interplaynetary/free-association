<script lang="ts">
	import { onMount } from 'svelte';
	import { getColorForUserId } from '../utils/colorUtils';
	import { browser } from '$app/environment';
	import { BottomSheet } from 'svelte-bottom-sheet';
	import type { Readable } from 'svelte/store';

	// Helper function to truncate text
	function truncateText(text: string, maxLength: number = 20): string {
		if (!text || text.length <= maxLength) return text;
		return text.slice(0, maxLength - 3) + '...';
	}

	// Props using Svelte 5 runes
	let {
		title = 'Select Item',
		searchPlaceholder = 'Search...',
		maxHeight = 0.8,
		dataProvider,
		filterText = '',
		show = false,
		select = (detail: { id: string; name: string; metadata?: any }) => {},
		close = () => {}
	} = $props<{
		title?: string;
		searchPlaceholder?: string;
		maxHeight?: number;
		dataProvider: Readable<Array<{ id: string; name: string; metadata?: any }>>;
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
	let originalBodyOverflow = $state<string>('');

	// Get filtered items based on search
	let filteredItems = $derived(() => {
		const items = $dataProvider || [];
		if (!searchFilter) return items;

		const searchLower = searchFilter.toLowerCase();
		return items.filter(
			(item: { name: string; id: string }) =>
				item.name.toLowerCase().includes(searchLower) || item.id.toLowerCase().includes(searchLower)
		);
	});

	// Update the sheet open state when show prop changes
	$effect(() => {
		isSheetOpen = show;
	});

	// Update the filter when the prop changes
	$effect(() => {
		searchFilter = filterText;
	});

	// Manage body overflow to prevent scrolling issues
	$effect(() => {
		if (browser) {
			if (isSheetOpen) {
				// Store original overflow and disable body scrolling
				originalBodyOverflow = document.body.style.overflow || '';
				document.body.style.overflow = 'hidden';
			} else {
				// Restore original overflow
				document.body.style.overflow = originalBodyOverflow;
			}
		}

		// Cleanup function to restore overflow if component is unmounted
		return () => {
			if (browser && isSheetOpen) {
				document.body.style.overflow = originalBodyOverflow;
			}
		};
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
				<h3 class="title">{title}</h3>
				<input
					type="text"
					placeholder={searchPlaceholder}
					bind:this={searchInput}
					bind:value={searchFilter}
					class="search-input"
				/>

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
								<div class="item-name" title={item.name || item.id}>
									{truncateText(item.name || item.id, 20)}
								</div>
								{#if item.metadata?.contributorCount !== undefined}
									<div class="item-meta">({item.metadata.contributorCount} contributors)</div>
								{/if}
							</div>
						</div>
					{/each}
				{/if}
			</BottomSheet.Content>
		</BottomSheet.Sheet>
	</BottomSheet.Overlay>
</BottomSheet>

<style>
	.title {
		margin: 20px 20px 16px 20px;
		font-size: 18px;
		font-weight: 600;
	}

	.search-input {
		margin: 0 20px 20px 20px;
		padding: 12px;
		border: 1px solid #ddd;
		border-radius: 8px;
		outline: none;
	}

	.message {
		padding: 24px;
		text-align: center;
		color: #666;
	}

	.item {
		padding: 16px 20px;
		cursor: pointer;
		display: flex;
		align-items: center;
	}

	.item:hover {
		background: #f8f8f8;
	}

	.color-dot {
		width: 12px;
		height: 12px;
		border-radius: 50%;
		margin-right: 12px;
	}

	.item-content {
		flex: 1;
	}

	.item-name {
		font-weight: 500;
	}

	.item-meta {
		font-size: 14px;
		color: #666;
	}
</style>
