<script lang="ts">
	import { onMount } from 'svelte';
	import { getColorForUserId } from '../utils/colorUtils';
	import { browser } from '$app/environment';
	import { BottomSheet } from 'svelte-bottom-sheet';
	import type { DropdownDataProvider } from '$lib/state.svelte';

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
		max-height: 85vh;
		width: 100%;
		max-width: 100vw;
		overflow: hidden;
	}

	.header {
		padding: 20px 24px 16px;
		border-bottom: 1px solid #e8e8e8;
		background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
		flex-shrink: 0;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
	}

	.title {
		margin: 0 0 16px 0;
		font-size: 20px;
		font-weight: 700;
		color: #1a1a1a;
		letter-spacing: -0.01em;
		line-height: 1.2;
	}

	.search-input {
		width: 100%;
		padding: 14px 18px;
		border: none;
		border-radius: 12px;
		background: #ffffff;
		box-shadow:
			inset 0 0 0 1px #e8e8e8,
			0 2px 4px rgba(0, 0, 0, 0.04);
		outline: none;
		font-size: 16px;
		color: #333;
		transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);
		box-sizing: border-box;
	}

	.search-input:focus {
		box-shadow:
			inset 0 0 0 2px #007aff,
			0 4px 12px rgba(0, 122, 255, 0.15);
		transform: translateY(-1px);
	}

	.search-input::placeholder {
		color: #999;
	}

	.results {
		overflow-y: auto;
		overflow-x: hidden;
		flex: 1;
		padding-bottom: 24px;
		scrollbar-width: thin;
		scrollbar-color: #d0d0d0 transparent;
		-webkit-overflow-scrolling: touch;
		overscroll-behavior: contain;
	}

	.results::-webkit-scrollbar {
		width: 4px;
	}

	.results::-webkit-scrollbar-track {
		background: transparent;
	}

	.results::-webkit-scrollbar-thumb {
		background-color: #d0d0d0;
		border-radius: 2px;
	}

	.results::-webkit-scrollbar-thumb:hover {
		background-color: #b0b0b0;
	}

	.message {
		padding: 32px 24px;
		text-align: center;
		color: #888;
		font-size: 16px;
		line-height: 1.4;
	}

	.item {
		padding: 18px 24px;
		cursor: pointer;
		font-size: 16px;
		border-bottom: 1px solid #f5f5f5;
		display: flex;
		align-items: flex-start;
		transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);
		min-height: 64px;
		position: relative;
		overflow: hidden;
	}

	.item:hover {
		background: linear-gradient(135deg, #f8f9ff 0%, #f0f4ff 100%);
		transform: translateX(2px);
	}

	.item:active {
		background: linear-gradient(135deg, #e8f0fe 0%, #dae8fc 100%);
		transform: translateX(1px);
	}

	.item:last-child {
		border-bottom: none;
	}

	.color-dot {
		width: 14px;
		height: 14px;
		border-radius: 50%;
		margin-right: 16px;
		margin-top: 3px;
		flex-shrink: 0;
		box-shadow: 0 1px 2px rgba(0, 0, 0, 0.1);
	}

	.item-content {
		flex: 1;
		min-width: 0;
		width: 100%;
		overflow: hidden;
	}

	.item-name {
		color: #1a1a1a;
		font-weight: 600;
		margin-bottom: 4px;
		line-height: 1.4;
		word-wrap: break-word;
		overflow-wrap: break-word;
		hyphens: auto;
		white-space: normal;
		word-break: break-word;
		max-width: 100%;
	}

	.item-meta {
		font-size: 14px;
		color: #666;
		line-height: 1.3;
		margin-top: 2px;
		word-wrap: break-word;
		overflow-wrap: break-word;
	}

	/* Enhanced mobile experience */
	@media (max-width: 768px) {
		.bottomsheet-container {
			max-height: 90vh;
		}

		.header {
			padding: 16px 20px 12px;
		}

		.title {
			font-size: 18px;
			margin-bottom: 12px;
		}

		.search-input {
			padding: 12px 16px;
			font-size: 16px; /* Prevents zoom on iOS */
			border-radius: 10px;
		}

		.item {
			padding: 16px 20px;
			min-height: 60px;
		}

		.color-dot {
			width: 12px;
			height: 12px;
			margin-right: 14px;
		}

		.item-name {
			font-size: 15px;
			line-height: 1.3;
		}

		.item-meta {
			font-size: 13px;
		}

		.message {
			padding: 24px 20px;
			font-size: 15px;
		}
	}

	/* Extra small screens */
	@media (max-width: 480px) {
		.header {
			padding: 12px 16px 10px;
		}

		.title {
			font-size: 17px;
			margin-bottom: 10px;
		}

		.search-input {
			padding: 11px 14px;
			border-radius: 8px;
		}

		.item {
			padding: 14px 16px;
			min-height: 56px;
		}

		.color-dot {
			margin-right: 12px;
		}

		.message {
			padding: 20px 16px;
		}
	}

	/* Improve text selection */
	.item-name,
	.item-meta {
		user-select: none;
		-webkit-user-select: none;
		-webkit-touch-callout: none;
	}

	/* Smooth animations */
	@media (prefers-reduced-motion: no-preference) {
		.item {
			transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);
		}

		.search-input {
			transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);
		}
	}

	@media (prefers-reduced-motion: reduce) {
		.item,
		.search-input {
			transition: none;
		}
	}
</style>
