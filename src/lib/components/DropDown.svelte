<script lang="ts">
	import { onMount, onDestroy } from 'svelte';
	import { getColorForUserId } from '../utils/colorUtils';
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
		allowCreateContact = false,
		selectedIds = [],
		select = (detail: { id: string; name: string; metadata?: any }) => {},
		removeItem = (detail: { id: string; name: string; metadata?: any }) => {},
		createContact = (detail: { name: string; publicKey?: string }) => {},
		updateContact = (detail: { contactId: string; name: string }) => {},
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
		allowCreateContact?: boolean;
		selectedIds?: string[];
		select?: (detail: { id: string; name: string; metadata?: any }) => void;
		removeItem?: (detail: { id: string; name: string; metadata?: any }) => void;
		createContact?: (detail: { name: string; publicKey?: string }) => void;
		updateContact?: (detail: { contactId: string; name: string }) => void;
		close?: () => void;
	}>();

	// State
	let dropdownContainer = $state<HTMLDivElement | null>(null);
	let searchInput = $state<HTMLInputElement | null>(null);
	let resultsContainer = $state<HTMLDivElement | null>(null);
	let nameInput = $state<HTMLInputElement | null>(null);
	let initialized = $state(false);
	let searchFilter = $state(filterText);

	// Contact creation state
	let showCreateForm = $state(false);
	let createForm = $state({
		name: '',
		publicKey: ''
	});
	let isCreating = $state(false);

	// Inline naming state for converting entries to contacts
	let editingEntryId = $state<string | null>(null);
	let editingEntryName = $state('');
	let editingEntryInput = $state<HTMLInputElement | null>(null);
	let isCreatingFromEntry = $state(false);
	let editingMode = $state<'create' | 'edit'>('create'); // Track whether we're creating or editing

	// Get filtered items based on search
	let filteredItems = $derived(() => {
		const items = $dataProvider || [];
		let filtered = items;

		// Apply search filter if there's a search query
		if (searchFilter) {
			const searchLower = searchFilter.toLowerCase();
			filtered = items.filter(
				(item: { id: string; name: string; metadata?: any }) =>
					item.name.toLowerCase().includes(searchLower) ||
					item.id.toLowerCase().includes(searchLower)
			);
		}

		// Sort to show selected items first, then by selection status and name
		return filtered.sort(
			(
				a: { id: string; name: string; metadata?: any },
				b: { id: string; name: string; metadata?: any }
			) => {
				const aSelected = isItemSelected(a.id);
				const bSelected = isItemSelected(b.id);

				// Selected items first
				if (aSelected && !bSelected) return -1;
				if (!aSelected && bSelected) return 1;

				// Within selected or unselected groups, contacts first, then alphabetically
				if (a.metadata?.isContact && !b.metadata?.isContact) return -1;
				if (!a.metadata?.isContact && b.metadata?.isContact) return 1;

				// Finally, alphabetically by name
				return a.name.localeCompare(b.name);
			}
		);
	});

	// Helper function to check if an item is selected
	function isItemSelected(itemId: string): boolean {
		// Handle case where selectedIds might be a derived value function
		const ids = Array.isArray(selectedIds)
			? selectedIds
			: typeof selectedIds === 'function'
				? selectedIds()
				: [];
		return ids.includes(itemId);
	}

	// Update the filter when the prop changes
	$effect(() => {
		searchFilter = filterText;
	});

	// Helper function to determine if an entry should show the "add to contacts" button
	function shouldShowAddButton(item: { id: string; name: string; metadata?: any }): boolean {
		// Only show for non-contact entries that have meaningful Gun aliases
		return (
			!item.metadata?.isContact &&
			item.metadata?.gunAlias &&
			item.metadata.gunAlias !== item.id && // Has a real alias, not just the ID
			item.metadata.gunAlias.length > 3 // Meaningful alias length
		);
	}

	// Helper function to determine if an entry should show the "edit contact" button
	function shouldShowEditButton(item: { id: string; name: string; metadata?: any }): boolean {
		// Only show for contact entries
		return item.metadata?.isContact === true;
	}

	// Event handlers
	function handleClose() {
		showCreateForm = false;
		resetCreateForm();
		handleCancelNaming();
		close();
	}

	function handleSelect(id: string, name: string, metadata?: any) {
		select({ id, name, metadata });
		handleClose();
	}

	function handleItemClick(id: string, name: string, metadata?: any) {
		// If the item is already selected, remove it; otherwise, select it
		if (isItemSelected(id)) {
			removeItem({ id, name, metadata });
		} else {
			select({ id, name, metadata });
		}
		// Don't close dropdown when removing items, only when adding
		if (!isItemSelected(id)) {
			handleClose();
		}
	}

	// Contact creation handlers
	function handleShowCreateForm() {
		showCreateForm = true;
		// Pre-fill name with current search if it looks like a name
		if (searchFilter && searchFilter.trim() && !searchFilter.includes('@')) {
			createForm.name = searchFilter.trim();
		}

		// Focus the name input after a short delay
		setTimeout(() => {
			nameInput?.focus();
		}, 50);
	}

	function handleHideCreateForm() {
		showCreateForm = false;
		resetCreateForm();
	}

	function resetCreateForm() {
		createForm = {
			name: '',
			publicKey: ''
		};
		isCreating = false;
	}

	async function handleCreateContact() {
		if (!createForm.name.trim()) {
			return;
		}

		isCreating = true;

		try {
			await createContact({
				name: createForm.name.trim(),
				publicKey: createForm.publicKey.trim() || undefined
			});

			// Reset and close
			resetCreateForm();
			showCreateForm = false;

			// Optional: could auto-select the newly created contact here
			// but for now just close the dropdown to show the updated list
			handleClose();
		} catch (error) {
			console.error('Error creating contact:', error);
			// Keep form open so user can fix any issues
		} finally {
			isCreating = false;
		}
	}

	function handleCreateFormKeydown(event: KeyboardEvent) {
		if (event.key === 'Enter') {
			event.preventDefault();
			handleCreateContact();
		} else if (event.key === 'Escape') {
			event.preventDefault();
			handleHideCreateForm();
		}
	}

	// Inline naming handlers
	function handleStartNaming(entryId: string, event?: Event) {
		// Prevent the click from bubbling up and closing the dropdown
		if (event) {
			event.stopPropagation();
			event.preventDefault();
		}

		editingEntryId = entryId;
		editingEntryName = '';
		editingMode = 'create';

		// Focus the input after a short delay
		setTimeout(() => {
			editingEntryInput?.focus();
		}, 50);
	}

	function handleStartEditingContact(contactId: string, currentName: string, event?: Event) {
		// Prevent the click from bubbling up and closing the dropdown
		if (event) {
			event.stopPropagation();
			event.preventDefault();
		}

		editingEntryId = contactId;
		editingEntryName = currentName; // Pre-fill with current name
		editingMode = 'edit';

		// Focus the input after a short delay
		setTimeout(() => {
			editingEntryInput?.focus();
			editingEntryInput?.select(); // Select all text for easy editing
		}, 50);
	}

	function handleCancelNaming() {
		editingEntryId = null;
		editingEntryName = '';
		isCreatingFromEntry = false;
		editingMode = 'create';
	}

	function handleCancelNamingWithEvent(event: Event) {
		// Prevent event propagation
		event.stopPropagation();
		event.preventDefault();
		handleCancelNaming();
	}

	async function handleCreateFromEntry(entryId: string, entryName: string, event?: Event) {
		// Prevent event propagation
		if (event) {
			event.stopPropagation();
			event.preventDefault();
		}

		if (!entryName.trim()) {
			return;
		}

		isCreatingFromEntry = true;

		try {
			if (editingMode === 'create') {
				// Creating a new contact from Gun alias
				await createContact({
					name: entryName.trim(),
					publicKey: entryId // The entryId is the public key for non-contact entries
				});
			} else {
				// Editing an existing contact
				await updateContact({
					contactId: entryId,
					name: entryName.trim()
				});
			}

			// Reset editing state
			editingEntryId = null;
			editingEntryName = '';

			// Close the dropdown to show the updated list
			handleClose();
		} catch (error) {
			console.error(`Error ${editingMode === 'create' ? 'creating' : 'updating'} contact:`, error);
			// Keep editing state so user can fix any issues
		} finally {
			isCreatingFromEntry = false;
		}
	}

	function handleEntryKeydown(event: KeyboardEvent, entryId: string) {
		if (event.key === 'Enter') {
			event.preventDefault();
			handleCreateFromEntry(entryId, editingEntryName);
		} else if (event.key === 'Escape') {
			event.preventDefault();
			handleCancelNaming();
		}
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

	// Lifecycle
	onMount(() => {
		if (browser) {
			initialize();
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

	// Effect to manage click outside listener
	$effect(() => {
		if (!browser) return;

		if (show) {
			const handleClickOutside = (event: MouseEvent) => {
				if (dropdownContainer && !dropdownContainer.contains(event.target as Node)) {
					handleClose();
				}
			};

			// Add listener after a small delay to prevent immediate closure
			const timeoutId = setTimeout(() => {
				document.addEventListener('click', handleClickOutside);
			}, 100);

			return () => {
				clearTimeout(timeoutId);
				document.removeEventListener('click', handleClickOutside);
			};
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
			{#if allowCreateContact}
				<button
					class="create-button"
					onclick={handleShowCreateForm}
					title="Create new contact"
					disabled={showCreateForm}
				>
					+
				</button>
			{/if}
			<button class="close-button" onclick={handleClose}>×</button>
		</div>

		{#if showCreateForm}
			<div class="create-form">
				<div class="form-field">
					<input
						type="text"
						placeholder="Contact name"
						bind:this={nameInput}
						bind:value={createForm.name}
						onkeydown={handleCreateFormKeydown}
					/>
				</div>
				<div class="form-field">
					<input
						type="text"
						placeholder="Public key (optional)"
						bind:value={createForm.publicKey}
						onkeydown={handleCreateFormKeydown}
					/>
				</div>
				<div class="form-actions">
					<button class="cancel-button" onclick={handleHideCreateForm} disabled={isCreating}>
						Cancel
					</button>
					<button
						class="save-button"
						onclick={handleCreateContact}
						disabled={isCreating || !createForm.name.trim()}
					>
						{isCreating ? 'Creating...' : 'Create'}
					</button>
				</div>
			</div>
		{/if}

		<div class="results" bind:this={resultsContainer}>
			{#if !filteredItems() || filteredItems().length === 0}
				<div class="message">
					{searchFilter ? 'No matching items found' : 'No items available'}
				</div>
			{:else}
				{#each filteredItems() as item (item.id)}
					<div
						class="item"
						class:is-contact={item.metadata?.isContact}
						class:is-child-contributor={item.metadata?.isChildContributor}
						class:is-editing={editingEntryId === item.id}
						class:is-selected={isItemSelected(item.id)}
						data-id={item.id}
					>
						<div class="color-dot" style="background-color: {getColorForUserId(item.id)}"></div>

						{#if editingEntryId === item.id}
							<!-- Editing mode for this entry -->
							<div class="item-content editing">
								<div class="editing-form">
									<input
										type="text"
										placeholder={editingMode === 'create'
											? 'Enter contact name'
											: 'Edit contact name'}
										bind:this={editingEntryInput}
										bind:value={editingEntryName}
										onkeydown={(e) => handleEntryKeydown(e, item.id)}
										onclick={(e) => e.stopPropagation()}
										disabled={isCreatingFromEntry}
									/>
									<div class="editing-actions">
										<button
											class="save-entry-button"
											onclick={(e) => handleCreateFromEntry(item.id, editingEntryName, e)}
											disabled={isCreatingFromEntry || !editingEntryName.trim()}
										>
											{isCreatingFromEntry ? '...' : '✓'}
										</button>
										<button
											class="cancel-entry-button"
											onclick={handleCancelNamingWithEvent}
											disabled={isCreatingFromEntry}
										>
											×
										</button>
									</div>
								</div>
								<div class="item-meta">
									{#if editingMode === 'create'}
										@{item.metadata?.gunAlias}
									{:else}
										Contact: {item.metadata?.contactName || 'Editing contact name'}
									{/if}
								</div>
							</div>
						{:else}
							<!-- Normal display mode -->
							<div
								class="item-content"
								onclick={() => handleItemClick(item.id, item.name, item.metadata)}
							>
								<div class="item-name">
									{#if isItemSelected(item.id)}
										<span class="selected-icon">✓</span>
									{/if}
									{#if item.metadata?.isChildContributor}
										<span class="child-contributor-icon">⭐</span>
									{:else if item.metadata?.isContact}
										<span class="contact-icon">👤</span>
									{/if}
									{item.name || item.id}
									{#if isItemSelected(item.id)}
										<span class="remove-hint">click to remove</span>
									{/if}
								</div>
								{#if item.metadata?.contributorCount !== undefined}
									<div class="item-meta">({item.metadata.contributorCount} contributors)</div>
								{:else if item.metadata?.isChildContributor}
									<div class="item-meta">Current contributor</div>
								{:else if item.metadata?.isContact && item.metadata?.gunAlias && item.metadata.gunAlias !== item.name}
									<div class="item-meta">@{item.metadata.gunAlias}</div>
								{:else if !item.metadata?.isContact && item.metadata?.gunAlias && item.metadata.gunAlias !== item.name}
									<div class="item-meta">@{item.metadata.gunAlias}</div>
								{/if}
							</div>

							{#if shouldShowAddButton(item)}
								<button
									class="add-entry-button"
									onclick={(e) => handleStartNaming(item.id, e)}
									title="Add to contacts"
								>
									+
								</button>
							{/if}
							{#if shouldShowEditButton(item)}
								<button
									class="edit-entry-button"
									onclick={(e) => handleStartEditingContact(item.id, item.name, e)}
									title="Edit contact name"
								>
									✏️
								</button>
							{/if}
						{/if}
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

	.create-button {
		padding: 6px 8px;
		margin-left: 8px;
		cursor: pointer;
		color: #4caf50;
		font-weight: bold;
		font-size: 16px;
		line-height: 1;
		border-radius: 4px;
		background: transparent;
		border: 1px solid #4caf50;
		transition: all 0.2s;
		min-width: 32px;
		display: flex;
		align-items: center;
		justify-content: center;
	}

	.create-button:hover:not(:disabled) {
		background-color: #4caf50;
		color: white;
	}

	.create-button:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}

	.create-form {
		padding: 12px;
		border-bottom: 1px solid #eee;
		background-color: #fafafa;
	}

	.form-field {
		margin-bottom: 8px;
	}

	.form-field:last-of-type {
		margin-bottom: 12px;
	}

	.form-field input {
		width: 100%;
		padding: 8px 12px;
		border: 1px solid #ddd;
		border-radius: 4px;
		font-size: 14px;
		outline: none;
		transition: border-color 0.2s;
	}

	.form-field input:focus {
		border-color: #4caf50;
		box-shadow: 0 0 0 2px rgba(76, 175, 80, 0.1);
	}

	.form-actions {
		display: flex;
		gap: 8px;
		justify-content: flex-end;
	}

	.cancel-button,
	.save-button {
		padding: 6px 12px;
		border-radius: 4px;
		font-size: 13px;
		cursor: pointer;
		transition: all 0.2s;
		border: 1px solid;
	}

	.cancel-button {
		background: white;
		color: #666;
		border-color: #ddd;
	}

	.cancel-button:hover:not(:disabled) {
		background-color: #f5f5f5;
	}

	.save-button {
		background: #4caf50;
		color: white;
		border-color: #4caf50;
	}

	.save-button:hover:not(:disabled) {
		background-color: #45a049;
	}

	.cancel-button:disabled,
	.save-button:disabled {
		opacity: 0.5;
		cursor: not-allowed;
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

	.item.is-child-contributor {
		background-color: #f0f4ff;
		border-left: 3px solid #2196f3;
		font-weight: 500;
	}

	.item.is-child-contributor:hover {
		background-color: #e3f2fd;
	}

	.item.is-contact {
		background-color: #f8fffe;
		border-left: 3px solid #4caf50;
	}

	.item.is-contact:hover {
		background-color: #f0f8f0;
	}

	.item.is-selected {
		background-color: #e3f2fd;
		border-left: 3px solid #2196f3;
	}

	.item.is-selected:hover {
		background-color: #bbdefb;
		cursor: pointer;
	}

	.item.is-selected .item-content {
		cursor: pointer;
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
		display: flex;
		align-items: center;
		gap: 4px;
	}

	.selected-icon {
		color: #2196f3;
		font-weight: bold;
		font-size: 12px;
	}

	.child-contributor-icon {
		font-size: 12px;
		opacity: 0.8;
		color: #2196f3;
	}

	.contact-icon {
		font-size: 12px;
		opacity: 0.7;
	}

	.item-meta {
		font-size: 12px;
		color: #666;
	}

	.item.is-editing {
		background-color: #f8f9fa;
		border-left: 3px solid #2196f3;
	}

	.item-content.editing {
		flex: 1;
		padding: 8px 0;
	}

	.editing-form {
		display: flex;
		gap: 8px;
		align-items: center;
		margin-bottom: 4px;
	}

	.editing-form input {
		flex: 1;
		padding: 6px 10px;
		border: 1px solid #ddd;
		border-radius: 4px;
		font-size: 13px;
		outline: none;
		transition: border-color 0.2s;
	}

	.editing-form input:focus {
		border-color: #2196f3;
		box-shadow: 0 0 0 2px rgba(33, 150, 243, 0.1);
	}

	.editing-actions {
		display: flex;
		gap: 4px;
	}

	.save-entry-button,
	.cancel-entry-button {
		padding: 4px 8px;
		border-radius: 4px;
		font-size: 12px;
		cursor: pointer;
		transition: all 0.2s;
		border: 1px solid;
		min-width: 24px;
		height: 24px;
		display: flex;
		align-items: center;
		justify-content: center;
	}

	.save-entry-button {
		background: #4caf50;
		color: white;
		border-color: #4caf50;
	}

	.save-entry-button:hover:not(:disabled) {
		background-color: #45a049;
	}

	.cancel-entry-button {
		background: white;
		color: #666;
		border-color: #ddd;
	}

	.cancel-entry-button:hover:not(:disabled) {
		background-color: #f5f5f5;
	}

	.save-entry-button:disabled,
	.cancel-entry-button:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}

	.add-entry-button {
		padding: 4px 8px;
		border-radius: 4px;
		font-size: 12px;
		cursor: pointer;
		transition: all 0.2s;
		border: 1px solid #ddd;
		background: transparent;
		color: #666;
		margin-left: 8px;
		min-width: 24px;
		height: 24px;
		display: flex;
		align-items: center;
		justify-content: center;
	}

	.add-entry-button:hover:not(:disabled) {
		background-color: #f5f7fa;
		border-color: #4caf50;
		color: #4caf50;
	}

	.add-entry-button:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}

	.edit-entry-button {
		padding: 4px 8px;
		border-radius: 4px;
		font-size: 12px;
		cursor: pointer;
		transition: all 0.2s;
		border: 1px solid #ddd;
		background: transparent;
		color: #666;
		margin-left: 8px;
		min-width: 24px;
		height: 24px;
		display: flex;
		align-items: center;
		justify-content: center;
	}

	.edit-entry-button:hover:not(:disabled) {
		background-color: #f5f7fa;
		border-color: #4caf50;
		color: #4caf50;
	}

	.edit-entry-button:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}

	.remove-hint {
		font-size: 10px;
		color: #999;
		font-style: italic;
		margin-left: auto;
		opacity: 0;
		transition: opacity 0.2s;
	}

	.item.is-selected:hover .remove-hint {
		opacity: 1;
	}
</style>
