<script lang="ts">
	import { onMount } from 'svelte';
	import { getColorForUserId } from '../utils/colorUtils';
	import {
		resolveToPublicKey,
		userAliasesCache,
		updateContact as updateContactInStore,
		createCollectiveTarget,
		getUserName
	} from '$lib/state/users.svelte';
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
		allowCreateCollective = false,
		collectiveMode = false,
		selectedIds = [],
		select = (detail: { id: string; name: string; metadata?: any }) => {},
		removeItem = (detail: { id: string; name: string; metadata?: any }) => {},
		createContact = (detail: { name: string; publicKey?: string }) => {},
		createCollective = (detail: { name: string; memberIds: string[] }) => {},
		updateContact = (detail: { contactId: string; name: string }) => {},
		deleteContact = (detail: { contactId: string; name: string; publicKey?: string }) => {},
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
		allowCreateCollective?: boolean;
		collectiveMode?: boolean;
		selectedIds?: string[];
		select?: (detail: { id: string; name: string; metadata?: any }) => void;
		removeItem?: (detail: { id: string; name: string; metadata?: any }) => void;
		createContact?: (detail: { name: string; publicKey?: string }) => void;
		createCollective?: (detail: { name: string; memberIds: string[] }) => void;
		updateContact?: (detail: { contactId: string; name: string }) => void;
		deleteContact?: (detail: { contactId: string; name: string; publicKey?: string }) => void;
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
		name: ''
	});
	let isCreating = $state(false);

	// Collective creation state
	let showCreateCollectiveForm = $state(false);
	let createCollectiveForm = $state({
		name: '',
		selectedMembers: [] as string[]
	});
	let isCreatingCollective = $state(false);

	// Inline naming state for converting entries to contacts
	let editingEntryId = $state<string | null>(null);
	let editingEntryName = $state('');
	let editingEntryInput = $state<HTMLInputElement | null>(null);
	let isCreatingFromEntry = $state(false);
	let editingMode = $state<'create' | 'edit'>('create'); // Track whether we're creating or editing

	// Alias selection state for contact editing
	let editingContactAlias = $state('');
	let editingContactAliasInput = $state<HTMLInputElement | null>(null);
	let showAliasDropdown = $state(false);
	let selectedAliasIndex = $state(0);

	// Create a filtered aliases store that reactively filters available aliases
	let filteredAliases = $derived(() => {
		if (!editingContactAlias.trim() || editingMode !== 'edit') {
			return [];
		}

		const aliases = $userAliasesCache || {};
		const searchTerm = editingContactAlias.toLowerCase();

		return Object.entries(aliases)
			.filter(
				([pubkey, alias]) =>
					alias.toLowerCase().includes(searchTerm) && alias.toLowerCase() !== searchTerm // Don't show exact matches
			)
			.map(([pubkey, alias]) => ({ pubkey, alias }))
			.slice(0, 10); // Limit to 10 results
	});

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

		// Simple check first for direct match
		if (ids.includes(itemId)) {
			return true;
		}

		// Get the current item's metadata to understand its type
		const items = $dataProvider || [];
		const currentItem = items.find((item: { id: string; metadata?: any }) => item.id === itemId);
		if (!currentItem) return false;

		// For more intelligent matching, check if this person is represented
		// by a different ID in the contributors list
		if (currentItem.metadata?.isContact) {
			// This is a contact item - check if its public key is in the contributors list
			const publicKey = currentItem.metadata?.userId;
			if (publicKey && publicKey !== itemId && ids.includes(publicKey)) {
				return true;
			}
		} else {
			// This is a user item (public key) - check if any contact ID that resolves to this public key is selected
			const publicKey = itemId;
			for (const contributorId of ids) {
				if (contributorId.startsWith('contact_')) {
					const resolvedPublicKey = resolveToPublicKey(contributorId);
					if (resolvedPublicKey === publicKey) {
						return true;
					}
				}
			}
		}

		return false;
	}

	// Update the filter when the prop changes, but only after initial setup
	$effect(() => {
		// Only sync with prop after the dropdown has been initialized
		// This prevents restoring old search values when opening fresh
		if (initialized) {
			searchFilter = filterText;
		}
	});

	// Helper function to determine if an entry should show the "add to contacts" button
	function shouldShowAddButton(item: { id: string; name: string; metadata?: any }): boolean {
		// Show for all non-contact entries - users should be able to add anyone to contacts
		return !item.metadata?.isContact;
	}

	// Helper function to determine if an entry should show the "edit contact" button
	function shouldShowEditButton(item: { id: string; name: string; metadata?: any }): boolean {
		// Only show for contact entries
		return item.metadata?.isContact === true;
	}

	// Helper function to determine if an entry should show the "delete contact" button
	function shouldShowDeleteButton(item: { id: string; name: string; metadata?: any }): boolean {
		// Only show for contact entries
		return item.metadata?.isContact === true;
	}

	// Event handlers
	function handleClose() {
		showCreateForm = false;
		showCreateCollectiveForm = false;
		resetCreateForm();
		resetCreateCollectiveForm();
		handleCancelNaming();
		initialized = false; // Reset so dropdown initializes fresh next time
		close();
	}

	function handleSelect(id: string, name: string, metadata?: any) {
		select({ id, name, metadata });
		handleClose();
	}

	function handleItemClick(id: string, name: string, metadata?: any) {
		// In collective mode, allow multi-selection without closing
		if (collectiveMode) {
			if (isItemSelected(id)) {
				removeItem({ id, name, metadata });
			} else {
				select({ id, name, metadata });
			}
			// Don't close dropdown in collective mode
			return;
		}

		// Original single-selection behavior
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
			name: ''
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
				name: createForm.name.trim()
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

	// Collective creation handlers
	function handleShowCreateCollectiveForm() {
		showCreateCollectiveForm = true;
		// Pre-populate with currently selected items
		const currentlySelected = Array.isArray(selectedIds)
			? selectedIds
			: typeof selectedIds === 'function'
				? selectedIds()
				: [];

		createCollectiveForm.selectedMembers = [...currentlySelected];

		// Pre-fill name with current search if it looks like a name
		if (searchFilter && searchFilter.trim() && !searchFilter.includes('@')) {
			createCollectiveForm.name = searchFilter.trim();
		}

		// Focus the name input after a short delay
		setTimeout(() => {
			nameInput?.focus();
		}, 50);
	}

	function handleHideCreateCollectiveForm() {
		showCreateCollectiveForm = false;
		resetCreateCollectiveForm();
	}

	function resetCreateCollectiveForm() {
		createCollectiveForm = {
			name: '',
			selectedMembers: []
		};
		isCreatingCollective = false;
	}

	async function handleCreateCollective() {
		if (!createCollectiveForm.name.trim() || createCollectiveForm.selectedMembers.length < 2) {
			return;
		}

		isCreatingCollective = true;

		try {
			await createCollective({
				name: createCollectiveForm.name.trim(),
				memberIds: createCollectiveForm.selectedMembers
			});

			// Reset and close
			resetCreateCollectiveForm();
			showCreateCollectiveForm = false;

			// Close the dropdown to show the updated list
			handleClose();
		} catch (error) {
			console.error('Error creating collective:', error);
			// Keep form open so user can fix any issues
		} finally {
			isCreatingCollective = false;
		}
	}

	function handleCreateCollectiveFormKeydown(event: KeyboardEvent) {
		if (event.key === 'Enter') {
			event.preventDefault();
			handleCreateCollective();
		} else if (event.key === 'Escape') {
			event.preventDefault();
			handleHideCreateCollectiveForm();
		}
	}

	function handleToggleMemberInCollective(memberId: string) {
		const index = createCollectiveForm.selectedMembers.indexOf(memberId);
		if (index > -1) {
			createCollectiveForm.selectedMembers = createCollectiveForm.selectedMembers.filter(
				(id) => id !== memberId
			);
		} else {
			createCollectiveForm.selectedMembers = [...createCollectiveForm.selectedMembers, memberId];
		}
	}

	function isMemberInCollective(memberId: string): boolean {
		return createCollectiveForm.selectedMembers.includes(memberId);
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

		// Initialize alias editing state
		editingContactAlias = '';
		showAliasDropdown = false;
		selectedAliasIndex = 0;

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

		// Reset alias editing state
		editingContactAlias = '';
		showAliasDropdown = false;
		selectedAliasIndex = 0;
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
				// Editing an existing contact - update name in store and notify parent
				updateContactInStore(entryId, {
					name: entryName.trim()
				});

				// Also notify parent component through prop
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

	// Alias selection handlers
	function handleAliasInput(event: Event) {
		const target = event.target as HTMLInputElement;
		editingContactAlias = target.value;
		showAliasDropdown = target.value.trim().length > 0;
		selectedAliasIndex = 0;
	}

	function handleSelectAlias(alias: string, pubkey: string) {
		editingContactAlias = alias;
		showAliasDropdown = false;
		selectedAliasIndex = 0;

		// Update the contact with the selected alias's pubkey
		if (editingEntryId) {
			updateContactInStore(editingEntryId, {
				public_key: pubkey,
				name: editingEntryName
			});

			// Also notify parent component through prop
			updateContact({
				contactId: editingEntryId,
				name: editingEntryName
			});
		}
	}

	function handleAliasKeydown(event: KeyboardEvent) {
		const aliases = filteredAliases();

		if (event.key === 'ArrowDown') {
			event.preventDefault();
			selectedAliasIndex = Math.min(selectedAliasIndex + 1, aliases.length - 1);
		} else if (event.key === 'ArrowUp') {
			event.preventDefault();
			selectedAliasIndex = Math.max(selectedAliasIndex - 1, 0);
		} else if (event.key === 'Enter' && aliases.length > 0) {
			event.preventDefault();
			const selectedAlias = aliases[selectedAliasIndex];
			if (selectedAlias) {
				handleSelectAlias(selectedAlias.alias, selectedAlias.pubkey);
			}
		} else if (event.key === 'Escape') {
			event.preventDefault();
			showAliasDropdown = false;
		}
	}

	// Contact deletion handler
	function handleDeleteContact(contactId: string, contactName: string, event?: Event) {
		// Prevent the click from bubbling up and closing the dropdown
		if (event) {
			event.stopPropagation();
			event.preventDefault();
		}

		// Show confirmation dialog
		const confirmed = confirm(
			`Delete contact "${contactName}"?\n\nThis will remove the contact and replace any references in the tree with the public key (if available) or remove them entirely.`
		);

		if (!confirmed) {
			return;
		}

		try {
			// Find the contact metadata to get the public key
			const items = filteredItems();
			const contactItem = items.find(
				(item: { id: string; metadata?: any }) => item.id === contactId && item.metadata?.isContact
			);
			const publicKey = contactItem?.metadata?.userId; // The userId contains the public key for contacts

			// Call the deleteContact prop with contact details
			deleteContact({
				contactId,
				name: contactName,
				publicKey
			});

			console.log('[CONTACT] Deleted contact:', { contactId, name: contactName, publicKey });

			// Close the dropdown to show the updated list
			handleClose();
		} catch (error) {
			console.error('[CONTACT] Error deleting contact:', error);
			// Keep dropdown open so user can see the error
		}
	}

	// Initialize the component when shown
	function initialize() {
		if (!initialized && show) {
			// Clear search filter to start fresh
			searchFilter = '';

			// Focus search input
			if (searchInput) {
				setTimeout(() => searchInput?.focus(), 50);
			}

			adjustPosition();
			initialized = true;
		}
	}

	// Adjust position to stay within app content boundaries
	function adjustPosition() {
		if (!dropdownContainer || !browser) return;

		const rect = dropdownContainer.getBoundingClientRect();

		// Get app content container bounds
		const appContent = document.querySelector('.app-content');
		const headerHeight = 76; // Approximate header height
		const contentPadding = 16; // App content padding

		const contentRect = appContent
			? appContent.getBoundingClientRect()
			: {
					left: contentPadding,
					top: headerHeight,
					right: window.innerWidth - contentPadding,
					bottom: window.innerHeight - contentPadding,
					width: window.innerWidth - contentPadding * 2,
					height: window.innerHeight - headerHeight - contentPadding
				};

		const padding = 10; // Padding from container edges
		const originalX = position.x;
		const originalY = position.y;

		// Check right edge - keep within app content
		if (position.x + rect.width > contentRect.right - padding) {
			position.x = Math.max(contentRect.left + padding, contentRect.right - rect.width - padding);
		}

		// Check left edge - keep within app content
		if (position.x < contentRect.left + padding) {
			position.x = contentRect.left + padding;
		}

		// Check bottom edge - keep within app content
		if (position.y + rect.height > contentRect.bottom - padding) {
			// If dropdown would go above the app content top, position it at the top with padding
			if (position.y - rect.height < contentRect.top + padding) {
				position.y = contentRect.top + padding;
			} else {
				// Otherwise, position above the click
				position.y = position.y - rect.height - 10;
			}
		}

		// Check top edge - ensure it's not above app content
		if (position.y < contentRect.top + padding) {
			position.y = contentRect.top + padding;
		}

		// Debug logging for position adjustments
		if (originalX !== position.x || originalY !== position.y) {
			console.log('[DROPDOWN] Position adjusted:', {
				original: { x: originalX, y: originalY },
				adjusted: { x: position.x, y: position.y },
				contentBounds: contentRect,
				dropdownSize: { width: rect.width, height: rect.height }
			});
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

	// Effect to manage click outside and keyboard listeners
	$effect(() => {
		if (!browser) return;

		if (show) {
			const handleClickOutside = (event: MouseEvent | TouchEvent) => {
				if (dropdownContainer && !dropdownContainer.contains(event.target as Node)) {
					handleClose();
				}
			};

			const handleKeydown = (event: KeyboardEvent) => {
				if (event.key === 'Escape') {
					handleClose();
				}
			};

			// Add listeners after a small delay to prevent immediate closure
			const timeoutId = setTimeout(() => {
				document.addEventListener('click', handleClickOutside);
				document.addEventListener('touchstart', handleClickOutside);
				document.addEventListener('keydown', handleKeydown);
			}, 100);

			return () => {
				clearTimeout(timeoutId);
				document.removeEventListener('click', handleClickOutside);
				document.removeEventListener('touchstart', handleClickOutside);
				document.removeEventListener('keydown', handleKeydown);
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
					disabled={showCreateForm || showCreateCollectiveForm}
				>
					👤+
				</button>
			{/if}
			{#if allowCreateCollective}
				<button
					class="create-button collective-button"
					onclick={handleShowCreateCollectiveForm}
					title="Create new collective"
					disabled={showCreateForm || showCreateCollectiveForm}
				>
					👥+
				</button>
			{/if}
			{#if collectiveMode}
				<div class="mode-indicator" title="Collective mode - select multiple people">
					👥 Multi-select
				</div>
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

		{#if showCreateCollectiveForm}
			<div class="create-form collective-form">
				<div class="form-field">
					<input
						type="text"
						placeholder="Collective name"
						bind:this={nameInput}
						bind:value={createCollectiveForm.name}
						onkeydown={handleCreateCollectiveFormKeydown}
					/>
				</div>
				<div class="collective-members">
					<div class="members-header">
						<span>Members ({createCollectiveForm.selectedMembers.length})</span>
						<span class="members-hint">Select at least 2 people</span>
					</div>
					<div class="members-list">
						{#each $dataProvider || [] as item (item.id)}
							{#if !item.metadata?.isContact || item.metadata?.userId}
								<div
									class="member-item"
									class:selected={isMemberInCollective(item.id)}
									onclick={() => handleToggleMemberInCollective(item.id)}
								>
									<div
										class="color-dot"
										style="background-color: {getColorForUserId(item.id)}"
									></div>
									<span class="member-name">{item.name}</span>
									{#if isMemberInCollective(item.id)}
										<span class="member-selected">✓</span>
									{/if}
								</div>
							{/if}
						{/each}
					</div>
				</div>
				<div class="form-actions">
					<button
						class="cancel-button"
						onclick={handleHideCreateCollectiveForm}
						disabled={isCreatingCollective}
					>
						Cancel
					</button>
					<button
						class="save-button"
						onclick={handleCreateCollective}
						disabled={isCreatingCollective ||
							!createCollectiveForm.name.trim() ||
							createCollectiveForm.selectedMembers.length < 2}
					>
						{isCreatingCollective
							? 'Creating...'
							: `Create Collective (${createCollectiveForm.selectedMembers.length})`}
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
									<div class="input-row">
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

										{#if editingMode === 'edit'}
											<div class="alias-input-container">
												<input
													type="text"
													placeholder="Link to alias (optional)"
													bind:this={editingContactAliasInput}
													bind:value={editingContactAlias}
													oninput={handleAliasInput}
													onkeydown={handleAliasKeydown}
													onclick={(e) => e.stopPropagation()}
													disabled={isCreatingFromEntry}
												/>

												{#if showAliasDropdown && filteredAliases().length > 0}
													<div class="alias-dropdown">
														{#each filteredAliases() as aliasItem, index}
															<div
																class="alias-item"
																class:selected={index === selectedAliasIndex}
																role="option"
																tabindex="0"
																aria-selected={index === selectedAliasIndex}
																onclick={() => handleSelectAlias(aliasItem.alias, aliasItem.pubkey)}
																onkeydown={(e) => {
																	if (e.key === 'Enter' || e.key === ' ') {
																		e.preventDefault();
																		handleSelectAlias(aliasItem.alias, aliasItem.pubkey);
																	}
																}}
															>
																<span class="alias-name"
																	>@{aliasItem.alias.length > 10
																		? aliasItem.alias.substring(0, 10) + '...'
																		: aliasItem.alias}</span
																>
															</div>
														{/each}
													</div>
												{/if}
											</div>
										{/if}
									</div>

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
										{#if editingMode === 'edit'}
											<button
												class="delete-entry-button"
												onclick={(e) => handleDeleteContact(item.id, item.name, e)}
												title="Delete contact"
											>
												🗑️
											</button>
										{/if}
									</div>
								</div>
								<div class="item-meta">
									{#if editingMode === 'create'}
										{item.metadata?.gunAlias
											? `@${item.metadata.gunAlias}`
											: `Public key: ${item.id.substring(0, 12)}...`}
									{:else}
										Contact: {item.metadata?.contactName || 'Editing contact name'}
									{/if}
								</div>
							</div>
						{:else}
							<!-- Normal display mode -->
							<div
								class="item-content"
								role="button"
								tabindex="0"
								onclick={() => handleItemClick(item.id, item.name, item.metadata)}
								onkeydown={(e) => {
									if (e.key === 'Enter' || e.key === ' ') {
										e.preventDefault();
										handleItemClick(item.id, item.name, item.metadata);
									}
								}}
							>
								<div class="item-name">
									{#if isItemSelected(item.id)}
										<span class="selected-icon">✓</span>
									{/if}

									<!-- Enhanced icon display with better logic -->
									{#if item.metadata?.isContact}
										<span class="contact-icon" title="Contact">👤</span>
									{/if}

									<span class="item-display-name">{item.name || item.id}</span>

									{#if isItemSelected(item.id)}
										<span class="remove-hint">click to remove</span>
									{/if}
								</div>

								<!-- Enhanced metadata display -->
								<div class="item-meta">
									{#if item.metadata?.contributorCount !== undefined}
										({item.metadata.contributorCount} contributors)
									{/if}

									<!-- Show Gun alias when available -->
									{#if item.metadata?.isContact && item.metadata?.gunAlias}
										<span class="gun-alias">@{item.metadata.gunAlias}</span>
									{/if}
								</div>
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
		/* Ensure dropdown never exceeds viewport bounds */
		max-width: calc(100vw - 32px);
		max-height: calc(100vh - 120px);
	}

	/* Mobile-specific dropdown adjustments */
	@media (max-width: 768px) {
		.dropdown-container {
			max-width: calc(100vw - 16px);
			max-height: calc(100vh - 100px);
			/* On mobile, use slightly smaller width if needed */
			min-width: 260px;
		}
	}

	@media (max-width: 480px) {
		.dropdown-container {
			/* On very small screens, take most of the width */
			width: calc(100vw - 20px);
			max-width: calc(100vw - 20px);
			left: 10px !important; /* Override position for very small screens */
		}
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
		max-height: calc(min(var(--max-height, 320px), 50vh) - 56px);
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
		position: relative;
		border-left: 3px solid transparent;
	}

	.item:hover {
		background-color: #f5f7fa;
	}

	/* SELECTED STATE - highest priority, overrides everything */
	.item.is-selected {
		background-color: #e3f2fd;
		border-left: 4px solid #2196f3;
		font-weight: 500;
	}

	.item.is-selected:hover {
		background-color: #bbdefb;
	}

	/* CONTACT STATE - only when not selected */
	.item:not(.is-selected).is-contact {
		background-color: #f8fffe;
		border-left: 3px solid #4caf50;
	}

	.item:not(.is-selected).is-contact:hover {
		background-color: #f0f8f0;
	}

	/* CONTRIBUTOR STATE - only when not selected */
	.item:not(.is-selected).is-child-contributor {
		background-color: #f0f4ff;
		border-left: 3px solid #2196f3;
		font-weight: 500;
	}

	.item:not(.is-selected).is-child-contributor:hover {
		background-color: #e3f2fd;
	}

	/* CONTACT + CONTRIBUTOR - simplified approach */
	.item:not(.is-selected).is-contact.is-child-contributor {
		background-color: #f4f8ff;
		border-left: 3px solid #2196f3;
		border-right: 2px solid #4caf50;
		font-weight: 500;
	}

	.item:not(.is-selected).is-contact.is-child-contributor:hover {
		background-color: #e6f3ff;
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
		flex-wrap: wrap;
	}

	.item-display-name {
		font-weight: inherit;
		color: inherit;
	}

	.selected-icon {
		color: #2196f3;
		font-weight: bold;
		font-size: 12px;
		margin-right: 2px;
	}

	.contact-icon {
		font-size: 12px;
		opacity: 0.8;
		color: #4caf50;
		margin-right: 2px;
	}

	.item-meta {
		font-size: 12px;
		color: #666;
		margin-top: 2px;
		line-height: 1.3;
	}

	.gun-alias {
		color: #888;
		font-style: italic;
		font-size: 11px;
		opacity: 0.9;
	}

	.remove-hint {
		font-size: 10px;
		color: #999;
		font-style: italic;
		margin-left: auto;
		opacity: 0;
		transition: opacity 0.2s;
		white-space: nowrap;
	}

	.item.is-selected:hover .remove-hint {
		opacity: 1;
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
		flex-direction: column;
		gap: 8px;
		margin-bottom: 4px;
	}

	.input-row {
		display: flex;
		gap: 8px;
		align-items: flex-start;
	}

	.input-row input {
		flex: 1;
		padding: 6px 10px;
		border: 1px solid #ddd;
		border-radius: 4px;
		font-size: 12px;
		outline: none;
		transition: border-color 0.2s;
		min-width: 0; /* Allows flex item to shrink below content size */
	}

	.input-row input:focus {
		border-color: #2196f3;
		box-shadow: 0 0 0 2px rgba(33, 150, 243, 0.1);
	}

	.alias-input-container {
		position: relative;
		flex: 1;
		min-width: 0; /* Allows flex item to shrink below content size */
	}

	.alias-input-container input {
		width: 100%;
		padding: 6px 10px;
		border: 1px solid #ddd;
		border-radius: 4px;
		font-size: 12px;
		outline: none;
		transition: border-color 0.2s;
	}

	.alias-input-container input:focus {
		border-color: #4caf50;
		box-shadow: 0 0 0 2px rgba(76, 175, 80, 0.1);
	}

	.alias-dropdown {
		position: absolute;
		top: 100%;
		left: 0;
		right: 0;
		background: white;
		border: 1px solid #ddd;
		border-radius: 4px;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
		max-height: 160px;
		overflow-y: auto;
		z-index: 1000;
		margin-top: 2px;
		font-size: 12px;
	}

	.alias-item {
		padding: 6px 8px;
		cursor: pointer;
		border-bottom: 1px solid #f0f0f0;
		transition: background-color 0.2s;
		white-space: nowrap;
		overflow: hidden;
		text-overflow: ellipsis;
	}

	.alias-item:hover,
	.alias-item.selected {
		background-color: #f5f7fa;
	}

	.alias-item:last-child {
		border-bottom: none;
	}

	.alias-name {
		font-weight: 500;
		color: #4caf50;
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

	.delete-entry-button {
		padding: 4px 8px;
		border-radius: 4px;
		font-size: 12px;
		cursor: pointer;
		transition: all 0.2s;
		border: 1px solid #f44336;
		background: transparent;
		color: #f44336;
		margin-left: 8px;
		min-width: 24px;
		height: 24px;
		display: flex;
		align-items: center;
		justify-content: center;
	}

	.delete-entry-button:hover:not(:disabled) {
		background-color: #ffebee;
		color: #f44336;
	}

	.delete-entry-button:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}

	/* COLLECTIVE CREATION STYLES */

	.collective-button {
		background: linear-gradient(135deg, #6a5acd, #483d8b);
		color: white;
		border-color: #6a5acd;
	}

	.collective-button:hover:not(:disabled) {
		background: linear-gradient(135deg, #5a4fcf, #3a2d7b);
		border-color: #5a4fcf;
	}

	.mode-indicator {
		font-size: 11px;
		color: #6a5acd;
		font-weight: 500;
		padding: 4px 8px;
		background: rgba(106, 90, 205, 0.1);
		border-radius: 12px;
		white-space: nowrap;
		margin-left: 8px;
		display: flex;
		align-items: center;
		gap: 4px;
	}

	.collective-form {
		background: linear-gradient(135deg, #f8f7ff, #f0f0ff);
		border: 1px solid #e0d9ff;
	}

	.collective-members {
		margin-top: 12px;
		padding-top: 12px;
		border-top: 1px solid #e0d9ff;
	}

	.members-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 8px;
		font-size: 13px;
	}

	.members-header span:first-child {
		font-weight: 500;
		color: #6a5acd;
	}

	.members-hint {
		color: #888;
		font-size: 11px;
	}

	.members-list {
		max-height: 150px;
		overflow-y: auto;
		border: 1px solid #e0d9ff;
		border-radius: 4px;
		background: white;
	}

	.member-item {
		display: flex;
		align-items: center;
		padding: 6px 8px;
		cursor: pointer;
		transition: all 0.2s;
		border-bottom: 1px solid #f5f5f5;
		gap: 8px;
	}

	.member-item:hover {
		background-color: #f8f7ff;
	}

	.member-item.selected {
		background-color: #e8e4ff;
		border-left: 3px solid #6a5acd;
		font-weight: 500;
	}

	.member-item:last-child {
		border-bottom: none;
	}

	.member-name {
		flex: 1;
		font-size: 12px;
		color: #333;
	}

	.member-selected {
		color: #6a5acd;
		font-weight: bold;
		font-size: 12px;
	}
</style>
