<script lang="ts">
	import type { ProviderCapacity, AvailabilitySlot } from '$lib/schema';
	import Bar from './Bar.svelte';
	import TagPill from './TagPill.svelte';
	import DropDown from './DropDown.svelte';
	import Chat from './Chat.svelte';

	import Slot from './Slot.svelte';
	import { Rules } from '$lib/filters';
	import { onMount } from 'svelte';
	import { browser } from '$app/environment';
	import { createSubtreesDataProvider } from '$lib/utils/ui-providers.svelte';
	import { globalState } from '$lib/global.svelte';
	import { ProviderCapacitySchema } from '$lib/schema';
	import { getReactiveUnreadCount } from '$lib/state/chat.svelte';

	interface Props {
		capacity: ProviderCapacity;
		canDelete: boolean;
		onupdate?: (capacity: ProviderCapacity) => void;
		ondelete?: (id: string) => void;
	}

	let { capacity, canDelete, onupdate, ondelete }: Props = $props();

	// UI state for expanded capacity editing
	let expanded = $state(false);

	// UI state for expanded chat
	let chatExpanded = $state(false);

	// Get reactive unread message count for this capacity's chat
	let unreadCount = getReactiveUnreadCount(capacity.id);

	// UI state for expanded slots
	let slotsExpanded = $state(false);

	// Simple filter state - just track selected subtree IDs
	let selectedSubtrees = $state<string[]>([]);

	// Dropdown state for adding subtree filters
	let showSubtreeDropdown = $state(false);
	let dropdownPosition = $state({ x: 0, y: 0 });

	// Emoji picker state
	let showEmojiPicker = $state(false);
	let emojiPickerContainer: HTMLDivElement | undefined = $state();
	let emojiPickerElement: any = $state();

	// Create subtrees data provider for the dropdown
	let subtreesDataProvider = createSubtreesDataProvider();

	// Reactive capacity properties for proper binding (matching schema types)
	let capacityName = $state(capacity.name);
	let capacityEmoji = $state(capacity.emoji);
	let capacityUnit = $state(capacity.unit);
	let capacityDescription = $state(capacity.description);
	let capacityMaxNaturalDiv = $state(capacity.max_natural_div);
	let capacityMaxPercentageDiv = $state(capacity.max_percentage_div);

	// Derived filter rule - automatically updates when selectedSubtrees changes
	let filterRule = $derived(() => {
		if (selectedSubtrees.length === 0) {
			return null;
		} else if (selectedSubtrees.length === 1) {
			return Rules.inSubtree(selectedSubtrees[0]);
		} else {
			return Rules.inSubtrees(selectedSubtrees);
		}
	});

	// Get subtree names for display
	let subtreeNames = $derived(() => {
		const names: Record<string, string> = {};
		selectedSubtrees.forEach((subtreeId) => {
			const item = $subtreesDataProvider.find((item) => item.id === subtreeId);
			if (item) {
				names[subtreeId] = item.name;
			}
		});
		return names;
	});

	// Load emoji picker client-side only
	onMount(async () => {
		if (browser) {
			try {
				// Dynamic import to avoid SSR issues
				await import('emoji-picker-element');
			} catch (error) {
				console.warn('Failed to load emoji picker:', error);
			}
		}
	});

	// Initialize emoji picker when container is available
	$effect(() => {
		if (emojiPickerContainer && browser && !emojiPickerElement) {
			try {
				const picker = document.createElement('emoji-picker');
				picker.style.position = 'absolute';
				picker.style.zIndex = '1000';
				picker.style.boxShadow = '0 10px 25px rgba(0, 0, 0, 0.2)';
				picker.style.border = '1px solid #e5e7eb';
				picker.style.borderRadius = '8px';
				picker.style.width = '320px';
				picker.style.height = '400px';

				// Listen for emoji selection
				picker.addEventListener('emoji-click', (event: any) => {
					capacityEmoji = event.detail.unicode;
					handleCapacityUpdate();
					showEmojiPicker = false;
				});

				emojiPickerElement = picker;
			} catch (error) {
				console.warn('Failed to create emoji picker:', error);
			}
		}
	});

	// Handle emoji picker visibility
	$effect(() => {
		if (emojiPickerElement && emojiPickerContainer) {
			if (showEmojiPicker) {
				emojiPickerContainer.appendChild(emojiPickerElement);
			} else if (emojiPickerElement.parentNode) {
				emojiPickerElement.parentNode.removeChild(emojiPickerElement);
			}
		}
	});

	// Convert recipient_shares to bar segments
	const recipientSegments = $derived(
		Object.entries(capacity.recipient_shares || {}).map(([userId, share]) => ({
			id: userId,
			value: share * 100 // Convert from 0-1 to 0-100 percentage
		}))
	);

	// Recurrence options
	const recurrenceOptions = [
		'Does not repeat',
		'Daily',
		'Weekly',
		'Monthly',
		'Annually',
		'Every weekday (Monday to Friday)',
		'Every 4 days',
		'Custom...'
	];

	// Helper to track original value on focus
	function handleFocus(fieldName: string, currentValue: any) {
		originalValues[fieldName] = currentValue;
	}

	// Helper to save only if value changed on blur
	function handleBlurIfChanged(fieldName: string, currentValue: any) {
		if (originalValues[fieldName] !== currentValue) {
			handleCapacityUpdate();
		}
	}

	// Handler for input events that updates capacity
	function handleCapacityUpdate() {
		// Create updated capacity with current filter rule
		const updatedCapacity = {
			...capacity,
			name: capacityName,
			emoji: capacityEmoji,
			unit: capacityUnit,
			description: capacityDescription,
			max_natural_div: capacityMaxNaturalDiv,
			max_percentage_div: capacityMaxPercentageDiv,
			filter_rule: filterRule()
		};

		// Validate using schema
		const validationResult = ProviderCapacitySchema.safeParse(updatedCapacity);

		if (!validationResult.success) {
			// Check for specific validation errors and show appropriate messages
			const errors = validationResult.error.issues;
			const unitErrors = errors.filter((issue) => issue.path.includes('unit'));

			if (unitErrors.length > 0) {
				globalState.showToast(unitErrors[0].message, 'warning');
				// Revert unit to previous valid value
				capacityUnit = capacity.unit || '';
				return;
			}

			// Handle other validation errors
			globalState.showToast('Invalid capacity data', 'error');
			console.error('Capacity validation failed:', validationResult.error);
			return;
		}

		// Validation passed, proceed with update
		onupdate?.(validationResult.data);
	}

	// Delete this capacity
	function handleDelete() {
		ondelete?.(capacity.id);
	}

	// Toggle expanded state
	function toggleExpanded() {
		expanded = !expanded;
		// If we're expanding settings, close chat and slots
		if (expanded) {
			chatExpanded = false;
			slotsExpanded = false;
		}
	}

	// Toggle chat state
	function toggleChat() {
		chatExpanded = !chatExpanded;
		// If we're expanding chat, close settings and slots
		if (chatExpanded) {
			expanded = false;
			slotsExpanded = false;
		}
	}

	// Toggle slots state
	function toggleSlots() {
		slotsExpanded = !slotsExpanded;
		// If we're expanding slots, close settings and chat
		if (slotsExpanded) {
			expanded = false;
			chatExpanded = false;
		}
	}

	// Handle adding a subtree filter
	function handleAddSubtreeFilter(event: MouseEvent) {
		event.preventDefault();
		event.stopPropagation();

		const rect = (event.target as HTMLElement).getBoundingClientRect();
		dropdownPosition = {
			x: rect.left,
			y: rect.bottom + 5
		};

		showSubtreeDropdown = true;
	}

	// Handle subtree selection from dropdown
	function handleSubtreeSelect(detail: { id: string; name: string; metadata?: any }) {
		const { id: subtreeId } = detail;

		// Don't add if already selected
		if (selectedSubtrees.includes(subtreeId)) {
			showSubtreeDropdown = false;
			return;
		}

		// Add to selected subtrees
		selectedSubtrees = [...selectedSubtrees, subtreeId];

		// Update capacity
		handleCapacityUpdate();

		// Close dropdown
		showSubtreeDropdown = false;
	}

	// Handle removing a subtree filter
	function handleRemoveSubtree(subtreeId: string) {
		selectedSubtrees = selectedSubtrees.filter((id) => id !== subtreeId);
		handleCapacityUpdate();
	}

	// Close dropdown
	function handleDropdownClose() {
		showSubtreeDropdown = false;
	}

	// Handle emoji picker
	function handleEmojiPickerToggle(event: MouseEvent) {
		event.preventDefault();
		event.stopPropagation();
		showEmojiPicker = !showEmojiPicker;
	}

	// Close emoji picker when clicking outside
	function handleClickOutside(event: MouseEvent) {
		if (emojiPickerContainer && !emojiPickerContainer.contains(event.target as Node)) {
			showEmojiPicker = false;
		}
	}

	// Add click outside listener
	$effect(() => {
		if (showEmojiPicker) {
			document.addEventListener('click', handleClickOutside);
			return () => {
				document.removeEventListener('click', handleClickOutside);
			};
		}
	});

	// Format date for input
	function formatDateForInput(date: Date | undefined): string {
		if (!date) return '';
		return date.toISOString().split('T')[0];
	}

	// State for description field expansion
	let descriptionExpanded = $state(false);

	// Track original values for change detection
	let originalValues = $state<Record<string, any>>({});

	// Handle slot updates
	function handleSlotUpdate(updatedSlot: AvailabilitySlot) {
		const updatedSlots = capacity.availability_slots.map((slot) =>
			slot.id === updatedSlot.id ? updatedSlot : slot
		);

		const updatedCapacity = {
			...capacity,
			availability_slots: updatedSlots
		};

		onupdate?.(updatedCapacity);
	}

	// Handle slot deletion
	function handleSlotDelete(slotId: string) {
		const updatedSlots = capacity.availability_slots.filter((slot) => slot.id !== slotId);

		const updatedCapacity = {
			...capacity,
			availability_slots: updatedSlots
		};

		onupdate?.(updatedCapacity);
	}

	// Add new slot
	function handleAddSlot() {
		const newSlot = {
			id: `slot-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
			quantity: 1,
			location_type: 'Undefined'
		};

		const updatedSlots = [...capacity.availability_slots, newSlot];

		const updatedCapacity = {
			...capacity,
			availability_slots: updatedSlots
		};

		onupdate?.(updatedCapacity);
	}
</script>

<div class="capacity-item" class:chat-expanded={chatExpanded}>
	<!-- Recipient shares bar -->
	{#if recipientSegments.length > 0}
		<div class="recipient-shares-bar mb-1">
			<Bar
				segments={recipientSegments}
				height="8px"
				rounded={true}
				backgroundColor="#f3f4f6"
				showLabelsAboveOnSelect={true}
			/>
		</div>
	{/if}

	<div class="capacity-row flex flex-wrap items-center gap-2 rounded bg-white p-2 shadow-sm">
		<!-- Emoji picker button -->
		<div class="relative">
			<button
				type="button"
				class="emoji-btn"
				onclick={handleEmojiPickerToggle}
				title="Select emoji"
			>
				{capacityEmoji || '📦'}
			</button>
			<!-- Emoji picker container -->
			{#if showEmojiPicker}
				<div bind:this={emojiPickerContainer} class="emoji-picker-container"></div>
			{/if}
		</div>

		<!-- Main capacity inputs -->
		<input
			type="text"
			class="capacity-input name auto-size"
			bind:value={capacityName}
			placeholder="Name"
			onfocus={() => handleFocus('name', capacityName)}
			onblur={() => handleBlurIfChanged('name', capacityName)}
			style="width: {Math.max(capacityName?.length || 0, 'Name'.length) +
				3}ch; min-width: {Math.max(6, 'Name'.length + 2)}ch;"
		/>
		<input
			type="text"
			class="capacity-input unit auto-size"
			bind:value={capacityUnit}
			placeholder="Unit"
			onfocus={() => handleFocus('unit', capacityUnit)}
			onblur={() => handleBlurIfChanged('unit', capacityUnit)}
			style="width: {Math.max(capacityUnit?.length || 0, 'Unit'.length) +
				3}ch; min-width: {Math.max(6, 'Unit'.length + 2)}ch;"
		/>
		<!-- Description field with integrated toggle -->
		<div class="description-field-container">
			{#if descriptionExpanded}
				<textarea
					class="capacity-input description-textarea auto-size"
					bind:value={capacityDescription}
					placeholder="Description"
					onfocus={() => handleFocus('description', capacityDescription)}
					onblur={() => handleBlurIfChanged('description', capacityDescription)}
					rows="3"
				></textarea>
			{:else}
				<input
					type="text"
					class="capacity-input description auto-size"
					bind:value={capacityDescription}
					placeholder="Description"
					onfocus={() => handleFocus('description', capacityDescription)}
					onblur={() => handleBlurIfChanged('description', capacityDescription)}
					style="width: {Math.max(capacityDescription?.length || 0, 'Description'.length) +
						3}ch; min-width: {Math.max(12, 'Description'.length + 2)}ch;"
				/>
			{/if}
			{#if capacityDescription}
				<button
					type="button"
					class="description-expand-btn"
					onclick={() => (descriptionExpanded = !descriptionExpanded)}
					title={descriptionExpanded ? 'Collapse' : 'Expand'}
				>
					{descriptionExpanded ? '▼' : '▲'}
				</button>
			{/if}
		</div>

		<!-- Action buttons -->
		<button
			type="button"
			class="chat-btn relative ml-1"
			onclick={toggleChat}
			title="Chat about this capacity"
		>
			💬
			{#if $unreadCount > 0}
				<span class="unread-badge">{$unreadCount > 99 ? '99+' : $unreadCount}</span>
			{/if}
		</button>

		<button
			type="button"
			class="slots-btn ml-1"
			onclick={toggleSlots}
			title="Manage availability slots"
		>
			🕒
		</button>
		<button type="button" class="settings-btn ml-1" onclick={toggleExpanded}> ⚙️ </button>
		<button type="button" class="remove-btn ml-1" onclick={handleDelete} disabled={!canDelete}
			>✖️</button
		>
	</div>

	<!-- Filter tags section (always visible, like contributors in Child.svelte) -->
	<div class="filter-section mt-2">
		<div class="filter-layout">
			<!-- Add filter button -->
			<div class="filter-button-container">
				<button type="button" class="add-filter-btn" onclick={handleAddSubtreeFilter}>
					<span class="add-icon">+</span>
					<span class="add-text">Filter</span>
				</button>
			</div>

			<!-- Filter tags -->
			{#if selectedSubtrees.length > 0}
				<div class="filter-tags-container">
					{#each selectedSubtrees as subtreeId}
						<div class="filter-tag-wrapper">
							<TagPill
								userId={subtreeId}
								displayName={subtreeNames()[subtreeId] || subtreeId}
								truncateLength={15}
								removable={true}
								onClick={() => {}}
								onRemove={() => handleRemoveSubtree(subtreeId)}
							/>
						</div>
					{/each}
				</div>
			{/if}
		</div>

		<!-- Filter description -->
		{#if selectedSubtrees.length > 0}
			<div class="filter-description">
				<span class="filter-desc-text">
					Only contributors from {selectedSubtrees.length === 1
						? 'this subtree'
						: `${selectedSubtrees.length} subtrees`}
				</span>
			</div>
		{/if}
	</div>

	<!-- Expanded chat section -->
	{#if chatExpanded}
		<div class="chat-container rounded border border-gray-200 bg-gray-50 p-3">
			<div class="chat-header mb-2">
				<h4 class="text-sm font-medium text-gray-700">
					💬 Chat about {capacity.emoji || '📦'}
					{capacity.name}
				</h4>
				<p class="mt-1 text-xs text-gray-500">
					Discuss this capacity with recipients and other stakeholders
				</p>
			</div>
			<Chat chatId={capacity.id} placeholder={`Discuss ${capacity.name}...`} maxLength={200} />
		</div>
	{/if}

	<!-- Expanded slots section -->
	{#if slotsExpanded}
		<div class="slots-section mb-4 rounded border border-gray-200 bg-purple-50 p-3">
			<div class="slots-header mb-3">
				<h4 class="text-sm font-medium text-gray-700">🕒 Availability Slots</h4>
				<p class="mt-1 text-xs text-gray-500">
					Manage specific time and location slots for this capacity
				</p>
			</div>

			<div class="slots-content">
				<!-- Add slot button -->
				<button type="button" class="add-slot-btn mb-4" onclick={handleAddSlot}>
					<span class="add-icon">+</span>
					<span class="add-text">Add new slot</span>
				</button>

				<!-- Existing slots -->
				{#if capacity.availability_slots && capacity.availability_slots.length > 0}
					<div class="slots-list space-y-3">
						{#each capacity.availability_slots as slot (slot.id)}
							<Slot
								{slot}
								capacityId={capacity.id}
								unit={capacity.unit}
								canDelete={capacity.availability_slots.length > 1}
								onupdate={handleSlotUpdate}
								ondelete={handleSlotDelete}
							/>
						{/each}
					</div>
				{:else}
					<div class="empty-slots py-6 text-center text-xs text-gray-500 italic">
						No slots defined. Click "Add new slot" to get started.
					</div>
				{/if}
			</div>
		</div>
	{/if}

	<!-- Expanded settings (other options only) -->
	{#if expanded}
		<div class="expanded-settings mt-2 rounded-md bg-white shadow-sm">
			<div class="settings-content">
				<div class="other-options mb-4">
					<div class="max-divisibility-section mb-6">
						<h4 class="mb-4 text-sm font-medium text-gray-700">Max-divisibility</h4>
						<div class="grid grid-cols-2 gap-8">
							<div>
								<input
									type="number"
									min="1"
									step="1"
									class="capacity-input qty w-full text-right"
									bind:value={capacityMaxNaturalDiv}
									placeholder="Natural"
									onfocus={() => handleFocus('maxNaturalDiv', capacityMaxNaturalDiv)}
									onblur={() => handleBlurIfChanged('maxNaturalDiv', capacityMaxNaturalDiv)}
								/>
							</div>
							<div>
								<input
									type="number"
									min="0"
									max="1"
									step="0.01"
									class="capacity-input qty w-full text-right"
									bind:value={capacityMaxPercentageDiv}
									placeholder="Percentage (0-1)"
									onfocus={() => handleFocus('maxPercentageDiv', capacityMaxPercentageDiv)}
									onblur={() => handleBlurIfChanged('maxPercentageDiv', capacityMaxPercentageDiv)}
								/>
							</div>
						</div>
					</div>
				</div>
			</div>
		</div>
	{/if}
</div>

<!-- Subtree dropdown for adding filters -->
{#if showSubtreeDropdown}
	<DropDown
		position={dropdownPosition}
		show={showSubtreeDropdown}
		title="Select Subtree Category"
		searchPlaceholder="Search subtrees..."
		dataProvider={subtreesDataProvider}
		select={handleSubtreeSelect}
		close={handleDropdownClose}
	/>
{/if}

<style>
	/* Make sure inputs and selects match our design */
	.capacity-input {
		font-size: 1rem;
		padding: 6px 8px;
		border: none;
		border-bottom: 1.5px solid #e5e7eb;
		background: transparent;
		outline: none;
		transition: all 0.2s ease;
		min-width: 0;
		flex-shrink: 0;
	}

	.capacity-input.auto-size {
		max-width: 200px; /* Prevent inputs from becoming too wide */
	}

	.capacity-input::placeholder {
		color: #cbd5e1;
	}
	.capacity-input:focus {
		border-bottom: 1.5px solid #3b82f6;
		box-shadow: 0 1px 0 0 rgba(59, 130, 246, 0.2);
	}

	.capacity-select {
		font-size: 1rem;
		padding: 6px 8px;
		border: none;
		border-bottom: 1.5px solid #e5e7eb;
		background: transparent;
		outline: none;
		transition: all 0.2s ease;
		appearance: none;
		background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='16' height='16' viewBox='0 0 24 24' fill='none' stroke='%23a1a1aa' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpolyline points='6 9 12 15 18 9'%3E%3C/polyline%3E%3C/svg%3E");
		background-repeat: no-repeat;
		background-position: right 6px center;
		background-size: 16px;
		padding-right: 28px;
		border-radius: 0;
	}
	.capacity-select:focus {
		border-bottom: 1.5px solid #3b82f6;
		box-shadow: 0 1px 0 0 rgba(59, 130, 246, 0.2);
	}

	/* Icon buttons */
	.remove-btn,
	.settings-btn,
	.chat-btn,
	.slots-btn {
		background: none;
		border: none;
		color: #cbd5e1;
		font-size: 1em;
		cursor: pointer;
		padding: 0 4px;
		line-height: 1;
		border-radius: 50%;
		transition: all 0.2s ease;
		display: flex;
		align-items: center;
		justify-content: center;
		width: 24px;
		height: 24px;
	}

	.emoji-btn {
		background: none;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		padding: 4px 6px;
		font-size: 1.2em;
		cursor: pointer;
		transition: all 0.2s ease;
		display: flex;
		align-items: center;
		justify-content: center;
		min-width: 32px;
		height: 32px;
	}

	.emoji-btn:hover {
		background: #f9fafb;
		border-color: #3b82f6;
		transform: scale(1.05);
	}

	.emoji-picker-container {
		position: absolute;
		top: 100%;
		left: 0;
		z-index: 1000;
		margin-top: 4px;
	}

	.settings-btn:hover {
		background: #f3f4f6;
		color: #4b5563;
		transform: scale(1.05);
	}

	.chat-btn:hover {
		background: #f0f9ff;
		color: #3b82f6;
		transform: scale(1.05);
	}

	.slots-btn:hover {
		background: #fdf4ff;
		color: #a855f7;
		transform: scale(1.05);
	}

	/* Unread message badge */
	.unread-badge {
		position: absolute;
		top: -2px;
		right: -2px;
		background: #ef4444;
		color: white;
		font-size: 0.65rem;
		font-weight: 600;
		line-height: 1;
		min-width: 16px;
		height: 16px;
		border-radius: 8px;
		display: flex;
		align-items: center;
		justify-content: center;
		padding: 0 4px;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.2);
		z-index: 10;
		border: 1px solid white;
	}

	.remove-btn:disabled {
		color: #e5e7eb;
		cursor: not-allowed;
	}

	.remove-btn:hover:not(:disabled) {
		background: #fef2f2;
		color: #ef4444;
		transform: scale(1.05);
	}

	/* Item container */
	.capacity-item {
		display: flex;
		flex-direction: column;
	}

	.capacity-item.chat-expanded {
		margin-bottom: 2rem;
	}

	/* Filter section styling (similar to Child.svelte contributor layout) */
	.filter-section {
		background: rgba(255, 255, 255, 0.8);
		border-radius: 6px;
		padding: 8px 12px;
		border: 1px solid rgba(229, 231, 235, 0.6);
	}

	.filter-layout {
		display: flex;
		align-items: center;
		gap: 8px;
		flex-wrap: wrap;
	}

	.filter-button-container {
		display: flex;
		align-items: center;
	}

	.add-filter-btn {
		display: inline-flex;
		align-items: center;
		gap: 4px;
		padding: 4px 8px;
		border: 1px dashed #d1d5db;
		border-radius: 12px;
		background: rgba(249, 250, 251, 0.8);
		color: #6b7280;
		font-size: 0.75rem;
		cursor: pointer;
		transition: all 0.2s ease;
		height: 22px;
		line-height: 1;
	}

	.add-filter-btn:hover {
		border-color: #3b82f6;
		color: #3b82f6;
		background: rgba(248, 250, 252, 0.9);
		transform: scale(1.02);
	}

	.add-icon {
		font-size: 0.875rem;
		font-weight: bold;
		line-height: 1;
	}

	.add-text {
		font-weight: 500;
	}

	.filter-tags-container {
		display: flex;
		flex-wrap: wrap;
		gap: 4px;
		align-items: center;
		flex: 1;
	}

	.filter-tag-wrapper {
		display: flex;
		align-items: center;
	}

	.filter-description {
		margin-top: 6px;
		padding-top: 6px;
		border-top: 1px solid rgba(229, 231, 235, 0.4);
	}

	.filter-desc-text {
		font-size: 0.75rem;
		color: #6b7280;
		font-style: italic;
	}

	.empty-state {
		background: rgba(243, 244, 246, 0.5);
		border-radius: 6px;
		border: 1px dashed #d1d5db;
	}

	/* Expanded settings styling */
	.expanded-settings {
		font-size: 0.875rem;
		animation: slideDown 0.25s ease-out;
		border-top: 1px solid rgba(229, 231, 235, 0.5);
		box-shadow:
			0 4px 6px -1px rgba(0, 0, 0, 0.05),
			0 2px 4px -1px rgba(0, 0, 0, 0.03);
	}

	.settings-content {
		padding: 1.5rem;
	}

	.custom-recurrence {
		border: 1px solid rgba(229, 231, 235, 0.7);
	}

	/* Animation for expanding the settings */
	@keyframes slideDown {
		from {
			opacity: 0;
			transform: translateY(-10px);
		}
		to {
			opacity: 1;
			transform: translateY(0);
		}
	}

	/* Radio buttons */
	input[type='radio'] {
		appearance: auto;
		margin-right: 0.5rem;
		color: #3b82f6;
	}

	/* Chat container styling */
	.chat-container {
		animation: slideDown 0.2s ease-out;
	}

	.chat-header h4 {
		margin: 0;
	}

	.chat-header p {
		margin: 0;
	}

	/* Capacity row responsive behavior */
	.capacity-row {
		gap: 8px; /* Slightly smaller gap for better wrapping */
	}

	/* Ensure buttons don't wrap unnecessarily */
	.capacity-row button {
		flex-shrink: 0;
	}

	/* Description field with integrated toggle */
	.description-field-container {
		position: relative;
		display: flex;
		align-items: flex-start;
	}

	.description-expand-btn {
		position: absolute;
		right: 4px;
		top: 50%;
		transform: translateY(-50%);
		background: none;
		border: none;
		color: #9ca3af;
		font-size: 0.75rem;
		cursor: pointer;
		padding: 2px 4px;
		border-radius: 2px;
		transition: all 0.2s ease;
		z-index: 1;
		line-height: 1;
	}

	.description-expand-btn:hover {
		background: #f3f4f6;
		color: #6b7280;
	}

	.description-textarea {
		resize: vertical;
		min-height: 60px;
		padding-right: 24px; /* Make room for the button */
		font-family: inherit;
		font-size: 1rem;
		line-height: 1.4;
	}

	/* Adjust input padding when there's a toggle button */
	.description-field-container input {
		padding-right: 24px;
	}

	/* Slots section styling */
	.slots-section {
		animation: slideDown 0.2s ease-out;
	}

	.slots-header h4 {
		margin: 0;
	}

	.slots-header p {
		margin: 0;
	}

	.add-slot-btn {
		display: inline-flex;
		align-items: center;
		gap: 6px;
		padding: 8px 12px;
		border: 1px dashed #d1d5db;
		border-radius: 6px;
		background: rgba(249, 250, 251, 0.8);
		color: #6b7280;
		font-size: 0.75rem;
		cursor: pointer;
		transition: all 0.2s ease;
		width: 100%;
		justify-content: center;
	}

	.add-slot-btn:hover {
		border-color: #a855f7;
		color: #a855f7;
		background: rgba(253, 244, 255, 0.9);
		transform: scale(1.02);
	}

	.add-slot-btn .add-icon {
		font-size: 0.875rem;
		font-weight: bold;
	}

	.slots-list {
		max-height: 500px;
		overflow-y: auto;
	}

	.empty-slots {
		background: rgba(243, 244, 246, 0.5);
		border-radius: 6px;
		border: 1px dashed #d1d5db;
	}

	/* Animation for expanding sections */
	@keyframes slideDown {
		from {
			opacity: 0;
			transform: translateY(-10px);
		}
		to {
			opacity: 1;
			transform: translateY(0);
		}
	}
</style>
