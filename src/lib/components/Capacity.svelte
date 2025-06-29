<script lang="ts">
	import type { ProviderCapacity } from '$lib/schema';
	import Bar from './Bar.svelte';
	import TagPill from './TagPill.svelte';
	import DropDown from './DropDown.svelte';
	import Chat from './Chat.svelte';
	import CompositionItem from './CompositionItem.svelte';
	import { createSubtreesDataProvider } from '$lib/state.svelte';
	import { Rules } from '$lib/filters';
	import { onMount } from 'svelte';
	import { browser } from '$app/environment';
	import { get } from 'svelte/store';
	import {
		createCapacitiesDataProvider,
		createAllNetworkCapacitiesDataProvider
	} from '$lib/utils/ui-providers.svelte';
	import {
		userDesiredComposeFrom,
		userDesiredComposeInto
	} from '$lib/state/protocol/compose.svelte';

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

	// Create capacity data providers for composition dropdowns
	let composeFromDataProvider = createCapacitiesDataProvider(capacity.id); // Capacities we have shares in
	let composeIntoDataProvider = createAllNetworkCapacitiesDataProvider(capacity.id); // All network capacities

	// Reactive capacity properties for proper binding (matching schema types)
	let capacityName = $state(capacity.name);
	let capacityEmoji = $state(capacity.emoji);
	let capacityQuantity = $state(capacity.quantity);
	let capacityUnit = $state(capacity.unit);
	let capacityLocationType = $state(capacity.location_type);
	let capacityLongitude = $state(capacity.longitude);
	let capacityLatitude = $state(capacity.latitude);
	let capacityAllDay = $state(capacity.all_day);
	let capacityStartDate = $state(capacity.start_date);
	let capacityEndDate = $state(capacity.end_date);
	let capacityStartTime = $state(capacity.start_time);
	let capacityEndTime = $state(capacity.end_time);
	let capacityTimeZone = $state(capacity.time_zone);
	let capacityRecurrence = $state(capacity.recurrence);
	let capacityCustomRecurrenceRepeatEvery = $state(capacity.custom_recurrence_repeat_every);
	let capacityCustomRecurrenceRepeatUnit = $state(capacity.custom_recurrence_repeat_unit);
	let capacityCustomRecurrenceEndType = $state(capacity.custom_recurrence_end_type);
	let capacityCustomRecurrenceEndValue = $state(capacity.custom_recurrence_end_value);
	let capacityMaxNaturalDiv = $state(capacity.max_natural_div);
	let capacityMaxPercentageDiv = $state(capacity.max_percentage_div);
	let capacityHiddenUntilRequestAccepted = $state(capacity.hidden_until_request_accepted);

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
			const item = subtreesDataProvider.items.find((item) => item.id === subtreeId);
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

	// Handler for input events that updates capacity
	function handleCapacityUpdate() {
		// Create updated capacity with current filter rule
		const updatedCapacity = {
			...capacity,
			name: capacityName,
			emoji: capacityEmoji,
			quantity: capacityQuantity,
			unit: capacityUnit,
			location_type: capacityLocationType,
			longitude: capacityLongitude,
			latitude: capacityLatitude,
			all_day: capacityAllDay,
			start_date: capacityStartDate,
			end_date: capacityEndDate,
			start_time: capacityStartTime,
			end_time: capacityEndTime,
			time_zone: capacityTimeZone,
			recurrence: capacityRecurrence,
			custom_recurrence_repeat_every: capacityCustomRecurrenceRepeatEvery,
			custom_recurrence_repeat_unit: capacityCustomRecurrenceRepeatUnit,
			custom_recurrence_end_type: capacityCustomRecurrenceEndType,
			custom_recurrence_end_value: capacityCustomRecurrenceEndValue,
			max_natural_div: capacityMaxNaturalDiv,
			max_percentage_div: capacityMaxPercentageDiv,
			hidden_until_request_accepted: capacityHiddenUntilRequestAccepted,
			filter_rule: filterRule()
		};

		onupdate?.(updatedCapacity);
	}

	// Delete this capacity
	function handleDelete() {
		ondelete?.(capacity.id);
	}

	// Toggle expanded state
	function toggleExpanded() {
		expanded = !expanded;
		// If we're expanding settings, close chat
		if (expanded) {
			chatExpanded = false;
		}
	}

	// Toggle chat state
	function toggleChat() {
		chatExpanded = !chatExpanded;
		// If we're expanding chat, close settings
		if (chatExpanded) {
			expanded = false;
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

	// Helper functions for map marker integration
	export function hasValidCoordinates(): boolean {
		return (
			typeof capacityLatitude === 'number' &&
			typeof capacityLongitude === 'number' &&
			capacityLatitude >= -90 &&
			capacityLatitude <= 90 &&
			capacityLongitude >= -180 &&
			capacityLongitude <= 180
		);
	}

	export function getLngLat(): [number, number] | null {
		if (!hasValidCoordinates()) return null;
		return [capacityLongitude!, capacityLatitude!];
	}

	export function updateCoordinatesFromMarker(lnglat: { lng: number; lat: number }) {
		capacityLongitude = lnglat.lng;
		capacityLatitude = lnglat.lat;
		handleCapacityUpdate();
	}

	// Derive composition capacities directly from stores (reactive)
	let composeFromCapacities = $derived(() => {
		const ourCapacityDesires = $userDesiredComposeFrom[capacity.id];
		const result = ourCapacityDesires ? Object.keys(ourCapacityDesires) : [];
		console.log(
			`[CAPACITY-${capacity.id}] composeFromCapacities:`,
			result,
			'from store:',
			$userDesiredComposeFrom
		);
		return result;
	});

	let composeIntoCapacities = $derived(() => {
		const ourCapacityDesires = $userDesiredComposeInto[capacity.id];
		const result = ourCapacityDesires ? Object.keys(ourCapacityDesires) : [];
		console.log(
			`[CAPACITY-${capacity.id}] composeIntoCapacities:`,
			result,
			'from store:',
			$userDesiredComposeInto
		);
		return result;
	});

	// State for tracking expanded composition items
	let expandedCompositions = $state<Record<string, boolean>>({});

	// Dropdown states for adding capacities
	let showComposeFromDropdown = $state(false);
	let showComposeIntoDropdown = $state(false);
	let composeFromDropdownPosition = $state({ x: 0, y: 0 });
	let composeIntoDropdownPosition = $state({ x: 0, y: 0 });

	// Toggle composition expansion
	function toggleComposition(compositionKey: string) {
		expandedCompositions[compositionKey] = !expandedCompositions[compositionKey];
	}

	// Handle adding capacity to compose from
	function handleAddComposeFrom(event: MouseEvent) {
		event.preventDefault();
		event.stopPropagation();

		const rect = (event.target as HTMLElement).getBoundingClientRect();
		composeFromDropdownPosition = {
			x: rect.left,
			y: rect.bottom + 5
		};

		showComposeFromDropdown = true;
	}

	// Handle adding capacity to compose into
	function handleAddComposeInto(event: MouseEvent) {
		event.preventDefault();
		event.stopPropagation();

		const rect = (event.target as HTMLElement).getBoundingClientRect();
		composeIntoDropdownPosition = {
			x: rect.left,
			y: rect.bottom + 5
		};

		showComposeIntoDropdown = true;
	}

	// Handle selecting capacity for compose from
	function handleComposeFromSelect(detail: { id: string; name: string; metadata?: any }) {
		const { id: capacityId } = detail;

		// Add to store with initial quantity of 0
		const current = get(userDesiredComposeFrom);
		const updated = {
			...current,
			[capacity.id]: {
				...current[capacity.id],
				[capacityId]: 0
			}
		};
		userDesiredComposeFrom.set(updated);

		showComposeFromDropdown = false;
	}

	// Handle selecting capacity for compose into
	function handleComposeIntoSelect(detail: { id: string; name: string; metadata?: any }) {
		const { id: capacityId } = detail;

		// Add to store with initial quantity of 0
		const current = get(userDesiredComposeInto);
		const updated = {
			...current,
			[capacity.id]: {
				...current[capacity.id],
				[capacityId]: 0
			}
		};
		userDesiredComposeInto.set(updated);

		showComposeIntoDropdown = false;
	}

	// Remove capacity from compose from list
	function removeComposeFrom(capacityId: string) {
		const current = get(userDesiredComposeFrom);
		if (current[capacity.id]) {
			const { [capacityId]: _, ...remainingDesires } = current[capacity.id];
			const updated = {
				...current,
				[capacity.id]: remainingDesires
			};
			userDesiredComposeFrom.set(updated);
		}
	}

	// Remove capacity from compose into list
	function removeComposeInto(capacityId: string) {
		const current = get(userDesiredComposeInto);
		if (current[capacity.id]) {
			const { [capacityId]: _, ...remainingDesires } = current[capacity.id];
			const updated = {
				...current,
				[capacity.id]: remainingDesires
			};
			userDesiredComposeInto.set(updated);
		}
	}

	// Close dropdowns
	function handleComposeFromDropdownClose() {
		showComposeFromDropdown = false;
	}

	function handleComposeIntoDropdownClose() {
		showComposeIntoDropdown = false;
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
			onchange={handleCapacityUpdate}
			style="width: {Math.max(capacityName?.length || 0, 'Name'.length) +
				3}ch; min-width: {Math.max(6, 'Name'.length + 2)}ch;"
		/>
		<input
			type="number"
			class="capacity-input qty auto-size text-right"
			min="0"
			step="0.01"
			bind:value={capacityQuantity}
			placeholder="Qty"
			onchange={handleCapacityUpdate}
			style="width: {Math.max(capacityQuantity?.toString().length || 0, 'Qty'.length) +
				3}ch; min-width: {Math.max(5, 'Qty'.length + 2)}ch;"
		/>
		<input
			type="text"
			class="capacity-input unit auto-size"
			bind:value={capacityUnit}
			placeholder="Unit"
			onchange={handleCapacityUpdate}
			style="width: {Math.max(capacityUnit?.length || 0, 'Unit'.length) +
				3}ch; min-width: {Math.max(6, 'Unit'.length + 2)}ch;"
		/>

		<!-- Action buttons -->
		<button
			type="button"
			class="chat-btn ml-1"
			onclick={toggleChat}
			title="Chat about this capacity"
		>
			💬
		</button>
		<button type="button" class="settings-btn ml-1" onclick={toggleExpanded}>
			<svg
				xmlns="http://www.w3.org/2000/svg"
				width="16"
				height="16"
				viewBox="0 0 24 24"
				fill="none"
				stroke="currentColor"
				stroke-width="2"
				stroke-linecap="round"
				stroke-linejoin="round"
			>
				<circle cx="12" cy="12" r="3"></circle>
				<path
					d="M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 0 1 0 2.83 2 2 0 0 1-2.83 0l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 0 1-2 2 2 2 0 0 1-2-2v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 0 1-2.83 0 2 2 0 0 1 0-2.83l.06-.06a1.65 1.65 0 0 0 .33-1.82 1.65 1.65 0 0 0-1.51-1H3a2 2 0 0 1-2-2 2 2 0 0 1 2-2h.09A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 0 1 0-2.83 2 2 0 0 1 2.83 0l.06.06a1.65 1.65 0 0 0 1.82.33H9a1.65 1.65 0 0 0 1-1.51V3a2 2 0 0 1 2-2 2 2 0 0 1 2 2v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 0 1 2.83 0 2 2 0 0 1 0 2.83l-.06.06a1.65 1.65 0 0 0-.33 1.82V9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 0 1 2 2 2 2 0 0 1-2 2h-.09a1.65 1.65 0 0 0-1.51 1z"
				></path>
			</svg>
		</button>
		<button type="button" class="remove-btn ml-1" onclick={handleDelete} disabled={!canDelete}
			>×</button
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
		<!-- Composition section in expanded view -->
		<div class="composition-section mb-4 rounded border border-gray-200 bg-blue-50 p-3">
			<div class="composition-header mb-3">
				<h4 class="text-sm font-medium text-gray-700">🔄 Composition</h4>
				<p class="mt-1 text-xs text-gray-500">
					Enhance {capacity.emoji || '📦'}
					{capacity.name} by composing with other capacities
				</p>
			</div>

			<div class="composition-columns grid grid-cols-1 gap-4 md:grid-cols-2">
				<!-- Compose FROM column -->
				<div class="compose-from-column">
					<div class="column-header mb-3">
						<h5 class="flex items-center gap-2 text-sm font-medium text-gray-700">
							<span>FROM</span>
						</h5>
						<p class="mt-1 text-xs text-gray-500">Use their capacities to enhance yours</p>
					</div>

					<div class="column-content">
						<!-- Add capacity button -->
						<button type="button" class="add-capacity-btn mb-3" onclick={handleAddComposeFrom}>
							<span class="add-icon">+</span>
							<span class="add-text">Add capacity to compose from</span>
						</button>

						<!-- Compose from items -->
						{#if composeFromCapacities().length > 0}
							<div class="composition-items space-y-2">
								{#each composeFromCapacities() as capacityId}
									{@const theirCapacity = composeFromDataProvider.items.find(
										(item) => item.id === capacityId
									)?.metadata}
									{#if theirCapacity}
										{@const compositionKey = `${capacity.id}-${capacityId}-from`}
										<CompositionItem
											ourCapacity={{
												id: capacity.id,
												name: capacity.name,
												emoji: capacity.emoji,
												unit: capacity.unit || 'unit',
												quantity: capacity.quantity || 0,
												provider_name: 'You'
											}}
											theirCapacity={{
												...theirCapacity,
												id: capacityId,
												provider_id: theirCapacity.owner_id || theirCapacity.provider_id
											}}
											direction="from"
											expanded={expandedCompositions[compositionKey] || false}
											onToggle={() => toggleComposition(compositionKey)}
										/>
									{/if}
								{/each}
							</div>
						{:else}
							<div class="empty-state py-4 text-center text-xs text-gray-500 italic">
								No capacities selected for composition
							</div>
						{/if}
					</div>
				</div>

				<!-- Compose INTO column -->
				<div class="compose-into-column">
					<div class="column-header mb-3">
						<h5 class="flex items-center gap-2 text-sm font-medium text-gray-700">
							<span>INTO</span>
						</h5>
						<p class="mt-1 text-xs text-gray-500">Use your capacity to enhance theirs</p>
					</div>

					<div class="column-content">
						<!-- Add capacity button -->
						<button type="button" class="add-capacity-btn mb-3" onclick={handleAddComposeInto}>
							<span class="add-icon">+</span>
							<span class="add-text">Add capacity to compose into</span>
						</button>

						<!-- Compose into items -->
						{#if composeIntoCapacities().length > 0}
							<div class="composition-items space-y-2">
								{#each composeIntoCapacities() as capacityId}
									{@const theirCapacity = composeIntoDataProvider.items.find(
										(item) => item.id === capacityId
									)?.metadata}
									{#if theirCapacity}
										{@const compositionKey = `${capacity.id}-${capacityId}-into`}
										<CompositionItem
											ourCapacity={{
												id: capacity.id,
												name: capacity.name,
												emoji: capacity.emoji,
												unit: capacity.unit || 'unit',
												quantity: capacity.quantity || 0,
												provider_name: 'You'
											}}
											theirCapacity={{
												...theirCapacity,
												id: capacityId,
												provider_id: theirCapacity.owner_id || theirCapacity.provider_id
											}}
											direction="into"
											expanded={expandedCompositions[compositionKey] || false}
											onToggle={() => toggleComposition(compositionKey)}
										/>
									{/if}
								{/each}
							</div>
						{:else}
							<div class="empty-state py-4 text-center text-xs text-gray-500 italic">
								No capacities selected for composition
							</div>
						{/if}
					</div>
				</div>
			</div>
		</div>

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

	<!-- Expanded settings (space-time only) -->
	{#if expanded}
		<div class="expanded-settings mt-2 rounded-md bg-white shadow-sm">
			<div class="settings-content">
				<div class="space-time-section mb-6">
					<h4 class="mb-4 text-sm font-medium text-gray-700">Availability</h4>

					<div class="space-time-options mb-6 ml-2">
						<div class="flex flex-wrap gap-x-8 gap-y-3">
							<label class="inline-flex items-center">
								<input
									type="radio"
									name="location-type-{capacity.id}"
									value="Undefined"
									bind:group={capacityLocationType}
									class="mr-3"
									onchange={handleCapacityUpdate}
								/>
								<span class="text-sm text-gray-600">Undefined</span>
							</label>
							<label class="inline-flex items-center">
								<input
									type="radio"
									name="location-type-{capacity.id}"
									value="LiveLocation"
									bind:group={capacityLocationType}
									class="mr-3"
									onchange={handleCapacityUpdate}
								/>
								<span class="text-sm text-gray-600">Live location</span>
							</label>
							<label class="inline-flex items-center">
								<input
									type="radio"
									name="location-type-{capacity.id}"
									value="Specific"
									bind:group={capacityLocationType}
									class="mr-3"
									onchange={handleCapacityUpdate}
								/>
								<span class="text-sm text-gray-600">Specific</span>
							</label>
						</div>
					</div>

					{#if capacityLocationType === 'Specific'}
						<div class="date-time-section mb-6 ml-2">
							<!-- Geographic coordinates section -->
							<div class="mb-6">
								<h5 class="mb-4 text-sm font-medium text-gray-600">Geographic Coordinates</h5>
								<div class="grid grid-cols-2 gap-4">
									<div>
										<label class="mb-2 block text-xs text-gray-500">Latitude (-90 to 90)</label>
										<input
											type="number"
											min="-90"
											max="90"
											step="0.000001"
											class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
											bind:value={capacityLatitude}
											placeholder="e.g. 37.7749"
											onchange={handleCapacityUpdate}
										/>
									</div>
									<div>
										<label class="mb-2 block text-xs text-gray-500">Longitude (-180 to 180)</label>
										<input
											type="number"
											min="-180"
											max="180"
											step="0.000001"
											class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
											bind:value={capacityLongitude}
											placeholder="e.g. -122.4194"
											onchange={handleCapacityUpdate}
										/>
									</div>
								</div>
							</div>

							<div class="mb-4 ml-1">
								<label class="inline-flex items-center">
									<input
										type="checkbox"
										bind:checked={capacityAllDay}
										class="mr-3 h-4 w-4"
										onchange={handleCapacityUpdate}
									/>
									<span class="text-sm text-gray-600">All day</span>
								</label>
							</div>

							<div class="mb-4 flex flex-col gap-6">
								<div>
									<h5 class="mb-2 text-sm text-gray-600">Start Date</h5>
									<input
										type="date"
										class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
										bind:value={capacityStartDate}
										onchange={handleCapacityUpdate}
									/>
								</div>

								<div>
									<h5 class="mb-2 text-sm text-gray-600">End Date</h5>
									<input
										type="date"
										class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
										bind:value={capacityEndDate}
										onchange={handleCapacityUpdate}
									/>
								</div>
							</div>

							{#if !capacityAllDay}
								<div class="mb-4 flex flex-col gap-4 md:flex-row">
									<div class="md:w-1/2">
										<input
											type="time"
											class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
											bind:value={capacityStartTime}
											onchange={handleCapacityUpdate}
										/>
									</div>

									<div class="flex items-center md:w-1/2">
										<span class="mx-2 hidden text-gray-400 md:inline">to</span>
										<input
											type="time"
											class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
											bind:value={capacityEndTime}
											onchange={handleCapacityUpdate}
										/>
									</div>
								</div>

								<div class="mb-4">
									<button class="text-sm text-blue-600 hover:text-blue-800 focus:outline-none">
										Time zone
									</button>
									<div class="mt-2">
										<input
											type="text"
											class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
											bind:value={capacityTimeZone}
											onchange={handleCapacityUpdate}
										/>
									</div>
								</div>
							{/if}

							<!-- Recurrence dropdown -->
							<div class="mb-6">
								<div class="relative w-full md:w-auto">
									<select
										class="capacity-select w-full appearance-none rounded-md bg-gray-100 px-3 py-2"
										bind:value={capacityRecurrence}
										onchange={handleCapacityUpdate}
									>
										{#each recurrenceOptions as option}
											<option value={option}>{option}</option>
										{/each}
									</select>
								</div>
							</div>

							<!-- Custom recurrence options -->
							{#if capacityRecurrence === 'Custom...' && capacityCustomRecurrenceRepeatEvery !== undefined}
								<div class="custom-recurrence mt-4 rounded-md bg-gray-50 p-6">
									<div class="mb-6 flex items-center">
										<span class="mr-3 text-sm font-medium text-gray-600">Repeat every</span>
										<input
											type="number"
											min="1"
											class="capacity-input qty w-16 text-right"
											bind:value={capacityCustomRecurrenceRepeatEvery}
											onchange={handleCapacityUpdate}
										/>
										<select
											class="capacity-select ml-3 w-28"
											bind:value={capacityCustomRecurrenceRepeatUnit}
											onchange={handleCapacityUpdate}
										>
											<option value="days">days</option>
											<option value="weeks">weeks</option>
											<option value="months">months</option>
											<option value="years">years</option>
										</select>
									</div>

									<div class="ends-section ml-2">
										<span class="mb-4 block text-sm font-medium text-gray-600">Ends</span>

										<div class="radio-options space-y-4">
											<label class="inline-flex items-center">
												<input
													type="radio"
													name="ends-{capacity.id}"
													value="never"
													checked={capacityCustomRecurrenceEndType === 'never'}
													onchange={() => {
														capacityCustomRecurrenceEndType = 'never';
														handleCapacityUpdate();
													}}
													class="mr-3"
												/>
												<span class="text-sm text-gray-600">Never</span>
											</label>

											<div class="flex items-center">
												<label class="inline-flex items-center">
													<input
														type="radio"
														name="ends-{capacity.id}"
														value="endsOn"
														checked={capacityCustomRecurrenceEndType === 'endsOn'}
														onchange={() => {
															capacityCustomRecurrenceEndType = 'endsOn';
															capacityCustomRecurrenceEndValue = formatDateForInput(new Date());
															handleCapacityUpdate();
														}}
														class="mr-3"
													/>
													<span class="text-sm text-gray-600">On</span>
												</label>
												{#if capacityCustomRecurrenceEndType === 'endsOn'}
													<div class="ml-6">
														<input
															type="date"
															class="capacity-input w-40 rounded-md bg-gray-100 px-3 py-2"
															bind:value={capacityCustomRecurrenceEndValue}
															onchange={handleCapacityUpdate}
														/>
													</div>
												{/if}
											</div>

											<div class="flex items-center">
												<label class="inline-flex items-center">
													<input
														type="radio"
														name="ends-{capacity.id}"
														value="endsAfter"
														checked={capacityCustomRecurrenceEndType === 'endsAfter'}
														onchange={() => {
															capacityCustomRecurrenceEndType = 'endsAfter';
															capacityCustomRecurrenceEndValue = '5';
															handleCapacityUpdate();
														}}
														class="mr-3"
													/>
													<span class="text-sm text-gray-600">After</span>
												</label>
												{#if capacityCustomRecurrenceEndType === 'endsAfter'}
													<div class="ml-6 flex items-center">
														<input
															type="number"
															min="1"
															class="capacity-input qty w-16 text-right"
															bind:value={capacityCustomRecurrenceEndValue}
															onchange={handleCapacityUpdate}
														/>
														<span class="ml-2 text-sm text-gray-600">occurrences</span>
													</div>
												{/if}
											</div>
										</div>
									</div>
								</div>
							{/if}
						</div>
					{/if}
				</div>

				<div class="hidden-option mb-4">
					<label class="inline-flex items-center">
						<input
							type="checkbox"
							bind:checked={capacityHiddenUntilRequestAccepted}
							class="mr-3 h-4 w-4"
							onchange={handleCapacityUpdate}
						/>
						<span class="text-sm font-medium text-gray-600">Hidden till Request Accepted</span>
					</label>
				</div>

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
									onchange={handleCapacityUpdate}
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
									onchange={handleCapacityUpdate}
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

<!-- Compose from dropdown -->
{#if showComposeFromDropdown}
	<DropDown
		position={composeFromDropdownPosition}
		show={showComposeFromDropdown}
		title="Select Capacity to Compose From"
		searchPlaceholder="Search capacities you have shares in..."
		dataProvider={composeFromDataProvider}
		select={handleComposeFromSelect}
		close={handleComposeFromDropdownClose}
	/>
{/if}

<!-- Compose into dropdown -->
{#if showComposeIntoDropdown}
	<DropDown
		position={composeIntoDropdownPosition}
		show={showComposeIntoDropdown}
		title="Select Capacity to Compose Into"
		searchPlaceholder="Search all network capacities..."
		dataProvider={composeIntoDataProvider}
		select={handleComposeIntoSelect}
		close={handleComposeIntoDropdownClose}
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
	.chat-btn {
		background: none;
		border: none;
		color: #cbd5e1;
		font-size: 1.3em;
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

	.settings-btn {
		font-size: 1em;
	}

	.settings-btn:hover {
		background: #f3f4f6;
		color: #4b5563;
		transform: scale(1.05);
	}

	.chat-btn {
		font-size: 1em;
	}

	.chat-btn:hover {
		background: #f0f9ff;
		color: #3b82f6;
		transform: scale(1.05);
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

	/* Composition section styling */
	.composition-section {
		animation: slideDown 0.2s ease-out;
	}

	.composition-header h4 {
		margin: 0;
	}

	.composition-header p {
		margin: 0;
	}

	.composition-columns {
		gap: 1rem;
	}

	.compose-from-column,
	.compose-into-column {
		background: rgba(255, 255, 255, 0.5);
		border-radius: 8px;
		padding: 16px;
		border: 1px solid rgba(229, 231, 235, 0.6);
	}

	.column-header h5 {
		margin: 0;
	}

	.column-header p {
		margin: 0;
	}

	.add-capacity-btn {
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

	.add-capacity-btn:hover {
		border-color: #3b82f6;
		color: #3b82f6;
		background: rgba(248, 250, 252, 0.9);
		transform: scale(1.02);
	}

	.add-capacity-btn .add-icon {
		font-size: 0.875rem;
		font-weight: bold;
	}

	.remove-composition-btn {
		background: rgba(239, 68, 68, 0.1);
		color: #dc2626;
		border: 1px solid rgba(239, 68, 68, 0.2);
		border-radius: 4px;
		padding: 4px 8px;
		cursor: pointer;
		transition: all 0.2s ease;
		margin-top: 4px;
	}

	.remove-composition-btn:hover {
		background: rgba(239, 68, 68, 0.2);
		border-color: #dc2626;
	}

	.composition-items {
		max-height: 400px;
		overflow-y: auto;
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
</style>
