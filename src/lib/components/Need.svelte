<script lang="ts">
	import type { UserNeed, NeedSlot as NeedSlotType } from '$lib/schema';
	import TagPill from '$lib/components/TagPill.svelte';
	import DropDown from '$lib/components/DropDown.svelte';
	import Chat from '$lib/components/Chat.svelte';

	import NeedSlot from './NeedSlot.svelte';
	import { Rules } from '$lib/filters';
	import { onMount } from 'svelte';
	import { browser } from '$app/environment';
	import { createSubtreesDataProvider } from '$lib/utils/ui-providers.svelte';
	import { globalState } from '$lib/global.svelte';
	import { UserNeedSchema } from '$lib/schema';
	import { getReactiveUnreadCount } from '$lib/state/chat.svelte';

	interface Props {
		need: UserNeed;
		canDelete: boolean;
		onupdate?: (need: UserNeed) => void;
		ondelete?: (id: string) => void;
	}

	let { need, canDelete, onupdate, ondelete }: Props = $props();

	// UI state for expanded need editing
	let expanded = $state(false);

	// UI state for expanded chat
	let chatExpanded = $state(false);

	// Get reactive unread message count for this need's chat
	let unreadCount = getReactiveUnreadCount(need.id);

	// UI state for expanded slots
	let slotsExpanded = $state(false);

	// Slot filtering and ordering state
	let slotFilterBy = $state<'all' | 'time' | 'location' | 'quantity'>('all');
	let slotSortBy = $state<'time' | 'location' | 'quantity'>('time');
	let slotSortDirection = $state<'asc' | 'desc'>('asc');

	// Section expansion state
	let pastSlotsExpanded = $state(false);
	let recurringSlotsExpanded = $state(true);
	let currentFutureSlotsExpanded = $state(true);

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

	// Reactive need properties for proper binding (matching schema types)
	let needName = $state(need.name);
	let needEmoji = $state(need.emoji);
	let needUnit = $state(need.unit);
	let needDescription = $state(need.description);
	let needCategory = $state(need.category);
	let needImportance = $state(need.importance);
	let needRecurring = $state(need.recurring);

	// Derived provider filter - automatically updates when selectedSubtrees changes
	let providerFilter = $derived(() => {
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
					needEmoji = event.detail.unicode;
					handleNeedUpdate();
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

	// Note: Recipient shares are no longer displayed as bars in the new efficient allocation system
	// Recipients can see their allocations directly in the shares view

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
			handleNeedUpdate();
		}
	}

	// Handler for input events that updates need
	function handleNeedUpdate() {
		// Create updated need with current provider filter
		const updatedNeed = {
			...need,
			name: needName,
			emoji: needEmoji,
			unit: needUnit,
			description: needDescription,
			category: needCategory,
			importance: needImportance,
			recurring: needRecurring,
			provider_filter: providerFilter()
		};

		// Validate using schema
		const validationResult = UserNeedSchema.safeParse(updatedNeed);

		if (!validationResult.success) {
			// Check for specific validation errors and show appropriate messages
			const errors = validationResult.error.issues;
			const unitErrors = errors.filter((issue) => issue.path.includes('unit'));

			if (unitErrors.length > 0) {
				globalState.showToast(unitErrors[0].message, 'warning');
				// Revert unit to previous valid value
				needUnit = need.unit || '';
				return;
			}

			// Handle other validation errors
			globalState.showToast('Invalid need data', 'error');
			console.error('Need validation failed:', validationResult.error);
			return;
		}

		// Validation passed, proceed with update
		onupdate?.(validationResult.data);
	}

	// Delete this need
	function handleDelete() {
		ondelete?.(need.id);
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

		// Update need
		handleNeedUpdate();

		// Close dropdown
		showSubtreeDropdown = false;
	}

	// Handle removing a subtree filter
	function handleRemoveSubtree(subtreeId: string) {
		selectedSubtrees = selectedSubtrees.filter((id) => id !== subtreeId);
		handleNeedUpdate();
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

	// Utility function to check if a slot is recurring
	function isSlotRecurring(slot: any): boolean {
		return slot.recurrence && slot.recurrence !== 'Does not repeat' && slot.recurrence !== null;
	}

	// Utility function to safely display recurrence value
	function getRecurrenceDisplay(slot: any): string {
		return slot.recurrence || 'Does not repeat';
	}

	// Helper function to safely extract time from potentially malformed time strings
	function safeExtractTime(timeValue: string | null | undefined): string | undefined {
		if (!timeValue) return undefined;
		if (/^\d{2}:\d{2}$/.test(timeValue)) {
			return timeValue;
		}
		if (timeValue.includes('T')) {
			try {
				const date = new Date(timeValue);
				return date.toTimeString().substring(0, 5);
			} catch (e) {
				console.warn('Failed to parse time:', timeValue);
				return undefined;
			}
		}
		console.warn('Unknown time format:', timeValue);
		return undefined;
	}

	// Helper function to parse slot dates and times consistently
	function parseSlotDateTime(slot: any): {
		slotStart: Date | null;
		slotEnd: Date | null;
	} {
		const slotStart = slot.start_date ? new Date(slot.start_date) : null;
		let slotEnd = slot.end_date ? new Date(slot.end_date) : slotStart ? new Date(slotStart) : null;

		// For all-day events, don't add time components - work with dates only
		if (slot.all_day) {
			// For all-day events, set start to beginning of day and end to end of day
			if (slotStart) {
				slotStart.setHours(0, 0, 0, 0);
			}
			if (slotEnd) {
				slotEnd.setHours(23, 59, 59, 999);
			} else if (slotStart) {
				// If no end date, all-day event ends at end of start day
				slotEnd = new Date(slotStart);
				slotEnd.setHours(23, 59, 59, 999);
			}
		} else {
			// For timed events, add time components using safe extraction
			if (slotStart && slot.start_time) {
				const safeStartTime = safeExtractTime(slot.start_time);
				if (safeStartTime) {
					const [hours, minutes] = safeStartTime.split(':');
					slotStart.setHours(parseInt(hours), parseInt(minutes), 0, 0);
				}
			}

			if (slotEnd && slot.end_time) {
				const safeEndTime = safeExtractTime(slot.end_time);
				if (safeEndTime) {
					const [hours, minutes] = safeEndTime.split(':');
					slotEnd.setHours(parseInt(hours), parseInt(minutes), 59, 999);
				}
			}

			// Handle missing end times for timed events (only when no end_date was specified)
			if (slotStart && !slot.end_date) {
				if (!slot.start_time && !slot.end_time) {
					// No specific times - treat as all-day
					slotEnd = new Date(slotStart);
					slotEnd.setHours(23, 59, 59, 999);
				} else if (slot.start_time && !slot.end_time) {
					// Has start time but no end time - assume 1 hour duration
					slotEnd = new Date(slotStart.getTime() + 60 * 60 * 1000);
				}
			}
		}

		return { slotStart, slotEnd };
	}

	// Utility function to check if a slot is in the past
	function isSlotInPast(slot: any): boolean {
		if (isSlotRecurring(slot)) return false;

		const now = new Date();
		const { slotStart, slotEnd } = parseSlotDateTime(slot);

		if (!slotStart) return false;

		// Use the effective end time (parseSlotDateTime handles all-day vs timed logic)
		const effectiveEndTime = slotEnd || slotStart;
		return effectiveEndTime < now;
	}

	// Utility function to get slot priority for sorting
	function getSlotSortValue(slot: any, sortBy: string): number | string {
		switch (sortBy) {
			case 'time':
				if (!slot.start_date) return '9999-12-31';
				return slot.start_date;
			case 'location':
				if (slot.location_type === 'Specific') {
					if (slot.street_address) {
						return slot.street_address.toLowerCase();
					}
					if (slot.latitude && slot.longitude) {
						return `${slot.latitude.toFixed(4)}, ${slot.longitude.toFixed(4)}`;
					}
				} else if (slot.location_type === 'Online') {
					if (slot.online_link) {
						return slot.online_link.toLowerCase();
					}
				}
				return (slot.location_type || 'No location').toLowerCase();
			case 'quantity':
				return slot.quantity || 0;
			default:
				return 0;
		}
	}

	// Categorize and sort slots
	let categorizedSlots = $derived(() => {
		if (!need.need_slots || !Array.isArray(need.need_slots)) {
			return { past: [], recurring: [], currentFuture: [] };
		}

		const past: any[] = [];
		const recurring: any[] = [];
		const currentFuture: any[] = [];

		// Categorize slots
		need.need_slots.forEach((slot) => {
			if (isSlotRecurring(slot)) {
				recurring.push(slot);
			} else if (isSlotInPast(slot)) {
				past.push(slot);
			} else {
				currentFuture.push(slot);
			}
		});

		// Sort each category
		const sortFn = (a: any, b: any) => {
			const aValue = getSlotSortValue(a, slotSortBy);
			const bValue = getSlotSortValue(b, slotSortBy);

			if (typeof aValue === 'string' && typeof bValue === 'string') {
				const comparison = aValue.localeCompare(bValue);
				return slotSortDirection === 'asc' ? comparison : -comparison;
			}

			const comparison = (aValue as number) - (bValue as number);
			return slotSortDirection === 'asc' ? comparison : -comparison;
		};

		past.sort(sortFn);
		recurring.sort(sortFn);
		currentFuture.sort(sortFn);

		return { past, recurring, currentFuture };
	});

	// Get total slot count for display
	let totalSlotCount = $derived(() => {
		const { past, recurring, currentFuture } = categorizedSlots();
		return past.length + recurring.length + currentFuture.length;
	});

	// State for description field expansion
	let descriptionExpanded = $state(false);

	// Track original values for change detection
	let originalValues = $state<Record<string, any>>({});

	// Handle slot updates
	function handleSlotUpdate(updatedSlot: NeedSlotType) {
		// 🚨 DEBUG: Log the incoming updated slot
		console.log('[CAPACITY] 🚨 DEBUG: handleSlotUpdate called with slot:', updatedSlot.id);
		console.log('[CAPACITY] 🚨 DEBUG: Updated slot location data:', {
			location_type: updatedSlot.location_type,
			latitude: updatedSlot.latitude,
			longitude: updatedSlot.longitude,
			street_address: updatedSlot.street_address,
			city: updatedSlot.city,
			state_province: updatedSlot.state_province,
			postal_code: updatedSlot.postal_code,
			country: updatedSlot.country
		});

		const updatedSlots = need.need_slots.map((slot) =>
			slot.id === updatedSlot.id ? updatedSlot : slot
		);

		// 🚨 DEBUG: Log the updated slots array
		console.log('[CAPACITY] 🚨 DEBUG: Updated slots array:');
		updatedSlots.forEach((slot, index) => {
			if (slot.id === updatedSlot.id) {
				console.log(`[CAPACITY] 🚨 DEBUG: Updated slot ${index} (${slot.id}) location data:`, {
					location_type: slot.location_type,
					latitude: slot.latitude,
					longitude: slot.longitude,
					street_address: slot.street_address,
					city: slot.city,
					state_province: slot.state_province,
					postal_code: slot.postal_code,
					country: slot.country
				});
			}
		});

		const updatedNeed = {
			...need,
			need_slots: updatedSlots
		};

		// 🚨 DEBUG: Log the final updated need
		console.log('[NEED] 🚨 DEBUG: Final updated need need_slots:');
		updatedNeed.need_slots.forEach((slot, index) => {
			if (slot.id === updatedSlot.id) {
				console.log(`[NEED] 🚨 DEBUG: Final need slot ${index} (${slot.id}) location data:`, {
					location_type: slot.location_type,
					latitude: slot.latitude,
					longitude: slot.longitude,
					street_address: slot.street_address,
					city: slot.city,
					state_province: slot.state_province,
					postal_code: slot.postal_code,
					country: slot.country
				});
			}
		});

		onupdate?.(updatedNeed);
	}

	// Handle slot deletion
	function handleSlotDelete(slotId: string) {
		const updatedSlots = need.need_slots.filter((slot) => slot.id !== slotId);

		const updatedNeed = {
			...need,
			need_slots: updatedSlots
		};

		onupdate?.(updatedNeed);
	}

	// Track the ID of the most recently added slot for scrolling and highlighting
	let recentlyAddedSlotId = $state<string | null>(null);

	// Add new slot
	function handleAddSlot() {
		const todayString = new Date().toISOString().split('T')[0]; // Get YYYY-MM-DD format
		const newSlotId = `slot-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
		const newSlot = {
			id: newSlotId,
			quantity: 1,
			location_type: 'Undefined',
			all_day: true,
			start_date: todayString,
			start_time: null,
			end_date: null,
			end_time: null,
			recurrence: 'Does not repeat' // Start with no recurrence for clarity
		};

		const updatedSlots = [...need.need_slots, newSlot];

		const updatedNeed = {
			...need,
			need_slots: updatedSlots
		};

		// Ensure slots section is expanded so user can see the new slot
		if (!slotsExpanded) {
			slotsExpanded = true;
		}

		// Ensure the current/future slots section is expanded (where new slots appear)
		currentFutureSlotsExpanded = true;

		// Track the new slot ID for scrolling and highlighting
		recentlyAddedSlotId = newSlotId;

		// Add to highlighted slots using global state
		globalState.highlightSlot(newSlotId);

		onupdate?.(updatedNeed);
	}

	// Clean up tracking ID after highlighting (the global state handles scrolling now)
	$effect(() => {
		if (recentlyAddedSlotId) {
			// Clear the tracking ID after a short delay
			setTimeout(() => {
				recentlyAddedSlotId = null;
			}, 100);
		}
	});
</script>

<div class="need-item" class:chat-expanded={chatExpanded} data-need-id={need.id}>
	<!-- Note: Recipient shares bars removed - recipients see allocations in shares view via efficient algorithm -->

	<div class="need-row flex flex-wrap items-center gap-2 rounded bg-white p-2 shadow-sm">
		<!-- Emoji picker button -->
		<div class="relative">
			<button
				type="button"
				class="emoji-btn"
				onclick={handleEmojiPickerToggle}
				title="Select emoji"
			>
				{needEmoji || '🙋'}
			</button>
			<!-- Emoji picker container -->
			{#if showEmojiPicker}
				<div bind:this={emojiPickerContainer} class="emoji-picker-container"></div>
			{/if}
		</div>

		<!-- Main need inputs -->
		<input
			type="text"
			class="need-input name auto-size"
			bind:value={needName}
			placeholder="Need name"
			onfocus={() => handleFocus('name', needName)}
			onblur={() => handleBlurIfChanged('name', needName)}
			style="width: {Math.max(needName?.length || 0, 'Need name'.length) +
				3}ch; min-width: {Math.max(6, 'Need name'.length + 2)}ch;"
		/>
		<input
			type="text"
			class="need-input unit auto-size"
			bind:value={needUnit}
			placeholder="Unit"
			onfocus={() => handleFocus('unit', needUnit)}
			onblur={() => handleBlurIfChanged('unit', needUnit)}
			style="width: {Math.max(needUnit?.length || 0, 'Unit'.length) + 3}ch; min-width: {Math.max(
				6,
				'Unit'.length + 2
			)}ch;"
		/>
		<!-- Description field with integrated toggle -->
		<div class="description-field-container">
			{#if descriptionExpanded}
				<textarea
					class="need-input description-textarea auto-size"
					bind:value={needDescription}
					placeholder="Need description"
					onfocus={() => handleFocus('description', needDescription)}
					onblur={() => handleBlurIfChanged('description', needDescription)}
					rows="3"
				></textarea>
			{:else}
				<input
					type="text"
					class="need-input description auto-size"
					bind:value={needDescription}
					placeholder="Need description"
					onfocus={() => handleFocus('description', needDescription)}
					onblur={() => handleBlurIfChanged('description', needDescription)}
					style="width: {Math.max(needDescription?.length || 0, 'Need description'.length) +
						3}ch; min-width: {Math.max(12, 'Need description'.length + 2)}ch;"
				/>
			{/if}
			{#if needDescription}
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
			title="Chat about this need"
		>
			💬
			{#if $unreadCount > 0}
				<span class="unread-badge">{$unreadCount > 99 ? '99+' : $unreadCount}</span>
			{/if}
		</button>

		<button type="button" class="slots-btn ml-1" onclick={toggleSlots} title="Manage need slots">
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
					Only providers from {selectedSubtrees.length === 1
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
					💬 Chat about {need.emoji || '🙋'}
					{need.name}
				</h4>
				<p class="mt-1 text-xs text-gray-500">
					Discuss this need with potential providers and other stakeholders
				</p>
			</div>
			<Chat chatId={need.id} placeholder={`Discuss ${need.name}...`} maxLength={200} />
		</div>
	{/if}

	<!-- Expanded slots section -->
	{#if slotsExpanded}
		<div class="slots-section mb-4 rounded border border-gray-200 bg-orange-50 p-3">
			<div class="slots-header mb-3">
				<h4 class="text-sm font-medium text-gray-700">🕒 Need Slots</h4>
				<p class="mt-1 text-xs text-gray-500">
					Manage specific time and location slots for when this need arises ({totalSlotCount()} total)
				</p>
			</div>

			<!-- Add slot button -->
			<button type="button" class="add-slot-btn mb-4" onclick={handleAddSlot}>
				<span class="add-icon">+</span>
				<span class="add-text">Add new slot</span>
			</button>

			<!-- Slot controls -->
			<div class="slots-controls mb-4 rounded border border-gray-200 bg-white p-3">
				<div class="flex flex-wrap items-center gap-2">
					<span class="text-xs font-medium text-gray-600">Sort by:</span>
					<select class="slot-control-select" bind:value={slotSortBy}>
						<option value="time">Time</option>
						<option value="location">Location</option>
						<option value="quantity">Quantity</option>
					</select>
					<button
						class="slot-control-btn"
						onclick={() => (slotSortDirection = slotSortDirection === 'asc' ? 'desc' : 'asc')}
						title="Toggle sort direction"
					>
						{slotSortDirection === 'asc' ? '↑' : '↓'}
					</button>
				</div>
			</div>

			<div class="slots-content space-y-4">
				{#if totalSlotCount() > 0}
					<!-- Recurring slots section -->
					{#if categorizedSlots().recurring.length > 0}
						<div class="slot-category">
							<button
								class="category-header"
								onclick={() => (recurringSlotsExpanded = !recurringSlotsExpanded)}
							>
								<span class="category-icon">{recurringSlotsExpanded ? '▼' : '▶'}</span>
								<span class="category-title"
									>🔄 Recurring Needs ({categorizedSlots().recurring.length})</span
								>
							</button>
							{#if recurringSlotsExpanded}
								<div class="category-content">
									{#each categorizedSlots().recurring as slot (slot.id)}
										<div
											class="slot-wrapper"
											data-slot-id={slot.id}
											class:newly-created={globalState.highlightedSlots.has(slot.id)}
										>
											<div class="slot-badges mb-2">
												<span class="recurrence-badge bg-purple-100 text-purple-800">
													{getRecurrenceDisplay(slot)}
												</span>
											</div>
											<NeedSlot
												{slot}
												needId={need.id}
												unit={need.unit}
												canDelete={need.need_slots.length > 1}
												onupdate={handleSlotUpdate}
												ondelete={handleSlotDelete}
											/>
										</div>
									{/each}
								</div>
							{/if}
						</div>
					{/if}

					<!-- Current/Future slots section -->
					{#if categorizedSlots().currentFuture.length > 0}
						<div class="slot-category">
							<button
								class="category-header"
								onclick={() => (currentFutureSlotsExpanded = !currentFutureSlotsExpanded)}
							>
								<span class="category-icon">{currentFutureSlotsExpanded ? '▼' : '▶'}</span>
								<span class="category-title"
									>📅 Current & Upcoming Needs ({categorizedSlots().currentFuture.length})</span
								>
							</button>
							{#if currentFutureSlotsExpanded}
								<div class="category-content">
									{#each categorizedSlots().currentFuture as slot (slot.id)}
										<div
											data-slot-id={slot.id}
											class:newly-created={globalState.highlightedSlots.has(slot.id)}
										>
											<NeedSlot
												{slot}
												needId={need.id}
												unit={need.unit}
												canDelete={need.need_slots.length > 1}
												onupdate={handleSlotUpdate}
												ondelete={handleSlotDelete}
											/>
										</div>
									{/each}
								</div>
							{/if}
						</div>
					{/if}

					<!-- Past slots section -->
					{#if categorizedSlots().past.length > 0}
						<div class="slot-category">
							<button
								class="category-header"
								onclick={() => (pastSlotsExpanded = !pastSlotsExpanded)}
							>
								<span class="category-icon">{pastSlotsExpanded ? '▼' : '▶'}</span>
								<span class="category-title">📜 Past Needs ({categorizedSlots().past.length})</span>
							</button>
							{#if pastSlotsExpanded}
								<div class="category-content">
									{#each categorizedSlots().past as slot (slot.id)}
										<div
											class="slot-wrapper past-slot"
											data-slot-id={slot.id}
											class:newly-created={globalState.highlightedSlots.has(slot.id)}
										>
											<NeedSlot
												{slot}
												needId={need.id}
												unit={need.unit}
												canDelete={need.need_slots.length > 1}
												onupdate={handleSlotUpdate}
												ondelete={handleSlotDelete}
											/>
										</div>
									{/each}
								</div>
							{/if}
						</div>
					{/if}
				{:else}
					<div class="empty-slots py-6 text-center text-xs text-gray-500 italic">
						No slots defined. Click "Add new slot" to get started.
					</div>
				{/if}
			</div>
		</div>
	{/if}

	<!-- Expanded settings (need-specific options) -->
	{#if expanded}
		<div class="expanded-settings mt-2 rounded-md bg-white shadow-sm">
			<div class="settings-content">
				<div class="need-options mb-4">
					<div class="need-properties-section mb-6">
						<h4 class="mb-4 text-sm font-medium text-gray-700">Need Properties</h4>
						<div class="grid grid-cols-2 gap-8">
							<div>
								<label for="need-category" class="mb-1 block text-xs text-gray-500">Category</label>
								<input
									id="need-category"
									type="text"
									class="need-input w-full"
									bind:value={needCategory}
									placeholder="e.g., food, transport, skills"
									onfocus={() => handleFocus('category', needCategory)}
									onblur={() => handleBlurIfChanged('category', needCategory)}
								/>
							</div>
							<div>
								<label for="need-importance" class="mb-1 block text-xs text-gray-500"
									>Importance</label
								>
								<select
									id="need-importance"
									class="need-select w-full"
									bind:value={needImportance}
									onchange={handleNeedUpdate}
								>
									<option value="">Not specified</option>
									<option value="low">Low</option>
									<option value="medium">Medium</option>
									<option value="high">High</option>
									<option value="critical">Critical</option>
								</select>
							</div>
						</div>
						<div class="mt-4">
							<label class="inline-flex items-center">
								<input
									type="checkbox"
									bind:checked={needRecurring}
									class="mr-2"
									onchange={handleNeedUpdate}
								/>
								<span class="text-sm text-gray-600">This is a recurring need</span>
							</label>
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
	.need-input {
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

	.need-input.auto-size {
		max-width: 200px; /* Prevent inputs from becoming too wide */
	}

	.need-input::placeholder {
		color: #cbd5e1;
	}
	.need-input:focus {
		border-bottom: 1.5px solid #3b82f6;
		box-shadow: 0 1px 0 0 rgba(59, 130, 246, 0.2);
	}

	.need-select {
		font-size: 1rem;
		padding: 6px 8px;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		background: white;
		outline: none;
		transition: all 0.2s ease;
		appearance: none;
		background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='16' height='16' viewBox='0 0 24 24' fill='none' stroke='%23a1a1aa' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpolyline points='6 9 12 15 18 9'%3E%3C/polyline%3E%3C/svg%3E");
		background-repeat: no-repeat;
		background-position: right 6px center;
		background-size: 16px;
		padding-right: 28px;
	}

	.need-select:focus {
		border-color: #3b82f6;
		box-shadow: 0 0 0 1px rgba(59, 130, 246, 0.2);
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
	.need-item {
		display: flex;
		flex-direction: column;
	}

	.need-item.chat-expanded {
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

	/* Need row responsive behavior */
	.need-row {
		gap: 8px; /* Slightly smaller gap for better wrapping */
	}

	/* Ensure buttons don't wrap unnecessarily */
	.need-row button {
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

	.empty-slots {
		background: rgba(243, 244, 246, 0.5);
		border-radius: 6px;
		border: 1px dashed #d1d5db;
	}

	/* Slot controls styling */
	.slots-controls {
		animation: slideDown 0.2s ease-out;
	}

	.slot-control-select {
		padding: 4px 8px;
		border: 1px solid #d1d5db;
		border-radius: 4px;
		font-size: 0.75rem;
		background: white;
		color: #374151;
		cursor: pointer;
		transition: border-color 0.2s ease;
	}

	.slot-control-select:focus {
		outline: none;
		border-color: #3b82f6;
	}

	.slot-control-btn {
		padding: 4px 8px;
		border: 1px solid #d1d5db;
		border-radius: 4px;
		background: white;
		color: #6b7280;
		cursor: pointer;
		font-size: 0.75rem;
		font-weight: 600;
		transition: all 0.2s ease;
		display: flex;
		align-items: center;
		justify-content: center;
		width: 28px;
		height: 28px;
	}

	.slot-control-btn:hover {
		background: #f3f4f6;
		border-color: #9ca3af;
		color: #374151;
	}

	/* Slot category styling */
	.slot-category {
		border: 1px solid #e5e7eb;
		border-radius: 6px;
		background: white;
		overflow: hidden;
	}

	.category-header {
		width: 100%;
		display: flex;
		align-items: center;
		gap: 8px;
		padding: 12px 16px;
		background: #f9fafb;
		border: none;
		cursor: pointer;
		transition: all 0.2s ease;
		font-size: 0.875rem;
		font-weight: 500;
		text-align: left;
		border-bottom: 1px solid #e5e7eb;
	}

	.category-header:hover {
		background: #f3f4f6;
	}

	.category-icon {
		color: #6b7280;
		font-size: 0.75rem;
		transition: transform 0.2s ease;
	}

	.category-title {
		color: #374151;
		flex: 1;
	}

	.category-content {
		padding: 16px;
		background: white;
		animation: slideDown 0.2s ease-out;
	}

	.slot-wrapper {
		margin-bottom: 12px;
	}

	.slot-wrapper:last-child {
		margin-bottom: 0;
	}

	.past-slot {
		opacity: 0.7;
	}

	.slot-badges {
		display: flex;
		gap: 4px;
		flex-wrap: wrap;
	}

	/* Badge styling */
	.recurrence-badge {
		font-family: 'SF Mono', 'Monaco', 'Inconsolata', 'Roboto Mono', monospace;
		font-size: 0.6rem;
		font-weight: 600;
		text-transform: uppercase;
		letter-spacing: 0.05em;
		padding: 2px 6px;
		border-radius: 4px;
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

	/* Newly created slot highlight animation */
	@keyframes highlightPulse {
		0% {
			background-color: rgba(34, 197, 94, 0.2);
			box-shadow: 0 0 0 0 rgba(34, 197, 94, 0.4);
		}
		50% {
			background-color: rgba(34, 197, 94, 0.15);
			box-shadow: 0 0 0 8px rgba(34, 197, 94, 0.1);
		}
		100% {
			background-color: rgba(34, 197, 94, 0.1);
			box-shadow: 0 0 0 0 rgba(34, 197, 94, 0);
		}
	}

	@keyframes highlightFadeOut {
		0% {
			background-color: rgba(34, 197, 94, 0.1);
		}
		100% {
			background-color: transparent;
		}
	}

	/* Apply highlight styling to newly created slots */
	.slot-wrapper.newly-created,
	div[data-slot-id].newly-created {
		animation:
			highlightPulse 2s ease-in-out,
			highlightFadeOut 1s ease-out 2s;
		border: 2px solid rgba(34, 197, 94, 0.3) !important;
		border-radius: 8px;
		transition: all 0.3s ease;
	}

	.slot-wrapper.newly-created :global(.slot-item),
	div[data-slot-id].newly-created :global(.slot-item) {
		background: rgba(240, 253, 244, 0.8) !important;
		border-color: rgba(34, 197, 94, 0.2) !important;
	}
</style>
