<script lang="ts">
	import type { Commitment, AvailabilitySlot } from '$lib/commons/v5/schemas';
	import { CommitmentSchema, AvailabilitySlotSchema } from '$lib/commons/v5/schemas';
	import TagPill from '$lib/components/TagPill.svelte';
	import DropDown from '$lib/components/DropDown.svelte';
	import Chat from '$lib/components/Chat.svelte';
	import { emojiPicker } from '$lib/actions/emojiPicker';
	import { outsideClick } from '$lib/actions/outsideClick';

	import Slot from './Slot.svelte';
	import { Rules } from '$lib/utils/filters';
	import { onMount } from 'svelte';
	import { browser } from '$app/environment';
	import { createSubtreesDataProvider } from '$lib/utils/ui-providers.svelte';
	import { globalState } from '$lib/global.svelte';
	import { getReactiveUnreadCount } from '$lib/state/chat.svelte';
	import { t } from '$lib/translations';
	// V5: Import from auth.svelte (already v5/Holster-only)
	import { userPub } from '$lib/state/auth.svelte';
	
	// V5 Pure Types - No Backward Compatibility
	type CommitmentWithId = Commitment & { id: string };

	interface Props {
		capacity: CommitmentWithId;
		canDelete: boolean;
		onupdate?: (capacity: CommitmentWithId) => void;
		ondelete?: (id: string) => void;
	}

	let { capacity, canDelete, onupdate, ondelete }: Props = $props();

	// UI state for expanded chat
	let chatExpanded = $state(false);

	// Get reactive unread message count for this capacity's chat
	let unreadCount = getReactiveUnreadCount(capacity.id);

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

	// Create subtrees data provider for the dropdown
	let subtreesDataProvider = createSubtreesDataProvider();
	
	// V5 Structure: Metadata is per-slot, not per-commitment
	// For display purposes, we'll use the first slot's metadata as the "commitment name"
	// This is a UI convenience - in v5, each slot is independent
	const firstSlot = $derived(capacity.capacity_slots?.[0]);
	const displayName = $derived(firstSlot?.name || 'Unnamed');
	const displayEmoji = $derived(firstSlot?.emoji || '');
	
	// Slots accessor (v5 uses capacity_slots not availability_slots)
	const slots = $derived(capacity.capacity_slots || []);

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

	// Note: Recipient shares are no longer displayed as bars in the new efficient allocation system
	// Recipients can see their allocations directly in the shares view

	// V5: Commitment updates are passed through
	// In v5, metadata (name, emoji, etc.) is on individual slots, not commitment
	// Slot updates are handled by the Slot component
	function handleCommitmentUpdate(updatedCommitment: CommitmentWithId) {
		// Validate using schema
		const validationResult = CommitmentSchema.safeParse(updatedCommitment);

		if (!validationResult.success) {
			globalState.showToast('Invalid commitment data', 'error');
			console.error('[CAPACITY] Commitment validation failed:', validationResult.error);
			return;
		}

		// Validation passed, proceed with update
		onupdate?.(validationResult.data as CommitmentWithId);
	}
	
	// Handler when a slot is updated from the Slot component
	function handleSlotUpdate(updatedSlot: AvailabilitySlot) {
		// Update the commitment with the modified slot
		const updatedSlots = (capacity.capacity_slots || []).map(slot =>
			slot.id === updatedSlot.id ? updatedSlot : slot
		);
		
		const updatedCommitment: CommitmentWithId = {
			...capacity,
			capacity_slots: updatedSlots,
			timestamp: Date.now()
		};
		
		handleCommitmentUpdate(updatedCommitment);
	}

	// Delete this capacity
	function handleDelete() {
		ondelete?.(capacity.id);
	}

	// Toggle chat state
	function toggleChat() {
		chatExpanded = !chatExpanded;
		// If we're expanding chat, close slots
		if (chatExpanded) {
			slotsExpanded = false;
		}
	}

	// Toggle slots state
	function toggleSlots() {
		slotsExpanded = !slotsExpanded;
		// If we're expanding slots, close chat
		if (slotsExpanded) {
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

		// Note: Filter rule updates happen automatically via $derived filterRule
		// No need to manually update commitment here

		// Close dropdown
		showSubtreeDropdown = false;
	}

	// Handle removing a subtree filter
	function handleRemoveSubtree(subtreeId: string) {
		selectedSubtrees = selectedSubtrees.filter((id) => id !== subtreeId);
		
		// Note: Filter rule updates happen automatically via $derived filterRule
		// No need to manually update commitment here
	}

	// Close dropdown
	function handleDropdownClose() {
		showSubtreeDropdown = false;
	}

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
	// V5: Use capacity_slots instead of availability_slots
	let categorizedSlots = $derived(() => {
		if (!capacity.capacity_slots || !Array.isArray(capacity.capacity_slots)) {
			return { past: [], recurring: [], currentFuture: [] };
		}

		const past: any[] = [];
		const recurring: any[] = [];
		const currentFuture: any[] = [];

		// Categorize slots
		capacity.capacity_slots.forEach((slot) => {
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

	// Handle slot deletion
	function handleSlotDelete(slotId: string) {
		const updatedSlots = (capacity.capacity_slots || []).filter((slot) => slot.id !== slotId);

		const updatedCommitment: CommitmentWithId = {
			...capacity,
			capacity_slots: updatedSlots,
			timestamp: Date.now()
		};

		handleCommitmentUpdate(updatedCommitment);
	}

	// Add new slot (v5)
	function handleAddSlot() {
		const todayString = new Date().toISOString().split('T')[0]; // Get YYYY-MM-DD format
		const newSlotId = `slot-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
		
		// ✅ Determine capacity_group_id to keep slots grouped together
		// If this capacity already has slots, use their group ID
		// Otherwise, create a new group ID for this capacity
		const existingSlots = capacity.capacity_slots || [];
		const capacityGroupId = existingSlots.length > 0 
			? (existingSlots[0] as any).capacity_group_id || existingSlots[0].id
			: `group-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
		
		// ✅ CRITICAL: Normalize existing slots to fix old invalid enum values
		// AND ensure all slots have the same capacity_group_id for proper grouping
		const normalizedExistingSlots = existingSlots.map(slot => {
			const normalized = { ...slot } as any;
			
			// ✅ CRITICAL: Ensure all existing slots have the same capacity_group_id
			// This fixes the issue where old slots don't have a group ID and new ones do
			if (!normalized.capacity_group_id) {
				normalized.capacity_group_id = capacityGroupId;
				console.log(`[CAPACITY] Added group ID "${capacityGroupId}" to slot "${slot.id}"`);
			}
			
			// Normalize recurrence enum
			if (typeof slot.recurrence === 'string') {
				const lower = slot.recurrence.toLowerCase();
				const validValues = ['daily', 'weekly', 'monthly', 'yearly'];
				
				if (validValues.includes(lower)) {
					normalized.recurrence = lower as any;
				} else {
					// Map legacy values to valid schema values
					const legacyMap: Record<string, string> = {
						'bi-weekly': 'weekly',
						'biweekly': 'weekly',
						'weekends': 'weekly',
						'weekdays': 'weekly',
					};
					normalized.recurrence = (legacyMap[lower] || 'weekly') as any;
					console.log(`[CAPACITY] Normalized recurrence from "${slot.recurrence}" to "${normalized.recurrence}"`);
				}
			}
			
			return normalized;
		});
		
		// V5: All fields required, including need_type_id
		const newSlot: AvailabilitySlot & { capacity_group_id?: string } = {
			id: newSlotId,
			quantity: 1,
			need_type_id: 'need_type_general', // Default need type
			name: '',
			location_type: 'Undefined',
			start_date: todayString,
			end_date: null,
			time_zone: Intl.DateTimeFormat().resolvedOptions().timeZone,
			recurrence: 'daily',
			capacity_group_id: capacityGroupId // ✅ Add group ID for virtual grouping
		};

		const updatedSlots = [...normalizedExistingSlots, newSlot];

		const updatedCommitment: CommitmentWithId = {
			...capacity,
			capacity_slots: updatedSlots,
			timestamp: Date.now()
		};

		// Ensure slots section is expanded so user can see the new slot
		if (!slotsExpanded) {
			slotsExpanded = true;
		}

		// Ensure the current/future slots section is expanded (where new slots appear)
		currentFutureSlotsExpanded = true;

		// Add to highlighted slots using global state
		globalState.highlightSlot(newSlotId);

		handleCommitmentUpdate(updatedCommitment);
	}
</script>

<div class="capacity-item" class:chat-expanded={chatExpanded} data-capacity-id={capacity.id}>
	<!-- Note: Recipient shares bars removed - recipients see allocations in shares view via efficient algorithm -->

	<!-- V5: Clean, Slot-Centric Design -->
	<div class="capacity-row flex flex-wrap items-center gap-2 rounded bg-white p-3 shadow-sm">
		<!-- Commitment Display (Read-Only) -->
		<div class="commitment-display flex flex-1 items-center gap-2">
			<!-- Display first slot's emoji or default -->
			<span class="text-2xl" title="First slot emoji">
				{displayEmoji || '📦'}
			</span>
			
			<!-- Display first slot's name or placeholder -->
			<div class="flex flex-col">
				<span class="text-base font-medium text-gray-800">
					{displayName || 'Unnamed Commitment'}
				</span>
				<span class="text-xs text-gray-500">
					{slots.length} {slots.length === 1 ? 'slot' : 'slots'}
				</span>
			</div>
		</div>

		<!-- Action Buttons -->
		<button
			type="button"
			class="chat-btn relative ml-1"
			onclick={toggleChat}
			title={$t('inventory.chat_about_capacity')}
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
			title={$t('inventory.manage_slots')}
		>
			🕒
		</button>
		<button 
			type="button" 
			class="remove-btn ml-1" 
			onclick={handleDelete} 
			disabled={!canDelete}
			title={$t('inventory.delete_capacity')}
		>
			✖️
		</button>
	</div>

	<!-- Filter tags section (always visible, like contributors in Child.svelte) -->
	<div class="filter-section mt-2">
		<div class="filter-layout">
			<!-- Add filter button -->
			<div class="filter-button-container">
				<button type="button" class="add-filter-btn" onclick={handleAddSubtreeFilter}>
					<span class="add-icon">+</span>
					<span class="add-text">{$t('common.filter')}</span>
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
					{$t('inventory.filter_description', { count: selectedSubtrees.length } as any)}
				</span>
			</div>
		{/if}
	</div>

	<!-- Expanded chat section -->
	{#if chatExpanded}
		<div class="chat-container rounded border border-gray-200 bg-gray-50 p-3">
			<div class="chat-header mb-2">
				<h4 class="text-sm font-medium text-gray-700">
					💬 {$t('inventory.chat_about_capacity')} {displayEmoji || '📦'} {displayName}
				</h4>
				<p class="mt-1 text-xs text-gray-500">
					{$t('inventory.discuss_capacity')}
				</p>
			</div>
			<Chat chatId={capacity.id} placeholder={`${$t('inventory.discuss')} ${displayName}...`} maxLength={200} />
		</div>
	{/if}

	<!-- Expanded slots section -->
	{#if slotsExpanded}
		<div class="slots-section mb-4 rounded border border-gray-200 bg-purple-50 p-3">
			<div class="slots-header mb-3">
				<h4 class="text-sm font-medium text-gray-700">🕒 {$t('inventory.availability_slots')}</h4>
				<p class="mt-1 text-xs text-gray-500">
					{$t('inventory.manage_slots_description', { count: totalSlotCount() } as any)}
				</p>
			</div>

			<!-- Add slot button -->
			<button type="button" class="add-slot-btn mb-4" onclick={handleAddSlot}>
				<span class="add-icon">+</span>
				<span class="add-text">{$t('inventory.add_slot')}</span>
			</button>

			<!-- Slot controls -->
			<div class="slots-controls mb-4 rounded border border-gray-200 bg-white p-3">
				<div class="flex flex-wrap items-center gap-2">
					<span class="text-xs font-medium text-gray-600">{$t('common.sort_by')}:</span>
					<select class="slot-control-select" bind:value={slotSortBy}>
						<option value="time">{$t('inventory.time')}</option>
						<option value="location">{$t('inventory.location')}</option>
						<option value="quantity">{$t('inventory.quantity')}</option>
					</select>
					<button
						class="slot-control-btn"
						onclick={() => (slotSortDirection = slotSortDirection === 'asc' ? 'desc' : 'asc')}
						title={$t('common.toggle_sort')}
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
									>🔄 {$t('inventory.recurring_availability')} ({categorizedSlots().recurring.length})</span
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
											{#key JSON.stringify(slot)}
												<Slot
													{slot}
													capacityId={capacity.id}
													canDelete={slots.length > 1}
													onupdate={handleSlotUpdate}
													ondelete={handleSlotDelete}
												/>
											{/key}
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
									>📅 {$t('inventory.current_upcoming')} ({categorizedSlots().currentFuture
										.length})</span
								>
							</button>
							{#if currentFutureSlotsExpanded}
								<div class="category-content">
									{#each categorizedSlots().currentFuture as slot (slot.id)}
										<div
											data-slot-id={slot.id}
											class:newly-created={globalState.highlightedSlots.has(slot.id)}
										>
											{#key JSON.stringify(slot)}
												<Slot
													{slot}
													capacityId={capacity.id}
													canDelete={slots.length > 1}
													onupdate={handleSlotUpdate}
													ondelete={handleSlotDelete}
												/>
											{/key}
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
								<span class="category-title"
									>📜 {$t('inventory.past_availability')} ({categorizedSlots().past.length})</span
								>
							</button>
							{#if pastSlotsExpanded}
								<div class="category-content">
									{#each categorizedSlots().past as slot (slot.id)}
										<div
											class="slot-wrapper past-slot"
											data-slot-id={slot.id}
											class:newly-created={globalState.highlightedSlots.has(slot.id)}
										>
											{#key JSON.stringify(slot)}
												<Slot
													{slot}
													capacityId={capacity.id}
													canDelete={slots.length > 1}
													onupdate={handleSlotUpdate}
													ondelete={handleSlotDelete}
												/>
											{/key}
										</div>
									{/each}
								</div>
							{/if}
						</div>
					{/if}
				{:else}
					<div class="empty-slots py-6 text-center text-xs text-gray-500 italic">
						{$t('inventory.no_slots')}
					</div>
				{/if}
			</div>
		</div>
	{/if}

</div>

<!-- Subtree dropdown for adding filters -->
{#if showSubtreeDropdown}
	<DropDown
		position={dropdownPosition}
		show={showSubtreeDropdown}
		title={$t('inventory.select_subtree')}
		searchPlaceholder={$t('common.search_placeholder')}
		dataProvider={subtreesDataProvider}
		select={handleSubtreeSelect}
		updatePosition={(newPosition) => (dropdownPosition = newPosition)}
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

	/* Member Management Styles */
	.member-management-section {
		border-top: 1px solid #e5e7eb;
		padding-top: 1.5rem;
	}

	.add-member-btn {
		display: inline-flex;
		align-items: center;
		gap: 4px;
		padding: 3px 10px;
		border: 1px dashed #d1d5db;
		border-radius: 12px;
		background: rgba(249, 250, 251, 0.8);
		color: #6b7280;
		font-size: 12px;
		cursor: pointer;
		transition: all 0.2s ease;
		height: 22px;
		line-height: 1;
	}

	.add-member-btn:hover {
		border-color: #3b82f6;
		color: #3b82f6;
		background: rgba(248, 250, 252, 0.9);
		transform: scale(1.02);
	}

	.add-member-btn .add-icon {
		font-size: 0.875rem;
		font-weight: bold;
		line-height: 1;
	}

	.add-member-btn .add-text {
		font-weight: 500;
	}

	.members-list {
		padding: 1rem;
		background: rgba(249, 250, 251, 0.5);
		border-radius: 6px;
		border: 1px solid rgba(229, 231, 235, 0.6);
	}

	.auto-update-section {
		animation: slideDown 0.2s ease-out;
	}

	.auto-update-config {
		animation: slideDown 0.2s ease-out;
	}

	.recompute-btn {
		display: flex;
		align-items: center;
		justify-content: center;
		gap: 6px;
		padding: 10px 16px;
		border: 2px solid #3b82f6;
		border-radius: 6px;
		background: white;
		color: #3b82f6;
		font-size: 0.875rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s ease;
	}

	.recompute-btn:hover:not(:disabled) {
		background: #3b82f6;
		color: white;
		transform: scale(1.02);
	}

	.recompute-btn:disabled {
		opacity: 0.6;
		cursor: not-allowed;
		border-color: #9ca3af;
		color: #9ca3af;
	}
</style>
