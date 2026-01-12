<script lang="ts">
	import type { NeedSlot, AvailabilitySlot, AvailabilityWindow, SlotAllocationRecord } from '$lib/protocol/schemas';
	import { TimePatternEditor, LocationEditor, DivisibilityEditor, type LocationData } from './slots';
	import SlotPriorityDistributionEditor from './slots/form/SlotPriorityDistributionEditor.svelte';
	import { types, type NeedType } from '$lib/protocol/needTypes-local';
	import { myAllocationsAsProvider } from '$lib/protocol/stores/allocation.svelte';
	import { networkAllocations } from '$lib/protocol/stores/stores.svelte';
	import { holsterUserPub } from '$lib/network/holster.svelte';
	import { getUserName } from '$lib/network/users.svelte';
	import Chat from '$lib/components/Chat.svelte';
	import { getReactiveUnreadCount } from '$lib/chat/chat.svelte';
	import { outsideClick } from '$lib/actions/outsideClick';
	import { emojiPicker } from '$lib/actions/emojiPicker';
	
	/**
	 * Generalized resource slots component with need type selection
	 * Default time: per month
	 * Simple fields: quantity + time pattern only
	 */
	
	type SlotType = NeedSlot | AvailabilitySlot;
	
	interface Props {
		needSlots: NeedSlot[];
		capacitySlots: AvailabilitySlot[];
		onNeedUpdate: (slot: NeedSlot) => void;
		onNeedDelete: (id: string) => void;
		onCapacityUpdate: (slot: AvailabilitySlot) => void;
		onCapacityDelete: (id: string) => void;
		onNeedAdd: (name: string, quantity: number, needTypeId: string, emoji: string) => void;
		onCapacityAdd: (name: string, quantity: number, needTypeId: string, emoji: string) => void;
	}
	
	let {
		needSlots, 
		capacitySlots,
		onNeedUpdate,
		onNeedDelete,
		onCapacityUpdate,
		onCapacityDelete,
		onNeedAdd,
		onCapacityAdd
	}: Props = $props();
	
	$effect(() => {
		console.log('[ResourceSlots] Updated props:', { 
			needSlotsCount: needSlots.length, 
			capacitySlotsCount: capacitySlots.length,
			activeTab,
			selectedNeedType,
			currentSlotsCount: (activeTab === 'needs' ? needSlots : capacitySlots)
				.filter(slot => slot.type_id === selectedNeedType).length
		});
	});
	
	// Selected need type
	let selectedNeedType = $state<string>('general'); // Default to general (matching demo data)
	
	// Tab state: 'needs' or 'capacity'
	let activeTab = $state<'needs' | 'capacity'>('needs');
	
	// Add form state
	// Draft Slot State (Rich object)
	let draftSlot = $state({
		name: '',
		quantity: 100,
		emoji: '📦',
		time_pattern: {
			type: 'monthly' as const,
			days: [],
			start_time: '09:00',
			end_time: '17:00',
			timezone: Intl.DateTimeFormat().resolvedOptions().timeZone
		},
		location: {
			type: 'any' as const,
			latitude: 0,
			longitude: 0
		}
	});

	let showEmojiPicker = $state(false);

	// Editor state for the Draft slot
	let draftExpanded = $state<EditorType | null>(null);

	function toggleDraftEditor(type: EditorType) {
		if (draftExpanded === type) {
			draftExpanded = null;
		} else {
			draftExpanded = type;
		}
	}
	
	// Single unified state for tab switching - elegant solution!
	type EditorType = 'time' | 'priority' | 'allocations' | 'chat' | 'location' | 'divisibility';
	let expandedEditor = $state<Map<string, EditorType>>(new Map());

	
	// Delete confirmation state
	let deletePending = $state<string | null>(null);
	
	// Get current need type info
	const currentNeedType = $derived(types.find(t => t.id === selectedNeedType) || types[0]);
	
	// Get current slots based on active tab AND selected need type
	const currentSlots = $derived(
		(activeTab === 'needs' ? needSlots : capacitySlots)
			.filter(slot => slot.type_id === selectedNeedType)
	);
	const isNeedMode = $derived(activeTab === 'needs');
	
	// Reactive allocation data - derived from stores
	const myAllocations = $derived($myAllocationsAsProvider.allocations || []);
	const allNetworkAllocations = $derived($networkAllocations);
	const myPubKey = $derived($holsterUserPub);
	
	// Get allocations map for all slots (reactive)
	const capacityAllocationsMap = $derived.by(() => {
		const map = new Map<string, SlotAllocationRecord[]>();
		for (const allocation of myAllocations) {
			const slotId = allocation.availability_slot_id;
			if (!map.has(slotId)) {
				map.set(slotId, []);
			}
			map.get(slotId)!.push(allocation);
		}
		return map;
	});
	
	const needAllocationsMap = $derived.by(() => {
		const map = new Map<string, SlotAllocationRecord[]>();
		if (!myPubKey) return map;
		
        let networkAllocCount = 0;
        let selfAllocFound = 0;

		for (const [providerPubKey, allocations] of allNetworkAllocations) {
			if (allocations && Array.isArray(allocations)) {
				for (const allocation of allocations) {
                    networkAllocCount++;
					if (allocation.recipient_pubkey === myPubKey && allocation.recipient_need_slot_id) {
						const slotId = allocation.recipient_need_slot_id;
						if (!map.has(slotId)) {
							map.set(slotId, []);
						}
						map.get(slotId)!.push(allocation);
                        if (providerPubKey === myPubKey) selfAllocFound++;
					}
				}
			}
		}

        // Merge my own allocations (as provider) to myself (as recipient)
        // These are not always in networkAllocations depending on how the stores are setup
        for (const allocation of myAllocations) {
            // Check if I am the recipient (Self-Allocation)
            if (allocation.recipient_pubkey === myPubKey && allocation.recipient_need_slot_id) {
                const slotId = allocation.recipient_need_slot_id;
                
                if (!map.has(slotId)) {
                    map.set(slotId, []);
                }
                
                // Avoid duplicates if I am somehow in networkAllocations
                const existing = map.get(slotId)!;
                if (!existing.some(a => a.availability_slot_id === allocation.availability_slot_id && a.provider_pubkey === allocation.provider_pubkey)) {
                     existing.push(allocation);
                     selfAllocFound++;
                }
            }
        }

        console.log(`[UI-NEEDS] Mapped ${networkAllocCount} network allocations to needs. found ${selfAllocFound} from myself.`);
        console.log(`[UI-NEEDS] myAllocations (local) has:`, myAllocations.filter(a => a.recipient_pubkey === myPubKey));

		return map;
	});
	
	// Add slot handler
	// Add slot handler
	function handleAddSlot() {
		if (!draftSlot.name.trim()) return;
		
		// TODO: Pass full draftSlot object when API supports it
		if (isNeedMode) {
			onNeedAdd(draftSlot.name, draftSlot.quantity, selectedNeedType, draftSlot.emoji);
		} else {
			onCapacityAdd(draftSlot.name, draftSlot.quantity, selectedNeedType, draftSlot.emoji);
		}
		
		// Reset form
		draftSlot.name = '';
		draftSlot.quantity = 100;
		draftSlot.emoji = '📦';
		// Reset other fields if desirable, or keep them as "defaults"
		draftExpanded = null;
	}
	
	
	// Universal toggle function - elegant single solution!
	function toggleEditor(id: string, editorType: EditorType) {
		const current = expandedEditor.get(id);
		const newMap = new Map(expandedEditor);
		
		if (current === editorType) {
			// Close if same editor clicked
			newMap.delete(id);
		} else {
			// Open new editor (automatically closes any other)
			newMap.set(id, editorType);
		}
		
		expandedEditor = newMap;
		deletePending = null;
	}





	
	// Handle delete with confirmation
	function handleDelete(id: string, isNeed: boolean) {
		if (deletePending === id) {
			// Confirm deletion
			isNeed ? onNeedDelete(id) : onCapacityDelete(id);
			deletePending = null;
		} else {
			// First click - show confirmation
			deletePending = id;
		}
	}

    function handlePriorityUpdate(slot: SlotType, priority_distribution: Record<string, number>, isNeed: boolean) {
        const updatedSlot = { ...slot, priority_distribution };
        if (isNeed) {
            onNeedUpdate(updatedSlot as NeedSlot);
        } else {
            onCapacityUpdate(updatedSlot as AvailabilitySlot);
        }
    }
	
	// Generic handlers (DRY)
	function handleQuantityChange(slot: SlotType, quantity: number, isNeed: boolean) {
		const updated = { ...slot, quantity };
		isNeed ? onNeedUpdate(updated as NeedSlot) : onCapacityUpdate(updated as AvailabilitySlot);
	}
	
	function handleTimePatternUpdate(
		slot: SlotType,
		recurrence: string | null,
		availabilityWindow: AvailabilityWindow | undefined,
		isNeed: boolean
	) {
		const updated = {
			...slot,
			recurrence: recurrence as any,
			availability_window: availabilityWindow as any
		};
		isNeed ? onNeedUpdate(updated as NeedSlot) : onCapacityUpdate(updated as AvailabilitySlot);
	}
	
	// Location handler
	function handleLocationUpdate(slot: SlotType, location: LocationData, isNeed: boolean) {
		const updated = { ...slot, ...location };
		isNeed ? onNeedUpdate(updated as NeedSlot) : onCapacityUpdate(updated as AvailabilitySlot);
	}
	
	// Divisibility handler
	function handleDivisibilityUpdate(slot: SlotType, maxNaturalDiv?: number, minAllocationPercentage?: number, isNeed: boolean = false) {
		const updated = {
			...slot,
			max_natural_div: maxNaturalDiv,
			min_allocation_percentage: minAllocationPercentage
		};
		isNeed ? onNeedUpdate(updated as NeedSlot) : onCapacityUpdate(updated as AvailabilitySlot);
	}
	
	// Display formatter for time patterns (matches Slot.svelte format)
	function formatTimeDisplay(slot: SlotType): string {
		if (!slot.recurrence && !slot.start_date) return 'Not specified';
		
		let parts: string[] = [];
		
		if (slot.recurrence) {
			parts.push(slot.recurrence);
		}
		
		if (slot.start_date) {
			const date = new Date(slot.start_date);
			parts.push(date.toLocaleDateString());
		}
		
		// Check for time ranges in availability window
		if (slot.availability_window?.time_ranges?.[0]) {
			const range = slot.availability_window.time_ranges[0];
			parts.push(`${range.start_time}-${range.end_time}`);
		} else if (slot.recurrence) {
			parts.push('All day');
		}
		
		return parts.join(', ');
	}
	
	// Display formatter for location
	function formatLocationDisplay(slot: SlotType): string {
		if (!slot.location_type || slot.location_type === 'Undefined') {
			return 'Not specified';
		}
		
		if (slot.location_type === 'Online') {
			return slot.online_link ? 'Online' : 'Online (no link)';
		}
		
		if (slot.location_type === 'Specific' && slot.city) {
			return slot.city;
		}
		
		if (slot.location_type === 'Coordinates' && slot.latitude && slot.longitude) {
			return `${slot.latitude.toFixed(2)}, ${slot.longitude.toFixed(2)}`;
		}
		
		return slot.location_type;
	}
	
	// Display formatter for divisibility
	function formatDivisibilityDisplay(slot: SlotType): string {
		const parts: string[] = [];
		
		if (slot.max_natural_div) {
			parts.push(`Max ${slot.max_natural_div}`);
		}
		
		if (slot.min_allocation_percentage) {
			parts.push(`Min ${Math.round(slot.min_allocation_percentage * 100)}%`);
		}
		
		return parts.length > 0 ? parts.join(', ') : 'None';
	}

	// Status helpers
	function getStatusLabel(withinLimit: boolean): string {
		return withinLimit ? 'priority-flow' : 'surplus-flow';
	}

	function getStatusEmoji(withinLimit: boolean): string {
		return withinLimit ? '⚡' : '🌊';
	}
	
	function getStatusDisplayName(withinLimit: boolean): string {
		return withinLimit ? 'Priority Flow' : 'Surplus Flow';
	}
</script>

{#snippet slotCard(slot: SlotType)}
	{@const rawAllocations = isNeedMode 
		? (needAllocationsMap.get(slot.id) || [])
		: (capacityAllocationsMap.get(slot.id) || [])
	}
	<!-- Sort allocations by priority (within limit first) then by amount (desc) -->
	{@const allocations = [...rawAllocations].sort((a, b) => {
		const pA = a.withinPriorityLimit ? 0 : 1;
		const pB = b.withinPriorityLimit ? 0 : 1;
		if (pA !== pB) return pA - pB;
		return b.quantity - a.quantity;
	})}
	
	{@const totalAllocated = allocations.reduce((sum, a) => sum + a.quantity, 0)}
	{@const percentFilled = slot.quantity > 0 ? Math.min((totalAllocated / slot.quantity) * 100, 100) : 0}
	
	<div class="slot-card {isNeedMode ? 'need-card' : 'capacity-card'}">
		<div class="slot-fill-indicator" style="width: {percentFilled}%"></div>
		<!-- Compact single-row layout -->
		<div class="slot-main">
			<!-- Name input -->
			{#if selectedNeedType === 'money'}
				<span class="currency-symbol">$</span>
			{:else}
				<span class="type-emoji">{slot.emoji || currentNeedType.emoji}</span>
			{/if}
			<input
				type="text"
				class="slot-name-input"
				value={slot.name}
				oninput={(e) => {
					const updated = { ...slot, name: (e.target as HTMLInputElement).value };
					isNeedMode ? onNeedUpdate(updated as NeedSlot) : onCapacityUpdate(updated as AvailabilitySlot);
				}}
				onclick={(e) => e.stopPropagation()}
				placeholder="Name"
			/>
			
			<!-- Quantity with currency/emoji -->
			<input
				type="number"
				class="slot-qty-input"
				value={slot.quantity}
				min="0"
				step="1"
				oninput={(e) => handleQuantityChange(slot, parseFloat((e.target as HTMLInputElement).value), isNeedMode)}
				onclick={(e) => e.stopPropagation()}
			/>
			
			<!-- Unit input -->
			<input
				type="text"
				class="slot-unit-input"
				value={slot.unit || ''}
				oninput={(e) => {
					const updated = { ...slot, unit: (e.target as HTMLInputElement).value };
					isNeedMode ? onNeedUpdate(updated as NeedSlot) : onCapacityUpdate(updated as AvailabilitySlot);
				}}
				onclick={(e) => e.stopPropagation()}
				placeholder="unit"
			/>
			
			<!-- Compact buttons -->
			<button
				type="button"
				class="slot-btn"
				onclick={(e) => { e.stopPropagation(); toggleEditor(slot.id, 'time'); }}
				title="Edit time pattern"
			>
				🕐 {formatTimeDisplay(slot)}
			</button>

			<button
				type="button"
				class="slot-btn"
				onclick={(e) => { e.stopPropagation(); toggleEditor(slot.id, 'location'); }}
				title="Edit location"
			>
				📍 {formatLocationDisplay(slot)}
			</button>

			<button
				type="button"
				class="slot-btn"
				onclick={(e) => { e.stopPropagation(); toggleEditor(slot.id, 'chat'); }}
				title="Chat about this slot"
			>
				💬
			</button>

			<button
				type="button"
				class="slot-btn"
				onclick={(e) => { e.stopPropagation(); toggleEditor(slot.id, 'divisibility'); }}
				title="Edit divisibility"
			>
				⚙️ {formatDivisibilityDisplay(slot)}
			</button>
			

            {#if !isNeedMode}
            <button
				type="button"
				class="slot-btn"
				onclick={(e) => { e.stopPropagation(); toggleEditor(slot.id, 'priority'); }}
                title="Edit Priority Distribution"
			>
				⭐
			</button>
            {/if}
			
			<button
				type="button"
				class="slot-btn-delete"
				onclick={(e) => { e.stopPropagation(); handleDelete(slot.id, isNeedMode); }}
				title={deletePending === slot.id ? "Click again to confirm" : "Delete"}
			>
				{deletePending === slot.id ? 'Confirm?' : '🗑️'}
			</button>
			
			<!-- Allocation indicator (clickable) -->
			{#if allocations.length > 0}
				<button
					type="button"
					class="slot-btn-alloc"
					onclick={(e) => { e.stopPropagation(); toggleEditor(slot.id, 'allocations'); }}
					title="View allocations"
				>
					{allocations.length}
				</button>
			{/if}
		</div>
		
		{#if expandedEditor.get(slot.id) === 'time'}
			<!-- svelte-ignore a11y_no_noninteractive_element_interactions -->
			<!-- svelte-ignore a11y_click_events_have_key_events -->
			<div 
				class="time-editor-section" 
				role="region"
				onclick={(e) => e.stopPropagation()}
			>
				<TimePatternEditor
					recurrence={slot.recurrence || null}
					availabilityWindow={slot.availability_window}
					onUpdate={(recurrence: string | null, availabilityWindow?: any) => 
						handleTimePatternUpdate(slot, recurrence, availabilityWindow, isNeedMode)
					}
				/>
			</div>
		{/if}
		
        {#if expandedEditor.get(slot.id) === 'priority' && !isNeedMode}
			<!-- svelte-ignore a11y_no_noninteractive_element_interactions -->
			<!-- svelte-ignore a11y_click_events_have_key_events -->
			<div 
				class="time-editor-section" 
				role="region"
				onclick={(e) => e.stopPropagation()}
			>
                <div class="priority-header">
                    <h4>Person-to-Person Priorities</h4>
                    <p class="help-text">Override global recognition for this slot.</p>
                </div>
				<SlotPriorityDistributionEditor
					priorityDistribution={(slot as AvailabilitySlot).priority_distribution}
                    onUpdate={(dist) => handlePriorityUpdate(slot, dist, isNeedMode)}
				/>
			</div>
		{/if}

		{#if expandedEditor.get(slot.id) === 'chat'}
			<!-- svelte-ignore a11y_no_noninteractive_element_interactions -->
			<!-- svelte-ignore a11y_click_events_have_key_events -->
			<div 
				class="chat-section" 
				role="region"
				onclick={(e) => e.stopPropagation()}
			>
				<div class="chat-header">
					<h4>💬 Chat about {slot.name}</h4>
					<p class="help-text">Discuss this {isNeedMode ? 'need' : 'capacity'} slot with others</p>
				</div>
				<Chat 
					chatId={slot.id} 
					placeholder={`Discuss ${slot.name}...`} 
					maxLength={200} 
				/>
			</div>
		{/if}

		{#if expandedEditor.get(slot.id) === 'location'}
			<!-- svelte-ignore a11y_no_noninteractive_element_interactions -->
			<!-- svelte-ignore a11y_click_events_have_key_events -->
			<div 
				class="editor-section" 
				role="region"
				onclick={(e) => e.stopPropagation()}
			>
				<LocationEditor
					locationType={slot.location_type}
					streetAddress={slot.street_address}
					city={slot.city}
					stateProvince={slot.state_province}
					postalCode={slot.postal_code}
					country={slot.country}
					latitude={slot.latitude}
					longitude={slot.longitude}
					onlineLink={slot.online_link}
					onUpdate={(location) => handleLocationUpdate(slot, location, isNeedMode)}
				/>
			</div>
		{/if}

		{#if expandedEditor.get(slot.id) === 'divisibility'}
			<!-- svelte-ignore a11y_no_noninteractive_element_interactions -->
			<!-- svelte-ignore a11y_click_events_have_key_events -->
			<div 
				class="editor-section" 
				role="region"
				onclick={(e) => e.stopPropagation()}
			>
				<div class="editor-header">
					<h4>⚙️ Divisibility</h4>
					<p class="help-text">Control how this slot can be divided among allocations</p>
				</div>
				<DivisibilityEditor
					maxNaturalDiv={slot.max_natural_div}
					minAllocationPercentage={slot.min_allocation_percentage}
					onUpdate={(maxDiv, minPct) => handleDivisibilityUpdate(slot, maxDiv, minPct, isNeedMode)}
				/>
			</div>
		{/if}

		{#if expandedEditor.get(slot.id) === 'allocations'}
			<!-- svelte-ignore a11y_no_noninteractive_element_interactions -->
			<!-- svelte-ignore a11y_click_events_have_key_events -->
			<div 
				class="allocations-section" 
				role="region"
				onclick={(e) => e.stopPropagation()}
			>
				<h4 class="allocations-title">
					{#if isNeedMode}
						💫 Receiving from {allocations.length} provider{allocations.length !== 1 ? 's' : ''}
					{:else}
						💫 Providing to {allocations.length} recipient{allocations.length !== 1 ? 's' : ''}
					{/if}
					{#if totalAllocated > 0}
						<span class="total-allocated">
							({selectedNeedType === 'money' ? '$' : currentNeedType.emoji}{totalAllocated.toFixed(2)} total)
						</span>
					{/if}
				</h4>
				
				{#if allocations.length === 0}
					<div class="no-allocations">
						{#if isNeedMode}
							No providers yet. Others will see your need and may offer support.
						{:else}
							No recipients yet. Your capacity will automatically allocate to those who recognize you.
						{/if}
					</div>
				{:else}
					<div class="allocations-list">
						{#each allocations as allocation}
							{@const otherPubKey = isNeedMode ? 
								// For needs, find the provider by looking through networkAllocations
								(() => {
									for (const [providerKey, allocList] of $networkAllocations) {
										if (allocList?.some(a => a === allocation)) return providerKey;
									}
									return '';
								})()
								: allocation.recipient_pubkey
							}
							{@const statusLabel = getStatusLabel(allocation.withinPriorityLimit)}
							
							<div class="allocation-item {statusLabel}">
								<div class="allocation-header">
									<span class="user-name">
										{#await getUserName(otherPubKey)}
											{otherPubKey.slice(0, 8)}...
										{:then displayName}
											{displayName}
										{:catch}
											{otherPubKey.slice(0, 8)}...
										{/await}
									</span>
									<span class="allocation-amount">
										{selectedNeedType === 'money' ? '$' : currentNeedType.emoji}{allocation.quantity.toFixed(2)}
									</span>
								</div>
								<div class="allocation-meta">
									<span class="status-badge {statusLabel}">
										{getStatusEmoji(allocation.withinPriorityLimit)} {getStatusDisplayName(allocation.withinPriorityLimit)}
									</span>
									{#if allocation.time_compatible && allocation.location_compatible}
										<span class="compatible">✓ Time & Location match</span>
									{:else if allocation.time_compatible}
										<span class="compatible">✓ Time match</span>
									{:else if allocation.location_compatible}
										<span class="compatible">✓ Location match</span>
									{/if}
								</div>
							</div>
						{/each}
					</div>
				{/if}
			</div>
		{/if}
	</div>
{/snippet}

<div class="slots-container">
	<!-- Tab Navigation -->
	<!-- Draft Slot (Add New) -->
	<!-- Draft Slot (Add New) -->
	<div class="slot-card draft-card {isNeedMode ? 'need-draft' : 'capacity-draft'}">
		<div class="draft-tabs">
			<button 
				class="draft-tab {isNeedMode ? 'active' : ''}"
				onclick={() => activeTab = 'needs'}
			>
				🎯 Needs ({needSlots.filter(s => s.type_id === selectedNeedType).length})
			</button>
			<button 
				class="draft-tab {!isNeedMode ? 'active' : ''}"
				onclick={() => activeTab = 'capacity'}
			>
				🎁 Capacity ({capacitySlots.filter(s => s.type_id === selectedNeedType).length})
			</button>
		</div>

		<div class="slot-main">
			<!-- Compact Type Selector -->
			<select bind:value={selectedNeedType} class="compact-select">
				{#each types as needType}
					<option value={needType.id}>
						{needType.emoji} {needType.label}
					</option>
				{/each}
			</select>

			<!-- Quick Inputs -->
			<input
				type="text"
				bind:value={draftSlot.name}
				placeholder="Add new {isNeedMode ? 'need' : 'capacity'}..."
				onkeydown={(e) => e.key === 'Enter' && handleAddSlot()}
				class="draft-input"
			/>
			
			<div class="draft-amount">
				<div class="relative inline-block">
					<button 
						type="button"
						class="text-xl leading-none px-2 py-1 rounded hover:bg-gray-100 transition-colors"
						onclick={(e) => { e.preventDefault(); e.stopPropagation(); showEmojiPicker = !showEmojiPicker; }}
					>
						{draftSlot.emoji}
					</button>
					{#if showEmojiPicker}
						<div 
							class="absolute bottom-full left-0 mb-2 z-[9999]"
							use:outsideClick={() => showEmojiPicker = false}
							use:emojiPicker={{ onClick: (e) => { draftSlot.emoji = e; showEmojiPicker = false; } }}
							style="min-width: 320px;"
						></div>
					{/if}
				</div>
				<input
					type="number"
					bind:value={draftSlot.quantity}
					min="0"
					step="1"
					class="slot-qty-input"
				/>
			</div>

			<!-- Functional Draft Controls -->
			<button 
				class="slot-btn {draftExpanded === 'time' ? 'active' : ''}" 
				onclick={() => toggleDraftEditor('time')}
				title="Edit time pattern"
			>
				🕐 {draftSlot.time_pattern.type === 'monthly' ? 'Monthly' : 'Custom'}
			</button>
			<button 
				class="slot-btn {draftExpanded === 'location' ? 'active' : ''}" 
				onclick={() => toggleDraftEditor('location')}
				title="Set location"
			>
				📍 {draftSlot.location.type === 'any' ? 'Any' : draftSlot.location.type}
			</button>

			<button onclick={handleAddSlot} class="btn-add {isNeedMode ? 'need-btn' : 'capacity-btn'}">
				➕ Add
			</button>
		</div>

		<!-- Expanded Editors for Draft Slot -->
		{#if draftExpanded === 'time'}
			<div class="editor-section">
				<TimePatternEditor 
					pattern={draftSlot.time_pattern} 
					onUpdate={(p) => { 
						draftSlot.time_pattern = p;
						// Auto-close if needed or keep open
					}} 
				/>
			</div>
		{/if}
		
		{#if draftExpanded === 'location'}
			<div class="editor-section">
				<LocationEditor 
					locationType={draftSlot.location.type}
					latitude={draftSlot.location.latitude}
					longitude={draftSlot.location.longitude}
					onUpdate={(l) => {
						// Map location editor update back to draftSlot format
						draftSlot.location.type = l.type;
						if (l.latitude) draftSlot.location.latitude = l.latitude;
						if (l.longitude) draftSlot.location.longitude = l.longitude;
					}}
				/>
			</div>
		{/if}
	</div>
	
	<!-- Slots List -->
	<div class="slots-list">
		{#if currentSlots.length === 0}
			<div class="empty-state">
				No {activeTab === 'needs' ? 'need' : 'capacity'} slots yet
			</div>
		{:else}
			{#each currentSlots as slot (slot.id)}
				{@render slotCard(slot)}
			{/each}
		{/if}
	</div>
</div>

<style>
	.slots-container {
		display: flex;
		flex-direction: column;
		padding: 0.75rem;
		height: 100%;
		overflow: hidden;
		gap: 0.75rem;
	}
	
	/* Need Type Selector */
	.need-type-selector {
		background: white;
		padding: 0.75rem;
		border-radius: 6px;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.08);
		display: flex;
		align-items: center;
		gap: 0.75rem;
		flex-shrink: 0;
		flex-wrap: wrap;
	}
	
	.need-type-selector label {
		font-size: 0.875rem;
		font-weight: 600;
		color: #374151;
		white-space: nowrap;
	}
	
	.type-select {
		flex: 1;
		min-width: 200px;
		padding: 0.5rem 0.75rem;
		border: 2px solid #e5e7eb;
		border-radius: 6px;
		font-size: 0.875rem;
		font-weight: 500;
		background: white;
		cursor: pointer;
		transition: all 0.15s ease;
	}
	
	.type-select:hover {
		border-color: #3b82f6;
	}
	
	.type-select:focus {
		outline: none;
		border-color: #3b82f6;
		box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
	}
	
	.type-description {
		flex: 1 1 100%;
		font-size: 0.75rem;
		color: #6b7280;
		font-style: italic;
	}
	
	/* Tab Navigation */
	.tabs {
		display: flex;
		gap: 0.5rem;
		flex-shrink: 0;
	}
	
	.tab {
		flex: 1;
		padding: 0.625rem 1rem;
		background: white;
		border: none;
		border-radius: 6px;
		font-size: 0.875rem;
		font-weight: 600;
		color: #6b7280;
		cursor: pointer;
		transition: all 0.2s ease;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.08);
	}
	
	.tab:hover {
		background: #f9fafb;
		transform: translateY(-1px);
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
	}
	
	.tab.active {
		color: white;
		font-weight: 700;
		box-shadow: 0 2px 6px rgba(0, 0, 0, 0.15);
	}
	
	.tab:first-child.active {
		background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
	}
	
	.tab:last-child.active {
		background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
	}
	
	/* Add Form */
	.add-form {
		display: flex;
		gap: 0.5rem;
		background: white;
		padding: 0.75rem;
		border-radius: 6px;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.08);
		flex-shrink: 0;
	}
	
	.input-name {
		flex: 1;
		padding: 0.5rem;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		font-size: 0.875rem;
		transition: all 0.15s ease;
	}
	
	.input-name:focus {
		outline: none;
		border-color: #3b82f6;
		box-shadow: 0 0 0 2px rgba(59, 130, 246, 0.1);
	}
	
	.quantity-input {
		display: flex;
		align-items: center;
		gap: 0.375rem;
		background: #f9fafb;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		padding: 0.375rem 0.5rem;
	}
	
	.currency-symbol,
	.quantity-label {
		font-size: 1rem;
		font-weight: 700;
		color: #10b981;
	}
	
	.quantity-input input {
		width: 70px;
		border: none;
		background: transparent;
		font-size: 0.875rem;
		font-weight: 600;
		padding: 0.125rem;
	}
	
	.quantity-input input:focus {
		outline: none;
	}
	
	.btn-add {
		padding: 0.5rem 1rem;
		border: none;
		border-radius: 4px;
		font-size: 0.8rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.15s ease;
		color: white;
		white-space: nowrap;
	}
	
	.need-btn {
		background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
	}
	
	.need-btn:hover {
		transform: translateY(-1px);
		box-shadow: 0 2px 8px rgba(245, 87, 108, 0.3);
	}
	
	.capacity-btn {
		background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
	}
	
	.capacity-btn:hover {
		transform: translateY(-1px);
		box-shadow: 0 2px 8px rgba(0, 242, 254, 0.3);
	}
	
	/* Slots List */
	.slots-list {
		display: flex;
		flex-wrap: wrap;
		gap: 0.5rem;
		overflow-y: auto;
		padding-right: 0.25rem;
		flex: 1;
		min-height: 0;
		align-content: flex-start; /* Fix vertical stretching and packing */
	}

	/* Draft Card Styles */
	.draft-card {
		width: 100%;
		max-width: 100%; /* Full width for the draft/search bar */
		border: 2px dashed #e5e7eb;
		background: #f9fafb;
		padding: 0; /* Let children handle padding */
		display: flex;
		flex-direction: column;
	}

	.need-draft {
		border-color: #f5576c;
		background: rgba(245, 87, 108, 0.02);
	}

	.capacity-draft {
		border-color: #00f2fe;
		background: rgba(0, 242, 254, 0.02);
	}

	.draft-tabs {
		display: flex;
		border-bottom: 1px solid #e5e7eb;
	}

	.draft-tab {
		flex: 1;
		padding: 0.5rem;
		font-size: 0.875rem;
		font-weight: 600;
		color: #6b7280;
		background: transparent;
		border: none;
		cursor: pointer;
		transition: all 0.2s;
	}

	.draft-tab:hover {
		background: rgba(0,0,0,0.02);
		color: #374151;
	}

	.draft-tab.active {
		color: #1f2937;
		background: white;
		box-shadow: 0 1px 0 white; /* Cover border */
	}
	
	.need-draft .draft-tab.active {
		color: #f5576c;
		border-bottom: 2px solid #f5576c;
	}

	.capacity-draft .draft-tab.active {
		color: #00a8b0;
		border-bottom: 2px solid #00f2fe;
	}

	.compact-select {
		appearance: none;
		background: white;
		border: 1px solid #e5e7eb;
		padding: 0.25rem 0.5rem;
		border-radius: 4px;
		font-size: 0.9rem;
		font-weight: 500;
		cursor: pointer;
		min-width: 120px;
	}

	/* Common input styles for mobile compactness */
	@media (max-width: 480px) {
		.slot-info {
			flex: 0 1 auto !important; /* Allow sharing the line! */
			min-width: 60px;
			margin-right: 0.25rem;
		}

		.slot-name {
			max-width: 140px; /* Force ellipsis on mobile to save space */
		}

		.slot-qty-input, .slot-unit-input {
			flex: 1 1 auto;
			min-width: 60px; /* Allow small inputs */
			width: auto;
		}
		
		.slot-unit-input {
			max-width: 60px; /* Keep unit small */
		}
	}
	
	.draft-input {
		flex: 1 1 auto;
		min-width: 80px;
		width: auto;
		padding: 0.5rem;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		font-size: 0.875rem;
	}

	/* Responsive adjustment for very small screens */
	@media (max-width: 480px) {
		.slot-main {
			/* Force standard row wrapping behavior */
			display: flex;
			flex-direction: row;
			flex-wrap: wrap;
			gap: 0.25rem;
			justify-content: flex-start;
			align-items: center;
		}
		
		/* 1. Name/Text Inputs: allowable elasticity */
		.slot-info, .draft-input {
			flex: 1 1 auto;      /* Grow to fill, shrink if needed */
			min-width: 80px;     /* Minimum legible width */
			max-width: 100%;     /* Prevent overflow */
			width: auto;         /* Reset any fixed widths */
			margin-right: 0.25rem;
		}
		
		.slot-name {
			max-width: 120px;
		}

		/* 2. Selectors & Fixed Controls: keep tight */
		.compact-select {
			flex: 0 1 auto;
			min-width: auto;     /* Reset desktop min-width */
			width: auto;
			max-width: 90px;
		}

		/* 3. Number Inputs: fixed small width */
		.slot-qty-input, .slot-unit-input, .draft-amount input {
			width: 3rem !important; /* Force small */
			flex: 0 0 auto;
			min-width: 0;
		}
		
		.draft-amount {
			flex: 0 1 auto;
			width: auto;
		}

		/* 4. Buttons: compact, side-by-side */
		.slot-btn, .btn-add {
			flex: 0 0 auto;      /* Never grow, take content width */
			width: auto;         /* Reset fixed widths */
			min-width: 0;
			white-space: nowrap;
		}
	}

	.draft-amount {
		display: flex;
		align-items: center;
		gap: 0.25rem;
		background: white;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		padding-left: 0.5rem;
	}

	.draft-amount input {
		border: none;
		width: 4rem;
	}
	
	.draft-amount input:focus {
		outline: none;
	}

	.slot-btn.disabled {
		opacity: 0.6;
		cursor: default;
		background: #f3f4f6;
		border-style: dashed;
	}

	
	.slot-card {
		flex: 0 1 auto;
		min-width: 300px;
		max-width: 500px;
		height: fit-content;
		background: white;
		border-radius: 6px;
		padding: 0.75rem;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.06);
		transition: all 0.15s ease;
		cursor: pointer;
		position: relative;
		overflow: hidden;
	}
	
	.slot-card:hover {
		box-shadow: 0 2px 6px rgba(0, 0, 0, 0.1);
		transform: translateY(-1px);
	}
	
	.slot-card.expanded {
		box-shadow: 0 3px 8px rgba(0, 0, 0, 0.12);
	}
	
	.slot-fill-indicator {
		position: absolute;
		top: 0;
		left: 0;
		height: 100%;
		background: linear-gradient(90deg, 
			rgba(0, 0, 0, 0.03) 0%, 
			rgba(0, 0, 0, 0.06) 100%
		);
		transition: width 0.3s ease;
		pointer-events: none;
		z-index: 0;
	}
	
	.need-card .slot-fill-indicator {
		background: linear-gradient(90deg, 
			rgba(245, 87, 108, 0.08) 0%, 
			rgba(245, 87, 108, 0.15) 100%
		);
	}
	
	.capacity-card .slot-fill-indicator {
		background: linear-gradient(90deg, 
			rgba(0, 242, 254, 0.08) 0%, 
			rgba(0, 242, 254, 0.15) 100%
		);
	}
	
	.need-card {
		border-left: 3px solid #f5576c;
	}
	
	.capacity-card {
		border-left: 3px solid #00f2fe;
	}
	
	.slot-main {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		flex-wrap: wrap;
		flex-wrap: wrap;
		position: relative;
		z-index: 1;
		padding: 0.5rem;
	}
	
	.slot-info {
		flex: 1;
		min-width: 0;
	}
	
	.slot-name {
		font-size: 0.95rem;
		font-weight: 600;
		color: #1f2937;
		margin-bottom: 0.125rem;
		white-space: nowrap;
		overflow: hidden;
		text-overflow: ellipsis;
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.allocation-badge {
		display: inline-flex;
		align-items: center;
		justify-content: center;
		min-width: 1.25rem;
		height: 1.25rem;
		padding: 0 0.375rem;
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		color: white;
		border-radius: 10px;
		font-size: 0.7rem;
		font-weight: 700;
	}
	
	.slot-time {
		font-size: 0.75rem;
		color: #6b7280;
		font-style: italic;
	}
	
	.slot-quantity {
		display: flex;
		align-items: center;
		gap: 0.375rem;
		background: #f9fafb;
		padding: 0.375rem 0.625rem;
		border-radius: 4px;
		border: 1px solid #e5e7eb;
	}
	
	.currency,
	.quantity-emoji {
		font-size: 1rem;
		font-weight: 700;
		color: #10b981;
	}
	
	
	/* Compact inline inputs */
	.slot-name-input {
		flex: 1;
		min-width: 120px;
		padding: 0.25rem 0.5rem;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		font-size: 0.875rem;
		font-weight: 500;
		color: #1f2937;
	}
	
	.slot-name-input:focus {
		outline: none;
		border-color: #3b82f6;
	}
	
	.currency-symbol,
	.type-emoji {
		font-size: 1rem;
		font-weight: 600;
	}
	
	.slot-qty-input {
		width: 4rem;
		padding: 0.25rem 0.375rem;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		font-size: 0.875rem;
		font-weight: 500;
		text-align: right;
		color: #1f2937;
	}
	
	.slot-qty-input:focus {
		outline: none;
		border-color: #3b82f6;
	}
	
	.slot-unit-input {
		width: 3rem;
		padding: 0.25rem 0.375rem;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		font-size: 0.875rem;
		color: #6b7280;
	}
	
	.slot-unit-input:focus {
		outline: none;
		border-color: #3b82f6;
	}
	
	/* Compact emoji-only buttons */
	.slot-btn {
		padding: 0.25rem 0.5rem;
		background: white;
		border: 1px solid #d1d5db;
		border-radius: 4px;
		font-size: 0.75rem;
		white-space: nowrap;
		cursor: pointer;
		transition: all 0.15s ease;
		line-height: 1.2;
	}
	
	.slot-btn:hover {
		background: #f3f4f6;
		border-color: #9ca3af;
	}
	
	.slot-btn-delete {
		padding: 0.25rem 0.5rem;
		background: white;
		border: 1px solid #fecaca;
		border-radius: 4px;
		font-size: 0.75rem;
		white-space: nowrap;
		cursor: pointer;
		transition: all 0.15s ease;
		line-height: 1.2;
	}
	
	.slot-btn-delete:hover {
		background: #fee2e2;
		border-color: #ef4444;
	}
	
	.slot-btn-alloc {
		padding: 0.25rem 0.5rem;
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		color: white;
		border: none;
		border-radius: 4px;
		font-size: 0.75rem;
		font-weight: 700;
		cursor: pointer;
		transition: all 0.15s ease;
	}
	
	.slot-btn-alloc:hover {
		transform: scale(1.05);
		box-shadow: 0 2px 4px rgba(102, 126, 234, 0.3);
	}
	
	/* Editor sections */
	.editor-section {
		margin-top: 0.5rem;
		padding: 0.75rem;
		background: #f9fafb;
		border-radius: 4px;
		border: 1px solid #e5e7eb;
		position: relative;
		z-index: 1;
	}
	
	.editor-header {
		margin-bottom: 0.75rem;
	}
	
	.editor-header h4 {
		margin: 0 0 0.25rem 0;
		font-size: 0.875rem;
		font-weight: 600;
		color: #374151;
	}
	
	.editor-header .help-text {
		margin: 0;
		font-size: 0.75rem;
		color: #6b7280;
		font-style: italic;
	}
	
	/* Remove old unused styles */
	.slot-actions {
		display: flex;
		gap: 0.375rem;
		align-items: center;
		position: relative;
		z-index: 1;
	}
	
	.btn-time {
		flex: 1;
		padding: 0.375rem 0.625rem;
		background: white;
		border: 1px solid #d1d5db;
		border-radius: 4px;
		font-size: 0.75rem;
		font-weight: 500;
		color: #374151;
		cursor: pointer;
		transition: all 0.15s ease;
	}
	
	.btn-time:hover {
		background: #f9fafb;
		border-color: #9ca3af;
	}
	
	.btn-delete {
		padding: 0.375rem 0.5rem;
		background: white;
		border: 1px solid #fecaca;
		border-radius: 4px;
		font-size: 0.9rem;
		cursor: pointer;
		transition: all 0.15s ease;
	}
	
	.btn-delete:hover {
		background: #fee2e2;
		border-color: #ef4444;
	}
	
	.btn-chat {
		padding: 0.375rem 0.5rem;
		background: white;
		border: 1px solid #cbd5e1;
		border-radius: 4px;
		font-size: 0.9rem;
		cursor: pointer;
		transition: all 0.15s ease;
	}
	
	.btn-chat:hover {
		background: #eff6ff;
		border-color: #3b82f6;
	}
	
	.unread-badge {
		position: absolute;
		top: -4px;
		right: -4px;
		background: #ef4444;
		color: white;
		font-size: 0.625rem;
		font-weight: 700;
		padding: 0.125rem 0.375rem;
		border-radius: 10px;
		min-width: 18px;
		text-align: center;
		line-height: 1;
	}
	
	.chat-section {
		margin-top: 0.75rem;
		padding: 0.75rem;
		background: #f0f9ff;
		border-radius: 6px;
		border: 1px solid #bae6fd;
		position: relative;
		z-index: 1;
	}
	
	.chat-header {
		margin-bottom: 0.75rem;
	}
	
	.chat-header h4 {
		margin: 0 0 0.25rem 0;
		font-size: 0.875rem;
		font-weight: 600;
		color: #075985;
	}
	
	.chat-header .help-text {
		margin: 0;
		font-size: 0.75rem;
		color: #0369a1;
		font-style: italic;
	}
	
	.time-editor-section {
		margin-top: 0.5rem;
		padding: 0.75rem;
		background: #f9fafb;
		border-radius: 4px;
		border: 1px solid #e5e7eb;
		position: relative;
		z-index: 1;
	}
	
	/* Allocations Section */
	.allocations-section {
		margin-top: 0.75rem;
		padding: 0.75rem;
		background: linear-gradient(135deg, #f0f9ff 0%, #e0f2fe 100%);
		border-radius: 6px;
		border: 1px solid #bae6fd;
		position: relative;
		z-index: 1;
	}
	
	.allocations-title {
		margin: 0 0 0.75rem 0;
		font-size: 0.85rem;
		font-weight: 600;
		color: #075985;
		display: flex;
		align-items: center;
		gap: 0.5rem;
		flex-wrap: wrap;
	}
	
	.total-allocated {
		font-size: 0.75rem;
		color: #0369a1;
		font-weight: 500;
	}
	
	.no-allocations {
		padding: 1rem;
		text-align: center;
		color: #64748b;
		font-size: 0.8rem;
		font-style: italic;
		background: white;
		border-radius: 4px;
	}
	
	.allocations-list {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}
	
	.allocation-item {
		background: white;
		padding: 0.625rem;
		border-radius: 4px;
		border: 1px solid #e0e7ff;
		transition: all 0.15s ease;
	}
	
	.allocation-item:hover {
		border-color: #c7d2fe;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
	}
	
	.allocation-item.mutual {
		border-left: 3px solid #10b981;
	}
	
	.allocation-item.extended {
		border-left: 3px solid #3b82f6;
	}
	
	.allocation-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 0.375rem;
	}
	
	.user-name {
		font-size: 0.875rem;
		font-weight: 600;
		color: #1e293b;
	}
	
	.allocation-amount {
		font-size: 0.875rem;
		font-weight: 700;
		color: #10b981;
	}
	
	.allocation-meta {
		display: flex;
		gap: 0.5rem;
		flex-wrap: wrap;
		align-items: center;
	}
	
	.status-badge {
		display: inline-flex;
		align-items: center;
		gap: 0.25rem;
		padding: 0.125rem 0.375rem;
		border-radius: 3px;
		font-size: 0.7rem;
		font-weight: 600;
	}
	
	.status-badge.mutual {
		background: #d1fae5;
		color: #065f46;
	}
	
	.status-badge.extended {
		background: #dbeafe;
		color: #1e40af;
	}
	
	.compatible {
		font-size: 0.7rem;
		color: #059669;
		font-weight: 500;
	}
	
	.empty-state {
		text-align: center;
		padding: 2rem 1rem;
		color: #9ca3af;
		font-style: italic;
		font-size: 0.85rem;
		background: #f9fafb;
		border-radius: 6px;
		border: 2px dashed #d1d5db;
	}
	
	/* Mobile responsive */
	@media (max-width: 768px) {
		.slots-container {
			padding: 0.5rem;
			gap: 0.5rem;
		}
		
		.type-select {
			min-width: 150px;
		}
		
		.tab {
			font-size: 0.8rem;
			padding: 0.5rem 0.75rem;
		}
		
		.slot-main {
			flex-direction: column;
			align-items: stretch;
		}
		
		.slot-quantity {
			justify-content: center;
		}
	}
</style>

