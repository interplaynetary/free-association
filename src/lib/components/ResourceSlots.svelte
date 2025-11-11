<script lang="ts">
	import type { NeedSlot, AvailabilitySlot, AvailabilityWindow, SlotAllocationRecord } from '$lib/protocol/schemas';
	import { TimePatternEditor } from './slots';
	import { NEED_TYPES, type NeedType } from '$lib/protocol/utils/needTypes';
	import { myAllocationsAsProvider } from '$lib/protocol/allocation.svelte';
	import { networkAllocations } from '$lib/protocol/stores.svelte';
	import { holsterUserPub } from '$lib/network/holster.svelte';
	import { getUserName } from '$lib/network/users.svelte';
	
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
		onNeedAdd: (name: string, quantity: number, needTypeId: string) => void;
		onCapacityAdd: (name: string, quantity: number, needTypeId: string) => void;
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
	
	// Selected need type
	let selectedNeedType = $state<string>('money'); // Default to money (USD)
	
	// Tab state: 'needs' or 'capacity'
	let activeTab = $state<'needs' | 'capacity'>('needs');
	
	// Add form state
	let newName = $state('');
	let newQuantity = $state(100);
	
	// Expanded state for time pattern editors
	let expandedSlots = $state<Set<string>>(new Set());
	
	// Expanded state for allocation details
	let expandedAllocations = $state<Set<string>>(new Set());
	
	// Get current need type info
	const currentNeedType = $derived(NEED_TYPES.find(t => t.id === selectedNeedType) || NEED_TYPES[0]);
	
	// Get current slots based on active tab AND selected need type
	const currentSlots = $derived(
		(activeTab === 'needs' ? needSlots : capacitySlots)
			.filter(slot => slot.need_type_id === selectedNeedType)
	);
	const isNeedMode = $derived(activeTab === 'needs');
	
	// Add slot handler
	function handleAddSlot() {
		if (!newName.trim()) return;
		
		if (isNeedMode) {
			onNeedAdd(newName, newQuantity, selectedNeedType);
		} else {
			onCapacityAdd(newName, newQuantity, selectedNeedType);
		}
		
		// Reset form
		newName = '';
		newQuantity = 100;
	}
	
	// Toggle time pattern editor (DRY)
	function toggleTimeEditor(id: string) {
		const newSet = new Set(expandedSlots);
		if (newSet.has(id)) {
			newSet.delete(id);
		} else {
			newSet.add(id);
		}
		expandedSlots = newSet;
	}
	
	// Toggle allocation details
	function toggleAllocationDetails(id: string) {
		const newSet = new Set(expandedAllocations);
		if (newSet.has(id)) {
			newSet.delete(id);
		} else {
			newSet.add(id);
		}
		expandedAllocations = newSet;
	}
	
	// Get outgoing allocations for a capacity slot
	function getCapacityAllocations(slotId: string): SlotAllocationRecord[] {
		const allocations = $myAllocationsAsProvider.allocations || [];
		return allocations.filter(a => a.availability_slot_id === slotId);
	}
	
	// Get incoming allocations for a need slot
	function getNeedAllocations(slotId: string): SlotAllocationRecord[] {
		const myPubKey = $holsterUserPub;
		if (!myPubKey) return [];
		
		const allAllocations: SlotAllocationRecord[] = [];
		
		// Iterate through all providers' allocations
		for (const [providerPubKey, allocations] of $networkAllocations) {
			if (allocations && Array.isArray(allocations)) {
				// Find allocations where I'm the recipient and it's for this slot
				const relevantAllocations = allocations.filter(
					a => a.recipient_pubkey === myPubKey && 
					     a.recipient_need_slot_id === slotId
				);
				allAllocations.push(...relevantAllocations);
			}
		}
		
		return allAllocations;
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
			availability_window: availabilityWindow
		};
		isNeed ? onNeedUpdate(updated as NeedSlot) : onCapacityUpdate(updated as AvailabilitySlot);
	}
	
	// Display formatter
	function formatTimeDisplay(slot: SlotType): string {
		if (!slot.recurrence) return 'monthly';
		return slot.recurrence + (slot.start_date ? ` from ${new Date(slot.start_date).toLocaleDateString()}` : '');
	}
</script>

{#snippet slotCard(slot: SlotType)}
	{@const allocations = isNeedMode ? getNeedAllocations(slot.id) : getCapacityAllocations(slot.id)}
	{@const totalAllocated = allocations.reduce((sum, a) => sum + a.quantity, 0)}
	{@const percentFilled = slot.quantity > 0 ? Math.min((totalAllocated / slot.quantity) * 100, 100) : 0}
	
	<div class="slot-card {isNeedMode ? 'need-card' : 'capacity-card'} {expandedAllocations.has(slot.id) ? 'expanded' : ''}">
		<div class="slot-fill-indicator" style="width: {percentFilled}%"></div>
		<div 
			class="slot-main" 
			role="button"
			tabindex="0"
			onclick={() => toggleAllocationDetails(slot.id)}
			onkeydown={(e) => (e.key === 'Enter' || e.key === ' ') && toggleAllocationDetails(slot.id)}
		>
			<div class="slot-info">
				<div class="slot-name">
					{slot.name}
					{#if allocations.length > 0}
						<span class="allocation-badge">{allocations.length}</span>
					{/if}
				</div>
				<div class="slot-time">{formatTimeDisplay(slot)}</div>
			</div>
			
			<div class="slot-quantity">
				{#if selectedNeedType === 'money'}
					<span class="currency">$</span>
				{:else}
					<span class="quantity-emoji">{currentNeedType.emoji}</span>
				{/if}
				<input
					type="number"
					value={slot.quantity}
					min="0"
					step="1"
					oninput={(e) => handleQuantityChange(slot, parseFloat((e.target as HTMLInputElement).value), isNeedMode)}
					onclick={(e) => e.stopPropagation()}
				/>
			</div>
		</div>
		
		<div class="slot-actions">
			<button
				type="button"
				class="btn-time"
				onclick={(e) => { e.stopPropagation(); toggleTimeEditor(slot.id); }}
			>
				🕐 {expandedSlots.has(slot.id) ? 'Close' : 'Edit Time'}
			</button>
			
			<button
				type="button"
				class="btn-delete"
				onclick={(e) => { e.stopPropagation(); isNeedMode ? onNeedDelete(slot.id) : onCapacityDelete(slot.id); }}
				title="Delete"
			>
				🗑️
			</button>
		</div>
		
		{#if expandedSlots.has(slot.id)}
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
					onUpdate={(recurrence: string | null, availabilityWindow?: AvailabilityWindow) => 
						handleTimePatternUpdate(slot, recurrence, availabilityWindow, isNeedMode)
					}
				/>
			</div>
		{/if}
		
		{#if expandedAllocations.has(slot.id)}
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
							
							<div class="allocation-item {allocation.tier}">
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
									<span class="tier-badge {allocation.tier}">
										{allocation.tier === 'mutual' ? '🤝 Mutual' : '➡️ One-way'}
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
	<!-- Need Type Selector -->
	<div class="need-type-selector">
		<label for="need-type-select">Resource Type:</label>
		<select 
			id="need-type-select"
			bind:value={selectedNeedType}
			class="type-select"
		>
			{#each NEED_TYPES as needType}
				<option value={needType.id}>
					{needType.emoji} {needType.label}
				</option>
			{/each}
		</select>
		<div class="type-description">
			{currentNeedType.description}
		</div>
	</div>
	
	<!-- Tab Navigation -->
	<div class="tabs">
		<button
			class="tab {activeTab === 'needs' ? 'active' : ''}"
			onclick={() => activeTab = 'needs'}
		>
		🎯 Needs ({needSlots.filter(s => s.need_type_id === selectedNeedType).length})
		</button>
		<button
			class="tab {activeTab === 'capacity' ? 'active' : ''}"
			onclick={() => activeTab = 'capacity'}
		>
			🎁 Capacity ({capacitySlots.filter(s => s.need_type_id === selectedNeedType).length})
		</button>
	</div>
	
	<!-- Add Form -->
	<div class="add-form">
		<input
			type="text"
			bind:value={newName}
			placeholder="Name or description..."
			onkeydown={(e) => e.key === 'Enter' && handleAddSlot()}
			class="input-name"
		/>
		<div class="quantity-input">
			{#if selectedNeedType === 'money'}
				<span class="currency-symbol">$</span>
			{:else}
				<span class="quantity-label">{currentNeedType.emoji}</span>
			{/if}
			<input
				type="number"
				bind:value={newQuantity}
				min="0"
				step="1"
				placeholder="per month"
			/>
		</div>
		<button onclick={handleAddSlot} class="btn-add {isNeedMode ? 'need-btn' : 'capacity-btn'}">
			➕ Add
		</button>
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
		flex-direction: column;
		gap: 0.5rem;
		overflow-y: auto;
		padding-right: 0.25rem;
		flex: 1;
		min-height: 0;
	}
	
	.slot-card {
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
		justify-content: space-between;
		align-items: center;
		gap: 0.75rem;
		margin-bottom: 0.5rem;
		position: relative;
		z-index: 1;
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
	
	.slot-quantity input {
		width: 70px;
		padding: 0.25rem 0.375rem;
		border: none;
		background: transparent;
		font-size: 0.9rem;
		font-weight: 600;
		text-align: right;
	}
	
	.slot-quantity input:focus {
		outline: none;
	}
	
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
	
	.allocation-item.non-mutual {
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
	
	.tier-badge {
		display: inline-flex;
		align-items: center;
		gap: 0.25rem;
		padding: 0.125rem 0.375rem;
		border-radius: 3px;
		font-size: 0.7rem;
		font-weight: 600;
	}
	
	.tier-badge.mutual {
		background: #d1fae5;
		color: #065f46;
	}
	
	.tier-badge.non-mutual {
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

