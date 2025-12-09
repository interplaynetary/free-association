<script lang="ts">
	import type { NeedSlot, AvailabilitySlot } from '@playnet/free-association/schemas';
	import { formatSlotTimeDisplay, formatSlotLocationDisplay } from '@playnet/free-association/utils/slots';
	import { onMount } from 'svelte';
	import SlotEditor from './Slot.svelte';
	
	/**
	 * Type Container Component - With Batch Editing
	 * 
	 * Generic container for displaying slots of a specific type.
	 * Works for both need slots and capacity slots.
	 * 
	 * NEW: Type-level batch editing - changes apply to all slots of this type!
	 * 
	 * Usage:
	 *   <Type 
	 *     typeId="food" 
	 *     slots={$myNeedSlotsStore} 
	 *     kind="need"
	 *     onBatchUpdate={handleBatchUpdate}
	 *   />
	 */
	
	import type { Snippet } from 'svelte';
	
	// Load emoji picker library and setup event listeners
	onMount(() => {
		// Load emoji picker asynchronously
		if (typeof window !== 'undefined') {
			import('emoji-picker-element').catch(err => 
				console.warn('Failed to load emoji picker:', err)
			);
			
			// Add global click listener to close picker when clicking outside
			document.addEventListener('click', handleClickOutside);
		}
		
		return () => {
			if (typeof window !== 'undefined') {
				document.removeEventListener('click', handleClickOutside);
			}
		};
	});
	
	type Slot = NeedSlot | AvailabilitySlot;
	
	interface Props {
		/** The need_type_id to display slots for */
		typeId: string;
		/** Array of slots (need or capacity) */
		slots: Slot[] | null | undefined;
		/** Kind of slot for display purposes */
		kind: 'need' | 'capacity';
		/** Optional: Custom display name for the type (defaults to typeId) */
		typeName?: string;
		/** Optional: Show empty state if no slots */
		showEmpty?: boolean;
		/** Optional: Custom content for each slot (snippet) */
		children?: Snippet<[{ slot: Slot }]>;
		/** Optional: Batch update callback - called when type-level fields change */
		onBatchUpdate?: (typeId: string, updates: Partial<Slot>) => void;
		/** Optional: Individual slot update callback */
		onSlotUpdate?: (slot: Slot) => void;
		/** Optional: Individual slot delete callback */
		onSlotDelete?: (slotId: string) => void;
		/** Optional: Capacity ID (for Slot.svelte) */
		capacityId?: string;
	}
	
	let {
		typeId,
		slots = null,
		kind,
		typeName = typeId,
		showEmpty = false,
		children: childrenSnippet,
		onBatchUpdate,
		onSlotUpdate,
		onSlotDelete,
		capacityId = 'default'
	}: Props = $props();
	
	// Filter slots to only this type
	const filteredSlots = $derived(
		slots?.filter(slot => slot.need_type_id === typeId) || []
	);
	
	// Check if we should show this section
	const shouldShow = $derived(filteredSlots.length > 0 || showEmpty);
	
	// Extract common metadata from first slot (for display)
	const firstSlot = $derived(filteredSlots[0]);
	
	// Derive display values from first slot
	// NOTE: Time/location patterns are NOT batch-edited - too specific per slot
	const typeEmoji = $derived(firstSlot?.emoji || '📦');
	const typeUnit = $derived(firstSlot?.unit || '');
	const typeDescription = $derived(firstSlot?.description || '');
	const typeResourceType = $derived(firstSlot?.resource_type || '');
	
	// Local editable state for batch fields (only update on blur)
	let localUnit = $state('');
	let localDescription = $state('');
	let localResourceType = $state('');
	
	// Keep local state in sync when derived values change
	$effect(() => {
		localUnit = typeUnit;
		localDescription = typeDescription;
		localResourceType = typeResourceType;
	});
	
	// Expanded state for type-level controls
	let typeMetadataExpanded = $state(false);
	
	// Emoji picker visibility
	let showEmojiPicker = $state(false);
	
	// Apply batch update to all slots of this type
	function applyBatchUpdate(field: keyof Slot, value: any) {
		if (!onBatchUpdate) return;
		
		const updates: Partial<Slot> = {
			[field]: value
		};
		
		onBatchUpdate(typeId, updates);
	}
	
	// Toggle emoji picker
	function toggleEmojiPicker() {
		showEmojiPicker = !showEmojiPicker;
	}
	
	// Handle emoji change (from emoji picker)
	function handleEmojiChange(emoji: string) {
		applyBatchUpdate('emoji', emoji);
		showEmojiPicker = false; // Close picker after selection
	}
	
	// Close emoji picker when clicking outside
	function handleClickOutside(event: MouseEvent) {
		if (!showEmojiPicker) return;
		
		const target = event.target as HTMLElement;
		const emojiContainer = target.closest('.emoji-picker-container');
		
		// Don't close if clicking inside the emoji picker container
		if (!emojiContainer) {
			showEmojiPicker = false;
		}
	}
	
	// Handle blur events - only update when field loses focus
	function handleUnitBlur() {
		if (localUnit !== typeUnit) {
			applyBatchUpdate('unit', localUnit);
		}
	}
	
	function handleDescriptionBlur() {
		if (localDescription !== typeDescription) {
			applyBatchUpdate('description', localDescription);
		}
	}
	
	function handleResourceTypeBlur() {
		if (localResourceType !== typeResourceType) {
			applyBatchUpdate('resource_type', localResourceType);
		}
	}
</script>

{#if shouldShow}
	<section class="type-container">
		<header class="type-header">
			<div class="type-header-main">
				<div class="type-info">
					<!-- Editable emoji (batch updates all slots) -->
					<div class="emoji-picker-container">
						<button
							type="button"
							class="emoji-picker-btn"
							onclick={toggleEmojiPicker}
							title="Click to pick emoji for all {typeName} slots"
						>
							{typeEmoji || '📦'}
						</button>
						
						{#if showEmojiPicker}
							<div class="emoji-picker-popup">
								<emoji-picker 
									class="light"
									style="width: 320px; height: 400px;"
									onemoji-click={(e: any) => handleEmojiChange(e.detail.unicode)}
								></emoji-picker>
							</div>
						{/if}
					</div>
					
					<div class="type-title-group">
						<h3 class="type-title">
							{typeName}
							<span class="type-count">({filteredSlots.length})</span>
						</h3>
						<span class="type-kind-badge {kind}">{kind}</span>
					</div>
				</div>
				
				<!-- Toggle type metadata button -->
				<button
					type="button"
					class="metadata-toggle-btn"
					onclick={() => (typeMetadataExpanded = !typeMetadataExpanded)}
					title="Edit type-level metadata (batch updates all slots)"
				>
					{typeMetadataExpanded ? '▼' : '▶'}  ⚙️
				</button>
			</div>
		</header>
		
		<!-- Type-level metadata editor (batch updates) -->
		{#if typeMetadataExpanded}
			<div class="type-metadata-section">
				<h4 class="metadata-title">📝 Batch Edit (applies to all {filteredSlots.length} slots)</h4>
				
				<div class="metadata-fields">
					<div class="metadata-field">
						<label for="{typeId}-unit">Unit:</label>
						<input
							id="{typeId}-unit"
							type="text"
							bind:value={localUnit}
							placeholder="units"
							onblur={handleUnitBlur}
						/>
					</div>
					
					<div class="metadata-field">
						<label for="{typeId}-resource-type">Resource Type:</label>
						<input
							id="{typeId}-resource-type"
							type="text"
							bind:value={localResourceType}
							placeholder="e.g., perishable, durable"
							onblur={handleResourceTypeBlur}
						/>
					</div>
					
					<div class="metadata-field full-width">
						<label for="{typeId}-description">Description:</label>
						<textarea
							id="{typeId}-description"
							bind:value={localDescription}
							placeholder="Shared description for all slots..."
							rows="3"
							onblur={handleDescriptionBlur}
						></textarea>
					</div>
				</div>
				
				<p class="metadata-hint">
					💡 Changes here update all {filteredSlots.length} slot(s) of this type
				</p>
			</div>
		{/if}
		
		{#if filteredSlots.length > 0}
			<div class="slots-list">
				{#each filteredSlots as slot (slot.id)}
					<SlotEditor
						slot={slot as AvailabilitySlot}
						capacityId={capacityId}
						canDelete={filteredSlots.length > 1}
						isCapacity={kind === 'capacity'}
						onupdate={(updatedSlot: AvailabilitySlot) => onSlotUpdate?.(updatedSlot)}
						ondelete={(slotId: string) => onSlotDelete?.(slotId)}
					/>
					
					<!-- Custom content snippet (if provided) -->
					{#if childrenSnippet}
						<div class="slot-actions">
							{@render childrenSnippet({ slot })}
						</div>
					{/if}
				{/each}
			</div>
		{:else}
			<div class="empty-state">
				<p>No {kind} slots of this type yet</p>
			</div>
		{/if}
	</section>
{/if}

<style>
	.type-container {
		border: 1px solid #e5e7eb;
		border-radius: 8px;
		padding: 0;
		margin-bottom: 1rem;
		background: white;
		overflow: hidden;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
		transition: box-shadow 0.2s ease;
	}
	
	.type-container:hover {
		box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
	}
	
	.type-header {
		background: rgba(249, 250, 251, 0.8);
		border-bottom: 1px solid #e5e7eb;
		padding: 0.75rem 1rem;
	}
	
	.type-header-main {
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 1rem;
	}
	
	.type-info {
		display: flex;
		align-items: center;
		gap: 0.75rem;
		flex: 1;
	}
	
	.emoji-picker-container {
		position: relative;
	}
	
	.emoji-picker-popup {
		position: absolute;
		top: 100%;
		left: 0;
		margin-top: 0.5rem;
		z-index: 1000;
		box-shadow: 0 10px 25px rgba(0, 0, 0, 0.2);
		border-radius: 8px;
		overflow: hidden;
	}
	
	.emoji-picker-btn {
		width: 3rem;
		height: 3rem;
		font-size: 1.75rem;
		display: flex;
		align-items: center;
		justify-content: center;
		border: 1px dashed #cbd5e1;
		border-radius: 8px;
		background: white;
		cursor: pointer;
		transition: all 0.2s ease;
		flex-shrink: 0;
	}
	
	.emoji-picker-btn:hover {
		border-color: #3b82f6;
		background: #f0f9ff;
		transform: scale(1.05);
	}
	
	.emoji-picker-btn:active {
		transform: scale(0.95);
	}
	
	.type-title-group {
		display: flex;
		flex-direction: column;
		gap: 0.25rem;
	}
	
	.type-title {
		margin: 0;
		font-size: 1.25rem;
		font-weight: 600;
		color: #1f2937;
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.type-count {
		font-size: 0.9rem;
		color: #6b7280;
		font-weight: 400;
	}
	
	.type-kind-badge {
		padding: 0.25rem 0.75rem;
		border-radius: 4px;
		font-size: 0.75rem;
		font-weight: 600;
		text-transform: uppercase;
		letter-spacing: 0.05em;
	}
	
	.type-kind-badge.need {
		background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%);
		color: white;
	}
	
	.type-kind-badge.capacity {
		background: linear-gradient(135deg, #10b981 0%, #059669 100%);
		color: white;
	}
	
	.metadata-toggle-btn {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		padding: 0.4rem 0.75rem;
		border: 1px solid #cbd5e1;
		border-radius: 6px;
		background: white;
		color: #64748b;
		font-size: 0.8rem;
		cursor: pointer;
		transition: all 0.2s ease;
		flex-shrink: 0;
	}
	
	.metadata-toggle-btn:hover {
		background: #f8fafc;
		border-color: #94a3b8;
		color: #475569;
		transform: translateY(-1px);
	}
	
	.metadata-toggle-btn:active {
		transform: translateY(0);
	}
	
	/* Type metadata section */
	.type-metadata-section {
		background: #fef3c7;
		border-bottom: 1px solid #f59e0b;
		padding: 1rem 1.25rem;
		animation: slideDown 0.2s ease-out;
	}
	
	.metadata-title {
		margin: 0 0 1rem 0;
		font-size: 0.875rem;
		font-weight: 600;
		color: #92400e;
	}
	
	.metadata-fields {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
		gap: 1rem;
	}
	
	.metadata-field {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}
	
	.metadata-field.full-width {
		grid-column: 1 / -1;
	}
	
	.metadata-field label {
		font-size: 0.75rem;
		font-weight: 600;
		color: #78350f;
		text-transform: uppercase;
		letter-spacing: 0.05em;
	}
	
	.metadata-field input,
	.metadata-field textarea {
		padding: 0.5rem 0.75rem;
		border: 1px solid #d97706;
		border-radius: 4px;
		background: white;
		font-size: 0.875rem;
		color: #1f2937;
		transition: all 0.2s ease;
	}
	
	.metadata-field input:focus,
	.metadata-field textarea:focus {
		outline: none;
		border-color: #f59e0b;
		box-shadow: 0 0 0 3px rgba(245, 158, 11, 0.1);
	}
	
	.metadata-field textarea {
		resize: vertical;
		font-family: inherit;
	}
	
	.metadata-hint {
		margin: 1rem 0 0 0;
		font-size: 0.75rem;
		color: #92400e;
		font-style: italic;
	}
	
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
	
	/* Slots list */
	.slots-list {
		padding: 0.75rem 1rem;
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
		background: #fafbfc;
	}
	
	.slot-actions {
		margin-top: 0;
		padding: 0.75rem;
		background: transparent;
		border-radius: 0;
		border: none;
	}
	
	.slot-actions:empty {
		display: none;
	}
	
	.empty-state {
		padding: 2.5rem 1rem;
		text-align: center;
		color: #94a3b8;
		font-style: italic;
		background: #f8fafc;
		border-radius: 6px;
		margin: 0.75rem 1rem;
		border: 1px dashed #cbd5e1;
	}
	
	.empty-state p {
		margin: 0;
		font-size: 0.875rem;
	}
</style>
