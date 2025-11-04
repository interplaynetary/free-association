<script lang="ts">
	import type { AvailabilitySlot, AvailabilityWindow } from '$lib/protocol/schemas';
	import { AvailabilitySlotSchema } from '$lib/protocol/schemas';
	import { 
		TimePatternEditor, 
		DivisibilityEditor, 
		LocationEditor,
		type LocationData
	} from './slots';
	import { t } from '$lib/translations';

	interface Props {
		slot: AvailabilitySlot;
		capacityId: string;
		canDelete: boolean;
		onupdate?: (slot: AvailabilitySlot) => void;
		ondelete?: (slotId: string) => void;
	}

	let { slot, capacityId, canDelete, onupdate, ondelete }: Props = $props();

	// UI state for expanded sections
	let timeExpanded = $state(false);
	let locationExpanded = $state(false);
	let constraintsExpanded = $state(false);

	// Derive display values from slot (reactive, no need for local state)
	const slotEmoji = $derived(slot.emoji || '📦');
	
	// Local editable fields for basic metadata
	let localName = $state(slot.name);
	let localQuantity = $state(slot.quantity);
	let localUnit = $state(slot.unit || 'units');
	let localNeedTypeId = $state(slot.need_type_id);
	let localDescription = $state(slot.description || '');
	
	// Keep local state in sync with prop changes
	$effect(() => {
		localName = slot.name;
		localQuantity = slot.quantity;
		localUnit = slot.unit || 'units';
		localNeedTypeId = slot.need_type_id;
		localDescription = slot.description || '';
	});

	// Update slot helper
	function updateSlot(updates: Partial<AvailabilitySlot>) {
		const updatedSlot: AvailabilitySlot = {
			...slot,
			...updates
		};

		// Validate
		const result = AvailabilitySlotSchema.safeParse(updatedSlot);
		if (!result.success) {
			console.error('[SLOT] Validation failed:', result.error);
			return;
		}

		onupdate?.(result.data);
	}

	// Basic field handlers
	function handleNameBlur() {
		if (localName !== slot.name) {
			updateSlot({ name: localName });
		}
	}

	function handleQuantityBlur() {
		if (localQuantity !== slot.quantity) {
			updateSlot({ quantity: localQuantity });
		}
	}

	function handleUnitBlur() {
		if (localUnit !== slot.unit) {
			updateSlot({ unit: localUnit });
		}
	}

	function handleNeedTypeChange() {
		if (localNeedTypeId !== slot.need_type_id) {
			updateSlot({ need_type_id: localNeedTypeId });
		}
	}

	function handleDescriptionBlur() {
		if (localDescription !== slot.description) {
			updateSlot({ description: localDescription });
		}
	}

	// Time pattern handler
	function handleTimePatternUpdate(recurrence: string | null, availabilityWindow?: AvailabilityWindow) {
		updateSlot({
			recurrence: recurrence as any,
			availability_window: availabilityWindow
		});
	}

	// Location handler
	function handleLocationUpdate(location: LocationData) {
		updateSlot(location as any);
	}

	// Divisibility handler
	function handleDivisibilityUpdate(maxNaturalDiv?: number, maxPercentageDiv?: number) {
		updateSlot({
			max_natural_div: maxNaturalDiv,
			max_percentage_div: maxPercentageDiv
		});
	}

	// Delete handler
	function handleDelete() {
		ondelete?.(slot.id);
	}

	// Toggle sections - ensure only one is open at a time
	function toggleTime() {
		timeExpanded = !timeExpanded;
		if (timeExpanded) {
			locationExpanded = false;
			constraintsExpanded = false;
		}
	}

	function toggleLocation() {
		locationExpanded = !locationExpanded;
		if (locationExpanded) {
			timeExpanded = false;
			constraintsExpanded = false;
		}
	}

	function toggleConstraints() {
		constraintsExpanded = !constraintsExpanded;
		if (constraintsExpanded) {
			timeExpanded = false;
			locationExpanded = false;
		}
	}

	// Display formatters
	function formatTimeDisplay(): string {
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
		} else {
			parts.push('All day');
		}
		
		return parts.join(', ');
	}

	function formatLocationDisplay(): string {
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

	function formatConstraintsDisplay(): string {
		const parts: string[] = [];
		
		if (slot.max_natural_div) {
			parts.push(`Max ${slot.max_natural_div} divisions`);
		}
		
		if (slot.max_percentage_div) {
			parts.push(`Min ${Math.round(slot.max_percentage_div * 100)}%`);
		}
		
		if (slot.advance_notice_hours) {
			parts.push(`${slot.advance_notice_hours}h notice`);
		}
		
		return parts.length > 0 ? parts.join(', ') : 'None';
	}
</script>

<div class="slot-item">
	<!-- Basic Metadata Row -->
	<div class="slot-metadata">
		<!-- Emoji (read-only display from slot) -->
		<span class="slot-emoji" title="Emoji">{slotEmoji}</span>
		
		<!-- Name (REQUIRED) -->
		<input
			type="text"
			class="slot-input name"
			bind:value={localName}
			onblur={handleNameBlur}
			placeholder="Slot name (required)"
			required
		/>
		
		<!-- Unit (editable) -->
		<div class="field-group">
			<label for="slot-unit-{slot.id}" class="field-label">Unit:</label>
			<input
				id="slot-unit-{slot.id}"
				type="text"
				class="slot-input unit"
				bind:value={localUnit}
				onblur={handleUnitBlur}
				placeholder="units"
			/>
		</div>
		
		<!-- Need Type (REQUIRED) -->
		<div class="field-group">
			<label for="slot-type-{slot.id}" class="field-label">Type:</label>
			<select
				id="slot-type-{slot.id}"
				class="slot-input need-type"
				bind:value={localNeedTypeId}
				onchange={handleNeedTypeChange}
				required
			>
				<option value="general">General</option>
				<option value="food">Food</option>
				<option value="housing">Housing</option>
				<option value="healthcare">Healthcare</option>
				<option value="education">Education</option>
				<option value="transportation">Transportation</option>
				<option value="childcare">Childcare</option>
				<option value="money">Money</option>
				<option value="other">Other</option>
			</select>
		</div>
	</div>
	
	<!-- Description (optional) -->
	{#if slot.description || localDescription}
		<div class="slot-description">
			<textarea
				bind:value={localDescription}
				onblur={handleDescriptionBlur}
				placeholder="Description..."
				rows="2"
			></textarea>
		</div>
	{/if}

	<!-- Slot Header Row -->
	<div class="slot-header">
		<!-- Quantity -->
		<input
			type="number"
			class="slot-input qty"
			min="0"
			step="0.01"
			bind:value={localQuantity}
			onblur={handleQuantityBlur}
			placeholder="Qty"
		/>

		<!-- Section Buttons -->
		<button
			type="button"
			class="section-btn time-btn"
			class:active={timeExpanded}
			onclick={toggleTime}
			title="Edit time pattern"
		>
			⏰ {formatTimeDisplay()}
		</button>

		<button
			type="button"
			class="section-btn location-btn"
			class:active={locationExpanded}
			onclick={toggleLocation}
			title="Edit location"
		>
			📍 {formatLocationDisplay()}
		</button>

		<button
			type="button"
			class="section-btn constraints-btn"
			class:active={constraintsExpanded}
			onclick={toggleConstraints}
			title="Edit constraints"
		>
			⚙️ {formatConstraintsDisplay()}
		</button>

		<!-- Delete Button -->
		<button
			type="button"
			class="delete-btn"
			onclick={handleDelete}
			disabled={!canDelete}
			title="Delete slot"
		>
			✖️
		</button>
	</div>

	<!-- Expanded Sections -->
	
	<!-- Time Section -->
	{#if timeExpanded}
		<div class="slot-details time-details">
			<TimePatternEditor
				recurrence={slot.recurrence}
				availabilityWindow={slot.availability_window}
				startDate={slot.start_date}
				endDate={slot.end_date}
				onUpdate={handleTimePatternUpdate}
			/>
		</div>
	{/if}

	<!-- Location Section -->
	{#if locationExpanded}
		<div class="slot-details location-details">
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
				onUpdate={handleLocationUpdate}
			/>
		</div>
	{/if}

	<!-- Constraints Section -->
	{#if constraintsExpanded}
		<div class="slot-details constraints-details">
			<DivisibilityEditor
				maxNaturalDiv={slot.max_natural_div}
				maxPercentageDiv={slot.max_percentage_div}
				onUpdate={handleDivisibilityUpdate}
			/>
		</div>
	{/if}
</div>

<style>
	.slot-item {
		padding: 1rem;
		background: white;
		border: 1px solid #e5e7eb;
		border-radius: 8px;
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
		transition: all 0.2s ease;
	}
	
	.slot-item:hover {
		border-color: #cbd5e1;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05);
	}
	
	.slot-metadata {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		flex-wrap: wrap;
		padding: 0.5rem;
		background: #f8fafc;
		border-radius: 6px;
	}
	
	.slot-emoji {
		font-size: 1.25rem;
		display: flex;
		align-items: center;
		justify-content: center;
		width: 2rem;
		height: 2rem;
	}
	
	.slot-input {
		padding: 0.375rem 0.5rem;
		border: 1px solid #cbd5e1;
		border-radius: 4px;
		font-size: 0.875rem;
		color: #1f2937;
		background: white;
		transition: all 0.2s ease;
	}
	
	.slot-input:focus {
		outline: none;
		border-color: #3b82f6;
		box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
	}
	
	.slot-input.name {
		flex: 1;
		min-width: 150px;
	}
	
	.slot-input.unit {
		width: 5rem;
	}
	
	.slot-input.need-type {
		width: auto;
		min-width: 120px;
	}
	
	.slot-input.qty {
		width: 5rem;
		text-align: right;
	}
	
	.field-group {
		display: flex;
		align-items: center;
		gap: 0.25rem;
	}
	
	.field-label {
		font-size: 0.7rem;
		font-weight: 600;
		color: #6b7280;
		white-space: nowrap;
	}
	
	.slot-description {
		padding: 0.5rem;
		background: #fffbeb;
		border-radius: 4px;
		border: 1px solid #fde68a;
	}
	
	.slot-description textarea {
		width: 100%;
		padding: 0.375rem 0.5rem;
		border: 1px solid #fbbf24;
		border-radius: 4px;
		font-size: 0.813rem;
		color: #78350f;
		background: white;
		resize: vertical;
	}
	
	.slot-description textarea:focus {
		outline: none;
		border-color: #f59e0b;
		box-shadow: 0 0 0 3px rgba(245, 158, 11, 0.1);
	}
	
	.slot-header {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		flex-wrap: wrap;
	}
	
	.section-btn {
		padding: 0.375rem 0.75rem;
		border: 1px solid #cbd5e1;
		border-radius: 6px;
		background: white;
		color: #475569;
		font-size: 0.75rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
		white-space: nowrap;
	}
	
	.section-btn:hover {
		background: #f8fafc;
		border-color: #94a3b8;
	}
	
	.section-btn.active {
		background: #eff6ff;
		border-color: #3b82f6;
		color: #1e40af;
	}
	
	.delete-btn {
		margin-left: auto;
		padding: 0.375rem 0.5rem;
		border: 1px solid #fecaca;
		border-radius: 6px;
		background: #fef2f2;
		color: #dc2626;
		font-size: 0.875rem;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.delete-btn:hover:not(:disabled) {
		background: #fee2e2;
		border-color: #fca5a5;
		transform: scale(1.05);
	}
	
	.delete-btn:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}
	
	.slot-details {
		margin-top: 0.5rem;
		animation: slideDown 0.2s ease-out;
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
</style>
