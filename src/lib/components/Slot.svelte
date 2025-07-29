<script lang="ts">
	import type { AvailabilitySlot } from '$lib/schema';
	import { AvailabilitySlotSchema } from '$lib/schema';

	interface Props {
		slot: AvailabilitySlot;
		capacityId: string;
		unit?: string;
		canDelete: boolean;
		onupdate?: (slot: AvailabilitySlot) => void;
		ondelete?: (slotId: string) => void;
	}

	let { slot, capacityId, unit, canDelete, onupdate, ondelete }: Props = $props();

	// UI state for expanded slot details sections
	let timeExpanded = $state(false);
	let constraintsExpanded = $state(false);
	let locationExpanded = $state(false);

	// Reactive slot properties for proper binding
	let slotId = $state(slot.id);
	let slotQuantity = $state(slot.quantity);
	let slotAdvanceNoticeHours = $state(slot.advance_notice_hours);
	let slotBookingWindowHours = $state(slot.booking_window_hours);
	let slotMutualAgreementRequired = $state(slot.mutual_agreement_required);
	let slotPriority = $state(slot.priority);

	// Time fields
	let slotAllDay = $state(slot.all_day);
	let slotStartDate = $state(slot.start_date);
	let slotEndDate = $state(slot.end_date);
	let slotStartTime = $state(slot.start_time);
	let slotEndTime = $state(slot.end_time);
	let slotTimeZone = $state(slot.time_zone);
	let slotRecurrence = $state(slot.recurrence);
	let slotCustomRecurrenceRepeatEvery = $state(slot.custom_recurrence_repeat_every);
	let slotCustomRecurrenceRepeatUnit = $state(slot.custom_recurrence_repeat_unit);
	let slotCustomRecurrenceEndType = $state(slot.custom_recurrence_end_type);
	let slotCustomRecurrenceEndValue = $state(slot.custom_recurrence_end_value);

	// Location fields
	let slotLocationType = $state(slot.location_type);
	let slotLongitude = $state(slot.longitude);
	let slotLatitude = $state(slot.latitude);
	let slotStreetAddress = $state(slot.street_address);
	let slotCity = $state(slot.city);
	let slotStateProvince = $state(slot.state_province);
	let slotPostalCode = $state(slot.postal_code);
	let slotCountry = $state(slot.country);

	// Location format state ('coordinates' or 'address')
	let locationFormat = $state<'coordinates' | 'address'>('address');

	// Track original values for change detection
	let originalValues = $state<Record<string, any>>({});

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
			handleSlotUpdate();
		}
	}

	// Handler for slot updates
	function handleSlotUpdate() {
		const updatedSlot = {
			...slot,
			id: slotId,
			quantity: slotQuantity,
			advance_notice_hours: slotAdvanceNoticeHours,
			booking_window_hours: slotBookingWindowHours,
			mutual_agreement_required: slotMutualAgreementRequired,
			priority: slotPriority,

			// Time fields
			all_day: slotAllDay,
			start_date: slotStartDate,
			end_date: slotEndDate,
			start_time: slotStartTime,
			end_time: slotEndTime,
			time_zone: slotTimeZone,
			recurrence: slotRecurrence,
			custom_recurrence_repeat_every: slotCustomRecurrenceRepeatEvery,
			custom_recurrence_repeat_unit: slotCustomRecurrenceRepeatUnit,
			custom_recurrence_end_type: slotCustomRecurrenceEndType,
			custom_recurrence_end_value: slotCustomRecurrenceEndValue,

			// Location fields
			location_type: slotLocationType,
			longitude: slotLongitude,
			latitude: slotLatitude,
			street_address: slotStreetAddress,
			city: slotCity,
			state_province: slotStateProvince,
			postal_code: slotPostalCode,
			country: slotCountry
		};

		// Validate using schema
		const validationResult = AvailabilitySlotSchema.safeParse(updatedSlot);

		if (!validationResult.success) {
			console.error('Slot validation failed:', validationResult.error);
			return;
		}

		// Validation passed, proceed with update
		onupdate?.(validationResult.data);
	}

	// Delete this slot
	function handleDelete() {
		ondelete?.(slot.id);
	}

	// Toggle expanded sections
	function toggleTime() {
		timeExpanded = !timeExpanded;
	}

	function toggleConstraints() {
		constraintsExpanded = !constraintsExpanded;
	}

	function toggleLocation() {
		locationExpanded = !locationExpanded;
	}

	// Format time display
	function formatTimeDisplay(): string {
		const parts = [];

		if (slotStartDate) {
			parts.push(new Date(slotStartDate).toLocaleDateString());
		}

		if (!slotAllDay && slotStartTime) {
			parts.push(slotStartTime);
		}

		if (slotAllDay) {
			parts.push('All day');
		}

		return parts.length > 0 ? parts.join(' ') : 'No time set';
	}

	// Format location display
	function formatLocationDisplay(): string {
		if (slotLocationType === 'Specific') {
			if (slotStreetAddress) {
				return slotStreetAddress;
			}
			if (slotLatitude && slotLongitude) {
				return `${slotLatitude.toFixed(4)}, ${slotLongitude.toFixed(4)}`;
			}
		}

		return slotLocationType || 'No location';
	}

	// Format date for input
	function formatDateForInput(date: Date | undefined): string {
		if (!date) return '';
		return date.toISOString().split('T')[0];
	}

	// Format constraints display
	function formatConstraintsDisplay(): string {
		const parts = [];

		if (slotAdvanceNoticeHours && slotAdvanceNoticeHours > 0) {
			parts.push(`${slotAdvanceNoticeHours}h notice`);
		}

		if (slotBookingWindowHours && slotBookingWindowHours > 0) {
			parts.push(`${slotBookingWindowHours}h window`);
		}

		if (slotMutualAgreementRequired) {
			parts.push('Agreement req.');
		}

		if (slotPriority && slotPriority !== 0) {
			parts.push(`Priority: ${slotPriority}`);
		}

		return parts.length > 0 ? parts.join(', ') : 'No constraints';
	}
</script>

<div class="slot-item rounded border border-gray-200 bg-white p-3 shadow-sm">
	<!-- Slot header row -->
	<div class="slot-header mb-2 flex flex-wrap items-center gap-2">
		<!-- Quantity input -->
		<input
			type="number"
			class="slot-input qty w-20 text-right"
			min="0"
			step="0.01"
			bind:value={slotQuantity}
			placeholder="Qty"
			onfocus={() => handleFocus('quantity', slotQuantity)}
			onblur={() => handleBlurIfChanged('quantity', slotQuantity)}
		/>
		{#if unit}
			<span class="slot-unit text-xs text-gray-500">({unit})</span>
		{/if}

		<!-- Time button -->
		<button
			type="button"
			class="section-btn time-btn"
			onclick={toggleTime}
			title="Edit time settings"
		>
			⏰ {formatTimeDisplay()}
		</button>

		<!-- Constraints button -->
		<button
			type="button"
			class="section-btn constraints-btn"
			onclick={toggleConstraints}
			title="Edit constraints"
		>
			⚙️ {formatConstraintsDisplay()}
		</button>

		<!-- Location button -->
		<button
			type="button"
			class="section-btn location-btn"
			onclick={toggleLocation}
			title="Edit location"
		>
			📍 {formatLocationDisplay()}
		</button>

		<!-- Delete button -->
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

	<!-- Slot summary row -->
	<div class="slot-summary mb-2 text-xs text-gray-500">
		<div class="flex flex-wrap gap-4">
			<span>⏰ {formatTimeDisplay()}</span>
			<span>📍 {formatLocationDisplay()}</span>
			{#if slotMutualAgreementRequired}
				<span>🤝 Mutual agreement required</span>
			{/if}
		</div>
	</div>

	<!-- Expanded slot details -->
	<!-- Time section -->
	{#if timeExpanded}
		<div class="slot-details time-details mt-3 rounded bg-blue-50 p-4">
			<h5 class="mb-3 text-sm font-medium text-gray-700">⏰ Time Settings</h5>

			<div class="mb-4">
				<label class="inline-flex items-center">
					<input
						type="checkbox"
						bind:checked={slotAllDay}
						class="mr-2"
						onchange={handleSlotUpdate}
					/>
					<span class="text-sm text-gray-600">All day</span>
				</label>
			</div>

			<div class="mb-4 grid grid-cols-1 gap-4 md:grid-cols-2">
				<div>
					<label class="mb-1 block text-xs text-gray-500">Start Date</label>
					<input
						type="date"
						class="slot-input w-full"
						bind:value={slotStartDate}
						onfocus={() => handleFocus('startDate', slotStartDate)}
						onblur={() => handleBlurIfChanged('startDate', slotStartDate)}
					/>
				</div>
				<div>
					<label class="mb-1 block text-xs text-gray-500">End Date</label>
					<input
						type="date"
						class="slot-input w-full"
						bind:value={slotEndDate}
						onfocus={() => handleFocus('endDate', slotEndDate)}
						onblur={() => handleBlurIfChanged('endDate', slotEndDate)}
					/>
				</div>
			</div>

			{#if !slotAllDay}
				<div class="mb-4 grid grid-cols-1 gap-4 md:grid-cols-2">
					<div>
						<label class="mb-1 block text-xs text-gray-500">Start Time</label>
						<input
							type="time"
							class="slot-input w-full"
							bind:value={slotStartTime}
							onfocus={() => handleFocus('startTime', slotStartTime)}
							onblur={() => handleBlurIfChanged('startTime', slotStartTime)}
						/>
					</div>
					<div>
						<label class="mb-1 block text-xs text-gray-500">End Time</label>
						<input
							type="time"
							class="slot-input w-full"
							bind:value={slotEndTime}
							onfocus={() => handleFocus('endTime', slotEndTime)}
							onblur={() => handleBlurIfChanged('endTime', slotEndTime)}
						/>
					</div>
				</div>

				<div class="mb-4">
					<label class="mb-1 block text-xs text-gray-500">Time Zone</label>
					<input
						type="text"
						class="slot-input w-full"
						bind:value={slotTimeZone}
						placeholder="e.g. America/New_York"
						onfocus={() => handleFocus('timeZone', slotTimeZone)}
						onblur={() => handleBlurIfChanged('timeZone', slotTimeZone)}
					/>
				</div>
			{/if}

			<!-- Recurrence -->
			<div class="mb-4">
				<label class="mb-1 block text-xs text-gray-500">Recurrence</label>
				<select class="slot-select w-full" bind:value={slotRecurrence} onchange={handleSlotUpdate}>
					{#each recurrenceOptions as option}
						<option value={option}>{option}</option>
					{/each}
				</select>
			</div>

			<!-- Custom recurrence -->
			{#if slotRecurrence === 'Custom...' && slotCustomRecurrenceRepeatEvery !== undefined}
				<div class="custom-recurrence rounded border bg-white p-3">
					<div class="mb-3 flex items-center gap-2">
						<span class="text-xs text-gray-500">Repeat every</span>
						<input
							type="number"
							min="1"
							class="slot-input w-16 text-center"
							bind:value={slotCustomRecurrenceRepeatEvery}
							onfocus={() =>
								handleFocus('customRecurrenceRepeatEvery', slotCustomRecurrenceRepeatEvery)}
							onblur={() =>
								handleBlurIfChanged('customRecurrenceRepeatEvery', slotCustomRecurrenceRepeatEvery)}
						/>
						<select
							class="slot-select w-20"
							bind:value={slotCustomRecurrenceRepeatUnit}
							onchange={handleSlotUpdate}
						>
							<option value="days">days</option>
							<option value="weeks">weeks</option>
							<option value="months">months</option>
							<option value="years">years</option>
						</select>
					</div>
				</div>
			{/if}
		</div>
	{/if}

	<!-- Constraints section -->
	{#if constraintsExpanded}
		<div class="slot-details constraints-details mt-3 rounded bg-yellow-50 p-4">
			<h5 class="mb-3 text-sm font-medium text-gray-700">⚙️ Constraints</h5>
			<div class="grid grid-cols-1 gap-4 md:grid-cols-2">
				<div>
					<label class="mb-1 block text-xs text-gray-500">Advance notice (hours)</label>
					<input
						type="number"
						class="slot-input w-full"
						min="0"
						step="0.5"
						bind:value={slotAdvanceNoticeHours}
						placeholder="0"
						onfocus={() => handleFocus('advanceNoticeHours', slotAdvanceNoticeHours)}
						onblur={() => handleBlurIfChanged('advanceNoticeHours', slotAdvanceNoticeHours)}
					/>
				</div>
				<div>
					<label class="mb-1 block text-xs text-gray-500">Booking window (hours)</label>
					<input
						type="number"
						class="slot-input w-full"
						min="0"
						step="0.5"
						bind:value={slotBookingWindowHours}
						placeholder="No limit"
						onfocus={() => handleFocus('bookingWindowHours', slotBookingWindowHours)}
						onblur={() => handleBlurIfChanged('bookingWindowHours', slotBookingWindowHours)}
					/>
				</div>
			</div>

			<div class="mt-4">
				<label class="inline-flex items-center">
					<input
						type="checkbox"
						bind:checked={slotMutualAgreementRequired}
						class="mr-2"
						onchange={handleSlotUpdate}
					/>
					<span class="text-sm text-gray-600">Requires mutual agreement</span>
				</label>
			</div>

			<div class="mt-4">
				<label class="mb-1 block text-xs text-gray-500">Priority (optional)</label>
				<input
					type="number"
					class="slot-input w-24"
					bind:value={slotPriority}
					placeholder="0"
					onfocus={() => handleFocus('priority', slotPriority)}
					onblur={() => handleBlurIfChanged('priority', slotPriority)}
				/>
			</div>
		</div>
	{/if}

	<!-- Location section -->
	{#if locationExpanded}
		<div class="slot-details location-details mt-3 rounded bg-green-50 p-4">
			<h5 class="mb-3 text-sm font-medium text-gray-700">📍 Location</h5>

			<div class="mb-4">
				<div class="flex flex-wrap gap-4">
					<label class="inline-flex items-center">
						<input
							type="radio"
							name="location-type-{slotId}"
							value="Undefined"
							bind:group={slotLocationType}
							class="mr-2"
							onchange={handleSlotUpdate}
						/>
						<span class="text-sm text-gray-600">Undefined</span>
					</label>
					<label class="inline-flex items-center">
						<input
							type="radio"
							name="location-type-{slotId}"
							value="LiveLocation"
							bind:group={slotLocationType}
							class="mr-2"
							onchange={handleSlotUpdate}
						/>
						<span class="text-sm text-gray-600">Live location</span>
					</label>
					<label class="inline-flex items-center">
						<input
							type="radio"
							name="location-type-{slotId}"
							value="Specific"
							bind:group={slotLocationType}
							class="mr-2"
							onchange={handleSlotUpdate}
						/>
						<span class="text-sm text-gray-600">Specific</span>
					</label>
				</div>
			</div>

			{#if slotLocationType === 'Specific'}
				<!-- Location format toggle -->
				<div class="mb-4">
					<div class="flex flex-wrap gap-4">
						<label class="inline-flex items-center">
							<input
								type="radio"
								name="location-format-{slotId}"
								value="address"
								checked={locationFormat === 'address'}
								class="mr-2"
								onchange={() => {
									locationFormat = 'address';
								}}
							/>
							<span class="text-sm text-gray-600">Address</span>
						</label>
						<label class="inline-flex items-center">
							<input
								type="radio"
								name="location-format-{slotId}"
								value="coordinates"
								checked={locationFormat === 'coordinates'}
								class="mr-2"
								onchange={() => {
									locationFormat = 'coordinates';
								}}
							/>
							<span class="text-sm text-gray-600">Coordinates</span>
						</label>
					</div>
				</div>

				{#if locationFormat === 'address'}
					<div class="address-fields space-y-3">
						<input
							type="text"
							class="slot-input w-full"
							bind:value={slotStreetAddress}
							placeholder="Street address"
							onfocus={() => handleFocus('streetAddress', slotStreetAddress)}
							onblur={() => handleBlurIfChanged('streetAddress', slotStreetAddress)}
						/>
						<div class="grid grid-cols-2 gap-3">
							<input
								type="text"
								class="slot-input w-full"
								bind:value={slotCity}
								placeholder="City"
								onfocus={() => handleFocus('city', slotCity)}
								onblur={() => handleBlurIfChanged('city', slotCity)}
							/>
							<input
								type="text"
								class="slot-input w-full"
								bind:value={slotStateProvince}
								placeholder="State/Province"
								onfocus={() => handleFocus('stateProvince', slotStateProvince)}
								onblur={() => handleBlurIfChanged('stateProvince', slotStateProvince)}
							/>
						</div>
						<div class="grid grid-cols-2 gap-3">
							<input
								type="text"
								class="slot-input w-full"
								bind:value={slotPostalCode}
								placeholder="Postal code"
								onfocus={() => handleFocus('postalCode', slotPostalCode)}
								onblur={() => handleBlurIfChanged('postalCode', slotPostalCode)}
							/>
							<input
								type="text"
								class="slot-input w-full"
								bind:value={slotCountry}
								placeholder="Country"
								onfocus={() => handleFocus('country', slotCountry)}
								onblur={() => handleBlurIfChanged('country', slotCountry)}
							/>
						</div>
					</div>
				{:else}
					<div class="coordinates-fields grid grid-cols-2 gap-3">
						<div>
							<label class="mb-1 block text-xs text-gray-500">Latitude</label>
							<input
								type="number"
								class="slot-input w-full"
								min="-90"
								max="90"
								step="0.000001"
								bind:value={slotLatitude}
								placeholder="37.7749"
								onfocus={() => handleFocus('latitude', slotLatitude)}
								onblur={() => handleBlurIfChanged('latitude', slotLatitude)}
							/>
						</div>
						<div>
							<label class="mb-1 block text-xs text-gray-500">Longitude</label>
							<input
								type="number"
								class="slot-input w-full"
								min="-180"
								max="180"
								step="0.000001"
								bind:value={slotLongitude}
								placeholder="-122.4194"
								onfocus={() => handleFocus('longitude', slotLongitude)}
								onblur={() => handleBlurIfChanged('longitude', slotLongitude)}
							/>
						</div>
					</div>
				{/if}
			{/if}
		</div>
	{/if}
</div>

<style>
	.slot-input {
		font-size: 0.875rem;
		padding: 6px 8px;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		background: white;
		outline: none;
		transition: all 0.2s ease;
	}

	.slot-input::placeholder {
		color: #cbd5e1;
	}

	.slot-input:focus {
		border-color: #3b82f6;
		box-shadow: 0 0 0 1px rgba(59, 130, 246, 0.2);
	}

	.slot-select {
		font-size: 0.875rem;
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

	.slot-select:focus {
		border-color: #3b82f6;
		box-shadow: 0 0 0 1px rgba(59, 130, 246, 0.2);
	}

	.expand-btn {
		background: none;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		padding: 2px 6px;
		color: #6b7280;
		font-size: 0.75rem;
		cursor: pointer;
		transition: all 0.2s ease;
		width: 24px;
		height: 24px;
		display: flex;
		align-items: center;
		justify-content: center;
	}

	.expand-btn:hover {
		background: #f9fafb;
		border-color: #3b82f6;
		color: #3b82f6;
	}

	.delete-btn {
		background: none;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		padding: 2px 6px;
		color: #6b7280;
		font-size: 0.75rem;
		cursor: pointer;
		transition: all 0.2s ease;
		width: 24px;
		height: 24px;
		display: flex;
		align-items: center;
		justify-content: center;
	}

	.delete-btn:hover:not(:disabled) {
		background: #fef2f2;
		border-color: #ef4444;
		color: #ef4444;
	}

	.delete-btn:disabled {
		color: #d1d5db;
		cursor: not-allowed;
	}

	.slot-item {
		animation: slideDown 0.2s ease-out;
	}

	.slot-details {
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

	.slot-summary span {
		display: inline-flex;
		align-items: center;
		gap: 2px;
	}

	.section-btn {
		background: white;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		padding: 4px 8px;
		color: #374151;
		font-size: 0.75rem;
		cursor: pointer;
		transition: all 0.2s ease;
		min-width: 80px;
		text-align: left;
		line-height: 1.2;
		max-width: 150px;
		white-space: nowrap;
		overflow: hidden;
		text-overflow: ellipsis;
	}

	.section-btn:hover {
		background: #f9fafb;
		border-color: #d1d5db;
		transform: scale(1.02);
	}

	.time-btn:hover {
		background: #eff6ff;
		border-color: #3b82f6;
		color: #1d4ed8;
	}

	.constraints-btn:hover {
		background: #fffbeb;
		border-color: #f59e0b;
		color: #92400e;
	}

	.location-btn:hover {
		background: #f0fdf4;
		border-color: #10b981;
		color: #047857;
	}
</style>
