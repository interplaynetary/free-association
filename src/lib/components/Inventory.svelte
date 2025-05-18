<script lang="ts">
	import { onMount } from 'svelte';
	import { globalState } from '$lib/global.svelte';
	import type { Capacity, CapacityShare } from '$lib/protocol/protocol';
	import { Calendar, DatePicker, Button } from 'bits-ui';
	import { getLocalTimeZone, today } from '@internationalized/date';

	// Local state for capacity inventory
	let capacityEntries = $state<Capacity[]>([]);

	// Local state for expanded capacity editing
	let openSettings = $state<string | null>(null);

	// Initialize from API when component mounts
	onMount(async () => {
		await loadCapacities();
	});

	// Load capacities from API
	async function loadCapacities() {
		try {
			const nodeId = globalState.currentNodeId;
			if (!nodeId) {
				capacityEntries = [];
				return;
			}

			const response = await fetch(`/api/${nodeId}/capacities`);
			if (response.ok) {
				const data = await response.json();
				if (data.success && data.capacities) {
					capacityEntries = data.capacities.map((cap: any) => ({
						id: cap.id,
						name: cap.name,
						quantity: cap.quantity,
						unit: cap.unit,
						share_depth: cap.share_depth || 3,
						expanded: false,
						location_type: cap.location_type || 'Undefined',
						all_day: cap.all_day || false,
						recurrence: cap.recurrence,
						custom_recurrence_repeat_every: cap.custom_recurrence_repeat_every,
						custom_recurrence_repeat_unit: cap.custom_recurrence_repeat_unit,
						custom_recurrence_end_type: cap.custom_recurrence_end_type,
						custom_recurrence_end_value: cap.custom_recurrence_end_value,
						start_date: cap.start_date,
						start_time: cap.start_time,
						end_date: cap.end_date,
						end_time: cap.end_time,
						time_zone: cap.time_zone || getLocalTimeZone(),
						max_natural_div: cap.max_natural_div || 1,
						max_percentage_div: cap.max_percentage_div || 1.0,
						hidden_until_request_accepted: cap.hidden_until_request_accepted || false,
						owner_id: nodeId,
						shares: cap.shares || []
					}));
				}
			}
		} catch (error) {
			console.error('Error loading capacities:', error);
			globalState.showToast('Error loading capacities', 'error');
		}
	}

	// Save capacity to API
	async function saveCapacity(capacity: Capacity) {
		try {
			const nodeId = globalState.currentNodeId;
			if (!nodeId) return false;

			const response = await fetch(`/api/${nodeId}/capacities/${capacity.id}`, {
				method: 'PUT',
				headers: {
					'Content-Type': 'application/json'
				},
				body: JSON.stringify(capacity)
			});

			if (response.ok) {
				const data = await response.json();
				if (data.success) {
					globalState.showToast(`Capacity "${capacity.name}" updated`, 'success');
					return true;
				} else {
					globalState.showToast(data.message || 'Error saving capacity', 'error');
					return false;
				}
			} else {
				globalState.showToast('Error saving capacity to server', 'error');
				return false;
			}
		} catch (error) {
			console.error('Error saving capacity:', error);
			globalState.showToast('Error saving capacity', 'error');
			return false;
		}
	}

	// Delete capacity from API
	async function deleteCapacity(capacityId: string) {
		try {
			const nodeId = globalState.currentNodeId;
			if (!nodeId) return false;

			const response = await fetch(`/api/${nodeId}/capacities/${capacityId}`, {
				method: 'DELETE'
			});

			if (response.ok) {
				const data = await response.json();
				if (data.success) {
					globalState.showToast('Capacity deleted', 'success');
					return true;
				} else {
					globalState.showToast(data.message || 'Error deleting capacity', 'error');
					return false;
				}
			} else {
				globalState.showToast('Error deleting capacity from server', 'error');
				return false;
			}
		} catch (error) {
			console.error('Error deleting capacity:', error);
			globalState.showToast('Error deleting capacity', 'error');
			return false;
		}
	}

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

	// Time zone options
	const timeZones = [
		'(GMT-07:00) Pacific Time - Los Angeles',
		'(GMT-05:00) Eastern Time - New York',
		'(GMT+00:00) Greenwich Mean Time - London',
		'(GMT+01:00) Central European Time - Paris',
		'(GMT+02:00) Central European Time - Berlin',
		'(GMT+08:00) China Standard Time - Beijing',
		'(GMT+09:00) Japan Standard Time - Tokyo'
	];

	// Helper functions for date/time handling
	function getTodayDate(): Date {
		return new Date();
	}

	function getOneHourLater(): Date {
		const date = new Date();
		date.setHours(date.getHours() + 1);
		return date;
	}

	// Format date for input
	function formatDateForInput(date: Date | undefined): string {
		if (!date) return '';
		return date.toISOString().split('T')[0];
	}

	// Format time for input
	function formatTimeForInput(date: Date | undefined): string {
		if (!date) return '';
		return `${String(date.getHours()).padStart(2, '0')}:${String(date.getMinutes()).padStart(2, '0')}`;
	}

	// Parse date and time from input
	function parseDateTime(dateStr: string, timeStr: string): Date {
		const result = new Date(dateStr);
		if (timeStr) {
			const [hours, minutes] = timeStr.split(':').map(Number);
			result.setHours(hours, minutes);
		}
		return result;
	}

	// Create a new capacity
	function createDefaultCapacity(): Capacity {
		const now = new Date().toISOString();
		return {
			id: crypto.randomUUID(),
			name: '',
			quantity: 0,
			unit: '',
			share_depth: 3,
			expanded: false,
			location_type: 'Undefined',
			all_day: false,
			start_date: now,
			start_time: now,
			end_date: now,
			end_time: now,
			time_zone: getLocalTimeZone(),
			max_natural_div: 1,
			max_percentage_div: 1.0,
			hidden_until_request_accepted: false,
			owner_id: globalState.currentNodeId || '',
			shares: []
		};
	}

	// Green color scale for depth
	const colors = ['#22c55e', '#86efac', '#dcfce7'];

	// Update capacity depth
	function setEntryDepth(entries: Capacity[], entryId: string, newDepth: number) {
		const idx = entries.findIndex((e) => e.id === entryId);
		if (idx !== -1) {
			entries[idx] = { ...entries[idx], share_depth: newDepth };
			saveCapacity(entries[idx]);
		}
	}

	// Add a new capacity row
	async function addCapacityRow() {
		const newCapacity = createDefaultCapacity();
		try {
			const nodeId = globalState.currentNodeId;
			if (!nodeId) return;

			const response = await fetch(`/api/${nodeId}/capacities`, {
				method: 'POST',
				headers: {
					'Content-Type': 'application/json'
				},
				body: JSON.stringify(newCapacity)
			});

			if (response.ok) {
				const data = await response.json();
				if (data.success) {
					capacityEntries = [...capacityEntries, data.capacity];
					globalState.showToast('New capacity added', 'success');
				}
			}
		} catch (error) {
			console.error('Error adding capacity:', error);
			globalState.showToast('Error adding capacity', 'error');
		}
	}

	// Remove a capacity row
	async function removeCapacityRow(entryId: string) {
		if (capacityEntries.length === 1) return;

		const success = await deleteCapacity(entryId);
		if (success) {
			capacityEntries = capacityEntries.filter((e) => e.id !== entryId);
		}
	}

	// Toggle expanded state for a capacity
	function toggleExpanded(entryId: string) {
		const idx = capacityEntries.findIndex((e) => e.id === entryId);
		if (idx !== -1) {
			capacityEntries[idx].expanded = !capacityEntries[idx].expanded;
		}
	}

	// Handle capacity updates from form inputs
	function handleCapacityUpdate(capacity: Capacity) {
		const idx = capacityEntries.findIndex((e) => e.id === capacity.id);
		if (idx !== -1) {
			capacityEntries[idx] = capacity;
			saveCapacity(capacity);
		}
	}

	// Convert form string dates to Date objects for saving
	function prepareCapacityForSave(
		capacity: Capacity,
		startDateStr: string,
		startTimeStr: string,
		endDateStr: string,
		endTimeStr: string
	) {
		// Parse date and time strings to Date objects
		const startDate = parseDateTime(startDateStr, startTimeStr);
		const endDate = parseDateTime(endDateStr, endTimeStr);

		// Create a copy with the parsed dates as ISO strings
		const updatedCapacity = {
			...capacity,
			location_type: capacity.location_type,
			all_day: capacity.all_day,
			recurrence: capacity.recurrence,
			custom_recurrence_repeat_every: capacity.custom_recurrence_repeat_every,
			custom_recurrence_repeat_unit: capacity.custom_recurrence_repeat_unit,
			custom_recurrence_end_type: capacity.custom_recurrence_end_type,
			custom_recurrence_end_value: capacity.custom_recurrence_end_value,
			start_date: startDate.toISOString(),
			start_time: startDate.toISOString(),
			end_date: endDate.toISOString(),
			end_time: endDate.toISOString(),
			time_zone: capacity.time_zone,
			max_natural_div: capacity.max_natural_div,
			max_percentage_div: capacity.max_percentage_div,
			hidden_until_request_accepted: capacity.hidden_until_request_accepted,
			shares: capacity.shares
		};

		handleCapacityUpdate(updatedCapacity);
	}
</script>

<!-- <SixDegrees /> -->

<div class="inventory-list grid grid-cols-1 gap-3 p-2 md:grid-cols-2 lg:grid-cols-3">
	{#each capacityEntries as entry, i (entry.id)}
		<div class="capacity-item">
			<div class="capacity-row flex items-center gap-2 rounded bg-white p-2 shadow-sm">
				<input
					type="text"
					class="capacity-input name min-w-0 flex-1"
					bind:value={entry.name}
					placeholder="Name"
					onblur={() => handleCapacityUpdate(entry)}
				/>
				<input
					type="number"
					class="capacity-input qty w-16 text-right"
					min="0"
					step="0.01"
					bind:value={entry.quantity}
					placeholder="Qty"
					onblur={() => handleCapacityUpdate(entry)}
				/>
				<input
					type="text"
					class="capacity-input unit w-20"
					bind:value={entry.unit}
					placeholder="Unit"
					onblur={() => handleCapacityUpdate(entry)}
				/>
				<div class="depth-bar ml-2 flex gap-1">
					{#each Array(3) as _, j}
						<span
							class="depth-dot {j < entry.share_depth ? 'active' : ''}"
							style="background: {j < entry.share_depth ? colors[j] : '#e5e7eb'}"
							onclick={() => {
								setEntryDepth(capacityEntries, entry.id, j + 1);
							}}
						></span>
					{/each}
				</div>
				<button type="button" class="settings-btn ml-1" onclick={() => toggleExpanded(entry.id)}>
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
				<button
					type="button"
					class="remove-btn ml-1"
					onclick={() => removeCapacityRow(entry.id)}
					disabled={capacityEntries.length === 1}>×</button
				>
			</div>

			{#if entry.expanded}
				<div class="expanded-settings mt-2 rounded-md bg-white shadow-sm">
					<div class="settings-content mx-8 my-8">
						<div class="space-time-section mb-8">
							<h4 class="mb-4 text-sm font-medium text-gray-700">Space-time coordinates</h4>

							<div class="space-time-options mb-6 ml-2">
								<div class="flex flex-wrap gap-x-8 gap-y-3">
									<label class="inline-flex items-center">
										<input
											type="radio"
											name="location-type-{entry.id}"
											value="Undefined"
											bind:group={entry.location_type}
											class="mr-3"
											onchange={() => handleCapacityUpdate(entry)}
										/>
										<span class="text-sm text-gray-600">Undefined</span>
									</label>
									<label class="inline-flex items-center">
										<input
											type="radio"
											name="location-type-{entry.id}"
											value="LiveLocation"
											bind:group={entry.location_type}
											class="mr-3"
											onchange={() => handleCapacityUpdate(entry)}
										/>
										<span class="text-sm text-gray-600">Live location</span>
									</label>
									<label class="inline-flex items-center">
										<input
											type="radio"
											name="location-type-{entry.id}"
											value="Specific"
											bind:group={entry.location_type}
											class="mr-3"
											onchange={() => handleCapacityUpdate(entry)}
										/>
										<span class="text-sm text-gray-600">Specific coordinates</span>
									</label>
								</div>
							</div>

							{#if entry.location_type === 'Specific'}
								<div class="date-time-section mb-6 ml-2">
									<div class="mb-4 ml-1">
										<label class="inline-flex items-center">
											<input
												type="checkbox"
												bind:checked={entry.all_day}
												class="mr-3 h-4 w-4"
												onchange={() => handleCapacityUpdate(entry)}
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
												bind:value={entry.start_date}
												onchange={() => handleCapacityUpdate(entry)}
											/>
										</div>

										<div>
											<h5 class="mb-2 text-sm text-gray-600">End Date</h5>
											<input
												type="date"
												class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
												bind:value={entry.end_date}
												onchange={() => handleCapacityUpdate(entry)}
											/>
										</div>
									</div>

									{#if !entry.all_day}
										<div class="mb-4 flex flex-col gap-4 md:flex-row">
											<div class="md:w-1/2">
												<input
													type="time"
													class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
													bind:value={entry.start_time}
													onchange={() => handleCapacityUpdate(entry)}
												/>
											</div>

											<div class="flex items-center md:w-1/2">
												<span class="mx-2 hidden text-gray-400 md:inline">to</span>
												<input
													type="time"
													class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
													bind:value={entry.end_time}
													onchange={() => handleCapacityUpdate(entry)}
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
													bind:value={entry.time_zone}
													onchange={() => handleCapacityUpdate(entry)}
												/>
											</div>
										</div>
									{/if}
								</div>
							{/if}
						</div>

						<div class="hidden-option ml-2">
							<label class="inline-flex items-center">
								<input
									type="checkbox"
									bind:checked={entry.hidden_until_request_accepted}
									class="mr-3 h-4 w-4"
									onchange={() => handleCapacityUpdate(entry)}
								/>
								<span class="text-sm font-medium text-gray-600">Hidden till Request Accepted</span>
							</label>
						</div>

						<div class="max-divisibility-section mb-6 ml-2">
							<h4 class="mb-4 text-sm font-medium text-gray-700">Max-divisibility</h4>

							<div class="grid grid-cols-2 gap-8">
								<div>
									<input
										type="number"
										min="1"
										step="1"
										class="capacity-input qty w-full text-right"
										bind:value={entry.max_natural_div}
										placeholder="Natural"
										onblur={() => handleCapacityUpdate(entry)}
									/>
								</div>
								<div>
									<input
										type="number"
										min="0"
										max="1"
										step="0.01"
										class="capacity-input qty w-full text-right"
										bind:value={entry.max_percentage_div}
										placeholder="Percentage (0-1)"
										onblur={() => handleCapacityUpdate(entry)}
									/>
								</div>
							</div>
						</div>
					</div>
				</div>
			{/if}
		</div>
	{/each}
	<button type="button" class="add-btn mx-auto my-2 h-10 w-10" onclick={addCapacityRow}>+</button>
</div>

<style>
	:global(body) {
		font-family: 'Inter', system-ui, sans-serif;
		background: #f7fafc;
	}

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

	/* Depth indicator dots */
	.depth-dot {
		width: 15px;
		height: 15px;
		border-radius: 50%;
		background: #e5e7eb;
		display: inline-block;
		margin-right: 0;
		transition: all 0.2s ease;
		cursor: pointer;
		border: 1px solid #e5e7eb;
	}
	.depth-dot.active {
		border: 1px solid #16a34a;
		box-shadow: 0 0 0 2px rgba(187, 247, 208, 0.5);
	}
	.depth-dot:hover {
		border: 1px solid #15803d;
		transform: scale(1.05);
	}

	/* Add button styles - override Bits UI Button with this class */
	.add-btn {
		background: #f0fdf4;
		color: #16a34a;
		border: none;
		border-radius: 50%;
		font-size: 1.3em;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
		display: flex;
		align-items: center;
		justify-content: center;
	}
	.add-btn:hover {
		background: #dcfce7;
		color: #15803d;
		transform: scale(1.05);
		box-shadow: 0 2px 5px rgba(0, 0, 0, 0.08);
	}

	/* Icon buttons */
	.remove-btn,
	.settings-btn {
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

	.settings-btn {
		font-size: 1em;
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

	.settings-btn:hover {
		background: #f3f4f6;
		color: #4b5563;
		transform: scale(1.05);
	}

	/* Item container */
	.capacity-item {
		display: flex;
		flex-direction: column;
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
</style>
