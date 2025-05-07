<script lang="ts">
	import { onMount } from 'svelte';
	import { globalState } from '$lib/global.svelte';
	import type {
		TreeZipper,
		Capacity,
		CustomRecurrence,
		RecurrenceEnd
	} from '$lib/centralized/types';
	import { RecurrenceUnit, LocationType } from '$lib/centralized/types';
	import { addCapacity, updateComputedQuantities } from '$lib/centralized/capacity';
	import { Calendar, DatePicker, Button } from 'bits-ui';
	import { getLocalTimeZone, today } from '@internationalized/date';

	// Get current zipper from global state
	let currentZipper = $derived(globalState.currentZipper);

	// Get capacity inventory from current zipper
	let capacityInventory = $derived(
		currentZipper ? Array.from(currentZipper.zipperCurrent.nodeCapacities.values()) : []
	);

	// Local state for expanded capacity editing
	let openSettings = $state<string | null>(null);
	let capacityEntries = $state<Capacity[]>([]);

	// Initialize the capacity entries from the inventory
	$effect(() => {
		if (capacityInventory.length > 0) {
			capacityEntries = [...capacityInventory];
		} else if (capacityEntries.length === 0) {
			// Create a default entry if inventory is empty
			capacityEntries = [createDefaultCapacity()];
		}
	});

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
	function formatDateForInput(date: Date): string {
		return date.toISOString().split('T')[0];
	}

	// Format time for input
	function formatTimeForInput(date: Date): string {
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

	// Create a default capacity entry
	function createDefaultCapacity(): Capacity {
		const now = getTodayDate();
		const later = getOneHourLater();

		return {
			capacityId: crypto.randomUUID(),
			capacityName: '',
			quantity: 0,
			unit: '',
			shareDepth: 3,
			expanded: false,
			coordinates: {
				locationType: LocationType.Undefined,
				allDay: false,
				recurrence: 'Does not repeat',
				customRecurrence: {
					repeatEvery: 4,
					repeatUnit: RecurrenceUnit.Days,
					recurrenceEnd: { type: 'never' }
				},
				startDate: now,
				startTime: now,
				endDate: now,
				endTime: later,
				timeZone: '(GMT+02:00) Central European Time - Berlin'
			},
			maxDivisibility: {
				naturalDiv: 1,
				percentageDiv: 1.0
			},
			hiddenUntilRequestAccepted: false
		};
	}

	// Green color scale for depth
	const colors = ['#dcfce7', '#86efac', '#22c55e', '#15803d', '#14532d'];

	// Update capacity depth
	function setEntryDepth(entries: Capacity[], entryId: string, newDepth: number) {
		const idx = entries.findIndex((e) => e.capacityId === entryId);
		if (idx !== -1) {
			entries[idx] = { ...entries[idx], shareDepth: newDepth };
			saveCapacityToNode(entries[idx]);
		}
	}

	// Add a new capacity row
	function addCapacityRow() {
		const newCapacity = createDefaultCapacity();
		capacityEntries = [...capacityEntries, newCapacity];
	}

	// Remove a capacity row
	function removeCapacityRow(entryId: string) {
		if (capacityEntries.length === 1) return; // Always keep at least one row
		capacityEntries = capacityEntries.filter((e) => e.capacityId !== entryId);

		// Remove from node if it exists there
		if (currentZipper) {
			const updatedCapacities = new Map(currentZipper.zipperCurrent.nodeCapacities);
			updatedCapacities.delete(entryId);

			const updatedZipper = {
				...currentZipper,
				zipperCurrent: {
					...currentZipper.zipperCurrent,
					nodeCapacities: updatedCapacities
				}
			};

			globalState.currentZipper = updatedZipper;
		}
	}

	// Toggle expanded state for a capacity
	function toggleExpanded(entryId: string) {
		const idx = capacityEntries.findIndex((e) => e.capacityId === entryId);
		if (idx !== -1) {
			capacityEntries[idx].expanded = !capacityEntries[idx].expanded;
		}
	}

	// Save capacity to node when changed
	function saveCapacityToNode(capacity: Capacity) {
		if (!currentZipper) return;

		const updatedZipper = addCapacity(capacity, currentZipper);
		globalState.currentZipper = updateComputedQuantities(updatedZipper);

		globalState.showToast(`Capacity "${capacity.capacityName}" updated`, 'success');
	}

	// Handle capacity updates from form inputs
	function handleCapacityUpdate(capacity: Capacity) {
		const idx = capacityEntries.findIndex((e) => e.capacityId === capacity.capacityId);
		if (idx !== -1) {
			capacityEntries[idx] = capacity;
			saveCapacityToNode(capacity);
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

		// Create a copy with the parsed dates
		const updatedCapacity = {
			...capacity,
			coordinates: {
				...capacity.coordinates,
				startDate,
				startTime: startDate, // We'll just use the combined date
				endDate,
				endTime: endDate
			}
		};

		handleCapacityUpdate(updatedCapacity);
	}

	onMount(() => {
		// If no entries initially, create demo inventory entries
		if (capacityEntries.length === 0 && capacityInventory.length === 0) {
			const demoCapacity1 = createDefaultCapacity();
			demoCapacity1.capacityName = 'Reading Messages';
			demoCapacity1.quantity = 140;
			demoCapacity1.unit = 'Chars/Day';
			demoCapacity1.shareDepth = 5;

			const demoCapacity2 = createDefaultCapacity();
			demoCapacity2.capacityName = 'Potable Water';
			demoCapacity2.quantity = 50;
			demoCapacity2.unit = 'gallons';
			demoCapacity2.shareDepth = 5;

			const demoCapacity3 = createDefaultCapacity();
			demoCapacity3.capacityName = 'Rice';
			demoCapacity3.quantity = 20;
			demoCapacity3.unit = 'lbs';
			demoCapacity3.shareDepth = 5;

			capacityEntries = [demoCapacity1, demoCapacity2, demoCapacity3];

			// Save these to the node if we have one
			if (currentZipper) {
				capacityEntries.forEach((cap) => saveCapacityToNode(cap));
			}
		}
	});
</script>

<!-- <SixDegrees /> -->

<div class="inventory-list grid grid-cols-1 gap-3 p-2 md:grid-cols-2 lg:grid-cols-3">
	{#each capacityEntries as entry, i (entry.capacityId)}
		<div class="capacity-item">
			<div class="capacity-row flex items-center gap-2 rounded bg-white p-2 shadow-sm">
				<input
					type="text"
					class="capacity-input name min-w-0 flex-1"
					bind:value={entry.capacityName}
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
					{#each Array(5) as _, j}
						<span
							class="depth-dot {j < entry.shareDepth ? 'active' : ''}"
							style="background: {j < entry.shareDepth ? colors[j] : '#e5e7eb'}"
							onclick={() => {
								setEntryDepth(capacityEntries, entry.capacityId, j + 1);
							}}
						></span>
					{/each}
				</div>
				<button
					type="button"
					class="settings-btn ml-1"
					onclick={() => toggleExpanded(entry.capacityId)}
				>
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
					onclick={() => removeCapacityRow(entry.capacityId)}
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
											name="location-type-{entry.capacityId}"
											value={LocationType.Undefined}
											bind:group={entry.coordinates.locationType}
											class="mr-3"
											onchange={() => handleCapacityUpdate(entry)}
										/>
										<span class="text-sm text-gray-600">Undefined</span>
									</label>
									<label class="inline-flex items-center">
										<input
											type="radio"
											name="location-type-{entry.capacityId}"
											value={LocationType.LiveLocation}
											bind:group={entry.coordinates.locationType}
											class="mr-3"
											onchange={() => handleCapacityUpdate(entry)}
										/>
										<span class="text-sm text-gray-600">Live location</span>
									</label>
									<label class="inline-flex items-center">
										<input
											type="radio"
											name="location-type-{entry.capacityId}"
											value={LocationType.Specific}
											bind:group={entry.coordinates.locationType}
											class="mr-3"
											onchange={() => handleCapacityUpdate(entry)}
										/>
										<span class="text-sm text-gray-600">Specific coordinates</span>
									</label>
								</div>
							</div>

							{#if entry.coordinates.locationType === LocationType.Specific}
								<!-- Date picker with Bits UI -->
								<div class="date-time-section mb-6 ml-2">
									{#if true}
										{@const startDateStr = formatDateForInput(entry.coordinates.startDate)}
										{@const startTimeStr = formatTimeForInput(entry.coordinates.startTime)}
										{@const endDateStr = formatDateForInput(entry.coordinates.endDate)}
										{@const endTimeStr = formatTimeForInput(entry.coordinates.endTime)}

										<!-- All day checkbox -->
										<div class="mb-4 ml-1">
											<label class="inline-flex items-center">
												<input
													type="checkbox"
													bind:checked={entry.coordinates.allDay}
													class="mr-3 h-4 w-4"
													onchange={() => handleCapacityUpdate(entry)}
												/>
												<span class="text-sm text-gray-600">All day</span>
											</label>
										</div>

										<!-- Calendar picker using Bits UI -->
										<div class="mb-4 flex flex-col gap-6">
											<div>
												<h5 class="mb-2 text-sm text-gray-600">Start Date</h5>
												<input
													type="date"
													class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
													value={startDateStr}
													onchange={(e) => {
														const newDate = e.currentTarget.value;
														prepareCapacityForSave(
															entry,
															newDate,
															startTimeStr,
															endDateStr,
															endTimeStr
														);
													}}
												/>
											</div>

											<div>
												<h5 class="mb-2 text-sm text-gray-600">End Date</h5>
												<input
													type="date"
													class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
													value={endDateStr}
													onchange={(e) => {
														const newDate = e.currentTarget.value;
														prepareCapacityForSave(
															entry,
															startDateStr,
															startTimeStr,
															newDate,
															endTimeStr
														);
													}}
												/>
											</div>
										</div>

										<!-- Times (only shown if not all day) -->
										{#if !entry.coordinates.allDay}
											<div class="mb-4 flex flex-col gap-4 md:flex-row">
												<div class="md:w-1/2">
													<input
														type="time"
														class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
														value={startTimeStr}
														onchange={(e) => {
															const newTime = e.currentTarget.value;
															prepareCapacityForSave(
																entry,
																startDateStr,
																newTime,
																endDateStr,
																endTimeStr
															);
														}}
													/>
												</div>

												<div class="flex items-center md:w-1/2">
													<span class="mx-2 hidden text-gray-400 md:inline">to</span>
													<input
														type="time"
														class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
														value={endTimeStr}
														onchange={(e) => {
															const newTime = e.currentTarget.value;
															prepareCapacityForSave(
																entry,
																startDateStr,
																startTimeStr,
																endDateStr,
																newTime
															);
														}}
													/>
												</div>
											</div>

											<!-- Time zone -->
											<div class="mb-4">
												<button
													class="text-sm text-blue-600 hover:text-blue-800 focus:outline-none"
												>
													Time zone
												</button>
												<div class="mt-2">
													<select
														class="capacity-select w-full rounded-md bg-gray-100 px-3 py-2"
														bind:value={entry.coordinates.timeZone}
														onchange={() => handleCapacityUpdate(entry)}
													>
														{#each timeZones as zone}
															<option value={zone}>{zone}</option>
														{/each}
													</select>
												</div>
											</div>
										{/if}
									{/if}

									<!-- Recurrence dropdown styled like Google Calendar -->
									<div class="mb-6">
										<div class="relative w-full md:w-auto">
											<select
												class="capacity-select w-full appearance-none rounded-md bg-gray-100 px-3 py-2"
												bind:value={entry.coordinates.recurrence}
												onchange={() => handleCapacityUpdate(entry)}
											>
												{#each recurrenceOptions as option}
													<option value={option}>{option}</option>
												{/each}
											</select>
										</div>
									</div>

									<!-- Custom recurrence options -->
									{#if entry.coordinates.recurrence === 'Custom...' && entry.coordinates.customRecurrence}
										<div class="custom-recurrence mt-4 rounded-md bg-gray-50 p-6">
											{#if true}
												{@const repeatEvery = entry.coordinates.customRecurrence.repeatEvery}
												{@const repeatUnit = entry.coordinates.customRecurrence.repeatUnit}
												{@const recurrenceEnd = entry.coordinates.customRecurrence.recurrenceEnd}

												<div class="mb-6 flex items-center">
													<span class="mr-3 text-sm font-medium text-gray-600">Repeat every</span>
													<input
														type="number"
														min="1"
														class="capacity-input qty w-16 text-right"
														value={repeatEvery}
														onchange={(e) => {
															if (entry.coordinates.customRecurrence) {
																entry.coordinates.customRecurrence.repeatEvery = parseInt(
																	e.currentTarget.value
																);
																handleCapacityUpdate(entry);
															}
														}}
													/>
													<select
														class="capacity-select ml-3 w-28"
														value={repeatUnit}
														onchange={(e) => {
															if (entry.coordinates.customRecurrence) {
																entry.coordinates.customRecurrence.repeatUnit = e.currentTarget
																	.value as RecurrenceUnit;
																handleCapacityUpdate(entry);
															}
														}}
													>
														<option value={RecurrenceUnit.Days}>days</option>
														<option value={RecurrenceUnit.Weeks}>weeks</option>
														<option value={RecurrenceUnit.Months}>months</option>
														<option value={RecurrenceUnit.Years}>years</option>
													</select>
												</div>

												<div class="ends-section ml-2">
													<span class="mb-4 block text-sm font-medium text-gray-600">Ends</span>

													<div class="radio-options space-y-4">
														<label class="inline-flex items-center">
															<input
																type="radio"
																name="ends-{entry.capacityId}"
																value="never"
																checked={recurrenceEnd.type === 'never'}
																onchange={() => {
																	if (entry.coordinates.customRecurrence) {
																		entry.coordinates.customRecurrence.recurrenceEnd = {
																			type: 'never'
																		};
																		handleCapacityUpdate(entry);
																	}
																}}
																class="mr-3"
															/>
															<span class="text-sm text-gray-600">Never</span>
														</label>

														<div class="flex items-center">
															<label class="inline-flex items-center">
																<input
																	type="radio"
																	name="ends-{entry.capacityId}"
																	value="endsOn"
																	checked={recurrenceEnd.type === 'endsOn'}
																	onchange={() => {
																		if (entry.coordinates.customRecurrence) {
																			entry.coordinates.customRecurrence.recurrenceEnd = {
																				type: 'endsOn',
																				date: new Date()
																			};
																			handleCapacityUpdate(entry);
																		}
																	}}
																	class="mr-3"
																/>
																<span class="text-sm text-gray-600">On</span>
															</label>
															{#if recurrenceEnd.type === 'endsOn'}
																<div class="ml-6">
																	<input
																		type="date"
																		class="capacity-input w-40 rounded-md bg-gray-100 px-3 py-2"
																		value={formatDateForInput(recurrenceEnd.date)}
																		onchange={(e) => {
																			if (entry.coordinates.customRecurrence) {
																				entry.coordinates.customRecurrence.recurrenceEnd = {
																					type: 'endsOn',
																					date: new Date(e.currentTarget.value)
																				};
																				handleCapacityUpdate(entry);
																			}
																		}}
																	/>
																</div>
															{/if}
														</div>

														<div class="flex items-center">
															<label class="inline-flex items-center">
																<input
																	type="radio"
																	name="ends-{entry.capacityId}"
																	value="endsAfter"
																	checked={recurrenceEnd.type === 'endsAfter'}
																	onchange={() => {
																		if (entry.coordinates.customRecurrence) {
																			entry.coordinates.customRecurrence.recurrenceEnd = {
																				type: 'endsAfter',
																				count: 5
																			};
																			handleCapacityUpdate(entry);
																		}
																	}}
																	class="mr-3"
																/>
																<span class="text-sm text-gray-600">After</span>
															</label>
															{#if recurrenceEnd.type === 'endsAfter'}
																<div class="ml-6 flex items-center">
																	<input
																		type="number"
																		min="1"
																		class="capacity-input qty w-16 text-right"
																		value={recurrenceEnd.count}
																		onchange={(e) => {
																			if (entry.coordinates.customRecurrence) {
																				entry.coordinates.customRecurrence.recurrenceEnd = {
																					type: 'endsAfter',
																					count: parseInt(e.currentTarget.value)
																				};
																				handleCapacityUpdate(entry);
																			}
																		}}
																	/>
																	<span class="ml-2 text-sm text-gray-600">occurrences</span>
																</div>
															{/if}
														</div>
													</div>
												</div>
											{/if}
										</div>
									{/if}
								</div>
							{/if}
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
										bind:value={entry.maxDivisibility.naturalDiv}
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
										bind:value={entry.maxDivisibility.percentageDiv}
										placeholder="Percentage (0-1)"
										onblur={() => handleCapacityUpdate(entry)}
									/>
								</div>
							</div>
						</div>

						<div class="hidden-option ml-2">
							<label class="inline-flex items-center">
								<input
									type="checkbox"
									bind:checked={entry.hiddenUntilRequestAccepted}
									class="mr-3 h-4 w-4"
									onchange={() => handleCapacityUpdate(entry)}
								/>
								<span class="text-sm font-medium text-gray-600">Hidden till Request Accepted</span>
							</label>
						</div>
					</div>
				</div>
			{/if}
		</div>
	{/each}
	<Button.Root onclick={addCapacityRow} class="add-btn mx-auto my-2 h-10 w-10">+</Button.Root>
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
