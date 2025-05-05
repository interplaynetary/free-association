<script lang="ts">
	import { onMount } from 'svelte';

	// TODO:

	interface CapacityEntry {
		id: string;
		name: string;
		quantity: number;
		unit: string;
		depth: number;
		expanded: boolean;
		spaceTimeCoordinates: {
			type: 'undefined' | 'live-location' | 'specific';
			allDay: boolean;
			recurrence: string;
			customRecurrence: {
				repeatEvery: number;
				repeatUnit: 'days' | 'weeks' | 'months' | 'years';
				ends: 'never' | 'on' | 'after';
				endDate?: string;
				endAfter?: number;
			};
			startDate: string;
			startTime: string;
			endDate: string;
			endTime: string;
			timeZone: string;
		};
		maxDivisibility: {
			natural: number;
			percentage: number;
		};
		hiddenUntilRequestAccepted: boolean;
	}

	let capacityEntries = $state<CapacityEntry[]>([createCapacityEntry()]);
	let openSettings = $state<string | null>(null);

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

	// Get today's date in YYYY-MM-DD format
	function getTodayDate(): string {
		const today = new Date();
		return today.toISOString().split('T')[0];
	}

	// Get current time in HH:MM format
	function getCurrentTime(): string {
		const now = new Date();
		return `${String(now.getHours()).padStart(2, '0')}:${String(now.getMinutes()).padStart(2, '0')}`;
	}

	// Create a new capacity entry row
	function createCapacityEntry(): CapacityEntry {
		const today = getTodayDate();
		const currentTime = getCurrentTime();

		// Set end time 1 hour after current time
		const endTimeDate = new Date();
		endTimeDate.setHours(endTimeDate.getHours() + 1);
		const endTime = `${String(endTimeDate.getHours()).padStart(2, '0')}:${String(endTimeDate.getMinutes()).padStart(2, '0')}`;

		return {
			id: crypto.randomUUID(),
			name: '',
			quantity: 0,
			unit: '',
			depth: 3,
			expanded: false,
			spaceTimeCoordinates: {
				type: 'undefined',
				allDay: false,
				recurrence: 'Does not repeat',
				customRecurrence: {
					repeatEvery: 4,
					repeatUnit: 'days',
					ends: 'never'
				},
				startDate: today,
				startTime: currentTime,
				endDate: today,
				endTime: endTime,
				timeZone: '(GMT+02:00) Central European Time - Berlin'
			},
			maxDivisibility: {
				natural: 1,
				percentage: 100
			},
			hiddenUntilRequestAccepted: false
		};
	}

	// Green color scale for depth
	const colors = ['#dcfce7', '#86efac', '#22c55e', '#15803d', '#14532d'];

	function setEntryDepth(entries: CapacityEntry[], entryId: string, newDepth: number) {
		const idx = entries.findIndex((e) => e.id === entryId);
		if (idx !== -1) {
			entries[idx] = { ...entries[idx], depth: newDepth };
		}
	}

	function addCapacityRow() {
		capacityEntries = [...capacityEntries, createCapacityEntry()];
	}

	function removeCapacityRow(entryId: string) {
		if (capacityEntries.length === 1) return; // Always keep at least one row
		capacityEntries = capacityEntries.filter((e) => e.id !== entryId);
	}

	function toggleExpanded(entryId: string) {
		const idx = capacityEntries.findIndex((e) => e.id === entryId);
		if (idx !== -1) {
			capacityEntries[idx].expanded = !capacityEntries[idx].expanded;
		}
	}

	onMount(() => {
		// Initialize with demo inventory entries
		const baseEntry = createCapacityEntry();
		const demoEntries: CapacityEntry[] = [
			// sample entries with expanded properties
			{
				id: crypto.randomUUID(),
				name: 'Reading Messages',
				quantity: 140,
				unit: 'Chars/Day',
				depth: 5,
				expanded: false,
				spaceTimeCoordinates: {
					type: 'undefined' as const,
					allDay: false,
					recurrence: 'Does not repeat',
					customRecurrence: baseEntry.spaceTimeCoordinates.customRecurrence,
					startDate: getTodayDate(),
					startTime: getCurrentTime(),
					endDate: getTodayDate(),
					endTime: getCurrentTime(),
					timeZone: '(GMT+02:00) Central European Time - Berlin'
				},
				maxDivisibility: {
					natural: 1,
					percentage: 100
				},
				hiddenUntilRequestAccepted: false
			},
			{
				id: crypto.randomUUID(),
				name: 'Potable Water',
				quantity: 50,
				unit: 'gallons',
				depth: 5,
				expanded: false,
				spaceTimeCoordinates: { ...baseEntry.spaceTimeCoordinates },
				maxDivisibility: { ...baseEntry.maxDivisibility },
				hiddenUntilRequestAccepted: false
			},
			// Add minimum required properties to all other entries
			{
				id: crypto.randomUUID(),
				name: 'Rice',
				quantity: 20,
				unit: 'lbs',
				depth: 5,
				expanded: false,
				spaceTimeCoordinates: { ...baseEntry.spaceTimeCoordinates },
				maxDivisibility: { ...baseEntry.maxDivisibility },
				hiddenUntilRequestAccepted: false
			}
			// Continue with the rest of the entries following this pattern
			// (abbreviated for conciseness)
		];

		capacityEntries = demoEntries;
	});
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
				/>
				<input
					type="number"
					class="capacity-input qty w-16 text-right"
					min="0"
					step="0.01"
					bind:value={entry.quantity}
					placeholder="Qty"
				/>
				<input
					type="text"
					class="capacity-input unit w-20"
					bind:value={entry.unit}
					placeholder="Unit"
				/>
				<div class="depth-bar ml-2 flex gap-1">
					{#each Array(5) as _, j}
						<span
							class="depth-dot {j < entry.depth ? 'active' : ''}"
							style="background: {j < entry.depth ? colors[j] : '#e5e7eb'}"
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
											value="undefined"
											bind:group={entry.spaceTimeCoordinates.type}
											class="mr-3"
										/>
										<span class="text-sm text-gray-600">Undefined</span>
									</label>
									<label class="inline-flex items-center">
										<input
											type="radio"
											name="location-type-{entry.id}"
											value="live-location"
											bind:group={entry.spaceTimeCoordinates.type}
											class="mr-3"
										/>
										<span class="text-sm text-gray-600">Live location</span>
									</label>
									<label class="inline-flex items-center">
										<input
											type="radio"
											name="location-type-{entry.id}"
											value="specific"
											bind:group={entry.spaceTimeCoordinates.type}
											class="mr-3"
										/>
										<span class="text-sm text-gray-600">Specific coordinates</span>
									</label>
								</div>
							</div>

							{#if entry.spaceTimeCoordinates.type === 'specific'}
								<!-- Google Calendar style date/time picker -->
								<div class="date-time-section mb-6 ml-2">
									<!-- First row: Dates -->
									<div class="mb-4 flex flex-col gap-4 md:flex-row">
										<div class="md:w-1/2">
											<input
												type="date"
												class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
												bind:value={entry.spaceTimeCoordinates.startDate}
											/>
										</div>

										<div class="md:w-1/2">
											<input
												type="date"
												class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
												bind:value={entry.spaceTimeCoordinates.endDate}
											/>
										</div>
									</div>

									<!-- All day checkbox -->
									<div class="mb-4 ml-1">
										<label class="inline-flex items-center">
											<input
												type="checkbox"
												bind:checked={entry.spaceTimeCoordinates.allDay}
												class="mr-3 h-4 w-4"
											/>
											<span class="text-sm text-gray-600">All day</span>
										</label>
									</div>

									<!-- Times (only shown if not all day) -->
									{#if !entry.spaceTimeCoordinates.allDay}
										<div class="mb-4 flex flex-col gap-4 md:flex-row">
											<div class="md:w-1/2">
												<input
													type="time"
													class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
													bind:value={entry.spaceTimeCoordinates.startTime}
												/>
											</div>

											<div class="flex items-center md:w-1/2">
												<span class="mx-2 hidden text-gray-400 md:inline">to</span>
												<input
													type="time"
													class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
													bind:value={entry.spaceTimeCoordinates.endTime}
												/>
											</div>
										</div>

										<!-- Time zone -->
										<div class="mb-4">
											<button class="text-sm text-blue-600 hover:text-blue-800 focus:outline-none">
												Time zone
											</button>
											<div class="mt-2">
												<select
													class="capacity-select w-full rounded-md bg-gray-100 px-3 py-2"
													bind:value={entry.spaceTimeCoordinates.timeZone}
												>
													{#each timeZones as zone}
														<option value={zone}>{zone}</option>
													{/each}
												</select>
											</div>
										</div>
									{/if}

									<!-- Recurrence dropdown styled like Google Calendar -->
									<div class="mb-6">
										<div class="relative w-full md:w-auto">
											<select
												class="capacity-select w-full appearance-none rounded-md bg-gray-100 px-3 py-2"
												bind:value={entry.spaceTimeCoordinates.recurrence}
											>
												{#each recurrenceOptions as option}
													<option value={option}>{option}</option>
												{/each}
											</select>
										</div>
									</div>

									<!-- Custom recurrence options -->
									{#if entry.spaceTimeCoordinates.recurrence === 'Custom...'}
										<div class="custom-recurrence mt-4 rounded-md bg-gray-50 p-6">
											<div class="mb-6 flex items-center">
												<span class="mr-3 text-sm font-medium text-gray-600">Repeat every</span>
												<input
													type="number"
													min="1"
													class="capacity-input qty w-16 text-right"
													bind:value={entry.spaceTimeCoordinates.customRecurrence.repeatEvery}
												/>
												<select
													class="capacity-select ml-3 w-28"
													bind:value={entry.spaceTimeCoordinates.customRecurrence.repeatUnit}
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
															name="ends-{entry.id}"
															value="never"
															bind:group={entry.spaceTimeCoordinates.customRecurrence.ends}
															class="mr-3"
														/>
														<span class="text-sm text-gray-600">Never</span>
													</label>

													<div class="flex items-center">
														<label class="inline-flex items-center">
															<input
																type="radio"
																name="ends-{entry.id}"
																value="on"
																bind:group={entry.spaceTimeCoordinates.customRecurrence.ends}
																class="mr-3"
															/>
															<span class="text-sm text-gray-600">On</span>
														</label>
														{#if entry.spaceTimeCoordinates.customRecurrence.ends === 'on'}
															<div class="ml-6">
																<input
																	type="date"
																	class="capacity-input w-40"
																	bind:value={entry.spaceTimeCoordinates.customRecurrence.endDate}
																/>
															</div>
														{/if}
													</div>

													<div class="flex items-center">
														<label class="inline-flex items-center">
															<input
																type="radio"
																name="ends-{entry.id}"
																value="after"
																bind:group={entry.spaceTimeCoordinates.customRecurrence.ends}
																class="mr-3"
															/>
															<span class="text-sm text-gray-600">After</span>
														</label>
														{#if entry.spaceTimeCoordinates.customRecurrence.ends === 'after'}
															<div class="ml-6 flex items-center">
																<input
																	type="number"
																	min="1"
																	class="capacity-input qty w-16 text-right"
																	bind:value={entry.spaceTimeCoordinates.customRecurrence.endAfter}
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

						<div class="max-divisibility-section mb-6 ml-2">
							<h4 class="mb-4 text-sm font-medium text-gray-700">Max-divisibility</h4>

							<div class="grid grid-cols-2 gap-8">
								<div>
									<input
										type="number"
										min="1"
										step="1"
										class="capacity-input qty w-full text-right"
										bind:value={entry.maxDivisibility.natural}
										placeholder="Natural"
									/>
								</div>
								<div>
									<input
										type="number"
										min="0"
										max="100"
										step="0.01"
										class="capacity-input qty w-full text-right"
										bind:value={entry.maxDivisibility.percentage}
										placeholder="Percentage"
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
								/>
								<span class="text-sm font-medium text-gray-600">Hidden till Request Accepted</span>
							</label>
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

	.capacity-item {
		display: flex;
		flex-direction: column;
	}

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
