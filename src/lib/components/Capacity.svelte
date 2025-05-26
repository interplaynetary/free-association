<script lang="ts">
	import type { Capacity } from '$lib/schema';
	import Bar from './Bar.svelte';

	interface Props {
		capacity: Capacity;
		canDelete: boolean;
		onupdate?: (capacity: Capacity) => void;
		ondelete?: (id: string) => void;
	}

	let { capacity, canDelete, onupdate, ondelete }: Props = $props();

	// UI state for expanded capacity editing
	let expanded = $state(false);

	// Convert recipient_shares to bar segments
	const recipientSegments = $derived(
		capacity.recipient_shares
			? Object.entries(capacity.recipient_shares).map(([userId, share]) => ({
					id: userId,
					value: share * 100 // Convert from 0-1 to 0-100 percentage
				}))
			: []
	);

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

	// Handler for input events that updates capacity
	function handleCapacityUpdate() {
		onupdate?.(capacity);
	}

	// Delete this capacity
	function handleDelete() {
		ondelete?.(capacity.id);
	}

	// Toggle expanded state
	function toggleExpanded() {
		expanded = !expanded;
	}

	// Format date for input
	function formatDateForInput(date: Date | undefined): string {
		if (!date) return '';
		return date.toISOString().split('T')[0];
	}
</script>

<div class="capacity-item">
	<!-- Recipient shares bar -->
	{#if recipientSegments.length > 0}
		<div class="recipient-shares-bar mb-1">
			<Bar segments={recipientSegments} height="8px" rounded={true} backgroundColor="#f3f4f6" />
		</div>
	{/if}

	<div class="capacity-row flex items-center gap-2 rounded bg-white p-2 shadow-sm">
		<input
			type="text"
			class="capacity-input name min-w-0 flex-1"
			bind:value={capacity.name}
			placeholder="Name"
			oninput={handleCapacityUpdate}
		/>
		<input
			type="number"
			class="capacity-input qty w-16 text-right"
			min="0"
			step="0.01"
			bind:value={capacity.quantity}
			placeholder="Qty"
			oninput={handleCapacityUpdate}
		/>
		<input
			type="text"
			class="capacity-input unit w-20"
			bind:value={capacity.unit}
			placeholder="Unit"
			oninput={handleCapacityUpdate}
		/>
		<button type="button" class="settings-btn ml-1" onclick={toggleExpanded}>
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
		<button type="button" class="remove-btn ml-1" onclick={handleDelete} disabled={!canDelete}
			>×</button
		>
	</div>

	{#if expanded}
		<div class="expanded-settings mt-2 rounded-md bg-white shadow-sm">
			<div class="settings-content mx-8 my-8">
				<div class="space-time-section mb-8">
					<h4 class="mb-4 text-sm font-medium text-gray-700">Space-time coordinates</h4>

					<div class="space-time-options mb-6 ml-2">
						<div class="flex flex-wrap gap-x-8 gap-y-3">
							<label class="inline-flex items-center">
								<input
									type="radio"
									name="location-type-{capacity.id}"
									value="Undefined"
									bind:group={capacity.location_type}
									class="mr-3"
									onchange={handleCapacityUpdate}
								/>
								<span class="text-sm text-gray-600">Undefined</span>
							</label>
							<label class="inline-flex items-center">
								<input
									type="radio"
									name="location-type-{capacity.id}"
									value="LiveLocation"
									bind:group={capacity.location_type}
									class="mr-3"
									onchange={handleCapacityUpdate}
								/>
								<span class="text-sm text-gray-600">Live location</span>
							</label>
							<label class="inline-flex items-center">
								<input
									type="radio"
									name="location-type-{capacity.id}"
									value="Specific"
									bind:group={capacity.location_type}
									class="mr-3"
									onchange={handleCapacityUpdate}
								/>
								<span class="text-sm text-gray-600">Specific coordinates</span>
							</label>
						</div>
					</div>

					{#if capacity.location_type === 'Specific'}
						<div class="date-time-section mb-6 ml-2">
							<div class="mb-4 ml-1">
								<label class="inline-flex items-center">
									<input
										type="checkbox"
										bind:checked={capacity.all_day}
										class="mr-3 h-4 w-4"
										onchange={handleCapacityUpdate}
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
										bind:value={capacity.start_date}
										onchange={handleCapacityUpdate}
									/>
								</div>

								<div>
									<h5 class="mb-2 text-sm text-gray-600">End Date</h5>
									<input
										type="date"
										class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
										bind:value={capacity.end_date}
										onchange={handleCapacityUpdate}
									/>
								</div>
							</div>

							{#if !capacity.all_day}
								<div class="mb-4 flex flex-col gap-4 md:flex-row">
									<div class="md:w-1/2">
										<input
											type="time"
											class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
											bind:value={capacity.start_time}
											onchange={handleCapacityUpdate}
										/>
									</div>

									<div class="flex items-center md:w-1/2">
										<span class="mx-2 hidden text-gray-400 md:inline">to</span>
										<input
											type="time"
											class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
											bind:value={capacity.end_time}
											onchange={handleCapacityUpdate}
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
											bind:value={capacity.time_zone}
											onchange={handleCapacityUpdate}
										/>
									</div>
								</div>
							{/if}

							<!-- Recurrence dropdown styled like Google Calendar -->
							<div class="mb-6">
								<div class="relative w-full md:w-auto">
									<select
										class="capacity-select w-full appearance-none rounded-md bg-gray-100 px-3 py-2"
										bind:value={capacity.recurrence}
										onchange={handleCapacityUpdate}
									>
										{#each recurrenceOptions as option}
											<option value={option}>{option}</option>
										{/each}
									</select>
								</div>
							</div>

							<!-- Custom recurrence options -->
							{#if capacity.recurrence === 'Custom...' && capacity.custom_recurrence_repeat_every !== undefined}
								<div class="custom-recurrence mt-4 rounded-md bg-gray-50 p-6">
									<div class="mb-6 flex items-center">
										<span class="mr-3 text-sm font-medium text-gray-600">Repeat every</span>
										<input
											type="number"
											min="1"
											class="capacity-input qty w-16 text-right"
											bind:value={capacity.custom_recurrence_repeat_every}
											onchange={handleCapacityUpdate}
										/>
										<select
											class="capacity-select ml-3 w-28"
											bind:value={capacity.custom_recurrence_repeat_unit}
											onchange={handleCapacityUpdate}
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
													name="ends-{capacity.id}"
													value="never"
													checked={capacity.custom_recurrence_end_type === 'never'}
													onchange={() => {
														capacity.custom_recurrence_end_type = 'never';
														handleCapacityUpdate();
													}}
													class="mr-3"
												/>
												<span class="text-sm text-gray-600">Never</span>
											</label>

											<div class="flex items-center">
												<label class="inline-flex items-center">
													<input
														type="radio"
														name="ends-{capacity.id}"
														value="endsOn"
														checked={capacity.custom_recurrence_end_type === 'endsOn'}
														onchange={() => {
															capacity.custom_recurrence_end_type = 'endsOn';
															capacity.custom_recurrence_end_value = formatDateForInput(new Date());
															handleCapacityUpdate();
														}}
														class="mr-3"
													/>
													<span class="text-sm text-gray-600">On</span>
												</label>
												{#if capacity.custom_recurrence_end_type === 'endsOn'}
													<div class="ml-6">
														<input
															type="date"
															class="capacity-input w-40 rounded-md bg-gray-100 px-3 py-2"
															bind:value={capacity.custom_recurrence_end_value}
															onchange={handleCapacityUpdate}
														/>
													</div>
												{/if}
											</div>

											<div class="flex items-center">
												<label class="inline-flex items-center">
													<input
														type="radio"
														name="ends-{capacity.id}"
														value="endsAfter"
														checked={capacity.custom_recurrence_end_type === 'endsAfter'}
														onchange={() => {
															capacity.custom_recurrence_end_type = 'endsAfter';
															capacity.custom_recurrence_end_value = '5';
															handleCapacityUpdate();
														}}
														class="mr-3"
													/>
													<span class="text-sm text-gray-600">After</span>
												</label>
												{#if capacity.custom_recurrence_end_type === 'endsAfter'}
													<div class="ml-6 flex items-center">
														<input
															type="number"
															min="1"
															class="capacity-input qty w-16 text-right"
															bind:value={capacity.custom_recurrence_end_value}
															onchange={handleCapacityUpdate}
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

				<div class="hidden-option ml-2">
					<label class="inline-flex items-center">
						<input
							type="checkbox"
							bind:checked={capacity.hidden_until_request_accepted}
							class="mr-3 h-4 w-4"
							onchange={handleCapacityUpdate}
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
								bind:value={capacity.max_natural_div}
								placeholder="Natural"
								onchange={handleCapacityUpdate}
							/>
						</div>
						<div>
							<input
								type="number"
								min="0"
								max="1"
								step="0.01"
								class="capacity-input qty w-full text-right"
								bind:value={capacity.max_percentage_div}
								placeholder="Percentage (0-1)"
								onchange={handleCapacityUpdate}
							/>
						</div>
					</div>
				</div>
			</div>
		</div>
	{/if}
</div>

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
