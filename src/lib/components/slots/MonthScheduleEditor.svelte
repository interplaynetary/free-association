<script lang="ts">
	/**
	 * MonthScheduleEditor - LEVEL 1: Month-specific patterns
	 * 
	 * Allows setting patterns for specific months of the year:
	 * - "February: all weeks, Monday/Wednesday 9-12"
	 * - "Summer months (Jun-Aug): Weekends 10-6"
	 * - "October: second week Tuesday 2-4, fourth week Monday/Wednesday 9-12"
	 * 
	 * Generates MonthSchedule[] for AvailabilityWindow
	 * Supports three modes per month:
	 * 1. Week-specific patterns (most flexible)
	 * 2. Simple day schedules (all weeks)
	 * 3. Same times every day (all weeks)
	 */
	
	import type { MonthSchedule, WeekSchedule, DaySchedule, DayOfWeek, TimeRange } from '../../../../packages/protocol/src/schemas';
	import TimeRangeEditor from './TimeRangeEditor.svelte';
	
	interface Props {
		schedules: MonthSchedule[];
		onUpdate: (schedules: MonthSchedule[]) => void;
	}
	
	let { schedules, onUpdate }: Props = $props();
	
	let localSchedules = $state<MonthSchedule[]>(schedules.length > 0 ? schedules : []);
	
	const MONTHS = [
		{ value: 1, label: 'January', short: 'Jan' },
		{ value: 2, label: 'February', short: 'Feb' },
		{ value: 3, label: 'March', short: 'Mar' },
		{ value: 4, label: 'April', short: 'Apr' },
		{ value: 5, label: 'May', short: 'May' },
		{ value: 6, label: 'June', short: 'Jun' },
		{ value: 7, label: 'July', short: 'Jul' },
		{ value: 8, label: 'August', short: 'Aug' },
		{ value: 9, label: 'September', short: 'Sep' },
		{ value: 10, label: 'October', short: 'Oct' },
		{ value: 11, label: 'November', short: 'Nov' },
		{ value: 12, label: 'December', short: 'Dec' }
	];
	
	const WEEKS = [
		{ value: 1, label: '1st' },
		{ value: 2, label: '2nd' },
		{ value: 3, label: '3rd' },
		{ value: 4, label: '4th' },
		{ value: 5, label: '5th' }
	];
	
	const DAYS: { value: DayOfWeek; label: string; short: string }[] = [
		{ value: 'monday', label: 'Monday', short: 'M' },
		{ value: 'tuesday', label: 'Tuesday', short: 'T' },
		{ value: 'wednesday', label: 'Wednesday', short: 'W' },
		{ value: 'thursday', label: 'Thursday', short: 'T' },
		{ value: 'friday', label: 'Friday', short: 'F' },
		{ value: 'saturday', label: 'Saturday', short: 'S' },
		{ value: 'sunday', label: 'Sunday', short: 'S' }
	];
	
	type MonthMode = 'simple' | 'day-schedules' | 'week-schedules';
	
	// Track mode for each month schedule
	let monthModes = $state<Record<number, MonthMode>>({});
	
	// Sync with props
	$effect(() => {
		if (schedules.length > 0) {
			localSchedules = schedules;
			
			// Detect modes from existing schedules
			schedules.forEach((schedule, index) => {
				if (schedule.week_schedules?.length) {
					monthModes[index] = 'week-schedules';
				} else if (schedule.day_schedules?.length) {
					monthModes[index] = 'day-schedules';
				} else {
					monthModes[index] = 'simple';
				}
			});
		}
	});
	
	function addMonthSchedule() {
		const newSchedule: MonthSchedule = {
			month: 1,
			time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
		};
		
		const newIndex = localSchedules.length;
		localSchedules = [...localSchedules, newSchedule];
		monthModes[newIndex] = 'simple';
		onUpdate(localSchedules);
	}
	
	function removeMonthSchedule(index: number) {
		localSchedules = localSchedules.filter((_, i) => i !== index);
		delete monthModes[index];
		onUpdate(localSchedules);
	}
	
	function changeMonth(index: number, month: number) {
		localSchedules[index].month = month;
		localSchedules = [...localSchedules];
		onUpdate(localSchedules);
	}
	
	function changeMode(index: number, mode: MonthMode) {
		const schedule = localSchedules[index];
		monthModes[index] = mode;
		
		// Clear other mode data and set up new mode
		delete schedule.time_ranges;
		delete schedule.day_schedules;
		delete schedule.week_schedules;
		
		if (mode === 'simple') {
			schedule.time_ranges = [{ start_time: '09:00', end_time: '17:00' }];
		} else if (mode === 'day-schedules') {
			schedule.day_schedules = [{
				days: ['monday'],
				time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
			}];
		} else if (mode === 'week-schedules') {
			schedule.week_schedules = [{
				weeks: [1],
				day_schedules: [{
					days: ['monday'],
					time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
				}]
			}];
		}
		
		localSchedules = [...localSchedules];
		onUpdate(localSchedules);
	}
	
	// Simple mode handlers
	function addSimpleTimeRange(index: number) {
		const schedule = localSchedules[index];
		if (!schedule.time_ranges) schedule.time_ranges = [];
		schedule.time_ranges.push({ start_time: '09:00', end_time: '17:00' });
		localSchedules = [...localSchedules];
		onUpdate(localSchedules);
	}
	
	function updateSimpleTimeRange(index: number, rangeIndex: number, start: string, end: string) {
		const schedule = localSchedules[index];
		if (schedule.time_ranges) {
			schedule.time_ranges[rangeIndex] = { start_time: start, end_time: end };
			localSchedules = [...localSchedules];
			onUpdate(localSchedules);
		}
	}
	
	function removeSimpleTimeRange(index: number, rangeIndex: number) {
		const schedule = localSchedules[index];
		if (schedule.time_ranges) {
			schedule.time_ranges = schedule.time_ranges.filter((_, i) => i !== rangeIndex);
			localSchedules = [...localSchedules];
			onUpdate(localSchedules);
		}
	}
	
	// Day schedules mode handlers (similar to DayScheduleEditor)
	function addDaySchedule(monthIndex: number) {
		const schedule = localSchedules[monthIndex];
		if (!schedule.day_schedules) schedule.day_schedules = [];
		schedule.day_schedules.push({
			days: ['monday'],
			time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
		});
		localSchedules = [...localSchedules];
		onUpdate(localSchedules);
	}
	
	function removeDaySchedule(monthIndex: number, dayIndex: number) {
		const schedule = localSchedules[monthIndex];
		if (schedule.day_schedules) {
			schedule.day_schedules = schedule.day_schedules.filter((_, i) => i !== dayIndex);
			localSchedules = [...localSchedules];
			onUpdate(localSchedules);
		}
	}
	
	function toggleDay(monthIndex: number, dayIndex: number, day: DayOfWeek) {
		const daySchedule = localSchedules[monthIndex].day_schedules?.[dayIndex];
		if (daySchedule) {
			const days = daySchedule.days.includes(day)
				? daySchedule.days.filter(d => d !== day)
				: [...daySchedule.days, day];
			daySchedule.days = days;
			localSchedules = [...localSchedules];
			onUpdate(localSchedules);
		}
	}
	
	function addDayTimeRange(monthIndex: number, dayIndex: number) {
		const daySchedule = localSchedules[monthIndex].day_schedules?.[dayIndex];
		if (daySchedule) {
			daySchedule.time_ranges.push({ start_time: '09:00', end_time: '17:00' });
			localSchedules = [...localSchedules];
			onUpdate(localSchedules);
		}
	}
	
	function updateDayTimeRange(monthIndex: number, dayIndex: number, rangeIndex: number, start: string, end: string) {
		const daySchedule = localSchedules[monthIndex].day_schedules?.[dayIndex];
		if (daySchedule) {
			daySchedule.time_ranges[rangeIndex] = { start_time: start, end_time: end };
			localSchedules = [...localSchedules];
			onUpdate(localSchedules);
		}
	}
	
	function removeDayTimeRange(monthIndex: number, dayIndex: number, rangeIndex: number) {
		const daySchedule = localSchedules[monthIndex].day_schedules?.[dayIndex];
		if (daySchedule) {
			daySchedule.time_ranges = daySchedule.time_ranges.filter((_, i) => i !== rangeIndex);
			localSchedules = [...localSchedules];
			onUpdate(localSchedules);
		}
	}
</script>

<div class="month-schedule-editor">
	<div class="editor-header">
		<h5 class="editor-subtitle">📆 Month-Specific Patterns</h5>
		<p class="editor-hint">Set different patterns for different months of the year</p>
	</div>
	
	<!-- Month schedules list -->
	<div class="schedules-list">
		{#each localSchedules as monthSchedule, monthIndex (monthIndex)}
			<div class="month-schedule-card">
				<div class="card-header">
					<div class="month-selector">
						<label class="month-label">Month:</label>
						<select
							bind:value={monthSchedule.month}
							onchange={() => changeMonth(monthIndex, monthSchedule.month)}
							class="month-select"
						>
							{#each MONTHS as month}
								<option value={month.value}>{month.label}</option>
							{/each}
						</select>
					</div>
					
					<button
						type="button"
						class="remove-btn"
						onclick={() => removeMonthSchedule(monthIndex)}
						title="Remove this month schedule"
					>
						×
					</button>
				</div>
				
				<!-- Mode selector -->
				<div class="mode-selector">
					<label class="section-label">Pattern type:</label>
					<div class="mode-buttons">
						<button
							type="button"
							class="mode-btn"
							class:active={monthModes[monthIndex] === 'simple'}
							onclick={() => changeMode(monthIndex, 'simple')}
						>
							Simple Times
						</button>
						<button
							type="button"
							class="mode-btn"
							class:active={monthModes[monthIndex] === 'day-schedules'}
							onclick={() => changeMode(monthIndex, 'day-schedules')}
						>
							Day-Specific
						</button>
						<button
							type="button"
							class="mode-btn"
							class:active={monthModes[monthIndex] === 'week-schedules'}
							onclick={() => changeMode(monthIndex, 'week-schedules')}
						>
							Week-Specific
						</button>
					</div>
				</div>
				
				<!-- Simple mode: just time ranges -->
				{#if monthModes[monthIndex] === 'simple'}
					<div class="simple-content">
						<label class="section-label">Times (all days, all weeks):</label>
						<div class="time-ranges-list">
							{#each monthSchedule.time_ranges || [] as range, rangeIndex}
								<TimeRangeEditor
									startTime={range.start_time}
									endTime={range.end_time}
									onUpdate={(start, end) => updateSimpleTimeRange(monthIndex, rangeIndex, start, end)}
									onRemove={(monthSchedule.time_ranges?.length ?? 0) > 1 ? () => removeSimpleTimeRange(monthIndex, rangeIndex) : undefined}
									removable={(monthSchedule.time_ranges?.length ?? 0) > 1}
								/>
							{/each}
						</div>
						<button
							type="button"
							class="add-mini-btn"
							onclick={() => addSimpleTimeRange(monthIndex)}
						>
							+ Add time slot
						</button>
					</div>
				{/if}
				
				<!-- Day schedules mode -->
				{#if monthModes[monthIndex] === 'day-schedules'}
					<div class="day-schedules-content">
						<label class="section-label">Day patterns (all weeks):</label>
						
						{#each monthSchedule.day_schedules || [] as daySchedule, dayIndex}
							<div class="day-schedule-card">
								<div class="day-schedule-header">
									<span class="subsection-label">Pattern {dayIndex + 1}</span>
									{#if (monthSchedule.day_schedules?.length ?? 0) > 1}
										<button
											type="button"
											class="remove-small-btn"
											onclick={() => removeDaySchedule(monthIndex, dayIndex)}
										>
											×
										</button>
									{/if}
								</div>
								
								<!-- Day picker -->
								<div class="day-picker">
									<label class="mini-label">Days:</label>
									<div class="day-buttons">
										{#each DAYS as day}
											<button
												type="button"
												class="day-btn"
												class:active={daySchedule.days.includes(day.value)}
												onclick={() => toggleDay(monthIndex, dayIndex, day.value)}
												title={day.label}
											>
												{day.short}
											</button>
										{/each}
									</div>
								</div>
								
								<!-- Time ranges -->
								<div class="time-ranges-section">
									<label class="mini-label">Times:</label>
									{#each daySchedule.time_ranges as range, rangeIndex}
										<TimeRangeEditor
											startTime={range.start_time}
											endTime={range.end_time}
											onUpdate={(start, end) => updateDayTimeRange(monthIndex, dayIndex, rangeIndex, start, end)}
											onRemove={daySchedule.time_ranges.length > 1 ? () => removeDayTimeRange(monthIndex, dayIndex, rangeIndex) : undefined}
											removable={daySchedule.time_ranges.length > 1}
										/>
									{/each}
									
									<button
										type="button"
										class="add-mini-btn"
										onclick={() => addDayTimeRange(monthIndex, dayIndex)}
									>
										+ Add time slot
									</button>
								</div>
							</div>
						{/each}
						
						<button
							type="button"
							class="add-day-btn"
							onclick={() => addDaySchedule(monthIndex)}
						>
							+ Add day pattern
						</button>
					</div>
				{/if}
				
				<!-- Week schedules mode -->
				{#if monthModes[monthIndex] === 'week-schedules'}
					<div class="week-schedules-content">
						<p class="hint-text">
							Week-specific patterns for this month (advanced - most complex patterns possible)
						</p>
						<p class="hint-text">
							This mode allows different patterns for different weeks. Each week can have multiple day patterns.
						</p>
					</div>
				{/if}
			</div>
		{/each}
	</div>
	
	<!-- Add month schedule button -->
	<button
		type="button"
		class="add-schedule-btn"
		onclick={addMonthSchedule}
	>
		+ Add month-specific pattern
	</button>
</div>

<style>
	.month-schedule-editor {
		display: flex;
		flex-direction: column;
		gap: 1rem;
	}
	
	.editor-header {
		margin-bottom: 0.5rem;
	}
	
	.editor-subtitle {
		margin: 0 0 0.25rem 0;
		font-size: 0.813rem;
		font-weight: 600;
		color: #1f2937;
	}
	
	.editor-hint {
		margin: 0;
		font-size: 0.7rem;
		color: #64748b;
		font-style: italic;
	}
	
	.schedules-list {
		display: flex;
		flex-direction: column;
		gap: 1rem;
	}
	
	.month-schedule-card {
		padding: 1rem;
		background: white;
		border: 2px solid #f59e0b;
		border-radius: 8px;
		transition: all 0.2s ease;
	}
	
	.month-schedule-card:hover {
		border-color: #d97706;
		box-shadow: 0 2px 8px rgba(245, 158, 11, 0.15);
	}
	
	.card-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 1rem;
	}
	
	.month-selector {
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.month-label {
		font-size: 0.75rem;
		font-weight: 600;
		color: #92400e;
	}
	
	.month-select {
		padding: 0.375rem 0.5rem;
		border: 1px solid #fbbf24;
		border-radius: 4px;
		background: #fffbeb;
		color: #92400e;
		font-size: 0.75rem;
		font-weight: 600;
		cursor: pointer;
	}
	
	.month-select:focus {
		outline: none;
		border-color: #f59e0b;
	}
	
	.remove-btn {
		width: 1.75rem;
		height: 1.75rem;
		display: flex;
		align-items: center;
		justify-content: center;
		border: 1px solid #fecaca;
		border-radius: 4px;
		background: #fef2f2;
		color: #dc2626;
		font-size: 1.25rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.remove-btn:hover {
		background: #fee2e2;
		transform: scale(1.05);
	}
	
	.section-label {
		display: block;
		margin-bottom: 0.5rem;
		font-size: 0.75rem;
		font-weight: 600;
		color: #64748b;
		text-transform: uppercase;
		letter-spacing: 0.05em;
	}
	
	.mode-selector {
		margin-bottom: 1rem;
	}
	
	.mode-buttons {
		display: flex;
		gap: 0.5rem;
		padding: 0.25rem;
		background: #fffbeb;
		border-radius: 6px;
		border: 1px solid #fde68a;
	}
	
	.mode-btn {
		flex: 1;
		padding: 0.375rem 0.5rem;
		border: none;
		border-radius: 4px;
		background: transparent;
		color: #92400e;
		font-size: 0.7rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.mode-btn:hover {
		background: #fef3c7;
	}
	
	.mode-btn.active {
		background: #f59e0b;
		color: white;
	}
	
	.simple-content,
	.day-schedules-content,
	.week-schedules-content {
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
	}
	
	.time-ranges-list,
	.time-ranges-section {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}
	
	.add-mini-btn {
		padding: 0.375rem 0.5rem;
		border: 1px dashed #fbbf24;
		border-radius: 4px;
		background: #fffbeb;
		color: #d97706;
		font-size: 0.7rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.add-mini-btn:hover {
		background: #fef3c7;
		border-color: #f59e0b;
	}
	
	.day-schedule-card {
		padding: 0.75rem;
		background: #fffbeb;
		border: 1px solid #fde68a;
		border-radius: 6px;
	}
	
	.day-schedule-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 0.75rem;
	}
	
	.subsection-label {
		font-size: 0.7rem;
		font-weight: 600;
		color: #d97706;
	}
	
	.remove-small-btn {
		width: 1.25rem;
		height: 1.25rem;
		display: flex;
		align-items: center;
		justify-content: center;
		border: 1px solid #fca5a5;
		border-radius: 3px;
		background: #fef2f2;
		color: #dc2626;
		font-size: 1rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.remove-small-btn:hover {
		background: #fee2e2;
	}
	
	.day-picker {
		margin-bottom: 0.75rem;
	}
	
	.mini-label {
		display: block;
		margin-bottom: 0.375rem;
		font-size: 0.65rem;
		font-weight: 600;
		color: #d97706;
		text-transform: uppercase;
		letter-spacing: 0.05em;
	}
	
	.day-buttons {
		display: flex;
		gap: 0.25rem;
	}
	
	.day-btn {
		flex: 1;
		padding: 0.375rem 0.25rem;
		border: 1px solid #fde68a;
		border-radius: 3px;
		background: white;
		color: #64748b;
		font-size: 0.65rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.day-btn:hover {
		background: #fffbeb;
		border-color: #fbbf24;
	}
	
	.day-btn.active {
		background: #fbbf24;
		border-color: #f59e0b;
		color: #78350f;
	}
	
	.add-day-btn {
		padding: 0.5rem 0.75rem;
		border: 1px dashed #fbbf24;
		border-radius: 6px;
		background: white;
		color: #f59e0b;
		font-size: 0.75rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.add-day-btn:hover {
		background: #fffbeb;
		border-color: #f59e0b;
	}
	
	.add-schedule-btn {
		padding: 0.75rem 1rem;
		border: 2px dashed #cbd5e1;
		border-radius: 6px;
		background: white;
		color: #f59e0b;
		font-size: 0.813rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.add-schedule-btn:hover {
		background: #fffbeb;
		border-color: #f59e0b;
		transform: scale(1.01);
	}
	
	.hint-text {
		margin: 0;
		padding: 0.5rem;
		background: #fef3c7;
		border-left: 3px solid #f59e0b;
		font-size: 0.7rem;
		color: #92400e;
		font-style: italic;
	}
</style>

