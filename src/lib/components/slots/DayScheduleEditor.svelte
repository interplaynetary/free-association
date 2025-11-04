<script lang="ts">
	/**
	 * DayScheduleEditor - LEVEL 3: Day-specific time patterns
	 * 
	 * Allows setting different time ranges for different days:
	 * - "Mondays & Fridays: 9am-12pm"
	 * - "Tuesdays: 2pm-5pm and 7pm-9pm" (multiple ranges per day)
	 * - "Weekends: 10am-6pm"
	 * 
	 * Generates DaySchedule[] for AvailabilityWindow
	 */
	
	import type { DaySchedule, DayOfWeek, TimeRange } from '$lib/protocol/schemas';
	import TimeRangeEditor from './TimeRangeEditor.svelte';
	
	interface Props {
		schedules: DaySchedule[];
		onUpdate: (schedules: DaySchedule[]) => void;
	}
	
	let { schedules, onUpdate }: Props = $props();
	
	let localSchedules = $state<DaySchedule[]>(schedules.length > 0 ? schedules : []);
	
	const DAYS: { value: DayOfWeek; label: string; short: string }[] = [
		{ value: 'monday', label: 'Monday', short: 'Mon' },
		{ value: 'tuesday', label: 'Tuesday', short: 'Tue' },
		{ value: 'wednesday', label: 'Wednesday', short: 'Wed' },
		{ value: 'thursday', label: 'Thursday', short: 'Thu' },
		{ value: 'friday', label: 'Friday', short: 'Fri' },
		{ value: 'saturday', label: 'Saturday', short: 'Sat' },
		{ value: 'sunday', label: 'Sunday', short: 'Sun' }
	];
	
	// Preset patterns
	const PRESETS = [
		{
			name: 'Weekdays (Mon-Fri)',
			days: ['monday', 'tuesday', 'wednesday', 'thursday', 'friday'] as DayOfWeek[]
		},
		{
			name: 'Weekends (Sat-Sun)',
			days: ['saturday', 'sunday'] as DayOfWeek[]
		},
		{
			name: 'All Week',
			days: ['monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'] as DayOfWeek[]
		}
	];
	
	// Sync with props
	$effect(() => {
		if (schedules.length > 0) {
			localSchedules = schedules;
		}
	});
	
	function addSchedule(preset?: DayOfWeek[]) {
		const newSchedule: DaySchedule = {
			days: preset || ['monday'],
			time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
		};
		
		localSchedules = [...localSchedules, newSchedule];
		onUpdate(localSchedules);
	}
	
	function removeSchedule(index: number) {
		localSchedules = localSchedules.filter((_, i) => i !== index);
		onUpdate(localSchedules);
	}
	
	function updateScheduleDays(index: number, days: DayOfWeek[]) {
		localSchedules[index].days = days;
		localSchedules = [...localSchedules];
		onUpdate(localSchedules);
	}
	
	function toggleDay(scheduleIndex: number, day: DayOfWeek) {
		const schedule = localSchedules[scheduleIndex];
		const days = schedule.days.includes(day)
			? schedule.days.filter(d => d !== day)
			: [...schedule.days, day];
		
		updateScheduleDays(scheduleIndex, days);
	}
	
	function addTimeRange(scheduleIndex: number) {
		const schedule = localSchedules[scheduleIndex];
		schedule.time_ranges.push({ start_time: '09:00', end_time: '17:00' });
		localSchedules = [...localSchedules];
		onUpdate(localSchedules);
	}
	
	function updateTimeRange(scheduleIndex: number, rangeIndex: number, start: string, end: string) {
		const schedule = localSchedules[scheduleIndex];
		schedule.time_ranges[rangeIndex] = { start_time: start, end_time: end };
		localSchedules = [...localSchedules];
		onUpdate(localSchedules);
	}
	
	function removeTimeRange(scheduleIndex: number, rangeIndex: number) {
		const schedule = localSchedules[scheduleIndex];
		schedule.time_ranges = schedule.time_ranges.filter((_, i) => i !== rangeIndex);
		localSchedules = [...localSchedules];
		onUpdate(localSchedules);
	}
</script>

<div class="day-schedule-editor">
	<div class="editor-header">
		<h5 class="editor-subtitle">📅 Day-Specific Patterns</h5>
		<p class="editor-hint">Set different times for different days of the week</p>
	</div>
	
	<!-- Quick presets -->
	{#if localSchedules.length === 0}
		<div class="presets">
			<span class="presets-label">Quick start:</span>
			{#each PRESETS as preset}
				<button
					type="button"
					class="preset-btn"
					onclick={() => addSchedule(preset.days)}
				>
					{preset.name}
				</button>
			{/each}
		</div>
	{/if}
	
	<!-- Schedule list -->
	<div class="schedules-list">
		{#each localSchedules as schedule, scheduleIndex (scheduleIndex)}
			<div class="schedule-card">
				<div class="schedule-header">
					<span class="schedule-number">{scheduleIndex + 1}</span>
					<button
						type="button"
						class="remove-schedule-btn"
						onclick={() => removeSchedule(scheduleIndex)}
						title="Remove this schedule"
					>
						×
					</button>
				</div>
				
				<!-- Day picker -->
				<div class="day-picker">
					<label class="section-label">Days:</label>
					<div class="day-buttons">
						{#each DAYS as day}
							<button
								type="button"
								class="day-btn"
								class:active={schedule.days.includes(day.value)}
								onclick={() => toggleDay(scheduleIndex, day.value)}
								title={day.label}
							>
								{day.short}
							</button>
						{/each}
					</div>
				</div>
				
				<!-- Time ranges -->
				<div class="time-ranges-section">
					<label class="section-label">Time ranges:</label>
					<div class="time-ranges-list">
						{#each schedule.time_ranges as range, rangeIndex}
							<TimeRangeEditor
								startTime={range.start_time}
								endTime={range.end_time}
								onUpdate={(start, end) => updateTimeRange(scheduleIndex, rangeIndex, start, end)}
								onRemove={schedule.time_ranges.length > 1 ? () => removeTimeRange(scheduleIndex, rangeIndex) : undefined}
								removable={schedule.time_ranges.length > 1}
							/>
						{/each}
					</div>
					
					<button
						type="button"
						class="add-range-btn"
						onclick={() => addTimeRange(scheduleIndex)}
					>
						+ Add another time slot
					</button>
				</div>
			</div>
		{/each}
	</div>
	
	<!-- Add schedule button -->
	<button
		type="button"
		class="add-schedule-btn"
		onclick={() => addSchedule()}
	>
		+ Add another day pattern
	</button>
</div>

<style>
	.day-schedule-editor {
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
	
	.presets {
		display: flex;
		flex-wrap: wrap;
		align-items: center;
		gap: 0.5rem;
		padding: 0.75rem;
		background: #eff6ff;
		border: 1px solid #bfdbfe;
		border-radius: 6px;
	}
	
	.presets-label {
		font-size: 0.75rem;
		font-weight: 600;
		color: #1e40af;
	}
	
	.preset-btn {
		padding: 0.375rem 0.75rem;
		border: 1px solid #93c5fd;
		border-radius: 4px;
		background: white;
		color: #2563eb;
		font-size: 0.75rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.preset-btn:hover {
		background: #dbeafe;
		border-color: #60a5fa;
	}
	
	.schedules-list {
		display: flex;
		flex-direction: column;
		gap: 1rem;
	}
	
	.schedule-card {
		padding: 1rem;
		background: white;
		border: 2px solid #e5e7eb;
		border-radius: 8px;
		transition: all 0.2s ease;
	}
	
	.schedule-card:hover {
		border-color: #cbd5e1;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05);
	}
	
	.schedule-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 1rem;
	}
	
	.schedule-number {
		display: inline-flex;
		align-items: center;
		justify-content: center;
		width: 1.5rem;
		height: 1.5rem;
		border-radius: 50%;
		background: #3b82f6;
		color: white;
		font-size: 0.75rem;
		font-weight: 600;
	}
	
	.remove-schedule-btn {
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
	
	.remove-schedule-btn:hover {
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
	
	.day-picker {
		margin-bottom: 1rem;
	}
	
	.day-buttons {
		display: flex;
		gap: 0.25rem;
	}
	
	.day-btn {
		flex: 1;
		padding: 0.5rem 0.25rem;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		background: white;
		color: #64748b;
		font-size: 0.7rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.day-btn:hover {
		background: #f8fafc;
		border-color: #cbd5e1;
	}
	
	.day-btn.active {
		background: #3b82f6;
		border-color: #2563eb;
		color: white;
	}
	
	.time-ranges-section {
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
	}
	
	.time-ranges-list {
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
	}
	
	.add-range-btn {
		padding: 0.5rem 0.75rem;
		border: 1px dashed #cbd5e1;
		border-radius: 4px;
		background: #f8fafc;
		color: #3b82f6;
		font-size: 0.75rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.add-range-btn:hover {
		background: #eff6ff;
		border-color: #3b82f6;
	}
	
	.add-schedule-btn {
		padding: 0.75rem 1rem;
		border: 2px dashed #cbd5e1;
		border-radius: 6px;
		background: white;
		color: #3b82f6;
		font-size: 0.813rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.add-schedule-btn:hover {
		background: #eff6ff;
		border-color: #3b82f6;
		transform: scale(1.01);
	}
</style>

