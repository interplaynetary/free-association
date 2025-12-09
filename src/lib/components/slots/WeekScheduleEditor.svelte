<script lang="ts">
	/**
	 * WeekScheduleEditor - LEVEL 2: Week-specific patterns
	 * 
	 * Allows setting patterns for specific weeks of the month:
	 * - "First and third weeks: Mon-Fri 9-5"
	 * - "Second week: Tuesday only 2-4"
	 * - "Every week except the last: Weekends 10-6"
	 * 
	 * Generates WeekSchedule[] for AvailabilityWindow
	 */
	
	import type { WeekSchedule, DaySchedule, DayOfWeek, TimeRange } from '../../../../packages/protocol/src/schemas';
	import TimeRangeEditor from './TimeRangeEditor.svelte';
	
	interface Props {
		schedules: WeekSchedule[];
		onUpdate: (schedules: WeekSchedule[]) => void;
	}
	
	let { schedules, onUpdate }: Props = $props();
	
	let localSchedules = $state<WeekSchedule[]>(schedules.length > 0 ? schedules : []);
	
	const WEEKS = [
		{ value: 1, label: '1st week' },
		{ value: 2, label: '2nd week' },
		{ value: 3, label: '3rd week' },
		{ value: 4, label: '4th week' },
		{ value: 5, label: '5th week' }
	];
	
	const DAYS: { value: DayOfWeek; label: string; short: string }[] = [
		{ value: 'monday', label: 'Monday', short: 'Mon' },
		{ value: 'tuesday', label: 'Tuesday', short: 'Tue' },
		{ value: 'wednesday', label: 'Wednesday', short: 'Wed' },
		{ value: 'thursday', label: 'Thursday', short: 'Thu' },
		{ value: 'friday', label: 'Friday', short: 'Fri' },
		{ value: 'saturday', label: 'Saturday', short: 'Sat' },
		{ value: 'sunday', label: 'Sunday', short: 'Sun' }
	];
	
	// Sync with props
	$effect(() => {
		if (schedules.length > 0) {
			localSchedules = schedules;
		}
	});
	
	function addWeekSchedule() {
		const newSchedule: WeekSchedule = {
			weeks: [1],
			day_schedules: [{
				days: ['monday'],
				time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
			}]
		};
		
		localSchedules = [...localSchedules, newSchedule];
		onUpdate(localSchedules);
	}
	
	function removeWeekSchedule(index: number) {
		localSchedules = localSchedules.filter((_, i) => i !== index);
		onUpdate(localSchedules);
	}
	
	function toggleWeek(scheduleIndex: number, week: number) {
		const schedule = localSchedules[scheduleIndex];
		const weeks = schedule.weeks.includes(week)
			? schedule.weeks.filter(w => w !== week)
			: [...schedule.weeks, week].sort();
		
		schedule.weeks = weeks;
		localSchedules = [...localSchedules];
		onUpdate(localSchedules);
	}
	
	function addDaySchedule(weekScheduleIndex: number) {
		const weekSchedule = localSchedules[weekScheduleIndex];
		weekSchedule.day_schedules.push({
			days: ['monday'],
			time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
		});
		localSchedules = [...localSchedules];
		onUpdate(localSchedules);
	}
	
	function removeDaySchedule(weekScheduleIndex: number, dayScheduleIndex: number) {
		const weekSchedule = localSchedules[weekScheduleIndex];
		weekSchedule.day_schedules = weekSchedule.day_schedules.filter((_, i) => i !== dayScheduleIndex);
		localSchedules = [...localSchedules];
		onUpdate(localSchedules);
	}
	
	function toggleDay(weekScheduleIndex: number, dayScheduleIndex: number, day: DayOfWeek) {
		const daySchedule = localSchedules[weekScheduleIndex].day_schedules[dayScheduleIndex];
		const days = daySchedule.days.includes(day)
			? daySchedule.days.filter(d => d !== day)
			: [...daySchedule.days, day];
		
		daySchedule.days = days;
		localSchedules = [...localSchedules];
		onUpdate(localSchedules);
	}
	
	function addTimeRange(weekScheduleIndex: number, dayScheduleIndex: number) {
		const daySchedule = localSchedules[weekScheduleIndex].day_schedules[dayScheduleIndex];
		daySchedule.time_ranges.push({ start_time: '09:00', end_time: '17:00' });
		localSchedules = [...localSchedules];
		onUpdate(localSchedules);
	}
	
	function updateTimeRange(weekScheduleIndex: number, dayScheduleIndex: number, rangeIndex: number, start: string, end: string) {
		const daySchedule = localSchedules[weekScheduleIndex].day_schedules[dayScheduleIndex];
		daySchedule.time_ranges[rangeIndex] = { start_time: start, end_time: end };
		localSchedules = [...localSchedules];
		onUpdate(localSchedules);
	}
	
	function removeTimeRange(weekScheduleIndex: number, dayScheduleIndex: number, rangeIndex: number) {
		const daySchedule = localSchedules[weekScheduleIndex].day_schedules[dayScheduleIndex];
		daySchedule.time_ranges = daySchedule.time_ranges.filter((_, i) => i !== rangeIndex);
		localSchedules = [...localSchedules];
		onUpdate(localSchedules);
	}
</script>

<div class="week-schedule-editor">
	<div class="editor-header">
		<h5 class="editor-subtitle">📊 Week-Specific Patterns</h5>
		<p class="editor-hint">Set patterns for specific weeks of the month</p>
	</div>
	
	<!-- Week schedules list -->
	<div class="schedules-list">
		{#each localSchedules as weekSchedule, weekScheduleIndex (weekScheduleIndex)}
			<div class="week-schedule-card">
				<div class="card-header">
					<span class="schedule-number">{weekScheduleIndex + 1}</span>
					<button
						type="button"
						class="remove-btn"
						onclick={() => removeWeekSchedule(weekScheduleIndex)}
						title="Remove this week schedule"
					>
						×
					</button>
				</div>
				
				<!-- Week picker -->
				<div class="week-picker">
					<label class="section-label">Active weeks of month:</label>
					<div class="week-buttons">
						{#each WEEKS as week}
							<button
								type="button"
								class="week-btn"
								class:active={weekSchedule.weeks.includes(week.value)}
								onclick={() => toggleWeek(weekScheduleIndex, week.value)}
								title={week.label}
							>
								{week.label}
							</button>
						{/each}
					</div>
				</div>
				
				<!-- Day schedules within this week pattern -->
				<div class="day-schedules-section">
					<label class="section-label">Day patterns for these weeks:</label>
					
					{#each weekSchedule.day_schedules as daySchedule, dayScheduleIndex}
						<div class="day-schedule-card">
							<div class="day-schedule-header">
								<span class="subsection-label">Pattern {dayScheduleIndex + 1}</span>
								{#if weekSchedule.day_schedules.length > 1}
									<button
										type="button"
										class="remove-small-btn"
										onclick={() => removeDaySchedule(weekScheduleIndex, dayScheduleIndex)}
									>
										×
									</button>
								{/if}
							</div>
							
							<!-- Day picker for this pattern -->
							<div class="day-picker">
								<label class="mini-label">Days:</label>
								<div class="day-buttons">
									{#each DAYS as day}
										<button
											type="button"
											class="day-btn"
											class:active={daySchedule.days.includes(day.value)}
											onclick={() => toggleDay(weekScheduleIndex, dayScheduleIndex, day.value)}
											title={day.label}
										>
											{day.short}
										</button>
									{/each}
								</div>
							</div>
							
							<!-- Time ranges -->
							<div class="time-ranges-list">
								<label class="mini-label">Times:</label>
								{#each daySchedule.time_ranges as range, rangeIndex}
									<TimeRangeEditor
										startTime={range.start_time}
										endTime={range.end_time}
										onUpdate={(start, end) => updateTimeRange(weekScheduleIndex, dayScheduleIndex, rangeIndex, start, end)}
										onRemove={daySchedule.time_ranges.length > 1 ? () => removeTimeRange(weekScheduleIndex, dayScheduleIndex, rangeIndex) : undefined}
										removable={daySchedule.time_ranges.length > 1}
									/>
								{/each}
								
								<button
									type="button"
									class="add-mini-btn"
									onclick={() => addTimeRange(weekScheduleIndex, dayScheduleIndex)}
								>
									+ Add time slot
								</button>
							</div>
						</div>
					{/each}
					
					<button
						type="button"
						class="add-day-pattern-btn"
						onclick={() => addDaySchedule(weekScheduleIndex)}
					>
						+ Add day pattern
					</button>
				</div>
			</div>
		{/each}
	</div>
	
	<!-- Add week schedule button -->
	<button
		type="button"
		class="add-schedule-btn"
		onclick={addWeekSchedule}
	>
		+ Add week-specific pattern
	</button>
</div>

<style>
	.week-schedule-editor {
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
	
	.week-schedule-card {
		padding: 1rem;
		background: white;
		border: 2px solid #a78bfa;
		border-radius: 8px;
		transition: all 0.2s ease;
	}
	
	.week-schedule-card:hover {
		border-color: #8b5cf6;
		box-shadow: 0 2px 8px rgba(139, 92, 246, 0.15);
	}
	
	.card-header {
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
		background: #8b5cf6;
		color: white;
		font-size: 0.75rem;
		font-weight: 600;
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
	
	.week-picker {
		margin-bottom: 1rem;
	}
	
	.week-buttons {
		display: flex;
		gap: 0.5rem;
		flex-wrap: wrap;
	}
	
	.week-btn {
		flex: 1;
		min-width: 4rem;
		padding: 0.5rem 0.75rem;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		background: white;
		color: #64748b;
		font-size: 0.7rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.week-btn:hover {
		background: #f8fafc;
		border-color: #cbd5e1;
	}
	
	.week-btn.active {
		background: #8b5cf6;
		border-color: #7c3aed;
		color: white;
	}
	
	.day-schedules-section {
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
	}
	
	.day-schedule-card {
		padding: 0.75rem;
		background: #faf5ff;
		border: 1px solid #e9d5ff;
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
		color: #7c3aed;
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
	
	.day-picker,
	.time-ranges-list {
		margin-bottom: 0.75rem;
	}
	
	.mini-label {
		display: block;
		margin-bottom: 0.375rem;
		font-size: 0.65rem;
		font-weight: 600;
		color: #7c3aed;
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
		border: 1px solid #e9d5ff;
		border-radius: 3px;
		background: white;
		color: #64748b;
		font-size: 0.65rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.day-btn:hover {
		background: #f5f3ff;
		border-color: #d8b4fe;
	}
	
	.day-btn.active {
		background: #a78bfa;
		border-color: #8b5cf6;
		color: white;
	}
	
	.add-mini-btn {
		padding: 0.375rem 0.5rem;
		border: 1px dashed #d8b4fe;
		border-radius: 4px;
		background: white;
		color: #7c3aed;
		font-size: 0.7rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.add-mini-btn:hover {
		background: #faf5ff;
		border-color: #a78bfa;
	}
	
	.add-day-pattern-btn {
		padding: 0.5rem 0.75rem;
		border: 1px dashed #d8b4fe;
		border-radius: 6px;
		background: white;
		color: #8b5cf6;
		font-size: 0.75rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.add-day-pattern-btn:hover {
		background: #faf5ff;
		border-color: #a78bfa;
	}
	
	.add-schedule-btn {
		padding: 0.75rem 1rem;
		border: 2px dashed #cbd5e1;
		border-radius: 6px;
		background: white;
		color: #8b5cf6;
		font-size: 0.813rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.add-schedule-btn:hover {
		background: #faf5ff;
		border-color: #8b5cf6;
		transform: scale(1.01);
	}
</style>

