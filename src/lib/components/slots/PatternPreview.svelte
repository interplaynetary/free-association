<script lang="ts">
	/**
	 * PatternPreview - Visual preview of time patterns
	 * 
	 * Shows a calendar-like visualization of when a pattern is active
	 * Helps users understand complex recurring patterns
	 */
	
	import type { AvailabilityWindow, DayOfWeek } from '$lib/protocol/core/schemas';
	
	interface Props {
		recurrence?: 'daily' | 'weekly' | 'monthly' | 'yearly' | null;
		availabilityWindow?: AvailabilityWindow;
	}
	
	let { recurrence, availabilityWindow }: Props = $props();
	
	const DAYS: DayOfWeek[] = ['monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'];
	const DAY_LABELS = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'];
	
	// Check if a day is active based on availability window
	function isDayActive(day: DayOfWeek): boolean {
		if (!availabilityWindow) return true; // All days if no window
		
		// Check day_schedules
		if (availabilityWindow.day_schedules?.length) {
			return availabilityWindow.day_schedules.some(schedule => 
				schedule.days.includes(day)
			);
		}
		
		// Check week_schedules
		if (availabilityWindow.week_schedules?.length) {
			return availabilityWindow.week_schedules.some(weekSchedule =>
				weekSchedule.day_schedules.some(daySchedule =>
					daySchedule.days.includes(day)
				)
			);
		}
		
		// Check month_schedules
		if (availabilityWindow.month_schedules?.length) {
			return availabilityWindow.month_schedules.some(monthSchedule => {
				if (monthSchedule.day_schedules?.length) {
					return monthSchedule.day_schedules.some(daySchedule =>
						daySchedule.days.includes(day)
					);
				}
				if (monthSchedule.week_schedules?.length) {
					return monthSchedule.week_schedules.some(weekSchedule =>
						weekSchedule.day_schedules.some(daySchedule =>
							daySchedule.days.includes(day)
						)
					);
				}
				return false;
			});
		}
		
		return true; // Default: all days
	}
	
	// Get time ranges for a day
	function getTimeRangesForDay(day: DayOfWeek): string[] {
		if (!availabilityWindow) return ['All day'];
		
		// Simple time_ranges (applies to all days)
		if (availabilityWindow.time_ranges?.length) {
			return availabilityWindow.time_ranges.map(r => `${r.start_time}-${r.end_time}`);
		}
		
		// Day-specific schedules
		if (availabilityWindow.day_schedules?.length) {
			const relevantSchedules = availabilityWindow.day_schedules.filter(s => s.days.includes(day));
			return relevantSchedules.flatMap(s => 
				s.time_ranges.map(r => `${r.start_time}-${r.end_time}`)
			);
		}
		
		return ['All day'];
	}
	
	// Get summary text
	const summaryText = $derived(() => {
		if (!recurrence || recurrence === null) return 'One-time event';
		
		if (!availabilityWindow) {
			return `Repeats ${recurrence}, all day`;
		}
		
		const activeDays = DAYS.filter(isDayActive);
		if (activeDays.length === 0) return 'No active days';
		if (activeDays.length === 7) return `Repeats ${recurrence}, every day`;
		
		return `Repeats ${recurrence}, on ${activeDays.length} day${activeDays.length > 1 ? 's' : ''}`;
	});
</script>

<div class="pattern-preview">
	<h5 class="preview-title">📊 Pattern Preview</h5>
	
	<div class="summary-text">{summaryText()}</div>
	
	{#if recurrence && recurrence !== null}
		<div class="week-grid">
			{#each DAYS as day, index}
				{@const isActive = isDayActive(day)}
				{@const timeRanges = isActive ? getTimeRangesForDay(day) : []}
				
				<div class="day-cell" class:active={isActive} class:inactive={!isActive}>
					<div class="day-label">{DAY_LABELS[index]}</div>
					{#if isActive}
						<div class="time-ranges">
							{#each timeRanges as range}
								<div class="time-range-chip">{range}</div>
							{/each}
						</div>
					{/if}
				</div>
			{/each}
		</div>
	{/if}
</div>

<style>
	.pattern-preview {
		padding: 1rem;
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		border-radius: 8px;
		color: white;
	}
	
	.preview-title {
		margin: 0 0 0.75rem 0;
		font-size: 0.813rem;
		font-weight: 600;
		color: white;
	}
	
	.summary-text {
		margin-bottom: 1rem;
		padding: 0.5rem 0.75rem;
		background: rgba(255, 255, 255, 0.2);
		border-radius: 4px;
		font-size: 0.75rem;
		font-weight: 500;
		backdrop-filter: blur(10px);
	}
	
	.week-grid {
		display: grid;
		grid-template-columns: repeat(7, 1fr);
		gap: 0.375rem;
	}
	
	.day-cell {
		padding: 0.5rem 0.25rem;
		border-radius: 4px;
		background: rgba(255, 255, 255, 0.15);
		backdrop-filter: blur(10px);
		text-align: center;
		transition: all 0.2s ease;
	}
	
	.day-cell.active {
		background: rgba(255, 255, 255, 0.95);
		color: #5b21b6;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
	}
	
	.day-cell.inactive {
		opacity: 0.4;
		background: rgba(0, 0, 0, 0.1);
	}
	
	.day-label {
		font-size: 0.65rem;
		font-weight: 700;
		text-transform: uppercase;
		letter-spacing: 0.05em;
		margin-bottom: 0.25rem;
	}
	
	.time-ranges {
		display: flex;
		flex-direction: column;
		gap: 0.25rem;
		margin-top: 0.375rem;
	}
	
	.time-range-chip {
		padding: 0.125rem 0.25rem;
		background: #7c3aed;
		color: white;
		border-radius: 3px;
		font-size: 0.55rem;
		font-weight: 600;
		white-space: nowrap;
		overflow: hidden;
		text-overflow: ellipsis;
	}
</style>

