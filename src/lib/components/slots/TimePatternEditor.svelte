<script lang="ts">
	/**
	 * TimePatternEditor - Progressive disclosure time pattern editing
	 * 
	 * Modes:
	 * - Simple: Basic recurrence dropdown (daily/weekly/monthly/yearly)
	 * - Intermediate: Day-specific patterns with multiple time ranges
	 * - Advanced: Full month/week/day schedule hierarchy
	 * 
	 * Generates proper AvailabilityWindow schema objects
	 */
	
	import type { AvailabilityWindow, TimeRange, DaySchedule, WeekSchedule, MonthSchedule } from '@playnet/free-association/schemas';
	import TimeRangeEditor from './TimeRangeEditor.svelte';
	import DayScheduleEditor from './DayScheduleEditor.svelte';
	import WeekScheduleEditor from './WeekScheduleEditor.svelte';
	import MonthScheduleEditor from './MonthScheduleEditor.svelte';
	import PatternPreview from './PatternPreview.svelte';
	
	type EditorMode = 'simple' | 'intermediate' | 'advanced';
	
	interface Props {
		/** Current recurrence pattern */
		recurrence?: 'daily' | 'weekly' | 'monthly' | 'yearly' | null;
		/** Current availability window */
		availabilityWindow?: AvailabilityWindow;
		/** Start and end dates */
		startDate?: string | null;
		endDate?: string | null;
		/** Callback when pattern changes */
		onUpdate: (recurrence: string | null, availabilityWindow?: AvailabilityWindow) => void;
		/** Initial mode */
		initialMode?: EditorMode;
	}
	
	let {
		recurrence,
		availabilityWindow,
		startDate,
		endDate,
		onUpdate,
		initialMode = 'simple'
	}: Props = $props();
	
	// Editor mode state
	let currentMode = $state<EditorMode>(initialMode);
	
	// Simple mode state
	let simpleRecurrence = $state<string | null>(recurrence ?? null);
	let simpleAllDay = $state(true);
	let simpleStartTime = $state('09:00');
	let simpleEndTime = $state('17:00');
	
	// Intermediate mode state (day-specific)
	let daySchedules = $state<DaySchedule[]>([]);
	
	// Advanced mode state
	let advancedLevel = $state<'day' | 'week' | 'month'>('day');
	let weekSchedules = $state<WeekSchedule[]>([]);
	let monthSchedules = $state<MonthSchedule[]>([]);
	
	// Initialize from existing availability window
	$effect(() => {
		if (availabilityWindow) {
			// Determine mode based on what's populated
			if (availabilityWindow.month_schedules?.length) {
				currentMode = 'advanced';
				advancedLevel = 'month';
				monthSchedules = availabilityWindow.month_schedules;
			} else if (availabilityWindow.week_schedules?.length) {
				currentMode = 'advanced';
				advancedLevel = 'week';
				weekSchedules = availabilityWindow.week_schedules;
			} else if (availabilityWindow.day_schedules?.length) {
				currentMode = 'intermediate';
				daySchedules = availabilityWindow.day_schedules;
			} else if (availabilityWindow.time_ranges?.length) {
				// Simple mode with times
				currentMode = 'simple';
				simpleAllDay = false;
				const range = availabilityWindow.time_ranges[0];
				simpleStartTime = range.start_time;
				simpleEndTime = range.end_time;
			}
		}
	});
	
	function handleSimpleModeChange() {
		if (simpleRecurrence === 'custom') {
			currentMode = 'intermediate';
			// Initialize with a basic day schedule
			daySchedules = [{
				days: ['monday', 'tuesday', 'wednesday', 'thursday', 'friday'],
				time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
			}];
			return;
		}
		
		// Build availability window for simple mode
		let window: AvailabilityWindow | undefined;
		if (!simpleAllDay) {
			window = {
				time_ranges: [{
					start_time: simpleStartTime,
					end_time: simpleEndTime
				}]
			};
		}
		
		onUpdate(simpleRecurrence, window);
	}
	
	function handleTimeRangeUpdate(start: string, end: string) {
		simpleStartTime = start;
		simpleEndTime = end;
		handleSimpleModeChange();
	}
	
	function handleDaySchedulesUpdate(schedules: DaySchedule[]) {
		daySchedules = schedules;
		
		const window: AvailabilityWindow = {
			day_schedules: schedules
		};
		
		// Use weekly recurrence for day schedules
		onUpdate('weekly', window);
	}
	
	function handleWeekSchedulesUpdate(schedules: WeekSchedule[]) {
		weekSchedules = schedules;
		
		const window: AvailabilityWindow = {
			week_schedules: schedules
		};
		
		// Use monthly recurrence for week schedules
		onUpdate('monthly', window);
	}
	
	function handleMonthSchedulesUpdate(schedules: MonthSchedule[]) {
		monthSchedules = schedules;
		
		const window: AvailabilityWindow = {
			month_schedules: schedules
		};
		
		// Use yearly recurrence for month schedules
		onUpdate('yearly', window);
	}
	
	function switchToAdvanced() {
		// Convert current pattern to day schedules if coming from intermediate mode
		if (currentMode === 'intermediate' && daySchedules.length === 0) {
			daySchedules = [{
				days: ['monday', 'tuesday', 'wednesday', 'thursday', 'friday'],
				time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
			}];
		}
		
		currentMode = 'advanced';
		advancedLevel = 'day';
	}
	
	// Compute current window for preview
	const currentWindow = $derived(() => {
		if (currentMode === 'simple') {
			if (simpleAllDay) return undefined;
			return {
				time_ranges: [{ start_time: simpleStartTime, end_time: simpleEndTime }]
			};
		} else if (currentMode === 'intermediate') {
			return { day_schedules: daySchedules };
		} else if (currentMode === 'advanced') {
			if (advancedLevel === 'day') return { day_schedules: daySchedules };
			if (advancedLevel === 'week') return { week_schedules: weekSchedules };
			if (advancedLevel === 'month') return { month_schedules: monthSchedules };
		}
		return undefined;
	});
	
	const currentRecurrence = $derived(() => {
		if (currentMode === 'simple') return simpleRecurrence;
		if (currentMode === 'intermediate') return 'weekly';
		if (advancedLevel === 'week') return 'monthly';
		if (advancedLevel === 'month') return 'yearly';
		return 'daily';
	});
</script>

<div class="time-pattern-editor">
	<h4 class="editor-title">🕐 Time Pattern</h4>
	
	<!-- Mode Selector -->
	<div class="mode-selector">
		<button
			type="button"
			class="mode-btn"
			class:active={currentMode === 'simple'}
			onclick={() => currentMode = 'simple'}
		>
			Simple
		</button>
		<button
			type="button"
			class="mode-btn"
			class:active={currentMode === 'intermediate'}
			onclick={() => currentMode = 'intermediate'}
		>
			Day-Specific
		</button>
		<button
			type="button"
			class="mode-btn"
			class:active={currentMode === 'advanced'}
			onclick={() => currentMode = 'advanced'}
		>
			Advanced
		</button>
	</div>
	
	<!-- Simple Mode -->
	{#if currentMode === 'simple'}
		<div class="simple-mode">
			<div class="form-field">
				<label for="recurrence">Repeats:</label>
				<select
					id="recurrence"
					bind:value={simpleRecurrence}
					onchange={handleSimpleModeChange}
					class="recurrence-select"
				>
					<option value={null}>Does not repeat (one-time)</option>
					<option value="daily">Daily</option>
					<option value="weekly">Weekly</option>
					<option value="monthly">Monthly</option>
					<option value="yearly">Yearly</option>
					<option value="custom">Custom Pattern →</option>
				</select>
			</div>
			
			<div class="form-field">
				<label>
					<input
						type="checkbox"
						bind:checked={simpleAllDay}
						onchange={handleSimpleModeChange}
					/>
					All day
				</label>
			</div>
			
			{#if !simpleAllDay}
				<TimeRangeEditor
					startTime={simpleStartTime}
					endTime={simpleEndTime}
					onUpdate={handleTimeRangeUpdate}
					label="Time of day"
				/>
			{/if}
		</div>
	{/if}
	
	<!-- Intermediate Mode - Day-Specific -->
	{#if currentMode === 'intermediate'}
		<div class="intermediate-mode">
			<DayScheduleEditor
				schedules={daySchedules}
				onUpdate={handleDaySchedulesUpdate}
			/>
			
			<button
				type="button"
				class="mode-upgrade-btn"
				onclick={switchToAdvanced}
			>
				Need week or month patterns? → Advanced Mode
			</button>
		</div>
	{/if}
	
	<!-- Advanced Mode - Full Hierarchy -->
	{#if currentMode === 'advanced'}
		<div class="advanced-mode">
			<div class="advanced-level-selector">
				<button
					type="button"
					class="level-btn"
					class:active={advancedLevel === 'day'}
					onclick={() => advancedLevel = 'day'}
				>
					Day-Specific
				</button>
				<button
					type="button"
					class="level-btn"
					class:active={advancedLevel === 'week'}
					onclick={() => advancedLevel = 'week'}
				>
					Week-Specific
				</button>
				<button
					type="button"
					class="level-btn"
					class:active={advancedLevel === 'month'}
					onclick={() => advancedLevel = 'month'}
				>
					Month-Specific
				</button>
			</div>
			
			{#if advancedLevel === 'day'}
				<DayScheduleEditor
					schedules={daySchedules}
					onUpdate={handleDaySchedulesUpdate}
				/>
			{:else if advancedLevel === 'week'}
				<WeekScheduleEditor
					schedules={weekSchedules}
					onUpdate={handleWeekSchedulesUpdate}
				/>
			{:else if advancedLevel === 'month'}
				<MonthScheduleEditor
					schedules={monthSchedules}
					onUpdate={handleMonthSchedulesUpdate}
				/>
			{/if}
		</div>
	{/if}
	
	<!-- Pattern Preview -->
	<PatternPreview 
		recurrence={currentRecurrence() as any}
		availabilityWindow={currentWindow()} 
	/>
</div>

<style>
	.time-pattern-editor {
		padding: 1rem;
		background: #f8fafc;
		border: 1px solid #e5e7eb;
		border-radius: 8px;
	}
	
	.editor-title {
		margin: 0 0 1rem 0;
		font-size: 0.875rem;
		font-weight: 600;
		color: #1f2937;
	}
	
	.mode-selector {
		display: flex;
		gap: 0.5rem;
		margin-bottom: 1rem;
		padding: 0.25rem;
		background: white;
		border-radius: 6px;
		border: 1px solid #e5e7eb;
	}
	
	.mode-btn {
		flex: 1;
		padding: 0.5rem 1rem;
		border: none;
		border-radius: 4px;
		background: transparent;
		color: #64748b;
		font-size: 0.75rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.mode-btn:hover {
		background: #f8fafc;
		color: #475569;
	}
	
	.mode-btn.active {
		background: #3b82f6;
		color: white;
	}
	
	.simple-mode,
	.intermediate-mode,
	.advanced-mode {
		display: flex;
		flex-direction: column;
		gap: 1rem;
	}
	
	.form-field {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}
	
	.form-field label {
		font-size: 0.75rem;
		font-weight: 600;
		color: #475569;
	}
	
	.recurrence-select {
		padding: 0.5rem 0.75rem;
		border: 1px solid #cbd5e1;
		border-radius: 6px;
		font-size: 0.875rem;
		color: #1f2937;
		background: white;
		transition: all 0.2s ease;
	}
	
	.recurrence-select:focus {
		outline: none;
		border-color: #3b82f6;
		box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
	}
	
	.mode-upgrade-btn {
		padding: 0.75rem 1rem;
		border: 1px dashed #cbd5e1;
		border-radius: 6px;
		background: white;
		color: #3b82f6;
		font-size: 0.75rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.mode-upgrade-btn:hover {
		background: #eff6ff;
		border-color: #3b82f6;
	}
	
	.advanced-level-selector {
		display: flex;
		gap: 0.5rem;
		padding: 0.25rem;
		background: white;
		border-radius: 6px;
		border: 1px solid #e5e7eb;
	}
	
	.level-btn {
		flex: 1;
		padding: 0.5rem;
		border: none;
		border-radius: 4px;
		background: transparent;
		color: #64748b;
		font-size: 0.7rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
	}
	
	.level-btn:hover {
		background: #f8fafc;
		color: #475569;
	}
	
	.level-btn.active {
		background: #10b981;
		color: white;
	}
</style>

