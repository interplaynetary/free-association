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
	
	/* 
	   NEW PREMIUM UI INTEGRATION 
	   We are replacing the old mode selector with the SentenceBuilder as the primary interface.
	   "Visual Rhythm" and "Advanced" views can be added as toggles later.
	*/
	
	import SentenceBuilder from './sentence/SentenceBuilder.svelte';
	
	/* ... existing imports for fallback or advanced mode if we keep them hidden ... */
	
	// For now, simpler is better. We just delegate to SentenceBuilder.
	
	function handleSentenceHelperUpdate(newWindow: AvailabilityWindow) {
		// SentenceBuilder returns a full AvailabilityWindow.
		// We determine recurrence roughly based on what's inside.
		// Detailed recurrence (daily/weekly/monthly) is implied by the window structure.
		// For the protocol, we can often leave 'recurrence' string as metadata or derive it.
		// If the window has month_schedules -> 'yearly'
		// If week_schedules -> 'monthly'
		// If day_schedules -> 'weekly'
		// If just time_ranges -> 'daily' (or 'none' if assumed one-off, but the editor implies availability pattern)
		
		let derivedRecurrence = 'daily';
		if (newWindow.month_schedules?.length) derivedRecurrence = 'yearly';
		else if (newWindow.week_schedules?.length) derivedRecurrence = 'monthly';
		else if (newWindow.day_schedules?.length) derivedRecurrence = 'weekly';
		
		onUpdate(derivedRecurrence, newWindow);
	}
</script>

<div class="time-pattern-editor-premium">
	<!-- 
		The "Natural Language" Interface 
		This replaces the old "Simple/Intermediate/Advanced" tabs with a unified
		progressive disclosure experience.
	-->
	<SentenceBuilder 
		availabilityWindow={availabilityWindow}
		onChange={handleSentenceHelperUpdate}
	/>
	
	<!-- 
		Future: Add "Timeline View" toggle here.
	-->
</div>

<style>
	.time-pattern-editor-premium {
		/* Premium container styles */
		background: linear-gradient(to bottom right, #f8fafc, #f1f5f9);
		border: 1px solid #e2e8f0;
		border-radius: 16px; /* Larger, softer radius */
		padding: 1.5rem;
		box-shadow: 
			0 4px 6px -1px rgba(0, 0, 0, 0.05),
			0 2px 4px -1px rgba(0, 0, 0, 0.03);
	}
</style>

