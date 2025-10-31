/**
 * Format availability_window for display in CLI
 */

import type { AvailabilityWindow } from '../../schemas';

export function formatScheduleForDisplay(window: AvailabilityWindow | undefined): string[] {
	if (!window) return [];
	
	const lines: string[] = [];
	
	// Monthly/Yearly with month_schedules
	if (window.month_schedules && window.month_schedules.length > 0) {
		const months = window.month_schedules.map(ms => {
			const monthNames = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
			return monthNames[ms.month - 1];
		}).join(', ');
		lines.push(`Months: ${months}`);
		
		// Check first month_schedule for weeks and days
		const firstMonth = window.month_schedules[0];
		if (firstMonth.week_schedules && firstMonth.week_schedules.length > 0) {
			const weeks = firstMonth.week_schedules[0].weeks;
			const weekLabels = weeks.map(w => {
				const labels = ['1st', '2nd', '3rd', '4th', '5th'];
				return labels[w - 1];
			}).join(', ');
			lines.push(`Weeks: ${weekLabels}`);
			
			const days = firstMonth.week_schedules[0].day_schedules?.[0]?.days || [];
			if (days.length > 0) {
				const dayLabels = days.map(d => d.charAt(0).toUpperCase() + d.slice(1)).join(', ');
				lines.push(`Days: ${dayLabels}`);
			}
			
			const timeRanges = firstMonth.week_schedules[0].day_schedules?.[0]?.time_ranges || [];
			if (timeRanges.length > 0) {
				const times = timeRanges.map(tr => `${tr.start_time}-${tr.end_time}`).join(', ');
				lines.push(`Times: ${times}`);
			}
		} else if (firstMonth.day_schedules && firstMonth.day_schedules.length > 0) {
			const days = firstMonth.day_schedules[0].days;
			const dayLabels = days.map(d => d.charAt(0).toUpperCase() + d.slice(1)).join(', ');
			lines.push(`Days: ${dayLabels}`);
			
			const timeRanges = firstMonth.day_schedules[0].time_ranges;
			const times = timeRanges.map(tr => `${tr.start_time}-${tr.end_time}`).join(', ');
			lines.push(`Times: ${times}`);
		}
	}
	// Weekly/Monthly with week_schedules
	else if (window.week_schedules && window.week_schedules.length > 0) {
		const weeks = window.week_schedules[0].weeks;
		const weekLabels = weeks.map(w => {
			const labels = ['1st', '2nd', '3rd', '4th', '5th'];
			return labels[w - 1];
		}).join(', ');
		lines.push(`Weeks: ${weekLabels}`);
		
		const days = window.week_schedules[0].day_schedules[0].days;
		const dayLabels = days.map(d => d.charAt(0).toUpperCase() + d.slice(1)).join(', ');
		lines.push(`Days: ${dayLabels}`);
		
		const timeRanges = window.week_schedules[0].day_schedules[0].time_ranges;
		const times = timeRanges.map(tr => `${tr.start_time}-${tr.end_time}`).join(', ');
		lines.push(`Times: ${times}`);
	}
	// Weekly with day_schedules
	else if (window.day_schedules && window.day_schedules.length > 0) {
		const days = window.day_schedules[0].days;
		const dayLabels = days.map(d => d.charAt(0).toUpperCase() + d.slice(1)).join(', ');
		lines.push(`Days: ${dayLabels}`);
		
		const timeRanges = window.day_schedules[0].time_ranges;
		const times = timeRanges.map(tr => `${tr.start_time}-${tr.end_time}`).join(', ');
		lines.push(`Times: ${times}`);
	}
	// Daily with just time_ranges
	else if (window.time_ranges && window.time_ranges.length > 0) {
		const times = window.time_ranges.map(tr => `${tr.start_time}-${tr.end_time}`).join(', ');
		lines.push(`Times: ${times}`);
	}
	
	return lines;
}

export function formatScheduleOneLine(window: AvailabilityWindow | undefined, recurrence: string | null): string {
	if (!window) {
		return recurrence ? recurrence.charAt(0).toUpperCase() + recurrence.slice(1) : 'One-time';
	}
	
	const parts: string[] = [recurrence ? recurrence.charAt(0).toUpperCase() + recurrence.slice(1) : ''];
	
	// Get days if present
	if (window.month_schedules?.[0]?.week_schedules?.[0]?.day_schedules?.[0]?.days) {
		const days = window.month_schedules[0].week_schedules[0].day_schedules[0].days;
		parts.push(`on ${days.map(d => d.slice(0, 3)).join(', ')}`);
	} else if (window.month_schedules?.[0]?.day_schedules?.[0]?.days) {
		const days = window.month_schedules[0].day_schedules[0].days;
		parts.push(`on ${days.map(d => d.slice(0, 3)).join(', ')}`);
	} else if (window.week_schedules?.[0]?.day_schedules?.[0]?.days) {
		const days = window.week_schedules[0].day_schedules[0].days;
		parts.push(`on ${days.map(d => d.slice(0, 3)).join(', ')}`);
	} else if (window.day_schedules?.[0]?.days) {
		const days = window.day_schedules[0].days;
		parts.push(`on ${days.map(d => d.slice(0, 3)).join(', ')}`);
	}
	
	// Get time range
	const timeRange = 
		window.month_schedules?.[0]?.week_schedules?.[0]?.day_schedules?.[0]?.time_ranges?.[0] ||
		window.month_schedules?.[0]?.day_schedules?.[0]?.time_ranges?.[0] ||
		window.week_schedules?.[0]?.day_schedules?.[0]?.time_ranges?.[0] ||
		window.day_schedules?.[0]?.time_ranges?.[0] ||
		window.time_ranges?.[0];
	
	if (timeRange) {
		parts.push(`${timeRange.start_time}-${timeRange.end_time}`);
	}
	
	return parts.filter(p => p).join(' ');
}

