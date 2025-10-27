/**
 * Slot-to-Slot Matching Logic v5 - Multi-Dimensional Framework
 * 
 * Pure functions for matching need slots to availability slots based on:
 * - **Type compatibility (need_type_id matching) - NEW in v5**
 * - Time compatibility (date/time range overlap)
 * - Location compatibility (city/country/coordinates/online)
 * - Quantity constraints
 * 
 * v5 changes:
 * - Uses v5 schemas with required need_type_id
 * - Type matching is built into slotsCompatible()
 * - Pure multi-dimensional design
 */

import type { AvailabilitySlot, NeedSlot, AvailabilityWindow, TimeRange, DayOfWeek, DaySchedule, WeekSchedule, MonthSchedule } from './schemas';

// ═══════════════════════════════════════════════════════════════════
// RECURRENCE TRACK IDENTIFICATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Determine if a slot represents a recurring or one-time commitment
 * 
 * ASYMMETRIC TRACK MODEL:
 * - Capacity: Unified (can serve any compatible need, regardless of recurrence)
 * - Needs: Separated into two tracks:
 *   - "recurring": Ongoing commitments (weekly tutoring, monthly groceries, etc.)
 *   - "onetime": Discrete requests (help moving, one workshop, etc.)
 * 
 * This separation happens naturally at the slot level:
 * - Need slots with recurrence are "recurring track"
 * - Need slots without recurrence are "onetime track"
 * - Capacity slots match both tracks (no filtering by recurrence)
 * 
 * @param slot - Any slot (need or capacity)
 * @returns "recurring" if slot has a recurrence pattern, "onetime" otherwise
 */
export function getRecurrenceTrack(
	slot: { recurrence?: string | null }
): 'recurring' | 'onetime' {
	// Check if recurrence field is present and meaningful
	if (slot.recurrence && 
	    slot.recurrence !== '' && 
	    slot.recurrence !== 'none' &&
	    slot.recurrence !== null) {
		return 'recurring';
	}
	return 'onetime';
}

// ═══════════════════════════════════════════════════════════════════
// TIMEZONE-AWARE TIME UTILITIES
// ═══════════════════════════════════════════════════════════════════
/**
 * TIMEZONE SUPPORT IN FREE-ASSOCIATION v5
 * 
 * The matching system is fully timezone-aware, enabling global coordination:
 * 
 * **Problem:**
 * - Provider in NYC offers capacity "2pm-4pm" (EST/UTC-5)
 * - Recipient in London needs help "7pm-9pm" (GMT/UTC+0)
 * - These times OVERLAP! (2pm EST = 7pm GMT)
 * 
 * **Solution:**
 * - All times are converted to UTC before comparison
 * - Uses IANA timezone strings: "America/New_York", "Europe/London", "Asia/Tokyo"
 * - Handles DST automatically via JavaScript Intl API
 * - If no timezone specified, assumes UTC
 * 
 * **Schema:**
 * Both `AvailabilitySlot` and `NeedSlot` have:
 * - `time_zone?: string` (IANA timezone, e.g., "America/Chicago")
 * - All times in `availability_window` are interpreted in this timezone
 * 
 * **Matching Logic:**
 * 1. Extract times from both slots (in their respective timezones)
 * 2. Convert both to UTC using the reference date
 * 3. Compare UTC times for overlap
 * 4. Match if overlapping in UTC time
 * 
 * **Current Coverage:**
 * ✅ One-time vs Recurring: Full timezone support
 * ✅ One-time vs One-time: Falls back to date comparison (works across timezones)
 * ✅ Recurring vs Recurring: Full timezone support with day-shift logic!
 * 
 * **Example:**
 * ```typescript
 * const nycProvider = {
 *   availability_window: {
 *     day_schedules: [{
 *       days: ['monday'],
 *       time_ranges: [{ start_time: '14:00', end_time: '16:00' }]
 *     }]
 *   },
 *   time_zone: 'America/New_York'  // UTC-5
 * };
 * 
 * const londonRecipient = {
 *   start_date: '2024-03-04',  // Monday
 *   availability_window: {
 *     time_ranges: [{ start_time: '19:00', end_time: '21:00' }]
 *   },
 *   time_zone: 'Europe/London'  // UTC+0
 * };
 * 
 * // Will match! 14:00 EST = 19:00 GMT, 16:00 EST = 21:00 GMT
 * ```
 * 
 * **Day-Shift Example (Recurring-to-Recurring):**
 * ```typescript
 * const laProvider = {
 *   recurrence: 'weekly',
 *   availability_window: {
 *     day_schedules: [{
 *       days: ['monday'],
 *       time_ranges: [{ start_time: '23:00', end_time: '23:59' }]
 *     }]
 *   },
 *   time_zone: 'America/Los_Angeles'  // PST (UTC-8)
 * };
 * 
 * const parisRecipient = {
 *   recurrence: 'weekly',
 *   availability_window: {
 *     day_schedules: [{
 *       days: ['tuesday'],
 *       time_ranges: [{ start_time: '08:00', end_time: '10:00' }]
 *     }]
 *   },
 *   time_zone: 'Europe/Paris'  // CET (UTC+1)
 * };
 * 
 * // Will match! Monday 11pm PST = Tuesday 8am CET (day shifts!)
 * ```
 */

/**
 * Convert a time string (HH:MM) from one timezone to UTC
 * 
 * @param timeStr - Time in HH:MM format (e.g., "14:30")
 * @param dateStr - Date in YYYY-MM-DD format (needed for DST calculations)
 * @param timezone - IANA timezone string (e.g., "America/New_York", "Europe/London")
 * @returns Time in HH:MM format in UTC
 * 
 * Strategy: Create a UTC date, format it in the target timezone to get the offset,
 * then apply the reverse offset to convert our local time to UTC.
 */
function convertTimeToUTC(timeStr: string, dateStr: string, timezone?: string): string {
	if (!timezone || timezone === 'UTC' || timezone === 'Etc/UTC') {
		return timeStr; // Already UTC
	}
	
	try {
		// Parse input
		const [hours, minutes] = timeStr.split(':').map(Number);
		const [year, month, day] = dateStr.split('-').map(Number);
		
		// Create a reference UTC date at noon on the target date
		const refUTC = Date.UTC(year, month - 1, day, 12, 0, 0);
		const refDate = new Date(refUTC);
		
		// Format this UTC date in the target timezone
		const formatter = new Intl.DateTimeFormat('en-US', {
			timeZone: timezone,
			year: 'numeric',
			month: '2-digit',
			day: '2-digit',
			hour: '2-digit',
			minute: '2-digit',
			second: '2-digit',
			hour12: false
		});
		
		const parts = formatter.formatToParts(refDate);
		const tzYear = parseInt(parts.find(p => p.type === 'year')!.value);
		const tzMonth = parseInt(parts.find(p => p.type === 'month')!.value);
		const tzDay = parseInt(parts.find(p => p.type === 'day')!.value);
		const tzHour = parseInt(parts.find(p => p.type === 'hour')!.value);
		const tzMinute = parseInt(parts.find(p => p.type === 'minute')!.value);
		
		// Calculate offset: what is the difference between UTC noon and local noon?
		// If UTC is 12:00 and timezone shows 07:00, offset is -5 hours (UTC is 5 hours ahead)
		const offsetHours = 12 - tzHour;
		const offsetMinutes = 0 - tzMinute;
		const dayShift = tzDay - day;
		
		// Apply offset to convert our local time to UTC
		let utcHours = hours + offsetHours - (dayShift * 24);
		let utcMinutes = minutes + offsetMinutes;
		
		// Normalize minutes
		if (utcMinutes < 0) {
			utcMinutes += 60;
			utcHours -= 1;
		} else if (utcMinutes >= 60) {
			utcMinutes -= 60;
			utcHours += 1;
		}
		
		// Normalize hours
		utcHours = ((utcHours % 24) + 24) % 24;
		
		return `${utcHours.toString().padStart(2, '0')}:${utcMinutes.toString().padStart(2, '0')}`;
	} catch (e) {
		console.warn(`[TIMEZONE] Failed to convert ${timeStr} from ${timezone} to UTC:`, e);
		return timeStr;
	}
}

/**
 * Convert time ranges to UTC given a timezone and date context
 */
function convertTimeRangesToUTC(
	ranges: TimeRange[],
	dateStr: string,
	timezone?: string
): TimeRange[] {
	if (!timezone || timezone === 'UTC') {
		return ranges; // Already UTC
	}
	
	return ranges.map(range => ({
		start_time: convertTimeToUTC(range.start_time, dateStr, timezone),
		end_time: convertTimeToUTC(range.end_time, dateStr, timezone)
	}));
}

/**
 * Convert a day schedule from one timezone to UTC, tracking day shifts
 * 
 * @returns Array of { day, timeRanges } where day may have shifted due to timezone conversion
 * 
 * Example: "Monday 11pm PST" becomes "Tuesday 7am UTC" (day shifts from monday to tuesday)
 */
function convertDayScheduleToUTC(
	daySchedule: DaySchedule,
	sampleDate: string,
	timezone?: string
): Array<{ day: DayOfWeek; timeRanges: TimeRange[] }> {
	if (!timezone || timezone === 'UTC') {
		// Already UTC, no day shift
		return daySchedule.days.map(day => ({
			day,
			timeRanges: daySchedule.time_ranges
		}));
	}
	
	const result: Array<{ day: DayOfWeek; timeRanges: TimeRange[] }> = [];
	const dayNames: DayOfWeek[] = ['sunday', 'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday'];
	
	// For each day in the schedule, convert each time range to UTC
	// and track which UTC day it falls on
	for (const day of daySchedule.days) {
		// Find a sample date that falls on this day of week
		const sampleDateForDay = getDateStringForDayOfWeek(day, sampleDate);
		
		for (const range of daySchedule.time_ranges) {
			// Convert start time to UTC and determine which day it's on
			const startTimeUTC = convertTimeToUTC(range.start_time, sampleDateForDay, timezone);
			const endTimeUTC = convertTimeToUTC(range.end_time, sampleDateForDay, timezone);
			
			// To get the UTC day, we need to know what UTC datetime corresponds to 
			// "sampleDateForDay at range.start_time in timezone"
			const [year, month, dayNum] = sampleDateForDay.split('-').map(Number);
			const [startHour, startMin] = range.start_time.split(':').map(Number);
			
			// Create a UTC date at noon on the sample date
			const refUTC = Date.UTC(year, month - 1, dayNum, 12, 0, 0);
			const refDate = new Date(refUTC);
			
			// Format this UTC date in the target timezone
			const formatter = new Intl.DateTimeFormat('en-US', {
				timeZone: timezone,
				year: 'numeric',
				month: '2-digit',
				day: '2-digit',
				hour: '2-digit',
				minute: '2-digit',
				hour12: false
			});
			
			const parts = formatter.formatToParts(refDate);
			const tzHour = parseInt(parts.find(p => p.type === 'hour')!.value);
			const tzDay = parseInt(parts.find(p => p.type === 'day')!.value);
			
			// Calculate the offset
			const offsetHours = 12 - tzHour;
			const dayShift = tzDay - dayNum;
			
			// Apply offset to get UTC day
			// If startHour + offsetHours crosses midnight, adjust the day
			let utcHour = startHour + offsetHours - (dayShift * 24);
			let utcDayIndex = refDate.getUTCDay();
			
			// Adjust for day boundary crossings
			if (utcHour < 0) {
				utcDayIndex = (utcDayIndex - 1 + 7) % 7;
			} else if (utcHour >= 24) {
				utcDayIndex = (utcDayIndex + 1) % 7;
			}
			
			const utcDay = dayNames[utcDayIndex];
			
			// Add this converted range
			const existing = result.find(r => r.day === utcDay);
			if (existing) {
				existing.timeRanges.push({ start_time: startTimeUTC, end_time: endTimeUTC });
			} else {
				result.push({
					day: utcDay,
					timeRanges: [{ start_time: startTimeUTC, end_time: endTimeUTC }]
				});
			}
		}
	}
	
	return result;
}

/**
 * Get a date string (YYYY-MM-DD) that falls on the specified day of week,
 * starting from the reference date
 */
function getDateStringForDayOfWeek(targetDay: DayOfWeek, referenceDate: string): string {
	const dayNames: DayOfWeek[] = ['sunday', 'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday'];
	const targetDayIndex = dayNames.indexOf(targetDay);
	
	// Parse reference date
	const [year, month, day] = referenceDate.split('-').map(Number);
	const refDate = new Date(year, month - 1, day);
	const refDayIndex = refDate.getDay();
	
	// Calculate days to add (0-6)
	let daysToAdd = targetDayIndex - refDayIndex;
	if (daysToAdd < 0) daysToAdd += 7;
	
	// Create target date
	const targetDate = new Date(refDate);
	targetDate.setDate(targetDate.getDate() + daysToAdd);
	
	// Format as YYYY-MM-DD
	const y = targetDate.getFullYear();
	const m = String(targetDate.getMonth() + 1).padStart(2, '0');
	const d = String(targetDate.getDate()).padStart(2, '0');
	
	return `${y}-${m}-${d}`;
}

// ═══════════════════════════════════════════════════════════════════
// STRUCTURED AVAILABILITY WINDOW MATCHING
// ═══════════════════════════════════════════════════════════════════

/**
 * Check if two time ranges overlap
 * Time format: "HH:MM" (24-hour)
 * 
 * IMPORTANT: Times should already be in the same timezone (ideally UTC) before calling this.
 * Use convertTimeRangesToUTC() before comparing if slots have different timezones.
 */
function timeRangesOverlapSimple(range1: TimeRange, range2: TimeRange): boolean {
	const start1 = range1.start_time;
	const end1 = range1.end_time;
	const start2 = range2.start_time;
	const end2 = range2.end_time;
	
	// Simple string comparison works for HH:MM format in the same timezone
	// Ranges overlap if: start1 < end2 AND start2 < end1
	return start1 < end2 && start2 < end1;
}

/**
 * Check if any time ranges from two arrays overlap
 */
function anyTimeRangesOverlap(ranges1?: TimeRange[], ranges2?: TimeRange[]): boolean {
	// If either has no time ranges specified, be optimistic (assume all day)
	if (!ranges1 || ranges1.length === 0 || !ranges2 || ranges2.length === 0) {
		return true;
	}
	
	// Check if any pair overlaps
	for (const r1 of ranges1) {
		for (const r2 of ranges2) {
			if (timeRangesOverlapSimple(r1, r2)) {
				return true;
			}
		}
	}
	
	return false;
}

/**
 * Extract all unique days from day_schedules
 */
function getDaysFromSchedules(schedules: DaySchedule[]): DayOfWeek[] {
	const days = new Set<DayOfWeek>();
	for (const schedule of schedules) {
		for (const day of schedule.days) {
			days.add(day);
		}
	}
	return Array.from(days);
}

/**
 * Get time ranges for a specific day from day_schedules
 * Returns all time ranges that apply to this day
 */
function getTimeRangesForDay(schedules: DaySchedule[], targetDay: DayOfWeek): TimeRange[] {
	const ranges: TimeRange[] = [];
	for (const schedule of schedules) {
		if (schedule.days.includes(targetDay)) {
			ranges.push(...schedule.time_ranges);
		}
	}
	return ranges;
}

/**
 * Check if two day_schedules arrays have any overlap
 * Returns true if they share any day AND have overlapping times on that day
 */
function daySchedulesOverlap(schedules1: DaySchedule[], schedules2: DaySchedule[]): boolean {
	const days1 = getDaysFromSchedules(schedules1);
	const days2 = getDaysFromSchedules(schedules2);
	
	// Find common days
	const commonDays = days1.filter(day => days2.includes(day));
	
	if (commonDays.length === 0) {
		return false; // No common days
	}
	
	// For each common day, check if times overlap
	for (const day of commonDays) {
		const ranges1 = getTimeRangesForDay(schedules1, day);
		const ranges2 = getTimeRangesForDay(schedules2, day);
		
		if (anyTimeRangesOverlap(ranges1, ranges2)) {
			return true; // Found a day with overlapping times
		}
	}
	
	return false; // No day had overlapping times
}

/**
 * Extract day_schedules from week_schedules for specific weeks
 */
function getDaySchedulesFromWeeks(weekSchedules: WeekSchedule[], targetWeeks?: number[]): DaySchedule[] {
	const daySchedules: DaySchedule[] = [];
	
	for (const weekSched of weekSchedules) {
		// If target weeks specified, only include matching weeks
		if (targetWeeks && targetWeeks.length > 0) {
			const hasCommonWeek = weekSched.weeks.some(w => targetWeeks.includes(w));
			if (!hasCommonWeek) {
				continue;
			}
		}
		
		// Add all day schedules from this week
		daySchedules.push(...weekSched.day_schedules);
	}
	
	return daySchedules;
}

/**
 * Extract patterns from month_schedules for specific months
 * Returns day_schedules that apply to those months
 */
function getDaySchedulesFromMonths(monthSchedules: MonthSchedule[], targetMonths?: number[]): DaySchedule[] {
	const daySchedules: DaySchedule[] = [];
	
	for (const monthSched of monthSchedules) {
		// If target months specified, only include matching months
		if (targetMonths && targetMonths.length > 0) {
			if (!targetMonths.includes(monthSched.month)) {
				continue;
			}
		}
		
		// Extract day schedules from this month
		if (monthSched.week_schedules && monthSched.week_schedules.length > 0) {
			// Month has week-specific patterns
			daySchedules.push(...getDaySchedulesFromWeeks(monthSched.week_schedules));
		} else if (monthSched.day_schedules && monthSched.day_schedules.length > 0) {
			// Month has day-specific patterns
			daySchedules.push(...monthSched.day_schedules);
		}
		// Note: time_ranges are handled separately
	}
	
	return daySchedules;
}

/**
 * Check if a MonthSchedule applies to a specific date
 * Returns the day_schedules that apply, or null if month doesn't match
 */
function getMonthScheduleForDate(monthSched: MonthSchedule, dateString: string): DaySchedule[] | null {
	const date = new Date(dateString);
	const month = date.getMonth() + 1; // 1-12
	
	if (month !== monthSched.month) {
		return null; // Wrong month
	}
	
	const dayOfWeek = getDayOfWeekFromDate(dateString);
	const weekOfMonth = getWeekOfMonth(dateString);
	
	// Check week_schedules (most specific)
	if (monthSched.week_schedules && monthSched.week_schedules.length > 0) {
		const relevantWeekSchedules = monthSched.week_schedules.filter(ws => 
			ws.weeks.includes(weekOfMonth)
		);
		
		if (relevantWeekSchedules.length === 0) {
			return null; // This week isn't available
		}
		
		// Collect day schedules from matching weeks
		const daySchedules: DaySchedule[] = [];
		for (const ws of relevantWeekSchedules) {
			daySchedules.push(...ws.day_schedules);
		}
		return daySchedules;
	}
	
	// Fall back to day_schedules (applies to all weeks in this month)
	if (monthSched.day_schedules && monthSched.day_schedules.length > 0) {
		return monthSched.day_schedules;
	}
	
	// No specific day/week schedules - would use time_ranges (handled by caller)
	return [];
}

/**
 * Check if two week-of-month arrays have any overlap
 */
function weeksOfMonthOverlap(weeks1?: number[], weeks2?: number[]): boolean {
	// If either has no weeks specified, be optimistic (assume all weeks)
	if (!weeks1 || weeks1.length === 0 || !weeks2 || weeks2.length === 0) {
		return true;
	}
	
	// Check for any common week
	return weeks1.some(week => weeks2.includes(week));
}

/**
 * Check if two month arrays have any overlap
 */
function monthsOverlap(months1?: number[], months2?: number[]): boolean {
	// If either has no months specified, be optimistic (assume all months)
	if (!months1 || months1.length === 0 || !months2 || months2.length === 0) {
		return true;
	}
	
	// Check for any common month
	return months1.some(month => months2.includes(month));
}

/**
 * Check if two structured availability windows overlap with timezone awareness
 * 
 * Converts both windows to UTC before comparing, handling day-shift logic.
 * 
 * @param window1 - First availability window
 * @param window2 - Second availability window
 * @param timezone1 - Timezone for window1
 * @param timezone2 - Timezone for window2
 * @param sampleDate - Reference date for timezone conversions (default: '2024-01-01')
 * @returns true if windows overlap in UTC time
 */
function availabilityWindowsOverlapWithTimezone(
	window1: AvailabilityWindow | undefined,
	window2: AvailabilityWindow | undefined,
	timezone1?: string,
	timezone2?: string,
	sampleDate: string = '2024-01-01'
): boolean {
	if (!window1 || !window2) {
		return true; // Be optimistic
	}
	
	// If both are UTC or no timezone specified, use the standard comparison
	if ((!timezone1 || timezone1 === 'UTC') && (!timezone2 || timezone2 === 'UTC')) {
		return availabilityWindowsOverlap(window1, window2);
	}
	
	// Convert both windows' day_schedules to UTC
	// For simplicity, we'll focus on day_schedules (most common use case)
	let dayScheds1UTC: Array<{ day: DayOfWeek; timeRanges: TimeRange[] }> = [];
	let dayScheds2UTC: Array<{ day: DayOfWeek; timeRanges: TimeRange[] }> = [];
	
	// Extract day schedules from window1
	if (window1.day_schedules && window1.day_schedules.length > 0) {
		for (const sched of window1.day_schedules) {
			const convertedScheds = convertDayScheduleToUTC(sched, sampleDate, timezone1);
			dayScheds1UTC.push(...convertedScheds);
		}
	}
	
	// Extract day schedules from window2
	if (window2.day_schedules && window2.day_schedules.length > 0) {
		for (const sched of window2.day_schedules) {
			const convertedScheds = convertDayScheduleToUTC(sched, sampleDate, timezone2);
			dayScheds2UTC.push(...convertedScheds);
		}
	}
	
	// If we have converted day schedules, check for overlaps
	if (dayScheds1UTC.length > 0 && dayScheds2UTC.length > 0) {
		// Find common days in UTC
		const days1 = new Set(dayScheds1UTC.map(d => d.day));
		const days2 = new Set(dayScheds2UTC.map(d => d.day));
		
		for (const day of days1) {
			if (days2.has(day)) {
				// This day appears in both - check if times overlap
				const ranges1 = dayScheds1UTC.filter(d => d.day === day).flatMap(d => d.timeRanges);
				const ranges2 = dayScheds2UTC.filter(d => d.day === day).flatMap(d => d.timeRanges);
				
				if (anyTimeRangesOverlap(ranges1, ranges2)) {
					return true; // Found overlapping day and time in UTC
				}
			}
		}
		
		return false; // No overlapping days or times
	}
	
	// Fallback to standard comparison if no day_schedules
	return availabilityWindowsOverlap(window1, window2);
}

/**
 * Check if two structured availability windows overlap
 * 
 * NEW HIERARCHICAL approach!
 * 
 * Handles 4 levels:
 * 1. month_schedules: Month → Week → Day → Time
 * 2. week_schedules: Week → Day → Time
 * 3. day_schedules: Day → Time
 * 4. time_ranges: Time only
 * 
 * Priority: Uses most specific level available in each window
 * 
 * NOTE: This version assumes same timezone. For timezone-aware matching,
 * use availabilityWindowsOverlapWithTimezone()
 */
function availabilityWindowsOverlap(window1?: AvailabilityWindow, window2?: AvailabilityWindow): boolean {
	// If either window is not specified, be optimistic
	if (!window1 || !window2) {
		return true;
	}
	
	// LEVEL 1: Both use month_schedules (most specific)
	if (window1.month_schedules && window1.month_schedules.length > 0 &&
	    window2.month_schedules && window2.month_schedules.length > 0) {
		// Find common months
		const months1 = window1.month_schedules.map(m => m.month);
		const months2 = window2.month_schedules.map(m => m.month);
		const commonMonths = months1.filter(m => months2.includes(m));
		
		if (commonMonths.length === 0) {
			return false; // No common months
		}
		
		// For each common month, check if patterns overlap
		for (const month of commonMonths) {
			const sched1 = window1.month_schedules.find(m => m.month === month)!;
			const sched2 = window2.month_schedules.find(m => m.month === month)!;
			
			// Extract day schedules from each month schedule
			const dayScheds1 = extractDaySchedulesFromMonth(sched1);
			const dayScheds2 = extractDaySchedulesFromMonth(sched2);
			
			if (dayScheds1.length > 0 && dayScheds2.length > 0) {
				if (daySchedulesOverlap(dayScheds1, dayScheds2)) {
					return true; // Found overlapping pattern in this month
				}
			}
		}
		
		return false; // No overlapping patterns in any common month
	}
	
	// LEVEL 2: At least one uses month_schedules, other uses simpler level
	if (window1.month_schedules && window1.month_schedules.length > 0) {
		// Extract patterns from window1's months and compare with window2's simpler patterns
		const allDayScheds1 = getDaySchedulesFromMonths(window1.month_schedules);
		return compareWithSimplerLevels(allDayScheds1, window2);
	}
	
	if (window2.month_schedules && window2.month_schedules.length > 0) {
		const allDayScheds2 = getDaySchedulesFromMonths(window2.month_schedules);
		return compareSimplerLevels(window1, allDayScheds2);
	}
	
	// LEVEL 3: At least one uses week_schedules
	if (window1.week_schedules && window1.week_schedules.length > 0) {
		const dayScheds1 = getDaySchedulesFromWeeks(window1.week_schedules);
		return compareSimplerLevels(window2, dayScheds1);
	}
	
	if (window2.week_schedules && window2.week_schedules.length > 0) {
		const dayScheds2 = getDaySchedulesFromWeeks(window2.week_schedules);
		return compareWithSimplerLevels(dayScheds2, window1);
	}
	
	// LEVEL 4: Both use day_schedules or time_ranges (original logic)
	if (window1.day_schedules && window1.day_schedules.length > 0 &&
	    window2.day_schedules && window2.day_schedules.length > 0) {
		return daySchedulesOverlap(window1.day_schedules, window2.day_schedules);
	}
	
	if (window1.day_schedules && window1.day_schedules.length > 0 && window2.time_ranges) {
		const allRanges1: TimeRange[] = [];
		for (const schedule of window1.day_schedules) {
			allRanges1.push(...schedule.time_ranges);
		}
		return anyTimeRangesOverlap(allRanges1, window2.time_ranges);
	}
	
	if (window2.day_schedules && window2.day_schedules.length > 0 && window1.time_ranges) {
		const allRanges2: TimeRange[] = [];
		for (const schedule of window2.day_schedules) {
			allRanges2.push(...schedule.time_ranges);
		}
		return anyTimeRangesOverlap(window1.time_ranges, allRanges2);
	}
	
	if (window1.time_ranges && window2.time_ranges) {
		return anyTimeRangesOverlap(window1.time_ranges, window2.time_ranges);
	}
	
	return true;
}

/**
 * Extract day schedules from a month schedule
 */
function extractDaySchedulesFromMonth(monthSched: MonthSchedule): DaySchedule[] {
	if (monthSched.week_schedules && monthSched.week_schedules.length > 0) {
		return getDaySchedulesFromWeeks(monthSched.week_schedules);
	}
	if (monthSched.day_schedules && monthSched.day_schedules.length > 0) {
		return monthSched.day_schedules;
	}
	// If only time_ranges, return empty (handled separately)
	return [];
}

/**
 * Compare day schedules with simpler window levels
 */
function compareSimplerLevels(window: AvailabilityWindow, dayScheds: DaySchedule[]): boolean {
	if (window.week_schedules && window.week_schedules.length > 0) {
		const windowDayScheds = getDaySchedulesFromWeeks(window.week_schedules);
		return daySchedulesOverlap(dayScheds, windowDayScheds);
	}
	if (window.day_schedules && window.day_schedules.length > 0) {
		return daySchedulesOverlap(dayScheds, window.day_schedules);
	}
	if (window.time_ranges && window.time_ranges.length > 0) {
		const allRanges: TimeRange[] = [];
		for (const sched of dayScheds) {
			allRanges.push(...sched.time_ranges);
		}
		return anyTimeRangesOverlap(allRanges, window.time_ranges);
	}
	return true;
}

/**
 * Compare window with simpler levels to day schedules (reverse order)
 */
function compareWithSimplerLevels(dayScheds: DaySchedule[], window: AvailabilityWindow): boolean {
	return compareSimplerLevels(window, dayScheds);
}

/**
 * Get day of week from a date string
 * Returns lowercase day name (e.g., 'monday', 'tuesday')
 * 
 * IMPORTANT: Parses date as LOCAL date, not UTC, to avoid timezone issues.
 * "2024-03-04" should be Monday regardless of timezone.
 */
function getDayOfWeekFromDate(dateString: string): DayOfWeek {
	// Parse as local date to avoid UTC timezone shift issues
	const parts = dateString.split('-');
	const year = parseInt(parts[0], 10);
	const month = parseInt(parts[1], 10) - 1; // Month is 0-indexed
	const day = parseInt(parts[2], 10);
	const date = new Date(year, month, day);
	
	const dayNames: DayOfWeek[] = ['sunday', 'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday'];
	return dayNames[date.getDay()];
}

/**
 * Get week of month from a date string
 * Returns 1-5 (first week, second week, etc.)
 */
function getWeekOfMonth(dateString: string): number {
	// Parse as local date to avoid UTC timezone shift issues
	const parts = dateString.split('-');
	const year = parseInt(parts[0], 10);
	const month = parseInt(parts[1], 10) - 1;
	const day = parseInt(parts[2], 10);
	const date = new Date(year, month, day);
	
	const dayOfMonth = date.getDate();
	return Math.ceil(dayOfMonth / 7);
}

/**
 * Get month from a date string
 * Returns 1-12 (January = 1, December = 12)
 */
function getMonthFromDate(dateString: string): number {
	// Parse as local date to avoid UTC timezone shift issues
	const parts = dateString.split('-');
	const month = parseInt(parts[1], 10);
	return month; // Already 1-12 from the string
}

/**
 * Check if a one-time slot (with a specific date) matches a recurring availability window
 * 
 * This checks if the one-time slot's date/time falls within the recurring pattern.
 * Now handles hierarchical structure: month_schedules → week_schedules → day_schedules → time_ranges
 * 
 * TIMEZONE AWARE: Converts both slot times to UTC before comparing.
 */
function onetimeSlotMatchesRecurringWindow(
	onetimeSlot: { 
		start_date?: string | null; 
		start_time?: string | null; 
		end_time?: string | null;
		availability_window?: AvailabilityWindow;
		time_zone?: string;
	},
	recurringWindow: AvailabilityWindow,
	recurringTimezone?: string
): boolean {
	if (!onetimeSlot.start_date) {
		return true; // No date specified, be optimistic
	}
	
	// Extract time info - prioritize availability_window if present
	let startTime = onetimeSlot.start_time;
	let endTime = onetimeSlot.end_time;
	
	// If availability_window.time_ranges exists, use those times (they're more specific)
	if (onetimeSlot.availability_window?.time_ranges && onetimeSlot.availability_window.time_ranges.length > 0) {
		startTime = onetimeSlot.availability_window.time_ranges[0].start_time;
		endTime = onetimeSlot.availability_window.time_ranges[0].end_time;
	}
	
	// Extract date components from one-time slot
	const dayOfWeek = getDayOfWeekFromDate(onetimeSlot.start_date);
	const weekOfMonth = getWeekOfMonth(onetimeSlot.start_date);
	const month = getMonthFromDate(onetimeSlot.start_date);
	
	// LEVEL 1: Check month_schedules (most specific)
	if (recurringWindow.month_schedules && recurringWindow.month_schedules.length > 0) {
		// Find the matching month schedule
		const monthSchedule = recurringWindow.month_schedules.find(m => m.month === month);
		
		if (!monthSchedule) {
			return false; // This month is not available
		}
		
		// Check if the date matches within this month's patterns
		const daySchedules = getMonthScheduleForDate(monthSchedule, onetimeSlot.start_date);
		
		if (daySchedules === null) {
			return false; // Week doesn't match
		}
		
		if (daySchedules.length === 0) {
			// Month schedule only has time_ranges (no specific days/weeks)
			if (monthSchedule.time_ranges && monthSchedule.time_ranges.length > 0) {
				if (startTime && endTime) {
					const onetimeRange: TimeRange = {
						start_time: startTime,
						end_time: endTime
					};
					return monthSchedule.time_ranges.some(r => timeRangesOverlapSimple(r, onetimeRange));
				}
				return true;
			}
			return true; // No constraints
		}
		
		// Check if day matches and time overlaps
		return checkDayAndTimeMatch(
			dayOfWeek, 
			startTime, 
			endTime, 
			daySchedules,
			onetimeSlot.start_date,
			onetimeSlot.time_zone,
			recurringTimezone
		);
	}
	
	// LEVEL 2: Check week_schedules
	if (recurringWindow.week_schedules && recurringWindow.week_schedules.length > 0) {
		// Find week schedules that include this week of month
		const relevantWeekSchedules = recurringWindow.week_schedules.filter(ws => 
			ws.weeks.includes(weekOfMonth)
		);
		
		if (relevantWeekSchedules.length === 0) {
			return false; // This week is not available
		}
		
		// Collect day schedules from all matching weeks
		const daySchedules: DaySchedule[] = [];
		for (const ws of relevantWeekSchedules) {
			daySchedules.push(...ws.day_schedules);
		}
		
		return checkDayAndTimeMatch(
			dayOfWeek,
			startTime,
			endTime,
			daySchedules,
			onetimeSlot.start_date,
			onetimeSlot.time_zone,
			recurringTimezone
		);
	}
	
	// LEVEL 3: Check day_schedules
	if (recurringWindow.day_schedules && recurringWindow.day_schedules.length > 0) {
		// If either slot has a timezone, we need to convert to UTC for proper comparison
		if (onetimeSlot.time_zone || recurringTimezone) {
			// Convert one-time slot to UTC
			const onetimeUTCDate = onetimeSlot.start_date || '2024-01-01';
			const onetimeRangesUTC = startTime && endTime ? convertTimeRangesToUTC(
				[{ start_time: startTime, end_time: endTime }],
				onetimeUTCDate,
				onetimeSlot.time_zone
			) : [];
			
			// Get UTC day of week for the one-time slot
			// We need to check if the time conversion causes a day shift
			const [year, month, day] = onetimeUTCDate.split('-').map(Number);
			const [hours, minutes] = (startTime || '12:00').split(':').map(Number);
			const utcTimeStr = onetimeRangesUTC.length > 0 ? onetimeRangesUTC[0].start_time : startTime || '12:00';
			const [utcHours] = utcTimeStr.split(':').map(Number);
			
			// Calculate if there's a day shift
			const hourDiff = utcHours - hours;
			let dayShift = 0;
			if (hourDiff > 12) dayShift = -1; // Went back a day
			else if (hourDiff < -12) dayShift = 1; // Went forward a day
			
			// Get the UTC day
			const localDate = new Date(year, month - 1, day);
			const utcDate = new Date(localDate.getTime() + (dayShift * 24 * 60 * 60 * 1000));
			const utcDayIndex = utcDate.getDay();
			const dayNames: DayOfWeek[] = ['sunday', 'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday'];
			const utcDayOfWeek = dayNames[utcDayIndex];
			
			// Convert recurring window's day schedules to UTC
			const recurringDaySchedulesUTC: Array<{ day: DayOfWeek; timeRanges: TimeRange[] }> = [];
			for (const sched of recurringWindow.day_schedules) {
				const converted = convertDayScheduleToUTC(sched, onetimeUTCDate, recurringTimezone);
				recurringDaySchedulesUTC.push(...converted);
			}
			
			// Now check if UTC days match
			const matchingSchedules = recurringDaySchedulesUTC.filter(s => s.day === utcDayOfWeek);
			if (matchingSchedules.length === 0) {
				return false;
			}
			
			// Check if times overlap in UTC
			if (onetimeRangesUTC.length > 0) {
				const onetimeRange = onetimeRangesUTC[0];
				for (const schedule of matchingSchedules) {
					if (schedule.timeRanges.some(r => timeRangesOverlapSimple(r, onetimeRange))) {
						return true;
					}
				}
				return false;
			}
			
			return true; // Days match, no specific time to check
		}
		
		// No timezone conversion needed
		return checkDayAndTimeMatch(
			dayOfWeek,
			startTime,
			endTime,
			recurringWindow.day_schedules,
			onetimeSlot.start_date,
			onetimeSlot.time_zone,
			recurringTimezone
		);
	}
	
	// LEVEL 4: Check time_ranges (all days available)
	if (recurringWindow.time_ranges && recurringWindow.time_ranges.length > 0) {
		if (startTime && endTime && onetimeSlot.start_date) {
			// Convert both to UTC before comparing
			const onetimeRangesUTC = convertTimeRangesToUTC(
				[{ start_time: startTime, end_time: endTime }],
				onetimeSlot.start_date,
				onetimeSlot.time_zone
			);
			
			const recurringRangesUTC = convertTimeRangesToUTC(
				recurringWindow.time_ranges,
				onetimeSlot.start_date,
				recurringTimezone
			);
			
			return recurringRangesUTC.some(r => timeRangesOverlapSimple(r, onetimeRangesUTC[0]));
		}
		return true;
	}
	
	// No constraints specified - be optimistic
	return true;
}

/**
 * Check if a specific day and time matches day schedules
 * 
 * @param dayOfWeek - Day of the week for the one-time slot
 * @param startTime - Start time in HH:MM format (in slot's timezone)
 * @param endTime - End time in HH:MM format (in slot's timezone)
 * @param daySchedules - Available day schedules to match against
 * @param onetimeDateStr - Date string for timezone conversion (YYYY-MM-DD)
 * @param onetimeTimezone - Timezone of the one-time slot
 * @param recurringTimezone - Timezone of the recurring window
 */
function checkDayAndTimeMatch(
	dayOfWeek: DayOfWeek,
	startTime: string | null | undefined,
	endTime: string | null | undefined,
	daySchedules: DaySchedule[],
	onetimeDateStr?: string,
	onetimeTimezone?: string,
	recurringTimezone?: string
): boolean {
	// Check if this day is in any schedule
	const relevantSchedules = daySchedules.filter(s => s.days.includes(dayOfWeek));
	
	if (relevantSchedules.length === 0) {
		return false; // This day of week is not available
	}
	
	// Check if one-time slot's time overlaps with any of the relevant schedules' time ranges
	if (startTime && endTime && onetimeDateStr) {
		// Convert one-time slot times to UTC
		const onetimeRangesUTC = convertTimeRangesToUTC(
			[{ start_time: startTime, end_time: endTime }],
			onetimeDateStr,
			onetimeTimezone
		);
		
		const onetimeRangeUTC = onetimeRangesUTC[0];
		
		for (const schedule of relevantSchedules) {
			// Convert recurring window times to UTC (use the same date for consistency)
			const recurringRangesUTC = convertTimeRangesToUTC(
				schedule.time_ranges,
				onetimeDateStr,
				recurringTimezone
			);
			
			if (recurringRangesUTC.some(r => timeRangesOverlapSimple(r, onetimeRangeUTC))) {
				return true; // Found matching time on this day (in UTC)
			}
		}
		
		return false; // No matching time on this day
	}
	
	return true; // Day matches and no specific time to check
}

// ═══════════════════════════════════════════════════════════════════
// SLOT COMPATIBILITY CHECKING
// ═══════════════════════════════════════════════════════════════════

/**
 * Calculate distance between two points using Haversine formula (in km)
 */
export function haversineDistance(lat1: number, lon1: number, lat2: number, lon2: number): number {
	const R = 6371; // Earth's radius in km
	const dLat = ((lat2 - lat1) * Math.PI) / 180;
	const dLon = ((lon2 - lon1) * Math.PI) / 180;
	const a =
		Math.sin(dLat / 2) * Math.sin(dLat / 2) +
		Math.cos((lat1 * Math.PI) / 180) *
			Math.cos((lat2 * Math.PI) / 180) *
			Math.sin(dLon / 2) *
			Math.sin(dLon / 2);
	const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
	return R * c;
}

/**
 * Check if two time ranges overlap
 * 
 * NEW: Uses structured availability_window if present (precise and simple!)
 * LEGACY: Falls back to date range comparison if no availability_window
 * 
 * Returns true if there's any overlap, or if time info is missing (optimistic)
 */
export function timeRangesOverlap(
	slot1: { 
		start_date?: string | null; 
		end_date?: string | null; 
		start_time?: string | null; 
		end_time?: string | null;
		recurrence?: string | null;
		availability_window?: AvailabilityWindow;
		time_zone?: string;
	},
	slot2: { 
		start_date?: string | null; 
		end_date?: string | null; 
		start_time?: string | null; 
		end_time?: string | null;
		recurrence?: string | null;
		availability_window?: AvailabilityWindow;
		time_zone?: string;
	}
): boolean {
	// **NEW APPROACH: Use structured availability windows if both slots have them**
	if (slot1.availability_window && slot2.availability_window) {
		const track1 = getRecurrenceTrack(slot1);
		const track2 = getRecurrenceTrack(slot2);
		
		// CASE 1: Both recurring - check if windows overlap
		if (track1 === 'recurring' && track2 === 'recurring') {
			// Use timezone-aware matching if either slot has a timezone specified
			if (slot1.time_zone || slot2.time_zone) {
				return availabilityWindowsOverlapWithTimezone(
					slot1.availability_window,
					slot2.availability_window,
					slot1.time_zone,
					slot2.time_zone
				);
			}
			// Otherwise use standard comparison (assumes same timezone)
			return availabilityWindowsOverlap(slot1.availability_window, slot2.availability_window);
		}
		
		// CASE 2: One recurring, one one-time - check if one-time falls within recurring window
		if (track1 === 'recurring' && track2 === 'onetime') {
			return onetimeSlotMatchesRecurringWindow(
				{ ...slot2, time_zone: slot2.time_zone },
				slot1.availability_window,
				slot1.time_zone
			);
		}
		
		if (track1 === 'onetime' && track2 === 'recurring') {
			return onetimeSlotMatchesRecurringWindow(
				{ ...slot1, time_zone: slot1.time_zone },
				slot2.availability_window,
				slot2.time_zone
			);
		}
		
		// CASE 3: Both one-time - fall through to legacy logic below
	}
	
	// **LEGACY APPROACH: Use start_date/end_date if no availability_window**
	// If either slot has no time info, be optimistic - assume they match
	if (!slot1.start_date && !slot1.end_date && !slot2.start_date && !slot2.end_date) {
		return true;
	}

	// If only one has time info, be optimistic
	if ((!slot1.start_date && !slot1.end_date) || (!slot2.start_date && !slot2.end_date)) {
		return true;
	}

	try {
		// Parse dates (handle both date-only and datetime strings)
		const start1 = slot1.start_date ? new Date(slot1.start_date) : new Date('1900-01-01');
		const end1 = slot1.end_date ? new Date(slot1.end_date) : new Date('2100-12-31');
		const start2 = slot2.start_date ? new Date(slot2.start_date) : new Date('1900-01-01');
		const end2 = slot2.end_date ? new Date(slot2.end_date) : new Date('2100-12-31');

		// Check if date ranges overlap
		// Range1: [start1, end1], Range2: [start2, end2]
		// Overlap if: start1 <= end2 AND start2 <= end1
		return start1 <= end2 && start2 <= end1;
	} catch (e) {
		// If date parsing fails, be optimistic
		return true;
	}
}

/**
 * Check if two locations are compatible
 * Returns true if locations match or are compatible, or if location info is missing (optimistic)
 */
export function locationsCompatible(
	slot1: {
		location_type?: string;
		city?: string;
		country?: string;
		online_link?: string;
		latitude?: number;
		longitude?: number;
	},
	slot2: {
		location_type?: string;
		city?: string;
		country?: string;
		online_link?: string;
		latitude?: number;
		longitude?: number;
	}
): boolean {
	// If neither has location info, be optimistic - assume they match
	const slot1HasLocation = slot1.city || slot1.country || slot1.latitude !== undefined;
	const slot2HasLocation = slot2.city || slot2.country || slot2.latitude !== undefined;

	if (!slot1HasLocation && !slot2HasLocation) {
		return true;
	}

	// If only one has location info, be optimistic
	if (!slot1HasLocation || !slot2HasLocation) {
		return true;
	}

	// If either is online/remote, consider compatible
	if (
		slot1.location_type?.toLowerCase().includes('online') ||
		slot1.location_type?.toLowerCase().includes('remote') ||
		slot2.location_type?.toLowerCase().includes('online') ||
		slot2.location_type?.toLowerCase().includes('remote') ||
		slot1.online_link ||
		slot2.online_link
	) {
		return true;
	}

	// Check country match (case insensitive)
	if (slot1.country && slot2.country) {
		if (slot1.country.toLowerCase() === slot2.country.toLowerCase()) {
			return true;
		}
	}

	// Check city match (case insensitive)
	if (slot1.city && slot2.city) {
		if (slot1.city.toLowerCase() === slot2.city.toLowerCase()) {
			return true;
		}
	}

	// If we have precise coordinates for both, check proximity (within ~50km)
	if (
		slot1.latitude !== undefined &&
		slot1.longitude !== undefined &&
		slot2.latitude !== undefined &&
		slot2.longitude !== undefined
	) {
		const distance = haversineDistance(
			slot1.latitude,
			slot1.longitude,
			slot2.latitude,
			slot2.longitude
		);
		return distance <= 50; // Within 50km
	}

	// If neither country nor city matched, be pessimistic
	// (we have location info but they don't match)
	if ((slot1.country || slot1.city) && (slot2.country || slot2.city)) {
		return false;
	}

	// Default: be optimistic if unclear
	return true;
}

/**
 * Check if a need slot can be fulfilled by an availability slot (v5 - Multi-Dimensional)
 * 
 * COMPATIBILITY REQUIREMENTS:
 * - **Type match: need_type_id must be identical (E28' - CRITICAL for multi-dimensional)**
 * - **Time compatibility: date/time ranges must overlap**
 * - **Location compatibility: city/country/coordinates must match**
 * - **Recurrence: NO FILTERING** - capacity can serve any compatible need regardless of recurrence pattern
 * 
 * ASYMMETRIC RECURRENCE MODEL:
 * - Capacity slots (recurring or one-time) can match ANY compatible need slot
 * - Need slots are implicitly separated by recurrence track (getRecurrenceTrack)
 * - A recurring capacity (e.g., "tutoring every Monday") can simultaneously serve:
 *   - Recurring needs (e.g., "weekly tutoring")
 *   - One-time needs (e.g., "help this Monday only")
 * - This provides maximum flexibility for providers while maintaining clarity for recipients
 * 
 * FILTER LOGIC:
 * - Availability slot filter: Who can RECEIVE from this slot
 * - Need slot filter: Who can PROVIDE to fulfill this need
 * - Both filters must pass for compatibility
 * 
 * Note: Filter checking requires provider/recipient context and should be done
 * in the allocation algorithm (see algorithm.svelte.ts via passesSlotFilters)
 * 
 * @param needSlot - The need slot to check
 * @param availabilitySlot - The availability slot to check
 * @returns true if slots are compatible (type AND time AND location match, recurrence ignored)
 */
export function slotsCompatible(needSlot: NeedSlot, availabilitySlot: AvailabilitySlot): boolean {
	// **V5 CRITICAL: Type matching (E28')**
	// Different need types CANNOT be matched (no cross-type allocation)
	if (needSlot.need_type_id !== availabilitySlot.need_type_id) {
		return false;
	}

	// **NO RECURRENCE FILTERING**
	// Capacity (recurring or one-time) can match any compatible need
	// This implements the asymmetric track model:
	//   - Need slots are separated into recurring/onetime tracks (via slot metadata)
	//   - Capacity slots serve both tracks flexibly
	// The allocation algorithm processes each slot independently, so this happens naturally

	// Check time compatibility
	if (!timeRangesOverlap(needSlot, availabilitySlot)) {
		return false;
	}

	// Check location compatibility
	if (!locationsCompatible(needSlot, availabilitySlot)) {
		return false;
	}

	// All checks passed: type + time + location compatible!
	return true;
}

// ═══════════════════════════════════════════════════════════════════
// FILTER TYPES AND EVALUATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Filter context for evaluation
 */
export interface FilterContext {
	pubKey: string;
	commitment?: any;
	mutualRecognition?: number;
	attributes?: Record<string, any>;
}

/**
 * Standard filter rule types
 */
export type FilterRule =
	| { type: 'trust'; min_mutual_recognition?: number; only_mutual?: boolean }
	| { type: 'location'; allowed_cities?: string[]; allowed_countries?: string[]; max_distance_km?: number }
	| { type: 'attribute'; required?: string[]; forbidden?: string[] }
	| { type: 'certification'; required?: string[]; min_level?: number }
	| { type: 'resource_type'; allowed_types?: string[]; forbidden_types?: string[] }
	| { type: 'custom'; fn: string }
	| { type: 'allow_all' }
	| { type: 'deny_all' }
	| any; // Legacy/unknown filters

/**
 * Evaluate a single filter rule against a context
 * 
 * @param filter - The filter rule to evaluate
 * @param context - The context (person/entity) being evaluated
 * @returns true if filter passes, false if rejected
 */
export function evaluateFilter(filter: FilterRule | null | undefined, context: FilterContext): boolean {
	// No filter = pass by default (optimistic)
	if (!filter) return true;
	
	try {
		switch (filter.type) {
			case 'allow_all':
				return true;
			
			case 'deny_all':
				return false;
			
			case 'trust': {
				// Trust-based filter: require minimum mutual recognition
				if (filter.only_mutual) {
					// Require mutual recognition > 0
					const mr = context.mutualRecognition || 0;
					if (mr <= 0) {
						console.log(`[FILTER-REJECT:TRUST] ${context.pubKey.slice(0,8)} - only_mutual required, MR=${mr}`);
						return false;
					}
				}
				
				if (filter.min_mutual_recognition !== undefined) {
					const mr = context.mutualRecognition || 0;
					if (mr < filter.min_mutual_recognition) {
						console.log(`[FILTER-REJECT:TRUST] ${context.pubKey.slice(0,8)} - requires MR>=${filter.min_mutual_recognition}, has ${mr}`);
						return false;
					}
				}
				
				return true;
			}
			
			case 'location': {
				// Location-based filter
				if (!context.commitment) return true; // Optimistic if no commitment data
				
				const commitment = context.commitment;
				
				// Check allowed cities
				if (filter.allowed_cities && filter.allowed_cities.length > 0) {
					const city = commitment.city?.toLowerCase();
					if (!city || !filter.allowed_cities.some((c: string) => c.toLowerCase() === city)) {
						console.log(`[FILTER-REJECT:LOCATION] ${context.pubKey.slice(0,8)} - city not in allowed list`);
						return false;
					}
				}
				
				// Check allowed countries
				if (filter.allowed_countries && filter.allowed_countries.length > 0) {
					const country = commitment.country?.toLowerCase();
					if (!country || !filter.allowed_countries.some((c: string) => c.toLowerCase() === country)) {
						console.log(`[FILTER-REJECT:LOCATION] ${context.pubKey.slice(0,8)} - country not in allowed list`);
						return false;
					}
				}
				
				// Check distance (if coordinates available)
				if (filter.max_distance_km && context.commitment.latitude && context.commitment.longitude) {
					// Distance checking would need reference coordinates from the other party
					// For now, we'll pass this check
					console.log(`[FILTER-INFO:LOCATION] Distance check not yet implemented`);
				}
				
				return true;
			}
			
			case 'attribute': {
				// Attribute-based filter: require/forbid specific attributes
				const attrs = context.attributes || {};
				
				// Check required attributes
				if (filter.required && filter.required.length > 0) {
					for (const requiredAttr of filter.required) {
						if (!attrs[requiredAttr]) {
							console.log(`[FILTER-REJECT:ATTRIBUTE] ${context.pubKey.slice(0,8)} - missing required attribute: ${requiredAttr}`);
							return false;
						}
					}
				}
				
				// Check forbidden attributes
				if (filter.forbidden && filter.forbidden.length > 0) {
					for (const forbiddenAttr of filter.forbidden) {
						if (attrs[forbiddenAttr]) {
							console.log(`[FILTER-REJECT:ATTRIBUTE] ${context.pubKey.slice(0,8)} - has forbidden attribute: ${forbiddenAttr}`);
							return false;
						}
					}
				}
				
				return true;
			}
			
			case 'certification': {
				// Certification-based filter
				const attrs = context.attributes || {};
				const certifications = attrs.certifications || [];
				
				if (filter.required && filter.required.length > 0) {
					for (const requiredCert of filter.required) {
						if (!certifications.includes(requiredCert)) {
							console.log(`[FILTER-REJECT:CERTIFICATION] ${context.pubKey.slice(0,8)} - missing certification: ${requiredCert}`);
							return false;
						}
					}
				}
				
				if (filter.min_level !== undefined) {
					const level = attrs.certification_level || 0;
					if (level < filter.min_level) {
						console.log(`[FILTER-REJECT:CERTIFICATION] ${context.pubKey.slice(0,8)} - level ${level} < ${filter.min_level}`);
						return false;
					}
				}
				
				return true;
			}
			
			case 'resource_type': {
				// Resource type filter (for multi-type resources)
				const resourceType = context.commitment?.resource_type;
				
				if (filter.allowed_types && filter.allowed_types.length > 0) {
					if (!resourceType || !filter.allowed_types.includes(resourceType)) {
						console.log(`[FILTER-REJECT:RESOURCE_TYPE] ${context.pubKey.slice(0,8)} - type "${resourceType}" not in allowed list`);
						return false;
					}
				}
				
				if (filter.forbidden_types && filter.forbidden_types.length > 0) {
					if (resourceType && filter.forbidden_types.includes(resourceType)) {
						console.log(`[FILTER-REJECT:RESOURCE_TYPE] ${context.pubKey.slice(0,8)} - type "${resourceType}" is forbidden`);
						return false;
					}
				}
				
				return true;
			}
			
			case 'custom': {
				// Custom filter function (serialized)
				console.log(`[FILTER-WARN] Custom filter functions not yet implemented for security`);
				return true; // Be optimistic until implemented
			}
			
			default: {
				// Unknown filter type - log and be optimistic
				console.log(`[FILTER-WARN] Unknown filter type:`, filter);
				return true;
			}
		}
	} catch (error) {
		console.error(`[FILTER-ERROR] Error evaluating filter:`, error, filter);
		return false; // Be pessimistic on errors for safety
	}
}

/**
 * Check if a provider-recipient pair passes bilateral filters
 * 
 * BILATERAL FILTER CHECKING:
 * - Capacity filter (availSlot.filter_rule): Does recipient pass provider's filter?
 * - Need filter (needSlot.filter_rule): Does provider pass recipient's filter?
 * - Both must pass for allocation to occur
 * 
 * @param needSlot - Recipient's need slot (with optional filter on providers)
 * @param availabilitySlot - Provider's availability slot (with optional filter on recipients)
 * @param providerContext - Provider's context for evaluation
 * @param recipientContext - Recipient's context for evaluation
 * @returns true if both filters pass (or no filters present)
 */
export function passesSlotFilters(
	needSlot: NeedSlot,
	availabilitySlot: AvailabilitySlot,
	providerContext: FilterContext,
	recipientContext: FilterContext
): boolean {
	// Check availability slot filter (who can receive from provider)
	// Provider is checking if recipient passes their filter
	if (availabilitySlot.filter_rule) {
		if (!evaluateFilter(availabilitySlot.filter_rule, recipientContext)) {
			console.log(`[FILTER-BILATERAL-REJECT] Recipient ${recipientContext.pubKey.slice(0,8)} failed provider's capacity filter`);
			return false;
		}
	}
	
	// Check need slot filter (who can provide to recipient)
	// Recipient is checking if provider passes their filter
	if (needSlot.filter_rule) {
		if (!evaluateFilter(needSlot.filter_rule, providerContext)) {
			console.log(`[FILTER-BILATERAL-REJECT] Provider ${providerContext.pubKey.slice(0,8)} failed recipient's need filter`);
			return false;
		}
	}
	
	// Both filters passed (or no filters present)
	return true;
}

// ═══════════════════════════════════════════════════════════════════
// SLOT MATCHING AND QUANTITY CALCULATION (v5: Simplified)
// ═══════════════════════════════════════════════════════════════════

/**
 * NOTE: v5 uses slot-native allocation directly in algorithm.svelte.ts
 * 
 * The core matching logic is in slotsCompatible() above, which checks:
 * - Type compatibility (need_type_id match)
 * - Time compatibility (date/time overlap)
 * - Location compatibility (city/country/coordinates)
 * 
 * The allocation algorithm processes each availability slot independently
 * and finds compatible need slots using slotsCompatible().
 * 
 * Legacy v2 functions (matchNeedToCapacitySlots, etc.) are not needed in v5
 * since the multi-dimensional framework works directly with slot arrays.
 */

// ═══════════════════════════════════════════════════════════════════
// SPACE-TIME GROUPING & BUCKETING
// ═══════════════════════════════════════════════════════════════════

/**
 * Get time bucket key for coarse-grained filtering
 * Uses month-level granularity for fast bucketing
 * 
 * @param slot - Availability or need slot
 * @returns Time bucket key (e.g., "2024-06" or "any-time")
 */
export function getTimeBucketKey(slot: AvailabilitySlot | NeedSlot): string {
	if (slot.start_date) {
		// Month-level bucketing: "YYYY-MM"
		return slot.start_date.substring(0, 7);
	}
	return 'any-time';
}

/**
 * Get location bucket key for coarse-grained filtering
 * Buckets by: remote > city > country > unknown
 * 
 * @param slot - Availability or need slot
 * @returns Location bucket key (e.g., "remote", "san-francisco", "usa", "unknown")
 */
export function getLocationBucketKey(slot: AvailabilitySlot | NeedSlot): string {
	// Remote slots are universally compatible
	if (slot.location_type?.toLowerCase().includes('remote') || 
	    slot.location_type?.toLowerCase().includes('online') ||
	    slot.online_link) {
		return 'remote';
	}
	
	// Use city if available (most specific)
	if (slot.city) {
		return slot.city.toLowerCase();
	}
	
	// Fall back to country
	if (slot.country) {
		return slot.country.toLowerCase();
	}
	
	// No location specified
	return 'unknown';
}

/**
 * Generate a space-time signature for a slot
 * Slots with identical signatures can be aggregated
 * 
 * This is more precise than bucketing and used for exact grouping.
 * 
 * @param slot - Availability or need slot
 * @returns Space-time signature string
 */
export function getSpaceTimeSignature(
	slot: AvailabilitySlot | NeedSlot
): string {
	// Time component
	let timeKey: string;
	
	if (slot.availability_window) {
		// Build signature from hierarchical availability_window
		const window = slot.availability_window;
		
		// Extract months from month_schedules
		let monthsKey = 'all-months';
		if (window.month_schedules && window.month_schedules.length > 0) {
			const months = window.month_schedules.map(m => m.month).sort();
			monthsKey = months.join(',');
		}
		
		// Extract weeks from week_schedules or month_schedules
		let weeksKey = 'all-weeks';
		if (window.week_schedules && window.week_schedules.length > 0) {
			const allWeeks = new Set<number>();
			for (const ws of window.week_schedules) {
				ws.weeks.forEach(w => allWeeks.add(w));
			}
			weeksKey = Array.from(allWeeks).sort().join(',');
		} else if (window.month_schedules && window.month_schedules.length > 0) {
			const allWeeks = new Set<number>();
			for (const ms of window.month_schedules) {
				if (ms.week_schedules && ms.week_schedules.length > 0) {
					for (const ws of ms.week_schedules) {
						ws.weeks.forEach(w => allWeeks.add(w));
					}
				}
			}
			if (allWeeks.size > 0) {
				weeksKey = Array.from(allWeeks).sort().join(',');
			}
		}
		
		// Extract days from day_schedules (at any level)
		let daysKey = 'all-days';
		const allDayScheds: DaySchedule[] = [];
		
		if (window.month_schedules && window.month_schedules.length > 0) {
			allDayScheds.push(...getDaySchedulesFromMonths(window.month_schedules));
		} else if (window.week_schedules && window.week_schedules.length > 0) {
			allDayScheds.push(...getDaySchedulesFromWeeks(window.week_schedules));
		} else if (window.day_schedules && window.day_schedules.length > 0) {
			allDayScheds.push(...window.day_schedules);
		}
		
		if (allDayScheds.length > 0) {
			const allDays = getDaysFromSchedules(allDayScheds);
			daysKey = allDays.sort().join(',');
		}
		
		// Recurrence
		const recKey = slot.recurrence || 'onetime';
		
		timeKey = `${recKey}|${monthsKey}|${weeksKey}|${daysKey}`;
	} else {
		// Fallback: use basic date/recurrence info
		timeKey = [
		slot.start_date || 'any-date',
		slot.end_date || 'any-date',
			slot.recurrence || 'onetime'
	].join('|');
	}
	
	// Location component (precise)
	const locKey = slot.location_type?.toLowerCase().includes('remote') || slot.online_link
		? 'remote'
		: [
			slot.city?.toLowerCase() || 'any-city',
			slot.country?.toLowerCase() || 'any-country',
			slot.latitude?.toFixed(2) || 'any-lat',
			slot.longitude?.toFixed(2) || 'any-lon'
		].join('|');
	
	return `${timeKey}::${locKey}`;
}

/**
 * Group slots by their space-time signature and aggregate quantities
 * Slots at the same time/location are merged
 * 
 * @param slots - Array of slots to group
 * @returns Map of signature -> aggregated quantity
 */
export function groupSlotsBySpaceTime<T extends AvailabilitySlot | NeedSlot>(
	slots: T[]
): Map<string, { quantity: number; slots: T[] }> {
	const groups = new Map<string, { quantity: number; slots: T[] }>();
	
	for (const slot of slots) {
		const signature = getSpaceTimeSignature(slot);
		const existing = groups.get(signature);
		
		if (existing) {
			existing.quantity += slot.quantity;
			existing.slots.push(slot);
		} else {
			groups.set(signature, {
				quantity: slot.quantity,
				slots: [slot]
			});
		}
	}
	
	return groups;
}

// ═══════════════════════════════════════════════════════════════════
// UTILITY FUNCTIONS (v5: Simplified for slot arrays)
// ═══════════════════════════════════════════════════════════════════

/**
 * Calculate total quantity from slot array
 * 
 * NOTE: This sums ALL slots regardless of time/space compatibility.
 * For allocation purposes, the algorithm handles compatibility per-slot.
 * This is useful for reporting/display purposes only.
 */
export function calculateTotalQuantity(slots: (NeedSlot | AvailabilitySlot)[]): number {
	return slots.reduce((sum, slot) => sum + slot.quantity, 0);
}

/**
 * Calculate space-time aware profile from slots
 * Groups slots by space-time signature and returns unique space-time combinations
 * 
 * This is the "true" quantity considering that slots at the same time/location
 * should be aggregated, but slots at different times/locations are separate.
 * 
 * @param slots - Array of slots to profile
 * @returns Array of space-time combinations with aggregated quantities
 */
export function getSpaceTimeProfile<T extends NeedSlot | AvailabilitySlot>(
	slots: T[]
): Array<{
	signature: string;
	quantity: number;
	slots: T[];
}> {
	const groups = groupSlotsBySpaceTime(slots);
	return Array.from(groups.entries()).map(([signature, data]) => ({
		signature,
		quantity: data.quantity,
		slots: data.slots
	}));
}

// ═══════════════════════════════════════════════════════════════════
// SPACE-TIME MATCHING EXAMPLES
// ═══════════════════════════════════════════════════════════════════

/**
 * EXAMPLE 1: Same Space-Time (Should Aggregate)
 * 
 * Provider has:
 *   - Slot A: Monday 9-10am @ SF, quantity 5
 *   - Slot B: Monday 9-10am @ SF, quantity 3
 * 
 * These have the SAME space-time signature, so they aggregate to 8 units
 * available on Monday 9-10am in SF.
 * 
 * In allocation, both slots allocate independently (slot-native), but they're
 * providing to the same space-time, so a recipient with a need at that exact
 * time/place can receive from both.
 */

/**
 * EXAMPLE 2: Same Time, Different Space (Cannot Aggregate)
 * 
 * Provider has:
 *   - Slot A: Monday 9-10am @ SF, quantity 5
 *   - Slot B: Monday 9-10am @ NYC, quantity 3
 * 
 * These have DIFFERENT space-time signatures (different cities).
 * A recipient in SF can only receive from Slot A.
 * A recipient in NYC can only receive from Slot B.
 * Total capacity is NOT 8 - it's 5 in SF and 3 in NYC (separate pools).
 */

/**
 * EXAMPLE 3: Same Space, Different Time (Cannot Aggregate)
 * 
 * Provider has:
 *   - Slot A: Monday 9-10am @ SF, quantity 5
 *   - Slot B: Tuesday 9-10am @ SF, quantity 3
 * 
 * These have DIFFERENT space-time signatures (different days).
 * A recipient needing Monday service can only receive from Slot A (5 units).
 * A recipient needing Tuesday service can only receive from Slot B (3 units).
 * Total capacity is NOT 8 - it's 5 on Monday and 3 on Tuesday (separate pools).
 */

/**
 * EXAMPLE 4: Overlapping Time Ranges
 * 
 * Provider has:
 *   - Slot A: Monday 9-11am @ SF, quantity 5
 *   - Slot B: Monday 10am-12pm @ SF, quantity 3
 * 
 * These overlap but have DIFFERENT signatures (different time ranges).
 * 
 * Recipient with need Monday 10-11am @ SF:
 *   - Compatible with Slot A (within 9-11am range) ✓
 *   - Compatible with Slot B (within 10am-12pm range) ✓
 *   - Can receive from BOTH slots (up to 8 units total)
 * 
 * This is correct! The provider is available during both time windows,
 * and the overlap period has both capacities available.
 */

/**
 * EXAMPLE 5: Remote vs In-Person
 * 
 * Provider has:
 *   - Slot A: Monday 9-10am @ Remote, quantity 5
 *   - Slot B: Monday 9-10am @ SF, quantity 3
 * 
 * These have DIFFERENT signatures (remote vs in-person).
 * 
 * Remote slots are compatible with ANY location (optimistic matching).
 * So a recipient in SF can potentially receive from both Slot A (remote)
 * and Slot B (in-person SF), getting up to 8 units.
 * 
 * This is correct! Remote capacity can serve anyone, anywhere.
 */

/**
 * ALLOCATION ALGORITHM BEHAVIOR:
 * 
 * The slot-native algorithm processes each availability slot independently:
 * 
 * 1. For each availability slot, find all compatible recipients
 * 2. Run two-tier allocation on that slot's quantity
 * 3. Record allocations with slot-to-slot pairing
 * 
 * This means:
 * - Slots at the same space-time allocate independently
 * - A recipient can receive from multiple slots (if compatible)
 * - Space-time compatibility is checked via slotsCompatible()
 * - Filters are checked bilaterally for each pairing
 * 
 * The space-time grouping functions (getSpaceTimeSignature, etc.) are
 * useful for ANALYSIS and VISUALIZATION, but the actual allocation
 * respects slot-level compatibility automatically through the
 * slot-native design.
 */

// ═══════════════════════════════════════════════════════════════════
// RECURRENCE TRACK EXAMPLES (ASYMMETRIC MODEL)
// ═══════════════════════════════════════════════════════════════════

/**
 * EXAMPLE 1: Recurring Capacity Serving Both Tracks
 * 
 * Provider offers:
 *   - Capacity: "Monday tutoring, recurring weekly, 10 hours"
 *     recurrence: "weekly", start_date: "2024-01-01"
 * 
 * Recipients need:
 *   - Alice: "Weekly tutoring, recurring, 5 hours"
 *     recurrence: "weekly", start_date: "2024-01-01"
 *   - Bob: "One-time tutoring this Monday, 3 hours"  
 *     recurrence: null, start_date: "2024-01-15"
 *   - Carol: "Weekly tutoring, recurring, 4 hours"
 *     recurrence: "weekly", start_date: "2024-01-01"
 * 
 * Week of 2024-01-15 allocation:
 *   - Provider's capacity: 10 hours available
 *   - Alice gets 5 hours (recurring track)
 *   - Bob gets 3 hours (onetime track)
 *   - Carol gets 2 hours (recurring track, partially satisfied)
 * 
 * Week of 2024-01-22 allocation:
 *   - Provider's capacity: 10 hours available (recurring, so available again)
 *   - Bob is GONE (one-time need was satisfied, doesn't recur)
 *   - Alice gets 5 hours (recurring track, still needs it)
 *   - Carol gets 4 hours (recurring track, gets full amount this week)
 *   - 1 hour spare capacity
 * 
 * KEY INSIGHT: Recurring capacity can flexibly serve both types of needs.
 * The distinction is on the NEED side - recipients signal their commitment type.
 */

/**
 * EXAMPLE 2: One-Time Capacity Serving Both Tracks
 * 
 * Provider offers:
 *   - Capacity: "Workshop on Saturday, one-time, 20 slots"
 *     recurrence: null, start_date: "2024-01-20"
 * 
 * Recipients need:
 *   - Alice: "Monthly workshop series, recurring"
 *     recurrence: "monthly", start_date: "2024-01-20"
 *   - Bob: "This specific workshop, one-time"
 *     recurrence: null, start_date: "2024-01-20"
 * 
 * Allocation:
 *   - Both Alice and Bob can attend this Saturday's workshop
 *   - Alice: Her recurring need will seek the February workshop too
 *   - Bob: His one-time need is satisfied and done
 * 
 * KEY INSIGHT: Even one-time capacity can serve recurring needs 
 * (for that specific occurrence). The recurring need will continue seeking
 * fulfillment in future periods.
 */

/**
 * EXAMPLE 3: User Mental Model
 * 
 * FROM THE RECIPIENT'S PERSPECTIVE:
 * 
 * "I have two types of needs:
 * 
 * RECURRING (ongoing commitment to community):
 *   - Weekly groceries (I need this every week indefinitely)
 *   - Monthly healthcare checkup (ongoing health management)
 *   - Daily childcare (long-term need)
 * 
 * ONE-TIME (specific, bounded needs):
 *   - Help moving next month (discrete event)
 *   - One workshop on Python (specific learning goal)
 *   - Repair my bike (task that completes)
 * 
 * The system treats these separately in tracking my needs,
 * but providers can flexibly serve either type."
 * 
 * FROM THE PROVIDER'S PERSPECTIVE:
 * 
 * "I don't care if someone needs recurring or one-time help:
 * 
 * MY RECURRING CAPACITY:
 *   - Tutoring every Monday (available to anyone who needs it that day)
 *   - Can serve people with weekly tutoring needs
 *   - Can serve people who just need help once
 * 
 * MY ONE-TIME CAPACITY:
 *   - Hosting a workshop this Saturday
 *   - Can serve people in a workshop series (they'll come back for next one)
 *   - Can serve people who just want this specific workshop
 * 
 * My capacity is available - whoever needs it can request it!"
 */

/**
 * IMPLEMENTATION NOTES:
 * 
 * The asymmetric recurrence model is implemented through:
 * 
 * 1. **Slot metadata**: Each slot has recurrence field (or null)
 * 2. **Track identification**: getRecurrenceTrack() classifies slots
 * 3. **No matching filter**: slotsCompatible() does NOT reject based on recurrence
 * 4. **Natural separation**: Need slots naturally separate into tracks by their metadata
 * 5. **Unified capacity**: All capacity can serve any compatible need
 * 
 * Benefits:
 * - Simple implementation (no complex recurrence expansion)
 * - Clear user model (recipients declare commitment type)
 * - Maximum flexibility (providers serve any compatible need)
 * - Natural convergence (each need track converges independently)
 * - Slot-native design (everything at the slot level, no special cases)
 */

