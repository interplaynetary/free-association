/**
 * Time-Based Indexing and Filtering System
 *
 * This module provides sophisticated time-range indexing for objects with:
 * - Time slot generation with configurable intervals
 * - Recurring event support (daily, weekly, monthly, yearly, custom)
 * - Availability checking across time ranges
 * - Timezone-aware scheduling
 * - Automatic expansion of recurring patterns
 *
 * @example Time-Range Filtering
 * ```typescript
 * const timeIndexed = createTimeIndex(objects, {
 *   timeSlotMinutes: 30, // 30-minute slots
 *   dateRange: { start: '2024-01-01', end: '2024-12-31' },
 *   timezone: 'America/Los_Angeles'
 * });
 *
 * const available = findObjectsInTimeRange(timeIndexed, {
 *   start: '2024-01-15T10:00:00',
 *   end: '2024-01-15T12:00:00'
 * });
 * ```
 */

// ===== TYPES & INTERFACES =====

export interface TimeSlot {
	start: Date;
	end: Date;
	objectIds: string[];
}

export interface TimeIndexConfig {
	timeSlotMinutes?: number; // Duration of each time slot (default: 30)
	dateRange?: { start: string; end: string }; // ISO date range to index
	timezone?: string; // Timezone for calculations (default: UTC)
	includeRecurring?: boolean; // Whether to expand recurring events (default: true)
	maxRecurrenceInstances?: number; // Max instances of recurring events (default: 100)
}

export interface TimeIndex {
	slots: Record<string, TimeSlot>; // ISO datetime string -> TimeSlot
	objectSchedules: Record<string, TimeSlot[]>; // objectId -> TimeSlots
	config: TimeIndexConfig;
}

// ===== TIME UTILITIES =====

/**
 * Parse time string to minutes since midnight
 */
export const parseTimeToMinutes = (timeStr: string): number => {
	const [hours, minutes] = timeStr.split(':').map(Number);
	return hours * 60 + (minutes || 0);
};

/**
 * Parse date string to Date object
 */
export const parseDate = (dateStr: string, timezone: string = 'UTC'): Date => {
	// Handle various date formats
	if (dateStr.includes('T')) {
		return new Date(dateStr);
	}
	// Assume local date if no time specified
	const date = new Date(dateStr + 'T00:00:00');
	return date;
};

/**
 * Generate time slots for a date range
 */
export const generateTimeSlots = (config: TimeIndexConfig): string[] => {
	const slots: string[] = [];
	const startDate = parseDate(config.dateRange?.start || new Date().toISOString().split('T')[0]);
	const endDate = parseDate(
		config.dateRange?.end ||
			new Date(Date.now() + 365 * 24 * 60 * 60 * 1000).toISOString().split('T')[0]
	);
	const slotMinutes = config.timeSlotMinutes || 30;

	let current = new Date(startDate);
	while (current <= endDate) {
		slots.push(current.toISOString());
		current = new Date(current.getTime() + slotMinutes * 60 * 1000);
	}

	return slots;
};

/**
 * Check if an object is available during a time slot
 */
export const isAvailableInSlot = (obj: any, slotStart: Date, slotEnd: Date): boolean => {
	// Handle all-day events
	if (obj.all_day) {
		const objDate = parseDate(obj.start_date || obj.end_date);
		const slotDate = new Date(slotStart.getFullYear(), slotStart.getMonth(), slotStart.getDate());
		return objDate.getTime() === slotDate.getTime();
	}

	// Parse object time range
	const objStartDate = parseDate(obj.start_date);
	const objEndDate = parseDate(obj.end_date || obj.start_date);

	let objStart: Date, objEnd: Date;

	if (obj.start_time) {
		const startMinutes = parseTimeToMinutes(obj.start_time);
		objStart = new Date(objStartDate.getTime() + startMinutes * 60 * 1000);
	} else {
		objStart = objStartDate;
	}

	if (obj.end_time) {
		const endMinutes = parseTimeToMinutes(obj.end_time);
		objEnd = new Date(objEndDate.getTime() + endMinutes * 60 * 1000);
	} else {
		objEnd = new Date(objEndDate.getTime() + 24 * 60 * 60 * 1000); // End of day
	}

	// Check overlap
	return objStart < slotEnd && objEnd > slotStart;
};

/**
 * Generate recurring instances for an object
 */
export const generateRecurringInstances = (obj: any, config: TimeIndexConfig): Date[] => {
	if (!obj.recurrence && !obj.custom_recurrence_repeat_every) {
		return [parseDate(obj.start_date)];
	}

	const instances: Date[] = [];
	const startDate = parseDate(obj.start_date);
	const maxInstances = config.maxRecurrenceInstances || 100;
	const endDate = parseDate(
		config.dateRange?.end ||
			new Date(Date.now() + 365 * 24 * 60 * 60 * 1000).toISOString().split('T')[0]
	);

	// Handle standard recurrence patterns
	if (obj.recurrence) {
		let interval: number;
		switch (obj.recurrence.toLowerCase()) {
			case 'daily':
				interval = 1;
				break;
			case 'weekly':
				interval = 7;
				break;
			case 'monthly':
				interval = 30;
				break; // Approximate
			case 'yearly':
				interval = 365;
				break;
			default:
				return [startDate];
		}

		let current = new Date(startDate);
		let count = 0;
		while (current <= endDate && count < maxInstances) {
			instances.push(new Date(current));
			current = new Date(current.getTime() + interval * 24 * 60 * 60 * 1000);
			count++;
		}
	}

	// Handle custom recurrence
	if (obj.custom_recurrence_repeat_every && obj.custom_recurrence_repeat_unit) {
		const interval = obj.custom_recurrence_repeat_every;
		let multiplier: number;

		switch (obj.custom_recurrence_repeat_unit.toLowerCase()) {
			case 'days':
				multiplier = 1;
				break;
			case 'weeks':
				multiplier = 7;
				break;
			case 'months':
				multiplier = 30;
				break; // Approximate
			case 'years':
				multiplier = 365;
				break;
			default:
				return [startDate];
		}

		let current = new Date(startDate);
		let count = 0;
		while (current <= endDate && count < maxInstances) {
			instances.push(new Date(current));
			current = new Date(current.getTime() + interval * multiplier * 24 * 60 * 60 * 1000);
			count++;
		}
	}

	return instances.length > 0 ? instances : [startDate];
};

// ===== TIME INDEX FUNCTIONS =====

/**
 * Create a time-based index for objects
 */
export function createTimeIndex(
	objects: Record<string, any>,
	config: TimeIndexConfig = {}
): TimeIndex {
	const timeSlots: Record<string, TimeSlot> = {};
	const objectSchedules: Record<string, TimeSlot[]> = {};
	const slotMinutes = config.timeSlotMinutes || 30;

	// Generate all time slots
	const allSlots = generateTimeSlots(config);
	allSlots.forEach((slotISOString) => {
		const slotStart = new Date(slotISOString);
		const slotEnd = new Date(slotStart.getTime() + slotMinutes * 60 * 1000);

		timeSlots[slotISOString] = {
			start: slotStart,
			end: slotEnd,
			objectIds: []
		};
	});

	// Index each object
	Object.entries(objects).forEach(([objectId, obj]) => {
		objectSchedules[objectId] = [];

		// Generate instances (including recurring)
		const instances =
			config.includeRecurring !== false
				? generateRecurringInstances(obj, config)
				: [parseDate(obj.start_date)];

		instances.forEach((instanceDate) => {
			// Check each time slot for availability
			allSlots.forEach((slotISOString) => {
				const slot = timeSlots[slotISOString];

				// Create a temporary object with the instance date
				const instanceObj = { ...obj, start_date: instanceDate.toISOString().split('T')[0] };

				if (isAvailableInSlot(instanceObj, slot.start, slot.end)) {
					slot.objectIds.push(objectId);
					objectSchedules[objectId].push(slot);
				}
			});
		});
	});

	return {
		slots: timeSlots,
		objectSchedules,
		config
	};
}

/**
 * Find objects available in a specific time range
 */
export function findObjectsInTimeRange(
	timeIndex: TimeIndex,
	timeRange: { start: string; end: string }
): string[] {
	const startDate = new Date(timeRange.start);
	const endDate = new Date(timeRange.end);
	const availableObjects = new Set<string>();

	Object.values(timeIndex.slots).forEach((slot) => {
		if (slot.start < endDate && slot.end > startDate) {
			slot.objectIds.forEach((id) => availableObjects.add(id));
		}
	});

	return Array.from(availableObjects);
}

/**
 * Get time slots for a specific object
 */
export function getObjectTimeSlots(timeIndex: TimeIndex, objectId: string): TimeSlot[] {
	return timeIndex.objectSchedules[objectId] || [];
}
