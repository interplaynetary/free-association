
import type { AvailabilitySlot, NeedSlot } from '$lib/protocol/schemas';

// Polymorphic slot type
export type SlotType = NeedSlot | AvailabilitySlot;

/**
 * Safely extract time string "HH:MM" -> "HH:MM"
 * Handles "HH:MM:SS", Date objects, T-separated ISO strings
 */
export function safeExtractTime(timeValue: string | null | undefined | Date): string | undefined {
    if (!timeValue) return undefined;
    if (timeValue instanceof Date) {
        return timeValue.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit', hour12: false });
    }
    const strVal = String(timeValue);
    if (/^\d{2}:\d{2}$/.test(strVal)) {
        return strVal;
    }
    // Handle HH:MM:SS
    if (/^\d{2}:\d{2}:\d{2}$/.test(strVal)) {
        return strVal.substring(0, 5);
    }
    if (strVal.includes('T')) {
        try {
            const date = new Date(strVal);
            return date.toTimeString().substring(0, 5);
        } catch (e) {
            console.warn('Failed to parse time:', strVal);
            return undefined;
        }
    }
    return undefined;
}

/**
 * Parse slot date and time into Date objects
 * Handles all-day vs timed events, and v5 availability_window
 */
export function parseSlotDateTime(slot: SlotType): { slotStart: Date | null; slotEnd: Date | null } {
    const slotStart = slot.start_date ? new Date(slot.start_date) : null;
    let slotEnd = slot.end_date ? new Date(slot.end_date) : slotStart ? new Date(slotStart) : null;

    // Extract time info from availability_window if present (v5 schema)
    let startTimeStr: string | undefined = undefined;
    let endTimeStr: string | undefined = undefined;

    if (slot.availability_window?.time_ranges?.length) {
        startTimeStr = slot.availability_window.time_ranges[0].start_time;
        endTimeStr = slot.availability_window.time_ranges[0].end_time;
    }
    // Fallback to legacy fields if present (though schema deprecates them, runtime might have them)
    // We treat them as 'any' safely
    else {
        const anySlot = slot as any;
        if (anySlot.start_time) startTimeStr = anySlot.start_time;
        if (anySlot.end_time) endTimeStr = anySlot.end_time;
    }

    // Determine if all-day.
    // In v5, if recurrence is present but no time_ranges, it's effectively all-day for that pattern.
    // Or if explicit 'all_day' flag exists (legacy/runtime).
    const isAllDay = (slot as any).all_day || (slot.recurrence && !startTimeStr);

    if (isAllDay) {
        // For all-day events, set start to beginning of day and end to end of day
        if (slotStart) {
            slotStart.setHours(0, 0, 0, 0);
        }
        if (slotEnd) {
            slotEnd.setHours(23, 59, 59, 999);
        } else if (slotStart) {
            // If no end date, all-day event ends at end of start day
            slotEnd = new Date(slotStart);
            slotEnd.setHours(23, 59, 59, 999);
        }
    } else {
        // Timed events
        if (slotStart && startTimeStr) {
            const safeStartTime = safeExtractTime(startTimeStr);
            if (safeStartTime) {
                const [hours, minutes] = safeStartTime.split(':').map(Number);
                slotStart.setHours(hours, minutes, 0, 0);
            }
        }

        if (slotEnd && endTimeStr) {
            const safeEndTime = safeExtractTime(endTimeStr);
            if (safeEndTime) {
                const [hours, minutes] = safeEndTime.split(':').map(Number);
                slotEnd.setHours(hours, minutes, 59, 999);
            }
        }

        // Handle missing end times (default to +1 hour)
        if (slotStart && !slot.end_date) {
            if (!startTimeStr && !endTimeStr) {
                // Should have been caught by all-day logic, but just in case
                slotEnd = new Date(slotStart);
                slotEnd.setHours(23, 59, 59, 999);
            } else if (startTimeStr && !endTimeStr) {
                slotEnd = new Date(slotStart.getTime() + 60 * 60 * 1000);
            }
        }
    }

    return { slotStart, slotEnd };
}


/**
 * consistently formats time display for any slot type
 * prioritizes availability_window, falls back to recurrence/dates
 */
export function formatTimeDisplay(slot: SlotType): string {
    if (!slot.recurrence && !slot.start_date && !slot.availability_window) return 'Not specified';

    let parts: string[] = [];

    // 1. Recurrence
    if (slot.recurrence) {
        // Capitalize specific enum values if needed, otherwise just use string
        const rec = slot.recurrence.charAt(0).toUpperCase() + slot.recurrence.slice(1);
        parts.push(rec);
    }

    // 2. Date Range (if specific dates)
    if (slot.start_date) {
        const date = new Date(slot.start_date);
        // Simple date format: "1/12/2026"
        parts.push(date.toLocaleDateString());
    }

    // 3. Time Ranges (from availability_window)
    if (slot.availability_window?.time_ranges?.[0]) {
        const range = slot.availability_window.time_ranges[0];
        parts.push(`${range.start_time}-${range.end_time}`);
    } else if (slot.recurrence) {
        // If it recurs but has no specific time range, it implies all-day availability for that recurrence
        parts.push('All day');
    }

    return parts.join(', ');
}

/**
 * consistently formats location display for any slot type
 */
export function formatLocationDisplay(slot: SlotType): string {
    if (!slot.location_type || slot.location_type === 'Undefined') {
        return 'Not specified';
    }

    if (slot.location_type === 'Online') {
        return slot.online_link ? 'Online' : 'Online (no link)';
    }

    if (slot.location_type === 'Specific') {
        const addressParts = [];
        if (slot.street_address) addressParts.push(slot.street_address);
        if (slot.city) addressParts.push(slot.city);
        if (slot.state_province) addressParts.push(slot.state_province);
        if (slot.country) addressParts.push(slot.country);

        if (addressParts.length > 0) {
            return addressParts.join(', ');
        }

        // Fallback to coords if specific but no address text
        if (slot.latitude && slot.longitude) {
            return `${slot.latitude.toFixed(4)}, ${slot.longitude.toFixed(4)}`;
        }

        return 'Specific Location';
    }

    if (slot.location_type === 'Coordinates' && slot.latitude && slot.longitude) {
        return `${slot.latitude.toFixed(4)}, ${slot.longitude.toFixed(4)}`;
    }

    return slot.location_type;
}


/**
 * Check if slot has specific address components (street, city, etc.)
 */
export function hasAddressComponents(slot: any): boolean {
    return !!(slot.street_address || slot.city || slot.state_province || slot.postal_code || slot.country);
}

/**
 * consistently formats divisibility rules
 */
export function formatDivisibilityDisplay(slot: SlotType): string {
    const parts: string[] = [];

    if (slot.min_atomic_size) {
        parts.push(`Size >= ${slot.min_atomic_size}`);
    }

    if (slot.max_participation) {
        parts.push(`Max ${slot.max_participation} agents`);
    }

    if (slot.max_concurrency) {
        parts.push(`Max ${slot.max_concurrency} concurrent`);
    }

    return parts.length > 0 ? parts.join(', ') : 'None';
}

/**
 * Check if a slot is in the past
 * Useful for filtering or categorization
 */
export function isSlotInPast(slot: SlotType): boolean {
    if (slot.recurrence) return false;

    const now = new Date();
    const { slotStart, slotEnd } = parseSlotDateTime(slot);

    if (!slotStart) return false;

    // Use the effective end time (or start if no end)
    const effectiveEndTime = slotEnd || slotStart;
    return effectiveEndTime < now;
}
