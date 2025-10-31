/**
 * Slot Utilities
 * 
 * Pure utility functions for working with availability and need slots.
 * Formatting, display, and calculation helpers separated from tree logic.
 */

import type { AvailabilitySlot, NeedSlot, ResourceMetadata } from '../schemas';
import { timeRangesOverlap, locationsCompatible } from '../match';

// ═══════════════════════════════════════════════════════════════════
// SLOT QUANTITY CALCULATIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Get allocated quantity for a specific slot
 */
export function getSlotAllocatedQuantity(capacity: any, slotId: string): number {
	const slot = capacity.capacity_slots?.find((s: any) => s.id === slotId);
	return slot?.allocated_quantity || 0;
}

/**
 * Get available quantity for a specific slot
 */
export function getSlotAvailableQuantity(capacity: any, slotId: string): number {
	const slot = capacity.capacity_slots?.find((s: any) => s.id === slotId);
	return slot?.available_quantity || slot?.quantity || 0;
}

/**
 * Get count of slots with allocated quantities
 */
export function getAllocatedSlotCount(capacity: any): number {
	if (!capacity.capacity_slots || !Array.isArray(capacity.capacity_slots)) {
		return 0;
	}
	return capacity.capacity_slots.filter((slot: any) => (slot.allocated_quantity || 0) > 0)
		.length;
}

/**
 * Get total number of slots available
 */
export function getTotalSlotCount(capacity: any): number {
	return capacity.capacity_slots?.length || 0;
}

/**
 * Calculate total allocated quantity across all slots
 */
export function getTotalAllocated(capacity: any): number {
	if (!capacity.capacity_slots || !Array.isArray(capacity.capacity_slots)) {
		return 0;
	}
	return capacity.capacity_slots.reduce((total: number, slot: any) => {
		return total + (slot.allocated_quantity || 0);
	}, 0);
}

/**
 * Calculate total available quantity across all slots
 */
export function getTotalAvailable(capacity: any): number {
	if (!capacity.capacity_slots || !Array.isArray(capacity.capacity_slots)) {
		return 0;
	}
	return capacity.capacity_slots.reduce((total: number, slot: any) => {
		return total + (slot.available_quantity || slot.quantity || 0);
	}, 0);
}

// ═══════════════════════════════════════════════════════════════════
// SLOT TYPE AND RECURRENCE
// ═══════════════════════════════════════════════════════════════════

/**
 * Check if a slot is recurring (v5 - considers availability_window)
 */
export function isSlotRecurring(slot: any): boolean {
	if (slot.availability_window) {
		return true; // Availability windows are used for recurring patterns
	}
	return slot.recurrence && slot.recurrence !== 'Does not repeat' && slot.recurrence !== null;
}

/**
 * Get display string for slot recurrence
 */
export function getRecurrenceDisplay(slot: any): string {
	return slot.recurrence || 'Does not repeat';
}

/**
 * Check if a slot is in the past
 */
export function isSlotInPast(slot: any): boolean {
	if (!slot.start_date) return false;

	const now = new Date();
	const slotDate = new Date(slot.start_date);

	if (slot.all_day) {
		const today = new Date(now.getFullYear(), now.getMonth(), now.getDate());
		const slotDateOnly = new Date(slotDate.getFullYear(), slotDate.getMonth(), slotDate.getDate());
		return slotDateOnly < today;
	}

	if (slot.start_time) {
		const [hours, minutes] = slot.start_time.split(':');
		slotDate.setHours(parseInt(hours), parseInt(minutes));
	}

	return slotDate < now;
}

// ═══════════════════════════════════════════════════════════════════
// TIME AND DATE FORMATTING
// ═══════════════════════════════════════════════════════════════════

/**
 * Safely extract time from potentially malformed time strings
 */
export function safeExtractTime(timeValue: string | null | undefined): string | undefined {
	if (!timeValue) return undefined;

	if (/^\d{2}:\d{2}$/.test(timeValue)) {
		return timeValue;
	}

	if (timeValue.includes('T')) {
		try {
			const date = new Date(timeValue);
			return date.toTimeString().substring(0, 5);
		} catch (e) {
			console.warn('Failed to parse time:', timeValue);
			return undefined;
		}
	}

	console.warn('Unknown time format:', timeValue);
	return undefined;
}

/**
 * Format time without leading zeros (08:30 → 8:30)
 */
export function formatTimeClean(timeStr: string): string {
	if (!timeStr) return timeStr;

	const [hours, minutes] = timeStr.split(':');
	const cleanHours = parseInt(hours).toString();
	return `${cleanHours}:${minutes}`;
}

/**
 * Format date for display with smart labels
 */
export function formatDateForDisplay(date: Date): string {
	const today = new Date();
	const tomorrow = new Date(today);
	tomorrow.setDate(tomorrow.getDate() + 1);

	if (date.toDateString() === today.toDateString()) {
		return 'Today';
	} else if (date.toDateString() === tomorrow.toDateString()) {
		return 'Tomorrow';
	} else {
		return date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' });
	}
}

/**
 * Format slot time display - comprehensive time formatting for slots
 */
export function formatSlotTimeDisplay(slot: any): string {
	const rawStartTime = safeExtractTime(slot.start_time);
	const rawEndTime = safeExtractTime(slot.end_time);

	const cleanStartTime = rawStartTime ? formatTimeClean(rawStartTime) : '';
	const cleanEndTime = rawEndTime ? formatTimeClean(rawEndTime) : '';

	const recurrenceDisplay =
		slot.recurrence && slot.recurrence !== 'Does not repeat' ? slot.recurrence : '';

	if (slot.all_day) {
		const startDate = slot.start_date ? new Date(slot.start_date) : null;
		const endDate = slot.end_date ? new Date(slot.end_date) : null;

		let timeStr = '';
		if (startDate && endDate && startDate.getTime() !== endDate.getTime()) {
			const startStr = formatDateForDisplay(startDate);
			const endStr = formatDateForDisplay(endDate);
			timeStr = `${startStr} - ${endStr}, All day`;
		} else if (startDate) {
			const dateStr = formatDateForDisplay(startDate);
			timeStr = `${dateStr}, All day`;
		} else {
			timeStr = 'All day';
		}

		return recurrenceDisplay ? `${timeStr} (${recurrenceDisplay})` : timeStr;
	}

	const startDate = slot.start_date ? new Date(slot.start_date) : null;
	const endDate = slot.end_date ? new Date(slot.end_date) : null;

	let timeStr = '';
	if (startDate) {
		const startDateStr = formatDateForDisplay(startDate);

		if (endDate && startDate.getTime() !== endDate.getTime()) {
			const endDateStr = formatDateForDisplay(endDate);
			const startTimeStr = cleanStartTime || '';
			const endTimeStr = cleanEndTime || '';

			if (startTimeStr && endTimeStr) {
				timeStr = `${startDateStr}, ${startTimeStr} - ${endDateStr}, ${endTimeStr}`;
			} else if (startTimeStr) {
				timeStr = `${startDateStr}, ${startTimeStr} - ${endDateStr}`;
			} else {
				timeStr = `${startDateStr} - ${endDateStr}`;
			}
		} else {
			if (cleanStartTime) {
				const timeRange = cleanEndTime ? `${cleanStartTime}-${cleanEndTime}` : cleanStartTime;
				timeStr = `${startDateStr}, ${timeRange}`;
			} else {
				timeStr = startDateStr;
			}
		}
	} else if (cleanStartTime) {
		timeStr = cleanEndTime ? `${cleanStartTime}-${cleanEndTime}` : cleanStartTime;
	} else {
		timeStr = 'No time set';
	}

	return recurrenceDisplay ? `${timeStr} (${recurrenceDisplay})` : timeStr;
}

/**
 * Format slot location display - show complete address
 */
export function formatSlotLocationDisplay(slot: any): string {
	if (slot.location_type === 'Specific') {
		const addressParts = [];

		if (slot.street_address) addressParts.push(slot.street_address);
		if (slot.city) addressParts.push(slot.city);
		if (slot.state_province) addressParts.push(slot.state_province);
		if (slot.postal_code) addressParts.push(slot.postal_code);
		if (slot.country) addressParts.push(slot.country);

		if (addressParts.length > 0) {
			return addressParts.join(', ');
		}

		if (slot.latitude && slot.longitude) {
			return `${slot.latitude.toFixed(4)}, ${slot.longitude.toFixed(4)}`;
		}
	} else if (slot.location_type === 'Online') {
		if (slot.online_link) {
			if (slot.online_link.startsWith('http')) {
				try {
					const url = new URL(slot.online_link);
					return `Online (${url.hostname})`;
				} catch {
					return 'Online (link provided)';
				}
			}
			return `Online (${slot.online_link.length > 30 ? slot.online_link.substring(0, 30) + '...' : slot.online_link})`;
		}
		return 'Online';
	}

	return slot.location_type || 'No location';
}

/**
 * Get slot sort value for different sort criteria
 */
export function getSlotSortValue(slot: any, sortBy: string): number | string {
	switch (sortBy) {
		case 'time':
			if (!slot.start_date) return '9999-12-31';
			return slot.start_date;
		case 'location':
			return formatSlotLocationDisplay(slot).toLowerCase();
		case 'quantity':
			return slot.allocated_quantity || 0;
		default:
			return 0;
	}
}

// ═══════════════════════════════════════════════════════════════════
// RESOURCE METADATA
// ═══════════════════════════════════════════════════════════════════

/**
 * Extract resource metadata from a slot (v5 - slot-native)
 */
export function extractResourceMetadata(
	resource: AvailabilitySlot | NeedSlot | any
): ResourceMetadata {
	return {
		name: resource.name || '',
		emoji: resource.emoji,
		unit: resource.unit,
		description: resource.description,
		resource_type: resource.resource_type,
		filter_rule: resource.filter_rule,
		hidden_until_request_accepted: resource.hidden_until_request_accepted
	};
}

/**
 * Check if a slot has a filter rule
 */
export function hasFilterRule(slot: AvailabilitySlot | NeedSlot): boolean {
	return slot.filter_rule !== undefined && slot.filter_rule !== null;
}

/**
 * Get effective filter rule for a slot
 */
export function getEffectiveFilterRule(slot: AvailabilitySlot | NeedSlot): any | null {
	return slot.filter_rule !== undefined && slot.filter_rule !== null 
		? slot.filter_rule 
		: null;
}

// ═══════════════════════════════════════════════════════════════════
// SLOT COMPATIBILITY (Delegates to match.ts)
// ═══════════════════════════════════════════════════════════════════

/**
 * Check if two slots have compatible time constraints
 * Delegates to match.ts for hierarchical availability window support
 */
export function areSlotsTimeCompatible(
	availabilitySlot: AvailabilitySlot,
	needSlot: NeedSlot
): boolean {
	return timeRangesOverlap(availabilitySlot, needSlot);
}

/**
 * Check if two slots have compatible location constraints
 * Delegates to match.ts for comprehensive location matching
 */
export function areSlotsLocationCompatible(
	availabilitySlot: AvailabilitySlot,
	needSlot: NeedSlot
): boolean {
	return locationsCompatible(availabilitySlot, needSlot);
}

