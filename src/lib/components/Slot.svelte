<script lang="ts">
	import type { AvailabilitySlot } from '$lib/schema';
	import { AvailabilitySlotSchema } from '$lib/schema';
	import { derived } from 'svelte/store';
	import SlotCompositionItem from '$lib/components/SlotCompositionItem.svelte';
	import DropDown from '$lib/components/DropDown.svelte';

	import {
		userDesiredSlotComposeFrom,
		userDesiredSlotComposeInto,
		networkDesiredSlotComposeFrom,
		networkDesiredSlotComposeInto
	} from '$lib/state/core.svelte';
	import {
		userNetworkCapacitiesWithSlotQuantities,
		networkCapacities,
		userCapacities
	} from '$lib/state/core.svelte';
	import { getUserName, userNamesOrAliasesCache } from '$lib/state/users.svelte';

	interface Props {
		slot: AvailabilitySlot;
		capacityId: string;
		unit?: string;
		canDelete: boolean;
		onupdate?: (slot: AvailabilitySlot) => void;
		ondelete?: (slotId: string) => void;
	}

	let { slot, capacityId, unit, canDelete, onupdate, ondelete }: Props = $props();

	// UI state for expanded slot details sections
	let timeExpanded = $state(false);
	let constraintsExpanded = $state(false);
	let locationExpanded = $state(false);
	let compositionsExpanded = $state(false);

	// Provider name cache for compositions
	let providerNames = $state<Record<string, string>>({});

	// Composition UI state
	let expandedCompositions = $state<Record<string, boolean>>({});
	let showAddComposeFrom = $state(false);
	let showAddComposeInto = $state(false);

	// Data providers for slot selection
	// FROM: Show slots from ALL capacities EXCEPT the current capacity
	let composeFromDataProvider = derived(
		[userNetworkCapacitiesWithSlotQuantities, userCapacities, userNamesOrAliasesCache],
		([$userNetworkCapacitiesWithSlotQuantities, $userCapacities, $userNamesCache]) => {
			const items: Array<{
				id: string;
				name: string;
				metadata: {
					capacityId: string;
					slotId: string;
					capacityName: string;
					providerId?: string;
					providerName?: string;
					quantity: number;
					location?: string;
					timeInfo?: string;
					isOwned: boolean;
				};
			}> = [];

			// Track seen slots to avoid duplicates (capacity:slot)
			const seenSlots = new Set<string>();

			// Helper function to format slot display info
			function formatSlotInfo(slotData: any): { timeInfo: string; location: string } {
				const timeParts = [];

				// Handle start_date - this could be a string date or an ISO string
				if (slotData.start_date) {
					try {
						const date = new Date(slotData.start_date);
						timeParts.push(date.toLocaleDateString());
					} catch (e) {
						// If parsing fails, use the raw string
						timeParts.push(String(slotData.start_date));
					}
				}

				// Handle start_time if not all day
				if (!slotData.all_day && slotData.start_time) {
					// Handle different time formats
					if (typeof slotData.start_time === 'string') {
						if (slotData.start_time.includes('T')) {
							// Handle ISO format dates
							try {
								const date = new Date(slotData.start_time);
								timeParts.push(date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' }));
							} catch (e) {
								// If parsing fails, use the raw string but truncated
								const timeStr = slotData.start_time;
								timeParts.push(timeStr.split('T')[1]?.substring(0, 5) || timeStr);
							}
						} else if (/^\d{2}:\d{2}(:\d{2})?$/.test(slotData.start_time)) {
							// Already in HH:MM or HH:MM:SS format
							timeParts.push(slotData.start_time.substring(0, 5)); // Take just HH:MM
						} else {
							// Unknown format, use as is
							timeParts.push(slotData.start_time);
						}
					}
				}

				// Handle all_day flag
				if (slotData.all_day) {
					timeParts.push('All day');
				}

				// Join time parts and clean up any duplicate information
				let timeInfo = timeParts.join(' ');

				// If we see the full ISO string in the output, remove it
				if (
					slotData.start_date &&
					typeof slotData.start_date === 'string' &&
					slotData.start_date.includes('T')
				) {
					timeInfo = timeInfo.replace(slotData.start_date, '').trim();
				}

				timeInfo = timeInfo || 'No time set';

				// Location handling
				let location = 'No location';
				if (slotData.location_type === 'Specific') {
					if (slotData.street_address) {
						location = slotData.street_address;
					} else if (slotData.latitude && slotData.longitude) {
						location = `${slotData.latitude.toFixed(4)}, ${slotData.longitude.toFixed(4)}`;
					}
				} else if (slotData.location_type) {
					location = slotData.location_type;
				}

				return { timeInfo, location };
			}

			// Add slots from user's own capacities (EXCEPT current capacity)
			if ($userCapacities) {
				Object.entries($userCapacities).forEach(([capId, capacity]) => {
					if (capId === capacityId) return; // Skip current capacity

					if (capacity.availability_slots && Array.isArray(capacity.availability_slots)) {
						capacity.availability_slots.forEach((slotData: any) => {
							if (slotData.id === slot.id) return; // Skip current slot just in case

							// Create unique key to avoid duplicates
							const slotKey = `${capId}:${slotData.id}`;
							if (seenSlots.has(slotKey)) return;
							seenSlots.add(slotKey);

							const { timeInfo, location } = formatSlotInfo(slotData);
							const displayName = `${capacity.emoji || '📦'} ${capacity.name} - ${timeInfo}`;

							items.push({
								id: slotKey, // Use unique key combining capacity and slot ID
								name: displayName,
								metadata: {
									capacityId: capId,
									slotId: slotData.id,
									capacityName: capacity.name,
									quantity: slotData.quantity || 0,
									location,
									timeInfo,
									isOwned: true
								}
							});
						});
					}
				});
			}

			// Add slots from network capacities (shares) - ALL of them since they're other people's
			if ($userNetworkCapacitiesWithSlotQuantities) {
				Object.entries($userNetworkCapacitiesWithSlotQuantities).forEach(([capId, capacity]) => {
					const providerId = (capacity as any).provider_id;
					const providerName = providerId ? $userNamesCache[providerId] || providerId : 'Unknown';

					if (capacity.availability_slots && Array.isArray(capacity.availability_slots)) {
						capacity.availability_slots.forEach((slotData: any) => {
							// Create unique key to avoid duplicates
							const slotKey = `${capId}:${slotData.id}`;
							if (seenSlots.has(slotKey)) return;
							seenSlots.add(slotKey);

							const { timeInfo, location } = formatSlotInfo(slotData);
							const displayName = `${capacity.emoji || '📦'} ${capacity.name} (${providerName}) - ${timeInfo}`;

							items.push({
								id: slotKey, // Use unique key combining capacity and slot ID
								name: displayName,
								metadata: {
									capacityId: capId,
									slotId: slotData.id,
									capacityName: capacity.name,
									providerId,
									providerName,
									quantity: slotData.quantity || 0,
									location,
									timeInfo,
									isOwned: false
								}
							});
						});
					}
				});
			}

			// Sort by capacity name, then time
			return items.sort((a, b) => {
				const capCompare = a.metadata.capacityName.localeCompare(b.metadata.capacityName);
				if (capCompare !== 0) return capCompare;
				const aTime = a.metadata.timeInfo || '';
				const bTime = b.metadata.timeInfo || '';
				return aTime.localeCompare(bTime);
			});
		}
	);

	// INTO: Show allocated slots from ALL capacities except current slot
	let composeIntoDataProvider = derived(
		[userNetworkCapacitiesWithSlotQuantities, userCapacities, userNamesOrAliasesCache],
		([$userNetworkCapacitiesWithSlotQuantities, $userCapacities, $userNamesCache]) => {
			const items: Array<{
				id: string;
				name: string;
				metadata: {
					capacityId: string;
					slotId: string;
					capacityName: string;
					providerId?: string;
					providerName?: string;
					quantity: number;
					location?: string;
					timeInfo?: string;
					isOwned: boolean;
				};
			}> = [];

			// Track seen slots to avoid duplicates (capacity:slot)
			const seenSlots = new Set<string>();

			// Helper function to format slot display info
			function formatSlotInfo(slotData: any): { timeInfo: string; location: string } {
				const timeParts = [];

				// Handle start_date - this could be a string date or an ISO string
				if (slotData.start_date) {
					try {
						const date = new Date(slotData.start_date);
						timeParts.push(date.toLocaleDateString());
					} catch (e) {
						// If parsing fails, use the raw string
						timeParts.push(String(slotData.start_date));
					}
				}

				// Handle start_time if not all day
				if (!slotData.all_day && slotData.start_time) {
					// Handle different time formats
					if (typeof slotData.start_time === 'string') {
						if (slotData.start_time.includes('T')) {
							// Handle ISO format dates
							try {
								const date = new Date(slotData.start_time);
								timeParts.push(date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' }));
							} catch (e) {
								// If parsing fails, use the raw string but truncated
								const timeStr = slotData.start_time;
								timeParts.push(timeStr.split('T')[1]?.substring(0, 5) || timeStr);
							}
						} else if (/^\d{2}:\d{2}(:\d{2})?$/.test(slotData.start_time)) {
							// Already in HH:MM or HH:MM:SS format
							timeParts.push(slotData.start_time.substring(0, 5)); // Take just HH:MM
						} else {
							// Unknown format, use as is
							timeParts.push(slotData.start_time);
						}
					}
				}

				// Handle all_day flag
				if (slotData.all_day) {
					timeParts.push('All day');
				}

				// Join time parts and clean up any duplicate information
				let timeInfo = timeParts.join(' ');

				// If we see the full ISO string in the output, remove it
				if (
					slotData.start_date &&
					typeof slotData.start_date === 'string' &&
					slotData.start_date.includes('T')
				) {
					timeInfo = timeInfo.replace(slotData.start_date, '').trim();
				}

				timeInfo = timeInfo || 'No time set';

				// Location handling
				let location = 'No location';
				if (slotData.location_type === 'Specific') {
					if (slotData.street_address) {
						location = slotData.street_address;
					} else if (slotData.latitude && slotData.longitude) {
						location = `${slotData.latitude.toFixed(4)}, ${slotData.longitude.toFixed(4)}`;
					}
				} else if (slotData.location_type) {
					location = slotData.location_type;
				}

				return { timeInfo, location };
			}

			// Add slots from user's own capacities (INCLUDING current capacity for INTO)
			if ($userCapacities) {
				Object.entries($userCapacities).forEach(([capId, capacity]) => {
					if (capacity.availability_slots && Array.isArray(capacity.availability_slots)) {
						capacity.availability_slots.forEach((slotData: any) => {
							if (slotData.id === slot.id && capId === capacityId) return; // Skip current slot

							// Create unique key to avoid duplicates
							const slotKey = `${capId}:${slotData.id}`;
							if (seenSlots.has(slotKey)) return;
							seenSlots.add(slotKey);

							const { timeInfo, location } = formatSlotInfo(slotData);
							const displayName = `${capacity.emoji || '📦'} ${capacity.name} - ${timeInfo}`;

							items.push({
								id: slotKey,
								name: displayName,
								metadata: {
									capacityId: capId,
									slotId: slotData.id,
									capacityName: capacity.name,
									quantity: slotData.quantity || 0,
									location,
									timeInfo,
									isOwned: true
								}
							});
						});
					}
				});
			}

			// Add slots from network capacities (shares)
			if ($userNetworkCapacitiesWithSlotQuantities) {
				Object.entries($userNetworkCapacitiesWithSlotQuantities).forEach(([capId, capacity]) => {
					const providerId = (capacity as any).provider_id;
					const providerName = providerId ? $userNamesCache[providerId] || providerId : 'Unknown';

					if (capacity.availability_slots && Array.isArray(capacity.availability_slots)) {
						capacity.availability_slots.forEach((slotData: any) => {
							// Create unique key to avoid duplicates
							const slotKey = `${capId}:${slotData.id}`;
							if (seenSlots.has(slotKey)) return;
							seenSlots.add(slotKey);

							const { timeInfo, location } = formatSlotInfo(slotData);
							const displayName = `${capacity.emoji || '📦'} ${capacity.name} (${providerName}) - ${timeInfo}`;

							items.push({
								id: slotKey,
								name: displayName,
								metadata: {
									capacityId: capId,
									slotId: slotData.id,
									capacityName: capacity.name,
									providerId,
									providerName,
									quantity: slotData.quantity || 0,
									location,
									timeInfo,
									isOwned: false
								}
							});
						});
					}
				});
			}

			// Sort by capacity name, then time
			return items.sort((a, b) => {
				const capCompare = a.metadata.capacityName.localeCompare(b.metadata.capacityName);
				if (capCompare !== 0) return capCompare;
				const aTime = a.metadata.timeInfo || '';
				const bTime = b.metadata.timeInfo || '';
				return aTime.localeCompare(bTime);
			});
		}
	);

	// Reactive slot properties for proper binding
	let slotId = $state(slot.id);
	let slotQuantity = $state(slot.quantity);
	let slotAdvanceNoticeHours = $state(slot.advance_notice_hours);
	let slotBookingWindowHours = $state(slot.booking_window_hours);
	let slotMutualAgreementRequired = $state(slot.mutual_agreement_required);

	// Time fields
	let slotAllDay = $state(slot.all_day);
	let slotStartDate = $state(slot.start_date);
	let slotEndDate = $state(slot.end_date);
	let slotStartTime = $state(slot.start_time);
	let slotEndTime = $state(slot.end_time);

	// Computed properties for time inputs (convert stored values to HH:MM format)
	let displayStartTime = $derived(() => safeExtractTime(slotStartTime) || '');
	let displayEndTime = $derived(() => safeExtractTime(slotEndTime) || '');
	let slotTimeZone = $state(slot.time_zone);
	let slotRecurrence = $state(slot.recurrence);
	let slotCustomRecurrenceRepeatEvery = $state(slot.custom_recurrence_repeat_every);
	let slotCustomRecurrenceRepeatUnit = $state(slot.custom_recurrence_repeat_unit);
	let slotCustomRecurrenceEndType = $state(slot.custom_recurrence_end_type);
	let slotCustomRecurrenceEndValue = $state(slot.custom_recurrence_end_value);

	// Location fields
	let slotLocationType = $state(slot.location_type);
	let slotLongitude = $state(slot.longitude);
	let slotLatitude = $state(slot.latitude);
	let slotStreetAddress = $state(slot.street_address);
	let slotCity = $state(slot.city);
	let slotStateProvince = $state(slot.state_province);
	let slotPostalCode = $state(slot.postal_code);
	let slotCountry = $state(slot.country);
	let slotOnlineLink = $state(slot.online_link);

	// Location format state ('coordinates' or 'address')
	let locationFormat = $state<'coordinates' | 'address'>('address');

	// Track original values for change detection
	let originalValues = $state<Record<string, any>>({});

	// Recurrence options
	const recurrenceOptions = [
		'Does not repeat',
		'Daily',
		'Weekly',
		'Monthly',
		'Annually',
		'Every weekday (Monday to Friday)',
		'Every 4 days',
		'Custom...'
	];

	// Helper to track original value on focus
	function handleFocus(fieldName: string, currentValue: any) {
		originalValues[fieldName] = currentValue;
	}

	// Helper to save only if value changed on blur
	function handleBlurIfChanged(fieldName: string, currentValue: any) {
		// 🚨 DEBUG: Log blur events for location fields
		if (
			[
				'street_address',
				'city',
				'state_province',
				'postal_code',
				'country',
				'latitude',
				'longitude',
				'location_type'
			].includes(fieldName)
		) {
			console.log(
				`[SLOT] 🚨 DEBUG: handleBlurIfChanged called for location field '${fieldName}':`,
				{
					originalValue: originalValues[fieldName],
					currentValue: currentValue,
					changed: originalValues[fieldName] !== currentValue,
					slotId: slot.id
				}
			);
		}

		if (originalValues[fieldName] !== currentValue) {
			console.log(`[SLOT] 🚨 DEBUG: Field '${fieldName}' changed, calling handleSlotUpdate`);
			handleSlotUpdate();
		} else {
			console.log(`[SLOT] 🚨 DEBUG: Field '${fieldName}' unchanged, not calling handleSlotUpdate`);
		}
	}

	// Handler for slot updates
	function handleSlotUpdate() {
		const updatedSlot = {
			...slot,
			id: slotId,
			quantity: slotQuantity,
			advance_notice_hours: slotAdvanceNoticeHours,
			booking_window_hours: slotBookingWindowHours,
			mutual_agreement_required: slotMutualAgreementRequired,

			// Time fields
			all_day: slotAllDay,
			start_date: slotStartDate,
			end_date: slotEndDate,
			start_time: slotStartTime,
			end_time: slotEndTime,
			time_zone: slotTimeZone,
			recurrence: slotRecurrence,
			custom_recurrence_repeat_every: slotCustomRecurrenceRepeatEvery,
			custom_recurrence_repeat_unit: slotCustomRecurrenceRepeatUnit,
			custom_recurrence_end_type: slotCustomRecurrenceEndType,
			custom_recurrence_end_value: slotCustomRecurrenceEndValue,

			// Location fields
			location_type: slotLocationType,
			longitude: slotLongitude,
			latitude: slotLatitude,
			street_address: slotStreetAddress,
			city: slotCity,
			state_province: slotStateProvince,
			postal_code: slotPostalCode,
			country: slotCountry,
			online_link: slotOnlineLink
		};

		// 🚨 DEBUG: Log the location data being sent in handleSlotUpdate
		console.log(`[SLOT] 🚨 DEBUG: handleSlotUpdate called for slot ${slot.id}`);
		console.log(`[SLOT] 🚨 DEBUG: Current location state variables:`, {
			slotLocationType,
			slotLatitude,
			slotLongitude,
			slotStreetAddress,
			slotCity,
			slotStateProvince,
			slotPostalCode,
			slotCountry
		});
		console.log(`[SLOT] 🚨 DEBUG: Location data in updatedSlot:`, {
			location_type: updatedSlot.location_type,
			latitude: updatedSlot.latitude,
			longitude: updatedSlot.longitude,
			street_address: updatedSlot.street_address,
			city: updatedSlot.city,
			state_province: updatedSlot.state_province,
			postal_code: updatedSlot.postal_code,
			country: updatedSlot.country
		});

		// Validate using schema
		const validationResult = AvailabilitySlotSchema.safeParse(updatedSlot);

		if (!validationResult.success) {
			console.error('Slot validation failed:', validationResult.error);
			return;
		}

		// 🚨 DEBUG: Log the validated data being passed to onupdate
		console.log(`[SLOT] 🚨 DEBUG: Validation passed, calling onupdate with:`, {
			location_type: validationResult.data.location_type,
			latitude: validationResult.data.latitude,
			longitude: validationResult.data.longitude,
			street_address: validationResult.data.street_address,
			city: validationResult.data.city,
			state_province: validationResult.data.state_province,
			postal_code: validationResult.data.postal_code,
			country: validationResult.data.country
		});

		// Validation passed, proceed with update
		onupdate?.(validationResult.data);
	}

	// Delete this slot
	function handleDelete() {
		ondelete?.(slot.id);
	}

	// Toggle expanded sections - ensure only one is open at a time
	function toggleTime() {
		timeExpanded = !timeExpanded;
		// Close other sections when opening this one
		if (timeExpanded) {
			constraintsExpanded = false;
			locationExpanded = false;
			compositionsExpanded = false;
		}
	}

	function toggleConstraints() {
		constraintsExpanded = !constraintsExpanded;
		// Close other sections when opening this one
		if (constraintsExpanded) {
			timeExpanded = false;
			locationExpanded = false;
			compositionsExpanded = false;
		}
	}

	function toggleLocation() {
		locationExpanded = !locationExpanded;
		// Close other sections when opening this one
		if (locationExpanded) {
			timeExpanded = false;
			constraintsExpanded = false;
			compositionsExpanded = false;
		}
	}

	function toggleCompositions() {
		compositionsExpanded = !compositionsExpanded;
		// Close other sections when opening this one
		if (compositionsExpanded) {
			timeExpanded = false;
			constraintsExpanded = false;
			locationExpanded = false;
		}
	}

	// Helper function to safely extract time from potentially malformed time strings
	function safeExtractTime(timeValue: string | null | undefined): string | undefined {
		if (!timeValue) return undefined;

		// If it's already in HH:MM format, return as-is
		if (/^\d{2}:\d{2}$/.test(timeValue)) {
			return timeValue;
		}

		// If it's an ISO datetime string, extract just the time part
		if (timeValue.includes('T')) {
			try {
				const date = new Date(timeValue);
				return date.toTimeString().substring(0, 5); // Get HH:MM from "HH:MM:SS GMT..."
			} catch (e) {
				console.warn('Failed to parse time:', timeValue);
				return undefined;
			}
		}

		// If it's some other format, try to extract time
		console.warn('Unknown time format:', timeValue);
		return undefined;
	}

	// Helper function to format time without leading zeros (08:30 → 8:30)
	function formatTimeClean(timeStr: string): string {
		if (!timeStr) return timeStr;

		const [hours, minutes] = timeStr.split(':');
		const cleanHours = parseInt(hours).toString(); // Remove leading zero
		return `${cleanHours}:${minutes}`;
	}

	// Format time display for button - clean and simple
	function formatTimeDisplay(): string {
		// Use the display values which are already in clean HH:MM format
		const rawStartTime = displayStartTime();
		const rawEndTime = displayEndTime();

		// Format times without leading zeros
		const cleanStartTime = rawStartTime ? formatTimeClean(rawStartTime) : '';
		const cleanEndTime = rawEndTime ? formatTimeClean(rawEndTime) : '';

		// Get recurrence display
		const recurrenceDisplay =
			slotRecurrence && slotRecurrence !== 'Does not repeat' ? slotRecurrence : '';

		// Handle "All day" case first
		if (slotAllDay) {
			const startDate = slotStartDate ? new Date(slotStartDate) : null;
			const endDate = slotEndDate ? new Date(slotEndDate) : null;

			let timeStr = '';
			if (startDate && endDate && startDate.getTime() !== endDate.getTime()) {
				// Multi-day all-day event
				const startStr = formatDateForDisplay(startDate);
				const endStr = formatDateForDisplay(endDate);
				timeStr = `${startStr} - ${endStr}, All day`;
			} else if (startDate) {
				// Single day all-day event
				const dateStr = formatDateForDisplay(startDate);
				timeStr = `${dateStr}, All day`;
			} else {
				timeStr = 'All day';
			}

			// Add recurrence if present
			return recurrenceDisplay ? `${timeStr} (${recurrenceDisplay})` : timeStr;
		}

		// Handle timed slots
		const startDate = slotStartDate ? new Date(slotStartDate) : null;
		const endDate = slotEndDate ? new Date(slotEndDate) : null;

		let timeStr = '';
		if (startDate) {
			const startDateStr = formatDateForDisplay(startDate);

			// Check if we have an end date and it's different from start date
			if (endDate && startDate.getTime() !== endDate.getTime()) {
				// Multi-day timed event
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
				// Single day or no end date
				if (cleanStartTime) {
					const timeRange = cleanEndTime ? `${cleanStartTime}-${cleanEndTime}` : cleanStartTime;
					timeStr = `${startDateStr}, ${timeRange}`;
				} else {
					timeStr = startDateStr;
				}
			}
		} else if (cleanStartTime) {
			// Just time, no date
			timeStr = cleanEndTime ? `${cleanStartTime}-${cleanEndTime}` : cleanStartTime;
		} else {
			timeStr = 'No time set';
		}

		// Add recurrence if present
		return recurrenceDisplay ? `${timeStr} (${recurrenceDisplay})` : timeStr;
	}

	// Helper function to format date for display with smart labels
	function formatDateForDisplay(date: Date): string {
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

	// Format location display - show complete address
	function formatLocationDisplay(): string {
		if (slotLocationType === 'Specific') {
			// Build complete address from components
			const addressParts = [];

			if (slotStreetAddress) {
				addressParts.push(slotStreetAddress);
			}

			if (slotCity) {
				addressParts.push(slotCity);
			}

			if (slotStateProvince) {
				addressParts.push(slotStateProvince);
			}

			if (slotPostalCode) {
				addressParts.push(slotPostalCode);
			}

			if (slotCountry) {
				addressParts.push(slotCountry);
			}

			// If we have address components, join them with commas
			if (addressParts.length > 0) {
				return addressParts.join(', ');
			}

			// Fall back to coordinates if no address components
			if (slotLatitude && slotLongitude) {
				return `${slotLatitude.toFixed(4)}, ${slotLongitude.toFixed(4)}`;
			}
		}

		return slotLocationType || 'No location';
	}

	// Format date for input
	function formatDateForInput(date: Date | undefined): string {
		if (!date) return '';
		return date.toISOString().split('T')[0];
	}

	// Format constraints display
	function formatConstraintsDisplay(): string {
		const parts = [];

		if (slotAdvanceNoticeHours && slotAdvanceNoticeHours > 0) {
			parts.push(`${slotAdvanceNoticeHours}h notice`);
		}

		if (slotBookingWindowHours && slotBookingWindowHours > 0) {
			parts.push(`${slotBookingWindowHours}h window`);
		}

		if (slotMutualAgreementRequired) {
			parts.push('Agreement req.');
		}

		return parts.length > 0 ? parts.join(', ') : 'No constraints';
	}

	// Load provider names asynchronously
	$effect(() => {
		void (async () => {
			const capacities = Object.values($userNetworkCapacitiesWithSlotQuantities || {});
			const uniqueProviders = [
				...new Set(capacities.map((cap: any) => cap.provider_id).filter(Boolean))
			];

			for (const providerId of uniqueProviders) {
				if (!providerNames[providerId]) {
					try {
						const name = await getUserName(providerId);
						if (name) {
							providerNames = {
								...providerNames,
								[providerId]: name.length > 20 ? name.substring(0, 20) + '...' : name
							};
						}
					} catch (error) {
						console.warn('Failed to get provider name:', providerId, error);
					}
				}
			}
		})();
	});

	// ============================================================================
	// COMPOSITION LOGIC - Following Original Capacity Pattern
	// ============================================================================

	// FROM other slots = compositions coming INTO this slot
	let fromOtherSlots = $derived(() => {
		const result: Array<{
			sourceCapacityId: string;
			sourceSlotId: string;
			targetCapacityId: string;
			targetSlotId: string;
			desiredAmount: number;
			direction: 'from';
		}> = [];

		// Get all available slots that could compose INTO this slot
		const availableSlots = $composeIntoDataProvider;

		availableSlots.forEach((slotItem) => {
			const sourceCapacityId = slotItem.metadata.capacityId;
			const sourceSlotId = slotItem.metadata.slotId;

			// Check if we have a user desire for this composition (we want FROM their slot INTO our slot)
			const userDesire =
				$userDesiredSlotComposeFrom[sourceCapacityId]?.[sourceSlotId]?.[capacityId]?.[slot.id] || 0;

			// Check if there's a network desire for this composition (they want FROM their slot INTO our slot)
			// From their perspective, this is a ComposeInto desire (INTO our slot)
			let networkDesire = 0;
			Object.values($networkDesiredSlotComposeInto).forEach((userCompositions) => {
				const desire = userCompositions[sourceCapacityId]?.[sourceSlotId]?.[capacityId]?.[slot.id];
				if (desire > networkDesire) networkDesire = desire;
			});

			// Include slots where EITHER party has expressed desire (user OR network)
			if (userDesire > 0 || networkDesire > 0) {
				result.push({
					sourceCapacityId,
					sourceSlotId,
					targetCapacityId: capacityId,
					targetSlotId: slot.id,
					desiredAmount: Math.max(userDesire, networkDesire),
					direction: 'from'
				});
			}
		});

		return result;
	});

	// INTO other slots = compositions going FROM this slot
	let intoOtherSlots = $derived(() => {
		const result: Array<{
			sourceCapacityId: string;
			sourceSlotId: string;
			targetCapacityId: string;
			targetSlotId: string;
			desiredAmount: number;
			direction: 'into';
		}> = [];

		// Get all available slots that this slot could compose INTO
		const availableSlots = $composeFromDataProvider;

		availableSlots.forEach((slotItem) => {
			const targetCapacityId = slotItem.metadata.capacityId;
			const targetSlotId = slotItem.metadata.slotId;

			// Check if we have a user desire for this composition (we want FROM our slot INTO their slot)
			const userDesire =
				$userDesiredSlotComposeInto[capacityId]?.[slot.id]?.[targetCapacityId]?.[targetSlotId] || 0;

			// Check if there's a network desire for this composition (they want FROM our slot INTO their slot)
			// From their perspective, this is a ComposeFrom desire (FROM our slot)
			let networkDesire = 0;
			Object.values($networkDesiredSlotComposeFrom).forEach((userCompositions) => {
				const desire = userCompositions[capacityId]?.[slot.id]?.[targetCapacityId]?.[targetSlotId];
				if (desire > networkDesire) networkDesire = desire;
			});

			// Include slots where EITHER party has expressed desire (user OR network)
			if (userDesire > 0 || networkDesire > 0) {
				result.push({
					sourceCapacityId: capacityId,
					sourceSlotId: slot.id,
					targetCapacityId,
					targetSlotId,
					desiredAmount: Math.max(userDesire, networkDesire),
					direction: 'into'
				});
			}
		});

		return result;
	});

	// Total compositions count for display in button
	let totalCompositionsCount = $derived(() => {
		return fromOtherSlots().length + intoOtherSlots().length;
	});

	// Toggle composition item expansion
	function toggleCompositionItem(compositionKey: string) {
		expandedCompositions = {
			...expandedCompositions,
			[compositionKey]: !expandedCompositions[compositionKey]
		};
	}

	// Handle adding new compose-from relationship
	function handleAddComposeFrom() {
		showAddComposeFrom = !showAddComposeFrom;
		if (showAddComposeFrom) {
			showAddComposeInto = false;
		}
	}

	// Handle adding new compose-into relationship
	function handleAddComposeInto() {
		showAddComposeInto = !showAddComposeInto;
		if (showAddComposeInto) {
			showAddComposeFrom = false;
		}
	}

	// Handle selecting a target slot for FROM composition (our slot → target slot)
	function handleSelectComposeFromSlot(data: { id: string; name: string; metadata?: any }) {
		const targetCapacityId = data.metadata?.capacityId;
		const targetSlotId = data.metadata?.slotId;

		if (!targetCapacityId || !targetSlotId) {
			console.error('Invalid slot data for FROM composition:', data);
			return;
		}

		// Initialize the composition desire in userDesiredSlotComposeInto
		// Structure: [sourceCapacity][sourceSlot][targetCapacity][targetSlot]
		const currentDesires = $userDesiredSlotComposeInto;
		if (!currentDesires[capacityId]) currentDesires[capacityId] = {};
		if (!currentDesires[capacityId][slot.id]) currentDesires[capacityId][slot.id] = {};
		if (!currentDesires[capacityId][slot.id][targetCapacityId]) {
			currentDesires[capacityId][slot.id][targetCapacityId] = {};
		}

		// Set initial desire of 1 unit
		currentDesires[capacityId][slot.id][targetCapacityId][targetSlotId] = 1;

		userDesiredSlotComposeInto.set(currentDesires);

		console.log('[SLOT-COMPOSITION] Added INTO composition to user store, will sync via Gun:', {
			sourceCapacity: capacityId,
			sourceSlot: slot.id,
			targetCapacity: targetCapacityId,
			targetSlot: targetSlotId
		});

		showAddComposeFrom = false;
	}

	// Handle selecting a source slot for INTO composition (source slot → our slot)
	function handleSelectComposeIntoSlot(data: { id: string; name: string; metadata?: any }) {
		const sourceCapacityId = data.metadata?.capacityId;
		const sourceSlotId = data.metadata?.slotId;

		if (!sourceCapacityId || !sourceSlotId) {
			console.error('Invalid slot data for INTO composition:', data);
			return;
		}

		// Initialize the composition desire in userDesiredSlotComposeFrom
		// Structure: [sourceCapacity][sourceSlot][targetCapacity][targetSlot]
		const currentDesires = $userDesiredSlotComposeFrom;
		if (!currentDesires[sourceCapacityId]) currentDesires[sourceCapacityId] = {};
		if (!currentDesires[sourceCapacityId][sourceSlotId])
			currentDesires[sourceCapacityId][sourceSlotId] = {};
		if (!currentDesires[sourceCapacityId][sourceSlotId][capacityId]) {
			currentDesires[sourceCapacityId][sourceSlotId][capacityId] = {};
		}

		// Set initial desire of 1 unit
		currentDesires[sourceCapacityId][sourceSlotId][capacityId][slot.id] = 1;

		userDesiredSlotComposeFrom.set(currentDesires);

		console.log('[SLOT-COMPOSITION] Added FROM composition to user store, will sync via Gun:', {
			sourceCapacity: sourceCapacityId,
			sourceSlot: sourceSlotId,
			targetCapacity: capacityId,
			targetSlot: slot.id
		});

		showAddComposeInto = false;
	}
</script>

<div class="slot-item rounded border border-gray-200 bg-white p-3 shadow-sm">
	<!-- Slot header row -->
	<div class="slot-header mb-2 flex flex-wrap items-center gap-2">
		<!-- Quantity input -->
		<input
			type="number"
			class="slot-input qty w-20 text-right"
			min="0"
			step="0.01"
			bind:value={slotQuantity}
			placeholder="Qty"
			onfocus={() => handleFocus('quantity', slotQuantity)}
			onblur={() => handleBlurIfChanged('quantity', slotQuantity)}
		/>
		{#if unit}
			<span class="slot-unit text-xs text-gray-500">({unit})</span>
		{/if}

		<!-- Time button -->
		<button
			type="button"
			class="section-btn time-btn"
			onclick={toggleTime}
			title="Edit time settings"
		>
			⏰ {formatTimeDisplay()}
		</button>

		<!-- Location button -->
		<button
			type="button"
			class="section-btn location-btn"
			onclick={toggleLocation}
			title="Edit location"
		>
			📍 {formatLocationDisplay()}
		</button>

		<!-- Compositions button -->
		<button
			type="button"
			class="section-btn compositions-btn"
			onclick={toggleCompositions}
			title="View slot compositions"
		>
			🔄 Compositions ({totalCompositionsCount()})
		</button>

		<!-- Constraints button -->
		<button
			type="button"
			class="section-btn constraints-btn"
			onclick={toggleConstraints}
			title="Edit constraints"
		>
			⚙️ {formatConstraintsDisplay()}
		</button>

		<!-- Delete button -->
		<button
			type="button"
			class="delete-btn"
			onclick={handleDelete}
			disabled={!canDelete}
			title="Delete slot"
		>
			✖️
		</button>
	</div>

	<!-- Expanded slot details -->
	<!-- Time section -->
	{#if timeExpanded}
		<div class="slot-details time-details mt-3 rounded bg-blue-50 p-4">
			<h5 class="mb-3 text-sm font-medium text-gray-700">⏰ Time Settings</h5>

			<div class="mb-4">
				<label class="inline-flex items-center">
					<input
						type="checkbox"
						bind:checked={slotAllDay}
						class="mr-2"
						onchange={handleSlotUpdate}
					/>
					<span class="text-sm text-gray-600">All day</span>
				</label>
			</div>

			<div class="mb-4 grid grid-cols-1 gap-4 md:grid-cols-2">
				<div>
					<label for="slot-start-date" class="mb-1 block text-xs text-gray-500">Start Date</label>
					<input
						id="slot-start-date"
						type="date"
						class="slot-input w-full"
						bind:value={slotStartDate}
						onfocus={() => handleFocus('startDate', slotStartDate)}
						onblur={() => handleBlurIfChanged('startDate', slotStartDate)}
						onchange={() => {
							// If start date is after end date, clear the end date
							if (slotStartDate && slotEndDate) {
								const startDate = new Date(slotStartDate);
								const endDate = new Date(slotEndDate);
								if (startDate > endDate) {
									slotEndDate = null;
									console.log('[SLOT] Start date is after end date, clearing end date');
								}
							}
							handleSlotUpdate();
						}}
					/>
				</div>
				<div>
					<label for="slot-end-date" class="mb-1 block text-xs text-gray-500">End Date</label>
					<input
						id="slot-end-date"
						type="date"
						class="slot-input w-full"
						bind:value={slotEndDate}
						onfocus={() => handleFocus('endDate', slotEndDate)}
						onblur={() => handleBlurIfChanged('endDate', slotEndDate)}
						onchange={() => {
							// If end date is before start date, clear the end date
							if (slotStartDate && slotEndDate) {
								const startDate = new Date(slotStartDate);
								const endDate = new Date(slotEndDate);
								if (endDate < startDate) {
									slotEndDate = null;
									console.log('[SLOT] End date is before start date, clearing end date');
								}
							}
							handleSlotUpdate();
						}}
					/>
				</div>
			</div>

			{#if !slotAllDay}
				<div class="mb-4 grid grid-cols-1 gap-4 md:grid-cols-2">
					<div>
						<label for="slot-start-time" class="mb-1 block text-xs text-gray-500">Start Time</label>
						<input
							id="slot-start-time"
							type="time"
							class="slot-input w-full"
							value={displayStartTime()}
							onfocus={() => handleFocus('startTime', displayStartTime())}
							onchange={(e) => {
								const target = e.target as HTMLInputElement;
								slotStartTime = target.value; // Store as HH:MM format
								handleSlotUpdate();
							}}
						/>
					</div>
					<div>
						<label for="slot-end-time" class="mb-1 block text-xs text-gray-500">End Time</label>
						<input
							id="slot-end-time"
							type="time"
							class="slot-input w-full"
							value={displayEndTime()}
							onfocus={() => handleFocus('endTime', displayEndTime())}
							onchange={(e) => {
								const target = e.target as HTMLInputElement;
								slotEndTime = target.value; // Store as HH:MM format
								handleSlotUpdate();
							}}
						/>
					</div>
				</div>

				<div class="mb-4">
					<label for="slot-timezone" class="mb-1 block text-xs text-gray-500">Time Zone</label>
					<input
						id="slot-timezone"
						type="text"
						class="slot-input w-full"
						bind:value={slotTimeZone}
						placeholder="e.g. America/New_York"
						onfocus={() => handleFocus('timeZone', slotTimeZone)}
						onblur={() => handleBlurIfChanged('timeZone', slotTimeZone)}
					/>
				</div>
			{/if}

			<!-- Recurrence -->
			<div class="mb-4">
				<label for="slot-recurrence" class="mb-1 block text-xs text-gray-500">Recurrence</label>
				<select
					id="slot-recurrence"
					class="slot-select w-full"
					bind:value={slotRecurrence}
					onchange={handleSlotUpdate}
				>
					{#each recurrenceOptions as option}
						<option value={option}>{option}</option>
					{/each}
				</select>
			</div>

			<!-- Custom recurrence -->
			{#if slotRecurrence === 'Custom...' && slotCustomRecurrenceRepeatEvery !== undefined}
				<div class="custom-recurrence rounded border bg-white p-3">
					<div class="mb-3 flex items-center gap-2">
						<span class="text-xs text-gray-500">Repeat every</span>
						<input
							type="number"
							min="1"
							class="slot-input w-16 text-center"
							bind:value={slotCustomRecurrenceRepeatEvery}
							onfocus={() =>
								handleFocus('customRecurrenceRepeatEvery', slotCustomRecurrenceRepeatEvery)}
							onblur={() =>
								handleBlurIfChanged('customRecurrenceRepeatEvery', slotCustomRecurrenceRepeatEvery)}
						/>
						<select
							class="slot-select w-20"
							bind:value={slotCustomRecurrenceRepeatUnit}
							onchange={handleSlotUpdate}
						>
							<option value="days">days</option>
							<option value="weeks">weeks</option>
							<option value="months">months</option>
							<option value="years">years</option>
						</select>
					</div>
				</div>
			{/if}
		</div>
	{/if}

	<!-- Constraints section -->
	{#if constraintsExpanded}
		<div class="slot-details constraints-details mt-3 rounded bg-yellow-50 p-4">
			<h5 class="mb-3 text-sm font-medium text-gray-700">⚙️ Constraints</h5>
			<div class="grid grid-cols-1 gap-4 md:grid-cols-2">
				<div>
					<label for="slot-advance-notice" class="mb-1 block text-xs text-gray-500"
						>Advance notice (hours)</label
					>
					<input
						id="slot-advance-notice"
						type="number"
						class="slot-input w-full"
						min="0"
						step="0.5"
						bind:value={slotAdvanceNoticeHours}
						placeholder="0"
						onfocus={() => handleFocus('advanceNoticeHours', slotAdvanceNoticeHours)}
						onblur={() => handleBlurIfChanged('advanceNoticeHours', slotAdvanceNoticeHours)}
					/>
				</div>
				<div>
					<label for="slot-booking-window" class="mb-1 block text-xs text-gray-500"
						>Booking window (hours)</label
					>
					<input
						id="slot-booking-window"
						type="number"
						class="slot-input w-full"
						min="0"
						step="0.5"
						bind:value={slotBookingWindowHours}
						placeholder="No limit"
						onfocus={() => handleFocus('bookingWindowHours', slotBookingWindowHours)}
						onblur={() => handleBlurIfChanged('bookingWindowHours', slotBookingWindowHours)}
					/>
				</div>
			</div>

			<div class="mt-4">
				<label class="inline-flex items-center">
					<input
						type="checkbox"
						bind:checked={slotMutualAgreementRequired}
						class="mr-2"
						onchange={handleSlotUpdate}
					/>
					<span class="text-sm text-gray-600">Requires mutual agreement</span>
				</label>
			</div>
		</div>
	{/if}

	<!-- Location section -->
	{#if locationExpanded}
		<div class="slot-details location-details mt-3 rounded bg-green-50 p-4">
			<h5 class="mb-3 text-sm font-medium text-gray-700">📍 Location</h5>

			<div class="mb-4">
				<div class="flex flex-wrap gap-4">
					<label class="inline-flex items-center">
						<input
							type="radio"
							name="location-type-{slotId}"
							value="Undefined"
							bind:group={slotLocationType}
							class="mr-2"
							onchange={handleSlotUpdate}
						/>
						<span class="text-sm text-gray-600">Undefined</span>
					</label>
					<label class="inline-flex items-center">
						<input
							type="radio"
							name="location-type-{slotId}"
							value="LiveLocation"
							bind:group={slotLocationType}
							class="mr-2"
							onchange={handleSlotUpdate}
						/>
						<span class="text-sm text-gray-600">Live location</span>
					</label>
					<label class="inline-flex items-center">
						<input
							type="radio"
							name="location-type-{slotId}"
							value="Specific"
							bind:group={slotLocationType}
							class="mr-2"
							onchange={handleSlotUpdate}
						/>
						<span class="text-sm text-gray-600">Specific</span>
					</label>
					<label class="inline-flex items-center">
						<input
							type="radio"
							name="location-type-{slotId}"
							value="Online"
							bind:group={slotLocationType}
							class="mr-2"
							onchange={handleSlotUpdate}
						/>
						<span class="text-sm text-gray-600">Online</span>
					</label>
				</div>
			</div>

			{#if slotLocationType === 'Specific'}
				<!-- Location format toggle -->
				<div class="mb-4">
					<div class="flex flex-wrap gap-4">
						<label class="inline-flex items-center">
							<input
								type="radio"
								name="location-format-{slotId}"
								value="address"
								checked={locationFormat === 'address'}
								class="mr-2"
								onchange={() => {
									locationFormat = 'address';
								}}
							/>
							<span class="text-sm text-gray-600">Address</span>
						</label>
						<label class="inline-flex items-center">
							<input
								type="radio"
								name="location-format-{slotId}"
								value="coordinates"
								checked={locationFormat === 'coordinates'}
								class="mr-2"
								onchange={() => {
									locationFormat = 'coordinates';
								}}
							/>
							<span class="text-sm text-gray-600">Coordinates</span>
						</label>
					</div>
				</div>

				{#if locationFormat === 'address'}
					<div class="address-fields space-y-3">
						<input
							type="text"
							class="slot-input w-full"
							bind:value={slotStreetAddress}
							placeholder="Street address"
							onfocus={() => handleFocus('streetAddress', slotStreetAddress)}
							onblur={() => handleBlurIfChanged('streetAddress', slotStreetAddress)}
						/>
						<div class="grid grid-cols-2 gap-3">
							<input
								type="text"
								class="slot-input w-full"
								bind:value={slotCity}
								placeholder="City"
								onfocus={() => handleFocus('city', slotCity)}
								onblur={() => handleBlurIfChanged('city', slotCity)}
							/>
							<input
								type="text"
								class="slot-input w-full"
								bind:value={slotStateProvince}
								placeholder="State/Province"
								onfocus={() => handleFocus('stateProvince', slotStateProvince)}
								onblur={() => handleBlurIfChanged('stateProvince', slotStateProvince)}
							/>
						</div>
						<div class="grid grid-cols-2 gap-3">
							<input
								type="text"
								class="slot-input w-full"
								bind:value={slotPostalCode}
								placeholder="Postal code"
								onfocus={() => handleFocus('postalCode', slotPostalCode)}
								onblur={() => handleBlurIfChanged('postalCode', slotPostalCode)}
							/>
							<input
								type="text"
								class="slot-input w-full"
								bind:value={slotCountry}
								placeholder="Country"
								onfocus={() => handleFocus('country', slotCountry)}
								onblur={() => handleBlurIfChanged('country', slotCountry)}
							/>
						</div>
					</div>
				{:else}
					<div class="coordinates-fields grid grid-cols-2 gap-3">
						<div>
							<label for="slot-latitude" class="mb-1 block text-xs text-gray-500">Latitude</label>
							<input
								id="slot-latitude"
								type="number"
								class="slot-input w-full"
								min="-90"
								max="90"
								step="0.000001"
								bind:value={slotLatitude}
								placeholder="37.7749"
								onfocus={() => handleFocus('latitude', slotLatitude)}
								onblur={() => handleBlurIfChanged('latitude', slotLatitude)}
							/>
						</div>
						<div>
							<label for="slot-longitude" class="mb-1 block text-xs text-gray-500">Longitude</label>
							<input
								id="slot-longitude"
								type="number"
								class="slot-input w-full"
								min="-180"
								max="180"
								step="0.000001"
								bind:value={slotLongitude}
								placeholder="-122.4194"
								onfocus={() => handleFocus('longitude', slotLongitude)}
								onblur={() => handleBlurIfChanged('longitude', slotLongitude)}
							/>
						</div>
					</div>
				{/if}
			{/if}

			{#if slotLocationType === 'Online'}
				<div class="online-fields space-y-3">
					<input
						type="url"
						class="slot-input w-full"
						bind:value={slotOnlineLink}
						placeholder="https://example.com/meeting-link or meeting room details"
						onfocus={() => handleFocus('onlineLink', slotOnlineLink)}
						onblur={() => handleBlurIfChanged('onlineLink', slotOnlineLink)}
					/>
					<div class="text-xs text-gray-500">
						Enter a meeting link (Zoom, Teams, etc.) or text describing how to join online
					</div>
				</div>
			{/if}
		</div>
	{/if}

	<!-- Compositions section -->
	{#if compositionsExpanded}
		<div class="slot-details compositions-details mt-3 rounded bg-purple-50 p-4">
			<h5 class="mb-3 text-sm font-medium text-gray-700">🔄 Slot Compositions</h5>

			<div class="composition-columns grid grid-cols-1 gap-4 md:grid-cols-2">
				<!-- Compose FROM column -->
				<div class="compose-from-column">
					<div class="column-header mb-3">
						<h5 class="flex items-center gap-2 text-sm font-medium text-gray-700">
							<span>FROM other slots</span>
						</h5>
						<p class="mt-1 text-xs text-gray-500">Other slots composing into this slot</p>
					</div>

					<div class="column-content">
						<!-- Add slot button -->
						<button type="button" class="add-slot-btn mb-3" onclick={handleAddComposeInto}>
							<span class="add-icon">+</span>
							<span class="add-text">Add source slot</span>
						</button>

						<!-- Add slot dropdown -->
						{#if showAddComposeInto}
							<div class="add-dropdown mb-3">
								<DropDown
									dataProvider={composeIntoDataProvider}
									searchPlaceholder="Select source slot..."
									select={handleSelectComposeIntoSlot}
									show={showAddComposeInto}
									close={() => (showAddComposeInto = false)}
									width={280}
									maxHeight={200}
								/>
							</div>
						{/if}

						<!-- From other slots items -->
						{#if fromOtherSlots().length > 0}
							<div class="composition-items space-y-2">
								{#each fromOtherSlots() as composition}
									{@const compositionKey = `${composition.sourceCapacityId}-${composition.sourceSlotId}-${composition.targetCapacityId}-${composition.targetSlotId}`}
									<SlotCompositionItem
										sourceCapacityId={composition.sourceCapacityId}
										sourceSlotId={composition.sourceSlotId}
										targetCapacityId={composition.targetCapacityId}
										targetSlotId={composition.targetSlotId}
										direction="from"
										currentCapacityId={capacityId}
										currentSlotId={slot.id}
										expanded={expandedCompositions[compositionKey] || false}
										onToggle={() => toggleCompositionItem(compositionKey)}
									/>
								{/each}
							</div>
						{:else}
							<div class="empty-state py-4 text-center text-xs text-gray-500 italic">
								No compositions from other slots yet. Click "Add source slot" to start.
							</div>
						{/if}
					</div>
				</div>

				<!-- Compose INTO column -->
				<div class="compose-into-column">
					<div class="column-header mb-3">
						<h5 class="flex items-center gap-2 text-sm font-medium text-gray-700">
							<span>INTO other slots</span>
						</h5>
						<p class="mt-1 text-xs text-gray-500">This slot composing into other slots</p>
					</div>

					<div class="column-content">
						<!-- Add slot button -->
						<button type="button" class="add-slot-btn mb-3" onclick={handleAddComposeFrom}>
							<span class="add-icon">+</span>
							<span class="add-text">Add target slot</span>
						</button>

						<!-- Add slot dropdown -->
						{#if showAddComposeFrom}
							<div class="add-dropdown mb-3">
								<DropDown
									dataProvider={composeFromDataProvider}
									searchPlaceholder="Select target slot..."
									select={handleSelectComposeFromSlot}
									show={showAddComposeFrom}
									close={() => (showAddComposeFrom = false)}
									width={280}
									maxHeight={200}
								/>
							</div>
						{/if}

						<!-- Into other slots items -->
						{#if intoOtherSlots().length > 0}
							<div class="composition-items space-y-2">
								{#each intoOtherSlots() as composition}
									{@const compositionKey = `${composition.sourceCapacityId}-${composition.sourceSlotId}-${composition.targetCapacityId}-${composition.targetSlotId}`}
									<SlotCompositionItem
										sourceCapacityId={composition.sourceCapacityId}
										sourceSlotId={composition.sourceSlotId}
										targetCapacityId={composition.targetCapacityId}
										targetSlotId={composition.targetSlotId}
										direction="into"
										currentCapacityId={capacityId}
										currentSlotId={slot.id}
										expanded={expandedCompositions[compositionKey] || false}
										onToggle={() => toggleCompositionItem(compositionKey)}
									/>
								{/each}
							</div>
						{:else}
							<div class="empty-state py-4 text-center text-xs text-gray-500 italic">
								No compositions to other slots yet. Click "Add target slot" to start.
							</div>
						{/if}
					</div>
				</div>
			</div>
		</div>
	{/if}
</div>

<style>
	.slot-input {
		font-size: 0.875rem;
		padding: 6px 8px;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		background: white;
		outline: none;
		transition: all 0.2s ease;
	}

	.slot-input::placeholder {
		color: #cbd5e1;
	}

	.slot-input:focus {
		border-color: #3b82f6;
		box-shadow: 0 0 0 1px rgba(59, 130, 246, 0.2);
	}

	.slot-select {
		font-size: 0.875rem;
		padding: 6px 8px;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		background: white;
		outline: none;
		transition: all 0.2s ease;
		appearance: none;
		background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='16' height='16' viewBox='0 0 24 24' fill='none' stroke='%23a1a1aa' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpolyline points='6 9 12 15 18 9'%3E%3C/polyline%3E%3C/svg%3E");
		background-repeat: no-repeat;
		background-position: right 6px center;
		background-size: 16px;
		padding-right: 28px;
	}

	.slot-select:focus {
		border-color: #3b82f6;
		box-shadow: 0 0 0 1px rgba(59, 130, 246, 0.2);
	}

	.delete-btn {
		background: none;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		padding: 2px 6px;
		color: #6b7280;
		font-size: 0.75rem;
		cursor: pointer;
		transition: all 0.2s ease;
		width: 24px;
		height: 24px;
		display: flex;
		align-items: center;
		justify-content: center;
	}

	.delete-btn:hover:not(:disabled) {
		background: #fef2f2;
		border-color: #ef4444;
		color: #ef4444;
	}

	.delete-btn:disabled {
		color: #d1d5db;
		cursor: not-allowed;
	}

	.slot-item {
		animation: slideDown 0.2s ease-out;
	}

	.slot-details {
		animation: slideDown 0.2s ease-out;
	}

	@keyframes slideDown {
		from {
			opacity: 0;
			transform: translateY(-10px);
		}
		to {
			opacity: 1;
			transform: translateY(0);
		}
	}

	.section-btn {
		background: white;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		padding: 4px 8px;
		color: #374151;
		font-size: 0.75rem;
		cursor: pointer;
		transition: all 0.2s ease;
		text-align: left;
		line-height: 1.2;
		white-space: nowrap;
		display: inline-block;
		width: auto;
	}

	.section-btn:hover {
		background: #f9fafb;
		border-color: #d1d5db;
		transform: scale(1.02);
	}

	.time-btn:hover {
		background: #eff6ff;
		border-color: #3b82f6;
		color: #1d4ed8;
	}

	.constraints-btn:hover {
		background: #fffbeb;
		border-color: #f59e0b;
		color: #92400e;
	}

	.location-btn:hover {
		background: #f0fdf4;
		border-color: #10b981;
		color: #047857;
	}

	.compositions-btn:hover {
		background: #faf5ff;
		border-color: #8b5cf6;
		color: #7c2d12;
	}

	.compositions-details {
		background: #faf5ff;
	}

	/* Composition columns styling */
	.composition-columns {
		gap: 1rem;
	}

	.compose-from-column,
	.compose-into-column {
		border: 1px solid #e5e7eb;
		border-radius: 6px;
		background: white;
		padding: 12px;
	}

	.column-header h5 {
		margin: 0;
		color: #374151;
	}

	.column-header p {
		margin: 0;
	}

	.add-slot-btn {
		width: 100%;
		display: flex;
		align-items: center;
		justify-content: center;
		gap: 8px;
		padding: 8px 12px;
		border: 2px dashed #d1d5db;
		border-radius: 6px;
		background: #f9fafb;
		color: #6b7280;
		cursor: pointer;
		transition: all 0.2s ease;
		font-size: 0.875rem;
	}

	.add-slot-btn:hover {
		border-color: #9ca3af;
		background: #f3f4f6;
		color: #374151;
	}

	.add-icon {
		font-size: 1.2em;
		font-weight: bold;
	}

	.add-dropdown {
		animation: slideDown 0.2s ease-out;
		position: relative;
		z-index: 100;
	}

	/* Override the DropDown's fixed positioning for our use case */
	.add-dropdown :global(.dropdown-container) {
		position: relative !important;
		top: 0 !important;
		left: 0 !important;
		width: 100% !important;
		max-width: 280px;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
		border: 1px solid #e5e7eb;
	}

	.empty-state {
		background: rgba(243, 244, 246, 0.5);
		border-radius: 6px;
		border: 1px dashed #d1d5db;
		font-style: italic;
	}

	.composition-items {
		max-height: 300px;
		overflow-y: auto;
	}
</style>
