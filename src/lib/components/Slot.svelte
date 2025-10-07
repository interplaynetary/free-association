<script lang="ts">
	import type { AvailabilitySlot } from '$lib/schema';
	import { AvailabilitySlotSchema } from '$lib/schema';
	import { derived } from 'svelte/store';
	import SlotCompositionItem from '$lib/components/SlotCompositionItem.svelte';
	import DropDown from '$lib/components/DropDown.svelte';
	import CountrySelector from '$lib/components/CountrySelector.svelte';
	import TimezoneSelector from '$lib/components/TimezoneSelector.svelte';
	import { createCompositionTargetsDataProvider } from '$lib/utils/ui-providers.svelte';

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

	// Data providers for slot selection - now includes pubkey targets
	// FROM: Show targets from ALL sources EXCEPT the current slot
	let composeFromDataProvider = createCompositionTargetsDataProvider([`${capacityId}:${slot.id}`]);

	// Legacy derived provider for backward compatibility during transition
	let legacyComposeFromDataProvider = derived(
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
							const displayName = `${capacity.emoji || '🎁'} ${capacity.name} - ${timeInfo}`;

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
							const displayName = `${capacity.emoji || '🎁'} ${capacity.name} (${providerName}) - ${timeInfo}`;

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

	// INTO: Show targets from ALL sources EXCEPT the current slot
	let composeIntoDataProvider = createCompositionTargetsDataProvider([`${capacityId}:${slot.id}`]);

	// Legacy INTO: Show allocated slots from ALL capacities except current slot
	let legacyComposeIntoDataProvider = derived(
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
							const displayName = `${capacity.emoji || '🎁'} ${capacity.name} - ${timeInfo}`;

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
							const displayName = `${capacity.emoji || '🎁'} ${capacity.name} (${providerName}) - ${timeInfo}`;

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

	// Autofill state
	let selectedCountryId = $state('');

	// Track original values for change detection
	let originalValues = $state<Record<string, any>>({});

	// Track last state sent to parent to prevent infinite loops
	let lastSentState = $state<string | null>(null);

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

	// Calculate total occurrences and quantity over the pattern window
	function calculateTotalOccurrences(): {
		occurrences: number;
		totalQuantity: number;
		isInfinite: boolean;
	} | null {
		if (!slotStartDate || !slotRecurrence || slotRecurrence === 'Does not repeat') {
			return null;
		}

		const startDate = new Date(slotStartDate);
		const endDate = slotEndDate ? new Date(slotEndDate) : null;
		const isInfinite = !endDate;

		// For infinite patterns, calculate for one year from start date
		const calculationEndDate = endDate || new Date(startDate.getTime() + 365 * 24 * 60 * 60 * 1000);

		let occurrences = 0;
		const msPerDay = 24 * 60 * 60 * 1000;

		switch (slotRecurrence) {
			case 'Daily':
				occurrences =
					Math.floor((calculationEndDate.getTime() - startDate.getTime()) / msPerDay) + 1;
				break;
			case 'Weekly':
				occurrences =
					Math.floor((calculationEndDate.getTime() - startDate.getTime()) / (7 * msPerDay)) + 1;
				break;
			case 'Monthly':
				// Approximate - 30.44 days per month on average
				occurrences =
					Math.floor((calculationEndDate.getTime() - startDate.getTime()) / (30.44 * msPerDay)) + 1;
				break;
			case 'Annually':
				occurrences =
					Math.floor((calculationEndDate.getTime() - startDate.getTime()) / (365.25 * msPerDay)) +
					1;
				break;
			case 'Every weekday (Monday to Friday)':
				// Approximate - 5/7 of total days
				occurrences =
					Math.floor(((calculationEndDate.getTime() - startDate.getTime()) / msPerDay) * (5 / 7)) +
					1;
				break;
			case 'Every 4 days':
				occurrences =
					Math.floor((calculationEndDate.getTime() - startDate.getTime()) / (4 * msPerDay)) + 1;
				break;
			case 'Custom...':
				if (slotCustomRecurrenceRepeatEvery && slotCustomRecurrenceRepeatUnit) {
					let intervalMs = 0;
					switch (slotCustomRecurrenceRepeatUnit) {
						case 'days':
							intervalMs = slotCustomRecurrenceRepeatEvery * msPerDay;
							break;
						case 'weeks':
							intervalMs = slotCustomRecurrenceRepeatEvery * 7 * msPerDay;
							break;
						case 'months':
							intervalMs = slotCustomRecurrenceRepeatEvery * 30.44 * msPerDay;
							break;
						case 'years':
							intervalMs = slotCustomRecurrenceRepeatEvery * 365.25 * msPerDay;
							break;
					}
					if (intervalMs > 0) {
						occurrences =
							Math.floor((calculationEndDate.getTime() - startDate.getTime()) / intervalMs) + 1;
					}
				}
				break;
			default:
				occurrences = 1;
		}

		occurrences = Math.max(1, occurrences); // At least 1 occurrence
		const totalQuantity = occurrences * (slotQuantity || 0);

		return { occurrences, totalQuantity, isInfinite };
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

		// Validate using schema
		const validationResult = AvailabilitySlotSchema.safeParse(updatedSlot);

		if (!validationResult.success) {
			console.error('Slot validation failed:', validationResult.error);
			return;
		}

		// Compare with last sent state to prevent infinite loops
		const currentState = JSON.stringify(validationResult.data);
		if (currentState === lastSentState) {
			console.log(`[SLOT] Slot data unchanged, skipping update for slot ${slot.id}`);
			return;
		}

		// Update last sent state and notify parent
		lastSentState = currentState;
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

		// Get recurrence display and total calculation
		const recurrenceDisplay =
			slotRecurrence && slotRecurrence !== 'Does not repeat' ? slotRecurrence : '';
		const totalCalc = calculateTotalOccurrences();

		// Handle "All day" case first
		if (slotAllDay) {
			const startDate = slotStartDate ? new Date(slotStartDate) : null;
			const endDate = slotEndDate ? new Date(slotEndDate) : null;

			let timeStr = '';
			if (recurrenceDisplay) {
				// Recurring all-day pattern
				if (startDate) {
					const startStr = formatDateForDisplay(startDate);
					if (endDate) {
						const endStr = formatDateForDisplay(endDate);
						timeStr = `${recurrenceDisplay}, ${startStr} - ${endStr}, All day`;
					} else {
						timeStr = `${recurrenceDisplay}, from ${startStr}, All day`;
					}
				} else {
					timeStr = `${recurrenceDisplay}, All day`;
				}
			} else {
				// One-time all-day event
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
			}

			// Add total quantity if available
			if (totalCalc && recurrenceDisplay) {
				timeStr += ` (${totalCalc.totalQuantity} ${unit || 'units'} total)`;
			}

			return timeStr;
		}

		// Handle timed slots
		const startDate = slotStartDate ? new Date(slotStartDate) : null;
		const endDate = slotEndDate ? new Date(slotEndDate) : null;

		let timeStr = '';
		if (recurrenceDisplay) {
			// Recurring timed pattern
			const timeRange = cleanEndTime
				? `${cleanStartTime || '?'}-${cleanEndTime}`
				: cleanStartTime || 'No time';

			if (startDate) {
				const startStr = formatDateForDisplay(startDate);
				if (endDate) {
					const endStr = formatDateForDisplay(endDate);
					timeStr = `${recurrenceDisplay}, ${timeRange}, ${startStr} - ${endStr}`;
				} else {
					timeStr = `${recurrenceDisplay}, ${timeRange}, from ${startStr}`;
				}
			} else {
				timeStr = `${recurrenceDisplay}, ${timeRange}`;
			}
		} else {
			// One-time timed event
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
		}

		// Add total quantity if available
		if (totalCalc && recurrenceDisplay) {
			timeStr += ` (${totalCalc.totalQuantity} ${unit || 'units'} total)`;
		}

		return timeStr;
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

	// Sync local state when slot prop changes from parent
	$effect(() => {
		// Initialize or update lastSentState when slot prop changes
		// This allows updates from parent while preventing our own circular updates
		const incomingState = JSON.stringify(slot);
		if (incomingState !== lastSentState) {
			lastSentState = incomingState;

			// Sync local state variables with incoming slot data
			slotId = slot.id;
			slotQuantity = slot.quantity;
			slotAdvanceNoticeHours = slot.advance_notice_hours;
			slotBookingWindowHours = slot.booking_window_hours;
			slotMutualAgreementRequired = slot.mutual_agreement_required;
			slotAllDay = slot.all_day;
			slotStartDate = slot.start_date;
			slotEndDate = slot.end_date;
			slotStartTime = slot.start_time;
			slotEndTime = slot.end_time;
			slotTimeZone = slot.time_zone;
			slotRecurrence = slot.recurrence;
			slotCustomRecurrenceRepeatEvery = slot.custom_recurrence_repeat_every;
			slotCustomRecurrenceRepeatUnit = slot.custom_recurrence_repeat_unit;
			slotCustomRecurrenceEndType = slot.custom_recurrence_end_type;
			slotCustomRecurrenceEndValue = slot.custom_recurrence_end_value;
			slotLocationType = slot.location_type;
			slotLongitude = slot.longitude;
			slotLatitude = slot.latitude;
			slotStreetAddress = slot.street_address;
			slotCity = slot.city;
			slotStateProvince = slot.state_province;
			slotPostalCode = slot.postal_code;
			slotCountry = slot.country;
			slotOnlineLink = slot.online_link;
		}
	});

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
			// Only process slot-type targets for FROM compositions
			if (slotItem.metadata.type !== 'slot') return;

			const sourceCapacityId = slotItem.metadata.capacityId;
			const sourceSlotId = slotItem.metadata.slotId;

			// Skip if we don't have valid slot metadata
			if (!sourceCapacityId || !sourceSlotId) return;

			// Check if we have a user desire for this composition (we want FROM their slot INTO our slot)
			const userDesire =
				$userDesiredSlotComposeFrom[sourceCapacityId]?.[sourceSlotId]?.[capacityId]?.[slot.id] || 0;

			// Check if there's a network desire for this composition (they want FROM their slot INTO our slot)
			// From their perspective, this is a ComposeInto desire (INTO our slot)
			let networkDesire = 0;
			Object.values($networkDesiredSlotComposeInto).forEach((userCompositions) => {
				const desire = userCompositions[sourceCapacityId]?.[sourceSlotId]?.[capacityId]?.[slot.id];
				if (desire && desire > networkDesire) networkDesire = desire;
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

	// INTO other targets = compositions going FROM this slot (slots + pubkeys)
	let intoOtherSlots = $derived(() => {
		const result: Array<{
			sourceCapacityId: string;
			sourceSlotId: string;
			targetCapacityId: string;
			targetSlotId: string;
			desiredAmount: number;
			direction: 'into';
		}> = [];

		// Get all available targets that this slot could compose INTO
		const availableTargets = $composeFromDataProvider;

		availableTargets.forEach((targetItem) => {
			let targetCapacityId: string | undefined;
			let targetSlotId: string | undefined;
			let userDesire = 0;
			let networkDesire = 0;

			if (targetItem.metadata.type === 'slot') {
				// Handle slot targets (traditional slot-to-slot composition)
				targetCapacityId = targetItem.metadata.capacityId;
				targetSlotId = targetItem.metadata.slotId;

				if (!targetCapacityId || !targetSlotId) return; // Skip invalid slot targets

				// Check if we have a user desire for this composition
				userDesire =
					$userDesiredSlotComposeInto[capacityId]?.[slot.id]?.[targetCapacityId]?.[targetSlotId] ||
					0;

				// Check if there's a network desire for this composition
				Object.values($networkDesiredSlotComposeFrom).forEach((userCompositions) => {
					const desire =
						userCompositions[capacityId]?.[slot.id]?.[targetCapacityId!]?.[targetSlotId!];
					if (desire && desire > networkDesire) networkDesire = desire;
				});
			} else if (targetItem.metadata.type === 'pubkey') {
				// Handle pubkey targets (slot-to-person composition)
				targetCapacityId = targetItem.id; // pubkey becomes the target "capacity"
				targetSlotId = slot.id; // for pubkey targets, use source slot id (self-consumption pattern)

				// Check if we have a user desire for this pubkey composition
				userDesire =
					$userDesiredSlotComposeInto[capacityId]?.[slot.id]?.[targetCapacityId]?.[targetSlotId] ||
					0;

				// Check if the pubkey user wants our slot (they express this via Share.svelte -> compose-from pattern)
				const pubkeyUserId = targetItem.id;
				networkDesire =
					$networkDesiredSlotComposeFrom[pubkeyUserId]?.[capacityId]?.[slot.id]?.[pubkeyUserId]?.[
						slot.id
					] || 0;
			}

			// Include targets where EITHER party has expressed desire (user OR network) and we have valid target IDs
			if ((userDesire > 0 || networkDesire > 0) && targetCapacityId && targetSlotId) {
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
		console.log('[SLOT-COMPOSITION] Adding INTO composition:', data);

		let targetCapacityId: string;
		let targetSlotId: string;

		if (data.metadata?.type === 'slot') {
			// Handle slot targets (traditional slot-to-slot)
			targetCapacityId = data.metadata.capacityId;
			targetSlotId = data.metadata.slotId;

			if (!targetCapacityId || !targetSlotId) {
				console.error('Invalid slot data for INTO composition:', data);
				return;
			}
		} else if (data.metadata?.type === 'pubkey') {
			// Handle pubkey targets (slot-to-person composition)
			targetCapacityId = data.id; // pubkey becomes the target "capacity"
			targetSlotId = slot.id; // for pubkey targets, use source slot id (self-consumption pattern)
		} else {
			console.error('Unknown target type for INTO composition:', data);
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

		// Update store (Gun handles timestamps natively now)
		userDesiredSlotComposeInto.set(currentDesires);

		console.log('[SLOT-COMPOSITION] Added INTO composition to user store, will sync via Gun:', {
			sourceCapacity: capacityId,
			sourceSlot: slot.id,
			targetCapacity: targetCapacityId,
			targetSlot: targetSlotId,
			targetType: data.metadata?.type || 'unknown'
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

		// Update store (Gun handles timestamps natively now)
		userDesiredSlotComposeFrom.set(currentDesires);

		console.log('[SLOT-COMPOSITION] Added FROM composition to user store, will sync via Gun:', {
			sourceCapacity: sourceCapacityId,
			sourceSlot: sourceSlotId,
			targetCapacity: capacityId,
			targetSlot: slot.id
		});

		showAddComposeInto = false;
	}

	// Handle country selection from CountrySelector
	function handleCountrySelect(country: { id: string; name: string; timezones: string[] }) {
		if (country.name) {
			slotCountry = country.name;
			selectedCountryId = country.id;
			// Optionally set timezone based on country's first timezone
			if (country.timezones && country.timezones.length > 0 && !slotTimeZone) {
				slotTimeZone = country.timezones[0];
			}
			handleSlotUpdate();
		}
	}

	// Handle timezone selection from TimezoneSelector
	function handleTimezoneSelect(timezone: {
		name: string;
		utcOffsetStr: string;
		countries: string[];
	}) {
		if (timezone.name) {
			slotTimeZone = timezone.name;
			handleSlotUpdate();
		}
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

			<!-- Recurrence - PRIMARY SETTING -->
			<div class="recurrence-section mb-6">
				<label for="slot-recurrence" class="mb-2 block text-sm font-medium text-gray-700"
					>Recurrence Pattern</label
				>
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
				<div class="mt-2 text-xs text-gray-500">
					This determines how often the slot repeats within the date range below
				</div>

				<!-- Custom recurrence -->
				{#if slotRecurrence === 'Custom...' && slotCustomRecurrenceRepeatEvery !== undefined}
					<div class="custom-recurrence mt-3">
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
									handleBlurIfChanged(
										'customRecurrenceRepeatEvery',
										slotCustomRecurrenceRepeatEvery
									)}
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

			<!-- Pattern Window (Start/End Dates) -->
			<div class="pattern-window-section mb-6">
				<h6 class="mb-3 text-sm font-medium text-gray-700">Pattern Window</h6>
				<div class="mb-2 text-xs text-gray-500">
					{#if slotRecurrence && slotRecurrence !== 'Does not repeat'}
						Define when this recurrence pattern starts and ends
					{:else}
						Define the specific date(s) for this one-time availability
					{/if}
				</div>

				<div class="mb-4 grid grid-cols-1 gap-4 md:grid-cols-2">
					<div>
						<label for="slot-start-date" class="mb-1 block text-xs text-gray-500">
							{#if slotRecurrence && slotRecurrence !== 'Does not repeat'}
								Pattern Start Date
							{:else}
								Start Date
							{/if}
						</label>
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
						<label for="slot-end-date" class="mb-1 block text-xs text-gray-500">
							{#if slotRecurrence && slotRecurrence !== 'Does not repeat'}
								Pattern End Date (optional)
							{:else}
								End Date (optional)
							{/if}
						</label>
						<input
							id="slot-end-date"
							type="date"
							class="slot-input w-full"
							bind:value={slotEndDate}
							placeholder={slotRecurrence && slotRecurrence !== 'Does not repeat'
								? 'Infinite if empty'
								: 'Same day if empty'}
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

				<!-- Total calculation display -->
				{#if calculateTotalOccurrences()}
					{@const totalCalculation = calculateTotalOccurrences()}
					<div class="total-calculation mt-3 rounded p-3">
						<div class="text-sm font-medium">
							Total Availability: {totalCalculation?.totalQuantity}
							{unit || 'units'}
						</div>
						<div class="text-xs">
							{totalCalculation?.occurrences} occurrences × {slotQuantity}
							{unit || 'units'} each
							{#if totalCalculation?.isInfinite}
								(infinite pattern - showing first year)
							{/if}
						</div>
					</div>
				{/if}
			</div>

			<!-- Time of Day Settings -->
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
					<!-- Timezone Selector integrated directly -->
					<div class="timezone-field">
						<TimezoneSelector
							value={slotTimeZone ?? ''}
							placeholder="Select timezone..."
							onselect={handleTimezoneSelect}
							countryFilter={selectedCountryId ?? ''}
						/>
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
							<!-- Country Selector integrated directly -->
							<div class="country-field">
								<CountrySelector
									value={slotCountry ?? ''}
									placeholder="Select country..."
									onselect={handleCountrySelect}
								/>
							</div>
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
							<span class="add-text">Add source</span>
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
							<span class="add-text">Add target</span>
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

	/* Section styling - matches existing pattern */
	.recurrence-section {
		background: rgba(255, 255, 255, 0.8);
		border: 1px solid rgba(229, 231, 235, 0.6);
		border-radius: 6px;
		padding: 12px;
	}

	.pattern-window-section {
		background: rgba(255, 255, 255, 0.8);
		border: 1px solid rgba(229, 231, 235, 0.6);
		border-radius: 6px;
		padding: 12px;
	}

	.custom-recurrence {
		background: rgba(249, 250, 251, 0.8);
		border: 1px solid rgba(209, 213, 219, 0.6);
		border-radius: 4px;
		padding: 8px;
	}

	/* Total calculation styling - matches existing info pattern */
	.total-calculation {
		background: rgba(239, 246, 255, 0.8);
		border: 1px solid rgba(219, 234, 254, 0.8);
		color: #1e40af;
	}

	.total-calculation .text-sm {
		color: #1d4ed8;
	}

	.total-calculation .text-xs {
		color: #3730a3;
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

	/* Country field styling */
	.country-field {
		width: 100%;
	}

	/* Override CountrySelector styles to match form inputs */
	.country-field :global(.country-select .sv-control) {
		font-size: 0.875rem !important;
		padding: 6px 8px !important;
		border: 1px solid #e5e7eb !important;
		border-radius: 4px !important;
		background: white !important;
		min-height: 34px !important;
		transition: all 0.2s ease !important;
	}

	.country-field :global(.country-select .sv-control:focus-within) {
		border-color: #3b82f6 !important;
		box-shadow: 0 0 0 1px rgba(59, 130, 246, 0.2) !important;
	}

	.country-field :global(.country-select .sv-item) {
		padding: 6px 8px !important;
		font-size: 0.875rem !important;
	}

	.country-field :global(.country-select .sv-dropdown) {
		border: 1px solid #e5e7eb !important;
		border-radius: 4px !important;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15) !important;
		max-height: 200px !important;
	}

	/* Timezone field styling */
	.timezone-field {
		width: 100%;
	}

	/* Override TimezoneSelector styles to match form inputs */
	.timezone-field :global(.timezone-select .sv-control) {
		font-size: 0.875rem !important;
		padding: 6px 8px !important;
		border: 1px solid #e5e7eb !important;
		border-radius: 4px !important;
		background: white !important;
		min-height: 34px !important;
		transition: all 0.2s ease !important;
	}

	.timezone-field :global(.timezone-select .sv-control:focus-within) {
		border-color: #3b82f6 !important;
		box-shadow: 0 0 0 1px rgba(59, 130, 246, 0.2) !important;
	}

	.timezone-field :global(.timezone-select .sv-item) {
		padding: 6px 8px !important;
		font-size: 0.875rem !important;
	}

	.timezone-field :global(.timezone-select .sv-dropdown) {
		border: 1px solid #e5e7eb !important;
		border-radius: 4px !important;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15) !important;
		max-height: 200px !important;
	}
</style>
