<script lang="ts">
	import type { AvailabilitySlot } from '$lib/protocol/schemas';
	import { AvailabilitySlotSchema } from '$lib/protocol/schemas';
	import { derived } from 'svelte/store';
	// V5: Composition feature not yet implemented
	// import SlotCompositionItem from '$lib/components/SlotCompositionItem.svelte';
	import DropDown from '$lib/components/DropDown.svelte';
	import CountrySelector from '$lib/components/CountrySelector.svelte';
	import TimezoneSelector from '$lib/components/TimezoneSelector.svelte';
	import { t } from '$lib/translations';

	// V5: Composition feature not yet implemented in v5
	// TODO: Re-implement slot composition using v5 patterns when needed
	// import {
	// 	userDesiredSlotComposeFrom,
	// 	userDesiredSlotComposeInto,
	// 	networkDesiredSlotComposeFrom,
	// 	networkDesiredSlotComposeInto
	// } from '$lib/state/core.svelte';
	
	// V5: Import capacity stores from v5
	import { myCommitmentStore, networkCommitments, getAllCommitmentsRecord } from '$lib/protocol/stores.svelte';
	import { get } from 'svelte/store';
	import { getUserName, userNamesOrAliasesCache } from '$lib/network/users.svelte';

	interface Props {
		slot: AvailabilitySlot;
		capacityId: string;
		// V5 REMOVED: unit prop (now on slot itself: slot.unit)
		canDelete: boolean;
		onupdate?: (slot: AvailabilitySlot) => void;
		ondelete?: (slotId: string) => void;
	}

	let { slot, capacityId, canDelete, onupdate, ondelete }: Props = $props();

	// UI state for expanded slot details sections
	let timeExpanded = $state(false);
	let constraintsExpanded = $state(false);
	let locationExpanded = $state(false);
	let compositionsExpanded = $state(false);

	// V5: Composition feature not yet implemented
	// TODO: Re-enable when v5 composition is implemented
	// let expandedCompositions = $state<Record<string, boolean>>({});
	// let showAddComposeFrom = $state(false);
	// let showAddComposeInto = $state(false);

	// V5: Composition data providers commented out
	// let composeFromDataProvider = createCompositionTargetsDataProvider([`${capacityId}:${slot.id}`]);
	// let composeIntoDataProvider = createCompositionTargetsDataProvider([`${capacityId}:${slot.id}`]);

	// V5 REQUIRED FIELDS - Core slot metadata
	let slotId = $state(slot.id);
	let slotQuantity = $state(slot.quantity);
	let slotNeedTypeId = $state(slot.need_type_id || 'need_type_general'); // ✅ REQUIRED in v5
	let slotName = $state(slot.name || ''); // ✅ REQUIRED in v5
	let slotEmoji = $state(slot.emoji || '');
	let slotUnit = $state(slot.unit || '');
	let slotDescription = $state(slot.description || '');
	
	// V5 Divisibility constraints (moved from commitment level to slot level)
	let slotMaxNaturalDiv = $state(slot.max_natural_div);
	let slotMaxPercentageDiv = $state(slot.max_percentage_div);
	
	// V5 Filter rule (per-slot filtering)
	let slotFilterRule = $state(slot.filter_rule);
	
	// Booking constraints
	let slotAdvanceNoticeHours = $state(slot.advance_notice_hours);
	let slotBookingWindowHours = $state(slot.booking_window_hours);
	let slotMutualAgreementRequired = $state(slot.mutual_agreement_required);

	// Time fields (v5 uses availability_window, but we support legacy fields for now)
	let slotStartDate = $state(slot.start_date);
	let slotEndDate = $state(slot.end_date);
	let slotTimeZone = $state(slot.time_zone || Intl.DateTimeFormat().resolvedOptions().timeZone);
	let slotRecurrence = $state(slot.recurrence);
	
	// Legacy time fields (will be converted to availability_window on save)
	// These allow existing UI to work while we migrate
	let legacyStartTime = $state<string | null>(null);
	let legacyEndTime = $state<string | null>(null);
	let legacyAllDay = $state(false);
	
	// Initialize legacy fields from availability_window if present
	$effect(() => {
		if (slot.availability_window?.time_ranges?.[0]) {
			const range = slot.availability_window.time_ranges[0];
			legacyStartTime = range.start_time;
			legacyEndTime = range.end_time;
			legacyAllDay = false;
		} else {
			legacyAllDay = true;
		}
	});
	
	// Computed properties for time inputs
	let displayStartTime = $derived(() => legacyStartTime || '');
	let displayEndTime = $derived(() => legacyEndTime || '');

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

	// V5: Simple recurrence options matching v5 schema enum
	// z.enum(['daily', 'weekly', 'monthly', 'yearly']).nullable()
	const recurrenceOptions = [
		{ value: null, label: 'Does not repeat' },
		{ value: 'daily', label: 'Daily' },
		{ value: 'weekly', label: 'Weekly' },
		{ value: 'monthly', label: 'Monthly' },
		{ value: 'yearly', label: 'Yearly' }
	];

	// Helper to track original value on focus
	function handleFocus(fieldName: string, currentValue: any) {
		originalValues[fieldName] = currentValue;
	}

	// V5 REMOVED: calculateTotalOccurrences()
	// No longer needed with simplified v5 recurrence
	// V5 uses 'daily' | 'weekly' | 'monthly' | 'yearly' | null
	// Occurrence tracking will be handled by allocation algorithm if needed
	
	/****** LEGACY FUNCTION REMOVED ******
	function calculateTotalOccurrences(): {
		occurrences: number;
		totalQuantity: number;
		isInfinite: boolean;
	} | null {
		// Based on old string-based recurrence ('Daily', 'Weekly', etc.)
		// V5 uses enum: 'daily' | 'weekly' | 'monthly' | 'yearly' | null
		// ...350 lines of legacy logic...
	}
	**************************************/

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

	// Handler for slot updates (V5)
	function handleSlotUpdate() {
		// Build availability_window from legacy time fields
		let availabilityWindow = undefined;
		if (!legacyAllDay && legacyStartTime && legacyEndTime) {
			availabilityWindow = {
				time_ranges: [
					{
						start_time: legacyStartTime,
						end_time: legacyEndTime
					}
				]
			};
		}
		
		// Create v5-compliant AvailabilitySlot
		const updatedSlot: AvailabilitySlot = {
			// V5 REQUIRED FIELDS
			id: slotId,
			quantity: slotQuantity,
			need_type_id: slotNeedTypeId, // ✅ Required
			name: slotName,                 // ✅ Required
			
			// Optional metadata
			emoji: slotEmoji,
			unit: slotUnit,
			description: slotDescription,
			
			// Divisibility constraints
			max_natural_div: slotMaxNaturalDiv,
			max_percentage_div: slotMaxPercentageDiv,
			
			// Filter rule
			filter_rule: slotFilterRule,
			
			// Booking constraints
			advance_notice_hours: slotAdvanceNoticeHours,
			booking_window_hours: slotBookingWindowHours,
			mutual_agreement_required: slotMutualAgreementRequired,

			// Time fields (v5 structure)
			start_date: slotStartDate,
			end_date: slotEndDate,
			time_zone: slotTimeZone,
			recurrence: slotRecurrence,
			availability_window: availabilityWindow,

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

		// Validate using v5 schema
		const validationResult = AvailabilitySlotSchema.safeParse(updatedSlot);

		if (!validationResult.success) {
			console.error('[SLOT] V5 validation failed:', validationResult.error);
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
		console.log('[SLOT] ✅ Sending v5-compliant slot update');
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

	// V5: Composition feature not yet implemented
	// function toggleCompositions() {
	// 	compositionsExpanded = !compositionsExpanded;
	// 	if (compositionsExpanded) {
	// 		timeExpanded = false;
	// 		constraintsExpanded = false;
	// 		locationExpanded = false;
	// 	}
	// }

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
		const recurrenceDisplay = slotRecurrence || '';
		// V5 REMOVED: totalCalc = calculateTotalOccurrences() - no longer needed

		// Handle "All day" case first
		if (legacyAllDay) {
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

			// V5 REMOVED: Total quantity calculation (was based on legacy calculateTotalOccurrences)
			// Quantity tracking now handled by allocation algorithm

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
				timeStr = $t('inventory.no_time_set');
			}
		}

		// V5 REMOVED: Total quantity calculation
		// (was based on legacy calculateTotalOccurrences)

		return timeStr;
	}

	// Helper function to format date for display with smart labels
	function formatDateForDisplay(date: Date): string {
		const today = new Date();
		const tomorrow = new Date(today);
		tomorrow.setDate(tomorrow.getDate() + 1);

		if (date.toDateString() === today.toDateString()) {
			return $t('inventory.today');
		} else if (date.toDateString() === tomorrow.toDateString()) {
			return $t('inventory.tomorrow');
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

		return slotLocationType || $t('inventory.no_location');
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
			parts.push($t('inventory.agreement_required'));
		}

		return parts.length > 0 ? parts.join(', ') : $t('inventory.no_constraints');
	}

	// ============================================================================
	// COMPOSITION LOGIC - Following Original Capacity Pattern
	// ============================================================================

	// FROM other slots = compositions coming INTO this slot
	// V5: Composition feature not yet implemented - commented out
	// TODO: Re-enable when v5 composition is implemented
	// let fromOtherSlots = $derived(() => {
	// 	const result: Array<{
	// 		sourceCapacityId: string;
	// 		sourceSlotId: string;
	// 		targetCapacityId: string;
	// 		targetSlotId: string;
	// 		desiredAmount: number;
	// 		direction: 'from';
	// 	}> = [];
	// 	return result;
	// });

	// V5: Composition feature not yet implemented - commented out
	// TODO: Re-enable when v5 composition is implemented
	// INTO other targets = compositions going FROM this slot (slots + pubkeys)
	// let intoOtherSlots = $derived(() => {
	// 	const result: Array<{
	// 		sourceCapacityId: string;
	// 		sourceSlotId: string;
	// 		targetCapacityId: string;
	// 		targetSlotId: string;
	// 		desiredAmount: number;
	// 		direction: 'into';
	// 	}> = [];
	// 	return result;
	// });

	// V5: Composition feature not yet implemented - commented out
	// TODO: Re-enable when v5 composition is implemented
	// Total compositions count for display in button
	// let totalCompositionsCount = $derived(() => {
	// 	return fromOtherSlots().length + intoOtherSlots().length;
	// });

	// V5: Composition feature not yet implemented - commented out
	// TODO: Re-enable when v5 composition is implemented
	// function toggleCompositionItem(compositionKey: string) {
	// 	expandedCompositions = {
	// 		...expandedCompositions,
	// 		[compositionKey]: !expandedCompositions[compositionKey]
	// 	};
	// }

	// function handleAddComposeFrom() {
	// 	showAddComposeFrom = !showAddComposeFrom;
	// 	if (showAddComposeFrom) {
	// 		showAddComposeInto = false;
	// 	}
	// }

	// function handleAddComposeInto() {
	// 	showAddComposeInto = !showAddComposeInto;
	// 	if (showAddComposeInto) {
	// 		showAddComposeFrom = false;
	// 	}
	// }

	// V5: Composition feature not yet implemented - commented out
	// TODO: Re-enable when v5 composition is implemented
	// function handleSelectComposeFromSlot(data: { id: string; name: string; metadata?: any }) {
	// 	console.log('[SLOT-COMPOSITION] Adding INTO composition:', data);
	// 	// ... implementation
	// }

	// function handleSelectComposeIntoSlot(data: { id: string; name: string; metadata?: any }) {
	// 	console.log('[SLOT-COMPOSITION] Adding FROM composition:', data);
	// 	// ... implementation
	// }

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
	<!-- V5 Required Fields Row -->
	<div class="slot-metadata mb-3 flex flex-wrap items-center gap-2 rounded bg-gray-50 p-2">
		<!-- Emoji (optional but helpful) -->
		<input
			type="text"
			class="slot-input emoji w-12 text-center"
			bind:value={slotEmoji}
			placeholder="📦"
			maxlength="2"
			onfocus={() => handleFocus('emoji', slotEmoji)}
			onblur={() => handleBlurIfChanged('emoji', slotEmoji)}
			title="Emoji"
		/>
		
		<!-- Name (REQUIRED) -->
		<input
			type="text"
			class="slot-input name flex-1"
			bind:value={slotName}
			placeholder="Slot name (required)"
			onfocus={() => handleFocus('name', slotName)}
			onblur={() => handleBlurIfChanged('name', slotName)}
			required
		/>
		
		<!-- Unit (optional) -->
		<input
			type="text"
			class="slot-input unit w-20"
			bind:value={slotUnit}
			placeholder="units"
			onfocus={() => handleFocus('unit', slotUnit)}
			onblur={() => handleBlurIfChanged('unit', slotUnit)}
			title="Unit of measurement"
		/>
		
		<!-- Need Type (REQUIRED) -->
		<select
			class="slot-input need-type w-40"
			bind:value={slotNeedTypeId}
			onchange={handleSlotUpdate}
			required
			title="Need type category"
		>
			<option value="need_type_general">General</option>
			<option value="need_type_food">Food</option>
			<option value="need_type_housing">Housing</option>
			<option value="need_type_healthcare">Healthcare</option>
			<option value="need_type_education">Education</option>
			<option value="need_type_transportation">Transportation</option>
			<option value="need_type_childcare">Childcare</option>
			<option value="need_type_other">Other</option>
		</select>
	</div>
	
	<!-- Description (optional, expandable) -->
	{#if slotDescription}
		<div class="slot-description mb-2 text-sm text-gray-600 italic">
			{slotDescription}
		</div>
	{/if}

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
		{#if slotUnit}
			<span class="slot-unit text-xs text-gray-500">({slotUnit})</span>
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

		<!-- V5: Composition feature not yet implemented -->
		<!-- 
		<button
			type="button"
			class="section-btn compositions-btn"
			onclick={toggleCompositions}
			title="View slot compositions"
		>
			🔄 Compositions ({totalCompositionsCount()})
		</button>
		-->

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
						<option value={option.value}>{option.label}</option>
					{/each}
				</select>
				<div class="mt-2 text-xs text-gray-500">
					This determines how often the slot repeats within the date range below
				</div>
			</div>

			<!-- Pattern Window (Start/End Dates) -->
			<div class="pattern-window-section mb-6">
				<h6 class="mb-3 text-sm font-medium text-gray-700">Pattern Window</h6>
				<div class="mb-2 text-xs text-gray-500">
					{#if slotRecurrence}
						Define when this recurrence pattern starts and ends
					{:else}
						Define the specific date(s) for this one-time availability
					{/if}
				</div>

				<div class="mb-4 grid grid-cols-1 gap-4 md:grid-cols-2">
					<div>
						<label for="slot-start-date" class="mb-1 block text-xs text-gray-500">
							{#if slotRecurrence}
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
							{#if slotRecurrence}
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
						placeholder={slotRecurrence
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

		<!-- V5 REMOVED: Total calculation display -->
		<!-- calculateTotalOccurrences() removed - occurrence tracking handled by allocation algorithm -->
		<!-- If needed, occurrence info will be computed by allocation algorithm based on availability_window -->
		
		</div>

			<!-- Time of Day Settings -->
			<div class="mb-4">
				<label class="inline-flex items-center">
					<input
						type="checkbox"
						bind:checked={legacyAllDay}
						class="mr-2"
						onchange={handleSlotUpdate}
					/>
					<span class="text-sm text-gray-600">All day</span>
				</label>
			</div>

			{#if !legacyAllDay}
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
								legacyStartTime = target.value; // Store as HH:MM format
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
								legacyEndTime = target.value; // Store as HH:MM format
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

	<!-- V5: Composition section removed (not yet implemented in v5) -->
	<!-- TODO: Re-implement slot composition using v5 patterns when needed -->
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
