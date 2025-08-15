<script lang="ts">
	import type { GroupedSlotMarkerData } from '$lib/components/Map.svelte';
	import { handleAddressClick } from '$lib/utils/mapUtils';
	import { globalState } from '$lib/global.svelte';

	interface Props {
		markerData: GroupedSlotMarkerData | null;
		onClose: () => void;
		onBackToSearch?: () => void; // New callback for going back to search
		isSearchMode?: boolean;
		searchQuery?: string;
		searchResults?: GroupedSlotMarkerData[];
		searchSortBy?: 'relevance' | 'distance';
		onSearchResultClick?: (marker: GroupedSlotMarkerData) => void;
		onSortChange?: () => void;
		currentLocation?: any;
		timeFilterBy?: 'any' | 'now' | 'next24h' | 'between';
		timeFilterStartDate?: string;
		timeFilterEndDate?: string;
		timeFilterStartTime?: string;
		timeFilterEndTime?: string;
		showTimeFilterDetails?: boolean;
		onTimeFilterChange?: (filter: 'any' | 'now' | 'next24h' | 'between') => void;
		onTimeFilterDetailsChange?: (details: {
			startDate: string;
			endDate: string;
			startTime: string;
			endTime: string;
		}) => void;
	}

	let {
		markerData,
		onClose,
		onBackToSearch,
		isSearchMode = false,
		searchQuery = '',
		searchResults = [],
		searchSortBy = 'relevance',
		onSearchResultClick,
		onSortChange,
		currentLocation,
		timeFilterBy = 'any',
		timeFilterStartDate = '',
		timeFilterEndDate = '',
		timeFilterStartTime = '',
		timeFilterEndTime = '',
		showTimeFilterDetails = false,
		onTimeFilterChange,
		onTimeFilterDetailsChange
	}: Props = $props();

	let searchInputElement: HTMLInputElement | undefined = $state();

	// Track if we came to marker details from search results
	let viewingMarkerFromSearch = $state(false);

	// Determine panel state
	let panelState = $derived(() => {
		if (markerData && viewingMarkerFromSearch) return 'marker-from-search'; // Marker details with back to search
		if (markerData) return 'marker'; // Showing marker details (direct click)
		if (globalState.isSearchMode) return 'search'; // Showing search results
		return 'expanded'; // Always expanded now (search input + time filter always visible)
	});

	// Handle search input
	function handleSearchInput(event: Event) {
		const target = event.target as HTMLInputElement;
		const value = target.value;

		globalState.updateSearchQuery(value);

		// Trigger search
		window.dispatchEvent(
			new CustomEvent('panel-search', {
				detail: { query: value }
			})
		);
	}

	// Handle search focus
	function handleSearchFocus() {
		if (globalState.searchQuery.trim()) {
			globalState.isSearchMode = true;
		}
	}

	// Clear search
	function clearSearch() {
		globalState.clearSearch();
		viewingMarkerFromSearch = false; // Reset navigation state
		searchInputElement?.focus();
		// Search mode will automatically close since globalState.clearSearch() sets isSearchMode = false
	}

	// Handle search result click with navigation state
	function handleSearchResultClick(marker: GroupedSlotMarkerData) {
		viewingMarkerFromSearch = true; // Mark that we came from search
		onSearchResultClick?.(marker); // Call parent handler
	}

	// Go back to search results from marker details
	function goBackToSearch() {
		viewingMarkerFromSearch = false;
		// Use specific back-to-search callback if available, otherwise fall back to onClose
		if (onBackToSearch) {
			onBackToSearch(); // This should clear markerData but preserve search
		} else {
			onClose?.(); // Fallback to regular close
		}
	}

	// Reset navigation state when marker changes or search is cleared
	$effect(() => {
		// If search is cleared while viewing marker from search, reset flag
		if (!globalState.isSearchMode && viewingMarkerFromSearch) {
			viewingMarkerFromSearch = false;
		}
	});

	// Get computed quantity for a specific slot (your share)
	function getSlotComputedQuantity(capacity: any, slotId: string): number {
		if (!capacity.computed_quantities || !Array.isArray(capacity.computed_quantities)) {
			return 0;
		}
		const slotQuantity = capacity.computed_quantities.find((cq: any) => cq.slot_id === slotId);
		return slotQuantity?.quantity || 0;
	}

	// Reactive visibility derived from markerData
	let isVisible = $derived(!!markerData);

	// Debug logging (better pattern than $effect)
	$inspect('MapSidePanel markerData:', markerData?.id);

	// Helper function to safely extract time from potentially malformed time strings
	function safeExtractTime(timeValue: string | null | undefined): string | undefined {
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

	// Helper function to format time without leading zeros (08:30 → 8:30)
	function formatTimeClean(timeStr: string): string {
		if (!timeStr) return timeStr;
		const [hours, minutes] = timeStr.split(':');
		const cleanHours = parseInt(hours).toString();
		return `${cleanHours}:${minutes}`;
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

	// Format slot time display - clean and comprehensive (matches Share.svelte)
	function formatSlotTimeDisplay(slot: any): string {
		const rawStartTime = safeExtractTime(slot.start_time);
		const rawEndTime = safeExtractTime(slot.end_time);
		const cleanStartTime = rawStartTime ? formatTimeClean(rawStartTime) : '';
		const cleanEndTime = rawEndTime ? formatTimeClean(rawEndTime) : '';

		// Get recurrence display
		const recurrenceDisplay =
			slot.recurrence && slot.recurrence !== 'Does not repeat' ? slot.recurrence : '';

		let timeStr = '';
		if (slot.all_day) {
			const startDate = slot.start_date ? new Date(slot.start_date) : null;
			const endDate = slot.end_date ? new Date(slot.end_date) : null;
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
		} else {
			const startDate = slot.start_date ? new Date(slot.start_date) : null;
			const endDate = slot.end_date ? new Date(slot.end_date) : null;

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
		}

		// Add recurrence if present
		return recurrenceDisplay ? `${timeStr} (${recurrenceDisplay})` : timeStr;
	}

	// Helper function to format slot location display
	function formatSlotLocationDisplay(slot: any): string {
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
		}

		return slot.location_type || 'No location';
	}

	// Helper function to check if slot has actual address components (not just coordinates)
	function hasAddressComponents(slot: any): boolean {
		return !!(
			slot.street_address ||
			slot.city ||
			slot.state_province ||
			slot.postal_code ||
			slot.country
		);
	}

	// Helper function to check if a slot is recurring (matches Share.svelte)
	function isSlotRecurring(slot: any): boolean {
		return slot.recurrence && slot.recurrence !== 'Does not repeat';
	}

	// Helper function to parse slot dates and times consistently
	function parseSlotDateTime(slot: any): {
		slotStart: Date | null;
		slotEnd: Date | null;
	} {
		const slotStart = slot.start_date ? new Date(slot.start_date) : null;
		let slotEnd = slot.end_date ? new Date(slot.end_date) : slotStart ? new Date(slotStart) : null;

		// For all-day events, don't add time components - work with dates only
		if (slot.all_day) {
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
			// For timed events, add time components using safe extraction
			if (slotStart && slot.start_time) {
				const safeStartTime = safeExtractTime(slot.start_time);
				if (safeStartTime) {
					const [hours, minutes] = safeStartTime.split(':');
					slotStart.setHours(parseInt(hours), parseInt(minutes), 0, 0);
				}
			}

			if (slotEnd && slot.end_time) {
				const safeEndTime = safeExtractTime(slot.end_time);
				if (safeEndTime) {
					const [hours, minutes] = safeEndTime.split(':');
					slotEnd.setHours(parseInt(hours), parseInt(minutes), 59, 999);
				}
			}

			// Handle missing end times for timed events (only when no end_date was specified)
			if (slotStart && !slot.end_date) {
				if (!slot.start_time && !slot.end_time) {
					// No specific times - treat as all-day
					slotEnd = new Date(slotStart);
					slotEnd.setHours(23, 59, 59, 999);
				} else if (slot.start_time && !slot.end_time) {
					// Has start time but no end time - assume 1 hour duration
					slotEnd = new Date(slotStart.getTime() + 60 * 60 * 1000);
				}
			}
		}

		return { slotStart, slotEnd };
	}

	// Helper function to check if a slot is in the past (matches Share.svelte)
	function isSlotInPast(slot: any): boolean {
		if (isSlotRecurring(slot)) return false;

		const now = new Date();
		const { slotStart, slotEnd } = parseSlotDateTime(slot);

		if (!slotStart) return false;

		// Use the effective end time (parseSlotDateTime handles all-day vs timed logic)
		const effectiveEndTime = slotEnd || slotStart;
		return effectiveEndTime < now;
	}

	// Categorize slots like in Share.svelte
	function categorizeSlots(slots: any[]): {
		recurring: any[];
		currentFuture: any[];
		past: any[];
	} {
		const recurring: any[] = [];
		const currentFuture: any[] = [];
		const past: any[] = [];

		slots.forEach((slot) => {
			if (isSlotRecurring(slot)) {
				recurring.push(slot);
			} else if (isSlotInPast(slot)) {
				past.push(slot);
			} else {
				currentFuture.push(slot);
			}
		});

		return { recurring, currentFuture, past };
	}

	// Calculate distance between two coordinates (Haversine formula)
	function calculateDistance(lat1: number, lon1: number, lat2: number, lon2: number): number {
		const R = 6371; // Earth's radius in kilometers
		const dLat = ((lat2 - lat1) * Math.PI) / 180;
		const dLon = ((lon2 - lon1) * Math.PI) / 180;
		const a =
			Math.sin(dLat / 2) * Math.sin(dLat / 2) +
			Math.cos((lat1 * Math.PI) / 180) *
				Math.cos((lat2 * Math.PI) / 180) *
				Math.sin(dLon / 2) *
				Math.sin(dLon / 2);
		const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
		return R * c; // Distance in kilometers
	}

	// Format distance for display
	function formatDistance(distance: number): string {
		if (distance < 1) {
			return `${(distance * 1000).toFixed(0)}m`;
		} else if (distance < 10) {
			return `${distance.toFixed(1)}km`;
		} else {
			return `${distance.toFixed(0)}km`;
		}
	}

	// Format time filter for display
	function formatTimeFilterDisplay(filter: string): string {
		switch (filter) {
			case 'any':
				return '';
			case 'now':
				return 'Available now';
			case 'next24h':
				return 'Next 24 hours';
			case 'between':
				return 'Custom time range';
			default:
				return '';
		}
	}
</script>

<!-- Fixed search input that never moves -->
<div class="search-panel expanded">
	<!-- Always visible search input in fixed position -->
	<div class="fixed-search-input">
		<div class="search-input-wrapper">
			<input
				bind:this={searchInputElement}
				type="text"
				class="search-input"
				placeholder="Search offers..."
				bind:value={globalState.searchQuery}
				oninput={handleSearchInput}
				onfocus={handleSearchFocus}
			/>
			{#if globalState.searchQuery}
				<button
					class="clear-btn"
					onclick={clearSearch}
					title="Clear search"
					aria-label="Clear search"
				>
					✕
				</button>
			{/if}
		</div>
	</div>

	<!-- Always visible time filter controls -->
	<div class="panel-content time-filter-only">
		<!-- Time Filter Controls -->
		<div class="content-section time-filter-compact">
			<div class="time-filter-header">
				<span class="time-filter-label">⏰</span>
				<select
					class="time-filter-select"
					bind:value={globalState.timeFilterBy}
					onchange={() => onTimeFilterChange?.(globalState.timeFilterBy)}
				>
					<option value="any">Any time</option>
					<option value="now">Now</option>
					<option value="next24h">Next 24 hours</option>
					<option value="between">Between</option>
				</select>
			</div>

			{#if globalState.showTimeFilterDetails}
				<div class="time-filter-details">
					<div class="time-range-row">
						<div class="time-input-group">
							<span class="time-input-label">From:</span>
							<input
								type="date"
								class="time-filter-date"
								bind:value={globalState.timeFilterStartDate}
								onchange={() =>
									onTimeFilterDetailsChange?.({
										startDate: globalState.timeFilterStartDate,
										endDate: globalState.timeFilterEndDate,
										startTime: globalState.timeFilterStartTime,
										endTime: globalState.timeFilterEndTime
									})}
							/>
							<input
								type="time"
								class="time-filter-time"
								bind:value={globalState.timeFilterStartTime}
								onchange={() =>
									onTimeFilterDetailsChange?.({
										startDate: globalState.timeFilterStartDate,
										endDate: globalState.timeFilterEndDate,
										startTime: globalState.timeFilterStartTime,
										endTime: globalState.timeFilterEndTime
									})}
							/>
						</div>
					</div>
					<div class="time-range-row">
						<div class="time-input-group">
							<span class="time-input-label">To:</span>
							<input
								type="date"
								class="time-filter-date"
								bind:value={globalState.timeFilterEndDate}
								onchange={() =>
									onTimeFilterDetailsChange?.({
										startDate: globalState.timeFilterStartDate,
										endDate: globalState.timeFilterEndDate,
										startTime: globalState.timeFilterStartTime,
										endTime: globalState.timeFilterEndTime
									})}
							/>
							<input
								type="time"
								class="time-filter-time"
								bind:value={globalState.timeFilterEndTime}
								onchange={() =>
									onTimeFilterDetailsChange?.({
										startDate: globalState.timeFilterStartDate,
										endDate: globalState.timeFilterEndDate,
										startTime: globalState.timeFilterStartTime,
										endTime: globalState.timeFilterEndTime
									})}
							/>
						</div>
					</div>
				</div>
			{/if}
		</div>
	</div>

	<!-- Panel content that appears/disappears below the time filter -->

	{#if markerData}
		<!-- Show marker details when markerData exists -->
		{@const { capacity, slots, lnglat, source, providerName } = markerData}
		{@const lngLatText = `${lnglat.lat.toFixed(6)}, ${lnglat.lng.toFixed(6)}`}
		{@const isGeocoded = source === 'geocoded'}
		{@const locationDisplay = formatSlotLocationDisplay(slots[0])}
		{@const categorizedSlots = categorizeSlots(slots)}
		{@const totalSlots = slots.length}

		<div class="panel-content">
			<!-- Header with conditional back button -->
			<div class="content-section marker-header">
				{#if viewingMarkerFromSearch}
					<!-- Back to search button -->
					<button
						class="back-btn"
						onclick={goBackToSearch}
						title="Back to search results"
						aria-label="Back to search results"
					>
						<span>←</span>
					</button>
				{/if}
				<div class="capacity-info">
					<h2 class="capacity-title">
						<span class="capacity-emoji">{capacity.emoji || '🏠'}</span>
						{capacity.name}
					</h2>
					<div class="provider-info">
						<span class="provider-label">👤 {providerName}</span>
						{#if totalSlots > 1}
							<span class="slot-count-badge">{totalSlots} slots</span>
						{/if}
					</div>
				</div>
				{#if !viewingMarkerFromSearch}
					<!-- Only show close button when not from search -->
					<button class="close-btn" onclick={onClose} title="Close panel" aria-label="Close panel">
						✕
					</button>
				{/if}
			</div>

			<!-- Location Info -->
			<div class="content-section location-section">
				<h3 class="section-title">📍 Location</h3>
				<div class="location-details">
					{#if hasAddressComponents(slots[0])}
						<!-- Show separate address and coordinates when we have actual address data -->
						<button
							class="location-address clickable-address"
							onclick={() => handleAddressClick(slots[0])}
							title="Click to open in maps or copy address"
						>
							📍 {locationDisplay}
						</button>
						<button
							class="location-coords clickable-coords"
							onclick={() => handleAddressClick(slots[0])}
							title="Click to open in maps or copy coordinates"
						>
							📐 {lngLatText}
						</button>
					{:else}
						<!-- Show single coordinates button when we only have coordinates -->
						<button
							class="location-coords clickable-coords single-location"
							onclick={() => handleAddressClick(slots[0])}
							title="Click to open in maps or copy coordinates"
						>
							📐 {lngLatText}
						</button>
					{/if}
				</div>
			</div>

			<!-- Slots Section -->
			<div class="content-section slots-section">
				<h3 class="section-title">🕒 Your Share of Available Slots</h3>

				{#if categorizedSlots.recurring.length > 0}
					<div class="slot-category">
						<h4 class="category-title">🔄 Recurring ({categorizedSlots.recurring.length})</h4>
						<div class="slot-list">
							{#each categorizedSlots.recurring as slot}
								{@const computedQuantity = getSlotComputedQuantity(capacity, slot.id)}
								<div class="slot-item">
									<div class="slot-main">
										<span class="slot-quantity">
											{Number.isInteger(computedQuantity)
												? computedQuantity
												: computedQuantity.toFixed(2)}
											{capacity.unit || ''}
										</span>
										<span class="slot-total">of {slot.quantity} total</span>
										<span class="slot-time">⏰ {formatSlotTimeDisplay(slot)}</span>
									</div>
									{#if slot.advance_notice_hours}
										<div class="slot-meta">
											<small class="notice-info">{slot.advance_notice_hours}h notice</small>
										</div>
									{/if}
								</div>
							{/each}
						</div>
					</div>
				{/if}

				{#if categorizedSlots.currentFuture.length > 0}
					<div class="slot-category">
						<h4 class="category-title">
							📅 Current & Upcoming ({categorizedSlots.currentFuture.length})
						</h4>
						<div class="slot-list">
							{#each categorizedSlots.currentFuture as slot}
								{@const computedQuantity = getSlotComputedQuantity(capacity, slot.id)}
								<div class="slot-item">
									<div class="slot-main">
										<span class="slot-quantity">
											{Number.isInteger(computedQuantity)
												? computedQuantity
												: computedQuantity.toFixed(2)}
											{capacity.unit || ''}
										</span>
										<span class="slot-total">of {slot.quantity} total</span>
										<span class="slot-time">⏰ {formatSlotTimeDisplay(slot)}</span>
									</div>
									{#if slot.advance_notice_hours}
										<div class="slot-meta">
											<small class="notice-info">{slot.advance_notice_hours}h notice</small>
										</div>
									{/if}
								</div>
							{/each}
						</div>
					</div>
				{/if}

				{#if categorizedSlots.past.length > 0}
					<div class="slot-category">
						<h4 class="category-title">📜 Past ({categorizedSlots.past.length})</h4>
						<div class="slot-list">
							{#each categorizedSlots.past as slot}
								{@const computedQuantity = getSlotComputedQuantity(capacity, slot.id)}
								<div class="slot-item past-slot">
									<div class="slot-main">
										<span class="slot-quantity">
											{Number.isInteger(computedQuantity)
												? computedQuantity
												: computedQuantity.toFixed(2)}
											{capacity.unit || ''}
										</span>
										<span class="slot-total">of {slot.quantity} total</span>
										<span class="slot-time">⏰ {formatSlotTimeDisplay(slot)}</span>
									</div>
								</div>
							{/each}
						</div>
					</div>
				{/if}
			</div>
		</div>
	{:else if globalState.isSearchMode}
		<!-- Search results content -->
		<div class="panel-content">
			<!-- Search Controls -->
			<div class="content-section">
				<div class="sort-controls">
					<span class="sort-label">Sort by:</span>
					<select class="sort-select" bind:value={searchSortBy} onchange={onSortChange}>
						<option value="relevance">Relevance</option>
						{#if currentLocation}
							<option value="distance">Distance</option>
						{/if}
					</select>
				</div>
			</div>

			<!-- Search Results -->
			<div class="content-section">
				{#if searchResults.length > 0}
					<div class="search-results">
						{#each searchResults as result (result.id)}
							{@const distance = currentLocation
								? calculateDistance(
										currentLocation.latitude,
										currentLocation.longitude,
										result.lnglat.lat,
										result.lnglat.lng
									)
								: null}

							<div
								class="search-result-item"
								onclick={() => handleSearchResultClick(result)}
								role="button"
								tabindex="0"
								onkeydown={(e) => {
									if (e.key === 'Enter' || e.key === ' ') {
										e.preventDefault();
										handleSearchResultClick(result);
									}
								}}
							>
								<div class="result-header">
									<div class="result-title">
										<span class="result-emoji">{result.capacity.emoji || '📦'}</span>
										<span class="result-name">{result.capacity.name}</span>
									</div>
									{#if distance !== null}
										<span class="result-distance">{formatDistance(distance)}</span>
									{/if}
								</div>

								<div class="result-details">
									<div class="result-provider">👤 {result.providerName}</div>
									{#if result.capacity.unit}
										<div class="result-unit">{result.capacity.unit}</div>
									{/if}
									<div class="result-slots">{result.slots.length} slots</div>
								</div>

								{#if result.capacity.description}
									<div class="result-description">
										{result.capacity.description.length > 100
											? result.capacity.description.substring(0, 100) + '...'
											: result.capacity.description}
									</div>
								{/if}
							</div>
						{/each}
					</div>
				{:else}
					<div class="no-results">
						<div class="no-results-icon">🔍</div>
						<h3>No results found</h3>
						<p>Try different search terms or check your spelling.</p>
					</div>
				{/if}
			</div>
		</div>
	{/if}
</div>

<style>
	/* Ultra-simple fixed search panel - CONSTRAINED to map height */
	.search-panel {
		position: absolute;
		top: 16px;
		left: 16px;
		z-index: 1000;
		width: 400px; /* Fixed width - never changes */
		display: flex;
		flex-direction: column;
	}

	/* Collapsed state: only search input, no map interference */
	.search-panel.collapsed {
		pointer-events: none; /* Don't block map touch events */
		/* No bottom constraint - only takes space needed for search input */
	}

	/* Expanded state: full panel with content, constrain to map height */
	.search-panel.expanded {
		bottom: 16px; /* Constrain bottom when content is visible */
		pointer-events: none; /* Don't block map events by default */
	}

	/* Fixed search input that never moves or changes size */
	.fixed-search-input {
		background: rgba(255, 255, 255, 0.98);
		border-radius: 8px;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);
		border: 1px solid #e5e7eb;
		backdrop-filter: blur(4px);
		flex-shrink: 0; /* Never shrink */
		height: 48px; /* Fixed height - never changes */
		pointer-events: auto; /* Always allow events on search input */
	}

	.search-input-wrapper {
		position: relative;
		display: flex;
		align-items: center;
		height: 100%; /* Fill the fixed height container */
	}

	.search-input {
		border: none;
		font-size: 14px;
		background: transparent;
		outline: none;
		color: #374151;
		width: 100%;
		padding: 12px 16px;
		padding-right: 40px;
		border-radius: 8px;
		min-width: 280px;
	}

	.search-input::placeholder {
		color: #9ca3af;
	}

	.search-input:focus {
		box-shadow: 0 0 0 2px rgba(59, 130, 246, 0.2);
	}

	.clear-btn {
		position: absolute;
		right: 8px;
		top: 50%;
		transform: translateY(-50%);
		background: none;
		border: none;
		cursor: pointer;
		color: #9ca3af;
		font-size: 14px;
		padding: 4px;
		border-radius: 4px;
		display: flex;
		align-items: center;
		justify-content: center;
		width: 24px;
		height: 24px;
		transition: all 0.2s ease;
	}

	.clear-btn:hover {
		background: #f3f4f6;
		color: #374151;
	}

	/* Unified panel content - IDENTICAL sizing for ALL states */
	.panel-content {
		background: white;
		border-radius: 8px;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
		border: 1px solid #e5e7eb;
		margin-top: 8px;
		padding: 16px;
		flex: 1; /* Take remaining space in flex container */
		min-height: 0; /* Allow flexbox to shrink */
		overflow-y: auto; /* Scroll when content exceeds container */
		animation: slideDown 0.3s ease-out;
		pointer-events: auto; /* Allow interaction with panel content */
	}

	/* Time filter only panel - ultra-compact styling */
	.panel-content.time-filter-only {
		flex: 0 0 auto; /* Don't grow/shrink, take only needed space */
		overflow: visible; /* No scrolling needed for just time filter */
		margin-bottom: 0; /* Remove bottom margin to connect with next panel */
		padding: 0; /* Remove padding */
		background: transparent; /* Remove white background */
		box-shadow: none; /* Remove shadow */
		border: none; /* Remove border */
		pointer-events: auto; /* Allow interaction with time filter controls */
	}

	/* Content sections - uniform spacing and borders */
	.content-section {
		border-bottom: 1px solid #f3f4f6;
		padding-bottom: 16px;
		margin-bottom: 16px;
	}

	.content-section:last-child {
		border-bottom: none;
		margin-bottom: 0;
		padding-bottom: 0;
	}

	/* Compact time filter section */
	.content-section.time-filter-compact {
		border-bottom: none; /* No border for minimal look */
		padding-bottom: 0; /* No padding */
		margin-bottom: 0; /* No margin */
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

	/* Time filter styling - now uses panel-section-header */

	.time-filter-header {
		display: inline-flex;
		align-items: center;
		gap: 4px;
		margin-bottom: 8px;
	}

	.time-filter-label {
		font-size: 14px;
		line-height: 1;
	}

	.time-filter-select {
		padding: 2px 6px; /* Much smaller padding */
		border: 1px solid #d1d5db;
		border-radius: 4px;
		font-size: 11px; /* Smaller font */
		background: white;
		color: #374151;
		cursor: pointer;
		min-width: 100px; /* Smaller width */
	}

	.time-filter-select:focus {
		outline: none;
		border-color: #3b82f6;
		box-shadow: 0 0 0 2px rgba(59, 130, 246, 0.2);
	}

	.time-filter-details {
		background: #f9fafb;
		border: 1px solid #e5e7eb;
		border-radius: 6px;
		padding: 12px;
		margin-top: 8px;
	}

	.time-range-row {
		margin-bottom: 8px;
	}

	.time-range-row:last-child {
		margin-bottom: 0;
	}

	.time-input-group {
		display: flex;
		align-items: center;
		gap: 8px;
	}

	.time-input-label {
		font-size: 12px;
		font-weight: 500;
		color: #6b7280;
		min-width: 35px;
	}

	.time-filter-date,
	.time-filter-time {
		padding: 4px 6px;
		border: 1px solid #d1d5db;
		border-radius: 4px;
		font-size: 12px;
		background: white;
		color: #374151;
		cursor: pointer;
	}

	.time-filter-date:focus,
	.time-filter-time:focus {
		outline: none;
		border-color: #3b82f6;
		box-shadow: 0 0 0 1px rgba(59, 130, 246, 0.2);
	}

	.time-filter-date {
		min-width: 130px;
	}

	.time-filter-time {
		min-width: 80px;
	}

	/* Search results styling - now uses panel-section-header */

	.search-meta {
		display: flex;
		align-items: center;
		gap: 12px;
		flex-wrap: wrap;
	}

	.search-query {
		color: #3b82f6;
		font-style: italic;
		font-size: 14px;
	}

	.result-count {
		color: #6b7280;
		font-size: 12px;
		background: #f3f4f6;
		padding: 2px 8px;
		border-radius: 12px;
	}

	.time-filter-badge {
		color: #7c3aed;
		font-size: 12px;
		background: #f3e8ff;
		padding: 2px 8px;
		border-radius: 12px;
		font-weight: 500;
	}

	/* Search controls styling - now uses panel-section-header */

	.sort-controls {
		display: flex;
		align-items: center;
		gap: 8px;
	}

	.sort-label {
		font-size: 13px;
		color: #6b7280;
		font-weight: 500;
	}

	.sort-select {
		padding: 4px 8px;
		border: 1px solid #d1d5db;
		border-radius: 4px;
		font-size: 13px;
		background: white;
		color: #374151;
		cursor: pointer;
	}

	/* Results container - now uses panel-scrollable-content */

	.search-results {
		display: flex;
		flex-direction: column;
		gap: 12px;
	}

	.search-result-item {
		background: white;
		border: 1px solid #e5e7eb;
		border-radius: 8px;
		padding: 16px;
		cursor: pointer;
		transition: all 0.2s ease;
	}

	.search-result-item:hover {
		background: #f9fafb;
		border-color: #3b82f6;
		transform: translateY(-1px);
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
	}

	.result-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 8px;
	}

	.result-title {
		display: flex;
		align-items: center;
		gap: 8px;
		flex: 1;
		min-width: 0;
	}

	.result-emoji {
		font-size: 16px;
		flex-shrink: 0;
	}

	.result-name {
		font-weight: 600;
		color: #111827;
		font-size: 14px;
		overflow: hidden;
		text-overflow: ellipsis;
		white-space: nowrap;
	}

	.result-distance {
		color: #6b7280;
		font-size: 12px;
		font-weight: 500;
		background: #f3f4f6;
		padding: 2px 6px;
		border-radius: 4px;
		flex-shrink: 0;
	}

	.result-details {
		display: flex;
		align-items: center;
		gap: 12px;
		margin-bottom: 8px;
		font-size: 12px;
		color: #6b7280;
		flex-wrap: wrap;
	}

	.result-provider {
		font-style: italic;
	}

	.result-slots {
		background: #dbeafe;
		color: #1d4ed8;
		padding: 2px 6px;
		border-radius: 4px;
		font-weight: 500;
	}

	.result-unit {
		background: #f3e8ff;
		color: #7c3aed;
		padding: 2px 6px;
		border-radius: 4px;
		font-weight: 500;
	}

	.result-description {
		color: #4b5563;
		font-size: 13px;
		line-height: 1.4;
		margin-top: 4px;
	}

	.no-results {
		text-align: center;
		padding: 48px 24px;
		color: #6b7280;
	}

	.no-results-icon {
		font-size: 48px;
		margin-bottom: 16px;
		opacity: 0.5;
	}

	.no-results h3 {
		margin: 0 0 8px 0;
		font-size: 16px;
		font-weight: 600;
		color: #374151;
	}

	.no-results p {
		margin: 0;
		font-size: 14px;
	}

	/* Close section styles removed since close button was removed */

	/* Marker header styling */
	.marker-header {
		display: flex;
		justify-content: space-between;
		align-items: flex-start;
	}

	.capacity-info {
		flex: 1;
	}

	.capacity-title {
		font-size: 18px;
		font-weight: 600;
		color: #111827;
		margin: 0 0 8px 0;
		display: flex;
		align-items: center;
		gap: 8px;
	}

	.capacity-emoji {
		font-size: 24px;
	}

	.provider-info {
		display: flex;
		align-items: center;
		gap: 12px;
		flex-wrap: wrap;
	}

	.provider-label {
		color: #6b7280;
		font-size: 14px;
		font-style: italic;
	}

	.slot-count-badge {
		background: #ef4444;
		color: white;
		padding: 2px 8px;
		border-radius: 12px;
		font-size: 12px;
		font-weight: 500;
	}

	.close-btn {
		background: none;
		border: none;
		cursor: pointer;
		color: #6b7280;
		padding: 4px;
		border-radius: 4px;
		transition: all 0.2s ease;
		font-size: 18px;
	}

	.close-btn:hover {
		background: #f3f4f6;
		color: #374151;
	}

	.back-btn {
		background: none;
		border: none;
		cursor: pointer;
		color: #6b7280;
		padding: 6px 8px;
		border-radius: 4px;
		transition: all 0.2s ease;
		font-size: 13px;
		font-weight: 500;
		margin-bottom: 8px;
		display: flex;
		align-items: center;
		gap: 6px;
		text-align: left;
	}

	.back-btn:hover {
		background: #f3f4f6;
		color: #374151;
	}

	/* Location and slot styling - now uses content-section */

	.section-title {
		font-size: 16px;
		font-weight: 600;
		color: #374151;
		margin: 0 0 12px 0;
		display: flex;
		align-items: center;
		gap: 6px;
	}

	.location-details {
		background: #f9fafb;
		padding: 12px;
		border-radius: 8px;
		border: 1px solid #e5e7eb;
	}

	.location-address,
	.location-coords {
		font-size: 14px;
		color: #111827;
		margin-bottom: 4px;
	}

	.location-coords {
		font-size: 12px;
		color: #6b7280;
		font-family: monospace;
	}

	/* Clickable address and coordinates styling */
	.clickable-address,
	.clickable-coords {
		background: none;
		border: 1px solid transparent;
		color: inherit;
		font: inherit;
		cursor: pointer;
		padding: 8px 12px;
		border-radius: 6px;
		transition: all 0.2s ease;
		text-align: left;
		width: 100%;
		margin-bottom: 4px;
		display: block;
	}

	.clickable-address:hover,
	.clickable-coords:hover {
		background: #f0f9ff;
		border-color: #3b82f6;
		color: #3b82f6;
		transform: translateY(-1px);
		box-shadow: 0 2px 4px rgba(59, 130, 246, 0.1);
	}

	.clickable-address:active,
	.clickable-coords:active {
		transform: translateY(0);
		background: #e0f2fe;
		box-shadow: 0 1px 2px rgba(59, 130, 246, 0.1);
	}

	.clickable-coords {
		margin-bottom: 0;
	}

	.single-location {
		margin-bottom: 0 !important;
		text-align: center;
		font-weight: 500;
	}

	.slot-category {
		margin-bottom: 20px;
	}

	.category-title {
		font-size: 14px;
		font-weight: 600;
		color: #374151;
		margin: 0 0 8px 0;
		padding: 8px 0;
		border-bottom: 1px solid #f3f4f6;
	}

	.slot-list {
		display: flex;
		flex-direction: column;
		gap: 8px;
	}

	.slot-item {
		background: #f9fafb;
		border: 1px solid #e5e7eb;
		border-radius: 8px;
		padding: 12px;
		transition: all 0.2s ease;
	}

	.slot-item:hover {
		background: #f3f4f6;
		border-color: #d1d5db;
	}

	.slot-item.past-slot {
		opacity: 0.7;
		background: #f5f5f5;
	}

	.slot-main {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 4px;
	}

	.slot-quantity {
		font-weight: 600;
		color: #111827;
		font-size: 14px;
		font-family: 'SF Mono', 'Monaco', 'Inconsolata', 'Roboto Mono', monospace;
	}

	.slot-total {
		color: #6b7280;
		font-size: 12px;
		font-weight: 500;
		margin-left: 8px;
	}

	.slot-time {
		color: #6b7280;
		font-size: 13px;
	}

	.slot-meta {
		margin-top: 4px;
	}

	.notice-info {
		color: #9ca3af;
		font-size: 11px;
	}

	/* Responsive design - constrain to map width */
	@media (max-width: 768px) {
		.search-panel {
			width: calc(100% - 32px); /* Full map width minus margins */
			max-width: none;
		}

		.search-input {
			min-width: 240px;
		}

		.time-filter-header {
			flex-direction: column;
			align-items: flex-start;
			gap: 4px;
		}

		.time-input-group {
			flex-wrap: wrap;
			gap: 4px;
		}

		.time-filter-date,
		.time-filter-time {
			min-width: auto;
			flex: 1;
		}
	}

	@media (max-width: 480px) {
		.search-panel {
			left: 8px;
			top: 8px;
			width: calc(100% - 16px); /* Constrain to map width */
		}

		.search-input {
			min-width: 200px;
		}
	}
</style>
