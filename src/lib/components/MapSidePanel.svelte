<script lang="ts">
	import type { GroupedSlotMarkerData, ClusterMarkerData } from '$lib/components/Map.svelte';
	import { handleAddressClick } from '$lib/utils/mapUtils';
	import { globalState } from '$lib/global.svelte';
	import { mutualRecognition } from '$lib/state/core.svelte';

	interface Props {
		markerData: GroupedSlotMarkerData | ClusterMarkerData | null;
		onClose: () => void;
		onBackToSearch?: () => void; // New callback for going back to search
		isSearchMode?: boolean;
		searchQuery?: string;
		searchResults?: GroupedSlotMarkerData[];
		searchSortBy?: 'relevance' | 'distance';
		onSearchResultClick?: (marker: GroupedSlotMarkerData) => void;
		onSortChange?: () => void;
		currentLocation?: any;
		// Cluster view props
		isClusterViewMode?: boolean;
		clusterViewResults?: GroupedSlotMarkerData[];
		onClusterResultClick?: (marker: GroupedSlotMarkerData) => void;
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
		isClusterViewMode = false,
		clusterViewResults = [],
		onClusterResultClick
	}: Props = $props();

	let searchInputElement: HTMLInputElement | undefined = $state();

	// Track if we came to marker details from search results
	let viewingMarkerFromSearch = $state(false);

	// Track fullscreen state for responsive panel sizing
	let isFullscreen = $state(false);

	// Handle fullscreen changes
	const handleFullscreenChange = () => {
		isFullscreen = !!document.fullscreenElement;
		console.log('[MapSidePanel] Fullscreen changed:', isFullscreen);
	};

	// Prevent scroll events from bubbling to parent page
	function handlePanelScroll(event: Event) {
		// Stop propagation to prevent page scrolling
		event.stopPropagation();
	}

	// Handle wheel events to prevent page scroll when panel is scrolling
	function handlePanelWheel(event: WheelEvent) {
		const target = event.currentTarget as HTMLElement;
		const { scrollTop, scrollHeight, clientHeight } = target;

		// If scrolling up and already at top, prevent default to avoid page scroll
		if (event.deltaY < 0 && scrollTop === 0) {
			event.preventDefault();
			return;
		}

		// If scrolling down and already at bottom, prevent default to avoid page scroll
		if (event.deltaY > 0 && scrollTop + clientHeight >= scrollHeight) {
			event.preventDefault();
			return;
		}

		// Otherwise, allow normal scrolling but stop propagation
		event.stopPropagation();
	}

	// Determine panel state - using simple derived instead of derived.by
	let panelState = $derived(() => {
		let state: string;

		if (markerData && viewingMarkerFromSearch) {
			console.log('[Panel State] -> marker-from-search');
			state = 'marker-from-search'; // Marker details with back to search
		} else if (markerData && !isClusterViewMode) {
			console.log('[Panel State] -> marker (individual)');
			state = 'marker'; // Showing individual marker details
		} else if (isClusterViewMode) {
			console.log('[Panel State] -> cluster');
			state = 'cluster'; // Showing cluster contents
		} else if (globalState.isSearchMode) {
			console.log('[Panel State] -> search');
			state = 'search'; // Showing search results
		} else {
			console.log('[Panel State] -> expanded');
			state = 'expanded'; // Always expanded now (search input + time filter always visible)
		}

		console.log('[Panel State]', {
			state,
			markerData: markerData ? `${markerData.id}` : 'null',
			markerType: markerData ? ('capacity' in markerData ? 'individual' : 'cluster') : 'none',
			isClusterViewMode,
			viewingMarkerFromSearch,
			isSearchMode: globalState.isSearchMode
		});
		console.log('[Panel State] Final state:', state);
		return state;
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

	// Time filter handlers
	function handleTimeFilterChange() {
		globalState.updateTimeFilter(globalState.timeFilterBy);
		// Trigger search to re-filter with new time filter
		window.dispatchEvent(
			new CustomEvent('panel-search', {
				detail: { query: globalState.searchQuery }
			})
		);
	}

	function handleTimeFilterDetailsChange() {
		globalState.updateTimeFilterDetails({
			startDate: globalState.timeFilterStartDate,
			endDate: globalState.timeFilterEndDate,
			startTime: globalState.timeFilterStartTime,
			endTime: globalState.timeFilterEndTime
		});
		// Trigger search to re-filter with new time filter details
		window.dispatchEvent(
			new CustomEvent('panel-search', {
				detail: { query: globalState.searchQuery }
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
		// If cluster view mode is disabled, reset viewing from search flag
		if (!isClusterViewMode && viewingMarkerFromSearch) {
			viewingMarkerFromSearch = false;
		}
	});

	// Get allocated quantity for a specific slot (your share from efficient algorithm)
	function getSlotAllocatedQuantity(capacity: any, slotId: string): number {
		// Use the new efficient allocation data structure
		const slot = capacity.capacity_slots?.find((s: any) => s.id === slotId);
		return slot?.allocated_quantity || 0;
	}

	// Calculate mutual recognition share for a slot: provider total quantity * user mutual-rec share
	function getSlotMutualRecognitionShare(capacity: any, slotId: string): number {
		const slot = capacity.capacity_slots?.find((s: any) => s.id === slotId);
		if (!slot) return 0;

		const providerId = capacity.provider_id;
		if (!providerId) return 0;

		// Get the user's mutual recognition share with this provider
		const userMutualRecShare = $mutualRecognition[providerId] || 0;

		// Calculate: slot total quantity * mutual recognition share
		const totalQuantity = slot.quantity || 0;
		const mutualRecShare = totalQuantity * userMutualRecShare;

		return mutualRecShare;
	}

	// Reactive visibility derived from markerData
	let isVisible = $derived(!!markerData);

	// Helper to check if marker is a cluster
	function isClusterMarker(
		marker: GroupedSlotMarkerData | ClusterMarkerData | null
	): marker is ClusterMarkerData {
		return marker !== null && 'markers' in marker && 'totalCapacities' in marker;
	}

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

<svelte:document onfullscreenchange={handleFullscreenChange} />

<!-- Fixed search input that never moves -->
<div
	class="search-panel expanded"
	class:fullscreen={isFullscreen}
	onscroll={handlePanelScroll}
	onwheel={handlePanelWheel}
>
	<!-- Always visible search input with integrated time filter -->
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
			<div class="time-filter-inline">
				<span class="time-filter-icon">🕒</span>
				<select
					class="time-filter-select"
					bind:value={globalState.timeFilterBy}
					onchange={handleTimeFilterChange}
				>
					<option value="any">Any Time</option>
					<option value="now">Now</option>
					<option value="next24h">Next 24h</option>
					<option value="between">Custom</option>
				</select>
			</div>
		</div>

		<!-- Custom time range details (shown below search bar) -->
		{#if globalState.timeFilterBy === 'between'}
			<div class="time-filter-details">
				<div class="time-row">
					<input
						type="date"
						class="time-input"
						bind:value={globalState.timeFilterStartDate}
						onchange={handleTimeFilterDetailsChange}
						placeholder="Start date"
					/>
					<input
						type="time"
						class="time-input"
						bind:value={globalState.timeFilterStartTime}
						onchange={handleTimeFilterDetailsChange}
						placeholder="Start time"
					/>
				</div>
				<div class="time-row">
					<input
						type="date"
						class="time-input"
						bind:value={globalState.timeFilterEndDate}
						onchange={handleTimeFilterDetailsChange}
						placeholder="End date"
					/>
					<input
						type="time"
						class="time-input"
						bind:value={globalState.timeFilterEndTime}
						onchange={handleTimeFilterDetailsChange}
						placeholder="End time"
					/>
				</div>
			</div>
		{/if}
	</div>

	<!-- Panel content that appears/disappears below the search/time filter -->
	<!-- Debug: Current panel state = {panelState} -->

	{#if isClusterViewMode}
		<!-- Cluster view content (prioritize over cluster marker details) -->
		<!-- Debug: clusterViewResults.length = {clusterViewResults.length} -->
		<div class="panel-content" onscroll={handlePanelScroll} onwheel={handlePanelWheel}>
			<!-- Cluster Header with close button -->
			<div class="content-section marker-header">
				<div class="cluster-view-header">
					<h3 class="cluster-view-title">
						<span class="cluster-view-emoji">🎁</span>
						{clusterViewResults.length} Capacities at this Location
					</h3>
				</div>
				<button class="close-btn" onclick={onClose} title="Close panel" aria-label="Close panel">
					✕
				</button>
			</div>

			<!-- Cluster Results -->
			<div class="content-section">
				{#if clusterViewResults.length > 0}
					<div class="cluster-results">
						{#each clusterViewResults as result (result.id)}
							{@const distance = currentLocation
								? calculateDistance(
										currentLocation.latitude,
										currentLocation.longitude,
										result.lnglat.lat,
										result.lnglat.lng
									)
								: null}

							<div
								class="cluster-result-item"
								onclick={() => {
									console.log('[Cluster View] Capacity clicked:', result.capacity.name);
									onClusterResultClick?.(result);
								}}
								role="button"
								tabindex="0"
								onkeydown={(e) => {
									if (e.key === 'Enter' || e.key === ' ') {
										e.preventDefault();
										onClusterResultClick?.(result);
									}
								}}
							>
								<div class="result-header">
									<div class="result-title">
										<span class="result-emoji">{result.capacity.emoji || '🎁'}</span>
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
					<p class="no-results">No capacities found in this cluster.</p>
				{/if}
			</div>
		</div>
	{:else if markerData}
		{#if isClusterMarker(markerData)}
			<!-- Show cluster details -->
			{@const { lnglat, markers, totalSlots, totalCapacities } = markerData}
			{@const lngLatText = `${lnglat.lat.toFixed(6)}, ${lnglat.lng.toFixed(6)}`}

			<div class="panel-content">
				<!-- Cluster Header -->
				<div class="content-section marker-header">
					<div class="capacity-info">
						<h2 class="capacity-title">
							<span class="capacity-emoji">📍</span>
							{totalCapacities} Capacities
						</h2>
						<div class="provider-info">
							<span class="provider-label">Clustered at this location</span>
						</div>
					</div>
					<button class="close-btn" onclick={onClose} title="Close panel" aria-label="Close panel">
						✕
					</button>
				</div>

				<!-- Cluster Location -->
				<div class="content-section location-section">
					<h3 class="section-title"><span style="font-size:8px;">📍</span> Cluster Center</h3>
					<div class="location-details">
						<div class="location-coords single-location" style="font-size: 8px; line-height: 1;">
							<span style="font-size:8px;">📐</span>
							<span style="font-size: 8px; font-family: monospace;">{lngLatText}</span>
						</div>
						<p style="font-size: 10px; color: #6b7280; margin-top: 6px;">
							Click to zoom in and see individual capacities
						</p>
					</div>
				</div>

				<!-- Cluster Contents -->
				<div class="content-section slots-section">
					<h3 class="section-title">🎁 {totalCapacities} Capacities</h3>
					<div class="cluster-contents">
						{#each markers as marker}
							{@const { capacity, slots, providerName } = marker}
							<div class="cluster-item">
								<div class="cluster-item-header">
									<span class="cluster-item-emoji">{capacity.emoji || '🎁'}</span>
									<span class="cluster-item-name">{capacity.name}</span>
									{#if capacity.unit}
										<span class="cluster-item-unit">{capacity.unit}</span>
									{/if}
									<span class="cluster-item-slots">{slots.length} slots</span>
								</div>
								<div class="cluster-item-provider">👤 {providerName}</div>
								{#if capacity.description}
									<div class="cluster-item-description">
										{capacity.description.length > 60
											? capacity.description.substring(0, 60) + '...'
											: capacity.description}
									</div>
								{/if}
							</div>
						{/each}
					</div>
				</div>
			</div>
		{:else}
			<!-- Show individual marker details -->
			{@const { capacity, slots, lnglat, source, providerName } = markerData}
			{@const lngLatText = `${lnglat.lat.toFixed(6)}, ${lnglat.lng.toFixed(6)}`}
			{@const isGeocoded = source === 'geocoded'}
			{@const locationDisplay = formatSlotLocationDisplay(slots[0])}
			{@const categorizedSlots = categorizeSlots(slots)}
			{@const totalSlots = slots.length}

			<div class="panel-content" onscroll={handlePanelScroll} onwheel={handlePanelWheel}>
				<!-- Header with conditional back button and always-visible close button -->
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
							{#if capacity.unit}
								<span class="capacity-unit-badge">{capacity.unit}</span>
							{/if}
							{#if totalSlots > 1}
								<span class="slot-count-badge">{totalSlots} slots</span>
							{/if}
						</div>
					</div>
					<!-- Always show close button -->
					<button class="close-btn" onclick={onClose} title="Close panel" aria-label="Close panel">
						✕
					</button>
				</div>

				<!-- Location Info -->
				<div class="content-section location-section">
					<h3 class="section-title"><span style="font-size:8px;">📍</span> Location</h3>
					<div class="location-details">
						{#if hasAddressComponents(slots[0])}
							<!-- Show separate address and coordinates when we have actual address data -->
							<button
								class="location-address clickable-address"
								onclick={() => handleAddressClick(slots[0])}
								title="Click to open in maps or copy address"
								style="font-size: 8px; line-height: 1;"
							>
								<span style="font-size:8px;">📍</span>
								<span style="font-size: px;">{locationDisplay}</span>
							</button>
							<button
								class="location-coords clickable-coords"
								onclick={() => handleAddressClick(slots[0])}
								title="Click to open in maps or copy coordinates"
								style="font-size: px; line-height: 1;"
							>
								<span style="font-size:8px;">📐</span>
								<span style="font-size: 8px; font-family: monospace;">{lngLatText}</span>
							</button>
						{:else}
							<!-- Show single coordinates button when we only have coordinates -->
							<button
								class="location-coords clickable-coords single-location"
								onclick={() => handleAddressClick(slots[0])}
								title="Click to open in maps or copy coordinates"
								style="font-size: 8px; line-height: 1;"
							>
								<span style="font-size:8px;">📐</span>
								<span style="font-size: 8px; font-family: monospace;">{lngLatText}</span>
							</button>
						{/if}
					</div>
				</div>

				<!-- Slots Section -->
				<div class="content-section slots-section">
					<h3 class="section-title">🕒 Available Slots</h3>

					{#if categorizedSlots.recurring.length > 0}
						<div class="slot-category">
							<h4 class="category-title">🔄 Recurring ({categorizedSlots.recurring.length})</h4>
							<div class="slot-list">
								{#each categorizedSlots.recurring as slot}
									{@const allocatedQuantity = getSlotAllocatedQuantity(capacity, slot.id)}
									{@const mutualRecShare = getSlotMutualRecognitionShare(capacity, slot.id)}
									<div class="slot-item">
										<div class="slot-main">
											{#if allocatedQuantity > 0}
												<span class="slot-quantity allocated">
													{Number.isInteger(allocatedQuantity)
														? allocatedQuantity
														: allocatedQuantity.toFixed(2)}
													{capacity.unit || ''} allocated
												</span>
												<span class="slot-total">of {slot.quantity} total</span>
											{:else}
												<span class="slot-quantity available">
													{slot.quantity}
													{capacity.unit || ''} available
												</span>
												<span class="slot-total">(express desire to get some!)</span>
											{/if}
											{#if mutualRecShare > 0}
												<span class="slot-share">
													Share: {Number.isInteger(mutualRecShare)
														? mutualRecShare
														: mutualRecShare.toFixed(2)}
													{capacity.unit || ''}
												</span>
											{/if}
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
									{@const allocatedQuantity = getSlotAllocatedQuantity(capacity, slot.id)}
									{@const mutualRecShare = getSlotMutualRecognitionShare(capacity, slot.id)}
									<div class="slot-item">
										<div class="slot-main">
											{#if allocatedQuantity > 0}
												<span class="slot-quantity allocated">
													{Number.isInteger(allocatedQuantity)
														? allocatedQuantity
														: allocatedQuantity.toFixed(2)}
													{capacity.unit || ''} allocated
												</span>
												<span class="slot-total">of {slot.quantity} total</span>
											{:else}
												<span class="slot-quantity available">
													{slot.quantity}
													{capacity.unit || ''} available
												</span>
												<span class="slot-total">(express desire to get some!)</span>
											{/if}
											{#if mutualRecShare > 0}
												<span class="slot-share">
													Share: {Number.isInteger(mutualRecShare)
														? mutualRecShare
														: mutualRecShare.toFixed(2)}
													{capacity.unit || ''}
												</span>
											{/if}
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
									{@const allocatedQuantity = getSlotAllocatedQuantity(capacity, slot.id)}
									{@const mutualRecShare = getSlotMutualRecognitionShare(capacity, slot.id)}
									<div class="slot-item past-slot">
										<div class="slot-main">
											{#if allocatedQuantity > 0}
												<span class="slot-quantity allocated">
													{Number.isInteger(allocatedQuantity)
														? allocatedQuantity
														: allocatedQuantity.toFixed(2)}
													{capacity.unit || ''} allocated
												</span>
												<span class="slot-total">of {slot.quantity} total</span>
											{:else}
												<span class="slot-quantity available">
													{slot.quantity}
													{capacity.unit || ''} was available
												</span>
												<span class="slot-total">(past slot)</span>
											{/if}
											{#if mutualRecShare > 0}
												<span class="slot-share">
													Share: {Number.isInteger(mutualRecShare)
														? mutualRecShare
														: mutualRecShare.toFixed(2)}
													{capacity.unit || ''}
												</span>
											{/if}
											<span class="slot-time">⏰ {formatSlotTimeDisplay(slot)}</span>
										</div>
									</div>
								{/each}
							</div>
						</div>
					{/if}
				</div>
			</div>
		{/if}
	{:else if globalState.isSearchMode}
		<!-- Search results content -->
		<div class="panel-content" onscroll={handlePanelScroll} onwheel={handlePanelWheel}>
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
										<span class="result-emoji">{result.capacity.emoji || '🎁'}</span>
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
	/* Panel as CustomControl - responsive to fullscreen */
	.search-panel {
		width: 320px;
		max-width: calc(100vw - 32px);
		/* Direct constraint matching map dimensions */
		max-height: min(380px, calc(50vh - 20px)); /* Match map: min(400px, 50vh) minus margin */
		display: flex;
		flex-direction: column;
		background: rgba(255, 255, 255, 0.95);
		border-radius: 8px;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
		backdrop-filter: blur(8px);
		border: 1px solid rgba(255, 255, 255, 0.2);
		overscroll-behavior: contain;
		touch-action: auto;
	}

	/* Fullscreen mode - use full viewport height */
	.search-panel.fullscreen {
		max-height: calc(100vh - 40px); /* Full viewport minus margin */
	}

	/* Fixed search input that never moves or changes size */
	.fixed-search-input {
		background: rgba(255, 255, 255, 0.98);
		border-radius: 8px;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);
		border: 1px solid #e5e7eb;
		backdrop-filter: blur(4px);
		flex-shrink: 0; /* Never shrink */
		min-height: 36px; /* Changed to min-height to accommodate time filter details */
		pointer-events: auto; /* Always allow events on search input */
	}

	.search-input-wrapper {
		position: relative;
		display: flex;
		align-items: center;
		min-height: 36px; /* Changed to min-height */
		gap: 8px; /* Add gap between search input and time filter */
	}

	.search-input {
		border: none;
		font-size: 10px;
		background: transparent;
		outline: none;
		color: #374151;
		flex: 1; /* Take remaining space */
		padding: 8px 12px;
		padding-right: 40px;
		border-radius: 8px;
		min-width: 120px; /* Reduced to leave space for time filter */
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
		font-size: 10px;
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

	/* Inline time filter - improved styling */
	.time-filter-inline {
		display: flex;
		align-items: center;
		gap: 6px;
		flex-shrink: 0;
		background: rgba(255, 255, 255, 0.8);
		border: 1px solid rgba(0, 0, 0, 0.1);
		border-radius: 6px;
		padding: 4px 8px;
		margin-left: 8px; /* Add margin from search input */
		transition: all 0.2s ease;
		backdrop-filter: blur(4px);
	}

	.time-filter-inline:hover {
		background: rgba(255, 255, 255, 0.95);
		border-color: rgba(59, 130, 246, 0.3);
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
	}

	.time-filter-icon {
		font-size: 13px;
		color: #6b7280;
	}

	.time-filter-select {
		border: none;
		background: transparent;
		font-size: 11px;
		color: #374151;
		outline: none;
		cursor: pointer;
		min-width: 70px;
		padding: 2px 4px;
		font-weight: 500;
		border-radius: 4px;
		transition: background-color 0.2s ease;
	}

	.time-filter-select:hover {
		background: rgba(59, 130, 246, 0.1);
	}

	.time-filter-select:focus {
		box-shadow: 0 0 0 2px rgba(59, 130, 246, 0.3);
		background: rgba(59, 130, 246, 0.05);
	}

	/* Time filter details (shown below search bar) - improved styling */
	.time-filter-details {
		margin-top: 8px;
		padding: 12px;
		background: rgba(255, 255, 255, 0.9);
		border: 1px solid rgba(0, 0, 0, 0.1);
		border-radius: 8px;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
		backdrop-filter: blur(8px);
	}

	.time-row {
		display: flex;
		gap: 8px;
		margin-bottom: 4px;
	}

	.time-row:last-child {
		margin-bottom: 0;
	}

	.time-input {
		flex: 1;
		border: 1px solid rgba(0, 0, 0, 0.15);
		border-radius: 6px;
		padding: 6px 8px;
		font-size: 11px;
		background: rgba(255, 255, 255, 0.95);
		outline: none;
		transition: all 0.2s ease;
	}

	.time-input:hover {
		border-color: rgba(59, 130, 246, 0.4);
		background: white;
	}

	.time-input:focus {
		border-color: rgba(59, 130, 246, 0.6);
		box-shadow: 0 0 0 2px rgba(59, 130, 246, 0.2);
		background: white;
	}

	/* Unified panel content - IDENTICAL sizing for ALL states */
	.panel-content {
		background: white;
		border-radius: 8px;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
		border: 1px solid #e5e7eb;
		margin-top: 4px;
		padding: 10px;
		flex: 1; /* Take remaining space in flex container */
		min-height: 0; /* Allow flexbox to shrink */
		overflow-y: auto; /* Scroll when content exceeds container */
		animation: slideDown 0.3s ease-out;
		/* Ensure scrollable content is interactive inside CustomControl */
		pointer-events: auto;
		/* Prevent scroll events from bubbling to parent page */
		overscroll-behavior: contain;
		/* Improve mobile scrolling */
		-webkit-overflow-scrolling: touch;
	}

	/* Content sections - uniform spacing and borders */
	.content-section {
		border-bottom: 1px solid #f3f4f6;
		padding-bottom: 8px;
		margin-bottom: 6px;
	}

	.content-section:last-child {
		border-bottom: none;
		margin-bottom: 0;
		padding-bottom: 0;
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

	/* Search results styling - now uses panel-section-header */

	/* Search controls styling - now uses panel-section-header */

	.sort-controls {
		display: flex;
		align-items: center;
		gap: 4px;
	}

	.sort-label {
		font-size: 10px;
		color: #6b7280;
		font-weight: 500;
	}

	.sort-select {
		padding: 4px 8px;
		border: 1px solid #d1d5db;
		border-radius: 4px;
		font-size: 10px;
		background: white;
		color: #374151;
		cursor: pointer;
	}

	/* Results container - now uses panel-scrollable-content */

	.search-results {
		display: flex;
		flex-direction: column;
		gap: 6px;
	}

	.search-result-item {
		background: white;
		border: 1px solid #e5e7eb;
		border-radius: 8px;
		padding: 10px;
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
		margin-bottom: 6px;
	}

	.result-title {
		display: flex;
		align-items: center;
		gap: 4px;
		flex: 1;
		min-width: 0;
	}

	.result-emoji {
		font-size: 14px;
		flex-shrink: 0;
	}

	.result-name {
		font-weight: 600;
		color: #111827;
		font-size: 10px;
		overflow: hidden;
		text-overflow: ellipsis;
		white-space: nowrap;
	}

	.result-distance {
		color: #6b7280;
		font-size: 10px;
		font-weight: 500;
		background: #f3f4f6;
		padding: 2px 6px;
		border-radius: 4px;
		flex-shrink: 0;
	}

	.result-details {
		display: flex;
		align-items: center;
		gap: 6px;
		margin-bottom: 6px;
		font-size: 10px;
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
		font-size: 10px;
		line-height: 1.2;
		margin-top: 4px;
	}

	.no-results {
		text-align: center;
		padding: 48px 24px;
		color: #6b7280;
	}

	.no-results-icon {
		font-size: 48px;
		margin-bottom: 6px;
		opacity: 0.5;
	}

	.no-results h3 {
		margin: 0 0 8px 0;
		font-size: 14px;
		font-weight: 600;
		color: #374151;
	}

	.no-results p {
		margin: 0;
		font-size: 10px;
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
		font-size: 15px;
		font-weight: 600;
		color: #111827;
		margin: 0 0 4px 0;
		display: flex;
		align-items: center;
		gap: 4px;
	}

	.capacity-emoji {
		font-size: 18px;
	}

	.provider-info {
		display: flex;
		align-items: center;
		gap: 6px;
		flex-wrap: wrap;
	}

	.provider-label {
		color: #6b7280;
		font-size: 10px;
		font-style: italic;
	}

	.slot-count-badge {
		background: #ef4444;
		color: white;
		padding: 2px 8px;
		border-radius: 12px;
		font-size: 10px;
		font-weight: 500;
	}

	.capacity-unit-badge {
		background: #f3e8ff;
		color: #7c3aed;
		padding: 2px 8px;
		border-radius: 12px;
		font-size: 10px;
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
		font-size: 15px;
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
		font-size: 10px;
		font-weight: 500;
		margin-bottom: 6px;
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
		font-size: 14px;
		font-weight: 600;
		color: #374151;
		margin: 0 0 6px 0;
		display: flex;
		align-items: center;
		gap: 6px;
	}

	.location-details {
		background: #f9fafb;
		padding: 3px;
		border-radius: 3px;
		border: 1px solid #e5e7eb;
	}

	.location-address,
	.location-coords {
		font-size: 6px;
		color: #111827;
		margin-bottom: 1px;
		line-height: 1;
	}

	.location-coords {
		font-size: 5px;
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
		padding: 1px 3px;
		border-radius: 2px;
		transition: all 0.2s ease;
		text-align: left;
		width: 100%;
		margin-bottom: 1px;
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
		margin-bottom: 10px;
	}

	.category-title {
		font-size: 10px;
		font-weight: 600;
		color: #374151;
		margin: 0 0 4px 0;
		padding: 4px 0;
		border-bottom: 1px solid #f3f4f6;
	}

	.slot-list {
		display: flex;
		flex-direction: column;
		gap: 4px;
	}

	.slot-item {
		background: #f9fafb;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		padding: 6px;
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
		margin-bottom: 2px;
	}

	.slot-quantity {
		font-weight: 600;
		color: #111827;
		font-size: 10px;
		font-family: 'SF Mono', 'Monaco', 'Inconsolata', 'Roboto Mono', monospace;
	}

	.slot-quantity.allocated {
		color: #10b981;
		background: #d1fae5;
		padding: 2px 4px;
		border-radius: 3px;
		border: 1px solid #a7f3d0;
	}

	.slot-quantity.available {
		color: #3b82f6;
		background: #dbeafe;
		padding: 2px 4px;
		border-radius: 3px;
		border: 1px solid #93c5fd;
	}

	.slot-total {
		color: #6b7280;
		font-size: 10px;
		font-weight: 500;
		margin-left: 8px;
	}

	.slot-share {
		color: #7c3aed;
		background: #f3e8ff;
		padding: 2px 4px;
		border-radius: 3px;
		border: 1px solid #c4b5fd;
		font-size: 10px;
		font-weight: 600;
		font-family: 'SF Mono', 'Monaco', 'Inconsolata', 'Roboto Mono', monospace;
		margin-left: 8px;
	}

	.slot-time {
		color: #6b7280;
		font-size: 10px;
	}

	.slot-meta {
		margin-top: 4px;
	}

	.notice-info {
		color: #9ca3af;
		font-size: 10px;
	}

	/* Cluster display styles */
	.cluster-contents {
		display: flex;
		flex-direction: column;
		gap: 6px;
	}

	.cluster-item {
		background: #f9fafb;
		border: 1px solid #e5e7eb;
		border-radius: 6px;
		padding: 8px;
		transition: all 0.2s ease;
	}

	.cluster-item:hover {
		background: #f3f4f6;
		border-color: #d1d5db;
	}

	.cluster-item-header {
		display: flex;
		align-items: center;
		gap: 6px;
		margin-bottom: 4px;
	}

	.cluster-item-emoji {
		font-size: 14px;
		flex-shrink: 0;
	}

	.cluster-item-name {
		font-weight: 600;
		color: #111827;
		font-size: 10px;
		flex: 1;
		min-width: 0;
		overflow: hidden;
		text-overflow: ellipsis;
		white-space: nowrap;
	}

	.cluster-item-slots {
		background: #dbeafe;
		color: #1d4ed8;
		padding: 2px 6px;
		border-radius: 4px;
		font-size: 8px;
		font-weight: 500;
		flex-shrink: 0;
	}

	.cluster-item-unit {
		background: #f3e8ff;
		color: #7c3aed;
		padding: 2px 6px;
		border-radius: 4px;
		font-size: 8px;
		font-weight: 500;
		flex-shrink: 0;
	}

	.cluster-item-provider {
		color: #6b7280;
		font-size: 8px;
		font-style: italic;
		margin-bottom: 4px;
	}

	.cluster-item-description {
		color: #4b5563;
		font-size: 8px;
		line-height: 1.2;
	}

	/* Cluster view header styles */
	.cluster-view-header {
		text-align: center;
		padding: 8px 0;
	}

	.cluster-view-title {
		font-size: 14px;
		font-weight: 600;
		color: #111827;
		margin: 0 0 6px 0;
		display: flex;
		align-items: center;
		justify-content: center;
		gap: 6px;
	}

	.cluster-view-emoji {
		font-size: 16px;
	}

	/* Cluster results styling */
	.cluster-results {
		display: flex;
		flex-direction: column;
		gap: 6px;
	}

	.cluster-result-item {
		background: white;
		border: 1px solid #e5e7eb;
		border-radius: 8px;
		padding: 10px;
		cursor: pointer;
		transition: all 0.2s ease;
	}

	.cluster-result-item:hover {
		background: #f9fafb;
		border-color: #3b82f6;
		transform: translateY(-1px);
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
	}

	/* Responsive design - maintain map height constraints */
	@media (max-width: 768px) {
		.search-panel {
			width: 280px;
			max-width: calc(100vw - 32px);
			/* Keep the same height constraint */
			max-height: min(380px, calc(50vh - 20px));
		}

		.search-panel.fullscreen {
			max-height: calc(100vh - 40px);
		}

		.search-input {
			min-width: 120px;
		}
	}

	@media (max-width: 480px) {
		.search-panel {
			width: 260px;
			max-width: calc(100vw - 16px);
			/* Keep the same height constraint */
			max-height: min(380px, calc(50vh - 20px));
		}

		.search-panel.fullscreen {
			max-height: calc(100vh - 40px);
		}

		.search-input {
			min-width: 100px;
		}
	}
</style>
