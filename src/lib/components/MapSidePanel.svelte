<script lang="ts">
	import type { GroupedSlotMarkerData, ClusterMarkerData } from '$lib/components/Map.svelte';
	import type { NeedSlot, AvailabilitySlot, SlotAllocationRecord, Commitment } from '$lib/protocol/schemas';
	import { handleAddressClick } from '$lib/location/mapUtils';
	import { globalState } from '$lib/global.svelte';
	// V5: Import user pubkey to look up recognition shares
	import { holsterUserPub } from '$lib/network/holster.svelte';
	import { myAllocationsAsProvider } from '$lib/protocol/stores/allocation.svelte';
	import { networkAllocations } from '$lib/protocol/stores/stores.svelte';
	import { get } from 'svelte/store';
	import { 
		formatTimeDisplay, 
		formatLocationDisplay, 
		parseSlotDateTime, 
		isSlotInPast,
		hasAddressComponents
	} from '$lib/utils/formatting';
	import { getNeedTypeLabel, getNeedTypeEmoji } from '$lib/protocol/needTypes-local';

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

	// Allocation data integration (matching ResourceSlots pattern)
	const myAllocations = $derived($myAllocationsAsProvider.allocations || []);
	const allNetworkAllocations = $derived($networkAllocations);
	const myPubKey = $derived($holsterUserPub);

	// PERFORMANCE FIX: On-demand allocation lookup instead of building expensive map
	// Only search when needed for visible slots (much faster than rebuilding 1,484-item map)

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

	// Get allocated quantity for a specific slot (on-demand search)
	// PERFORMANCE: Direct search is faster than maintaining a map when we only need a few slots
	function getSlotAllocatedQuantity(capacity: Commitment, slotId: string): number {
		return myAllocations
			.filter(alloc => alloc.availability_slot_id === slotId)
			.reduce((sum, alloc) => sum + alloc.quantity, 0);
	}

	// Calculate recognition share for a slot from slot's priority_distribution
	function getSlotRecognitionShare(capacity: Commitment, slotId: string): number {
		const slot = capacity.capacity_slots?.find((s: AvailabilitySlot) => s.id === slotId);
		if (!slot) return 0;

		// Get user's pubkey to look up their share in this slot's priority_distribution
		const userPubkey = get(holsterUserPub);
		if (!userPubkey) return 0;

		// Get the user's recognition share from the slot's priority_distribution
		// This is the slot-specific recognition weight (0-1)
		const recognitionWeight = slot.priority_distribution?.[userPubkey] || 0;

		// Calculate: slot total quantity * recognition weight
		const totalQuantity = slot.quantity || 0;
		const recognitionShare = totalQuantity * recognitionWeight;

		return recognitionShare;
	}

	// Reactive visibility derived from markerData
	let isVisible = $derived(!!markerData);

	// Helper to check if marker is a cluster
	function isClusterMarker(
		marker: GroupedSlotMarkerData | ClusterMarkerData | null
	): marker is ClusterMarkerData {
		return marker !== null && 'markers' in marker && 'totalCapacities' in marker;
	}

	// Debug logging
	$inspect('MapSidePanel markerData:', markerData?.id);

	// Helper function to check if a slot is recurring (matches Share.svelte)
	function isSlotRecurring(slot: AvailabilitySlot | NeedSlot): boolean {
		return !!(slot.recurrence && slot.recurrence !== null);
	}

	// Categorize slots like in Share.svelte
	function categorizeSlots(slots: (AvailabilitySlot | NeedSlot)[] & { is_need?: boolean }[]): {
		recurring: ((AvailabilitySlot | NeedSlot) & { is_need?: boolean })[];
		currentFuture: ((AvailabilitySlot | NeedSlot) & { is_need?: boolean })[];
		past: ((AvailabilitySlot | NeedSlot) & { is_need?: boolean })[];
	} {
		const recurring: ((AvailabilitySlot | NeedSlot) & { is_need?: boolean })[] = [];
		const currentFuture: ((AvailabilitySlot | NeedSlot) & { is_need?: boolean })[] = [];
		const past: ((AvailabilitySlot | NeedSlot) & { is_need?: boolean })[] = [];

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

	// Helper to get display info for a commitment (Need or Capacity)
	function getCommitmentDisplayInfo(slots: any[]) {
		if (!slots || slots.length === 0) {
			return { name: 'Unknown', emoji: '🎁', unit: undefined, description: undefined };
		}
		
		const firstSlot = slots[0];
		
		if (firstSlot.is_need) {
			// NeedSlot handling
			return {
				name: firstSlot.name, // Use the need slot's name (e.g., "Housing")
				emoji: getNeedTypeEmoji(firstSlot.type_id) || '🚩', // Use type emoji or default flag
				unit: firstSlot.unit,
				description: firstSlot.description
			};
		} else {
			// AvailabilitySlot handling
			return {
				name: firstSlot.name || 'Available Capacity', // Use slot name or fallback
				emoji: firstSlot.emoji || '🎁',
				unit: firstSlot.unit,
				description: firstSlot.description
			};
		}
	}
	
	// Format allocated quantity to max 2 decimals, avoiding long floats
	function formatAllocatedQuantity(quantity: number): string {
		if (quantity === 0) return '0';
		
		// If it's effectively an integer (within small epsilon), show as integer
		if (Math.abs(Math.round(quantity) - quantity) < 0.0000001) {
			return Math.round(quantity).toString();
		}
		
		// Otherwise show up to 2 decimal places, stripping trailing zeros
		return parseFloat(quantity.toFixed(2)).toString();
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
						{clusterViewResults.length} Items at this Location
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
							{@const displayInfo = getCommitmentDisplayInfo(result.slots)}

							<div
								class="cluster-result-item"
								onclick={() => {
									console.log('[Cluster View] Item clicked:', displayInfo.name);
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
										<span class="result-emoji">{displayInfo.emoji}</span>
										<span class="result-name">{displayInfo.name}</span>
									</div>
									{#if distance !== null}
										<span class="result-distance">{formatDistance(distance)}</span>
									{/if}
								</div>

								<div class="result-details">
									<div class="result-provider">👤 {result.providerName}</div>
									{#if displayInfo.unit}
										<div class="result-unit">{displayInfo.unit}</div>
									{/if}
									<div class="result-slots">{result.slots.length} slots</div>
								</div>

								{#if displayInfo.description}
									<div class="result-description">
										{displayInfo.description.length > 100
											? displayInfo.description.substring(0, 100) + '...'
											: displayInfo.description}
									</div>
								{/if}
							</div>
						{/each}
					</div>
				{:else}
					<p class="no-results">No items found in this cluster.</p>
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
							{totalCapacities} Items
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
							Click to zoom in and see individual items
						</p>
					</div>
				</div>

				<!-- Cluster Contents -->
				<div class="content-section slots-section">
					<h3 class="section-title">🎁 {totalCapacities} Items</h3>
					<div class="cluster-contents">
						{#each markers as marker}
							{@const { slots, providerName } = marker}
							{@const displayInfo = getCommitmentDisplayInfo(slots)}
							<div class="cluster-item">
								<div class="cluster-item-header">
									<span class="cluster-item-emoji">{displayInfo.emoji}</span>
									<span class="cluster-item-name">{displayInfo.name}</span>
									{#if displayInfo.unit}
										<span class="cluster-item-unit">{displayInfo.unit}</span>
									{/if}
									<span class="cluster-item-slots">{slots.length} slots</span>
								</div>
								<div class="cluster-item-provider">👤 {providerName}</div>
								{#if displayInfo.description}
									<div class="cluster-item-description">
										{displayInfo.description.length > 60
											? displayInfo.description.substring(0, 60) + '...'
											: displayInfo.description}
									</div>
								{/if}
							</div>
						{/each}
					</div>
				</div>
			</div>
		{:else}
			<!-- Show individual marker details -->
			{@const { slots, lnglat, source, providerName } = markerData}
			{@const displayInfo = getCommitmentDisplayInfo(slots)}
			
			{@const lngLatText = `${lnglat.lat.toFixed(6)}, ${lnglat.lng.toFixed(6)}`}
			{@const isGeocoded = source === 'geocoded'}
			{@const locationDisplay = formatLocationDisplay(slots[0])}
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
							<span class="capacity-emoji">{displayInfo.emoji}</span>
							{displayInfo.name}
						</h2>
						<div class="provider-info">
							<span class="provider-label">👤 {providerName}</span>
							{#if displayInfo.unit}
								<span class="capacity-unit-badge">{displayInfo.unit}</span>
							{/if}
							{#if totalSlots > 1}
								<span class="slot-count-badge">{totalSlots} slots</span>
							{/if}
						</div>
					</div>
					<button class="close-btn" onclick={onClose} title="Close panel" aria-label="Close panel">
						✕
					</button>
				</div>

				<!-- Location & Provider Section -->
				<div class="content-section location-section">
					<h3 class="section-title"><span style="font-size:8px;">📍</span> Location</h3>
					<div class="location-details">
						<div class="location-coords single-location" style="font-size: 8px; line-height: 1;">
							<span style="font-size:8px;">📐</span>
							<span style="font-size: 8px; font-family: monospace;">{lngLatText}</span>
						</div>

						{#if hasAddressComponents(slots[0])}
							<div class="location-address" style="margin-top: 8px;">
								<span style="font-size:12px;">🏠</span>
								<a
									href="#"
									onclick={(e) => {
										e.preventDefault();
										handleAddressClick(slots[0]);
									}}
									class="address-link"
								>
									{formatLocationDisplay(slots[0])}
								</a>
							</div>
						{/if}

						<div class="location-meta">
							<span class="source-badge" class:geocoded={isGeocoded}>
								{isGeocoded ? '📍 Geocoded' : '🎯 Exact'}
							</span>
						</div>
					</div>
				</div>

				<!-- Slots Sections -->
				<div class="content-section slots-section">
					<h3 class="section-title">
						{slots[0]?.is_need ? '🚩 Needs' : '📅 Availability'} 
						({totalSlots})
					</h3>
					
					<!-- Recurring Slots -->
					{#if categorizedSlots.recurring.length > 0}
						<div class="slots-list recurring">
							{#each categorizedSlots.recurring as slot}
								{@const allocated = getSlotAllocatedQuantity(markerData.capacity as Commitment, slot.id)}
								{@const dateTimeInfo = parseSlotDateTime(slot)}
								{@const isAllDay = (slot as any).all_day}

								<div class="slot-item">
									<!-- Slot Details Header: Recurrence -->
									<div class="slot-header-info">
										<div class="recurrence-badge">
											<span class="icon">🔄</span>
											<span class="text">{slot.recurrence}</span>
										</div>
										<!-- Time Range Display for Recurring -->
										{#if slot.availability_window?.time_ranges?.length}
											<div class="time-windows">
												{#each slot.availability_window.time_ranges as range}
													<span class="time-pill">
														{range.start_time.slice(0, 5)} - {range.end_time.slice(0, 5)}
													</span>
												{/each}
											</div>
										{:else if dateTimeInfo.slotStart && !isAllDay}
											<div class="time-windows">
												<span class="time-pill">
													{dateTimeInfo.slotStart.toLocaleTimeString([], {hour: '2-digit', minute:'2-digit'})} 
													{#if dateTimeInfo.slotEnd} - {dateTimeInfo.slotEnd.toLocaleTimeString([], {hour: '2-digit', minute:'2-digit'})}{/if}
												</span>
											</div>
										{:else}
											<div class="time-windows">
												<span class="time-pill">All Day</span>
											</div>
										{/if}
									</div>

									<div class="slot-info">
										{#if slot.is_need}
											<div class="slot-need-details">
												<div class="need-header">
													<span class="need-icon">🚩</span>
													<span class="need-label">Required:</span>
												</div>
												<span class="need-quantity">{slot.quantity} {slot.unit || ''}</span>
											</div>
										{:else}
											<div class="slot-quantity-row">
												<div class="qty-available-group">
													<span class="qty-number">{slot.quantity}</span>
													<span class="qty-label">{slot.unit || 'available'}</span>
												</div>
												{#if allocated > 0}
													<div class="qty-allocated-badge">
														{formatAllocatedQuantity(allocated)} allocated
													</div>
												{/if}
											</div>
										{/if}
										
										{#if slot.description}
											<div class="slot-desc">{slot.description}</div>
										{/if}
									</div>
								</div>
							{/each}
						</div>
					{/if}

					<!-- Current/Future Slots -->
					{#if categorizedSlots.currentFuture.length > 0}
						<div class="slots-list current">
							{#each categorizedSlots.currentFuture as slot}
								{@const allocated = getSlotAllocatedQuantity(markerData.capacity as Commitment, slot.id)}
								{@const dateTimeInfo = parseSlotDateTime(slot)}
								{@const isAllDay = (slot as any).all_day}
								
								<div class="slot-item">
									<!-- Slot Details Header: Recurrence & Date -->
									<div class="slot-header-info">
										{#if slot.recurrence}
											<div class="recurrence-badge">
												<span class="icon">🔄</span>
												<span class="text">{slot.recurrence}</span>
											</div>
										{:else if slot.start_date}
											<div class="date-badge">
												<span class="icon">📅</span>
												<span class="text">{new Date(slot.start_date).toLocaleDateString(undefined, { weekday: 'short', month: 'short', day: 'numeric' })}</span>
											</div>
										{:else}
											<div class="availability-badge">
												<span class="icon">✅</span>
												<span class="text">Flexible</span>
											</div>
										{/if}

										<!-- Time Range Display -->
										{#if slot.availability_window?.time_ranges?.length}
											<div class="time-windows">
												{#each slot.availability_window.time_ranges as range}
													<span class="time-pill">
														{range.start_time.slice(0, 5)} - {range.end_time.slice(0, 5)}
													</span>
												{/each}
											</div>
										{:else if dateTimeInfo.slotStart && !isAllDay}
											<div class="time-windows">
												<span class="time-pill">
													{dateTimeInfo.slotStart.toLocaleTimeString([], {hour: '2-digit', minute:'2-digit'})} 
													{#if dateTimeInfo.slotEnd} - {dateTimeInfo.slotEnd.toLocaleTimeString([], {hour: '2-digit', minute:'2-digit'})}{/if}
												</span>
											</div>
										{/if}
									</div>

									<div class="slot-info">
										{#if slot.is_need}
											<div class="slot-need-details">
												<div class="need-header">
													<span class="need-icon">🚩</span>
													<span class="need-label">Required:</span>
												</div>
												<span class="need-quantity">{slot.quantity} {slot.unit || ''}</span>
											</div>
										{:else}
											<div class="slot-quantity-row">
												<div class="qty-available-group">
													<span class="qty-number">{slot.quantity}</span>
													<span class="qty-label">{slot.unit || 'available'}</span>
												</div>
												{#if allocated > 0}
													<div class="qty-allocated-badge">
														{formatAllocatedQuantity(allocated)} allocated
													</div>
												{/if}
											</div>
										{/if}
										
										{#if slot.description}
											<div class="slot-desc">{slot.description}</div>
										{/if}
									</div>
								</div>
							{/each}
						</div>
					{/if}

					<!-- Past Slots -->
					{#if categorizedSlots.past.length > 0}
						<div class="past-slots-summary">
							<span class="past-icon">⏳</span>
							<span>{categorizedSlots.past.length} past items</span>
						</div>
					{/if}
				</div>
			</div>
		{/if}
	{:else if globalState.isSearchMode}
		<!-- Search Results -->
		<div
			class="search-results-container"
			class:visible={true}
			onscroll={handlePanelScroll}
			onwheel={handlePanelWheel}
		>
			<div class="search-header">
				<h3 class="results-count">
					{#if searchResults.length === 0 && globalState.searchQuery}
						No results found
					{:else if searchResults.length > 0}
						{searchResults.length} {searchResults.length === 1 ? 'result' : 'results'}
					{:else}
						Start typing to search
					{/if}
				</h3>

				{#if searchResults.length > 0}
					<div class="sort-controls">
						<select
							class="sort-select"
							value={searchSortBy}
							onchange={(e) => {
								// Update parent prop by calling the change handler which will likely update a bind or trigger a refetch
								// For now we just call the onSortChange event
								onSortChange?.();
							}}
						>
							<option value="relevance">By Relevance</option>
							<option value="distance">By Distance</option>
						</select>
					</div>
				{/if}
			</div>

			<div class="results-list">
				{#each searchResults as result (result.id)}
					{@const distance = currentLocation
						? calculateDistance(
								currentLocation.latitude,
								currentLocation.longitude,
								result.lnglat.lat,
								result.lnglat.lng
							)
						: null}
					{@const displayInfo = getCommitmentDisplayInfo(result.slots)}

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
								<span class="result-emoji">{displayInfo.emoji}</span>
								<span class="highlight-match">{displayInfo.name}</span>
							</div>
							{#if distance !== null}
								<span class="result-distance">{formatDistance(distance)}</span>
							{/if}
						</div>

						<div class="result-details">
							<div class="result-provider">👤 {result.providerName}</div>
							{#if displayInfo.unit}
								<div class="result-unit">{displayInfo.unit}</div>
							{/if}
							<div class="result-slots">{result.slots.length} slots</div>
						</div>
					</div>
				{/each}
			</div>
		</div>
	{:else}
		<!-- Default state when no marker selected: Just show search bar -->
		<!-- No empty state message needed, keeps it clean and floating -->
	{/if}
</div>

<style>
	/* ... existing styles ... */
	
	.search-panel {
		position: absolute;
		top: 10px; /* Floating offset from top */
		left: 10px; /* Floating offset from left */
		width: 320px;
		height: auto;
		max-height: calc(100% - 20px); /* Constrain height minus margins */
		background: white;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15); /* Stronger shadow/lift */
		z-index: 20;
		display: flex;
		flex-direction: column;
		transition: transform 0.3s cubic-bezier(0.4, 0, 0.2, 1);
		pointer-events: auto; /* Ensure panel captures events */
		border-radius: 24px; /* Even softer floating look */
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
		background: #f3f4f6;
		min-height: 36px; /* Changed to min-height */
		border-radius: 24px; /* Pill/round shape for search bar */
		padding: 4px 4px; /* Slightly more padding for the round look */
		border: 1px solid transparent;
		transition: all 0.2s;
	}

	.search-input-wrapper:focus-within {
		background: white;
		border-color: #3b82f6;
		box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
	}

	.search-input {
		flex: 1;
		border: none;
		background: transparent;
		padding: 10px 12px;
		font-size: 14px;
		outline: none;
		min-width: 0;
	}

	.clear-btn {
		background: none;
		border: none;
		color: #9ca3af;
		cursor: pointer;
		padding: 4px 8px;
		font-size: 14px;
		line-height: 1;
		border-radius: 50%;
	}

	.clear-btn:hover {
		color: #4b5563;
		background: #e5e7eb;
	}

	.time-filter-inline {
		display: flex;
		align-items: center;
		border-left: 1px solid #e5e7eb;
		padding-left: 8px;
		margin-left: 4px;
		margin-right: 4px;
	}

	.time-filter-icon {
		font-size: 14px;
		margin-right: 4px;
		opacity: 0.6;
	}

	.time-filter-select {
		border: none;
		background: transparent;
		font-size: 12px;
		color: #4b5563;
		cursor: pointer;
		outline: none;
		padding-right: 4px;
		max-width: 80px;
	}

	.time-filter-details {
		margin-top: 12px;
		padding-top: 12px;
		border-top: 1px dashed #e5e7eb;
		display: flex;
		flex-direction: column;
		gap: 8px;
		animation: slideDown 0.2s ease-out;
	}

	@keyframes slideDown {
		from { opacity: 0; transform: translateY(-10px); }
		to { opacity: 1; transform: translateY(0); }
	}

	.time-row {
		display: flex;
		gap: 8px;
	}

	.time-input {
		flex: 1;
		border: 1px solid #e5e7eb;
		border-radius: 6px;
		padding: 6px;
		font-size: 12px;
		color: #4b5563;
	}

	.panel-content {
		flex: 1;
		overflow-y: auto;
		padding: 0;
		/* Custom scrollbar */
		scrollbar-width: thin;
		scrollbar-color: #d1d5db transparent;
	}

	.panel-content::-webkit-scrollbar {
		width: 6px;
	}

	.panel-content::-webkit-scrollbar-thumb {
		background-color: #d1d5db;
		border-radius: 3px;
	}

	.content-section {
		padding: 16px;
		border-bottom: 1px solid #f3f4f6;
	}

	.marker-header {
		display: flex;
		align-items: flex-start;
		gap: 12px;
		background: #f9fafb;
		position: sticky;
		top: 0;
		z-index: 10;
	}

	.cluster-view-header {
		flex: 1;
	}

	.cluster-view-title {
		margin: 0;
		font-size: 16px;
		font-weight: 600;
		color: #111827;
		display: flex;
		align-items: center;
		gap: 8px;
	}

	.back-btn {
		background: white;
		border: 1px solid #e5e7eb;
		color: #4b5563;
		width: 32px;
		height: 32px;
		border-radius: 50%;
		display: flex;
		align-items: center;
		justify-content: center;
		cursor: pointer;
		flex-shrink: 0;
		box-shadow: 0 1px 2px rgba(0,0,0,0.05);
		transition: all 0.2s;
	}

	.back-btn:hover {
		background: #f3f4f6;
		color: #111827;
		transform: translateX(-2px);
	}

	.capacity-info {
		flex: 1;
		min-width: 0;
	}

	.capacity-title {
		margin: 0 0 4px 0;
		font-size: 18px;
		font-weight: 600;
		color: #111827;
		display: flex;
		align-items: center;
		gap: 8px;
		line-height: 1.3;
	}

	.capacity-emoji, .cluster-view-emoji {
		font-size: 20px;
	}

	.provider-info {
		display: flex;
		align-items: center;
		flex-wrap: wrap;
		gap: 8px;
		font-size: 13px;
		color: #6b7280;
	}

	.capacity-unit-badge, .slot-count-badge {
		background: #e5e7eb;
		color: #374151;
		padding: 2px 6px;
		border-radius: 4px;
		font-size: 11px;
		font-weight: 500;
	}

	.slot-count-badge {
		background: #dbeafe;
		color: #1e40af;
	}

	.close-btn {
		background: transparent;
		border: none;
		color: #9ca3af;
		font-size: 18px;
		cursor: pointer;
		padding: 4px;
		line-height: 1;
		border-radius: 4px;
		align-self: flex-start;
		margin-top: -4px;
		margin-right: -4px;
	}

	.close-btn:hover {
		color: #4b5563;
		background: #e5e7eb;
	}

	.section-title {
		font-size: 12px;
		text-transform: uppercase;
		letter-spacing: 0.05em;
		color: #9ca3af;
		margin: 0 0 12px 0;
		font-weight: 600;
		display: flex;
		align-items: center;
		gap: 6px;
	}

	/* Cluster Results List */
	.cluster-results, .cluster-contents {
		display: flex;
		flex-direction: column;
		gap: 12px;
	}

	.cluster-result-item, .search-result-item, .cluster-item {
		background: white;
		border: 1px solid #e5e7eb;
		border-radius: 8px;
		padding: 12px;
		cursor: pointer;
		transition: all 0.2s;
	}

	.cluster-result-item:hover, .search-result-item:hover, .cluster-item:hover {
		border-color: #3b82f6;
		box-shadow: 0 2px 4px rgba(59, 130, 246, 0.1);
		transform: translateY(-1px);
	}

	.result-header, .cluster-item-header {
		display: flex;
		justify-content: space-between;
		align-items: flex-start;
		margin-bottom: 6px;
	}

	.result-title, .cluster-item-header {
		font-weight: 500;
		color: #111827;
		display: flex;
		align-items: center;
		gap: 6px;
		flex: 1;
		min-width: 0; /* truncate text properly */
	}

	.result-emoji, .cluster-item-emoji {
		font-size: 16px;
	}

	.result-name, .cluster-item-name {
		white-space: nowrap;
		overflow: hidden;
		text-overflow: ellipsis;
	}

	.highlight-match {
		color: #2563eb;
		font-weight: 600;
	}

	.result-distance {
		font-size: 11px;
		color: #6b7280;
		background: #f3f4f6;
		padding: 2px 6px;
		border-radius: 12px;
		white-space: nowrap;
		margin-left: 8px;
	}

	.result-details, .cluster-item-provider {
		display: flex;
		align-items: center;
		gap: 8px;
		font-size: 12px;
		color: #6b7280;
	}
	
	.result-slots, .cluster-item-slots {
		margin-left: auto;
		background: #f3f4f6;
		padding: 2px 6px;
		border-radius: 4px;
		font-size: 10px;
	}

	.result-description, .cluster-item-description {
		margin-top: 8px;
		font-size: 11px;
		color: #6b7280;
		display: -webkit-box;
		-webkit-line-clamp: 2;
		-webkit-box-orient: vertical;
		overflow: hidden;
		line-height: 1.4;
	}

	.no-results {
		text-align: center;
		color: #9ca3af;
		padding: 24px 0;
		font-style: italic;
	}



	/* Slot Items - Soft Aesthetic */
	.slots-list {
		display: flex;
		flex-direction: column;
		gap: 12px;
	}

	.slot-item {
		background: white;
		border: 1px solid #f3f4f6; /* Lighter border */
		border-radius: 12px; /* Softer rounded corners */
		padding: 14px;
		box-shadow: 0 2px 4px rgba(0,0,0,0.02); /* Very subtle shadow */
		transition: transform 0.1s ease, box-shadow 0.1s ease;
	}

	.slot-item:hover {
		box-shadow: 0 4px 8px rgba(0,0,0,0.05); /* Hover lift */
	}
	
	.slot-header-info {
		display: flex;
		flex-wrap: wrap;
		gap: 8px;
		align-items: center;
		margin-bottom: 10px;
		padding-bottom: 10px;
		border-bottom: 1px solid #f9fafb;
	}

	.recurrence-badge, .date-badge, .availability-badge {
		display: flex;
		align-items: center;
		gap: 4px;
		background: #f0fdf4; /* Light green bg */
		color: #166534; /* Green text */
		font-size: 11px;
		padding: 4px 8px;
		border-radius: 20px; /* Pill shape */
		font-weight: 500;
	}
	
	.recurrence-badge {
		background: #eff6ff;
		color: #1e40af;
	}

	.availability-badge {
		background: #fdf4ff; 
		color: #86198f;
	}

	.time-windows {
		display: flex;
		gap: 4px;
		flex-wrap: wrap;
	}

	.time-pill {
		font-family: 'Roboto Mono', monospace;
		font-size: 11px;
		background: #f3f4f6;
		color: #374151;
		padding: 2px 6px;
		border-radius: 4px;
	}

	.slot-info {
		font-size: 13px;
	}

	.slot-quantity-row {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 4px;
	}

	.qty-available-group {
		display: flex;
		align-items: baseline;
		gap: 4px;
	}

	.qty-number {
		font-size: 16px;
		font-weight: 700;
		color: #111827;
	}

	.qty-label {
		font-size: 12px;
		color: #6b7280;
	}

	/* Search Results Container - Fixed scrolling */
	.search-results-container {
		flex: 1;
		min-height: 0;
		overflow-y: auto;
		display: flex;
		flex-direction: column;
		/* Custom scrollbar */
		scrollbar-width: thin;
		scrollbar-color: #d1d5db transparent;
	}

	.search-results-container::-webkit-scrollbar {
		width: 6px;
	}

	.search-results-container::-webkit-scrollbar-thumb {
		background-color: #d1d5db;
		border-radius: 3px;
	}

	.search-header {
		padding: 12px 16px;
		background: #f9fafb;
		border-bottom: 1px solid #f3f4f6;
		display: flex;
		justify-content: space-between;
		align-items: center;
		position: sticky;
		top: 0;
		z-index: 10;
	}

	.results-count {
		font-size: 13px;
		font-weight: 600;
		color: #4b5563;
		margin: 0;
		text-transform: uppercase;
		letter-spacing: 0.02em;
	}

	.sort-select {
		font-size: 12px;
		border: none;
		background: transparent;
		color: #6b7280;
		cursor: pointer;
		outline: none;
		font-weight: 500;
	}

	.sort-select:hover {
		color: #111827;
	}

	.results-list {
		padding: 12px;
		display: flex;
		flex-direction: column;
		gap: 12px;
	}
	
	.slot-need-details {
		display: flex;
		flex-direction: column;
		gap: 4px;
		background: #fffbeb;
		padding: 8px;
		border-radius: 8px;
		border: 1px solid #fef3c7;
	}
	
	.need-header {
		display: flex;
		align-items: center;
		gap: 6px;
	}

	.need-label {
		font-weight: 600;
		font-size: 11px;
		text-transform: uppercase;
		color: #d97706; /* Amber-600 */
		letter-spacing: 0.5px;
	}
	
	.need-quantity {
		font-weight: 700;
		color: #b45309;
		font-size: 14px;
		padding-left: 20px; /* Indent under icon */
	}

	.slot-desc {
		margin-top: 8px;
		font-size: 12px;
		color: #4b5563;
		line-height: 1.5;
		font-style: italic;
	}

	.past-slots-summary {
		margin-top: 8px;
		padding: 12px;
		background: #f9fafb;
		border-radius: 8px;
		color: #9ca3af;
		font-size: 12px;
		display: flex;
		align-items: center;
		justify-content: center;
		gap: 8px;
	}
	
	.source-badge {
		font-size: 10px;
		padding: 2px 8px;
		border-radius: 12px;
		background: #eef2ff;
		color: #4f46e5;
		font-weight: 600;
		border: 1px solid #e0e7ff;
	}
	
	.source-badge.geocoded {
		background: #f3f4f6;
		color: #6b7280;
		border: 1px solid #e5e7eb;
	}
</style>
